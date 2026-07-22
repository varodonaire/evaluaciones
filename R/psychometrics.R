# ==============================================================================
# ANALISIS PSICOMETRICO
# ==============================================================================

#' Calcular indice de dificultad de items
#'
#' El indice de dificultad es la proporcion de estudiantes que responden
#' correctamente. Valores cercanos a 0 indican items dificiles, cercanos a 1
#' indican items faciles. El rango ideal es 0.3-0.7.
#'
#' @param scored_data Data frame con respuestas calificadas
#' @param by_form Si TRUE, calcula por forma ademas de global
#'
#' @return Data frame con indices de dificultad por item
#' @export
calculate_difficulty_index <- function(scored_data, by_form = FALSE) {
  
  scored_data <- normalize_correct_col(scored_data)
  
  if (by_form) {
    difficulty <- scored_data %>%
      group_by(Origen, Forma) %>%
      summarise(
        n_respuestas = n(),
        n_correctas = sum(is_correcta, na.rm = TRUE),
        dificultad = n_correctas / n_respuestas,
        .groups = "drop"
      )
  } else {
    difficulty <- scored_data %>%
      group_by(Origen) %>%
      summarise(
        n_respuestas = n(),
        n_correctas = sum(is_correcta, na.rm = TRUE),
        dificultad = n_correctas / n_respuestas,
        .groups = "drop"
      )
  }
  
  # Clasificar dificultad
  difficulty <- difficulty %>%
    mutate(
      clasificacion = case_when(
        dificultad < 0.20 ~ "Muy dificil",
        dificultad < 0.40 ~ "Dificil",
        dificultad < 0.60 ~ "Moderado",
        dificultad < 0.80 ~ "Facil",
        TRUE ~ "Muy facil"
      )
    )
  
  return(difficulty)
}

#' Calcular indice de discriminacion de items
#'
#' El indice de discriminacion mide que tan bien un item distingue entre
#' estudiantes de alto y bajo rendimiento. Se calcula como la correlacion
#' punto-biserial entre el item y el puntaje total.
#'
#' @param scored_data Data frame con respuestas calificadas
#' @param method Metodo de calculo: "pbis" (punto-biserial), "d27" (grupos extremos)
#' @param id_col Columna de identificacion de estudiante. NULL autodetecta
#'   entre "student_id" y "Rut"
#'
#' @return Data frame con indices de discriminacion por item
#' @export
calculate_discrimination_index <- function(scored_data, 
                                            method = c("pbis", "d27"),
                                            id_col = NULL) {
  method <- match.arg(method)

  id_col <- resolve_id_col(scored_data, id_col)
  scored_data <- normalize_correct_col(scored_data)
  
  # Calcular puntaje total por estudiante
  student_totals <- scored_data %>%
    group_by(across(all_of(id_col))) %>%
    summarise(
      total_correctas = sum(is_correcta, na.rm = TRUE),
      n_items = n(),
      .groups = "drop"
    )
  
  # Unir puntajes totales
  scored_with_total <- scored_data %>%
    left_join(student_totals, by = id_col)
  
  if (method == "pbis") {
    # Correlacion punto-biserial
    discrimination <- scored_with_total %>%
      group_by(Origen) %>%
      summarise(
        n_respuestas = n(),
        discriminacion = cor(as.numeric(is_correcta), total_correctas, 
                             use = "pairwise.complete.obs"),
        .groups = "drop"
      )
  } else {
    # Metodo D27 (grupos extremos 27% superior e inferior)
    n_students <- nrow(student_totals)
    cutoff_n <- ceiling(n_students * 0.27)
    
    # Identificar grupos
    student_totals <- student_totals %>%
      arrange(desc(total_correctas)) %>%
      mutate(
        grupo = case_when(
          row_number() <= cutoff_n ~ "alto",
          row_number() > n() - cutoff_n ~ "bajo",
          TRUE ~ "medio"
        )
      )
    
    # Calcular proporcion correcta por grupo
    scored_with_groups <- scored_with_total %>%
      left_join(student_totals %>% select(all_of(id_col), grupo), by = id_col) %>%
      filter(grupo %in% c("alto", "bajo"))
    
    discrimination <- scored_with_groups %>%
      group_by(Origen, grupo) %>%
      summarise(prop_correcta = mean(is_correcta, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = grupo, values_from = prop_correcta, values_fill = 0) %>%
      mutate(
        discriminacion = alto - bajo,
        n_respuestas = NA_integer_
      ) %>%
      select(Origen, n_respuestas, discriminacion)
  }
  
  # Clasificar discriminacion
  discrimination <- discrimination %>%
    mutate(
      clasificacion = case_when(
        is.na(discriminacion) ~ "No calculable",
        discriminacion < 0 ~ "Negativa (revisar)",
        discriminacion < 0.20 ~ "Baja",
        discriminacion < 0.30 ~ "Moderada",
        discriminacion < 0.40 ~ "Buena",
        TRUE ~ "Excelente"
      )
    )
  
  return(discrimination)
}

#' Calcular confiabilidad de la prueba (KR-20 / Alpha de Cronbach)
#'
#' Para items dicotomicos (correcto/incorrecto), KR-20 es equivalente a Alpha.
#' Valores > 0.7 se consideran aceptables, > 0.8 buenos, > 0.9 excelentes.
#'
#' @param scored_data Data frame con respuestas calificadas
#' @param id_col Columna de identificacion de estudiante. NULL autodetecta
#'   entre "student_id" y "Rut"
#'
#' @return Lista con coeficiente de confiabilidad y estadisticas relacionadas
#' @export
calculate_reliability <- function(scored_data, id_col = NULL) {

  id_col <- resolve_id_col(scored_data, id_col)
  scored_data <- normalize_correct_col(scored_data)
  
  # Crear matriz de respuestas (filas = estudiantes, columnas = items)
  response_matrix <- scored_data %>%
    select(all_of(id_col), Origen, is_correcta) %>%
    mutate(is_correcta = as.numeric(is_correcta)) %>%
    pivot_wider(
      names_from = Origen,
      values_from = is_correcta,
      values_fn = first
    ) %>%
    select(-all_of(id_col)) %>%
    as.matrix()
  
  # Remover columnas con varianza 0
  item_vars <- apply(response_matrix, 2, var, na.rm = TRUE)
  response_matrix <- response_matrix[, item_vars > 0, drop = FALSE]
  
  # Calcular KR-20 / Alpha de Cronbach
  n_items <- ncol(response_matrix)
  n_students <- nrow(response_matrix)

  # KR-20 no esta definido con menos de 2 items con varianza
  if (n_items < 2) {
    stop(sprintf(
      "KR-20 requiere al menos 2 items con varianza; se encontraron %d. Revisa los datos.",
      n_items
    ), call. = FALSE)
  }

  # Proporciones de respuestas correctas por item
  p <- colMeans(response_matrix, na.rm = TRUE)
  q <- 1 - p

  # Suma de p*q
  sum_pq <- sum(p * q)

  # Varianza del puntaje total
  total_scores <- rowSums(response_matrix, na.rm = TRUE)
  var_total <- var(total_scores, na.rm = TRUE)

  if (is.na(var_total) || var_total == 0) {
    stop("KR-20 no es calculable: todos los estudiantes obtuvieron el mismo puntaje.",
         call. = FALSE)
  }

  # KR-20
  kr20 <- (n_items / (n_items - 1)) * (1 - sum_pq / var_total)
  
  # Error estandar de medida (SEM). KR-20 puede superar 1 por poco cuando la
  # prueba es muy consistente (varianza muestral vs poblacional); sin acotar,
  # sqrt() de un negativo devolvia NaN.
  sem <- sqrt(var_total) * sqrt(max(0, 1 - kr20))
  
  # Clasificacion
  clasificacion <- case_when(
    kr20 < 0.5 ~ "Inaceptable",
    kr20 < 0.6 ~ "Pobre",
    kr20 < 0.7 ~ "Cuestionable",
    kr20 < 0.8 ~ "Aceptable",
    kr20 < 0.9 ~ "Bueno",
    TRUE ~ "Excelente"
  )
  
  list(
    kr20 = kr20,
    alpha = kr20,  # Son equivalentes para items dicotomicos
    sem = sem,
    n_items = n_items,
    n_students = n_students,
    var_total = var_total,
    clasificacion = clasificacion,
    mean_score = mean(total_scores, na.rm = TRUE),
    sd_score = sd(total_scores, na.rm = TRUE)
  )
}

#' Analisis de distractores
#'
#' Analiza la distribucion de respuestas por alternativa, separando por
#' grupos de rendimiento (alto, medio, bajo).
#'
#' @param scored_data Data frame con respuestas calificadas
#' @param id_col Columna de identificacion de estudiante. NULL autodetecta
#'   entre "student_id" y "Rut"
#'
#' @return Data frame con analisis de distractores por item
#' @export
analyze_distractors <- function(scored_data, id_col = NULL) {

  id_col <- resolve_id_col(scored_data, id_col)
  scored_data <- normalize_correct_col(scored_data)
  
  # Calcular puntaje total y clasificar en grupos
  student_totals <- scored_data %>%
    group_by(across(all_of(id_col))) %>%
    summarise(total = sum(is_correcta, na.rm = TRUE), .groups = "drop")
  
  # Terciles para grupos
  terciles <- quantile(student_totals$total, probs = c(1/3, 2/3), na.rm = TRUE)
  
  student_totals <- student_totals %>%
    mutate(
      grupo = case_when(
        total <= terciles[1] ~ "Bajo",
        total <= terciles[2] ~ "Medio",
        TRUE ~ "Alto"
      )
    )
  
  # Unir grupos y analizar respuestas
  scored_with_groups <- scored_data %>%
    left_join(student_totals %>% select(all_of(id_col), grupo), by = id_col)
  
  # Contar respuestas por alternativa y grupo
  distractor_analysis <- scored_with_groups %>%
    group_by(Origen, Correcta, grupo, Respuesta) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(Origen, Correcta, grupo) %>%
    mutate(
      total_grupo = sum(n),
      prop = n / total_grupo
    ) %>%
    ungroup() %>%
    mutate(
      es_correcta = toupper(Respuesta) == toupper(Correcta),
      es_omision = is.na(Respuesta) | Respuesta == ""
    )
  
  return(distractor_analysis)
}

#' Comparar equivalencia entre formas
#'
#' Analiza si las diferentes formas/versiones del examen son equivalentes
#' en terminos de dificultad.
#'
#' @param scored_data Data frame con respuestas calificadas
#' @param id_col Columna de identificacion de estudiante. NULL autodetecta
#'   entre "student_id" y "Rut"
#'
#' @return Lista con estadisticas de comparacion entre formas
#' @export
compare_forms <- function(scored_data, id_col = NULL) {

  id_col <- resolve_id_col(scored_data, id_col)
  scored_data <- normalize_correct_col(scored_data)
  
  # Calcular estadisticas por forma
  form_stats <- scored_data %>%
    group_by(Forma, across(all_of(id_col))) %>%
    summarise(
      n_items = n(),
      correctas = sum(is_correcta, na.rm = TRUE),
      porcentaje = 100 * correctas / n_items,
      .groups = "drop"
    ) %>%
    group_by(Forma) %>%
    summarise(
      n_estudiantes = n(),
      promedio = mean(porcentaje, na.rm = TRUE),
      mediana = median(porcentaje, na.rm = TRUE),
      desv_std = sd(porcentaje, na.rm = TRUE),
      minimo = min(porcentaje, na.rm = TRUE),
      maximo = max(porcentaje, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(Forma)
  
  # ANOVA si hay mas de una forma
  if (n_distinct(scored_data$Forma) > 1) {
    student_scores <- scored_data %>%
      group_by(Forma, across(all_of(id_col))) %>%
      summarise(porcentaje = 100 * mean(is_correcta, na.rm = TRUE), .groups = "drop")
    
    anova_result <- tryCatch({
      aov_model <- aov(porcentaje ~ factor(Forma), data = student_scores)
      summary(aov_model)[[1]]
    }, error = function(e) NULL)
    
    # Calcular eta cuadrado si ANOVA exitoso
    if (!is.null(anova_result)) {
      ss_between <- anova_result$`Sum Sq`[1]
      ss_total <- sum(anova_result$`Sum Sq`)
      eta_squared <- ss_between / ss_total
      p_value <- anova_result$`Pr(>F)`[1]
    } else {
      eta_squared <- NA
      p_value <- NA
    }
  } else {
    eta_squared <- NA
    p_value <- NA
  }
  
  list(
    form_stats = form_stats,
    n_forms = n_distinct(scored_data$Forma),
    anova_p_value = p_value,
    eta_squared = eta_squared,
    forms_equivalent = is.na(p_value) || p_value > 0.05
  )
}

#' Detectar items problematicos
#'
#' Identifica items que podrian necesitar revision basandose en
#' criterios psicometricos.
#'
#' @param difficulty Data frame de indices de dificultad
#' @param discrimination Data frame de indices de discriminacion
#'
#' @return Data frame con items problematicos y razones
#' @export
detect_problematic_items <- function(difficulty, discrimination) {
  
  # Unir datos
  item_analysis <- difficulty %>%
    left_join(
      discrimination %>% select(Origen, discriminacion, clasificacion_disc = clasificacion),
      by = "Origen"
    ) %>%
    rename(clasificacion_dif = clasificacion)
  
  # Identificar problemas
  item_analysis <- item_analysis %>%
    mutate(
      problemas = case_when(
        discriminacion < 0 ~ "Discriminacion negativa",
        dificultad < 0.10 ~ "Demasiado dificil",
        dificultad > 0.95 ~ "Demasiado facil",
        discriminacion < 0.15 ~ "Baja discriminacion",
        TRUE ~ NA_character_
      ),
      requiere_revision = !is.na(problemas)
    )
  
  # Filtrar solo problematicos
  problematic <- item_analysis %>%
    filter(requiere_revision) %>%
    arrange(discriminacion)
  
  list(
    all_items = item_analysis,
    problematic_items = problematic,
    n_problematic = nrow(problematic),
    pct_problematic = 100 * nrow(problematic) / nrow(item_analysis)
  )
}

#' Realizar analisis psicometrico completo
#'
#' @param scored_data Data frame con respuestas calificadas
#' @param id_col Columna de identificacion de estudiante. NULL autodetecta
#'   entre "student_id" y "Rut"
#'
#' @return Lista con todos los analisis psicometricos
#' @export
run_psychometric_analysis <- function(scored_data, id_col = NULL) {

  id_col <- resolve_id_col(scored_data, id_col)
  message(sprintf("Ejecutando analisis psicometrico (id: %s)...", id_col))
  
  # Calcular todos los indices
  difficulty <- calculate_difficulty_index(scored_data)
  message("  [OK] Indices de dificultad calculados")
  
  discrimination <- calculate_discrimination_index(scored_data, id_col = id_col)
  message("  [OK] Indices de discriminacion calculados")
  
  reliability <- calculate_reliability(scored_data, id_col = id_col)
  message(sprintf("  [OK] Confiabilidad (KR-20): %.3f (%s)", reliability$kr20, reliability$clasificacion))
  
  distractors <- analyze_distractors(scored_data, id_col = id_col)
  message("  [OK] Analisis de distractores completado")
  
  form_comparison <- compare_forms(scored_data, id_col = id_col)
  message(sprintf("  [OK] Comparacion de %d formas completada", form_comparison$n_forms))
  
  problematic <- detect_problematic_items(difficulty, discrimination)
  message(sprintf("  [OK] Detectados %d items problematicos (%.1f%%)", 
                  problematic$n_problematic, problematic$pct_problematic))
  
  list(
    difficulty = difficulty,
    discrimination = discrimination,
    reliability = reliability,
    distractors = distractors,
    form_comparison = form_comparison,
    problematic_items = problematic,
    timestamp = Sys.time()
  )
}
