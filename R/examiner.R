# ==============================================================================
# EXAMINER: Generación y Revisión de Pruebas
# ==============================================================================
# Un paquete flexible para generar múltiples versiones de exámenes y
# procesar los resultados de los estudiantes.
# ==============================================================================

#' @import tidyverse
#' @import exams
#' @import jsonlite
#' @import readxl
#' @import writexl
#' @importFrom knitr knit
NULL

# ==============================================================================
# CLASE S3: ExamConfig - Configuración de Examen
# ==============================================================================

#' Crear configuración de examen
#'
#' @param name Nombre identificador del examen (ej: "algebra_b4_p1")
#' @param year Año de la evaluación
#' @param period Período/bimestre (ej: 1, 2, 3, 4 o "semestre1")
#' @param subject Asignatura (ej: "Matemáticas", "Física")
#' @param num_forms Número de formas/versiones del examen (default: 10)
#' @param groups Lista con configuración de grupos: list(sizes = c(4,4,2), seeds = c(13,17,57))
#' @param template Ruta al archivo template LaTeX (opcional)
#' @param output_dir Directorio de salida para PDFs
#' @param tex_engine Motor LaTeX a usar ("xelatex", "pdflatex", etc.)
#'
#' @return Objeto de clase "exam_config"
#' @export
#'
#' @examples
#' config <- create_exam_config(
#'   name = "prueba1_algebra",
#'   year = 2025,
#'   period = 4,
#'   subject = "Algebra",
#'   num_forms = 10
#' )
create_exam_config <- function(name,
                                year,
                                period,
                                subject,
                                num_forms = 10,
                                groups = NULL,
                                template = NULL,
                                output_dir = "exams",
                                tex_engine = "xelatex") {
  

  # Valores por defecto para grupos si no se especifican

  if (is.null(groups)) {
    groups <- list(
      sizes = c(num_forms),
      seeds = c(sample(1:100, 1))
    )
  }
  
  # Validar que sizes sumen num_forms
 if (sum(groups$sizes) != num_forms) {
    stop(sprintf(
      "La suma de group sizes (%d) debe ser igual a num_forms (%d)",
      sum(groups$sizes), num_forms
    ))
  }
  
  config <- list(
    name = name,
    year = as.integer(year),
    period = period,
    subject = subject,
    num_forms = as.integer(num_forms),
    groups = groups,
    template = template,
    output_dir = output_dir,
    tex_engine = tex_engine,
    form_labels = LETTERS[1:num_forms]
  )
  
  class(config) <- c("exam_config", "list")
  return(config)
}

#' @export
print.exam_config <- function(x, ...) {
  cat("═══════════════════════════════════════════════════════\n")
  cat("CONFIGURACIÓN DE EXAMEN\n")
  cat("═══════════════════════════════════════════════════════\n")
  cat(sprintf("  Nombre:      %s\n", x$name))
  cat(sprintf("  Asignatura:  %s\n", x$subject))
  cat(sprintf("  Año:         %d\n", x$year))
  cat(sprintf("  Período:     %s\n", x$period))
  cat(sprintf("  Formas:      %d (%s)\n", x$num_forms, paste(x$form_labels, collapse = ", ")))
  cat(sprintf("  Grupos:      %d\n", length(x$groups$sizes)))
  cat(sprintf("  Output:      %s\n", x$output_dir))
  cat("═══════════════════════════════════════════════════════\n")
  invisible(x)
}

# ==============================================================================
# CLASE S3: QuestionPool - Pool de Preguntas
# ==============================================================================

#' Crear pool de preguntas
#'
#' @param questions Vector de nombres de archivos .Rnw o data.frame con columnas 'file' y 'topic'
#' @param topics Vector de temas/ejes correspondientes a cada pregunta (si questions es vector)
#' @param base_dir Directorio base donde están los archivos .Rnw
#'
#' @return Objeto de clase "question_pool"
#' @export
#'
#' @examples
#' # Opción 1: Vectores separados
#' pool <- create_question_pool(
#'   questions = c("ProgArit_1.Rnw", "ProgArit_2.Rnw", "Sumatoria_1.Rnw"),
#'   topics = c("Progresiones", "Progresiones", "Sumatorias")
#' )
#'
#' # Opción 2: Data frame
#' pool <- create_question_pool(
#'   questions = data.frame(
#'     file = c("ProgArit_1.Rnw", "Sumatoria_1.Rnw"),
#'     topic = c("Progresiones", "Sumatorias")
#'   )
#' )
create_question_pool <- function(questions, topics = NULL, base_dir = ".") {
  
  # Si es data.frame, extraer columnas

if (is.data.frame(questions)) {
    if (!all(c("file", "topic") %in% colnames(questions))) {
      stop("El data.frame debe tener columnas 'file' y 'topic'")
    }
    files <- questions$file
    topics <- questions$topic
  } else {
    files <- questions
    if (is.null(topics)) {
      # Intentar inferir tópicos del nombre del archivo
      topics <- gsub("_\\d+\\.Rnw$", "", basename(files))
      message("Tópicos inferidos de nombres de archivo: ", paste(unique(topics), collapse = ", "))
    }
  }
  
  if (length(files) != length(topics)) {
    stop("El número de preguntas debe coincidir con el número de tópicos")
  }
  
  # Crear lookup de archivo a tópico
  # Usar nombre sin extensión como clave
  names_clean <- gsub("\\.Rnw$", "", basename(files))
  topic_lookup <- setNames(topics, names_clean)
  
  pool <- list(
    files = files,
    topics = topics,
    topic_lookup = topic_lookup,
    base_dir = base_dir,
    n_questions = length(files),
    unique_topics = unique(topics)
  )
  
  class(pool) <- c("question_pool", "list")
  return(pool)
}

#' Validar que existan todos los archivos del pool
#'
#' @param pool Objeto question_pool
#' @param stop_on_missing Si TRUE, detiene con error si faltan archivos
#'
#' @return Vector lógico indicando qué archivos existen
#' @export
validate_question_pool <- function(pool, stop_on_missing = TRUE) {
  full_paths <- file.path(pool$base_dir, pool$files)
  exists <- file.exists(full_paths)
  
  if (!all(exists)) {
    missing <- pool$files[!exists]
    msg <- sprintf("Archivos faltantes (%d):\n  %s", 
                   length(missing), 
                   paste("-", missing, collapse = "\n  "))
    if (stop_on_missing) {
      stop(msg)
    } else {
      warning(msg)
    }
  } else {
    message(sprintf("✓ Todos los %d archivos de preguntas encontrados", pool$n_questions))
  }
  
  invisible(exists)
}

#' @export
print.question_pool <- function(x, ...) {
  cat("═══════════════════════════════════════════════════════\n")
  cat("POOL DE PREGUNTAS\n")
  cat("═══════════════════════════════════════════════════════\n")
  cat(sprintf("  Total preguntas: %d\n", x$n_questions))
  cat(sprintf("  Directorio:      %s\n", x$base_dir))
  cat(sprintf("  Tópicos (%d):\n", length(x$unique_topics)))
  
  # Contar preguntas por tópico
  topic_counts <- table(x$topics)
  for (topic in names(topic_counts)) {
    cat(sprintf("    - %s: %d preguntas\n", topic, topic_counts[topic]))
  }
  cat("═══════════════════════════════════════════════════════\n")
  invisible(x)
}

# ==============================================================================
# GENERACIÓN DE EXÁMENES
# ==============================================================================

#' Generar matrices de seeds y orden para exámenes
#'
#' @param config Objeto exam_config
#' @param pool Objeto question_pool
#' @param order_seed Seed para la aleatorización del orden
#'
#' @return Lista con seed_matrix y order_matrix
#' @keywords internal
generate_exam_matrices <- function(config, pool, order_seed = 1234) {
  num_questions <- pool$n_questions
  num_forms <- config$num_forms
  
  # Generar seeds por grupo
  group_seeds <- list()
  for (i in seq_along(config$groups$seeds)) {
    set.seed(config$groups$seeds[i])
    group_seeds[[i]] <- sample(1:100, num_questions, replace = TRUE)
  }
  
  # Matriz de seeds (expandir por tamaño de grupo)
  seed_matrix <- NULL
  for (i in seq_along(config$groups$sizes)) {
    group_matrix <- matrix(
      rep(group_seeds[[i]], config$groups$sizes[i]),
      nrow = config$groups$sizes[i],
      byrow = TRUE
    )
    seed_matrix <- rbind(seed_matrix, group_matrix)
  }
  
  # Matriz de orden (qué pregunta va en qué posición)
  order_matrix <- matrix(0, nrow = num_forms, ncol = num_questions)
  set.seed(order_seed)
  for (i in 1:num_forms) {
    order_matrix[i, ] <- sample(1:num_questions, num_questions)
  }
  
  # Matriz de preguntas (archivos en orden)
  question_matrix <- matrix("", nrow = num_forms, ncol = num_questions)
  for (i in 1:num_forms) {
    question_matrix[i, ] <- pool$files[order_matrix[i, ]]
  }
  
  list(
    seed_matrix = seed_matrix,
    order_matrix = order_matrix,
    question_matrix = question_matrix
  )
}

#' Generar exámenes PDF
#'
#' @param config Objeto exam_config
#' @param pool Objeto question_pool
#' @param order_seed Seed para aleatorización del orden de preguntas
#' @param verbose Mostrar progreso detallado
#'
#' @return Objeto exam_result con los resultados de la generación
#' @export
#'
#' @examples
#' \dontrun{
#' config <- create_exam_config("algebra", 2025, 4, "Matemáticas")
#' pool <- create_question_pool(c("P1.Rnw", "P2.Rnw"), c("Tema1", "Tema2"))
#' result <- generate_exams(config, pool)
#' }
generate_exams <- function(config, pool, order_seed = 1234, verbose = TRUE) {
  
  # Validar archivos
  validate_question_pool(pool)
  
  # Generar matrices
  matrices <- generate_exam_matrices(config, pool, order_seed)
  
  # Guardar matriz de orden para referencia
  order_file <- file.path(config$output_dir, paste0(config$name, "_orden.csv"))
  if (!dir.exists(config$output_dir)) {
    dir.create(config$output_dir, recursive = TRUE)
  }
  write.csv(matrices$order_matrix, order_file, row.names = FALSE)
  
  # Configurar directorio de trabajo
  old_wd <- getwd()
  if (pool$base_dir != ".") {
    setwd(pool$base_dir)
  }
  
  # Generar exámenes
  tryCatch({
    exams_result <- exams2pdf(
      matrices$question_matrix,
      seed = matrices$seed_matrix,
      template = config$template,
      texengine = config$tex_engine,
      verbose = verbose,
      texdir = file.path(config$output_dir, "tex"),
      name = config$name,
      header = list(forma = function(i) sprintf("{%s}", config$form_labels[i])),
      dir = config$output_dir
    )
    
    setwd(old_wd)
    
    # Crear objeto resultado
    result <- list(
      config = config,
      pool = pool,
      exams_raw = exams_result,
      matrices = matrices,
      order_file = order_file,
      timestamp = Sys.time()
    )
    
    class(result) <- c("exam_result", "list")
    
    if (verbose) {
      message("\n✓ Exámenes generados exitosamente")
      message(sprintf("  Formas creadas: %d", config$num_forms))
      message(sprintf("  Directorio: %s", config$output_dir))
    }
    
    return(result)
    
  }, error = function(e) {
    setwd(old_wd)
    stop(sprintf("Error generando exámenes: %s", e$message))
  })
}

# ==============================================================================
# EXPORTACIÓN DE ÍTEMS (JSON)
# ==============================================================================

#' Convertir resultados de exams a formato de ítems JSON
#'
#' @param exam_result Objeto exam_result de generate_exams()
#' @param output_file Ruta del archivo JSON de salida (opcional)
#'
#' @return Lista de ítems en formato estructurado
#' @export
export_items_json <- function(exam_result, output_file = NULL) {
  
  config <- exam_result$config
  pool <- exam_result$pool
  results <- exam_result$exams_raw
  
  # Helper para acceso seguro
  get_or_null <- function(x, name) if (!is.null(x[[name]])) x[[name]] else NULL
  
  exam_names <- names(results)
  
  items <- lapply(seq_along(results), function(i_exam) {
    exam <- results[[i_exam]]
    
    # Número de forma
    forma <- suppressWarnings(as.integer(gsub("\\D", "", exam_names[i_exam])))
    if (is.na(forma)) forma <- i_exam
    
    lapply(seq_along(exam), function(i_q) {
      q <- exam[[i_q]]
      mi <- q$metainfo
      
      # Texto de la pregunta
      question_lines <- q$question[nzchar(q$question)]
      texto <- paste(question_lines, collapse = " ")
      
      # Alternativas
      alternativas <- q$questionlist
      
      # Origen y eje
      origen <- get_or_null(mi, "name")
      if (is.null(origen)) origen <- get_or_null(mi, "file")
      
      eje <- if (!is.null(origen) && origen %in% names(pool$topic_lookup)) {
        pool$topic_lookup[origen]
      } else {
        NA_character_
      }
      
      # Respuesta correcta
      sol_log <- get_or_null(mi, "solution")
      letra_correcta <- if (is.null(sol_log)) {
        NA_character_
      } else {
        idx <- which(sol_log)
        if (length(idx) == 1) letters[idx] else paste(letters[idx], collapse = ",")
      }
      
      list(
        Texto = texto,
        Alternativas = alternativas,
        Eje = unname(eje),
        Correcta = letra_correcta,
        Seed = get_or_null(mi, "seed"),
        Origen = origen,
        Forma = forma,
        Pregunta = i_q,
        Bimestre = config$period,
        Año = config$year,
        Prueba = config$name
      )
    })
  })
  
  # Aplanar lista
  items_flat <- unlist(items, recursive = FALSE)
  
  # Exportar si se especifica archivo
  if (!is.null(output_file)) {
    write_json(items_flat, output_file, auto_unbox = TRUE, pretty = TRUE)
    message(sprintf("✓ Ítems exportados a %s (%d ítems)", output_file, length(items_flat)))
  }
  
  invisible(items_flat)
}

# ==============================================================================
# CLASE S3: ProcessingConfig - Configuración de Procesamiento
# ==============================================================================

#' Crear configuración para procesamiento de resultados
#'
#' @param results_file Ruta al archivo Excel con respuestas de estudiantes
#' @param items_file Ruta al archivo JSON con ítems/claves (generado por export_items_json)
#' @param students_file Ruta al archivo CSV con datos de estudiantes
#' @param sessions_file Ruta al archivo JSON con sesiones (opcional)
#' @param session_id ID de la sesión a procesar (opcional, si no se usa sessions_file)
#' @param num_questions Número de preguntas en la prueba
#' @param output_dir Directorio para archivos de salida
#' @param form_mapping Mapeo de letras a números para formas
#' @param generate_reports Generar reportes PDF (requiere Quarto)
#'
#' @return Objeto de clase "processing_config"
#' @export
create_processing_config <- function(results_file,
                                      items_file,
                                      students_file,
                                      sessions_file = NULL,
                                      session_id = NULL,
                                      num_questions = 25,
                                      output_dir = "reports",
                                      form_mapping = NULL,
                                      generate_reports = FALSE) {
  
  # Mapeo por defecto de formas
  if (is.null(form_mapping)) {
    form_mapping <- setNames(1:10, letters[1:10])
  }
  
  config <- list(
    files = list(
      results = results_file,
      items = items_file,
      students = students_file,
      sessions = sessions_file
    ),
    session_id = session_id,
    num_questions = num_questions,
    output_dir = output_dir,
    form_mapping = form_mapping,
    generate_reports = generate_reports
  )
  
  class(config) <- c("processing_config", "list")
  return(config)
}

#' @export
print.processing_config <- function(x, ...) {
  cat("═══════════════════════════════════════════════════════\n")
  cat("CONFIGURACIÓN DE PROCESAMIENTO\n")
  cat("═══════════════════════════════════════════════════════\n")
  cat(sprintf("  Resultados:  %s\n", x$files$results))
  cat(sprintf("  Ítems:       %s\n", x$files$items))
  cat(sprintf("  Estudiantes: %s\n", x$files$students))
  cat(sprintf("  Sesiones:    %s\n", ifelse(is.null(x$files$sessions), "(no)", x$files$sessions)))
  cat(sprintf("  Session ID:  %s\n", ifelse(is.null(x$session_id), "(todas)", x$session_id)))
  cat(sprintf("  Preguntas:   %d\n", x$num_questions))
  cat(sprintf("  Output:      %s\n", x$output_dir))
  cat(sprintf("  Reportes:    %s\n", ifelse(x$generate_reports, "Sí", "No")))
  cat("═══════════════════════════════════════════════════════\n")
  invisible(x)
}

# ==============================================================================
# FUNCIONES DE LECTURA DE DATOS
# ==============================================================================

#' Leer archivo de forma segura con manejo de errores
#' @keywords internal
safe_read <- function(path, type = c("excel", "csv", "json"), ...) {
  type <- match.arg(type)
  
  if (!file.exists(path)) {
    stop(sprintf("Archivo no encontrado: %s", path))
  }
  
  tryCatch({
    switch(type,
      excel = readxl::read_excel(path, ...),
      csv = readr::read_csv(path, show_col_types = FALSE, 
                            locale = locale(encoding = "UTF-8"), ...),
      json = {
        json_text <- readLines(path, encoding = "UTF-8", warn = FALSE)
        jsonlite::fromJSON(paste(json_text, collapse = "\n"), ...)
      }
    )
  }, error = function(e) {
    stop(sprintf("Error leyendo %s: %s", path, e$message))
  })
}

#' Cargar todos los datos necesarios para el procesamiento
#'
#' @param config Objeto processing_config
#'
#' @return Lista con todos los datos cargados
#' @export
load_processing_data <- function(config) {
  message("Cargando datos...")
  
  data <- list(
    results = safe_read(config$files$results, "excel"),
    items = safe_read(config$files$items, "json"),
    students = safe_read(config$files$students, "csv")
  )
  
  if (!is.null(config$files$sessions)) {
    sessions_raw <- safe_read(config$files$sessions, "json", simplifyDataFrame = FALSE)
    data$sessions <- process_sessions_data(sessions_raw)
  }
  
  message(sprintf("✓ Datos cargados: %d respuestas, %d ítems, %d estudiantes",
                  nrow(data$results), length(data$items), nrow(data$students)))
  
  return(data)
}

# ==============================================================================
# PROCESAMIENTO DE SESIONES (MongoDB)
# ==============================================================================

#' Procesar datos de sesiones desde formato MongoDB
#' @keywords internal
process_sessions_data <- function(sessions_data) {
  
  extract_id <- function(obj) {
    if (is.character(obj)) return(obj)
    if (is.list(obj) && "$oid" %in% names(obj)) return(obj$`$oid`)
    if (is.list(obj) && length(obj) == 1) return(obj[[1]])
    NA_character_
  }
  
  extract_date <- function(obj) {
    if (is.character(obj)) return(tryCatch(as.Date(obj), error = function(e) NA))
    if (is.list(obj) && "$date" %in% names(obj)) return(as.Date(obj$`$date`))
    if (inherits(obj, c("Date", "POSIXct"))) return(as.Date(obj))
    NA
  }
  
  sessions_list <- lapply(sessions_data, function(s) {
    tibble(
      session_id = extract_id(s$`_id`),
      assessment_id = extract_id(s$assessment_id),
      class_id = extract_id(s$class_id),
      date = extract_date(s$date),
      group_name = as.character(s$group_name),
      students = list(unlist(s$students))
    )
  })
  
  bind_rows(sessions_list)
}

#' Obtener estudiantes de una sesión específica
#' @export
get_session_students <- function(sessions, session_id) {
  session <- sessions %>%
    filter(session_id == !!session_id) %>%
    slice(1)
  
  if (nrow(session) == 0) {
    stop(sprintf("Sesión no encontrada: %s", session_id))
  }
  
  session$students[[1]]
}

# ==============================================================================
# TRANSFORMACIÓN DE DATOS DE RESULTADOS
# ==============================================================================

#' Normalizar columnas de RUT chileno
#' @keywords internal
normalize_rut_columns <- function(df) {
  
  # Buscar columnas de RUT
  rut_cols <- grep("^Rut-|^verificador-", colnames(df), value = TRUE)
  
  if (length(rut_cols) == 0) {
    # Si no hay columnas separadas, buscar columna Rut directamente
    if ("Rut" %in% colnames(df)) {
      return(df)
    }
    stop("No se encontraron columnas de RUT en los datos")
  }
  
  # Reemplazar 10 con K en verificador
  if ("verificador-0" %in% colnames(df)) {
    df <- df %>%
      mutate(`verificador-0` = ifelse(`verificador-0` == 10, "K", as.character(`verificador-0`)))
  }
  
  # Unir columnas de RUT
  df %>%
    unite("Rut", all_of(rut_cols), sep = "", remove = TRUE)
}

#' Estandarizar nombres de columnas de respuestas
#' @keywords internal
standardize_response_columns <- function(df, num_questions) {
  
  # Eliminar columnas completamente vacías
  df <- df %>%
    select(where(~ !all(is.na(.))))
  
  # Renombrar Forma-0 a Forma si existe
  if ("Forma-0" %in% colnames(df)) {
    df <- df %>% rename(Forma = `Forma-0`)
  }
  
  # Identificar columnas de respuesta y renombrarlas
  # Buscar patrones comunes: Resp*, Respuestas-*, etc.
  resp_cols <- grep("^Resp|^respuesta", colnames(df), value = TRUE, ignore.case = TRUE)
  
  if (length(resp_cols) > 0) {
    # Renombrar a formato estándar Respuestas-1, Respuestas-2, etc.
    new_names <- paste0("Respuestas-", seq_along(resp_cols))
    names(df)[match(resp_cols, names(df))] <- new_names
  }
  
  df
}

#' Convertir formas de letras a números
#' @keywords internal
convert_form_codes <- function(df, form_mapping) {
  df %>%
    mutate(Forma = tolower(trimws(as.character(Forma)))) %>%
    mutate(Forma = form_mapping[Forma]) %>%
    filter(!is.na(Forma))
}

#' Transformar datos de formato ancho a largo
#' @keywords internal
pivot_responses_long <- function(df) {
  df %>%
    pivot_longer(
      cols = matches("^Respuestas-\\d+$"),
      names_to = "Pregunta",
      values_to = "Respuesta"
    ) %>%
    mutate(
      Pregunta = as.integer(str_remove(Pregunta, "Respuestas-")),
      Respuesta = tolower(trimws(as.character(Respuesta)))
    )
}

# ==============================================================================
# SCORING Y ANÁLISIS
# ==============================================================================

#' Unir respuestas con claves y calcular correctas
#'
#' @param responses_long Data frame de respuestas en formato largo
#' @param items Lista o data frame de ítems con claves
#'
#' @return Data frame con columna is_correct
#' @export
score_responses <- function(responses_long, items) {
  
  # Convertir items a data frame si es lista
  if (is.list(items) && !is.data.frame(items)) {
    items_df <- bind_rows(items)
  } else {
    items_df <- items
  }
  
  # Asegurar tipos correctos
  items_df <- items_df %>%
    mutate(
      Forma = as.integer(Forma),
      Pregunta = as.integer(Pregunta),
      Correcta = tolower(trimws(as.character(Correcta)))
    )
  
  # Unir y calificar
  responses_long %>%
    inner_join(items_df, by = c("Forma", "Pregunta")) %>%
    mutate(
      is_correct = case_when(
        is.na(Respuesta) | Respuesta == "" ~ FALSE,
        TRUE ~ Respuesta == Correcta
      )
    )
}

#' Calcular puntajes por estudiante
#'
#' @param scored_data Data frame con respuestas calificadas
#' @param id_col Nombre de la columna de identificación (default: "Rut")
#'
#' @return Data frame con puntajes por estudiante
#' @export
calculate_student_scores <- function(scored_data, id_col = "Rut") {
  scored_data %>%
    group_by(across(all_of(id_col))) %>%
    summarise(
      n_respuestas = n(),
      correctas = sum(is_correct, na.rm = TRUE),
      porcentaje = 100 * correctas / n_respuestas,
      .groups = "drop"
    ) %>%
    arrange(desc(porcentaje))
}

#' Calcular rendimiento por pregunta/eje
#'
#' @param scored_data Data frame con respuestas calificadas
#' @param by Variable de agrupación ("Origen" o "Eje")
#'
#' @return Data frame con rendimiento por grupo
#' @export
calculate_item_performance <- function(scored_data, by = "Origen") {
  scored_data %>%
    group_by(across(all_of(by))) %>%
    summarise(
      n_respuestas = n(),
      correctas = sum(is_correct, na.rm = TRUE),
      porcentaje = 100 * correctas / n_respuestas,
      .groups = "drop"
    ) %>%
    arrange(porcentaje)
}

# ==============================================================================
# PIPELINE PRINCIPAL DE PROCESAMIENTO
# ==============================================================================

#' Procesar resultados de evaluación
#'
#' @param config Objeto processing_config
#'
#' @return Lista con todos los resultados del procesamiento
#' @export
#'
#' @examples
#' \dontrun{
#' config <- create_processing_config(
#'   results_file = "resultados.xlsx",
#'   items_file = "items.json",
#'   students_file = "estudiantes.csv",
#'   num_questions = 25
#' )
#' results <- process_assessment(config)
#' }
process_assessment <- function(config) {
  
  message("\n", strrep("═", 60))
  message("PROCESAMIENTO DE EVALUACIÓN")
  message(strrep("═", 60), "\n")
  
  # 1. Cargar datos
  data <- load_processing_data(config)
  
  # 2. Transformar resultados
  message("Transformando datos...")
  clean_results <- data$results %>%
    normalize_rut_columns() %>%
    standardize_response_columns(config$num_questions) %>%
    convert_form_codes(config$form_mapping)
  
  # 3. Normalizar respuestas
  response_cols <- grep("^Respuestas-", colnames(clean_results), value = TRUE)
  clean_results <- clean_results %>%
    mutate(across(all_of(response_cols), ~ tolower(trimws(as.character(.)))))
  
  # 4. Pivotar a formato largo
  results_long <- pivot_responses_long(clean_results)
  
  # 5. Filtrar por sesión si se especifica
  if (!is.null(config$session_id) && !is.null(data$sessions)) {
    session_students <- get_session_students(data$sessions, config$session_id)
    results_long <- results_long %>%
      filter(Rut %in% session_students)
    message(sprintf("✓ Filtrado a sesión: %d estudiantes", length(session_students)))
  }
  
  # 6. Calificar respuestas
  scored <- score_responses(results_long, data$items)
  
  # 7. Calcular métricas
  student_scores <- calculate_student_scores(scored)
  item_performance <- calculate_item_performance(scored, "Origen")
  topic_performance <- calculate_item_performance(scored, "Eje")
  
  # 8. Crear directorio de salida
  if (!dir.exists(config$output_dir)) {
    dir.create(config$output_dir, recursive = TRUE)
  }
  
  # 9. Exportar resultados
  scores_file <- file.path(config$output_dir, "puntajes_estudiantes.xlsx")
  items_file <- file.path(config$output_dir, "rendimiento_items.xlsx")
  topics_file <- file.path(config$output_dir, "rendimiento_ejes.xlsx")
  
  write_xlsx(student_scores, scores_file)
  write_xlsx(item_performance, items_file)
  write_xlsx(topic_performance, topics_file)
  
  message(sprintf("✓ Archivos exportados a %s", config$output_dir))
  
  # 10. Resumen
  message("\n", strrep("═", 60))
  message("✓ PROCESAMIENTO COMPLETO")
  message(strrep("═", 60))
  message(sprintf("  Estudiantes: %d", nrow(student_scores)))
  message(sprintf("  Respuestas:  %d", nrow(scored)))
  message(sprintf("  Promedio:    %.1f%%", mean(student_scores$porcentaje, na.rm = TRUE)))
  message(strrep("═", 60), "\n")
  
  # Retornar resultados
  invisible(list(
    scored_data = scored,
    student_scores = student_scores,
    item_performance = item_performance,
    topic_performance = topic_performance,
    config = config
  ))
}

# ==============================================================================
# FUNCIONES DE CONVENIENCIA
# ==============================================================================

#' Crear pool de preguntas desde patrón de nombres
#'
#' @param patterns Lista nombrada con patrones: list(Tema1 = "Patron1_", Tema2 = "Patron2_")
#' @param nums Rango de números para cada patrón (default: 1:5)
#' @param extension Extensión de archivo (default: ".Rnw")
#'
#' @return Objeto question_pool
#' @export
#'
#' @examples
#' pool <- create_pool_from_patterns(
#'   patterns = list(
#'     "Progresiones" = "ProgArit_",
#'     "Sumatorias" = "Sumatoria_"
#'   ),
#'   nums = 1:5
#' )
create_pool_from_patterns <- function(patterns, nums = 1:5, extension = ".Rnw") {
  
  files <- c()
  topics <- c()
  
  for (topic in names(patterns)) {
    pattern <- patterns[[topic]]
    new_files <- paste0(pattern, nums, extension)
    files <- c(files, new_files)
    topics <- c(topics, rep(topic, length(nums)))
  }
  
  create_question_pool(files, topics)
}

#' Listar sesiones disponibles
#'
#' @param sessions_file Ruta al archivo JSON de sesiones
#'
#' @return Data frame con sesiones (invisible)
#' @export
list_sessions <- function(sessions_file) {
  sessions_raw <- safe_read(sessions_file, "json", simplifyDataFrame = FALSE)
  sessions <- process_sessions_data(sessions_raw)
  
  message("\nSesiones disponibles:")
  message(strrep("-", 70))
  
  for (i in seq_len(nrow(sessions))) {
    s <- sessions[i, ]
    message(sprintf(
      "[%d] %s | %s | %d estudiantes | ID: %s",
      i, s$group_name, s$date, length(s$students[[1]]), s$session_id
    ))
  }
  
  message(strrep("-", 70))
  invisible(sessions)
}

# ==============================================================================
# GENERACIÓN DE REPORTES
# ==============================================================================

#' Obtener ruta al template incluido en el paquete
#' @keywords internal
get_template_path <- function(template_name) {
 # Buscar en inst/templates si está instalado, o en el directorio actual
 pkg_path <- system.file("templates", template_name, package = "PruebasUDD")
 
 if (pkg_path == "") {
   # Buscar en directorio local (desarrollo)
   local_path <- file.path("inst", "templates", template_name)
   if (file.exists(local_path)) {
     return(normalizePath(local_path))
   }
   stop(sprintf("Template no encontrado: %s", template_name))
 }
 
 return(pkg_path)
}

#' Preparar metadatos en formato compatible con templates
#'
#' Esta función prepara los datos en el formato exacto que esperan los templates
#' Reporte.qmd y ReporteDocente.qmd, que incluye:
#' - results: respuestas calificadas con is_correcta
#' - students: información de estudiantes
#' - results_general: puntajes por estudiante
#' - ejes: definición de ejes temáticos
#' - questions_extra: preguntas de una forma de referencia
#' - sessions: información de sesiones
#'
#' @param scored_data Data frame con respuestas calificadas
#' @param students_df Data frame con información de estudiantes
#' @param items_df Data frame con ítems/preguntas
#' @param ejes_df Data frame con definición de ejes (identificador, nombre)
#' @param sessions_df Data frame con sesiones (opcional)
#' @param reference_form Forma de referencia para questions_extra (default: última forma)
#' @param id_col Columna de identificación de estudiantes
#'
#' @return Lista con estructura compatible con templates
#' @export
prepare_report_metadata <- function(scored_data,
                                     students_df,
                                     items_df = NULL,
                                     ejes_df = NULL,
                                     sessions_df = NULL,
                                     reference_form = NULL,
                                     id_col = "student_id") {
  
  # Renombrar columna de ID si es necesario
  if (id_col != "student_id" && id_col %in% colnames(scored_data)) {
    scored_data <- scored_data %>% rename(student_id = !!sym(id_col))
  }
  
  # Renombrar is_correct a is_correcta si es necesario
  if ("is_correct" %in% colnames(scored_data) && !"is_correcta" %in% colnames(scored_data)) {
    scored_data <- scored_data %>% rename(is_correcta = is_correct)
  }
  
  # Calcular results_general (puntajes por estudiante)
  results_general <- scored_data %>%
    group_by(student_id) %>%
    summarise(
      n = n(),
      correctas = sum(is_correcta, na.rm = TRUE),
      porcentaje = 100 * correctas / n,
      .groups = "drop"
    )
  
  # Asegurar que students_df tenga columna student_id
  if (!is.null(students_df)) {
    if (id_col != "student_id" && id_col %in% colnames(students_df)) {
      students_df <- students_df %>% rename(student_id = !!sym(id_col))
    }
    # Asegurar que tenga columna name
    if (!"name" %in% colnames(students_df)) {
      name_cols <- intersect(c("nombre", "Name", "Nombre"), colnames(students_df))
      if (length(name_cols) > 0) {
        students_df <- students_df %>% rename(name = !!sym(name_cols[1]))
      }
    }
  }
  
  # Crear ejes_df si no se proporciona (inferir de scored_data)
  if (is.null(ejes_df)) {
    unique_ejes <- unique(scored_data$Eje)
    ejes_df <- tibble(
      identificador = unique_ejes,
      nombre = unique_ejes  # Usar mismo valor como nombre
    )
    message("Ejes inferidos de datos. Para nombres descriptivos, proporciona ejes_df.")
  }
  
  # Preparar questions_extra (preguntas de forma de referencia)
  if (is.null(items_df)) {
    # Intentar extraer de scored_data
    if (is.null(reference_form)) {
      reference_form <- max(scored_data$Forma, na.rm = TRUE)
    }
    questions_extra <- scored_data %>%
      filter(Forma == reference_form) %>%
      select(any_of(c("Origen", "Eje", "Texto", "Alternativas", "Correcta", "Pregunta"))) %>%
      distinct(Origen, .keep_all = TRUE)
  } else {
    if (is.null(reference_form)) {
      reference_form <- max(items_df$Forma, na.rm = TRUE)
    }
    questions_extra <- items_df %>%
      filter(Forma == reference_form) %>%
      select(any_of(c("Origen", "Eje", "Texto", "Alternativas", "Correcta", "Pregunta")))
  }
  
  # Construir lista de metadatos
  metadata <- list(
    results = scored_data,
    students = students_df,
    results_general = results_general,
    ejes = ejes_df,
    questions_extra = questions_extra,
    sessions = sessions_df,
    timestamp = Sys.time()
  )
  
  return(metadata)
}

#' Guardar metadatos para reportes
#'
#' @param metadata Lista de metadatos (de prepare_report_metadata)
#' @param file Ruta del archivo .rds de salida
#'
#' @return Ruta del archivo guardado (invisible)
#' @export
save_report_metadata <- function(metadata, file = ".__session_cache.rds") {
  saveRDS(metadata, file)
  message(sprintf("✓ Metadatos guardados en: %s", file))
  invisible(file)
}

#' Generar reporte individual de estudiante
#'
#' @param student_id ID del estudiante
#' @param session_id ID de la sesión
#' @param metadata Lista de metadatos (de prepare_report_metadata) o ruta a archivo .rds
#' @param output_dir Directorio de salida para el PDF
#' @param template Ruta al template Quarto (usa Reporte.qmd por defecto)
#' @param working_dir Directorio de trabajo para Quarto (donde están las fuentes)
#'
#' @return Lista con status y ruta del archivo generado
#' @export
#'
#' @examples
#' \dontrun{
#' # Preparar metadatos una vez
#' metadata <- prepare_report_metadata(scored_data, students, items, ejes)
#'
#' # Generar reporte para un estudiante
#' generate_student_report("12345678-9", "session123", metadata)
#' }
generate_student_report <- function(student_id,
                                     session_id,
                                     metadata,
                                     output_dir = "reportes",
                                     template = NULL,
                                     working_dir = NULL) {
  
  # Si metadata es una ruta, cargar el archivo
  if (is.character(metadata) && length(metadata) == 1) {
    if (!file.exists(metadata)) {
      stop(sprintf("Archivo de metadatos no encontrado: %s", metadata))
    }
    data_file <- normalizePath(metadata)
  } else {
    # Guardar metadatos en archivo temporal
    data_file <- tempfile(fileext = ".rds")
    saveRDS(metadata, data_file)
  }
  
  # Crear directorio de salida
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  output_dir <- normalizePath(output_dir)
  
  # Obtener template
  if (is.null(template)) {
    template <- get_template_path("Reporte.qmd")
  }
  
  # Archivo de salida
  safe_id <- gsub("[^a-zA-Z0-9]", "_", student_id)
  output_file <- file.path(output_dir, sprintf("%s.pdf", safe_id))
  
  # Construir comando Quarto
  cmd <- sprintf(
    'quarto render "%s" --to pdf -P student_id:"%s" -P session_id:"%s" -P data_file:"%s" --output-dir "%s" -o "%s.pdf"',
    template,
    student_id,
    session_id,
    data_file,
    output_dir,
    safe_id
  )
  
  # Cambiar directorio de trabajo si se especifica
  old_wd <- NULL
  if (!is.null(working_dir) && dir.exists(working_dir)) {
    old_wd <- getwd()
    setwd(working_dir)
  }
  
  # Ejecutar Quarto
  status <- system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
  
  # Restaurar directorio
  if (!is.null(old_wd)) {
    setwd(old_wd)
  }
  
  # Limpiar archivo temporal si fue creado
  if (!is.character(metadata)) {
    unlink(data_file)
  }
  
  # Verificar resultado
  Sys.sleep(0.3)  # Esperar a que se escriba el archivo
  file_exists <- file.exists(output_file)
  
  result <- list(
    student_id = student_id,
    status = if (file_exists) "success" else "failed",
    message = if (file_exists) "Reporte generado exitosamente" else sprintf("Error al generar reporte (código: %d)", status),
    output_file = if (file_exists) output_file else NA_character_
  )
  
  if (file_exists) {
    message(sprintf("✓ Reporte generado: %s", basename(output_file)))
  } else {
    warning(sprintf("✗ Error generando reporte para %s", student_id))
  }
  
  return(result)
}

#' Generar reportes para múltiples estudiantes
#'
#' @param student_ids Vector de IDs de estudiantes
#' @param session_id ID de la sesión
#' @param metadata Lista de metadatos o ruta a archivo .rds
#' @param output_dir Directorio de salida
#' @param template Ruta al template Quarto
#' @param working_dir Directorio de trabajo para Quarto
#'
#' @return Data frame con status de cada reporte
#' @export
generate_student_reports <- function(student_ids,
                                      session_id,
                                      metadata,
                                      output_dir = "reportes",
                                      template = NULL,
                                      working_dir = NULL) {
  
  message(sprintf("Generando reportes para %d estudiantes...", length(student_ids)))
  
  # Si metadata es objeto, guardarlo una vez y reutilizar
  if (!is.character(metadata)) {
    data_file <- tempfile(fileext = ".rds")
    saveRDS(metadata, data_file)
    metadata <- data_file
    on.exit(unlink(data_file))
  }
  
  results <- vector("list", length(student_ids))
  
  for (i in seq_along(student_ids)) {
    message(sprintf("  [%d/%d] %s", i, length(student_ids), student_ids[i]))
    
    results[[i]] <- generate_student_report(
      student_id = student_ids[i],
      session_id = session_id,
      metadata = metadata,
      output_dir = output_dir,
      template = template,
      working_dir = working_dir
    )
  }
  
  # Combinar resultados
  results_df <- bind_rows(results)
  
  success_count <- sum(results_df$status == "success")
  message(sprintf("\n✓ Reportes generados: %d/%d exitosos", success_count, length(student_ids)))
  
  return(results_df)
}

#' Generar reporte de sesión para docente
#'
#' @param session_id ID de la sesión
#' @param metadata Lista de metadatos (de prepare_report_metadata) o ruta a archivo .rds
#' @param output_dir Directorio de salida para el PDF
#' @param output_name Nombre del archivo de salida (sin extensión)
#' @param template Ruta al template Quarto (usa ReporteDocente.qmd por defecto)
#' @param working_dir Directorio de trabajo para Quarto
#'
#' @return Lista con status y ruta del archivo generado
#' @export
#'
#' @examples
#' \dontrun{
#' metadata <- prepare_report_metadata(scored_data, students, items, ejes, sessions)
#' generate_session_report("session123", metadata)
#' }
generate_session_report <- function(session_id,
                                     metadata,
                                     output_dir = "reportes",
                                     output_name = NULL,
                                     template = NULL,
                                     working_dir = NULL) {
  
  message("Generando reporte de sesión...")
  
  # Si metadata es una ruta, cargar el archivo
  if (is.character(metadata) && length(metadata) == 1) {
    if (!file.exists(metadata)) {
      stop(sprintf("Archivo de metadatos no encontrado: %s", metadata))
    }
    data_file <- normalizePath(metadata)
  } else {
    # Guardar metadatos en archivo temporal
    data_file <- tempfile(fileext = ".rds")
    saveRDS(metadata, data_file)
  }
  
  # Crear directorio de salida
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  output_dir <- normalizePath(output_dir)
  
  # Obtener template
  if (is.null(template)) {
    template <- get_template_path("ReporteDocente.qmd")
  }
  
  # Nombre de archivo de salida
  if (is.null(output_name)) {
    output_name <- sprintf("ReporteDocente_%s", session_id)
  }
  output_file <- file.path(output_dir, sprintf("%s.pdf", output_name))
  
  # Construir comando Quarto
  cmd <- sprintf(
    'quarto render "%s" --to pdf -P session_id:"%s" -P data_file:"%s" --output-dir "%s" -o "%s.pdf"',
    template,
    session_id,
    data_file,
    output_dir,
    output_name
  )
  
  # Cambiar directorio de trabajo si se especifica
  old_wd <- NULL
  if (!is.null(working_dir) && dir.exists(working_dir)) {
    old_wd <- getwd()
    setwd(working_dir)
  }
  
  # Ejecutar Quarto
  status <- system(cmd, ignore.stdout = FALSE, ignore.stderr = FALSE)
  
  # Restaurar directorio
  if (!is.null(old_wd)) {
    setwd(old_wd)
  }
  
  # Limpiar archivo temporal si fue creado
  if (!is.character(metadata)) {
    unlink(data_file)
  }
  
  # Verificar resultado
  Sys.sleep(0.5)
  file_exists <- file.exists(output_file)
  
  result <- list(
    session_id = session_id,
    status = if (file_exists) "success" else "failed",
    message = if (file_exists) "Reporte de sesión generado exitosamente" else sprintf("Error al generar reporte (código: %d)", status),
    output_file = if (file_exists) output_file else NA_character_
  )
  
  if (file_exists) {
    message(sprintf("✓ Reporte de sesión generado: %s", basename(output_file)))
  } else {
    warning("✗ Error generando reporte de sesión")
  }
  
  return(result)
}

#' Generar todos los reportes de una evaluación
#'
#' @param metadata Lista de metadatos o ruta a archivo .rds
#' @param session_id ID de la sesión a procesar
#' @param output_dir Directorio base de salida
#' @param generate_individual Generar reportes individuales de estudiantes
#' @param generate_session Generar reporte de sesión para docente
#' @param working_dir Directorio de trabajo para Quarto
#'
#' @return Lista con resultados de generación
#' @export
generate_all_reports <- function(metadata,
                                  session_id,
                                  output_dir = "reportes",
                                  generate_individual = TRUE,
                                  generate_session = TRUE,
                                  generate_psychometric = FALSE,
                                  working_dir = NULL) {
  
  message("\n", strrep("═", 60))
  message("GENERACIÓN DE REPORTES")
  message(strrep("═", 60), "\n")
  
  # Cargar metadatos si es ruta
  if (is.character(metadata) && length(metadata) == 1) {
    md <- readRDS(metadata)
    data_file <- normalizePath(metadata)
  } else {
    md <- metadata
    # Guardar una vez para reutilizar
    data_file <- tempfile(fileext = ".rds")
    saveRDS(metadata, data_file)
  }
  
  results <- list()
  
  # Obtener estudiantes de la sesión
  if (!is.null(md$sessions)) {
    session_row <- md$sessions %>%
      filter(session_id == !!session_id) %>%
      slice(1)
    
    if (nrow(session_row) > 0) {
      student_ids <- session_row$students[[1]]
    } else {
      student_ids <- unique(md$results$student_id)
    }
  } else {
    student_ids <- unique(md$results$student_id)
  }
  
  # Reporte de sesión
  if (generate_session) {
    session_result <- generate_session_report(
      session_id = session_id,
      metadata = data_file,
      output_dir = output_dir,
      working_dir = working_dir
    )
    results$session_report <- session_result
  }
  
  # Reportes individuales
  if (generate_individual) {
    individual_results <- generate_student_reports(
      student_ids = student_ids,
      session_id = session_id,
      metadata = data_file,
      output_dir = output_dir,
      working_dir = working_dir
    )
    results$individual_reports <- individual_results
  }
  
  # Reporte psicométrico
  if (generate_psychometric) {
    psychometric_result <- generate_operator_report(
      session_id = session_id,
      metadata = data_file,
      output_dir = output_dir,
      working_dir = working_dir
    )
    results$psychometric_report <- psychometric_result
  }
  
  # Limpiar si fue temporal
  if (!is.character(metadata)) {
    unlink(data_file)
  }
  
  message("\n", strrep("═", 60))
  message("✓ GENERACIÓN DE REPORTES COMPLETA")
  message(strrep("═", 60), "\n")
  
  invisible(results)
}

# ==============================================================================
# CONVERSIÓN A NOTAS
# ==============================================================================

#' Convertir porcentaje a nota en escala chilena (1.0 - 7.0)
#'
#' @param porcentaje Vector de porcentajes (0-100)
#' @param nota_minima Nota mínima (default: 1.0)
#' @param nota_maxima Nota máxima (default: 7.0)
#' @param nota_aprobacion Nota de aprobación (default: 4.0)
#' @param exigencia Porcentaje de exigencia para aprobar (default: 60)
#'
#' @return Vector de notas
#' @export
#'
#' @examples
#' porcentaje_to_nota(c(0, 30, 60, 80, 100))
#' # [1] 1.0 2.5 4.0 5.5 7.0
porcentaje_to_nota <- function(porcentaje,
                                nota_minima = 1.0,
                                nota_maxima = 7.0,
                                nota_aprobacion = 4.0,
                                exigencia = 60) {
  
  nota <- ifelse(
    porcentaje < exigencia,
    # Bajo el umbral de aprobación: escala entre nota_minima y nota_aprobacion
    nota_minima + (nota_aprobacion - nota_minima) * (porcentaje / exigencia),
    # Sobre el umbral: escala entre nota_aprobacion y nota_maxima
    nota_aprobacion + (nota_maxima - nota_aprobacion) * ((porcentaje - exigencia) / (100 - exigencia))
  )
  
  # Asegurar límites
  nota <- pmax(nota_minima, pmin(nota_maxima, nota))
  
  round(nota, 1)
}

#' Agregar columna de notas a puntajes de estudiantes
#'
#' @param student_scores Data frame con columna 'porcentaje'
#' @param ... Argumentos adicionales para porcentaje_to_nota
#'
#' @return Data frame con columna 'nota' agregada
#' @export
add_grades <- function(student_scores, ...) {
  student_scores %>%
    mutate(nota = porcentaje_to_nota(porcentaje, ...))
}

# ==============================================================================
# ANÁLISIS PSICOMÉTRICO
# ==============================================================================

#' Calcular índice de dificultad de ítems
#'
#' El índice de dificultad es la proporción de estudiantes que responden
#' correctamente. Valores cercanos a 0 indican ítems difíciles, cercanos a 1
#' indican ítems fáciles. El rango ideal es 0.3-0.7.
#'
#' @param scored_data Data frame con respuestas calificadas
#' @param by_form Si TRUE, calcula por forma además de global
#'
#' @return Data frame con índices de dificultad por ítem
#' @export
calculate_difficulty_index <- function(scored_data, by_form = FALSE) {
  
  # Asegurar nombre de columna
  if ("is_correct" %in% colnames(scored_data)) {
    scored_data <- scored_data %>% rename(is_correcta = is_correct)
  }
  
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
        dificultad < 0.20 ~ "Muy difícil",
        dificultad < 0.40 ~ "Difícil",
        dificultad < 0.60 ~ "Moderado",
        dificultad < 0.80 ~ "Fácil",
        TRUE ~ "Muy fácil"
      )
    )
  
  return(difficulty)
}

#' Calcular índice de discriminación de ítems
#'
#' El índice de discriminación mide qué tan bien un ítem distingue entre
#' estudiantes de alto y bajo rendimiento. Se calcula como la correlación
#' punto-biserial entre el ítem y el puntaje total.
#'
#' @param scored_data Data frame con respuestas calificadas
#' @param method Método de cálculo: "pbis" (punto-biserial), "d27" (grupos extremos)
#' @param id_col Columna de identificación de estudiante
#'
#' @return Data frame con índices de discriminación por ítem
#' @export
calculate_discrimination_index <- function(scored_data, 
                                            method = c("pbis", "d27"),
                                            id_col = "student_id") {
  method <- match.arg(method)
  
  # Asegurar nombre de columna
  if ("is_correct" %in% colnames(scored_data)) {
    scored_data <- scored_data %>% rename(is_correcta = is_correct)
  }
  
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
    # Correlación punto-biserial
    discrimination <- scored_with_total %>%
      group_by(Origen) %>%
      summarise(
        n_respuestas = n(),
        discriminacion = cor(as.numeric(is_correcta), total_correctas, 
                             use = "pairwise.complete.obs"),
        .groups = "drop"
      )
  } else {
    # Método D27 (grupos extremos 27% superior e inferior)
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
    
    # Calcular proporción correcta por grupo
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
  
  # Clasificar discriminación
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
#' Para ítems dicotómicos (correcto/incorrecto), KR-20 es equivalente a Alpha.
#' Valores > 0.7 se consideran aceptables, > 0.8 buenos, > 0.9 excelentes.
#'
#' @param scored_data Data frame con respuestas calificadas
#' @param id_col Columna de identificación de estudiante
#'
#' @return Lista con coeficiente de confiabilidad y estadísticas relacionadas
#' @export
calculate_reliability <- function(scored_data, id_col = "student_id") {
  
  # Asegurar nombre de columna
  if ("is_correct" %in% colnames(scored_data)) {
    scored_data <- scored_data %>% rename(is_correcta = is_correct)
  }
  
  # Crear matriz de respuestas (filas = estudiantes, columnas = ítems)
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
  
  # Proporciones de respuestas correctas por ítem
  p <- colMeans(response_matrix, na.rm = TRUE)
  q <- 1 - p
  
  # Suma de p*q
  sum_pq <- sum(p * q)
  
  # Varianza del puntaje total
  total_scores <- rowSums(response_matrix, na.rm = TRUE)
  var_total <- var(total_scores, na.rm = TRUE)
  
  # KR-20
  kr20 <- (n_items / (n_items - 1)) * (1 - sum_pq / var_total)
  
  # Error estándar de medida (SEM)
  sem <- sqrt(var_total) * sqrt(1 - kr20)
  
  # Clasificación
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
    alpha = kr20,  # Son equivalentes para ítems dicotómicos
    sem = sem,
    n_items = n_items,
    n_students = n_students,
    var_total = var_total,
    clasificacion = clasificacion,
    mean_score = mean(total_scores, na.rm = TRUE),
    sd_score = sd(total_scores, na.rm = TRUE)
  )
}

#' Análisis de distractores
#'
#' Analiza la distribución de respuestas por alternativa, separando por
#' grupos de rendimiento (alto, medio, bajo).
#'
#' @param scored_data Data frame con respuestas calificadas
#' @param id_col Columna de identificación de estudiante
#'
#' @return Data frame con análisis de distractores por ítem
#' @export
analyze_distractors <- function(scored_data, id_col = "student_id") {
  
  # Asegurar nombre de columna
  if ("is_correct" %in% colnames(scored_data)) {
    scored_data <- scored_data %>% rename(is_correcta = is_correct)
  }
  
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
#' en términos de dificultad.
#'
#' @param scored_data Data frame con respuestas calificadas
#' @param id_col Columna de identificación de estudiante
#'
#' @return Lista con estadísticas de comparación entre formas
#' @export
compare_forms <- function(scored_data, id_col = "student_id") {
  
  # Asegurar nombre de columna
  if ("is_correct" %in% colnames(scored_data)) {
    scored_data <- scored_data %>% rename(is_correcta = is_correct)
  }
  
  # Calcular estadísticas por forma
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
  
  # ANOVA si hay más de una forma
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

#' Detectar ítems problemáticos
#'
#' Identifica ítems que podrían necesitar revisión basándose en
#' criterios psicométricos.
#'
#' @param difficulty Data frame de índices de dificultad
#' @param discrimination Data frame de índices de discriminación
#'
#' @return Data frame con ítems problemáticos y razones
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
        discriminacion < 0 ~ "Discriminación negativa",
        dificultad < 0.10 ~ "Demasiado difícil",
        dificultad > 0.95 ~ "Demasiado fácil",
        discriminacion < 0.15 ~ "Baja discriminación",
        TRUE ~ NA_character_
      ),
      requiere_revision = !is.na(problemas)
    )
  
  # Filtrar solo problemáticos
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

#' Realizar análisis psicométrico completo
#'
#' @param scored_data Data frame con respuestas calificadas
#' @param id_col Columna de identificación de estudiante
#'
#' @return Lista con todos los análisis psicométricos
#' @export
run_psychometric_analysis <- function(scored_data, id_col = "student_id") {
  
  message("Ejecutando análisis psicométrico...")
  
  # Calcular todos los índices
  difficulty <- calculate_difficulty_index(scored_data)
  message("  ✓ Índices de dificultad calculados")
  
  discrimination <- calculate_discrimination_index(scored_data, id_col = id_col)
  message("  ✓ Índices de discriminación calculados")
  
  reliability <- calculate_reliability(scored_data, id_col = id_col)
  message(sprintf("  ✓ Confiabilidad (KR-20): %.3f (%s)", reliability$kr20, reliability$clasificacion))
  
  distractors <- analyze_distractors(scored_data, id_col = id_col)
  message("  ✓ Análisis de distractores completado")
  
  form_comparison <- compare_forms(scored_data, id_col = id_col)
  message(sprintf("  ✓ Comparación de %d formas completada", form_comparison$n_forms))
  
  problematic <- detect_problematic_items(difficulty, discrimination)
  message(sprintf("  ✓ Detectados %d ítems problemáticos (%.1f%%)", 
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

#' Generar reporte psicométrico para operador
#'
#' @param session_id ID de la sesión (puede ser NA para análisis global)
#' @param metadata Lista de metadatos o ruta a archivo .rds
#' @param output_dir Directorio de salida para el PDF
#' @param output_name Nombre del archivo de salida (sin extensión)
#' @param template Ruta al template Quarto (usa ReporteOperador.qmd por defecto)
#' @param working_dir Directorio de trabajo para Quarto
#' @param id_col Columna de identificación de estudiante
#'
#' @return Lista con status y ruta del archivo generado
#' @export
generate_operator_report <- function(session_id = NULL,
                                      metadata,
                                      output_dir = "reportes",
                                      output_name = NULL,
                                      template = NULL,
                                      working_dir = NULL,
                                      id_col = "student_id") {
  
  message("Generando reporte psicométrico para operador...")
  
  # Cargar metadatos si es ruta
  if (is.character(metadata) && length(metadata) == 1) {
    if (!file.exists(metadata)) {
      stop(sprintf("Archivo de metadatos no encontrado: %s", metadata))
    }
    md <- readRDS(metadata)
  } else {
    md <- metadata
  }
  
  # Filtrar por sesión si se especifica
  scored_data <- md$results
  if (!is.null(session_id) && !is.null(md$sessions)) {
    session_row <- md$sessions %>%
      filter(session_id == !!session_id) %>%
      slice(1)
    
    if (nrow(session_row) > 0) {
      session_students <- session_row$students[[1]]
      scored_data <- scored_data %>%
        filter(student_id %in% session_students)
    }
  }
  
  # Ejecutar análisis psicométrico
  psychometric <- run_psychometric_analysis(scored_data, id_col = id_col)
  
  # Agregar análisis al metadata
  md$psychometric <- psychometric
  
  # Guardar metadata aumentado
  data_file <- tempfile(fileext = ".rds")
  saveRDS(md, data_file)
  
  # Crear directorio de salida
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  output_dir <- normalizePath(output_dir)
  
  # Obtener template
  if (is.null(template)) {
    template <- get_template_path("ReporteOperador.qmd")
  }
  
  # Nombre de archivo
  if (is.null(output_name)) {
    if (!is.null(session_id)) {
      output_name <- sprintf("ReportePsicometrico_%s", session_id)
    } else {
      output_name <- sprintf("ReportePsicometrico_%s", format(Sys.Date(), "%Y%m%d"))
    }
  }
  output_file <- file.path(output_dir, sprintf("%s.pdf", output_name))
  
  # Construir comando Quarto
  session_param <- if (!is.null(session_id)) session_id else "ALL"
  cmd <- sprintf(
    'quarto render "%s" --to pdf -P session_id:"%s" -P data_file:"%s" --output-dir "%s" -o "%s.pdf"',
    template,
    session_param,
    data_file,
    output_dir,
    output_name
  )
  
  # Cambiar directorio si se especifica
  old_wd <- NULL
  if (!is.null(working_dir) && dir.exists(working_dir)) {
    old_wd <- getwd()
    setwd(working_dir)
  }
  
  # Ejecutar Quarto
  status <- system(cmd, ignore.stdout = FALSE, ignore.stderr = FALSE)
  
  # Restaurar directorio
  if (!is.null(old_wd)) {
    setwd(old_wd)
  }
  
  # Limpiar archivo temporal
  unlink(data_file)
  
  # Verificar resultado
  Sys.sleep(0.5)
  file_exists <- file.exists(output_file)
  
  result <- list(
    status = if (file_exists) "success" else "failed",
    message = if (file_exists) "Reporte psicométrico generado exitosamente" else sprintf("Error al generar reporte (código: %d)", status),
    output_file = if (file_exists) output_file else NA_character_,
    psychometric_summary = list(
      kr20 = psychometric$reliability$kr20,
      n_items = psychometric$reliability$n_items,
      n_problematic = psychometric$problematic_items$n_problematic,
      forms_equivalent = psychometric$form_comparison$forms_equivalent
    )
  )
  
  if (file_exists) {
    message(sprintf("✓ Reporte psicométrico generado: %s", basename(output_file)))
    message(sprintf("  KR-20: %.3f | Ítems problemáticos: %d | Formas equivalentes: %s",
                    psychometric$reliability$kr20,
                    psychometric$problematic_items$n_problematic,
                    ifelse(psychometric$form_comparison$forms_equivalent, "Sí", "No")))
  } else {
    warning("✗ Error generando reporte psicométrico")
  }
  
  return(result)
}