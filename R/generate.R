# ==============================================================================
# GENERACION DE EXAMENES Y EXPORTACION DE ITEMS
# ==============================================================================

#' Generar matrices de seeds y orden para examenes
#'
#' Las formas de un mismo grupo son el MISMO examen con las preguntas en distinto
#' orden. Para lograrlo, la semilla debe seguir a la pregunta y no a la posicion:
#' exams2pdf indexa `seed` por columna, es decir por posicion dentro del examen,
#' de modo que la fila de semillas se permuta con el mismo orden que la fila de
#' preguntas.
#'
#' @param config Objeto exam_config
#' @param pool Objeto question_pool
#' @param order_seed Seed para la aleatorizacion del orden
#'
#' @return Lista con seed_matrix, order_matrix y question_matrix
#' @keywords internal
generate_exam_matrices <- function(config, pool, order_seed = 1234) {
  num_questions <- pool$n_questions
  num_forms <- config$num_forms

  # Una semilla por pregunta y por grupo, indexada por identidad de la pregunta
  group_seeds <- list()
  for (i in seq_along(config$groups$seeds)) {
    set.seed(config$groups$seeds[i])
    group_seeds[[i]] <- sample(1:100, num_questions, replace = TRUE)
  }

  # A que grupo pertenece cada forma
  form_group <- rep(seq_along(config$groups$sizes), times = config$groups$sizes)

  # Matriz de orden (que pregunta va en que posicion)
  order_matrix <- matrix(0L, nrow = num_forms, ncol = num_questions)
  set.seed(order_seed)
  for (i in 1:num_forms) {
    order_matrix[i, ] <- sample(1:num_questions, num_questions)
  }

  # Matriz de preguntas y de semillas, permutadas con el MISMO orden para que
  # cada pregunta conserve su semilla al cambiar de posicion
  question_matrix <- matrix("", nrow = num_forms, ncol = num_questions)
  seed_matrix <- matrix(0L, nrow = num_forms, ncol = num_questions)
  for (i in 1:num_forms) {
    question_matrix[i, ] <- pool$files[order_matrix[i, ]]
    seed_matrix[i, ] <- group_seeds[[form_group[i]]][order_matrix[i, ]]
  }

  list(
    seed_matrix = seed_matrix,
    order_matrix = order_matrix,
    question_matrix = question_matrix
  )
}

#' Generar examenes PDF
#'
#' @param config Objeto exam_config
#' @param pool Objeto question_pool
#' @param order_seed Seed para aleatorizacion del orden de preguntas
#' @param verbose Mostrar progreso detallado
#'
#' @return Objeto exam_result con los resultados de la generacion
#' @export
#'
#' @examples
#' \dontrun{
#' config <- create_exam_config("algebra", 2025, 4, "Matematicas")
#' pool <- create_question_pool(c("P1.Rnw", "P2.Rnw"), c("Tema1", "Tema2"))
#' result <- generate_exams(config, pool)
#' }
generate_exams <- function(config, pool, order_seed = 1234, verbose = TRUE) {
  
  # Validar archivos
  validate_question_pool(pool)
  
  # Generar matrices
  matrices <- generate_exam_matrices(config, pool, order_seed)
  
  # Resolver rutas a absolutas ANTES de cambiar de directorio: exams2pdf se
  # ejecuta desde pool$base_dir, y una ruta relativa se resolveria contra esa
  # base en vez de contra el directorio del usuario.
  if (!dir.exists(config$output_dir)) {
    dir.create(config$output_dir, recursive = TRUE)
  }
  output_dir <- normalizePath(config$output_dir, mustWork = TRUE)

  # exams2pdf no acepta template = NULL; su default es "plain". Aceptamos NULL
  # por compatibilidad con configuraciones antiguas.
  template <- config$template
  if (is.null(template)) {
    template <- "plain"
  }
  # Un template propio se indica relativo al directorio del usuario
  template_propio <- file.exists(template)
  if (template_propio) {
    template <- normalizePath(template, mustWork = TRUE)
  }

  # El campo `forma` solo puede pasarse a un template que defina \forma. Los
  # templates que trae 'exams' no lo hacen y fallan al compilar.
  header <- config$header
  if (is.null(header) && template_propio) {
    header <- list(forma = function(i) sprintf("{%s}", config$form_labels[i]))
  }

  # Guardar matriz de orden para referencia (formato legible)
  order_file <- file.path(output_dir, paste0(config$name, "_orden.csv"))
  write.csv(matrices$order_matrix, order_file, row.names = FALSE)

  # Registro completo de generacion: sin la seed_matrix el examen no se puede
  # reproducir, y hasta ahora solo vivia en memoria.
  record_file <- file.path(output_dir, paste0(config$name, "_generacion.rds"))
  saveRDS(generation_record(config, pool, matrices, order_seed), record_file)

  tex_dir <- file.path(output_dir, "tex")
  if (!dir.exists(tex_dir)) {
    dir.create(tex_dir, recursive = TRUE)
  }

  # Un pool ligado a un store no tiene los .Rnw en disco: se materializan a un
  # directorio temporal, que pasa a ser el base_dir de esta generacion.
  base_dir <- pool$base_dir
  if (!is.null(pool$store)) {
    base_dir <- tempfile("preguntas")
    qs_materialize(pool$store, pool$question_ids, dir = base_dir)
    if (verbose) {
      message(sprintf("Materializadas %d preguntas del store en %s",
                      pool$n_questions, base_dir))
    }
  }

  # Los ejercicios se tejen en un entorno que cuelga de globalenv, asi que las
  # funciones de 'exams' que usan (answerlist, num_to_schoice...) tienen que
  # estar en el search path. Es la convencion de R/exams: los ejercicios se
  # escriben llamandolas sin calificar.
  with_exams_attached()

  # Configurar directorio de trabajo (restaurado pase lo que pase)
  if (base_dir != ".") {
    old_wd <- getwd()
    on.exit(setwd(old_wd), add = TRUE)
    setwd(base_dir)
  }

  # Generar examenes
  exams_result <- tryCatch(
    exams2pdf(
      matrices$question_matrix,
      seed = matrices$seed_matrix,
      template = template,
      usepackage = config$usepackage,
      texengine = config$tex_engine,
      verbose = verbose,
      texdir = tex_dir,
      name = config$name,
      header = header,
      dir = output_dir
    ),
    error = function(e) {
      stop(sprintf("Error generando examenes: %s", e$message), call. = FALSE)
    }
  )

  # Crear objeto resultado
  result <- list(
    config = config,
    pool = pool,
    exams_raw = exams_result,
    matrices = matrices,
    order_seed = order_seed,
    order_file = order_file,
    record_file = record_file,
    output_dir = output_dir,
    timestamp = Sys.time()
  )

  class(result) <- c("exam_result", "list")

  if (verbose) {
    message("\n[OK] Examenes generados exitosamente")
    message(sprintf("  Formas creadas: %d", config$num_forms))
    message(sprintf("  Directorio: %s", output_dir))
    message(sprintf("  Registro:   %s", basename(record_file)))
  }

  result
}

#' Asegurar que 'exams' este en el search path
#'
#' Los ejercicios .Rnw llaman a `answerlist()`, `num_to_schoice()` y companía
#' sin calificar, que es como los escribe la documentacion de R/exams. Al tejer,
#' esas funciones se buscan en el search path, no en el namespace de este
#' paquete, asi que hay que adjuntar 'exams' si el usuario no lo hizo.
#'
#' Si esta funcion lo adjunta, lo deja adjunto: desengancharlo a mitad de camino
#' romperia cualquier codigo posterior del usuario que dependa de el.
#'
#' @return Invisiblemente TRUE si 'exams' quedo adjunto
#' @keywords internal
with_exams_attached <- function() {
  if ("package:exams" %in% search()) {
    return(invisible(TRUE))
  }
  attachNamespace("exams")
  invisible(TRUE)
}

#' Construir el registro de una generacion de examenes
#'
#' Reune todo lo necesario para reproducir una generacion mas adelante: la
#' configuracion, el pool, ambas matrices y las versiones del entorno. La
#' seed_matrix es la pieza critica; sin ella el examen no se puede regenerar.
#'
#' @param config Objeto exam_config
#' @param pool Objeto question_pool
#' @param matrices Lista de generate_exam_matrices()
#' @param order_seed Seed usada para el orden
#'
#' @return Lista con el registro de generacion
#' @keywords internal
generation_record <- function(config, pool, matrices, order_seed) {
  list(
    config = config,
    pool = pool,
    matrices = matrices,
    order_seed = order_seed,
    versions = list(
      package = as.character(utils::packageVersion("PruebasUDD")),
      exams = tryCatch(as.character(utils::packageVersion("exams")),
                       error = function(e) NA_character_),
      r = paste(R.version$major, R.version$minor, sep = ".")
    ),
    created_at = Sys.time()
  )
}

#' Leer el registro de generacion de un examen
#'
#' @param file Ruta al archivo `<name>_generacion.rds`
#'
#' @return Lista con el registro de generacion
#' @export
read_generation_record <- function(file) {
  if (!file.exists(file)) {
    stop(sprintf("Registro de generacion no encontrado: %s", file))
  }
  readRDS(file)
}

#' Convertir resultados de exams a formato de items JSON
#'
#' @param exam_result Objeto exam_result de generate_exams()
#' @param output_file Ruta del archivo JSON de salida (opcional)
#'
#' @return Lista de items en formato estructurado
#' @export
export_items_json <- function(exam_result, output_file = NULL) {
  
  config <- exam_result$config
  pool <- exam_result$pool
  results <- exam_result$exams_raw
  
  # Helper para acceso seguro
  get_or_null <- function(x, name) if (!is.null(x[[name]])) x[[name]] else NULL

  exam_names <- names(results)

  # exams registra en metainfo la semilla que efectivamente uso para cada
  # ejercicio (solo cuando se genero pasando `seed`). Es la fuente autoritativa;
  # la seed_matrix queda como respaldo si por algun motivo no viniera.
  seed_matrix <- exam_result$matrices$seed_matrix

  items <- lapply(seq_along(results), function(i_exam) {
    exam <- results[[i_exam]]

    # Numero de forma
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
      
      # Semilla de ESTE item, con respaldo en la matriz [forma, posicion]
      seed <- get_or_null(mi, "seed")
      if (is.null(seed) && !is.null(seed_matrix) &&
          i_exam <= nrow(seed_matrix) && i_q <= ncol(seed_matrix)) {
        seed <- seed_matrix[i_exam, i_q]
      }
      seed <- if (is.null(seed)) NA_integer_ else as.integer(seed)

      list(
        Texto = texto,
        Alternativas = alternativas,
        Eje = unname(eje),
        Correcta = letra_correcta,
        Seed = seed,
        Origen = origen,
        Forma = forma,
        Pregunta = i_q,
        Bimestre = config$period,
        Anio = config$year,
        Prueba = config$name
      )
    })
  })
  
  # Aplanar lista
  items_flat <- unlist(items, recursive = FALSE)
  
  # Exportar si se especifica archivo
  if (!is.null(output_file)) {
    write_json(items_flat, output_file, auto_unbox = TRUE, pretty = TRUE)
    message(sprintf("[OK] Items exportados a %s (%d items)", output_file, length(items_flat)))
  }
  
  invisible(items_flat)
}
