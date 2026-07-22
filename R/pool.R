# ==============================================================================
# POOL DE PREGUNTAS
# ==============================================================================

#' Crear pool de preguntas
#'
#' Cuando no se indican topicos explicitamente, se leen del campo \\exsection de
#' cada ejercicio. Solo si el ejercicio no lo declara se cae al patron del nombre
#' de archivo, que es como funcionaba antes.
#'
#' @param questions Vector de nombres de archivos .Rnw o data.frame con columnas 'file' y 'topic'
#' @param topics Vector de temas/ejes correspondientes a cada pregunta (si questions es vector)
#' @param base_dir Directorio base donde estan los archivos .Rnw
#'
#' @return Objeto de clase "question_pool"
#' @export
#'
#' @examples
#' # Opcion 1: Vectores separados
#' pool <- create_question_pool(
#'   questions = c("ProgArit_1.Rnw", "ProgArit_2.Rnw", "Sumatoria_1.Rnw"),
#'   topics = c("Progresiones", "Progresiones", "Sumatorias")
#' )
#'
#' # Opcion 2: Data frame
#' pool <- create_question_pool(
#'   questions = data.frame(
#'     file = c("ProgArit_1.Rnw", "Sumatoria_1.Rnw"),
#'     topic = c("Progresiones", "Sumatorias")
#'   )
#' )
#'
#' \dontrun{
#' # Opcion 3: dejar que cada ejercicio declare su eje en \exsection
#' pool <- create_question_pool(
#'   questions = c("EcCuad_1.Rnw", "logica_1.Rnw"),
#'   base_dir = "questions"
#' )
#' }
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
      topics <- infer_topics(files, base_dir)
    }
  }

  if (length(files) != length(topics)) {
    stop("El numero de preguntas debe coincidir con el numero de topicos")
  }
  
  # Crear lookup de archivo a topico
  # Usar nombre sin extension como clave
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

#' Inferir el eje de cada pregunta
#'
#' Prefiere el campo \\exsection declarado por el propio ejercicio. Cae al
#' patron del nombre de archivo solo para los que no lo declaran, de modo que
#' un pool mixto funciona sin intervencion.
#'
#' @param files Nombres de archivo de los ejercicios
#' @param base_dir Directorio base
#'
#' @return Vector de topicos, uno por archivo
#' @keywords internal
infer_topics <- function(files, base_dir = ".") {
  rutas <- file.path(base_dir, files)

  desde_seccion <- vapply(rutas, function(r) {
    if (!file.exists(r)) return(NA_character_)
    as.character(read_exercise_metadata(r, fields = "section")$section)[1]
  }, character(1), USE.NAMES = FALSE)

  # Fallback: el patron del nombre de archivo, quitando la extension y el numero
  desde_nombre <- gsub("_\\d+$", "", tools::file_path_sans_ext(basename(files)))

  topics <- ifelse(is.na(desde_seccion), desde_nombre, desde_seccion)

  n_seccion <- sum(!is.na(desde_seccion))
  if (n_seccion == length(files)) {
    message(sprintf("Ejes leidos de \\exsection: %s",
                    paste(unique(topics), collapse = ", ")))
  } else if (n_seccion > 0) {
    message(sprintf(
      "Ejes: %d de %d desde \\exsection, el resto inferidos del nombre de archivo",
      n_seccion, length(files)
    ))
  } else {
    message("Topicos inferidos de nombres de archivo: ",
            paste(unique(topics), collapse = ", "))
  }

  topics
}

#' Validar que existan todos los archivos del pool
#'
#' @param pool Objeto question_pool
#' @param stop_on_missing Si TRUE, detiene con error si faltan archivos
#'
#' @return Vector logico indicando que archivos existen
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
    message(sprintf("[OK] Todos los %d archivos de preguntas encontrados", pool$n_questions))
  }
  
  invisible(exists)
}

#' Print method for question_pool objects
#'
#' @param x A question_pool object
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns the input object
#' @export
print.question_pool <- function(x, ...) {
  cat(strrep("=", 55), "\n", sep = "")
  cat("POOL DE PREGUNTAS\n")
  cat(strrep("=", 55), "\n", sep = "")
  cat(sprintf("  Total preguntas: %d\n", x$n_questions))
  cat(sprintf("  Directorio:      %s\n", x$base_dir))
  cat(sprintf("  Topicos (%d):\n", length(x$unique_topics)))
  
  # Contar preguntas por topico
  topic_counts <- table(x$topics)
  for (topic in names(topic_counts)) {
    cat(sprintf("    - %s: %d preguntas\n", topic, topic_counts[topic]))
  }
  cat(strrep("=", 55), "\n", sep = "")
  invisible(x)
}

#' Crear pool de preguntas desde patron de nombres
#'
#' @param patterns Lista nombrada con patrones: list(Tema1 = "Patron1_", Tema2 = "Patron2_").
#'   Un mismo tema puede repetirse para asociarle varios patrones, o el elemento
#'   puede ser un vector de patrones: list(Tema1 = c("Patron1_", "Patron2_"))
#' @param nums Rango de numeros para cada patron (default: 1:5)
#' @param extension Extension de archivo (default: ".Rnw")
#'
#' @return Objeto question_pool
#' @export
#'
#' @examples
#' # Un patron por tema
#' pool <- create_pool_from_patterns(
#'   patterns = list(
#'     "Progresiones" = "ProgArit_",
#'     "Sumatorias" = "Sumatoria_"
#'   ),
#'   nums = 1:5
#' )
#'
#' # Varios patrones para el mismo tema (nombre repetido o vector)
#' pool <- create_pool_from_patterns(
#'   patterns = list(
#'     "Progresiones" = c("ProgArit_", "ProgGeom_"),
#'     "Sumatorias" = "Sumatoria_"
#'   ),
#'   nums = 1:5
#' )
create_pool_from_patterns <- function(patterns, nums = 1:5, extension = ".Rnw") {

  if (is.null(names(patterns)) || any(!nzchar(names(patterns)))) {
    stop("'patterns' debe ser una lista nombrada: list(Tema = \"Patron_\")")
  }

  # Iterar por indice: patterns[[nombre]] devolveria siempre el primer elemento
  # cuando hay nombres repetidos, perdiendo patrones silenciosamente.
  files <- c()
  topics <- c()

  for (i in seq_along(patterns)) {
    topic <- names(patterns)[i]
    for (pattern in patterns[[i]]) {
      new_files <- paste0(pattern, nums, extension)
      files <- c(files, new_files)
      topics <- c(topics, rep(topic, length(nums)))
    }
  }

  if (anyDuplicated(files)) {
    dups <- unique(files[duplicated(files)])
    warning(sprintf(
      "Patrones generan archivos duplicados (%d):\n  %s",
      length(dups), paste("-", dups, collapse = "\n  ")
    ))
  }

  create_question_pool(files, topics)
}
