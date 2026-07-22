# ==============================================================================
# PREVISUALIZACION Y VALIDACION DE PREGUNTAS
# ==============================================================================
# El editor necesita dos cosas del motor que no dependen de generar un examen
# entero: ver como se ve UNA pregunta, y saber si compila antes de guardarla.
# Ambas tejen el ejercicio con xexams, el mismo motor comun bajo todos los
# exams2*.
# ==============================================================================

#' Extraer texto, alternativas y clave de un item ya tejido
#'
#' Un item de xexams trae el enunciado en `question`, las alternativas en
#' `questionlist` y la solucion logica en `metainfo$solution`. Esta funcion
#' reune eso, y es la pieza que comparten export_items_json() y preview.
#'
#' @param q Un item de la lista que devuelve xexams
#'
#' @return Lista con Texto, Alternativas, Correcta, Origen y Seed
#' @keywords internal
parse_exam_item <- function(q) {
  mi <- q$metainfo
  get_or_null <- function(x, name) if (!is.null(x[[name]])) x[[name]] else NULL

  question_lines <- q$question[nzchar(q$question)]
  texto <- paste(question_lines, collapse = " ")

  alternativas <- q$questionlist

  origen <- get_or_null(mi, "name")
  if (is.null(origen)) origen <- get_or_null(mi, "file")

  sol_log <- get_or_null(mi, "solution")
  letra_correcta <- if (is.null(sol_log)) {
    NA_character_
  } else {
    idx <- which(sol_log)
    if (length(idx) == 1) letters[idx] else paste(letters[idx], collapse = ",")
  }

  seed <- get_or_null(mi, "seed")
  seed <- if (is.null(seed)) NA_integer_ else as.integer(seed)

  list(
    Texto = texto,
    Alternativas = alternativas,
    Correcta = letra_correcta,
    Origen = origen,
    Seed = seed
  )
}

#' Materializar el codigo fuente de una pregunta a un archivo .Rnw
#'
#' Acepta la fuente en las formas que usan los distintos llamadores: la ruta a
#' un .Rnw, un vector de lineas, o el texto completo.
#'
#' @param source Ruta a un archivo, o el contenido del ejercicio
#' @param dir Directorio donde escribir
#'
#' @return Ruta al .Rnw escrito
#' @keywords internal
materialize_source <- function(source, dir) {
  # Una ruta es una sola cadena corta sin saltos de linea. Comprobar file.exists
  # sobre el contenido completo de un ejercicio dispara warnings de codificacion.
  es_ruta <- length(source) == 1 && !grepl("\n", source, fixed = TRUE) &&
    nchar(source) < 1000 && file.exists(source)

  if (es_ruta) {
    destino <- file.path(dir, basename(source))
    file.copy(source, destino, overwrite = TRUE)
    return(destino)
  }

  slug <- tryCatch(extract_slug(source), error = function(e) "pregunta")
  destino <- file.path(dir, paste0(slug, ".Rnw"))
  writeLines(paste(source, collapse = "\n"), destino, useBytes = TRUE)
  destino
}

#' Tejer una sola pregunta
#'
#' @param source Ruta al .Rnw o su contenido
#' @param seed Semilla para fijar la instancia. NULL sortea una
#'
#' @return El item tejido (crudo, de xexams)
#' @keywords internal
weave_question <- function(source, seed = NULL) {
  if (!requireNamespace("exams", quietly = TRUE)) {
    stop("Previsualizar preguntas necesita el paquete 'exams'.")
  }

  edir <- tempfile("preview")
  dir.create(edir, recursive = TRUE)
  on.exit(unlink(edir, recursive = TRUE), add = TRUE)

  rnw <- materialize_source(source, edir)
  outdir <- tempfile("weave")
  dir.create(outdir, recursive = TRUE)
  on.exit(unlink(outdir, recursive = TRUE), add = TRUE)

  with_exams_attached()

  seed_arg <- if (is.null(seed)) NULL else matrix(as.integer(seed), 1, 1)

  resultado <- exams::xexams(
    basename(rnw),
    n = 1,
    seed = seed_arg,
    driver = list(sweave = list(quiet = TRUE)),
    edir = edir,
    dir = outdir
  )

  resultado[[1]][[1]]
}

#' Previsualizar una pregunta
#'
#' Teje una instancia de la pregunta y devuelve su enunciado, alternativas y la
#' letra correcta. Es la lectura central del editor: muestra como se ve una
#' pregunta sin generar un examen completo.
#'
#' @param source Ruta a un archivo .Rnw, o el contenido del ejercicio
#' @param seed Semilla para fijar la instancia. NULL sortea una; fijarla permite
#'   reproducir exactamente la misma version
#' @param as_html Si TRUE, agrega un campo `html` con la pregunta renderizada,
#'   con las formulas ya convertidas (via exams2html)
#'
#' @return Lista con Texto, Alternativas, Correcta, Origen, Seed y, si se pide,
#'   html
#' @export
#'
#' @examples
#' \dontrun{
#' preview_question("questions/EcCuad_1.Rnw")
#' preview_question("questions/EcCuad_1.Rnw", seed = 42, as_html = TRUE)
#' }
preview_question <- function(source, seed = NULL, as_html = FALSE) {
  q <- weave_question(source, seed = seed)
  preview <- parse_exam_item(q)

  if (isTRUE(as_html)) {
    preview$html <- render_question_html(source, seed = preview$Seed)
  }

  preview
}

#' Renderizar una pregunta a HTML
#' @keywords internal
render_question_html <- function(source, seed = NULL) {
  edir <- tempfile("html_src")
  dir.create(edir, recursive = TRUE)
  on.exit(unlink(edir, recursive = TRUE), add = TRUE)

  rnw <- materialize_source(source, edir)
  outdir <- tempfile("html_out")
  dir.create(outdir, recursive = TRUE)
  on.exit(unlink(outdir, recursive = TRUE), add = TRUE)

  with_exams_attached()
  seed_arg <- if (is.null(seed) || is.na(seed)) NULL else matrix(as.integer(seed), 1, 1)

  tryCatch({
    # pandoc-mathjax convierte la matematica a MathJax; evita depender de 'tth',
    # que es el converter por defecto de exams y no siempre esta instalado.
    exams::exams2html(
      basename(rnw),
      n = 1,
      seed = seed_arg,
      edir = edir,
      dir = outdir,
      converter = "pandoc-mathjax",
      quiet = TRUE
    )
    archivo <- dir(outdir, pattern = "\\.html$", full.names = TRUE)[1]
    if (is.na(archivo)) NA_character_ else paste(readLines(archivo, warn = FALSE), collapse = "\n")
  }, error = function(e) NA_character_)
}

#' Validar que una pregunta compile
#'
#' Teje la pregunta y reporta si se pudo, sin detenerse con un error. El editor
#' lo usa para negarse a guardar una pregunta con codigo R roto.
#'
#' @param source Ruta a un archivo .Rnw, o el contenido del ejercicio
#' @param seed Semilla para el intento
#'
#' @return Lista con `valid` (logico), `message`, y si es valido el `type` y una
#'   `preview` con texto y alternativas
#' @export
#'
#' @examples
#' \dontrun{
#' validate_question("questions/EcCuad_1.Rnw")
#' }
validate_question <- function(source, seed = NULL) {
  resultado <- tryCatch({
    q <- weave_question(source, seed = seed)
    preview <- parse_exam_item(q)
    tipo <- q$metainfo$type

    problemas <- character(0)
    if (is.null(tipo) || is.na(tipo)) {
      problemas <- c(problemas, "no declara \\extype")
    }
    if (length(preview$Alternativas) == 0) {
      problemas <- c(problemas, "no tiene alternativas")
    }
    if (is.na(preview$Correcta)) {
      problemas <- c(problemas, "no declara respuesta correcta (\\exsolution)")
    }

    list(
      valid = length(problemas) == 0,
      message = if (length(problemas) == 0) "La pregunta compila correctamente" else
        paste("La pregunta compila pero:", paste(problemas, collapse = "; ")),
      type = tipo,
      preview = preview
    )
  }, error = function(e) {
    list(
      valid = FALSE,
      message = sprintf("La pregunta no compila: %s", conditionMessage(e)),
      type = NA_character_,
      preview = NULL
    )
  })

  resultado
}

#' Previsualizar una pregunta de un store
#'
#' Trae el codigo fuente del store y lo previsualiza. Es lo que llama el editor
#' al seleccionar una pregunta de la lista.
#'
#' @param store Objeto question_store
#' @param id Identificador de la pregunta (question_id)
#' @param seed Semilla para fijar la instancia
#' @param as_html Si TRUE, incluye la pregunta renderizada a HTML
#'
#' @return Lista con la previsualizacion (ver preview_question)
#' @export
#'
#' @examples
#' \dontrun{
#' store <- question_store_mongo()
#' qs_preview(store, "logica_1-v1")
#' }
qs_preview <- function(store, id, seed = NULL, as_html = FALSE) {
  pregunta <- qs_get(store, id)
  preview_question(pregunta$source[1], seed = seed, as_html = as_html)
}
