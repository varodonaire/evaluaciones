# ==============================================================================
# STORE DE PREGUNTAS: IMPLEMENTACION LOCAL
# ==============================================================================
# Archivos .Rnw en un directorio, que es como funcionaba el paquete antes de
# que existiera la base de datos. No hay versiones: cada archivo es su propia
# ultima version, y el question_id es el slug. Sirve para trabajar sin conexion
# y para que la suite de tests corra sin MongoDB.
# ==============================================================================

#' Store de preguntas en un directorio local
#'
#' @param base_dir Directorio con los archivos .Rnw
#' @param pattern Patron de archivos a considerar
#'
#' @return Objeto de clase "question_store_local"
#' @export
#'
#' @examples
#' \dontrun{
#' store <- question_store_local("questions")
#' qs_search(store, section = "logica")
#' }
question_store_local <- function(base_dir = ".", pattern = "\\.(Rnw|rnw)$") {
  store <- list(
    type = "local",
    base_dir = base_dir,
    pattern = pattern
  )
  class(store) <- c("question_store_local", "question_store", "list")
  store
}

#' Leer todas las preguntas del directorio como tabla
#' @keywords internal
local_index <- function(store, with_source = FALSE) {
  if (!dir.exists(store$base_dir)) {
    stop(sprintf("Directorio de preguntas no encontrado: %s", store$base_dir))
  }

  archivos <- sort(dir(store$base_dir, pattern = store$pattern, full.names = TRUE))

  if (length(archivos) == 0) {
    vacio <- tibble(
      question_id = character(0), slug = character(0), version = integer(0),
      content_hash = character(0), section = character(0),
      tags = list(), author = character(0), type = character(0),
      status = character(0)
    )
    if (with_source) vacio$source <- character(0)
    return(vacio)
  }

  metas <- lapply(archivos, read_exercise_metadata)
  fuentes <- vapply(archivos, function(f) {
    paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  }, character(1), USE.NAMES = FALSE)

  slugs <- vapply(seq_along(archivos), function(i) {
    nombre <- as.character(metas[[i]]$name)[1]
    if (is.na(nombre)) tools::file_path_sans_ext(basename(archivos[i])) else nombre
  }, character(1))

  versiones <- vapply(metas, function(m) {
    v <- suppressWarnings(as.integer(m$version))
    if (length(v) == 0 || is.na(v)) 1L else v
  }, integer(1))

  idx <- tibble(
    question_id = slugs,          # sin versiones, la identidad es el slug
    slug = slugs,
    version = versiones,
    content_hash = vapply(fuentes, content_hash, character(1), USE.NAMES = FALSE),
    section = vapply(metas, function(m) as.character(m$section)[1], character(1)),
    tags = I(lapply(metas, function(m) as_tags(m$tags))),
    author = vapply(metas, function(m) as.character(m$author)[1], character(1)),
    type = vapply(metas, function(m) as.character(m$type)[1], character(1)),
    # Un archivo en el directorio se considera vigente
    status = rep("published", length(archivos))
  )

  if (with_source) {
    idx$source <- fuentes
  }

  idx
}

#' @rdname qs_search
#' @export
qs_search.question_store_local <- function(store, section = NULL, tags = NULL,
                                           status = "published", slug = NULL, ...) {
  idx <- local_index(store)
  filter_index(idx, section = section, tags = tags, status = status, slug = slug)
}

#' Aplicar los filtros de qs_search a una tabla de preguntas
#' @keywords internal
filter_index <- function(idx, section = NULL, tags = NULL,
                         status = "published", slug = NULL) {
  if (!is.null(section)) {
    idx <- idx[!is.na(idx$section) & idx$section %in% section, , drop = FALSE]
  }
  if (!is.null(slug)) {
    idx <- idx[idx$slug %in% slug, , drop = FALSE]
  }
  if (!is.null(status)) {
    idx <- idx[idx$status %in% status, , drop = FALSE]
  }
  if (!is.null(tags)) {
    tiene <- vapply(idx$tags, function(t) any(tags %in% t), logical(1))
    idx <- idx[tiene, , drop = FALSE]
  }
  idx
}

#' @rdname qs_get
#' @export
qs_get.question_store_local <- function(store, ids, ...) {
  idx <- local_index(store, with_source = TRUE)

  faltan <- setdiff(ids, idx$question_id)
  if (length(faltan) > 0) {
    stop(sprintf("Preguntas no encontradas en %s:\n  %s",
                 store$base_dir, paste("-", faltan, collapse = "\n  ")))
  }

  # Respetar el orden pedido, que es el orden del examen
  idx[match(ids, idx$question_id), , drop = FALSE]
}

#' @rdname qs_materialize
#' @export
qs_materialize.question_store_local <- function(store, ids,
                                                dir = tempfile("preguntas"), ...) {
  write_materialized(qs_get(store, ids), dir)
}

#' @rdname qs_put
#' @export
qs_put.question_store_local <- function(store, source, slug = NULL, ...) {
  meta <- list(...)

  if (is.null(slug)) {
    slug <- extract_slug(source)
  }

  if (!dir.exists(store$base_dir)) {
    dir.create(store$base_dir, recursive = TRUE)
  }

  fuente <- inject_exercise_metadata(source, meta)
  ruta <- file.path(store$base_dir, paste0(slug, ".Rnw"))
  writeLines(fuente, ruta, useBytes = TRUE)

  invisible(slug)
}

#' Extraer el slug de \\exname del codigo fuente
#' @keywords internal
extract_slug <- function(source) {
  lineas <- unlist(strsplit(paste(source, collapse = "\n"), "\n", fixed = TRUE))
  hits <- regmatches(lineas, regexpr("\\\\exname\\{[^}]*\\}", lineas))
  hits <- hits[nzchar(hits)]
  if (length(hits) == 0) {
    stop("No se pudo determinar el slug: el ejercicio no declara \\exname y no se paso 'slug'")
  }
  sub("\\\\exname\\{([^}]*)\\}", "\\1", hits[1])
}
