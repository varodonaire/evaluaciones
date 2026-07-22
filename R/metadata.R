# ==============================================================================
# METADATOS DE EJERCICIOS
# ==============================================================================
# Los campos \exname, \exsection, \extags, \exauthor y \exversion son metadatos
# estaticos de 'exams'. Se pueden leer con una expresion regular, sin tejer el
# ejercicio, que es mucho mas barato y no requiere ejecutar su codigo R.
# ==============================================================================

#' Leer los metadatos estaticos de un ejercicio
#'
#' Extrae los campos de meta-informacion de un archivo .Rnw o .Rmd sin
#' ejecutarlo. Solo sirve para campos estaticos: un valor calculado con
#' \\Sexpr{} (como suele ser \\exsolution) se devuelve tal cual, sin evaluar.
#'
#' @param file Ruta al archivo de ejercicio
#' @param fields Campos a extraer (sin el prefijo "ex")
#'
#' @return Lista nombrada con los campos encontrados; NA_character_ para los
#'   ausentes. `tags` se devuelve como vector separando por comas.
#' @keywords internal
read_exercise_metadata <- function(file,
                                   fields = c("name", "section", "tags",
                                              "author", "version", "type")) {
  if (!file.exists(file)) {
    stop(sprintf("Ejercicio no encontrado: %s", file))
  }

  lineas <- readLines(file, warn = FALSE, encoding = "UTF-8")

  extraer <- function(campo) {
    # Acepta tanto "%% \excampo{...}" (LaTeX) como "\excampo{...}" suelto
    patron <- sprintf("\\\\ex%s\\{([^}]*)\\}", campo)
    hits <- regmatches(lineas, regexpr(patron, lineas))
    hits <- hits[nzchar(hits)]
    if (length(hits) == 0) {
      return(NA_character_)
    }
    sub(patron, "\\1", hits[1])
  }

  meta <- lapply(stats::setNames(fields, fields), extraer)

  if ("tags" %in% fields && !is.na(meta$tags)) {
    meta$tags <- trimws(strsplit(meta$tags, ",")[[1]])
  }

  meta
}

#' Leer los metadatos de varios ejercicios
#'
#' @param files Rutas a los archivos de ejercicio
#'
#' @return Data frame con una fila por ejercicio
#' @keywords internal
read_exercise_metadata_all <- function(files) {
  metas <- lapply(files, read_exercise_metadata)

  tibble(
    file = basename(files),
    name = vapply(metas, function(m) as.character(m$name)[1], character(1)),
    section = vapply(metas, function(m) as.character(m$section)[1], character(1)),
    author = vapply(metas, function(m) as.character(m$author)[1], character(1)),
    version = vapply(metas, function(m) as.character(m$version)[1], character(1)),
    type = vapply(metas, function(m) as.character(m$type)[1], character(1)),
    tags = I(lapply(metas, function(m) if (all(is.na(m$tags))) character(0) else m$tags))
  )
}

#' Construir la tabla de ejes a partir de un pool
#'
#' Los templates de reporte esperan un data frame con columnas `identificador`
#' y `nombre`: el primero es la clave de union con la columna Eje, el segundo
#' el texto que se muestra. Esta funcion arma el esqueleto con el identificador
#' en ambas columnas, para que se le pongan nombres descriptivos encima.
#'
#' @param pool Objeto question_pool
#' @param nombres Vector nombrado opcional: c(slug = "Nombre descriptivo")
#'
#' @return Data frame con columnas identificador y nombre
#' @export
#'
#' @examples
#' pool <- create_question_pool(
#'   c("A_1.Rnw", "B_1.Rnw"),
#'   c("algebra", "geometria")
#' )
#' build_ejes_df(pool, nombres = c(algebra = "Algebra", geometria = "Geometria"))
build_ejes_df <- function(pool, nombres = NULL) {
  ids <- pool$unique_topics

  etiquetas <- ids
  if (!is.null(nombres)) {
    conocidos <- ids %in% names(nombres)
    etiquetas[conocidos] <- unname(nombres[ids[conocidos]])

    sobrantes <- setdiff(names(nombres), ids)
    if (length(sobrantes) > 0) {
      warning(sprintf(
        "Nombres para ejes que no estan en el pool: %s",
        paste(sobrantes, collapse = ", ")
      ))
    }
  }

  tibble(identificador = ids, nombre = etiquetas)
}
