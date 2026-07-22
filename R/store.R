# ==============================================================================
# STORE DE PREGUNTAS: CONTRATO
# ==============================================================================
# El core no sabe de donde vienen las preguntas. Un store implementa cuatro
# verbos y hay dos implementaciones: local (archivos .Rnw en un directorio) y
# mongo (versiones inmutables en MongoDB).
#
#   qs_search       buscar preguntas por eje, tags o estado
#   qs_get          traer preguntas con su codigo fuente
#   qs_materialize  escribir las preguntas a un directorio, listas para exams
#   qs_put          guardar una pregunta nueva
#
# Todas devuelven las mismas columnas, para que el codigo que las consume no
# tenga que saber contra que store esta hablando:
#
#   question_id  identidad de esta version (el slug en local, el _id en mongo)
#   slug         nombre legible y estable entre versiones
#   version      numero de version
#   content_hash sha256 del codigo fuente
#   section      el eje tematico
#   tags, author, type, status
#   source       el codigo .Rnw (solo en qs_get)
# ==============================================================================

#' Columnas que devuelven qs_search y qs_get
#' @keywords internal
qs_columns <- function(with_source = FALSE) {
  cols <- c("question_id", "slug", "version", "content_hash",
            "section", "tags", "author", "type", "status")
  if (with_source) c(cols, "source") else cols
}

#' Normalizar los tags a un vector de caracteres
#'
#' Los tags llegan en formatos distintos segun el origen: NULL, character(0),
#' o listas anidadas cuando vuelven de MongoDB. Sin normalizar, un
#' `as.character()` sobre una lista vacia produce el literal "list()", que
#' terminaba escrito en el .Rnw como un campo \code{extags} vacio pero presente.
#'
#' @param x Tags en cualquiera de esas formas
#'
#' @return Vector de caracteres, posiblemente vacio
#' @keywords internal
as_tags <- function(x) {
  if (is.null(x)) {
    return(character(0))
  }
  x <- unlist(x, use.names = FALSE)
  if (is.null(x) || length(x) == 0) {
    return(character(0))
  }
  x <- as.character(x)
  x[!is.na(x) & nzchar(x)]
}

#' Calcular el hash de contenido de una pregunta
#'
#' Se normalizan los finales de linea y el espacio final para que el mismo
#' contenido guardado en Windows y en Unix produzca el mismo hash.
#'
#' @param source Codigo fuente del ejercicio
#'
#' @return Cadena "sha256:..."
#' @keywords internal
content_hash <- function(source) {
  normalizado <- gsub("\r\n", "\n", paste(source, collapse = "\n"))
  normalizado <- sub("[ \t\n]+$", "", normalizado)
  paste0("sha256:", digest::digest(normalizado, algo = "sha256", serialize = FALSE))
}

# ------------------------------------------------------------------
# Genericos
# ------------------------------------------------------------------

#' Buscar preguntas en un store
#'
#' @param store Objeto question_store
#' @param section Filtrar por eje tematico
#' @param tags Filtrar por tags (devuelve las que tengan alguno)
#' @param status Filtrar por estado ("published", "draft", "retired"). NULL trae todas
#' @param slug Filtrar por slug
#' @param ... Argumentos adicionales para la implementacion
#'
#' @return Data frame con una fila por pregunta, sin el codigo fuente
#' @export
qs_search <- function(store, section = NULL, tags = NULL,
                      status = "published", slug = NULL, ...) {
  UseMethod("qs_search")
}

#' Traer preguntas con su codigo fuente
#'
#' @param store Objeto question_store
#' @param ids Identificadores de pregunta (question_id)
#' @param ... Argumentos adicionales para la implementacion
#'
#' @return Data frame con una fila por pregunta, incluyendo la columna source
#' @export
qs_get <- function(store, ids, ...) {
  UseMethod("qs_get")
}

#' Escribir preguntas a un directorio
#'
#' Deja los .Rnw listos para que 'exams' los teja. Los metadatos del store se
#' inyectan en el archivo, de modo que una pregunta descargada sigue siendo
#' autodescriptiva fuera de la base de datos.
#'
#' @param store Objeto question_store
#' @param ids Identificadores de pregunta
#' @param dir Directorio de destino (se crea si no existe)
#' @param ... Argumentos adicionales para la implementacion
#'
#' @return Vector de rutas a los archivos escritos, en el orden de `ids`
#' @export
qs_materialize <- function(store, ids, dir = tempfile("preguntas"), ...) {
  UseMethod("qs_materialize")
}

#' Guardar una pregunta
#'
#' @param store Objeto question_store
#' @param source Codigo fuente del ejercicio (.Rnw)
#' @param slug Nombre estable de la pregunta. Si es NULL se lee de \\exname
#' @param ... Metadatos adicionales (section, tags, author, status)
#'
#' @return question_id de la version guardada
#' @export
qs_put <- function(store, source, slug = NULL, ...) {
  UseMethod("qs_put")
}

#' Print para objetos question_store
#'
#' @param x Objeto question_store
#' @param ... Ignorado
#'
#' @return Invisiblemente el objeto
#' @export
print.question_store <- function(x, ...) {
  cat(strrep("=", 55), "\n", sep = "")
  cat("STORE DE PREGUNTAS\n")
  cat(strrep("=", 55), "\n", sep = "")
  cat(sprintf("  Tipo:     %s\n", x$type))
  for (campo in setdiff(names(x), c("type"))) {
    valor <- x[[campo]]
    if (is.character(valor) && length(valor) == 1) {
      cat(sprintf("  %-9s %s\n", paste0(campo, ":"), valor))
    }
  }
  cat(strrep("=", 55), "\n", sep = "")
  invisible(x)
}

# ------------------------------------------------------------------
# Utilidades compartidas
# ------------------------------------------------------------------

#' Inyectar metadatos en el codigo fuente de un ejercicio
#'
#' Reescribe (o agrega) las lineas de meta-informacion para que reflejen lo que
#' dice el store. El store es la fuente de verdad: si el archivo trae otro
#' valor, gana el del store.
#'
#' @param source Codigo fuente
#' @param meta Lista nombrada de campos (section, author, version, tags)
#'
#' @return Codigo fuente con los metadatos actualizados
#' @keywords internal
inject_exercise_metadata <- function(source, meta) {
  lineas <- unlist(strsplit(paste(source, collapse = "\n"), "\n", fixed = TRUE))

  poner <- function(lineas, campo, valor) {
    if (campo == "tags") valor <- as_tags(valor)
    if (is.null(valor) || length(valor) == 0 || all(is.na(valor))) {
      return(lineas)
    }
    valor <- paste(valor, collapse = ", ")
    patron <- sprintf("\\\\ex%s\\{[^}]*\\}", campo)
    idx <- grep(patron, lineas)

    if (length(idx) > 0) {
      lineas[idx[1]] <- sub(patron, sprintf("\\\\ex%s{%s}", campo, valor), lineas[idx[1]])
      return(lineas)
    }

    # No existe: agregarlo despues de \exname, o al final si tampoco esta
    ancla <- grep("\\\\exname\\{[^}]*\\}", lineas)
    nueva <- sprintf("%%%% \\ex%s{%s}", campo, valor)
    if (length(ancla) > 0) {
      append(lineas, nueva, after = ancla[1])
    } else {
      c(lineas, nueva)
    }
  }

  for (campo in c("section", "author", "version", "tags")) {
    lineas <- poner(lineas, campo, meta[[campo]])
  }

  paste(lineas, collapse = "\n")
}

#' Escribir a disco las preguntas ya traidas de un store
#' @keywords internal
write_materialized <- function(preguntas, dir) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }

  rutas <- character(nrow(preguntas))
  for (i in seq_len(nrow(preguntas))) {
    fuente <- inject_exercise_metadata(
      preguntas$source[i],
      list(
        section = preguntas$section[i],
        author = preguntas$author[i],
        version = preguntas$version[i],
        tags = as_tags(preguntas$tags[[i]])
      )
    )
    rutas[i] <- file.path(dir, paste0(preguntas$slug[i], ".Rnw"))
    writeLines(fuente, rutas[i], useBytes = TRUE)
  }

  rutas
}
