# ==============================================================================
# STORE DE PREGUNTAS: IMPLEMENTACION MONGODB
# ==============================================================================
# Las versiones son inmutables: editar una pregunta crea un documento nuevo y
# el anterior queda intacto. Un analisis viejo sigue apuntando a la version
# exacta que se aplico, aunque la pregunta se haya reescrito diez veces desde
# entonces.
#
#   question_id  el _id del documento, o sea la identidad de ESTA version
#   slug         estable entre versiones, es el \exname
#   content_hash sha256 del source, para detectar cambios reales
#   supersedes   el _id de la version anterior
# ==============================================================================

#' URL de MongoDB por defecto
#'
#' Sale de la variable de entorno `EVALUACIONES_MONGO_URL` si esta definida, y
#' cae a localhost si no. Es lo que permite que el mismo codigo apunte a
#' `localhost` en tu maquina y a `mongo` dentro de Docker, sin cambiar nada.
#'
#' @return Cadena con la URI de conexion
#' @export
default_mongo_url <- function() {
  Sys.getenv("EVALUACIONES_MONGO_URL", unset = "mongodb://localhost:27017")
}

#' Nombre de base de MongoDB por defecto
#'
#' Sale de `EVALUACIONES_MONGO_DB` si esta definida, o "evaluaciones".
#'
#' @return Nombre de la base
#' @export
default_mongo_db <- function() {
  Sys.getenv("EVALUACIONES_MONGO_DB", unset = "evaluaciones")
}

#' Store de preguntas en MongoDB
#'
#' @param url URI de conexion. Por defecto la de `default_mongo_url()`
#' @param db Nombre de la base. Por defecto la de `default_mongo_db()`
#' @param collection Nombre de la coleccion
#'
#' @return Objeto de clase "question_store_mongo"
#' @export
#'
#' @examples
#' \dontrun{
#' store <- question_store_mongo()
#' qs_search(store, section = "logica")
#' }
question_store_mongo <- function(url = default_mongo_url(),
                                 db = default_mongo_db(),
                                 collection = "questions") {
  if (!requireNamespace("mongolite", quietly = TRUE)) {
    stop("El store de MongoDB necesita el paquete 'mongolite'. Instalalo con install.packages(\"mongolite\").")
  }

  store <- list(
    type = "mongo",
    url = url,
    db = db,
    collection = collection
  )
  class(store) <- c("question_store_mongo", "question_store", "list")
  store
}

#' Abrir la conexion a la coleccion
#' @keywords internal
mongo_conn <- function(store) {
  mongolite::mongo(collection = store$collection, db = store$db, url = store$url)
}

#' Comprobar si hay un MongoDB accesible
#'
#' Pensado para `skip_if_not()` en los tests y para avisar temprano en scripts.
#'
#' @param url URI de conexion
#'
#' @return TRUE si se pudo conectar
#' @export
mongo_available <- function(url = default_mongo_url()) {
  if (!requireNamespace("mongolite", quietly = TRUE)) {
    return(FALSE)
  }
  tryCatch({
    con <- mongolite::mongo(collection = "ping", db = "admin", url = url)
    on.exit(try(con$disconnect(), silent = TRUE), add = TRUE)
    con$count()
    TRUE
  }, error = function(e) FALSE)
}

#' Crear los indices de la coleccion de preguntas
#'
#' Idempotente: MongoDB ignora la creacion de un indice que ya existe.
#'
#' @param store Objeto question_store_mongo
#'
#' @return Invisiblemente el store
#' @export
qs_ensure_indexes <- function(store) {
  con <- mongo_conn(store)
  on.exit(try(con$disconnect(), silent = TRUE), add = TRUE)

  con$index(add = '{"slug": 1, "version": -1}')
  con$index(add = '{"section": 1, "status": 1}')
  con$index(add = '{"content_hash": 1}')
  con$index(add = '{"tags": 1}')

  invisible(store)
}

#' Convertir documentos de mongo a la tabla del contrato
#' @keywords internal
mongo_to_index <- function(docs, with_source = FALSE) {
  cols <- qs_columns(with_source)

  if (length(docs) == 0 || nrow(docs) == 0) {
    vacio <- tibble(question_id = character(0))
    for (col in setdiff(cols, "question_id")) {
      vacio[[col]] <- if (col == "tags") list() else
        if (col == "version") integer(0) else character(0)
    }
    return(vacio[cols])
  }

  campo <- function(nombre, default = NA_character_) {
    if (is.null(docs[[nombre]])) rep(default, nrow(docs)) else docs[[nombre]]
  }

  out <- tibble(
    question_id = campo("_id"),
    slug = campo("slug"),
    version = as.integer(campo("version", NA_integer_)),
    content_hash = campo("content_hash"),
    section = campo("section"),
    tags = I(if (is.null(docs$tags)) {
      replicate(nrow(docs), character(0), simplify = FALSE)
    } else {
      lapply(docs$tags, as_tags)
    }),
    author = campo("author"),
    type = campo("type"),
    status = campo("status")
  )

  if (with_source) {
    out$source <- campo("source")
  }

  out[cols]
}

#' Construir el filtro JSON de una busqueda
#' @keywords internal
mongo_query <- function(section = NULL, tags = NULL, status = NULL, slug = NULL) {
  partes <- list()

  agregar <- function(campo, valor) {
    if (is.null(valor)) return(NULL)
    if (length(valor) == 1) {
      sprintf('"%s": %s', campo, jsonlite::toJSON(valor, auto_unbox = TRUE))
    } else {
      sprintf('"%s": {"$in": %s}', campo, jsonlite::toJSON(valor))
    }
  }

  partes <- c(partes, agregar("section", section))
  partes <- c(partes, agregar("status", status))
  partes <- c(partes, agregar("slug", slug))
  if (!is.null(tags)) {
    partes <- c(partes, sprintf('"tags": {"$in": %s}', jsonlite::toJSON(tags)))
  }

  if (length(partes) == 0) "{}" else paste0("{", paste(partes, collapse = ", "), "}")
}

#' @rdname qs_search
#' @export
qs_search.question_store_mongo <- function(store, section = NULL, tags = NULL,
                                           status = "published", slug = NULL, ...) {
  con <- mongo_conn(store)
  on.exit(try(con$disconnect(), silent = TRUE), add = TRUE)

  docs <- con$find(
    query = mongo_query(section, tags, status, slug),
    fields = '{"source": 0}',
    sort = '{"slug": 1, "version": -1}'
  )

  mongo_to_index(docs)
}

#' @rdname qs_get
#' @export
qs_get.question_store_mongo <- function(store, ids, ...) {
  con <- mongo_conn(store)
  on.exit(try(con$disconnect(), silent = TRUE), add = TRUE)

  # fields = "{}" es necesario: mongolite excluye _id por defecto
  docs <- con$find(
    query = sprintf('{"_id": {"$in": %s}}', jsonlite::toJSON(ids)),
    fields = "{}"
  )
  idx <- mongo_to_index(docs, with_source = TRUE)

  faltan <- setdiff(ids, idx$question_id)
  if (length(faltan) > 0) {
    stop(sprintf("Preguntas no encontradas en %s.%s:\n  %s",
                 store$db, store$collection,
                 paste("-", faltan, collapse = "\n  ")))
  }

  idx[match(ids, idx$question_id), , drop = FALSE]
}

#' @rdname qs_materialize
#' @export
qs_materialize.question_store_mongo <- function(store, ids,
                                                dir = tempfile("preguntas"), ...) {
  write_materialized(qs_get(store, ids), dir)
}

#' @rdname qs_put
#' @export
qs_put.question_store_mongo <- function(store, source, slug = NULL, ...) {
  meta <- list(...)

  if (is.null(slug)) {
    slug <- extract_slug(source)
  }

  fuente <- paste(source, collapse = "\n")
  hash <- content_hash(fuente)

  con <- mongo_conn(store)
  on.exit(try(con$disconnect(), silent = TRUE), add = TRUE)

  # Si el contenido no cambio, no se crea una version nueva
  previas <- con$find(
    query = sprintf('{"slug": %s}', jsonlite::toJSON(slug, auto_unbox = TRUE)),
    fields = '{"_id": 1, "version": 1, "content_hash": 1}',
    sort = '{"version": -1}',
    limit = 1
  )

  if (nrow(previas) > 0 && identical(previas$content_hash[1], hash)) {
    message(sprintf("'%s' sin cambios (v%d): se reutiliza la version existente",
                    slug, as.integer(previas$version[1])))
    return(invisible(previas$`_id`[1]))
  }

  version <- if (nrow(previas) > 0) as.integer(previas$version[1]) + 1L else 1L
  supersedes <- if (nrow(previas) > 0) previas$`_id`[1] else NA_character_

  # Lo que no venga en meta se toma del propio ejercicio
  del_archivo <- read_exercise_metadata_from_source(fuente)
  tomar <- function(campo, default = NA_character_) {
    if (!is.null(meta[[campo]])) return(meta[[campo]])
    valor <- del_archivo[[campo]]
    if (is.null(valor) || all(is.na(valor))) default else valor
  }

  doc <- list(
    `_id` = paste0(slug, "-v", version),
    slug = slug,
    version = version,
    content_hash = hash,
    source = fuente,
    section = tomar("section"),
    tags = I(list(as_tags(tomar("tags", character(0))))),
    author = tomar("author"),
    type = tomar("type"),
    status = if (!is.null(meta$status)) meta$status else "published",
    supersedes = supersedes,
    created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  )

  con$insert(jsonlite::toJSON(doc, auto_unbox = TRUE, null = "null"))

  invisible(doc$`_id`)
}

#' Leer metadatos desde el codigo fuente en memoria
#' @keywords internal
read_exercise_metadata_from_source <- function(source) {
  tmp <- tempfile(fileext = ".Rnw")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(paste(source, collapse = "\n"), tmp, useBytes = TRUE)
  read_exercise_metadata(tmp)
}
