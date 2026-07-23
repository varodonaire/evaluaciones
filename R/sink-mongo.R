# ==============================================================================
# SINK DE RESULTADOS: IMPLEMENTACION MONGODB
# ==============================================================================
# Dos colecciones:
#
#   runs       un documento por corrida, con el examen completo: que version de
#              cada pregunta, en que posicion de que forma, con que semilla
#   responses  una fila por alumno y pregunta
#
# Con un run_id se puede regenerar el examen identico o rehacer el analisis
# sobre los datos crudos, incluso con un metodo mejor que el original.
# ==============================================================================

#' Sink de resultados en MongoDB
#'
#' @param url URI de conexion
#' @param db Nombre de la base
#' @param runs_collection Coleccion de corridas
#' @param responses_collection Coleccion de respuestas
#'
#' @return Objeto de clase "results_sink_mongo"
#' @export
#'
#' @examples
#' \dontrun{
#' sink <- results_sink_mongo()
#' rs_list_runs(sink)
#' }
results_sink_mongo <- function(url = default_mongo_url(),
                               db = default_mongo_db(),
                               runs_collection = "runs",
                               responses_collection = "responses") {
  if (!requireNamespace("mongolite", quietly = TRUE)) {
    stop("El sink de MongoDB necesita el paquete 'mongolite'. Instalalo con install.packages(\"mongolite\").")
  }

  sink <- list(
    type = "mongo",
    url = url,
    db = db,
    runs_collection = runs_collection,
    responses_collection = responses_collection
  )
  class(sink) <- c("results_sink_mongo", "results_sink", "list")
  sink
}

#' Abrir conexion a una de las colecciones del sink
#' @keywords internal
sink_conn <- function(sink, which = c("runs", "responses")) {
  which <- match.arg(which)
  col <- if (which == "runs") sink$runs_collection else sink$responses_collection
  mongolite::mongo(collection = col, db = sink$db, url = sink$url)
}

#' Crear los indices de las colecciones de resultados
#'
#' Los indices por run_id y student_id son los que mantienen usable la
#' coleccion de respuestas, que crece rapido: cientos de alumnos por decenas de
#' preguntas por varias pruebas al ano.
#'
#' @param sink Objeto results_sink_mongo
#'
#' @return Invisiblemente el sink
#' @export
rs_ensure_indexes <- function(sink) {
  runs <- sink_conn(sink, "runs")
  on.exit(try(runs$disconnect(), silent = TRUE), add = TRUE)
  runs$index(add = '{"run_id": 1}')
  runs$index(add = '{"assessment_id": 1, "applied_at": -1}')
  runs$index(add = '{"session_id": 1}')

  resp <- sink_conn(sink, "responses")
  on.exit(try(resp$disconnect(), silent = TRUE), add = TRUE)
  resp$index(add = '{"run_id": 1, "student_id": 1}')
  resp$index(add = '{"run_id": 1, "question_id": 1}')
  resp$index(add = '{"student_id": 1}')

  invisible(sink)
}

#' @rdname rs_write_run
#' @export
rs_write_run.results_sink_mongo <- function(sink, run, ...) {
  con <- sink_conn(sink, "runs")
  on.exit(try(con$disconnect(), silent = TRUE), add = TRUE)

  # Una corrida se reescribe entera si se vuelve a guardar con el mismo id
  con$remove(sprintf('{"run_id": %s}',
                     jsonlite::toJSON(run$run_id, auto_unbox = TRUE)))
  con$insert(jsonlite::toJSON(run, auto_unbox = TRUE, null = "null",
                              digits = NA, force = TRUE))

  invisible(run$run_id)
}

#' @rdname rs_write_responses
#' @export
rs_write_responses.results_sink_mongo <- function(sink, scored_data, run_id, ...) {
  respuestas <- ensure_responses(scored_data, run_id, ...)

  con <- sink_conn(sink, "responses")
  on.exit(try(con$disconnect(), silent = TRUE), add = TRUE)

  # Reescribir es idempotente: se borra lo anterior de esta corrida
  con$remove(sprintf('{"run_id": %s}',
                     jsonlite::toJSON(run_id, auto_unbox = TRUE)))
  con$insert(respuestas)

  invisible(nrow(respuestas))
}

#' @rdname rs_read_run
#' @export
rs_read_run.results_sink_mongo <- function(sink, run_id, ...) {
  runs <- sink_conn(sink, "runs")
  on.exit(try(runs$disconnect(), silent = TRUE), add = TRUE)

  docs <- runs$find(
    query = sprintf('{"run_id": %s}', jsonlite::toJSON(run_id, auto_unbox = TRUE)),
    fields = "{}"
  )
  if (nrow(docs) == 0) {
    stop(sprintf("Corrida no encontrada en %s.%s: %s",
                 sink$db, sink$runs_collection, run_id))
  }

  run <- mongo_doc_to_run(docs[1, ])

  resp <- sink_conn(sink, "responses")
  on.exit(try(resp$disconnect(), silent = TRUE), add = TRUE)
  respuestas <- resp$find(
    query = sprintf('{"run_id": %s}', jsonlite::toJSON(run_id, auto_unbox = TRUE)),
    fields = '{"_id": 0}'
  )

  list(
    run = run,
    responses = if (nrow(respuestas) == 0) empty_responses() else tibble::as_tibble(respuestas)
  )
}

#' Convertir un documento de mongo al registro de corrida
#'
#' mongolite devuelve las estructuras anidadas como data frames; hay que
#' devolverlas a listas para que `run_items_df()` y `reproduce_run()` las
#' recorran igual que un registro recien construido.
#'
#' @keywords internal
mongo_doc_to_run <- function(doc) {
  run <- as.list(doc)

  desanidar <- function(x) {
    if (is.data.frame(x)) {
      return(lapply(seq_len(nrow(x)), function(i) desanidar(as.list(x[i, ]))))
    }
    if (is.list(x)) {
      if (length(x) == 1 && is.null(names(x))) return(desanidar(x[[1]]))
      return(lapply(x, desanidar))
    }
    x
  }

  # Un objeto unico vuelve de mongo como data frame de una fila, que desanidar
  # convierte en una lista de largo 1. `forms` si es una coleccion y conserva
  # esa forma; el resto hay que desenvolverlo.
  unico <- function(x) {
    y <- desanidar(x)
    if (is.list(y) && length(y) == 1 && is.null(names(y))) y[[1]] else y
  }

  run$forms <- if (is.null(run$forms)) list() else desanidar(run$forms)
  run$exam_config <- unico(run$exam_config)
  run$processing_config <- unico(run$processing_config)
  run$versions <- unico(run$versions)
  run$question_store <- unico(run$question_store)
  run$analysis <- unico(run$analysis)

  run[["_id"]] <- NULL
  run
}

#' @rdname rs_list_runs
#' @export
rs_list_runs.results_sink_mongo <- function(sink, ...) {
  con <- sink_conn(sink, "runs")
  on.exit(try(con$disconnect(), silent = TRUE), add = TRUE)

  docs <- con$find(
    fields = '{"run_id": 1, "assessment_id": 1, "session_id": 1, "applied_at": 1, "_id": 0}',
    sort = '{"applied_at": -1}'
  )

  if (nrow(docs) == 0) {
    return(tibble(run_id = character(0), assessment_id = character(0),
                  session_id = character(0), applied_at = character(0),
                  n_responses = integer(0)))
  }

  resp <- sink_conn(sink, "responses")
  on.exit(try(resp$disconnect(), silent = TRUE), add = TRUE)

  campo <- function(nombre) {
    if (is.null(docs[[nombre]])) rep(NA_character_, nrow(docs)) else as.character(docs[[nombre]])
  }

  tibble(
    run_id = campo("run_id"),
    assessment_id = campo("assessment_id"),
    session_id = campo("session_id"),
    applied_at = campo("applied_at"),
    n_responses = vapply(campo("run_id"), function(r) {
      as.integer(resp$count(sprintf('{"run_id": %s}',
                                    jsonlite::toJSON(r, auto_unbox = TRUE))))
    }, integer(1), USE.NAMES = FALSE)
  )
}
