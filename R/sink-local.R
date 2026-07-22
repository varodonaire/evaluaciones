# ==============================================================================
# SINK DE RESULTADOS: IMPLEMENTACION LOCAL
# ==============================================================================
# Reproduce el comportamiento historico del paquete: los tres .xlsx de siempre
# en el directorio de salida. Agrega, sin cambiar nada de lo anterior, un .rds
# por corrida con el registro completo y las respuestas, de modo que tambien en
# modo local se pueda reproducir un analisis.
# ==============================================================================

#' Sink de resultados en archivos locales
#'
#' @param dir Directorio de salida
#'
#' @return Objeto de clase "results_sink_local"
#' @export
#'
#' @examples
#' \dontrun{
#' sink <- results_sink_local("reportes")
#' }
results_sink_local <- function(dir = "reports") {
  sink <- list(type = "local", dir = dir)
  class(sink) <- c("results_sink_local", "results_sink", "list")
  sink
}

#' Directorio donde viven las corridas de un sink local
#' @keywords internal
local_runs_dir <- function(sink) {
  file.path(sink$dir, "runs")
}

#' Ruta del archivo de una corrida
#' @keywords internal
local_run_file <- function(sink, run_id) {
  file.path(local_runs_dir(sink), paste0(run_id, ".rds"))
}

#' @rdname rs_write_run
#' @export
rs_write_run.results_sink_local <- function(sink, run, ...) {
  dir <- local_runs_dir(sink)
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }

  archivo <- local_run_file(sink, run$run_id)
  contenido <- if (file.exists(archivo)) readRDS(archivo) else list()
  contenido$run <- run
  saveRDS(contenido, archivo)

  invisible(run$run_id)
}

#' @rdname rs_write_responses
#' @export
rs_write_responses.results_sink_local <- function(sink, scored_data, run_id, ...) {
  archivo <- local_run_file(sink, run_id)
  if (!file.exists(archivo)) {
    stop(sprintf("No existe la corrida '%s'. Llama primero a rs_write_run().", run_id))
  }

  contenido <- readRDS(archivo)
  contenido$responses <- ensure_responses(scored_data, run_id, ...)
  saveRDS(contenido, archivo)

  invisible(nrow(contenido$responses))
}

#' @rdname rs_read_run
#' @export
rs_read_run.results_sink_local <- function(sink, run_id, ...) {
  archivo <- local_run_file(sink, run_id)
  if (!file.exists(archivo)) {
    stop(sprintf("Corrida no encontrada: %s", run_id))
  }

  contenido <- readRDS(archivo)
  list(
    run = contenido$run,
    responses = contenido$responses %||% empty_responses()
  )
}

#' @rdname rs_list_runs
#' @export
rs_list_runs.results_sink_local <- function(sink, ...) {
  dir <- local_runs_dir(sink)
  if (!dir.exists(dir)) {
    return(tibble(run_id = character(0), assessment_id = character(0),
                  session_id = character(0), applied_at = character(0),
                  n_responses = integer(0)))
  }

  archivos <- dir(dir, pattern = "\\.rds$", full.names = TRUE)
  if (length(archivos) == 0) {
    return(tibble(run_id = character(0), assessment_id = character(0),
                  session_id = character(0), applied_at = character(0),
                  n_responses = integer(0)))
  }

  bind_rows(lapply(archivos, function(f) {
    contenido <- readRDS(f)
    tibble(
      run_id = contenido$run$run_id %||% NA_character_,
      assessment_id = as.character(contenido$run$assessment_id %||% NA_character_),
      session_id = as.character(contenido$run$session_id %||% NA_character_),
      applied_at = contenido$run$applied_at %||% NA_character_,
      n_responses = nrow(contenido$responses %||% empty_responses())
    )
  }))
}

#' Escribir los tres .xlsx historicos
#'
#' Se mantiene aparte del contrato del sink porque es especifico del modo local
#' y del formato que consumen los docentes.
#'
#' @param sink Sink local
#' @param student_scores,item_performance,topic_performance Data frames a exportar
#'
#' @return Rutas de los archivos escritos (invisible)
#' @keywords internal
write_xlsx_reports <- function(sink, student_scores, item_performance,
                               topic_performance) {
  if (!dir.exists(sink$dir)) {
    dir.create(sink$dir, recursive = TRUE)
  }

  rutas <- c(
    scores = file.path(sink$dir, "puntajes_estudiantes.xlsx"),
    items = file.path(sink$dir, "rendimiento_items.xlsx"),
    topics = file.path(sink$dir, "rendimiento_ejes.xlsx")
  )

  write_xlsx(student_scores, rutas[["scores"]])
  write_xlsx(item_performance, rutas[["items"]])
  write_xlsx(topic_performance, rutas[["topics"]])

  invisible(rutas)
}
