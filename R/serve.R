# ==============================================================================
# SERVIR LA API HTTP
# ==============================================================================

#' Lanzar la API HTTP
#'
#' Levanta el servicio plumber con los endpoints de trabajos (generar, corregir,
#' informar, reproducir, buscar y migrar preguntas). Es el mismo `inst/plumber`
#' que ya existe, envuelto en un lanzador para que Docker —y cualquiera— lo
#' inicie con un comando.
#'
#' @param host Interfaz donde escuchar. En un contenedor debe ser "0.0.0.0"
#' @param port Puerto
#' @param ... Argumentos adicionales para plumber::pr_run
#'
#' @return No retorna: corre hasta que se detiene
#' @export
#'
#' @examples
#' \dontrun{
#' run_api(port = 8000)
#' }
run_api <- function(host = "0.0.0.0", port = 8000, ...) {
  if (!requireNamespace("plumber", quietly = TRUE)) {
    stop("La API necesita el paquete 'plumber'. Instalalo con install.packages(\"plumber\").")
  }

  app_file <- system.file("plumber", "plumber.R", package = "evaluaciones")
  if (!nzchar(app_file) || !file.exists(app_file)) {
    app_file <- file.path("inst", "plumber", "plumber.R")
    if (!file.exists(app_file)) {
      stop("No se encontro la definicion de la API (plumber.R).")
    }
  }

  pr <- plumber::pr(app_file)
  plumber::pr_run(pr, host = host, port = port, ...)
}
