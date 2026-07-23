# ==============================================================================
# EDITOR DE PREGUNTAS
# ==============================================================================
# Lanzador de la app Shiny que vive en inst/shiny/editor/. La app llama al store
# en el mismo proceso, asi que no necesita HTTP ni subir archivos: es el patron
# que hace de Shiny la herramienta adecuada para el editor.
# ==============================================================================

#' Lanzar el editor de preguntas
#'
#' Abre una app Shiny para navegar, previsualizar, editar y versionar las
#' preguntas de un store. Cada guardado crea una version nueva sin tocar la
#' anterior (contra MongoDB) o reescribe el archivo (contra un directorio local).
#'
#' @param store Objeto question_store. Por defecto un store local sobre el
#'   directorio de trabajo
#' @param ... Argumentos adicionales para shiny::runApp (por ejemplo `port`,
#'   `host`, `launch.browser`)
#'
#' @return No retorna: corre hasta que se cierra la app
#' @export
#'
#' @examples
#' \dontrun{
#' run_question_editor(question_store_mongo())
#' run_question_editor(question_store_local("questions"), port = 4000)
#' }
run_question_editor <- function(store = question_store_local("."), ...) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("El editor necesita el paquete 'shiny'. Instalalo con install.packages(\"shiny\").")
  }
  for (pkg in c("DT", "bslib")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("El editor necesita el paquete '%s'.", pkg))
    }
  }

  app_dir <- system.file("shiny", "editor", package = "evaluaciones")
  if (!nzchar(app_dir) || !dir.exists(app_dir)) {
    # En desarrollo, antes de instalar el paquete
    app_dir <- file.path("inst", "shiny", "editor")
    if (!dir.exists(app_dir)) {
      stop("No se encontro la app del editor.")
    }
  }

  # La app lee el store desde una opcion de shiny
  shiny::shinyOptions(evaluaciones_store = store)
  shiny::runApp(app_dir, ...)
}

#' Obtener el store configurado para el editor
#'
#' Lo usa la app Shiny para recuperar el store que le paso el lanzador.
#'
#' @return Objeto question_store
#' @keywords internal
editor_store <- function() {
  store <- shiny::getShinyOption("evaluaciones_store", default = NULL)
  if (is.null(store)) {
    store <- question_store_local(".")
  }
  store
}
