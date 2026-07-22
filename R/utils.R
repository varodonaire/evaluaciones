# ==============================================================================
# UTILIDADES INTERNAS
# ==============================================================================

#' Resolver el nombre de la columna identificadora de estudiante
#'
#' El pipeline de correccion produce datos con columna "Rut", mientras que los
#' metadatos preparados para los templates usan "student_id". Esta funcion
#' detecta cual esta presente para que las funciones de analisis acepten ambos.
#'
#' @param df Data frame a inspeccionar
#' @param id_col Nombre explicito de la columna, o NULL para autodetectar
#' @param candidates Nombres candidatos, en orden de preferencia
#'
#' @return Nombre de la columna identificadora
#' @keywords internal
resolve_id_col <- function(df, id_col = NULL,
                           candidates = c("student_id", "Rut", "rut", "RUT")) {
  if (!is.null(id_col)) {
    if (!id_col %in% colnames(df)) {
      stop(sprintf(
        "Columna identificadora '%s' no encontrada. Columnas disponibles: %s",
        id_col, paste(colnames(df), collapse = ", ")
      ))
    }
    return(id_col)
  }

  found <- intersect(candidates, colnames(df))
  if (length(found) == 0) {
    stop(sprintf(
      "No se encontro columna identificadora (%s). Especifica id_col explicitamente.",
      paste(candidates, collapse = ", ")
    ))
  }
  found[1]
}

#' Normalizar el nombre de la columna de aciertos a is_correcta
#' @keywords internal
normalize_correct_col <- function(scored_data) {
  if ("is_correct" %in% colnames(scored_data) &&
      !"is_correcta" %in% colnames(scored_data)) {
    scored_data <- scored_data %>% rename(is_correcta = is_correct)
  }
  if (!"is_correcta" %in% colnames(scored_data)) {
    stop("Los datos no contienen columna 'is_correcta' ni 'is_correct'. Usa score_responses() primero.")
  }
  scored_data
}
