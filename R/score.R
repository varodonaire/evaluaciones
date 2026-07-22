# ==============================================================================
# SCORING
# ==============================================================================

#' Unir respuestas con claves y calcular correctas
#'
#' @param responses_long Data frame de respuestas en formato largo
#' @param items Lista o data frame de items con claves
#'
#' @return Data frame con columna is_correct
#' @export
score_responses <- function(responses_long, items) {
  
  # Convertir items a data frame si es lista
  if (is.list(items) && !is.data.frame(items)) {
    items_df <- bind_rows(items)
  } else {
    items_df <- items
  }
  
  # Asegurar tipos correctos
  items_df <- items_df %>%
    mutate(
      Forma = as.integer(Forma),
      Pregunta = as.integer(Pregunta),
      Correcta = tolower(trimws(as.character(Correcta)))
    )
  
  # Unir y calificar
  responses_long %>%
    inner_join(items_df, by = c("Forma", "Pregunta")) %>%
    mutate(
      is_correct = case_when(
        is.na(Respuesta) | Respuesta == "" ~ FALSE,
        TRUE ~ Respuesta == Correcta
      )
    )
}

#' Calcular puntajes por estudiante
#'
#' @param scored_data Data frame con respuestas calificadas
#' @param id_col Nombre de la columna de identificacion. NULL autodetecta
#'   entre "student_id" y "Rut"
#'
#' @return Data frame con puntajes por estudiante
#' @export
calculate_student_scores <- function(scored_data, id_col = NULL) {
  id_col <- resolve_id_col(scored_data, id_col)
  scored_data <- normalize_correct_col(scored_data)

  scored_data %>%
    group_by(across(all_of(id_col))) %>%
    summarise(
      n_respuestas = n(),
      correctas = sum(is_correcta, na.rm = TRUE),
      porcentaje = 100 * correctas / n_respuestas,
      .groups = "drop"
    ) %>%
    arrange(desc(porcentaje))
}

#' Calcular rendimiento por pregunta/eje
#'
#' @param scored_data Data frame con respuestas calificadas
#' @param by Variable de agrupacion ("Origen" o "Eje")
#'
#' @return Data frame con rendimiento por grupo
#' @export
calculate_item_performance <- function(scored_data, by = "Origen") {
  scored_data <- normalize_correct_col(scored_data)

  scored_data %>%
    group_by(across(all_of(by))) %>%
    summarise(
      n_respuestas = n(),
      correctas = sum(is_correcta, na.rm = TRUE),
      porcentaje = 100 * correctas / n_respuestas,
      .groups = "drop"
    ) %>%
    arrange(porcentaje)
}
