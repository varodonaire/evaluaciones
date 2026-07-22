# ==============================================================================
# PIPELINE DE PROCESAMIENTO
# ==============================================================================

#' Procesar resultados de evaluacion
#'
#' @param config Objeto processing_config
#'
#' @return Lista con todos los resultados del procesamiento
#' @export
#'
#' @examples
#' \dontrun{
#' config <- create_processing_config(
#'   results_file = "resultados.xlsx",
#'   items_file = "items.json",
#'   students_file = "estudiantes.csv",
#'   num_questions = 25
#' )
#' results <- process_assessment(config)
#' }
process_assessment <- function(config) {
  
  message("\n", strrep("=", 60))
  message("PROCESAMIENTO DE EVALUACION")
  message(strrep("=", 60), "\n")
  
  # 1. Cargar datos
  data <- load_processing_data(config)
  
  # 2. Transformar resultados
  message("Transformando datos...")
  clean_results <- data$results %>%
    normalize_rut_columns() %>%
    standardize_response_columns(config$num_questions) %>%
    convert_form_codes(config$form_mapping)
  
  # 3. Normalizar respuestas
  response_cols <- grep("^Respuestas-", colnames(clean_results), value = TRUE)
  clean_results <- clean_results %>%
    mutate(across(all_of(response_cols), ~ tolower(trimws(as.character(.)))))
  
  # 4. Pivotar a formato largo
  results_long <- pivot_responses_long(clean_results)
  
  # 5. Filtrar por sesion si se especifica
  if (!is.null(config$session_id) && !is.null(data$sessions)) {
    session_students <- get_session_students(data$sessions, config$session_id)
    results_long <- results_long %>%
      filter(!!sym(config$id_col) %in% session_students)
    message(sprintf("[OK] Filtrado a sesion: %d estudiantes", length(session_students)))
  }
  
  # 6. Calificar respuestas
  scored <- score_responses(results_long, data$items)
  
  # 7. Calcular metricas
  student_scores <- calculate_student_scores(scored, id_col = config$id_col)
  item_performance <- calculate_item_performance(scored, "Origen")
  topic_performance <- calculate_item_performance(scored, "Eje")
  
  # 8. Crear directorio de salida
  if (!dir.exists(config$output_dir)) {
    dir.create(config$output_dir, recursive = TRUE)
  }
  
  # 9. Exportar resultados
  scores_file <- file.path(config$output_dir, "puntajes_estudiantes.xlsx")
  items_file <- file.path(config$output_dir, "rendimiento_items.xlsx")
  topics_file <- file.path(config$output_dir, "rendimiento_ejes.xlsx")
  
  write_xlsx(student_scores, scores_file)
  write_xlsx(item_performance, items_file)
  write_xlsx(topic_performance, topics_file)
  
  message(sprintf("[OK] Archivos exportados a %s", config$output_dir))
  
  # 10. Resumen
  message("\n", strrep("=", 60))
  message("[OK] PROCESAMIENTO COMPLETO")
  message(strrep("=", 60))
  message(sprintf("  Estudiantes: %d", nrow(student_scores)))
  message(sprintf("  Respuestas:  %d", nrow(scored)))
  message(sprintf("  Promedio:    %.1f%%", mean(student_scores$porcentaje, na.rm = TRUE)))
  message(strrep("=", 60), "\n")
  
  # Retornar resultados
  invisible(list(
    scored_data = scored,
    student_scores = student_scores,
    item_performance = item_performance,
    topic_performance = topic_performance,
    id_col = config$id_col,
    config = config
  ))
}
