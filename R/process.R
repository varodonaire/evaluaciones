# ==============================================================================
# PIPELINE DE PROCESAMIENTO
# ==============================================================================

#' Procesar resultados de evaluacion
#'
#' Por defecto escribe los tres .xlsx de siempre en `config$output_dir`. Si se
#' le pasa un `sink`, ademas registra la corrida y las respuestas, una fila por
#' alumno y pregunta, con lo necesario para rehacer o mejorar el analisis mas
#' adelante.
#'
#' @param config Objeto processing_config
#' @param sink Sink de resultados donde persistir la corrida. NULL usa un sink
#'   local sobre `config$output_dir`, que es el comportamiento historico
#' @param exam_result Objeto exam_result de la generacion, opcional. Sin el, la
#'   corrida se registra sin el detalle de formas y semillas
#' @param run_id Identificador de la corrida. NULL genera uno
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
#'
#' # Persistiendo la corrida en MongoDB
#' process_assessment(config, sink = results_sink_mongo(), exam_result = res)
#' }
process_assessment <- function(config, sink = NULL, exam_result = NULL,
                               run_id = NULL) {

  message("\n", strrep("=", 60))
  message("PROCESAMIENTO DE EVALUACION")
  message(strrep("=", 60), "\n")
  
  # 1. Cargar datos
  data <- load_processing_data(config)
  
  # 2-4. Transformar a formato largo. Con un layout la lectura sigue el
  # contrato declarado; sin el, las heuristicas de siempre.
  if (!is.null(config$layout)) {
    message("Leyendo la hoja segun el layout ", config$layout$layout_id, "...")
    results_long <- sheet_read(config$layout, data$results) %>%
      rename(!!config$id_col := "student_id") %>%
      convert_form_codes(config$form_mapping)
  } else {
    message("Transformando datos...")
    clean_results <- data$results %>%
      normalize_rut_columns() %>%
      standardize_response_columns(config$num_questions) %>%
      convert_form_codes(config$form_mapping)

    response_cols <- grep("^Respuestas-", colnames(clean_results), value = TRUE)
    clean_results <- clean_results %>%
      mutate(across(all_of(response_cols), ~ tolower(trimws(as.character(.)))))

    results_long <- pivot_responses_long(clean_results)
  }

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
  
  # 8. Los .xlsx de siempre, en el directorio de salida
  local_sink <- results_sink_local(config$output_dir)
  write_xlsx_reports(local_sink, student_scores, item_performance, topic_performance)
  message(sprintf("[OK] Archivos exportados a %s", config$output_dir))

  # 9. Registrar la corrida: en el sink que se pida, o en el local por defecto
  destino <- sink %||% local_sink
  run <- build_run_record(
    exam_result = exam_result,
    processing_config = config,
    session_id = config$session_id,
    analysis = list(
      n_estudiantes = nrow(student_scores),
      n_respuestas = nrow(scored),
      promedio = mean(student_scores$porcentaje, na.rm = TRUE)
    )
  )
  if (!is.null(run_id)) {
    run$run_id <- run_id
  }

  rs_write_run(destino, run)
  rs_write_responses(destino, normalize_responses(scored, run$run_id,
                                                  id_col = config$id_col,
                                                  run = run),
                     run$run_id)
  message(sprintf("[OK] Corrida registrada: %s", run$run_id))

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
    run_id = run$run_id,
    run = run,
    sink = destino,
    config = config
  ))
}
