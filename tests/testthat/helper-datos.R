# Helpers para construir datos sinteticos de prueba
# ==============================================================================

#' Construir un set de items con clave conocida
#'
#' @param n_forms Numero de formas
#' @param n_questions Preguntas por forma
fake_items <- function(n_forms = 2, n_questions = 5) {
  expand.grid(
    Forma = seq_len(n_forms),
    Pregunta = seq_len(n_questions),
    KEEP.OUT.ATTRS = FALSE
  ) |>
    dplyr::as_tibble() |>
    dplyr::mutate(
      # Cada forma presenta los mismos items en distinto orden
      Origen = paste0("P", ((Pregunta + Forma - 2L) %% n_questions) + 1L),
      Eje = ifelse(Pregunta <= ceiling(n_questions / 2), "Algebra", "Geometria"),
      Correcta = rep(c("a", "b", "c", "d"), length.out = dplyr::n())
    )
}

#' Construir respuestas donde se controla exactamente quien acierta
#'
#' @param items Data frame de items (de fake_items)
#' @param n_students Numero de estudiantes
#' @param p_correct Probabilidad de acierto
#' @param seed Semilla
fake_responses <- function(items, n_students = 20, p_correct = 0.6, seed = 42) {
  set.seed(seed)
  n_forms <- max(items$Forma)

  tibble::tibble(
    Rut = sprintf("%08d-%d", seq_len(n_students), seq_len(n_students) %% 10),
    Forma = rep(seq_len(n_forms), length.out = n_students)
  ) |>
    tidyr::crossing(Pregunta = sort(unique(items$Pregunta))) |>
    dplyr::left_join(items, by = c("Forma", "Pregunta")) |>
    dplyr::mutate(
      Respuesta = ifelse(
        stats::runif(dplyr::n()) < p_correct,
        Correcta,
        # Elegir una alternativa distinta de la correcta
        vapply(Correcta, function(k) setdiff(letters[1:4], k)[1], character(1))
      )
    ) |>
    dplyr::select(Rut, Forma, Pregunta, Respuesta)
}

#' Datos ya calificados, listos para las funciones psicometricas
fake_scored <- function(n_forms = 2, n_questions = 5, n_students = 20, seed = 42) {
  items <- fake_items(n_forms, n_questions)
  responses <- fake_responses(items, n_students, seed = seed)
  score_responses(responses, items)
}
