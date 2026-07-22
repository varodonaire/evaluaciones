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

# ------------------------------------------------------------------
# Generacion de examenes
# ------------------------------------------------------------------

#' Directorio con los ejercicios .Rnw de prueba
questions_dir <- function() {
  testthat::test_path("fixtures")
}

#' Construir un exam_result sin necesitar LaTeX
#'
#' Usa xexams(), el motor comun bajo todos los exams2*, en vez de exams2pdf.
#' Produce la misma estructura que generate_exams() salvo los PDFs.
#'
#' @param n_forms Numero de formas
#' @param sizes,seeds Configuracion de grupos (NULL = un grupo por defecto)
#' @param order_seed Seed para el orden de las preguntas
fake_exam_result <- function(n_forms = 2, sizes = NULL, seeds = NULL,
                             order_seed = 1) {
  qdir <- normalizePath(questions_dir())

  files <- c("Suma_1.Rnw", "Suma_2.Rnw", "Resta_1.Rnw")
  topics <- c("Suma", "Suma", "Resta")

  groups <- if (is.null(sizes)) NULL else list(sizes = sizes, seeds = seeds)
  config <- create_exam_config("test", 2025, 4, "Mat",
                               num_forms = n_forms, groups = groups)
  pool <- create_question_pool(files, topics, base_dir = qdir)
  matrices <- generate_exam_matrices(config, pool, order_seed)

  outdir <- tempfile("xexams")
  dir.create(outdir)

  raw <- exams::xexams(
    matrices$question_matrix,
    seed = matrices$seed_matrix,
    driver = list(sweave = list(quiet = TRUE)),
    edir = qdir,
    dir = outdir
  )

  structure(
    list(
      config = config,
      pool = pool,
      exams_raw = raw,
      matrices = matrices,
      order_seed = order_seed
    ),
    class = c("exam_result", "list")
  )
}

# ------------------------------------------------------------------
# Corridas y respuestas
# ------------------------------------------------------------------

# Un exam_result minimo, sin necesidad de generar PDFs
exam_result_falso <- function(n_forms = 2, n_questions = 3, con_store = TRUE) {
  slugs <- paste0("Q", seq_len(n_questions))
  ids <- if (con_store) paste0(slugs, "-v1") else slugs

  config <- create_exam_config("prueba", 2025, 4, "Mat", num_forms = n_forms)
  pool <- list(
    files = paste0(slugs, ".Rnw"),
    topics = rep("algebra", n_questions),
    topic_lookup = setNames(rep("algebra", n_questions), slugs),
    base_dir = ".", n_questions = n_questions,
    unique_topics = "algebra",
    question_ids = if (con_store) ids else NULL,
    content_hashes = if (con_store) paste0("sha256:", seq_len(n_questions)) else NULL
  )
  class(pool) <- c("question_pool", "list")

  matrices <- generate_exam_matrices(config, pool, order_seed = 3)

  structure(list(config = config, pool = pool, matrices = matrices,
                 order_seed = 3),
            class = c("exam_result", "list"))
}

respuestas_falsas <- function(n_students = 4, n_questions = 3, form = 1L) {
  tidyr::crossing(
    Rut = sprintf("%d-%d", seq_len(n_students), seq_len(n_students)),
    Pregunta = seq_len(n_questions)
  ) |>
    dplyr::mutate(
      Forma = form,
      Origen = paste0("Q", Pregunta),
      Eje = "algebra",
      Correcta = "a",
      Respuesta = ifelse(Pregunta == 1, "a", "b"),
      is_correct = Respuesta == Correcta
    )
}
