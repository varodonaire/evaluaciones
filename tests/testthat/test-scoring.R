test_that("score_responses marca aciertos y errores", {
  items <- tibble::tibble(
    Forma = c(1L, 1L), Pregunta = c(1L, 2L),
    Origen = c("P1", "P2"), Eje = c("E1", "E1"),
    Correcta = c("a", "b")
  )
  responses <- tibble::tibble(
    Rut = c("1-9", "1-9", "2-7", "2-7"),
    Forma = 1L, Pregunta = c(1L, 2L, 1L, 2L),
    Respuesta = c("a", "c", "a", "b")
  )

  scored <- score_responses(responses, items)

  expect_equal(scored$is_correct, c(TRUE, FALSE, TRUE, TRUE))
  expect_equal(nrow(scored), 4)
})

test_that("score_responses trata omisiones como incorrectas", {
  items <- tibble::tibble(
    Forma = 1L, Pregunta = 1L, Origen = "P1", Eje = "E1", Correcta = "a"
  )
  responses <- tibble::tibble(
    Rut = c("1-9", "2-7"), Forma = 1L, Pregunta = 1L,
    Respuesta = c(NA_character_, "")
  )

  scored <- score_responses(responses, items)
  expect_equal(scored$is_correct, c(FALSE, FALSE))
})

test_that("score_responses cruza cada forma con su propia clave", {
  # La misma pregunta 1 tiene clave distinta en cada forma
  items <- tibble::tibble(
    Forma = c(1L, 2L), Pregunta = c(1L, 1L),
    Origen = c("P1", "P1"), Eje = "E1", Correcta = c("a", "d")
  )
  responses <- tibble::tibble(
    Rut = c("1-9", "2-7"), Forma = c(1L, 2L), Pregunta = 1L,
    Respuesta = c("a", "a")
  )

  scored <- score_responses(responses, items)
  expect_equal(scored$is_correct, c(TRUE, FALSE))
})

test_that("score_responses acepta items como lista de listas (JSON)", {
  items_list <- list(
    list(Forma = 1, Pregunta = 1, Origen = "P1", Eje = "E1", Correcta = "a"),
    list(Forma = 1, Pregunta = 2, Origen = "P2", Eje = "E1", Correcta = "b")
  )
  responses <- tibble::tibble(
    Rut = "1-9", Forma = 1L, Pregunta = c(1L, 2L), Respuesta = c("a", "a")
  )

  scored <- score_responses(responses, items_list)
  expect_equal(scored$is_correct, c(TRUE, FALSE))
})

test_that("calculate_student_scores agrega por estudiante", {
  scored <- tibble::tibble(
    Rut = c("1-9", "1-9", "2-7", "2-7"),
    is_correct = c(TRUE, TRUE, TRUE, FALSE)
  )
  scores <- calculate_student_scores(scored)

  expect_equal(nrow(scores), 2)
  # Ordenado por porcentaje descendente
  expect_equal(scores$Rut, c("1-9", "2-7"))
  expect_equal(scores$porcentaje, c(100, 50))
  expect_equal(scores$correctas, c(2L, 1L))
})

test_that("calculate_item_performance ordena de peor a mejor", {
  scored <- tibble::tibble(
    Origen = c("P1", "P1", "P2", "P2"),
    is_correct = c(TRUE, TRUE, FALSE, FALSE)
  )
  perf <- calculate_item_performance(scored, by = "Origen")

  expect_equal(perf$Origen, c("P2", "P1"))
  expect_equal(perf$porcentaje, c(0, 100))
})

# ------------------------------------------------------------------
# Columna identificadora (regresion)
# ------------------------------------------------------------------

test_that("resolve_id_col autodetecta student_id y Rut", {
  expect_equal(resolve_id_col(tibble::tibble(Rut = "1-9")), "Rut")
  expect_equal(resolve_id_col(tibble::tibble(student_id = "x")), "student_id")
  # student_id tiene prioridad si estan ambas
  expect_equal(resolve_id_col(tibble::tibble(student_id = "x", Rut = "1-9")), "student_id")
})

test_that("resolve_id_col valida el nombre explicito", {
  df <- tibble::tibble(Rut = "1-9")
  expect_equal(resolve_id_col(df, "Rut"), "Rut")
  expect_error(resolve_id_col(df, "student_id"), "no encontrada")
  expect_error(resolve_id_col(tibble::tibble(x = 1)), "No se encontro columna")
})

test_that("normalize_correct_col unifica is_correct e is_correcta", {
  expect_true("is_correcta" %in% names(normalize_correct_col(tibble::tibble(is_correct = TRUE))))
  expect_true("is_correcta" %in% names(normalize_correct_col(tibble::tibble(is_correcta = TRUE))))
  expect_error(normalize_correct_col(tibble::tibble(x = 1)), "score_responses")
})

# El pipeline de correccion produce "Rut" y el de reportes "student_id";
# antes las funciones de analisis asumian "student_id" y fallaban.
test_that("las funciones de analisis aceptan ambas convenciones de ID", {
  scored_rut <- fake_scored(n_students = 20)
  scored_sid <- dplyr::rename(scored_rut, student_id = Rut)

  expect_equal(
    calculate_reliability(scored_rut)$kr20,
    calculate_reliability(scored_sid)$kr20
  )
  expect_equal(
    calculate_student_scores(scored_rut)$porcentaje,
    calculate_student_scores(scored_sid)$porcentaje
  )
})
