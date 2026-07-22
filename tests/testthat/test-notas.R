test_that("porcentaje_to_nota respeta los puntos de anclaje", {
  # 0% -> nota minima, exigencia -> nota de aprobacion, 100% -> nota maxima
  expect_equal(porcentaje_to_nota(0), 1.0)
  expect_equal(porcentaje_to_nota(60), 4.0)
  expect_equal(porcentaje_to_nota(100), 7.0)
})

test_that("porcentaje_to_nota interpola linealmente en cada tramo", {
  # Tramo bajo: mitad del camino a la exigencia -> mitad entre 1.0 y 4.0
  expect_equal(porcentaje_to_nota(30), 2.5)
  # Tramo alto: mitad del camino de 60 a 100 -> mitad entre 4.0 y 7.0
  expect_equal(porcentaje_to_nota(80), 5.5)
})

test_that("porcentaje_to_nota es vectorizada", {
  expect_equal(
    porcentaje_to_nota(c(0, 30, 60, 80, 100)),
    c(1.0, 2.5, 4.0, 5.5, 7.0)
  )
})

test_that("porcentaje_to_nota acepta escalas y exigencias distintas", {
  # Exigencia 50%: la mitad exacta debe dar la nota de aprobacion
  expect_equal(porcentaje_to_nota(50, exigencia = 50), 4.0)
  expect_equal(porcentaje_to_nota(25, exigencia = 50), 2.5)

  # Escala 0-10 con aprobacion en 5
  expect_equal(
    porcentaje_to_nota(60, nota_minima = 0, nota_maxima = 10, nota_aprobacion = 5),
    5.0
  )
})

test_that("porcentaje_to_nota acota fuera de rango", {
  expect_equal(porcentaje_to_nota(-20), 1.0)
  expect_equal(porcentaje_to_nota(150), 7.0)
})

test_that("porcentaje_to_nota propaga NA", {
  expect_true(is.na(porcentaje_to_nota(NA_real_)))
})

test_that("add_grades agrega la columna nota sin alterar las demas", {
  scores <- tibble::tibble(Rut = c("1-9", "2-7"), porcentaje = c(100, 60))
  graded <- add_grades(scores)

  expect_true("nota" %in% names(graded))
  expect_equal(graded$nota, c(7.0, 4.0))
  expect_equal(graded$porcentaje, scores$porcentaje)
  expect_equal(nrow(graded), nrow(scores))
})

test_that("add_grades pasa argumentos a porcentaje_to_nota", {
  scores <- tibble::tibble(porcentaje = 50)
  expect_equal(add_grades(scores, exigencia = 50)$nota, 4.0)
})
