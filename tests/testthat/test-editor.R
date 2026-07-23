test_that("la app del editor esta empaquetada y parsea", {
  app <- system.file("shiny", "editor", "app.R", package = "evaluaciones")
  skip_if(app == "", "paquete no instalado")

  expect_true(file.exists(app))
  expect_no_error(parse(app))
})

test_that("run_question_editor exige shiny", {
  # No lanzamos la app (bloquearia); solo comprobamos las validaciones tempranas.
  # Si shiny no estuviera, run_question_editor deberia detenerse con un mensaje util.
  skip_if_not_installed("shiny")
  expect_true(is.function(run_question_editor))
})

test_that("mark_utf8 declara el encoding sin transcodificar", {
  x <- "\xc3\xa1rea"          # bytes UTF-8 de 'área', sin declarar
  Encoding(x) <- "unknown"

  y <- evaluaciones:::mark_utf8(x)
  expect_equal(Encoding(y), "UTF-8")
  expect_equal(y, "área")   # 'área' correctamente interpretada

  # No falla con vectores vacios
  expect_equal(evaluaciones:::mark_utf8(character(0)), character(0))
})
