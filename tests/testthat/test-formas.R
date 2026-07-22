form_mapping <- stats::setNames(1:10, letters[1:10])

test_that("convert_form_codes traduce letras a numeros", {
  df <- tibble::tibble(Forma = c("a", "B", " c "), x = 1:3)
  out <- convert_form_codes(df, form_mapping)

  expect_equal(out$Forma, 1:3)
  expect_equal(out$x, 1:3)
})

# Regresion: si el lector optico entregaba la forma como numero, el mapeo por
# letra devolvia NA y el filter dejaba la tabla vacia sin ningun aviso.
test_that("convert_form_codes acepta formas ya numericas", {
  expect_equal(convert_form_codes(tibble::tibble(Forma = c(1, 2, 3)), form_mapping)$Forma, 1:3)
  expect_equal(convert_form_codes(tibble::tibble(Forma = c("1", "2")), form_mapping)$Forma, 1:2)
})

test_that("convert_form_codes avisa de las filas que descarta", {
  df <- tibble::tibble(Forma = c("a", "zz", NA, "3"), x = 1:4)

  expect_warning(out <- convert_form_codes(df, form_mapping), "2 de 4 filas descartadas")
  expect_equal(nrow(out), 2)
  expect_equal(out$Forma, c(1L, 3L))
})

test_that("convert_form_codes falla si ninguna forma es reconocible", {
  expect_error(
    convert_form_codes(tibble::tibble(Forma = c("zz", "yy")), form_mapping),
    "ninguna de las 2 filas"
  )
})

test_that("convert_form_codes rechaza formas fuera del mapeo", {
  # Con solo 3 formas definidas, la forma 7 no es valida
  fm <- stats::setNames(1:3, letters[1:3])
  expect_warning(
    out <- convert_form_codes(tibble::tibble(Forma = c("a", "7")), fm),
    "1 de 2 filas descartadas"
  )
  expect_equal(out$Forma, 1L)
})

test_that("pivot_responses_long normaliza a formato largo", {
  df <- tibble::tibble(
    Rut = "1-9", Forma = 1L,
    `Respuestas-1` = "A", `Respuestas-2` = " b "
  )
  long <- pivot_responses_long(df)

  expect_equal(nrow(long), 2)
  expect_equal(long$Pregunta, 1:2)
  expect_equal(long$Respuesta, c("a", "b"))
})

test_that("normalize_rut_columns une rut y digito verificador", {
  df <- tibble::tibble(`Rut-0` = "1234567", `verificador-0` = 8L)
  out <- normalize_rut_columns(df)

  expect_true("Rut" %in% names(out))
  expect_equal(out$Rut, "12345678")
})

test_that("normalize_rut_columns traduce el verificador 10 a K", {
  df <- tibble::tibble(`Rut-0` = "1234567", `verificador-0` = 10L)
  expect_equal(normalize_rut_columns(df)$Rut, "1234567K")
})

test_that("normalize_rut_columns deja pasar una columna Rut ya formada", {
  df <- tibble::tibble(Rut = "12345678-9")
  expect_equal(normalize_rut_columns(df)$Rut, "12345678-9")
})

test_that("normalize_rut_columns falla si no hay identificador", {
  expect_error(normalize_rut_columns(tibble::tibble(x = 1)), "No se encontraron columnas de RUT")
})
