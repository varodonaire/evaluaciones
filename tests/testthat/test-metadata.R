escribir_ejercicio <- function(dir, name, section = NULL, author = NULL,
                               version = NULL, tags = NULL) {
  meta <- c(
    "%% META-INFORMATION",
    "%% \\extype{schoice}",
    "%% \\exsolution{10000}",
    sprintf("%%%% \\exname{%s}", name),
    if (!is.null(section)) sprintf("%%%% \\exsection{%s}", section),
    if (!is.null(author)) sprintf("%%%% \\exauthor{%s}", author),
    if (!is.null(version)) sprintf("%%%% \\exversion{%s}", version),
    if (!is.null(tags)) sprintf("%%%% \\extags{%s}", paste(tags, collapse = ", "))
  )
  f <- file.path(dir, paste0(name, ".Rnw"))
  writeLines(c("\\begin{question}", "Pregunta.", "\\end{question}", "", meta), f)
  f
}

test_that("read_exercise_metadata extrae los campos declarados", {
  d <- withr::local_tempdir()
  f <- escribir_ejercicio(d, "Alg_1", section = "algebra",
                          author = "Quien Sea", version = "3",
                          tags = c("uno", "dos"))

  meta <- read_exercise_metadata(f)

  expect_equal(meta$name, "Alg_1")
  expect_equal(meta$section, "algebra")
  expect_equal(meta$author, "Quien Sea")
  expect_equal(meta$version, "3")
  expect_equal(meta$type, "schoice")
  expect_equal(meta$tags, c("uno", "dos"))
})

test_that("read_exercise_metadata devuelve NA para los campos ausentes", {
  d <- withr::local_tempdir()
  f <- escribir_ejercicio(d, "Alg_1")

  meta <- read_exercise_metadata(f)

  expect_equal(meta$name, "Alg_1")
  expect_true(is.na(meta$section))
  expect_true(is.na(meta$author))
  expect_true(all(is.na(meta$tags)))
})

test_that("read_exercise_metadata falla si el archivo no existe", {
  expect_error(read_exercise_metadata(tempfile()), "no encontrado")
})

test_that("read_exercise_metadata_all arma una fila por ejercicio", {
  d <- withr::local_tempdir()
  f1 <- escribir_ejercicio(d, "Alg_1", section = "algebra")
  f2 <- escribir_ejercicio(d, "Geo_1", section = "geometria", tags = "areas")

  meta <- read_exercise_metadata_all(c(f1, f2))

  expect_equal(nrow(meta), 2)
  expect_equal(meta$section, c("algebra", "geometria"))
  expect_equal(meta$tags[[1]], character(0))
  expect_equal(meta$tags[[2]], "areas")
})

# ------------------------------------------------------------------
# Inferencia de ejes en el pool
# ------------------------------------------------------------------

test_that("create_question_pool lee el eje de exsection", {
  d <- withr::local_tempdir()
  escribir_ejercicio(d, "EcCuad_1", section = "ecuaciones_cuadraticas")
  escribir_ejercicio(d, "logica_1", section = "logica")

  expect_message(
    pool <- create_question_pool(c("EcCuad_1.Rnw", "logica_1.Rnw"), base_dir = d),
    "exsection"
  )

  expect_equal(pool$topics, c("ecuaciones_cuadraticas", "logica"))
  expect_equal(unname(pool$topic_lookup[["EcCuad_1"]]), "ecuaciones_cuadraticas")
})

test_that("un topic explicito gana sobre exsection", {
  d <- withr::local_tempdir()
  escribir_ejercicio(d, "EcCuad_1", section = "ecuaciones_cuadraticas")

  pool <- create_question_pool("EcCuad_1.Rnw", topics = "otro_eje", base_dir = d)
  expect_equal(pool$topics, "otro_eje")
})

test_that("sin exsection se cae al patron del nombre de archivo", {
  d <- withr::local_tempdir()
  escribir_ejercicio(d, "Sumatoria_1")   # sin section

  expect_message(
    pool <- create_question_pool("Sumatoria_1.Rnw", base_dir = d),
    "nombres de archivo"
  )
  expect_equal(pool$topics, "Sumatoria")
})

test_that("un pool mixto combina ambas fuentes", {
  d <- withr::local_tempdir()
  escribir_ejercicio(d, "EcCuad_1", section = "ecuaciones_cuadraticas")
  escribir_ejercicio(d, "Sumatoria_1")   # sin section

  expect_message(
    pool <- create_question_pool(c("EcCuad_1.Rnw", "Sumatoria_1.Rnw"), base_dir = d),
    "1 de 2"
  )
  expect_equal(pool$topics, c("ecuaciones_cuadraticas", "Sumatoria"))
})

test_that("archivos inexistentes caen al patron del nombre sin fallar", {
  # create_question_pool no valida existencia; eso lo hace validate_question_pool
  expect_message(
    pool <- create_question_pool(c("A_1.Rnw", "A_2.Rnw"), base_dir = tempfile()),
    "nombres de archivo"
  )
  expect_equal(pool$topics, c("A", "A"))
})

test_that("infer_topics tolera otras extensiones", {
  expect_equal(infer_topics(c("A_1.Rmd", "B_12.Rnw"), base_dir = tempfile()) |>
                 suppressMessages(),
               c("A", "B"))
})

# ------------------------------------------------------------------
# Tabla de ejes
# ------------------------------------------------------------------

test_that("build_ejes_df arma identificador y nombre", {
  pool <- create_question_pool(c("A_1.Rnw", "B_1.Rnw"), c("algebra", "geometria"))

  ejes <- build_ejes_df(pool)
  expect_equal(names(ejes), c("identificador", "nombre"))
  expect_equal(ejes$identificador, c("algebra", "geometria"))
  # Sin nombres, el identificador hace de etiqueta
  expect_equal(ejes$nombre, c("algebra", "geometria"))
})

test_that("build_ejes_df aplica los nombres descriptivos", {
  pool <- create_question_pool(c("A_1.Rnw", "B_1.Rnw"), c("algebra", "geometria"))

  ejes <- build_ejes_df(pool, nombres = c(algebra = "Algebra elemental"))

  expect_equal(ejes$nombre, c("Algebra elemental", "geometria"))
})

test_that("build_ejes_df avisa de nombres que no corresponden a ningun eje", {
  pool <- create_question_pool("A_1.Rnw", "algebra")

  expect_warning(
    build_ejes_df(pool, nombres = c(algebra = "Algebra", inexistente = "Otro")),
    "inexistente"
  )
})

test_that("build_ejes_df se une con la columna Eje como esperan los templates", {
  pool <- create_question_pool(c("A_1.Rnw", "B_1.Rnw"), c("algebra", "geometria"))
  ejes <- build_ejes_df(pool, nombres = c(algebra = "Algebra", geometria = "Geometria"))

  scored <- tibble::tibble(Eje = c("algebra", "geometria", "algebra"))
  unido <- dplyr::left_join(scored, ejes, by = c("Eje" = "identificador"))

  expect_equal(unido$nombre, c("Algebra", "Geometria", "Algebra"))
  expect_false(any(is.na(unido$nombre)))
})
