test_that("create_question_pool acepta vectores separados", {
  pool <- create_question_pool(
    questions = c("A_1.Rnw", "A_2.Rnw", "B_1.Rnw"),
    topics = c("Tema1", "Tema1", "Tema2")
  )

  expect_s3_class(pool, "question_pool")
  expect_equal(pool$n_questions, 3)
  expect_equal(pool$unique_topics, c("Tema1", "Tema2"))
  expect_equal(unname(pool$topic_lookup[["A_1"]]), "Tema1")
})

test_that("create_question_pool acepta data.frame", {
  pool <- create_question_pool(
    questions = data.frame(
      file = c("P1.Rnw", "P2.Rnw"),
      topic = c("T1", "T2")
    )
  )

  expect_equal(pool$n_questions, 2)
  expect_equal(pool$topics, c("T1", "T2"))
})

test_that("create_question_pool valida entradas inconsistentes", {
  expect_error(
    create_question_pool(c("A.Rnw", "B.Rnw"), topics = "SoloUno"),
    "numero de preguntas"
  )
  expect_error(
    create_question_pool(data.frame(archivo = "A.Rnw")),
    "columnas 'file' y 'topic'"
  )
})

test_that("create_question_pool infiere topicos del nombre de archivo", {
  expect_message(
    pool <- create_question_pool(c("ProgArit_1.Rnw", "ProgArit_2.Rnw")),
    "Topicos inferidos"
  )
  expect_equal(unique(pool$topics), "ProgArit")
})

# Regresion: con nombres repetidos, patterns[[topic]] devolvia siempre el
# primer patron, perdiendo el resto en silencio.
test_that("create_pool_from_patterns no pierde patrones con temas repetidos", {
  pool <- create_pool_from_patterns(
    patterns = list(
      "Progresiones" = "ProgArit_",
      "Progresiones" = "ProgGeom_",
      "Sumatorias"   = "Sumatoria_"
    ),
    nums = 1:3
  )

  expect_equal(pool$n_questions, 9)
  expect_true(all(c("ProgArit_1.Rnw", "ProgGeom_1.Rnw", "Sumatoria_1.Rnw") %in% pool$files))
  expect_equal(sum(pool$topics == "Progresiones"), 6)
  expect_equal(sum(pool$topics == "Sumatorias"), 3)
})

test_that("create_pool_from_patterns acepta un vector de patrones por tema", {
  pool <- create_pool_from_patterns(
    patterns = list(
      "Progresiones" = c("ProgArit_", "ProgGeom_"),
      "Sumatorias"   = "Sumatoria_"
    ),
    nums = 1:3
  )

  expect_equal(pool$n_questions, 9)
  expect_equal(unname(pool$topic_lookup[["ProgGeom_2"]]), "Progresiones")
})

test_that("create_pool_from_patterns avisa de archivos duplicados", {
  expect_warning(
    create_pool_from_patterns(list("T1" = "P_", "T2" = "P_"), nums = 1:2),
    "duplicados"
  )
})

test_that("create_pool_from_patterns exige lista nombrada", {
  expect_error(create_pool_from_patterns(list("P_", "Q_")), "lista nombrada")
})

test_that("create_pool_from_patterns respeta la extension", {
  pool <- create_pool_from_patterns(list("T" = "P_"), nums = 1, extension = ".Rmd")
  expect_equal(pool$files, "P_1.Rmd")
})

test_that("validate_question_pool detecta archivos faltantes", {
  tmp <- withr::local_tempdir()
  file.create(file.path(tmp, "existe.Rnw"))

  pool_ok <- create_question_pool("existe.Rnw", "T", base_dir = tmp)
  expect_message(validate_question_pool(pool_ok), "Todos los 1 archivos")

  pool_bad <- create_question_pool(c("existe.Rnw", "falta.Rnw"), c("T", "T"), base_dir = tmp)
  expect_error(validate_question_pool(pool_bad), "falta.Rnw")
  expect_warning(validate_question_pool(pool_bad, stop_on_missing = FALSE), "falta.Rnw")
})
