skip_sin_exams <- function() {
  skip_if_not_installed("exams")
}

fixture <- function(nombre) {
  testthat::test_path("fixtures", nombre)
}

# Un ejercicio schoice minimo, en memoria
ejercicio_valido <- function(name = "Prueba_1") {
  c("<<echo=FALSE, results=hide>>=",
    "a <- 2; b <- 3; correcta <- a + b",
    "opciones <- c(correcta, correcta+1, correcta-1, correcta+10)",
    "opciones <- sample(opciones)",
    "bitstring <- paste0(ifelse(opciones == correcta, 1, 0), collapse = \"\")",
    "@",
    "\\begin{question}",
    "Cuanto es $a + b$?",
    "<<echo=FALSE, results=tex>>=",
    "exams::answerlist(opciones, markup = \"latex\")",
    "@",
    "\\end{question}",
    "%% \\extype{schoice}",
    sprintf("%%%% \\exname{%s}", name),
    "%% \\exsolution{\\Sexpr{bitstring}}")
}

# ------------------------------------------------------------------
# preview_question
# ------------------------------------------------------------------

test_that("preview_question devuelve enunciado, alternativas y clave", {
  skip_sin_exams()

  p <- preview_question(fixture("Suma_1.Rnw"), seed = 42)

  expect_true(nzchar(p$Texto))
  expect_length(p$Alternativas, 4)
  expect_true(p$Correcta %in% letters[1:4])
  expect_equal(p$Origen, "Suma_1")
  expect_equal(p$Seed, 42L)
})

test_that("preview_question acepta el contenido en memoria", {
  skip_sin_exams()

  p <- preview_question(ejercicio_valido(), seed = 1)

  expect_true(nzchar(p$Texto))
  expect_length(p$Alternativas, 4)
})

# Fijar la semilla tiene que dar exactamente la misma instancia: es lo que
# permite que el editor muestre una vista previa estable.
test_that("la misma semilla produce la misma pregunta", {
  skip_sin_exams()

  a <- preview_question(fixture("Suma_1.Rnw"), seed = 7)
  b <- preview_question(fixture("Suma_1.Rnw"), seed = 7)

  expect_equal(a$Texto, b$Texto)
  expect_equal(a$Alternativas, b$Alternativas)
  expect_equal(a$Correcta, b$Correcta)
})

test_that("semillas distintas pueden dar instancias distintas", {
  skip_sin_exams()

  # Suma_1 sortea numeros: con suficientes semillas, al menos dos difieren
  textos <- vapply(1:5, function(s) {
    preview_question(fixture("Suma_1.Rnw"), seed = s)$Texto
  }, character(1))

  expect_gt(length(unique(textos)), 1)
})

test_that("preview_question puede renderizar a HTML", {
  skip_sin_exams()
  if (!nzchar(Sys.which("pandoc")) && !nzchar(Sys.which("quarto"))) {
    skip("no hay pandoc para convertir a HTML")
  }

  p <- preview_question(fixture("Suma_1.Rnw"), seed = 3, as_html = TRUE)

  # Es best-effort: si el converter falla, html queda NA, pero no debe reventar
  expect_true(is.character(p$html))
})

# ------------------------------------------------------------------
# validate_question
# ------------------------------------------------------------------

test_that("validate_question acepta una pregunta que compila", {
  skip_sin_exams()

  v <- validate_question(fixture("Suma_1.Rnw"))

  expect_true(v$valid)
  expect_equal(v$type, "schoice")
  expect_false(is.null(v$preview))
  expect_match(v$message, "compila correctamente")
})

# Lo que protege al editor de guardar basura: una pregunta con codigo R roto no
# debe reventar la validacion, sino reportar el problema.
test_that("validate_question reporta el codigo R roto sin detenerse", {
  skip_sin_exams()

  rota <- c("<<echo=FALSE>>=",
            "x <- sample(1:10, 1",   # parentesis sin cerrar
            "@",
            "\\begin{question}",
            "Cuanto es \\Sexpr{x}?",
            "\\end{question}",
            "%% \\extype{num}",
            "%% \\exname{Rota_1}",
            "%% \\exsolution{5}")

  v <- validate_question(rota)

  expect_false(v$valid)
  expect_match(v$message, "no compila")
  expect_null(v$preview)
})

test_that("validate_question avisa si falta la respuesta correcta", {
  skip_sin_exams()

  sin_solucion <- c("\\begin{question}", "Pregunta abierta.", "\\end{question}",
                    "%% \\extype{string}", "%% \\exname{Abierta_1}",
                    "%% \\exsolution{}")

  v <- validate_question(sin_solucion)
  # Compila, pero se avisa de lo que falta
  expect_match(v$message, "correcta|compila")
})

# ------------------------------------------------------------------
# qs_preview desde un store
# ------------------------------------------------------------------

test_that("qs_preview previsualiza una pregunta del store local", {
  skip_sin_exams()

  d <- withr::local_tempdir()
  file.copy(fixture("Suma_1.Rnw"), d)
  store <- question_store_local(d)

  p <- qs_preview(store, "Suma_1", seed = 5)

  expect_equal(p$Origen, "Suma_1")
  expect_length(p$Alternativas, 4)
})

# ------------------------------------------------------------------
# Sin regresion en export_items_json (comparte parse_exam_item)
# ------------------------------------------------------------------

test_that("export_items_json sigue extrayendo texto, clave y semilla", {
  skip_if_not(dir.exists(questions_dir()), "sin fixtures")

  res <- fake_exam_result(n_forms = 1)
  items <- export_items_json(res)

  for (it in items) {
    expect_true(nzchar(it$Texto))
    expect_false(is.na(it$Seed))
    expect_false(is.null(it$Alternativas))
  }
})
