# Estos tests usan xexams(), el motor comun bajo exams2pdf, para poder ejercitar
# la generacion y la exportacion de items sin necesidad de LaTeX.

skip_if_no_questions <- function() {
  skip_if_not_installed("exams")
  if (!dir.exists(questions_dir())) {
    skip("directorio de ejercicios de prueba no disponible")
  }
}

skip_if_no_latex <- function() {
  skip_if_not_installed("exams")
  if (!nzchar(Sys.which("xelatex")) && !nzchar(Sys.which("pdflatex"))) {
    skip("no hay una instalacion de LaTeX disponible")
  }
}

# El template "plain" carga inputenc, que xelatex (el motor por defecto) ignora.
# Es cosmetico y no afecta al PDF resultante.
generate_quiet <- function(...) suppressWarnings(suppressMessages(generate_exams(...)))

# ------------------------------------------------------------------
# Template
# ------------------------------------------------------------------

# Regresion: create_exam_config ponia template = NULL y generate_exams se lo
# pasaba tal cual a exams2pdf, que no lo acepta ("invalid 'file' argument").
# La generacion fallaba con la configuracion por defecto.
test_that("el template por defecto es utilizable", {
  config <- create_exam_config("p", 2025, 4, "Mat", num_forms = 2)
  expect_equal(config$template, "plain")
  expect_true(all(c("siunitx", "amsmath") %in% config$usepackage))
})

test_that("generate_exams produce PDFs con la configuracion por defecto", {
  skip_if_no_latex()

  outdir <- withr::local_tempdir()
  config <- create_exam_config("e2e", 2025, 4, "Mat", num_forms = 2,
                               output_dir = file.path(outdir, "salida"))
  pool <- create_question_pool(
    c("Suma_1.Rnw", "Suma_2.Rnw", "Resta_1.Rnw"),
    c("Suma", "Suma", "Resta"),
    base_dir = normalizePath(questions_dir())
  )

  res <- generate_quiet(config, pool, order_seed = 5, verbose = FALSE)

  expect_s3_class(res, "exam_result")
  expect_length(dir(res$output_dir, pattern = "\\.pdf$"), 2)
  expect_true(file.exists(res$record_file))
})

# Regresion: el header `forma` se pasaba siempre, pero los templates de 'exams'
# no definen \forma y LaTeX abortaba con "Undefined control sequence".
test_that("el header forma solo se usa con un template propio", {
  skip_if_no_latex()

  outdir <- withr::local_tempdir()
  config <- create_exam_config("hdr", 2025, 4, "Mat", num_forms = 1,
                               output_dir = file.path(outdir, "salida"))
  pool <- create_question_pool("Suma_1.Rnw", "Suma",
                               base_dir = normalizePath(questions_dir()))

  expect_null(config$header)
  expect_no_error(generate_quiet(config, pool, verbose = FALSE))
})

test_that("una configuracion antigua con template NULL sigue funcionando", {
  skip_if_no_latex()

  outdir <- withr::local_tempdir()
  config <- create_exam_config("compat", 2025, 4, "Mat", num_forms = 1,
                               output_dir = file.path(outdir, "salida"))
  config$template <- NULL   # como lo dejaba la version anterior

  pool <- create_question_pool("Suma_1.Rnw", "Suma",
                               base_dir = normalizePath(questions_dir()))

  expect_no_error(generate_quiet(config, pool, verbose = FALSE))
})

# ------------------------------------------------------------------
# Registro de generacion
# ------------------------------------------------------------------

test_that("generation_record captura ambas matrices, la order_seed y las versiones", {
  config <- create_exam_config("p", 2025, 4, "Mat", num_forms = 2)
  pool <- create_question_pool(paste0("P", 1:3, ".Rnw"), rep("T", 3))
  matrices <- generate_exam_matrices(config, pool, order_seed = 7)

  rec <- generation_record(config, pool, matrices, order_seed = 7)

  expect_equal(rec$order_seed, 7)
  expect_identical(rec$matrices$seed_matrix, matrices$seed_matrix)
  expect_identical(rec$matrices$order_matrix, matrices$order_matrix)
  expect_true(nzchar(rec$versions$package))
  expect_true(nzchar(rec$versions$r))
})

test_that("read_generation_record devuelve lo guardado", {
  config <- create_exam_config("p", 2025, 4, "Mat", num_forms = 2)
  pool <- create_question_pool(paste0("P", 1:3, ".Rnw"), rep("T", 3))
  matrices <- generate_exam_matrices(config, pool, order_seed = 7)

  f <- withr::local_tempfile(fileext = ".rds")
  saveRDS(generation_record(config, pool, matrices, order_seed = 7), f)

  expect_identical(read_generation_record(f)$matrices$seed_matrix,
                   matrices$seed_matrix)
  expect_error(read_generation_record(tempfile()), "no encontrado")
})

# ------------------------------------------------------------------
# Exportacion de items
# ------------------------------------------------------------------

test_that("export_items_json exporta la semilla de cada item", {
  skip_if_no_questions()

  res <- fake_exam_result(n_forms = 2)
  items <- export_items_json(res)

  seeds <- vapply(items, function(x) x$Seed, integer(1))
  expect_false(any(is.na(seeds)))
})

# exams registra en metainfo la semilla que realmente uso. Debe coincidir con
# la que le pasamos en seed_matrix; si algun dia divergieran, es senal de que
# exams reinterpreto la matriz y la generacion no seria reproducible.
test_that("la semilla exportada coincide con la seed_matrix de la generacion", {
  skip_if_no_questions()

  res <- fake_exam_result(n_forms = 2)
  items <- export_items_json(res)
  sm <- res$matrices$seed_matrix

  for (it in items) {
    expect_equal(it$Seed, as.integer(sm[it$Forma, it$Pregunta]))
  }
})

test_that("export_items_json cae a la seed_matrix si metainfo no trae semilla", {
  skip_if_no_questions()

  res <- fake_exam_result(n_forms = 1)
  # Simular un resultado generado sin pasar `seed`: exams no agrega el campo
  for (j in seq_along(res$exams_raw[[1]])) {
    res$exams_raw[[1]][[j]]$metainfo$seed <- NULL
  }

  items <- export_items_json(res)
  seeds <- vapply(items, function(x) x$Seed, integer(1))

  expect_false(any(is.na(seeds)))
  expect_equal(seeds, as.integer(res$matrices$seed_matrix[1, ]))
})

test_that("sin metainfo$seed ni seed_matrix la semilla queda NA", {
  skip_if_no_questions()

  res <- fake_exam_result(n_forms = 1)
  for (j in seq_along(res$exams_raw[[1]])) {
    res$exams_raw[[1]][[j]]$metainfo$seed <- NULL
  }
  res$matrices$seed_matrix <- NULL

  items <- export_items_json(res)
  expect_true(all(is.na(vapply(items, function(x) x$Seed, integer(1)))))
})

# Invariante del diseno: las formas de un grupo son el mismo examen reordenado,
# asi que una pregunta debe llevar la misma semilla en todas ellas.
test_that("una pregunta conserva su semilla en todas las formas del grupo", {
  skip_if_no_questions()

  res <- fake_exam_result(n_forms = 3, sizes = 3, seeds = 13)
  items <- export_items_json(res)

  por_origen <- split(items, vapply(items, function(x) x$Origen, character(1)))
  for (grupo in por_origen) {
    expect_length(unique(vapply(grupo, function(x) x$Seed, integer(1))), 1)
  }
})

test_that("export_items_json resuelve el eje desde el pool", {
  skip_if_no_questions()

  res <- fake_exam_result(n_forms = 1)
  items <- export_items_json(res)

  ejes <- vapply(items, function(x) as.character(x$Eje), character(1))
  expect_false(any(is.na(ejes)))
  expect_true(all(ejes %in% res$pool$unique_topics))
})

test_that("export_items_json escribe el JSON cuando se le da output_file", {
  skip_if_no_questions()

  res <- fake_exam_result(n_forms = 1)
  f <- withr::local_tempfile(fileext = ".json")

  suppressMessages(export_items_json(res, output_file = f))
  expect_true(file.exists(f))

  leidos <- jsonlite::fromJSON(f, simplifyDataFrame = FALSE)
  expect_length(leidos, res$pool$n_questions)
  expect_false(any(vapply(leidos, function(x) is.null(x$Seed), logical(1))))
})
