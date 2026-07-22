skip_sin_mongo <- function() {
  skip_if_not_installed("mongolite")
  skip_if_not(mongo_available(), "no hay un MongoDB accesible en localhost")
}

store_local_sembrado <- function(env = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = env)
  file.copy(dir(questions_dir(), pattern = "\\.Rnw$", full.names = TRUE), d)
  d
}

# ------------------------------------------------------------------
# Especificacion
# ------------------------------------------------------------------

test_that("run_job rechaza acciones desconocidas y faltantes", {
  expect_error(run_job(list(action = "volar")), "Accion desconocida")
  expect_error(run_job(list()), "no declara 'action'")
})

test_that("job_actions lista las acciones implementadas", {
  expect_true(all(c("generate", "score", "reproduce", "list_runs",
                    "search_questions", "migrate_questions") %in% job_actions()))
})

test_that("as_job_spec acepta lista, cadena JSON y archivo", {
  esperado <- list(action = "list_runs")

  expect_equal(as_job_spec(esperado)$action, "list_runs")
  expect_equal(as_job_spec('{"action":"list_runs"}')$action, "list_runs")

  f <- withr::local_tempfile(fileext = ".json")
  writeLines('{"action":"list_runs"}', f)
  expect_equal(as_job_spec(f)$action, "list_runs")
})

# ------------------------------------------------------------------
# Constructores desde especificacion
# ------------------------------------------------------------------

test_that("store_from_spec construye ambos tipos", {
  local <- store_from_spec(list(type = "local", base_dir = "questions"))
  expect_s3_class(local, "question_store_local")
  expect_equal(local$base_dir, "questions")

  skip_if_not_installed("mongolite")
  mongo <- store_from_spec(list(type = "mongo", db = "otra"))
  expect_s3_class(mongo, "question_store_mongo")
  expect_equal(mongo$db, "otra")
})

test_that("store_from_spec aplica los valores por defecto", {
  expect_equal(store_from_spec(list(type = "local"))$base_dir, ".")

  skip_if_not_installed("mongolite")
  m <- store_from_spec(list(type = "mongo"))
  expect_equal(m$db, "evaluaciones")
  expect_equal(m$collection, "questions")
})

test_that("store_from_spec falla con tipos desconocidos o sin spec", {
  expect_error(store_from_spec(list(type = "sqlite")), "Tipo de store desconocido")
  expect_error(store_from_spec(NULL), "necesita un 'store'")
})

test_that("sink_from_spec construye ambos tipos", {
  local <- sink_from_spec(list(type = "local", dir = "salida"))
  expect_s3_class(local, "results_sink_local")
  expect_equal(local$dir, "salida")

  skip_if_not_installed("mongolite")
  expect_s3_class(sink_from_spec(list(type = "mongo")), "results_sink_mongo")
})

test_that("sink_from_spec falla con tipos desconocidos", {
  expect_error(sink_from_spec(list(type = "csv")), "Tipo de sink desconocido")
  expect_error(sink_from_spec(NULL), "necesita un 'sink'")
})

test_that("exam_config_from_spec arma la configuracion", {
  config <- exam_config_from_spec(list(
    name = "prueba", year = 2025, period = 4, subject = "Mat",
    num_forms = 4, groups = list(sizes = c(2, 2), seeds = c(13, 17))
  ))

  expect_s3_class(config, "exam_config")
  expect_equal(config$num_forms, 4L)
  expect_equal(config$groups$sizes, c(2, 2))
})

test_that("exam_config_from_spec exige el nombre", {
  expect_error(exam_config_from_spec(list(year = 2025)), "necesita 'name'")
})

test_that("exam_config_from_spec usa los defaults del paquete", {
  config <- exam_config_from_spec(list(name = "x"))
  expect_equal(config$template, "plain")
  expect_true("csquotes" %in% config$usepackage)
})

# ------------------------------------------------------------------
# Acciones sobre el store local
# ------------------------------------------------------------------

test_that("search_questions devuelve preguntas y respeta los filtros", {
  d <- store_local_sembrado()

  todas <- run_job(list(
    action = "search_questions",
    store = list(type = "local", base_dir = d)
  ))

  expect_equal(todas$status, "ok")
  expect_equal(todas$action, "search_questions")
  expect_equal(todas$n, nrow(todas$questions))
  expect_gt(todas$n, 0)

  filtradas <- run_job(list(
    action = "search_questions",
    store = list(type = "local", base_dir = d),
    filters = list(section = "Suma")
  ))
  expect_true(all(filtradas$questions$section == "Suma"))
})

test_that("list_runs funciona sobre un sink local vacio", {
  res <- run_job(list(
    action = "list_runs",
    sink = list(type = "local", dir = withr::local_tempdir())
  ))

  expect_equal(res$status, "ok")
  expect_equal(res$n, 0)
})

test_that("migrate_questions copia entre dos stores locales", {
  origen <- store_local_sembrado()
  destino <- withr::local_tempdir()

  res <- run_job(list(
    action = "migrate_questions",
    from = list(type = "local", base_dir = origen),
    to = list(type = "local", base_dir = destino)
  ))

  expect_equal(res$status, "ok")
  expect_equal(res$n, length(dir(origen, pattern = "\\.Rnw$")))
  expect_equal(length(dir(destino, pattern = "\\.Rnw$")), res$n)
})

test_that("reproduce exige run_id", {
  expect_error(
    run_job(list(action = "reproduce",
                 sink = list(type = "local", dir = tempdir()),
                 store = list(type = "local", base_dir = "."))),
    "necesita 'run_id'"
  )
})

# ------------------------------------------------------------------
# Generacion y reproduccion completas
# ------------------------------------------------------------------

test_that("generate produce PDFs, items y registra la corrida", {
  skip_if_not_installed("exams")
  if (!nzchar(Sys.which("xelatex")) && !nzchar(Sys.which("pdflatex"))) {
    skip("no hay LaTeX disponible")
  }

  d <- store_local_sembrado()
  salida <- file.path(withr::local_tempdir(), "salida")
  sink_dir <- withr::local_tempdir()

  res <- suppressWarnings(suppressMessages(run_job(list(
    action = "generate",
    store = list(type = "local", base_dir = d),
    sink = list(type = "local", dir = sink_dir),
    exam = list(name = "trabajo", year = 2025, period = 4,
                subject = "Mat", num_forms = 2),
    output_dir = salida,
    order_seed = 5
  ))))

  expect_equal(res$status, "ok")
  expect_length(res$pdfs, 2)
  expect_true(file.exists(res$items_file))
  expect_gt(res$n_items, 0)
  expect_true(nzchar(res$run_id))

  # La corrida quedo registrada en el sink
  listado <- run_job(list(action = "list_runs",
                          sink = list(type = "local", dir = sink_dir)))
  expect_equal(listado$n, 1)
  expect_equal(listado$runs$run_id, res$run_id)
})

test_that("un trabajo de generacion y otro de reproduccion dan el mismo examen", {
  skip_if_not_installed("exams")
  if (!nzchar(Sys.which("xelatex")) && !nzchar(Sys.which("pdflatex"))) {
    skip("no hay LaTeX disponible")
  }

  d <- store_local_sembrado()
  sink_dir <- withr::local_tempdir()
  store_spec <- list(type = "local", base_dir = d)
  sink_spec <- list(type = "local", dir = sink_dir)

  gen <- suppressWarnings(suppressMessages(run_job(list(
    action = "generate", store = store_spec, sink = sink_spec,
    exam = list(name = "ida", year = 2025, period = 4, subject = "Mat",
                num_forms = 2),
    output_dir = file.path(withr::local_tempdir(), "ida"),
    order_seed = 9
  ))))

  repro <- suppressWarnings(suppressMessages(run_job(list(
    action = "reproduce", store = store_spec, sink = sink_spec,
    run_id = gen$run_id,
    output_dir = file.path(withr::local_tempdir(), "vuelta")
  ))))

  expect_equal(repro$status, "ok")
  expect_equal(repro$n_forms, 2)
  expect_length(repro$pdfs, 2)
})

# ------------------------------------------------------------------
# Contra MongoDB
# ------------------------------------------------------------------

test_that("los trabajos funcionan contra mongo igual que contra local", {
  skip_sin_mongo()

  sufijo <- paste(sample(letters, 10, replace = TRUE), collapse = "")
  coleccion <- paste0("q_", sufijo)
  withr::defer(try(mongolite::mongo(coleccion, "evaluaciones_test")$drop(),
                   silent = TRUE))

  origen <- store_local_sembrado()
  mongo_spec <- list(type = "mongo", db = "evaluaciones_test",
                     collection = coleccion)

  migrado <- suppressMessages(run_job(list(
    action = "migrate_questions",
    from = list(type = "local", base_dir = origen),
    to = mongo_spec
  )))
  expect_equal(migrado$status, "ok")

  encontradas <- run_job(list(action = "search_questions", store = mongo_spec))
  expect_equal(encontradas$n, migrado$n)
  expect_true(all(grepl("-v1$", encontradas$questions$question_id)))
})

# ------------------------------------------------------------------
# El resultado tiene que poder viajar como JSON
# ------------------------------------------------------------------

test_that("el resultado de un trabajo se serializa a JSON", {
  d <- store_local_sembrado()
  res <- run_job(list(action = "search_questions",
                      store = list(type = "local", base_dir = d)))

  json <- jsonlite::toJSON(res, auto_unbox = TRUE, null = "null", na = "null")
  devuelto <- jsonlite::fromJSON(json, simplifyDataFrame = FALSE)

  expect_equal(devuelto$status, "ok")
  expect_equal(devuelto$n, res$n)
})
