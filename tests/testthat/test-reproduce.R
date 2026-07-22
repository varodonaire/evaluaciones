skip_sin_latex <- function() {
  skip_if_not_installed("exams")
  if (!nzchar(Sys.which("xelatex")) && !nzchar(Sys.which("pdflatex"))) {
    skip("no hay una instalacion de LaTeX disponible")
  }
}

# Store local sembrado con los ejercicios de prueba
store_con_ejercicios <- function(env = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = env)
  file.copy(dir(questions_dir(), pattern = "\\.Rnw$", full.names = TRUE), d)
  question_store_local(d)
}

generar_corrida <- function(store, sink, n_forms = 2, env = parent.frame()) {
  pool <- create_pool_from_store(store)
  config <- create_exam_config(
    "repro", 2025, 4, "Mat", num_forms = n_forms,
    groups = list(sizes = n_forms, seeds = 13),
    output_dir = file.path(withr::local_tempdir(.local_envir = env), "orig")
  )
  res <- suppressWarnings(suppressMessages(
    generate_exams(config, pool, order_seed = 7, verbose = FALSE)
  ))
  run <- build_run_record(res)
  rs_write_run(sink, run)
  list(exam_result = res, run = run)
}

# ------------------------------------------------------------------
# Reconstruccion de matrices
# ------------------------------------------------------------------

test_that("run_matrices reconstruye las matrices de la generacion", {
  res <- exam_result_falso(n_forms = 2, n_questions = 3)
  run <- build_run_record(res)

  m <- run_matrices(run)

  expect_equal(m$seed_matrix, res$matrices$seed_matrix)
  expect_equal(dim(m$question_matrix), dim(res$matrices$question_matrix))
})

test_that("run_matrices falla si la corrida no registra formas", {
  expect_error(run_matrices(build_run_record(NULL)), "no registra ninguna forma")
})

# ------------------------------------------------------------------
# Verificacion de contenido
# ------------------------------------------------------------------

test_that("verify_run_hashes acepta cuando el contenido no cambio", {
  items <- tibble::tibble(question_id = c("A-v1", "B-v1"),
                          content_hash = c("sha256:aa", "sha256:bb"))
  preguntas <- tibble::tibble(question_id = c("A-v1", "B-v1"),
                              content_hash = c("sha256:aa", "sha256:bb"))

  expect_true(suppressMessages(verify_run_hashes(items, preguntas)))
})

test_that("verify_run_hashes aborta si una pregunta cambio", {
  items <- tibble::tibble(question_id = c("A-v1", "B-v1"),
                          content_hash = c("sha256:aa", "sha256:bb"))
  preguntas <- tibble::tibble(question_id = c("A-v1", "B-v1"),
                              content_hash = c("sha256:aa", "sha256:OTRO"))

  expect_error(verify_run_hashes(items, preguntas), "B-v1")
  expect_warning(verify_run_hashes(items, preguntas, strict = FALSE), "cambio")
})

test_that("verify_run_hashes tolera corridas sin hash registrado", {
  items <- tibble::tibble(question_id = "A", content_hash = NA_character_)
  preguntas <- tibble::tibble(question_id = "A", content_hash = "sha256:aa")

  expect_message(verify_run_hashes(items, preguntas), "no se puede verificar")
})

# ------------------------------------------------------------------
# Reproduccion end to end
# ------------------------------------------------------------------

# La prueba de que una corrida quedo bien registrada: partiendo solo del
# run_id, el examen regenerado tiene que ser identico al original.
test_that("reproduce_run regenera el examen identico", {
  skip_sin_latex()

  store <- store_con_ejercicios()
  sink <- results_sink_local(withr::local_tempdir())
  orig <- generar_corrida(store, sink)

  repro <- suppressWarnings(suppressMessages(
    reproduce_run(sink, store, orig$run$run_id,
                  output_dir = file.path(withr::local_tempdir(), "repro"),
                  verbose = FALSE)
  ))

  expect_equal(repro$matrices$seed_matrix, orig$exam_result$matrices$seed_matrix)
  expect_equal(repro$matrices$question_matrix,
               orig$exam_result$matrices$question_matrix)
  expect_equal(repro$reproduced_from, orig$run$run_id)
})

test_that("los items reproducidos conservan clave y semilla", {
  skip_sin_latex()

  store <- store_con_ejercicios()
  sink <- results_sink_local(withr::local_tempdir())
  orig <- generar_corrida(store, sink)

  repro <- suppressWarnings(suppressMessages(
    reproduce_run(sink, store, orig$run$run_id,
                  output_dir = file.path(withr::local_tempdir(), "repro"),
                  verbose = FALSE)
  ))

  clave <- function(items) {
    sort(vapply(items, function(x) {
      paste(x$Forma, x$Pregunta, x$Origen, x$Correcta, x$Seed, sep = "|")
    }, character(1)))
  }

  expect_equal(clave(export_items_json(repro)),
               clave(export_items_json(orig$exam_result)))
})

test_that("reproduce_run produce los PDFs de todas las formas", {
  skip_sin_latex()

  store <- store_con_ejercicios()
  sink <- results_sink_local(withr::local_tempdir())
  orig <- generar_corrida(store, sink, n_forms = 2)

  destino <- file.path(withr::local_tempdir(), "repro")
  suppressWarnings(suppressMessages(
    reproduce_run(sink, store, orig$run$run_id, output_dir = destino, verbose = FALSE)
  ))

  expect_length(dir(destino, pattern = "\\.pdf$"), 2)
})

# El store local no versiona, asi que editar un .Rnw rompe la reproducibilidad
# de las corridas que lo usaron. reproduce_run tiene que darse cuenta.
test_that("reproduce_run detecta que una pregunta cambio en el store local", {
  skip_sin_latex()

  store <- store_con_ejercicios()
  sink <- results_sink_local(withr::local_tempdir())
  orig <- generar_corrida(store, sink, n_forms = 1)

  archivo <- file.path(store$base_dir, "Suma_1.Rnw")
  writeLines(c("% editada despues de la corrida", readLines(archivo, warn = FALSE)),
             archivo)

  expect_error(
    suppressWarnings(suppressMessages(
      reproduce_run(sink, store, orig$run$run_id,
                    output_dir = file.path(withr::local_tempdir(), "r"),
                    verbose = FALSE)
    )),
    "cambio desde la corrida"
  )
})

test_that("strict = FALSE avisa del cambio pero reproduce igual", {
  skip_sin_latex()

  store <- store_con_ejercicios()
  sink <- results_sink_local(withr::local_tempdir())
  orig <- generar_corrida(store, sink, n_forms = 1)

  archivo <- file.path(store$base_dir, "Suma_1.Rnw")
  writeLines(c("% editada", readLines(archivo, warn = FALSE)), archivo)

  destino <- file.path(withr::local_tempdir(), "r")
  avisos <- character(0)
  withCallingHandlers(
    suppressMessages(reproduce_run(sink, store, orig$run$run_id,
                                   output_dir = destino, strict = FALSE,
                                   verbose = FALSE)),
    warning = function(w) {
      avisos <<- c(avisos, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("cambio desde la corrida", avisos)))
  expect_length(dir(destino, pattern = "\\.pdf$"), 1)
})

# Con versiones inmutables la deriva no puede ocurrir: la corrida apunta a una
# version concreta, que editar la pregunta no toca.
test_that("editar una pregunta en mongo no afecta a las corridas previas", {
  skip_if_not_installed("mongolite")
  skip_if_not(mongo_available(), "no hay un MongoDB accesible en localhost")

  sufijo <- paste(sample(letters, 10, replace = TRUE), collapse = "")
  store <- question_store_mongo(db = "evaluaciones_test",
                                collection = paste0("q_", sufijo))
  withr::defer(try(mongolite::mongo(paste0("q_", sufijo), "evaluaciones_test")$drop(),
                   silent = TRUE))

  fuente <- "\\begin{question}Original.\\end{question}\n%% \\exname{R_1}"
  id_v1 <- qs_put(store, fuente, section = "algebra")
  hash_v1 <- qs_get(store, id_v1)$content_hash

  # Se edita despues de que la corrida quedo registrada
  qs_put(store, sub("Original", "Editada", fuente), section = "algebra")

  items <- tibble::tibble(question_id = id_v1, content_hash = hash_v1)
  expect_true(suppressMessages(verify_run_hashes(items, qs_get(store, id_v1))))
  expect_equal(qs_search(store, slug = "R_1")$question_id[1], "R_1-v2")
})
