skip_sin_mongo <- function() {
  skip_if_not_installed("mongolite")
  skip_if_not(mongo_available(), "no hay un MongoDB accesible en localhost")
}

sink_mongo_temporal <- function(env = parent.frame()) {
  sufijo <- paste(sample(letters, 10, replace = TRUE), collapse = "")
  sink <- results_sink_mongo(
    db = "pruebasudd_test",
    runs_collection = paste0("runs_", sufijo),
    responses_collection = paste0("resp_", sufijo)
  )
  withr::defer({
    for (col in c(sink$runs_collection, sink$responses_collection)) {
      try(mongolite::mongo(col, "pruebasudd_test")$drop(), silent = TRUE)
    }
  }, envir = env)
  sink
}

# ------------------------------------------------------------------
# Registro de corrida
# ------------------------------------------------------------------

test_that("build_run_record describe cada forma, posicion y semilla", {
  res <- exam_result_falso(n_forms = 2, n_questions = 3)
  run <- build_run_record(res)

  expect_length(run$forms, 2)
  items <- run_items_df(run)

  expect_equal(nrow(items), 6)
  expect_setequal(items$form, c(1, 2))
  expect_setequal(items$position, 1:3)
  expect_false(any(is.na(items$seed)))
  expect_false(any(is.na(items$question_id)))
})

test_that("el registro ancla cada item a la version del store", {
  res <- exam_result_falso(con_store = TRUE)
  items <- run_items_df(build_run_record(res))

  expect_true(all(grepl("-v1$", items$question_id)))
  expect_false(any(is.na(items$content_hash)))
})

test_that("sin store el registro cae al slug del archivo", {
  res <- exam_result_falso(con_store = FALSE)
  items <- run_items_df(build_run_record(res))

  expect_true(all(items$question_id %in% c("Q1", "Q2", "Q3")))
  expect_true(all(is.na(items$content_hash)))
})

test_that("la semilla del registro coincide con la de la generacion", {
  res <- exam_result_falso(n_forms = 2, n_questions = 3)
  items <- run_items_df(build_run_record(res))

  for (k in seq_len(nrow(items))) {
    expect_equal(items$seed[k],
                 as.integer(res$matrices$seed_matrix[items$form[k], items$position[k]]))
  }
})

test_that("build_run_record guarda las versiones del entorno", {
  run <- build_run_record(exam_result_falso())

  expect_true(nzchar(run$versions$package))
  expect_true(nzchar(run$versions$r))
  expect_true(nzchar(run$run_id))
})

# Regresion: new_run_id usaba sample(), pero generate_exam_matrices llama a
# set.seed() justo antes, asi que dos corridas seguidas obtenian el mismo id y
# la segunda pisaba a la primera.
test_that("dos corridas seguidas obtienen ids distintos", {
  ids <- vapply(1:5, function(i) build_run_record(exam_result_falso())$run_id,
                character(1))
  expect_length(unique(ids), 5)
})

test_that("generar un run_id no altera el RNG del usuario", {
  set.seed(123)
  esperado <- runif(3)

  set.seed(123)
  invisible(new_run_id("x"))
  expect_equal(runif(3), esperado)
})

test_that("run_items_df devuelve una tabla vacia si no hay formas", {
  run <- build_run_record(NULL)
  expect_equal(nrow(run_items_df(run)), 0)
})

# ------------------------------------------------------------------
# Normalizacion de respuestas
# ------------------------------------------------------------------

test_that("normalize_responses produce una fila por alumno y pregunta", {
  scored <- respuestas_falsas(n_students = 4, n_questions = 3)
  resp <- normalize_responses(scored, "run1")

  expect_equal(nrow(resp), 12)
  expect_setequal(names(resp), names(empty_responses()))
  expect_equal(unique(resp$run_id), "run1")
  expect_equal(sum(resp$is_correcta), 4)
})

test_that("normalize_responses enriquece con question_id desde la corrida", {
  res <- exam_result_falso(n_forms = 1, n_questions = 3)
  run <- build_run_record(res)

  scored <- respuestas_falsas(n_questions = 3, form = 1L)
  resp <- normalize_responses(scored, run$run_id, run = run)

  expect_false(any(is.na(resp$question_id)))
  expect_true(all(grepl("-v1$", resp$question_id)))
  expect_false(any(is.na(resp$content_hash)))
})

test_that("normalize_responses funciona sin corrida asociada", {
  resp <- normalize_responses(respuestas_falsas(), "run1")
  expect_true(all(is.na(resp$question_id)))
})

# ------------------------------------------------------------------
# Sink local
# ------------------------------------------------------------------

test_that("el sink local guarda y recupera una corrida", {
  sink <- results_sink_local(withr::local_tempdir())
  run <- build_run_record(exam_result_falso())

  rs_write_run(sink, run)
  rs_write_responses(sink, respuestas_falsas(), run$run_id)

  leido <- rs_read_run(sink, run$run_id)
  expect_equal(leido$run$run_id, run$run_id)
  expect_equal(nrow(leido$responses), 12)
  expect_equal(length(leido$run$forms), length(run$forms))
})

test_that("el sink local lista las corridas", {
  sink <- results_sink_local(withr::local_tempdir())
  expect_equal(nrow(rs_list_runs(sink)), 0)

  for (i in 1:2) {
    run <- build_run_record(exam_result_falso())
    rs_write_run(sink, run)
    rs_write_responses(sink, respuestas_falsas(), run$run_id)
  }

  listado <- rs_list_runs(sink)
  expect_equal(nrow(listado), 2)
  expect_true(all(listado$n_responses == 12))
})

test_that("el sink local falla al leer una corrida inexistente", {
  sink <- results_sink_local(withr::local_tempdir())
  expect_error(rs_read_run(sink, "no-existe"), "no encontrada")
})

test_that("no se pueden escribir respuestas sin corrida previa", {
  sink <- results_sink_local(withr::local_tempdir())
  expect_error(rs_write_responses(sink, respuestas_falsas(), "fantasma"),
               "No existe la corrida")
})

# ------------------------------------------------------------------
# Sink de MongoDB
# ------------------------------------------------------------------

test_that("el sink de mongo guarda y recupera una corrida entera", {
  skip_sin_mongo()
  sink <- sink_mongo_temporal()
  run <- build_run_record(exam_result_falso(n_forms = 2, n_questions = 3))

  rs_write_run(sink, run)
  rs_write_responses(sink, respuestas_falsas(), run$run_id)

  leido <- rs_read_run(sink, run$run_id)

  expect_equal(leido$run$run_id, run$run_id)
  expect_equal(nrow(leido$responses), 12)

  # Lo que importa: las formas sobreviven al viaje de ida y vuelta
  items_ida <- run_items_df(run)
  items_vuelta <- run_items_df(leido$run)
  expect_equal(nrow(items_vuelta), nrow(items_ida))
  expect_equal(items_vuelta$seed, items_ida$seed)
  expect_equal(items_vuelta$question_id, items_ida$question_id)
})

test_that("reescribir una corrida no duplica sus respuestas", {
  skip_sin_mongo()
  sink <- sink_mongo_temporal()
  run <- build_run_record(exam_result_falso())

  rs_write_run(sink, run)
  rs_write_responses(sink, respuestas_falsas(), run$run_id)
  rs_write_responses(sink, respuestas_falsas(), run$run_id)

  expect_equal(nrow(rs_read_run(sink, run$run_id)$responses), 12)
  expect_equal(nrow(rs_list_runs(sink)), 1)
})

test_that("rs_list_runs cuenta las respuestas de cada corrida", {
  skip_sin_mongo()
  sink <- sink_mongo_temporal()

  run1 <- build_run_record(exam_result_falso())
  rs_write_run(sink, run1)
  rs_write_responses(sink, respuestas_falsas(n_students = 2), run1$run_id)

  run2 <- build_run_record(exam_result_falso())
  rs_write_run(sink, run2)
  rs_write_responses(sink, respuestas_falsas(n_students = 5), run2$run_id)

  listado <- rs_list_runs(sink)
  expect_equal(nrow(listado), 2)
  expect_equal(listado$n_responses[match(run1$run_id, listado$run_id)], 6L)
  expect_equal(listado$n_responses[match(run2$run_id, listado$run_id)], 15L)
})

test_that("leer una corrida inexistente falla nombrandola", {
  skip_sin_mongo()
  sink <- sink_mongo_temporal()
  expect_error(rs_read_run(sink, "no-existe"), "no-existe")
})

test_that("rs_ensure_indexes es idempotente", {
  skip_sin_mongo()
  sink <- sink_mongo_temporal()
  expect_no_error(rs_ensure_indexes(sink))
  expect_no_error(rs_ensure_indexes(sink))
})

# ------------------------------------------------------------------
# Equivalencia entre sinks
# ------------------------------------------------------------------

test_that("local y mongo devuelven la misma corrida", {
  skip_sin_mongo()
  local <- results_sink_local(withr::local_tempdir())
  mongo <- sink_mongo_temporal()

  run <- build_run_record(exam_result_falso(n_forms = 2, n_questions = 3))
  scored <- respuestas_falsas()

  for (s in list(local, mongo)) {
    rs_write_run(s, run)
    rs_write_responses(s, normalize_responses(scored, run$run_id), run$run_id)
  }

  de_local <- rs_read_run(local, run$run_id)
  de_mongo <- rs_read_run(mongo, run$run_id)

  expect_equal(run_items_df(de_local$run), run_items_df(de_mongo$run))
  expect_equal(nrow(de_local$responses), nrow(de_mongo$responses))
  expect_equal(sort(de_local$responses$student_id),
               sort(de_mongo$responses$student_id))
})
