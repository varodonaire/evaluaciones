skip_sin_mongo <- function() {
  skip_if_not_installed("mongolite")
  skip_if_not(mongo_available(), "no hay un MongoDB accesible en localhost")
}

# Cada test usa su propia coleccion, para que puedan correr en cualquier orden
store_mongo_temporal <- function(env = parent.frame()) {
  col <- paste0("test_", paste(sample(letters, 10, replace = TRUE), collapse = ""))
  store <- question_store_mongo(db = "evaluaciones_test", collection = col)
  withr::defer({
    con <- mongolite::mongo(col, "evaluaciones_test")
    try(con$drop(), silent = TRUE)
  }, envir = env)
  store
}

store_local_temporal <- function(env = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = env)
  for (i in 1:3) {
    escribir_ejercicio_store(d, sprintf("Alg_%d", i), section = "algebra")
  }
  escribir_ejercicio_store(d, "Geo_1", section = "geometria", tags = c("areas"))
  question_store_local(d)
}

escribir_ejercicio_store <- function(dir, name, section = NULL, tags = NULL,
                                     cuerpo = "Pregunta.") {
  meta <- c(
    "%% META-INFORMATION",
    "%% \\extype{schoice}",
    "%% \\exsolution{10000}",
    sprintf("%%%% \\exname{%s}", name),
    if (!is.null(section)) sprintf("%%%% \\exsection{%s}", section),
    "%% \\exauthor{Autor Prueba}",
    "%% \\exversion{1}",
    if (!is.null(tags)) sprintf("%%%% \\extags{%s}", paste(tags, collapse = ", "))
  )
  f <- file.path(dir, paste0(name, ".Rnw"))
  writeLines(c("\\begin{question}", cuerpo, "\\end{question}", "", meta), f)
  f
}

# ------------------------------------------------------------------
# content_hash
# ------------------------------------------------------------------

test_that("content_hash es estable e ignora finales de linea y espacio final", {
  expect_equal(content_hash("abc\ndef"), content_hash("abc\r\ndef"))
  expect_equal(content_hash("abc"), content_hash("abc\n\n"))
  expect_false(content_hash("abc") == content_hash("abd"))
  expect_match(content_hash("abc"), "^sha256:[0-9a-f]{64}$")
})

# ------------------------------------------------------------------
# Store local
# ------------------------------------------------------------------

test_that("qs_search devuelve las columnas del contrato", {
  store <- store_local_temporal()
  res <- qs_search(store)

  expect_setequal(names(res), qs_columns())
  expect_equal(nrow(res), 4)
})

test_that("qs_search filtra por eje, tags y slug", {
  store <- store_local_temporal()

  expect_equal(nrow(qs_search(store, section = "algebra")), 3)
  expect_equal(qs_search(store, section = "geometria")$slug, "Geo_1")
  expect_equal(qs_search(store, tags = "areas")$slug, "Geo_1")
  expect_equal(qs_search(store, slug = "Alg_2")$slug, "Alg_2")
  expect_equal(nrow(qs_search(store, section = "inexistente")), 0)
})

test_that("qs_get trae el source y respeta el orden pedido", {
  store <- store_local_temporal()
  res <- qs_get(store, c("Geo_1", "Alg_1"))

  expect_equal(res$question_id, c("Geo_1", "Alg_1"))
  expect_true(all(nzchar(res$source)))
  expect_true("source" %in% names(res))
})

test_that("qs_get falla nombrando las preguntas ausentes", {
  store <- store_local_temporal()
  expect_error(qs_get(store, c("Alg_1", "NoExiste")), "NoExiste")
})

test_that("qs_materialize escribe archivos utilizables", {
  store <- store_local_temporal()
  d <- withr::local_tempdir()

  rutas <- qs_materialize(store, c("Alg_1", "Geo_1"), dir = d)

  expect_length(rutas, 2)
  expect_true(all(file.exists(rutas)))
  expect_equal(basename(rutas), c("Alg_1.Rnw", "Geo_1.Rnw"))

  # El archivo escrito conserva los metadatos
  meta <- read_exercise_metadata(rutas[2])
  expect_equal(meta$section, "geometria")
  expect_equal(meta$tags, "areas")
})

test_that("qs_put guarda una pregunta nueva en el directorio", {
  d <- withr::local_tempdir()
  store <- question_store_local(d)

  fuente <- paste(c("\\begin{question}", "Nueva.", "\\end{question}",
                    "%% \\extype{schoice}", "%% \\exname{Nueva_1}"),
                  collapse = "\n")
  id <- qs_put(store, fuente, section = "algebra")

  expect_equal(id, "Nueva_1")
  expect_equal(qs_search(store, slug = "Nueva_1")$section, "algebra")
})

test_that("qs_put deduce el slug de exname", {
  d <- withr::local_tempdir()
  store <- question_store_local(d)

  fuente <- "%% \\exname{Deducida_1}"
  expect_equal(qs_put(store, fuente), "Deducida_1")

  expect_error(qs_put(store, "sin metadatos"), "no declara")
})

test_that("un directorio vacio devuelve una tabla vacia con las columnas correctas", {
  store <- question_store_local(withr::local_tempdir())
  res <- qs_search(store)

  expect_equal(nrow(res), 0)
  expect_setequal(names(res), qs_columns())
})

test_that("el store local falla si el directorio no existe", {
  store <- question_store_local(tempfile())
  expect_error(qs_search(store), "no encontrado")
})

# ------------------------------------------------------------------
# Inyeccion de metadatos
# ------------------------------------------------------------------

test_that("inject_exercise_metadata reescribe los campos existentes", {
  fuente <- paste(c("%% \\exname{A_1}", "%% \\exsection{viejo}"), collapse = "\n")
  res <- inject_exercise_metadata(fuente, list(section = "nuevo"))

  expect_match(res, "\\\\exsection\\{nuevo\\}")
  expect_false(grepl("viejo", res))
})

test_that("inject_exercise_metadata agrega los campos ausentes tras exname", {
  fuente <- "%% \\exname{A_1}"
  res <- inject_exercise_metadata(fuente, list(section = "algebra", author = "Yo"))

  expect_match(res, "\\\\exsection\\{algebra\\}")
  expect_match(res, "\\\\exauthor\\{Yo\\}")
  # exname sigue primero
  expect_lt(regexpr("exname", res), regexpr("exsection", res))
})

test_that("inject_exercise_metadata ignora los campos vacios", {
  fuente <- "%% \\exname{A_1}"
  res <- inject_exercise_metadata(fuente, list(section = NA, tags = character(0)))

  expect_false(grepl("exsection", res))
  expect_false(grepl("extags", res))
})

# ------------------------------------------------------------------
# Store de MongoDB
# ------------------------------------------------------------------

test_that("qs_put crea la version 1 y qs_get la devuelve entera", {
  skip_sin_mongo()
  store <- store_mongo_temporal()

  fuente <- "\\begin{question}Hola.\\end{question}\n%% \\exname{M_1}"
  id <- qs_put(store, fuente, section = "algebra", author = "Yo")

  expect_equal(id, "M_1-v1")

  res <- qs_get(store, id)
  expect_equal(res$slug, "M_1")
  expect_equal(res$version, 1L)
  expect_equal(res$section, "algebra")
  expect_equal(res$content_hash, content_hash(fuente))
  expect_match(res$source, "Hola")
})

# El nucleo del diseno: editar una pregunta NO destruye la version anterior,
# de modo que un analisis viejo sigue apuntando al item que realmente se aplico.
test_that("editar una pregunta crea una version nueva sin tocar la anterior", {
  skip_sin_mongo()
  store <- store_mongo_temporal()

  v1_fuente <- "\\begin{question}Original.\\end{question}\n%% \\exname{M_1}"
  v2_fuente <- "\\begin{question}Corregida.\\end{question}\n%% \\exname{M_1}"

  id1 <- qs_put(store, v1_fuente, section = "algebra")
  id2 <- qs_put(store, v2_fuente, section = "algebra")

  expect_equal(c(id1, id2), c("M_1-v1", "M_1-v2"))

  v1 <- qs_get(store, id1)
  v2 <- qs_get(store, id2)

  expect_match(v1$source, "Original")
  expect_false(grepl("Corregida", v1$source))
  expect_match(v2$source, "Corregida")
  expect_false(v1$content_hash == v2$content_hash)
  expect_equal(v2$version, 2L)
})

test_that("reponer contenido identico no crea una version nueva", {
  skip_sin_mongo()
  store <- store_mongo_temporal()

  fuente <- "\\begin{question}Igual.\\end{question}\n%% \\exname{M_1}"
  id1 <- qs_put(store, fuente)
  expect_message(id2 <- qs_put(store, fuente), "sin cambios")

  expect_equal(id1, id2)
  expect_equal(nrow(qs_search(store, status = NULL)), 1)
})

test_that("qs_search contra mongo filtra igual que contra local", {
  skip_sin_mongo()
  store <- store_mongo_temporal()

  qs_put(store, "%% \\exname{A_1}", section = "algebra")
  qs_put(store, "%% \\exname{G_1}", section = "geometria", tags = c("areas"))

  expect_equal(qs_search(store, section = "algebra")$slug, "A_1")
  expect_equal(qs_search(store, tags = "areas")$slug, "G_1")
  expect_setequal(names(qs_search(store)), qs_columns())
})

test_that("qs_get contra mongo falla nombrando lo que no encuentra", {
  skip_sin_mongo()
  store <- store_mongo_temporal()
  qs_put(store, "%% \\exname{A_1}")

  expect_error(qs_get(store, c("A_1-v1", "NoExiste-v9")), "NoExiste-v9")
})

test_that("qs_ensure_indexes es idempotente", {
  skip_sin_mongo()
  store <- store_mongo_temporal()
  qs_put(store, "%% \\exname{A_1}")

  expect_no_error(qs_ensure_indexes(store))
  expect_no_error(qs_ensure_indexes(store))
})

# ------------------------------------------------------------------
# Migracion y equivalencia entre stores
# ------------------------------------------------------------------

test_that("qs_migrate copia todas las preguntas y es idempotente", {
  skip_sin_mongo()
  local <- store_local_temporal()
  mongo <- store_mongo_temporal()

  res <- suppressMessages(qs_migrate(local, mongo, verbose = FALSE))

  expect_equal(nrow(res), 4)
  expect_equal(nrow(qs_search(mongo)), 4)

  # Segunda corrida: no duplica
  suppressMessages(qs_migrate(local, mongo, verbose = FALSE))
  expect_equal(nrow(qs_search(mongo)), 4)
})

# Lo que garantiza que trabajar sin conexion de el mismo resultado
test_that("local y mongo materializan contenido equivalente", {
  skip_sin_mongo()
  local <- store_local_temporal()
  mongo <- store_mongo_temporal()
  suppressMessages(qs_migrate(local, mongo, verbose = FALSE))

  d1 <- withr::local_tempdir()
  d2 <- withr::local_tempdir()

  desde_local <- qs_materialize(local, c("Alg_1", "Geo_1"), dir = d1)
  desde_mongo <- qs_materialize(mongo, c("Alg_1-v1", "Geo_1-v1"), dir = d2)

  expect_equal(basename(desde_local), basename(desde_mongo))
  for (i in seq_along(desde_local)) {
    expect_equal(
      content_hash(readLines(desde_local[i], warn = FALSE)),
      content_hash(readLines(desde_mongo[i], warn = FALSE))
    )
  }
})

# ------------------------------------------------------------------
# Pool desde un store
# ------------------------------------------------------------------

test_that("create_pool_from_store arma un pool con los ids de version", {
  store <- store_local_temporal()
  pool <- create_pool_from_store(store, section = "algebra")

  expect_s3_class(pool, "question_pool")
  expect_equal(pool$n_questions, 3)
  expect_equal(pool$unique_topics, "algebra")
  expect_equal(pool$question_ids, c("Alg_1", "Alg_2", "Alg_3"))
  expect_equal(pool$files, c("Alg_1.Rnw", "Alg_2.Rnw", "Alg_3.Rnw"))
  expect_false(is.null(pool$store))
})

test_that("create_pool_from_store acepta ids explicitos y respeta su orden", {
  store <- store_local_temporal()
  pool <- create_pool_from_store(store, ids = c("Geo_1", "Alg_2"))

  expect_equal(pool$question_ids, c("Geo_1", "Alg_2"))
  expect_equal(pool$topics, c("geometria", "algebra"))
})

test_that("create_pool_from_store falla si la busqueda no devuelve nada", {
  store <- store_local_temporal()
  expect_error(create_pool_from_store(store, section = "inexistente"), "ninguna pregunta")
  expect_error(create_pool_from_store(store, ids = "NoExiste"), "NoExiste")
})

test_that("validate_question_pool valida contra el store, no contra el disco", {
  store <- store_local_temporal()
  pool <- create_pool_from_store(store, section = "algebra")

  expect_message(validate_question_pool(pool), "estan en el store")

  pool$question_ids <- c(pool$question_ids, "Fantasma")
  expect_error(validate_question_pool(pool), "Fantasma")
})

test_that("un pool con store se puede materializar para generar", {
  store <- store_local_temporal()
  pool <- create_pool_from_store(store, section = "algebra")

  d <- withr::local_tempdir()
  rutas <- qs_materialize(pool$store, pool$question_ids, dir = d)

  expect_equal(basename(rutas), pool$files)
})
