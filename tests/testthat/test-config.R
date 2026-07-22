test_that("create_exam_config arma la configuracion por defecto", {
  config <- create_exam_config("prueba1", 2025, 4, "Algebra", num_forms = 5)

  expect_s3_class(config, "exam_config")
  expect_equal(config$num_forms, 5L)
  expect_equal(config$year, 2025L)
  expect_equal(config$form_labels, c("A", "B", "C", "D", "E"))
  # Sin grupos explicitos, un solo grupo cubre todas las formas
  expect_equal(sum(config$groups$sizes), 5)
})

test_that("create_exam_config acepta grupos con seeds propias", {
  config <- create_exam_config(
    "p", 2025, 4, "Mat",
    num_forms = 10,
    groups = list(sizes = c(4, 4, 2), seeds = c(13, 17, 57))
  )

  expect_equal(config$groups$sizes, c(4, 4, 2))
  expect_equal(length(config$groups$seeds), 3)
})

test_that("create_exam_config exige que los grupos sumen num_forms", {
  expect_error(
    create_exam_config("p", 2025, 4, "Mat", num_forms = 10,
                       groups = list(sizes = c(4, 4), seeds = c(1, 2))),
    "debe ser igual a num_forms"
  )
})

test_that("print.exam_config no altera el objeto", {
  config <- create_exam_config("p", 2025, 4, "Mat", num_forms = 2)
  expect_output(print(config), "CONFIGURACION DE EXAMEN")
  # print devuelve el objeto de forma invisible
  utils::capture.output(res <- print(config))
  expect_identical(res, config)
})

test_that("generate_exam_matrices produce matrices con la forma correcta", {
  config <- create_exam_config("p", 2025, 4, "Mat", num_forms = 6,
                               groups = list(sizes = c(3, 3), seeds = c(11, 22)))
  pool <- create_question_pool(paste0("P", 1:4, ".Rnw"), rep("T", 4))

  m <- generate_exam_matrices(config, pool, order_seed = 1)

  expect_equal(dim(m$seed_matrix), c(6, 4))
  expect_equal(dim(m$order_matrix), c(6, 4))
  expect_equal(dim(m$question_matrix), c(6, 4))
})

test_that("generate_exam_matrices reparte cada pregunta una sola vez por forma", {
  config <- create_exam_config("p", 2025, 4, "Mat", num_forms = 3)
  pool <- create_question_pool(paste0("P", 1:5, ".Rnw"), rep("T", 5))

  m <- generate_exam_matrices(config, pool, order_seed = 1)

  # Cada fila es una permutacion completa del pool
  for (i in 1:3) {
    expect_setequal(m$order_matrix[i, ], 1:5)
    expect_setequal(m$question_matrix[i, ], pool$files)
  }
})

# Invariante del diseño original: mismo grupo = mismo examen, distinto orden.
# La semilla debe seguir a la PREGUNTA. exams2pdf indexa `seed` por posicion,
# asi que la fila de semillas se permuta junto con la de preguntas; si no, una
# misma pregunta sale con valores distintos en cada forma del grupo.
test_that("cada pregunta conserva su seed en todas las formas de un grupo", {
  config <- create_exam_config("p", 2025, 4, "Mat", num_forms = 4,
                               groups = list(sizes = c(2, 2), seeds = c(13, 57)))
  pool <- create_question_pool(paste0("P", 1:5, ".Rnw"), rep("T", 5))

  m <- generate_exam_matrices(config, pool, order_seed = 1)

  # Semilla que recibe cada archivo en una forma dada
  seed_de <- function(forma, archivo) {
    m$seed_matrix[forma, which(m$question_matrix[forma, ] == archivo)]
  }

  for (archivo in pool$files) {
    # Grupo 1 (formas 1-2) y grupo 2 (formas 3-4)
    expect_equal(seed_de(1, archivo), seed_de(2, archivo))
    expect_equal(seed_de(3, archivo), seed_de(4, archivo))
  }

  # Y los grupos deben diferir entre si en al menos una pregunta
  expect_false(all(vapply(
    pool$files,
    function(a) isTRUE(all.equal(seed_de(1, a), seed_de(3, a))),
    logical(1)
  )))
})

test_that("las formas de un grupo son el mismo examen en distinto orden", {
  config <- create_exam_config("p", 2025, 4, "Mat", num_forms = 3,
                               groups = list(sizes = c(3), seeds = c(13)))
  pool <- create_question_pool(paste0("P", 1:5, ".Rnw"), rep("T", 5))

  m <- generate_exam_matrices(config, pool, order_seed = 7)

  # El par (archivo, seed) es identico en las 3 formas; solo cambia el orden
  pares <- lapply(1:3, function(i) {
    p <- data.frame(archivo = m$question_matrix[i, ], seed = m$seed_matrix[i, ])
    p[order(p$archivo), ]
  })
  expect_equal(pares[[1]], pares[[2]], ignore_attr = TRUE)
  expect_equal(pares[[1]], pares[[3]], ignore_attr = TRUE)

  # pero el orden de presentacion si difiere
  expect_false(identical(m$question_matrix[1, ], m$question_matrix[2, ]))
})

test_that("create_exam_config exige una seed por grupo", {
  expect_error(
    create_exam_config("p", 2025, 4, "Mat", num_forms = 6,
                       groups = list(sizes = c(3, 3), seeds = 13)),
    "una seed por grupo"
  )
})

test_that("generate_exam_matrices es reproducible con la misma order_seed", {
  config <- create_exam_config("p", 2025, 4, "Mat", num_forms = 3)
  pool <- create_question_pool(paste0("P", 1:5, ".Rnw"), rep("T", 5))

  a <- generate_exam_matrices(config, pool, order_seed = 99)
  b <- generate_exam_matrices(config, pool, order_seed = 99)
  expect_identical(a$order_matrix, b$order_matrix)
})

test_that("create_processing_config expone id_col y su mapeo de formas", {
  config <- create_processing_config(
    results_file = "r.xlsx", items_file = "i.json", students_file = "s.csv",
    num_questions = 25
  )

  expect_s3_class(config, "processing_config")
  expect_equal(config$id_col, "Rut")
  expect_equal(unname(config$form_mapping[["a"]]), 1L)
  expect_equal(config$num_questions, 25)
})

test_that("create_processing_config acepta un id_col distinto", {
  config <- create_processing_config("r.xlsx", "i.json", "s.csv", id_col = "student_id")
  expect_equal(config$id_col, "student_id")
})

test_that("print.processing_config muestra la columna identificadora", {
  config <- create_processing_config("r.xlsx", "i.json", "s.csv")
  expect_output(print(config), "ID columna:\\s+Rut")
})

test_that("safe_read falla con un mensaje util si no existe el archivo", {
  expect_error(safe_read("no_existe_xyz.csv", "csv"), "Archivo no encontrado")
})
