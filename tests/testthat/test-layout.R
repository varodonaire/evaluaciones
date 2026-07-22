# Layout equivalente a la hoja que se usa hoy, con N preguntas parametrizable
layout_estandar <- function(n = 5, choices = c("a", "b", "c", "d", "e")) {
  answer_sheet_layout(
    n_questions = n,
    choices = choices,
    fields = list(
      sheet_field("rut", role = "student_id", kind = "digits", length = 8),
      sheet_field("verificador", role = "student_id", kind = "digits",
                  length = 1, value_map = list("10" = "K")),
      sheet_field("forma", role = "form", kind = "letter"),
      sheet_field("respuestas", role = "answers", kind = "choice_grid")
    ),
    reader = sheet_reader_excel(rut = "Rut-0", verificador = "verificador-0",
                                forma = "Forma-0", answers_pattern = "^Resp")
  )
}

datos_lector <- function(n_alumnos = 3, n_preguntas = 5, verificadores = NULL) {
  d <- data.frame(
    `Rut-0` = sprintf("%08d", seq_len(n_alumnos)),
    `verificador-0` = verificadores %||% rep(5, n_alumnos),
    `Forma-0` = rep(c("A", "b"), length.out = n_alumnos),
    check.names = FALSE
  )
  for (q in seq_len(n_preguntas)) {
    d[[paste0("Respuestas-", q)]] <- rep("a", n_alumnos)
  }
  d
}

# ------------------------------------------------------------------
# Construccion y validacion
# ------------------------------------------------------------------

test_that("answer_sheet_layout arma un contrato valido", {
  l <- layout_estandar(n = 25)

  expect_s3_class(l, "answer_sheet_layout")
  expect_equal(l$n_questions, 25L)
  expect_length(l$choices, 25)
  expect_length(l$fields, 4)
  expect_equal(l$reader$type, "excel_columns")
})

# El numero de preguntas no es fijo: el layout lo declara y todo se dimensiona
# a partir de ahi.
test_that("el layout admite cualquier numero de preguntas", {
  for (n in c(1, 5, 25, 40, 120)) {
    l <- layout_estandar(n = n)
    expect_equal(l$n_questions, as.integer(n))
    expect_length(l$choices, n)
    expect_equal(layout_choices(l, n), c("a", "b", "c", "d", "e"))
  }
})

test_that("n_questions tiene que ser un entero positivo", {
  expect_error(layout_estandar(n = 0), "entero positivo")
  expect_error(layout_estandar(n = -3), "entero positivo")
})

test_that("cada pregunta puede tener sus propias alternativas", {
  l <- answer_sheet_layout(
    n_questions = 3,
    choices = list(c("a", "b", "c", "d"), c("a", "b", "c", "d", "e"), c("v", "f")),
    fields = list(
      sheet_field("rut", role = "student_id", kind = "digits", length = 8),
      sheet_field("forma", role = "form", kind = "letter"),
      sheet_field("respuestas", role = "answers", kind = "choice_grid")
    ),
    reader = sheet_reader_excel(rut = "Rut-0", forma = "Forma-0",
                                answers_pattern = "^Resp")
  )

  expect_length(layout_choices(l, 1), 4)
  expect_length(layout_choices(l, 2), 5)
  expect_equal(layout_choices(l, 3), c("v", "f"))
})

test_that("las alternativas por pregunta deben cubrir todas las preguntas", {
  expect_error(
    answer_sheet_layout(
      n_questions = 3,
      choices = list(c("a", "b"), c("a", "b")),
      fields = list(
        sheet_field("rut", role = "student_id", kind = "digits"),
        sheet_field("forma", role = "form", kind = "letter"),
        sheet_field("resp", role = "answers", kind = "choice_grid")
      ),
      reader = sheet_reader_excel(rut = "R", forma = "F")
    ),
    "alternativas para 2 preguntas"
  )
})

test_that("layout_choices rechaza preguntas fuera de rango", {
  l <- layout_estandar(n = 5)
  expect_error(layout_choices(l, 6), "fuera del rango")
  expect_error(layout_choices(l, 0), "fuera del rango")
})

test_that("el layout exige los tres roles obligatorios", {
  sin_rol <- function(quitar) {
    campos <- list(
      sheet_field("rut", role = "student_id", kind = "digits"),
      sheet_field("forma", role = "form", kind = "letter"),
      sheet_field("resp", role = "answers", kind = "choice_grid")
    )
    roles <- vapply(campos, function(f) f$role, character(1))
    answer_sheet_layout(2, fields = campos[roles != quitar],
                        reader = sheet_reader_excel(rut = "R"))
  }

  expect_error(sin_rol("student_id"), "student_id")
  expect_error(sin_rol("form"), "form")
  expect_error(sin_rol("answers"), "answers")
})

test_that("no puede haber dos grillas de respuestas ni dos formas", {
  campos <- list(
    sheet_field("rut", role = "student_id", kind = "digits"),
    sheet_field("forma", role = "form", kind = "letter"),
    sheet_field("r1", role = "answers", kind = "choice_grid"),
    sheet_field("r2", role = "answers", kind = "choice_grid")
  )
  expect_error(answer_sheet_layout(2, fields = campos,
                                   reader = sheet_reader_excel(rut = "R")),
               "exactamente un campo con rol 'answers'")
})

test_that("los nombres de campo no se pueden repetir", {
  campos <- list(
    sheet_field("x", role = "student_id", kind = "digits"),
    sheet_field("x", role = "form", kind = "letter"),
    sheet_field("r", role = "answers", kind = "choice_grid")
  )
  expect_error(answer_sheet_layout(2, fields = campos,
                                   reader = sheet_reader_excel(x = "X")),
               "nombre repetido")
})

test_that("sheet_field rechaza roles y tipos desconocidos", {
  expect_error(sheet_field("x", role = "inventado"))
  expect_error(sheet_field("x", role = "info", kind = "inventado"))
})

test_that("print muestra el contrato", {
  expect_output(print(layout_estandar(n = 25)), "HOJA DE RESPUESTA")
  expect_output(print(layout_estandar(n = 25)), "25")
})

# ------------------------------------------------------------------
# Serializacion: el contrato viaja como JSON
# ------------------------------------------------------------------

test_that("un layout sobrevive el viaje de ida y vuelta a JSON", {
  original <- layout_estandar(n = 25)
  copia <- layout_from_json(layout_to_json(original))

  expect_equal(layout_to_json(copia), layout_to_json(original))
  expect_equal(copia$n_questions, original$n_questions)
  expect_equal(copia$fields[[2]]$value_map$`10`, "K")
})

test_that("las alternativas por pregunta sobreviven al JSON", {
  original <- answer_sheet_layout(
    n_questions = 3,
    choices = list(c("a", "b", "c", "d"), c("a", "b", "c", "d", "e"), c("v", "f")),
    fields = list(
      sheet_field("rut", role = "student_id", kind = "digits"),
      sheet_field("forma", role = "form", kind = "letter"),
      sheet_field("resp", role = "answers", kind = "choice_grid")
    ),
    reader = sheet_reader_excel(rut = "R", forma = "F", answers_pattern = "^Resp")
  )

  copia <- layout_from_json(layout_to_json(original))
  expect_equal(layout_choices(copia, 1), c("a", "b", "c", "d"))
  expect_equal(layout_choices(copia, 3), c("v", "f"))
})

test_that("layout_to_json puede escribir a un archivo", {
  f <- withr::local_tempfile(fileext = ".json")
  layout_to_json(layout_estandar(n = 10), f)

  expect_true(file.exists(f))
  expect_equal(layout_from_json(f)$n_questions, 10L)
})

# ------------------------------------------------------------------
# Lectura dirigida por el contrato
# ------------------------------------------------------------------

test_that("sheet_read produce la tabla que espera score_responses", {
  datos <- sheet_read(layout_estandar(n = 5), datos_lector(3, 5))

  expect_setequal(names(datos), c("student_id", "Forma", "Pregunta", "Respuesta"))
  expect_equal(nrow(datos), 15)
  expect_setequal(datos$Pregunta, 1:5)
  expect_type(datos$Pregunta, "integer")
})

test_that("el identificador se compone de los campos en el orden declarado", {
  datos <- sheet_read(layout_estandar(n = 2), datos_lector(1, 2, verificadores = 7))
  expect_equal(unique(datos$student_id), "000000017")
})

# El verificador 10 representa la K del RUT chileno. Se resuelve con value_map,
# de forma declarativa, en vez de con codigo especial para Chile.
test_that("value_map traduce el verificador 10 a K", {
  datos <- sheet_read(layout_estandar(n = 2), datos_lector(2, 2, verificadores = c(10, 3)))
  expect_setequal(unique(datos$student_id), c("00000001K", "000000023"))
})

test_that("la forma se normaliza a minusculas", {
  datos <- sheet_read(layout_estandar(n = 2), datos_lector(2, 2))
  expect_setequal(unique(datos$Forma), c("a", "b"))
})

# Es el error que hoy pasa desapercibido: la hoja impresa y el archivo leido
# tienen que declarar el mismo numero de preguntas.
test_that("sheet_read detecta que el archivo no calza con el layout", {
  expect_error(sheet_read(layout_estandar(n = 25), datos_lector(3, 30)),
               "declara 25 preguntas pero el archivo trae 30")
  expect_error(sheet_read(layout_estandar(n = 25), datos_lector(3, 20)),
               "declara 25 preguntas pero el archivo trae 20")
})

test_that("sheet_read falla si no encuentra las columnas de respuesta", {
  d <- datos_lector(2, 3)
  names(d) <- sub("^Respuestas", "Otra", names(d))
  expect_error(sheet_read(layout_estandar(n = 3), d), "Ninguna columna coincide")
})

test_that("sheet_read falla nombrando el campo que no encuentra", {
  d <- datos_lector(2, 3)
  d[["Forma-0"]] <- NULL
  expect_error(sheet_read(layout_estandar(n = 3), d), "forma")
})

test_that("sheet_read avisa de respuestas fuera de las alternativas", {
  d <- datos_lector(1, 3)
  d[["Respuestas-2"]] <- "z"

  expect_warning(sheet_read(layout_estandar(n = 3), d), "fuera de las alternativas")
})

test_that("las omisiones no cuentan como respuesta invalida", {
  d <- datos_lector(1, 3)
  d[["Respuestas-2"]] <- NA
  d[["Respuestas-3"]] <- ""

  expect_no_warning(datos <- sheet_read(layout_estandar(n = 3), d))
  expect_true(any(is.na(datos$Respuesta) | !nzchar(datos$Respuesta)))
})

test_that("sheet_read rechaza un tipo de lector no implementado", {
  l <- layout_estandar(n = 3)
  l$reader$type <- "omr"
  expect_error(sheet_read(l, datos_lector(1, 3)), "No hay lector implementado")
})

# ------------------------------------------------------------------
# Equivalencia con el pipeline historico
# ------------------------------------------------------------------

# Mientras exista el camino viejo, ambos tienen que dar lo mismo: es lo que
# permite migrar sin sobresaltos.
test_that("el contrato produce el mismo identificador que normalize_rut_columns", {
  d <- datos_lector(4, 3, verificadores = c(10, 5, 10, 1))

  por_contrato <- sort(unique(sheet_read(layout_estandar(n = 3), d)$student_id))
  por_convencion <- sort(normalize_rut_columns(d)$Rut)

  expect_equal(por_contrato, por_convencion)
})

test_that("el contrato produce las mismas respuestas que el camino historico", {
  set.seed(11)
  d <- datos_lector(3, 4)
  for (q in 1:4) d[[paste0("Respuestas-", q)]] <- sample(letters[1:4], 3, TRUE)

  por_contrato <- sheet_read(layout_estandar(n = 4), d) %>%
    dplyr::arrange(.data$student_id, .data$Pregunta)

  por_convencion <- d %>%
    normalize_rut_columns() %>%
    standardize_response_columns(4) %>%
    pivot_responses_long() %>%
    dplyr::arrange(.data$Rut, .data$Pregunta)

  expect_equal(por_contrato$Respuesta, por_convencion$Respuesta)
  expect_equal(por_contrato$student_id, por_convencion$Rut)
})

# ------------------------------------------------------------------
# Integracion con el procesamiento
# ------------------------------------------------------------------

test_that("create_processing_config toma num_questions del layout", {
  cfg <- create_processing_config("r.xlsx", "i.json", "e.csv",
                                  num_questions = 99, layout = layout_estandar(n = 12))

  expect_equal(cfg$num_questions, 12L)
  expect_equal(cfg$layout$layout_id, layout_estandar(n = 12)$layout_id)
})

test_that("sin layout la configuracion se comporta como antes", {
  cfg <- create_processing_config("r.xlsx", "i.json", "e.csv", num_questions = 25)

  expect_null(cfg$layout)
  expect_equal(cfg$num_questions, 25)
})

test_that("print.processing_config informa si hay layout", {
  expect_output(print(create_processing_config("r", "i", "e")), "por convencion")
  expect_output(
    print(create_processing_config("r", "i", "e", layout = layout_estandar(n = 5))),
    "layout-5p"
  )
})
