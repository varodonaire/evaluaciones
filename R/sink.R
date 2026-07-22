# ==============================================================================
# SINK DE RESULTADOS: CONTRATO
# ==============================================================================
# Donde van a parar los resultados de una evaluacion. Dos conceptos:
#
#   run        una corrida: que examen se genero, con que semillas y versiones
#   responses  una fila por alumno y pregunta, con todo lo necesario para
#              recalcular o mejorar el analisis mas adelante
#
# Verbos:
#   rs_write_run        guardar el registro de la corrida
#   rs_write_responses  guardar las respuestas calificadas
#   rs_read_run         recuperar corrida y respuestas
#   rs_list_runs        listar corridas disponibles
#
# El sink local reproduce el comportamiento historico (archivos .xlsx) para que
# nada se rompa; el sink de mongo agrega la persistencia rica.
# ==============================================================================

#' Guardar el registro de una corrida
#'
#' @param sink Objeto results_sink
#' @param run Lista con el registro de la corrida (de build_run_record)
#' @param ... Argumentos adicionales para la implementacion
#'
#' @return Identificador de la corrida
#' @export
rs_write_run <- function(sink, run, ...) {
  UseMethod("rs_write_run")
}

#' Guardar las respuestas calificadas
#'
#' @param sink Objeto results_sink
#' @param scored_data Data frame de respuestas calificadas
#' @param run_id Identificador de la corrida
#' @param ... Argumentos adicionales para la implementacion
#'
#' @return Numero de respuestas guardadas (invisible)
#' @export
rs_write_responses <- function(sink, scored_data, run_id, ...) {
  UseMethod("rs_write_responses")
}

#' Recuperar una corrida con sus respuestas
#'
#' @param sink Objeto results_sink
#' @param run_id Identificador de la corrida
#' @param ... Argumentos adicionales para la implementacion
#'
#' @return Lista con `run` y `responses`
#' @export
rs_read_run <- function(sink, run_id, ...) {
  UseMethod("rs_read_run")
}

#' Listar las corridas disponibles
#'
#' @param sink Objeto results_sink
#' @param ... Filtros para la implementacion
#'
#' @return Data frame con una fila por corrida
#' @export
rs_list_runs <- function(sink, ...) {
  UseMethod("rs_list_runs")
}

#' Print para objetos results_sink
#'
#' @param x Objeto results_sink
#' @param ... Ignorado
#'
#' @return Invisiblemente el objeto
#' @export
print.results_sink <- function(x, ...) {
  cat(strrep("=", 55), "\n", sep = "")
  cat("SINK DE RESULTADOS\n")
  cat(strrep("=", 55), "\n", sep = "")
  cat(sprintf("  Tipo:     %s\n", x$type))
  for (campo in setdiff(names(x), "type")) {
    valor <- x[[campo]]
    if (is.character(valor) && length(valor) == 1) {
      cat(sprintf("  %-9s %s\n", paste0(campo, ":"), valor))
    }
  }
  cat(strrep("=", 55), "\n", sep = "")
  invisible(x)
}

# ------------------------------------------------------------------
# Construccion del registro de corrida
# ------------------------------------------------------------------

#' Construir el registro de una corrida
#'
#' Reune en un solo documento todo lo necesario para regenerar el examen y
#' rehacer el analisis: que version de cada pregunta se uso, con que semilla,
#' en que posicion de que forma, y con que versiones del entorno.
#'
#' @param exam_result Objeto exam_result de generate_exams(), o NULL si la
#'   corrida corresponde a un examen generado fuera del paquete
#' @param processing_config Objeto processing_config, opcional
#' @param session_id Identificador de la sesion, opcional
#' @param assessment_id Identificador de la evaluacion, opcional
#' @param analysis Resumen del analisis a guardar junto a la corrida, opcional
#'
#' @return Lista con el registro de la corrida
#' @export
build_run_record <- function(exam_result = NULL,
                             processing_config = NULL,
                             session_id = NULL,
                             assessment_id = NULL,
                             analysis = NULL) {

  formas <- list()
  config <- NULL
  pool <- NULL

  if (!is.null(exam_result)) {
    config <- exam_result$config
    pool <- exam_result$pool
    formas <- describe_forms(exam_result)
  }

  list(
    run_id = new_run_id(config$name),
    assessment_id = assessment_id %||% config$name,
    session_id = session_id %||% processing_config$session_id,
    applied_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    exam_config = config,
    processing_config = processing_config,
    order_seed = exam_result$order_seed,
    form_mapping = processing_config$form_mapping,
    question_store = describe_store(pool$store),
    forms = formas,
    analysis = analysis,
    versions = list(
      package = as.character(utils::packageVersion("evaluaciones")),
      exams = tryCatch(as.character(utils::packageVersion("exams")),
                       error = function(e) NA_character_),
      r = paste(R.version$major, R.version$minor, sep = ".")
    ),
    created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  )
}

# Operador de valor por defecto. Base R lo trae desde 4.4.0, pero se define
# aqui para no exigir esa version.
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Generar un identificador de corrida
#'
#' No usa `sample()` a proposito. El paquete llama a `set.seed()` al generar las
#' matrices, asi que dos corridas seguidas obtendrian el mismo sufijo aleatorio;
#' y ademas consumir el RNG del usuario tendria efectos a distancia. `tempfile()`
#' da unicidad sin tocar el generador.
#'
#' @param prefix Prefijo legible, normalmente el nombre del examen
#'
#' @return Identificador de corrida
#' @keywords internal
new_run_id <- function(prefix = NULL) {
  sello <- format(Sys.time(), "%Y%m%d%H%M%S")
  sufijo <- sub("^file", "", basename(tempfile("file")))
  paste(c(prefix, sello, sufijo), collapse = "-")
}

#' Describir el store de preguntas para guardarlo en la corrida
#' @keywords internal
describe_store <- function(store) {
  if (is.null(store)) {
    return(NULL)
  }
  campos <- store[vapply(store, function(x) is.character(x) && length(x) == 1,
                         logical(1))]
  as.list(campos)
}

#' Describir cada forma: que pregunta, en que posicion, con que semilla
#'
#' Es el nucleo de la reproducibilidad. Cruza las matrices de la generacion con
#' los question_id del pool, de modo que cada item queda anclado a la version
#' exacta que se aplico.
#'
#' @param exam_result Objeto exam_result
#'
#' @return Lista con una entrada por forma
#' @keywords internal
describe_forms <- function(exam_result) {
  matrices <- exam_result$matrices
  pool <- exam_result$pool

  if (is.null(matrices)) {
    return(list())
  }

  n_forms <- nrow(matrices$order_matrix)
  n_questions <- ncol(matrices$order_matrix)

  # El pool puede venir de un store versionado o de archivos sueltos
  ids <- pool$question_ids %||% tools::file_path_sans_ext(basename(pool$files))
  hashes <- pool$content_hashes %||% rep(NA_character_, length(ids))

  lapply(seq_len(n_forms), function(i) {
    posiciones <- matrices$order_matrix[i, ]
    list(
      form = i,
      label = exam_result$config$form_labels[i],
      items = lapply(seq_len(n_questions), function(j) {
        idx <- posiciones[j]
        list(
          position = j,
          question_id = ids[idx],
          slug = tools::file_path_sans_ext(basename(pool$files[idx])),
          content_hash = hashes[idx],
          section = pool$topics[idx],
          seed = as.integer(matrices$seed_matrix[i, j])
        )
      })
    )
  })
}

#' Estructura vacia de respuestas
#' @keywords internal
empty_responses <- function() {
  tibble(
    run_id = character(0), student_id = character(0), form = integer(0),
    position = integer(0), question_id = character(0), slug = character(0),
    content_hash = character(0), seed = integer(0), section = character(0),
    correcta = character(0), respuesta = character(0), is_correcta = logical(0)
  )
}

#' Normalizar las respuestas calificadas al esquema del sink
#'
#' Una fila por alumno y pregunta, con todo lo necesario para recalcular o
#' mejorar el analisis mas adelante sin volver a la hoja escaneada.
#'
#' @param scored_data Data frame calificado (de score_responses)
#' @param run_id Identificador de la corrida
#' @param id_col Columna identificadora. NULL autodetecta
#' @param run Registro de corrida, para enriquecer con question_id y content_hash
#'
#' @return Data frame con el esquema de respuestas
#' @export
normalize_responses <- function(scored_data, run_id, id_col = NULL, run = NULL) {
  id_col <- resolve_id_col(scored_data, id_col)
  scored_data <- normalize_correct_col(scored_data)

  columna <- function(nombre, default = NA) {
    if (nombre %in% names(scored_data)) scored_data[[nombre]] else default
  }

  out <- tibble(
    run_id = run_id,
    student_id = as.character(scored_data[[id_col]]),
    form = as.integer(columna("Forma", NA_integer_)),
    position = as.integer(columna("Pregunta", NA_integer_)),
    slug = as.character(columna("Origen", NA_character_)),
    section = as.character(columna("Eje", NA_character_)),
    correcta = as.character(columna("Correcta", NA_character_)),
    respuesta = as.character(columna("Respuesta", NA_character_)),
    is_correcta = as.logical(scored_data$is_correcta),
    seed = as.integer(columna("Seed", NA_integer_))
  )

  # Anclar cada respuesta a la version exacta de la pregunta que se aplico
  if (!is.null(run)) {
    items <- run_items_df(run)
    if (nrow(items) > 0) {
      out <- out %>%
        left_join(
          items %>% select("form", "position", "question_id", "content_hash"),
          by = c("form", "position")
        )
    }
  }

  if (!"question_id" %in% names(out)) out$question_id <- NA_character_
  if (!"content_hash" %in% names(out)) out$content_hash <- NA_character_

  out[names(empty_responses())]
}

#' Normalizar respuestas solo si hace falta
#'
#' Los sinks reciben tanto datos calificados crudos como respuestas ya
#' normalizadas. Volver a normalizar lo ya normalizado destruye la informacion,
#' porque los nombres de columna ya no son los que se buscan.
#'
#' @param scored_data Datos calificados o respuestas ya normalizadas
#' @param run_id Identificador de la corrida
#' @param ... Argumentos para normalize_responses()
#'
#' @return Data frame con el esquema de respuestas
#' @keywords internal
ensure_responses <- function(scored_data, run_id, ...) {
  if (all(names(empty_responses()) %in% names(scored_data))) {
    scored_data$run_id <- run_id
    return(scored_data[names(empty_responses())])
  }
  normalize_responses(scored_data, run_id, ...)
}

#' Convertir el items.json a data frame
#'
#' `Alternativas` es un vector por item, asi que un `bind_rows()` ingenuo
#' multiplica cada item en tantas filas como alternativas tenga. Debe quedar
#' como columna-lista: una fila por item, con sus alternativas dentro.
#'
#' @param items Lista de items (de export_items_json o del .json)
#'
#' @return Data frame con una fila por item
#' @export
items_to_df <- function(items) {
  if (is.data.frame(items)) {
    return(items)
  }
  if (length(items) == 0) {
    stop("La lista de items esta vacia.")
  }

  campo <- function(nombre) {
    vapply(items, function(x) {
      valor <- x[[nombre]]
      if (is.null(valor) || length(valor) == 0) NA_character_ else as.character(valor)[1]
    }, character(1))
  }
  entero <- function(nombre) {
    vapply(items, function(x) {
      valor <- x[[nombre]]
      if (is.null(valor) || length(valor) == 0) NA_integer_ else as.integer(valor)[1]
    }, integer(1))
  }

  tibble(
    Texto = campo("Texto"),
    Alternativas = I(lapply(items, function(x) as.character(x$Alternativas))),
    Eje = campo("Eje"),
    Correcta = campo("Correcta"),
    Origen = campo("Origen"),
    Forma = entero("Forma"),
    Pregunta = entero("Pregunta"),
    Seed = entero("Seed")
  )
}

#' Devolver las respuestas persistidas al formato calificado
#'
#' El esquema del sink usa nombres en minuscula y neutros; el analisis y los
#' templates de reporte esperan los nombres historicos. Esta funcion traduce
#' entre ambos, que es lo que permite rehacer un analisis meses despues
#' partiendo solo de lo que quedo guardado.
#'
#' @param responses Data frame de respuestas (de rs_read_run)
#'
#' @return Data frame con el formato que consume el analisis
#' @export
responses_to_scored <- function(responses) {
  if (nrow(responses) == 0) {
    stop("La corrida no tiene respuestas guardadas.")
  }

  tibble(
    student_id = as.character(responses$student_id),
    Forma = as.integer(responses$form),
    Pregunta = as.integer(responses$position),
    Origen = as.character(responses$slug),
    Eje = as.character(responses$section),
    Correcta = as.character(responses$correcta),
    Respuesta = as.character(responses$respuesta),
    is_correcta = as.logical(responses$is_correcta),
    question_id = as.character(responses$question_id),
    Seed = as.integer(responses$seed)
  )
}

#' Aplanar el registro de formas a un data frame
#'
#' @param run Registro de corrida
#'
#' @return Data frame con una fila por forma y posicion
#' @export
run_items_df <- function(run) {
  if (length(run$forms) == 0) {
    return(tibble(
      form = integer(0), position = integer(0), question_id = character(0),
      slug = character(0), content_hash = character(0),
      section = character(0), seed = integer(0)
    ))
  }

  filas <- lapply(run$forms, function(f) {
    bind_rows(lapply(f$items, function(it) {
      tibble(
        form = as.integer(f$form),
        position = as.integer(it$position),
        question_id = as.character(it$question_id %||% NA_character_),
        slug = as.character(it$slug %||% NA_character_),
        content_hash = as.character(it$content_hash %||% NA_character_),
        section = as.character(it$section %||% NA_character_),
        seed = as.integer(it$seed %||% NA_integer_)
      )
    }))
  })

  bind_rows(filas)
}
