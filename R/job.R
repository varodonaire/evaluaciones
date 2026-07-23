# ==============================================================================
# TRABAJOS
# ==============================================================================
# La capa que comparten los adaptadores. Un trabajo es una especificacion en
# JSON que dice que hacer y contra que; `run_job()` la ejecuta y devuelve un
# resultado serializable.
#
# La CLI y la API HTTP son envoltorios delgados sobre esto: ninguno contiene
# logica propia, de modo que ambos hacen exactamente lo mismo y hay un solo
# lugar donde arreglar las cosas.
#
# Especificacion minima:
#
#   { "action": "generate",
#     "store":  { "type": "mongo" },
#     "sink":   { "type": "mongo" },
#     "exam":   { "name": "algebra_b4", "year": 2025, "period": 4,
#                 "subject": "Algebra", "num_forms": 10 },
#     "pool":   { "section": ["logica"] },
#     "output_dir": "salida" }
# ==============================================================================

ACCIONES <- c("generate", "score", "report", "reproduce", "list_runs",
              "search_questions", "migrate_questions")

#' Acciones que acepta run_job()
#'
#' @return Vector con los nombres de las acciones
#' @export
job_actions <- function() {
  ACCIONES
}

#' Ejecutar un trabajo
#'
#' @param spec Lista con la especificacion, o ruta a un archivo .json, o una
#'   cadena JSON
#'
#' @return Lista con el resultado, lista para serializar
#' @export
#'
#' @examples
#' \dontrun{
#' run_job(list(
#'   action = "search_questions",
#'   store = list(type = "local", base_dir = "questions"),
#'   filters = list(section = "logica")
#' ))
#' }
run_job <- function(spec) {
  spec <- as_job_spec(spec)

  if (is.null(spec$action)) {
    stop("La especificacion no declara 'action'.")
  }
  if (!spec$action %in% ACCIONES) {
    stop(sprintf("Accion desconocida: '%s'. Validas: %s",
                 spec$action, paste(ACCIONES, collapse = ", ")))
  }

  ejecutar <- switch(spec$action,
    generate = job_generate,
    score = job_score,
    report = job_report,
    reproduce = job_reproduce,
    list_runs = job_list_runs,
    search_questions = job_search_questions,
    migrate_questions = job_migrate_questions
  )

  resultado <- ejecutar(spec)
  c(list(action = spec$action, status = "ok"), resultado)
}

#' Normalizar la especificacion de un trabajo
#' @keywords internal
as_job_spec <- function(spec) {
  if (is.list(spec)) {
    return(spec)
  }
  if (length(spec) == 1 && file.exists(spec)) {
    return(jsonlite::fromJSON(
      paste(readLines(spec, warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
      simplifyDataFrame = FALSE
    ))
  }
  jsonlite::fromJSON(spec, simplifyDataFrame = FALSE)
}

# ------------------------------------------------------------------
# Constructores desde especificacion
# ------------------------------------------------------------------

#' Construir un store de preguntas desde su especificacion
#'
#' @param spec Lista con `type` ("local" o "mongo") y sus parametros
#'
#' @return Objeto question_store
#' @export
store_from_spec <- function(spec) {
  if (is.null(spec)) {
    stop("El trabajo necesita un 'store' de preguntas.")
  }
  tipo <- spec$type %||% "local"

  switch(tipo,
    local = question_store_local(
      base_dir = spec$base_dir %||% "."
    ),
    mongo = question_store_mongo(
      url = spec$url %||% default_mongo_url(),
      db = spec$db %||% default_mongo_db(),
      collection = spec$collection %||% "questions"
    ),
    stop(sprintf("Tipo de store desconocido: '%s'. Validos: local, mongo", tipo))
  )
}

#' Construir un sink de resultados desde su especificacion
#'
#' @param spec Lista con `type` ("local" o "mongo") y sus parametros
#'
#' @return Objeto results_sink
#' @export
sink_from_spec <- function(spec) {
  if (is.null(spec)) {
    stop("El trabajo necesita un 'sink' de resultados.")
  }
  tipo <- spec$type %||% "local"

  switch(tipo,
    local = results_sink_local(dir = spec$dir %||% "reports"),
    mongo = results_sink_mongo(
      url = spec$url %||% default_mongo_url(),
      db = spec$db %||% default_mongo_db(),
      runs_collection = spec$runs_collection %||% "runs",
      responses_collection = spec$responses_collection %||% "responses"
    ),
    stop(sprintf("Tipo de sink desconocido: '%s'. Validos: local, mongo", tipo))
  )
}

#' Construir un exam_config desde su especificacion
#' @keywords internal
exam_config_from_spec <- function(spec, output_dir = NULL) {
  if (is.null(spec$name)) {
    stop("La configuracion de examen necesita 'name'.")
  }

  grupos <- if (!is.null(spec$groups)) {
    list(sizes = unlist(spec$groups$sizes), seeds = unlist(spec$groups$seeds))
  } else {
    NULL
  }

  create_exam_config(
    name = spec$name,
    year = spec$year %||% as.integer(format(Sys.Date(), "%Y")),
    period = spec$period %||% NA,
    subject = spec$subject %||% NA_character_,
    num_forms = spec$num_forms %||% 1L,
    groups = grupos,
    template = spec$template %||% "plain",
    usepackage = unlist(spec$usepackage %||% c("siunitx", "amsmath", "csquotes")),
    output_dir = output_dir %||% spec$output_dir %||% "exams",
    tex_engine = spec$tex_engine %||% "xelatex"
  )
}

# ------------------------------------------------------------------
# Acciones
# ------------------------------------------------------------------

#' Generar examenes
#' @keywords internal
job_generate <- function(spec) {
  store <- store_from_spec(spec$store)

  filtros <- spec$pool %||% list()
  pool <- create_pool_from_store(
    store,
    ids = unlist(filtros$ids),
    section = unlist(filtros$section),
    tags = unlist(filtros$tags),
    status = filtros$status %||% "published"
  )

  config <- exam_config_from_spec(spec$exam, spec$output_dir)
  resultado <- generate_exams(config, pool,
                              order_seed = spec$order_seed %||% 1234,
                              verbose = isTRUE(spec$verbose))

  items_file <- file.path(resultado$output_dir, paste0(config$name, "_items.json"))
  items <- export_items_json(resultado, items_file)

  salida <- list(
    output_dir = resultado$output_dir,
    pdfs = dir(resultado$output_dir, pattern = "\\.pdf$"),
    items_file = items_file,
    n_items = length(items),
    record_file = resultado$record_file,
    n_forms = config$num_forms,
    n_questions = pool$n_questions,
    question_ids = pool$question_ids
  )

  # Si se pide un sink, la corrida queda registrada de una vez
  if (!is.null(spec$sink)) {
    destino <- sink_from_spec(spec$sink)
    run <- build_run_record(resultado, session_id = spec$session_id,
                            assessment_id = spec$assessment_id)
    rs_write_run(destino, run)
    salida$run_id <- run$run_id
  }

  salida
}

#' Corregir respuestas
#' @keywords internal
job_score <- function(spec) {
  archivos <- spec$files %||% list()

  layout <- if (!is.null(spec$layout)) {
    if (is.character(spec$layout)) layout_from_json(spec$layout) else
      layout_from_json(jsonlite::toJSON(spec$layout, auto_unbox = TRUE))
  } else {
    NULL
  }

  config <- create_processing_config(
    results_file = archivos$results,
    items_file = archivos$items,
    students_file = archivos$students,
    sessions_file = archivos$sessions,
    session_id = spec$session_id,
    num_questions = spec$num_questions %||% 25,
    output_dir = spec$output_dir %||% "reports",
    id_col = spec$id_col %||% "Rut",
    layout = layout
  )

  destino <- if (!is.null(spec$sink)) sink_from_spec(spec$sink) else NULL
  resultado <- process_assessment(config, sink = destino, run_id = spec$run_id)

  list(
    run_id = resultado$run_id,
    output_dir = config$output_dir,
    n_estudiantes = nrow(resultado$student_scores),
    n_respuestas = nrow(resultado$scored_data),
    promedio = mean(resultado$student_scores$porcentaje, na.rm = TRUE)
  )
}

#' Generar los informes de una corrida
#' @keywords internal
job_report <- function(spec) {
  if (is.null(spec$run_id)) {
    stop("La accion 'report' necesita 'run_id'.")
  }

  destino <- sink_from_spec(spec$sink)
  corrida <- rs_read_run(destino, spec$run_id)
  scored <- responses_to_scored(corrida$responses)

  archivos <- spec$files %||% list()
  estudiantes <- if (!is.null(archivos$students)) {
    safe_read(archivos$students, "csv")
  } else {
    NULL
  }

  # Los informes muestran los enunciados y sus alternativas, que no viven en las
  # respuestas sino en el items.json de la generacion. El pipeline historico los
  # arrastraba porque score_responses() los unia al calificar; al reconstruir
  # desde el sink hay que volver a unirlos.
  items_df <- if (!is.null(archivos$items)) {
    items_to_df(safe_read(archivos$items, "json", simplifyDataFrame = FALSE))
  } else {
    NULL
  }

  if (!is.null(items_df)) {
    scored <- scored %>%
      left_join(
        items_df %>% select("Forma", "Pregunta", "Texto", "Alternativas"),
        by = c("Forma", "Pregunta")
      )
  }

  # Los nombres legibles de los ejes se dan como {"slug": "Nombre"}
  ejes_df <- if (!is.null(spec$ejes)) {
    tibble(identificador = names(spec$ejes),
           nombre = unlist(spec$ejes, use.names = FALSE))
  } else {
    NULL
  }

  salida <- spec$output_dir %||% "informes"
  session_id <- spec$session_id %||% spec$run_id

  # Los templates esperan una tabla de sesiones. Si la corrida no vino de una,
  # se sintetiza tratando la corrida misma como la sesion: los estudiantes son
  # los que efectivamente respondieron.
  sessions_df <- tibble(
    session_id = session_id,
    assessment_id = as.character(corrida$run$assessment_id %||% NA_character_),
    class_id = NA_character_,
    date = as.Date(substr(corrida$run$applied_at %||% Sys.Date(), 1, 10)),
    group_name = spec$group_name %||% as.character(corrida$run$assessment_id %||% "Sesion"),
    students = list(unique(scored$student_id))
  )

  resultado <- generate_all_reports(
    metadata = prepare_report_metadata(
      scored_data = scored,
      students_df = estudiantes,
      items_df = items_df,
      ejes_df = ejes_df,
      sessions_df = sessions_df
    ),
    session_id = session_id,
    output_dir = salida,
    generate_individual = spec$individual %||% TRUE,
    generate_session = spec$session %||% TRUE,
    generate_psychometric = spec$psychometric %||% FALSE
  )

  individuales <- resultado$individual_reports
  list(
    run_id = spec$run_id,
    output_dir = normalizePath(salida, mustWork = FALSE),
    pdfs = dir(salida, pattern = "\\.pdf$"),
    n_individuales = if (is.null(individuales)) 0L else sum(individuales$status == "success"),
    n_fallidos = if (is.null(individuales)) 0L else sum(individuales$status != "success")
  )
}

#' Reproducir una corrida
#' @keywords internal
job_reproduce <- function(spec) {
  if (is.null(spec$run_id)) {
    stop("La accion 'reproduce' necesita 'run_id'.")
  }

  destino <- sink_from_spec(spec$sink)
  store <- store_from_spec(spec$store)

  salida <- spec$output_dir %||% file.path(tempdir(), spec$run_id)
  resultado <- reproduce_run(destino, store, spec$run_id,
                             output_dir = salida,
                             strict = spec$strict %||% TRUE,
                             verbose = isTRUE(spec$verbose))

  list(
    run_id = spec$run_id,
    output_dir = resultado$output_dir,
    pdfs = dir(resultado$output_dir, pattern = "\\.pdf$"),
    n_forms = nrow(resultado$matrices$seed_matrix)
  )
}

#' Listar corridas
#' @keywords internal
job_list_runs <- function(spec) {
  runs <- rs_list_runs(sink_from_spec(spec$sink))
  list(n = nrow(runs), runs = runs)
}

#' Buscar preguntas
#' @keywords internal
job_search_questions <- function(spec) {
  filtros <- spec$filters %||% list()
  encontradas <- qs_search(
    store_from_spec(spec$store),
    section = unlist(filtros$section),
    tags = unlist(filtros$tags),
    status = filtros$status %||% "published",
    slug = unlist(filtros$slug)
  )

  list(n = nrow(encontradas), questions = encontradas)
}

#' Migrar preguntas entre stores
#' @keywords internal
job_migrate_questions <- function(spec) {
  origen <- store_from_spec(spec$from)
  destino <- store_from_spec(spec$to)

  resultado <- qs_migrate(origen, destino,
                          ids = unlist(spec$ids),
                          status = spec$status %||% "published",
                          verbose = isTRUE(spec$verbose))

  list(n = nrow(resultado), migrated = resultado)
}
