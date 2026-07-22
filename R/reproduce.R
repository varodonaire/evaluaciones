# ==============================================================================
# REPRODUCCION DE CORRIDAS
# ==============================================================================

#' Reconstruir las matrices de generacion desde un registro de corrida
#'
#' El registro guarda, por forma y posicion, que pregunta se aplico y con que
#' semilla. Eso alcanza para rearmar las matrices sin volver a sortear nada.
#'
#' @param run Registro de corrida
#'
#' @return Lista con seed_matrix, order_matrix y question_matrix
#' @export
run_matrices <- function(run) {
  items <- run_items_df(run)
  if (nrow(items) == 0) {
    stop("La corrida no registra ninguna forma; no se puede reproducir.")
  }

  formas <- sort(unique(items$form))
  posiciones <- sort(unique(items$position))
  n_f <- length(formas)
  n_q <- length(posiciones)

  seed_matrix <- matrix(NA_integer_, nrow = n_f, ncol = n_q)
  question_matrix <- matrix(NA_character_, nrow = n_f, ncol = n_q)
  order_matrix <- matrix(NA_integer_, nrow = n_f, ncol = n_q)

  # El orden es el indice de la pregunta dentro del pool de la corrida
  pool_ids <- unique(items$question_id[order(items$form, items$position)])
  pool_ids <- pool_ids[!is.na(pool_ids)]

  for (k in seq_len(nrow(items))) {
    i <- match(items$form[k], formas)
    j <- match(items$position[k], posiciones)
    seed_matrix[i, j] <- items$seed[k]
    question_matrix[i, j] <- paste0(items$slug[k], ".Rnw")
    order_matrix[i, j] <- match(items$question_id[k], pool_ids)
  }

  list(
    seed_matrix = seed_matrix,
    order_matrix = order_matrix,
    question_matrix = question_matrix,
    pool_ids = pool_ids
  )
}

#' Regenerar los examenes de una corrida
#'
#' Toma un `run_id`, recupera el registro, materializa las versiones exactas de
#' las preguntas que se aplicaron y vuelve a generar los PDFs con las mismas
#' semillas. Es la comprobacion de que la corrida quedo bien registrada.
#'
#' Si alguna pregunta cambio en el store desde entonces, se detecta comparando
#' el `content_hash` y se aborta antes de generar nada, a menos que se pida lo
#' contrario con `strict = FALSE`.
#'
#' @param sink Sink de resultados donde vive la corrida
#' @param store Store de preguntas del que traer las versiones
#' @param run_id Identificador de la corrida
#' @param output_dir Directorio de salida para los PDFs
#' @param strict Abortar si el contenido de alguna pregunta ya no coincide
#' @param verbose Mostrar el progreso
#'
#' @return Objeto exam_result de la regeneracion
#' @export
#'
#' @examples
#' \dontrun{
#' sink <- results_sink_mongo()
#' store <- question_store_mongo()
#' reproduce_run(sink, store, "algebra-20250722103000-a1b2c3")
#' }
reproduce_run <- function(sink, store, run_id,
                          output_dir = file.path(tempdir(), run_id),
                          strict = TRUE, verbose = TRUE) {

  corrida <- rs_read_run(sink, run_id)
  run <- corrida$run

  matrices <- run_matrices(run)
  items <- run_items_df(run)

  if (verbose) {
    message(sprintf("Reproduciendo la corrida %s (%d formas, %d preguntas)",
                    run_id, nrow(matrices$seed_matrix), ncol(matrices$seed_matrix)))
  }

  # Traer las versiones exactas que se aplicaron
  ids <- matrices$pool_ids
  preguntas <- qs_get(store, ids)

  verify_run_hashes(items, preguntas, strict = strict, verbose = verbose)

  base_dir <- tempfile("reproduce")
  qs_materialize(store, ids, dir = base_dir)

  config <- rebuild_exam_config(run, output_dir)
  pool <- rebuild_pool(preguntas, base_dir)

  with_exams_attached()

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  output_dir <- normalizePath(output_dir, mustWork = TRUE)

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(base_dir)

  exams_result <- exams2pdf(
    matrices$question_matrix,
    seed = matrices$seed_matrix,
    template = config$template,
    usepackage = config$usepackage,
    texengine = config$tex_engine,
    verbose = FALSE,
    texdir = file.path(output_dir, "tex"),
    name = config$name,
    dir = output_dir
  )

  resultado <- list(
    config = config,
    pool = pool,
    exams_raw = exams_result,
    matrices = matrices[c("seed_matrix", "order_matrix", "question_matrix")],
    order_seed = run$order_seed,
    output_dir = output_dir,
    reproduced_from = run_id,
    timestamp = Sys.time()
  )
  class(resultado) <- c("exam_result", "list")

  if (verbose) {
    message(sprintf("[OK] Corrida reproducida en %s", output_dir))
  }

  resultado
}

#' Comprobar que las preguntas no cambiaron desde la corrida
#' @keywords internal
verify_run_hashes <- function(items, preguntas, strict = TRUE, verbose = TRUE) {
  esperados <- items$content_hash[match(preguntas$question_id, items$question_id)]
  comparables <- !is.na(esperados) & !is.na(preguntas$content_hash)

  if (!any(comparables)) {
    if (verbose) {
      message("La corrida no registra content_hash: no se puede verificar el contenido.")
    }
    return(invisible(TRUE))
  }

  difieren <- comparables & esperados != preguntas$content_hash
  if (any(difieren)) {
    detalle <- paste("-", preguntas$question_id[difieren], collapse = "\n  ")
    msg <- sprintf(
      "El contenido de %d pregunta(s) cambio desde la corrida:\n  %s\nLa reproduccion no seria fiel al examen original.",
      sum(difieren), detalle
    )
    if (strict) {
      stop(msg, call. = FALSE)
    }
    warning(msg, call. = FALSE)
  } else if (verbose) {
    message(sprintf("[OK] Las %d preguntas conservan su content_hash", sum(comparables)))
  }

  invisible(!any(difieren))
}

#' Rearmar el exam_config guardado en una corrida
#' @keywords internal
rebuild_exam_config <- function(run, output_dir) {
  guardada <- run$exam_config

  primero <- function(x, default) {
    if (is.null(x) || length(x) == 0) default else x[[1]]
  }

  config <- list(
    name = primero(guardada$name, "reproducido"),
    year = primero(guardada$year, NA_integer_),
    period = primero(guardada$period, NA),
    subject = primero(guardada$subject, NA_character_),
    num_forms = length(run$forms),
    template = primero(guardada$template, "plain"),
    usepackage = guardada$usepackage %||% c("siunitx", "amsmath", "csquotes"),
    tex_engine = primero(guardada$tex_engine, "xelatex"),
    output_dir = output_dir,
    form_labels = vapply(run$forms, function(f) {
      as.character(f$label %||% NA_character_)[1]
    }, character(1))
  )
  class(config) <- c("exam_config", "list")
  config
}

#' Rearmar un pool a partir de las preguntas traidas del store
#' @keywords internal
rebuild_pool <- function(preguntas, base_dir) {
  topics <- ifelse(is.na(preguntas$section), preguntas$slug, preguntas$section)

  pool <- list(
    files = paste0(preguntas$slug, ".Rnw"),
    topics = topics,
    topic_lookup = setNames(topics, preguntas$slug),
    base_dir = base_dir,
    n_questions = nrow(preguntas),
    unique_topics = unique(topics),
    question_ids = preguntas$question_id,
    content_hashes = preguntas$content_hash
  )
  class(pool) <- c("question_pool", "list")
  pool
}
