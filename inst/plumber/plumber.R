# ==============================================================================
# API HTTP de PruebasUDD
# ==============================================================================
# Los mismos verbos que la CLI, sobre run_job(). Ninguna logica propia: si algo
# se comporta distinto por HTTP que por linea de comandos, es un bug.
#
# Levantar:
#
#   plumber::pr_run(plumber::pr(system.file("plumber/plumber.R",
#                                           package = "PruebasUDD")), port = 8000)
#
# La generacion de examenes necesita LaTeX y puede tardar; en produccion
# conviene encolarla en vez de servirla sincrona.
# ==============================================================================

suppressMessages(library(PruebasUDD))

#* @apiTitle PruebasUDD
#* @apiDescription Generacion y revision de pruebas educativas
#* @apiVersion 0.2.0

# ------------------------------------------------------------------
# Manejo uniforme de errores
# ------------------------------------------------------------------

#* @filter errores
function(req, res) {
  tryCatch(
    plumber::forward(),
    error = function(e) {
      res$status <- 400
      list(status = "error", message = conditionMessage(e))
    }
  )
}

responder <- function(res, expr) {
  tryCatch(
    expr,
    error = function(e) {
      res$status <- 400
      list(status = "error", message = conditionMessage(e))
    }
  )
}

# ------------------------------------------------------------------
# Estado
# ------------------------------------------------------------------

#* Comprobar que el servicio responde
#* @get /health
#* @serializer unboxedJSON
function() {
  list(
    status = "ok",
    package = as.character(utils::packageVersion("PruebasUDD")),
    r = paste(R.version$major, R.version$minor, sep = "."),
    latex = nzchar(Sys.which("xelatex")) || nzchar(Sys.which("pdflatex")),
    mongo = mongo_available(),
    actions = job_actions()
  )
}

# ------------------------------------------------------------------
# Punto de entrada generico
# ------------------------------------------------------------------

#* Ejecutar cualquier trabajo
#* @post /jobs
#* @serializer unboxedJSON
function(req, res) {
  responder(res, run_job(jsonlite::fromJSON(req$postBody, simplifyDataFrame = FALSE)))
}

# ------------------------------------------------------------------
# Atajos por accion, para que la API se lea sola
# ------------------------------------------------------------------

#* Buscar preguntas
#* @post /questions/search
#* @serializer unboxedJSON
function(req, res) {
  spec <- jsonlite::fromJSON(req$postBody, simplifyDataFrame = FALSE)
  spec$action <- "search_questions"
  responder(res, run_job(spec))
}

#* Migrar preguntas entre stores
#* @post /questions/migrate
#* @serializer unboxedJSON
function(req, res) {
  spec <- jsonlite::fromJSON(req$postBody, simplifyDataFrame = FALSE)
  spec$action <- "migrate_questions"
  responder(res, run_job(spec))
}

#* Generar examenes
#* @post /exams/generate
#* @serializer unboxedJSON
function(req, res) {
  spec <- jsonlite::fromJSON(req$postBody, simplifyDataFrame = FALSE)
  spec$action <- "generate"
  responder(res, run_job(spec))
}

#* Corregir respuestas
#* @post /assessments/score
#* @serializer unboxedJSON
function(req, res) {
  spec <- jsonlite::fromJSON(req$postBody, simplifyDataFrame = FALSE)
  spec$action <- "score"
  responder(res, run_job(spec))
}

#* Listar corridas
#* @post /runs/list
#* @serializer unboxedJSON
function(req, res) {
  spec <- jsonlite::fromJSON(req$postBody, simplifyDataFrame = FALSE)
  spec$action <- "list_runs"
  responder(res, run_job(spec))
}

#* Reproducir una corrida
#* @post /runs/reproduce
#* @serializer unboxedJSON
function(req, res) {
  spec <- jsonlite::fromJSON(req$postBody, simplifyDataFrame = FALSE)
  spec$action <- "reproduce"
  responder(res, run_job(spec))
}
