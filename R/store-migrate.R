# ==============================================================================
# MIGRACION ENTRE STORES
# ==============================================================================

#' Copiar preguntas de un store a otro
#'
#' Sirve para cargar un directorio de .Rnw a MongoDB, y tambien al reves para
#' bajarse un subconjunto y trabajar sin conexion.
#'
#' Es idempotente contra el store de Mongo: si el contenido de una pregunta no
#' cambio, `qs_put()` reutiliza la version existente en vez de crear una nueva.
#' Correr la migracion dos veces no duplica nada.
#'
#' @param origen Store de origen
#' @param destino Store de destino
#' @param ids Preguntas a copiar. NULL copia todas las del origen
#' @param status Estado a asignar en el destino
#' @param verbose Mostrar el progreso
#'
#' @return Data frame con el resultado por pregunta (invisible)
#' @export
#'
#' @examples
#' \dontrun{
#' local <- question_store_local("questions")
#' mongo <- question_store_mongo()
#' qs_migrate(local, mongo)
#' }
qs_migrate <- function(origen, destino, ids = NULL,
                       status = "published", verbose = TRUE) {

  if (is.null(ids)) {
    ids <- qs_search(origen, status = NULL)$question_id
  }

  if (length(ids) == 0) {
    message("No hay preguntas que migrar.")
    return(invisible(tibble(question_id = character(0), destino_id = character(0))))
  }

  preguntas <- qs_get(origen, ids)

  if (verbose) {
    message(sprintf("Migrando %d preguntas...", nrow(preguntas)))
  }

  destino_ids <- character(nrow(preguntas))
  for (i in seq_len(nrow(preguntas))) {
    destino_ids[i] <- qs_put(
      destino,
      source = preguntas$source[i],
      slug = preguntas$slug[i],
      section = preguntas$section[i],
      tags = preguntas$tags[[i]],
      author = preguntas$author[i],
      type = preguntas$type[i],
      status = status
    )
    if (verbose) {
      message(sprintf("  [%d/%d] %s -> %s",
                      i, nrow(preguntas), preguntas$slug[i], destino_ids[i]))
    }
  }

  resultado <- tibble(
    question_id = preguntas$question_id,
    slug = preguntas$slug,
    destino_id = destino_ids
  )

  if (verbose) {
    message(sprintf("[OK] %d preguntas migradas", nrow(resultado)))
  }

  invisible(resultado)
}
