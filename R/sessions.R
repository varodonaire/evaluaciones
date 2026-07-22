# ==============================================================================
# SESIONES
# ==============================================================================

#' Procesar datos de sesiones desde formato MongoDB
#' @keywords internal
process_sessions_data <- function(sessions_data) {
  
  extract_id <- function(obj) {
    if (is.character(obj)) return(obj)
    if (is.list(obj) && "$oid" %in% names(obj)) return(obj$`$oid`)
    if (is.list(obj) && length(obj) == 1) return(obj[[1]])
    NA_character_
  }
  
  extract_date <- function(obj) {
    if (is.character(obj)) return(tryCatch(as.Date(obj), error = function(e) NA))
    if (is.list(obj) && "$date" %in% names(obj)) return(as.Date(obj$`$date`))
    if (inherits(obj, c("Date", "POSIXct"))) return(as.Date(obj))
    NA
  }
  
  sessions_list <- lapply(sessions_data, function(s) {
    tibble(
      session_id = extract_id(s$`_id`),
      assessment_id = extract_id(s$assessment_id),
      class_id = extract_id(s$class_id),
      date = extract_date(s$date),
      group_name = as.character(s$group_name),
      students = list(unlist(s$students))
    )
  })
  
  bind_rows(sessions_list)
}

#' Obtener estudiantes de una sesion especifica
#'
#' @param sessions Data frame con informacion de sesiones
#' @param session_id ID de la sesion a buscar
#'
#' @return Vector con IDs de estudiantes de la sesion
#' @export
get_session_students <- function(sessions, session_id) {
  session <- sessions %>%
    filter(session_id == !!session_id) %>%
    slice(1)
  
  if (nrow(session) == 0) {
    stop(sprintf("Sesion no encontrada: %s", session_id))
  }
  
  session$students[[1]]
}

#' Listar sesiones disponibles
#'
#' @param sessions_file Ruta al archivo JSON de sesiones
#'
#' @return Data frame con sesiones (invisible)
#' @export
list_sessions <- function(sessions_file) {
  sessions_raw <- safe_read(sessions_file, "json", simplifyDataFrame = FALSE)
  sessions <- process_sessions_data(sessions_raw)
  
  message("\nSesiones disponibles:")
  message(strrep("-", 70))
  
  for (i in seq_len(nrow(sessions))) {
    s <- sessions[i, ]
    message(sprintf(
      "[%d] %s | %s | %d estudiantes | ID: %s",
      i, s$group_name, s$date, length(s$students[[1]]), s$session_id
    ))
  }
  
  message(strrep("-", 70))
  invisible(sessions)
}
