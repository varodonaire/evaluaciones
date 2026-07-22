# ==============================================================================
# LECTURA Y TRANSFORMACION DE DATOS DE RESULTADOS
# ==============================================================================

#' Leer archivo de forma segura con manejo de errores
#' @keywords internal
safe_read <- function(path, type = c("excel", "csv", "json"), ...) {
  type <- match.arg(type)
  
  if (!file.exists(path)) {
    stop(sprintf("Archivo no encontrado: %s", path))
  }
  
  tryCatch({
    switch(type,
      excel = readxl::read_excel(path, ...),
      csv = readr::read_csv(path, show_col_types = FALSE, 
                            locale = locale(encoding = "UTF-8"), ...),
      json = {
        json_text <- readLines(path, encoding = "UTF-8", warn = FALSE)
        jsonlite::fromJSON(paste(json_text, collapse = "\n"), ...)
      }
    )
  }, error = function(e) {
    stop(sprintf("Error leyendo %s: %s", path, e$message))
  })
}

#' Cargar todos los datos necesarios para el procesamiento
#'
#' @param config Objeto processing_config
#'
#' @return Lista con todos los datos cargados
#' @export
load_processing_data <- function(config) {
  message("Cargando datos...")
  
  data <- list(
    results = safe_read(config$files$results, "excel"),
    items = safe_read(config$files$items, "json"),
    students = safe_read(config$files$students, "csv")
  )
  
  if (!is.null(config$files$sessions)) {
    sessions_raw <- safe_read(config$files$sessions, "json", simplifyDataFrame = FALSE)
    data$sessions <- process_sessions_data(sessions_raw)
  }
  
  message(sprintf("[OK] Datos cargados: %d respuestas, %d items, %d estudiantes",
                  nrow(data$results), length(data$items), nrow(data$students)))
  
  return(data)
}

#' Normalizar columnas de RUT chileno
#' @keywords internal
normalize_rut_columns <- function(df) {
  
  # Buscar columnas de RUT
  rut_cols <- grep("^Rut-|^verificador-", colnames(df), value = TRUE)
  
  if (length(rut_cols) == 0) {
    # Si no hay columnas separadas, buscar columna Rut directamente
    if ("Rut" %in% colnames(df)) {
      return(df)
    }
    stop("No se encontraron columnas de RUT en los datos")
  }
  
  # Reemplazar 10 con K en verificador
  if ("verificador-0" %in% colnames(df)) {
    df <- df %>%
      mutate(`verificador-0` = ifelse(`verificador-0` == 10, "K", as.character(`verificador-0`)))
  }
  
  # Unir columnas de RUT
  df %>%
    unite("Rut", all_of(rut_cols), sep = "", remove = TRUE)
}

#' Estandarizar nombres de columnas de respuestas
#' @keywords internal
standardize_response_columns <- function(df, num_questions) {
  
  # Eliminar columnas completamente vacias
  df <- df %>%
    select(where(~ !all(is.na(.))))
  
  # Renombrar Forma-0 a Forma si existe
  if ("Forma-0" %in% colnames(df)) {
    df <- df %>% rename(Forma = `Forma-0`)
  }
  
  # Identificar columnas de respuesta y renombrarlas
  # Buscar patrones comunes: Resp*, Respuestas-*, etc.
  resp_cols <- grep("^Resp|^respuesta", colnames(df), value = TRUE, ignore.case = TRUE)
  
  if (length(resp_cols) > 0) {
    # Renombrar a formato estandar Respuestas-1, Respuestas-2, etc.
    new_names <- paste0("Respuestas-", seq_along(resp_cols))
    names(df)[match(resp_cols, names(df))] <- new_names
  }
  
  df
}

#' Convertir formas de letras a numeros
#'
#' Acepta formas codificadas como letra ("a", "B") o como numero ("3", 3). Las
#' filas cuya forma no se puede resolver se descartan, informando cuantas y con
#' que valores, porque un mapeo equivocado vaciaba la tabla sin dejar rastro.
#'
#' @keywords internal
convert_form_codes <- function(df, form_mapping) {

  raw_forma <- tolower(trimws(as.character(df$Forma)))

  # Mapeo por letra; si no aplica, intentar interpretar como numero
  mapped <- unname(form_mapping[raw_forma])
  numeric_fallback <- suppressWarnings(as.integer(raw_forma))
  mapped <- ifelse(is.na(mapped), numeric_fallback, mapped)

  # Solo son validas las formas dentro del rango del mapeo
  valid_codes <- unname(form_mapping)
  mapped[!is.na(mapped) & !mapped %in% valid_codes] <- NA_integer_

  n_dropped <- sum(is.na(mapped))
  if (n_dropped > 0) {
    bad_values <- unique(raw_forma[is.na(mapped)])
    bad_values[is.na(bad_values)] <- "(vacio)"
    detalle <- sprintf(
      paste0("  Valores no reconocidos: %s\n",
             "  Formas esperadas: %s\n",
             "  Revisa el argumento form_mapping de create_processing_config()."),
      paste(head(bad_values, 10), collapse = ", "),
      paste(names(form_mapping), collapse = ", ")
    )

    if (n_dropped == nrow(df)) {
      stop(sprintf(
        "convert_form_codes: ninguna de las %d filas tiene una forma reconocible.\n%s",
        nrow(df), detalle
      ), call. = FALSE)
    }

    warning(sprintf(
      "convert_form_codes: %d de %d filas descartadas por forma no reconocida.\n%s",
      n_dropped, nrow(df), detalle
    ), call. = FALSE)
  }

  df %>%
    mutate(Forma = mapped) %>%
    filter(!is.na(Forma))
}

#' Transformar datos de formato ancho a largo
#' @keywords internal
pivot_responses_long <- function(df) {
  df %>%
    pivot_longer(
      cols = matches("^Respuestas-\\d+$"),
      names_to = "Pregunta",
      values_to = "Respuesta"
    ) %>%
    mutate(
      Pregunta = as.integer(str_remove(Pregunta, "Respuestas-")),
      Respuesta = tolower(trimws(as.character(Respuesta)))
    )
}
