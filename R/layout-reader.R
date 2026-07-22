# ==============================================================================
# LECTORES DE HOJA DE RESPUESTA
# ==============================================================================
# Un reader traduce lo que devuelve el escaneo a la tabla normalizada que el
# paquete sabe calificar. El `type` del reader elige la implementacion, de modo
# que cambiar de lector optico, o pasar a OMR propio, no toca nada mas.
#
# Salida normalizada, una fila por estudiante y pregunta:
#
#   student_id  Forma  Pregunta  Respuesta
#
# Es exactamente lo que espera score_responses().
# ==============================================================================

#' Leer una hoja de respuesta segun su layout
#'
#' @param layout Objeto answer_sheet_layout
#' @param source Fuente de datos: ruta al archivo del lector, o un data frame
#'   ya cargado
#' @param ... Argumentos adicionales para la implementacion
#'
#' @return Data frame normalizado con student_id, Forma, Pregunta y Respuesta
#' @export
sheet_read <- function(layout, source, ...) {
  validate_layout(layout)

  tipo <- layout$reader$type
  impl <- switch(tipo,
    excel_columns = sheet_read_excel_columns,
    stop(sprintf(
      "No hay lector implementado para el tipo '%s'. Disponibles: %s",
      tipo, paste(readers_disponibles(), collapse = ", ")
    ))
  )

  resultado <- impl(layout, source, ...)
  validate_sheet_data(layout, resultado)
}

#' Tipos de lector implementados
#'
#' @return Vector con los tipos disponibles
#' @export
readers_disponibles <- function() {
  c("excel_columns")
}

# ------------------------------------------------------------------
# Lector: columnas de un Excel
# ------------------------------------------------------------------

#' Declarar un lector de columnas de Excel
#'
#' Describe en que columna del archivo que produce el lector optico vive cada
#' campo del layout. Los valores pueden ser el nombre exacto de la columna o una
#' expresion regular, util para la grilla de respuestas.
#'
#' @param ... Pares `campo = columna`, donde `campo` es el nombre declarado en
#'   el layout
#' @param answers_pattern Expresion regular que captura las columnas de
#'   respuesta. Si se da, tiene prioridad sobre el mapeo del campo de respuestas
#'
#' @return Lista con la especificacion del lector
#' @export
#'
#' @examples
#' sheet_reader_excel(
#'   rut = "Rut-0",
#'   verificador = "verificador-0",
#'   forma = "Forma-0",
#'   respuestas = "^Resp"
#' )
sheet_reader_excel <- function(..., answers_pattern = NULL) {
  mapping <- list(...)
  list(
    type = "excel_columns",
    mapping = mapping,
    answers_pattern = answers_pattern
  )
}

#' Implementacion del lector de columnas de Excel
#' @keywords internal
sheet_read_excel_columns <- function(layout, source, ...) {

  df <- if (is.data.frame(source)) source else safe_read(source, "excel", ...)

  mapping <- layout$reader$mapping %||% list()

  # --- identificador del estudiante: se concatenan los campos en orden ---
  partes <- lapply(layout_fields(layout, "student_id"), function(f) {
    columna <- resolve_column(df, mapping[[f$name]] %||% f$name, f, obligatorio = f$required)
    if (is.null(columna)) {
      return(NULL)
    }
    apply_value_map(as.character(df[[columna]]), f$value_map)
  })
  partes <- Filter(Negate(is.null), partes)

  if (length(partes) == 0) {
    stop("El layout declara un identificador de estudiante que no se encontro en los datos.")
  }
  student_id <- do.call(paste0, partes)

  # --- forma ---
  campo_forma <- layout_fields(layout, "form")[[1]]
  col_forma <- resolve_column(df, mapping[[campo_forma$name]] %||% campo_forma$name,
                              campo_forma, obligatorio = TRUE)
  forma <- trimws(tolower(as.character(df[[col_forma]])))

  # --- respuestas ---
  campo_resp <- layout_fields(layout, "answers")[[1]]
  patron <- layout$reader$answers_pattern %||% mapping[[campo_resp$name]] %||% "^Resp"
  cols_resp <- grep(patron, colnames(df), value = TRUE)

  if (length(cols_resp) == 0) {
    stop(sprintf("Ninguna columna coincide con el patron de respuestas '%s'.", patron))
  }

  if (length(cols_resp) != layout$n_questions) {
    stop(sprintf(
      paste0("El layout declara %d preguntas pero el archivo trae %d columnas de respuesta.\n",
             "  Patron usado: %s\n",
             "  Columnas encontradas: %s"),
      layout$n_questions, length(cols_resp), patron,
      paste(head(cols_resp, 10), collapse = ", ")
    ))
  }

  respuestas <- df[, cols_resp, drop = FALSE]
  colnames(respuestas) <- seq_len(layout$n_questions)

  # --- a formato largo ---
  ancho <- tibble(student_id = student_id, Forma = forma)
  ancho <- bind_cols(ancho, respuestas)

  ancho %>%
    pivot_longer(
      cols = -all_of(c("student_id", "Forma")),
      names_to = "Pregunta",
      values_to = "Respuesta"
    ) %>%
    mutate(
      Pregunta = as.integer(.data$Pregunta),
      Respuesta = tolower(trimws(as.character(.data$Respuesta)))
    ) %>%
    arrange(.data$student_id, .data$Pregunta)
}

#' Resolver el nombre de columna de un campo
#' @keywords internal
resolve_column <- function(df, spec, field, obligatorio = TRUE) {
  if (spec %in% colnames(df)) {
    return(spec)
  }

  coincidencias <- grep(spec, colnames(df), value = TRUE)
  if (length(coincidencias) >= 1) {
    return(coincidencias[1])
  }

  if (obligatorio) {
    stop(sprintf(
      "El campo '%s' apunta a la columna '%s', que no esta en los datos.\n  Columnas disponibles: %s",
      field$name, spec, paste(colnames(df), collapse = ", ")
    ))
  }
  NULL
}

#' Aplicar el mapa de valores de un campo
#'
#' Es lo que resuelve, sin codigo especial, casos como el verificador 10 que
#' representa la K del RUT chileno.
#'
#' @keywords internal
apply_value_map <- function(x, value_map) {
  if (is.null(value_map) || length(value_map) == 0) {
    return(x)
  }
  for (desde in names(value_map)) {
    x[x == desde] <- as.character(value_map[[desde]])
  }
  x
}

# ------------------------------------------------------------------
# Validacion de lo leido
# ------------------------------------------------------------------

#' Comprobar que lo leido calza con el layout
#'
#' Avisa de respuestas fuera de las alternativas declaradas en vez de dejar que
#' se conviertan en errores silenciosos al calificar.
#'
#' @param layout Objeto answer_sheet_layout
#' @param data Data frame normalizado
#'
#' @return El data frame, invisiblemente
#' @export
validate_sheet_data <- function(layout, data) {
  requeridas <- c("student_id", "Forma", "Pregunta", "Respuesta")
  faltan <- setdiff(requeridas, names(data))
  if (length(faltan) > 0) {
    stop(sprintf("La lectura no produjo las columnas: %s",
                 paste(faltan, collapse = ", ")))
  }

  fuera <- data$Pregunta < 1 | data$Pregunta > layout$n_questions
  if (any(fuera, na.rm = TRUE)) {
    stop(sprintf("Se leyeron preguntas fuera del rango 1 a %d.", layout$n_questions))
  }

  # Respuestas que no estan entre las alternativas de su pregunta
  invalidas <- vapply(seq_len(nrow(data)), function(i) {
    r <- data$Respuesta[i]
    if (is.na(r) || !nzchar(r)) return(FALSE)     # omision, es legitima
    !(r %in% layout_choices(layout, data$Pregunta[i]))
  }, logical(1))

  if (any(invalidas)) {
    ejemplos <- unique(data$Respuesta[invalidas])
    warning(sprintf(
      paste0("%d respuesta(s) fuera de las alternativas declaradas.\n",
             "  Valores encontrados: %s\n",
             "  Se calificaran como incorrectas."),
      sum(invalidas), paste(head(ejemplos, 10), collapse = ", ")
    ), call. = FALSE)
  }

  invisible(data)
}
