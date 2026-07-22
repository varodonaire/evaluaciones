# ==============================================================================
# CONTRATO DE HOJA DE RESPUESTA
# ==============================================================================
# Un layout describe QUE hay en la hoja que se imprimio; un reader describe COMO
# leer lo que devolvio el escaneo. Separarlos es lo que permite cambiar de
# lector sin tocar el resto del sistema.
#
# El layout es el contrato entre quien genera la hoja (a futuro, el editor web)
# y el paquete que la procesa. Se serializa a JSON, de modo que la app puede
# producirlo sin saber nada de R.
#
# Estructura:
#
#   layout_id     identificador estable
#   version       version del layout
#   n_questions   cuantas preguntas trae la hoja (NO es fijo)
#   choices       alternativas: un vector para todas, o una lista por pregunta
#   fields        que campos se marcan, con su rol semantico
#   reader        como traducir la salida del lector a respuestas
#
# Cada campo declara un ROL, que es lo que el paquete entiende:
#
#   student_id  identifica al estudiante (puede venir partido en varios campos,
#               que se concatenan en el orden declarado)
#   form        la forma o version del examen
#   answers     la grilla de respuestas
#   info        cualquier otra cosa que se quiera conservar
# ==============================================================================

ROLES_VALIDOS <- c("student_id", "form", "answers", "info")

KINDS_VALIDOS <- c("digits", "letter", "choice_grid", "text")

#' Crear un layout de hoja de respuesta
#'
#' @param n_questions Numero de preguntas de la hoja
#' @param fields Lista de campos, cada uno creado con `sheet_field()`
#' @param reader Especificacion del lector, creada con `sheet_reader_excel()`
#'   u otra implementacion
#' @param choices Alternativas por pregunta. Un vector se aplica a todas; una
#'   lista de largo `n_questions` permite que cada pregunta tenga las suyas
#' @param layout_id Identificador estable del layout
#' @param version Version del layout
#' @param assessment_id,run_id Vinculos opcionales con la evaluacion y la corrida
#'
#' @return Objeto de clase "answer_sheet_layout"
#' @export
#'
#' @examples
#' layout <- answer_sheet_layout(
#'   n_questions = 25,
#'   fields = list(
#'     sheet_field("rut", role = "student_id", kind = "digits", length = 8),
#'     sheet_field("verificador", role = "student_id", kind = "digits",
#'                 length = 1, value_map = list("10" = "K")),
#'     sheet_field("forma", role = "form", kind = "letter"),
#'     sheet_field("respuestas", role = "answers", kind = "choice_grid")
#'   ),
#'   reader = sheet_reader_excel(
#'     rut = "Rut-0", verificador = "verificador-0",
#'     forma = "Forma-0", respuestas = "^Resp"
#'   )
#' )
answer_sheet_layout <- function(n_questions,
                                fields,
                                reader,
                                choices = c("a", "b", "c", "d", "e"),
                                layout_id = NULL,
                                version = 1L,
                                assessment_id = NULL,
                                run_id = NULL) {

  n_questions <- as.integer(n_questions)
  if (is.na(n_questions) || n_questions < 1) {
    stop("n_questions debe ser un entero positivo.")
  }

  layout <- list(
    layout_id = layout_id %||% paste0("layout-", n_questions, "p-v", version),
    version = as.integer(version),
    assessment_id = assessment_id,
    run_id = run_id,
    n_questions = n_questions,
    choices = normalize_choices(choices, n_questions),
    fields = fields,
    reader = reader
  )

  class(layout) <- c("answer_sheet_layout", "list")
  validate_layout(layout)
}

#' Declarar un campo de la hoja
#'
#' @param name Nombre del campo
#' @param role Rol semantico: "student_id", "form", "answers" o "info"
#' @param kind Tipo de marca: "digits", "letter", "choice_grid" o "text"
#' @param length Numero de posiciones que ocupa. NA en una grilla de respuestas,
#'   donde el largo lo da `n_questions`
#' @param domain Valores admitidos. NULL los deduce del `kind`
#' @param value_map Reemplazos a aplicar tras leer, por ejemplo el 10 que
#'   representa la K del RUT chileno: `list("10" = "K")`
#' @param required Si el campo debe venir presente
#'
#' @return Lista con la declaracion del campo
#' @export
sheet_field <- function(name,
                        role = c("info", "student_id", "form", "answers"),
                        kind = c("digits", "letter", "choice_grid", "text"),
                        length = NA_integer_,
                        domain = NULL,
                        value_map = NULL,
                        required = TRUE) {
  role <- match.arg(role)
  kind <- match.arg(kind)

  list(
    name = name,
    role = role,
    kind = kind,
    length = as.integer(length),
    domain = domain,
    value_map = value_map,
    required = isTRUE(required)
  )
}

#' Normalizar las alternativas a una lista por pregunta
#' @keywords internal
normalize_choices <- function(choices, n_questions) {
  # jsonlite simplifica un array de arrays a una matriz de n_questions filas
  if (is.matrix(choices)) {
    choices <- lapply(seq_len(nrow(choices)), function(i) as.character(choices[i, ]))
  }

  if (is.list(choices)) {
    if (length(choices) != n_questions) {
      stop(sprintf(
        "Se dieron alternativas para %d preguntas, pero la hoja declara %d.",
        length(choices), n_questions
      ))
    }
    return(lapply(choices, as.character))
  }
  # Un vector se aplica a todas las preguntas
  replicate(n_questions, as.character(choices), simplify = FALSE)
}

#' Validar la coherencia de un layout
#'
#' @param layout Objeto answer_sheet_layout
#'
#' @return El layout, invisiblemente, si es valido
#' @export
validate_layout <- function(layout) {
  if (!inherits(layout, "answer_sheet_layout")) {
    stop("El objeto no es un answer_sheet_layout.")
  }

  if (length(layout$fields) == 0) {
    stop("El layout no declara ningun campo.")
  }

  roles <- vapply(layout$fields, function(f) f$role, character(1))
  nombres <- vapply(layout$fields, function(f) f$name, character(1))
  kinds <- vapply(layout$fields, function(f) f$kind, character(1))

  desconocidos <- setdiff(roles, ROLES_VALIDOS)
  if (length(desconocidos) > 0) {
    stop(sprintf("Roles no reconocidos: %s. Validos: %s",
                 paste(unique(desconocidos), collapse = ", "),
                 paste(ROLES_VALIDOS, collapse = ", ")))
  }

  desconocidos <- setdiff(kinds, KINDS_VALIDOS)
  if (length(desconocidos) > 0) {
    stop(sprintf("Tipos de campo no reconocidos: %s. Validos: %s",
                 paste(unique(desconocidos), collapse = ", "),
                 paste(KINDS_VALIDOS, collapse = ", ")))
  }

  if (anyDuplicated(nombres)) {
    stop(sprintf("Hay campos con nombre repetido: %s",
                 paste(unique(nombres[duplicated(nombres)]), collapse = ", ")))
  }

  for (rol in c("student_id", "form", "answers")) {
    if (!rol %in% roles) {
      stop(sprintf("El layout no declara ningun campo con rol '%s'.", rol))
    }
  }

  if (sum(roles == "answers") != 1) {
    stop("Debe haber exactamente un campo con rol 'answers'.")
  }
  if (sum(roles == "form") != 1) {
    stop("Debe haber exactamente un campo con rol 'form'.")
  }

  if (length(layout$choices) != layout$n_questions) {
    stop("Las alternativas no cubren todas las preguntas declaradas.")
  }

  if (is.null(layout$reader) || is.null(layout$reader$type)) {
    stop("El layout no declara un reader.")
  }

  invisible(layout)
}

#' Campos de un layout que cumplen un rol
#'
#' @param layout Objeto answer_sheet_layout
#' @param role Rol a buscar
#'
#' @return Lista de campos, en el orden en que fueron declarados
#' @keywords internal
layout_fields <- function(layout, role) {
  Filter(function(f) f$role == role, layout$fields)
}

#' Alternativas admitidas por una pregunta
#'
#' @param layout Objeto answer_sheet_layout
#' @param question Numero de pregunta
#'
#' @return Vector de alternativas
#' @export
layout_choices <- function(layout, question) {
  if (question < 1 || question > layout$n_questions) {
    stop(sprintf("La pregunta %d esta fuera del rango declarado (1 a %d).",
                 question, layout$n_questions))
  }
  layout$choices[[question]]
}

#' Print para layouts
#'
#' @param x Objeto answer_sheet_layout
#' @param ... Ignorado
#'
#' @return Invisiblemente el objeto
#' @export
print.answer_sheet_layout <- function(x, ...) {
  cat(strrep("=", 55), "\n", sep = "")
  cat("HOJA DE RESPUESTA\n")
  cat(strrep("=", 55), "\n", sep = "")
  cat(sprintf("  Layout:      %s (v%d)\n", x$layout_id, x$version))
  cat(sprintf("  Preguntas:   %d\n", x$n_questions))

  distintas <- unique(vapply(x$choices, paste, character(1), collapse = ""))
  if (length(distintas) == 1) {
    cat(sprintf("  Alternativas: %s\n", paste(x$choices[[1]], collapse = ", ")))
  } else {
    cat(sprintf("  Alternativas: variables (%d combinaciones)\n", length(distintas)))
  }

  cat(sprintf("  Lector:      %s\n", x$reader$type))
  cat("  Campos:\n")
  for (f in x$fields) {
    largo <- if (f$kind == "choice_grid") {
      sprintf("%d posiciones", x$n_questions)
    } else if (is.na(f$length)) {
      "largo libre"
    } else {
      sprintf("%d posicion(es)", f$length)
    }
    cat(sprintf("    - %-14s %-11s %-10s %s\n", f$name, f$role, f$kind, largo))
  }
  cat(strrep("=", 55), "\n", sep = "")
  invisible(x)
}

# ------------------------------------------------------------------
# Serializacion
# ------------------------------------------------------------------

#' Serializar un layout a JSON
#'
#' Es la forma en que el editor de pruebas entregara el contrato al paquete.
#'
#' @param layout Objeto answer_sheet_layout
#' @param file Ruta de salida. NULL devuelve la cadena
#'
#' @return La cadena JSON, invisiblemente si se escribio a un archivo
#' @export
layout_to_json <- function(layout, file = NULL) {
  validate_layout(layout)

  # na = "null" es necesario: por defecto un NA_integer_ sale como la cadena
  # "NA", que al releer no vuelve a ser un ausente
  json <- jsonlite::toJSON(unclass(layout), auto_unbox = TRUE, pretty = TRUE,
                           null = "null", na = "null")

  if (is.null(file)) {
    return(as.character(json))
  }

  writeLines(as.character(json), file, useBytes = TRUE)
  invisible(as.character(json))
}

#' Leer un layout desde JSON
#'
#' @param x Ruta a un archivo .json o una cadena JSON
#'
#' @return Objeto answer_sheet_layout
#' @export
layout_from_json <- function(x) {
  crudo <- if (length(x) == 1 && file.exists(x)) {
    jsonlite::fromJSON(paste(readLines(x, warn = FALSE, encoding = "UTF-8"),
                             collapse = "\n"), simplifyDataFrame = FALSE)
  } else {
    jsonlite::fromJSON(x, simplifyDataFrame = FALSE)
  }

  campos <- lapply(crudo$fields, function(f) {
    largo <- if (is.null(f$length) || identical(f$length, "NA")) {
      NA_integer_
    } else {
      suppressWarnings(as.integer(f$length))
    }

    sheet_field(
      name = f$name,
      role = f$role,
      kind = f$kind,
      length = largo,
      domain = f$domain,
      value_map = f$value_map,
      required = if (is.null(f$required)) TRUE else f$required
    )
  })

  answer_sheet_layout(
    n_questions = crudo$n_questions,
    fields = campos,
    reader = crudo$reader,
    choices = crudo$choices,
    layout_id = crudo$layout_id,
    version = crudo$version %||% 1L,
    assessment_id = crudo$assessment_id,
    run_id = crudo$run_id
  )
}
