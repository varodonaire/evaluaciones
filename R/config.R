# ==============================================================================
# CONFIGURACION: examen y procesamiento
# ==============================================================================

#' Crear configuracion de examen
#'
#' @param name Nombre identificador del examen (ej: "algebra_b4_p1")
#' @param year Anio de la evaluacion
#' @param period Periodo/bimestre (ej: 1, 2, 3, 4 o "semestre1")
#' @param subject Asignatura (ej: "Matematicas", "Fisica")
#' @param num_forms Numero de formas/versiones del examen (default: 10)
#' @param groups Lista con configuracion de grupos: list(sizes = c(4,4,2), seeds = c(13,17,57))
#' @param template Template LaTeX: ruta a un .tex propio, o el nombre de un
#'   template de 'exams' (default: "plain")
#' @param usepackage Paquetes LaTeX adicionales a cargar. Por defecto siunitx,
#'   amsmath y csquotes, que cubren \\qty, \\num, align* y \\enquote
#' @param header Lista de campos a pasar al template LaTeX. NULL (default)
#'   significa automatico: se pasa `forma` con la letra de cada version solo si
#'   se usa un template propio, porque los templates de 'exams' no definen
#'   \\forma y fallarian al compilar
#' @param output_dir Directorio de salida para PDFs
#' @param tex_engine Motor LaTeX a usar ("xelatex", "pdflatex", etc.)
#'
#' @return Objeto de clase "exam_config"
#' @export
#'
#' @examples
#' config <- create_exam_config(
#'   name = "prueba1_algebra",
#'   year = 2025,
#'   period = 4,
#'   subject = "Algebra",
#'   num_forms = 10
#' )
create_exam_config <- function(name,
                                year,
                                period,
                                subject,
                                num_forms = 10,
                                groups = NULL,
                                template = "plain",
                                usepackage = c("siunitx", "amsmath", "csquotes"),
                                header = NULL,
                                output_dir = "exams",
                                tex_engine = "xelatex") {
  

  # Valores por defecto para grupos si no se especifican

  if (is.null(groups)) {
    groups <- list(
      sizes = c(num_forms),
      seeds = c(sample(1:100, 1))
    )
  }
  
  # Validar que sizes sumen num_forms
  if (sum(groups$sizes) != num_forms) {
    stop(sprintf(
      "La suma de group sizes (%d) debe ser igual a num_forms (%d)",
      sum(groups$sizes), num_forms
    ))
  }

  # Cada grupo necesita su propia seed: las formas del grupo comparten examen
  if (length(groups$sizes) != length(groups$seeds)) {
    stop(sprintf(
      "Debe haber una seed por grupo: %d sizes vs %d seeds",
      length(groups$sizes), length(groups$seeds)
    ))
  }


  config <- list(
    name = name,
    year = as.integer(year),
    period = period,
    subject = subject,
    num_forms = as.integer(num_forms),
    groups = groups,
    template = template,
    usepackage = usepackage,
    header = header,
    output_dir = output_dir,
    tex_engine = tex_engine,
    form_labels = LETTERS[1:num_forms]
  )
  
  class(config) <- c("exam_config", "list")
  return(config)
}

#' Print method for exam_config objects
#'
#' @param x An exam_config object
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns the input object
#' @export
print.exam_config <- function(x, ...) {
  cat(strrep("=", 55), "\n", sep = "")
  cat("CONFIGURACION DE EXAMEN\n")
  cat(strrep("=", 55), "\n", sep = "")
  cat(sprintf("  Nombre:      %s\n", x$name))
  cat(sprintf("  Asignatura:  %s\n", x$subject))
  cat(sprintf("  Anio:        %d\n", x$year))
  cat(sprintf("  Periodo:     %s\n", x$period))
  cat(sprintf("  Formas:      %d (%s)\n", x$num_forms, paste(x$form_labels, collapse = ", ")))
  cat(sprintf("  Grupos:      %d\n", length(x$groups$sizes)))
  cat(sprintf("  Output:      %s\n", x$output_dir))
  cat(strrep("=", 55), "\n", sep = "")
  invisible(x)
}

#' Crear configuracion para procesamiento de resultados
#'
#' @param results_file Ruta al archivo Excel con respuestas de estudiantes
#' @param items_file Ruta al archivo JSON con items/claves (generado por export_items_json)
#' @param students_file Ruta al archivo CSV con datos de estudiantes
#' @param sessions_file Ruta al archivo JSON con sesiones (opcional)
#' @param session_id ID de la sesion a procesar (opcional, si no se usa sessions_file)
#' @param num_questions Numero de preguntas en la prueba
#' @param output_dir Directorio para archivos de salida
#' @param form_mapping Mapeo de letras a numeros para formas
#' @param generate_reports Generar reportes PDF (requiere Quarto)
#' @param id_col Nombre de la columna identificadora de estudiante en los
#'   resultados (default: "Rut")
#' @param layout Objeto answer_sheet_layout que describe la hoja aplicada. Si se
#'   da, la lectura se dirige por el contrato en vez de adivinar por convencion,
#'   y `num_questions` se toma de el
#'
#' @return Objeto de clase "processing_config"
#' @export
create_processing_config <- function(results_file,
                                      items_file,
                                      students_file,
                                      sessions_file = NULL,
                                      session_id = NULL,
                                      num_questions = 25,
                                      output_dir = "reports",
                                      form_mapping = NULL,
                                      generate_reports = FALSE,
                                      id_col = "Rut",
                                      layout = NULL) {

  if (!is.null(layout)) {
    validate_layout(layout)
    num_questions <- layout$n_questions
  }
  
  # Mapeo por defecto de formas
  if (is.null(form_mapping)) {
    form_mapping <- setNames(1:10, letters[1:10])
  }
  
  config <- list(
    files = list(
      results = results_file,
      items = items_file,
      students = students_file,
      sessions = sessions_file
    ),
    session_id = session_id,
    num_questions = num_questions,
    output_dir = output_dir,
    form_mapping = form_mapping,
    generate_reports = generate_reports,
    id_col = id_col,
    layout = layout
  )
  
  class(config) <- c("processing_config", "list")
  return(config)
}

#' Print method for processing_config objects
#'
#' @param x A processing_config object
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns the input object
#' @export
print.processing_config <- function(x, ...) {
  cat(strrep("=", 55), "\n", sep = "")
  cat("CONFIGURACION DE PROCESAMIENTO\n")
  cat(strrep("=", 55), "\n", sep = "")
  cat(sprintf("  Resultados:  %s\n", x$files$results))
  cat(sprintf("  Items:       %s\n", x$files$items))
  cat(sprintf("  Estudiantes: %s\n", x$files$students))
  cat(sprintf("  Sesiones:    %s\n", ifelse(is.null(x$files$sessions), "(no)", x$files$sessions)))
  cat(sprintf("  Session ID:  %s\n", ifelse(is.null(x$session_id), "(todas)", x$session_id)))
  cat(sprintf("  Preguntas:   %d\n", x$num_questions))
  cat(sprintf("  ID columna:  %s\n", x$id_col))
  cat(sprintf("  Layout:      %s\n",
              if (is.null(x$layout)) "(por convencion)" else x$layout$layout_id))
  cat(sprintf("  Output:      %s\n", x$output_dir))
  cat(sprintf("  Reportes:    %s\n", ifelse(x$generate_reports, "Si", "No")))
  cat(strrep("=", 55), "\n", sep = "")
  invisible(x)
}
