# ==============================================================================
# GENERACION DE REPORTES
# ==============================================================================

#' Obtener ruta al template incluido en el paquete
#' @keywords internal
get_template_path <- function(template_name) {
 # Buscar en inst/templates si esta instalado, o en el directorio actual
 pkg_path <- system.file("templates", template_name, package = "evaluaciones")
 
 if (pkg_path == "") {
   # Buscar en directorio local (desarrollo)
   local_path <- file.path("inst", "templates", template_name)
   if (file.exists(local_path)) {
     return(normalizePath(local_path))
   }
   stop(sprintf("Template no encontrado: %s", template_name))
 }
 
 return(pkg_path)
}

#' Asegurar un locale UTF-8 para los procesos hijos
#'
#' Devuelve una funcion que restaura el estado anterior. Si el locale actual ya
#' es UTF-8 no toca nada.
#'
#' @return Funcion de restauracion
#' @keywords internal
force_utf8_locale <- function() {
  actual <- Sys.getenv("LC_ALL", unset = "")
  if (!nzchar(actual)) {
    actual_efectivo <- Sys.getenv("LANG", unset = "")
  } else {
    actual_efectivo <- actual
  }

  if (grepl("UTF-?8", actual_efectivo, ignore.case = TRUE)) {
    return(function() invisible(NULL))
  }

  Sys.setenv(LC_ALL = "en_US.UTF-8")
  function() {
    if (nzchar(actual)) Sys.setenv(LC_ALL = actual) else Sys.unsetenv("LC_ALL")
    invisible(NULL)
  }
}

#' Renderizar un template de Quarto a PDF
#'
#' Copia el template a un directorio temporal y renderiza ahi. Renderizar en el
#' directorio del paquete deja `.tex`, `.pdf` y carpetas de apoyo dentro de la
#' instalacion, falla si esta en un sitio de solo lectura, y no soporta dos
#' informes a la vez. Ademas evita `--output-dir`, cuya interaccion con `-o` no
#' produce el PDF donde se espera.
#'
#' @param template Ruta al .qmd
#' @param params Lista nombrada de parametros para el template
#' @param output_file Ruta final del PDF
#' @param quiet Silenciar la salida de Quarto
#'
#' @return TRUE si el PDF quedo escrito
#' @keywords internal
render_quarto_report <- function(template, params, output_file, quiet = TRUE) {
  if (!file.exists(template)) {
    stop(sprintf("Template no encontrado: %s", template))
  }

  trabajo <- tempfile("quarto")
  dir.create(trabajo, recursive = TRUE)
  on.exit(unlink(trabajo, recursive = TRUE), add = TRUE)

  copia <- file.path(trabajo, basename(template))
  file.copy(template, copia, overwrite = TRUE)

  # Los parametros se pasan como rutas absolutas: quarto corre en otro directorio
  args_params <- vapply(names(params), function(n) {
    sprintf('-P %s:"%s"', n, params[[n]])
  }, character(1))

  cmd <- sprintf('quarto render "%s" --to pdf %s',
                 copia, paste(args_params, collapse = " "))

  # Los enunciados llevan tildes. Si el proceso hijo hereda un locale que no es
  # UTF-8, R no puede ni siquiera parsear los trozos de codigo del template y
  # falla con "unexpected input". Se fuerza aqui y no en cada .qmd, para que
  # valga tambien para las plantillas que se escriban despues.
  restaurar <- force_utf8_locale()
  on.exit(restaurar(), add = TRUE)

  system(cmd, ignore.stdout = quiet, ignore.stderr = quiet)

  producido <- file.path(trabajo, sub("\\.qmd$", ".pdf", basename(template)))
  if (!file.exists(producido)) {
    return(FALSE)
  }

  destino <- dirname(output_file)
  if (!dir.exists(destino)) {
    dir.create(destino, recursive = TRUE)
  }
  file.copy(producido, output_file, overwrite = TRUE)

  file.exists(output_file)
}

#' Preparar metadatos en formato compatible con templates
#'
#' Esta funcion prepara los datos en el formato exacto que esperan los templates
#' Reporte.qmd y ReporteDocente.qmd, que incluye:
#' - results: respuestas calificadas con is_correcta
#' - students: informacion de estudiantes
#' - results_general: puntajes por estudiante
#' - ejes: definicion de ejes tematicos
#' - questions_extra: preguntas de una forma de referencia
#' - sessions: informacion de sesiones
#'
#' @param scored_data Data frame con respuestas calificadas
#' @param students_df Data frame con informacion de estudiantes
#' @param items_df Data frame con items/preguntas
#' @param ejes_df Data frame con definicion de ejes (identificador, nombre)
#' @param sessions_df Data frame con sesiones (opcional)
#' @param reference_form Forma de referencia para questions_extra (default: ultima forma)
#' @param id_col Columna de identificacion de estudiantes. NULL autodetecta entre
#'   "student_id" y "Rut"; en ambos casos se renombra a "student_id", que es lo
#'   que esperan los templates
#'
#' @return Lista con estructura compatible con templates
#' @export
prepare_report_metadata <- function(scored_data,
                                     students_df,
                                     items_df = NULL,
                                     ejes_df = NULL,
                                     sessions_df = NULL,
                                     reference_form = NULL,
                                     id_col = NULL) {

  # Los templates esperan student_id: renombrar la columna identificadora
  id_col <- resolve_id_col(scored_data, id_col)
  if (id_col != "student_id") {
    scored_data <- scored_data %>% rename(student_id = !!sym(id_col))
  }

  scored_data <- normalize_correct_col(scored_data)

  # Calcular results_general (puntajes por estudiante)
  results_general <- scored_data %>%
    group_by(student_id) %>%
    summarise(
      n = n(),
      correctas = sum(is_correcta, na.rm = TRUE),
      porcentaje = 100 * correctas / n,
      .groups = "drop"
    )
  
  # Asegurar que students_df tenga columna student_id. La tabla de estudiantes
  # puede identificar con otro nombre que los datos calificados: es lo normal
  # cuando estos ya vienen renombrados desde el sink.
  if (!is.null(students_df)) {
    if (!"student_id" %in% colnames(students_df)) {
      col_estudiantes <- tryCatch(resolve_id_col(students_df),
                                  error = function(e) NULL)
      if (is.null(col_estudiantes)) {
        stop(sprintf(
          "La tabla de estudiantes no tiene columna identificadora. Columnas: %s",
          paste(colnames(students_df), collapse = ", ")
        ))
      }
      students_df <- students_df %>% rename(student_id = !!sym(col_estudiantes))
    }
    # Asegurar que tenga columna name
    if (!"name" %in% colnames(students_df)) {
      name_cols <- intersect(c("nombre", "Name", "Nombre"), colnames(students_df))
      if (length(name_cols) > 0) {
        students_df <- students_df %>% rename(name = !!sym(name_cols[1]))
      }
    }
  }
  
  # Crear ejes_df si no se proporciona (inferir de scored_data)
  if (is.null(ejes_df)) {
    unique_ejes <- unique(scored_data$Eje)
    ejes_df <- tibble(
      identificador = unique_ejes,
      nombre = unique_ejes  # Usar mismo valor como nombre
    )
    message("Ejes inferidos de datos. Para nombres descriptivos, proporciona ejes_df.")
  }
  
  # Preparar questions_extra (preguntas de forma de referencia)
  if (is.null(items_df)) {
    # Intentar extraer de scored_data
    if (is.null(reference_form)) {
      reference_form <- max(scored_data$Forma, na.rm = TRUE)
    }
    questions_extra <- scored_data %>%
      filter(Forma == reference_form) %>%
      select(any_of(c("Origen", "Eje", "Texto", "Alternativas", "Correcta", "Pregunta"))) %>%
      distinct(Origen, .keep_all = TRUE)
  } else {
    if (is.null(reference_form)) {
      reference_form <- max(items_df$Forma, na.rm = TRUE)
    }
    questions_extra <- items_df %>%
      filter(Forma == reference_form) %>%
      select(any_of(c("Origen", "Eje", "Texto", "Alternativas", "Correcta", "Pregunta")))
  }
  
  # Construir lista de metadatos
  metadata <- list(
    results = scored_data,
    students = students_df,
    results_general = results_general,
    ejes = ejes_df,
    questions_extra = questions_extra,
    sessions = sessions_df,
    timestamp = Sys.time()
  )
  
  return(metadata)
}

#' Guardar metadatos para reportes
#'
#' @param metadata Lista de metadatos (de prepare_report_metadata)
#' @param file Ruta del archivo .rds de salida
#'
#' @return Ruta del archivo guardado (invisible)
#' @export
save_report_metadata <- function(metadata, file = ".__session_cache.rds") {
  saveRDS(metadata, file)
  message(sprintf("[OK] Metadatos guardados en: %s", file))
  invisible(file)
}

#' Generar reporte individual de estudiante
#'
#' @param student_id ID del estudiante
#' @param session_id ID de la sesion
#' @param metadata Lista de metadatos (de prepare_report_metadata) o ruta a archivo .rds
#' @param output_dir Directorio de salida para el PDF
#' @param template Ruta al template Quarto (usa Reporte.qmd por defecto)
#' @param working_dir Directorio de trabajo para Quarto (donde estan las fuentes)
#'
#' @return Lista con status y ruta del archivo generado
#' @export
#'
#' @examples
#' \dontrun{
#' # Preparar metadatos una vez
#' metadata <- prepare_report_metadata(scored_data, students, items, ejes)
#'
#' # Generar reporte para un estudiante
#' generate_student_report("12345678-9", "session123", metadata)
#' }
generate_student_report <- function(student_id,
                                     session_id,
                                     metadata,
                                     output_dir = "reportes",
                                     template = NULL,
                                     working_dir = NULL) {
  
  # Si metadata es una ruta, cargar el archivo
  if (is.character(metadata) && length(metadata) == 1) {
    if (!file.exists(metadata)) {
      stop(sprintf("Archivo de metadatos no encontrado: %s", metadata))
    }
    data_file <- normalizePath(metadata)
  } else {
    # Guardar metadatos en archivo temporal
    data_file <- tempfile(fileext = ".rds")
    saveRDS(metadata, data_file)
  }
  
  # Crear directorio de salida
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  output_dir <- normalizePath(output_dir)
  
  # Obtener template
  if (is.null(template)) {
    template <- get_template_path("Reporte.qmd")
  }
  
  # Archivo de salida
  safe_id <- gsub("[^a-zA-Z0-9]", "_", student_id)
  output_file <- file.path(output_dir, sprintf("%s.pdf", safe_id))
  
  file_exists <- render_quarto_report(
    template,
    params = list(student_id = student_id, session_id = session_id,
                  data_file = data_file),
    output_file = output_file
  )

  # Limpiar archivo temporal si fue creado
  if (!is.character(metadata)) {
    unlink(data_file)
  }

  result <- list(
    student_id = student_id,
    status = if (file_exists) "success" else "failed",
    message = if (file_exists) "Reporte generado exitosamente" else
      "Error al generar reporte: Quarto no produjo el PDF",
    output_file = if (file_exists) output_file else NA_character_
  )
  
  if (file_exists) {
    message(sprintf("[OK] Reporte generado: %s", basename(output_file)))
  } else {
    warning(sprintf("[X] Error generando reporte para %s", student_id))
  }
  
  return(result)
}

#' Generar reportes para multiples estudiantes
#'
#' @param student_ids Vector de IDs de estudiantes
#' @param session_id ID de la sesion
#' @param metadata Lista de metadatos o ruta a archivo .rds
#' @param output_dir Directorio de salida
#' @param template Ruta al template Quarto
#' @param working_dir Directorio de trabajo para Quarto
#'
#' @return Data frame con status de cada reporte
#' @export
generate_student_reports <- function(student_ids,
                                      session_id,
                                      metadata,
                                      output_dir = "reportes",
                                      template = NULL,
                                      working_dir = NULL) {
  
  message(sprintf("Generando reportes para %d estudiantes...", length(student_ids)))
  
  # Si metadata es objeto, guardarlo una vez y reutilizar
  if (!is.character(metadata)) {
    data_file <- tempfile(fileext = ".rds")
    saveRDS(metadata, data_file)
    metadata <- data_file
    on.exit(unlink(data_file))
  }
  
  results <- vector("list", length(student_ids))
  
  for (i in seq_along(student_ids)) {
    message(sprintf("  [%d/%d] %s", i, length(student_ids), student_ids[i]))
    
    results[[i]] <- generate_student_report(
      student_id = student_ids[i],
      session_id = session_id,
      metadata = metadata,
      output_dir = output_dir,
      template = template,
      working_dir = working_dir
    )
  }
  
  # Combinar resultados
  results_df <- bind_rows(results)
  
  success_count <- sum(results_df$status == "success")
  message(sprintf("\n[OK] Reportes generados: %d/%d exitosos", success_count, length(student_ids)))
  
  return(results_df)
}

#' Generar reporte de sesion para docente
#'
#' @param session_id ID de la sesion
#' @param metadata Lista de metadatos (de prepare_report_metadata) o ruta a archivo .rds
#' @param output_dir Directorio de salida para el PDF
#' @param output_name Nombre del archivo de salida (sin extension)
#' @param template Ruta al template Quarto (usa ReporteDocente.qmd por defecto)
#' @param working_dir Directorio de trabajo para Quarto
#'
#' @return Lista con status y ruta del archivo generado
#' @export
#'
#' @examples
#' \dontrun{
#' metadata <- prepare_report_metadata(scored_data, students, items, ejes, sessions)
#' generate_session_report("session123", metadata)
#' }
generate_session_report <- function(session_id,
                                     metadata,
                                     output_dir = "reportes",
                                     output_name = NULL,
                                     template = NULL,
                                     working_dir = NULL) {
  
  message("Generando reporte de sesion...")
  
  # Si metadata es una ruta, cargar el archivo
  if (is.character(metadata) && length(metadata) == 1) {
    if (!file.exists(metadata)) {
      stop(sprintf("Archivo de metadatos no encontrado: %s", metadata))
    }
    data_file <- normalizePath(metadata)
  } else {
    # Guardar metadatos en archivo temporal
    data_file <- tempfile(fileext = ".rds")
    saveRDS(metadata, data_file)
  }
  
  # Crear directorio de salida
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  output_dir <- normalizePath(output_dir)
  
  # Obtener template
  if (is.null(template)) {
    template <- get_template_path("ReporteDocente.qmd")
  }
  
  # Nombre de archivo de salida
  if (is.null(output_name)) {
    output_name <- sprintf("ReporteDocente_%s", session_id)
  }
  output_file <- file.path(output_dir, sprintf("%s.pdf", output_name))
  
  file_exists <- render_quarto_report(
    template,
    params = list(session_id = session_id, data_file = data_file),
    output_file = output_file
  )

  # Limpiar archivo temporal si fue creado
  if (!is.character(metadata)) {
    unlink(data_file)
  }

  result <- list(
    session_id = session_id,
    status = if (file_exists) "success" else "failed",
    message = if (file_exists) "Reporte de sesion generado exitosamente" else
      "Error al generar reporte: Quarto no produjo el PDF",
    output_file = if (file_exists) output_file else NA_character_
  )
  
  if (file_exists) {
    message(sprintf("[OK] Reporte de sesion generado: %s", basename(output_file)))
  } else {
    warning("[X] Error generando reporte de sesion")
  }
  
  return(result)
}

#' Generar todos los reportes de una evaluacion
#'
#' @param metadata Lista de metadatos o ruta a archivo .rds
#' @param session_id ID de la sesion a procesar
#' @param output_dir Directorio base de salida
#' @param generate_individual Generar reportes individuales de estudiantes
#' @param generate_session Generar reporte de sesion para docente
#' @param generate_psychometric Generar reporte psicometrico
#' @param working_dir Directorio de trabajo para Quarto
#'
#' @return Lista con resultados de generacion
#' @export
generate_all_reports <- function(metadata,
                                  session_id,
                                  output_dir = "reportes",
                                  generate_individual = TRUE,
                                  generate_session = TRUE,
                                  generate_psychometric = FALSE,
                                  working_dir = NULL) {
  
  message("\n", strrep("=", 60))
  message("GENERACION DE REPORTES")
  message(strrep("=", 60), "\n")
  
  # Cargar metadatos si es ruta
  if (is.character(metadata) && length(metadata) == 1) {
    md <- readRDS(metadata)
    data_file <- normalizePath(metadata)
  } else {
    md <- metadata
    # Guardar una vez para reutilizar
    data_file <- tempfile(fileext = ".rds")
    saveRDS(metadata, data_file)
  }
  
  results <- list()
  
  # Obtener estudiantes de la sesion
  if (!is.null(md$sessions)) {
    session_row <- md$sessions %>%
      filter(session_id == !!session_id) %>%
      slice(1)
    
    if (nrow(session_row) > 0) {
      student_ids <- session_row$students[[1]]
    } else {
      student_ids <- unique(md$results$student_id)
    }
  } else {
    student_ids <- unique(md$results$student_id)
  }
  
  # Reporte de sesion
  if (generate_session) {
    session_result <- generate_session_report(
      session_id = session_id,
      metadata = data_file,
      output_dir = output_dir,
      working_dir = working_dir
    )
    results$session_report <- session_result
  }
  
  # Reportes individuales
  if (generate_individual) {
    individual_results <- generate_student_reports(
      student_ids = student_ids,
      session_id = session_id,
      metadata = data_file,
      output_dir = output_dir,
      working_dir = working_dir
    )
    results$individual_reports <- individual_results
  }
  
  # Reporte psicometrico
  if (generate_psychometric) {
    psychometric_result <- generate_operator_report(
      session_id = session_id,
      metadata = data_file,
      output_dir = output_dir,
      working_dir = working_dir
    )
    results$psychometric_report <- psychometric_result
  }
  
  # Limpiar si fue temporal
  if (!is.character(metadata)) {
    unlink(data_file)
  }
  
  message("\n", strrep("=", 60))
  message("[OK] GENERACION DE REPORTES COMPLETA")
  message(strrep("=", 60), "\n")
  
  invisible(results)
}

#' Generar reporte psicometrico para operador
#'
#' @param session_id ID de la sesion (puede ser NA para analisis global)
#' @param metadata Lista de metadatos o ruta a archivo .rds
#' @param output_dir Directorio de salida para el PDF
#' @param output_name Nombre del archivo de salida (sin extension)
#' @param template Ruta al template Quarto (usa ReporteOperador.qmd por defecto)
#' @param working_dir Directorio de trabajo para Quarto
#' @param id_col Columna de identificacion de estudiante. NULL autodetecta
#'   entre "student_id" y "Rut"
#'
#' @return Lista con status y ruta del archivo generado
#' @export
generate_operator_report <- function(session_id = NULL,
                                      metadata,
                                      output_dir = "reportes",
                                      output_name = NULL,
                                      template = NULL,
                                      working_dir = NULL,
                                      id_col = NULL) {
  
  message("Generando reporte psicometrico para operador...")
  
  # Cargar metadatos si es ruta
  if (is.character(metadata) && length(metadata) == 1) {
    if (!file.exists(metadata)) {
      stop(sprintf("Archivo de metadatos no encontrado: %s", metadata))
    }
    md <- readRDS(metadata)
  } else {
    md <- metadata
  }
  
  # Filtrar por sesion si se especifica
  scored_data <- md$results
  if (!is.null(session_id) && !is.null(md$sessions)) {
    session_row <- md$sessions %>%
      filter(session_id == !!session_id) %>%
      slice(1)
    
    if (nrow(session_row) > 0) {
      session_students <- session_row$students[[1]]
      scored_data <- scored_data %>%
        filter(student_id %in% session_students)
    }
  }
  
  # Ejecutar analisis psicometrico
  psychometric <- run_psychometric_analysis(scored_data, id_col = id_col)
  
  # Agregar analisis al metadata
  md$psychometric <- psychometric
  
  # Guardar metadata aumentado
  data_file <- tempfile(fileext = ".rds")
  saveRDS(md, data_file)
  
  # Crear directorio de salida
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  output_dir <- normalizePath(output_dir)
  
  # Obtener template
  if (is.null(template)) {
    template <- get_template_path("ReporteOperador.qmd")
  }
  
  # Nombre de archivo
  if (is.null(output_name)) {
    if (!is.null(session_id)) {
      output_name <- sprintf("ReportePsicometrico_%s", session_id)
    } else {
      output_name <- sprintf("ReportePsicometrico_%s", format(Sys.Date(), "%Y%m%d"))
    }
  }
  output_file <- file.path(output_dir, sprintf("%s.pdf", output_name))
  
  session_param <- if (!is.null(session_id)) session_id else "ALL"

  file_exists <- render_quarto_report(
    template,
    params = list(session_id = session_param, data_file = data_file),
    output_file = output_file
  )

  unlink(data_file)

  result <- list(
    status = if (file_exists) "success" else "failed",
    message = if (file_exists) "Reporte psicometrico generado exitosamente" else
      "Error al generar reporte: Quarto no produjo el PDF",
    output_file = if (file_exists) output_file else NA_character_,
    psychometric_summary = list(
      kr20 = psychometric$reliability$kr20,
      n_items = psychometric$reliability$n_items,
      n_problematic = psychometric$problematic_items$n_problematic,
      forms_equivalent = psychometric$form_comparison$forms_equivalent
    )
  )
  
  if (file_exists) {
    message(sprintf("[OK] Reporte psicometrico generado: %s", basename(output_file)))
    message(sprintf("  KR-20: %.3f | Items problematicos: %d | Formas equivalentes: %s",
                    psychometric$reliability$kr20,
                    psychometric$problematic_items$n_problematic,
                    ifelse(psychometric$form_comparison$forms_equivalent, "Si", "No")))
  } else {
    warning("[X] Error generando reporte psicometrico")
  }
  
  return(result)
}
