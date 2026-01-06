# ==============================================================================
# MODERN ASSESSMENT RESULTS PROCESSOR
# ==============================================================================
# Purpose: Process student assessment results, calculate scores, and generate reports
# Author: Refactored from Revisador2.R
# Date: November 7, 2025
# ==============================================================================

library(tidyverse)
library(jsonlite)
library(readxl)
library(writexl)

# Set locale to UTF-8 for proper character encoding
Sys.setlocale("LC_ALL", "es_MX.UTF-8")

# ==============================================================================
# CONFIGURATION
# ==============================================================================

CONFIG <- list(
  # Directory paths
  data_dir = "udddd",
  output_dir = "reports_p2_b4",
  cache_file = ".__session_cache2.rds",
  
  # Session configuration
  session_id = "692ce3f5a602af67a5ca9289",  # Session to process
  
  # File paths
  files = list(
    results = "udddd/27-11.xlsx",
    questions = "udddd/b4_p2_items.json",
    students = "udddd/Students.csv",
    ejes = "udddd/Ejes.json",
    sessions = "udddd/AssessmentSessions.json"
  ),
  
  # Output files
  output_files = list(
    student_scores = "puntajes_grupo1_P2_B4.xlsx",
    question_performance = "preguntas_grupo1_P2_B4.xlsx"
  ),
  
  # Report generation options
  generate_student_reports = TRUE,  # Generate individual student reports
  generate_teacher_report = TRUE,    # Generate teacher summary report
  
  # Constants
  num_questions = 25,
  form_letters = setNames(1:10, letters[1:10])
)

# ==============================================================================
# UTILITY FUNCTIONS
# ==============================================================================

#' Safely read an Excel file with error handling
#' @param path File path to read
#' @return Data frame or stops with error
safe_read_excel <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf("File not found: %s", path))
  }
  tryCatch(
    read_excel(path),
    error = function(e) {
      stop(sprintf("Error reading Excel file %s: %s", path, e$message))
    }
  )
}

#' Safely read a CSV file with error handling
#' @param path File path to read
#' @return Data frame or stops with error
safe_read_csv <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf("File not found: %s", path))
  }
  tryCatch(
    read_csv(path, show_col_types = FALSE, locale = locale(encoding = "UTF-8")),
    error = function(e) {
      stop(sprintf("Error reading CSV file %s: %s", path, e$message))
    }
  )
}

#' Safely read a JSON file with error handling
#' @param path File path to read
#' @return Parsed JSON object or stops with error
safe_read_json <- function(path, simplify = TRUE) {
  if (!file.exists(path)) {
    stop(sprintf("File not found: %s", path))
  }
  tryCatch({
    # Read with explicit UTF-8 encoding
    json_text <- readLines(path, encoding = "UTF-8", warn = FALSE)
    fromJSON(paste(json_text, collapse = "\n"), simplifyDataFrame = simplify)
  },
    error = function(e) {
      stop(sprintf("Error reading JSON file %s: %s", path, e$message))
    }
  )
}

#' Normalize text columns (lowercase and trim whitespace)
#' @param x Character vector
#' @return Normalized character vector
normalize_text <- function(x) {
  tolower(trimws(as.character(x)))
}

# ==============================================================================
# DATA LOADING
# ==============================================================================

#' Load all required data files
#' @param config Configuration list
#' @return List containing all loaded data
load_all_data <- function(config = CONFIG) {
  message("Loading data files...")
  
  data <- list(
    results = safe_read_excel(config$files$results),
    questions = safe_read_json(config$files$questions),
    students = safe_read_csv(config$files$students),
    ejes = safe_read_json(config$files$ejes),
    # Load sessions without automatic simplification to preserve list structure
    sessions_raw = safe_read_json(config$files$sessions, simplify = FALSE)
  )
  
  # Process sessions to flatten MongoDB structure
  data$sessions <- process_sessions_data(data$sessions_raw)
  data$sessions_raw <- NULL  # Remove raw version
  
  message(sprintf("✓ Loaded %d data files successfully", length(data)))
  return(data)
}

# ==============================================================================
# DATA TRANSFORMATION - SESSION PROCESSING
# ==============================================================================

#' Process sessions data from MongoDB export format
#' @param sessions_data Raw sessions data from JSON (list format)
#' @return Processed sessions data frame with flattened structure
process_sessions_data <- function(sessions_data) {
  message("Processing sessions data...")
  
  # Helper to safely extract MongoDB ID field
  extract_id <- function(obj) {
    if (is.character(obj)) {
      return(obj)
    }
    if (is.list(obj)) {
      if ("$oid" %in% names(obj)) {
        return(obj$`$oid`)
      }
      # Sometimes it's a single-element unnamed list
      if (length(obj) == 1) {
        return(obj[[1]])
      }
    }
    return(NA_character_)
  }
  
  # Helper to extract date
  extract_date <- function(obj) {
    if (is.character(obj)) {
      # Try to parse ISO date string
      parsed <- tryCatch(as.Date(obj), error = function(e) NA)
      return(if (!is.na(parsed)) parsed else obj)
    }
    if (is.list(obj) && "$date" %in% names(obj)) {
      # MongoDB extended JSON format with $date
      date_val <- obj$`$date`
      if (is.character(date_val)) {
        return(as.Date(date_val))
      }
      return(date_val)
    }
    # If it's already a date/POSIXct, keep it
    if (inherits(obj, c("Date", "POSIXct", "POSIXt"))) {
      return(as.Date(obj))
    }
    return(NA)
  }
  
  # Convert list of sessions to data frame manually
  sessions_list <- lapply(sessions_data, function(session) {
    tibble(
      session_id = extract_id(session$`_id`),
      assessment_id = extract_id(session$assessment_id),
      class_id = extract_id(session$class_id),
      date = extract_date(session$date),
      group_name = as.character(session$group_name),
      students = list(unlist(session$students))  # Keep as list column
    )
  })
  
  # Combine all sessions into one data frame
  sessions <- bind_rows(sessions_list)
  
  message(sprintf("✓ Processed %d sessions", nrow(sessions)))
  return(sessions)
}

#' Get student IDs for a specific session
#' @param sessions Processed sessions data
#' @param session_id Session ID to filter by
#' @return Vector of student IDs in that session
get_session_students <- function(sessions, session_id) {
  session <- sessions %>%
    filter(session_id == !!session_id) %>%
    slice(1)
  
  if (nrow(session) == 0) {
    stop(sprintf("Session ID not found: %s", session_id))
  }
  
  student_ids <- session$students[[1]]
  message(sprintf("✓ Found %d students in session '%s'", 
                  length(student_ids), session$group_name[1]))
  
  return(student_ids)
}

#' List available sessions for user selection
#' @param sessions Processed sessions data
#' @return Prints session list and returns invisible sessions
list_available_sessions <- function(sessions) {
  message("\nAvailable Sessions:")
  message(strrep("-", 70))
  
  for (i in seq_len(nrow(sessions))) {
    s <- sessions[i, ]
    message(sprintf(
      "[%d] %s | %s | %d students | ID: %s",
      i, s$group_name, s$date, length(s$students[[1]]), s$session_id
    ))
  }
  
  message(strrep("-", 70))
  invisible(sessions)
}

# ==============================================================================
# DATA TRANSFORMATION - RUT PROCESSING
# ==============================================================================

#' Combine RUT columns and normalize format
#' @param df Data frame with RUT columns
#' @return Data frame with unified Rut column
normalize_rut_columns <- function(df) {
  message("Normalizing RUT columns...")
  
  # Replace 10 with K in verificador column
  df <- df %>%
    mutate(`verificador-0` = ifelse(
      `verificador-0` == 10, 
      "K", 
      as.character(`verificador-0`)
    ))
  
  # Unite all RUT columns into single column
  rut_cols <- c("Rut-0", "Rut-1", "Rut-2", "Rut-3", "Rut-4", 
                "Rut-5", "Rut-6", "Rut-7", "verificador-0")
  
  df <- df %>%
    unite("Rut", all_of(rut_cols), sep = "", remove = TRUE)
  
  message(sprintf("✓ Normalized %d student RUTs", nrow(df)))
  return(df)
}

# ==============================================================================
# DATA TRANSFORMATION - COLUMN CLEANUP
# ==============================================================================

#' Clean and standardize column names
#' @param df Data frame with raw column names
#' @param num_questions Number of questions in assessment
#' @return Data frame with standardized column names
standardize_column_names <- function(df, num_questions = 30) {
  message("Standardizing column names...")
  
  # Remove completely empty columns
  df <- df %>%
    select(where(~ !all(is.na(.))))
  
  # Rename response columns to standard format
  # This handles the complex pattern from the original Resp-X-Y format
  colnames(df) <- str_replace(colnames(df), 
    "Resp(\\d+)-(\\d+)", 
    function(x) {
      nums <- str_extract_all(x, "\\d+")[[1]]
      paste0("Respuestas-", as.numeric(nums[1]) * 10 + as.numeric(nums[2]) + 1)
    }
  )
  
  # Handle Respuestas-NA.n pattern
  colnames(df) <- str_replace(colnames(df), 
    "Respuestas-NA\\.(\\d+)", 
    function(x) {
      num <- as.numeric(str_extract(x, "\\d+"))
      paste0("Respuestas-", num + 1)
    }
  )
  
#change names from columns 4 on to Respuestas-1 to Respuestas-30
colnames(df)[4:ncol(df)] <- paste0("Respuestas-", 1:(ncol(df)-3))
  
  # Rename Forma-0 to Forma
  colnames(df) <- str_replace(colnames(df), "Forma-0", "Forma")
  
  message("✓ Standardized column names")
  return(df)
}

# ==============================================================================
# DATA TRANSFORMATION - FORMA (FORM) PROCESSING
# ==============================================================================

#' Convert form letters to numeric codes
#' @param df Data frame with Forma column
#' @param form_mapping Named vector mapping letters to numbers
#' @return Data frame with numeric Forma column
convert_forma_to_numeric <- function(df, form_mapping = CONFIG$form_letters) {
  message("Converting form codes...")
  
  # Normalize Forma column
  df <- df %>%
    mutate(Forma = normalize_text(Forma))
  
  # Convert letter codes to numbers
  df <- df %>%
    mutate(Forma = form_mapping[Forma]) %>%
    filter(!is.na(Forma))  # Remove invalid forms
  
  message(sprintf("✓ Converted %d form codes", nrow(df)))
  return(df)
}

# ==============================================================================
# DATA TRANSFORMATION - RESPONSE NORMALIZATION
# ==============================================================================

#' Normalize all response columns (lowercase, trim)
#' @param df Data frame with response columns
#' @param num_questions Number of questions
#' @return Data frame with normalized responses
normalize_responses <- function(df, num_questions = 30) {
  message("Normalizing student responses...")
  
  response_cols <- paste0("Respuestas-", 1:num_questions)
  existing_cols <- base::intersect(response_cols, colnames(df))
  
  df <- df %>%
    mutate(across(all_of(existing_cols), normalize_text))
  
  message(sprintf("✓ Normalized %d response columns", length(existing_cols)))
  return(df)
}

# ==============================================================================
# DATA TRANSFORMATION - RESHAPE
# ==============================================================================

#' Transform wide format results to long format
#' @param df Data frame in wide format
#' @return Data frame in long format
transform_wide_to_long <- function(df) {
  message("Transforming data from wide to long format...")
  
  df_long <- df %>%
    pivot_longer(
      cols = matches("^Respuestas-\\d+$"),
      names_to = "Pregunta",
      values_to = "Respuesta"
    ) %>%
    mutate(
      Pregunta = as.integer(str_remove(Pregunta, "Respuestas-"))
    ) %>%
    select(-any_of(c("file_name")))  # Remove any file tracking columns
  
  message(sprintf("✓ Transformed to %d rows (long format)", nrow(df_long)))
  return(df_long)
}

# ==============================================================================
# DATA JOINING AND SCORING
# ==============================================================================

#' Join results with question metadata and score responses
#' @param results_long Results in long format
#' @param questions Question metadata
#' @return Scored results with correctness indicators
join_and_score_results <- function(results_long, questions) {
  message("Joining results with questions and scoring...")
  
  # Join with questions
  results_joined <- results_long %>%
    inner_join(questions, by = c("Forma", "Pregunta"))
  
  # Score responses
  results_scored <- results_joined %>%
    mutate(
      is_correcta = case_when(
        is.na(Respuesta) | Respuesta == "" ~ FALSE,
        TRUE ~ Respuesta == normalize_text(Correcta)
      )
    ) %>%
    rename(student_id = Rut)
  
  message(sprintf("✓ Scored %d responses", nrow(results_scored)))
  return(results_scored)
}

# ==============================================================================
# DATA VALIDATION
# ==============================================================================

#' Validate that all students in results exist in master student list
#' @param test_students Vector of student IDs from test
#' @param master_students Data frame of all valid students
#' @return Invisible TRUE or stops with error
validate_student_ids <- function(test_students, master_students) {
  message("Validating student IDs...")
  
  missing_students <- setdiff(test_students, master_students$student_id)
  
  if (length(missing_students) > 0) {
    warning(sprintf(
      "⚠ %d student(s) in results not found in Students.csv:\n  %s",
      length(missing_students),
      paste(head(missing_students, 10), collapse = ", ")
    ))
    
    if (length(missing_students) > 10) {
      warning(sprintf("  ... and %d more", length(missing_students) - 10))
    }
  } else {
    message("✓ All student IDs validated")
  }
  
  invisible(TRUE)
}

# ==============================================================================
# ANALYSIS - STUDENT SCORES
# ==============================================================================

#' Calculate overall scores per student
#' @param scored_results Scored results data
#' @return Data frame with student scores
calculate_student_scores <- function(scored_results) {
  message("Calculating student scores...")
  
  scores <- scored_results %>%
    group_by(student_id) %>%
    summarise(
      n = n(), 
      correctas = sum(is_correcta, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(porcentaje = ifelse(n > 0, 100 * correctas / n, NA_real_))
  
  message(sprintf("✓ Calculated scores for %d students", nrow(scores)))
  return(scores)
}

# ==============================================================================
# ANALYSIS - QUESTION PERFORMANCE
# ==============================================================================

#' Calculate performance statistics per question (by Origen)
#' @param scored_results Scored results data
#' @return Data frame with question performance metrics
calculate_question_performance <- function(scored_results) {
  message("Calculating question performance...")
  
  performance <- scored_results %>%
    select(Origen, is_correcta) %>%
    group_by(Origen) %>%
    summarise(
      n = n(), 
      correctas = sum(is_correcta, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(porcentaje = ifelse(n > 0, 100 * correctas / n, NA_real_)) %>%
    arrange(porcentaje)
  
  message(sprintf("✓ Calculated performance for %d questions", nrow(performance)))
  return(performance)
}

# ==============================================================================
# OUTPUT - EXPORT FUNCTIONS
# ==============================================================================

#' Export results to Excel file
#' @param data Data frame to export
#' @param filename Output filename
#' @return Invisible TRUE
export_to_excel <- function(data, filename) {
  message(sprintf("Exporting to %s...", filename))
  
  tryCatch({
    write_xlsx(data, filename)
    message(sprintf("✓ Exported %d rows to %s", nrow(data), filename))
  }, error = function(e) {
    stop(sprintf("Error exporting to %s: %s", filename, e$message))
  })
  
  invisible(TRUE)
}

#' Export session students list to text file
#' @param students_df Complete students data frame
#' @param session_students Vector of student IDs in the session
#' @param session_id Session ID for naming the file
#' @param output_dir Directory for output file
#' @return Invisible TRUE
export_session_students <- function(students_df, session_students, session_id, 
                                   output_dir = CONFIG$output_dir) {
  filename <- file.path(output_dir, sprintf("students_session_%s.txt", session_id))
  message(sprintf("Exporting session students to %s...", filename))
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Filter and select students in the session
  session_student_data <- students_df %>%
    filter(student_id %in% session_students) %>%
    select(student_id, name, sexo, email) %>%
    arrange(student_id)
  
  tryCatch({
    # Write without headers, comma-separated
    write.table(
      session_student_data,
      file = filename,
      sep = ",",
      row.names = FALSE,
      col.names = FALSE,
      quote = FALSE,
      fileEncoding = "UTF-8"
    )
    message(sprintf("✓ Exported %d students to %s", nrow(session_student_data), filename))
  }, error = function(e) {
    stop(sprintf("Error exporting students to %s: %s", filename, e$message))
  })
  
  invisible(TRUE)
}

# ==============================================================================
# OUTPUT - REPORT GENERATION
# ==============================================================================

#' Generate individual student reports using Quarto
#' @param student_ids Vector of student IDs
#' @param session_id Session ID for the assessment
#' @param output_dir Directory for output files
#' @return Data frame with generation results
generate_student_reports <- function(student_ids, session_id, output_dir = CONFIG$output_dir) {
  message(sprintf("Generating reports for %d students in session %s...", 
                  length(student_ids), session_id))
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  results <- tibble(
    student_id = student_ids,
    status = NA_character_,
    message = NA_character_
  )
  
  for (i in seq_along(student_ids)) {
    sid <- student_ids[i]
    out_file <- sprintf("%s.pdf", sid)
    
    cmd <- sprintf(
      'quarto render Reporte.qmd --to pdf --no-clean -P student_id:%s -P session_id:%s --output-dir %s -o %s',
      sid, session_id, output_dir, out_file
    )
    
    message(sprintf("  [%d/%d] Processing %s...", i, length(student_ids), sid))
    
    status <- system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
    
    if (status == 0) {
      results$status[i] <- "success"
      results$message[i] <- "Report generated successfully"
    } else {
      results$status[i] <- "failed"
      results$message[i] <- sprintf("Quarto render failed with status %d", status)
      warning(sprintf("⚠ Failed to generate report for %s", sid))
    }
  }
  
  success_count <- sum(results$status == "success")
  message(sprintf("✓ Generated %d/%d reports successfully", 
                  success_count, length(student_ids)))
  
  return(results)
}

#' Generate teacher report for a session using Quarto
#' @param session_id Session ID for the assessment
#' @param output_dir Directory for output files
#' @return List with status and message
generate_teacher_report <- function(session_id, output_dir = CONFIG$output_dir) {
  message(sprintf("Generating teacher report for session %s...", session_id))
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  out_file <- sprintf("ReporteDocente_%s.pdf", session_id)
  
  cmd <- sprintf(
    'quarto render ReporteDocente.qmd --to pdf --no-clean -P session_id:%s --output-dir %s -o %s',
    session_id, output_dir, out_file
  )
  
  status <- system(cmd, ignore.stdout = FALSE, ignore.stderr = FALSE)
  
  # Check if the PDF was actually created (sometimes quarto returns non-zero but succeeds)
  pdf_path <- file.path(output_dir, out_file)
  
  # Wait a moment for file system to sync (especially on network drives)
  Sys.sleep(0.5)
  
  pdf_exists <- file.exists(pdf_path)
  
  # Debug: print paths to help troubleshoot
  if (!pdf_exists) {
    message(sprintf("Debug: Looking for PDF at: %s", pdf_path))
    message(sprintf("Debug: Output dir exists: %s", dir.exists(output_dir)))
    message(sprintf("Debug: Files in output dir:"))
    if (dir.exists(output_dir)) {
      message(paste("  ", list.files(output_dir, pattern = ".*Docente.*"), collapse = "\n"))
    }
  }
  
  result <- list(
    session_id = session_id,
    status = if (pdf_exists) "success" else "failed",
    message = if (pdf_exists) {
      "Teacher report generated successfully"
    } else {
      sprintf("Quarto render failed with status %d - PDF not found at %s", status, pdf_path)
    },
    output_file = if (pdf_exists) pdf_path else NA_character_
  )
  
  if (pdf_exists) {
    message(sprintf("✓ Teacher report generated: %s", out_file))
  } else {
    warning(sprintf("⚠ Failed to generate teacher report (exit status: %d)", status))
  }
  
  return(result)
}

# ==============================================================================
# CACHING
# ==============================================================================

#' Save metadata to cache file
#' @param metadata List of metadata objects
#' @param cache_file Path to cache file
#' @return Invisible TRUE
save_metadata_cache <- function(metadata, cache_file = CONFIG$cache_file) {
  message(sprintf("Saving metadata cache to %s...", cache_file))
  
  tryCatch({
    saveRDS(metadata, file = cache_file)
    message("✓ Metadata cached successfully")
  }, error = function(e) {
    warning(sprintf("Failed to save cache: %s", e$message))
  })
  
  invisible(TRUE)
}

# ==============================================================================
# MAIN PROCESSING PIPELINE
# ==============================================================================

#' Main processing pipeline
#' @param config Configuration list
#' @return List with processed data and results
process_assessment_results <- function(config = CONFIG) {
  message("\n" , strrep("=", 70))
  message("ASSESSMENT RESULTS PROCESSOR")
  message(strrep("=", 70), "\n")
  
  # ============================================================================
  # STEP 1: Load Data
  # ============================================================================
  data <- load_all_data(config)
  
  # ============================================================================
  # STEP 2: Clean and Transform Results
  # ============================================================================
  clean_results <- data$results %>%
    normalize_rut_columns() %>%
    standardize_column_names(config$num_questions) %>%
    convert_forma_to_numeric(config$form_letters) %>%
    normalize_responses(config$num_questions)
  
  # ============================================================================
  # STEP 3: Transform to Long Format and Score
  # ============================================================================
  results_long <- transform_wide_to_long(clean_results)
  scored_results <- join_and_score_results(results_long, data$questions)
  
  # ============================================================================
  # STEP 4: Process Session Data and Filter Students
  # ============================================================================
  sessions <- data$sessions
  session_students <- get_session_students(sessions, config$session_id)
  
  # Filter scored results to only students in this session
  scored_results <- scored_results %>%
    filter(student_id %in% session_students)
  
  students_in_test <- unique(scored_results$student_id)
  validate_student_ids(students_in_test, data$students)
  
  # Filter to valid students only
  scored_results <- scored_results %>%
    filter(student_id %in% data$students$student_id)
  
  # ============================================================================
  # STEP 5: Calculate Scores and Performance
  # ============================================================================
  student_scores <- calculate_student_scores(scored_results)
  question_performance <- calculate_question_performance(scored_results)
  
  # ============================================================================
  # STEP 6: Export Results
  # ============================================================================
  export_to_excel(student_scores, config$output_files$student_scores)
  export_to_excel(question_performance, config$output_files$question_performance)
  export_session_students(data$students, session_students, config$session_id, config$output_dir)
  
  # ============================================================================
  # STEP 7: Cache Metadata
  # ============================================================================
  forma_10_questions <- data$questions %>% filter(Forma == 10)
  
  metadata <- list(
    results = scored_results,
    students = data$students,
    results_general = student_scores,
    ejes = data$ejes,
    questions_extra = forma_10_questions,
    sessions = sessions,
    timestamp = Sys.time()
  )
  
  save_metadata_cache(metadata, config$cache_file)
  
  # ============================================================================
  # STEP 8: Generate Reports (Optional)
  # ============================================================================
  student_report_status <- NULL
  teacher_report_status <- NULL
  
  # Generate student reports if configured
  if (config$generate_student_reports) {
    student_report_status <- generate_student_reports(
      students_in_test,
      config$session_id,
      config$output_dir
    )
  }
  
  # Generate teacher report if configured
  if (config$generate_teacher_report) {
    teacher_report_status <- generate_teacher_report(
      config$session_id,
      config$output_dir
    )
  }
  
  # ============================================================================
  # COMPLETION
  # ============================================================================
  message("\n", strrep("=", 70))
  message("✓ PROCESSING COMPLETE")
  message(strrep("=", 70))
  message(sprintf("  Students processed: %d", length(students_in_test)))
  message(sprintf("  Total responses: %d", nrow(scored_results)))
  message(sprintf("  Average score: %.1f%%", mean(student_scores$porcentaje, na.rm = TRUE)))
  message(sprintf("  Output directory: %s", config$output_dir))
  if (!is.null(student_report_status)) {
    success_count <- sum(student_report_status$status == "success")
    message(sprintf("  Student reports: %d/%d generated", success_count, nrow(student_report_status)))
  }
  if (!is.null(teacher_report_status) && teacher_report_status$status == "success") {
    message(sprintf("  Teacher report: %s", basename(teacher_report_status$output_file)))
  }
  message(strrep("=", 70), "\n")
  
  # Return results
  invisible(list(
    scored_results = scored_results,
    student_scores = student_scores,
    question_performance = question_performance,
    student_report_status = student_report_status,
    teacher_report_status = teacher_report_status,
    metadata = metadata
  ))
}

# ==============================================================================
# ENTRY POINT
# ==============================================================================

#' Run the complete assessment processing pipeline
#' @export
main <- function() {
  tryCatch({
    results <- process_assessment_results(config = CONFIG)
    return(results)
  }, error = function(e) {
    message("\n", strrep("=", 70))
    message("✗ ERROR OCCURRED")
    message(strrep("=", 70))
    message(sprintf("Error: %s", e$message))
    message(strrep("=", 70), "\n")
    stop(e)
  })
}

main()
