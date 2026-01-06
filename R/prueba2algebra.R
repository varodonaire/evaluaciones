# Improved version of prueba1algebra.R
# This script generates multiple versions of an algebra exam

# Load required libraries
library(tidyverse)
library(exams)
library(knitr)
library(jsonlite)

# Configuration parameters
EXAM_CONFIG <- list(
  num_exams = 10,
  num_groups = 3,
  group_sizes = c(4, 4, 2),  # Forms A-D, E-H, I-J
  seeds = c(13, 17, 57),
  output_dir = "exams",
  tex_dir = "tex",
  template = "prueba2algebra.tex"
)

# Question pool - organized by topic
create_question_list <- function() {
  # Only include files that actually exist in the workspace
  prog_arit <- paste0("ProgArit_", c(1, 2, 3, 4, 5), ".Rnw")
  prog_geom <- paste0("ProgGeom_", c(1, 2, 3, 4, 5), ".Rnw")
  sumatoria <- paste0("Sumatoria_", 1:5, ".Rnw")
  bino_newton <- paste0("BinoNewton_", 1:5, ".Rnw")
  sucesion <- paste0("Sucesion_", c(1, 2, 3, 4,5 ), ".Rnw")

  return(c(prog_arit, prog_geom, sumatoria, bino_newton, sucesion))
}

# Validate that all question files exist
validate_question_files <- function(question_list) {
  missing_files <- question_list[!file.exists(question_list)]
  
  if (length(missing_files) > 0) {
    cat("ERROR: Missing question files:\n")
    cat(paste("-", missing_files, collapse = "\n"), "\n")
    stop("Cannot proceed: missing question files. Please check that all .Rnw files are in the working directory.")
  }
  
  cat("✓ All", length(question_list), "question files found.\n")
  return(TRUE)
}

# Generate random seeds for each group
generate_group_seeds <- function(num_questions, seeds) {
  group_seeds <- list()
  
  for (i in seq_along(seeds)) {
    set.seed(seeds[i])
    group_seeds[[i]] <- sample(1:100, num_questions, replace = TRUE)
  }
  
  return(group_seeds)
}

# Create question order matrix
create_question_order_matrix <- function(num_exams, num_questions, seed = 1234) {
  order_matrix <- matrix(0, nrow = num_exams, ncol = num_questions)
  
  set.seed(seed)
  for (i in 1:num_exams) {
    order_matrix[i, ] <- sample(1:num_questions, num_questions)
  }
  
  return(order_matrix)
}

# Create seed matrix for all exams
create_seed_matrix <- function(group_seeds, group_sizes) {
  seed_matrix <- NULL
  
  for (i in seq_along(group_sizes)) {
    group_matrix <- matrix(rep(group_seeds[[i]], group_sizes[i]), 
                          nrow = group_sizes[i], byrow = TRUE)
    seed_matrix <- rbind(seed_matrix, group_matrix)
  }
  
  return(seed_matrix)
}

# Create question selection matrix
create_question_matrix <- function(question_list, order_matrix) {
  num_rows <- nrow(order_matrix)
  num_cols <- ncol(order_matrix)
  question_matrix <- matrix("", nrow = num_rows, ncol = num_cols)
  
  for (i in 1:num_rows) {
    question_matrix[i, ] <- question_list[order_matrix[i, ]]
  }
  
  return(question_matrix)
}

# Main execution function
generate_exams <- function() {
  # Initialize
  question_list <- create_question_list()
  
  # Validate that all files exist
  validate_question_files(question_list)
  
  num_questions <- length(question_list)
  cat("Using", num_questions, "questions total\n")
  
  # Generate components
  group_seeds <- generate_group_seeds(num_questions, EXAM_CONFIG$seeds)
  order_matrix <- create_question_order_matrix(EXAM_CONFIG$num_exams, num_questions)
  seed_matrix <- create_seed_matrix(group_seeds, EXAM_CONFIG$group_sizes)
  question_matrix <- create_question_matrix(question_list, order_matrix)
  
  # Save order matrix for reference
  write.csv(order_matrix, "matriz_orden.csv", row.names = FALSE)
  
  # Generate exam forms
  form_labels <- LETTERS[1:EXAM_CONFIG$num_exams]
  
  # Create exams
  exams_result <- exams2pdf(
    question_matrix, 
    seed = seed_matrix,
    template = EXAM_CONFIG$template, 
    texengine = "xelatex",
    verbose = TRUE, 
    texdir = EXAM_CONFIG$tex_dir, 
    name = "prueba1algebra",
    header = list(forma = function(i) sprintf("{%s}", form_labels[i])),
    dir = EXAM_CONFIG$output_dir
  )
  
  return(exams_result)
}


results_to_items <- function(results, bimestre, prueba, ano, eje_lookup = NULL) {

  # Helper para acceso seguro
  get_or_null <- function(x, name) if (!is.null(x[[name]])) x[[name]] else NULL

  exam_names <- names(results)

  out <- lapply(seq_along(results), function(i_exam) {
    exam <- results[[i_exam]]

    # Forma (desde el nombre exam01 → 1)
    forma <- if (!is.null(exam_names))
      suppressWarnings(as.integer(gsub("\\D", "", exam_names[i_exam])))
    if (is.na(forma) || is.null(forma)) forma <- i_exam

    lapply(seq_along(exam), function(i_ex) {
      ex <- exam[[i_ex]]
      mi <- ex$metainfo

      # --- Texto ---
      question_lines <- ex$question
      question_lines <- question_lines[nzchar(question_lines)] # quita líneas vacías
      texto <- paste(question_lines, collapse = " ")

      # --- Alternativas ---
      alternativas <- ex$questionlist

# --- Origen / Eje ---
origen <- get_or_null(mi, "name")
if (is.null(origen)) origen <- get_or_null(mi, "file")

if (!is.null(eje_lookup) && !is.null(origen)) {
  if (origen %in% names(eje_lookup)) {
    eje <- unname(eje_lookup[origen])
  } else {
    stop(sprintf("Origen '%s' no está definido en eje_lookup.", origen))
  }
} else {
  stop("Falta el campo 'name' o 'file' en metainfo; no se puede determinar el eje.")
}

      # --- Correcta ---
      sol_log <- get_or_null(mi, "solution")
      if (is.null(sol_log)) {
        letra_correcta <- NA_character_
      } else {
        idx <- which(sol_log)
        letra_correcta <- if (length(idx) == 1) letters[idx] else paste(letters[idx], collapse = ",")
      }

      # --- Arma el registro final ---
      list(
        Texto        = texto,
        Alternativas = alternativas,
        Eje          = eje,
        Correcta     = letra_correcta,
        Seed         = get_or_null(mi, "seed"),
        Origen       = origen,
        Forma        = forma,
        Pregunta     = i_ex,
        Bimestre     = bimestre,
        Año          = ano,
        Prueba       = prueba
      )
    })
  })
  
  # Flatten the nested list structure
  unlist(out, recursive = FALSE)
}

# Execute the exam generation
if (interactive() || !exists("source_only")) {
  result <- generate_exams()
  cat("Exams generated successfully!\n")
  cat("Number of exams created:", EXAM_CONFIG$num_exams, "\n")
  cat("Output directory:", EXAM_CONFIG$output_dir, "\n")
  
  eje_lookup <- c(
    "BinoNewton_1" = "Teo_bin",
    "BinoNewton_2" = "Teo_bin",
    "BinoNewton_3" = "Teo_bin",
    "BinoNewton_4" = "Teo_bin",
    "BinoNewton_5" = "Teo_bin",
    "ProgArit_1"   = "PR",
    "ProgArit_2"   = "PR",
    "ProgArit_3"   = "PR",
    "ProgArit_4"   = "PR",
    "ProgArit_5"   = "PR",
    "ProgGeom_1"   = "PR",
    "ProgGeom_2"   = "PR",
    "ProgGeom_3"   = "PR",
    "ProgGeom_4"   = "PR",
    "ProgGeom_5"   = "PR",
    "Sumatoria_1"  = "SU",
    "Sumatoria_2"  = "SU",
    "Sumatoria_3"  = "SU",
    "Sumatoria_4"  = "SU",
    "Sumatoria_5"  = "SU",
    "Sucesion_1"   = "SEC",
    "Sucesion_2"   = "SEC",
    "Sucesion_3"   = "SEC",
    "Sucesion_4"   = "SEC",
    "Sucesion_5"   = "SEC",
    "Sucesion_6"   = "SEC",
    "Sucesion_7"   = "SEC"
  )

  items <- results_to_items(
  results    = result,
  bimestre   = 4,
  prueba     = 1,
  ano        = 2025,
  eje_lookup = eje_lookup
)

write_json(
  items,
  "b4_p2_items.json",
  auto_unbox = TRUE,
  pretty     = TRUE
)
  cat("Items JSON file created: b4_p1_items.json\n")
}
