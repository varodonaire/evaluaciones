# Global variables
# Declare global variables used in dplyr/tidyr non-standard evaluation
# to avoid R CMD check NOTEs about "no visible binding"

utils::globalVariables(c(
  # Common column names used throughout the package
  "Correcta", "Forma", "Forma-0", "Origen", "Pregunta", "Respuesta", "Rut",
  "verificador-0",
  
  # Calculated/intermediate variables
  "alto", "bajo", "clasificacion", "correctas", "discriminacion",
  "grupo", "is_correct", "is_correcta", "n_correctas", "n_items",
  "n_respuestas", "porcentaje", "problemas", "prop_correcta",
  "requiere_revision", "student_id", "total_correctas", "total_grupo",
  "dificultad"
))
