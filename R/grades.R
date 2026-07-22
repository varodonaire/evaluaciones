# ==============================================================================
# CONVERSION A NOTAS
# ==============================================================================

#' Convertir porcentaje a nota en escala chilena (1.0 - 7.0)
#'
#' @param porcentaje Vector de porcentajes (0-100)
#' @param nota_minima Nota minima (default: 1.0)
#' @param nota_maxima Nota maxima (default: 7.0)
#' @param nota_aprobacion Nota de aprobacion (default: 4.0)
#' @param exigencia Porcentaje de exigencia para aprobar (default: 60)
#'
#' @return Vector de notas
#' @export
#'
#' @examples
#' porcentaje_to_nota(c(0, 30, 60, 80, 100))
#' # [1] 1.0 2.5 4.0 5.5 7.0
porcentaje_to_nota <- function(porcentaje,
                                nota_minima = 1.0,
                                nota_maxima = 7.0,
                                nota_aprobacion = 4.0,
                                exigencia = 60) {
  
  nota <- ifelse(
    porcentaje < exigencia,
    # Bajo el umbral de aprobacion: escala entre nota_minima y nota_aprobacion
    nota_minima + (nota_aprobacion - nota_minima) * (porcentaje / exigencia),
    # Sobre el umbral: escala entre nota_aprobacion y nota_maxima
    nota_aprobacion + (nota_maxima - nota_aprobacion) * ((porcentaje - exigencia) / (100 - exigencia))
  )
  
  # Asegurar limites
  nota <- pmax(nota_minima, pmin(nota_maxima, nota))
  
  round(nota, 1)
}

#' Agregar columna de notas a puntajes de estudiantes
#'
#' @param student_scores Data frame con columna 'porcentaje'
#' @param ... Argumentos adicionales para porcentaje_to_nota
#'
#' @return Data frame con columna 'nota' agregada
#' @export
add_grades <- function(student_scores, ...) {
  student_scores %>%
    mutate(nota = porcentaje_to_nota(porcentaje, ...))
}
