# ==============================================================================
# EJEMPLO DE USO DEL PAQUETE PruebasUDD
# ==============================================================================

library(PruebasUDD)

# ==============================================================================
# PARTE 1: GENERACIÓN DE EXÁMENES
# ==============================================================================

# --- 1.1 Crear configuración del examen ---
config_examen <- create_exam_config(
  name = "algebra_b4_p1",
  year = 2025,
  period = 4,                    # Bimestre 4
  subject = "Álgebra",
  num_forms = 10,                # 10 versiones (A-J)
  groups = list(
    sizes = c(4, 4, 2),          # Formas A-D, E-H, I-J
    seeds = c(13, 17, 57)        # Seeds diferentes por grupo
  ),
  template = "mi_template.tex",  # Template LaTeX personalizado
  output_dir = "examenes_generados",
  tex_engine = "xelatex"
)

print(config_examen)

# --- 1.2 Crear pool de preguntas ---

# Opción A: Usando patrones de nombres
pool <- create_pool_from_patterns(
  patterns = list(
    "Progresiones"    = "ProgArit_",
    "Progresiones"    = "ProgGeom_",
    "Sumatorias"      = "Sumatoria_",
    "Binomio Newton"  = "BinoNewton_",
    "Sucesiones"      = "Sucesion_"
  ),
  nums = 1:5,
  extension = ".Rnw"
)

# Opción B: Especificando manualmente
pool_manual <- create_question_pool(
  questions = c(
    "ProgArit_1.Rnw", "ProgArit_2.Rnw", "ProgArit_3.Rnw",
    "Sumatoria_1.Rnw", "Sumatoria_2.Rnw"
  ),
  topics = c(
    "Progresiones", "Progresiones", "Progresiones",
    "Sumatorias", "Sumatorias"
  ),
  base_dir = "preguntas/"
)

# Opción C: Usando data.frame
pool_df <- create_question_pool(
  questions = data.frame(
    file = c("P1.Rnw", "P2.Rnw", "P3.Rnw"),
    topic = c("Tema1", "Tema1", "Tema2")
  )
)

print(pool)

# --- 1.3 Validar que existan los archivos ---
validate_question_pool(pool, stop_on_missing = FALSE)

# --- 1.4 Generar los exámenes ---
resultado <- generate_exams(
  config = config_examen,
  pool = pool,
  order_seed = 1234,
  verbose = TRUE
)

# --- 1.5 Exportar ítems a JSON ---
items <- export_items_json(
  exam_result = resultado,
  output_file = "items_algebra_b4_p1.json"
)


# ==============================================================================
# PARTE 2: PROCESAMIENTO DE RESULTADOS
# ==============================================================================

# --- 2.1 Crear configuración de procesamiento ---
config_proceso <- create_processing_config(
  results_file = "datos/resultados_escaneados.xlsx",
  items_file = "items_algebra_b4_p1.json",
  students_file = "datos/estudiantes.csv",
  sessions_file = "datos/sesiones.json",      # Opcional
  session_id = "692ce3f5a602af67a5ca9289",    # Opcional: filtrar por sesión
  num_questions = 25,
  output_dir = "reportes_b4_p1",
  generate_reports = FALSE
)

print(config_proceso)

# --- 2.2 (Opcional) Listar sesiones disponibles ---
list_sessions("datos/sesiones.json")

# --- 2.3 Procesar los resultados ---
resultados <- process_assessment(config_proceso)

# --- 2.4 Acceder a los resultados ---
# Puntajes por estudiante
head(resultados$student_scores)
#   Rut          n_respuestas correctas porcentaje
# 1 12345678K            25        22      88.0
# 2 23456789-0           25        20      80.0

# Rendimiento por pregunta
head(resultados$item_performance)
#   Origen       n_respuestas correctas porcentaje
# 1 ProgArit_1           45        38      84.4
# 2 Sumatoria_3          45        25      55.6

# Rendimiento por eje/tema
head(resultados$topic_performance)
#   Eje              n_respuestas correctas porcentaje
# 1 Progresiones           450       356      79.1
# 2 Sumatorias             225       145      64.4


# ==============================================================================
# PARTE 3: GENERACIÓN DE REPORTES
# ==============================================================================

# Los reportes usan tus templates originales (Reporte.qmd y ReporteDocente.qmd)
# que esperan datos en formato específico con: results, students, results_general,
# ejes, questions_extra, sessions

# --- 3.1 Preparar metadatos para reportes ---
# Esta función convierte los datos al formato que esperan los templates

# Definir ejes temáticos (identificador y nombre descriptivo)
ejes_df <- tibble::tibble(
  identificador = c("PR", "SU", "Teo_bin", "SEC"),
  nombre = c("Progresiones", "Sumatorias", "Teorema del Binomio", "Sucesiones")
)

# Preparar metadatos
metadata <- prepare_report_metadata(
  scored_data = resultados$scored_data,
  students_df = estudiantes,
  items_df = items_df,           # Data frame con ítems (Texto, Alternativas, etc.)
  ejes_df = ejes_df,
  sessions_df = sesiones_df,     # Data frame con sesiones
  reference_form = 10            # Forma de referencia para preguntas completas
)

# Opcionalmente, guardar para reutilizar
save_report_metadata(metadata, ".__session_cache.rds")

# --- 3.2 Generar reporte de sesión (para el docente) ---
reporte_sesion <- generate_session_report(
  session_id = "692ce3f5a602af67a5ca9289",
  metadata = metadata,
  output_dir = "reportes"
)

# --- 3.3 Generar reporte individual de un estudiante ---
reporte_individual <- generate_student_report(
  student_id = "12345678-9",
  session_id = "692ce3f5a602af67a5ca9289",
  metadata = metadata,
  output_dir = "reportes"
)

# --- 3.4 Generar reportes para TODOS los estudiantes de la sesión ---
reportes_todos <- generate_student_reports(
  student_ids = unique(resultados$scored_data$student_id),
  session_id = "692ce3f5a602af67a5ca9289",
  metadata = metadata,
  output_dir = "reportes"
)

# Ver resultados de generación
print(reportes_todos)

# --- 3.5 Generar TODOS los reportes de una vez ---
todos_reportes <- generate_all_reports(
  metadata = metadata,
  session_id = "692ce3f5a602af67a5ca9289",
  output_dir = "reportes_completos",
  generate_individual = TRUE,
  generate_session = TRUE,
  generate_psychometric = TRUE  # Incluir reporte psicométrico
)

# --- 3.6 Generar SOLO el reporte psicométrico (para operadores/administradores) ---
reporte_psicometrico <- generate_operator_report(
  session_id = "692ce3f5a602af67a5ca9289",
  metadata = metadata,
  output_dir = "reportes"
)

# Ver resumen del análisis
print(reporte_psicometrico$psychometric_summary)
# $kr20
# [1] 0.782
# $n_items
# [1] 25
# $n_problematic
# [1] 2
# $forms_equivalent
# [1] TRUE

# --- 3.7 Usando archivo de caché existente ---
# Si ya tienes un archivo .__session_cache2.rds puedes usarlo directamente:
# reporte <- generate_student_report(
#   student_id = "12345678-9",
#   session_id = "692ce3f5a602af67a5ca9289",
#   metadata = ".__session_cache2.rds",  # Ruta al archivo
#   output_dir = "reportes"
# )


# ==============================================================================
# PARTE 4: ANÁLISIS PSICOMÉTRICO (AVANZADO)
# ==============================================================================

# Puedes ejecutar el análisis psicométrico directamente sin generar PDF

# --- 4.1 Análisis completo ---
analisis <- run_psychometric_analysis(resultados$scored_data, id_col = "student_id")

# --- 4.2 Acceder a componentes individuales ---

# Dificultad de ítems
head(analisis$difficulty)
# Origen     n_respuestas n_correctas dificultad clasificacion
# ProgArit_1          45          38      0.844 Fácil
# Sumatoria_3         45          25      0.556 Moderado

# Discriminación de ítems
head(analisis$discrimination)
# Origen     n_respuestas discriminacion clasificacion
# ProgArit_1          45          0.412 Excelente
# Sumatoria_3         45          0.287 Moderada

# Confiabilidad
print(analisis$reliability)
# $kr20
# [1] 0.782
# $classificacion
# [1] "Aceptable"
# $sem
# [1] 2.34

# Ítems problemáticos
print(analisis$problematic_items$problematic_items)
# Origen     dificultad discriminacion problemas
# BadItem_1       0.05          -0.12 Discriminación negativa
# EasyItem        0.98           0.08 Demasiado fácil

# Comparación entre formas
print(analisis$form_comparison$form_stats)
# Forma n_estudiantes promedio mediana desv_std
# A            15     72.3    74.0     12.4
# B            15     70.8    71.0     13.1

# --- 4.3 Análisis individuales ---

# Solo dificultad (con desglose por forma)
dificultad_por_forma <- calculate_difficulty_index(resultados$scored_data, by_form = TRUE)

# Solo discriminación (método alternativo D27)
discriminacion_d27 <- calculate_discrimination_index(
  resultados$scored_data, 
  method = "d27",  # Usa grupos extremos 27% superior/inferior
  id_col = "student_id"
)

# Solo análisis de distractores
distractores <- analyze_distractors(resultados$scored_data, id_col = "student_id")
# Muestra qué alternativas eligen los grupos alto/medio/bajo

# Detectar ítems problemáticos
problematicos <- detect_problematic_items(
  calculate_difficulty_index(resultados$scored_data),
  calculate_discrimination_index(resultados$scored_data)
)


# ==============================================================================
# PARTE 5: CONVERSIÓN A NOTAS
# ==============================================================================

# --- 5.1 Agregar notas a los puntajes ---
puntajes_con_notas <- add_grades(
  resultados$student_scores,
  nota_minima = 1.0,
  nota_maxima = 7.0,
  nota_aprobacion = 4.0,
  exigencia = 60  # 60% para aprobar
)

head(puntajes_con_notas)
#   Rut          n_respuestas correctas porcentaje nota
# 1 12345678K            25        22      88.0    6.1
# 2 23456789-0           25        20      80.0    5.5
# 3 34567890-1           25        14      56.0    3.8

# --- 5.2 Convertir porcentaje a nota directamente ---
porcentaje_to_nota(c(0, 30, 60, 80, 100))
# [1] 1.0 2.5 4.0 5.5 7.0

# Con exigencia diferente (50%)
porcentaje_to_nota(c(0, 25, 50, 75, 100), exigencia = 50)
# [1] 1.0 2.5 4.0 5.5 7.0


# ==============================================================================
# PARTE 6: FLUJO COMPLETO SIMPLIFICADO
# ==============================================================================

# Para uso rápido, puedes encadenar todo:

# 1. Configurar y generar examen
config <- create_exam_config("fisica_b1", 2026, 1, "Física", num_forms = 5)
pool <- create_pool_from_patterns(
  list("Cinemática" = "Cinem_", "Dinámica" = "Dinam_"),
  nums = 1:3
)

# Si los archivos existen:
# resultado <- generate_exams(config, pool)
# export_items_json(resultado, "items_fisica.json")

# 2. Después de aplicar la prueba y escanear:
# proc_config <- create_processing_config(
#   results_file = "respuestas.xlsx",
#   items_file = "items_fisica.json", 
#   students_file = "alumnos.csv",
#   num_questions = 6
# )
# resultados <- process_assessment(proc_config)

# 3. Generar todos los reportes
# generate_all_reports(resultados, estudiantes, exam_info = list(nombre = "Física B1"))


# ==============================================================================
# INFORMACIÓN NECESARIA PARA EL PAQUETE
# ==============================================================================

# PARA GENERAR EXÁMENES necesitas:
# ┌─────────────────────────────────────────────────────────────────────────┐
# │ 1. Archivos .Rnw con las preguntas (formato paquete 'exams')           │
# │ 2. Template LaTeX (opcional, usa default si no se especifica)          │
# │ 3. Configuración: año, período, asignatura, número de formas           │
# │ 4. Mapeo de preguntas a temas/ejes                                     │
# └─────────────────────────────────────────────────────────────────────────┘

# PARA PROCESAR RESULTADOS necesitas:
# ┌─────────────────────────────────────────────────────────────────────────┐
# │ 1. Excel con respuestas escaneadas (columnas: Rut, Forma, Respuestas)  │
# │ 2. JSON con ítems y claves (generado por export_items_json)            │
# │ 3. CSV con datos de estudiantes (student_id, name, etc.)               │
# │ 4. (Opcional) JSON de sesiones para filtrar por grupo                  │
# └─────────────────────────────────────────────────────────────────────────┘

# PARA GENERAR REPORTES necesitas:
# ┌─────────────────────────────────────────────────────────────────────────┐
# │ 1. Resultados procesados (scored_data con is_correcta)                 │
# │ 2. Data frame de estudiantes (student_id, name)                        │
# │ 3. Data frame de ítems (Origen, Eje, Texto, Alternativas, Correcta)    │
# │ 4. Data frame de ejes (identificador, nombre)                          │
# │ 5. (Opcional) Data frame de sesiones (session_id, group_name, etc.)    │
# │ 6. Quarto + XeLaTeX + fuente Libertinus Sans instalados                │
# └─────────────────────────────────────────────────────────────────────────┘

# ESTRUCTURA DE METADATOS (prepare_report_metadata genera esto):
# metadata <- list(
#   results = scored_data,        # Respuestas con student_id, is_correcta, Eje, etc.
#   students = students_df,       # student_id, name
#   results_general = ...,        # Puntajes por estudiante (se calcula automáticamente)
#   ejes = ejes_df,              # identificador, nombre
#   questions_extra = ...,        # Preguntas de forma de referencia
#   sessions = sessions_df        # session_id, group_name, date, students
# )

# FORMATO DEL EXCEL DE RESULTADOS:
# | Rut-0 | Rut-1 | ... | verificador-0 | Forma-0 | Resp0-0 | Resp0-1 | ... |
# O alternativamente:
# | Rut       | Forma | Respuestas-1 | Respuestas-2 | ... |

# FORMATO DEL CSV DE ESTUDIANTES:
# | student_id | name        | email           | sexo |
# | 12345678K  | Juan Pérez  | juan@email.com  | M    |
