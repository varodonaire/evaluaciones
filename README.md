# PruebasUDD

Generación y revisión de pruebas educativas en R.

El paquete cubre el ciclo completo de una evaluación de alternativas: generar
varias formas aleatorizadas de un examen, exportar la clave de respuestas,
corregir las hojas de respuesta de los estudiantes, calcular notas y análisis
psicométrico, y emitir informes en PDF.

## Instalación

```r
# install.packages("remotes")
remotes::install_local(".")
```

Dependencias externas según lo que se use:

| Para | Se necesita |
|---|---|
| Generar exámenes | Una distribución LaTeX con `xelatex` (TeX Live / MacTeX) |
| Generar informes | [Quarto](https://quarto.org) en el `PATH`, más `tidyverse`, `gt` y `showtext` |
| Solo corregir y analizar | Nada adicional |

## Las tres etapas

### 1. Generar los exámenes

```r
library(PruebasUDD)

config <- create_exam_config(
  name       = "algebra_b4_p1",
  year       = 2025,
  period     = 4,
  subject    = "Álgebra",
  num_forms  = 10,                        # formas A–J
  groups     = list(sizes = c(4, 4, 2),   # A–D, E–H, I–J
                    seeds = c(13, 17, 57)),
  output_dir = "examenes"
)

pool <- create_pool_from_patterns(
  patterns = list(
    "Progresiones" = c("ProgArit_", "ProgGeom_"),
    "Sumatorias"   = "Sumatoria_"
  ),
  nums = 1:5                              # ProgArit_1.Rnw … Sumatoria_5.Rnw
)

validate_question_pool(pool)               # avisa si falta algún .Rnw
resultado <- generate_exams(config, pool, order_seed = 1234)

# La clave de respuestas, que alimenta la corrección
export_items_json(resultado, "items.json")
```

**Formas y grupos.** Las formas de un mismo grupo son **el mismo examen con las
preguntas en distinto orden**: cada pregunta conserva su semilla, y por lo tanto
sus valores numéricos, en todas las formas del grupo. Sirve para repartir
versiones equivalentes por filas de la sala sin que se copien.

Formas de grupos distintos tienen además valores numéricos distintos, porque
cada grupo sortea sus semillas a partir de su propia `seed`.

`sizes` debe sumar `num_forms`, y debe haber una `seed` por grupo. En el ejemplo,
las formas A–D son un examen, E–H otro, e I–J un tercero.

Un tema puede tener varios patrones, ya sea con un vector (como arriba) o
repitiendo el nombre en la lista.

### 2. Corregir las respuestas

```r
config_proc <- create_processing_config(
  results_file  = "respuestas.xlsx",   # salida del lector óptico
  items_file    = "items.json",        # clave del paso anterior
  students_file = "estudiantes.csv",
  num_questions = 25,
  output_dir    = "reportes"
)

resultados <- process_assessment(config_proc)
```

Deja en `output_dir` tres archivos: `puntajes_estudiantes.xlsx`,
`rendimiento_items.xlsx` y `rendimiento_ejes.xlsx`.

Para convertir a la escala chilena de 1,0 a 7,0:

```r
notas <- add_grades(resultados$student_scores, exigencia = 60)
```

### 3. Analizar e informar

```r
psico <- run_psychometric_analysis(resultados$scored_data)

psico$reliability$kr20            # confiabilidad KR-20 / alfa
psico$problematic_items           # ítems que conviene revisar
psico$form_comparison             # ¿son equivalentes las formas?
```

Los informes en PDF pasan por Quarto:

```r
metadata <- prepare_report_metadata(
  scored_data = resultados$scored_data,
  students_df = estudiantes,
  ejes_df     = ejes            # opcional: nombres descriptivos de los ejes
)

generate_all_reports(
  metadata,
  session_id            = "sesion123",
  generate_individual   = TRUE,   # un PDF por estudiante
  generate_session      = TRUE,   # informe para el docente
  generate_psychometric = TRUE    # informe para el operador
)
```

## Columna identificadora

El pipeline de corrección trabaja con `Rut` y las plantillas de informe con
`student_id`. Las funciones de análisis detectan cuál está presente, así que
puedes encadenarlas en cualquier orden. Si tus datos usan otro nombre, pásalo
explícitamente:

```r
run_psychometric_analysis(datos, id_col = "matricula")
```

`create_processing_config(..., id_col = "Rut")` fija la columna para todo el
pipeline de corrección.

## Formato de los datos de entrada

**Respuestas** (Excel del lector óptico). El paquete reconoce columnas
`Rut-0` / `verificador-0` (las une, traduciendo el verificador 10 a `K`) o una
columna `Rut` ya formada; la forma en `Forma-0` o `Forma`, como letra o como
número; y las respuestas en columnas que empiecen con `Resp`.

Las filas cuya forma no se reconoce se descartan **con un aviso** que indica
qué valores aparecieron y cuáles se esperaban. Si no se reconoce ninguna, se
detiene con error en vez de devolver una tabla vacía.

**Ítems** (JSON). Lo genera `export_items_json()`. Un objeto por ítem con
`Forma`, `Pregunta`, `Origen`, `Eje` y `Correcta`.

**Sesiones** (JSON, opcional). Formato de exportación de MongoDB; se usa para
filtrar por grupo de estudiantes.

## Referencia rápida

| Etapa | Funciones |
|---|---|
| Configuración | `create_exam_config`, `create_question_pool`, `create_pool_from_patterns`, `validate_question_pool`, `create_processing_config` |
| Generación | `generate_exams`, `export_items_json` |
| Corrección | `process_assessment`, `score_responses`, `calculate_student_scores`, `calculate_item_performance` |
| Notas | `porcentaje_to_nota`, `add_grades` |
| Psicometría | `run_psychometric_analysis`, `calculate_difficulty_index`, `calculate_discrimination_index`, `calculate_reliability`, `analyze_distractors`, `compare_forms`, `detect_problematic_items` |
| Informes | `prepare_report_metadata`, `generate_all_reports`, `generate_student_report`, `generate_session_report`, `generate_operator_report` |
| Sesiones | `list_sessions`, `get_session_students` |

Hay un recorrido completo y comentado en
[`inst/examples/ejemplo_uso.R`](inst/examples/ejemplo_uso.R).

## Fuentes de los informes

Las plantillas usan Libertinus Sans. Se busca en las rutas habituales del
sistema y, si no está instalada, los gráficos caen a la fuente por defecto en
vez de fallar. Para apuntar a otro archivo:

```r
Sys.setenv(PRUEBASUDD_FONT_REGULAR = "/ruta/a/LibertinusSans-Regular.otf")
```

El `mainfont` del preámbulo LaTeX sigue siendo fijo: si xelatex no encuentra
Libertinus, hay que editar el YAML de las plantillas en `inst/templates/`.

## Tests

```r
devtools::test()
```

## Licencia

MIT
