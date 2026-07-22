# evaluaciones

Generación y revisión de pruebas educativas en R.

El paquete cubre el ciclo completo de una evaluación de alternativas: generar
varias formas aleatorizadas de un examen, exportar la clave de respuestas,
corregir las hojas de respuesta de los estudiantes, calcular notas y análisis
psicométrico, y emitir informes en PDF.

> **¿Vas a operar el sistema, no a programarlo?**
> Lee [MANUAL-OPERADOR.md](MANUAL-OPERADOR.md): el ciclo completo —armar la
> prueba, corregirla y emitir los informes— sin escribir una línea de R.

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
library(evaluaciones)

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

## Ejes temáticos

Cada pregunta declara su eje en el propio `.Rnw`, con los campos de metadatos de
`exams`:

```latex
%% \extype{schoice}
%% \exname{EcCuad_1}
%% \exsection{ecuaciones_cuadraticas}
%% \exauthor{Alvaro Mattus-Donaire}
%% \exversion{1}
```

El eje tiene **dos nombres, con roles distintos**:

- **`\exsection` lleva el slug**: es la clave de unión, agrupa la psicometría y
  viaja al JSON. Minúsculas, `snake_case`, sin tildes ni espacios. Tiene que ser
  estable: cambiarlo rompe los análisis históricos.
- **`ejes_df` lleva el nombre legible**: es lo que ve el estudiante en el informe.
  Puede cambiar y llevar tildes sin romper nada.

```r
pool <- create_question_pool(dir("questions"), base_dir = "questions")

ejes <- build_ejes_df(pool, nombres = c(
  ecuaciones_cuadraticas = "Ecuaciones cuadráticas",
  logica                 = "Lógica"
))
```

`create_question_pool()` lee `\exsection` de cada archivo. Si un ejercicio no lo
declara, infiere el eje del prefijo del nombre de archivo, así que un pool mixto
funciona sin intervención.

## Preguntas en base de datos

Las preguntas pueden vivir en un directorio o en MongoDB, detrás del mismo
contrato. El resto del paquete no sabe cuál de los dos está usando.

```r
local <- question_store_local("questions")
mongo <- question_store_mongo()            # mongodb://localhost:27017, base "evaluaciones"

qs_search(mongo, section = "logica")       # buscar
qs_get(mongo, "logica_1-v2")               # traer con el código fuente
qs_materialize(mongo, ids, dir = "tmp")    # bajar a archivos .Rnw
qs_put(mongo, source, section = "logica")  # guardar
```

Cargar un directorio a la base:

```r
qs_ensure_indexes(mongo)
qs_migrate(question_store_local("questions"), mongo)
```

Es idempotente: si el contenido de una pregunta no cambió, no se crea una
versión nueva. Correrlo dos veces no duplica nada.

### Versiones inmutables

En MongoDB, **editar una pregunta crea una versión nueva y deja intacta la
anterior**. El `question_id` (`logica_1-v2`) identifica una versión concreta; el
`slug` (`logica_1`) es estable entre versiones.

Eso es lo que permite que un análisis de hace dos años siga apuntando al ítem que
realmente se aplicó, aunque la pregunta se haya reescrito diez veces desde
entonces. El `content_hash` es un sha256 del código fuente y es lo que decide si
un cambio es real.

El store local no versiona: cada archivo es su propia última versión y el
`question_id` es el slug. Sirve para trabajar sin conexión.

### Generar desde la base

```r
pool <- create_pool_from_store(mongo, section = c("logica", "conjuntos"))
res <- generate_exams(config, pool)
```

`generate_exams()` materializa las preguntas a un directorio temporal antes de
llamar a `exams`. El pool guarda los `question_id`, así que queda registrado con
qué versión exacta se generó cada examen.

## Corridas reproducibles

Cada procesamiento queda registrado como una **corrida**: qué versión de cada
pregunta se aplicó, en qué posición de qué forma, con qué semilla y con qué
versiones de R, `exams` y el paquete.

```r
sink <- results_sink_mongo()          # o results_sink_local("reportes")
rs_ensure_indexes(sink)

res <- process_assessment(config, sink = sink, exam_result = generacion)
res$run_id
```

`process_assessment()` sigue escribiendo los tres `.xlsx` de siempre. El sink
añade, sin quitar nada, una fila por **alumno y pregunta**:

```
run_id  student_id  form  position  question_id  content_hash  seed
section  correcta  respuesta  is_correcta
```

Con eso se puede rehacer el análisis más adelante, o correr uno mejor, sin
volver a la hoja escaneada.

### Regenerar un examen desde su `run_id`

```r
reproduce_run(sink, store, res$run_id, output_dir = "regenerado")
```

Recupera la corrida, trae del store las versiones exactas que se aplicaron y
regenera los PDFs con las mismas semillas. Antes de generar nada compara el
`content_hash` de cada pregunta con el registrado: si algo cambió, aborta en vez
de producir un examen que no es el que se rindió. Con `strict = FALSE` avisa y
continúa.

Contra MongoDB esa comprobación casi nunca salta, porque las versiones son
inmutables: editar una pregunta crea `logica_1-v2` y la corrida sigue apuntando
a `logica_1-v1`. Donde sí protege es en el store local, donde los `.Rnw` se
editan en el sitio.

```r
rs_list_runs(sink)     # corridas disponibles
rs_read_run(sink, id)  # corrida + respuestas
```

## Contrato de hoja de respuesta

Es la API entre quien **genera** la hoja y quien la **procesa**. El layout
describe qué hay en la hoja; el reader, cómo traducir lo que devolvió el
escaneo. Separarlos permite cambiar de lector sin tocar el resto.

```r
hoja <- answer_sheet_layout(
  n_questions = 25,                       # no es fijo
  choices = c("a", "b", "c", "d", "e"),
  fields = list(
    sheet_field("rut",         role = "student_id", kind = "digits", length = 8),
    sheet_field("verificador", role = "student_id", kind = "digits", length = 1,
                value_map = list("10" = "K")),
    sheet_field("forma",       role = "form",    kind = "letter"),
    sheet_field("respuestas",  role = "answers", kind = "choice_grid")
  ),
  reader = sheet_reader_excel(
    rut = "Rut-0", verificador = "verificador-0",
    forma = "Forma-0", answers_pattern = "^Resp"
  )
)

respuestas <- sheet_read(hoja, "escaneo.xlsx")
#> student_id  Forma  Pregunta  Respuesta
```

### Roles

Lo que el paquete entiende no son los nombres de los campos sino su **rol**:

| Rol | Significado |
|---|---|
| `student_id` | Identifica al estudiante. Puede venir partido en varios campos, que se concatenan en el orden declarado |
| `form` | La forma o versión del examen |
| `answers` | La grilla de respuestas |
| `info` | Cualquier otro dato que se quiera conservar |

Eso es lo que permite que el RUT chileno —cuerpo más dígito verificador, que
puede ser `K`— se exprese sin código especial: dos campos con rol `student_id` y
un `value_map` que traduce el 10 que marca el lector.

### El número de preguntas no es fijo

El layout lo declara y todo se dimensiona a partir de ahí. Si el archivo
escaneado no trae exactamente esas columnas, `sheet_read()` falla diciendo
cuántas esperaba y cuántas encontró, en vez de renombrar por posición y seguir.

Las alternativas pueden variar por pregunta:

```r
answer_sheet_layout(
  n_questions = 3,
  choices = list(c("a","b","c","d"), c("a","b","c","d","e"), c("v","f")),
  ...
)
```

Una respuesta fuera de las alternativas de *su* pregunta produce un aviso; las
omisiones no, porque son legítimas.

### El contrato viaja como JSON

Es la forma en que el editor de pruebas se lo entregará al paquete:

```r
layout_to_json(hoja, "hoja.json")
hoja <- layout_from_json("hoja.json")
```

```json
{
  "layout_id": "hoja-25p",
  "version": 1,
  "n_questions": 25,
  "choices": [["a","b","c","d","e"], ...],
  "fields": [
    { "name": "rut", "role": "student_id", "kind": "digits",
      "length": 8, "domain": null, "value_map": null, "required": true },
    { "name": "verificador", "role": "student_id", "kind": "digits",
      "length": 1, "value_map": { "10": "K" }, "required": true },
    { "name": "forma", "role": "form", "kind": "letter", "length": null },
    { "name": "respuestas", "role": "answers", "kind": "choice_grid" }
  ],
  "reader": {
    "type": "excel_columns",
    "mapping": { "rut": "Rut-0", "verificador": "verificador-0",
                 "forma": "Forma-0" },
    "answers_pattern": "^Resp"
  }
}
```

### Cambiar de lector

El `type` del reader elige la implementación. Hoy existe `excel_columns`, que
describe el archivo del lector óptico actual. El día que haya OMR propio se
agrega un `type` nuevo con su geometría y **nada más del sistema cambia**: el
layout, los roles y la salida normalizada son los mismos.

```r
readers_disponibles()
#> "excel_columns"
```

### Usarlo en el procesamiento

```r
config <- create_processing_config(..., layout = hoja)
```

Con layout, la lectura sigue el contrato y `num_questions` se toma de él. Sin
layout, el paquete usa las heurísticas de siempre, así que el flujo existente no
cambia.

## Usarlo desde fuera de R

El paquete expone sus operaciones como **trabajos**: una especificación en JSON
que dice qué hacer y contra qué. La CLI y la API HTTP son envoltorios delgados
sobre `run_job()`; ninguno tiene lógica propia, así que ambos hacen exactamente
lo mismo.

```r
job_actions()
#> "generate" "score" "reproduce" "list_runs" "search_questions" "migrate_questions"
```

### CLI

```bash
Rscript "$(Rscript -e 'cat(system.file("cli/evaluaciones.R", package="evaluaciones"))')" --help
```

Conviene dejarlo en el `PATH`:

```bash
ln -s "$(Rscript -e 'cat(system.file("cli/evaluaciones.R", package="evaluaciones"))')" /usr/local/bin/evaluaciones
```

Cualquier campo de la especificación se puede dar como bandera con puntos:

```bash
evaluaciones search-questions --store.type mongo --filters.section logica
```

```bash
evaluaciones generate --spec examen.json
```

```json
{
  "action": "generate",
  "store": { "type": "mongo", "db": "evaluaciones", "collection": "questions" },
  "sink":  { "type": "mongo", "db": "evaluaciones" },
  "exam":  { "name": "algebra_b4", "year": 2025, "period": 4,
             "subject": "Algebra", "num_forms": 10,
             "groups": { "sizes": [4, 4, 2], "seeds": [13, 17, 57] } },
  "pool":  { "section": ["ecuaciones_lineales", "logica"] },
  "output_dir": "salida",
  "order_seed": 1234
}
```

Devuelve JSON por la salida estándar y código 0 o 1, que es lo que necesita un
backend para encolarlo:

```json
{
  "action": "generate", "status": "ok",
  "pdfs": ["algebra_b41.pdf", "..."],
  "items_file": "salida/algebra_b4_items.json",
  "run_id": "algebra_b4-20250722103000-a1b2c3"
}
```

También acepta la especificación por entrada estándar con `--spec -`.

### API HTTP

```r
pr <- plumber::pr(system.file("plumber/plumber.R", package = "evaluaciones"))
plumber::pr_run(pr, port = 8000)
```

| Endpoint | Acción |
|---|---|
| `GET /health` | estado, versiones, si hay LaTeX y MongoDB |
| `POST /jobs` | cualquier trabajo |
| `POST /questions/search` | buscar preguntas |
| `POST /questions/migrate` | migrar entre stores |
| `POST /exams/generate` | generar exámenes |
| `POST /assessments/score` | corregir respuestas |
| `POST /runs/list` | listar corridas |
| `POST /runs/reproduce` | reproducir una corrida |

```bash
curl -X POST localhost:8000/questions/search \
  -H 'Content-Type: application/json' \
  -d '{"store":{"type":"mongo"},"filters":{"section":"logica"}}'
```

Los errores vuelven con status 400 y `{"status":"error","message":"..."}`.

**Generar exámenes necesita LaTeX y tarda.** En producción conviene encolar esa
acción en vez de servirla síncrona, y empaquetar el motor en una imagen con TeX
Live, Quarto y R.

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
Sys.setenv(EVALUACIONES_FONT_REGULAR = "/ruta/a/LibertinusSans-Regular.otf")
```

El `mainfont` del preámbulo LaTeX sigue siendo fijo: si xelatex no encuentra
Libertinus, hay que editar el YAML de las plantillas en `inst/templates/`.

## Tests

```r
devtools::test()
```

## Licencia

MIT
