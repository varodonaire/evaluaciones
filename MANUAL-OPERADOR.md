# Manual del operador

Cómo armar una prueba, corregirla y emitir los informes.

**No hace falta saber R.** Todo se hace escribiendo un archivo de texto con los
datos de la prueba y ejecutando un comando. Este manual asume que alguien ya
dejó el sistema instalado; si no es así, ve a [Si algo falla](#si-algo-falla).

---

## El ciclo completo

```
  1. Recibes            2. Generas           3. Imprimes
     las preguntas   →     la prueba      →     y aplicas
     (.Rnw)                (PDF + clave)        (sala)
                                                    ↓
  6. Entregas        ←  5. Emites          ←  4. Escaneas
     los informes        los informes          las hojas
                         (PDF)                 (Excel)
```

Cada paso deja archivos que el siguiente necesita. **Guarda todo**: la carpeta
de una prueba es su respaldo, y con ella se puede rehacer el trabajo meses
después.

---

## Antes de empezar

Abre la aplicación **Terminal** y comprueba que el sistema responde:

```bash
pruebasudd --help
```

Debe aparecer una lista de acciones. Si dice "command not found", ve a
[Si algo falla](#si-algo-falla).

### Arma la carpeta de trabajo

Una carpeta por prueba, siempre con la misma estructura:

```
matematica-2026-p1/
├── preguntas/        ← los .Rnw que te entregaron
├── prueba/           ← se llena solo (paso 2)
├── datos/            ← se llena solo (registro interno)
├── informes/         ← se llena solo (paso 5)
├── estudiantes.csv   ← lo preparas tú
└── escaneo.xlsx      ← lo entrega el lector óptico (paso 4)
```

```bash
mkdir -p ~/pruebas/matematica-2026-p1/preguntas
cd ~/pruebas/matematica-2026-p1
```

Copia ahí los archivos de preguntas que te pasaron.

### La lista de estudiantes

Un archivo `estudiantes.csv` con dos columnas. Se hace en Excel y se guarda
como CSV:

| Rut | nombre |
|---|---|
| 12345678K | Ana Pérez |
| 23456789-0 | Luis Soto |

El RUT debe ir **igual a como lo marcan en la hoja**: sin puntos, y con la K en
mayúscula si corresponde.

---

## Paso 1 — Revisa las preguntas que recibiste

Antes de armar nada, mira qué tienes:

```bash
pruebasudd search-questions --store.type local --store.base_dir preguntas
```

Responde algo así:

```json
{
  "action": "search_questions", "status": "ok", "n": 32,
  "questions": [
    { "slug": "EcCuad_1", "section": "ecuaciones_cuadraticas",
      "author": "Alvaro Mattus-Donaire", "type": "schoice" }
  ]
}
```

Fíjate en dos cosas:

- **`n`** es cuántas preguntas hay. Si no cuadra con lo que te entregaron,
  falta algún archivo.
- **`section`** es el eje temático de cada pregunta. Si aparece vacío o dice
  algo raro, la pregunta no declara su eje: avísale a quien la escribió, porque
  los informes por eje van a salir mal.

Para ver solo un eje:

```bash
pruebasudd search-questions --store.type local --store.base_dir preguntas \
  --filters.section logica
```

---

## Paso 2 — Genera la prueba

Crea un archivo `prueba.json` con los datos de la evaluación:

```json
{
  "action": "generate",
  "store": { "type": "local", "base_dir": "preguntas" },
  "sink":  { "type": "local", "dir": "datos" },
  "exam": {
    "name": "matematica_2026_p1",
    "year": 2026,
    "period": 1,
    "subject": "Matemática",
    "num_forms": 4,
    "groups": { "sizes": [2, 2], "seeds": [13, 57] }
  },
  "output_dir": "prueba",
  "order_seed": 1234
}
```

Qué significa cada cosa:

| Campo | Qué es |
|---|---|
| `name` | Nombre interno. Sin espacios ni tildes: aparece en los nombres de archivo |
| `num_forms` | Cuántas versiones distintas quieres |
| `groups` | Cómo se agrupan las versiones (ver abajo) |
| `order_seed` | Número cualquiera. **Anótalo**: con él se puede regenerar la misma prueba |

Y ejecuta:

```bash
pruebasudd --spec prueba.json
```

### Qué significa `groups`

`"sizes": [2, 2]` con `"seeds": [13, 57]` significa **dos grupos de dos formas
cada uno**:

- Formas A y B: **el mismo examen**, con las preguntas en distinto orden.
- Formas C y D: **otro examen distinto**, también reordenado entre sí.

Sirve para repartir por filas: quien se siente al lado tiene otro orden, y quien
se siente dos puestos más allá tiene además otros números.

Si te da lo mismo y solo quieres N versiones todas distintas entre sí, omite
`groups`.

### Qué obtienes

En la carpeta `prueba/`:

| Archivo | Para qué |
|---|---|
| `matematica_2026_p11.pdf` … | **Los exámenes.** Uno por forma, en orden A, B, C… |
| `matematica_2026_p1_items.json` | **La clave de respuestas.** La necesitas en los pasos 4 y 5 |
| `matematica_2026_p1_generacion.rds` | Respaldo técnico. No lo borres |
| `matematica_2026_p1_orden.csv` | Qué pregunta va en qué posición, legible en Excel |

**Guarda el `items.json`.** Sin él no se puede corregir ni emitir informes.

---

## Paso 3 — Imprime y aplica

Los PDF salen en orden: el archivo terminado en `1` es la forma **A**, el `2`
es la **B**, y así.

Al repartir en la sala, anota **qué forma le tocó a cada estudiante**, o
asegúrate de que la marquen en la hoja de respuesta. Sin eso no hay corrección
posible: cada forma tiene su propia clave.

---

## Paso 4 — Corrige

Pasa las hojas por el lector óptico y guarda el Excel que produce como
`escaneo.xlsx` en la carpeta de la prueba.

Crea `correccion.json`:

```json
{
  "action": "score",
  "sink": { "type": "local", "dir": "datos" },
  "files": {
    "results":  "escaneo.xlsx",
    "items":    "prueba/matematica_2026_p1_items.json",
    "students": "estudiantes.csv"
  },
  "num_questions": 25,
  "output_dir": "resultados"
}
```

`num_questions` es cuántas preguntas tiene la prueba. Debe coincidir con lo que
trae el Excel.

```bash
pruebasudd --spec correccion.json
```

Respuesta:

```json
{
  "action": "score", "status": "ok",
  "run_id": "matematica_2026_p1-20260722103000-a1b2c3",
  "n_estudiantes": 87, "n_respuestas": 2175, "promedio": 62.4
}
```

**Anota el `run_id`.** Lo necesitas en el paso 5.

Revisa que `n_estudiantes` cuadre con cuántos rindieron. Si es menor, hubo
hojas que no se pudieron leer: mira [Si algo falla](#si-algo-falla).

### Qué obtienes

En `resultados/`, tres archivos que se abren en Excel:

| Archivo | Qué trae |
|---|---|
| `puntajes_estudiantes.xlsx` | Un estudiante por fila, con su porcentaje |
| `rendimiento_items.xlsx` | Cuántos acertaron cada pregunta |
| `rendimiento_ejes.xlsx` | Rendimiento por eje temático |

Para muchos usos, con esto basta.

---

## Paso 5 — Emite los informes

Crea `informes.json` con el `run_id` que anotaste:

```json
{
  "action": "report",
  "sink": { "type": "local", "dir": "datos" },
  "run_id": "matematica_2026_p1-20260722103000-a1b2c3",
  "files": {
    "students": "estudiantes.csv",
    "items":    "prueba/matematica_2026_p1_items.json"
  },
  "ejes": {
    "ecuaciones_cuadraticas": "Ecuaciones cuadráticas",
    "logica":                 "Lógica proposicional"
  },
  "output_dir": "informes",
  "psychometric": true
}
```

El bloque `ejes` traduce los nombres internos a los que verá el estudiante. Los
nombres internos son los `section` que viste en el paso 1. Si omites el bloque,
los informes muestran el nombre interno tal cual.

```bash
pruebasudd --spec informes.json
```

Tarda: es un PDF por estudiante. Al terminar:

```json
{
  "action": "report", "status": "ok",
  "n_individuales": 87, "n_fallidos": 0
}
```

**Revisa `n_fallidos`.** Si no es cero, hubo informes que no salieron.

### Qué obtienes

En `informes/`:

| Archivo | Para quién |
|---|---|
| `12345678K.pdf` … | **Un PDF por estudiante**, con sus resultados y qué falló |
| `ReporteDocente_<run_id>.pdf` | **Para el profesor**: cómo le fue al curso, pregunta por pregunta |
| `ReportePsicometrico_<run_id>.pdf` | **Para quien diseña la prueba**: qué preguntas funcionaron mal |

Si no necesitas el psicométrico, pon `"psychometric": false` y sale más rápido.

---

## Cosas que te van a pedir

### "¿Puedes volver a imprimir la prueba? Perdí los PDF"

Sí, y salen **idénticos** a los originales:

```json
{
  "action": "reproduce",
  "sink":  { "type": "local", "dir": "datos" },
  "store": { "type": "local", "base_dir": "preguntas" },
  "run_id": "matematica_2026_p1-20260722103000-a1b2c3",
  "output_dir": "prueba-regenerada"
}
```

Si alguien editó las preguntas desde entonces, el sistema **se detiene y avisa**
en vez de darte una prueba distinta a la que rindieron.

### "¿Qué pruebas hemos hecho?"

```bash
pruebasudd list-runs --sink.type local --sink.dir datos
```

### "Necesito los informes de nuevo"

Repite el paso 5 con el mismo `run_id`. No hace falta volver a corregir.

### "Se traspapeló una hoja, hay que recorregir"

Corrige el `escaneo.xlsx` y repite el paso 4. Se genera un `run_id` nuevo; usa
ese para los informes.

---

## Si algo falla

Todos los comandos responden con `"status"`. Si dice `"error"`, el campo
`"message"` explica qué pasó.

### `command not found: pruebasudd`

No está instalado o no está en el PATH. Pídele a quien administra el sistema que
ejecute:

```bash
ln -s "$(Rscript -e 'cat(system.file("cli/pruebasudd.R", package="PruebasUDD"))')" /usr/local/bin/pruebasudd
```

### `El layout declara 25 preguntas pero el archivo trae 30 columnas`

El `num_questions` no coincide con el Excel del lector. Cuenta las columnas de
respuesta en el Excel y corrige el número.

### `ninguna de las N filas tiene una forma reconocible`

La columna de forma del Excel trae algo que el sistema no entiende. Ábrelo y
mira qué dice: se esperan letras (`a`, `b`, `c`…) o números.

### `X de N filas descartadas por forma no reconocida`

Es un aviso, no un error: la corrección siguió con el resto. Suele ser gente que
no marcó la forma. El mensaje dice qué valores encontró.

### `n_estudiantes` es menor de lo esperado

Hubo hojas que no se leyeron. Causas típicas: el RUT quedó mal marcado, o el
estudiante no marcó la forma. Revisa el Excel del lector.

### `n_fallidos` mayor que cero en los informes

Suele ser un estudiante que está en el escaneo pero no en `estudiantes.csv`, o
al revés. Comprueba que los RUT estén escritos igual en ambos.

### `El contenido de N pregunta(s) cambió desde la corrida`

Estás intentando regenerar una prueba cuyas preguntas fueron editadas después.
El sistema se niega para no darte algo distinto a lo que se rindió. Si igual lo
necesitas, agrega `"strict": false`.

### Los informes no salen y no dice por qué

Necesitan **Quarto** y **LaTeX** instalados. Comprueba con:

```bash
quarto --version
```

Si no responde, el equipo no está configurado para emitir informes. Los tres
Excel del paso 4 sí funcionan sin eso.

---

## Lo que conviene guardar

Por cada prueba, archiva la carpeta completa. Lo imprescindible:

| Archivo | Por qué |
|---|---|
| `preguntas/` | Sin las preguntas no se puede regenerar nada |
| `prueba/*_items.json` | La clave. Sin ella no hay corrección ni informes |
| `prueba/*_generacion.rds` | Permite regenerar los PDF idénticos |
| `datos/` | El registro de qué se aplicó y qué respondió cada uno |
| `escaneo.xlsx` | El dato crudo. Permite recorregir si algo salió mal |
| `estudiantes.csv` | Para volver a emitir informes |

Los PDF de la prueba se pueden regenerar; el escaneo, no. **Si vas a borrar
algo, que no sea el escaneo.**

---

## Glosario

**Forma** — Una versión de la prueba. Formas distintas tienen las preguntas en
otro orden y, según cómo se agrupen, otros números.

**Eje** — El tema al que pertenece una pregunta. Los informes muestran cómo le
fue al estudiante en cada uno.

**`run_id`** — El identificador de una corrección. Todo lo que produce el
sistema queda colgando de él.

**`items.json`** — La clave de respuestas: qué alternativa es correcta en cada
pregunta de cada forma.

**Informe psicométrico** — Análisis de la prueba en sí: qué preguntas eran
demasiado fáciles o difíciles, cuáles no distinguen entre quien sabe y quien no,
y si las formas resultaron equivalentes.
