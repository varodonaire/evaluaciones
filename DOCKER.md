# Correr evaluaciones con Docker

Docker congela **todo el entorno** —R, los paquetes, LaTeX, Quarto y el propio
paquete— en un solo artefacto que corre igual en cualquier computador, Mac,
Windows o Linux. Escribes la receta una vez y `docker compose up` te da el
sistema completo en cualquier parte.

Esto reemplaza toda la instalación manual: no hay que instalar R, ni TeX, ni
MongoDB en la máquina. Solo Docker.

---

## Qué necesitas

**Docker Desktop.** Descárgalo de [docker.com](https://www.docker.com/products/docker-desktop/)
e instálalo. Ábrelo una vez para que el motor quede corriendo (verás la
ballena en la barra de menú).

Nada más. Ni R, ni LaTeX, ni Mongo.

---

## Levantar todo

Desde la carpeta del proyecto:

```bash
docker compose up --build
```

La **primera vez** tarda bastante (varios minutos): descarga la base, instala
LaTeX y Quarto, y construye la imagen. Las siguientes veces arranca en segundos.

Cuando termine, tienes tres cosas corriendo:

| Servicio | Dónde | Qué es |
|---|---|---|
| **editor** | http://localhost:3838 | El editor de preguntas, en el navegador |
| **api** | http://localhost:8000 | La API HTTP para automatizar |
| **mongo** | (interno) | La base de datos |

Comprueba que el motor está sano:

```bash
curl http://localhost:8000/health
```

Debe responder `"latex": true, "mongo": true`.

Para detenerlo: `Ctrl-C`, o desde otra terminal `docker compose down`.

---

## Cargar las preguntas la primera vez

La base arranca vacía. Para meterle tus preguntas (las de la carpeta
`questions/`), una sola vez:

```bash
docker compose run --rm seed
```

Migra las 32 preguntas a MongoDB. Ya puedes abrir el editor en
http://localhost:3838 y verlas.

Es idempotente: correrlo de nuevo no duplica nada.

---

## Los datos no se pierden

MongoDB guarda sus datos en un **volumen** con nombre (`mongo_data`), que
sobrevive a apagar y reiniciar los contenedores. Tus preguntas y corridas
siguen ahí después de `docker compose down` y `up`.

Solo se borran si lo pides explícitamente:

```bash
docker compose down -v      # -v = borra también el volumen
```

---

## Usar la API

Los mismos trabajos que la CLI, por HTTP. Por ejemplo, buscar preguntas:

```bash
curl -X POST http://localhost:8000/questions/search \
  -H 'Content-Type: application/json' \
  -d '{"store":{"type":"mongo"},"filters":{"section":"logica"}}'
```

Los endpoints están en el [README](README.md#api-http).

---

## Preguntas frecuentes

**¿Por qué la imagen es para amd64 y no para mi Mac con chip Apple?**
Se construye para amd64 a propósito: así la misma imagen corre nativa en
cualquier servidor (que es a donde iría en producción) y emulada en tu Mac. La
emulación la hace un poco más lenta, nada más. Si algún día quieres velocidad
nativa en Apple, se cambia la base del `Dockerfile` a una con soporte arm64.

**¿Puedo conectarme a la base de datos con una herramienta gráfica?**
Por seguridad, el puerto de Mongo no está expuesto fuera de Docker. Para
abrirlo, descomenta las líneas `ports` del servicio `mongo` en
`docker-compose.yml`.

**El build falla en el paso de LaTeX o Quarto.**
Es lo más frágil de un primer build. El error dice qué paquete falta; se agrega
a la lista del `Dockerfile` (sección 3) y se reconstruye. La versión de Quarto
está fijada en el `Dockerfile` (`QUARTO_VERSION`); si esa versión ya no existe,
súbela.

**Cambié el código del paquete, ¿reconstruyo todo?**
No. `docker compose up --build` reusa las capas pesadas (LaTeX, Quarto, los
paquetes de R) y solo rehace la última, que es rápida.

---

## Qué es cada archivo

| Archivo | Qué hace |
|---|---|
| `Dockerfile` | La receta de la imagen del motor: R + paquetes + LaTeX + Quarto + evaluaciones |
| `docker-compose.yml` | Orquesta los tres servicios (mongo, api, editor) y el volumen de datos |
| `.dockerignore` | Qué NO copiar a la imagen, para que el build sea liviano |
