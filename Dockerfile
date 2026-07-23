# ==============================================================================
# Imagen del motor de evaluaciones
# ==============================================================================
# Congela todo el entorno —R, los paquetes, LaTeX, Quarto y el paquete
# evaluaciones— en un solo artefacto que corre igual en cualquier maquina.
#
# La misma imagen sirve para tres cosas, segun el comando con que se lance
# (ver docker-compose.yml): la API HTTP, el editor de preguntas, y trabajos
# sueltos por linea de comandos.
#
# Se parte de rocker/verse, que ya trae R, el tidyverse, TinyTeX (LaTeX),
# Quarto y pandoc compilados, para no reconstruir esas dependencias pesadas.
# ==============================================================================

FROM rocker/verse:4.4.2

# --- 1. Dependencias del sistema -------------------------------------------
# La base ya trae libssl, libcurl, libxml2 y curl. Se agregan las que faltan:
# SASL para mongolite y sodium para plumber.
RUN apt-get update && apt-get install -y --no-install-recommends \
      libsasl2-dev \
      libsodium-dev \
      fontconfig \
    && rm -rf /var/lib/apt/lists/*

# --- 2. Paquetes de R ------------------------------------------------------
# Antes de copiar el codigo del paquete, para que un cambio en el codigo no
# invalide esta capa (que es la que tarda).
RUN install2.r --error --skipinstalled \
      mongolite \
      plumber \
      shiny \
      DT \
      bslib \
      writexl \
      readxl \
      digest \
      jsonlite \
      rlang \
      gt \
      showtext \
      quarto \
    && rm -rf /tmp/downloaded_packages

# exams desde el CRAN al dia: el snapshot de rocker trae 2.4-1, que tiene un
# bug ("the condition has length > 1") al tejer .Rnw, arreglado en 2.4-3.
RUN R -e 'install.packages("exams", repos = "https://cloud.r-project.org")'

# --- 3. Paquetes de LaTeX --------------------------------------------------
# TinyTeX ya esta; se agregan los que faltan. La lista sale del preambulo del
# template 'plain' de exams (a4wide, xcolor, eurosym...) y de lo que usan los
# informes (xelatex + fontspec + fuentes Libertinus). tlmgr actualiza solo la
# base de archivos de TeX tras instalar.
RUN tlmgr install \
      a4wide ntgclass eurosym fancyvrb xcolor amsfonts \
      siunitx csquotes enumitem \
      fontspec unicode-math xetex \
      libertinus-fonts libertinus-type1 libertinus-otf \
      koma-script caption booktabs colortbl \
    && tlmgr path add \
    && mktexlsr

# Hacer visibles a fontconfig las fuentes Libertinus, para que xelatex las
# encuentre por nombre en los informes. Best-effort: si la ruta cambia, no
# rompe el build.
RUN mkdir -p /usr/share/fonts/opentype/libertinus \
    && find / -iname "libertinus*.otf" -exec cp {} /usr/share/fonts/opentype/libertinus/ \; 2>/dev/null || true \
    && fc-cache -f

# --- 4. El paquete evaluaciones --------------------------------------------
WORKDIR /app
COPY . /app
RUN R CMD INSTALL --no-multiarch --with-keep.source .

# Dentro del contenedor, MongoDB vive en el host "mongo" (el servicio de
# compose), no en localhost.
ENV EVALUACIONES_MONGO_URL="mongodb://mongo:27017"
ENV EVALUACIONES_MONGO_DB="evaluaciones"

# Por defecto se levanta la API; compose sobrescribe el comando por servicio.
EXPOSE 8000 3838
CMD ["R", "-e", "evaluaciones::run_api(port = 8000)"]
