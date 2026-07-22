#!/usr/bin/env Rscript
# ==============================================================================
# CLI de evaluaciones
# ==============================================================================
# Envoltorio delgado sobre run_job(): parsea argumentos, arma la especificacion
# y escribe el resultado como JSON. Toda la logica vive en el paquete.
#
#   evaluaciones <accion> [--spec archivo.json] [--clave valor ...]
#
# La especificacion puede venir de un archivo, de la entrada estandar, o
# armarse con banderas sueltas. Las banderas ganan sobre el archivo.
#
# Ejemplos:
#
#   evaluaciones search-questions --store.type mongo --filters.section logica
#   evaluaciones generate --spec examen.json
#   evaluaciones reproduce --run-id algebra-2025... --sink.type mongo --store.type mongo
#   echo '{"action":"list_runs","sink":{"type":"mongo"}}' | evaluaciones --spec -
# ==============================================================================

suppressMessages(library(evaluaciones))

# ------------------------------------------------------------------
# Parseo de argumentos
# ------------------------------------------------------------------

# Convierte --a.b valor en spec$a$b <- valor, de modo que la CLI puede expresar
# cualquier especificacion sin necesitar una bandera por campo.
set_path <- function(spec, ruta, valor) {
  partes <- strsplit(ruta, ".", fixed = TRUE)[[1]]

  asignar <- function(nodo, restantes) {
    if (length(restantes) == 1) {
      nodo[[restantes]] <- valor
      return(nodo)
    }
    hijo <- if (is.null(nodo[[restantes[1]]])) list() else nodo[[restantes[1]]]
    nodo[[restantes[1]]] <- asignar(hijo, restantes[-1])
    nodo
  }

  asignar(spec, partes)
}

# Los valores llegan como texto; se convierten a numero o logico cuando lo son,
# y una lista separada por comas se vuelve vector.
coerce_value <- function(x) {
  if (grepl(",", x, fixed = TRUE)) {
    return(trimws(strsplit(x, ",", fixed = TRUE)[[1]]))
  }
  if (x %in% c("true", "TRUE")) return(TRUE)
  if (x %in% c("false", "FALSE")) return(FALSE)
  if (x == "null") return(NULL)
  num <- suppressWarnings(as.numeric(x))
  if (!is.na(num) && grepl("^-?[0-9.]+$", x)) return(num)
  x
}

parse_args <- function(args) {
  spec <- list()
  accion <- NULL
  spec_file <- NULL

  i <- 1
  while (i <= length(args)) {
    a <- args[i]

    if (!startsWith(a, "--")) {
      if (is.null(accion)) accion <- gsub("-", "_", a)
      i <- i + 1
      next
    }

    clave <- sub("^--", "", a)
    if (i + 1 > length(args)) {
      stop(sprintf("La opcion --%s no trae valor.", clave), call. = FALSE)
    }
    valor <- args[i + 1]

    if (clave == "spec") {
      spec_file <- valor
    } else {
      spec <- set_path(spec, gsub("-", "_", clave), coerce_value(valor))
    }
    i <- i + 2
  }

  list(accion = accion, spec = spec, spec_file = spec_file)
}

# Mezcla dos especificaciones; la segunda gana
merge_spec <- function(base, encima) {
  for (k in names(encima)) {
    if (is.list(encima[[k]]) && is.list(base[[k]])) {
      base[[k]] <- merge_spec(base[[k]], encima[[k]])
    } else {
      base[[k]] <- encima[[k]]
    }
  }
  base
}

uso <- function() {
  cat("Uso: evaluaciones <accion> [--spec archivo.json] [--clave.subclave valor ...]\n\n")
  cat("Acciones:\n")
  for (a in job_actions()) cat(sprintf("  %s\n", gsub("_", "-", a)))
  cat("\nEjemplos:\n")
  cat("  evaluaciones search-questions --store.type local --store.base_dir questions\n")
  cat("  evaluaciones generate --spec examen.json\n")
  cat("  evaluaciones list-runs --sink.type mongo\n")
}

# ------------------------------------------------------------------
# Ejecucion
# ------------------------------------------------------------------

main <- function(args = commandArgs(trailingOnly = TRUE)) {
  if (length(args) == 0 || any(args %in% c("-h", "--help", "help"))) {
    uso()
    return(invisible(0))
  }

  parsed <- parse_args(args)

  desde_archivo <- if (!is.null(parsed$spec_file)) {
    texto <- if (parsed$spec_file == "-") {
      paste(readLines(file("stdin"), warn = FALSE), collapse = "\n")
    } else {
      paste(readLines(parsed$spec_file, warn = FALSE, encoding = "UTF-8"),
            collapse = "\n")
    }
    jsonlite::fromJSON(texto, simplifyDataFrame = FALSE)
  } else {
    list()
  }

  spec <- merge_spec(desde_archivo, parsed$spec)
  if (!is.null(parsed$accion)) {
    spec$action <- parsed$accion
  }

  resultado <- tryCatch(
    run_job(spec),
    error = function(e) {
      list(action = if (is.null(spec$action)) NA_character_ else spec$action,
           status = "error",
           message = conditionMessage(e))
    }
  )

  cat(jsonlite::toJSON(resultado, auto_unbox = TRUE, pretty = TRUE,
                       null = "null", na = "null"), "\n", sep = "")

  invisible(if (identical(resultado$status, "ok")) 0 else 1)
}

if (!interactive() && identical(environment(), globalenv())) {
  codigo <- main()
  quit(status = codigo, save = "no")
}
