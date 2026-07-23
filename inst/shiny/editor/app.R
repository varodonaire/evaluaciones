# ==============================================================================
# EDITOR DE PREGUNTAS — app Shiny
# ==============================================================================
# Se lanza con evaluaciones::run_question_editor(store). Llama al store en el
# mismo proceso: listar, previsualizar, editar y versionar preguntas.
# ==============================================================================

library(shiny)
library(evaluaciones)

store <- evaluaciones:::editor_store()

# Los ejes se leen una vez al arrancar: rara vez cambian durante una sesion, y
# fijarlos aqui evita el updateSelectInput dinamico, cuya actualizacion sobre un
# select recien creado disparaba un error de cliente que abortaba el render.
ejes_iniciales <- tryCatch({
  todas <- qs_search(store, status = NULL)
  sort(unique(stats::na.omit(todas$section)))
}, error = function(e) character(0))

# Construye el HTML de una vista previa a partir de preview_question()
preview_html <- function(preview) {
  if (is.null(preview)) {
    return(tags$p(class = "text-muted", "Sin vista previa."))
  }
  letras <- letters[seq_along(preview$Alternativas)]
  correcta <- preview$Correcta

  alternativas <- Map(function(letra, texto) {
    es_correcta <- !is.na(correcta) && letra == correcta
    tags$li(
      style = if (es_correcta) "font-weight:600; color:#1a7f37;" else "",
      HTML(paste0(letra, ") ", texto, if (es_correcta) "  ✓" else ""))
    )
  }, letras, preview$Alternativas)

  withMathJax(tagList(
    tags$p(HTML(preview$Texto)),
    tags$ol(type = "a", alternativas)
  ))
}

ui <- bslib::page_sidebar(
  title = "Editor de preguntas",
  sidebar = bslib::sidebar(
    width = 340,
    selectInput("eje", "Eje", choices = c("Todos" = "__all__", ejes_iniciales)),
    selectInput("estado", "Estado",
                choices = c("Publicadas" = "published", "Borradores" = "draft",
                            "Retiradas" = "retired", "Todas" = "__all__"),
                selected = "published"),
    actionButton("nueva", "Nueva pregunta", class = "btn-primary w-100"),
    hr(),
    DT::DTOutput("tabla")
  ),
  bslib::navset_card_tab(
    bslib::nav_panel(
      "Editar",
      textOutput("slug_actual"),
      textAreaInput("fuente", NULL, width = "100%", height = "340px",
                    resize = "vertical"),
      div(
        class = "d-flex gap-2 mb-3",
        numericInput("seed", "Semilla", value = 1, width = "110px"),
        actionButton("previsualizar", "Vista previa", class = "btn-outline-primary"),
        actionButton("validar", "Validar", class = "btn-outline-secondary"),
        actionButton("guardar", "Guardar versión", class = "btn-success ms-auto")
      ),
      uiOutput("estado_validacion")
    ),
    bslib::nav_panel(
      "Vista previa",
      uiOutput("vista_previa")
    ),
    bslib::nav_panel(
      "Historial",
      DT::DTOutput("historial")
    )
  )
)

server <- function(input, output, session) {

  rv <- reactiveValues(
    seleccion = NULL,   # question_id seleccionado
    preview = NULL,
    recargar = 0
  )

  # --- Catalogo de preguntas, segun filtros ---
  preguntas <- reactive({
    rv$recargar
    estado <- if (identical(input$estado, "__all__")) NULL else input$estado
    seccion <- if (identical(input$eje, "__all__")) NULL else input$eje
    qs_search(store, section = seccion, status = estado)
  })

  # fillContainer = FALSE: dentro de un contenedor fill de bslib (el sidebar) DT
  # colapsa a altura cero y se muestra vacio. Renderizarlo a su altura natural
  # lo evita.
  output$tabla <- DT::renderDT({
    df <- preguntas()[, c("slug", "version", "section", "status")]
    DT::datatable(df, selection = "single", rownames = FALSE,
                  fillContainer = FALSE,
                  options = list(pageLength = 10, dom = "tp"),
                  colnames = c("Pregunta", "v", "Eje", "Estado"))
  })

  # --- Seleccion de una pregunta ---
  observeEvent(input$tabla_rows_selected, {
    fila <- input$tabla_rows_selected
    req(fila)
    id <- preguntas()$question_id[fila]
    rv$seleccion <- id

    pregunta <- qs_get(store, id)
    updateTextAreaInput(session, "fuente", value = pregunta$source[1])
    rv$preview <- NULL
    output$estado_validacion <- renderUI(NULL)
  })

  output$slug_actual <- renderText({
    if (is.null(rv$seleccion)) "Nueva pregunta" else paste("Editando:", rv$seleccion)
  })

  # --- Nueva pregunta ---
  observeEvent(input$nueva, {
    rv$seleccion <- NULL
    plantilla <- paste(
      c("<<echo=FALSE, results=hide>>=",
        "# Codigo R que genera la pregunta",
        "@",
        "",
        "\\begin{question}",
        "Enunciado de la pregunta.",
        "<<echo=FALSE, results=tex>>=",
        "exams::answerlist(c(\"Correcta.\", \"Distractor 1.\", \"Distractor 2.\"))",
        "@",
        "\\end{question}",
        "",
        "%% \\extype{schoice}",
        "%% \\exname{NuevaPregunta_1}",
        "%% \\exsection{sin_eje}",
        "%% \\exsolution{100}"),
      collapse = "\n")
    updateTextAreaInput(session, "fuente", value = plantilla)
    rv$preview <- NULL
  })

  # --- Vista previa ---
  observeEvent(input$previsualizar, {
    req(input$fuente)
    rv$preview <- tryCatch(
      preview_question(input$fuente, seed = input$seed),
      error = function(e) {
        showNotification(paste("No se pudo previsualizar:", conditionMessage(e)),
                         type = "error")
        NULL
      }
    )
    bslib::nav_select(id = NULL, selected = "Vista previa")
  })

  output$vista_previa <- renderUI(preview_html(rv$preview))

  # --- Validar ---
  observeEvent(input$validar, {
    req(input$fuente)
    v <- validate_question(input$fuente, seed = input$seed)
    rv$preview <- v$preview
    output$estado_validacion <- renderUI({
      div(class = if (v$valid) "alert alert-success" else "alert alert-danger",
          v$message)
    })
  })

  # --- Guardar ---
  observeEvent(input$guardar, {
    req(input$fuente)
    v <- validate_question(input$fuente, seed = input$seed)
    if (!v$valid) {
      showNotification(paste("No se guarda:", v$message), type = "error", duration = 8)
      return()
    }
    tryCatch({
      id <- qs_put(store, input$fuente)
      showNotification(paste("Guardada:", id), type = "message")
      rv$recargar <- rv$recargar + 1
    }, error = function(e) {
      showNotification(paste("Error al guardar:", conditionMessage(e)), type = "error")
    })
  })

  # --- Historial de versiones ---
  output$historial <- DT::renderDT({
    req(rv$seleccion)
    slug <- qs_get(store, rv$seleccion)$slug[1]
    versiones <- qs_search(store, slug = slug, status = NULL)
    DT::datatable(versiones[, c("question_id", "version", "status", "content_hash")],
                  rownames = FALSE, options = list(dom = "t"),
                  colnames = c("ID", "Versión", "Estado", "Hash"))
  })
}

shinyApp(ui, server)
