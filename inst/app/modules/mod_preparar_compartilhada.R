# Um espaço de preparo, com consulta única da base usada pelas análises.
mod_preparar_compartilhada_ui <- function(id) {
  ns <- NS(id)
  nt <- NS("tratar")
  tagList(
    tags$style(HTML(".preparo-estudio {display:grid;grid-template-columns:minmax(270px,310px) minmax(0,1fr);gap:16px;align-items:start}.preparo-estudio > * {min-width:0}.preparo-estudio .card {height:auto}.preparo-estudio .btn {white-space:normal}.preparo-estudio pre {white-space:pre-wrap;overflow-wrap:anywhere}@media(max-width:900px){.preparo-estudio{grid-template-columns:1fr}}")),
    div(class = "d-flex justify-content-between align-items-center gap-3 flex-wrap mb-3",
      div(h4("Preparar Base Compartilhada", class = "mb-1"),
          p(class = "small text-muted mb-0", "Esta base alimenta as análises e dá origem a cada base derivada.")),
      uiOutput(ns("resumo"))
    ),
    div(class = "preparo-estudio",
      div(
        card(fill = FALSE, card_header("Escolher grupo de ações"),
          card_body(fill = FALSE, fillable = FALSE,
            selectInput(ns("grupo"), NULL, c("Variáveis e categorias" = "variaveis", "Cálculos e transformações" = "calculos", "Limpeza" = "limpeza")),
            conditionalPanel(condition = sprintf("input['%s'] == 'variaveis'", ns("grupo")),
              mod_organizar_variaveis_ui("organizar_variaveis", somente_controles = TRUE))
          )),
        conditionalPanel(condition = sprintf("input['%s'] != 'variaveis'", ns("grupo")),
          mod_tratar_ui("tratar", somente_controles = TRUE))
      ),
      div(
        uiOutput(ns("aviso")),
        navset_card_tab(height = "auto",
          wrapper = function(...) card_body(..., fill = FALSE, fillable = FALSE, min_height = "5rem"), id = ns("consulta"),
          nav_panel("Dados preparados", value = "dados", icon = icon("table"),
            card_body(fill = FALSE, fillable = FALSE,
              uiOutput(ns("opcao_previa")),
              DTOutput(ns("dados"), fill = FALSE),
              div(class = "d-flex justify-content-end mt-3", downloadButton(ns("baixar_base"), "Baixar base preparada (.xlsx)", class = "btn-outline-primary")))),
          nav_panel("Etapas do Preparo", value = "etapas", icon = icon("list-ol"),
            card_body(fill = FALSE, fillable = FALSE,
              uiOutput(ns("estrutura")),
              uiOutput(nt("trilha_display")),
              selectInput(nt("etapa_sel"), "Etapa selecionada:", NULL),
              div(class = "d-flex gap-2 flex-wrap",
                actionButton(nt("subir"), "Subir", icon = icon("arrow-up"), class = "btn-outline-secondary"),
                actionButton(nt("descer"), "Descer", icon = icon("arrow-down"), class = "btn-outline-secondary"),
                actionButton(nt("ativar"), "Ativar / desativar", class = "btn-outline-secondary"),
                actionButton(nt("remover"), "Remover", class = "btn-outline-danger"),
                actionButton(nt("limpar"), "Limpar etapas", class = "btn-outline-danger")
              ),
              helpText("As etapas abaixo da estrutura inicial seguem a ordem mostrada. Se uma alteração invalidar outra etapa, o aviso identifica o que precisa ser corrigido."))),
          nav_panel("Código R", value = "codigo", icon = icon("code"),
            card_body(fill = FALSE, fillable = FALSE,
              p(class = "small text-muted", "O trecho abaixo parte da base de entrada, chamada dados. O Projeto R exportado inclui também a importação e o registro da reestruturação."),
              tags$pre(verbatimTextOutput(ns("codigo"))),
              downloadButton(ns("baixar_codigo"), "Baixar etapas (.R)", class = "btn-outline-secondary")))
        )
      )
    )
  )
}

mod_preparar_compartilhada_server <- function(id, dados_analise, replay_res, pipeline_rv, base_externa_rv, organizacao) {
  moduleServer(id, function(input, output, session) {
    grupo <- reactive(input$grupo %||% "variaveis")
    output$resumo <- renderUI({
      d <- dados_analise(); req(d)
      span(class = "badge text-bg-light border p-2", sprintf("%d linhas · %d variáveis · %d etapas", nrow(d), ncol(d), length(pipeline_rv())))
    })
    output$aviso <- renderUI({
      erros <- replay_res()$erros
      tagList(
        if (length(erros)) div(class = "alert alert-danger",
          strong("Há etapas com erro. "), paste(paste0("Etapa ", names(erros), ": ", unlist(erros)), collapse = " · "),
          " Confira Etapas do Preparo antes de continuar."),
        if (organizacao$pendente()) div(class = "alert alert-info",
          "Há ajustes de variáveis na prévia. Clique em Adicionar etapa do preparo para usá-los nas análises.")
      )
    })
    output$opcao_previa <- renderUI({
      if (organizacao$pendente()) radioButtons(session$ns("ver_dados"), "Conferir:",
        c("Base usada nas análises" = "atual", "Prévia dos ajustes pendentes" = "previa"),
        selected = isolate(input$ver_dados %||% "previa"), inline = TRUE)
    })
    output$dados <- renderDT({
      d <- if (organizacao$pendente() && identical(input$ver_dados, "previa")) organizacao$previa() else dados_analise()
      req(d)
      datatable(d, rownames = FALSE, filter = "top", options = list(scrollX = TRUE, pageLength = 10,
        lengthMenu = c(10,25,50,100)))
    }, server = TRUE)
    output$estrutura <- renderUI({
      b <- base_externa_rv()
      if (is.null(b)) return(p(class = "small text-muted", "Entrada: dados importados."))
      tags$details(class = "border rounded p-3 mb-3",
        tags$summary("Estrutura inicial — alterações de Reestruturar Planilha"),
        p(class = "small text-muted mt-2", "Estas alterações antecedem as etapas abaixo. A ordem interna foi preservada."),
        tags$pre(b$codigo %||% ""))
    })
    codigo <- reactive({
      linhas <- c("# Etapas do preparo da Base Compartilhada",
        "# Antes de executar: dados deve conter a base importada e reestruturada.")
      for (et in pipeline_rv()) if (isTRUE(et$ativa)) {
        tr <- tratamentos[[et$tipo]]
        linhas <- c(linhas, "", paste0("# ", tr$rotulo(et$params)), tr$codigo(et$params))
      }
      if (any(grepl("trat_moda", linhas, fixed = TRUE))) linhas <- c(
        "trat_moda <- function(x) { valores <- unique(x[!is.na(x)]); if (!length(valores)) return(NA); valores[which.max(tabulate(match(x, valores)))] }", linhas)
      paste(c(linhas, "", "dados_analise <- dados"), collapse = "\n")
    })
    output$codigo <- renderText(codigo())
    output$baixar_codigo <- downloadHandler(filename = function() "etapas_preparo.R",
      content = function(file) writeLines(codigo(), file, useBytes = TRUE))
    output$baixar_base <- downloadHandler(filename = function() "base_compartilhada.xlsx", content = function(file) {
      validate(need(!length(replay_res()$erros), "Corrija as etapas com erro antes de baixar a base."))
      writexl::write_xlsx(as.data.frame(dados_analise()), file)
    })
    list(grupo = grupo)
  })
}
