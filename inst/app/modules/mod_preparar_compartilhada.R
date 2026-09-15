# Um espaço de preparo, com consulta única da base usada pelas análises.
mod_preparar_compartilhada_ui <- function(id) {
  ns <- NS(id)
  nt <- NS("tratar")
  controles <- mod_tratar_ui("tratar", separar_controles = TRUE)
  tratamento <- sprintf("input['%s'] != 'variaveis'", ns("grupo"))
  variaveis <- sprintf("input['%s'] == 'variaveis'", ns("grupo"))
  tagList(
    tags$link(rel = "stylesheet", href = "preparo.css"),
    div(class = "compartilhada-cabecalho d-flex justify-content-between align-items-center gap-3 flex-wrap mb-3",
      div(h4("Preparar Base Compartilhada", class = "mb-1"),
          p(class = "small text-muted mb-0", "Esta base alimenta as análises e dá origem a cada base derivada.")),
      div(class = "d-flex align-items-center gap-3 flex-wrap",
        uiOutput(ns("resumo")),
        downloadButton(ns("baixar_base"), "Baixar base preparada (.xlsx)", class = "btn-outline-primary"),
        downloadButton(ns("baixar_codigo"), "Baixar sequência completa (.R)", class = "btn-outline-secondary"))
    ),
    div(class = "preparo-estudio compartilhada-painel",
        uiOutput(ns("aviso")),
        navset_card_tab(height = "auto",
          wrapper = function(...) card_body(..., fill = FALSE, fillable = FALSE, min_height = "5rem"), id = ns("consulta"),
          nav_panel("Etapas do Preparo", value = "etapas", icon = icon("list-ol"),
            card_body(fill = FALSE, fillable = FALSE,
              div(class = "compartilhada-etapas",
                div(class = "compartilhada-escolhas",
                  selectizeInput(ns("grupo"), "Grupo de ações:", c("Limpeza" = "limpeza", "Cálculos e transformações" = "calculos", "Variáveis e categorias" = "variaveis"), options = list(dropdownParent = "body")),
                  conditionalPanel(condition = tratamento, controles$selecao, controles$colunas)),
                div(class = "compartilhada-parametros",
                  conditionalPanel(condition = tratamento, controles$parametros, controles$adicionar,
                    helpText("Ao adicionar, a Base Compartilhada é atualizada. Confira a aba Dados preparados.")),
                  conditionalPanel(condition = variaveis,
                    mod_organizar_variaveis_ui("organizar_variaveis", somente_controles = TRUE))),
                div(class = "compartilhada-trilha",
              h5("Etapas do Preparo", class = "mb-2"),
              div(class = "compartilhada-lista-etapas",
              uiOutput(ns("estrutura")),
              uiOutput(nt("trilha_display"))),
              selectizeInput(nt("etapa_sel"), "Etapa selecionada:", NULL, width = "100%", options = list(dropdownParent = "body", maxOptions = 10000)),
              div(class = "d-flex gap-2 flex-wrap",
                actionButton(nt("subir"), "Subir", icon = icon("arrow-up"), class = "btn-outline-secondary"),
                actionButton(nt("descer"), "Descer", icon = icon("arrow-down"), class = "btn-outline-secondary"),
                actionButton(nt("ativar"), "Ativar / desativar", class = "btn-outline-secondary"),
                actionButton(nt("remover"), "Remover", class = "btn-outline-danger"),
                actionButton(nt("limpar"), "Limpar etapas", class = "btn-outline-danger")
              ),
              helpText("Desativar preserva a etapa sem executá-la. Remover retira a etapa. A ordem mostrada é a ordem de aplicação."))))),
          nav_panel("Dados Preparados", value = "dados", icon = icon("table"),
            card_body(fill = FALSE, fillable = FALSE,
              uiOutput(ns("opcao_previa")),
              div(class = "preparo-tabela-esquerda",
                DTOutput(ns("dados"), fill = FALSE)))),
          nav_panel("Códigos R", value = "codigo", icon = icon("code"),
            card_body(fill = FALSE, fillable = FALSE,
              p(class = "small text-muted", "Sequência desde a importação até a Base Compartilhada. Guarde o arquivo original junto ao script ou ajuste o caminho indicado."),
              verbatimTextOutput(ns("codigo"))))
        )
    )
  )
}

mod_preparar_compartilhada_server <- function(id, dados_analise, replay_res, pipeline_rv, base_externa_rv, organizacao, import_info = NULL) {
  moduleServer(id, function(input, output, session) {
    grupo <- reactive(input$grupo %||% "limpeza")
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
          "Mudança ainda não adicionada à Base Compartilhada. Clique em Adicionar etapa do preparo para incorporá-la à trilha.")
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
      datatable(d, rownames = FALSE, filter = "top", width = "auto",
        class = "stripe hover compact preparo-tabela-compacta",
        # Reúne os controles reais do DT no rodapé, inclusive com o tema Bootstrap.
        # Só mudar o dom/CSS não deslocava a paginação em todas as versões do tema.
        callback = htmlwidgets::JS(
          "function alinharRodape() {",
          "  var container = $(table.table().container());",
          "  var rodape = container.find('.preparo-rodape');",
          "  if (!rodape.length) rodape = $('<div class=\"preparo-rodape\"></div>').appendTo(container);",
          "  container.find('.dataTables_info, .dt-info').appendTo(rodape);",
          "  container.find('.dataTables_paginate, .dt-paging').appendTo(rodape);",
          "}",
          "alinharRodape();",
          "table.on('draw.dt', alinharRodape);"
        ),
        options = list(scrollX = TRUE, pageLength = 10,
        dom = '<"d-flex justify-content-between flex-wrap"lf>rt<"preparo-rodape"ip>',
        lengthMenu = c(10,25,50,100), language = preparo_idioma_tabela(ncol(d))))
    }, server = TRUE)
    output$estrutura <- renderUI({
      b <- base_externa_rv()
      if (is.null(b)) return(p(class = "small text-muted", "Entrada: dados importados."))
      partes <- strsplit(paste(b$codigo_sequencial %||% b$codigo, collapse = "\n"), "\n", fixed = TRUE)[[1]]
      rotulos <- grep("^# Etapa", trimws(partes), value = TRUE)
      div(class = "border rounded p-3 mb-3",
        strong("Reestruturação incorporada — antes dos tratamentos"),
        if (length(rotulos)) tags$ol(class = "preparo-etapas preparo-etapas-finas", lapply(sub("^# Etapa [0-9]+: ?", "", rotulos), tags$li))
        else p(b$fonte %||% "Alterações de Reestruturar Planilha"),
        tags$details(tags$summary("Ver código da reestruturação"), tags$pre(b$codigo %||% "")))
    })
    codigo <- reactive({
      if (is.function(import_info)) return(preparo_codigo_completo(import_info(), pipeline_rv(), base_externa_rv()))
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
    output$baixar_codigo <- downloadHandler(filename = function() "base_compartilhada.R",
      content = function(file) writeLines(codigo(), file, useBytes = TRUE))
    output$baixar_base <- downloadHandler(filename = function() "base_compartilhada.xlsx", content = function(file) {
      validate(need(!length(replay_res()$erros), "Corrija as etapas com erro antes de baixar a base."))
      writexl::write_xlsx(as.data.frame(dados_analise()), file)
    })
    list(grupo = grupo, codigo = codigo)
  })
}
