# ANOVA com subamostras — um fator fixo e uma unidade aleatoria.
# A tela pergunta em linguagem de estudo; o motor explicita a formula do nlme.

library(shiny)
library(bslib)
library(ggplot2)

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a) || !length(a)) b else a

if (!exists("catalyser_anova_mista", mode = "function")) {
  arquivo_fonte <- file.path("..", "..", "R", "anova_mista.R")
  if (file.exists(arquivo_fonte)) {
    source(arquivo_fonte, encoding = "UTF-8")
  } else {
    catalyser_anova_mista <- function(dados, p) {
      getExportedValue("catalyser", "catalyser_anova_mista")(dados, p)
    }
  }
}
if (!exists("anova_mista_p", mode = "function")) {
  anova_mista_p <- function(p) {
    if (is.na(p)) return("não calculado")
    if (p < 0.001) "p < 0,001" else paste0("p = ", formatC(p, digits = 3, format = "f", decimal.mark = ","))
  }
}
if (!exists("anova_mista_num", mode = "function")) {
  anova_mista_num <- function(x, casas = 2L) {
    ifelse(is.na(x), "-", formatC(x, digits = casas, format = "f", decimal.mark = ","))
  }
}

anova_mista_secao <- function(texto) {
  h6(texto, style = "font-family:'Outfit';font-weight:700;color:#0F3B5F;margin-top:6px;")
}

mod_anova_mista_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(1, 1, 1),
      style = "grid-template-columns:2.7fr 6.8fr 2.5fr !important;",
      div(
        card(
          card_header("Estrutura do estudo"),
          card_body(
            selectInput(ns("resposta"), "Qual é a resposta numérica?", choices = NULL),
            selectInput(ns("fator"), "Qual fator ou condição será comparado?", choices = NULL),
            selectInput(
              ns("unidade"),
              "Qual coluna identifica a unidade que se repete, por exemplo a praia ou o tanque?",
              choices = NULL
            ),
            selectInput(
              ns("subamostra"),
              "Qual coluna identifica as medições dentro dessa unidade, por exemplo o arrasto ou o peixe?",
              choices = NULL
            ),
            uiOutput(ns("ficha_ui")),
            execucao_explicita_controles_ui(ns)
          )
        ),
        div(
          class = "alert alert-light border small",
          tags$b("Escopo desta primeira versão: "),
          "um fator fixo e um nível de agrupamento. O ajuste usa nlme, que acompanha a instalação padrão do R. Comparações múltiplas e um segundo nível de aninhamento ficam para a evolução do módulo."
        )
      ),
      execucao_explicita_resultados_ui(ns, navset_card_tab(
        nav_panel(
          "As duas análises", icon = icon("scale-balanced"),
          card_body(
            anova_mista_secao("Síntese"),
            uiOutput(ns("narrativa_ui")),
            anova_mista_secao("Lado a lado"),
            tableOutput(ns("comparacao")),
            div(
              class = "alert alert-warning py-2 small",
              tags$b("Caminho ingênuo, somente para aprender: "),
              "a terceira linha trata cada subamostra como réplica. Ela não deve orientar a conclusão."
            ),
            anova_mista_secao("Resumo no nível da unidade"),
            tableOutput(ns("descritivos"))
          )
        ),
        nav_panel(
          "Gráfico", icon = icon("chart-column"),
          card_body(plotOutput(ns("grafico"), height = "470px"))
        ),
        nav_panel(
          "Pressupostos", icon = icon("circle-check"),
          card_body(
            tableOutput(ns("pressupostos")),
            helpText(
              "As frases acompanham o resultado efetivamente calculado. Um p-valor alto não comprova um pressuposto; a independência continua sendo decisão do delineamento.",
              style = "font-size:.84rem;"
            ),
            plotOutput(ns("diagnosticos"), height = "410px")
          )
        ),
        nav_panel(
          "Código R", icon = icon("code"),
          card_body(
            anova_mista_secao("Fórmula central"),
            verbatimTextOutput(ns("codigo")),
            div(
              class = "alert alert-info py-2 small",
              "A subamostra permanece nas linhas. A unidade entra como intercepto aleatório e impede que essas linhas sejam contadas como réplicas independentes."
            )
          )
        ),
        nav_panel(
          "Console R", icon = icon("terminal"),
          card_body(verbatimTextOutput(ns("console")))
        )
      )),
      card(
        card_header("Leitura do delineamento"),
        card_body(
          uiOutput(ns("estrutura_ui")),
          div(
            class = "alert alert-light border small mb-0",
            tags$b("Por que mostrar as duas? "),
            "A média por unidade oferece o caminho simples. O modelo misto mantém as subamostras. Quando ambos respeitam a unidade, espera-se que contem a mesma história, embora os números não precisem ser idênticos."
          )
        )
      )
    )
  )
}

mod_anova_mista_server <- function(id, dados_rv, ficha_rv = NULL) {
  moduleServer(id, function(input, output, session) {
    revisao_execucao <- execucao_revisao_dados(dados_rv)
    gatilho_execucao <- reactiveVal(0L)

    escolher <- function(atual, candidatos, padrao) {
      candidatos <- intersect(candidatos, padrao)
      if (!is.null(atual) && atual %in% padrao) atual else if (length(candidatos)) candidatos[1] else padrao[1]
    }

    observe({
      dados <- dados_rv()
      req(dados)
      colunas <- names(dados)
      numericas <- colunas[vapply(dados, is.numeric, logical(1))]
      categoricas <- colunas[!vapply(dados, is.numeric, logical(1)) |
        vapply(dados, function(x) length(unique(x[!is.na(x)])) <= 30L, logical(1))]
      if (!length(numericas)) numericas <- colunas
      if (!length(categoricas)) categoricas <- colunas

      ficha <- if (is.function(ficha_rv)) ficha_rv() else NULL
      unidade_ficha <- as.character(ficha$unidade_coluna %||% "")
      subamostra_ficha <- ficha_subamostra_coluna(ficha)
      fator_ficha <- ficha_fator_coluna(ficha)
      resposta_ficha <- as.character(ficha$resposta_coluna %||% "")

      resposta <- escolher(isolate(input$resposta), resposta_ficha, numericas)
      fator <- escolher(isolate(input$fator), fator_ficha, categoricas)
      unidade <- escolher(
        isolate(input$unidade),
        c(unidade_ficha, "praia", "tanque", "sitio", "sítio", "UE", "id_unidade"),
        categoricas
      )
      subamostra <- escolher(
        isolate(input$subamostra),
        c(subamostra_ficha, "arrasto", "peixe", "subamostra", "id_subunidade"),
        categoricas
      )
      updateSelectInput(session, "resposta", choices = numericas, selected = resposta)
      updateSelectInput(session, "fator", choices = categoricas, selected = fator)
      updateSelectInput(session, "unidade", choices = categoricas, selected = unidade)
      updateSelectInput(session, "subamostra", choices = categoricas, selected = subamostra)
    })

    assinatura <- reactive({
      req(input$resposta, input$fator, input$unidade, input$subamostra)
      execucao_assinatura(
        input, c("resposta", "fator", "unidade", "subamostra"), revisao_execucao()
      )
    })

    resultado <- eventReactive(gatilho_execucao(), {
      dados <- dados_rv()
      req(dados)
      parametros <- list(
        resposta = input$resposta,
        fator = input$fator,
        unidade = input$unidade,
        subamostra = input$subamostra,
        nivel_confianca = 0.95
      )
      catalyser_anova_mista(dados, parametros)
    }, ignoreInit = FALSE)

    exec_ctrl <- execucao_explicita_server(
      input, output, session, assinatura, resultado,
      nome_analise = "A ANOVA com subamostras", gatilho_rv = gatilho_execucao
    )

    output$ficha_ui <- renderUI({
      ficha <- if (is.function(ficha_rv)) ficha_rv() else NULL
      ficha_aviso_analise_ui(ficha, "anova_mista_subamostras")
    })

    output$estrutura_ui <- renderUI({
      req(input$unidade, input$subamostra)
      div(
        class = "small",
        tags$p(tags$b("Unidade independente: "), input$unidade),
        tags$p(tags$b("Medição dentro da unidade: "), input$subamostra),
        tags$p(tags$b("Modelo: "), tags$code(sprintf("%s ~ %s, random = ~1 | %s", input$resposta, input$fator, input$unidade)))
      )
    })

    output$narrativa_ui <- renderUI({ div(class = "alert alert-secondary", resultado()$narrativa) })
    output$comparacao <- renderTable({
      x <- resultado()$tabela
      x$F <- anova_mista_num(x$F)
      x$`p-valor` <- vapply(x$`p-valor`, anova_mista_p, character(1))
      x
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
    output$descritivos <- renderTable({
      x <- resultado()$descritivos
      x[["Média das unidades"]] <- anova_mista_num(x[["Média das unidades"]])
      x[["DP entre unidades"]] <- anova_mista_num(x[["DP entre unidades"]])
      x
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
    output$pressupostos <- renderTable({ resultado()$pressupostos }, striped = TRUE, hover = TRUE, bordered = TRUE)
    output$grafico <- renderPlot({ resultado()$grafico })
    output$diagnosticos <- renderPlot({ resultado()$diagnosticos })
    output$codigo <- renderText({ resultado()$codigo })
    output$console <- renderText({ paste(resultado()$console, collapse = "\n") })

    estado_execucao <- reactive({
      req(exec_ctrl$atualizada())
      r <- resultado()
      list(
        analise_id = "anova_mista",
        tipo = "anova_mista_subamostras",
        titulo = sprintf("%s entre níveis de %s, com %s como unidade", input$resposta, input$fator, input$unidade),
        parametros = list(
          resposta = input$resposta,
          fator = input$fator,
          unidade = input$unidade,
          subamostra = input$subamostra,
          nivel_confianca = 0.95
        ),
        saidas_disponiveis = c("narrativa", "descritivos", "tabela", "grafico", "pressupostos", "diagnosticos"),
        resultado_resumo = list(
          n = as.integer(r$n), n_unidades = as.integer(r$n_unidades),
          f = unname(r$f_misto), gl_1 = unname(r$gl_1), gl_2 = unname(r$gl_2),
          p = unname(r$p_misto), icc = unname(r$icc), excluidos = as.integer(r$excluidos)
        ),
        codigo_r = r$codigo
      )
    })

    invisible(list(estado_execucao = estado_execucao, resultado = resultado, exec_ctrl = exec_ctrl))
  })
}
