# Módulo: ANOVA de dois fatores (fatorial com interação).
# O módulo segue o mesmo contrato da ANOVA de um fator:
# Base Compartilhada/Base Derivada -> Executar -> Registrar -> Comunicação.

library(shiny)
library(bslib)
library(ggplot2)

if (file.exists("templates/funcoes_anova_dois_fatores.R")) {
  source("templates/funcoes_anova_dois_fatores.R", encoding = "UTF-8")
}

anova2_titulo_secao <- function(texto) {
  h6(texto, style = "font-family: 'Outfit'; font-weight: 700; color: #0F3B5F; margin-top: 6px;")
}

mod_anova_dois_fatores_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(3, 7, 2),
      card(
        card_header("Configuração do modelo"),
        card_body(
          selectInput(ns("var_y"), "Variável resposta (Y — numérica):", choices = NULL),
          selectInput(ns("var_a"), "Fator A:", choices = NULL),
          selectInput(ns("var_b"), "Fator B:", choices = NULL),
          sliderInput(ns("conf_level"), "Nível de confiança (%):", min = 80, max = 99,
                      value = 95, step = 1),
          execucao_explicita_controles_ui(ns)
        )
      ),
      execucao_explicita_resultados_ui(ns, navset_card_tab(
        id = ns("active_tab"),
        title = "Painel da ANOVA fatorial",
        nav_panel(
          title = "Resultado principal", icon = icon("square-poll-vertical"),
          card_body(uiOutput(ns("principal_ui")))
        ),
        nav_panel(
          title = "Tabela e comparações", icon = icon("table"),
          card_body(uiOutput(ns("tabela_ui")))
        ),
        nav_panel(
          title = "Pressupostos", icon = icon("circle-check"),
          card_body(uiOutput(ns("pressupostos_ui")))
        ),
        nav_panel(
          title = "Console R", icon = icon("terminal"),
          card_body(verbatimTextOutput(ns("console_bruto")))
        )
      )),
      card(
        card_header("Exibição"),
        card_body(
          textInput(ns("custom_title"), "Título do gráfico:", value = ""),
          textInput(ns("custom_label_x"), "Rótulo do eixo X:", value = ""),
          textInput(ns("custom_label_y"), "Rótulo do eixo Y:", value = ""),
          selectInput(ns("graph_theme"), "Tema:",
                      choices = c("Mínimo" = "minimal", "Clássico" = "classic",
                                  "Preto e Branco" = "bw", "Cinza" = "gray",
                                  "Light" = "light"), selected = "minimal"),
          helpText("A interação é lida pelo cruzamento das linhas: linhas não paralelas sugerem que o efeito de um fator depende do outro.",
                   style = "font-size: 0.8rem;")
        )
      )
    )
  )
}

mod_anova_dois_fatores_server <- function(id, data_rv, import_info) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    revisao_execucao <- execucao_revisao_dados(data_rv)
    gatilho_execucao <- reactiveVal(0L)

    observe({
      df <- data_rv()
      req(df)
      cols <- names(df)
      num_cols <- cols[vapply(df, is.numeric, logical(1))]
      cat_cols <- cols[
        !vapply(df, is.numeric, logical(1)) |
          vapply(df, function(x) length(unique(x[!is.na(x)])) < 15L, logical(1))
      ]
      if (!length(num_cols)) num_cols <- cols
      if (length(cat_cols) < 2L) cat_cols <- cols
      y <- isolate(input$var_y)
      a <- isolate(input$var_a)
      b <- isolate(input$var_b)
      if (is.null(y) || !y %in% num_cols) y <- num_cols[1]
      if (is.null(a) || !a %in% cat_cols || identical(a, y)) {
        a <- setdiff(cat_cols, y)[1] %||% cat_cols[1]
      }
      if (is.null(b) || !b %in% cat_cols || identical(b, a) || identical(b, y)) {
        b <- setdiff(cat_cols, c(y, a))[1] %||% cat_cols[1]
      }
      updateSelectInput(session, "var_y", choices = num_cols, selected = y)
      updateSelectInput(session, "var_a", choices = cat_cols, selected = a)
      updateSelectInput(session, "var_b", choices = cat_cols, selected = b)
    })

    nivel_confianca <- reactive({
      valor <- suppressWarnings(as.numeric(input$conf_level))
      if (!length(valor) || is.na(valor)) valor <- 95
      valor / 100
    })

    assinatura_execucao <- reactive({
      req(input$var_y, input$var_a, input$var_b)
      execucao_assinatura(
        input,
        c("var_y", "var_a", "var_b", "conf_level", "graph_theme",
          "custom_title", "custom_label_x", "custom_label_y"),
        revisao_execucao()
      )
    })

    result_rv <- eventReactive(gatilho_execucao(), {
      df <- data_rv()
      req(df, input$var_y, input$var_a, input$var_b)
      mensagem <- anova2_validar_entrada(df, input$var_y, input$var_a, input$var_b)
      if (!is.null(mensagem)) stop(mensagem, call. = FALSE)
      calcular_anova_dois_fatores(
        df, input$var_y, input$var_a, input$var_b,
        nivel_confianca = nivel_confianca()
      )
    }, ignoreInit = FALSE)

    exec_ctrl <- execucao_explicita_server(
      input, output, session, assinatura_execucao, result_rv,
      nome_analise = "A ANOVA de dois fatores",
      gatilho_rv = gatilho_execucao
    )

    output$principal_ui <- renderUI({
      r <- result_rv(); req(r)
      tagList(
        anova2_titulo_secao("Narrativa automática"),
        div(class = "alert alert-secondary", style = "font-size: 0.9rem; line-height: 1.45;",
            relatar_anova_dois_fatores(r)),
        if (!isTRUE(r$delineamento_balanceado)) div(
          class = "alert alert-warning py-2 small",
          "Delineamento desequilibrado: confira os tamanhos de cada célula e registre essa limitação no relatório."
        ),
        if (r$excluidos > 0) div(
          class = "alert alert-warning py-2 small",
          sprintf("%d linha(s) foram excluídas por dados faltantes. Restaram %d observações.",
                  r$excluidos, r$n)
        ),
        hr(),
        anova2_titulo_secao("Médias por célula"),
        tableOutput(ns("celulas_table")),
        hr(),
        anova2_titulo_secao("Gráfico de interação"),
        plotOutput(ns("interacao_plot"), height = "430px")
      )
    })

    output$celulas_table <- renderTable({
      r <- result_rv(); req(r)
      arrumar_celulas_anova_dois_fatores(r)
    }, striped = TRUE, hover = TRUE, bordered = TRUE)

    output$interacao_plot <- renderPlot({
      r <- result_rv(); req(r)
      grafico_anova_dois_fatores(
        r,
        titulo = if (nzchar(input$custom_title %||% "")) input$custom_title else NULL,
        rotulo_x = if (nzchar(input$custom_label_x %||% "")) input$custom_label_x else NULL,
        rotulo_y = if (nzchar(input$custom_label_y %||% "")) input$custom_label_y else NULL,
        tema = input$graph_theme %||% "minimal"
      )
    })

    output$tabela_ui <- renderUI({
      r <- result_rv(); req(r)
      tagList(
        anova2_titulo_secao("Tabela da ANOVA"),
        tableOutput(ns("anova_table")),
        anova2_titulo_secao("Tamanhos de efeito"),
        tableOutput(ns("efeito_table")),
        anova2_titulo_secao("Comparações entre células (Tukey)"),
        tableOutput(ns("comparacoes_table"))
      )
    })
    output$anova_table <- renderTable({
      r <- result_rv(); req(r)
      arrumar_tabela_anova_dois_fatores(r)
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
    output$efeito_table <- renderTable({
      r <- result_rv(); req(r)
      arrumar_efeito_anova_dois_fatores(r)
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
    output$comparacoes_table <- renderTable({
      r <- result_rv(); req(r)
      arrumar_comparacoes_anova_dois_fatores(r)
    }, striped = TRUE, hover = TRUE, bordered = TRUE)

    output$pressupostos_ui <- renderUI({
      r <- result_rv(); req(r)
      tagList(
        anova2_titulo_secao("Testes de pressupostos"),
        tableOutput(ns("pressupostos_table")),
        helpText("Leia os testes junto com os gráficos de resíduos; p-valor alto não prova que o pressuposto é verdadeiro.",
                 style = "font-size: 0.82rem;"),
        hr(),
        layout_columns(
          plotOutput(ns("resid_plot"), height = "360px"),
          plotOutput(ns("qq_plot"), height = "360px")
        )
      )
    })
    output$pressupostos_table <- renderTable({
      r <- result_rv(); req(r)
      arrumar_pressupostos_anova_dois_fatores(r)
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
    output$resid_plot <- renderPlot({ grafico_diagnosticos_anova_dois_fatores(result_rv(), "residuos") })
    output$qq_plot <- renderPlot({ grafico_diagnosticos_anova_dois_fatores(result_rv(), "qq") })
    output$console_bruto <- renderText({
      r <- result_rv(); req(r)
      paste(r$console, collapse = "\n")
    })

    estado_execucao <- reactive({
      req(exec_ctrl$atualizada())
      r <- result_rv(); req(r)
      list(
        analise_id = "anova_dois_fatores",
        tipo = "anova_dois_fatores",
        titulo = if (nzchar(input$custom_title %||% "")) input$custom_title else
          sprintf("ANOVA de dois fatores: %s por %s e %s", r$dep_var, r$fator_a, r$fator_b),
        parametros = list(
          resposta = r$dep_var,
          fator_a = r$fator_a,
          fator_b = r$fator_b,
          nivel_confianca = r$nivel_confianca,
          tema = input$graph_theme %||% "minimal",
          titulo_grafico = input$custom_title %||% "",
          rotulo_x = input$custom_label_x %||% "",
          rotulo_y = input$custom_label_y %||% ""
        ),
        saidas_disponiveis = c("narrativa", "celulas", "tabela", "efeito",
                               "comparacoes", "grafico", "pressupostos", "diagnosticos"),
        resultado_resumo = list(
          n = as.integer(r$n), excluidos = as.integer(r$excluidos),
          gl_a = as.integer(r$df_a), gl_b = as.integer(r$df_b),
          gl_interacao = as.integer(r$df_interacao),
          p_a = unname(r$p_a), p_b = unname(r$p_b),
          p_interacao = unname(r$p_interacao)
        )
      )
    })

    invisible(list(
      resultado = result_rv,
      estado_execucao = estado_execucao,
      estado_execucao_ui = exec_ctrl$estado,
      execucao_atualizada = exec_ctrl$atualizada
    ))
  })
}
