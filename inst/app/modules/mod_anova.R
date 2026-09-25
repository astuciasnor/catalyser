# Módulo de ANOVA de um Fator — V16
# -----------------------------------------------------------------------------
# A ANOVA passou a usar o mesmo contrato das demais análises homologadas:
#   Base Compartilhada ou Base Derivada -> Executar análise -> Adicionar aos
#   resultados -> Comunicação -> Projeto R -> Render no RStudio (HTML e Word).
# O cálculo, a arrumação e a narrativa vivem em templates/funcoes_anova.R
# (fonte canônica). Este módulo é só interface e estado.

library(shiny)
library(bslib)
library(ggplot2)
library(DT)

if (file.exists("templates/funcoes_anova.R")) {
  source("templates/funcoes_anova.R")
}

anova_titulo_secao <- function(texto) {
  h6(texto, style = "font-family: 'Outfit'; font-weight: 700; color: #0F3B5F; margin-top: 6px;")
}

mod_anova_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(1, 1, 1),
      style = "grid-template-columns: 2.5fr 7fr 2.5fr !important;",

      # COLUNA 1: CONFIGURAÇÃO DO MODELO
      div(
        card(
          card_header("Configuração da ANOVA"),
          card_body(
            style = "padding: 12px 15px;",
            uiOutput(ns("aviso_ficha")),
            selectInput(ns("var_y"), "Variável resposta (Y — numérica):", choices = NULL),
            selectInput(ns("var_x"), "Fator / grupo (X — categórico):", choices = NULL),
            sliderInput(ns("conf_level"), "Nível de confiança (%):",
                        min = 80, max = 99, value = 95, step = 1),
            execucao_explicita_controles_ui(ns)
          )
        ),
        card(
          card_header("Relatório e Projeto R"),
          card_body(
            style = "padding: 12px 15px;",
            div(
              class = "alert alert-light border small mb-0",
              icon("file-export"), " ",
              "Baixe o Projeto R em ",
              strong("Comunicação de Resultados"), ". Execute a análise, clique em ",
              strong("Adicionar ao Projeto R"), " e escolha lá os componentes do relatório. ",
              "No RStudio, abra o projeto e use Render para gerar o caderno HTML e o Word."
            )
          )
        )
      ),

      # COLUNA 2: RESULTADOS (divulgação progressiva)
      execucao_explicita_resultados_ui(ns, navset_card_tab(
        id = ns("active_tab"),
        title = "Painel de Resultados da ANOVA",
        nav_panel(
          title = "Resultado principal",
          icon = icon("square-poll-vertical"),
          card_body(uiOutput(ns("principal_ui")))
        ),
        nav_panel(
          title = "Comparações",
          icon = icon("arrow-right-arrow-left"),
          card_body(uiOutput(ns("tukey_ui")))
        ),
        nav_panel(
          title = "Pressupostos e diagnósticos",
          icon = icon("circle-check"),
          card_body(uiOutput(ns("pressupostos_ui")))
        ),
        nav_panel(
          title = "Console R",
          icon = icon("terminal"),
          card_body(
            anova_titulo_secao("Saída bruta do R"),
            helpText(
              "Esta é a saída que o R mostra sem a camada de apresentação da CatalyseR.",
              "Vale conhecê-la para não se perder fora do ecossistema — mas ela fica",
              "só aqui: não é oferecida como conteúdo do relatório.",
              style = "font-size: 0.85rem;"
            ),
            verbatimTextOutput(ns("console_bruto"))
          )
        )
      )),

      # COLUNA 3: CONFIGURAÇÕES DE EXIBIÇÃO
      card(
        card_header("Configurações de exibição"),
        card_body(
          style = "padding: 10px 12px;",
          conditionalPanel(
            condition = sprintf(
              "input['%s'] == 'Resultado principal' || input['%s'] == 'Pressupostos e diagnósticos'",
              ns("active_tab"), ns("active_tab")
            ),
            textInput(ns("custom_title"), "Título do gráfico:", value = ""),
            textInput(ns("custom_label_x"), "Rótulo do eixo X:", value = ""),
            textInput(ns("custom_label_y"), "Rótulo do eixo Y:", value = ""),
            selectInput(ns("graph_theme"), "Tema do gráfico:",
                        choices = c("Mínimo" = "minimal",
                                    "Clássico" = "classic",
                                    "Preto e Branco" = "bw",
                                    "Cinza" = "gray",
                                    "Light" = "light"),
                        selected = "minimal"),
            helpText(
              "Mudar qualquer campo desta coluna deixa a execução pendente:",
              "o relatório precisa refletir exatamente o que você viu.",
              style = "font-size: 0.78rem;"
            )
          ),
          conditionalPanel(
            condition = sprintf(
              "input['%s'] == 'Comparações' || input['%s'] == 'Console R'",
              ns("active_tab"), ns("active_tab")
            ),
            helpText("As tabelas e a saída de console não dependem de configurações gráficas.")
          ),
        )
      )
    )
  )
}

# ficha_rv (opcional): ficha de planejamento; quando sugere esta ANOVA, suas
# colunas de resposta e de grupo são pré-selecionadas.
mod_anova_server <- function(id, data_rv, import_info, ficha_rv = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    revisao_execucao <- execucao_revisao_dados(data_rv)
    gatilho_execucao <- reactiveVal(0L)

    # Atualiza seletores preservando a escolha do usuário quando ela continua válida.
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
      if (!length(cat_cols)) cat_cols <- cols

      y_atual <- isolate(input$var_y)
      x_atual <- isolate(input$var_x)
      if (is.null(y_atual) || !y_atual %in% num_cols) y_atual <- num_cols[1]
      if (is.null(x_atual) || !x_atual %in% cat_cols) {
        alternativas <- setdiff(cat_cols, y_atual)
        x_atual <- if (length(alternativas)) alternativas[1] else cat_cols[1]
      }
      # Quando a ficha de planejamento sugere esta ANOVA, suas colunas têm prioridade.
      ficha <- if (is.function(ficha_rv)) ficha_rv() else NULL
      if (identical(ficha$analise_sugerida, "anova_um_fator")) {
        if (isTRUE(ficha$resposta_coluna %in% num_cols)) y_atual <- ficha$resposta_coluna
        if (ficha_fator_coluna(ficha) %in% cat_cols) x_atual <- ficha_fator_coluna(ficha)
      }
      updateSelectInput(session, "var_y", choices = num_cols, selected = y_atual)
      updateSelectInput(session, "var_x", choices = cat_cols, selected = x_atual)
    })

    # A sugestão de teste chega do planejamento, sem ser reescolhida do zero.
    output$aviso_ficha <- renderUI({
      if (!is.function(ficha_rv)) return(NULL)
      ficha_aviso_analise_ui(ficha_rv(), "anova_um_fator")
    })
    nivel_confianca <- reactive({
      valor <- suppressWarnings(as.numeric(input$conf_level))
      if (!length(valor) || is.na(valor)) valor <- 95
      valor / 100
    })

    assinatura_execucao <- reactive({
      req(input$var_y, input$var_x)
      execucao_assinatura(
        input,
        c("var_y", "var_x", "conf_level", "graph_theme",
          "custom_title", "custom_label_x", "custom_label_y"),
        revisao_execucao()
      )
    })

    # A validação acontece antes do ajuste e devolve mensagem com ação corretiva.
    result_rv <- eventReactive(gatilho_execucao(), {
      df <- data_rv()
      req(df, input$var_y, input$var_x)
      mensagem <- anova_validar_entrada(df, input$var_y, input$var_x)
      if (!is.null(mensagem)) stop(mensagem, call. = FALSE)
      calcular_anova(df, input$var_y, input$var_x, nivel_confianca = nivel_confianca())
    }, ignoreInit = FALSE)

    exec_ctrl <- execucao_explicita_server(
      input, output, session, assinatura_execucao, result_rv,
      nome_analise = "A ANOVA",
      gatilho_rv = gatilho_execucao
    )

    # ---- 1. Resultado principal ----------------------------------------------
    output$principal_ui <- renderUI({
      r <- result_rv()
      req(r)
      tagList(
        anova_titulo_secao("Narrativa automática"),
        div(class = "alert alert-secondary", style = "font-size: 0.9rem; line-height: 1.45;",
            relatar_anova(r)),
        if (r$excluidos > 0) div(
          class = "alert alert-warning py-2 small",
          icon("filter"), sprintf(
            " %d linha(s) foram excluídas por dados faltantes em '%s' ou '%s'. Restaram %d observações.",
            r$excluidos, r$dep_var, r$ind_var, r$n
          )
        ),
        if (length(r$grupos_pequenos)) div(
          class = "alert alert-warning py-2 small",
          icon("triangle-exclamation"),
          sprintf(" Grupos com menos de cinco observações: %s.",
                  paste(r$grupos_pequenos, collapse = ", "))
        ),
        hr(),
        anova_titulo_secao("Resumo por grupo"),
        tableOutput(ns("descritivos_table")),
        helpText(
          "A coluna Diferença traz as letras de Tukey: grupos que compartilham",
          "ao menos uma letra não apresentaram evidência de diferença entre si.",
          "A letra 'a' fica com o grupo de maior média.",
          style = "font-size: 0.82rem;"
        ),
        hr(),
        anova_titulo_secao("Tabela da ANOVA"),
        tableOutput(ns("anova_table")),
        anova_titulo_secao("Tamanho de efeito"),
        tableOutput(ns("efeito_table")),
        helpText(
          "η² é a fração da variação da resposta atribuída ao fator; ω² corrige o viés",
          "otimista do η² em amostras pequenas. A coluna Leitura usa a convenção de Cohen",
          "(0,01 pequeno · 0,06 médio · 0,14 grande) — é referência estatística, não",
          "interpretação biológica: um efeito pequeno pode importar no manejo, e um grande",
          "pode ser irrelevante na prática.",
          style = "font-size: 0.82rem;"
        ),
        if (!is.na(r$efeito_aviso)) div(class = "alert alert-light border py-2 small", r$efeito_aviso),
        hr(),
        anova_titulo_secao("Gráfico principal"),
        plotOutput(ns("fit_plot"), height = "440px"),
        anova_titulo_secao("Boxplot exploratório"),
        plotOutput(ns("box_plot"), height = "380px")
      )
    })

    output$descritivos_table <- renderTable({
      r <- result_rv(); req(r)
      arrumar_descritivos_anova(r)
    }, striped = TRUE, hover = TRUE, bordered = TRUE)

    output$anova_table <- renderTable({
      r <- result_rv(); req(r)
      arrumar_tabela_anova(r)
    }, striped = TRUE, hover = TRUE, bordered = TRUE)

    output$efeito_table <- renderTable({
      r <- result_rv(); req(r)
      arrumar_tamanho_efeito_anova(r)
    }, striped = TRUE, hover = TRUE, bordered = TRUE)

    grafico_principal <- reactive({
      r <- result_rv(); req(r)
      grafico_anova(
        r,
        titulo = if (nzchar(input$custom_title %||% "")) input$custom_title else NULL,
        rotulo_x = if (nzchar(input$custom_label_x %||% "")) input$custom_label_x else NULL,
        rotulo_y = if (nzchar(input$custom_label_y %||% "")) input$custom_label_y else NULL,
        tema = input$graph_theme %||% "minimal"
      )
    })

    output$fit_plot <- renderPlot({ grafico_principal() })

    output$box_plot <- renderPlot({
      r <- result_rv(); req(r)
      grafico_boxplot_anova(r, tema = input$graph_theme %||% "minimal")
    })

    # ---- 2. Comparações -------------------------------------------------------
    output$tukey_ui <- renderUI({
      r <- result_rv()
      req(r)
      tagList(
        anova_titulo_secao(sprintf("Comparações múltiplas de Tukey (IC %.0f%%)",
                                   100 * r$nivel_confianca)),
        tableOutput(ns("tukey_table")),
        helpText(
          "As comparações são sempre calculadas para manter a reprodutibilidade.",
          "A interpretação principal, porém, decorre da ANOVA global e do plano",
          "analítico definido antes da coleta — não de uma varredura de pares.",
          style = "font-size: 0.85rem;"
        )
      )
    })

    output$tukey_table <- renderTable({
      r <- result_rv(); req(r)
      arrumar_tukey_anova(r)
    }, striped = TRUE, hover = TRUE, bordered = TRUE)

    # ---- 3. Pressupostos e diagnósticos --------------------------------------
    output$pressupostos_ui <- renderUI({
      r <- result_rv()
      req(r)
      tagList(
        anova_titulo_secao("Testes de pressupostos"),
        tableOutput(ns("pressupostos_table")),
        if (!is.na(r$levene_aviso)) div(class = "alert alert-light border py-2 small", r$levene_aviso),
        helpText(
          "Um p-valor alto não comprova o pressuposto: apenas indica que estes dados",
          "não revelaram afastamento detectável. Olhe também os gráficos abaixo.",
          style = "font-size: 0.85rem;"
        ),
        hr(),
        anova_titulo_secao("Inspeção gráfica dos resíduos"),
        layout_columns(
          plotOutput(ns("resid_fit_plot"), height = "380px"),
          plotOutput(ns("qq_plot"), height = "380px")
        )
      )
    })

    output$pressupostos_table <- renderTable({
      r <- result_rv(); req(r)
      arrumar_pressupostos_anova(r)
    }, striped = TRUE, hover = TRUE, bordered = TRUE)

    output$resid_fit_plot <- renderPlot({
      r <- result_rv(); req(r)
      grafico_diagnosticos_anova(r, "residuos", tema = input$graph_theme %||% "minimal")
    })

    output$qq_plot <- renderPlot({
      r <- result_rv(); req(r)
      grafico_diagnosticos_anova(r, "qq", tema = input$graph_theme %||% "minimal")
    })

    # ---- 4. Console -----------------------------------------------------------
    output$console_bruto <- renderText({
      r <- result_rv(); req(r)
      paste(r$console, collapse = "\n")
    })

    # ---- Estado canônico registrado ------------------------------------------
    estado_execucao <- reactive({
      req(exec_ctrl$atualizada())
      r <- result_rv()
      req(r)
      titulo <- if (nzchar(input$custom_title %||% "")) {
        input$custom_title
      } else {
        sprintf("%s entre grupos de %s", r$dep_var, r$ind_var)
      }
      list(
        analise_id = "anova",
        tipo = "anova_um_fator",
        titulo = titulo,
        parametros = list(
          resposta = r$dep_var,
          fator = r$ind_var,
          nivel_confianca = r$nivel_confianca,
          ajuste_comparacoes = "tukey",
          tema = input$graph_theme %||% "minimal",
          titulo_grafico = input$custom_title %||% "",
          rotulo_x = input$custom_label_x %||% "",
          rotulo_y = input$custom_label_y %||% ""
        ),
        # O console fica fora do relatório: a aba Console R existe na interface
        # para estudo, mas a saída bruta não vai para o Word nem para o QMD.
        saidas_disponiveis = c(
          "narrativa", "descritivos", "tabela", "comparacoes",
          "grafico", "pressupostos", "diagnosticos"
        ),
        resultado_resumo = list(
          n = as.integer(r$n),
          excluidos = as.integer(r$excluidos),
          grupos = as.integer(r$n_grupos),
          f = unname(r$f_anova),
          gl_1 = as.integer(r$df_entre),
          gl_2 = as.integer(r$df_dentro),
          p = unname(r$p_anova),
          eta2 = unname(r$eta2),
          omega2 = unname(r$omega2)
        )
      )
    })

    invisible(list(
      resultado = result_rv,
      grafico = grafico_principal,
      estado_execucao = estado_execucao,
      estado_execucao_ui = exec_ctrl$estado,
      execucao_atualizada = exec_ctrl$atualizada
    ))
  })
}
