# Módulo de ANOVA de um Fator — V16
# -----------------------------------------------------------------------------
# A ANOVA passou a usar o mesmo contrato das demais análises homologadas:
#   Base Compartilhada ou Base Derivada -> Executar análise -> Adicionar aos
#   resultados -> Comunicação -> Word -> Projeto R.
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
              "O Word e o Projeto R da ANOVA saem de ",
              strong("Comunicação de Resultados"), ". Execute a análise, clique em ",
              strong("Adicionar aos resultados"), " e escolha lá os componentes do relatório."
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
              "Vale conhecê-la para não se perder fora do ecossistema.",
              style = "font-size: 0.85rem;"
            ),
            verbatimTextOutput(ns("console_bruto"))
          )
        ),
        nav_panel(
          title = "Laboratório didático",
          icon = icon("flask"),
          card_body(
            div(
              class = "alert alert-warning py-2 small",
              icon("triangle-exclamation"), " ",
              strong("Estas duas ferramentas não são resultados dos seus dados."),
              " A curva F é teórica e o simulador gera dados artificiais, apenas para",
              " entender como a ANOVA se comporta. Nada daqui entra no relatório."
            ),
            navset_pill(
              nav_panel(
                title = "Curva F teórica",
                layout_columns(
                  col_widths = c(4, 8),
                  card(
                    card_header("Parâmetros"),
                    card_body(
                      checkboxInput(ns("use_calculated_values"), "Usar valores reais do modelo", TRUE),
                      conditionalPanel(
                        condition = sprintf("!input['%s']", ns("use_calculated_values")),
                        numericInput(ns("sim_df_num"), "Graus de liberdade do fator:", value = 3, min = 1, step = 1),
                        numericInput(ns("sim_df_den"), "Graus de liberdade dos resíduos:", value = 15, min = 1, step = 1),
                        numericInput(ns("sim_f_val"), "F a visualizar:", value = 2.5, min = 0, step = 0.1)
                      ),
                      conditionalPanel(
                        condition = sprintf("input['%s']", ns("use_calculated_values")),
                        uiOutput(ns("calculated_values_info_ui"))
                      ),
                      sliderInput(ns("sim_alpha"), "Nível de significância (alfa):",
                                  min = 0.001, max = 0.20, value = 0.05, step = 0.005)
                    )
                  ),
                  card(
                    card_header("Distribuição F teórica"),
                    card_body(plotOutput(ns("f_dist_plot"), height = "420px"))
                  )
                )
              ),
              nav_panel(
                title = "Simulador",
                layout_columns(
                  col_widths = c(4, 8),
                  card(
                    card_header("Parâmetros do simulador"),
                    card_body(style = "padding: 10px 12px;", uiOutput(ns("sim_sliders_ui")))
                  ),
                  card(
                    card_header("Dados simulados"),
                    card_body(plotOutput(ns("sim_plot"), height = "410px"))
                  )
                )
              )
            )
          )
        )
      )),

      # COLUNA 3: EXIBIÇÃO / RESULTADOS DO SIMULADOR
      card(
        card_header(
          conditionalPanel(
            condition = sprintf("input['%s'] != 'Laboratório didático'", ns("active_tab")),
            "Configurações de exibição"
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'Laboratório didático'", ns("active_tab")),
            "Resultados do simulador"
          )
        ),
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
          conditionalPanel(
            condition = sprintf("input['%s'] == 'Laboratório didático'", ns("active_tab")),
            uiOutput(ns("sim_stats_col3_ui"))
          )
        )
      )
    )
  )
}

mod_anova_server <- function(id, data_rv, import_info) {
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
      updateSelectInput(session, "var_y", choices = num_cols, selected = y_atual)
      updateSelectInput(session, "var_x", choices = cat_cols, selected = x_atual)
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
        hr(),
        anova_titulo_secao("Tabela da ANOVA"),
        tableOutput(ns("anova_table")),
        anova_titulo_secao("Tamanho de efeito"),
        tableOutput(ns("efeito_table")),
        if (!is.na(r$efeito_aviso)) div(class = "alert alert-light border py-2 small", r$efeito_aviso),
        hr(),
        anova_titulo_secao("Gráfico principal"),
        plotOutput(ns("fit_plot"), height = "440px")
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

    # ---- 5. Laboratório didático ---------------------------------------------
    output$calculated_values_info_ui <- renderUI({
      r <- result_rv()
      req(r)
      tagList(
        tags$p(tags$b("gl do fator: "), r$df_entre, style = "margin-bottom: 4px; font-size: 0.85rem;"),
        tags$p(tags$b("gl dos resíduos: "), r$df_dentro, style = "margin-bottom: 4px; font-size: 0.85rem;"),
        tags$p(tags$b("F do modelo: "), round(r$f_anova, 4), style = "margin-bottom: 4px; font-size: 0.85rem;")
      )
    })

    output$f_dist_plot <- renderPlot({
      if (!requireNamespace("vistributions", quietly = TRUE)) {
        return(
          ggplot() +
            annotate("text", x = 0.5, y = 0.5,
                     label = "Instale o pacote 'vistributions' para ver a curva F teórica.",
                     color = "#0F3B5F") +
            theme_void()
        )
      }
      if (isTRUE(input$use_calculated_values)) {
        r <- result_rv()
        req(r)
        df_num <- r$df_entre; df_den <- r$df_dentro; f_val <- r$f_anova
      } else {
        req(input$sim_df_num, input$sim_df_den)
        df_num <- input$sim_df_num; df_den <- input$sim_df_den; f_val <- input$sim_f_val
      }
      alpha <- input$sim_alpha

      p <- tryCatch(
        vistributions::vdist_f_perc(probs = 1 - alpha, num_df = df_num,
                                    den_df = df_den, type = "lower", print_plot = FALSE),
        error = function(e) {
          ggplot() +
            annotate("text", x = 0.5, y = 0.5,
                     label = paste("Não foi possível gerar a curva:", e$message),
                     color = "#E76F51") +
            theme_void()
        }
      )
      if (!is.null(p) && inherits(p, "ggplot") && !is.null(f_val) && !is.na(f_val)) {
        f_crit <- qf(1 - alpha, df_num, df_den)
        max_x <- min(max(f_crit * 1.5, f_val * 1.2, 5), 50)
        p <- p +
          geom_vline(xintercept = f_val, color = "#2E7D8F", linewidth = 1.2) +
          annotate("label", x = f_val, y = 0, label = paste("F =", round(f_val, 2)),
                   fill = "white", color = "#2E7D8F", fontface = "bold", size = 4) +
          coord_cartesian(xlim = c(0, max_x)) +
          labs(
            title = paste("Distribuição F teórica (gl =", df_num, ",", df_den, ")"),
            subtitle = paste("Região de rejeição a", alpha * 100, "% (F >", round(f_crit, 3), ")")
          ) +
          theme(
            plot.title = element_text(face = "bold", color = "#0F3B5F", size = 13),
            plot.subtitle = element_text(color = "#495057", size = 10)
          )
      }
      p
    })

    output$sim_sliders_ui <- renderUI({
      r <- result_rv()
      req(r)
      niveis <- head(levels(r$dados$fator), 5)
      medias <- vapply(niveis, function(nivel) mean(r$dados$resposta[r$dados$fator == nivel]), numeric(1))
      sd_res <- stats::sd(r$residuals)
      min_y <- min(r$dados$resposta); max_y <- max(r$dados$resposta)
      amplitude <- max_y - min_y
      if (!is.finite(amplitude) || amplitude <= 0) amplitude <- 1

      sliders <- lapply(niveis, function(nivel) {
        sliderInput(
          inputId = session$ns(paste0("sim_mean_", nivel)),
          label = paste("Média do grupo", nivel),
          min = round(min_y - amplitude * 0.15, 1),
          max = round(max_y + amplitude * 0.15, 1),
          value = round(unname(medias[nivel]), 1),
          step = 0.1
        )
      })
      tagList(
        sliders,
        sliderInput(
          inputId = session$ns("sim_sd"),
          label = "Desvio-padrão dentro dos grupos:",
          min = round(max(0.1, sd_res * 0.1), 2),
          max = round(max(0.2, sd_res * 3), 2),
          value = round(sd_res, 2),
          step = 0.05
        )
      )
    })

    simulated_data_rv <- reactive({
      r <- result_rv()
      req(r)
      niveis <- head(levels(r$dados$fator), 5)
      medias_sim <- vapply(niveis, function(nivel) {
        valor <- input[[paste0("sim_mean_", nivel)]]
        if (is.null(valor)) mean(r$dados$resposta[r$dados$fator == nivel]) else as.numeric(valor)
      }, numeric(1))
      sd_sim <- input$sim_sd
      if (is.null(sd_sim)) sd_sim <- stats::sd(r$residuals)
      n_sim <- round(mean(table(r$dados$fator[r$dados$fator %in% niveis])))
      if (!is.finite(n_sim) || n_sim < 2) n_sim <- 5

      set.seed(1234)
      sim_df <- data.frame(
        Grupo = factor(rep(niveis, each = n_sim), levels = niveis),
        Valor = unlist(lapply(niveis, function(nivel) {
          stats::rnorm(n_sim, mean = medias_sim[nivel], sd = sd_sim)
        }))
      )
      fit_sim <- stats::aov(Valor ~ Grupo, data = sim_df)
      resumo_sim <- summary(fit_sim)[[1]]
      list(
        data = sim_df, n = n_sim, sd = sd_sim,
        f_val = resumo_sim$`F value`[1], p_val = resumo_sim$`Pr(>F)`[1],
        sq_entre = resumo_sim$`Sum Sq`[1], sq_dentro = resumo_sim$`Sum Sq`[2]
      )
    })

    output$sim_plot <- renderPlot({
      sim <- simulated_data_rv()
      r <- result_rv()
      req(sim, r)
      medias <- aggregate(Valor ~ Grupo, data = sim$data, FUN = mean)
      ggplot(sim$data, aes(x = Grupo, y = Valor)) +
        geom_jitter(aes(color = Grupo), width = 0.12, height = 0, alpha = 0.65,
                    size = 2.2, show.legend = FALSE) +
        geom_point(data = medias, aes(x = Grupo, y = Valor), inherit.aes = FALSE,
                   size = 3.4, shape = 18, color = "#E76F51") +
        scale_color_manual(values = rep(anova_cores_ocean, length.out = nlevels(sim$data$Grupo))) +
        theme_minimal(base_size = 13) +
        labs(title = "Dados simulados (não são os seus dados)",
             x = r$ind_var, y = r$dep_var) +
        theme(plot.title = element_text(face = "bold", color = "#0F3B5F"))
    })

    output$sim_stats_col3_ui <- renderUI({
      sim <- simulated_data_rv()
      req(sim)
      f_str <- anova_fmt(sim$f_val, 3)
      p_str <- anova_p_texto(sim$p_val)
      classe <- if (!is.na(sim$p_val) && sim$p_val < 0.05) "alert-success" else "alert-secondary"
      leitura <- if (!is.na(sim$p_val) && sim$p_val < 0.05) {
        "Rejeitou-se H0 nesta simulação"
      } else {
        "Não houve evidência suficiente para rejeitar H0 nesta simulação"
      }
      tagList(
        div(class = "card text-center border-primary", style = "padding: 8px; margin-bottom: 6px; border-radius: 6px;",
            h6("F simulado", class = "card-subtitle text-muted", style = "font-size: 0.75rem; margin-bottom: 2px; font-weight: 600;"),
            h4(f_str, class = "card-title text-primary", style = "font-weight: 800; margin-bottom: 0; font-size: 1.25rem;")),
        div(class = paste("card text-center alert", classe), style = "padding: 8px; margin-bottom: 6px; border: 1px solid; color: inherit; border-radius: 6px;",
            h6("p-valor simulado", class = "card-subtitle text-muted", style = "font-size: 0.75rem; margin-bottom: 2px; font-weight: 600;"),
            h4(p_str, class = "card-title", style = "font-weight: 800; margin-bottom: 0; font-size: 1.1rem;")),
        div(class = "card text-center border-secondary", style = "padding: 8px; margin-bottom: 6px; border-radius: 6px;",
            h6("SQ entre grupos", class = "card-subtitle text-muted", style = "font-size: 0.75rem; margin-bottom: 2px; font-weight: 600;"),
            h4(anova_fmt(sim$sq_entre, 1), class = "card-title text-secondary", style = "font-weight: 800; margin-bottom: 0; font-size: 1.1rem;")),
        div(class = "card text-center border-secondary", style = "padding: 8px; margin-bottom: 6px; border-radius: 6px;",
            h6("SQ dentro dos grupos", class = "card-subtitle text-muted", style = "font-size: 0.75rem; margin-bottom: 2px; font-weight: 600;"),
            h4(anova_fmt(sim$sq_dentro, 1), class = "card-title text-secondary", style = "font-weight: 800; margin-bottom: 0; font-size: 1.1rem;")),
        div(class = paste("alert text-center", classe), style = "padding: 8px; font-weight: 600; font-size: 0.8rem; margin-top: 8px; margin-bottom: 0; border-radius: 6px;",
            leitura)
      )
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
        saidas_disponiveis = c(
          "narrativa", "descritivos", "tabela", "comparacoes",
          "grafico", "pressupostos", "diagnosticos", "console"
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
