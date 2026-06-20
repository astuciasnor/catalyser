# Módulo de ANOVA de um Fator para IDE_R
library(shiny)
library(bslib)
library(ggplot2)
library(DT)

if (file.exists("templates/funcoes_anova.R")) {
  source("templates/funcoes_anova.R")
}

# Helper para customizar parâmetros do relatório Quarto de ANOVA
customize_anova_qmd_params <- function(qmd_path, var_y, var_x, label_y, label_x) {
  lines <- readLines(qmd_path, warn = FALSE)
  lines <- gsub('var_y: ".*"', sprintf('var_y: "%s"', var_y), lines)
  lines <- gsub('var_x: ".*"', sprintf('var_x: "%s"', var_x), lines)
  lines <- gsub('label_y: ".*"', sprintf('label_y: "%s"', label_y), lines)
  lines <- gsub('label_x: ".*"', sprintf('label_x: "%s"', label_x), lines)
  return(lines)
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
            selectInput(ns("var_y"), "Variável Dependente (Y - Numérica):", choices = NULL),
            selectInput(ns("var_x"), "Fator / Grupo (X - Categórica):", choices = NULL)
          )
        ),
        card(
          card_header("Relatório e Pacote de Estudo"),
          card_body(
            style = "padding: 12px 15px;",
            downloadButton(ns("download_report_docx"), "Baixar Relatório Word (.docx)", class = "btn-success w-100"),
            div(style = "margin-top: 8px;"),
            downloadButton(ns("download_project_zip"), "Exportar Projeto R (.zip)", class = "btn-primary w-100"),
            helpText("Gera os relatórios e projetos contendo os códigos estatísticos.", style = "margin-top: 10px; font-size: 0.85rem;")
          )
        )
      ),
      
      # COLUNA 2: ABAS DE RESULTADOS (PRINCIPAL)
      navset_card_tab(
        id = ns("active_tab"),
        title = "Painel de Resultados da ANOVA",
        nav_panel(
          title = "Tabela ANOVA & Pressupostos",
          icon = icon("table"),
          card_body(
            uiOutput(ns("anova_pressupostos_ui"))
          )
        ),
        nav_panel(
          title = "Comparações (Tukey HSD)",
          icon = icon("arrow-right-arrow-left"),
          card_body(
            uiOutput(ns("tukey_ui"))
          )
        ),
        nav_panel(
          title = "Gráfico de Médias",
          icon = icon("chart-bar"),
          card_body(
            plotOutput(ns("fit_plot"), height = "450px")
          )
        ),
        nav_panel(
          title = "Diagnóstico (Resíduos)",
          icon = icon("circle-check"),
          card_body(
            layout_columns(
              plotOutput(ns("resid_fit_plot"), height = "400px"),
              plotOutput(ns("qq_plot"), height = "400px")
            )
          )
        ),
        nav_panel(
          title = "Curva F e Simulação",
          icon = icon("chart-line"),
          card_body(
            layout_columns(
              col_widths = c(4, 8),
              card(
                card_header("Parâmetros do Simulador"),
                card_body(
                  checkboxInput(ns("use_calculated_values"), "Usar valores reais do modelo", TRUE),
                  conditionalPanel(
                    condition = sprintf("!input['%s']", ns("use_calculated_values")),
                    numericInput(ns("sim_df_num"), "Graus de Liberdade Num. (gl Fator):", value = 3, min = 1, step = 1),
                    numericInput(ns("sim_df_den"), "Graus de Liberdade Den. (gl Resíduos):", value = 15, min = 1, step = 1),
                    numericInput(ns("sim_f_val"), "F Calculado para Visualizar:", value = 2.5, min = 0, step = 0.1)
                  ),
                  conditionalPanel(
                    condition = sprintf("input['%s']", ns("use_calculated_values")),
                    uiOutput(ns("calculated_values_info_ui"))
                  ),
                  sliderInput(ns("sim_alpha"), "Nível de Significância (alfa):", min = 0.001, max = 0.20, value = 0.05, step = 0.005)
                )
              ),
              card(
                card_header("Visualização da Distribuição F Teórica"),
                card_body(
                  plotOutput(ns("f_dist_plot"), height = "420px")
                )
              )
            )
          )
        ),
        nav_panel(
          title = "Simulador Didático",
          icon = icon("gamepad"),
          card_body(
            layout_columns(
              col_widths = c(4, 8),
              # Painel de Sliders reativos (Coluna da Esquerda)
              card(
                card_header("Parâmetros do Simulador"),
                card_body(
                  style = "padding: 10px 12px; overflow-y: auto; max-height: 520px;",
                  helpText("Arraste os valores para observar a variação no gráfico e nas estatísticas em tempo real (dados simulados com base no modelo real)."),
                  hr(style = "margin: 8px 0;"),
                  uiOutput(ns("sim_sliders_ui"))
                )
              ),
              # Painel do Gráfico e Estatísticas (Coluna da Direita)
              div(
                card(
                  card_header("Dados Simulados vs Médias e SD"),
                  card_body(
                    plotOutput(ns("sim_plot"), height = "300px")
                  )
                ),
                card(
                  card_header("Resultados Estatísticos da Simulação (ANOVA)"),
                  card_body(
                    uiOutput(ns("sim_stats_ui"))
                  )
                )
              )
            )
          )
        )
      ),
      
      # COLUNA 3: CONFIGURAÇÕES DE EXIBIÇÃO
      card(
        card_header("Configurações de Exibição"),
        card_body(
          conditionalPanel(
            condition = sprintf("input['%s'] != 'Tabela ANOVA & Pressupostos' && input['%s'] != 'Comparações (Tukey HSD)' && input['%s'] != 'Curva F e Simulação' && input['%s'] != 'Simulador Didático'", ns("active_tab"), ns("active_tab"), ns("active_tab"), ns("active_tab")),
            textInput(ns("custom_title"), "Título do Gráfico:", value = ""),
            textInput(ns("custom_label_x"), "Rótulo Eixo X:", value = ""),
            textInput(ns("custom_label_y"), "Rótulo Eixo Y:", value = ""),
            selectInput(ns("graph_theme"), "Tema do Gráfico:", 
                        choices = c("Mínimo" = "minimal", 
                                    "Clássico" = "classic", 
                                    "Preto e Branco" = "bw", 
                                    "Cinza" = "gray", 
                                    "Light" = "light"), 
                        selected = "minimal")
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'Tabela ANOVA & Pressupostos' || input['%s'] == 'Comparações (Tukey HSD)' || input['%s'] == 'Curva F e Simulação' || input['%s'] == 'Simulador Didático'", ns("active_tab"), ns("active_tab"), ns("active_tab"), ns("active_tab")),
            helpText("Os resultados das tabelas e simulações teóricas são calculados de forma exata e não possuem configurações gráficas adicionais.")
          )
        )
      )
    )
  )
}

mod_anova_server <- function(id, data_rv, import_info) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Atualiza seletores de variáveis
    observe({
      df <- data_rv()
      req(df)
      cols <- names(df)
      num_cols <- cols[sapply(df, is.numeric)]
      
      # Fatores podem ser texto ou numéricos discretos
      cat_cols <- cols[!sapply(df, is.numeric) | sapply(df, function(x) length(unique(x)) < 15)]
      
      updateSelectInput(session, "var_y", choices = num_cols)
      updateSelectInput(session, "var_x", choices = cat_cols)
    })
    
    # Executa cálculo reativo da ANOVA
    result_rv <- reactive({
      df <- data_rv()
      req(df, input$var_y, input$var_x)
      calcular_anova(df, input$var_y, input$var_x)
    })
    
    # Renderiza UI de ANOVA e Pressupostos
    output$anova_pressupostos_ui <- renderUI({
      r <- result_rv()
      req(r)
      relato <- relatar_anova(r)
      
      tagList(
        h6("Tabela da Análise de Variância (ANOVA)", style = "font-family: 'Outfit'; font-weight: 700; color: #0F3B5F; margin-top: 5px;"),
        tableOutput(ns("anova_table")),
        hr(),
        h6("Validação de Pressupostos Estatísticos", style = "font-family: 'Outfit'; font-weight: 700; color: #0F3B5F;"),
        tableOutput(ns("pressupostos_table")),
        hr(),
        h6("Relato Científico Automatizado", style = "font-family: 'Outfit'; font-weight: 700; color: #0F3B5F;"),
        div(class = "alert alert-secondary", style = "font-size: 0.9rem; line-height: 1.4;", relato)
      )
    })
    
    output$anova_table <- renderTable({
      r <- result_rv()
      req(r)
      mostrar_anova(r)
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
    
    output$pressupostos_table <- renderTable({
      r <- result_rv()
      req(r)
      mostrar_pressupostos(r)
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
    
    # Renderiza UI do teste de Tukey HSD
    output$tukey_ui <- renderUI({
      r <- result_rv()
      req(r)
      
      tagList(
        h6("Comparações Múltiplas de Tukey HSD (IC 95% das diferenças)", style = "font-family: 'Outfit'; font-weight: 700; color: #0F3B5F; margin-top: 5px;"),
        tableOutput(ns("tukey_table")),
        helpText("As comparações são estatisticamente significativas se o p-valor ajustado for menor que 0,05 e o intervalo de confiança não contiver o valor zero.")
      )
    })
    
    output$tukey_table <- renderTable({
      r <- result_rv()
      req(r)
      mostrar_tukey(r)
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
    
    # Gráfico de Médias (Boxplot + Jitter)
    output$fit_plot <- renderPlot({
      r <- result_rv()
      df <- data_rv()
      req(r, df)
      
      g_theme <- switch(input$graph_theme,
                        "minimal" = theme_minimal(base_size = 14),
                        "classic" = theme_classic(base_size = 14),
                        "bw"      = theme_bw(base_size = 14),
                        "gray"    = theme_gray(base_size = 14),
                        "light"   = theme_light(base_size = 14),
                        theme_minimal(base_size = 14))
      
      title_val <- if (nzchar(input$custom_title)) input$custom_title else paste("Distribuição de", r$dep_var, "por", r$ind_var)
      x_label <- if (nzchar(input$custom_label_x)) input$custom_label_x else r$ind_var
      y_label <- if (nzchar(input$custom_label_y)) input$custom_label_y else r$dep_var
      
      ggplot(df, aes(x = as.factor(.data[[r$ind_var]]), y = .data[[r$dep_var]], fill = as.factor(.data[[r$ind_var]]))) +
        geom_boxplot(alpha = 0.6, outlier.color = NA) +
        geom_jitter(color = "#495057", width = 0.12, alpha = 0.5, size = 2) +
        # Adicionar ponto de média amostral de cada grupo
        stat_summary(fun = mean, geom = "point", shape = 23, size = 4, fill = "white", color = "#0F3B5F") +
        g_theme +
        labs(title = title_val, x = x_label, y = y_label, fill = r$ind_var) +
        theme(plot.title = element_text(face = "bold", color = "#0F3B5F"),
              legend.position = "none")
    })
    
    # Gráficos Diagnósticos: Resíduos vs Ajustados
    output$resid_fit_plot <- renderPlot({
      r <- result_rv()
      req(r)
      
      g_theme <- switch(input$graph_theme,
                        "minimal" = theme_minimal(base_size = 13),
                        "classic" = theme_classic(base_size = 13),
                        "bw"      = theme_bw(base_size = 13),
                        "gray"    = theme_gray(base_size = 13),
                        "light"   = theme_light(base_size = 13),
                        theme_minimal(base_size = 13))
      
      diag_data <- data.frame(Ajustados = r$fitted, Residuos = r$residuals)
      
      ggplot(diag_data, aes(x = Ajustados, y = Residuos)) +
        geom_point(color = "#495057", alpha = 0.7, size = 2.5) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "#dc3545", linewidth = 1) +
        geom_smooth(method = "loess", formula = y ~ x, color = "#198754", fill = "#d1e7dd", se = FALSE) +
        g_theme +
        labs(title = "Resíduos vs Ajustados", x = "Valores Ajustados (Fitted)", y = "Resíduos (Residuals)") +
        theme(plot.title = element_text(face = "bold", color = "#212529"))
    })
    
    # Gráficos Diagnósticos: Normalidade (QQ Plot)
    output$qq_plot <- renderPlot({
      r <- result_rv()
      req(r)
      
      g_theme <- switch(input$graph_theme,
                        "minimal" = theme_minimal(base_size = 13),
                        "classic" = theme_classic(base_size = 13),
                        "bw"      = theme_bw(base_size = 13),
                        "gray"    = theme_gray(base_size = 13),
                        "light"   = theme_light(base_size = 13),
                        theme_minimal(base_size = 13))
      
      # Calcular resíduos padronizados
      std_resid <- scale(r$residuals)
      diag_data_qq <- data.frame(ResiduosStd = std_resid)
      
      ggplot(diag_data_qq, aes(sample = ResiduosStd)) +
        stat_qq(color = "#495057", alpha = 0.7, size = 2.5) +
        stat_qq_line(color = "#0d6efd", linewidth = 1) +
        g_theme +
        labs(title = "Normal Q-Q Plot", x = "Quantis Teóricos", y = "Resíduos Padronizados") +
        theme(plot.title = element_text(face = "bold", color = "#212529"))
    })
    
    # Renderizar informações dos valores calculados do modelo real
    output$calculated_values_info_ui <- renderUI({
      r <- result_rv()
      req(r)
      tagList(
        tags$p(tags$b("Graus de Liberdade Fator (gl): "), r$df_entre, style = "margin-bottom: 4px; font-size: 0.85rem;"),
        tags$p(tags$b("Graus de Liberdade Resíduos (gl): "), r$df_dentro, style = "margin-bottom: 4px; font-size: 0.85rem;"),
        tags$p(tags$b("F Calculado do Modelo: "), round(r$f_anova, 4), style = "margin-bottom: 4px; font-size: 0.85rem;")
      )
    })
    
    # Renderizar gráfico de curva F teórica com simulação interativa
    output$f_dist_plot <- renderPlot({
      library(vistributions)
      
      # Carregar valores reais ou usar inputs de simulação
      if (input$use_calculated_values) {
        r <- result_rv()
        req(r)
        df_num <- r$df_entre
        df_den <- r$df_dentro
        f_val <- r$f_anova
      } else {
        req(input$sim_df_num, input$sim_df_den)
        df_num <- input$sim_df_num
        df_den <- input$sim_df_den
        f_val <- input$sim_f_val
      }
      
      alpha <- input$sim_alpha
      
      # Gerar a curva de distribuição F usando vdist_f_perc (para realçar região crítica)
      p <- tryCatch({
        vdist_f_perc(probs = 1 - alpha, num_df = df_num, den_df = df_den, type = "lower", print_plot = FALSE)
      }, error = function(e) {
        ggplot() + 
          annotate("text", x = 0.5, y = 0.5, label = paste("Erro ao gerar gráfico:", e$message), color = "red") + 
          theme_void()
      })
      
      # Adicionar F experimental/calculado no gráfico se for válido
      if (!is.null(p) && inherits(p, "ggplot") && !is.null(f_val) && !is.na(f_val)) {
        f_crit <- qf(1 - alpha, df_num, df_den)
        max_x <- max(f_crit * 1.5, f_val * 1.2, 5)
        max_x <- min(max_x, 50) # limite de segurança para a visualização
        
        p <- p + 
          geom_vline(xintercept = f_val, color = "#0d6efd", linewidth = 1.2, linetype = "solid") +
          annotate("label", x = f_val, y = 0, label = paste("F calc =", round(f_val, 2)), 
                   fill = "white", color = "#0d6efd", fontface = "bold", size = 4) +
          coord_cartesian(xlim = c(0, max_x)) +
          labs(
            title = paste("Distribuição F Teórica (gl =", df_num, ",", df_den, ")"),
            subtitle = paste("Região de Rejeição a", alpha * 100, "% (F >", round(f_crit, 3), ")")
          ) +
          theme(
            plot.title = element_text(face = "bold", color = "#0F3B5F", size = 13),
            plot.subtitle = element_text(color = "#495057", size = 10)
          )
      }
      
      p
    })
    
    # ==========================================
    # LÓGICA DO SIMULADOR DIDÁTICO
    # ==========================================
    
    # 1. Renderiza a interface de Sliders dinâmicos com base nos níveis do fator categórico atual
    output$sim_sliders_ui <- renderUI({
      r <- result_rv()
      df <- data_rv()
      req(r, df)
      
      levels_x <- levels(as.factor(df[[r$ind_var]]))
      # Limitar a no máximo 5 níveis no simulador para não sobrecarregar
      levels_x <- head(levels_x, 5)
      
      # Calcular médias reais de cada grupo
      means <- sapply(levels_x, function(lvl) {
        mean(df[[r$dep_var]][df[[r$ind_var]] == lvl], na.rm = TRUE)
      })
      
      # Desvio padrão residual real
      sd_res <- sd(r$residuals)
      
      # Tamanho amostral real aproximado
      n_rep <- round(mean(table(df[[r$ind_var]][df[[r$ind_var]] %in% levels_x])))
      
      # Limites dos sliders baseados nas variáveis
      min_y <- min(df[[r$dep_var]], na.rm = TRUE)
      max_y <- max(df[[r$dep_var]], na.rm = TRUE)
      range_y <- max_y - min_y
      
      # Gerar os sliders para cada média de grupo
      mean_sliders <- lapply(levels_x, function(lvl) {
        sliderInput(
          inputId = session$ns(paste0("sim_mean_", lvl)),
          label = paste("Média Grupo", lvl),
          min = round(min_y - range_y * 0.15, 1),
          max = round(max_y + range_y * 0.15, 1),
          value = round(means[lvl], 1),
          step = 0.1
        )
      })
      
      tagList(
        h6("Médias dos Grupos", style = "font-family: 'Outfit'; font-weight: 700; color: #0F3B5F; margin-top: 10px;"),
        mean_sliders,
        hr(style = "margin: 10px 0;"),
        h6("Dispersão e Amostragem", style = "font-family: 'Outfit'; font-weight: 700; color: #0F3B5F;"),
        sliderInput(
          inputId = session$ns("sim_sd"),
          label = "Desvio Padrão Interno (Erro / SD):",
          min = round(max(0.1, sd_res * 0.1), 2),
          max = round(sd_res * 3, 2),
          value = round(sd_res, 2),
          step = 0.05
        ),
        sliderInput(
          inputId = session$ns("sim_n"),
          label = "Nº de Réplicas por Grupo (n):",
          min = 3,
          max = 30,
          value = n_rep,
          step = 1
        )
      )
    })
    
    # 2. Dados Simulados reativos
    simulated_data_rv <- reactive({
      r <- result_rv()
      df <- data_rv()
      req(r, df)
      
      levels_x <- levels(as.factor(df[[r$ind_var]]))
      levels_x <- head(levels_x, 5)
      
      # Obter valores de médias dos sliders
      sim_means <- sapply(levels_x, function(lvl) {
        val <- input[[paste0("sim_mean_", lvl)]]
        if (is.null(val)) {
          # Fallback para média real caso o slider ainda não esteja renderizado
          mean(df[[r$dep_var]][df[[r$ind_var]] == lvl], na.rm = TRUE)
        } else {
          val
        }
      })
      
      sim_sd <- input$sim_sd
      if (is.null(sim_sd)) sim_sd <- sd(r$residuals)
      
      sim_n <- input$sim_n
      if (is.null(sim_n)) sim_n <- 5
      
      # Semente fixa para que os pontos não fiquem pulando na tela a cada mudança
      set.seed(1234)
      
      sim_df <- data.frame(
        Grupo = rep(levels_x, each = sim_n),
        Valor = unlist(lapply(levels_x, function(lvl) {
          rnorm(sim_n, mean = sim_means[lvl], sd = sim_sd)
        }))
      )
      
      # Ajustar ANOVA simulada
      fit_sim <- aov(Valor ~ Grupo, data = sim_df)
      anova_sim <- summary(fit_sim)[[1]]
      
      list(
        data = sim_df,
        fit = fit_sim,
        anova_summary = anova_sim,
        means = sim_means,
        sd = sim_sd,
        n = sim_n,
        f_val = anova_sim$`F value`[1],
        p_val = anova_sim$`Pr(>F)`[1],
        sq_entre = anova_sim$`Sum Sq`[1],
        sq_dentro = anova_sim$`Sum Sq`[2]
      )
    })
    
    # 3. Renderiza o gráfico do simulador
    output$sim_plot <- renderPlot({
      sim <- simulated_data_rv()
      r <- result_rv()
      req(sim, r)
      
      library(ggpubr)
      
      # Níveis ordenados do fator para manter a consistência no eixo X
      levels_x <- levels(as.factor(sim$data$Grupo))
      
      ggline(
        data = sim$data,
        x = "Grupo",
        y = "Valor",
        add = c("mean_se", "jitter"),
        color = "Grupo",
        palette = "jco",
        order = levels_x,
        ylab = r$dep_var,
        xlab = r$ind_var,
        title = "Gráfico de Médias e Erro Padrão (Simulado)",
        ggtheme = theme_minimal(base_size = 13)
      ) +
        # Linha pontilhada conectando as médias para guiar a visualização da tendência
        stat_summary(fun = mean, geom = "line", aes(group = 1), color = "#a0aec0", linewidth = 0.8, linetype = "dashed") +
        theme(
          plot.title = element_text(face = "bold", color = "#0F3B5F"),
          legend.position = "none"
        )
    })
    
    # 4. Renderiza o dashboard de estatísticas da simulação
    output$sim_stats_ui <- renderUI({
      sim <- simulated_data_rv()
      req(sim)
      
      # Formatar strings
      f_str <- fmt(sim$f_val, 3)
      p_str <- if (sim$p_val < 0.001) "p < 0,001" else paste0("p = ", fmt(sim$p_val, 4))
      
      # Cores de feedback dinâmico
      sig_class <- if (sim$p_val < 0.05) "alert-success" else "alert-danger"
      sig_label <- if (sim$p_val < 0.05) "Diferenças estatisticamente significativas (H0 rejeitada)" else "Sem diferenças estatisticamente significativas (H0 aceita)"
      
      tagList(
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          # Box 1
          div(class = "card text-center border-primary", style = "padding: 8px; margin-bottom: 4px;",
            h6("F Calculado", class = "card-subtitle text-muted", style = "font-size: 0.8rem; margin-bottom: 2px;"),
            h4(f_str, class = "card-title text-primary", style = "font-weight: 800; margin-bottom: 0;")
          ),
          # Box 2
          div(class = paste("card text-center alert", sig_class), style = "padding: 8px; margin-bottom: 4px; border: 1px solid; color: inherit;",
            h6("p-valor", class = "card-subtitle text-muted", style = "font-size: 0.8rem; margin-bottom: 2px;"),
            h4(p_str, class = "card-title", style = "font-weight: 800; margin-bottom: 0;")
          ),
          # Box 3
          div(class = "card text-center border-secondary", style = "padding: 8px; margin-bottom: 4px;",
            h6("SQ Entre (Fator)", class = "card-subtitle text-muted", style = "font-size: 0.8rem; margin-bottom: 2px;"),
            h4(fmt(sim$sq_entre, 1), class = "card-title text-secondary", style = "font-weight: 800; margin-bottom: 0;")
          ),
          # Box 4
          div(class = "card text-center border-secondary", style = "padding: 8px; margin-bottom: 4px;",
            h6("SQ Dentro (Erro)", class = "card-subtitle text-muted", style = "font-size: 0.8rem; margin-bottom: 2px;"),
            h4(fmt(sim$sq_dentro, 1), class = "card-title text-secondary", style = "font-weight: 800; margin-bottom: 0;")
          )
        ),
        div(class = paste("alert text-center", sig_class), style = "padding: 6px; font-weight: 600; font-size: 0.85rem; margin-top: 6px; margin-bottom: 0;",
          sig_label
        )
      )
    })
    
    # Handlers de Download
    output$download_report_docx <- downloadHandler(
      filename = function() {
        paste0("relatorio_anova_", format(Sys.Date(), "%Y-%m-%d"), ".docx")
      },
      content = function(file) {
        req(data_rv())
        
        temp_dir <- tempdir()
        temp_qmd <- file.path(temp_dir, "relatorio_anova.qmd")
        temp_ref <- file.path(temp_dir, "custom-reference.docx")
        temp_func <- file.path(temp_dir, "funcoes_anova.R")
        temp_data <- file.path(temp_dir, "dados_limpos.rda")
        
        file.copy("templates/custom-reference.docx", temp_ref, overwrite = TRUE)
        file.copy("templates/funcoes_anova.R", temp_func, overwrite = TRUE)
        file.copy("templates/relatorio_anova.qmd", temp_qmd, overwrite = TRUE)
        
        df_clean <- data_rv()
        save(df_clean, file = temp_data)
        
        custom_qmd_lines <- customize_anova_qmd_params(
          temp_qmd,
          var_y = input$var_y,
          var_x = input$var_x,
          label_y = input$var_y,
          label_x = input$var_x
        )
        writeLines(custom_qmd_lines, temp_qmd)
        
        old_wd <- getwd()
        setwd(temp_dir)
        system2("quarto", args = c("render", "relatorio_anova.qmd", "--to", "docx"))
        setwd(old_wd)
        
        generated_docx <- file.path(temp_dir, "relatorio_anova.docx")
        if (file.exists(generated_docx)) {
          file.copy(generated_docx, file, overwrite = TRUE)
        } else {
          writeLines("Erro ao compilar o Word.", file)
        }
      }
    )
    
    output$download_project_zip <- downloadHandler(
      filename = function() {
        paste0("projeto_anova_", format(Sys.Date(), "%Y-%m-%d"), ".zip")
      },
      content = function(file) {
        info <- import_info()
        proj_dir_name <- paste0("projeto_anova_", format(Sys.Date(), "%Y-%m-%d"))
        temp_dir <- tempdir()
        proj_dir <- file.path(temp_dir, proj_dir_name)
        
        dir.create(proj_dir, showWarnings = FALSE)
        dir_dados <- file.path(proj_dir, "dados")
        dir_scripts <- file.path(proj_dir, "scripts")
        dir_relatorios <- file.path(proj_dir, "relatorios")
        
        dir.create(dir_dados, showWarnings = FALSE)
        dir.create(dir_scripts, showWarnings = FALSE)
        dir.create(dir_relatorios, showWarnings = FALSE)
        
        df_clean <- data_rv()
        save(df_clean, file = file.path(dir_dados, "dados_limpos.rda"))
        write.csv(df_clean, file = file.path(dir_dados, "dados_limpos.csv"), row.names = FALSE)
        ds_name <- if (info$source == "package") info$package_dataset else info$excel_sheet
        export_to_xlsx(df_clean, dataset_name = ds_name, file_path = file.path(dir_dados, "dados_limpos.xlsx"))
        
        r_script_content <- c(
          "# --- SCRIPT DE ANÁLISE DE VARIÂNCIA (ANOVA) ---",
          "# Instalação de pacotes recomendados no RStudio:",
          "# install.packages(c('ggplot2', 'readxl', 'writexl'))",
          "library(ggplot2)",
          "source('scripts/funcoes_anova.R')",
          "",
          "# 1. CARREGAR OS DADOS LIMPOS",
          "load('dados/dados_limpos.rda')",
          "dados <- df_clean",
          "",
          "# 2. AJUSTAR MODELO ANOVA E VERIFICAR PRESSUPOSTOS",
          sprintf("r <- calcular_anova(dados, dep_var = '%s', ind_var = '%s')", 
                  input$var_y, input$var_x),
          "print(mostrar_anova(r))",
          "print(mostrar_pressupostos(r))",
          "",
          "# 3. PÓS-TESTE (TUKEY HSD)",
          "if (r$p_anova < 0.05) {",
          "  print(mostrar_tukey(r))",
          "}",
          "cat(relatar_anova(r))",
          "",
          "# 4. GRÁFICOS",
          sprintf("ggplot(dados, aes(x = as.factor(`%s`), y = `%s`, fill = as.factor(`%s`))) +", input$var_x, input$var_y, input$var_x),
          "  geom_boxplot(alpha = 0.6) +",
          "  geom_jitter(color = '#495057', width = 0.1) +",
          "  stat_summary(fun = mean, geom = 'point', shape = 23, size = 4, fill = 'white') +",
          "  theme_minimal() +",
          "  theme(legend.position = 'none') +",
          "  labs(title = 'Comparação de Médias (ANOVA)')"
        )
        
        writeLines(r_script_content, file.path(dir_scripts, "analise_anova.R"))
        file.copy("templates/custom-reference.docx", file.path(dir_relatorios, "custom-reference.docx"), overwrite = TRUE)
        file.copy("templates/funcoes_anova.R", file.path(dir_scripts, "funcoes_anova.R"), overwrite = TRUE)
        
        custom_qmd_lines <- customize_anova_qmd_params(
          "templates/relatorio_anova.qmd",
          var_y = input$var_y,
          var_x = input$var_x,
          label_y = input$var_y,
          label_x = input$var_x
        )
        writeLines(custom_qmd_lines, file.path(dir_relatorios, "relatorio_anova.qmd"))
        
        rproj_content <- c("Version: 1.0", "RestoreWorkspace: Default", "SaveWorkspace: Default", "Encoding: UTF-8")
        writeLines(rproj_content, file.path(proj_dir, "projeto_analise.Rproj"))
        
        readme_content <- c(
          "PACOTE DE ANÁLISE DE VARIÂNCIA (ANOVA)",
          "- projeto_analise.Rproj: Duplo clique para abrir no RStudio.",
          "- dados/               : Contém os dados limpos em .rda, .csv e .xlsx.",
          "- scripts/analise_anova.R : Script contendo o cálculo da ANOVA e pós-testes."
        )
        writeLines(readme_content, file.path(proj_dir, "README.txt"))
        
        old_wd <- getwd()
        setwd(temp_dir)
        utils::zip(file, files = proj_dir_name)
        setwd(old_wd)
      }
    )
  })
}
