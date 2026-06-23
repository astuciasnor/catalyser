# Módulo de Regressão Linear Simples para IDE_R

mod_regression_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(1, 1, 1),
      style = "grid-template-columns: 2.5fr 7fr 2.5fr !important;",
      
      # COLUNA 1: CONFIGURAÇÃO DO MODELO E RELATÓRIOS
      div(
        card(
          card_header("Configuração do Modelo"),
          card_body(
            style = "padding: 12px 15px;",
            selectInput(ns("var_y"), "Variável Dependente (Y):", choices = NULL),
            div(style = "margin-top: -8px;", selectInput(ns("var_x"), "Variável Independente (X):", choices = NULL))
          )
        ),
        card(
          card_header("Relatório e Pacote de Estudo"),
          card_body(
            style = "padding: 12px 15px;",
            downloadButton(ns("download_report_docx"), "Baixar Relatório Word (.docx)", class = "btn-success w-100"),
            div(style = "margin-top: 8px;"),
            downloadButton(ns("download_project_zip"), "Exportar Projeto R (.zip)", class = "btn-primary w-100"),
            helpText("Gera os relatórios diretamente em DOCX ou exporta um projeto completo em Quarto.", style = "margin-top: 10px; margin-bottom: 0; font-size: 0.85rem;")
          )
        )
      ),
      
      # COLUNA 2: ABAS DE RESULTADOS (PRINCIPAL)
      navset_card_tab(
        id = ns("active_tab"),
        title = "Painel de Resultados",
        nav_panel(
          title = "Tabela de Resultados",
          icon = icon("table"),
          card_body(
            verbatimTextOutput(ns("formula_text")),
            div(style = "margin-bottom: -20px;", DTOutput(ns("coef_table"), height = "auto")),
            hr(style = "margin: 10px 0; border-color: #dee2e6;"),
            uiOutput(ns("metrics_summary"))
          )
        ),
        nav_panel(
          title = "Reta Ajustada",
          icon = icon("chart-line"),
          card_body(
            plotOutput(ns("fit_plot"), height = "450px")
          )
        ),
        nav_panel(
          title = "Resíduos vs Ajustados",
          icon = icon("chart-bar"),
          card_body(
            plotOutput(ns("resid_fit_plot"), height = "450px")
          )
        ),
        nav_panel(
          title = "Normalidade (Q-Q Plot)",
          icon = icon("chart-area"),
          card_body(
            plotOutput(ns("qq_plot"), height = "450px")
          )
        )
      ),
      
      # COLUNA 3: PERSONALIZAÇÃO DA ABA ATIVA
      card(
        card_header("Configurações de Exibição"),
        card_body(
          # Controles mostrados apenas quando abas de gráfico estão selecionadas
          conditionalPanel(
            condition = sprintf("input['%s'] != 'Tabela de Resultados'", ns("active_tab")),
            textInput(ns("custom_title"), "Título do Gráfico:", value = ""),
            textInput(ns("custom_label_x"), "Rótulo Eixo X:", value = ""),
            textInput(ns("custom_label_y"), "Rótulo Eixo Y:", value = ""),
            selectInput(ns("var_group"), "Variável de Agrupamento (Cor):", choices = c("Nenhuma" = "none")),
            conditionalPanel(
              condition = sprintf("input['%s'] != 'none'", ns("var_group")),
              div(style = "display: flex; flex-direction: column; gap: 2px; margin-top: -5px; margin-bottom: 10px;",
                checkboxInput(ns("grp_reg"), "Ajustar reta por grupo (Retas independentes)", value = TRUE),
                div(style = "display: flex; gap: 15px;",
                  checkboxInput(ns("grp_color"), "Mapear Cor", value = TRUE),
                  checkboxInput(ns("grp_fill"), "Mapear Preenchimento", value = TRUE)
                )
              )
            ),
            selectInput(ns("graph_theme"), "Tema do Gráfico:", 
                        choices = c("Mínimo" = "minimal", 
                                    "Clássico" = "classic", 
                                    "Preto e Branco" = "bw", 
                                    "Cinza" = "gray", 
                                    "Light" = "light"), 
                        selected = "minimal"),
            # Mostrar a equação somente na aba de "Reta Ajustada"
            conditionalPanel(
              condition = sprintf("input['%s'] == 'Reta Ajustada'", ns("active_tab")),
              checkboxInput(ns("show_eq"), "Exibir Equação da Reta", value = TRUE)
            )
          ),
          
          # Mensagem informativa para a Tabela de Resultados
          conditionalPanel(
            condition = sprintf("input['%s'] == 'Tabela de Resultados'", ns("active_tab")),
            helpText(HTML(
              "<h5>Resultados Estatísticos</h5>
              <p>Esta aba exibe a fórmula matemática ajustada, a tabela científica de coeficientes (estimativas, erros padrão, estatísticas t e p-valores) e as métricas globais de ajuste:</p>
              <ul>
                <li><b>R²:</b> Percentual de variância de Y explicada por X.</li>
                <li><b>RSE:</b> Medida do desvio padrão dos resíduos.</li>
                <li><b>Estatística F:</b> Teste global de significância do modelo.</li>
              </ul>"
            ))
          )
        )
      )
    )
  )
}

mod_regression_server <- function(id, data_rv, import_info) {
  moduleServer(id, function(input, output, session) {
    
    # Atualiza as escolhas de variáveis com base nos dados importados
    observe({
      df <- data_rv()
      req(df)
      
      # Filtra apenas variáveis numéricas
      num_cols <- names(df)[sapply(df, is.numeric)]
      
      updateSelectInput(session, "var_y", choices = num_cols, selected = num_cols[1])
      updateSelectInput(session, "var_x", choices = num_cols, selected = if (length(num_cols) > 1) num_cols[2] else num_cols[1])
      
      # Atualiza a variável de agrupamento
      all_cols <- names(df)
      updateSelectInput(session, "var_group", choices = c("Nenhuma" = "none", all_cols), selected = "none")
    })
    
    # Quando o usuário muda a aba ativa ou as variáveis, atualizamos os campos para o padrão daquela aba
    observeEvent(list(input$active_tab, input$var_x, input$var_y), {
      req(input$active_tab, input$var_x, input$var_y)
      
      if (input$active_tab == "Reta Ajustada") {
        updateTextInput(session, "custom_title", value = paste("Ajuste Linear:", input$var_y, "vs", input$var_x))
        updateTextInput(session, "custom_label_x", value = input$var_x)
        updateTextInput(session, "custom_label_y", value = input$var_y)
      } else if (input$active_tab == "Resíduos vs Ajustados") {
        updateTextInput(session, "custom_title", value = "Resíduos vs Valores Ajustados")
        updateTextInput(session, "custom_label_x", value = "Valores Ajustados (Fitted)")
        updateTextInput(session, "custom_label_y", value = "Resíduos (Residuals)")
      } else if (input$active_tab == "Normalidade (Q-Q Plot)") {
        updateTextInput(session, "custom_title", value = "Normal Q-Q Plot")
        updateTextInput(session, "custom_label_x", value = "Quantis Teóricos")
        updateTextInput(session, "custom_label_y", value = "Resíduos Padronizados")
      }
    }, ignoreInit = FALSE)
    
    # Modelo estatístico reativo
    model_fit <- reactive({
      df <- data_rv()
      req(df, input$var_x, input$var_y)
      req(input$var_x %in% names(df), input$var_y %in% names(df))
      req(is.numeric(df[[input$var_x]]), is.numeric(df[[input$var_y]]))
      
      # Remover valores ausentes antes de ajustar o modelo
      clean_df <- df[, c(input$var_x, input$var_y), drop = FALSE]
      clean_df <- na.omit(clean_df)
      
      formula_obj <- as.formula(paste(backtick(input$var_y), "~", backtick(input$var_x)))
      lm(formula_obj, data = clean_df)
    })
    
    # Auxiliar para colocar crase em nomes de variáveis com espaços
    backtick <- function(s) {
      paste0("`", s, "`")
    }
    
    # Texto da fórmula ajustada
    output$formula_text <- renderPrint({
      fit <- model_fit()
      req(fit)
      cat("Fórmula do Modelo:\n")
      print(fit$call)
    })
    
    # Tabela de coeficientes científica
    output$coef_table <- renderDT({
      fit <- model_fit()
      req(fit)
      
      sum_fit <- summary(fit)
      coef_matrix <- sum_fit$coefficients
      
      # Converte para data frame legível
      df_coef <- as.data.frame(coef_matrix)
      names(df_coef) <- c("Estimativa", "Erro Padrão", "Valor t", "p-valor")
      df_coef <- cbind(Termo = rownames(df_coef), df_coef)
      
      # Formatação científica da tabela
      datatable(
        df_coef,
        options = list(dom = 't', ordering = FALSE),
        rownames = FALSE,
        selection = 'none'
      ) %>%
        formatRound(columns = c("Estimativa", "Erro Padrão", "Valor t"), digits = 4) %>%
        formatSignif(columns = "p-valor", digits = 4)
    })
    
    # Sumário de métricas de ajuste do modelo
    output$metrics_summary <- renderUI({
      fit <- model_fit()
      req(fit)
      
      sum_fit <- summary(fit)
      r2 <- sum_fit$r.squared
      adj_r2 <- sum_fit$adj.r.squared
      rse <- sum_fit$sigma
      df_residual <- sum_fit$df[2]
      f_stat <- sum_fit$fstatistic
      
      # Formata p-valor da estatística F
      f_p_val <- if (!is.null(f_stat)) {
        pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)
      } else {
        NULL
      }
      
      f_stat_text <- if (!is.null(f_stat)) {
        sprintf("%.4f (GL: %d; %d, p-valor: %s)", 
                f_stat[1], as.integer(f_stat[2]), as.integer(f_stat[3]),
                format.pval(f_p_val, digits = 4))
      } else {
        "N/A"
      }
      
      HTML(paste0(
        "<div style='line-height: 1.3;'>",
        "<p style='margin-bottom: 5px;'><b>Coeficiente de Determinação (R²):</b> ", round(r2, 4), " (", round(r2 * 100, 2), "%)</p>",
        "<p style='margin-bottom: 5px;'><b>R² Ajustado:</b> ", round(adj_r2, 4), "</p>",
        "<p style='margin-bottom: 5px;'><b>Erro Padrão Residual (RSE):</b> ", round(rse, 4), " em ", df_residual, " graus de liberdade</p>",
        "<p style='margin-bottom: 0;'><b>Estatística F:</b> ", f_stat_text, "</p>",
        "</div>"
      ))
    })
    
    # Gráfico 1: Reta Ajustada
    output$fit_plot <- renderPlot({
      df <- data_rv()
      req(df, input$var_x, input$var_y)
      fit <- model_fit()
      req(fit)
      
      # Determina títulos, rótulos e legenda da equação
      if (input$show_eq) {
        if (input$var_group == "none" || !input$grp_reg) {
          coefs <- coef(fit)
          eq_text <- sprintf("Y = %.4f + (%.4f) * X", coefs[1], coefs[2])
        } else {
          df_clean <- df[, c(input$var_x, input$var_y, input$var_group)]
          df_clean <- na.omit(df_clean)
          groups <- unique(df_clean[[input$var_group]])
          eq_text_list <- sapply(groups, function(g) {
            df_sub <- df_clean[df_clean[[input$var_group]] == g, ]
            if (nrow(df_sub) > 2) {
              fit_sub <- lm(as.formula(paste(backtick(input$var_y), "~", backtick(input$var_x))), data = df_sub)
              coefs_sub <- coef(fit_sub)
              sprintf("%s: Y = %.4f + (%.4f) * X", g, coefs_sub[1], coefs_sub[2])
            } else {
              sprintf("%s: N/A", g)
            }
          })
          eq_text <- paste(eq_text_list, collapse = "  |  ")
        }
        subtitle_val <- eq_text
      } else {
        subtitle_val <- NULL
      }
      
      title_val <- if (nzchar(input$custom_title)) input$custom_title else paste("Ajuste Linear:", input$var_y, "vs", input$var_x)
      x_label <- if (nzchar(input$custom_label_x)) input$custom_label_x else input$var_x
      y_label <- if (nzchar(input$custom_label_y)) input$custom_label_y else input$var_y
      
      # Seleciona o tema do ggplot2
      g_theme <- switch(input$graph_theme,
                        "minimal" = theme_minimal(base_size = 14),
                        "classic" = theme_classic(base_size = 14),
                        "bw"      = theme_bw(base_size = 14),
                        "gray"    = theme_gray(base_size = 14),
                        "light"   = theme_light(base_size = 14),
                        theme_minimal(base_size = 14))
      
      # Condicional de agrupamento
      if (input$var_group != "none") {
        df[[input$var_group]] <- as.factor(df[[input$var_group]])
        
        if (input$grp_reg) {
          # Mapeamento estrito para RETAS INDEPENDENTES
          if (input$grp_color && input$grp_fill) {
            p <- ggplot(df, aes(x = .data[[input$var_x]], y = .data[[input$var_y]], group = .data[[input$var_group]], color = .data[[input$var_group]], fill = .data[[input$var_group]])) +
              geom_point(alpha = 0.8, size = 2.5) +
              geom_smooth(method = "lm", formula = y ~ x, linewidth = 1.2)
          } else if (input$grp_color) {
            p <- ggplot(df, aes(x = .data[[input$var_x]], y = .data[[input$var_y]], group = .data[[input$var_group]], color = .data[[input$var_group]])) +
              geom_point(alpha = 0.8, size = 2.5) +
              geom_smooth(method = "lm", formula = y ~ x, fill = "#cfe2ff", linewidth = 1.2)
          } else if (input$grp_fill) {
            p <- ggplot(df, aes(x = .data[[input$var_x]], y = .data[[input$var_y]], group = .data[[input$var_group]], fill = .data[[input$var_group]])) +
              geom_point(color = "#495057", alpha = 0.7, size = 2.5) +
              geom_smooth(method = "lm", formula = y ~ x, color = "#0d6efd", linewidth = 1.2)
          } else {
            p <- ggplot(df, aes(x = .data[[input$var_x]], y = .data[[input$var_y]], group = .data[[input$var_group]])) +
              geom_point(color = "#495057", alpha = 0.7, size = 2.5) +
              geom_smooth(method = "lm", formula = y ~ x, color = "#0d6efd", fill = "#cfe2ff", linewidth = 1.2)
          }
        } else {
          # Ajustar RETA GLOBAL ÚNICA (mesmo que haja agrupamento visual)
          if (input$grp_color) {
            p <- ggplot(df, aes(x = .data[[input$var_x]], y = .data[[input$var_y]])) +
              geom_point(aes(color = .data[[input$var_group]]), alpha = 0.8, size = 2.5) +
              geom_smooth(method = "lm", formula = y ~ x, color = "#0d6efd", fill = "#cfe2ff", linewidth = 1.2)
          } else {
            p <- ggplot(df, aes(x = .data[[input$var_x]], y = .data[[input$var_y]])) +
              geom_point(color = "#495057", alpha = 0.7, size = 2.5) +
              geom_smooth(method = "lm", formula = y ~ x, color = "#0d6efd", fill = "#cfe2ff", linewidth = 1.2)
          }
        }
      } else {
        p <- ggplot(df, aes(x = .data[[input$var_x]], y = .data[[input$var_y]])) +
          geom_point(color = "#495057", alpha = 0.7, size = 2.5) +
          geom_smooth(method = "lm", formula = y ~ x, color = "#0d6efd", fill = "#cfe2ff", linewidth = 1.2)
      }
      
      p + g_theme +
        labs(
          title = title_val,
          subtitle = subtitle_val,
          x = x_label,
          y = y_label
        ) +
        theme(
          plot.title = element_text(face = "bold", size = 16, color = "#212529"),
          plot.subtitle = element_text(color = "#0d6efd", face = "italic", size = 13)
        )
    })
    
    # Gráfico 2: Resíduos vs Ajustados
    output$resid_fit_plot <- renderPlot({
      fit <- model_fit()
      req(fit)
      
      diag_data <- data.frame(
        Ajustados = fitted(fit),
        Residuos = residuals(fit)
      )
      
      # Títulos e rótulos customizados
      title_val <- if (nzchar(input$custom_title)) input$custom_title else "Resíduos vs Valores Ajustados"
      x_label <- if (nzchar(input$custom_label_x)) input$custom_label_x else "Valores Ajustados (Fitted)"
      y_label <- if (nzchar(input$custom_label_y)) input$custom_label_y else "Resíduos (Residuals)"
      
      g_theme <- switch(input$graph_theme,
                        "minimal" = theme_minimal(base_size = 14),
                        "classic" = theme_classic(base_size = 14),
                        "bw"      = theme_bw(base_size = 14),
                        "gray"    = theme_gray(base_size = 14),
                        "light"   = theme_light(base_size = 14),
                        theme_minimal(base_size = 14))
      
      ggplot(diag_data, aes(x = Ajustados, y = Residuos)) +
        geom_point(color = "#495057", alpha = 0.7, size = 2.5) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "#dc3545", linewidth = 1) +
        geom_smooth(method = "loess", formula = y ~ x, color = "#198754", fill = "#d1e7dd", se = FALSE, linewidth = 1) +
        g_theme +
        labs(
          title = title_val,
          x = x_label,
          y = y_label
        ) +
        theme(
          plot.title = element_text(face = "bold", size = 16, color = "#212529")
        )
    })
    
    # Gráfico 3: Normal Q-Q Plot
    output$qq_plot <- renderPlot({
      fit <- model_fit()
      req(fit)
      
      # Calcular resíduos padronizados
      std_resid <- rstandard(fit)
      diag_data <- data.frame(ResiduosStd = std_resid)
      
      # Títulos e rótulos customizados
      title_val <- if (nzchar(input$custom_title)) input$custom_title else "Normal Q-Q Plot"
      x_label <- if (nzchar(input$custom_label_x)) input$custom_label_x else "Quantis Teóricos"
      y_label <- if (nzchar(input$custom_label_y)) input$custom_label_y else "Resíduos Padronizados"
      
      g_theme <- switch(input$graph_theme,
                        "minimal" = theme_minimal(base_size = 14),
                        "classic" = theme_classic(base_size = 14),
                        "bw"      = theme_bw(base_size = 14),
                        "gray"    = theme_gray(base_size = 14),
                        "light"   = theme_light(base_size = 14),
                        theme_minimal(base_size = 14))
      
      ggplot(diag_data, aes(sample = ResiduosStd)) +
        stat_qq(color = "#495057", alpha = 0.7, size = 2.5) +
        stat_qq_line(color = "#0d6efd", size = 1) +
        g_theme +
        labs(
          title = title_val,
          x = x_label,
          y = y_label
        ) +
        theme(
          plot.title = element_text(face = "bold", size = 16, color = "#212529")
        )
    })
    
    # --- EXPORTAR CÓDIGO R ---
    
    # Gera o código R de reprodutibilidade reativamente
    r_code_text <- reactive({
      req(input$var_x, input$var_y, import_info())
      info <- import_info()
      
      # 1. Carregamento de Pacotes
      code <- c(
        "# --- Código de Reprodutibilidade da IDE_R ---",
        "library(ggplot2)",
        "library(readxl)",
        ""
      )
      
      # 2. Carregamento de Dados
      if (info$source == "package") {
        code <- c(code,
          "# Carregar pacote e dataset",
          "if (!requireNamespace('EAPADados', quietly = TRUE)) {",
          "  install.packages('https://github.com/astuciasnor/EAPADados/releases/download/v0.1.1/EAPADados_0.1.1.zip', repos = NULL, type = 'win.binary')",
          "}",
          "library(EAPADados)",
          sprintf("dados <- as.data.frame(%s)", info$package_dataset),
          ""
        )
      } else {
        # Local
        ext <- tolower(tools::file_ext(info$file_name))
        if (ext %in% c("xlsx", "xls")) {
          code <- c(code,
            "# Carregar dados do Excel",
            sprintf("caminho_arquivo <- 'dados/%s'", info$file_name),
            "if (!file.exists(caminho_arquivo)) {",
            sprintf("  caminho_arquivo <- '%s'", info$file_name),
            "}",
            sprintf("dados <- as.data.frame(read_excel(caminho_arquivo, sheet = '%s'))", info$excel_sheet),
            ""
          )
        } else {
          # CSV
          code <- c(code,
            "# Carregar dados do CSV",
            sprintf("caminho_arquivo <- 'dados/%s'", info$file_name),
            "if (!file.exists(caminho_arquivo)) {",
            sprintf("  caminho_arquivo <- '%s'", info$file_name),
            "}",
            sprintf("dados <- read.csv(caminho_arquivo, header = %s, sep = '%s', dec = '%s')", 
                    as.character(info$csv_header), info$csv_sep, info$csv_dec),
            ""
          )
        }
      }
      
      # 3. Ajuste do Modelo
      code <- c(code,
        "# Ajustar modelo de Regressão Linear Simples",
        sprintf("modelo <- lm(`%s` ~ `%s`, data = dados)", input$var_y, input$var_x),
        "print(summary(modelo))",
        ""
      )
      
      # 4. Gráfico ggplot2 com condicional de agrupamento
      theme_code <- switch(input$graph_theme,
                           "minimal" = "theme_minimal(base_size = 14)",
                           "classic" = "theme_classic(base_size = 14)",
                           "bw"      = "theme_bw(base_size = 14)",
                           "gray"    = "theme_gray(base_size = 14)",
                           "light"   = "theme_light(base_size = 14)",
                           "theme_minimal(base_size = 14)")
      
      title_val <- if (nzchar(input$custom_title)) input$custom_title else paste("Ajuste Linear:", input$var_y, "vs", input$var_x)
      x_label <- if (nzchar(input$custom_label_x)) input$custom_label_x else input$var_x
      y_label <- if (nzchar(input$custom_label_y)) input$custom_label_y else input$var_y
      
      if (input$var_group != "none") {
        plot_lines <- c(
          "# Converter variável de agrupamento para fator",
          sprintf("dados$`%s` <- as.factor(dados$`%s`)", input$var_group, input$var_group),
          "",
          "# Gerar gráfico da reta ajustada com ggplot2 (com agrupamento)",
          sprintf("ggplot(dados, aes(x = `%s`, y = `%s`, color = `%s`, fill = `%s`)) +", input$var_x, input$var_y, input$var_group, input$var_group),
          "  geom_point(alpha = 0.8, size = 2.5) +",
          "  geom_smooth(method = 'lm', formula = y ~ x, size = 1.2) +",
          sprintf("  %s +", theme_code),
          "  labs("
        )
      } else {
        plot_lines <- c(
          "# Gerar gráfico da reta ajustada com ggplot2",
          sprintf("ggplot(dados, aes(x = `%s`, y = `%s`)) +", input$var_x, input$var_y),
          "  geom_point(color = '#495057', alpha = 0.7, size = 2.5) +",
          "  geom_smooth(method = 'lm', formula = y ~ x, color = '#0d6efd', fill = '#cfe2ff', size = 1.2) +",
          sprintf("  %s +", theme_code),
          "  labs("
        )
      }
      
      plot_lines <- c(plot_lines, sprintf("    title = '%s',", title_val))
      
      if (input$show_eq) {
        fit <- tryCatch(model_fit(), error = function(e) NULL)
        if (!is.null(fit)) {
          coefs <- coef(fit)
          plot_lines <- c(plot_lines, sprintf("    subtitle = 'Y = %.4f + (%.4f) * X',", coefs[1], coefs[2]))
        } else {
          plot_lines <- c(plot_lines, "    subtitle = 'Equação ajustada',")
        }
      }
      
      plot_lines <- c(plot_lines,
        sprintf("    x = '%s',", x_label),
        sprintf("    y = '%s'", y_label),
        "  ) +",
        "  theme(",
        "    plot.title = element_text(face = 'bold', size = 16, color = '#212529'),",
        "    plot.subtitle = element_text(color = '#0d6efd', face = 'italic', size = 13)",
        "  )"
      )
      
      code <- c(code, plot_lines)
      
      paste(code, collapse = "\n")
    })
    
    # Exibe modal com o código R gerado
    observeEvent(input$export_code, {
      showModal(modalDialog(
        title = "Código R de Reprodutibilidade",
        size = "l",
        easyClose = TRUE,
        fade = TRUE,
        footer = tagList(
          downloadButton(session$ns("download_code"), "Baixar Script (.R)", class = "btn-success"),
          modalButton("Fechar")
        ),
        tagList(
          p("Copie o código R abaixo ou clique no botão de download para salvar o script que reproduz esta análise estatística e gráfico:"),
          verbatimTextOutput(session$ns("r_code_preview"))
        )
      ))
    })
    
    # Exibe a prévia do código no modal
    output$r_code_preview <- renderPrint({
      cat(r_code_text())
    })
    
    # Download do script .R
    output$download_code <- downloadHandler(
      filename = function() {
        paste0("analise_regressao_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".R")
      },
      content = function(file) {
        writeLines(r_code_text(), file)
      }
    )
    
    # --- EXPORTAR RELATÓRIO QUARTO (QMD) ---
    
    # Gera o conteúdo do arquivo Quarto (.qmd) reativamente
    qmd_code_text <- reactive({
      req(input$var_x, input$var_y, import_info())
      info <- import_info()
      
      # Títulos e rótulos atuais
      title_val <- if (nzchar(input$custom_title)) input$custom_title else paste("Ajuste Linear:", input$var_y, "vs", input$var_x)
      x_label <- if (nzchar(input$custom_label_x)) input$custom_label_x else input$var_x
      y_label <- if (nzchar(input$custom_label_y)) input$custom_label_y else input$var_y
      
      theme_code <- switch(input$graph_theme,
                           "minimal" = "theme_minimal(base_size = 12)",
                           "classic" = "theme_classic(base_size = 12)",
                           "bw"      = "theme_bw(base_size = 12)",
                           "gray"    = "theme_gray(base_size = 12)",
                           "light"   = "theme_light(base_size = 12)",
                           "theme_minimal(base_size = 12)")
      
      # YAML Header
      yaml <- c(
        "---",
        sprintf("title: \"Relatório de Regressão Linear Simples: %s vs %s\"", input$var_y, input$var_x),
        "author: \"IDE CatalyseR\"",
        sprintf("date: \"%s\"", format(Sys.Date(), "%d/%m/%Y")),
        "format:",
        "  docx:",
        "    toc: false",
        "    highlight-style: github",
        "---",
        ""
      )
      
      # Setup Chunk
      setup <- c(
        "```{r}",
        "#| label: setup",
        "#| include: false",
        "knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE, fig.width = 6, fig.height = 4, fig.align = 'center')",
        "library(ggplot2)",
        "```",
        ""
      )
      
      # Load Data Chunk
      load_data <- c(
        "```{r}",
        "#| label: load-data",
        "# Carregamento dos dados limpos (preservando fatores e formatação)",
        "load('../dados/dados_limpos.rda')",
        "dados <- df_clean",
        "",
        "# Alternativa em formato aberto CSV (se preferir):",
        "# library(readr)",
        "# dados <- read_csv('../dados/dados_limpos.csv')",
        "",
        "# Ajuste do modelo",
        sprintf("modelo <- lm(`%s` ~ `%s`, data = dados)", input$var_y, input$var_x),
        "```",
        ""
      )
      
      body <- c(
        "## Introdução",
        sprintf("Este relatório apresenta o ajuste do modelo de regressão linear simples para a variável dependente **%s** em função da variável independente **%s**.", input$var_y, input$var_x),
        ""
      )
      
      # 1. Tabela de Coeficientes
      if (input$show_out_coef) {
        body <- c(body,
          "## Tabela de Coeficientes",
          "A tabela abaixo detalha as estimativas dos parâmetros do modelo (coeficiente linear e angular), seus erros padrão, estatísticas de teste t e p-valores correspondentes.",
          "",
          "```{r}",
          "#| label: coef-table",
          "coef_df <- as.data.frame(summary(modelo)$coefficients)",
          "names(coef_df) <- c('Estimativa', 'Erro Padrão', 'Valor t', 'p-valor')",
          "knitr::kable(coef_df, digits = 4, caption = 'Coeficientes do Modelo de Regressão Ajustado')",
          "```",
          ""
        )
      }
      
      # 2. Métricas de Ajuste
      if (input$show_out_metrics) {
        body <- c(body,
          "## Métricas de Ajuste do Modelo",
          "As estatísticas abaixo indicam a qualidade de ajuste global do modelo linear estimado:",
          "",
          "```{r}",
          "#| label: metrics-summary",
          "#| comment: NA",
          "sum_fit <- summary(modelo)",
          "cat(sprintf('- Coeficiente de Determinação (R²): %.4f (%.2f%%)\\n', sum_fit$r.squared, sum_fit$r.squared * 100))",
          "cat(sprintf('- R² Ajustado: %.4f\\n', sum_fit$adj.r.squared))",
          "cat(sprintf('- Erro Padrão Residual (RSE): %.4f em %d graus de liberdade\\n', sum_fit$sigma, sum_fit$df[2]))",
          "if (!is.null(sum_fit$fstatistic)) {",
          "  f_stat <- sum_fit$fstatistic",
          "  f_p_val <- pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)",
          "  cat(sprintf('- Estatística F: %.4f (GL: %d; %d, p-valor: %s)\\n', f_stat[1], f_stat[2], f_stat[3], format.pval(f_p_val, digits=4)))",
          "}",
          "```",
          ""
        )
      }
      
      # 3. Gráfico da Reta Ajustada
      if (input$show_out_fit_plot) {
        # Calcula a equação real se possível
        fit <- tryCatch(model_fit(), error = function(e) NULL)
        subtitle_expr <- if (input$show_eq && !is.null(fit)) {
          coefs <- coef(fit)
          sprintf("Y = %.4f + (%.4f) * X", coefs[1], coefs[2])
        } else {
          ""
        }
        
        if (input$var_group != "none") {
          body <- c(body,
            "## Reta Ajustada",
            "Gráfico de dispersão dos dados observados com a reta de regressão ajustada e intervalo de confiança (com agrupamento).",
            "",
            "```{r}",
            "#| label: fit-plot",
            sprintf("dados$`%s` <- as.factor(dados$`%s`)", input$var_group, input$var_group),
            sprintf("ggplot(dados, aes(x = `%s`, y = `%s`, color = `%s`, fill = `%s`)) +", input$var_x, input$var_y, input$var_group, input$var_group),
            "  geom_point(alpha = 0.8, size = 2.5) +",
            "  geom_smooth(method = 'lm', formula = y ~ x, size = 1.2) +",
            sprintf("  %s +", theme_code),
            "  labs(",
            sprintf("    title = '%s',", title_val),
            if (nzchar(subtitle_expr)) sprintf("    subtitle = '%s',", subtitle_expr) else NULL,
            sprintf("    x = '%s',", x_label),
            sprintf("    y = '%s'", y_label),
            "  ) +",
            "  theme(plot.title = element_text(face = 'bold'))",
            "```",
            ""
          )
        } else {
          body <- c(body,
            "## Reta Ajustada",
            "Gráfico de dispersão dos dados observados com a reta de regressão ajustada e intervalo de confiança.",
            "",
            "```{r}",
            "#| label: fit-plot",
            sprintf("ggplot(dados, aes(x = `%s`, y = `%s`)) +", input$var_x, input$var_y),
            "  geom_point(color = '#495057', alpha = 0.7, size = 2.5) +",
            "  geom_smooth(method = 'lm', formula = y ~ x, color = '#0d6efd', fill = '#cfe2ff', size = 1.2) +",
            sprintf("  %s +", theme_code),
            "  labs(",
            sprintf("    title = '%s',", title_val),
            if (nzchar(subtitle_expr)) sprintf("    subtitle = '%s',", subtitle_expr) else NULL,
            sprintf("    x = '%s',", x_label),
            sprintf("    y = '%s'", y_label),
            "  ) +",
            "  theme(plot.title = element_text(face = 'bold'))",
            "```",
            ""
          )
        }
      }
      
      # 4. Resíduos vs Ajustados
      if (input$show_out_resid_plot) {
        body <- c(body,
          "## Análise de Resíduos (Resíduos vs Valores Ajustados)",
          "Este gráfico de dispersão avalia a linearidade e homocedasticidade dos erros residuais.",
          "",
          "```{r}",
          "#| label: resid-plot",
          "diag_data <- data.frame(Ajustados = fitted(modelo), Residuos = residuals(modelo))",
          "ggplot(diag_data, aes(x = Ajustados, y = Residuos)) +",
          "  geom_point(color = '#495057', alpha = 0.7, size = 2.5) +",
          "  geom_hline(yintercept = 0, linetype = 'dashed', color = '#dc3545', size = 1) +",
          "  geom_smooth(method = 'loess', formula = y ~ x, color = '#198754', fill = '#d1e7dd', se = FALSE, size = 1) +",
          sprintf("  %s +", theme_code),
          "  labs(title = 'Resíduos vs Valores Ajustados', x = 'Valores Ajustados (Fitted)', y = 'Resíduos (Residuals)') +",
          "  theme(plot.title = element_text(face = 'bold'))",
          "```",
          ""
        )
      }
      
      # 5. Q-Q Plot
      if (input$show_out_qq_plot) {
        body <- c(body,
          "## Normalidade (Normal Q-Q Plot)",
          "Gráfico quantil-quantil para verificação visual da suposição de normalidade dos resíduos.",
          "",
          "```{r}",
          "#| label: qq-plot",
          "diag_data_qq <- data.frame(ResiduosStd = rstandard(modelo))",
          "ggplot(diag_data_qq, aes(sample = ResiduosStd)) +",
          "  stat_qq(color = '#495057', alpha = 0.7, size = 2.5) +",
          "  stat_qq_line(color = '#0d6efd', size = 1) +",
          sprintf("  %s +", theme_code),
          "  labs(title = 'Normal Q-Q Plot', x = 'Quantis Teóricos', y = 'Resíduos Padronizados') +",
          "  theme(plot.title = element_text(face = 'bold'))",
          "```",
          ""
        )
      }
      
      # Junta tudo
      all_lines <- c(yaml, setup, load_data, body)
      all_lines <- all_lines[!sapply(all_lines, is.null)]
      paste(all_lines, collapse = "\n")
    })
    
    # Exibe modal com o código .qmd gerado
    observeEvent(input$export_qmd, {
      showModal(modalDialog(
        title = "Código Quarto (.qmd) de Reprodutibilidade",
        size = "l",
        easyClose = TRUE,
        fade = TRUE,
        footer = tagList(
          downloadButton(session$ns("download_qmd"), "Baixar Script (.qmd)", class = "btn-success"),
          modalButton("Fechar")
        ),
        tagList(
          p("Copie o código Quarto (.qmd) abaixo ou faça o download para salvar o arquivo de relatório:"),
          verbatimTextOutput(session$ns("qmd_code_preview"))
        )
      ))
    })
    
    # Exibe a prévia do código no modal
    output$qmd_code_preview <- renderPrint({
      cat(qmd_code_text())
    })
    
    # Download do script .qmd
    output$download_qmd <- downloadHandler(
      filename = function() {
        paste0("relatorio_regressao_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".qmd")
      },
      content = function(file) {
        writeLines(qmd_code_text(), file)
      }
    )
    
    # Função auxiliar para customizar os parâmetros do QMD de regressão
    customize_regression_qmd_params <- function(qmd_path, var_y, var_x, label_y, label_x) {
      lines <- readLines(qmd_path, warn = FALSE)
      lines <- gsub('var_y: ".*"', sprintf('var_y: "%s"', var_y), lines)
      lines <- gsub('var_x: ".*"', sprintf('var_x: "%s"', var_x), lines)
      lines <- gsub('label_y: ".*"', sprintf('label_y: "%s"', label_y), lines)
      lines <- gsub('label_x: ".*"', sprintf('label_x: "%s"', label_x), lines)
      return(lines)
    }

    # Download do Relatório Word (.docx)
    output$download_report_docx <- downloadHandler(
      filename = function() {
        paste0("relatorio_regressao_", format(Sys.Date(), "%Y-%m-%d"), ".docx")
      },
      content = function(file) {
        req(data_rv())
        
        # Criar diretório temporário para compilação
        temp_dir <- tempdir()
        temp_qmd <- file.path(temp_dir, "relatorio_regressao.qmd")
        temp_ref <- file.path(temp_dir, "custom-reference.docx")
        temp_func <- file.path(temp_dir, "funcoes_regressao.R")
        temp_data <- file.path(temp_dir, "dados_limpos.rda")
        
        # Copiar arquivos de templates para o diretório temporário
        file.copy("templates/custom-reference.docx", temp_ref, overwrite = TRUE)
        file.copy("templates/funcoes_regressao.R", temp_func, overwrite = TRUE)
        file.copy("templates/relatorio_regressao.qmd", temp_qmd, overwrite = TRUE)
        
        # Salvar os dados limpos ativos
        df_clean <- data_rv()
        save(df_clean, file = temp_data)
        
        # Obter os nomes das variáveis
        var_y_val <- input$var_y
        var_x_val <- input$var_x
        label_y_val <- paste0("a variável ", var_y_val)
        label_x_val <- paste0("a variável ", var_x_val)
        
        # Customizar e escrever o QMD
        custom_qmd_lines <- customize_regression_qmd_params(
          temp_qmd,
          var_y = var_y_val,
          var_x = var_x_val,
          label_y = label_y_val,
          label_x = label_x_val
        )
        writeLines(custom_qmd_lines, temp_qmd)
        
        # Renderizar o relatório usando quarto CLI
        old_wd <- getwd()
        setwd(temp_dir)
        
        system2("quarto", args = c("render", "relatorio_regressao.qmd", "--to", "docx"))
        
        setwd(old_wd)
        
        # Copiar o arquivo final gerado para a saída
        generated_docx <- file.path(temp_dir, "relatorio_regressao.docx")
        if (file.exists(generated_docx)) {
          file.copy(generated_docx, file, overwrite = TRUE)
        } else {
          writeLines("Erro: Não foi possível renderizar o relatório .docx usando o Quarto CLI.", file)
        }
      }
    )

    # Gerar e baixar pacote de estudo (.zip) contendo scripts, qmd e dados limpos
    output$download_project_zip <- downloadHandler(
      filename = function() {
        paste0("projeto_regressao_linear_simples_", format(Sys.Date(), "%Y-%m-%d"), ".zip")
      },
      content = function(file) {
        info <- import_info()
        # Nome limpo da pasta do projeto
        proj_dir_name <- paste0("projeto_regressao_linear_simples_", format(Sys.Date(), "%Y-%m-%d"))
        # Criar diretórios temporários para o pacote
        temp_dir <- tempdir()
        proj_dir <- file.path(temp_dir, proj_dir_name)
        dir.create(proj_dir, showWarnings = FALSE)
        
        dir_dados <- file.path(proj_dir, "dados")
        dir_scripts <- file.path(proj_dir, "scripts")
        dir_relatorios <- file.path(proj_dir, "relatorios")
        
        dir.create(dir_dados, showWarnings = FALSE)
        dir.create(dir_scripts, showWarnings = FALSE)
        dir.create(dir_relatorios, showWarnings = FALSE)
        
        # 1. Salvar os dados limpos (.rda, .csv e .xlsx)
        df_clean <- data_rv()
        req(df_clean)
        # Salva o data frame com o nome 'dados' dentro do arquivo .rda
        save(df_clean, file = file.path(dir_dados, "dados_limpos.rda"))
        # Salva em formato .csv
        write.csv(df_clean, file = file.path(dir_dados, "dados_limpos.csv"), row.names = FALSE)
        ds_name <- if (info$source == "package") info$package_dataset else info$excel_sheet
        export_to_xlsx(df_clean, dataset_name = ds_name, file_path = file.path(dir_dados, "dados_limpos.xlsx"))
        
        # 2. Gerar o script .R (scripts/analise.R)
        theme_code <- switch(input$graph_theme,
                             "minimal" = "theme_minimal(base_size = 14)",
                             "classic" = "theme_classic(base_size = 14)",
                             "bw"      = "theme_bw(base_size = 14)",
                             "gray"    = "theme_gray(base_size = 14)",
                             "light"   = "theme_light(base_size = 14)",
                             "theme_minimal(base_size = 14)")
        
        title_val <- if (nzchar(input$custom_title)) input$custom_title else paste("Ajuste Linear:", input$var_y, "vs", input$var_x)
        x_label <- if (nzchar(input$custom_label_x)) input$custom_label_x else input$var_x
        y_label <- if (nzchar(input$custom_label_y)) input$custom_label_y else input$var_y
        
        r_script_content <- c(
          "# --- SCRIPT DE ANÁLISE ESTATÍSTICA (IDE_R) ---",
          "# Instalação de pacotes recomendados no RStudio:",
          "# install.packages(c('ggplot2', 'readxl', 'writexl'))",
          "library(ggplot2)",
          "",
          "# 1. CARREGAR OS DADOS LIMPOS",
          "# Se aberto via projeto_analise.Rproj, a pasta de trabalho ativa será a raiz.",
          "# Se rodado diretamente da pasta scripts/, buscaremos no nível superior.",
          "if (file.exists('dados/dados_limpos.rda')) {",
          "  load('dados/dados_limpos.rda')",
          "} else if (file.exists('../dados/dados_limpos.rda')) {",
          "  load('../dados/dados_limpos.rda')",
          "} else {",
          "  stop('Não foi possível encontrar o arquivo dados_limpos.rda. Certifique-se de abrir o projeto clicando em projeto_analise.Rproj.')",
          "}",
          "dados <- df_clean",
          "",
          "# Alternativa em formato aberto Excel (se preferir):",
          "# library(readxl)",
          "# dados <- as.data.frame(read_excel('dados/dados_limpos.xlsx', sheet = 'Dados'))",
          "",
          "# Alternativa em formato aberto CSV:",
          "# dados <- read.csv('dados/dados_limpos.csv', stringsAsFactors = TRUE, check.names = FALSE)",
          "",
          "# 2. AJUSTAR O MODELO LINEAR",
          sprintf("modelo <- lm(`%s` ~ `%s`, data = dados)", input$var_y, input$var_x),
          "print(summary(modelo))",
          "",
          "# 3. GERAR O GRÁFICO DA RETA AJUSTADA",
          if (input$var_group != "none") {
            c(
              sprintf("dados$`%s` <- as.factor(dados$`%s`)", input$var_group, input$var_group),
              sprintf("ggplot(dados, aes(x = `%s`, y = `%s`, color = `%s`, fill = `%s`)) +", input$var_x, input$var_y, input$var_group, input$var_group),
              "  geom_point(alpha = 0.8, size = 2.5) +",
              "  geom_smooth(method = 'lm', formula = y ~ x, size = 1.2) +"
            )
          } else {
            c(
              sprintf("ggplot(dados, aes(x = `%s`, y = `%s`)) +", input$var_x, input$var_y),
              "  geom_point(color = '#495057', alpha = 0.7, size = 2.5) +",
              "  geom_smooth(method = 'lm', formula = y ~ x, color = '#0d6efd', fill = '#cfe2ff', size = 1.2) +"
            )
          },
          sprintf("  %s +", theme_code),
          "  labs(",
          sprintf("    title = '%s',", title_val),
          if (input$show_eq) {
            fit <- tryCatch(model_fit(), error = function(e) NULL)
            if (!is.null(fit)) {
              coefs <- coef(fit)
              sprintf("    subtitle = 'Y = %.4f + (%.4f) * X',", coefs[1], coefs[2])
            } else {
              "    subtitle = 'Equação ajustada',"
            }
          } else {
            NULL
          },
          sprintf("    x = '%s',", x_label),
          sprintf("    y = '%s'", y_label),
          "  ) +",
          "  theme(",
          "    plot.title = element_text(face = 'bold', size = 16, color = '#212529'),",
          "    plot.subtitle = element_text(color = '#0d6efd', face = 'italic', size = 13)",
          "  )",
          "",
          "# 4. GRÁFICO DE RESÍDUOS VS VALORES AJUSTADOS",
          "diag_data <- data.frame(Ajustados = fitted(modelo), Residuos = residuals(modelo))",
          "ggplot(diag_data, aes(x = Ajustados, y = Residuos)) +",
          "  geom_point(color = '#495057', alpha = 0.7, size = 2.5) +",
          "  geom_hline(yintercept = 0, linetype = 'dashed', color = '#dc3545', size = 1) +",
          "  geom_smooth(method = 'loess', formula = y ~ x, color = '#198754', fill = '#d1e7dd', se = FALSE, size = 1) +",
          sprintf("  %s +", theme_code),
          "  labs(title = 'Resíduos vs Valores Ajustados', x = 'Valores Ajustados (Fitted)', y = 'Resíduos (Residuals)') +",
          "  theme(plot.title = element_text(face = 'bold', size = 16, color = '#212529'))",
          "",
          "# 5. GRÁFICO DE NORMALIDADE (Q-Q PLOT)",
          "diag_data_qq <- data.frame(ResiduosStd = rstandard(modelo))",
          "ggplot(diag_data_qq, aes(sample = ResiduosStd)) +",
          "  stat_qq(color = '#495057', alpha = 0.7, size = 2.5) +",
          "  stat_qq_line(color = '#0d6efd', size = 1) +",
          sprintf("  %s +", theme_code),
          "  labs(title = 'Normal Q-Q Plot', x = 'Quantis Teóricos', y = 'Resíduos Padronizados') +",
          "  theme(plot.title = element_text(face = 'bold', size = 16, color = '#212529'))"
        )
        r_script_content <- r_script_content[!sapply(r_script_content, is.null)]
        writeLines(paste(r_script_content, collapse = "\n"), file.path(dir_scripts, "analise.R"))
        
        # Copiar arquivos de templates
        file.copy("templates/custom-reference.docx", file.path(dir_relatorios, "custom-reference.docx"), overwrite = TRUE)
        file.copy("templates/funcoes_regressao.R", file.path(dir_scripts, "funcoes_regressao.R"), overwrite = TRUE)
        
        # Obter os nomes das variáveis
        var_y_val <- input$var_y
        var_x_val <- input$var_x
        label_y_val <- paste0("a variável ", var_y_val)
        label_x_val <- paste0("a variável ", var_x_val)
        
        # Customizar e escrever o QMD
        custom_qmd_lines <- customize_regression_qmd_params(
          "templates/relatorio_regressao.qmd",
          var_y = var_y_val,
          var_x = var_x_val,
          label_y = label_y_val,
          label_x = label_x_val
        )
        writeLines(custom_qmd_lines, file.path(dir_relatorios, "relatorio_regressao.qmd"))
        
        # 4. Criar arquivo de Projeto do RStudio (.Rproj)
        rproj_content <- c(
          "Version: 1.0",
          "",
          "RestoreWorkspace: Default",
          "SaveWorkspace: Default",
          "AlwaysSaveHistory: Default",
          "",
          "EnableCodeIndexing: Yes",
          "UseSpacesForTab: Yes",
          "NumSpacesForTab: 2",
          "Encoding: UTF-8",
          "",
          "RnwWeave: Sweave",
          "LaTeX: pdfLaTeX"
        )
        writeLines(rproj_content, file.path(proj_dir, "projeto_analise.Rproj"))
        
        # 5. Criar README.txt
        readme_content <- c(
          "===========================================================",
          " PACOTE DE ANÁLISE REPRODUTÍVEL (IDE_R CIENTÍFICA)",
          "===========================================================",
          "",
          "Parabéns! Você exportou um projeto de análise completo da IDE_R.",
          "Este pacote contém a estrutura perfeita para você começar a programar",
          "em R e Quarto diretamente em seu computador.",
          "",
          "ESTRUTURA DE PASTAS E ARQUIVOS:",
          "- projeto_analise.Rproj: Arquivo de projeto do RStudio. Dê duplo clique nele!",
          "- dados/               : Contém os dados limpos exportados em formatos .rda, .csv e .xlsx.",
          "- scripts/             : Contém scripts e funções de apoio.",
          "  - scripts/analise.R  : Script com o código de cálculo e gráficos.",
          "  - scripts/funcoes_regressao.R : Funções de formatação e relato.",
          "- relatorios/          : Contém o arquivo Quarto ('relatorio_regressao.qmd') para geração de relatórios.",
          "- README.txt           : Este arquivo de instruções.",
          "",
          "COMO USAR E CONTINUAR SEUS ESTUDOS:",
          "1. Dê um duplo clique no arquivo 'projeto_analise.Rproj' para abrir o projeto diretamente no RStudio.",
          "   Isso definirá automaticamente o diretório de trabalho correto, facilitando os caminhos!",
          "2. Para rodar a análise básica e ver o código R:",
          "   - Com o RStudio aberto pelo projeto, abra o arquivo 'scripts/analise.R' e execute as linhas.",
          "3. Para compilar seu relatório em múltiplos formatos (HTML, Word DOCX ou Typst PDF):",
          "   - Abra o arquivo 'relatorios/relatorio_regressao.qmd'.",
          "   - Certifique-se de que possui o Quarto instalado em sua máquina.",
          "   - Clique no botão 'Render' no topo do editor do RStudio.",
          "   - O Quarto gerará o relatório no formato escolhido. Você pode configurar o formato sob a seção 'format' no cabeçalho YAML do arquivo .qmd.",
          "",
          "Bons estudos! A programação em R abre portas incríveis para a ciência de dados.",
          "IDE CatalyseR - Estatística Aplicada"
        )
        writeLines(readme_content, file.path(proj_dir, "README.txt"))
        
        # 6. Compactar em arquivo ZIP mantendo a pasta principal no topo
        old_wd <- getwd()
        setwd(temp_dir)
        utils::zip(file, files = proj_dir_name)
        setwd(old_wd)
      }
    )
  })
}
