# Módulo de Teste t de Student para IDE_R (Testes Paramétricos)

mod_parametric_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(1, 1, 1),
      style = "grid-template-columns: 2.5fr 7fr 2.5fr !important;",
      
      # COLUNA 1: CONFIGURAÇÃO DO TESTE
      div(
        card(
          card_header("Configuração do Teste t"),
          card_body(
            style = "padding: 12px 15px;",
            selectInput(ns("test_type"), "Tipo de Teste t:",
                        choices = c("Uma Amostra" = "one_val",
                                    "Duas Amostras Independentes" = "two_ind",
                                    "Amostras Pareadas (Antes vs Depois)" = "paired")),
            
            # Condicionais para seleção de variáveis
            conditionalPanel(
              condition = sprintf("input['%s'] == 'one_val'", ns("test_type")),
              selectInput(ns("one_var_y"), "Variável Numérica:", choices = NULL),
              numericInput(ns("one_mu"), "Média Hipotética (μ0):", value = 0, step = 0.5)
            ),
            
            conditionalPanel(
              condition = sprintf("input['%s'] == 'two_ind'", ns("test_type")),
              selectInput(ns("two_var_y"), "Variável Dependente (Numérica):", choices = NULL),
              selectInput(ns("two_var_x"), "Variável de Agrupamento (Categórica):", choices = NULL),
              checkboxInput(ns("two_var_equal"), "Assumir Variâncias Iguais (Homocedasticidade)", value = FALSE)
            ),
            
            conditionalPanel(
              condition = sprintf("input['%s'] == 'paired'", ns("test_type")),
              selectInput(ns("pair_var_y1"), "Variável 1 (Antes):", choices = NULL),
              selectInput(ns("pair_var_y2"), "Variável 2 (Depois):", choices = NULL)
            ),
            
            hr(style = "margin: 8px 0;"),
            
            # Opções compartilhadas do teste
            selectInput(ns("alternative"), "Hipótese Alternativa (H1):",
                        choices = c("Bilateral (≠)" = "two.sided",
                                    "Unilateral Direita (>)" = "greater",
                                    "Unilateral Esquerda (<)" = "less")),
            
            numericInput(ns("conf_level"), "Nível de Confiança (%):", value = 95, min = 80, max = 99, step = 1)
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
      
      # COLUNA 2: RESULTADOS (TABELA PRINCIPAL / GRÁFICOS)
      navset_card_tab(
        id = ns("active_tab"),
        title = "Painel de Resultados",
        nav_panel(
          title = "Tabela de Resultados",
          icon = icon("table"),
          card_body(
            verbatimTextOutput(ns("hypothesis_text")),
            div(style = "margin-bottom: -20px;", DTOutput(ns("results_table"), height = "auto")),
            hr(style = "margin: 15px 0; border-color: #dee2e6;"),
            uiOutput(ns("results_summary"))
          )
        ),
        nav_panel(
          title = "Gráfico do Teste",
          icon = icon("chart-line"),
          card_body(
            plotOutput(ns("test_plot"), height = "450px")
          )
        ),
        nav_panel(
          title = "Distribuição t",
          icon = icon("square-poll-horizontal"),
          card_body(
            plotOutput(ns("dist_plot"), height = "450px")
          )
        ),
        nav_panel(
          title = "Verificação de Pressupostos",
          icon = icon("check-double"),
          card_body(
            h6("Teste de Normalidade (Shapiro-Wilk)", style = "font-weight: 700; color: #0d6efd; margin-bottom: 5px;"),
            verbatimTextOutput(ns("normality_test_out")),
            hr(style = "margin: 10px 0; border-color: #dee2e6;"),
            h6("Gráfico de Normalidade (Q-Q Plot)", style = "font-weight: 700; color: #0d6efd; margin-bottom: 10px;"),
            plotOutput(ns("qq_plot"), height = "300px")
          )
        )
      ),
      
      # COLUNA 3: PERSONALIZAÇÃO DA ABA ATIVA
      card(
        card_header("Configurações de Exibição"),
        card_body(
          # Controles de customização visual para gráficos
          conditionalPanel(
            condition = sprintf("input['%s'] != 'Tabela de Resultados'", ns("active_tab")),
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
          
          # Mensagem informativa para a Tabela de Resultados
          conditionalPanel(
            condition = sprintf("input['%s'] == 'Tabela de Resultados'", ns("active_tab")),
            helpText(HTML(
              "<h5>Interpretação do Teste t</h5>
              <p>O Teste t de Student compara a média observada com uma média de referência (ou entre grupos) para verificar se a diferença é estatisticamente significativa.</p>
              <ul>
                <li><b>p-valor < α (normalmente 0.05):</b> Rejeita-se H0. A diferença é estatisticamente significativa.</li>
                <li><b>p-valor ≥ α:</b> Não se rejeita H0. Não há evidência suficiente de diferença significativa.</li>
                <li><b>Intervalo de Confiança:</b> Se contiver zero (nos testes de comparação de grupos ou de diferenças), sugere que a diferença média pode ser nula.</li>
              </ul>"
            ))
          )
        )
      )
    )
  )
}

plot_t_distribution <- function(df_val, t_calc, alternative, alpha, g_theme, title_val, x_label, y_label) {
  # Define o limite do eixo x: de -4.5 a 4.5 ou maior se o t calculado for extremo
  x_limit <- max(4.5, abs(t_calc) + 1.5)
  x_seq <- seq(-x_limit, x_limit, length.out = 500)
  y_seq <- dt(x_seq, df = df_val)
  df_dist <- data.frame(x = x_seq, y = y_seq)
  
  # Gráfico base da curva t
  p <- ggplot(df_dist, aes(x = x, y = y)) +
    geom_line(color = "gray30", linewidth = 1)
  
  # Sombreia a região de rejeição (região crítica)
  if (alternative == "two.sided") {
    t_crit_up <- qt(1 - alpha/2, df = df_val)
    t_crit_low <- qt(alpha/2, df = df_val)
    
    df_low <- subset(df_dist, x <= t_crit_low)
    p <- p + geom_area(data = df_low, aes(x = x, y = y), fill = "#dc3545", alpha = 0.4)
    
    df_up <- subset(df_dist, x >= t_crit_up)
    p <- p + geom_area(data = df_up, aes(x = x, y = y), fill = "#dc3545", alpha = 0.4)
    
    p <- p + 
      geom_vline(xintercept = c(t_crit_low, t_crit_up), linetype = "dashed", color = "#dc3545", linewidth = 0.8) +
      annotate("text", x = t_crit_up + 0.35 * (x_limit/4.5), y = max(y_seq)*0.7, label = sprintf("t(crítico) =\n%.3f", t_crit_up), color = "#dc3545", fontface = "bold", size = 3.5) +
      annotate("text", x = t_crit_low - 0.35 * (x_limit/4.5), y = max(y_seq)*0.7, label = sprintf("t(crítico) =\n%.3f", t_crit_low), color = "#dc3545", fontface = "bold", size = 3.5)
  } else if (alternative == "greater") {
    t_crit <- qt(1 - alpha, df = df_val)
    
    df_up <- subset(df_dist, x >= t_crit)
    p <- p + geom_area(data = df_up, aes(x = x, y = y), fill = "#dc3545", alpha = 0.4)
    
    p <- p + 
      geom_vline(xintercept = t_crit, linetype = "dashed", color = "#dc3545", linewidth = 0.8) +
      annotate("text", x = t_crit + 0.4 * (x_limit/4.5), y = max(y_seq)*0.7, label = sprintf("t(crítico) =\n%.3f", t_crit), color = "#dc3545", fontface = "bold", size = 3.5)
  } else if (alternative == "less") {
    t_crit <- qt(alpha, df = df_val)
    
    df_low <- subset(df_dist, x <= t_crit)
    p <- p + geom_area(data = df_low, aes(x = x, y = y), fill = "#dc3545", alpha = 0.4)
    
    p <- p + 
      geom_vline(xintercept = t_crit, linetype = "dashed", color = "#dc3545", linewidth = 0.8) +
      annotate("text", x = t_crit - 0.4 * (x_limit/4.5), y = max(y_seq)*0.7, label = sprintf("t(crítico) =\n%.3f", t_crit), color = "#dc3545", fontface = "bold", size = 3.5)
  }
  
  # Adiciona a linha do t calculado
  p <- p + 
    geom_vline(xintercept = t_calc, color = "#0d6efd", linewidth = 1.2) +
    annotate("label", x = t_calc, y = max(y_seq)*0.9, label = sprintf("t(calculado) = %.3f", t_calc), 
             fill = "white", color = "#0d6efd", fontface = "bold", label.size = 0.5, size = 4)
  
  p + g_theme +
    labs(x = x_label, y = y_label, title = title_val,
         subtitle = sprintf("Graus de Liberdade (df) = %g | Área Vermelha = Região de Rejeição (α = %g)", df_val, alpha))
}

mod_parametric_server <- function(id, data_rv, import_info) {
  moduleServer(id, function(input, output, session) {
    
    # Auxiliar para colocar crase em nomes de variáveis com espaços
    backtick <- function(s) {
      paste0("`", s, "`")
    }
    
    # Atualiza as escolhas de variáveis com base nos dados importados
    observe({
      df <- data_rv()
      req(df)
      
      num_cols <- names(df)[sapply(df, is.numeric)]
      all_cols <- names(df)
      cat_cols <- all_cols[!sapply(df, is.numeric) | sapply(df, function(col) length(unique(col)) < 10)]
      
      updateSelectInput(session, "one_var_y", choices = num_cols, selected = num_cols[1])
      
      updateSelectInput(session, "two_var_y", choices = num_cols, selected = num_cols[1])
      updateSelectInput(session, "two_var_x", choices = cat_cols, selected = if (length(cat_cols) > 0) cat_cols[1] else NULL)
      
      updateSelectInput(session, "pair_var_y1", choices = num_cols, selected = num_cols[1])
      updateSelectInput(session, "pair_var_y2", choices = num_cols, selected = if (length(num_cols) > 1) num_cols[2] else num_cols[1])
    })
    
    # Atualiza títulos e eixos automaticamente dependendo do teste e da aba ativa
    observeEvent(list(input$active_tab, input$test_type, input$one_var_y, input$two_var_y, input$two_var_x, input$pair_var_y1, input$pair_var_y2, input$one_mu), {
      req(input$active_tab, input$test_type)
      
      if (input$active_tab == "Gráfico do Teste") {
        if (input$test_type == "one_val") {
          req(input$one_var_y)
          updateTextInput(session, "custom_title", value = paste("Distribuição de", input$one_var_y, "vs Média Hipotética (μ0 =", input$one_mu, ")"))
          updateTextInput(session, "custom_label_x", value = "")
          updateTextInput(session, "custom_label_y", value = input$one_var_y)
        } else if (input$test_type == "two_ind") {
          req(input$two_var_y, input$two_var_x)
          updateTextInput(session, "custom_title", value = paste("Comparação de", input$two_var_y, "por", input$two_var_x))
          updateTextInput(session, "custom_label_x", value = input$two_var_x)
          updateTextInput(session, "custom_label_y", value = input$two_var_y)
        } else if (input$test_type == "paired") {
          req(input$pair_var_y1, input$pair_var_y2)
          updateTextInput(session, "custom_title", value = paste("Comparação Pareada:", input$pair_var_y1, "vs", input$pair_var_y2))
          updateTextInput(session, "custom_label_x", value = "Condição")
          updateTextInput(session, "custom_label_y", value = "Valores")
        }
      } else if (input$active_tab == "Distribuição t") {
        updateTextInput(session, "custom_title", value = "Distribuição t de Student e Regiões Críticas")
        updateTextInput(session, "custom_label_x", value = "Valores de t")
        updateTextInput(session, "custom_label_y", value = "Densidade de Probabilidade")
      } else if (input$active_tab == "Verificação de Pressupostos") {
        updateTextInput(session, "custom_title", value = "Normal Q-Q Plot (Resíduos/Diferenças)")
        updateTextInput(session, "custom_label_x", value = "Quantis Teóricos")
        updateTextInput(session, "custom_label_y", value = "Resíduos Padronizados")
      }
    }, ignoreInit = FALSE)
    
    # Cálculo reativo do Teste t de Student
    test_results <- reactive({
      df <- data_rv()
      req(df, input$test_type)
      
      conf_level_decimal <- input$conf_level / 100
      alternative_val <- input$alternative
      
      if (input$test_type == "one_val") {
        req(input$one_var_y)
        req(input$one_var_y %in% names(df))
        x <- df[[input$one_var_y]]
        x_clean <- x[!is.na(x)]
        req(length(x_clean) > 2)
        
        req(!is.null(input$one_mu), !is.na(input$one_mu))
        t_out <- t.test(x_clean, mu = input$one_mu, alternative = alternative_val, conf.level = conf_level_decimal)
        
        # Obter dados para pressupostos (a própria variável centrada na média)
        norm_data <- x_clean - mean(x_clean)
        
        list(t_out = t_out, norm_data = norm_data, type = "one_val", var_names = c(input$one_var_y))
        
      } else if (input$test_type == "two_ind") {
        req(input$two_var_y, input$two_var_x)
        req(input$two_var_y %in% names(df), input$two_var_x %in% names(df))
        
        df_clean <- df[, c(input$two_var_y, input$two_var_x)]
        df_clean <- na.omit(df_clean)
        df_clean[[input$two_var_x]] <- as.factor(df_clean[[input$two_var_x]])
        
        # Verifica se o fator tem exatamente 2 níveis
        levels_gp <- levels(df_clean[[input$two_var_x]])
        req(length(levels_gp) == 2)
        
        formula_obj <- as.formula(paste(backtick(input$two_var_y), "~", backtick(input$two_var_x)))
        t_out <- t.test(formula_obj, data = df_clean, alternative = alternative_val, 
                        conf.level = conf_level_decimal, var.equal = input$two_var_equal)
        
        # Obter resíduos do modelo linear para verificar normalidade
        fit_lm <- lm(formula_obj, data = df_clean)
        norm_data <- rstandard(fit_lm)
        
        list(t_out = t_out, norm_data = norm_data, type = "two_ind", var_names = c(input$two_var_y, input$two_var_x))
        
      } else if (input$test_type == "paired") {
        req(input$pair_var_y1, input$pair_var_y2)
        req(input$pair_var_y1 %in% names(df), input$pair_var_y2 %in% names(df))
        
        df_clean <- df[, c(input$pair_var_y1, input$pair_var_y2)]
        df_clean <- na.omit(df_clean)
        req(nrow(df_clean) > 2)
        
        x1 <- df_clean[[input$pair_var_y1]]
        x2 <- df_clean[[input$pair_var_y2]]
        
        t_out <- t.test(x1, x2, paired = TRUE, alternative = alternative_val, conf.level = conf_level_decimal)
        
        # Na análise pareada, a hipótese de normalidade se aplica às diferenças d = x1 - x2
        differences <- x1 - x2
        norm_data <- differences - mean(differences)
        
        list(t_out = t_out, norm_data = norm_data, type = "paired", var_names = c(input$pair_var_y1, input$pair_var_y2))
      }
    })
    
    # Exibe as hipóteses estatísticas na tela
    output$hypothesis_text <- renderPrint({
      res <- test_results()
      req(res)
      
      alt <- input$alternative
      alt_symbol <- switch(alt, "two.sided" = "≠", "greater" = ">", "less" = "<")
      
      cat("Hipóteses Estatísticas:\n")
      if (res$type == "one_val") {
        cat(sprintf("  H0: Média de %s = %s\n", res$var_names[1], input$one_mu))
        cat(sprintf("  H1: Média de %s %s %s\n", res$var_names[1], alt_symbol, input$one_mu))
      } else if (res$type == "two_ind") {
        cat(sprintf("  H0: Média de %s (%s) = Média de %s (%s)\n", 
                    res$var_names[1], "Grupo 1", res$var_names[1], "Grupo 2"))
        cat(sprintf("  H1: Média de %s (%s) %s Média de %s (%s)\n", 
                    res$var_names[1], "Grupo 1", alt_symbol, res$var_names[1], "Grupo 2"))
      } else if (res$type == "paired") {
        cat(sprintf("  H0: Média das diferenças (%s - %s) = 0\n", res$var_names[1], res$var_names[2]))
        cat(sprintf("  H1: Média das diferenças (%s - %s) %s 0\n", res$var_names[1], res$var_names[2], alt_symbol))
      }
    })
    
    # Renderiza tabela DT de resultados
    output$results_table <- renderDT({
      res <- test_results()
      req(res)
      t_out <- res$t_out
      
      # Formatar valores individualmente para evitar coersão de tipos e erros no DT
      val_statistic <- sprintf("%.4f", t_out$statistic)
      val_df <- sprintf("%g", t_out$parameter)
      val_p <- format.pval(t_out$p.value, digits = 4, eps = 1e-4)
      
      val_mean <- if (res$type == "two_ind") {
        paste0(names(t_out$estimate)[1], ": ", sprintf("%.4f", t_out$estimate[1]), " | ", 
               names(t_out$estimate)[2], ": ", sprintf("%.4f", t_out$estimate[2]))
      } else {
        sprintf("%.4f", t_out$estimate)
      }
      
      val_conf_low <- sprintf("%.4f", t_out$conf.int[1])
      val_conf_high <- sprintf("%.4f", t_out$conf.int[2])
      
      # Organizar estatísticas em um dataframe limpo
      df_res <- data.frame(
        "Métrica" = c("Estatística t", "Graus de Liberdade (df)", "p-valor", 
                       "Média Amostral", "Limite Inf. IC", "Limite Sup. IC"),
        "Valor" = c(
          val_statistic,
          val_df,
          val_p,
          val_mean,
          val_conf_low,
          val_conf_high
        ),
        stringsAsFactors = FALSE
      )
      
      datatable(
        df_res,
        options = list(dom = 't', ordering = FALSE),
        rownames = FALSE,
        selection = 'none'
      )
    })
    
    # Resumo descritivo e interpretação textual
    output$results_summary <- renderUI({
      res <- test_results()
      req(res)
      t_out <- res$t_out
      
      p_val <- t_out$p.value
      sig_level <- 0.05
      
      is_significant <- p_val < sig_level
      
      interpretation <- if (is_significant) {
        sprintf("<div class='alert alert-success' style='padding: 10px; border-radius: 8px; font-size: 0.92rem; margin-bottom: 0;'>
                 <b>Resultado Significativo (p < 0.05):</b> Rejeitamos a hipótese nula H0 com um nível de significância de 5%%. 
                 A diferença observada é estatisticamente significativa (p = %s).</div>", format.pval(p_val, digits = 4))
      } else {
        sprintf("<div class='alert alert-secondary' style='padding: 10px; border-radius: 8px; font-size: 0.92rem; margin-bottom: 0;'>
                 <b>Não Significativo (p ≥ 0.05):</b> Não há evidências estatísticas para rejeitar H0 (p = %s). 
                 A diferença observada não é estatisticamente significativa.</div>", format.pval(p_val, digits = 4))
      }
      
      # Informações de estimativa pontual
      estimates_html <- if (res$type == "one_val") {
        sprintf("<p style='margin-bottom: 5px;'>A média amostral estimada para <b>%s</b> é <b>%.4f</b>, comparada à média hipotética de <b>%.4f</b>.</p>", 
                res$var_names[1], t_out$estimate, input$one_mu)
      } else if (res$type == "two_ind") {
        sprintf("<p style='margin-bottom: 5px;'>As médias estimadas dos grupos são <b>%.4f</b> (para %s) e <b>%.4f</b> (para %s).</p>",
                t_out$estimate[1], names(t_out$estimate)[1], t_out$estimate[2], names(t_out$estimate)[2])
      } else if (res$type == "paired") {
        sprintf("<p style='margin-bottom: 5px;'>A média estimada das diferenças pareadas (%s - %s) é <b>%.4f</b>.</p>",
                res$var_names[1], res$var_names[2], t_out$estimate)
      }
      
      # Informações de intervalo de confiança
      conf_html <- sprintf("<p style='margin-bottom: 12px;'>O intervalo de confiança de %d%% para a diferença média é [<b>%.4f</b>, <b>%.4f</b>].</p>",
                           input$conf_level, t_out$conf.int[1], t_out$conf.int[2])
      
      HTML(paste0(
        "<div style='line-height: 1.45;'>",
        estimates_html,
        conf_html,
        interpretation,
        "</div>"
      ))
    })
    
    # Gráficos de Visualização do Teste t
    output$test_plot <- renderPlot({
      df <- data_rv()
      req(df, input$test_type)
      
      title_val <- if (nzchar(input$custom_title)) input$custom_title else "Gráfico do Teste"
      x_label <- if (nzchar(input$custom_label_x)) input$custom_label_x else ""
      y_label <- if (nzchar(input$custom_label_y)) input$custom_label_y else "Valores"
      
      g_theme <- switch(input$graph_theme,
                        "minimal" = theme_minimal(base_size = 14),
                        "classic" = theme_classic(base_size = 14),
                        "bw"      = theme_bw(base_size = 14),
                        "gray"    = theme_gray(base_size = 14),
                        "light"   = theme_light(base_size = 14),
                        theme_minimal(base_size = 14))
      
      g_theme <- g_theme + theme(plot.title = element_text(face = "bold", size = 16, color = "#212529"))
      
      if (input$test_type == "one_val") {
        req(input$one_var_y)
        df_clean <- df[!is.na(df[[input$one_var_y]]), ]
        mean_val <- mean(df_clean[[input$one_var_y]])
        
        # Histograma com densidade e linhas para média amostral e de referência
        # Ou um Boxplot elegante mostrando os pontos individuais
        ggplot(df_clean, aes(x = "", y = .data[[input$one_var_y]])) +
          geom_boxplot(fill = "#cfe2ff", color = "#0d6efd", alpha = 0.7, outlier.color = NA) +
          geom_jitter(color = "#495057", width = 0.15, alpha = 0.5, size = 2) +
          # Média amostral em Azul
          geom_hline(aes(yintercept = mean_val, color = "Média Amostral"), linetype = "solid", linewidth = 1.2) +
          # Média hipotética em Vermelho
          geom_hline(aes(yintercept = input$one_mu, color = "Média Hipotética (μ0)"), linetype = "dashed", linewidth = 1.2) +
          scale_color_manual(name = "Linhas de Referência",
                             values = c("Média Amostral" = "#0d6efd", "Média Hipotética (μ0)" = "#dc3545")) +
          g_theme +
          labs(title = title_val, x = x_label, y = y_label) +
          theme(legend.position = "bottom")
        
      } else if (input$test_type == "two_ind") {
        req(input$two_var_y, input$two_var_x)
        df_clean <- df[, c(input$two_var_y, input$two_var_x)]
        df_clean <- na.omit(df_clean)
        df_clean[[input$two_var_x]] <- as.factor(df_clean[[input$two_var_x]])
        
        # Boxplot comparativo entre grupos com médias indicadas
        ggplot(df_clean, aes(x = .data[[input$two_var_x]], y = .data[[input$two_var_y]], fill = .data[[input$two_var_x]])) +
          geom_boxplot(alpha = 0.7, outlier.color = NA) +
          geom_jitter(color = "#495057", width = 0.15, alpha = 0.5, size = 2) +
          stat_summary(fun = mean, geom = "point", shape = 23, size = 4, fill = "white", color = "black") +
          g_theme +
          labs(title = title_val, x = x_label, y = y_label) +
          theme(legend.position = "none")
        
      } else if (input$test_type == "paired") {
        req(input$pair_var_y1, input$pair_var_y2)
        df_clean <- df[, c(input$pair_var_y1, input$pair_var_y2)]
        df_clean <- na.omit(df_clean)
        df_clean$ID <- 1:nrow(df_clean)
        
        # Converter para formato longo para plotar antes/depois pareado
        df_long <- data.frame(
          ID = rep(df_clean$ID, 2),
          Condicao = factor(rep(c(input$pair_var_y1, input$pair_var_y2), each = nrow(df_clean)), 
                            levels = c(input$pair_var_y1, input$pair_var_y2)),
          Valores = c(df_clean[[input$pair_var_y1]], df_clean[[input$pair_var_y2]])
        )
        
        # Gráfico de linhas conectando observações pareadas e boxplot leve
        ggplot(df_long, aes(x = Condicao, y = Valores, group = ID)) +
          geom_line(color = "gray70", alpha = 0.6) +
          geom_point(aes(color = Condicao), size = 2.5, alpha = 0.8) +
          geom_boxplot(aes(group = Condicao), fill = NA, color = "black", outlier.color = NA, width = 0.3) +
          g_theme +
          labs(title = title_val, x = x_label, y = y_label) +
          theme(legend.position = "none")
      }
    })
    
    # Gráfico de Distribuição t Teórica
    output$dist_plot <- renderPlot({
      res <- test_results()
      req(res)
      t_out <- res$t_out
      
      df_val <- t_out$parameter
      t_calc <- t_out$statistic
      alternative_val <- input$alternative
      alpha_val <- 1 - (input$conf_level / 100)
      
      title_val <- if (nzchar(input$custom_title)) input$custom_title else "Distribuição t de Student e Regiões Críticas"
      x_label <- if (nzchar(input$custom_label_x)) input$custom_label_x else "Valores de t"
      y_label <- if (nzchar(input$custom_label_y)) input$custom_label_y else "Densidade de Probabilidade"
      
      g_theme <- switch(input$graph_theme,
                        "minimal" = theme_minimal(base_size = 14),
                        "classic" = theme_classic(base_size = 14),
                        "bw"      = theme_bw(base_size = 14),
                        "gray"    = theme_gray(base_size = 14),
                        "light"   = theme_light(base_size = 14),
                        theme_minimal(base_size = 14))
      
      g_theme <- g_theme + theme(plot.title = element_text(face = "bold", size = 16, color = "#212529"))
      
      plot_t_distribution(df_val, t_calc, alternative_val, alpha_val, g_theme, title_val, x_label, y_label)
    })
    
    # Teste de Normalidade de Shapiro-Wilk (Analítico)
    output$normality_test_out <- renderPrint({
      res <- test_results()
      req(res)
      
      shapiro_res <- shapiro.test(res$norm_data)
      print(shapiro_res)
      
      # Interpretação breve do Shapiro-Wilk
      cat("\nInterpretação:\n")
      if (shapiro_res$p.value < 0.05) {
        cat("  p-valor < 0.05 -> Rejeita-se a normalidade.\n")
        cat("  Os resíduos/diferenças podem NÃO seguir uma distribuição Normal.\n")
      } else {
        cat("  p-valor >= 0.05 -> Não se rejeita a normalidade.\n")
        cat("  Os resíduos/diferenças seguem estatisticamente uma distribuição Normal.\n")
      }
    })
    
    # Q-Q Plot de pressupostos
    output$qq_plot <- renderPlot({
      res <- test_results()
      req(res)
      
      diag_data <- data.frame(ResiduosStd = res$norm_data)
      
      g_theme <- theme_minimal(base_size = 12) +
        theme(plot.title = element_text(face = "bold", size = 13, color = "#212529"))
      
      ggplot(diag_data, aes(sample = ResiduosStd)) +
        stat_qq(color = "#495057", alpha = 0.7, size = 2) +
        stat_qq_line(color = "#0d6efd", size = 0.8) +
        g_theme +
        labs(title = "Normal Q-Q Plot", x = "Quantis Teóricos", y = "Quantis Amostrais")
    })
    
    # --- EXPORTAÇÃO INDIVIDUAL DO PROJETO ZIP ---
    
    # Código R para download
    r_code_text <- reactive({
      req(input$test_type, import_info())
      info <- import_info()
      
      # Pacotes
      code <- c(
        "# --- Código de Reprodutibilidade: Teste t de Student ---",
        "# Instalação de pacotes recomendados no RStudio:",
        "# install.packages(c('ggplot2', 'readxl', 'writexl'))",
        "library(ggplot2)",
        ""
      )
      
      # Carregar dados
      code <- c(code,
        "# 1. CARREGAR OS DADOS LIMPOS",
        "# (Carrega o arquivo RDA que preserva fatores e formatação)",
        "load('dados/dados_limpos.rda')",
        "dados <- df_clean",
        "",
        "# Alternativa em formato aberto Excel (se preferir):",
        "# library(readxl)",
        "# dados <- as.data.frame(read_excel('dados/dados_limpos.xlsx', sheet = 'Dados'))",
        "",
        "# Alternativa em formato aberto CSV:",
        "# dados <- read.csv('dados/dados_limpos.csv', stringsAsFactors = TRUE, check.names = FALSE)",
        ""
      )
      
      # Ajuste e saída do teste t
      conf_level_decimal <- input$conf_level / 100
      alt_val <- input$alternative
      
      if (input$test_type == "one_val") {
        req(input$one_var_y)
        code <- c(code,
          "# 1. Teste t de Uma Amostra",
          sprintf("resultado <- t.test(dados$`%s`, mu = %s, alternative = '%s', conf.level = %s)", 
                  input$one_var_y, input$one_mu, alt_val, conf_level_decimal),
          "print(resultado)",
          "",
          "# 2. Gráfico do Teste",
          sprintf("ggplot(dados, aes(x = '', y = `%s`)) +", input$one_var_y),
          "  geom_boxplot(fill = '#cfe2ff', color = '#0d6efd', alpha = 0.7, outlier.color = NA) +",
          "  geom_jitter(color = '#495057', width = 0.15, alpha = 0.5) +",
          sprintf("  geom_hline(aes(yintercept = mean(`%s`, na.rm=TRUE), color = 'Média Amostral'), linewidth = 1.2) +", input$one_var_y),
          sprintf("  geom_hline(aes(yintercept = %s, color = 'Média Hipotética'), linetype = 'dashed', linewidth = 1.2) +", input$one_mu),
          "  scale_color_manual(name = 'Referências', values = c('Média Amostral'='#0d6efd', 'Média Hipotética'='#dc3545')) +",
          "  theme_minimal() +",
          sprintf("  labs(title = 'Teste t de Uma Amostra: %s', y = '%s', x = '')", input$one_var_y, input$one_var_y)
        )
      } else if (input$test_type == "two_ind") {
        req(input$two_var_y, input$two_var_x)
        code <- c(code,
          "# 1. Teste t de Duas Amostras Independentes",
          sprintf("dados$`%s` <- as.factor(dados$`%s`)", input$two_var_x, input$two_var_x),
          sprintf("resultado <- t.test(`%s` ~ `%s`, data = dados, alternative = '%s', conf.level = %s, var.equal = %s)",
                  input$two_var_y, input$two_var_x, alt_val, conf_level_decimal, as.character(input$two_var_equal)),
          "print(resultado)",
          "",
          "# 2. Gráfico de Comparação",
          sprintf("ggplot(dados, aes(x = `%s`, y = `%s`, fill = `%s`)) +", input$two_var_x, input$two_var_y, input$two_var_x),
          "  geom_boxplot(alpha = 0.7, outlier.color = NA) +",
          "  geom_jitter(color = '#495057', width = 0.15, alpha = 0.5) +",
          "  stat_summary(fun = mean, geom = 'point', shape = 23, size = 4, fill = 'white') +",
          "  theme_minimal() +",
          sprintf("  labs(title = 'Comparação de Médias: %s por %s', x = '%s', y = '%s')", 
                  input$two_var_y, input$two_var_x, input$two_var_x, input$two_var_y)
        )
      } else if (input$test_type == "paired") {
        req(input$pair_var_y1, input$pair_var_y2)
        code <- c(code,
          "# 1. Teste t Pareado",
          sprintf("resultado <- t.test(dados$`%s`, dados$`%s`, paired = TRUE, alternative = '%s', conf.level = %s)",
                  input$pair_var_y1, input$pair_var_y2, alt_val, conf_level_decimal),
          "print(resultado)",
          "",
          "# 2. Gráfico Pareado",
          "df_clean <- na.omit(dados[, c(names(dados)[sapply(dados, is.numeric)])])", # Simplificado
          "df_long <- data.frame(",
          sprintf("  ID = rep(1:nrow(dados), 2),"),
          sprintf("  Condicao = factor(rep(c('%s', '%s'), each = nrow(dados)), levels = c('%s', '%s')),", 
                  input$pair_var_y1, input$pair_var_y2, input$pair_var_y1, input$pair_var_y2),
          sprintf("  Valores = c(dados$`%s`, dados$`%s`)", input$pair_var_y1, input$pair_var_y2),
          ")",
          "ggplot(df_long, aes(x = Condicao, y = Valores, group = ID)) +",
          "  geom_line(color = 'gray70', alpha = 0.6) +",
          "  geom_point(aes(color = Condicao), size = 2.5) +",
          "  geom_boxplot(aes(group = Condicao), fill = NA, outlier.color = NA, width = 0.3) +",
          "  theme_minimal() +",
          sprintf("  labs(title = 'Comparação Pareada: %s vs %s', x = 'Condição', y = 'Valores')", 
                  input$pair_var_y1, input$pair_var_y2)
        )
      }
      
      # 3. Gráfico da Distribuição t de Student Teórica
      code <- c(code,
        "",
        "# 3. Gráfico da Distribuição t de Student Teórica",
        "df_val <- resultado$parameter",
        "t_calc <- resultado$statistic",
        sprintf("alpha <- 1 - %s", conf_level_decimal),
        "x_limit <- max(4.5, abs(t_calc) + 1.5)",
        "x_seq <- seq(-x_limit, x_limit, length.out = 500)",
        "y_seq <- dt(x_seq, df = df_val)",
        "df_dist <- data.frame(x = x_seq, y = y_seq)",
        "p_dist <- ggplot(df_dist, aes(x = x, y = y)) +",
        "  geom_line(color = 'gray30', linewidth = 1)",
        "",
        "if (resultado$alternative == 'two.sided') {",
        "  t_crit_up <- qt(1 - alpha/2, df = df_val)",
        "  t_crit_low <- qt(alpha/2, df = df_val)",
        "  p_dist <- p_dist +",
        "    geom_area(data = subset(df_dist, x <= t_crit_low), aes(x = x, y = y), fill = '#dc3545', alpha = 0.4) +",
        "    geom_area(data = subset(df_dist, x >= t_crit_up), aes(x = x, y = y), fill = '#dc3545', alpha = 0.4) +",
        "    geom_vline(xintercept = c(t_crit_low, t_crit_up), linetype = 'dashed', color = '#dc3545') +",
        "    annotate('text', x = t_crit_up + 0.35, y = max(y_seq)*0.7, label = paste('t(crit) =', round(t_crit_up, 3)), color = '#dc3545', fontface = 'bold')",
        "} else if (resultado$alternative == 'greater') {",
        "  t_crit <- qt(1 - alpha, df = df_val)",
        "  p_dist <- p_dist +",
        "    geom_area(data = subset(df_dist, x >= t_crit), aes(x = x, y = y), fill = '#dc3545', alpha = 0.4) +",
        "    geom_vline(xintercept = t_crit, linetype = 'dashed', color = '#dc3545') +",
        "    annotate('text', x = t_crit + 0.4, y = max(y_seq)*0.7, label = paste('t(crit) =', round(t_crit, 3)), color = '#dc3545', fontface = 'bold')",
        "} else if (resultado$alternative == 'less') {",
        "  t_crit <- qt(alpha, df = df_val)",
        "  p_dist <- p_dist +",
        "    geom_area(data = subset(df_dist, x <= t_crit), aes(x = x, y = y), fill = '#dc3545', alpha = 0.4) +",
        "    geom_vline(xintercept = t_crit, linetype = 'dashed', color = '#dc3545') +",
        "    annotate('text', x = t_crit - 0.4, y = max(y_seq)*0.7, label = paste('t(crit) =', round(t_crit, 3)), color = '#dc3545', fontface = 'bold')",
        "}",
        "p_dist <- p_dist +",
        "  geom_vline(xintercept = t_calc, color = '#0d6efd', linewidth = 1.2) +",
        "  annotate('label', x = t_calc, y = max(y_seq)*0.9, label = paste('t(calc) =', round(t_calc, 3)), fill = 'white', color = '#0d6efd', fontface = 'bold') +",
        "  theme_minimal() +",
        "  labs(title = 'Distribuição t de Student e Regiões Críticas', x = 't', y = 'Densidade')",
        "print(p_dist)"
      )
      
      # Pressupostos
      code <- c(code,
        "",
        "# 4. Teste de Normalidade dos Resíduos (Shapiro-Wilk)",
        if (input$test_type == "one_val") {
          sprintf("shapiro.test(dados$`%s`)", input$one_var_y)
        } else if (input$test_type == "two_ind") {
          sprintf("shapiro.test(residuals(lm(`%s` ~ `%s`, data = dados)))", input$two_var_y, input$two_var_x)
        } else if (input$test_type == "paired") {
          sprintf("shapiro.test(dados$`%s` - dados$`%s`)", input$pair_var_y1, input$pair_var_y2)
        }
      )
      
      paste(code, collapse = "\n")
    })
    
    # Função auxiliar para customizar os parâmetros do arquivo QMD
    customize_qmd_params <- function(qmd_path, test_type, var_y, var_x, mu, conf_level, label_y, label_x) {
      lines <- readLines(qmd_path, warn = FALSE)
      lines <- gsub('test_type: ".*"', sprintf('test_type: "%s"', test_type), lines)
      lines <- gsub('var_y: ".*"', sprintf('var_y: "%s"', var_y), lines)
      lines <- gsub('var_x: ".*"', sprintf('var_x: "%s"', var_x), lines)
      lines <- gsub('mu: .*', sprintf('mu: %s', mu), lines)
      lines <- gsub('conf_level: .*', sprintf('conf_level: %s', conf_level), lines)
      lines <- gsub('label_y: ".*"', sprintf('label_y: "%s"', label_y), lines)
      lines <- gsub('label_x: ".*"', sprintf('label_x: "%s"', label_x), lines)
      return(lines)
    }

    # Download do Relatório Word (.docx)
    output$download_report_docx <- downloadHandler(
      filename = function() {
        paste0("relatorio_teste_t_", format(Sys.Date(), "%Y-%m-%d"), ".docx")
      },
      content = function(file) {
        req(data_rv())
        
        # Criar diretório temporário para compilação
        temp_dir <- tempdir()
        temp_qmd <- file.path(temp_dir, "relatorio_teste_t.qmd")
        temp_ref <- file.path(temp_dir, "custom-reference.docx")
        temp_func <- file.path(temp_dir, "funcoes_teste_t.R")
        temp_data <- file.path(temp_dir, "dados_limpos.rda")
        
        # Copiar arquivos de templates para o diretório temporário
        file.copy("templates/custom-reference.docx", temp_ref, overwrite = TRUE)
        file.copy("templates/funcoes_teste_t.R", temp_func, overwrite = TRUE)
        file.copy("templates/relatorio_teste_t.qmd", temp_qmd, overwrite = TRUE)
        
        # Salvar os dados limpos ativos
        df_clean <- data_rv()
        save(df_clean, file = temp_data)
        
        # Determinar os parâmetros de entrada com base no tipo de teste
        t_type <- input$test_type
        
        var_y_val <- ""
        var_x_val <- "NULL"
        mu_val <- 0
        label_y_val <- ""
        label_x_val <- ""
        
        if (t_type == "one_val") {
          var_y_val <- input$one_var_y
          mu_val <- input$one_mu
          label_y_val <- paste0("o ", var_y_val)
        } else if (t_type == "two_ind") {
          var_y_val <- input$two_var_y
          var_x_val <- input$two_var_x
          label_y_val <- paste0("a variável ", var_y_val)
          label_x_val <- paste0("a variável ", var_x_val)
        } else if (t_type == "paired") {
          var_y_val <- input$pair_var_y1
          var_x_val <- input$pair_var_y2
          label_y_val <- paste0("a variável ", var_y_val)
          label_x_val <- paste0("a variável ", var_x_val)
        }
        
        conf_level_val <- input$conf_level / 100
        
        # Customizar e escrever o QMD
        custom_qmd_lines <- customize_qmd_params(
          temp_qmd,
          test_type = t_type,
          var_y = var_y_val,
          var_x = var_x_val,
          mu = mu_val,
          conf_level = conf_level_val,
          label_y = label_y_val,
          label_x = label_x_val
        )
        writeLines(custom_qmd_lines, temp_qmd)
        
        # Renderizar o relatório usando quarto CLI
        old_wd <- getwd()
        setwd(temp_dir)
        
        # Renderização direta
        system2("quarto", args = c("render", "relatorio_teste_t.qmd", "--to", "docx"))
        
        setwd(old_wd)
        
        # Copiar o arquivo final gerado para a saída
        generated_docx <- file.path(temp_dir, "relatorio_teste_t.docx")
        if (file.exists(generated_docx)) {
          file.copy(generated_docx, file, overwrite = TRUE)
        } else {
          # Fallback error file
          writeLines("Erro: Não foi possível renderizar o relatório .docx usando o Quarto CLI.", file)
        }
      }
    )

    # Download do zip
    output$download_project_zip <- downloadHandler(
      filename = function() {
        paste0("projeto_teste_t_", format(Sys.Date(), "%Y-%m-%d"), ".zip")
      },
      content = function(file) {
        info <- import_info()
        proj_dir_name <- paste0("projeto_teste_t_", format(Sys.Date(), "%Y-%m-%d"))
        temp_dir <- tempdir()
        proj_dir <- file.path(temp_dir, proj_dir_name)
        dir.create(proj_dir, showWarnings = FALSE)
        
        dir_dados <- file.path(proj_dir, "dados")
        dir_scripts <- file.path(proj_dir, "scripts")
        dir_relatorios <- file.path(proj_dir, "relatorios")
        
        dir.create(dir_dados, showWarnings = FALSE)
        dir.create(dir_scripts, showWarnings = FALSE)
        dir.create(dir_relatorios, showWarnings = FALSE)
        
        # Salvar dados limpos (RDA, CSV e XLSX)
        df_clean <- data_rv()
        req(df_clean)
        save(df_clean, file = file.path(dir_dados, "dados_limpos.rda"))
        write.csv(df_clean, file = file.path(dir_dados, "dados_limpos.csv"), row.names = FALSE)
        ds_name <- if (info$source == "package") info$package_dataset else info$excel_sheet
        export_to_xlsx(df_clean, dataset_name = ds_name, file_path = file.path(dir_dados, "dados_limpos.xlsx"))
        
        # Gerar o script R
        writeLines(r_code_text(), file.path(dir_scripts, "teste_t.R"))
        
        # Copiar arquivos de templates
        file.copy("templates/custom-reference.docx", file.path(dir_relatorios, "custom-reference.docx"), overwrite = TRUE)
        file.copy("templates/funcoes_teste_t.R", file.path(dir_scripts, "funcoes_teste_t.R"), overwrite = TRUE)
        
        # Determinar os parâmetros de entrada com base no tipo de teste
        t_type <- input$test_type
        
        var_y_val <- ""
        var_x_val <- "NULL"
        mu_val <- 0
        label_y_val <- ""
        label_x_val <- ""
        
        if (t_type == "one_val") {
          var_y_val <- input$one_var_y
          mu_val <- input$one_mu
          label_y_val <- paste0("o ", var_y_val)
        } else if (t_type == "two_ind") {
          var_y_val <- input$two_var_y
          var_x_val <- input$two_var_x
          label_y_val <- paste0("a variável ", var_y_val)
          label_x_val <- paste0("a variável ", var_x_val)
        } else if (t_type == "paired") {
          var_y_val <- input$pair_var_y1
          var_x_val <- input$pair_var_y2
          label_y_val <- paste0("a variável ", var_y_val)
          label_x_val <- paste0("a variável ", var_x_val)
        }
        
        conf_level_val <- input$conf_level / 100
        
        # Customizar e escrever o QMD
        custom_qmd_lines <- customize_qmd_params(
          "templates/relatorio_teste_t.qmd",
          test_type = t_type,
          var_y = var_y_val,
          var_x = var_x_val,
          mu = mu_val,
          conf_level = conf_level_val,
          label_y = label_y_val,
          label_x = label_x_val
        )
        writeLines(custom_qmd_lines, file.path(dir_relatorios, "relatorio_teste_t.qmd"))
        
        # Criar Rproj
        rproj_content <- c(
          "Version: 1.0",
          "RestoreWorkspace: Default",
          "SaveWorkspace: Default",
          "AlwaysSaveHistory: Default",
          "EnableCodeIndexing: Yes",
          "UseSpacesForTab: Yes",
          "NumSpacesForTab: 2",
          "Encoding: UTF-8"
        )
        writeLines(rproj_content, file.path(proj_dir, "projeto_analise.Rproj"))
        
        # README.txt
        readme_content <- c(
          "===========================================================",
          " PACOTE DE ESTUDO: TESTE T DE STUDENT (IDE_R CIENTÍFICA)",
          "===========================================================",
          "",
          "Estrutura do projeto:",
          "- projeto_analise.Rproj: Dê duplo clique para abrir no RStudio.",
          "- dados/               : Contém os dados limpos em .rda, .csv e .xlsx.",
          "- scripts/             : Contém scripts e funções de apoio.",
          "  - scripts/teste_t.R  : Script com o código de cálculo e gráficos.",
          "  - scripts/funcoes_teste_t.R : Funções de formatação e relato.",
          "- relatorios/relatorio_teste_t.qmd: Relatório em Quarto para compilação."
        )
        writeLines(readme_content, file.path(proj_dir, "README.txt"))
        
        # Compactar
        old_wd <- getwd()
        setwd(temp_dir)
        zip::zip(file, files = proj_dir_name)
        setwd(old_wd)
      }
    )
  })
}
