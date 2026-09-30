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
            
            numericInput(ns("conf_level"), "Nível de Confiança (%):", value = 95, min = 80, max = 99, step = 1),
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
      
      # COLUNA 2: RESULTADOS (TABELA PRINCIPAL / GRÁFICOS)
      execucao_explicita_resultados_ui(ns, navset_card_tab(
        id = ns("active_tab"),
        title = "Painel de Resultados:",
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
            plotOutput(ns("test_plot"), height = "450px"),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'two_ind'", ns("test_type")),
              div(style = "margin-top: 20px;"),
              h6("Médias com desvio padrão e letras de significância",
                 style = "font-weight: 700; color: #0d6efd; margin-bottom: 10px;"),
              plotOutput(ns("test_plot_medias"), height = "400px")
            )
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
            conditionalPanel(
              condition = sprintf("input['%s'] == 'two_ind'", ns("test_type")),
              h6("Teste de Igualdade de Variâncias (Levene)", style = "font-weight: 700; color: #0d6efd; margin-bottom: 5px;"),
              verbatimTextOutput(ns("levene_test_out")),
              hr(style = "margin: 10px 0; border-color: #dee2e6;")
            ),
            h6("Gráfico de Normalidade (Q-Q Plot)", style = "font-weight: 700; color: #0d6efd; margin-bottom: 10px;"),
            plotOutput(ns("qq_plot"), height = "300px")
          )
        )
      )),
      
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

teste_t_validar_duas_amostras <- function(df, resposta, grupo) {
  if (is.null(df) || !is.data.frame(df))
    return("A base utilizada ainda não está disponível.")
  if (is.null(resposta) || !nzchar(resposta) || !resposta %in% names(df))
    return("Selecione uma variável resposta existente na base.")
  if (is.null(grupo) || !nzchar(grupo) || !grupo %in% names(df))
    return("Selecione uma variável de grupo existente na base.")
  if (!is.numeric(df[[resposta]]))
    return(sprintf(
      "A variável resposta '%s' precisa ser numérica para o teste t.",
      resposta
    ))

  dados <- stats::na.omit(df[, c(resposta, grupo), drop = FALSE])
  niveis <- unique(as.character(dados[[grupo]]))
  niveis <- niveis[!is.na(niveis)]

  if (length(niveis) != 2L) {
    encontrados <- if (length(niveis)) {
      paste(sprintf("'%s'", niveis), collapse = ", ")
    } else {
      "nenhuma categoria"
    }
    return(sprintf(
      paste0(
        "O teste t independente exige exatamente duas categorias em '%s'. ",
        "Foram encontradas %d: %s. Padronize ou recodifique essa variável ",
        "em Criar e Editar Variáveis e Níveis e recalcule a Base Derivada."
      ),
      grupo, length(niveis), encontrados
    ))
  }

  contagens <- table(factor(dados[[grupo]], levels = niveis))
  if (any(contagens < 2L))
    return("Cada categoria precisa ter pelo menos duas observações válidas.")

  NULL
}

mod_parametric_server <- function(id, data_rv, import_info) {
  moduleServer(id, function(input, output, session) {
    revisao_execucao <- execucao_revisao_dados(data_rv)
    gatilho_execucao <- reactiveVal(0L)
    
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
      
      manter <- function(atual, escolhas, padrao = NULL) {
        if (!is.null(atual) && atual %in% escolhas) atual else padrao
      }

      updateSelectInput(session, "one_var_y", choices = num_cols,
                        selected = manter(isolate(input$one_var_y), num_cols, num_cols[1]))

      updateSelectInput(session, "two_var_y", choices = num_cols,
                        selected = manter(isolate(input$two_var_y), num_cols, num_cols[1]))
      updateSelectInput(session, "two_var_x", choices = cat_cols,
                        selected = manter(isolate(input$two_var_x), cat_cols,
                                          if (length(cat_cols) > 0) cat_cols[1] else NULL))

      updateSelectInput(session, "pair_var_y1", choices = num_cols,
                        selected = manter(isolate(input$pair_var_y1), num_cols, num_cols[1]))
      updateSelectInput(session, "pair_var_y2", choices = num_cols,
                        selected = manter(isolate(input$pair_var_y2), num_cols,
                                          if (length(num_cols) > 1) num_cols[2] else num_cols[1]))
    })

    assinatura_execucao <- reactive({
      req(input$test_type)
      execucao_assinatura(
        input,
        c("test_type", "one_var_y", "one_mu", "two_var_y", "two_var_x",
          "two_var_equal", "pair_var_y1", "pair_var_y2", "alternative",
          "conf_level"),
        revisao_execucao()
      )
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
    
    # O teste só é calculado após confirmação explícita.
    test_results <- eventReactive(gatilho_execucao(), {
      df <- data_rv()
      validate(
        need(!is.null(df) && is.data.frame(df), "A base utilizada ainda não está disponível."),
        need(!is.null(input$test_type) && nzchar(input$test_type),
             "Selecione o tipo de teste t.")
      )

      validate(
        need(!is.null(input$conf_level) && !is.na(input$conf_level),
             "Informe o nível de confiança."),
        need(!is.null(input$alternative) && nzchar(input$alternative),
             "Selecione a hipótese alternativa.")
      )
      conf_level_decimal <- input$conf_level / 100
      alternative_val <- input$alternative

      if (input$test_type == "one_val") {
        validate(
          need(!is.null(input$one_var_y) && nzchar(input$one_var_y),
               "Selecione a variável numérica do teste t."),
          need(input$one_var_y %in% names(df),
               "A variável numérica selecionada não existe na base utilizada.")
        )
        x <- df[[input$one_var_y]]
        x_clean <- x[!is.na(x)]
        validate(
          need(is.numeric(x), "A variável selecionada precisa ser numérica."),
          need(length(x_clean) > 2,
               "A variável selecionada precisa ter pelo menos três valores não ausentes.")
        )

        validate(
          need(!is.null(input$one_mu) && !is.na(input$one_mu),
               "Informe a média hipotética do teste.")
        )
        t_out <- t.test(x_clean, mu = input$one_mu, alternative = alternative_val, conf.level = conf_level_decimal)
        
        # Obter dados para pressupostos (a própria variável centrada na média)
        norm_data <- x_clean - mean(x_clean)
        
        list(t_out = t_out, norm_data = norm_data, type = "one_val", var_names = c(input$one_var_y))
        
      } else if (input$test_type == "two_ind") {
        validate(
          need(!is.null(input$two_var_y) && nzchar(input$two_var_y),
               "Selecione a variável dependente numérica."),
          need(!is.null(input$two_var_x) && nzchar(input$two_var_x),
               "Selecione a variável de agrupamento."),
          need(input$two_var_y %in% names(df),
               "A variável dependente selecionada não existe na base utilizada."),
          need(input$two_var_x %in% names(df),
               "A variável de agrupamento selecionada não existe na base utilizada.")
        )

        erro_configuracao <- teste_t_validar_duas_amostras(
          df, input$two_var_y, input$two_var_x
        )
        validate(need(is.null(erro_configuracao), erro_configuracao))

        df_clean <- df[, c(input$two_var_y, input$two_var_x)]
        df_clean <- na.omit(df_clean)
        df_clean[[input$two_var_x]] <- as.factor(df_clean[[input$two_var_x]])

        formula_obj <- as.formula(paste(backtick(input$two_var_y), "~", backtick(input$two_var_x)))
        t_out <- t.test(formula_obj, data = df_clean, alternative = alternative_val, 
                        conf.level = conf_level_decimal, var.equal = input$two_var_equal)
        
        # Obter resíduos do modelo linear para verificar normalidade
        fit_lm <- lm(formula_obj, data = df_clean)
        norm_data <- rstandard(fit_lm)
        
        # Levene compara as variâncias dos dois grupos; é o mesmo cálculo do
        # script exportado (car::leveneTest sobre a fórmula do teste).
        teste_levene <- car::leveneTest(formula_obj, data = df_clean)
        
        list(t_out = t_out, norm_data = norm_data, type = "two_ind",
             var_names = c(input$two_var_y, input$two_var_x),
             levene = teste_levene)
        
      } else if (input$test_type == "paired") {
        validate(
          need(!is.null(input$pair_var_y1) && nzchar(input$pair_var_y1),
               "Selecione a primeira variável da análise pareada."),
          need(!is.null(input$pair_var_y2) && nzchar(input$pair_var_y2),
               "Selecione a segunda variável da análise pareada."),
          need(input$pair_var_y1 %in% names(df),
               "A primeira variável selecionada não existe na base utilizada."),
          need(input$pair_var_y2 %in% names(df),
               "A segunda variável selecionada não existe na base utilizada.")
        )

        df_clean <- df[, c(input$pair_var_y1, input$pair_var_y2)]
        df_clean <- na.omit(df_clean)
        validate(
          need(nrow(df_clean) > 2,
               "A análise pareada precisa ter pelo menos três pares completos.")
        )
        
        x1 <- df_clean[[input$pair_var_y1]]
        x2 <- df_clean[[input$pair_var_y2]]
        
        t_out <- t.test(x1, x2, paired = TRUE, alternative = alternative_val, conf.level = conf_level_decimal)
        
        # Na análise pareada, a hipótese de normalidade se aplica às diferenças d = x1 - x2
        differences <- x1 - x2
        norm_data <- differences - mean(differences)
        
        list(t_out = t_out, norm_data = norm_data, type = "paired", var_names = c(input$pair_var_y1, input$pair_var_y2))
      }
    }, ignoreInit = FALSE)
    
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
    
    # Figura de médias ± desvio padrão com letras de significância
    # (apenas duas amostras independentes)
    output$test_plot_medias <- renderPlot({
      res <- test_results()
      req(res, res$type == "two_ind")
      df <- data_rv()
      req(df, input$two_var_y, input$two_var_x)
      
      title_val <- if (nzchar(input$custom_title)) input$custom_title else "Médias com desvio padrão"
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
      
      df_clean <- df[, c(input$two_var_y, input$two_var_x)]
      df_clean <- na.omit(df_clean)
      df_clean[[input$two_var_x]] <- as.factor(df_clean[[input$two_var_x]])
      
      alfa_painel <- 1 - input$conf_level / 100
      cores_grupo <- c("#0F3B5F", "#E89B3C")
      
      resumo <- df_clean |>
        dplyr::group_by(.data[[input$two_var_x]]) |>
        dplyr::summarise(
          media = mean(.data[[input$two_var_y]]),
          dp = sd(.data[[input$two_var_y]]),
          .groups = "drop"
        )
      # As letras seguem o p do teste escolhido (Student ou Welch, conforme o
      # campo de variâncias): sem diferença, "a" para os dois grupos; com
      # diferença, "a" para o grupo de maior média e "b" para o outro.
      # which.max localiza a maior média pelo índice, sem comparar números de
      # ponto flutuante por igualdade.
      resumo$letra <- dplyr::case_when(
        res$t_out$p.value >= alfa_painel ~ "a",
        seq_along(resumo$media) == which.max(resumo$media) ~ "a",
        TRUE ~ "b"
      )
      # Rótulo ao lado do losango, com vírgula decimal e as mesmas casas do
      # projeto exportado (lá, fmt(); aqui, o mesmo formatC que os módulos
      # usam). O DP é o amostral, o mesmo das hastes.
      rotular_media <- function(x) formatC(x, format = "f", digits = 2, decimal.mark = ",")
      resumo$rotulo_media <- paste0(rotular_media(resumo$media), " ± ", rotular_media(resumo$dp))
      # Alturas do texto: a letra fica acima do ponto mais alto e da haste
      # mais alta; o rótulo, ao lado do losango, na altura da média. A folga
      # é aditiva (6% da amplitude da figura): com valores todos negativos,
      # max * 1.06 colocaria o texto dentro dos dados.
      valores_figura <- c(resumo$media + resumo$dp, resumo$media - resumo$dp,
                          df_clean[[input$two_var_y]])
      folga_y <- 0.06 * diff(range(valores_figura, na.rm = TRUE))
      y_letra <- max(valores_figura, na.rm = TRUE) + folga_y
      resumo$y_rotulo <- resumo$media

      ggplot(resumo, aes(x = .data[[input$two_var_x]])) +
        geom_jitter(
          data = df_clean,
          aes(y = .data[[input$two_var_y]], colour = .data[[input$two_var_x]]),
          width = 0.10, size = 2.2, alpha = 0.7
        ) +
        geom_errorbar(aes(ymin = media - dp, ymax = media + dp),
                      width = 0.15, linewidth = 0.8, colour = "#0F3B5F") +
        geom_point(aes(y = media), shape = 18, size = 4.4, colour = "#0F3B5F") +
        # Rótulo à direita do losango: fundo branco translúcido e sem borda,
        # para continuar legível sobre pontos próximos.
        geom_label(aes(y = y_rotulo, label = rotulo_media),
                   nudge_x = 0.18, hjust = 0,
                   linewidth = 0, label.padding = grid::unit(0.12, "lines"),
                   fill = ggplot2::alpha("white", 0.75), colour = "#0F3B5F",
                   size = 3.2) +
        geom_text(aes(y = y_letra, label = letra), size = 5, fontface = "bold", colour = "#0F3B5F") +
        scale_colour_manual(values = cores_grupo, guide = "none") +
        # A folga à direita evita que o rótulo ao lado do segundo grupo seja cortado.
        scale_x_discrete(expand = expansion(mult = c(0.20, 0.80))) +
        scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
        labs(
          title = title_val,
          x = x_label, y = y_label,
          subtitle = paste0("Pontos: observações; losango: média; hastes e rótulo: média ± DP.\n",
                            "Letras iguais: sem diferença significativa")
        ) +
        g_theme
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
    
    # Teste de Igualdade de Variâncias (Levene), apenas duas amostras
    output$levene_test_out <- renderPrint({
      res <- test_results()
      req(res, res$type == "two_ind")

      lev <- res$levene
      cat("Teste de Levene (homogeneidade das variâncias)\n")
      cat("Hipóteses:\n")
      cat("  H0: as variâncias dos dois grupos são iguais.\n")
      cat("  H1: as variâncias dos dois grupos são diferentes.\n\n")
      print(lev)

      # Leitura em linguagem simples, com a estatística, os gl e o p, no
      # mesmo alfa do projeto (1 menos o nível de confiança). A leitura é
      # recomendatória: o teste aplicado segue a caixa "Assumir Variâncias
      # Iguais (Homocedasticidade)", não a decisão automática do Levene.
      p_levene <- lev[["Pr(>F)"]][1]
      gl_num <- lev[["Df"]][1]
      gl_den <- lev[["Df"]][2]
      f_val <- lev[["F value"]][1]
      alfa_levene <- 1 - input$conf_level / 100
      cat("\nInterpretação:\n")
      cat(sprintf("  F(%s, %s) = %.3f; p = %.4f\n", gl_num, gl_den, f_val, p_levene))
      # A ausência de rejeição não prova igualdade; NA não é resultado favorável.
      leitura_levene <- dplyr::case_when(
        is.na(p_levene) ~ "Levene não pôde ser calculado; examine os dados e considere Welch.",
        p_levene < alfa_levene ~ "Rejeitamos H0: há evidência de variâncias diferentes. Considere o t de Welch.",
        TRUE ~ "Não há evidência suficiente de variâncias diferentes: não rejeitamos H0. Student pressupõe variâncias iguais; Welch continua disponível."
      )
      cat(sprintf("  Alfa do projeto = %.2f. %s\n", alfa_levene, leitura_levene))
      cat("  Esta leitura é recomendatória; o teste aplicado segue a escolha de variâncias do painel.\n")
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
        stat_qq_line(color = "#0d6efd", linewidth = 0.8) +
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
    

    estado_execucao <- reactive({
      req(exec_ctrl$atualizada())
      res <- test_results()
      req(res)
      t_out <- res$t_out
      parametros_teste <- switch(
        res$type,
        one_val = list(variavel = input$one_var_y, media_hipotetica = input$one_mu),
        two_ind = list(resposta = input$two_var_y, grupo = input$two_var_x,
                       variancias_iguais = isTRUE(input$two_var_equal)),
        paired = list(variavel_1 = input$pair_var_y1, variavel_2 = input$pair_var_y2)
      )
      titulo <- switch(
        res$type,
        one_val = paste("Teste t de uma amostra:", input$one_var_y),
        two_ind = paste("Teste t independente:", input$two_var_y, "por", input$two_var_x),
        paired = paste("Teste t pareado:", input$pair_var_y1, "e", input$pair_var_y2)
      )
      list(
        analise_id = "parametric",
        tipo = paste0("teste_t_", res$type),
        titulo = titulo,
        parametros = c(
          list(
            tipo_teste = res$type,
            alternativa = input$alternative,
            nivel_confianca = input$conf_level / 100
          ),
          parametros_teste
        ),
        saidas_disponiveis = c(
          "narrativa", "tabela", "grafico", "pressupostos", "diagnosticos", "console"
        ),
        resultado_resumo = list(
          estatistica = unname(as.numeric(t_out$statistic)),
          graus_liberdade = unname(as.numeric(t_out$parameter)),
          p_valor = t_out$p.value,
          intervalo_confianca = unname(t_out$conf.int)
        ),
        codigo_r = paste(r_code_text(), collapse = "\n")
      )
    })

    exec_ctrl <- execucao_explicita_server(
      input, output, session, assinatura_execucao, test_results,
      nome_analise = "O teste t",
      gatilho_rv = gatilho_execucao
    )

    invisible(list(
      resultado = test_results,
      estado_execucao = estado_execucao,
      estado_execucao_ui = exec_ctrl$estado,
      execucao_atualizada = exec_ctrl$atualizada
    ))
  })
}
