# Módulo de Regressão Linear Simples e Não-Linear para IDE_R

# Motor de ajuste não-linear (potência, Von Bertalanffy, logístico).
# Fonte canônica: EAPADados; fallback local para execução offline.
if (requireNamespace("EAPADados", quietly = TRUE)) {
  library(EAPADados)
} else if (!exists("ajustar_curva")) {
  .fc_path <- file.path("templates", "funcoes_crescimento.R")
  if (file.exists(.fc_path)) source(.fc_path, encoding = "UTF-8")
}

mod_regression_ui <- function(id, is_logistic = FALSE) {
  ns <- NS(id)
  aba_ajuste <- if (is_logistic) "Curva de Probabilidade" else "Reta Ajustada"
  aba_residuos <- if (is_logistic) "Resíduos de Deviance" else "Resíduos vs Ajustados"
  aba_diagnostico <- if (is_logistic) "Influência (Cook)" else "Normalidade (Q-Q Plot)"
  tagList(
    layout_columns(
      col_widths = c(1, 1, 1),
      style = "grid-template-columns: 2.5fr 7fr 2.5fr !important;",
      
      # COLUNA 1: CONFIGURAÇÃO DO MODELO E RELATÓRIOS
      div(
        card(
          card_header(if (is_logistic) "Configuração da Regressão Logística Binária" else "Configuração do Modelo"),
          card_body(
            style = "padding: 12px 15px;",
            if (is_logistic) tagList(
              selectInput(ns("dataset_entrada"), "Base utilizada:",
                          choices = c("Base compartilhada — dados_analise" = "dados_analise")),
              uiOutput(ns("base_status")),
              hr(style = "margin: 8px 0 12px;")
            ),
            selectInput(ns("var_y"), "Variável Dependente (Y):", choices = NULL),
            div(style = "margin-top: -8px;", selectInput(ns("var_x"), "Variável Independente (X):", choices = NULL)),
            if (is_logistic) {
              tags$div(style = "display: none;",
                selectInput(ns("model_type"), "Tipo de Modelo:", choices = c("Logístico" = "logistico"), selected = "logistico")
              )
            } else {
              div(style = "margin-top: -8px;", selectInput(ns("model_type"), "Tipo de Modelo:",
                choices = c(
                  "Linear (reta)"          = "linear",
                  "Potência (W = a·L^b)"   = "potencia",
                  "Von Bertalanffy"        = "von_bertalanffy"
                ), selected = "linear"))
            },
            execucao_explicita_controles_ui(ns)
          )
        ),
        card(
          card_header("Relatório e Pacote de Estudo"),
          card_body(
            style = "padding: 12px 15px;",
            execucao_explicita_downloads_ui(ns, tagList(
              actionButton(ns("export_code"), "Ver Código R", icon = icon("code"),
                           class = "btn-info w-100"),
              div(style = "margin-top: 8px;"),
              downloadButton(ns("download_report_docx"), "Baixar Relatório Word (.docx)", class = "btn-success w-100"),
              div(style = "margin-top: 8px;"),
              downloadButton(ns("download_project_zip"), "Exportar Projeto R (.zip)", class = "btn-primary w-100"),
              helpText("Gera os relatórios diretamente em DOCX ou exporta um projeto completo em Quarto.", style = "margin-top: 10px; margin-bottom: 0; font-size: 0.85rem;")
            ))
          )
        )
      ),
      
      # COLUNA 2: ABAS DE RESULTADOS (PRINCIPAL)
      execucao_explicita_resultados_ui(ns, navset_card_tab(
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
          title = aba_ajuste,
          icon = icon("chart-line"),
          card_body(
            plotOutput(ns("fit_plot"), height = "450px")
          )
        ),
        nav_panel(
          title = aba_residuos,
          icon = icon("chart-bar"),
          card_body(
            plotOutput(ns("resid_fit_plot"), height = "450px")
          )
        ),
        nav_panel(
          title = aba_diagnostico,
          icon = icon("chart-area"),
          card_body(
            plotOutput(ns("qq_plot"), height = "450px")
          )
        )
      )),
      
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
            if (!is_logistic) tagList(
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
              )
            ),
            selectInput(ns("graph_theme"), "Tema do Gráfico:", 
                        choices = c("Mínimo" = "minimal", 
                                    "Clássico" = "classic", 
                                    "Preto e Branco" = "bw", 
                                    "Cinza" = "gray", 
                                    "Light" = "light"), 
                        selected = "minimal"),
            # Mostrar a equação somente na aba principal do ajuste.
            conditionalPanel(
              condition = sprintf("input['%s'] == '%s'", ns("active_tab"), aba_ajuste),
              checkboxInput(
                ns("show_eq"),
                if (is_logistic) "Exibir equação logística" else "Exibir Equação da Reta",
                value = TRUE
              )
            )
          ),
          
          # Mensagem informativa para a Tabela de Resultados
          conditionalPanel(
            condition = sprintf("input['%s'] == 'Tabela de Resultados'", ns("active_tab")),
            helpText(HTML(
              if (is_logistic) {
                "<h5>Resultados da Regressão Logística Binária</h5>
                <p>Esta aba exibe a fórmula de probabilidade ajustada, a tabela de coeficientes com estatística z e p-valores, e as métricas globais de ajuste:</p>
                <ul>
                  <li><b>McFadden Pseudo-R²:</b> Medida de qualidade do ajuste (0 a 1).</li>
                  <li><b>Desvio Residual:</b> Medida de discrepância do modelo.</li>
                  <li><b>L50 estimado:</b> Ponto de transição onde a probabilidade é de 50%.</li>
                </ul>"
              } else {
                "<h5>Resultados Estatísticos</h5>
                <p>Esta aba exibe a fórmula matemática ajustada, a tabela científica de coeficientes (estimativas, erros padrão, estatísticas t e p-valores) e as métricas globais de ajuste:</p>
                <ul>
                  <li><b>R²:</b> Percentual de variância de Y explicada por X.</li>
                  <li><b>RSE:</b> Medida do desvio padrão dos resíduos.</li>
                  <li><b>Estatística F:</b> Teste global de significância do modelo.</li>
                </ul>"
              }
            ))
          )
        )
      )
    )
  )
}

mod_regression_server <- function(id, data_rv, import_info, is_logistic = FALSE,
                                  registro_bases_rv = NULL, cache_bases_rv = NULL,
                                  revisao_origem_rv = NULL,
                                  base_contexto_externo = NULL) {
  moduleServer(id, function(input, output, session) {
    aba_ajuste <- if (isTRUE(is_logistic)) "Curva de Probabilidade" else "Reta Ajustada"
    aba_residuos <- if (isTRUE(is_logistic)) "Resíduos de Deviance" else "Resíduos vs Ajustados"
    aba_diagnostico <- if (isTRUE(is_logistic)) "Influência (Cook)" else "Normalidade (Q-Q Plot)"

    registros_bases <- reactive({
      if (is.function(registro_bases_rv)) registro_bases_rv() %||% list() else list()
    })
    caches_bases <- reactive({
      if (is.function(cache_bases_rv)) cache_bases_rv() %||% list() else list()
    })
    revisao_bases <- reactive({
      if (is.function(revisao_origem_rv)) as.integer(revisao_origem_rv()) else 1L
    })

    opcoes_bases <- reactive({
      if (!isTRUE(is_logistic))
        return(stats::setNames("dados_analise", "Base compartilhada — dados_analise"))
      bases_opcoes_analise(
        registros_bases(), caches_bases(), revisao_bases(),
        finalidade_preferida = "reg_logistica"
      )
    })

    observe({
      if (!isTRUE(is_logistic)) return()
      escolhas <- opcoes_bases()
      atual <- isolate(input$dataset_entrada %||% "dados_analise")
      if (!atual %in% unname(escolhas)) {
        if (!identical(atual, "dados_analise"))
          showNotification(
            "A base escolhida deixou de estar pronta e atualizada. A regressão logística voltou para dados_analise.",
            type = "warning", duration = 10
          )
        atual <- "dados_analise"
      }
      updateSelectInput(session, "dataset_entrada", choices = escolhas, selected = atual)
    })

    base_contexto <- reactive({
      raiz <- data_rv()
      req(raiz)
      if (!isTRUE(is_logistic) && is.function(base_contexto_externo))
        return(base_contexto_externo())
      if (!isTRUE(is_logistic))
        return(list(df = as.data.frame(raiz), base_id = "dados_analise",
                    base_objeto = "dados_analise", nome_amigavel = "Base compartilhada",
                    derivada = FALSE))
      chave <- input$dataset_entrada %||% "dados_analise"
      resolvida <- tryCatch(
        bases_resolver_analise(
          chave, as.data.frame(raiz), registros_bases(), caches_bases(), revisao_bases()
        ),
        error = function(e) e
      )
      if (inherits(resolvida, "error"))
        validate(need(FALSE, conditionMessage(resolvida)))
      resolvida
    })

    dados_modulo <- reactive({ base_contexto()$df })
    revisao_execucao <- execucao_revisao_dados(dados_modulo)
    gatilho_execucao <- reactiveVal(0L)

    assinatura_execucao <- reactive({
      req(input$var_x, input$var_y, input$model_type)
      execucao_assinatura(
        input,
        c("dataset_entrada", "var_y", "var_x", "model_type", "var_group",
          "grp_reg"),
        revisao_execucao()
      )
    })

    output$base_status <- renderUI({
      if (!isTRUE(is_logistic)) return(NULL)
      base <- base_contexto()
      if (isTRUE(base$derivada)) {
        div(class = "alert alert-info", style = "font-size:0.78rem; padding:7px 9px; margin-bottom:8px;",
            icon("diagram-project"), " Usando ", tags$code(base$base_objeto),
            sprintf(" (%d linhas × %d colunas).", nrow(base$df), ncol(base$df)))
      } else {
        div(class = "alert alert-light border", style = "font-size:0.78rem; padding:7px 9px; margin-bottom:8px;",
            icon("database"), " Usando a base compartilhada ", tags$code("dados_analise"), ".")
      }
    })

    # Atualiza as escolhas de variáveis com base nos dados importados
    observe({
      df <- dados_modulo()
      req(df)
      
      # Para Y e X, permitimos todas as colunas (sem supressão de tipo)
      all_cols <- names(df)
      
      # Manter as seleções atuais se elas continuarem válidas no novo dataset
      curr_y <- input$var_y
      curr_x <- input$var_x
      curr_grp <- input$var_group
      
      selected_y <- if (!is.null(curr_y) && curr_y %in% all_cols) curr_y else (if (length(all_cols) > 0) all_cols[1] else NULL)
      selected_x <- if (!is.null(curr_x) && curr_x %in% all_cols) curr_x else (if (length(all_cols) > 1 && all_cols[1] == selected_y) all_cols[2] else if (length(all_cols) > 0) all_cols[1] else NULL)
      selected_grp <- if (!is.null(curr_grp) && curr_grp %in% c("none", all_cols)) curr_grp else "none"
      
      updateSelectInput(session, "var_y", choices = all_cols, selected = selected_y)
      updateSelectInput(session, "var_x", choices = all_cols, selected = selected_x)
      
      # Atualiza a variável de agrupamento
      updateSelectInput(session, "var_group", choices = c("Nenhuma" = "none", all_cols), selected = selected_grp)
    })
    
    # Quando o usuário muda a aba ativa ou as variáveis, atualizamos os campos para o padrão daquela aba
    observeEvent(list(input$active_tab, input$var_x, input$var_y), {
      req(input$active_tab, input$var_x, input$var_y)
      
      if (input$active_tab == aba_ajuste) {
        updateTextInput(
          session, "custom_title",
          value = paste(
            if (isTRUE(is_logistic)) "Regressão Logística Binária:" else "Ajuste Linear:",
            input$var_y, "vs", input$var_x
          )
        )
        updateTextInput(session, "custom_label_x", value = input$var_x)
        updateTextInput(
          session, "custom_label_y",
          value = if (isTRUE(is_logistic)) "Probabilidade estimada" else input$var_y
        )
      } else if (input$active_tab == aba_residuos) {
        updateTextInput(
          session, "custom_title",
          value = if (isTRUE(is_logistic))
            "Resíduos de Deviance vs Probabilidades Ajustadas"
          else "Resíduos vs Valores Ajustados"
        )
        updateTextInput(
          session, "custom_label_x",
          value = if (isTRUE(is_logistic)) "Probabilidades ajustadas" else "Valores Ajustados (Fitted)"
        )
        updateTextInput(
          session, "custom_label_y",
          value = if (isTRUE(is_logistic)) "Resíduos de deviance" else "Resíduos (Residuals)"
        )
      } else if (input$active_tab == aba_diagnostico) {
        updateTextInput(
          session, "custom_title",
          value = if (isTRUE(is_logistic)) "Influência das observações (Distância de Cook)" else "Normal Q-Q Plot"
        )
        updateTextInput(
          session, "custom_label_x",
          value = if (isTRUE(is_logistic)) "Observação" else "Quantis Teóricos"
        )
        updateTextInput(
          session, "custom_label_y",
          value = if (isTRUE(is_logistic)) "Distância de Cook" else "Resíduos Padronizados"
        )
      }
    }, ignoreInit = FALSE)
    
    # O modelo só é ajustado após o clique explícito.
    model_fit <- eventReactive(gatilho_execucao(), {
      df <- dados_modulo()
      req(df, input$var_x, input$var_y)
      req(input$var_x %in% names(df), input$var_y %in% names(df))

      mt <- input$model_type
      if (is.null(mt) || !nzchar(mt)) mt <- "linear"

      if (mt == "logistico") {
        # Regressão Logística Binária
        y_col <- df[[input$var_y]]
        unique_vals <- unique(na.omit(y_col))
        validate(
          need(length(unique_vals) == 2, 
               "A variável dependente (Y) deve ser binária (ex: 0 e 1, maduro e imaturo, etc.) com exatamente 2 categorias únicas para a regressão logística.")
        )
        
        # Mapear Y para 0/1 se necessário
        clean_df <- df[, c(input$var_x, input$var_y), drop = FALSE]
        clean_df <- na.omit(clean_df)
        y_vec <- clean_df[[input$var_y]]
        if (!is.numeric(y_vec) || !all(y_vec %in% c(0, 1))) {
          y_factor <- as.factor(y_vec)
          clean_df$y_bin <- as.integer(y_factor) - 1L
        } else {
          clean_df$y_bin <- as.numeric(y_vec)
        }
        
        formula_obj <- as.formula(paste("y_bin ~", backtick(input$var_x)))
        tryCatch(
          glm(formula_obj, data = clean_df, family = binomial),
          error = function(e) validate(need(FALSE, paste("Falha no ajuste da Regressão Logística:", conditionMessage(e))))
        )
      } else {
        # Para outros modelos (linear, potência, von bertalanffy), X e Y devem ser estritamente numéricos
        validate(
          need(is.numeric(df[[input$var_y]]), "A variável dependente (Y) deve ser numérica para este tipo de modelo."),
          need(is.numeric(df[[input$var_x]]), "A variável independente (X) deve ser numérica para este tipo de modelo.")
        )
        
        # Remover valores ausentes antes de ajustar o modelo
        clean_df <- df[, c(input$var_x, input$var_y), drop = FALSE]
        clean_df <- na.omit(clean_df)

        if (mt == "linear") {
          formula_obj <- as.formula(paste(backtick(input$var_y), "~", backtick(input$var_x)))
          lm(formula_obj, data = clean_df)
        } else {
          # Ajuste não-linear (potência / Von Bertalanffy)
          validate(need(exists("ajustar_curva"), "Motor de ajuste não-linear indisponível."))
          tryCatch(
            ajustar_curva(clean_df, var_y = input$var_y, var_x = input$var_x, tipo = mt),
            error = function(e) validate(need(FALSE, paste("Falha no ajuste:", conditionMessage(e))))
          )
        }
      }
    }, ignoreInit = FALSE)

    # Auxiliar para colocar crase em nomes de variáveis com espaços
    backtick <- function(s) {
      paste0("`", s, "`")
    }

    # Detecta se o ajuste é uma curva não-linear (lista de ajustar_curva) vs lm
    is_curve <- function(fit) is.list(fit) && !is.null(fit$tipo) && !inherits(fit, "lm")
    
    # Texto da fórmula ajustada
    output$formula_text <- renderPrint({
      fit <- model_fit()
      req(fit)
      if (is_curve(fit)) {
        cat("Modelo:", tipo_curva_label(fit$tipo), "\n")
        cat("Equação ajustada:\n  ", equacao_curva(fit), "\n")
      } else if (inherits(fit, "glm")) {
        cat("Modelo: Regressão Logística Binária (GLM Binomial)\n")
        coefs <- coef(fit)
        cat(sprintf("Equação ajustada:\n  P(Y = 1) = 1 / (1 + exp(-(%.4f + %.4f * X)))\n\n", coefs[1], coefs[2]))
        x50 <- -coefs[1] / coefs[2]
        cat(sprintf("L50 (ou X50) estimado: %.2f\n", x50))
      } else {
        cat("Fórmula do Modelo:\n")
        print(fit$call)
      }
    })
    
    # Tabela de coeficientes científica
    output$coef_table <- renderDT({
      fit <- model_fit()
      req(fit)

      coef_matrix <- if (is_curve(fit)) fit$coefs else summary(fit)$coefficients

      # Converte para data frame legível
      df_coef <- as.data.frame(coef_matrix)
      
      stat_col <- if (inherits(fit, "glm")) "Valor z" else "Valor t"
      names(df_coef) <- c("Estimativa", "Erro Padrão", stat_col, "p-valor")
      df_coef <- cbind(Termo = rownames(df_coef), df_coef)
      
      # Formatação científica da tabela
      datatable(
        df_coef,
        options = list(dom = 't', ordering = FALSE),
        rownames = FALSE,
        selection = 'none'
      ) %>%
        formatRound(columns = c("Estimativa", "Erro Padrão", stat_col), digits = 4) %>%
        formatSignif(columns = "p-valor", digits = 4)
    })
    
    # Sumário de métricas de ajuste do modelo
    output$metrics_summary <- renderUI({
      fit <- model_fit()
      req(fit)

      # Ajuste não-linear: pseudo-R², RSE, AIC e equação
      if (is_curve(fit)) {
        return(HTML(paste0(
          "<div style='line-height: 1.3;'>",
          "<p style='margin-bottom: 5px;'><b>Pseudo-R²:</b> ", round(fit$pseudo_r2, 4), " (", round(fit$pseudo_r2 * 100, 2), "%)</p>",
          "<p style='margin-bottom: 5px;'><b>Erro Padrão Residual (RSE):</b> ", round(fit$rse, 4), "</p>",
          "<p style='margin-bottom: 5px;'><b>AIC:</b> ", if (is.na(fit$aic)) "-" else round(fit$aic, 2), "</p>",
          "<p style='margin-bottom: 5px;'><b>N (observações):</b> ", fit$n, "</p>",
          "<p style='margin-bottom: 0;'><b>Equação ajustada:</b> ", equacao_curva(fit), "</p>",
          "</div>"
        )))
      }

      if (inherits(fit, "glm")) {
        # Métricas para Regressão Logística (GLM Binomial)
        dev_res <- deviance(fit)
        dev_null <- fit$null.deviance
        mcfadden_r2 <- 1 - dev_res / dev_null
        aic_val <- AIC(fit)
        n_obs <- length(fit$y)
        
        coefs <- coef(fit)
        x50 <- -coefs[1] / coefs[2]
        
        return(HTML(paste0(
          "<div style='line-height: 1.3;'>",
          "<p style='margin-bottom: 5px;'><b>McFadden Pseudo-R²:</b> ", round(mcfadden_r2, 4), " (", round(mcfadden_r2 * 100, 2), "%)</p>",
          "<p style='margin-bottom: 5px;'><b>Desvio Residual (Residual Deviance):</b> ", round(dev_res, 4), " em ", df.residual(fit), " GL</p>",
          "<p style='margin-bottom: 5px;'><b>Desvio Nulo (Null Deviance):</b> ", round(dev_null, 4), " em ", fit$df.null, " GL</p>",
          "<p style='margin-bottom: 5px;'><b>AIC:</b> ", round(aic_val, 2), "</p>",
          "<p style='margin-bottom: 5px;'><b>N (observações):</b> ", n_obs, "</p>",
          "<p style='margin-bottom: 0;'><b>Ponto de Transição L50 / X50 estimado:</b> ", round(x50, 2), "</p>",
          "</div>"
        )))
      }

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
    
    # Gráfico 1: Reta / Curva Ajustada
    output$fit_plot <- renderPlot({
      df <- dados_modulo()
      req(df, input$var_x, input$var_y)
      fit <- model_fit()
      req(fit)

      # --- Ajuste NÃO-LINEAR: sobrepõe a curva ajustada (potência / VB / logístico) ---
      if (is_curve(fit)) {
        g_theme_nl <- switch(input$graph_theme,
                             "minimal" = theme_minimal(base_size = 14),
                             "classic" = theme_classic(base_size = 14),
                             "bw"      = theme_bw(base_size = 14),
                             "gray"    = theme_gray(base_size = 14),
                             "light"   = theme_light(base_size = 14),
                             theme_minimal(base_size = 14))
        title_nl <- if (nzchar(input$custom_title)) input$custom_title else paste(tipo_curva_label(fit$tipo), "—", input$var_y, "vs", input$var_x)
        x_label_nl <- if (nzchar(input$custom_label_x)) input$custom_label_x else input$var_x
        y_label_nl <- if (nzchar(input$custom_label_y)) input$custom_label_y else input$var_y
        subtitle_nl <- if (input$show_eq) equacao_curva(fit) else NULL
        grid <- curva_predita(fit)
        return(
          ggplot(df, aes(x = .data[[input$var_x]], y = .data[[input$var_y]])) +
            geom_point(color = "#495057", alpha = 0.7, size = 2.5) +
            geom_line(data = grid, aes(x = .data[[input$var_x]], y = .data[[input$var_y]]),
                      color = "#0d6efd", linewidth = 1.2) +
            g_theme_nl +
            labs(title = title_nl, subtitle = subtitle_nl, x = x_label_nl, y = y_label_nl) +
            theme(
              plot.title = element_text(face = "bold", size = 16, color = "#212529"),
              plot.subtitle = element_text(color = "#0d6efd", face = "italic", size = 13)
            )
        )
      }
      
      # --- Ajuste da REGRESSÃO LOGÍSTICA BINÁRIA (GLM) ---
      if (inherits(fit, "glm")) {
        g_theme_glm <- switch(input$graph_theme,
                              "minimal" = theme_minimal(base_size = 14),
                              "classic" = theme_classic(base_size = 14),
                              "bw"      = theme_bw(base_size = 14),
                              "gray"    = theme_gray(base_size = 14),
                              "light"   = theme_light(base_size = 14),
                              theme_minimal(base_size = 14))
        title_glm <- if (nzchar(input$custom_title)) input$custom_title else
          paste("Regressão Logística Binária:", input$var_y, "vs", input$var_x)
        x_label_glm <- if (nzchar(input$custom_label_x)) input$custom_label_x else input$var_x
        y_label_glm <- if (nzchar(input$custom_label_y)) input$custom_label_y else "Probabilidade estimada"
        
        coefs <- coef(fit)
        x50 <- -coefs[1] / coefs[2]
        
        subtitle_glm <- if (input$show_eq) {
          sprintf("P(Y=1) = 1 / (1 + e^-(%.4f + %.4f * X))  |  L50 = %.2f", coefs[1], coefs[2], x50)
        } else {
          NULL
        }
        
        # Grid para a curva
        x_range <- range(df[[input$var_x]], na.rm = TRUE)
        x50_na_faixa <- is.finite(x50) && x50 >= x_range[1] && x50 <= x_range[2]
        grade <- data.frame(x = seq(x_range[1], x_range[2], length.out = 200))
        names(grade) <- input$var_x
        grade$prob <- predict(fit, newdata = grade, type = "response")
        
        # Mapear Y para 0/1 para plotagem
        df_plot <- df[, c(input$var_x, input$var_y), drop = FALSE]
        df_plot <- na.omit(df_plot)
        y_vec <- df_plot[[input$var_y]]
        if (!is.numeric(y_vec) || !all(y_vec %in% c(0, 1))) {
          y_factor <- as.factor(y_vec)
          df_plot$y_plot <- as.integer(y_factor) - 1L
        } else {
          df_plot$y_plot <- as.numeric(y_vec)
        }
        
        grafico_glm <-
          ggplot(df_plot, aes(x = .data[[input$var_x]], y = y_plot)) +
            geom_point(color = "#495057", alpha = 0.5, size = 2.5, position = position_jitter(height = 0.02, width = 0)) +
            geom_line(data = grade, aes(x = .data[[input$var_x]], y = prob),
                      color = "#dc3545", linewidth = 1.3) +
            g_theme_glm +
            labs(
              title = title_glm,
              subtitle = subtitle_glm,
              x = x_label_glm,
              y = y_label_glm,
              caption = if (!x50_na_faixa)
                sprintf(
                  "L50/X50 = %.2f está fora da faixa observada de X (%.2f a %.2f); interprete como extrapolação.",
                  x50, x_range[1], x_range[2]
                )
              else NULL
            ) +
            coord_cartesian(xlim = x_range, ylim = c(-0.03, 1.03)) +
            theme(
              plot.title = element_text(face = "bold", size = 16, color = "#212529"),
              plot.subtitle = element_text(color = "#dc3545", face = "italic", size = 12),
              plot.caption = element_text(color = "#6c757d", hjust = 0)
            )

        if (x50_na_faixa) {
          grafico_glm <- grafico_glm +
            geom_vline(xintercept = x50, linetype = "dashed", color = "#E89B3C", linewidth = 1) +
            annotate(
              "label", x = x50, y = 0.5, hjust = if (x50 > mean(x_range)) 1.05 else -0.05,
              label = sprintf("L50/X50 = %.2f", x50), color = "#0F3B5F",
              fill = "#FFF7E8", linewidth = 0.2, fontface = "bold"
            )
        }

        return(grafico_glm)
      }

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
      mdl <- if (is_curve(fit)) fit$modelo else fit
      eh_glm <- inherits(fit, "glm")

      diag_data <- data.frame(
        Ajustados = fitted(mdl),
        Residuos = if (eh_glm) residuals(mdl, type = "deviance") else residuals(mdl)
      )

      # Títulos e rótulos customizados
      title_val <- if (nzchar(input$custom_title)) input$custom_title else
        if (eh_glm) "Resíduos de Deviance vs Probabilidades Ajustadas" else "Resíduos vs Valores Ajustados"
      x_label <- if (nzchar(input$custom_label_x)) input$custom_label_x else
        if (eh_glm) "Probabilidades ajustadas" else "Valores Ajustados (Fitted)"
      y_label <- if (nzchar(input$custom_label_y)) input$custom_label_y else
        if (eh_glm) "Resíduos de deviance" else "Resíduos (Residuals)"
      
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
    
    # Gráfico 3: influência para GLM; Normal Q-Q para os demais modelos.
    output$qq_plot <- renderPlot({
      fit <- model_fit()
      req(fit)

      if (inherits(fit, "glm")) {
        cook <- stats::cooks.distance(fit)
        diag_cook <- data.frame(
          Observacao = seq_along(cook),
          DistanciaCook = as.numeric(cook)
        )
        limite <- 4 / max(1, nrow(diag_cook))
        title_val <- if (nzchar(input$custom_title)) input$custom_title else
          "Influência das observações (Distância de Cook)"
        x_label <- if (nzchar(input$custom_label_x)) input$custom_label_x else "Observação"
        y_label <- if (nzchar(input$custom_label_y)) input$custom_label_y else "Distância de Cook"
        g_theme <- switch(input$graph_theme,
                          "minimal" = theme_minimal(base_size = 14),
                          "classic" = theme_classic(base_size = 14),
                          "bw"      = theme_bw(base_size = 14),
                          "gray"    = theme_gray(base_size = 14),
                          "light"   = theme_light(base_size = 14),
                          theme_minimal(base_size = 14))

        return(
          ggplot(diag_cook, aes(x = Observacao, y = DistanciaCook)) +
            geom_col(fill = "#2E7D8F", width = 0.75) +
            geom_hline(yintercept = limite, linetype = "dashed", color = "#E76F51", linewidth = 0.9) +
            g_theme +
            labs(
              title = title_val,
              subtitle = sprintf("Linha tracejada: referência 4/n = %.4f", limite),
              x = x_label,
              y = y_label
            ) +
            theme(plot.title = element_text(face = "bold", size = 16, color = "#212529"))
        )
      }

      # Resíduos padronizados (rstandard não existe para nls: usa z-score dos resíduos)
      std_resid <- if (is_curve(fit)) {
        as.numeric(scale(residuals(fit$modelo)))
      } else {
        tryCatch(rstandard(fit), error = function(e) residuals(fit, type = "pearson"))
      }
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
      base_execucao <- base_contexto()
      
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

      if (isTRUE(base_execucao$derivada)) {
        base_registro <- bases_obter(registros_bases(), base_execucao$base_id)
        receita <- strsplit(bases_codigo(base_registro), "\n", fixed = TRUE)[[1]]
        receita <- receita[!grepl("^print\\(", receita)]
        code <- c(
          code,
          "# Base derivada escolhida na CatalyseR",
          "# O Projeto R integrado incluirá antes os tratamentos da Base Compartilhada.",
          "dados_analise <- dados",
          receita,
          sprintf("dados <- %s", base_execucao$base_objeto),
          ""
        )
      } else {
        code <- c(code, "# Base utilizada na análise: dados_analise", "")
      }

      # 3. Ajuste do Modelo
      mt <- input$model_type
      if (is.null(mt) || !nzchar(mt)) mt <- "linear"
      
      if (mt == "logistico") {
        code <- c(code,
          "# Ajustar modelo de Regressão Logística Binária (GLM Binomial)",
          "dados_mat <- dados",
          sprintf("if (!is.numeric(dados_mat$`%s`) || !all(na.omit(dados_mat$`%s`) %%in%% c(0, 1))) {",
                  input$var_y, input$var_y),
          sprintf("  dados_mat$y_bin <- as.integer(as.factor(dados_mat$`%s`)) - 1L", input$var_y),
          "} else {",
          sprintf("  dados_mat$y_bin <- dados_mat$`%s`", input$var_y),
          "}",
          sprintf("modelo <- glm(y_bin ~ `%s`, data = dados_mat, family = binomial)", input$var_x),
          "print(summary(modelo))",
          "# Calcular L50 / X50",
          "coefs <- coef(modelo)",
          "cat('L50 estimado:', -coefs[1] / coefs[2], '\\n')",
          ""
        )
      } else if (mt == "linear") {
        code <- c(code,
          "# Ajustar modelo de Regressão Linear Simples",
          sprintf("modelo <- lm(`%s` ~ `%s`, data = dados)", input$var_y, input$var_x),
          "print(summary(modelo))",
          ""
        )
      } else {
        code <- c(code,
          "# Ajustar modelo não-linear usando EAPADados",
          sprintf("modelo <- ajustar_curva(dados, var_y = '%s', var_x = '%s', tipo = '%s')", input$var_y, input$var_x, mt),
          "print(modelo$coefs)",
          ""
        )
      }
      
      # 4. Gráfico ggplot2 com condicional de agrupamento
      theme_code <- switch(input$graph_theme,
                           "minimal" = "theme_minimal(base_size = 14)",
                           "classic" = "theme_classic(base_size = 14)",
                           "bw"      = "theme_bw(base_size = 14)",
                           "gray"    = "theme_gray(base_size = 14)",
                           "light"   = "theme_light(base_size = 14)",
                           "theme_minimal(base_size = 14)")
      
      personalizar_ajuste <- identical(input$active_tab, aba_ajuste)
      title_val <- if (personalizar_ajuste && nzchar(input$custom_title)) input$custom_title else {
        if (mt == "logistico") paste("Regressão Logística Binária:", input$var_y, "vs", input$var_x)
        else if (mt == "linear") paste("Ajuste Linear:", input$var_y, "vs", input$var_x)
        else paste("Modelo Ajustado:", input$var_y, "vs", input$var_x)
      }
      x_label <- if (personalizar_ajuste && nzchar(input$custom_label_x)) input$custom_label_x else input$var_x
      y_label <- if (personalizar_ajuste && nzchar(input$custom_label_y)) {
        input$custom_label_y
      } else if (mt == "logistico") {
        "Probabilidade estimada"
      } else {
        input$var_y
      }
      
      if (mt == "logistico") {
        plot_lines <- c(
          "# Gerar gráfico de regressão logística com a curva em S ajustada",
          sprintf("x_range <- range(dados_mat$`%s`, na.rm = TRUE)", input$var_x),
          "x50 <- -coefs[1] / coefs[2]",
          "l50_na_faixa <- is.finite(x50) && x50 >= x_range[1] && x50 <= x_range[2]",
          "aviso_l50 <- if (l50_na_faixa) NULL else sprintf(",
          "  'L50/X50 = %.2f está fora da faixa observada de X (%.2f a %.2f); interprete como extrapolação.',",
          "  x50, x_range[1], x_range[2]",
          ")",
          "grade <- data.frame(x = seq(x_range[1], x_range[2], length.out = 200))",
          sprintf("names(grade) <- '%s'", input$var_x),
          "grade$prob <- predict(modelo, newdata = grade, type = 'response')",
          "",
          sprintf("ggplot(dados_mat, aes(x = `%s`, y = y_bin)) +", input$var_x),
          "  geom_point(color = '#495057', alpha = 0.5, size = 2.5, position = position_jitter(height = 0.02, width = 0)) +",
          sprintf("  geom_line(data = grade, aes(x = `%s`, y = prob), color = '#dc3545', linewidth = 1.3) +", input$var_x),
          "  geom_vline(",
          "    data = data.frame(x50 = if (l50_na_faixa) x50 else numeric(0)),",
          "    aes(xintercept = x50), inherit.aes = FALSE,",
          "    linetype = 'dashed', color = '#ffc107', linewidth = 1",
          "  ) +",
          sprintf("  %s +", theme_code),
          "  labs("
        )
      } else if (input$var_group != "none") {
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
          if (mt == "logistico") {
            plot_lines <- c(plot_lines, sprintf("    subtitle = 'P(Y=1) = 1 / (1 + exp(-(%.4f + %.4f * X)))  |  L50 = %.2f',", coefs[1], coefs[2], -coefs[1]/coefs[2]))
          } else {
            plot_lines <- c(plot_lines, sprintf("    subtitle = 'Y = %.4f + (%.4f) * X',", coefs[1], coefs[2]))
          }
        } else {
          plot_lines <- c(plot_lines, "    subtitle = 'Equação ajustada',")
        }
      }
      
      plot_lines <- c(plot_lines,
        sprintf("    x = '%s',", x_label),
        sprintf("    y = '%s'%s", y_label, if (mt == "logistico") "," else ""),
        if (mt == "logistico") "    caption = aviso_l50" else NULL,
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
        if (input$model_type != "linear") {
          showNotification("O relatório Word (.docx) está disponível apenas para o modelo Linear nesta versão.", type = "warning")
          writeLines("O relatório está disponível apenas para o modelo Linear (reta) nesta versão.", file)
          return()
        }
        req(dados_modulo())
        
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
        df_clean <- dados_modulo()
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
        if (input$model_type != "linear") {
          showNotification("O pacote de estudo (.zip) está disponível apenas para o modelo Linear nesta versão.", type = "warning")
          writeLines("O pacote de estudo está disponível apenas para o modelo Linear (reta) nesta versão.", file)
          return()
        }
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
        df_clean <- dados_modulo()
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
        zip::zip(file, files = proj_dir_name)
        setwd(old_wd)
      }
    )

    estado_execucao <- reactive({
      req(exec_ctrl$atualizada())
      fit <- model_fit()
      req(fit, input$var_x, input$var_y)
      tipo <- if (inherits(fit, "glm")) "regressao_logistica" else
        if (is_curve(fit)) paste0("regressao_", fit$tipo) else "regressao_linear"
      analise_id <- if (isTRUE(is_logistic)) "logistic_regression" else "regression"
      titulo <- if (inherits(fit, "glm"))
        paste("Regressão logística:", input$var_y, "por", input$var_x) else
        paste("Regressão linear:", input$var_y, "por", input$var_x)
      resumo <- if (is_curve(fit)) {
        list(n = fit$n, pseudo_r2 = fit$pseudo_r2, aic = fit$aic)
      } else if (inherits(fit, "glm")) {
        list(n = stats::nobs(fit), aic = stats::AIC(fit),
             desvio_residual = stats::deviance(fit))
      } else {
        sm <- summary(fit)
        list(n = stats::nobs(fit), r2 = sm$r.squared,
             r2_ajustado = sm$adj.r.squared, aic = stats::AIC(fit))
      }
      list(
        analise_id = analise_id,
        tipo = tipo,
        titulo = titulo,
        parametros = list(
          resposta = input$var_y,
          preditor = input$var_x,
          grupo = input$var_group %||% "none",
          tipo_modelo = input$model_type %||% if (isTRUE(is_logistic)) "logistico" else "linear",
          regressao_por_grupo = isTRUE(input$grp_reg),
          mostrar_equacao = isTRUE(input$show_eq),
          tema = input$graph_theme
        ),
        saidas_disponiveis = c(
          "narrativa", "tabela", "grafico", "pressupostos", "diagnosticos", "console"
        ),
        resultado_resumo = resumo,
        codigo_r = paste(r_code_text(), collapse = "\n")
      )
    })

    exec_ctrl <- execucao_explicita_server(
      input, output, session, assinatura_execucao, model_fit,
      nome_analise = if (isTRUE(is_logistic)) "A regressão logística" else "A regressão",
      gatilho_rv = gatilho_execucao
    )

    invisible(list(
      modelo = model_fit,
      base_contexto = base_contexto,
      dados = dados_modulo,
      estado_execucao = estado_execucao,
      estado_execucao_ui = exec_ctrl$estado,
      execucao_atualizada = exec_ctrl$atualizada
    ))
  })
}
