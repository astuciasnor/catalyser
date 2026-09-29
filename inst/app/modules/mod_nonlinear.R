# Módulo de Regressão Não Linear (Modelos de Crescimento e Alometria) para IDE_R

# Garante o carregamento do motor de ajuste não linear (EAPADados ou fallback local)
if (requireNamespace("EAPADados", quietly = TRUE)) {
  library(EAPADados)
} else if (!exists("ajustar_curva")) {
  .fc_path <- file.path("templates", "funcoes_crescimento.R")
  if (file.exists(.fc_path)) source(.fc_path, encoding = "UTF-8")
}

# ---- Heurística: seleciona X/Y padrão por nome de coluna e tipo de modelo ----
# Crescimento (von Bertalanffy, logístico): X = idade/tempo, Y = comprimento.
# Potência (relação peso-comprimento): X = comprimento, Y = peso.
# Demais modelos: evita colunas de ID/ano e usa as duas primeiras numéricas.
.nl_pick_defaults <- function(num_cols, model_type) {
  if (length(num_cols) == 0) return(list(x = NULL, y = NULL))

  match_first <- function(pats) {
    for (p in pats) {
      h <- grep(p, num_cols, ignore.case = TRUE, value = TRUE)
      if (length(h) > 0) return(h[1])
    }
    NA_character_
  }

  age_pat    <- c("idade", "\\bage\\b", "tempo", "\\btime\\b", "^t$", "\\bdias?\\b", "semanas?", "\\bmes(es)?\\b")
  length_pat <- c("comprimento", "compr", "length", "tamanho", "\\bsize\\b", "altura", "^ct", "^lt?$")
  weight_pat <- c("peso", "weight", "massa", "\\bmass\\b", "biomassa", "^wt?$", "^pt$")
  id_pat     <- c("^id", "c[oó]digo", "grade", "lance", "\\bano\\b", "year")

  x <- NA_character_
  y <- NA_character_
  if (model_type %in% c("von_bertalanffy", "logistico")) {
    x <- match_first(age_pat)
    y <- match_first(length_pat)
  } else if (model_type == "potencia") {
    x <- match_first(length_pat)
    y <- match_first(weight_pat)
  }

  # Fallback genérico: prioriza colunas que não sejam de ID/ano
  non_id <- num_cols[!grepl(paste(id_pat, collapse = "|"), num_cols, ignore.case = TRUE)]
  pool <- if (length(non_id) >= 2) non_id else num_cols

  if (is.na(x) || !(x %in% num_cols)) x <- pool[1]
  if (is.na(y) || !(y %in% num_cols) || identical(y, x)) {
    y <- setdiff(pool, x)[1]
    if (is.na(y)) y <- setdiff(num_cols, x)[1]
    if (is.na(y)) y <- x
  }

  list(x = x, y = y)
}

mod_nonlinear_ui <- function(id, model_type) {
  ns <- NS(id)
  
  # Rótulo de título amigável para a barra lateral
  model_label <- switch(model_type,
    "potencia"        = "Modelo de Potência (W = a·L^b)",
    "von_bertalanffy" = "Curva de Von Bertalanffy",
    "logistico"       = "Modelo Logístico Sigmoidal",
    "exponencial"     = "Modelo Exponencial (Y = a·e^(b·X))",
    "polinomial"      = "Modelo Polinomial (Regressão Quadrática)",
    "logaritmica"     = "Modelo Logarítmico (Y = a + b·ln(X))",
    "Modelo Não Linear"
  )
  
  tagList(
    layout_columns(
      col_widths = c(1, 1, 1),
      style = "grid-template-columns: 2.5fr 7fr 2.5fr !important;",
      
      # COLUNA 1: CONFIGURAÇÃO DO MODELO E RELATÓRIOS
      div(
        card(
          card_header(paste("Ajuste:", model_label)),
          card_body(
            style = "padding: 12px 15px;",
            selectInput(ns("var_y"), "Variável Dependente (Y):", choices = NULL),
            div(style = "margin-top: -8px;", selectInput(ns("var_x"), "Variável Independente (X):", choices = NULL))
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
      
      # COLUNA 2: ABAS DE RESULTADOS (PRINCIPAL)
      navset_card_tab(
        id = ns("active_tab"),
        title = "Painel de Resultados:",
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
          title = "Curva Ajustada",
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
            selectInput(ns("graph_theme"), "Tema do Gráfico:", 
                        choices = c("Mínimo" = "minimal", 
                                    "Clássico" = "classic", 
                                    "Preto e Branco" = "bw", 
                                    "Cinza" = "gray", 
                                    "Light" = "light"), 
                        selected = "minimal"),
            # Mostrar a equação somente na aba de "Curva Ajustada"
            conditionalPanel(
              condition = sprintf("input['%s'] == 'Curva Ajustada'", ns("active_tab")),
              checkboxInput(ns("show_eq"), "Exibir Equação do Ajuste", value = TRUE)
            )
          ),
          
          # Mensagem informativa para a Tabela de Resultados
          conditionalPanel(
            condition = sprintf("input['%s'] == 'Tabela de Resultados'", ns("active_tab")),
            helpText(HTML(
              sprintf("<h5>Resultados Estatísticos</h5>
              <p>Esta aba exibe o modelo ajustado, a tabela científica de coeficientes (estimativas, erros padrão, estatísticas t e p-valores de significância) e as métricas globais de ajuste:</p>
              <ul>
                <li><b>Pseudo-R²:</b> Fração da variabilidade total explicada pela curva ajustada.</li>
                <li><b>RSE:</b> Erro padrão residual do ajuste não linear.</li>
                <li><b>AIC:</b> Critério de Informação de Akaike (para comparação de modelos).</li>
              </ul>")
            ))
          )
        )
      )
    )
  )
}

mod_nonlinear_server <- function(id, data_rv, import_info, model_type) {
  moduleServer(id, function(input, output, session) {
    
    # Atualiza as escolhas de variáveis com base nos dados importados
    observe({
      df <- data_rv()
      req(df)
      
      all_cols <- names(df)
      
      # Manter as seleções atuais se elas continuarem válidas no novo dataset
      curr_y <- input$var_y
      curr_x <- input$var_x
      
      num_cols <- names(df)[sapply(df, is.numeric)]
      pick_cols <- if (length(num_cols) >= 1) num_cols else all_cols
      defs <- .nl_pick_defaults(pick_cols, model_type)
      
      selected_y <- if (!is.null(curr_y) && curr_y %in% all_cols) curr_y else defs$y
      selected_x <- if (!is.null(curr_x) && curr_x %in% all_cols) curr_x else defs$x
      
      updateSelectInput(session, "var_y", choices = all_cols, selected = selected_y)
      updateSelectInput(session, "var_x", choices = all_cols, selected = selected_x)
    })
    
    # Quando o usuário muda a aba ativa ou as variáveis, atualizamos os campos para o padrão daquela aba
    observeEvent(list(input$active_tab, input$var_x, input$var_y), {
      req(input$active_tab, input$var_x, input$var_y)
      
      model_label <- switch(model_type,
        "potencia"        = "Modelo de Potência",
        "von_bertalanffy" = "Curva de Von Bertalanffy",
        "logistico"       = "Curva Logística Sigmoidal",
        "exponencial"     = "Curva Exponencial",
        "polinomial"      = "Modelo Polinomial",
        "logaritmica"     = "Curva Logarítmica",
        "Curva Não Linear"
      )
      
      if (input$active_tab == "Curva Ajustada") {
        updateTextInput(session, "custom_title", value = paste(model_label, ":", input$var_y, "vs", input$var_x))
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
    
    # Modelo estatístico reativo (não linear)
    model_fit <- reactive({
      df <- data_rv()
      req(df, input$var_x, input$var_y)
      req(input$var_x %in% names(df), input$var_y %in% names(df))
      
      validate(
        need(is.numeric(df[[input$var_y]]), "A variável dependente (Y) deve ser numérica para este modelo."),
        need(is.numeric(df[[input$var_x]]), "A variável independente (X) deve ser numérica para este modelo.")
      )

      # Remover valores ausentes antes de ajustar o modelo
      clean_df <- df[, c(input$var_x, input$var_y), drop = FALSE]
      clean_df <- na.omit(clean_df)

      validate(need(exists("ajustar_curva"), "Motor de ajuste não-linear indisponível."))
      tryCatch(
        ajustar_curva(clean_df, var_y = input$var_y, var_x = input$var_x, tipo = model_type),
        error = function(e) validate(need(FALSE, paste("Falha no ajuste:", conditionMessage(e))))
      )
    })
    
    # Texto da fórmula ajustada
    output$formula_text <- renderPrint({
      fit <- model_fit()
      req(fit)
      cat("Modelo:", tipo_curva_label(fit$tipo), "\n")
      cat("Equação ajustada:\n  ", equacao_curva(fit), "\n")
    })
    
    # Tabela de coeficientes científica
    output$coef_table <- renderDT({
      fit <- model_fit()
      req(fit)

      df_coef <- as.data.frame(fit$coefs)
      names(df_coef) <- c("Estimativa", "Erro Padrão", "Valor t", "p-valor")
      df_coef <- cbind(Parâmetro = rownames(df_coef), df_coef)
      
      datatable(
        df_coef,
        options = list(dom = 't', ordering = FALSE),
        rownames = FALSE,
        selection = 'none'
      ) %>%
        formatRound(columns = c("Estimativa", "Erro Padrão", "Valor t"), digits = 5) %>%
        formatSignif(columns = "p-valor", digits = 4)
    })
    
    # Sumário de métricas de ajuste do modelo
    output$metrics_summary <- renderUI({
      fit <- model_fit()
      req(fit)

      HTML(paste0(
        "<div style='line-height: 1.3;'>",
        "<p style='margin-bottom: 5px;'><b>Pseudo-R²:</b> ", round(fit$pseudo_r2, 4), " (", round(fit$pseudo_r2 * 100, 2), "%)</p>",
        "<p style='margin-bottom: 5px;'><b>Erro Padrão Residual (RSE):</b> ", round(fit$rse, 4), "</p>",
        "<p style='margin-bottom: 5px;'><b>AIC:</b> ", if (is.na(fit$aic)) "-" else round(fit$aic, 2), "</p>",
        "<p style='margin-bottom: 5px;'><b>N (observações):</b> ", fit$n, "</p>",
        "<p style='margin-bottom: 0;'><b>Equação ajustada:</b> ", equacao_curva(fit), "</p>",
        "</div>"
      ))
    })
    
    # Gráfico 1: Curva Ajustada
    output$fit_plot <- renderPlot({
      df <- data_rv()
      req(df, input$var_x, input$var_y)
      fit <- model_fit()
      req(fit)

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
    })
    
    # Gráfico 2: Resíduos vs Ajustados
    output$resid_fit_plot <- renderPlot({
      fit <- model_fit()
      req(fit)
      mdl <- fit$modelo

      diag_data <- data.frame(
        Ajustados = fitted(mdl),
        Residuos = residuals(mdl)
      )
      
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

      std_resid <- as.numeric(scale(residuals(fit$modelo)))
      diag_data <- data.frame(ResiduosStd = std_resid)
      
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
        stat_qq_line(color = "#0d6efd", linewidth = 1) +
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

  })
}
