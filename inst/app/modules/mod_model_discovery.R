# Módulo Descobrindo o Modelo (Trendlines do Excel) para IDE_R

mod_model_discovery_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(1, 1),
      style = "grid-template-columns: 3fr 9fr !important;",
      
      # COLUNA 1: CONFIGURAÇÃO E ESCOLHA DE MODELOS
      div(
        card(
          card_header("Configuração do Ajuste"),
          card_body(
            style = "padding: 12px 15px;",
            selectInput(ns("var_y"), "Variável Dependente (Y):", choices = NULL),
            div(style = "margin-top: -8px;", selectInput(ns("var_x"), "Variável Independente (X):", choices = NULL)),
            
            checkboxGroupInput(
              inputId = ns("selected_curves"),
              label = "Exibir no Gráfico:",
              choices = c(
                "Linear" = "linear",
                "Exponencial" = "exponencial",
                "Logarítmico" = "logaritmica",
                "Potência" = "potencia",
                "Polinomial (Grau 2)" = "polinomial"
              ),
              selected = c("linear")
            )
          )
        ),
        card(
          card_header("Exportar Análise"),
          card_body(
            style = "padding: 12px 15px;",
            downloadButton(ns("download_code"), "Baixar Script (.R)", class = "btn-success w-100"),
            helpText("Baixa o script R que ajusta todos os modelos e gera o gráfico correspondente.", style = "margin-top: 10px; margin-bottom: 0; font-size: 0.85rem;")
          )
        )
      ),
      
      # COLUNA 2: GRÁFICO E TABELA DE COMPARATIVO DE R²
      navset_card_tab(
        title = "Explorador e Comparador de Modelos",
        nav_panel(
          title = "Comparativo de Modelos",
          icon = icon("chart-line"),
          card_body(
            style = "padding: 15px;",
            plotOutput(ns("discovery_plot"), height = "400px"),
            hr(style = "margin: 15px 0; border-color: #dee2e6;"),
            h5("Comparação dos Ajustes (Ordenado por R²)", style = "margin-bottom: 10px; font-weight: bold; color: #495057;"),
            DTOutput(ns("summary_table"))
          )
        ),
        nav_panel(
          title = "Código R",
          icon = icon("code"),
          card_body(
            p("Abaixo está o script R de reprodutibilidade exata contendo o ajuste de todos os modelos e o gráfico correspondente:"),
            verbatimTextOutput(ns("r_code_preview"))
          )
        )
      )
    )
  )
}

mod_model_discovery_server <- function(id, data_rv, import_info) {
  moduleServer(id, function(input, output, session) {
    
    # Atualiza as escolhas de variáveis com base nas colunas numéricas
    observe({
      df <- data_rv()
      req(df)
      num_cols <- names(df)[sapply(df, is.numeric)]
      
      curr_y <- input$var_y
      curr_x <- input$var_x
      
      selected_y <- if (!is.null(curr_y) && curr_y %in% num_cols) curr_y else (if (length(num_cols) > 0) num_cols[1] else NULL)
      selected_x <- if (!is.null(curr_x) && curr_x %in% num_cols) curr_x else (if (length(num_cols) > 1 && num_cols[1] == selected_y) num_cols[2] else if (length(num_cols) > 0) num_cols[1] else NULL)
      
      updateSelectInput(session, "var_y", choices = num_cols, selected = selected_y)
      updateSelectInput(session, "var_x", choices = num_cols, selected = selected_x)
    })
    
    # Tabela comparativa reativa dos modelos com suas equações e R²
    models_summary <- reactive({
      df <- data_rv()
      req(df, input$var_x, input$var_y)
      
      clean_df <- df[, c(input$var_x, input$var_y), drop = FALSE]
      clean_df <- na.omit(clean_df)
      names(clean_df) <- c("x", "y")
      
      req(nrow(clean_df) > 2)
      
      # 1. Linear
      fit_lin <- tryCatch(lm(y ~ x, data = clean_df), error = function(e) NULL)
      r2_lin <- if (!is.null(fit_lin)) summary(fit_lin)$r.squared else NA
      eq_lin <- if (!is.null(fit_lin)) sprintf("Y = %.4f + (%.4f) * X", coef(fit_lin)[1], coef(fit_lin)[2]) else "Erro no ajuste"
      
      # 2. Exponencial
      fit_exp <- tryCatch({
        if (any(clean_df$y <= 0)) stop("Valores de Y <= 0")
        lm(log(y) ~ x, data = clean_df)
      }, error = function(e) NULL)
      r2_exp <- if (!is.null(fit_exp)) summary(fit_exp)$r.squared else NA
      eq_exp <- if (!is.null(fit_exp)) sprintf("Y = %.4f * e^(%.4f * X)", exp(coef(fit_exp)[1]), coef(fit_exp)[2]) else "N/A (Y deve ser > 0)"
      
      # 3. Logarítmico
      fit_log <- tryCatch({
        if (any(clean_df$x <= 0)) stop("Valores de X <= 0")
        lm(y ~ log(x), data = clean_df)
      }, error = function(e) NULL)
      r2_log <- if (!is.null(fit_log)) summary(fit_log)$r.squared else NA
      eq_log <- if (!is.null(fit_log)) sprintf("Y = %.4f + (%.4f) * ln(X)", coef(fit_log)[1], coef(fit_log)[2]) else "N/A (X deve ser > 0)"
      
      # 4. Potência
      fit_pot <- tryCatch({
        if (any(clean_df$y <= 0) || any(clean_df$x <= 0)) stop("Valores <= 0")
        lm(log(y) ~ log(x), data = clean_df)
      }, error = function(e) NULL)
      r2_pot <- if (!is.null(fit_pot)) summary(fit_pot)$r.squared else NA
      eq_pot <- if (!is.null(fit_pot)) sprintf("Y = %.4f * X^(%.4f)", exp(coef(fit_pot)[1]), coef(fit_pot)[2]) else "N/A (X e Y devem ser > 0)"
      
      # 5. Polinomial (Grau 2)
      fit_poly <- tryCatch(lm(y ~ x + I(x^2), data = clean_df), error = function(e) NULL)
      r2_poly <- if (!is.null(fit_poly)) summary(fit_poly)$r.squared else NA
      eq_poly <- if (!is.null(fit_poly)) sprintf("Y = %.4f + (%.4f) * X + (%.4f) * X²", coef(fit_poly)[1], coef(fit_poly)[2], coef(fit_poly)[3]) else "Erro no ajuste"
      
      # Monta data frame
      tbl <- data.frame(
        ID = c("linear", "exponencial", "logaritmica", "potencia", "polinomial"),
        Modelo = c("Linear", "Exponencial", "Logarítmico", "Potência", "Polinomial (Grau 2)"),
        Equacao = c(eq_lin, eq_exp, eq_log, eq_pot, eq_poly),
        R2 = c(r2_lin, r2_exp, r2_log, r2_pot, r2_poly),
        Cor = c("#0d6efd", "#198754", "#fd7e14", "#6f42c1", "#dc3545"),
        stringsAsFactors = FALSE
      )
      
      # Ordena por R2 de forma decrescente (NAs por último)
      tbl <- tbl[order(tbl$R2, decreasing = TRUE, na.last = TRUE), ]
      tbl
    })
    
    # Gráfico de Dispersão com Curvas selecionadas sobrepostas
    output$discovery_plot <- renderPlot({
      df <- data_rv()
      req(df, input$var_x, input$var_y)
      
      clean_df <- df[, c(input$var_x, input$var_y), drop = FALSE]
      clean_df <- na.omit(clean_df)
      names(clean_df) <- c("x", "y")
      req(nrow(clean_df) > 2)
      
      x_range <- range(clean_df$x, na.rm = TRUE)
      grade <- data.frame(x = seq(x_range[1], x_range[2], length.out = 300))
      
      p <- ggplot(clean_df, aes(x = x, y = y)) +
        geom_point(color = "#495057", alpha = 0.6, size = 2.5) +
        theme_minimal(base_size = 14) +
        labs(
          title = paste("Explorador de Ajustes:", input$var_y, "vs", input$var_x),
          x = input$var_x,
          y = input$var_y
        ) +
        theme(
          plot.title = element_text(face = "bold", size = 16, color = "#212529"),
          legend.position = "bottom"
        )
      
      curves <- input$selected_curves
      
      # 1. Linear
      if ("linear" %in% curves) {
        fit <- tryCatch(lm(y ~ x, data = clean_df), error = function(e) NULL)
        if (!is.null(fit)) {
          grade$y_lin <- predict(fit, newdata = grade)
          p <- p + geom_line(data = grade, aes(x = x, y = y_lin, color = "Linear"), linewidth = 1.2)
        }
      }
      # 2. Exponencial
      if ("exponencial" %in% curves && !any(clean_df$y <= 0)) {
        fit <- tryCatch(lm(log(y) ~ x, data = clean_df), error = function(e) NULL)
        if (!is.null(fit)) {
          grade$y_exp <- exp(predict(fit, newdata = grade))
          p <- p + geom_line(data = grade, aes(x = x, y = y_exp, color = "Exponencial"), linewidth = 1.2)
        }
      }
      # 3. Logarítmico
      if ("logaritmica" %in% curves && !any(clean_df$x <= 0)) {
        fit <- tryCatch(lm(y ~ log(x), data = clean_df), error = function(e) NULL)
        if (!is.null(fit)) {
          grade$y_log <- predict(fit, newdata = grade)
          p <- p + geom_line(data = grade, aes(x = x, y = y_log, color = "Logarítmico"), linewidth = 1.2)
        }
      }
      # 4. Potência
      if ("potencia" %in% curves && !any(clean_df$y <= 0) && !any(clean_df$x <= 0)) {
        fit <- tryCatch(lm(log(y) ~ log(x), data = clean_df), error = function(e) NULL)
        if (!is.null(fit)) {
          grade$y_pot <- exp(predict(fit, newdata = grade))
          p <- p + geom_line(data = grade, aes(x = x, y = y_pot, color = "Potência"), linewidth = 1.2)
        }
      }
      # 5. Polinomial
      if ("polinomial" %in% curves) {
        fit <- tryCatch(lm(y ~ x + I(x^2), data = clean_df), error = function(e) NULL)
        if (!is.null(fit)) {
          grade$y_poly <- predict(fit, newdata = grade)
          p <- p + geom_line(data = grade, aes(x = x, y = y_poly, color = "Polinomial (Grau 2)"), linewidth = 1.2)
        }
      }
      
      p + scale_color_manual(
        name = "Linhas de Tendência",
        values = c(
          "Linear" = "#0d6efd",
          "Exponencial" = "#198754",
          "Logarítmico" = "#fd7e14",
          "Potência" = "#6f42c1",
          "Polinomial (Grau 2)" = "#dc3545"
        )
      )
    })
    
    # Tabela DT comparativa
    output$summary_table <- renderDT({
      tbl <- models_summary()
      req(tbl)
      
      tbl$Visual <- sprintf(
        "<span style='color: %s; font-weight: bold; font-size: 1.1rem; line-height: 1;'>⬤ <span style='color: #212529; font-weight: 500; font-size: 0.9rem;'>%s</span></span>",
        tbl$Cor, tbl$Modelo
      )
      
      disp_tbl <- tbl[, c("Visual", "Equacao", "R2")]
      names(disp_tbl) <- c("Modelo", "Equação Ajustada", "R²")
      
      datatable(
        disp_tbl,
        escape = FALSE,
        options = list(dom = 't', ordering = FALSE, pageLength = 5),
        rownames = FALSE,
        selection = 'none'
      ) %>%
        formatRound(columns = "R²", digits = 4)
    })
    
    # Código de Reprodutibilidade reativo
    r_code_text <- reactive({
      req(input$var_x, input$var_y, import_info())
      info <- import_info()
      
      code <- c(
        "# --- Código de Reprodutibilidade (Descobrindo o Modelo) ---",
        "library(ggplot2)",
        "library(readxl)",
        ""
      )
      
      if (info$source == "package") {
        code <- c(code,
          "# Carregar pacote e dataset",
          "library(EAPADados)",
          sprintf("dados <- as.data.frame(%s)", info$package_dataset),
          ""
        )
      } else {
        ext <- tolower(tools::file_ext(info$file_name))
        if (ext %in% c("xlsx", "xls")) {
          code <- c(code,
            "# Carregar dados do Excel",
            sprintf("dados <- as.data.frame(read_excel('%s', sheet = '%s'))", info$file_name, info$excel_sheet),
            ""
          )
        } else {
          code <- c(code,
            "# Carregar dados do CSV",
            sprintf("dados <- read.csv('%s', header = %s, sep = '%s', dec = '%s')", 
                    info$file_name, as.character(info$csv_header), info$csv_sep, info$csv_dec),
            ""
          )
        }
      }
      
      code <- c(code,
        "# Limpar dados ausentes",
        sprintf("dados_limpos <- na.omit(dados[, c('%s', '%s')])", input$var_x, input$var_y),
        sprintf("names(dados_limpos) <- c('x', 'y')"),
        ""
      )
      
      code <- c(code,
        "# 1. Ajuste do Modelo Linear",
        "fit_lin <- lm(y ~ x, data = dados_limpos)",
        "print(summary(fit_lin))",
        ""
      )
      
      code <- c(code,
        "# 2. Ajuste do Modelo Exponencial",
        "if (all(dados_limpos$y > 0)) {",
        "  fit_exp <- lm(log(y) ~ x, data = dados_limpos)",
        "  print(summary(fit_exp))",
        "}",
        ""
      )
      
      code <- c(code,
        "# 3. Ajuste do Modelo Logarítmico",
        "if (all(dados_limpos$x > 0)) {",
        "  fit_log <- lm(y ~ log(x), data = dados_limpos)",
        "  print(summary(fit_log))",
        "}",
        ""
      )
      
      code <- c(code,
        "# 4. Ajuste do Modelo de Potência",
        "if (all(dados_limpos$y > 0) && all(dados_limpos$x > 0)) {",
        "  fit_pot <- lm(log(y) ~ log(x), data = dados_limpos)",
        "  print(summary(fit_pot))",
        "}",
        ""
      )
      
      code <- c(code,
        "# 5. Ajuste do Modelo Polinomial (Grau 2)",
        "fit_poly <- lm(y ~ x + I(x^2), data = dados_limpos)",
        "print(summary(fit_poly))",
        ""
      )
      
      code <- c(code,
        "# --- Gráfico de Comparação com ggplot2 ---",
        "p <- ggplot(dados_limpos, aes(x = x, y = y)) +",
        "  geom_point(color = '#495057', alpha = 0.6, size = 2.5) +",
        "  theme_minimal(base_size = 14) +",
        sprintf("  labs(title = 'Comparação de Modelos', x = '%s', y = '%s') +", input$var_x, input$var_y),
        "  theme(plot.title = element_text(face = 'bold', size = 16))",
        "",
        "# Adicionar curvas baseadas nas seleções",
        "x_seq <- seq(min(dados_limpos$x), max(dados_limpos$x), length.out = 300)",
        "grade <- data.frame(x = x_seq)",
        ""
      )
      
      curves <- input$selected_curves
      if ("linear" %in% curves) {
        code <- c(code,
          "grade$y_lin <- predict(fit_lin, newdata = grade)",
          "p <- p + geom_line(data = grade, aes(x = x, y = y_lin, color = 'Linear'), linewidth = 1.2)",
          ""
        )
      }
      if ("exponencial" %in% curves) {
        code <- c(code,
          "if (exists('fit_exp')) {",
          "  grade$y_exp <- exp(predict(fit_exp, newdata = grade))",
          "  p <- p + geom_line(data = grade, aes(x = x, y = y_exp, color = 'Exponencial'), linewidth = 1.2)",
          "}",
          ""
        )
      }
      if ("logaritmica" %in% curves) {
        code <- c(code,
          "if (exists('fit_log')) {",
          "  grade$y_log <- predict(fit_log, newdata = grade)",
          "  p <- p + geom_line(data = grade, aes(x = x, y = y_log, color = 'Logarítmico'), linewidth = 1.2)",
          "}",
          ""
        )
      }
      if ("potencia" %in% curves) {
        code <- c(code,
          "if (exists('fit_pot')) {",
          "  grade$y_pot <- exp(predict(fit_pot, newdata = grade))",
          "  p <- p + geom_line(data = grade, aes(x = x, y = y_pot, color = 'Potência'), linewidth = 1.2)",
          "}",
          ""
        )
      }
      if ("polinomial" %in% curves) {
        code <- c(code,
          "grade$y_poly <- predict(fit_poly, newdata = grade)",
          "p <- p + geom_line(data = grade, aes(x = x, y = y_poly, color = 'Polinomial (Grau 2)'), linewidth = 1.2)",
          ""
        )
      }
      
      code <- c(code,
        "p + scale_color_manual(",
        "  name = 'Linhas de Tendência',",
        "  values = c('Linear' = '#0d6efd', 'Exponencial' = '#198754', 'Logarítmico' = '#fd7e14', 'Potência' = '#6f42c1', 'Polinomial (Grau 2)' = '#dc3545')",
        ")"
      )
      
      paste(code, collapse = "\n")
    })
    
    # Preview do código na UI
    output$r_code_preview <- renderPrint({
      cat(r_code_text())
    })
    
    # Download do script .R
    output$download_code <- downloadHandler(
      filename = function() {
        paste0("descobrindo_modelo_", format(Sys.Date(), "%Y-%m-%d"), ".R")
      },
      content = function(file) {
        writeLines(r_code_text(), file)
      }
    )
  })
}
