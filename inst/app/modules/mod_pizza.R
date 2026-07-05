# Módulo de Gráfico de Pizza (menu "Visualizando Dados")
# Frequências de uma variável categórica desenhadas com ggplot2 + coord_polar.

mod_pizza_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(1, 1, 1),
      style = "grid-template-columns: 2.5fr 7fr 2.5fr !important;",

      # COLUNA 1: CONFIGURAÇÃO
      div(
        card(
          card_header("Configuração da Pizza"),
          card_body(
            style = "padding: 12px 15px;",
            selectInput(ns("var"), "Variável Categórica:", choices = NULL),
            radioButtons(ns("label_type"), "Rótulos nas fatias:",
                         choices = c("Percentual (%)" = "pct",
                                     "Contagem" = "count",
                                     "Nenhum" = "none"),
                         selected = "pct"),
            checkboxInput(ns("donut"), "Estilo rosca (donut)", value = FALSE)
          )
        ),
        card(
          card_header("Pacote de Estudo R/Quarto"),
          card_body(
            style = "padding: 12px 15px;",
            downloadButton(ns("download_project_zip"), "Exportar Projeto R (.zip)", class = "btn-primary w-100"),
            helpText("Gera um pacote com o código ggplot2 do gráfico de pizza.", style = "margin-top: 10px; font-size: 0.85rem;")
          )
        )
      ),

      # COLUNA 2: GRÁFICO
      navset_card_tab(
        id = ns("active_tab"),
        title = "Gráfico de Pizza",
        nav_panel(
          title = "Visualização",
          icon = icon("chart-pie"),
          card_body(
            style = "padding: 15px;",
            plotOutput(ns("pie_plot"), height = "450px")
          )
        )
      ),

      # COLUNA 3: PERSONALIZAÇÃO
      card(
        card_header("Personalização Visual"),
        card_body(
          style = "padding: 12px 15px;",
          textInput(ns("custom_title"), "Título do Gráfico:", value = ""),
          selectInput(ns("legend_pos"), "Posição da Legenda:",
                      choices = c("Direita" = "right", "Abaixo" = "bottom",
                                  "Esquerda" = "left", "Ocultar" = "none"),
                      selected = "right")
        )
      )
    )
  )
}

mod_pizza_server <- function(id, data_rv, import_info) {
  moduleServer(id, function(input, output, session) {

    ocean <- c("#0F3B5F", "#2E7D8F", "#62B6B7", "#E89B3C", "#E76F51", "#8FBF9F", "#B5651D", "#9D8DF1")

    # Atualiza o seletor com as variáveis categóricas
    observe({
      df <- data_rv()
      req(df)
      cat_cols <- names(df)[sapply(df, function(col) {
        is.factor(col) || is.character(col) || is.logical(col) || length(unique(col)) < 15
      })]
      if (length(cat_cols) == 0) cat_cols <- names(df)
      updateSelectInput(session, "var", choices = cat_cols, selected = cat_cols[1])
    })

    observeEvent(input$var, {
      req(input$var)
      updateTextInput(session, "custom_title", value = paste("Distribuição de", input$var))
    })

    # Frequências da variável escolhida
    freq_data <- reactive({
      df <- data_rv()
      req(df, input$var, input$var %in% names(df))
      v <- df[[input$var]]
      v <- v[!is.na(v)]
      tb <- as.data.frame(table(v), stringsAsFactors = FALSE)
      names(tb) <- c("categoria", "n")
      tb <- tb[tb$n > 0, , drop = FALSE]
      req(nrow(tb) > 0)
      tb$pct <- tb$n / sum(tb$n) * 100
      tb
    })

    make_plot <- reactive({
      tb <- freq_data()
      tb$lab <- switch(input$label_type,
                       "pct"   = paste0(formatC(tb$pct, format = "f", digits = 1, decimal.mark = ","), "%"),
                       "count" = as.character(tb$n),
                       "none"  = "")
      tb$x <- if (isTRUE(input$donut)) 2 else 1
      fill_cols <- rep(ocean, length.out = nrow(tb))
      title_val <- if (nzchar(input$custom_title)) input$custom_title else paste("Distribuição de", input$var)

      p <- ggplot(tb, aes(x = x, y = n, fill = categoria)) +
        geom_col(width = 1, color = "white", linewidth = 0.6) +
        coord_polar(theta = "y")

      if (input$label_type != "none") {
        p <- p + geom_text(aes(label = lab), position = position_stack(vjust = 0.5),
                           color = "white", fontface = "bold", size = 4.6)
      }
      if (isTRUE(input$donut)) {
        p <- p + xlim(0.5, 2.5)
      }

      p +
        scale_fill_manual(values = fill_cols) +
        theme_void(base_size = 14) +
        labs(title = title_val, fill = input$var) +
        theme(
          plot.title = element_text(face = "bold", size = 16, color = "#212529", hjust = 0.5),
          legend.position = input$legend_pos
        )
    })

    output$pie_plot <- renderPlot({ make_plot() })

    # Exportação do Projeto R (.zip) com o código ggplot2
    output$download_project_zip <- downloadHandler(
      filename = function() paste0("projeto_pizza_", format(Sys.Date(), "%Y-%m-%d"), ".zip"),
      content = function(file) {
        info <- import_info()
        proj_dir_name <- paste0("projeto_pizza_", format(Sys.Date(), "%Y-%m-%d"))
        temp_dir <- tempdir()
        proj_dir <- file.path(temp_dir, proj_dir_name)
        dir.create(proj_dir, showWarnings = FALSE)
        dir_dados <- file.path(proj_dir, "dados")
        dir_scripts <- file.path(proj_dir, "scripts")
        dir.create(dir_dados, showWarnings = FALSE)
        dir.create(dir_scripts, showWarnings = FALSE)

        df_clean <- data_rv()
        req(df_clean)
        save(df_clean, file = file.path(dir_dados, "dados_limpos.rda"))
        write.csv(df_clean, file = file.path(dir_dados, "dados_limpos.csv"), row.names = FALSE)
        ds_name <- if (info$source == "package") info$package_dataset else info$excel_sheet
        tryCatch(export_to_xlsx(df_clean, dataset_name = ds_name, file_path = file.path(dir_dados, "dados_limpos.xlsx")),
                 error = function(e) NULL)

        title_val <- if (nzchar(input$custom_title)) input$custom_title else paste("Distribuição de", input$var)
        xbase <- if (isTRUE(input$donut)) 2L else 1L

        code <- c(
          "# --- GRÁFICO DE PIZZA REPRODUTÍVEL (CatalyseR) ---",
          "# install.packages('ggplot2')",
          "library(ggplot2)",
          "",
          "load('dados/dados_limpos.rda')",
          "dados <- df_clean",
          "",
          sprintf("v <- dados[['%s']]", input$var),
          "v <- v[!is.na(v)]",
          "tb <- as.data.frame(table(v), stringsAsFactors = FALSE)",
          "names(tb) <- c('categoria', 'n')",
          "tb <- tb[tb$n > 0, ]",
          "tb$pct <- tb$n / sum(tb$n) * 100",
          sprintf("tb$lab <- paste0(formatC(tb$pct, format = 'f', digits = 1, decimal.mark = ','), '%%')  # rótulo escolhido: %s", input$label_type),
          "ocean <- c('#0F3B5F','#2E7D8F','#62B6B7','#E89B3C','#E76F51','#8FBF9F','#B5651D','#9D8DF1')",
          "",
          sprintf("p <- ggplot(tb, aes(x = %d, y = n, fill = categoria)) +", xbase),
          "  geom_col(width = 1, color = 'white') +",
          "  coord_polar(theta = 'y') +",
          "  geom_text(aes(label = lab), position = position_stack(vjust = 0.5), color = 'white', fontface = 'bold') +",
          if (isTRUE(input$donut)) "  xlim(0.5, 2.5) +" else NULL,
          "  scale_fill_manual(values = rep(ocean, length.out = nrow(tb))) +",
          "  theme_void() +",
          sprintf("  labs(title = '%s', fill = '%s')", gsub("'", "", title_val), input$var),
          "print(p)"
        )
        code <- code[!vapply(code, is.null, logical(1))]
        writeLines(unlist(code), file.path(dir_scripts, "grafico_pizza.R"))

        writeLines(c("Version: 1.0", "RestoreWorkspace: Default", "SaveWorkspace: Default", "Encoding: UTF-8"),
                   file.path(proj_dir, "projeto_analise.Rproj"))
        writeLines(c(
          "PACOTE DE ESTUDO: GRÁFICO DE PIZZA (CatalyseR)",
          "- projeto_analise.Rproj: duplo clique para abrir no RStudio.",
          "- dados/    : dados limpos (.rda, .csv, .xlsx).",
          "- scripts/grafico_pizza.R : código ggplot2 do gráfico de pizza."
        ), file.path(proj_dir, "README.txt"))

        old_wd <- getwd()
        setwd(temp_dir)
        zip::zip(file, files = proj_dir_name)
        setwd(old_wd)
      }
    )
  })
}
