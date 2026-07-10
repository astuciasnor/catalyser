# Modulo: Correlacao (Pearson/Spearman/Kendall) — ponte dispersao -> regressao.
# Autocontido (usa cor.test). Piloto do padrao de "analise nova" na v1.
library(shiny)
library(bslib)

# Narrativa em PT (pura, testavel).
relatar_cor <- function(ct, metodo_lbl, vx, vy) {
  est <- unname(ct$estimate); p <- ct$p.value
  forca <- as.character(cut(abs(est), c(-Inf, .1, .3, .5, .7, .9, Inf),
    labels = c("desprezivel", "fraca", "moderada", "forte", "muito forte", "quase perfeita")))
  dir <- if (est >= 0) "positiva" else "negativa"
  ic <- if (!is.null(ct$conf.int)) sprintf(" IC95%% [%.3f, %.3f].", ct$conf.int[1], ct$conf.int[2]) else ""
  sig <- if (p < 0.05) "estatisticamente significativa" else "nao significativa"
  sprintf("Correlacao de %s entre '%s' e '%s': coeficiente = %.3f (associacao %s e %s), p = %s — %s (alfa = 0,05).%s",
          metodo_lbl, vx, vy, est, forca, dir, format.pval(p, digits = 3), sig, ic)
}

mod_correlacao_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(1, 1, 1),
      style = "grid-template-columns: 2.8fr 6.4fr 2.8fr !important;",
      div(
        card(card_header("1. Variáveis"),
          card_body(style = "padding:12px 15px;",
            selectInput(ns("var_x"), "Variável X:", choices = NULL),
            selectInput(ns("var_y"), "Variável Y:", choices = NULL),
            radioButtons(ns("metodo"), "Método:",
              choices = c("Pearson (linear)" = "pearson", "Spearman (postos)" = "spearman", "Kendall (tau)" = "kendall"),
              selected = "pearson"),
            helpText("Pearson exige relação aproximadamente linear; Spearman/Kendall são robustos e não paramétricos."))),
        card(card_header("Exportar"),
          card_body(style = "padding:12px 15px;",
            downloadButton(ns("baixar_script"), "Baixar script .R", class = "btn-outline-secondary btn-sm w-100")))
      ),
      navset_card_tab(
        title = "Correlação",
        nav_panel(title = "Resultado", icon = icon("square-root-variable"),
          card_body(style = "padding:12px 15px;",
            uiOutput(ns("relato")),
            tags$pre(style = "white-space:pre-wrap; font-size:0.85rem; margin-top:8px;", verbatimTextOutput(ns("bruto"))))),
        nav_panel(title = "Dispersão", icon = icon("braille"),
          card_body(style = "padding:12px 15px;", plotOutput(ns("grafico"), height = "420px"))),
        nav_panel(title = "Script", icon = icon("code"),
          card_body(style = "padding:12px 15px;",
            tags$pre(style = "white-space:pre-wrap; font-size:0.82rem;", verbatimTextOutput(ns("script")))))
      ),
      div(
        card(card_header("Como ler"),
          card_body(style = "padding:12px 15px; font-size:0.82rem; line-height:1.5;",
            tags$ul(style = "padding-left:16px; margin:0;",
              tags$li("Coeficiente entre -1 e 1: sinal = direção; módulo = força."),
              tags$li("p < 0,05: correlação significativa."),
              tags$li("Correlação não implica causa."),
              tags$li("Próximo passo natural: regressão linear."))))
      )
    )
  )
}

mod_correlacao_server <- function(id, data_rv, import_info) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    num_cols <- function(df) names(df)[vapply(df, is.numeric, logical(1))]
    observeEvent(data_rv(), {
      df <- data_rv(); req(df); nums <- num_cols(df)
      updateSelectInput(session, "var_x", choices = nums, selected = isolate(input$var_x) %||% nums[1])
      updateSelectInput(session, "var_y", choices = nums,
                        selected = isolate(input$var_y) %||% (if (length(nums) > 1) nums[2] else nums[1]))
    })
    cor_res <- reactive({
      df <- data_rv(); req(df, input$var_x, input$var_y)
      req(input$var_x %in% names(df), input$var_y %in% names(df))
      x <- suppressWarnings(as.numeric(df[[input$var_x]]))
      y <- suppressWarnings(as.numeric(df[[input$var_y]]))
      ok <- is.finite(x) & is.finite(y)
      if (sum(ok) < 3) return(structure(list(erro = "Poucos pares válidos (mínimo 3)."), class = "cor_erro"))
      tryCatch(cor.test(x[ok], y[ok], method = input$metodo %||% "pearson"),
               error = function(e) structure(list(erro = conditionMessage(e)), class = "cor_erro"))
    })
    metodo_lbl <- reactive(c(pearson = "Pearson", spearman = "Spearman", kendall = "Kendall")[input$metodo %||% "pearson"])
    output$relato <- renderUI({
      r <- cor_res(); req(r)
      if (inherits(r, "cor_erro")) return(div(class = "alert alert-warning", r$erro))
      div(class = "alert alert-info", style = "font-size:0.9rem;", relatar_cor(r, metodo_lbl(), input$var_x, input$var_y))
    })
    output$bruto <- renderPrint({
      r <- cor_res(); req(r)
      if (inherits(r, "cor_erro")) { cat(r$erro); return(invisible()) }
      print(r)
    })
    output$grafico <- renderPlot({
      r <- cor_res(); df <- data_rv(); req(df, input$var_x, input$var_y)
      validate(need(!inherits(r, "cor_erro"), if (inherits(r, "cor_erro")) r$erro else ""))
      if (!requireNamespace("ggplot2", quietly = TRUE)) {
        plot(df[[input$var_x]], df[[input$var_y]], xlab = input$var_x, ylab = input$var_y, pch = 19, col = "#2E7D8F")
        return(invisible())
      }
      ggplot2::ggplot(df, ggplot2::aes(x = .data[[input$var_x]], y = .data[[input$var_y]])) +
        ggplot2::geom_point(color = "#2E7D8F", size = 2.4, alpha = 0.8) +
        ggplot2::geom_smooth(method = "lm", se = TRUE, color = "#E76F51", fill = "#F2C9BE") +
        ggplot2::labs(title = sprintf("Dispersão: %s vs %s", input$var_x, input$var_y),
                      subtitle = relatar_cor(r, metodo_lbl(), input$var_x, input$var_y),
                      x = input$var_x, y = input$var_y) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", color = "#0F3B5F"),
                       plot.subtitle = ggplot2::element_text(size = 9, color = "#495057"))
    })
    codigo <- reactive({
      req(input$var_x, input$var_y)
      info <- if (is.function(import_info)) import_info() else NULL
      if (!is.null(info) && identical(info$source, "package")) {
        leitura <- c("library(EAPADados)", sprintf("data(%s)", info$package_dataset), sprintf("dados <- %s", info$package_dataset))
      } else {
        fn <- if (!is.null(info)) info$file_name else "SEU_ARQUIVO.xlsx"
        abn <- if (!is.null(info)) info$excel_sheet else "SUA_ABA"
        leitura <- sprintf('dados <- readxl::read_excel("%s", sheet = "%s")', fn, abn)
      }
      paste(c(
        "# Script gerado pela CatalyseR — Correlacao",
        "library(ggplot2)", leitura, "",
        sprintf('r <- cor.test(dados[["%s"]], dados[["%s"]], method = "%s")', input$var_x, input$var_y, input$metodo %||% "pearson"),
        "print(r)", "",
        sprintf('ggplot(dados, aes(x = `%s`, y = `%s`)) +', input$var_x, input$var_y),
        '  geom_point(color = "#2E7D8F") +',
        '  geom_smooth(method = "lm", se = TRUE, color = "#E76F51") +',
        '  theme_minimal()'
      ), collapse = "\n")
    })
    output$script <- renderText(codigo())
    output$baixar_script <- downloadHandler(
      filename = function() paste0("correlacao_", Sys.Date(), ".R"),
      content = function(file) writeLines(codigo(), file))
  })
}

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a
