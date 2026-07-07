# Módulo de Séries Temporais (menu "Estatísticas Avançadas").
# Stack MODERNO (tidyverts): tsibble + feasts + fabletools, tudo em ggplot2 e
# integrado ao tidyverse. Funciona com QUALQUER conjunto que tenha uma coluna de
# data (ou ano numérico) e uma variável numérica. Exemplo: EAPADados::cpue_tubarao.
# Introdutório: série, sazonalidade (gg_season), decomposição STL e ACF.
# (Modelagem ARIMA/forecast com fable fica para uma v2.)

# Carrega o exemplo cpue_tubarao do EAPADados (ou NULL se indisponível)
.carrega_cpue_tubarao <- function() {
  if (!requireNamespace("EAPADados", quietly = TRUE)) return(NULL)
  e <- new.env()
  ok <- tryCatch({ utils::data("cpue_tubarao", package = "EAPADados", envir = e); TRUE },
                 error = function(err) FALSE)
  if (!ok) return(NULL)
  get("cpue_tubarao", envir = e)
}

# Interpolação linear de NAs (STL/ACF não aceitam buracos) — base R, sem dependência
.interp_na <- function(x) {
  i <- seq_along(x); ok <- !is.na(x)
  if (sum(ok) < 2) return(x)
  stats::approx(i[ok], x[ok], xout = i, rule = 2)$y
}

mod_series_temporais_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(1, 1, 1),
      style = "grid-template-columns: 2.8fr 7fr 2.8fr !important;",

      # COLUNA 1: CONFIGURAÇÃO
      div(
        card(
          card_header("Configuração da Série"),
          card_body(
            style = "padding: 12px 15px;",
            radioButtons(ns("source"), "Fonte:",
                         choices = c("Exemplo: CPUE de tubarão" = "exemplo",
                                     "Meus dados" = "dados"),
                         selected = "exemplo"),
            selectInput(ns("date_col"), "Coluna de data (ou ano):", choices = NULL),
            selectInput(ns("value_col"), "Variável (valor numérico):", choices = NULL),
            selectInput(ns("agregacao"), "Granularidade:",
                        choices = c("Mensal (média)" = "mensal", "Anual (média)" = "anual"),
                        selected = "mensal"),
            checkboxInput(ns("trend"), "Linha de tendência (loess)", value = TRUE),
            checkboxInput(ns("mm"), "Média móvel", value = FALSE),
            conditionalPanel(
              condition = sprintf("input['%s']", ns("mm")),
              sliderInput(ns("mm_janela"), "Janela da média móvel:", min = 2, max = 12, value = 3, step = 1)
            )
          )
        ),
        card(
          card_header("Pacote de Estudo R/Quarto"),
          card_body(
            style = "padding: 12px 15px;",
            downloadButton(ns("download_project_zip"), "Exportar Projeto R (.zip)", class = "btn-primary w-100"),
            helpText("Gera um pacote com o código tsibble + feasts + ggplot2.", style = "margin-top: 10px; font-size: 0.85rem;")
          )
        )
      ),

      # COLUNA 2: GRÁFICOS
      navset_card_tab(
        title = "Série Temporal (tidyverts)",
        nav_panel(title = "Série no tempo", icon = icon("chart-area"),
          card_body(style = "padding: 15px;", plotOutput(ns("ts_plot"), height = "460px"))),
        nav_panel(title = "Sazonalidade", icon = icon("calendar"),
          card_body(style = "padding: 15px;", plotOutput(ns("season_plot"), height = "460px"))),
        nav_panel(title = "Decomposição (STL)", icon = icon("layer-group"),
          card_body(style = "padding: 15px;", plotOutput(ns("stl_plot"), height = "500px"))),
        nav_panel(title = "Autocorrelação (ACF)", icon = icon("wave-square"),
          card_body(style = "padding: 15px;", plotOutput(ns("acf_plot"), height = "420px")))
      ),

      # COLUNA 3: PERSONALIZAÇÃO
      card(
        card_header("Personalização Visual"),
        card_body(
          style = "padding: 12px 15px;",
          textInput(ns("custom_title"), "Título:", value = "CPUE ao longo do tempo"),
          textInput(ns("custom_subtitle"), "Subtítulo:", value = "Tendência de queda ao longo dos anos"),
          textInput(ns("y_label"), "Rótulo do eixo Y:", value = "CPUE"),
          helpText(HTML("Stack moderno <b>tidyverts</b>: <code>tsibble</code> + <code>feasts</code>.<br>Tendência = rumo geral; Sazonalidade/STL = padrão por mês; ACF = autocorrelação."),
                   style = "font-size: 0.8rem;")
        )
      )
    )
  )
}

mod_series_temporais_server <- function(id, data_rv, import_info) {
  moduleServer(id, function(input, output, session) {

    active_data <- reactive({
      if (input$source == "exemplo") .carrega_cpue_tubarao() else data_rv()
    })

    observe({
      d <- active_data()
      if (is.null(d)) return()
      all_cols <- names(d); num_cols <- names(d)[sapply(d, is.numeric)]
      data_guess <- names(d)[vapply(d, function(x) inherits(x, "Date"), logical(1))]
      if (length(data_guess) == 0) data_guess <- all_cols[grepl("data|date|ano|year", tolower(all_cols))]
      updateSelectInput(session, "date_col", choices = all_cols,
                        selected = if (length(data_guess)) data_guess[1] else all_cols[1])
      val_guess <- num_cols[grepl("cpue|valor|captura|biomassa", tolower(num_cols))]
      updateSelectInput(session, "value_col", choices = num_cols,
                        selected = if (length(val_guess)) val_guess[1] else num_cols[1])
    })

    # --- tsibble regular (índice yearmonth ou ano), tidyverts --------------
    serie_ts <- reactive({
      validate(need(requireNamespace("tsibble", quietly = TRUE) && requireNamespace("dplyr", quietly = TRUE),
        "Para séries temporais, instale: install.packages(c('tsibble','feasts','fabletools','dplyr','lubridate'))."))

      d <- active_data()
      validate(need(!is.null(d), "Fonte indisponível: instale o EAPADados::cpue_tubarao ou escolha 'Meus dados'."))
      req(input$date_col, input$value_col)
      datac <- input$date_col; valc <- input$value_col
      req(datac %in% names(d), valc %in% names(d))

      raw <- d[[datac]]
      dt <- suppressWarnings(as.Date(raw))
      # Se não for data mas for ano numérico (ex.: 1995), converte para 1º de janeiro
      if (all(is.na(dt)) && is.numeric(raw) && all(raw >= 1900 & raw <= 2100, na.rm = TRUE)) {
        dt <- as.Date(paste0(as.integer(raw), "-01-01"))
      }
      df <- data.frame(data = dt, valor = suppressWarnings(as.numeric(d[[valc]])))
      df <- df[!is.na(df$data) & !is.na(df$valor), , drop = FALSE]
      validate(need(nrow(df) > 3, "Sem série válida — confira a coluna de data."))

      ag <- if (is.null(input$agregacao)) "mensal" else input$agregacao
      if (ag == "anual") {
        ag_df <- df |>
          dplyr::mutate(idx = as.integer(format(data, "%Y"))) |>
          dplyr::group_by(idx) |>
          dplyr::summarise(valor = mean(valor), .groups = "drop")
      } else {
        ag_df <- df |>
          dplyr::mutate(idx = tsibble::yearmonth(data)) |>
          dplyr::group_by(idx) |>
          dplyr::summarise(valor = mean(valor), .groups = "drop")
      }
      tsibble::fill_gaps(tsibble::as_tsibble(ag_df, index = idx))
    })

    # Versão data.frame com eixo Date, para o gráfico principal customizável
    serie_df <- reactive({
      ts <- serie_ts()
      idxname <- tsibble::index_var(ts)
      d <- as.data.frame(ts)
      idxv <- d[[idxname]]
      d$data <- if (inherits(idxv, "yearmonth")) as.Date(idxv)
                else if (is.numeric(idxv)) as.Date(paste0(idxv, "-01-01"))
                else as.Date(idxv)
      d
    })

    # --- Aba 1: série no tempo (ggplot2) ----------------------------------
    output$ts_plot <- renderPlot({
      d <- serie_df()
      ylab <- if (nzchar(input$y_label)) input$y_label else "valor"
      if (isTRUE(input$mm)) {
        k <- max(2, as.integer(input$mm_janela))
        d$mm <- as.numeric(stats::filter(d$valor, rep(1 / k, k), sides = 2))
      }
      titulo <- if (nzchar(input$custom_title)) input$custom_title else "Série temporal"
      subt <- if (nzchar(input$custom_subtitle)) input$custom_subtitle else NULL
      p <- ggplot(d, aes(x = data, y = valor)) +
        geom_line(color = "#2E7D8F", linewidth = 0.6, na.rm = TRUE) +
        geom_point(color = "#0F3B5F", size = 1.7, alpha = 0.8, na.rm = TRUE)
      if (isTRUE(input$trend)) p <- p + geom_smooth(method = "loess", se = FALSE, color = "#E76F51", linewidth = 1, na.rm = TRUE)
      if (isTRUE(input$mm)) p <- p + geom_line(aes(y = mm), color = "#E89B3C", linewidth = 1, na.rm = TRUE)
      p + labs(title = titulo, subtitle = subt, x = NULL, y = ylab) +
        theme_minimal(base_size = 13) +
        theme(plot.title = element_text(face = "bold", color = "#0F3B5F", size = 15),
              plot.subtitle = element_text(color = "#495057"))
    })

    # --- Aba 2: sazonalidade (feasts::gg_season) --------------------------
    output$season_plot <- renderPlot({
      validate(need(requireNamespace("feasts", quietly = TRUE), "Instale o pacote feasts para a sazonalidade."))
      validate(need((if (is.null(input$agregacao)) "mensal" else input$agregacao) != "anual",
                    "A sazonalidade precisa da granularidade mensal."))
      feasts::gg_season(serie_ts(), valor) +
        labs(title = "Padrão sazonal (gg_season)") +
        theme_minimal(base_size = 13)
    })

    # --- Aba 3: decomposição STL (feasts + fabletools) --------------------
    output$stl_plot <- renderPlot({
      validate(need(requireNamespace("feasts", quietly = TRUE) && requireNamespace("fabletools", quietly = TRUE),
                    "Instale os pacotes feasts e fabletools para a decomposição STL."))
      validate(need((if (is.null(input$agregacao)) "mensal" else input$agregacao) != "anual",
                    "A decomposição STL precisa da granularidade mensal (série sazonal)."))
      ts <- serie_ts(); ts$valor <- .interp_na(ts$valor)
      comp <- tryCatch(fabletools::components(fabletools::model(ts, feasts::STL(valor))),
                       error = function(e) NULL)
      validate(need(!is.null(comp), "Não foi possível decompor (série curta ou muito incompleta)."))
      ggplot2::autoplot(comp) + labs(title = "Decomposição STL: tendência + sazonalidade + resíduo")
    })

    # --- Aba 4: autocorrelação (feasts::ACF) ------------------------------
    output$acf_plot <- renderPlot({
      validate(need(requireNamespace("feasts", quietly = TRUE) && requireNamespace("fabletools", quietly = TRUE),
                    "Instale os pacotes feasts e fabletools para a ACF."))
      ts <- serie_ts(); ts$valor <- .interp_na(ts$valor)
      ggplot2::autoplot(feasts::ACF(ts, valor)) +
        labs(title = "Função de autocorrelação (ACF)") +
        theme_minimal(base_size = 13)
    })

    # --- Exportação do projeto (.zip) -------------------------------------
    output$download_project_zip <- downloadHandler(
      filename = function() paste0("projeto_serie_temporal_", format(Sys.Date(), "%Y-%m-%d"), ".zip"),
      content = function(file) {
        proj <- paste0("projeto_serie_temporal_", format(Sys.Date(), "%Y-%m-%d"))
        td <- tempdir(); pd <- file.path(td, proj)
        dir.create(pd, showWarnings = FALSE)
        dd <- file.path(pd, "dados"); sc <- file.path(pd, "scripts")
        dir.create(dd, showWarnings = FALSE); dir.create(sc, showWarnings = FALSE)
        utils::write.csv(serie_df()[, c("data", "valor")], file.path(dd, "serie.csv"), row.names = FALSE)

        code <- c(
          "# --- SÉRIE TEMPORAL (tidyverts + ggplot2) — CatalyseR ---",
          "# install.packages(c('tsibble','feasts','fabletools','dplyr','lubridate','ggplot2'))",
          "library(tsibble); library(feasts); library(fabletools); library(dplyr); library(ggplot2)",
          "",
          "serie <- read.csv('dados/serie.csv'); serie$data <- as.Date(serie$data)",
          "ts <- serie |> mutate(mes = yearmonth(data)) |>",
          "  as_tsibble(index = mes) |> fill_gaps()",
          "",
          "# 1) Série no tempo",
          "autoplot(ts, valor)",
          "# 2) Sazonalidade",
          "gg_season(ts, valor)",
          "# 3) Decomposição STL",
          "ts |> model(STL(valor)) |> components() |> autoplot()",
          "# 4) Autocorrelação",
          "ts |> ACF(valor) |> autoplot()"
        )
        writeLines(code, file.path(sc, "serie_temporal.R"))
        writeLines(c("Version: 1.0", "RestoreWorkspace: Default", "SaveWorkspace: Default", "Encoding: UTF-8"),
                   file.path(pd, "projeto_analise.Rproj"))
        writeLines(c("PACOTE DE ESTUDO: SÉRIE TEMPORAL (tidyverts)",
                     "- dados/serie.csv : série (data, valor).",
                     "- scripts/serie_temporal.R : tsibble + feasts + ggplot2."),
                   file.path(pd, "README.txt"))
        owd <- getwd(); setwd(td); zip::zip(file, files = proj); setwd(owd)
      }
    )
  })
}
