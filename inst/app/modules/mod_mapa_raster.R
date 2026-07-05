# Módulo de Mapas — Ambiente em grade (raster/contorno) — menu "Mapas".
# Mostra uma variável ambiental distribuída no espaço (grade lon/lat/valor):
# TSM, clorofila, profundidade etc. Saída ESTÁTICA (ggplot). Segue mapas.md.
# Introdutório: sem modelagem espacial pesada; raster != ponto amostral.

# Grade de exemplo (procedural) — costa norte do PA. Valores sintéticos.
.grade_ambiental_exemplo <- function() {
  g <- expand.grid(lon = seq(-48.5, -46.1, by = 0.15),
                   lat = seq(-1.5, 0.15, by = 0.15))
  g$sst          <- 27.5 + 1.5 * ((g$lat + 1.5) / 1.7)          # mais quente mar afora
  g$clorofila    <- 0.3 + 4.0 * exp(-2.2 * (g$lat + 1.15)^2)    # alta perto da costa
  g$profundidade <- 5 + 62 * ((g$lat + 1.5) / 1.7)             # aumenta mar afora
  g
}

.unidade_var <- function(v) switch(v, sst = "°C", clorofila = "mg/m³", profundidade = "m", "")

mod_mapa_raster_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(1, 1, 1),
      style = "grid-template-columns: 2.8fr 7fr 2.8fr !important;",

      # COLUNA 1: DADOS
      div(
        card(
          card_header("Dados em Grade"),
          card_body(
            style = "padding: 12px 15px;",
            radioButtons(ns("source"), "Fonte:",
                         choices = c("Exemplo: ambiente da costa norte" = "exemplo",
                                     "Meus dados (grade x/y/valor)" = "dados"),
                         selected = "exemplo"),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'exemplo'", ns("source")),
              selectInput(ns("variavel"), "Variável ambiental:",
                          choices = c("Temperatura da superfície (°C)" = "sst",
                                      "Clorofila-a (mg/m³)" = "clorofila",
                                      "Profundidade (m)" = "profundidade"),
                          selected = "sst")
            ),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'dados'", ns("source")),
              selectInput(ns("x_col"), "Coluna X (longitude):", choices = NULL),
              selectInput(ns("y_col"), "Coluna Y (latitude):", choices = NULL),
              selectInput(ns("val_col"), "Variável (valor da grade):", choices = NULL),
              textInput(ns("unidade"), "Unidade da legenda:", value = "")
            )
          )
        ),
        card(
          card_header("Pacote de Estudo R/Quarto"),
          card_body(
            style = "padding: 12px 15px;",
            downloadButton(ns("download_project_zip"), "Exportar Projeto R (.zip)", class = "btn-primary w-100"),
            helpText("Gera um pacote com o código ggplot2 (geom_raster + contorno).", style = "margin-top: 10px; font-size: 0.85rem;")
          )
        )
      ),

      # COLUNA 2: MAPA
      navset_card_tab(
        title = "Mapa Ambiental em Grade",
        nav_panel(
          title = "Mapa", icon = icon("layer-group"),
          card_body(style = "padding: 15px;", plotOutput(ns("map_plot"), height = "560px"))
        )
      ),

      # COLUNA 3: PERSONALIZAÇÃO
      card(
        card_header("Personalização Visual"),
        card_body(
          style = "padding: 12px 15px;",
          textInput(ns("custom_title"), "Título:", value = "Variável ambiental em grade"),
          selectInput(ns("palette"), "Paleta:",
                      choices = c("Viridis" = "viridis", "Magma" = "magma",
                                  "Cividis" = "cividis", "Ocean (EAPA)" = "ocean"),
                      selected = "viridis"),
          checkboxInput(ns("contorno"), "Linhas de contorno", value = TRUE),
          helpText("Raster mostra uma superfície contínua — diferente de pontos medidos. Cheque a unidade na legenda.",
                   style = "font-size: 0.82rem;")
        )
      )
    )
  )
}

mod_mapa_raster_server <- function(id, data_rv, import_info) {
  moduleServer(id, function(input, output, session) {

    ocean_seq <- c("#EAF3F4", "#9AD1D4", "#62B6B7", "#2E7D8F", "#0F3B5F")

    observe({
      d <- data_rv()
      if (is.null(d)) return()
      num_cols <- names(d)[sapply(d, is.numeric)]
      all_cols <- names(d)
      x_guess <- all_cols[grepl("^lon|^x$", tolower(all_cols))]
      y_guess <- all_cols[grepl("^lat|^y$", tolower(all_cols))]
      val_guess <- setdiff(num_cols, c(x_guess, y_guess))
      updateSelectInput(session, "x_col", choices = all_cols, selected = if (length(x_guess)) x_guess[1] else all_cols[1])
      updateSelectInput(session, "y_col", choices = all_cols, selected = if (length(y_guess)) y_guess[1] else all_cols[min(2, length(all_cols))])
      updateSelectInput(session, "val_col", choices = num_cols, selected = if (length(val_guess)) val_guess[1] else num_cols[1])
    })

    # Grade padronizada: x, y, valor (+ unidade)
    grade <- reactive({
      if (input$source == "exemplo") {
        g <- .grade_ambiental_exemplo()
        v <- input$variavel %||% "sst"
        data.frame(x = g$lon, y = g$lat, valor = g[[v]], stringsAsFactors = FALSE)
      } else {
        d <- data_rv(); req(d, input$x_col, input$y_col, input$val_col)
        req(input$x_col %in% names(d), input$y_col %in% names(d), input$val_col %in% names(d))
        data.frame(x = suppressWarnings(as.numeric(d[[input$x_col]])),
                   y = suppressWarnings(as.numeric(d[[input$y_col]])),
                   valor = suppressWarnings(as.numeric(d[[input$val_col]])),
                   stringsAsFactors = FALSE)
      }
    })

    unidade_atual <- reactive({
      if (input$source == "exemplo") .unidade_var(input$variavel %||% "sst") else (input$unidade %||% "")
    })

    make_map <- reactive({
      g <- grade()
      g <- g[!is.na(g$x) & !is.na(g$y) & !is.na(g$valor), , drop = FALSE]
      validate(need(nrow(g) > 0, "Sem dados de grade válidos para exibir."))

      leg <- if (nzchar(unidade_atual())) unidade_atual() else "valor"

      p <- ggplot(g, aes(x = x, y = y)) +
        geom_raster(aes(fill = valor), interpolate = TRUE)

      if (isTRUE(input$contorno)) {
        p <- p + geom_contour(aes(z = valor), color = "white", alpha = 0.5, linewidth = 0.3)
      }

      p <- p + switch(input$palette,
                      "viridis" = scale_fill_viridis_c(option = "D", name = leg),
                      "magma"   = scale_fill_viridis_c(option = "A", name = leg),
                      "cividis" = scale_fill_viridis_c(option = "E", name = leg),
                      "ocean"   = scale_fill_gradientn(colours = ocean_seq, name = leg),
                      scale_fill_viridis_c(name = leg))

      titulo <- if (nzchar(input$custom_title)) input$custom_title else "Variável ambiental em grade"
      p +
        coord_fixed(expand = FALSE) +
        labs(title = titulo, x = "Longitude", y = "Latitude") +
        theme_minimal(base_size = 13) +
        theme(plot.title = element_text(face = "bold", color = "#0F3B5F", size = 15),
              legend.position = "right")
    })

    output$map_plot <- renderPlot({ make_map() })

    output$download_project_zip <- downloadHandler(
      filename = function() paste0("projeto_mapa_ambiente_", format(Sys.Date(), "%Y-%m-%d"), ".zip"),
      content = function(file) {
        proj <- paste0("projeto_mapa_ambiente_", format(Sys.Date(), "%Y-%m-%d"))
        td <- tempdir(); pd <- file.path(td, proj)
        dir.create(pd, showWarnings = FALSE)
        dd <- file.path(pd, "dados"); sc <- file.path(pd, "scripts")
        dir.create(dd, showWarnings = FALSE); dir.create(sc, showWarnings = FALSE)

        g <- grade()
        utils::write.csv(g, file.path(dd, "grade.csv"), row.names = FALSE)
        leg <- if (nzchar(unidade_atual())) unidade_atual() else "valor"
        pal_code <- switch(input$palette,
                           "ocean" = "  scale_fill_gradientn(colours = c('#EAF3F4','#62B6B7','#2E7D8F','#0F3B5F')) +",
                           sprintf("  scale_fill_viridis_c(option = '%s') +",
                                   switch(input$palette, magma = "A", cividis = "E", "D")))
        code <- c(
          "# --- MAPA AMBIENTAL EM GRADE (CatalyseR) ---",
          "# install.packages('ggplot2')",
          "library(ggplot2)",
          "",
          "grade <- read.csv('dados/grade.csv')  # colunas: x, y, valor",
          "",
          "ggplot(grade, aes(x = x, y = y)) +",
          "  geom_raster(aes(fill = valor), interpolate = TRUE) +",
          if (isTRUE(input$contorno)) "  geom_contour(aes(z = valor), color = 'white', alpha = 0.5) +" else NULL,
          pal_code,
          "  coord_fixed() +",
          sprintf("  labs(title = '%s', fill = '%s', x = 'Longitude', y = 'Latitude') +",
                  gsub("'", "", if (nzchar(input$custom_title)) input$custom_title else "Variável ambiental"), leg),
          "  theme_minimal()"
        )
        code <- code[!vapply(code, is.null, logical(1))]
        writeLines(unlist(code), file.path(sc, "mapa_ambiente.R"))
        writeLines(c("Version: 1.0", "RestoreWorkspace: Default", "SaveWorkspace: Default", "Encoding: UTF-8"),
                   file.path(pd, "projeto_analise.Rproj"))
        writeLines(c("PACOTE DE ESTUDO: MAPA AMBIENTAL EM GRADE (CatalyseR)",
                     "- dados/grade.csv : grade x, y, valor.",
                     "- scripts/mapa_ambiente.R : código ggplot2 (raster + contorno)."),
                   file.path(pd, "README.txt"))
        owd <- getwd(); setwd(td); zip::zip(file, files = proj); setwd(owd)
      }
    )
  })
}
