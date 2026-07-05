# Módulo de Mapas — Densidade / heatmap de ocorrências (menu "Mapas").
# Mostra CONCENTRAÇÃO espacial quando há muitos pontos (geom_bin2d / geom_hex).
# Visualização EXPLORATÓRIA — não é inferência espacial. Segue mapas.md.

# Exemplo embutido: ocorrências em dois agrupamentos (estuário e plataforma, PA)
.ocorrencias_exemplo <- function() {
  set.seed(7)
  a <- data.frame(longitude = -46.70 + rnorm(70, 0, 0.12),
                  latitude  = -1.00 + rnorm(70, 0, 0.10),
                  especie   = "Pescada-amarela", stringsAsFactors = FALSE)
  b <- data.frame(longitude = -47.20 + rnorm(55, 0, 0.22),
                  latitude  = -0.60 + rnorm(55, 0, 0.18),
                  especie   = "Camarao-rosa", stringsAsFactors = FALSE)
  d <- rbind(a, b)
  d$longitude <- -abs(d$longitude)
  d
}

mod_mapa_densidade_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(1, 1, 1),
      style = "grid-template-columns: 2.8fr 7fr 2.8fr !important;",

      # COLUNA 1: DADOS
      div(
        card(
          card_header("Ocorrências"),
          card_body(
            style = "padding: 12px 15px;",
            radioButtons(ns("source"), "Fonte:",
                         choices = c("Exemplo: ocorrências (costa PA)" = "exemplo",
                                     "Meus dados (colunas lat/lon)" = "dados"),
                         selected = "exemplo"),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'dados'", ns("source")),
              selectInput(ns("lat_col"), "Coluna de Latitude:", choices = NULL),
              selectInput(ns("lon_col"), "Coluna de Longitude:", choices = NULL)
            ),
            selectInput(ns("filter_col"), "Filtrar por (opcional):", choices = c("Nenhum" = "none")),
            conditionalPanel(
              condition = sprintf("input['%s'] != 'none'", ns("filter_col")),
              selectInput(ns("filter_val"), "Valor:", choices = NULL)
            )
          )
        ),
        card(
          card_header("Pacote de Estudo R/Quarto"),
          card_body(
            style = "padding: 12px 15px;",
            downloadButton(ns("download_project_zip"), "Exportar Projeto R (.zip)", class = "btn-primary w-100"),
            helpText("Gera um pacote com o código ggplot2 (geom_bin2d/geom_hex).", style = "margin-top: 10px; font-size: 0.85rem;")
          )
        )
      ),

      # COLUNA 2: MAPA
      navset_card_tab(
        title = "Densidade / Heatmap de Ocorrências",
        nav_panel(
          title = "Mapa", icon = icon("fire"),
          card_body(style = "padding: 15px;", plotOutput(ns("map_plot"), height = "560px"))
        )
      ),

      # COLUNA 3: PERSONALIZAÇÃO
      card(
        card_header("Personalização Visual"),
        card_body(
          style = "padding: 12px 15px;",
          textInput(ns("custom_title"), "Título:", value = "Concentração de ocorrências"),
          selectInput(ns("metodo"), "Método:",
                      choices = c("Células quadradas (bin2d)" = "bin2d", "Hexágonos (hexbin)" = "hex"),
                      selected = "bin2d"),
          sliderInput(ns("bins"), "Nº de células (bins):", min = 8, max = 40, value = 22, step = 2),
          helpText("Analisar quantidades: mostra onde há MAIS registros. Cuidado — mais registros podem indicar mais esforço de coleta, não mais peixe.",
                   style = "font-size: 0.82rem;")
        )
      )
    )
  )
}

mod_mapa_densidade_server <- function(id, data_rv, import_info) {
  moduleServer(id, function(input, output, session) {

    active_data <- reactive({
      if (input$source == "exemplo") .ocorrencias_exemplo() else data_rv()
    })

    observe({
      d <- active_data(); req(d)
      all_cols <- names(d)
      lat_guess <- all_cols[grepl("^lat", tolower(all_cols))]
      lon_guess <- all_cols[grepl("^lon|^lng", tolower(all_cols))]
      cat_cols <- all_cols[sapply(d, function(x) is.factor(x) || is.character(x) || length(unique(x)) < 15)]
      updateSelectInput(session, "lat_col", choices = all_cols,
                        selected = if (length(lat_guess)) lat_guess[1] else all_cols[1])
      updateSelectInput(session, "lon_col", choices = all_cols,
                        selected = if (length(lon_guess)) lon_guess[1] else all_cols[min(2, length(all_cols))])
      updateSelectInput(session, "filter_col", choices = c("Nenhum" = "none", cat_cols), selected = "none")
    })

    observeEvent(input$filter_col, {
      d <- active_data(); req(d)
      if (is.null(input$filter_col) || input$filter_col == "none" || !(input$filter_col %in% names(d))) {
        updateSelectInput(session, "filter_val", choices = character(0)); return()
      }
      vals <- sort(unique(as.character(d[[input$filter_col]])))
      updateSelectInput(session, "filter_val", choices = vals, selected = vals[1])
    })

    pts <- reactive({
      d <- active_data(); req(d)
      if (input$source == "exemplo") { latc <- "latitude"; lonc <- "longitude" }
      else { latc <- input$lat_col; lonc <- input$lon_col; req(latc, lonc) }
      req(latc %in% names(d), lonc %in% names(d))
      if (!is.null(input$filter_col) && input$filter_col != "none" &&
          input$filter_col %in% names(d) && !is.null(input$filter_val) && nzchar(input$filter_val)) {
        d <- d[as.character(d[[input$filter_col]]) == input$filter_val, , drop = FALSE]
      }
      out <- data.frame(lon = suppressWarnings(as.numeric(d[[lonc]])),
                        lat = suppressWarnings(as.numeric(d[[latc]])))
      out[!is.na(out$lon) & !is.na(out$lat), , drop = FALSE]
    })

    make_map <- reactive({
      d <- pts()
      validate(need(nrow(d) > 0, "Sem coordenadas válidas para exibir."))

      p <- ggplot(d, aes(x = lon, y = lat))
      if (input$metodo == "hex" && requireNamespace("hexbin", quietly = TRUE)) {
        p <- p + geom_hex(bins = input$bins)
      } else {
        p <- p + geom_bin2d(bins = input$bins)
      }
      titulo <- if (nzchar(input$custom_title)) input$custom_title else "Concentração de ocorrências"
      p +
        scale_fill_viridis_c(name = "Nº de registros", option = "C") +
        coord_fixed() +
        labs(title = titulo, x = "Longitude", y = "Latitude") +
        theme_minimal(base_size = 13) +
        theme(plot.title = element_text(face = "bold", color = "#0F3B5F", size = 15),
              legend.position = "right")
    })

    output$map_plot <- renderPlot({ make_map() })

    output$download_project_zip <- downloadHandler(
      filename = function() paste0("projeto_mapa_densidade_", format(Sys.Date(), "%Y-%m-%d"), ".zip"),
      content = function(file) {
        proj <- paste0("projeto_mapa_densidade_", format(Sys.Date(), "%Y-%m-%d"))
        td <- tempdir(); pd <- file.path(td, proj)
        dir.create(pd, showWarnings = FALSE)
        dd <- file.path(pd, "dados"); sc <- file.path(pd, "scripts")
        dir.create(dd, showWarnings = FALSE); dir.create(sc, showWarnings = FALSE)

        utils::write.csv(pts(), file.path(dd, "ocorrencias.csv"), row.names = FALSE)
        geom_code <- if (input$metodo == "hex") sprintf("  geom_hex(bins = %d) +", input$bins)
                     else sprintf("  geom_bin2d(bins = %d) +", input$bins)
        code <- c(
          "# --- MAPA DE DENSIDADE / HEATMAP (CatalyseR) ---",
          "# install.packages('ggplot2')  # hexbin, se usar geom_hex",
          "library(ggplot2)",
          "",
          "d <- read.csv('dados/ocorrencias.csv')  # colunas: lon, lat",
          "",
          "ggplot(d, aes(x = lon, y = lat)) +",
          geom_code,
          "  scale_fill_viridis_c(name = 'Nº de registros', option = 'C') +",
          "  coord_fixed() +",
          sprintf("  labs(title = '%s', x = 'Longitude', y = 'Latitude') +",
                  gsub("'", "", if (nzchar(input$custom_title)) input$custom_title else "Concentração de ocorrências")),
          "  theme_minimal()"
        )
        writeLines(code, file.path(sc, "mapa_densidade.R"))
        writeLines(c("Version: 1.0", "RestoreWorkspace: Default", "SaveWorkspace: Default", "Encoding: UTF-8"),
                   file.path(pd, "projeto_analise.Rproj"))
        writeLines(c("PACOTE DE ESTUDO: MAPA DE DENSIDADE (CatalyseR)",
                     "- dados/ocorrencias.csv : coordenadas lon, lat.",
                     "- scripts/mapa_densidade.R : código ggplot2 (bin2d/hex)."),
                   file.path(pd, "README.txt"))
        owd <- getwd(); setwd(td); zip::zip(file, files = proj); setwd(owd)
      }
    )
  })
}
