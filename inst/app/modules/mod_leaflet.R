# Módulo de Mapa Interativo (menu "Mapas") — pacote leaflet.
# Traz um exemplo embutido de pontos da Península de Ajuruteua (Bragança, PA)
# e também plota coordenadas (lat/lon) de qualquer conjunto carregado.

# Pontos de exemplo — Península de Ajuruteua, Bragança-PA (coordenadas aproximadas)
.pontos_ajuruteua <- data.frame(
  local = c("Praia de Ajuruteua", "Vila de Ajuruteua", "Manguezal do Furo Grande",
            "Estrada Bragança–Ajuruteua (mangue)", "Bragança (IECOS/UFPA)"),
  tipo  = c("Praia", "Vila", "Manguezal", "Manguezal", "Cidade"),
  lat   = c(-0.82871, -0.84500, -0.88000, -0.95000, -1.05360),
  lon   = c(-46.60409, -46.61500, -46.63000, -46.68000, -46.76560),
  descricao = c("Ponto turístico e de coleta na orla.",
                "Núcleo urbano da península.",
                "Bosque de mangue — área de estudo do caranguejo-uçá.",
                "Faixa de manguezal ao longo da rodovia PA-458.",
                "Sede do campus e do Instituto de Estudos Costeiros."),
  stringsAsFactors = FALSE
)

mod_leaflet_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(1, 1, 1),
      style = "grid-template-columns: 2.8fr 7fr 2.8fr !important;",

      # COLUNA 1: CONFIGURAÇÃO
      div(
        card(
          card_header("Fonte dos Pontos"),
          card_body(
            style = "padding: 12px 15px;",
            radioButtons(ns("source"), NULL,
                         choices = c("Exemplo: Península de Ajuruteua" = "exemplo",
                                     "Meus dados (colunas lat/lon)" = "dados"),
                         selected = "exemplo"),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'dados'", ns("source")),
              selectInput(ns("lat_col"), "Coluna de Latitude:", choices = NULL),
              selectInput(ns("lon_col"), "Coluna de Longitude:", choices = NULL),
              selectInput(ns("label_col"), "Coluna de Rótulo (opcional):", choices = c("Nenhuma" = "none"))
            )
          )
        ),
        card(
          card_header("Exportar"),
          card_body(
            style = "padding: 12px 15px;",
            downloadButton(ns("download_code"), "Baixar Script (.R)", class = "btn-success w-100"),
            helpText("Baixa o script R com o mapa interativo em leaflet.", style = "margin-top: 10px; font-size: 0.85rem;")
          )
        )
      ),

      # COLUNA 2: MAPA INTERATIVO
      navset_card_tab(
        title = "Mapa Interativo (leaflet)",
        nav_panel(
          title = "Mapa", icon = icon("map-location-dot"),
          card_body(
            style = "padding: 8px;",
            if (requireNamespace("leaflet", quietly = TRUE)) {
              leaflet::leafletOutput(ns("map"), height = "580px")
            } else {
              div(class = "alert alert-warning", style = "margin: 15px;",
                  "Para o mapa interativo, instale o pacote: ",
                  tags$code("install.packages('leaflet')"))
            }
          )
        )
      ),

      # COLUNA 3: PERSONALIZAÇÃO
      card(
        card_header("Aparência do Mapa"),
        card_body(
          style = "padding: 12px 15px;",
          selectInput(ns("basemap"), "Mapa de base:",
                      choices = c("Ruas (OpenStreetMap)" = "osm",
                                  "Satélite (Esri)" = "sat",
                                  "Claro (CartoDB)" = "carto"),
                      selected = "osm"),
          checkboxInput(ns("cluster"), "Agrupar marcadores (cluster)", value = FALSE),
          sliderInput(ns("radius"), "Tamanho do marcador:", min = 4, max = 14, value = 8, step = 1)
        )
      )
    )
  )
}

mod_leaflet_server <- function(id, data_rv, import_info) {
  moduleServer(id, function(input, output, session) {

    # Detecta colunas de latitude/longitude nos dados carregados
    observe({
      df <- data_rv(); req(df)
      num_cols <- names(df)[sapply(df, is.numeric)]
      all_cols <- names(df)
      lat_guess <- all_cols[grepl("^lat", tolower(all_cols))]
      lon_guess <- all_cols[grepl("^lon|^lng", tolower(all_cols))]
      updateSelectInput(session, "lat_col", choices = all_cols,
                        selected = if (length(lat_guess)) lat_guess[1] else (if (length(num_cols)) num_cols[1] else all_cols[1]))
      updateSelectInput(session, "lon_col", choices = all_cols,
                        selected = if (length(lon_guess)) lon_guess[1] else (if (length(num_cols) > 1) num_cols[2] else all_cols[1]))
      updateSelectInput(session, "label_col", choices = c("Nenhuma" = "none", all_cols), selected = "none")
    })

    # Conjunto de pontos a exibir
    pontos <- reactive({
      if (input$source == "exemplo") {
        data.frame(lat = .pontos_ajuruteua$lat, lon = .pontos_ajuruteua$lon,
                   label = .pontos_ajuruteua$local, desc = .pontos_ajuruteua$descricao,
                   stringsAsFactors = FALSE)
      } else {
        df <- data_rv(); req(df, input$lat_col, input$lon_col)
        req(input$lat_col %in% names(df), input$lon_col %in% names(df))
        lat <- suppressWarnings(as.numeric(df[[input$lat_col]]))
        lon <- suppressWarnings(as.numeric(df[[input$lon_col]]))
        lab <- if (!is.null(input$label_col) && input$label_col != "none" && input$label_col %in% names(df)) {
          as.character(df[[input$label_col]])
        } else {
          paste("Ponto", seq_len(nrow(df)))
        }
        d <- data.frame(lat = lat, lon = lon, label = lab, desc = "", stringsAsFactors = FALSE)
        d[!is.na(d$lat) & !is.na(d$lon), , drop = FALSE]
      }
    })

    output$map <- leaflet::renderLeaflet({
      validate(need(requireNamespace("leaflet", quietly = TRUE),
                    "Instale o pacote leaflet para usar o mapa interativo."))
      d <- pontos()
      validate(need(nrow(d) > 0, "Sem coordenadas válidas para exibir."))

      m <- leaflet::leaflet(d)
      m <- switch(input$basemap,
                  "sat"   = leaflet::addProviderTiles(m, "Esri.WorldImagery"),
                  "carto" = leaflet::addProviderTiles(m, "CartoDB.Positron"),
                  leaflet::addTiles(m))

      popup <- paste0("<b>", d$label, "</b>",
                      ifelse(!is.na(d$desc) & nzchar(d$desc), paste0("<br>", d$desc), ""))
      cluster_opt <- if (isTRUE(input$cluster)) leaflet::markerClusterOptions() else NULL

      m <- leaflet::addCircleMarkers(
        m, lng = ~lon, lat = ~lat, popup = popup, label = ~label,
        radius = input$radius, color = "#0F3B5F", weight = 2,
        fillColor = "#2E7D8F", fillOpacity = 0.85,
        clusterOptions = cluster_opt
      )
      leaflet::fitBounds(m, min(d$lon), min(d$lat), max(d$lon), max(d$lat))
    })

    # Download do script R
    output$download_code <- downloadHandler(
      filename = function() paste0("mapa_leaflet_", format(Sys.Date(), "%Y-%m-%d"), ".R"),
      content = function(file) {
        code <- c(
          "# --- MAPA INTERATIVO COM LEAFLET (CatalyseR) ---",
          "# install.packages('leaflet')",
          "library(leaflet)",
          "",
          "# Pontos de exemplo — Península de Ajuruteua, Bragança-PA (aprox.)",
          "pontos <- data.frame(",
          "  local = c('Praia de Ajuruteua','Vila de Ajuruteua','Manguezal do Furo Grande',",
          "            'Estrada Bragança-Ajuruteua','Bragança (IECOS/UFPA)'),",
          "  lat = c(-0.82871, -0.84500, -0.88000, -0.95000, -1.05360),",
          "  lon = c(-46.60409, -46.61500, -46.63000, -46.68000, -46.76560)",
          ")",
          "",
          "leaflet(pontos) |>",
          "  addTiles() |>                       # base OpenStreetMap",
          "  # addProviderTiles('Esri.WorldImagery') |>  # (satélite)",
          "  addCircleMarkers(lng = ~lon, lat = ~lat, label = ~local, popup = ~local,",
          "                   radius = 8, color = '#0F3B5F', fillColor = '#2E7D8F',",
          "                   fillOpacity = 0.85, weight = 2) |>",
          "  fitBounds(min(pontos$lon), min(pontos$lat), max(pontos$lon), max(pontos$lat))"
        )
        writeLines(code, file)
      }
    )
  })
}
