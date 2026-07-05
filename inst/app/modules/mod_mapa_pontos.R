# Módulo de Mapas — Pontos, estações e bolhas proporcionais (menu "Mapas").
# Plota coordenadas (lat/lon) sobre a base do geobr (recortada na região dos
# pontos), com tamanho por uma variável (CPUE), cor por grupo e facetas.
# Saída ESTÁTICA (ggplot). Segue o guia mapas.md.

# Exemplo embutido: estações de ictiofauna na costa do Pará (mesmos dados do
# EAPADados::estacoes_ictiofauna). Coordenadas plausíveis; CPUE/abundância sintéticas.
.estacoes_exemplo <- data.frame(
  id_estacao = c("E01", "E02", "E03", "E04", "E05", "E06", "E07", "E08", "E09", "E10", "E11", "E12", "E13", "E14"),
  latitude   = c(-0.829, -0.88, -0.78, -0.95, -1.0536, -0.616, -0.77, -0.5, -0.829, -0.88, -0.78, -1.0536, -0.77, -0.5),
  longitude  = c(-46.604, -46.63, -46.63, -46.68, -46.7656, -47.356, -47.18, -47.0, -46.604, -46.63, -46.63, -46.7656, -47.18, -47.0),
  ano        = c(2023, 2023, 2023, 2023, 2023, 2023, 2023, 2023, 2024, 2024, 2024, 2024, 2024, 2024),
  campanha   = c("Seca", "Seca", "Seca", "Seca", "Seca", "Seca", "Seca", "Seca", "Chuvosa", "Chuvosa", "Chuvosa", "Chuvosa", "Chuvosa", "Chuvosa"),
  ambiente   = c("Praia", "Manguezal", "Estuario", "Manguezal", "Estuario", "Praia", "Plataforma", "Plataforma", "Praia", "Manguezal", "Estuario", "Estuario", "Plataforma", "Plataforma"),
  especie    = c("Pescada-amarela", "Bagre", "Robalo", "Bagre", "Pescada-amarela", "Robalo", "Pescada-amarela", "Pescada-amarela", "Pescada-amarela", "Bagre", "Robalo", "Pescada-amarela", "Pescada-amarela", "Pescada-amarela"),
  cpue       = c(12.4, 28.7, 19.1, 22.3, 15.8, 9.6, 33.5, 41.2, 18.9, 35.1, 24.6, 21.0, 44.8, 52.3),
  abundancia = c(31, 64, 40, 51, 37, 22, 70, 88, 44, 79, 52, 49, 95, 110),
  stringsAsFactors = FALSE
)

mod_mapa_pontos_ui <- function(id, modo = "bolhas") {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(1, 1, 1),
      style = "grid-template-columns: 2.8fr 7fr 2.8fr !important;",

      # COLUNA 1: CONFIGURAÇÃO
      div(
        card(
          card_header("Configuração dos Pontos"),
          card_body(
            style = "padding: 12px 15px;",
            radioButtons(ns("source"), "Fonte:",
                         choices = c("Exemplo: estações de ictiofauna (PA)" = "exemplo",
                                     "Meus dados (colunas lat/lon)" = "dados"),
                         selected = "exemplo"),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'dados'", ns("source")),
              selectInput(ns("lat_col"), "Coluna de Latitude:", choices = NULL),
              selectInput(ns("lon_col"), "Coluna de Longitude:", choices = NULL)
            ),
            selectInput(ns("size_var"), "Tamanho da bolha (opcional):", choices = c("Nenhum" = "none")),
            selectInput(ns("color_var"), "Cor por grupo (opcional):", choices = c("Nenhuma" = "none")),
            selectInput(ns("facet_var"), "Facetas (opcional):", choices = c("Nenhuma" = "none")),
            checkboxInput(ns("show_base"), "Mostrar mapa de base (geobr)", value = TRUE)
          )
        ),
        card(
          card_header("Pacote de Estudo R/Quarto"),
          card_body(
            style = "padding: 12px 15px;",
            downloadButton(ns("download_project_zip"), "Exportar Projeto R (.zip)", class = "btn-primary w-100"),
            helpText("Gera um pacote com o código sf + ggplot2 do mapa de pontos.", style = "margin-top: 10px; font-size: 0.85rem;")
          )
        )
      ),

      # COLUNA 2: MAPA
      navset_card_tab(
        title = "Mapa de Pontos e Estações",
        nav_panel(
          title = "Mapa", icon = icon("location-dot"),
          card_body(style = "padding: 15px;", plotOutput(ns("map_plot"), height = "560px"))
        )
      ),

      # COLUNA 3: PERSONALIZAÇÃO
      card(
        card_header("Personalização Visual"),
        card_body(
          style = "padding: 12px 15px;",
          textInput(ns("custom_title"), "Título:", value = "Estações de coleta"),
          textInput(ns("custom_subtitle"), "Subtítulo:", value = ""),
          if (modo == "bolhas") textInput(ns("size_label"), "Rótulo/unidade da bolha:", value = "CPUE") else NULL,
          selectInput(ns("palette"), "Paleta (cor por grupo):",
                      choices = c("Ocean (EAPA)" = "ocean", "Viridis" = "viridis"), selected = "ocean"),
          checkboxInput(ns("show_scale"), "Escala e Norte", value = TRUE),
          selectInput(ns("label_col"), "Rótulo dos pontos:", choices = c("Nenhum" = "none")),
          checkboxInput(ns("show_labels"), "Mostrar rótulos das estações", value = (modo == "pontos")),
          checkboxInput(ns("show_grid"), "Grade de coordenadas (graus)", value = TRUE),
          checkboxInput(ns("show_inset"), "Mapa de localização (inset)", value = (modo == "pontos")),
          helpText(if (modo == "bolhas")
                     "Analisar quantidades: o tamanho da bolha = magnitude (ex.: CPUE, biomassa) em cada local — mostra onde os valores são maiores."
                   else
                     "Apoiar a amostragem: mostre onde a coleta aconteceu — cobertura espacial, lacunas de amostragem e ambientes cobertos.",
                   style = "font-size: 0.82rem;")
        )
      )
    )
  )
}

mod_mapa_pontos_server <- function(id, data_rv, import_info, modo = "bolhas") {
  moduleServer(id, function(input, output, session) {

    ocean <- c("#0F3B5F", "#2E7D8F", "#62B6B7", "#E89B3C", "#E76F51", "#8FBF9F", "#9D8DF1", "#B5651D")

    # Dados ativos: exemplo embutido ou dados carregados
    active_data <- reactive({
      if (input$source == "exemplo") .estacoes_exemplo else data_rv()
    })

    # Atualiza os seletores conforme os dados ativos
    observe({
      d <- active_data(); req(d)
      all_cols <- names(d)
      num_cols <- names(d)[sapply(d, is.numeric)]
      cat_cols <- names(d)[sapply(d, function(x) is.factor(x) || is.character(x) || is.logical(x) || length(unique(x)) < 15)]

      lat_guess <- all_cols[grepl("^lat", tolower(all_cols))]
      lon_guess <- all_cols[grepl("^lon|^lng", tolower(all_cols))]
      updateSelectInput(session, "lat_col", choices = all_cols,
                        selected = if (length(lat_guess)) lat_guess[1] else all_cols[1])
      updateSelectInput(session, "lon_col", choices = all_cols,
                        selected = if (length(lon_guess)) lon_guess[1] else all_cols[min(2, length(all_cols))])
      updateSelectInput(session, "size_var", choices = c("Nenhum" = "none", num_cols),
                        selected = if (modo == "bolhas" && "cpue" %in% num_cols) "cpue"
                                   else if (modo == "bolhas" && length(num_cols)) num_cols[1] else "none")
      updateSelectInput(session, "color_var", choices = c("Nenhuma" = "none", cat_cols),
                        selected = if ("ambiente" %in% cat_cols) "ambiente" else "none")
      updateSelectInput(session, "facet_var", choices = c("Nenhuma" = "none", cat_cols), selected = "none")
      lab_guess <- all_cols[grepl("estac|^id|nome|local|ponto", tolower(all_cols))]
      updateSelectInput(session, "label_col", choices = c("Nenhum" = "none", all_cols),
                        selected = if (modo == "pontos" && length(lab_guess)) lab_guess[1] else "none")
    })

    # Título/subtítulo informativos conforme o modo e as variáveis
    observeEvent(list(input$size_var, input$color_var), {
      if (modo == "bolhas") {
        v <- if (!is.null(input$size_var) && input$size_var != "none") humaniza_rotulo(input$size_var) else "Magnitude"
        updateTextInput(session, "custom_title", value = paste0(v, " por estação"))
        updateTextInput(session, "custom_subtitle", value = "Tamanho da bolha = magnitude; cor indica o ambiente")
      } else {
        cw <- if (!is.null(input$color_var) && input$color_var != "none") paste0(" por ", humaniza_rotulo(input$color_var)) else ""
        updateTextInput(session, "custom_title", value = paste0("Estações de coleta", cw))
        updateTextInput(session, "custom_subtitle", value = "Cada ponto é uma estação; a cor/forma indica o grupo")
      }
    }, ignoreInit = FALSE)

    # Limites estaduais do geobr (uma vez por sessão)
    estados_base <- reactive({
      if (!requireNamespace("geobr", quietly = TRUE) || !requireNamespace("sf", quietly = TRUE)) return(NULL)
      out <- tryCatch(geobr::read_state(code_state = "all", year = 2020, showProgress = FALSE),
                      error = function(e) NULL)
      if (is.null(out)) NULL else sf::st_transform(out, 4326)
    })

    # Tabela de pontos padronizada (lat, lon e estéticas opcionais)
    pts <- reactive({
      d <- active_data(); req(d)
      if (input$source == "exemplo") {
        latc <- "latitude"; lonc <- "longitude"
      } else {
        latc <- input$lat_col; lonc <- input$lon_col
        req(latc, lonc)
      }
      req(latc %in% names(d), lonc %in% names(d))
      out <- data.frame(lat = suppressWarnings(as.numeric(d[[latc]])),
                        lon = suppressWarnings(as.numeric(d[[lonc]])),
                        stringsAsFactors = FALSE)
      if (!is.null(input$size_var) && input$size_var != "none" && input$size_var %in% names(d))
        out$tamanho <- suppressWarnings(as.numeric(d[[input$size_var]]))
      if (!is.null(input$color_var) && input$color_var != "none" && input$color_var %in% names(d))
        out$grupo <- as.factor(d[[input$color_var]])
      if (!is.null(input$facet_var) && input$facet_var != "none" && input$facet_var %in% names(d))
        out$faceta <- as.factor(d[[input$facet_var]])
      if (!is.null(input$label_col) && input$label_col != "none" && input$label_col %in% names(d))
        out$rotulo <- as.character(d[[input$label_col]])
      out <- out[!is.na(out$lat) & !is.na(out$lon), , drop = FALSE]
      out
    })

    make_map <- reactive({
      d <- pts()
      validate(need(nrow(d) > 0, "Sem coordenadas válidas para exibir."))

      xr <- range(d$lon); yr <- range(d$lat)
      mx <- max(diff(xr), 0.5) * 0.18; my <- max(diff(yr), 0.5) * 0.18
      xlim <- c(xr[1] - mx, xr[2] + mx); ylim <- c(yr[1] - my, yr[2] + my)
      has_size  <- "tamanho" %in% names(d)
      has_color <- "grupo" %in% names(d)
      has_facet <- "faceta" %in% names(d)
      has_label <- isTRUE(input$show_labels) && "rotulo" %in% names(d)
      usar_sf   <- isTRUE(input$show_base) && !is.null(estados_base())
      grade     <- isTRUE(input$show_grid)

      p <- ggplot()
      if (usar_sf) {
        p <- p + geom_sf(data = estados_base(), fill = "#EAF0F2", color = "grey75", linewidth = 0.2)
        pts_sf <- sf::st_as_sf(d, coords = c("lon", "lat"), crs = 4326)
        if (has_size && has_color) p <- p + geom_sf(data = pts_sf, aes(size = tamanho, color = grupo, shape = grupo), alpha = 0.9)
        else if (has_size)         p <- p + geom_sf(data = pts_sf, aes(size = tamanho), color = "#0F3B5F", alpha = 0.9)
        else if (has_color)        p <- p + geom_sf(data = pts_sf, aes(color = grupo, shape = grupo), size = 3.2, alpha = 0.95)
        else                       p <- p + geom_sf(data = pts_sf, color = "#0F3B5F", size = 3.2, alpha = 0.95)
        if (has_label) {
          if (requireNamespace("ggrepel", quietly = TRUE))
            p <- p + ggrepel::geom_text_repel(data = pts_sf, aes(label = rotulo, geometry = geometry),
                        stat = "sf_coordinates", size = 3, color = "grey20",
                        min.segment.length = 0, segment.color = "grey60", max.overlaps = 50)
          else
            p <- p + geom_sf_text(data = pts_sf, aes(label = rotulo), size = 3, color = "grey20", nudge_y = my * 0.15)
        }
        p <- p + coord_sf(xlim = xlim, ylim = ylim, expand = FALSE,
                          datum = if (grade) sf::st_crs(4326) else NA)
      } else {
        if (has_size && has_color) p <- p + geom_point(data = d, aes(lon, lat, size = tamanho, color = grupo, shape = grupo), alpha = 0.9)
        else if (has_size)         p <- p + geom_point(data = d, aes(lon, lat, size = tamanho), color = "#0F3B5F", alpha = 0.9)
        else if (has_color)        p <- p + geom_point(data = d, aes(lon, lat, color = grupo, shape = grupo), size = 3.2, alpha = 0.95)
        else                       p <- p + geom_point(data = d, aes(lon, lat), color = "#0F3B5F", size = 3.2, alpha = 0.95)
        if (has_label) {
          if (requireNamespace("ggrepel", quietly = TRUE))
            p <- p + ggrepel::geom_text_repel(data = d, aes(lon, lat, label = rotulo), size = 3,
                        color = "grey20", min.segment.length = 0, segment.color = "grey60", max.overlaps = 50)
          else
            p <- p + geom_text(data = d, aes(lon, lat, label = rotulo), size = 3, color = "grey20", vjust = -0.8)
        }
        p <- p + coord_fixed(xlim = xlim, ylim = ylim)
      }

      if (has_size) {
        size_name <- if (modo == "bolhas" && !is.null(input$size_label) && nzchar(input$size_label))
          input$size_label else humaniza_rotulo(input$size_var)
        p <- p + scale_size_area(max_size = 11, name = size_name)   # área proporcional ao valor
      }
      if (has_color) {
        nlv <- nlevels(d$grupo)
        if (input$palette == "viridis" && requireNamespace("viridisLite", quietly = TRUE))
          p <- p + scale_color_viridis_d(name = input$color_var)
        else
          p <- p + scale_color_manual(values = rep(ocean, length.out = nlv), name = input$color_var)
        p <- p + scale_shape_manual(values = rep(c(16, 17, 15, 18, 8, 7, 3, 4), length.out = nlv), name = input$color_var)
      }
      if (has_facet) p <- p + facet_wrap(~ faceta)

      if (isTRUE(input$show_scale) && requireNamespace("ggspatial", quietly = TRUE)) {
        p <- p +
          ggspatial::annotation_scale(location = "br", height = unit(0.2, "cm")) +
          ggspatial::annotation_north_arrow(location = "tr", width = unit(1, "cm"),
            height = unit(1, "cm"), style = ggspatial::north_arrow_fancy_orienteering())
      }

      titulo <- if (nzchar(input$custom_title)) input$custom_title else "Estações de coleta"
      subt <- if (!is.null(input$custom_subtitle) && nzchar(input$custom_subtitle)) input$custom_subtitle else NULL
      base_theme <- if (grade) theme_bw(base_size = 13) else theme_minimal(base_size = 13)
      p <- p +
        labs(title = titulo, subtitle = subt,
             x = if (grade) "Longitude" else NULL, y = if (grade) "Latitude" else NULL) +
        base_theme +
        theme(plot.title = element_text(face = "bold", color = "#0F3B5F", size = 15),
              plot.subtitle = element_text(color = "#495057", size = 11),
              legend.position = "right",
              panel.grid = if (grade) element_line(color = "grey90") else element_blank())

      # Mapa de localização (inset) — hall­mark de figura de TCC/artigo (canto sup. esq.)
      if (isTRUE(input$show_inset) && !has_facet && !is.null(estados_base()) &&
          requireNamespace("cowplot", quietly = TRUE)) {
        br <- estados_base()
        inset <- ggplot() +
          geom_sf(data = br, fill = "grey88", color = "white", linewidth = 0.1) +
          annotate("rect", xmin = xlim[1], xmax = xlim[2], ymin = ylim[1], ymax = ylim[2],
                   color = "#E76F51", fill = NA, linewidth = 0.7) +
          coord_sf(datum = NA) + theme_void() +
          theme(panel.background = element_rect(fill = "white", color = "grey70"))
        p <- cowplot::ggdraw() +
          cowplot::draw_plot(p) +
          cowplot::draw_plot(inset, x = 0.015, y = 0.66, width = 0.30, height = 0.30)
      }
      p
    })

    output$map_plot <- renderPlot({ make_map() })

    # Exportação do projeto (.zip)
    output$download_project_zip <- downloadHandler(
      filename = function() paste0("projeto_mapa_pontos_", format(Sys.Date(), "%Y-%m-%d"), ".zip"),
      content = function(file) {
        info <- import_info()
        proj <- paste0("projeto_mapa_pontos_", format(Sys.Date(), "%Y-%m-%d"))
        td <- tempdir(); pd <- file.path(td, proj)
        dir.create(pd, showWarnings = FALSE)
        dd <- file.path(pd, "dados"); sc <- file.path(pd, "scripts")
        dir.create(dd, showWarnings = FALSE); dir.create(sc, showWarnings = FALSE)

        d_exp <- active_data()
        utils::write.csv(d_exp, file.path(dd, "estacoes.csv"), row.names = FALSE)

        latc <- if (input$source == "exemplo") "latitude" else input$lat_col
        lonc <- if (input$source == "exemplo") "longitude" else input$lon_col
        code <- c(
          "# --- MAPA DE PONTOS/ESTAÇÕES (CatalyseR) ---",
          "# install.packages(c('sf','geobr','ggplot2','ggspatial'))",
          "library(sf); library(ggplot2)",
          "",
          "dados <- read.csv('dados/estacoes.csv', stringsAsFactors = FALSE)",
          sprintf("pts <- sf::st_as_sf(dados, coords = c('%s', '%s'), crs = 4326)", lonc, latc),
          "",
          "base <- geobr::read_state(code_state = 'all', year = 2020, showProgress = FALSE)",
          "base <- sf::st_transform(base, 4326)",
          "bb <- sf::st_bbox(pts)",
          "",
          "ggplot() +",
          "  geom_sf(data = base, fill = '#EAF0F2', color = 'white', linewidth = 0.2) +",
          sprintf("  geom_sf(data = pts, aes(size = %s, color = %s), alpha = 0.85) +",
                  if (!is.null(input$size_var) && input$size_var != 'none') input$size_var else "1",
                  if (!is.null(input$color_var) && input$color_var != 'none') input$color_var else "'#0F3B5F'"),
          "  coord_sf(xlim = c(bb['xmin'], bb['xmax']), ylim = c(bb['ymin'], bb['ymax'])) +",
          "  ggspatial::annotation_scale(location = 'br') +",
          "  theme_minimal() +",
          sprintf("  labs(title = '%s')", gsub("'", "", if (nzchar(input$custom_title)) input$custom_title else "Estações de coleta"))
        )
        writeLines(code, file.path(sc, "mapa_pontos.R"))
        writeLines(c("Version: 1.0", "RestoreWorkspace: Default", "SaveWorkspace: Default", "Encoding: UTF-8"),
                   file.path(pd, "projeto_analise.Rproj"))
        writeLines(c("PACOTE DE ESTUDO: MAPA DE PONTOS/ESTAÇÕES (CatalyseR)",
                     "- dados/estacoes.csv : coordenadas e variáveis.",
                     "- scripts/mapa_pontos.R : código sf + ggplot2 do mapa."),
                   file.path(pd, "README.txt"))
        owd <- getwd(); setwd(td); zip::zip(file, files = proj); setwd(owd)
      }
    )
  })
}
