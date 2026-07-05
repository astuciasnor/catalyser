# Módulo de Mapas (menu "Mapas") — coropleto do Brasil por estado.
# Junta os dados carregados (por sigla de UF) aos limites estaduais do geobr
# e desenha com ggplot2 + geom_sf. Escala e rosa-dos-ventos via ggspatial.

# Siglas das 27 UFs (para detectar automaticamente a coluna de estado)
.UF_SIGLAS <- c("AC","AL","AP","AM","BA","CE","DF","ES","GO","MA","MT","MS",
                "MG","PA","PB","PR","PE","PI","RJ","RN","RS","RO","RR","SC",
                "SP","SE","TO")

# Deixa um nome de coluna mais "humano" para títulos e legendas (global, usado
# também pelos outros módulos de mapa, que são carregados depois deste).
humaniza_rotulo <- function(x) {
  if (is.null(x) || length(x) == 0 || !nzchar(x)) return("")
  mapa <- c(producao_t = "Produção (t)", producao = "Produção", cpue = "CPUE",
            abundancia = "Abundância", posicao = "Posição", ano = "Ano",
            biomassa = "Biomassa", latitude = "Latitude", longitude = "Longitude")
  key <- tolower(x)
  if (key %in% names(mapa)) return(unname(mapa[key]))
  y <- gsub("_", " ", x)
  paste0(toupper(substr(y, 1, 1)), substr(y, 2, nchar(y)))
}

mod_mapa_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(1, 1, 1),
      style = "grid-template-columns: 2.8fr 7fr 2.8fr !important;",

      # COLUNA 1: CONFIGURAÇÃO
      div(
        card(
          card_header("Configuração do Mapa"),
          card_body(
            style = "padding: 12px 15px;",
            helpText(HTML("Preencha em 3 passos:<br>1) coluna com a <b>sigla do estado</b> (UF: PR, SP…);<br>2) a <b>variável numérica</b> a mapear;<br>3) se houver vários anos, escolha um em <b>Filtrar por</b>."),
                     style = "font-size: 0.82rem; margin-bottom: 8px;"),
            selectInput(ns("var_uf"), "Coluna da sigla do estado (UF):", choices = NULL),
            selectInput(ns("var_value"), "Variável a mapear (numérica):", choices = NULL),
            selectInput(ns("filter_col"), "Filtrar por (opcional):", choices = c("Nenhum" = "none")),
            conditionalPanel(
              condition = sprintf("input['%s'] != 'none'", ns("filter_col")),
              selectInput(ns("filter_val"), "Valor do filtro:", choices = NULL)
            ),
            radioButtons(ns("map_mode"), "Escala de cores:",
                         choices = c("Classes (intervalos)" = "classes", "Contínua" = "continuo"),
                         selected = "classes"),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'classes'", ns("map_mode")),
              numericInput(ns("n_classes"), "Nº de classes:", value = 5, min = 3, max = 8, step = 1),
              checkboxInput(ns("isolar_outlier"), "Isolar outliers (faixa própria no topo)", value = TRUE)
            )
          )
        ),
        card(
          card_header("Pacote de Estudo R/Quarto"),
          card_body(
            style = "padding: 12px 15px;",
            downloadButton(ns("download_project_zip"), "Exportar Projeto R (.zip)", class = "btn-primary w-100"),
            helpText("Gera um pacote com o código geobr + ggplot2 do mapa.", style = "margin-top: 10px; font-size: 0.85rem;")
          )
        )
      ),

      # COLUNA 2: MAPA
      navset_card_tab(
        id = ns("active_tab"),
        title = "Mapa Coroplético do Brasil",
        nav_panel(
          title = "Mapa", icon = icon("map"),
          card_body(style = "padding: 15px;", plotOutput(ns("map_plot"), height = "560px"))
        )
      ),

      # COLUNA 3: PERSONALIZAÇÃO
      card(
        card_header("Personalização Visual"),
        card_body(
          style = "padding: 12px 15px;",
          textInput(ns("custom_title"), "Título:", value = ""),
          textInput(ns("custom_subtitle"), "Subtítulo:", value = ""),
          textInput(ns("legend_title"), "Título da legenda:", value = ""),
          selectInput(ns("palette"), "Paleta de cores:",
                      choices = c("Azuis" = "azuis", "Verdes" = "verdes", "Vermelhos" = "vermelhos",
                                  "Roxos" = "roxos", "Laranjas" = "laranjas", "Ocean (EAPA)" = "ocean"),
                      selected = "azuis"),
          checkboxInput(ns("show_scale"), "Barra de escala", value = TRUE),
          checkboxInput(ns("show_arrow"), "Rosa-dos-ventos (Norte)", value = TRUE),
          selectInput(ns("map_theme"), "Tema:",
                      choices = c("Limpo (void)" = "void", "Minimalista" = "minimal",
                                  "Preto e Branco" = "bw"), selected = "void")
        )
      )
    )
  )
}

mod_mapa_server <- function(id, data_rv, import_info) {
  moduleServer(id, function(input, output, session) {

    ocean_cols <- c("#EAF3F4", "#9AD1D4", "#62B6B7", "#2E7D8F", "#0F3B5F")

    # --- Atualiza os seletores -------------------------------------------
    observe({
      df <- data_rv(); req(df)
      num_cols <- names(df)[sapply(df, is.numeric)]
      all_cols <- names(df)

      # Detecta a coluna que contém siglas de UF
      is_uf <- vapply(df, function(col) {
        u <- toupper(trimws(as.character(unique(col[!is.na(col)]))))
        length(u) > 0 && mean(u %in% .UF_SIGLAS) > 0.5
      }, logical(1))
      uf_cands <- names(df)[is_uf]
      uf_sel <- if (length(uf_cands) > 0) uf_cands[1] else all_cols[1]

      # Pré-seleciona a variável de produção e o filtro de ano, se existirem
      val_guess <- num_cols[grepl("prod", tolower(num_cols))]
      val_sel <- if (length(val_guess)) val_guess[1] else num_cols[1]
      filtro_guess <- all_cols[grepl("^ano$|^year$", tolower(all_cols))]

      updateSelectInput(session, "var_uf", choices = all_cols, selected = uf_sel)
      updateSelectInput(session, "var_value", choices = num_cols, selected = val_sel)
      updateSelectInput(session, "filter_col", choices = c("Nenhum" = "none", all_cols),
                        selected = if (length(filtro_guess)) filtro_guess[1] else "none")
    })

    # Popula os valores do filtro conforme a coluna escolhida
    observeEvent(input$filter_col, {
      df <- data_rv(); req(df)
      if (is.null(input$filter_col) || input$filter_col == "none" || !(input$filter_col %in% names(df))) {
        updateSelectInput(session, "filter_val", choices = character(0))
        return()
      }
      vals <- sort(unique(as.character(df[[input$filter_col]])))
      # Se parecer ano, começa pelo mais recente
      sel <- if (all(grepl("^[0-9]{4}$", vals))) rev(vals)[1] else vals[1]
      updateSelectInput(session, "filter_val", choices = vals, selected = sel)
    })

    # Preenche título/subtítulo/legenda de forma informativa (variável + filtro)
    observeEvent(list(input$var_value, input$filter_col, input$filter_val, input$map_mode), {
      req(input$var_value)
      leg <- humaniza_rotulo(input$var_value)
      ano_txt <- if (!is.null(input$filter_col) && input$filter_col != "none" &&
                     !is.null(input$filter_val) && nzchar(input$filter_val)) paste0(" — ", input$filter_val) else ""
      updateTextInput(session, "custom_title", value = paste0(leg, " por estado", ano_txt))
      updateTextInput(session, "legend_title", value = leg)
      updateTextInput(session, "custom_subtitle",
                      value = if (identical(input$map_mode, "classes"))
                        "Cores representam classes da variável por UF"
                      else "Cor proporcional ao valor por UF")
    }, ignoreInit = FALSE)

    # --- Limites estaduais do geobr (baixados uma vez por sessão) ---------
    estados_sf <- reactive({
      validate(need(requireNamespace("geobr", quietly = TRUE) && requireNamespace("sf", quietly = TRUE),
                    "Para usar os mapas, instale os pacotes: install.packages(c('geobr','sf'))."))
      out <- tryCatch(geobr::read_state(code_state = "all", year = 2020, showProgress = FALSE),
                      error = function(e) e)
      validate(need(!inherits(out, "error"),
                    paste0("Não foi possível carregar os limites do geobr",
                           if (inherits(out, "error")) paste0(": ", conditionMessage(out))
                           else " (precisa de internet na 1ª vez).")))
      out
    })

    # --- Dados agregados por UF ------------------------------------------
    dados_uf <- reactive({
      df <- data_rv(); req(df, input$var_uf, input$var_value)
      req(input$var_uf %in% names(df), input$var_value %in% names(df))

      if (!is.null(input$filter_col) && input$filter_col != "none" &&
          input$filter_col %in% names(df) && !is.null(input$filter_val) && nzchar(input$filter_val)) {
        df <- df[as.character(df[[input$filter_col]]) == input$filter_val, , drop = FALSE]
      }
      uf <- toupper(trimws(as.character(df[[input$var_uf]])))
      val <- suppressWarnings(as.numeric(df[[input$var_value]]))
      ok <- !is.na(uf) & !is.na(val)
      validate(need(any(ok), "Sem dados válidos para mapear com esta seleção."))
      agg <- aggregate(val[ok], by = list(UF = uf[ok]), FUN = sum, na.rm = TRUE)
      names(agg) <- c("UF", "valor")
      agg
    })

    # Paleta escolhida
    get_cols <- function(n_disc = NULL) {
      if (input$palette == "ocean") {
        base <- ocean_cols
      } else {
        nm <- switch(input$palette, azuis = "Blues", verdes = "Greens", vermelhos = "Reds",
                     roxos = "Purples", laranjas = "Oranges", "Blues")
        base <- if (requireNamespace("RColorBrewer", quietly = TRUE)) {
          suppressWarnings(RColorBrewer::brewer.pal(9, nm))
        } else c("#EFF3FF", "#9ECAE1", "#4292C6", "#08519C")
      }
      if (is.null(n_disc)) base else grDevices::colorRampPalette(base)(n_disc)
    }

    map_theme_obj <- reactive({
      switch(input$map_theme,
             "minimal" = theme_minimal(base_size = 13),
             "bw"      = theme_bw(base_size = 13),
             theme_void(base_size = 13))
    })

    # --- Monta o mapa -----------------------------------------------------
    make_map <- reactive({
      estados <- estados_sf()
      d <- dados_uf()

      juntos <- merge(estados, d, by.x = "abbrev_state", by.y = "UF", all.x = TRUE)
      if (!inherits(juntos, "sf")) juntos <- sf::st_as_sf(juntos)

      title_val <- if (nzchar(input$custom_title)) input$custom_title else paste0(humaniza_rotulo(input$var_value), " por estado")
      subtitle_val <- if (nzchar(input$custom_subtitle)) input$custom_subtitle else NULL
      leg_title <- if (nzchar(input$legend_title)) input$legend_title else humaniza_rotulo(input$var_value)

      if (input$map_mode == "continuo") {
        p <- ggplot(juntos) +
          geom_sf(aes(fill = valor), color = "white", linewidth = 0.2) +
          scale_fill_gradientn(colours = get_cols(), na.value = "grey90")
      } else {
        n <- max(3, min(8, as.integer(input$n_classes %||% 5)))
        vals <- juntos$valor
        finite_vals <- vals[is.finite(vals)]

        # Quebras. Se pedido, isola outliers (regra do IQR) numa faixa própria
        # no topo — genérico: funciona para qualquer variável, não só produção.
        brks <- NULL
        if (isTRUE(input$isolar_outlier) && length(finite_vals) >= 5) {
          q <- stats::quantile(finite_vals, c(0.25, 0.75), na.rm = TRUE)
          cerca <- q[2] + 1.5 * (q[2] - q[1])                  # cerca superior do boxplot
          base_vals <- finite_vals[finite_vals <= cerca]
          if (any(finite_vals > cerca) && length(unique(base_vals)) >= (n - 1)) {
            inner <- unique(stats::quantile(base_vals, probs = seq(0, 1, length.out = n), na.rm = TRUE))
            brks <- unique(c(inner, max(finite_vals)))          # última faixa = outliers
          }
        }
        if (is.null(brks)) {
          brks <- unique(stats::quantile(finite_vals, probs = seq(0, 1, length.out = n + 1), na.rm = TRUE))
        }

        if (length(brks) < 3) {
          p <- ggplot(juntos) + geom_sf(aes(fill = valor), color = "white", linewidth = 0.2) +
            scale_fill_gradientn(colours = get_cols(), na.value = "grey90")
        } else {
          fmt_br <- function(x) format(round(x), big.mark = ".", scientific = FALSE, trim = TRUE)
          rotulos <- paste0(fmt_br(brks[-length(brks)]), " – ", fmt_br(brks[-1]))
          juntos$categoria <- cut(vals, breaks = brks, labels = rotulos, include.lowest = TRUE)
          p <- ggplot(juntos) +
            geom_sf(aes(fill = categoria), color = "white", linewidth = 0.2) +
            scale_fill_manual(values = get_cols(length(rotulos)), na.value = "grey90")
        }
      }

      if (isTRUE(input$show_scale) && requireNamespace("ggspatial", quietly = TRUE)) {
        p <- p + ggspatial::annotation_scale(location = "br", height = unit(0.2, "cm"))
      }
      if (isTRUE(input$show_arrow) && requireNamespace("ggspatial", quietly = TRUE)) {
        p <- p + ggspatial::annotation_north_arrow(location = "tr",
                    height = unit(1.2, "cm"), width = unit(1.2, "cm"),
                    style = ggspatial::north_arrow_fancy_orienteering())
      }

      p + map_theme_obj() +
        labs(title = title_val, subtitle = subtitle_val, fill = leg_title, x = NULL, y = NULL) +
        theme(plot.title = element_text(face = "bold", size = 16, color = "#0F3B5F"),
              legend.position = "right")
    })

    output$map_plot <- renderPlot({ make_map() })

    # --- Exportação do Projeto (.zip) ------------------------------------
    output$download_project_zip <- downloadHandler(
      filename = function() paste0("projeto_mapa_", format(Sys.Date(), "%Y-%m-%d"), ".zip"),
      content = function(file) {
        info <- import_info()
        proj <- paste0("projeto_mapa_", format(Sys.Date(), "%Y-%m-%d"))
        td <- tempdir(); pd <- file.path(td, proj)
        dir.create(pd, showWarnings = FALSE)
        dd <- file.path(pd, "dados"); sc <- file.path(pd, "scripts")
        dir.create(dd, showWarnings = FALSE); dir.create(sc, showWarnings = FALSE)

        df_clean <- data_rv(); req(df_clean)
        save(df_clean, file = file.path(dd, "dados_limpos.rda"))
        write.csv(df_clean, file = file.path(dd, "dados_limpos.csv"), row.names = FALSE)
        ds_name <- if (info$source == "package") info$package_dataset else info$excel_sheet
        tryCatch(export_to_xlsx(df_clean, dataset_name = ds_name, file_path = file.path(dd, "dados_limpos.xlsx")),
                 error = function(e) NULL)

        filtro_code <- if (!is.null(input$filter_col) && input$filter_col != "none")
          sprintf("dados <- dados[as.character(dados[['%s']]) == '%s', ]", input$filter_col, input$filter_val) else "# (sem filtro)"
        title_val <- if (nzchar(input$custom_title)) input$custom_title else "Mapa coroplético"

        code <- c(
          "# --- MAPA COROPLÉTICO DO BRASIL (CatalyseR) ---",
          "# install.packages(c('geobr','sf','ggplot2','ggspatial'))",
          "library(geobr); library(sf); library(ggplot2)",
          "",
          "load('dados/dados_limpos.rda'); dados <- df_clean",
          filtro_code,
          sprintf("dados[['%s']] <- toupper(trimws(as.character(dados[['%s']])))", input$var_uf, input$var_uf),
          "",
          "estados <- read_state(code_state = 'all', year = 2020, showProgress = FALSE)",
          sprintf("agg <- aggregate(dados[['%s']], by = list(UF = dados[['%s']]), FUN = sum, na.rm = TRUE)",
                  input$var_value, input$var_uf),
          "names(agg) <- c('UF', 'valor')",
          "juntos <- merge(estados, agg, by.x = 'abbrev_state', by.y = 'UF')",
          "",
          "p <- ggplot(juntos) +",
          "  geom_sf(aes(fill = valor), color = 'white', linewidth = 0.2) +",
          "  scale_fill_viridis_c(option = 'D') +",
          "  ggspatial::annotation_scale(location = 'br') +",
          "  ggspatial::annotation_north_arrow(location = 'tr', style = ggspatial::north_arrow_fancy_orienteering()) +",
          "  theme_void() +",
          sprintf("  labs(title = '%s', fill = '%s')", gsub("'", "", title_val), input$var_value),
          "print(p)"
        )
        writeLines(code, file.path(sc, "mapa.R"))
        writeLines(c("Version: 1.0", "RestoreWorkspace: Default", "SaveWorkspace: Default", "Encoding: UTF-8"),
                   file.path(pd, "projeto_analise.Rproj"))
        writeLines(c("PACOTE DE ESTUDO: MAPA COROPLÉTICO (CatalyseR)",
                     "- dados/    : dados limpos (.rda, .csv, .xlsx).",
                     "- scripts/mapa.R : código geobr + ggplot2 do mapa."),
                   file.path(pd, "README.txt"))
        owd <- getwd(); setwd(td); zip::zip(file, files = proj); setwd(owd)
      }
    )
  })
}

# Coalescência de nulos (caso não definida por outro módulo)
if (!exists("%||%")) {
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
}
