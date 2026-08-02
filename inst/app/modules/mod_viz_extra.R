# Módulos de Visualização (menu "Visualizando Dados"):
#   - Gráfico de Dispersão (scatter)
#   - Gráfico de Linhas
#   - Gráfico de Barras
# Todos em ggplot2, paleta Ocean Gradient e estilo científico elegante.

# ---- Paleta e tema compartilhados ------------------------------------------
.ocean_pal <- c("#0F3B5F", "#2E7D8F", "#62B6B7", "#E89B3C", "#E76F51",
                "#8FBF9F", "#B5651D", "#9D8DF1", "#C44536", "#3D5A80")

viz_theme <- function(nome, legend_pos = "right") {
  base <- switch(nome,
                 "minimal" = theme_minimal(base_size = 14),
                 "classic" = theme_classic(base_size = 14),
                 "bw"      = theme_bw(base_size = 14),
                 "gray"    = theme_gray(base_size = 14),
                 "light"   = theme_light(base_size = 14),
                 theme_minimal(base_size = 14))
  base + theme(
    plot.title    = element_text(face = "bold", size = 16, color = "#0F3B5F"),
    plot.subtitle = element_text(size = 12, color = "#495057"),
    axis.title    = element_text(color = "#212529"),
    legend.position = legend_pos,
    legend.title  = element_text(face = "bold")
  )
}

viz_theme_select <- function(ns) {
  selectInput(ns("graph_theme"), "Tema do Gráfico:",
              choices = c("Mínimo" = "minimal", "Clássico" = "classic",
                          "Preto e Branco" = "bw", "Cinza" = "gray", "Light" = "light"),
              selected = "minimal")
}

viz_legend_select <- function(ns) {
  selectInput(ns("legend_pos"), "Posição da Legenda:",
              choices = c("Direita" = "right", "Abaixo" = "bottom",
                          "Esquerda" = "left", "Acima" = "top", "Ocultar" = "none"),
              selected = "right")
}

# Empacota um projeto R (.zip) com dados limpos + script ggplot2
viz_write_project <- function(file, prefix, data_rv, import_info, code_lines) {
  info <- import_info()
  proj <- paste0("projeto_", prefix, "_", format(Sys.Date(), "%Y-%m-%d"))
  td <- tempdir()
  pd <- file.path(td, proj)
  dir.create(pd, showWarnings = FALSE)
  dd <- file.path(pd, "dados")
  sc <- file.path(pd, "scripts")
  dir.create(dd, showWarnings = FALSE)
  dir.create(sc, showWarnings = FALSE)

  df_clean <- data_rv()
  if (is.null(df_clean)) return(invisible())
  save(df_clean, file = file.path(dd, "dados_limpos.rda"))
  write.csv(df_clean, file = file.path(dd, "dados_limpos.csv"), row.names = FALSE)
  ds_name <- if (info$source == "package") info$package_dataset else info$excel_sheet
  tryCatch(export_to_xlsx(df_clean, dataset_name = ds_name, file_path = file.path(dd, "dados_limpos.xlsx")),
           error = function(e) NULL)

  code_lines <- code_lines[!vapply(code_lines, is.null, logical(1))]
  writeLines(unlist(code_lines), file.path(sc, paste0(prefix, ".R")))
  writeLines(c("Version: 1.0", "RestoreWorkspace: Default", "SaveWorkspace: Default", "Encoding: UTF-8"),
             file.path(pd, "projeto_analise.Rproj"))
  writeLines(c(paste0("PACOTE DE ESTUDO: ", toupper(prefix), " (CatalyseR)"),
               "- projeto_analise.Rproj: duplo clique para abrir no RStudio.",
               "- dados/    : dados limpos (.rda, .csv, .xlsx).",
               paste0("- scripts/", prefix, ".R : código ggplot2 do gráfico.")),
             file.path(pd, "README.txt"))

  owd <- getwd()
  setwd(td)
  zip::zip(file, files = proj)
  setwd(owd)
}

# Prólogo comum dos scripts exportados
viz_code_head <- function() {
  c("# --- GRÁFICO REPRODUTÍVEL (CatalyseR) ---",
    "# install.packages('ggplot2')",
    "library(ggplot2)",
    "",
    "load('dados/dados_limpos.rda')",
    "dados <- df_clean",
    paste0("ocean <- c('", paste(.ocean_pal, collapse = "', '"), "')"),
    "")
}

# =============================================================================
# 1. GRÁFICO DE DISPERSÃO
# =============================================================================
mod_scatter_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(1, 1, 1),
      style = "grid-template-columns: 2.5fr 7fr 2.5fr !important;",
      div(
        card(
          card_header("Configuração da Dispersão"),
          card_body(
            style = "padding: 12px 15px;",
            selectInput(ns("var_x"), "Variável X (numérica):", choices = NULL),
            selectInput(ns("var_y"), "Variável Y (numérica):", choices = NULL),
            selectInput(ns("var_group"), "Agrupar por Cor (opcional):", choices = c("Nenhuma" = "none")),
            checkboxInput(ns("reg_line"), "Linha de tendência (regressão linear)", value = FALSE),
            conditionalPanel(
              condition = sprintf("input['%s']", ns("reg_line")),
              checkboxInput(ns("reg_se"), "Mostrar intervalo de confiança", value = TRUE)
            )
          )
        ),
        card(
          card_header("Pacote de Estudo R/Quarto"),
          card_body(
            style = "padding: 12px 15px;",
            downloadButton(ns("download_project_zip"), "Exportar Projeto R (.zip)", class = "btn-primary w-100"),
            helpText("Gera um pacote com o código ggplot2 do gráfico.", style = "margin-top: 10px; font-size: 0.85rem;")
          )
        )
      ),
      navset_card_tab(
        id = ns("active_tab"),
        title = "Gráfico de Dispersão",
        nav_panel(
          title = "Visualização", icon = icon("ellipsis"),
          card_body(style = "padding: 15px;", plotOutput(ns("plot"), height = "450px"))
        )
      ),
      card(
        card_header("Personalização Visual"),
        card_body(
          style = "padding: 12px 15px;",
          textInput(ns("custom_title"), "Título do Gráfico:", value = ""),
          textInput(ns("custom_label_x"), "Rótulo Eixo X:", value = ""),
          textInput(ns("custom_label_y"), "Rótulo Eixo Y:", value = ""),
          sliderInput(ns("pt_size"), "Tamanho dos pontos:", min = 1, max = 6, value = 2.6, step = 0.2),
          sliderInput(ns("pt_alpha"), "Transparência:", min = 0.2, max = 1, value = 0.8, step = 0.1),
          viz_theme_select(ns),
          viz_legend_select(ns)
        )
      )
    )
  )
}

mod_scatter_server <- function(id, data_rv, import_info) {
  moduleServer(id, function(input, output, session) {
    observe({
      df <- data_rv(); req(df)
      num_cols <- names(df)[sapply(df, is.numeric)]
      if (length(num_cols) == 0) num_cols <- names(df)
      cat_cols <- names(df)[sapply(df, function(c) is.factor(c) || is.character(c) || is.logical(c) || length(unique(c)) < 15)]
      updateSelectInput(session, "var_x", choices = num_cols, selected = num_cols[1])
      updateSelectInput(session, "var_y", choices = num_cols, selected = if (length(num_cols) > 1) num_cols[2] else num_cols[1])
      updateSelectInput(session, "var_group", choices = c("Nenhuma" = "none", cat_cols), selected = "none")
    })

    observeEvent(list(input$var_x, input$var_y), {
      req(input$var_x, input$var_y)
      updateTextInput(session, "custom_title", value = paste(input$var_y, "em função de", input$var_x))
      updateTextInput(session, "custom_label_x", value = input$var_x)
      updateTextInput(session, "custom_label_y", value = input$var_y)
    })

    make_plot <- reactive({
      df <- data_rv(); req(df, input$var_x, input$var_y)
      req(input$var_x %in% names(df), input$var_y %in% names(df))
      grp <- input$var_group
      has_grp <- !is.null(grp) && grp != "none" && grp %in% names(df)
      if (has_grp) df[[grp]] <- as.factor(df[[grp]])

      title_val <- if (nzchar(input$custom_title)) input$custom_title else paste(input$var_y, "vs", input$var_x)
      xlab <- if (nzchar(input$custom_label_x)) input$custom_label_x else input$var_x
      ylab <- if (nzchar(input$custom_label_y)) input$custom_label_y else input$var_y

      if (has_grp) {
        p <- ggplot(df, aes(x = .data[[input$var_x]], y = .data[[input$var_y]], color = .data[[grp]])) +
          geom_point(size = input$pt_size, alpha = input$pt_alpha)
      } else {
        p <- ggplot(df, aes(x = .data[[input$var_x]], y = .data[[input$var_y]])) +
          geom_point(size = input$pt_size, alpha = input$pt_alpha, color = "#0F3B5F")
      }
      if (isTRUE(input$reg_line)) {
        if (has_grp) {
          p <- p + geom_smooth(method = "lm", se = isTRUE(input$reg_se), linewidth = 1)
        } else {
          p <- p + geom_smooth(method = "lm", se = isTRUE(input$reg_se), color = "#E76F51", fill = "#E89B3C", linewidth = 1)
        }
      }
      if (has_grp) p <- p + scale_color_manual(values = .ocean_pal) + scale_fill_manual(values = .ocean_pal)
      p + viz_theme(input$graph_theme, input$legend_pos) +
        labs(title = title_val, x = xlab, y = ylab, color = if (has_grp) grp else NULL)
    })

    output$plot <- renderPlot({ make_plot() })

    output$download_project_zip <- downloadHandler(
      filename = function() paste0("projeto_dispersao_", format(Sys.Date(), "%Y-%m-%d"), ".zip"),
      content = function(file) {
        grp <- input$var_group
        has_grp <- !is.null(grp) && grp != "none"
        title_val <- if (nzchar(input$custom_title)) input$custom_title else paste(input$var_y, "vs", input$var_x)
        aes_str <- if (has_grp) sprintf("x = `%s`, y = `%s`, color = `%s`", input$var_x, input$var_y, grp)
                   else sprintf("x = `%s`, y = `%s`", input$var_x, input$var_y)
        code <- c(viz_code_head(),
          sprintf("p <- ggplot(dados, aes(%s)) +", aes_str),
          sprintf("  geom_point(size = %s, alpha = %s%s) +", input$pt_size, input$pt_alpha,
                  if (has_grp) "" else ", color = '#0F3B5F'"),
          if (isTRUE(input$reg_line)) sprintf("  geom_smooth(method = 'lm', se = %s) +", isTRUE(input$reg_se)) else NULL,
          if (has_grp) "  scale_color_manual(values = ocean) +" else NULL,
          "  theme_minimal(base_size = 14) +",
          sprintf("  labs(title = '%s', x = '%s', y = '%s')", gsub("'", "", title_val), input$var_x, input$var_y),
          "print(p)")
        viz_write_project(file, "dispersao", data_rv, import_info, code)
      }
    )
  })
}

# =============================================================================
# 2. GRÁFICO DE LINHAS
# =============================================================================
mod_lines_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(1, 1, 1),
      style = "grid-template-columns: 2.5fr 7fr 2.5fr !important;",
      div(
        card(
          card_header("Configuração das Linhas"),
          card_body(
            style = "padding: 12px 15px;",
            selectInput(ns("var_x"), "Variável X (eixo):", choices = NULL),
            selectInput(ns("var_y"), "Variável Y (numérica):", choices = NULL),
            selectInput(ns("var_group"), "Série por Cor (opcional):", choices = c("Nenhuma" = "none")),
            checkboxInput(ns("show_points"), "Marcar os pontos", value = TRUE),
            execucao_explicita_controles_ui(ns)
          )
        ),
        card(
          card_header("Pacote de Estudo R/Quarto"),
          card_body(
            style = "padding: 12px 15px;",
            execucao_explicita_downloads_ui(ns, tagList(
              downloadButton(ns("download_project_zip"), "Exportar Projeto R (.zip)", class = "btn-primary w-100"),
              helpText("Gera um pacote com o código ggplot2 do gráfico.", style = "margin-top: 10px; font-size: 0.85rem;")
            ))
          )
        )
      ),
      execucao_explicita_resultados_ui(ns, navset_card_tab(
        id = ns("active_tab"),
        title = "Gráfico de Linhas",
        nav_panel(
          title = "Visualização", icon = icon("chart-line"),
          card_body(
            style = "padding: 15px;",
            plotOutput(ns("plot"), height = "450px"),
            uiOutput(ns("observacoes_plotadas"))
          )
        )
      )),
      card(
        card_header("Personalização Visual"),
        card_body(
          style = "padding: 12px 15px;",
          textInput(ns("custom_title"), "Título do Gráfico:", value = ""),
          textInput(ns("custom_label_x"), "Rótulo Eixo X:", value = ""),
          textInput(ns("custom_label_y"), "Rótulo Eixo Y:", value = ""),
          sliderInput(ns("line_w"), "Espessura da linha:", min = 0.4, max = 3, value = 1, step = 0.2),
          viz_theme_select(ns),
          viz_legend_select(ns)
        )
      )
    )
  )
}

mod_lines_server <- function(id, data_rv, import_info) {
  moduleServer(id, function(input, output, session) {
    revisao_execucao <- execucao_revisao_dados(data_rv)
    gatilho_execucao <- reactiveVal(0L)

    observe({
      df <- data_rv(); req(df)
      num_cols <- names(df)[sapply(df, is.numeric)]
      all_cols <- names(df)
      cat_cols <- names(df)[sapply(df, function(c) is.factor(c) || is.character(c) || is.logical(c) || length(unique(c)) < 15)]
      if (length(num_cols) == 0) num_cols <- all_cols
      x_atual <- isolate(input$var_x)
      y_atual <- isolate(input$var_y)
      grupo_atual <- isolate(input$var_group %||% "none")
      if (is.null(x_atual) || !x_atual %in% all_cols) x_atual <- all_cols[1]
      if (is.null(y_atual) || !y_atual %in% num_cols) y_atual <- num_cols[1]
      if (!grupo_atual %in% c("none", cat_cols)) grupo_atual <- "none"
      updateSelectInput(session, "var_x", choices = all_cols, selected = x_atual)
      updateSelectInput(session, "var_y", choices = num_cols, selected = y_atual)
      updateSelectInput(session, "var_group", choices = c("Nenhuma" = "none", cat_cols), selected = grupo_atual)
    })

    assinatura_execucao <- reactive({
      req(input$var_x, input$var_y)
      execucao_assinatura(
        input,
        c("var_x", "var_y", "var_group", "show_points", "line_w",
          "graph_theme", "legend_pos", "custom_title", "custom_label_x",
          "custom_label_y"),
        revisao_execucao()
      )
    })

    observeEvent(list(input$var_x, input$var_y), {
      req(input$var_x, input$var_y)
      updateTextInput(session, "custom_title", value = paste(input$var_y, "ao longo de", input$var_x))
      updateTextInput(session, "custom_label_x", value = input$var_x)
      updateTextInput(session, "custom_label_y", value = input$var_y)
    })

    # Contagem honesta das observações do gráfico: o ggplot2 descartaria as
    # linhas incompletas com um aviso discreto. A ANOVA já informa exclusões;
    # o gráfico de linhas passa a fazer o mesmo.
    observacoes_grafico <- reactiveVal(NULL)

    make_plot <- eventReactive(gatilho_execucao(), {
      df <- data_rv(); req(df, input$var_x, input$var_y)
      req(input$var_x %in% names(df), input$var_y %in% names(df))
      grp <- input$var_group
      has_grp <- !is.null(grp) && grp != "none" && grp %in% names(df)

      colunas <- c(input$var_x, input$var_y, if (has_grp) grp)
      completos <- stats::complete.cases(df[unique(colunas)])
      observacoes_grafico(list(n = sum(completos), descartadas = sum(!completos)))
      if (!sum(completos))
        stop("Nenhuma observação tem os dois eixos preenchidos. Trate os dados faltantes antes de montar o gráfico.",
             call. = FALSE)
      df <- df[completos, , drop = FALSE]
      if (has_grp) df[[grp]] <- as.factor(df[[grp]])

      title_val <- if (nzchar(input$custom_title)) input$custom_title else paste(input$var_y, "x", input$var_x)
      xlab <- if (nzchar(input$custom_label_x)) input$custom_label_x else input$var_x
      ylab <- if (nzchar(input$custom_label_y)) input$custom_label_y else input$var_y

      if (has_grp) {
        p <- ggplot(df, aes(x = .data[[input$var_x]], y = .data[[input$var_y]],
                            color = .data[[grp]], group = .data[[grp]])) +
          geom_line(linewidth = input$line_w)
        if (isTRUE(input$show_points)) p <- p + geom_point(size = 2.4)
        p <- p + scale_color_manual(values = .ocean_pal)
      } else {
        p <- ggplot(df, aes(x = .data[[input$var_x]], y = .data[[input$var_y]], group = 1)) +
          geom_line(linewidth = input$line_w, color = "#0F3B5F")
        if (isTRUE(input$show_points)) p <- p + geom_point(size = 2.4, color = "#2E7D8F")
      }
      p + viz_theme(input$graph_theme, input$legend_pos) +
        labs(title = title_val, x = xlab, y = ylab, color = if (has_grp) grp else NULL)
    }, ignoreInit = FALSE)

    exec_ctrl <- execucao_explicita_server(
      input, output, session, assinatura_execucao, make_plot,
      nome_analise = "O gráfico de linhas",
      gatilho_rv = gatilho_execucao
    )

    output$plot <- renderPlot({ make_plot() })

    output$observacoes_plotadas <- renderUI({
      make_plot()
      info <- observacoes_grafico()
      if (is.null(info)) return(NULL)
      classe <- if (info$descartadas > 0) "alert-warning" else "alert-light border"
      div(
        class = paste("alert py-2 small mt-2 mb-0", classe),
        icon(if (info$descartadas > 0) "filter" else "circle-check"),
        sprintf(
          " %d observação(ões) plotada(s); %d descartada(s) por dados faltantes em %s ou %s.",
          info$n, info$descartadas, input$var_x %||% "X", input$var_y %||% "Y"
        )
      )
    })

    output$download_project_zip <- downloadHandler(
      filename = function() paste0("projeto_linhas_", format(Sys.Date(), "%Y-%m-%d"), ".zip"),
      content = function(file) {
        grp <- input$var_group
        has_grp <- !is.null(grp) && grp != "none"
        title_val <- if (nzchar(input$custom_title)) input$custom_title else paste(input$var_y, "x", input$var_x)
        aes_str <- if (has_grp) sprintf("x = `%s`, y = `%s`, color = `%s`, group = `%s`", input$var_x, input$var_y, grp, grp)
                   else sprintf("x = `%s`, y = `%s`, group = 1", input$var_x, input$var_y)
        code <- c(viz_code_head(),
          sprintf("p <- ggplot(dados, aes(%s)) +", aes_str),
          sprintf("  geom_line(linewidth = %s%s) +", input$line_w, if (has_grp) "" else ", color = '#0F3B5F'"),
          if (isTRUE(input$show_points)) "  geom_point(size = 2.4) +" else NULL,
          if (has_grp) "  scale_color_manual(values = ocean) +" else NULL,
          "  theme_minimal(base_size = 14) +",
          sprintf("  labs(title = '%s', x = '%s', y = '%s')", gsub("'", "", title_val), input$var_x, input$var_y),
          "print(p)")
        viz_write_project(file, "linhas", data_rv, import_info, code)
      }
    )

    estado_execucao <- reactive({
      req(exec_ctrl$atualizada())
      grafico <- make_plot()
      req(grafico, input$var_x, input$var_y)
      grupo <- input$var_group %||% "none"
      titulo <- if (nzchar(input$custom_title %||% "")) input$custom_title else
        paste(input$var_y, "ao longo de", input$var_x)
      list(
        analise_id = "lines",
        tipo = "grafico_linhas",
        titulo = titulo,
        parametros = list(
          x = input$var_x,
          y = input$var_y,
          grupo = grupo,
          mostrar_pontos = isTRUE(input$show_points),
          espessura_linha = input$line_w,
          tema = input$graph_theme,
          posicao_legenda = input$legend_pos,
          titulo_grafico = titulo,
          rotulo_x = input$custom_label_x,
          rotulo_y = input$custom_label_y
        ),
        saidas_disponiveis = c("grafico"),
        resultado_resumo = list(
          x = input$var_x, y = input$var_y, grupo = grupo,
          n = as.integer((observacoes_grafico()$n) %||% NA_integer_),
          descartadas = as.integer((observacoes_grafico()$descartadas) %||% NA_integer_)
        )
      )
    })

    invisible(list(
      estado_execucao = estado_execucao,
      estado_execucao_ui = exec_ctrl$estado,
      execucao_atualizada = exec_ctrl$atualizada
    ))
  })
}

# =============================================================================
# 3. GRÁFICO DE BARRAS
# =============================================================================
mod_bar_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(1, 1, 1),
      style = "grid-template-columns: 2.5fr 7fr 2.5fr !important;",
      div(
        card(
          card_header("Configuração das Barras"),
          card_body(
            style = "padding: 12px 15px;",
            selectInput(ns("var_x"), "Variável Categórica (X):", choices = NULL),
            selectInput(ns("var_y"), "Valor (Y):", choices = c("Contagem de casos" = "none")),
            selectInput(ns("var_group"), "Subgrupo por Cor (opcional):", choices = c("Nenhuma" = "none")),
            selectInput(ns("position"), "Disposição das barras:",
                        choices = c("Lado a lado (dodge)" = "dodge",
                                    "Empilhadas (stack)" = "stack",
                                    "Proporção 100% (fill)" = "fill"),
                        selected = "dodge")
          )
        ),
        card(
          card_header("Pacote de Estudo R/Quarto"),
          card_body(
            style = "padding: 12px 15px;",
            downloadButton(ns("download_project_zip"), "Exportar Projeto R (.zip)", class = "btn-primary w-100"),
            helpText("Gera um pacote com o código ggplot2 do gráfico.", style = "margin-top: 10px; font-size: 0.85rem;")
          )
        )
      ),
      navset_card_tab(
        id = ns("active_tab"),
        title = "Gráfico de Barras",
        nav_panel(
          title = "Visualização", icon = icon("chart-column"),
          card_body(style = "padding: 15px;", plotOutput(ns("plot"), height = "450px"))
        )
      ),
      card(
        card_header("Personalização Visual"),
        card_body(
          style = "padding: 12px 15px;",
          textInput(ns("custom_title"), "Título do Gráfico:", value = ""),
          textInput(ns("custom_label_x"), "Rótulo Eixo X:", value = ""),
          textInput(ns("custom_label_y"), "Rótulo Eixo Y:", value = ""),
          viz_theme_select(ns),
          viz_legend_select(ns)
        )
      )
    )
  )
}

mod_bar_server <- function(id, data_rv, import_info) {
  moduleServer(id, function(input, output, session) {
    observe({
      df <- data_rv(); req(df)
      num_cols <- names(df)[sapply(df, is.numeric)]
      cat_cols <- names(df)[sapply(df, function(c) is.factor(c) || is.character(c) || is.logical(c) || length(unique(c)) < 15)]
      if (length(cat_cols) == 0) cat_cols <- names(df)
      updateSelectInput(session, "var_x", choices = cat_cols, selected = cat_cols[1])
      updateSelectInput(session, "var_y", choices = c("Contagem de casos" = "none", num_cols), selected = "none")
      updateSelectInput(session, "var_group", choices = c("Nenhuma" = "none", cat_cols), selected = "none")
    })

    observeEvent(input$var_x, {
      req(input$var_x)
      updateTextInput(session, "custom_title", value = paste("Distribuição por", input$var_x))
      updateTextInput(session, "custom_label_x", value = input$var_x)
    })

    make_plot <- reactive({
      df <- data_rv(); req(df, input$var_x, input$var_x %in% names(df))
      grp <- input$var_group
      has_grp <- !is.null(grp) && grp != "none" && grp %in% names(df)
      yvar <- input$var_y
      use_count <- is.null(yvar) || yvar == "none" || !(yvar %in% names(df))
      pos <- input$position

      df[[input$var_x]] <- as.factor(df[[input$var_x]])
      if (has_grp) df[[grp]] <- as.factor(df[[grp]])

      title_val <- if (nzchar(input$custom_title)) input$custom_title else paste("Distribuição por", input$var_x)
      xlab <- if (nzchar(input$custom_label_x)) input$custom_label_x else input$var_x
      ylab_default <- if (use_count) "Contagem" else paste("Soma de", yvar)
      ylab <- if (nzchar(input$custom_label_y)) input$custom_label_y else ylab_default

      if (use_count) {
        if (has_grp) {
          p <- ggplot(df, aes(x = .data[[input$var_x]], fill = .data[[grp]])) +
            geom_bar(position = pos, color = "white", linewidth = 0.3)
        } else {
          p <- ggplot(df, aes(x = .data[[input$var_x]])) +
            geom_bar(fill = "#0F3B5F", color = "white", linewidth = 0.3)
        }
      } else {
        if (has_grp) {
          agg <- aggregate(df[[yvar]], by = list(X = df[[input$var_x]], G = df[[grp]]), FUN = sum, na.rm = TRUE)
          names(agg) <- c("X", "G", "valor")
          p <- ggplot(agg, aes(x = X, y = valor, fill = G)) +
            geom_col(position = pos, color = "white", linewidth = 0.3)
        } else {
          agg <- aggregate(df[[yvar]], by = list(X = df[[input$var_x]]), FUN = sum, na.rm = TRUE)
          names(agg) <- c("X", "valor")
          p <- ggplot(agg, aes(x = X, y = valor)) +
            geom_col(fill = "#0F3B5F", color = "white", linewidth = 0.3)
        }
      }
      if (has_grp) p <- p + scale_fill_manual(values = .ocean_pal)
      p + viz_theme(input$graph_theme, input$legend_pos) +
        labs(title = title_val, x = xlab, y = ylab, fill = if (has_grp) grp else NULL)
    })

    output$plot <- renderPlot({ make_plot() })

    output$download_project_zip <- downloadHandler(
      filename = function() paste0("projeto_barras_", format(Sys.Date(), "%Y-%m-%d"), ".zip"),
      content = function(file) {
        grp <- input$var_group
        has_grp <- !is.null(grp) && grp != "none"
        yvar <- input$var_y
        use_count <- is.null(yvar) || yvar == "none"
        pos <- input$position
        title_val <- if (nzchar(input$custom_title)) input$custom_title else paste("Distribuição por", input$var_x)

        if (use_count) {
          aes_str <- if (has_grp) sprintf("x = `%s`, fill = `%s`", input$var_x, grp) else sprintf("x = `%s`", input$var_x)
          geom_line_code <- sprintf("  geom_bar(position = '%s'%s) +", pos, if (has_grp) "" else ", fill = '#0F3B5F'")
          prep <- NULL
        } else {
          if (has_grp) {
            prep <- c(sprintf("agg <- aggregate(dados[['%s']], by = list(X = dados[['%s']], G = dados[['%s']]), FUN = sum, na.rm = TRUE)", yvar, input$var_x, grp),
                      "names(agg) <- c('X', 'G', 'valor')")
            aes_str <- "x = X, y = valor, fill = G"
          } else {
            prep <- c(sprintf("agg <- aggregate(dados[['%s']], by = list(X = dados[['%s']]), FUN = sum, na.rm = TRUE)", yvar, input$var_x),
                      "names(agg) <- c('X', 'valor')")
            aes_str <- "x = X, y = valor"
          }
          geom_line_code <- sprintf("  geom_col(position = '%s'%s) +", pos, if (has_grp) "" else ", fill = '#0F3B5F'")
        }
        data_obj <- if (use_count) "dados" else "agg"
        code <- c(viz_code_head(), prep,
          sprintf("p <- ggplot(%s, aes(%s)) +", data_obj, aes_str),
          geom_line_code,
          if (has_grp) "  scale_fill_manual(values = ocean) +" else NULL,
          "  theme_minimal(base_size = 14) +",
          sprintf("  labs(title = '%s', x = '%s')", gsub("'", "", title_val), input$var_x),
          "print(p)")
        viz_write_project(file, "barras", data_rv, import_info, code)
      }
    )
  })
}
