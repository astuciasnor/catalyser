# Módulo de Descrição de Dados para IDE_R (Estatística Descritiva, Histogramas e Boxplots)

# ==========================================
# 1. COMPONENTE: ESTATÍSTICA DESCRITIVA
# ==========================================

mod_descr_stats_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(1, 1, 1),
      style = "grid-template-columns: 2.5fr 7fr 2.5fr !important;",
      
      # COLUNA 1: CONFIGURAÇÃO DE VARIÁVEIS & EXPORTAÇÃO
      div(
        card(
          card_header("Seleção de Variáveis"),
          card_body(
            style = "padding: 12px 15px;",
            selectInput(ns("vars_selected"), "Variáveis Numéricas:", choices = NULL, multiple = TRUE),
            div(style = "margin-top: -8px;",
                selectInput(ns("var_group"), "Agrupar por (Categórica):", choices = c("Nenhuma" = "none"))),
            execucao_explicita_controles_ui(ns),
            card(
              card_header("Relatório e Projeto R"),
              card_body(
                style = "padding: 12px 15px;",
                div(
                  class = "alert alert-light border small mb-0",
                  icon("file-export"), " ",
                  "Baixe o Projeto R em ",
                  strong("Comunicação de Resultados"), ". Execute a análise, clique em ",
                  strong("Adicionar ao Projeto R"), " e escolha lá os componentes do relatório. ",
                  "No RStudio, abra o projeto e use Render para gerar o caderno HTML e o Word."
                )
              )
            )
          )
        )
      ),
      
      # COLUNA 2: RESULTADOS (TABELA PRINCIPAL)
      execucao_explicita_resultados_ui(ns, navset_card_tab(
        title = "Tabela de Medidas Resumo:",
        nav_panel(
          title = "Estatísticas de Resumo",
          icon = icon("table"),
          card_body(
            style = "padding: 10px 15px;",
            div(style = "margin-bottom: -20px;", DTOutput(ns("summary_table"), height = "auto"))
          )
        )
      )),
      
      # COLUNA 3: CONFIGURAÇÕES DE EXIBIÇÃO
      card(
        card_header("Métricas de Resumo"),
        card_body(
          style = "padding: 12px 15px;",
          helpText("Marque as medidas estatísticas que deseja incluir na tabela resumo:"),
          checkboxInput(ns("show_n"), "Tamanho Amostral (N)", value = TRUE),
          checkboxInput(ns("show_nas"), "Valores Faltantes (NAs)", value = TRUE),
          checkboxInput(ns("show_mean"), "Média", value = TRUE),
          checkboxInput(ns("show_median"), "Mediana", value = TRUE),
          checkboxInput(ns("show_sd"), "Desvio Padrão", value = TRUE),
          checkboxInput(ns("show_var"), "Variância", value = FALSE),
          checkboxInput(ns("show_minmax"), "Mínimo & Máximo", value = TRUE),
          checkboxInput(ns("show_quartiles"), "Quartis (Q25 & Q75)", value = TRUE)
        )
      )
    )
  )
}

mod_descr_stats_server <- function(id, data_rv, import_info) {
  moduleServer(id, function(input, output, session) {
    revisao_execucao <- execucao_revisao_dados(data_rv)
    gatilho_execucao <- reactiveVal(0L)
    
    # Atualiza as escolhas de variáveis com base nos dados importados
    observe({
      df <- data_rv()
      req(df)
      
      # Filtra variáveis numéricas
      num_cols <- names(df)[sapply(df, is.numeric)]
      atuais <- isolate(input$vars_selected)
      selecionadas <- intersect(atuais %||% character(), num_cols)
      if (!length(selecionadas)) selecionadas <- head(num_cols, 2)
      updateSelectInput(session, "vars_selected", choices = num_cols, selected = selecionadas)
      
      # Atualiza a variável de agrupamento (categórica ou caractere, ou numérica com poucos valores únicos)
      all_cols <- names(df)
      cat_cols <- all_cols[!sapply(df, is.numeric) | sapply(df, function(col) length(unique(col)) < 10)]
      grupo_atual <- isolate(input$var_group %||% "none")
      if (!grupo_atual %in% c("none", cat_cols)) grupo_atual <- "none"
      updateSelectInput(session, "var_group", choices = c("Nenhuma" = "none", cat_cols), selected = grupo_atual)
    })

    assinatura_execucao <- reactive({
      req(length(input$vars_selected) > 0)
      execucao_assinatura(
        input,
        c("vars_selected", "var_group", "show_n", "show_nas", "show_mean",
          "show_median", "show_sd", "show_var", "show_minmax", "show_quartiles"),
        revisao_execucao()
      )
    })

    # Calcula somente quando o usuário confirma a configuração.
    descr_data <- eventReactive(gatilho_execucao(), {
      df <- data_rv()
      req(df, input$vars_selected)
      
      stats_list <- list()
      
      for (var_name in input$vars_selected) {
        val <- df[[var_name]]
        
        if (input$var_group == "none") {
          stats_list[[var_name]] <- calculate_summary_stats(val, var_name, "Global")
        } else {
          groups <- as.factor(df[[input$var_group]])
          levels_gp <- levels(groups)
          for (lvl in levels_gp) {
            subset_val <- val[groups == lvl]
            stats_list[[paste0(var_name, "_", lvl)]] <- calculate_summary_stats(subset_val, var_name, lvl)
          }
        }
      }
      
      # Junta a lista de estatísticas em um data frame estruturado
      res_df <- do.call(rbind, stats_list)
      rownames(res_df) <- NULL
      res_df
    }, ignoreInit = FALSE)

    exec_ctrl <- execucao_explicita_server(
      input, output, session, assinatura_execucao, descr_data,
      nome_analise = "A estatística descritiva",
      gatilho_rv = gatilho_execucao
    )
    
    # Renderiza a tabela DT formatada
    output$summary_table <- renderDT({
      df_stats <- descr_data()
      req(df_stats)
      
      # Seleciona dinamicamente as colunas a serem exibidas
      cols_to_show <- c("Variável")
      if (input$var_group != "none") {
        cols_to_show <- c(cols_to_show, "Grupo")
      }
      
      if (input$show_n) cols_to_show <- c(cols_to_show, "N")
      if (input$show_nas) cols_to_show <- c(cols_to_show, "NAs")
      if (input$show_mean) cols_to_show <- c(cols_to_show, "Média")
      if (input$show_median) cols_to_show <- c(cols_to_show, "Mediana")
      if (input$show_sd) cols_to_show <- c(cols_to_show, "Desvio Padrão")
      if (input$show_var) cols_to_show <- c(cols_to_show, "Variância")
      if (input$show_minmax) cols_to_show <- c(cols_to_show, "Mínimo", "Máximo")
      if (input$show_quartiles) cols_to_show <- c(cols_to_show, "Q25 (1º Q)", "Q75 (3º Q)")
      
      df_filtered <- df_stats[, cols_to_show, drop = FALSE]
      
      # Arredonda colunas numéricas
      num_cols_idx <- sapply(df_filtered, is.numeric)
      # Exceto as colunas N e NAs
      num_cols_idx[names(df_filtered) %in% c("N", "NAs")] <- FALSE
      
      datatable(df_filtered, 
                options = list(pageLength = 10, dom = 't', scrollX = TRUE), 
                class = 'table table-striped table-bordered table-hover') %>%
        formatRound(columns = which(num_cols_idx), digits = 3)
    })
    

    estado_execucao <- reactive({
      req(exec_ctrl$atualizada())
      req(length(input$vars_selected) > 0)
      resumo <- descr_data()
      req(resumo)
      grupo <- input$var_group %||% "none"
      titulo <- paste0(
        "Estatística descritiva: ", paste(input$vars_selected, collapse = ", "),
        if (!identical(grupo, "none")) paste0(" por ", grupo) else ""
      )
      list(
        analise_id = "descr_stats",
        tipo = "estatistica_descritiva",
        titulo = titulo,
        parametros = list(
          variaveis = input$vars_selected,
          grupo = grupo,
          metricas = list(
            n = isTRUE(input$show_n), nas = isTRUE(input$show_nas),
            media = isTRUE(input$show_mean), mediana = isTRUE(input$show_median),
            desvio_padrao = isTRUE(input$show_sd), variancia = isTRUE(input$show_var),
            minimo_maximo = isTRUE(input$show_minmax), quartis = isTRUE(input$show_quartiles)
          )
        ),
        saidas_disponiveis = c("tabela"),
        resultado_resumo = list(linhas_tabela = nrow(resumo), colunas_tabela = ncol(resumo))
      )
    })

    invisible(list(
      estado_execucao = estado_execucao,
      estado_execucao_ui = exec_ctrl$estado,
      execucao_atualizada = exec_ctrl$atualizada
    ))
  })
}

# Função auxiliar para calcular estatísticas de resumo
calculate_summary_stats <- function(x, var_name, group_name) {
  x_clean <- x[!is.na(x)]
  
  if (length(x_clean) == 0) {
    return(data.frame(
      "Variável" = var_name,
      "Grupo" = group_name,
      "N" = length(x),
      "NAs" = sum(is.na(x)),
      "Média" = NA_real_,
      "Mediana" = NA_real_,
      "Desvio Padrão" = NA_real_,
      "Variância" = NA_real_,
      "Mínimo" = NA_real_,
      "Máximo" = NA_real_,
      "Q25 (1º Q)" = NA_real_,
      "Q75 (3º Q)" = NA_real_,
      check.names = FALSE
    ))
  }
  
  data.frame(
    "Variável" = var_name,
    "Grupo" = group_name,
    "N" = length(x),
    "NAs" = sum(is.na(x)),
    "Média" = mean(x, na.rm = TRUE),
    "Mediana" = median(x, na.rm = TRUE),
    "Desvio Padrão" = sd(x, na.rm = TRUE),
    "Variância" = var(x, na.rm = TRUE),
    "Mínimo" = min(x, na.rm = TRUE),
    "Máximo" = max(x, na.rm = TRUE),
    "Q25 (1º Q)" = as.numeric(quantile(x, 0.25, na.rm = TRUE)),
    "Q75 (3º Q)" = as.numeric(quantile(x, 0.75, na.rm = TRUE)),
    check.names = FALSE
  )
}

# ==========================================
# 2. COMPONENTE: HISTOGRAMAS
# ==========================================

mod_histogram_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(1, 1, 1),
      style = "grid-template-columns: 2.5fr 7fr 2.5fr !important;",
      
      # COLUNA 1: CONFIGURAÇÃO DE VARIÁVEIS
      div(
        card(
          card_header("Configuração do Histograma"),
          card_body(
            style = "padding: 12px 15px;",
            selectInput(ns("var_x"), "Variável Numérica (X):", choices = NULL),
            div(style = "margin-top: -8px;",
                selectInput(ns("var_group"), "Agrupar por Cor (Opcional):", choices = c("Nenhuma" = "none")))
          )
        ),
        card(
          card_header("Relatório e Projeto R"),
          card_body(
            style = "padding: 12px 15px;",
            div(
              class = "alert alert-light border small mb-0",
              icon("file-export"), " ",
              "Baixe o Projeto R em ",
              strong("Comunicação de Resultados"), ". Execute a análise, clique em ",
              strong("Adicionar ao Projeto R"), " e escolha lá os componentes do relatório. ",
              "No RStudio, abra o projeto e use Render para gerar o caderno HTML e o Word."
            )
          )
        )
      ),
      
      # COLUNA 2: RESULTADOS (GRÁFICO DO HISTOGRAMA)
      navset_card_tab(
        id = ns("active_tab"),
        title = "Gráfico do Histograma:",
        nav_panel(
          title = "Visualização do Histograma",
          icon = icon("chart-bar"),
          card_body(
            style = "padding: 15px;",
            plotOutput(ns("hist_plot"), height = "450px")
          )
        )
      ),
      
      # COLUNA 3: CONFIGURAÇÕES DE EXIBIÇÃO
      card(
        card_header("Personalização Visual"),
        card_body(
          style = "padding: 12px 15px;",
          textInput(ns("custom_title"), "Título do Gráfico:", value = ""),
          textInput(ns("custom_label_x"), "Rótulo Eixo X:", value = ""),
          textInput(ns("custom_label_y"), "Rótulo Eixo Y:", value = ""),
          sliderInput(ns("bins"), "Classes (Bins):", min = 5, max = 50, value = 15, step = 1),
          checkboxInput(ns("show_density"), "Exibir Densidade", value = FALSE),
          selectInput(ns("graph_theme"), "Tema do Gráfico:", 
                      choices = c("Mínimo" = "minimal", 
                                  "Clássico" = "classic", 
                                  "Preto e Branco" = "bw", 
                                  "Cinza" = "gray", 
                                  "Light" = "light"), 
                      selected = "minimal")
        )
      )
    )
  )
}

mod_histogram_server <- function(id, data_rv, import_info) {
  moduleServer(id, function(input, output, session) {
    
    # Atualiza as escolhas de variáveis
    observe({
      df <- data_rv()
      req(df)
      
      num_cols <- names(df)[sapply(df, is.numeric)]
      updateSelectInput(session, "var_x", choices = num_cols, selected = num_cols[1])
      
      all_cols <- names(df)
      cat_cols <- all_cols[!sapply(df, is.numeric) | sapply(df, function(col) length(unique(col)) < 10)]
      updateSelectInput(session, "var_group", choices = c("Nenhuma" = "none", cat_cols), selected = "none")
    })
    
    # Quando muda a variável X, limpa títulos customizados para recarregar o padrão correspondente
    observeEvent(input$var_x, {
      req(input$var_x)
      updateTextInput(session, "custom_title", value = paste("Distribuição de", input$var_x))
      updateTextInput(session, "custom_label_x", value = input$var_x)
      updateTextInput(session, "custom_label_y", value = if (input$show_density) "Densidade" else "Frequência")
    })
    
    # Atualiza o rótulo do eixo Y quando alterna densidade
    observeEvent(input$show_density, {
      updateTextInput(session, "custom_label_y", value = if (input$show_density) "Densidade" else "Frequência")
    })
    
    # Cria o gráfico de forma reativa
    make_plot <- reactive({
      df <- data_rv()
      req(df, input$var_x)
      
      title_val <- if (nzchar(input$custom_title)) input$custom_title else paste("Distribuição de", input$var_x)
      x_label <- if (nzchar(input$custom_label_x)) input$custom_label_x else input$var_x
      y_label <- if (nzchar(input$custom_label_y)) input$custom_label_y else (if (input$show_density) "Densidade" else "Frequência")
      
      g_theme <- switch(input$graph_theme,
                        "minimal" = theme_minimal(base_size = 14),
                        "classic" = theme_classic(base_size = 14),
                        "bw"      = theme_bw(base_size = 14),
                        "gray"    = theme_gray(base_size = 14),
                        "light"   = theme_light(base_size = 14),
                        theme_minimal(base_size = 14))
      
      g_theme <- g_theme + theme(plot.title = element_text(face = "bold", size = 16, color = "#212529"))
      
      if (input$var_group != "none") {
        df[[input$var_group]] <- as.factor(df[[input$var_group]])
        
        if (input$show_density) {
          p <- ggplot(df, aes(x = .data[[input$var_x]], y = after_stat(density), fill = .data[[input$var_group]], color = .data[[input$var_group]])) +
            geom_histogram(bins = input$bins, alpha = 0.5, position = "identity") +
            geom_density(linewidth = 1, fill = NA)
        } else {
          p <- ggplot(df, aes(x = .data[[input$var_x]], fill = .data[[input$var_group]])) +
            geom_histogram(bins = input$bins, alpha = 0.7, position = "dodge")
        }
      } else {
        if (input$show_density) {
          p <- ggplot(df, aes(x = .data[[input$var_x]], y = after_stat(density))) +
            geom_histogram(bins = input$bins, fill = "#cfe2ff", color = "#0d6efd", alpha = 0.7) +
            geom_density(color = "#dc3545", linewidth = 1, fill = NA)
        } else {
          p <- ggplot(df, aes(x = .data[[input$var_x]])) +
            geom_histogram(bins = input$bins, fill = "#cfe2ff", color = "#0d6efd", alpha = 0.8)
        }
      }
      
      p + g_theme + labs(title = title_val, x = x_label, y = y_label)
    })
    
    output$hist_plot <- renderPlot({
      make_plot()
    })
    
  })
}

# ==========================================
# 3. COMPONENTE: BOXPLOT
# ==========================================

mod_boxplot_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(1, 1, 1),
      style = "grid-template-columns: 2.5fr 7fr 2.5fr !important;",
      
      # COLUNA 1: CONFIGURAÇÃO DO BOXPLOT
      div(
        card(
          card_header("Configuração do Boxplot"),
          card_body(
            style = "padding: 12px 15px;",
            selectInput(ns("var_y"), "Variável Numérica (Y):", choices = NULL),
            div(style = "margin-top: -8px;",
                selectInput(ns("var_x"), "Variável Categórica (X - Opcional):", choices = c("Nenhuma" = "none")))
          )
        ),
        card(
          card_header("Relatório e Projeto R"),
          card_body(
            style = "padding: 12px 15px;",
            div(
              class = "alert alert-light border small mb-0",
              icon("file-export"), " ",
              "Baixe o Projeto R em ",
              strong("Comunicação de Resultados"), ". Execute a análise, clique em ",
              strong("Adicionar ao Projeto R"), " e escolha lá os componentes do relatório. ",
              "No RStudio, abra o projeto e use Render para gerar o caderno HTML e o Word."
            )
          )
        )
      ),
      
      # COLUNA 2: RESULTADOS (GRÁFICO DO BOXPLOT)
      navset_card_tab(
        id = ns("active_tab"),
        title = "Gráfico do Boxplot:",
        nav_panel(
          title = "Visualização do Boxplot",
          icon = icon("square-poll-vertical"),
          card_body(
            style = "padding: 15px;",
            plotOutput(ns("box_plot"), height = "450px")
          )
        )
      ),
      
      # COLUNA 3: CONFIGURAÇÕES DE EXIBIÇÃO
      card(
        card_header("Personalização Visual"),
        card_body(
          style = "padding: 12px 15px;",
          textInput(ns("custom_title"), "Título do Gráfico:", value = ""),
          textInput(ns("custom_label_x"), "Rótulo Eixo X:", value = ""),
          textInput(ns("custom_label_y"), "Rótulo Eixo Y:", value = ""),
          checkboxInput(ns("show_points"), "Exibir Observações (Jitter)", value = FALSE),
          selectInput(ns("var_group"), "Agrupar por (Cor/Preenchimento):", choices = c("Nenhuma" = "none")),
          conditionalPanel(
            condition = sprintf("input['%s'] != 'none'", ns("var_group")),
            div(style = "display: flex; gap: 15px; margin-top: -5px; margin-bottom: 10px;",
                checkboxInput(ns("grp_color"), "Mapear Cor", value = TRUE),
                checkboxInput(ns("grp_fill"), "Mapear Preenchimento", value = TRUE)
            )
          ),
          selectInput(ns("graph_theme"), "Tema do Gráfico:", 
                      choices = c("Mínimo" = "minimal", 
                                  "Clássico" = "classic", 
                                  "Preto e Branco" = "bw", 
                                  "Cinza" = "gray", 
                                  "Light" = "light"), 
                      selected = "minimal")
        )
      )
    )
  )
}

mod_boxplot_server <- function(id, data_rv, import_info) {
  moduleServer(id, function(input, output, session) {
    
    # Atualiza as escolhas de variáveis
    observe({
      df <- data_rv()
      req(df)
      
      num_cols <- names(df)[sapply(df, is.numeric)]
      updateSelectInput(session, "var_y", choices = num_cols, selected = num_cols[1])
      
      all_cols <- names(df)
      cat_cols <- all_cols[!sapply(df, is.numeric) | sapply(df, function(col) length(unique(col)) < 10)]
      updateSelectInput(session, "var_x", choices = c("Nenhuma" = "none", cat_cols), selected = "none")
      updateSelectInput(session, "var_group", choices = c("Nenhuma" = "none", cat_cols), selected = "none")
    })
    
    # Quando muda a variável, limpa os títulos customizados
    observeEvent(list(input$var_y, input$var_x, input$var_group), {
      req(input$var_y, input$var_x, input$var_group)
      t_suffix <- if (input$var_x == "none") "" else paste(" por", input$var_x)
      if (input$var_group != "none") {
        t_suffix <- paste0(t_suffix, if (input$var_x == "none") " agrupado por " else " e ", input$var_group)
      }
      updateTextInput(session, "custom_title", value = paste("Diagrama de Caixa (Boxplot) de", input$var_y, t_suffix))
      updateTextInput(session, "custom_label_x", value = if (input$var_x == "none") "" else input$var_x)
      updateTextInput(session, "custom_label_y", value = input$var_y)
    })
    
    # Renderiza o Boxplot reativamente
    make_plot <- reactive({
      df <- data_rv()
      req(df, input$var_y)
      
      title_val <- if (nzchar(input$custom_title)) input$custom_title else paste("Boxplot de", input$var_y)
      x_label <- if (nzchar(input$custom_label_x)) input$custom_label_x else (if (input$var_x == "none") "" else input$var_x)
      y_label <- if (nzchar(input$custom_label_y)) input$custom_label_y else input$var_y
      
      g_theme <- switch(input$graph_theme,
                        "minimal" = theme_minimal(base_size = 14),
                        "classic" = theme_classic(base_size = 14),
                        "bw"      = theme_bw(base_size = 14),
                        "gray"    = theme_gray(base_size = 14),
                        "light"   = theme_light(base_size = 14),
                        theme_minimal(base_size = 14))
      
      g_theme <- g_theme + theme(plot.title = element_text(face = "bold", size = 16, color = "#212529"))
      
      var_x_active <- input$var_x != "none"
      var_group_active <- input$var_group != "none"
      
      if (var_x_active) {
        df[[input$var_x]] <- as.factor(df[[input$var_x]])
      }
      if (var_group_active) {
        df[[input$var_group]] <- as.factor(df[[input$var_group]])
      }
      
      if (var_x_active && var_group_active) {
        if (input$grp_fill && input$grp_color) {
          p <- ggplot(df, aes(x = .data[[input$var_x]], y = .data[[input$var_y]], fill = .data[[input$var_group]], color = .data[[input$var_group]], group = interaction(.data[[input$var_x]], .data[[input$var_group]])))
        } else if (input$grp_fill) {
          p <- ggplot(df, aes(x = .data[[input$var_x]], y = .data[[input$var_y]], fill = .data[[input$var_group]], group = interaction(.data[[input$var_x]], .data[[input$var_group]])))
        } else if (input$grp_color) {
          p <- ggplot(df, aes(x = .data[[input$var_x]], y = .data[[input$var_y]], color = .data[[input$var_group]], group = interaction(.data[[input$var_x]], .data[[input$var_group]])))
        } else {
          p <- ggplot(df, aes(x = .data[[input$var_x]], y = .data[[input$var_y]], group = interaction(.data[[input$var_x]], .data[[input$var_group]])))
        }
        
        p <- p + geom_boxplot(alpha = 0.7, outlier.size = 2, position = position_dodge(0.8))
        
        if (input$show_points) {
          p <- p + geom_jitter(alpha = 0.5, size = 1.8, color = "#495057",
                               position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8))
        }
      } else if (var_x_active && !var_group_active) {
        p <- ggplot(df, aes(x = .data[[input$var_x]], y = .data[[input$var_y]], fill = .data[[input$var_x]])) +
          geom_boxplot(alpha = 0.7, outlier.color = "#dc3545", outlier.size = 2)
        
        if (input$show_points) {
          p <- p + geom_jitter(color = "#495057", width = 0.15, alpha = 0.5, size = 1.8)
        }
      } else if (!var_x_active && var_group_active) {
        if (input$grp_fill && input$grp_color) {
          p <- ggplot(df, aes(x = .data[[input$var_group]], y = .data[[input$var_y]], fill = .data[[input$var_group]], color = .data[[input$var_group]]))
        } else if (input$grp_fill) {
          p <- ggplot(df, aes(x = .data[[input$var_group]], y = .data[[input$var_y]], fill = .data[[input$var_group]]))
        } else if (input$grp_color) {
          p <- ggplot(df, aes(x = .data[[input$var_group]], y = .data[[input$var_y]], color = .data[[input$var_group]]))
        } else {
          p <- ggplot(df, aes(x = .data[[input$var_group]], y = .data[[input$var_y]]))
        }
        
        p <- p + geom_boxplot(alpha = 0.7, outlier.size = 2)
        
        if (input$show_points) {
          p <- p + geom_jitter(color = "#495057", width = 0.15, alpha = 0.5, size = 1.8)
        }
      } else {
        p <- ggplot(df, aes(x = "", y = .data[[input$var_y]])) +
          geom_boxplot(fill = "#cfe2ff", color = "#0d6efd", alpha = 0.7, outlier.color = "#dc3545", outlier.size = 2)
        
        if (input$show_points) {
          p <- p + geom_jitter(color = "#495057", width = 0.1, alpha = 0.5, size = 1.8)
        }
      }
      
      p + g_theme + labs(title = title_val, x = x_label, y = y_label)
    })
    
    output$box_plot <- renderPlot({
      make_plot()
    })
    
  })
}
