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
            card(
              card_header("Relatório e Pacote de Estudo"),
              card_body(
                style = "padding: 12px 15px;",
                downloadButton(ns("download_report_docx"), "Baixar Relatório Word (.docx)", class = "btn-success w-100"),
                div(style = "margin-top: 8px;"),
                downloadButton(ns("download_project_zip"), "Exportar Projeto R (.zip)", class = "btn-primary w-100"),
                helpText("Gera os relatórios diretamente em DOCX ou exporta um projeto completo em Quarto.", style = "margin-top: 10px; margin-bottom: 0; font-size: 0.85rem;")
              )
            )
          )
        )
      ),
      
      # COLUNA 2: RESULTADOS (TABELA PRINCIPAL)
      navset_card_tab(
        title = "Tabela de Medidas Resumo",
        nav_panel(
          title = "Estatísticas de Resumo",
          icon = icon("table"),
          card_body(
            style = "padding: 10px 15px;",
            div(style = "margin-bottom: -20px;", DTOutput(ns("summary_table"), height = "auto"))
          )
        )
      ),
      
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
    
    # Atualiza as escolhas de variáveis com base nos dados importados
    observe({
      df <- data_rv()
      req(df)
      
      # Filtra variáveis numéricas
      num_cols <- names(df)[sapply(df, is.numeric)]
      updateSelectInput(session, "vars_selected", choices = num_cols, selected = num_cols[1:min(2, length(num_cols))])
      
      # Atualiza a variável de agrupamento (categórica ou caractere, ou numérica com poucos valores únicos)
      all_cols <- names(df)
      cat_cols <- all_cols[!sapply(df, is.numeric) | sapply(df, function(col) length(unique(col)) < 10)]
      updateSelectInput(session, "var_group", choices = c("Nenhuma" = "none", cat_cols), selected = "none")
    })
    
    # Calcula as medidas estatísticas de forma reativa
    descr_data <- reactive({
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
    })
    
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
    
    # Função auxiliar para customizar os parâmetros do QMD descritivo
    customize_descr_qmd_params <- function(qmd_path, vars_selected, grupo) {
      lines <- readLines(qmd_path, warn = FALSE)
      lines <- gsub('vars_selected: ".*"', sprintf('vars_selected: "%s"', vars_selected), lines)
      lines <- gsub('grupo: ".*"', sprintf('grupo: "%s"', grupo), lines)
      return(lines)
    }

    # Download do Relatório Word (.docx)
    output$download_report_docx <- downloadHandler(
      filename = function() {
        paste0("relatorio_estatistica_descritiva_", format(Sys.Date(), "%Y-%m-%d"), ".docx")
      },
      content = function(file) {
        req(data_rv())
        
        # Criar diretório temporário para compilação
        temp_dir <- tempdir()
        temp_qmd <- file.path(temp_dir, "relatorio_descritiva.qmd")
        temp_ref <- file.path(temp_dir, "custom-reference.docx")
        temp_func <- file.path(temp_dir, "funcoes_descritiva.R")
        temp_data <- file.path(temp_dir, "dados_limpos.rda")
        
        # Copiar arquivos de templates para o diretório temporário
        file.copy("templates/custom-reference.docx", temp_ref, overwrite = TRUE)
        file.copy("templates/funcoes_descritiva.R", temp_func, overwrite = TRUE)
        file.copy("templates/relatorio_descritiva.qmd", temp_qmd, overwrite = TRUE)
        
        # Salvar os dados limpos ativos
        df_clean <- data_rv()
        save(df_clean, file = temp_data)
        
        # Preparar variáveis selecionadas como string de parâmetros
        vars_str <- paste(input$vars_selected, collapse = ",")
        grupo_val <- input$var_group
        
        # Customizar e escrever o QMD
        custom_qmd_lines <- customize_descr_qmd_params(
          temp_qmd,
          vars_selected = vars_str,
          grupo = grupo_val
        )
        writeLines(custom_qmd_lines, temp_qmd)
        
        # Renderizar o relatório usando quarto CLI
        old_wd <- getwd()
        setwd(temp_dir)
        
        system2("quarto", args = c("render", "relatorio_descritiva.qmd", "--to", "docx"))
        
        setwd(old_wd)
        
        # Copiar o arquivo final gerado para a saída
        generated_docx <- file.path(temp_dir, "relatorio_descritiva.docx")
        if (file.exists(generated_docx)) {
          file.copy(generated_docx, file, overwrite = TRUE)
        } else {
          writeLines("Erro: Não foi possível renderizar o relatório .docx usando o Quarto CLI.", file)
        }
      }
    )

    # Download do zip
    output$download_project_zip <- downloadHandler(
      filename = function() {
        paste0("projeto_estatistica_descritiva_", format(Sys.Date(), "%Y-%m-%d"), ".zip")
      },
      content = function(file) {
        info <- import_info()
        proj_dir_name <- paste0("projeto_estatistica_descritiva_", format(Sys.Date(), "%Y-%m-%d"))
        temp_dir <- tempdir()
        proj_dir <- file.path(temp_dir, proj_dir_name)
        dir.create(proj_dir, showWarnings = FALSE)
        
        dir_dados <- file.path(proj_dir, "dados")
        dir_scripts <- file.path(proj_dir, "scripts")
        dir_relatorios <- file.path(proj_dir, "relatorios")
        
        dir.create(dir_dados, showWarnings = FALSE)
        dir.create(dir_scripts, showWarnings = FALSE)
        dir.create(dir_relatorios, showWarnings = FALSE)
        
        # 1. Salvar os dados limpos (RDA, CSV e XLSX)
        df_clean <- data_rv()
        req(df_clean)
        save(df_clean, file = file.path(dir_dados, "dados_limpos.rda"))
        write.csv(df_clean, file = file.path(dir_dados, "dados_limpos.csv"), row.names = FALSE)
        ds_name <- if (info$source == "package") info$package_dataset else info$excel_sheet
        export_to_xlsx(df_clean, dataset_name = ds_name, file_path = file.path(dir_dados, "dados_limpos.xlsx"))
        
        # 2. Gerar o script R
        vars_str <- paste(paste0("'", input$vars_selected, "'"), collapse = ", ")
        r_script_content <- c(
          "# --- SCRIPT DE ESTATÍSTICA DESCRITIVA (IDE_R) ---",
          "# Instalação de pacotes recomendados no RStudio:",
          "# install.packages(c('ggplot2', 'readxl', 'writexl'))",
          "",
          "# 1. CARREGAR OS DADOS LIMPOS",
          "# (Carrega o arquivo RDA que preserva fatores e formatação)",
          "load('dados/dados_limpos.rda')",
          "dados <- df_clean",
          "",
          "# Alternativa em formato aberto Excel (se preferir):",
          "# library(readxl)",
          "# dados <- as.data.frame(read_excel('dados/dados_limpos.xlsx', sheet = 'Dados'))",
          "",
          "# Alternativa em formato aberto CSV:",
          "# dados <- read.csv('dados/dados_limpos.csv', stringsAsFactors = TRUE, check.names = FALSE)",
          "",
          "# 2. SELEÇÃO DE VARIÁVEIS",
          sprintf("vars_selecionadas <- c(%s)", vars_str),
          "",
          "# 3. CÁLCULO DE ESTATÍSTICAS DESCRITIVAS",
          "obter_resumo <- function(x) {",
          "  c(N = sum(!is.na(x)),",
          "    NAs = sum(is.na(x)),",
          "    Media = mean(x, na.rm = TRUE),",
          "    Mediana = median(x, na.rm = TRUE),",
          "    Desvio_Padrao = sd(x, na.rm = TRUE),",
          "    Variancia = var(x, na.rm = TRUE),",
          "    Minimo = min(x, na.rm = TRUE),",
          "    Maximo = max(x, na.rm = TRUE),",
          "    Q25 = quantile(x, 0.25, na.rm = TRUE),",
          "    Q75 = quantile(x, 0.75, na.rm = TRUE))",
          "}",
          "",
          "cat('--- ESTATÍSTICAS GLOBAIS ---\\n')",
          "for (v in vars_selecionadas) {",
          "  cat('\\nVariável:', v, '\\n')",
          "  print(obter_resumo(dados[[v]]))",
          "}",
          ""
        )
        
        if (input$var_group != "none") {
          r_script_content <- c(r_script_content,
            "# 4. CÁLCULO DE ESTATÍSTICAS AGRUPADAS",
            sprintf("cat('\\n--- ESTATÍSTICAS AGRUPADAS POR %s ---\\n')", input$var_group),
            "for (v in vars_selecionadas) {",
            "  cat('\\nVariável:', v, '\\n')",
            sprintf("  print(tapply(dados[[v]], dados[['%s']], obter_resumo))", input$var_group),
            "}"
          )
        }
        
        writeLines(r_script_content, file.path(dir_scripts, "descrever.R"))
        
        # Copiar arquivos de templates
        file.copy("templates/custom-reference.docx", file.path(dir_relatorios, "custom-reference.docx"), overwrite = TRUE)
        file.copy("templates/funcoes_descritiva.R", file.path(dir_scripts, "funcoes_descritiva.R"), overwrite = TRUE)
        
        # Customizar e escrever o QMD
        vars_str_params <- paste(input$vars_selected, collapse = ",")
        custom_qmd_lines <- customize_descr_qmd_params(
          "templates/relatorio_descritiva.qmd",
          vars_selected = vars_str_params,
          grupo = input$var_group
        )
        writeLines(custom_qmd_lines, file.path(dir_relatorios, "relatorio_descritivo.qmd"))
        
        # 4. Criar arquivo de Projeto
        rproj_content <- c(
          "Version: 1.0",
          "RestoreWorkspace: Default",
          "SaveWorkspace: Default",
          "AlwaysSaveHistory: Default",
          "EnableCodeIndexing: Yes",
          "UseSpacesForTab: Yes",
          "NumSpacesForTab: 2",
          "Encoding: UTF-8"
        )
        writeLines(rproj_content, file.path(proj_dir, "projeto_analise.Rproj"))
        
        # 5. README.txt
        readme_content <- c(
          "===========================================================",
          " PACOTE DE ESTATÍSTICA DESCRITIVA (IDE_R CIENTÍFICA)",
          "===========================================================",
          "",
          "Estrutura do projeto:",
          "- projeto_analise.Rproj: Dê duplo clique para abrir no RStudio.",
          "- dados/               : Contém os dados limpos em .rda, .csv e .xlsx.",
          "- scripts/             : Contém scripts e funções de apoio.",
          "  - scripts/descrever.R: Script com o código de cálculo estatístico.",
          "  - scripts/funcoes_descritiva.R : Funções de formatação e relato.",
          "- relatorios/relatorio_descritivo.qmd: Relatório em Quarto para compilação.",
          "",
          "Instruções:",
          "1. Abra o arquivo 'projeto_analise.Rproj'.",
          "2. Abra 'scripts/descrever.R' e execute o código para verificar as estatísticas.",
          "3. Para gerar o relatório final, abra 'relatorios/relatorio_descritivo.qmd' e clique no botão 'Render' do RStudio."
        )
        writeLines(readme_content, file.path(proj_dir, "README.txt"))
        
        # 6. Compactar
        old_wd <- getwd()
        setwd(temp_dir)
        utils::zip(file, files = proj_dir_name)
        setwd(old_wd)
      }
    )
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
          card_header("Pacote de Estudo R/Quarto"),
          card_body(
            style = "padding: 12px 15px;",
            downloadButton(ns("download_project_zip"), "Exportar Projeto R (.zip)", class = "btn-primary w-100"),
            helpText("Gera um pacote Quarto estruturado contendo o gráfico de distribuição correspondente.", style = "margin-top: 10px; margin-bottom: 0; font-size: 0.85rem;")
          )
        )
      ),
      
      # COLUNA 2: RESULTADOS (GRÁFICO DO HISTOGRAMA)
      navset_card_tab(
        id = ns("active_tab"),
        title = "Gráfico do Histograma",
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
    
    # Download do zip do projeto para Histogramas
    output$download_project_zip <- downloadHandler(
      filename = function() {
        paste0("projeto_histograma_", format(Sys.Date(), "%Y-%m-%d"), ".zip")
      },
      content = function(file) {
        info <- import_info()
        proj_dir_name <- paste0("projeto_histograma_", format(Sys.Date(), "%Y-%m-%d"))
        temp_dir <- tempdir()
        proj_dir <- file.path(temp_dir, proj_dir_name)
        dir.create(proj_dir, showWarnings = FALSE)
        
        dir_dados <- file.path(proj_dir, "dados")
        dir_scripts <- file.path(proj_dir, "scripts")
        dir_relatorios <- file.path(proj_dir, "relatorios")
        
        dir.create(dir_dados, showWarnings = FALSE)
        dir.create(dir_scripts, showWarnings = FALSE)
        dir.create(dir_relatorios, showWarnings = FALSE)
        
        # 1. Salvar os dados (RDA, CSV e XLSX)
        df_clean <- data_rv()
        req(df_clean)
        save(df_clean, file = file.path(dir_dados, "dados_limpos.rda"))
        write.csv(df_clean, file = file.path(dir_dados, "dados_limpos.csv"), row.names = FALSE)
        ds_name <- if (info$source == "package") info$package_dataset else info$excel_sheet
        export_to_xlsx(df_clean, dataset_name = ds_name, file_path = file.path(dir_dados, "dados_limpos.xlsx"))
        
        # 2. Gerar o script R
        theme_code <- switch(input$graph_theme,
                             "minimal" = "theme_minimal(base_size = 14)",
                             "classic" = "theme_classic(base_size = 14)",
                             "bw"      = "theme_bw(base_size = 14)",
                             "gray"    = "theme_gray(base_size = 14)",
                             "light"   = "theme_light(base_size = 14)",
                             "theme_minimal(base_size = 14)")
        
        title_val <- if (nzchar(input$custom_title)) input$custom_title else paste("Distribuição de", input$var_x)
        x_label <- if (nzchar(input$custom_label_x)) input$custom_label_x else input$var_x
        y_label <- if (nzchar(input$custom_label_y)) input$custom_label_y else (if (input$show_density) "Densidade" else "Frequência")
        
        r_script_content <- c(
          "# --- SCRIPT DE HISTOGRAMA REPRODUTÍVEL (IDE_R) ---",
          "# Instalação de pacotes recomendados no RStudio:",
          "# install.packages(c('ggplot2', 'readxl', 'writexl'))",
          "library(ggplot2)",
          "",
          "# 1. CARREGAR OS DADOS LIMPOS",
          "# (Carrega o arquivo RDA que preserva fatores e formatação)",
          "load('dados/dados_limpos.rda')",
          "dados <- df_clean",
          "",
          "# Alternativa em formato aberto Excel (se preferir):",
          "# library(readxl)",
          "# dados <- as.data.frame(read_excel('dados/dados_limpos.xlsx', sheet = 'Dados'))",
          "",
          "# Alternativa em formato aberto CSV:",
          "# dados <- read.csv('dados/dados_limpos.csv', stringsAsFactors = TRUE, check.names = FALSE)",
          "",
          "# 2. GERAR O PLOT DO HISTOGRAMA"
        )
        
        if (input$var_group != "none") {
          r_script_content <- c(r_script_content,
            sprintf("dados$`%s` <- as.factor(dados$`%s`)", input$var_group, input$var_group),
            if (input$show_density) {
              c(
                sprintf("ggplot(dados, aes(x = `%s`, y = after_stat(density), fill = `%s`, color = `%s`)) +", input$var_x, input$var_group, input$var_group),
                sprintf("  geom_histogram(bins = %d, alpha = 0.5, position = 'identity') +", input$bins),
                "  geom_density(linewidth = 1, fill = NA) +"
              )
            } else {
              c(
                sprintf("ggplot(dados, aes(x = `%s`, fill = `%s`)) +", input$var_x, input$var_group),
                sprintf("  geom_histogram(bins = %d, alpha = 0.7, position = 'dodge') +", input$bins)
              )
            }
          )
        } else {
          if (input$show_density) {
            r_script_content <- c(r_script_content,
              sprintf("ggplot(dados, aes(x = `%s`, y = after_stat(density))) +", input$var_x),
              sprintf("  geom_histogram(bins = %d, fill = '#cfe2ff', color = '#0d6efd', alpha = 0.7) +", input$bins),
              "  geom_density(color = '#dc3545', linewidth = 1, fill = NA) +"
            )
          } else {
            r_script_content <- c(r_script_content,
              sprintf("ggplot(dados, aes(x = `%s`)) +", input$var_x),
              sprintf("  geom_histogram(bins = %d, fill = '#cfe2ff', color = '#0d6efd', alpha = 0.8) +", input$bins)
            )
          }
        }
        
        r_script_content <- c(r_script_content,
          sprintf("  %s +", theme_code),
          sprintf("  labs(title = '%s', x = '%s', y = '%s') +", title_val, x_label, y_label),
          "  theme(plot.title = element_text(face = 'bold'))"
        )
        
        writeLines(r_script_content, file.path(dir_scripts, "histograma.R"))
        
        # 3. Gerar o arquivo Quarto
        start_idx <- grep("^# 2\\. GERAR", r_script_content)
        if (length(start_idx) == 0) start_idx <- 8
        qmd_lines <- c(
          "---",
          "title: \"Análise de Frequências (Histograma)\"",
          "author: \"IDE CatalyseR - CatalyseR\"",
          "date: today",
          "format:",
          "  html:",
          "    theme: cosmo",
          "---",
          "",
          "## Histograma de Distribuição",
          "Abaixo está o gráfico gerado dinamicamente para avaliar a simetria e distribuição de frequência.",
          "",
          "```{r}",
          "#| label: setup",
          "#| include: false",
          "knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)",
          "library(ggplot2)",
          "",
          "# Carrega os dados limpos (preservando fatores e tipagem)",
          "load('../dados/dados_limpos.rda')",
          "dados <- df_clean",
          "",
          "# Alternativa em CSV:",
          "# library(readr)",
          "# dados <- read_csv('../dados/dados_limpos.csv')",
          "```",
          "",
          "```{r}",
          "#| label: grafico-histograma",
          paste(r_script_content[start_idx:length(r_script_content)], collapse = "\n"),
          "```"
        )
        
        writeLines(paste(qmd_lines, collapse = "\n"), file.path(dir_relatorios, "relatorio_histograma.qmd"))
        
        # 4. Criar projeto R
        rproj_content <- c(
          "Version: 1.0",
          "RestoreWorkspace: Default",
          "SaveWorkspace: Default",
          "AlwaysSaveHistory: Default",
          "EnableCodeIndexing: Yes",
          "UseSpacesForTab: Yes",
          "NumSpacesForTab: 2",
          "Encoding: UTF-8"
        )
        writeLines(rproj_content, file.path(proj_dir, "projeto_analise.Rproj"))
        
        # 5. README.txt
        readme_content <- c(
          "===========================================================",
          " PACOTE DE HISTOGRAMA REPRODUTÍVEL (IDE_R CIENTÍFICA)",
          "===========================================================",
          "",
          "Estrutura do projeto:",
          "- projeto_analise.Rproj: Dê duplo clique para abrir no RStudio.",
          "- dados/               : Contém os dados limpos em .rda, .csv e .xlsx.",
          "- scripts/histograma.R : Script contendo o código de visualização.",
          "- relatorios/relatorio_histograma.qmd: Relatório em Quarto para compilação."
        )
        writeLines(readme_content, file.path(proj_dir, "README.txt"))
        
        # 6. Compactar
        old_wd <- getwd()
        setwd(temp_dir)
        utils::zip(file, files = proj_dir_name)
        setwd(old_wd)
      }
    )
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
          card_header("Pacote de Estudo R/Quarto"),
          card_body(
            style = "padding: 12px 15px;",
            downloadButton(ns("download_project_zip"), "Exportar Projeto R (.zip)", class = "btn-primary w-100"),
            helpText("Gera um pacote Quarto estruturado contendo o gráfico Boxplot correspondente.", style = "margin-top: 10px; margin-bottom: 0; font-size: 0.85rem;")
          )
        )
      ),
      
      # COLUNA 2: RESULTADOS (GRÁFICO DO BOXPLOT)
      navset_card_tab(
        id = ns("active_tab"),
        title = "Gráfico do Boxplot",
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
    
    # Download do zip do projeto para Boxplot
    output$download_project_zip <- downloadHandler(
      filename = function() {
        paste0("projeto_boxplot_", format(Sys.Date(), "%Y-%m-%d"), ".zip")
      },
      content = function(file) {
        info <- import_info()
        proj_dir_name <- paste0("projeto_boxplot_", format(Sys.Date(), "%Y-%m-%d"))
        temp_dir <- tempdir()
        proj_dir <- file.path(temp_dir, proj_dir_name)
        dir.create(proj_dir, showWarnings = FALSE)
        
        dir_dados <- file.path(proj_dir, "dados")
        dir_scripts <- file.path(proj_dir, "scripts")
        dir_relatorios <- file.path(proj_dir, "relatorios")
        
        dir.create(dir_dados, showWarnings = FALSE)
        dir.create(dir_scripts, showWarnings = FALSE)
        dir.create(dir_relatorios, showWarnings = FALSE)
        
        # 1. Salvar dados (RDA, CSV e XLSX)
        df_clean <- data_rv()
        req(df_clean)
        save(df_clean, file = file.path(dir_dados, "dados_limpos.rda"))
        write.csv(df_clean, file = file.path(dir_dados, "dados_limpos.csv"), row.names = FALSE)
        ds_name <- if (info$source == "package") info$package_dataset else info$excel_sheet
        export_to_xlsx(df_clean, dataset_name = ds_name, file_path = file.path(dir_dados, "dados_limpos.xlsx"))
        
        # 2. Gerar o script R
        theme_code <- switch(input$graph_theme,
                             "minimal" = "theme_minimal(base_size = 14)",
                             "classic" = "theme_classic(base_size = 14)",
                             "bw"      = "theme_bw(base_size = 14)",
                             "gray"    = "theme_gray(base_size = 14)",
                             "light"   = "theme_light(base_size = 14)",
                             "theme_minimal(base_size = 14)")
        
        title_val <- if (nzchar(input$custom_title)) input$custom_title else paste("Boxplot de", input$var_y)
        x_label <- if (nzchar(input$custom_label_x)) input$custom_label_x else (if (input$var_x == "none") "" else input$var_x)
        y_label <- if (nzchar(input$custom_label_y)) input$custom_label_y else input$var_y
        
        r_script_content <- c(
          "# --- SCRIPT DE BOXPLOT REPRODUTÍVEL (IDE_R) ---",
          "# Instalação de pacotes recomendados no RStudio:",
          "# install.packages(c('ggplot2', 'readxl', 'writexl'))",
          "library(ggplot2)",
          "",
          "# 1. CARREGAR OS DADOS LIMPOS",
          "# (Carrega o arquivo RDA que preserva fatores e formatação)",
          "load('dados/dados_limpos.rda')",
          "dados <- df_clean",
          "",
          "# Alternativa em formato aberto Excel (se preferir):",
          "# library(readxl)",
          "# dados <- as.data.frame(read_excel('dados/dados_limpos.xlsx', sheet = 'Dados'))",
          "",
          "# Alternativa em formato aberto CSV:",
          "# dados <- read.csv('dados/dados_limpos.csv', stringsAsFactors = TRUE, check.names = FALSE)",
          "",
          "# 2. GERAR O PLOT DO BOXPLOT"
        )
        
        var_x_active <- input$var_x != "none"
        var_group_active <- input$var_group != "none"
        
        if (var_x_active) {
          r_script_content <- c(r_script_content, sprintf("dados$`%s` <- as.factor(dados$`%s`)", input$var_x, input$var_x))
        }
        if (var_group_active) {
          r_script_content <- c(r_script_content, sprintf("dados$`%s` <- as.factor(dados$`%s`)", input$var_group, input$var_group))
        }
        
        aes_parts <- c(sprintf("y = `%s`", input$var_y))
        
        if (var_x_active && var_group_active) {
          aes_parts <- c(aes_parts, sprintf("x = `%s`", input$var_x))
          if (input$grp_fill) aes_parts <- c(aes_parts, sprintf("fill = `%s`", input$var_group))
          if (input$grp_color) aes_parts <- c(aes_parts, sprintf("color = `%s`", input$var_group))
          aes_parts <- c(aes_parts, sprintf("group = interaction(`%s`, `%s`)", input$var_x, input$var_group))
          
          aes_str <- paste(aes_parts, collapse = ", ")
          r_script_content <- c(r_script_content,
            sprintf("ggplot(dados, aes(%s)) +", aes_str),
            "  geom_boxplot(alpha = 0.7, outlier.size = 2, position = position_dodge(0.8)) +"
          )
          if (input$show_points) {
            r_script_content <- c(r_script_content,
              "  geom_jitter(color = '#495057', alpha = 0.5, size = 1.8, position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8)) +"
            )
          }
        } else if (var_x_active && !var_group_active) {
          aes_parts <- c(aes_parts, sprintf("x = `%s`", input$var_x), sprintf("fill = `%s`", input$var_x))
          aes_str <- paste(aes_parts, collapse = ", ")
          r_script_content <- c(r_script_content,
            sprintf("ggplot(dados, aes(%s)) +", aes_str),
            "  geom_boxplot(alpha = 0.7, outlier.color = '#dc3545', outlier.size = 2) +"
          )
          if (input$show_points) {
            r_script_content <- c(r_script_content,
              "  geom_jitter(color = '#495057', width = 0.15, alpha = 0.5, size = 1.8) +"
            )
          }
        } else if (!var_x_active && var_group_active) {
          aes_parts <- c(aes_parts, sprintf("x = `%s`", input$var_group))
          if (input$grp_fill) aes_parts <- c(aes_parts, sprintf("fill = `%s`", input$var_group))
          if (input$grp_color) aes_parts <- c(aes_parts, sprintf("color = `%s`", input$var_group))
          aes_str <- paste(aes_parts, collapse = ", ")
          r_script_content <- c(r_script_content,
            sprintf("ggplot(dados, aes(%s)) +", aes_str),
            "  geom_boxplot(alpha = 0.7, outlier.size = 2) +"
          )
          if (input$show_points) {
            r_script_content <- c(r_script_content,
              "  geom_jitter(color = '#495057', width = 0.15, alpha = 0.5, size = 1.8) +"
            )
          }
        } else {
          aes_parts <- c(aes_parts, "x = ''")
          aes_str <- paste(aes_parts, collapse = ", ")
          r_script_content <- c(r_script_content,
            sprintf("ggplot(dados, aes(%s)) +", aes_str),
            "  geom_boxplot(fill = '#cfe2ff', color = '#0d6efd', alpha = 0.7, outlier.color = '#dc3545', outlier.size = 2) +"
          )
          if (input$show_points) {
            r_script_content <- c(r_script_content,
              "  geom_jitter(color = '#495057', width = 0.1, alpha = 0.5, size = 1.8) +"
            )
          }
        }
        
        r_script_content <- c(r_script_content,
          sprintf("  %s +", theme_code),
          sprintf("  labs(title = '%s', x = '%s', y = '%s') +", title_val, x_label, y_label),
          "  theme(plot.title = element_text(face = 'bold'))"
        )
        
        writeLines(r_script_content, file.path(dir_scripts, "boxplot.R"))
        
        # 3. Gerar o arquivo Quarto
        start_idx <- grep("^# 2\\. GERAR", r_script_content)
        if (length(start_idx) == 0) start_idx <- 8
        qmd_lines <- c(
          "---",
          "title: \"Diagrama de Caixa (Boxplot)\"",
          "author: \"IDE CatalyseR - CatalyseR\"",
          "date: today",
          "format:",
          "  html:",
          "    theme: cosmo",
          "---",
          "",
          "## Gráfico Boxplot",
          "Abaixo está o gráfico boxplot gerado dinamicamente para avaliação visual da dispersão, medianas e outliers.",
          "",
          "```{r}",
          "#| label: setup",
          "#| include: false",
          "knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)",
          "library(ggplot2)",
          "",
          "# Carrega os dados limpos (preservando fatores e tipagem)",
          "load('../dados/dados_limpos.rda')",
          "dados <- df_clean",
          "",
          "# Alternativa em CSV:",
          "# library(readr)",
          "# dados <- read_csv('../dados/dados_limpos.csv')",
          "```",
          "",
          "```{r}",
          "#| label: grafico-boxplot",
          paste(r_script_content[start_idx:length(r_script_content)], collapse = "\n"),
          "```"
        )
        
        writeLines(paste(qmd_lines, collapse = "\n"), file.path(dir_relatorios, "relatorio_boxplot.qmd"))
        
        # 4. Criar projeto R
        rproj_content <- c(
          "Version: 1.0",
          "RestoreWorkspace: Default",
          "SaveWorkspace: Default",
          "AlwaysSaveHistory: Default",
          "EnableCodeIndexing: Yes",
          "UseSpacesForTab: Yes",
          "NumSpacesForTab: 2",
          "Encoding: UTF-8"
        )
        writeLines(rproj_content, file.path(proj_dir, "projeto_analise.Rproj"))
        
        # 5. README.txt
        readme_content <- c(
          "===========================================================",
          " PACOTE DE BOXPLOT REPRODUTÍVEL (IDE_R CIENTÍFICA)",
          "===========================================================",
          "",
          "Estrutura do projeto:",
          "- projeto_analise.Rproj: Dê duplo clique para abrir no RStudio.",
          "- dados/               : Contém os dados limpos em .rda, .csv e .xlsx.",
          "- scripts/boxplot.R    : Script contendo o código de visualização.",
          "- relatorios/relatorio_boxplot.qmd: Relatório em Quarto para compilação."
        )
        writeLines(readme_content, file.path(proj_dir, "README.txt"))
        
        # 6. Compactar
        old_wd <- getwd()
        setwd(temp_dir)
        utils::zip(file, files = proj_dir_name)
        setwd(old_wd)
      }
    )
  })
}
