# Módulo de Tabela de Contingência para IDE_R (Preparando Dados)
library(shiny)
library(bslib)
library(ggplot2)
library(DT)

if (file.exists("templates/funcoes_contingency.R")) {
  source("templates/funcoes_contingency.R")
}

# Helper para customizar parâmetros do relatório Quarto de Contingência
customize_contingency_qmd_params <- function(qmd_path, var_row, var_col, pct_type) {
  lines <- readLines(qmd_path, warn = FALSE)
  lines <- gsub('var_row: ".*"', sprintf('var_row: "%s"', var_row), lines)
  lines <- gsub('var_col: ".*"', sprintf('var_col: "%s"', var_col), lines)
  lines <- gsub('pct_type: ".*"', sprintf('pct_type: "%s"', pct_type), lines)
  return(lines)
}

mod_contingency_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(1, 1, 1),
      style = "grid-template-columns: 2.5fr 7fr 2.5fr !important;",
      
      # COLUNA 1: CONFIGURAÇÃO DA TABELA
      div(
        card(
          card_header("Configuração da Tabela"),
          card_body(
            style = "padding: 12px 15px;",
            selectInput(ns("var_row"), "Variável de Linha (Categorias de Linha):", choices = NULL),
            selectInput(ns("var_col"), "Variável de Coluna (Categorias de Coluna):", choices = NULL),
            radioButtons(ns("pct_type"), "Exibir Percentuais (%):",
                         choices = c("Apenas Contagens" = "none",
                                     "Percentual da Linha" = "row",
                                     "Percentual da Coluna" = "col",
                                     "Percentual do Total" = "total"),
                         selected = "none")
          )
        ),
        card(
          card_header("Relatório e Pacote de Estudo"),
          card_body(
            style = "padding: 12px 15px;",
            downloadButton(ns("download_report_docx"), "Baixar Relatório Word (.docx)", class = "btn-success w-100"),
            div(style = "margin-top: 8px;"),
            downloadButton(ns("download_project_zip"), "Exportar Projeto R (.zip)", class = "btn-primary w-100"),
            helpText("Gera tabelas de contingência cruzadas e pacotes de análise.", style = "margin-top: 10px; font-size: 0.85rem;")
          )
        )
      ),
      
      # COLUNA 2: ABAS DE RESULTADOS
      navset_card_tab(
        id = ns("active_tab"),
        title = "Tabela Cruzada & Associação",
        nav_panel(
          title = "Tabela de Contingência",
          icon = icon("table"),
          card_body(
            uiOutput(ns("table_ui"))
          )
        ),
        nav_panel(
          title = "Gráfico de Frequências",
          icon = icon("chart-bar"),
          card_body(
            plotOutput(ns("freq_plot"), height = "450px")
          )
        )
      ),
      
      # COLUNA 3: CONFIGURAÇÕES DE EXIBIÇÃO
      card(
        card_header("Configurações de Exibição"),
        card_body(
          conditionalPanel(
            condition = sprintf("input['%s'] == 'Gráfico de Frequências'", ns("active_tab")),
            selectInput(ns("bar_position"), "Posição das Barras:",
                        choices = c("Lado a Lado (Dodge)" = "dodge",
                                    "Empilhado (Stack)" = "stack"),
                        selected = "dodge"),
            helpText("O posicionamento 'Lado a Lado' facilita a comparação de frequência absoluta direta entre grupos.")
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] != 'Gráfico de Frequências'", ns("active_tab")),
            helpText("Os totais marginais e de canto inferior direito representam as contagens consolidadas das respectivas colunas e linhas.")
          )
        )
      )
    )
  )
}

mod_contingency_server <- function(id, data_rv, import_info) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Atualiza as variáveis de linha e coluna categóricas
    observe({
      df <- data_rv()
      req(df)
      
      # Seleciona colunas que são fatores, character, logical ou que tenham menos de 15 níveis únicos
      cat_cols <- names(df)[sapply(df, function(x) {
        is.factor(x) || is.character(x) || is.logical(x) || length(unique(x)) < 15
      })]
      
      # Se não houver o suficiente, usa todas
      if (length(cat_cols) < 2) {
        cat_cols <- names(df)
      }
      
      # Atualiza seletores de linha e coluna
      old_row <- input$var_row
      old_col <- input$var_col
      
      # Garantir escolhas válidas e não iguais por padrão
      sel_row <- if (old_row %in% cat_cols) old_row else cat_cols[1]
      sel_col <- if (old_col %in% cat_cols) old_col else (if(length(cat_cols) > 1) cat_cols[2] else cat_cols[1])
      
      updateSelectInput(session, "var_row", choices = cat_cols, selected = sel_row)
      updateSelectInput(session, "var_col", choices = cat_cols, selected = sel_col)
    })
    
    # Executa a contingência
    result_rv <- reactive({
      df <- data_rv()
      req(df, input$var_row, input$var_col)
      
      calcular_contingencia(
        df = df,
        var_row = input$var_row,
        var_col = input$var_col,
        pct_type = input$pct_type
      )
    })
    
    # Renderizar UI da Tabela de Contingência
    output$table_ui <- renderUI({
      r <- result_rv()
      req(r)
      
      tab_cruzada <- mostrar_contingencia(r)
      
      tagList(
        h6(sprintf("Cruzamento: %s (Linhas) vs %s (Colunas)", r$var_row, r$var_col), style = "font-weight: 700; color: #0f3b5f; margin-bottom: 12px;"),
        renderDT({
          datatable(
            tab_cruzada,
            options = list(dom = "t", ordering = FALSE, pageLength = 50),
            rownames = FALSE,
            class = "cell-border stripe"
          )
        }),
        hr(style = "margin: 15px 0;"),
        h6("Estatística de Associação (Qui-Quadrado)", style = "font-weight: 700; color: #0f3b5f; margin-bottom: 8px;"),
        div(
          class = "alert alert-light",
          style = "border-left: 4px solid #E76F51; background-color: #f8f9fa; color: #333333; font-size: 0.9rem; line-height: 1.5; padding: 12px 15px; margin-bottom: 0;",
          relatar_contingencia(r)
        )
      )
    })
    
    # Renderizar Gráfico de Barras de Frequências
    output$freq_plot <- renderPlot({
      r <- result_rv()
      req(r)
      
      df <- data_rv()
      req(df)
      
      # Limpar dados de NAs
      dados_plot <- df[!is.na(df[[r$var_row]]) & !is.na(df[[r$var_col]]), ]
      
      # Mapear cores
      ocean_cols <- c("#0F3B5F", "#62B6B7", "#E89B3C", "#E76F51", "#2E7D8F")
      levels_col <- length(unique(dados_plot[[r$var_col]]))
      fill_cols <- ocean_cols[1:min(levels_col, length(ocean_cols))]
      if(levels_col > length(ocean_cols)) {
        fill_cols <- c(fill_cols, rainbow(levels_col - length(ocean_cols)))
      }
      
      ggplot(dados_plot, aes(x = as.factor(.data[[r$var_row]]), fill = as.factor(.data[[r$var_col]]))) +
        geom_bar(position = input$bar_position, alpha = 0.85, color = "white", linewidth = 0.5) +
        scale_fill_manual(values = fill_cols) +
        theme_minimal(base_size = 14) +
        labs(
          title = paste("Distribuição de Frequências: ", r$var_col, "por", r$var_row),
          x = r$var_row,
          y = "Frequência Absoluta (Contagem)",
          fill = r$var_col
        ) +
        theme(
          plot.title = element_text(face = "bold", color = "#0F3B5F"),
          legend.position = "bottom"
        )
    })
    
    # Handlers de Download
    output$download_report_docx <- downloadHandler(
      filename = function() {
        paste0("relatorio_contingencia_", format(Sys.Date(), "%Y-%m-%d"), ".docx")
      },
      content = function(file) {
        req(data_rv())
        
        temp_dir <- tempdir()
        temp_qmd <- file.path(temp_dir, "relatorio_contingency.qmd")
        temp_ref <- file.path(temp_dir, "custom-reference.docx")
        temp_func <- file.path(temp_dir, "funcoes_contingency.R")
        temp_data <- file.path(temp_dir, "dados_limpos.rda")
        
        file.copy("templates/custom-reference.docx", temp_ref, overwrite = TRUE)
        file.copy("templates/funcoes_contingency.R", temp_func, overwrite = TRUE)
        file.copy("templates/relatorio_contingency.qmd", temp_qmd, overwrite = TRUE)
        
        df_clean <- data_rv()
        save(df_clean, file = temp_data)
        
        custom_qmd_lines <- customize_contingency_qmd_params(
          temp_qmd,
          var_row = input$var_row,
          var_col = input$var_col,
          pct_type = input$pct_type
        )
        writeLines(custom_qmd_lines, temp_qmd)
        
        old_wd <- getwd()
        setwd(temp_dir)
        system2("quarto", args = c("render", "relatorio_contingency.qmd", "--to", "docx"))
        setwd(old_wd)
        
        generated_docx <- file.path(temp_dir, "relatorio_contingency.docx")
        if (file.exists(generated_docx)) {
          file.copy(generated_docx, file, overwrite = TRUE)
        } else {
          writeLines("Erro ao compilar o Word.", file)
        }
      }
    )
    
    output$download_project_zip <- downloadHandler(
      filename = function() {
        paste0("projeto_contingencia_", format(Sys.Date(), "%Y-%m-%d"), ".zip")
      },
      content = function(file) {
        info <- import_info()
        proj_dir_name <- paste0("projeto_contingencia_", format(Sys.Date(), "%Y-%m-%d"))
        temp_dir <- tempdir()
        proj_dir <- file.path(temp_dir, proj_dir_name)
        
        dir.create(proj_dir, showWarnings = FALSE)
        dir_dados <- file.path(proj_dir, "dados")
        dir_scripts <- file.path(proj_dir, "scripts")
        dir_relatorios <- file.path(proj_dir, "relatorios")
        
        dir.create(dir_dados, showWarnings = FALSE)
        dir.create(dir_scripts, showWarnings = FALSE)
        dir.create(dir_relatorios, showWarnings = FALSE)
        
        df_clean <- data_rv()
        save(df_clean, file = file.path(dir_dados, "dados_limpos.rda"))
        write.csv(df_clean, file = file.path(dir_dados, "dados_limpos.csv"), row.names = FALSE)
        ds_name <- if (info$source == "package") info$package_dataset else info$excel_sheet
        export_to_xlsx(df_clean, dataset_name = ds_name, file_path = file.path(dir_dados, "dados_limpos.xlsx"))
        
        r_script_content <- c(
          "# --- SCRIPT DE TABELA DE CONTINGÊNCIA E TESTE QUI-QUADRADO ---",
          "# Instalação de pacotes recomendados no RStudio:",
          "# install.packages(c('ggplot2', 'readxl', 'writexl'))",
          "source('scripts/funcoes_contingency.R')",
          "",
          "# 1. CARREGAR OS DADOS LIMPOS",
          "load('dados/dados_limpos.rda')",
          "dados <- df_clean",
          "",
          "# 2. EXECUTAR CONTINGÊNCIA E MOSTRAR ESTATÍSTICAS",
          sprintf("var_row <- '%s'", input$var_row),
          sprintf("var_col <- '%s'", input$var_col),
          sprintf("r <- calcular_contingencia(dados, var_row, var_col, pct_type = '%s')", input$pct_type),
          "print(mostrar_contingencia(r))",
          "cat(relatar_contingencia(r))",
          "",
          "# 3. GERAR O PLOT DE FREQUÊNCIAS",
          "library(ggplot2)",
          "dados_plot <- dados[!is.na(dados[[var_row]]) & !is.na(dados[[var_col]]), ]",
          "ocean_cols <- c('#0F3B5F', '#62B6B7', '#E89B3C', '#E76F51', '#2E7D8F')",
          "levels_col <- length(unique(dados_plot[[var_col]]))",
          "fill_cols <- ocean_cols[1:min(levels_col, length(ocean_cols))]",
          "if(levels_col > length(ocean_cols)) { fill_cols <- c(fill_cols, rainbow(levels_col - length(ocean_cols))) }",
          "ggplot(dados_plot, aes(x = as.factor(.data[[var_row]]), fill = as.factor(.data[[var_col]]))) +",
          sprintf("  geom_bar(position = '%s', alpha = 0.85, color = 'white') +", input$bar_position),
          "  scale_fill_manual(values = fill_cols) +",
          "  theme_minimal() +",
          "  labs(title = 'Distribuição de Frequências Cruzadas', x = var_row, y = 'Frequência Absoluta', fill = var_col)"
        )
        
        writeLines(r_script_content, file.path(dir_scripts, "analise_contingencia.R"))
        file.copy("templates/custom-reference.docx", file.path(dir_relatorios, "custom-reference.docx"), overwrite = TRUE)
        file.copy("templates/funcoes_contingency.R", file.path(dir_scripts, "funcoes_contingency.R"), overwrite = TRUE)
        
        custom_qmd_lines <- customize_contingency_qmd_params(
          "templates/relatorio_contingency.qmd",
          var_row = input$var_row,
          var_col = input$var_col,
          pct_type = input$pct_type
        )
        writeLines(custom_qmd_lines, file.path(dir_relatorios, "relatorio_contingency.qmd"))
        
        rproj_content <- c("Version: 1.0", "RestoreWorkspace: Default", "SaveWorkspace: Default", "Encoding: UTF-8")
        writeLines(rproj_content, file.path(proj_dir, "projeto_analise.Rproj"))
        
        readme_content <- c(
          "PACOTE DE TABELA DE CONTINGÊNCIA (CROSS-TAB & ASSOCIATIONS)",
          "- projeto_analise.Rproj: Duplo clique para abrir no RStudio.",
          "- dados/               : Contém os dados limpos em .rda, .csv e .xlsx.",
          "- scripts/analise_contingencia.R : Script contendo o cálculo da contingência e gráfico."
        )
        writeLines(readme_content, file.path(proj_dir, "README.txt"))
        
        old_wd <- getwd()
        setwd(temp_dir)
        utils::zip(file, files = proj_dir_name)
        setwd(old_wd)
      }
    )
  })
}
