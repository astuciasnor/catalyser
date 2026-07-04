# Módulos de Amostragem para IDE_R (AAS, AEP, AS)
library(shiny)
library(bslib)
library(ggplot2)
library(DT)

if (file.exists("templates/funcoes_sampling.R")) {
  source("templates/funcoes_sampling.R")
}

# Helper para customizar parâmetros do relatório Quarto
customize_sampling_qmd_params <- function(qmd_path, sampling_type, var_y, var_strata, n, seed, label_y, label_strata) {
  lines <- readLines(qmd_path, warn = FALSE)
  lines <- gsub('sampling_type: ".*"', sprintf('sampling_type: "%s"', sampling_type), lines)
  lines <- gsub('var_y: ".*"', sprintf('var_y: "%s"', var_y), lines)
  lines <- gsub('var_strata: ".*"', sprintf('var_strata: "%s"', var_strata), lines)
  lines <- gsub('n: .*', sprintf('n: %s', n), lines)
  lines <- gsub('seed: .*', sprintf('seed: %s', seed), lines)
  lines <- gsub('label_y: ".*"', sprintf('label_y: "%s"', label_y), lines)
  lines <- gsub('label_strata: ".*"', sprintf('label_strata: "%s"', label_strata), lines)
  return(lines)
}

# =============================================================================
# 1. AMOSTRAGEM ALEATÓRIA SIMPLES (AAS)
# =============================================================================

mod_aas_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(1, 1, 1),
      style = "grid-template-columns: 2.5fr 7fr 2.5fr !important;",
      
      # COLUNA 1: CONFIGURAÇÕES
      div(
        card(
          card_header("Configurações da AAS"),
          card_body(
            style = "padding: 12px 15px;",
            selectInput(ns("var_y"), "Variável de Interesse (Y):", choices = NULL),
            numericInput(ns("sample_n"), "Tamanho da Amostra (n):", value = 10, min = 1),
            numericInput(ns("seed"), "Semente Aleatória:", value = 42, min = 1),
            actionButton(ns("btn_draw"), "Sortear Amostra", class = "btn-primary w-100")
          )
        )
      ),
      
      # COLUNA 2: RESULTADOS
      navset_card_tab(
        nav_panel(
          title = "Estimativas Populacionais",
          icon = icon("calculator"),
          card_body(
            uiOutput(ns("estimativas_ui"))
          )
        ),
        nav_panel(
          title = "Dados Sorteados",
          icon = icon("table"),
          card_body(
            DTOutput(ns("dados_table"))
          )
        ),
        nav_panel(
          title = "Visualização do Sorteio",
          icon = icon("chart-line"),
          card_body(
            plotOutput(ns("plot_sorteio"), height = "450px")
          )
        )
      ),
      
      # COLUNA 3: EXPORTAÇÃO
      div(
        card(
          card_header("Exportar Resultados"),
          card_body(
            style = "padding: 12px 15px;",
            downloadButton(ns("download_report_docx"), "Relatório Word (.docx)", class = "btn-success w-100"),
            div(style = "margin-top: 8px;"),
            downloadButton(ns("download_project_zip"), "Exportar Projeto R (.zip)", class = "btn-primary w-100"),
            helpText("Baixe o relatório detalhado ou o projeto estruturado para compilação local no RStudio.", style = "margin-top: 10px; font-size: 0.85rem;")
          )
        )
      )
    )
  )
}

mod_aas_server <- function(id, data_rv, import_info) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Atualiza variáveis
    observe({
      df <- data_rv()
      req(df)
      num_cols <- names(df)[sapply(df, is.numeric)]
      updateSelectInput(session, "var_y", choices = num_cols)
    })
    
    # Executa o cálculo canônico de forma reativa sob gatilho do botão
    result_rv <- eventReactive(input$btn_draw, {
      df <- data_rv()
      req(df, input$var_y, input$sample_n)
      calcular_aas(df, input$var_y, input$sample_n, input$seed)
    }, ignoreNULL = FALSE)
    
    # Renderiza estimativas
    output$estimativas_ui <- renderUI({
      r <- result_rv()
      req(r)
      
      tab <- mostrar_aas(r)
      relato <- relatar_aas(r, r$var_y)
      
      tagList(
        h6("Estimadores Populacionais da AAS", style = "font-family: 'Outfit'; font-weight: 700; color: #0F3B5F; margin-top: 5px;"),
        tableOutput(ns("estimativas_table")),
        hr(),
        h6("Relato Científico Automatizado", style = "font-family: 'Outfit'; font-weight: 700; color: #0F3B5F;"),
        div(class = "alert alert-secondary", style = "font-size: 0.9rem; line-height: 1.4;", relato)
      )
    })
    
    output$estimativas_table <- renderTable({
      r <- result_rv()
      req(r)
      mostrar_aas(r)
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
    
    # Renderiza tabela de dados sorteados
    output$dados_table <- renderDT({
      r <- result_rv()
      req(r)
      datatable(r$sample_data, options = list(pageLength = 10, scrollX = TRUE))
    })
    
    # Renderiza gráfico mostrando a amostra destacada na população
    output$plot_sorteio <- renderPlot({
      r <- result_rv()
      df <- data_rv()
      req(r, df)
      
      # Cria vetor indicador para coloração
      df_plot <- df
      df_plot$Status <- "Não Sorteado"
      df_plot$Status[r$indices] <- "Sorteado"
      df_plot$Indice <- 1:nrow(df_plot)
      
      ggplot(df_plot, aes(x = Indice, y = .data[[r$var_y]], color = Status, size = Status)) +
        geom_point(alpha = 0.8) +
        scale_color_manual(values = c("Não Sorteado" = "#62B6B7", "Sorteado" = "#E76F51")) +
        scale_size_manual(values = c("Não Sorteado" = 2, "Sorteado" = 4)) +
        theme_minimal(base_size = 14) +
        labs(title = "Visualização da Amostra Aleatória Simples",
             x = "Índice da Observação na População",
             y = r$var_y,
             color = "Status de Sorteio") +
        theme(plot.title = element_text(face = "bold", color = "#0F3B5F"),
              legend.position = "bottom")
    })
    
    # Handlers de Download
    output$download_report_docx <- downloadHandler(
      filename = function() {
        paste0("relatorio_amostragem_aas_", format(Sys.Date(), "%Y-%m-%d"), ".docx")
      },
      content = function(file) {
        req(data_rv())
        
        temp_dir <- tempdir()
        temp_qmd <- file.path(temp_dir, "relatorio_sampling.qmd")
        temp_ref <- file.path(temp_dir, "custom-reference.docx")
        temp_func <- file.path(temp_dir, "funcoes_sampling.R")
        temp_data <- file.path(temp_dir, "dados_limpos.rda")
        
        file.copy("templates/custom-reference.docx", temp_ref, overwrite = TRUE)
        file.copy("templates/funcoes_sampling.R", temp_func, overwrite = TRUE)
        file.copy("templates/relatorio_sampling.qmd", temp_qmd, overwrite = TRUE)
        
        df_clean <- data_rv()
        save(df_clean, file = temp_data)
        
        custom_qmd_lines <- customize_sampling_qmd_params(
          temp_qmd,
          sampling_type = "aas",
          var_y = input$var_y,
          var_strata = "NULL",
          n = input$sample_n,
          seed = input$seed,
          label_y = input$var_y,
          label_strata = "NULL"
        )
        writeLines(custom_qmd_lines, temp_qmd)
        
        old_wd <- getwd()
        setwd(temp_dir)
        system2("quarto", args = c("render", "relatorio_sampling.qmd", "--to", "docx"))
        setwd(old_wd)
        
        generated_docx <- file.path(temp_dir, "relatorio_sampling.docx")
        if (file.exists(generated_docx)) {
          file.copy(generated_docx, file, overwrite = TRUE)
        } else {
          writeLines("Erro ao compilar o Word.", file)
        }
      }
    )
    
    output$download_project_zip <- downloadHandler(
      filename = function() {
        paste0("projeto_amostragem_aas_", format(Sys.Date(), "%Y-%m-%d"), ".zip")
      },
      content = function(file) {
        info <- import_info()
        proj_dir_name <- paste0("projeto_amostragem_aas_", format(Sys.Date(), "%Y-%m-%d"))
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
          "# --- SCRIPT DE AMOSTRAGEM ALEATÓRIA SIMPLES (AAS) ---",
          "# Instalação de pacotes recomendados no RStudio:",
          "# install.packages(c('ggplot2', 'readxl', 'writexl'))",
          "library(ggplot2)",
          "source('scripts/funcoes_sampling.R')",
          "",
          "# 1. CARREGAR OS DADOS LIMPOS",
          "load('dados/dados_limpos.rda')",
          "dados <- df_clean",
          "",
          "# 2. CALCULAR ESTIMATIVAS DA AAS",
          sprintf("r <- calcular_aas(dados, var_y = '%s', n = %d, seed = %d)", 
                  input$var_y, input$sample_n, input$seed),
          "print(mostrar_aas(r))",
          "cat(relatar_aas(r))",
          "",
          "# 3. VISUALIZAÇÃO",
          "df_plot <- dados",
          "df_plot$Status <- 'Não Sorteado'",
          "df_plot$Status[r$indices] <- 'Sorteado'",
          "df_plot$Indice <- 1:nrow(df_plot)",
          sprintf("ggplot(df_plot, aes(x = Indice, y = `%s`, color = Status)) +", input$var_y),
          "  geom_point(size = 3) +",
          "  scale_color_manual(values = c('Não Sorteado' = '#62B6B7', 'Sorteado' = '#E76F51')) +",
          "  theme_minimal() +",
          "  labs(title = 'Amostra Sorteada via AAS')"
        )
        
        writeLines(r_script_content, file.path(dir_scripts, "amostrar_aas.R"))
        file.copy("templates/custom-reference.docx", file.path(dir_relatorios, "custom-reference.docx"), overwrite = TRUE)
        file.copy("templates/funcoes_sampling.R", file.path(dir_scripts, "funcoes_sampling.R"), overwrite = TRUE)
        
        custom_qmd_lines <- customize_sampling_qmd_params(
          "templates/relatorio_sampling.qmd",
          sampling_type = "aas",
          var_y = input$var_y,
          var_strata = "NULL",
          n = input$sample_n,
          seed = input$seed,
          label_y = input$var_y,
          label_strata = "NULL"
        )
        writeLines(custom_qmd_lines, file.path(dir_relatorios, "relatorio_sampling.qmd"))
        
        rproj_content <- c("Version: 1.0", "RestoreWorkspace: Default", "SaveWorkspace: Default", "Encoding: UTF-8")
        writeLines(rproj_content, file.path(proj_dir, "projeto_analise.Rproj"))
        
        readme_content <- c(
          "PACOTE DE AMOSTRAGEM ALEATÓRIA SIMPLES (AAS)",
          "- projeto_analise.Rproj: Duplo clique para abrir no RStudio.",
          "- dados/               : Contém os dados limpos em .rda, .csv e .xlsx.",
          "- scripts/amostrar_aas.R : Script contendo o sorteio e cálculo das estimativas."
        )
        writeLines(readme_content, file.path(proj_dir, "README.txt"))
        
        old_wd <- getwd()
        setwd(temp_dir)
        zip::zip(file, files = proj_dir_name)
        setwd(old_wd)
      }
    )
  })
}


# =============================================================================
# 2. AMOSTRAGEM ESTRATIFICADA PROPORCIONAL (AEP)
# =============================================================================

mod_aep_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(1, 1, 1),
      style = "grid-template-columns: 2.5fr 7fr 2.5fr !important;",
      
      # COLUNA 1: CONFIGURAÇÕES
      div(
        card(
          card_header("Configurações da AEP"),
          card_body(
            style = "padding: 12px 15px;",
            selectInput(ns("var_strata"), "Variável de Estratificação (Estratos):", choices = NULL),
            selectInput(ns("var_y"), "Variável de Interesse (Y):", choices = NULL),
            numericInput(ns("sample_n"), "Tamanho Amostral Total (n):", value = 15, min = 1),
            numericInput(ns("seed"), "Semente Aleatória:", value = 42, min = 1),
            actionButton(ns("btn_draw"), "Sortear Amostra", class = "btn-primary w-100")
          )
        )
      ),
      
      # COLUNA 2: RESULTADOS
      navset_card_tab(
        nav_panel(
          title = "Alocação e Estimativas",
          icon = icon("calculator"),
          card_body(
            uiOutput(ns("estimativas_ui"))
          )
        ),
        nav_panel(
          title = "Dados Sorteados",
          icon = icon("table"),
          card_body(
            DTOutput(ns("dados_table"))
          )
        ),
        nav_panel(
          title = "Distribuição nos Estratos",
          icon = icon("chart-simple"),
          card_body(
            plotOutput(ns("plot_sorteio"), height = "450px")
          )
        )
      ),
      
      # COLUNA 3: EXPORTAÇÃO
      div(
        card(
          card_header("Exportar Resultados"),
          card_body(
            style = "padding: 12px 15px;",
            downloadButton(ns("download_report_docx"), "Relatório Word (.docx)", class = "btn-success w-100"),
            div(style = "margin-top: 8px;"),
            downloadButton(ns("download_project_zip"), "Exportar Projeto R (.zip)", class = "btn-primary w-100"),
            helpText("Gere relatórios elegantes e projetos completos estruturados.", style = "margin-top: 10px; font-size: 0.85rem;")
          )
        )
      )
    )
  )
}

mod_aep_server <- function(id, data_rv, import_info) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Atualiza seletores de variáveis
    observe({
      df <- data_rv()
      req(df)
      cols <- names(df)
      num_cols <- cols[sapply(df, is.numeric)]
      
      # Estratos podem ser categóricos ou numéricos discretos
      cat_cols <- cols[!sapply(df, is.numeric) | sapply(df, function(x) length(unique(x)) < 15)]
      
      updateSelectInput(session, "var_y", choices = num_cols)
      updateSelectInput(session, "var_strata", choices = cat_cols)
    })
    
    # Executa cálculo reativo
    result_rv <- eventReactive(input$btn_draw, {
      df <- data_rv()
      req(df, input$var_y, input$var_strata, input$sample_n)
      calcular_aep(df, input$var_y, input$var_strata, input$sample_n, input$seed)
    }, ignoreNULL = FALSE)
    
    # Interface de estimativas
    output$estimativas_ui <- renderUI({
      r <- result_rv()
      req(r)
      relato <- relatar_aep(r, r$var_y, r$var_strata)
      
      tagList(
        h6("Amostragem Proporcional por Estrato", style = "font-family: 'Outfit'; font-weight: 700; color: #0F3B5F; margin-top: 5px;"),
        tableOutput(ns("alocacao_table")),
        hr(),
        h6("Estimativas Globais Estratificadas", style = "font-family: 'Outfit'; font-weight: 700; color: #0F3B5F;"),
        tableOutput(ns("estimativas_table")),
        hr(),
        h6("Relato Científico Automatizado", style = "font-family: 'Outfit'; font-weight: 700; color: #0F3B5F;"),
        div(class = "alert alert-secondary", style = "font-size: 0.9rem; line-height: 1.4;", relato)
      )
    })
    
    output$alocacao_table <- renderTable({
      r <- result_rv()
      req(r)
      r$allocation
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
    
    output$estimativas_table <- renderTable({
      r <- result_rv()
      req(r)
      mostrar_aep(r)
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
    
    # Dados sorteados
    output$dados_table <- renderDT({
      r <- result_rv()
      req(r)
      datatable(r$sample_data, options = list(pageLength = 10, scrollX = TRUE))
    })
    
    # Boxplots com pontos sorteados
    output$plot_sorteio <- renderPlot({
      r <- result_rv()
      df <- data_rv()
      req(r, df)
      
      # Dados completos e amostra sorteada
      df_all <- df
      df_all$Status <- "Não Sorteado"
      df_all$Status[match(rownames(r$sample_data), rownames(df_all))] <- "Sorteado"
      
      ggplot(df_all, aes(x = as.factor(.data[[r$var_strata]]), y = .data[[r$var_y]])) +
        geom_boxplot(fill = "#e0f2fe", color = "#0F3B5F", alpha = 0.5, outlier.shape = NA) +
        geom_jitter(aes(color = Status, size = Status), width = 0.15, alpha = 0.8) +
        scale_color_manual(values = c("Não Sorteado" = "#62B6B7", "Sorteado" = "#E76F51")) +
        scale_size_manual(values = c("Não Sorteado" = 1.8, "Sorteado" = 3.5)) +
        theme_minimal(base_size = 14) +
        labs(title = "Distribuição de Amostragem Estratificada",
             x = r$var_strata,
             y = r$var_y,
             color = "Status de Sorteio") +
        theme(plot.title = element_text(face = "bold", color = "#0F3B5F"),
              legend.position = "bottom")
    })
    
    # Relatórios e Zip
    output$download_report_docx <- downloadHandler(
      filename = function() {
        paste0("relatorio_amostragem_aep_", format(Sys.Date(), "%Y-%m-%d"), ".docx")
      },
      content = function(file) {
        req(data_rv())
        
        temp_dir <- tempdir()
        temp_qmd <- file.path(temp_dir, "relatorio_sampling.qmd")
        temp_ref <- file.path(temp_dir, "custom-reference.docx")
        temp_func <- file.path(temp_dir, "funcoes_sampling.R")
        temp_data <- file.path(temp_dir, "dados_limpos.rda")
        
        file.copy("templates/custom-reference.docx", temp_ref, overwrite = TRUE)
        file.copy("templates/funcoes_sampling.R", temp_func, overwrite = TRUE)
        file.copy("templates/relatorio_sampling.qmd", temp_qmd, overwrite = TRUE)
        
        df_clean <- data_rv()
        save(df_clean, file = temp_data)
        
        custom_qmd_lines <- customize_sampling_qmd_params(
          temp_qmd,
          sampling_type = "aep",
          var_y = input$var_y,
          var_strata = input$var_strata,
          n = input$sample_n,
          seed = input$seed,
          label_y = input$var_y,
          label_strata = input$var_strata
        )
        writeLines(custom_qmd_lines, temp_qmd)
        
        old_wd <- getwd()
        setwd(temp_dir)
        system2("quarto", args = c("render", "relatorio_sampling.qmd", "--to", "docx"))
        setwd(old_wd)
        
        generated_docx <- file.path(temp_dir, "relatorio_sampling.docx")
        if (file.exists(generated_docx)) {
          file.copy(generated_docx, file, overwrite = TRUE)
        } else {
          writeLines("Erro ao compilar o Word.", file)
        }
      }
    )
    
    output$download_project_zip <- downloadHandler(
      filename = function() {
        paste0("projeto_amostragem_aep_", format(Sys.Date(), "%Y-%m-%d"), ".zip")
      },
      content = function(file) {
        info <- import_info()
        proj_dir_name <- paste0("projeto_amostragem_aep_", format(Sys.Date(), "%Y-%m-%d"))
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
          "# --- SCRIPT DE AMOSTRAGEM ESTRATIFICADA PROPORCIONAL (AEP) ---",
          "# Instalação de pacotes recomendados no RStudio:",
          "# install.packages(c('ggplot2', 'readxl', 'writexl'))",
          "library(ggplot2)",
          "source('scripts/funcoes_sampling.R')",
          "",
          "# 1. CARREGAR OS DADOS LIMPOS",
          "load('dados/dados_limpos.rda')",
          "dados <- df_clean",
          "",
          "# 2. CALCULAR ESTIMATIVAS ESTRATIFICADAS DA AEP",
          sprintf("r <- calcular_aep(dados, var_y = '%s', var_strata = '%s', n = %d, seed = %d)", 
                  input$var_y, input$var_strata, input$sample_n, input$seed),
          "print(r$allocation)",
          "print(mostrar_aep(r))",
          "cat(relatar_aep(r))",
          "",
          "# 3. VISUALIZAÇÃO",
          "df_plot <- dados",
          "df_plot$Status <- 'Não Sorteado'",
          "df_plot$Status[match(rownames(r$sample_data), rownames(df_plot))] <- 'Sorteado'",
          sprintf("ggplot(df_plot, aes(x = as.factor(`%s`), y = `%s`)) +", input$var_strata, input$var_y),
          "  geom_boxplot(fill = '#e0f2fe', color = '#0F3B5F', alpha = 0.5, outlier.shape = NA) +",
          "  geom_jitter(aes(color = Status), width = 0.15, size = 2.5) +",
          "  scale_color_manual(values = c('Não Sorteado' = '#62B6B7', 'Sorteado' = '#E76F51')) +",
          "  theme_minimal() +",
          "  labs(title = 'Amostra Sorteada via AEP')"
        )
        
        writeLines(r_script_content, file.path(dir_scripts, "amostrar_aep.R"))
        file.copy("templates/custom-reference.docx", file.path(dir_relatorios, "custom-reference.docx"), overwrite = TRUE)
        file.copy("templates/funcoes_sampling.R", file.path(dir_scripts, "funcoes_sampling.R"), overwrite = TRUE)
        
        custom_qmd_lines <- customize_sampling_qmd_params(
          "templates/relatorio_sampling.qmd",
          sampling_type = "aep",
          var_y = input$var_y,
          var_strata = input$var_strata,
          n = input$sample_n,
          seed = input$seed,
          label_y = input$var_y,
          label_strata = input$var_strata
        )
        writeLines(custom_qmd_lines, file.path(dir_relatorios, "relatorio_sampling.qmd"))
        
        rproj_content <- c("Version: 1.0", "RestoreWorkspace: Default", "SaveWorkspace: Default", "Encoding: UTF-8")
        writeLines(rproj_content, file.path(proj_dir, "projeto_analise.Rproj"))
        
        readme_content <- c(
          "PACOTE DE AMOSTRAGEM ESTRATIFICADA PROPORCIONAL (AEP)",
          "- projeto_analise.Rproj: Duplo clique para abrir no RStudio.",
          "- dados/               : Contém os dados limpos em .rda, .csv e .xlsx.",
          "- scripts/amostrar_aep.R : Script contendo o sorteio e estimadores da AEP."
        )
        writeLines(readme_content, file.path(proj_dir, "README.txt"))
        
        old_wd <- getwd()
        setwd(temp_dir)
        zip::zip(file, files = proj_dir_name)
        setwd(old_wd)
      }
    )
  })
}


# =============================================================================
# 3. AMOSTRAGEM SISTEMÁTICA (AS)
# =============================================================================

mod_as_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(1, 1, 1),
      style = "grid-template-columns: 2.5fr 7fr 2.5fr !important;",
      
      # COLUNA 1: CONFIGURAÇÕES
      div(
        card(
          card_header("Configurações da AS"),
          card_body(
            style = "padding: 12px 15px;",
            selectInput(ns("var_y"), "Variável de Interesse (Y):", choices = NULL),
            numericInput(ns("sample_n"), "Tamanho da Amostra (n):", value = 10, min = 1),
            numericInput(ns("seed"), "Semente Aleatória:", value = 42, min = 1),
            actionButton(ns("btn_draw"), "Sortear Amostra", class = "btn-primary w-100")
          )
        )
      ),
      
      # COLUNA 2: RESULTADOS
      navset_card_tab(
        nav_panel(
          title = "Estimativas Populacionais",
          icon = icon("calculator"),
          card_body(
            uiOutput(ns("estimativas_ui"))
          )
        ),
        nav_panel(
          title = "Dados Sorteados",
          icon = icon("table"),
          card_body(
            DTOutput(ns("dados_table"))
          )
        ),
        nav_panel(
          title = "Espaçamento Sistemático",
          icon = icon("chart-line"),
          card_body(
            plotOutput(ns("plot_sorteio"), height = "450px")
          )
        )
      ),
      
      # COLUNA 3: EXPORTAÇÃO
      div(
        card(
          card_header("Exportar Resultados"),
          card_body(
            style = "padding: 12px 15px;",
            downloadButton(ns("download_report_docx"), "Relatório Word (.docx)", class = "btn-success w-100"),
            div(style = "margin-top: 8px;"),
            downloadButton(ns("download_project_zip"), "Exportar Projeto R (.zip)", class = "btn-primary w-100"),
            helpText("Gere pacotes e relatórios completos da análise sistemática.", style = "margin-top: 10px; font-size: 0.85rem;")
          )
        )
      )
    )
  )
}

mod_as_server <- function(id, data_rv, import_info) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Atualiza variáveis
    observe({
      df <- data_rv()
      req(df)
      num_cols <- names(df)[sapply(df, is.numeric)]
      updateSelectInput(session, "var_y", choices = num_cols)
    })
    
    # Executa cálculo reativo
    result_rv <- eventReactive(input$btn_draw, {
      df <- data_rv()
      req(df, input$var_y, input$sample_n)
      calcular_as(df, input$var_y, input$sample_n, input$seed)
    }, ignoreNULL = FALSE)
    
    # Estimativas
    output$estimativas_ui <- renderUI({
      r <- result_rv()
      req(r)
      relato <- relatar_as(r, r$var_y)
      
      tagList(
        h6(sprintf("Informações do Sorteio (Salto k = %d, Partida r = %d)", r$k, r$r), style = "font-family: 'Outfit'; font-weight: 700; color: #0F3B5F; margin-top: 5px;"),
        tableOutput(ns("estimativas_table")),
        hr(),
        h6("Relato Científico Automatizado (Aproximação AAS)", style = "font-family: 'Outfit'; font-weight: 700; color: #0F3B5F;"),
        div(class = "alert alert-secondary", style = "font-size: 0.9rem; line-height: 1.4;", relato)
      )
    })
    
    output$estimativas_table <- renderTable({
      r <- result_rv()
      req(r)
      mostrar_as(r)
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
    
    # Dados sorteados
    output$dados_table <- renderDT({
      r <- result_rv()
      req(r)
      datatable(r$sample_data, options = list(pageLength = 10, scrollX = TRUE))
    })
    
    # Gráfico do espaçamento da amostragem sistemática
    output$plot_sorteio <- renderPlot({
      r <- result_rv()
      df <- data_rv()
      req(r, df)
      
      df_plot <- df
      df_plot$Status <- "Não Sorteado"
      df_plot$Status[r$indices] <- "Sorteado"
      df_plot$Indice <- 1:nrow(df_plot)
      
      ggplot(df_plot, aes(x = Indice, y = .data[[r$var_y]])) +
        geom_line(color = "#cfe2ff", alpha = 0.5) +
        geom_point(aes(color = Status, size = Status, shape = Status), alpha = 0.8) +
        scale_color_manual(values = c("Não Sorteado" = "#62B6B7", "Sorteado" = "#E76F51")) +
        scale_size_manual(values = c("Não Sorteado" = 1.8, "Sorteado" = 4)) +
        scale_shape_manual(values = c("Não Sorteado" = 16, "Sorteado" = 18)) +
        theme_minimal(base_size = 14) +
        labs(title = "Dispersão e Salto Sistemático na População",
             x = "Índice de Seleção Sequencial",
             y = r$var_y,
             color = "Status de Sorteio") +
        theme(plot.title = element_text(face = "bold", color = "#0F3B5F"),
              legend.position = "bottom")
    })
    
    # Relatórios e Zip
    output$download_report_docx <- downloadHandler(
      filename = function() {
        paste0("relatorio_amostragem_as_", format(Sys.Date(), "%Y-%m-%d"), ".docx")
      },
      content = function(file) {
        req(data_rv())
        
        temp_dir <- tempdir()
        temp_qmd <- file.path(temp_dir, "relatorio_sampling.qmd")
        temp_ref <- file.path(temp_dir, "custom-reference.docx")
        temp_func <- file.path(temp_dir, "funcoes_sampling.R")
        temp_data <- file.path(temp_dir, "dados_limpos.rda")
        
        file.copy("templates/custom-reference.docx", temp_ref, overwrite = TRUE)
        file.copy("templates/funcoes_sampling.R", temp_func, overwrite = TRUE)
        file.copy("templates/relatorio_sampling.qmd", temp_qmd, overwrite = TRUE)
        
        df_clean <- data_rv()
        save(df_clean, file = temp_data)
        
        custom_qmd_lines <- customize_sampling_qmd_params(
          temp_qmd,
          sampling_type = "as",
          var_y = input$var_y,
          var_strata = "NULL",
          n = input$sample_n,
          seed = input$seed,
          label_y = input$var_y,
          label_strata = "NULL"
        )
        writeLines(custom_qmd_lines, temp_qmd)
        
        old_wd <- getwd()
        setwd(temp_dir)
        system2("quarto", args = c("render", "relatorio_sampling.qmd", "--to", "docx"))
        setwd(old_wd)
        
        generated_docx <- file.path(temp_dir, "relatorio_sampling.docx")
        if (file.exists(generated_docx)) {
          file.copy(generated_docx, file, overwrite = TRUE)
        } else {
          writeLines("Erro ao compilar o Word.", file)
        }
      }
    )
    
    output$download_project_zip <- downloadHandler(
      filename = function() {
        paste0("projeto_amostragem_as_", format(Sys.Date(), "%Y-%m-%d"), ".zip")
      },
      content = function(file) {
        info <- import_info()
        proj_dir_name <- paste0("projeto_amostragem_as_", format(Sys.Date(), "%Y-%m-%d"))
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
          "# --- SCRIPT DE AMOSTRAGEM SISTEMÁTICA (AS) ---",
          "# Instalação de pacotes recomendados no RStudio:",
          "# install.packages(c('ggplot2', 'readxl', 'writexl'))",
          "library(ggplot2)",
          "source('scripts/funcoes_sampling.R')",
          "",
          "# 1. CARREGAR OS DADOS LIMPOS",
          "load('dados/dados_limpos.rda')",
          "dados <- df_clean",
          "",
          "# 2. CALCULAR ESTIMATIVAS SISTEMÁTICAS DA AS",
          sprintf("r <- calcular_as(dados, var_y = '%s', n = %d, seed = %d)", 
                  input$var_y, input$sample_n, input$seed),
          "print(mostrar_as(r))",
          "cat(relatar_as(r))",
          "",
          "# 3. VISUALIZAÇÃO",
          "df_plot <- dados",
          "df_plot$Status <- 'Não Sorteado'",
          "df_plot$Status[r$indices] <- 'Sorteado'",
          "df_plot$Indice <- 1:nrow(df_plot)",
          sprintf("ggplot(df_plot, aes(x = Indice, y = `%s`)) +", input$var_y),
          "  geom_line(color = '#cfe2ff', alpha = 0.5) +",
          "  geom_point(aes(color = Status), size = 3) +",
          "  scale_color_manual(values = c('Não Sorteado' = '#62B6B7', 'Sorteado' = '#E76F51')) +",
          "  theme_minimal() +",
          "  labs(title = 'Amostra Sorteada via AS')"
        )
        
        writeLines(r_script_content, file.path(dir_scripts, "amostrar_as.R"))
        file.copy("templates/custom-reference.docx", file.path(dir_relatorios, "custom-reference.docx"), overwrite = TRUE)
        file.copy("templates/funcoes_sampling.R", file.path(dir_scripts, "funcoes_sampling.R"), overwrite = TRUE)
        
        custom_qmd_lines <- customize_sampling_qmd_params(
          "templates/relatorio_sampling.qmd",
          sampling_type = "as",
          var_y = input$var_y,
          var_strata = "NULL",
          n = input$sample_n,
          seed = input$seed,
          label_y = input$var_y,
          label_strata = "NULL"
        )
        writeLines(custom_qmd_lines, file.path(dir_relatorios, "relatorio_sampling.qmd"))
        
        rproj_content <- c("Version: 1.0", "RestoreWorkspace: Default", "SaveWorkspace: Default", "Encoding: UTF-8")
        writeLines(rproj_content, file.path(proj_dir, "projeto_analise.Rproj"))
        
        readme_content <- c(
          "PACOTE DE AMOSTRAGEM SISTEMÁTICA (AS)",
          "- projeto_analise.Rproj: Duplo clique para abrir no RStudio.",
          "- dados/               : Contém os dados limpos em .rda, .csv e .xlsx.",
          "- scripts/amostrar_as.R : Script contendo o sorteio e estimadores da AS."
        )
        writeLines(readme_content, file.path(proj_dir, "README.txt"))
        
        old_wd <- getwd()
        setwd(temp_dir)
        zip::zip(file, files = proj_dir_name)
        setwd(old_wd)
      }
    )
  })
}
