# Módulo de Análise de Componentes Principais (PCA) para IDE_R
library(shiny)
library(bslib)
library(ggplot2)
library(DT)

if (file.exists("templates/funcoes_pca.R")) {
  source("templates/funcoes_pca.R")
}

# Helper para customizar parâmetros do relatório Quarto de PCA
customize_pca_qmd_params <- function(qmd_path, vars_selected, scale) {
  lines <- readLines(qmd_path, warn = FALSE)
  lines <- gsub('vars_selected: ".*"', sprintf('vars_selected: "%s"', vars_selected), lines)
  lines <- gsub('scale: .*', sprintf('scale: %s', tolower(as.character(scale))), lines)
  return(lines)
}

mod_pca_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(1, 1, 1),
      style = "grid-template-columns: 2.5fr 7fr 2.5fr !important;",
      
      # COLUNA 1: CONFIGURAÇÃO DO MODELO
      div(
        card(
          card_header("Configuração da PCA"),
          card_body(
            style = "padding: 12px 15px;",
            checkboxGroupInput(ns("vars_selected"), "Selecione as Variáveis Numéricas (mínimo 2):", choices = NULL),
            checkboxInput(ns("scale"), "Padronizar Variáveis (Escala Unitária)", value = TRUE)
          )
        ),
        card(
          card_header("Relatório e Pacote de Estudo"),
          card_body(
            style = "padding: 12px 15px;",
            downloadButton(ns("download_report_docx"), "Baixar Relatório Word (.docx)", class = "btn-success w-100"),
            div(style = "margin-top: 8px;"),
            downloadButton(ns("download_project_zip"), "Exportar Projeto R (.zip)", class = "btn-primary w-100"),
            helpText("Gera relatórios de PCA e pacotes para compilação local.", style = "margin-top: 10px; font-size: 0.85rem;")
          )
        )
      ),
      
      # COLUNA 2: ABAS DE RESULTADOS (PRINCIPAL)
      navset_card_tab(
        id = ns("active_tab"),
        title = "Painel de Resultados da PCA",
        nav_panel(
          title = "Variância Explicada",
          icon = icon("chart-pie"),
          card_body(
            uiOutput(ns("variancia_ui"))
          )
        ),
        nav_panel(
          title = "Cargas dos Componentes",
          icon = icon("table"),
          card_body(
            uiOutput(ns("cargas_ui"))
          )
        ),
        nav_panel(
          title = "Biplot Bidimensional",
          icon = icon("diagram-project"),
          card_body(
            plotOutput(ns("biplot"), height = "450px")
          )
        ),
        nav_panel(
          title = "Escores dos Indivíduos",
          icon = icon("list"),
          card_body(
            DTOutput(ns("scores_table"))
          )
        )
      ),
      
      # COLUNA 3: CONFIGURAÇÕES DE EXIBIÇÃO
      card(
        card_header("Configurações de Exibição"),
        card_body(
          conditionalPanel(
            condition = sprintf("input['%s'] == 'Biplot Bidimensional'", ns("active_tab")),
            checkboxInput(ns("show_labels"), "Exibir Rótulos de Cargas", value = TRUE),
            checkboxInput(ns("show_points"), "Exibir Observações", value = TRUE),
            selectInput(ns("graph_theme"), "Tema do Gráfico:", 
                        choices = c("Mínimo" = "minimal", 
                                    "Clássico" = "classic", 
                                    "Preto e Branco" = "bw", 
                                    "Cinza" = "gray", 
                                    "Light" = "light"), 
                        selected = "minimal")
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] != 'Biplot Bidimensional'", ns("active_tab")),
            helpText("As tabelas de autovalores e cargas são calculadas de forma exata e não dependem de configurações gráficas.")
          )
        )
      )
    )
  )
}

mod_pca_server <- function(id, data_rv, import_info) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Atualiza lista de variáveis numéricas na interface
    observe({
      df <- data_rv()
      req(df)
      num_cols <- names(df)[sapply(df, is.numeric)]
      # Remove a coluna id se existir para não poluir
      num_cols <- setdiff(num_cols, c("id", "ID"))
      updateCheckboxGroupInput(session, "vars_selected", choices = num_cols, selected = head(num_cols, 3))
    })
    
    # Executa cálculo reativo da PCA
    result_rv <- reactive({
      df <- data_rv()
      req(df, length(input$vars_selected) >= 2)
      calcular_pca(df, input$vars_selected, input$scale)
    })
    
    # Tabela de variância explicada e gráfico de cotovelo
    output$variancia_ui <- renderUI({
      r <- result_rv()
      req(r)
      relato <- relatar_pca(r)
      
      tagList(
        layout_columns(
          col_widths = c(6, 6),
          card_body(
            h6("Autovalores e Proporção de Variabilidade", style = "font-family: 'Outfit'; font-weight: 700; color: #0F3B5F;"),
            tableOutput(ns("variancia_table"))
          ),
          card_body(
            h6("Gráfico de Cotovelo (Scree Plot)", style = "font-family: 'Outfit'; font-weight: 700; color: #0F3B5F;"),
            plotOutput(ns("scree_plot"), height = "250px")
          )
        ),
        hr(),
        h6("Relato Científico Automatizado", style = "font-family: 'Outfit'; font-weight: 700; color: #0F3B5F;"),
        div(class = "alert alert-secondary", style = "font-size: 0.9rem; line-height: 1.4;", relato)
      )
    })
    
    output$variancia_table <- renderTable({
      r <- result_rv()
      req(r)
      mostrar_pca_var(r)
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
    
    output$scree_plot <- renderPlot({
      r <- result_rv()
      req(r)
      
      scree_data <- data.frame(
        PC = factor(r$var_df$PC, levels = r$var_df$PC),
        Variancia = r$var_df$Variancia_Pct
      )
      
      ggplot(scree_data, aes(x = PC, y = Variancia)) +
        geom_bar(stat = "identity", fill = "#0F3B5F", width = 0.6) +
        geom_line(group = 1, color = "#E76F51", linewidth = 1) +
        geom_point(color = "#E76F51", size = 2) +
        theme_minimal(base_size = 11) +
        labs(x = "Componentes Principais", y = "Variância Explicada (%)")
    })
    
    # Cargas dos componentes
    output$cargas_ui <- renderUI({
      r <- result_rv()
      req(r)
      
      tagList(
        h6("Cargas dos Componentes Principais (Autovetores)", style = "font-family: 'Outfit'; font-weight: 700; color: #0F3B5F; margin-top: 5px;"),
        tableOutput(ns("cargas_table")),
        helpText("As cargas representam o peso linear (correlação) de cada variável original com os respectivos Componentes Principais. Valores mais altos (positivos ou negativos) indicam maior importância.")
      )
    })
    
    output$cargas_table <- renderTable({
      r <- result_rv()
      req(r)
      mostrar_pca_loadings(r)
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
    
    # DT escores
    output$scores_table <- renderDT({
      r <- result_rv()
      req(r)
      datatable(round(r$scores, 4), options = list(pageLength = 10, scrollX = TRUE))
    })
    
    # Biplot de PCA
    output$biplot <- renderPlot({
      r <- result_rv()
      req(r)
      
      g_theme <- switch(input$graph_theme,
                        "minimal" = theme_minimal(base_size = 14),
                        "classic" = theme_classic(base_size = 14),
                        "bw"      = theme_bw(base_size = 14),
                        "gray"    = theme_gray(base_size = 14),
                        "light"   = theme_light(base_size = 14),
                        theme_minimal(base_size = 14))
      
      scores_df <- r$scores
      loadings_mat <- r$rotation
      
      # Escalar as setas para o gráfico
      scaling <- max(abs(scores_df[,1:2])) / max(abs(loadings_mat[,1:2])) * 0.75
      
      arrow_df <- data.frame(
        Variable = rownames(loadings_mat),
        x = 0, y = 0,
        vx = loadings_mat[,1] * scaling,
        vy = loadings_mat[,2] * scaling
      )
      
      p <- ggplot()
      
      # Adicionar observações
      if (input$show_points) {
        p <- p + geom_point(data = scores_df, aes(x = PC1, y = PC2), color = "#62B6B7", alpha = 0.6, size = 2.5)
      }
      
      # Adicionar cargas/setas
      p <- p + geom_segment(data = arrow_df, aes(x = x, y = y, xend = vx, yend = vy),
                           arrow = arrow(length = unit(0.25, "cm")), color = "#E76F51", linewidth = 1.2)
      
      # Adicionar rótulos das variáveis
      if (input$show_labels) {
        p <- p + ggrepel::geom_text_repel(data = arrow_df, aes(x = vx, y = vy, label = Variable),
                                         color = "#0F3B5F", fontface = "bold", size = 4.5)
      }
      
      p + g_theme +
        labs(title = "Biplot Bidimensional (PC1 vs PC2)",
             x = paste0("PC1 (", round(r$var_df$Variancia_Pct[1], 1), "%)"),
             y = paste0("PC2 (", round(r$var_df$Variancia_Pct[2], 1), "%)")) +
        theme(plot.title = element_text(face = "bold", color = "#0F3B5F"))
    })
    
    # Handlers de Download
    output$download_report_docx <- downloadHandler(
      filename = function() {
        paste0("relatorio_pca_", format(Sys.Date(), "%Y-%m-%d"), ".docx")
      },
      content = function(file) {
        req(data_rv())
        
        temp_dir <- tempdir()
        temp_qmd <- file.path(temp_dir, "relatorio_pca.qmd")
        temp_ref <- file.path(temp_dir, "custom-reference.docx")
        temp_func <- file.path(temp_dir, "funcoes_pca.R")
        temp_data <- file.path(temp_dir, "dados_limpos.rda")
        
        file.copy("templates/custom-reference.docx", temp_ref, overwrite = TRUE)
        file.copy("templates/funcoes_pca.R", temp_func, overwrite = TRUE)
        file.copy("templates/relatorio_pca.qmd", temp_qmd, overwrite = TRUE)
        
        df_clean <- data_rv()
        save(df_clean, file = temp_data)
        
        vars_str <- paste(input$vars_selected, collapse = ",")
        custom_qmd_lines <- customize_pca_qmd_params(
          temp_qmd,
          vars_selected = vars_str,
          scale = input$scale
        )
        writeLines(custom_qmd_lines, temp_qmd)
        
        old_wd <- getwd()
        setwd(temp_dir)
        system2("quarto", args = c("render", "relatorio_pca.qmd", "--to", "docx"))
        setwd(old_wd)
        
        generated_docx <- file.path(temp_dir, "relatorio_pca.docx")
        if (file.exists(generated_docx)) {
          file.copy(generated_docx, file, overwrite = TRUE)
        } else {
          writeLines("Erro ao compilar o Word.", file)
        }
      }
    )
    
    output$download_project_zip <- downloadHandler(
      filename = function() {
        paste0("projeto_pca_", format(Sys.Date(), "%Y-%m-%d"), ".zip")
      },
      content = function(file) {
        info <- import_info()
        proj_dir_name <- paste0("projeto_pca_", format(Sys.Date(), "%Y-%m-%d"))
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
        
        vars_str <- paste(paste0("'", input$vars_selected, "'"), collapse = ", ")
        r_script_content <- c(
          "# --- SCRIPT DE ANÁLISE DE COMPONENTES PRINCIPAIS (PCA) ---",
          "# Instalação de pacotes recomendados no RStudio:",
          "# install.packages(c('ggplot2', 'readxl', 'writexl', 'ggrepel'))",
          "library(ggplot2)",
          "source('scripts/funcoes_pca.R')",
          "",
          "# 1. CARREGAR OS DADOS LIMPOS",
          "load('dados/dados_limpos.rda')",
          "dados <- df_clean",
          "",
          "# 2. EXECUTAR PCA E MOSTRAR ESTATÍSTICAS",
          sprintf("vars_selected <- c(%s)", vars_str),
          sprintf("r <- calcular_pca(dados, vars_selected, scale = %s)", 
                  as.character(input$scale)),
          "print(mostrar_pca_var(r))",
          "print(mostrar_pca_loadings(r))",
          "cat(relatar_pca(r))",
          "",
          "# 3. GERAR O PLOT DO BIPLOT",
          "scores_df <- r$scores",
          "loadings_mat <- r$rotation",
          "scaling <- max(abs(scores_df[,1:2])) / max(abs(loadings_mat[,1:2])) * 0.75",
          "arrow_df <- data.frame(Variable = rownames(loadings_mat), x = 0, y = 0,",
          "                       vx = loadings_mat[,1] * scaling, vy = loadings_mat[,2] * scaling)",
          "ggplot(scores_df, aes(x = PC1, y = PC2)) +",
          "  geom_point(color = '#62B6B7', alpha = 0.6) +",
          "  geom_segment(data = arrow_df, aes(x = x, y = y, xend = vx, yend = vy),",
          "               arrow = arrow(length = unit(0.2, 'cm')), color = '#E76F51', linewidth = 1) +",
          "  geom_text(data = arrow_df, aes(x = vx, y = vy, label = Variable), color = '#0F3B5F', fontface = 'bold') +",
          "  theme_minimal() +",
          "  labs(title = 'PCA: Biplot PC1 vs PC2')"
        )
        
        writeLines(r_script_content, file.path(dir_scripts, "analise_pca.R"))
        file.copy("templates/custom-reference.docx", file.path(dir_relatorios, "custom-reference.docx"), overwrite = TRUE)
        file.copy("templates/funcoes_pca.R", file.path(dir_scripts, "funcoes_pca.R"), overwrite = TRUE)
        
        vars_str_qmd <- paste(input$vars_selected, collapse = ",")
        custom_qmd_lines <- customize_pca_qmd_params(
          "templates/relatorio_pca.qmd",
          vars_selected = vars_str_qmd,
          scale = input$scale
        )
        writeLines(custom_qmd_lines, file.path(dir_relatorios, "relatorio_pca.qmd"))
        
        rproj_content <- c("Version: 1.0", "RestoreWorkspace: Default", "SaveWorkspace: Default", "Encoding: UTF-8")
        writeLines(rproj_content, file.path(proj_dir, "projeto_analise.Rproj"))
        
        readme_content <- c(
          "PACOTE DE ANÁLISE DE COMPONENTES PRINCIPAIS (PCA)",
          "- projeto_analise.Rproj: Duplo clique para abrir no RStudio.",
          "- dados/               : Contém os dados limpos em .rda, .csv e .xlsx.",
          "- scripts/analise_pca.R : Script contendo o cálculo da PCA e biplot."
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
