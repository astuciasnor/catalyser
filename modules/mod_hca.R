# Módulo de Análise de Agrupamento Hierárquico (AAH / HCA) para IDE_R
library(shiny)
library(bslib)
library(ggplot2)
library(DT)

if (file.exists("templates/funcoes_hca.R")) {
  source("templates/funcoes_hca.R")
}

# Helper para customizar parâmetros do relatório Quarto de HCA
customize_hca_qmd_params <- function(qmd_path, vars_selected, distance_method, linkage_method, k_groups, scale) {
  lines <- readLines(qmd_path, warn = FALSE)
  lines <- gsub('vars_selected: ".*"', sprintf('vars_selected: "%s"', vars_selected), lines)
  lines <- gsub('distance_method: ".*"', sprintf('distance_method: "%s"', distance_method), lines)
  lines <- gsub('linkage_method: ".*"', sprintf('linkage_method: "%s"', linkage_method), lines)
  lines <- gsub('k_groups: .*', sprintf('k_groups: %s', k_groups), lines)
  lines <- gsub('scale: .*', sprintf('scale: %s', tolower(as.character(scale))), lines)
  return(lines)
}

mod_hca_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(1, 1, 1),
      style = "grid-template-columns: 2.5fr 7fr 2.5fr !important;",
      
      # COLUNA 1: CONFIGURAÇÃO DO MODELO
      div(
        card(
          card_header("Configuração da AAH (Clustering)"),
          card_body(
            style = "padding: 12px 15px;",
            checkboxGroupInput(ns("vars_selected"), "Selecione as Variáveis Numéricas (mínimo 2):", choices = NULL),
            selectInput(ns("distance_method"), "Métrica de Distância:",
                        choices = c("Euclidiana" = "euclidean",
                                    "Manhattan (City-Block)" = "manhattan"),
                        selected = "euclidean"),
            selectInput(ns("linkage_method"), "Método de Ligação:",
                        choices = c("Ward.D2 (Mínima Variância)" = "ward.D2",
                                    "Completa (Complete Linkage)" = "complete",
                                    "Simples (Single Linkage)" = "single",
                                    "Média (UPGMA)" = "average"),
                        selected = "ward.D2"),
            sliderInput(ns("k_groups"), "Número de Grupos (k):",
                        min = 2, max = 8, value = 3, step = 1),
            checkboxInput(ns("scale"), "Padronizar Variáveis (Z-Score)", value = TRUE)
          )
        ),
        card(
          card_header("Relatório e Pacote de Estudo"),
          card_body(
            style = "padding: 12px 15px;",
            downloadButton(ns("download_report_docx"), "Baixar Relatório Word (.docx)", class = "btn-success w-100"),
            div(style = "margin-top: 8px;"),
            downloadButton(ns("download_project_zip"), "Exportar Projeto R (.zip)", class = "btn-primary w-100"),
            helpText("Gera relatórios de AAH e pacotes para compilação local.", style = "margin-top: 10px; font-size: 0.85rem;")
          )
        )
      ),
      
      # COLUNA 2: ABAS DE RESULTADOS (PRINCIPAL)
      navset_card_tab(
        id = ns("active_tab"),
        title = "Painel de Resultados da AAH",
        nav_panel(
          title = "Dendrograma",
          icon = icon("diagram-project"),
          card_body(
            plotOutput(ns("dendrogram"), height = "450px")
          )
        ),
        nav_panel(
          title = "Perfil dos Grupos",
          icon = icon("chart-pie"),
          card_body(
            uiOutput(ns("profile_ui"))
          )
        ),
        nav_panel(
          title = "Pertinência por Observação",
          icon = icon("list"),
          card_body(
            DTOutput(ns("pert_table"))
          )
        )
      ),
      
      # COLUNA 3: CONFIGURAÇÕES DE EXIBIÇÃO
      card(
        card_header("Configurações de Exibição"),
        card_body(
          conditionalPanel(
            condition = sprintf("input['%s'] == 'Dendrograma'", ns("active_tab")),
            checkboxInput(ns("show_labels"), "Exibir Rótulos das Observações", value = FALSE),
            helpText("Rótulos podem se sobrepor se houver muitas linhas de observação no dataset.")
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] != 'Dendrograma'", ns("active_tab")),
            helpText("As tabelas de agrupamento e perfis de média são baseadas no critério de corte k definido nas configurações.")
          )
        )
      )
    )
  )
}

mod_hca_server <- function(id, data_rv, import_info) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Atualiza as opções de variáveis baseadas no dataset
    observe({
      df <- data_rv()
      req(df)
      num_cols <- names(df)[sapply(df, is.numeric)]
      
      # Manter seleção se possível
      old_sel <- input$vars_selected
      valid_sel <- intersect(old_sel, num_cols)
      
      if (length(num_cols) >= 2) {
        if (length(valid_sel) < 2) {
          valid_sel <- num_cols[1:2]
        }
        updateCheckboxGroupInput(session, "vars_selected", choices = num_cols, selected = valid_sel)
      } else {
        updateCheckboxGroupInput(session, "vars_selected", choices = num_cols, selected = NULL)
      }
    })
    
    # Executa a AAH de forma reativa
    result_rv <- reactive({
      df <- data_rv()
      req(df)
      vars <- input$vars_selected
      req(length(vars) >= 2)
      
      # Executa cálculo AAH
      calcular_hca(
        df = df,
        vars_selected = vars,
        distance_method = input$distance_method,
        linkage_method = input$linkage_method,
        k_groups = input$k_groups,
        scale = input$scale
      )
    })
    
    # Renderizar Dendrograma
    output$dendrogram <- renderPlot({
      r <- result_rv()
      req(r)
      
      # Cores da identidade visual Ocean Gradient
      ocean_cols <- c("#0F3B5F", "#2E7D8F", "#62B6B7", "#E89B3C", "#E76F51")
      k_val <- r$k_groups
      border_cols <- ocean_cols[1:min(k_val, length(ocean_cols))]
      if(k_val > length(ocean_cols)) {
        border_cols <- c(border_cols, rainbow(k_val - length(ocean_cols)))
      }
      
      # Rótulos das observações
      lbls <- if (input$show_labels) NULL else FALSE
      
      plot(
        r$fit,
        labels = lbls,
        hang = -1,
        main = "Dendrograma de Agrupamento Hierárquico",
        sub = paste0("Distância: ", r$distance_method, " | Ligação: ", r$linkage_method),
        xlab = "Observações",
        ylab = "Altura (Distância de Agregação)",
        col = "#333333",
        font.main = 2,
        col.main = "#0F3B5F"
      )
      rect.hclust(r$fit, k = k_val, border = border_cols)
    })
    
    # Renderizar Perfil dos Grupos UI
    output$profile_ui <- renderUI({
      r <- result_rv()
      req(r)
      
      tab_perfil <- mostrar_hca_perfil(r)
      
      tagList(
        h6("Perfil de Médias por Cluster", style = "font-weight: 700; color: #0f3b5f; margin-bottom: 12px;"),
        renderDT({
          datatable(
            tab_perfil,
            options = list(dom = "t", ordering = FALSE, pageLength = 20),
            rownames = FALSE,
            class = "cell-border stripe"
          ) |> formatRound(columns = names(tab_perfil)[-c(1, 2)], digits = 3)
        }),
        hr(style = "margin: 15px 0;"),
        h6("Síntese dos Resultados", style = "font-weight: 700; color: #0f3b5f; margin-bottom: 8px;"),
        div(
          class = "alert alert-light",
          style = "border-left: 4px solid #E76F51; background-color: #f8f9fa; color: #333333; font-size: 0.9rem; line-height: 1.5; padding: 12px 15px; margin-bottom: 0;",
          relatar_hca(r)
        )
      )
    })
    
    # Renderizar tabela de pertinência de grupo
    output$pert_table <- renderDT({
      r <- result_rv()
      req(r)
      
      tab_pert <- mostrar_hca_pertinencia(r)
      datatable(
        tab_pert,
        options = list(pageLength = 10, dom = "lfrtip"),
        rownames = FALSE,
        class = "cell-border stripe"
      )
    })
    
    # Handlers de Download
    output$download_report_docx <- downloadHandler(
      filename = function() {
        paste0("relatorio_aah_", format(Sys.Date(), "%Y-%m-%d"), ".docx")
      },
      content = function(file) {
        req(data_rv())
        
        temp_dir <- tempdir()
        temp_qmd <- file.path(temp_dir, "relatorio_hca.qmd")
        temp_ref <- file.path(temp_dir, "custom-reference.docx")
        temp_func <- file.path(temp_dir, "funcoes_hca.R")
        temp_data <- file.path(temp_dir, "dados_limpos.rda")
        
        file.copy("templates/custom-reference.docx", temp_ref, overwrite = TRUE)
        file.copy("templates/funcoes_hca.R", temp_func, overwrite = TRUE)
        file.copy("templates/relatorio_hca.qmd", temp_qmd, overwrite = TRUE)
        
        df_clean <- data_rv()
        save(df_clean, file = temp_data)
        
        vars_str <- paste(input$vars_selected, collapse = ",")
        custom_qmd_lines <- customize_hca_qmd_params(
          temp_qmd,
          vars_selected = vars_str,
          distance_method = input$distance_method,
          linkage_method = input$linkage_method,
          k_groups = input$k_groups,
          scale = input$scale
        )
        writeLines(custom_qmd_lines, temp_qmd)
        
        old_wd <- getwd()
        setwd(temp_dir)
        system2("quarto", args = c("render", "relatorio_hca.qmd", "--to", "docx"))
        setwd(old_wd)
        
        generated_docx <- file.path(temp_dir, "relatorio_hca.docx")
        if (file.exists(generated_docx)) {
          file.copy(generated_docx, file, overwrite = TRUE)
        } else {
          writeLines("Erro ao compilar o Word.", file)
        }
      }
    )
    
    output$download_project_zip <- downloadHandler(
      filename = function() {
        paste0("projeto_aah_", format(Sys.Date(), "%Y-%m-%d"), ".zip")
      },
      content = function(file) {
        info <- import_info()
        proj_dir_name <- paste0("projeto_aah_", format(Sys.Date(), "%Y-%m-%d"))
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
          "# --- SCRIPT DE ANÁLISE DE AGRUPAMENTO HIERÁRQUICO (AAH / HCA) ---",
          "# Instalação de pacotes recomendados no RStudio:",
          "# install.packages(c('ggplot2', 'readxl', 'writexl'))",
          "source('scripts/funcoes_hca.R')",
          "",
          "# 1. CARREGAR OS DADOS LIMPOS",
          "load('dados/dados_limpos.rda')",
          "dados <- df_clean",
          "",
          "# 2. EXECUTAR AAH E MOSTRAR ESTATÍSTICAS",
          sprintf("vars_selected <- c(%s)", vars_str),
          sprintf("r <- calcular_hca(dados, vars_selected, distance_method = '%s', linkage_method = '%s', k_groups = %s, scale = %s)", 
                  input$distance_method, input$linkage_method, input$k_groups, as.character(input$scale)),
          "print(mostrar_hca_perfil(r))",
          "print(head(mostrar_hca_pertinencia(r), 20))",
          "cat(relatar_hca(r))",
          "",
          "# 3. GERAR O DENDROGRAMA COLORIDO",
          "ocean_cols <- c('#0F3B5F', '#2E7D8F', '#62B6B7', '#E89B3C', '#E76F51')",
          sprintf("k_val <- %s", input$k_groups),
          "border_cols <- ocean_cols[1:min(k_val, length(ocean_cols))]",
          "if(k_val > length(ocean_cols)) { border_cols <- c(border_cols, rainbow(k_val - length(ocean_cols))) }",
          "plot(r$fit, labels = FALSE, hang = -1, main = 'Dendrograma de Agrupamento Hierárquico',",
          sprintf("     sub = 'Distância: %s | Ligação: %s',", input$distance_method, input$linkage_method),
          "     xlab = 'Observações', ylab = 'Altura (Distância de Agregação)')",
          "rect.hclust(r$fit, k = k_val, border = border_cols)"
        )
        
        writeLines(r_script_content, file.path(dir_scripts, "analise_aah.R"))
        file.copy("templates/custom-reference.docx", file.path(dir_relatorios, "custom-reference.docx"), overwrite = TRUE)
        file.copy("templates/funcoes_hca.R", file.path(dir_scripts, "funcoes_hca.R"), overwrite = TRUE)
        
        vars_str_qmd <- paste(input$vars_selected, collapse = ",")
        custom_qmd_lines <- customize_hca_qmd_params(
          "templates/relatorio_hca.qmd",
          vars_selected = vars_str_qmd,
          distance_method = input$distance_method,
          linkage_method = input$linkage_method,
          k_groups = input$k_groups,
          scale = input$scale
        )
        writeLines(custom_qmd_lines, file.path(dir_relatorios, "relatorio_hca.qmd"))
        
        rproj_content <- c("Version: 1.0", "RestoreWorkspace: Default", "SaveWorkspace: Default", "Encoding: UTF-8")
        writeLines(rproj_content, file.path(proj_dir, "projeto_analise.Rproj"))
        
        readme_content <- c(
          "PACOTE DE ANÁLISE DE AGRUPAMENTO HIERÁRQUICO (AAH / HCA)",
          "- projeto_analise.Rproj: Duplo clique para abrir no RStudio.",
          "- dados/               : Contém os dados limpos em .rda, .csv e .xlsx.",
          "- scripts/analise_aah.R : Script contendo o cálculo da AAH e dendrograma."
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
