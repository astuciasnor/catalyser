# Módulo de Planejamento Experimental e Geração de Croqui - CatalyseR
library(shiny)
library(bslib)
library(ggplot2)
library(DT)

if (file.exists("templates/funcoes_experimental_design.R")) {
  source("templates/funcoes_experimental_design.R")
}

# Separar e limpar níveis digitados pelo usuário
parse_levels <- function(text) {
  parts <- strsplit(text, ",")[[1]]
  parts <- trimws(parts)
  parts <- parts[parts != ""]
  return(parts)
}

mod_experimental_design_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(1, 1, 1),
      style = "grid-template-columns: 2.5fr 7fr 2.5fr !important;",
      
      # COLUNA 1: CONFIGURAÇÃO DO EXPERIMENTO
      div(
        card(
          card_header("Configuração do Planejamento"),
          card_body(
            style = "padding: 12px 15px;",
            selectInput(ns("design_type"), "Tipo de Delineamento:",
                        choices = c("DIC - Inteiramente Casualizado" = "DIC",
                                    "DBC - Blocos Casualizados" = "DBC",
                                    "DQL - Quadrado Latino" = "DQL",
                                    "Fatorial (em DIC)" = "fatorial",
                                    "Parcelas Subdivididas (Split-Plot)" = "split_plot")),
            
            # Painel Condicional para DIC
            conditionalPanel(
              condition = sprintf("input['%s'] == 'DIC'", ns("design_type")),
              textInput(ns("dic_factor_name"), "Nome do Fator:", value = "Tratamento"),
              textInput(ns("dic_levels"), "Níveis do Fator (separados por vírgula):", value = "A, B, C"),
              numericInput(ns("dic_reps"), "Número de Repetições:", value = 5, min = 1),
              numericInput(ns("dic_nrows"), "Linhas do Croqui:", value = 5, min = 1),
              numericInput(ns("dic_ncols"), "Colunas do Croqui:", value = 3, min = 1),
              helpText("DIC: Grade recomendada = Níveis x Repetições.")
            ),
            
            # Painel Condicional para DBC
            conditionalPanel(
              condition = sprintf("input['%s'] == 'DBC'", ns("design_type")),
              textInput(ns("dbc_factor_name"), "Nome do Fator:", value = "Tratamento"),
              textInput(ns("dbc_levels"), "Níveis do Fator (separados por vírgula):", value = "A, B, C"),
              numericInput(ns("dbc_blocks"), "Número de Blocos:", value = 4, min = 1),
              numericInput(ns("dbc_nrows"), "Linhas do Croqui (igual a Blocos):", value = 4, min = 1),
              numericInput(ns("dbc_ncols"), "Colunas do Croqui (igual a Níveis):", value = 3, min = 1),
              helpText("DBC: Por padrão, Linhas = Blocos e Colunas = Níveis.")
            ),
            
            # Painel Condicional para DQL
            conditionalPanel(
              condition = sprintf("input['%s'] == 'DQL'", ns("design_type")),
              textInput(ns("dql_factor_name"), "Nome do Fator:", value = "Tratamento"),
              textInput(ns("dql_levels"), "Níveis do Fator (separados por vírgula):", value = "A, B, C"),
              helpText("DQL: Grade automática de tamanho K x K (onde K é o número de níveis).")
            ),
            
            # Painel Condicional para Fatorial
            conditionalPanel(
              condition = sprintf("input['%s'] == 'fatorial'", ns("design_type")),
              textInput(ns("fat_factor_a_name"), "Nome do Fator A:", value = "Adubo"),
              textInput(ns("fat_factor_a_levels"), "Níveis de A (separados por vírgula):", value = "A1, A2"),
              textInput(ns("fat_factor_b_name"), "Nome do Fator B:", value = "Irrigacao"),
              textInput(ns("fat_factor_b_levels"), "Níveis de B (separados por vírgula):", value = "I1, I2, I3"),
              numericInput(ns("fat_reps"), "Número de Repetições:", value = 3, min = 1),
              numericInput(ns("fat_nrows"), "Linhas do Croqui:", value = 6, min = 1),
              numericInput(ns("fat_ncols"), "Colunas do Croqui:", value = 3, min = 1),
              helpText("Fatorial: Grade recomendada = (Níveis de A x Níveis de B) x Repetições.")
            ),
            
            # Painel Condicional para Split-Plot
            conditionalPanel(
              condition = sprintf("input['%s'] == 'split_plot'", ns("design_type")),
              textInput(ns("sp_factor_main_name"), "Fator Principal (Parcela):", value = "Espacamento"),
              textInput(ns("sp_factor_main_levels"), "Níveis do Fator Principal:", value = "E1, E2"),
              textInput(ns("sp_factor_sub_name"), "Fator Subdividido (Subparcela):", value = "Variedade"),
              textInput(ns("sp_factor_sub_levels"), "Níveis do Fator Subdividido:", value = "V1, V2, V3"),
              numericInput(ns("sp_blocks"), "Número de Blocos:", value = 3, min = 1),
              numericInput(ns("sp_nrows"), "Linhas do Croqui (igual a Blocos):", value = 3, min = 1),
              numericInput(ns("sp_ncols"), "Colunas do Croqui (Main x Sub):", value = 6, min = 1),
              helpText("Split-Plot: Por padrão, Linhas = Blocos e Colunas = Níveis Principal x Níveis Subdividido.")
            ),
            
            hr(style = "margin: 10px 0;"),
            textInput(ns("response_var"), "Variável(is) de Resposta (separe por vírgula):", value = "Produtividade"),
            helpText("Cada nome vira uma coluna vazia no croqui/planilha para o discente coletar os dados. Ex.: Produtividade, Altura, Peso."),
            numericInput(ns("seed"), "Semente Aleatória:", value = 42, min = 1),
            actionButton(ns("btn_generate"), "Gerar Croqui", class = "btn-primary w-100")
          )
        )
      ),
      
      # COLUNA 2: RESULTADOS (ABAS)
      navset_card_tab(
        nav_panel(
          title = "Croqui da Área",
          icon = icon("table-cells"),
          card_body(
            style = "padding: 10px 15px;",
            plotOutput(ns("plot_croqui"), height = "450px")
          )
        ),
        nav_panel(
          title = "Ficha de Campo (Tabela Tidy)",
          icon = icon("table"),
          card_body(
            style = "padding: 10px 15px;",
            DTOutput(ns("table_unidades"))
          )
        ),
        nav_panel(
          title = "Descrição Metodológica",
          icon = icon("file-lines"),
          card_body(
            style = "padding: 10px 15px;",
            uiOutput(ns("report_descritivo"))
          )
        )
      ),
      
      # COLUNA 3: EXPORTAÇÃO
      div(
        card(
          card_header("Exportar Planejamento"),
          card_body(
            style = "padding: 12px 15px;",
            downloadButton(ns("download_report_docx"), "Relatório Word (.docx)", class = "btn-success w-100"),
            div(style = "margin-top: 8px;"),
            downloadButton(ns("download_project_zip"), "Exportar Projeto R (.zip)", class = "btn-primary w-100"),
            helpText("Gera a ficha de campo no Word (com tabelas e croqui editáveis) ou exporta o projeto completo estruturado para o RStudio (contendo planilhas de coleta).", style = "margin-top: 10px; font-size: 0.85rem;")
          )
        )
      )
    )
  )
}

mod_experimental_design_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Armazena o resultado do delineamento gerado
    delineamento_rv <- reactiveVal(NULL)
    
    # Evento de geração do croqui
    observeEvent(input$btn_generate, {
      type <- input$design_type
      resp <- parse_levels(input$response_var)   # vetor: uma coluna por resposta
      if (length(resp) == 0) resp <- "Resposta"
      
      res <- NULL
      
      if (type == "DIC") {
        levels_vec <- parse_levels(input$dic_levels)
        if (length(levels_vec) < 2) {
          showNotification("Erro: O fator de entrada deve possuir pelo menos 2 níveis.", type = "error")
          return()
        }
        res <- gerar_delineamento_dic(
          factor_name = input$dic_factor_name,
          levels_vec = levels_vec,
          reps = input$dic_reps,
          nrows = input$dic_nrows,
          ncols = input$dic_ncols,
          seed = input$seed,
          response_var = resp
        )
      } else if (type == "DBC") {
        levels_vec <- parse_levels(input$dbc_levels)
        if (length(levels_vec) < 2) {
          showNotification("Erro: O fator de entrada deve possuir pelo menos 2 níveis.", type = "error")
          return()
        }
        res <- gerar_delineamento_dbc(
          factor_name = input$dbc_factor_name,
          levels_vec = levels_vec,
          blocks = input$dbc_blocks,
          nrows = input$dbc_nrows,
          ncols = input$dbc_ncols,
          seed = input$seed,
          response_var = resp
        )
      } else if (type == "DQL") {
        levels_vec <- parse_levels(input$dql_levels)
        if (length(levels_vec) < 2) {
          showNotification("Erro: O fator de entrada deve possuir pelo menos 2 níveis.", type = "error")
          return()
        }
        res <- gerar_delineamento_dql(
          factor_name = input$dql_factor_name,
          levels_vec = levels_vec,
          seed = input$seed,
          response_var = resp
        )
      } else if (type == "fatorial") {
        levels_a <- parse_levels(input$fat_factor_a_levels)
        levels_b <- parse_levels(input$fat_factor_b_levels)
        if (length(levels_a) < 2 || length(levels_b) < 2) {
          showNotification("Erro: Ambos os fatores A e B devem possuir pelo menos 2 níveis.", type = "error")
          return()
        }
        res <- gerar_delineamento_fatorial(
          fator_a_name = input$fat_factor_a_name,
          fator_a_levels = levels_a,
          fator_b_name = input$fat_factor_b_name,
          fator_b_levels = levels_b,
          reps = input$fat_reps,
          nrows = input$fat_nrows,
          ncols = input$fat_ncols,
          seed = input$seed,
          response_var = resp
        )
      } else if (type == "split_plot") {
        levels_main <- parse_levels(input$sp_factor_main_levels)
        levels_sub <- parse_levels(input$sp_factor_sub_levels)
        if (length(levels_main) < 2 || length(levels_sub) < 2) {
          showNotification("Erro: Ambos os fatores (principal e subdividido) devem possuir pelo menos 2 níveis.", type = "error")
          return()
        }
        res <- gerar_delineamento_split_plot(
          fator_main_name = input$sp_factor_main_name,
          fator_main_levels = levels_main,
          fator_sub_name = input$sp_factor_sub_name,
          fator_sub_levels = levels_sub,
          blocks = input$sp_blocks,
          nrows = input$sp_nrows,
          ncols = input$sp_ncols,
          seed = input$seed,
          response_var = resp
        )
      }
      
      if (!is.null(res)) {
        if (res$error) {
          showModal(modalDialog(
            title = "Aviso: Grade Insuficiente",
            res$message,
            easyClose = TRUE,
            footer = modalButton("OK")
          ))
        } else {
          delineamento_rv(res)
          showNotification("Croqui gerado com sucesso!", type = "message")
        }
      }
    })
    
    # Forçar a primeira geração por padrão se estiver vazio
    observe({
      if (is.null(delineamento_rv())) {
        # Dispara o clique inicial simulado
        click("btn_generate")
      }
    })
    
    # Auxiliar para clique simulado inicial
    click <- function(id) {
      session$sendInputMessage(id, list(value = input[[id]] + 1))
    }
    
    # 1. Renderizar Croqui
    output$plot_croqui <- renderPlot({
      res <- delineamento_rv()
      req(res)
      plotar_croqui(res, input$design_type)
    })
    
    # 2. Renderizar Tabela Tidy (DT)
    output$table_unidades <- renderDT({
      res <- delineamento_rv()
      req(res)
      
      df_tab <- res$df
      # Remover a coluna de Cor interna para a visualização na tabela do usuário
      if ("Cor" %in% names(df_tab)) {
        df_tab$Cor <- NULL
      }
      
      # Mapear cores para estilização do DT datatable
      # (deixa as linhas do DT coloridas de acordo com o croqui)
      dt_tbl <- datatable(
        df_tab,
        options = list(pageLength = 15, dom = "rtip"),
        rownames = FALSE,
        class = "cell-border stripe"
      )
      
      # Adicionar formatação de cores nas células de tratamento do DT
      # Para cada tratamento, aplica a respectiva cor de fundo
      level_colors <- res$level_colors
      for (t in names(level_colors)) {
        dt_tbl <- formatStyle(
          dt_tbl,
          "Tratamento",
          target = "row",
          backgroundColor = styleEqual(t, level_colors[t]),
          fontWeight = styleEqual(t, "bold")
        )
      }
      
      dt_tbl
    })
    
    # 3. Renderizar Descrição Metodológica
    output$report_descritivo <- renderUI({
      res <- delineamento_rv()
      req(res)
      
      tagList(
        h6("Relato do Delineamento Experimental", style = "font-weight: 700; color: #0f3b5f; margin-bottom: 12px;"),
        div(
          class = "alert alert-light",
          style = "border-left: 4px solid #0F3B5F; background-color: #f8f9fa; color: #333333; font-size: 0.9rem; line-height: 1.5; padding: 12px 15px; margin-bottom: 0;",
          relatar_delineamento(res, input$design_type)
        )
      )
    })
    
    # Handlers de Download
    
    # Download do Relatório Word (.docx)
    output$download_report_docx <- downloadHandler(
      filename = function() {
        paste0("relatorio_planejamento_", tolower(input$design_type), "_", format(Sys.Date(), "%Y-%m-%d"), ".docx")
      },
      content = function(file) {
        res <- delineamento_rv()
        req(res)
        
        temp_dir <- tempdir()
        temp_qmd <- file.path(temp_dir, "relatorio_experimental_design.qmd")
        temp_ref <- file.path(temp_dir, "custom-reference.docx")
        temp_func <- file.path(temp_dir, "funcoes_experimental_design.R")
        temp_data <- file.path(temp_dir, "delineamento_data.rda")
        
        file.copy("templates/custom-reference.docx", temp_ref, overwrite = TRUE)
        file.copy("templates/funcoes_experimental_design.R", temp_func, overwrite = TRUE)
        file.copy("templates/relatorio_experimental_design.qmd", temp_qmd, overwrite = TRUE)
        
        # Salva o objeto para compilação pelo Quarto
        delineamento_res <- res
        design_type <- input$design_type
        save(delineamento_res, design_type, file = temp_data)
        
        old_wd <- getwd()
        setwd(temp_dir)
        system2("quarto", args = c("render", "relatorio_experimental_design.qmd", "--to", "docx"))
        setwd(old_wd)
        
        generated_docx <- file.path(temp_dir, "relatorio_experimental_design.docx")
        if (file.exists(generated_docx)) {
          file.copy(generated_docx, file, overwrite = TRUE)
        } else {
          writeLines("Erro ao renderizar o Word pelo Quarto CLI.", file)
        }
      }
    )
    
    # Download do Pacote ZIP (.zip)
    output$download_project_zip <- downloadHandler(
      filename = function() {
        paste0("projeto_delineamento_", tolower(input$design_type), "_", format(Sys.Date(), "%Y-%m-%d"), ".zip")
      },
      content = function(file) {
        res <- delineamento_rv()
        req(res)
        
        proj_dir_name <- paste0("projeto_delineamento_", tolower(input$design_type), "_", format(Sys.Date(), "%Y-%m-%d"))
        temp_dir <- tempdir()
        proj_dir <- file.path(temp_dir, proj_dir_name)
        
        dir.create(proj_dir, showWarnings = FALSE)
        dir_dados <- file.path(proj_dir, "dados")
        dir_scripts <- file.path(proj_dir, "scripts")
        dir_relatorios <- file.path(proj_dir, "relatorios")
        
        dir.create(dir_dados, showWarnings = FALSE)
        dir.create(dir_scripts, showWarnings = FALSE)
        dir.create(dir_relatorios, showWarnings = FALSE)
        
        # Dados limpos em formato longo e tidy
        df_exp <- res$df
        df_exp_clean <- df_exp
        if ("Cor" %in% names(df_exp_clean)) {
          df_exp_clean$Cor <- NULL
        }
        
        # Salva dados
        save(df_exp, file = file.path(dir_dados, "dados_experimento.rda"))
        write.csv(df_exp_clean, file = file.path(dir_dados, "dados_experimento.csv"), row.names = FALSE)
        writexl::write_xlsx(df_exp_clean, path = file.path(dir_dados, "dados_experimento.xlsx"))
        
        # Script de reprodução e análise futura (ANOVA)
        r_script_content <- c(
          sprintf("# --- SCRIPT DE REPRODUÇÃO: PLANEJAMENTO EXPERIMENTAL (%s) ---", input$design_type),
          "# Instale os pacotes requeridos no RStudio:",
          "# install.packages(c('ggplot2', 'flextable', 'writexl', 'readxl'))",
          "library(ggplot2)",
          "library(flextable)",
          "source('scripts/funcoes_experimental_design.R')",
          "",
          "# 1. CARREGAR OS DADOS DO DELINEAMENTO",
          "# Insira os valores da variável de resposta na coluna vazia no Excel",
          "# 'dados/dados_experimento.xlsx' e salve o arquivo.",
          "dados <- readxl::read_excel('dados/dados_experimento.xlsx')",
          "head(dados)",
          "",
          "# 2. REGERAR O PLOT DO CROQUI",
          "load('relatorios/delineamento_data.rda')",
          "p <- plotar_croqui(delineamento_res, design_type)",
          "print(p)",
          "ggsave('relatorios/croqui_area.png', p, width = 7, height = 5, dpi = 300)",
          "",
          "# 3. EXIBIR A TABELA FORMATADA E COLORIDA",
          "ft <- flextable_croqui(dados, design_type, delineamento_res$level_colors)",
          "print(ft)",
          "",
          "# 4. EXEMPLO DE ANÁLISE DE VARIÂNCIA (ANOVA) APÓS A COLETA",
          sprintf("# Quando você preencher a(s) coluna(s) %s, execute (repita a analise para cada resposta):",
                  paste0("'", res$response_var, "'", collapse = ", ")),
          sprintf("# fit <- aov(%s ~ Tratamento, data = dados)", res$response_var[1]),
          "# summary(fit)"
        )
        
        # Se for DBC ou Split-Plot, ajustar o exemplo de ANOVA no script
        if (input$design_type == "DBC") {
          r_script_content <- c(r_script_content,
            sprintf("# fit_dbc <- aov(%s ~ Tratamento + factor(Bloco), data = dados)", res$response_var[1]),
            "# summary(fit_dbc)"
          )
        } else if (input$design_type == "DQL") {
          r_script_content <- c(r_script_content,
            sprintf("# fit_dql <- aov(%s ~ Tratamento + factor(Linha) + factor(Coluna), data = dados)", res$response_var[1]),
            "# summary(fit_dql)"
          )
        } else if (input$design_type == "fatorial") {
          r_script_content <- c(r_script_content,
            "# # Para fatorial, o tratamento é a combinação (Ex: Adubo-Irrigacao).",
            "# # Mas podemos separar os fatores no R para testar efeitos principais e interações:",
            "# # Separando os fatores:",
            "dados$Fator_A <- sapply(strsplit(dados$Tratamento, '-'), function(x) x[1])",
            "dados$Fator_B <- sapply(strsplit(dados$Tratamento, '-'), function(x) x[2])",
            sprintf("# fit_fat <- aov(%s ~ Fator_A * Fator_B, data = dados)", res$response_var[1]),
            "# summary(fit_fat)"
          )
        } else if (input$design_type == "split_plot") {
          r_script_content <- c(r_script_content,
            "# # Para parcelas subdivididas, usamos termo de erro para a parcela principal:",
            sprintf("# fit_sp <- aov(%s ~ Fator_Main * Fator_Sub + Error(factor(Bloco)/Fator_Main), data = dados)", res$response_var[1]),
            "# summary(fit_sp)"
          )
        }
        
        writeLines(r_script_content, file.path(dir_scripts, "analise_experimento.R"))
        
        # Copiar recursos do Quarto
        file.copy("templates/custom-reference.docx", file.path(dir_relatorios, "custom-reference.docx"), overwrite = TRUE)
        file.copy("templates/funcoes_experimental_design.R", file.path(dir_scripts, "funcoes_experimental_design.R"), overwrite = TRUE)
        file.copy("templates/funcoes_experimental_design.R", file.path(dir_relatorios, "funcoes_experimental_design.R"), overwrite = TRUE)
        file.copy("templates/relatorio_experimental_design.qmd", file.path(dir_relatorios, "relatorio_experimental_design.qmd"), overwrite = TRUE)
        
        # Salva dados do delineamento para uso local
        delineamento_res <- res
        design_type <- input$design_type
        save(delineamento_res, design_type, file = file.path(dir_relatorios, "delineamento_data.rda"))
        save(delineamento_res, design_type, file = file.path(proj_dir, "delineamento_data.rda"))
        
        # Adicionar o .Rproj
        rproj_content <- c("Version: 1.0", "RestoreWorkspace: Default", "SaveWorkspace: Default", "Encoding: UTF-8")
        writeLines(rproj_content, file.path(proj_dir, "projeto_experimento.Rproj"))
        
        # README.txt
        readme_content <- c(
          "PACOTE DE PLANEJAMENTO EXPERIMENTAL (CROQUI E SORTEIO)",
          "-----------------------------------------------------",
          "",
          "Este pacote contém a ficha de campo e os scripts para reproduzir o planejamento.",
          "",
          "ESTRUTURA DE ARQUIVOS:",
          "- projeto_experimento.Rproj      : Clique duplo para abrir no RStudio.",
          "- dados/dados_experimento.xlsx  : Planilha para inserção dos dados de resposta.",
          "- dados/dados_experimento.csv   : Ficha de dados em formato CSV.",
          "- scripts/analise_experimento.R : Script R para plotar o croqui e gerar tabelas.",
          "- relatorios/                   : Pasta com o arquivo Quarto (.qmd) do relatório.",
          "",
          "COMO PREENCHER E ANALISAR SEUS DADOS:",
          sprintf("1. Abra a planilha Excel 'dados/dados_experimento.xlsx' e insira os valores da(s) variável(is) de resposta %s para cada Unidade Experimental (UE).", paste0("'", res$response_var, "'", collapse = ", ")),
          "2. Salve o arquivo Excel.",
          "3. Para analisar os dados, abra o projeto no RStudio, carregue os dados do Excel e execute a ANOVA (Análise de Variância):",
          "   Execute o script 'scripts/analise_experimento.R' para ver exemplos prontos correspondentes ao seu delineamento!"
        )
        writeLines(readme_content, file.path(proj_dir, "README.txt"))
        
        # Zipar
        old_wd <- getwd()
        setwd(temp_dir)
        zip::zip(file, files = proj_dir_name)
        setwd(old_wd)
      }
    )
  })
}
