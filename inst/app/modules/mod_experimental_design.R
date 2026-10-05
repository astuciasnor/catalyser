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

mod_experimental_design_ui <- function(id, variaveis_ui = NULL, tipo_fixo = NULL) {
  ns <- NS(id)
  escolhas_tipo <- c("DIC (Inteiramente Casualizado)" = "DIC",
                     "DBC (Blocos Casualizados)" = "DBC",
                     "DQL (Quadrado Latino)" = "DQL",
                     "Fatorial (em DIC)" = "fatorial",
                     "Parcelas Subdivididas (Split-Plot)" = "split_plot")
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
            if (is.null(tipo_fixo)) {
              selectInput(ns("design_type"), "Tipo de Delineamento", choices = escolhas_tipo)
            } else {
              # A mesma entrada mantém o servidor comum; no menu aberto, o
              # tipo vem do item clicado e não aparece como um segundo seletor.
              tags$div(style = "display:none;",
                selectInput(ns("design_type"), "Tipo de Delineamento", choices = escolhas_tipo,
                            selected = tipo_fixo)
              )
            },
            
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
              helpText("Linhas e colunas: K x K, necessariamente iguais ao número de tratamentos. O DQL não permite escolher medidas diferentes sem deixar de ser um quadrado latino.")
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
          title = "Croqui",
          icon = icon("table-cells"),
          card_body(
            style = "padding: 10px 15px;",
            plotOutput(ns("plot_croqui"), height = "450px")
          )
        ),
        if (!is.null(variaveis_ui)) nav_panel(
          title = "Variáveis do experimento",
          icon = icon("list-check"),
          card_body(
            style = "padding: 10px 15px;",
            p("Depois de gerar o croqui, descreva as variáveis que serão medidas. Tratamento, bloco e unidade experimental devem continuar identificáveis na planilha."),
            variaveis_ui
          )
        ),
        nav_panel(
          title = "Ficha de campo",
          icon = icon("table"),
          card_body(
            style = "padding: 10px 15px;",
            DTOutput(ns("table_unidades"))
          )
        ),
        nav_panel(
          title = "Descrição",
          icon = icon("file-lines"),
          card_body(
            style = "padding: 10px 15px;",
            uiOutput(ns("report_descritivo")),
            if (identical(tipo_fixo, "split_plot")) tags$details(class = "small mt-2",
              tags$summary("Exemplo híbrido: ração e reservatório"),
              p(class = "mt-1 mb-0", "Rações sorteadas entre tanques dentro de cada reservatório formam o componente experimental. O reservatório preexistente é um componente observacional: sua comparação é associativa. Para estimar a interação, cada ração precisa de mais de um tanque em cada reservatório. Esse exemplo não é gerado pelo croqui clássico de parcelas subdivididas."))
          )
        )
      ),
      
      # COLUNA 3: EXPORTAÇÃO
      div(
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
          # Guarda o tipo para orientar a análise futura sem precisar reconstruir o croqui.
          res$design_type <- type
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

    # A escolha do tipo também atualiza o croqui, a ficha e a descrição. Os
    # controles visíveis já mudam no navegador; esta reinicialização aplica os
    # valores próprios do novo delineamento à sua estrutura gerada.
    observeEvent(input$design_type, {
      delineamento_rv(NULL)
    }, ignoreInit = TRUE)
    
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
    
    # Devolve o croqui para que o planejamento de variáveis reutilize fator, bloco e repetição.
    delineamento_rv
  })
}
