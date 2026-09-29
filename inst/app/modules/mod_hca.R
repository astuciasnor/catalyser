# Módulo de Análise de Agrupamento Hierárquico (AAH / HCA) para IDE_R
library(shiny)
library(bslib)
library(ggplot2)
library(DT)

if (file.exists("templates/funcoes_hca.R")) {
  source("templates/funcoes_hca.R")
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
            actionButton(ns("sel_all_num"), "Selecionar todas as numéricas",
                         icon = icon("list-check"), class = "btn-outline-secondary btn-sm w-100 mb-2"),
            selectInput(ns("distance_method"), "Métrica de Distância:",
                        choices = c("Euclidiana" = "euclidean",
                                    "Manhattan (City-Block)" = "manhattan",
                                    "Jaccard / binária (presença-ausência)" = "binary"),
                        selected = "euclidean"),
            selectInput(ns("linkage_method"), "Método de Ligação:",
                        choices = c("Ward.D2 (Mínima Variância)" = "ward.D2",
                                    "Completa (Complete Linkage)" = "complete",
                                    "Simples (Single Linkage)" = "single",
                                    "Média (UPGMA)" = "average"),
                        selected = "ward.D2"),
            sliderInput(ns("k_groups"), "Número de Grupos (k):",
                        min = 2, max = 8, value = 3, step = 1),
            checkboxInput(ns("scale"), "Padronizar Variáveis (Z-Score)", value = TRUE),
            selectInput(ns("label_var"), "Rótulo das folhas do dendrograma:", choices = NULL),
            helpText(HTML("A AAH agrupa as <b>linhas</b> (observações). Em matriz espécie × local: marque os <b>locais</b> como variáveis, escolha <b>Jaccard</b> e ponha a coluna de <b>espécies</b> em \"Rótulo das folhas\". Jaccard/binária é para presença-ausência (não padroniza).")),
            execucao_explicita_controles_ui(ns)
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
      
      # COLUNA 2: ABAS DE RESULTADOS (PRINCIPAL)
      execucao_explicita_resultados_ui(ns, navset_card_tab(
        id = ns("active_tab"),
        title = "Painel de Resultados da AAH:",
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
      )),
      
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
    revisao_execucao <- execucao_revisao_dados(data_rv)
    gatilho_execucao <- reactiveVal(0L)
    
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

    # Botao: selecionar todas as numericas
    observeEvent(input$sel_all_num, {
      df <- data_rv(); req(df)
      num_cols <- names(df)[sapply(df, is.numeric)]
      updateCheckboxGroupInput(session, "vars_selected", selected = num_cols)
    })
    # Popular o seletor de rotulo das folhas (todas as colunas; "" = numero da linha)
    observe({
      df <- data_rv(); req(df)
      updateSelectInput(session, "label_var",
                        choices = c("(numero da linha)" = "", stats::setNames(names(df), names(df))),
                        selected = isolate(input$label_var))
    })
    
    assinatura_execucao <- reactive({
      req(length(input$vars_selected) >= 2)
      execucao_assinatura(
        input,
        c("vars_selected", "distance_method", "linkage_method", "k_groups",
          "scale", "label_var"),
        revisao_execucao()
      )
    })

    # Executa a AAH apenas após confirmação explícita.
    result_rv <- eventReactive(gatilho_execucao(), {
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
        scale = input$scale,
        label_var = input$label_var
      )
    }, ignoreInit = FALSE)

    exec_ctrl <- execucao_explicita_server(
      input, output, session, assinatura_execucao, result_rv,
      nome_analise = "A análise de agrupamentos",
      gatilho_rv = gatilho_execucao
    )
    
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
      dist_nome <- switch(r$distance_method, euclidean = "Euclidiana",
                          manhattan = "Manhattan", binary = "Jaccard (binária)", r$distance_method)
      
      plot(
        r$fit,
        labels = lbls,
        hang = -1,
        main = "Dendrograma de Agrupamento Hierárquico",
        sub = paste0("Distância: ", dist_nome, " | Ligação: ", r$linkage_method),
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
    

    estado_execucao <- reactive({
      req(exec_ctrl$atualizada())
      r <- result_rv()
      req(r)
      list(
        analise_id = "hca",
        tipo = "hca",
        titulo = paste("Análise de agrupamentos:", paste(input$vars_selected, collapse = ", ")),
        parametros = list(
          variaveis = input$vars_selected,
          distancia = input$distance_method,
          ligacao = input$linkage_method,
          numero_grupos = input$k_groups,
          padronizar = isTRUE(input$scale),
          variavel_rotulo = input$label_var,
          mostrar_rotulos = isTRUE(input$show_labels)
        ),
        saidas_disponiveis = c("narrativa", "tabela", "grafico", "diagnosticos"),
        resultado_resumo = list(
          n = r$N,
          n_total = r$N_total,
          numero_grupos = r$k_groups,
          tamanhos_grupos = as.list(table(r$clusters))
        )
      )
    })

    invisible(list(
      resultado = result_rv,
      estado_execucao = estado_execucao,
      estado_execucao_ui = exec_ctrl$estado,
      execucao_atualizada = exec_ctrl$atualizada
    ))
  })
}
