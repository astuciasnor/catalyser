# Seletor reutilizável de base para módulos analíticos — Fase 3B.3
# ---------------------------------------------------------------------------
# Resolve dados_analise ou um ramo pronto/atualizado sem executar replay. O
# módulo analítico recebe apenas a reactive `dados`; `contexto` preserva o ID e
# o nome R para o futuro registro de execuções.

library(shiny)
library(bslib)

mod_seletor_base_analise_ui <- function(id) {
  ns <- NS(id)
  card(
    class = "mb-2",
    card_body(
      style = "padding:8px 12px;",
      layout_columns(
        col_widths = c(5, 7),
        selectInput(ns("base_id"), "Base utilizada:",
                    choices = c("Base compartilhada — dados_analise" = "dados_analise")),
        uiOutput(ns("status"))
      )
    )
  )
}

mod_seletor_base_analise_server <- function(id, dados_analise_rv, registro_bases_rv,
                                            cache_bases_rv, revisao_origem_rv,
                                            finalidade_preferida = NULL,
                                            nome_analise = "Esta análise") {
  moduleServer(id, function(input, output, session) {
    registros <- reactive({ registro_bases_rv() %||% list() })
    caches <- reactive({ cache_bases_rv() %||% list() })
    revisao <- reactive({ as.integer(revisao_origem_rv()) })

    opcoes <- reactive({
      bases_opcoes_analise(
        registros(), caches(), revisao(),
        finalidade_preferida = finalidade_preferida
      )
    })

    observe({
      escolhas <- opcoes()
      atual <- isolate(input$base_id %||% "dados_analise")
      if (!atual %in% unname(escolhas)) {
        if (!identical(atual, "dados_analise"))
          showNotification(
            sprintf("%s voltou para dados_analise porque a base escolhida deixou de estar pronta e atualizada.",
                    nome_analise),
            type = "warning", duration = 10
          )
        atual <- "dados_analise"
      }
      updateSelectInput(session, "base_id", choices = escolhas, selected = atual)
    })

    contexto <- reactive({
      raiz <- dados_analise_rv()
      req(raiz)
      chave <- input$base_id %||% "dados_analise"
      if (!chave %in% unname(opcoes())) chave <- "dados_analise"
      resolvida <- tryCatch(
        bases_resolver_analise(chave, as.data.frame(raiz), registros(), caches(), revisao()),
        error = function(e) e
      )
      if (inherits(resolvida, "error"))
        validate(need(FALSE, conditionMessage(resolvida)))
      resolvida
    })

    dados <- reactive({ contexto()$df })

    output$status <- renderUI({
      base <- contexto()
      estilo <- "font-size:0.78rem; padding:7px 9px; margin:20px 0 0;"
      if (isTRUE(base$derivada)) {
        div(class = "alert alert-info", style = estilo,
            icon("diagram-project"), " ", tags$code(base$base_objeto),
            sprintf(" — %d linhas × %d colunas", nrow(base$df), ncol(base$df)))
      } else {
        div(class = "alert alert-light border", style = estilo,
            icon("database"), " ", tags$code("dados_analise"), " — base compartilhada")
      }
    })

    invisible(list(dados = dados, contexto = contexto, opcoes = opcoes))
  })
}
