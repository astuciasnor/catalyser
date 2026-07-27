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
    class = "mb-2 catalyser-base-selector",
    fill = FALSE,
    card_body(
      style = "padding:8px 12px;",
      div(
        class = "row g-2 align-items-start",
        div(
          class = "col-12",
          selectizeInput(
            ns("base_id"), "Base utilizada:",
            choices = c("Base compartilhada — dados_analise" = "dados_analise"),
            width = "100%",
            options = list(maxOptions = 200)
          )
        ),
        div(
          class = "col-12",
          uiOutput(ns("status"))
        )
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
      updateSelectizeInput(session, "base_id", choices = escolhas, selected = atual)
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
      status_atual <- if (isTRUE(base$derivada)) {
        div(class = "alert alert-info", style = estilo,
            icon("diagram-project"), " ", strong(base$nome_amigavel), " — ",
            tags$code(base$base_objeto),
            sprintf(" — %d linhas × %d colunas", nrow(base$df), ncol(base$df)))
      } else {
        div(class = "alert alert-light border", style = estilo,
            icon("database"), " ", tags$code("dados_analise"), " — base compartilhada")
      }

      ids_disponiveis <- unname(opcoes())
      pendentes <- Filter(
        function(item) !item$id %in% ids_disponiveis,
        registros()
      )
      aviso_pendentes <- if (length(pendentes)) {
        motivos <- vapply(pendentes, function(item) {
          estado_cache <- bases_estado_cache(
            item, bases_cache_obter(caches(), item$id), revisao()
          )
          acao <- if (!identical(estado_cache, "Atualizada")) {
            "falta Recalcular e Finalizar"
          } else if (!identical(item$estado, "pronta")) {
            "falta Finalizar preparo"
          } else {
            sprintf("cache %s", tolower(estado_cache))
          }
          sprintf("%s: %s", item$nome_amigavel, acao)
        }, character(1))
        div(
          class = "alert alert-warning",
          style = "font-size:0.76rem; padding:7px 9px; margin:6px 0 0;",
          icon("triangle-exclamation"), " ",
          strong("Base(s) ainda fora da lista: "),
          paste0(paste(motivos, collapse = "; "), ".")
        )
      }
      tagList(status_atual, aviso_pendentes)
    })

    invisible(list(dados = dados, contexto = contexto, opcoes = opcoes))
  })
}
