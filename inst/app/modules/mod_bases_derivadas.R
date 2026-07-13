# Gerenciador de Bases Derivadas — Fase 3A
# ---------------------------------------------------------------------------
# Interface para criar e administrar ramos em estrela. Nesta fase, o painel não
# altera a entrada dos módulos analíticos; o seletor de bases entra na Fase 3B.

library(shiny)
library(bslib)
library(DT)

mod_bases_derivadas_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(4, 8),
      card(
        card_header("Criar base derivada"),
        card_body(
          textInput(ns("nome_amigavel"), "Nome amigável:",
                    placeholder = "Ex.: Dados anuais para gráficos"),
          selectInput(ns("finalidade"), "Finalidade:", choices = bases_finalidades),
          textInput(ns("nome_r"), "Nome no código R:", value = "base_derivada"),
          textAreaInput(ns("descricao"), "Descrição (opcional):", rows = 3,
                        placeholder = "Explique por que esta preparação é específica."),
          div(class = "alert alert-info", style = "font-size:0.82rem; padding:9px 11px;",
              icon("diagram-project"),
              " Toda base derivada nasce diretamente de dados_analise. Ramos de ramos não são permitidos."),
          actionButton(ns("criar"), "Criar base derivada", icon = icon("plus"),
                       class = "btn-primary w-100")
        )
      ),
      navset_card_tab(
        nav_panel("Registro de Bases", icon = icon("list"),
                  card_body(DTOutput(ns("tabela")))),
        nav_panel("Prévia da base selecionada", icon = icon("table"),
                  card_body(DTOutput(ns("preview")))),
        nav_panel("Código do ramo", icon = icon("code"),
                  card_body(tags$pre(style = "white-space:pre-wrap; font-size:0.82rem;",
                                     verbatimTextOutput(ns("codigo")))))
      )
    ),
    layout_columns(
      col_widths = c(8, 4),
      card(
        card_header("Base selecionada"),
        card_body(uiOutput(ns("detalhes")))
      ),
      card(
        card_header("Ações"),
        card_body(
          div(class = "d-grid gap-2",
              actionButton(ns("renomear"), "Renomear / editar descrição",
                           icon = icon("pen"), class = "btn-outline-primary"),
              actionButton(ns("recalcular"), "Recalcular esta base",
                           icon = icon("arrows-rotate"), class = "btn-primary"),
              actionButton(ns("finalizar"), "Finalizar preparo",
                           icon = icon("circle-check"), class = "btn-success"),
              actionButton(ns("reabrir"), "Reabrir como rascunho",
                           icon = icon("rotate-left"), class = "btn-outline-secondary"),
              actionButton(ns("excluir"), "Excluir base derivada",
                           icon = icon("trash"), class = "btn-outline-danger")),
          helpText("Selecione uma linha no Registro de Bases. dados_analise nunca pode ser excluída aqui.")
        )
      )
    )
  )
}

mod_bases_derivadas_server <- function(id, dados_analise_rv, registro_bases_rv,
                                       cache_bases_rv, revisao_origem_rv = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    contador <- reactiveVal(1L)
    id_selecionada_rv <- reactiveVal(NULL)

    dados_raiz <- reactive({ req(dados_analise_rv()); as.data.frame(dados_analise_rv()) })
    registros <- reactive({ registro_bases_rv() %||% list() })
    caches <- reactive({ cache_bases_rv() %||% list() })
    revisao_atual <- reactive({
      if (is.function(revisao_origem_rv)) as.integer(revisao_origem_rv()) else 1L
    })

    proximo_id <- function() {
      usados <- vapply(registros(), `[[`, character(1), "id")
      repeat {
        n <- contador(); contador(n + 1L)
        id_novo <- sprintf("base_%04d", n)
        if (!(id_novo %in% usados)) return(id_novo)
      }
    }

    observeEvent(input$nome_amigavel, {
      atual <- isolate(input$nome_r %||% "")
      if (!nzchar(atual) || identical(atual, "base_derivada"))
        updateTextInput(session, "nome_r",
                        value = bases_sugerir_nome_r(input$nome_amigavel, input$finalidade))
    }, ignoreInit = TRUE)

    observeEvent(input$finalidade, {
      if (!nzchar(input$nome_amigavel %||% ""))
        updateTextInput(session, "nome_r",
                        value = bases_sugerir_nome_r("", input$finalidade))
    }, ignoreInit = TRUE)

    observeEvent(input$criar, {
      msg <- bases_validar_metadados(registros(), input$nome_amigavel, input$nome_r)
      if (!is.null(msg)) {
        showNotification(msg, type = "error", duration = 8)
        return()
      }
      revisao <- if (is.function(revisao_origem_rv)) revisao_origem_rv() else 1L
      nova <- bases_novo_registro(
        id = proximo_id(), nome_amigavel = input$nome_amigavel,
        nome_r = input$nome_r, finalidade = input$finalidade,
        descricao = input$descricao, revisao_origem = revisao
      )
      registro_bases_rv(bases_adicionar(registros(), nova))
      updateTextInput(session, "nome_amigavel", value = "")
      updateTextInput(session, "nome_r", value = "base_derivada")
      updateTextAreaInput(session, "descricao", value = "")
      showNotification(sprintf("Base '%s' criada como rascunho.", nova$nome_r),
                       type = "message", duration = 5)
    })

    # A tabela lê somente metadados do cache. Não executa replay de nenhum ramo.
    tabela_atual <- reactive({ bases_tabela(registros(), caches(), revisao_atual()) })

    output$tabela <- renderDT({
      tab <- tabela_atual()
      validate(need(nrow(tab) > 0, "Nenhuma base derivada criada. dados_analise continua sendo a base compartilhada."))
      datatable(tab, selection = "single", rownames = FALSE,
                options = list(pageLength = 8, scrollX = TRUE, dom = "tip"))
    })

    observeEvent(input$tabela_rows_selected, {
      linha <- input$tabela_rows_selected
      if (!length(linha)) return()
      tab <- tabela_atual()
      if (linha <= nrow(tab)) id_selecionada_rv(tab$ID[[linha]])
    })

    base_selecionada <- reactive({
      id <- id_selecionada_rv()
      if (is.null(id)) return(NULL)
      bases_obter(registros(), id)
    })

    cache_selecionado <- reactive({
      base <- base_selecionada()
      if (is.null(base)) return(NULL)
      bases_cache_obter(caches(), base$id)
    })

    output$preview <- renderDT({
      entrada <- cache_selecionado()
      validate(
        need(!is.null(base_selecionada()), "Selecione uma base no registro."),
        need(!is.null(entrada), "Esta base ainda não foi calculada. Clique em Recalcular esta base."),
        need(!is.null(entrada$df), "O último recálculo não produziu uma tabela válida.")
      )
      datatable(head(entrada$df, 300), rownames = FALSE,
                options = list(pageLength = 12, scrollX = TRUE))
    })

    output$codigo <- renderText({
      base <- base_selecionada()
      if (is.null(base)) return("# Selecione uma base no registro.")
      bases_codigo(base)
    })

    output$detalhes <- renderUI({
      base <- base_selecionada()
      if (is.null(base))
        return(div(style = "color:#777;", icon("circle-info"), " Selecione uma linha na tabela."))
      entrada <- cache_selecionado()
      estado_cache <- bases_estado_cache(base, entrada, revisao_atual())
      finalidade_rotulo <- names(bases_finalidades)[match(base$finalidade, bases_finalidades)]
      classe_badge <- switch(estado_cache,
        "Atualizada" = "bg-success",
        "Desatualizada" = "bg-warning text-dark",
        "Com erro" = "bg-danger",
        "bg-secondary")
      tagList(
        tags$dl(class = "row mb-0",
          tags$dt(class = "col-sm-3", "Nome"), tags$dd(class = "col-sm-9", base$nome_amigavel),
          tags$dt(class = "col-sm-3", "Objeto R"), tags$dd(class = "col-sm-9", tags$code(base$nome_r)),
          tags$dt(class = "col-sm-3", "Origem"), tags$dd(class = "col-sm-9", tags$code("dados_analise")),
          tags$dt(class = "col-sm-3", "Finalidade"), tags$dd(class = "col-sm-9", finalidade_rotulo %||% base$finalidade),
          tags$dt(class = "col-sm-3", "Estado"), tags$dd(class = "col-sm-9", strong(base$estado)),
          tags$dt(class = "col-sm-3", "Cache"),
          tags$dd(class = "col-sm-9", tags$span(class = paste("badge", classe_badge), estado_cache)),
          tags$dt(class = "col-sm-3", "Etapas"), tags$dd(class = "col-sm-9", length(base$etapas %||% list())),
          tags$dt(class = "col-sm-3", "Dimensões"),
          tags$dd(class = "col-sm-9", if (is.null(entrada) || is.null(entrada$df)) "—" else
                    sprintf("%d linhas × %d colunas", nrow(entrada$df), ncol(entrada$df))),
          tags$dt(class = "col-sm-3", "Última tentativa"),
          tags$dd(class = "col-sm-9", if (is.null(entrada$calculada_em)) "—" else
                    format(entrada$calculada_em, "%d/%m/%Y %H:%M:%S")),
          tags$dt(class = "col-sm-3", "Prévia válida"),
          tags$dd(class = "col-sm-9", if (is.null(entrada$resultado_em)) "—" else
                    format(entrada$resultado_em, "%d/%m/%Y %H:%M:%S")),
          tags$dt(class = "col-sm-3", "Descrição"), tags$dd(class = "col-sm-9", if (nzchar(base$descricao)) base$descricao else "—")
        ),
        if (identical(estado_cache, "Desatualizada"))
          div(class = "alert alert-warning mt-2 mb-0", style = "font-size:0.82rem; padding:8px 10px;",
              "dados_analise ou a receita mudou. A última prévia foi preservada, mas esta base não poderá alimentar análises antes de ser recalculada."),
        if (identical(estado_cache, "Com erro"))
          div(class = "alert alert-danger mt-2 mb-0", style = "font-size:0.82rem; padding:8px 10px;",
              strong("Erro isolado neste ramo: "), paste(unlist(entrada$erros), collapse = "; "),
              if (isTRUE(entrada$preview_anterior))
                tags$span(" A tabela exibida é a última prévia válida e não poderá alimentar análises.")),
        if (!length(base$etapas %||% list()))
          div(class = "alert alert-warning mt-2 mb-0", style = "font-size:0.82rem; padding:8px 10px;",
              "Este ramo ainda é idêntico a dados_analise. A edição das transformações específicas entra na Fase 3B.")
      )
    })

    observeEvent(input$recalcular, {
      base <- base_selecionada()
      if (is.null(base)) { showNotification("Selecione uma base.", type = "warning"); return() }
      anterior <- bases_cache_obter(caches(), base$id)
      entrada <- bases_recalcular_cache(
        dados_raiz(), base, revisao_atual(), entrada_anterior = anterior
      )
      cache_bases_rv(bases_cache_gravar(caches(), base$id, entrada))
      if (length(entrada$erros)) {
        showNotification(
          paste("O ramo foi isolado com erro:", paste(unlist(entrada$erros), collapse = "; ")),
          type = "error", duration = 12
        )
      } else {
        showNotification(sprintf("Base '%s' recalculada: %d linhas × %d colunas.",
                                 base$nome_r, entrada$linhas, entrada$colunas),
                         type = "message", duration = 5)
      }
    })

    observeEvent(input$finalizar, {
      base <- base_selecionada()
      if (is.null(base)) { showNotification("Selecione uma base.", type = "warning"); return() }
      novo <- tryCatch(
        bases_finalizar(registros(), base$id, caches(), revisao_atual()),
        error = function(e) e
      )
      if (inherits(novo, "error")) {
        showNotification(conditionMessage(novo), type = "error", duration = 10)
      } else {
        registro_bases_rv(novo)
        showNotification(sprintf("Base '%s' marcada como pronta.", base$nome_r), type = "message")
      }
    })

    observeEvent(input$reabrir, {
      base <- base_selecionada()
      if (is.null(base)) { showNotification("Selecione uma base.", type = "warning"); return() }
      registro_bases_rv(bases_reabrir(registros(), base$id))
      showNotification(sprintf("Base '%s' reaberta como rascunho.", base$nome_r), type = "message")
    })

    observeEvent(input$renomear, {
      base <- base_selecionada()
      if (is.null(base)) { showNotification("Selecione uma base.", type = "warning"); return() }
      showModal(modalDialog(
        title = "Renomear base derivada",
        textInput(ns("editar_nome_amigavel"), "Nome amigável:", value = base$nome_amigavel),
        textInput(ns("editar_nome_r"), "Nome no código R:", value = base$nome_r),
        textAreaInput(ns("editar_descricao"), "Descrição:", value = base$descricao, rows = 3),
        footer = tagList(modalButton("Cancelar"),
                         actionButton(ns("confirmar_renomear"), "Salvar", class = "btn-primary"))
      ))
    })

    observeEvent(input$confirmar_renomear, {
      base <- base_selecionada()
      if (is.null(base)) { removeModal(); return() }
      novo <- tryCatch(
        bases_renomear(registros(), base$id, input$editar_nome_amigavel,
                       input$editar_nome_r, input$editar_descricao),
        error = function(e) e
      )
      if (inherits(novo, "error")) {
        showNotification(conditionMessage(novo), type = "error", duration = 8)
      } else {
        registro_bases_rv(novo)
        removeModal()
        showNotification("Metadados da base atualizados.", type = "message")
      }
    })

    observeEvent(input$excluir, {
      base <- base_selecionada()
      if (is.null(base)) { showNotification("Selecione uma base.", type = "warning"); return() }
      showModal(modalDialog(
        title = "Excluir base derivada?",
        tags$p("Esta ação remove apenas o ramo ", tags$code(base$nome_r),
               ". A base compartilhada dados_analise não será alterada."),
        tags$p(strong("A exclusão não pode ser desfeita nesta sessão.")),
        footer = tagList(modalButton("Cancelar"),
                         actionButton(ns("confirmar_excluir"), "Excluir base",
                                      icon = icon("trash"), class = "btn-danger"))
      ))
    })

    observeEvent(input$confirmar_excluir, {
      base <- base_selecionada()
      if (is.null(base)) { removeModal(); return() }
      registro_bases_rv(bases_excluir(registros(), base$id))
      cache_bases_rv(bases_cache_excluir(caches(), base$id))
      id_selecionada_rv(NULL)
      removeModal()
      showNotification(sprintf("Base '%s' excluída.", base$nome_r), type = "message")
    })

    disponiveis <- reactive({
      bases_disponiveis_analise(registros(), caches(), revisao_atual())
    })
    invisible(list(registro = registros, cache = caches, disponiveis = disponiveis,
                   selecionada = base_selecionada))
  })
}
