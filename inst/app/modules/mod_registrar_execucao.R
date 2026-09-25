# Controles reutilizáveis para registrar uma prévia analítica — Fase 3C

library(shiny)
library(bslib)

mod_analise_registravel_ui <- function(id, analise_ui, registro_ui) {
  ns <- NS(id)
  navset_card_tab(
    id = ns("fluxo_registro_subabas"),
    nav_panel(
      title = "1. Configurar e executar",
      icon = icon("sliders"),
      div(
        class = "pt-2",
        analise_ui,
        div(
          class = "alert alert-light border small mt-3 mb-1",
          icon("arrow-right"), " ",
          "Depois de executar a análise, abra a sub-aba ",
          strong("2. Adicionar ao Projeto R"), "."
        )
      )
    ),
    nav_panel(
      title = "2. Adicionar ao Projeto R",
      icon = icon("bookmark"),
      div(
        class = "pt-2",
        registro_ui
      )
    )
  )
}

mod_registrar_execucao_ui <- function(id) {
  ns <- NS(id)
  card(
    class = "mb-2",
    style = "border-left:4px solid #2E7D8F;",
    card_header(
      div(class = "d-flex justify-content-between align-items-center",
          span(icon("bookmark"), " Registrar execução"),
          uiOutput(ns("contador")))
    ),
    card_body(
      style = "padding:10px 14px;",
      layout_columns(
        col_widths = c(7, 5),
        div(
          textInput(ns("titulo"), "Título da execução:", value = ""),
          helpText("Dê um nome que indique a resposta e os grupos comparados, por exemplo: Comprimento do bico entre espécies de pinguins. Na exportação de uma única ANOVA de um fator, esse será o título do relatório.")
        ),
        uiOutput(ns("dependencia"))
      ),
      selectInput(ns("execucao_id"), "Execução selecionada:",
                  choices = c("Nova execução" = "")),
      uiOutput(ns("gerenciamento")),
      uiOutput(ns("detalhes")),
      helpText(
        "Mudar variáveis altera somente a prévia. O registro só muda quando você adicionar ou atualizar explicitamente.",
        style = "font-size:0.78rem; margin:5px 0 0;"
      )
    )
  )
}

mod_registrar_execucao_server <- function(id, estado_execucao_rv, base_contexto_rv,
                                          registro_execucoes_rv, contador_execucoes_rv,
                                          revisao_origem_rv, registro_bases_rv,
                                          cache_bases_rv, analise_id,
                                          nome_analise = "Esta análise",
                                          nova_configuracao_rv = NULL) {
  moduleServer(id, function(input, output, session) {
    selecionada_rv <- reactiveVal("")
    ultimo_sugerido_rv <- reactiveVal("")

    # Nas áreas com histórico, outra prévia significa uma nova análise por padrão.
    # Os módulos antigos mantêm a seleção explícita e o comportamento já aprovado.
    if (!is.null(nova_configuracao_rv)) observeEvent(nova_configuracao_rv(), {
      selecionada_rv("")
      updateSelectInput(session, "execucao_id", selected = "")
      estado <- estado_execucao_rv()
      updateTextInput(session, "titulo", value = if (is.null(estado)) "" else estado$titulo)
    }, ignoreNULL = FALSE, priority = 10)

    execucoes_modulo <- reactive({
      execucoes_da_analise(registro_execucoes_rv(), analise_id)
    })

    estado_disponivel <- reactive({
      tryCatch(estado_execucao_rv(), error = function(e) NULL)
    })

    estado_seguro <- function() {
      tryCatch(
        isolate(estado_execucao_rv()),
        error = function(e) {
          mensagem <- conditionMessage(e)
          if (!nzchar(mensagem)) mensagem <- "Complete os parâmetros e obtenha uma prévia válida antes de registrar."
          showNotification(mensagem, type = "warning", duration = 8)
          NULL
        }
      )
    }

    contexto_seguro <- function(estado) {
      if (is.list(estado$base_contexto_override)) return(estado$base_contexto_override)
      tryCatch(
        isolate(base_contexto_rv()),
        error = function(e) {
          showNotification("A base desta análise não está disponível.", type = "warning", duration = 8)
          NULL
        }
      )
    }

    observe({
      estado <- tryCatch(estado_execucao_rv(), error = function(e) NULL)
      if (is.null(estado) || is.null(estado$titulo)) return()
      novo <- estado$titulo
      anterior <- ultimo_sugerido_rv()
      atual <- isolate(input$titulo)
      if (is.null(atual) || !nzchar(trimws(atual)) || identical(atual, anterior))
        updateTextInput(session, "titulo", value = novo)
      ultimo_sugerido_rv(novo)
    })

    output$contador <- renderUI({
      n <- length(execucoes_modulo())
      span(class = "badge text-bg-info", sprintf("%d registrada%s", n, if (n == 1L) "" else "s"))
    })

    # O seletor permanece montado. Recriá-lo a cada seleção fazia o navegador
    # alternar entre a execução nova e a anterior, redesenhando o cartão.
    observe({
      execs <- execucoes_modulo()
      escolhas <- c("Nova execução" = "")
      if (length(execs)) {
        ids <- vapply(execs, `[[`, character(1), "id")
        rotulos <- vapply(execs, execucoes_rotulo, character(1))
        escolhas <- c(escolhas, stats::setNames(ids, rotulos))
      }
      atual <- selecionada_rv()
      if (!atual %in% unname(escolhas)) atual <- ""
      freezeReactiveValue(input, "execucao_id")
      updateSelectInput(session, "execucao_id", choices = escolhas, selected = atual)
    })

    output$gerenciamento <- renderUI({
      pronto <- !is.null(estado_disponivel())
      atual <- selecionada_rv()
      botao <- function(id, rotulo, icone, classe, habilitado = TRUE) {
        tag <- actionButton(session$ns(id), rotulo, icon = icon(icone), class = classe)
        if (!isTRUE(habilitado))
          tag <- tagAppendAttributes(tag, disabled = "disabled",
                                     title = "Execute a análise com a configuração atual primeiro.")
        tag
      }
      tagList(
        if (!nzchar(atual)) {
          botao("adicionar", "Adicionar Novo Resultado", "plus", "btn-success", pronto)
        } else {
          div(class = "d-flex gap-2 flex-wrap",
              botao("atualizar", "Atualizar Resultado Anterior", "rotate", "btn-primary", pronto),
              botao("salvar_novo", "Adicionar Novo Resultado", "copy", "btn-outline-success", pronto),
              botao("remover", "Apagar Resultado Selecionado", "trash", "btn-outline-danger", TRUE))
        },
        if (!pronto) div(
          class = "small text-warning mt-2",
          icon("triangle-exclamation"),
          " Execute a análise com a configuração atual antes de adicionar ou atualizar."
        )
      )
    })

    observeEvent(input$execucao_id, {
      id_atual <- input$execucao_id %||% ""
      selecionada_rv(id_atual)
      if (nzchar(id_atual)) {
        execucao <- execucoes_obter(registro_execucoes_rv(), id_atual)
        if (!is.null(execucao)) updateTextInput(session, "titulo", value = execucao$titulo)
      } else {
        sugerido <- ultimo_sugerido_rv()
        if (nzchar(sugerido)) updateTextInput(session, "titulo", value = sugerido)
      }
    }, ignoreInit = TRUE)

    registrar_nova <- function() {
      estado <- estado_seguro()
      if (is.null(estado)) return()
      contexto <- contexto_seguro(estado)
      if (is.null(contexto)) return()
      proximo <- as.integer(contador_execucoes_rv()) + 1L
      id_novo <- sprintf("execucao_%04d", proximo)
      nova <- tryCatch(
        execucoes_criar(
          id_novo, estado, contexto, revisao_origem_rv(),
          titulo = input$titulo %||% estado$titulo
        ),
        error = function(e) e
      )
      if (inherits(nova, "error")) {
        showNotification(conditionMessage(nova), type = "error", duration = 8)
        return()
      }
      campos <- c("analise_id", "tipo", "titulo", "parametros", "base_id", "base_tipo",
                  "base_versao_receita", "revisao_origem", "codigo_r", "saidas_disponiveis")
      iguais <- Filter(function(x) identical(x[campos], nova[campos]), execucoes_modulo())
      if (length(iguais)) {
        selecionada_rv(iguais[[1]]$id)
        showNotification("Esta análise já foi adicionada. O resultado existente foi selecionado.",
                         type = "message", duration = 5)
        return()
      }
      registro_execucoes_rv(execucoes_adicionar(registro_execucoes_rv(), nova))
      contador_execucoes_rv(proximo)
      selecionada_rv(id_novo)
      showNotification(sprintf("%s registrada como %s.", nome_analise, id_novo),
                       type = "message", duration = 5)
    }

    observeEvent(input$adicionar, registrar_nova())
    observeEvent(input$salvar_novo, registrar_nova())

    observeEvent(input$atualizar, {
      id_atual <- selecionada_rv()
      if (!nzchar(id_atual)) return()
      estado <- estado_seguro()
      if (is.null(estado)) return()
      contexto <- contexto_seguro(estado)
      if (is.null(contexto)) return()
      novo_registro <- tryCatch(
        execucoes_atualizar(
          registro_execucoes_rv(), id_atual, estado, contexto,
          revisao_origem_rv(), titulo = input$titulo %||% estado$titulo
        ),
        error = function(e) e
      )
      if (inherits(novo_registro, "error")) {
        showNotification(conditionMessage(novo_registro), type = "error", duration = 8)
        return()
      }
      registro_execucoes_rv(novo_registro)
      showNotification(sprintf("%s atualizada.", id_atual), type = "message", duration = 5)
    })

    observeEvent(input$remover, {
      id_atual <- selecionada_rv()
      execucao <- execucoes_obter(registro_execucoes_rv(), id_atual)
      if (is.null(execucao)) return()
      showModal(modalDialog(
        title = "Remover execução registrada?",
        p(tags$code(id_atual), " — ", execucao$titulo),
        p("A prévia atual da análise não será alterada."),
        footer = tagList(
          modalButton("Cancelar"),
          actionButton(session$ns("confirmar_remocao"), "Remover",
                       class = "btn-danger", icon = icon("trash"))
        ),
        easyClose = TRUE
      ))
    })

    observeEvent(input$confirmar_remocao, {
      id_atual <- selecionada_rv()
      registro_execucoes_rv(execucoes_remover(registro_execucoes_rv(), id_atual))
      selecionada_rv("")
      removeModal()
      showNotification(sprintf("%s removida do registro.", id_atual),
                       type = "message", duration = 5)
    })

    output$dependencia <- renderUI({
      id_atual <- selecionada_rv()
      if (!nzchar(id_atual)) {
        pronta <- !is.null(estado_disponivel())
        return(div(class = "alert alert-light border", style = "font-size:0.78rem; margin:20px 0 0; padding:7px 9px;",
                   icon(if (pronta) "eye" else "play"),
                   if (pronta) " Prévia executada, ainda não registrada" else " Aguardando execução da análise"))
      }
      execucao <- execucoes_obter(registro_execucoes_rv(), id_atual)
      estado <- execucoes_estado_dependencia(
        execucao, registro_bases_rv(), cache_bases_rv(), revisao_origem_rv()
      )
      classe <- if (identical(estado, "Atualizada")) "alert-success" else "alert-warning"
      div(class = paste("alert", classe), style = "font-size:0.78rem; margin:20px 0 0; padding:7px 9px;",
          icon(if (identical(estado, "Atualizada")) "check" else "triangle-exclamation"),
          " ", tags$code(id_atual), " — ", estado)
    })

    output$detalhes <- renderUI({
      id_atual <- selecionada_rv()
      execucao <- execucoes_obter(registro_execucoes_rv(), id_atual)
      if (is.null(execucao)) return(NULL)
      saidas <- unname(execucoes_rotulos_saidas[execucao$saidas_disponiveis])
      div(
        class = "alert alert-light border py-2 mt-2 mb-1",
        style = "font-size:0.78rem;",
        strong("Base: "), tags$code(execucao$base_objeto),
        " | ", strong("Saídas disponíveis: "), paste(saidas, collapse = ", "),
        " | ", strong("Versão: "), execucao$versao
      )
    })

    invisible(list(
      execucoes = execucoes_modulo,
      selecionada = reactive(execucoes_obter(registro_execucoes_rv(), selecionada_rv()))
    ))
  })
}
