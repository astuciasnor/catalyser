# Contrato reutilizável de execução explícita — Fase 3C.1
# ---------------------------------------------------------------------------
# A configuração da análise continua reativa, mas o cálculo só acontece após
# o clique em "Executar análise". A assinatura executada congela a revisão da
# base e os parâmetros; qualquer alteração posterior deixa o rascunho pendente.

execucao_explicita_controles_ui <- function(ns, ativo = TRUE) {
  if (!isTRUE(ativo)) return(NULL)
  tagList(
    hr(style = "margin: 10px 0;"),
    actionButton(
      ns("executar_analise"), "Executar análise",
      icon = icon("play"), class = "btn-primary w-100",
      onmousedown = HTML(sprintf(
        paste0(
          "var botao=this;",
          "document.querySelectorAll('[id^=\"%s\"]').forEach(function(el){",
          "if(el!==botao && el.classList.contains('shiny-bound-input') && window.jQuery){",
          "window.jQuery(el).trigger('change');",
          "}",
          "});"
        ),
        ns("")
      )),
      onclick = HTML(sprintf(
        paste0(
          "event.preventDefault();",
          "event.stopPropagation();",
          "event.stopImmediatePropagation();",
          "var inputId='%s';",
          "window.setTimeout(function(){",
          "if(window.Shiny){",
          "Shiny.setInputValue(inputId, Date.now(), {priority:'event'});",
          "}",
          "},150);",
          "return false;"
        ),
        ns("executar_analise")
      ))
    ),
    uiOutput(ns("execucao_status"))
  )
}

execucao_explicita_resultados_ui <- function(ns, conteudo, ativo = TRUE) {
  if (!isTRUE(ativo)) return(conteudo)
  div(
    uiOutput(ns("execucao_painel_aviso")),
    conditionalPanel(
      condition = sprintf("output['%s'] === 'atualizada'", ns("execucao_estado_cliente")),
      conteudo
    )
  )
}

execucao_explicita_downloads_ui <- function(ns, conteudo, ativo = TRUE) {
  if (!isTRUE(ativo)) return(conteudo)
  tagList(
    conditionalPanel(
      condition = sprintf("output['%s'] !== 'atualizada'", ns("execucao_estado_cliente")),
      div(
        class = "alert alert-light border py-2 small mb-0",
        icon("lock"), " Execute a análise para liberar os arquivos desta configuração."
      )
    ),
    conditionalPanel(
      condition = sprintf("output['%s'] === 'atualizada'", ns("execucao_estado_cliente")),
      conteudo
    )
  )
}

execucao_revisao_dados <- function(data_rv) {
  revisao <- reactiveVal(0L)
  observeEvent(data_rv(), {
    revisao(as.integer(revisao()) + 1L)
  }, ignoreInit = FALSE, priority = 1000)
  reactive(revisao())
}

execucao_assinatura <- function(input, ids, revisao_dados) {
  valores <- lapply(ids, function(id) input[[id]])
  names(valores) <- ids
  list(revisao_dados = as.integer(revisao_dados), parametros = valores)
}

execucao_explicita_server <- function(input, output, session, assinatura_rv,
                                      resultado_rv, nome_analise = "A análise",
                                      gatilho_rv = NULL) {
  assinatura_executada_rv <- reactiveVal(NULL)
  tentativa_execucao_rv <- reactiveVal(0L)

  estado <- reactive({
    executada <- assinatura_executada_rv()
    if (is.null(executada)) return("aguardando")
    atual <- tryCatch(assinatura_rv(), error = function(e) NULL)
    if (!is.null(atual) && identical(executada, atual)) "atualizada" else "pendente"
  })

  # Os painéis de resultado começam ocultos. Sem esta inscrição silenciosa, um
  # eventReactive() lazy podia ser inicializado somente depois do primeiro
  # clique e tratar esse clique como preparação, não como execução. Com o botão
  # ainda em zero, a chamada apenas registra a dependência; nenhum cálculo roda.
  observe({
    tryCatch(
      invisible(resultado_rv()),
      error = function(e) invisible(NULL)
    )
  }, priority = 1000)

  concluir_execucao <- function(mostrar_erro = TRUE) {
    resultado <- tryCatch(isolate(resultado_rv()), error = function(e) e)
    if (inherits(resultado, "error")) {
      mensagem <- conditionMessage(resultado)
      if (!nzchar(mensagem)) mensagem <- "A configuração ainda não produz uma análise válida."
      if (isTRUE(mostrar_erro))
        showNotification(mensagem, type = "warning", duration = 10)
      return(invisible(FALSE))
    }

    # A assinatura é capturada depois do cálculo para congelar exatamente a
    # configuração que produziu a prévia apresentada. Isso evita registrar uma
    # assinatura intermediária quando o cálculo sincroniza controles dependentes.
    assinatura <- tryCatch(isolate(assinatura_rv()), error = function(e) e)
    if (inherits(assinatura, "error")) {
      mensagem <- conditionMessage(assinatura)
      if (!nzchar(mensagem)) mensagem <- "Complete a configuração antes de executar."
      if (isTRUE(mostrar_erro))
        showNotification(mensagem, type = "warning", duration = 8)
      return(invisible(FALSE))
    }

    assinatura_executada_rv(assinatura)
    showNotification(
      sprintf("%s foi executada com a configuração atual.", nome_analise),
      type = "message", duration = 5
    )
    invisible(TRUE)
  }

  if (is.function(gatilho_rv)) {
    observeEvent(input$executar_analise, {
      tentativa_execucao_rv(0L)
      gatilho_rv(as.integer(isolate(gatilho_rv())) + 1L)
    }, priority = 1000)

    observeEvent(gatilho_rv(), {
      tentativa <- as.integer(isolate(tentativa_execucao_rv()))
      max_tentativas_internas <- 2L
      sucesso <- concluir_execucao(
        mostrar_erro = tentativa >= max_tentativas_internas
      )
      if (!isTRUE(sucesso) && tentativa < max_tentativas_internas) {
        tentativa_execucao_rv(tentativa + 1L)
        later::later(function() {
          gatilho_rv(as.integer(isolate(gatilho_rv())) + 1L)
        }, delay = 0.15)
      }
    }, ignoreInit = TRUE, priority = -1000)
  } else {
    observeEvent(input$executar_analise, {
      concluir_execucao()
    }, priority = -1000)
  }

  observe({
    rotulo <- if (identical(estado(), "aguardando")) "Executar análise" else "Executar novamente"
    icone <- if (identical(estado(), "aguardando")) icon("play") else icon("rotate")
    updateActionButton(session, "executar_analise", label = rotulo, icon = icone)
  })

  output$execucao_estado_cliente <- renderText(estado())
  outputOptions(output, "execucao_estado_cliente", suspendWhenHidden = FALSE)

  output$execucao_status <- renderUI({
    switch(
      estado(),
      aguardando = div(
        class = "alert alert-light border mt-2 mb-0 py-2 small",
        icon("sliders"), " Ajuste as variáveis e execute para gerar a primeira prévia."
      ),
      pendente = div(
        class = "alert alert-warning mt-2 mb-0 py-2 small",
        icon("triangle-exclamation"),
        " A base ou a configuração mudou. Execute novamente."
      ),
      atualizada = div(
        class = "alert alert-success mt-2 mb-0 py-2 small",
        icon("check"), " Prévia executada e pronta para ser registrada."
      )
    )
  })

  output$execucao_painel_aviso <- renderUI({
    if (identical(estado(), "atualizada")) return(NULL)
    if (identical(estado(), "aguardando")) {
      return(div(
        class = "alert alert-info",
        style = "min-height:150px; display:flex; align-items:center; justify-content:center; text-align:center;",
        div(icon("play", class = "fa-2x mb-2"), br(),
            strong("Configure a análise e clique em Executar análise."), br(),
            "Nenhum resultado foi calculado ainda.")
      ))
    }
    div(
      class = "alert alert-warning",
      style = "min-height:150px; display:flex; align-items:center; justify-content:center; text-align:center;",
      div(icon("triangle-exclamation", class = "fa-2x mb-2"), br(),
          strong("A configuração ou a base foi alterada."), br(),
          "Execute novamente para substituir a prévia anterior.")
    )
  })

  list(
    estado = estado,
    atualizada = reactive(identical(estado(), "atualizada")),
    assinatura_executada = reactive(assinatura_executada_rv())
  )
}
