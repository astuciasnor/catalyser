# Proporções e aderência: a mesma receita calcula na tela e no Projeto R.
if (file.exists(file.path("..", "..", "R", "analises.R"))) {
  source(file.path("..", "..", "R", "analises.R"), encoding = "UTF-8", local = TRUE)
}

proporcoes_titulo <- function(tipo) {
  switch(tipo,
    uma = "Uma proporção",
    duas = "Duas proporções",
    aderencia = "Qui-quadrado de aderência"
  )
}

proporcoes_tipo_execucao <- function(tipo) {
  switch(tipo, uma = "proporcao_uma", duas = "proporcao_duas",
         aderencia = "qui_quadrado_aderencia")
}

proporcoes_categoricas <- function(dados) {
  nomes <- names(dados)[vapply(dados, function(x) {
    is.factor(x) || is.character(x) || is.logical(x)
  }, logical(1))]
  nomes
}

proporcoes_codigo <- function(tipo, p) {
  texto <- function(x) encodeString(as.character(x), quote = '"')
  if (tipo == "uma") return(c(
    "# Unidades independentes; ausências na resposta não entram no denominador.",
    sprintf("resposta <- dados[[%s]]", texto(p$variavel)),
    "resposta <- resposta[!is.na(resposta)]",
    sprintf("sucessos <- sum(resposta == %s)", texto(p$sucesso)),
    "total <- length(resposta)",
    sprintf("teste <- binom.test(sucessos, total, p = %s, alternative = %s, conf.level = %s)",
      format(p$referencia, scientific = FALSE), texto(p$alternativa), format(p$confianca, scientific = FALSE)),
    "teste"
  ))
  if (tipo == "duas") return(c(
    "# Dois grupos de unidades independentes; pares e blocos exigem outro procedimento.",
    sprintf("dados_validos <- dados[complete.cases(dados[c(%s, %s)]), c(%s, %s)]",
      texto(p$resposta), texto(p$grupo), texto(p$resposta), texto(p$grupo)),
    sprintf("tab <- table(dados_validos[[%s]], dados_validos[[%s]])", texto(p$resposta), texto(p$grupo)),
    sprintf("sucessos <- tab[%s, ]", texto(p$sucesso)),
    "totais <- colSums(tab)",
    sprintf("teste <- prop.test(sucessos, totais, correct = %s, conf.level = %s)",
      if (isTRUE(p$correcao)) "TRUE" else "FALSE", format(p$confianca, scientific = FALSE)),
    "fisher.test(tab)",
    "teste"
  ))
  esperadas <- paste(sprintf("%s = %s", texto(names(p$esperadas)), format(unname(p$esperadas), scientific = FALSE)), collapse = ", ")
  c(
    "# Unidades independentes; as proporções esperadas vêm da hipótese substantiva.",
    sprintf("resposta <- dados[[%s]]", texto(p$variavel)),
    "observadas <- table(resposta, useNA = \"no\")",
    sprintf("proporcoes_esperadas <- c(%s)", esperadas),
    "teste <- chisq.test(observadas, p = proporcoes_esperadas[names(observadas)])",
    "teste"
  )
}

mod_proporcoes_ui <- function(id, tipo) {
  ns <- shiny::NS(id)
  ajuda_desenho <- "Use somente quando cada linha representa uma unidade independente. Pares, medidas repetidas, blocos, estratos ou agrupamentos exigem outro procedimento."
  controles <- if (tipo == "uma") tagList(
    selectInput(ns("variavel"), "Resposta binária:", choices = NULL),
    selectInput(ns("sucesso"), "Categoria considerada sucesso:", choices = NULL),
    numericInput(ns("referencia"), "Proporção de referência:", value = .5, min = 0, max = 1, step = .01),
    selectInput(ns("alternativa"), "Hipótese alternativa:", c("Diferente" = "two.sided", "Maior" = "greater", "Menor" = "less")),
    numericInput(ns("confianca"), "Nível de confiança:", value = .95, min = .5, max = .999, step = .01)
  ) else if (tipo == "duas") tagList(
    selectInput(ns("resposta"), "Resposta binária:", choices = NULL),
    selectInput(ns("grupo"), "Grupos a comparar:", choices = NULL),
    selectInput(ns("sucesso"), "Categoria considerada sucesso:", choices = NULL),
    numericInput(ns("confianca"), "Nível de confiança:", value = .95, min = .5, max = .999, step = .01),
    checkboxInput(ns("correcao"), "Usar correção de continuidade", value = FALSE)
  ) else tagList(
    selectInput(ns("variavel"), "Variável categórica:", choices = NULL),
    uiOutput(ns("esperadas_ui"))
  )
  tagList(
    tags$h2(proporcoes_titulo(tipo), class = "h4 mb-3"),
    mod_seletor_base_analise_ui(ns("base")),
    bslib::layout_sidebar(
      fill = FALSE, fillable = FALSE,
      sidebar = bslib::sidebar(
        width = 315,
        controles,
        radioButtons(ns("desenho"), "Delineamento:",
          c("Unidades independentes" = "independente", "Há pares, blocos, estratos ou agrupamentos" = "nao_atendido")),
        helpText(ajuda_desenho),
        actionButton(ns("executar"), "Executar análise", icon = icon("play"), class = "btn-primary")
      ),
      uiOutput(ns("status")),
      conditionalPanel(sprintf("output['%s'] === true", ns("pronto")),
        uiOutput(ns("narrativa")),
        plotOutput(ns("grafico"), height = "400px"),
        DT::DTOutput(ns("tabela")),
        tags$details(tags$summary("Ver verificações, código R e saída do console"),
          DT::DTOutput(ns("diagnosticos")),
          verbatimTextOutput(ns("codigo")),
          verbatimTextOutput(ns("console"))
        )
      ),
      bslib::card(
        bslib::card_header(icon("bookmark"), " Adicionar ao Projeto R"),
        bslib::card_body(mod_registrar_execucao_ui(ns("registrar")))
      )
    )
  )
}

mod_proporcoes_server <- function(id, tipo, dados_rv, registro_bases_rv,
                                  cache_bases_rv, revisao_origem_rv,
                                  registro_execucoes_rv, contador_execucoes_rv) {
  moduleServer(id, function(input, output, session) {
    seletor <- mod_seletor_base_analise_server(
      "base", dados_rv, registro_bases_rv, cache_bases_rv, revisao_origem_rv,
      finalidade_preferida = "geral", nome_analise = proporcoes_titulo(tipo)
    )
    resultado_rv <- reactiveVal(NULL)
    estado_rv <- reactiveVal(NULL)
    rodada_rv <- reactiveVal(0L)
    contexto_anterior <- reactiveVal(NULL)
    contexto <- reactive({
      ctx <- seletor$contexto()
      list(base_id = ctx$base_id, versao = ctx$versao_receita,
           revisao = revisao_origem_rv(), dados = seletor$dados())
    })
    observeEvent(contexto(), {
      if (identical(contexto(), contexto_anterior())) return()
      contexto_anterior(contexto())
      resultado_rv(NULL); estado_rv(NULL); rodada_rv(rodada_rv() + 1L)
    }, priority = 100)

    observe({
      dados <- seletor$dados(); req(is.data.frame(dados))
      categoricas <- proporcoes_categoricas(dados)
      atualizar <- function(campo, escolhas, padrao = 1L) {
        atual <- isolate(input[[campo]])
        if (is.null(atual) || !atual %in% escolhas) atual <- if (length(escolhas)) escolhas[min(padrao, length(escolhas))] else ""
        updateSelectInput(session, campo, choices = escolhas, selected = atual)
      }
      if (tipo %in% c("uma", "aderencia")) atualizar("variavel", categoricas)
      if (tipo == "duas") {
        atualizar("resposta", categoricas)
        atualizar("grupo", setdiff(categoricas, input$resposta))
      }
    })
    resposta_atual <- reactive({
      dados <- seletor$dados(); req(is.data.frame(dados))
      coluna <- if (tipo == "duas") input$resposta else input$variavel
      req(!is.null(coluna), nzchar(coluna), coluna %in% names(dados))
      unique(as.character(dados[[coluna]][!is.na(dados[[coluna]])]))
    })
    observe({
      niveis <- resposta_atual()
      if (tipo %in% c("uma", "duas")) {
        atual <- isolate(input$sucesso)
        updateSelectInput(session, "sucesso", choices = niveis, selected = if (atual %in% niveis) atual else niveis[1])
      }
    })
    output$esperadas_ui <- renderUI({
      if (tipo != "aderencia") return(NULL)
      categorias <- resposta_atual()
      req(length(categorias) >= 2L)
      tagList(
        tags$label("Proporções esperadas (%)"),
        helpText("Defina a hipótese antes de olhar o resultado. A soma deve ser 100%."),
        lapply(seq_along(categorias), function(i) numericInput(session$ns(paste0("esperada_", i)), categorias[i], value = 100 / length(categorias), min = 0, max = 100, step = .01))
      )
    })
    parametros <- reactive({
      desenho <- input$desenho
      if (tipo == "uma") return(list(variavel = input$variavel, sucesso = input$sucesso,
        referencia = input$referencia, confianca = input$confianca, alternativa = input$alternativa, desenho = desenho))
      if (tipo == "duas") return(list(resposta = input$resposta, grupo = input$grupo,
        sucesso = input$sucesso, confianca = input$confianca, correcao = isTRUE(input$correcao), desenho = desenho))
      categorias <- resposta_atual()
      valores <- vapply(seq_along(categorias), function(i) input[[paste0("esperada_", i)]], numeric(1))
      list(variavel = input$variavel, esperadas = stats::setNames(valores / 100, categorias), desenho = desenho)
    })
    analisar <- function(dados, p) {
      switch(tipo,
        uma = catalyser_proporcao_uma(dados, p),
        duas = catalyser_proporcao_duas(dados, p),
        aderencia = catalyser_aderencia(dados, p)
      )
    }
    observeEvent(input$executar, {
      p <- tryCatch(parametros(), error = identity)
      if (inherits(p, "error")) {
        showNotification(conditionMessage(p), type = "warning", duration = 8); return()
      }
      resposta <- tryCatch(analisar(seletor$dados(), p), error = identity)
      if (inherits(resposta, "error")) {
        resultado_rv(NULL); estado_rv(NULL)
        showNotification(conditionMessage(resposta), type = "warning", duration = 9); return()
      }
      resultado_rv(resposta)
      titulo <- switch(tipo,
        uma = paste("Uma proporção:", p$sucesso, "em", p$variavel),
        duas = paste("Duas proporções:", p$sucesso, "por", p$grupo),
        aderencia = paste("Aderência:", p$variavel)
      )
      estado_rv(list(
        analise_id = proporcoes_tipo_execucao(tipo),
        tipo = proporcoes_tipo_execucao(tipo),
        titulo = titulo, parametros = p,
        saidas_disponiveis = intersect(names(resposta), c("narrativa", "tabela", "grafico", "diagnosticos", "console")),
        codigo_r = paste(proporcoes_codigo(tipo, p), collapse = "\n")
      ))
      rodada_rv(rodada_rv() + 1L)
      showNotification("Resultado disponível para adicionar ao Projeto R.", type = "message", duration = 4)
    })
    pronto <- reactive({
      contexto()
      !is.null(resultado_rv()) && !is.null(estado_rv())
    })
    output$pronto <- reactive(pronto())
    outputOptions(output, "pronto", suspendWhenHidden = FALSE)
    output$status <- renderUI({
      if (pronto()) return(NULL)
      div(class = "alert alert-light border", icon("circle-info"), " Escolha as variáveis, confirme o delineamento e execute a análise.")
    })
    output$narrativa <- renderUI({ req(pronto()); p(resultado_rv()$narrativa) })
    tabela_dt <- function(valor) {
      DT::datatable(valor, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE,
        language = list(search = "Buscar:", zeroRecords = "Nenhum resultado encontrado")))
    }
    output$tabela <- DT::renderDT({ req(pronto()); tabela_dt(resultado_rv()$tabela) })
    output$diagnosticos <- DT::renderDT({ req(pronto()); tabela_dt(resultado_rv()$diagnosticos) })
    output$grafico <- renderPlot({ req(pronto(), resultado_rv()$grafico); print(resultado_rv()$grafico) })
    output$codigo <- renderText({ req(pronto()); paste(proporcoes_codigo(tipo, estado_rv()$parametros), collapse = "\n") })
    output$console <- renderText({ req(pronto()); paste(resultado_rv()$console, collapse = "\n") })
    registro <- mod_registrar_execucao_server(
      "registrar", reactive(estado_rv()), seletor$contexto, registro_execucoes_rv,
      contador_execucoes_rv, revisao_origem_rv, registro_bases_rv, cache_bases_rv,
      paste0("frequencias_", tipo), proporcoes_titulo(tipo), nova_configuracao_rv = rodada_rv
    )
    invisible(list(estado_execucao = estado_rv, resultado = resultado_rv, registro = registro, seletor = seletor))
  })
}
