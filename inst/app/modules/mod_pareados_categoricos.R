# Testes para dados pareados que completam as grades de decisão da CatalyseR.
# -----------------------------------------------------------------------------
# Friedman compara k condições nos mesmos blocos. McNemar compara duas respostas
# binárias nas mesmas unidades. Os cálculos e a validação vivem em R/analises.R.

pareados_titulo <- function(tipo) {
  switch(tipo,
    friedman = "Friedman (k grupos pareados)",
    mcnemar = "McNemar (duas respostas binárias pareadas)"
  )
}

pareados_pergunta <- function(tipo) {
  switch(tipo,
    friedman = "As condições diferem quando são medidas nos mesmos indivíduos ou blocos?",
    mcnemar = "A proporção de uma resposta binária muda entre duas medições da mesma unidade?"
  )
}

pareados_codigo <- function(tipo, p) {
  texto <- function(x) encodeString(as.character(x), quote = '"')
  switch(tipo,
    friedman = c(
      "# 1. Declarar a resposta, a condição e o bloco que se repete.",
      sprintf("resposta <- %s", texto(p$resposta)),
      sprintf("condicao <- %s", texto(p$condicao)),
      sprintf("bloco <- %s", texto(p$bloco)),
      "",
      "# 2. Conferir se todos os blocos têm todas as condições.",
      "dados_friedman <- dados[stats::complete.cases(dados[c(resposta, condicao, bloco)]), c(resposta, condicao, bloco)]",
      "names(dados_friedman) <- c('valor', 'condicao', 'bloco')",
      "presencas <- stats::xtabs(~ bloco + condicao, data = dados_friedman) > 0",
      "stopifnot(all(rowSums(presencas) == ncol(presencas)))",
      "",
      "# 3. Aplicar o Friedman, equivalente não paramétrico da ANOVA repetida.",
      "resultado <- stats::friedman.test(valor ~ condicao | bloco, data = dados_friedman)",
      "",
      "# 4. Se o teste global indicar diferença, comparar os pares com Holm.",
      "posteste <- stats::pairwise.wilcox.test(dados_friedman$valor, dados_friedman$condicao, paired = TRUE, p.adjust.method = 'holm')",
      "resultado",
      "posteste"
    ),
    mcnemar = c(
      "# 1. Declarar as duas respostas binárias medidas nos mesmos pares.",
      sprintf("primeira_medicao <- %s", texto(p$variavel_1)),
      sprintf("segunda_medicao <- %s", texto(p$variavel_2)),
      "",
      "# 2. Montar a tabela 2 por 2 dos pares completos.",
      "dados_mcnemar <- dados[stats::complete.cases(dados[c(primeira_medicao, segunda_medicao)]), ]",
      "tabela_pares <- table(dados_mcnemar[[primeira_medicao]], dados_mcnemar[[segunda_medicao]])",
      "",
      "# 3. Testar se as proporções mudaram entre as duas medições.",
      sprintf("resultado <- stats::mcnemar.test(tabela_pares, correct = %s)", if (isTRUE(p$correcao)) "TRUE" else "FALSE"),
      "resultado"
    )
  )
}

mod_pareados_categoricos_ui <- function(id, tipo) {
  ns <- NS(id)
  titulo <- pareados_titulo(tipo)
  controles <- if (identical(tipo, "friedman")) {
    tagList(
      selectInput(ns("resposta"), "Resposta numérica:", choices = NULL),
      selectInput(ns("condicao"), "Condição com k níveis:", choices = NULL),
      selectInput(ns("bloco"), "Identificação do bloco ou indivíduo:", choices = NULL),
      uiOutput(ns("opcoes_posteste")),
      helpText("Use o formato longo: uma linha por bloco e condição. Cada bloco deve ter todas as condições, sem repetição.")
    )
  } else {
    tagList(
      selectInput(ns("variavel_1"), "Primeira medição binária:", choices = NULL),
      selectInput(ns("variavel_2"), "Segunda medição binária:", choices = NULL),
      checkboxInput(ns("correcao"), "Usar correção de continuidade", value = TRUE),
      helpText("Cada linha representa o mesmo indivíduo, unidade ou par nas duas medições.")
    )
  }
  tagList(
    h2(titulo, class = "h4 mb-1"),
    div(class = "alert alert-light border mb-2", strong("Pergunta: "), pareados_pergunta(tipo)),
    bslib::navset_card_tab(
      bslib::nav_panel(
        "Configurar e executar",
        mod_seletor_base_analise_ui(ns("base"), compacto = TRUE),
        bslib::layout_sidebar(
          sidebar = bslib::sidebar(
            controles,
            if (identical(tipo, "friedman")) sliderInput(ns("nivel_confianca"), "Nível de confiança (%):", min = 80, max = 99, value = 95),
            actionButton(ns("executar"), "Executar teste", icon = icon("play"), class = "btn-primary w-100")
          ),
          div(
            uiOutput(ns("status")), uiOutput(ns("narrativa")),
            bslib::navset_card_tab(
              bslib::nav_panel("Resultado", DT::DTOutput(ns("tabela"))),
              if (identical(tipo, "friedman")) bslib::nav_panel("Pós-teste", DT::DTOutput(ns("comparacoes"))),
              bslib::nav_panel("Gráfico", plotOutput(ns("grafico"), height = "390px")),
              bslib::nav_panel("Cuidados", DT::DTOutput(ns("pressupostos"))),
              bslib::nav_panel("Código R", verbatimTextOutput(ns("codigo"))),
              bslib::nav_panel("Console R", verbatimTextOutput(ns("console")))
            )
          )
        )
      ),
      bslib::nav_panel(
        "Inserir análise", icon = icon("bookmark"),
        p("Guarde o resultado no Projeto R depois de executar o teste."),
        mod_registrar_execucao_ui(ns("registrar"))
      )
    )
  )
}

mod_pareados_categoricos_server <- function(id, tipo, dados_rv, registro_bases_rv,
                                             cache_bases_rv, revisao_origem_rv,
                                             registro_execucoes_rv, contador_execucoes_rv) {
  moduleServer(id, function(input, output, session) {
    titulo <- pareados_titulo(tipo)
    seletor <- mod_seletor_base_analise_server(
      "base", dados_rv, registro_bases_rv, cache_bases_rv, revisao_origem_rv,
      finalidade_preferida = "geral", nome_analise = titulo
    )
    dados <- seletor$dados
    observe({
      d <- dados()
      numericas <- names(d)[vapply(d, is.numeric, logical(1))]
      categoricas <- names(d)[vapply(d, function(x) {
        niveis <- unique(x[!is.na(x)])
        length(niveis) >= 2L && length(niveis) <= 30L
      }, logical(1))]
      binarias <- names(d)[vapply(d, function(x) length(unique(x[!is.na(x)])) == 2L, logical(1))]
      if (identical(tipo, "friedman")) {
        updateSelectInput(session, "resposta", choices = numericas)
        updateSelectInput(session, "condicao", choices = categoricas)
        updateSelectInput(session, "bloco", choices = names(d))
      } else {
        updateSelectInput(session, "variavel_1", choices = binarias)
        updateSelectInput(session, "variavel_2", choices = binarias)
      }
    })
    output$opcoes_posteste <- renderUI({
      escolhas <- c("Wilcoxon pareado com correção de Holm" = "holm")
      if (requireNamespace("PMCMRplus", quietly = TRUE)) escolhas <- c(escolhas, "Nemenyi" = "nemenyi")
      tagList(
        selectInput(session$ns("posteste"), "Pós-teste quando houver diferença:", choices = escolhas),
        if (!"nemenyi" %in% unname(escolhas)) helpText("Nemenyi fica disponível quando PMCMRplus estiver instalado; Holm já atende esta versão.")
      )
    })
    parametros <- reactive(if (identical(tipo, "friedman")) {
      list(resposta = input$resposta, condicao = input$condicao, bloco = input$bloco,
           posteste = input$posteste, nivel_confianca = input$nivel_confianca / 100)
    } else {
      list(variavel_1 = input$variavel_1, variavel_2 = input$variavel_2, correcao = isTRUE(input$correcao))
    })
    resultado_rv <- reactiveVal(NULL)
    estado_rv <- reactiveVal(NULL)
    assinatura_rv <- reactiveVal(NULL)
    assinatura <- reactive(serialize(list(seletor$contexto()$base_id, parametros()), NULL))
    observeEvent(input$executar, {
      p <- parametros()
      resultado <- tryCatch(
        if (identical(tipo, "friedman")) catalyser_friedman(dados(), p) else catalyser_mcnemar(dados(), p),
        error = function(e) e
      )
      if (inherits(resultado, "error")) {
        resultado_rv(NULL); estado_rv(NULL)
        showNotification(conditionMessage(resultado), type = "error", duration = 8)
        return()
      }
      resultado_rv(resultado); assinatura_rv(assinatura())
      estado_rv(list(
        analise_id = tipo, tipo = tipo, titulo = titulo, parametros = p,
        saidas_disponiveis = intersect(names(resultado), c("narrativa", "tabela", "comparacoes", "grafico", "pressupostos", "diagnosticos")),
        codigo_r = paste(pareados_codigo(tipo, p), collapse = "\n")
      ))
      showNotification("Resultado disponível para inserir no Projeto R.", type = "message")
    })
    pronto <- reactive(!is.null(resultado_rv()) && identical(assinatura_rv(), assinatura()))
    output$status <- renderUI({
      if (pronto()) return(NULL)
      div(class = if (is.null(resultado_rv())) "alert alert-light border" else "alert alert-warning",
          if (is.null(resultado_rv())) "Escolha as variáveis e execute o teste." else "A configuração mudou. Execute novamente.")
    })
    tabela_dt <- function(x) DT::datatable(x, rownames = FALSE, options = list(pageLength = 12, scrollX = TRUE, dom = "tip"))
    output$narrativa <- renderUI({ req(pronto()); div(class = "alert alert-secondary", resultado_rv()$narrativa) })
    output$tabela <- DT::renderDT({ req(pronto()); tabela_dt(resultado_rv()$tabela) })
    output$comparacoes <- DT::renderDT({ req(pronto()); tabela_dt(resultado_rv()$comparacoes %||% data.frame(Aviso = "Pós-teste não necessário.")) })
    output$pressupostos <- DT::renderDT({ req(pronto()); tabela_dt(resultado_rv()$pressupostos) })
    output$grafico <- renderPlot({ req(pronto(), resultado_rv()$grafico); print(resultado_rv()$grafico) })
    output$codigo <- renderText({ req(pronto()); paste(pareados_codigo(tipo, parametros()), collapse = "\n") })
    output$console <- renderText({ req(pronto()); paste(resultado_rv()$console, collapse = "\n") })
    registro <- mod_registrar_execucao_server(
      "registrar", reactive(if (pronto()) estado_rv() else NULL), seletor$contexto,
      registro_execucoes_rv, contador_execucoes_rv, revisao_origem_rv,
      registro_bases_rv, cache_bases_rv, tipo, titulo,
      nova_configuracao_rv = reactive(list(input$executar, parametros()))
    )
    invisible(list(resultado = resultado_rv, estado_execucao = estado_rv,
                   contexto = seletor$contexto, registro = registro))
  })
}
