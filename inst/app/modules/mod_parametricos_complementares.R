# Testes paramétricos complementares organizados pela pergunta estatística.
# -----------------------------------------------------------------------------
# O mesmo módulo atende a ANOVA de medidas repetidas, o qui-quadrado para uma
# variância e o teste F para duas variâncias. Os cálculos vivem em R/analises.R.

parametrico_complementar_titulo <- function(tipo) {
  switch(tipo,
    anova_medidas_repetidas = "ANOVA de medidas repetidas",
    qui_quadrado_variancia = "Qui-quadrado para variância",
    teste_f_variancias = "Teste F para duas variâncias"
  )
}

parametrico_complementar_pergunta <- function(tipo) {
  switch(tipo,
    anova_medidas_repetidas = "A média muda quando a mesma unidade é medida em ocasiões ou condições diferentes?",
    qui_quadrado_variancia = "A variância ou o desvio padrão difere de um valor de referência?",
    teste_f_variancias = "A dispersão difere entre dois grupos independentes?"
  )
}

parametrico_complementar_ui <- function(ns, tipo) {
  alternativas <- c("Diferente" = "two.sided", "Menor" = "less", "Maior" = "greater")
  switch(tipo,
    anova_medidas_repetidas = tagList(
      selectInput(ns("resposta"), "Resposta numérica:", choices = NULL),
      selectInput(ns("sujeito"), "Identificação da unidade:", choices = NULL),
      selectInput(ns("momento"), "Ocasião ou condição repetida:", choices = NULL),
      helpText("Use o formato longo: uma linha por unidade e ocasião. Cada unidade deve aparecer uma vez em cada ocasião.")
    ),
    qui_quadrado_variancia = tagList(
      selectInput(ns("variavel"), "Variável numérica:", choices = NULL),
      numericInput(ns("desvio_hipotetico"), "Desvio padrão de referência:", value = 1, min = 0.000001),
      radioButtons(ns("alternativa"), "Hipótese alternativa:", alternativas, selected = "two.sided", inline = TRUE),
      helpText("O teste pressupõe uma população aproximadamente normal. Independência vem do planejamento da coleta.")
    ),
    teste_f_variancias = tagList(
      selectInput(ns("resposta"), "Resposta numérica:", choices = NULL),
      selectInput(ns("grupo"), "Grupo com exatamente dois níveis:", choices = NULL),
      radioButtons(ns("alternativa"), "Hipótese alternativa:", alternativas, selected = "two.sided", inline = TRUE),
      helpText("O teste F é sensível a desvios da normalidade. Use-o para comparar dois grupos independentes.")
    )
  )
}

mod_parametrico_complementar_ui <- function(id, tipo) {
  ns <- NS(id)
  titulo <- parametrico_complementar_titulo(tipo)
  # O painel do bslib espalha os filhos com gap de 16px; o wrapper junta as
  # linhas de topo (título, pergunta, cartão) com 6px para caber na primeira dobra.
  div(
    class = "catalyser-complementar html-fill-item html-fill-container",
    style = "display:flex; flex-direction:column; gap:6px;",
    h2(titulo, class = "h4 mb-0"),
    div(class = "alert alert-light border mb-0", style = "padding:4px 10px; font-size:0.85rem;",
      strong("Pergunta: "), parametrico_complementar_pergunta(tipo)),
    bslib::navset_card_tab(
      id = ns("abas"),
      bslib::nav_panel(
        "Configurar e executar", value = "executar",
        mod_seletor_base_analise_ui(ns("base"), compacto = TRUE),
        bslib::layout_sidebar(
          sidebar = bslib::sidebar(
            parametrico_complementar_ui(ns, tipo),
            sliderInput(ns("nivel_confianca"), "Nível de confiança (%):", min = 80, max = 99, value = 95),
            actionButton(ns("executar"), "Executar teste", icon = icon("play"), class = "btn-primary w-100")
          ),
          div(
            uiOutput(ns("status")),
            uiOutput(ns("narrativa")),
            bslib::navset_card_tab(
              bslib::nav_panel("Resultado", DT::DTOutput(ns("tabela"))),
              bslib::nav_panel("Gráfico", plotOutput(ns("grafico"), height = "390px")),
              bslib::nav_panel("Pressupostos", DT::DTOutput(ns("pressupostos"))),
              bslib::nav_panel("Código R", verbatimTextOutput(ns("codigo"))),
              bslib::nav_panel("Console R", verbatimTextOutput(ns("console")))
            )
          )
        )
      ),
      bslib::nav_panel(
        "Inserir análise", value = "inserir", icon = icon("bookmark"),
        p("Guarde o resultado no Projeto R depois de executar o teste."),
        mod_registrar_execucao_ui(ns("registrar"))
      )
    )
  )
}

parametrico_complementar_codigo <- function(tipo, p) {
  texto <- function(x) encodeString(as.character(x), quote = '"')
  comum <- c(sprintf("nivel_confianca <- %s", format(p$nivel_confianca, scientific = FALSE)))
  switch(tipo,
    anova_medidas_repetidas = c(
      sprintf("resposta <- %s", texto(p$resposta)), sprintf("sujeito <- %s", texto(p$sujeito)),
      sprintf("momento <- %s", texto(p$momento)), comum,
      "dados_repetidos <- dados[stats::complete.cases(dados[c(resposta, sujeito, momento)]), c(resposta, sujeito, momento)]",
      "names(dados_repetidos) <- c('valor', 'sujeito', 'momento')",
      "dados_repetidos$sujeito <- factor(dados_repetidos$sujeito)",
      "dados_repetidos$momento <- factor(dados_repetidos$momento)",
      "presencas <- stats::xtabs(~ sujeito + momento, data = dados_repetidos) > 0",
      "sujeitos_completos <- rownames(presencas)[rowSums(presencas) == ncol(presencas)]",
      "dados_repetidos <- droplevels(dados_repetidos[dados_repetidos$sujeito %in% sujeitos_completos, ])",
      "modelo <- stats::aov(valor ~ sujeito + momento, data = dados_repetidos)",
      "summary(modelo)"
    ),
    qui_quadrado_variancia = c(
      sprintf("variavel <- %s", texto(p$variavel)), sprintf("desvio_referencia <- %s", p$desvio_hipotetico),
      sprintf("alternativa <- %s", texto(p$alternativa)), comum,
      "x <- dados[[variavel]]", "x <- x[is.finite(x)]", "gl <- length(x) - 1L",
      "qui_quadrado <- gl * stats::var(x) / desvio_referencia^2",
      "p_inferior <- stats::pchisq(qui_quadrado, gl)",
      "p_superior <- stats::pchisq(qui_quadrado, gl, lower.tail = FALSE)",
      "p_valor <- if (alternativa == 'less') p_inferior else if (alternativa == 'greater') p_superior else min(1, 2 * min(p_inferior, p_superior))",
      "c(qui_quadrado = qui_quadrado, gl = gl, p_valor = p_valor)"
    ),
    teste_f_variancias = c(
      sprintf("resposta <- %s", texto(p$resposta)), sprintf("grupo <- %s", texto(p$grupo)),
      sprintf("alternativa <- %s", texto(p$alternativa)), comum,
      "formula_teste <- stats::reformulate(grupo, response = resposta)",
      "resultado <- stats::var.test(formula_teste, data = dados, alternative = alternativa, conf.level = nivel_confianca)",
      "resultado"
    )
  )
}

mod_parametrico_complementar_server <- function(id, tipo, dados_rv, registro_bases_rv,
                                                 cache_bases_rv, revisao_origem_rv,
                                                 registro_execucoes_rv, contador_execucoes_rv) {
  moduleServer(id, function(input, output, session) {
    titulo <- parametrico_complementar_titulo(tipo)
    seletor <- mod_seletor_base_analise_server(
      "base", dados_rv, registro_bases_rv, cache_bases_rv, revisao_origem_rv,
      finalidade_preferida = "geral", nome_analise = titulo
    )
    dados <- seletor$dados

    observe({
      d <- dados()
      numericas <- names(d)[vapply(d, is.numeric, logical(1))]
      candidatas_grupo <- names(d)[vapply(d, function(x) {
        n <- length(unique(x[!is.na(x)])); n >= 2L && n <= 30L
      }, logical(1))]
      if (tipo == "qui_quadrado_variancia") {
        updateSelectInput(session, "variavel", choices = numericas)
      } else {
        updateSelectInput(session, "resposta", choices = numericas)
      }
      if (tipo == "anova_medidas_repetidas") {
        updateSelectInput(session, "sujeito", choices = names(d))
        updateSelectInput(session, "momento", choices = candidatas_grupo)
      }
      if (tipo == "teste_f_variancias") updateSelectInput(session, "grupo", choices = candidatas_grupo)
    })

    parametros <- reactive({
      base <- list(nivel_confianca = input$nivel_confianca / 100)
      switch(tipo,
        anova_medidas_repetidas = c(base, list(resposta = input$resposta, sujeito = input$sujeito, momento = input$momento)),
        qui_quadrado_variancia = c(base, list(variavel = input$variavel,
          desvio_hipotetico = input$desvio_hipotetico, alternativa = input$alternativa)),
        teste_f_variancias = c(base, list(resposta = input$resposta, grupo = input$grupo,
          alternativa = input$alternativa))
      )
    })
    resultado_rv <- reactiveVal(NULL)
    estado_rv <- reactiveVal(NULL)
    assinatura_rv <- reactiveVal(NULL)
    assinatura <- reactive(serialize(list(seletor$contexto()$base_id, parametros()), NULL))

    observeEvent(input$executar, {
      p <- parametros()
      resultado <- tryCatch(switch(tipo,
        anova_medidas_repetidas = catalyser_anova_medidas_repetidas(dados(), p),
        qui_quadrado_variancia = catalyser_variancia_uma(dados(), p),
        teste_f_variancias = catalyser_variancias_duas(dados(), p)
      ), error = function(e) e)
      if (inherits(resultado, "error")) {
        resultado_rv(NULL); estado_rv(NULL)
        showNotification(conditionMessage(resultado), type = "error", duration = 8)
        return()
      }
      resultado_rv(resultado)
      assinatura_rv(assinatura())
      estado_rv(list(
        analise_id = tipo, tipo = tipo, titulo = titulo, parametros = p,
        saidas_disponiveis = intersect(names(resultado), c("narrativa", "tabela", "grafico", "pressupostos", "diagnosticos")),
        codigo_r = paste(parametrico_complementar_codigo(tipo, p), collapse = "\n")
      ))
      showNotification("Resultado disponível para inserir no Projeto R.", type = "message")
    })
    pronto <- reactive(!is.null(resultado_rv()) && identical(assinatura_rv(), assinatura()))
    output$status <- renderUI({
      if (pronto()) return(NULL)
      div(class = if (is.null(resultado_rv())) "alert alert-light border" else "alert alert-warning",
          style = "padding:4px 10px; font-size:0.85rem; margin-bottom:4px;",
          if (is.null(resultado_rv())) "Escolha as variáveis e execute o teste." else "A configuração mudou. Execute novamente.")
    })
    output$narrativa <- renderUI({
      req(pronto())
      div(class = "alert alert-secondary", style = "padding:4px 10px; font-size:0.85rem; margin-bottom:4px;",
          resultado_rv()$narrativa)
    })
    tabela_dt <- function(x) DT::datatable(x, rownames = FALSE, options = list(pageLength = 12, scrollX = TRUE, dom = "tip"))
    output$tabela <- DT::renderDT({ req(pronto()); tabela_dt(resultado_rv()$tabela) })
    output$pressupostos <- DT::renderDT({ req(pronto()); tabela_dt(resultado_rv()$pressupostos) })
    output$grafico <- renderPlot({ req(pronto(), resultado_rv()$grafico); print(resultado_rv()$grafico) })
    output$codigo <- renderText({ req(pronto()); paste(parametrico_complementar_codigo(tipo, parametros()), collapse = "\n") })
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
