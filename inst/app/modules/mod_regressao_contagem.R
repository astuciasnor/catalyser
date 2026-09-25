# Regressões para respostas de contagem.
# -----------------------------------------------------------------------------
# Poisson e Binomial Negativa respondem à mesma pergunta: como a contagem muda
# com os preditores? A diferença aparece no diagnóstico de dispersão, não no
# menu nem na escolha arbitrária de um p-valor.

regressao_contagem_titulo <- function(familia) {
  if (identical(familia, "poisson")) "Regressão de Poisson" else "Regressão Binomial Negativa"
}

regressao_contagem_tipo <- function(familia) {
  if (identical(familia, "poisson")) "regressao_poisson" else "regressao_binomial_negativa"
}

# O código fica visível para a pessoa poder levar a mesma análise ao RStudio.
regressao_contagem_codigo <- function(p, familia) {
  texto <- function(x) encodeString(as.character(x), quote = '"')
  preditores <- paste(vapply(p$preditores, texto, character(1)), collapse = ", ")
  linha_offset <- if (isTRUE(p$usar_offset)) c(
    "# Transforme o esforço, a área ou o volume em log antes do ajuste.",
    sprintf("dados_modelo$.catalyser_offset_log <- log(dados_modelo[[%s]])", texto(p$offset))
  ) else character()
  termos <- c(vapply(p$preditores, texto, character(1)), if (isTRUE(p$usar_offset)) '"offset(.catalyser_offset_log)"')
  ajustar <- if (identical(familia, "poisson")) {
    "modelo <- stats::glm(formula_modelo, data = dados_modelo, family = stats::poisson())"
  } else {
    "modelo <- MASS::glm.nb(formula_modelo, data = dados_modelo)"
  }
  c(
    "# Escolha a resposta de contagem e os preditores já definidos na interface.",
    sprintf("variaveis <- c(%s, %s)", texto(p$resposta), preditores),
    "# Retire apenas as linhas que não têm os valores necessários para este modelo.",
    "dados_modelo <- dados[stats::complete.cases(dados[variaveis]), variaveis, drop = FALSE]",
    linha_offset,
    "# Monte a fórmula sem escrever nomes de colunas manualmente.",
    sprintf("formula_modelo <- stats::reformulate(c(%s), response = %s)", paste(termos, collapse = ", "), texto(p$resposta)),
    "# Ajuste o modelo de contagem escolhido.",
    ajustar,
    "# Calcule a dispersão de Pearson para conferir a variabilidade residual.",
    "dispersao_pearson <- sum(stats::residuals(modelo, type = 'pearson')^2) / stats::df.residual(modelo)",
    "# Veja os coeficientes e as razões de taxas no console.",
    "summary(modelo)"
  )
}

mod_regressao_contagem_ui <- function(id, familia) {
  ns <- shiny::NS(id)
  titulo <- regressao_contagem_titulo(familia)
  explicacao <- if (identical(familia, "poisson")) {
    "Use quando a resposta é uma contagem. Depois do ajuste, a dispersão de Pearson indica se a variação residual é compatível com Poisson ou se a Binomial Negativa é mais adequada."
  } else {
    "Use para contagens com variação maior que a prevista pelo Poisson. A escolha costuma ser motivada pela superdispersão observada no modelo de Poisson."
  }

  tagList(
    tags$h2(titulo, class = "h4 mb-3"),
    mod_seletor_base_analise_ui(ns("base")),
    bslib::layout_sidebar(
      fill = FALSE,
      fillable = FALSE,
      sidebar = bslib::sidebar(
        width = 330,
        tags$h5("Configuração do modelo"),
        selectInput(ns("resposta"), "Resposta de contagem:", choices = NULL),
        selectizeInput(
          ns("preditores"), "Preditores:", choices = NULL,
          multiple = TRUE,
          options = list(placeholder = "Escolha uma ou mais variáveis")
        ),
        selectInput(ns("offset"), "Offset de exposição, opcional:", choices = c("Nenhum" = "")),
        helpText("Escolha esforço, área ou volume apenas se ele for positivo e representar a oportunidade de contar. O modelo usa o log desse valor."),
        div(
          class = "alert alert-light border small mt-2",
          icon("circle-info"), " ", explicacao
        ),
        actionButton(ns("executar"), "Executar análise", icon = icon("play"), class = "btn-primary w-100")
      ),
      uiOutput(ns("status")),
      conditionalPanel(
        sprintf("output['%s'] === true", ns("pronto")),
        bslib::navset_card_tab(
          bslib::nav_panel(
            "Resultados", icon = icon("table"),
            uiOutput(ns("narrativa")),
            tags$h5("Coeficientes e razões de taxas", class = "mt-3"),
            DT::DTOutput(ns("coeficientes")),
            helpText("A razão de taxas mostra o efeito multiplicativo esperado para uma unidade do preditor, mantendo os demais preditores do modelo constantes."),
            tags$h5("Ajuste e dispersão", class = "mt-3"),
            DT::DTOutput(ns("ajuste")),
            uiOutput(ns("orientacao_dispersao"))
          ),
          bslib::nav_panel(
            "Diagnóstico dos resíduos", icon = icon("chart-line"),
            plotOutput(ns("grafico_residuos"), height = "430px"),
            helpText("Procure uma nuvem sem padrão forte. A independência das observações vem do delineamento e não é demonstrada por este gráfico.")
          ),
          bslib::nav_panel(
            "Código e console", icon = icon("code"),
            tags$details(
              open = TRUE,
              tags$summary("Ver código R que reproduz o ajuste"),
              verbatimTextOutput(ns("codigo"))
            ),
            tags$details(
              tags$summary("Ver saída resumida do R"),
              verbatimTextOutput(ns("console"))
            )
          )
        ),
        bslib::card(
          class = "mt-3",
          bslib::card_header(icon("bookmark"), " Adicionar ao Projeto R"),
          bslib::card_body(mod_registrar_execucao_ui(ns("registrar")))
        )
      )
    )
  )
}

mod_regressao_contagem_server <- function(id, familia, dados_rv,
                                           registro_bases_rv, cache_bases_rv,
                                           revisao_origem_rv,
                                           registro_execucoes_rv,
                                           contador_execucoes_rv) {
  moduleServer(id, function(input, output, session) {
    titulo <- regressao_contagem_titulo(familia)

    # Reutilize a mesma base compartilhada ou derivada oferecida aos outros modelos.
    seletor <- mod_seletor_base_analise_server(
      "base", dados_rv, registro_bases_rv, cache_bases_rv, revisao_origem_rv,
      finalidade_preferida = "geral", nome_analise = titulo
    )

    # Guarde resultado e assinatura separados para não exibir análise desatualizada.
    resultado_rv <- reactiveVal(NULL)
    estado_rv <- reactiveVal(NULL)
    assinatura_executada_rv <- reactiveVal(NULL)
    rodada_rv <- reactiveVal(0L)

    # Atualize seletores ao mudar a base, preservando escolhas que ainda existem.
    observe({
      dados <- seletor$dados()
      req(is.data.frame(dados))
      nomes <- names(dados)
      numericas <- nomes[vapply(dados, is.numeric, logical(1))]
      resposta_atual <- isolate(input$resposta)
      resposta_escolhida <- if (resposta_atual %in% numericas) {
        resposta_atual
      } else if (length(numericas)) {
        numericas[[1]]
      } else {
        ""
      }
      preditores_atuais <- isolate(input$preditores) %||% character()
      preditores_validos <- setdiff(intersect(preditores_atuais, nomes), resposta_escolhida)
      offset_atual <- isolate(input$offset) %||% ""
      offsets <- setdiff(numericas, resposta_escolhida)
      offset_escolhido <- if (offset_atual %in% offsets) offset_atual else ""

      updateSelectInput(session, "resposta", choices = numericas, selected = resposta_escolhida)
      updateSelectizeInput(session, "preditores", choices = setdiff(nomes, resposta_escolhida), selected = preditores_validos)
      updateSelectInput(session, "offset", choices = c("Nenhum" = "", offsets), selected = offset_escolhido)
    })

    # Congele somente parâmetros leves, nunca a tabela inteira, para detectar mudanças.
    assinatura_atual <- reactive({
      contexto <- seletor$contexto()
      list(
        base_id = contexto$base_id,
        versao_base = contexto$versao_receita,
        revisao_origem = as.integer(revisao_origem_rv()),
        resposta = input$resposta %||% "",
        preditores = sort(input$preditores %||% character()),
        offset = input$offset %||% "",
        familia = familia
      )
    })

    # Construa os parâmetros que seguem para o motor canônico e para o Projeto R.
    parametros <- reactive({
      list(
        resposta = input$resposta,
        preditores = input$preditores %||% character(),
        offset = input$offset %||% "",
        usar_offset = nzchar(input$offset %||% ""),
        nivel_confianca = 0.95
      )
    })

    # Execute uma única vez por clique e transforme erros estatísticos em aviso legível.
    observeEvent(input$executar, {
      p <- tryCatch(parametros(), error = function(e) e)
      if (inherits(p, "error")) {
        showNotification(conditionMessage(p), type = "warning", duration = 8)
        return()
      }
      resultado <- tryCatch(
        catalyser_regressao_contagem(seletor$dados(), p, familia = familia),
        error = function(e) e
      )
      if (inherits(resultado, "error")) {
        resultado_rv(NULL)
        estado_rv(NULL)
        assinatura_executada_rv(NULL)
        showNotification(conditionMessage(resultado), type = "warning", duration = 10)
        return()
      }

      # Só marque a análise como pronta depois que resultado e configuração coincidirem.
      resultado_rv(resultado)
      estado_rv(list(
        analise_id = regressao_contagem_tipo(familia),
        tipo = regressao_contagem_tipo(familia),
        titulo = sprintf("%s: %s", titulo, p$resposta),
        parametros = p,
        saidas_disponiveis = c("narrativa", "tabela", "grafico", "pressupostos", "diagnosticos", "console"),
        resultado_resumo = resultado$resumo,
        codigo_r = paste(regressao_contagem_codigo(p, familia), collapse = "\n")
      ))
      assinatura_executada_rv(assinatura_atual())
      rodada_rv(as.integer(rodada_rv()) + 1L)
      showNotification("Resultado disponível para adicionar ao Projeto R.", type = "message", duration = 4)
    })

    # Uma mudança de configuração exige nova execução antes de mostrar ou registrar saída.
    pronto <- reactive({
      !is.null(resultado_rv()) &&
        !is.null(estado_rv()) &&
        identical(assinatura_executada_rv(), assinatura_atual())
    })
    output$pronto <- reactive(pronto())
    outputOptions(output, "pronto", suspendWhenHidden = FALSE)

    output$status <- renderUI({
      if (pronto()) return(NULL)
      mudou <- !is.null(resultado_rv())
      div(
        class = if (mudou) "alert alert-warning" else "alert alert-light border",
        icon(if (mudou) "triangle-exclamation" else "circle-info"), " ",
        if (mudou) "A base ou a configuração mudou. Execute novamente." else "Escolha a contagem, os preditores e, se necessário, o offset."
      )
    })

    output$narrativa <- renderUI({
      req(pronto())
      div(class = "alert alert-secondary", resultado_rv()$narrativa)
    })

    # O Viewer usa uma tabela navegável; o Projeto R usa catalyser_tabela_ocean().
    tabela_dt <- function(x) {
      DT::datatable(
        x,
        rownames = FALSE,
        options = list(pageLength = 15, scrollX = TRUE,
          language = list(search = "Buscar:", zeroRecords = "Nenhum resultado encontrado"))
      ) |>
        DT::formatStyle(names(x), `font-family` = "Calibri")
    }
    output$coeficientes <- DT::renderDT({ req(pronto()); tabela_dt(resultado_rv()$tabela) })
    output$ajuste <- DT::renderDT({ req(pronto()); tabela_dt(resultado_rv()$diagnosticos) })

    output$orientacao_dispersao <- renderUI({
      req(pronto())
      resultado <- resultado_rv()
      if (identical(familia, "poisson")) {
        classe <- if (resultado$dispersao > 2) "alert alert-warning mt-3" else "alert alert-light border mt-3"
        div(class = classe, icon("route"), " ", resultado$orientacao_dispersao)
      } else {
        div(class = "alert alert-light border mt-3", icon("circle-info"), " ", resultado$orientacao_dispersao)
      }
    })

    output$grafico_residuos <- renderPlot({
      req(pronto(), resultado_rv()$grafico)
      print(resultado_rv()$grafico)
    })
    output$codigo <- renderText({ req(pronto()); paste(regressao_contagem_codigo(parametros(), familia), collapse = "\n") })
    output$console <- renderText({ req(pronto()); paste(resultado_rv()$console, collapse = "\n") })

    # Registre apenas a configuração que produziu a prévia ainda atual.
    registro <- mod_registrar_execucao_server(
      "registrar", reactive(if (pronto()) estado_rv() else NULL), seletor$contexto,
      registro_execucoes_rv, contador_execucoes_rv, revisao_origem_rv,
      registro_bases_rv, cache_bases_rv, regressao_contagem_tipo(familia), titulo,
      nova_configuracao_rv = rodada_rv
    )

    invisible(list(
      estado_execucao = estado_rv,
      resultado = resultado_rv,
      registro = registro,
      seletor = seletor
    ))
  })
}
