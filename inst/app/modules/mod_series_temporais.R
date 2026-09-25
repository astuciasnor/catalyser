# Séries Temporais: exploração descritiva em R base.
# -----------------------------------------------------------------------------
# A ordem no tempo é parte do dado. Este módulo mostra a série, a média móvel,
# a decomposição e a autocorrelação sem introduzir previsão na versão 1.

# Carregue o exemplo de CPUE apenas quando o pacote de dados estiver disponível.
.carrega_cpue_tubarao <- function() {
  if (!requireNamespace("EAPADados", quietly = TRUE)) return(NULL)
  ambiente <- new.env(parent = emptyenv())
  carregou <- tryCatch({
    utils::data("cpue_tubarao", package = "EAPADados", envir = ambiente)
    exists("cpue_tubarao", envir = ambiente, inherits = FALSE)
  }, error = function(e) FALSE)
  if (!carregou) return(NULL)
  get("cpue_tubarao", envir = ambiente, inherits = FALSE)
}

# Preencha lacunas temporárias por interpolação apenas para decompose() e acf().
.serie_interpolar_ausentes <- function(x) {
  indices <- seq_along(x)
  observados <- !is.na(x)
  if (sum(observados) < 2L) return(x)
  stats::approx(indices[observados], x[observados], xout = indices, rule = 2)$y
}

# Converta data, texto ou ano em datas sem exigir pacote adicional.
.serie_converter_data <- function(x) {
  if (inherits(x, "Date")) return(as.Date(x))
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x) && all(x[!is.na(x)] >= 1900 & x[!is.na(x)] <= 2100)) {
    return(as.Date(paste0(as.integer(x), "-01-01")))
  }
  data_iso <- suppressWarnings(as.Date(x))
  if (!all(is.na(data_iso))) return(data_iso)
  suppressWarnings(as.Date(x, format = "%d/%m/%Y"))
}

# Prepare uma série regular, agregada por mês ou ano, a partir da base escolhida.
.serie_preparar <- function(dados, coluna_data, coluna_valor, agregacao) {
  if (!is.data.frame(dados)) stop("A fonte de dados não está disponível.", call. = FALSE)
  if (!coluna_data %in% names(dados) || !coluna_valor %in% names(dados)) {
    stop("Escolha uma coluna de tempo e uma variável numérica existentes.", call. = FALSE)
  }
  # Converta as duas colunas sem alterar a base compartilhada da CatalyseR.
  data <- .serie_converter_data(dados[[coluna_data]])
  valor <- suppressWarnings(as.numeric(dados[[coluna_valor]]))
  valido <- !is.na(data) & is.finite(valor)
  if (sum(valido) < 4L) {
    stop("A série precisa de pelo menos quatro observações com data e valor válidos.", call. = FALSE)
  }
  bruto <- data.frame(data = data[valido], valor = valor[valido])
  # Transforme cada observação em seu período de leitura.
  if (identical(agregacao, "anual")) {
    bruto$periodo <- as.Date(paste0(format(bruto$data, "%Y"), "-01-01"))
    frequencia <- 1L
  } else {
    bruto$periodo <- as.Date(format(bruto$data, "%Y-%m-01"))
    frequencia <- 12L
  }
  # Quando há mais de uma observação no período, a série usa a média declarada.
  resumo <- stats::aggregate(valor ~ periodo, data = bruto, FUN = mean)
  # Crie períodos ausentes para não fingir que a série é regular quando não é.
  todos_periodos <- if (identical(agregacao, "anual")) {
    anos <- seq.int(as.integer(format(min(resumo$periodo), "%Y")), as.integer(format(max(resumo$periodo), "%Y")))
    data.frame(periodo = as.Date(paste0(anos, "-01-01")))
  } else {
    data.frame(periodo = seq.Date(min(resumo$periodo), max(resumo$periodo), by = "month"))
  }
  regular <- merge(todos_periodos, resumo, by = "periodo", all.x = TRUE, sort = TRUE)
  inicio <- c(as.integer(format(regular$periodo[[1]], "%Y")))
  if (frequencia == 12L) inicio <- c(inicio, as.integer(format(regular$periodo[[1]], "%m")))
  # A série usada nos cálculos recebe interpolação explícita, mas o gráfico mantém as lacunas visíveis.
  valores_calculo <- .serie_interpolar_ausentes(regular$valor)
  serie_ts <- stats::ts(valores_calculo, start = inicio, frequency = frequencia)
  list(
    dados = regular,
    ts = serie_ts,
    frequencia = frequencia,
    ausentes_interpolados = sum(is.na(regular$valor)),
    observacoes_originais = nrow(bruto)
  )
}

# Aplique a identidade Ocean aos gráficos que ggfortify constrói a partir do R base.
.serie_tema_ocean <- function(grafico) {
  grafico +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", color = "#0F3B5F", size = 15),
      plot.subtitle = ggplot2::element_text(color = "#495057"),
      axis.title = ggplot2::element_text(color = "#0F3B5F"),
      panel.grid.minor = ggplot2::element_blank()
    )
}

# Carregue o método S3 de ggfortify antes de pedir autoplot para objetos do R base.
.serie_autoplot <- function(objeto, ...) {
  if (!requireNamespace("ggfortify", quietly = TRUE)) {
    stop("Instale o pacote ggfortify para desenhar objetos de série temporal.", call. = FALSE)
  }
  ggplot2::autoplot(objeto, ...)
}

# Cada opção do menu reaproveita o mesmo motor, mas mostra um único retrato da série.
mod_series_temporais_ui <- function(id, painel = c("visualizar", "decompor", "autocorrelacao")) {
  # Garanta que o painel pedido tenha um nome válido e previsível.
  painel <- match.arg(painel)
  ns <- shiny::NS(id)
  # Dê a cada opção uma pergunta própria, sem transformar o percurso em obrigação.
  titulo_painel <- switch(
    painel,
    visualizar = "Visualizar e suavizar a série",
    decompor = "Decomposição da série",
    autocorrelacao = "Autocorrelação"
  )
  # Mostre somente o retrato escolhido no menu superior.
  retrato_painel <- switch(
    painel,
    visualizar = card(
      card_header(icon("chart-line"), " Série temporal e suavização"),
      card_body(
        plotOutput(ns("serie_plot"), height = "460px"),
        helpText("A média móvel reduz oscilações curtas para facilitar a leitura da tendência. Ela não é uma previsão.")
      )
    ),
    decompor = card(
      card_header(icon("layer-group"), " Tendência, sazonalidade e resíduo"),
      card_body(
        plotOutput(ns("decomposicao_plot"), height = "540px"),
        helpText("A decomposição separa tendência, sazonalidade e resíduo. Ela exige uma série mensal com pelo menos dois ciclos completos.")
      )
    ),
    autocorrelacao = card(
      card_header(icon("wave-square"), " Dependência entre observações vizinhas"),
      card_body(
        plotOutput(ns("acf_plot"), height = "440px"),
        helpText("A ACF mostra quanto valores separados por um ou mais períodos se parecem. Dependência temporal pede cautela antes de aplicar testes que assumem independência.")
      )
    )
  )
  tagList(
    tags$h2(titulo_painel, class = "h4 mb-2"),
    div(
      class = "alert alert-light border mb-3",
      icon("clock"), " A série temporal começa pela ordem dos registros. Esta versão explora, descreve e diagnostica a dependência no tempo. Previsão com ETS e ARIMA ficará para a v2."
    ),
    bslib::layout_sidebar(
      fill = FALSE,
      fillable = FALSE,
      sidebar = bslib::sidebar(
        width = 330,
        tags$h5("Configuração da série"),
        radioButtons(
          ns("source"), "Fonte:",
          choices = c("Exemplo: CPUE de tubarão" = "exemplo", "Meus dados" = "dados"),
          selected = "exemplo"
        ),
        selectInput(ns("date_col"), "Coluna de tempo:", choices = NULL),
        selectInput(ns("value_col"), "Variável numérica:", choices = NULL),
        selectInput(
          ns("agregacao"), "Granularidade:",
          choices = c("Mensal, média por mês" = "mensal", "Anual, média por ano" = "anual"),
          selected = "mensal"
        ),
        checkboxInput(ns("media_movel"), "Mostrar média móvel", value = TRUE),
        conditionalPanel(
          condition = sprintf("input['%s'] === true", ns("media_movel")),
          sliderInput(ns("janela_media"), "Janela da média móvel:", min = 2, max = 12, value = 3, step = 1)
        ),
        tags$hr(),
        tags$h6("Projeto R"),
        downloadButton(ns("download_project_zip"), "Baixar projeto de estudo", class = "btn-outline-primary w-100")
      ),
      uiOutput(ns("status")),
      conditionalPanel(
        sprintf("output['%s'] === true", ns("pronto")),
        retrato_painel
      )
    )
  )
}

mod_series_temporais_server <- function(id, data_rv, import_info) {
  moduleServer(id, function(input, output, session) {
    # Mantenha a escolha antiga entre exemplo didático e base importada.
    dados_ativos <- reactive({
      if (identical(input$source, "exemplo")) .carrega_cpue_tubarao() else data_rv()
    })
    # Atualize as colunas disponíveis ao trocar de fonte.
    observe({
      dados <- dados_ativos()
      req(is.data.frame(dados))
      nomes <- names(dados)
      numericas <- nomes[vapply(dados, is.numeric, logical(1))]
      candidatas_data <- nomes[vapply(dados, function(x) inherits(x, "Date") || inherits(x, "POSIXt"), logical(1))]
      if (!length(candidatas_data)) candidatas_data <- nomes[grepl("data|date|ano|year|mes", tolower(nomes))]
      data_atual <- isolate(input$date_col)
      valor_atual <- isolate(input$value_col)
      data_escolhida <- if (data_atual %in% nomes) {
        data_atual
      } else if (length(candidatas_data)) {
        candidatas_data[[1]]
      } else {
        nomes[[1]]
      }
      valor_escolhido <- if (valor_atual %in% numericas) {
        valor_atual
      } else if (length(numericas)) {
        numericas[[1]]
      } else {
        ""
      }
      updateSelectInput(session, "date_col", choices = nomes, selected = data_escolhida)
      updateSelectInput(session, "value_col", choices = numericas, selected = valor_escolhido)
    })
    # Preserve a leitura imediata do módulo anterior: trocar uma escolha atualiza a série.
    resultado_rv <- reactive({
      tryCatch(
        .serie_preparar(
          dados_ativos(), input$date_col, input$value_col,
          input$agregacao %||% "mensal"
        ),
        error = function(e) e
      )
    })
    # Só mostre as abas quando a preparação atual produziu uma série válida.
    pronto <- reactive(!inherits(resultado_rv(), "error"))
    output$pronto <- reactive(pronto())
    outputOptions(output, "pronto", suspendWhenHidden = FALSE)
    output$status <- renderUI({
      if (pronto()) {
        serie <- resultado_rv()
        if (serie$ausentes_interpolados > 0) {
          return(div(class = "alert alert-warning", icon("triangle-exclamation"), " ",
                     sprintf("%d período(s) sem valor foram interpolados apenas para decomposição e ACF. O gráfico da série mantém as lacunas visíveis.", serie$ausentes_interpolados)))
        }
        return(NULL)
      }
      div(
        class = "alert alert-light border",
        icon("circle-info"), " ", conditionMessage(resultado_rv())
      )
    })
    # Use o método autoplot que ggfortify registra para objetos ts do R base.
    output$serie_plot <- renderPlot({
      validate(need(requireNamespace("ggfortify", quietly = TRUE), "Instale o pacote ggfortify para desenhar os objetos de série temporal."))
      req(pronto())
      serie <- resultado_rv()
      objeto <- serie$ts
      if (isTRUE(input$media_movel)) {
        janela <- max(2L, as.integer(input$janela_media %||% 3L))
        media <- stats::filter(objeto, rep(1 / janela, janela), sides = 2)
        objeto <- stats::ts(cbind(Observado = objeto, `Média móvel` = media),
                            start = stats::start(serie$ts), frequency = stats::frequency(serie$ts))
        grafico <- .serie_autoplot(objeto) +
          ggplot2::scale_colour_manual(values = c("Observado" = "#2E7D8F", "Média móvel" = "#E89B3C"))
      } else {
        grafico <- .serie_autoplot(objeto, ts.colour = "#2E7D8F")
      }
      .serie_tema_ocean(grafico + ggplot2::labs(
        title = "Série temporal e suavização",
        subtitle = "A linha de suavização resume oscilações locais, sem prever valores futuros.",
        x = "Tempo", y = input$value_col
      ))
    })
    # Decompose exige periodicidade mensal e pelo menos dois ciclos completos.
    output$decomposicao_plot <- renderPlot({
      validate(need(requireNamespace("ggfortify", quietly = TRUE), "Instale o pacote ggfortify para desenhar a decomposição."))
      req(pronto())
      serie <- resultado_rv()
      validate(need(serie$frequencia == 12L && length(serie$ts) >= 24L,
                    "A decomposição exige série mensal com pelo menos 24 períodos regulares."))
      decomposicao <- stats::decompose(serie$ts, type = "additive")
      .serie_tema_ocean(.serie_autoplot(decomposicao) + ggplot2::labs(
        title = "Decomposição aditiva", subtitle = "Tendência, sazonalidade e resíduo"
      ))
    })
    # ACF é calculada em R base e desenhada pelo método ggfortify.
    output$acf_plot <- renderPlot({
      validate(need(requireNamespace("ggfortify", quietly = TRUE), "Instale o pacote ggfortify para desenhar a ACF."))
      req(pronto())
      acf <- stats::acf(resultado_rv()$ts, plot = FALSE)
      .serie_tema_ocean(.serie_autoplot(acf) + ggplot2::labs(
        title = "Função de autocorrelação", subtitle = "Dependência entre observações separadas no tempo",
        x = "Defasagem", y = "Autocorrelação"
      ))
    })
    # Exporte um projeto curto que repete exatamente o percurso exploratório da tela.
    output$download_project_zip <- downloadHandler(
      filename = function() paste0("projeto_serie_temporal_", format(Sys.Date(), "%Y-%m-%d"), ".zip"),
      content = function(file) {
        req(pronto())
        serie <- resultado_rv()
        projeto <- paste0("projeto_serie_temporal_", format(Sys.Date(), "%Y-%m-%d"))
        temporario <- tempdir()
        pasta_projeto <- file.path(temporario, projeto)
        dir.create(pasta_projeto, showWarnings = FALSE, recursive = TRUE)
        pasta_dados <- file.path(pasta_projeto, "dados")
        pasta_scripts <- file.path(pasta_projeto, "scripts")
        dir.create(pasta_dados, showWarnings = FALSE)
        dir.create(pasta_scripts, showWarnings = FALSE)
        utils::write.csv(serie$dados, file.path(pasta_dados, "serie_regular.csv"), row.names = FALSE)
        codigo <- c(
          "# Séries Temporais exploratórias, gerado pela CatalyseR.",
          "# install.packages(c('ggplot2', 'ggfortify'))",
          "library(ggfortify)",
          "",
          "# Leia a série regular preparada na interface.",
          "serie <- utils::read.csv('dados/serie_regular.csv')",
          "serie$periodo <- as.Date(serie$periodo)",
          "",
          "# Preencha lacunas apenas para os cálculos que exigem uma sequência completa.",
          "indice <- seq_len(nrow(serie))",
          "observados <- !is.na(serie$valor)",
          "valor_calculo <- stats::approx(indice[observados], serie$valor[observados], xout = indice, rule = 2)$y",
          "",
          sprintf("# Construa a série com frequência %d.", serie$frequencia),
          sprintf("serie_ts <- stats::ts(valor_calculo, start = c(%s), frequency = %d)", paste(stats::start(serie$ts), collapse = ", "), serie$frequencia),
          "",
          "# Visualize a série e uma média móvel de três períodos.",
          "media_movel <- stats::filter(serie_ts, rep(1 / 3, 3), sides = 2)",
          "ggplot2::autoplot(cbind(Observado = serie_ts, Media_movel = media_movel))",
          "",
          "# Decomponha somente séries mensais com pelo menos dois ciclos completos.",
          "if (stats::frequency(serie_ts) == 12 && length(serie_ts) >= 24) ggplot2::autoplot(stats::decompose(serie_ts))",
          "",
          "# Veja a dependência entre períodos vizinhos.",
          "ggplot2::autoplot(stats::acf(serie_ts, plot = FALSE))"
        )
        writeLines(codigo, file.path(pasta_scripts, "serie_temporal.R"), useBytes = TRUE)
        writeLines(c(
          "SÉRIE TEMPORAL EXPLORATÓRIA", "",
          "dados/serie_regular.csv guarda a série mensal ou anual regular.",
          "scripts/serie_temporal.R mostra série, média móvel, decomposição e ACF.",
          "ETS, ARIMA e previsão não fazem parte deste projeto v1."
        ), file.path(pasta_projeto, "README.txt"), useBytes = TRUE)
        diretorio_anterior <- getwd()
        on.exit(setwd(diretorio_anterior), add = TRUE)
        setwd(temporario)
        zip::zip(file, files = projeto)
      }
    )
  })
}
