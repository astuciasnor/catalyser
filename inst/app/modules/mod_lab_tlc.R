# Laboratório de Conceitos — Teorema do Limite Central
# -----------------------------------------------------
# Este visualizador é autocontido: ele simula populações simples e mostra como
# a distribuição das médias amostrais muda quando aumentamos o tamanho da amostra.

mod_lab_tlc_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_columns(
    col_widths = c(4, 8),
    bslib::card(
      bslib::card_header(shiny::tagList(shiny::icon("sliders"), " Experimente")),
      bslib::card_body(
        shiny::selectInput(
          ns("populacao"),
          "População de origem:",
          choices = c(
            "Uniforme: valores espalhados de modo regular" = "uniforme",
            "Exponencial: muitos valores pequenos e cauda longa" = "exponencial",
            "Binomial: contagens com chance fixa de sucesso" = "binomial"
          ),
          selected = "exponencial"
        ),
        shiny::sliderInput(ns("n"), "Observações em cada amostra (n):", min = 1, max = 100, value = 5, step = 1),
        shiny::sliderInput(ns("repeticoes"), "Número de amostras simuladas:", min = 100, max = 2000, value = 800, step = 100),
        shiny::actionButton(ns("nova_simulacao"), "Nova simulação", icon = shiny::icon("rotate")),
        shiny::tags$hr(),
        shiny::p(class = "small text-muted mb-0", "A população não muda. O que muda é quantas observações entram em cada média.")
      )
    ),
    bslib::card(
      bslib::card_header(shiny::tagList(shiny::icon("bell"), " Médias amostrais")),
      bslib::card_body(
        shiny::plotOutput(ns("grafico"), height = "390px"),
        shiny::uiOutput(ns("leitura"))
      )
    )
  )
}

mod_lab_tlc_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    parametros_populacao <- shiny::reactive({
      switch(
        input$populacao,
        uniforme = list(gerar = function(n) stats::runif(n), media = 0.5, desvio = sqrt(1 / 12), rotulo = "uniforme"),
        exponencial = list(gerar = function(n) stats::rexp(n, rate = 1), media = 1, desvio = 1, rotulo = "exponencial"),
        binomial = list(gerar = function(n) stats::rbinom(n, size = 10, prob = 0.2), media = 2, desvio = sqrt(10 * 0.2 * 0.8), rotulo = "binomial")
      )
    })

    medias_amostrais <- shiny::reactive({
      input$nova_simulacao
      p <- parametros_populacao()
      n <- input$n
      repeticoes <- input$repeticoes

      # A semente deixa cada configuração estável até o botão pedir outra rodada.
      set.seed(20260922L + input$nova_simulacao)
      medias <- vapply(seq_len(repeticoes), function(i) mean(p$gerar(n)), numeric(1))

      list(
        valores = medias,
        media_teorica = p$media,
        erro_padrao_teorico = p$desvio / sqrt(n),
        populacao = p$rotulo,
        n = n,
        repeticoes = repeticoes
      )
    })

    output$grafico <- shiny::renderPlot({
      sim <- medias_amostrais()
      dados <- data.frame(media_amostral = sim$valores)
      limite_inferior <- min(dados$media_amostral, sim$media_teorica - 4 * sim$erro_padrao_teorico)
      limite_superior <- max(dados$media_amostral, sim$media_teorica + 4 * sim$erro_padrao_teorico)
      margem <- max((limite_superior - limite_inferior) * 0.08, 0.05)

      ggplot2::ggplot(dados, ggplot2::aes(x = media_amostral)) +
        ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)), bins = 28, fill = "#62B6B7", color = "white", linewidth = 0.25) +
        ggplot2::stat_function(fun = stats::dnorm, args = list(mean = sim$media_teorica, sd = sim$erro_padrao_teorico), color = "#E76F51", linewidth = 1.1) +
        ggplot2::geom_vline(xintercept = sim$media_teorica, color = "#0F3B5F", linetype = "dashed", linewidth = 0.8) +
        ggplot2::annotate("label", x = sim$media_teorica, y = Inf, label = "média verdadeira", vjust = 1.5, size = 3.2, fill = "white", color = "#0F3B5F") +
        ggplot2::coord_cartesian(xlim = c(limite_inferior - margem, limite_superior + margem)) +
        ggplot2::labs(
          title = "Como se distribuem as médias das amostras?",
          subtitle = sprintf("%d amostras de tamanho n = %d, vindas de uma população %s", sim$repeticoes, sim$n, sim$populacao),
          x = "Média amostral",
          y = "Densidade",
          caption = "Barras: médias observadas. Linha coral: normal teórica prevista pelo TLC."
        ) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold", color = "#0F3B5F"),
          plot.subtitle = ggplot2::element_text(color = "#495057"),
          plot.caption = ggplot2::element_text(color = "#6C757D", hjust = 0),
          panel.grid.minor = ggplot2::element_blank()
        )
    })

    output$leitura <- shiny::renderUI({
      sim <- medias_amostrais()
      media_observada <- mean(sim$valores)
      desvio_observado <- stats::sd(sim$valores)
      perto_da_normal <- if (sim$n < 5) {
        "Com amostras tão pequenas, a forma ainda pode lembrar bastante a população de origem."
      } else if (sim$n < 30) {
        "A distribuição das médias já começa a se concentrar e a ganhar uma forma mais regular."
      } else {
        "As médias ficam mais concentradas e a aproximação normal tende a ficar bem visível."
      }

      shiny::tags$div(
        class = "alert alert-light border mt-2 mb-0",
        shiny::tags$p(style = "margin-bottom: 6px;", shiny::tags$b("O que observar: "), perto_da_normal),
        shiny::tags$p(
          style = "margin-bottom: 0; font-size: 0.9rem;",
          sprintf(
            "A média das médias foi %.3f; a média verdadeira da população é %.3f. O desvio das médias simuladas foi %.3f, próximo do erro-padrão teórico de %.3f.",
            media_observada, sim$media_teorica, desvio_observado, sim$erro_padrao_teorico
          )
        )
      )
    })
  })
}
