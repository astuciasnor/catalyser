# Laboratório de Conceitos — Simulador do teste t
# --------------------------------------------------
# Dois grupos artificiais para visualizar a comparação entre diferença de médias,
# tamanho de amostra e variação. Não usa a base importada nem gera resultados.

mod_lab_teste_t_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_columns(
    col_widths = c(5, 4, 3),
    bslib::card(
      bslib::card_header(shiny::tagList(shiny::icon("sliders"), " Experimente")),
      bslib::card_body(
        shiny::sliderInput(ns("n_por_grupo"), "Observações em cada grupo:", min = 3, max = 60, value = 15, step = 1),
        shiny::h6("Grupo A", style = "color:#0F3B5F; font-weight:700; margin:12px 0 4px;"),
        bslib::layout_columns(col_widths = c(6, 6),
          shiny::sliderInput(ns("media_a"), "Média:", min = 0, max = 25, value = 10, step = 0.5, width = "100%"),
          shiny::sliderInput(ns("desvio_a"), "Desvio-padrão:", min = 0.2, max = 8, value = 2, step = 0.1, width = "100%")
        ),
        shiny::h6("Grupo B", style = "color:#0F3B5F; font-weight:700; margin:12px 0 4px;"),
        bslib::layout_columns(col_widths = c(6, 6),
          shiny::sliderInput(ns("media_b"), "Média:", min = 0, max = 25, value = 13, step = 0.5, width = "100%"),
          shiny::sliderInput(ns("desvio_b"), "Desvio-padrão:", min = 0.2, max = 8, value = 2, step = 0.1, width = "100%")
        ),
        shiny::actionButton(ns("nova_simulacao"), "Nova simulação", icon = shiny::icon("rotate")),
        shiny::tags$hr(),
        shiny::p(class = "small text-muted mb-0", "Compare médias e variações dos dois grupos. O teste t de Welch acomoda desvios-padrão diferentes.")
      )
    ),
    bslib::card(
      bslib::card_header(shiny::tagList(shiny::icon("chart-column"), " Dados artificiais e resultado")),
      bslib::card_body(
        shiny::plotOutput(ns("grafico"), height = "365px"),
        shiny::uiOutput(ns("leitura"))
      )
    ),
    bslib::card(
      bslib::card_header(shiny::tagList(shiny::icon("chart-area"), " Distribuição t")),
      bslib::card_body(
        shiny::plotOutput(ns("curva_t"), height = "365px"),
        shiny::p(class = "small text-muted mb-0", "Âmbar: limites críticos teóricos para α = 5% bilateral. Coral: t obtido nesta simulação. A curva representa H0.")
      )
    )
  )
}

mod_lab_teste_t_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    simulacao <- shiny::reactive({
      input$nova_simulacao
      set.seed(20260923L + input$nova_simulacao)
      dados <- data.frame(
        grupo = factor(rep(c("Grupo A", "Grupo B"), each = input$n_por_grupo)),
        valor = c(
          stats::rnorm(input$n_por_grupo, input$media_a, input$desvio_a),
          stats::rnorm(input$n_por_grupo, input$media_b, input$desvio_b)
        )
      )
      teste <- stats::t.test(valor ~ grupo, data = dados, var.equal = FALSE)
      list(dados = dados, t = unname(teste$statistic), gl = unname(teste$parameter), p = teste$p.value)
    })

    output$grafico <- shiny::renderPlot({
      sim <- simulacao()
      medias_observadas <- stats::aggregate(valor ~ grupo, data = sim$dados, FUN = mean)

      ggplot2::ggplot(sim$dados, ggplot2::aes(x = grupo, y = valor, colour = grupo)) +
        ggplot2::geom_jitter(width = 0.12, alpha = 0.65, size = 2.1, show.legend = FALSE) +
        ggplot2::geom_point(data = medias_observadas, ggplot2::aes(x = grupo, y = valor),
          inherit.aes = FALSE, shape = 18, size = 3.8, colour = "#E76F51") +
        ggplot2::scale_colour_manual(values = c("Grupo A" = "#0F3B5F", "Grupo B" = "#2E7D8F")) +
        ggplot2::labs(
          title = "Teste t de Welch em dados simulados",
          subtitle = "Losangos corais mostram as médias observadas dos dois grupos.",
          x = NULL, y = "Valor simulado"
        ) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold", colour = "#0F3B5F"),
          plot.subtitle = ggplot2::element_text(colour = "#495057"),
          panel.grid.minor = ggplot2::element_blank()
        )
    })

    output$curva_t <- shiny::renderPlot({
      sim <- simulacao()
      critico <- stats::qt(0.975, df = sim$gl)
      limite <- max(stats::qt(0.9975, df = sim$gl), abs(sim$t) * 1.1, critico * 1.1)
      x <- seq(-limite, limite, length.out = 500)
      curva <- data.frame(x = x, densidade = stats::dt(x, df = sim$gl))
      ggplot2::ggplot(curva, ggplot2::aes(x = x, y = densidade)) +
        ggplot2::geom_line(colour = "#0F3B5F", linewidth = 1) +
        ggplot2::geom_vline(xintercept = c(-critico, critico), colour = "#E89B3C", linewidth = 1, linetype = "dashed") +
        ggplot2::geom_vline(xintercept = sim$t, colour = "#E76F51", linewidth = 1) +
        ggplot2::labs(title = "t sob H0", subtitle = sprintf("Limites = ±%.2f  |  t simulado = %.2f", critico, sim$t),
          x = "t", y = "Densidade") +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", colour = "#0F3B5F"),
          plot.subtitle = ggplot2::element_text(size = 9), panel.grid.minor = ggplot2::element_blank())
    })

    output$leitura <- shiny::renderUI({
      sim <- simulacao()
      significativo <- is.finite(sim$p) && sim$p < 0.05
      leitura <- if (significativo) {
        "Nesta rodada, a diferença observada entre os grupos foi grande em relação à variação interna. O teste t de Welch encontrou evidência contra a hipótese de médias iguais."
      } else {
        "Nesta rodada, a diferença observada não se destacou o bastante da variação interna. Isso não demonstra igualdade: significa apenas que a evidência foi insuficiente aqui."
      }
      shiny::tags$div(
        class = if (significativo) "alert alert-success mt-2 mb-0" else "alert alert-light border mt-2 mb-0",
        shiny::tags$p(style = "margin-bottom: 6px;", shiny::tags$b(sprintf("t(%.1f) = %.2f; p = %.4f. ", sim$gl, sim$t, sim$p)), leitura),
        shiny::tags$p(style = "margin-bottom: 0; font-size: 0.9rem;", "Estes dados existem apenas para experimentar o raciocínio do teste t; não são resultados da sua pesquisa.")
      )
    })
  })
}
