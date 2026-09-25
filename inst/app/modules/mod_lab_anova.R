# Laboratório de Conceitos — Simulador da ANOVA
# ------------------------------------------------
# Dados artificiais para enxergar a ideia da ANOVA: diferenças entre as médias
# dos grupos competem com a variação dentro de cada grupo. Não usa nem altera a
# base importada e nunca entra na Comunicação de Resultados.

mod_lab_anova_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_columns(
    col_widths = c(5, 4, 3),
    bslib::card(
      bslib::card_header(shiny::tagList(shiny::icon("sliders"), " Experimente")),
      bslib::card_body(
        shiny::sliderInput(ns("grupos"), "Número de grupos:", min = 2, max = 5, value = 3, step = 1),
        shiny::sliderInput(ns("n_por_grupo"), "Observações em cada grupo:", min = 3, max = 50, value = 12, step = 1),
        shiny::uiOutput(ns("medias_ui")),
        shiny::actionButton(ns("nova_simulacao"), "Nova simulação", icon = shiny::icon("rotate")),
        shiny::tags$hr(),
        shiny::p(class = "small text-muted mb-0", "Cada grupo tem sua própria média e variação. A ANOVA de Welch acomoda desvios-padrão diferentes.")
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
      bslib::card_header(shiny::tagList(shiny::icon("chart-area"), " Distribuição F")),
      bslib::card_body(
        shiny::plotOutput(ns("curva_f"), height = "365px"),
        shiny::p(class = "small text-muted mb-0", "Âmbar: F crítico teórico para α = 5%. Coral: F obtido nesta simulação. A curva representa H0.")
      )
    )
  )
}

mod_lab_anova_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    output$medias_ui <- shiny::renderUI({
      medias_padrao <- seq(8, by = 2, length.out = input$grupos)
      shiny::tagList(lapply(seq_len(input$grupos), function(i) {
        media_atual <- shiny::isolate(input[[paste0("media_", i)]])
        desvio_atual <- shiny::isolate(input[[paste0("desvio_", i)]])
        if (is.null(media_atual)) media_atual <- medias_padrao[i]
        if (is.null(desvio_atual)) desvio_atual <- 2
        shiny::tagList(
          shiny::h6(sprintf("Grupo %d", i), style = "color:#0F3B5F; font-weight:700; margin:12px 0 4px;"),
          bslib::layout_columns(
            col_widths = c(6, 6),
            shiny::sliderInput(session$ns(paste0("media_", i)), "Média:",
              min = 0, max = 25, value = media_atual, step = 0.5, width = "100%"),
            shiny::sliderInput(session$ns(paste0("desvio_", i)), "Desvio-padrão:",
              min = 0.2, max = 8, value = desvio_atual, step = 0.1, width = "100%")
          )
        )
      }))
    })

    simulacao <- shiny::reactive({
      input$nova_simulacao
      medias <- vapply(seq_len(input$grupos), function(i) {
        valor <- input[[paste0("media_", i)]]
        if (is.null(valor)) valor <- seq(8, by = 2, length.out = input$grupos)[i]
        valor
      }, numeric(1))
      desvios <- vapply(seq_len(input$grupos), function(i) {
        valor <- input[[paste0("desvio_", i)]]
        if (is.null(valor)) valor <- 2
        valor
      }, numeric(1))

      # A rodada fica estável até o botão pedir novos dados, facilitando comparar ajustes.
      set.seed(20260922L + input$nova_simulacao)
      dados <- data.frame(
        grupo = factor(rep(paste("Grupo", seq_len(input$grupos)), each = input$n_por_grupo)),
        valor = unlist(Map(function(media, desvio) stats::rnorm(input$n_por_grupo, mean = media, sd = desvio), medias, desvios))
      )
      modelo <- stats::oneway.test(valor ~ grupo, data = dados, var.equal = FALSE)
      list(
        dados = dados,
        f = unname(modelo$statistic),
        gl = unname(modelo$parameter),
        p = modelo$p.value,
        medias = medias,
        desvios = desvios
      )
    })

    output$grafico <- shiny::renderPlot({
      sim <- simulacao()
      medias_observadas <- stats::aggregate(valor ~ grupo, data = sim$dados, FUN = mean)

      ggplot2::ggplot(sim$dados, ggplot2::aes(x = grupo, y = valor, colour = grupo)) +
        ggplot2::geom_jitter(width = 0.12, alpha = 0.65, size = 2.1, show.legend = FALSE) +
        ggplot2::geom_point(data = medias_observadas, ggplot2::aes(x = grupo, y = valor),
          inherit.aes = FALSE, shape = 18, size = 3.8, colour = "#E76F51") +
        ggplot2::scale_colour_manual(values = rep(c("#0F3B5F", "#2E7D8F", "#62B6B7", "#E89B3C", "#E76F51"), length.out = input$grupos)) +
        ggplot2::labs(
          title = "ANOVA de Welch em dados simulados",
          subtitle = "Losangos corais mostram as médias observadas de cada grupo.",
          x = NULL, y = "Valor simulado"
        ) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold", colour = "#0F3B5F"),
          plot.subtitle = ggplot2::element_text(colour = "#495057"),
          panel.grid.minor = ggplot2::element_blank()
        )
    })

    output$curva_f <- shiny::renderPlot({
      sim <- simulacao()
      gl1 <- sim$gl[1]
      gl2 <- sim$gl[2]
      critico <- stats::qf(0.95, gl1, gl2)
      limite <- max(stats::qf(0.995, gl1, gl2), sim$f * 1.1, critico * 1.1)
      x <- seq(0, limite, length.out = 500)
      curva <- data.frame(x = x, densidade = stats::df(x, gl1, gl2))
      ggplot2::ggplot(curva, ggplot2::aes(x = x, y = densidade)) +
        ggplot2::geom_line(colour = "#0F3B5F", linewidth = 1) +
        ggplot2::geom_vline(xintercept = critico, colour = "#E89B3C", linewidth = 1, linetype = "dashed") +
        ggplot2::geom_vline(xintercept = sim$f, colour = "#E76F51", linewidth = 1) +
        ggplot2::labs(title = "F sob H0", subtitle = sprintf("F crítico = %.2f  |  F simulado = %.2f", critico, sim$f),
          x = "F", y = "Densidade") +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", colour = "#0F3B5F"),
          plot.subtitle = ggplot2::element_text(size = 9), panel.grid.minor = ggplot2::element_blank())
    })

    output$leitura <- shiny::renderUI({
      sim <- simulacao()
      significativo <- is.finite(sim$p) && sim$p < 0.05
      leitura <- if (significativo) {
        "Nesta rodada, a diferença entre as médias foi grande em relação à variação dentro dos grupos. A ANOVA de Welch encontrou evidência contra a hipótese de médias iguais."
      } else {
        "Nesta rodada, a diferença entre as médias ainda não se destacou da variação dentro dos grupos. Isso não prova médias iguais; apenas não há evidência suficiente contra elas aqui."
      }

      shiny::tags$div(
        class = if (significativo) "alert alert-success mt-2 mb-0" else "alert alert-light border mt-2 mb-0",
        shiny::tags$p(style = "margin-bottom: 6px;", shiny::tags$b(sprintf("F = %.2f; p = %.4f. ", sim$f, sim$p)), leitura),
        shiny::tags$p(style = "margin-bottom: 0; font-size: 0.9rem;", "Estes dados existem só para aprender o raciocínio da ANOVA; não são resultados da sua pesquisa.")
      )
    })
  })
}
