# Exploração Visual dos Dados -------------------------------------------------
# Cada tela responde a uma pergunta visual antes de o aluno escolher uma análise.

# Use somente cores Ocean com contraste suficiente entre grupos e séries.
# A fonte única vive no motor compartilhado (R/descrevendo_dados.R).
.visual_ocean <- cores_ocean()

# Mantenha o gráfico legível sem ocupar toda a largura da tela.
.visual_card_grafico <- function(titulo, id_plot, altura = 460) {
  bslib::card(
    bslib::card_header(titulo),
    bslib::card_body(
      div(
        class = "mx-auto",
        style = "max-width: 960px; width: 100%;",
        shiny::plotOutput(id_plot, height = altura, width = "100%")
      )
    )
  )
}

# Aplique a identidade Ocean sem competir com os dados desenhados.
.visual_tema <- function() {
  tema_ocean()
}

# Identifique variáveis que o aluno pode usar como medida numérica.
.visual_numericas <- function(dados) names(dados)[vapply(dados, is.numeric, logical(1))]

# Identifique categorias sem transformar códigos numéricos em categorias por conta própria.
.visual_categoricas <- function(dados) {
  names(dados)[vapply(dados, function(x) is.factor(x) || is.character(x) || is.logical(x), logical(1))]
}

# Monte uma fórmula de facetas que respeita nomes de colunas com espaços.
.visual_formula_faceta <- function(variavel) stats::as.formula(paste0("~`", variavel, "`"))

# Reescale a segunda variável para a escala visual da primeira sem mudar seus valores originais.
reescalar_para_eixo <- function(y2, y1) {
  # Guarde apenas valores finitos para calcular intervalos reais de cada série.
  y1_valido <- y1[is.finite(y1)]
  # Guarde apenas valores finitos para calcular intervalos reais de cada série.
  y2_valido <- y2[is.finite(y2)]
  # Interrompa quando uma série não tiver variação suficiente para construir dois eixos honestos.
  if (length(y1_valido) < 2L || length(y2_valido) < 2L || diff(range(y1_valido)) == 0 || diff(range(y2_valido)) == 0) {
    stop("As duas séries precisam ter pelo menos dois valores distintos para usar o duplo eixo.", call. = FALSE)
  }
  # Calcule quanto a amplitude da segunda série precisa ser ampliada ou reduzida.
  a <- diff(range(y1_valido)) / diff(range(y2_valido))
  # Alinhe o menor valor da segunda série ao menor valor da primeira.
  b <- min(y1_valido) - a * min(y2_valido)
  # Devolva os coeficientes e a cópia projetada apenas para o desenho no eixo esquerdo.
  list(a = a, b = b, y2_no_eixo_esquerdo = a * y2 + b)
}

# Construa a mesma moldura compacta em todos os tipos de gráfico.
mod_exploracao_visual_ui <- function(id, tipo) {
  ns <- shiny::NS(id)
  tagList(
    tags$h2(switch(
      tipo,
      histograma = "Histograma com densidade",
      caixa_violino = "Boxplot e violino",
      dispersao = "Dispersão e tendência",
      duplo_eixo = "Duas séries, dois eixos Y",
      barras = "Barras",
      rosca = "Rosca",
      matriz = "Matriz de dispersão",
      calor = "Mapa de calor de correlação"
    ), class = "h4 mb-2"),
    div(
      class = "alert alert-light border mb-3",
      icon("compass"), " Escolha as variáveis e observe o padrão. O gráfico orienta a próxima pergunta, mas não escolhe uma análise por você."
    ),
    bslib::layout_sidebar(
      fill = FALSE,
      sidebar = bslib::sidebar(width = 310, uiOutput(ns("controles"))),
      uiOutput(ns("aviso")),
      .visual_card_grafico("Retrato visual", ns("grafico"), if (tipo %in% c("matriz", "calor")) 620 else 460),
      bslib::accordion(
        bslib::accordion_panel("Ver o código R", verbatimTextOutput(ns("codigo"))),
        open = FALSE
      )
    )
  )
}

# Atenda aos oito gráficos com um motor comum e interfaces específicas.
mod_exploracao_visual_server <- function(id, data_rv, tipo) {
  shiny::moduleServer(id, function(input, output, session) {
    # Atualize as listas sempre que a base compartilhada mudar.
    escolhas <- reactive({
      dados <- data_rv()
      shiny::req(is.data.frame(dados))
      list(numericas = .visual_numericas(dados), categoricas = .visual_categoricas(dados), todas = names(dados))
    })

    # Mostre apenas controles que fazem sentido para a intenção visual escolhida.
    output$controles <- shiny::renderUI({
      opcoes <- escolhas()
      numericas <- opcoes$numericas
      categoricas <- opcoes$categoricas
      shiny::validate(shiny::need(length(numericas) > 0, "A base precisa ter ao menos uma variável numérica."))
      facetas <- c("Não repetir por grupo" = "nenhuma", stats::setNames(categoricas, categoricas))
      switch(
        tipo,
        histograma = tagList(
          tags$h5("Distribuição"),
          selectInput(session$ns("x"), "Variável numérica:", numericas),
          checkboxInput(session$ns("densidade"), "Sobrepor curva de densidade", TRUE),
          selectInput(session$ns("grupo"), "Cor por grupo:", c("Nenhum" = "nenhum", stats::setNames(categoricas, categoricas))),
          selectInput(session$ns("faceta"), "Repetir por:", facetas)
        ),
        caixa_violino = tagList(
          tags$h5("Centro e dispersão"),
          selectInput(session$ns("y"), "Variável numérica:", numericas),
          selectInput(session$ns("grupo"), "Comparar grupos:", c("Todos juntos" = "nenhum", stats::setNames(categoricas, categoricas))),
          radioButtons(session$ns("forma"), "Mostrar:", c("Boxplot" = "caixa", "Violino" = "violino", "Os dois" = "ambos"), inline = TRUE),
          selectInput(session$ns("faceta"), "Repetir por:", facetas)
        ),
        dispersao = tagList(
          tags$h5("Relação entre medidas"),
          selectInput(session$ns("x"), "Eixo X:", numericas),
          selectInput(session$ns("y"), "Eixo Y:", numericas, selected = numericas[min(2, length(numericas))]),
          selectInput(session$ns("grupo"), "Cor por grupo:", c("Nenhum" = "nenhum", stats::setNames(categoricas, categoricas))),
          selectInput(session$ns("suavizacao"), "Linha de tendência:", c("Sem linha" = "nenhuma", "Linear" = "lm", "Suave" = "loess")),
          selectInput(session$ns("faceta"), "Repetir por:", facetas)
        ),
        duplo_eixo = tagList(
          tags$h5("Duas medidas no mesmo X"),
          selectInput(session$ns("x"), "Eixo X ordenado:", opcoes$todas),
          selectInput(session$ns("y1"), "Primeira série, eixo esquerdo:", numericas),
          selectInput(session$ns("y2"), "Segunda série, eixo direito:", numericas, selected = numericas[min(2, length(numericas))])
        ),
        barras = tagList(
          tags$h5("Composição e categorias"),
          selectInput(session$ns("x"), "Categoria:", categoricas),
          selectInput(session$ns("y"), "Valor a somar:", c("Contagem de casos" = "nenhum", stats::setNames(numericas, numericas))),
          selectInput(session$ns("grupo"), "Preencher por grupo:", c("Nenhum" = "nenhum", stats::setNames(categoricas, categoricas))),
          selectInput(session$ns("posicao"), "Disposição:", c("Lado a lado" = "dodge", "Empilhadas" = "stack", "Proporção de 100%" = "fill")),
          selectInput(session$ns("faceta"), "Repetir por:", facetas)
        ),
        rosca = tagList(
          tags$h5("Poucas categorias"),
          selectInput(session$ns("x"), "Categoria:", categoricas),
          helpText("A rosca é mais legível com até seis categorias. Para comparar valores, prefira barras.")
        ),
        matriz = tagList(
          tags$h5("Muitas medidas"),
          selectizeInput(session$ns("vars"), "Variáveis numéricas, de 2 a 6:", numericas, multiple = TRUE, options = list(maxItems = 6)),
          helpText("Cada painel mostra a relação entre um par de variáveis.")
        ),
        calor = tagList(
          tags$h5("Relações lineares"),
          selectizeInput(session$ns("vars"), "Variáveis numéricas, de 2 a 12:", numericas, multiple = TRUE, options = list(maxItems = 12)),
          helpText("O mapa mostra correlações de Pearson. Ele resume associação, não causa e efeito.")
        )
      )
    })

    # A faceta segue a mesma regra do motor: só entra quando a coluna existe na base.
    aplicar_faceta_ocean <- function(grafico, faceta) {
      aplicar_faceta_ocean_pacote(grafico, data_rv(), faceta)
    }

    # Construa o gráfico selecionado com ggplot2 e dados completos para seus eixos.
    grafico <- reactive({
      dados <- data_rv()
      opcoes <- escolhas()
      shiny::req(nrow(dados) > 0)
      if (tipo == "histograma") {
        shiny::req(input$x)
        grupo <- input$grupo %||% "nenhum"
        desenhar_distribuicao(dados, input$x,
          tipo = if (isTRUE(input$densidade)) "densidade" else "histograma",
          classes = 30, grupo = grupo, faceta = input$faceta)
      } else if (tipo == "caixa_violino") {
        shiny::req(input$y)
        grupo <- input$grupo %||% "nenhum"
        desenhar_caixa_ocean(dados, input$y, grupo = grupo, forma = input$forma, faceta = input$faceta)
      } else if (tipo == "dispersao") {
        shiny::req(input$x, input$y)
        grupo <- input$grupo %||% "nenhum"
        desenhar_dispersao_ocean(dados, input$x, input$y, grupo = grupo,
          tendencia = input$suavizacao %||% "nenhuma", faceta = input$faceta)
      } else if (tipo == "duplo_eixo") {
        shiny::req(input$x, input$y1, input$y2)
        shiny::validate(shiny::need(input$y1 != input$y2, "Escolha duas variáveis numéricas diferentes."))
        completos <- stats::complete.cases(dados[c(input$x, input$y1, input$y2)])
        dados_plot <- dados[completos, , drop = FALSE]
        escala <- reescalar_para_eixo(dados_plot[[input$y2]], dados_plot[[input$y1]])
        dados_plot$y2_no_eixo_esquerdo <- escala$y2_no_eixo_esquerdo
        ggplot2::ggplot(dados_plot, ggplot2::aes(x = .data[[input$x]])) +
          ggplot2::geom_line(ggplot2::aes(y = .data[[input$y1]], color = input$y1), linewidth = 1) +
          ggplot2::geom_line(ggplot2::aes(y = .data$y2_no_eixo_esquerdo, color = input$y2), linewidth = 1) +
          ggplot2::scale_color_manual(values = c(cores_ocean()[["NAVY"]], cores_ocean()[["CORAL"]]), name = "Série") +
          ggplot2::scale_y_continuous(name = input$y1, sec.axis = ggplot2::sec_axis(~ (. - escala$b) / escala$a, name = input$y2)) +
          .visual_tema() + ggplot2::labs(title = "Duas séries em escalas diferentes", subtitle = "O eixo direito é reescalado a partir do esquerdo. Compare tendências, não alturas entre as duas séries.", x = input$x)
      } else if (tipo == "barras") {
        shiny::req(input$x)
        grupo <- input$grupo %||% "nenhum"
        y <- input$y %||% "nenhum"
        desenhar_barras_ocean(dados, input$x, grupo = grupo, peso = y,
          posicao = input$posicao, faceta = input$faceta)
      } else if (tipo == "rosca") {
        shiny::req(input$x)
        tabela <- as.data.frame(table(dados[[input$x]], useNA = "no"), stringsAsFactors = FALSE)
        names(tabela) <- c("categoria", "n")
        shiny::validate(shiny::need(nrow(tabela) > 0, "Não há categorias preenchidas para desenhar a rosca."), shiny::need(nrow(tabela) <= 6, "A rosca é reservada a até seis categorias. Use barras para comparar muitas categorias."))
        tabela$percentual <- tabela$n / sum(tabela$n) * 100
        tabela$rotulo <- paste0(formatC(tabela$percentual, digits = 1, format = "f", decimal.mark = ","), "%")
        ggplot2::ggplot(tabela, ggplot2::aes(x = 2, y = .data$n, fill = .data$categoria)) +
          ggplot2::geom_col(color = "white", linewidth = .6) +
          ggplot2::geom_text(ggplot2::aes(label = .data$rotulo), position = ggplot2::position_stack(vjust = .5), color = "white", fontface = "bold") +
          ggplot2::coord_polar(theta = "y") + ggplot2::xlim(.5, 2.5) +
          ggplot2::scale_fill_manual(values = rep(.visual_ocean, length.out = nrow(tabela))) +
          ggplot2::theme_void(base_size = 13) + ggplot2::labs(title = "Composição de poucas categorias", fill = input$x)
      } else if (tipo == "matriz") {
        vars <- input$vars %||% character()
        shiny::validate(shiny::need(length(vars) >= 2, "Escolha pelo menos duas variáveis numéricas."))
        pares <- expand.grid(x_var = vars, y_var = vars, stringsAsFactors = FALSE)
        pares <- pares[pares$x_var != pares$y_var, , drop = FALSE]
        painel <- do.call(rbind, lapply(seq_len(nrow(pares)), function(i) data.frame(x = dados[[pares$x_var[i]]], y = dados[[pares$y_var[i]]], x_var = pares$x_var[i], y_var = pares$y_var[i])))
        ggplot2::ggplot(painel, ggplot2::aes(x = .data$x, y = .data$y)) +
          ggplot2::geom_point(color = cores_ocean()[["TEAL"]], alpha = .55, na.rm = TRUE) +
          ggplot2::facet_grid(y_var ~ x_var, scales = "free") + .visual_tema() +
          ggplot2::theme(axis.title = ggplot2::element_blank(), strip.text = ggplot2::element_text(face = "bold")) +
          ggplot2::labs(title = "Matriz de dispersão", subtitle = "Cada painel compara duas variáveis. A diagonal permanece vazia porque uma variável contra ela mesma não acrescenta informação.")
      } else {
        vars <- input$vars %||% character()
        shiny::validate(shiny::need(length(vars) >= 2, "Escolha pelo menos duas variáveis numéricas."))
        matriz <- stats::cor(dados[vars], use = "pairwise.complete.obs")
        tabela <- as.data.frame(as.table(matriz), stringsAsFactors = FALSE)
        names(tabela) <- c("x", "y", "correlacao")
        tabela$rotulo <- formatC(tabela$correlacao, digits = 2, format = "f", decimal.mark = ",")
        ggplot2::ggplot(tabela, ggplot2::aes(x = .data$x, y = .data$y, fill = .data$correlacao)) +
          ggplot2::geom_tile(color = "white") + ggplot2::geom_text(ggplot2::aes(label = .data$rotulo), size = 3.8) +
          ggplot2::scale_fill_gradient2(low = cores_ocean()[["CORAL"]], mid = "white", high = cores_ocean()[["NAVY"]], midpoint = 0, limits = c(-1, 1), name = "Correlação") +
          .visual_tema() + ggplot2::coord_equal() + ggplot2::labs(title = "Mapa de calor de correlação", x = NULL, y = NULL)
      }
    })

    # Informe a limitação específica do duplo eixo antes que o aluno o interprete como comparação direta.
    output$aviso <- shiny::renderUI({
      if (tipo == "duplo_eixo") div(class = "alert alert-warning small", icon("triangle-exclamation"), " Os eixos usam escalas diferentes. Leia a direção e o momento das mudanças, não compare alturas das linhas como se estivessem na mesma unidade.") else NULL
    })

    # Mostre o gráfico reativo no painel central de largura limitada.
    output$grafico <- shiny::renderPlot(grafico())

    # Mostre o código essencial que corresponde à intenção escolhida.
    output$codigo <- shiny::renderText({
      switch(tipo,
        histograma = sprintf("desenhar_distribuicao(\n  dados, variavel = %s, tipo = %s, classes = 30,\n  grupo = %s, faceta = %s\n)",
          deparse(input$x %||% "variavel"),
          deparse(if (isTRUE(input$densidade)) "densidade" else "histograma"),
          deparse(input$grupo %||% "nenhum"), deparse(input$faceta %||% "nenhuma")),
        caixa_violino = sprintf("desenhar_caixa_ocean(\n  dados, variavel = %s, grupo = %s, forma = %s, faceta = %s\n)",
          deparse(input$y %||% "variavel"), deparse(input$grupo %||% "nenhum"),
          deparse(input$forma %||% "ambos"), deparse(input$faceta %||% "nenhuma")),
        dispersao = sprintf("desenhar_dispersao_ocean(\n  dados, x = %s, y = %s, grupo = %s, tendencia = %s, faceta = %s\n)",
          deparse(input$x %||% "x"), deparse(input$y %||% "y"), deparse(input$grupo %||% "nenhum"),
          deparse(input$suavizacao %||% "nenhuma"), deparse(input$faceta %||% "nenhuma")),
        duplo_eixo = "escala <- reescalar_para_eixo(dados$y2, dados$y1)\nggplot(dados, aes(x)) +\n  geom_line(aes(y = y1)) +\n  geom_line(aes(y = escala$y2_no_eixo_esquerdo)) +\n  scale_y_continuous(sec.axis = sec_axis(~ (. - escala$b) / escala$a))",
        barras = sprintf("desenhar_barras_ocean(\n  dados, variavel = %s, grupo = %s, peso = %s, posicao = %s, faceta = %s\n)",
          deparse(input$x %||% "categoria"), deparse(input$grupo %||% "nenhum"),
          deparse(input$y %||% "nenhum"), deparse(input$posicao %||% "dodge"),
          deparse(input$faceta %||% "nenhuma")),
        rosca = "ggplot(tabela, aes(x = 2, y = n, fill = categoria)) +\n  geom_col() + coord_polar(theta = 'y') + xlim(.5, 2.5)",
        matriz = "# A CatalyseR monta cada par de variáveis e os organiza em facet_grid().\nggplot(painel, aes(x, y)) + geom_point() + facet_grid(y_var ~ x_var)",
        calor = "correlacoes <- cor(dados[variaveis], use = 'pairwise.complete.obs')\nggplot(tabela, aes(x, y, fill = correlacao)) + geom_tile()"
      )
    })
  })
}
