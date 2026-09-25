# Laboratório de Conceitos — catálogo e Visão geral
# Este catálogo é a única fonte para a lista de opções e para o mapa pedagógico.

lab_placeholder <- function(titulo, desc, icone = "flask") {
  bslib::card(
    bslib::card_header(shiny::tagList(shiny::icon(icone), " ", titulo)),
    bslib::card_body(
      shiny::div(class = "alert alert-info", style = "margin-bottom:10px;", shiny::icon("hammer"), " Visualizador em construção."),
      shiny::p(desc),
      shiny::helpText("Quando for implementado, continuará autocontido: controles, gráfico e explicação, sem acessar dados de pesquisa.")
    )
  )
}

lab_item <- function(id, titulo, icone, status, classe, descricao, observar, tela) {
  list(id = id, titulo = titulo, icone = icone, status = status, classe = classe,
       descricao = descricao, observar = observar, tela = tela)
}

# A ordem daqui é a ordem usada tanto na lista do menu quanto no mapa pedagógico.
laboratorio_catalogo <- function() {
  list(
    lab_item("lab_visao_geral", "Visão geral", "compass", "Disponível", "success",
      "Um mapa pedagógico para escolher o conceito antes de experimentar.",
      "Escolha um item na barra lateral para entender a pergunta e o que observar.",
      function() mod_laboratorio_ui("laboratorio")),
    lab_item("lab_tlc", "Teorema do Limite Central", "bell", "Disponível", "success",
      "Mostra por que as médias de muitas amostras tendem a formar uma distribuição aproximadamente normal, mesmo quando a população não tem formato de sino.",
      "Varie n e veja as médias se concentrarem perto do valor verdadeiro.",
      function() mod_lab_tlc_ui("lab_tlc")),
    lab_item("lab_anova", "Simulador da ANOVA", "chart-column", "Disponível", "success",
      "Mostra se as diferenças entre médias de vários grupos se destacam diante da variação dentro deles.",
      "Aproxime ou afaste as médias e altere o desvio-padrão para ver F e o p-valor mudarem.",
      function() mod_lab_anova_ui("lab_anova")),
    lab_item("lab_teste_t", "Simulador do teste t", "arrows-left-right", "Disponível", "success",
      "Mostra como diferença entre duas médias, variação e tamanho amostral se combinam no teste t.",
      "Mude as médias, n e o desvio-padrão; p alto não equivale a ausência de efeito.",
      function() mod_lab_teste_t_ui("lab_teste_t")),
    lab_item("lab_lgn", "Lei dos Grandes Números", "arrow-trend-up", "Em construção", "secondary",
      "Mostrará a média acumulada se aproximando do valor esperado à medida que novas observações são acrescentadas.",
      "A ideia conversa diretamente com precisão de estimativas e tamanho amostral.",
      function() lab_placeholder("Lei dos Grandes Números", "A média amostral converge para o valor esperado à medida que n aumenta.", "arrow-trend-up")),
    lab_item("lab_ic", "Cobertura do IC", "bullseye", "Em construção", "secondary",
      "Mostrará muitas amostras e seus intervalos de confiança, alguns cobrindo e outros não cobrindo o parâmetro verdadeiro.",
      "A confiança é uma propriedade do procedimento repetido, não uma probabilidade retrospectiva de um único intervalo.",
      function() lab_placeholder("Cobertura do Intervalo de Confiança", "100 amostras, 100 intervalos: cerca de 95% contêm a média verdadeira.", "bullseye")),
    lab_item("lab_h0_pvalor", "Distribuição sob H0 / p-valor", "chart-area", "Em construção", "secondary",
      "Mostrará a distribuição esperada sob a hipótese nula, a estatística observada e a área correspondente ao p-valor.",
      "É a ponte entre uma curva teórica e a evidência contra H0.",
      function() lab_placeholder("Distribuição sob H0 / p-valor", "A distribuição nula, a estatística observada e a região crítica.", "chart-area")),
    lab_item("lab_curvas", "Curvas z / t / F / qui-quadrado", "wave-square", "Em construção", "secondary",
      "Permitirá comparar forma, graus de liberdade, quantis e áreas das distribuições usadas nos testes.",
      "Cada curva aparece quando o conceito é necessário, sem virar uma lista para decorar.",
      function() lab_placeholder("Curvas de distribuição", "Forma das curvas conforme os graus de liberdade; áreas e quantis.", "wave-square")),
    lab_item("lab_normal", "Distribuição Normal", "circle-nodes", "Em construção", "secondary",
      "Permitirá variar média, desvio-padrão e áreas sob a curva normal.",
      "É uma ponte entre probabilidade, amostragem e inferência.",
      function() lab_placeholder("Distribuição Normal", "Áreas sob a curva e visualização dos parâmetros média e desvio-padrão.", "circle-nodes")),
    lab_item("lab_binomial", "Distribuição Binomial", "cubes", "Em construção", "secondary",
      "Permitirá variar número de tentativas e probabilidade de sucesso em contagens binomiais.",
      "É a ponte direta para proporções e testes categóricos.",
      function() lab_placeholder("Distribuição Binomial", "Cálculo e simulação de probabilidades binomiais (n, p).", "cubes"))
  )
}

laboratorio_paineis <- function() {
  lapply(laboratorio_catalogo(), function(item) {
    bslib::nav_panel(title = item$titulo, value = item$id, icon = shiny::icon(item$icone), item$tela())
  })
}

mod_laboratorio_ui <- function(id) {
  ns <- shiny::NS(id)
  catalogo <- laboratorio_catalogo()
  escolhas <- vapply(catalogo, `[[`, character(1), "titulo")
  names(escolhas) <- vapply(catalogo, `[[`, character(1), "id")
  bslib::layout_sidebar(
    fill = FALSE,
    sidebar = bslib::sidebar(
      title = "Visualizadores", width = 290,
      shiny::radioButtons(ns("conceito"), label = NULL, choices = escolhas, selected = "lab_tlc"),
      shiny::helpText("Escolha um item para ler seu propósito antes de abrir a respectiva opção do menu.")
    ),
    shiny::div(style = "padding: 6px 2px 12px;",
      shiny::h4("Laboratório de Conceitos", style = "font-family:'Outfit',sans-serif; font-weight:700; color:#0F3B5F; margin-bottom:2px;"),
      shiny::p(style = "color:#495057; font-size:0.92rem; margin:0;", "Um ", shiny::strong("laboratório visual"), " para aprender fundamentos da estatística com situações artificiais. Nada daqui altera a pesquisa, entra em Inserir análise, Comunicação de Resultados ou Projeto R."),
      shiny::tags$hr(style = "margin:12px 0 10px;"), shiny::uiOutput(ns("conceito_ui"))
    )
  )
}

mod_laboratorio_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    catalogo <- laboratorio_catalogo()
    nomes <- vapply(catalogo, `[[`, character(1), "id")
    output$conceito_ui <- shiny::renderUI({
      escolha <- input$conceito
      if (is.null(escolha) || !escolha %in% nomes) escolha <- "lab_tlc"
      info <- catalogo[[match(escolha, nomes)]]
      shiny::div(class = "card border-0 shadow-sm", shiny::div(class = "card-body", style = "padding: 18px 20px;",
        shiny::tags$div(shiny::tags$h5(info$titulo, style = "display:inline; color:#0F3B5F; font-weight:700;"), shiny::tags$span(class = paste0("badge bg-", info$classe), style = "margin-left:8px;", info$status)),
        shiny::tags$p(style = "margin:14px 0 10px; font-size:1rem;", info$descricao),
        shiny::div(class = "alert alert-light border mb-0", shiny::tags$b("O que observar: "), info$observar)
      ))
    })
  })
}
