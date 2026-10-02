# Laboratório de Conceitos — catálogo e Visão geral
# Este catálogo é a única fonte para a lista de opções e para o mapa pedagógico.
# Cada item pode ter `grupo`; os cabeçalhos do menu nascem daí.
# PENDENCIA-V2: os itens com status "Em construção" (placeholders) viram visualizadores; ver Pendencias_CatalyseR.md.

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

lab_item <- function(id, titulo, icone, status, classe, descricao, observar, tela, grupo = NULL) {
  list(id = id, titulo = titulo, icone = icone, status = status, classe = classe,
       descricao = descricao, observar = observar, tela = tela, grupo = grupo)
}

# Um item do menu que reúne variações do mesmo conceito em abas (segunda faixa,
# como nos resultados das análises). Cada aba é um par c(título, conteúdo).
lab_abas <- function(id, titulo, ...) {
  abas <- list(...)
  paineis <- lapply(abas, function(a) {
    bslib::nav_panel(title = a$titulo, icon = if (!is.null(a$icone)) shiny::icon(a$icone), a$tela)
  })
  do.call(bslib::navset_card_tab, c(list(id = id, title = titulo), paineis))
}

lab_aba <- function(titulo, tela, icone = NULL) list(titulo = titulo, tela = tela, icone = icone)

# A ordem daqui é a ordem usada tanto na lista do menu quanto no mapa pedagógico.
# Os grupos seguem a pergunta que o aluno faz, não o nome da técnica.
laboratorio_catalogo <- function() {
  list(
    lab_item("lab_visao_geral", "Visão geral", "compass", "Disponível", "success",
      "Um mapa pedagógico para escolher o conceito antes de experimentar.",
      "Escolha um item na barra lateral para entender a pergunta e o que observar.",
      function() mod_laboratorio_ui("laboratorio")),

    # ---- Da amostra à população -------------------------------------------
    lab_item("lab_lgn", "Lei dos Grandes Números", "arrow-trend-up", "Em construção", "secondary",
      "Mostrará a média acumulada se aproximando do valor esperado à medida que novas observações são acrescentadas.",
      "A ideia conversa diretamente com precisão de estimativas e tamanho amostral.",
      function() lab_placeholder("Lei dos Grandes Números", "A média amostral converge para o valor esperado à medida que n aumenta.", "arrow-trend-up"),
      grupo = "Da amostra à população"),
    lab_item("lab_tlc", "Teorema do Limite Central", "bell", "Disponível", "success",
      "Mostra por que as médias de muitas amostras tendem a formar uma distribuição aproximadamente normal, mesmo quando a população não tem formato de sino. A aba Média está pronta; a aba Proporção virá depois.",
      "Varie n e veja as médias se concentrarem perto do valor verdadeiro.",
      function() lab_abas("lab_tabs_tlc", "Teorema do Limite Central",
        lab_aba("Média", mod_lab_tlc_ui("lab_tlc"), "bell"),
        lab_aba("Proporção", lab_placeholder("Limite Central para proporções", "A proporção amostral se aproxima da normal quando n cresce, e mais devagar quando p está perto de 0 ou 1.", "percent"), "percent")),
      grupo = "Da amostra à população"),
    lab_item("lab_ic", "Intervalo de confiança", "bullseye", "Em construção", "secondary",
      "Mostrará muitas amostras e seus intervalos de confiança, alguns cobrindo e outros não cobrindo o parâmetro verdadeiro.",
      "A confiança é uma propriedade do procedimento repetido, não uma probabilidade retrospectiva de um único intervalo.",
      function() lab_placeholder("Intervalo de confiança", "100 amostras, 100 intervalos: cerca de 95% contêm a média verdadeira.", "bullseye"),
      grupo = "Da amostra à população"),

    # ---- Distribuições ------------------------------------------------------
    lab_item("lab_distribuicoes", "Distribuições", "chart-line", "Em construção", "secondary",
      "Reunirá as distribuições que aparecem nos testes: Normal (com a escala z), Binomial, Poisson, t de Student, F e qui-quadrado, com parâmetros, áreas e quantis.",
      "Cada curva aparece quando o conceito é necessário, sem virar uma lista para decorar.",
      function() lab_abas("lab_tabs_distribuicoes", "Distribuições",
        lab_aba("Normal (z)", lab_placeholder("Distribuição Normal e a normal padrão (z)", "Áreas sob a curva, média e desvio-padrão; a escala z é a normal com média 0 e desvio-padrão 1.", "circle-nodes"), "circle-nodes"),
        lab_aba("Binomial", lab_placeholder("Distribuição Binomial", "Cálculo e simulação de probabilidades binomiais (n, p).", "cubes"), "cubes"),
        lab_aba("Poisson", lab_placeholder("Distribuição de Poisson", "Contagens de eventos raros e a relação entre média e variância.", "dice"), "dice"),
        lab_aba("t de Student", lab_placeholder("Distribuição t de Student", "Forma da curva conforme os graus de liberdade, comparada à normal; quantis e áreas.", "wave-square"), "wave-square"),
        lab_aba("F", lab_placeholder("Distribuição F", "Forma da curva conforme os graus de liberdade do numerador e do denominador; área acima do F observado.", "chart-area"), "chart-area"),
        lab_aba("Qui-quadrado", lab_placeholder("Distribuição qui-quadrado", "Forma da curva conforme os graus de liberdade; área acima do valor observado.", "chart-simple"), "chart-simple")),
      grupo = "Distribuições"),

    # ---- Testando hipóteses -------------------------------------------------
    lab_item("lab_h0_pvalor", "O p-valor", "chart-area", "Em construção", "secondary",
      "Mostrará a distribuição esperada sob a hipótese nula, a estatística observada e a área correspondente ao p-valor, e como o mesmo p-valor surge por permutação.",
      "É a ponte entre uma curva teórica e a evidência contra H0.",
      function() lab_abas("lab_tabs_pvalor", "O p-valor",
        lab_aba("Distribuição sob H0", lab_placeholder("Distribuição sob H0", "A distribuição nula, a estatística observada e a região crítica.", "chart-area"), "chart-area"),
        lab_aba("Permutação", lab_placeholder("O p-valor por permutação", "Embaralhar os rótulos dos grupos muitas vezes e ver onde cai o valor observado.", "shuffle"), "shuffle")),
      grupo = "Testando hipóteses"),
    lab_item("lab_erros_poder", "Erros e poder do teste", "scale-balanced", "Em construção", "secondary",
      "Mostrará os erros tipo I e tipo II e como o poder do teste cresce com o tamanho do efeito, o n e o nível de significância.",
      "Ajuda a entender por que um p-valor alto não prova ausência de efeito.",
      function() lab_placeholder("Erros e poder do teste", "Erro tipo I, erro tipo II e poder, variando efeito, n e alfa.", "scale-balanced"),
      grupo = "Testando hipóteses"),
    lab_item("lab_simulador_testes", "Simulador de testes", "flask-vial", "Disponível", "success",
      "Reúne simuladores para ver como diferença, variação e tamanho amostral se combinam em cada teste. Teste t e ANOVA estão prontos; qui-quadrado e correlação virão depois.",
      "Mude as médias, n e o desvio-padrão; p alto não equivale a ausência de efeito.",
      function() lab_abas("lab_tabs_simulador", "Simulador de testes",
        lab_aba("Teste t", mod_lab_teste_t_ui("lab_teste_t"), "arrows-left-right"),
        lab_aba("ANOVA", mod_lab_anova_ui("lab_anova"), "chart-column"),
        lab_aba("Qui-quadrado", lab_placeholder("Simulador do qui-quadrado", "Tabelas de contagem, esperado sob independência e a estatística qui-quadrado.", "table-cells"), "table-cells"),
        lab_aba("Correlação", lab_placeholder("Simulador da correlação", "Nuvem de pontos, coeficiente de correlação e p-valor conforme força e n.", "chart-line"), "chart-line")),
      grupo = "Testando hipóteses"),

    # ---- Olhando os dados ---------------------------------------------------
    lab_item("lab_anscombe", "Por que olhar os dados?", "eye", "Em construção", "secondary",
      "Mostrará conjuntos com as mesmas estatísticas resumo e formas totalmente diferentes (Anscombe, Datasaurus).",
      "Números iguais não garantem dados parecidos: o gráfico vem antes do teste.",
      function() lab_placeholder("Por que olhar os dados?", "Mesmas médias, variâncias e correlação, gráficos completamente diferentes.", "eye"),
      grupo = "Olhando os dados"),
    lab_item("lab_teste_fila", "Teste de fila", "users-viewfinder", "Em construção", "secondary",
      "Mostrará o gráfico real escondido entre gráficos gerados sob H0, para o aluno tentar apontar qual é o verdadeiro.",
      "Se você não distingue o gráfico real dos falsos, talvez não haja padrão.",
      function() lab_placeholder("Teste de fila", "Um gráfico real entre vários gerados por permutação: você acha qual?", "users-viewfinder"),
      grupo = "Olhando os dados"),
    lab_item("lab_correlacao", "Correlação e dispersão", "chart-line", "Em construção", "secondary",
      "Mostrará como formas diferentes de nuvem de pontos dão o mesmo r, e como um ponto extremo muda a correlação.",
      "Correlação mede associação linear, não causa nem qualquer forma de relação.",
      function() lab_placeholder("Correlação e dispersão", "Formas de dispersão, valores extremos e o coeficiente de correlação.", "chart-line"),
      grupo = "Olhando os dados")
  )
}

# Monta os itens do menu, com um cabeçalho cada vez que o grupo muda.
laboratorio_paineis <- function() {
  saida <- list()
  grupo_atual <- NULL
  for (item in laboratorio_catalogo()) {
    if (!is.null(item$grupo) && !identical(item$grupo, grupo_atual)) {
      saida[[length(saida) + 1]] <- bslib::nav_item(
        shiny::div(class = "dropdown-header fw-bold text-uppercase small", item$grupo))
      grupo_atual <- item$grupo
    }
    saida[[length(saida) + 1]] <- bslib::nav_panel(
      title = item$titulo, value = item$id, icon = shiny::icon(item$icone), item$tela())
  }
  saida
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
