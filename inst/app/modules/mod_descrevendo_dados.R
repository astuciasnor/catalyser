# Cinco seções; cálculos e código de estudo vêm do mesmo motor.
if (file.exists(file.path("..", "..", "R", "descrevendo_dados.R"))) {
  source(file.path("..", "..", "R", "descrevendo_dados.R"), encoding = "UTF-8", local = TRUE)
} else {
  for (nome in c(
    "descricao_catalogo", "descricao_tipo", "exploracao_tipo_variavel",
    "catalyser_codigo_descricao", "catalyser_descricao", "cores_ocean", "tema_ocean",
    "aplicar_faceta_ocean", "desenhar_distribuicao", "desenhar_barras_ocean",
    "desenhar_caixa_ocean", "desenhar_dispersao_ocean", "resumir_continuas",
    "tabela_frequencia_exploratoria", "exploracao_tipos", "exploracao_base_visual",
    "exploracao_cores", "exploracao_grupo", "exploracao_retratos", "exploracao_mapa_ausentes", "exploracao_saude", "exploracao_normalidade_grupos"
  )) {
    assign(nome, getFromNamespace(nome, "catalyser"), envir = globalenv())
  }
}

# Cada entrada conserva a base exata: mudar os dados pede uma nova leitura.
exploracao_ficha_ler <- function(ficha, dados) {
  entradas <- ficha()
  iguais <- which(vapply(entradas, function(x) identical(x$dados, dados), logical(1)))
  if (length(iguais)) entradas[[iguais[1]]]$leituras else NULL
}

exploracao_ficha_gravar <- function(ficha, dados, nome, tipo) {
  entradas <- shiny::isolate(ficha())
  iguais <- which(vapply(entradas, function(x) identical(x$dados, dados), logical(1)))
  i <- if (length(iguais)) iguais[1] else length(entradas) + 1L
  entrada <- if (length(iguais)) entradas[[i]] else list(dados = dados, leituras = character())
  entrada$leituras[nome] <- tipo
  entradas[[i]] <- entrada
  ficha(entradas)
}

# Os retratos têm espaço próprio; detalhes e edição ficam nas abas seguintes.
mod_explorar_dataset_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    tags$h2("Explorar Dataset", class = "h4 mb-1"),
    p("Confira o conjunto, observe cada variável e escolha a próxima pergunta.", class = "text-muted small mb-3"),
    mod_seletor_base_analise_ui(ns("base"), compacto = TRUE),
    div(class = "exploracao-espaco",
      bslib::navset_card_tab(id = ns("abas"), title = "Painel de Resultados da Exploração:",
        bslib::nav_panel("Painel de retratos", value = "panorama",
          uiOutput(ns("indicadores")), uiOutput(ns("saude")),
          selectInput(ns("pagina_retratos"), "Página de variáveis:", choices = c("1" = 1), width = "160px"),
          uiOutput(ns("retratos_plot")),
          tags$details(tags$summary("Leitura e código R"), uiOutput(ns("panorama_narrativa")), verbatimTextOutput(ns("panorama_codigo")))),
        bslib::nav_panel("Tabela", value = "tabela", DT::DTOutput(ns("panorama_tabela"))),
        bslib::nav_panel("Mapa de ausentes", value = "ausentes", plotOutput(ns("mapa_ausentes"), height = "480px")),
        bslib::nav_panel("Ficha de variáveis", value = "ficha",
          p("A leitura é um palpite. Corrija uma vez para as telas deste menu. Os valores da base permanecem iguais.", class = "text-muted small"),
          bslib::layout_columns(col_widths = c(5, 5, 2),
            selectInput(ns("ficha_variavel"), "Variável:", choices = NULL),
            selectInput(ns("ficha_tipo"), "Como ler:", choices = c("Detectar automaticamente" = "automatico", "Categórica nominal", "Categórica ordinal", "Numérica discreta", "Numérica contínua")),
            div(class = "pt-4", actionButton(ns("ficha_salvar"), "Confirmar", class = "btn-outline-primary"))),
          uiOutput(ns("ficha_pista")), DT::DTOutput(ns("ficha_tabela"))),
        bslib::nav_panel("Inserir análise", value = "inserir", icon = icon("bookmark"),
          p("Guarde o panorama e a primeira página dos retratos no Projeto R.", class = "small text-muted"),
          actionButton(ns("panorama_executar"), "Atualizar panorama para inserir", icon = icon("arrows-rotate"), class = "btn-outline-primary mb-3"),
          selectInput(ns("previa_id"), "Resultados disponíveis:", choices = NULL),
          uiOutput(ns("resumo_selecao")), mod_registrar_execucao_ui(ns("registrar")))
      )
    )
  )
}

# A tela de Conhecer as Variáveis reúne tabelas descritivas e um apoio visual.
mod_conhecer_variaveis_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    tags$h2("Conhecer as Variáveis", class = "h4 mb-1"),
    div(class = "alert alert-light border mb-2",
      strong("Aqui a tabela conduz a leitura. "),
      "Os gráficos são apoios para interpretar. Para ajustar figuras, use VISUALIZANDO OS DADOS, no mesmo menu. Estas frequências são descritivas; testes ficam em Frequências e Proporções."),
    mod_seletor_base_analise_ui(ns("base"), compacto = TRUE),
    bslib::navset_card_tab(
      id = ns("abas_conhecer"),
      title = "Painel de Resultados das Variáveis:",
      bslib::nav_panel(
        "Resumo das contínuas", value = "resumo",
        bslib::layout_columns(
          col_widths = c(7, 5),
          bslib::card(
            bslib::card_header("Resumo essencial"),
            bslib::card_body(DT::DTOutput(ns("tabela_resumo")), uiOutput(ns("leitura_resumo")))
          ),
          bslib::card(
            bslib::card_header("Distribuição da variável selecionada"),
            bslib::card_body(
              radioButtons(ns("tipo_distribuicao"), NULL,
                choices = c("Densidade" = "densidade", "Polígono de frequência" = "poligono"),
                selected = "densidade", inline = TRUE),
              plotOutput(ns("grafico_resumo"), height = "390px")
            )
          )
        )
      ),
      bslib::nav_panel(
        "Tabelas de frequência", value = "frequencias",
        bslib::layout_columns(
          col_widths = c(7, 5),
          bslib::card(
            bslib::card_header("Frequências e acumuladas"),
            bslib::card_body(
              bslib::layout_columns(
                col_widths = c(5, 3, 4),
                selectInput(ns("variavel_frequencia"), "Variável:", choices = NULL),
                uiOutput(ns("controle_classes")),
                uiOutput(ns("controle_ordem"))
              ),
              DT::DTOutput(ns("tabela_frequencia")),
              uiOutput(ns("leitura_frequencia"))
            )
          ),
          bslib::card(
            bslib::card_header("Apoio visual"),
            bslib::card_body(plotOutput(ns("grafico_frequencia"), height = "410px"))
          )
        )
      ),
      bslib::nav_panel(
        "Inserir análise", value = "inserir", icon = icon("bookmark"),
        p("Guarde a tabela e seu gráfico de apoio no Projeto R."),
        mod_registrar_execucao_ui(ns("registrar"))
      )
    )
  )
}

# Executa o retrato consolidado sem duplicar o motor gráfico de Visualização.
mod_conhecer_variaveis_server <- function(id, dados_rv, registro_bases_rv,
                                          cache_bases_rv, revisao_origem_rv,
                                          registro_execucoes_rv, contador_execucoes_rv, ficha_rv = shiny::reactiveVal(list())) {
  shiny::moduleServer(id, function(input, output, session) {
    seletor <- mod_seletor_base_analise_server(
      "base", dados_rv, registro_bases_rv, cache_bases_rv, revisao_origem_rv,
      finalidade_preferida = "geral", nome_analise = "Conhecer as Variáveis"
    )
    contexto <- seletor$contexto
    dados <- seletor$dados

    variaveis_continuas <- reactive({
      base <- dados()
      tipos <- exploracao_tipos(base, exploracao_ficha_ler(ficha_rv, base))
      names(tipos)[tipos == "Numérica contínua"]
    })
    resumo_bruto <- reactive({
      vars <- variaveis_continuas()
      validate(need(length(vars), "A base selecionada não possui variável reconhecida como numérica contínua."))
      resumir_continuas(dados(), vars)
    })
    resumo_exibicao <- reactive({
      tab <- resumo_bruto()
      numericas <- c("media", "mediana", "desvio_padrao", "minimo", "maximo")
      tab[numericas] <- lapply(tab[numericas], function(x) round(x, 3))
      tab
    })
    output$tabela_resumo <- DT::renderDT({
      tab <- resumo_exibicao()
      nomes_exibidos <- c("Variável", "n válidos", "Ausentes", "Média", "Mediana",
                          "Desvio-padrão", "Mínimo", "Máximo", "Pista de assimetria")
      widget <- DT::datatable(tab, rownames = FALSE, selection = list(mode = "single", selected = 1),
        colnames = nomes_exibidos,
        options = list(pageLength = 10, scrollX = TRUE, dom = "tip",
          columnDefs = list(list(targets = 8, visible = FALSE))))
      widget <- DT::formatStyle(widget, "n_validos", background = DT::styleColorBar(range(tab$n_validos), "#E7EFEA"), backgroundSize = "95% 75%", backgroundRepeat = "no-repeat", backgroundPosition = "center")
      widget <- DT::formatStyle(widget, "ausentes", backgroundColor = DT::styleInterval(0, c("transparent", "#FFF3CD")))
      DT::formatStyle(widget, "pista_assimetria", target = "row",
        backgroundColor = DT::styleEqual(c(FALSE, TRUE), c("transparent", "#FFF3CD")))
    })
    variavel_resumo <- reactive({
      linha <- input$tabela_resumo_rows_selected
      if (!length(linha)) linha <- 1L
      resumo_bruto()$variavel[min(linha, nrow(resumo_bruto()))]
    })
    output$grafico_resumo <- renderPlot({
      desenhar_distribuicao(dados(), variavel_resumo(), tipo = input$tipo_distribuicao %||% "densidade")
    })
    output$leitura_resumo <- renderUI({
      linha <- resumo_bruto()[match(variavel_resumo(), resumo_bruto()$variavel), ]
      pista <- if (isTRUE(linha$pista_assimetria)) " Média e mediana estão afastadas, uma pista de assimetria que merece inspeção no gráfico." else " Média e mediana estão próximas."
      div(class = "alert alert-light border mt-2 mb-0",
        sprintf("%s tem %d valores válidos, %d ausentes e amplitude de %s a %s.",
          linha$variavel, linha$n_validos, linha$ausentes,
          formatC(linha$minimo, digits = 3, format = "fg", decimal.mark = ","),
          formatC(linha$maximo, digits = 3, format = "fg", decimal.mark = ",")), pista)
    })

    observe({
      base <- dados()
      opcoes <- names(base)[vapply(base, function(x) descricao_tipo(x) %in% c("Numérica", "Categórica"), logical(1))]
      atual <- isolate(input$variavel_frequencia)
      if (is.null(atual) || !atual %in% opcoes) atual <- opcoes[1]
      updateSelectInput(session, "variavel_frequencia", choices = opcoes, selected = atual)
    })
    tipo_frequencia <- reactive({
      req(input$variavel_frequencia)
      exploracao_tipos(dados(), exploracao_ficha_ler(ficha_rv, dados()))[[input$variavel_frequencia]]
    })
    output$controle_classes <- renderUI({
      if (!identical(tipo_frequencia(), "Numérica contínua")) return(NULL)
      x <- dados()[[input$variavel_frequencia]]
      padrao <- max(2L, grDevices::nclass.Sturges(x[is.finite(x)]))
      numericInput(session$ns("classes_frequencia"), "Número de classes:", value = padrao, min = 2, max = 100)
    })
    output$controle_ordem <- renderUI({
      if (!grepl("nominal", tipo_frequencia(), fixed = TRUE)) return(NULL)
      checkboxInput(session$ns("ordenar_nominal"), "Ordenar pela frequência", TRUE)
    })
    frequencia_bruta <- reactive({
      req(input$variavel_frequencia)
      tabela_frequencia_exploratoria(dados(), input$variavel_frequencia, tipo_frequencia(),
        classes = input$classes_frequencia, ordenar_nominal = input$ordenar_nominal %||% TRUE)
    })
    output$tabela_frequencia <- DT::renderDT({
      tab <- frequencia_bruta()
      nomes_exibidos <- c(if (identical(tipo_frequencia(), "Numérica contínua")) "Classe" else "Categoria",
                          if (identical(tipo_frequencia(), "Numérica contínua")) "Frequência" else "Contagem",
                          "Proporção", "Frequência acumulada",
                          "Proporção acumulada", "Modal")
      widget <- DT::datatable(tab, rownames = FALSE, selection = "none",
        colnames = nomes_exibidos,
        options = list(pageLength = 12, scrollX = TRUE, dom = "tip",
          columnDefs = list(list(targets = ncol(tab) - 1L, visible = FALSE))))
      widget <- DT::formatPercentage(widget, c("proporcao", "prop_acumulada"), digits = 1)
      DT::formatStyle(widget, "modal", target = "row",
        backgroundColor = DT::styleEqual(c(FALSE, TRUE), c("transparent", "#DDF1EE")))
    })
    output$grafico_frequencia <- renderPlot({
      if (identical(tipo_frequencia(), "Numérica contínua")) {
        desenhar_distribuicao(dados(), input$variavel_frequencia, tipo = "histograma",
          classes = input$classes_frequencia)
      } else desenhar_barras_ocean(dados(), input$variavel_frequencia)
    })
    output$leitura_frequencia <- renderUI({
      tab <- frequencia_bruta()
      rotulo <- names(tab)[1]
      moda <- paste(tab[[rotulo]][tab$modal], collapse = ", ")
      ausentes <- sum(is.na(dados()[[input$variavel_frequencia]]))
      aviso <- if (grepl("nominal", tipo_frequencia(), fixed = TRUE))
        " A acumulada tem leitura limitada porque categorias nominais não possuem ordem natural." else " A acumulada segue a ordem natural dos valores ou das classes."
      div(class = "alert alert-light border mt-2 mb-0",
        sprintf("%s é a classe ou categoria modal, com %d observações. Há %d valores ausentes.",
          moda, max(tab$frequencia), ausentes), aviso)
    })

    ultima_saida <- reactiveVal("resumo")
    observeEvent(input$abas_conhecer, {
      if (input$abas_conhecer %in% c("resumo", "frequencias")) ultima_saida(input$abas_conhecer)
    }, ignoreInit = FALSE)
    estado_execucao <- reactive({
      aba <- ultima_saida()
      if (identical(aba, "frequencias")) {
        req(input$variavel_frequencia)
        list(analise_id = "descricao_conhecer", tipo = "descricao_exploratoria",
          titulo = paste("Frequências de", input$variavel_frequencia),
          parametros = list(analise = "frequencia_exploratoria", variavel = input$variavel_frequencia,
                            tipo = tipo_frequencia(), classes = input$classes_frequencia),
          saidas_disponiveis = c("narrativa", "tabela", "grafico"),
          codigo_r = sprintf("tabela_frequencia_exploratoria(dados, %s, %s)",
            deparse(input$variavel_frequencia), deparse(tipo_frequencia())))
      } else {
        list(analise_id = "descricao_conhecer", tipo = "descricao_exploratoria",
          titulo = "Resumo das variáveis contínuas",
          parametros = list(leituras = exploracao_tipos(dados(), exploracao_ficha_ler(ficha_rv, dados())), analise = "resumo_continuas", variavel = variavel_resumo(), variaveis = variaveis_continuas()),
          saidas_disponiveis = c("narrativa", "tabela", "grafico"),
          codigo_r = "resumir_continuas(dados, variaveis_continuas)"
        )
      }
    })
    mod_registrar_execucao_server(
      "registrar", estado_execucao, contexto, registro_execucoes_rv, contador_execucoes_rv,
      revisao_origem_rv, registro_bases_rv, cache_bases_rv, "descricao_conhecer",
      "O retrato das variáveis", reactive(list(ultima_saida(), input$variavel_frequencia, variavel_resumo()))
    )
  })
}

mod_descrevendo_dados_ui <- function(id, area) {
  if (identical(area, "explorar")) return(mod_explorar_dataset_ui(id))
  if (identical(area, "descrever")) return(mod_conhecer_variaveis_ui(id))
  ns <- shiny::NS(id)
  catalogo <- descricao_catalogo()[[area]]
  titulo <- switch(area,
    explorar = "Explorar Dataset", descrever = "Conhecer as Variáveis",
    relacoes = "Encontrar Relações", pressupostos = "Avaliar Pressupostos",
    transformar = "Transformar Variáveis")
  painel <- function(modo, rotulo) {
    prefixo <- function(sufixo) ns(paste0(modo, "_", sufixo))
    bslib::nav_panel(
      title = rotulo, value = modo,
      bslib::layout_sidebar(
        fill = FALSE, fillable = FALSE,
        sidebar = bslib::sidebar(
          width = 285,
          # A seleção começa pela pergunta, não por uma lista de técnicas.
          if (!area %in% "explorar") selectInput(prefixo("variavel"), if (area == "relacoes") "Primeira variável:" else "Variável:", choices = NULL),
          if (modo %in% c("correlacao", "dispersao", "marginais", "relacao")) selectInput(prefixo("outra"), "Segunda variável:", choices = NULL),
          # O aluno pode corrigir a leitura automática quando conhece melhor a coluna.
          if (modo %in% c("retrato", "relacao")) selectInput(prefixo("tipo"), "Como ler esta variável?", choices = c("Detectar automaticamente" = "automatico", "Categórica nominal" = "Categórica nominal", "Categórica ordinal" = "Categórica ordinal", "Numérica discreta" = "Numérica discreta", "Numérica contínua" = "Numérica contínua")),
          # A segunda leitura só é necessária ao explorar uma relação.
          if (modo == "relacao") selectInput(prefixo("tipo_outra"), "Como ler a segunda variável?", choices = c("Detectar automaticamente" = "automatico", "Categórica nominal" = "Categórica nominal", "Categórica ordinal" = "Categórica ordinal", "Numérica discreta" = "Numérica discreta", "Numérica contínua" = "Numérica contínua")),
          if (modo %in% c("grupos", "normalidade")) selectInput(prefixo("grupo"), "Agrupar por:", choices = NULL),
          if (modo == "correlacao") radioButtons(prefixo("metodo"), "Método:", c("Pearson" = "pearson", "Spearman" = "spearman")),
          if (modo == "histograma") numericInput(prefixo("classes"), "Número de classes:", 30, min = 2, max = 200, step = 1),
          if (modo == "boxplot") selectInput(prefixo("forma"), "Apresentação:", c("Boxplot" = "boxplot", "Violino" = "violino", "Ambos" = "ambos")),
          if (modo == "dispersao") selectInput(prefixo("tendencia"), "Linha de tendência:", c("Linear" = "linear", "Suavizada (LOESS)" = "loess", "Sem linha" = "nenhuma")),
          if (modo == "outliers") numericInput(prefixo("limite_z"), "Sinalizar quando |z| for maior que:", 3, min = .1, step = .1),
          actionButton(prefixo("executar"), "Executar análise", icon = icon("play"), class = "btn-primary"),
          helpText("A CatalyseR sugere um caminho. A decisão continua sendo sua, junto com a pergunta e o delineamento."),
          if (area == "pressupostos") helpText("Aqui você examina variáveis. Para validar um modelo, examine seus resíduos e o delineamento. Nenhum valor é removido automaticamente."),
          if (area == "transformar") helpText("Compare as transformações sugeridas. A base original não é alterada; esta seção não aplica mudanças à Trilha de Preparo.")
        ),
        uiOutput(prefixo("status")),
        conditionalPanel(sprintf("output['%s'] === true", prefixo("pronto")),
          uiOutput(prefixo("narrativa")),
          uiOutput(prefixo("caminho")),
          uiOutput(prefixo("painel_grafico")),
          DT::DTOutput(prefixo("tabela")),
          # O convite para abrir mais medidas só aparece quando há algo adicional a mostrar.
          uiOutput(prefixo("painel_detalhes")),
          tags$details(tags$summary("Ver código R e saída do console"),
                       verbatimTextOutput(prefixo("codigo")), verbatimTextOutput(prefixo("console")))
        )
      )
    )
  }
  tagList(
    # O título curto aproxima a pergunta do conteúdo e elimina o vazio superior.
    tags$h2(titulo, id = ns("titulo"), class = "h4 mb-1"),
    # Um contêiner comum deixa a página crescer com os resultados, em vez de
    # comprimir gráfico e registrador na altura restante da janela.
    div(class = "descricao-conteudo", id = ns("conteudo"),
      # Uma única linha basta para escolher a base nas cinco perguntas do menu.
      mod_seletor_base_analise_ui(ns("base"), compacto = TRUE),
      do.call(bslib::navset_card_tab, c(
        list(id = ns("abas"), title = paste0("Painel de Resultados de ", titulo, ":"), wrapper = function(...) bslib::card_body(..., fillable = FALSE)),
        unname(Map(painel, unname(catalogo), names(catalogo))),
        list(bslib::nav_panel(
          "Inserir análise", value = "inserir", icon = icon("bookmark"),
          p("Escolha um resultado já executado. Trocar a variável e executar cria outra opção; não substitui o resultado anterior."),
          selectInput(ns("previa_id"), "Resultados disponíveis para inserir:", choices = NULL),
          uiOutput(ns("resumo_selecao")),
          mod_registrar_execucao_ui(ns("registrar"))
        ))
      ))
    )
  )
}

mod_descrevendo_dados_server <- function(id, area, dados_rv, registro_bases_rv,
                                        cache_bases_rv, revisao_origem_rv,
                                        registro_execucoes_rv, contador_execucoes_rv, ficha_rv = shiny::reactiveVal(list())) {
  if (identical(area, "descrever")) return(mod_conhecer_variaveis_server(
    id, dados_rv, registro_bases_rv, cache_bases_rv, revisao_origem_rv,
    registro_execucoes_rv, contador_execucoes_rv, ficha_rv
  ))
  moduleServer(id, function(input, output, session) {
    catalogo <- descricao_catalogo()[[area]]
    seletor <- mod_seletor_base_analise_server(
      "base", dados_rv, registro_bases_rv, cache_bases_rv, revisao_origem_rv,
      finalidade_preferida = "geral", nome_analise = "Esta área"
    )
    historico <- reactiveVal(list())
    resultados <- reactiveVal(list())
    selecionada <- reactiveVal("")
    contexto_anterior <- reactiveVal(NULL)
    # A fotografia de parâmetros pertence a esta revisão e receita, nunca à próxima.
    contexto <- reactive({
      ctx <- seletor$contexto()
      list(base_id = ctx$base_id, versao = ctx$versao_receita,
           revisao = revisao_origem_rv(), dados = seletor$dados())
    })
    leituras <- reactive(exploracao_tipos(seletor$dados(), exploracao_ficha_ler(ficha_rv, seletor$dados())))
    if (identical(area, "explorar")) {
      observe({
        base <- seletor$dados(); req(is.data.frame(base))
        atual <- isolate(input$ficha_variavel)
        updateSelectInput(session, "ficha_variavel", choices = names(base), selected = if (length(atual) && atual %in% names(base)) atual else names(base)[1])
        paginas <- seq_len(max(1L, ceiling(ncol(base) / 12)))
        updateSelectInput(session, "pagina_retratos", choices = paginas, selected = min(as.integer(isolate(input$pagina_retratos) %||% 1), max(paginas)))
      })
      observeEvent(list(input$ficha_variavel, leituras()), {
        req(input$ficha_variavel)
        escolha <- exploracao_ficha_ler(ficha_rv, seletor$dados())[input$ficha_variavel]
        updateSelectInput(session, "ficha_tipo", selected = if (length(escolha) && !is.na(escolha)) escolha else "automatico")
      })
      observeEvent(input$ficha_salvar, {
        req(input$ficha_variavel, input$ficha_tipo)
        base <- seletor$dados()
        # Uma medida só pode ser lida como numérica quando seus valores são numéricos.
        if (grepl("Numérica", input$ficha_tipo) && !is.numeric(base[[input$ficha_variavel]])) {
          showNotification("Prepare a coluna como numérica antes de confirmar esta leitura.", type = "warning"); return()
        }
        exploracao_ficha_gravar(ficha_rv, base, input$ficha_variavel, input$ficha_tipo)
        showNotification("Leitura compartilhada com as telas deste menu.", type = "message")
      })
      output$ficha_pista <- renderUI({
        req(input$ficha_variavel)
        p(paste("Palpite pelo nome, classe e valores:", exploracao_tipo_variavel(seletor$dados()[[input$ficha_variavel]], input$ficha_variavel)), class = "small text-muted")
      })
      output$ficha_tabela <- DT::renderDT({
        base <- seletor$dados(); tipos <- leituras()
        DT::datatable(data.frame(Variável = names(base), Leitura = unname(tipos), Classe = vapply(base, function(x) paste(class(x), collapse = "/"), character(1))),
          rownames = FALSE, options = list(dom = "t", scrollX = TRUE, pageLength = 12))
      })
      output$indicadores <- renderUI({
        base <- seletor$dados(); req(is.data.frame(base)); tipos <- leituras()
        valores <- c("Observações" = nrow(base), "Variáveis" = ncol(base),
          "Preenchidas" = sprintf("%.1f%%", if (length(base) && nrow(base)) 100 * mean(!is.na(base)) else 0),
          "Medidas / grupos" = paste(sum(tipos == "Numérica contínua"), sum(grepl("Categ", tipos)), sep = " / "))
        do.call(bslib::layout_columns, c(lapply(names(valores), function(nome) div(class = "exploracao-indicador", span(nome), tags$strong(valores[[nome]]))), list(col_widths = c(3, 3, 3, 3))))
      })
      output$saude <- renderUI({
        tab <- exploracao_saude(seletor$dados())
        div(class = "exploracao-saude", lapply(seq_len(nrow(tab)), function(i) span(class = if (tab$quantidade[i]) "badge text-bg-warning" else "badge text-bg-light", paste(tab$verificacao[i], tab$quantidade[i], sep = ": "))),
          span("Linhas iguais são um alerta para conferir, não uma ordem para excluir.", class = "small text-muted"))
      })
      output$retratos_plot <- renderUI({
        pagina <- as.integer(input$pagina_retratos %||% 1)
        quantidade <- min(12L, max(1L, ncol(seletor$dados()) - (pagina - 1L) * 12L))
        plotOutput(session$ns("retratos"), height = paste0(250 * ceiling(quantidade / 3), "px"))
      })
      output$retratos <- renderPlot({ print(exploracao_retratos(seletor$dados(), leituras(), as.integer(input$pagina_retratos %||% 1))) })
      output$mapa_ausentes <- renderPlot({ print(exploracao_mapa_ausentes(seletor$dados())) })
    }
    observeEvent(contexto(), {
      if (identical(contexto(), contexto_anterior())) return()
      contexto_anterior(contexto())
      historico(list())
      resultados(list())
      selecionada("")
      updateSelectInput(session, "previa_id", choices = character())
    }, priority = 100)

    observe({
      dados <- seletor$dados()
      req(is.data.frame(dados))
      tipos <- exploracao_tipos(dados, exploracao_ficha_ler(ficha_rv, dados))
      numericas <- names(tipos)[grepl("Numérica", tipos)]
      categoricas <- names(tipos)[grepl("Categórica", tipos)]
      todas <- names(dados)
      for (modo in unname(catalogo)) {
        # Retratos novos aceitam qualquer coluna; os modos históricos preservam seus filtros.
        escolhas <- if (modo %in% c("retrato", "relacao")) todas else if (modo == "frequencias") categoricas else numericas
        atualizar <- function(campo, opcoes, padrao = 1L) {
          chave <- paste0(modo, "_", campo)
          atual <- isolate(input[[chave]])
          if (is.null(atual) || !atual %in% opcoes) atual <- if (length(opcoes)) opcoes[min(padrao, length(opcoes))] else ""
          updateSelectInput(session, chave, choices = opcoes, selected = atual)
        }
        if (area != "explorar") atualizar("variavel", escolhas)
        if (modo %in% c("correlacao", "dispersao", "marginais")) atualizar("outra", numericas, 2L)
        if (modo == "relacao") atualizar("outra", todas, 2L)
        if (modo %in% c("grupos", "normalidade")) atualizar("grupo", c("Todos juntos" = "nenhum", stats::setNames(categoricas, categoricas)))
      }
    })

    parametros <- function(modo) {
      campos <- c(if (area != "explorar") "variavel",
                  if (modo %in% c("correlacao", "dispersao", "marginais", "relacao")) "outra",
                  if (modo %in% c("retrato", "relacao")) "tipo",
                  if (modo == "relacao") "tipo_outra",
                  switch(modo, grupos = "grupo", normalidade = "grupo", correlacao = "metodo", histograma = "classes",
                         boxplot = "forma", dispersao = "tendencia", outliers = "limite_z", NULL))
      p <- c(list(analise = modo, leituras = leituras()), stats::setNames(lapply(campos, function(campo) input[[paste0(modo, "_", campo)]]), campos))
      if (modo %in% c("retrato", "relacao") && identical(p$tipo, "automatico")) p$tipo <- unname(leituras()[p$variavel])
      if (modo == "relacao" && identical(p$tipo_outra, "automatico")) p$tipo_outra <- unname(leituras()[p$outra])
      p
    }
    for (modo_atual in unname(catalogo)) local({
      modo <- modo_atual
      chave <- function(campo) paste0(modo, "_", campo)
      rotulo <- names(catalogo)[match(modo, catalogo)]
      observeEvent(if (modo == "panorama") list(contexto(), leituras(), input[[chave("executar")]]) else input[[chave("executar")]], {
        req(is.data.frame(seletor$dados()), ncol(seletor$dados()) > 0)
        p <- parametros(modo)
        dados <- seletor$dados()
        resposta <- tryCatch(catalyser_descricao(dados, p), error = identity)
        if (inherits(resposta, "error")) {
          # Uma tentativa inválida não deixa uma prévia antiga parecer atual.
          atuais <- resultados(); atuais[[modo]] <- NULL; resultados(atuais)
          showNotification(conditionMessage(resposta), type = "warning", duration = 9)
          return()
        }
        titulo <- paste(c(rotulo, p$variavel, p$outra, p$grupo, p$metodo, p$forma,
                          p$tendencia, if (!is.null(p$classes)) paste(p$classes, "classes"),
                          if (!is.null(p$limite_z)) paste("|z| >", p$limite_z)), collapse = " · ")
        estado <- list(analise_id = paste0("descricao_", area), tipo = "descricao_exploratoria",
                       titulo = titulo, parametros = p,
                       # Detalhes e sugestão orientam a tela; o relatório recebe apenas componentes editoriais.
                       saidas_disponiveis = intersect(names(resposta), c("narrativa", "tabela", "grafico", "console")),
                       codigo_r = paste(catalyser_codigo_descricao(p), collapse = "\n"))
        anteriores <- historico()
        iguais <- which(vapply(anteriores, function(z) identical(z$parametros, p), logical(1)))
        identificador <- if (length(iguais)) names(anteriores)[iguais[1]] else sprintf("previa_%04d", length(anteriores) + 1L)
        anteriores[[identificador]] <- estado
        historico(anteriores)
        selecionada(identificador)
        atuais <- resultados(); atuais[[modo]] <- list(parametros = p, resultado = resposta)
        resultados(atuais)
        updateSelectInput(session, "previa_id", choices = stats::setNames(names(anteriores),
          paste0(vapply(anteriores, `[[`, character(1), "titulo"), " [", seq_along(anteriores), "]")), selected = identificador)
        showNotification("Resultado disponível na aba Inserir análise.", type = "message", duration = 4)
      })
      pronto <- reactive({
        contexto()
        atual <- resultados()[[modo]]
        !is.null(atual) && identical(atual$parametros, parametros(modo))
      })
      resposta <- reactive({ req(pronto()); resultados()[[modo]]$resultado })
      output[[chave("pronto")]] <- reactive(pronto())
      outputOptions(output, chave("pronto"), suspendWhenHidden = FALSE)
      output[[chave("status")]] <- renderUI({
        if (pronto()) return(NULL)
        div(class = "alert alert-light border", "Escolha as variáveis e execute a análise. Resultados de escolhas anteriores continuam na aba Inserir análise enquanto a base não mudar.")
      })
      output[[chave("narrativa")]] <- renderUI(p(resposta()$narrativa))
      output[[chave("tabela")]] <- DT::renderDT({
        tabela <- resposta()$tabela
        req(!is.null(tabela))
        widget <- DT::datatable(tabela, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE,
          # O cabeçalho navy conserva a identidade Ocean também nas tabelas interativas.
          headerCallback = DT::JS("function(thead) { $(thead).css({'background-color':'#0F3B5F','color':'white'}); }"),
          language = list(decimal = ",", thousands = ".", search = "Buscar:",
                          lengthMenu = "Mostrar _MENU_ linhas", info = "Mostrando _START_ a _END_ de _TOTAL_ linhas",
                          infoEmpty = "Nenhuma linha", zeroRecords = "Nenhum resultado encontrado",
                          paginate = list(previous = "Anterior", "next" = "Próxima"))))
        decimais <- names(tabela)[vapply(tabela, is.double, logical(1))]
        if (length(decimais)) widget <- DT::formatSignif(widget, decimais, digits = 4, dec.mark = ",")
        widget
      })
      output[[chave("detalhes")]] <- DT::renderDT({
        detalhes <- resposta()$detalhes
        req(!is.null(detalhes))
        # A expansão usa a mesma linguagem e o mesmo acabamento Ocean do retrato principal.
        widget <- DT::datatable(detalhes, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE,
          headerCallback = DT::JS("function(thead) { $(thead).css({'background-color':'#0F3B5F','color':'white'}); }"),
          language = list(decimal = ",", thousands = ".", search = "Buscar:",
                          lengthMenu = "Mostrar _MENU_ linhas", info = "Mostrando _START_ a _END_ de _TOTAL_ linhas",
                          infoEmpty = "Nenhuma linha", zeroRecords = "Nenhum resultado encontrado",
                          paginate = list(previous = "Anterior", "next" = "Próxima"))))
        decimais <- names(detalhes)[vapply(detalhes, is.double, logical(1))]
        if (length(decimais)) widget <- DT::formatSignif(widget, decimais, digits = 4, dec.mark = ",")
        widget
      })
      output[[chave("painel_detalhes")]] <- renderUI({
        # Detalhes ficam recolhidos para não afogar o primeiro olhar em medidas.
        if (is.null(resposta()$detalhes)) return(NULL)
        tags$details(tags$summary("Mais medidas"), DT::DTOutput(session$ns(chave("detalhes"))))
      })
      output[[chave("caminho")]] <- renderUI({
        caminho <- resposta()$sugestao
        req(!is.null(caminho))
        destino <- caminho$destino
        abrir <- if (nzchar(destino)) tags$a(
          paste("Abrir", destino), href = "#", class = "btn btn-sm btn-outline-primary ms-2",
          onclick = sprintf("var itens = Array.from(document.querySelectorAll('#main_navbar a')); var alvo = itens.find(function(a) { return a.textContent.trim() === '%s'; }); if (alvo) alvo.click(); return false;", destino)
        ) else NULL
        div(class = "alert border", style = "background:#E7EFEA; border-color:#62B6B7 !important; color:#0F3B5F;",
            strong("Isto costuma pedir: ", caminho$analise), tags$br(), caminho$retrato, abrir)
      })
      output[[chave("painel_grafico")]] <- renderUI({
        # Os gráficos ficam em uma janela menor e centralizada para facilitar a leitura do conjunto.
        if (!is.null(resposta()$grafico)) {
          div(
            class = "descricao-grafico-compacto",
            plotOutput(session$ns(chave("grafico")), height = if (modo == "transformacoes") "640px" else if (modo == "normalidade") "460px" else "360px")
          )
        }
      })
      output[[chave("grafico")]] <- renderPlot({ req(resposta()$grafico); print(resposta()$grafico) })
      output[[chave("codigo")]] <- renderText({ req(pronto()); paste(catalyser_codigo_descricao(parametros(modo)), collapse = "\n") })
      output[[chave("console")]] <- renderText(paste(resposta()$console, collapse = "\n"))
    })
    observeEvent(input$previa_id, {
      if (input$previa_id %in% names(historico())) selecionada(input$previa_id)
    })
    estado_execucao <- reactive({ contexto(); historico()[[selecionada()]] })
    output$resumo_selecao <- renderUI({
      estado <- estado_execucao()
      if (is.null(estado)) return(p("Nenhuma análise executada nesta base. Volte a uma sub-aba e clique em Executar análise."))
      div(class = "alert alert-info", strong(estado$titulo),
          " — esta é a configuração que será inserida, mesmo se você tiver mudado a prévia de outra aba.")
    })
    registro <- mod_registrar_execucao_server(
      "registrar", estado_execucao, seletor$contexto, registro_execucoes_rv,
      contador_execucoes_rv, revisao_origem_rv, registro_bases_rv, cache_bases_rv,
      paste0("descricao_", area), "A análise", nova_configuracao_rv = selecionada
    )
    invisible(list(historico = historico, estado_execucao = estado_execucao,
                   resultados = resultados, registro = registro, seletor = seletor))
  })
}
