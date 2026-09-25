# Planejamento de variáveis: ponte entre a coleta, a planilha tidy e a análise.
library(shiny)
library(bslib)
library(DT)

# Converte um rótulo livre em um nome seguro para uma coluna no R e no Excel.
gerar_nome_reduzido <- function(nome_extenso) {
  # Padroniza letras maiúsculas e minúsculas.
  x <- tolower(nome_extenso)
  # Troca caracteres acentuados por letras simples.
  x <- iconv(x, to = "ASCII//TRANSLIT")
  # Mantém letras e números, separando as demais sequências por underscore.
  x <- gsub("[^a-z0-9]+", "_", x)
  # Retira separadores que tenham ficado nas pontas.
  x <- gsub("^_+|_+$", "", x)
  # Evita um cabeçalho vazio quando o nome ainda não foi informado.
  if (!nzchar(x)) x <- "variavel"
  x
}

# Relaciona cada tipo de dado ao próximo menu que costuma ajudar na análise.
guia_analise_variavel <- function(tipo) {
  dplyr::case_when(
    tipo == "Quantitativa contínua" ~ "Testes paramétricos e regressão",
    tipo == "Quantitativa discreta, de contagem" ~ "Regressão de Poisson e Binomial Negativa",
    tipo %in% c("Qualitativa nominal", "Binária, de dois níveis") ~ "Frequências, proporções e qui-quadrado",
    tipo == "Qualitativa ordinal" ~ "Testes não paramétricos",
    tipo == "Data ou tempo" ~ "Menu Séries Temporais",
    TRUE ~ "Identificador: rótulo da unidade, não entra na análise"
  )
}

# Combina a família de análise sugerida com a estrutura de erro do delineamento.
guia_analise_planejada <- function(tipo, delineamento_rv = NULL) {
  guia_base <- guia_analise_variavel(tipo)
  if (is.null(delineamento_rv)) return(guia_base)
  res <- tryCatch(delineamento_rv(), error = function(e) NULL)
  tipo_delineamento <- res$design_type %||% ""
  estrutura <- dplyr::case_when(
    tipo_delineamento == "DBC" ~ "respeitando os blocos",
    tipo_delineamento == "split_plot" ~ "com a estrutura de erro de parcelas subdivididas",
    tipo_delineamento == "fatorial" ~ "com efeitos principais e interação entre os fatores",
    TRUE ~ "respeitando o delineamento declarado"
  )
  dplyr::case_when(
    tipo == "Quantitativa contínua" ~ paste("ANOVA ou regressão", estrutura),
    tipo == "Quantitativa discreta, de contagem" ~ paste("Poisson ou Binomial Negativa", estrutura),
    TRUE ~ guia_base
  )
}

# modo: "experimental" (croqui), "observacional" (delineamentos observacionais)
# ou "sorteio" (passo curto depois de Como sortear a amostra, sem os controles de unidade).
mod_planejamento_variaveis_ui <- function(id, experimental = FALSE,
                                          modo = if (experimental) "experimental" else "observacional") {
  ns <- NS(id)
  raiz <- paste0("#", ns("raiz"))
  controle_planejamento <- if (identical(modo, "experimental")) {
    tagList(
      radioButtons(ns("relacao_ue"), "A linha da planilha representa:",
        choices = c("A própria unidade experimental" = "ue", "Uma subunidade dentro da UE" = "subunidade"),
        selected = "ue", inline = TRUE),
      conditionalPanel(
        condition = sprintf("input['%s'] === 'subunidade'", ns("relacao_ue")),
        radioButtons(ns("caminho_subunidade"), "Como registrar a subunidade?",
          choices = c("Usar a média por UE, caminho simples e correto na v1" = "agregar",
                      "Manter id da UE e id da subunidade, estrutura hierárquica para a v2" = "hierarquico"),
          selected = "agregar"),
        conditionalPanel(
          condition = sprintf("input['%s'] === 'hierarquico'", ns("caminho_subunidade")),
          numericInput(ns("n_subunidades"), "Subunidades registradas por UE:", value = 1, min = 1, max = 1000)
        )
      )
    )
  } else {
    # Nos delineamentos observacionais, as linhas vêm do mesmo sorteio da tela Como sortear a amostra.
    helpText("As linhas desta ficha vêm do último sorteio feito em Como sortear a amostra, com as colunas do marco amostral.")
  }
  # Cartão da unidade amostral: no modo sorteio, a unidade já é a linha do marco.
  cartao_unidade <- card(
    card_header("Unidade amostral"),
    card_body(
      radioButtons(ns("unidade"), "Cada linha representa:",
        choices = c("Tanque" = "tanque", "Peixe" = "peixe", "Indivíduo" = "individuo",
                    "Local de coleta" = "local", "Baia" = "baia", "Animal" = "animal",
                    "Grupo de n animais" = "grupo", "Outro" = "outro"), inline = TRUE),
      conditionalPanel(sprintf("input['%s'] == 'outro'", ns("unidade")),
        textInput(ns("unidade_livre"), "Escreva a unidade:")),
      controle_planejamento
    )
  )
  cartao_coerencia <- card(
    card_header(if (identical(modo, "sorteio")) "Unidades sorteadas" else "Coerência da ficha"),
    card_body(
      uiOutput(ns("resumo_planejamento")),
      uiOutput(ns("alerta_pseudorreplicacao")),
      actionButton(
        ns("enviar_ficha_analises"), "Usar esta ficha nas análises",
        icon = icon("arrow-right"), class = "btn-outline-primary w-100"
      )
    )
  )
  tagList(
    tags$style(HTML(paste0(
      raiz, " { min-width: 0; }\n",
      raiz, " .planejamento-variaveis-tabela { display: table !important; table-layout: fixed; width: 100% !important; }\n",
      raiz, " .planejamento-variaveis-tabela > thead { display: table-header-group !important; }\n",
      raiz, " .planejamento-variaveis-tabela > tbody { display: table-row-group !important; }\n",
      raiz, " .planejamento-variaveis-tabela > thead > tr, ", raiz, " .planejamento-variaveis-tabela > tbody > tr { display: table-row !important; }\n",
      raiz, " .planejamento-variaveis-tabela th, ", raiz, " .planejamento-variaveis-tabela td { display: table-cell !important; vertical-align: middle; }\n",
      raiz, " .planejamento-variaveis-tabela thead th { background-color: #0F3B5F !important; color: #FFFFFF !important; padding: 10px 12px !important; }\n",
      raiz, " .planejamento-variaveis-tabela tbody td { padding: 7px 12px !important; }\n",
      raiz, " .planejamento-variaveis-tabela .shiny-input-container { margin-bottom: 0; }\n",
      raiz, " .planejamento-variaveis-quantidade .shiny-input-container { margin: 0 !important; }\n",
      # Listas abertas no body: todas as opções visíveis, acima dos cartões.
      ".planejamento-lista-completa { z-index: 2000 !important; }\n",
      ".planejamento-lista-completa .selectize-dropdown-content { max-height: none !important; }\n",
      ".planejamento-lista-completa .option { white-space: normal; padding: 8px 12px; }"
    ))),
    tags$div(id = ns("raiz"),
    if (identical(modo, "sorteio")) {
      cartao_coerencia
    } else {
      layout_columns(col_widths = c(6, 6), cartao_unidade, cartao_coerencia)
    },
    if (!identical(modo, "sorteio")) card(
      card_header("Medições repetidas"),
      card_body(
        radioButtons(ns("medidas_repetidas"), "Haverá medições repetidas na mesma unidade ao longo do tempo?",
          choices = c("Não, uma observação por unidade" = "nao", "Sim, em várias ocasiões" = "sim"),
          selected = "nao", inline = TRUE),
        conditionalPanel(sprintf("input['%s'] === 'sim'", ns("medidas_repetidas")),
          textInput(ns("ocasioes"), "Ocasiões ou datas previstas, separadas por vírgula:", placeholder = "Ex.: 0, 15, 30 dias"),
          div(class = "alert alert-info mb-0", "A ficha ficará no formato longo: cada unidade aparecerá uma vez por ocasião. O módulo misto atual cobre subamostras em um momento; a extensão longitudinal, com dependência no tempo, será construída depois."))
      )
    ),
    card(
      card_header(
        div(class = "d-flex justify-content-between align-items-center",
          span(if (identical(modo, "sorteio")) "O que será medido em cada unidade" else "Declarar variáveis"),
          div(class = "planejamento-variaveis-quantidade d-flex align-items-center gap-3",
            checkboxInput(ns("mostrar_detalhes"), "Faixa e descrição", FALSE),
            tags$label(`for` = ns("n_variaveis"), "Número de variáveis:", class = "mb-0 text-nowrap"),
            numericInput(ns("n_variaveis"), NULL, value = 2, min = 1, max = 20, width = "90px")
          )
        )
      ),
      card_body(style = "padding: 10px; overflow-x: auto;", uiOutput(ns("form_variaveis")))
    ),
    navset_card_tab(
      nav_panel("Planilha tidy", icon = icon("table"), card_body(DTOutput(ns("planilha")))),
      nav_panel("Dicionário e guia de análise", icon = icon("book-open"), card_body(DTOutput(ns("dicionario"))))
    ),
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      downloadButton(ns("baixar_relatorio"), "Relatório Word (.docx)", class = "btn-success w-100"),
      downloadButton(ns("baixar_planilha"), "Planilha de coleta (.xlsx)", class = "btn-primary w-100"),
      downloadButton(ns("baixar_projeto"), "Projeto R (.zip)", class = "btn-success w-100"),
      downloadButton(ns("baixar_dicionario"), "Dicionário (.csv)", class = "btn-outline-primary w-100")
    ),
    helpText(if (identical(modo, "sorteio")) {
      "A planilha de coleta tem duas famílias de coluna: as do marco, já preenchidas pelo sorteio, e as de resposta, em branco para o campo. O dicionário vai numa segunda aba e o projeto R inclui o código que refaz o sorteio."
    } else if (identical(modo, "experimental")) {
      "O projeto reúne a planilha tidy, o dicionário de dados e um roteiro de análise. Tratamento, bloco e repetição entram depois que o croqui é gerado."
    } else {
      "O projeto reúne a planilha tidy, o dicionário de dados e a ficha de planejamento em dados/. Eixos e hierarquia entram quando a ficha do delineamento foi enviada; n e sorteio, quando foram calculados e feitos."
    }, style = "font-size: .84rem;")
    )
  )
}

# estrutura_rv: reativo com o sorteio de Como sortear a amostra (lista com
# sorteados e registro). Quando existe, as linhas da ficha são as unidades sorteadas.
mod_planejamento_variaveis_server <- function(id, delineamento_rv = NULL,
                                               estrutura_rv = NULL,
                                               ficha_destino_rv = NULL,
                                               modo = if (is.null(delineamento_rv)) "observacional" else "experimental") {
  moduleServer(id, function(input, output, session) {
    tipos <- c("Qualitativa nominal", "Qualitativa ordinal", "Quantitativa discreta, de contagem",
      "Quantitativa contínua", "Binária, de dois níveis", "Data ou tempo", "Identificador")

    # Valor já digitado num campo, para não apagar o que o pesquisador escreveu ao redesenhar a tabela.
    valor_atual <- function(campo, padrao = "") isolate(input[[campo]]) %||% padrao

    # Monta uma tabela horizontal para que seja fácil comparar as variáveis declaradas.
    output$form_variaveis <- renderUI({
      n <- input$n_variaveis
      req(n)
      # Faixa de validação e descrição são opcionais e ficam escondidas por padrão.
      detalhes <- isTRUE(input$mostrar_detalhes)
      # No modo sorteio, todas as variáveis declaradas são respostas; a primeira costuma ser contínua.
      tipo_padrao <- function(i) if (i == 1 || identical(modo, "sorteio")) "Quantitativa contínua" else "Qualitativa nominal"
      tags$table(class = "table table-hover align-middle mb-0 planejamento-variaveis-tabela",
        style = if (detalhes) "min-width: 1250px;" else "min-width: 950px;",
        tags$thead(style = "background: #0F3B5F; color: white;",
          tags$tr(tags$th("Variável", style = "width: 7%;"), tags$th("Nome por extenso", style = "width: 26%;"),
            tags$th("Nome reduzido", style = "width: 20%;"), tags$th("Tipo de dado", style = "width: 27%;"),
            tags$th("Unidade", style = "width: 12%;"),
            if (detalhes) tags$th("Faixa ou níveis", style = "width: 14%;"),
            if (detalhes) tags$th("Descrição", style = "width: 22%;"))),
        tags$tbody(lapply(seq_len(n), function(i) tags$tr(
          tags$td(strong(i)),
          tags$td(textInput(session$ns(paste0("nome_", i)), NULL, value = valor_atual(paste0("nome_", i)), width = "100%")),
          tags$td(textInput(session$ns(paste0("reduzido_", i)), NULL, value = valor_atual(paste0("reduzido_", i)), width = "100%")),
          # A lista abre fora da tabela (que tem rolagem lateral) e mostra os sete tipos de uma vez.
          tags$td(selectizeInput(session$ns(paste0("tipo_", i)), NULL, choices = tipos,
            selected = valor_atual(paste0("tipo_", i), tipo_padrao(i)), width = "100%",
            options = list(dropdownParent = "body", dropdownClass = "selectize-dropdown planejamento-lista-completa"))),
          tags$td(textInput(session$ns(paste0("unidade_", i)), NULL, value = valor_atual(paste0("unidade_", i)),
            placeholder = "cm, g, mg/L", width = "100%")),
          if (detalhes) tags$td(textInput(session$ns(paste0("faixa_", i)), NULL, value = valor_atual(paste0("faixa_", i)),
            placeholder = "0 a 50", width = "100%")),
          if (detalhes) tags$td(textInput(session$ns(paste0("descricao_", i)), NULL, value = valor_atual(paste0("descricao_", i)), width = "100%"))
        )))
      )
    })

    # Sugere o nome seguro assim que o pesquisador informa o nome por extenso.
    observe({
      n <- input$n_variaveis
      req(n)
      lapply(seq_len(n), function(i) observeEvent(input[[paste0("nome_", i)]], {
        nome <- input[[paste0("nome_", i)]]
        if (nzchar(nome %||% "")) updateTextInput(session, paste0("reduzido_", i), value = gerar_nome_reduzido(nome))
      }, ignoreInit = TRUE))
    })

    especificacao <- reactive({
      n <- input$n_variaveis
      req(n)
      nomes <- vapply(seq_len(n), function(i) input[[paste0("reduzido_", i)]] %||% "", character(1))
      nomes[nomes == ""] <- vapply(seq_len(n)[nomes == ""], function(i) gerar_nome_reduzido(input[[paste0("nome_", i)]] %||% ""), character(1))
      nomes <- make.unique(vapply(nomes, gerar_nome_reduzido, character(1)), sep = "_")
      # Lê um campo de texto de cada linha do formulário.
      campo <- function(prefixo) vapply(seq_len(n), function(i) input[[paste0(prefixo, i)]] %||% "", character(1))
      data.frame(
        nome_extenso = campo("nome_"),
        nome_reduzido = nomes,
        tipo = vapply(seq_len(n), function(i) input[[paste0("tipo_", i)]] %||% tipos[1], character(1)),
        # Depois de um sorteio, o que se declara aqui é o que será medido: tudo é resposta.
        papel = if (identical(modo, "sorteio")) rep("Resposta", n) else ifelse(seq_len(n) == 1, "Resposta", "Fator"),
        unidade = campo("unidade_"),
        faixa_ou_niveis = campo("faixa_"),
        descricao = campo("descricao_"),
        stringsAsFactors = FALSE
      )
    })

    # Usa a estrutura do croqui como ponto de partida, quando este módulo está no ramo experimental.
    contexto_experimental <- reactive({
      if (is.null(delineamento_rv)) return(NULL)
      res <- delineamento_rv()
      if (is.null(res) || isTRUE(res$error)) return(NULL)
      manter <- intersect(c("UE", "Tratamento", "Bloco", "Repeticao", "Linha", "Coluna"), names(res$df))
      res$df[, manter, drop = FALSE]
    })

    # O sorteio de Como sortear a amostra, quando já foi feito.
    sorteio_atual <- reactive({
      if (is.null(estrutura_rv)) return(NULL)
      tryCatch(estrutura_rv(), error = function(e) NULL)
    })

    # As unidades sorteadas entram com todas as colunas do marco (identificador, estrato, ordem...).
    contexto_observacional <- reactive({
      s <- sorteio_atual()
      if (is.null(s) || is.null(s$sorteados)) return(NULL)
      s$sorteados
    })

    # A estrutura vem sempre do planejamento escolhido, nunca de um novo tamanho digitado aqui.
    contexto_planejamento <- reactive({
      experimental <- contexto_experimental()
      if (!is.null(experimental)) return(experimental)
      contexto_observacional()
    })

    output$resumo_planejamento <- renderUI({
      base <- contexto_planejamento()
      if (is.null(base)) {
        return(div(class = "alert alert-warning mb-0", "Primeiro gere o croqui ou execute o sorteio. A quantidade de linhas da ficha virá automaticamente desse planejamento."))
      }
      if (!is.null(contexto_experimental())) {
        return(div(class = "alert alert-success mb-0",
          strong("Delineamento experimental"), ": a ficha terá ", strong(nrow(base)), " unidade(s) definida(s) pelo planejamento."))
      }
      r <- sorteio_atual()$registro
      div(class = "alert alert-success mb-0",
        strong(sprintf("%s, semente %d.", nome_metodo_sorteio(r$metodo), r$semente)),
        sprintf(" A ficha terá %d unidade(s). Colunas herdadas do marco: %s.", nrow(base), paste(names(base), collapse = ", ")),
        if (isTRUE(r$aninhado)) tags$div(class = "mt-1", icon("sitemap"),
          sprintf(" Unidades aninhadas em %s: a ficha registra %s como efeito aleatório para o modelo misto.", r$conglomerado, r$conglomerado)))
    })

    output$alerta_pseudorreplicacao <- renderUI({
      if (is.null(delineamento_rv) || !identical(input$relacao_ue, "subunidade")) return(NULL)
      if (identical(input$caminho_subunidade, "agregar")) {
        div(class = "alert alert-warning mt-3 mb-0", strong("Atenção: pseudorreplicação. "),
          "Vários indivíduos no mesmo tanque não são réplicas independentes do tratamento. Na v1, registre a resposta agregada, como a média por tanque, e mantenha uma linha por UE.")
      } else {
        div(class = "alert alert-info mt-3 mb-0", strong("Estrutura hierárquica reconhecida. "),
          "A ficha manterá a identificação da UE e da subunidade. Para um fator e um nível de agrupamento, use ANOVA com subamostras; a ficha pode preencher essas colunas na análise.")
      }
    })

    planilha <- reactive({
      base <- contexto_planejamento()
      if (is.null(base)) {
        # Sem planejamento, a ficha começa vazia, só com a coluna da unidade escolhida.
        unidade <- if (identical(input$unidade, "outro")) input$unidade_livre else input$unidade
        id <- paste0("id_", gerar_nome_reduzido(unidade %||% "unidade"))
        base <- data.frame(setNames(list(character(0)), id), check.names = FALSE)
      }
      if (!is.null(delineamento_rv) && identical(input$relacao_ue, "subunidade") && identical(input$caminho_subunidade, "hierarquico")) {
        n_sub <- input$n_subunidades %||% 1
        base <- base[rep(seq_len(nrow(base)), each = n_sub), , drop = FALSE]
        base$id_subunidade <- rep(sprintf("sub_%02d", seq_len(n_sub)), times = nrow(base) / n_sub)
      }
      if (identical(input$medidas_repetidas, "sim")) {
        ocasioes <- trimws(strsplit(input$ocasioes %||% "", ",")[[1]])
        ocasioes <- ocasioes[nzchar(ocasioes)]
        if (!length(ocasioes)) ocasioes <- "ocasião_1"
        base <- base[rep(seq_len(nrow(base)), each = length(ocasioes)), , drop = FALSE]
        base$ocasiao <- rep(ocasioes, times = nrow(base) / length(ocasioes))
      }
      for (nm in especificacao()$nome_reduzido) if (!nm %in% names(base)) base[[nm]] <- rep(NA_character_, nrow(base))
      base
    })

    ficha_analise <- reactive({
      base <- planilha()
      hierarquica <- !is.null(delineamento_rv) &&
        identical(input$relacao_ue, "subunidade") &&
        identical(input$caminho_subunidade, "hierarquico")
      sorteio <- if (is.null(contexto_experimental())) sorteio_atual() else NULL
      # Sorteio por conglomerados com mais de uma unidade por grupo: unidade aninhada no conglomerado.
      aninhado_sorteio <- !is.null(sorteio) && isTRUE(sorteio$registro$aninhado)
      if (aninhado_sorteio) hierarquica <- TRUE
      unidade_coluna <- if ("UE" %in% names(base)) "UE" else if (aninhado_sorteio) {
        # O conglomerado é a unidade independente; o identificador é a subamostra dentro dele.
        sorteio$registro$conglomerado
      } else if (!is.null(sorteio)) {
        sorteio$registro$id
      } else if ("id_unidade" %in% names(base)) {
        "id_unidade"
      } else if (length(names(base))) names(base)[1] else "id_unidade"
      especificado <- especificacao()
      experimental <- contexto_experimental()
      subamostra <- if (aninhado_sorteio) sorteio$registro$id else if (hierarquica) "id_subunidade" else ""
      tipo_exp <- if (!is.null(experimental)) delineamento_rv()$design_type else NULL
      # Parte da ficha de planejamento (contrato 1) que esta tela conhece. Origem,
      # tipo e eixos só são declarados aqui no ramo experimental; no observacional
      # eles vêm da aba do delineamento e não podem ser apagados.
      parte <- list(
        resposta_coluna = especificado$nome_reduzido[especificado$papel == "Resposta"][1],
        unidade_coluna = unidade_coluna,
        hierarquia = list(
          colunas_id = c(unidade_coluna, if (nzchar(subamostra)) subamostra,
                         intersect(c("Bloco", "Linha", "Coluna", "Repeticao"), names(base))),
          subamostra_coluna = subamostra,
          # Aninhamento declarado pela amostragem: o menu de análise sugere o modelo misto.
          efeito_aleatorio = if (aninhado_sorteio) sorteio$registro$conglomerado else NULL,
          momentos = if (identical(input$medidas_repetidas, "sim")) unique(base$ocasiao) else NULL
        ),
        # Como as unidades foram escolhidas, quando houve sorteio.
        sorteio = ficha_parte_sorteio(sorteio)$sorteio,
        analise_sugerida = if (hierarquica) "anova_mista_subamostras" else "anova_um_fator"
      )
      if (!is.null(experimental)) {
        parte$origem <- "Planejamento de variáveis do experimento"
        parte$tipo <- tipo_exp
        parte$eixos <- list(grupos = list(coluna = "Tratamento", niveis = length(unique(experimental$Tratamento))))
        # Só o DIC tem passagem validada para uma análise; nos demais a sugestão fica NULL.
        if (!identical(tipo_exp, "dic")) parte["analise_sugerida"] <- list(NULL)
      }
      parte
    })

    # Ficha consolidada: a enviada pelos outros módulos (delineamento, n, sorteio)
    # com a parte desta tela por cima. É ela que vai para a planilha e para o projeto.
    ficha_consolidada <- reactive({
      recebida <- if (is.function(ficha_destino_rv)) ficha_destino_rv() else NULL
      ficha <- ficha_mesclar(recebida, ficha_analise())
      if (is.null(ficha$origem)) {
        ficha$origem <- if (identical(modo, "sorteio")) "Sorteio do marco amostral" else "Planejamento de variáveis observacionais"
      }
      ficha
    })

    observeEvent(input$enviar_ficha_analises, {
      if (is.function(ficha_destino_rv)) ficha_destino_rv(ficha_consolidada())
      f <- ficha_consolidada()
      showNotification(
        if (!is.null(f$hierarquia$efeito_aleatorio)) {
          sprintf("Ficha enviada. O modelo misto receberá %s como efeito aleatório e %s como unidade dentro dele.",
            f$unidade_coluna, ficha_subamostra_coluna(f))
        } else if (ficha_tem_subamostras(f)) {
          "Ficha enviada. A ANOVA com subamostras receberá UE e id_subunidade automaticamente."
        } else {
          "Ficha enviada. Como cada linha representa uma unidade, a ANOVA comum é a sugestão inicial."
        },
        type = "message", duration = 7
      )
    }, ignoreInit = TRUE)

    # Dicionário completo: colunas estruturais herdadas do sorteio + variáveis de resposta declaradas.
    dicionario_completo <- reactive({
      d <- especificacao()
      d$guia_analise <- vapply(d$tipo, guia_analise_planejada, character(1), delineamento_rv = delineamento_rv)
      base <- contexto_observacional()
      if (is.null(base) || !is.null(contexto_experimental())) return(d)
      id <- sorteio_atual()$registro$id
      estruturais <- names(base)
      # O tipo de cada coluna estrutural é deduzido do conteúdo: identificador, número ou categoria.
      tipo_estrutural <- vapply(estruturais, function(col) dplyr::case_when(
        col == id ~ "Identificador",
        is.numeric(base[[col]]) ~ "Quantitativa contínua",
        TRUE ~ "Qualitativa nominal"
      ), character(1))
      e <- data.frame(
        nome_extenso = estruturais, nome_reduzido = estruturais, tipo = tipo_estrutural,
        papel = "Estrutural (marco amostral)", unidade = "", faixa_ou_niveis = "",
        descricao = "Preenchida pelo sorteio", stringsAsFactors = FALSE
      )
      e$guia_analise <- vapply(e$tipo, guia_analise_variavel, character(1))
      rbind(e, d)
    })

    # Abas da planilha de coleta: os dados, o dicionário e, se houve sorteio, o registro dele.
    abas_planilha <- reactive({
      abas <- list(coleta = planilha(), dicionario = dicionario_completo())
      s <- if (is.null(contexto_experimental())) sorteio_atual() else NULL
      if (!is.null(s)) abas$registro_sorteio <- tabela_registro_sorteio(s$registro)
      # A ficha de planejamento viaja com a planilha, como metadados em aba própria.
      abas$ficha_planejamento <- ficha_tabela(ficha_consolidada())
      abas
    })

    output$planilha <- renderDT(datatable(planilha(), rownames = FALSE, options = list(pageLength = 12, scrollX = TRUE, dom = "tip"), class = "stripe hover"))
    output$dicionario <- renderDT({
      datatable(dicionario_completo(), rownames = FALSE, options = list(pageLength = 12, scrollX = TRUE, dom = "tip"), class = "stripe hover")
    })

    output$baixar_planilha <- downloadHandler(
      filename = function() paste0("planilha_coleta_", format(Sys.Date(), "%Y-%m-%d"), ".xlsx"),
      content = function(file) writexl::write_xlsx(abas_planilha(), file)
    )
    output$baixar_relatorio <- downloadHandler(
      filename = function() paste0("relatorio_planejamento_variaveis_", format(Sys.Date(), "%Y-%m-%d"), ".docx"),
      content = function(file) {
        pasta <- tempfile("planejamento_variaveis_")
        dir.create(pasta)
        antigo <- getwd()
        on.exit(setwd(antigo), add = TRUE)
        on.exit(unlink(pasta, recursive = TRUE), add = TRUE)
        utils::write.csv(planilha(), file.path(pasta, "planilha.csv"), row.names = FALSE, fileEncoding = "UTF-8")
        utils::write.csv(dicionario_completo(), file.path(pasta, "dicionario.csv"), row.names = FALSE, fileEncoding = "UTF-8")
        abas <- abas_planilha()
        secao_sorteio <- if (!is.null(abas$registro_sorteio)) {
          utils::write.csv(abas$registro_sorteio, file.path(pasta, "registro_sorteio.csv"), row.names = FALSE, fileEncoding = "UTF-8")
          c("# Sorteio das unidades", "", "As unidades foram sorteadas do marco amostral com semente registrada, o que permite refazer o mesmo sorteio.", "",
            "```{r}", "#| echo: false", "registro <- read.csv('registro_sorteio.csv', check.names = FALSE)", "knitr::kable(registro)", "```", "")
        } else character()
        writeLines(c(
          "---", "title: \"Planejamento de Variáveis\"", "format:", "  docx: default", "---", "",
          "# Unidade amostral", "", "Cada linha da planilha representa uma unidade amostral e, portanto, uma réplica do estudo.", "",
          secao_sorteio,
          "# Dicionário de dados", "", "```{r}", "#| echo: false", "dicionario <- read.csv('dicionario.csv', check.names = FALSE)", "knitr::kable(dicionario)", "```", "",
          "# Modelo da planilha tidy", "", "```{r}", "#| echo: false", "planilha <- read.csv('planilha.csv', check.names = FALSE)", "knitr::kable(utils::head(planilha, 20))", "```", "",
          "# Próximos passos", "", "Use o guia do dicionário para localizar, na CatalyseR, o caminho de análise adequado a cada variável."
        ), file.path(pasta, "relatorio.qmd"), useBytes = TRUE)
        setwd(pasta)
        system2("quarto", c("render", "relatorio.qmd", "--to", "docx"))
        resultado <- file.path(pasta, "relatorio.docx")
        if (!file.exists(resultado)) stop("Não foi possível gerar o Word. Verifique se o Quarto está instalado.", call. = FALSE)
        file.copy(resultado, file, overwrite = TRUE)
      }
    )
    output$baixar_dicionario <- downloadHandler(
      filename = function() paste0("dicionario_variaveis_", format(Sys.Date(), "%Y-%m-%d"), ".csv"),
      content = function(file) utils::write.csv(dicionario_completo(), file, row.names = FALSE, fileEncoding = "UTF-8")
    )
    output$baixar_projeto <- downloadHandler(
      filename = function() paste0("planejamento_variaveis_", format(Sys.Date(), "%Y-%m-%d"), ".zip"),
      content = function(file) {
        pasta <- file.path(tempdir(), paste0("planejamento_variaveis_", as.integer(Sys.time())))
        dir.create(pasta, recursive = TRUE)
        dir.create(file.path(pasta, "dados"))
        utils::write.csv(planilha(), file.path(pasta, "dados", "planilha_coleta.csv"), row.names = FALSE, fileEncoding = "UTF-8")
        writexl::write_xlsx(abas_planilha(), file.path(pasta, "dados", "planilha_coleta.xlsx"))
        utils::write.csv(dicionario_completo(), file.path(pasta, "dados", "dicionario_variaveis.csv"), row.names = FALSE, fileEncoding = "UTF-8")
        leia_me <- c("# Planejamento de variáveis", "", "Abra `dados/planilha_coleta.xlsx` para registrar a coleta.", "", "A primeira coluna identifica a unidade amostral. Cada linha é uma réplica definida pelo delineamento.", "Consulte `dados/dicionario_variaveis.csv` antes de importar a planilha na CatalyseR.")
        # Com sorteio, o projeto leva o marco, a função e o script que refazem o mesmo sorteio.
        s <- if (is.null(contexto_experimental())) sorteio_atual() else NULL
        if (!is.null(s)) {
          dir.create(file.path(pasta, "R"))
          writexl::write_xlsx(list(marco_amostral = s$marco %||% s$sorteados,
                                   registro_sorteio = tabela_registro_sorteio(s$registro)),
                              file.path(pasta, "dados", "marco_amostral.xlsx"))
          file.copy("templates/funcoes_sorteio.R", file.path(pasta, "R", "funcoes_sorteio.R"), overwrite = TRUE)
          writeLines(codigo_sorteio(s$registro), file.path(pasta, "sorteio.R"), useBytes = TRUE)
          leia_me <- c(leia_me, "", "## Sorteio", "", "`sorteio.R` refaz o sorteio a partir de `dados/marco_amostral.xlsx`, com a mesma semente, usando `R/funcoes_sorteio.R`.")
        }
        # A ficha completa vai como objeto R em dados/, para o projeto relê-la.
        leia_me <- c(leia_me, ficha_salvar_projeto(ficha_consolidada(), pasta))
        writeLines(leia_me, file.path(pasta, "README.md"), useBytes = TRUE)
        antigo <- getwd(); on.exit(setwd(antigo), add = TRUE); setwd(dirname(pasta)); zip::zip(file, files = basename(pasta))
      }
    )
    # Expõe a ficha para usos futuros sem duplicar as regras de coerência.
    list(planilha = planilha, especificacao = especificacao, ficha = ficha_consolidada)
  })
}
