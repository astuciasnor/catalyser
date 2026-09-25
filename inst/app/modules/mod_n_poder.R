# Planejamento amostral: quantas unidades coletar, em cinco métodos e duas
# famílias. Para estimar: uma média e uma proporção (margem de erro). Para
# comparar: médias (teste t/ANOVA, stats), proporções (pwr::pwr.2p.test) e
# relação entre variáveis (pwr::pwr.r.test). Os cálculos são premissas para
# planejar a coleta, não resultados dos dados; todas as abas oferecem a
# correção opcional de população finita.
# As duas abas de estimação têm, na coluna da direita, uma sub-aba
# "Visualização gráfica" com a curva da distribuição do estimador e a faixa
# do intervalo de confiança; a "média esperada" digitada só centraliza esse
# desenho, sem entrar no cálculo do n nem na ficha de premissas. As três abas
# de comparação têm a mesma sub-aba com as distribuições planejadas (curvas t
# dos grupos; proporções amostrais dos grupos; correlação sob H0 e sob a
# esperada), e os cinco cartões de premissas usam a mesma grade interna de
# duas colunas.

planejamento_contexto_ui <- function(titulo, texto) {
  shiny::div(
    class = "alert alert-light border mb-3",
    style = "border-left:4px solid #2E7D8F !important; line-height:1.45;",
    shiny::tags$b(titulo), shiny::tags$br(), texto
  )
}

# A porta recolhe os cinco métodos em dois grupos de sub-abas ("Para estimar"
# e "Para comparar"), sem item novo de menu. Novos métodos entram como mais
# uma sub-aba quando tiverem a ficha de premissas e a unidade independente
# explicitadas.
mod_quantos_coletar_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    # Cabeçalho "Planejamento atual", lido da ficha.
    shiny::uiOutput(ns("planejamento_atual")),
    bslib::navset_card_tab(
      id = ns("metodo"),
      bslib::nav_menu("Para estimar", icon = shiny::icon("bullseye"),
        bslib::nav_panel("Uma média",
          bslib::card_body(fillable = FALSE, mod_n_media_ui(ns("media")))),
        bslib::nav_panel("Uma proporção",
          bslib::card_body(fillable = FALSE, mod_n_proporcao_ui(ns("proporcao"))))
      ),
      bslib::nav_menu("Para comparar", icon = shiny::icon("chart-line"),
        bslib::nav_panel("Comparação de médias",
          bslib::card_body(fillable = FALSE, mod_n_poder_ui(ns("poder")))),
        bslib::nav_panel("Comparação de proporções",
          bslib::card_body(fillable = FALSE, mod_n_duas_prop_ui(ns("duas_prop")))),
        bslib::nav_panel("Relação entre variáveis",
          bslib::card_body(fillable = FALSE, mod_n_correlacao_ui(ns("correlacao"))))
      )
    )
  )
}

mod_quantos_coletar_server <- function(id, ficha_rv = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    output$planejamento_atual <- shiny::renderUI({
      if (is.function(ficha_rv)) ficha_cabecalho_ui(ficha_rv())
    })
  })
}

# ---- Comparar médias (n por poder) -------------------------------------------
# Aba original do módulo: a lógica de cálculo e a escrita na ficha não mudam.
# A saída tem duas colunas (premissas em grade compacta, sem rolagem) e o
# resultado em duas abas, uma delas com as distribuições t dos grupos.

mod_n_poder_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    # A lista suspensa cresce até caber a comparação mais longa numa linha só.
    shiny::tags$style(shiny::HTML(paste0(
      "#", ns("premissas"), " .selectize-dropdown { width: max-content !important; min-width: 100% !important; } ",
      "#", ns("premissas"), " .selectize-dropdown .option { white-space: nowrap; }"
    ))),
    planejamento_contexto_ui(
      "Quantos indivíduos para comparar médias?",
      "Use esta opção antes da coleta quando você espera uma diferença entre grupos e quer uma chance definida de detectá-la. O n é por grupo e conta unidades experimentais independentes, não subamostras de um mesmo tanque."
    ),
    bslib::layout_columns(
      col_widths = c(5, 7),
      shiny::div(id = ns("premissas"),
        bslib::card(
          bslib::card_header("Premissas da coleta"),
          bslib::card_body(
            shiny::uiOutput(ns("nota_niveis")),
            shiny::selectInput(ns("metodo"), "Comparação planejada:",
              choices = c("Dois grupos: teste t" = "t", "Três ou mais grupos: ANOVA de um fator" = "anova")),
            # Grade de duas colunas dentro do cartão: à esquerda os campos do
            # método e o desvio; à direita poder, significância e o botão.
            bslib::layout_columns(
              col_widths = c(6, 6),
              shiny::div(
                shiny::conditionalPanel(sprintf("input['%s'] === 't'", ns("metodo")),
                  shiny::numericInput(ns("media_a"), "Média esperada no grupo A:", value = 10, step = 0.5),
                  shiny::numericInput(ns("media_b"), "Média esperada no grupo B:", value = 12, step = 0.5)
                ),
                shiny::conditionalPanel(sprintf("input['%s'] === 'anova'", ns("metodo")),
                  shiny::numericInput(ns("grupos"), "Número de grupos:", value = 3, min = 3, max = 10, step = 1),
                  shiny::uiOutput(ns("medias_ui"))
                ),
                shiny::numericInput(ns("desvio"), "Desvio-padrão esperado dentro dos grupos:", value = 3, min = 0.01, step = 0.1)
              ),
              # O botão desce até a altura do campo do desvio, fechando a grade.
              shiny::div(style = "display:flex; flex-direction:column; height:100%;",
                shiny::sliderInput(ns("poder"), "Poder desejado:", min = 0.5, max = 0.95, value = 0.8, step = 0.05),
                shiny::selectInput(ns("alfa"), "Nível de significância:", choices = c("5%" = "0.05", "1%" = "0.01", "10%" = "0.10"), selected = "0.05"),
                shiny::div(style = "margin-top:auto; padding-top:10px;",
                  shiny::actionButton(ns("calcular"), "Calcular n", class = "btn-primary w-100"))
              )
            ),
            shiny::p(class = "small text-muted mt-2 mb-0",
              "Informe o desvio-padrão a partir de estudo piloto, literatura comparável ou uma premissa justificada. O cálculo não escolhe essa premissa por você.")
          )
        )
      ),
      bslib::navset_card_tab(
        id = ns("saida"),
        bslib::nav_panel("Tamanho amostral planejado",
          bslib::card_body(shiny::uiOutput(ns("resultado")))),
        bslib::nav_panel("Visualização gráfica",
          bslib::card_body(shiny::uiOutput(ns("grafico_ui"))))
      )
    )
  )
}

calcular_n_poder <- function(metodo, medias, desvio, poder, alfa) {
  if (!metodo %in% c("t", "anova") || length(medias) < 2L ||
      any(!is.finite(medias)) || !is.finite(desvio) || desvio <= 0 ||
      !is.finite(poder) || poder <= 0 || poder >= 1 ||
      !is.finite(alfa) || alfa <= 0 || alfa >= 1) {
    stop("Revise as médias, o desvio-padrão, o poder e o nível de significância.", call. = FALSE)
  }
  if (metodo == "t" && length(medias) != 2L) {
    stop("O teste t desta opção compara exatamente dois grupos.", call. = FALSE)
  }
  if (metodo == "anova" && length(medias) < 3L) {
    stop("Informe pelo menos três grupos para a ANOVA.", call. = FALSE)
  }
  if (diff(range(medias)) <= 0) {
    stop("As médias previstas são iguais. Defina a menor diferença relevante que deseja detectar.", call. = FALSE)
  }

  calculo <- tryCatch({
    if (metodo == "t") {
      stats::power.t.test(
        delta = abs(diff(medias)), sd = desvio, power = poder,
        sig.level = alfa, type = "two.sample", alternative = "two.sided", strict = TRUE
      )
    } else {
      stats::power.anova.test(
        groups = length(medias), between.var = stats::var(medias),
        within.var = desvio^2, power = poder, sig.level = alfa
      )
    }
  }, error = function(e) stop(
    "Não foi possível encontrar um n com estas premissas; confira a diferença esperada e o desvio-padrão.",
    call. = FALSE
  ))
  n <- max(2L, ceiling(calculo$n))
  if (!is.finite(n) || n > 100000L) {
    stop("O n calculado excede 100 mil por grupo. Revise as premissas e a viabilidade da coleta.", call. = FALSE)
  }
  list(n_por_grupo = n, n_total = n * length(medias), grupos = length(medias),
       n_bruto = unname(calculo$n), metodo = metodo, medias = medias,
       desvio = desvio, poder = poder, alfa = alfa)
}

# Pontos das curvas t dos grupos planejados: cada curva é centrada na média
# esperada do grupo, com escala do desvio-padrão comum e graus de liberdade do
# n calculado (N - k). Serve à aba "Visualização gráfica" do Comparar médias.
dados_curvas_n_poder <- function(res) {
  gl <- max(2, res$n_total - res$grupos)
  rotulos <- if (identical(res$metodo, "t")) {
    c("Grupo A", "Grupo B")
  } else {
    paste("Grupo", seq_len(res$grupos))
  }
  curvas <- do.call(rbind, lapply(seq_len(res$grupos), function(i) {
    # A grade cobre quatro desvios-padrão para cada lado da média.
    x <- seq(res$medias[i] - 4 * res$desvio, res$medias[i] + 4 * res$desvio, length.out = 200)
    data.frame(
      grupo = sprintf("%s (média %g)", rotulos[i], res$medias[i]),
      x = x,
      y = stats::dt((x - res$medias[i]) / res$desvio, df = gl) / res$desvio,
      media = res$medias[i]
    )
  }))
  # A ordem dos grupos na legenda segue a ordem digitada, não a alfabética.
  curvas$grupo <- factor(curvas$grupo, levels = unique(curvas$grupo))
  curvas
}

# Paleta Ocean para as curvas dos grupos (azul-marinho ao coral).
curvas_cores_n_poder <- function(k) {
  grDevices::colorRampPalette(c("#0F3B5F", "#2E7D8F", "#62B6B7", "#E89B3C", "#E76F51"))(k)
}

# ficha_destino_rv: ficha de planejamento compartilhada; cada n calculado
# preenche o campo n_planejado (valor, unidade, método e premissas).
mod_n_poder_server <- function(id, ficha_destino_rv = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    output$medias_ui <- shiny::renderUI({
      shiny::req(input$grupos)
      shiny::tagList(lapply(seq_len(input$grupos), function(i) {
        valor <- shiny::isolate(input[[paste0("media_", i)]])
        if (is.null(valor)) valor <- 10 + 2 * (i - 1)
        shiny::numericInput(session$ns(paste0("media_", i)), sprintf("Média esperada no grupo %d:", i), value = valor, step = 0.5)
      }))
    })

    # Os níveis do planejamento atual preenchem o número de grupos. O campo
    # continua editável: digitar outro valor é o plano B.
    niveis_ficha <- shiny::reactive({
      if (is.function(ficha_destino_rv)) ficha_niveis(ficha_destino_rv()) else NULL
    })
    shiny::observeEvent(niveis_ficha(), {
      niveis <- niveis_ficha()
      if (is.null(niveis) || niveis < 2) return()
      if (niveis == 2) {
        shiny::updateSelectInput(session, "metodo", selected = "t")
      } else {
        shiny::updateSelectInput(session, "metodo", selected = "anova")
        shiny::updateNumericInput(session, "grupos", value = min(niveis, 10))
      }
    })
    output$nota_niveis <- shiny::renderUI({
      niveis <- niveis_ficha()
      if (is.null(niveis)) return(NULL)
      shiny::p(class = "small text-muted mb-2", shiny::icon("link"),
        sprintf(" %d níveis vieram do planejamento atual; altere se o seu estudo mudou.", niveis))
    })

    resultado <- shiny::eventReactive(input$calcular, {
      medias <- if (identical(input$metodo, "t")) {
        c(input$media_a, input$media_b)
      } else {
        shiny::req(input$grupos)
        vapply(seq_len(input$grupos), function(i) {
          shiny::req(input[[paste0("media_", i)]])
        }, numeric(1))
      }
      tryCatch(calcular_n_poder(input$metodo, medias, input$desvio,
        input$poder, as.numeric(input$alfa)), error = function(e) e)
    })

    shiny::observeEvent(resultado(), {
      res <- resultado()
      if (inherits(res, "error") || !is.function(ficha_destino_rv)) return()
      ficha_destino_rv(ficha_mesclar(ficha_destino_rv(), ficha_parte_n(res)))
    })

    output$resultado <- shiny::renderUI({
      res <- resultado()
      if (inherits(res, "error")) return(shiny::div(class = "alert alert-warning mb-0", conditionMessage(res)))
      metodo <- if (res$metodo == "t") "teste t bilateral, com grupos independentes e desvio-padrão comum" else "ANOVA de um fator, com grupos balanceados e desvio-padrão comum"
      shiny::tagList(
        shiny::h3(sprintf("%d por grupo", res$n_por_grupo), style = "color:#0F3B5F;"),
        shiny::p(sprintf("Total planejado: %d unidades experimentais independentes em %d grupos.", res$n_total, res$grupos)),
        shiny::div(class = "alert alert-info", sprintf("Premissas: %s; poder de %.0f%%; α = %.0f%%; desvio-padrão %.2f.",
          metodo, 100 * res$poder, 100 * res$alfa, res$desvio)),
        shiny::p("O arredondamento foi feito para cima. Este n não incorpora perdas de seguimento, blocos, tanques compartilhados, pool ou variâncias diferentes entre grupos."),
        if (is.function(ficha_destino_rv)) shiny::p(class = "small text-success", shiny::icon("link"),
          " Este n foi registrado na ficha de planejamento e segue para a planilha de coleta e o Projeto R."),
        shiny::p(class = "small text-muted mb-0", "Pelo princípio dos 3Rs, planeje o menor número de animais que ainda responda à pergunta. Justifique a premissa de efeito e a unidade experimental antes da coleta.")
      )
    })

    # Aba "Visualização gráfica": antes do cálculo, só a orientação.
    output$grafico_ui <- shiny::renderUI({
      res <- resultado()
      if (is.null(res) || inherits(res, "error")) {
        return(shiny::p(class = "small text-muted mb-0",
          "Clique em Calcular n para ver as distribuições t planejadas dos grupos, centradas nas médias esperadas e abertas pelo desvio-padrão."))
      }
      shiny::plotOutput(session$ns("grafico_poder"), height = "340px")
    })

    # Duas curvas por grupo, no estilo limpo do fluxograma do menu: área
    # translúcida, linha firme e um traço vertical em cada média esperada.
    output$grafico_poder <- shiny::renderPlot({
      res <- resultado()
      shiny::req(res, !inherits(res, "error"))
      curvas <- dados_curvas_n_poder(res)
      marcas <- curvas[!duplicated(curvas$grupo), ]
      ggplot2::ggplot(curvas, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_area(
          ggplot2::aes(fill = grupo),
          alpha = 0.22, position = "identity", show.legend = FALSE) +
        ggplot2::geom_line(ggplot2::aes(colour = grupo), linewidth = 1) +
        ggplot2::geom_vline(data = marcas, ggplot2::aes(xintercept = media, colour = grupo), linetype = "dashed", linewidth = 0.7, show.legend = FALSE) +
        ggplot2::scale_colour_manual(values = curvas_cores_n_poder(res$grupos), name = NULL) +
        ggplot2::scale_fill_manual(values = curvas_cores_n_poder(res$grupos)) +
        ggplot2::labs(x = "valor da variável", y = "densidade",
                      caption = sprintf("Planejamento: n = %d por grupo; desvio-padrão %.2f; poder %.0f%%; α = %.0f%%.",
                                        res$n_por_grupo, res$desvio, 100 * res$poder, 100 * res$alfa)) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(legend.position = "bottom")
    })
    resultado
  })
}

# ---- Peças comuns às quatro abas novas ---------------------------------------
# Cada calcular_* devolve uma lista com dois blocos de campos: os que a tela
# lê (destaque, destaque_bruto, frase, premissas_texto, N) e os que a ficha
# grava via ficha_parte_n_calculo (n, n_total, unidade, metodo_descricao,
# premissas).

# Rótulo percentual da confiança (0,95 vira "95%").
rotulo_confianca <- function(confianca) sprintf("%.0f%%", 100 * confianca)

# N vale NULL (sem correção) ou um número maior que 1.
validar_populacao <- function(N) {
  if (is.null(N)) return(invisible(NULL))
  if (length(N) != 1L || !is.finite(N) || N < 2) {
    stop("O tamanho da população (N) deve ser um número maior que 1.", call. = FALSE)
  }
}

# Correção de população finita, opcional em todas as abas: quando a população
# N é conhecida e pequena, o n necessário cai. Como todo n daqui, sobe no teto.
ajustar_populacao_finita <- function(n, N) {
  as.integer(ceiling(n / (1 + (n - 1) / N)))
}

# Campos da correção de população finita, iguais nas quatro abas: a caixa liga
# a correção e, com ela ligada, aparece o campo do N.
populacao_finita_ui <- function(ns) {
  shiny::tagList(
    shiny::checkboxInput(ns("pf_usar"), "Corrigir para população finita (N conhecido)", value = FALSE),
    shiny::conditionalPanel(sprintf("input['%s']", ns("pf_usar")),
      shiny::numericInput(ns("pf_N"), "Tamanho da população (N):", value = 500, min = 2, step = 1))
  )
}

# Bloco de resultado comum às quatro abas novas: n em destaque com a frase de
# leitura, as premissas, a correção de população finita quando ligada (os dois
# n, sem e com correção) e as notas de arredondamento, ficha e 3Rs.
bloco_resultado_n <- function(res, ficha_ativa) {
  shiny::tagList(
    shiny::h3(res$destaque, style = "color:#0F3B5F;"),
    shiny::p(res$frase),
    shiny::div(class = "alert alert-info", res$premissas_texto),
    if (!is.null(res$N)) shiny::div(class = "alert alert-light border",
      sprintf("Correção de população finita (N = %d): sem correção seriam %s; com correção, %s. Anote no relatório qual dos dois foi usado.",
              as.integer(res$N), res$destaque_bruto, res$destaque)),
    shiny::p(class = "small", "O arredondamento foi feito para cima. Este n vem das premissas declaradas acima; anote-as no relatório junto com o n."),
    if (ficha_ativa) shiny::p(class = "small text-success", shiny::icon("link"),
      " Este n foi registrado na ficha de planejamento e segue para a planilha de coleta e o Projeto R."),
    shiny::p(class = "small text-muted mb-0",
      "Pelo princípio dos 3Rs, planeje o menor número de animais que ainda responda à pergunta.")
  )
}

# Molde do servidor das abas novas: o botão dispara a conta (que já trata o
# erro e devolve o objeto de erro), o resultado grava na ficha e o bloco comum
# desenha a saída. Cada aba só informa como ler os inputs e qual conta fazer.
# Quando a aba tem a sub-aba gráfica, `grafico` recebe o resultado e devolve
# o ggplot (curva com faixa do IC nas estimações; distribuições planejadas
# nas comparações), e `grafico_orientacao` é o texto visto antes do cálculo.
servidor_n_calculo <- function(id, ficha_destino_rv, conta, grafico = NULL,
                               grafico_orientacao = "Clique em Calcular n para ver a curva da distribuição do estimador com o n planejado e a faixa do intervalo de confiança.") {
  shiny::moduleServer(id, function(input, output, session) {
    resultado <- shiny::eventReactive(input$calcular, conta(input))
    shiny::observeEvent(resultado(), {
      res <- resultado()
      if (inherits(res, "error") || !is.function(ficha_destino_rv)) return()
      ficha_destino_rv(ficha_mesclar(ficha_destino_rv(), ficha_parte_n_calculo(res)))
    })
    output$resultado <- shiny::renderUI({
      res <- resultado()
      if (is.null(res)) {
        return(shiny::p(class = "small text-muted mb-0", "Informe os valores e clique em Calcular n."))
      }
      if (inherits(res, "error")) {
        return(shiny::div(class = "alert alert-warning mb-0", conditionMessage(res)))
      }
      bloco_resultado_n(res, is.function(ficha_destino_rv))
    })
    # Aba "Visualização gráfica": antes do cálculo, só a orientação.
    if (!is.null(grafico)) {
      output$grafico_ui <- shiny::renderUI({
        res <- resultado()
        if (is.null(res) || inherits(res, "error")) {
          return(shiny::p(class = "small text-muted mb-0", grafico_orientacao))
        }
        shiny::plotOutput(session$ns("grafico_ic"), height = "340px")
      })
      output$grafico_ic <- shiny::renderPlot({
        res <- resultado()
        shiny::req(res, !inherits(res, "error"))
        grafico(res)
      })
    }
    resultado
  })
}

# O N da população finita só conta quando a caixa está ligada.
ler_populacao <- function(input) {
  if (isTRUE(input$pf_usar)) input$pf_N else NULL
}

# ---- Peças da curva com faixa do IC (abas de estimação) -----------------------

# Erro-padrão do estimador com o n planejado: desvio/√n. Com população finita,
# encolhe pelo fator de correção √((N - n)/(N - 1)), só quando N passa de n.
erro_padrao_ic <- function(desvio, n, N = NULL) {
  ep <- desvio / sqrt(n)
  if (!is.null(N) && N > n) ep <- ep * sqrt((N - n) / (N - 1))
  ep
}

# Pontos da curva Normal do estimador: a grade cobre quatro erros-padrão para
# cada lado do centro (e fica dentro de `limites`, quando dados, como o [0, 1]
# da proporção). A faixa do IC é a região central da curva: centro ± z * EP,
# que com o n redondado para cima fica um passinho mais estreita que a margem
# digitada.
dados_curva_ic <- function(centro, ep, confianca, limites = NULL) {
  z <- stats::qnorm(1 - (1 - confianca) / 2)
  meia_largura <- z * ep
  de <- centro - 4 * ep
  ate <- centro + 4 * ep
  if (!is.null(limites)) {
    de <- max(de, limites[1])
    ate <- min(ate, limites[2])
  }
  x <- seq(de, ate, length.out = 200)
  list(
    curva = data.frame(x = x, y = stats::dnorm(x, centro, ep),
                       ic = x >= centro - meia_largura & x <= centro + meia_largura),
    centro = centro, ep = ep, confianca = confianca, meia_largura = meia_largura
  )
}

# O desenho, no mesmo estilo das curvas do comparar médias: área da curva
# inteira em seafoam suave, faixa do IC em teal firme, linha navy e traços
# coral nos limites do intervalo.
grafico_ic_ggplot <- function(info, rotulo_eixo, n) {
  curva <- info$curva
  faixa <- curva[curva$ic, ]
  ggplot2::ggplot(curva, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_area(fill = "#62B6B7", alpha = 0.18) +
    ggplot2::geom_area(
      data = faixa,
      ggplot2::aes(x = x, y = y),
      fill = "#2E7D8F", alpha = 0.45) +
    ggplot2::geom_line(colour = "#0F3B5F", linewidth = 1) +
    ggplot2::geom_vline(xintercept = info$centro, colour = "#0F3B5F", linewidth = 0.7) +
    ggplot2::geom_vline(xintercept = info$centro + c(-1, 1) * info$meia_largura, colour = "#E76F51", linetype = "dashed", linewidth = 0.7) +
    ggplot2::labs(x = rotulo_eixo, y = "densidade",
                  caption = sprintf("Planejamento: n = %d; erro-padrão %.2f; faixa sombreada: IC de %s (centro ± %.2f).",
                                    n, info$ep, rotulo_confianca(info$confianca), info$meia_largura)) +
    ggplot2::theme_minimal(base_size = 12)
}

# A curva da média amostral: X̄ ~ Normal(média esperada, desvio/√n).
grafico_ic_media <- function(res) {
  ep <- erro_padrao_ic(res$premissas$desvio, res$n, res$N)
  info <- dados_curva_ic(res$centro, ep, res$premissas$confianca)
  grafico_ic_ggplot(info, "média amostral", res$n)
}

# A curva da proporção amostral: p̂ ~ aprox. Normal(p, √(p(1-p)/n)), em [0, 1].
grafico_ic_proporcao <- function(res) {
  p <- res$premissas$p
  ep <- erro_padrao_ic(sqrt(p * (1 - p)), res$n, res$N)
  info <- dados_curva_ic(p, ep, res$premissas$confianca, limites = c(0, 1))
  grafico_ic_ggplot(info, "proporção amostral", res$n)
}

# ---- Peças das curvas de poder (abas de comparação) ---------------------------

# Pontos das curvas da proporção amostral de cada grupo: p̂ ~ aprox.
# Normal(p, √(p(1-p)/n)) com o n por grupo planejado; a grade cobre quatro
# erros-padrão para cada lado e fica dentro de [0, 1]. Serve à aba
# "Visualização gráfica" do Comparar proporções.
dados_curvas_duas_prop <- function(res) {
  rotulos <- c("Grupo A", "Grupo B")
  ps <- c(res$premissas$p1, res$premissas$p2)
  curvas <- do.call(rbind, lapply(seq_along(ps), function(i) {
    ep <- sqrt(ps[i] * (1 - ps[i]) / res$n)
    x <- seq(max(0, ps[i] - 4 * ep), min(1, ps[i] + 4 * ep), length.out = 200)
    data.frame(grupo = sprintf("%s (p = %g)", rotulos[i], ps[i]),
               x = x, y = stats::dnorm(x, ps[i], ep), marca = ps[i])
  }))
  # A ordem dos grupos na legenda segue a ordem digitada, não a alfabética.
  curvas$grupo <- factor(curvas$grupo, levels = unique(curvas$grupo))
  curvas
}

# Pontos das curvas da correlação amostral com o n planejado: pela
# transformação z de Fisher, atanh(r) é aprox. Normal(atanh(r), 1/√(n - 3)).
# A grade mora na escala z e a densidade volta à escala da correlação pelo
# jacobiano 1/(1 - r²); uma curva é centrada em zero (a H0 do teste) e a
# outra na correlação esperada. Serve à aba "Visualização gráfica" da Relação
# entre variáveis.
dados_curvas_correlacao <- function(res) {
  r <- res$premissas$r
  ep_z <- 1 / sqrt(res$n - 3)
  # A grade cobre as duas curvas: quatro erros-padrão além dos dois centros.
  z <- seq(min(0, atanh(r)) - 4 * ep_z, max(0, atanh(r)) + 4 * ep_z, length.out = 200)
  x <- tanh(z)
  curvas <- rbind(
    data.frame(curva = "Sob H0 (r = 0)", x = x, marca = 0,
               y = stats::dnorm(z, 0, ep_z) / (1 - x^2)),
    data.frame(curva = sprintf("Esperada (r = %g)", r), x = x, marca = r,
               y = stats::dnorm(z, atanh(r), ep_z) / (1 - x^2))
  )
  curvas$curva <- factor(curvas$curva, levels = unique(curvas$curva))
  curvas
}

# As duas curvas no mesmo estilo das do Comparar médias: área translúcida,
# linha firme, traço vertical em cada centro e legenda embaixo. `rotulo_x`,
# `rotulo_grupo` e a legenda da caixa de texto mudam de aba para aba.
grafico_poder_ggplot <- function(curvas, coluna_grupo, rotulo_x, legenda) {
  marcas <- curvas[!duplicated(curvas[[coluna_grupo]]), ]
  ggplot2::ggplot(curvas, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_area(
      ggplot2::aes(fill = .data[[coluna_grupo]]),
      alpha = 0.22, position = "identity", show.legend = FALSE) +
    ggplot2::geom_line(ggplot2::aes(colour = .data[[coluna_grupo]]), linewidth = 1) +
    ggplot2::geom_vline(
      data = marcas,
      ggplot2::aes(xintercept = marca, colour = .data[[coluna_grupo]]),
      linetype = "dashed", linewidth = 0.7, show.legend = FALSE) +
    ggplot2::scale_colour_manual(values = curvas_cores_n_poder(nlevels(curvas[[coluna_grupo]])), name = NULL) +
    ggplot2::scale_fill_manual(values = curvas_cores_n_poder(nlevels(curvas[[coluna_grupo]]))) +
    ggplot2::labs(x = rotulo_x, y = "densidade", caption = legenda) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "bottom")
}

# As distribuições da proporção amostral dos dois grupos com o n planejado.
grafico_poder_duas_prop <- function(res) {
  grafico_poder_ggplot(dados_curvas_duas_prop(res), "grupo", "proporção amostral",
    sprintf("Planejamento: n = %d por grupo; poder %.0f%%; α = %.0f%%. Distribuições da proporção amostral com o n planejado.",
            res$n, 100 * res$premissas$poder, 100 * res$premissas$alfa))
}

# A correlação amostral sob H0 e sob a correlação esperada com o n planejado.
grafico_poder_correlacao <- function(res) {
  grafico_poder_ggplot(dados_curvas_correlacao(res), "curva", "correlação amostral",
    sprintf("Planejamento: n = %d pares; poder %.0f%%; α = %.0f%%. Curvas sob H0 e sob a correlação esperada (transformação z de Fisher).",
            res$n, 100 * res$premissas$poder, 100 * res$premissas$alfa))
}

# ---- Para estimar: uma média -------------------------------------------------

# n para estimar uma média com margem de erro E: n = (z * desvio / E)^2.
calcular_n_media <- function(desvio, margem, confianca, N = NULL) {
  validar_populacao(N)
  if (!is.finite(desvio) || desvio <= 0 || !is.finite(margem) || margem <= 0 ||
      !is.finite(confianca) || confianca <= 0 || confianca >= 1) {
    stop("Revise o desvio-padrão e a margem de erro (maiores que zero) e a confiança (entre 0 e 1).", call. = FALSE)
  }
  # z da confiança bilateral: 95% vira 1,96.
  z <- stats::qnorm(1 - (1 - confianca) / 2)
  n_bruto <- as.integer(ceiling((z * desvio / margem)^2))
  n <- if (is.null(N)) n_bruto else ajustar_populacao_finita(n_bruto, N)
  list(
    n = n, n_total = n, n_bruto = n_bruto, N = N,
    unidade = "unidades independentes no total",
    metodo_descricao = "n para estimar uma média: n = (z * desvio / margem)^2",
    premissas = c(list(desvio = desvio, margem = margem, confianca = confianca),
                  if (!is.null(N)) list(N = N, n_bruto = n_bruto)),
    destaque = sprintf("%d indivíduos", n),
    destaque_bruto = sprintf("%d indivíduos", n_bruto),
    frase = sprintf("São necessários %d indivíduos para estimar a média com margem de erro de ±%g e %s de confiança.",
                    n, margem, rotulo_confianca(confianca)),
    premissas_texto = sprintf("Premissas: desvio-padrão %.2f; margem de erro ±%g; confiança de %s (z = %.2f).",
                              desvio, margem, rotulo_confianca(confianca), z)
  )
}

mod_n_media_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    planejamento_contexto_ui(
      "Quantos indivíduos para estimar uma média?",
      "Use antes da coleta quando o objetivo é estimar a média de uma variável (ex.: o peso médio dos peixes de uma fazenda) com uma margem de erro definida. O n conta unidades independentes, não subamostras de um mesmo tanque."
    ),
    bslib::layout_columns(
      col_widths = c(5, 7),
      bslib::card(
        bslib::card_header("Premissas da coleta"),
        bslib::card_body(
          # Grade de duas colunas dentro do cartão, igual à do teste t: à
          # esquerda os campos da variável; à direita confiança, população
          # finita e o botão.
          bslib::layout_columns(
            col_widths = c(6, 6),
            shiny::div(
              # A média esperada só centraliza o desenho da aba de visualização:
              # o cálculo do n depende do desvio-padrão e da margem, não dela.
              shiny::numericInput(ns("media"), "Média esperada da variável (só para o desenho):", value = 10, step = 0.5),
              shiny::numericInput(ns("desvio"), "Desvio-padrão esperado da variável:", value = 3, min = 0.01, step = 0.5),
              shiny::numericInput(ns("margem"), "Margem de erro aceita (± na mesma unidade da média):", value = 1, min = 0.01, step = 0.5)
            ),
            # O botão desce até a altura do campo da margem, fechando a grade.
            shiny::div(style = "display:flex; flex-direction:column; height:100%;",
              shiny::selectInput(ns("confianca"), "Nível de confiança:",
                choices = c("90%" = "0.90", "95%" = "0.95", "99%" = "0.99"), selected = "0.95"),
              populacao_finita_ui(ns),
              shiny::div(style = "margin-top:auto; padding-top:10px;",
                shiny::actionButton(ns("calcular"), "Calcular n", class = "btn-primary w-100"))
            )
          ),
          shiny::p(class = "small text-muted mt-2 mb-0",
            "O desvio-padrão vem de estudo piloto, literatura comparável ou uma premissa justificada. A margem de erro usa a mesma unidade da variável. A média esperada só serve para desenhar a curva; o n não depende dela.")
        )
      ),
      # Coluna da direita em abas: o resultado em texto e a curva com a faixa do IC.
      bslib::navset_card_tab(
        id = ns("saida"),
        bslib::nav_panel(
          "Tamanho amostral planejado",
          bslib::card_body(shiny::uiOutput(ns("resultado")))
        ),
        bslib::nav_panel(
          "Visualização gráfica",
          bslib::card_body(shiny::uiOutput(ns("grafico_ui")))
        )
      )
    )
  )
}

mod_n_media_server <- function(id, ficha_destino_rv = NULL) {
  servidor_n_calculo(id, ficha_destino_rv, function(input) {
    shiny::req(input$media, input$desvio, input$margem, input$confianca)
    tryCatch({
      res <- calcular_n_media(input$desvio, input$margem, as.numeric(input$confianca), ler_populacao(input))
      # Centro da curva da distribuição da média amostral (só o desenho o usa).
      res$centro <- input$media
      res
    },
      error = function(e) e)
  }, grafico = grafico_ic_media)
}

# ---- Para estimar: uma proporção ---------------------------------------------

# n para estimar uma proporção com margem de erro E: n = z^2 * p * (1 - p) / E^2.
# Sem palpite para p, 0,5 é o pior caso e dá o maior n.
calcular_n_proporcao <- function(p, margem, confianca, N = NULL) {
  validar_populacao(N)
  if (!is.finite(p) || p <= 0 || p >= 1 || !is.finite(margem) || margem <= 0 || margem >= 1 ||
      !is.finite(confianca) || confianca <= 0 || confianca >= 1) {
    stop("Revise a proporção esperada e a margem de erro (as duas entre 0 e 1) e a confiança.", call. = FALSE)
  }
  z <- stats::qnorm(1 - (1 - confianca) / 2)
  n_bruto <- as.integer(ceiling(z^2 * p * (1 - p) / margem^2))
  n <- if (is.null(N)) n_bruto else ajustar_populacao_finita(n_bruto, N)
  list(
    n = n, n_total = n, n_bruto = n_bruto, N = N,
    unidade = "unidades independentes no total",
    metodo_descricao = "n para estimar uma proporção: n = z^2 * p * (1 - p) / E^2",
    premissas = c(list(p = p, margem = margem, confianca = confianca),
                  if (!is.null(N)) list(N = N, n_bruto = n_bruto)),
    destaque = sprintf("%d indivíduos", n),
    destaque_bruto = sprintf("%d indivíduos", n_bruto),
    frase = sprintf("São necessários %d indivíduos para estimar a proporção com margem de erro de ±%g e %s de confiança.",
                    n, margem, rotulo_confianca(confianca)),
    premissas_texto = sprintf("Premissas: proporção esperada %.2f; margem de erro ±%g; confiança de %s (z = %.2f).",
                              p, margem, rotulo_confianca(confianca), z)
  )
}

mod_n_proporcao_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    planejamento_contexto_ui(
      "Quantos indivíduos para estimar uma proporção?",
      "Use antes da coleta quando o objetivo é estimar uma proporção da população (ex.: a proporção de fêmeas no desembarque ou a prevalência de uma doença no viveiro) com uma margem de erro definida. O n conta unidades independentes."
    ),
    bslib::layout_columns(
      col_widths = c(5, 7),
      bslib::card(
        bslib::card_header("Premissas da coleta"),
        bslib::card_body(
          # Grade de duas colunas dentro do cartão, igual à do teste t: à
          # esquerda proporção e margem; à direita confiança, população finita
          # e o botão.
          bslib::layout_columns(
            col_widths = c(6, 6),
            shiny::div(
              shiny::numericInput(ns("p"), "Proporção esperada (palpite; deixe 0,5 se não souber):", value = 0.5, min = 0.01, max = 0.99, step = 0.05),
              shiny::numericInput(ns("margem"), "Margem de erro aceita (± em torno da proporção):", value = 0.1, min = 0.005, max = 0.5, step = 0.01)
            ),
            # O botão desce até a altura do campo da margem, fechando a grade.
            shiny::div(style = "display:flex; flex-direction:column; height:100%;",
              shiny::selectInput(ns("confianca"), "Nível de confiança:",
                choices = c("90%" = "0.90", "95%" = "0.95", "99%" = "0.99"), selected = "0.95"),
              populacao_finita_ui(ns),
              shiny::div(style = "margin-top:auto; padding-top:10px;",
                shiny::actionButton(ns("calcular"), "Calcular n", class = "btn-primary w-100"))
            )
          ),
          shiny::p(class = "small text-muted mt-2 mb-0",
            "Sem palpite para a proporção, 0,5 é o pior caso e garante o maior n. A margem 0,1 significa ±10 pontos percentuais.")
        )
      ),
      # Coluna da direita em abas: o resultado em texto e a curva com a faixa do IC.
      bslib::navset_card_tab(
        id = ns("saida"),
        bslib::nav_panel(
          "Tamanho amostral planejado",
          bslib::card_body(shiny::uiOutput(ns("resultado")))
        ),
        bslib::nav_panel(
          "Visualização gráfica",
          bslib::card_body(shiny::uiOutput(ns("grafico_ui")))
        )
      )
    )
  )
}

mod_n_proporcao_server <- function(id, ficha_destino_rv = NULL) {
  servidor_n_calculo(id, ficha_destino_rv, function(input) {
    shiny::req(input$p, input$margem, input$confianca)
    tryCatch(
      calcular_n_proporcao(input$p, input$margem, as.numeric(input$confianca), ler_populacao(input)),
      error = function(e) e)
  }, grafico = grafico_ic_proporcao)
}

# ---- Para comparar: duas proporções ------------------------------------------

# n por grupo para comparar duas proporções: h de Cohen e pwr::pwr.2p.test.
calcular_n_duas_prop <- function(p1, p2, poder, alfa, N = NULL) {
  validar_populacao(N)
  if (!is.finite(p1) || p1 <= 0 || p1 >= 1 || !is.finite(p2) || p2 <= 0 || p2 >= 1 ||
      !is.finite(poder) || poder <= 0 || poder >= 1 || !is.finite(alfa) || alfa <= 0 || alfa >= 1) {
    stop("Revise as duas proporções (entre 0 e 1), o poder e o nível de significância.", call. = FALSE)
  }
  if (p1 == p2) {
    stop("As duas proporções são iguais. Defina a menor diferença relevante que deseja detectar.", call. = FALSE)
  }
  # h de Cohen mede a distância entre as proporções na escala do arco-seno.
  h <- abs(pwr::ES.h(p1, p2))
  calculo <- pwr::pwr.2p.test(h = h, sig.level = alfa, power = poder, alternative = "two.sided")
  n_bruto <- as.integer(ceiling(calculo$n))
  n <- if (is.null(N)) n_bruto else ajustar_populacao_finita(n_bruto, N)
  list(
    n = n, n_total = 2L * n, n_bruto = n_bruto, N = N,
    unidade = "unidades independentes por grupo",
    metodo_descricao = "n por poder: duas proporções (pwr::pwr.2p.test, h de Cohen)",
    premissas = c(list(p1 = p1, p2 = p2, h = h, poder = poder, alfa = alfa),
                  if (!is.null(N)) list(N = N, n_bruto_por_grupo = n_bruto)),
    destaque = sprintf("%d por grupo", n),
    destaque_bruto = sprintf("%d por grupo", n_bruto),
    frase = sprintf("São necessários %d indivíduos por grupo (%d no total) para comparar %.0f%% com %.0f%%.",
                    n, 2L * n, 100 * p1, 100 * p2),
    premissas_texto = sprintf("Premissas: proporções %.2f e %.2f (h de Cohen = %.2f); poder de %.0f%%; α = %.0f%%, bilateral.",
                              p1, p2, h, 100 * poder, 100 * alfa)
  )
}

mod_n_duas_prop_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    planejamento_contexto_ui(
      "Quantos indivíduos para comparar duas proporções?",
      "Use antes da coleta quando você espera uma diferença entre as proporções de dois grupos (ex.: sobrevivência em dois tratamentos) e quer uma chance definida de detectá-la. O n é por grupo e conta unidades independentes."
    ),
    bslib::layout_columns(
      col_widths = c(5, 7),
      bslib::card(
        bslib::card_header("Premissas da coleta"),
        bslib::card_body(
          # Grade de duas colunas dentro do cartão, igual à do Comparar
          # médias: à esquerda as proporções; à direita poder, significância,
          # população finita e o botão.
          bslib::layout_columns(
            col_widths = c(6, 6),
            shiny::div(
              shiny::numericInput(ns("p1"), "Proporção esperada no grupo A:", value = 0.4, min = 0.01, max = 0.99, step = 0.05),
              shiny::numericInput(ns("p2"), "Proporção esperada no grupo B:", value = 0.6, min = 0.01, max = 0.99, step = 0.05)
            ),
            # O botão desce até a altura do campo da proporção B, fechando a grade.
            shiny::div(style = "display:flex; flex-direction:column; height:100%;",
              shiny::sliderInput(ns("poder"), "Poder desejado:", min = 0.5, max = 0.95, value = 0.8, step = 0.05),
              shiny::selectInput(ns("alfa"), "Nível de significância:", choices = c("5%" = "0.05", "1%" = "0.01", "10%" = "0.10"), selected = "0.05"),
              populacao_finita_ui(ns),
              shiny::div(style = "margin-top:auto; padding-top:10px;",
                shiny::actionButton(ns("calcular"), "Calcular n", class = "btn-primary w-100"))
            )
          ),
          shiny::p(class = "small text-muted mt-2 mb-0",
            "As proporções vêm de estudo piloto, literatura comparável ou da menor diferença relevante para a pergunta.")
        )
      ),
      # Coluna da direita em abas: o resultado em texto e as curvas planejadas.
      bslib::navset_card_tab(
        id = ns("saida"),
        bslib::nav_panel(
          "Tamanho amostral planejado",
          bslib::card_body(shiny::uiOutput(ns("resultado")))
        ),
        bslib::nav_panel(
          "Visualização gráfica",
          bslib::card_body(shiny::uiOutput(ns("grafico_ui")))
        )
      )
    )
  )
}

mod_n_duas_prop_server <- function(id, ficha_destino_rv = NULL) {
  servidor_n_calculo(id, ficha_destino_rv, function(input) {
    shiny::req(input$p1, input$p2, input$poder, input$alfa)
    tryCatch(
      calcular_n_duas_prop(input$p1, input$p2, input$poder, as.numeric(input$alfa), ler_populacao(input)),
      error = function(e) e)
  }, grafico = grafico_poder_duas_prop,
     grafico_orientacao = "Clique em Calcular n para ver as distribuições da proporção amostral dos dois grupos com o n planejado.")
}

# ---- Para comparar: relação entre variáveis ----------------------------------

# n de pares para detectar uma correlação r: pwr::pwr.r.test.
calcular_n_correlacao <- function(r, poder, alfa, N = NULL) {
  validar_populacao(N)
  if (!is.finite(r) || abs(r) >= 1 || r == 0 ||
      !is.finite(poder) || poder <= 0 || poder >= 1 || !is.finite(alfa) || alfa <= 0 || alfa >= 1) {
    stop("Revise a correlação esperada (entre -1 e 1, diferente de zero), o poder e a significância.", call. = FALSE)
  }
  calculo <- pwr::pwr.r.test(r = r, sig.level = alfa, power = poder, alternative = "two.sided")
  n_bruto <- as.integer(ceiling(calculo$n))
  n <- if (is.null(N)) n_bruto else ajustar_populacao_finita(n_bruto, N)
  list(
    n = n, n_total = n, n_bruto = n_bruto, N = N,
    unidade = "unidades independentes no total",
    metodo_descricao = "n por poder: correlação (pwr::pwr.r.test)",
    premissas = c(list(r = r, poder = poder, alfa = alfa),
                  if (!is.null(N)) list(N = N, n_bruto = n_bruto)),
    destaque = sprintf("%d pares de observações", n),
    destaque_bruto = sprintf("%d pares de observações", n_bruto),
    frase = sprintf("São necessários %d indivíduos, cada um com as duas medidas, para detectar uma correlação de %.2f.",
                    n, r),
    premissas_texto = sprintf("Premissas: correlação esperada %.2f; poder de %.0f%%; α = %.0f%%, bilateral.",
                              r, 100 * poder, 100 * alfa)
  )
}

mod_n_correlacao_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    planejamento_contexto_ui(
      "Quantos pares para medir uma relação entre variáveis?",
      "Use antes da coleta quando o objetivo é medir a força da relação entre duas variáveis quantitativas (ex.: peso e comprimento) com uma chance definida de detectá-la. Cada indivíduo fornece um par de medidas; a análise desses dados é a correlação, ponte para a regressão no menu Modelos de Regressão."
    ),
    bslib::layout_columns(
      col_widths = c(5, 7),
      bslib::card(
        bslib::card_header("Premissas da coleta"),
        bslib::card_body(
          # Grade de duas colunas dentro do cartão, igual à do Comparar
          # médias: à esquerda a correlação; à direita poder, significância,
          # população finita e o botão.
          bslib::layout_columns(
            col_widths = c(6, 6),
            shiny::div(
              shiny::numericInput(ns("r"), "Correlação esperada (sinal conforme a relação):", value = 0.3, min = -0.95, max = 0.95, step = 0.05)
            ),
            # O botão desce até a altura do campo da correlação, fechando a grade.
            shiny::div(style = "display:flex; flex-direction:column; height:100%;",
              shiny::sliderInput(ns("poder"), "Poder desejado:", min = 0.5, max = 0.95, value = 0.8, step = 0.05),
              shiny::selectInput(ns("alfa"), "Nível de significância:", choices = c("5%" = "0.05", "1%" = "0.01", "10%" = "0.10"), selected = "0.05"),
              populacao_finita_ui(ns),
              shiny::div(style = "margin-top:auto; padding-top:10px;",
                shiny::actionButton(ns("calcular"), "Calcular n", class = "btn-primary w-100"))
            )
          ),
          shiny::p(class = "small text-muted mt-2 mb-0",
            "A correlação esperada vem de estudo piloto, literatura comparável ou da menor relação relevante para a pergunta. Na dúvida, 0,3 é uma relação fraca a moderada.")
        )
      ),
      # Coluna da direita em abas: o resultado em texto e as curvas planejadas.
      bslib::navset_card_tab(
        id = ns("saida"),
        bslib::nav_panel(
          "Tamanho amostral planejado",
          bslib::card_body(shiny::uiOutput(ns("resultado")))
        ),
        bslib::nav_panel(
          "Visualização gráfica",
          bslib::card_body(shiny::uiOutput(ns("grafico_ui")))
        )
      )
    )
  )
}

mod_n_correlacao_server <- function(id, ficha_destino_rv = NULL) {
  servidor_n_calculo(id, ficha_destino_rv, function(input) {
    shiny::req(input$r, input$poder, input$alfa)
    tryCatch(
      calcular_n_correlacao(input$r, input$poder, as.numeric(input$alfa), ler_populacao(input)),
      error = function(e) e)
  }, grafico = grafico_poder_correlacao,
     grafico_orientacao = "Clique em Calcular n para ver a distribuição da correlação amostral sob H0 e sob a correlação esperada, com o n planejado.")
}
