# =============================================================================
# Como sortear a amostra (menu Planejando sua Pesquisa)
# -----------------------------------------------------------------------------
# Ferramenta de ANTES da coleta. Ainda não há dado medido, só a lista de
# unidades candidatas: o marco amostral. O pesquisador monta essa tabela uma
# vez (importando, colando ou gerando de 1 a N), corrige ou acrescenta colunas
# ali mesmo, e pode sortear de três formas sem reconstruir nada. As unidades
# sorteadas seguem para o Planejamento de Variáveis, que gera a planilha de
# coleta e o dicionário.
#
# Distinto de "Sortear subamostra" em Preparar Dados: lá se recorta uma base
# que já foi medida; aqui se escolhe quem será medido.
#
# O sorteio em si mora em templates/funcoes_sorteio.R (sortear_marco), para
# que a tela, os delineamentos e o projeto R exportado usem a mesma função.
#
# Seções:
#   1. Leitura do marco (arquivo, texto colado, geração de 1 a N)
#   2. Interface
#   3. Servidor
# =============================================================================

library(shiny)
library(bslib)
library(DT)

if (file.exists("templates/funcoes_sorteio.R")) {
  source("templates/funcoes_sorteio.R", encoding = "UTF-8")
}


# ---- 1. Leitura do marco -----------------------------------------------------

# Lê um CSV ou Excel e devolve a tabela do marco.
ler_marco_arquivo <- function(caminho, nome_arquivo, aba = NULL) {
  # A extensão diz qual leitor usar.
  extensao <- tolower(tools::file_ext(nome_arquivo))
  tabela <- if (extensao %in% c("xlsx", "xls")) {
    # No Excel, lemos a aba escolhida (ou a primeira).
    as.data.frame(readxl::read_excel(caminho, sheet = aba %||% 1))
  } else {
    # No CSV, olhamos a primeira linha para descobrir o separador.
    primeira <- readLines(caminho, n = 1, warn = FALSE, encoding = "UTF-8")
    # Planilhas brasileiras costumam usar ponto e vírgula e decimal com vírgula.
    usa_ponto_virgula <- lengths(regmatches(primeira, gregexpr(";", primeira))) >
      lengths(regmatches(primeira, gregexpr(",", primeira)))
    utils::read.csv(caminho,
      sep = if (usa_ponto_virgula) ";" else ",",
      dec = if (usa_ponto_virgula) "," else ".",
      stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8-BOM")
  }
  # Linhas totalmente vazias (comuns no fim de planilhas) não são unidades.
  vazia <- apply(tabela, 1, function(linha) all(is.na(linha) | trimws(as.character(linha)) == ""))
  tabela[!vazia, , drop = FALSE]
}

# Lê uma lista colada ou digitada: uma unidade por linha.
# Se as linhas tiverem tabulação ou ponto e vírgula, cada pedaço vira uma coluna.
ler_marco_texto <- function(texto, cabecalho = FALSE) {
  # Separamos as linhas e descartamos as vazias.
  linhas <- trimws(strsplit(texto %||% "", "\r?\n")[[1]])
  linhas <- linhas[nzchar(linhas)]
  if (!length(linhas)) return(NULL)
  # Descobrimos se o texto veio de uma planilha (com colunas) ou é uma lista simples.
  separador <- dplyr::case_when(
    any(grepl("\t", linhas)) ~ "\t",
    any(grepl(";", linhas)) ~ ";",
    TRUE ~ ""
  )
  # Lista simples: uma coluna só, que é o identificador.
  if (!nzchar(separador)) {
    nome <- if (cabecalho) gerar_nome_reduzido(linhas[1]) else "id_unidade"
    valores <- if (cabecalho) linhas[-1] else linhas
    return(stats::setNames(data.frame(valores, stringsAsFactors = FALSE), nome))
  }
  # Texto com colunas: lemos como uma pequena tabela.
  utils::read.table(text = linhas, sep = separador, header = cabecalho,
    stringsAsFactors = FALSE, check.names = FALSE, quote = "\"", comment.char = "",
    fill = TRUE, strip.white = TRUE)
}

# Gera um marco numerado de 1 a N, para quando ainda não existe lista.
gerar_marco_numerado <- function(N, prefixo = "") {
  # Números com zeros à esquerda ordenam bem no Excel (U001, U002, ...).
  digitos <- nchar(as.character(N))
  ids <- if (nzchar(prefixo)) sprintf("%s%0*d", prefixo, digitos, seq_len(N)) else seq_len(N)
  data.frame(id_unidade = ids, stringsAsFactors = FALSE)
}

# ---- Nova coluna gerada por níveis ou blocos ---------------------------------

# Valores da nova coluna, na ordem das linhas, conforme o que o pesquisador pediu.
# papel: "estrato", "ordem", "conglomerado" ou "outra".
# modo : "niveis" (lista de níveis e repetições), "livre" (vazia), "sequencia" (1 a N)
#        ou "blocos" (conglomerados rotulados, cada um com o mesmo número de unidades).
valores_nova_coluna <- function(modo, N = 0, niveis = character(), repeticoes = integer(),
                                n_grupos = NA, por_grupo = NA, prefixo = "") {
  switch(modo,
    # Valor livre: nada a gerar; a coluna nasce vazia, do tamanho da lista.
    livre = rep(NA_character_, N),
    # Ordem: 1, 2, 3... até N, na ordem atual da lista.
    sequencia = seq_len(N),
    # Níveis: cada nível repetido quantas vezes o pesquisador disse, um após o outro.
    niveis = rep(as.character(niveis), times = as.integer(repeticoes)),
    # Conglomerados: rótulos E01, E02... cada um repetido para as suas unidades.
    blocos = {
      digitos <- nchar(as.character(n_grupos))
      rotulos <- sprintf("%s%0*d", prefixo, digitos, seq_len(n_grupos))
      rep(rotulos, each = as.integer(por_grupo))
    }
  )
}

# Continua a numeração dos identificadores para as linhas criadas (5, 6... ou E06, E07...).
# Se não houver um padrão claro, as novas linhas ficam sem identificador para o pesquisador nomear.
continuar_ids <- function(ids, quantas) {
  existentes <- as.character(ids[!is.na(ids) & trimws(as.character(ids)) != ""])
  # Lista vazia: começamos do 1.
  if (!length(existentes)) return(seq_len(quantas))
  # Só números: seguimos do maior.
  if (all(grepl("^[0-9]+$", existentes))) return(max(as.numeric(existentes)) + seq_len(quantas))
  # Prefixo + número (E01, P010): seguimos com o mesmo prefixo e a mesma largura.
  partes <- regmatches(existentes, regexec("^(.*?)([0-9]+)$", existentes))
  prefixos <- unique(vapply(partes, function(x) if (length(x) == 3) x[2] else NA_character_, character(1)))
  if (length(prefixos) == 1 && !is.na(prefixos)) {
    numeros <- as.integer(vapply(partes, `[`, character(1), 3))
    largura <- max(nchar(vapply(partes, `[`, character(1), 3)))
    return(sprintf("%s%0*d", prefixos, largura, max(numeros) + seq_len(quantas)))
  }
  # Sem padrão: deixamos em branco (a tabela destaca essas linhas).
  rep(NA, quantas)
}

# Acrescenta a coluna à lista, resolvendo a diferença entre as repetições e o N.
# ajuste: "criar" (cria as linhas que faltam), "cortar" (usa só as primeiras N repetições),
#         "remover" (tira da lista as linhas que sobram) ou "branco" (deixa essas linhas vazias).
aplicar_nova_coluna <- function(marco, nome, valores, ajuste = NULL, id = NULL) {
  # Lista ainda vazia: a própria coluna monta a lista, com unidades numeradas.
  if (is.null(marco) || !nrow(marco)) {
    marco <- data.frame(id_unidade = seq_along(valores))
    marco[[nome]] <- valores
    return(marco)
  }
  N <- nrow(marco)
  total <- length(valores)
  id <- id %||% names(marco)[1]
  # Mais repetições do que unidades: criar linhas ou cortar a coluna.
  if (total > N) {
    if (identical(ajuste, "criar")) {
      novas <- marco[rep(1, total - N), , drop = FALSE]
      novas[, ] <- NA
      novas[[id]] <- continuar_ids(marco[[id]], total - N)
      marco <- rbind(marco, novas)
    } else {
      valores <- valores[seq_len(N)]
    }
  }
  # Menos repetições do que unidades: tirar as linhas sobrando ou deixá-las em branco.
  if (total < N) {
    if (identical(ajuste, "remover")) {
      marco <- marco[seq_len(total), , drop = FALSE]
    } else {
      valores <- c(valores, rep(NA, N - total))
    }
  }
  rownames(marco) <- NULL
  marco[[nome]] <- valores
  marco
}

# Frase curta que resume o sorteio para o pesquisador.
resumir_sorteio <- function(sorteio) {
  r <- sorteio$registro
  base <- sprintf("%d de %d unidades sorteadas com semente %d.", r$n, r$N, r$semente)
  detalhe <- switch(r$metodo,
    aas = if (isTRUE(r$corrigir_finita)) {
      sprintf("Aleatória simples. O n informado (%d) foi ajustado para %d pela correção para população finita.", r$n_pedido, r$n)
    } else "Aleatória simples: todas as unidades tinham a mesma chance.",
    estratificada = sprintf("Estratificada %s por %s, em %d estratos.",
      if (identical(r$alocacao, "igual")) "com o mesmo n em cada estrato" else "proporcional",
      r$estrato, nrow(sorteio$alocacao)),
    sistematica = sprintf("Sistemática: uma unidade a cada %d, na ordem de %s, começando pela %dª.", r$k, r$ordem, r$partida),
    conglomerados = sprintf("Conglomerados: %d de %d grupos de %s sorteados; %s.", r$n_conglomerados, r$conglomerados_disponiveis, r$conglomerado,
      if (identical(r$estagios, "dois")) sprintf("dentro de cada um, %d unidades sorteadas (dois estágios)", r$m)
      else "todas as unidades de cada um entram (estágio único)")
  )
  paste(base, detalhe)
}


# ---- 2. Interface ------------------------------------------------------------

mod_sortear_amostra_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    # Liga e desliga as abas seguintes, para que o fluxo siga a ordem certa.
    shiny::tags$script(shiny::HTML(sprintf(
      "Shiny.addCustomMessageHandler('%s', function(msg) {
         document.querySelectorAll('#%s .nav-link').forEach(function(a) {
           if (msg.abas.indexOf(a.getAttribute('data-value')) >= 0) {
             a.classList.toggle('disabled', !msg.ativo);
             a.setAttribute('aria-disabled', !msg.ativo);
             a.title = msg.ativo ? '' : 'Primeiro defina a lista em De onde sortear';
           }
         });
       });", ns("abas_liberadas"), ns("passos")))),
    planejamento_contexto_ui(
      "Sortear antes de medir",
      "Faça a lista das unidades, sorteie quais serão visitadas e declare o que medir em cada uma. O resultado é a planilha de coleta, já com as colunas do sorteio preenchidas. Para recortar dados que já foram medidos, use Sortear subamostra em Preparar Dados."
    ),
    # Cabeçalho "Planejamento atual", lido da ficha.
    shiny::uiOutput(ns("planejamento_atual")),
    bslib::navset_card_tab(
      id = ns("passos"),

      # Passo 1: montar a lista (o marco amostral).
      bslib::nav_panel("1. De onde sortear", value = "marco", icon = shiny::icon("list-ul"),
        shiny::p(class = "mb-3", style = "font-size:1.02rem;", shiny::HTML(
          "Monte aqui a lista de todas as unidades que poderiam ser sorteadas, uma por linha, por exemplo uma embarcação, uma estação ou uma praia. Na estatística, essa lista se chama <b>marco amostral</b>.")),
        bslib::layout_columns(
          col_widths = c(4, 8),
          # À esquerda, as duas formas de montar a lista.
          shiny::div(
            bslib::navset_pill(
              id = ns("modo_entrada"), selected = "importar",
              # Importar reúne o arquivo e a lista colada: quem tem arquivo escolhe o arquivo,
              # quem tem uma lista curta de nomes cola ali mesmo.
              bslib::nav_panel("Importar", value = "importar",
                shiny::div(class = "pt-2",
                  shiny::p(class = "small text-muted mb-1", "Tem a lista num arquivo CSV ou Excel? Escolha o arquivo."),
                  shiny::fileInput(ns("arquivo"), NULL, accept = c(".csv", ".xlsx", ".xls"),
                    buttonLabel = "Escolher arquivo", placeholder = "nenhum arquivo"),
                  shiny::uiOutput(ns("aba_ui")),
                  shiny::p(class = "small text-muted mb-1", "Ou cole a lista aqui, uma unidade por linha."),
                  shiny::textAreaInput(ns("texto"), NULL, rows = 7, width = "100%",
                    placeholder = "Barco A\nBarco B\nBarco C"),
                  shiny::checkboxInput(ns("texto_cabecalho"), "A primeira linha traz os nomes das colunas", FALSE),
                  shiny::helpText("Marque só se colou do Excel com uma linha de títulos. As colunas são reconhecidas pela tabulação.")
                )
              ),
              bslib::nav_panel("Gerar 1 a N", value = "gerar",
                shiny::div(class = "pt-2",
                  shiny::p(class = "small text-muted", "Para quando as unidades não têm nome e são apenas numeradas, de 1 até N (tanques, pontos de um transecto)."),
                  shiny::numericInput(ns("gerar_N"), "Quantas unidades existem (N)?", value = 50, min = 1, step = 1),
                  shiny::textInput(ns("gerar_prefixo"), "Prefixo do número (opcional):", placeholder = "E para E01, E02..."),
                  shiny::helpText("Sortear pontos dentro de uma área virá numa próxima versão.")
                )
              )
            ),
            # Um único botão primário, que vale para a forma de entrada escolhida.
            shiny::actionButton(ns("usar_marco"), "Usar como marco", icon = shiny::icon("check"), class = "btn-primary w-100 mt-2")
          ),
          # À direita, a lista nascendo enquanto se escreve.
          shiny::div(
            shiny::uiOutput(ns("lista_cabecalho")),
            shiny::uiOutput(ns("lista_barra")),
            DT::DTOutput(ns("marco_tabela")),
            shiny::uiOutput(ns("lista_ferramentas"))
          )
        )
      ),

      # Passo 2: escolher o tipo de amostragem e sortear.
      bslib::nav_panel("2. Sorteio", value = "sorteio", icon = shiny::icon("shuffle"),
        # O tipo de amostragem vem primeiro e em destaque.
        bslib::card(
          class = "mb-3", style = "border-left: 4px solid #0F3B5F;", fill = FALSE,
          bslib::card_header(shiny::tags$b("Tipo de amostragem")),
          bslib::card_body(fill = FALSE, class = "py-2",
            shiny::radioButtons(ns("metodo"), NULL, inline = TRUE,
              choices = c("Aleatória simples" = "aas",
                          "Estratificada proporcional" = "estratificada",
                          "Sistemática" = "sistematica",
                          "Conglomerados (em dois estágios)" = "conglomerados")),
            shiny::uiOutput(ns("metodo_dica"))
          )
        ),
        bslib::layout_columns(
          col_widths = c(4, 8),
          shiny::div(
            shiny::numericInput(ns("semente"), "Semente (anote para reproduzir):", value = NA, min = 1, step = 1),
            shiny::conditionalPanel(
              sprintf("['aas', 'estratificada'].includes(input['%s'])", ns("metodo")),
              shiny::numericInput(ns("n"), "Quantas unidades sortear (n)?", value = 10, min = 1, step = 1)
            ),
            shiny::conditionalPanel(
              sprintf("input['%s'] === 'aas'", ns("metodo")),
              shiny::checkboxInput(ns("corrigir_finita"), "Aplicar correção para população finita", FALSE),
              shiny::uiOutput(ns("fpc_dica"))
            ),
            shiny::conditionalPanel(
              sprintf("input['%s'] === 'estratificada'", ns("metodo")),
              shiny::selectInput(ns("estrato"), "Coluna de estrato:", choices = NULL)
            ),
            shiny::conditionalPanel(
              sprintf("input['%s'] === 'sistematica'", ns("metodo")),
              shiny::selectInput(ns("ordem"), "Coluna de ordem:", choices = NULL),
              shiny::numericInput(ns("k"), "Intervalo k (uma unidade a cada k):", value = 5, min = 1, step = 1),
              shiny::uiOutput(ns("k_dica"))
            ),
            shiny::conditionalPanel(
              sprintf("input['%s'] === 'conglomerados'", ns("metodo")),
              shiny::selectInput(ns("conglomerado"), "Coluna de conglomerado:", choices = NULL),
              shiny::numericInput(ns("n_conglomerados"), "1º estágio: quantos conglomerados sortear?", value = 6, min = 1, step = 1),
              shiny::uiOutput(ns("conglomerado_dica")),
              shiny::radioButtons(ns("estagios"), "2º estágio: dentro de cada conglomerado sorteado,",
                choices = c("medir todas as unidades (estágio único)" = "unico",
                            "sortear algumas unidades (dois estágios)" = "dois")),
              shiny::helpText("Se dá para medir tudo no conglomerado, use estágio único; se é muita coisa, subamostre."),
              shiny::conditionalPanel(
                sprintf("input['%s'] === 'dois'", ns("estagios")),
                shiny::numericInput(ns("m"), "Unidades por conglomerado (m):", value = 5, min = 1, step = 1)
              )
            ),
            shiny::uiOutput(ns("falta_coluna")),
            shiny::actionButton(ns("sortear"), "Sortear", icon = shiny::icon("shuffle"), class = "btn-primary w-100")
          ),
          shiny::div(
            shiny::uiOutput(ns("previa_alocacao")),
            shiny::uiOutput(ns("resultado_resumo")),
            DT::DTOutput(ns("sorteados_tabela")),
            shiny::uiOutput(ns("resultado_acoes"))
          )
        )
      ),

      # Passo 3: declarar o que será medido (Planejamento de Variáveis existente).
      bslib::nav_panel("3. Planilha de coleta", value = "coleta", icon = shiny::icon("table-list"),
        mod_planejamento_variaveis_ui(ns("variaveis"), modo = "sorteio")
      )
    )
  )
}


# ---- 3. Servidor -------------------------------------------------------------

# Motivo de atenção de cada linha: sem identificador ou identificador repetido.
problemas_por_linha <- function(tabela, id) {
  ids <- trimws(as.character(tabela[[id]]))
  vazio <- is.na(ids) | ids == ""
  repetido <- !vazio & (duplicated(ids) | duplicated(ids, fromLast = TRUE))
  dplyr::case_when(
    vazio ~ "sem identificador: preencha ou remova a linha",
    repetido ~ "identificador repetido: cada linha deve ser uma unidade diferente",
    TRUE ~ ""
  )
}

# Devolve um reativo com o último sorteio (ou NULL). Os delineamentos
# observacionais leem esse mesmo resultado para montar a sua ficha de coleta.
mod_sortear_amostra_server <- function(id, ficha_destino_rv = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # A lista confirmada (marco amostral); toda edição passa por ela.
    marco_rv <- shiny::reactiveVal(NULL)
    # Conta mudanças de estrutura (novas colunas, novas linhas) para redesenhar a tabela.
    versao_rv <- shiny::reactiveVal(0)
    # O último sorteio feito sobre a lista atual.
    sorteio_rv <- shiny::reactiveVal(NULL)
    output$planejamento_atual <- shiny::renderUI({
      if (is.function(ficha_destino_rv)) ficha_cabecalho_ui(ficha_destino_rv())
    })
    # "Assinatura" do que foi confirmado, para saber se o que está à esquerda mudou depois.
    fonte_confirmada_rv <- shiny::reactiveVal("")
    # Mostra a orientação do próximo passo logo depois de confirmar.
    recem_confirmado_rv <- shiny::reactiveVal(FALSE)
    # Coluna recém-criada que deve aparecer já escolhida no seletor do seu método.
    preferencia_rv <- shiny::reactiveVal(NULL)

    # Troca a lista inteira e zera o sorteio, que só vale para a lista em que foi feito.
    definir_marco <- function(tabela) {
      marco_rv(as.data.frame(tabela, stringsAsFactors = FALSE, check.names = FALSE))
      versao_rv(versao_rv() + 1)
      sorteio_rv(NULL)
    }

    # Com uma coluna só, ela é o identificador; com mais, vale a escolha do pesquisador.
    id_atual <- shiny::reactive({
      m <- marco_rv()
      if (is.null(m)) return(NULL)
      escolhido <- input$coluna_id
      if (ncol(m) > 1 && !is.null(escolhido) && escolhido %in% names(m)) escolhido else names(m)[1]
    })

    # As abas Sorteio e Planilha de coleta só abrem com a lista definida.
    shiny::observe({
      session$sendCustomMessage(ns("abas_liberadas"),
        list(abas = list("sorteio", "coleta"), ativo = !is.null(marco_rv())))
    })

    # --- Entradas: prévia ao vivo -------------------------------------------

    # No Excel, o pesquisador escolhe a aba.
    output$aba_ui <- shiny::renderUI({
      arq <- input$arquivo
      shiny::req(arq, tolower(tools::file_ext(arq$name)) %in% c("xlsx", "xls"))
      shiny::selectInput(ns("aba"), "Aba:", choices = readxl::excel_sheets(arq$datapath))
    })

    # O texto é lido um instante depois de o pesquisador parar de digitar.
    texto_calmo <- shiny::debounce(shiny::reactive(input$texto %||% ""), 300)

    # Dentro de Importar há duas fontes (arquivo e texto colado): vale a última usada.
    fonte_importar_rv <- shiny::reactiveVal("texto")
    shiny::observeEvent(input$arquivo, fonte_importar_rv("arquivo"))
    shiny::observeEvent(texto_calmo(), {
      if (nzchar(trimws(texto_calmo()))) fonte_importar_rv("texto")
    }, ignoreInit = TRUE)

    # O que a forma de entrada escolhida produziria agora: tabela, assinatura e erro.
    previa <- shiny::reactive({
      modo <- input$modo_entrada %||% "importar"
      # Em Importar, o arquivo vale se foi o último usado; senão, o texto colado.
      if (identical(modo, "importar")) modo <- if (identical(fonte_importar_rv(), "arquivo") && !is.null(input$arquivo)) "arquivo" else "texto"
      resultado <- tryCatch(switch(modo,
        arquivo = {
          arq <- input$arquivo
          list(tabela = ler_marco_arquivo(arq$datapath, arq$name, input$aba),
               assinatura = paste("arquivo", arq$name, arq$size, input$aba %||% ""),
               origem = sprintf("do arquivo %s", arq$name))
        },
        texto = {
          texto <- texto_calmo()
          if (!nzchar(trimws(texto))) return(NULL)
          list(tabela = ler_marco_texto(texto, isTRUE(input$texto_cabecalho)),
               assinatura = paste("texto", texto, isTRUE(input$texto_cabecalho)),
               origem = "da lista colada")
        },
        gerar = {
          N <- suppressWarnings(as.integer(input$gerar_N))
          if (is.na(N) || N < 1) return(NULL)
          prefixo <- trimws(input$gerar_prefixo %||% "")
          list(tabela = gerar_marco_numerado(N, prefixo), assinatura = paste("gerar", N, prefixo),
               origem = "numerada de 1 a N")
        }
      ), error = function(e) list(erro = conditionMessage(e)))
      resultado
    })

    # Há uma prévia diferente da lista já confirmada?
    pendente <- shiny::reactive({
      p <- previa()
      !is.null(p) && is.null(p$erro) && !is.null(p$tabela) && nrow(p$tabela) > 0 &&
        !identical(p$assinatura, fonte_confirmada_rv())
    })

    # Confirmar: a prévia vira a lista (marco amostral) usada no sorteio.
    shiny::observeEvent(input$usar_marco, {
      p <- previa()
      if (!is.null(p$erro)) {
        shiny::showNotification(paste("Não foi possível ler a lista:", p$erro), type = "error")
        return()
      }
      if (is.null(p) || is.null(p$tabela) || !nrow(p$tabela)) {
        shiny::showNotification("Escreva, cole, importe ou gere a lista primeiro.", type = "warning")
        return()
      }
      definir_marco(p$tabela)
      fonte_confirmada_rv(p$assinatura)
      recem_confirmado_rv(TRUE)
    })

    shiny::observeEvent(input$ir_sorteio, bslib::nav_select("passos", "sorteio"))

    # --- A tabela à direita --------------------------------------------------

    # A tabela mostrada: a prévia enquanto não confirmada, depois a lista editável.
    tabela_mostrada <- shiny::reactive({
      if (pendente()) previa()$tabela else marco_rv()
    })

    output$lista_cabecalho <- shiny::renderUI({
      p <- previa()
      if (!is.null(p$erro)) {
        return(shiny::div(class = "alert alert-danger py-2", "Não consegui ler a lista: ", p$erro))
      }
      tab <- tabela_mostrada()
      if (is.null(tab)) {
        return(shiny::div(class = "alert alert-light border",
          shiny::tags$b("Prévia da sua lista"), shiny::tags$br(),
          "Cole à esquerda uma unidade por linha, ou escolha um arquivo, e veja a lista nascer aqui."))
      }
      id <- if (pendente()) names(tab)[1] else id_atual()
      n_atencao <- sum(nzchar(problemas_por_linha(tab, id)))
      contagem <- shiny::p(class = "mb-2",
        shiny::tags$b(sprintf("N = %d unidades candidatas", nrow(tab))),
        if (n_atencao) shiny::span(class = "text-danger",
          sprintf(" · %d linha(s) precisam de atenção, destacadas em vermelho com o motivo na última coluna.", n_atencao)))
      if (pendente()) {
        shiny::tagList(
          shiny::h6(class = "fw-bold mb-1", "Prévia da sua lista",
            shiny::span(class = "fw-normal text-muted small", paste0(" (", previa()$origem %||% "", ")"))),
          if (!is.null(marco_rv())) shiny::p(class = "small text-muted mb-1", "Ainda não confirmada: ao clicar em Usar como marco, ela substitui a lista atual."),
          contagem
        )
      } else {
        shiny::tagList(
          if (isTRUE(recem_confirmado_rv())) shiny::div(class = "alert alert-success d-flex justify-content-between align-items-center py-2",
            shiny::span(shiny::icon("circle-check"), " Pronto, agora vá para a aba Sorteio para escolher como sortear."),
            shiny::actionButton(ns("ir_sorteio"), "Ir para o Sorteio", icon = shiny::icon("arrow-right"), class = "btn-sm btn-success")),
          shiny::h6(class = "fw-bold mb-1", "Sua lista (marco amostral)"),
          contagem
        )
      }
    })

    # Acrescenta a coluna de atenção só quando alguma linha tem problema.
    com_atencao <- function(tab, id) {
      motivo <- problemas_por_linha(tab, id)
      if (any(nzchar(motivo))) tab[["Atenção"]] <- motivo
      tab
    }

    # Monta a tabela: prévia só para ver; lista confirmada editável.
    desenhar_tabela <- function(tab, id, editavel) {
      tab <- com_atencao(tab, id)
      tem_atencao <- "Atenção" %in% names(tab)
      editar <- if (!editavel) FALSE else if (tem_atencao) {
        list(target = "cell", disable = list(columns = ncol(tab) - 1))
      } else list(target = "cell")
      dt <- DT::datatable(tab, rownames = FALSE, editable = editar,
        selection = if (editavel) "multiple" else "none",
        options = list(pageLength = 10, scrollX = TRUE, dom = "tip",
          language = list(info = "_START_ a _END_ de _TOTAL_", paginate = list(previous = "Anterior", `next` = "Próxima"))),
        class = "stripe hover compact")
      if (tem_atencao) {
        # Pinta a linha inteira com problema. O listrado do tema usa sombra interna nas células,
        # por isso o destaque também vai como sombra interna (um fundo comum ficaria escondido).
        motivos <- unique(tab[["Atenção"]][nzchar(tab[["Atenção"]])])
        dt <- DT::formatStyle(dt, columns = names(tab), valueColumns = "Atenção",
          boxShadow = DT::styleEqual(motivos, rep("inset 0 0 0 9999px #FDE8E4", length(motivos))))
        dt <- DT::formatStyle(dt, "Atenção", color = "#B42318", fontWeight = "600")
      }
      dt
    }

    # Redesenha quando muda a prévia ou a estrutura da lista; edições de célula usam o proxy.
    output$marco_tabela <- DT::renderDT({
      if (pendente()) {
        tab <- previa()$tabela
        return(desenhar_tabela(tab, names(tab)[1], editavel = FALSE))
      }
      versao_rv()
      m <- shiny::isolate(marco_rv())
      shiny::req(m)
      desenhar_tabela(m, shiny::isolate(id_atual()), editavel = TRUE)
    })
    proxy <- DT::dataTableProxy("marco_tabela")

    shiny::observeEvent(input$marco_tabela_cell_edit, {
      antes <- any(nzchar(problemas_por_linha(marco_rv(), id_atual())))
      m <- DT::editData(marco_rv(), input$marco_tabela_cell_edit, rownames = FALSE)
      marco_rv(m)
      sorteio_rv(NULL)
      recem_confirmado_rv(FALSE)
      depois <- any(nzchar(problemas_por_linha(m, id_atual())))
      # Se a coluna de atenção aparece ou some, a tabela precisa ser redesenhada.
      if (!identical(antes, depois)) versao_rv(versao_rv() + 1)
      else DT::replaceData(proxy, com_atencao(m, id_atual()), resetPaging = FALSE, rownames = FALSE)
    })

    # Barra acima da tabela: linhas e colunas. "+ Coluna" funciona até com a lista vazia,
    # porque níveis e conglomerados também servem para montar a lista.
    output$lista_barra <- shiny::renderUI({
      versao_rv()
      m <- marco_rv()
      if (pendente()) return(NULL)
      tem_lista <- !is.null(m)
      shiny::div(class = "d-flex gap-2 flex-wrap mb-2",
        if (tem_lista) shiny::actionButton(ns("add_linha"), "Linha", icon = shiny::icon("plus"), class = "btn-sm btn-outline-primary"),
        shiny::actionButton(ns("abrir_coluna"), "Coluna", icon = shiny::icon("plus"), class = "btn-sm btn-primary"),
        if (tem_lista && ncol(m) > 1) shiny::actionButton(ns("abrir_remover_coluna"), "Coluna", icon = shiny::icon("minus"), class = "btn-sm btn-outline-secondary"),
        if (tem_lista) shiny::actionButton(ns("del_linhas"), "Remover linhas selecionadas", icon = shiny::icon("trash"), class = "btn-sm btn-outline-secondary"),
        if (!tem_lista) shiny::span(class = "small text-muted align-self-center",
          "Sem nomes para as unidades? Crie uma coluna de conglomerado ou de níveis e a lista se monta sozinha.")
      )
    })

    # Abaixo da tabela: dica de edição e, só se houver mais de uma coluna, o identificador.
    output$lista_ferramentas <- shiny::renderUI({
      versao_rv()
      m <- marco_rv()
      if (is.null(m) || pendente()) return(NULL)
      shiny::tagList(
        shiny::p(class = "small text-muted mt-1", "Dê dois cliques numa célula para corrigir. Clique numa linha para selecioná-la."),
        if (ncol(m) > 1) shiny::div(style = "max-width: 320px;",
          shiny::selectInput(ns("coluna_id"), "Qual coluna identifica cada unidade?", choices = names(m),
            selected = shiny::isolate(id_atual())))
      )
    })

    # --- Janela de nova coluna ----------------------------------------------

    # Nome sugerido para cada papel; o pesquisador pode trocar (porto, embarcacao...).
    nomes_padrao <- c(estrato = "estrato", ordem = "ordem", conglomerado = "conglomerado", outra = "")

    shiny::observeEvent(input$abrir_coluna, {
      N <- if (is.null(marco_rv())) 0 else nrow(marco_rv())
      shiny::showModal(shiny::modalDialog(
        title = "Nova coluna", size = "l", easyClose = TRUE,
        shiny::radioButtons(ns("nc_papel"), "Para que serve esta coluna?", inline = TRUE, width = "100%",
          choices = c("Estrato (estratificada)" = "estrato", "Ordem (sistemática)" = "ordem",
                      "Conglomerado (conglomerados)" = "conglomerado", "Outra" = "outra")),
        shiny::textInput(ns("nc_nome"), "Nome da coluna:", value = "estrato", placeholder = "porto, regiao, embarcacao..."),
        # Estrato e outra: níveis (destaque) ou valor livre.
        shiny::conditionalPanel(sprintf("['estrato', 'outra'].includes(input['%s'])", ns("nc_papel")),
          shiny::radioButtons(ns("nc_modo"), NULL, width = "100%",
            choices = c("Com níveis: liste os níveis e quantas vezes cada um se repete; a CatalyseR preenche" = "niveis",
                        "Valor livre: a coluna nasce vazia, para preencher à mão" = "livre")),
          shiny::conditionalPanel(sprintf("input['%s'] === 'niveis'", ns("nc_modo")),
            shiny::numericInput(ns("nc_n_niveis"), "Quantos níveis?", value = 2, min = 1, max = 30, step = 1, width = "160px"),
            shiny::uiOutput(ns("nc_niveis_ui"))
          )
        ),
        # Ordem: sequência automática ou valor livre.
        shiny::conditionalPanel(sprintf("input['%s'] === 'ordem'", ns("nc_papel")),
          shiny::radioButtons(ns("nc_modo_ordem"), NULL, width = "100%",
            choices = c("Numerar em sequência, de 1 a N, na ordem atual da lista" = "sequencia",
                        "Valor livre: a coluna nasce vazia, para preencher à mão (posição no transecto, por exemplo)" = "livre"))
        ),
        # Conglomerado: quantos grupos e quantas unidades em cada.
        shiny::conditionalPanel(sprintf("input['%s'] === 'conglomerado'", ns("nc_papel")),
          bslib::layout_columns(col_widths = c(4, 4, 4),
            shiny::numericInput(ns("nc_n_grupos"), "Quantos conglomerados?", value = 5, min = 1, step = 1),
            shiny::numericInput(ns("nc_por_grupo"), "Unidades em cada um:", value = max(1, if (N) round(N / 5) else 20), min = 1, step = 1),
            shiny::textInput(ns("nc_prefixo"), "Rótulo:", value = "E", placeholder = "E para E1, E2...")
          ),
          shiny::helpText("Exemplo: 5 embarcações com 20 peixes cada viram os rótulos E1 a E5, cada um repetido 20 vezes.")
        ),
        shiny::uiOutput(ns("nc_conferencia")),
        shiny::tags$b("Prévia"),
        shiny::uiOutput(ns("nc_previa_aviso")),
        shiny::tableOutput(ns("nc_previa")),
        footer = shiny::tagList(
          shiny::modalButton("Cancelar"),
          shiny::actionButton(ns("nc_criar"), "Criar coluna", icon = shiny::icon("check"), class = "btn-primary")
        )
      ))
    })

    # Ao trocar o papel, o nome sugerido acompanha (a menos que o pesquisador já tenha escrito outro).
    shiny::observeEvent(input$nc_papel, {
      atual <- input$nc_nome %||% ""
      if (atual %in% c(nomes_padrao, "")) shiny::updateTextInput(session, "nc_nome", value = nomes_padrao[[input$nc_papel]])
    }, ignoreInit = TRUE)

    # Uma linha por nível: nome e quantas vezes se repete (sugestão: dividir N por igual).
    output$nc_niveis_ui <- shiny::renderUI({
      k <- suppressWarnings(as.integer(input$nc_n_niveis))
      shiny::req(!is.na(k), k >= 1)
      N <- if (is.null(marco_rv())) 0 else nrow(marco_rv())
      sugestao <- if (N) alocar_estratos(stats::setNames(rep(1, k), seq_len(k)), N) else rep(2L, k)
      shiny::tags$table(class = "table table-sm align-middle mb-2", style = "max-width: 520px;",
        shiny::tags$thead(shiny::tags$tr(shiny::tags$th("Nível"), shiny::tags$th("Repetições"))),
        shiny::tags$tbody(lapply(seq_len(k), function(i) shiny::tags$tr(
          shiny::tags$td(shiny::textInput(ns(paste0("nc_nivel_", i)), NULL,
            value = shiny::isolate(input[[paste0("nc_nivel_", i)]]) %||% "", placeholder = paste("Nível", i), width = "100%")),
          shiny::tags$td(shiny::numericInput(ns(paste0("nc_rep_", i)), NULL,
            value = shiny::isolate(input[[paste0("nc_rep_", i)]]) %||% sugestao[[i]], min = 0, step = 1, width = "120px"))
        )))
      )
    })

    # O modo efetivo da nova coluna, a partir do papel escolhido.
    nc_modo_efetivo <- shiny::reactive({
      switch(input$nc_papel %||% "estrato",
        ordem = input$nc_modo_ordem %||% "sequencia",
        conglomerado = "blocos",
        input$nc_modo %||% "niveis")
    })

    # Os valores que a nova coluna terá, antes de qualquer ajuste ao N.
    nc_valores <- shiny::reactive({
      modo <- nc_modo_efetivo()
      N <- if (is.null(marco_rv())) 0 else nrow(marco_rv())
      if (identical(modo, "niveis")) {
        k <- suppressWarnings(as.integer(input$nc_n_niveis))
        shiny::req(!is.na(k), k >= 1)
        niveis <- vapply(seq_len(k), function(i) trimws(input[[paste0("nc_nivel_", i)]] %||% ""), character(1))
        repeticoes <- vapply(seq_len(k), function(i) {
          r <- suppressWarnings(as.integer(input[[paste0("nc_rep_", i)]])); if (is.na(r) || r < 0) 0L else r
        }, integer(1))
        # Níveis sem nome ainda não entram.
        usar <- nzchar(niveis) & repeticoes > 0
        return(valores_nova_coluna("niveis", niveis = niveis[usar], repeticoes = repeticoes[usar]))
      }
      if (identical(modo, "blocos")) {
        g <- suppressWarnings(as.integer(input$nc_n_grupos)); u <- suppressWarnings(as.integer(input$nc_por_grupo))
        shiny::req(!is.na(g), g >= 1, !is.na(u), u >= 1)
        return(valores_nova_coluna("blocos", n_grupos = g, por_grupo = u, prefixo = trimws(input$nc_prefixo %||% "")))
      }
      valores_nova_coluna(modo, N = N)
    })

    # Confere a soma das repetições com o N e oferece as duas saídas.
    output$nc_conferencia <- shiny::renderUI({
      valores <- nc_valores()
      m <- marco_rv()
      total <- length(valores)
      if (is.null(m)) {
        if (nc_modo_efetivo() %in% c("livre", "sequencia") || !total) {
          return(shiny::div(class = "alert alert-warning py-2", "A lista ainda está vazia. Para montá-la por aqui, use níveis ou conglomerado; ou escreva a lista à esquerda."))
        }
        return(shiny::div(class = "alert alert-info py-2",
          sprintf("A lista ainda está vazia: serão criadas %d unidades numeradas, já com esta coluna preenchida.", total)))
      }
      N <- nrow(m)
      if (total == N) {
        return(shiny::div(class = "alert alert-success py-2", sprintf("A soma das repetições (%d) bate com as %d unidades da lista.", total, N)))
      }
      if (total > N) {
        falta <- total - N
        escolhas <- stats::setNames(c("criar", "cortar"), c(
          if (falta == 1) "Ajustar a lista: criar a linha que falta" else sprintf("Ajustar a lista: criar as %d linhas que faltam", falta),
          sprintf("Cortar o excedente: usar só as %d primeiras repetições", N)))
        texto <- sprintf("A soma das repetições (%d) passa das %d unidades da lista.", total, N)
      } else {
        sobra <- N - total
        escolhas <- stats::setNames(c("remover", "branco"), c(
          if (sobra == 1) "Ajustar a lista: remover a última linha" else sprintf("Ajustar a lista: remover as %d últimas linhas", sobra),
          if (sobra == 1) "Manter a lista: deixar a última linha em branco nesta coluna" else sprintf("Manter a lista: deixar as %d últimas linhas em branco nesta coluna", sobra)))
        texto <- sprintf("A soma das repetições (%d) não chega às %d unidades da lista.", total, N)
      }
      shiny::div(class = "alert alert-warning py-2",
        shiny::tags$b(texto), " Escolha como resolver:",
        shiny::radioButtons(ns("nc_ajuste"), NULL, choices = escolhas, selected = character(0), width = "100%"))
    })

    # Como a lista ficaria: identificador + nova coluna, nas primeiras linhas.
    nc_resultado <- shiny::reactive({
      nome <- gerar_nome_reduzido(input$nc_nome %||% "")
      aplicar_nova_coluna(marco_rv(), nome, nc_valores(), input$nc_ajuste, id = id_atual())
    })

    # Enquanto a soma não bate e nenhuma saída foi escolhida, a prévia espera a escolha.
    nc_aguardando_ajuste <- shiny::reactive({
      m <- marco_rv()
      !is.null(m) && !nc_modo_efetivo() %in% c("livre", "sequencia") &&
        length(nc_valores()) != nrow(m) && is.null(input$nc_ajuste)
    })
    output$nc_previa_aviso <- shiny::renderUI({
      if (nc_aguardando_ajuste()) shiny::p(class = "small text-muted", "Escolha uma das saídas acima para ver como a lista vai ficar.")
    })

    output$nc_previa <- shiny::renderTable({
      shiny::req(!nc_aguardando_ajuste())
      tab <- nc_resultado()
      nome <- gerar_nome_reduzido(input$nc_nome %||% "")
      id <- id_atual() %||% names(tab)[1]
      mostrar <- tab[, unique(c(id, nome)), drop = FALSE]
      rbind(utils::head(mostrar, 8),
            if (nrow(mostrar) > 8) stats::setNames(as.data.frame(as.list(rep("...", ncol(mostrar)))), names(mostrar)))
    }, striped = TRUE, bordered = TRUE, na = "")

    shiny::observeEvent(input$nc_criar, {
      m <- marco_rv()
      nome_bruto <- trimws(input$nc_nome %||% "")
      nome <- gerar_nome_reduzido(nome_bruto)
      valores <- nc_valores()
      # Cada impedimento vira uma mensagem clara, sem fechar a janela.
      problema <- dplyr::case_when(
        !nzchar(nome_bruto) ~ "Dê um nome à coluna.",
        !is.null(m) && nome %in% names(m) ~ sprintf("Já existe a coluna '%s'. Escolha outro nome.", nome),
        is.null(m) && (nc_modo_efetivo() %in% c("livre", "sequencia") || !length(valores)) ~ "A lista está vazia: use níveis ou conglomerado para montá-la por aqui.",
        !is.null(m) && length(valores) != nrow(m) && !nc_modo_efetivo() %in% c("livre", "sequencia") && is.null(input$nc_ajuste) ~ "A soma não bate com o N: escolha uma das duas saídas.",
        identical(nc_modo_efetivo(), "niveis") && !length(valores) ~ "Escreva ao menos um nível com repetições.",
        TRUE ~ ""
      )
      if (nzchar(problema)) {
        shiny::showNotification(problema, type = "warning")
        return()
      }
      nova <- aplicar_nova_coluna(m, nome, valores, input$nc_ajuste, id = id_atual())
      estava_vazia <- is.null(m)
      definir_marco(nova)
      # A nova coluna já aparece escolhida no seletor do método correspondente.
      preferencia_rv(list(papel = input$nc_papel, nome = nome))
      if (estava_vazia) {
        fonte_confirmada_rv(previa()$assinatura %||% "")
        recem_confirmado_rv(TRUE)
      }
      shiny::removeModal()
      shiny::showNotification(sprintf("Coluna '%s' criada.", nome), type = "message")
    })

    # Remover coluna: uma janela curta com a escolha.
    shiny::observeEvent(input$abrir_remover_coluna, {
      m <- marco_rv()
      shiny::req(m, ncol(m) > 1)
      shiny::showModal(shiny::modalDialog(
        title = "Remover coluna", easyClose = TRUE,
        shiny::selectInput(ns("remover_coluna"), "Qual coluna remover?", choices = setdiff(names(m), id_atual())),
        footer = shiny::tagList(shiny::modalButton("Cancelar"),
          shiny::actionButton(ns("del_coluna"), "Remover", icon = shiny::icon("trash"), class = "btn-danger"))
      ))
    })

    # Mantém os seletores de coluna do Sorteio em dia com a lista.
    shiny::observe({
      versao_rv()
      m <- marco_rv()
      colunas <- if (is.null(m)) character() else names(m)
      id <- id_atual()
      outras <- setdiff(colunas, id)
      # Para estrato e conglomerado, sugerimos a primeira coluna de texto com valores repetidos.
      candidatas <- Filter(function(col) !is.numeric(m[[col]]) && anyDuplicated(m[[col]]) > 0, outras)
      # Uma coluna recém-criada com papel definido passa na frente das escolhas anteriores.
      pref <- shiny::isolate(preferencia_rv())
      preferida <- function(papel, validas) if (identical(pref$papel, papel) && isTRUE(pref$nome %in% validas)) pref$nome else NULL
      shiny::updateSelectInput(session, "estrato", choices = outras,
        selected = preferida("estrato", outras) %||% shiny::isolate(if (isTRUE(input$estrato %in% outras)) input$estrato else c(candidatas, outras)[1]))
      shiny::updateSelectInput(session, "conglomerado", choices = outras,
        selected = preferida("conglomerado", outras) %||% shiny::isolate(if (isTRUE(input$conglomerado %in% outras)) input$conglomerado else c(candidatas, outras)[1]))
      shiny::updateSelectInput(session, "ordem", choices = colunas,
        selected = preferida("ordem", colunas) %||% shiny::isolate(if (isTRUE(input$ordem %in% colunas)) input$ordem else colunas[1]))
      if (!is.null(pref)) preferencia_rv(NULL)
    })

    # Trocar a coluna de identificador muda o que conta como unidade: o sorteio anterior deixa de valer.
    shiny::observeEvent(input$coluna_id, {
      sorteio_rv(NULL)
      versao_rv(versao_rv() + 1)
    }, ignoreInit = TRUE)

    # Se o método pede uma coluna que a lista ainda não tem, dizemos onde criá-la.
    # (A sistemática pode usar o próprio identificador como ordem, então não entra aqui.)
    output$falta_coluna <- shiny::renderUI({
      m <- marco_rv()
      metodo <- input$metodo %||% "aas"
      shiny::req(m, metodo %in% c("estratificada", "conglomerados"), ncol(m) == 1)
      coluna <- if (identical(metodo, "estratificada")) "de estrato" else "de conglomerado"
      shiny::div(class = "alert alert-warning py-2 small",
        sprintf("Sua lista ainda não tem uma coluna %s. Crie-a em De onde sortear, no botão + Coluna.", coluna))
    })

    shiny::observeEvent(input$del_coluna, {
      m <- marco_rv()
      shiny::req(m, input$remover_coluna %in% names(m), ncol(m) > 1)
      m[[input$remover_coluna]] <- NULL
      definir_marco(m)
      shiny::removeModal()
    })

    shiny::observeEvent(input$add_linha, {
      m <- marco_rv()
      shiny::req(m)
      nova <- m[1, , drop = FALSE]
      nova[1, ] <- NA
      definir_marco(rbind(m, nova))
    })

    shiny::observeEvent(input$del_linhas, {
      m <- marco_rv()
      linhas <- input$marco_tabela_rows_selected
      shiny::req(m)
      if (!length(linhas)) {
        shiny::showNotification("Clique nas linhas que quer remover.", type = "warning")
        return()
      }
      definir_marco(m[-linhas, , drop = FALSE])
    })

    # --- Sorteio ------------------------------------------------------------

    output$metodo_dica <- shiny::renderUI({
      texto <- switch(input$metodo %||% "aas",
        aas = "Usa só o identificador e o total N. Cada unidade tem a mesma chance.",
        estratificada = "Usa uma coluna de estrato (porto, região). Cada estrato entra na amostra conforme o seu tamanho.",
        sistematica = "Usa uma coluna de ordem (posição no transecto, sequência de chegada). Sorteia o início e segue de k em k.",
        conglomerados = "Usa uma coluna de conglomerado (a embarcação ou o desembarque de cada peixe). Sorteia grupos inteiros e, se preciso, unidades dentro deles."
      )
      shiny::p(class = "small text-muted", texto)
    })

    output$fpc_dica <- shiny::renderUI({
      m <- marco_rv()
      n <- suppressWarnings(as.integer(input$n))
      shiny::req(m, !is.na(n), n >= 1)
      if (!isTRUE(input$corrigir_finita)) {
        return(shiny::helpText("Útil quando o n veio de um cálculo que supõe população infinita (por exemplo, em Quantos coletar) e o marco é pequeno."))
      }
      shiny::helpText(sprintf("Com N = %d, o n = %d passa a %d unidades.", nrow(m), n, corrigir_n_finito(n, nrow(m))))
    })

    output$k_dica <- shiny::renderUI({
      m <- marco_rv()
      k <- suppressWarnings(as.integer(input$k))
      shiny::req(m, !is.na(k), k >= 1)
      N <- nrow(m)
      shiny::helpText(sprintf("Com N = %d e k = %d, saem %d ou %d unidades, conforme o início sorteado. Para cerca de n unidades, use k perto de N/n.",
        N, k, floor(N / k), ceiling(N / k)))
    })

    # Quantos conglomerados existem e de que tamanho, antes de sortear.
    output$conglomerado_dica <- shiny::renderUI({
      m <- marco_rv()
      shiny::req(m, input$conglomerado %in% names(m))
      tamanhos <- table(as.character(m[[input$conglomerado]]))
      shiny::helpText(sprintf("%d conglomerados no marco, com %d a %d unidades cada.",
        length(tamanhos), min(tamanhos), max(tamanhos)))
    })

    # Prévia da alocação proporcional, antes de sortear.
    output$previa_alocacao <- shiny::renderUI({
      m <- marco_rv()
      if (is.null(m)) {
        return(shiny::div(class = "alert alert-light border", "Defina primeiro a lista em De onde sortear."))
      }
      if (!identical(input$metodo, "estratificada") || !is.null(sorteio_rv())) return(NULL)
      shiny::req(input$estrato %in% names(m))
      n <- suppressWarnings(as.integer(input$n))
      shiny::req(!is.na(n), n >= 1)
      grupos <- as.character(m[[input$estrato]])
      if (any(is.na(grupos) | trimws(grupos) == "")) {
        return(shiny::div(class = "alert alert-warning", "Há unidades sem estrato. Preencha a coluna no passo 1."))
      }
      nomes <- sort(unique(grupos), method = "radix")
      tamanhos <- vapply(nomes, function(h) sum(grupos == h), integer(1))
      shiny::tagList(
        shiny::p(shiny::tags$b("Alocação proporcional prevista")),
        shiny::tableOutput(ns("previa_tabela"))
      )
    })

    output$previa_tabela <- shiny::renderTable({
      m <- marco_rv()
      n <- suppressWarnings(as.integer(input$n))
      shiny::req(m, input$estrato %in% names(m), !is.na(n), n >= 1)
      grupos <- as.character(m[[input$estrato]])
      nomes <- sort(unique(grupos), method = "radix")
      tamanhos <- vapply(nomes, function(h) sum(grupos == h), integer(1))
      data.frame(Estrato = nomes, `Unidades (N_h)` = tamanhos,
        `Sorteadas (n_h)` = alocar_estratos(tamanhos, n), check.names = FALSE)
    }, striped = TRUE, bordered = TRUE, digits = 0)

    shiny::observeEvent(input$sortear, {
      m <- marco_rv()
      if (is.null(m)) {
        shiny::showNotification("Defina primeiro a lista em De onde sortear.", type = "warning")
        return()
      }
      resultado <- tryCatch(
        sortear_marco(
          m, metodo = input$metodo, semente = input$semente,
          id = id_atual(),
          n = input$n, corrigir_finita = isTRUE(input$corrigir_finita),
          estrato = input$estrato, ordem = input$ordem, k = input$k,
          conglomerado = input$conglomerado, n_conglomerados = input$n_conglomerados,
          estagios = input$estagios %||% "unico", m = input$m
        ),
        error = function(e) { shiny::showNotification(conditionMessage(e), type = "error", duration = 8); NULL }
      )
      if (is.null(resultado)) return()
      resultado$marco <- m
      resultado$codigo <- codigo_sorteio(resultado$registro)
      sorteio_rv(resultado)
    })

    output$resultado_resumo <- shiny::renderUI({
      s <- sorteio_rv()
      shiny::req(s)
      shiny::tagList(
        shiny::div(class = "alert alert-success", resumir_sorteio(s)),
        # Em conglomerados, uma linha lembra que as unidades não são independentes entre si.
        if (isTRUE(s$registro$aninhado)) shiny::div(class = "alert alert-info py-2 small",
          shiny::icon("sitemap"), sprintf(" As unidades ficam aninhadas em %s: na análise, %s entra como efeito aleatório (modelo misto). A ficha registra esse aninhamento.",
            s$registro$conglomerado, s$registro$conglomerado)),
        lapply(s$avisos, function(a) shiny::div(class = "alert alert-warning py-2 small", a)),
        if (!is.null(s$alocacao)) shiny::tableOutput(ns("alocacao_tabela"))
      )
    })

    output$alocacao_tabela <- shiny::renderTable({
      s <- sorteio_rv()
      shiny::req(s, s$alocacao)
      if (identical(s$registro$metodo, "conglomerados")) {
        stats::setNames(s$alocacao, c("Conglomerado", "Unidades no conglomerado", "Sorteadas"))
      } else {
        stats::setNames(s$alocacao, c("Estrato", "Unidades (N_h)", "Sorteadas (n_h)"))
      }
    }, striped = TRUE, bordered = TRUE, digits = 0)

    output$sorteados_tabela <- DT::renderDT({
      s <- sorteio_rv()
      shiny::req(s)
      DT::datatable(s$sorteados, rownames = FALSE,
        options = list(pageLength = 10, scrollX = TRUE, dom = "tip", language = list(info = "_START_ a _END_ de _TOTAL_")),
        class = "stripe hover compact")
    })

    output$resultado_acoes <- shiny::renderUI({
      s <- sorteio_rv()
      shiny::req(s)
      shiny::tagList(
        shiny::div(class = "d-flex gap-2 mt-2",
          shiny::downloadButton(ns("baixar_sorteio"), "Sorteados (.xlsx)", class = "btn-outline-primary"),
          shiny::actionButton(ns("ir_coleta"), "Declarar o que medir", icon = shiny::icon("arrow-right"), class = "btn-primary")
        ),
        shiny::tags$details(class = "mt-3",
          shiny::tags$summary(class = "small fw-bold", "Código R para refazer este sorteio"),
          shiny::tags$pre(class = "small", paste(s$codigo, collapse = "\n"))
        )
      )
    })

    shiny::observeEvent(input$ir_coleta, {
      bslib::nav_select("passos", "coleta")
    })

    # O arquivo guarda os sorteados, o marco e o registro, para refazer o sorteio depois.
    output$baixar_sorteio <- shiny::downloadHandler(
      filename = function() paste0("sorteio_", format(Sys.Date(), "%Y-%m-%d"), ".xlsx"),
      content = function(file) {
        s <- sorteio_rv()
        writexl::write_xlsx(list(
          sorteados = s$sorteados,
          marco_amostral = s$marco,
          registro_sorteio = tabela_registro_sorteio(s$registro)
        ), file)
      }
    )

    # --- Passo 3: Planejamento de Variáveis ---------------------------------

    # A ficha de coleta recebe as unidades sorteadas como colunas estruturais.
    mod_planejamento_variaveis_server("variaveis", modo = "sorteio",
      estrutura_rv = sorteio_rv, ficha_destino_rv = ficha_destino_rv)

    # Cada sorteio concluído preenche o campo sorteio da ficha de planejamento
    # (marco, identificador, método, semente, parâmetros e unidades escolhidas).
    shiny::observeEvent(sorteio_rv(), {
      if (!is.function(ficha_destino_rv)) return()
      ficha_destino_rv(ficha_mesclar(ficha_destino_rv(), ficha_parte_sorteio(sorteio_rv())))
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    # Devolve o sorteio para os delineamentos reutilizarem.
    sorteio_rv
  })
}
