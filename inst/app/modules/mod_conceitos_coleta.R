# Porta de entrada do menu Planejando sua Pesquisa.
# Textos curtos, exemplos do domínio e esquemas antes de qualquer calculadora.

conceitos_coleta <- function() {
  list(
    pergunta = list(
      titulo = "Comece pela pergunta", chamada = "O que você precisa descobrir?",
      texto = "A pergunta define o que será medido e qual resposta precisa ser precisa. Comparar médias de rações pede um planejamento diferente de estimar a proporção de peixes maturos.",
      exemplo = "Você quer saber se duas rações mudam o ganho médio de massa da tilápia? Isso é comparação entre grupos. Quer estimar a proporção de peixes maturos num desembarque? Isso é estimação.",
      cuidado = "Escreva o efeito ou a margem de erro que teria importância prática antes de abrir uma calculadora de n.",
      esquema = c("Pergunta biológica", "Resposta a medir", "Cálculo de n"),
      proximo = "Para comparar médias, abra “Quanto amostrar”, grupo “Para comparar”, aba “Comparação de médias”. Para estimar uma média ou uma proporção, o mesmo item de menu, grupo “Para estimar”."
    ),
    unidade = list(
      titulo = "Unidade experimental e pseudorrepetição", chamada = "É o peixe, o tanque ou o pool?",
      texto = "A unidade experimental é a menor unidade que recebe um tratamento de forma independente. A unidade observacional é aquilo que você mede. Elas podem ser diferentes.",
      exemplo = "Se a ração vai para o tanque inteiro, cinco peixes pesados dentro dele são subamostras de um tanque. Para comparar rações, o n de repetições independentes é o número de tanques, não o de peixes. Registre o identificador de cada tanque e peixe.",
      cuidado = "Se tecidos de vários animais forem reunidos antes da medição, cada pool gera uma observação composta: os peixes que o formam não viram repetições independentes. Registre quem entrou em cada pool e distinga animais coletados de observações analisadas.",
      esquema = c("Tratamento no tanque", "Peixes medidos", "n = tanques independentes"),
      proximo = "Depois de identificar a unidade independente, escolha o planejamento observacional ou experimental. O cálculo de n fica ao final deste menu."
    ),
    tamanho = list(
      titulo = "Escolha n com uma premissa visível", chamada = "Quantas unidades independentes bastam?",
      texto = "O n depende da pergunta, da diferença que vale detectar, da variação esperada e da precisão desejada. Um número isolado, sem essas premissas, não justifica uma coleta.",
      exemplo = "Para comparar o crescimento com duas rações, você informa uma diferença mínima relevante, o desvio-padrão esperado e o poder desejado. O resultado é n por grupo — na unidade experimental escolhida.",
      cuidado = "Busque o desvio-padrão em estudo piloto ou literatura comparável. Explique de onde veio a escolha na ficha do delineamento.",
      esquema = c("Efeito + variação", "Poder e confiança", "n por grupo"),
      proximo = "As calculadoras do “Quanto amostrar” já estão disponíveis: estimar média e proporção no grupo “Para estimar”; comparar médias, proporções e relação entre variáveis no grupo “Para comparar”."
    ),
    amostragem = list(
      titulo = "Decida como as unidades entram", chamada = "Quem terá chance de ser escolhido?",
      texto = "Amostragem responde como selecionar unidades da população. Um cálculo de n pode estar correto e ainda assim produzir uma amostra enviesada se o sorteio não corresponder à pergunta.",
      exemplo = "Se desembarques vêm de portos de tamanhos diferentes, a estratificação ajuda a representar cada porto. Se todos os peixes elegíveis estão numa lista confiável, o sorteio simples pode bastar.",
      cuidado = "Defina a população elegível, a lista de unidades e o motivo de incluir cada estrato antes de sortear.",
      esquema = c("População definida", "AAS ou estratos", "Unidades selecionadas"),
      proximo = "As opções AAS, estratificada proporcional e sistemática fazem o sorteio depois que você escolhe n."
    ),
    delineamento = list(
      titulo = "Organize os tratamentos antes de medir", chamada = "O desenho do experimento muda a análise.",
      texto = "O delineamento define como os tratamentos são distribuídos e quais diferenças devem ser controladas. O croqui torna essa decisão visível antes da coleta.",
      exemplo = "Num DIC, as unidades recebem tratamentos por sorteio. Num DBC, blocos ajudam a lidar com uma diferença conhecida entre locais ou lotes. Parcelas subdivididas exigem atenção a duas escalas de unidade experimental.",
      cuidado = "A CatalyseR já desenha DIC, DBC, quadrado latino e parcelas subdivididas, mas a análise correspondente aos três últimos ainda não está completa no estúdio.",
      esquema = c("Unidade + fatores", "Sorteio e croqui", "Análise compatível"),
      proximo = "Abra “Delineamento experimental” para criar o croqui e, na aba “Variáveis do experimento”, estruture a coleta."
    ),
    responsabilidade = list(
      titulo = "Planeje uma coleta responsável", chamada = "O menor n que responde à pergunta.",
      texto = "Nos estudos com animais, os 3Rs lembram de substituir quando possível, reduzir sem perder validade e refinar procedimentos para diminuir sofrimento.",
      exemplo = "Se a unidade é o tanque, medir mais peixes no mesmo tanque pode melhorar a descrição daquele tanque, mas não substitui tanques independentes. Uma coleta bem planejada evita tanto desperdício quanto conclusões frágeis.",
      cuidado = "Revise perdas previstas, viabilidade, origem das premissas e a unidade que realmente entrará na análise.",
      esquema = c("Pergunta útil", "n válido", "Coleta responsável"),
      proximo = "Antes de coletar, confira se a planilha guarda tratamento, unidade experimental e identificação de cada observação."
    ),
    tipo_estudo = list(
      titulo = "Escolha o tipo de estudo", chamada = "Você vai observar ou manipular?",
      texto = "Antes de tudo, decida se vai apenas observar a natureza como ela é, ou impor tratamentos e ver o que muda. Essa escolha define o delineamento e a análise que virão depois.",
      exemplo = "Comparar a composição química das bexigas de cinco espécies é um estudo observacional, porque você seleciona as espécies, não as cria. Testar três rações em tanques é experimental, porque você atribui a ração a cada tanque.",
      cuidado = "Decida observar ou manipular antes de coletar, porque isso muda o delineamento e o teste.",
      esquema = c("Pergunta", "Observar ou manipular", "Delineamento"),
      proximo = "Os dois caminhos estão em Delineamentos observacionais e Delineamentos experimentais."
    ),
    padronizar = list(
      titulo = "Padronize antes de comparar", chamada = "O que mais poderia explicar a diferença?",
      texto = "Para uma comparação justa entre grupos, controle o que não interessa. Padronizar tamanho, idade ou classe comercial faz a diferença refletir o fator que você estuda, e não uma variação de porte.",
      exemplo = "Ao comparar bexigas entre espécies, use adultos numa faixa estreita de comprimento e peso, por exemplo mais ou menos dez por cento no comprimento. Assim o que difere é a espécie, não o tamanho do peixe.",
      cuidado = "Defina os critérios de padronização antes de ir a campo.",
      esquema = c("Fator de interesse", "Critérios fixos", "Comparação justa"),
      proximo = "Registre esses critérios no delineamento; eles viajam para a ficha e para a metodologia."
    ),
    blocos = list(
      titulo = "Agrupe unidades parecidas em blocos", chamada = "O ambiente não é uniforme?",
      texto = "Quando as unidades diferem por algo que você não quer medir, o sorteio livre pode concentrar essa diferença num só tratamento. Formar blocos homogêneos e sortear os tratamentos dentro de cada bloco tira essa variação do caminho da comparação.",
      exemplo = "Oito tanques em duas fileiras com sombra diferente? Cada fileira vira um bloco e as rações são sorteadas dentro dela. Alevinos de dois lotes com peso inicial distinto? Cada lote vira um bloco, e os tratamentos são comparados em condições parecidas.",
      cuidado = "Bloco é tanque, viveiro ou lote, nunca o peixe individual. Antes da coleta, liste o que pode variar entre unidades (posição, luz, manejo, peso inicial) e escolha um fator para blocar.",
      esquema = c("O que varia", "Blocos homogêneos", "Sorteio dentro do bloco"),
      proximo = "Para montar o croqui em blocos, abra “DBC - Blocos Casualizados”, no grupo “Delineamentos experimentais” deste mesmo menu. Na aba “Croqui”, informe os níveis do fator e o número de blocos: cada linha do croqui é um bloco, e tratamento e bloco seguem identificáveis na planilha."
    )
  )
}

painel_conceito_coleta <- function(tema) {
  passos <- lapply(seq_along(tema$esquema), function(i) {
    if (i == 1L) return(shiny::span(tema$esquema[[i]]))
    shiny::tagList(shiny::icon("arrow-right"), shiny::span(tema$esquema[[i]]))
  })
  shiny::div(class = "coleta-conteudo",
    shiny::div(class = "coleta-tema", "Conceito de planejamento"),
    shiny::h2(tema$titulo),
    shiny::p(class = "coleta-chamada", tema$chamada),
    shiny::p(tema$texto),
    shiny::div(class = "coleta-esquema", passos),
    # Na coluna de 60%, exemplo e cuidado lado a lado só em telas bem largas.
    bslib::layout_columns(col_widths = bslib::breakpoints(sm = c(12, 12), xxl = c(6, 6)),
      bslib::card(bslib::card_header("Exemplo em pesca e aquicultura"), bslib::card_body(tema$exemplo)),
      bslib::card(bslib::card_header("Antes de seguir"), bslib::card_body(tema$cuidado))
    ),
    shiny::div(class = "coleta-proximo", shiny::tags$b("Na CatalyseR: "), tema$proximo)
  )
}

# ---- Fluxograma do caminho da pesquisa ---------------------------------------
# Mapa estático em HTML e CSS (sem imagem): oito etapas empilhadas, a volta do
# ciclo pela esquerda e o fio da ficha e do Projeto R pela margem. Não é clicável.

etapas_caminho_pesquisa <- function() {
  list(
    list(titulo = "Pergunta e lacuna", icone = "circle-question", menu = NULL,
         texto = "O que ainda não se sabe e vale descobrir, apoiado na literatura."),
    list(titulo = "Hipótese", icone = "lightbulb", menu = NULL,
         texto = "Uma resposta provisória que os dados poderão sustentar ou refutar."),
    list(titulo = "Planejar a pesquisa", icone = "compass-drafting", menu = "Planejando sua Pesquisa",
         texto = "Delineamento, unidade independente, n e sorteio, antes de medir."),
    list(titulo = "Preparar os dados", icone = "table", menu = "Preparar Dados",
         texto = "Importar, arrumar e registrar cada ajuste numa trilha reprodutível."),
    list(titulo = "Explorar e visualizar", icone = "chart-column", menu = "Explorando os Dados · Visualização",
         texto = "Resumos e gráficos que antecipam o teste e revelam problemas."),
    list(titulo = "Analisar", icone = "calculator", menu = "Menus de testes e modelos",
         texto = "O teste ou modelo que responde à pergunta, com os pressupostos conferidos."),
    list(titulo = "Comunicar", icone = "file-lines", menu = "Comunicação de Resultados",
         texto = "Relatório e Projeto R que outra pessoa consegue ler e reproduzir."),
    list(titulo = "Decisões", icone = "scale-balanced", menu = NULL,
         texto = "O que o resultado muda no manejo, na produção ou na próxima pesquisa.")
  )
}

fluxograma_caminho_pesquisa <- function(raiz) {
  etapas <- etapas_caminho_pesquisa()
  # A cor caminha do azul-marinho ao verde-água; a última etapa fica em âmbar.
  cores <- c(grDevices::colorRampPalette(c("#0F3B5F", "#2E7D8F", "#3C9A9C"))(7), "#E89B3C")
  total <- length(etapas)
  # Cada linha da grade: coluna do ciclo, caixa, fio e texto.
  linha_passo <- function(i) {
    e <- etapas[[i]]
    posicao <- if (i == 1) "ciclo-inicio" else if (i == total) "ciclo-fim" else "ciclo-meio"
    shiny::tagList(
      shiny::div(class = paste("cp-ciclo", posicao)),
      shiny::div(class = "cp-caixa", style = sprintf("background:%s;%s", cores[i], if (i == total) "color:#0F3B5F;" else ""),
        shiny::span(class = "cp-num", i), shiny::icon(e$icone), shiny::span(class = "cp-titulo", e$titulo)),
      shiny::div(class = "cp-fio"),
      shiny::div(class = "cp-texto",
        if (!is.null(e$menu)) shiny::span(class = "cp-menu", e$menu),
        shiny::span(e$texto))
    )
  }
  # Numa seta do meio, os rótulos do ciclo e do fio, escritos na vertical.
  linha_seta <- function(rotulos = FALSE) shiny::tagList(
    shiny::div(class = "cp-ciclo ciclo-meio",
      if (rotulos) shiny::span(class = "cp-rotulo cp-rotulo-ciclo", "novo ciclo, nova pergunta")),
    shiny::div(class = "cp-seta", shiny::icon("arrow-down")),
    shiny::div(class = "cp-fio",
      if (rotulos) shiny::span(class = "cp-rotulo cp-rotulo-fio", "a ficha e o Projeto R acompanham você")),
    shiny::div()
  )
  # Entre planejar e preparar: a coleta de dados, mais apagada que as etapas.
  linha_campo <- shiny::tagList(
    shiny::div(class = "cp-ciclo ciclo-meio"),
    shiny::div(class = "cp-campo", shiny::icon("clipboard-list"), " Coleta de dados"),
    shiny::div(class = "cp-fio"),
    shiny::div(class = "cp-texto cp-texto-campo", "A planilha de coleta liga o planejamento aos dados de campo, laboratório, aquário, fazenda de cultivo, etc.")
  )
  linhas <- list()
  for (i in seq_len(total)) {
    linhas <- c(linhas, list(linha_passo(i)))
    if (i == 3) linhas <- c(linhas, list(linha_seta(), linha_campo))
    if (i < total) linhas <- c(linhas, list(linha_seta(rotulos = i == 5)))
  }
  estilo <- paste0(
    raiz, " .cp-mapa { height:auto; overflow:visible; background:white; border:1px solid #dbe5e8; border-radius:14px; padding:12px 14px 16px 8px; } ",
    raiz, " .coleta-bloco-titulo { font-family:'Outfit',sans-serif; font-size:1.15rem; font-weight:750; color:#0F3B5F; margin:0 0 10px; } ",
    raiz, " .coleta-bloco-intro { color:#495057; font-size:.95rem; line-height:1.45; margin:0 0 10px; } ",
    raiz, " .coleta-colunas { display:grid; grid-template-columns:minmax(0,3fr) minmax(0,2fr); gap:18px; align-items:start; } ",
    "@media(max-width:900px){", raiz, " .coleta-colunas { grid-template-columns:1fr; } }",
    # Em telas largas, toda linha de etapa tem a altura do cartão: espaçamento uniforme.
    "@media(min-width:1400px){", raiz, " .cp-texto { height:53px; overflow:visible; } }",
    # Tela baixa: rola o painel inteiro, nunca o diagrama isolado.
    ".tab-pane:has(> ", raiz, ") { overflow-y:auto; } ",
    raiz, " .cp-grade { position:relative; display:grid; grid-template-columns:40px 240px 26px minmax(0,1fr); grid-auto-rows:auto; align-items:stretch; } ",
    raiz, " .cp-caixa { display:flex; align-items:center; gap:9px; height:53px; align-self:center; padding:10px 12px; border-radius:10px; color:white; font-weight:700; font-size:.9rem; line-height:1.2; } ",
    raiz, " .cp-num { display:inline-flex; align-items:center; justify-content:center; width:24px; height:24px; border-radius:50%; background:#ffffff33; font-size:.8rem; flex:none; } ",
    raiz, " .cp-seta { height:13px; display:flex; align-items:center; justify-content:center; color:#9fb6bf; font-size:12px; line-height:12px; padding:0; } ",
    raiz, " .cp-seta svg { width:12px; height:12px; } ",
    raiz, " .cp-campo { height:53px; align-self:center; border:1.5px dashed #9fb6bf; border-radius:10px; color:#6c8793; background:#f6f9fa; font-size:.86rem; font-weight:600; padding:10px 12px; display:flex; align-items:center; justify-content:center; gap:6px; } ",
    raiz, " .cp-texto { display:flex; flex-direction:column; justify-content:center; gap:0; min-height:53px; padding:0 16px 0 6px; color:#6c757d; font-size:.8rem; line-height:1.3; } ",
    raiz, " .cp-texto-campo { color:#8a9ba3; font-style:italic; } ",
    raiz, " .cp-menu { align-self:flex-start; background:#eaf4f4; border:1px solid #bdd9d9; color:#2E7D8F; border-radius:999px; padding:0 8px; margin-bottom:2px; font-size:.7rem; line-height:1.25; font-weight:700; } ",
    # A volta do ciclo: linha à esquerda que sai da etapa 8 e entra na etapa 1.
    raiz, " .cp-ciclo { position:relative; } ",
    raiz, " .cp-ciclo::before { content:''; position:absolute; left:14px; border-color:#E89B3C; border-style:solid; border-width:0; } ",
    raiz, " .ciclo-meio::before { top:0; bottom:0; border-left-width:2px; } ",
    raiz, " .ciclo-inicio::before { top:50%; bottom:0; right:4px; border-left-width:2px; border-top-width:2px; border-top-left-radius:12px; } ",
    raiz, " .ciclo-inicio::after { content:''; position:absolute; right:0; top:calc(50% - 5px); border:6px solid transparent; border-left:7px solid #E89B3C; border-right:0; } ",
    raiz, " .ciclo-fim::before { top:0; height:50%; right:4px; border-left-width:2px; border-bottom-width:2px; border-bottom-left-radius:12px; } ",
    raiz, " .cp-rotulo { position:absolute; left:15px; top:50%; z-index:1; transform:translate(-50%,-50%) rotate(-90deg); white-space:nowrap; background:white; padding:0 6px; font-size:.73rem; } ",
    raiz, " .cp-rotulo-ciclo { color:#c07a25; font-weight:700; } ",
    # O fio da ficha e do Projeto R, fino e pontilhado, ao lado da espinha.
    raiz, " .cp-fio { position:relative; } ",
    raiz, " .cp-fio::before { content:''; position:absolute; left:14px; top:0; bottom:0; border-left:1.5px dotted #62B6B7; } ",
    raiz, " .cp-rotulo-fio { color:#2E7D8F; font-weight:600; } ",
    "@media(max-width:700px){", raiz, " .cp-grade { grid-template-columns:34px minmax(150px,200px) 22px minmax(0,1fr); } ", raiz, " .cp-texto { font-size:.76rem; } }"
  )
  shiny::div(class = "cp-mapa",
    shiny::tags$style(shiny::HTML(estilo)),
    shiny::div(class = "cp-grade", role = "img",
      `aria-label` = "Fluxograma em oito etapas: pergunta e lacuna, hipótese, planejar, preparar os dados, explorar e visualizar, analisar, comunicar e decisões, com retorno à pergunta.",
      linhas
    )
  )
}

mod_conceitos_coleta_ui <- function(id) {
  ns <- shiny::NS(id)
  raiz <- paste0("#", ns("raiz"))
  temas <- conceitos_coleta()
  escolhas <- stats::setNames(names(temas), vapply(temas, `[[`, character(1), "titulo"))

  shiny::tagList(
    shiny::tags$style(shiny::HTML(paste0(
      raiz, " { color:#0F3B5F; } ",
      raiz, " .coleta-conteudo { padding:8px 10px 18px; } ",
      raiz, " .coleta-tema { color:#2E7D8F; font-size:.8rem; font-weight:800; letter-spacing:.12em; text-transform:uppercase; } ",
      raiz, " .coleta-conteudo h2 { font-family:'Outfit',sans-serif; color:#0F3B5F; font-weight:750; margin:8px 0; } ",
      raiz, " .coleta-chamada { font-size:1.1rem; color:#2E7D8F; font-weight:650; margin:0 0 16px; } ",
      raiz, " .coleta-esquema { display:flex; gap:8px; align-items:center; flex-wrap:wrap; margin:20px 0; } ",
      raiz, " .coleta-esquema span { background:#eaf4f4; border:1px solid #bdd9d9; border-radius:10px; padding:10px 14px; font-weight:700; } ",
      raiz, " .coleta-esquema i { color:#E89B3C; } ",
      raiz, " .coleta-nota { background:#fff8ee; border-left:4px solid #E89B3C; border-radius:8px; padding:12px 15px; margin-top:15px; } ",
      raiz, " .coleta-proximo { background:#edf6f3; border-radius:10px; padding:13px 16px; margin-top:17px; } ",
      raiz, " .coleta-lateral .shiny-options-group .radio { background:white; border:1px solid #dbe5e8; border-radius:9px; padding:10px 14px; margin:0 0 5px; line-height:1.35; } ",
      raiz, " .coleta-lateral .shiny-options-group .radio:has(input:checked) { border-color:#2E7D8F; background:#eaf4f4; } ",
      raiz, " .coleta-lateral .shiny-options-group label { display:flex; align-items:flex-start; gap:10px; font-weight:600; margin:0; padding:0; text-indent:0; cursor:pointer; } ",
      raiz, " .coleta-lateral .shiny-options-group label input[type=radio] { position:static; flex:0 0 18px; width:18px; height:18px; margin:2px 0 0 0; float:none; } ",
      raiz, " .coleta-lateral .shiny-options-group label > span { flex:1 1 auto; min-width:0; line-height:1.35; text-indent:0; padding-left:0; margin:0; } ",
      raiz, " .bslib-sidebar-layout > .sidebar > .sidebar-content { padding-top:14px; } ",
      raiz, " .coleta-lateral .shiny-input-container { margin-bottom:0; } ",
      ""
    ))),
    shiny::div(id = ns("raiz"),
      # Duas colunas: à esquerda (60%) os conceitos de planejamento; à direita,
      # o mapa do caminho completo da pesquisa. Em telas estreitas, empilham.
      shiny::div(class = "coleta-colunas",
        shiny::div(class = "coleta-bloco",
          shiny::h3(class = "coleta-bloco-titulo", "Conceitos de Planejamento"),
          shiny::p(class = "coleta-bloco-intro",
            "Uma boa análise começa antes da planilha. Escolha um tema e veja como a pergunta, a unidade experimental e o modo de coletar sustentam a resposta que virá depois."),
          bslib::layout_sidebar(
            fill = FALSE, fillable = FALSE,
            sidebar = bslib::sidebar(
              # Sempre aberta, sem título nem botão de recolher: os temas começam no topo.
              position = "left", width = 290, open = "always",
              shiny::div(class = "coleta-lateral",
                shiny::radioButtons(ns("tema"), label = NULL, choices = escolhas, selected = "pergunta")
              )
            ),
            shiny::div(id = ns("conteudo"), class = "shiny-html-output", painel_conceito_coleta(temas[["pergunta"]]))
          )
        ),
        shiny::div(class = "coleta-bloco",
          shiny::h3(class = "coleta-bloco-titulo", "O caminho da pesquisa"),
          # Apresenta o mapa, no mesmo estilo da introdução da coluna da esquerda.
          shiny::p(class = "coleta-bloco-intro",
            "Da pergunta à decisão, cada etapa da pesquisa tem o seu lugar. Veja a ordem do caminho e em que menu da CatalyseR cada parte acontece."),
          fluxograma_caminho_pesquisa(raiz)
        )
      )
    )
  )
}

mod_conceitos_coleta_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    temas <- conceitos_coleta()
    output$conteudo <- shiny::renderUI({
      tema_id <- input$tema
      if (is.null(tema_id) || !tema_id %in% names(temas)) tema_id <- "pergunta"
      tema <- temas[[tema_id]]
      shiny::req(tema)
      painel_conceito_coleta(tema)
    })
    shiny::outputOptions(output, "conteudo", suspendWhenHidden = FALSE)
  })
}
