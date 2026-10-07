# Módulo de Planejamento de Delineamentos Observacionais — CatalyseR
# Unificação: Transversal Comparativo com tratamento de Amostras Compostas (Pool).
# Abas: Definições → Desenho → Ficha do delineamento →
# Metodologia para artigo → Modelo e cuidados → Resumo.
# Conteúdo de cada etapa adaptado ao delineamento.
# Estrutura anterior:
# 1. O delineamento e Variáveis de resposta (dividida em 3 colunas equilibradas)
# 2. Ficha do delineamento (coleta + saídas; no transversal, somente Excel e Word)
# 3. Metodologia para artigo (texto pré-formatado para Material e Métodos)
# 4. Modelo e cuidados (modelo estatístico recomendado e cuidados de coleta/análise)

catalogo_delineamentos_observacionais <- function() {
  list(
    transversal_comparativo = list(
      titulo = "Transversal comparativo",
      definicao = "Compara grupos preexistentes na natureza (espécies, sexos, locais ou fases) em um mesmo recorte temporal, usando unidades individuais ou amostras compostas (pool).",
      quando = "No estudo de bexigas natatórias ou desembarques pesqueiros, espécies ou categorias são níveis escolhidos de propósito; o sorteio alcança indivíduos elegíveis dentro de cada grupo. Se o tecido exigir pool, a réplica da medida será o pool independente (cada UA ocupa uma linha), e não cada peixe individual.",
      eixo = "Grupos preexistentes em um só momento",
      alerta = "O peixe ou o pool é a unidade da resposta; repetições de bancada não aumentam o n. Pools diferentes podem alterar a variabilidade. Defina a análise considerando a resposta, a independência e os pressupostos, e registre possíveis fatores de confusão entre os grupos."
    ),
    gradiente = list(
      titulo = "Estudo de gradiente",
      definicao = "Estuda como uma resposta varia ao longo de uma faixa contínua (distância de uma fonte, salinidade, contaminação, altura na maré). As estações são os locais de observação; a análise relaciona a resposta ao gradiente medido, considerando possíveis dependências espaciais e temporais.",
      quando = "Quando a pergunta é como a resposta muda ao longo de uma variação contínua, como a abundância do caranguejo com a distância do manguezal, e não comparar grupos.",
      eixo = "Valor contínuo medido em cada estação independente",
      alerta = "Arrastos ou subamostras da mesma estação não aumentam o n. Distribua as estações por toda a faixa do gradiente, não apenas nos extremos."
    ),
    longitudinal = list(
      titulo = "Longitudinal Comparativo",
      definicao = "Acompanha as mesmas unidades em momentos sucessivos. O tempo é o eixo; cada unidade mantém seu identificador em todas as visitas.",
      quando = "Quando você quer saber como o crescimento de um peixe ou a condição de um tanque muda ao longo do tempo.",
      eixo = "Tempo, com as mesmas unidades repetidas",
      alerta = "Medições repetidas da mesma unidade não são réplicas independentes. A planilha deve ficar em formato longo: uma linha por unidade e momento."
    ),
    impacto = list(
      titulo = "Estudos de impacto (BA, CI, BACI)",
      definicao = "Planeja três comparações sobre uma mudança ambiental: CI compara impacto e referência após a mudança; BA acompanha sítios antes e depois; BACI compara a mudança no impacto com a mudança nas referências. Sítios, ambientes, campanhas e subamostras têm papéis diferentes no desenho.",
      quando = "Quando a pergunta é o efeito de uma intervenção sobre condições do ambiente — um efluente de aquicultura, uma dragagem, uma barragem a montante — medida em sítios impactados e de referência, antes e depois da mudança.",
      eixo = "Local, tempo ou o cruzamento local × tempo",
      alerta = "Um único sítio de impacto não representa vários sítios independentes. Medidas antes e depois no mesmo sítio também permanecem relacionadas."
    )
  )
}

eixos_padrao_observacional <- function(tipo) {
  if (tipo %in% c("transversal", "comparativo", "transversal_comparativo")) return("grupos")
  switch(tipo,
    longitudinal = "tempo",
    gradiente = "gradiente",
    impacto = c("impacto", "tempo"),
    character()
  )
}

planejamento_observacional_painel <- function(tipo) {
  if (tipo %in% c("transversal", "comparativo")) tipo <- "transversal_comparativo"
  definicao <- catalogo_delineamentos_observacionais()[[tipo]]
  if (is.null(definicao)) stop("Delineamento observacional desconhecido.", call. = FALSE)
  bslib::nav_panel(
    title = definicao$titulo,
    icon = shiny::icon("clipboard-list"),
    mod_planejamento_observacional_ui(
      paste0("obs_", tipo), tipo,
      variaveis_ui = NULL
    )
  )
}

# Saídas diretas do transversal: planilha para preencher e metodologia editável.
# Esclarecimentos compartilhados entre a tela e as orientações exportadas.
obs_instaladas <- "Padronizar origem, tamanho, densidade, alimentação e manejo reduz explicações alternativas. Transporte, confinamento e o próprio cultivo também influenciam a resposta; crescimento e sobrevivência não identificam sozinhos qual variável ambiental atuou."
obs_espacamento <- "Não existe distância universal que garanta independência. Fluxo da água, conectividade, eventos compartilhados e estudo piloto devem orientar o espaçamento. O desenho mostra a organização, não as distâncias reais de campo."
obs_ausencias <- "Zero observado, inclusive sobrevivência zero, é uma resposta válida. Deixe a célula vazia quando a medida não foi obtida. Preserve o código da UA e registre o motivo em observacoes; para ausências, use também motivo_ausencia quando disponível."
obs_medidas <- "Cada linha representa uma UA em um momento. Peixes do mesmo tanque, folhas da mesma árvore ou amostras do mesmo local podem ser medidas internas: registre o valor-resumo e, em n_medidas, quantas medidas o formaram. Elas não aumentam o número de UAs; a unidade da comparação depende da pergunta. Sobrevivência exige contagens e denominador próprios."
obs_termos <- "Hurlbert (1984) chama essa comparação de condições preexistentes de experimento mensurativo, sem atribuição por sorteio. Quando organismos padronizados são instalados como indicadores nos ambientes, também se usam os termos transplante ativo ou bioensaio in situ (Oikari, 2006)."

cuidados_gradiente <- c(
  "Cada estação ocupa uma linha por campanha. Resuma medidas internas conforme a resposta; pool conta somente itens misturados fisicamente, com contribuição equitativa.",
  "Cubra extremos e valores intermediários. O valor planejado orienta a coleta; registre o valor medido e preserve a previsão separadamente.",
  obs_espacamento,
  "Padronize esforço, horário e maré quando cabível; registre coordenadas, sistema e fatores que possam variar junto com o gradiente.",
  "Subamostras e réplicas de bancada não aumentam o número de estações independentes. Revisitas exigem considerar a dependência temporal.",
  "Escolha o modelo conforme a resposta e examine forma da relação, resíduos, variância e dependência espacial. A reta é um ponto de partida, não uma escolha automática para toda resposta.",
  "Uma fonte limita o alcance da conclusão ao sistema estudado. Estações não replicam fontes; associação no gradiente, por si só, não demonstra causalidade.")

obs_pseudorreplicas <- paste(
  "Imagine oito estações ao longo de um canal e três arrastos em cada estação: são 24 medidas, oito estações e um canal.",
  "Os arrastos ajudam a conhecer cada estação. As estações ajudam a descrever a associação entre resposta e gradiente naquele canal, desde que a dependência espacial seja considerada.",
  "Elas não são oito canais nem oito fontes de impacto. Para generalizar a outros canais ou empreendimentos, é preciso replicar esses ambientes e registrar a hierarquia.",
  "Voltar às oito estações em outra campanha acrescenta informação temporal; não cria novas estações.",
  "A pseudorreplicação aparece quando a análise usa as medidas como réplicas independentes no nível errado para a pergunta. Coletar subamostras ou revisitar locais é válido; tratá-los como novos impactos é o problema.")

cuidados_impacto <- c(
  "Escreva o efeito esperado, a resposta, a área de influência e a data do impacto antes de escolher sítios.",
  "Registre o ambiente e a fonte de cada sítio. Quatro pontos num canal e quatro noutro são oito sítios em dois canais; não quatro ambientes independentes por condição.",
  "Escolha referências por habitat, profundidade e conectividade; confirme que ficam fora da influência do impacto. Um canal de referência limita a separação entre condição e ambiente.",
  "Um impacto específico pode ser acompanhado com vários ambientes de referência e várias campanhas antes/depois (desenho assimétrico). A conclusão continua ligada ao evento estudado.",
  "Campanhas devem cobrir a variação natural e manter controle e impacto em janelas comparáveis. Uma campanha por período limita a evidência temporal; isso não torna toda comparação automaticamente pseudorreplicada.",
  "Arrastos ou outras subamostras não replicam impactos. Registre esforço e medidas separadas; uma média é resumo, pool é mistura física.",
  "BACI compara mudanças: (Depois − Antes) no impacto menos (Depois − Antes) na referência. Também exige avaliar tendências prévias e outras mudanças específicas dos locais.",
  "Use monitoramento prévio e literatura para dimensionar o esforço. Mais linhas não corrigem confundimento; a contagem não calcula poder nem valida causalidade.",
  "Registre data, versão, critérios, covariáveis e desvios antes da coleta; mantenha identificadores permanentes e motivos de ausências.")

limite_impacto <- function(tipo) switch(tipo,
  ci = "CI compara impacto e referência após a mudança. Sem antes, diferenças que já existiam entre ambientes podem explicar o resultado.",
  ba = "BA compara os mesmos sítios antes e depois. Sem referência, a mudança pode acompanhar chuvas, sazonalidade ou uma tendência regional.",
  baci = "BACI compara a mudança antes/depois no impacto com a mudança nas referências. A comparação reduz explicações alternativas, mas não elimina confundimento nem prova causalidade por si só.")

inteiro_impacto <- function(valor, padrao) {
  if (is.null(valor)) valor <- padrao
  shiny::validate(shiny::need(length(valor) == 1L && is.finite(valor) && valor >= 1 && valor == floor(valor), "Informe quantidades inteiras positivas."))
  as.integer(valor)
}

# Cada linha do desenho representa um sítio; as cores são condições, não respostas.
desenhar_plano_impacto <- function(sitios, campanhas, n_sub, tipo) {
  pontos <- expand.grid(sitio_idx = seq_len(nrow(sitios)), campanha_idx = seq_len(nrow(campanhas)))
  pontos$x <- pontos$campanha_idx
  pontos$y <- nrow(sitios) - pontos$sitio_idx + 1
  pontos$condicao <- sitios$condicao[pontos$sitio_idx]
  cores <- stats::setNames(c("#E76F51", "#2E7D8F")[seq_along(unique(sitios$condicao))], unique(sitios$condicao))
  limite <- sum(campanhas$periodo == "Antes") + .5
  ggplot2::ggplot(pontos, ggplot2::aes(x, y, group = sitio_idx)) +
    ggplot2::geom_line(color = "#C5D8CF", linewidth = .7) +
    ggplot2::geom_point(ggplot2::aes(fill = condicao), shape = 21, size = 3.3, color = "white") +
    {if (tipo != "ci") ggplot2::geom_vline(xintercept = limite, color = "#0F3B5F", linetype = 2)} +
    ggplot2::scale_fill_manual(values = cores) +
    ggplot2::scale_x_continuous(breaks = seq_len(nrow(campanhas)), labels = campanhas$campanha,
      limits = c(.5, nrow(campanhas) + .5)) +
    ggplot2::scale_y_continuous(breaks = seq_len(nrow(sitios)), labels = rev(paste(sitios$sitio, sitios$condicao)), limits = c(-3.2, nrow(sitios) + .5)) +
    ggplot2::annotate("rect", xmin = .55, xmax = nrow(campanhas) + .45, ymin = -2.9, ymax = -.35, fill = "#E7EFEA", color = NA) +
    ggplot2::annotate("text", x = (nrow(campanhas) + 1) / 2, y = -1,
      label = sprintf("Um sítio em uma campanha → %d subamostras → %d linhas ligadas ao mesmo sítio", n_sub, n_sub), size = 3.4, color = "#0F3B5F") +
    ggplot2::annotate("text", x = (nrow(campanhas) + 1) / 2, y = -2.1,
      label = "Empreendimento / ambiente → sítios → campanhas → subamostras\nRevisitas acompanham o sítio; pontos no mesmo canal não replicam canais.", size = 3.4, color = "#2E7D8F") +
    ggplot2::labs(title = paste("Delineamento observacional", toupper(tipo)),
      subtitle = sprintf("%d sítios × %d campanhas × %d subamostras = %d linhas de coleta", nrow(sitios), nrow(campanhas), n_sub, nrow(sitios) * nrow(campanhas) * n_sub),
      x = if (tipo == "ci") "Campanhas depois do início previsto do impacto" else "Campanhas: A = antes; D = depois. A divisão marca o início previsto do impacto.", y = "Mesmo sítio em cada revisita", fill = "Condição",
      caption = paste(sprintf("Em cada ponto: %d subamostras ligadas ao sítio e à campanha; não %d impactos independentes.", n_sub, n_sub),
        "Sítios no mesmo ambiente compartilham contexto. Identifique ambiente e fonte na ficha.",
        if (tipo == "baci") "Comparação: mudança no impacto − mudança nas referências." else limite_impacto(tipo), sep = "\n")) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(plot.title = ggplot2::element_text(color = "#0F3B5F", face = "bold"),
      panel.grid.minor = ggplot2::element_blank(), legend.position = "bottom",
      plot.caption = ggplot2::element_text(hjust = 0, color = "#0F3B5F"),
      axis.text.x = ggplot2::element_text(angle = if (nrow(campanhas) > 15) 90 else 0))
}

obs_tipo_unidade_ui <- function(ns) {
  shiny::tagList(
    shiny::selectInput(ns("tipo_unidade"), "Tipo de unidade (opcional):",
      choices = c("Não informado" = "na",
        "Unidades naturais (ostras de banco, peixes capturados, plâncton)" = "naturais",
        "Unidades instaladas pelo pesquisador (tanques-rede, gaiolas, mesas de cultivo)" = "instaladas"), selected = "na", width = "100%"),
    shiny::conditionalPanel(sprintf("input['%s'] === 'instaladas'", ns("tipo_unidade")),
      shiny::tags$details(class = "small mb-2",
        shiny::tags$summary("Saiba mais sobre padronização e indicadores"),
        shiny::p(class = "mt-1 mb-0", obs_instaladas))))
}

escrever_excel_transversal <- function(coleta, orientacoes, arquivo,
    tabelas = list(coleta = coleta, orientacoes = orientacoes)) {
  wb <- openxlsx::createWorkbook()
  cabecalho <- openxlsx::createStyle(fontName = "Calibri", fontColour = "#FFFFFF",
    fgFill = "#0F3B5F", textDecoration = "bold", wrapText = TRUE, halign = "center",
    border = c("top", "bottom", "left", "right"), borderColour = "#62B6B7")
  corpo <- openxlsx::createStyle(fontName = "Calibri", fontSize = 11, valign = "top", halign = "center")
  for (aba in names(tabelas)) {
    openxlsx::addWorksheet(wb, aba)
    tabela <- tabelas[[aba]]
    openxlsx::writeData(wb, aba, tabela, headerStyle = cabecalho, withFilter = FALSE,
      borders = "all", borderColour = "#CBDDE4")
    openxlsx::addStyle(wb, aba, corpo, rows = 2:(nrow(tabela) + 1),
      cols = seq_len(ncol(tabela)), gridExpand = TRUE, stack = TRUE)
    openxlsx::addStyle(wb, aba, openxlsx::createStyle(fgFill = "#EFF7F8"),
      rows = seq(2, nrow(tabela) + 1, by = 2), cols = seq_len(ncol(tabela)),
      gridExpand = TRUE, stack = TRUE)
    openxlsx::setRowHeights(wb, aba, rows = 1, heights = 30)
    if (aba == "coleta") openxlsx::setRowHeights(wb, aba, rows = 2:(nrow(tabela) + 1), heights = 22)
    openxlsx::freezePane(wb, aba, firstRow = TRUE)
    openxlsx::setColWidths(wb, aba, cols = seq_len(ncol(tabela)), widths = if (aba == "orientacoes") c(18, 25, 95) else 20)
    if (aba == "orientacoes") {
      openxlsx::addStyle(wb, aba, openxlsx::createStyle(wrapText = TRUE, valign = "top"),
        rows = 2:(nrow(tabela) + 1), cols = 3, gridExpand = TRUE, stack = TRUE)
      openxlsx::setRowHeights(wb, aba, rows = 2:(nrow(tabela) + 1), heights = 60)
    }
  }
  # Esta planilha não tem imagens. Algumas versões do openxlsx conservam
  # relações de desenho vazias, que impedem a leitura por outros programas.
  for (i in seq_along(wb$worksheets_rels)) {
    wb$worksheets_rels[[i]] <- wb$worksheets_rels[[i]][
      !grepl("relationships/(drawing|vmlDrawing)", wb$worksheets_rels[[i]])]
  }
  openxlsx::saveWorkbook(wb, arquivo, overwrite = TRUE)
}

# Esquema do plano: uma coleta, grupos e UAs com seus pools.
# O desenho usa o plano em memória, tanto na tela como no Word.
# As setas são a estrutura do delineamento: coleta -> grupo -> UA.
desenhar_plano_transversal <- function(resumo, fator = "Grupo", respostas = character(), item = "item a definir") {
  item <- trimws(item)
  if (!nzchar(item)) item <- "item a definir"
  n_grupos <- nrow(resumo)
  centros <- rev(seq_len(n_grupos)) * 3.35
  origem <- mean(centros)
  topo <- max(centros) + 2.25
  pools <- resumo[["Itens por UA"]]
  cores <- rep(c("#2E7D8F", "#0F3B5F", "#62B6B7"), length.out = n_grupos)
  quebra <- function(x, largura = 26) paste(strwrap(x, width = largura), collapse = "\n")
  p <- ggplot2::ggplot()
  texto <- function(x, y, rotulo, tamanho = 3.3, negrito = FALSE, cor = "#0F3B5F") {
    p <<- p + ggplot2::annotate("text", x = x, y = y, label = rotulo,
      size = tamanho, colour = cor, fontface = if (negrito) "bold" else "plain", lineheight = 1.05)
  }
  caixa <- function(xmin, xmax, ymin, ymax, fundo, borda = NA) {
    p <<- p + ggplot2::annotate("rect", xmin = xmin, xmax = xmax, ymin = ymin,
      ymax = ymax, fill = fundo, colour = borda, linewidth = 0.4)
  }
  seta <- function(x, xend, y, yend = y, cor = "#62B6B7") {
    p <<- p + ggplot2::annotate("segment", x = x, xend = xend, y = y, yend = yend,
      colour = cor, linewidth = 0.6,
      arrow = grid::arrow(length = grid::unit(0.10, "inches"), type = "closed"))
  }

  # A linha do tempo só situa a coleta: o desenho detalha um único recorte.
  seta(0.45, 15.4, topo + 0.85, cor = "#2E7D8F")
  texto(14.9, topo + 0.42, "tempo", 2.8)
  caixa(0.45, 2.65, topo - 0.35, topo + 0.35, "#CBDCE9")
  texto(1.55, topo, "COLETA EM UMA\nÚNICA JANELA", 3.1, TRUE)
  texto(5.0, topo, quebra(paste0(if (grepl("×", fator, fixed = TRUE)) "FATORES: " else "FATOR: ", fator)), 3.5, TRUE)
  texto(8.0, topo, "UNIDADES AMOSTRAIS", 3.3, TRUE)
  texto(12.25, topo, "COMPOSIÇÃO DE CADA UA", 3.3, TRUE)

  # Um ponto de coleta se abre para grupos já existentes; não há tratamentos.
  p <- p + ggplot2::annotate("point", x = 1.65, y = origem, size = 6.5,
    shape = 21, fill = "#2E7D8F", colour = "white", stroke = 0.7)
  texto(1.65, origem - 0.62, "recorte\nda coleta", 2.8, FALSE)
  for (i in seq_len(n_grupos)) {
    y <- centros[i]
    n <- resumo$UAs[i]
    k <- pools[i]

    # A primeira seta é a seleção dos grupos de comparação.
    seta(2.0, 3.15, origem, y, cor = cores[i])
    caixa(3.25, 6.35, y - 0.70, y + 0.70, if (i %% 2) "#F1F7F8" else "#E6F3F1")
    texto(4.8, y + 0.20, quebra(resumo$Grupo[i]), 3.35, TRUE)
    texto(4.8, y - 0.42, quebra(sprintf("%d %s · %d %s/UA (%s)", n,
      if (n == 1) "UA" else "UAs", k, if (k == 1) "item" else "itens", item), 42), 2.55)

    # As setas seguintes são as unidades que pertencem a cada grupo.
    exibidas <- min(n, 5L)
    y_uas <- y + if (exibidas == 1) 0 else seq(0.82, -0.82, length.out = exibidas)
    for (j in seq_along(y_uas)) {
      seta(6.45, 7.65, y, y_uas[j], cor = cores[i])
      caixa(7.78, 8.12, y_uas[j] - 0.16, y_uas[j] + 0.16, cores[i])
    }
    texto(8.0, y - 1.18, if (n > exibidas)
      sprintf("%d UAs desenhadas; + %d na ficha", exibidas, n - exibidas) else
      sprintf("%d %s desenhada%s", exibidas, if (exibidas == 1) "UA" else "UAs",
        if (exibidas == 1) "" else "s"), 2.55)

    # Apenas um pequeno detalhe por grupo empresta a leitura do esquema novo:
    # bolinha única é item único; várias bolinhas são pool físico.
    caixa(9.25, 15.55, y - 0.82, y + 0.82, "#FAFCFC", "#D9E8E9")
    texto(12.45, y + 0.30, quebra(sprintf("%d %s (%s) → uma UA", k,
      if (k == 1) "item" else "itens", item), 30), 3.05, TRUE)
    itens_x <- seq(10.35, 10.35 + 0.28 * (min(k, 5L) - 1), length.out = min(k, 5L))
    p <- p + ggplot2::annotate("point", x = itens_x, y = y - 0.16,
      shape = 21, size = 2.55, fill = cores[i], colour = "white")
    seta(max(itens_x) + 0.16, 12.0, y - 0.16, cor = cores[i])
    caixa(12.15, 12.48, y - 0.32, y, cores[i])
    if (k > 5) texto(11.0, y - 0.47, sprintf("+ %d", k - 5), 2.35)
  }
  nomes <- gsub("_", " ", utils::head(respostas, 3))
  rotulo_respostas <- if (length(nomes)) paste(nomes, collapse = ", ") else "a definir"
  if (length(respostas) > 3) rotulo_respostas <- paste0(rotulo_respostas, "; + ", length(respostas) - 3, " na ficha")
  total <- sum(resumo$UAs)
  itens <- sum(resumo$UAs * pools)
  caixa(0.45, 15.55, 0.45, 1.50, "#FBEAD1")
  texto(8, 1.18, sprintf("1 UA = 1 linha na coleta · respostas: %s", rotulo_respostas), 3.05, TRUE)
  texto(8, 0.75, sprintf("%d %s · %d %s = %d %s de coleta · %d %s",
    n_grupos, if (grepl("×", fator, fixed = TRUE)) "combinações de fatores" else if (n_grupos == 1) "grupo" else "grupos",
    total, if (total == 1) "UA" else "UAs", total, if (total == 1) "linha" else "linhas",
    itens, sprintf("unidades físicas previstas (%s)", item)), 2.65)
  p + ggplot2::coord_cartesian(xlim = c(0, 16), ylim = c(0, topo + 1.25), expand = FALSE, clip = "off") +
    ggplot2::labs(title = "Delineamento transversal comparativo",
      subtitle = "Comparação entre grupos preexistentes em um único recorte temporal. As setas mostram a hierarquia da coleta.",
      caption = paste0("Cada seta que sai de um grupo representa uma UA. Mostram-se até 5 UAs por grupo; a ficha preserva os totais.\n",
        "Cada bolinha é a unidade física declarada acima; várias bolinhas indicam pool físico. O esquema não tem escala espacial e não comprova independência nem causalidade.")) +
    ggplot2::theme_void(base_size = 12) + ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", colour = "#0F3B5F", size = 15, hjust = 0),
      plot.subtitle = ggplot2::element_text(size = 10, margin = ggplot2::margin(b = 12)),
      plot.title.position = "plot",
      plot.caption = ggplot2::element_text(hjust = 0, size = 9.5, margin = ggplot2::margin(t = 10)),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.margin = ggplot2::margin(12, 16, 12, 16))
}

# Infográfico gerado do plano informado, como no longitudinal.
# Pontos em posições proporcionais aos valores do gradiente.
# Os cartões mantêm espaço para leitura e são ligados às posições das estações.
desenhar_plano_gradiente <- function(coleta, coluna, unidade) {
  n <- nrow(coleta)
  indices <- if (n <= 12L) seq_len(n) else unique(round(seq(1, n, length.out = 12)))
  tab <- coleta[indices, , drop = FALSE]
  topo <- 9.5
  eixo_y <- 6.8
  posicoes_cartoes <- seq(1.3, 14.7, length.out = nrow(tab))
  faixa <- range(coleta[[coluna]])
  posicoes <- if (diff(faixa) > 0) {
    1.3 + (tab[[coluna]] - faixa[1]) / diff(faixa) * 13.4
  } else rep(8, nrow(tab))
  p <- ggplot2::ggplot()
  texto <- function(x, y, rotulo, tamanho = 3.3, negrito = FALSE) {
    p <<- p + ggplot2::annotate("text", x = x, y = y, label = rotulo,
      size = tamanho, colour = "#0F3B5F", fontface = if (negrito) "bold" else "plain", lineheight = 1.1)
  }
  caixa <- function(xmin, xmax, ymin, ymax, fundo) {
    p <<- p + ggplot2::annotate("rect", xmin = xmin, xmax = xmax,
      ymin = ymin, ymax = ymax, fill = fundo, colour = NA)
  }
  quebra <- function(x, largura = 30) paste(strwrap(x, width = largura), collapse = "\n")
  texto(8, topo, quebra(paste(gsub("_", " ", coluna), paste0("(", unidade, ")")), 70), 4, TRUE)
  p <- p + ggplot2::annotate("segment", x = 0.5, xend = 15.5, y = eixo_y,
    yend = eixo_y, colour = "#2E7D8F", linewidth = 1,
    arrow = grid::arrow(length = grid::unit(0.12, "inches"), type = "closed"))
  texto(2, eixo_y - 0.35, sprintf("Início: %g %s", min(coleta[[coluna]]), unidade), 3.2)
  texto(14, eixo_y - 0.35, sprintf("Fim: %g %s", max(coleta[[coluna]]), unidade), 3.2)
  for (i in seq_len(nrow(tab))) {
    x <- posicoes_cartoes[i]
    # Alternar os cartões mantém uma única linha de pontos e evita sobreposição.
    acima <- i %% 2 == 0
    y <- eixo_y + if (acima) 1.35 else -1.35
    borda_y <- y + if (acima) -0.7 else 0.7
    p <- p + ggplot2::annotate("segment", x = posicoes[i], xend = x,
      y = eixo_y, yend = borda_y, colour = "#62B6B7", linewidth = 0.5)
    caixa(x - 1.15, x + 1.15, y - 0.7, y + 0.7, "#E6F3F1")
    texto(x, y + 0.43, tab$estacao[i], 3.7, TRUE)
    texto(x, y + 0.06, sprintf("%g %s", signif(tab[[coluna]][i], 4), unidade), 3.2)
    texto(x, y - 0.30, "1 estação = 1 UA", 3.1, TRUE)
    texto(x, y - 0.56, sprintf("Pool: %d item(ns)", tab$pool[i]), 2.8)
  }
  # Cada ponto é uma estação planejada; não representa uma resposta observada.
  p <- p + ggplot2::annotate("point", x = posicoes, y = eixo_y,
    shape = 21, size = 5, stroke = 0.8, fill = "#2E7D8F", colour = "white")
  texto(8, 4.35, "DA ESTAÇÃO AO REGISTRO DA COLETA", 3.7, TRUE)
  cores <- c("#CBDCE9", "#E6F3F1", "#FBEAD1")
  for (i in 1:3) caixa((i - 1) * 5.4, (i - 1) * 5.4 + 5.1, 1.65, 3.9, cores[i])
  texto(2.55, 3.48, "Identificar e medir o eixo", 3.6, TRUE)
  texto(2.55, 2.62, "Código e posição da estação\nConferir o valor real em campo\nRegistrar local, data e esforço", 3.1)
  texto(7.95, 3.48, "Medir a resposta", 3.6, TRUE)
  texto(7.95, 2.62, "Resumo de medidas ≠ pool\nPool é mistura física de itens\nMedidas internas não são novas UAs", 3.1)
  texto(13.35, 3.48, "1 linha por estação", 3.6, TRUE)
  respostas <- setdiff(names(coleta), c("estacao", coluna, "pool", "observacoes"))
  rotulo <- paste(gsub("_", " ", utils::head(respostas, 3)), collapse = ", ")
  if (length(respostas) > 3) rotulo <- paste0(rotulo, "; + ", length(respostas) - 3, " na ficha")
  texto(13.35, 2.65, quebra(paste("Respostas:", rotulo)), 3.1)
  texto(8, 1.1, sprintf("%d estações previstas = %d UAs = %d linhas por campanha", n, n, n), 3.5, TRUE)
  texto(8, 0.5, "Cobrir extremos e valores intermediários · Justificar o espaçamento e conferir fatores de confusão", 3.1)
  p + ggplot2::coord_cartesian(xlim = c(-0.1, 16.3), ylim = c(0, topo + 0.5), expand = FALSE, clip = "off") +
    ggplot2::labs(title = "Delineamento observacional de gradiente",
      subtitle = paste0("Como a resposta varia ao longo do eixo?\n",
        "Pontos proporcionais aos valores do gradiente; cartões ligados às estações, deslocados para facilitar a leitura."),
      caption = paste0(if (n > 12) sprintf("Exibidas %d de %d estações, incluindo os extremos; a ficha contém todas.\n", nrow(tab), n) else "",
        "A escala representa a variável informada; é espacial quando o eixo é distância. Não representa um mapa de coordenadas.\n",
        "Valores previstos: substitua pelos medidos na coleta e preserve a previsão na aba estacoes.\n",
        "O desenho não comprova independência nem causalidade. Revisitas não aumentam o número de estações.")) +
    ggplot2::theme_void(base_size = 12) + ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", colour = "#0F3B5F", size = 15),
      plot.subtitle = ggplot2::element_text(size = 10, margin = ggplot2::margin(b = 12)),
      plot.caption = ggplot2::element_text(hjust = 0, size = 10, margin = ggplot2::margin(t = 12)),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.margin = ggplot2::margin(12, 12, 12, 12))
}

# Infográfico do planejamento: grupos, mesmas UAs nas visitas e registro.
# Os símbolos são unidades previstas, não resultados ou trajetórias simuladas.
desenhar_plano_longitudinal <- function(coleta, fator, unidade, momentos,
    medidas, meta_final, perda, janela, linha_base = TRUE, relogio = "comum") {
  grupos <- unique(coleta[[fator]])
  centros <- 4.6 + rev(seq_along(grupos) - 1) * 1.5
  topo <- max(centros) + 1.3
  xs <- seq(4.8, 14.6, length.out = length(momentos))
  largura <- min(2.35, 9.8 / length(momentos) * 0.85)
  cores <- rep(c("#2E7D8F", "#0F3B5F", "#62B6B7"), length.out = length(grupos))
  quebra <- function(x, largura = 28) paste(strwrap(x, width = largura), collapse = "\n")
  p <- ggplot2::ggplot()
  texto <- function(x, y, rotulo, tamanho = 3.5, cor = "#0F3B5F", negrito = FALSE) {
    p <<- p + ggplot2::annotate("text", x = x, y = y, label = rotulo,
      size = tamanho, colour = cor, fontface = if (negrito) "bold" else "plain", lineheight = 1.05)
  }
  caixa <- function(xmin, xmax, ymin, ymax, fundo, borda = NA) {
    p <<- p + ggplot2::annotate("rect", xmin = xmin, xmax = xmax,
      ymin = ymin, ymax = ymax, fill = fundo, colour = borda, linewidth = 0.4)
  }
  seta <- function(x, xend, y, cor = "#62B6B7") {
    p <<- p + ggplot2::annotate("segment", x = x, xend = xend, y = y, yend = y,
      colour = cor, linewidth = 0.6, arrow = grid::arrow(length = grid::unit(0.10, "inches"), type = "closed"))
  }
  texto(1.55, topo, paste0("GRUPOS\n", quebra(gsub("_", " ", fator), 22)), 3.5, negrito = TRUE)
  # A mesma coleção de códigos reaparece em cada visita de cada grupo.
  for (j in seq_along(momentos)) {
    caixa(xs[j] - largura / 2, xs[j] + largura / 2, topo - 0.35, topo + 0.35,
      if (j == 1 && linha_base) "#FBEAD1" else "#CBDCE9")
    texto(xs[j], topo + 0.08, if (grepl("dias?", momentos[j])) momentos[j] else paste(momentos[j], "dias"), 3.5, negrito = TRUE)
    texto(xs[j], topo - 0.18, if (j == 1 && linha_base) "linha de base" else paste("visita", j), 2.8)
  }
  for (i in seq_along(grupos)) {
    y <- centros[i]
    ids <- unique(coleta[[unidade]][coleta[[fator]] == grupos[i]])
    n <- length(ids)
    caixa(0, 16, y - 0.68, y + 0.68, if (i %% 2) "#F1F7F8" else "#FAFCFC")
    caixa(0, 0.07, y - 0.68, y + 0.68, cores[i])
    texto(1.55, y + 0.17, quebra(grupos[i], 20), 3.7, negrito = TRUE)
    texto(1.55, y - 0.35, quebra(sprintf("%d UAs (%s)", n, gsub("_", " ", unidade)), 26), 3.1)
    for (j in seq_along(momentos)) {
      texto(xs[j], y + 0.44, sprintf("%d mesmas UAs", n), 3.0, cores[i], TRUE)
      exibidas <- min(n, 5L)
      posicoes <- if (exibidas == 1) xs[j] else seq(xs[j] - largura * 0.39,
        xs[j] + largura * 0.39, length.out = exibidas)
      w <- min(0.34, largura / (exibidas + 1))
      for (k in seq_len(exibidas)) {
        # Símbolo genérico: o nome da UA pode mudar sem mudar seu significado.
        caixa(posicoes[k] - w / 2, posicoes[k] + w / 2, y - 0.04, y + 0.13, cores[i])
        texto(posicoes[k], y - 0.39, as.character(ids[k]), 2.7, cores[i])
      }
      if (n > exibidas) texto(xs[j], y - 0.60, sprintf("+ %d UAs não desenhadas", n - exibidas), 2.5)
      if (j < length(momentos)) seta(xs[j] + largura / 2 + 0.05,
        xs[j + 1] - largura / 2 - 0.05, y + 0.03)
    }
  }
  texto(8, 3.60, "O QUE ACONTECE DENTRO DE CADA UA, EM CADA VISITA?", 3.5, negrito = TRUE)
  caixa(0.2, 4.5, 1.45, 3.25, "#CBDCE9")
  caixa(5.4, 10.2, 1.45, 3.25, "#E6F3F1")
  caixa(11.1, 15.9, 1.45, 3.25, "#FBEAD1")
  texto(2.35, 2.91, quebra(paste0("1 ", gsub("_", " ", unidade), " = 1 UA"), 28), 3.6, negrito = TRUE)
  texto(2.35, 2.12, "Código permanente\nA mesma unidade volta a ser medida", 3.1)
  texto(7.8, 2.91, sprintf("%d medidas previstas", medidas), 3.8, negrito = TRUE)
  texto(7.8, 2.12, "Resumir conforme a resposta\nEx.: altura média; sobrevivência em %\nSubamostras não são novas UAs", 3.0)
  respostas <- names(coleta)[!names(coleta) %in% c(unidade, fator, "replica", "momento",
    "data_prevista", "ordem_grupo", "ordem_visita", "data_real", "n_medidas", "status_ua", "motivo_ausencia", "observacoes")]
  texto(13.5, 2.91, "1 registro por UA × visita", 3.7, negrito = TRUE)
    # Mostrar até três nomes mantém o cartão legível; todas as colunas seguem na ficha.
  nomes_resposta <- gsub("_", " ", utils::head(respostas, 3))
  resumo_respostas <- paste(nomes_resposta, collapse = ", ")
  if (length(respostas) > 3) resumo_respostas <- paste0(resumo_respostas,
    sprintf("; + %d na ficha", length(respostas) - 3))
  texto(13.5, 2.10, quebra(paste("Respostas:", resumo_respostas), 32), 3.0)
  seta(4.6, 5.25, 2.35)
  seta(10.3, 10.95, 2.35)
  n_uas <- length(unique(coleta[[unidade]]))
  texto(8, 0.96, sprintf("%d grupos × %d UAs por grupo = %d UAs distintas  |  %d UAs × %d visitas = %d linhas na coleta",
    length(grupos), n_uas / length(grupos), n_uas, n_uas, length(momentos), nrow(coleta)), 3.6, negrito = TRUE)
  texto(8, 0.43, sprintf("Meta: %d UAs finais/grupo · Perda prevista: %g%% · Início ampliado para %d UAs/grupo",
    meta_final, perda, n_uas / length(grupos)), 3.2)
  p + ggplot2::coord_cartesian(xlim = c(-0.1, 16.1), ylim = c(0, topo + 0.55), expand = FALSE, clip = "off") +
    ggplot2::labs(title = "Longitudinal comparativo: os grupos mudam de forma diferente com o tempo?",
      subtitle = "Grupos preexistentes, sem sorteio de tratamentos. Cada símbolo representa uma UA com o mesmo código nas visitas.",
      caption = paste0(sprintf("Em cada momento: todos os grupos na janela de %d dias; ordem de visita sorteada. ", janela),
        if (relogio == "individual") "Tempo contado desde o início de cada UA." else "Tempo contado desde um início comum.",
        "\nUA perdida mantém código e motivo registrado. Linhas e subamostras não aumentam o n.",
        "\nEsquema sem escala espacial: o desenho não comprova independência. UAs dentro de um ambiente não replicam ambientes.")) +
    ggplot2::theme_void(base_size = 12) + ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", colour = "#0F3B5F", size = 15, hjust = 0),
      plot.subtitle = ggplot2::element_text(size = 10, margin = ggplot2::margin(b = 12)),
      plot.title.position = "plot", plot.caption = ggplot2::element_text(hjust = 0, size = 9.5, margin = ggplot2::margin(t = 10)),
      plot.margin = ggplot2::margin(12, 16, 12, 16), plot.background = ggplot2::element_rect(fill = "white", colour = NA))
}

escrever_word_transversal <- function(arquivo, pergunta, metodologia, resumo, dicionario, cuidados, coleta, esquema, longitudinal = FALSE, fonte = "Curadoria EAPA: Planejamento_03_TRANSVERSAL, exemplo de bexigas natatórias (setembro de 2026). Referências de apoio registradas na curadoria: Hurlbert (1984), Quinn e Keough (2002), Blainey, Krzywinski e Altman (2014) e Wickham (2014).", titulo_documento = NULL, impacto = FALSE, item = "itens") {
  # O Word é criado diretamente: este download não depende do Quarto instalado.
  doc <- officer::read_docx()
  doc <- officer::body_set_default_section(doc, officer::prop_section(
    page_size = officer::page_size(width = 8.27, height = 11.69),
    page_margins = officer::page_mar(top = 1.18, bottom = 0.79, left = 1.18, right = 0.79)))
  paragrafo <- function(texto) {
    doc <<- officer::body_add_fpar(doc, officer::fpar(
      officer::ftext(texto, officer::fp_text(font.family = "Calibri", font.size = 11)),
      fp_p = officer::fp_par(text.align = "justify", padding.bottom = 8)))
  }
  titulo <- function(texto) {
    doc <<- officer::body_add_par(doc, texto, style = "heading 1")
  }
  tabela <- function(dados) {
    ft <- flextable::flextable(dados)
    ft <- flextable::theme_booktabs(ft)
    ft <- flextable::font(ft, fontname = "Calibri", part = "all")
    ft <- flextable::fontsize(ft, size = 10, part = "all")
    ft <- flextable::bg(ft, bg = "#0F3B5F", part = "header")
    ft <- flextable::color(ft, color = "white", part = "header")
    ft <- flextable::set_table_properties(ft, layout = "autofit", width = 1)
    doc <<- flextable::body_add_flextable(doc, ft)
  }
  doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(
    if (!is.null(titulo_documento)) titulo_documento else if (longitudinal) "Planejamento longitudinal comparativo" else "Planejamento — Transversal comparativo", officer::fp_text(
      font.family = "Cambria", font.size = 20, bold = TRUE, color = "#0F3B5F"))))
  paragrafo("CatalyseR · Minuta de metodologia para revisão antes da coleta")
  titulo("Pergunta do estudo")
  paragrafo(pergunta)
  titulo("Material e métodos previstos")
  for (p in strsplit(metodologia, "\n\n", fixed = TRUE)[[1]]) paragrafo(p)
  titulo("Distribuição prevista das unidades amostrais")
  tabela(resumo)
  if (impacto) paragrafo(sprintf("Total previsto: %d sítios e %d linhas de coleta (sítio × campanha × subamostra). Essas contagens não demonstram independência nem representam um cálculo de tamanho amostral.", sum(resumo$UAs_iniciais), sum(resumo$Linhas))) else if (longitudinal) paragrafo(sprintf("Total previsto: %d UAs iniciais e %d linhas de coleta (UA × momento).", sum(resumo$UAs_iniciais), sum(resumo$Linhas))) else paragrafo(sprintf("Total previsto: %d UAs e %d unidades físicas (%s). Essas quantidades descrevem o plano informado; não representam um cálculo de tamanho amostral nem um sorteio realizado.", sum(resumo$UAs), sum(resumo[["Itens previstos"]]), item))
  # Cada quebra encerra a seção anterior. O restante volta ao retrato padrão.
  secao <- function(horizontal = FALSE) officer::block_section(officer::prop_section(
    page_size = officer::page_size(width = 8.27, height = 11.69,
      orient = if (horizontal) "landscape" else "portrait"),
    page_margins = officer::page_mar(top = 0.79, bottom = 0.79, left = 0.79, right = 0.79),
    type = "nextPage"))
  doc <- officer::body_end_block_section(doc, secao())
  titulo("Esquema do delineamento")
  imagem <- tempfile(fileext = ".png")
  on.exit(unlink(imagem), add = TRUE)
  ggplot2::ggsave(imagem, esquema, width = 10, height = 6.1, dpi = 180, bg = "white")
  doc <- officer::body_add_img(doc, imagem, width = 9.7, height = 5.917, style = "centered")
  doc <- officer::body_end_block_section(doc, secao(TRUE))
  titulo("Planilha de coleta — primeiras linhas")
  paragrafo(sprintf(if (impacto) "Prévia das primeiras %d de %d linhas (sítio × campanha × subamostra). As respostas ficam vazias; o dicionário explica todas as colunas." else if (longitudinal) "Prévia das primeiras %d de %d linhas (UA × momento). As respostas ficam vazias; o dicionário explica todas as colunas." else "Prévia das primeiras %d de %d UAs. As células de resposta estão vazias para preenchimento após a coleta; todas as variáveis da planilha aparecem abaixo.", min(8L, nrow(coleta)), nrow(coleta)))
  previa <- flextable::flextable(utils::head(coleta, 8))
  previa <- flextable::theme_booktabs(previa)
  previa <- flextable::font(previa, fontname = "Calibri", part = "all")
  previa <- flextable::fontsize(previa, size = 8, part = "all")
  previa <- flextable::bg(previa, bg = "#0F3B5F", part = "header")
  previa <- flextable::color(previa, color = "white", part = "header")
  previa <- flextable::width(previa, width = 9.7 / ncol(coleta))
  previa <- flextable::set_table_properties(previa, layout = "fixed", width = 1)
  doc <- flextable::body_add_flextable(doc, previa)
  doc <- officer::body_end_block_section(doc, secao(TRUE))
  titulo("Variáveis e registro da coleta")
  tabela(dicionario[, c("coluna", "unidade", "descricao")])
  titulo("Cuidados e pontos a conferir")
  for (p in cuidados) paragrafo(p)
  titulo("Base de curadoria")
  paragrafo(fonte)
  print(doc, target = arquivo)
  invisible(arquivo)
}

# ---- INTERFACE DO MÓDULO -----------------------------------------------------

mod_planejamento_observacional_ui <- function(id, tipo, variaveis_ui = NULL) {
  ns <- shiny::NS(id)
  tipo_resolvido <- if (tipo %in% c("transversal", "comparativo")) "transversal_comparativo" else tipo
  definicao <- catalogo_delineamentos_observacionais()[[tipo_resolvido]]
  if (is.null(definicao)) stop("Delineamento observacional desconhecido.", call. = FALSE)

  estilo <- shiny::tags$style(shiny::HTML("
    .obs-estudio .card, .obs-estudio .card-body, .obs-estudio .tab-content, .obs-estudio .tab-pane {
      overflow: visible !important; height: auto !important; min-height: 0; }
    .obs-estudio .card-body { display: block !important; padding: 16px 18px; }
    .obs-coluna-bloco { display: flex; flex-direction: column; gap: 12px; }
    /* No impacto, cada sub-aba ocupa a largura disponível em três colunas.
       O contorno envolve a coluna inteira, inclusive seus vários cartões. */
    .obs-impacto-coluna {
      border: 2px solid #7895ad;
      border-radius: 10px;
      align-self: stretch;
      min-width: 0;
      flex: 1;
    }
    .obs-impacto-coluna > .obs-card-interno { border: 0; box-shadow: none; }
    .obs-impacto-coluna .shiny-input-container { width: 100%; min-width: 0; }
    .obs-impacto-coluna h5 {
      background: #cbdce9; padding: 8px 10px; border-radius: 4px;
    }
    .obs-modelos-impacto pre {
      white-space: pre-wrap; overflow-wrap: anywhere;
      font-size: 0.9rem; line-height: 1.35; padding: 8px 10px; margin-bottom: 6px;
    }
    .obs-modelos-impacto pre code { padding: 0; font-size: inherit; line-height: inherit; }
    .obs-modelos-impacto .obs-modelo-caso {
      border-top: 1px solid #dbe5e8; padding-top: 6px; margin-top: 6px;
    }
    .obs-modelos-impacto h6 { color: #0F3B5F; font-weight: 700; }
    .obs-modelos-impacto p, .obs-cuidados-impacto p { margin-bottom: 6px; }
    .obs-cuidados-impacto li { margin-bottom: 6px; }
    .obs-cuidados-impacto .alert { padding: 10px 12px; margin-bottom: 12px; }
    .obs-gradiente-coluna {
      border: 2px solid #7895ad;
      border-radius: 10px;
      align-self: stretch;
    }
    .obs-gradiente-coluna > .obs-card-interno {
      border: 0;
      box-shadow: none;
    }
    .obs-card-interno {
      background: #ffffff;
      border: 1px solid #dbe5e8;
      border-radius: 10px;
      padding: 14px 16px;
      box-shadow: 0 1px 3px rgba(15, 59, 95, 0.03);
    }
    .obs-card-interno h5 {
      color: #0F3B5F;
      font-size: 0.95rem;
      font-weight: 700;
      margin-bottom: 8px;
    }
    .obs-coluna-definicoes {
      align-self: stretch;
      min-height: calc(100vh - 220px);
    }
    .obs-coluna-definicoes > .obs-card-interno {
      border: 2px solid #7895ad;
      flex: 1;
    }
    .obs-amostra-tabela { width: 100%; table-layout: fixed; }
    .obs-amostra-tabela th, .obs-amostra-tabela td {
      padding: 8px; border: 1px solid #dbe5e8; vertical-align: middle;
    }
    .obs-amostra-tabela th { background: #cbdce9; color: #0F3B5F; }
    .obs-amostra-tabela .shiny-input-container { margin-bottom: 0; }
    .obs-estacoes-tabela td { padding: 3px 8px; }
    .obs-estacoes-tabela .form-control {
      height: 28px; min-height: 28px; padding: 2px 8px;
    }
    .obs-fator-niveis {
      margin-top: 16px; padding: 0; background: transparent;
    }
    .obs-coluna-definicoes h5 {
      background: #cbdce9; padding: 8px 10px;
      border-radius: 4px; font-size: 0.9rem;
    }
    .obs-coluna-definicoes .alert,
    .obs-coluna-definicoes .obs-var-linha {
      background: #ffffff !important;
    }
    @media (max-width: 767px) {
      .obs-coluna-definicoes { min-height: 0; }
    }
    /* Variante compacta da coluna 1: rótulos e margens apertados para ganhar
       espaço vertical sem mudar o contrato dos inputs. */
    .obs-estudio .obs-apertado .shiny-input-container { margin-bottom: 6px; }
    .obs-estudio .obs-apertado .control-label { font-size: 0.83rem; margin-bottom: 2px; }
    .obs-estudio .obs-apertado .form-control { padding: 3px 8px; font-size: 0.87rem; height: auto; }
    .obs-estudio .obs-apertado .shiny-input-radiogroup .radio { margin: 1px 0; }
    .obs-estudio .obs-apertado .shiny-input-radiogroup .radio label { font-size: 0.86rem; }
    .obs-pool-grid {
      display: grid;
      grid-template-columns: repeat(auto-fill, minmax(130px, 1fr));
      gap: 10px;
      margin-top: 8px;
    }
    .obs-botoes-exportacao .btn {
      font-weight: 600;
      font-size: 0.88rem;
      padding: 8px 12px;
    }
    .obs-resumo-downloads {
      display: flex; flex-direction: column; gap: 24px;
      width: 100%; max-width: 410px; margin: 8px auto 0;
    }
    .obs-resumo-downloads .btn {
      width: 100%; min-width: 0; white-space: normal;
      font-size: 0.82rem; font-weight: 600; padding: 5px 10px;
    }
    .obs-cuidados-longitudinal ul {
      column-count: 2; column-gap: 28px; padding-left: 20px; margin-bottom: 0;
    }
    .obs-cuidados-longitudinal li { break-inside: avoid; margin-bottom: 6px; line-height: 1.45; }
    .obs-modelo-longitudinal code { font-size: 0.9rem; white-space: pre-wrap; overflow-wrap: anywhere; }
    .obs-modelo-longitudinal .obs-parametros { line-height: 1.4; margin-top: 8px; }
    @media (max-width: 767px) { .obs-cuidados-longitudinal ul { column-count: 1; } }
    .obs-resumo-grade {
      display: grid; grid-template-columns: minmax(0, 1fr) minmax(0, 1fr);
      gap: 32px; align-items: start;
    }
    @media (max-width: 700px) {
      .obs-resumo-grade { grid-template-columns: 1fr; }
    }
    /* Linhas compactas das variáveis de resposta: uma embaixo da outra, sem respiro extra */
    .obs-var-cab {
      display: flex; align-items: center; gap: 8px;
      padding: 0 8px; margin-bottom: 2px;
    }
    .obs-var-linha {
      display: flex; align-items: center; gap: 8px;
      padding: 3px 8px; margin-bottom: 4px;
    }
    .obs-var-linha .shiny-input-container { margin-bottom: 0 !important; width: 100%; }
    .obs-var-linha .form-control { padding: 2px 8px; font-size: 0.85rem; height: auto; }
    .obs-var-rotulo {
      min-width: 16px; text-align: right;
      font-weight: 700; font-size: 0.85rem; color: #0F3B5F;
    }
    /* Paginação e contador da tabela tidy encostados na tabela, sem respiro extra */
    .obs-estudio .dataTables_wrapper table.dataTable { margin-bottom: 0 !important; }
    .obs-estudio .dataTables_wrapper .dataTables_info { padding-top: 2px !important; }
    .obs-estudio .dataTables_wrapper .dataTables_paginate {
      margin-top: 0 !important; padding-top: 0 !important;
    }
    .obs-estudio .dataTables_wrapper > .row:last-child { margin-top: 0 !important; }
  "))

  definicoes_conteudo <- bslib::card_body(fillable = FALSE,
          bslib::layout_columns(
            col_widths = c(4, 4, 4),
            gap = "20px",

            # COLUNA 1: Delineamento
            shiny::div(class = if (tipo_resolvido == "impacto") "obs-coluna-bloco obs-impacto-coluna obs-apertado" else if (tipo_resolvido %in% c("transversal_comparativo", "longitudinal")) "obs-coluna-bloco obs-coluna-definicoes" else if (tipo_resolvido == "gradiente") "obs-coluna-bloco obs-gradiente-coluna" else "obs-coluna-bloco",
              # No estudo de gradiente o eixo é contínuo: no lugar de fator e
              # níveis entram a variável do gradiente e as estações da faixa.
              if (identical(tipo_resolvido, "gradiente")) {
                shiny::tagList(
                  shiny::div(class = "obs-card-interno obs-apertado",
                    shiny::h5(shiny::icon("water"), " 1. Definição do gradiente"),
                    shiny::p(class = "small text-muted", "Orientação inicial: este painel organiza um plano indicativo. Confirme o delineamento, o esforço e os procedimentos na literatura do ambiente e da resposta estudados antes da coleta."),
                    # Pergunta do estudo, já com um exemplo de gradiente.
                    shiny::textInput(
                      ns("pergunta"), "Pergunta do estudo:",
                      value = "A abundância do caranguejo diminui com a distância do manguezal?",
                      placeholder = "O que você deseja responder com este estudo?",
                      width = "100%"
                    ),
                    # Variável e unidade dividem a mesma linha: a unidade é
                    # curta e não precisa de uma linha inteira.
                    bslib::layout_columns(
                      col_widths = c(8, 4), gap = "10px",
                      # Nome da variável contínua; vira o nome da coluna na planilha.
                      shiny::textInput(
                        ns("gradiente_nome"), "Variável do gradiente (nome da coluna):",
                        value = "distancia_fonte",
                        placeholder = "Ex.: distancia_fonte, salinidade, altura_mare",
                        width = "100%"
                      ),
                      # Unidade do gradiente; vai ao dicionário e aos textos.
                      shiny::textInput(
                        ns("gradiente_unidade"), "Unidade de medida:",
                        value = "m",
                        placeholder = "Ex.: m, ‰, mg/kg",
                        width = "100%"
                      )
                    )
                  ),
                  shiny::div(class = "obs-card-interno obs-apertado",
                    shiny::h5(shiny::icon("signs-post"), " 2. Estações ao longo da faixa"),
                    # Duas formas de definir as estações; gerar espaçadas é o padrão.
                    # Rótulos curtos para não quebrar linha na coluna estreita.
                    shiny::radioButtons(
                      ns("modo_estacoes"), "Como definir os valores das estações?",
                      choices = c(
                        "Equidistantes no eixo do gradiente" = "igual",
                        "Progressão geométrica (valores positivos)" = "geometrico",
                        "Digitar valores livremente" = "livre"
                      ),
                      selected = "igual",
                      inline = FALSE
                    ),
                    # No modo gerado, a CatalyseR distribui os valores com seq().
                    # Início, fim e número de estações dividem uma única linha.
                    shiny::conditionalPanel(
                      condition = sprintf("input['%s'] != 'livre'", ns("modo_estacoes")),
                      bslib::layout_columns(
                        col_widths = c(4, 4, 4), gap = "10px",
                        shiny::numericInput(
                          ns("estacao_inicio"), "Início da faixa:",
                          value = 50, step = 1, width = "100%"
                        ),
                        shiny::numericInput(
                          ns("estacao_fim"), "Fim da faixa:",
                          value = 400, step = 1, width = "100%"
                        ),
                        shiny::numericInput(
                          ns("n_estacoes"), "Nº de estações:",
                          value = 8, min = 2, step = 1, width = "100%"
                        )
                      )
                    ),
                    # No modo livre, a planilha usa os valores digitados em ordem crescente.
                    shiny::conditionalPanel(
                      condition = sprintf("input['%s'] == 'livre'", ns("modo_estacoes")),
                      shiny::textInput(
                        ns("valores_livres"), "Valores das estações (separados por vírgula):",
                        value = "50, 100, 150, 200, 250, 300, 350, 400",
                        placeholder = "Ex.: 50, 100, 150, 200, 250",
                        width = "100%"
                      )
                    ),
                    # Lembretes de desenho amostral em texto pequeno.
                    shiny::p(class = "small text-muted", "Equidistante significa intervalos iguais na variável informada; só representa distância física se o eixo for distância. A progressão geométrica concentra valores perto do início positivo e precisa de justificativa ecológica."),
                    shiny::p(class = "small text-muted mb-0",
                      "Distribua as estações por toda a faixa do gradiente, e não apenas nos extremos. Espace-as para que estações vizinhas não fiquem parecidas só pela proximidade."
                    )
                  )
                )
              } else {
                shiny::tagList(
              # Delineamentos de impacto: o formulário pede a condição dos
              # sítios (impactado × referência) e, conforme o tipo, os momentos
              # antes/depois — com exemplos de ambiente, não de bancada.
              if (identical(tipo_resolvido, "impacto")) {
                shiny::div(class = if (tipo_resolvido == "longitudinal") "obs-card-interno obs-apertado" else "obs-card-interno",
                  shiny::h5(shiny::icon("sliders"), " 1. Pergunta e alcance do impacto"),
                  shiny::textInput(
                    ns("pergunta"), "Pergunta do estudo:",
                    value = "A qualidade da água mudou depois da instalação dos tanques-rede, em comparação com trechos de referência?",
                    placeholder = "O que você deseja responder com este estudo?",
                    width = "100%"
                  ),
                  shiny::radioButtons(
                    ns("tipo_impacto"), "Qual a pergunta de impacto?",
                    choices = c(
                      "Controle–Impacto (CI): comparar sítios depois da mudança" = "ci",
                      "Antes–Depois (BA): comparar o mesmo sítio antes e depois" = "ba",
                      "BACI: antes–depois com sítios de referência" = "baci"
                    ),
                    selected = "baci",
                    inline = FALSE
                  ),
                  shiny::selectInput(ns("impacto_escala"), "Alcance do estudo:",
                    c("Um empreendimento / sistema específico", "Vários empreendimentos / sistemas"), width = "100%")
                )
              } else {
              shiny::div(class = if (tipo_resolvido == "longitudinal") "obs-card-interno obs-apertado" else "obs-card-interno",
                shiny::h5(shiny::icon("sliders"), if (tipo_resolvido %in% c("transversal_comparativo", "longitudinal")) " 1. Apresentação do estudo e procedimentos de coleta e medição" else " 1. Definição do Fator e Amostragem"),
                shiny::textInput(
                  ns("pergunta"), "Pergunta do estudo:",
                  value = if (tipo_resolvido == "longitudinal") "O crescimento e a sobrevivência das ostras seguem trajetórias diferentes nos três estuários?" else "Qual a composição lipídica das bexigas natatórias entre espécies de peixes?",
                  placeholder = "O que você deseja responder com este estudo?",
                  width = "100%"
                ),
                if (tipo_resolvido %in% c("transversal_comparativo", "longitudinal")) shiny::tagList(
                  shiny::selectInput(ns("comparacao_ambiental"), "Comparação ambiental (opcional):",
                    choices = c("Não se aplica" = "na",
                      "Faixas de um mesmo sistema (ex.: estuário interno, externo e costa)" = "faixas",
                      "Ambientes específicos escolhidos (ex.: três estuários)" = "especificos",
                      "Categorias de ambientes (ex.: reservatórios eutrofizados e pouco eutrofizados)" = "categorias"),
                    selected = "na", width = "100%"),
                  obs_tipo_unidade_ui(ns),
                  shiny::textInput(ns("local_periodo"), "Local e período previstos:", placeholder = "Onde e em qual recorte temporal ocorrerá a coleta?", width = "100%"),
                  shiny::textAreaInput(ns("criterios_coleta"), "Critérios de seleção e composição das UAs:",
                    placeholder = if (tipo_resolvido == "longitudinal") "Defina elegibilidade, origem e seleção das mesmas UAs que serão acompanhadas." else "Defina elegibilidade, janela biométrica, lotes e seleção dos indivíduos. Se houver pool, descreva sua formação.", rows = 3, width = "100%"),
                  shiny::textAreaInput(ns("procedimentos_coleta"), "Procedimentos de coleta e medição:",
                    placeholder = if (tipo_resolvido == "longitudinal") "Descreva identificação permanente, biometria, horário, maré e protocolo entre visitas." else "Descreva identificação, conservação, preparo, ensaios e unidades de medida.", rows = 3, width = "100%")
                ),
                if (tipo_resolvido == "transversal_comparativo") shiny::div(class = "mt-3",
                  shiny::h5(shiny::icon("location-dot"), " Espaçamento e localização das estações"),
                  shiny::p(class = "small text-muted", "Use estes campos quando a UA for uma estação ou ponto de coleta. A distância mínima é uma decisão do protocolo; ela não é provada pelo desenho."),
                  shiny::textInput(ns("espacamento_minimo_m"), "Distância mínima planejada entre estações (m):",
                    placeholder = "Deixe em branco até concluir o piloto e a avaliação hidrodinâmica.", width = "100%"),
                  shiny::textAreaInput(ns("justificativa_espacamento"), "Hidrodinâmica, estudo-piloto e justificativa do espaçamento:",
                    placeholder = "Descreva circulação, maré, conectividade, alcance espacial e como o piloto orientará a distância mínima.", rows = 3, width = "100%"),
                  shiny::checkboxInput(ns("registrar_coordenadas"), "Incluir coordenadas previstas de cada estação na ficha de coleta", FALSE),
                  shiny::conditionalPanel(condition = sprintf("input['%s']", ns("registrar_coordenadas")),
                    bslib::layout_columns(col_widths = c(7, 5), gap = "10px",
                      shiny::selectInput(ns("referencia_coordenadas"), "Sistema de referência:",
                        choices = c("WGS 84 — latitude / longitude decimal" = "WGS84"), width = "100%"),
                      shiny::numericInput(ns("precisao_gps_m"), "Precisão horizontal máxima (m):", 5, min = 1, step = 1, width = "100%")))) ,
                shiny::div(class = if (tipo_resolvido == "transversal_comparativo") "obs-fator-niveis" else NULL,
                if (tipo_resolvido == "transversal_comparativo") shiny::h5(shiny::icon("tags"), " 2. Fator (variável independente) e seus níveis ou grupos"),
                shiny::textInput(
                  ns("fator_nome"), if (tipo_resolvido == "transversal_comparativo") "Fator (variável em estudo):" else "Nome do fator ou categoria:",
                  value = if (tipo_resolvido == "longitudinal") "estuario" else "especie",
                  placeholder = "Ex.: especie, sexo, local, estagio",
                  width = "100%"
                ),
                shiny::textInput(
                  ns("fator_niveis"), if (tipo_resolvido == "transversal_comparativo") "Categorias ou níveis (separados por vírgula):" else "Níveis ou grupos (separados por vírgula):",
                  value = if (tipo_resolvido == "transversal_comparativo") "Tambaqui, Gurijuba, Pescada-amarela, Pescada-corvina, Uritinga" else if (tipo_resolvido == "longitudinal") "Caeté, Emboraí Velho, Quatipuru" else "Tambaqui, Gurijuba, Pescada Amarela, Pargo, Camurupim",
                  placeholder = "Ex.: Tambaqui, Gurijuba, Pescada Amarela",
                  width = "100%"
                ),
                if (tipo_resolvido == "transversal_comparativo") shiny::tagList(
                  shiny::checkboxInput(ns("usar_fator2"), "Incluir um segundo fator", FALSE),
                  shiny::conditionalPanel(condition = sprintf("input['%s']", ns("usar_fator2")),
                    shiny::textInput(ns("fator2_nome"), "Segundo fator:", value = "sexo", width = "100%"),
                    shiny::textInput(ns("fator2_niveis"), "Categorias ou níveis do segundo fator:", value = "Fêmea, Macho", width = "100%"))
                )),
                if (tipo_resolvido != "transversal_comparativo") shiny::numericInput(
                  ns("n_uas"), if (tipo_resolvido == "longitudinal") "UAs desejadas ao final por grupo:" else "Unidades amostrais (UAs / réplicas) por grupo:",
                  value = if (tipo_resolvido == "longitudinal") 4 else 5, min = 1, step = 1, width = "100%"
                )
              )
              },
              # O longitudinal acompanha unidades repetidas. No impacto,
              # as campanhas ficam na sub-aba Protocolo e calendário.
              if (identical(tipo_resolvido, "longitudinal")) {
                shiny::div(class = if (tipo_resolvido == "longitudinal") "obs-card-interno obs-apertado" else "obs-card-interno",
                  shiny::h5(shiny::icon("clock"), " 2. Momentos e unidade repetida"),
                  shiny::p(class = "small text-muted mb-2",
                    "Liste os momentos em que cada unidade será medida de novo. Na planilha, cada unidade aparece uma vez por momento, sempre com o mesmo identificador."
                  ),
                  shiny::textInput(
                    ns("momentos"), "Momentos ou visitas (separados por vírgula):",
                    value = "0, 60, 120, 180 dias",
                    placeholder = "Ex.: 0, 15, 30, 45 dias",
                    width = "100%"
                  ),
                  shiny::textInput(
                    ns("coluna_unidade"), "Nome da coluna do identificador da unidade:",
                    value = "mesa",
                    placeholder = "Ex.: peixe, tanque, animal",
                    width = "100%"
                  ),
                  shiny::checkboxInput(ns("linha_base"), "Primeiro momento é linha de base", TRUE),
                  shiny::radioButtons(ns("relogio"), "Referência do tempo:",
                    c("Data inicial comum" = "comum", "Dias desde o início de cada UA" = "individual")),
                  shiny::dateInput(ns("data_inicial"), "Data inicial prevista:", value = "2027-02-01"),
                  shiny::numericInput(ns("janela_dias"), "Janela de visita por momento (dias):", 3, min = 1, step = 1),
                  shiny::numericInput(ns("semente_visitas"), "Semente da ordem de visita:", 2027, min = 1, step = 1),
                  shiny::uiOutput(ns("avisos_longitudinal"))
                )
              }
                )
              }
            ),

            # COLUNA 2: Composição da UA (Pool) e, no longitudinal, os momentos
            shiny::div(class = if (tipo_resolvido == "impacto") "obs-coluna-bloco obs-impacto-coluna obs-apertado" else if (tipo_resolvido %in% c("transversal_comparativo", "gradiente", "longitudinal")) "obs-coluna-bloco obs-coluna-definicoes" else "obs-coluna-bloco",
              if (identical(tipo_resolvido, "gradiente")) {
                shiny::div(class = if (tipo_resolvido == "longitudinal") "obs-card-interno obs-apertado" else "obs-card-interno",
                  shiny::h5(shiny::icon("layer-group"), " 3. Composição da estação (Pool)"),
                  # Mesma regra do Transversal, com a estação como unidade amostral.
                  shiny::p(class = "small text-muted mb-2",
                    shiny::tags$b("Regra da CatalyseR: "),
                    "cada estação ocupa exatamente uma linha na planilha. Quando indivíduos pequenos são misturados para gerar massa de análise, essa mistura é uma amostra composta e os itens viram um número na coluna pool."
                  ),
                  shiny::uiOutput(ns("ui_pools_grupos")),
                  # No gradiente este espaço mostra o resumo do plano, porque a
                  # homogeneidade do pool não muda a análise (sempre regressão).
                  shiny::div(class = "mt-2",
                    shiny::uiOutput(ns("alerta_pool_diagnostico"))
                  )
                )
              } else if (tipo_resolvido == "impacto") {
                shiny::div(class = "obs-card-interno",
                  shiny::h5(shiny::icon("location-dot"), " 2. Sítios e condições"),
                  # No BA (antes–depois) o próprio sítio é o seu controle, então
                  # a condição dos sítios não se aplica: escondemos o campo.
                  shiny::conditionalPanel(
                    condition = sprintf("input['%s'] !== 'ba'", ns("tipo_impacto")),
                    shiny::textInput(
                      ns("fator_nome"), "Nome da coluna da condição do sítio:",
                      value = "situacao",
                      placeholder = "Ex.: situacao, condicao, trecho",
                      width = "100%"
                    ),
                    shiny::textInput(
                      ns("fator_niveis"), "Condições dos sítios (separadas por vírgula):",
                      value = "Impacto, Referência",
                      placeholder = "Ex.: Impacto, Referência",
                      width = "100%"
                    )
                  ),
                  shiny::numericInput(
                    ns("n_uas"), "Sítios de impacto previstos:",
                    value = 3, min = 1, step = 1, width = "100%"
                  ),
                  shiny::conditionalPanel(condition = sprintf("input['%s'] !== 'ba'", ns("tipo_impacto")),
                    shiny::numericInput(ns("n_referencia"), "Sítios de referência previstos:", 3, min = 1, step = 1)),
                  shiny::p(class = "small text-muted", "Sítios são locais de coleta. Sua independência e o nível de generalização precisam ser justificados; vários pontos no mesmo canal não são vários canais."),
                  shiny::textInput(ns("coluna_unidade"), "Nome da coluna do sítio:", "sitio"),
                  obs_tipo_unidade_ui(ns)
                )
              } else if (tipo_resolvido == "transversal_comparativo") {
                shiny::div(class = if (tipo_resolvido == "longitudinal") "obs-card-interno obs-apertado" else "obs-card-interno",
                  shiny::h5(shiny::icon("layer-group"), " 3. UAs e pool por categoria"),
                  shiny::textInput(ns("unidade_item"), "O que é cada item ou porção física?",
                    value = "peixe individual", placeholder = "Ex.: peixe individual; garrafa de água (1 L); porção de 1 kg de sedimento"),
                  shiny::p(class = "small text-muted", "Descreva a unidade física que forma cada UA. Pool conta itens ou porções misturados numa amostra composta; 1 indica uma unidade física, e não necessariamente um indivíduo. Uma média de medidas individuais é um resumo, não um pool."),
                  shiny::uiOutput(ns("ui_amostra_categorias")),
                  shiny::div(class = "mt-2", shiny::uiOutput(ns("alerta_pool_diagnostico")))
                )
              } else if (tipo_resolvido == "longitudinal") {
                shiny::div(class = if (tipo_resolvido == "longitudinal") "obs-card-interno obs-apertado" else "obs-card-interno",
                  shiny::h5("3. Unidade, subamostras e perdas"),
                  shiny::p(class = "small", "A UA é o que volta a ser medido: mesa, viveiro ou indivíduo marcado. As medidas internas recebem um resumo adequado por visita; isso não é pool."),
                  bslib::layout_columns(col_widths = c(6, 6), gap = "10px",
                    shiny::numericInput(ns("n_medidas"), "Medidas por UA e visita:", 60, min = 1, step = 1, width = "100%"),
                    shiny::numericInput(ns("perda_pct"), "Perda de UAs (%):", 20, min = 0, max = 99, step = 1, width = "100%")),
                  shiny::textInput(ns("caracteristicas_base"), "Características de partida (colunas, separadas por vírgula):", "area, densidade_inicial"),
                  shiny::textInput(ns("agrupamento"), "Local ou propriedade em comum (nome da coluna):", "local_origem"),
                  shiny::p(class = "small", "Unidades no mesmo local podem compartilhar fluxo, manejo e eventos, como hipóxia ou cheia. Registre o local de cada UA; se a UA for o indivíduo marcado, registre também o viveiro."),
                  shiny::actionButton(ns("conferir_locais"), "Conferir locais das UAs", icon = shiny::icon("table"), class = "btn-outline-primary btn-sm"))
              } else {
              shiny::div(class = if (tipo_resolvido == "longitudinal") "obs-card-interno obs-apertado" else "obs-card-interno",
                # No impacto o número do cartão depende do tipo (CI esconde o
                # cartão de momentos), então o título é montado no servidor.
                if (identical(tipo_resolvido, "impacto")) {
                  shiny::uiOutput(ns("titulo_pool_impacto"))
                } else {
                  shiny::h5(shiny::icon("layer-group"),
                    if (identical(tipo_resolvido, "longitudinal")) " 3. Composição da Unidade Amostral (Pool)" else " 2. Composição da Unidade Amostral (Pool)")
                },
                shiny::p(class = "small text-muted mb-2",
                  shiny::tags$b("Regra da CatalyseR: "),
                  "cada UA ocupa exatamente uma linha na planilha. Quando indivíduos pequenos são misturados para gerar massa de análise, essa mistura é uma amostra composta e os itens viram um número na coluna pool."
                ),
                shiny::radioButtons(
                  ns("tipo_pool"), "Como os itens compõem cada UA?",
                  choices = if (tipo_resolvido == "transversal_comparativo") c(
                    "Mesmo pool em todos os grupos" = "igual",
                    "Pool diferente por grupo" = "desigual"
                  ) else c(
                    "Todos os grupos têm o mesmo pool (padrão = 1 item por UA)" = "igual",
                    "O pool varia conforme o grupo (amostras compostas desiguais)" = "desigual"
                  ),
                  selected = "igual",
                  inline = FALSE
                ),
                shiny::conditionalPanel(
                  condition = sprintf("input['%s'] == 'igual'", ns("tipo_pool")),
                  shiny::numericInput(
                    ns("pool_unico"), "Número de itens em cada UA (para todos os grupos):",
                    value = 1, min = 1, step = 1, width = "100%"
                  )
                ),
                shiny::conditionalPanel(
                  condition = sprintf("input['%s'] == 'desigual'", ns("tipo_pool")),
                  shiny::p(class = "small text-muted mb-1", "Informe a quantidade de itens agrupados em cada UA por grupo:"),
                  shiny::uiOutput(ns("ui_pools_grupos"))
                ),
                shiny::div(class = "mt-2",
                  shiny::uiOutput(ns("alerta_pool_diagnostico"))
                )
              )
              }
            ),
            
            # COLUNA 3: Variáveis de Resposta
            shiny::div(class = if (tipo_resolvido == "impacto") "obs-coluna-bloco obs-impacto-coluna obs-apertado" else if (tipo_resolvido %in% c("transversal_comparativo", "longitudinal")) "obs-coluna-bloco obs-coluna-definicoes" else if (tipo_resolvido == "gradiente") "obs-coluna-bloco obs-gradiente-coluna" else "obs-coluna-bloco",
              shiny::div(class = if (tipo_resolvido == "longitudinal") "obs-card-interno obs-apertado" else "obs-card-interno",
                if (identical(tipo_resolvido, "impacto")) {
                  shiny::uiOutput(ns("titulo_variaveis_impacto"))
                } else {
                  shiny::h5(shiny::icon("list-check"),
                    if (tipo_resolvido %in% c("transversal_comparativo", "longitudinal", "gradiente")) " 4. Variáveis de Resposta" else " 3. Variáveis de Resposta")
                },
                shiny::p(class = "small text-muted mb-2",
                  "Declare as variáveis que serão medidas em laboratório ou campo para cada UA. Cada nome vira uma coluna com células vazias na planilha de coleta."
                ),
                shiny::numericInput(
                  ns("n_vars_resposta"),
                  if (tipo_resolvido == "transversal_comparativo") "Número de variáveis:" else "Quantidade de variáveis a medir:",
                  value = 2, min = 1, max = 10, step = 1, width = "150px"
                ),
                shiny::uiOutput(ns("ui_vars_resposta_campos")),
                shiny::div(class = "alert alert-light border small mt-3 mb-0",
                  style = "border-left: 4px solid #2E7D8F !important;",
                  shiny::tags$b("Atenção às réplicas analíticas: "),
                  "Duplicatas ou triplicatas de bancada no mesmo extrato medem apenas a precisão instrumental e devem ser resumidas pela média antes de entrar nesta planilha. Elas não aumentam o número de UAs."
                )
              )
            )
          )
        )

  shiny::div(class = "obs-estudio",
    estilo,
    shiny::p(class = "small text-muted",
      "Orientação de planejamento: as opções ajudam a estruturar uma proposta inicial. Revise o delineamento e justifique as decisões na literatura específica antes da coleta; este painel não valida o plano nem calcula automaticamente o esforço necessário."),
    shiny::uiOutput(ns("planejamento_atual")),
    shiny::div(class = "alert alert-light border mb-2 py-2 px-3",
      style = "border-left: 4px solid #2E7D8F !important;",
      shiny::tags$b(definicao$titulo), shiny::tags$br(),
      definicao$definicao
    ),
    bslib::navset_card_tab(
      id = ns("etapas"),

      # Exercício de entrada: aplica um cenário pequeno e deixa explícitas as
      # decisões espaciais que a turma ainda precisa justificar.
      if (tipo_resolvido == "transversal_comparativo") bslib::nav_panel(
        "Exercício", icon = shiny::icon("graduation-cap"),
        bslib::card_body(fillable = FALSE,
          bslib::layout_columns(col_widths = c(7, 5), gap = "20px",
            shiny::div(class = "obs-card-interno",
              shiny::h5(shiny::icon("water"), " Exercício: faixas ambientais de um sistema estuarino"),
              shiny::p("Em uma única campanha, compare três faixas do mesmo sistema: estuário interno, estuário externo e costa. Cada faixa terá cinco estações independentes; em cada estação será coletada uma amostra de água. Não haverá amostra composta (pool)."),
              shiny::p(class = "small text-muted", "O exercício gera 15 UAs e 15 linhas de coleta. Elas representam a variação entre estações dentro deste sistema; não representam automaticamente todos os estuários e costas da região."),
              shiny::actionButton(ns("usar_exercicio_faixas"), "Usar este cenário no formulário", icon = shiny::icon("arrow-right"), class = "btn-primary")),
            shiny::div(class = "obs-card-interno",
              shiny::h5(shiny::icon("pen-to-square"), " O que a turma deve completar"),
              shiny::tags$ol(class = "small ps-3 mb-2",
                shiny::tags$li("Definir pergunta, respostas, sistema, período e janela de maré."),
                shiny::tags$li("Delimitar o quadro amostral e justificar como as estações serão selecionadas."),
                shiny::tags$li("Registrar latitude e longitude previstas de cada estação, em WGS 84, com precisão horizontal de até 5 m."),
                shiny::tags$li("Explicar como hidrodinâmica, conectividade e estudo-piloto definirão o espaçamento mínimo.")),
              shiny::div(class = "alert alert-warning small mb-0",
                shiny::tags$b("Não fixe 100 ou 200 m por convenção. "),
                "As distâncias reais podem variar; a distância mínima precisa ser justificada para este sistema antes da coleta."))))),
      
      # ABA 1: O DELINEAMENTO E VARIÁVEIS DE RESPOSTA (3 COLUNAS)
      bslib::nav_panel("Definições", icon = shiny::icon("compass-drafting"),
        if (tipo_resolvido == "gradiente") bslib::navset_tab(id = ns("definicoes_gradiente"),
          bslib::nav_panel("Pergunta, eixo e respostas", definicoes_conteudo),
          bslib::nav_panel(
        "Organizar e avaliar", icon = shiny::icon("clipboard-check"),
        bslib::card_body(fillable = FALSE,
          shiny::p("Plano indicativo para revisão com a literatura. As escolhas abaixo registram decisões; não demonstram independência, poder amostral ou efeito causal."),
          bslib::layout_columns(col_widths = c(5, 7), gap = "20px",
            shiny::div(class = if (tipo_resolvido == "longitudinal") "obs-card-interno obs-apertado" else "obs-card-interno",
              shiny::h5("Contexto e organização da coleta"),
              shiny::selectInput(ns("gradiente_ambiente"), "Ambiente:",
                c("A definir", "Rio / canal", "Praia / costa", "Estuário", "Outro"), width = "100%"),
              shiny::selectInput(ns("gradiente_fontes"), "Fonte ou origem do gradiente:",
                c("Gradiente ambiental sem fonte pontual", "Uma fonte identificada", "Várias fontes / sistemas"), width = "100%"),
              shiny::textAreaInput(ns("gradiente_referencia"), "Estações de referência (quando cabíveis):",
                placeholder = "Informe locais e justificativa; no rio, considere montante e possíveis diferenças naturais.", rows = 3, width = "100%"),
              bslib::layout_columns(col_widths = c(7, 5), gap = "12px",
              shiny::selectInput(ns("gradiente_subamostras"), "Registro dentro de cada estação:",
                c("Uma medida por estação", "Subamostras resumidas por estação", "Amostra composta física (pool)"), width = "100%"),
              shiny::selectInput(ns("gradiente_campanhas"), "Campanhas previstas:",
                c("Uma campanha", "Revisitas às mesmas estações"), width = "100%")),
              shiny::textAreaInput(ns("gradiente_protocolo"), "Procedimento e distribuição das subamostras:",
                placeholder = "Descreva esforço, posição, preservação e resumo das medidas; média e mistura física são procedimentos diferentes.", rows = 4, width = "100%")),
            shiny::div(class = if (tipo_resolvido == "longitudinal") "obs-card-interno obs-apertado" else "obs-card-interno",
              shiny::h5("O que conferir antes de ir a campo"),
              shiny::textAreaInput(ns("gradiente_covariaveis"), "Segundo gradiente e outros fatores a medir:",
                placeholder = "Ex.: granulometria, matéria orgânica, salinidade; registre possíveis fatores de confusão.", rows = 3, width = "100%"),
              shiny::textAreaInput(ns("gradiente_janela"), "Período, maré / hidrologia e ordem de visita:",
                placeholder = "Defina uma janela comparável e justifique a ordem de visita conforme acesso e segurança.", rows = 3, width = "100%"),
              shiny::textAreaInput(ns("gradiente_literatura"), "Referências e decisões ainda pendentes:",
                placeholder = "Quais estudos sustentam o espaçamento, o esforço e o protocolo? O que falta confirmar?", rows = 4, width = "100%"),
              shiny::uiOutput(ns("avaliacao_gradiente"))))))) else if (tipo_resolvido == "impacto") bslib::navset_tab(id = ns("definicoes_impacto"),
          bslib::nav_panel("Pergunta, sítios e respostas", definicoes_conteudo),
          bslib::nav_panel("Protocolo e calendário", bslib::card_body(fillable = FALSE,
            bslib::layout_columns(col_widths = c(4, 4, 4), gap = "20px",
              # Procedimentos e subamostras ficam juntos: descrevem a coleta.
              shiny::div(class = "obs-coluna-bloco obs-impacto-coluna obs-apertado",
                shiny::div(class = "obs-card-interno",
                  shiny::h5(shiny::icon("clipboard-list"), " 4. Protocolo de coleta"),
                  shiny::textAreaInput(ns("impacto_criterios"), "Seleção dos sítios e ambientes de referência:",
                    placeholder = "Habitat, profundidade, conectividade e ausência da pluma. Pontos no mesmo canal compartilham ambiente.", rows = 3, width = "100%"),
                  shiny::numericInput(ns("impacto_subamostras"), "Subamostras por sítio e campanha:", 3, min = 1, step = 1, width = "100%"),
                  shiny::textAreaInput(ns("impacto_protocolo"), "Apetrecho, esforço e distribuição das subamostras:", rows = 3, width = "100%"),
                  shiny::p(class = "small text-muted", "Uma linha por sítio × campanha × subamostra. Arrastos, quadrados ou medidas internas permanecem ligados ao sítio; não são novos impactos."),
                  shiny::p(class = "small text-muted mb-0", "Guarde as medidas separadamente. Uma média é um resumo; pool é mistura física e exige um protocolo próprio."))),
              # Campanhas, datas e intervalo compõem um único calendário.
              shiny::div(class = "obs-coluna-bloco obs-impacto-coluna obs-apertado",
                shiny::div(class = "obs-card-interno",
                  shiny::h5(shiny::icon("calendar-days"), " 5. Campanhas e calendário"),
                  shiny::p(class = "small text-muted", "Cada sítio mantém o identificador nas campanhas. No CI há apenas campanhas depois; BA e BACI incluem antes e depois. No BACI, a referência acompanha o mesmo calendário."),
                  shiny::dateInput(ns("impacto_inicio"), "Início previsto do impacto:", "2028-01-01", width = "100%"),
                  shiny::conditionalPanel(condition = sprintf("input['%s'] !== 'ci'", ns("tipo_impacto")),
                    bslib::layout_columns(col_widths = c(5, 7), gap = "10px",
                      shiny::numericInput(ns("impacto_antes"), "Campanhas antes:", 6, min = 1, step = 1, width = "100%"),
                      shiny::dateInput(ns("impacto_primeira_antes"), "Primeira campanha antes:", "2027-01-15", width = "100%"))),
                  bslib::layout_columns(col_widths = c(5, 7), gap = "10px",
                    shiny::numericInput(ns("impacto_depois"), "Campanhas depois:", 6, min = 1, step = 1, width = "100%"),
                    shiny::dateInput(ns("impacto_primeira_depois"), "Primeira campanha depois:", "2028-01-15", width = "100%")),
                  shiny::numericInput(ns("impacto_intervalo"), "Intervalo entre campanhas (meses):", 2, min = 1, step = 1, width = "100%"),
                  shiny::textAreaInput(ns("impacto_janela"), "Janela, maré e ordem de visita:",
                    placeholder = "Todos os sítios na mesma janela de cada campanha; maré comparável e ordem justificada.", rows = 3, width = "100%"),
                  shiny::p(class = "small text-muted mb-0", "As datas são indicativas. A estação climática depende da região; confira se antes e depois cobrem condições sazonais comparáveis."))),
              shiny::div(class = "obs-coluna-bloco obs-impacto-coluna obs-apertado",
                shiny::div(class = "obs-card-interno",
                  shiny::h5(shiny::icon("clipboard-check"), " 6. Registro e revisão do plano"),
                  shiny::textAreaInput(ns("impacto_covariaveis"), "Covariáveis e outras mudanças previstas:",
                    placeholder = "Maré, salinidade, chuvas, dragagem e mudanças no manejo.", rows = 3, width = "100%"),
                  shiny::textAreaInput(ns("impacto_registro"), "Referências, data e versão do plano:", rows = 3, width = "100%"),
                  shiny::uiOutput(ns("avaliacao_impacto")))))))) else definicoes_conteudo),
      if (tipo_resolvido == "transversal_comparativo") bslib::nav_panel(
        "Desenho", icon = shiny::icon("diagram-project"),
        bslib::card_body(fillable = FALSE,
          shiny::p("Leia da esquerda para a direita: a coleta se abre para os grupos e as setas seguintes levam às UAs. À direita, as bolinhas mostram a unidade física declarada — por exemplo, peixe, garrafa de água ou porção de sedimento — e se cada UA é formada por uma unidade ou por um pool; o rodapé informa as respostas registradas."),
          shiny::uiOutput(ns("esquema_transversal_ui")))) ,
      if (tipo_resolvido != "transversal_comparativo") bslib::nav_panel(
        "Desenho", icon = shiny::icon("diagram-project"),
        bslib::card_body(fillable = FALSE,
          shiny::p(if (tipo_resolvido == "longitudinal") "Leia da esquerda para a direita: os grupos mantêm as mesmas UAs nas visitas. A parte inferior mostra como as medidas dentro da UA viram um registro na coleta." else if (tipo_resolvido == "gradiente") "Leia o eixo e os cartões das estações em ordem crescente. A parte inferior mostra como medir o eixo e a resposta e registrar uma linha por estação." else if (tipo_resolvido == "impacto") "Cada linha mantém o mesmo sítio nas campanhas. A mudança começa na divisão antes/depois; as subamostras estão dentro de cada visita." else definicao$eixo),
          if (tipo_resolvido != "longitudinal") shiny::p(class = "small text-muted", definicao$alerta),
          if (tipo_resolvido == "gradiente") shiny::p(class = "small text-muted",
            "Os pontos respeitam os intervalos dos valores planejados. Para distância, isso representa escala espacial; para salinidade ou concentração, representa a escala da variável. Os cartões são deslocados para continuar legíveis."),
          shiny::plotOutput(ns("desenho_observacional"), height = if (tipo_resolvido %in% c("longitudinal", "gradiente", "impacto")) "620px" else "520px"),
          if (tipo_resolvido == "longitudinal") shiny::div(class = "alert alert-light border small mt-2",
            shiny::tags$b("Qual nível está sendo replicado? "),
            "Várias UAs de cultivo em um rio permitem estudar a variação dentro daquele rio; elas não são vários rios. Para comparar categorias ambientais, planeje vários ambientes por categoria. O espaçamento e a dependência entre UAs precisam ser justificados em campo."))),
      # ABA 2: FICHA DO DELINEAMENTO (TABELA TIDY COMPLETA + BOTÕES DE EXPORTAÇÃO ABAIXO)
      # Sem botão de vínculo com as análises: os dados serão importados depois da
      # coleta e a estrutura da tabela pode mudar até lá.
      bslib::nav_panel("Ficha do delineamento", icon = shiny::icon("table"),
        bslib::card_body(fillable = FALSE,
          shiny::div(class = "d-flex justify-content-between align-items-center mb-2",
            shiny::h5(class = "fw-bold text-primary mb-0", shiny::icon("table"),
              if (tipo_resolvido == "longitudinal") " Planilhas do delineamento" else " Planilha Tidy Completa de Coleta"),
            shiny::uiOutput(ns("badge_total_uas"))
          ),
          if (tipo_resolvido == "impacto") {
            bslib::navset_tab(id = ns("planilhas_impacto"),
              bslib::nav_panel("Coleta", shiny::p("Uma linha por subamostra de um sítio em uma campanha. Respostas vazias aguardam a coleta."), DT::DTOutput(ns("tabela_tidy_coleta"))),
              bslib::nav_panel("Sítios e ambientes", DT::DTOutput(ns("sitios_impacto"))),
              bslib::nav_panel("Campanhas", DT::DTOutput(ns("campanhas_impacto"))),
              bslib::nav_panel("Orientações", DT::DTOutput(ns("orientacoes_impacto"))))
          } else if (tipo_resolvido == "longitudinal") {
            # As quatro sub-abas correspondem às quatro planilhas do Excel.
            bslib::navset_tab(id = ns("planilhas_ficha"),
              bslib::nav_panel("Coleta",
                shiny::p(class = "small text-muted mt-3 mb-2",
                  "Uma linha por UA × momento. O mesmo código volta em cada visita; linhas não são novas UAs. Respostas ausentes ficam vazias, com o motivo anotado."),
                DT::DTOutput(ns("tabela_tidy_coleta"))),
              bslib::nav_panel("Unidades e características de partida",
                shiny::p(class = "small text-muted mt-3 mb-2",
                  "Preencha as características de partida e o local compartilhado na aba unidades do Excel: fluxo, manejo e eventos podem relacionar UAs do mesmo local. Em inícios individuais, registre data_inicio."),
                DT::DTOutput(ns("unidades_longitudinal"))),
              bslib::nav_panel("Calendário previsto",
                shiny::p(class = "small text-muted mt-3 mb-2",
                  "A ordem é sorteada dentro de cada momento. Com início individual, use dia_previsto e deslocamento_dias a partir de data_inicio; a data real será registrada na coleta."),
                shiny::tableOutput(ns("calendario_longitudinal"))),
              bslib::nav_panel("Orientações",
                shiny::p(class = "small text-muted mt-3 mb-2",
                  "As mesmas orientações da aba orientacoes do Excel: decisões do planejamento, descrição das colunas e cuidados no acompanhamento."),
                DT::DTOutput(ns("orientacoes_longitudinal"))))
          } else if (tipo_resolvido == "gradiente") {
            bslib::navset_tab(id = ns("planilhas_gradiente"),
              bslib::nav_panel("Coleta",
                shiny::p(class = "small text-muted mt-3", "Uma linha por estação em uma campanha. Substitua o valor previsto do eixo pelo medido em campo; preserve a previsão na aba estacoes. Respostas não obtidas ficam vazias, com motivo em observacoes."),
                DT::DTOutput(ns("tabela_tidy_coleta"))),
              bslib::nav_panel("Estações e localização",
                shiny::p(class = "small text-muted mt-3", "Guarde o valor planejado e preencha sistema, local e coordenadas no Excel. Esta tabela organiza a localização; não comprova independência."),
                DT::DTOutput(ns("estacoes_gradiente"))),
              bslib::nav_panel("Orientações",
                shiny::p(class = "small text-muted mt-3", "Decisões declaradas, descrição das colunas e cuidados também disponíveis na aba orientacoes do Excel."),
                DT::DTOutput(ns("orientacoes_gradiente"))))
          } else {
            shiny::tagList(
              shiny::p(class = "small text-muted mb-2",
                "Uma linha por Unidade Amostral (UA). As colunas de resposta estão com células vazias prontas para o registro dos dados."),
              DT::DTOutput(ns("tabela_tidy_coleta")))
          }
        )
      ),

      # ABA 3: METODOLOGIA PARA ARTIGO / RELATÓRIO
      bslib::nav_panel("Metodologia para artigo", icon = shiny::icon("paragraph"),
        bslib::card_body(fillable = FALSE,
          shiny::div(class = "obs-card-interno mb-3",
            shiny::h5(shiny::icon("paragraph"), " Seção de Metodologia para Artigo / Relatório"),
            shiny::p(class = "small text-muted mb-2",
              "Texto pré-formatado gerado dinamicamente com base nas decisões declaradas. Pronto para copiar diretamente para a seção de Material e Métodos da sua pesquisa:"
            ),
            shiny::div(
              class = "p-3 bg-light border rounded",
              # Texto em duas colunas fluidas (estilo artigo); abaixo de ~20rem
              # de largura disponível o CSS recolhe automaticamente para 1 coluna.
              style = paste(
                "font-family: Georgia, serif; line-height: 1.6; font-size: 0.95rem; color: #212529;",
                "column-width: 20rem; column-gap: 2.2rem; column-rule: 1px solid #d7e2e6;"
              ),
              shiny::uiOutput(ns("texto_metodologia_artigo"))
            )
          )
        )
      ),

      # ABA 4: MODELO ESTATÍSTICO E CUIDADOS
      bslib::nav_panel("Modelo e cuidados", icon = shiny::icon("calculator"),
        bslib::card_body(fillable = FALSE,
          # No gradiente a aba fica dividida em duas colunas: à esquerda o
          # modelo estatístico e quando usar; à direita os cuidados de coleta
          # e de análise. Nos demais delineamentos vale o empilhamento com o
          # modelo acima dos cuidados de pool.
          if (identical(tipo_resolvido, "longitudinal")) {
            shiny::div(class = "obs-coluna-bloco",
              shiny::uiOutput(ns("card_modelo_estatistico")),
              shiny::div(class = "obs-card-interno obs-cuidados-longitudinal",
                shiny::h5("Cuidados no acompanhamento"),
                shiny::uiOutput(ns("cuidados_longitudinal"))))
          } else if (tipo_resolvido == "impacto") {
            bslib::layout_columns(col_widths = c(6, 6), gap = "20px",
              shiny::div(class = "obs-coluna-bloco obs-impacto-coluna",
                shiny::div(class = "obs-card-interno obs-modelos-impacto",
                  shiny::h5(shiny::icon("calculator"), " Modelos para CI, BA e BACI"),
                  shiny::uiOutput(ns("card_modelo_estatistico")))),
              shiny::div(class = "obs-coluna-bloco obs-impacto-coluna",
                shiny::div(class = "obs-card-interno obs-cuidados-impacto",
                  shiny::h5(shiny::icon("triangle-exclamation"), " Cuidados de interpretação e coleta"),
                  shiny::uiOutput(ns("limites_modelo_impacto")),
                  shiny::tags$ul(class = "ps-3 small",
                    shiny::tags$li("Defina resposta, efeito esperado e área de influência antes da coleta."),
                    shiny::tags$li("Escolha referências comparáveis, fora da influência do impacto; registre ambiente e fonte."),
                    shiny::tags$li("Cubra a variação sazonal e visite impacto e referência em janelas comparáveis."),
                    shiny::tags$li("Subamostras e revisitas não são novos impactos. Pontos no mesmo canal não replicam canais."),
                    shiny::tags$li("Registre covariáveis, tendências prévias, desvios e motivos de ausências.")),
                  shiny::p(class = "small", "O intercepto do sítio liga suas revisitas. Autocorrelação temporal, campanhas compartilhadas e dependência entre ambientes exigem avaliação adicional."),
                  shiny::tags$details(class = "small",
                    shiny::tags$summary("Casos especiais e orientações completas"),
                    shiny::p(class = "mt-2", "CI com uma campanha: lm() pode ser pertinente com uma resposta por sítio independente. BA com um resumo antes e outro depois por sítio independente pode admitir teste t pareado. Com um único sítio, não se estima a variância do efeito aleatório de sítio; é necessária uma análise temporal adequada."),
                    shiny::tags$ul(class = "ps-3", lapply(cuidados_impacto, shiny::tags$li)),
                    shiny::h6("Quando a pergunta é sobre um gradiente"),
                    shiny::p(class = "mb-0", obs_pseudorreplicas)))))
          } else if (identical(tipo_resolvido, "gradiente")) {
            bslib::navset_tab(id = ns("modelo_gradiente"),
              bslib::nav_panel("Modelo e interpretação",
                shiny::uiOutput(ns("card_modelo_estatistico")),
                shiny::p("A família do modelo depende da resposta: contagens, proporções e medidas contínuas podem exigir modelos diferentes. Confira forma da relação, resíduos e dependência espacial antes de interpretar a regressão.")),
              bslib::nav_panel("Cuidados na coleta",
                shiny::tags$ul(class = "mt-3", lapply(cuidados_gradiente[1:4], shiny::tags$li))),
              bslib::nav_panel("Cuidados na análise",
                shiny::tags$ul(class = "mt-3", lapply(cuidados_gradiente[5:7], shiny::tags$li))),
              bslib::nav_panel("Entender as pseudorréplicas", shiny::p(obs_pseudorreplicas)))
          } else {
          shiny::tagList(
            shiny::div(class = "obs-card-interno mb-3",
              shiny::h5(shiny::icon("calculator"), " Modelo Estatístico Recomendado"),
              shiny::uiOutput(ns("card_modelo_estatistico"))
            ),
          shiny::div(class = if (tipo_resolvido == "longitudinal") "obs-card-interno obs-apertado" else "obs-card-interno",
            shiny::h5(shiny::icon("triangle-exclamation"), " Cuidados Metodológicos para o Pesquisador"),
            shiny::tags$ul(class = "mb-0 ps-3",
              shiny::tags$li(class = "mb-2",
                shiny::tags$b("Massa ou volume equitativo: "),
                "Cada item deve contribuir com a mesma quantidade (massa ou volume) para a amostra composta, para que nenhum item domine a mistura."
              ),
              shiny::tags$li(class = "mb-2",
                shiny::tags$b("Indivíduo único: "),
                "Nenhum item pode participar de mais de uma unidade amostral (evita dependência física entre UAs)."
              ),
              shiny::tags$li(class = "mb-2",
                shiny::tags$b("Réplicas analíticas de bancada: "),
                "Réplicas analíticas medem apenas a precisão da bancada e não aumentam o número de unidades amostrais independentes."
              ),
              shiny::tags$li(class = "mb-2", obs_espacamento),
              shiny::tags$li(class = "mb-0",
                shiny::tags$b("Interpretação de variâncias sob pools desiguais: "),
                "As médias podem ser comparadas entre grupos, mas a variabilidade entre UAs não é diretamente comparável quando o pool difere, porque em alguns grupos ela reflete a variação entre itens individuais e em outros a variação entre misturas (Var = σ²/k)."
              )
            )
          )
          )
          },
          if (tipo_resolvido %in% c("transversal_comparativo", "longitudinal"))
            shiny::uiOutput(ns("aviso_comparacao_ambiental"))
        )
      ),
      if (tipo_resolvido != "transversal_comparativo") bslib::nav_panel(
        "Resumo", icon = shiny::icon("clipboard-check"),
        bslib::card_body(fillable = FALSE,
          shiny::h5("Resumo do planejamento"),
          shiny::p(definicao$titulo),
          shiny::uiOutput(ns("resumo_observacional_intro")),
          shiny::div(class = "obs-resumo-grade",
            shiny::tableOutput(ns("resumo_observacional_tabela")),
            shiny::div(class = "obs-resumo-downloads",
              shiny::downloadButton(ns("baixar_planilha"), "Planilha Excel (.xlsx)",
                class = if (tipo_resolvido == "gradiente") "btn-success" else "btn-primary"),
              shiny::downloadButton(ns("baixar_relatorio"), "Relatório Word (.docx)",
                class = if (tipo_resolvido == "gradiente") "btn-primary" else "btn-success"),
              if (!tipo_resolvido %in% c("gradiente", "longitudinal", "impacto")) shiny::downloadButton(ns("baixar_projeto"), "Projeto R (.zip)", class = "btn-success"),
              if (!tipo_resolvido %in% c("gradiente", "longitudinal", "impacto")) shiny::downloadButton(ns("baixar_dicionario"), "Dicionário (.csv)", class = "btn-outline-primary"))),
          shiny::p(class = "small text-muted", definicao$alerta))),
      if (tipo_resolvido == "transversal_comparativo") bslib::nav_panel(
        "Resumo", icon = shiny::icon("clipboard-check"),
        bslib::card_body(fillable = FALSE,
          shiny::h5("Resumo do planejamento"),
          shiny::uiOutput(ns("resumo_transversal_intro")),
          shiny::div(class = "obs-resumo-grade",
            shiny::tableOutput(ns("resumo_transversal_tabela")),
            shiny::div(class = "obs-resumo-downloads",
              shiny::downloadButton(ns("baixar_planilha"), "Planilha Excel (.xlsx)", class = "btn-primary"),
              shiny::downloadButton(ns("baixar_relatorio"), "Relatório Word (.docx)", class = "btn-success"))),
          shiny::p(class = "small text-muted", "O Excel reúne coleta e orientações. O Word reúne a metodologia, o desenho e a prévia da coleta.")
        ))
    )
  )
}

# ---- SERVIDOR DO MÓDULO ------------------------------------------------------

mod_planejamento_observacional_server <- function(id, tipo, ficha_destino_rv = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    tipo_resolvido <- if (tipo %in% c("transversal", "comparativo")) "transversal_comparativo" else tipo
    definicao <- catalogo_delineamentos_observacionais()[[tipo_resolvido]]
    if (is.null(definicao)) stop("Delineamento observacional desconhecido.", call. = FALSE)

    ou_vazio <- function(valor, padrao) {
      if (is.null(valor) || !length(valor) || (is.character(valor) && !nzchar(valor))) padrao else valor
    }

    # O cenário didático não resolve o planejamento pelo aluno: só preenche a
    # estrutura mínima para que ele complete o local, a seleção e a justificativa.
    if (identical(tipo_resolvido, "transversal_comparativo")) {
      shiny::observeEvent(input$usar_exercicio_faixas, {
        shiny::updateTextInput(session, "pergunta",
          value = "A qualidade da água difere entre as faixas ambientais deste sistema estuarino?")
        shiny::updateSelectInput(session, "comparacao_ambiental", selected = "faixas")
        shiny::updateTextInput(session, "fator_nome", value = "faixa_ambiental")
        shiny::updateTextInput(session, "fator_niveis",
          value = "Estuário interno, Estuário externo, Costa")
        shiny::updateTextInput(session, "unidade_item", value = "garrafa de água (1 L)")
        shiny::updateCheckboxInput(session, "usar_fator2", value = FALSE)
        shiny::updateCheckboxInput(session, "registrar_coordenadas", value = TRUE)
        shiny::updateTextInput(session, "espacamento_minimo_m", value = "")
        shiny::updateTextAreaInput(session, "justificativa_espacamento", value = "")
        shiny::updateNumericInput(session, "precisao_gps_m", value = 5)
        shiny::updateNumericInput(session, "n_vars_resposta", value = 3)
        shiny::updateTabsetPanel(session, "etapas", selected = "Definições")
        session$onFlushed(function() {
          for (i in seq_len(3)) {
            shiny::updateNumericInput(session, id_amostra(i), value = 5)
            shiny::updateNumericInput(session, id_amostra(i, TRUE), value = 1)
          }
          shiny::updateTextInput(session, "var_nome_1", value = "salinidade")
          shiny::updateTextInput(session, "var_unidade_1", value = "psu")
          shiny::updateTextInput(session, "var_nome_2", value = "turbidez")
          shiny::updateTextInput(session, "var_unidade_2", value = "NTU")
          shiny::updateTextInput(session, "var_nome_3", value = "oxigenio_dissolvido")
          shiny::updateTextInput(session, "var_unidade_3", value = "mg/L")
        }, once = TRUE)
      }, ignoreInit = TRUE)
    }

    n_longitudinal <- shiny::reactive({
      n <- as.numeric(ou_vazio(input$n_uas, 4))
      perda <- as.numeric(ou_vazio(input$perda_pct, 20)) / 100
      shiny::validate(shiny::need(is.finite(n) && n >= 1 && n == floor(n) &&
        is.finite(perda) && perda >= 0 && perda < 1, "Informe UAs inteiras positivas e perda entre 0 e 99%."))
      as.integer(ceiling(n / (1 - perda)))
    })

    # O tipo de pergunta de impacto (CI, BA ou BACI) só existe neste módulo;
    # fora dele vale o BACI, que é o desenho mais completo.
    tipo_impacto <- shiny::reactive({
      if (!identical(tipo_resolvido, "impacto")) return("baci")
      ou_vazio(input$tipo_impacto, "baci")
    })
    # BA e BACI repetem cada sítio nos momentos; o CI (só depois) não tem eixo de tempo.
    impacto_com_momentos <- shiny::reactive({
      identical(tipo_resolvido, "impacto") && !identical(tipo_impacto(), "ci")
    })

    # Cabeçalho da ficha se houver conexão. Sem ficha enviada, não mostra o
    # aviso "Ainda não há um planejamento atual": a linha some para ganhar espaço.
    output$planejamento_atual <- shiny::renderUI({
      if (is.function(ficha_destino_rv)) {
        ficha <- ficha_destino_rv()
        if (!is.null(ficha) && !is.null(ficha_nome_delineamento(ficha$tipo))) {
          ficha_cabecalho_ui(ficha)
        }
      }
    })

    campanhas_impacto <- shiny::reactive({
      ti <- tipo_impacto()
      n_antes <- if (ti == "ci") 0L else inteiro_impacto(input$impacto_antes, 6)
      n_depois <- inteiro_impacto(input$impacto_depois, 6)
      intervalo <- inteiro_impacto(input$impacto_intervalo, 2)
      inicio <- as.Date(ou_vazio(input$impacto_inicio, "2028-01-01"))
      datas_antes <- if (n_antes) seq(as.Date(ou_vazio(input$impacto_primeira_antes, "2027-01-15")), by = paste(intervalo, "months"), length.out = n_antes) else as.Date(character())
      datas_depois <- seq(as.Date(ou_vazio(input$impacto_primeira_depois, "2028-01-15")), by = paste(intervalo, "months"), length.out = n_depois)
      shiny::validate(shiny::need(all(datas_antes < inicio) && all(datas_depois >= inicio), "As campanhas antes devem preceder o início do impacto; as depois devem começar nessa data ou depois. Ajuste as datas ou o intervalo."))
      data.frame(campanha = c(if (n_antes) sprintf("A%02d", seq_len(n_antes)) else character(), sprintf("D%02d", seq_len(n_depois))),
        periodo = c(rep("Antes", n_antes), rep("Depois", n_depois)),
        data_prevista = as.character(c(datas_antes, datas_depois)), mare_prevista = "", estacao_regional = "", observacoes = "")
    })
    sitios_impacto <- shiny::reactive({
      grupos <- grupos_lista()
      n <- c(inteiro_impacto(input$n_uas, 3), if (tipo_impacto() != "ba") inteiro_impacto(input$n_referencia, 3))
      data.frame(sitio = sprintf("S%02d", seq_len(sum(n))), condicao = rep(grupos, n),
        ambiente = "", fonte_impacto = "", habitat = "", latitude = "", longitude = "", criterio_selecao = "", observacoes = "")
    })
    orientacoes_impacto <- shiny::reactive({
      dic <- dicionario_dados()
      ids <- c("impacto_escala", "impacto_criterios", "impacto_protocolo", "impacto_covariaveis", "impacto_janela", "impacto_registro")
      rbind(data.frame(secao = "Plano", campo = c("Pergunta", ids), orientacao = c(ou_vazio(input$pergunta, "A definir"), vapply(ids, function(id) ou_vazio(input[[id]], "A definir"), character(1)))),
        data.frame(secao = "Colunas de coleta", campo = dic$coluna, orientacao = dic$descricao),
        data.frame(secao = "Sítios", campo = names(sitios_impacto()), orientacao = c("Código permanente usado na coleta", "Condição declarada", "Canal, rio ou ambiente compartilhado; não repita um ambiente como se fossem vários", "Empreendimento ou fonte; vários pontos não são novas fontes", "Características para comparar com referências", "Graus decimais, com referencial nas observações", "Graus decimais", "Critério escrito antes da coleta", "Referencial, mudanças e ocorrências")),
        data.frame(secao = "Campanhas", campo = names(campanhas_impacto()), orientacao = c("Código compartilhado entre sítios na mesma campanha", "Antes ou depois do início do impacto", "Data indicativa AAAA-MM-DD", "Fase de maré planejada", "Defina conforme a região; confira comparabilidade sazonal", "Janela, ordem e desvios")),
        data.frame(secao = "Cuidados", campo = paste("Conferir", seq_along(cuidados_impacto)), orientacao = cuidados_impacto))
    })
    output$sitios_impacto <- DT::renderDT({ DT::datatable(sitios_impacto(), rownames = FALSE, options = list(scrollX = TRUE)) })
    output$campanhas_impacto <- DT::renderDT({ DT::datatable(campanhas_impacto(), rownames = FALSE, options = list(scrollX = TRUE)) })
    output$orientacoes_impacto <- DT::renderDT({ DT::datatable(orientacoes_impacto(), rownames = FALSE, options = list(scrollX = TRUE)) })
    output$avaliacao_impacto <- shiny::renderUI({
      shiny::req(tipo_resolvido == "impacto")
      sitios <- sitios_impacto()
      campanhas <- campanhas_impacto()
      n_sub <- inteiro_impacto(input$impacto_subamostras, 3)
      shiny::div(class = "alert alert-info small",
        shiny::p(sprintf("%d sítios × %d campanhas × %d subamostras = %d linhas. Linhas não são o n de impactos independentes.", nrow(sitios), nrow(campanhas), n_sub, nrow(sitios) * nrow(campanhas) * n_sub)),
        shiny::p(limite_impacto(tipo_impacto())),
        if (any(table(sitios$condicao) == 1L)) shiny::p("Há uma condição com um só sítio. Considere o alcance específico e a possibilidade de um desenho assimétrico com vários ambientes de referência."),
        if (any(table(campanhas$periodo) == 1L)) shiny::p("Uma campanha em um período não caracteriza sua variação temporal."),
        shiny::p("Confira ambientes compartilhados, cobertura sazonal, esforço e tendências prévias. A ficha não certifica o plano."))
    })

    # Vetor de grupos/níveis limpos
    niveis_primeiro_fator <- shiny::reactive({
      # No impacto as "condições" são os sítios impactados e de referência; no
      # BA (antes–depois) não há controle, então só resta a condição de interesse.
      if (identical(tipo_resolvido, "impacto")) {
        if (identical(tipo_impacto(), "ba")) return("Impacto")
        txt <- ou_vazio(input$fator_niveis, "Impacto, Referência")
        partes <- trimws(unlist(strsplit(txt, ",")))
        partes <- partes[nzchar(partes)]
        {
          shiny::validate(shiny::need(length(partes) == 2L && !anyDuplicated(partes), "Informe duas condições distintas: primeiro impacto, depois referência."))
          partes
        }
      } else {
        txt <- ou_vazio(input$fator_niveis, if (tipo_resolvido == "transversal_comparativo") "Tambaqui, Gurijuba, Pescada-amarela, Pescada-corvina, Uritinga" else if (tipo_resolvido == "longitudinal") "Caeté, Emboraí Velho, Quatipuru" else "Tambaqui, Gurijuba, Pescada Amarela, Pargo, Camurupim")
        partes <- trimws(unlist(strsplit(txt, ",")))
        partes <- partes[nzchar(partes)]
        if (!length(partes)) c("Grupo 1", "Grupo 2", "Grupo 3") else partes
      }
    })

    segundo_fator <- shiny::reactive({
      tipo_resolvido == "transversal_comparativo" && isTRUE(input$usar_fator2)
    })
    fator2_coluna <- shiny::reactive({
      nome <- gerar_nome_reduzido(ou_vazio(input$fator2_nome, "sexo"))
      shiny::validate(shiny::need(nzchar(nome) && !nome %in% c(fator_nome_limpo(), "ua", "replica", "pool", "lote_origem", "data_coleta", "observacoes"), "Use um nome distinto para o segundo fator."))
      nome
    })
    combinacoes_transversal <- shiny::reactive({
      primeiro <- unique(niveis_primeiro_fator())
      if (!segundo_fator()) return(data.frame(primeiro = primeiro, segundo = "", rotulo = primeiro))
      fator2_coluna()
      niveis <- unique(trimws(strsplit(ou_vazio(input$fator2_niveis, "Fêmea, Macho"), ",")[[1]]))
      niveis <- niveis[nzchar(niveis)]
      shiny::validate(shiny::need(length(niveis) >= 2, "Informe pelo menos dois níveis para o segundo fator."))
      cruzamento <- expand.grid(segundo = niveis, primeiro = primeiro, stringsAsFactors = FALSE)
      cruzamento$rotulo <- paste(cruzamento$primeiro, cruzamento$segundo, sep = " · ")
      cruzamento
    })
    grupos_lista <- shiny::reactive({
      if (tipo_resolvido == "transversal_comparativo") combinacoes_transversal()$rotulo else niveis_primeiro_fator()
    })
    id_amostra <- function(i, pool = FALSE) {
      paste0(if (pool) "pool_" else "uas_", if (segundo_fator()) "combinacao_" else "grupo_", i)
    }

    fator_nome_limpo <- shiny::reactive({
      txt <- ou_vazio(input$fator_nome, if (identical(tipo_resolvido, "impacto")) "situacao" else if (tipo_resolvido == "longitudinal") "estuario" else "especie")
      gerar_nome_reduzido(txt)
    })

    # No gradiente o eixo é uma variável contínua: nome da coluna e unidade.
    gradiente_coluna <- shiny::reactive({
      gerar_nome_reduzido(ou_vazio(input$gradiente_nome, "distancia_fonte"))
    })
    gradiente_unidade <- shiny::reactive({
      ou_vazio(input$gradiente_unidade, "m")
    })

    # Valores das estações ao longo da faixa: seq() igualmente espaçada (modo
    # padrão) ou lista digitada; a planilha segue a ordem crescente dos valores.
    estacoes_valores <- shiny::reactive({
      if (!identical(tipo_resolvido, "gradiente")) return(numeric())
      modo <- ou_vazio(input$modo_estacoes, "igual")
      if (identical(modo, "livre")) {
        txt <- ou_vazio(input$valores_livres, "50, 100, 150, 200, 250, 300, 350, 400")
        partes <- trimws(unlist(strsplit(txt, ",")))
        valores <- suppressWarnings(as.numeric(partes))
        valores <- valores[is.finite(valores)]
        if (!length(valores)) valores <- c(50, 100, 150, 200, 250)
        return(sort(valores))
      }
      inicio <- as.numeric(ou_vazio(input$estacao_inicio, 50))
      fim <- as.numeric(ou_vazio(input$estacao_fim, 400))
      n <- suppressWarnings(as.integer(ou_vazio(input$n_estacoes, 8)))
      if (identical(modo, "geometrico")) {
        shiny::validate(shiny::need(is.finite(inicio) && is.finite(fim) && inicio > 0 && fim > inicio,
          "Progressão geométrica: informe início positivo e fim maior que o início."))
      }
      if (!is.finite(inicio)) inicio <- 50
      if (!is.finite(fim) || fim <= inicio) fim <- inicio + 350
      if (!is.finite(n) || n < 2L) n <- 8L
      if (identical(modo, "geometrico")) {
        shiny::validate(shiny::need(inicio > 0 && fim > inicio,
          "Progressão geométrica: informe início positivo e fim maior que o início."))
        return(exp(seq(log(inicio), log(fim), length.out = n)))
      }
      seq(inicio, fim, length.out = n)
    })

    # Estes lembretes ajudam a revisar escolhas, sem impor limiares universais.
    output$avaliacao_gradiente <- shiny::renderUI({
      shiny::req(identical(tipo_resolvido, "gradiente"))
      valores <- estacoes_valores()
      avisos <- c(sprintf("%d estações planejadas; faixa de %g a %g %s. O total informado não é um cálculo de tamanho amostral.",
        length(valores), min(valores), max(valores), gradiente_unidade()),
        "Confira extremos, valores intermediários e distância física entre estações. O espaçamento escolhido não comprova independência.")
      if (length(valores) < 5L) avisos <- c(avisos, "Poucas estações podem dificultar descrever a forma da relação; justifique o esforço com literatura e estudo piloto.")
      if (anyDuplicated(valores)) avisos <- c(avisos, "Há valores repetidos no eixo: só conte como réplicas estações distintas cuja independência seja defensável.")
      if (identical(input$gradiente_fontes, "Uma fonte identificada")) avisos <- c(avisos,
        "Uma fonte limita o alcance da conclusão a esse sistema. Estações de referência ajudam a contextualizar, mas não demonstram causalidade.")
      if (identical(input$gradiente_fontes, "Várias fontes / sistemas")) avisos <- c(avisos,
        "Várias fontes exigem identificação do sistema e organização das estações dentro dele. Esta primeira ficha não gera automaticamente essa estrutura.")
      if (identical(input$gradiente_ambiente, "Rio / canal")) avisos <- c(avisos,
        "Confira afluentes, zonas de mistura e mudanças naturais do rio. Não adote uma distância universal de exclusão sem justificativa local.")
      if (identical(input$gradiente_campanhas, "Revisitas às mesmas estações")) avisos <- c(avisos,
        "Revisitas são medidas repetidas, não novas estações independentes. A ficha atual representa uma campanha; o registro temporal precisa ser adaptado.")
      avisos <- c(avisos, "Meça o gradiente em campo e confira se covariáveis variam junto com ele. Subamostras e réplicas de bancada não aumentam o número de estações.",
        "Orientação inicial: confirme as decisões e a análise na literatura específica antes da coleta.")
      shiny::tags$ul(class = "small", lapply(avisos, shiny::tags$li))
    })

    # Códigos sequenciais das estações (E01, E02, ...), na ordem do gradiente.
    estacoes_codigos <- shiny::reactive({
      sprintf("E%02d", seq_along(estacoes_valores()))
    })

    # Unidades que recebem pool: os grupos nos delineamentos por comparação;
    # as estações no estudo de gradiente.
    unidades_pool <- shiny::reactive({
      if (identical(tipo_resolvido, "gradiente")) estacoes_codigos() else grupos_lista()
    })

    # Marca do aviso de poucas estações: com menos de 5 pontos fica difícil
    # enxergar a forma da relação ao longo do gradiente (aviso não bloqueia).
    poucas_estacoes <- shiny::reactive({
      identical(tipo_resolvido, "gradiente") && length(estacoes_valores()) < 5L
    })

    # Vetor de momentos limpos (longitudinal e impacto BA/BACI; vem do campo de texto).
    momentos_lista <- shiny::reactive({
      if (tipo_resolvido == "impacto") return(campanhas_impacto()$campanha)
      padrao <- if (identical(tipo_resolvido, "impacto")) "antes, depois" else "0, 60, 120, 180 dias"
      txt <- ou_vazio(input$momentos, padrao)
      partes <- trimws(unlist(strsplit(txt, ",")))
      partes <- partes[nzchar(partes)]
      if (!length(partes)) {
        if (identical(tipo_resolvido, "impacto")) c("antes", "depois") else c("início", "fim")
      } else {
        if (tipo_resolvido == "longitudinal") shiny::validate(shiny::need(
          length(partes) >= 2 && !anyDuplicated(partes), "Informe pelo menos dois momentos distintos."))
        partes
      }
    })

    dias_longitudinal <- shiny::reactive({
      dias <- suppressWarnings(as.numeric(sub("\\s*dias?\\s*$", "", momentos_lista())))
      shiny::validate(shiny::need(all(is.finite(dias)) && all(dias >= 0) &&
        all(diff(dias) > 0), "Informe momentos em dias crescentes, por exemplo: 0, 60, 120, 180 dias."))
      dias
    })
    # Sorteios locais preservam a semente global e ficam estáveis entre downloads.
    sortear_visitas <- function(n, deslocamento = 0L) {
      tinha <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
      if (tinha) anterior <- get(".Random.seed", envir = .GlobalEnv)
      on.exit(if (tinha) assign(".Random.seed", anterior, envir = .GlobalEnv)
        else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv))
      semente <- as.numeric(ou_vazio(input$semente_visitas, 2027))
      shiny::validate(shiny::need(is.finite(semente) && semente >= 1 &&
        semente == floor(semente) && semente < .Machine$integer.max - 100000,
        "Informe uma semente inteira positiva dentro do limite do R."))
      set.seed(as.integer(semente) + deslocamento)
      sample.int(n)
    }
    calendario_longitudinal <- shiny::reactive({
      grupos <- grupos_lista()
      shiny::validate(shiny::need(length(grupos) >= 2 && !anyDuplicated(grupos),
        "Informe pelo menos dois grupos distintos para a comparação longitudinal."))
      dias <- dias_longitudinal()
      janela <- as.integer(ou_vazio(input$janela_dias, 3))
      shiny::validate(shiny::need(is.finite(janela) && janela >= 1,
        "Informe uma janela de visita positiva."))
      inicio <- as.Date(ou_vazio(input$data_inicial, "2027-02-01"))
      shiny::validate(shiny::need(!is.na(inicio), "Informe a data inicial."))
      do.call(rbind, lapply(seq_along(dias), function(j) {
        ordem <- sortear_visitas(length(grupos), j)
        deslocamento <- floor((ordem - 1) * janela / length(grupos))
        data.frame(grupo = grupos, momento = momentos_lista()[j],
          dia_previsto = dias[j], ordem_grupo = ordem,
          deslocamento_dias = deslocamento,
          data_prevista = if (identical(input$relogio, "individual")) "" else
            as.character(inicio + dias[j] + deslocamento), stringsAsFactors = FALSE)
      }))
    })
    ordem_longitudinal <- function(df, chave) {
      ordem <- integer(nrow(df))
      for (i in seq_along(unique(chave))) {
        idx <- which(chave == unique(chave)[i])
        ordem[idx] <- sortear_visitas(length(idx), 1000L + i)
      }
      ordem
    }
    unidades_longitudinal <- shiny::reactive({
      coleta <- tabela_coleta_dados()
      colunas <- c(unidade_coluna_nome(), fator_nome_limpo(), "replica")
      unidades <- unique(coleta[colunas])
      unidades$data_inicio <- if (identical(input$relogio, "individual")) "" else
        as.character(as.Date(ou_vazio(input$data_inicial, "2027-02-01")))
      extras <- trimws(strsplit(ou_vazio(input$caracteristicas_base, "area, densidade_inicial"), ",")[[1]])
      extras <- c(ou_vazio(input$agrupamento, "local_origem"), extras[nzchar(extras)])
      extras <- vapply(extras[nzchar(extras)], gerar_nome_reduzido, character(1))
      shiny::validate(shiny::need(!anyDuplicated(c(names(unidades), extras)),
        "Use nomes distintos para características de partida e identificação."))
      for (nome in extras) unidades[[nome]] <- ""
      unidades
    })
    shiny::observeEvent(input$conferir_locais, {
      shiny::updateTabsetPanel(session, "etapas", selected = "Ficha do delineamento")
      shiny::updateTabsetPanel(session, "planilhas_ficha", selected = "Unidades e características de partida")
    }, ignoreInit = TRUE)
    alcance_comparacao <- shiny::reactive({
      switch(ou_vazio(input$comparacao_ambiental, "na"),
        faixas = "As diferenças serão interpretadas como associadas às faixas deste sistema estudado, no período informado. As estações descrevem a variação dentro do sistema; não são réplicas de outros estuários ou costas.",
        especificos = "As diferenças serão interpretadas como associadas aos ambientes estudados, nos locais e no período de coleta informados; não identificarão qual variável ambiental as produziu.",
        categorias = "A associação entre categoria e resposta dependerá de ambientes distintos em cada categoria. O número e a seleção desses ambientes deverão ser descritos no projeto; UAs de cultivo não serão usadas como sua contagem. Outras características que acompanham a categoria poderão contribuir para as diferenças, na região e no período estudados.",
        "As diferenças serão interpretadas como associadas aos grupos estudados, considerando os locais, o período e os critérios de seleção informados, sem estabelecer uma causa específica.")
    })
    output$aviso_comparacao_ambiental <- shiny::renderUI({
      if (identical(input$comparacao_ambiental, "categorias"))
        shiny::div(class = "alert alert-warning small mt-2 mb-0",
          "Para comparar categorias, a réplica é o ambiente. Vários tanques no mesmo reservatório descrevem aquele reservatório, mas não substituem outros reservatórios da mesma categoria.")
    })
    orientacoes_contexto <- shiny::reactive({
      data.frame(secao = "Planejamento", campo = c("Tipo de unidade", "Alcance da comparação", "Terminologia"),
        orientacao = c(if (identical(input$tipo_unidade, "instaladas")) obs_instaladas else
          if (identical(input$tipo_unidade, "naturais")) "Unidades naturais." else "Tipo de unidade não informado.",
          alcance_comparacao(), obs_termos))
    })
    cuidados_longitudinal <- c(
      "Mesmo código em todas as visitas significa a mesma UA. Unidades novas em cada momento caracterizam cortes transversais sucessivos.",
      obs_medidas,
      obs_espacamento,
      "Registre o ambiente e os locais compartilhados. A UA da comparação depende da pergunta; unidades internas não devem ser contadas automaticamente como ambientes distintos.",
      "Visite todos os grupos na mesma janela curta; sorteie grupos e UAs e mantenha protocolo, horário e maré comparáveis.",
      "Anote as características de partida. Desequilíbrios podem exigir recrutamento mais equilibrado ou covariáveis na análise.",
      obs_ausencias,
      "Perdas podem estar relacionadas ao resultado: despesca antecipada pode retirar justamente os viveiros com maior crescimento.",
      "Em inícios diferentes, conte os dias desde o início de cada UA e guarde a data real. Preencha data_inicio na aba unidades; o calendário informa os deslocamentos previstos.",
      "Investigue grupo × tempo, respeitando a dependência dentro da UA e agrupamentos de origem. Não conte linhas como UAs nem faça um teste separado a cada momento.",
      "Mais de seis momentos pede revisão do escopo com Monitoramento. O aumento por perdas não calcula tamanho amostral ou poder."
    )
    orientacoes_longitudinal <- shiny::reactive({
      dic <- dicionario_dados()
      rbind(orientacoes_contexto(), data.frame(secao = "Planejamento", campo = c("Pergunta", "Medidas previstas por visita", "Linha de base", "Características de partida", "Referência temporal", "Semente"),
        orientacao = c(ou_vazio(input$pergunta, "A definir"), as.character(ou_vazio(input$n_medidas, 60)),
          as.character(ou_vazio(input$linha_base, TRUE)), ou_vazio(input$caracteristicas_base, "A definir"),
          ou_vazio(input$relogio, "comum"), as.character(ou_vazio(input$semente_visitas, 2027)))),
        data.frame(secao = "Colunas", campo = dic$coluna, orientacao = dic$descricao),
        data.frame(secao = "Cuidados", campo = paste("Cuidado", seq_along(cuidados_longitudinal)), orientacao = cuidados_longitudinal))
    })
    output$unidades_longitudinal <- DT::renderDT({
      DT::datatable(unidades_longitudinal(), rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10))
    })
    output$orientacoes_longitudinal <- DT::renderDT({
      DT::datatable(orientacoes_longitudinal(), rownames = FALSE,
        colnames = c("Seção", "Campo", "Orientação"),
        class = "display compact cell-border stripe",
        options = list(scrollX = TRUE, pageLength = 10,
          language = list(search = "Buscar:", lengthMenu = "Mostrar _MENU_ linhas",
            info = "Mostrando _START_ a _END_ de _TOTAL_ orientações", infoEmpty = "Nenhuma orientação",
            zeroRecords = "Nenhuma orientação encontrada", infoFiltered = "(de _MAX_ orientações)",
            paginate = list(previous = "Anterior", "next" = "Próxima"))))
    })
    output$calendario_longitudinal <- shiny::renderTable({ calendario_longitudinal() }, rownames = FALSE)
    output$cuidados_longitudinal <- shiny::renderUI({ shiny::tags$ul(lapply(cuidados_longitudinal, shiny::tags$li)) })
    output$avisos_longitudinal <- shiny::renderUI({
      avisos <- character()
      dias <- dias_longitudinal()
      if (length(dias) > 6) avisos <- c(avisos, "Mais de seis momentos: confira se a pergunta pertence ao Monitoramento.")
      if (isTRUE(ou_vazio(input$linha_base, TRUE)) && dias[1] != 0) avisos <- c(avisos, "Linha de base marcada: considere incluir o momento 0 ou justifique o primeiro momento.")
      if (length(dias) > 2 && length(unique(diff(dias))) > 1) avisos <- c(avisos, "Intervalos desiguais: justifique e considere o tempo real na análise.")
      if (ou_vazio(input$janela_dias, 3) > 3) avisos <- c(avisos, "Janela maior que três dias: confira se os grupos permanecem comparáveis.")
      if (ou_vazio(input$janela_dias, 3) > min(diff(dias))) avisos <- c(avisos, "As janelas dos momentos podem se sobrepor. Revise o calendário.")
      shiny::tagList(shiny::p(class = "small", sprintf("Iniciar com %d UAs por grupo; %d no total. A previsão de perdas é uma suposição do planejamento.", n_longitudinal(), n_longitudinal() * length(grupos_lista()))),
        shiny::tags$ul(class = "small text-warning", lapply(avisos, shiny::tags$li)))
    })

    # Coluna do identificador da unidade: no longitudinal é o campo próprio; no
    # impacto é o sítio (repetido nos momentos) ou, no CI, a UA de sempre.
    unidade_coluna_nome <- shiny::reactive({
      if (identical(tipo_resolvido, "longitudinal")) {
        gerar_nome_reduzido(ou_vazio(input$coluna_unidade, "mesa"))
      } else if (identical(tipo_resolvido, "impacto")) {
        gerar_nome_reduzido(ou_vazio(input$coluna_unidade, "sitio"))
      } else {
        "ua"
      }
    })

    uas_por_grupo <- shiny::reactive({
      grupos <- grupos_lista()
      valores <- vapply(seq_along(grupos), function(i) {
        valor <- if (tipo_resolvido == "transversal_comparativo") input[[id_amostra(i)]] else input$n_uas
        valor <- as.numeric(ou_vazio(valor, 5))
        shiny::validate(shiny::need(is.finite(valor) && valor >= 1 && valor == floor(valor), "Informe um número inteiro positivo de UAs por categoria."))
        as.integer(valor)
      }, integer(1))
      stats::setNames(valores, grupos)
    })

    output$ui_amostra_categorias <- shiny::renderUI({
      grupos <- grupos_lista()
      combinacoes <- combinacoes_transversal()
      exemplo <- c(Tambaqui = 2, Gurijuba = 1, `Pescada-amarela` = 1, `Pescada-corvina` = 3, Uritinga = 3)
      linhas <- lapply(seq_along(grupos), function(i) {
        categoria <- combinacoes$primeiro[i]
        padrao <- if (categoria %in% names(exemplo)) exemplo[[categoria]] else 1
        shiny::tags$tr(
          shiny::tags$td(categoria),
          if (segundo_fator()) shiny::tags$td(combinacoes$segundo[i]),
          shiny::tags$td(shiny::numericInput(ns(id_amostra(i)), NULL,
            value = shiny::isolate(ou_vazio(input[[id_amostra(i)]], 5)), min = 1, step = 1, width = "100%")),
          shiny::tags$td(shiny::numericInput(ns(id_amostra(i, TRUE)), NULL,
            value = shiny::isolate(ou_vazio(input[[id_amostra(i, TRUE)]], padrao)), min = 1, max = 50, step = 1, width = "100%"))
        )
      })
      shiny::tags$table(class = "obs-amostra-tabela",
        shiny::tags$thead(shiny::tags$tr(shiny::tags$th(ou_vazio(input$fator_nome, "Categoria")),
          if (segundo_fator()) shiny::tags$th(ou_vazio(input$fator2_nome, "Segundo fator")),
          shiny::tags$th("UAs"), shiny::tags$th(paste0("Unidades físicas por UA (", ou_vazio(input$unidade_item, "item a definir"), ")")))),
        shiny::tags$tbody(linhas))
    })

    # Campos de pool desigual por unidade (grupo ou estação)
    output$ui_pools_grupos <- shiny::renderUI({
      unidades <- unidades_pool()
      gradiente <- identical(tipo_resolvido, "gradiente")
      impacto <- identical(tipo_resolvido, "impacto")
      valores <- if (gradiente) estacoes_valores() else NULL
      if (gradiente) {
        # Uma estação por linha, como as categorias do transversal. Arredondar
        # aqui melhora a leitura sem alterar os valores usados na planilha.
        linhas <- lapply(seq_along(unidades), function(i) {
          campo <- paste0("pool_grupo_", i)
          shiny::tags$tr(
            shiny::tags$td(style = "color: #0F3B5F; font-weight: 600;", unidades[i]),
            shiny::tags$td(format(round(valores[i], 2), trim = TRUE, decimal.mark = ",", nsmall = 0)),
            shiny::tags$td(shiny::numericInput(ns(campo), NULL,
              value = shiny::isolate(ou_vazio(input[[campo]], 1)),
              min = 1, max = 50, step = 1, width = "100%")))
        })
        return(shiny::tags$table(class = "obs-amostra-tabela obs-estacoes-tabela",
          shiny::tags$thead(shiny::tags$tr(shiny::tags$th("Estação / local"),
            shiny::tags$th(paste0("Valor (", gradiente_unidade(), ")")),
            shiny::tags$th("Itens por estação (pool)"))),
          shiny::tags$tbody(linhas)))
      }
      padroes_bexiga <- if (tipo_resolvido == "transversal_comparativo") {
        c(Tambaqui = 2, Gurijuba = 1, `Pescada-amarela` = 1, `Pescada-corvina` = 3, Uritinga = 3)
      } else c(Tambaqui = 1, Gurijuba = 3, `Pescada Amarela` = 2, Pargo = 1, Camurupim = 1)

      campos <- lapply(seq_along(unidades), function(i) {
        u <- unidades[i]
        # No gradiente e no impacto o pool padrão de cada unidade é 1; nos
        # grupos comparativos valem os padrões das espécies de bexiga natatória.
        val_default <- if (gradiente || impacto) 1 else if (u %in% names(padroes_bexiga)) padroes_bexiga[[u]] else if (i == 2) 3 else 1
        shiny::div(class = "border rounded p-2 bg-light text-center",
          shiny::tags$b(class = "d-block text-truncate small mb-1", title = u, u),
          # Sob o código da estação aparece o valor dela no gradiente.
          if (gradiente) shiny::tags$span(class = "d-block small text-muted mb-1", paste(valores[i], gradiente_unidade())),
          shiny::numericInput(
            ns(paste0("pool_grupo_", i)), NULL,
            value = val_default, min = 1, max = 50, step = 1, width = "100%"
          )
        )
      })
      shiny::div(class = "obs-pool-grid", campos)
    })

    # Vetor numérico de pools por unidade (grupo ou estação)
    pools_por_grupo <- shiny::reactive({
      if (tipo_resolvido == "longitudinal") return(stats::setNames(rep(1L, length(grupos_lista())), grupos_lista()))
      unidades <- unidades_pool()
      if (!tipo_resolvido %in% c("transversal_comparativo", "gradiente") && (is.null(input$tipo_pool) || identical(input$tipo_pool, "igual"))) {
        val <- as.integer(ou_vazio(input$pool_unico, 1))
        return(stats::setNames(rep(val, length(unidades)), unidades))
      }
      gradiente <- identical(tipo_resolvido, "gradiente")
      impacto <- identical(tipo_resolvido, "impacto")
      vals <- vapply(seq_along(unidades), function(i) {
        padrao <- if (!gradiente && !impacto && i == 2) 3 else 1
        if (tipo_resolvido == "transversal_comparativo") {
          exemplo <- c(Tambaqui = 2, Gurijuba = 1, `Pescada-amarela` = 1, `Pescada-corvina` = 3, Uritinga = 3)
          categoria <- combinacoes_transversal()$primeiro[i]
          padrao <- if (categoria %in% names(exemplo)) exemplo[[categoria]] else 1
        }
        campo <- if (tipo_resolvido == "transversal_comparativo") id_amostra(i, TRUE) else paste0("pool_grupo_", i)
        valor <- as.numeric(ou_vazio(input[[campo]], padrao))
        shiny::validate(shiny::need(is.finite(valor) && valor >= 1 && valor == floor(valor), "Informe um número inteiro positivo de itens por UA."))
        as.integer(valor)
      }, integer(1))
      stats::setNames(vals, unidades)
    })

    # Verificação de homogeneidade do pool
    pools_iguais <- shiny::reactive({
      pools <- pools_por_grupo()
      length(unique(pools)) <= 1
    })

    # Alerta visual de diagnóstico
    output$alerta_pool_diagnostico <- shiny::renderUI({
      if (identical(tipo_resolvido, "transversal_comparativo")) {
        resumo <- resumo_transversal()
        return(shiny::div(class = "alert alert-light border py-2 px-3 small mb-0",
          style = "border-left: 4px solid #2E7D8F !important;",
          shiny::tags$b("Confira a composição das UAs. "),
          sprintf("%d %s: %d UAs e %d unidades físicas previstas (%s). ", nrow(resumo), if (segundo_fator()) "combinações de níveis" else "categorias", sum(resumo$UAs), sum(resumo[["Itens previstos"]]), ou_vazio(input$unidade_item, "item a definir")),
          if (segundo_fator()) "A quantidade de UAs vale para cada combinação, por exemplo, para cada espécie e sexo. Cada pool deve reunir itens da mesma combinação; não misture categorias dos fatores estudados. ",
          sprintf("Pool = 1 indica uma unidade física (%s); pool maior que 1 indica uma amostra composta. ", ou_vazio(input$unidade_item, "item a definir")),
          "O número de UAs não inclui repetições de bancada. Pools diferentes podem alterar a variabilidade; confirme a massa disponível para todos os ensaios."))
      }
      # No gradiente este espaço é o resumo do plano: estações, linhas e a
      # análise indicada (regressão), com aviso quando há poucas estações.
      if (identical(tipo_resolvido, "gradiente")) {
        n <- length(estacoes_valores())
        coluna <- gradiente_coluna()
        if (poucas_estacoes()) {
          return(shiny::div(class = "alert alert-warning py-2 px-3 small mb-0",
            style = "border-left: 4px solid #E89B3C !important;",
            shiny::tags$b(sprintf("⚠ Resumo do plano: %d estações = %d linhas. ", n, n)),
            "Poucos pontos dificultam enxergar a forma da relação ao longo do gradiente. ",
            "Análise indicada: ", shiny::tags$b(sprintf("regressão da resposta contra %s", coluna)), "."
          ))
        }
        return(shiny::div(class = "alert alert-success py-2 px-3 small mb-0",
          shiny::tags$b(sprintf("✓ Resumo do plano: %d estações = %d linhas. ", n, n)),
          "Análise indicada: ", shiny::tags$b(sprintf("regressão da resposta contra %s", coluna)), "."
        ))
      }
      # No impacto a análise indicada depende do tipo de pergunta (CI, BA ou
      # BACI), então o diagnóstico do pool aponta para o modelo da aba 4 em vez
      # de cravar uma ANOVA específica.
      if (identical(tipo_resolvido, "impacto")) {
        pools <- pools_por_grupo()
        iguais <- pools_iguais()
        analise_txt <- switch(tipo_impacto(),
          ci = "teste t de amostras independentes (sítios impactados × referência, depois da mudança)",
          ba = "teste t pareado (antes × depois nos mesmos sítios)",
          baci = "interação local × tempo (mudança nos sítios impactados contra a dos de referência)"
        )
        if (iguais) {
          return(shiny::div(class = "alert alert-success py-2 px-3 small mb-0",
            shiny::tags$b(sprintf("✓ Pools homogêneos (k = %d itens por UA). ", pools[1])),
            "A variabilidade não foi distorcida pelo tamanho da mistura. ",
            "Análise indicada: ", shiny::tags$b(analise_txt), "."
          ))
        }
        return(shiny::div(class = "alert alert-warning py-2 px-3 small mb-0",
          style = "border-left: 4px solid #E89B3C !important;",
          shiny::tags$b("⚠ Atenção: unidades amostrais com pools diferentes. "),
          "A média de uma amostra composta varia menos quanto mais itens ela reúne (Var = σ²/k). ",
          "Análise indicada: ", shiny::tags$b(analise_txt),
          ", com a correção de Welch se as variâncias diferirem."
        ))
      }
      pools <- pools_por_grupo()
      iguais <- pools_iguais()
      
      if (iguais) {
        k <- pools[1]
        shiny::div(class = "alert alert-success py-2 px-3 small mb-0",
          shiny::tags$b("✓ Pools homogêneos (k = ", k, " itens por UA em todos os grupos). "),
          "A variabilidade intra-grupo não foi distorcida pelo tamanho amostral da mistura. ",
          "Análise indicada: ", shiny::tags$b("ANOVA de um fator clássica"), "."
        )
      } else {
        shiny::div(class = "alert alert-warning py-2 px-3 small mb-0",
          style = "border-left: 4px solid #E89B3C !important;",
          shiny::tags$b("⚠ Atenção: unidades amostrais com pools diferentes. "),
          "A média de uma amostra composta varia menos quanto mais itens ela reúne (Var = σ²/k), gerando heterocedasticidade estrutural. ",
          shiny::tags$br(),
          "Análise recomendada: ", shiny::tags$b("ANOVA de Welch com pós-teste de Games-Howell"), ", que não exige variâncias iguais."
        )
      }
    })

    # Títulos numerados dos cartões do impacto: o CI esconde o cartão de
    # momentos, então o número do pool e das variáveis muda conforme o tipo.
    output$titulo_pool_impacto <- shiny::renderUI({
      num <- if (identical(tipo_impacto(), "ci")) " 2." else " 3."
      shiny::h5(shiny::icon("layer-group"), num, " Composição da Unidade Amostral (Pool)")
    })
    output$titulo_variaveis_impacto <- shiny::renderUI({
      shiny::h5(shiny::icon("list-check"), " 3. Variáveis de Resposta")
    })

    # Campos de variáveis de resposta
    output$ui_vars_resposta_campos <- shiny::renderUI({
      n_vars <- max(1L, min(10L, as.integer(ou_vazio(input$n_vars_resposta, 2))))
      # No delineamento de impacto a resposta é uma condição do ambiente medida
      # nos sítios, então as sugestões acompanham o tipo de plano: variáveis
      # clássicas de qualidade de água, e não as de bancada dos comparativos.
      if (identical(tipo_resolvido, "impacto")) {
        padroes_nome <- c("oxigenio_dissolvido", "turbidez", "condutividade", "amonia_total", "solidos_suspensos")
        padroes_unid <- c("mg/L", "NTU", "µS/cm", "mg/L", "mg/L")
      } else if (tipo_resolvido == "longitudinal") {
        padroes_nome <- c("altura", "sobrevivencia")
        padroes_unid <- c("mm", "%")
      } else if (tipo_resolvido == "gradiente") {
        padroes_nome <- c("abundancia", "biomassa")
        padroes_unid <- c("ind/unidade de esforço", "g/unidade de esforço")
      } else {
        padroes_nome <- c("colesterol", "lipideos_totais", "umidade", "proteina_bruta", "cinzas")
        padroes_unid <- c("mg/100g", "g", "%", "%", "%")
      }
      
      itens <- lapply(seq_len(n_vars), function(i) {
        nome_sug <- if (i <= length(padroes_nome)) padroes_nome[i] else paste0("resposta_", i)
        unid_sug <- if (i <= length(padroes_unid)) padroes_unid[i] else "unidade"

        shiny::div(class = "obs-var-linha border rounded bg-light",
          shiny::tags$span(class = "obs-var-rotulo", i),
          shiny::textInput(ns(paste0("var_nome_", i)), NULL,
            value = nome_sug, placeholder = "Nome da coluna", width = "100%"),
          shiny::textInput(ns(paste0("var_unidade_", i)), NULL,
            value = unid_sug, placeholder = "Unidade", width = "100%")
        )
      })
      # Rótulos das colunas aparecem uma única vez, no topo da lista compacta.
      cabecalho <- shiny::div(class = "obs-var-cab",
        shiny::tags$span(class = "obs-var-rotulo", ""),
        shiny::tags$span(class = "small text-muted", style = "width: 100%;", "Nome da coluna"),
        shiny::tags$span(class = "small text-muted", style = "width: 100%;", "Unidade de medida")
      )
      do.call(shiny::tagList, c(list(cabecalho), itens))
    })

    # Tabela Tidy Completa
    tabela_coleta_dados <- shiny::reactive({
      # No gradiente há uma linha por estação, na ordem crescente dos valores,
      # com a coluna do gradiente preenchida e as respostas vazias.
      if (tipo_resolvido == "impacto") {
        sitios <- sitios_impacto()
        campanhas <- campanhas_impacto()
        n_sub <- inteiro_impacto(input$impacto_subamostras, 3)
        indices <- expand.grid(subamostra = seq_len(n_sub), campanha_idx = seq_len(nrow(campanhas)), sitio_idx = seq_len(nrow(sitios)))
        df <- data.frame(sitio = sitios$sitio[indices$sitio_idx], stringsAsFactors = FALSE)
        names(df) <- unidade_coluna_nome()
        if (tipo_impacto() != "ba") df[[fator_nome_limpo()]] <- sitios$condicao[indices$sitio_idx]
        df$campanha <- campanhas$campanha[indices$campanha_idx]
        df$periodo <- campanhas$periodo[indices$campanha_idx]
        df$data_prevista <- campanhas$data_prevista[indices$campanha_idx]
        df$subamostra <- indices$subamostra
        df$data_real <- ""
        df$esforco_real <- ""
        df$observacoes <- ""
        shiny::validate(shiny::need(!anyDuplicated(names(df)), "Use nomes distintos para sítio, condição e colunas de coleta."))
      } else if (identical(tipo_resolvido, "gradiente")) {
        valores <- estacoes_valores()
        codigos <- estacoes_codigos()
        pools <- pools_por_grupo()
        df <- data.frame(
          estacao = codigos,
          valor_gradiente = valores,
          pool = unname(pools[codigos]),
          stringsAsFactors = FALSE
        )
        names(df)[names(df) == "valor_gradiente"] <- gradiente_coluna()
      } else if (identical(tipo_resolvido, "impacto") && identical(tipo_impacto(), "ba")) {
        # Antes–Depois (BA): só os sítios impactados, repetidos nos momentos.
        # Não há coluna de condição — o próprio sítio é o seu controle.
        n_sitios <- max(1L, as.integer(ou_vazio(input$n_uas, 3)))
        pools <- pools_por_grupo()
        unidade_col <- unidade_coluna_nome()
        df <- data.frame(
          ua = seq_len(n_sitios),
          pool = rep(unname(pools[1]), n_sitios),
          stringsAsFactors = FALSE
        )
        momentos <- momentos_lista()
        df <- df[rep(seq_len(nrow(df)), each = length(momentos)), , drop = FALSE]
        df$momento <- rep(momentos, times = n_sitios)
        rownames(df) <- NULL
        names(df)[names(df) == "ua"] <- unidade_col
      } else {
      grupos <- grupos_lista()
      n_uas <- max(1L, as.integer(ou_vazio(input$n_uas, if (identical(tipo_resolvido, "impacto")) 3 else 5)))
      if (tipo_resolvido == "longitudinal") n_uas <- n_longitudinal()
      fator_col <- fator_nome_limpo()
      pools <- pools_por_grupo()

      quantidades <- if (tipo_resolvido == "transversal_comparativo") unname(uas_por_grupo()[grupos]) else rep(n_uas, length(grupos))
      total_linhas <- sum(quantidades)
      df <- data.frame(
        ua = seq_len(total_linhas),
        fator = rep(grupos, times = quantidades),
        replica = unlist(lapply(quantidades, seq_len), use.names = FALSE),
        pool = rep(unname(pools[grupos]), times = quantidades),
        stringsAsFactors = FALSE
      )
      names(df)[names(df) == "fator"] <- fator_col
      # Quando o plano pede localização, cada UA recebe as coordenadas previstas
      # e a precisão exigida. Os valores ficam em branco até a definição em mapa
      # ou em campo; não são coordenadas inventadas pela CatalyseR.
      if (identical(tipo_resolvido, "transversal_comparativo") &&
          isTRUE(ou_vazio(input$registrar_coordenadas, FALSE))) {
        df$latitude_wgs84 <- ""
        df$longitude_wgs84 <- ""
        df$precisao_gps_m <- as.integer(ou_vazio(input$precisao_gps_m, 5))
      }
      if (segundo_fator()) {
        combinacoes <- combinacoes_transversal()
        df[[fator_col]] <- rep(combinacoes$primeiro, times = quantidades)
        df[[fator2_coluna()]] <- rep(combinacoes$segundo, times = quantidades)
        colunas <- c("ua", fator_col, fator2_coluna(), "replica", "pool")
        if (isTRUE(ou_vazio(input$registrar_coordenadas, FALSE))) {
          colunas <- c(colunas, "latitude_wgs84", "longitude_wgs84", "precisao_gps_m")
        }
        df <- df[colunas]
      }

      # No longitudinal e no impacto (BA ou BACI) a planilha fica longa: cada
      # unidade repete a mesma linha de identificação em todos os momentos, com
      # o mesmo id. No impacto CI (só depois) a planilha fica como o transversal.
      repetir_momentos <- identical(tipo_resolvido, "longitudinal") ||
        (identical(tipo_resolvido, "impacto") && impacto_com_momentos())
      if (repetir_momentos) {
        momentos <- momentos_lista()
        df <- df[rep(seq_len(nrow(df)), each = length(momentos)), , drop = FALSE]
        df$momento <- rep(momentos, times = total_linhas)
        rownames(df) <- NULL
        names(df)[names(df) == "ua"] <- unidade_coluna_nome()
      } else if (identical(tipo_resolvido, "impacto")) {
        # CI: o identificador da UA é o sítio, sem eixo de tempo.
        names(df)[names(df) == "ua"] <- unidade_coluna_nome()
      }
      }

      if (tipo_resolvido == "longitudinal") {
        shiny::validate(shiny::need(!anyDuplicated(c(unidade_coluna_nome(), fator_col,
          "replica", "momento", "data_prevista", "ordem_grupo", "ordem_visita", "data_real", "n_medidas", "status_ua", "motivo_ausencia", "observacoes")),
          "Use nomes distintos para o identificador, fator e colunas de acompanhamento."))
        df$pool <- NULL
        calendario <- calendario_longitudinal()
        chave <- paste(df[[fator_col]], df$momento)
        idx <- match(chave, paste(calendario$grupo, calendario$momento))
        df$data_prevista <- calendario$data_prevista[idx]
        df$ordem_grupo <- calendario$ordem_grupo[idx]
        df$ordem_visita <- ordem_longitudinal(df, chave)
        df$data_real <- ""
        df$n_medidas <- ""
        df$status_ua <- ""
        df$motivo_ausencia <- ""
        df$observacoes <- ""
      }

      n_vars <- max(1L, min(10L, as.integer(ou_vazio(input$n_vars_resposta, 2))))
      for (i in seq_len(n_vars)) {
        col_nome <- gerar_nome_reduzido(ou_vazio(input[[paste0("var_nome_", i)]], paste0("resposta_", i)))
        if (tipo_resolvido %in% c("transversal_comparativo", "longitudinal", "impacto") &&
            col_nome %in% c(names(df), "lote_origem", "data_coleta", "observacoes")) {
          stop("Use nomes distintos para as respostas e para as colunas de identificação.", call. = FALSE)
        }
        df[[col_nome]] <- ""
      }
      if (identical(tipo_resolvido, "transversal_comparativo")) {
        df$lote_origem <- ""
        df$data_coleta <- ""
        df$observacoes <- ""
      }
      if (anyDuplicated(names(df))) stop("Use nomes distintos para as respostas e para as colunas de identificação.", call. = FALSE)
      df
    })

    # Dicionário de variáveis
    dicionario_dados <- shiny::reactive({
      if (tipo_resolvido == "impacto") {
        nomes <- names(tabela_coleta_dados())
        respostas <- vapply(seq_len(max(1L, min(10L, as.integer(ou_vazio(input$n_vars_resposta, 2))))),
          function(i) gerar_nome_reduzido(ou_vazio(input[[paste0("var_nome_", i)]], paste0("resposta_", i))), character(1))
        unidades <- vapply(seq_along(respostas), function(i) ou_vazio(input[[paste0("var_unidade_", i)]], "unidade"), character(1))
        descricao <- c("Sítio permanente; registre ambiente e fonte na aba sitios", "Campanha compartilhada por todos os sítios", "Antes ou depois do início previsto do impacto", "Data indicativa no formato AAAA-MM-DD", "Medida interna ao sítio nesta campanha; não é réplica de impacto", "Data efetiva da coleta", "Esforço efetivamente realizado, com unidade definida no protocolo", "Desvios do protocolo e motivos de ausências")
        names(descricao) <- c(unidade_coluna_nome(), "campanha", "periodo", "data_prevista", "subamostra", "data_real", "esforco_real", "observacoes")
        if (tipo_impacto() != "ba") descricao[fator_nome_limpo()] <- "Condição: primeiro impacto, depois referência"
        descricao[respostas] <- paste("Resposta medida em", unidades)
        return(data.frame(coluna = nomes, tipo = ifelse(nomes %in% respostas, "Resposta a medir", "Identificação / campo"), papel = ifelse(nomes %in% respostas, "Resposta", "Delineamento e protocolo"), unidade = ifelse(nomes %in% respostas, unidades[match(nomes, respostas)], ""), descricao = unname(descricao[nomes])))
      }
      # No gradiente o eixo é quantitativo contínuo e a unidade é a estação.
      if (identical(tipo_resolvido, "gradiente")) {
        valores <- estacoes_valores()
        df_dict <- data.frame(
          coluna = c("estacao", gradiente_coluna(), "pool"),
          tipo = c("Identificador", "Quantitativa contínua", "Contagem (itens por estação)"),
          papel = c("Identificador da estação", "Eixo do gradiente (explanatória contínua)", "Amostra composta (pool)"),
          unidade = c("", gradiente_unidade(), "indivíduos/porções"),
          descricao = c(
            "Código da estação amostral ao longo do gradiente",
            sprintf("Posição da estação no gradiente (faixa de %g a %g %s)", min(valores), max(valores), gradiente_unidade()),
            "Itens misturados fisicamente numa amostra composta (pool = 1 indica item único); médias de medidas individuais não são pool. Registre a contagem das medidas internas no protocolo."
          ),
          stringsAsFactors = FALSE
        )
      } else if (identical(tipo_resolvido, "impacto") && identical(tipo_impacto(), "ba")) {
        # Antes–Depois: sem coluna de condição, só sítio, pool e momento.
        unidade_col <- unidade_coluna_nome()
        df_dict <- data.frame(
          coluna = c(unidade_col, "pool", "momento"),
          tipo = c("Identificador", "Contagem (itens por UA)", "Qualitativa ordinal"),
          papel = c("Identificador do sítio", "Amostra composta (pool)", "Momento da medição (antes/depois)"),
          unidade = c("", "indivíduos/porções", ""),
          descricao = c(
            "Identificador único do sítio amostral",
            "Quantidade de itens misturados para compor a UA (pool = 1 indica item único)",
            paste("Momentos:", paste(momentos_lista(), collapse = ", "))
          ),
          stringsAsFactors = FALSE
        )
      } else {
      fator_col <- fator_nome_limpo()
      unidade_col <- unidade_coluna_nome()
      impacto <- identical(tipo_resolvido, "impacto")
      item_pool <- if (identical(tipo_resolvido, "transversal_comparativo")) ou_vazio(input$unidade_item, "item a definir") else "indivíduos/porções"
      descricao_pool <- if (identical(tipo_resolvido, "transversal_comparativo")) {
        paste0("Quantidade de unidades físicas para compor a UA. Unidade declarada: ", item_pool, ". Pool = 1 indica uma unidade; pool maior que 1 indica mistura física.")
      } else {
        "Quantidade de itens misturados para compor a UA (pool = 1 indica item único)"
      }
      df_dict <- data.frame(
        coluna = c(unidade_col, fator_col, "replica", "pool"),
        tipo = c("Identificador", "Qualitativa nominal", "Contagem / Ordem", "Contagem (itens por UA)"),
        papel = c(
          if (impacto) "Identificador do sítio" else "Identificador da UA",
          if (impacto) "Condição do sítio (impacto ou referência)" else "Fator / Categoria",
          if (tipo_resolvido == "longitudinal") "Ordem da UA dentro do grupo" else "Réplica do grupo",
          "Amostra composta (pool)"
        ),
        unidade = c("", "", "", item_pool),
        descricao = c(
          if (impacto) "Identificador único do sítio amostral" else if (tipo_resolvido == "longitudinal") "Código permanente da UA acompanhada; a independência precisa ser justificada pelo desenho e pela origem" else "Identificador único da unidade amostral independente",
          paste(if (impacto) "Condições do sítio:" else "Níveis do fator:", paste(niveis_primeiro_fator(), collapse = ", ")),
          if (tipo_resolvido == "longitudinal") "Número da UA dentro do grupo; não representa uma réplica do ambiente" else "Número da réplica independente dentro de cada grupo",
          descricao_pool
        ),
        stringsAsFactors = FALSE
      )
      if (segundo_fator()) df_dict <- rbind(df_dict, data.frame(
        coluna = fator2_coluna(), tipo = "Qualitativa nominal", papel = "Segundo fator observado",
        unidade = "", descricao = paste("Níveis:", paste(unique(combinacoes_transversal()$segundo), collapse = ", ")),
        stringsAsFactors = FALSE))
      if (identical(tipo_resolvido, "transversal_comparativo") &&
          isTRUE(ou_vazio(input$registrar_coordenadas, FALSE))) {
        precisao <- as.integer(ou_vazio(input$precisao_gps_m, 5))
        df_dict <- rbind(df_dict, data.frame(
          coluna = c("latitude_wgs84", "longitude_wgs84", "precisao_gps_m"),
          tipo = c("Quantitativa contínua", "Quantitativa contínua", "Quantitativa contínua"),
          papel = c("Localização prevista da UA", "Localização prevista da UA", "Qualidade do posicionamento"),
          unidade = c("graus decimais", "graus decimais", "m"),
          descricao = c(
            "Latitude prevista da estação no sistema WGS 84; preencher após mapear ou localizar a UA.",
            "Longitude prevista da estação no sistema WGS 84; preencher após mapear ou localizar a UA.",
            sprintf("Precisão horizontal máxima planejada para o GPS: %d m. Não demonstra independência entre estações.", precisao)
          ), stringsAsFactors = FALSE))
      }
      # No longitudinal e no impacto BACI o dicionário ganha o momento.
      if (identical(tipo_resolvido, "longitudinal") ||
          (impacto && impacto_com_momentos())) {
        df_dict <- rbind(df_dict, data.frame(
          coluna = "momento",
          tipo = "Qualitativa ordinal",
          papel = "Momento da medição (repetição da mesma UA)",
          unidade = "",
          descricao = paste("Momentos:", paste(momentos_lista(), collapse = ", ")),
          stringsAsFactors = FALSE
        ))
      }
      }
      n_vars <- max(1L, min(10L, as.integer(ou_vazio(input$n_vars_resposta, 2))))
      for (i in seq_len(n_vars)) {
        col_nome <- gerar_nome_reduzido(ou_vazio(input[[paste0("var_nome_", i)]], paste0("resposta_", i)))
        unid <- ou_vazio(input[[paste0("var_unidade_", i)]], "unidade")
        df_dict <- rbind(df_dict, data.frame(
          coluna = col_nome,
          tipo = "Quantitativa contínua",
          papel = "Resposta",
          unidade = unid,
          descricao = paste("Medição de resposta:", col_nome, "em", unid),
          stringsAsFactors = FALSE
        ))
      }
      if (identical(tipo_resolvido, "transversal_comparativo")) {
        df_dict <- rbind(df_dict, data.frame(
          coluna = c("lote_origem", "data_coleta", "observacoes"), tipo = "Texto",
          papel = "Registro da coleta", unidade = "",
          descricao = c("Lote e origem da UA (barco, viveiro, banca ou outra fonte); detalhe a procedência nos registros de campo",
            "Data da coleta no formato AAAA-MM-DD", "Ocorrências, desvios do protocolo e motivos de medidas ausentes")))
      }
      if (tipo_resolvido == "longitudinal") {
        df_dict <- df_dict[df_dict$coluna != "pool", , drop = FALSE]
        nomes <- c("data_prevista", "ordem_grupo", "ordem_visita", "data_real", "n_medidas", "status_ua", "motivo_ausencia", "observacoes")
        descricoes <- c("Data indicativa da visita; vazia quando o início varia por UA",
          "Ordem sorteada dos grupos em cada momento", "Ordem sorteada das UAs dentro do grupo e momento",
          "Data efetiva da visita", "Quantidade efetivamente medida; medidas internas resumidas conforme a resposta, não misturadas fisicamente",
          "Situação da UA na visita: presente, perdida ou outra ocorrência", "Motivo da falta de medida ou saída da UA",
          "Registro do protocolo, maré, horário e ocorrências")
        df_dict <- rbind(df_dict, data.frame(coluna = nomes, tipo = "Registro de campo", papel = "Acompanhamento", unidade = "", descricao = descricoes))
      }
      df_dict
    })

    # Badge de contagem
    output$badge_total_uas <- shiny::renderUI({
      # No gradiente o total é o número de estações, que é o número de linhas.
      if (identical(tipo_resolvido, "gradiente")) {
        n <- length(estacoes_valores())
        return(shiny::tags$span(class = "badge bg-primary fs-6 px-3 py-2",
          sprintf("%d estações = %d linhas", n, n)
        ))
      }
      # No impacto a unidade é o sítio; o total depende do tipo de pergunta.
      if (identical(tipo_resolvido, "impacto")) {
        tab <- tabela_coleta_dados()
        return(shiny::tags$span(class = "badge bg-primary", sprintf("%d sítios · %d campanhas · %d linhas", nrow(sitios_impacto()), nrow(campanhas_impacto()), nrow(tab))))
      }

      grupos <- grupos_lista()
      n_uas <- max(1L, as.integer(ou_vazio(input$n_uas, 5)))
      if (tipo_resolvido == "longitudinal") n_uas <- n_longitudinal()
      total <- length(grupos) * n_uas
      if (identical(tipo_resolvido, "longitudinal")) {
        n_momentos <- length(momentos_lista())
        return(shiny::tags$span(class = "badge bg-primary fs-6 px-3 py-2",
          sprintf("%d linhas (%d UAs × %d momentos)", total * n_momentos, total, n_momentos)
        ))
      }
      if (tipo_resolvido == "transversal_comparativo") return(shiny::tags$span(class = "badge bg-primary fs-6 px-3 py-2",
        sprintf("%d UAs no total (%d categorias)", sum(uas_por_grupo()), length(grupos))))
      shiny::tags$span(class = "badge bg-primary fs-6 px-3 py-2",
        sprintf("%d UAs no total (%d grupos × %d réplicas)", total, length(grupos), n_uas)
      )
    })

    # Tabela Tidy DT
    output$tabela_tidy_coleta <- DT::renderDT({
      df <- tabela_coleta_dados()
      DT::datatable(
        df,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = "tip",
          language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json")
        ),
        rownames = FALSE,
        class = "display compact cell-border stripe"
      )
    })

    # Texto formatado da seção de metodologia para artigo
    texto_metodologia_artigo_str <- shiny::reactive({
      # No gradiente o texto descreve as estações ao longo da faixa, o pool por
      # estação e a análise por regressão.
      if (identical(tipo_resolvido, "gradiente")) {
        valores <- estacoes_valores()
        n <- length(valores)
        nome_extenso <- ou_vazio(input$gradiente_nome, "distancia_fonte")
        unidade <- gradiente_unidade()
        pools <- pools_por_grupo()
        iguais <- pools_iguais()
        modo <- ou_vazio(input$modo_estacoes, "igual")
        espacamento <- switch(modo, igual = "equidistante no eixo informado",
          geometrico = "em progressão geométrica", livre = "com valores definidos pelo pesquisador")
        paragrafos <- c(
          "ORIENTAÇÃO INICIAL — Minuta de planejamento, sujeita à revisão na literatura específica. Não constitui um delineamento validado nem um relato de coleta realizada.",
          sprintf("Prevê-se um estudo observacional com %d estações ao longo de %s (%g a %g %s), com distribuição %s. Cada estação será identificada como unidade de registro; sua independência deverá ser justificada pelo contexto espacial e pelo processo estudado.",
            n, nome_extenso, min(valores), max(valores), unidade, espacamento),
          paste("Ambiente:", ou_vazio(input$gradiente_ambiente, "A definir"),
            "— Origem:", ou_vazio(input$gradiente_fontes, "Gradiente ambiental sem fonte pontual")),
          paste("Referências espaciais previstas:", ou_vazio(input$gradiente_referencia, "A definir e justificar quando cabíveis.")),
          paste("Registro dentro da estação:", ou_vazio(input$gradiente_subamostras, "Uma medida por estação"),
            "— Protocolo:", ou_vazio(input$gradiente_protocolo, "A definir antes da coleta.")),
          paste("Campanhas:", ou_vazio(input$gradiente_campanhas, "Uma campanha"),
            "— Janela e ordem de visita:", ou_vazio(input$gradiente_janela, "A definir.")),
          paste("Covariáveis e segundo gradiente:", ou_vazio(input$gradiente_covariaveis, "A avaliar conforme a pergunta.")),
          "As subamostras deverão ser distinguidas de estações independentes. Mistura física e resumo de medidas não são equivalentes. O campo pool da ficha deverá ser usado apenas quando houver amostra composta física; o protocolo deverá explicitar o tratamento das demais subamostras.",
          "A análise deverá relacionar a resposta ao gradiente medido, considerando a forma da relação, a distribuição da resposta e possíveis dependências espaciais ou temporais. Uma fonte isolada limita a generalização; revisitas não aumentam o número de estações independentes. A ficha atual representa uma campanha e um sistema; vários sistemas ou revisitas exigem adaptação da estrutura de registro.",
          "A resposta será interpretada como associada ao gradiente medido, nos ambientes, na região e no período definidos no protocolo; a associação não identifica sozinha um mecanismo causal.",
          paste("Literatura e pendências:", ou_vazio(input$gradiente_literatura, "Buscar referências específicas para esforço, espaçamento, protocolo e análise.")))
        return(paste(paragrafos, collapse = "\n\n"))
      }
      # No impacto o texto depende do tipo de pergunta: CI compara sítios num
      # só momento, BA compara o mesmo sítio antes e depois, BACI combina os dois.
      if (identical(tipo_resolvido, "impacto")) {
        sitios <- sitios_impacto()
        campanhas <- campanhas_impacto()
        n_sub <- inteiro_impacto(input$impacto_subamostras, 3)
        return(paste(c(
          sprintf("Será realizado um estudo observacional do tipo %s. Serão visitados %d sítios (%s), em %d campanhas, com %d subamostras por sítio e campanha.", toupper(tipo_impacto()), nrow(sitios), paste(names(table(sitios$condicao)), table(sitios$condicao), collapse = "; "), nrow(campanhas), n_sub),
          paste("Alcance previsto:", ou_vazio(input$impacto_escala, "Um empreendimento / sistema específico")),
          paste("Critérios de seleção:", ou_vazio(input$impacto_criterios, "A definir e justificar antes da coleta.")),
          paste("Procedimentos e esforço:", ou_vazio(input$impacto_protocolo, "A definir antes da coleta.")),
          sprintf("O início do impacto está previsto para %s. As campanhas previstas são: %s.", ou_vazio(input$impacto_inicio, "2028-01-01"), paste(campanhas$campanha, campanhas$data_prevista, collapse = "; ")),
          paste("Janela e ordem de visita:", ou_vazio(input$impacto_janela, "A definir, com condições comparáveis entre sítios.")),
          paste("Covariáveis e outras mudanças:", ou_vazio(input$impacto_covariaveis, "A definir.")),
          limite_impacto(tipo_impacto()),
          "As subamostras serão registradas separadamente e ligadas ao sítio e à campanha. A análise deverá respeitar ambientes compartilhados, dependência espacial e revisitas. A independência não decorre da contagem de sítios. O painel não escolhe um teste nem calcula poder amostral.",
          paste("Registro prévio, referências e pendências:", ou_vazio(input$impacto_registro, "A completar antes da coleta."))), collapse = "\n\n"))
      }

      grupos <- grupos_lista()
      n_uas <- max(1L, as.integer(ou_vazio(input$n_uas, 5)))
      if (tipo_resolvido == "longitudinal") n_uas <- n_longitudinal()
      total <- length(grupos) * n_uas
      fator_extenso <- ou_vazio(input$fator_nome, "espécie")
      pools <- pools_por_grupo()
      iguais <- pools_iguais()
      longitudinal <- identical(tipo_resolvido, "longitudinal")

      if (identical(tipo_resolvido, "transversal_comparativo")) {
        quantidades <- unname(uas_por_grupo()[grupos])
        if (segundo_fator()) fator_extenso <- paste(fator_extenso, "e", ou_vazio(input$fator2_nome, "sexo"))
        total <- sum(quantidades)
        sem_pool <- all(unname(pools[grupos]) == 1L)
        item <- ou_vazio(input$unidade_item, "item a definir")
        composicao <- if (sem_pool) {
          paste0("Não haverá amostra composta: cada UA será formada por 1 item (", item, "). A coluna pool terá valor 1 apenas para registrar que a UA não foi formada pela mistura de unidades físicas.")
        } else {
          paste0(paste(sprintf("%s: %d UAs, com %d itens (%s) por UA", grupos, quantidades, unname(pools[grupos]), item), collapse = "; "), ". Quando houver pool, cada unidade física contribuirá com a mesma massa ou volume e integrará somente uma UA.")
        }
        espacamento <- trimws(ou_vazio(input$espacamento_minimo_m, ""))
        plano_espacial <- if (isTRUE(ou_vazio(input$registrar_coordenadas, FALSE))) c(
          sprintf("Serão registradas para cada estação latitude e longitude previstas no sistema WGS 84, com precisão horizontal planejada de até %d m.", as.integer(ou_vazio(input$precisao_gps_m, 5))),
          if (nzchar(espacamento)) paste("Distância mínima declarada entre estações:", espacamento, "m. Ela deverá ser justificada pelo estudo-piloto e pelo contexto hidrodinâmico.") else "A distância mínima entre estações ainda será definida após o estudo-piloto e a avaliação de hidrodinâmica, maré e conectividade.",
          paste("Justificativa espacial:", ou_vazio(input$justificativa_espacamento, "A completar antes da coleta; as coordenadas e a contagem de estações não demonstram independência por si só."))
        ) else character()
        return(paste(c(
          sprintf("Será realizado um estudo observacional transversal comparativo entre as categorias de %s (%s), em um único recorte temporal. Serão previstas %d unidades amostrais, distribuídas por categoria conforme o plano de coleta. A independência dessas unidades deverá ser assegurada pelo plano de seleção e pela consideração da origem das amostras.", fator_extenso, paste(grupos, collapse = ", "), total),
          paste("Local e período previstos:", ou_vazio(input$local_periodo, "A definir antes da coleta.")),
          paste("Unidade física declarada:", item),
          paste("Composição prevista das unidades:", composicao),
          paste("Critérios de seleção e formação das UAs:", ou_vazio(input$criterios_coleta, "A definir: população, elegibilidade, janela biométrica quando pertinente, lotes e seleção dentro de cada grupo.")),
          paste("Procedimentos de coleta e medição:", ou_vazio(input$procedimentos_coleta, "A definir: identificação, conservação, preparo e métodos de medição.")),
          plano_espacial,
          "Cada UA ocupará uma linha da planilha. As respostas serão registradas nas unidades indicadas no dicionário de colunas; lote, data e ocorrências acompanharão os registros. A análise será definida considerando a natureza da resposta, a independência e os pressupostos. As diferenças entre grupos serão interpretadas como associações, considerando possíveis fatores de confusão.",
          alcance_comparacao()
        ), collapse = "\n\n"))
      }

      if (longitudinal) {
        return(paste(c(
          sprintf("Será realizado um estudo observacional longitudinal comparativo entre %s (%s), com %d UAs iniciais por grupo, totalizando %d UAs, acompanhadas em %d momentos (%s).", fator_extenso, paste(grupos, collapse = ", "), n_longitudinal(), length(grupos) * n_longitudinal(), length(momentos_lista()), paste(momentos_lista(), collapse = ", ")),
          sprintf("A meta é terminar com %d UAs por grupo. A previsão de perda de %g%% foi usada apenas para ampliar o número inicial, por arredondamento para cima de n/(1-taxa); não representa cálculo de poder amostral.", ou_vazio(input$n_uas, 4), ou_vazio(input$perda_pct, 20)),
          paste("Local e período:", ou_vazio(input$local_periodo, "A definir.")),
          paste("Seleção das UAs:", ou_vazio(input$criterios_coleta, "A definir, assegurando várias unidades por grupo e registrando locais compartilhados.")),
          paste("Procedimentos:", ou_vazio(input$procedimentos_coleta, "A definir; manter protocolo, equipamento, horário e fase da maré comparáveis em todas as visitas.")),
          sprintf("Cada UA manterá seu código em todas as visitas. Estão previstas %d medidas internas por UA e visita. Cada resposta será resumida de forma adequada à sua natureza, por exemplo, altura média e proporção de sobreviventes; a quantidade efetivamente medida será registrada em n_medidas. Subamostras não são pool e não aumentam o número de UAs.", ou_vazio(input$n_medidas, 60)),
          if (isTRUE(ou_vazio(input$linha_base, TRUE))) "O primeiro momento será a linha de base, com registro das características de partida para avaliar diferenças preexistentes entre grupos." else "O primeiro momento não foi declarado como linha de base; justificar essa escolha.",
          sprintf("Todos os grupos serão visitados em cada momento numa janela de %d dias, com ordem de grupos e UAs sorteada. A data real será registrada. %s", ou_vazio(input$janela_dias, 3), if (identical(input$relogio, "individual")) "O tempo será contado desde o início de cada UA, cuja data inicial será registrada na aba unidades." else "O calendário partirá de uma data inicial comum."),
          "Medidas não obtidas permanecerão vazias, com motivo registrado; sobrevivência zero observada será registrada como zero. Códigos nunca serão reaproveitados. Despesca antecipada e outras perdas relacionadas à resposta precisarão ser consideradas na interpretação.",
          "A independência entre UAs será avaliada considerando posicionamento, circulação da água, origem e manejo compartilhados. UAs de cultivo em um mesmo ambiente não serão contadas como réplicas desse ambiente. A comparação entre locais escolhidos será distinguida da generalização para categorias ambientais, que exige replicação de ambientes em cada categoria.",
          "A análise futura poderá usar modelo linear misto com grupo, tempo e interação grupo × tempo como efeitos fixos e UA como efeito aleatório, considerando agrupamentos de origem e dependência temporal. A interação permite investigar trajetórias diferentes. Medidas-resumo por UA são alternativa a avaliar; não será feito um teste separado por momento. Grupos preexistentes permitem interpretar associações, considerando confundidores.",
          alcance_comparacao()
        ), collapse = "\n\n"))
      }

      paragrafos <- c()
      if (longitudinal) {
        momentos <- momentos_lista()
        paragrafos <- c(paragrafos, sprintf(
          "Trata-se de um delineamento observacional longitudinal comparativo. Foram acompanhadas %d unidades amostrais por %s (%s), totalizando %d unidades amostrais independentes, cada uma medida nos mesmos %d momentos (%s).",
          n_uas, fator_extenso, paste(grupos, collapse = ", "), total,
          length(momentos), paste(momentos, collapse = ", ")
        ))
      } else {
        paragrafos <- c(paragrafos, sprintf(
          "Trata-se de um delineamento observacional transversal comparativo. Foram analisadas %d unidades amostrais por %s (%s), totalizando %d unidades amostrais independentes.",
          n_uas, fator_extenso, paste(grupos, collapse = ", "), total
        ))
      }
      
      if (iguais) {
        k <- pools[1]
        if (k > 1) {
          paragrafos <- c(paragrafos, sprintf(
            "Cada unidade amostral foi constituída por uma amostra composta de %d indivíduos, formada com contribuições equitativas de massa de cada indivíduo. Cada espécime integrou uma única unidade amostral. As réplicas analíticas foram resumidas pela média antes da análise estatística.",
            k
          ))
        } else {
          paragrafos <- c(paragrafos, "Cada unidade amostral foi constituída por um único espécime individual.")
        }
      } else {
        faixa_pool <- paste(range(pools), collapse = " a ")
        paragrafos <- c(paragrafos, sprintf(
          "Cada unidade amostral foi constituída por uma amostra composta de %s indivíduos, conforme a %s (registrado na coluna pool), formada com contribuições equitativas de massa de cada espécime. Cada indivíduo integrou uma única unidade amostral. As réplicas analíticas de laboratório foram resumidas pela média dentro de cada unidade, não sendo consideradas repetições na análise estatística.",
          faixa_pool, fator_extenso
        ))
      }
      
      if (longitudinal) {
        paragrafos <- c(paragrafos, sprintf(
          "Como as medições repetidas da mesma unidade amostral não são independentes, a comparação entre as categorias de %s ao longo do tempo foi conduzida por um modelo linear misto, com o momento, a %s e sua interação como efeitos fixos e a unidade amostral como efeito aleatório.",
          fator_extenso, fator_extenso
        ))
      } else if (iguais) {
        paragrafos <- c(paragrafos, sprintf(
          "Para a comparação das médias entre as diferentes categorias de %s, foi aplicada Análise de Variância (ANOVA) de um fator, seguida de teste post-hoc adequado se verificada significância estatística.",
          fator_extenso
        ))
      } else {
        paragrafos <- c(paragrafos, sprintf(
          "Em razão dos diferentes tamanhos de pool entre as categorias de %s, que induzem heterocedasticidade estrutural (a variância amostral da média é inversamente proporcional ao pool, Var = σ²/k), a comparação entre os grupos foi conduzida por meio da ANOVA de Welch, associada ao teste post-hoc de Games-Howell para comparações múltiplas com variâncias desiguais.",
          fator_extenso
        ))
      }
      
      paste(paragrafos, collapse = "\n\n")
    })

    output$texto_metodologia_artigo <- shiny::renderUI({
      texto <- texto_metodologia_artigo_str()
      paragrafos <- unlist(strsplit(texto, "\n\n"))
      shiny::tagList(lapply(paragrafos, function(p) shiny::p(class = "mb-2", p)))
    })

    # Card do Modelo Estatístico
    output$card_modelo_estatistico <- shiny::renderUI({
      if (identical(tipo_resolvido, "transversal_comparativo")) {
        return(shiny::div(class = "alert alert-info border mb-0",
          shiny::h6(class = "fw-bold", "Planejar a análise antes da coleta"),
          shiny::p(class = "small mb-0",
            "Defina a resposta e a comparação de interesse. Para respostas quantitativas, testes de comparação de médias podem ser pertinentes, mas a escolha depende do número de grupos, da independência, das variâncias e dos demais pressupostos. Pools diferentes e dependência por lote exigem atenção; não escolha o teste somente pelo pool.")))
      }
      # No gradiente a análise é uma regressão da resposta contra o eixo contínuo.
      if (identical(tipo_resolvido, "gradiente")) {
        return(shiny::div(class = "alert alert-info border mb-0",
          style = "border-left: 4px solid #2E7D8F !important;",
          shiny::h6(class = "alert-heading fw-bold mb-1", shiny::icon("chart-line"), " Regressão da resposta contra o gradiente"),
          shiny::p(class = "small mb-1",
            "Não há grupos a comparar: cada estação é um ponto ao longo da faixa contínua. A regressão descreve como a resposta muda com a variável do gradiente e permite interpolar valores não amostrados."
          ),
          shiny::p(class = "small mb-1",
            "Para uma resposta contínua, a reta pode ser um ponto de partida. Confira a forma da relação e os pressupostos; contagens e proporções podem exigir outra família de modelo."
          ),
          shiny::p(class = "small mb-0",
            shiny::tags$b("Exemplo para resposta contínua: "),
            shiny::tags$code(sprintf("lm(resposta ~ %s, data = dados)", gradiente_coluna()))
          )
        ))
      }
      if (identical(tipo_resolvido, "longitudinal")) {
        unidade_col <- unidade_coluna_nome()
        fator_col <- fator_nome_limpo()
        return(shiny::div(class = "alert alert-info border mb-0 obs-modelo-longitudinal",
          style = "border-left: 4px solid #2E7D8F !important;",
          shiny::h6(class = "alert-heading fw-bold mb-1", shiny::icon("arrows-rotate"), " Modelo misto para medidas repetidas"),
          shiny::p(class = "small mb-2",
            "A interação grupo × tempo investiga trajetórias diferentes. As visitas da mesma UA são relacionadas. Este exemplo é para uma resposta contínua; a adequação do modelo e a dependência temporal precisam ser avaliadas."),
          shiny::p(class = "small mb-0",
            shiny::tags$b("Função no R: "),
            shiny::tags$code(sprintf("nlme::lme(resposta ~ %s * factor(momento), random = ~ 1 | %s, data = dados)",
                                     fator_col, unidade_col))),
          shiny::tags$details(class = "small mt-2",
            shiny::tags$summary("Limite deste exemplo"),
            shiny::p(class = "mt-1 mb-0", "Este exemplo considera a repetição da mesma UA no tempo. Outros níveis, como categoria, ambiente e local, não são incluídos automaticamente; o modelo precisa representá-los conforme a pergunta do estudo.")),
          shiny::div(class = "small obs-parametros",
            shiny::p(class = "mb-1",
              shiny::tags$code("nlme::lme"), " ajusta o modelo linear misto; ",
              shiny::tags$code("resposta"), " é a variável medida; ",
              shiny::tags$code(sprintf("%s * factor(momento)", fator_col)),
              " inclui grupo, visita e interação. ", shiny::tags$code("factor(momento)"), " trata as visitas como categorias."),
            shiny::p(class = "mb-0",
              shiny::tags$code(sprintf("random = ~ 1 | %s", unidade_col)),
              " permite um nível médio próprio para cada UA, compartilhado por suas visitas; ",
              shiny::tags$code("data = dados"), " indica a tabela usada."))
        ))
      }
      # Os três modelos ficam visíveis para comparar suas perguntas. São
      # exemplos para resposta contínua resumida por sítio e campanha, não
      # uma análise automática das linhas de subamostras da planilha.
      if (identical(tipo_resolvido, "impacto")) {
        sitio <- unidade_coluna_nome()
        condicao <- gerar_nome_reduzido(ou_vazio(input$fator_nome, "situacao"))
        return(shiny::tagList(
          shiny::p(class = "small", "Resposta contínua; uma linha por sítio × campanha, com subamostras resumidas. Substitua resposta pela variável; condição e periodo devem ser fatores. Use códigos únicos de sítio."),
          shiny::div(class = "obs-modelo-caso",
            shiny::h6("CI — Controle–Impacto"),
            shiny::p(class = "small", "Compara impacto e referência depois da mudança, ajustando campanha e diferenças persistentes entre sítios."),
            shiny::tags$pre(.noWS = "inside", shiny::tags$code(sprintf(
              "nlme::lme(resposta ~ %s + factor(campanha),\n  random = ~ 1 | %s, data = dados_campanhas)", condicao, sitio)))),
          shiny::div(class = "obs-modelo-caso",
            shiny::h6("BA — Antes–Depois"),
            shiny::p(class = "small", "Compara os mesmos sítios antes e depois; periodo estima a mudança entre os dois períodos."),
            shiny::tags$pre(.noWS = "inside", shiny::tags$code(sprintf(
              "nlme::lme(resposta ~ periodo,\n  random = ~ 1 | %s, data = dados_campanhas)", sitio)))),
          shiny::div(class = "obs-modelo-caso",
            shiny::h6("BACI — Antes–Depois / Controle–Impacto"),
            shiny::p(class = "small", "A interação condição × período compara a mudança no impacto com a mudança nas referências."),
            shiny::tags$pre(.noWS = "inside", shiny::tags$code(sprintf(
              "nlme::lme(resposta ~ %s * periodo,\n  random = ~ 1 | %s, data = dados_campanhas)", condicao, sitio))),
            shiny::p(class = "small mb-0", "Contraste: (Depois − Antes) no impacto − (Depois − Antes) na referência. Confira a ordem dos níveis para interpretar o sinal.")),
          shiny::p(class = "small text-muted mt-2 mb-0",
            "Referências: ",
            shiny::tags$a(href = "https://stat.ethz.ch/R-manual/R-devel/library/nlme/html/lme.html", target = "_blank", rel = "noopener noreferrer", "documentação de nlme::lme"),
            "; ", shiny::tags$a(href = "https://doi.org/10.1111/2041-210X.13287", target = "_blank", rel = "noopener noreferrer", "Fisher et al. (2019), modelos BACI"), ".")
        ))
      }

      iguais <- pools_iguais()
      if (iguais) {
        k <- pools_por_grupo()[1]
        shiny::div(class = "alert alert-success border mb-0",
          shiny::h6(class = "alert-heading fw-bold mb-1", shiny::icon("check"), " ANOVA de 1 Fator Clássica"),
          shiny::p(class = "small mb-1", if (k == 1) "Não há amostra composta: cada UA contém um único item, como uma amostra de água em uma estação. A independência continua dependendo da seleção espacial e do contexto hidrodinâmico." else "Com tamanhos de pool homogêneos entre os grupos, a premissa de homogeneidade de variâncias decorrente do delineamento não é estruturalmente violada."),
          shiny::p(class = "small mb-0", shiny::tags$b("Fórmula no R: "), shiny::tags$code("lm(resposta ~ especie, data = dados)"), " ou ", shiny::tags$code("aov()"), " + TukeyHSD.")
        )
      } else {
        shiny::div(class = "alert alert-warning border mb-0",
          style = "border-left: 4px solid #E89B3C !important;",
          shiny::h6(class = "alert-heading fw-bold mb-1", shiny::icon("triangle-exclamation"), " ANOVA de Welch + Pós-teste de Games-Howell"),
          shiny::p(class = "small mb-1",
            "Pools desiguais geram variâncias populacionais estruturalmente distintas entre os grupos (Var = σ²/k). A ANOVA clássica subestima ou superestima erros padrão. A correção de Welch ajusta os graus de liberdade sem exigir homocedasticidade."
          ),
          shiny::p(class = "small mb-0",
            shiny::tags$b("Função no R: "), shiny::tags$code("rstatix::welch_anova_test(resposta ~ especie, data = dados)"), " e ", shiny::tags$code("rstatix::games_howell_test()")
          )
        )
      }
    })

    output$limites_modelo_impacto <- shiny::renderUI({
      shiny::req(tipo_resolvido == "impacto")
      shiny::div(class = "alert alert-info small",
        shiny::h6(paste("Plano selecionado:", toupper(tipo_impacto()))),
        shiny::p(switch(tipo_impacto(),
          ci = "Sem dados de antes, diferenças prévias entre ambientes podem explicar o resultado.",
          ba = "Sem referência, sazonalidade ou tendências regionais podem explicar a mudança.",
          baci = "Compara mudanças entre condições; reduz explicações alternativas, mas não prova causalidade.")),
        shiny::p(class = "mb-0", "Modelos iniciais para resposta contínua. Contagens, proporções e outras hierarquias exigem adaptações; revisitas e subamostras não são independentes."))
    })

    # Ficha persistente do delineamento
    ficha <- shiny::reactive({
      grupos <- if (!is.null(input$n_niveis) && is.null(input$fator_niveis)) {
        paste("Grupo", seq_len(as.integer(input$n_niveis)))
      } else {
        grupos_lista()
      }
      n_uas <- max(1L, as.integer(ou_vazio(input$n_uas, input$sitios_por_nivel %||% 5)))
      if (tipo_resolvido == "longitudinal") n_uas <- n_longitudinal()
      fator_col <- fator_nome_limpo()
      fator_extenso <- ou_vazio(input$fator_nome, "especie")
      pools <- pools_por_grupo()
      iguais <- pools_iguais()

      subamostras <- as.integer(input$subamostras_por_sitio %||% 1)
      tem_subamostras <- is.finite(subamostras) && subamostras > 1
      analise <- if (tem_subamostras) {
        "anova_mista_subamostras"
      } else if (!iguais && is.null(input$n_niveis)) {
        "welch_games_howell"
      } else {
        "anova_um_fator"
      }

      col_unidade <- unidade_coluna_nome()
      col_subamostra <- if (tem_subamostras) ou_vazio(input$coluna_subamostra, "subamostra") else ""

      # O gradiente grava o eixo contínuo com os valores das estações e já
      # declara o n planejado (número de estações) e a regressão sugerida.
      if (identical(tipo_resolvido, "gradiente")) {
        valores <- estacoes_valores()
        codigos <- estacoes_codigos()
        n_est <- length(valores)
        pools <- pools_por_grupo()
        return(ficha_mesclar(NULL, list(
          origem = paste("Planejamento observacional —", definicao$titulo),
          tipo = tipo_resolvido,
          pergunta = input$pergunta,
          resposta_coluna = NULL,
          # Eixo contínuo: sem níveis nem nomes de fator; niveis guarda o nº de
          # estações só para leitura (cabeçalho), e valores guarda a faixa.
          eixos = list(gradiente = list(
            coluna = gradiente_coluna(),
            continua = TRUE,
            unidade = gradiente_unidade(),
            niveis = n_est,
            valores = valores
          )),
          unidade_coluna = "estacao",
          hierarquia = list(
            estacoes = n_est,
            pool_por_estacao = stats::setNames(as.integer(pools[codigos]), codigos),
            subamostra_coluna = "",
            subamostras_por_sitio = NULL
          ),
          n_planejado = list(
            valor = n_est,
            total = n_est,
            unidade = "estações ao longo do gradiente",
            metodo = "declarado no delineamento de gradiente (número de estações)",
            premissas = NULL
          ),
          sorteio = NULL,
          analise_sugerida = "regressao_linear_simples"
        )))
      }

      # O longitudinal grava o eixo do tempo com os momentos declarados; a
      # análise de medidas repetidas ainda não tem passagem validada (costura).
      if (identical(tipo_resolvido, "longitudinal")) {
        momentos <- momentos_lista()
        return(ficha_mesclar(NULL, list(
          origem = paste("Planejamento observacional —", definicao$titulo),
          tipo = tipo_resolvido,
          pergunta = input$pergunta,
          resposta_coluna = input$resposta,
          eixos = list(
            grupos = list(coluna = fator_col, niveis = length(grupos), nomes = grupos),
            tempo = list(coluna = "momento", niveis = length(momentos), nomes = momentos)
          ),
          unidade_coluna = col_unidade,
          hierarquia = list(
            sitios_por_nivel = n_longitudinal(),
            niveis = length(grupos),
            subamostra_coluna = col_subamostra,
            subamostras_por_sitio = if (tem_subamostras) subamostras else NULL,
            momentos = momentos,
            repeticao_temporal = TRUE
          ),
          n_planejado = list(valor = n_longitudinal(), total = length(grupos) * n_longitudinal(),
            unidade = "UAs iniciais por grupo", metodo = "Meta declarada ampliada pela previsão de perdas; sem cálculo de poder",
            uas_por_grupo = n_longitudinal(),
            total_uas = length(grupos) * n_longitudinal(),
            uas_desejadas = ou_vazio(input$n_uas, 4), perda_pct = ou_vazio(input$perda_pct, 20),
            medidas_por_visita = ou_vazio(input$n_medidas, 60),
            linha_base = ou_vazio(input$linha_base, TRUE),
            caracteristicas_base = input$caracteristicas_base, agrupamento = input$agrupamento,
            relogio = ou_vazio(input$relogio, "comum"), data_inicial = input$data_inicial,
            janela_dias = ou_vazio(input$janela_dias, 3), semente = ou_vazio(input$semente_visitas, 2027)),
          sorteio = NULL,
          analise_sugerida = NULL
        )))
      }

      # O impacto grava o eixo do local (condição dos sítios) e, no BA/BACI,
      # também o eixo do tempo (antes/depois); a análise segue NULL até haver
      # passagem validada para cada tipo (costura).
      if (identical(tipo_resolvido, "impacto")) {
        grupos <- grupos_lista()
        n_uas <- max(1L, as.integer(ou_vazio(input$n_uas, 3)))
        fator_col <- fator_nome_limpo()
        unidade_col <- unidade_coluna_nome()
        momentos <- if (impacto_com_momentos()) momentos_lista() else NULL

        eixos <- list()
        if (identical(tipo_impacto(), "ba")) {
          # Sem controle, o eixo da comparação é o tempo (antes × depois).
          eixos$tempo <- list(coluna = "campanha", niveis = length(momentos), nomes = momentos)
        } else {
          eixos$impacto <- list(coluna = fator_col, niveis = length(grupos), nomes = grupos)
          if (impacto_com_momentos()) {
            eixos$tempo <- list(coluna = "campanha", niveis = length(momentos), nomes = momentos)
          }
        }
        return(ficha_mesclar(NULL, list(
          origem = paste("Planejamento observacional —", definicao$titulo),
          tipo = tipo_resolvido,
          pergunta = input$pergunta,
          resposta_coluna = input$resposta,
          eixos = eixos,
          unidade_coluna = unidade_col,
          hierarquia = list(
            sitios_por_nivel = if (length(unique(as.integer(table(sitios_impacto()$condicao)))) == 1L) n_uas else NULL,
            niveis = length(grupos),
            subamostra_coluna = "subamostra",
            subamostras_por_sitio = inteiro_impacto(input$impacto_subamostras, 3),
            momentos = momentos,
            repeticao_temporal = impacto_com_momentos()
          ),
          n_planejado = NULL,
          sorteio = NULL,
          analise_sugerida = NULL
        )))
      }

      ficha_mesclar(NULL, list(
        origem = paste("Planejamento observacional —", definicao$titulo),
        tipo = tipo_resolvido,
        pergunta = input$pergunta,
        resposta_coluna = input$resposta,
        eixos = list(grupos = list(coluna = fator_col, niveis = length(niveis_primeiro_fator()), nomes = niveis_primeiro_fator()),
          segundo_fator = if (segundo_fator()) list(coluna = fator2_coluna(), nomes = unique(combinacoes_transversal()$segundo)) else NULL),
        unidade_coluna = col_unidade,
        hierarquia = list(
          sitios_por_nivel = if (tipo_resolvido == "transversal_comparativo") {
            quantidades <- uas_por_grupo()
            if (length(unique(quantidades)) == 1L) unname(quantidades[1]) else NULL
          } else n_uas,
          uas_por_grupo = if (tipo_resolvido == "transversal_comparativo") uas_por_grupo() else NULL,
          item_por_ua = if (tipo_resolvido == "transversal_comparativo") ou_vazio(input$unidade_item, "item a definir") else NULL,
          niveis = length(grupos),
          subamostra_coluna = col_subamostra,
          subamostras_por_sitio = if (tem_subamostras) subamostras else NULL,
          localizacao_planejada = if (identical(tipo_resolvido, "transversal_comparativo") &&
              isTRUE(ou_vazio(input$registrar_coordenadas, FALSE))) list(
            sistema_referencia = ou_vazio(input$referencia_coordenadas, "WGS84"),
            precisao_horizontal_m = as.integer(ou_vazio(input$precisao_gps_m, 5)),
            espacamento_minimo_m = ou_vazio(input$espacamento_minimo_m, NULL),
            justificativa_espacamento = ou_vazio(input$justificativa_espacamento, NULL)
          ) else NULL
        ),
        n_planejado = NULL,
        sorteio = NULL,
        analise_sugerida = if (segundo_fator()) NULL else analise
      ))
    })

    # Downloads: Planilha Excel (.xlsx)
    # O transversal entrega só a coleta e suas orientações, sem um Projeto R.
    resumo_transversal <- shiny::reactive({
      grupos <- grupos_lista()
      n <- unname(uas_por_grupo()[grupos])
      pools <- unname(pools_por_grupo()[grupos])
      data.frame(Grupo = grupos, UAs = n, `Itens por UA` = pools,
        `Itens previstos` = n * pools, check.names = FALSE)
    })
    esquema_transversal <- shiny::reactive({
      fator <- ou_vazio(input$fator_nome, "Grupo")
      if (segundo_fator()) fator <- paste(fator, "×", ou_vazio(input$fator2_nome, "sexo"))
      n_respostas <- max(1L, min(10L, as.integer(ou_vazio(input$n_vars_resposta, 2))))
      respostas <- vapply(seq_len(n_respostas), function(i)
        ou_vazio(input[[paste0("var_nome_", i)]], paste0("resposta_", i)), character(1))
      desenhar_plano_transversal(resumo_transversal(), fator, respostas,
        ou_vazio(input$unidade_item, "item a definir"))
    })
    output$resumo_transversal_intro <- shiny::renderUI({
      resumo <- resumo_transversal()
      shiny::tagList(
        shiny::p(shiny::strong("Pergunta: "), ou_vazio(input$pergunta, "A definir")),
        shiny::p(shiny::strong("Local e período: "), ou_vazio(input$local_periodo, "A definir")),
        shiny::p(sprintf("%d grupos · %d UAs · %d unidades físicas previstas (%s)", nrow(resumo),
          sum(resumo$UAs), sum(resumo[["Itens previstos"]]), ou_vazio(input$unidade_item, "item a definir")))
      )
    })
    output$resumo_transversal_tabela <- shiny::renderTable({ resumo_transversal() },
      striped = TRUE, bordered = TRUE, spacing = "s", rownames = FALSE)
    output$esquema_transversal_ui <- shiny::renderUI({
      shiny::plotOutput(session$ns("esquema_transversal"),
        height = paste0(max(650, 250 + nrow(resumo_transversal()) * 155), "px"))
    })
    output$esquema_transversal <- shiny::renderPlot({
      shiny::req(tipo_resolvido == "transversal_comparativo")
      esquema_transversal()
    }, res = 110)
    estacoes_gradiente <- shiny::reactive({
      data.frame(estacao = estacoes_codigos(), valor_previsto = estacoes_valores(),
        unidade_gradiente = gradiente_unidade(), sistema = "", local = "",
        latitude = "", longitude = "", observacoes = "", stringsAsFactors = FALSE)
    })
    orientacoes_gradiente <- shiny::reactive({
      campos <- c("Ambiente", "Origem", "Referências espaciais", "Registro interno", "Campanhas",
        "Protocolo", "Covariáveis", "Janela e ordem", "Literatura e pendências")
      ids <- c("gradiente_ambiente", "gradiente_fontes", "gradiente_referencia", "gradiente_subamostras",
        "gradiente_campanhas", "gradiente_protocolo", "gradiente_covariaveis", "gradiente_janela", "gradiente_literatura")
      dic <- dicionario_dados()
      rbind(data.frame(secao = "Planejamento", campo = c("Pergunta", campos),
          orientacao = c(ou_vazio(input$pergunta, "A definir"),
            vapply(ids, function(id) ou_vazio(input[[id]], "A definir"), character(1)))),
        data.frame(secao = "Coleta", campo = dic$coluna,
          orientacao = paste(dic$descricao, dic$unidade)),
        data.frame(secao = "Estações", campo = names(estacoes_gradiente()),
          orientacao = c("Código da mesma estação na coleta.", "Previsão do eixo: preserve para comparar com o valor real.",
            "Unidade do eixo informado.", "Identifique a fonte ou o sistema estudado.", "Nome do local de coleta.",
            "Latitude em graus decimais; informe o referencial nas observações.", "Longitude em graus decimais.", "Motivos de alterações, referencial das coordenadas e notas de campo.")),
        data.frame(secao = "Cuidados", campo = paste("Cuidado", seq_along(cuidados_gradiente)), orientacao = cuidados_gradiente))
    })
    output$estacoes_gradiente <- DT::renderDT({
      DT::datatable(estacoes_gradiente(), rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
    })
    output$orientacoes_gradiente <- DT::renderDT({
      DT::datatable(orientacoes_gradiente(), rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
    })
    # O desenho usa somente a identificação da coleta, nunca respostas fictícias.
    esquema_observacional <- shiny::reactive({
      tab <- tabela_coleta_dados()
      if (tipo_resolvido == "impacto") return(desenhar_plano_impacto(sitios_impacto(), campanhas_impacto(), inteiro_impacto(input$impacto_subamostras, 3), tipo_impacto()))
      if (identical(tipo_resolvido, "gradiente")) {
        return(desenhar_plano_gradiente(tab, gradiente_coluna(), gradiente_unidade()))
      }
      if (tipo_resolvido == "longitudinal") return(desenhar_plano_longitudinal(
        tab, fator_nome_limpo(), unidade_coluna_nome(), momentos_lista(),
        ou_vazio(input$n_medidas, 60), ou_vazio(input$n_uas, 4), ou_vazio(input$perda_pct, 20),
        ou_vazio(input$janela_dias, 3), ou_vazio(input$linha_base, TRUE), ou_vazio(input$relogio, "comum")))
      unidade <- unidade_coluna_nome()
      tab$.unidade <- factor(tab[[unidade]], levels = unique(tab[[unidade]]))
      tab$.momento <- if ("momento" %in% names(tab))
        factor(tab$momento, levels = momentos_lista()) else factor(rep("Depois", nrow(tab)))
      tab$.grupo <- if (fator_nome_limpo() %in% names(tab)) tab[[fator_nome_limpo()]] else "Impacto"
      ggplot2::ggplot(tab, ggplot2::aes(x = .momento, y = .unidade, group = .unidade)) +
        {if ("momento" %in% names(tab) && length(unique(tab$momento)) > 1)
          ggplot2::geom_line(color = "#62B6B7")} +
        ggplot2::geom_point(color = "#2E7D8F", size = 3) +
        {if (tipo_resolvido == "longitudinal") ggplot2::facet_grid(.grupo ~ ., scales = "free_y", space = "free_y") else ggplot2::facet_wrap(~ .grupo, scales = "free_y")} +
        ggplot2::labs(x = "Momento", y = "Identificador da unidade",
          title = "Unidades e momentos previstos",
          subtitle = if ("momento" %in% names(tab)) "Pontos ligados representam medições da mesma unidade" else "Cada ponto representa um sítio no recorte após a mudança") + ggplot2::theme_minimal()
    })
    output$desenho_observacional <- shiny::renderPlot({ esquema_observacional() }, res = 110)
    output$resumo_observacional_intro <- shiny::renderUI({
      shiny::p(shiny::strong("Pergunta: "), ou_vazio(input$pergunta, "A definir"))
    })
    output$resumo_observacional_tabela <- shiny::renderTable({
      tab <- tabela_coleta_dados()
      unidade <- if (identical(tipo_resolvido, "gradiente")) "estacao" else unidade_coluna_nome()
      if (tipo_resolvido == "gradiente") return(data.frame(
        Item = c("Variável do gradiente", "Faixa prevista", "Estações / UAs", "Linhas por campanha", "Variáveis de resposta", "Itens previstos nos pools"),
        Quantidade = c(gradiente_coluna(), sprintf("%g a %g %s", min(estacoes_valores()), max(estacoes_valores()), gradiente_unidade()),
          nrow(tab), nrow(tab), ou_vazio(input$n_vars_resposta, 2), sum(tab$pool))))
      if (tipo_resolvido == "longitudinal") return(data.frame(
        Item = c("Grupos", "Meta de UAs por grupo ao final", "Perda esperada (%)", "UAs iniciais por grupo", "UAs distintas iniciais", "Momentos por UA", "Linhas da coleta", "Medidas previstas por UA e visita"),
        Quantidade = c(length(grupos_lista()), ou_vazio(input$n_uas, 4), ou_vazio(input$perda_pct, 20), n_longitudinal(),
          length(unique(tab[[unidade]])), length(momentos_lista()), nrow(tab), ou_vazio(input$n_medidas, 60))))
      data.frame(Item = c("Unidades distintas", "Momentos por unidade", "Linhas da coleta", "Variáveis de resposta"),
        Quantidade = c(length(unique(tab[[unidade]])),
          if ("momento" %in% names(tab)) length(unique(tab$momento)) else 1,
          nrow(tab), ou_vazio(input$n_vars_resposta, 2)))
    }, striped = TRUE, bordered = TRUE, spacing = "s", rownames = FALSE)
    cuidados_transversal <- c(
      "Uma linha da coleta representa uma UA: uma unidade física declarada (pool = 1) ou uma amostra composta. O número de UAs é o n do planejamento; sua independência depende da seleção e da origem das amostras.",
      "Especifique a unidade física que forma cada UA, por exemplo, 1 peixe, 1 garrafa de água (1 L) ou 1 kg de sedimento. Se houver pool, cada unidade deve contribuir com a mesma massa ou volume e integrar uma única UA.",
      "Com dois fatores, cada UA recebe uma categoria de cada fator. Um pool não deve misturar categorias dos fatores em comparação. O n é contado por combinação e não duplicado por haver dois fatores.",
      "Medidas internas e réplicas de bancada não aumentam o n. Guarde as leituras individuais e registre o resumo adequado por UA. A ficha transversal não tem n_medidas; documente a contagem nos registros de campo. Pool indica mistura física, não média.",
      "Defina a população, a elegibilidade e a seleção dentro de cada grupo antes da coleta. Registre lote, local e data; indivíduos do mesmo lote podem apresentar dependência.",
      "Quando a UA for uma estação, delimite primeiro o quadro amostral e registre as coordenadas. Não existe distância universal que garanta independência: hidrodinâmica, maré, conectividade, heterogeneidade do habitat e estudo-piloto orientam o espaçamento mínimo. Estações no mesmo sistema descrevem esse sistema; não replicam automaticamente outros sistemas.",
      "No exemplo de bexigas, confira a massa no pior caso e o consumo de todos os ensaios. Os pools e as metas da curadoria ainda precisam dessa conferência; não são uma recomendação universal.",
      "O estudo compara grupos preexistentes e descreve associações. Se espécie e origem estiverem confundidas (por exemplo, cultivo e captura), a diferença não pode ser atribuída somente à espécie.",
      "Pools diferentes podem alterar a variabilidade. A escolha da análise depende da resposta, da independência e dos pressupostos; o tamanho do pool, sozinho, não determina o teste."
    )
    orientacoes_transversal <- shiny::reactive({
      dic <- dicionario_dados()
      rbind(
        orientacoes_contexto(),
        data.frame(secao = "Planejamento", campo = c("Pergunta", "Local e período", "Seleção", "Procedimentos", "Item ou porção física", "Distância mínima entre estações (m)", "Hidrodinâmica e estudo-piloto", "Coordenadas previstas"),
          orientacao = c(ou_vazio(input$pergunta, "A definir"), ou_vazio(input$local_periodo, "A definir"),
            ou_vazio(input$criterios_coleta, "A definir"), ou_vazio(input$procedimentos_coleta, "A definir"),
            ou_vazio(input$unidade_item, "item a definir"),
            ou_vazio(input$espacamento_minimo_m, "A definir após piloto e avaliação hidrodinâmica"),
            ou_vazio(input$justificativa_espacamento, "A definir antes da coleta"),
            if (isTRUE(ou_vazio(input$registrar_coordenadas, FALSE))) sprintf("Latitude e longitude WGS 84 por estação; precisão horizontal máxima: %d m.", as.integer(ou_vazio(input$precisao_gps_m, 5))) else "Não solicitadas nesta ficha")),
        data.frame(secao = "Preenchimento", campo = c("Como preencher", "Exemplo ilustrativo", "Ausências"),
          orientacao = c("Preserve ua, grupo, replica e pool previstos. Preencha as respostas, lote_origem, data_coleta e observacoes. O cabeçalho está na primeira linha; não acrescente títulos acima dele.",
            "Exemplo fictício: lote_origem = barco_01; data_coleta = 2026-10-15. Para uma resposta em %, digite 12,5 (sem o símbolo). Este exemplo não é um dado coletado.",
            obs_ausencias)),
        data.frame(secao = "Colunas", campo = dic$coluna,
          orientacao = paste0(dic$descricao, ifelse(nzchar(dic$unidade), paste0(" [", dic$unidade, "]"), ""))),
        data.frame(secao = "Cuidados", campo = paste("Cuidado", seq_along(cuidados_transversal)), orientacao = cuidados_transversal)
      )
    })
    output$baixar_planilha <- shiny::downloadHandler(
      # O nome do arquivo acompanha o delineamento.
      filename = function() {
        prefixo <- if (identical(tipo_resolvido, "gradiente")) "planilha_coleta_gradiente_" else if (tipo_resolvido == "longitudinal") "planilha_coleta_longitudinal_" else if (tipo_resolvido == "impacto") "planilha_coleta_impacto_" else "planilha_coleta_transversal_"
        paste0(prefixo, format(Sys.Date(), "%Y-%m-%d"), ".xlsx")
      },
      content = function(file) {
        coleta <- tabela_coleta_dados()
        dicionario <- dicionario_dados()
        if (identical(tipo_resolvido, "transversal_comparativo")) {
          escrever_excel_transversal(coleta, orientacoes_transversal(), file)
          return(invisible(NULL))
        }
        if (tipo_resolvido == "longitudinal") {
          orientacoes <- orientacoes_longitudinal()
          escrever_excel_transversal(coleta, orientacoes, file, tabelas = list(
            coleta = coleta, unidades = unidades_longitudinal(),
            calendario = calendario_longitudinal(), orientacoes = orientacoes))
          return(invisible(NULL))
        }
        if (tipo_resolvido == "gradiente") {
          orientacoes <- orientacoes_gradiente()
          escrever_excel_transversal(coleta, orientacoes, file, tabelas = list(
            coleta = coleta, estacoes = estacoes_gradiente(), orientacoes = orientacoes))
          return(invisible(NULL))
        }
        if (tipo_resolvido == "impacto") {
          orientacoes <- orientacoes_impacto()
          escrever_excel_transversal(coleta, orientacoes, file, tabelas = list(
            coleta = coleta, sitios = sitios_impacto(), campanhas = campanhas_impacto(), orientacoes = orientacoes))
          return(invisible(NULL))
        }
        ficha_tbl <- ficha_tabela(ficha())
        writexl::write_xlsx(list(coleta = coleta, dicionario = dicionario, ficha_planejamento = ficha_tbl), file)
      }
    )

    # Downloads: Dicionário CSV
    output$baixar_dicionario <- shiny::downloadHandler(
      filename = function() paste0("dicionario_dados_", format(Sys.Date(), "%Y-%m-%d"), ".csv"),
      content = function(file) {
        utils::write.csv(dicionario_dados(), file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )

    # Downloads: Relatório Word (.docx)
    output$baixar_relatorio <- shiny::downloadHandler(
      filename = function() paste0("relatorio_planejamento_observacional_", format(Sys.Date(), "%Y-%m-%d"), ".docx"),
      content = function(file) {
        if (identical(tipo_resolvido, "transversal_comparativo")) {
          escrever_word_transversal(file, ou_vazio(input$pergunta, "A definir"),
            texto_metodologia_artigo_str(), resumo_transversal(), dicionario_dados(),
            cuidados_transversal, tabela_coleta_dados(), esquema_transversal(),
            item = ou_vazio(input$unidade_item, "item a definir"))
          return(invisible(NULL))
        }
        if (tipo_resolvido == "longitudinal") {
          tab <- tabela_coleta_dados()
          resumo <- data.frame(Grupo = grupos_lista(), UAs_iniciais = n_longitudinal(),
            UAs_desejadas = ou_vazio(input$n_uas, 4), Momentos = length(momentos_lista()),
            Linhas = n_longitudinal() * length(momentos_lista()))
          escrever_word_transversal(file, ou_vazio(input$pergunta, "A definir"),
            texto_metodologia_artigo_str(), resumo, dicionario_dados(),
            cuidados_longitudinal, tab, esquema_observacional(), longitudinal = TRUE,
            fonte = "Curadoria EAPA: Planejamento_05_LONGITUDINAL (outubro de 2026). Exemplos didáticos de mesas de ostras e viveiros de tambaqui. Apoio: Hurlbert (1984), Matthews et al. (1990), Laird e Ware (1982), Diggle et al. (2002), Zuur et al. (2009), Fitzmaurice, Laird e Ware (2011) e Wickham (2014).")
          return(invisible(NULL))
        }
        if (tipo_resolvido == "gradiente") {
          tab <- tabela_coleta_dados()
          resumo <- data.frame(Estação = tab$estacao, Valor_previsto = tab[[gradiente_coluna()]],
            UAs = 1L, check.names = FALSE)
          resumo[["Itens previstos"]] <- tab$pool
          escrever_word_transversal(file, ou_vazio(input$pergunta, "A definir"),
            texto_metodologia_artigo_str(), resumo, dicionario_dados(), cuidados_gradiente,
            tab, esquema_observacional(),
            titulo_documento = "Planejamento — Delineamento observacional de gradiente",
            fonte = paste("Planejamento observacional EAPA: estudo de gradiente.",
              "As referências específicas e decisões pendentes são as declaradas pelo pesquisador na metodologia."))
          return(invisible(NULL))
        }
        if (tipo_resolvido == "impacto") {
          sitios <- sitios_impacto()
          resumo <- data.frame(Grupo = unique(sitios$condicao))
          resumo$UAs_iniciais <- as.integer(table(factor(sitios$condicao, levels = resumo$Grupo)))
          resumo$Campanhas <- nrow(campanhas_impacto())
          resumo$Subamostras_por_visita <- inteiro_impacto(input$impacto_subamostras, 3)
          resumo$Linhas <- resumo$UAs_iniciais * resumo$Campanhas * resumo$Subamostras_por_visita
          escrever_word_transversal(file, ou_vazio(input$pergunta, "A definir"), texto_metodologia_artigo_str(),
            resumo, dicionario_dados(), cuidados_impacto, tabela_coleta_dados(), esquema_observacional(),
            longitudinal = TRUE, impacto = TRUE, titulo_documento = paste("Planejamento de estudo de impacto", toupper(tipo_impacto())),
            fonte = "Curadoria EAPA Planejamento_01_IMPACTO, revisada em outubro de 2026. Apoio: Hurlbert (1984), Stewart-Oaten et al. (1986), Underwood (1994), Downes et al. (2002) e Smokorowski e Randall (2017). As contagens descrevem o plano; não validam independência nem causalidade.")
          return(invisible(NULL))
        }
        pasta <- tempfile("relatorio_obs_")
        dir.create(pasta)
        antigo <- getwd()
        on.exit(setwd(antigo), add = TRUE)
        on.exit(unlink(pasta, recursive = TRUE), add = TRUE)
        
        texto_metodo <- texto_metodologia_artigo_str()
        coleta <- tabela_coleta_dados()
        dicionario <- dicionario_dados()
        
        utils::write.csv(coleta, file.path(pasta, "planilha_coleta.csv"), row.names = FALSE, fileEncoding = "UTF-8")
        utils::write.csv(dicionario, file.path(pasta, "dicionario.csv"), row.names = FALSE, fileEncoding = "UTF-8")
        
        # O Word recebe título e cuidados conforme o delineamento.
        titulo_doc <- if (identical(tipo_resolvido, "gradiente")) {
          "Ficha de Planejamento — Estudo de Gradiente"
        } else {
          "Ficha de Planejamento — Transversal Comparativo"
        }
        cuidados_doc <- if (identical(tipo_resolvido, "gradiente")) {
          c(
            "- **Unidade amostral:** cada estação ocupa uma linha; medidas internas são resumidas conforme a resposta. Pool conta somente os itens de uma mistura física.",
            "- **Cobertura da faixa:** distribuir as estações por toda a faixa do gradiente, incluindo os valores intermediários.",
            "- **Autocorrelação espacial:** estações muito próximas tendem a valores parecidos só pela proximidade; aumentar o espaçamento.",
            "- **Pseudorréplica:** subamostras da mesma estação não são repetições independentes; a repetição vem das várias estações."
          )
        } else {
          c(
            "- **Massa ou volume equitativo:** Cada item deve contribuir com a mesma quantidade de biomassa para o pool.",
            "- **Indivíduo único:** Nenhum item participa de mais de uma UA.",
            "- **Réplicas analíticas:** Duplicatas de bancada medem precisão de pipetagem e devem ser resumidas pela média.",
            "- **Interpretação de variâncias sob pools desiguais:** As médias são comparáveis, mas variâncias de grupos com pools diferentes refletem misturas distintas (Var = σ²/k)."
          )
        }
        qmd_lines <- c(
          "---",
          sprintf("title: \"%s\"", titulo_doc),
          "author: \"CatalyseR — Estatística Aplicada à Pesca e Aquicultura\"",
          sprintf("date: \"%s\"", format(Sys.Date(), "%d/%m/%Y")),
          "format:",
          "  docx: default",
          "---",
          "",
          "# Pergunta do Estudo",
          ou_vazio(input$pergunta, "Não declarada"),
          "",
          "# Material e Métodos (Texto para Artigo)",
          texto_metodo,
          "",
          "# Cuidados Metodológicos de Bancada e Coleta",
          cuidados_doc,
          "",
          "# Dicionário de Dados da Coleta",
          "```{r, echo=FALSE}",
          "dicionario <- read.csv('dicionario.csv', check.names=FALSE)",
          "knitr::kable(dicionario)",
          "```",
          "",
          "# Planilha de Coleta (Primeiras Linhas)",
          "```{r, echo=FALSE}",
          "planilha <- read.csv('planilha_coleta.csv', check.names=FALSE)",
          "knitr::kable(head(planilha, 25))",
          "```"
        )
        writeLines(qmd_lines, file.path(pasta, "relatorio.qmd"), useBytes = TRUE)
        setwd(pasta)
        system2("quarto", c("render", "relatorio.qmd", "--to", "docx"))
        resultado <- file.path(pasta, "relatorio.docx")
        if (!file.exists(resultado)) stop("Não foi possível gerar o Word.", call. = FALSE)
        file.copy(resultado, file, overwrite = TRUE)
      }
    )

    # Downloads: Projeto R (.zip)
    output$baixar_projeto <- shiny::downloadHandler(
      filename = function() paste0("projeto_planejamento_observacional_", format(Sys.Date(), "%Y-%m-%d"), ".zip"),
      content = function(file) {
        pasta_temp <- tempfile("proj_obs_")
        dir.create(pasta_temp)
        antigo_wd <- getwd()
        on.exit(setwd(antigo_wd), add = TRUE)
        on.exit(unlink(pasta_temp, recursive = TRUE), add = TRUE)
        
        proj_dir <- file.path(pasta_temp, "projeto_planejamento_observacional")
        dir.create(proj_dir)
        dir.create(file.path(proj_dir, "dados"))
        dir.create(file.path(proj_dir, "R"))
        
        # Dados da coleta
        coleta <- tabela_coleta_dados()
        dicionario <- dicionario_dados()
        writexl::write_xlsx(list(coleta = coleta, dicionario = dicionario), file.path(proj_dir, "dados", "planilha_coleta.xlsx"))
        utils::write.csv(coleta, file.path(proj_dir, "dados", "planilha_coleta.csv"), row.names = FALSE, fileEncoding = "UTF-8")
        
        # Script modelo R
        r_script <- c(
          "# Planejamento de Delineamento Observacional — CatalyseR",
          "# Importação e Análise Preliminar",
          "",
          "library(readxl)",
          "dados <- read_excel('dados/planilha_coleta.xlsx', sheet = 'coleta')",
          "",
          "# Visualização rápida da estrutura",
          "head(dados)",
          "summary(dados)"
        )
        writeLines(r_script, file.path(proj_dir, "R", "01_importar_coleta.R"))
        
        # Rproj
        rproj <- c("Version: 1.0", "RestoreWorkspace: Default", "SaveWorkspace: Default")
        writeLines(rproj, file.path(proj_dir, "projeto.Rproj"))
        
        setwd(pasta_temp)
        utils::zip(file, files = "projeto_planejamento_observacional")
      }
    )

    invisible(list(
      pergunta = shiny::reactive(input$pergunta),
      fator_nome = fator_nome_limpo,
      grupos = grupos_lista,
      ficha = ficha
    ))
  })
}
