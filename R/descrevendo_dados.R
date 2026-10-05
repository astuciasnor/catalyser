# Catálogo e código compartilhados pela interface e pelo Projeto R.
descricao_catalogo <- function() {
  list(
    # Cada pergunta começa por um único retrato; os detalhes só aparecem quando solicitados.
    explorar = c("Que dados eu tenho na mão?" = "panorama"),
    descrever = c("Que história cada variável conta?" = "retrato"),
    relacoes = c("Uma coisa varia com a outra?" = "relacao"),
    pressupostos = c("Normalidade e QQ-plot" = "normalidade", "Outliers" = "outliers"),
    transformar = c("Transformações sugeridas" = "transformacoes")
  )
}

descricao_tipo <- function(x) {
  if (inherits(x, c("Date", "POSIXt"))) "Data" else if (is.numeric(x)) {
    "Numérica"
  } else if (is.factor(x) || is.character(x) || is.logical(x)) "Categórica" else "Outro"
}

# Reconhece o tipo substantivo que orienta o retrato, sem tomar a decisão pelo pesquisador.
exploracao_tipo_variavel <- function(x, nome = "") {
  if (inherits(x, c("Date", "POSIXt"))) return("Data — conferir")
  if (is.ordered(x)) return("Categórica ordinal")
  if (is.factor(x) || is.character(x) || is.logical(x)) return("Categórica nominal")
  if (is.numeric(x)) {
    valores <- x[is.finite(x)]
    nome <- tolower(iconv(nome, to = "ASCII//TRANSLIT"))
    nome[is.na(nome)] <- ""
    # Nome e unidade são pistas; a ficha permite corrigir este palpite.
    return(dplyr::case_when(
      grepl("^(ano|mes|dia|year|month|day)$", nome) ~ "Categórica ordinal",
      grepl("(^|_)(id|codigo|code|identificador)($|_)", nome) ~ "Categórica nominal",
      grepl("(^|_)(aneis|anéis|rings|contagem|count|numero|n)($|_)", nome) ~ "Numérica discreta",
      grepl("_(mm|cm|m|mg|kg|g|ml|l|c|pct)$", nome) ~ "Numérica contínua",
      any(abs(valores - round(valores)) >= sqrt(.Machine$double.eps)) ~ "Numérica contínua",
      length(unique(valores)) >= 20L ~ "Numérica contínua",
      TRUE ~ "Numérica discreta"
    ))
  }
  "Outro — conferir"
}

# Confirme uma leitura por base; as correções nunca alteram seus valores.
exploracao_tipos <- function(dados, escolhas = NULL) {
  tipos <- stats::setNames(vapply(names(dados), function(nome) {
    exploracao_tipo_variavel(dados[[nome]], nome)
  }, character(1)), names(dados))
  permitidos <- c("Categórica nominal", "Categórica ordinal", "Numérica discreta", "Numérica contínua", "Data — conferir", "Outro — conferir")
  corrigir <- intersect(names(escolhas), names(tipos))
  corrigir <- corrigir[escolhas[corrigir] %in% permitidos]
  tipos[corrigir] <- escolhas[corrigir]
  tipos
}

# Converta categorias apenas numa cópia de apresentação, inclusive códigos numéricos.
exploracao_base_visual <- function(dados, tipos) {
  for (nome in names(tipos)[grepl("Categ", tipos)]) {
    if (identical(tipos[[nome]], "Categórica ordinal")) {
      valores <- sort(unique(dados[[nome]][!is.na(dados[[nome]])]))
      if (is.ordered(dados[[nome]])) valores <- levels(dados[[nome]])
      dados[[nome]] <- factor(dados[[nome]], levels = valores, ordered = TRUE)
    } else dados[[nome]] <- factor(dados[[nome]])
  }
  dados
}

# Navy, Amber e Teal separam os primeiros grupos; formas reforçam a leitura.
exploracao_cores <- function(n) rep(c("#0F3B5F", "#E89B3C", "#2E7D8F", "#62B6B7", "#E76F51"), length.out = n)

# Um vazio de grupo permanece visível e tem nome, em vez de virar a categoria NA.
exploracao_grupo <- function(x) {
  niveis <- if (is.factor(x)) levels(droplevels(x)) else sort(unique(as.character(x[!is.na(x)])))
  rotulos <- as.character(x)
  if (anyNA(x)) {
    rotulo_na <- "sem registro"
    while (rotulo_na %in% niveis) rotulo_na <- paste0(rotulo_na, " (ausente)")
    rotulos[is.na(x)] <- rotulo_na
    niveis <- c(niveis, rotulo_na)
  }
  factor(rotulos, levels = niveis)
}

# Traduz uma combinação de tipos em uma sugestão visível, sem transformar sugestão em regra.
exploracao_sugestao <- function(tipo_x, tipo_y = NULL) {
  categorica <- function(tipo) grepl("Categ", tipo, fixed = TRUE)
  numerica <- function(tipo) grepl("Num", tipo, fixed = TRUE)
  if (is.null(tipo_y)) {
    if (categorica(tipo_x)) return(list(
      retrato = "Tabela de frequências e barras",
      analise = "Qui-quadrado de aderência",
      destino = "Qui-quadrado (aderência)"
    ))
    if (numerica(tipo_x)) return(list(
      retrato = "Centro, dispersão, histograma e boxplot",
      analise = "Teste t de uma amostra",
      destino = "Teste t de Student"
    ))
  }
  if (categorica(tipo_x) && categorica(tipo_y)) return(list(
    retrato = "Tabela de contingência e barras agrupadas",
    analise = "Qui-quadrado de associação",
    destino = "Qui-quadrado de independência"
  ))
  if (xor(categorica(tipo_x), categorica(tipo_y)) && (numerica(tipo_x) || numerica(tipo_y))) return(list(
    retrato = "Resumo por grupo e boxplot por grupo",
    analise = "Teste t ou ANOVA",
    destino = "Teste t de Student"
  ))
  if (numerica(tipo_x) && numerica(tipo_y)) return(list(
    retrato = "Correlação e diagrama de dispersão",
    analise = "Regressão linear",
    destino = "Linear Simples"
  ))
  list(retrato = "Confira o significado das variáveis antes de avançar.", analise = "Uma análise depende da sua pergunta", destino = "")
}

# ---- Motor gráfico compartilhado -------------------------------------------

# Paleta oficial Ocean Gradient: a única fonte de cores dos dois menus.
# O cinza entra depois das cinco cores de marca para completar categorias
# extras sem repetir o navy da primeira posição.
cores_ocean <- function() {
  c(NAVY = "#0F3B5F", TEAL = "#2E7D8F", SEAFOAM = "#62B6B7",
    AMBER = "#E89B3C", CORAL = "#E76F51", CINZA = "#6C757D")
}

# Aplica a identidade Ocean a todos os gráficos exploratórios e ajustáveis.
tema_ocean <- function() {
  ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", color = "#0F3B5F"),
      plot.subtitle = ggplot2::element_text(color = "#495057"),
      axis.title = ggplot2::element_text(color = "#0F3B5F"),
      panel.grid.minor = ggplot2::element_blank(),
      legend.title = ggplot2::element_text(face = "bold")
    )
}

# Repete o gráfico por grupo somente quando uma faceta válida foi escolhida.
aplicar_faceta_ocean <- function(grafico, dados, faceta = NULL) {
  if (!is.null(faceta) && !identical(faceta, "nenhuma") && faceta %in% names(dados)) {
    return(grafico + ggplot2::facet_wrap(stats::as.formula(paste0("~`", faceta, "`"))))
  }
  grafico
}

# Desenha a distribuição usada pronta em Explorando e ajustável em Visualização.
desenhar_distribuicao <- function(dados, variavel, tipo = "densidade", classes = NULL,
                                  grupo = NULL, faceta = NULL) {
  if (!variavel %in% names(dados) || !is.numeric(dados[[variavel]])) stop("Escolha uma variável numérica.")
  x <- dados[[variavel]]
  x <- x[is.finite(x)]
  if (!length(x)) stop("Não há valores numéricos finitos para desenhar.")
  if (is.null(classes)) classes <- max(2L, grDevices::nclass.Sturges(x))
  com_grupo <- !is.null(grupo) && !identical(grupo, "nenhum") && grupo %in% names(dados)
  mapa <- if (com_grupo) ggplot2::aes(x = .data[[variavel]], fill = .data[[grupo]]) else ggplot2::aes(x = .data[[variavel]])
  histograma <- if (com_grupo) {
    ggplot2::geom_histogram(
      ggplot2::aes(y = ggplot2::after_stat(density)), bins = classes,
      color = "white", alpha = .55, position = "identity", na.rm = TRUE
    )
  } else {
    ggplot2::geom_histogram(
      ggplot2::aes(y = ggplot2::after_stat(density)), bins = classes,
      fill = cores_ocean()[["SEAFOAM"]], color = "white", alpha = .85, position = "identity", na.rm = TRUE
    )
  }
  p <- ggplot2::ggplot(dados, mapa) + histograma
  p <- if (identical(tipo, "poligono")) {
    p + ggplot2::geom_freqpoly(ggplot2::aes(y = ggplot2::after_stat(density)), bins = classes,
                               color = cores_ocean()[["NAVY"]], linewidth = 1, na.rm = TRUE)
  } else if (identical(tipo, "densidade")) {
    p + ggplot2::geom_density(alpha = .16, color = "#0F3B5F", linewidth = .7, na.rm = TRUE)
  } else p
  if (com_grupo) p <- p + ggplot2::scale_fill_manual(values = rep(
    exploracao_cores(5),
    length.out = length(unique(stats::na.omit(dados[[grupo]])))))
  aplicar_faceta_ocean(p, dados, faceta) + tema_ocean() +
    ggplot2::labs(title = "Distribuição da variável", x = variavel, y = "Densidade", fill = if (com_grupo) grupo else NULL)
}

# Desenha barras para categorias, contagens e frequências exploratórias.
desenhar_barras_ocean <- function(dados, variavel, grupo = NULL, peso = NULL,
                                  posicao = "dodge", faceta = NULL, mostrar_ausentes = TRUE) {
  if (!variavel %in% names(dados)) stop("Escolha uma variável disponível.")
  com_grupo <- !is.null(grupo) && !identical(grupo, "nenhum") && grupo %in% names(dados)
  com_peso <- !is.null(peso) && !identical(peso, "nenhum") && peso %in% names(dados)
  if (!mostrar_ausentes) dados <- dados[!is.na(dados[[variavel]]) &
    (if (com_grupo) !is.na(dados[[grupo]]) else TRUE), , drop = FALSE]
  dados[[variavel]] <- exploracao_grupo(dados[[variavel]])
  if (com_grupo) dados[[grupo]] <- exploracao_grupo(dados[[grupo]])
  mapa <- if (com_grupo) ggplot2::aes(x = .data[[variavel]], fill = .data[[grupo]]) else ggplot2::aes(x = .data[[variavel]])
  barras <- if (com_peso && com_grupo) {
    ggplot2::geom_bar(ggplot2::aes(weight = .data[[peso]]), position = posicao, na.rm = TRUE)
  } else if (com_peso) {
    ggplot2::geom_bar(ggplot2::aes(weight = .data[[peso]]), position = posicao, fill = cores_ocean()[["TEAL"]], na.rm = TRUE)
  } else if (com_grupo) {
    ggplot2::geom_bar(position = posicao, na.rm = TRUE)
  } else {
    ggplot2::geom_bar(position = posicao, fill = cores_ocean()[["TEAL"]], na.rm = TRUE)
  }
  p <- ggplot2::ggplot(dados, mapa) + barras
  if (com_grupo) p <- p + ggplot2::scale_fill_manual(values = rep(
    exploracao_cores(5),
    length.out = length(unique(stats::na.omit(dados[[grupo]])))))
  if (identical(posicao, "fill")) p <- p + ggplot2::scale_y_continuous(labels = scales::label_percent())
  aplicar_faceta_ocean(p, dados, faceta) + tema_ocean() +
    ggplot2::labs(title = "Categorias e composição", x = variavel,
                  y = if (com_peso) paste("Soma de", peso) else "Contagem", fill = if (com_grupo) grupo else NULL)
}

# Desenha caixa, violino ou ambos com a mesma regra nos dois menus.
desenhar_caixa_ocean <- function(dados, variavel, grupo = NULL, forma = "ambos", faceta = NULL, mostrar_ausentes = TRUE) {
  if (!variavel %in% names(dados) || !is.numeric(dados[[variavel]])) stop("Escolha uma variável numérica.")
  grupo_valido <- !is.null(grupo) && !identical(grupo, "nenhum") && grupo %in% names(dados)
  plotar <- dados
  plotar <- plotar[is.finite(plotar[[variavel]]), , drop = FALSE]
  if (grupo_valido && !mostrar_ausentes) plotar <- plotar[!is.na(plotar[[grupo]]), , drop = FALSE]
  plotar$grupo_visual <- exploracao_grupo(if (grupo_valido) plotar[[grupo]] else rep("Todas as observações", nrow(plotar)))
  p <- ggplot2::ggplot(plotar, ggplot2::aes(x = .data$grupo_visual, y = .data[[variavel]], fill = .data$grupo_visual))
  if (forma %in% c("violino", "ambos")) p <- p + ggplot2::geom_violin(alpha = .55, na.rm = TRUE)
  if (forma %in% c("caixa", "boxplot", "ambos")) p <- p + ggplot2::geom_boxplot(width = .22, alpha = .8, outlier.color = cores_ocean()[["CORAL"]], na.rm = TRUE)
  if (forma %in% c("pontos", "caixa_pontos", "violino_pontos")) {
    # Densidades são apoio; grupos pequenos ou constantes ficam somente com pontos.
    grupos <- split(plotar, plotar$grupo_visual, drop = TRUE)
    densos <- Filter(function(d) nrow(d) >= 3L && length(unique(d[[variavel]])) >= 2L, grupos)
    if (forma %in% c("pontos", "violino_pontos") && length(densos)) p <- p +
      ggplot2::geom_violin(data = do.call(rbind, densos), alpha = .18, color = NA, trim = TRUE)
    if (forma == "caixa_pontos") p <- p + ggplot2::geom_boxplot(width = .35, alpha = .18, outlier.shape = NA)
    p <- p + ggplot2::geom_point(
      ggplot2::aes(color = .data$grupo_visual, shape = .data$grupo_visual),
      position = ggplot2::position_jitter(width = .12, height = 0, seed = 42), alpha = .55, size = 1.7
    )
    chaves <- c("grupo_visual", if (!is.null(faceta) && faceta %in% names(plotar)) faceta)
    fatias <- split(seq_len(nrow(plotar)), interaction(plotar[chaves], drop = TRUE))
    resumo <- do.call(rbind, lapply(fatias, function(i) {
      z <- plotar[[variavel]][i]; n <- length(z)
      erro <- if (n >= 2L) stats::qt(.975, n - 1L) * stats::sd(z) / sqrt(n) else NA_real_
      cbind(plotar[i[1], chaves, drop = FALSE], media = mean(z), inferior = mean(z) - erro, superior = mean(z) + erro)
    }))
    if (!is.null(resumo)) p <- p +
      ggplot2::geom_errorbar(data = resumo, ggplot2::aes(x = .data$grupo_visual, ymin = .data$inferior, ymax = .data$superior), inherit.aes = FALSE, width = .08, na.rm = TRUE) +
      ggplot2::geom_point(data = resumo, ggplot2::aes(x = .data$grupo_visual, y = .data$media), inherit.aes = FALSE, shape = 23, fill = "white", size = 3)
    contagem <- table(plotar$grupo_visual)
    p <- p + ggplot2::scale_x_discrete(labels = function(x) paste0(x, "\n(n = ", contagem[x], ")")) +
      ggplot2::scale_color_manual(values = exploracao_cores(length(contagem))) +
      ggplot2::scale_shape_manual(values = rep(c(16, 17, 15, 18, 3), length.out = length(contagem))) +
      ggplot2::labs(subtitle = paste0("Pontos: observações · Losango: média · Barras: IC 95% da média (t).", if (!is.null(faceta) && faceta %in% names(plotar)) " n reúne as facetas." else ""))
  }
  aplicar_faceta_ocean(p, plotar, faceta) +
    ggplot2::scale_fill_manual(values = rep(
      exploracao_cores(5),
      length.out = length(unique(stats::na.omit(plotar$grupo_visual))))) +
    tema_ocean() + ggplot2::theme(legend.position = "none") +
    ggplot2::labs(title = "Centro, dispersão e valores distantes", x = if (grupo_valido) grupo else NULL, y = variavel)
}

# Desenha a relação entre duas medidas com tendência opcional.
desenhar_dispersao_ocean <- function(dados, x, y, grupo = NULL, tendencia = "lm", faceta = NULL,
                                    linha_por_grupo = TRUE, muitos_pontos = "pontos") {
  if (!all(c(x, y) %in% names(dados))) stop("Escolha duas variáveis disponíveis.")
  com_grupo <- !is.null(grupo) && !identical(grupo, "nenhum") && grupo %in% names(dados)
  p <- ggplot2::ggplot(dados, ggplot2::aes(x = .data[[x]], y = .data[[y]]))
  if (com_grupo) p <- p + ggplot2::geom_point(ggplot2::aes(color = .data[[grupo]], shape = .data[[grupo]]), alpha = .6, na.rm = TRUE) +
    ggplot2::scale_color_manual(values = rep(
      exploracao_cores(5),
      length.out = length(unique(stats::na.omit(dados[[grupo]]))))) +
    ggplot2::scale_shape_manual(values = rep(c(16, 17, 15, 18, 3), length.out = length(unique(stats::na.omit(dados[[grupo]])))))
  else if (identical(muitos_pontos, "hex") && requireNamespace("hexbin", quietly = TRUE)) p <- p +
    ggplot2::geom_hex(bins = 35, na.rm = TRUE) + ggplot2::scale_fill_gradient(low = "#E7EFEA", high = "#0F3B5F", name = "Observações")
  else p <- p + ggplot2::geom_point(color = cores_ocean()[["NAVY"]], alpha = .75, na.rm = TRUE)
  if (!is.null(tendencia) && !identical(tendencia, "nenhuma")) {
    p <- if (com_grupo && linha_por_grupo) p + ggplot2::geom_smooth(
      ggplot2::aes(color = .data[[grupo]], group = .data[[grupo]]),
      method = tendencia, formula = y ~ x, se = FALSE, na.rm = TRUE
    ) else p + ggplot2::geom_smooth(method = tendencia, formula = y ~ x, se = TRUE, color = cores_ocean()[["CORAL"]], fill = cores_ocean()[["AMBER"]], na.rm = TRUE)
  }
  aplicar_faceta_ocean(p, dados, faceta) + tema_ocean() +
    ggplot2::labs(title = "Relação entre duas variáveis", x = x, y = y, color = if (com_grupo) grupo else NULL)
}

# ---- Tabelas exploratórias --------------------------------------------------

# Resume as variáveis contínuas com as medidas essenciais para o primeiro olhar.
resumir_continuas <- function(dados, variaveis) {
  linhas <- lapply(variaveis, function(v) {
    x <- dados[[v]]
    validos <- x[!is.na(x)]
    sem_dados <- !length(validos)
    media <- if (sem_dados) NA_real_ else mean(validos)
    mediana <- if (sem_dados) NA_real_ else stats::median(validos)
    desvio <- if (length(validos) < 2L) NA_real_ else stats::sd(validos)
    amplitude <- if (sem_dados) NA_real_ else diff(range(validos))
    escala_desvio <- if (is.finite(desvio)) desvio else 0
    distante <- is.finite(media) && is.finite(mediana) && abs(media - mediana) > max(.1 * amplitude, .5 * escala_desvio)
    data.frame(variavel = v, n_validos = length(validos), ausentes = sum(is.na(x)),
      media = media, mediana = mediana, desvio_padrao = desvio,
      minimo = if (sem_dados) NA_real_ else min(validos), maximo = if (sem_dados) NA_real_ else max(validos),
      pista_assimetria = distante, stringsAsFactors = FALSE)
  })
  do.call(rbind, linhas)
}

# Constrói frequências por classes ou categorias conforme o tipo reconhecido.
tabela_frequencia_exploratoria <- function(dados, variavel, tipo, classes = NULL, ordenar_nominal = TRUE) {
  x <- dados[[variavel]]
  continua <- identical(tipo, "Numérica contínua")
  if (continua) {
    validos <- x[is.finite(x)]
    if (!length(validos)) stop("Não há valores contínuos válidos.")
    if (is.null(classes)) classes <- max(2L, grDevices::nclass.Sturges(validos))
    categorias <- cut(validos, breaks = classes, include.lowest = TRUE)
    nome <- "classe"
  } else {
    categorias <- x[!is.na(x)]
    if (!length(categorias)) stop("Não há categorias válidas.")
    nome <- "categoria"
  }
  fi <- table(categorias)
  nominal <- grepl("nominal", tipo, fixed = TRUE)
  if (!continua && nominal && isTRUE(ordenar_nominal)) fi <- sort(fi, decreasing = TRUE)
  proporcao <- as.numeric(prop.table(fi))
  tabela <- data.frame(rotulo = names(fi), frequencia = as.integer(fi), proporcao = proporcao,
    freq_acumulada = cumsum(as.integer(fi)), prop_acumulada = cumsum(proporcao), stringsAsFactors = FALSE)
  names(tabela)[1] <- nome
  tabela$modal <- tabela$frequencia == max(tabela$frequencia)
  tabela
}

#' Código de estudo de uma análise exploratória
#'
#' O mesmo código executa na interface e no projeto exportado. As transformações
#' são apenas comparadas: nenhuma altera a base de origem.
#' @param parametros Lista com analise, variavel e opções específicas do método.
#' @return Vetor de linhas R, que espera um data.frame chamado dados.
#' @export
catalyser_codigo_descricao <- function(parametros) {
  p <- parametros
  # Mantém o replay de resultados criados antes da reformulação da interface.
  modos <- c(unname(unlist(descricao_catalogo())), "estrutura", "faltantes", "tipos",
             "frequencias", "resumo", "histograma", "boxplot", "densidade",
             "correlacao", "dispersao", "marginais", "grupos",
             "resumo_continuas", "frequencia_exploratoria")
  if (length(p$analise) != 1L || !p$analise %in% modos) stop("Escolha uma análise válida.")
  literal <- function(x) paste(deparse(x, width.cutoff = 100L), collapse = "\n")
  linhas <- function(bloco) unlist(lapply(as.list(bloco)[-1L], deparse, width.cutoff = 100L), use.names = FALSE)
  opcoes <- list(variavel = p$variavel, variaveis = p$variaveis, outra = p$outra, grupo = p$grupo,
                 tipo = if (is.null(p$tipo)) "automatico" else p$tipo,
                 tipo_outra = if (is.null(p$tipo_outra)) "automatico" else p$tipo_outra,
                 metodo = if (is.null(p$metodo)) "pearson" else p$metodo,
                 classes = if (is.null(p$classes)) 30L else p$classes,
                 forma = if (is.null(p$forma)) "boxplot" else p$forma,
                 tendencia = if (is.null(p$tendencia)) "linear" else p$tendencia,
                 limite_z = if (is.null(p$limite_z)) 3 else p$limite_z)
  campos <- c(if (!p$analise %in% c("estrutura", "faltantes", "tipos", "panorama")) "variavel",
              if (p$analise == "resumo_continuas") "variaveis",
              if (p$analise %in% c("correlacao", "dispersao", "marginais", "relacao")) "outra",
              if (p$analise %in% c("retrato", "relacao", "frequencia_exploratoria")) "tipo",
              if (p$analise %in% "relacao") "tipo_outra",
              switch(p$analise, grupos = "grupo", correlacao = "metodo", histograma = "classes",
                     frequencia_exploratoria = "classes",
                     boxplot = "forma", dispersao = "tendencia", outliers = "limite_z", NULL))
  opcoes <- opcoes[campos]
  codigo <- c("# 1. Escolhas da análise (você pode alterá-las no RStudio).",
              vapply(names(opcoes), function(n) paste(n, "<-", literal(opcoes[[n]])), character(1)),
              "# 2. Conferência e preparo: a base original não é modificada.", linhas(quote({
    if (!is.data.frame(dados) || !ncol(dados)) stop("Carregue uma tabela com variáveis.")
    tabela <- detalhes <- sugestao <- grafico <- console <- NULL
    narrativa <- ""
    tema <- ggplot2::theme_minimal(base_size = 12)
  })))
  # As leituras confirmadas acompanham o código e o replay desta execução.
  codigo <- c(codigo, paste0("leituras <- ", literal(p$leituras)),
    paste0("exploracao_tipo_variavel <- ", literal(exploracao_tipo_variavel)),
    paste0("exploracao_tipos <- ", literal(exploracao_tipos)))
  if (identical(p$analise, "normalidade")) codigo <- c(codigo,
    paste0("grupo <- ", literal(p$grupo)),
    paste0("exploracao_grupo <- ", literal(exploracao_grupo)),
    paste0("exploracao_normalidade_grupos <- ", literal(exploracao_normalidade_grupos)))
  exploracao <- p$analise %in% c("estrutura", "faltantes", "tipos", "panorama")
  if (exploracao) codigo <- c(codigo, linhas(quote({
    tipos <- exploracao_tipos(dados, leituras)
    ausentes <- vapply(dados, function(coluna) sum(is.na(coluna)), integer(1))
    narrativa <- sprintf("A base contém %d observações e %d variáveis. Tipos detectados pela classe armazenada; códigos numéricos de categorias precisam ser conferidos por você.", nrow(dados), ncol(dados))
  }))) else codigo <- c(codigo, linhas(quote({
    if (length(variavel) != 1L || !variavel %in% names(dados)) stop("Selecione uma variável disponível.")
    coluna <- dados[[variavel]]
  })))
  # O código exportado repete a sugestão para que o Projeto R conte a mesma história da tela.
  if (p$analise %in% c("retrato", "relacao")) codigo <- c(codigo, linhas(quote({
    tipo_detectado <- function(coluna, nome = "") {
      if (nome %in% names(leituras)) return(unname(leituras[[nome]]))
      exploracao_tipo_variavel(coluna, nome)
    }
    tipo_confirmado <- function(detectado, escolha) if (identical(escolha, "automatico")) detectado else escolha
    categorica <- function(tipo) grepl("Categ", tipo, fixed = TRUE)
    numerica <- function(tipo) grepl("Num", tipo, fixed = TRUE)
    tipo_x <- tipo_confirmado(tipo_detectado(coluna, variavel), tipo)
    sugestao_caminho <- function(tipo_x, tipo_y = NULL) {
      if (is.null(tipo_y) && categorica(tipo_x)) return(list("Tabela de frequências e barras", "Qui-quadrado de aderência", "Qui-quadrado (aderência)"))
      if (is.null(tipo_y) && numerica(tipo_x)) return(list("Centro, dispersão, histograma e boxplot", "Teste t de uma amostra", "Teste t de Student"))
      if (categorica(tipo_x) && categorica(tipo_y)) return(list("Tabela de contingência e barras agrupadas", "Qui-quadrado de associação", "Qui-quadrado de independência"))
      if (xor(categorica(tipo_x), categorica(tipo_y)) && (numerica(tipo_x) || numerica(tipo_y))) return(list("Resumo por grupo e boxplot por grupo", "Teste t ou ANOVA", "Teste t de Student"))
      if (numerica(tipo_x) && numerica(tipo_y)) return(list("Correlação e diagrama de dispersão", "Regressão linear", "Linear Simples"))
      list("Confira o significado das variáveis antes de avançar.", "Uma análise depende da sua pergunta", "")
    }
  })))
  if (!exploracao && !p$analise %in% c("frequencias", "retrato", "relacao", "resumo_continuas", "frequencia_exploratoria")) codigo <- c(codigo, linhas(quote({
    if (!is.numeric(coluna) || inherits(coluna, c("Date", "POSIXt"))) stop("Escolha uma variável numérica; datas têm outra interpretação.")
    manter <- is.finite(coluna)
    x <- coluna[manter]
    if (!length(x)) stop("Não há valores numéricos finitos para analisar.")
    narrativa <- sprintf("%s: %d valores analisados; %d ausentes ou não finitos excluídos.", variavel, length(x), sum(!manter))
  })))
  if (p$analise %in% c("correlacao", "dispersao", "marginais")) codigo <- c(codigo, linhas(quote({
    if (length(outra) != 1L || !outra %in% names(dados) || identical(variavel, outra)) stop("Selecione duas variáveis diferentes.")
    if (!is.numeric(dados[[outra]]) || inherits(dados[[outra]], c("Date", "POSIXt"))) stop("O segundo eixo precisa ser numérico.")
    manter <- is.finite(coluna) & is.finite(dados[[outra]])
    pares <- data.frame(x = coluna[manter], y = dados[[outra]][manter])
    if (nrow(pares) < 3L || length(unique(pares$x)) < 2L || length(unique(pares$y)) < 2L) stop("São necessários pelo menos três pares e variação nos dois eixos.")
    narrativa <- sprintf("%s × %s: %d pares completos e finitos; %d linhas excluídas. Associação não demonstra causalidade.", variavel, outra, nrow(pares), sum(!manter))
  })))
  # O Projeto R recebe as mesmas funções canônicas, sem uma segunda implementação dos gráficos.
  if (p$analise %in% c("retrato", "relacao", "resumo_continuas", "frequencia_exploratoria",
                       "histograma", "boxplot", "densidade", "dispersao", "grupos", "panorama")) {
    funcoes <- c("exploracao_cores", "exploracao_grupo", "exploracao_retratos", "cores_ocean", "tema_ocean", "aplicar_faceta_ocean", "desenhar_distribuicao",
                 "desenhar_barras_ocean", "desenhar_caixa_ocean", "desenhar_dispersao_ocean",
                 "resumir_continuas", "tabela_frequencia_exploratoria")
    codigo <- c(codigo, "# 3. Funções compartilhadas por Explorando e Visualização.",
      unlist(lapply(funcoes, function(nome) c(
        paste0(nome, " <- ", paste(deparse(get(nome), width.cutoff = 100L), collapse = "\n"))
      )), use.names = FALSE))
  }
  bloco <- switch(p$analise,
    resumo_continuas = quote({
      variaveis <- intersect(variaveis, names(dados))
      if (!length(variaveis)) stop("Não há variáveis contínuas disponíveis.")
      tabela <- resumir_continuas(dados, variaveis)
      grafico <- desenhar_distribuicao(dados, variavel, tipo = "densidade")
      linha <- tabela[match(variavel, tabela$variavel), ]
      narrativa <- sprintf("%s tem %d valores válidos, %d ausentes e amplitude de %s a %s.",
        variavel, linha$n_validos, linha$ausentes, linha$minimo, linha$maximo)
    }),
    frequencia_exploratoria = quote({
      tabela <- tabela_frequencia_exploratoria(dados, variavel, tipo, classes = classes)
      if (identical(tipo, "Numérica contínua")) {
        grafico <- desenhar_distribuicao(dados, variavel, tipo = "histograma", classes = classes)
      } else {
        grafico <- desenhar_barras_ocean(dados, variavel)
      }
      rotulo <- names(tabela)[1]
      narrativa <- sprintf("%s é a classe ou categoria modal, com %d observações.",
        paste(tabela[[rotulo]][tabela$modal], collapse = ", "), max(tabela$frequencia))
    }),
    panorama = quote({
      # Cada tipo recebe uma pista própria, curta o bastante para orientar sem escolher pelo aluno.
      pistas <- vapply(tipos, function(tipo_coluna) {
        switch(tipo_coluna,
          "Categórica nominal" = "Conte as categorias e compare as frequências com barras",
          "Categórica ordinal" = "Respeite a ordem e observe frequências e percentuais acumulados",
          "Numérica discreta" = "Conte os valores e examine centro, dispersão e barras ou histograma",
          "Numérica contínua" = "Examine centro, dispersão, histograma e boxplot",
          "Confira a natureza desta coluna antes de analisar"
        )
      }, character(1))
      # Os cabeçalhos conversam com o aluno em vez de expor nomes internos do motor.
      tabela <- data.frame("Nome da variável" = names(dados), "Tipo de variável" = unname(tipos),
                           "Valores ausentes" = ausentes, "Pistas para começar" = unname(pistas),
                           row.names = NULL, check.names = FALSE)
      grafico <- exploracao_retratos(dados, tipos)
      narrativa <- paste(narrativa, "Comece conferindo a natureza de cada coluna: um código numérico pode representar uma categoria. A CatalyseR sugere um retrato, mas você pode mudar essa leitura no próximo passo.")
    }),
    retrato = quote({
      caminho <- sugestao_caminho(tipo_x)
      sugestao <- list(retrato = caminho[[1]], analise = caminho[[2]], destino = caminho[[3]])
      if (categorica(tipo_x)) {
        # Ordinais preservam a ordem natural; nominais destacam primeiro as categorias mais frequentes.
        contagem <- table(coluna, useNA = "no")
        if (!grepl("ordinal", tipo_x, fixed = TRUE)) contagem <- sort(contagem, decreasing = TRUE)
        if (!sum(contagem)) stop("Não há categorias observadas para explorar.")
        tabela <- data.frame(Categoria = names(contagem), "Frequência" = as.integer(contagem),
                             Percentual = 100 * as.numeric(contagem) / sum(contagem), check.names = FALSE)
        # A expansão acrescenta uma leitura; não repete a tabela já visível.
        if (grepl("ordinal", tipo_x, fixed = TRUE)) {
          detalhes <- data.frame(
            Categoria = names(contagem),
            "Frequência acumulada" = cumsum(as.integer(contagem)),
            "Percentual acumulado" = 100 * cumsum(as.numeric(contagem)) / sum(contagem),
            check.names = FALSE
          )
        } else {
          detalhes <- data.frame(
            Medida = c("Categorias observadas", "Categoria mais frequente", "Frequência da mais frequente", "Valores ausentes"),
            Valor = c(length(contagem), names(contagem)[1], as.integer(contagem[1]), sum(is.na(coluna)))
          )
        }
        dados_barras <- data.frame(categoria = rep(as.character(tabela$Categoria), tabela[["Frequência"]]))
        dados_barras$categoria <- factor(dados_barras$categoria, levels = tabela$Categoria)
        grafico <- desenhar_barras_ocean(dados_barras, "categoria") + ggplot2::coord_flip() +
          ggplot2::labs(x = variavel, title = "O retrato recomendado: barras por categoria")
        narrativa <- sprintf("%s foi lida como %s. Conte as categorias antes de procurar diferenças: %d observações válidas e %d ausentes.", variavel, tipo_x, sum(contagem), sum(is.na(coluna)))
      } else if (numerica(tipo_x)) {
        x <- coluna[is.finite(coluna)]
        if (!length(x)) stop("Não há valores numéricos finitos para explorar.")
        tabela <- data.frame(Medida = c("Observações", "Média", "Mediana", "Desvio padrão", "IQR"),
                             Valor = c(length(x), mean(x), stats::median(x), stats::sd(x), stats::IQR(x)))
        detalhes <- data.frame("Mínimo" = min(x), Q1 = unname(stats::quantile(x, .25)), "Média" = mean(x),
                               Mediana = stats::median(x), Q3 = unname(stats::quantile(x, .75)), "Máximo" = max(x),
                               "Variância" = stats::var(x), Amplitude = diff(range(x)))
        grafico <- desenhar_distribuicao(data.frame(valor = x), "valor", tipo = "densidade",
          classes = min(30L, max(5L, floor(sqrt(length(x)))))) +
          ggplot2::geom_vline(xintercept = mean(x), colour = "#0F3B5F", linewidth = .8) +
          ggplot2::geom_vline(xintercept = stats::median(x), colour = "#E76F51", linetype = 2, linewidth = .8) +
          ggplot2::labs(x = variavel, title = "O retrato recomendado: distribuição, média e mediana")
        narrativa <- sprintf("%s foi lida como %s. A linha navy marca a média; a tracejada coral, a mediana. Quando se afastam, investigue a forma antes de escolher a medida de centro.", variavel, tipo_x)
      } else stop("Este tipo precisa ser conferido antes de receber um retrato automático.")
    }),
    relacao = quote({
      if (length(outra) != 1L || !outra %in% names(dados) || identical(variavel, outra)) stop("Escolha duas variáveis diferentes.")
      tipo_y <- tipo_confirmado(tipo_detectado(dados[[outra]], outra), tipo_outra)
      caminho <- sugestao_caminho(tipo_x, tipo_y)
      sugestao <- list(retrato = caminho[[1]], analise = caminho[[2]], destino = caminho[[3]])
      if (categorica(tipo_x) && categorica(tipo_y)) {
        completos <- stats::na.omit(data.frame(x = coluna, y = dados[[outra]]))
        if (!nrow(completos)) stop("Não há pares completos para explorar.")
        tabela <- as.data.frame.matrix(table(completos$x, completos$y))
        tabela <- cbind(Grupo = rownames(tabela), tabela); rownames(tabela) <- NULL
        # As proporções por linha ajudam a comparar perfis com totais diferentes.
        proporcoes <- 100 * prop.table(table(completos$x, completos$y), margin = 1)
        detalhes <- as.data.frame.matrix(round(proporcoes, 2))
        detalhes <- cbind(Grupo = rownames(detalhes), detalhes); rownames(detalhes) <- NULL
        grafico <- desenhar_barras_ocean(completos, "x", grupo = "y") +
          ggplot2::labs(x = variavel, fill = outra, title = "O retrato recomendado: categorias lado a lado")
      } else if (xor(categorica(tipo_x), categorica(tipo_y)) && (numerica(tipo_x) || numerica(tipo_y))) {
        grupo <- if (categorica(tipo_x)) coluna else dados[[outra]]
        valor <- if (numerica(tipo_x)) coluna else dados[[outra]]
        completos <- data.frame(grupo = grupo, valor = valor); completos <- completos[!is.na(completos$grupo) & is.finite(completos$valor), ]
        if (!nrow(completos)) stop("Não há pares completos para explorar.")
        partes <- split(completos$valor, completos$grupo)
        # Dois grupos apontam para o teste t; três ou mais apontam para a ANOVA de um fator.
        sugestao <- if (length(partes) == 2L) {
          list(retrato = "Resumo por grupo e boxplot por grupo", analise = "Teste t para grupos independentes", destino = "Teste t de Student")
        } else if (length(partes) > 2L) {
          list(retrato = "Resumo por grupo e boxplot por grupo", analise = "ANOVA de um fator", destino = "ANOVA de um fator")
        } else {
          list(retrato = "Resumo por grupo e boxplot por grupo", analise = "Comparar grupos exige ao menos duas categorias observadas", destino = "")
        }
        tabela <- do.call(rbind, lapply(names(partes), function(nome) data.frame(Grupo = nome, n = length(partes[[nome]]), "Média" = mean(partes[[nome]]), Mediana = stats::median(partes[[nome]]), DP = stats::sd(partes[[nome]]))))
        # Quartis e extremos ficam recolhidos até o aluno pedir mais contexto.
        detalhes <- do.call(rbind, lapply(names(partes), function(nome) data.frame(
          Grupo = nome, "Mínimo" = min(partes[[nome]]), Q1 = unname(stats::quantile(partes[[nome]], .25)),
          Mediana = stats::median(partes[[nome]]), Q3 = unname(stats::quantile(partes[[nome]], .75)), "Máximo" = max(partes[[nome]])
        )))
        grafico <- desenhar_caixa_ocean(completos, "valor", grupo = "grupo", forma = "caixa") +
          ggplot2::labs(x = if (categorica(tipo_x)) variavel else outra, y = if (numerica(tipo_x)) variavel else outra, title = "O retrato recomendado: distribuição por grupo")
      } else if (numerica(tipo_x) && numerica(tipo_y)) {
        completos <- data.frame(x = coluna, y = dados[[outra]]); completos <- completos[is.finite(completos$x) & is.finite(completos$y), ]
        if (nrow(completos) < 3L || length(unique(completos$x)) < 2L || length(unique(completos$y)) < 2L) stop("São necessários pelo menos três pares com variação nos dois eixos.")
        teste <- stats::cor.test(completos$x, completos$y)
        tabela <- data.frame(Pares = nrow(completos), "Correlação" = unname(teste$estimate), p_valor = teste$p.value)
        # Pearson e Spearman lado a lado mostram que a escolha do coeficiente também é uma decisão.
        teste_spearman <- stats::cor.test(completos$x, completos$y, method = "spearman", exact = FALSE)
        detalhes <- data.frame(
          "Método" = c("Pearson", "Spearman"),
          "Correlação" = c(unname(teste$estimate), unname(teste_spearman$estimate)),
          p_valor = c(teste$p.value, teste_spearman$p.value)
        )
        grafico <- desenhar_dispersao_ocean(completos, "x", "y", tendencia = "lm") +
          ggplot2::labs(x = variavel, y = outra, title = "O retrato recomendado: nuvem de pontos e reta")
      } else stop("Confira o significado das duas variáveis antes de explorar a relação.")
      narrativa <- sprintf("%s (%s) e %s (%s): este é um retrato exploratório. Ele aponta uma família de análise, mas a pergunta e o delineamento fecham a decisão.", variavel, tipo_x, outra, tipo_y)
    }),
    estrutura = quote({
      tabela <- data.frame("Variável" = names(dados), Tipo = unname(tipos),
                          Classe = vapply(dados, function(z) paste(class(z), collapse = "/"), character(1)),
                          "Observações" = nrow(dados), Ausentes = ausentes, row.names = NULL)
    }),
    tipos = quote({
      sugestoes <- vapply(tipos, function(tipo_coluna) {
        if (grepl("Num", tipo_coluna, fixed = TRUE)) "Resumo, histograma, boxplot e densidade"
        else if (grepl("Categ", tipo_coluna, fixed = TRUE)) "Frequências; grupos para comparações"
        else if (grepl("^Data", tipo_coluna)) "Série temporal; não tratar como medida contínua aqui"
        else "Conferir e preparar a coluna antes de analisar"
      }, character(1))
      tabela <- data.frame("Variável" = names(dados), Tipo = unname(tipos),
                          "Próximo_passo" = unname(sugestoes))
    }),
    faltantes = quote({
      tabela <- data.frame("Variável" = names(dados), Ausentes = ausentes,
                          Percentual = if (nrow(dados)) 100 * ausentes / nrow(dados) else NA_real_, row.names = NULL)
      grafico <- ggplot2::ggplot(tabela, ggplot2::aes(x = stats::reorder(.data[["Variável"]], .data$Ausentes), y = .data$Ausentes)) +
        ggplot2::geom_col(fill = "#E76F51") + ggplot2::coord_flip() + tema +
        ggplot2::labs(x = NULL, y = "Número de valores ausentes", title = "Mapa de dados faltantes")
      narrativa <- paste(narrativa, "Percentuais calculados sobre todas as linhas; tabela vazia tem percentual indefinido.")
    }),
    frequencias = quote({
      if (!(is.factor(coluna) || is.character(coluna) || is.logical(coluna))) stop("Escolha uma variável categórica.")
      contagem <- table(coluna, useNA = "no")
      if (!sum(contagem)) stop("Não há categorias observadas.")
      tabela <- data.frame(Categoria = names(contagem), Absoluta = as.integer(contagem),
                          Relativa_percentual = 100 * as.numeric(contagem) / sum(contagem),
                          Acumulada = cumsum(as.integer(contagem)),
                          Acumulada_percentual = 100 * cumsum(as.numeric(contagem)) / sum(contagem))
      narrativa <- sprintf("%s: %d observações válidas; %d ausentes excluídas do denominador. A frequência acumulada segue a ordem exibida e só tem interpretação substantiva se as categorias tiverem uma ordem científica.", variavel, sum(contagem), sum(is.na(coluna)))
      grafico <- ggplot2::ggplot(tabela, ggplot2::aes(x = factor(.data$Categoria, levels = .data$Categoria), y = .data$Absoluta)) +
        ggplot2::geom_col(fill = "#2E7D8F") + tema + ggplot2::labs(x = variavel, y = "Frequência absoluta")
    }),
    resumo = quote({
      media <- mean(x)
      desvio <- stats::sd(x)
      momento_2 <- mean((x - media)^2)
      tabela <- data.frame(n = length(x), "Média" = media, Mediana = stats::median(x),
                          "Mínimo" = min(x), Q1 = unname(stats::quantile(x, .25)),
                          Q3 = unname(stats::quantile(x, .75)), "Máximo" = max(x),
                          Amplitude = diff(range(x)), "Variância" = stats::var(x), DP = desvio,
                          IQR = stats::IQR(x), CV_percentual = if (all(x >= 0) && media > 0) 100 * desvio / media else NA_real_,
                          Assimetria = if (momento_2 > 0) mean((x - media)^3) / momento_2^1.5 else NA_real_,
                          Excesso_curtose = if (momento_2 > 0) mean((x - media)^4) / momento_2^2 - 3 else NA_real_)
      tabela <- data.frame(Medida = names(tabela), Valor = unname(unlist(tabela)))
      narrativa <- paste(narrativa, "Assimetria e excesso de curtose são momentos empíricos não corrigidos. CV só faz sentido em escala de razão, com zero significativo; não interprete CV em escalas como temperatura Celsius.")
    }),
    histograma = quote({
      if (length(classes) != 1L || !is.finite(classes) || classes < 2 || classes > 200 || classes != floor(classes)) stop("Escolha de 2 a 200 classes inteiras.")
      limites <- if (diff(range(x)) > 0) seq(min(x), max(x), length.out = classes + 1L) else seq(x[1] - .5, x[1] + .5, length.out = classes + 1L)
      histograma <- graphics::hist(x, breaks = limites, plot = FALSE, include.lowest = TRUE)
      tabela <- data.frame(Limite_inferior = utils::head(histograma$breaks, -1), Limite_superior = utils::tail(histograma$breaks, -1),
                          "Ponto_médio" = histograma$mids, "Frequência" = histograma$counts)
      grafico <- desenhar_distribuicao(data.frame(valor = x), "valor", tipo = "poligono", classes = classes) +
        ggplot2::labs(x = variavel, title = "Histograma e polígono de frequências")
    }),
    boxplot = quote({
      if (!forma %in% c("boxplot", "violino", "ambos")) stop("Escolha boxplot, violino ou ambos.")
      if (forma != "boxplot" && (length(x) < 2L || length(unique(x)) < 2L)) stop("O violino precisa de pelo menos dois valores distintos.")
      grafico <- desenhar_caixa_ocean(data.frame(valor = x), "valor", forma = forma) +
        ggplot2::labs(y = variavel)
      tabela <- data.frame(n = length(x), Mediana = stats::median(x), Q1 = unname(stats::quantile(x, .25)), Q3 = unname(stats::quantile(x, .75)))
    }),
    densidade = quote({
      if (length(x) < 2L || length(unique(x)) < 2L) stop("A densidade precisa de pelo menos dois valores distintos.")
      estimativa <- stats::density(x, kernel = "gaussian", bw = "nrd0")
      tabela <- data.frame(n = length(x), Largura_de_banda = estimativa$bw)
      grafico <- desenhar_distribuicao(data.frame(valor = x), "valor", tipo = "densidade") +
        ggplot2::labs(x = variavel, y = "Densidade estimada", title = "KDE gaussiana; banda automática nrd0")
      narrativa <- paste(narrativa, "A suavização pode ultrapassar limites físicos da variável; não representa probabilidade nesses valores.")
    }),
    correlacao = quote({
      if (!metodo %in% c("pearson", "spearman")) stop("Escolha Pearson ou Spearman.")
      teste <- stats::cor.test(pares$x, pares$y, method = metodo, exact = FALSE)
      intervalo <- if (is.null(teste$conf.int)) c(NA_real_, NA_real_) else teste$conf.int
      tabela <- data.frame("Método" = metodo, n = nrow(pares), "Correlação" = unname(teste$estimate),
                          "Estatística" = unname(teste$statistic), p_valor = teste$p.value,
                          IC95_inferior = intervalo[1], IC95_superior = intervalo[2])
      console <- utils::capture.output(teste)
      narrativa <- paste(narrativa, if (metodo == "spearman") "Spearman usa p-valor aproximado e não apresenta IC neste procedimento." else "O IC de Pearson é calculado a partir de quatro pares. A inferência pressupõe pares independentes e condições adequadas ao método.",
                         if (teste$p.value < .05) "Há evidência de associação pelo teste (nível de 5%)." else "Não houve evidência suficiente de associação pelo teste (nível de 5%).")
    }),
    dispersao = quote({
      if (!tendencia %in% c("nenhuma", "linear", "loess")) stop("Escolha uma tendência disponível.")
      grafico <- desenhar_dispersao_ocean(pares, "x", "y",
        tendencia = if (tendencia == "linear") "lm" else tendencia) +
        ggplot2::labs(x = variavel, y = outra)
      tabela <- data.frame(Pares = nrow(pares), "Excluídos" = sum(!manter), "Tendência" = tendencia)
    }),
    marginais = quote({
      central <- ggplot2::ggplot(pares, ggplot2::aes(.data$x, .data$y)) + ggplot2::geom_point(colour = "#2E7D8F", alpha = .55) + tema + ggplot2::labs(x = variavel, y = outra)
      superior <- ggplot2::ggplot(pares, ggplot2::aes(x = .data$x, y = "")) + ggplot2::geom_boxplot(fill = "#62B6B7") + ggplot2::scale_x_continuous(limits = range(pares$x)) + ggplot2::theme_void()
      lateral <- ggplot2::ggplot(pares, ggplot2::aes(x = "", y = .data$y)) + ggplot2::geom_boxplot(fill = "#62B6B7") + ggplot2::scale_y_continuous(limits = range(pares$y)) + ggplot2::theme_void()
      grafico <- cowplot::plot_grid(superior, NULL, central, lateral, ncol = 2, rel_widths = c(4, 1), rel_heights = c(1, 4), align = "hv", axis = "tblr")
      tabela <- data.frame(Pares = nrow(pares), "Excluídos" = sum(!manter))
      narrativa <- paste(narrativa, "Os boxplots marginais usam os mesmos pares completos do painel central.")
    }),
    grupos = quote({
      if (length(grupo) != 1L || !grupo %in% names(dados) || !(is.factor(dados[[grupo]]) || is.character(dados[[grupo]]) || is.logical(dados[[grupo]]))) stop("Selecione um fator categórico para os grupos.")
      manter <- is.finite(coluna) & !is.na(dados[[grupo]])
      agrupados <- data.frame(valor = coluna[manter], grupo = droplevels(as.factor(dados[[grupo]][manter])))
      if (!nrow(agrupados)) stop("Não há observações completas para os grupos.")
      partes <- split(agrupados$valor, agrupados$grupo, drop = TRUE)
      tabela <- do.call(rbind, lapply(names(partes), function(nome) data.frame(Grupo = nome, n = length(partes[[nome]]), "Média" = mean(partes[[nome]]), Mediana = stats::median(partes[[nome]]), DP = stats::sd(partes[[nome]]))))
      grafico <- desenhar_caixa_ocean(agrupados, "valor", grupo = "grupo", forma = "caixa") +
        ggplot2::labs(x = grupo, y = variavel)
      narrativa <- sprintf("%s por %s: %d observações; %d linhas ausentes ou não finitas excluídas. Comparação visual, sem teste de diferença entre grupos.", variavel, grupo, nrow(agrupados), sum(!manter))
    }),
    normalidade = quote({
      n <- length(x)
      if (n < 3L || stats::sd(x) == 0) stop("A normalidade requer pelo menos três valores e variação.")
      teste <- if (n <= 5000L) stats::shapiro.test(x) else NULL
      tabela <- data.frame(n = n, W = if (is.null(teste)) NA_real_ else unname(teste$statistic),
                          p_valor = if (is.null(teste)) NA_real_ else teste$p.value,
                          "Situação" = if (is.null(teste)) "Shapiro não calculado: limite de 5.000; sem subamostragem automática" else "Shapiro-Wilk")
      probabilidades <- stats::ppoints(n)
      qq <- data.frame("Teórico" = stats::qnorm(probabilidades), Observado = sort(x),
                       Inferior = mean(x) + stats::sd(x) * stats::qnorm(stats::qbeta(.025, seq_len(n), n + 1 - seq_len(n))),
                       Superior = mean(x) + stats::sd(x) * stats::qnorm(stats::qbeta(.975, seq_len(n), n + 1 - seq_len(n))))
      grafico <- ggplot2::ggplot(qq, ggplot2::aes(.data[["Teórico"]], .data$Observado)) +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$Inferior, ymax = .data$Superior), fill = "#62B6B7", alpha = .4) +
        ggplot2::geom_abline(intercept = mean(x), slope = stats::sd(x), colour = "#E76F51") +
        ggplot2::geom_point(colour = "#0F3B5F", alpha = .5) + tema +
        ggplot2::labs(x = "Quantis normais teóricos", y = variavel, title = "QQ-plot — banda pontual aproximada de 95%")
      console <- if (is.null(teste)) tabela[["Situação"]] else utils::capture.output(teste)
      narrativa <- paste(narrativa, "A banda usa estatísticas de ordem beta com média e DP estimados: é pontual e aproximada, não uma banda simultânea. Em modelos, confira os resíduos, não apenas os dados brutos.",
                         if (is.null(teste)) "Acima de 5.000 valores, examine o gráfico; Shapiro não foi executado." else if (teste$p.value < .05) "Shapiro aponta evidência de desvio da normalidade (5%)." else "Shapiro não encontrou evidência suficiente de desvio; isso não prova normalidade.")
      if (!is.null(grupo) && grupo %in% names(dados)) {
        por_grupo <- exploracao_normalidade_grupos(dados, variavel, grupo)
        tabela <- por_grupo$tabela
        grafico <- por_grupo$grafico
        narrativa <- por_grupo$narrativa
        console <- utils::capture.output(tabela)
      }
    }),
    outliers = quote({
      if (length(limite_z) != 1L || !is.finite(limite_z) || limite_z <= 0) stop("O limite do escore-z precisa ser positivo.")
      quartis <- stats::quantile(x, c(.25, .75), names = FALSE)
      limites <- quartis + c(-1.5, 1.5) * diff(quartis)
      z <- if (length(x) > 1L && stats::sd(x) > 0) (x - mean(x)) / stats::sd(x) else rep(NA_real_, length(x))
      tabela <- data.frame(Linha_na_base = which(manter), Valor = x, Escore_z = z,
                          Fora_IQR = x < limites[1] | x > limites[2], Fora_z = abs(z) > limite_z)
      grafico <- ggplot2::ggplot(tabela, ggplot2::aes(.data$Linha_na_base, .data$Valor, colour = .data$Fora_IQR)) +
        ggplot2::geom_point() + ggplot2::geom_hline(yintercept = limites, linetype = 2) + tema +
        ggplot2::scale_colour_manual(values = c("FALSE" = "#2E7D8F", "TRUE" = "#E76F51")) +
        ggplot2::labs(x = "Linha na base selecionada", y = variavel, colour = "Fora de 1,5 IQR")
      narrativa <- paste(narrativa, sprintf("%d pontos sinalizados pelo IQR; %d pelo limite |z| > %g. São alertas para investigação, não motivos automáticos para exclusão. Escore-z indefinido quando não há dispersão.", sum(tabela$Fora_IQR), sum(tabela$Fora_z, na.rm = TRUE), limite_z))
    }),
    transformacoes = quote({
      candidatas <- list(Original = x)
      dominio_log <- all(x > 0)
      dominio_raiz <- all(x >= 0)
      if (dominio_log) candidatas$Log <- log(x)
      if (dominio_raiz) candidatas$Raiz <- sqrt(x)
      lambda <- NA_real_
      boxcox_ok <- dominio_log && length(x) >= 3L && length(unique(x)) > 1L
      if (boxcox_ok) {
        modelo_referencia <- stats::lm(x ~ 1, y = TRUE, qr = TRUE)
        perfil <- MASS::boxcox(modelo_referencia, lambda = seq(-2, 2, .1), plotit = FALSE)
        lambda <- perfil$x[which.max(perfil$y)]
        candidatas$Box_Cox <- if (abs(lambda) < 1e-8) log(x) else expm1(lambda * log(x)) / lambda
      }
      tabela <- data.frame("Transformação" = c("Original", "Log", "Raiz", "Box_Cox"),
                          "Disponível" = c(TRUE, dominio_log, dominio_raiz, boxcox_ok),
                          "Condição" = c("Referência sem alteração", "Exige valores estritamente positivos", "Exige valores não negativos", "Exige valores positivos, n ≥ 3 e variação; referência com intercepto"),
                          Lambda = c(NA_real_, NA_real_, NA_real_, lambda))
      grafico_dados <- do.call(rbind, lapply(names(candidatas), function(nome) data.frame("Transformação" = nome, Valor = candidatas[[nome]])))
      grafico <- ggplot2::ggplot(grafico_dados, ggplot2::aes(sample = .data$Valor)) + ggplot2::stat_qq(colour = "#2E7D8F") +
        ggplot2::stat_qq_line(colour = "#E76F51") + ggplot2::facet_wrap(stats::as.formula("~ Transformação"), scales = "free_y") + tema +
        ggplot2::labs(x = "Quantis normais teóricos", y = "Quantis observados", title = "Compare a forma; não escolha por p-valor")
      grafico_dados$Transformação <- factor(grafico_dados$Transformação, levels = names(candidatas))
      # As duas filas precisam manter a mesma ordem para comparar cada transformação.
      grafico$data <- grafico_dados
      histogramas <- ggplot2::ggplot(grafico_dados, ggplot2::aes(x = .data$Valor)) +
        ggplot2::geom_histogram(bins = 25, fill = "#2E7D8F", color = "white") +
        ggplot2::facet_wrap(~ Transformação, scales = "free", nrow = 1) + tema +
        ggplot2::labs(x = "Valor na escala indicada", y = "Observações", title = "Antes e depois: distribuição")
      grafico <- patchwork::wrap_plots(histogramas, grafico + ggplot2::facet_wrap(~ Transformação, scales = "free", nrow = 1), ncol = 1)
      narrativa <- paste(narrativa, "Sugestões exploratórias, não correções automáticas. Box-Cox usa um modelo só com intercepto e uma grade de lambda de −2 a 2; refaça a avaliação no modelo científico e em seus resíduos. Considere unidades, zeros e interpretação biológica. Nenhuma transformação foi aplicada à base.",
                         if (is.finite(lambda) && abs(lambda) == 2) "Ótimo na borda da grade: lambda ainda não está bem localizado." else "")
    })
  )
  c(codigo, "# 4. Cálculo e apresentação da análise escolhida.", linhas(bloco),
    "# 5. Objetos usados pela interface e pelo relatório.",
    # Entrega detalhes e sugestão separadamente para a interface manter o primeiro olhar limpo.
    "resultado <- list(narrativa = narrativa, tabela = tabela, detalhes = detalhes, sugestao = sugestao, grafico = grafico, console = console)",
    "resultado <- resultado[!vapply(resultado, is.null, logical(1))]")
}

# Um retrato por variável, organizado por papel, com até doze cartões por página.
exploracao_retratos <- function(dados, tipos = exploracao_tipos(dados), pagina = 1L) {
  papeis <- c("Numérica contínua" = "Medidas", "Numérica discreta" = "Contagens",
              "Categórica nominal" = "Grupos", "Categórica ordinal" = "Grupos",
              "Data — conferir" = "Tempo", "Outro — conferir" = "Conferir")
  ordem <- names(tipos)[order(match(unname(papeis[tipos]), c("Medidas", "Contagens", "Grupos", "Tempo", "Conferir")))]
  ordem <- ordem[seq_along(ordem) > (pagina - 1L) * 12L & seq_along(ordem) <= pagina * 12L]
  if (!length(ordem)) return(NULL)
  graficos <- lapply(ordem, function(nome) {
    x <- dados[[nome]]; tipo <- tipos[[nome]]
    percentual <- if (nrow(dados)) 100 * sum(is.na(x)) / nrow(dados) else 0
    if (tipo == "Numérica contínua" && any(is.finite(x))) {
      g <- desenhar_distribuicao(dados, nome, tipo = "histograma")
    } else if (grepl("Categ|discreta", tipo) && any(!is.na(x))) {
      contagem <- table(x, useNA = "no")
      if (!identical(tipo, "Categórica ordinal")) contagem <- sort(contagem, decreasing = TRUE)
      tab <- data.frame(categoria = names(contagem)[seq_len(min(10L, length(contagem)))], n = as.numeric(contagem)[seq_len(min(10L, length(contagem)))])
      tab$categoria <- factor(tab$categoria, levels = rev(tab$categoria))
      g <- ggplot2::ggplot(tab, ggplot2::aes(x = .data$categoria, y = .data$n)) +
        ggplot2::geom_col(fill = "#2E7D8F") + ggplot2::coord_flip() + tema_ocean() +
        ggplot2::labs(x = NULL, y = "Observações", caption = if (length(contagem) > 10L) "Dez valores mostrados; veja todos na tabela." else NULL)
    } else {
      g <- ggplot2::ggplot() + ggplot2::annotate("text", x = 0, y = 0, label = "Confira a coluna na ficha") + ggplot2::theme_void()
    }
    g + ggplot2::labs(title = gsub("_", " ", nome),
      subtitle = sprintf("%s · %s · %.1f%% ausentes", papeis[[tipo]], tipo, percentual)) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 11), plot.subtitle = ggplot2::element_text(size = 8), axis.text = ggplot2::element_text(size = 8))
  })
  patchwork::wrap_plots(graficos, ncol = 3)
}

# Ausências são mostradas por linha original, sem ordenar ou remover indivíduos.
exploracao_mapa_ausentes <- function(dados) {
  if (!nrow(dados) || !ncol(dados)) return(NULL)
  ordem <- names(sort(vapply(dados, function(x) sum(is.na(x)), integer(1)), decreasing = TRUE))
  tab <- data.frame(linha = rep(seq_len(nrow(dados)), times = ncol(dados)),
                    variavel = rep(ordem, each = nrow(dados)),
                    ausente = as.vector(is.na(dados[ordem])))
  ggplot2::ggplot(tab, ggplot2::aes(x = factor(.data$variavel, levels = ordem), y = .data$linha, fill = .data$ausente)) +
    ggplot2::geom_raster() + ggplot2::scale_y_reverse() +
    ggplot2::scale_fill_manual(values = c("FALSE" = "#E7EFEA", "TRUE" = "#E89B3C"), labels = c("Preenchida", "Ausente"), drop = FALSE, name = NULL) +
    tema_ocean() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 25, hjust = 1)) +
    ggplot2::labs(x = NULL, y = "Linha na base", subtitle = sprintf("%d células ausentes em %d linhas. Nenhuma linha foi excluída.", sum(is.na(dados)), sum(!stats::complete.cases(dados))))
}

# Alertas pedem conferência; linhas iguais não comprovam indivíduos duplicados.
exploracao_saude <- function(dados) {
  iguais <- sum(duplicated(dados))
  constantes <- sum(vapply(dados, function(x) length(unique(x[!is.na(x)])) <= 1L, logical(1)))
  ausentes <- sum(vapply(dados, anyNA, logical(1)))
  data.frame(verificacao = c("Linhas iguais, conferir", "Colunas sem variação, conferir", "Colunas com ausências"),
             quantidade = c(iguais, constantes, ausentes), stringsAsFactors = FALSE)
}

# Dentro de cada grupo, confira a forma; um p-valor isolado não certifica pressupostos.
exploracao_normalidade_grupos <- function(dados, variavel, grupo) {
  categorias <- exploracao_grupo(dados[[grupo]])
  grupos <- split(dados[[variavel]], categorias, drop = TRUE)
  tabela <- do.call(rbind, lapply(names(grupos), function(nome) {
    x <- grupos[[nome]]; x <- x[is.finite(x)]; n <- length(x)
    valido <- n >= 3L && n <= 5000L && stats::sd(x) > 0
    teste <- if (valido) stats::shapiro.test(x) else NULL
    data.frame(Grupo = nome, n = n, W = if (valido) unname(teste$statistic) else NA_real_,
      p_valor = if (valido) teste$p.value else NA_real_,
      Situação = if (valido) "Conferir QQ e delineamento" else "Sem teste: requer n entre 3 e 5.000 e variação")
  }))
  plotar <- data.frame(valor = dados[[variavel]], grupo = categorias)
  plotar <- plotar[is.finite(plotar$valor), , drop = FALSE]
  grafico <- ggplot2::ggplot(plotar, ggplot2::aes(sample = .data$valor)) +
    ggplot2::stat_qq(color = "#2E7D8F", alpha = .6) + ggplot2::stat_qq_line(color = "#E89B3C") +
    ggplot2::facet_wrap(~ grupo, scales = "free") + ggplot2::theme_minimal(base_size = 12) +
    ggplot2::labs(x = "Quantis normais teóricos", y = variavel, title = "Normalidade dentro dos grupos")
  list(tabela = tabela, grafico = grafico,
    narrativa = "Shapiro-Wilk e QQ por grupo, usando somente medidas finitas. Grupos sem registro ficam identificados. Ausência de evidência não prova normalidade; no modelo, examine resíduos, variância e independência pelo delineamento.")
}

#' Executar uma análise do menu Descrevendo Dados
#' @param dados Base compartilhada ou derivada, sem modificações por esta função.
#' @param parametros Opções usadas por [catalyser_codigo_descricao()].
#' @return Lista com narrativa, tabela e, conforme o método, gráfico e console.
#' @export
catalyser_descricao <- function(dados, parametros) {
  ambiente <- new.env(parent = baseenv())
  ambiente$dados <- dados
  eval(parse(text = catalyser_codigo_descricao(parametros)), envir = ambiente)
  ambiente$resultado
}
