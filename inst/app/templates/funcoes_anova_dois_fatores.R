# Funções canônicas da ANOVA de dois fatores (fatorial com interação).
#
# A versão v1 assume desenho balanceado e observações independentes. O módulo
# expõe duas fontes de variação principais, a interação e os resíduos; o mesmo
# objeto alimenta a interface, a Comunicação de Resultados e o Projeto R.

if (!exists("%||%")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}

anova2_validar_entrada <- function(df, dep_var, fator_a, fator_b) {
  if (!is.data.frame(df)) return("A base precisa ser um data.frame.")
  ausentes <- setdiff(c(dep_var, fator_a, fator_b), names(df))
  if (length(ausentes)) {
    return(sprintf("A base não contém: %s.", paste(ausentes, collapse = ", ")))
  }
  if (!is.numeric(df[[dep_var]])) {
    return(sprintf("A resposta '%s' precisa ser numérica.", dep_var))
  }
  if (any(c(dep_var) == c(fator_a, fator_b))) {
    return("A resposta e os dois fatores precisam ser variáveis diferentes.")
  }
  completos <- stats::complete.cases(df[c(dep_var, fator_a, fator_b)])
  if (sum(completos) < 6L) {
    return("A ANOVA de dois fatores precisa de pelo menos seis observações completas.")
  }
  da <- droplevels(as.factor(df[[fator_a]][completos]))
  db <- droplevels(as.factor(df[[fator_b]][completos]))
  if (nlevels(da) < 2L || nlevels(db) < 2L) {
    return("Cada fator precisa ter pelo menos dois níveis com dados.")
  }
  celulas <- table(da, db)
  if (any(celulas < 2L)) {
    ruins <- which(celulas < 2L, arr.ind = TRUE)
    nomes <- apply(ruins, 1, function(i) paste(rownames(celulas)[i[1]],
                                                colnames(celulas)[i[2]], sep = " × "))
    return(sprintf("Cada célula do fatorial precisa de pelo menos duas observações: %s.",
                   paste(nomes, collapse = ", ")))
  }
  NULL
}

anova2_num <- function(x, dig = 3L) {
  if (!length(x) || is.na(x[[1]])) return("-")
  formatC(as.numeric(x[[1]]), format = "f", digits = dig, decimal.mark = ",")
}

anova2_p <- function(x) {
  if (!length(x) || is.na(x[[1]])) return("-")
  if (x[[1]] < 0.001) "< 0,001" else anova2_num(x, 3L)
}

# ---- Identidade visual dos gráficos fatoriais ------------------------------
#
# Estes helpers trabalham apenas com a tabela de médias por célula já produzida
# pelo núcleo analítico. Assim, a interface não volta aos dados brutos nem cria
# dependências de dplyr/patchwork para desenhar os dois painéis.

anova2_paleta_ocean <- function(n, nomes = NULL) {
  n <- max(1L, as.integer(n))
  base <- c("#2E7D8F", "#E76F51", "#0F3B5F", "#E89B3C", "#62B6B7")
  cores <- if (n <= length(base)) base[seq_len(n)] else
    grDevices::colorRampPalette(base)(n)
  if (!is.null(nomes) && length(nomes) == n) names(cores) <- nomes
  cores
}

anova2_tema_ocean <- function(tema = "minimal", base_size = 12) {
  tema_base <- switch(
    as.character(tema %||% "minimal"),
    classic = ggplot2::theme_classic(base_size = base_size),
    bw = ggplot2::theme_bw(base_size = base_size),
    gray = ggplot2::theme_gray(base_size = base_size),
    light = ggplot2::theme_light(base_size = base_size),
    ggplot2::theme_minimal(base_size = base_size)
  )
  tema_base + ggplot2::theme(
    plot.title = ggplot2::element_text(
      colour = "#0F3B5F", face = "bold", size = base_size
    ),
    plot.subtitle = ggplot2::element_text(colour = "#2E7D8F"),
    axis.title = ggplot2::element_text(colour = "#0F3B5F"),
    axis.text = ggplot2::element_text(colour = "#0F3B5F"),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(colour = "#E6EDF2"),
    legend.position = "bottom",
    legend.title = ggplot2::element_text(colour = "#0F3B5F", face = "bold")
  )
}

anova2_rotulo_media <- function(x, dig = 2L) {
  ifelse(
    is.na(x), "-",
    formatC(as.numeric(x), format = "f", digits = dig, decimal.mark = ",")
  )
}

anova2_grafico_interacao_celulas <- function(
    celulas, fator_a, fator_b, resposta, niveis_a, niveis_b,
    nivel_confianca = 0.95, titulo = NULL, rotulo_x = NULL, rotulo_y = NULL,
    tema = "minimal") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  d <- celulas
  d$fator_a <- factor(d$fator_a, levels = niveis_a)
  d$fator_b <- factor(d$fator_b, levels = niveis_b)
  cores <- anova2_paleta_ocean(length(niveis_b), niveis_b)
  formas <- stats::setNames(rep(c(16, 15, 17, 18, 8, 3), length.out = length(niveis_b)),
                            niveis_b)

  ggplot2::ggplot(
    d,
    ggplot2::aes(
      x = fator_a, y = media, colour = fator_b, shape = fator_b,
      group = fator_b
    )
  ) +
    ggplot2::geom_line(linewidth = 1.05) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = ic_inferior, ymax = ic_superior),
      width = 0.12, linewidth = 0.75
    ) +
    ggplot2::scale_colour_manual(values = cores) +
    ggplot2::scale_shape_manual(values = formas) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.06, 0.12))) +
    anova2_tema_ocean(tema) +
    ggplot2::labs(
      title = titulo %||% sprintf("Perfis de médias: %s × %s", fator_a, fator_b),
      subtitle = sprintf(
        "Médias observadas por célula; hastes = IC %.0f%%",
        100 * nivel_confianca
      ),
      x = rotulo_x %||% fator_a,
      y = rotulo_y %||% resposta,
      colour = fator_b,
      shape = fator_b
    )
}

anova2_grafico_combinacoes_celulas <- function(
    celulas, fator_a, fator_b, resposta, niveis_a, niveis_b,
    nivel_confianca = 0.95, titulo = NULL, rotulo_x = NULL, rotulo_y = NULL,
    tema = "minimal") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  d <- celulas
  d$fator_a <- factor(d$fator_a, levels = niveis_a)
  d$fator_b <- factor(d$fator_b, levels = niveis_b)
  d$rotulo_media <- anova2_rotulo_media(d$media)
  cores <- anova2_paleta_ocean(length(niveis_b), niveis_b)
  posicao <- ggplot2::position_dodge(width = 0.78)

  ggplot2::ggplot(
    d,
    ggplot2::aes(x = fator_a, y = media, fill = fator_b, group = fator_b)
  ) +
    ggplot2::geom_col(
      position = posicao, width = 0.68, colour = "white", linewidth = 0.3
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = ic_inferior, ymax = ic_superior),
      position = posicao, width = 0.13, linewidth = 0.7, colour = "#0F3B5F"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = rotulo_media),
      position = posicao, vjust = -0.55, size = 3, colour = "#0F3B5F"
    ) +
    ggplot2::scale_fill_manual(values = cores) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.04, 0.18))) +
    ggplot2::expand_limits(y = 0) +
    anova2_tema_ocean(tema) +
    ggplot2::labs(
      title = titulo %||% sprintf("Médias das combinações: %s × %s", fator_a, fator_b),
      subtitle = sprintf(
        "Valores sobre as barras; hastes = IC %.0f%%",
        100 * nivel_confianca
      ),
      x = rotulo_x %||% fator_a,
      y = rotulo_y %||% resposta,
      fill = fator_b
    )
}

calcular_anova_dois_fatores <- function(df, dep_var, fator_a, fator_b,
                                         nivel_confianca = 0.95) {
  mensagem <- anova2_validar_entrada(df, dep_var, fator_a, fator_b)
  if (!is.null(mensagem)) stop(mensagem, call. = FALSE)
  conf <- as.numeric(nivel_confianca)
  if (!is.finite(conf) || conf <= 0 || conf >= 1) conf <- 0.95

  colunas <- c(dep_var, fator_a, fator_b)
  completos <- stats::complete.cases(df[colunas])
  d <- data.frame(
    resposta = as.numeric(df[[dep_var]][completos]),
    fator_a = droplevels(as.factor(df[[fator_a]][completos])),
    fator_b = droplevels(as.factor(df[[fator_b]][completos]))
  )
  excluidos <- sum(!completos)
  modelo <- stats::aov(resposta ~ fator_a * fator_b, data = d)
  an <- summary(modelo)[[1]]
  nomes_efeitos <- c("fator_a", "fator_b", "fator_a:fator_b")
  nomes_an <- trimws(rownames(an))
  linhas <- match(nomes_efeitos, nomes_an)
  residuo <- match("Residuals", nomes_an)
  df_efeitos <- as.numeric(an$Df[linhas])
  ss_efeitos <- as.numeric(an$`Sum Sq`[linhas])
  qm_efeitos <- as.numeric(an$`Mean Sq`[linhas])
  f_efeitos <- as.numeric(an$`F value`[linhas])
  p_efeitos <- as.numeric(an$`Pr(>F)`[linhas])
  df_res <- as.numeric(an$Df[residuo])
  ss_res <- as.numeric(an$`Sum Sq`[residuo])
  qm_res <- as.numeric(an$`Mean Sq`[residuo])
  ss_total <- sum((d$resposta - mean(d$resposta))^2)

  nomes_pt <- c(
    sprintf("Fator A (%s)", fator_a),
    sprintf("Fator B (%s)", fator_b),
    sprintf("Interação %s × %s", fator_a, fator_b),
    "Resíduos", "Total"
  )
  tabela <- data.frame(
    `Fonte de variação` = nomes_pt,
    `Graus de liberdade` = c(df_efeitos, df_res, nrow(d) - 1L),
    `Soma de quadrados` = c(ss_efeitos, ss_res, ss_total),
    `Quadrado médio` = c(qm_efeitos, qm_res, NA_real_),
    `F` = c(f_efeitos, NA_real_, NA_real_),
    `p-valor` = c(p_efeitos, NA_real_, NA_real_),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  niveis_a <- levels(d$fator_a)
  niveis_b <- levels(d$fator_b)
  celulas <- do.call(rbind, lapply(niveis_a, function(a) {
    do.call(rbind, lapply(niveis_b, function(b) {
      valores <- d$resposta[d$fator_a == a & d$fator_b == b]
      n <- length(valores)
      media <- mean(valores)
      margem <- stats::qt(1 - (1 - conf) / 2, df = n - 1) *
        stats::sd(valores) / sqrt(n)
      data.frame(
        fator_a = a, fator_b = b, n = n, media = media,
        desvio = stats::sd(valores),
        ic_inferior = media - margem, ic_superior = media + margem,
        stringsAsFactors = FALSE
      )
    }))
  }))
  rownames(celulas) <- NULL
  tamanhos_celula <- with(celulas, stats::setNames(n, paste(fator_a, fator_b, sep = " × ")))
  delineamento_balanceado <- length(unique(celulas$n)) == 1L

  efeito <- data.frame(
    Efeito = c("Fator A", "Fator B", "Interação A × B"),
    `Eta² parcial` = ss_efeitos / (ss_efeitos + ss_res),
    `Ômega²` = (ss_efeitos - df_efeitos * qm_res) / (ss_total + qm_res),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  residuos <- stats::residuals(modelo)
  ajustados <- stats::fitted(modelo)
  shapiro <- if (length(residuos) >= 3L && length(residuos) <= 5000L) {
    tryCatch(stats::shapiro.test(residuos), error = function(e) NULL)
  } else NULL
  grupo_levene <- interaction(d$fator_a, d$fator_b, drop = TRUE, sep = " × ")
  levene <- if (requireNamespace("car", quietly = TRUE)) {
    tryCatch(car::leveneTest(d$resposta, grupo_levene, center = stats::median),
             error = function(e) NULL)
  } else NULL
  pressupostos <- data.frame(
    Pressuposto = c(
      "Normalidade dos resíduos (Shapiro-Wilk)",
      "Homogeneidade das variâncias por célula (Levene)"
    ),
    Estatística = c(
      if (is.null(shapiro)) NA_real_ else unname(as.numeric(shapiro$statistic)),
      if (is.null(levene)) NA_real_ else suppressWarnings(as.numeric(levene[["F value"]][1]))
    ),
    `p-valor` = c(
      if (is.null(shapiro)) NA_real_ else unname(as.numeric(shapiro$p.value)),
      if (is.null(levene)) NA_real_ else suppressWarnings(as.numeric(levene[["Pr(>F)"]][1]))
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  comparacoes <- tryCatch({
    bruto <- as.data.frame(stats::TukeyHSD(modelo, which = "fator_a:fator_b",
                                            conf.level = conf)[[1]])
    data.frame(
      `Célula comparada` = rownames(bruto),
      `Diferença estimada` = bruto$diff,
      `IC inferior` = bruto$lwr,
      `IC superior` = bruto$upr,
      `p ajustado` = bruto$`p adj`,
      check.names = FALSE,
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  }, error = function(e) NULL)

  p_interacao <- p_efeitos[3]
  leitura_interacao <- if (is.na(p_interacao)) "não disponível" else
    if (p_interacao < 0.05) "há evidência de interação" else
      "não há evidência suficiente de interação"
  narrativa <- paste0(
    sprintf("A pergunta foi se '%s' varia conforme '%s' e '%s'. ",
            dep_var, fator_a, fator_b),
    sprintf("Entraram %d observações completas", nrow(d)),
    if (excluidos > 0) sprintf("; %d linha(s) foram excluídas por dados faltantes. ", excluidos)
    else ". ",
    if (delineamento_balanceado) "As células têm o mesmo tamanho amostral. " else
      sprintf("O delineamento é desequilibrado (n por célula: %s). ",
              paste(names(tamanhos_celula), tamanhos_celula, sep = " = ", collapse = "; ")),
    sprintf("O modelo fatorial encontrou %s entre os fatores (F = %s; %s). ",
            leitura_interacao, anova2_num(f_efeitos[3]), anova2_p(p_interacao)),
    sprintf("Os efeitos principais devem ser interpretados junto com a interação: quando a interação é relevante, o efeito de '%s' depende de '%s'. ", fator_a, fator_b),
    "As médias por célula, os testes de pressupostos e o gráfico de interação completam a leitura."
  )

  console <- c(
    utils::capture.output(print(summary(modelo))),
    "",
    if (is.null(shapiro)) "Shapiro-Wilk não calculado." else utils::capture.output(print(shapiro)),
    "",
    if (is.null(levene)) "Levene indisponível (pacote 'car' ausente)." else utils::capture.output(print(levene)),
    "",
    if (is.null(comparacoes)) "Tukey das células indisponível." else utils::capture.output(print(stats::TukeyHSD(modelo, which = "fator_a:fator_b", conf.level = conf)))
  )

  list(
    dados = d, dep_var = dep_var, fator_a = fator_a, fator_b = fator_b,
    nivel_confianca = conf, n = nrow(d), excluidos = excluidos,
    n_niveis_a = length(niveis_a), n_niveis_b = length(niveis_b),
    niveis_a = niveis_a, niveis_b = niveis_b,
    tamanhos_celula = tamanhos_celula,
    delineamento_balanceado = delineamento_balanceado,
    modelo = modelo, tabela = tabela, celulas = celulas, efeito = efeito,
    pressupostos = pressupostos, comparacoes = comparacoes,
    residuos = residuos, ajustados = ajustados,
    df_a = df_efeitos[1], df_b = df_efeitos[2], df_interacao = df_efeitos[3],
    f_a = f_efeitos[1], f_b = f_efeitos[2], f_interacao = f_efeitos[3],
    p_a = p_efeitos[1], p_b = p_efeitos[2], p_interacao = p_interacao,
    narrativa = narrativa, console = console
  )
}

arrumar_tabela_anova_dois_fatores <- function(r) r$tabela
arrumar_celulas_anova_dois_fatores <- function(r) {
  data.frame(
    `Fator A` = r$celulas$fator_a,
    `Fator B` = r$celulas$fator_b,
    n = r$celulas$n,
    `Média` = r$celulas$media,
    `DP` = r$celulas$desvio,
    `IC inferior` = r$celulas$ic_inferior,
    `IC superior` = r$celulas$ic_superior,
    check.names = FALSE
  )
}
arrumar_efeito_anova_dois_fatores <- function(r) r$efeito
arrumar_pressupostos_anova_dois_fatores <- function(r) r$pressupostos
arrumar_comparacoes_anova_dois_fatores <- function(r) r$comparacoes
relatar_anova_dois_fatores <- function(r) r$narrativa

grafico_anova_dois_fatores <- function(r, titulo = NULL, rotulo_x = NULL,
                                       rotulo_y = NULL, tema = "minimal") {
  anova2_grafico_interacao_celulas(
    r$celulas, r$fator_a, r$fator_b, r$dep_var, r$niveis_a, r$niveis_b,
    nivel_confianca = r$nivel_confianca, titulo = titulo,
    rotulo_x = rotulo_x, rotulo_y = rotulo_y, tema = tema
  )
}

grafico_combinacoes_anova_dois_fatores <- function(
    r, titulo = NULL, rotulo_x = NULL, rotulo_y = NULL, tema = "minimal") {
  anova2_grafico_combinacoes_celulas(
    r$celulas, r$fator_a, r$fator_b, r$dep_var, r$niveis_a, r$niveis_b,
    nivel_confianca = r$nivel_confianca, titulo = titulo,
    rotulo_x = rotulo_x, rotulo_y = rotulo_y, tema = tema
  )
}

grafico_diagnosticos_anova_dois_fatores <- function(r, tipo = c("residuos", "qq"),
                                                    tema = "minimal") {
  tipo <- match.arg(tipo)
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  if (tipo == "qq") {
    return(ggplot2::ggplot(data.frame(residuos = r$residuos),
                           ggplot2::aes(sample = residuos)) +
      ggplot2::stat_qq(color = "#2E7D8F") + ggplot2::stat_qq_line(color = "#E76F51") +
      ggplot2::theme_minimal(base_size = 12) + ggplot2::labs(title = "Q-Q dos resíduos"))
  }
  ggplot2::ggplot(data.frame(ajustados = r$ajustados, residuos = r$residuos),
                  ggplot2::aes(x = ajustados, y = residuos)) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2, color = "#E76F51") +
    ggplot2::geom_point(color = "#2E7D8F", alpha = 0.8) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::labs(title = "Resíduos versus ajustados", x = "Valores ajustados", y = "Resíduos")
}
