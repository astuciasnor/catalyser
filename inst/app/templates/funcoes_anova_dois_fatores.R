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
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  d <- r$celulas
  d$fator_a <- factor(d$fator_a, levels = r$niveis_a)
  d$fator_b <- factor(d$fator_b, levels = r$niveis_b)
  tema_fun <- switch(
    as.character(tema %||% "minimal"),
    classic = ggplot2::theme_classic(base_size = 12),
    bw = ggplot2::theme_bw(base_size = 12),
    gray = ggplot2::theme_gray(base_size = 12),
    light = ggplot2::theme_light(base_size = 12),
    ggplot2::theme_minimal(base_size = 12)
  )
  ggplot2::ggplot(d, ggplot2::aes(x = fator_a, y = media, color = fator_b,
                                  group = fator_b)) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = ic_inferior, ymax = ic_superior),
                           width = 0.10, linewidth = 0.7) +
    ggplot2::scale_color_manual(values = rep(c("#0F3B5F", "#2E7D8F", "#E89B3C", "#E76F51"),
                                              length.out = length(r$niveis_b))) +
    tema_fun +
    ggplot2::labs(
      title = titulo %||% sprintf("Interação entre %s e %s", r$fator_a, r$fator_b),
      subtitle = sprintf("Médias por célula; hastes = IC %.0f%%", 100 * r$nivel_confianca),
      x = rotulo_x %||% r$fator_a,
      y = rotulo_y %||% r$dep_var,
      color = r$fator_b
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
