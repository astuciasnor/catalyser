# ANOVA com subamostras: um fator fixo e intercepto aleatorio por unidade.
#
# Este arquivo concentra o calculo usado pela interface e pelo replay do Projeto R.
# A coluna de subamostra identifica as linhas dentro da unidade; ela nao entra
# como uma replica independente nem como um segundo efeito no modelo.

anova_mista_validar <- function(dados, resposta, fator, unidade, subamostra) {
  colunas <- c(resposta, fator, unidade, subamostra)
  faltantes <- setdiff(colunas, names(dados))
  if (length(faltantes)) {
    return(sprintf("Coluna(s) não encontrada(s): %s.", paste(faltantes, collapse = ", ")))
  }
  if (length(unique(colunas)) < 4L) {
    return("Resposta, fator, unidade e subamostra devem ser colunas diferentes.")
  }
  if (!is.numeric(dados[[resposta]])) return("A variável resposta precisa ser numérica.")

  completos <- stats::complete.cases(dados[colunas]) & is.finite(dados[[resposta]])
  base <- dados[completos, colunas, drop = FALSE]
  if (nrow(base) < 6L) return("São necessárias ao menos seis linhas completas para ajustar os dois modelos.")
  if (length(unique(base[[fator]])) < 2L) return("O fator precisa ter ao menos dois níveis observados.")
  if (length(unique(base[[unidade]])) < 3L) return("São necessárias ao menos três unidades independentes.")

  mapa <- stats::aggregate(base[[fator]], list(unidade = base[[unidade]]),
                           function(x) length(unique(x)))
  if (any(mapa$x != 1L)) {
    return("Cada unidade deve pertencer a um único nível do fator. Confira a coluna da unidade.")
  }
  chave <- paste(base[[unidade]], base[[subamostra]], sep = "\r")
  if (anyDuplicated(chave)) {
    return("A identificação da subamostra se repete dentro da mesma unidade. Use uma coluna que identifique cada medição.")
  }
  unidades_por_grupo <- table(base[[fator]], base[[unidade]]) > 0
  if (any(rowSums(unidades_por_grupo) < 2L)) {
    return("Cada nível do fator precisa ter ao menos duas unidades independentes; várias subamostras da mesma unidade não substituem essa replicação.")
  }
  NULL
}

anova_mista_p <- function(p) {
  if (is.na(p)) return("não calculado")
  if (p < 0.001) "p < 0,001" else paste0("p = ", formatC(p, digits = 3, format = "f", decimal.mark = ","))
}

anova_mista_num <- function(x, casas = 2L) {
  ifelse(is.na(x), "-", formatC(x, digits = casas, format = "f", decimal.mark = ","))
}

#' ANOVA com subamostras por modelo misto
#'
#' Compara a ANOVA das medias por unidade com um modelo misto que preserva as
#' subamostras e usa intercepto aleatorio por unidade. A ANOVA ingenua por linha
#' e calculada somente para tornar visivel o risco de pseudorreplicacao.
#'
#' @param dados Um data.frame em formato longo, uma linha por subamostra.
#' @param p Lista com `resposta`, `fator`, `unidade`, `subamostra` e,
#'   opcionalmente, `nivel_confianca` e `tema`.
#' @return Lista de componentes para a interface e o Projeto R.
#' @export
catalyser_anova_mista <- function(dados, p) {
  valor_ou <- function(x, padrao = "") if (is.null(x) || !length(x)) padrao else x
  resposta <- as.character(valor_ou(p$resposta))
  fator <- as.character(valor_ou(p$fator))
  unidade <- as.character(valor_ou(p$unidade))
  subamostra <- as.character(valor_ou(p$subamostra))
  mensagem <- anova_mista_validar(dados, resposta, fator, unidade, subamostra)
  if (!is.null(mensagem)) stop(mensagem, call. = FALSE)

  colunas <- c(resposta, fator, unidade, subamostra)
  completos <- stats::complete.cases(dados[colunas]) & is.finite(dados[[resposta]])
  base <- dados[completos, colunas, drop = FALSE]
  names(base) <- c("resposta", "fator", "unidade", "subamostra")
  base$fator <- droplevels(as.factor(base$fator))
  base$unidade <- droplevels(as.factor(base$unidade))
  base$subamostra <- as.factor(base$subamostra)

  por_unidade <- stats::aggregate(
    base$resposta,
    list(unidade = base$unidade, fator = base$fator),
    mean
  )
  names(por_unidade)[3] <- "resposta_media"

  modelo_simples <- stats::aov(resposta_media ~ fator, data = por_unidade)
  modelo_misto <- nlme::lme(
    resposta ~ fator,
    random = ~1 | unidade,
    data = base,
    method = "REML",
    na.action = stats::na.omit
  )
  modelo_ingenuo <- stats::aov(resposta ~ fator, data = base)

  simples <- summary(modelo_simples)[[1]]
  misto <- stats::anova(modelo_misto)
  ingenuo <- summary(modelo_ingenuo)[[1]]
  linha_mista <- if (nrow(misto) >= 2L) 2L else nrow(misto)

  f_simples <- unname(simples$`F value`[1])
  p_simples <- unname(simples$`Pr(>F)`[1])
  f_misto <- unname(misto$`F-value`[linha_mista])
  p_misto <- unname(misto$`p-value`[linha_mista])
  f_ingenuo <- unname(ingenuo$`F value`[1])
  p_ingenuo <- unname(ingenuo$`Pr(>F)`[1])

  descritivos <- do.call(rbind, lapply(levels(base$fator), function(grupo) {
    b <- base[base$fator == grupo, , drop = FALSE]
    u <- por_unidade[por_unidade$fator == grupo, , drop = FALSE]
    data.frame(
      Grupo = grupo,
      `Unidades independentes` = length(unique(b$unidade)),
      Subamostras = nrow(b),
      `Média das unidades` = mean(u$resposta_media),
      `DP entre unidades` = stats::sd(u$resposta_media),
      check.names = FALSE
    )
  }))

  tabela <- data.frame(
    Abordagem = c(
      "ANOVA simples: média por unidade",
      "Modelo misto: unidade aleatória",
      "Ingênua: cada subamostra como réplica (evitar)"
    ),
    `n que sustenta a comparação` = c(nrow(por_unidade), nrow(por_unidade), nrow(base)),
    F = c(f_simples, f_misto, f_ingenuo),
    `gl do fator` = c(simples$Df[1], misto$numDF[linha_mista], ingenuo$Df[1]),
    `gl do erro` = c(simples$Df[2], misto$denDF[linha_mista], ingenuo$Df[2]),
    `p-valor` = c(p_simples, p_misto, p_ingenuo),
    check.names = FALSE
  )

  teste_shapiro_simples <- if (length(stats::residuals(modelo_simples)) >= 3L &&
                                length(stats::residuals(modelo_simples)) <= 5000L) {
    stats::shapiro.test(stats::residuals(modelo_simples))
  } else NULL
  teste_shapiro_misto <- if (nrow(base) >= 3L && nrow(base) <= 5000L) {
    stats::shapiro.test(stats::residuals(modelo_misto, type = "normalized"))
  } else NULL
  teste_levene <- tryCatch(
    car::leveneTest(por_unidade$resposta_media, por_unidade$fator, center = stats::median),
    error = function(e) NULL
  )
  p_levene <- if (is.null(teste_levene)) NA_real_ else teste_levene$`Pr(>F)`[1]
  p_shapiro_simples <- if (is.null(teste_shapiro_simples)) NA_real_ else teste_shapiro_simples$p.value
  p_shapiro_misto <- if (is.null(teste_shapiro_misto)) NA_real_ else teste_shapiro_misto$p.value

  leitura_teste <- function(p_valor, problema, sem_evidencia) {
    if (is.na(p_valor)) return("Teste não calculado para estes dados.")
    if (p_valor < 0.05) problema else sem_evidencia
  }
  pressupostos <- data.frame(
    "Verificação" = c(
      "Independência",
      "Variâncias entre grupos nas médias por unidade",
      "Normalidade dos resíduos da ANOVA simples",
      "Normalidade dos resíduos normalizados do modelo misto"
    ),
    "Resultado" = c(
      "Depende do delineamento",
      anova_mista_p(p_levene),
      anova_mista_p(p_shapiro_simples),
      anova_mista_p(p_shapiro_misto)
    ),
    "Leitura" = c(
      sprintf("A análise considera %d unidades independentes; as %d linhas são medições dentro delas.", nrow(por_unidade), nrow(base)),
      leitura_teste(p_levene,
        "Houve evidência de variâncias diferentes entre os grupos nas médias das unidades.",
        "Não houve evidência de variâncias diferentes entre os grupos nas médias das unidades."),
      leitura_teste(p_shapiro_simples,
        "Os resíduos da ANOVA simples se afastaram da normalidade.",
        "Os resíduos da ANOVA simples não apresentaram afastamento detectável da normalidade."),
      leitura_teste(p_shapiro_misto,
        "Os resíduos normalizados do modelo misto se afastaram da normalidade.",
        "Os resíduos normalizados do modelo misto não apresentaram afastamento detectável da normalidade.")
    ),
    check.names = FALSE
  )

  variancias <- nlme::VarCorr(modelo_misto)
  var_unidade <- suppressWarnings(as.numeric(as.character(variancias[1, "Variance"])))
  var_residual <- suppressWarnings(as.numeric(as.character(variancias[nrow(variancias), "Variance"])))
  icc <- var_unidade / (var_unidade + var_residual)

  decisao <- if (!is.na(p_misto) && p_misto < 0.05) {
    "houve evidência de diferença entre os níveis do fator"
  } else {
    "não houve evidência suficiente de diferença entre os níveis do fator"
  }
  concordam <- isTRUE((p_simples < 0.05) == (p_misto < 0.05))
  narrativa <- paste0(
    "Foram analisadas ", nrow(base), " subamostras pertencentes a ", nrow(por_unidade),
    " unidades independentes. No modelo misto, ", decisao, " (F(",
    misto$numDF[linha_mista], "; ", misto$denDF[linha_mista], ") = ",
    anova_mista_num(f_misto), ", ", anova_mista_p(p_misto), "). ",
    if (isTRUE(concordam)) {
      "A ANOVA das médias por unidade e o modelo misto levaram à mesma decisão porque ambos respeitam a unidade do estudo. "
    } else {
      "A ANOVA das médias por unidade e o modelo misto não levaram à mesma decisão; examine a incerteza, o balanceamento e os diagnósticos antes de interpretar. "
    },
    "A correlação intraclasse estimada foi ", anova_mista_num(icc),
    ", descrevendo a parcela da variação associada às diferenças entre unidades."
  )

  resumo_grafico <- do.call(rbind, lapply(levels(por_unidade$fator), function(grupo) {
    x <- por_unidade$resposta_media[por_unidade$fator == grupo]
    erro <- if (length(x) > 1L) stats::qt(0.975, length(x) - 1L) * stats::sd(x) / sqrt(length(x)) else NA_real_
    data.frame(fator = grupo, media = mean(x), inferior = mean(x) - erro, superior = mean(x) + erro)
  }))
  grafico <- ggplot2::ggplot(base, ggplot2::aes(x = fator, y = resposta)) +
    ggplot2::geom_point(
      ggplot2::aes(group = unidade),
      position = ggplot2::position_jitter(width = 0.13, height = 0),
      color = "#62B6B7", alpha = 0.62, size = 2
    ) +
    ggplot2::geom_errorbar(
      data = resumo_grafico,
      ggplot2::aes(x = fator, ymin = inferior, ymax = superior),
      inherit.aes = FALSE, width = 0.12, color = "#0F3B5F"
    ) +
    ggplot2::geom_point(
      data = resumo_grafico,
      ggplot2::aes(x = fator, y = media),
      inherit.aes = FALSE, size = 4, color = "#0F3B5F"
    ) +
    ggplot2::labs(
      x = fator, y = resposta,
      title = "Subamostras e média de cada grupo",
      subtitle = "Pontos claros são subamostras; o modelo reconhece a unidade que as agrupa."
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", color = "#0F3B5F"),
      panel.grid.minor = ggplot2::element_blank()
    )

  residuos_mistos <- stats::residuals(modelo_misto, type = "normalized")
  diagnosticos <- ggplot2::ggplot(data.frame(residuo = as.numeric(residuos_mistos)),
                                  ggplot2::aes(sample = residuo)) +
    ggplot2::stat_qq(color = "#2E7D8F") +
    ggplot2::stat_qq_line(color = "#E76F51") +
    ggplot2::labs(x = "Quantis teóricos", y = "Resíduos normalizados",
                  title = "QQ-plot do modelo misto") +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", color = "#0F3B5F"))

  citar_nome <- function(x) paste0("`", gsub("`", "\\`", x, fixed = TRUE), "`")
  codigo <- paste(
    sprintf("modelo_misto <- nlme::lme(%s ~ %s,", citar_nome(resposta), citar_nome(fator)),
    sprintf("  random = ~1 | %s, data = dados, method = \"REML\")", citar_nome(unidade)),
    "nlme::anova.lme(modelo_misto)",
    sep = "\n"
  )

  list(
    narrativa = narrativa,
    descritivos = descritivos,
    tabela = tabela,
    grafico = grafico,
    pressupostos = pressupostos,
    diagnosticos = diagnosticos,
    console = c(
      "ANOVA simples sobre as médias por unidade:", utils::capture.output(summary(modelo_simples)),
      "", "Modelo misto:", utils::capture.output(summary(modelo_misto)),
      "", "Teste F do fator no modelo misto:", utils::capture.output(stats::anova(modelo_misto)),
      "", "Componentes de variância:", utils::capture.output(nlme::VarCorr(modelo_misto))
    ),
    objeto = modelo_misto,
    modelo_simples = modelo_simples,
    modelo_ingenuo = modelo_ingenuo,
    dados = base,
    por_unidade = por_unidade,
    codigo = codigo,
    n = nrow(base),
    n_unidades = nrow(por_unidade),
    f_misto = f_misto,
    gl_1 = unname(misto$numDF[linha_mista]),
    gl_2 = unname(misto$denDF[linha_mista]),
    p_misto = p_misto,
    icc = icc,
    excluidos = nrow(dados) - nrow(base)
  )
}
