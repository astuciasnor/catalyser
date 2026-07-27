# =============================================================================
# funcoes_anova.R
# -----------------------------------------------------------------------------
# Fonte canônica única da ANOVA de um fator na CatalyseR (V16).
#
# Arquitetura:
#   anova_validar_entrada()        -> mensagem orientadora ou NULL.
#   calcular_anova()               -> ajuste, descritivos, efeito, pressupostos,
#                                     Tukey e console bruto (função pura).
#   arrumar_descritivos_anova()    -> resumo por grupo.
#   arrumar_tabela_anova()         -> tabela da ANOVA.
#   arrumar_tamanho_efeito_anova() -> eta quadrado e ômega quadrado.
#   arrumar_pressupostos_anova()   -> Shapiro-Wilk, Levene (mediana) e Bartlett.
#   arrumar_tukey_anova()          -> comparações múltiplas com IC 95%.
#   relatar_anova()                -> narrativa automática em português.
#   grafico_anova()                -> gráfico principal (observações + IC 95%).
#   grafico_diagnosticos_anova()   -> resíduos x ajustados e Q-Q plot.
#
# As funções `arrumar_*` devolvem data.frame e imprimem em qualquer lugar; a
# camada de apresentação (flextable/Viewer) fica por cima. Os wrappers
# `mostrar_anova()`, `mostrar_pressupostos()` e `mostrar_tukey()` existem apenas
# por compatibilidade com o código legado.
# =============================================================================

# Evitar erros de 'req' não encontrado fora do Shiny
if (!exists("req", mode = "function")) {
  req <- function(...) {
    invisible(TRUE)
  }
}

# Formatador privado: evita colisões com helpers homônimos de outros módulos.
anova_fmt <- function(x, dig = 2) {
  if (is.null(x) || length(x) == 0 || is.na(x[[1]])) return("-")
  formatC(as.numeric(x[[1]]), format = "f", digits = dig, decimal.mark = ",")
}

anova_p_texto <- function(p, dig = 4) {
  if (is.null(p) || !length(p) || is.na(p[[1]])) return("p não disponível")
  if (p[[1]] < 0.001) "p < 0,001" else paste0("p = ", anova_fmt(p, dig))
}

if (!exists("%||%")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}

anova_cores_ocean <- c("#0F3B5F", "#2E7D8F", "#62B6B7", "#E89B3C", "#E76F51",
                       "#8FBF9F", "#B5651D", "#9D8DF1", "#C44536", "#3D5A80")

anova_tema <- function(nome = "minimal", base_size = 14) {
  switch(
    as.character(nome %||% "minimal"),
    "classic" = ggplot2::theme_classic(base_size = base_size),
    "bw"      = ggplot2::theme_bw(base_size = base_size),
    "gray"    = ggplot2::theme_gray(base_size = base_size),
    "light"   = ggplot2::theme_light(base_size = base_size),
    ggplot2::theme_minimal(base_size = base_size)
  )
}

# ---- Validação ---------------------------------------------------------------

#' Valida a configuração da ANOVA antes do ajuste
#'
#' Devolve NULL quando a configuração é utilizável; caso contrário, uma
#' mensagem única com a ação corretiva. Nenhuma exclusão de NA é silenciosa:
#' quem chama recebe `n` analisado e casos excluídos em `calcular_anova()`.
anova_validar_entrada <- function(df, dep_var, ind_var) {
  if (is.null(df) || !is.data.frame(df) || !nrow(df))
    return("Importe uma base com observações antes de executar a ANOVA.")
  if (is.null(dep_var) || is.null(ind_var) || !nzchar(dep_var) || !nzchar(ind_var))
    return("Escolha a variável resposta (Y) e o fator (X).")
  if (identical(dep_var, ind_var))
    return("A resposta e o fator precisam ser variáveis diferentes. Escolha outra coluna para um dos dois.")
  ausentes <- setdiff(c(dep_var, ind_var), names(df))
  if (length(ausentes))
    return(sprintf("A base não contém a(s) coluna(s): %s. Reveja a base escolhida em Base utilizada.",
                   paste(ausentes, collapse = ", ")))
  if (!is.numeric(df[[dep_var]]))
    return(sprintf("A resposta '%s' precisa ser numérica. Use Organizar Variáveis para convertê-la antes da ANOVA.",
                   dep_var))

  completos <- stats::complete.cases(df[c(dep_var, ind_var)])
  d <- df[completos, , drop = FALSE]
  if (!nrow(d))
    return("Todas as linhas têm dados faltantes na resposta ou no fator. Trate os ausentes na Trilha de Preparo.")

  fator <- droplevels(as.factor(d[[ind_var]]))
  if (nlevels(fator) < 2L)
    return(sprintf("O fator '%s' precisa de pelo menos dois grupos com dados; foi encontrado %d. Reveja o filtro da base derivada.",
                   ind_var, nlevels(fator)))
  contagem <- table(fator)
  pequenos <- names(contagem)[contagem < 2L]
  if (length(pequenos))
    return(sprintf("Os grupos com menos de duas observações impedem o cálculo do resíduo: %s. Agrupe ou remova esses níveis antes da ANOVA.",
                   paste(pequenos, collapse = ", ")))
  if (nrow(d) - nlevels(fator) < 1L)
    return("Não sobraram graus de liberdade para o resíduo. Aumente o número de observações por grupo.")
  NULL
}

# ---- Cálculo -----------------------------------------------------------------

#' Executa a ANOVA de um fator, pressupostos, efeito e Tukey
#'
#' Função pura: recebe um data.frame e devolve uma lista com tudo o que a
#' interface e o Projeto R precisam. Não consulta inputs do Shiny.
calcular_anova <- function(df, dep_var, ind_var, nivel_confianca = 0.95) {
  mensagem <- anova_validar_entrada(df, dep_var, ind_var)
  if (!is.null(mensagem)) stop(mensagem, call. = FALSE)

  linhas_originais <- nrow(df)
  completos <- stats::complete.cases(df[c(dep_var, ind_var)])
  d <- df[completos, c(dep_var, ind_var), drop = FALSE]
  names(d) <- c("resposta", "fator")
  d$fator <- droplevels(as.factor(d$fator))
  excluidos <- linhas_originais - nrow(d)

  fit <- stats::aov(resposta ~ fator, data = d)
  resumo <- summary(fit)[[1]]

  df_entre <- resumo$Df[1]
  df_dentro <- resumo$Df[2]
  sq_entre <- resumo$`Sum Sq`[1]
  sq_dentro <- resumo$`Sum Sq`[2]
  qm_entre <- resumo$`Mean Sq`[1]
  qm_dentro <- resumo$`Mean Sq`[2]
  f_anova <- resumo$`F value`[1]
  p_anova <- resumo$`Pr(>F)`[1]

  anova_df <- data.frame(
    Fonte = c("Entre grupos (fator)", "Dentro dos grupos (resíduos)", "Total"),
    Df = c(df_entre, df_dentro, df_entre + df_dentro),
    Soma_Quadrados = c(sq_entre, sq_dentro, sq_entre + sq_dentro),
    Quadrados_Medios = c(qm_entre, qm_dentro, NA_real_),
    F_valor = c(f_anova, NA_real_, NA_real_),
    p_valor = c(p_anova, NA_real_, NA_real_),
    stringsAsFactors = FALSE
  )

  # --- Resumo por grupo ------------------------------------------------------
  niveis <- levels(d$fator)
  ausentes_por_grupo <- rep(NA_integer_, length(niveis))
  names(ausentes_por_grupo) <- niveis
  if (all(c(dep_var, ind_var) %in% names(df))) {
    fator_bruto <- as.character(df[[ind_var]])
    resposta_bruta <- df[[dep_var]]
    for (nivel in niveis) {
      selecao <- !is.na(fator_bruto) & fator_bruto == nivel
      ausentes_por_grupo[[nivel]] <- sum(selecao & is.na(resposta_bruta))
    }
  }
  descritivos_df <- do.call(rbind, lapply(niveis, function(nivel) {
    valores <- d$resposta[d$fator == nivel]
    data.frame(
      Grupo = nivel,
      N = length(valores),
      Ausentes = as.integer(ausentes_por_grupo[[nivel]]),
      Media = mean(valores),
      Desvio_Padrao = stats::sd(valores),
      Mediana = stats::median(valores),
      Minimo = min(valores),
      Maximo = max(valores),
      stringsAsFactors = FALSE
    )
  }))
  rownames(descritivos_df) <- NULL
  grupos_pequenos <- descritivos_df$Grupo[descritivos_df$N < 5L]

  # --- Tamanho de efeito -----------------------------------------------------
  sq_total <- sq_entre + sq_dentro
  eta2 <- sq_entre / sq_total
  omega2 <- (sq_entre - df_entre * qm_dentro) / (sq_total + qm_dentro)
  efeito_df <- data.frame(
    Medida = c("Eta quadrado (η²)", "Ômega quadrado (ω²)"),
    Valor = c(eta2, omega2),
    IC_Inferior = c(NA_real_, NA_real_),
    IC_Superior = c(NA_real_, NA_real_),
    stringsAsFactors = FALSE
  )
  efeito_aviso <- NA_character_
  if (requireNamespace("effectsize", quietly = TRUE)) {
    ic <- tryCatch({
      eta_pkg <- effectsize::eta_squared(fit, ci = nivel_confianca, verbose = FALSE)
      omega_pkg <- effectsize::omega_squared(fit, ci = nivel_confianca, verbose = FALSE)
      list(eta = eta_pkg, omega = omega_pkg)
    }, error = function(e) NULL)
    if (!is.null(ic)) {
      pega <- function(tab, coluna) {
        if (is.null(tab) || !nrow(as.data.frame(tab))) return(NA_real_)
        tabela <- as.data.frame(tab)
        if (!coluna %in% names(tabela)) return(NA_real_)
        suppressWarnings(as.numeric(tabela[[coluna]][1]))
      }
      efeito_df$IC_Inferior <- c(pega(ic$eta, "CI_low"), pega(ic$omega, "CI_low"))
      efeito_df$IC_Superior <- c(pega(ic$eta, "CI_high"), pega(ic$omega, "CI_high"))
    }
  } else {
    efeito_aviso <- paste(
      "O pacote 'effectsize' não está instalado: os intervalos de confiança do",
      "tamanho de efeito não foram calculados. Instale com",
      "install.packages('effectsize') para obtê-los."
    )
  }

  # --- Pressupostos ----------------------------------------------------------
  residuos <- stats::residuals(fit)
  sh_test <- if (length(residuos) >= 3L && length(residuos) <= 5000L) {
    tryCatch(stats::shapiro.test(residuos), error = function(e) NULL)
  } else NULL

  levene <- NULL
  levene_aviso <- NA_character_
  if (requireNamespace("car", quietly = TRUE)) {
    levene <- tryCatch(car::leveneTest(resposta ~ fator, data = d, center = stats::median),
                       error = function(e) NULL)
  } else {
    levene_aviso <- paste(
      "O pacote 'car' não está instalado: o teste de Levene não foi calculado.",
      "Instale com install.packages('car'). O teste de Bartlett aparece abaixo",
      "apenas como informação adicional."
    )
  }
  bt_test <- tryCatch(stats::bartlett.test(resposta ~ fator, data = d), error = function(e) NULL)

  extrair <- function(objeto, campo) {
    if (is.null(objeto)) return(NA_real_)
    valor <- objeto[[campo]]
    if (is.null(valor) || !length(valor)) return(NA_real_)
    unname(as.numeric(valor[[1]]))
  }
  levene_f <- if (is.null(levene)) NA_real_ else suppressWarnings(as.numeric(levene[["F value"]][1]))
  levene_p <- if (is.null(levene)) NA_real_ else suppressWarnings(as.numeric(levene[["Pr(>F)"]][1]))

  # --- Comparações múltiplas -------------------------------------------------
  tukey <- tryCatch(stats::TukeyHSD(fit, conf.level = nivel_confianca), error = function(e) NULL)
  tukey_df <- if (is.null(tukey)) {
    data.frame(
      Comparacao = character(), Diferenca = numeric(), Lwr = numeric(),
      Upr = numeric(), p_adj = numeric(), Evidencia = character(),
      stringsAsFactors = FALSE
    )
  } else {
    bruto <- as.data.frame(tukey[[1]])
    data.frame(
      Comparacao = rownames(bruto),
      Diferenca = bruto$diff,
      Lwr = bruto$lwr,
      Upr = bruto$upr,
      p_adj = bruto$`p adj`,
      Evidencia = ifelse(bruto$`p adj` < 0.05,
                         "Há evidência de diferença",
                         "Sem evidência de diferença"),
      stringsAsFactors = FALSE
    )
  }
  rownames(tukey_df) <- NULL

  console <- c(
    "# summary(modelo_anova)",
    utils::capture.output(print(summary(fit))),
    "",
    "# stats::TukeyHSD(modelo_anova)",
    if (is.null(tukey)) "Tukey HSD indisponível." else utils::capture.output(print(tukey)),
    "",
    "# stats::shapiro.test(stats::residuals(modelo_anova))",
    if (is.null(sh_test)) "Shapiro-Wilk não calculado para este tamanho amostral." else utils::capture.output(print(sh_test)),
    "",
    "# car::leveneTest(resposta ~ fator, center = median)",
    if (is.null(levene)) "Teste de Levene indisponível (pacote 'car' ausente)." else utils::capture.output(print(levene))
  )

  list(
    dep_var = dep_var,
    ind_var = ind_var,
    nivel_confianca = nivel_confianca,
    dados = d,
    n = nrow(d),
    excluidos = excluidos,
    grupos = niveis,
    n_grupos = length(niveis),
    grupos_pequenos = grupos_pequenos,
    fit = fit,
    anova_df = anova_df,
    descritivos_df = descritivos_df,
    efeito_df = efeito_df,
    efeito_aviso = efeito_aviso,
    eta2 = eta2,
    omega2 = omega2,
    sh_stat = extrair(sh_test, "statistic"),
    sh_p = extrair(sh_test, "p.value"),
    levene_f = levene_f,
    levene_p = levene_p,
    levene_aviso = levene_aviso,
    bt_stat = extrair(bt_test, "statistic"),
    bt_p = extrair(bt_test, "p.value"),
    tukey_df = tukey_df,
    p_anova = p_anova,
    f_anova = f_anova,
    df_entre = df_entre,
    df_dentro = df_dentro,
    sq_entre = sq_entre,
    sq_dentro = sq_dentro,
    qm_entre = qm_entre,
    qm_dentro = qm_dentro,
    residuals = residuos,
    fitted = stats::fitted(fit),
    console = console
  )
}

# ---- Arrumação (data.frame em português) -------------------------------------

#' Resumo descritivo por grupo
arrumar_descritivos_anova <- function(r) {
  d <- r$descritivos_df
  data.frame(
    `Grupo` = d$Grupo,
    `n` = d$N,
    `Ausentes excluídos` = d$Ausentes,
    `Média` = round(d$Media, 3),
    `Desvio-padrão` = round(d$Desvio_Padrao, 3),
    `Mediana` = round(d$Mediana, 3),
    `Mínimo` = round(d$Minimo, 3),
    `Máximo` = round(d$Maximo, 3),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

#' Tabela da ANOVA
arrumar_tabela_anova <- function(r) {
  fmt_col <- function(x, dig) vapply(x, function(v) if (is.na(v)) "-" else formatC(v, format = "f", digits = dig), character(1))
  data.frame(
    `Fonte de variação` = r$anova_df$Fonte,
    `Graus de liberdade` = r$anova_df$Df,
    `Soma de quadrados` = fmt_col(r$anova_df$Soma_Quadrados, 3),
    `Quadrado médio` = fmt_col(r$anova_df$Quadrados_Medios, 3),
    `F` = fmt_col(r$anova_df$F_valor, 3),
    `p-valor` = fmt_col(r$anova_df$p_valor, 4),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

#' Tamanho de efeito (eta quadrado e ômega quadrado)
arrumar_tamanho_efeito_anova <- function(r) {
  ic <- ifelse(
    is.na(r$efeito_df$IC_Inferior) | is.na(r$efeito_df$IC_Superior),
    "não disponível",
    sprintf("[%s; %s]",
            formatC(r$efeito_df$IC_Inferior, format = "f", digits = 3),
            formatC(r$efeito_df$IC_Superior, format = "f", digits = 3))
  )
  data.frame(
    `Medida` = r$efeito_df$Medida,
    `Valor` = round(r$efeito_df$Valor, 4),
    `Intervalo de confiança` = ic,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

#' Pressupostos: normalidade dos resíduos e homogeneidade de variâncias
arrumar_pressupostos_anova <- function(r) {
  leitura <- function(p, compativel, incompativel) {
    if (is.na(p)) return("Não calculado")
    if (p >= 0.05) compativel else incompativel
  }
  data.frame(
    `Pressuposto` = c(
      "Normalidade dos resíduos (Shapiro-Wilk)",
      "Homogeneidade de variâncias (Levene, centro na mediana)",
      "Homogeneidade de variâncias (Bartlett — informação adicional)"
    ),
    `Estatística` = c(round(r$sh_stat, 4), round(r$levene_f, 4), round(r$bt_stat, 4)),
    `p-valor` = c(round(r$sh_p, 4), round(r$levene_p, 4), round(r$bt_p, 4)),
    `Leitura` = c(
      leitura(r$sh_p,
              "Sem evidência de afastamento da normalidade",
              "Há evidência de afastamento da normalidade"),
      leitura(r$levene_p,
              "Sem evidência de heterogeneidade",
              "Há evidência de heterogeneidade"),
      leitura(r$bt_p,
              "Sem evidência de heterogeneidade",
              "Há evidência de heterogeneidade")
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

#' Comparações múltiplas de Tukey
arrumar_tukey_anova <- function(r) {
  d <- r$tukey_df
  nivel <- 100 * (r$nivel_confianca %||% 0.95)
  saida <- data.frame(
    `Par comparado` = d$Comparacao,
    `Diferença estimada` = round(d$Diferenca, 3),
    `IC inferior` = round(d$Lwr, 3),
    `IC superior` = round(d$Upr, 3),
    `p ajustado` = round(d$p_adj, 4),
    `Evidência` = d$Evidencia,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  names(saida)[3] <- sprintf("IC %.0f%% inferior", nivel)
  names(saida)[4] <- sprintf("IC %.0f%% superior", nivel)
  saida
}

# ---- Narrativa ---------------------------------------------------------------

#' Narrativa automática em português
#'
#' Nunca afirma a aceitação da hipótese nula: ausência de evidência não é
#' evidência de ausência.
relatar_anova <- function(r) {
  medias <- paste(
    sprintf("%s (n = %d; média = %s)",
            r$descritivos_df$Grupo, r$descritivos_df$N,
            vapply(r$descritivos_df$Media, anova_fmt, character(1), 2)),
    collapse = "; "
  )

  abertura <- sprintf(
    paste0(
      "A pergunta analisada foi se a média de '%s' difere entre os grupos de '%s'. ",
      "Foram usadas %d observações completas%s, distribuídas em %d grupos: %s. "
    ),
    r$dep_var, r$ind_var, r$n,
    if (r$excluidos > 0) sprintf(" (%d linha(s) excluída(s) por dados faltantes na resposta ou no fator)", r$excluidos) else "",
    r$n_grupos, medias
  )

  aviso_pequenos <- if (length(r$grupos_pequenos)) {
    sprintf("Atenção: os grupos %s têm menos de cinco observações, o que torna os testes menos sensíveis. ",
            paste(r$grupos_pequenos, collapse = ", "))
  } else ""

  efeito <- sprintf(
    "O tamanho de efeito foi η² = %s e ω² = %s. ",
    anova_fmt(r$eta2, 3), anova_fmt(r$omega2, 3)
  )

  diagnostico <- paste0(
    "Quanto aos diagnósticos, o teste de Shapiro-Wilk dos resíduos resultou em ",
    if (is.na(r$sh_p)) "valor não calculado para este tamanho amostral" else
      sprintf("W = %s (%s)", anova_fmt(r$sh_stat, 4), anova_p_texto(r$sh_p)),
    " e o teste de Levene com centro na mediana resultou em ",
    if (is.na(r$levene_p)) "valor não calculado (pacote 'car' ausente)" else
      sprintf("F = %s (%s)", anova_fmt(r$levene_f, 3), anova_p_texto(r$levene_p)),
    ". Esses resultados não comprovam os pressupostos; apenas não revelaram ",
    "afastamentos grandes o bastante para serem detectados com este n. ",
    "Examine também os gráficos de resíduos e o Q-Q plot. "
  )

  resultado <- if (!is.na(r$p_anova) && r$p_anova < 0.05) {
    pares <- r$tukey_df$Comparacao[r$tukey_df$p_adj < 0.05]
    complemento <- if (length(pares)) {
      sprintf(
        paste0(
          "Nas comparações de Tukey, houve evidência de diferença nos pares: %s. ",
          "A interpretação principal, no entanto, decorre da ANOVA global e do plano analítico definido antes da coleta."
        ),
        paste(pares, collapse = "; ")
      )
    } else {
      paste0(
        "As comparações de Tukey não apontaram pares com evidência de diferença, ",
        "situação possível quando o efeito global é pequeno e o ajuste para comparações múltiplas é conservador."
      )
    }
    sprintf(
      paste0(
        "Rejeitou-se H0 de igualdade das médias: F(%d; %d) = %s, %s. %s%s"
      ),
      r$df_entre, r$df_dentro, anova_fmt(r$f_anova, 3), anova_p_texto(r$p_anova),
      efeito, complemento
    )
  } else {
    sprintf(
      paste0(
        "Não houve evidência suficiente para rejeitar H0 de igualdade das médias: ",
        "F(%d; %d) = %s, %s. %s",
        "Isso não significa que as médias sejam iguais; significa que estes dados não permitiram detectar diferença."
      ),
      r$df_entre, r$df_dentro, anova_fmt(r$f_anova, 3), anova_p_texto(r$p_anova), efeito
    )
  }

  fecho <- paste0(
    " A ANOVA compara médias entre grupos observados; por si só, não estabelece relação de causa e efeito."
  )

  paste0(abertura, aviso_pequenos, diagnostico, resultado, fecho)
}

# ---- Gráficos ----------------------------------------------------------------

#' Gráfico principal da ANOVA
#'
#' Mostra as observações individuais, a distribuição de cada grupo, a média e o
#' IC 95%. As médias de níveis nominais NÃO são conectadas por linha: isso
#' sugeriria uma ordem que não existe entre espécies ou locais.
grafico_anova <- function(r, titulo = NULL, rotulo_x = NULL, rotulo_y = NULL,
                          tema = "minimal") {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("O pacote ggplot2 é necessário para o gráfico da ANOVA.", call. = FALSE)
  d <- r$dados
  nivel <- r$nivel_confianca %||% 0.95
  resumo <- do.call(rbind, lapply(levels(d$fator), function(nivel_fator) {
    valores <- d$resposta[d$fator == nivel_fator]
    n <- length(valores)
    ep <- stats::sd(valores) / sqrt(n)
    margem <- if (n > 1L) stats::qt(1 - (1 - nivel) / 2, df = n - 1L) * ep else NA_real_
    data.frame(
      fator = nivel_fator, media = mean(valores),
      inferior = mean(valores) - margem, superior = mean(valores) + margem,
      stringsAsFactors = FALSE
    )
  }))
  resumo$fator <- factor(resumo$fator, levels = levels(d$fator))

  titulo_final <- titulo %||% sprintf("%s por %s", r$dep_var, r$ind_var)
  if (!nzchar(titulo_final)) titulo_final <- sprintf("%s por %s", r$dep_var, r$ind_var)

  ggplot2::ggplot(d, ggplot2::aes(x = fator, y = resposta)) +
    ggplot2::geom_boxplot(ggplot2::aes(fill = fator), alpha = 0.25,
                          outlier.shape = NA, show.legend = FALSE) +
    ggplot2::geom_jitter(ggplot2::aes(color = fator), width = 0.12, height = 0,
                         alpha = 0.6, size = 2.2, show.legend = FALSE) +
    ggplot2::geom_errorbar(
      data = resumo,
      ggplot2::aes(x = fator, ymin = inferior, ymax = superior),
      inherit.aes = FALSE, width = 0.14, linewidth = 0.9, color = "#0F3B5F"
    ) +
    ggplot2::geom_point(
      data = resumo, ggplot2::aes(x = fator, y = media),
      inherit.aes = FALSE, size = 3.4, shape = 18, color = "#E76F51"
    ) +
    ggplot2::scale_fill_manual(values = rep(anova_cores_ocean, length.out = nlevels(d$fator))) +
    ggplot2::scale_color_manual(values = rep(anova_cores_ocean, length.out = nlevels(d$fator))) +
    anova_tema(tema) +
    ggplot2::labs(
      title = titulo_final,
      subtitle = sprintf("Losango = média; barra = IC %.0f%% da média; pontos = observações",
                         100 * nivel),
      x = rotulo_x %||% r$ind_var,
      y = rotulo_y %||% r$dep_var
    ) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", color = "#0F3B5F"))
}

#' Gráficos de diagnóstico dos resíduos
grafico_diagnosticos_anova <- function(r, tipo = c("residuos", "qq"), tema = "minimal") {
  tipo <- match.arg(tipo)
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("O pacote ggplot2 é necessário para os diagnósticos da ANOVA.", call. = FALSE)
  if (identical(tipo, "residuos")) {
    dg <- data.frame(Ajustados = r$fitted, Residuos = r$residuals)
    ggplot2::ggplot(dg, ggplot2::aes(x = Ajustados, y = Residuos)) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "#E76F51", linewidth = 0.9) +
      ggplot2::geom_point(color = "#2E7D8F", alpha = 0.75, size = 2.4) +
      anova_tema(tema, base_size = 13) +
      ggplot2::labs(title = "Resíduos x valores ajustados",
                    x = "Valores ajustados", y = "Resíduos")
  } else {
    dg <- data.frame(ResiduosStd = as.numeric(scale(r$residuals)))
    ggplot2::ggplot(dg, ggplot2::aes(sample = ResiduosStd)) +
      ggplot2::stat_qq(color = "#2E7D8F", alpha = 0.75, size = 2.4) +
      ggplot2::stat_qq_line(color = "#0F3B5F", linewidth = 0.9) +
      anova_tema(tema, base_size = 13) +
      ggplot2::labs(title = "Q-Q plot dos resíduos padronizados",
                    x = "Quantis teóricos", y = "Resíduos padronizados")
  }
}

# ---- Wrappers de compatibilidade (migração) ----------------------------------

#' @rdname arrumar_tabela_anova
mostrar_anova <- function(r) tibble::as_tibble(arrumar_tabela_anova(r))

#' @rdname arrumar_pressupostos_anova
mostrar_pressupostos <- function(r) tibble::as_tibble(arrumar_pressupostos_anova(r))

#' @rdname arrumar_tukey_anova
mostrar_tukey <- function(r) tibble::as_tibble(arrumar_tukey_anova(r))

# ---- Formatação da tabela (identidade Ocean Gradient, saída docx) -----------
flextable_ocean <- function(tab) {
  flextable::flextable(tab) |>
    flextable::theme_booktabs() |>
    flextable::bg(part = "header", bg = "#0F3B5F") |>
    flextable::color(part = "header", color = "white") |>
    flextable::bold(part = "header") |>
    flextable::font(fontname = "Times New Roman", part = "all") |>
    flextable::fontsize(size = 9, part = "all") |>
    flextable::align(align = "center", part = "all") |>
    flextable::align(j = 1, align = "left", part = "all") |>
    flextable::padding(padding = 4, part = "all") |>
    flextable::autofit()
}
