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

# Formatadores vetorizados das tabelas. A narrativa e as tabelas usam a mesma
# convenção brasileira: vírgula decimal e "-" para o que não se aplica.
anova_num_col <- function(x, dig = 2) {
  vapply(
    x,
    function(v) if (is.null(v) || is.na(v)) "-" else
      formatC(as.numeric(v), format = "f", digits = dig, decimal.mark = ","),
    character(1), USE.NAMES = FALSE
  )
}

anova_p_col <- function(p, dig = 4) {
  vapply(
    p,
    function(v) {
      if (is.null(v) || is.na(v)) return("-")
      if (v < 0.001) "< 0,001" else formatC(as.numeric(v), format = "f", digits = dig, decimal.mark = ",")
    },
    character(1), USE.NAMES = FALSE
  )
}

# Leitura convencional do tamanho de efeito (referências de Cohen para eta
# quadrado). É convenção estatística, não interpretação biológica: um efeito
# "pequeno" pode ser importante em manejo, e um "grande" pode ser irrelevante.
anova_leitura_efeito <- function(valor) {
  vapply(
    valor,
    function(v) {
      if (is.null(v) || is.na(v)) return("-")
      if (v < 0.01) "muito pequeno"
      else if (v < 0.06) "pequeno"
      else if (v < 0.14) "médio"
      else "grande"
    },
    character(1), USE.NAMES = FALSE
  )
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

# ---- Letras de diferença (compact letter display) -----------------------------

#' Letras compactas a partir das comparações de Tukey
#'
#' Grupos que compartilham ao menos uma letra não apresentaram evidência de
#' diferença entre si. É a convenção usada em pesca e agronomia.
#'
#' O algoritmo é o clássico "inserir e absorver": começa com todos os grupos
#' numa única letra e, a cada par com evidência de diferença, quebra as letras
#' que contêm os dois; no fim, descarta as letras contidas em outras. Está
#' escrito aqui, e não delegado a um pacote, para que o Projeto R exportado
#' funcione sem instalar nada além do que já é exigido.
#'
#' A letra "a" vai para o grupo de maior média.
#'
#' @param pares matriz 2 x n com os nomes dos dois grupos de cada comparação
#' @param p_ajustado vetor de p ajustados, na mesma ordem das colunas de `pares`
#' @param medias vetor nomeado com a média de cada grupo
#' @return vetor de caracteres nomeado por grupo
anova_letras_tukey <- function(pares, p_ajustado, medias, alfa = 0.05) {
  grupos <- names(medias)
  if (!length(grupos)) return(character())
  if (length(grupos) == 1L) return(stats::setNames("a", grupos))

  significativos <- which(!is.na(p_ajustado) & p_ajustado < alfa)
  colunas <- list(grupos)

  for (k in significativos) {
    a <- pares[1, k]
    b <- pares[2, k]
    if (!(a %in% grupos) || !(b %in% grupos)) next
    novas <- list()
    for (col in colunas) {
      if (a %in% col && b %in% col) {
        novas[[length(novas) + 1L]] <- setdiff(col, a)
        novas[[length(novas) + 1L]] <- setdiff(col, b)
      } else {
        novas[[length(novas) + 1L]] <- col
      }
    }
    novas <- Filter(function(x) length(x) > 0L, novas)
    novas <- unique(lapply(novas, sort))
    # Absorver: uma letra contida em outra não acrescenta informação.
    manter <- rep(TRUE, length(novas))
    for (i in seq_along(novas)) {
      for (j in seq_along(novas)) {
        if (i != j && manter[i] && manter[j] &&
            length(novas[[i]]) < length(novas[[j]]) &&
            all(novas[[i]] %in% novas[[j]])) {
          manter[i] <- FALSE
        }
      }
    }
    colunas <- novas[manter]
  }

  # A letra "a" fica com o grupo de maior média.
  chave <- vapply(colunas, function(col) max(medias[col], na.rm = TRUE), numeric(1))
  colunas <- colunas[order(chave, decreasing = TRUE)]

  alfabeto <- if (length(colunas) <= length(letters)) {
    letters
  } else {
    c(letters, paste0(rep(letters, each = length(letters)), letters))
  }

  saida <- stats::setNames(rep("", length(grupos)), grupos)
  for (k in seq_along(colunas)) {
    for (g in colunas[[k]]) saida[[g]] <- paste0(saida[[g]], alfabeto[k])
  }
  saida
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
    n_grupo <- length(valores)
    ep <- stats::sd(valores) / sqrt(n_grupo)
    margem <- if (n_grupo > 1L) {
      stats::qt(1 - (1 - nivel_confianca) / 2, df = n_grupo - 1L) * ep
    } else NA_real_
    data.frame(
      Grupo = nivel,
      N = n_grupo,
      Ausentes = as.integer(ausentes_por_grupo[[nivel]]),
      Media = mean(valores),
      Desvio_Padrao = stats::sd(valores),
      Erro_Padrao = ep,
      IC_Inferior = mean(valores) - margem,
      IC_Superior = mean(valores) + margem,
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

  # --- Letras de diferença ---------------------------------------------------
  # Os pares são reconstruídos a partir dos próprios níveis, e não quebrando o
  # nome "b-a" no hífen: nomes de espécie podem conter hífen ou espaço.
  medias_por_grupo <- stats::setNames(descritivos_df$Media, descritivos_df$Grupo)
  letras <- if (nrow(tukey_df) && length(niveis) > 1L) {
    combos <- utils::combn(niveis, 2L)
    nomes_esperados <- paste0(combos[2, ], "-", combos[1, ])
    posicao <- match(nomes_esperados, tukey_df$Comparacao)
    anova_letras_tukey(
      pares = combos[c(2L, 1L), , drop = FALSE],
      p_ajustado = tukey_df$p_adj[posicao],
      medias = medias_por_grupo
    )
  } else {
    stats::setNames(rep("a", length(niveis)), niveis)
  }
  descritivos_df$Letras <- unname(letras[descritivos_df$Grupo])

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
    letras = letras,
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

#' Resumo por grupo com média ± desvio-padrão e letras de diferença
#'
#' A coluna de letras é a leitura rápida da tabela: grupos que compartilham uma
#' letra não apresentaram evidência de diferença entre si no teste de Tukey.
arrumar_descritivos_anova <- function(r) {
  d <- r$descritivos_df
  nivel <- 100 * (r$nivel_confianca %||% 0.95)
  saida <- data.frame(
    `Grupo` = d$Grupo,
    `n` = d$N,
    `Média ± DP` = paste(anova_num_col(d$Media, 2), "±", anova_num_col(d$Desvio_Padrao, 2)),
    `IC da média` = ifelse(
      is.na(d$IC_Inferior) | is.na(d$IC_Superior),
      "não estimável",
      sprintf("[%s; %s]", anova_num_col(d$IC_Inferior, 2), anova_num_col(d$IC_Superior, 2))
    ),
    `Diferença` = d$Letras,
    `Ausentes excluídos` = d$Ausentes,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  names(saida)[4] <- sprintf("IC %.0f%% da média", nivel)
  saida
}

#' Tabela da ANOVA
#'
#' Casas decimais escolhidas pelo significado: somas e quadrados médios estão na
#' escala da resposta ao quadrado (2 casas bastam), F é um índice adimensional
#' (3 casas) e o p-valor decide (4 casas, ou "< 0,001").
arrumar_tabela_anova <- function(r) {
  data.frame(
    `Fonte de variação` = r$anova_df$Fonte,
    `Graus de liberdade` = r$anova_df$Df,
    `Soma de quadrados` = anova_num_col(r$anova_df$Soma_Quadrados, 2),
    `Quadrado médio` = anova_num_col(r$anova_df$Quadrados_Medios, 2),
    `F` = anova_num_col(r$anova_df$F_valor, 3),
    `p-valor` = anova_p_col(r$anova_df$p_valor),
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
            anova_num_col(r$efeito_df$IC_Inferior, 3),
            anova_num_col(r$efeito_df$IC_Superior, 3))
  )
  data.frame(
    `Medida` = r$efeito_df$Medida,
    `Valor` = anova_num_col(r$efeito_df$Valor, 3),
    `Intervalo de confiança` = ic,
    `Leitura convencional` = anova_leitura_efeito(r$efeito_df$Valor),
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
    `Estatística` = c(
      anova_num_col(r$sh_stat, 4),
      anova_num_col(r$levene_f, 3),
      anova_num_col(r$bt_stat, 3)
    ),
    `p-valor` = anova_p_col(c(r$sh_p, r$levene_p, r$bt_p)),
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
#'
#' Ordenada por p ajustado: com cinco grupos são dez linhas, e quem lê procura
#' primeiro os pares com evidência de diferença.
arrumar_tukey_anova <- function(r) {
  d <- r$tukey_df
  if (nrow(d)) d <- d[order(d$p_adj, na.last = TRUE), , drop = FALSE]
  nivel <- 100 * (r$nivel_confianca %||% 0.95)
  saida <- data.frame(
    `Par comparado` = d$Comparacao,
    `Diferença estimada` = anova_num_col(d$Diferenca, 2),
    `IC inferior` = anova_num_col(d$Lwr, 2),
    `IC superior` = anova_num_col(d$Upr, 2),
    `p ajustado` = anova_p_col(d$p_adj),
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
#' Duas regras moldam este texto:
#'
#' 1. Nunca afirma a aceitação da hipótese nula — ausência de evidência não é
#'    evidência de ausência.
#' 2. Não repete o que as tabelas ao lado já mostram. As médias de cada grupo
#'    ficam no resumo por grupo; os p de Shapiro-Wilk e Levene ficam na tabela de
#'    pressupostos. A narrativa dá a pergunta, a amostra, a decisão, o tamanho do
#'    efeito e a síntese dos pares — e remete ao resto.
relatar_anova <- function(r) {
  abertura <- sprintf(
    paste0(
      "A pergunta analisada foi se a média de '%s' difere entre os %d grupos de '%s' (%s). ",
      "Entraram %d observações completas%s. "
    ),
    r$dep_var, r$n_grupos, r$ind_var, paste(r$grupos, collapse = ", "), r$n,
    if (r$excluidos > 0)
      sprintf(", depois de excluir %d linha(s) com dados faltantes na resposta ou no fator",
              r$excluidos)
    else " (nenhuma linha foi excluída por dados faltantes)"
  )

  aviso_pequenos <- if (length(r$grupos_pequenos)) {
    sprintf("Atenção: os grupos %s têm menos de cinco observações, o que torna o teste menos sensível. ",
            paste(r$grupos_pequenos, collapse = ", "))
  } else ""

  efeito <- sprintf(
    "O fator explicou %s%% da variação da resposta (η² = %s; ω² = %s), efeito %s pela convenção de Cohen. ",
    anova_fmt(100 * r$eta2, 1), anova_fmt(r$eta2, 3), anova_fmt(r$omega2, 3),
    anova_leitura_efeito(r$eta2)
  )

  resultado <- if (!is.na(r$p_anova) && r$p_anova < 0.05) {
    pares <- r$tukey_df$Comparacao[!is.na(r$tukey_df$p_adj) & r$tukey_df$p_adj < 0.05]
    complemento <- if (length(pares)) {
      sprintf(
        paste0(
          "Entre os %d pares comparados por Tukey, houve evidência de diferença em %s. ",
          "A interpretação principal decorre da ANOVA global e do plano analítico definido antes da coleta, não de uma varredura de pares. "
        ),
        nrow(r$tukey_df), paste(pares, collapse = "; ")
      )
    } else {
      paste0(
        "Nenhum par isolado apresentou evidência de diferença no teste de Tukey — ",
        "situação possível quando o efeito global é modesto e o ajuste para comparações múltiplas é conservador. "
      )
    }
    sprintf(
      "Rejeitou-se H0 de igualdade das médias: F(%d; %d) = %s, %s. %s%s",
      r$df_entre, r$df_dentro, anova_fmt(r$f_anova, 3), anova_p_texto(r$p_anova),
      efeito, complemento
    )
  } else {
    sprintf(
      paste0(
        "Não houve evidência suficiente para rejeitar H0 de igualdade das médias: ",
        "F(%d; %d) = %s, %s. %s",
        "Isso não significa que as médias sejam iguais; significa que estes dados não permitiram detectar diferença. "
      ),
      r$df_entre, r$df_dentro, anova_fmt(r$f_anova, 3), anova_p_texto(r$p_anova), efeito
    )
  }

  remissao <- paste0(
    "As médias e dispersões de cada grupo estão no resumo por grupo; os testes de ",
    "normalidade dos resíduos e de homogeneidade de variâncias, na tabela de pressupostos, ",
    "que deve ser lida junto com os gráficos de resíduos e o Q-Q plot. "
  )

  fecho <- paste0(
    "A ANOVA compara médias entre grupos observados; por si só, não estabelece relação de causa e efeito."
  )

  paste0(abertura, aviso_pequenos, resultado, remissao, fecho)
}

# ---- Gráficos ----------------------------------------------------------------

#' Gráfico principal da ANOVA: barras com IC e letras
#'
#' Barras com a média de cada grupo, barras de erro com o intervalo de confiança
#' da média e, acima delas, as letras de diferença. Grupos que compartilham uma
#' letra não apresentaram evidência de diferença no teste de Tukey.
#'
#' Duas decisões de leitura: o eixo Y começa em zero, porque em gráfico de barras
#' o comprimento é o que se compara; e as médias de níveis nominais não são
#' conectadas por linha, que sugeriria uma ordem inexistente entre espécies.
grafico_anova <- function(r, titulo = NULL, rotulo_x = NULL, rotulo_y = NULL,
                          tema = "minimal") {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("O pacote ggplot2 é necessário para o gráfico da ANOVA.", call. = FALSE)
  nivel <- r$nivel_confianca %||% 0.95
  resumo <- r$descritivos_df
  resumo$fator <- factor(resumo$Grupo, levels = levels(r$dados$fator))

  titulo_final <- titulo %||% sprintf("%s por %s", r$dep_var, r$ind_var)
  if (!nzchar(titulo_final)) titulo_final <- sprintf("%s por %s", r$dep_var, r$ind_var)

  # Espaço acima da maior barra de erro para as letras não encostarem no topo.
  topo <- max(c(resumo$IC_Superior, resumo$Media), na.rm = TRUE)
  # Médias negativas tornam o zero um piso enganoso; nesse caso, escala livre.
  piso <- if (min(c(resumo$IC_Inferior, resumo$Media), na.rm = TRUE) < 0) NA_real_ else 0

  cores <- rep(anova_cores_ocean, length.out = nlevels(resumo$fator))

  ggplot2::ggplot(resumo, ggplot2::aes(x = fator, y = Media)) +
    ggplot2::geom_col(
      ggplot2::aes(fill = fator), width = 0.66, show.legend = FALSE, alpha = 0.92
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = IC_Inferior, ymax = IC_Superior),
      width = 0.16, linewidth = 0.8, color = "#0F3B5F"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(y = IC_Superior, label = Letras),
      vjust = -0.7, fontface = "bold", size = 4.6, color = "#0F3B5F"
    ) +
    ggplot2::scale_fill_manual(values = cores) +
    ggplot2::scale_y_continuous(
      limits = c(piso, topo * 1.18),
      expand = ggplot2::expansion(mult = c(0, 0.02))
    ) +
    anova_tema(tema) +
    ggplot2::labs(
      title = titulo_final,
      subtitle = sprintf(
        "Barras = média; hastes = IC %.0f%% da média; grupos com a mesma letra não diferiram (Tukey)",
        100 * nivel
      ),
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
