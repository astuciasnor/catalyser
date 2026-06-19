# =============================================================================
# funcoes_anova.R
# -----------------------------------------------------------------------------
# Funções de apoio para análise e relato de Análise de Variância (ANOVA) unifatorial.
#
# Arquitetura (fonte canônica única):
#   calcular_anova()      -> executa o ajuste, pressupostos e Tukey HSD.
#   mostrar_anova()       -> formata a tabela ANOVA.
#   mostrar_tukey()       -> formata a tabela de comparações de Tukey HSD.
#   mostrar_pressupostos()-> formata os testes de Shapiro-Wilk e Bartlett.
#   relatar_anova()       -> frase-relatório em português sintetizando os achados.
# =============================================================================

library(tibble)
library(flextable)

# Evitar erros de 'req' não encontrado fora do Shiny
if (!exists("req", mode = "function")) {
  req <- function(...) {
    invisible(TRUE)
  }
}

# ---- Utilitário: Formato numérico brasileiro ---------------------------------
fmt <- function(x, dig = 2) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return("-")
  formatC(x, format = "f", digits = dig, decimal.mark = ",")
}

#' Executa o cálculo da ANOVA de um fator, pressupostos e pós-teste de Tukey
calcular_anova <- function(df, dep_var, ind_var) {
  req(df, dep_var, ind_var)
  
  # Preparar variáveis
  y <- df[[dep_var]]
  x <- as.factor(df[[ind_var]])
  
  # Ajustar modelo aov
  fit <- aov(y ~ x)
  anova_summary <- summary(fit)[[1]]
  
  # Tabela ANOVA estruturada
  anova_df <- data.frame(
    Fonte = c("Entre Grupos (Fator)", "Dentro dos Grupos (Resíduos)", "Total"),
    Df = c(anova_summary$Df[1], anova_summary$Df[2], sum(anova_summary$Df)),
    Soma_Quadrados = c(anova_summary$`Sum Sq`[1], anova_summary$`Sum Sq`[2], sum(anova_summary$`Sum Sq`)),
    Quadrados_Medios = c(anova_summary$`Mean Sq`[1], anova_summary$`Mean Sq`[2], NA),
    F_valor = c(anova_summary$`F value`[1], NA, NA),
    p_valor = c(anova_summary$`Pr(>F)`[1], NA, NA),
    stringsAsFactors = FALSE
  )
  
  # Pressupostos
  # 1. Normalidade dos Resíduos (Shapiro-Wilk)
  residuos <- residuals(fit)
  sh_test <- if(length(residuos) >= 3 && length(residuos) <= 5000) shapiro.test(residuos) else list(statistic = NA, p.value = NA)
  
  # 2. Homocedasticidade (Bartlett)
  bt_test <- tryCatch(bartlett.test(y ~ x), error = function(e) list(statistic = NA, p.value = NA))
  
  # Pós-teste de Tukey HSD
  tukey_res <- TukeyHSD(fit)
  tukey_data <- as.data.frame(tukey_res[[1]])
  
  # Formata tabela de Tukey
  tukey_df <- data.frame(
    Comparacao = rownames(tukey_data),
    Diferenca = tukey_data$diff,
    Lwr = tukey_data$lwr,
    Upr = tukey_data$upr,
    p_adj = tukey_data$`p adj`,
    Significativo = tukey_data$`p adj` < 0.05,
    stringsAsFactors = FALSE
  )
  
  list(
    dep_var = dep_var,
    ind_var = ind_var,
    anova_df = anova_df,
    sh_stat = unname(sh_test$statistic),
    sh_p = sh_test$p.value,
    bt_stat = unname(bt_test$statistic),
    bt_p = bt_test$p.value,
    tukey_df = tukey_df,
    p_anova = anova_summary$`Pr(>F)`[1],
    f_anova = anova_summary$`F value`[1],
    df_entre = anova_summary$Df[1],
    df_dentro = anova_summary$Df[2],
    residuals = residuos,
    fitted = fitted(fit)
  )
}

#' Formata a tabela ANOVA
mostrar_anova <- function(r) {
  tab <- tibble::tibble(
    `Fonte de Variação` = r$anova_df$Fonte,
    `Graus de Liberdade (gl)` = r$anova_df$Df,
    `Soma dos Quadrados (SQ)` = round(r$anova_df$Soma_Quadrados, 2),
    `Quadrado Médio (QM)` = sapply(r$anova_df$Quadrados_Medios, function(x) if(is.na(x)) "-" else as.character(round(x, 2))),
    `F` = sapply(r$anova_df$F_valor, function(x) if(is.na(x)) "-" else as.character(round(x, 3))),
    `p-valor` = sapply(r$anova_df$p_valor, function(x) if(is.na(x)) "-" else as.character(round(x, 4)))
  )
  tab
}

#' Formata a tabela dos pressupostos
mostrar_pressupostos <- function(r) {
  tibble::tibble(
    `Pressuposto` = c("Normalidade dos Resíduos (Shapiro-Wilk)", "Homocedasticidade das Variâncias (Bartlett)"),
    `Estatística de Teste` = c(round(r$sh_stat, 4), round(r$bt_stat, 3)),
    `p-valor` = c(round(r$sh_p, 4), round(r$bt_p, 4)),
    `Resultado` = c(
      ifelse(r$sh_p >= 0.05, "Resíduos Normais (H0 mantida)", "Resíduos Não Normais (H0 rejeitada)"),
      ifelse(r$bt_p >= 0.05, "Variâncias Homogêneas (H0 mantida)", "Variâncias Heterogêneas (H0 rejeitada)")
    )
  )
}

#' Formata a tabela de comparações múltiplas de Tukey HSD
mostrar_tukey <- function(r) {
  tab <- tibble::tibble(
    `Comparação Par a Par` = r$tukey_df$Comparacao,
    `Diferença de Médias` = round(r$tukey_df$Diferenca, 2),
    `IC 95% Inferior` = round(r$tukey_df$Lwr, 2),
    `IC 95% Superior` = round(r$tukey_df$Upr, 2),
    `p-valor ajustado` = round(r$tukey_df$p_adj, 4),
    `Significância (p < 0,05)` = ifelse(r$tukey_df$Significativo, "Diferença Significativa", "Não Significativa")
  )
  tab
}

#' Gera o relato estatístico formal em português
relatar_anova <- function(r) {
  p_txt <- if (r$p_anova < 0.001) "p < 0,001" else paste0("p = ", fmt(r$p_anova, 3))
  
  # Pressupostos texto
  sh_txt <- if (r$sh_p >= 0.05) "passou na validação de normalidade" else "apresentou desvio de normalidade"
  bt_txt <- if (r$bt_p >= 0.05) "passou na validação de homocedasticidade" else "apresentou desvio de homocedasticidade"
  
  relato_base <- paste0(
    "Foi realizada uma análise de variância (ANOVA) unifatorial para avaliar o efeito do fator categórico '", r$ind_var,
    "' sobre a variável numérica '", r$dep_var, "'. Os resíduos do modelo estatístico foram avaliados ",
    "e o teste de Shapiro-Wilk (W = ", fmt(r$sh_stat, 4), ", p = ", fmt(r$sh_p, 4), ") ", sh_txt, 
    ", enquanto o teste de Bartlett (K² = ", fmt(r$bt_stat, 3), ", p = ", fmt(r$bt_p, 4), ") ", bt_txt, ". "
  )
  
  if (r$p_anova < 0.05) {
    # Tukey significativo pares
    sig_pairs <- r$tukey_df$Comparacao[r$tukey_df$Significativo]
    sig_txt <- if (length(sig_pairs) > 0) {
      paste0("O pós-teste de comparações múltiplas de Tukey HSD identificou diferenças significativas nos pares: ", 
             paste(sig_pairs, collapse = "; "), ".")
    } else {
      "Contudo, o pós-teste de comparações múltiplas de Tukey HSD não revelou pares com diferenças estatisticamente significativas."
    }
    
    paste0(
      relato_base,
      "A ANOVA indicou um efeito estatisticamente significativo do fator sobre a resposta, ",
      "F(", r$df_entre, ", ", r$df_dentro, ") = ", fmt(r$f_anova, 2), ", ", p_txt, ". ",
      sig_txt
    )
  } else {
    paste0(
      relato_base,
      "A ANOVA não indicou efeito estatisticamente significativo do fator sobre a resposta, ",
      "F(", r$df_entre, ", ", r$df_dentro, ") = ", fmt(r$f_anova, 2), ", ", p_txt, ", ",
      "não havendo justificativa estatística para a interpretação de pós-testes de comparações múltiplas."
    )
  }
}

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
