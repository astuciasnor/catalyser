# =============================================================================
# funcoes_regressao.R
# -----------------------------------------------------------------------------
# Funções de apoio para análise de regressão linear e relato.
#
# Conteúdo:
#   mostrar_coefs_regressao()    - tabela estruturada dos coeficientes (tibble)
#   mostrar_metricas_regressao() - tabela de métricas de ajuste do modelo (tibble)
#   relatar_regressao()          - relato descritivo em português da regressão
#   flextable_ocean()            - formata a tabela na identidade Ocean Gradient (docx)
# =============================================================================

library(tibble)
library(flextable)

# ---- Utilitário: formato numérico brasileiro (vírgula decimal) --------------
fmt <- function(x, dig = 2) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return("-")
  formatC(x, format = "f", digits = dig, decimal.mark = ",")
}

# ---- TABELA: Coeficientes da Regressão --------------------------------------
mostrar_coefs_regressao <- function(modelo) {
  sum_fit <- summary(modelo)
  coef_matrix <- sum_fit$coefficients
  df_coef <- as.data.frame(coef_matrix)
  
  tibble::tibble(
    `Termo`        = rownames(coef_matrix),
    `Estimativa`   = round(df_coef[[1]], 4),
    `Erro Padrão`  = round(df_coef[[2]], 4),
    `Valor t`      = round(df_coef[[3]], 3),
    `p-valor`      = round(df_coef[[4]], 4)
  )
}

# ---- TABELA: Métricas de Ajuste ---------------------------------------------
mostrar_metricas_regressao <- function(modelo) {
  sum_fit <- summary(modelo)
  r2 <- sum_fit$r.squared
  adj_r2 <- sum_fit$adj.r.squared
  rse <- sum_fit$sigma
  f_stat <- sum_fit$fstatistic
  
  f_p_val <- if (!is.null(f_stat)) {
    pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)
  } else {
    NA
  }
  
  tibble::tibble(
    `Métrica de Ajuste`            = c("R² (Coeficiente de Determinação)", 
                                       "R² Ajustado", 
                                       "Erro Padrão Residual (RSE)", 
                                       "Estatística F", 
                                       "p-valor do Modelo"),
    `Valor`                        = c(round(r2, 4), 
                                       round(adj_r2, 4), 
                                       round(rse, 4), 
                                       ifelse(is.null(f_stat), "-", round(f_stat[1], 3)), 
                                       ifelse(is.na(f_p_val), "-", round(f_p_val, 4)))
  )
}

# ---- RELATÓRIO NARRATIVO em português ---------------------------------------
relatar_regressao <- function(modelo, label_y = "a variável dependente", label_x = "a variável independente") {
  sum_fit <- summary(modelo)
  coef_matrix <- sum_fit$coefficients
  
  intercepto <- coef_matrix[1, 1]
  inclinacao <- coef_matrix[2, 1]
  t_inclinacao <- coef_matrix[2, 3]
  p_inclinacao <- coef_matrix[2, 4]
  
  r2 <- sum_fit$r.squared
  rse <- sum_fit$sigma
  
  f_stat <- sum_fit$fstatistic
  f_val <- if (!is.null(f_stat)) f_stat[1] else NA
  df_num <- if (!is.null(f_stat)) f_stat[2] else NA
  df_den <- if (!is.null(f_stat)) f_stat[3] else NA
  f_p_val <- if (!is.null(f_stat)) pf(f_val, df_num, df_den, lower.tail = FALSE) else NA
  
  p_inc_txt <- if (p_inclinacao < 0.001) "p < 0,001" else paste0("p = ", fmt(p_inclinacao, 3))
  p_mod_txt <- if (!is.na(f_p_val)) {
    if (f_p_val < 0.001) "p < 0,001" else paste0("p = ", fmt(f_p_val, 3))
  } else {
    "p-valor indisponível"
  }
  
  signif_mod <- if (!is.na(f_p_val) && f_p_val < 0.05) {
    "O modelo geral foi estatisticamente significativo"
  } else {
    "O modelo geral não foi estatisticamente significativo"
  }
  
  signif_inc <- if (p_inclinacao < 0.05) {
    "exerceu um efeito estatisticamente significativo"
  } else {
    "não exerceu um efeito estatisticamente significativo"
  }
  
  direcao_efeito <- if (inclinacao > 0) "positivo" else "negativo"
  acao_efeito <- if (inclinacao > 0) "aumenta" else "diminui"
  
  r2_pct <- r2 * 100
  
  eq <- sprintf("Y = %s + (%s) * X", fmt(intercepto, 4), fmt(inclinacao, 4))
  
  paste0(
    "Foi ajustada uma análise de regressão linear simples para modelar a relação entre ",
    label_y, " (variável de resposta) e ", label_x, " (variável preditora). ",
    signif_mod, ", *F*(", df_num, ", ", df_den, ") = ", fmt(f_val, 3), ", ", p_mod_txt,
    ", com um coeficiente de determinação *R*² de ", fmt(r2, 4), 
    " (explicando ", fmt(r2_pct, 2), "% da variabilidade de ", label_y, "). ",
    "A análise do coeficiente de inclinação indicou que ", label_x, " ", signif_inc, 
    " sobre ", label_y, " (*beta* = ", fmt(inclinacao, 4), ", *t*(", df_den, ") = ", fmt(t_inclinacao, 3),
    ", ", p_inc_txt, "). O efeito estimado foi ", direcao_efeito, ", indicando que para cada unidade de incremento em ",
    label_x, ", a variável ", label_y, " ", acao_efeito, ", em média, ", fmt(abs(inclinacao), 4), " unidades. ",
    "A equação estimada do modelo ajustado foi: *", eq, "*."
  )
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
