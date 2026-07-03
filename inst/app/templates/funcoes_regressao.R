# =============================================================================
# funcoes_regressao.R
# -----------------------------------------------------------------------------
# Funções de apoio para análise de regressão linear e relato.
# Serve como fallback local offline para os projetos dos alunos.
# =============================================================================

library(tibble)
library(flextable)

# ---- Utilitário: formato numérico brasileiro (vírgula decimal) --------------
fmt <- function(x, dig = 2) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return("-")
  formatC(x, format = "f", digits = dig, decimal.mark = ",")
}

# ---- CÁLCULO canônico da regressão linear simples ---------------------------
calcular_regressao <- function(modelo_ou_dados, var_y = NULL, var_x = NULL) {
  if (inherits(modelo_ou_dados, "lm")) {
    fit <- modelo_ou_dados
    vars <- all.vars(formula(fit))
    y_var <- vars[1]
    x_var <- vars[2]
  } else {
    df <- modelo_ou_dados
    req_cols <- c(var_y, var_x)
    clean_df <- na.omit(df[, req_cols, drop = FALSE])
    formula_obj <- as.formula(paste0("`", var_y, "` ~ `", var_x, "`"))
    fit <- lm(formula_obj, data = clean_df)
    y_var <- var_y
    x_var <- var_x
  }
  
  sum_fit <- summary(fit)
  coef_matrix <- sum_fit$coefficients
  
  r2 <- sum_fit$r.squared
  adj_r2 <- sum_fit$adj.r.squared
  rse <- sum_fit$sigma
  df_residual <- sum_fit$df[2]
  
  f_stat <- sum_fit$fstatistic
  f_val <- if (!is.null(f_stat)) f_stat[1] else NA_real_
  df_num <- if (!is.null(f_stat)) f_stat[2] else NA_real_
  df_den <- if (!is.null(f_stat)) f_stat[3] else NA_real_
  f_p_val <- if (!is.null(f_stat)) pf(f_val, df_num, df_den, lower.tail = FALSE) else NA_real_
  
  # Pressupostos
  residuos <- residuals(fit)
  sh_test <- if (length(residuos) >= 3 && length(residuos) <= 5000)
    shapiro.test(residuos) else list(statistic = NA_real_, p.value = NA_real_)
  
  res_sq <- residuos^2
  fitted_vals <- fitted(fit)
  bp_fit <- tryCatch(lm(res_sq ~ fitted_vals), error = function(e) NULL)
  bp_summary <- if (!is.null(bp_fit)) summary(bp_fit) else NULL
  bp_f <- if (!is.null(bp_summary)) bp_summary$fstatistic else NULL
  bp_p <- if (!is.null(bp_f)) {
    pf(bp_f[1], bp_f[2], bp_f[3], lower.tail = FALSE)
  } else {
    NA_real_
  }
  bp_stat <- if (!is.null(bp_f)) bp_f[1] else NA_real_
  
  list(
    modelo = fit,
    var_y = y_var,
    var_x = x_var,
    coefs = coef_matrix,
    r2 = r2,
    adj_r2 = adj_r2,
    rse = rse,
    df_residual = df_residual,
    f_val = f_val,
    df_num = df_num,
    df_den = df_den,
    f_p_val = f_p_val,
    sh_stat = unname(sh_test$statistic),
    sh_p = sh_test$p.value,
    bp_stat = bp_stat,
    bp_p = bp_p
  )
}

# ---- TABELA: Coeficientes da Regressão --------------------------------------
mostrar_coefs_regressao <- function(r) {
  if (!is.list(r) || is.null(r$coefs)) {
    r <- calcular_regressao(r)
  }
  coef_matrix <- r$coefs
  df_coef <- as.data.frame(coef_matrix)
  
  tibble::tibble(
    "Termo"        = rownames(coef_matrix),
    "Estimativa"   = round(df_coef[[1]], 4),
    "Erro Padr\u00e3o"  = round(df_coef[[2]], 4),
    "Valor t"      = round(df_coef[[3]], 3),
    "p-valor"      = round(df_coef[[4]], 4)
  )
}

# ---- TABELA: Métricas de Ajuste ---------------------------------------------
mostrar_metricas_regressao <- function(r) {
  if (!is.list(r) || is.null(r$r2)) {
    r <- calcular_regressao(r)
  }
  
  tibble::tibble(
    "M\u00e9trica de Ajuste" = c(
      "R\u00b2 (Coeficiente de Determina\u00e7\u00e3o)", 
      "R\u00b2 Ajustado", 
      "Erro Padr\u00e3o Residual (RSE)", 
      "Estat\u00edstica F", 
      "p-valor do Modelo"
    ),
    "Valor" = c(
      fmt(r$r2, 4), 
      fmt(r$adj_r2, 4), 
      fmt(r$rse, 4), 
      if (is.na(r$f_val)) "-" else fmt(r$f_val, 3), 
      if (is.na(r$f_p_val)) "-" else fmt(r$f_p_val, 4)
    )
  )
}

# ---- TABELA: Pressupostos da Regressão --------------------------------------
mostrar_pressupostos_regressao <- function(r) {
  if (!is.list(r) || is.null(r$sh_p)) {
    r <- calcular_regressao(r)
  }
  
  tibble::tibble(
    "Pressuposto" = c(
      "Normalidade dos Res\u00edduos (Shapiro-Wilk)",
      "Homocedasticidade das Vari\u00e2ncias (Auxiliar F)"
    ),
    "Estat\u00edstica de Teste" = c(
      if (is.na(r$sh_stat)) "-" else fmt(r$sh_stat, 4),
      if (is.na(r$bp_stat)) "-" else fmt(r$bp_stat, 3)
    ),
    "p-valor" = c(
      if (is.na(r$sh_p)) "-" else fmt(r$sh_p, 4),
      if (is.na(r$bp_p)) "-" else fmt(r$bp_p, 4)
    ),
    "Resultado" = c(
      if (is.na(r$sh_p)) "N/A" else ifelse(r$sh_p >= 0.05, "Res\u00edduos Normais (H0 mantida)", "Desvio de Normalidade (H0 rejeitada)"),
      if (is.na(r$bp_p)) "N/A" else ifelse(r$bp_p >= 0.05, "Vari\u00e2ncias Homog\u00eaneas (H0 mantida)", "Heterocedasticidade (H0 rejeitada)")
    )
  )
}

# ---- RELATÓRIO NARRATIVO em português ---------------------------------------
relatar_regressao <- function(r, label_y = NULL, label_x = NULL) {
  if (!is.list(r) || is.null(r$r2)) {
    r_fit <- calcular_regressao(r)
  } else {
    r_fit <- r
  }
  
  if (is.null(label_y)) label_y <- paste0("a vari\u00e1vel ", r_fit$var_y)
  if (is.null(label_x)) label_x <- paste0("a vari\u00e1vel ", r_fit$var_x)
  
  coef_matrix <- r_fit$coefs
  
  intercepto <- coef_matrix[1, 1]
  inclinacao <- coef_matrix[2, 1]
  t_inclinacao <- coef_matrix[2, 3]
  p_inclinacao <- coef_matrix[2, 4]
  
  r2 <- r_fit$r2
  f_val <- r_fit$f_val
  df_num <- r_fit$df_num
  df_den <- r_fit$df_den
  f_p_val <- r_fit$f_p_val
  
  p_inc_txt <- if (p_inclinacao < 0.001) "p < 0,001" else paste0("p = ", fmt(p_inclinacao, 3))
  p_mod_txt <- if (!is.na(f_p_val)) {
    if (f_p_val < 0.001) "p < 0,001" else paste0("p = ", fmt(f_p_val, 3))
  } else {
    "p-valor indispon\u00edvel"
  }
  
  signif_mod <- if (!is.na(f_p_val) && f_p_val < 0.05) {
    "O modelo geral foi estatisticamente significativo"
  } else {
    "O modelo geral n\u00e3o foi estatisticamente significativo"
  }
  
  signif_inc <- if (p_inclinacao < 0.05) {
    "exerceu um efeito estatisticamente significativo"
  } else {
    "n\u00e3o exerceu um efeito estatisticamente significativo"
  }
  
  direcao_efeito <- if (inclinacao > 0) "positivo" else "negativo"
  acao_efeito <- if (inclinacao > 0) "aumenta" else "diminui"
  
  r2_pct <- r2 * 100
  
  eq <- sprintf("Y = %s + (%s) * X", fmt(intercepto, 4), fmt(inclinacao, 4))
  
  paste0(
    "Foi ajustada uma an\u00e1lise de regress\u00e3o linear simples para modelar a rela\u00e7\u00e3o entre ",
    label_y, " (vari\u00e1vel de resposta) e ", label_x, " (vari\u00e1vel preditora). ",
    signif_mod, ", *F*(", df_num, ", ", df_den, ") = ", fmt(f_val, 3), ", ", p_mod_txt,
    ", com um coeficiente de determina\u00e7\u00e3o *R*\u00b2 de ", fmt(r2, 4), 
    " (explicando ", fmt(r2_pct, 2), "% da variabilidade de ", label_y, "). ",
    "A an\u00e1lise do coeficiente de inclina\u00e7\u00e3o indicou que ", label_x, " ", signif_inc, 
    " sobre ", label_y, " (*beta* = ", fmt(inclinacao, 4), ", *t*(", df_den, ") = ", fmt(t_inclinacao, 3),
    ", ", p_inc_txt, "). O efeito estimado foi ", direcao_efeito, ", indicando que para cada unidade de incremento em ",
    label_x, ", a vari\u00e1vel ", label_y, " ", acao_efeito, ", em m\u00e9dia, ", fmt(abs(inclinacao), 4), " unidades. ",
    "A equa\u00e7\u00e3o estimada do modelo ajustado foi: *", eq, "*."
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
