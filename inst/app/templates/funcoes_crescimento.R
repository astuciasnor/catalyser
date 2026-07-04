# =============================================================================
# funcoes_crescimento.R
# -----------------------------------------------------------------------------
# Funcoes de apoio para REGRESSAO NAO-LINEAR na CatalyseR:
#   - Modelo de potencia (relacao peso-comprimento):  W = a * L^b
#   - Von Bertalanffy:  L(t) = Linf * (1 - exp(-k * (t - t0)))
#   - Logistico:        L(t) = Linf / (1 + exp(-k * (t - tm)))
#
# Motor reutilizavel pela IDE (ajuste ao vivo) e pelos relatorios .qmd.
# Serve como fallback local offline para os projetos .zip dos alunos.
# =============================================================================

if (!requireNamespace("tibble", quietly = TRUE)) {
  # tibble e esperado no ambiente da IDE; o guard evita erro em fallback minimo.
}
suppressWarnings(suppressMessages(library(tibble)))

# ---- Utilitario: formato numerico brasileiro (virgula decimal) --------------
if (!exists("fmt")) {
  fmt <- function(x, dig = 2) {
    if (is.null(x) || length(x) == 0 || is.na(x)) return("-")
    formatC(x, format = "f", digits = dig, decimal.mark = ",")
  }
}

# ---- Rotulo amigavel do tipo de modelo --------------------------------------
tipo_curva_label <- function(tipo) {
  switch(tipo,
    "potencia"        = "Modelo de Potência (W = a·L^b)",
    "von_bertalanffy" = "Von Bertalanffy",
    "logistico"       = "Logístico",
    "exponencial"     = "Curva Exponencial (Y = a·e^(b·X))",
    "polinomial"      = "Modelo Polinomial (Regressão Quadrática)",
    "logaritmica"     = "Curva Logarítmica (Y = a + b·ln(X))",
    tipo)
}

# ---- Valores iniciais robustos ----------------------------------------------
.start_potencia <- function(x, y) {
  ok <- x > 0 & y > 0
  if (sum(ok) >= 2) {
    lm0 <- stats::lm(log(y[ok]) ~ log(x[ok]))
    a0 <- exp(unname(stats::coef(lm0)[1]))
    b0 <- unname(stats::coef(lm0)[2])
    if (!is.finite(a0) || a0 <= 0) a0 <- 1e-3
    if (!is.finite(b0)) b0 <- 3
  } else {
    a0 <- 1e-3; b0 <- 3
  }
  list(a = a0, b = b0)
}

.start_vonbert <- function(x, y) {
  s <- tryCatch({
    m0 <- stats::nls(y ~ stats::SSasymp(x, Asym, R0, lrc))
    cc <- stats::coef(m0)
    Linf <- unname(cc["Asym"]); R0 <- unname(cc["R0"]); k <- exp(unname(cc["lrc"]))
    ratio <- 1 - R0 / Linf
    t0 <- if (is.finite(ratio) && ratio > 0 && k > 0) log(ratio) / k else 0
    list(Linf = Linf, k = k, t0 = t0)
  }, error = function(e) NULL)
  if (is.null(s)) s <- list(Linf = 1.05 * max(y), k = 0.3, t0 = 0)
  s
}

.start_logistico <- function(x, y) {
  s <- tryCatch({
    m0 <- stats::nls(y ~ stats::SSlogis(x, Asym, xmid, scal))
    cc <- stats::coef(m0)
    list(Linf = unname(cc["Asym"]), k = 1 / unname(cc["scal"]), tm = unname(cc["xmid"]))
  }, error = function(e) NULL)
  if (is.null(s)) s <- list(Linf = 1.05 * max(y), k = 1, tm = stats::median(x))
  s
}

.start_exponencial <- function(x, y) {
  ok <- y > 0
  if (sum(ok) >= 2) {
    lm0 <- stats::lm(log(y[ok]) ~ x[ok])
    a0 <- exp(unname(stats::coef(lm0)[1]))
    b0 <- unname(stats::coef(lm0)[2])
    if (!is.finite(a0) || a0 <= 0) a0 <- 1
    if (!is.finite(b0)) b0 <- 0.1
  } else {
    a0 <- 1; b0 <- 0.1
  }
  list(a = a0, b = b0)
}

.start_polinomial <- function(x, y) {
  lm0 <- stats::lm(y ~ x + I(x^2))
  cc <- stats::coef(lm0)
  list(b0 = unname(cc[1]), b1 = unname(cc[2]), b2 = unname(cc[3]))
}

.start_logaritmica <- function(x, y) {
  ok <- x > 0
  if (sum(ok) >= 2) {
    lm0 <- stats::lm(y[ok] ~ log(x[ok]))
    cc <- stats::coef(lm0)
    a0 <- unname(cc[1])
    b0 <- unname(cc[2])
  } else {
    a0 <- 0; b0 <- 1
  }
  list(a = a0, b = b0)
}

# ---- CALCULO canonico do ajuste nao-linear ----------------------------------
# Retorna uma lista com: modelo (nls), tipo, variaveis, coeficientes, metricas
# de ajuste (pseudo-R2, RSE, AIC), teste de normalidade dos residuos e os dados.
ajustar_curva <- function(dados, var_y, var_x,
                          tipo = c("potencia", "von_bertalanffy", "logistico", "exponencial", "polinomial", "logaritmica")) {
  tipo <- match.arg(tipo)
  df <- stats::na.omit(dados[, c(var_x, var_y), drop = FALSE])
  x <- as.numeric(df[[var_x]])
  y <- as.numeric(df[[var_y]])
  if (length(y) < 3) stop("Dados insuficientes para o ajuste não-linear (mínimo 3 pontos).")
  df2 <- data.frame(x = x, y = y)

  ctrl <- stats::nls.control(maxiter = 200, warnOnly = TRUE, minFactor = 1e-10)

  fit <- switch(tipo,
    "potencia" = {
      st <- .start_potencia(x, y)
      tryCatch(stats::nls(y ~ a * x^b, data = df2, start = st, control = ctrl),
               error = function(e) NULL)
    },
    "von_bertalanffy" = {
      st <- .start_vonbert(x, y)
      tryCatch(stats::nls(y ~ Linf * (1 - exp(-k * (x - t0))), data = df2,
                          start = st, control = ctrl),
               error = function(e) NULL)
    },
    "logistico" = {
      st <- .start_logistico(x, y)
      tryCatch(stats::nls(y ~ Linf / (1 + exp(-k * (x - tm))), data = df2,
                          start = st, control = ctrl),
               error = function(e) NULL)
    },
    "exponencial" = {
      st <- .start_exponencial(x, y)
      tryCatch(stats::nls(y ~ a * exp(b * x), data = df2, start = st, control = ctrl),
               error = function(e) NULL)
    },
    "polinomial" = {
      st <- .start_polinomial(x, y)
      tryCatch(stats::nls(y ~ b0 + b1 * x + b2 * x^2, data = df2, start = st, control = ctrl),
               error = function(e) NULL)
    },
    "logaritmica" = {
      st <- .start_logaritmica(x, y)
      tryCatch(stats::nls(y ~ a + b * log(x), data = df2, start = st, control = ctrl),
               error = function(e) NULL)
    }
  )

  if (is.null(fit)) {
    stop("Não foi possível ajustar o modelo ", tipo_curva_label(tipo),
         " aos dados. Verifique as variáveis escolhidas.")
  }

  sum_fit <- summary(fit)
  coef_matrix <- sum_fit$coefficients
  resid <- stats::residuals(fit)
  ss_res <- sum(resid^2)
  ss_tot <- sum((y - mean(y))^2)
  pseudo_r2 <- if (ss_tot > 0) 1 - ss_res / ss_tot else NA_real_
  n <- length(y)
  n_par <- length(stats::coef(fit))
  rse <- if (n > n_par) sqrt(ss_res / (n - n_par)) else NA_real_
  aic <- tryCatch(stats::AIC(fit), error = function(e) NA_real_)

  # Normalidade dos residuos (teste simples, coerente com a regressao linear)
  sh <- if (n >= 3 && n <= 5000) tryCatch(stats::shapiro.test(resid),
                                          error = function(e) NULL) else NULL

  list(
    modelo = fit, tipo = tipo, var_y = var_y, var_x = var_x,
    coefs = coef_matrix, params = stats::coef(fit),
    pseudo_r2 = pseudo_r2, rse = rse, aic = aic, n = n,
    sh_stat = if (!is.null(sh)) unname(sh$statistic) else NA_real_,
    sh_p    = if (!is.null(sh)) sh$p.value else NA_real_,
    x = x, y = y
  )
}

# ---- Equacao ajustada formatada (PT-BR) -------------------------------------
equacao_curva <- function(r) {
  p <- r$params
  if (r$tipo == "potencia") {
    sprintf("Y = %s · X^%s", fmt(unname(p["a"]), 5), fmt(unname(p["b"]), 4))
  } else if (r$tipo == "von_bertalanffy") {
    sprintf("L(t) = %s · (1 − e^(−%s(t − %s)))",
            fmt(unname(p["Linf"]), 2), fmt(unname(p["k"]), 4), fmt(unname(p["t0"]), 4))
  } else if (r$tipo == "logistico") {
    sprintf("L(t) = %s / (1 + e^(−%s(t − %s)))",
            fmt(unname(p["Linf"]), 2), fmt(unname(p["k"]), 4), fmt(unname(p["tm"]), 4))
  } else if (r$tipo == "exponencial") {
    sprintf("Y = %s · e^(%s · X)", fmt(unname(p["a"]), 4), fmt(unname(p["b"]), 4))
  } else if (r$tipo == "polinomial") {
    sprintf("Y = %s + (%s) · X + (%s) · X²", fmt(unname(p["b0"]), 4), fmt(unname(p["b1"]), 4), fmt(unname(p["b2"]), 4))
  } else if (r$tipo == "logaritmica") {
    sprintf("Y = %s + (%s) · ln(X)", fmt(unname(p["a"]), 4), fmt(unname(p["b"]), 4))
  } else {
    "Equação não suportada"
  }
}

# ---- TABELA: coeficientes / parametros do modelo ----------------------------
mostrar_coefs_curva <- function(r) {
  if (!is.list(r) || is.null(r$coefs)) stop("Objeto de ajuste inválido.")
  cm <- as.data.frame(r$coefs)
  tibble::tibble(
    "Parâmetro"    = rownames(r$coefs),
    "Estimativa"        = round(cm[[1]], 5),
    "Erro Padrão"   = round(cm[[2]], 5),
    "Valor t"           = round(cm[[3]], 3),
    "p-valor"           = round(cm[[4]], 4)
  )
}

# ---- TABELA: metricas de ajuste ---------------------------------------------
mostrar_metricas_curva <- function(r) {
  tibble::tibble(
    "Métrica de Ajuste" = c(
      "Modelo", "Pseudo-R²", "Erro Padrão Residual (RSE)",
      "AIC", "N observações"
    ),
    "Valor" = c(
      tipo_curva_label(r$tipo),
      if (is.na(r$pseudo_r2)) "-" else paste0(fmt(r$pseudo_r2, 4), " (", fmt(r$pseudo_r2 * 100, 2), "%)"),
      fmt(r$rse, 4),
      if (is.na(r$aic)) "-" else fmt(r$aic, 2),
      as.character(r$n)
    )
  )
}

# ---- TABELA: normalidade dos residuos (Shapiro-Wilk) ------------------------
mostrar_normalidade_curva <- function(r) {
  tibble::tibble(
    "Teste" = "Shapiro-Wilk",
    "Estatística W" = if (is.na(r$sh_stat)) "-" else fmt(r$sh_stat, 4),
    "p-valor" = if (is.na(r$sh_p)) "-" else fmt(r$sh_p, 4),
    "Resultado" = if (is.na(r$sh_p)) "N/A" else ifelse(
      r$sh_p >= 0.05,
      "Resíduos normais (H0 mantida)",
      "Desvio de normalidade (H0 rejeitada)"
    )
  )
}

# ---- Curva predita em grade fina (para plotagem) ----------------------------
curva_predita <- function(r, n = 200) {
  xr <- range(r$x, na.rm = TRUE)
  grid <- data.frame(x = seq(xr[1], xr[2], length.out = n))
  grid$y <- as.numeric(stats::predict(r$modelo, newdata = grid))
  stats::setNames(grid, c(r$var_x, r$var_y))
}

# ---- RELATORIO narrativo em portugues ---------------------------------------
relatar_curva <- function(r, label_y = NULL, label_x = NULL) {
  if (is.null(label_y)) label_y <- paste0("a variável ", r$var_y)
  if (is.null(label_x)) label_x <- paste0("a variável ", r$var_x)
  p <- r$params
  r2txt <- if (is.na(r$pseudo_r2)) "indisponível"
           else paste0(fmt(r$pseudo_r2, 4), " (", fmt(r$pseudo_r2 * 100, 2), "%)")

  base <- sprintf(
    paste0("Foi ajustado um %s para descrever %s em função de %s, pelo método ",
           "dos mínimos quadrados não-lineares. A equação estimada foi: *%s*. ",
           "O modelo alcançou um pseudo-R² de %s e erro padrão residual (RSE) de %s."),
    tipo_curva_label(r$tipo), label_y, label_x, equacao_curva(r), r2txt, fmt(r$rse, 4))

  if (r$tipo == "potencia") {
    b <- unname(p["b"])
    alom <- if (abs(b - 3) <= 0.1) "isometria (b ≈ 3), com crescimento aproximadamente proporcional"
            else if (b < 3) "alometria negativa (b < 3): o corpo tende a alongar mais do que engordar"
            else "alometria positiva (b > 3): o corpo tende a engordar mais do que alongar"
    extra <- sprintf(" O expoente estimado foi *b* = %s, caracterizando %s.", fmt(b, 4), alom)
  } else if (r$tipo == "von_bertalanffy") {
    extra <- sprintf(paste0(" O comprimento assintótico estimado foi L∞ = %s e a taxa de ",
                            "crescimento *k* = %s por unidade de idade (t₀ = %s)."),
                     fmt(unname(p["Linf"]), 2), fmt(unname(p["k"]), 4), fmt(unname(p["t0"]), 4))
  } else if (r$tipo == "logistico") {
    extra <- sprintf(paste0(" O valor assintótico estimado foi L∞ = %s, com taxa *k* = %s ",
                            "e ponto de inflexão em t = %s."),
                     fmt(unname(p["Linf"]), 2), fmt(unname(p["k"]), 4), fmt(unname(p["tm"]), 4))
  } else if (r$tipo == "exponencial") {
    extra <- sprintf(" O coeficiente de crescimento estimado foi *b* = %s (com intercepto *a* = %s).",
                     fmt(unname(p["b"]), 4), fmt(unname(p["a"]), 4))
  } else if (r$tipo == "polinomial") {
    b1 <- unname(p["b1"])
    b2 <- unname(p["b2"])
    x_otimo <- -b1 / (2 * b2)
    otimo_txt <- if (b2 < 0) "ponto de máximo" else "ponto de mínimo"
    extra <- sprintf(" O modelo quadrático apresentou os coeficientes b1 = %s e b2 = %s, indicando um %s em X = %s.",
                     fmt(b1, 4), fmt(b2, 4), otimo_txt, fmt(x_otimo, 4))
  } else if (r$tipo == "logaritmica") {
    extra <- sprintf(" O coeficiente associado ao logaritmo foi *b* = %s (com intercepto *a* = %s).",
                     fmt(unname(p["b"]), 4), fmt(unname(p["a"]), 4))
  } else {
    extra <- ""
  }
  paste0(base, extra)
}

# ---- Alias de compatibilidade para pressupostos ----------------------------
mostrar_pressupostos_curva <- mostrar_normalidade_curva

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

