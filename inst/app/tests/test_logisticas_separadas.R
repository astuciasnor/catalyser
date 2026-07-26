# Teste focal: regressão logística binária e curva logística não linear
# devem permanecer como análises distintas.

source("app.R", encoding = "UTF-8")

html_binaria <- htmltools::renderTags(
  mod_regression_ui("teste_logistica_binaria", is_logistic = TRUE)
)$html

stopifnot(
  grepl("Configuração da Regressão Logística", html_binaria, fixed = TRUE),
  grepl("Curva de Probabilidade", html_binaria, fixed = TRUE),
  grepl("Resíduos de Deviance", html_binaria, fixed = TRUE),
  grepl("Influência (Cook)", html_binaria, fixed = TRUE),
  grepl("Ver Código R", html_binaria, fixed = TRUE),
  !grepl("Normalidade (Q-Q Plot)", html_binaria, fixed = TRUE)
)

html_curva <- htmltools::renderTags(
  mod_nonlinear_ui("teste_curva_logistica", "logistico")
)$html

stopifnot(
  grepl("Modelo Logístico Sigmoidal", html_curva, fixed = TRUE),
  !grepl("Base utilizada", html_curva, fixed = TRUE)
)

codigo_app <- paste(readLines("app.R", encoding = "UTF-8"), collapse = "\n")
stopifnot(
  grepl('title = "Regressão Logística Binária"', codigo_app, fixed = TRUE),
  grepl('mod_regression_ui("logistic_regression", is_logistic = TRUE)', codigo_app, fixed = TRUE),
  grepl('title = "Curva Logística"', codigo_app, fixed = TRUE),
  grepl('mod_nonlinear_ui("logistico", "logistico")', codigo_app, fixed = TRUE),
  grepl('mod_nonlinear_server("logistico", dados_analise, import_info, "logistico")',
        codigo_app, fixed = TRUE)
)

codigo_regressao <- paste(
  readLines("modules/mod_regression.R", encoding = "UTF-8"),
  collapse = "\n"
)
stopifnot(
  grepl("personalizar_ajuste <- identical(input$active_tab, aba_ajuste)",
        codigo_regressao, fixed = TRUE),
  grepl("y_label <- if (personalizar_ajuste", codigo_regressao, fixed = TRUE),
  grepl("l50_na_faixa <- is.finite(x50)", codigo_regressao, fixed = TRUE),
  grepl("data.frame(x50 = if (l50_na_faixa) x50 else numeric(0))",
        codigo_regressao, fixed = TRUE),
  grepl("caption = aviso_l50", codigo_regressao, fixed = TRUE),
  !grepl(
    "geom_vline(xintercept = -coefs[1]/coefs[2]",
    codigo_regressao,
    fixed = TRUE
  )
)

# Exercita diretamente o motor canônico da curva logística não linear.
motor <- new.env(parent = globalenv())
source("templates/funcoes_crescimento.R", local = motor, encoding = "UTF-8")

x <- seq(0, 12, length.out = 80)
y <- 42 / (1 + exp(-0.75 * (x - 5.5))) + 0.15 * sin(x)
dados_curva <- data.frame(tempo = x, comprimento_cm = y)

ajuste_curva <- motor$ajustar_curva(
  dados_curva,
  var_y = "comprimento_cm",
  var_x = "tempo",
  tipo = "logistico"
)

stopifnot(
  identical(ajuste_curva$tipo, "logistico"),
  inherits(ajuste_curva$modelo, "nls"),
  all(c("Linf", "k", "tm") %in% names(ajuste_curva$params)),
  is.finite(ajuste_curva$pseudo_r2),
  ajuste_curva$pseudo_r2 > 0.99
)

cat("OK: regressão logística binária e curva logística não linear permanecem separadas\n")
