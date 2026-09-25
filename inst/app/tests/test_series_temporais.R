# Teste focal do percurso exploratório de Séries Temporais.
# Execute a partir de inst/app: Rscript tests/test_series_temporais.R

Sys.setlocale("LC_CTYPE", "pt_BR.UTF-8")
source("app.R", encoding = "UTF-8")

# Uma série mensal regular deve alimentar os três objetos do R base.
datas <- seq.Date(as.Date("2023-01-01"), by = "month", length.out = 36)
dados <- data.frame(
  data = datas,
  cpue = 20 + sin(seq_along(datas) * 2 * pi / 12) * 4 + seq_along(datas) / 10
)
serie <- .serie_preparar(dados, "data", "cpue", "mensal")

stopifnot(
  stats::frequency(serie$ts) == 12,
  length(serie$ts) == 36,
  inherits(stats::decompose(serie$ts), "decomposed.ts"),
  inherits(.serie_autoplot(serie$ts), "ggplot"),
  inherits(.serie_autoplot(stats::acf(serie$ts, plot = FALSE)), "ggplot")
)

# Cada opção do menu declara seu retrato exploratório e o limite da versão 1.
html_visualizar <- htmltools::renderTags(mod_series_temporais_ui("serie_visualizar", "visualizar"))$html
html_decompor <- htmltools::renderTags(mod_series_temporais_ui("serie_decompor", "decompor"))$html
html_autocorrelacao <- htmltools::renderTags(mod_series_temporais_ui("serie_autocorrelacao", "autocorrelacao"))$html
stopifnot(
  grepl("Visualizar e suavizar", html_visualizar, fixed = TRUE),
  grepl("Decomposição", html_decompor, fixed = TRUE),
  grepl("Autocorrelação", html_autocorrelacao, fixed = TRUE),
  grepl("ARIMA ficará para a v2", html_visualizar, fixed = TRUE)
)

# A série deixa a família de regressão e torna-se menu próprio antes da multivariada.
codigo_app <- paste(readLines("app.R", encoding = "UTF-8"), collapse = "\n")
pos_series <- regexpr('title = HTML("Séries<br>Temporais")', codigo_app, fixed = TRUE)[[1]]
pos_multivariada <- regexpr('title = HTML("Estatística<br>Multivariada")', codigo_app, fixed = TRUE)[[1]]
pos_regressao_n_linear <- regexpr('title = HTML("Regressão<br>Não Linear")', codigo_app, fixed = TRUE)[[1]]
stopifnot(pos_regressao_n_linear > 0, pos_series > pos_regressao_n_linear, pos_multivariada > pos_series)

# O menu superior oferece os três caminhos, e não uma única entrada genérica.
stopifnot(
  grepl('title = "Visualizar e suavizar"', codigo_app, fixed = TRUE),
  grepl('title = "Decomposição"', codigo_app, fixed = TRUE),
  grepl('title = "Autocorrelação"', codigo_app, fixed = TRUE)
)

cat("OK: menu próprio, cálculo base e gráficos ggfortify validados.\n")
