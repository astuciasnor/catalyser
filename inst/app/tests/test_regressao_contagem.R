# Testes focais para os dois modelos de contagem.
# Execute a partir de inst/app: Rscript tests/test_regressao_contagem.R

Sys.setlocale("LC_CTYPE", "pt_BR.UTF-8")
source(file.path("..", "..", "R", "analises.R"), encoding = "UTF-8", local = TRUE)

# Primeiro cenário: contagem compatível com Poisson e exposição conhecida.
set.seed(20260921)
n <- 240
esforco <- runif(n, min = 1, max = 4)
temperatura <- rnorm(n)
media <- esforco * exp(0.4 + 0.35 * temperatura)
dados_poisson <- data.frame(
  individuos = stats::rpois(n, lambda = media),
  temperatura = temperatura,
  esforco_horas = esforco
)

poisson <- catalyser_regressao_contagem(
  dados_poisson,
  list(
    resposta = "individuos", preditores = "temperatura",
    offset = "esforco_horas", usar_offset = TRUE
  ),
  familia = "poisson"
)

stopifnot(
  inherits(poisson$objeto, "glm"),
  poisson$dispersao < 1.5,
  all(c("Razão de taxas", "IC 95% inferior", "IC 95% superior") %in% names(poisson$tabela))
)

# Segundo cenário: a mesma média com variância maior pede a Binomial Negativa.
dados_superdispersos <- transform(
  dados_poisson,
  individuos = MASS::rnegbin(n, mu = media, theta = 0.45)
)

poisson_superdisperso <- catalyser_regressao_contagem(
  dados_superdispersos,
  list(resposta = "individuos", preditores = "temperatura"),
  familia = "poisson"
)

binomial_negativa <- catalyser_regressao_contagem(
  dados_superdispersos,
  list(
    resposta = "individuos", preditores = "temperatura",
    offset = "esforco_horas", usar_offset = TRUE
  ),
  familia = "binomial_negativa"
)

stopifnot(
  poisson_superdisperso$dispersao > 2,
  grepl("Binomial Negativa", poisson_superdisperso$orientacao_dispersao, fixed = TRUE),
  inherits(binomial_negativa$objeto, "negbin"),
  is.finite(binomial_negativa$resumo$theta)
)

# A resposta não pode aceitar medidas contínuas ou contagens negativas.
erro_continuo <- try(
  catalyser_regressao_contagem(
    transform(dados_poisson, individuos = individuos + 0.5),
    list(resposta = "individuos", preditores = "temperatura"),
    familia = "poisson"
  ),
  silent = TRUE
)
stopifnot(inherits(erro_continuo, "try-error"))

# A ordem do menu é uma decisão pedagógica e deve permanecer explícita.
codigo_app <- paste(readLines("app.R", encoding = "UTF-8"), collapse = "\n")
posicoes <- vapply(c(
  "Descobrindo o Modelo", "Regressão Linear Simples", "Regressão Linear Múltipla",
  "Regressão Logística Binária", "Regressão de Poisson",
  "Regressão Binomial Negativa", "Séries Temporais"
), function(titulo) regexpr(titulo, codigo_app, fixed = TRUE)[[1]], integer(1))
stopifnot(all(posicoes > 0), identical(posicoes, sort(posicoes)))

cat("OK: Poisson, Binomial Negativa, superdispersão e ordem do menu validados.\n")
