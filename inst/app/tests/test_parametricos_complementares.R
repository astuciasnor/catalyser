# Testes focais da reorganização do menu e dos três procedimentos novos.
# Execute a partir de inst/app: Rscript tests/test_parametricos_complementares.R

Sys.setlocale("LC_CTYPE", "Portuguese_Brazil.utf8")
source("../../R/analises.R", encoding = "UTF-8")

set.seed(42)
repetidas <- expand.grid(
  unidade = factor(seq_len(12)),
  ocasiao = factor(c("antes", "meio", "depois"), levels = c("antes", "meio", "depois"))
)
repetidas$resposta <- rnorm(nrow(repetidas), rep(c(10, 12, 15), each = 12), 1)
resultado_repetidas <- catalyser_anova_medidas_repetidas(
  repetidas, list(resposta = "resposta", sujeito = "unidade", momento = "ocasiao")
)

uma_variancia <- data.frame(comprimento = rnorm(50, 30, 2))
resultado_qui <- catalyser_variancia_uma(
  uma_variancia,
  list(variavel = "comprimento", desvio_hipotetico = 2,
       alternativa = "two.sided", nivel_confianca = 0.95)
)

duas_variancias <- data.frame(
  resposta = c(rnorm(30, 0, 1), rnorm(28, 0, 2)),
  grupo = factor(rep(c("A", "B"), c(30, 28)))
)
resultado_f <- catalyser_variancias_duas(
  duas_variancias,
  list(resposta = "resposta", grupo = "grupo", alternativa = "two.sided", nivel_confianca = 0.95)
)

stopifnot(
  nrow(resultado_repetidas$tabela) == 2L,
  is.finite(resultado_repetidas$tabela$F[1]),
  is.finite(resultado_qui$tabela[["Qui-quadrado"]]),
  is.finite(resultado_f$tabela$F),
  inherits(catalyser_executar(list(
    tipo = "qui_quadrado_variancia",
    parametros = list(variavel = "comprimento", desvio_hipotetico = 2,
                      alternativa = "two.sided", nivel_confianca = 0.95)
  ), uma_variancia), "resultado_catalyser")
)

codigo_app <- paste(readLines("app.R", encoding = "UTF-8"), collapse = "\n")
ordem <- vapply(c(
  "Teste t de Student", "ANOVA de um fator", "ANOVA de medidas repetidas",
  "ANOVA a dois fatores", "ANCOVA (Análise de Covariância)",
  "Qui-quadrado para variância", "Teste F para duas variâncias"
), function(rotulo) regexpr(rotulo, codigo_app, fixed = TRUE)[1], numeric(1))
stopifnot(all(ordem > 0), all(diff(ordem) > 0),
          grepl("Qui-quadrado de independência", codigo_app, fixed = TRUE))

cat("OK: menu paramétrico agrupado e três testes complementares validados.\n")
