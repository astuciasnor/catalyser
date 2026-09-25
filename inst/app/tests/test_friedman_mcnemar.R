# Testes focais para os procedimentos pareados incluídos nos menus.
# Execute a partir de inst/app: Rscript tests/test_friedman_mcnemar.R

Sys.setlocale("LC_CTYPE", "Portuguese_Brazil.utf8")
source("../../R/analises.R", encoding = "UTF-8")

friedman_dados <- data.frame(
  bloco = rep(paste0("tanque_", 1:6), each = 3),
  condicao = rep(c("controle", "dose_1", "dose_2"), 6),
  crescimento = c(4, 6, 9, 5, 8, 11, 3, 7, 10, 6, 9, 12, 5, 8, 13, 4, 7, 11)
)
friedman <- catalyser_friedman(
  friedman_dados,
  list(resposta = "crescimento", condicao = "condicao", bloco = "bloco",
       posteste = "holm", nivel_confianca = 0.95)
)
stopifnot(
  is.finite(friedman$tabela[["Qui-quadrado de Friedman"]]),
  nrow(friedman$pressupostos) == 4L,
  !is.null(friedman$comparacoes)
)

incompleto <- try(
  catalyser_friedman(
    friedman_dados[-1, ],
    list(resposta = "crescimento", condicao = "condicao", bloco = "bloco")
  ),
  silent = TRUE
)
stopifnot(inherits(incompleto, "try-error"))

mcnemar_dados <- data.frame(
  antes = c("não", "não", "sim", "sim", "não", "sim", "não", "sim"),
  depois = c("sim", "não", "sim", "não", "sim", "sim", "sim", "não")
)
mcnemar <- catalyser_mcnemar(
  mcnemar_dados,
  list(variavel_1 = "antes", variavel_2 = "depois", correcao = TRUE)
)
stopifnot(is.finite(mcnemar$objeto$p.value), nrow(mcnemar$tabela) == 2L)

codigo_app <- paste(readLines("app.R", encoding = "UTF-8"), collapse = "\n")
stopifnot(
  grepl("Friedman (k grupos pareados)", codigo_app, fixed = TRUE),
  grepl("McNemar (pares binários)", codigo_app, fixed = TRUE),
  inherits(catalyser_executar(
    list(tipo = "friedman", parametros = list(
      resposta = "crescimento", condicao = "condicao", bloco = "bloco",
      posteste = "holm", nivel_confianca = 0.95
    )), friedman_dados
  ), "resultado_catalyser")
)

cat("OK: Friedman, pós-teste, trava de balanceamento e McNemar validados.\n")
