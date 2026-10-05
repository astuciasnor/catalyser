# Confere o bootstrap instalado sem iniciar um servidor Shiny.
arquivo_app <- file.path("inst", "app", "app.R")
expressoes <- parse(arquivo_app, encoding = "UTF-8")
bootstrap <- Filter(function(expr) {
  is.call(expr) && identical(expr[[1]], as.name("if")) &&
    grepl("descrevendo_dados.R", paste(deparse(expr), collapse = ""), fixed = TRUE)
}, as.list(expressoes))
stopifnot(length(bootstrap) == 1L)
ramo_instalado <- bootstrap[[1]][[4]]

# A cópia permite simular uma sessão antiga sem alterar o namespace real.
ns_instalado <- asNamespace("catalyser")
ns_simulado <- new.env(parent = emptyenv())
for (nome in ls(ns_instalado, all.names = TRUE)) {
  assign(nome, get(nome, ns_instalado), ns_simulado)
}
ambiente <- new.env(parent = baseenv())
ambiente$asNamespace <- function(...) ns_simulado
ambiente$getFromNamespace <- function(nome, ...) get(nome, ns_simulado)
ambiente$globalenv <- function() ambiente
eval(ramo_instalado, ambiente)
stopifnot(is.function(ambiente$exploracao_tipos))

rm("exploracao_tipos", envir = ns_simulado)
erro <- tryCatch(eval(ramo_instalado, ambiente), error = identity)
stopifnot(
  inherits(erro, "error"),
  grepl("Ctrl+Shift+F10", conditionMessage(erro), fixed = TRUE),
  grepl("exploracao_tipos", conditionMessage(erro), fixed = TRUE)
)
cat("OK: bootstrap instalado e orientação para namespace antigo\n")
