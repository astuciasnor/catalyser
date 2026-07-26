args <- commandArgs(trailingOnly = TRUE)

localizar_app <- function() {
  candidatos <- c(".", file.path("inst", "app"), file.path("..", "inst", "app"))
  encontrados <- candidatos[file.exists(file.path(candidatos, "app.R"))]
  if (!length(encontrados)) {
    stop("Execute este script na raiz do repositório ou em inst/app.", call. = FALSE)
  }
  normalizePath(encontrados[[1]], winslash = "/", mustWork = TRUE)
}

app_dir <- localizar_app()
setwd(app_dir)

arquivos_r <- list.files(".", pattern = "[.]R$", recursive = TRUE, full.names = TRUE)
erros_parse <- vapply(
  arquivos_r,
  function(arquivo) tryCatch({
    parse(arquivo)
    ""
  }, error = function(e) conditionMessage(e)),
  character(1)
)
if (any(nzchar(erros_parse))) {
  print(erros_parse[nzchar(erros_parse)])
  stop("Há arquivos R com erro de sintaxe.", call. = FALSE)
}
cat(sprintf("[OK] Sintaxe de %d arquivos R.\n", length(arquivos_r)))

testes <- c(
  "test_bases_derivadas.R",
  "test_menu_preparando_dados.R",
  "test_registro_execucoes.R",
  "test_execucao_explicita.R",
  "test_estados_execucao.R",
  "test_logisticas_separadas.R",
  "test_comunicacao_resultados.R",
  "test_funcoes_projeto_integrado.R",
  "test_exportacao_comunicacao.R"
)

ausentes <- testes[!file.exists(file.path("tests", testes))]
if (length(ausentes)) {
  stop(sprintf("Testes ausentes: %s", paste(ausentes, collapse = ", ")), call. = FALSE)
}

rscript <- file.path(R.home("bin"), "Rscript")
if (.Platform$OS.type == "windows") rscript <- paste0(rscript, ".exe")

for (teste in testes) {
  expressao <- sprintf("source(%s)", dQuote(file.path("tests", teste)))
  status <- system2(rscript, c("-e", shQuote(expressao)))
  if (!identical(status, 0L)) {
    stop(sprintf("Falha em %s (status %s).", teste, status), call. = FALSE)
  }
  cat(sprintf("[OK] %s\n", teste))
}

cat("\nTodos os testes automatizados das Fases 3 passaram.\n")
