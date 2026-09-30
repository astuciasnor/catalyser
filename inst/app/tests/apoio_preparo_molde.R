# Apoio dos testes de preparo. Executa o código exportado até a adoção da base,
# antes da análise estatística, que possui testes próprios com amostras adequadas.
executar_preparo_molde <- function(projeto) {
  script <- file.path(projeto, 'R/analise.R')
  expressoes <- parse(script, encoding = 'UTF-8')
  inicio_analise <- which(vapply(expressoes, function(x) {
    is.call(x) && identical(x[[1]], as.name('<-')) &&
      identical(x[[2]], as.name('dados_preparados'))
  }, logical(1)))
  stopifnot(length(inicio_analise) == 1L, inicio_analise > 1L)
  # Os dois QMDs devem executar justamente esse script, e não outra receita.
  for (nome in c('relatorio_completo.qmd', 'relatorio_artigo.qmd')) {
    qmd <- readLines(file.path(projeto, 'relatorios', nome), encoding = 'UTF-8')
    stopifnot(any(grepl('source(here::here("R", "analise.R"), encoding = "UTF-8")', qmd, fixed = TRUE)))
  }
  anterior <- getwd()
  on.exit(setwd(anterior), add = TRUE)
  setwd(projeto)
  ambiente <- new.env(parent = globalenv())
  eval(expressoes[seq_len(inicio_analise - 1L)], envir = ambiente)
  ambiente
}
