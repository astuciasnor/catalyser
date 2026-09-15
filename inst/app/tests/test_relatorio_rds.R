# Conferência dos projetos gerados por gerar_projetos_validacao.R.
# Uso: Rscript tests/test_relatorio_rds.R <pasta-dos-projetos>
raiz <- commandArgs(trailingOnly = TRUE)[1]
stopifnot(!is.na(raiz))
grDevices::pdf(tempfile(fileext = ".pdf"))
for (nome in c("pesca_anova", "pesca_varias")) {
  projeto <- normalizePath(file.path(raiz, nome), winslash = "/")
  ambiente <- new.env(parent = globalenv())
  sys.source(file.path(projeto, "R/funcoes.R"), ambiente)
  qmd <- readLines(file.path(projeto, "relatorios/relatorio.qmd"), encoding = "UTF-8")
  chunks <- ambiente$chunks_do_relatorio(qmd)
  stopifnot(!any(grepl("read_excel|catalyser_conferir_base|# fonte: tratar", qmd)))
  for (ch in chunks) {
    if (ch$rotulo %in% c("codigo-do-script", "atualizar", "instalar")) next
    codigo <- qmd[ch$ini:ch$fim]
    if (any(grepl("#| eval: false", codigo, fixed = TRUE))) next
    ambiente$here <- function(...) file.path(projeto, ...)
    if (identical(ch$rotulo, "pacotes")) {
      # here() é fixado à cópia em teste, sem depender do diretório da suíte.
      codigo <- codigo[!grepl("^library\\(here\\)", codigo)]
    }
    valor <- eval(parse(text = codigo), ambiente)
    if (identical(ch$rotulo, "fig-barras")) {
      ggplot2::ggsave(file.path(projeto, "conferencia_medias.png"), valor,
        width = 6.5, height = 4, dpi = 160)
      texto <- ggplot2::ggplot_build(valor)$data[[4]]
      stopifnot(all(grepl("±", texto$label, fixed = TRUE)))
    }
  }
  if (nome == "pesca_anova") {
    base <- readRDS(file.path(projeto, "dados/processados/base_0001.rds"))
    esperado <- stats::aov(peso_g ~ factor(especie), data = base)
    stopifnot(isTRUE(all.equal(unname(coef(ambiente$modelo)), unname(coef(esperado)))))
    stopifnot(length(ambiente$letras) == 3L)
  }
  cat("CONFERIDO: todos os chunks executáveis via RDS em", nome, "\n")
}
grDevices::dev.off()
