# Depois de gerar o HTML, o Quarto atualiza também a versão Word.
# Esta etapa cuida apenas da saída; a análise continua no relatorio.qmd.
saidas <- strsplit(Sys.getenv("QUARTO_PROJECT_OUTPUT_FILES"), "\n", fixed = TRUE)[[1]]
gerou_html <- any(grepl("\\.html$", saidas, ignore.case = TRUE))
gerou_word <- any(grepl("\\.docx$", saidas, ignore.case = TRUE))

# Ao renderizar só o Word (ou os dois formatos juntos), não repete a geração.
if (gerou_html && !gerou_word) {
  quarto <- Sys.getenv("QUARTO_CLI", unset = Sys.which("quarto"))
  if (!nzchar(quarto)) stop("Não foi possível localizar o Quarto para gerar o Word.")
  message("Atualizando a versão Word do relatório...")
  status <- system2(quarto, c("render", shQuote("relatorios/relatorio.qmd"), "--to", "docx"))
  if (status != 0L) {
    stop("O Word não foi atualizado. Confira o erro acima e renderize novamente.", call. = FALSE)
  }
}
