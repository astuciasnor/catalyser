# Execute na raiz do projeto: Rscript verificar_reprodutibilidade.R
# Cada render começa em outro processo R. Nenhum pacote é instalado aqui.
argumentos <- commandArgs(trailingOnly = TRUE)
if (length(argumentos)) setwd(normalizePath(argumentos[1], mustWork = TRUE))
if (!file.exists('_quarto.yml') || !file.exists('R/analise.R')) {
  stop('Abra a raiz do Projeto R antes de executar esta conferência.', call. = FALSE)
}
# QUARTO_PATH permite indicar uma instalação que não está no PATH.
quarto <- Sys.getenv('QUARTO_PATH', unname(Sys.which('quarto')))
if (!nzchar(quarto) || !file.exists(quarto)) {
  stop('Quarto não encontrado. Instale-o e reabra o terminal, ou defina QUARTO_PATH.', call. = FALSE)
}
dir.create('saida/verificacao', recursive = TRUE, showWarnings = FALSE)
documentos <- c('relatorio_completo', 'relatorio_artigo')
extensoes <- c('html', 'docx')
falhas <- character()
for (i in seq_along(documentos)) {
  documento <- documentos[i]
  log <- file.path('saida/verificacao', paste0(documento, '.log'))
  inicio <- Sys.time()
  # --execute impede a reutilização de resultados sem recalcular o script.
  status <- system2(quarto, c('render', shQuote(file.path('relatorios', paste0(documento, '.qmd'))),
                              '--execute'), stdout = log, stderr = log)
  produto <- file.path('saida/relatorios', paste0(documento, '.', extensoes[i]))
  # Um arquivo de uma execução antiga não comprova sucesso desta execução.
  recente <- file.exists(produto) && file.info(produto)$mtime >= inicio - 2
  ok <- identical(status, 0L) && isTRUE(recente)
  if (ok) {
    texto <- if (extensoes[i] == 'html') readLines(produto, warn = FALSE, encoding = 'UTF-8') else {
      conexao <- unz(produto, 'word/document.xml', open = 'r')
      conteudo <- readLines(conexao, warn = FALSE, encoding = 'UTF-8')
      close(conexao)
      conteudo
    }
    ok <- !any(grepl('??', texto, fixed = TRUE))
  }
  cat(documento, ': exit=', status, '; arquivo novo=', recente, '; aprovado=', ok, '\n', sep = '')
  if (!ok) falhas <- c(falhas, documento)
}
if (length(falhas)) {
  stop(paste('Conferência incompleta:', paste(falhas, collapse = ', '),
             '. Consulte os logs em saida/verificacao.'), call. = FALSE)
}
cat('OK: HTML e DOCX recalculados, arquivos novos e sem referências ?? .\n')
