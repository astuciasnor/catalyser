# funcoes.R — funções de apoio do projeto
# -----------------------------------------------------------------------------
# Este arquivo só DEFINE funções; não roda nada. As funções de análise vêm do
# pacote catalyser (digite ?catalyser_anova no console para ver a ajuda). O que
# mora aqui é a ligação entre o script comentado (R/analise.R) e o relatório
# (relatorios/relatorio.qmd), a mesma do EAPACaderno, o projeto-modelo do
# ecossistema EAPA:
#
#   trechos_do_script()   lê o script, trecho a trecho, sem os comentários
#   atualizar_codigo()    copia o código do script para os chunks do relatório
#   conferir_codigo()     para o Render se o relatório estiver atrasado
#
# O código do relatório mora em R/analise.R, junto com os comentários que
# explicam cada passo a quem estuda. O relatório recebe só as linhas de
# código, para ser rodado chunk a chunk no RStudio sem as explicações no meio.
#
# A ligação entre os dois é a primeira linha de cada chunk do relatório:
#
#     # fonte: tratar
#     # fonte: anova-profundidade-m-base, anova-profundidade-m-analise
#
# Ela diz de quais trechos do script (os marcados com "## ---- nome ----") o
# chunk é feito. A regra que sustenta tudo: o código se edita no script,
# nunca no relatório. Depois de editar, roda-se atualizar_codigo().

# trechos_do_script() -----------------------------------------------------
#
# O que faz: lê o script e devolve o código de cada trecho, já sem as linhas
#            de comentário e sem linhas em branco nas pontas.
#
# Argumentos:
#   script - o arquivo com os marcadores (padrão: R/analise.R)
#
# Retorna: uma lista com um vetor de linhas por trecho, nomeada pelo marcador.
#
trechos_do_script <- function(script = here::here("R", "analise.R")) {

  linhas   <- readLines(script, encoding = "UTF-8", warn = FALSE)
  marcador <- "^##\\s*----\\s*(.+?)\\s*----\\s*$"
  inicios  <- grep(marcador, linhas)
  if (!length(inicios)) {
    stop("Nenhum marcador '## ---- nome ----' em ", basename(script), call. = FALSE)
  }

  nomes <- sub(marcador, "\\1", linhas[inicios])
  fins  <- c(inicios[-1] - 1L, length(linhas))   # cada trecho vai até o marcador seguinte

  repetidos <- unique(nomes[duplicated(nomes)])
  if (length(repetidos)) {
    stop("Marcador repetido em ", basename(script), ": ",
         paste(repetidos, collapse = ", "), call. = FALSE)
  }

  # Saem as LINHAS de comentário (as que começam com #). Um comentário no fim
  # de uma linha de código, como em library(readxl)  # planilhas, fica.
  trechos <- Map(function(a, b) {
    codigo <- if (b > a) linhas[(a + 1L):b] else character()
    codigo <- codigo[!grepl("^\\s*#", codigo)]                    # fora as linhas de comentário
    vazias <- !nzchar(trimws(codigo))
    codigo[cumsum(!vazias) > 0 & rev(cumsum(rev(!vazias))) > 0]   # fora as pontas em branco
  }, inicios, fins)

  stats::setNames(trechos, nomes)
}

# corpo_do_chunk() --------------------------------------------------------
#
# O que faz: monta o corpo de um chunk a partir da sua linha "# fonte: ...":
#            a própria linha, uma em branco e o código dos trechos, na ordem.
#
corpo_do_chunk <- function(linha_fonte, trechos) {

  nomes  <- trimws(strsplit(sub("^#\\s*fonte:\\s*", "", linha_fonte), ",")[[1]])
  faltam <- setdiff(nomes, names(trechos))
  if (length(faltam)) {
    stop("Trecho(s) sem marcador em R/analise.R: ", paste(faltam, collapse = ", "),
         call. = FALSE)
  }

  codigo <- unlist(lapply(nomes, function(n) c(trechos[[n]], "")), use.names = FALSE)
  c(linha_fonte, "", codigo[-length(codigo)])   # sem a linha em branco final
}

# chunks_do_relatorio() ---------------------------------------------------
#
# O que faz: localiza os chunks R do relatório e devolve, para cada um, o
#            rótulo, a linha "# fonte:" (NA se não tiver) e onde o corpo
#            começa e termina.
#
chunks_do_relatorio <- function(linhas) {

  abre <- grep("^```\\{r\\}\\s*$", linhas)

  lapply(abre, function(a) {
    i <- a + 1L
    rotulo <- NA
    while (i <= length(linhas) && grepl("^#\\|", linhas[i])) {          # as opções
      if (grepl("^#\\|\\s*label:", linhas[i])) {
        rotulo <- trimws(sub("^#\\|\\s*label:", "", linhas[i]))
      }
      i <- i + 1L
    }
    f <- i
    while (f <= length(linhas) && !grepl("^```\\s*$", linhas[f])) f <- f + 1L
    corpo    <- if (f > i) linhas[i:(f - 1L)] else character()
    primeira <- corpo[nzchar(trimws(corpo))][1]
    fonte    <- if (!is.na(primeira) && grepl("^#\\s*fonte:", primeira)) primeira else NA
    list(rotulo = rotulo, ini = i, fim = f - 1L, fecha = f, fonte = fonte)
  })
}

# Os chunks que o script alimenta: os que têm a linha "# fonte:".
chunks_com_fonte <- function(chunks) Filter(function(ch) !is.na(ch$fonte), chunks)

# Os dois chunks de manutenção têm código próprio, de propósito.
chunks_de_manutencao <- c("codigo-do-script", "atualizar")

# atualizar_codigo() ------------------------------------------------------
#
# O que faz: copia para cada chunk do relatório o código dos trechos que a
#            sua linha "# fonte:" indica, sem os comentários. Só reescreve o
#            arquivo se algum chunk mudou.
#
# Argumentos:
#   qmd    - o relatório (padrão: relatorios/relatorio.qmd)
#   script - o script com o código comentado (padrão: R/analise.R)
#
# Exemplo:  atualizar_codigo()
#
atualizar_codigo <- function(qmd    = here::here("relatorios", "relatorio.qmd"),
                             script = here::here("R", "analise.R")) {

  linhas  <- readLines(qmd, encoding = "UTF-8", warn = FALSE)
  trechos <- trechos_do_script(script)
  chunks  <- chunks_com_fonte(chunks_do_relatorio(linhas))
  mudados <- character()

  # De trás para a frente, para que trocar um corpo não desloque os índices
  # dos chunks anteriores.
  for (ch in rev(chunks)) {
    novo  <- corpo_do_chunk(ch$fonte, trechos)
    velho <- linhas[ch$ini:ch$fim]
    if (!identical(velho, novo)) {
      mudados <- c(ch$fonte, mudados)
      linhas  <- c(linhas[seq_len(ch$ini - 1L)], novo, linhas[ch$fecha:length(linhas)])
    }
  }

  if (length(mudados)) {
    writeLines(linhas, qmd, useBytes = TRUE)
    message("Atualizado em ", basename(qmd), ":")
    message(paste0("  ", mudados, collapse = "\n"))
    message("Se o arquivo estiver aberto no RStudio, ele vai recarregar.")
  } else {
    message(basename(qmd), " já está em dia com ", basename(script), ".")
  }

  invisible(mudados)
}

# conferir_codigo() -------------------------------------------------------
#
# O que faz: verifica se o relatório está em dia com o script. Roda no
#            começo do Render, para o relatório nunca sair com código
#            diferente do que está no script. Também avisa se algum chunk
#            R ficou sem a linha "# fonte:", porque esse fica fora da
#            conferência (o aviso vai para o log do Render).
#
conferir_codigo <- function(qmd    = here::here("relatorios", "relatorio.qmd"),
                            script = here::here("R", "analise.R")) {

  linhas  <- readLines(qmd, encoding = "UTF-8", warn = FALSE)
  trechos <- trechos_do_script(script)
  chunks  <- chunks_do_relatorio(linhas)

  soltos <- vapply(Filter(function(ch) is.na(ch$fonte), chunks), `[[`, "", "rotulo")
  soltos <- setdiff(soltos[!is.na(soltos)], chunks_de_manutencao)
  if (length(soltos)) {
    cat("Aviso: chunk(s) sem '# fonte:' ficam fora da conferência com ",
        basename(script), ": ", paste(soltos, collapse = ", "), "\n",
        sep = "", file = stderr())
  }

  atrasados <- Filter(function(ch) {
    !identical(linhas[ch$ini:ch$fim], corpo_do_chunk(ch$fonte, trechos))
  }, chunks_com_fonte(chunks))

  if (length(atrasados)) {
    stop(
      "O código do relatório está diferente de ", basename(script), " em:\n",
      paste0("  ", vapply(atrasados, `[[`, "", "fonte"), collapse = "\n"),
      "\nRode o chunk `atualizar` e renderize de novo.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}
