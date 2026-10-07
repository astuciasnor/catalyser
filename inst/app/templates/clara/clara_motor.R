# =============================================================================
#  ClaRa: o motor
# =============================================================================
#
#  A ajuda() e as peças internas que todas as funções usam: conferir as
#  colunas, preencher a receita, mostrá-la e rodá-la, as paletas de cores
#  e a escrita dos números. O aluno não precisa abrir este arquivo.
#
#  Carregado por R/clara.R; no roteiro, basta source("R/clara.R").
# =============================================================================


# Ajuda ------------------------------------------------------------------------

# ajuda() ----------------------------------------------------------------------
#
# Pergunta: o que faz uma função da ClaRa e como usá-la?
#
# Mostra o cabeçalho da função, o mesmo que está escrito acima dela no
# arquivo da ClaRa onde ela mora: a pergunta, o que faz, os argumentos, o
# que devolve e um exemplo.
# Sem nome de função, lista todas as funções da ClaRa. No RStudio, a ajuda
# abre como página no painel Viewer; fora dele, aparece no console. (O
# painel Help do RStudio só mostra ajuda de pacotes, e a ClaRa ainda não é
# um pacote.)
#
# Argumentos:
#   funcao .............. o nome da função, com ou sem aspas
#   onde ................ "viewer" (padrão: painel Viewer do RStudio) ou
#                         "console"
#
# Exemplo:
#   ajuda()
#   ajuda(comparar_medias)
#   ajuda(grafico_medias, onde = "console")
#
ajuda <- function(funcao,
                  onde = c("viewer", "console")) {

  # Onde mostrar: no painel Viewer do RStudio (padrão) ou no console.
  onde <- escolher_opcao(onde, c("viewer", "console"), "onde")

  # Lemos os arquivos da ClaRa, onde cada função tem seu cabeçalho de ajuda.
  cabecalhos <- ler_cabecalhos(arquivos_clara)
  funcoes    <- cabecalhos$funcao

  # Sem nome de função, mostramos a lista de todas, agrupadas pela seção
  # onde moram, com a pergunta de cada uma.
  if (missing(funcao)) {
    mostrar_ajuda("Funções da ClaRa", listar_funcoes(cabecalhos), onde)
    return(invisible(funcoes))
  }

  # O nome que o aluno pediu, escrito com ou sem aspas.
  nome <- rlang::as_name(rlang::ensym(funcao))

  # Se não for uma função da ClaRa, dizemos quais existem.
  if (!is.element(nome, funcoes)) {
    stop("Não há ajuda da ClaRa para ", nome, ". As funções da ClaRa são: ",
         toString(funcoes), ".", call. = FALSE)
  }

  # O cabeçalho é o bloco de comentários logo abaixo da linha de título.
  texto <- cabecalhos$texto[[match(nome, funcoes)]]

  # Mostramos o título e o cabeçalho, sem os # do começo das linhas.
  mostrar_ajuda(paste0(nome, "()"), texto, onde)

  invisible(nome)
}

# Mostra uma ajuda: no RStudio, como página no painel Viewer; fora dele (ou
# com onde = "console"), como texto no console.
mostrar_ajuda <- function(titulo, linhas, onde) {
  visualizador <- getOption("viewer")
  if (onde == "viewer" && is.function(visualizador)) {
    pagina <- tempfile("ajuda_clara_", fileext = ".html")
    writeLines(enc2utf8(pagina_de_ajuda(titulo, linhas)), pagina, useBytes = TRUE)
    visualizador(pagina)
    cat("A ajuda de ", titulo, " abriu no painel Viewer. ",
        "Para vê-la aqui: ajuda(..., onde = \"console\").\n", sep = "")
  } else {
    cat("\n", titulo, "\n", strrep("=", nchar(titulo)), "\n", sep = "")
    cat(linhas, sep = "\n")
    cat("\n")
  }
}

# Monta a página HTML da ajuda, com as cores da ClaRa. O texto vai num bloco
# de largura fixa, para manter alinhadas as listas de argumentos.
pagina_de_ajuda <- function(titulo, linhas) {
  escapar <- function(x) gsub(">", "&gt;", gsub("<", "&lt;", gsub("&", "&amp;", x, fixed = TRUE), fixed = TRUE), fixed = TRUE)
  corpo <- escapar(linhas)
  # Rótulos de bloco (Pergunta:, Argumentos:, Exemplo: ...) em negrito.
  corpo <- sub("^([A-ZÁÉÍÓÚ][A-Za-zçãõáéíóú ]*:)", "<b>\\1</b>", corpo)
  c("<!DOCTYPE html>",
    "<html lang=\"pt-BR\"><head><meta charset=\"utf-8\">",
    paste0("<title>", escapar(titulo), "</title>"),
    "<style>",
    "body { font-family: Arial, sans-serif; margin: 16px; color: #1d2b36; }",
    "h1 { color: #0F3B5F; font-size: 20px; border-bottom: 2px solid #62B6B7; padding-bottom: 4px; }",
    "pre { font-family: Consolas, 'Courier New', monospace; font-size: 13px; line-height: 1.45; white-space: pre-wrap; }",
    "b { color: #2E7D8F; }",
    "</style></head><body>",
    paste0("<h1>", escapar(titulo), "</h1>"),
    "<pre>", corpo, "</pre>",
    "</body></html>")
}

# Lê os cabeçalhos de ajuda de todos os arquivos da ClaRa, na ordem deles.
# Devolve, para cada função do aluno, o nome, a seção onde ela mora e o
# texto do cabeçalho.
ler_cabecalhos <- function(arquivos) {
  partes <- lapply(arquivos, function(arquivo) {
    linhas <- readLines(arquivo, encoding = "UTF-8")

    # Cada cabeçalho começa com uma linha como "# comparar_medias() ------".
    inicios <- grep("^# [a-z_]+\\(\\) -+$", linhas)

    # A seção de cada função é o último título "# Nome da seção ----" antes
    # dela: começa com maiúscula e não tem parênteses.
    titulos <- grep("^# [A-Z][^(]* -{4,}$", linhas)
    secoes  <- vapply(inicios, function(inicio) {
      sub("^# (.+?) -{4,}$", "\\1", linhas[max(titulos[titulos < inicio])])
    }, character(1))

    list(funcao = sub("^# ([a-z_]+)\\(\\) -+$", "\\1", linhas[inicios]),
         secao  = secoes,
         texto  = lapply(inicios, function(inicio) bloco_de_comentarios(linhas, inicio)))
  })
  list(funcao = unlist(lapply(partes, function(parte) parte$funcao)),
       secao  = unlist(lapply(partes, function(parte) parte$secao)),
       texto  = unlist(lapply(partes, function(parte) parte$texto), recursive = FALSE))
}

# Lista as funções da ClaRa, cada uma com a pergunta que responde, agrupadas
# pela seção onde moram (como "Comparar médias"). Devolve as linhas do
# texto, para mostrar_ajuda() exibir.
listar_funcoes <- function(cabecalhos) {
  perguntas <- vapply(cabecalhos$texto, function(texto) {
    sub("^Pergunta: ", "", grep("^Pergunta: ", texto, value = TRUE)[1])
  }, character(1))
  secoes <- cabecalhos$secao

  nomes   <- paste0(cabecalhos$funcao, "()")
  largura <- max(nchar(nomes))

  # Um bloco por seção: o nome da seção e as suas funções, alinhadas.
  blocos <- lapply(unique(secoes), function(secao) {
    escolhidas <- secoes == secao
    c("", paste0(secao, ":"),
      paste0("  ", formatC(nomes[escolhidas], width = -largura), "  ",
             perguntas[escolhidas]))
  })
  c(unlist(blocos), "", "Para saber mais sobre uma delas: ajuda(comparar_medias)")
}

# Devolve o bloco de comentários que vem logo depois da linha de título,
# sem o "# " do começo e sem linhas vazias nas pontas.
bloco_de_comentarios <- function(linhas, inicio) {
  depois  <- linhas[(inicio + 1):length(linhas)]
  tamanho <- match(FALSE, startsWith(depois, "#")) - 1
  texto   <- sub("^# ?", "", depois[seq_len(tamanho)])
  preenchidas <- which(nzchar(texto))
  texto[min(preenchidas):max(preenchidas)]
}


# Motor das receitas -----------------------------------------------------------
#
# Peças internas usadas pelas funções acima: conferir colunas, descobrir
# nomes, preencher a receita, mostrá-la e rodá-la.

# Confere, antes da análise, se as colunas existem e servem para comparar.
conferir_colunas <- function(dados, coluna_resposta, coluna_grupos) {
  colunas_que_faltam <- setdiff(c(coluna_resposta, coluna_grupos), names(dados))
  nomes_com_simbolos <- make.names(c(coluna_resposta, coluna_grupos)) !=
    c(coluna_resposta, coluna_grupos)
  grupos_com_resposta <- as.character(
    dados[[coluna_grupos]][!is.na(dados[[coluna_resposta]])]
  )

  problema <- dplyr::case_when(
    length(colunas_que_faltam) > 0 ~
      paste("Coluna não encontrada nos dados:", toString(colunas_que_faltam)),
    any(nomes_com_simbolos) ~
      "Use nomes de coluna sem espaços nem símbolos, como peso_g. Renomeie com rename().",
    !is.numeric(dados[[coluna_resposta]]) ~
      "A resposta precisa ser numérica. Confira o tipo da coluna no preparo.",
    any(grepl("-", dados[[coluna_grupos]], fixed = TRUE)) ~
      "Há hífen no nome de um grupo; ele atrapalha as letras das comparações. Renomeie os grupos.",
    dplyr::n_distinct(dados[[coluna_grupos]], na.rm = TRUE) < 2 ~
      "A comparação precisa de pelo menos dois grupos com dados.",
    any(table(grupos_com_resposta) < 3) ~
      "Cada grupo precisa de pelo menos 3 observações com resposta.",
    .default = "nenhum"
  )

  if (problema != "nenhum") stop(problema, call. = FALSE)
}

# Confere se o resultado veio da análise que a função sabe desenhar.
conferir_resultado <- function(resultado, classe, mensagem) {
  if (!inherits(resultado, classe)) stop(mensagem, call. = FALSE)
}

# Escolhe, numa lista de receitas, a da análise que o resultado guarda. Sem
# receita para ela, explica ao aluno, em vez de deixar o R falhar adiante.
escolher_receita <- function(receitas, resultado, nome_funcao) {
  receita <- receitas[[ou_entao(resultado$nomes$analise, "")]]
  if (is.null(receita)) {
    stop(nome_funcao, "() não se aplica a este resultado. ",
         dplyr::case_when(
           is.element(nome_funcao, c("grafico_residuos", "grafico_qq")) ~
             paste("Os resíduos conferem os pressupostos das médias (ANOVA e teste t);",
                   "para olhar as observações de cada grupo, use grafico_boxplot()."),
           .default = "Ela serve aos resultados de comparar_medias() e comparar_medianas()."
         ), call. = FALSE)
  }
  receita
}

# Confere se um argumento de sim ou não recebeu TRUE ou FALSE.
conferir_sim_ou_nao <- function(valor, nome_argumento) {
  if (!isTRUE(valor) && !isFALSE(valor)) {
    stop(nome_argumento, " aceita só TRUE ou FALSE.", call. = FALSE)
  }
}

# Confere se um rótulo recebeu um texto só (ou nada, para usar o nome da coluna).
conferir_rotulo <- function(valor, nome_argumento) {
  if (!is.null(valor) && (!is.character(valor) || length(valor) != 1)) {
    stop(nome_argumento, " aceita um texto entre aspas, como \"Peso (g)\".",
         call. = FALSE)
  }
}

# Devolve a opção escolhida num argumento de escolha. O padrão do argumento
# lista todas as opções (assim o Tab do RStudio as mostra); se o aluno não
# escolheu, vale a primeira. Uma opção que não existe vira mensagem amigável.
escolher_opcao <- function(valor, opcoes, nome_argumento) {
  if (identical(valor, opcoes)) return(opcoes[1])
  if (!is.character(valor) || length(valor) != 1 || !is.element(valor, opcoes)) {
    stop(nome_argumento, " aceita só: ",
         paste0("\"", opcoes, "\"", collapse = " ou "), ".", call. = FALSE)
  }
  valor
}

# Confere se um argumento recebeu uma fração: um número maior que 0 e até 1.
conferir_fracao <- function(valor, nome_argumento) {
  if (!is.numeric(valor) || length(valor) != 1 || valor <= 0 || valor > 1) {
    stop(nome_argumento, " aceita um número maior que 0 e até 1, como 0.3 ou 0.6.",
         call. = FALSE)
  }
}

# Transforma a escolha de cores num vetor: o nome de uma paleta da ClaRa
# ("ocean", "cinza") ou um vetor de cores que o R reconheça.
escolher_cores <- function(cores) {
  e_paleta  <- is.character(cores) && length(cores) == 1 &&
    is.element(cores, names(paletas_clara))
  sao_cores <- is.character(cores) &&
    !inherits(try(grDevices::col2rgb(cores), silent = TRUE), "try-error")
  if (!e_paleta && !sao_cores) {
    stop("cores aceita \"ocean\", \"cinza\" ou um vetor de cores, ",
         "como c(\"darkgreen\", \"orange\").", call. = FALSE)
  }
  if (e_paleta) paletas_clara[[cores]] else cores
}

# Descobre o nome do objeto que o aluno passou (plantas, resultado...).
# Se ele passou uma expressão longa, usamos um nome padrão.
nome_do_objeto <- function(expressao, padrao) {
  if (is.symbol(expressao)) deparse(expressao) else padrao
}

# Junta o que o aluno pediu para salvar, em salvar_tabelas() e
# salvar_figuras(). Cada item leva o nome do seu arquivo: o nome escrito
# (anova = resultado$anova) ou, sem ele, o nome do próprio objeto (base).
# Devolve os nomes e o código de cada item como o aluno o escreveu; o
# conferir() diz se o valor é do tipo certo (tabela ou gráfico).
itens_para_salvar <- function(itens, o_que, exemplo, conferir) {
  if (!length(itens)) {
    stop("Diga o que salvar, com o nome do arquivo antes do =, por exemplo: ",
         exemplo, ".", call. = FALSE)
  }
  nomes   <- rlang::names2(itens)
  codigos <- vapply(itens, rlang::quo_text, character(1), USE.NAMES = FALSE)
  for (i in seq_along(itens)) {
    if (nzchar(nomes[i])) next
    expressao <- rlang::quo_get_expr(itens[[i]])
    if (!is.symbol(expressao)) {
      stop("Dê um nome a ", codigos[i], ": ele vira o nome do arquivo, ",
           "como em nome = ", codigos[i], ".", call. = FALSE)
    }
    nomes[i] <- as.character(expressao)
  }
  ruins <- nomes[!grepl("^[A-Za-z0-9_-]+$", nomes)]
  if (length(ruins)) {
    stop("Os nomes viram nomes de arquivo: use só letras sem acento, números ",
         "e _. Troque: ", toString(ruins), ".", call. = FALSE)
  }
  if (anyDuplicated(nomes)) {
    stop("Dois itens com o mesmo nome: ", toString(unique(nomes[duplicated(nomes)])),
         ". Cada arquivo precisa de um nome só dele.", call. = FALSE)
  }
  for (i in seq_along(itens)) {
    if (!conferir(rlang::eval_tidy(itens[[i]]))) {
      stop(codigos[i], " não é ", o_que, ".", call. = FALSE)
    }
  }
  list(nomes = nomes, codigos = codigos)
}

# Confere se a pasta recebeu um caminho só, entre aspas ou com here().
conferir_pasta <- function(pasta) {
  if (!is.character(pasta) || length(pasta) != 1 || !nzchar(pasta)) {
    stop("pasta aceita um caminho só, como here(\"saida\", \"tabelas\").",
         call. = FALSE)
  }
}

# Troca cada marcador <<NOME>> da receita pelo valor correspondente. Um
# valor vazio (NULL) fica de fora: a análise não usa aquele marcador.
preencher_receita <- function(receita, valores) {
  for (marcador in names(Filter(Negate(is.null), valores))) {
    receita <- gsub(paste0("<<", marcador, ">>"), trimws(valores[[marcador]]),
                    receita, fixed = TRUE)
  }
  trimws(receita)
}

# Imprime a receita com uma moldura, para o aluno copiar para o script.
mostrar_receita <- function(receita) {
  cat("\n# ---- Código R por trás da ClaRa: copie, cole e rode ----------\n\n")
  cat(receita, "\n")
  cat("\n# --------------------------------------------------------------\n\n")
}

# Roda a receita num espaço próprio, onde os objetos têm os nomes do aluno.
# Assim, o código mostrado e o código executado são o mesmo texto. Quando a
# receita repete o código que o aluno escreveu (resultado$resumo), ela roda
# a partir do lugar de onde a função foi chamada, para achar os objetos dele.
executar_receita <- function(receita, objetos, pacotes, mostrar_codigo,
                             ambiente_do_aluno = globalenv()) {
  for (pacote in pacotes) {
    suppressPackageStartupMessages(library(pacote, character.only = TRUE))
  }
  if (mostrar_codigo) mostrar_receita(receita)
  ambiente <- list2env(objetos, parent = ambiente_do_aluno)
  eval(parse(text = receita, keep.source = FALSE), envir = ambiente)
}

# Devolve o valor informado ou, se ele estiver vazio (NULL), a alternativa.
ou_entao <- function(valor, alternativa) {
  if (is.null(valor)) alternativa else valor
}

# Escreve um número com vírgula decimal, como no português.
numero <- function(x, casas = 2) {
  formatC(x, format = "f", digits = casas, decimal.mark = ",")
}

# Escreve o p-valor como no texto científico; um p muito pequeno nunca vira zero.
escrever_p <- function(p) {
  dplyr::case_when(
    p < 0.001 ~ "p < 0,001",
    .default  = paste("p =", numero(p, 3))
  )
}


# Cores dos gráficos -----------------------------------------------------------

# As paletas que o aluno escolhe pelo nome: a Ocean Gradient (oito cores) e
# uma em cinza, para revistas impressas em preto e branco.
paletas_clara <- list(
  ocean = c("#0F3B5F", "#2E7D8F", "#62B6B7", "#E89B3C",
            "#E76F51", "#6A4C93", "#1B998B", "#B23A48"),
  cinza = c("grey25", "grey70")
)

# Escreve um vetor de cores como código R, quatro por linha.
escrever_vetor <- function(valores) {
  textos <- paste0("\"", valores, "\"")
  linhas <- split(textos, ceiling(seq_along(textos) / 4))
  paste0("c(", paste(vapply(linhas, paste, character(1), collapse = ", "),
                     collapse = ",\n           "), ")")
}

# O trecho de cores que abre as receitas de gráfico. Com mais grupos que
# cores, colorRampPalette() cria cores intermediárias. Fica dentro da receita
# para o código mostrado rodar sozinho, sem a ClaRa.
trecho_de_cores <- function(vetor) {
  paste0("
# Cores: uma para cada grupo (intermediárias, se houver mais grupos que cores).
cores <- ", escrever_vetor(vetor), "
cores <- colorRampPalette(cores)(max(length(cores), nlevels(<<RESULTADO>>$dados$<<GRUPOS>>)))")
}

# O trecho com a paleta Ocean, usado pelos gráficos de diagnóstico.
trecho_cores <- trecho_de_cores(paletas_clara$ocean)
