# Executar de inst/app: valida o código gerado, sem alterar projetos baixados.
for (arquivo in c("registro_tratamentos.R", "registro_bases.R",
                  "registro_execucoes.R", "registro_comunicacao.R",
                  "exportacao_comunicacao.R")) {
  source(file.path("modules", arquivo), encoding = "UTF-8")
}

verificar_codigo_didatico <- function() {
  funcoes <- new.env(parent = globalenv())
  sys.source("templates/anova_um_fator/funcoes.R", funcoes)
  extrair <- function(linhas, nome) {
    inicio <- match(paste0("## ---- ", nome, " ----"), linhas)
    fim <- which(grepl("^## ---- ", linhas) & seq_along(linhas) > inicio)[1]
    linhas[seq.int(inicio + 1L, fim - 1L)]
  }
  # Inclui cabeçalhos com espaços na resposta, no fator e em ambos.
  for (nomes in list(c("Taxa", "Densidade"), c("Taxa (%)", "Densidade"),
                     c("Taxa", "Densidade inicial"), c("Taxa (%)", "Densidade inicial"))) {
    dados <- data.frame(y = c(4.9, 4.1, 10.7, 8.4, 5.8, 9.1, 6.8,
                             2.3, 6.8, 5.6, 4.1, 3.2, 7.1, 4.8,
                             2.4, 3.1, 2.8, 1.9, 3.5, 2.3, 2.7),
                        g = rep(c("A", "B", "C"), each = 7))
    names(dados) <- nomes
    manifesto <- list(execucoes = list(list(tipo = "anova_um_fator",
      titulo = "Taxa por densidade", incluir_word = TRUE,
      parametros = list(resposta = nomes[1], fator = nomes[2], nivel_confianca = .95))))
    linhas <- exportacao_modelo_anova("analise.R", manifesto,
      list(source = "local", file_name = "cultivo.csv"), templates_dir = "templates")
    stopifnot(!any(grepl("p_pares|nomes_comparacoes|normal_ok|levene_ok", linhas)))
    ambiente <- new.env(parent = funcoes)
    ambiente$dados <- dados
    ambiente$nivel_confianca <- .95
    for (trecho in c("preparar-analise", "analisar", "analisar-pressupostos", "analisar-tukey")) {
      invisible(capture.output(eval(parse(text = extrair(linhas, trecho)), ambiente)))
    }
    # A versão curta deve reproduzir as letras da antiga matriz explícita.
    grupos <- levels(ambiente$dados[[nomes[2]]])
    pares <- combn(grupos, 2)
    matriz <- matrix(1, length(grupos), length(grupos), dimnames = list(grupos, grupos))
    valores <- ambiente$tukey[[1]][paste(pares[2, ], pares[1, ], sep = "-"), "p adj"]
    matriz[cbind(pares[1, ], pares[2, ])] <- valores
    matriz[cbind(pares[2, ], pares[1, ])] <- valores
    medias <- tapply(ambiente$dados[[nomes[1]]], ambiente$dados[[nomes[2]]], mean)
    ordem <- names(sort(medias, decreasing = TRUE))
    anteriores <- multcompView::multcompLetters(matriz[ordem, ordem])$Letters
    stopifnot(identical(ambiente$letras, anteriores), !anyNA(ambiente$resumo$letra),
      identical(unname(ambiente$resumo$letra),
                unname(anteriores[as.character(ambiente$resumo[[nomes[2]]])])))

    # Hífen produz orientação clara, antes da chamada incompatível de letras.
    levels(ambiente$dados[[nomes[2]]])[1] <- "A-sequeiro"
    erro <- tryCatch({
      invisible(capture.output(eval(parse(text = extrair(linhas, "analisar-tukey")), ambiente)))
      NULL
    }, error = conditionMessage)
    stopifnot(is.character(erro), grepl("sem hífen", erro, fixed = TRUE))
  }

  # Confere decisões nos limites e no caso NA, usando as expressões exportadas.
  expressoes <- parse(text = extrair(linhas, "preparar-resultados-texto"))
  obter <- function(nome) Filter(function(x) is.call(x) && identical(x[[1]], as.name("<-")) &&
    identical(x[[2]], as.name(nome)), as.list(expressoes))[[1]]
  ambiente <- new.env(parent = funcoes)
  ambiente$p_anova_bruto <- c(NA, .0499, .05, .0501)
  ambiente$p_shapiro_bruto <- ambiente$p_levene_bruto <- ambiente$p_anova_bruto
  ambiente$eta_val <- c(NA, .0099, .01, .0599, .06, .1399, .14)
  for (nome in c("frase_anova", "frase_normalidade", "frase_levene", "classe_efeito")) {
    eval(obter(nome), ambiente)
  }
  stopifnot(
    grepl("não forneceu", ambiente$frase_anova[1]),
    startsWith(ambiente$frase_anova[2], "Houve evidência"),
    all(startsWith(ambiente$frase_anova[3:4], "Não houve evidência")),
    grepl("não foi calculado", ambiente$frase_normalidade[1]),
    startsWith(ambiente$frase_normalidade[2], "houve evidência"),
    all(startsWith(ambiente$frase_normalidade[3:4], "não houve evidência")),
    grepl("não forneceu", ambiente$frase_levene[1]),
    startsWith(ambiente$frase_levene[2], "houve evidência"),
    all(startsWith(ambiente$frase_levene[3:4], "não houve evidência")),
    identical(ambiente$classe_efeito,
      c("indeterminado", "muito pequeno", "pequeno", "pequeno", "médio", "médio", "grande")))
  cat("OK: Tukey curto preserva letras, cabeçalhos com espaços funcionam, hífen é explicado e case_when respeita NA e limites.\n")
}

verificar_codigo_didatico()
