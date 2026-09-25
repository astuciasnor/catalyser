# Cálculo, código independente, tipagem e limites das quinze sub-abas.
source("../../R/descrevendo_dados.R", encoding = "UTF-8")
source("../../R/analises.R", encoding = "UTF-8")
load("../../../EAPADados/data/abalone_adultos.rda")
dados <- abalone_adultos
stopifnot(nrow(dados) == 2641L)
nomes_num <- names(dados)[vapply(dados, is.numeric, logical(1))]
base_original <- dados
for (modo in unname(unlist(descricao_catalogo()))) {
  p <- list(analise = modo, variavel = if (modo == "frequencias") "sexo" else nomes_num[1],
            outra = nomes_num[2], grupo = "sexo")
  resultado <- catalyser_descricao(dados, p)
  independente <- new.env(parent = baseenv()); independente$dados <- dados
  eval(parse(text = catalyser_codigo_descricao(p)), independente)
  replay <- catalyser_executar(list(tipo = "descricao_exploratoria", parametros = p), dados)
  stopifnot(identical(resultado$tabela, independente$resultado$tabela),
            identical(resultado$tabela, replay$tabela),
            identical(resultado$narrativa, replay$narrativa), is.data.frame(resultado$tabela))
  if (!is.null(resultado$grafico)) ggplot2::ggplot_build(resultado$grafico)
}
stopifnot(identical(dados, base_original))
calc <- function(modo, d = dados, ...) catalyser_descricao(d, c(list(analise = modo, variavel = nomes_num[1]), list(...)))
falha <- function(expr) inherits(tryCatch(force(expr), error = identity), "error")
x <- dados[[nomes_num[1]]]
resumo <- calc("resumo")$tabela
stopifnot(isTRUE(all.equal(resumo$Valor[resumo$Medida == "Média"], mean(x))),
          isTRUE(all.equal(resumo$Valor[resumo$Medida == "DP"], stats::sd(x))),
          sum(calc("histograma")$tabela$Frequência) == nrow(dados),
          identical(calc("normalidade")$tabela$p_valor, stats::shapiro.test(x)$p.value))
frequencias <- catalyser_descricao(dados, list(analise = "frequencias", variavel = "sexo"))$tabela
stopifnot(sum(frequencias$Absoluta) == 2641L, tail(frequencias$Acumulada_percentual, 1) == 100)
for (metodo in c("pearson", "spearman")) {
  esperado <- stats::cor.test(x, dados[[nomes_num[2]]], method = metodo, exact = FALSE)
  obtido <- calc("correlacao", outra = nomes_num[2], metodo = metodo)$tabela
  stopifnot(identical(obtido$p_valor, esperado$p.value), identical(obtido$Correlação, unname(esperado$estimate)))
}
tipada <- data.frame(valor = c(1, NA, 3, Inf), data = as.Date("2026-01-01") + 1:4,
                     categoria = factor(c("F", "M", NA, "F"), levels = c("F", "M", "I")))
stopifnot(identical(vapply(tipada, descricao_tipo, character(1)), c(valor = "Numérica", data = "Data", categoria = "Categórica")))
# O motor novo distingue os quatro tipos que orientam o retrato recomendado.
tipos_exploracao <- vapply(data.frame(nominal = factor(c("F", "M")), ordinal = ordered(c("baixo", "alto"), levels = c("baixo", "alto")), discreta = c(1L, 2L), continua = c(1.2, 2.7)), exploracao_tipo_variavel, character(1))
stopifnot(identical(unname(tipos_exploracao), c("Categórica nominal", "Categórica ordinal", "Numérica discreta", "Numérica contínua")),
          identical(exploracao_sugestao("Categórica nominal")$analise, "Qui-quadrado de aderência"),
          identical(exploracao_sugestao("Numérica contínua", "Numérica discreta")$analise, "Regressão linear"))
# A tela consolidada mantém uma linha por contínua e frequências coerentes com os válidos.
resumo_continuas <- resumir_continuas(dados, nomes_num[1:2])
freq_continua <- tabela_frequencia_exploratoria(dados, nomes_num[1], "Numérica contínua")
freq_nominal <- tabela_frequencia_exploratoria(dados, "sexo", "Categórica nominal")
stopifnot(identical(names(resumo_continuas), c("variavel", "n_validos", "ausentes", "media", "mediana",
                                                   "desvio_padrao", "minimo", "maximo", "pista_assimetria")),
          nrow(resumo_continuas) == 2L,
          sum(freq_continua$frequencia) == sum(is.finite(dados[[nomes_num[1]]])),
          sum(freq_nominal$frequencia) == sum(!is.na(dados$sexo)),
          inherits(desenhar_distribuicao(dados, nomes_num[1]), "ggplot"),
          inherits(desenhar_barras_ocean(dados, "sexo"), "ggplot"))
# O panorama traduz os nomes internos e oferece uma pista diferente para cada tipo.
panorama_tipos <- catalyser_descricao(
  data.frame(nominal = factor(c("F", "M")), ordinal = ordered(c("baixo", "alto"), levels = c("baixo", "alto")),
             discreta = c(1L, 2L), continua = c(1.2, 2.7)),
  list(analise = "panorama")
)$tabela
stopifnot(identical(names(panorama_tipos), c("Nome da variável", "Tipo de variável", "Valores ausentes", "Pistas para começar")),
          length(unique(panorama_tipos[["Pistas para começar"]])) == 4L,
          grepl("ordem", panorama_tipos[["Pistas para começar"]][2], fixed = TRUE))
# O link acompanha a quantidade observada de grupos sem impedir que o aluno escolha outro caminho.
dois_grupos <- data.frame(grupo = factor(rep(c("A", "B"), each = 4)), resposta = seq_len(8))
tres_grupos <- data.frame(grupo = factor(rep(c("A", "B", "C"), each = 4)), resposta = seq_len(12))
sugestao_t <- catalyser_descricao(dois_grupos, list(analise = "relacao", variavel = "grupo", outra = "resposta"))$sugestao
sugestao_anova <- catalyser_descricao(tres_grupos, list(analise = "relacao", variavel = "grupo", outra = "resposta"))$sugestao
stopifnot(identical(sugestao_t$destino, "Teste t de Student"),
          identical(sugestao_anova$destino, "ANOVA (Análise de Variância)"))
na <- catalyser_descricao(tipada, list(analise = "faltantes"))$tabela
stopifnot(identical(na$Ausentes, c(1L, 0L, 1L)), identical(na$Percentual, c(25, 0, 25)))
stopifnot(falha(catalyser_descricao(tipada, list(analise = "resumo", variavel = "data"))))
constante <- dados[1:4, ]; constante[[nomes_num[1]]] <- 2
stopifnot(falha(calc("normalidade", constante)), falha(calc("densidade", constante)),
          falha(calc("boxplot", constante, forma = "violino")),
          all(is.na(calc("outliers", constante)$tabela$Escore_z)),
          falha(calc("histograma", classes = 2.5)), falha(calc("correlacao", outra = nomes_num[1])))
negativa <- dados[1:4, ]; negativa[[nomes_num[1]]] <- c(-1, 0, 1, 4)
stopifnot(identical(calc("transformacoes", negativa)$tabela$Disponível, c(TRUE, FALSE, FALSE, FALSE)))
grande <- dados[rep(seq_len(nrow(dados)), 2), ]
stopifnot(is.na(calc("normalidade", grande)$tabela$p_valor), grepl("sem subamostragem", calc("normalidade", grande)$tabela$Situação))
cat("OK: motor exploratório, modos legados, tipagem, limites e código/replay equivalentes.\n")
