source(file.path("tests", "carregar_catalyser.R"), chdir = FALSE)

dados <- data.frame(
  x = 1:12,
  y = c(2, 4, 5, 8, 8, 11, 13, 15, 17, 18, 21, 24),
  resposta_binaria = rep(c(0, 1), 6),
  grupo = factor(rep(c("F", "M"), each = 6)),
  especie = factor(rep(c("Pescada", "Corvina"), 6)),
  z = c(5, 4, 7, 8, 9, 11, 12, 11, 14, 15, 16, 18)
)

execucao <- function(tipo, parametros) list(
  id = "execucao_0001", tipo = tipo, titulo = tipo,
  parametros = parametros
)

resultados <- list(
  catalyser_executar(execucao(
    "estatistica_descritiva",
    list(variaveis = c("x", "y"), grupo = "grupo", metricas = NULL)
  ), dados),
  catalyser_executar(execucao(
    "regressao_linear",
    list(resposta = "y", preditor = "x", grupo = "none")
  ), dados),
  catalyser_executar(execucao(
    "regressao_linear",
    list(resposta = "y", preditor = "x", grupo = "grupo", regressao_por_grupo = TRUE)
  ), dados),
  catalyser_executar(execucao(
    "regressao_logistica",
    list(resposta = "resposta_binaria", preditor = "x", grupo = "none")
  ), dados),
  catalyser_executar(execucao(
    "teste_t_one_val",
    list(tipo_teste = "one_val", variavel = "y", media_hipotetica = 10,
         alternativa = "two.sided", nivel_confianca = 0.95)
  ), dados),
  catalyser_executar(execucao(
    "teste_t_two_ind",
    list(tipo_teste = "two_ind", resposta = "y", grupo = "grupo",
         variancias_iguais = FALSE, alternativa = "two.sided", nivel_confianca = 0.95)
  ), dados),
  catalyser_executar(execucao(
    "teste_t_paired",
    list(tipo_teste = "paired", variavel_1 = "y", variavel_2 = "z",
         alternativa = "two.sided", nivel_confianca = 0.95)
  ), dados),
  catalyser_executar(execucao(
    "grafico_linhas",
    list(x = "x", y = "y", grupo = "grupo", mostrar_pontos = TRUE, espessura_linha = 1)
  ), dados),
  catalyser_executar(execucao(
    "qui_quadrado",
    list(var_row = "grupo", var_col = "especie", tabela = NULL, yates = TRUE)
  ), dados),
  catalyser_executar(execucao(
    "pca",
    list(variaveis = c("x", "y", "z"), padronizar = TRUE)
  ), dados),
  catalyser_executar(execucao(
    "hca",
    list(variaveis = c("x", "y", "z"), distancia = "euclidean",
         ligacao = "complete", numero_grupos = 3L, padronizar = TRUE)
  ), dados)
)

stopifnot(
  length(resultados) == 11L,
  all(vapply(resultados, inherits, logical(1), "resultado_catalyser")),
  inherits(resultados[[2]]$objeto, "lm"),
  is.list(resultados[[3]]$objeto),
  all(vapply(resultados[[3]]$objeto, inherits, logical(1), "lm")),
  length(unique(ggplot2::ggplot_build(resultados[[3]]$grafico)$data[[1]]$colour)) > 1L,
  inherits(resultados[[4]]$objeto, "glm"),
  all(vapply(resultados[5:7], function(x) inherits(x$objeto, "htest"), logical(1))),
  inherits(resultados[[8]]$grafico, "ggplot"),
  length(unique(ggplot2::ggplot_build(resultados[[8]]$grafico)$data[[1]]$colour)) > 1L,
  inherits(resultados[[9]]$objeto, "htest"),
  inherits(resultados[[10]]$objeto, "prcomp"),
  inherits(resultados[[11]]$objeto, "hclust")
)

cat("OK: replay integrado cobre as oito análises prioritárias da Fase 3E\n")
