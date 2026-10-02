# A rota de duas análises deve manter os objetos de estudo do caderno HTML.
for (arquivo in c("registro_tratamentos.R", "registro_bases.R", "registro_execucoes.R",
  "registro_comunicacao.R", "exportacao_comunicacao.R")) source(file.path("modules", arquivo), encoding = "UTF-8")
source("../../R/analises.R", encoding = "UTF-8")
dados <- as.data.frame(EAPADados::artemia)
# Índice simulado apenas para exercitar os modelos desta verificação.
dados$indice_simulado <- seq_len(nrow(dados))
p_t <- list(tipo_teste = "two_ind", resposta = "taxa_crescimento_mg_dia", grupo = "racao",
  alternativa = "two.sided", variancias_iguais = FALSE, nivel_confianca = .95)
r_t <- catalyser_teste_t(dados, p_t)
for (nome in c("grafico_caixa", "grafico_residuos", "grafico_qq", "grafico_dispersao"))
  stopifnot(inherits(r_t[[nome]], "ggplot"))
ref <- t.test(taxa_crescimento_mg_dia ~ racao, dados, var.equal = FALSE)
stopifnot(isTRUE(all.equal(r_t$objeto$statistic, ref$statistic)))
letras <- ggplot2::ggplot_build(r_t$grafico)$data[[6]]$label
stopifnot(identical(sort(letras), c("a", "b")))
p_r <- list(resposta = "taxa_crescimento_mg_dia", preditor = "indice_simulado",
  grupo = "racao", regressao_por_grupo = TRUE, tipo_modelo = "linear")
r_r <- catalyser_regressao(dados, p_r)
for (nome in c("grafico_residuos", "grafico_qq", "grafico_dispersao", "grafico_influencia"))
  stopifnot(inherits(r_r[[nome]], "ggplot"))
stopifnot(nrow(ggplot2::ggplot_build(r_r$grafico_residuos)$data[[1]]) == nrow(dados))
itens <- list(
 t = list(id = "t", tipo = "teste_t_two_ind", parametros = p_t, titulo = "Teste t",
  incluir_word = TRUE, base_tipo = "compartilhada", base_id = "dados_analise", base_objeto = "dados_analise",
  saidas_word = c("narrativa", "tabela", "grafico", "pressupostos")),
 r = list(id = "r", tipo = "regressao_linear", parametros = p_r, titulo = "Regressão por grupo",
  incluir_word = TRUE, base_tipo = "compartilhada", base_id = "dados_analise", base_objeto = "dados_analise",
  saidas_word = c("narrativa", "tabela", "grafico", "pressupostos")))
manifesto <- list(execucoes = itens, secoes_globais = list())
qmd <- exportacao_gerar_qmd(manifesto)
script <- exportacao_gerar_script(manifesto, "prova", templates_dir = "templates")
for (nome in names(exportacao_figuras_estudo_t)) {
 stopifnot(any(grepl(gsub("_", "-", nome, fixed = TRUE), qmd, fixed = TRUE)), any(grepl(nome, script, fixed = TRUE)))
}
stopifnot(sum(grepl('### Exploração e pressupostos:', qmd, fixed = TRUE)) == 2)
for (item in itens) {
 raiz <- unname(exportacao_raizes_chunk(itens)[[item$id]])
 estudo <- match(paste0("#| label: ", exportacao_nome_componente(item$id,
   "grafico_residuos", raiz, item$tipo)), qmd)
 resultado <- match(paste0("#| label: ", exportacao_nome_componente(item$id,
   "grafico", raiz, item$tipo)), qmd)
 stopifnot(length(estudo) > 0, length(resultado) > 0, min(estudo) < min(resultado))
}
stopifnot(sum(grepl(':::: {.content-visible when-format="html"}', qmd, fixed = TRUE)) >= 2)
cat("OK: duas análises preservam exploração e diagnósticos do HTML, letras e cálculos.\n")
