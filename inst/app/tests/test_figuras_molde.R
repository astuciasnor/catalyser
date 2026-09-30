# Execute de inst/app. Compara dados e argumentos das camadas reais, não textos do código.
for (arquivo in c('registro_tratamentos.R', 'registro_bases.R', 'registro_execucoes.R',
                  'registro_comunicacao.R', 'exportacao_comunicacao.R')) {
  source(file.path('modules', arquivo), encoding = 'UTF-8')
}
source('templates/funcoes_anova.R', encoding = 'UTF-8')
bagres <- as.data.frame(EAPADados::isoproteica_bagre)
item <- list(id = 'execucao_0001', tipo = 'anova_um_fator', titulo = 'Peso de bagres',
  incluir_word = TRUE, estado_dependencia = 'Atualizada', base_tipo = 'compartilhada',
  base_id = 'dados_analise', base_objeto = 'dados_analise',
  parametros = list(resposta = 'peso_g', fator = 'racao', nivel_confianca = .95,
    ajuste_comparacoes = 'tukey', rotulo_x = 'Ração', rotulo_y = 'Peso (g)'),
  saidas_word = c('narrativa', 'tabela', 'grafico', 'pressupostos'))
manifesto <- list(execucoes = list(execucao_0001 = item), secoes_globais = list())
destino <- Sys.getenv('CATALYSER_PROVA_FIGURAS', tempfile('figuras_'))
dir.create(destino, recursive = TRUE, showWarnings = FALSE)
projeto <- exportacao_criar_projeto(destino = destino, nome_projeto = 'anova_figuras',
  dados_brutos = bagres, base_resolvida = bagres, dados_analise = bagres,
  pipeline = list(), base_externa = NULL, registro_bases = list(), cache_bases = list(),
  registro_execucoes = manifesto$execucoes, manifesto = manifesto, revisao_origem = 1L,
  import_info = list(source = 'package', package_dataset = 'isoproteica_bagre'),
  templates_dir = 'templates')
ambiente <- new.env(parent = globalenv())
anterior <- getwd()
setwd(projeto)
sys.source('R/analise.R', envir = ambiente)
setwd(anterior)
painel <- grafico_anova(calcular_anova(bagres, 'peso_g', 'racao'))
exportado <- ambiente$grafico_barras
camadas_painel <- ggplot2::ggplot_build(painel)$data
camadas_exportado <- ggplot2::ggplot_build(exportado)$data
# Jitter é aleatório; comparamos limites, médias, letras e rótulos determinísticos.
for (i in 2:5) {
  colunas <- intersect(c('x', 'y', 'ymin', 'ymax', 'label', 'shape', 'size',
                         'linewidth', 'alpha', 'fill', 'hjust', 'vjust'),
                       names(camadas_painel[[i]]))
  stopifnot(isTRUE(all.equal(camadas_painel[[i]][colunas],
                            camadas_exportado[[i]][colunas], check.attributes = FALSE)))
}
# As hastes continuam sendo IC; o rótulo usa DP amostral, conferido por fora.
rotulos <- vapply(split(bagres$peso_g, bagres$racao), function(x) {
  paste0(fmt(mean(x)), ' ± ', fmt(sd(x)))
}, character(1))
stopifnot(identical(unname(rotulos), as.character(camadas_painel[[5]]$label)))
ggplot2::ggsave(file.path(destino, 'painel_anova.png'), painel,
  width = 9, height = 5.5, dpi = 140, bg = 'white')
cat('OK: ANOVA painel/template com mesmas hastes, medias, letras, rotulos e argumentos das camadas.\n')
cat('PROJETO:', projeto, '\n')
