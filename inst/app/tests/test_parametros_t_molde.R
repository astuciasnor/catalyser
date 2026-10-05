# Execute de inst/app. As escolhas do painel precisam sobreviver à exportação.
# Dados com variâncias distintas permitem detectar uma troca indevida de método.
grDevices::pdf(NULL)
for (arquivo in c('registro_tratamentos.R', 'registro_bases.R',
                  'registro_execucoes.R', 'registro_comunicacao.R',
                  'exportacao_comunicacao.R')) {
  source(file.path('modules', arquivo), encoding = 'UTF-8')
}
dados <- data.frame(
  grupo = rep(c('A', 'B'), c(8, 11)),
  resposta = c(c(-1.3, -.8, -.4, -.1, .1, .4, .8, 1.3) + 4,
               c(-12, -8, -5, -3, -1, 0, 1, 3, 5, 8, 12) + 2)
)
destino <- Sys.getenv('CATALYSER_PROVA_PARAMETROS_T', tempfile('parametros_t_'))
dir.create(destino, recursive = TRUE, showWarnings = FALSE)
quarto <- Sys.getenv('QUARTO_PATH', unname(Sys.which('quarto')))
stopifnot(nzchar(quarto), file.exists(quarto))
for (igual in c(TRUE, FALSE)) {
  for (alternativa in c('two.sided', 'greater', 'less')) {
    nome <- paste(if (igual) 'student' else 'welch', alternativa, sep = '_')
    item <- list(id = 'execucao_0001', tipo = 'teste_t_two_ind', titulo = nome,
      incluir_word = TRUE, estado_dependencia = 'Atualizada',
      base_tipo = 'compartilhada', base_id = 'dados_analise',
      base_objeto = 'dados_analise', parametros = list(
        tipo_teste = 'two_ind', resposta = 'resposta', grupo = 'grupo',
        variancias_iguais = igual, alternativa = alternativa, nivel_confianca = .99),
      saidas_word = c('narrativa', 'tabela', 'grafico', 'pressupostos'))
    manifesto <- list(execucoes = list(execucao_0001 = item), secoes_globais = list())
    projeto <- exportacao_criar_projeto(destino = destino, nome_projeto = nome,
      dados_brutos = dados, base_resolvida = dados, dados_analise = dados,
      pipeline = list(), base_externa = NULL, registro_bases = list(),
      cache_bases = list(), registro_execucoes = manifesto$execucoes,
      manifesto = manifesto, revisao_origem = 1L,
      import_info = list(source = 'local', file_name = 'dados.xlsx'),
      templates_dir = 'templates')
    ambiente <- new.env(parent = globalenv())
    anterior <- getwd()
    setwd(projeto)
    capture.output(sys.source('R/analise.R', ambiente, keep.source = FALSE))
    setwd(anterior)
    # Referência por vetores, independente da fórmula do template e do replay.
    esperado <- stats::t.test(dados$resposta[dados$grupo == 'A'],
      dados$resposta[dados$grupo == 'B'], var.equal = igual,
      alternative = alternativa, conf.level = .99)
    stopifnot(identical(ambiente$variancias_iguais, igual),
      identical(ambiente$teste_t$alternative, alternativa),
      identical(attr(ambiente$teste_t$conf.int, 'conf.level'), .99),
      isTRUE(all.equal(unname(ambiente$teste_t$statistic), unname(esperado$statistic))),
      isTRUE(all.equal(ambiente$teste_t$p.value, esperado$p.value)),
      isTRUE(all.equal(ambiente$teste_t$conf.int, esperado$conf.int)),
      grepl(ambiente$metodo_teste, ambiente$texto_levene_decisao, fixed = TRUE))
    # O replay é a reconstrução usada pela Comunicação de Resultados.
    replay <- catalyser::catalyser_executar(item, dados)
    stopifnot(isTRUE(all.equal(replay$tabela[['p-valor']], esperado$p.value)),
      isTRUE(all.equal(replay$tabela[['IC inferior']], unname(esperado$conf.int[1]))),
      isTRUE(all.equal(replay$tabela[['IC superior']], unname(esperado$conf.int[2]))))
    # O poder preserva a direção; abs(d) daria uma resposta errada para H1 menor.
    poder <- pwr::pwr.t.test(n = mean(c(ambiente$n_1, ambiente$n_2)),
      d = ambiente$d_cohen, sig.level = .01, type = 'two.sample',
      alternative = alternativa)$power
    stopifnot(isTRUE(all.equal(ambiente$poder_teste_t, poder)))
    cat('OK:', nome, '; confiança 99%; t, p, IC e replay conferidos.\n')
    # Os dois sentidos unilaterais passam também pelo HTML e pelo Word.
    if ((igual && alternativa == 'greater') || (!igual && alternativa == 'less')) {
      log <- file.path(destino, paste0(nome, '_render.log'))
      status <- system2(file.path(R.home('bin'), 'Rscript.exe'),
        c(shQuote(normalizePath('templates/verificar_reprodutibilidade.R')), shQuote(projeto)),
        stdout = log, stderr = log)
      if (status != 0L) stop(paste(readLines(log, warn = FALSE), collapse = '\n'))
      cat('RENDER OK:', nome, '; HTML + DOCX, arquivos novos e sem ??.\n')
    }
  }
}
cat('PROVA PARAMETROS T CONCLUIDA\nDESTINO:', destino, '\n')
