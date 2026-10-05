# Projetos curtos: mesmo contrato, cálculos adequados a cada desenho.
grDevices::pdf(NULL)
for (arquivo in c('registro_tratamentos.R', 'registro_bases.R', 'registro_execucoes.R',
                  'registro_comunicacao.R', 'exportacao_comunicacao.R')) {
  source(file.path('modules', arquivo), encoding = 'UTF-8')
}
perto <- function(x, y, tolerancia = 1e-8) stopifnot(isTRUE(all.equal(
  unname(as.numeric(x)), unname(as.numeric(y)), tolerance = tolerancia)))
dados <- as.data.frame(EAPADados::morfometria_barbo)
# Faltantes em uma medida excluem o par, preservando a outra e a identificação.
dados$comprimento_cabeca[1] <- NA_real_
dados$focinho_occipital[2] <- NA_real_
dados$coluna_irrelevante <- NA_real_
destino <- Sys.getenv('CATALYSER_PROVA_T_DEGRAUS', tempfile('degraus_t_'))
dir.create(destino, recursive = TRUE, showWarnings = FALSE)
for (pareado in c(FALSE, TRUE)) {
  for (alternativa in c('two.sided', 'greater', 'less')) {
    conf <- switch(alternativa, two.sided = .95, greater = .90, less = .99)
    nome <- paste(if (pareado) 'pareado' else 'uma_amostra', alternativa, sep = '_')
    p <- c(list(tipo_teste = if (pareado) 'paired' else 'one_val',
      alternativa = alternativa, nivel_confianca = conf),
      if (pareado) list(variavel_1 = 'comprimento_cabeca', variavel_2 = 'focinho_occipital') else
        list(variavel = 'comprimento_cabeca', media_hipotetica = 29))
    item <- list(id = 'execucao_0001', tipo = if (pareado) 'teste_t_paired' else 'teste_t_one_val',
      titulo = nome, parametros = p, incluir_word = TRUE, estado_dependencia = 'Atualizada',
      base_tipo = 'compartilhada', base_id = 'dados_analise', base_objeto = 'dados_analise',
      saidas_word = c('narrativa', 'tabela', 'grafico', 'pressupostos', 'diagnosticos'))
    manifesto <- list(execucoes = list(execucao_0001 = item), secoes_globais = list())
    projeto <- exportacao_criar_projeto(destino = destino, nome_projeto = nome,
      dados_brutos = dados, base_resolvida = dados, dados_analise = dados, pipeline = list(),
      base_externa = NULL, registro_bases = list(), cache_bases = list(),
      registro_execucoes = manifesto$execucoes, manifesto = manifesto, revisao_origem = 1L,
      import_info = list(source = 'package', package_dataset = 'morfometria_barbo'), templates_dir = 'templates')
    stopifnot(file.exists(file.path(projeto, 'relatorios/relatorio_completo.qmd')),
      file.exists(file.path(projeto, 'relatorios/relatorio_artigo.qmd')),
      !file.exists(file.path(projeto, 'relatorios/relatorio.qmd')))
    e <- new.env(parent = globalenv()); anterior <- getwd(); setwd(projeto)
    capture.output(sys.source('R/analise.R', e)); setwd(anterior)
    esperado <- if (pareado) stats::t.test(dados$comprimento_cabeca, dados$focinho_occipital,
      paired = TRUE, alternative = alternativa, conf.level = conf) else
        stats::t.test(dados$comprimento_cabeca, mu = 29, alternative = alternativa, conf.level = conf)
    perto(e$estatistica_t, esperado$statistic); perto(e$p_teste, esperado$p.value)
    perto(e$ic_estimativa, esperado$conf.int)
    stopifnot(identical(e$teste_t$alternative, alternativa),
      e$n_excluido == if (pareado) 2L else 1L,
      identical(e$base_t$linha_original, which(if (pareado)
        complete.cases(dados[c('comprimento_cabeca', 'focinho_occipital')]) else !is.na(dados$comprimento_cabeca))))
    valores <- if (pareado) (dados$comprimento_cabeca - dados$focinho_occipital) else dados$comprimento_cabeca
    valores <- valores[!is.na(valores)]
    d <- (mean(valores) - if (pareado) 0 else 29) / sd(valores)
    perto(e$d_cohen, d)
    # IC independente: invertemos a t não central e dividimos por sqrt(n).
    t <- unname(esperado$statistic); gl <- length(valores) - 1
    # Neste conjunto grande, os limites ficam próximos de t; evitamos caudas
    # extremas onde a própria pt() avisa sobre perda de precisão numérica.
    intervalo <- t + c(-10, 10)
    limite <- function(prob) uniroot(function(ncp) stats::pt(t, gl, ncp = ncp) - prob,
      intervalo, tol = 1e-9)$root / sqrt(length(valores))
    ic_d <- c(limite(1 - (1 - conf) / 2), limite((1 - conf) / 2))
    perto(e$d_ic, ic_d, 1e-5)
    replay <- catalyser::catalyser_teste_t(dados, p)
    perto(replay$tabela[['p-valor']], esperado$p.value)
    # Levene não faz parte dos dois desenhos.
    stopifnot(!exists('teste_levene', envir = e, inherits = FALSE))
    if (pareado) perto(e$base_t$diferenca, valores)
    cat('SCRIPT OK:', nome, '; t, p, IC, d, IC independente e exclusoes conferidos.\n')
    if (alternativa == 'two.sided') {
      log <- file.path(destino, paste0(nome, '_render.log'))
      status <- system2(file.path(R.home('bin'), 'Rscript.exe'),
        c(shQuote(normalizePath('templates/verificar_reprodutibilidade.R')), shQuote(projeto)), stdout = log, stderr = log)
      if (status != 0L) stop(paste(readLines(log, warn = FALSE), collapse = '\n'))
      html <- paste(readLines(file.path(projeto, 'saida/relatorios/relatorio_completo.html'),
        warn = FALSE, encoding = 'UTF-8'), collapse = '\n')
      stopifnot(grepl('id="ref-weissgerber2015"', html, fixed = TRUE),
        grepl('id="ref-rcore2025"', html, fixed = TRUE))
      cat('RENDER OK:', nome, '; HTML + DOCX, arquivos novos e citacoes resolvidas.\n')
    }
  }
}
cat('PROVA T DEGRAUS CONCLUIDA\nDESTINO:', destino, '\n')
