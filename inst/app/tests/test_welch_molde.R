# Execute de inst/app: painel, replay e script precisam preservar o mesmo método.
grDevices::pdf(NULL)
source('templates/funcoes_anova.R', encoding = 'UTF-8')
for (arquivo in c('registro_tratamentos.R', 'registro_bases.R',
                  'registro_execucoes.R', 'registro_comunicacao.R', 'exportacao_comunicacao.R')) {
  source(file.path('modules', arquivo), encoding = 'UTF-8')
}
perto <- function(x, y, tolerancia = 1e-9) {
  stopifnot(isTRUE(all.equal(unname(as.numeric(x)), unname(as.numeric(y)), tolerance = tolerancia)))
}
montar_dados <- function(tamanhos, escalas) {
  valores <- lapply(seq_along(tamanhos), function(i) {
    seq(-1.7, 1.7, length.out = tamanhos[i]) * escalas[i] + i * 2
  })
  data.frame(peso_g = unlist(valores),
    racao = factor(rep(c('Ração A', 'Ração-B', 'C'), tamanhos)))
}
cenarios <- list(homogeneo = montar_dados(c(8, 8, 8), c(1, 1, 1)),
  heterogeneo = montar_dados(c(8, 11, 14), c(.4, 3, 6)),
  pequeno = montar_dados(c(8, 8, 4), c(.4, 3, 6)))
destino <- Sys.getenv('CATALYSER_PROVA_WELCH', tempfile('welch_'))
dir.create(destino, recursive = TRUE, showWarnings = FALSE)
for (nome in names(cenarios)) {
  dados <- cenarios[[nome]]
  for (conf in c(.90, .95, .99)) {
    for (metodo in c('classica', 'welch', 'auto')) {
      r <- calcular_anova(dados, 'peso_g', 'racao', conf, metodo,
        rotulo_resposta = 'Peso final (g)', rotulo_fator = 'Ração')
      esperado <- if (r$metodo_usado == 'welch')
        stats::oneway.test(peso_g ~ racao, dados, var.equal = FALSE) else
          stats::oneway.test(peso_g ~ racao, dados, var.equal = TRUE)
      perto(r$f_anova, esperado$statistic)
      perto(r$p_anova, esperado$p.value)
      if (r$metodo_usado == 'welch') {
        # Implementação independente: rstatix recebe a fórmula e os dados originais.
        gh <- rstatix::games_howell_test(dados, peso_g ~ racao,
          conf.level = conf, detailed = TRUE)
        pos <- match(paste0(gh$group2, '-', gh$group1), r$tukey_df$Comparacao)
        stopifnot(!anyNA(pos))
        perto(r$tukey_df$Diferenca[pos], gh$estimate)
        perto(r$tukey_df$Lwr[pos], gh$conf.low)
        perto(r$tukey_df$Upr[pos], gh$conf.high)
        # rstatix arredonda os p para três algarismos significativos.
        # rstatix também arredonda diferenças pequenas de cauda para zero.
        stopifnot(all(abs(r$tukey_df$p_adj[pos] - gh$p.adj) <= 0.00051))
        omega_formula <- max(0, (r$f_anova - 1) * r$df_entre /
          (r$f_anova * r$df_entre + r$df_dentro + 1))
        perto(r$omega2, omega_formula)
        stopifnot(is.na(r$eta2), nrow(r$efeito_df) == 1L,
          all(is.na(r$anova_df$Soma_Quadrados)))
      } else {
        tk <- stats::TukeyHSD(stats::aov(peso_g ~ racao, dados), conf.level = conf)$racao
        perto(r$tukey_df$Diferenca, tk[, 'diff'])
        perto(r$tukey_df$Lwr, tk[, 'lwr'])
        perto(r$tukey_df$Upr, tk[, 'upr'])
        perto(r$tukey_df$p_adj, tk[, 'p adj'])
      }
      if (nome == 'pequeno' && r$metodo_usado == 'welch')
        stopifnot(nzchar(r$aviso_comparacoes))
      if (nome == 'pequeno') stopifnot(grepl('o grupo C tem', relatar_anova(r), fixed = TRUE))
      stopifnot(grepl('Peso final (g)', relatar_anova(r), fixed = TRUE),
        grepl(r$post_teste, relatar_anova(r), fixed = TRUE))
      p <- list(resposta = 'peso_g', fator = 'racao', nivel_confianca = conf,
        metodo = metodo, rotulo_y = 'Peso final (g)', rotulo_x = 'Ração')
      replay <- catalyser::catalyser_anova(dados, p)
      stopifnot(identical(replay$metodo_usado, r$metodo_usado),
        identical(replay$post_teste, r$post_teste),
        identical(replay$descritivos[['Diferença']], unname(r$letras[as.character(r$descritivos_df$Grupo)])))
      perto(replay$tabela[['p-valor']][1], r$p_anova)
      perto(replay$comparacoes[['Diferença estimada']], r$tukey_df$Diferenca)
      perto(replay$comparacoes[['IC inferior']], r$tukey_df$Lwr)
      perto(replay$comparacoes[['IC superior']], r$tukey_df$Upr)
      perto(replay$comparacoes[['p ajustado']], r$tukey_df$p_adj)
      # Os nomes com hífen e acento permanecem intactos nas letras.
      stopifnot(identical(names(r$letras), levels(dados$racao)))
      cat('CALCULO OK:', nome, metodo, conf, '=>', r$metodo_usado, '\n')
      # Dois métodos, três níveis de confiança, mais o aviso de grupo pequeno.
      exportar <- (nome == 'homogeneo' && metodo == 'classica') ||
        (nome == 'heterogeneo' && metodo == 'auto') ||
        (nome == 'pequeno' && metodo == 'welch' && conf == .95)
      if (!exportar) next
      id <- paste(nome, metodo, conf * 100, sep = '_')
      item <- list(id = 'execucao_0001', tipo = 'anova_um_fator', titulo = id,
        incluir_word = TRUE, estado_dependencia = 'Atualizada',
        base_tipo = 'compartilhada', base_id = 'dados_analise', base_objeto = 'dados_analise',
        parametros = p, saidas_word = c('narrativa', 'descritivos', 'tabela', 'comparacoes',
          'grafico', 'pressupostos', 'diagnosticos'))
      manifesto <- list(execucoes = list(execucao_0001 = item), secoes_globais = list())
      projeto <- exportacao_criar_projeto(destino = destino, nome_projeto = id,
        dados_brutos = dados, base_resolvida = dados, dados_analise = dados,
        pipeline = list(), base_externa = NULL, registro_bases = list(), cache_bases = list(),
        registro_execucoes = manifesto$execucoes, manifesto = manifesto, revisao_origem = 1L,
        import_info = list(source = 'local', file_name = 'dados.xlsx'), templates_dir = 'templates')
      e <- new.env(parent = globalenv())
      anterior <- getwd(); setwd(projeto)
      capture.output(sys.source('R/analise.R', e))
      setwd(anterior)
      stopifnot(identical(e$metodo_usado, r$metodo_usado), identical(e$post_teste, r$post_teste))
      perto(e$p_anova, r$p_anova); perto(e$f_anova, r$f_anova)
      perto(e$tabela_tukey$diff, r$tukey_df$Diferenca)
      perto(e$tabela_tukey$lwr, r$tukey_df$Lwr)
      perto(e$tabela_tukey$upr, r$tukey_df$Upr)
      perto(e$tabela_tukey$`p adj`, r$tukey_df$p_adj)
      stopifnot(identical(e$letras, r$letras))
      if (r$metodo_usado == 'welch') {
        perto(e$omega2, r$omega2)
        perto(e$omega_ic, c(r$efeito_df$IC_Inferior, r$efeito_df$IC_Superior))
      }
      cat('SCRIPT OK:', id, '\n')
      if ((nome == 'homogeneo' && conf == .90) || (nome == 'heterogeneo' && conf == .99)) {
        log <- file.path(destino, paste0(id, '_render.log'))
        status <- system2(file.path(R.home('bin'), 'Rscript.exe'),
          c(shQuote(normalizePath('templates/verificar_reprodutibilidade.R')), shQuote(projeto)), stdout = log, stderr = log)
        if (status != 0L) stop(paste(readLines(log, warn = FALSE), collapse = '\n'))
        html <- paste(readLines(file.path(projeto, 'saida/relatorios/relatorio_completo.html'),
          encoding = 'UTF-8', warn = FALSE), collapse = '\n')
        stopifnot(grepl('id="ref-delacre2019"', html, fixed = TRUE),
          grepl('id="ref-effectsizeConversao"', html, fixed = TRUE),
          grepl(e$post_teste, html, fixed = TRUE))
        cat('RENDER OK:', id, '; HTML + DOCX e citacoes presentes.\n')
      }
    }
  }
}
# Registros antigos não mudam automaticamente para Welch.
antigo <- catalyser::catalyser_anova(cenarios$heterogeneo,
  list(resposta = 'peso_g', fator = 'racao'))
stopifnot(identical(antigo$metodo_usado, 'classica'))
# Variância nula impede Welch com uma mensagem, sem resultados artificiais.
zero <- cenarios$heterogeneo; zero$peso_g[zero$racao == 'C'] <- 1
erro <- tryCatch(calcular_anova(zero, 'peso_g', 'racao', metodo = 'welch'), error = identity)
stopifnot(inherits(erro, 'error'), grepl('variância positiva', conditionMessage(erro), fixed = TRUE))
cat('PROVA WELCH CONCLUIDA\nDESTINO:', destino, '\n')
