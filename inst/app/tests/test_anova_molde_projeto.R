# Execute de inst/app. Confere a árvore e o roteiro ANOVA do molde novo que
# efetivamente vão para o aluno, e que script e QMDs rodam em sessões novas.
grDevices::pdf(NULL)
for (arquivo in c("registro_tratamentos.R", "registro_bases.R", "registro_execucoes.R",
                  "registro_comunicacao.R", "exportacao_comunicacao.R")) {
  source(file.path("modules", arquivo), encoding = "UTF-8")
}

item <- list(id = "execucao_0001", tipo = "anova_um_fator", titulo = "Peso final de bagres por racao",
  incluir_word = TRUE, estado_dependencia = "Atualizada", base_tipo = "compartilhada",
  base_id = "dados_analise", base_objeto = "dados_analise",
  parametros = list(resposta = "peso_g", fator = "racao", nivel_confianca = .95,
    ajuste_comparacoes = "tukey", tema = "minimal", titulo_grafico = "",
    rotulo_x = "Racao", rotulo_y = "Peso final (g)"),
  saidas_word = c("narrativa", "descritivos", "tabela", "comparacoes",
                  "grafico", "pressupostos", "diagnosticos"))
bagres <- as.data.frame(EAPADados::isoproteica_bagre)

# Gera a árvore atual: um script e dois QMDs que executam esse mesmo script.
destino <- Sys.getenv("CATALYSER_TESTE_ANOVA_DESTINO", unset = tempfile("anova_bagres_"))
dir.create(destino, recursive = TRUE, showWarnings = FALSE)
manifesto <- list(execucoes = list(execucao_0001 = item), secoes_globais = list())
projeto <- exportacao_criar_projeto(destino = destino, nome_projeto = "anova_bagres",
  dados_brutos = bagres, base_resolvida = bagres, dados_analise = bagres,
  pipeline = list(), base_externa = NULL, registro_bases = list(), cache_bases = list(),
  registro_execucoes = manifesto$execucoes, manifesto = manifesto, revisao_origem = 1L,
  import_info = list(source = "package", package_dataset = "isoproteica_bagre"),
  templates_dir = "templates")

# Árvore do molde: dois QMDs, apoio único (funcoes.R, _quarto.yml, apa.csl) e
# imagens/ presente; nada do relatório sincronizado legado.
script <- file.path(projeto, "R", "analise.R")
documentos <- c("relatorio_completo.qmd", "relatorio_artigo.qmd")
stopifnot(file.exists(script),
  file.exists(file.path(projeto, "R", "funcoes.R")),
  file.exists(file.path(projeto, "_quarto.yml")),
  file.exists(file.path(projeto, "relatorios", "apa.csl")),
  dir.exists(file.path(projeto, "imagens")),
  !file.exists(file.path(projeto, "relatorios", "relatorio.qmd")))
# O funcoes.R do molde não tem a seção de manutenção do relatório sincronizado
# e traz a função de conferência da base reconstruída (molde só com CRAN).
funcoes <- readLines(file.path(projeto, "R", "funcoes.R"), encoding = "UTF-8")
stopifnot(!any(grepl("conferir_codigo", funcoes, fixed = TRUE)),
  any(grepl("conferir_base", funcoes, fixed = TRUE)))

# Script: sem marcadores legados nem {{...}} sobrando, funcoes.R carregado uma
# única vez, os objetos mínimos do contrato presentes e nenhuma dependência da
# CatalyseR ou do EAPADados (o projeto exportado só usa CRAN).
linhas_script <- readLines(script, encoding = "UTF-8")
stopifnot(!any(grepl("{{", linhas_script, fixed = TRUE)),
  !any(grepl("^## ---- ", linhas_script)),
  sum(grepl('source(here::here("R", "funcoes.R")', linhas_script, fixed = TRUE)) == 1L,
  !any(grepl("library(catalyser)|library(EAPADados)|requireNamespace",
    linhas_script)),
  !any(grepl("catalyser::|EAPADados::", linhas_script)))
objetos_contrato <- c("n_total", "n_utilizado", "n_excluido", "texto_amostra",
  "texto_sintese_estatistica", "alerta_modelo", "registro_ambiente",
  "ic_percentual", "resumo_console", "tabela_anova", "tabela_resumo",
  "tabela_tukey", "tabela_efeito", "texto_anova", "texto_efeito", "texto_tukey",
  "texto_pressupostos")
stopifnot(all(vapply(objetos_contrato, function(nome)
  any(grepl(nome, linhas_script, fixed = TRUE)), logical(1))))

# QMDs: executam o script, não leem arquivos de dados, não sobram marcadores e
# não exibem a saída bruta de console (fica no script; os relatórios usam
# tabelas formatadas).
for (documento in documentos) {
  qmd <- readLines(file.path(projeto, "relatorios", documento), encoding = "UTF-8")
  stopifnot(!any(grepl("{{", qmd, fixed = TRUE)),
    any(grepl('source(here::here("R", "analise.R"), encoding = "UTF-8")', qmd, fixed = TRUE)),
    !any(grepl("read.csv|readRDS|ggsave", qmd)),
    !any(grepl("resumo_console", qmd, fixed = TRUE)))
}

# README: seção de preparo do computador com a lista exata de pacotes do CRAN
# usados por script e funcoes.R, nunca uma lista fixa.
readme <- readLines(file.path(projeto, "README.md"), encoding = "UTF-8")
stopifnot(any(grepl("Preparar o computador", readme, fixed = TRUE)),
  any(grepl("install.packages(", readme, fixed = TRUE)),
  any(grepl("Nenhum pacote é instalado automaticamente", readme, fixed = TRUE)))

# Regra do molde: os Diagnósticos são subseção da Exploração, antes dos
# Resultados; os gráficos de resíduos e Q-Q saem um depois do outro, cada um
# com rótulo, legenda e out-width próprios, centralizados (sem layout-ncol).
completo <- readLines(file.path(projeto, "relatorios", "relatorio_completo.qmd"),
  encoding = "UTF-8")
titulos <- grep("^#+ ", completo, value = TRUE)
pos_exploracao <- which(grepl("^# Exploração", titulos))[1]
pos_diagnosticos <- which(grepl("^## Diagnósticos", titulos))[1]
pos_resultados <- which(grepl("^# Resultados", titulos))[1]
stopifnot(length(pos_diagnosticos) == 1L,
  pos_exploracao < pos_diagnosticos, pos_diagnosticos < pos_resultados,
  !any(grepl("^# Diagnósticos", titulos)))
linha_exploracao <- which(grepl("^# Exploração", completo))[1]
linha_resultados <- which(grepl("^# Resultados", completo))[1]
linha_residuos <- which(grepl("label: fig-residuos$", completo))[1]
linha_qq <- which(grepl("label: fig-qq$", completo))[1]
stopifnot(length(linha_residuos) == 1L, length(linha_qq) == 1L,
  linha_exploracao < linha_residuos, linha_residuos < linha_qq,
  linha_qq < linha_resultados,
  sum(grepl("out-width:", completo, fixed = TRUE)) == 2L,
  !any(grepl("layout-ncol", completo, fixed = TRUE)),
  !any(grepl("fig-subcap", completo, fixed = TRUE)))

# Script e cada QMD rodam em processos R independentes, como no Render.
# Além de rodarem, os números precisam reproduzir a ANOVA da CatalyseR.
esperado <- aov(peso_g ~ racao, data = bagres)
sumario <- summary(esperado)[[1]]
tukey_esperado <- TukeyHSD(esperado, conf.level = .95)$racao
entradas <- c(script, vapply(documentos, function(documento) {
  extraido <- file.path(destino, paste0(documento, ".R"))
  knitr::purl(file.path(projeto, "relatorios", documento), output = extraido, quiet = TRUE)
  extraido
}, character(1)))
for (i in seq_along(entradas)) {
  verificador <- file.path(destino, paste0("validar_", i, ".R"))
  resultado <- file.path(destino, paste0("resultado_", i, ".rds"))
  log <- file.path(destino, paste0("execucao_", i, ".log"))
  literal <- function(x) encodeString(normalizePath(x, winslash = "/", mustWork = FALSE), quote = '"')
  writeLines(c(
    sprintf("setwd(%s)", literal(projeto)),
    "grDevices::pdf(NULL)",
    sprintf("source(%s, encoding = 'UTF-8')", literal(entradas[i])),
    "stopifnot(is.data.frame(tabela_anova), is.data.frame(tabela_resumo), inherits(grafico_barras, 'ggplot'))",
    sprintf("saveRDS(list(f = tabela_anova$statistic[1], p = tabela_anova$p.value[1], n = nrow(base_anova), medias = unname(tapply(base_anova$resposta, base_anova$grupo, mean)), tukey = as.data.frame(tukey[[1]])), %s)", literal(resultado))
  ), verificador, useBytes = TRUE)
  status <- system2(file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"),
    shQuote(verificador), stdout = log, stderr = log)
  if (status != 0L) stop(paste(readLines(log, warn = FALSE), collapse = "\n"))
  obtido <- readRDS(resultado)
  stopifnot(isTRUE(all.equal(obtido$f, unname(sumario$`F value`[1]))),
    isTRUE(all.equal(obtido$p, unname(sumario$`Pr(>F)`[1]))),
    obtido$n == 19L,
    isTRUE(all.equal(obtido$medias, unname(tapply(bagres$peso_g, bagres$racao, mean)))),
    isTRUE(all.equal(obtido$tukey$diff, unname(tukey_esperado[, "diff"]))),
    isTRUE(all.equal(obtido$tukey$lwr, unname(tukey_esperado[, "lwr"]))))
}

cat("OK: árvore do molde ANOVA, contrato de objetos, só CRAN, README com pacotes, diagnósticos dentro da Exploração em gráficos separados sem saída bruta de console, e script + dois QMDs independentes com os números da ANOVA.\n")
cat("PROJETO:", projeto, "\n")
