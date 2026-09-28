# Percurso da ANOVA no molde novo: preparo confirmado -> ZIP -> código que
# viaja -> números do modelo. Script e QMDs rodam em sessões independentes,
# dentro do projeto descompactado (o molde usa here::i_am, sem stub de here).
invisible(Sys.setlocale("LC_ALL", "English_United States.utf8"))
source("app.R", encoding = "UTF-8")

brutos <- data.frame(
  tanque = seq_len(19),
  tratamento = c(rep(c("baixa", "media", "alta"), each = 6), "alta"),
  peso_g = c(100, 112, 108, 98, 115, 104, 120, 132, 118, 140, 125, 129,
             153, 148, 161, 155, 142, 165, NA_real_)
)
externa <- list(codigo = "dados <- dplyr::rename(dados, densidade = tratamento)",
                codigo_sequencial = "dados <- dplyr::rename(dados, densidade = tratamento)")
resolvida <- dplyr::rename(brutos, densidade = tratamento)
trilha <- list(list(tipo = "reescalar", ativa = TRUE,
                   params = list(coluna = "peso_g", simbolo = "k", nome = "peso_kg")))
compartilhada <- replay_pipeline(resolvida, trilha)$df
bases <- bases_adicionar(bases_vazio(),
  bases_novo_registro("base_0001", "Tanques selecionados", "base_tanques", finalidade = "anova"))
bases <- bases_adicionar_etapa(bases, "base_0001", "filtrar",
  list(coluna = "tanque", origem = "numerica", operador = ">", valor = 1), compartilhada)
cache <- list(base_0001 = bases_recalcular_cache(compartilhada, bases[[1]], 1L))
bases <- bases_finalizar(bases, "base_0001", cache, 1L)
e <- list(id = "execucao_0001", analise_id = "anova", tipo = "anova_um_fator",
  titulo = "Peso de tilápias por densidade",
  parametros = list(resposta = "peso_kg", fator = "densidade", nivel_confianca = .99),
  saidas_disponiveis = c("narrativa", "descritivos", "tabela", "comparacoes",
                         "grafico", "pressupostos", "diagnosticos"),
  base_id = "base_0001", base_objeto = "base_tanques", base_tipo = "derivada")
manifesto <- comunicacao_manifesto(comunicacao_estado_vazio(), list(execucao_0001 = e),
                                   list(execucao_0001 = "Atualizada"))
raiz <- Sys.getenv("CATALYSER_TESTE_ANOVA_DESTINO", unset = tempfile("anova_preparo_"))
dir.create(raiz, recursive = TRUE, showWarnings = FALSE)
zip_saida <- file.path(raiz, "anova_preparo.zip")
exportacao_empacotar_projeto(zip_saida, nome_projeto = "anova_preparo",
  dados_brutos = brutos, base_resolvida = resolvida, dados_analise = compartilhada,
  pipeline = trilha, base_externa = externa, registro_bases = bases, cache_bases = cache,
  registro_execucoes = list(execucao_0001 = e), manifesto = manifesto, revisao_origem = 1L,
  import_info = list(source = "package", package_dataset = "tilapias_teste"),
  templates_dir = "templates")
utils::unzip(zip_saida, exdir = raiz)
projeto <- file.path(raiz, "anova_preparo")
stopifnot(!file.exists(file.path(projeto, "dados/processados/base_resolvida.rds")))

# Cada entrada roda num processo R novo, com a pasta do projeto como local de
# trabalho — o .Rproj satisfaz here::i_am(), como no RStudio. O script e os
# dois QMDs do molde devem entregar a mesma base, as mesmas exclusões e os
# mesmos F, p e Tukey a 99%.
esperado <- calcular_anova(cache$base_0001$df, "peso_kg", "densidade", .99)
iguais <- function(x, y) isTRUE(all.equal(x, y, check.attributes = FALSE, tolerance = 1e-10))
documentos <- c("relatorio_completo.qmd", "relatorio_artigo.qmd")
entradas <- c(file.path(projeto, "R", "analise.R"), vapply(documentos, function(documento) {
  extraido <- file.path(raiz, paste0(documento, ".R"))
  knitr::purl(file.path(projeto, "relatorios", documento), output = extraido, quiet = TRUE)
  extraido
}, character(1)))
resultados <- list()
for (i in seq_along(entradas)) {
  verificador <- file.path(raiz, paste0("validar_", i, ".R"))
  resultado <- file.path(raiz, paste0("resultado_", i, ".rds"))
  log <- file.path(raiz, paste0("execucao_", i, ".log"))
  literal <- function(x) encodeString(normalizePath(x, winslash = "/", mustWork = FALSE), quote = '"')
  writeLines(c(
    sprintf("setwd(%s)", literal(projeto)),
    "grDevices::pdf(NULL)",
    sprintf("source(%s, encoding = 'UTF-8')", literal(entradas[i])),
    sprintf(paste(
      "saveRDS(list(compartilhada = as.data.frame(dados_analise),",
      "base_da_anova = as.data.frame(dados_da_analise),",
      "n_total = n_total, n_utilizado = n_utilizado, n_excluido = n_excluido,",
      "n_base = nrow(base_anova),",
      "f = tabela_anova$statistic[1], p = tabela_anova$p.value[1],",
      "tukey = tukey[[1]], resumo = as.data.frame(tabela_resumo),",
      "classe_efeito = classe_efeito, frase_anova = texto_anova), %s)"), literal(resultado))
  ), verificador, useBytes = TRUE)
  status <- system2(file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"),
    shQuote(verificador), stdout = log, stderr = log)
  if (status != 0L) stop(paste(readLines(log, warn = FALSE), collapse = "\n"))
  resultados[[i]] <- readRDS(resultado)
}
script <- resultados[[1]]
qmd_completo <- resultados[[2]]
qmd_artigo <- resultados[[3]]
for (env in resultados) {
  stopifnot(iguais(env$compartilhada, compartilhada),
    iguais(env$base_da_anova, cache$base_0001$df),
    env$n_total == 18L, env$n_utilizado == 17L, env$n_excluido == 1L,
    env$n_base == esperado$n,
    iguais(env$f, esperado$f_anova),
    iguais(env$p, esperado$p_anova),
    iguais(unname(env$tukey), unname(stats::TukeyHSD(esperado$fit, conf.level = .99)[[1]])))
}
stopifnot(iguais(script$resumo, qmd_completo$resumo),
          iguais(script$resumo, qmd_artigo$resumo),
          identical(script$classe_efeito, qmd_completo$classe_efeito),
          identical(script$classe_efeito, qmd_artigo$classe_efeito),
          identical(script$frase_anova, qmd_completo$frase_anova),
          identical(script$frase_anova, qmd_artigo$frase_anova))
cat("OK: ZIP ANOVA do molde, conferência da base, bases, exclusões, F, p, Tukey 99% e script/QMDs equivalentes.\n")
cat("Projeto para Render:", normalizePath(projeto, winslash = "/"), "\n")
