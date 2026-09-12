# Percurso da ANOVA: preparo confirmado -> ZIP -> código externo -> modelo.
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

# Executa os chunks e os trechos do ZIP, sem instalar pacotes nem usar a IDE.
# A função here local representa a raiz do projeto aberto no RStudio.
rodar <- function(script) {
  arquivo <- if (script) "R/analise.R" else "relatorios/relatorio.qmd"
  linhas <- readLines(file.path(projeto, arquivo), encoding = "UTF-8")
  env <- new.env(parent = globalenv())
  env$here <- function(...) file.path(projeto, ...)
  if (script) {
    inicio <- grep("^## ---- ", linhas)
    nomes <- sub("^## ---- (.*) ----$", "\\1", linhas[inicio])
    fim <- c(inicio[-1] - 1L, length(linhas))
  } else {
    inicio <- which(linhas == "```{r}")
    fim <- vapply(inicio, function(i) min(which(linhas == "```")[which(linhas == "```") > i]), integer(1))
    nomes <- vapply(inicio, function(i) sub("#| label: ", "", linhas[i + 1L], fixed = TRUE), character(1))
  }
  for (i in seq_along(inicio)) {
    if (nomes[i] == "instalar") next
    bloco <- linhas[seq.int(inicio[i] + 1L, fim[i] - as.integer(!script))]
    invisible(capture.output(eval(parse(text = bloco), env)))
  }
  env
}
grDevices::pdf(file.path(raiz, "diagnosticos_teste.pdf"))
script <- rodar(TRUE)
qmd <- rodar(FALSE)
grDevices::dev.off()
esperado <- calcular_anova(cache$base_0001$df, "peso_kg", "densidade", .99)
iguais <- function(x, y) isTRUE(all.equal(x, y, check.attributes = FALSE, tolerance = 1e-10))
for (env in list(script, qmd)) {
  stopifnot(iguais(as.data.frame(env$base_compartilhada), compartilhada),
    iguais(as.data.frame(env$base_da_anova), cache$base_0001$df),
    env$n_preparadas == 18L, env$n_excluidas == 1L, nrow(env$dados) == esperado$n,
    iguais(env$tabela_anova$`F value`[1], esperado$f_anova),
    iguais(env$tabela_anova$`Pr(>F)`[1], esperado$p_anova),
    iguais(unname(env$tukey[[1]]), unname(stats::TukeyHSD(esperado$fit, conf.level = .99)[[1]])))
}
stopifnot(iguais(script$resumo, qmd$resumo), identical(script$classe_efeito, qmd$classe_efeito),
          identical(script$frase_anova, qmd$frase_anova))
cat("OK: ZIP ANOVA, bases, exclusões, F, p, Tukey 99% e script/QMD equivalentes.\n")
cat("Projeto para Render:", normalizePath(projeto, winslash = "/"), "\n")
