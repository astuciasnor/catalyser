# Execute de inst/app. Equivalente do test_anova_molde_projeto.R para o teste
# t de duas amostras: a árvore do molde, as regras da rodada 4 (pacotes com
# checagem amigável, seção 3 em etapas 3.1–3.4, README com a mesma lista da
# seção 1, títulos-pergunta, pressupostos como subseção da Exploração antes
# dos Resultados, sem saída bruta nos relatórios, referências cruzadas sem
# "??" no HTML) e os números do teste t em processos R independentes.
grDevices::pdf(NULL)
for (arquivo in c("registro_tratamentos.R", "registro_bases.R", "registro_execucoes.R",
                  "registro_comunicacao.R", "exportacao_comunicacao.R")) {
  source(file.path("modules", arquivo), encoding = "UTF-8")
}

item <- list(id = "execucao_0001", tipo = "teste_t_two_ind",
  titulo = "Comprimento do cefalotorax por sexo",
  incluir_word = TRUE, estado_dependencia = "Atualizada", base_tipo = "compartilhada",
  base_id = "dados_analise", base_objeto = "dados_analise",
  parametros = list(resposta = "comprimento_cefalotorax_mm", grupo = "sexo",
    alternativa = "two.sided", nivel_confianca = .95, variancias_iguais = FALSE,
    rotulo_x = "Sexo", rotulo_y = "Comprimento do cefalotorax (mm)",
    titulo_grafico = ""),
  saidas_word = c("narrativa", "tabela", "grafico", "pressupostos"))
lagostas <- as.data.frame(EAPADados::lagostas_kelp_sexo)

destino <- Sys.getenv("CATALYSER_TESTE_TESTET_DESTINO", unset = tempfile("teste_t_"))
dir.create(destino, recursive = TRUE, showWarnings = FALSE)
manifesto <- list(execucoes = list(execucao_0001 = item), secoes_globais = list())
projeto <- exportacao_criar_projeto(destino = destino, nome_projeto = "teste_t",
  dados_brutos = lagostas, base_resolvida = lagostas, dados_analise = lagostas,
  pipeline = list(), base_externa = NULL, registro_bases = list(), cache_bases = list(),
  registro_execucoes = manifesto$execucoes, manifesto = manifesto, revisao_origem = 1L,
  import_info = list(source = "package", package_dataset = "lagostas_kelp_sexo"),
  templates_dir = "templates")

# Árvore do molde e funcoes.R sem conferir_base (a conferência é do pacote).
script <- file.path(projeto, "R", "analise.R")
documentos <- c("relatorio_completo.qmd", "relatorio_artigo.qmd")
stopifnot(file.exists(script),
  file.exists(file.path(projeto, "R", "funcoes.R")),
  file.exists(file.path(projeto, "_quarto.yml")),
  file.exists(file.path(projeto, "relatorios", "apa.csl")),
  dir.exists(file.path(projeto, "imagens")),
  !file.exists(file.path(projeto, "relatorios", "relatorio.qmd")))
funcoes <- readLines(file.path(projeto, "R", "funcoes.R"), encoding = "UTF-8")
stopifnot(!any(grepl("conferir_codigo", funcoes, fixed = TRUE)),
  !any(grepl("conferir_base", funcoes, fixed = TRUE)))

# Script: marcadores, source único, resumo_console no script (nunca nos
# relatórios) e catalyser/EAPADados com checagem amigável na seção 1.
linhas_script <- readLines(script, encoding = "UTF-8")
stopifnot(!any(grepl("{{", linhas_script, fixed = TRUE)),
  !any(grepl("^## ---- ", linhas_script)),
  sum(grepl('source(here::here("R", "funcoes.R")', linhas_script, fixed = TRUE)) == 1L,
  any(grepl("resumo_console", linhas_script, fixed = TRUE)),
  sum(grepl("library(catalyser)", linhas_script, fixed = TRUE)) == 1L,
  sum(grepl("library(EAPADados)", linhas_script, fixed = TRUE)) == 1L,
  any(grepl('!requireNamespace("catalyser", quietly = TRUE)', linhas_script, fixed = TRUE)),
  any(grepl('!requireNamespace("EAPADados", quietly = TRUE)', linhas_script, fixed = TRUE)),
  any(grepl("remotes::install_github('astuciasnor/catalyser')", linhas_script, fixed = TRUE)),
  any(grepl("remotes::install_github('astuciasnor/EAPADados')", linhas_script, fixed = TRUE)))
objetos_contrato <- c("n_total", "n_utilizado", "n_excluido", "texto_amostra",
  "texto_sintese_estatistica", "alerta_modelo", "registro_ambiente",
  "ic_percentual", "resumo_console", "teste_t", "d_cohen",
  "tabela_descritiva_exibir", "tabela_teste", "tabela_pressupostos",
  "texto_resultado", "texto_efeito", "texto_pressupostos", "texto_welch")
stopifnot(all(vapply(objetos_contrato, function(nome)
  any(grepl(nome, linhas_script, fixed = TRUE)), logical(1))))

# Seção 1 é a única portadora de library(); a seção 3 segue as etapas 3.1–3.4
# (uma leitura por RDS, conferência com catalyser_conferir_base, sem régua de
# banner no meio da seção).
linha_secao2 <- grep("^# 2\\. Definir as escolhas", linhas_script)
linha_secao4 <- grep("^# 4\\. Explorar", linhas_script)
stopifnot(length(linha_secao2) == 1L, length(linha_secao4) == 1L)
bibliotecas <- grep("^library\\(", linhas_script)
stopifnot(all(bibliotecas < linha_secao2))
secao3 <- linhas_script[seq.int(linha_secao2 + 1L, linha_secao4 - 1L)]
stopifnot(
  sum(grepl('readRDS(here("dados", "processados", "base_compartilhada.rds"))',
    secao3, fixed = TRUE)) == 1L,
  any(grepl("base_reconstruida", secao3, fixed = TRUE)),
  any(grepl("catalyser_conferir_base(", secao3, fixed = TRUE)),
  any(grepl("dados_da_analise <- dados_analise", secao3, fixed = TRUE)),
  any(grepl("# 3.1 Reconstruir.", secao3, fixed = TRUE)),
  any(grepl("# 3.2 Conferir.", secao3, fixed = TRUE)),
  any(grepl("# 3.3 Adotar.", secao3, fixed = TRUE)),
  any(grepl("# 3.4 Base desta análise.", secao3, fixed = TRUE)),
  !any(grepl("^# ={3,}", secao3)),
  !any(grepl("^library\\(", secao3)))

# QMDs: executam o script, não leem arquivos de dados, não sobram marcadores
# e não exibem a saída bruta de console.
for (documento in documentos) {
  qmd <- readLines(file.path(projeto, "relatorios", documento), encoding = "UTF-8")
  stopifnot(!any(grepl("{{", qmd, fixed = TRUE)),
    any(grepl('source(here::here("R", "analise.R"), encoding = "UTF-8")', qmd, fixed = TRUE)),
    !any(grepl("read.csv|readRDS|ggsave", qmd)),
    !any(grepl("resumo_console", qmd, fixed = TRUE)))
}

# README: lista do CRAN (incluindo remotes) igual à seção 1 do script e os
# dois pacotes do GitHub.
readme <- readLines(file.path(projeto, "README.md"), encoding = "UTF-8")
stopifnot(any(grepl("Preparar o computador", readme, fixed = TRUE)),
  any(grepl("install.packages(", readme, fixed = TRUE)),
  any(grepl("Nenhum pacote é instalado automaticamente", readme, fixed = TRUE)),
  any(grepl('remotes::install_github("astuciasnor/catalyser")', readme, fixed = TRUE)),
  any(grepl('remotes::install_github("astuciasnor/EAPADados")', readme, fixed = TRUE)),
  # Padrão do barbo (C10): tabela de três colunas, seção de reprodutibilidade
  # e origem dos dados com guia.
  any(grepl("| No script R | No relatório | Cópia salva para compartilhar |", readme, fixed = TRUE)),
  any(grepl("## Repro", readme, fixed = TRUE)),
  any(grepl("## Origem dos dados", readme, fixed = TRUE)),
  any(grepl("Registre aqui a origem da planilha, a licença e o período de coleta", readme, fixed = TRUE)))
pacotes_secao1 <- sub("^library\\((.*?)\\).*$", "\\1",
  grep("^library\\(", linhas_script, value = TRUE))
pacotes_secao1 <- setdiff(pacotes_secao1, c("catalyser", "EAPADados"))
inicio <- grep("install.packages(", readme, fixed = TRUE)[1]
fim <- inicio + which(readme[seq.int(inicio + 1L, length(readme))] == ")")[1]
bloco <- paste(readme[seq.int(inicio, fim)], collapse = " ")
pacotes_readme <- gsub('"', "", unlist(regmatches(bloco,
  gregexpr('"[A-Za-z][A-Za-z0-9.]*"', bloco, perl = TRUE)), use.names = FALSE))
stopifnot(identical(sort(pacotes_readme),
  sort(unique(c(pacotes_secao1, "remotes")))))

# Regras do molde: títulos-pergunta; Pressupostos é subseção (##) da
# Exploração, antes dos Resultados; sem "A saída bruta"; cada tabela e figura
# numerada é citada com @tbl-.../@fig-... no texto.
completo <- readLines(file.path(projeto, "relatorios", "relatorio_completo.qmd"),
  encoding = "UTF-8")
titulos <- grep("^#+ ", completo, value = TRUE)
pos_exploracao <- which(grepl("^# Exploração: conhecer os grupos antes do teste", titulos))[1]
pos_pressupostos <- which(grepl("^## Pressupostos", titulos))[1]
pos_resultados <- which(grepl("^# Resultados: o que o teste t responde?", titulos))[1]
stopifnot(length(pos_pressupostos) == 1L,
  pos_exploracao < pos_pressupostos, pos_pressupostos < pos_resultados,
  !any(grepl("^# Pressupostos", titulos)),
  !any(grepl("A saída bruta", completo, fixed = TRUE)),
  any(grepl("^# Introdução: qual comparação queremos investigar?", titulos)),
  any(grepl("^# Material e métodos: o que entrou na comparação?", titulos)),
  any(grepl("^# Discussão: voltar à pergunta biológica", titulos)),
  !any(grepl("layout-ncol", completo, fixed = TRUE)),
  !any(grepl("fig-subcap", completo, fixed = TRUE)))
rotulos_citados <- c("tbl-descritiva", "fig-caixa", "tbl-pressupostos",
  "tbl-teste", "fig-medias")
stopifnot(all(vapply(rotulos_citados, function(rotulo)
  any(grepl(paste0("@", rotulo), completo, fixed = TRUE)), logical(1))))
rotulos_artigo <- c("tbl-descritiva", "tbl-teste", "fig-medias")
artigo <- readLines(file.path(projeto, "relatorios", "relatorio_artigo.qmd"),
  encoding = "UTF-8")
stopifnot(all(vapply(rotulos_artigo, function(rotulo)
  any(grepl(paste0("@", rotulo), artigo, fixed = TRUE)), logical(1))))

# Script e cada QMD rodam em processos R independentes, reproduzindo o teste
# t da CatalyseR (a escolha Student/Welch segue o Levene, como no script).
formula_teste <- comprimento_cefalotorax_mm ~ sexo
p_levene <- car::leveneTest(formula_teste, data = lagostas)[["Pr(>F)"]][1]
variancias_iguais <- !is.na(p_levene) && p_levene >= (1 - .95)
esperado <- t.test(formula_teste, data = lagostas, var.equal = variancias_iguais)
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
    "stopifnot(is.data.frame(tabela_teste), inherits(grafico_caixa, 'ggplot'))",
    sprintf("saveRDS(list(t = unname(teste_t$statistic), p = teste_t$p.value, d = d_cohen, estimativas = unname(teste_t$estimate), n = n_utilizado), %s)", literal(resultado))
  ), verificador, useBytes = TRUE)
  status <- system2(file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"),
    shQuote(verificador), stdout = log, stderr = log)
  if (status != 0L) stop(paste(readLines(log, warn = FALSE), collapse = "\n"))
  obtido <- readRDS(resultado)
  completos <- complete.cases(lagostas[c("comprimento_cefalotorax_mm", "sexo")])
  stopifnot(isTRUE(all.equal(obtido$t, unname(esperado$statistic))),
    isTRUE(all.equal(obtido$p, esperado$p.value)),
    isTRUE(all.equal(obtido$estimativas, unname(esperado$estimate))),
    obtido$n == sum(completos))
}

# Render do caderno HTML: as referências cruzadas resolvem, sem nenhum "??"
# no lugar de número ou legenda.
quarto_bin <- Sys.getenv("QUARTO_PATH", unname(Sys.which("quarto")))
if (!nzchar(quarto_bin) || !file.exists(quarto_bin)) {
  cat("LACUNA: quarto não encontrado; o render do HTML não foi verificado.\n")
} else {
  antigo <- getwd()
  on.exit(setwd(antigo), add = TRUE)
  setwd(projeto)
  render_log <- file.path(destino, "render_completo.log")
  status <- system2(quarto_bin, c("render", shQuote(file.path("relatorios", "relatorio_completo.qmd"))),
    stdout = render_log, stderr = render_log)
  if (status != 0L) stop(paste(readLines(render_log, warn = FALSE), collapse = "\n"))
  html <- readLines(file.path(projeto, "saida", "relatorios", "relatorio_completo.html"),
    encoding = "UTF-8", warn = FALSE)
  stopifnot(!any(grepl("??", html, fixed = TRUE)))
}

cat("OK: árvore do molde teste t, pacotes com checagem amigável, seção 3 em etapas 3.1–3.4, README com a mesma lista da seção 1, títulos-pergunta, pressupostos como subseção da Exploração antes dos Resultados, sem saída bruta nos relatórios, referências cruzadas sem \"??\" no HTML, e script + dois QMDs independentes com os números do teste t.\n")
cat("PROJETO:", projeto, "\n")
