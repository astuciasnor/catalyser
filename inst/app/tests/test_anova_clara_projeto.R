# Execute de inst/app. Confere a opção experimental "código em ClaRa" do
# Projeto R da ANOVA de um fator: a árvore leva a ClaRa em R/, o script e os
# dois QMDs usam as funções da ClaRa, cada um roda em sessão nova e reproduz
# os números da ANOVA. Confere também que, sem a opção ou com Welch, o
# projeto continua saindo pelo molde atual.
grDevices::pdf(NULL)
for (arquivo in c("registro_tratamentos.R", "registro_bases.R", "registro_execucoes.R",
                  "registro_comunicacao.R", "exportacao_comunicacao.R")) {
  source(file.path("modules", arquivo), encoding = "UTF-8")
}

item <- list(id = "execucao_0001", tipo = "anova_um_fator", titulo = "Peso final de bagres por racao",
  incluir_word = TRUE, estado_dependencia = "Atualizada", base_tipo = "compartilhada",
  base_id = "dados_analise", base_objeto = "dados_analise",
  parametros = list(resposta = "peso_g", fator = "racao", nivel_confianca = .95,
    metodo = "classica", ajuste_comparacoes = "tukey", tema = "minimal", titulo_grafico = "",
    rotulo_x = "Ração", rotulo_y = "Peso final (g)"),
  saidas_word = c("narrativa", "descritivos", "tabela", "comparacoes",
                  "grafico", "pressupostos", "diagnosticos"))
bagres <- as.data.frame(EAPADados::isoproteica_bagre)

# A escolha da rota: ClaRa só com a opção marcada e o método clássico.
com_clara <- list(execucoes = list(execucao_0001 = item), secoes_globais = list(),
                  codigo_clara = TRUE)
sem_clara <- list(execucoes = list(execucao_0001 = item), secoes_globais = list())
item_welch <- item
item_welch$parametros$metodo <- "welch"
welch <- list(execucoes = list(execucao_0001 = item_welch), secoes_globais = list(),
              codigo_clara = TRUE)
stopifnot(identical(exportacao_molde_projeto_entrada(com_clara)$pasta, "anova_clara"),
  identical(exportacao_molde_projeto_entrada(sem_clara)$pasta, "anova_projeto"),
  identical(exportacao_molde_projeto_entrada(welch)$pasta, "anova_projeto"))

destino <- Sys.getenv("CATALYSER_TESTE_CLARA_DESTINO", unset = tempfile("anova_clara_"))
dir.create(destino, recursive = TRUE, showWarnings = FALSE)
projeto <- exportacao_criar_projeto(destino = destino, nome_projeto = "anova_clara",
  dados_brutos = bagres, base_resolvida = bagres, dados_analise = bagres,
  pipeline = list(), base_externa = NULL, registro_bases = list(), cache_bases = list(),
  registro_execucoes = com_clara$execucoes, manifesto = com_clara, revisao_origem = 1L,
  import_info = list(source = "package", package_dataset = "isoproteica_bagre"),
  templates_dir = "templates")

# Árvore: a ClaRa inteira em R/, ao lado do script e das funções de apresentação.
script <- file.path(projeto, "R", "analise.R")
documentos <- c("relatorio_completo.qmd", "relatorio_artigo.qmd")
arquivos_clara <- c("clara.R", "clara_medias.R", "clara_medianas.R",
                    "clara_qualquer_analise.R", "clara_motor.R")
stopifnot(file.exists(script),
  file.exists(file.path(projeto, "R", "funcoes.R")),
  all(file.exists(file.path(projeto, "R", arquivos_clara))),
  file.exists(file.path(projeto, "_quarto.yml")),
  all(file.exists(file.path(projeto, "relatorios", documentos))))

# Script, QMDs e README sem marcadores sobrando; script e QMDs com as
# chamadas da ClaRa e os nomes das colunas sem aspas.
linhas_script <- readLines(script, encoding = "UTF-8")
readme <- readLines(file.path(projeto, "README.md"), encoding = "UTF-8")
stopifnot(!any(grepl("{{", c(linhas_script, readme), fixed = TRUE)),
  sum(grepl('source(here("R", "clara.R"), encoding = "UTF-8")', linhas_script, fixed = TRUE)) == 1L,
  any(grepl("resposta        = peso_g,", linhas_script, fixed = TRUE)),
  any(grepl("grupos          = racao,", linhas_script, fixed = TRUE)),
  any(grepl("grafico_medias(titulo           = NULL,", linhas_script, fixed = TRUE)),
  any(grepl("escrever_resultados()", linhas_script, fixed = TRUE)),
  any(grepl('"multcompView"', readme, fixed = TRUE)),
  any(grepl('"effectsize"', readme, fixed = TRUE)),
  # Só CRAN: sem catalyser, EAPADados nem instalação pelo GitHub.
  !any(grepl("catalyser|EAPADados", linhas_script)),
  !any(grepl("install_github|\"remotes\"", readme)),
  any(grepl("all.equal(lapply(base_reconstruida, as.character), lapply(fotografia, as.character))",
            linhas_script, fixed = TRUE)),
  # O roteiro cabe em 200 linhas, com comentários.
  length(linhas_script) <= 200L)
for (documento in documentos) {
  qmd <- readLines(file.path(projeto, "relatorios", documento), encoding = "UTF-8")
  stopifnot(!any(grepl("{{", qmd, fixed = TRUE)),
    !any(grepl("analise.R\"), encoding", qmd, fixed = TRUE)),
    any(grepl("comparar_medias(resposta        = peso_g,", qmd, fixed = TRUE)),
    any(grepl("transmute(`Ração` = racao,", qmd, fixed = TRUE)),
    any(grepl("`r textos$teste`", qmd, fixed = TRUE)))
}

# Script e cada QMD rodam em processos R independentes, como no Render, e
# reproduzem a ANOVA e o Tukey calculados direto com o R.
esperado <- aov(peso_g ~ racao, data = bagres)
sumario <- summary(esperado)[[1]]
tukey_esperado <- TukeyHSD(esperado, conf.level = .95)$racao
entradas <- c(script, vapply(documentos, function(documento) {
  extraido <- file.path(destino, paste0(documento, ".R"))
  knitr::purl(file.path(projeto, "relatorios", documento), output = extraido, quiet = TRUE)
  extraido
}, character(1)))
literal <- function(x) encodeString(normalizePath(x, winslash = "/", mustWork = FALSE), quote = '"')
for (i in seq_along(entradas)) {
  verificador <- file.path(destino, paste0("validar_", i, ".R"))
  saida_rds <- file.path(destino, paste0("resultado_", i, ".rds"))
  log <- file.path(destino, paste0("execucao_", i, ".log"))
  writeLines(c(
    sprintf("setwd(%s)", literal(projeto)),
    "grDevices::pdf(NULL)",
    sprintf("source(%s, encoding = 'UTF-8')", literal(entradas[i])),
    "stopifnot(inherits(resultado, 'clara_medias'), inherits(textos, 'clara_textos'))",
    # No script, a conferência da base precisa dar TRUE.
    if (i == 1L) "stopifnot(isTRUE(all.equal(lapply(base_reconstruida, as.character), lapply(fotografia, as.character))))",
    sprintf("saveRDS(list(f = resultado$anova$f[1], p = resultado$anova$p[1], n = resultado$amostra$usadas, medias = resultado$resumo$media, pares = resultado$pares, textos = unclass(textos)), %s)", literal(saida_rds))
  ), verificador, useBytes = TRUE)
  status <- system2(file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"),
    shQuote(verificador), stdout = log, stderr = log)
  if (status != 0L) stop(paste(readLines(log, warn = FALSE), collapse = "\n"))
  obtido <- readRDS(saida_rds)
  stopifnot(isTRUE(all.equal(obtido$f, unname(sumario$`F value`[1]))),
    isTRUE(all.equal(obtido$p, unname(sumario$`Pr(>F)`[1]))),
    obtido$n == 19L,
    isTRUE(all.equal(obtido$medias, as.numeric(tapply(bagres$peso_g, bagres$racao, mean)))),
    isTRUE(all.equal(obtido$pares$diferenca, unname(tukey_esperado[, "diff"]))),
    isTRUE(all.equal(obtido$pares$ic_inf, unname(tukey_esperado[, "lwr"]))),
    all(nzchar(unlist(obtido$textos[c("amostra", "teste", "efeito", "sintese")]))))
}
# O caderno cabe em 300 linhas; o Word traz a tabela da ANOVA e a figura
# principal, sem a figura dos pares, e esconde o chunk de preparo.
completo <- readLines(file.path(projeto, "relatorios", "relatorio_completo.qmd"), encoding = "UTF-8")
artigo <- readLines(file.path(projeto, "relatorios", "relatorio_artigo.qmd"), encoding = "UTF-8")
stopifnot(length(completo) <= 300L,
  !any(grepl("fig-pares", artigo, fixed = TRUE)),
  any(grepl("label: tbl-anova", artigo, fixed = TRUE)),
  any(grepl("label: fig-barras", artigo, fixed = TRUE)),
  any(grepl("#| include: false", artigo, fixed = TRUE)))

# Render dos dois documentos, quando houver Quarto: sem "??" no HTML e sem as
# mensagens de carga dos pacotes no Word.
quarto_bin <- Sys.getenv("QUARTO_PATH", unname(Sys.which("quarto")))
if (!nzchar(quarto_bin) || !file.exists(quarto_bin)) {
  cat("LACUNA: quarto não encontrado; o Render não foi verificado.\n")
} else {
  render_log <- file.path(destino, "render.log")
  antigo <- setwd(projeto)
  status <- system2(quarto_bin, "render", stdout = render_log, stderr = render_log)
  setwd(antigo)
  if (status != 0L) stop(paste(readLines(render_log, warn = FALSE), collapse = "\n"))
  html <- readLines(file.path(projeto, "saida", "relatorios", "relatorio_completo.html"),
                    encoding = "UTF-8", warn = FALSE)
  docx <- file.path(projeto, "saida", "relatorios", "relatorio_artigo.docx")
  pasta_docx <- file.path(destino, "docx")
  utils::unzip(docx, files = "word/document.xml", exdir = pasta_docx)
  texto_docx <- paste(readLines(file.path(pasta_docx, "word", "document.xml"),
                                encoding = "UTF-8", warn = FALSE), collapse = "")
  stopifnot(!any(grepl("??", html, fixed = TRUE)),
    !grepl("here() starts", texto_docx, fixed = TRUE),
    !grepl("carregada", texto_docx, fixed = TRUE),
    !grepl("mascarados", texto_docx, fixed = TRUE))
}

# O script grava as cópias para compartilhar.
stopifnot(file.exists(file.path(projeto, "saida", "tabelas", "resumo_grupos.csv")),
  file.exists(file.path(projeto, "saida", "figuras", "barras.png")),
  file.exists(file.path(projeto, "saida", "sessionInfo.txt")))

cat("OK: rota ClaRa escolhida só com a opção e o método clássico; ClaRa copiada em R/; script e dois QMDs em ClaRa, sem marcadores, cada um em sessão nova, com a ANOVA e o Tukey reproduzidos.\n")
cat("PROJETO:", projeto, "\n")
