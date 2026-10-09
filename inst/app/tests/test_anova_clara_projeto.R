# Execute de inst/app. Confere a opção experimental "código em ClaRa" do
# Projeto R da ANOVA de um fator: a árvore leva a ClaRa em R/clara/, o script
# e o relatório (um só QMD, o Word) usam as funções da ClaRa, cada um roda em
# sessão nova e reproduz
# os números da ANOVA. Confere também que, sem a opção, o projeto continua
# saindo pelo molde atual, e que Welch e automático também saem em ClaRa,
# com variancias_iguais escrito no script.
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

# A escolha da rota: ClaRa só com a opção marcada, em qualquer método com a
# escolha registrada (no automático, a que a tela fez: metodo_usado).
com_clara <- list(execucoes = list(execucao_0001 = item), secoes_globais = list(),
                  codigo_clara = TRUE)
sem_clara <- list(execucoes = list(execucao_0001 = item), secoes_globais = list())
item_welch <- item
item_welch$parametros$metodo <- "welch"
welch <- list(execucoes = list(execucao_0001 = item_welch), secoes_globais = list(),
              codigo_clara = TRUE)
item_auto <- item
item_auto$parametros$metodo <- "auto"
item_auto_sem <- item_auto
item_auto$parametros$metodo_usado <- "welch"
automatico <- list(execucoes = list(execucao_0001 = item_auto), secoes_globais = list(),
                   codigo_clara = TRUE)
automatico_sem <- list(execucoes = list(execucao_0001 = item_auto_sem), secoes_globais = list(),
                       codigo_clara = TRUE)
stopifnot(identical(exportacao_molde_projeto_entrada(com_clara)$pasta, "anova_clara"),
  identical(exportacao_molde_projeto_entrada(sem_clara)$pasta, "anova_projeto"),
  identical(exportacao_molde_projeto_entrada(welch)$pasta, "anova_clara"),
  identical(exportacao_molde_projeto_entrada(automatico)$pasta, "anova_clara"),
  # Automático sem a escolha registrada: a ClaRa não decide pelo Levene.
  identical(exportacao_molde_projeto_entrada(automatico_sem)$pasta, "anova_projeto"),
  identical(exportacao_anova_clara_metodo(item_auto), "welch"))

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
documentos <- "relatorio.qmd"
arquivos_clara <- c("clara.R", "clara_medias.R", "clara_medianas.R",
                    "clara_qualquer_analise.R", "clara_motor.R")
stopifnot(file.exists(script),
  file.exists(file.path(projeto, "R", "funcoes.R")),
  all(file.exists(file.path(projeto, "R", "clara", arquivos_clara))),
  file.exists(file.path(projeto, "_quarto.yml")),
  all(file.exists(file.path(projeto, "relatorios", documentos))))

# Script, QMDs e README sem marcadores sobrando; script e QMDs com as
# chamadas da ClaRa e os nomes das colunas sem aspas.
linhas_script <- readLines(script, encoding = "UTF-8")
readme <- readLines(file.path(projeto, "README.md"), encoding = "UTF-8")
stopifnot(!any(grepl("{{", c(linhas_script, readme), fixed = TRUE)),
  sum(grepl('source(here("R", "clara", "clara.R"), encoding = "UTF-8")', linhas_script, fixed = TRUE)) == 1L,
  any(grepl("resposta          = peso_g,", linhas_script, fixed = TRUE)),
  any(grepl("variancias_iguais = TRUE)  # TRUE: ANOVA clássica; FALSE: ANOVA de Welch", linhas_script, fixed = TRUE)),
  any(grepl("grupos            = racao,", linhas_script, fixed = TRUE)),
  any(grepl("grafico_medias(titulo           = NULL,", linhas_script, fixed = TRUE)),
  any(grepl("escrever_resultados(casas    = 1,", linhas_script, fixed = TRUE)),
  any(grepl('"multcompView"', readme, fixed = TRUE)),
  any(grepl('"effectsize"', readme, fixed = TRUE)),
  # Só CRAN: sem catalyser, EAPADados nem instalação pelo GitHub.
  !any(grepl("catalyser|EAPADados", linhas_script)),
  !any(grepl("install_github|\"remotes\"", readme)),
  # Receita e carimbo no lugar da fotografia: nada de .rds no projeto.
  !any(grepl("readRDS|all.equal", linhas_script)),
  # dados/ guarda só a planilha, sem subpastas.
  identical(list.files(file.path(projeto, "dados"), all.files = TRUE, no.. = TRUE),
            "isoproteica_bagre.xlsx"),
  any(grepl('here("dados", "isoproteica_bagre.xlsx")', linhas_script, fixed = TRUE)),
  !any(grepl("brutos/|\"brutos\"|processados", linhas_script)),
  any(grepl("# Carimbo: 19 linhas.", linhas_script, fixed = TRUE)),
  any(grepl("# Contagem por racao: A 5; B 5; C 4; D 5.", linhas_script, fixed = TRUE)),
  any(grepl('mutate(racao = factor(racao, levels = c("A", "B", "C", "D")))', linhas_script, fixed = TRUE)),
  sum(grepl("stopifnot(nrow(base) == 19L)", linhas_script, fixed = TRUE)) == 1L)
for (documento in documentos) {
  qmd <- readLines(file.path(projeto, "relatorios", documento), encoding = "UTF-8")
  stopifnot(!any(grepl("{{", qmd, fixed = TRUE)),
    !any(grepl("analise.R\"), encoding", qmd, fixed = TRUE)),
    !any(grepl("readRDS", qmd, fixed = TRUE)),
    any(grepl("read_excel(here(\"dados\", \"isoproteica_bagre.xlsx\")", qmd, fixed = TRUE)),
    any(grepl("stopifnot(nrow(base) == 19L)", qmd, fixed = TRUE)),
    any(grepl("comparar_medias(resposta          = peso_g,", qmd, fixed = TRUE)),
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
# Um só documento: o relatório Word, com a tabela da ANOVA e a figura
# principal, sem a figura dos pares, e com o chunk de preparo escondido. O
# caderno de estudo é o próprio R/analise.R: não há HTML nem ocean.scss.
relatorio <- readLines(file.path(projeto, "relatorios", "relatorio.qmd"), encoding = "UTF-8")
quarto_yml <- readLines(file.path(projeto, "_quarto.yml"), encoding = "UTF-8")
# O tamanho é acompanhado, não travado: várias análises e bases pedem mais.
cat(sprintf("TAMANHO: analise.R %d linhas; relatorio.qmd %d.\n",
            length(linhas_script), length(relatorio)))
stopifnot(identical(list.files(file.path(projeto, "relatorios"), pattern = "[.]qmd$"), "relatorio.qmd"),
  !file.exists(file.path(projeto, "relatorios", "ocean.scss")),
  any(grepl("- relatorios/relatorio.qmd", quarto_yml, fixed = TRUE)),
  !any(grepl("relatorio_completo|relatorio_artigo", quarto_yml)),
  !any(grepl("fig-pares", relatorio, fixed = TRUE)),
  any(grepl("label: tbl-anova", relatorio, fixed = TRUE)),
  any(grepl("label: fig-barras", relatorio, fixed = TRUE)),
  any(grepl("#| include: false", relatorio, fixed = TRUE)),
  # Seções numeradas em sequência, de 1 a 9.
  identical(as.integer(sub("^# ([0-9]+)[.] .*$", "\\1",
    grep("^# [0-9]+[.] .*-{3,}$", linhas_script, value = TRUE))), 1:9),
  any(grepl("# 7. Textos dinâmicos", linhas_script, fixed = TRUE)),
  # A planilha como tibble e a mesma olhada (glimpse) na planilha e na base.
  !any(grepl("as.data.frame", c(linhas_script, relatorio), fixed = TRUE)),
  !any(grepl("str(dados_brutos)", linhas_script, fixed = TRUE)),
  any(linhas_script == "glimpse(dados_brutos)"),
  # O ambiente fica com a ClaRa, não com writeLines/sessionInfo à vista.
  any(linhas_script == 'registrar_ambiente(arquivo = here("saida", "sessionInfo.txt"))'),
  !any(grepl("capture.output(sessionInfo())", linhas_script, fixed = TRUE)),
  # Os rótulos dizem ao aluno onde trocar.
  # Com rótulos escritos na tela, sem o exemplo; sem eles, com o exemplo.
  sum(endsWith(c(linhas_script, relatorio), '"Peso final (g)",  # no texto e na figura')) == 2L,
  identical(exportacao_anova_clara_marcadores(list(parametros = list(
    resposta = "peso_g", fator = "racao")))$NOTA_ROTULO_RESPOSTA,
    'no texto e na figura, ex.: "Peso final (g)"'),
  # Relatório: comentário HTML puro e a leitura da planilha em duas linhas.
  !any(grepl("{=html}", relatorio, fixed = TRUE)),
  any(relatorio == '                           sheet = "isoproteica_bagre")'),
  any(grepl("^textos\\$efeito$", linhas_script)),
  any(grepl("^textos\\$pressupostos$", linhas_script)),
  # Bibliografia e estilo só no cabeçalho do relatório, onde o aluno procura.
  !any(grepl("^(bibliography|csl):", quarto_yml)),
  any(grepl("^csl: apa.csl$", relatorio)),
  # Sem título escrito na tela, o relatório não herda os nomes crus das colunas.
  any(relatorio == 'title: "Título do trabalho (preencher)"'),
  !any(grepl("entre grupos de", relatorio, fixed = TRUE)),
  # Métodos da rota ClaRa: só a clássica com Tukey; nada de Welch no Word
  # (só o comentário do argumento, no chunk escondido, cita a alternativa).
  !any(grepl("Welch|Games-Howell|effectsizeConversao",
             relatorio[!grepl("^ +variancias_iguais = ", relatorio)])),
  any(grepl("SQ: soma de quadrados", relatorio, fixed = TRUE)),
  any(grepl("[@benshachar2020]", relatorio, fixed = TRUE)),
  # Os dois comentários dos rótulos na mesma coluna, no script e no relatório.
  length(unique(regexpr("#", grep("^ +rotulo_(resposta|grupos) += ", linhas_script, value = TRUE)))) == 1L,
  length(unique(regexpr("#", grep("^ +rotulo_(resposta|grupos) += ", relatorio, value = TRUE)))) == 1L,
  # Seção 2 sem a nota repetida do caminho e sem linha em branco dupla.
  !any(grepl("^# Entrada: ", linhas_script)),
  !any(!nzchar(head(linhas_script, -1)) & !nzchar(linhas_script[-1])),
  # README: a ClaRa na tabela do ambiente e o install.packages recuado.
  any(grepl("^\\| ClaRa \\| [0-9.]+ \\|", readme)),
  any(grepl('^  c\\("broom"', readme)))

# R/funcoes.R só com o que o projeto chama: sem moda(), converter_datas()
# nem tema_projeto(), que este projeto não usa.
funcoes <- readLines(file.path(projeto, "R", "funcoes.R"), encoding = "UTF-8")
definidas <- sub(" <- function.*$", "", grep("^[a-z_]+ <- function", funcoes, value = TRUE))
stopifnot(identical(sort(definidas), c("flextable_ocean", "fmt", "formatar_p")),
  !any(grepl("lubridate", c(funcoes, readme), fixed = TRUE)),
  any(grepl("1. Apresentação ...... fmt(), formatar_p(), flextable_ocean()", funcoes, fixed = TRUE)))
# Com moda() na receita, ela entra numa seção de preparo.
com_moda <- exportacao_clara_funcoes(
  readLines(file.path("templates", "regressao_linear", "funcoes.R"), encoding = "UTF-8"),
  c("base <- dados_brutos |>", "  mutate(peso_g = coalesce(peso_g, moda(peso_g)))",
    "fmt(1)"))
stopifnot(identical(sort(sub(" <- function.*$", "", grep("^[a-z_]+ <- function", com_moda, value = TRUE))),
                    c("fmt", "moda")),
  any(grepl("^# 2[.] Preparo -+$", com_moda)))

# Render, quando houver Quarto: só o Word, sem as mensagens de carga dos
# pacotes.
quarto_bin <- Sys.getenv("QUARTO_PATH", unname(Sys.which("quarto")))
if (!nzchar(quarto_bin) || !file.exists(quarto_bin)) {
  cat("LACUNA: quarto não encontrado; o Render não foi verificado.\n")
} else {
  render_log <- file.path(destino, "render.log")
  antigo <- setwd(projeto)
  status <- system2(quarto_bin, "render", stdout = render_log, stderr = render_log)
  setwd(antigo)
  if (status != 0L) stop(paste(readLines(render_log, warn = FALSE), collapse = "\n"))
  stopifnot(identical(list.files(file.path(projeto, "saida", "relatorios")), "relatorio.docx"))
  docx <- file.path(projeto, "saida", "relatorios", "relatorio.docx")
  pasta_docx <- file.path(destino, "docx")
  utils::unzip(docx, files = "word/document.xml", exdir = pasta_docx)
  texto_docx <- paste(readLines(file.path(pasta_docx, "word", "document.xml"),
                                encoding = "UTF-8", warn = FALSE), collapse = "")
  stopifnot(!grepl("here() starts", texto_docx, fixed = TRUE),
    !grepl("carregada", texto_docx, fixed = TRUE),
    !grepl("mascarados", texto_docx, fixed = TRUE))
}

# O script grava as cópias para compartilhar.
stopifnot(file.exists(file.path(projeto, "saida", "tabelas", "resumo_grupos.csv")),
  # A cópia da base para o Excel nasce da receita, com as 19 linhas.
  nrow(read.csv2(file.path(projeto, "saida", "tabelas", "base.csv"))) == 19L,
  file.exists(file.path(projeto, "saida", "figuras", "barras.png")),
  file.exists(file.path(projeto, "saida", "sessionInfo.txt")),
  # Todas as cópias, pelas funções da ClaRa (sem dir.create, lista nem for).
  setequal(list.files(file.path(projeto, "saida", "tabelas")),
           paste0(c("base", "resumo_grupos", "anova", "tukey", "testes_pressupostos",
                    "tamanho_efeito"), ".csv")),
  setequal(list.files(file.path(projeto, "saida", "figuras")),
           paste0(c("barras", "boxplot", "pares", "residuos", "qq"), ".png")),
  any(grepl("^salvar_tabelas[(]base += base,", linhas_script)),
  any(grepl("^salvar_figuras[(]barras += grafico_barras,", linhas_script)),
  !any(grepl("^for [(]|dir.create|write.csv2|ggsave", linhas_script)),
  any(grepl(paste("ClaRa", exportacao_versao_clara("templates")),
             readLines(file.path(projeto, "saida", "sessionInfo.txt"), n = 1))))

# Segundo caso: operação estrutural (renomear), trilha (reescalar) e um ramo
# com filtro. A receita encadeia tudo da planilha até o ramo, e o script e
# os QMDs chegam à mesma base e às mesmas médias da tela.
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
ramo <- list(id = "execucao_0001", tipo = "anova_um_fator", titulo = "Peso de tilápias por densidade",
  incluir_word = TRUE, estado_dependencia = "Atualizada",
  parametros = list(resposta = "peso_kg", fator = "densidade", nivel_confianca = .95, metodo = "classica"),
  base_id = "base_0001", base_objeto = "base_tanques", base_tipo = "derivada")
manifesto_ramo <- list(execucoes = list(execucao_0001 = ramo), secoes_globais = list(), codigo_clara = TRUE)
projeto_ramo <- exportacao_criar_projeto(destino = destino, nome_projeto = "tilapias_clara",
  dados_brutos = brutos, base_resolvida = resolvida, dados_analise = compartilhada,
  pipeline = trilha, base_externa = externa, registro_bases = bases, cache_bases = cache,
  registro_execucoes = manifesto_ramo$execucoes, manifesto = manifesto_ramo, revisao_origem = 1L,
  import_info = list(source = "package", package_dataset = "tilapias_teste"),
  templates_dir = "templates")
script_ramo <- readLines(file.path(projeto_ramo, "R", "analise.R"), encoding = "UTF-8")
stopifnot(any(grepl("rename(densidade = tratamento)", script_ramo, fixed = TRUE)),
  any(grepl("filter(tanque > 1)", script_ramo, fixed = TRUE)),
  any(grepl("stopifnot(nrow(base) == 18L)", script_ramo, fixed = TRUE)),
  !any(grepl("dplyr::", script_ramo, fixed = TRUE)))
esperada <- cache$base_0001$df
medias_ramo <- as.numeric(tapply(esperada$peso_kg, factor(esperada$densidade), mean, na.rm = TRUE))
entradas_ramo <- c(file.path(projeto_ramo, "R", "analise.R"), vapply(documentos, function(documento) {
  extraido <- file.path(destino, paste0("ramo_", documento, ".R"))
  knitr::purl(file.path(projeto_ramo, "relatorios", documento), output = extraido, quiet = TRUE)
  extraido
}, character(1)))
for (i in seq_along(entradas_ramo)) {
  verificador <- file.path(destino, paste0("validar_ramo_", i, ".R"))
  saida_rds <- file.path(destino, paste0("resultado_ramo_", i, ".rds"))
  log <- file.path(destino, paste0("execucao_ramo_", i, ".log"))
  writeLines(c(
    sprintf("setwd(%s)", literal(projeto_ramo)),
    "grDevices::pdf(NULL)",
    sprintf("source(%s, encoding = 'UTF-8')", literal(entradas_ramo[i])),
    sprintf("saveRDS(list(n = nrow(base), medias = resultado$resumo$media), %s)", literal(saida_rds))
  ), verificador, useBytes = TRUE)
  status <- system2(file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"),
    shQuote(verificador), stdout = log, stderr = log)
  if (status != 0L) stop(paste(readLines(log, warn = FALSE), collapse = "\n"))
  obtido <- readRDS(saida_rds)
  stopifnot(obtido$n == 18L, isTRUE(all.equal(obtido$medias, medias_ramo)))
}

# Terceiro caso: ANOVA de Welch, vinda da escolha automática da tela. O
# script e o relatório escrevem variancias_iguais = FALSE, falam de
# Games-Howell e reproduzem o oneway.test() em sessão nova.
projeto_welch <- exportacao_criar_projeto(destino = destino, nome_projeto = "anova_welch_clara",
  dados_brutos = bagres, base_resolvida = bagres, dados_analise = bagres,
  pipeline = list(), base_externa = NULL, registro_bases = list(), cache_bases = list(),
  registro_execucoes = automatico$execucoes, manifesto = automatico, revisao_origem = 1L,
  import_info = list(source = "package", package_dataset = "isoproteica_bagre"),
  templates_dir = "templates")
script_welch <- readLines(file.path(projeto_welch, "R", "analise.R"), encoding = "UTF-8")
relatorio_welch <- readLines(file.path(projeto_welch, "relatorios", "relatorio.qmd"), encoding = "UTF-8")
readme_welch <- readLines(file.path(projeto_welch, "README.md"), encoding = "UTF-8")
stopifnot(!any(grepl("{{", c(script_welch, relatorio_welch, readme_welch), fixed = TRUE)),
  any(grepl("variancias_iguais = FALSE)  # FALSE: ANOVA de Welch; TRUE: ANOVA clássica", script_welch, fixed = TRUE)),
  any(grepl("variancias_iguais = FALSE)", relatorio_welch, fixed = TRUE)),
  any(grepl("escolha automática da CatalyseR", script_welch, fixed = TRUE)),
  any(grepl("games_howell        = resultado$pares,", script_welch, fixed = TRUE)),
  !any(grepl("Tukey|SQ: soma", c(script_welch, relatorio_welch))),
  any(grepl("Games-Howell", relatorio_welch, fixed = TRUE)),
  any(grepl("@effectsizeConversao", relatorio_welch, fixed = TRUE)),
  any(grepl("games_howell.csv", readme_welch, fixed = TRUE)))
esperado_welch <- oneway.test(peso_g ~ racao, data = bagres, var.equal = FALSE)
entradas_welch <- c(file.path(projeto_welch, "R", "analise.R"), vapply(documentos, function(documento) {
  extraido <- file.path(destino, paste0("welch_", documento, ".R"))
  knitr::purl(file.path(projeto_welch, "relatorios", documento), output = extraido, quiet = TRUE)
  extraido
}, character(1)))
for (i in seq_along(entradas_welch)) {
  verificador <- file.path(destino, paste0("validar_welch_", i, ".R"))
  saida_rds <- file.path(destino, paste0("resultado_welch_", i, ".rds"))
  log <- file.path(destino, paste0("execucao_welch_", i, ".log"))
  writeLines(c(
    sprintf("setwd(%s)", literal(projeto_welch)),
    "grDevices::pdf(NULL)",
    sprintf("source(%s, encoding = 'UTF-8')", literal(entradas_welch[i])),
    sprintf("saveRDS(list(analise = resultado$nomes$analise, f = resultado$anova$f[1], gl = resultado$anova$gl, p = resultado$anova$p[1], letras = resultado$resumo$letra, textos = unclass(textos)), %s)", literal(saida_rds))
  ), verificador, useBytes = TRUE)
  status <- system2(file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"),
    shQuote(verificador), stdout = log, stderr = log)
  if (status != 0L) stop(paste(readLines(log, warn = FALSE), collapse = "\n"))
  obtido <- readRDS(saida_rds)
  stopifnot(identical(obtido$analise, "anova_welch"),
    isTRUE(all.equal(obtido$f, unname(esperado_welch$statistic))),
    isTRUE(all.equal(obtido$gl, unname(esperado_welch$parameter))),
    isTRUE(all.equal(obtido$p, esperado_welch$p.value)),
    all(nzchar(obtido$letras)),
    grepl("análise de variância de Welch", obtido$textos$teste, fixed = TRUE),
    grepl("Games-Howell", obtido$textos$comparacoes, fixed = TRUE))
}

# Sem %in% na rota ClaRa: is.element() e um comentário que diz o que fica.
sem_in <- exportacao_clara_sem_in(c(
  "base_compartilhada <- dados_brutos |>",
  "  # Mantém somente as linhas que atendem à condição.",
  "  filter(especie %in% c(\"tambaqui\", \"pirarucu\", \"tilápia\")) |>",
  "  # Calcula ou transforma: alimentado.",
  "  mutate(alimentado = as.integer(racao %in% c(\"A\", \"B\")))"))
stopifnot(!any(grepl("%in%", sem_in, fixed = TRUE)),
  identical(sem_in[2], "  # Ficam só as linhas em que especie é \"tambaqui\", \"pirarucu\" ou \"tilápia\"; as demais saem."),
  identical(sem_in[3], "  filter(is.element(especie, c(\"tambaqui\", \"pirarucu\", \"tilápia\"))) |>"),
  identical(sem_in[4], "  # alimentado vale 1 quando racao é \"A\" ou \"B\", e 0 nos outros casos."))
sem_in_solta <- exportacao_clara_sem_in(c("# Níveis escolhidos na importação",
  "dados <- dados |> filter(especie %in% c(\"tambaqui\"))"))
stopifnot(length(sem_in_solta) == 3L, sem_in_solta[1] == "# Níveis escolhidos na importação",
  sem_in_solta[2] == "# Ficam só as linhas em que especie é \"tambaqui\"; as demais saem.")

cat("OK: rota ClaRa escolhida com a opção, na clássica, no Welch e no automático; ClaRa copiada em R/clara/; script e o relatório Word em ClaRa, sem marcadores, cada um em sessão nova, com a ANOVA e o Tukey reproduzidos.\n")
cat("PROJETO:", projeto, "\n")
