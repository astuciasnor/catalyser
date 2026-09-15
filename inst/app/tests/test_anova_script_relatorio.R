# O script fornece o código ao relatório; editar o texto não altera a análise.
# Rodar a partir de inst/app. Carrega só o núcleo de exportação, sem abrir a IDE.
for (arquivo in c("registro_tratamentos.R", "registro_bases.R",
                  "registro_execucoes.R", "registro_comunicacao.R",
                  "exportacao_comunicacao.R")) {
  source(file.path("modules", arquivo), encoding = "UTF-8")
}

verificar_par_anova <- function() {
  raiz <- tempfile("anova_script_relatorio_")
  dir.create(raiz)
  on.exit(unlink(raiz, recursive = TRUE), add = TRUE)
  brutos <- data.frame(
    peso_g = c(102, 107, 98, 110, 105, 125, 130, 118, 128, 123, 144, 151, 139, 148, 145),
    especie = rep(c("bagre", "corvina", "pescada"), each = 5)
  )
  execucao <- list(id = "execucao_0001", tipo = "anova_um_fator",
    titulo = "Peso entre espécies", incluir_word = TRUE, estado_dependencia = "Atualizada",
    parametros = list(resposta = "peso_g", fator = "especie", nivel_confianca = 0.95),
    base_tipo = "compartilhada", base_id = "dados_analise",
    saidas_word = c("narrativa", "tabela", "grafico"))
  manifesto <- list(execucoes = list(execucao_0001 = execucao),
    secoes_globais = list(introducao = "Texto científico preservado na atualização."))
  argumentos <- list(nome_projeto = "estudo_pesca", dados_brutos = brutos,
    base_resolvida = brutos, dados_analise = brutos, pipeline = list(), base_externa = NULL,
    registro_bases = list(), cache_bases = list(), registro_execucoes = manifesto$execucoes,
    manifesto = manifesto, revisao_origem = 1L,
    import_info = list(source = "local", file_name = "pesca.xlsx", excel_sheet = "pesca"),
    templates_dir = "templates")
  zip <- file.path(raiz, "projeto.zip")
  do.call(exportacao_empacotar_projeto, c(list(file = zip), argumentos))
  utils::unzip(zip, exdir = raiz)
  projeto <- file.path(raiz, "estudo_pesca")
  qmd <- file.path(projeto, "relatorios", "relatorio.qmd")
  script <- file.path(projeto, "R", "analise.R")
  funcoes <- new.env(parent = globalenv())
  sys.source(file.path(projeto, "R", "funcoes.R"), envir = funcoes)
  stopifnot(isTRUE(funcoes$conferir_codigo(qmd, script)))
  texto <- readLines(qmd, encoding = "UTF-8")
  chunks <- funcoes$chunks_do_relatorio(texto)
  stopifnot(length(funcoes$chunks_com_fonte(chunks)) == 15L,
    all(vapply(chunks, function(ch) !is.na(ch$fonte) ||
      ch$rotulo %in% funcoes$chunks_de_manutencao, logical(1))))

  # Comentários novos continuam no material de estudo, sem invalidar o relatório.
  original <- readLines(script, encoding = "UTF-8")
  writeLines(c("# Comentário de estudo acrescentado pelo pesquisador.", original), script, useBytes = TRUE)
  stopifnot(isTRUE(funcoes$conferir_codigo(qmd, script)))

  # Uma mudança real no preparo precisa chegar ao relatório antes do Render.
  mudado <- sub("base_da_anova <- dados",
    "dados$peso_g <- dados$peso_g + 10\nbase_da_anova <- dados", original, fixed = TRUE)
  writeLines(mudado, script, useBytes = TRUE)
  erro <- tryCatch(funcoes$conferir_codigo(qmd, script), error = conditionMessage)
  stopifnot(is.character(erro), grepl("preparar-analise", erro, fixed = TRUE))
  suppressMessages(funcoes$atualizar_codigo(qmd, script))
  stopifnot(isTRUE(funcoes$conferir_codigo(qmd, script)))
  novo <- readLines(qmd, encoding = "UTF-8")
  stopifnot(identical(grep("^#\\|", texto, value = TRUE), grep("^#\\|", novo, value = TRUE)),
    any(grepl("Texto científico preservado na atualização.", novo, fixed = TRUE)),
    !any(grepl("Comentário de estudo acrescentado", novo, fixed = TRUE)))

  # Executa o código copiado: as médias precisam aumentar exatamente dez gramas.
  ambiente <- new.env(parent = funcoes)
  ambiente$dados_brutos <- brutos
  ambiente$dados <- readRDS(file.path(projeto, "dados/processados/base_compartilhada.rds"))
  ambiente$here <- function(...) file.path(projeto, ...)
  ambiente$nivel_confianca <- 0.95
  suppressPackageStartupMessages(library(dplyr))
  ch <- Filter(function(ch) identical(ch$rotulo, "preparo"), funcoes$chunks_do_relatorio(novo))[[1]]
  invisible(capture.output(eval(parse(text = novo[ch$ini:ch$fim]), ambiente)))
  stopifnot(isTRUE(all.equal(ambiente$dados$peso_g, brutos$peso_g + 10)))

  # Uma execução fora do relatório continua guardada no projeto completo.
  outra <- execucao
  outra$id <- "execucao_0002"
  outra$incluir_word <- FALSE
  argumentos$manifesto$execucoes$execucao_0002 <- outra
  argumentos$registro_execucoes <- argumentos$manifesto$execucoes
  argumentos$nome_projeto <- "estudo_com_acervo"
  stopifnot(!exportacao_anova_simples(argumentos$manifesto))
  completo <- do.call(exportacao_criar_projeto, c(list(destino = raiz), argumentos))
  acervo <- readLines(file.path(completo, "R", "analise.R"), encoding = "UTF-8")
  stopifnot(!dir.exists(file.path(completo, "metadados")),
    any(grepl("execucao-0002-analisar", acervo, fixed = TRUE)))
  cat("OK: ZIP, atualização do código, preservação do texto e acervo de execuções.\n")
}

verificar_par_anova()
