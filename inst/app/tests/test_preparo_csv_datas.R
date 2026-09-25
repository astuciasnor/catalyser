# Casos novos da revisão da v1: CSV e datas, incluindo saídas fora da IDE.
invisible(Sys.setlocale("LC_ALL", "English_United States.utf8"))
source("app.R", encoding = "UTF-8")
source("tests/carregar_catalyser.R", encoding = "UTF-8")
library(dplyr)
library(tidyr)
arquivos <- normalizePath("../../docs/testes/fases-3/arquivos_preparo_v1", winslash = "/")
iguais <- function(a, b) isTRUE(all.equal(a, b, check.attributes = TRUE))
falha <- function(expr) inherits(tryCatch(force(expr), error = identity), "error")

esperadas <- as.Date(c("2026-09-01", "2026-09-02", NA, "2026-09-04",
                      "2026-09-05", "2026-09-06", "2026-09-07", "2026-09-08"))
stopifnot(iguais(preparo_converter_data(c("12/09/2026", "2026-09-12", "", NA)),
                as.Date(c("2026-09-12", "2026-09-12", NA, NA))),
          iguais(preparo_converter_data(c("12-09-2026", "2026/09/12")),
                 as.Date(c("2026-09-12", "2026-09-12"))),
          iguais(preparo_converter_data(c("12.09.2026", "1/9/2026")),
                 as.Date(c("2026-09-12", "2026-09-01"))),
          falha(preparo_converter_data("31-02-2026")),
          falha(preparo_converter_data("2026/02/31")),
          falha(preparo_converter_data(c("01/09/2026", "31/02/2026"))),
          falha(preparo_converter_data("09/30/2026")),
          falha(preparo_converter_data("2026-09-12 resto")),
          falha(preparo_converter_data(45000)),
          falha(preparo_converter_data("29/02/2025")),
          identical(preparo_converter_data("29/02/2024"), as.Date("2024-02-29")))
cat("PASSOU: formatos, ausentes, bissexto e rejeição de datas inválidas.\n")

# A função pública é a mesma executada na IDE; o download contém só chamadas.
stopifnot(identical(body(catalyser::converter_datas), body(preparo_converter_data)),
          identical(catalyser::converter_datas(character()), as.Date(character())))
# Receitas anteriores e várias conversões deixam somente as chamadas no projeto.
legado <- c("## ---- tratar ----", "# Datas", "converter_data <- function(x) {",
            "  as.Date(x)", "}", "dados <- converter_data(dados)")
limpo <- exportacao_preparo_sem_funcao_data(legado)
stopifnot(identical(limpo, c("## ---- tratar ----", "dados <- catalyser::converter_datas(dados)")))

# Uma data preenchida pela moda continua sendo data fora da IDE.
d <- data.frame(data_coleta = as.Date(c("2026-09-01", NA, "2026-09-01")))
p <- list(coluna = "data_coleta", metodo = "moda")
env <- new.env(); env$dados <- d; env$trat_moda <- trat_moda
eval(parse(text = tratamentos$tratar_na$codigo(p)), env)
stopifnot(identical(tratamentos$tratar_na$aplicar(d, p), env$dados))

xlsx <- file.path(arquivos, "Conferencia-Datas.xlsx")
excel <- as.data.frame(readxl::read_excel(xlsx, sheet = "coletas"))
stopifnot(inherits(excel$data_excel, "POSIXt"),
          identical(arrumar_detectar_tipo(excel$data_excel), "data"),
          iguais(preparo_converter_data(excel$data_excel), esperadas))

# Um erro no diálogo não altera a receita; a conversão válida vira etapa.
entrada <- reactiveVal(data.frame(data_coleta = c("01/09/2026", "31/02/2026")))
testServer(mod_organizar_variaveis_server, args = list(data_rv = entrada), {
  session$flushReact()
  session$setInputs(abrir_tipar = 1, tipo_1 = "data", confirmar_tipar = 1)
  stopifnot(!length(tipos_rv()), is.character(resultado_final()$data_coleta))
  entrada(data.frame(data_coleta = c("01/09/2026", "02/09/2026")))
  session$flushReact()
  session$setInputs(abrir_tipar = 2, tipo_1 = "data", confirmar_tipar = 2)
  stopifnot(inherits(resultado_final()$data_coleta, "Date"))
  externo <- new.env(parent = globalenv()); externo$dados <- entrada()
  invisible(capture.output(eval(parse(text = codigo_rv()), externo)))
  stopifnot(iguais(externo$dados_organizados, resultado_final()))
})
cat("PASSOU: diálogo rejeita data inválida e código reproduz a conversão válida.\n")

extrair <- function(linhas, nome, script = TRUE) {
  marcador <- if (script) paste0("## ---- ", nome, " ----") else paste0("#| label: ", nome)
  inicio <- match(marcador, linhas)
  stopifnot(!is.na(inicio))
  fins <- if (script) which(grepl("^## ---- ", linhas)) else which(linhas == "```")
  fim <- min(c(fins[fins > inicio], length(linhas) + 1L))
  linhas[seq.int(inicio + 1L, fim - 1L)]
}
etapa <- function(tipo, params) list(tipo = tipo, params = params, ativa = TRUE)
casos <- list(
  list(nome = "Coletas-ponto-e-virgula.csv", sep = ";", dec = ","),
  list(nome = "Coletas-virgula.csv", sep = ",", dec = "."),
  list(nome = "Conferencia-Datas.xlsx"))
for (caso in casos) {
  info <- list(source = "local", file_name = file.path(arquivos, caso$nome),
    datapath = file.path(arquivos, caso$nome), excel_sheet = "coletas",
    csv_sep = caso$sep, csv_dec = caso$dec, csv_header = TRUE)
  csv <- !is.null(caso$sep)
  brutos <- if (csv) read.csv(info$file_name, sep = caso$sep, dec = caso$dec,
    check.names = FALSE, stringsAsFactors = FALSE) else excel
  stopifnot(nrow(brutos) == 8, brutos$peso_g[1] == 90.5)
  datas <- if (csv) "data_coleta" else c("data_excel", "data_br", "data_iso")
  info$preparo_importacao <- list(colunas = names(brutos), colunas_originais = names(brutos),
    classes_originais = lapply(brutos, function(x) class(x)[1]),
    tipos = setNames(as.list(rep("Date", length(datas))), datas))
  importada <- brutos
  for (coluna in datas) importada[[coluna]] <- preparo_converter_data(brutos[[coluna]])
  stopifnot(all(vapply(importada[datas], iguais, logical(1), b = esperadas)))
  if (csv) {
    stopifnot(brutos$observacao[1] == "rede, margem; setor #1",
      brutos$observacao[2] == "d'água", brutos$observacao[3] == 'medida "conferida"')
    # Downloads de reestruturação e trilha também precisam ler CSV.
    for (codigo in list(arrumar_gerar_codigo(list(), info), gerar_script_preparo(list(), info))) {
      env <- new.env(parent = globalenv())
      invisible(capture.output(eval(parse(text = codigo), env)))
      obtida <- if (exists("dados_arrumados", env, inherits = FALSE)) env$dados_arrumados else env$dados
      stopifnot(iguais(obtida, brutos))
    }
  }
  trilha <- list(etapa("reescalar", list(coluna = "peso_g", nome = "peso_kg", simbolo = "k")))
  compartilhada <- replay_pipeline(importada, trilha)$df
  env <- new.env(parent = globalenv())
  invisible(capture.output(eval(parse(text = preparo_codigo_completo(info, trilha)), env)))
  stopifnot(iguais(env$base_compartilhada, compartilhada))

  bases <- bases_adicionar(bases_vazio(), bases_novo_registro("base_0001", "Pesos maiores", "base_pesos"))
  bases <- bases_adicionar_etapa(bases, "base_0001", "filtrar",
    list(coluna = "peso_g", origem = "numerica", operador = ">", valor = 100), compartilhada)
  caches <- list(base_0001 = bases_recalcular_cache(compartilhada, bases[[1]], 1L))
  bases <- bases_finalizar(bases, "base_0001", caches, 1L)
  stopifnot(nrow(caches$base_0001$df) == 5)
  for (anova in c(FALSE, TRUE)) {
    tipo <- if (anova) "anova_um_fator" else "estatistica_descritiva"
    e <- list(id = "execucao_0001", analise_id = tipo, tipo = tipo, titulo = "Peso dos peixes",
      parametros = list(resposta = "peso_kg", fator = "local", nivel_confianca = .95,
        variaveis = "peso_kg", grupo = "none", metricas = list(media = TRUE)),
      saidas_disponiveis = "tabela", base_id = "base_0001", base_objeto = "base_pesos",
      base_tipo = "derivada", codigo_r = NULL)
    manifesto <- comunicacao_manifesto(comunicacao_estado_vazio(), list(execucao_0001 = e),
      list(execucao_0001 = "Atualizada"))
    destino <- tempfile("csv_datas_"); dir.create(destino)
    zip_saida <- file.path(destino, "projeto.zip")
    exportacao_empacotar_projeto(zip_saida, nome_projeto = "coletas", dados_brutos = brutos,
      base_resolvida = importada, dados_analise = compartilhada, pipeline = trilha,
      base_externa = NULL,
      registro_bases = bases, cache_bases = caches, registro_execucoes = list(execucao_0001 = e),
      manifesto = manifesto, revisao_origem = 1L, import_info = info, templates_dir = "templates")
    utils::unzip(zip_saida, exdir = destino)
    projeto <- file.path(destino, "coletas")
    auxiliares <- readLines(file.path(projeto, "R/funcoes.R"), encoding = "UTF-8")
    stopifnot(!any(grepl("^converter_datas? <- function", auxiliares)))
    for (script in c(TRUE, FALSE)) {
      arquivo <- if (script) "R/analise.R" else "relatorios/relatorio.qmd"
      linhas <- readLines(file.path(projeto, arquivo), encoding = "UTF-8")
      stopifnot(!any(grepl("^converter_datas? <- function", linhas)))
      if (script) {
        # O script reconstrói a base e deve usar a conversão canônica.
        stopifnot(any(grepl("catalyser::converter_datas(", linhas, fixed = TRUE)))
      }
      env <- new.env(parent = globalenv())
      env$here <- function(...) file.path(projeto, ...)
      sys.source(file.path(projeto, "R/funcoes.R"), envir = env)
      if (script) {
        invisible(capture.output(eval(parse(text = extrair(linhas, "importar", TRUE)), env)))
        invisible(capture.output(eval(parse(text = extrair(linhas, "tratar", TRUE)), env)))
      } else {
        # O relatório adota a fotografia preparada, sem refazer a importação.
        # RDS preserva Date; a igualdade abaixo confere valores e classes.
        carregar <- extrair(linhas, if (anova) "carregar-bases" else "carregar-compartilhada", FALSE)
        stopifnot(any(grepl("readRDS(", carregar, fixed = TRUE)))
        invisible(capture.output(eval(parse(text = carregar), env)))
      }
      obtida <- if (anova && !script) env$dados else if (anova) env$base_compartilhada else env$dados_analise
      referencia <- if (anova && !script) caches$base_0001$df else compartilhada
      stopifnot(isTRUE(all.equal(obtida, referencia, check.attributes = FALSE)),
        all(vapply(obtida[datas], inherits, logical(1), "Date")),
        all(vapply(datas, function(coluna) iguais(obtida[[coluna]], referencia[[coluna]]), logical(1))))
      if (anova) {
        invisible(capture.output(eval(parse(text = extrair(linhas,
          if (script) "preparar-analise" else "preparo", script)), env)))
      } else {
        raiz <- unname(exportacao_raizes_chunk(manifesto$execucoes)[[1]])
        bloco <- extrair(linhas, if (script) paste0(raiz, "-base") else raiz, script)
        if (!script) bloco <- head(bloco, grep("^dados_da_analise <-", bloco)[1])
        invisible(capture.output(eval(parse(text = bloco), env)))
      }
      ramo <- if (anova) env$dados else env$dados_da_analise
      stopifnot(nrow(ramo) == 5, all(vapply(ramo[datas], inherits, logical(1), "Date")))
    }
    baixada <- as.data.frame(readxl::read_excel(file.path(projeto, "dados/processados/base_compartilhada.xlsx")))
    for (coluna in datas) stopifnot(iguais(preparo_converter_data(baixada[[coluna]]), esperadas))
  }
  cat("PASSOU:", caso$nome, "— leitura, compartilhada, derivada, Excel e preparo dos projetos geral e ANOVA.\n")
}
cat("CONCLUIDO: revisão CSV e datas.\n")
