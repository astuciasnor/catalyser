invisible(Sys.setlocale("LC_ALL", "English_United States.utf8"))
source("app.R", encoding = "UTF-8")
source("tests/carregar_catalyser.R", encoding = "UTF-8")
library(dplyr)
library(tidyr)

# Dados de pesca pequenos, com mudanças reais já na importação.
brutos <- data.frame(amostra = paste0(LETTERS[1:8], "_seca"),
  especie = rep(c("Tilápia", "Tambaqui"), each = 4),
  peso_texto = paste0(seq(100, 800, 100), ",0"),
  comprimento_cm = 10:17, descartar = "fora")
info <- list(source = "local", file_name = "pesca.xlsx", excel_sheet = "pesca",
  preparo_importacao = list(
    colunas = names(brutos)[1:4], colunas_originais = names(brutos),
    classes_originais = lapply(brutos, function(x) class(x)[1]),
    tipos = list(peso_texto = "numeric", especie = "factor", comprimento_cm = "integer"),
    recodificacoes = list(especie = c("Tilápia" = "tilapia", "Tambaqui" = "tambaqui")),
    filtros_niveis = list(especie = c("tilapia", "tambaqui")),
    filtros_faixas = list(comprimento_cm = c(11, 17)),
    renomes = c(peso_texto = "peso_g")))
info$datapath <- tempfile(fileext = ".xlsx")
writexl::write_xlsx(list(pesca = brutos, notas = data.frame(nota = "Planilha original")), info$datapath)
importada <- brutos[2:8, 1:4]
importada$peso_texto <- seq(200, 800, 100)
importada$especie <- factor(c(rep("tilapia", 3), rep("tambaqui", 4)))
names(importada)[3] <- "peso_g"

extrair <- function(linhas, nome, script = TRUE) {
  marcador <- if (script) paste0("## ---- ", nome, " ----") else paste0("#| label: ", nome)
  inicio <- match(marcador, linhas)
  stopifnot(!is.na(inicio))
  fins <- if (script) which(grepl("^## ---- ", linhas)) else which(linhas == "```")
  fim <- min(fins[fins > inicio])
  linhas[seq.int(inicio + 1L, fim - 1L)]
}
iguais <- function(a, b) isTRUE(all.equal(as.data.frame(a), as.data.frame(b),
                                        check.attributes = FALSE))
etapa <- function(tipo, params, ativa = TRUE) list(tipo = tipo, params = params, ativa = ativa)
trilha <- list(
  etapa("reescalar", list(coluna = "peso_g", simbolo = "k", nome = "peso_kg")),
  etapa("organizar", list(renomear = c(peso_kg = "massa_kg"))),
  etapa("calcular", list(nome = "nao_executar", expr = "peso_g * 999"), FALSE))

for (manter in c(FALSE, TRUE)) {
  cfgs <- list(
    list(modo = "separar", metodo = "delim", col_separar = "amostra",
         novas = c("local_amostra", "estacao"), delim = "_", manter_original = manter),
    list(modo = "empilhar", metodo = "regex", cols_medida = c("peso_g", "comprimento_cm"),
         novas = "medida", regex = "(.*)", values_to = "valor", como_numero = TRUE, saida = "longo"),
    list(modo = "alargar", names_from = "medida", values_from = "valor"))
  resolvida <- importada
  externa <- list(codigo = "", codigo_sequencial = character())
  for (cfg in cfgs) {
    resolvida <- as.data.frame(arrumar_aplicar(cfg, resolvida))
    stopifnot(nrow(resolvida) > 0)
    codigo <- arrumar_gerar_codigo(list(cfg), info)
    externa$codigo <- paste(externa$codigo, codigo, sep = "\n")
    externa$codigo_sequencial <- c(externa$codigo_sequencial, "dados_arrumados <- dados",
      arrumar_codigo_etapas(codigo), "dados <- dados_arrumados")
  }
  compartilhada <- replay_pipeline(resolvida, trilha)$df
  stopifnot(nrow(compartilhada) == 7, "amostra" %in% names(compartilhada) == manter,
            !"nao_executar" %in% names(compartilhada))
  bases <- bases_adicionar(bases_vazio(), bases_novo_registro("base_0001", "Pesos altos", "base_pesos"))
  bases <- bases_adicionar_etapa(bases, "base_0001", "filtrar",
    list(coluna = "peso_g", origem = "numerica", operador = ">", valor = 300), compartilhada)
  bases <- bases_adicionar_etapa(bases, "base_0001", "organizar",
    list(renomear = c(massa_kg = "massa_final")), compartilhada)
  caches <- list(base_0001 = bases_recalcular_cache(compartilhada, bases[[1]], 1L))
  bases <- bases_finalizar(bases, "base_0001", caches, 1L)
  for (anova in c(FALSE, TRUE)) {
    tipo <- if (anova) "anova_um_fator" else "estatistica_descritiva"
    e <- list(id = "execucao_0001", analise_id = tipo, tipo = tipo, titulo = "Peso dos peixes",
      parametros = list(resposta = "massa_final", fator = "especie", nivel_confianca = .95,
                        variaveis = "massa_final", grupo = "none", metricas = list(media = TRUE)),
      saidas_disponiveis = "tabela", base_id = "base_0001", base_objeto = "base_pesos",
      base_tipo = "derivada", codigo_r = NULL)
    manifesto <- comunicacao_manifesto(comunicacao_estado_vazio(), list(execucao_0001 = e),
                                       list(execucao_0001 = "Atualizada"))
    destino <- tempfile("preparo_completo_"); dir.create(destino)
    argumentos <- list(nome_projeto = "pesca_teste", dados_brutos = brutos,
      base_resolvida = resolvida, dados_analise = compartilhada, pipeline = trilha,
      base_externa = externa, registro_bases = bases, cache_bases = caches,
      registro_execucoes = list(execucao_0001 = e), manifesto = manifesto,
      revisao_origem = 1L, import_info = info, templates_dir = "templates")
    zip_saida <- file.path(destino, "projeto.zip")
    do.call(exportacao_empacotar_projeto, c(list(file = zip_saida), argumentos))
    utils::unzip(zip_saida, exdir = destino)
    projeto <- file.path(destino, "pesca_teste")
    # O teste executa os arquivos retirados do ZIP, sem fotografia estrutural.
    stopifnot(!file.exists(file.path(projeto, "dados/processados/base_resolvida.rds")),
      identical(readxl::excel_sheets(file.path(projeto, "dados/brutos/pesca.xlsx")), "pesca"),
      identical(readxl::excel_sheets(info$datapath), c("pesca", "notas")),
      isTRUE(all.equal(as.data.frame(readxl::read_excel(file.path(projeto, "dados/brutos/pesca.xlsx"))), brutos)))
    for (script in c(TRUE, FALSE)) {
      arquivo <- if (script) "R/analise.R" else "relatorios/relatorio.qmd"
      linhas <- readLines(file.path(projeto, arquivo), encoding = "UTF-8")
      env <- new.env(parent = globalenv())
      env$here <- function(...) file.path(projeto, ...)
      sys.source(file.path(projeto, "R/funcoes.R"), envir = env)
      # Importa de verdade a planilha que viajou no projeto.
      library(readxl)
      if (script) {
        eval(parse(text = extrair(linhas, "importar", TRUE)), env)
        eval(parse(text = extrair(linhas, "tratar", TRUE)), env)
      } else {
        eval(parse(text = extrair(linhas, if (anova) "carregar-bases" else "carregar-compartilhada", FALSE)), env)
      }
      obtida <- if (anova && !script) readRDS(file.path(projeto, "dados/processados/base_compartilhada.rds")) else if (anova) env$base_compartilhada else env$dados_analise
      stopifnot(iguais(obtida, compartilhada))
      if (!anova) {
        raiz <- unname(exportacao_raizes_chunk(manifesto$execucoes)[[1]])
        bloco <- extrair(linhas, if (script) paste0(raiz, "-base") else raiz, script)
        # No relatório, base e análise dividem um chunk. Confere até a entrega da base.
        if (!script) bloco <- head(bloco, grep("^dados_da_analise <-", bloco)[1])
        eval(parse(text = bloco), env)
      }
      obtida_ramo <- if (anova) env$dados else env$dados_da_analise
      stopifnot(iguais(obtida_ramo, caches$base_0001$df), nrow(obtida_ramo) == 5)
    }
    cat("PASSOU:", tipo, "— importação, separar, empilhar, alargar, reescala, renomes e derivada; original mantida:", manter, "\n")
    if (!manter) {
      # Uma etapa estrutural perdida deve interromper a exportação, sem recorrer à fotografia.
      quebrados <- argumentos
      quebrados$base_externa$codigo_sequencial <- "dados <- dados[, 1, drop = FALSE]"
      erro <- tryCatch({do.call(exportacao_criar_projeto, c(list(destino = tempfile()), quebrados)); NULL},
                       error = conditionMessage)
      stopifnot(is.character(erro), grepl("preparo", erro, ignore.case = TRUE))
      # O mesmo vale para uma receita de derivada que não bate com o cache.
      quebrados <- argumentos
      quebrados$cache_bases$base_0001$df$massa_final <- -999
      erro <- tryCatch({do.call(exportacao_criar_projeto, c(list(destino = tempfile()), quebrados)); NULL},
                       error = conditionMessage)
      stopifnot(is.character(erro), grepl("base derivada", erro, fixed = TRUE))
    }
  }
}
desconhecida <- list(etapa("tratamento_inexistente", list()))
stopifnot(inherits(try(exportacao_bloco_trilha(desconhecida), silent = TRUE), "try-error"),
          inherits(try(bases_codigo(list(nome_amigavel = "Teste", nome_r = "base_teste", etapas = desconhecida)), silent = TRUE), "try-error"))
cat("PREPARO_COMPLETO_CONFERIDO\n")
