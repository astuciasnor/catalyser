# Rodar a partir de inst/app. Exercita as saídas e os estados afetados pelo feedback.
library(shiny)
for (arquivo in c("utils_export", "mod_arrumar", "mod_organizar_variaveis",
                  "registro_tratamentos", "registro_bases", "registro_execucoes",
                  "registro_comunicacao", "exportacao_comunicacao", "mod_bases_derivadas")) {
  source(paste0("modules/", arquivo, ".R"), encoding = "UTF-8")
}

pasta <- normalizePath("../../docs/testes/fases-3/arquivos_preparo_v1", winslash = "/")
entradas <- list(
  list(source = "local", file_name = file.path(pasta, "Coletas-ponto-e-virgula.csv"), csv_sep = ";", csv_dec = ",", csv_header = TRUE),
  list(source = "local", file_name = file.path(pasta, "Coletas-virgula.csv"), csv_sep = ",", csv_dec = ".", csv_header = TRUE),
  list(source = "local", file_name = file.path(pasta, "Conferencia-Datas.xlsx"), excel_sheet = "coletas")
)
resultados <- lapply(entradas, function(info) {
  ambiente <- new.env(parent = globalenv())
  invisible(capture.output(eval(parse(text = preparo_codigo_importacao(info)), ambiente)))
  completo <- new.env(parent = globalenv())
  eval(parse(text = preparo_codigo_completo(info)), completo)
  stopifnot(nrow(ambiente$dados) == 8L, ambiente$dados$peso_g[1] == 90.5,
    identical(ambiente$dados, completo$dados_analise))
  ambiente$dados
})
stopifnot(identical(resultados[[1]], resultados[[2]]), ncol(resultados[[3]]) == 6L,
  inherits(resultados[[3]]$data_excel, "POSIXt"))
cat("PASSOU: código de importação dos dois CSVs e do Excel coincide com a sequência completa.\n")

reg <- reactiveVal(list()); cache <- reactiveVal(list()); revisao <- reactiveVal(1L)
origem <- reactiveVal(resultados[[1]])
testServer(mod_bases_derivadas_server, args = list(
  dados_analise_rv = origem, registro_bases_rv = reg, cache_bases_rv = cache,
  revisao_origem_rv = revisao,
  codigo_compartilhada_rv = reactive(preparo_codigo_completo(entradas[[1]]))), {
  session$flushReact()
  session$setInputs(nome_amigavel = "Pesos acima de 100 g", nome_r = "base_pesos",
    finalidade = "geral", descricao = "", criar = 1)
  session$setInputs(ramo_tipo = "filtrar", ramo_fil_col = "peso_g", ramo_fil_origem = "numerica",
    ramo_fil_op = ">", ramo_fil_valor = 100, adicionar_etapa = 1)
  session$setInputs(recalcular = 1, finalizar = 1)
  stopifnot(grepl("alert-success", output$editor_base$html),
    identical(nome_download(".xlsx"), "base_derivada_pesos.xlsx"),
    identical(nome_download(".R"), "base_derivada_pesos.R"))
  x <- readxl::read_excel(output$baixar_base)
  stopifnot(identical(x$id_peixe, c(2, 3, 5, 6, 8)))
  ambiente <- new.env(parent = globalenv())
  eval(parse(text = readLines(output$baixar_codigo, encoding = "UTF-8")), ambiente)
  stopifnot(isTRUE(all.equal(as.list(ambiente$base_pesos), as.list(cache_selecionado()$df))))
  revisao(2L); session$flushReact()
  stopifnot(!grepl("Esta base está pronta", output$editor_base$html),
    inherits(try(exigir_atualizada(), silent = TRUE), "try-error"))
})
cat("PASSOU: derivada atualizada em verde, nomes dos downloads e saídas equivalentes; base desatualizada sem mensagem de sucesso.\n")
cat("FEEDBACK_5_CONCLUIDO\n")
