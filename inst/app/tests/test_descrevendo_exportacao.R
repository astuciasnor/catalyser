# O destino opcional permite inspecionar e renderizar o projeto após o teste.
source("app.R", local = TRUE)
source("../../R/analises.R", encoding = "UTF-8")
load("../../../EAPADados/data/abalone_adultos.rda")
dados <- abalone_adultos
reg_bases <- bases_adicionar(bases_vazio(), bases_novo_registro(
  "amostra_sexo", "200 adultos por sexo", "base_abalone_400", "geral"))
reg_bases <- bases_adicionar_etapa(reg_bases, "amostra_sexo", "sortear_amostra",
  list(coluna = "sexo", n = 200L, semente = 42L), dados)
cache <- list(amostra_sexo = bases_recalcular_cache(dados, bases_obter(reg_bases, "amostra_sexo"), 1L))
reg_bases <- bases_finalizar(reg_bases, "amostra_sexo", cache, 1L)
ctx <- bases_resolver_analise("amostra_sexo", dados, reg_bases, cache, 1L)
stopifnot(nrow(ctx$df) == 400L, all(table(ctx$df$sexo) == 200L))
registro <- execucoes_vazio()
numericas <- names(dados)[vapply(dados, is.numeric, logical(1))]
modos <- unname(unlist(descricao_catalogo()))
for (i in seq_along(modos)) {
  modo <- modos[i]
  p <- list(analise = modo, variavel = if (modo == "frequencias") "sexo" else numericas[1],
            outra = numericas[2], grupo = "sexo")
  resposta <- catalyser_descricao(ctx$df, p)
  # Pistas da interface não se tornam seções independentes do relatório.
  saidas_relatorio <- intersect(names(resposta), c("narrativa", "tabela", "grafico", "console"))
  estado <- list(analise_id = "descricao_teste", tipo = "descricao_exploratoria",
                 titulo = paste("Abalone", modo), parametros = p, saidas_disponiveis = saidas_relatorio)
  item <- execucoes_criar(sprintf("execucao_%04d", i), estado, ctx, 1L)
  registro <- execucoes_adicionar(registro, item)
  env <- new.env(parent = globalenv()); env$dados <- ctx$df
  eval(parse(text = exportacao_codigo_estudo(item, incluir_carregamento = FALSE)), env)
  stopifnot(identical(env$resultado$tabela, resposta$tabela))
}
editorial <- comunicacao_sincronizar(comunicacao_estado_vazio(), registro)
# A tabela de outliers tem 400 linhas; o relatório usa a síntese e a figura.
# A tabela completa continua disponível no resultado e na interface.
# Localizar pela análise evita acoplar o teste à quantidade de retratos do menu.
id_outliers <- names(registro)[vapply(registro, function(item) identical(item$parametros$analise, "outliers"), logical(1))]
editorial <- comunicacao_definir_item(editorial, id_outliers, saidas_selecionadas = c("narrativa", "grafico"))
manifesto <- comunicacao_manifesto(editorial, registro,
  stats::setNames(as.list(rep("Atualizada", length(registro))), names(registro)))
destino <- Sys.getenv("CATALYSER_TESTE_DESCRICAO", unset = tempfile("descricao_exportacao_"))
dir.create(destino, recursive = TRUE, showWarnings = FALSE)
projeto <- exportacao_criar_projeto(destino, "abalone_descrevendo", dados, dados, dados,
  pipeline = list(), base_externa = NULL, registro_bases = reg_bases, cache_bases = cache,
  registro_execucoes = registro, manifesto = manifesto, revisao_origem = 1L,
  import_info = list(source = "package", package_dataset = "abalone_adultos"), templates_dir = "templates")
script <- readLines(file.path(projeto, "R", "analise.R"), encoding = "UTF-8")
qmd <- readLines(file.path(projeto, "relatorios", "relatorio.qmd"), encoding = "UTF-8")
stopifnot(any(grepl("stats::shapiro.test", script, fixed = TRUE)),
          any(grepl("MASS::boxcox", script, fixed = TRUE)),
          any(grepl("slice_sample", script, fixed = TRUE)),
          sum(grepl("#| eval: false", qmd, fixed = TRUE)) == 2L)
cat("OK: seis retratos e checagens exportados com base derivada de 200 por sexo.\nProjeto:", projeto, "\n")
