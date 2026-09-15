# Execute de inst/app. Confere o roteiro que efetivamente vai para o aluno.
grDevices::pdf(NULL)
for (arquivo in c("registro_tratamentos.R", "registro_bases.R", "registro_execucoes.R",
                  "registro_comunicacao.R", "exportacao_comunicacao.R")) {
  source(file.path("modules", arquivo), encoding = "UTF-8")
}
apoio <- new.env(parent = globalenv())
sys.source("templates/funcoes.R", apoio)

item <- list(id = "execucao_0001", tipo = "regressao_linear", titulo = "Distância e velocidade",
  incluir_word = TRUE, estado_dependencia = "Atualizada", base_tipo = "compartilhada",
  base_id = "dados_analise", base_objeto = "dados_analise",
  parametros = list(resposta = "distancia", preditor = "velocidade", nivel_confianca = .95),
  saidas_word = c("narrativa", "tabela", "grafico", "pressupostos", "diagnosticos"))
brutos <- data.frame(velocidade = cars$speed, distancia = cars$dist)
rodar <- function(item, dados, graficos = FALSE) {
  codigo <- c(exportacao_regressao_trechos(item, "reta"), "## ---- fim ----")
  marcas <- grep("^## ---- ", codigo)
  ambiente <- new.env(parent = apoio)
  ambiente$dados_da_analise <- dados
  for (i in seq_len(length(marcas) - 1L)) {
    nome <- codigo[marcas[i]]
    if (!graficos && grepl("reta-(tabela|metricas|grafico|diagnostico-)", nome)) next
    invisible(capture.output(eval(parse(text = codigo[seq.int(marcas[i] + 1L, marcas[i + 1L] - 1L)]), ambiente)))
  }
  ambiente
}
a <- rodar(item, brutos, TRUE)
esperado <- lm(dist ~ speed, cars)
stopifnot(isTRUE(all.equal(unname(coef(a$modelo_lm)), unname(coef(esperado)))),
  isTRUE(all.equal(a$tabela_coeficientes$conf.low, unname(confint(esperado)[, 1]))),
  abs(a$metricas_modelo$r.squared - summary(esperado)$r.squared) < 1e-12,
  abs(a$p_shapiro - shapiro.test(residuals(esperado))$p.value) < 1e-12,
  a$p_shapiro < .05, a$p_hetero < .05,
  grepl("houve evidência de desvio", a$texto_pressupostos),
  grepl("variância não constante", a$texto_pressupostos),
  grepl("Autocorrelação não testada", a$texto_pressupostos),
  !grepl("p = 0,000", a$texto_resultados, fixed = TRUE))
invisible(ggplot2::ggplot_build(a$grafico_regressao))
cat(sprintf("CARS: beta=%.8f; R2=%.8f; Shapiro p=%.8f; BP p=%.8f\n",
  a$beta, a$metricas_modelo$r.squared, a$p_shapiro, a$p_hetero))

# Cabeçalhos com espaços e unidades são preservados, sem interpretar nomes como código.
espacos <- item
espacos$parametros$resposta <- "Distância (ft)"
espacos$parametros$preditor <- "Velocidade (mph)"
dados_espacos <- brutos
names(dados_espacos) <- c(espacos$parametros$preditor, espacos$parametros$resposta)
b <- rodar(espacos, dados_espacos)
stopifnot(isTRUE(all.equal(unname(coef(a$modelo_lm)), unname(coef(b$modelo_lm)))))
faltantes <- rbind(brutos, data.frame(velocidade = NA, distancia = 3))
stopifnot(rodar(item, faltantes)$n_excluidos == 1)
negativos <- brutos
negativos$distancia <- -negativos$distancia
stopifnot(grepl("diminui", rodar(item, negativos)$texto_resultados))
for (dados_ruins in list(transform(brutos, velocidade = 1), transform(brutos, distancia = Inf), brutos[1:2, ])) {
  erro <- tryCatch({rodar(item, dados_ruins); NULL}, error = conditionMessage)
  stopifnot(is.character(erro))
}
longos <- data.frame(velocidade = seq_len(5001), distancia = seq_len(5001) + sin(seq_len(5001)))
stopifnot(is.na(rodar(item, longos)$p_shapiro))
ordenado <- item
ordenado$parametros$avaliar_autocorrelacao <- TRUE
o <- rodar(ordenado, brutos)
stopifnot(is.finite(o$p_autocorr), grepl("Durbin-Watson: p", o$texto_pressupostos))

# Gera um projeto real, sincroniza e executa os chunks copiados, inclusive tabelas.
destino <- tempfile("regressao_20260914_",
  tmpdir = normalizePath("../../../APOIO/temp", winslash = "/", mustWork = TRUE))
dir.create(destino)
manifesto <- list(execucoes = list(execucao_0001 = item), secoes_globais = list())
projeto <- exportacao_criar_projeto(destino = destino, nome_projeto = "regressao_cars_20260914",
  dados_brutos = brutos, base_resolvida = brutos, dados_analise = brutos,
  pipeline = list(), base_externa = NULL, registro_bases = list(), cache_bases = list(),
  registro_execucoes = manifesto$execucoes, manifesto = manifesto, revisao_origem = 1L,
  import_info = list(source = "local", file_name = "cars.xlsx", excel_sheet = "cars"),
  templates_dir = "templates")
script <- file.path(projeto, "R", "analise.R")
qmd <- file.path(projeto, "relatorios", "relatorio.qmd")
stopifnot(apoio$conferir_codigo(qmd, script))
linhas <- readLines(qmd, encoding = "UTF-8")
chunks <- apoio$chunks_do_relatorio(linhas)
ambiente <- new.env(parent = apoio)
ambiente$here <- function(...) file.path(projeto, ...)
for (ch in chunks) {
  if (grepl("regressao-", ch$rotulo)) {
    invisible(capture.output(eval(parse(text = linhas[ch$ini:ch$fim]), ambiente)))
  }
}
stopifnot(isTRUE(all.equal(unname(coef(ambiente$modelo_lm)), unname(coef(esperado)))))
# Duas regressões recebem chunks distintos, e alterações no script são detectadas.
outra <- item
outra$id <- "execucao_0002"
manifesto$execucoes$execucao_0002 <- outra
qmd_duplo <- exportacao_gerar_qmd(manifesto)
rotulos <- grep("^#\\| label:", qmd_duplo, value = TRUE)
stopifnot(!anyDuplicated(rotulos))
cat("OK: coeficientes, IC, diagnósticos, direção, dados inválidos, nomes com espaços e par script–relatório.\n")
cat("PROJETO:", projeto, "\n")
