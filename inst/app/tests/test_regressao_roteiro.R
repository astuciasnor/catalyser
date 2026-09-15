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

# O dado canônico do ecossistema é o camarão; cars serve de caso com desvio de
# variância, o camarão de caso com desvio de normalidade. Ambos precisam manter
# os mesmos números do livro e da atividade.
camarao <- as.data.frame(EAPADados::camarao_vannamei_biometria)
item_camarao <- item
item_camarao$parametros$resposta <- "peso_g"
item_camarao$parametros$preditor <- "comprimento_cm"
c_ <- rodar(item_camarao, camarao, TRUE)
esperado_camarao <- lm(peso_g ~ comprimento_cm, data = camarao)
stopifnot(
  isTRUE(all.equal(unname(coef(c_$modelo_lm)), unname(coef(esperado_camarao)))),
  isTRUE(all.equal(c_$tabela_coeficientes$conf.low, unname(confint(esperado_camarao)[, 1]))),
  abs(c_$metricas_modelo$r.squared - 0.8990865) < 1e-6,
  abs(c_$p_shapiro - shapiro.test(residuals(esperado_camarao))$p.value) < 1e-12,
  # Aqui a normalidade é rejeitada: o texto deve dizer isso, não o contrário.
  c_$p_shapiro < .05,
  grepl("houve evidência de desvio da normalidade", c_$texto_pressupostos),
  grepl("sinais de inadequação", c_$alerta_pressupostos),
  abs(max(c_$dados_diagnostico$.cooksd) - max(cooks.distance(esperado_camarao))) < 1e-12)
invisible(ggplot2::ggplot_build(c_$grafico_regressao))

# O nível de confiança escolhido na tela tem de chegar ao IC da tabela e da frase.
# graficos = TRUE porque `tabela_artigo` nasce no trecho reta-tabela, que o
# helper pula quando só o cálculo interessa.
noventa <- item_camarao
noventa$parametros$nivel_confianca <- .90
n_ <- rodar(noventa, camarao, TRUE)
stopifnot(isTRUE(all.equal(n_$tabela_coeficientes$conf.low,
    unname(confint(esperado_camarao, level = .90)[, 1]))),
  identical(names(n_$tabela_artigo)[4], "IC 90%"),
  grepl("IC 90%", n_$texto_resultados, fixed = TRUE))

# O tema escolhido na tela tem de chegar ao roteiro, sem cair no padrão.
for (tema in c("minimal", "classic")) {
  t_ <- item
  t_$parametros$tema <- tema
  stopifnot((function(env) inherits(env$tema_escolhido, "theme"))(rodar(t_, brutos, TRUE)))
}

# Título e rótulos mudam a apresentação, nunca o cálculo. Sem rótulo, a variável
# aparece pelo próprio nome; com rótulo, a narrativa e os eixos o usam.
rotulado <- item_camarao
rotulado$parametros$rotulo_resposta <- "Peso (g)"
rotulado$parametros$rotulo_preditor <- "Comprimento (cm)"
rotulado$parametros$titulo_personalizado <- "Crescimento do camarão"
r_ <- rodar(rotulado, camarao, TRUE)
stopifnot(identical(r_$rotulo_resposta, "Peso (g)"),
  identical(r_$rotulo_preditor, "Comprimento (cm)"),
  identical(r_$grafico_regressao$labels$x, "Comprimento (cm)"),
  identical(r_$grafico_regressao$labels$y, "Peso (g)"),
  identical(r_$grafico_regressao$labels$title, "Crescimento do camarão"),
  grepl("Peso (g) em função de Comprimento (cm)", r_$texto_resultados, fixed = TRUE),
  !grepl("peso_g em função", r_$texto_resultados, fixed = TRUE),
  # O rótulo é só apresentação: os números têm de ser os mesmos.
  isTRUE(all.equal(r_$beta, c_$beta)),
  isTRUE(all.equal(r_$metricas_modelo$r.squared, c_$metricas_modelo$r.squared)),
  isTRUE(all.equal(r_$tabela_artigo[[4]], c_$tabela_artigo[[4]])))
# Rótulo em branco (ou só espaços) volta ao nome da variável.
em_branco <- item_camarao
em_branco$parametros$rotulo_resposta <- "   "
stopifnot(identical(rodar(em_branco, camarao)$rotulo_resposta, "peso_g"))
# Sem título informado, o gráfico não recebe título nenhum.
stopifnot(is.null(c_$grafico_regressao$labels$title),
  identical(c_$rotulo_preditor, "comprimento_cm"))
cat(sprintf("CAMARAO: beta=%.6f; R2=%.7f; Shapiro p=%.4g; Cook max=%.4f\n",
  c_$beta, c_$metricas_modelo$r.squared, c_$p_shapiro, max(c_$dados_diagnostico$.cooksd)))

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
