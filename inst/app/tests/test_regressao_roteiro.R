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
invisible(ggplot2::ggplot_build(a$grafico_alavancagem))
stopifnot(isTRUE(all.equal(a$diagnostico_alavancagem$.hat,
    unname(hatvalues(esperado)))),
  identical(a$dados_diagnostico$.linha_base, seq_len(nrow(brutos))),
  grepl("sinais de inadequação", a$texto_conclusao, fixed = TRUE))
# Com um par incompleto no meio, o rótulo continua apontando para a linha original.
com_lacuna <- brutos
com_lacuna$distancia[4] <- NA_real_
lacuna <- rodar(item, com_lacuna, TRUE)
stopifnot(identical(lacuna$dados_diagnostico$.linha_base, setdiff(seq_len(50), 4L)))
# A conclusão também deve ser honesta quando não há evidência de associação.
sem_associacao <- data.frame(velocidade = 1:40, distancia = rep(c(-1, 1, 1, -1), 10))
stopifnot(grepl("não houve evidência de associação linear",
  rodar(item, sem_associacao)$texto_conclusao, fixed = TRUE))
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

# Camarão e cars permanecem como casos de diagnóstico com sinais de inadequação.
# O exemplo principal escolhido pelo autor é morfometria_barbo, conferido abaixo.
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

# O barbo passa a ser a referência do roteiro e do projeto de revisão.
# São medidas corrigidas pelo tamanho: interpretamos associação de forma.
barbo <- as.data.frame(EAPADados::morfometria_barbo)
item_barbo <- item
item_barbo$titulo <- "Associação entre medidas de forma do barbo"
item_barbo$parametros <- list(
  resposta = "comprimento_cabeca", preditor = "distancia_pre_peitoral",
  nivel_confianca = .95, grupo = "none", regressao_por_grupo = FALSE,
  avaliar_autocorrelacao = FALSE, mostrar_equacao = TRUE, tema = "classic",
  rotulo_resposta = "Comprimento da cabeça (medida corrigida)",
  rotulo_preditor = "Distância pré-peitoral (medida corrigida)")
barbo_roteiro <- rodar(item_barbo, barbo, TRUE)
esperado_barbo <- lm(comprimento_cabeca ~ distancia_pre_peitoral, data = barbo)
stopifnot(nrow(barbo) == 100L, barbo_roteiro$n_excluidos == 0,
  isTRUE(all.equal(unname(coef(barbo_roteiro$modelo_lm)), unname(coef(esperado_barbo)))),
  isTRUE(all.equal(barbo_roteiro$tabela_coeficientes$conf.low, unname(confint(esperado_barbo)[, 1]))),
  abs(barbo_roteiro$metricas_modelo$r.squared - summary(esperado_barbo)$r.squared) < 1e-12,
  abs(barbo_roteiro$p_shapiro - shapiro.test(residuals(esperado_barbo))$p.value) < 1e-12,
  abs(barbo_roteiro$p_hetero - as.numeric(performance::check_heteroscedasticity(esperado_barbo))) < 1e-12,
  grepl("medida corrigida", barbo_roteiro$texto_resultados, fixed = TRUE),
  !grepl("crescimento", barbo_roteiro$texto_resultados, fixed = TRUE))
cat(sprintf("BARBO: beta=%.8f; R2=%.8f; Shapiro p=%.8f; BP p=%.8f\n",
  barbo_roteiro$beta, barbo_roteiro$metricas_modelo$r.squared,
  barbo_roteiro$p_shapiro, barbo_roteiro$p_hetero))

# Gera a árvore atual: um script e dois QMDs que executam esse mesmo script.
# O teste não depende de uma pasta temporária histórica existente no computador.
destino <- Sys.getenv("CATALYSER_TESTE_REGRESSAO_DESTINO", unset = tempfile("regressao_barbo_"))
dir.create(destino, recursive = TRUE, showWarnings = FALSE)
manifesto <- list(execucoes = list(execucao_0001 = item_barbo), secoes_globais = list())
projeto <- exportacao_criar_projeto(destino = destino, nome_projeto = "regressao_barbo",
  dados_brutos = barbo, base_resolvida = barbo, dados_analise = barbo,
  pipeline = list(), base_externa = NULL, registro_bases = list(), cache_bases = list(),
  registro_execucoes = manifesto$execucoes, manifesto = manifesto, revisao_origem = 1L,
  import_info = list(source = "package", package_dataset = "morfometria_barbo"),
  templates_dir = "templates")
script <- file.path(projeto, "R", "analise.R")
documentos <- c("relatorio_completo.qmd", "relatorio_artigo.qmd")
stopifnot(file.exists(script),
  !file.exists(file.path(projeto, "relatorios", "relatorio.qmd")),
  file.exists(file.path(projeto, "_quarto.yml")))
for (documento in documentos) {
  linhas <- readLines(file.path(projeto, "relatorios", documento), encoding = "UTF-8")
  stopifnot(any(grepl('source(here::here("R", "analise.R"), encoding = "UTF-8")', linhas, fixed = TRUE)),
    any(grepl("cinco populações", linhas, fixed = TRUE)),
    any(grepl("takacs2022", linhas, fixed = TRUE)),
    !any(grepl("conferir_codigo(", linhas, fixed = TRUE)),
    !any(grepl("{{", linhas, fixed = TRUE)))
}
# Texto do autor prevalece em todas as seções; o exemplo do barbo não vaza para Excel.
autoral <- manifesto
autoral$secoes_globais <- list(introducao = "Introdução escrita pelo autor.",
  metodos = "Métodos escritos pelo autor.", discussao = "Discussão escrita pelo autor.",
  conclusao = "Conclusão escrita pelo autor.")
qmd_autoral <- exportacao_gerar_qmd(autoral)
stopifnot(all(unlist(autoral$secoes_globais) %in% qmd_autoral),
  !any(grepl("`r texto_conclusao`", qmd_autoral, fixed = TRUE)),
  !any(grepl("morfometria_barbo", exportacao_gerar_qmd(manifesto), fixed = TRUE)))
for (documento in documentos) {
  texto <- exportacao_regressao_projeto_qmd(documento, autoral, "Teste", list(source = "local"))
  stopifnot(all(unlist(autoral$secoes_globais) %in% texto),
            !any(grepl("cinco populações", texto, fixed = TRUE)))
}
# Script e cada QMD rodam em processos R independentes, como no Render.
# Não basta o arquivo existir: conferimos modelo, IC e objetos de apresentação.
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
    "stopifnot(is.data.frame(tabela_coeficientes), inherits(grafico_regressao, 'ggplot'))",
    sprintf("saveRDS(list(coeficientes = coef(modelo_lm), ic = confint(modelo_lm), r2 = summary(modelo_lm)$r.squared, n = nrow(base_regressao)), %s)", literal(resultado))
  ), verificador, useBytes = TRUE)
  status <- system2(file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"),
    shQuote(verificador), stdout = log, stderr = log)
  if (status != 0L) stop(paste(readLines(log, warn = FALSE), collapse = "\n"))
  obtido <- readRDS(resultado)
  stopifnot(isTRUE(all.equal(unname(obtido$coeficientes), unname(coef(esperado_barbo)))),
            isTRUE(all.equal(unname(obtido$ic), unname(confint(esperado_barbo)))),
            isTRUE(all.equal(obtido$r2, summary(esperado_barbo)$r.squared)), obtido$n == 100L)
}
# Projetos mistos preservam o caminho de chunks sincronizados já existente.
# Duas regressões devem continuar recebendo identificadores distintos.
outra <- item_barbo
outra$id <- "execucao_0002"
outra$parametros$nivel_confianca <- .90
manifesto$execucoes$execucao_0002 <- outra
qmd_duplo <- exportacao_gerar_qmd(manifesto)
rotulos <- grep("^#\\| label:", qmd_duplo, value = TRUE)
stopifnot(!anyDuplicated(rotulos))
stopifnot(!any(grepl("`r texto_conclusao`", qmd_duplo, fixed = TRUE)))
cat("OK: coeficientes, IC, diagnósticos, direção, dados inválidos, nomes com espaços, script e dois relatórios independentes; duas regressões no fluxo legado.\n")
cat("PROJETO:", projeto, "\n")
