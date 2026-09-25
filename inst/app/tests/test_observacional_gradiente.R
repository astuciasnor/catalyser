# Executar a partir de catalyser/inst/app.
# Estudo de gradiente: eixo contínuo definido por estações, planilha com uma
# linha por estação, ficha com eixo contínuo e regressão sugerida; os demais
# delineamentos seguem como antes.
library(shiny)
if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a) || !length(a)) b else a
source(file.path("modules", "ficha_planejamento.R"), encoding = "UTF-8")
source(file.path("modules", "mod_planejamento_variaveis.R"), encoding = "UTF-8")
source(file.path("modules", "mod_planejamento_observacional.R"), encoding = "UTF-8")

# 1. Modo gerado: distancia_fonte em m, 8 estações de 50 a 400, pool 3.
compartilhada <- reactiveVal(ficha_nova())
testServer(mod_planejamento_observacional_server,
           args = list(tipo = "gradiente", ficha_destino_rv = compartilhada), {
  session$setInputs(
    pergunta = "A abundância do caranguejo diminui com a distância do manguezal?",
    gradiente_nome = "distancia_fonte", gradiente_unidade = "m",
    modo_estacoes = "igual", estacao_inicio = 50, estacao_fim = 400, n_estacoes = 8,
    tipo_pool = "igual", pool_unico = 3,
    n_vars_resposta = 1, var_nome_1 = "abundancia", var_unidade_1 = "ind"
  )
  session$flushReact()
  # A planilha tem 8 linhas, coluna do gradiente preenchida e pool 3.
  tab <- tabela_coleta_dados()
  stopifnot(
    nrow(tab) == 8L,
    identical(names(tab)[1:3], c("estacao", "distancia_fonte", "pool")),
    identical(tab$estacao, sprintf("E%02d", 1:8)),
    identical(tab$distancia_fonte, seq(50, 400, length.out = 8)),
    all(tab$pool == 3L),
    "abundancia" %in% names(tab)
  )
  # A ficha grava o eixo contínuo, a regressão sugerida e o n das estações.
  f <- ficha()
  stopifnot(
    identical(f$tipo, "gradiente"),
    identical(f$origem, "Planejamento observacional — Estudo de gradiente"),
    isTRUE(f$eixos$gradiente$continua),
    identical(f$eixos$gradiente$coluna, "distancia_fonte"),
    identical(f$eixos$gradiente$unidade, "m"),
    identical(f$eixos$gradiente$niveis, 8L),
    identical(f$eixos$gradiente$valores, seq(50, 400, length.out = 8)),
    identical(f$unidade_coluna, "estacao"),
    identical(f$analise_sugerida, "regressao_linear_simples"),
    identical(f$n_planejado$valor, 8L),
    identical(f$n_planejado$total, 8L)
  )
  # A ficha pode ser levada à compartilhada e o cabeçalho fala em estações.
  compartilhada(ficha())
  cab <- as.character(ficha_cabecalho_ui(compartilhada()))
  stopifnot(
    identical(compartilhada()$tipo, "gradiente"),
    grepl("Estudo de gradiente", cab, fixed = TRUE),
    grepl("8 estações", cab, fixed = TRUE),
    grepl("regressão", as.character(ficha_dica_analise_ui(compartilhada())), fixed = TRUE)
  )
  # O eixo contínuo não preenche o número de grupos do Quanto amostrar.
  stopifnot(is.null(ficha_niveis(compartilhada())))
  # O texto de metodologia descreve o gradiente e a regressão.
  txt <- texto_metodologia_artigo_str()
  stopifnot(
    grepl("estudo de gradiente observacional", txt, fixed = TRUE),
    grepl("regressão linear simples", txt, fixed = TRUE),
    grepl("8 estações", txt, fixed = TRUE)
  )
  # O dicionário registra o eixo como quantitativa contínua.
  dic <- dicionario_dados()
  stopifnot(
    identical(dic$coluna[1:3], c("estacao", "distancia_fonte", "pool")),
    dic$tipo[dic$coluna == "distancia_fonte"] == "Quantitativa contínua"
  )
})

# 2. Modo livre: a planilha usa exatamente os valores digitados, em ordem.
testServer(mod_planejamento_observacional_server,
           args = list(tipo = "gradiente", ficha_destino_rv = NULL), {
  session$setInputs(
    modo_estacoes = "livre", valores_livres = "300, 50, 150, 250, 100",
    tipo_pool = "igual", pool_unico = 1, n_vars_resposta = 1, var_nome_1 = "peso"
  )
  session$flushReact()
  tab <- tabela_coleta_dados()
  stopifnot(
    nrow(tab) == 5L,
    identical(tab$distancia_fonte, c(50, 100, 150, 250, 300)),
    identical(tab$estacao, sprintf("E%02d", 1:5))
  )
})

# 3. Menos de 5 estações liga o aviso, sem bloquear a planilha.
testServer(mod_planejamento_observacional_server,
           args = list(tipo = "gradiente", ficha_destino_rv = NULL), {
  session$setInputs(
    modo_estacoes = "igual", estacao_inicio = 10, estacao_fim = 40, n_estacoes = 3,
    tipo_pool = "igual", pool_unico = 1
  )
  session$flushReact()
  stopifnot(isTRUE(poucas_estacoes()), nrow(tabela_coleta_dados()) == 3L)
})

# 4. Pool desigual por estação chega à planilha na ordem das estações.
testServer(mod_planejamento_observacional_server,
           args = list(tipo = "gradiente", ficha_destino_rv = NULL), {
  session$setInputs(
    modo_estacoes = "igual", estacao_inicio = 50, estacao_fim = 400, n_estacoes = 8,
    tipo_pool = "desigual",
    pool_grupo_1 = 2, pool_grupo_2 = 2, pool_grupo_3 = 4, pool_grupo_4 = 4,
    pool_grupo_5 = 1, pool_grupo_6 = 1, pool_grupo_7 = 3, pool_grupo_8 = 3
  )
  session$flushReact()
  stopifnot(identical(tabela_coleta_dados()$pool, c(2L, 2L, 4L, 4L, 1L, 1L, 3L, 3L)))
})

# 5. O transversal segue idêntico: eixo grupos, unidade ua, ANOVA de um fator.
testServer(mod_planejamento_observacional_server,
           args = list(tipo = "transversal_comparativo", ficha_destino_rv = NULL), {
  session$setInputs(fator_niveis = "A, B", n_uas = 2, tipo_pool = "igual", pool_unico = 1)
  session$flushReact()
  f <- ficha()
  tab <- tabela_coleta_dados()
  stopifnot(
    identical(f$origem, "Planejamento observacional — Transversal comparativo"),
    identical(names(f$eixos), "grupos"),
    identical(f$unidade_coluna, "ua"),
    identical(f$analise_sugerida, "anova_um_fator"),
    nrow(tab) == 4L
  )
})

cat("OK: estudo de gradiente — estações, planilha, ficha contínua e regressão\n")
