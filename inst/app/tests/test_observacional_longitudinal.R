# Executar a partir de catalyser/inst/app.
# Desvio do delineamento Longitudinal Comparativo: o formulário ganha os
# momentos e o identificador da unidade repetida; a ficha grava o eixo do
# tempo; os demais observacionais seguem como antes.
library(shiny)
if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a) || !length(a)) b else a
source(file.path("modules", "ficha_planejamento.R"), encoding = "UTF-8")
source(file.path("modules", "mod_planejamento_variaveis.R"), encoding = "UTF-8")
source(file.path("modules", "mod_planejamento_observacional.R"), encoding = "UTF-8")

# 1. Longitudinal: a ficha grava eixos$tempo com os momentos, a unidade dada e
#    a marca de repetição; a análise segue NULL (sem passagem validada).
compartilhada <- reactiveVal(ficha_nova())
testServer(mod_planejamento_observacional_server,
           args = list(tipo = "longitudinal", ficha_destino_rv = compartilhada), {
  session$setInputs(
    fator_nome = "sexo", fator_niveis = "Fêmeas, Machos", n_uas = 4,
    tipo_pool = "igual", pool_unico = 1,
    momentos = "0, 30, 60 dias", coluna_unidade = "peixe",
    n_vars_resposta = 1, var_nome_1 = "peso", var_unidade_1 = "g"
  )
  session$flushReact()
  f <- ficha()
  stopifnot(
    identical(f$tipo, "longitudinal"),
    identical(f$origem, "Planejamento observacional — Longitudinal Comparativo"),
    identical(f$eixos$tempo$nomes, c("0", "30", "60 dias")),
    f$eixos$tempo$niveis == 3L,
    identical(f$eixos$grupos$nomes, c("Fêmeas", "Machos")),
    identical(f$unidade_coluna, "peixe"),
    identical(f$hierarquia$momentos, c("0", "30", "60 dias")),
    isTRUE(f$hierarquia$repeticao_temporal),
    is.null(f$analise_sugerida)
  )
  # 2. A planilha fica longa: 2 grupos × 4 UAs = 8 unidades × 3 momentos = 24
  #    linhas, cada unidade com o mesmo identificador em todos os momentos.
  tab <- tabela_coleta_dados()
  stopifnot(
    nrow(tab) == 24L,
    all(c("peixe", "sexo", "momento") %in% names(tab)),
    identical(tab$peixe[1:3], c(1L, 1L, 1L)),
    identical(tab$momento[1:3], c("0", "30", "60 dias")),
    identical(tab$momento[22:24], c("0", "30", "60 dias"))
  )
  # 3. O dicionário explica o momento e usa o nome dado ao identificador.
  dic <- dicionario_dados()
  stopifnot(dic$coluna[1] == "peixe", "momento" %in% dic$coluna)
  # 4. O texto de metodologia descreve o acompanhamento e o modelo misto.
  txt <- texto_metodologia_artigo_str()
  stopifnot(
    grepl("longitudinal comparativo", txt, fixed = TRUE),
    grepl("3 momentos", txt, fixed = TRUE),
    grepl("modelo linear misto", txt, fixed = TRUE)
  )
  # 5. A ficha longitudinal pode ser levada à ficha compartilhada.
  compartilhada(ficha())
  stopifnot(
    identical(compartilhada()$tipo, "longitudinal"),
    grepl("3 momentos", as.character(ficha_cabecalho_ui(compartilhada())), fixed = TRUE)
  )
})

# 6. O transversal segue igual ao que era: mesma origem, eixo grupos, unidade
#    ua, sem momentos, ANOVA de um fator e planilha sem coluna de momento.
testServer(mod_planejamento_observacional_server,
           args = list(tipo = "transversal_comparativo", ficha_destino_rv = NULL), {
  session$setInputs(fator_niveis = "A, B", n_uas = 2, tipo_pool = "igual", pool_unico = 1)
  session$flushReact()
  f <- ficha()
  stopifnot(
    identical(f$origem, "Planejamento observacional — Transversal comparativo"),
    identical(names(f$eixos), "grupos"),
    identical(f$unidade_coluna, "ua"),
    identical(f$analise_sugerida, "anova_um_fator"),
    is.null(f$hierarquia$momentos)
  )
  tab <- tabela_coleta_dados()
  stopifnot(nrow(tab) == 4L, "ua" %in% names(tab), !"momento" %in% names(tab))
})

# 7. Gradiente e impacto deixam de sair com a origem do transversal: a origem
#    agora acompanha o título do catálogo de cada delineamento.
testServer(mod_planejamento_observacional_server,
           args = list(tipo = "gradiente", ficha_destino_rv = NULL), {
  session$flushReact()
  stopifnot(identical(ficha()$origem, "Planejamento observacional — Estudo de gradiente"))
})

cat("OK: longitudinal — momentos, unidade repetida, planilha longa e ficha\n")
