# Executar a partir de catalyser/inst/app.
# Antes/depois são períodos; A01/D01 identificam as campanhas em cada período.
# A coleta preserva sítio × campanha × subamostra, inclusive no CI (só depois).
library(shiny)
if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a) || !length(a)) b else a
source(file.path("modules", "ficha_planejamento.R"), encoding = "UTF-8")
source(file.path("modules", "mod_planejamento_variaveis.R"), encoding = "UTF-8")
source(file.path("modules", "mod_planejamento_observacional.R"), encoding = "UTF-8")

# 1. BACI: dois períodos, campanhas identificadas e revisitas dos mesmos sítios.
compartilhada <- reactiveVal(ficha_nova())
testServer(mod_planejamento_observacional_server,
           args = list(tipo = "impacto", ficha_destino_rv = compartilhada), {
  session$setInputs(
    tipo_impacto = "baci",
    fator_nome = "situacao", fator_niveis = "Impacto, Referência",
    n_uas = 3, n_referencia = 3, coluna_unidade = "sitio",
    impacto_antes = 2, impacto_depois = 2, impacto_intervalo = 3,
    impacto_inicio = "2028-01-01",
    impacto_primeira_antes = "2027-04-15", impacto_primeira_depois = "2028-01-15",
    impacto_subamostras = 2,
    n_vars_resposta = 1, var_nome_1 = "oxigenio", var_unidade_1 = "mg/L"
  )
  session$flushReact()
  f <- ficha()
  stopifnot(
    identical(f$tipo, "impacto"),
    identical(f$origem, "Planejamento observacional — Estudos de impacto (BA, CI, BACI)"),
    identical(f$eixos$impacto$coluna, "situacao"),
    identical(f$eixos$impacto$nomes, c("Impacto", "Referência")),
    identical(f$eixos$tempo$coluna, "campanha"),
    identical(f$eixos$tempo$nomes, c("A01", "A02", "D01", "D02")),
    identical(f$unidade_coluna, "sitio"),
    identical(f$hierarquia$momentos, c("A01", "A02", "D01", "D02")),
    identical(f$hierarquia$subamostra_coluna, "subamostra"),
    f$hierarquia$subamostras_por_sitio == 2L,
    isTRUE(f$hierarquia$repeticao_temporal),
    is.null(f$analise_sugerida)
  )
  campanhas <- campanhas_impacto()
  stopifnot(
    identical(campanhas$periodo, c("Antes", "Antes", "Depois", "Depois")),
    identical(campanhas$data_prevista, c("2027-04-15", "2027-07-15", "2028-01-15", "2028-04-15"))
  )
  # 6 sítios × 4 campanhas × 2 subamostras = 48 linhas, sem duplicar a chave.
  tab <- tabela_coleta_dados()
  stopifnot(
    nrow(tab) == 48L,
    all(c("sitio", "situacao", "campanha", "periodo", "subamostra", "data_prevista") %in% names(tab)),
    identical(tab$sitio[1:4], rep("S01", 4)),
    identical(tab$campanha[1:4], c("A01", "A01", "A02", "A02")),
    identical(tab$subamostra[1:4], c(1L, 2L, 1L, 2L)),
    !anyDuplicated(tab[c("sitio", "campanha", "subamostra")]),
    all(table(tab$sitio, tab$campanha) == 2L),
    "oxigenio" %in% names(tab)
  )
  dic <- dicionario_dados()
  stopifnot(all(c("sitio", "situacao", "campanha", "periodo", "subamostra") %in% dic$coluna))
  txt <- texto_metodologia_artigo_str()
  stopifnot(grepl("BACI", txt, fixed = TRUE), grepl("A01 2027-04-15", txt, fixed = TRUE))
  mod <- as.character(output$card_modelo_estatistico)
  stopifnot(any(grepl("interação", mod, fixed = TRUE)))
  badge <- as.character(output$badge_total_uas)
  stopifnot(any(grepl("48 linhas", badge, fixed = TRUE)))
  campos <- as.character(output$ui_vars_resposta_campos)
  stopifnot(any(grepl("oxigenio_dissolvido", campos, fixed = TRUE)),
            !any(grepl("colesterol", campos, fixed = TRUE)))
  compartilhada(ficha())
  stopifnot(identical(compartilhada()$tipo, "impacto"))

  # Antes não pode invadir o período posterior ao início do impacto.
  session$setInputs(impacto_primeira_antes = "2028-02-15")
  erro <- tryCatch(campanhas_impacto(), error = identity)
  stopifnot(inherits(erro, "shiny.silent.error"),
            grepl("As campanhas antes devem preceder", conditionMessage(erro), fixed = TRUE))
  session$setInputs(impacto_primeira_antes = "2027-04-15", impacto_primeira_depois = "2027-12-15")
  erro <- tryCatch(campanhas_impacto(), error = identity)
  stopifnot(inherits(erro, "shiny.silent.error"))
  # Depois pode começar exatamente na data de início; condições podem ter n diferentes.
  session$setInputs(impacto_primeira_depois = "2028-01-01", n_referencia = 2)
  stopifnot(campanhas_impacto()$data_prevista[3] == "2028-01-01",
            nrow(tabela_coleta_dados()) == 40L, is.null(ficha()$hierarquia$sitios_por_nivel))
})

# 2. CI: somente campanhas depois, preservando sítios, datas e subamostras.
testServer(mod_planejamento_observacional_server,
           args = list(tipo = "impacto", ficha_destino_rv = NULL), {
  session$setInputs(
    tipo_impacto = "ci", fator_nome = "situacao", fator_niveis = "Impacto, Referência",
    n_uas = 3, n_referencia = 3, coluna_unidade = "sitio",
    impacto_antes = 2, impacto_depois = 2, impacto_subamostras = 2
  )
  session$flushReact()
  f <- ficha()
  stopifnot(identical(names(f$eixos), "impacto"), is.null(f$eixos$tempo),
            isFALSE(f$hierarquia$repeticao_temporal), identical(f$unidade_coluna, "sitio"))
  tab <- tabela_coleta_dados()
  stopifnot(nrow(tab) == 24L, all(c("sitio", "situacao", "campanha", "periodo", "subamostra") %in% names(tab)),
            identical(unique(tab$campanha), c("D01", "D02")), all(tab$periodo == "Depois"),
            !anyDuplicated(tab[c("sitio", "campanha", "subamostra")]))
  txt <- texto_metodologia_artigo_str()
  stopifnot(grepl("CI", txt, fixed = TRUE))
  mod <- as.character(output$card_modelo_estatistico)
  stopifnot(any(grepl("CI", mod, fixed = TRUE)))
  badge <- as.character(output$badge_total_uas)
  stopifnot(any(grepl("6 sítios", badge, fixed = TRUE)), any(grepl("24 linhas", badge, fixed = TRUE)))
})

# 3. BA: sítios impactados revisitados, sem coluna de condição.
testServer(mod_planejamento_observacional_server,
           args = list(tipo = "impacto", ficha_destino_rv = NULL), {
  session$setInputs(
    tipo_impacto = "ba", n_uas = 3, coluna_unidade = "sitio",
    impacto_antes = 2, impacto_depois = 2, impacto_subamostras = 2
  )
  session$flushReact()
  f <- ficha()
  stopifnot(identical(names(f$eixos), "tempo"), identical(f$eixos$tempo$coluna, "campanha"),
            identical(f$eixos$tempo$nomes, c("A01", "A02", "D01", "D02")),
            isTRUE(f$hierarquia$repeticao_temporal))
  tab <- tabela_coleta_dados()
  stopifnot(nrow(tab) == 24L, all(c("sitio", "campanha", "periodo", "subamostra") %in% names(tab)),
            !"situacao" %in% names(tab), identical(unique(tab$periodo), c("Antes", "Depois")),
            all(table(tab$sitio, tab$campanha) == 2L),
            !anyDuplicated(tab[c("sitio", "campanha", "subamostra")]))
  txt <- texto_metodologia_artigo_str()
  stopifnot(grepl("BA", txt, fixed = TRUE))
  mod <- as.character(output$card_modelo_estatistico)
  stopifnot(any(grepl("resposta ~ periodo", mod, fixed = TRUE)),
            any(grepl("random = ~ 1 | sitio", mod, fixed = TRUE)))
  badge <- as.character(output$badge_total_uas)
  stopifnot(any(grepl("24 linhas", badge, fixed = TRUE)), any(grepl("sítios", badge, fixed = TRUE)))
})
cat("OK: impacto — CI/BA/BACI, campanhas, períodos, datas e subamostras\n")
