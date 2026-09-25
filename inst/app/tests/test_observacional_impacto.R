# Executar a partir de catalyser/inst/app.
# Delineamento de impacto (CI, BA, BACI): formulário próprio com a condição dos
# sítios e os momentos antes/depois; a planilha, o dicionário, o texto de
# metodologia e o modelo recomendado mudam conforme o tipo de pergunta.
library(shiny)
if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a) || !length(a)) b else a
source(file.path("modules", "ficha_planejamento.R"), encoding = "UTF-8")
source(file.path("modules", "mod_planejamento_variaveis.R"), encoding = "UTF-8")
source(file.path("modules", "mod_planejamento_observacional.R"), encoding = "UTF-8")

# 1. BACI (padrão): local × tempo, sítio repetido, planilha longa.
compartilhada <- reactiveVal(ficha_nova())
testServer(mod_planejamento_observacional_server,
           args = list(tipo = "impacto", ficha_destino_rv = compartilhada), {
  session$setInputs(
    tipo_impacto = "baci",
    fator_nome = "situacao", fator_niveis = "Impacto, Referência", n_uas = 3,
    momentos = "antes, depois", coluna_unidade = "sitio",
    tipo_pool = "igual", pool_unico = 1,
    n_vars_resposta = 1, var_nome_1 = "oxigenio", var_unidade_1 = "mg/L"
  )
  session$flushReact()
  f <- ficha()
  stopifnot(
    identical(f$tipo, "impacto"),
    identical(f$origem, "Planejamento observacional — De impacto (CI, BA, BACI)"),
    identical(f$eixos$impacto$coluna, "situacao"),
    identical(f$eixos$impacto$nomes, c("Impacto", "Referência")),
    identical(f$eixos$tempo$nomes, c("antes", "depois")),
    identical(f$unidade_coluna, "sitio"),
    identical(f$hierarquia$momentos, c("antes", "depois")),
    isTRUE(f$hierarquia$repeticao_temporal),
    is.null(f$analise_sugerida)
  )
  # 2 condições × 3 sítios = 6 unidades × 2 momentos = 12 linhas.
  tab <- tabela_coleta_dados()
  stopifnot(
    nrow(tab) == 12L,
    all(c("sitio", "situacao", "momento") %in% names(tab)),
    identical(tab$sitio[1:2], c(1L, 1L)),
    identical(tab$momento[1:2], c("antes", "depois")),
    "oxigenio" %in% names(tab)
  )
  # O dicionário descreve a condição do sítio e o momento.
  dic <- dicionario_dados()
  stopifnot(all(c("sitio", "situacao", "momento") %in% dic$coluna))
  # O texto de metodologia fala em BACI e na interação local × tempo.
  txt <- texto_metodologia_artigo_str()
  stopifnot(
    grepl("BACI", txt, fixed = TRUE),
    grepl("interação", txt, fixed = TRUE)
  )
  # O modelo recomendado aponta para a interação.
  mod <- as.character(output$card_modelo_estatistico)
  stopifnot(any(grepl("interação", mod, fixed = TRUE)))
  # O total da ficha conta sítios × momentos.
  badge <- as.character(output$badge_total_uas)
  stopifnot(any(grepl("12 linhas", badge, fixed = TRUE)))
  # As sugestões de variáveis de resposta são condições do ambiente (qualidade
  # de água), não as de bancada dos delineamentos comparativos.
  campos <- as.character(output$ui_vars_resposta_campos)
  stopifnot(
    any(grepl("oxigenio_dissolvido", campos, fixed = TRUE)),
    !any(grepl("colesterol", campos, fixed = TRUE))
  )
  # A ficha pode ser levada à compartilhada.
  compartilhada(ficha())
  stopifnot(identical(compartilhada()$tipo, "impacto"))
})

# 2. CI (só depois): sem eixo de tempo, planilha como o transversal.
testServer(mod_planejamento_observacional_server,
           args = list(tipo = "impacto", ficha_destino_rv = NULL), {
  session$setInputs(
    tipo_impacto = "ci",
    fator_nome = "situacao", fator_niveis = "Impacto, Referência", n_uas = 3,
    tipo_pool = "igual", pool_unico = 1
  )
  session$flushReact()
  f <- ficha()
  stopifnot(
    identical(names(f$eixos), "impacto"),
    is.null(f$eixos$tempo),
    isFALSE(f$hierarquia$repeticao_temporal),
    identical(f$unidade_coluna, "sitio")
  )
  tab <- tabela_coleta_dados()
  stopifnot(
    nrow(tab) == 6L,
    all(c("sitio", "situacao") %in% names(tab)),
    !"momento" %in% names(tab)
  )
  txt <- texto_metodologia_artigo_str()
  stopifnot(
    grepl("Controle–Impacto", txt, fixed = TRUE),
    grepl("amostras independentes", txt, fixed = TRUE)
  )
  mod <- as.character(output$card_modelo_estatistico)
  stopifnot(any(grepl("amostras independentes", mod, fixed = TRUE)))
  badge <- as.character(output$badge_total_uas)
  stopifnot(any(grepl("6 sítios no total", badge, fixed = TRUE)))
})

# 3. BA (antes–depois): sem condição de sítio, só sítio × momento.
testServer(mod_planejamento_observacional_server,
           args = list(tipo = "impacto", ficha_destino_rv = NULL), {
  session$setInputs(
    tipo_impacto = "ba",
    n_uas = 3, momentos = "antes, depois", coluna_unidade = "sitio",
    tipo_pool = "igual", pool_unico = 1
  )
  session$flushReact()
  f <- ficha()
  stopifnot(
    identical(names(f$eixos), "tempo"),
    identical(f$eixos$tempo$nomes, c("antes", "depois")),
    isTRUE(f$hierarquia$repeticao_temporal)
  )
  tab <- tabela_coleta_dados()
  stopifnot(
    nrow(tab) == 6L,
    all(c("sitio", "momento") %in% names(tab)),
    !"situacao" %in% names(tab)
  )
  txt <- texto_metodologia_artigo_str()
  stopifnot(grepl("Antes–Depois", txt, fixed = TRUE))
  mod <- as.character(output$card_modelo_estatistico)
  stopifnot(any(grepl("pareado", mod, fixed = TRUE)))
  badge <- as.character(output$badge_total_uas)
  stopifnot(any(grepl("6 linhas", badge, fixed = TRUE)), any(grepl("sítios", badge, fixed = TRUE)))
})

cat("OK: impacto — CI/BA/BACI, sítios, momentos e modelo recomendado\n")
