source("app.R", local = TRUE)

html_bases <- htmltools::renderTags(mod_bases_derivadas_ui("teste_bases"))$html
codigo_bases <- paste(
  readLines("modules/mod_bases_derivadas.R", encoding = "UTF-8"),
  collapse = "\n"
)
ids_preservados <- c(
  "nome_amigavel", "finalidade", "nome_r", "descricao", "criar",
  "tabela", "preview", "codigo", "detalhes", "renomear", "recalcular",
  "finalizar", "reabrir", "excluir", "editor_base", "ramo_tipo",
  "parametros_etapa", "adicionar_etapa", "receita_ramo",
  "etapa_subir", "etapa_descer", "etapa_alternar",
  "etapa_remover", "etapas_limpar"
)

stopifnot(
  grepl('id="teste_bases-bases_derivadas_subabas"', html_bases, fixed = TRUE),
  grepl("Criação e gestão da base", html_bases, fixed = TRUE),
  grepl("Receita da base", html_bases, fixed = TRUE),
  grepl("Base ativa:", codigo_bases, fixed = TRUE),
  grepl("col_widths = c(3, 7, 2)", codigo_bases, fixed = TRUE),
  grepl('min_height = "5rem"', codigo_bases, fixed = TRUE),
  grepl("seletor.selectize.enable()", codigo_bases, fixed = TRUE),
  grepl("var seletores = ['%s', '%s']", codigo_bases, fixed = TRUE),
  grepl('id="teste_bases-seletor_etapa"', html_bases, fixed = TRUE),
  grepl('ns("etapa_selecionada"), "Etapa selecionada:"', codigo_bases,
        fixed = TRUE),
  grepl("output$seletor_etapa <- renderUI", codigo_bases, fixed = TRUE),
  grepl("choices = escolhas, selected = atual", codigo_bases, fixed = TRUE),
  grepl("etapa_preferida_rv <- reactiveVal(NULL)", codigo_bases, fixed = TRUE),
  grepl("base_atualizada <- bases_obter(novo, base$id)", codigo_bases,
        fixed = TRUE),
  grepl('id="teste_bases-controles_receita" disabled="disabled"',
        html_bases, fixed = TRUE),
  grepl('class="d-grid mt-3 pt-3 border-top"', html_bases, fixed = TRUE),
  grepl("Atualizar a contingência existente", codigo_bases, fixed = TRUE),
  grepl("segunda contingência.", codigo_bases, fixed = TRUE),
  grepl('formatRound(tabela, columns = "percentual", digits = 2)',
        codigo_bases, fixed = TRUE),
  all(vapply(
    ids_preservados,
    function(id) grepl(sprintf('id="teste_bases-%s"', id), html_bases, fixed = TRUE),
    logical(1)
  ))
)

dados <- data.frame(
  id = 1:4,
  captura = c(10, 12, 15, 18),
  especie = c("A", "A", "B", "B"),
  stringsAsFactors = FALSE
)

registro_teste <- list(
  multiplicar = list(
    validar = function(df, params) {
      if (!params$coluna %in% names(df)) return("coluna ausente")
      if (!is.numeric(df[[params$coluna]])) return("coluna não numérica")
      NULL
    },
    aplicar = function(df, params) {
      df[[params$destino]] <- df[[params$coluna]] * params$fator
      df
    },
    rotulo = function(params) {
      sprintf("Multiplicar %s por %s", params$coluna, params$fator)
    },
    codigo = function(params) {
      sprintf("dados$%s <- dados$%s * %s", params$destino, params$coluna, params$fator)
    }
  )
)

registros <- bases_vazio()
cache <- bases_cache_vazio()
base <- bases_novo_registro(
  "base_0001", "Captura preparada", "base_captura",
  finalidade = "graficos", revisao_origem = 1L
)

# A topologia em estrela é uma regra estrutural, não apenas uma convenção da UI.
stopifnot(identical(base$origem_id, "dados_analise"))
ramo_de_ramo <- base
ramo_de_ramo$origem_id <- "base_antecessora"
erro_topologia <- try(bases_adicionar(registros, ramo_de_ramo), silent = TRUE)
stopifnot(inherits(erro_topologia, "try-error"))

registros <- bases_adicionar(registros, base)
stopifnot(identical(bases_estado_cache(base, NULL, 1L), "Não calculada"))

# O replay inicial é lazy: cadastrar a base não cria nem altera o cache.
stopifnot(length(cache) == 0L)
entrada <- bases_recalcular_cache(
  dados, base, 1L, reg_tratamentos = registro_teste
)
stopifnot(identical(entrada$df, dados), !length(entrada$erros))
cache <- bases_cache_gravar(cache, base$id, entrada)
stopifnot(identical(bases_estado_cache(base, entrada, 1L), "Atualizada"))

registros <- bases_finalizar(registros, base$id, cache, 1L)
base <- bases_obter(registros, base$id)
stopifnot(identical(base$estado, "pronta"))
stopifnot(base$id %in% unname(bases_opcoes_analise(registros, cache, 1L)))

# Editar a receita incrementa sua versão e invalida o cache sem recalculá-lo.
registros <- bases_reabrir(registros, base$id)
cache_antes <- cache
registros <- bases_adicionar_etapa(
  registros, base$id, "multiplicar",
  list(coluna = "captura", destino = "captura_kg", fator = 1000),
  dados_validacao = dados, reg_tratamentos = registro_teste
)
base <- bases_obter(registros, base$id)
stopifnot(base$versao == 2L, identical(cache, cache_antes))
stopifnot(identical(bases_estado_cache(base, entrada, 1L), "Desatualizada"))
stopifnot(!base$id %in% unname(bases_opcoes_analise(registros, cache, 1L)))

entrada_atual <- bases_recalcular_cache(
  dados, base, 1L, reg_tratamentos = registro_teste,
  entrada_anterior = entrada
)
stopifnot(identical(entrada_atual$df$captura_kg, dados$captura * 1000))
cache <- bases_cache_gravar(cache, base$id, entrada_atual)
registros <- bases_finalizar(registros, base$id, cache, 1L)
base <- bases_obter(registros, base$id)

resolvida <- bases_resolver_analise(base$id, dados, registros, cache, 1L)
stopifnot(isTRUE(resolvida$derivada))
stopifnot(identical(resolvida$base_objeto, "base_captura"))
stopifnot(identical(resolvida$df, entrada_atual$df))

# Se a origem perder uma coluna, o ramo falha isoladamente e preserva a última
# prévia válida; ele deixa de aparecer nos seletores até novo recálculo válido.
entrada_erro <- bases_recalcular_cache(
  data.frame(id = 1:4), base, 1L,
  reg_tratamentos = registro_teste,
  entrada_anterior = entrada_atual
)
stopifnot(length(entrada_erro$erros) == 1L)
stopifnot(isTRUE(entrada_erro$preview_anterior))
stopifnot(identical(entrada_erro$df, entrada_atual$df))
stopifnot(identical(bases_estado_cache(base, entrada_erro, 1L), "Com erro"))
cache_erro <- bases_cache_gravar(cache, base$id, entrada_erro)
stopifnot(!base$id %in% unname(bases_opcoes_analise(registros, cache_erro, 1L)))

# Alterar a revisão da base compartilhada também invalida o ramo sem cascata.
stopifnot(identical(bases_estado_cache(base, entrada_atual, 2L), "Desatualizada"))
stopifnot(grepl("dados <- dados_analise", bases_codigo(base, registro_teste), fixed = TRUE))

# Agrupar/Sumarizar e Contingência são etapas reais: alteram dimensões, cache
# e código do ramo, em vez de manter uma cópia vazia de dados_analise.
dados_resumo <- data.frame(
  especie = c("bagre", "bagre", "corvina", "corvina", "corvina", "corvina"),
  local = c("norte", "sul", "norte", "norte", "sul", "sul"),
  peso_g = c(100, 120, 200, 220, 240, 260),
  stringsAsFactors = FALSE
)

registro_resumo <- bases_adicionar(
  bases_vazio(),
  bases_novo_registro(
    "base_0100", "Resumo por espécie", "base_resumo_especie",
    finalidade = "geral", revisao_origem = 1L
  )
)
registro_resumo <- bases_adicionar_etapa(
  registro_resumo, "base_0100", "agrupar_sumarizar",
  list(
    grupos = "especie",
    variaveis = "peso_g",
    funcoes = c("n", "media"),
    ordenar = TRUE
  ),
  dados_validacao = dados_resumo
)
base_resumo <- bases_obter(registro_resumo, "base_0100")
cache_resumo <- bases_recalcular_cache(dados_resumo, base_resumo, 1L)
stopifnot(
  cache_resumo$linhas == 2L,
  cache_resumo$colunas == 3L,
  identical(names(cache_resumo$df), c("especie", "n", "peso_g_media")),
  identical(cache_resumo$df$n, c(2L, 4L)),
  grepl("dplyr::group_by(especie)", bases_codigo(base_resumo), fixed = TRUE)
)

registro_cont <- bases_adicionar(
  bases_vazio(),
  bases_novo_registro(
    "base_0101", "Contingência espécie por local", "base_cont_especie_local",
    finalidade = "qui_quadrado", revisao_origem = 1L
  )
)
registro_cont <- bases_adicionar_etapa(
  registro_cont, "base_0101", "contingencia",
  list(linha = "especie", coluna = "local", percentual = "row"),
  dados_validacao = dados_resumo
)
base_cont <- bases_obter(registro_cont, "base_0101")
cache_cont <- bases_recalcular_cache(dados_resumo, base_cont, 1L)
meta_cont <- attr(cache_cont$df, "catalyser_contingencia", exact = TRUE)
stopifnot(
  cache_cont$linhas == 4L,
  cache_cont$colunas == 4L,
  identical(names(cache_cont$df), c("especie", "local", "n", "percentual")),
  sum(cache_cont$df$n) == nrow(dados_resumo),
  identical(meta_cont$var_row, "especie"),
  identical(meta_cont$var_col, "local"),
  identical(meta_cont$freq, "n"),
  grepl("dplyr::count(especie, local", bases_codigo(base_cont), fixed = TRUE),
  grepl("tidyr::complete(especie, local", bases_codigo(base_cont), fixed = TRUE)
)

# Uma segunda contingência não pode contar novamente as linhas já resumidas.
# Alterar de contagens para percentuais substitui a etapa final existente.
erro_cont_duplicada <- tryCatch(
  {
    bases_adicionar_etapa(
      registro_cont, "base_0101", "contingencia",
      list(linha = "especie", coluna = "local", percentual = "total"),
      cache_cont$df
    )
    NULL
  },
  error = function(e) e
)
stopifnot(
  inherits(erro_cont_duplicada, "error"),
  grepl("já possui uma etapa final", conditionMessage(erro_cont_duplicada),
        fixed = TRUE)
)

registro_cont_pct <- bases_substituir_redutor(
  registro_cont, "base_0101", 1L, "contingencia",
  list(linha = "especie", coluna = "local", percentual = "row"),
  dados_resumo
)
base_cont_pct <- bases_obter(registro_cont_pct, "base_0101")
cache_cont_pct <- bases_recalcular_cache(dados_resumo, base_cont_pct, 1L)
stopifnot(
  length(base_cont_pct$etapas) == 1L,
  identical(base_cont_pct$etapas[[1]]$params$percentual, "row"),
  sum(cache_cont_pct$df$n) == nrow(dados_resumo),
  max(cache_cont_pct$df$n) == 2L,
  all(abs(
    tapply(cache_cont_pct$df$percentual, cache_cont_pct$df$especie, sum) - 100
  ) < 1e-10)
)

# Duas contingências prontas e atualizadas permanecem simultaneamente
# selecionáveis no Qui-quadrado; a finalidade apenas ordena, não filtra.
cache_duas_cont <- bases_cache_gravar(
  bases_cache_vazio(), "base_0101", cache_cont_pct
)
registro_duas_cont <- bases_finalizar(
  registro_cont_pct, "base_0101", cache_duas_cont, 1L
)
registro_duas_cont <- bases_adicionar(
  registro_duas_cont,
  bases_novo_registro(
    "base_0102", "Contingência local por espécie", "base_cont_local_especie",
    finalidade = "qui_quadrado", revisao_origem = 1L
  )
)
registro_duas_cont <- bases_adicionar_etapa(
  registro_duas_cont, "base_0102", "contingencia",
  list(linha = "local", coluna = "especie", percentual = "total"),
  dados_validacao = dados_resumo
)
base_cont_2 <- bases_obter(registro_duas_cont, "base_0102")
cache_cont_2 <- bases_recalcular_cache(dados_resumo, base_cont_2, 1L)
cache_duas_cont <- bases_cache_gravar(
  cache_duas_cont, "base_0102", cache_cont_2
)
registro_duas_cont <- bases_finalizar(
  registro_duas_cont, "base_0102", cache_duas_cont, 1L
)
opcoes_duas_cont <- bases_opcoes_analise(
  registro_duas_cont, cache_duas_cont, 1L,
  finalidade_preferida = "qui_quadrado"
)
stopifnot(
  all(c("base_0101", "base_0102") %in% unname(opcoes_duas_cont)),
  sum(unname(opcoes_duas_cont) != "dados_analise") == 2L
)

cat("OK: bases derivadas preservam estrela, cache lazy e falha isolada\n")
