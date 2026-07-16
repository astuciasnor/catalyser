source("app.R", local = TRUE)

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

cat("OK: bases derivadas preservam estrela, cache lazy e falha isolada\n")
