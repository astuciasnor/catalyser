# Registro central de execuções analíticas — Fase 3C
# ---------------------------------------------------------------------------
# O registro é deliberadamente leve: guarda vínculo, parâmetros, código já
# disponível e um resumo pequeno do resultado. Dados, modelos e gráficos não
# são duplicados; serão reconstruídos a partir da base e dos parâmetros nas
# fases de comunicação/exportação.

execucoes_saidas_validas <- c(
  "narrativa", "tabela", "grafico", "pressupostos", "diagnosticos", "console"
)

execucoes_vazio <- function() list()

execucoes_ou <- function(x, padrao) {
  if (is.null(x) || !length(x)) padrao else x
}

execucoes_validar_estado <- function(estado) {
  if (!is.list(estado)) stop("O estado da análise deve ser uma lista.", call. = FALSE)
  obrigatorios <- c("analise_id", "tipo", "titulo", "parametros", "saidas_disponiveis")
  faltantes <- setdiff(obrigatorios, names(estado))
  if (length(faltantes))
    stop(sprintf("Estado da análise incompleto: %s.", paste(faltantes, collapse = ", ")),
         call. = FALSE)
  for (campo in c("analise_id", "tipo", "titulo")) {
    valor <- estado[[campo]]
    if (!is.character(valor) || length(valor) != 1L || !nzchar(trimws(valor)))
      stop(sprintf("O campo '%s' deve conter um texto não vazio.", campo), call. = FALSE)
  }
  if (!is.list(estado$parametros))
    stop("Os parâmetros da execução devem ser uma lista.", call. = FALSE)
  saidas <- unique(as.character(estado$saidas_disponiveis))
  invalidas <- setdiff(saidas, execucoes_saidas_validas)
  if (length(invalidas))
    stop(sprintf("Saídas desconhecidas: %s.", paste(invalidas, collapse = ", ")),
         call. = FALSE)
  if (!is.null(estado$resultado_resumo) && !is.list(estado$resultado_resumo))
    stop("O resumo do resultado deve ser uma lista ou NULL.", call. = FALSE)
  invisible(TRUE)
}

execucoes_normalizar_base <- function(base_contexto) {
  if (!is.list(base_contexto))
    stop("O contexto da base não está disponível.", call. = FALSE)
  base_id <- execucoes_ou(base_contexto$base_id, "dados_analise")
  derivada <- isTRUE(base_contexto$derivada)
  base_tipo <- execucoes_ou(
    base_contexto$base_tipo,
    if (identical(base_id, "dados_analise")) "compartilhada" else if (derivada) "derivada" else "outra"
  )
  list(
    base_id = as.character(base_id),
    base_objeto = as.character(execucoes_ou(base_contexto$base_objeto, base_id)),
    base_nome = as.character(execucoes_ou(base_contexto$nome_amigavel, base_id)),
    base_tipo = as.character(base_tipo),
    base_derivada = derivada,
    base_finalidade = base_contexto$finalidade,
    base_versao_receita = base_contexto$versao_receita,
    depende_origem = !identical(base_tipo, "entrada_manual")
  )
}

execucoes_criar <- function(id, estado, base_contexto, revisao_origem,
                            titulo = NULL, agora = Sys.time()) {
  execucoes_validar_estado(estado)
  if (!is.character(id) || length(id) != 1L || !grepl("^execucao_[0-9]{4}$", id))
    stop("ID de execução inválido.", call. = FALSE)
  base <- execucoes_normalizar_base(base_contexto)
  titulo_final <- trimws(execucoes_ou(titulo, estado$titulo))
  if (!nzchar(titulo_final)) titulo_final <- estado$titulo
  c(list(
    id = id,
    analise_id = estado$analise_id,
    tipo = estado$tipo,
    titulo = titulo_final,
    parametros = estado$parametros,
    saidas_disponiveis = unique(as.character(estado$saidas_disponiveis)),
    resultado_resumo = estado$resultado_resumo,
    codigo_r = estado$codigo_r,
    revisao_origem = as.integer(revisao_origem),
    criada_em = agora,
    atualizada_em = agora,
    versao = 1L
  ), base)
}

execucoes_obter <- function(registro, id) {
  if (is.null(id) || !nzchar(id)) return(NULL)
  registro[[id]]
}

execucoes_adicionar <- function(registro, execucao) {
  if (!is.list(registro)) registro <- execucoes_vazio()
  if (!is.list(execucao) || is.null(execucao$id))
    stop("Execução inválida.", call. = FALSE)
  if (!is.null(registro[[execucao$id]]))
    stop(sprintf("A execução '%s' já existe.", execucao$id), call. = FALSE)
  registro[[execucao$id]] <- execucao
  registro
}

execucoes_atualizar <- function(registro, id, estado, base_contexto,
                                revisao_origem, titulo = NULL, agora = Sys.time()) {
  anterior <- execucoes_obter(registro, id)
  if (is.null(anterior)) stop("A execução selecionada não existe mais.", call. = FALSE)
  nova <- execucoes_criar(id, estado, base_contexto, revisao_origem, titulo, agora)
  nova$criada_em <- anterior$criada_em
  nova$versao <- as.integer(anterior$versao) + 1L
  registro[[id]] <- nova
  registro
}

execucoes_remover <- function(registro, id) {
  if (is.null(execucoes_obter(registro, id))) return(registro)
  registro[[id]] <- NULL
  registro
}

execucoes_da_analise <- function(registro, analise_id) {
  Filter(function(x) identical(x$analise_id, analise_id), registro)
}

execucoes_rotulo <- function(execucao) {
  sprintf("%s — %s [%s]", execucao$id, execucao$titulo, execucao$base_objeto)
}

execucoes_estado_dependencia <- function(execucao, registro_bases, cache_bases,
                                         revisao_origem_atual) {
  if (is.null(execucao)) return("Não selecionada")
  if (!isTRUE(execucao$depende_origem)) return("Atualizada")
  if (!identical(as.integer(execucao$revisao_origem), as.integer(revisao_origem_atual)))
    return("Precisa atualizar")
  if (!identical(execucao$base_tipo, "derivada")) return("Atualizada")
  base <- bases_obter(registro_bases, execucao$base_id)
  if (is.null(base)) return("Base excluída")
  if (!identical(as.integer(base$versao), as.integer(execucao$base_versao_receita)))
    return("Precisa atualizar")
  cache <- bases_cache_obter(cache_bases, execucao$base_id)
  estado_cache <- bases_estado_cache(base, cache, revisao_origem_atual)
  if (!identical(base$estado, "pronta") || !identical(estado_cache, "Atualizada"))
    return("Precisa atualizar")
  "Atualizada"
}
