# Estado editorial da Comunicação de Resultados — Fase 3D
# ---------------------------------------------------------------------------
# O registro analítico continua sendo a fonte da verdade do que foi executado.
# Este arquivo guarda apenas decisões editoriais do Word: ordem, inclusão e
# componentes escolhidos. Desmarcar um item nunca remove a execução do Projeto R.

comunicacao_rotulos_saidas <- execucoes_rotulos_saidas

# O console cru NAO entra no relatorio. Ele continua existindo na interface,
# para o aluno reconhecer a saida do R sem a camada da CatalyseR, e continua no
# objeto do replay para quem quiser inspecionar. Mas nao e oferecido como
# conteudo do Word: relatorio de artigo e tese nao leva despejo de console.
#
# A exclusao fica aqui, num lugar so, e vale para todas as analises.
comunicacao_saidas_fora_do_relatorio <- "console"

comunicacao_saidas_relatorio <- function() {
  setdiff(names(comunicacao_rotulos_saidas), comunicacao_saidas_fora_do_relatorio)
}

comunicacao_estado_vazio <- function() {
  list(versao = 1L, ordem = character(), itens = list())
}

comunicacao_saidas_padrao <- function(execucao) {
  disponiveis <- intersect(
    unique(as.character(execucao$saidas_disponiveis %||% character())),
    comunicacao_saidas_relatorio()
  )
  principais <- intersect(c("narrativa", "tabela", "grafico"), disponiveis)
  if (length(principais)) principais else head(disponiveis, 1L)
}

comunicacao_sincronizar <- function(estado, registro_execucoes) {
  if (!is.list(estado) || is.null(estado$ordem) || is.null(estado$itens))
    estado <- comunicacao_estado_vazio()
  if (!is.list(registro_execucoes)) registro_execucoes <- list()

  ids <- names(registro_execucoes)
  if (is.null(ids)) ids <- character()
  ordem <- c(intersect(estado$ordem, ids), setdiff(ids, estado$ordem))
  itens <- list()

  for (id in ordem) {
    execucao <- registro_execucoes[[id]]
    anterior <- estado$itens[[id]]
    disponiveis <- intersect(
      unique(as.character(execucao$saidas_disponiveis %||% character())),
      comunicacao_saidas_relatorio()
    )
    if (is.null(anterior)) {
      incluir <- TRUE
      selecionadas <- comunicacao_saidas_padrao(execucao)
    } else {
      incluir <- isTRUE(anterior$incluir_word)
      # Uma seleção vazia pode ser intencional e deve ser preservada.
      selecionadas <- intersect(
        unique(as.character(anterior$saidas_selecionadas %||% character())),
        disponiveis
      )
    }
    itens[[id]] <- list(
      execucao_id = id,
      incluir_word = incluir,
      saidas_selecionadas = selecionadas
    )
  }

  list(versao = 1L, ordem = ordem, itens = itens)
}

comunicacao_definir_item <- function(estado, id, incluir_word = NULL,
                                     saidas_selecionadas = NULL,
                                     saidas_disponiveis = comunicacao_saidas_relatorio()) {
  if (is.null(estado$itens[[id]]))
    stop("A execução não pertence ao estado editorial.", call. = FALSE)
  item <- estado$itens[[id]]
  if (!is.null(incluir_word)) item$incluir_word <- isTRUE(incluir_word)
  if (!is.null(saidas_selecionadas)) {
    item$saidas_selecionadas <- intersect(
      unique(as.character(saidas_selecionadas)),
      intersect(saidas_disponiveis, comunicacao_saidas_relatorio())
    )
  }
  estado$itens[[id]] <- item
  estado
}

comunicacao_mover <- function(estado, id, direcao) {
  posicao <- match(id, estado$ordem)
  if (is.na(posicao) || !direcao %in% c(-1L, 1L)) return(estado)
  destino <- posicao + direcao
  if (destino < 1L || destino > length(estado$ordem)) return(estado)
  nova_ordem <- estado$ordem
  nova_ordem[c(posicao, destino)] <- nova_ordem[c(destino, posicao)]
  estado$ordem <- nova_ordem
  estado
}

comunicacao_manifesto <- function(estado, registro_execucoes,
                                  estados_dependencia = list(), secoes_globais = list()) {
  estado <- comunicacao_sincronizar(estado, registro_execucoes)
  execucoes <- lapply(estado$ordem, function(id) {
    execucao <- registro_execucoes[[id]]
    item <- estado$itens[[id]]
    list(
      id = id,
      analise_id = execucao$analise_id,
      tipo = execucao$tipo,
      titulo = execucao$titulo,
      base_id = execucao$base_id,
      base_objeto = execucao$base_objeto,
      base_tipo = execucao$base_tipo,
      parametros = execucao$parametros,
      codigo_r = execucao$codigo_r,
      incluir_word = isTRUE(item$incluir_word),
      saidas_word = item$saidas_selecionadas,
      saidas_disponiveis = execucao$saidas_disponiveis,
      estado_dependencia = estados_dependencia[[id]] %||% "Não verificada"
    )
  })
  names(execucoes) <- estado$ordem
  list(
    versao = 1L,
    total_execucoes = length(execucoes),
    total_word = sum(vapply(execucoes, `[[`, logical(1), "incluir_word")),
    secoes_globais = secoes_globais,
    execucoes = execucoes
  )
}
