# Registro de bases derivadas da CatalyseR — Fases 3A a 3B
# ---------------------------------------------------------------------------
# Contrato puro para o modelo base/ramos em estrela. Toda base derivada nasce
# DIRETAMENTE de `dados_analise`; não existe ramo de ramo. As etapas usam o
# mesmo contrato e o mesmo replay do pipeline de tratamentos.

bases_finalidades <- c(
  "Uso geral" = "geral",
  "Gráficos" = "graficos",
  "Regressão logística" = "reg_logistica",
  "Qui-quadrado" = "qui_quadrado",
  "ANOVA" = "anova",
  "Regressão múltipla" = "reg_multipla",
  "PCA / Agrupamentos" = "multivariada",
  "Outra finalidade" = "outra"
)

bases_nomes_reservados <- c("base", "dados", "dados_brutos", "dados_analise")

bases_vazio <- function() list()
bases_cache_vazio <- function() list()

bases_slug <- function(x) {
  x <- trimws(tolower(x %||% ""))
  ascii <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  if (is.na(ascii)) ascii <- x
  ascii <- gsub("[^a-z0-9]+", "_", ascii)
  gsub("^_+|_+$", "", ascii)
}

bases_sugerir_nome_r <- function(nome_amigavel, finalidade = "geral") {
  raiz <- bases_slug(nome_amigavel)
  if (!nzchar(raiz)) raiz <- bases_slug(finalidade)
  if (!nzchar(raiz)) raiz <- "derivada"
  if (!startsWith(raiz, "base_")) raiz <- paste0("base_", raiz)
  raiz
}

bases_nome_r_valido <- function(nome) {
  is.character(nome) && length(nome) == 1L && !is.na(nome) &&
    grepl("^base_[A-Za-z][A-Za-z0-9._]*$", nome) &&
    !(nome %in% bases_nomes_reservados)
}

bases_validar_metadados <- function(registros, nome_amigavel, nome_r, ignorar_id = NULL) {
  nome_amigavel <- trimws(nome_amigavel %||% "")
  nome_r <- trimws(nome_r %||% "")
  if (!nzchar(nome_amigavel)) return("Dê um nome amigável à base derivada.")
  if (!bases_nome_r_valido(nome_r))
    return("O nome R deve começar por 'base_' e conter apenas letras, números, ponto ou sublinhado.")
  outros <- Filter(function(x) is.null(ignorar_id) || !identical(x$id, ignorar_id), registros)
  if (any(vapply(outros, function(x) identical(x$nome_r, nome_r), logical(1))))
    return(sprintf("Já existe uma base com o nome R '%s'.", nome_r))
  NULL
}

bases_novo_registro <- function(id, nome_amigavel, nome_r, finalidade = "geral",
                                descricao = "", revisao_origem = 1L) {
  agora <- Sys.time()
  list(
    id = id,
    nome_r = trimws(nome_r),
    nome_amigavel = trimws(nome_amigavel),
    origem_id = "dados_analise",
    finalidade = finalidade,
    descricao = trimws(descricao %||% ""),
    etapas = list(),
    estado = "rascunho",
    versao = 1L,
    revisao_origem = as.integer(revisao_origem),
    criada_em = agora,
    atualizada_em = agora
  )
}

bases_indice <- function(registros, id) {
  idx <- which(vapply(registros, function(x) identical(x$id, id), logical(1)))
  if (!length(idx)) NA_integer_ else idx[1]
}

bases_obter <- function(registros, id) {
  idx <- bases_indice(registros, id)
  if (is.na(idx)) NULL else registros[[idx]]
}

bases_adicionar <- function(registros, base) {
  if (!identical(base$origem_id, "dados_analise"))
    stop("Uma base derivada deve nascer diretamente de dados_analise.", call. = FALSE)
  msg <- bases_validar_metadados(registros, base$nome_amigavel, base$nome_r)
  if (!is.null(msg)) stop(msg, call. = FALSE)
  c(registros, list(base))
}

bases_renomear <- function(registros, id, nome_amigavel, nome_r, descricao = NULL) {
  idx <- bases_indice(registros, id)
  if (is.na(idx)) stop("Base derivada não encontrada.", call. = FALSE)
  msg <- bases_validar_metadados(registros, nome_amigavel, nome_r, ignorar_id = id)
  if (!is.null(msg)) stop(msg, call. = FALSE)
  registros[[idx]]$nome_amigavel <- trimws(nome_amigavel)
  registros[[idx]]$nome_r <- trimws(nome_r)
  if (!is.null(descricao)) registros[[idx]]$descricao <- trimws(descricao)
  registros[[idx]]$atualizada_em <- Sys.time()
  registros
}

bases_replay <- function(dados_analise, base, reg_tratamentos = tratamentos) {
  if (is.null(base)) return(list(df = NULL, erros = list("base" = "Base não encontrada.")))
  if (!identical(base$origem_id, "dados_analise"))
    return(list(df = NULL, erros = list("origem" = "Origem inválida: ramos de ramos não são permitidos.")))
  if (!is.data.frame(dados_analise))
    return(list(df = NULL, erros = list("origem" = "dados_analise não está disponível como data.frame.")))
  # Qualquer falha fica confinada a este ramo. O app e os ramos irmãos não caem.
  tryCatch(
    replay_pipeline(dados_analise, base$etapas %||% list(), reg = reg_tratamentos),
    error = function(e) list(df = NULL, erros = list("fatal" = conditionMessage(e)))
  )
}

bases_cache_obter <- function(cache, id) cache[[id]] %||% NULL

bases_estado_cache <- function(base, entrada_cache, revisao_origem_atual) {
  if (is.null(entrada_cache)) return("Não calculada")
  if (!identical(as.integer(entrada_cache$revisao_origem), as.integer(revisao_origem_atual)) ||
      !identical(as.integer(entrada_cache$versao_receita), as.integer(base$versao)))
    return("Desatualizada")
  if (length(entrada_cache$erros %||% list())) return("Com erro")
  "Atualizada"
}

bases_recalcular_cache <- function(dados_analise, base, revisao_origem_atual,
                                   reg_tratamentos = tratamentos,
                                   entrada_anterior = NULL) {
  resultado <- bases_replay(dados_analise, base, reg_tratamentos)
  tem_erros <- length(resultado$erros %||% list()) > 0L
  anterior_valido <- !is.null(entrada_anterior) && !is.null(entrada_anterior$df) &&
    !is.null(entrada_anterior$resultado_em %||% entrada_anterior$calculada_em)
  # Resultado parcial de replay com erro nunca vira prévia oficial. Se existir,
  # preservamos somente a última tabela que terminou sem erro.
  df_cache <- if (!tem_erros) resultado$df else if (anterior_valido) entrada_anterior$df else NULL
  resultado_em <- if (!tem_erros) {
    Sys.time()
  } else if (anterior_valido) {
    entrada_anterior$resultado_em %||% entrada_anterior$calculada_em
  } else {
    NULL
  }
  list(
    df = df_cache,
    erros = resultado$erros %||% list(),
    revisao_origem = as.integer(revisao_origem_atual),
    versao_receita = as.integer(base$versao),
    calculada_em = Sys.time(),
    resultado_em = resultado_em,
    preview_anterior = isTRUE(tem_erros && anterior_valido),
    linhas = if (is.null(df_cache)) NA_integer_ else nrow(df_cache),
    colunas = if (is.null(df_cache)) NA_integer_ else ncol(df_cache)
  )
}

bases_cache_gravar <- function(cache, id, entrada) {
  cache[[id]] <- entrada
  cache
}

bases_cache_excluir <- function(cache, id) {
  cache[[id]] <- NULL
  cache
}

bases_finalizar <- function(registros, id, cache, revisao_origem_atual) {
  idx <- bases_indice(registros, id)
  if (is.na(idx)) stop("Base derivada não encontrada.", call. = FALSE)
  entrada <- bases_cache_obter(cache, id)
  estado <- bases_estado_cache(registros[[idx]], entrada, revisao_origem_atual)
  if (!identical(estado, "Atualizada"))
    stop(sprintf("A base não pode ser finalizada: cache %s. Clique em Recalcular.",
                 tolower(estado)), call. = FALSE)
  registros[[idx]]$estado <- "pronta"
  registros[[idx]]$atualizada_em <- Sys.time()
  registros
}

bases_reabrir <- function(registros, id) {
  idx <- bases_indice(registros, id)
  if (is.na(idx)) stop("Base derivada não encontrada.", call. = FALSE)
  registros[[idx]]$estado <- "rascunho"
  registros[[idx]]$atualizada_em <- Sys.time()
  registros
}

bases_excluir <- function(registros, id) {
  idx <- bases_indice(registros, id)
  if (is.na(idx)) return(registros)
  registros[-idx]
}

bases_atualizar_etapas <- function(registros, id, etapas) {
  idx <- bases_indice(registros, id)
  if (is.na(idx)) stop("Base derivada não encontrada.", call. = FALSE)
  registros[[idx]]$etapas <- etapas %||% list()
  registros[[idx]]$versao <- as.integer(registros[[idx]]$versao) + 1L
  registros[[idx]]$estado <- "rascunho"
  registros[[idx]]$atualizada_em <- Sys.time()
  registros
}

bases_validar_edicao <- function(base) {
  if (is.null(base)) stop("Selecione uma base derivada.", call. = FALSE)
  if (!identical(base$origem_id, "dados_analise"))
    stop("A receita só pode ser editada sobre dados_analise.", call. = FALSE)
  if (!identical(base$estado, "rascunho"))
    stop("Reabra a base como rascunho antes de alterar a receita.", call. = FALSE)
  invisible(TRUE)
}

bases_tipos_redutores <- c("agrupar_sumarizar", "contingencia")

bases_indices_redutores <- function(base) {
  etapas <- base$etapas %||% list()
  which(vapply(
    etapas,
    function(etapa) etapa$tipo %in% bases_tipos_redutores,
    logical(1)
  ))
}

bases_validar_redutor_final <- function(etapas) {
  indices <- which(vapply(
    etapas %||% list(),
    function(etapa) etapa$tipo %in% bases_tipos_redutores,
    logical(1)
  ))
  if (length(indices) > 1L)
    stop(
      paste(
        "Use apenas uma etapa redutora por Base Derivada:",
        "Agrupar/Sumarizar ou Tabela de Contingência."
      ),
      call. = FALSE
    )
  if (length(indices) == 1L && indices[[1]] != length(etapas))
    stop(
      paste(
        "Agrupar/Sumarizar e Tabela de Contingência precisam ser a última",
        "etapa da receita."
      ),
      call. = FALSE
    )
  invisible(TRUE)
}

# Adicionar uma etapa valida parâmetros contra dados já disponíveis (a raiz para
# uma receita vazia ou o cache atual). Esta função nunca executa replay.
bases_adicionar_etapa <- function(registros, id, tipo, params, dados_validacao,
                                  reg_tratamentos = tratamentos) {
  base <- bases_obter(registros, id)
  bases_validar_edicao(base)
  if (length(bases_indices_redutores(base)))
    stop(
      paste(
        "Esta receita já possui uma etapa final de Agrupar/Sumarizar ou",
        "Tabela de Contingência. Atualize ou remova essa etapa antes de continuar."
      ),
      call. = FALSE
    )
  tt <- reg_tratamentos[[tipo]]
  if (is.null(tt)) stop("Tipo de tratamento não registrado.", call. = FALSE)
  if (!is.data.frame(dados_validacao))
    stop("Não há dados atuais para validar a nova etapa. Recalcule o ramo.", call. = FALSE)
  msg <- tt$validar(dados_validacao, params)
  if (!is.null(msg)) stop(msg, call. = FALSE)

  nova <- list(tipo = tipo, params = params, ativa = TRUE)
  etapas <- c(base$etapas %||% list(), list(nova))
  bases_validar_redutor_final(etapas)
  bases_atualizar_etapas(registros, id, etapas)
}

bases_substituir_redutor <- function(registros, id, indice, tipo, params,
                                     dados_analise,
                                     reg_tratamentos = tratamentos) {
  base <- bases_obter(registros, id)
  bases_validar_edicao(base)
  etapas <- base$etapas %||% list()
  indice <- as.integer(indice)
  if (is.na(indice) || indice < 1L || indice > length(etapas))
    stop("Etapa redutora não encontrada.", call. = FALSE)
  if (!etapas[[indice]]$tipo %in% bases_tipos_redutores ||
      !tipo %in% bases_tipos_redutores)
    stop("A substituição é exclusiva para Agrupar/Sumarizar e Contingência.",
         call. = FALSE)
  if (!identical(etapas[[indice]]$tipo, tipo))
    stop(
      paste(
        "A receita já termina com outro tipo de resumo.",
        "Remova-o antes de escolher uma transformação diferente."
      ),
      call. = FALSE
    )
  if (!is.data.frame(dados_analise))
    stop("A Base Compartilhada não está disponível para validar a etapa.",
         call. = FALSE)

  anteriores <- if (indice > 1L) etapas[seq_len(indice - 1L)] else list()
  entrada <- replay_pipeline(dados_analise, anteriores, reg = reg_tratamentos)
  if (length(entrada$erros %||% list()))
    stop(
      sprintf(
        "Corrija as etapas anteriores antes de atualizar o resumo: %s",
        paste(unlist(entrada$erros), collapse = "; ")
      ),
      call. = FALSE
    )
  tt <- reg_tratamentos[[tipo]]
  msg <- tt$validar(entrada$df, params)
  if (!is.null(msg)) stop(msg, call. = FALSE)

  etapas[[indice]] <- list(tipo = tipo, params = params, ativa = TRUE)
  bases_validar_redutor_final(etapas)
  bases_atualizar_etapas(registros, id, etapas)
}

bases_mover_etapa <- function(registros, id, indice, direcao = c("subir", "descer")) {
  direcao <- match.arg(direcao)
  base <- bases_obter(registros, id)
  bases_validar_edicao(base)
  etapas <- base$etapas %||% list()
  indice <- as.integer(indice)
  destino <- indice + if (identical(direcao, "subir")) -1L else 1L
  if (is.na(indice) || indice < 1L || indice > length(etapas) ||
      destino < 1L || destino > length(etapas)) return(registros)
  etapas[c(indice, destino)] <- etapas[c(destino, indice)]
  bases_validar_redutor_final(etapas)
  bases_atualizar_etapas(registros, id, etapas)
}

bases_alternar_etapa <- function(registros, id, indice) {
  base <- bases_obter(registros, id)
  bases_validar_edicao(base)
  etapas <- base$etapas %||% list()
  indice <- as.integer(indice)
  if (is.na(indice) || indice < 1L || indice > length(etapas)) return(registros)
  etapas[[indice]]$ativa <- !isTRUE(etapas[[indice]]$ativa)
  bases_validar_redutor_final(etapas)
  bases_atualizar_etapas(registros, id, etapas)
}

bases_remover_etapa <- function(registros, id, indice) {
  base <- bases_obter(registros, id)
  bases_validar_edicao(base)
  etapas <- base$etapas %||% list()
  indice <- as.integer(indice)
  if (is.na(indice) || indice < 1L || indice > length(etapas)) return(registros)
  bases_atualizar_etapas(registros, id, etapas[-indice])
}

bases_limpar_etapas <- function(registros, id) {
  base <- bases_obter(registros, id)
  bases_validar_edicao(base)
  if (!length(base$etapas %||% list())) return(registros)
  bases_atualizar_etapas(registros, id, list())
}

bases_rotulos_etapas <- function(base, reg_tratamentos = tratamentos) {
  etapas <- base$etapas %||% list()
  if (!length(etapas)) return(character(0))
  stats::setNames(
    as.character(seq_along(etapas)),
    vapply(seq_along(etapas), function(i) {
      et <- etapas[[i]]
      tt <- reg_tratamentos[[et$tipo]]
      rotulo <- if (is.null(tt)) et$tipo else tt$rotulo(et$params)
      sprintf("%d. %s%s", i, if (isTRUE(et$ativa)) "" else "[inativa] ", rotulo)
    }, character(1))
  )
}

bases_codigo <- function(base, reg_tratamentos = tratamentos) {
  linhas <- c(
    sprintf("# Base derivada: %s", base$nome_amigavel),
    "# Origem obrigatória: dados_analise",
    "dados <- dados_analise",
    ""
  )
  ativas <- Filter(function(et) isTRUE(et$ativa), base$etapas %||% list())
  if (!length(ativas)) linhas <- c(linhas, "# Nenhuma transformação específica registrada.")
  for (i in seq_along(ativas)) {
    et <- ativas[[i]]
    tt <- reg_tratamentos[[et$tipo]]
    if (is.null(tt)) next
    linhas <- c(linhas, sprintf("# Etapa %d: %s", i, tt$rotulo(et$params)),
                tt$codigo(et$params), "")
  }
  linhas <- c(linhas, sprintf("%s <- dados", base$nome_r), "", sprintf("print(%s)", base$nome_r))
  paste(linhas, collapse = "\n")
}

bases_tabela <- function(registros, cache, revisao_origem_atual) {
  if (!length(registros)) return(data.frame())
  rotulos_finalidade <- stats::setNames(names(bases_finalidades), bases_finalidades)
  entradas <- lapply(registros, function(x) bases_cache_obter(cache, x$id))
  data.frame(
    ID = vapply(registros, `[[`, character(1), "id"),
    Base = vapply(registros, `[[`, character(1), "nome_amigavel"),
    Objeto_R = vapply(registros, `[[`, character(1), "nome_r"),
    Finalidade = vapply(registros, function(x) {
      rotulo <- unname(rotulos_finalidade[x$finalidade])
      if (is.na(rotulo)) x$finalidade else rotulo
    }, character(1)),
    Preparo = vapply(registros, `[[`, character(1), "estado"),
    Cache = vapply(seq_along(registros), function(i)
      bases_estado_cache(registros[[i]], entradas[[i]], revisao_origem_atual), character(1)),
    Etapas = vapply(registros, function(x) length(x$etapas %||% list()), integer(1)),
    Linhas = vapply(entradas, function(x) if (is.null(x)) NA_integer_ else x$linhas, integer(1)),
    Colunas = vapply(entradas, function(x) if (is.null(x)) NA_integer_ else x$colunas, integer(1)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

# Gancho seguro para a Fase 3B: somente bases finalizadas, atualizadas e sem
# erros poderão aparecer nos seletores dos módulos analíticos.
bases_disponiveis_analise <- function(registros, cache, revisao_origem_atual) {
  Filter(function(base) {
    identical(base$estado, "pronta") &&
      identical(bases_estado_cache(base, bases_cache_obter(cache, base$id),
                                   revisao_origem_atual), "Atualizada")
  }, registros)
}

bases_opcoes_analise <- function(registros, cache, revisao_origem_atual,
                                 finalidade_preferida = NULL) {
  disponiveis <- bases_disponiveis_analise(registros, cache, revisao_origem_atual)
  if (length(disponiveis) && !is.null(finalidade_preferida)) {
    preferida <- vapply(disponiveis, function(base)
      identical(base$finalidade, finalidade_preferida), logical(1))
    disponiveis <- disponiveis[order(!preferida)]
  }
  raiz <- stats::setNames("dados_analise", "Base compartilhada — dados_analise")
  if (!length(disponiveis)) return(raiz)
  ids <- vapply(disponiveis, `[[`, character(1), "id")
  rotulos <- vapply(disponiveis, function(base) {
    destaque <- if (!is.null(finalidade_preferida) &&
                    identical(base$finalidade, finalidade_preferida)) "★ " else ""
    sprintf("%s%s — %s", destaque, base$nome_amigavel, base$nome_r)
  }, character(1))
  c(raiz, stats::setNames(ids, rotulos))
}

bases_resolver_analise <- function(chave, dados_analise, registros, cache,
                                   revisao_origem_atual) {
  chave <- chave %||% "dados_analise"
  if (identical(chave, "dados_analise")) {
    if (!is.data.frame(dados_analise))
      stop("dados_analise não está disponível.", call. = FALSE)
    return(list(
      df = dados_analise,
      base_id = "dados_analise",
      base_objeto = "dados_analise",
      nome_amigavel = "Base compartilhada",
      derivada = FALSE
    ))
  }

  base <- bases_obter(registros, chave)
  if (is.null(base)) stop("A base derivada selecionada não existe mais.", call. = FALSE)
  entrada <- bases_cache_obter(cache, base$id)
  estado_cache <- bases_estado_cache(base, entrada, revisao_origem_atual)
  if (!identical(base$estado, "pronta") || !identical(estado_cache, "Atualizada") ||
      is.null(entrada$df))
    stop(sprintf("A base '%s' não está disponível: preparo %s, cache %s.",
                 base$nome_r, base$estado, tolower(estado_cache)), call. = FALSE)
  list(
    df = entrada$df,
    base_id = base$id,
    base_objeto = base$nome_r,
    nome_amigavel = base$nome_amigavel,
    finalidade = base$finalidade,
    versao_receita = base$versao,
    derivada = TRUE
  )
}
