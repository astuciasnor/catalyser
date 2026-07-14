source("app.R", local = TRUE)

execucao_teste <- function(id, titulo, analise_id, saidas, base = "dados_analise") {
  list(
    id = id,
    analise_id = analise_id,
    tipo = analise_id,
    titulo = titulo,
    parametros = list(),
    saidas_disponiveis = saidas,
    resultado_resumo = list(),
    codigo_r = "# código de teste",
    revisao_origem = 5L,
    criada_em = Sys.time(),
    atualizada_em = Sys.time(),
    versao = 1L,
    base_id = base,
    base_objeto = base,
    base_nome = base,
    base_tipo = "compartilhada",
    base_derivada = FALSE,
    base_finalidade = "geral",
    base_versao_receita = NULL,
    depende_origem = TRUE
  )
}

e1 <- execucao_teste(
  "execucao_0001", "Regressão da captura por esforço", "regression",
  c("narrativa", "tabela", "grafico", "pressupostos", "diagnosticos", "console")
)
e2 <- execucao_teste(
  "execucao_0002", "Produção por ano", "lines", c("grafico")
)
registro_inicial <- list(execucao_0001 = e1, execucao_0002 = e2)

# Contrato puro: sincronização, seleção editorial, ordem e separação Word/Projeto R.
estado <- comunicacao_sincronizar(comunicacao_estado_vazio(), registro_inicial)
stopifnot(
  identical(estado$ordem, c("execucao_0001", "execucao_0002")),
  identical(
    estado$itens$execucao_0001$saidas_selecionadas,
    c("narrativa", "tabela", "grafico")
  ),
  identical(estado$itens$execucao_0002$saidas_selecionadas, "grafico")
)

estado <- comunicacao_definir_item(
  estado, "execucao_0001", incluir_word = TRUE,
  saidas_selecionadas = c("narrativa", "pressupostos", "inexistente"),
  saidas_disponiveis = e1$saidas_disponiveis
)
estado <- comunicacao_definir_item(
  estado, "execucao_0002", incluir_word = FALSE,
  saidas_disponiveis = e2$saidas_disponiveis
)
estado <- comunicacao_mover(estado, "execucao_0002", -1L)
plano <- comunicacao_manifesto(
  estado, registro_inicial,
  list(execucao_0001 = "Atualizada", execucao_0002 = "Atualizada")
)
stopifnot(
  identical(names(plano$execucoes), c("execucao_0002", "execucao_0001")),
  identical(plano$total_execucoes, 2L),
  identical(plano$total_word, 1L),
  identical(
    plano$execucoes$execucao_0001$saidas_word,
    c("narrativa", "pressupostos")
  )
)

# O módulo acompanha novas execuções, preserva decisões e produz o manifesto.
registro_rv <- reactiveVal(registro_inicial)
projeto_rv <- reactiveVal("projeto_1")
ramo_teste <- bases_novo_registro(
  "base_0001", "Base para regressão logística", "base_reg_logistica",
  finalidade = "reg_logistica", revisao_origem = 5L
)
ramo_teste$estado <- "pronta"
cache_teste <- list(
  base_0001 = list(
    df = data.frame(x = 1:3), erros = list(), revisao_origem = 5L,
    versao_receita = ramo_teste$versao, linhas = 3L, colunas = 1L
  )
)
testServer(
  mod_comunicacao_server,
  args = list(
    dados_analise = reactive(data.frame(ano = 2020:2022, captura = c(10, 12, 15))),
    import_info = reactive(list()),
    registro_execucoes_rv = registro_rv,
    registro_bases_rv = reactiveVal(list(ramo_teste)),
    cache_bases_rv = reactiveVal(cache_teste),
    revisao_origem_rv = reactiveVal(5L),
    projeto_rv = projeto_rv
  ),
  {
    session$flushReact()
    inicial <- manifesto()
    stopifnot(
      identical(inicial$total_execucoes, 2L),
      identical(inicial$total_word, 2L),
      all(vapply(inicial$execucoes, function(x) x$estado_dependencia == "Atualizada", logical(1))),
      grepl("dados_analise", output$bases_projeto$html, fixed = TRUE),
      grepl("base_reg_logistica", output$bases_projeto$html, fixed = TRUE)
    )

    session$setInputs(
      incluir_p1_execucao_0002 = FALSE,
      saidas_p1_execucao_0001 = c("narrativa", "pressupostos"),
      introducao = "A produção pesqueira foi acompanhada durante três anos."
    )
    session$flushReact()
    editado <- manifesto()
    stopifnot(
      identical(editado$total_execucoes, 2L),
      identical(editado$total_word, 1L),
      identical(
        editado$execucoes$execucao_0001$saidas_word,
        c("narrativa", "pressupostos")
      ),
      identical(
        editado$secoes_globais$introducao,
        "A produção pesqueira foi acompanhada durante três anos."
      )
    )

    session$setInputs(subir_p1_execucao_0002 = 1)
    session$flushReact()
    stopifnot(identical(names(manifesto()$execucoes)[1], "execucao_0002"))

    e3 <- execucao_teste(
      "execucao_0003", "Teste t de comprimento por sexo", "parametric",
      c("narrativa", "tabela", "grafico", "pressupostos")
    )
    registro_rv(c(registro_rv(), list(execucao_0003 = e3)))
    session$flushReact()
    com_nova <- manifesto()
    stopifnot(
      identical(com_nova$total_execucoes, 3L),
      identical(names(com_nova$execucoes)[3], "execucao_0003"),
      isTRUE(com_nova$execucoes$execucao_0003$incluir_word)
    )

    registro_rv(registro_rv()[setdiff(names(registro_rv()), "execucao_0001")])
    session$flushReact()
    sem_primeira <- manifesto()
    stopifnot(
      identical(sem_primeira$total_execucoes, 2L),
      is.null(sem_primeira$execucoes$execucao_0001)
    )

    # Um novo projeto pode reutilizar execucao_0001 sem herdar seus controles.
    registro_rv(list())
    projeto_rv("projeto_2")
    session$flushReact()
    registro_rv(list(execucao_0001 = e1))
    session$flushReact()
    reiniciado <- manifesto()
    stopifnot(
      identical(reiniciado$total_execucoes, 1L),
      identical(reiniciado$total_word, 1L),
      identical(
        reiniciado$execucoes$execucao_0001$saidas_word,
        c("narrativa", "tabela", "grafico")
      )
    )
  }
)

cat("OK: Fase 3D organiza execuções e separa Projeto R do conteúdo do Word\n")
