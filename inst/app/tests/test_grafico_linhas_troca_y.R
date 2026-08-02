# Teste das duas escolhas sucessivas de Y no Gráfico de Linhas — V16
# Executar a partir de inst/app.
#
# O percurso testado não é um gráfico com dois eixos Y. São duas configurações
# independentes do mesmo seletor Y, cada uma registrada como execução própria.

source("app.R", encoding = "UTF-8")
source(file.path("tests", "carregar_catalyser.R"), chdir = FALSE)

# Biometria de corvinas com comprimento e peso completos (Base Derivada B).
base_graficos_corvina <- data.frame(
  id = 1:8,
  especie = rep("corvina", 8),
  comprimento_cm = c(13.4, 15.3, 18.2, 21.0, 24.6, 27.3, 30.1, 33.1),
  peso_g = c(33, 42, 71, 110, 171, 226, 289, 340),
  stringsAsFactors = FALSE
)
dados_rv <- reactive(base_graficos_corvina)
info_rv <- reactive(list(
  source = "local", file_name = "Treino-Transformacoes.xlsx",
  excel_sheet = "biometria", csv_header = TRUE, csv_sep = ",", csv_dec = "."
))

estados_registrados <- list()

testServer(mod_lines_server, args = list(data_rv = dados_rv, import_info = info_rv), {
  # --- Primeira configuração: Y = comprimento_cm ---------------------------
  session$setInputs(
    var_x = "id", var_y = "comprimento_cm", var_group = "none",
    show_points = TRUE, line_w = 1, graph_theme = "minimal",
    legend_pos = "right", custom_title = "Comprimento das corvinas por observação",
    custom_label_x = "Observação", custom_label_y = "Comprimento (cm)"
  )
  stopifnot(identical(exec_ctrl$estado(), "aguardando"))

  session$setInputs(executar_analise = 1)
  stopifnot(identical(exec_ctrl$estado(), "atualizada"))

  estado_1 <- estado_execucao()
  stopifnot(
    identical(estado_1$analise_id, "lines"),
    identical(estado_1$tipo, "grafico_linhas"),
    identical(estado_1$parametros$x, "id"),
    identical(estado_1$parametros$y, "comprimento_cm"),
    identical(
      estado_1$parametros$titulo_grafico,
      "Comprimento das corvinas por observação"
    ),
    identical(estado_1$titulo, "Comprimento das corvinas por observação")
  )
  # O gráfico executado usa mesmo comprimento_cm.
  dados_camada_1 <- ggplot2::ggplot_build(make_plot())$data[[1]]
  stopifnot(isTRUE(
    abs(max(dados_camada_1$y) - max(base_graficos_corvina$comprimento_cm)) < 1e-8
  ))

  # --- Trocar somente Y deixa a execução pendente ---------------------------
  session$setInputs(var_y = "peso_g")
  stopifnot(identical(exec_ctrl$estado(), "pendente"))

  # O gráfico não muda silenciosamente antes do clique: o eventReactive ainda
  # devolve a configuração anterior.
  dados_camada_pendente <- ggplot2::ggplot_build(make_plot())$data[[1]]
  stopifnot(
    isTRUE(abs(max(dados_camada_pendente$y) - max(base_graficos_corvina$comprimento_cm)) < 1e-8),
    identical(estado_1$parametros$y, "comprimento_cm")
  )

  # --- Segunda configuração: Y = peso_g ------------------------------------
  session$setInputs(custom_title = "Peso das corvinas por observação",
                    custom_label_y = "Peso (g)")
  session$setInputs(executar_analise = 2)
  stopifnot(identical(exec_ctrl$estado(), "atualizada"))

  estado_2 <- estado_execucao()
  stopifnot(
    identical(estado_2$parametros$y, "peso_g"),
    identical(estado_2$parametros$x, "id"),
    identical(
      estado_2$parametros$titulo_grafico,
      "Peso das corvinas por observação"
    ),
    identical(estado_2$titulo, "Peso das corvinas por observação")
  )
  dados_camada_2 <- ggplot2::ggplot_build(make_plot())$data[[1]]
  stopifnot(isTRUE(
    abs(max(dados_camada_2$y) - max(base_graficos_corvina$peso_g)) < 1e-8
  ))

  # A primeira configuração não foi sobrescrita pela segunda.
  stopifnot(
    identical(estado_1$parametros$y, "comprimento_cm"),
    !identical(estado_1$parametros$y, estado_2$parametros$y)
  )

  # Contagem honesta das observações: nenhuma linha entra ou sai em silêncio.
  stopifnot(
    identical(estado_2$resultado_resumo$n, 8L),
    identical(estado_2$resultado_resumo$descartadas, 0L)
  )

  estados_registrados <<- list(primeira = estado_1, segunda = estado_2)
})

# --- Dados faltantes são contados, não descartados em silêncio ---------------
base_com_falhas <- base_graficos_corvina
base_com_falhas$peso_g[c(2, 5)] <- NA
testServer(
  mod_lines_server,
  args = list(data_rv = reactive(base_com_falhas), import_info = info_rv),
  {
    session$setInputs(
      var_x = "id", var_y = "peso_g", var_group = "none",
      show_points = TRUE, line_w = 1, graph_theme = "minimal",
      legend_pos = "right", custom_title = "Peso das corvinas por observação",
      custom_label_x = "Observação", custom_label_y = "Peso (g)"
    )
    session$setInputs(executar_analise = 1)
    estado <- estado_execucao()
    stopifnot(
      identical(estado$resultado_resumo$n, 6L),
      identical(estado$resultado_resumo$descartadas, 2L),
      # O gráfico usa apenas as observações completas.
      nrow(ggplot2::ggplot_build(make_plot())$data[[1]]) == 6L
    )
  }
)

# =============================================================================
# As duas execuções coexistem no registro central
# =============================================================================
contexto_base_b <- list(
  base_id = "base_0002", base_objeto = "base_graficos_corvina",
  nome_amigavel = "Biometria completa das corvinas", base_tipo = "derivada",
  derivada = TRUE, finalidade = "graficos", versao_receita = 3L
)

registro <- execucoes_vazio()
registro <- execucoes_adicionar(
  registro,
  execucoes_criar("execucao_0002", estados_registrados$primeira, contexto_base_b, 7L)
)
registro <- execucoes_adicionar(
  registro,
  execucoes_criar("execucao_0003", estados_registrados$segunda, contexto_base_b, 7L)
)

stopifnot(
  length(registro) == 2L,
  identical(registro$execucao_0002$parametros$y, "comprimento_cm"),
  identical(registro$execucao_0003$parametros$y, "peso_g"),
  identical(registro$execucao_0002$base_objeto, "base_graficos_corvina"),
  identical(registro$execucao_0003$base_objeto, "base_graficos_corvina"),
  length(execucoes_da_analise(registro, "lines")) == 2L
)

# =============================================================================
# Cada replay usa a sua própria variável Y
# =============================================================================
replay_1 <- catalyser_executar(registro$execucao_0002, base_graficos_corvina)
replay_2 <- catalyser_executar(registro$execucao_0003, base_graficos_corvina)
y_1 <- ggplot2::ggplot_build(replay_1$grafico)$data[[1]]$y
y_2 <- ggplot2::ggplot_build(replay_2$grafico)$data[[1]]$y

stopifnot(
  isTRUE(abs(max(y_1) - max(base_graficos_corvina$comprimento_cm)) < 1e-8),
  isTRUE(abs(max(y_2) - max(base_graficos_corvina$peso_g)) < 1e-8),
  !isTRUE(all.equal(y_1, y_2)),
  identical(replay_1$grafico$labels$title, "Comprimento das corvinas por observação"),
  identical(replay_1$grafico$labels$x, "Observação"),
  identical(replay_1$grafico$labels$y, "Comprimento (cm)"),
  identical(replay_2$grafico$labels$title, "Peso das corvinas por observação"),
  identical(replay_2$grafico$labels$y, "Peso (g)")
)

# =============================================================================
# O QMD contém os dois códigos, cada um com o seu Y
# =============================================================================
codigo_1 <- exportacao_codigo_estudo(registro$execucao_0002)
codigo_2 <- exportacao_codigo_estudo(registro$execucao_0003)
stopifnot(
  any(grepl("comprimento_cm", codigo_1, fixed = TRUE)),
  !any(grepl("peso_g", codigo_1, fixed = TRUE)),
  any(grepl("peso_g", codigo_2, fixed = TRUE)),
  !any(grepl("comprimento_cm", codigo_2, fixed = TRUE)),
  any(grepl("dados <- base_graficos_corvina", codigo_1, fixed = TRUE)),
  any(grepl("variavel_y <-", codigo_1, fixed = TRUE)),
  any(grepl("grafico_linhas <-", codigo_1, fixed = TRUE)),
  any(grepl("titulo_grafico <-", codigo_1, fixed = TRUE)),
  # O código exportado torna a exclusão visível e reproduzível, como na ANOVA.
  any(grepl("stats::complete.cases(dados[colunas_grafico])", codigo_1, fixed = TRUE)),
  any(grepl("descartadas por dados faltantes", codigo_1, fixed = TRUE)),
  any(grepl("ggplot2::ggplot(dados_grafico, mapeamento)", codigo_1, fixed = TRUE))
)

# O replay também conta as observações e usa somente as completas.
replay_falhas <- catalyser_linhas(
  base_com_falhas,
  list(x = "id", y = "peso_g", grupo = "none", mostrar_pontos = TRUE,
       espessura_linha = 1, tema = "minimal", posicao_legenda = "right")
)
stopifnot(
  is.data.frame(replay_falhas$observacoes),
  identical(replay_falhas$observacoes$Valor, c(6L, 2L)),
  nrow(ggplot2::ggplot_build(replay_falhas$grafico)$data[[1]]) == 6L
)

# =============================================================================
# Labels do QMD: a variável distingue; o ID desempata quando ela se repete
# =============================================================================
# Com Y diferente, a própria variável já separa os dois labels.
raizes_distintas <- exportacao_raizes_chunk(registro)
stopifnot(
  identical(unname(raizes_distintas[["execucao_0002"]]), "linhas-comprimento-cm"),
  identical(unname(raizes_distintas[["execucao_0003"]]), "linhas-peso-g")
)

# Com o mesmo Y, as raízes colidiriam. O Quarto falha com labels repetidos, então
# o desempate pelo ID precisa entrar.
registro_mesmo_y <- registro
registro_mesmo_y$execucao_0003$parametros$y <- "comprimento_cm"
raizes_iguais <- exportacao_raizes_chunk(registro_mesmo_y)
stopifnot(
  identical(unname(raizes_iguais[["execucao_0002"]]), "linhas-comprimento-cm-execucao-0002"),
  identical(unname(raizes_iguais[["execucao_0003"]]), "linhas-comprimento-cm-execucao-0003"),
  !any(duplicated(unname(raizes_iguais)))
)

estado_editorial <- comunicacao_sincronizar(comunicacao_estado_vazio(), registro)
manifesto <- comunicacao_manifesto(
  estado_editorial, registro,
  stats::setNames(as.list(rep("Atualizada", 2L)), names(registro))
)
stopifnot(
  manifesto$total_execucoes == 2L,
  identical(manifesto$execucoes$execucao_0002$parametros$y, "comprimento_cm"),
  identical(manifesto$execucoes$execucao_0003$parametros$y, "peso_g")
)

cat("OK: as duas escolhas de Y no Gráfico de Linhas coexistem e não se sobrescrevem\n")
