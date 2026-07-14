# Teste de integração dos estados analíticos da Fase 3C
# Executar a partir de inst/app.

source("app.R", encoding = "UTF-8")

dados_teste <- data.frame(
  ano = rep(2021:2025, each = 4),
  captura_t = seq(10, 48, by = 2),
  esforco_h = seq(5, 24),
  cpue = seq(2, 9.6, by = 0.4),
  comprimento_cm = seq(20, 39),
  peso_g = seq(100, 480, by = 20),
  ocorrencia = rep(c(0, 1), 10),
  sexo = factor(rep(c("F", "M"), 10)),
  maturidade = factor(rep(c("Imaturo", "Maduro"), each = 2, times = 5)),
  stringsAsFactors = FALSE
)

dados_rv <- reactive(dados_teste)
info_rv <- reactive(list(
  source = "local", file_name = "teste.xlsx", excel_sheet = "dados",
  csv_header = TRUE, csv_sep = ",", csv_dec = "."
))

testServer(mod_descr_stats_server, args = list(data_rv = dados_rv, import_info = info_rv), {
  session$setInputs(
    vars_selected = c("captura_t", "cpue"), var_group = "sexo",
    show_n = TRUE, show_nas = TRUE, show_mean = TRUE, show_median = TRUE,
    show_sd = TRUE, show_var = FALSE, show_minmax = TRUE, show_quartiles = TRUE
  )
  session$setInputs(executar_analise = 1)
  estado <- estado_execucao()
  stopifnot(
    identical(estado$analise_id, "descr_stats"),
    identical(estado$parametros$grupo, "sexo"),
    "tabela" %in% estado$saidas_disponiveis
  )
})

testServer(mod_regression_server, args = list(data_rv = dados_rv, import_info = info_rv), {
  session$setInputs(
    var_y = "peso_g", var_x = "comprimento_cm", var_group = "none",
    model_type = "linear", grp_reg = FALSE, show_eq = TRUE,
    graph_theme = "minimal", custom_title = "", custom_label_x = "", custom_label_y = ""
  )
  session$setInputs(executar_analise = 1)
  estado <- estado_execucao()
  stopifnot(
    identical(estado$analise_id, "regression"),
    identical(estado$parametros$resposta, "peso_g"),
    is.finite(estado$resultado_resumo$r2)
  )
})

testServer(
  mod_regression_server,
  args = list(data_rv = dados_rv, import_info = info_rv, is_logistic = TRUE),
  {
    session$setInputs(
      dataset_entrada = "dados_analise", var_y = "ocorrencia",
      var_x = "comprimento_cm", var_group = "none", model_type = "logistico",
      grp_reg = FALSE, show_eq = TRUE, graph_theme = "minimal",
      custom_title = "", custom_label_x = "", custom_label_y = ""
    )
    session$setInputs(executar_analise = 1)
    estado <- estado_execucao()
    stopifnot(
      identical(estado$analise_id, "logistic_regression"),
      identical(estado$tipo, "regressao_logistica"),
      is.finite(estado$resultado_resumo$aic)
    )
  }
)

testServer(mod_parametric_server, args = list(data_rv = dados_rv, import_info = info_rv), {
  session$setInputs(
    test_type = "two_ind", two_var_y = "captura_t", two_var_x = "sexo",
    two_var_equal = FALSE, alternative = "two.sided", conf_level = 95,
    graph_theme = "minimal", custom_title = "", custom_label_x = "", custom_label_y = ""
  )
  session$setInputs(executar_analise = 1)
  estado <- estado_execucao()
  stopifnot(
    identical(estado$analise_id, "parametric"),
    identical(estado$parametros$resposta, "captura_t"),
    is.finite(estado$resultado_resumo$p_valor)
  )
})

testServer(mod_lines_server, args = list(data_rv = dados_rv, import_info = info_rv), {
  session$setInputs(
    var_x = "ano", var_y = "captura_t", var_group = "none",
    show_points = TRUE, line_w = 1, graph_theme = "minimal",
    legend_pos = "right", custom_title = "Captura por ano",
    custom_label_x = "Ano", custom_label_y = "Captura (t)"
  )
  session$setInputs(executar_analise = 1)
  estado <- estado_execucao()
  stopifnot(
    identical(estado$analise_id, "lines"),
    identical(estado$parametros$x, "ano"),
    identical(estado$parametros$y, "captura_t")
  )
})

testServer(
  mod_nonparametric_server,
  args = list(data_rv = dados_rv, import_info = info_rv,
              contingency_shared = NULL, fixed_test = "quiquadrado"),
  {
    session$setInputs(
      chi_source = "vars", chi_row = "sexo", chi_col = "maturidade",
      chi_yates = FALSE, chi_fisher = FALSE
    )
    session$setInputs(executar_analise = 1)
    estado <- estado_execucao()
    stopifnot(
      identical(estado$analise_id, "np_qui_quadrado"),
      identical(estado$parametros$fonte, "vars"),
      is.finite(estado$resultado_resumo$p_valor)
    )
  }
)

testServer(mod_pca_server, args = list(data_rv = dados_rv, import_info = info_rv), {
  session$setInputs(
    vars_selected = c("captura_t", "esforco_h", "cpue"),
    scale = TRUE, show_labels = FALSE, graph_theme = "minimal"
  )
  session$setInputs(executar_analise = 1)
  estado <- estado_execucao()
  stopifnot(
    identical(estado$analise_id, "pca"),
    length(estado$parametros$variaveis) == 3L,
    is.finite(estado$resultado_resumo$variancia_pc1)
  )
})

testServer(mod_hca_server, args = list(data_rv = dados_rv, import_info = info_rv), {
  session$setInputs(
    vars_selected = c("captura_t", "esforco_h", "cpue"),
    distance_method = "euclidean", linkage_method = "ward.D2",
    k_groups = 3, scale = TRUE, label_var = "", show_labels = FALSE
  )
  session$setInputs(executar_analise = 1)
  estado <- estado_execucao()
  stopifnot(
    identical(estado$analise_id, "hca"),
    identical(estado$resultado_resumo$numero_grupos, 3),
    estado$resultado_resumo$n == nrow(dados_teste)
  )
})

cat("OK: estados de execução dos oito módulos da Fase 3C\n")
