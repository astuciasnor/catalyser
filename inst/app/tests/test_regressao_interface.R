# O cálculo dos pressupostos respeita o botão de execução e a ordem confirmada.
source("app.R", encoding = "UTF-8")
dados <- reactive(data.frame(velocidade = cars$speed, distancia = cars$dist))
info <- reactive(list(source = "local", file_name = "cars.xlsx", excel_sheet = "cars"))
testServer(mod_regression_server, args = list(data_rv = dados, import_info = info), {
  session$setInputs(var_y = "distancia", var_x = "velocidade", var_group = "none",
    model_type = "linear", grp_reg = FALSE, graph_theme = "classic", show_eq = TRUE,
    custom_title = "", custom_label_x = "", custom_label_y = "",
    avaliar_autocorrelacao = FALSE)
  session$setInputs(executar_analise = 1)
  stopifnot(identical(exec_ctrl$estado(), "atualizada"),
    grepl("IC 95%", output$coef_table, fixed = TRUE),
    identical(pressupostos_reta()$`p-valor`[1:2], c("0,022", "0,031")),
    grepl("delineamento", pressupostos_reta()$Leitura[3]),
    !estado_execucao()$parametros$avaliar_autocorrelacao,
    grepl("broom::tidy", r_code_text(), fixed = TRUE))
  session$setInputs(avaliar_autocorrelacao = TRUE)
  stopifnot(identical(exec_ctrl$estado(), "pendente"))
  session$setInputs(executar_analise = 2)
  stopifnot(identical(exec_ctrl$estado(), "atualizada"),
    estado_execucao()$parametros$avaliar_autocorrelacao,
    pressupostos_reta()$`p-valor`[3] != "—")
  stopifnot(grepl("performance::check_autocorrelation", r_code_text(), fixed = TRUE))
  invisible(parse(text = r_code_text()))
})
cat("OK: tabela com IC, pressupostos da tela e mudança de ordem exigindo nova execução.\n")

# Reproduz também a devolução dos updateTextInput pelo navegador. O testServer
# sozinho não devolve essas mensagens: só mudar active_tab esconderia o defeito.
base_barbo <- reactiveVal(as.data.frame(EAPADados::morfometria_barbo))
testServer(mod_regression_server,
  args = list(data_rv = reactive(base_barbo()), import_info = info), {
  mensagens_rotulos <- list()
  session_raiz <- .subset2(session, "parent")
  enviar_original <- session_raiz$sendInputMessage
  session_raiz$sendInputMessage <- function(inputId, message) {
    for (id in c("custom_title", "custom_label_x", "custom_label_y")) {
      if (endsWith(inputId, id) && !is.null(message$value))
        mensagens_rotulos[[id]] <<- message$value
    }
    enviar_original(inputId, message)
  }
  navegar <- function(aba) {
    mensagens_rotulos <<- list()
    session$setInputs(active_tab = aba)
    if (length(mensagens_rotulos)) do.call(session$setInputs, mensagens_rotulos)
  }
  session$setInputs(var_y = "comprimento_cabeca", var_x = "distancia_pre_peitoral",
    var_group = "none", model_type = "linear", grp_reg = FALSE, conf_level = 95,
    graph_theme = "classic", show_eq = TRUE, avaliar_autocorrelacao = FALSE,
    active_tab = "Tabela de Resultados", custom_title = "Morfometria do barbo",
    custom_label_x = "Distância pré-peitoral", custom_label_y = "Comprimento da cabeça")
  session$setInputs(executar_analise = 1)
  estado_inicial <- estado_execucao()
  modelo_inicial <- model_fit()
  gatilho_inicial <- gatilho_execucao()
  for (aba in c("Reta Ajustada", "Resíduos vs Ajustados", "Normalidade (Q-Q Plot)",
                "Tabela de Resultados", "Reta Ajustada")) {
    navegar(aba)
    stopifnot(identical(exec_ctrl$estado(), "atualizada"),
      identical(model_fit(), modelo_inicial), identical(gatilho_execucao(), gatilho_inicial),
      identical(estado_execucao(), estado_inicial))
  }
  # A apresentação dos diagnósticos não muda os rótulos registrados da reta.
  session$setInputs(resid_title = "Inspeção dos resíduos", resid_label_x = "Ajustados",
    resid_label_y = "Resíduos", qq_title = "Inspeção Q-Q",
    qq_label_x = "Quantis teóricos", qq_label_y = "Quantis observados")
  stopifnot(identical(exec_ctrl$estado(), "atualizada"),
    identical(estado_execucao(), estado_inicial))
  # As mudanças efetivas continuam exigindo execução, como antes.
  session$setInputs(custom_label_y = "Cabeça (medida corrigida)")
  stopifnot(identical(exec_ctrl$estado(), "pendente"))
  session$setInputs(executar_analise = 2, conf_level = 90)
  stopifnot(identical(exec_ctrl$estado(), "atualizada"))
  session$setInputs(conf_level = 95)
  stopifnot(identical(exec_ctrl$estado(), "pendente"))
  session$setInputs(executar_analise = 3)
  base_barbo(transform(base_barbo(), comprimento_cabeca = comprimento_cabeca + 1))
  session$flushReact()
  stopifnot(identical(exec_ctrl$estado(), "pendente"))
})
cat("OK: navegação preserva execução, modelo e rótulos; mudanças efetivas continuam pendentes.\n")
