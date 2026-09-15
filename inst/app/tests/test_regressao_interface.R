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
