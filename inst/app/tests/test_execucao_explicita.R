# Testes da Fase 3C.1 — execução explícita e retomada do rascunho
# Executar a partir de inst/app.

source("app.R", encoding = "UTF-8")

codigo_execucao_explicita <- paste(
  readLines("modules/mod_execucao_explicita.R", encoding = "UTF-8"),
  collapse = "\n"
)
stopifnot(
  grepl("invisible(resultado_rv())", codigo_execucao_explicita, fixed = TRUE),
  grepl("onmousedown = HTML", codigo_execucao_explicita, fixed = TRUE),
  grepl("window.jQuery(el).trigger('change')", codigo_execucao_explicita,
        fixed = TRUE),
  grepl("onclick = HTML", codigo_execucao_explicita, fixed = TRUE),
  grepl("Shiny.setInputValue(inputId, Date.now()", codigo_execucao_explicita,
        fixed = TRUE),
  grepl("priority = 1000", codigo_execucao_explicita, fixed = TRUE),
  grepl("priority = -1000", codigo_execucao_explicita, fixed = TRUE)
)

modulos_explicitos <- c(
  "mod_description.R", "mod_hca.R", "mod_nonparametric.R",
  "mod_parametric.R", "mod_pca.R", "mod_regression.R", "mod_viz_extra.R"
)
stopifnot(all(vapply(modulos_explicitos, function(arquivo) {
  codigo <- paste(
    readLines(file.path("modules", arquivo), encoding = "UTF-8"),
    collapse = "\n"
  )
  grepl("gatilho_execucao <- reactiveVal(0L)", codigo, fixed = TRUE) &&
    grepl("eventReactive(gatilho_execucao()", codigo, fixed = TRUE) &&
    grepl("gatilho_rv = gatilho_execucao", codigo, fixed = TRUE)
}, logical(1))))

dados_iniciais <- data.frame(
  ano = 2021:2025,
  captura_t = c(10, 13, 17, 16, 21),
  esforco_h = c(4, 5, 6, 5, 7),
  stringsAsFactors = FALSE
)
dados_estado <- reactiveVal(dados_iniciais)
info_rv <- reactive(list(
  source = "local", file_name = "teste.xlsx", excel_sheet = "dados",
  csv_header = TRUE, csv_sep = ",", csv_dec = "."
))

testServer(
  mod_lines_server,
  args = list(data_rv = reactive(dados_estado()), import_info = info_rv),
  {
    session$setInputs(
      var_x = "ano", var_y = "captura_t", var_group = "none",
      show_points = TRUE, line_w = 1, graph_theme = "minimal",
      legend_pos = "right", custom_title = "Captura por ano",
      custom_label_x = "Ano", custom_label_y = "Captura (t)"
    )

    # A configuração inicial é apenas um rascunho: nenhum resultado existe.
    stopifnot(
      identical(exec_ctrl$estado(), "aguardando"),
      grepl("Nenhum resultado foi calculado", output$execucao_painel_aviso$html,
            fixed = TRUE),
      inherits(try(estado_execucao(), silent = TRUE), "try-error")
    )

    session$setInputs(executar_analise = 1)
    primeira <- estado_execucao()
    stopifnot(
      identical(exec_ctrl$estado(), "atualizada"),
      identical(primeira$parametros$x, "ano"),
      identical(primeira$parametros$y, "captura_t")
    )

    # Alterar Y não recalcula nem libera um estado novo silenciosamente.
    session$setInputs(var_y = "esforco_h", custom_label_y = "Esforço (h)")
    stopifnot(
      identical(exec_ctrl$estado(), "pendente"),
      inherits(try(estado_execucao(), silent = TRUE), "try-error"),
      grepl("Execute novamente", output$execucao_painel_aviso$html, fixed = TRUE)
    )

    session$setInputs(executar_analise = 2)
    segunda <- estado_execucao()
    stopifnot(
      identical(exec_ctrl$estado(), "atualizada"),
      identical(segunda$parametros$y, "esforco_h")
    )

    # Interromper, transformar a base e retornar marca a prévia anterior como
    # pendente; a variável nova só entra depois de outra execução explícita.
    dados_transformados <- dados_iniciais
    dados_transformados$cpue <- dados_transformados$captura_t / dados_transformados$esforco_h
    dados_estado(dados_transformados)
    session$flushReact()
    stopifnot(
      identical(exec_ctrl$estado(), "pendente"),
      identical(input$var_x, "ano"),
      identical(input$var_y, "esforco_h")
    )

    session$setInputs(
      var_y = "cpue", custom_title = "CPUE por ano",
      custom_label_y = "CPUE", executar_analise = 3
    )
    terceira <- estado_execucao()
    stopifnot(
      identical(exec_ctrl$estado(), "atualizada"),
      identical(terceira$parametros$x, "ano"),
      identical(terceira$parametros$y, "cpue")
    )
  }
)

cat("OK: execução explícita, rascunho pendente e retomada após transformação\n")
