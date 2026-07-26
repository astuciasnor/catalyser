# Módulo "Adicionar Tratamentos à Base" para a CatalyseR (Fase 2)
# ---------------------------------------------------------------------------
# Edita o PIPELINE de preparo GLOBAL (pipeline_rv, criado no app). O dados_analise
# do app e o REPLAY desse pipeline sobre a base_resolvida (ordem LOGICA); a trilha
# alimenta as analises AUTOMATICAMENTE. Recebe do app: base_rv (base_resolvida),
# replay_rv (list(df, erros)) e pipeline_rv (compartilhado). Depende de
# registro_tratamentos.R (registro tratamentos, replay_pipeline, gerar_script_preparo,
# desenhar_trilha_svg, helpers trat_*).
library(shiny)
library(bslib)
library(DT)

mod_tratar_ui <- function(id, calcular_ui = NULL) {
  ns <- NS(id)
  conteudo_tratamentos <- layout_columns(
      col_widths = c(1, 1, 1),
      style = "grid-template-columns: 3.4fr 6.1fr 2.5fr !important;",

      div(
        card(
          card_header("1. Adicionar tratamento"),
          card_body(
            style = "padding: 12px 15px;",
            tags$style(HTML(sprintf(
              "#%s .selectize-dropdown-content { max-height: min(30rem, calc(100vh - 15rem)); }",
              ns("tipo_wrapper")
            ))),
            div(
              id = ns("tipo_wrapper"),
              selectizeInput(
                ns("tipo"), "Tipo de tratamento:",
                choices = c(
                  "Dados faltantes (NA)"          = "tratar_na",
                  "Dicotomizar (0/1)"             = "dicotomizar",
                  "Padronizar / Escalar"          = "padronizar",
                  "Classes de tamanho (binning)"  = "binning",
                  "Remover duplicatas"            = "remover_duplicatas",
                  "Padronizar texto"              = "padronizar_texto",
                  "Calcular variável"             = "calcular",
                  "Reescalar (prefixo SI)"        = "reescalar"
                ),
                options = list(maxOptions = 12)
              )
            ),

            conditionalPanel(
              condition = sprintf("input['%s'] == 'tratar_na'", ns("tipo")),
              selectInput(ns("na_col"), "Coluna:", choices = NULL),
              radioButtons(ns("na_metodo"), "O que fazer com os NA?",
                choices = c("Remover as linhas" = "remover",
                            "Imputar a média" = "media",
                            "Imputar a mediana" = "mediana",
                            "Imputar a moda" = "moda",
                            "Valor constante" = "constante"),
                selected = "mediana"),
              conditionalPanel(
                condition = sprintf("input['%s'] == 'constante'", ns("na_metodo")),
                numericInput(ns("na_valor"), "Valor constante:", value = 0)),
              div(style = "font-size: 0.8rem; color: #555;",
                  textOutput(ns("na_contagem"), inline = TRUE))
            ),

            conditionalPanel(
              condition = sprintf("input['%s'] == 'dicotomizar'", ns("tipo")),
              selectInput(ns("dic_col"), "Coluna de origem:", choices = NULL),
              radioButtons(ns("dic_origem"), "Como definir o 1?",
                choices = c("Por limiar (numérica)" = "numerica",
                            "Por níveis (categórica)" = "categorica"),
                selected = "numerica", inline = TRUE),
              conditionalPanel(
                condition = sprintf("input['%s'] == 'numerica'", ns("dic_origem")),
                div(class = "d-flex gap-2",
                  selectInput(ns("dic_op"), "Operador:",
                              choices = c(">=", ">", "<=", "<"), selected = ">="),
                  numericInput(ns("dic_limiar"), "Limiar:", value = 0))
              ),
              conditionalPanel(
                condition = sprintf("input['%s'] == 'categorica'", ns("dic_origem")),
                selectizeInput(ns("dic_niveis"), "Níveis que viram 1:",
                               choices = NULL, multiple = TRUE,
                               options = list(placeholder = "escolha um ou mais...",
                                              plugins = list("remove_button")))
              ),
              textInput(ns("dic_nome"), "Nome da variável 0/1:", value = "")
            ),

            conditionalPanel(
              condition = sprintf("input['%s'] == 'padronizar'", ns("tipo")),
              selectInput(ns("pad_col"), "Coluna numérica:", choices = NULL),
              radioButtons(ns("pad_metodo"), "Método:",
                choices = c("Escore z (centralizar e dividir pelo desvio)" = "zscore",
                            "Centralizar (subtrair a média)" = "centralizar",
                            "Normalizar 0-1 (min-máx)" = "normalizar"),
                selected = "zscore"),
              textInput(ns("pad_nome"), "Nome da coluna nova:", value = "")
            ),

            conditionalPanel(
              condition = sprintf("input['%s'] == 'binning'", ns("tipo")),
              selectInput(ns("bin_col"), "Coluna numérica:", choices = NULL),
              div(class = "d-flex gap-2",
                numericInput(ns("bin_n"), "Nº de classes:", value = 4, min = 2, max = 20, step = 1),
                radioButtons(ns("bin_metodo"), "Cortes:",
                  choices = c("Amplitude igual" = "igual", "Por quantis" = "quantil"),
                  selected = "igual")),
              textInput(ns("bin_nome"), "Nome da coluna de classes:", value = ""),
              helpText("Ex.: classes de comprimento para avaliação de estoque.")
            ),

            conditionalPanel(
              condition = sprintf("input['%s'] == 'remover_duplicatas'", ns("tipo")),
              selectizeInput(ns("dup_cols"), "Colunas-chave (vazio = linha inteira):",
                             choices = NULL, multiple = TRUE,
                             options = list(placeholder = "vazio = linhas idênticas",
                                            plugins = list("remove_button"))),
              helpText("Mantém a 1ª ocorrência de cada combinação.")
            ),

            conditionalPanel(
              condition = sprintf("input['%s'] == 'padronizar_texto'", ns("tipo")),
              selectInput(ns("txt_col"), "Coluna de texto:", choices = NULL),
              radioButtons(ns("txt_metodo"), "Ação:",
                choices = c("Remover espaços extras" = "squish",
                            "minúsculas" = "minusculas",
                            "MAIÚSCULAS" = "maiusculas",
                            "Iniciais Maiúsculas" = "titulo"),
                selected = "squish"),
              helpText("Reescreve a própria coluna (uniformiza nomes).")
            ),

            conditionalPanel(
              condition = sprintf("input['%s'] == 'calcular'", ns("tipo")),
              textInput(ns("calc_nome"), "Nome da variável nova:", value = "nova_variavel"),
              textInput(ns("calc_expr"), "Fórmula (use os nomes das colunas):",
                        value = "", placeholder = "100 * peso_g / comprimento_cm^3"),
              div(style = "font-size: 0.78rem; color: #555;",
                  strong("Colunas: "), textOutput(ns("calc_cols"), inline = TRUE))
            ),

            conditionalPanel(
              condition = sprintf("input['%s'] == 'reescalar'", ns("tipo")),
              selectInput(ns("re_col"), "Coluna numérica:", choices = NULL),
              selectInput(ns("re_prefixo"), "Prefixo de destino:",
                choices = c("base (x 1)" = "", "quilo k (x10^3)" = "k", "mega M (x10^6)" = "M",
                            "giga G (x10^9)" = "G", "tera T (x10^12)" = "T",
                            "mili m (x10^-3)" = "m", "micro u (x10^-6)" = "µ",
                            "nano n (x10^-9)" = "n", "pico p (x10^-12)" = "p"),
                selected = "k"),
              textInput(ns("re_nome"), "Nome da coluna nova:", value = "")
            ),

            actionButton(ns("add_etapa"), "Adicionar à Trilha da Base Compartilhada",
                         icon = icon("plus"), class = "btn-primary w-100 mt-2")
          )
        ),

        card(
          card_header("2. Tratamentos adicionados"),
          card_body(
            style = "padding: 12px 15px;",
            uiOutput(ns("trilha_display")),
            hr(style = "margin: 8px 0;"),
            selectInput(ns("etapa_sel"), "Etapa selecionada:", choices = NULL),
            div(class = "d-flex gap-2",
              actionButton(ns("subir"), NULL, icon = icon("arrow-up"),
                           class = "btn-outline-secondary btn-sm", title = "Subir"),
              actionButton(ns("descer"), NULL, icon = icon("arrow-down"),
                           class = "btn-outline-secondary btn-sm", title = "Descer"),
              actionButton(ns("ativar"), NULL, icon = icon("power-off"),
                           class = "btn-outline-secondary btn-sm", title = "Ativar/Desativar"),
              actionButton(ns("remover"), NULL, icon = icon("trash"),
                           class = "btn-outline-danger btn-sm", title = "Remover"),
              actionButton(ns("limpar"), NULL, icon = icon("broom"),
                           class = "btn-outline-danger btn-sm", title = "Limpar toda a trilha")),
            helpText("Reordenar re-deriva os dados como se a etapa sempre estivesse ali.")
          )
        )
      ),

      navset_card_tab(
        nav_panel(
          title = "Resultado", icon = icon("table"),
          card_body(style = "padding: 10px 15px;", DTOutput(ns("preview_depois")))
        ),
        nav_panel(
          title = "Original", icon = icon("table-list"),
          card_body(style = "padding: 10px 15px;", DTOutput(ns("preview_antes")))
        ),
        nav_panel(
          title = "Script de preparo", icon = icon("code"),
          card_body(style = "padding: 10px 15px;",
            tags$pre(style = "white-space: pre-wrap; font-size: 0.82rem;",
                     verbatimTextOutput(ns("script_preview"))))
        )
      ),

      div(
        card(
          card_header("Exportar"),
          card_body(
            style = "padding: 12px 15px;",
            uiOutput(ns("status_indicador")),
            hr(style = "margin: 10px 0;"),
            downloadButton(ns("baixar_script"), "Baixar script de preparo (.R)",
                           class = "btn-outline-secondary btn-sm w-100 mb-2"),
            downloadButton(ns("baixar_dados"), "Baixar dados preparados (.xlsx)",
                           class = "btn-outline-primary btn-sm w-100 mb-2"),
            hr(style = "margin: 10px 0;"),
            div(class = "alert alert-info", style = "padding:8px 10px; font-size:0.82rem; margin-bottom:0;",
                icon("wand-magic-sparkles"),
                " As análises já usam esta trilha automaticamente (é a camada mais externa do dataset ativo).")
          )
        ),
        card(
          card_header("Sobre a trilha"),
          card_body(
            style = "padding: 12px 15px; font-size: 0.8rem; line-height: 1.4;",
            tags$p(style = "margin: 0 0 6px;", "A trilha é a ", strong("ordem lógica"),
                   " do preparo — não a ordem em que você clicou."),
            tags$p(style = "margin: 0 0 6px;",
                   "Na Base Compartilhada, a redução de linhas fica restrita à remoção de duplicatas ou ao tratamento de dados faltantes."),
            tags$p(style = "margin: 0;", "Ela gera o script reprodutível e, no futuro, a seção ",
                   strong("Preparação dos dados"), " do relatório.")
          )
        )
      )
    )

  if (is.null(calcular_ui)) return(tagList(conteudo_tratamentos))

  tagList(
    tabsetPanel(
      id = ns("tratamentos_subabas"),
      tabPanel("Tratamentos e trilha", conteudo_tratamentos),
      tabPanel("Calculadora guiada", calcular_ui)
    )
  )
}

mod_tratar_server <- function(id, base_rv, replay_rv, pipeline_rv, import_info, base_externa_rv = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    base_data <- reactive({ req(base_rv()); base_rv() })
    resultado <- replay_rv
    df_res    <- reactive({ resultado()$df })

    # Base externa (resultado promovido do Arrumar): rotulo do 1o no + script na Secao 0.
    base_ext <- reactive({ if (is.function(base_externa_rv)) base_externa_rv() else NULL })
    base_lbl <- reactive({ be <- base_ext(); if (!is.null(be)) paste0("base: ", be$fonte) else "dados brutos" })

    observeEvent(resultado(), {
      df  <- df_res()
      num <- trat_cols_num(df)
      cat_cols <- names(df)[vapply(df, function(x) is.character(x) || is.factor(x), logical(1))]
      updateSelectInput(session, "na_col",
        choices = c("Todas as numéricas" = "__num__", stats::setNames(names(df), names(df))),
        selected = isolate(input$na_col) %||% "__num__")
      updateSelectInput(session, "dic_col", choices = names(df), selected = isolate(input$dic_col))
      updateSelectInput(session, "pad_col", choices = num, selected = isolate(input$pad_col))
      updateSelectInput(session, "bin_col", choices = num, selected = isolate(input$bin_col))
      updateSelectInput(session, "txt_col",
        choices = if (length(cat_cols)) cat_cols else names(df), selected = isolate(input$txt_col))
      updateSelectizeInput(session, "dup_cols", choices = names(df),
        selected = isolate(input$dup_cols), server = TRUE)
      updateSelectInput(session, "re_col", choices = num, selected = isolate(input$re_col))
    })

    output$na_contagem <- renderText({
      df <- df_res(); col <- input$na_col %||% ""
      if (identical(col, "__num__")) {
        num <- trat_cols_num(df)
        sprintf("%d valores ausentes em %d colunas numéricas.",
                sum(vapply(df[num], function(x) sum(is.na(x)), integer(1))), length(num))
      } else if (col %in% names(df)) {
        sprintf("%d valores ausentes em '%s'.", sum(is.na(df[[col]])), col)
      } else "—"
    })

    observeEvent(input$dic_col, {
      req(input$dic_col %in% names(df_res()))
      x <- df_res()[[input$dic_col]]
      updateRadioButtons(session, "dic_origem",
                         selected = if (is.numeric(x)) "numerica" else "categorica")
      lv <- sort(unique(as.character(x[!is.na(x)])))
      updateSelectizeInput(session, "dic_niveis", choices = lv,
                           selected = character(0), server = TRUE)
      updateTextInput(session, "dic_nome", value = paste0(input$dic_col, "_bin"))
    })

    observeEvent(list(input$pad_col, input$pad_metodo), {
      req(input$pad_col)
      suf <- c(zscore = "_z", centralizar = "_c", normalizar = "_norm")[input$pad_metodo %||% "zscore"]
      updateTextInput(session, "pad_nome", value = paste0(input$pad_col, suf))
    }, ignoreInit = TRUE)
    observeEvent(input$bin_col, {
      req(input$bin_col)
      updateTextInput(session, "bin_nome", value = paste0(input$bin_col, "_classe"))
    }, ignoreInit = TRUE)
    observeEvent(list(input$re_col, input$re_prefixo), {
      req(input$re_col)
      suf <- if (nzchar(input$re_prefixo %||% "")) input$re_prefixo else "base"
      updateTextInput(session, "re_nome", value = paste0(input$re_col, "_", suf))
    }, ignoreInit = TRUE)
    output$calc_cols <- renderText({
      df <- df_res(); if (is.null(df) || !ncol(df)) "—" else paste(names(df), collapse = ", ")
    })

    observeEvent(input$add_etapa, {
      df <- df_res(); tipo <- input$tipo
      params <- switch(tipo,
        tratar_na = list(coluna = input$na_col, metodo = input$na_metodo, valor = input$na_valor),
        dicotomizar = list(coluna = input$dic_col, origem = input$dic_origem,
                           operador = input$dic_op, limiar = input$dic_limiar,
                           niveis_1 = input$dic_niveis, nome = trimws(input$dic_nome %||% "")),
        padronizar = list(coluna = input$pad_col, metodo = input$pad_metodo,
                          nome = trimws(input$pad_nome %||% "")),
        binning = list(coluna = input$bin_col, n = input$bin_n, metodo = input$bin_metodo,
                       nome = trimws(input$bin_nome %||% "")),
        remover_duplicatas = list(colunas = input$dup_cols),
        padronizar_texto = list(coluna = input$txt_col, metodo = input$txt_metodo),
        calcular = list(nome = trimws(input$calc_nome %||% ""), expr = input$calc_expr %||% ""),
        reescalar = list(coluna = input$re_col, simbolo = input$re_prefixo, nome = trimws(input$re_nome %||% ""))
      )
      msg <- tratamentos[[tipo]]$validar(df, params)
      if (!is.null(msg)) { showNotification(msg, type = "error", duration = 8); return() }
      nova <- list(tipo = tipo, params = params, ativa = TRUE)
      pipeline_rv(c(pipeline_rv(), list(nova)))
      showNotification(sprintf("Etapa adicionada: %s", tratamentos[[tipo]]$rotulo(params)),
                       type = "message", duration = 3)
    })

    sel_idx <- reactive({ i <- suppressWarnings(as.integer(input$etapa_sel)); if (length(i)) i else NA_integer_ })

    observeEvent(input$subir, {
      i <- sel_idx(); ps <- pipeline_rv()
      if (is.na(i) || i <= 1 || i > length(ps)) return()
      ps[c(i - 1L, i)] <- ps[c(i, i - 1L)]; pipeline_rv(ps)
      updateSelectInput(session, "etapa_sel", selected = as.character(i - 1L))
    })
    observeEvent(input$descer, {
      i <- sel_idx(); ps <- pipeline_rv()
      if (is.na(i) || i < 1 || i >= length(ps)) return()
      ps[c(i, i + 1L)] <- ps[c(i + 1L, i)]; pipeline_rv(ps)
      updateSelectInput(session, "etapa_sel", selected = as.character(i + 1L))
    })
    observeEvent(input$ativar, {
      i <- sel_idx(); ps <- pipeline_rv()
      if (is.na(i) || i < 1 || i > length(ps)) return()
      ps[[i]]$ativa <- !isTRUE(ps[[i]]$ativa); pipeline_rv(ps)
    })
    observeEvent(input$remover, {
      i <- sel_idx(); ps <- pipeline_rv()
      if (is.na(i) || i < 1 || i > length(ps)) return()
      pipeline_rv(ps[-i])
    })
    observeEvent(input$limpar, {
      if (!length(pipeline_rv())) return()
      pipeline_rv(list())
      showNotification("Trilha esvaziada.", type = "message", duration = 3)
    })

    observeEvent(pipeline_rv(), {
      ps <- pipeline_rv()
      ch <- if (length(ps)) stats::setNames(
              as.character(seq_along(ps)),
              vapply(seq_along(ps), function(i)
                sprintf("%d. %s", i, tratamentos[[ps[[i]]$tipo]]$rotulo(ps[[i]]$params)),
                character(1))) else character(0)
      updateSelectInput(session, "etapa_sel", choices = ch,
                        selected = isolate(input$etapa_sel))
    }, ignoreNULL = FALSE)

    output$trilha_display <- renderUI({
      HTML(desenhar_trilha_svg(pipeline_rv(), resultado()$erros, base_label = base_lbl()))
    })

    output$preview_antes <- renderDT({
      req(base_data())
      datatable(head(base_data(), 100), options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
    })
    output$preview_depois <- renderDT({
      validate(need(length(pipeline_rv()) > 0, "Adicione uma etapa para ver o resultado aqui."))
      datatable(head(df_res(), 200), options = list(scrollX = TRUE, pageLength = 12), rownames = FALSE)
    })
    output$script_preview <- renderText({ gerar_script_preparo(pipeline_rv(), import_info(), base_extra = base_ext()$codigo) })

    output$status_indicador <- renderUI({
      ps <- pipeline_rv()
      if (!length(ps)) return(div(style = "color:#888; font-size:0.85rem;", "Nenhum tratamento adicionado."))
      r <- df_res(); n_ativas <- sum(vapply(ps, function(e) isTRUE(e$ativa), logical(1)))
      n_erros <- length(resultado()$erros)
      div(style = "font-size:0.85rem;",
          span(style = "color:#2E7D8F; font-weight:600;", "Tratamentos adicionados"), br(),
          sprintf("%d etapa(s) - %d ativa(s) - %d linhas x %d colunas", length(ps), n_ativas, nrow(r), ncol(r)),
          if (n_erros > 0) tagList(br(), span(style = "color:#E76F51; font-weight:600;",
                                              sprintf("%d etapa(s) com erro", n_erros))))
    })

    output$baixar_script <- downloadHandler(
      filename = function() paste0("preparo_dados_", Sys.Date(), ".R"),
      content  = function(file) writeLines(gerar_script_preparo(pipeline_rv(), import_info(), base_extra = base_ext()$codigo), file)
    )
    output$baixar_dados <- downloadHandler(
      filename = function() paste0("dados_preparados_", Sys.Date(), ".xlsx"),
      content  = function(file) { req(length(pipeline_rv()) > 0); writexl::write_xlsx(df_res(), file) }
    )
  })
}

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a
