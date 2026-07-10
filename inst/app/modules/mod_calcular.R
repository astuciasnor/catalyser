# Módulo Calcular / Reescalar variável para a CatalyseR
# ---------------------------------------------------------------------------
# Duas operações de PREPARO que criam colunas novas a partir das existentes:
#   (1) Variável calculada  — nova coluna = expressão sobre outras colunas.
#       Dois modos: GUIADO (no mouse: função · A · operador · B) e EXPRESSÃO
#       LIVRE (digitar a fórmula R com os nomes das colunas).
#   (2) Reescalar por prefixo — divide os valores por uma potência de dez e
#       anexa o prefixo SI (k, M, G, m, µ, n…). Sempre cria coluna NOVA.
# As etapas se acumulam (pilha com "Desfazer") e, ao final, geram um script .R
# reproduzível ("do mouse ao código"). O resultado é promovido às análises.
library(shiny)
library(bslib)
library(DT)

# --- Funções puras (fora do server): fáceis de testar e espelhar no livro -----

# Prefixos do Sistema Internacional (potências de mil).
calc_prefixos <- data.frame(
  simbolo = c("p", "n", "µ", "m", "", "k", "M", "G", "T"),
  nome    = c("pico", "nano", "micro", "mili", "(base)", "quilo", "mega", "giga", "tera"),
  fator   = c(1e-12, 1e-9, 1e-6, 1e-3, 1, 1e3, 1e6, 1e9, 1e12),
  stringsAsFactors = FALSE
)

# Rótulos amigáveis para o seletor de prefixo (ex.: "quilo  (k · 10^3)").
calc_prefixo_choices <- function() {
  rot <- ifelse(
    calc_prefixos$simbolo == "",
    "base  (× 1)",
    sprintf("%s  (%s · 10^%d)", calc_prefixos$nome, calc_prefixos$simbolo,
            as.integer(round(log10(calc_prefixos$fator))))
  )
  stats::setNames(calc_prefixos$simbolo, rot)
}

# Fator de um prefixo (10^n). Vetorizado; NA se símbolo desconhecido.
calc_fator <- function(simbolo) calc_prefixos$fator[match(simbolo, calc_prefixos$simbolo)]

# Reescala: exprimir os valores NO prefixo escolhido = dividir pelo fator.
# Ex.: 1 500 000 g / 1e3 = 1500 (agora em quilos, k).
calc_reescalar <- function(x, simbolo) as.numeric(x) / calc_fator(simbolo)

# Escolhe o prefixo que traz a magnitude típica para a faixa [1, 1000).
# Usa a mediana dos valores absolutos não nulos.
calc_prefixo_auto <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x) & x != 0]
  if (!length(x)) return("")
  m <- stats::median(abs(x), na.rm = TRUE)
  if (!is.finite(m) || m <= 0) return("")
  e <- 3 * floor(log10(m) / 3)                       # expoente múltiplo de 3
  e <- max(-12, min(12, e))
  calc_prefixos$simbolo[which.min(abs(log10(calc_prefixos$fator) - e))]
}

# --- Suporte ao construtor GUIADO --------------------------------------------

# Funções externas oferecidas (envolvem a expressão): rótulo -> token.
calc_funcoes_choices <- c(
  "(nenhuma)"            = "nenhuma",
  "log natural (ln)"     = "ln",
  "log base 10"          = "log10",
  "raiz quadrada"        = "sqrt",
  "exponencial (e^x)"    = "exp",
  "valor absoluto"       = "abs"
)
# token -> nome da função em R.
calc_func_r <- c(ln = "log", log10 = "log10", sqrt = "sqrt", exp = "exp", abs = "abs")

# Operadores binários: rótulo -> símbolo R (ou "nenhum").
calc_ops_choices <- c(
  "(nenhum)"          = "nenhum",
  "somar  (+)"        = "+",
  "subtrair  (−)" = "-",
  "multiplicar  (×)" = "*",
  "dividir  (÷)"  = "/",
  "potência  (^)"  = "^"
)

# Cerca com crase nomes que não são identificadores R válidos.
calc_bt <- function(x) {
  ok <- grepl("^[A-Za-z.][A-Za-z0-9._]*$", x) & !grepl("^\\.[0-9]", x)
  ifelse(ok, x, paste0("`", x, "`"))
}

# Número -> texto sem notação científica (para entrar na fórmula).
calc_num_txt <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return("0")
  format(x, scientific = FALSE, trim = TRUE)
}

# Monta a expressão R do modo guiado: funcao( A op B ).
# b_tipo: "coluna" ou "numero". Devolve a string da fórmula (RHS do mutate).
calc_montar_expr <- function(funcao, a_col, op, b_tipo, b_col, b_num) {
  a <- calc_bt(a_col)
  if (identical(op, "nenhum") || op == "") {
    base <- a
  } else {
    b <- if (identical(b_tipo, "coluna")) calc_bt(b_col) else calc_num_txt(b_num)
    base <- sprintf("(%s %s %s)", a, op, b)
  }
  if (!identical(funcao, "nenhuma") && funcao != "" && funcao %in% names(calc_func_r)) {
    sprintf("%s(%s)", calc_func_r[[funcao]], base)
  } else {
    base
  }
}

# Avalia uma expressão (guiada ou livre) no ambiente do data.frame.
# Devolve um vetor do tamanho de nrow(df); recicla constantes.
calc_avaliar <- function(expr, df) {
  val <- eval(parse(text = expr), envir = df, enclos = baseenv())
  if (length(val) == 1L) val <- rep(val, nrow(df))
  val
}

# Valida a expressão ANTES de aplicar. Devolve NULL se OK, ou a mensagem.
calc_validar_expr <- function(nome, expr, df) {
  if (!nzchar(trimws(nome)))  return("Dê um nome à variável nova.")
  if (!nzchar(trimws(expr)))  return("A fórmula está vazia.")
  parsed <- tryCatch(parse(text = expr), error = function(e) e)
  if (inherits(parsed, "error"))
    return(paste0("Fórmula inválida: ", conditionMessage(parsed)))
  val <- tryCatch(calc_avaliar(expr, df), error = function(e) e)
  if (inherits(val, "error"))
    return(paste0("Não consegui calcular: ", conditionMessage(val),
                  " (confira os nomes das colunas)."))
  if (!is.numeric(val) && !is.logical(val))
    return("O resultado precisa ser numérico. Ajuste a fórmula.")
  if (length(val) != nrow(df))
    return(sprintf("O resultado tem %d valores, mas os dados têm %d linhas.",
                   length(val), nrow(df)))
  NULL
}

# --- Geração do script .R (a pilha de passos vira mutate()s encadeados) -------
# Cada passo: list(nome = , expr = , comentario = ). Ambos os tipos (calcular e
# reescalar) reduzem a um mutate que cria UMA coluna.
calc_gerar_codigo <- function(passos, info) {
  if (!is.null(info) && identical(info$source, "package")) {
    leitura <- c("library(EAPADados)",
                 sprintf("data(%s)", info$package_dataset),
                 sprintf("dados <- %s", info$package_dataset))
    usa_readxl <- FALSE
  } else {
    fn  <- if (!is.null(info)) info$file_name   else "SEU_ARQUIVO.xlsx"
    abn <- if (!is.null(info)) info$excel_sheet else "SUA_ABA"
    leitura <- sprintf('dados <- read_excel("%s", sheet = "%s")', fn, abn)
    usa_readxl <- TRUE
  }

  linhas <- c(
    "# Script gerado pela CatalyseR — Calcular / Reescalar variáveis",
    "library(tidyverse)",
    if (usa_readxl) "library(readxl)" else NULL,
    "",
    "# 1. Dados de entrada (ajuste o caminho/aba se necessario)",
    leitura,
    ""
  )
  if (!length(passos)) {
    linhas <- c(linhas, "# (nenhuma variavel criada ainda)")
  }
  for (i in seq_along(passos)) {
    p <- passos[[i]]
    linhas <- c(
      linhas,
      sprintf("# Etapa %d: %s", i, p$comentario),
      "dados <- dados |>",
      sprintf("  mutate(%s = %s)", calc_bt(p$nome), p$expr),
      ""
    )
  }
  linhas <- c(linhas, "print(dados)")
  paste(linhas, collapse = "\n")
}

# --- UI ----------------------------------------------------------------------

mod_calcular_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(1, 1, 1),
      style = "grid-template-columns: 3.2fr 6.3fr 2.5fr !important;",

      # COLUNA 1: CONFIGURAÇÃO
      div(
        card(
          card_header("1. O que fazer?"),
          card_body(
            style = "padding: 12px 15px;",
            radioButtons(ns("acao"), NULL,
              choices = c("Criar variável calculada" = "calcular",
                          "Reescalar por prefixo (k, M, m, µ…)" = "reescalar"),
              selected = "calcular"),
            helpText(HTML("<b>Calcular</b>: nova coluna a partir de outras (ex.: <code>peso/comprimento</code>).<br><b>Reescalar</b>: números muito altos/baixos → prefixo de unidade (ex.: g → kg)."))
          )
        ),

        # ---- AÇÃO: CALCULAR ----
        conditionalPanel(
          condition = sprintf("input['%s'] == 'calcular'", ns("acao")),
          card(
            card_header("2. Como montar a fórmula?"),
            card_body(
              style = "padding: 12px 15px;",
              radioButtons(ns("modo_calc"), NULL,
                choices = c("Guiado (no mouse)" = "guiado",
                            "Expressão livre (digitar)" = "livre"),
                selected = "guiado"),

              # -- Modo GUIADO: funcao( A op B ) --
              conditionalPanel(
                condition = sprintf("input['%s'] == 'guiado'", ns("modo_calc")),
                selectInput(ns("g_funcao"), "Função externa (opcional):",
                            choices = calc_funcoes_choices, selected = "nenhuma"),
                selectInput(ns("g_a"), "Variável A:", choices = NULL),
                selectInput(ns("g_op"), "Operação:",
                            choices = calc_ops_choices, selected = "nenhum"),
                conditionalPanel(
                  condition = sprintf("input['%s'] != 'nenhum'", ns("g_op")),
                  radioButtons(ns("g_b_tipo"), "Segundo termo (B):",
                    choices = c("Uma coluna" = "coluna", "Um número" = "numero"),
                    selected = "coluna", inline = TRUE),
                  conditionalPanel(
                    condition = sprintf("input['%s'] == 'coluna'", ns("g_b_tipo")),
                    selectInput(ns("g_b_col"), NULL, choices = NULL)),
                  conditionalPanel(
                    condition = sprintf("input['%s'] == 'numero'", ns("g_b_tipo")),
                    numericInput(ns("g_b_num"), NULL, value = 100))
                ),
                helpText(HTML("Ex.: fator de condição ≈ <code>peso / comprimento</code>; ou <code>comprimento</code> ^ <code>3</code>."))
              ),

              # -- Modo LIVRE: digitar a expressao --
              conditionalPanel(
                condition = sprintf("input['%s'] == 'livre'", ns("modo_calc")),
                textInput(ns("l_expr"), "Fórmula (use os nomes das colunas):",
                          value = "", placeholder = "100 * peso / comprimento^3"),
                div(style = "font-size: 0.78rem; color: #555;",
                    strong("Colunas disponíveis: "),
                    textOutput(ns("cols_disponiveis"), inline = TRUE)),
                helpText(HTML("Operadores <code>+ - * / ^</code> e funções como <code>log</code>, <code>log10</code>, <code>sqrt</code>, <code>exp</code>, <code>abs</code>."))
              ),

              textInput(ns("nome_calc"), "Nome da variável nova:", value = "nova_variavel"),
              div(style = "background:#F3F6FA; border-radius:8px; padding:8px 10px; margin-top:4px; font-size:0.82rem;",
                  strong("Prévia da fórmula: "),
                  tags$code(textOutput(ns("expr_preview"), inline = TRUE)))
            )
          )
        ),

        # ---- AÇÃO: REESCALAR ----
        conditionalPanel(
          condition = sprintf("input['%s'] == 'reescalar'", ns("acao")),
          card(
            card_header("2. Reescalar por prefixo"),
            card_body(
              style = "padding: 12px 15px;",
              selectInput(ns("r_col"), "Coluna numérica:", choices = NULL),
              radioButtons(ns("r_modo"), "Prefixo:",
                choices = c("Automático (a IDE escolhe)" = "auto",
                            "Escolher manualmente" = "manual"),
                selected = "auto"),
              conditionalPanel(
                condition = sprintf("input['%s'] == 'manual'", ns("r_modo")),
                selectInput(ns("r_prefixo"), "Prefixo de destino:",
                            choices = calc_prefixo_choices(), selected = "k")),
              textInput(ns("nome_re"), "Nome da coluna nova:", value = ""),
              uiOutput(ns("reescala_info")),
              helpText("Divide os valores pela potência de dez do prefixo e cria uma coluna nova (a original é mantida).")
            )
          )
        ),

        actionButton(ns("aplicar"), "Aplicar",
                     icon = icon("play"), class = "btn-primary w-100 mt-2"),
        div(class = "d-flex gap-2 mt-2",
          actionButton(ns("desfazer"), "Desfazer última",
                       icon = icon("rotate-left"), class = "btn-outline-secondary btn-sm w-100"),
          uiOutput(ns("passos_indicador"), inline = TRUE)),
        helpText("As variáveis criadas se acumulam; 'Desfazer' remove a última.")
      ),

      # COLUNA 2: PRÉVIA + SCRIPT
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
          title = "Script gerado", icon = icon("code"),
          card_body(style = "padding: 10px 15px;",
            tags$pre(style = "white-space: pre-wrap; font-size: 0.82rem;",
                     verbatimTextOutput(ns("script_preview"))))
        )
      ),

      # COLUNA 3: STATUS / EXPORTAÇÃO
      div(
        card(
          card_header("Exportar"),
          card_body(
            style = "padding: 12px 15px;",
            uiOutput(ns("status_indicador")),
            hr(style = "margin: 10px 0;"),
            downloadButton(ns("baixar_script"), "Baixar script .R",
                           class = "btn-outline-secondary btn-sm w-100 mb-2"),
            downloadButton(ns("baixar_dados"), "Baixar dados (.xlsx)",
                           class = "btn-outline-primary btn-sm w-100 mb-2"),
            hr(style = "margin: 10px 0;"),
            actionButton(ns("usar_analises"), "Usar este resultado nas análises",
                         icon = icon("share-from-square"), class = "btn-primary w-100"),
            helpText("As análises passam a trabalhar sobre esta tabela (volte aos dados importados no painel de importação).")
          )
        ),
        card(
          card_header("Como funciona"),
          card_body(
            style = "padding: 12px 15px; font-size: 0.8rem; line-height: 1.4;",
            tags$p(style = "margin: 0 0 6px;", strong("Variável calculada"), ":"),
            tags$ol(style = "padding-left: 16px; margin: 0 0 8px;",
              tags$li("Monte a fórmula (guiado ou livre)."),
              tags$li("Dê um nome à coluna nova."),
              tags$li("Aplique — vira uma coluna no conjunto.")),
            tags$p(style = "margin: 0 0 6px;", strong("Reescalar"), ":"),
            tags$ol(style = "padding-left: 16px; margin: 0;",
              tags$li("Escolha a coluna e o prefixo (ou automático)."),
              tags$li("Ex.: 1 500 000 g → 1500 em quilos (k)."))
          )
        )
      )
    )
  )
}

# --- SERVER ------------------------------------------------------------------

mod_calcular_server <- function(id, data_rv, import_info, on_usar = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    base_data <- reactive({ req(data_rv()); data_rv() })

    resultado_rv <- reactiveVal(NULL)   # data.frame acumulado (NULL = base)
    passos_rv    <- reactiveVal(list()) # pilha de passos (nome/expr/comentario)
    snaps_rv     <- reactiveVal(list()) # snapshots p/ desfazer
    codigo_rv    <- reactiveVal("# Configure à esquerda e clique em 'Aplicar'.")

    # Entrada da PRÓXIMA operação = resultado acumulado (ou o original).
    entrada_atual <- reactive({
      r <- resultado_rv(); if (is.null(r)) base_data() else r
    })

    # Só colunas numéricas (para reescalar e para operandos guiados numéricos).
    cols_numericas <- function(df) names(df)[vapply(df, is.numeric, logical(1))]

    atualizar_seletores <- function(df) {
      todas <- names(df)
      num   <- cols_numericas(df)
      updateSelectInput(session, "g_a",     choices = num)
      updateSelectInput(session, "g_b_col", choices = num)
      updateSelectInput(session, "r_col",   choices = if (length(num)) num else todas)
    }

    # Ao (re)carregar dados: reinicia tudo.
    observeEvent(base_data(), {
      resultado_rv(NULL); passos_rv(list()); snaps_rv(list())
      atualizar_seletores(base_data())
      codigo_rv("# Configure à esquerda e clique em 'Aplicar'.")
    })

    # Reflete as colunas da entrada acumulada nos seletores.
    observeEvent(entrada_atual(), { atualizar_seletores(entrada_atual()) })

    output$cols_disponiveis <- renderText({
      req(entrada_atual()); paste(names(entrada_atual()), collapse = ", ")
    })

    # Expressão efetiva do modo CALCULAR (guiado -> string; livre -> texto).
    expr_calc <- reactive({
      if (identical(input$modo_calc, "livre")) {
        input$l_expr %||% ""
      } else {
        req(input$g_a)
        calc_montar_expr(input$g_funcao %||% "nenhuma", input$g_a,
                         input$g_op %||% "nenhum", input$g_b_tipo %||% "coluna",
                         input$g_b_col %||% "", input$g_b_num %||% 0)
      }
    })

    output$expr_preview <- renderText({
      e <- tryCatch(expr_calc(), error = function(err) "")
      if (nzchar(e)) e else "—"
    })

    # Nome padrão da coluna reescalada: <col>_<simbolo> (ou <col>_base).
    prefixo_efetivo <- reactive({
      req(identical(input$acao, "reescalar"), input$r_col)
      col <- input$r_col
      if (!col %in% names(entrada_atual())) return(NULL)
      if (identical(input$r_modo, "auto"))
        calc_prefixo_auto(entrada_atual()[[col]])
      else
        input$r_prefixo %||% ""
    })

    observeEvent(list(input$r_col, input$r_modo, input$r_prefixo, input$acao), {
      req(identical(input$acao, "reescalar"), input$r_col)
      sim <- prefixo_efetivo(); if (is.null(sim)) return()
      sufixo <- if (nzchar(sim)) sim else "base"
      updateTextInput(session, "nome_re", value = paste0(input$r_col, "_", sufixo))
    }, ignoreInit = TRUE)

    # Painel informativo da reescala: fator + exemplo com o 1º valor válido.
    output$reescala_info <- renderUI({
      req(identical(input$acao, "reescalar"), input$r_col)
      col <- input$r_col
      if (!col %in% names(entrada_atual())) return(NULL)
      sim <- prefixo_efetivo(); if (is.null(sim)) return(NULL)
      fator <- calc_fator(sim)
      x <- suppressWarnings(as.numeric(entrada_atual()[[col]]))
      x1 <- x[is.finite(x) & x != 0][1]
      exemplo <- if (!is.na(x1))
        sprintf("%s → %s", calc_num_txt(x1), calc_num_txt(round(x1 / fator, 4)))
      else "—"
      rot <- if (nzchar(sim)) sprintf("%s (%s)", calc_prefixos$nome[match(sim, calc_prefixos$simbolo)], sim) else "base"
      div(style = "background:#E7EFEA; border-radius:8px; padding:8px 10px; margin:4px 0; font-size:0.82rem;",
          sprintf("Prefixo: %s · dividir por 10^%d", rot, as.integer(round(log10(fator)))), br(),
          strong("Exemplo: "), exemplo)
    })

    # --- APLICAR ---------------------------------------------------------------
    observeEvent(input$aplicar, {
      entrada <- entrada_atual()

      if (identical(input$acao, "calcular")) {
        nome <- trimws(input$nome_calc %||% "")
        expr <- tryCatch(expr_calc(), error = function(e) "")
        msg  <- calc_validar_expr(nome, expr, entrada)
        if (!is.null(msg)) { showNotification(msg, type = "error", duration = 9); return() }

        novo <- tryCatch({
            d <- entrada; d[[nome]] <- calc_avaliar(expr, entrada); d
          },
          error = function(e) { showNotification(paste("Erro:", conditionMessage(e)),
                                                 type = "error", duration = 12); NULL })
        if (is.null(novo)) return()
        passo <- list(nome = nome, expr = expr,
                      comentario = sprintf("calcular '%s' = %s", nome, expr))

      } else {  # reescalar
        col <- input$r_col %||% ""
        if (!nzchar(col) || !col %in% names(entrada)) {
          showNotification("Escolha uma coluna válida.", type = "error"); return()
        }
        if (!is.numeric(entrada[[col]]) &&
            any(is.na(suppressWarnings(as.numeric(entrada[[col]]))) & !is.na(entrada[[col]]))) {
          showNotification("A coluna não é numérica.", type = "error"); return()
        }
        nome <- trimws(input$nome_re %||% "")
        if (!nzchar(nome)) { showNotification("Dê um nome à coluna nova.", type = "error"); return() }
        sim <- prefixo_efetivo(); if (is.null(sim)) sim <- ""
        fator <- calc_fator(sim)
        expr <- sprintf("%s / %s", calc_bt(col), calc_num_txt(fator))
        rot <- if (nzchar(sim)) sprintf("%s (%s)", calc_prefixos$nome[match(sim, calc_prefixos$simbolo)], sim) else "base"

        novo <- tryCatch({
            d <- entrada; d[[nome]] <- calc_reescalar(entrada[[col]], sim); d
          },
          error = function(e) { showNotification(paste("Erro:", conditionMessage(e)),
                                                 type = "error", duration = 12); NULL })
        if (is.null(novo)) return()
        passo <- list(nome = nome, expr = expr,
                      comentario = sprintf("reescalar '%s' para %s", col, rot))
      }

      # Empilha snapshot para desfazer, guarda resultado e regenera o script.
      snaps_rv(c(snaps_rv(), list(resultado_rv())))
      resultado_rv(novo)
      passos_rv(c(passos_rv(), list(passo)))
      codigo_rv(calc_gerar_codigo(passos_rv(), import_info()))
      showNotification(sprintf("Coluna '%s' criada.", passo$nome), type = "message", duration = 4)
    })

    # --- DESFAZER --------------------------------------------------------------
    observeEvent(input$desfazer, {
      ps <- passos_rv()
      if (!length(ps)) { showNotification("Nada para desfazer.", type = "warning"); return() }
      snaps <- snaps_rv()
      anterior <- snaps[[length(snaps)]]        # pode ser NULL (volta ao base)
      resultado_rv(anterior)
      passos_rv(ps[-length(ps)])
      snaps_rv(snaps[-length(snaps)])
      codigo_rv(calc_gerar_codigo(passos_rv(), import_info()))
      showNotification("Última variável removida.", type = "message", duration = 3)
    })

    # --- Prévias / status ------------------------------------------------------
    output$preview_antes <- renderDT({
      req(base_data())
      datatable(head(base_data(), 100), options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
    })
    output$preview_depois <- renderDT({
      validate(need(!is.null(resultado_rv()), "Aplique uma operação para ver o resultado aqui."))
      datatable(head(resultado_rv(), 200), options = list(scrollX = TRUE, pageLength = 12), rownames = FALSE)
    })
    output$script_preview <- renderText(codigo_rv())

    output$status_indicador <- renderUI({
      if (is.null(resultado_rv())) {
        div(style = "color:#888; font-size:0.85rem;", "Nenhuma variável criada ainda.")
      } else {
        r <- resultado_rv(); np <- length(passos_rv())
        div(style = "font-size:0.85rem;",
            span(style = "color:#2E7D8F; font-weight:600;", "✓ Variáveis criadas"), br(),
            sprintf("%d etapa(s) · %d linhas × %d colunas", np, nrow(r), ncol(r)))
      }
    })

    output$passos_indicador <- renderUI({
      span(style = "font-size:0.78rem; color:#2E7D8F; white-space:nowrap; align-self:center;",
           sprintf("%d etapa(s)", length(passos_rv())))
    })

    # --- Downloads / promoção --------------------------------------------------
    output$baixar_script <- downloadHandler(
      filename = function() paste0("calcular_variaveis_", Sys.Date(), ".R"),
      content  = function(file) writeLines(codigo_rv(), file)
    )
    output$baixar_dados <- downloadHandler(
      filename = function() paste0("dados_calculados_", Sys.Date(), ".xlsx"),
      content  = function(file) { req(resultado_rv()); writexl::write_xlsx(resultado_rv(), file) }
    )

    observeEvent(input$usar_analises, {
      if (is.null(resultado_rv())) {
        showNotification("Crie ao menos uma variável antes de usar nas análises.", type = "warning")
        return()
      }
      if (is.function(on_usar)) on_usar(resultado_rv(), "resultado do Calcular/Reescalar")
    })
  })
}

# Operador auxiliar (caso não exista no escopo do app)
if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a
