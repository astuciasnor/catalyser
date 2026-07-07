# Módulo de Arrumação (Largo -> Longo) para a CatalyseR
# ---------------------------------------------------------------------------
# Genérico: empilha colunas de MEDIDA (pivot_longer), extrai metadados do nome
# da coluna via regex com grupos de captura e, opcionalmente, alarga uma métrica
# (pivot_wider). Nada hardcoded — o usuário define colunas, regex e saída. Ao
# final, gera um script .R reproduzível ("do mouse ao código").
library(shiny)
library(bslib)
library(DT)

# --- Funções puras (fora do server): fáceis de testar e espelhar no livro -----

# Presets de extração: cada um mapeia para uma regex e nomes de colunas sugeridos.
arrumar_presets <- list(
  ano_metrica = list(regex = "^(\\d{4}) - (.*)$", novas = "ano, metrica"),
  metrica_ano = list(regex = "^(.*) - (\\d{4})$", novas = "metrica, ano"),
  hifen       = list(regex = "^(.*) - (.*)$",     novas = "parte1, parte2")
)

# Modal de ajuda: guia rápido de regex para o seletor "Padrão de extração".
arrumar_ajuda_regex_modal <- function() {
  linha <- function(sim, desc) tags$tr(
    tags$td(tags$code(sim), style = "white-space:nowrap; padding-right:14px; vertical-align:top;"),
    tags$td(desc))
  modalDialog(
    title = tagList(icon("circle-question"), " Guia rápido de regex"),
    size = "l", easyClose = TRUE, footer = modalButton("Fechar"),
    tags$div(
      style = "font-size:0.9rem; line-height:1.55;",
      tags$p(
        "Uma ", tags$b("regex"), " é um padrão que descreve um texto. Você a lê da esquerda ",
        "para a direita, casando com o nome da coluna. Cada trecho entre parênteses ",
        tags$code("( )"), " é um ", tags$b("grupo de captura"), " — e cada grupo vira uma coluna nova."),
      tags$p(style = "margin:10px 0; background:#F3F6FA; border-radius:8px; padding:10px 12px;",
        "Exemplo: ", tags$code("^(\\d{4}) - (.*)$"), " sobre ", tags$code("2025 - Valor US$ FOB"),
        " → o grupo 1 pega ", tags$b("2025"), " (coluna ", tags$code("ano"),
        ") e o grupo 2 pega ", tags$b("Valor US$ FOB"), " (coluna ", tags$code("metrica"), ")."),

      tags$h6("Peças mais usadas", style = "color:#0F3B5F; font-weight:700; margin-top:14px;"),
      tags$table(class = "table table-sm table-borderless", style = "margin-bottom:6px;",
        tags$tbody(
          linha("^",   "começo do texto"),
          linha("$",   "fim do texto"),
          linha("\\d", "um dígito (0–9)"),
          linha("{4}", HTML("exatamente 4 vezes do anterior — ex.: <code>\\d{4}</code> = 2025")),
          linha(".",   "qualquer caractere"),
          linha("*",   HTML("zero ou mais do anterior — ex.: <code>.*</code> pega o resto")),
          linha("\\w", "letra, dígito ou _ (underscore)"),
          linha("( )", "grupo de captura → vira uma coluna nova"),
          linha("espaço  -  letras", "literais: casam com eles mesmos"))),

      tags$h6("Os padrões prontos", style = "color:#0F3B5F; font-weight:700; margin-top:10px;"),
      tags$ul(style = "padding-left:18px; margin-bottom:8px;",
        tags$li(tags$b("Ano + métrica"), " ", tags$code("^(\\d{4}) - (.*)$"),
                " — nomes ", tags$code("ano, metrica"), " (ex.: ", tags$code("2025 - Valor US$ FOB"), ")."),
        tags$li(tags$b("Métrica + ano"), " ", tags$code("^(.*) - (\\d{4})$"),
                " — nomes ", tags$code("metrica, ano"), " (colunas invertidas)."),
        tags$li(tags$b("Separar por ' - '"), " — versão sem regex: corta o nome no hífen.")),

      tags$h6("Escrever o seu (Personalizado)", style = "color:#0F3B5F; font-weight:700; margin-top:10px;"),
      tags$ul(style = "padding-left:18px; margin-bottom:8px;",
        tags$li(tags$code("jan_2020"), ", ", tags$code("fev_2020"), " → ",
                tags$code("^(\\w+)_(\\d{4})$"), ", nomes ", tags$code("mes, ano"), "."),
        tags$li(tags$code("peso_2019_kg"), " → ", tags$code("^(.*)_(\\d{4})_(.*)$"),
                ", nomes ", tags$code("variavel, ano, unidade"), ".")),

      tags$div(style = "background:#E7EFEA; border-radius:8px; padding:10px 12px; margin-top:8px;",
        tags$b("Regra de ouro: "), "o número de grupos ", tags$code("( )"),
        " tem de ser igual ao número de nomes que você digita. Se a coluna extraída sair ",
        "toda vazia (NA), o padrão não casou — ajuste e aplique de novo.")
    )
  )
}

# Conta grupos de captura reais: "(" não escapado e não seguido de "?".
arrumar_n_grupos <- function(regex) {
  m <- gregexpr("(?<!\\\\)\\((?!\\?)", regex, perl = TRUE)[[1]]
  if (length(m) == 1 && m[1] == -1) 0L else length(m)
}

# Validação da configuração antes de aplicar. Devolve NULL se OK, ou a mensagem.
arrumar_validar <- function(cfg, df) {
  # Comum aos dois modos: nomes novos + regex válida + nº de grupos == nº de nomes
  if (any(!nzchar(cfg$novas)))
    return("Dê um nome a cada coluna nova (separe por vírgula).")
  ok <- tryCatch({ grepl(cfg$regex, "teste", perl = TRUE); TRUE }, error = function(e) FALSE)
  if (!ok)
    return("Regex inválida — confira os parênteses e as barras invertidas.")
  ng <- arrumar_n_grupos(cfg$regex)
  if (ng != length(cfg$novas))
    return(sprintf("A regex tem %d grupo(s) de captura, mas você nomeou %d coluna(s) nova(s).",
                   ng, length(cfg$novas)))

  if (identical(cfg$modo, "separar")) {
    if (is.null(cfg$col_separar) || !nzchar(cfg$col_separar))
      return("Escolha a coluna a separar.")
    if (!(cfg$col_separar %in% names(df)))
      return("A coluna a separar não foi encontrada no conjunto atual.")
  } else {
    if (length(cfg$cols_medida) == 0)
      return("Selecione ao menos uma coluna de medida para empilhar.")
    falta <- setdiff(cfg$cols_medida, names(df))
    if (length(falta))
      return(paste("Colunas não encontradas:", paste(falta, collapse = ", ")))
    if (!nzchar(cfg$values_to))
      return("Informe o nome da coluna de valores (ex.: 'valor').")
    if (identical(cfg$saida, "largo") && !nzchar(cfg$wider_names))
      return("Escolha qual coluna vira as novas colunas no formato largo.")
  }
  NULL
}

# Aplica a transformação. Warnings viram avisos amigáveis, mas não abortam.
arrumar_aplicar <- function(cfg, df, on_warn = NULL, on_err = NULL) {
  withCallingHandlers(
    tryCatch({
      if (identical(cfg$modo, "separar")) {
        # Extrai grupos da regex dos VALORES de uma coluna existente (sem pivô).
        tidyr::extract(df, col = cfg$col_separar, into = cfg$novas,
                       regex = cfg$regex, remove = !isTRUE(cfg$manter_original))
      } else {
        long <- tidyr::pivot_longer(
          df,
          cols          = tidyselect::all_of(cfg$cols_medida),
          names_to      = cfg$novas,
          names_pattern = cfg$regex,
          values_to     = cfg$values_to,
          values_transform = if (cfg$como_numero)
            stats::setNames(list(as.numeric), cfg$values_to) else NULL
        )
        if (identical(cfg$saida, "largo")) {
          long <- tidyr::pivot_wider(long, names_from = tidyselect::all_of(cfg$wider_names),
                                     values_from = tidyselect::all_of(cfg$values_to))
        }
        long
      }
    }, error = function(e) { if (is.function(on_err)) on_err(conditionMessage(e)); NULL }),
    warning = function(w) {
      if (is.function(on_warn)) on_warn(conditionMessage(w)); invokeRestart("muffleWarning")
    }
  )
}

# Coloca crase em nomes não sintáticos (ex.: "Valor US$ FOB" -> `Valor US$ FOB`).
arrumar_bt <- function(x) {
  ok <- grepl("^[A-Za-z.][A-Za-z0-9._]*$", x) & !grepl("^\\.[0-9]", x)
  ifelse(ok, x, paste0("`", x, "`"))
}

# Gera o texto do script .R. CRÍTICO: escapar "\" da regex ao virar código.
# `renomear`  : vetor nomeado names = nome antigo, valor = nome novo.
# `selecionar`: vetor de colunas a manter (no espaço já renomeado); NULL = todas.
arrumar_gerar_codigo <- function(cfg, info, renomear = character(0), selecionar = NULL) {
  esc <- function(s) gsub("\\", "\\\\", s, fixed = TRUE)
  q   <- function(v) paste(sprintf('"%s"', v), collapse = ", ")

  if (!is.null(info) && identical(info$source, "package")) {
    leitura <- c("library(EAPADados)",
                 sprintf("data(%s)", info$package_dataset),
                 sprintf("dados_largo <- %s", info$package_dataset))
    usa_readxl <- FALSE
  } else {
    fn  <- if (!is.null(info)) info$file_name   else "SEU_ARQUIVO.xlsx"
    abn <- if (!is.null(info)) info$excel_sheet else "SUA_ABA"
    leitura <- sprintf('dados_largo <- read_excel("%s", sheet = "%s")', fn, abn)
    usa_readxl <- TRUE
  }

  titulo <- if (identical(cfg$modo, "separar"))
    "# Script gerado pela CatalyseR — Arrumacao (separar uma coluna)"
  else
    "# Script gerado pela CatalyseR — Arrumacao (largo -> longo)"

  linhas <- c(
    titulo,
    "library(tidyverse)",
    if (usa_readxl) "library(readxl)" else NULL,
    "",
    "# 1. Dados de entrada (ajuste o caminho/aba se necessario)",
    leitura,
    ""
  )
  if (identical(cfg$modo, "separar")) {
    linhas <- c(linhas,
      "# 2. Separar uma coluna existente em varias (extract via regex)",
      "dados_arrumados <- dados_largo |>",
      "  extract(",
      sprintf("    col = %s,", arrumar_bt(cfg$col_separar)),
      sprintf("    into = c(%s),", q(cfg$novas)),
      sprintf('    regex = "%s",', esc(cfg$regex)),
      sprintf("    remove = %s", if (isTRUE(cfg$manter_original)) "FALSE" else "TRUE"),
      "  )")
  } else {
    linhas <- c(linhas,
      "# 2. Colunas de medida que serao empilhadas",
      "cols_medida <- c(",
      paste0("  ", q(cfg$cols_medida)),
      ")",
      "",
      "# 3. Empilhar e extrair metadados do nome da coluna",
      "dados_arrumados <- dados_largo |>",
      "  pivot_longer(",
      "    cols = all_of(cols_medida),",
      sprintf("    names_to = c(%s),", q(cfg$novas)),
      sprintf('    names_pattern = "%s",', esc(cfg$regex)),
      sprintf('    values_to = "%s"%s', cfg$values_to, if (cfg$como_numero) "," else ""),
      if (cfg$como_numero) sprintf("    values_transform = list(%s = as.numeric)", cfg$values_to) else NULL,
      "  )")
    if (identical(cfg$saida, "largo")) {
      linhas <- c(linhas, "",
        "# 4. Alargar uma metrica em colunas (pivot_wider)",
        "dados_arrumados <- dados_arrumados |>",
        sprintf("  pivot_wider(names_from = %s, values_from = %s)", cfg$wider_names, cfg$values_to))
    }
  }
  if (length(renomear)) {
    pares <- vapply(seq_along(renomear), function(i)
      sprintf("%s = %s", arrumar_bt(unname(renomear[i])), arrumar_bt(names(renomear)[i])),
      character(1))
    linhas <- c(linhas, "",
      "# 5. Renomear colunas",
      "dados_arrumados <- dados_arrumados |>",
      paste0("  rename(", paste(pares, collapse = ", "), ")"))
  }
  if (!is.null(selecionar) && length(selecionar)) {
    linhas <- c(linhas, "",
      "# 6. Selecionar apenas as colunas desejadas",
      "dados_arrumados <- dados_arrumados |>",
      paste0("  select(", paste(arrumar_bt(selecionar), collapse = ", "), ")"))
  }
  linhas <- c(linhas, "", "print(dados_arrumados)")
  paste(linhas, collapse = "\n")
}

# --- UI ----------------------------------------------------------------------

mod_arrumar_ui <- function(id, modo_fixo = NULL) {
  ns <- NS(id)
  combinado   <- is.null(modo_fixo)
  mostrar_emp <- combinado || identical(modo_fixo, "empilhar")
  mostrar_sep <- combinado || identical(modo_fixo, "separar")

  # Embrulha em conditionalPanel(modo == X) só quando os dois modos convivem.
  so_modo <- function(modo, ui) if (combinado)
    conditionalPanel(sprintf("input['%s'] == '%s'", ns("modo"), modo), ui) else ui

  # Numeração dos cartões conforme o contexto.
  if (combinado) { n_grp <- 2L; n_regex <- 3L; n_saida <- 4L }
  else if (identical(modo_fixo, "empilhar")) { n_grp <- 1L; n_regex <- 2L; n_saida <- 3L }
  else { n_grp <- 1L; n_regex <- 2L; n_saida <- NA_integer_ }

  tagList(
    layout_columns(
      col_widths = c(1, 1, 1),
      style = "grid-template-columns: 3fr 6.5fr 2.5fr !important;",

      # COLUNA 1: CONFIGURAÇÃO
      div(
        if (combinado) card(
          card_header("1. O que fazer?"),
          card_body(
            style = "padding: 12px 15px;",
            radioButtons(ns("modo"), NULL,
              choices = c("Empilhar colunas largas (largo → longo)" = "empilhar",
                          "Separar uma coluna (quebrar valores)"     = "separar"),
              selected = "empilhar"),
            helpText(HTML("<b>Empilhar</b>: metadados no NOME das colunas (ex.: <code>2025 - Valor…</code>).<br><b>Separar</b>: metadados DENTRO de uma coluna (ex.: <code>Pargo-2025</code>)."))
          )
        ),

        # --- Modo EMPILHAR: colunas de medida ---
        if (mostrar_emp) so_modo("empilhar", card(
          card_header(sprintf("%d. Colunas de medida", n_grp)),
          card_body(
            style = "padding: 12px 15px;",
            actionButton(ns("auto_detectar"), "Detectar automaticamente",
                         icon = icon("wand-magic-sparkles"), class = "btn-outline-primary btn-sm w-100 mb-2"),
            helpText("Sugere como 'medida' colunas que começam com 4 dígitos (ex.: '2025 - ...')."),
            selectizeInput(ns("cols_medida"), "Colunas a empilhar:",
                           choices = NULL, multiple = TRUE,
                           options = list(placeholder = "clique para escolher…",
                                          plugins = list("remove_button"))),
            div(style = "font-size: 0.78rem; color: #555;",
                strong("Identificadores (automático): "),
                textOutput(ns("cols_id_preview"), inline = TRUE))
          )
        )),

        # --- Modo SEPARAR: coluna a quebrar ---
        if (mostrar_sep) so_modo("separar", card(
          card_header(sprintf("%d. Coluna a separar", n_grp)),
          card_body(
            style = "padding: 12px 15px;",
            selectInput(ns("col_separar"), "Coluna a quebrar:", choices = NULL),
            checkboxInput(ns("manter_original"), "Manter a coluna original", value = FALSE),
            helpText("A regex abaixo é aplicada aos VALORES desta coluna; cada grupo ( ) vira uma coluna nova.")
          )
        )),

        # --- Extração (regex): comum aos dois modos ---
        card(
          card_header(sprintf("%d. Extrair com regex", n_regex)),
          card_body(
            style = "padding: 12px 15px;",
            selectInput(ns("regex_preset"),
              label = tagList(
                "Padrão de extração ",
                actionLink(ns("ajuda_regex"), label = NULL, icon = icon("circle-question"),
                           style = "color:#2E7D8F; margin-left:2px;", title = "O que é regex? Ver guia rápido")),
              choices = c("Ano + métrica  ( ^(\\d{4}) - (.*)$ )" = "ano_metrica",
                          "Métrica + ano  ( ^(.*) - (\\d{4})$ )" = "metrica_ano",
                          "Separar por ' - '"                    = "hifen",
                          "Personalizado (escrever regex)"       = "custom")),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'custom'", ns("regex_preset")),
              textInput(ns("regex"), "Regex (cada parêntese vira uma coluna):",
                        value = "^(\\d{4}) - (.*)$")
            ),
            textInput(ns("novas_cols"), "Nomes das colunas novas (ordem dos grupos):",
                      value = "ano, metrica"),
            if (mostrar_emp) so_modo("empilhar", tagList(
              textInput(ns("values_to"), "Nome da coluna de valores:", value = "valor"),
              checkboxInput(ns("como_numero"), "Converter valores para número", value = TRUE)
            ))
          )
        ),

        # --- Saída: só no modo empilhar ---
        if (mostrar_emp) so_modo("empilhar", card(
          card_header(sprintf("%d. Saída", n_saida)),
          card_body(
            style = "padding: 12px 15px;",
            radioButtons(ns("saida"), NULL,
              choices = c("Manter em formato LONGO" = "longo",
                          "ALARGAR uma métrica (pivot_wider)" = "largo")),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'largo'", ns("saida")),
              selectInput(ns("wider_names"), "Coluna que vira novas colunas:", choices = NULL)
            )
          )
        )),

        actionButton(ns("aplicar"), "Aplicar transformação",
                     icon = icon("play"), class = "btn-primary w-100 mt-2")
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

      # COLUNA 3: STATUS / DOWNLOAD
      div(
        card(
          card_header("Exportar"),
          card_body(
            style = "padding: 12px 15px;",
            uiOutput(ns("status_indicador")),
            hr(style = "margin: 10px 0;"),
            actionButton(ns("abrir_selecionar"), "Selecionar variáveis",
                         icon = icon("list-check"), class = "btn-outline-secondary btn-sm w-100 mb-2"),
            actionButton(ns("abrir_renomear"), "Renomear colunas",
                         icon = icon("i-cursor"), class = "btn-outline-secondary btn-sm w-100 mb-2"),
            downloadButton(ns("baixar_script"), "Baixar script .R",
                           class = "btn-outline-secondary btn-sm w-100 mb-2"),
            downloadButton(ns("baixar_dados"), "Baixar dados arrumados (.xlsx)",
                           class = "btn-outline-primary btn-sm w-100")
          )
        ),
        card(
          card_header("Como funciona"),
          card_body(
            style = "padding: 12px 15px; font-size: 0.8rem; line-height: 1.4;",
            if (mostrar_emp) tagList(
              tags$p(style = "margin: 0 0 6px;", strong("Empilhar"), " (metadados no nome da coluna):"),
              tags$ol(style = "padding-left: 16px; margin: 0 0 8px;",
                tags$li("Escolha (ou detecte) as colunas de medida."),
                tags$li("Defina a regex que extrai ano/métrica do nome."),
                tags$li("Mantenha longo ou alargue uma métrica."))),
            if (mostrar_sep) tagList(
              tags$p(style = "margin: 0 0 6px;", strong("Separar"), " (metadados dentro de uma coluna):"),
              tags$ol(style = "padding-left: 16px; margin: 0 0 8px;",
                tags$li("Escolha a coluna a quebrar."),
                tags$li("Defina a regex; cada grupo ( ) vira uma coluna."))),
            tags$p(style = "margin: 0;", "Depois: renomeie, selecione e baixe o script .R + os dados.")
          )
        )
      )
    )
  )
}

# --- SERVER ------------------------------------------------------------------

mod_arrumar_server <- function(id, data_rv, import_info, modo_fixo = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    base_data <- reactive({ req(data_rv()); data_rv() })

    # Modo efetivo: fixo (quando o menu já define) ou o do seletor (modo combinado).
    modo_atual <- reactive({ if (!is.null(modo_fixo)) modo_fixo else input$modo })

    resultado_rv <- reactiveVal(NULL)                 # resultado do pivot (sem renomear)
    codigo_rv    <- reactiveVal("# Configure à esquerda e clique em 'Aplicar transformação'.")
    cfg_rv       <- reactiveVal(NULL)                  # última config aplicada
    renomear_rv  <- reactiveVal(character(0))          # mapa nome_antigo -> nome_novo
    rn_cols      <- reactiveVal(character(0))          # ordem das colunas no modal
    sel_cols_rv  <- reactiveVal(NULL)                  # colunas a manter (NULL = todas)

    # Popular seletores de coluna quando os dados chegam
    observeEvent(base_data(), {
      updateSelectizeInput(session, "cols_medida", choices = names(base_data()),
                           selected = character(0), server = TRUE)
      updateSelectInput(session, "col_separar", choices = names(base_data()))
      resultado_rv(NULL)
      cfg_rv(NULL)
      renomear_rv(character(0))
      sel_cols_rv(NULL)
      codigo_rv("# Configure à esquerda e clique em 'Aplicar transformação'.")
    })

    # Detecção automática (heurística: começa com 4 dígitos)
    observeEvent(input$auto_detectar, {
      med <- grep("^\\d{4}", names(base_data()), value = TRUE)
      if (length(med) == 0) {
        showNotification("Nenhuma coluna começando com 4 dígitos foi encontrada.", type = "warning")
      } else {
        updateSelectizeInput(session, "cols_medida", selected = med)
      }
    })

    # Identificadores = complemento (como o setdiff do script)
    output$cols_id_preview <- renderText({
      req(base_data())
      ids <- setdiff(names(base_data()), input$cols_medida)
      if (length(ids) == 0) "—" else paste(ids, collapse = ", ")
    })

    # Ajuda "?" ao lado do seletor: abre o guia rápido de regex
    observeEvent(input$ajuda_regex, { showModal(arrumar_ajuda_regex_modal()) })

    # Preset -> sugerir regex e nomes das colunas novas
    observeEvent(input$regex_preset, {
      if (input$regex_preset != "custom") {
        p <- arrumar_presets[[input$regex_preset]]
        updateTextInput(session, "novas_cols", value = p$novas)
        updateTextInput(session, "regex", value = p$regex)
      }
    })

    # Regex efetiva (preset ou personalizada)
    regex_efetiva <- reactive({
      if (input$regex_preset == "custom") input$regex
      else arrumar_presets[[input$regex_preset]]$regex
    })

    # Atualizar as opções de "coluna que vira colunas" conforme os nomes novos
    observe({
      novas <- trimws(strsplit(input$novas_cols %||% "", ",")[[1]])
      novas <- novas[nzchar(novas)]
      updateSelectInput(session, "wider_names", choices = novas,
                        selected = if (length(novas)) novas[length(novas)] else NULL)
    })

    # Aplicar (validar -> transformar -> guardar) — calcula uma vez por clique
    observeEvent(input$aplicar, {
      cfg <- list(
        modo        = modo_atual(),
        regex       = regex_efetiva(),
        novas       = trimws(strsplit(input$novas_cols %||% "", ",")[[1]]),
        # campos do modo EMPILHAR:
        cols_medida = input$cols_medida,
        values_to   = trimws(input$values_to %||% ""),
        como_numero = isTRUE(input$como_numero),
        saida       = input$saida,
        wider_names = input$wider_names %||% "",
        # campos do modo SEPARAR:
        col_separar     = input$col_separar %||% "",
        manter_original = isTRUE(input$manter_original)
      )
      cfg$novas <- cfg$novas[nzchar(cfg$novas)]

      msg <- arrumar_validar(cfg, base_data())
      if (!is.null(msg)) { showNotification(msg, type = "error", duration = 9); return() }

      res <- arrumar_aplicar(
        cfg, base_data(),
        on_warn = function(m) showNotification(paste("Aviso:", m), type = "warning", duration = 9),
        on_err  = function(m) showNotification(paste("Erro:", m), type = "error", duration = 12)
      )
      if (is.null(res)) return()

      # Aviso informativo de coerção numérica (só no modo empilhar)
      if (identical(cfg$modo, "empilhar") && cfg$como_numero && cfg$values_to %in% names(res)) {
        n_na <- sum(is.na(res[[cfg$values_to]]))
        if (n_na > 0)
          showNotification(sprintf("%d valor(es) não puderam ser convertidos para número (viraram NA).", n_na),
                           type = "warning", duration = 9)
      }
      # Aviso se o padrão não casou (coluna extraída toda NA)
      if (length(cfg$novas) && cfg$novas[1] %in% names(res) && all(is.na(res[[cfg$novas[1]]])))
        showNotification(if (identical(cfg$modo, "separar"))
            "O padrão não casou com os valores da coluna — confira a regex."
          else
            "O padrão de extração não casou com os nomes das colunas — confira a regex.",
          type = "warning", duration = 10)

      resultado_rv(res)
      cfg_rv(cfg)
      renomear_rv(character(0))                        # renomeações reiniciam a cada aplicação
      sel_cols_rv(NULL)                                # seleção também reinicia
      codigo_rv(arrumar_gerar_codigo(cfg, import_info()))
      showNotification("Transformação aplicada.", type = "message", duration = 3)
    })

    # Nomes das colunas do resultado no espaço JÁ renomeado (base do modal de seleção)
    nomes_renomeados <- reactive({
      r <- resultado_rv(); req(r)
      nm <- names(r); mp <- renomear_rv()
      if (length(mp)) for (o in names(mp)) nm[nm == o] <- unname(mp[o])
      nm
    })

    # Resultado final = pivot + renomeações + seleção de colunas
    resultado_final <- reactive({
      r <- resultado_rv(); req(r)
      mp <- renomear_rv()
      if (length(mp)) {
        nm <- names(r)
        for (o in names(mp)) nm[nm == o] <- unname(mp[o])
        names(r) <- nm
      }
      sel <- sel_cols_rv()
      if (!is.null(sel)) {
        keep <- intersect(sel, names(r))
        if (length(keep)) r <- r[, keep, drop = FALSE]
      }
      r
    })

    # --- Renomear colunas (modal: mantém a tela limpa) ---
    observeEvent(input$abrir_renomear, {
      r <- resultado_rv()
      if (is.null(r)) {
        showNotification("Aplique uma transformação antes de renomear.", type = "warning"); return()
      }
      cols <- names(r); rn_cols(cols)
      atual <- cols
      mp <- renomear_rv()
      if (length(mp)) for (o in names(mp)) atual[cols == o] <- unname(mp[o])
      showModal(modalDialog(
        title = "Renomear colunas", size = "l", easyClose = TRUE,
        helpText("Ajuste os nomes à direita. Dica: nomes 'tidy' evitam espaços e símbolos (ex.: valor_usd, massa_kg)."),
        div(style = "max-height: 430px; overflow-y: auto; padding-right: 6px;",
          lapply(seq_along(cols), function(i)
            div(style = "display:flex; gap:10px; align-items:center; margin-bottom:6px;",
              div(style = "flex:1; font-size:0.82rem; color:#666; word-break:break-word;", cols[i]),
              div(style = "flex:0 0 20px; text-align:center; color:#aaa;", "→"),
              div(style = "flex:1;", textInput(ns(paste0("rn_", i)), NULL, value = atual[i], width = "100%"))))),
        footer = tagList(modalButton("Cancelar"),
                         actionButton(ns("confirmar_renomear"), "Aplicar", class = "btn-primary"))
      ))
    })

    observeEvent(input$confirmar_renomear, {
      cols <- rn_cols(); req(length(cols) > 0)
      novos <- vapply(seq_along(cols), function(i) {
        v <- input[[paste0("rn_", i)]]; if (is.null(v)) cols[i] else trimws(v)
      }, character(1))
      if (any(!nzchar(novos))) { showNotification("Os nomes não podem ficar vazios.", type = "error"); return() }
      if (anyDuplicated(novos)) { showNotification("Há nomes de coluna duplicados.", type = "error"); return() }
      mudou <- novos != cols
      mp <- stats::setNames(novos[mudou], cols[mudou])   # names = antigo, valor = novo
      renomear_rv(mp)
      sel_cols_rv(NULL)                                  # renomear reinicia a seleção (nomes mudaram)
      codigo_rv(arrumar_gerar_codigo(cfg_rv(), import_info(), mp, NULL))
      removeModal()
      showNotification(if (length(mp)) sprintf("%d coluna(s) renomeada(s).", length(mp))
                       else "Nenhuma alteração de nome.", type = "message", duration = 3)
    })

    # --- Selecionar variáveis (modal): escolher quais colunas manter no resultado ---
    observeEvent(input$abrir_selecionar, {
      if (is.null(resultado_rv())) {
        showNotification("Aplique uma transformação antes de selecionar.", type = "warning"); return()
      }
      nm <- nomes_renomeados()
      atual <- sel_cols_rv(); if (is.null(atual)) atual <- nm
      showModal(modalDialog(
        title = "Selecionar variáveis", size = "l", easyClose = TRUE,
        helpText("Marque as colunas que deseja manter no resultado arrumado."),
        checkboxGroupInput(ns("sel_check"), NULL, choices = nm, selected = atual),
        footer = tagList(
          actionButton(ns("sel_todas"), "Marcar todas", class = "btn btn-sm btn-outline-secondary"),
          modalButton("Cancelar"),
          actionButton(ns("confirmar_selecionar"), "Aplicar", class = "btn-primary"))
      ))
    })

    observeEvent(input$sel_todas, {
      updateCheckboxGroupInput(session, "sel_check", selected = nomes_renomeados())
    })

    observeEvent(input$confirmar_selecionar, {
      sel <- input$sel_check
      if (is.null(sel) || length(sel) == 0) {
        showNotification("Selecione ao menos uma coluna.", type = "error"); return()
      }
      nm <- nomes_renomeados()
      sel <- nm[nm %in% sel]                # mantém a ordem original das colunas
      # Se todas marcadas, guarda NULL (sem select() no script); senão, guarda a seleção.
      sel_cols_rv(if (setequal(sel, nm)) NULL else sel)
      codigo_rv(arrumar_gerar_codigo(cfg_rv(), import_info(), renomear_rv(), sel_cols_rv()))
      removeModal()
      showNotification("Seleção de variáveis aplicada.", type = "message", duration = 3)
    })

    # Prévias
    output$preview_antes <- renderDT({
      req(base_data())
      datatable(head(base_data(), 100), options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
    })
    output$preview_depois <- renderDT({
      validate(need(!is.null(resultado_rv()), "Aplique uma transformação para ver o resultado aqui."))
      datatable(head(resultado_final(), 200), options = list(scrollX = TRUE, pageLength = 12), rownames = FALSE)
    })
    output$script_preview <- renderText(codigo_rv())

    output$status_indicador <- renderUI({
      if (is.null(resultado_rv())) {
        div(style = "color:#888; font-size:0.85rem;", "Nenhuma transformação aplicada ainda.")
      } else {
        r <- resultado_final()
        div(style = "font-size:0.85rem;",
            span(style = "color:#2E7D8F; font-weight:600;", "✓ Dados arrumados"), br(),
            sprintf("%d linhas × %d colunas", nrow(r), ncol(r)))
      }
    })

    # Downloads
    output$baixar_script <- downloadHandler(
      filename = function() paste0("arrumar_dados_", Sys.Date(), ".R"),
      content  = function(file) writeLines(codigo_rv(), file)
    )
    output$baixar_dados <- downloadHandler(
      filename = function() paste0("dados_arrumados_", Sys.Date(), ".xlsx"),
      content  = function(file) {
        req(resultado_rv())
        writexl::write_xlsx(resultado_final(), file)
      }
    )
  })
}

# Operador auxiliar (caso não exista no escopo do app)
if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a
