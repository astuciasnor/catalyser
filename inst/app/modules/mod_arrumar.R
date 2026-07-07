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

# --- Suporte ao MODO DELIMITADOR (separar sem escrever regex) ----------------

# Candidatos de separador, em ordem de prioridade. " - " vem antes de "-" e de
# " " de propósito: em "2025 - Valor US$ FOB" queremos cortar no " - " (limpo),
# não no "-" (deixaria espaços sobrando).
arrumar_delim_candidatos <- c(" - ", "_", "-", ".", ";", ",", "|", "/", " ")

# Rótulo amigável para mensagens (o espaço puro não aparece bem entre aspas).
arrumar_delim_rotulo <- function(d) if (identical(d, " ")) "espaço" else d

# Escapa metacaracteres de regex — usado só no names_sep do pivot_longer, que é
# interpretado como regex (o delim do separate_wider_delim é string literal).
arrumar_escape_regex <- function(s) {
  especiais <- c(".", "\\", "|", "(", ")", "[", "]", "{", "}", "^", "$", "*", "+", "?")
  chars <- strsplit(s, "", fixed = TRUE)[[1]]
  paste(vapply(chars, function(c) if (c %in% especiais) paste0("\\", c) else c,
               character(1)), collapse = "")
}

# Detecta o melhor separador olhando os textos: escolhe o 1º candidato que
# aparece >= 1 vez e SEMPRE o mesmo número de vezes (divisão uniforme).
# Devolve list(delim, n) ou NULL. `n` = nº de colunas resultantes.
arrumar_detectar_delim <- function(x) {
  x <- x[!is.na(x) & nzchar(x)]
  if (!length(x)) return(NULL)
  for (d in arrumar_delim_candidatos) {
    cont <- lengths(regmatches(x, gregexpr(d, x, fixed = TRUE)))
    if (all(cont >= 1) && length(unique(cont)) == 1)
      return(list(delim = d, n = unique(cont) + 1L))
  }
  NULL
}

# Confere, ANTES de aplicar, se o separador divide tudo no mesmo nº de partes e
# se esse nº bate com quantas colunas o usuário nomeou. Devolve msg ou NULL.
arrumar_checar_delim <- function(x, delim, n_nomes, rotulo) {
  x <- x[!is.na(x) & nzchar(x)]
  if (!length(x)) return(NULL)
  partes <- lengths(regmatches(x, gregexpr(delim, x, fixed = TRUE))) + 1L
  d <- arrumar_delim_rotulo(delim)
  if (length(unique(partes)) > 1)
    return(sprintf(paste0("O separador '%s' divide %s em números diferentes de ",
                          "partes (de %d a %d). Para casos irregulares, use o modo regex."),
                   d, rotulo, min(partes), max(partes)))
  if (unique(partes)[1] != n_nomes)
    return(sprintf(paste0("Com o separador '%s', cada valor gera %d partes, mas você ",
                          "nomeou %d coluna(s). Ajuste os nomes ou o separador."),
                   d, unique(partes)[1], n_nomes))
  NULL
}

# Nomes-rascunho para N colunas: parte1, parte2, ...
arrumar_nomes_padrao <- function(n) paste(paste0("parte", seq_len(max(2L, n))), collapse = ", ")

# --- Suporte à TIPAGEM (definir o tipo de cada coluna) -----------------------

# Tokens de tipo -> função de conversão (para o script .R).
arrumar_tipo_fun <- c(texto = "as.character", numero = "as.numeric",
                      inteiro = "as.integer", fator = "as.factor", data = "as.Date")

# Rótulos amigáveis para o seletor de tipo.
arrumar_tipo_choices <- c("Texto" = "texto", "Número" = "numero",
                          "Inteiro" = "inteiro", "Fator" = "fator", "Data" = "data")

# Detecta o tipo atual de um vetor (para pré-selecionar no modal).
arrumar_detectar_tipo <- function(x) {
  if (is.factor(x)) "fator"
  else if (inherits(x, "Date")) "data"
  else if (is.integer(x)) "inteiro"
  else if (is.numeric(x)) "numero"
  else "texto"
}

# Converte um vetor para o tipo escolhido (defensivo: erro -> mantém original).
arrumar_converter_tipo <- function(x, tipo) {
  tryCatch(switch(tipo,
    texto   = as.character(x),
    numero  = as.numeric(as.character(x)),
    inteiro = as.integer(as.character(x)),
    fator   = as.factor(x),
    data    = as.Date(x),
    x), error = function(e) x)
}

# --- Suporte à RECODIFICAÇÃO de níveis (padronizar rótulos de fator) ---------

# Sugere correções de níveis que diferem só por CAIXA / ACENTOS / ESPAÇOS /
# PONTUAÇÃO (quase-duplicatas). Devolve vetor nomeado antigo -> novo, apontando
# cada variante para a grafia mais frequente do seu grupo. NÃO junta rótulos
# semanticamente diferentes (isso o usuário faz à mão).
arrumar_sugerir_recode <- function(x) {
  vals <- as.character(x); vals <- vals[!is.na(vals) & nzchar(vals)]
  if (!length(vals)) return(character(0))
  norm <- function(s) {
    s <- tolower(trimws(s))
    s <- iconv(s, to = "ASCII//TRANSLIT"); s[is.na(s)] <- ""
    s <- gsub("[[:punct:]]", " ", s)
    trimws(gsub("\\s+", " ", s))
  }
  tab  <- table(vals)                 # frequência por grafia
  uniq <- names(tab)
  chave <- norm(uniq)
  sug <- character(0)
  for (k in unique(chave)) {
    grp <- uniq[chave == k]
    if (length(grp) > 1) {
      alvo <- grp[which.max(as.integer(tab[grp]))]   # grafia mais comum vira alvo
      for (g in setdiff(grp, alvo)) sug[g] <- alvo
    }
  }
  sug
}

# Validação da configuração antes de aplicar. Devolve NULL se OK, ou a mensagem.
arrumar_validar <- function(cfg, df) {
  if (any(!nzchar(cfg$novas)))
    return("Dê um nome a cada coluna nova (separe por vírgula).")

  # Extração: por delimitador (simples) ou por regex (avançado)
  if (identical(cfg$metodo, "delim")) {
    if (is.null(cfg$delim) || !nzchar(cfg$delim))
      return("Escolha ou digite o separador.")
    if (length(cfg$novas) < 2)
      return("Para separar por delimitador, defina pelo menos duas colunas (nomes separados por vírgula).")
  } else {
    ok <- tryCatch({ grepl(cfg$regex, "teste", perl = TRUE); TRUE }, error = function(e) FALSE)
    if (!ok)
      return("Regex inválida — confira os parênteses e as barras invertidas.")
    ng <- arrumar_n_grupos(cfg$regex)
    if (ng != length(cfg$novas))
      return(sprintf("A regex tem %d grupo(s) de captura, mas você nomeou %d coluna(s) nova(s).",
                     ng, length(cfg$novas)))
  }

  if (identical(cfg$modo, "separar")) {
    if (is.null(cfg$col_separar) || !nzchar(cfg$col_separar))
      return("Escolha a coluna a separar.")
    if (!(cfg$col_separar %in% names(df)))
      return("A coluna a separar não foi encontrada no conjunto atual.")
    if (identical(cfg$metodo, "delim")) {
      msg <- arrumar_checar_delim(as.character(df[[cfg$col_separar]]), cfg$delim,
                                  length(cfg$novas), "os valores")
      if (!is.null(msg)) return(msg)
    }
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
    if (identical(cfg$metodo, "delim")) {
      msg <- arrumar_checar_delim(cfg$cols_medida, cfg$delim,
                                  length(cfg$novas), "os nomes de coluna")
      if (!is.null(msg)) return(msg)
    }
  }
  NULL
}

# Aplica a transformação. Warnings viram avisos amigáveis, mas não abortam.
arrumar_aplicar <- function(cfg, df, on_warn = NULL, on_err = NULL) {
  withCallingHandlers(
    tryCatch({
      if (identical(cfg$modo, "separar")) {
        if (identical(cfg$metodo, "delim")) {
          # Corta os VALORES no separador literal — uma coluna por pedaço.
          tidyr::separate_wider_delim(
            df, cols = tidyselect::all_of(cfg$col_separar),
            delim = cfg$delim, names = cfg$novas,
            cols_remove = !isTRUE(cfg$manter_original))
        } else {
          # Extrai grupos da regex dos VALORES de uma coluna existente (sem pivô).
          tidyr::extract(df, col = cfg$col_separar, into = cfg$novas,
                         regex = cfg$regex, remove = !isTRUE(cfg$manter_original))
        }
      } else {
        long <- tidyr::pivot_longer(
          df,
          cols          = tidyselect::all_of(cfg$cols_medida),
          names_to      = cfg$novas,
          names_sep     = if (identical(cfg$metodo, "delim")) arrumar_escape_regex(cfg$delim) else NULL,
          names_pattern = if (identical(cfg$metodo, "delim")) NULL else cfg$regex,
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
# `tipos`     : lista/vetor nomeado coluna -> token de tipo (texto/numero/...).
# `recodes`   : lista coluna -> vetor nomeado nivel_antigo -> nivel_novo.
# `selecionar`: vetor de colunas a manter (no espaço já renomeado); NULL = todas.
arrumar_gerar_codigo <- function(cfg, info, renomear = character(0),
                                 tipos = character(0), recodes = list(),
                                 selecionar = NULL) {
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
    if (identical(cfg$metodo, "delim")) {
      linhas <- c(linhas,
        "# 2. Separar uma coluna existente em varias (por delimitador)",
        "dados_arrumados <- dados_largo |>",
        "  separate_wider_delim(",
        sprintf("    cols = %s,", arrumar_bt(cfg$col_separar)),
        sprintf('    delim = "%s",', esc(cfg$delim)),
        sprintf("    names = c(%s),", q(cfg$novas)),
        sprintf("    cols_remove = %s", if (isTRUE(cfg$manter_original)) "FALSE" else "TRUE"),
        "  )")
    } else {
      linhas <- c(linhas,
        "# 2. Separar uma coluna existente em varias (extract via regex)",
        "dados_arrumados <- dados_largo |>",
        "  extract(",
        sprintf("    col = %s,", arrumar_bt(cfg$col_separar)),
        sprintf("    into = c(%s),", q(cfg$novas)),
        sprintf('    regex = "%s",', esc(cfg$regex)),
        sprintf("    remove = %s", if (isTRUE(cfg$manter_original)) "FALSE" else "TRUE"),
        "  )")
    }
  } else {
    sep_linha <- if (identical(cfg$metodo, "delim"))
      sprintf('    names_sep = "%s",', esc(arrumar_escape_regex(cfg$delim)))
    else
      sprintf('    names_pattern = "%s",', esc(cfg$regex))
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
      sep_linha,
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
  if (length(recodes)) {
    linhas <- c(linhas, "",
      "# 6. Padronizar (recodificar) niveis de fator",
      "dados_arrumados <- dados_arrumados |>",
      "  mutate(")
    cols <- names(recodes)
    for (ci in seq_along(cols)) {
      mapa <- recodes[[cols[ci]]]
      pares <- vapply(seq_along(mapa), function(i)
        sprintf('      `%s` = "%s"', names(mapa)[i], esc(unname(mapa[i]))), character(1))
      linhas <- c(linhas,
        sprintf("    %s = dplyr::recode(%s,", arrumar_bt(cols[ci]), arrumar_bt(cols[ci])),
        paste(pares, collapse = ",\n"),
        sprintf("    )%s", if (ci < length(cols)) "," else ""))
    }
    linhas <- c(linhas, "  )")
  }
  if (length(tipos)) {
    cols <- names(tipos)
    pares <- vapply(seq_along(cols), function(i) {
      f <- arrumar_tipo_fun[[ tipos[[cols[i]]] ]]
      sprintf("    %s = %s(%s)%s", arrumar_bt(cols[i]), f, arrumar_bt(cols[i]),
              if (i < length(cols)) "," else "")
    }, character(1))
    linhas <- c(linhas, "",
      "# 7. Tipar colunas (definir o tipo de cada variavel)",
      "dados_arrumados <- dados_arrumados |>",
      "  mutate(", pares, "  )")
  }
  if (!is.null(selecionar) && length(selecionar)) {
    linhas <- c(linhas, "",
      "# 8. Selecionar apenas as colunas desejadas",
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
            helpText("As partes dos VALORES desta coluna viram novas colunas.")
          )
        )),

        # --- Extração: comum aos dois modos (delimitador ou regex) ---
        card(
          card_header(sprintf("%d. Separar em colunas", n_regex)),
          card_body(
            style = "padding: 12px 15px;",
            radioButtons(ns("metodo"), "Como separar as partes?",
              choices = c("Por delimitador (mais simples)" = "delim",
                          "Por padrão / regex (avançado)"   = "regex"),
              selected = "delim"),

            # --- Método DELIMITADOR: separador + nº de colunas + auto-detecção ---
            conditionalPanel(
              condition = sprintf("input['%s'] == 'delim'", ns("metodo")),
              actionButton(ns("auto_detectar_sep"), "Detectar separador automaticamente",
                           icon = icon("wand-magic-sparkles"),
                           class = "btn-outline-primary btn-sm w-100 mb-2"),
              selectInput(ns("delim_comum"), "Separador:",
                choices = c("Underscore  _"                = "_",
                            "Hífen  -"                     = "-",
                            "Hífen entre espaços  ' - '"   = " - ",
                            "Ponto  ."                     = ".",
                            "Ponto e vírgula  ;"           = ";",
                            "Vírgula  ,"                   = ",",
                            "Espaço"                       = " ",
                            "Outro (digitar)…"             = "__custom__")),
              conditionalPanel(
                condition = sprintf("input['%s'] == '__custom__'", ns("delim_comum")),
                textInput(ns("delim_custom"), "Digite o separador:", value = "")),
              numericInput(ns("n_cols"), "Número de colunas a criar:",
                           value = 2, min = 2, max = 12, step = 1),
              helpText("A IDE corta os valores no separador; cada pedaço vira uma coluna.")
            ),

            # --- Método REGEX (avançado) ---
            conditionalPanel(
              condition = sprintf("input['%s'] == 'regex'", ns("metodo")),
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
                          value = "^(\\d{4}) - (.*)$"))
            ),

            textInput(ns("novas_cols"), "Nomes das colunas novas (na ordem):",
                      value = "parte1, parte2"),
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
            actionButton(ns("abrir_renomear"), "Renomear colunas",
                         icon = icon("i-cursor"), class = "btn-outline-secondary btn-sm w-100 mb-2"),
            actionButton(ns("abrir_recodificar"), "Recodificar níveis",
                         icon = icon("tags"), class = "btn-outline-secondary btn-sm w-100 mb-2"),
            actionButton(ns("abrir_tipar"), "Tipar colunas",
                         icon = icon("sliders"), class = "btn-outline-secondary btn-sm w-100 mb-2"),
            actionButton(ns("abrir_selecionar"), "Selecionar variáveis",
                         icon = icon("list-check"), class = "btn-outline-secondary btn-sm w-100 mb-2"),
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
                tags$li("Separe o nome por delimitador (ou regex) em ano/métrica."),
                tags$li("Mantenha longo ou alargue uma métrica."))),
            if (mostrar_sep) tagList(
              tags$p(style = "margin: 0 0 6px;", strong("Separar"), " (metadados dentro de uma coluna):"),
              tags$ol(style = "padding-left: 16px; margin: 0 0 8px;",
                tags$li("Escolha a coluna a quebrar."),
                tags$li("Detecte/escolha o separador; cada pedaço vira uma coluna."))),
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
    tipos_rv     <- reactiveVal(list())                # coluna -> token de tipo
    recode_rv    <- reactiveVal(list())                # coluna -> mapa nivel_antigo->novo
    tp_cols      <- reactiveVal(character(0))          # ordem das colunas no modal de tipagem
    recode_niveis_atual <- reactiveVal(character(0))   # níveis exibidos no modal de recode
    sel_cols_rv  <- reactiveVal(NULL)                  # colunas a manter (NULL = todas)

    # Popular seletores de coluna quando os dados chegam
    observeEvent(base_data(), {
      updateSelectizeInput(session, "cols_medida", choices = names(base_data()),
                           selected