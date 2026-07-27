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
  # ALARGAR (pivot_wider avulso): só precisa de names_from e values_from.
  if (identical(cfg$modo, "alargar")) {
    if (is.null(cfg$names_from) || !nzchar(cfg$names_from))
      return("Escolha a coluna que vira novas colunas (names_from).")
    if (is.null(cfg$values_from) || !nzchar(cfg$values_from))
      return("Escolha a coluna com os valores (values_from).")
    if (!(cfg$names_from %in% names(df)))
      return("A coluna 'names_from' não existe no resultado atual.")
    if (!(cfg$values_from %in% names(df)))
      return("A coluna 'values_from' não existe no resultado atual.")
    if (identical(cfg$names_from, cfg$values_from))
      return("As colunas 'names_from' e 'values_from' devem ser diferentes.")
    return(NULL)
  }

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
      if (identical(cfg$modo, "alargar")) {
        # Espalha os níveis de names_from em colunas, preenchidas por values_from.
        tidyr::pivot_wider(df,
                           names_from  = tidyselect::all_of(cfg$names_from),
                           values_from = tidyselect::all_of(cfg$values_from))
      } else if (identical(cfg$modo, "separar")) {
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

# Linhas de UMA etapa de transformação, sempre canalizando `dados_arrumados`
# nele mesmo (encadeável). `n` é o número da etapa (para o comentário).
arrumar_codigo_transformacao <- function(cfg, n, esc, q) {
  if (identical(cfg$modo, "alargar")) {
    c(sprintf("# Etapa %d: alargar (longo -> largo)", n),
      "dados_arrumados <- dados_arrumados |>",
      sprintf("  pivot_wider(names_from = %s, values_from = %s)",
              arrumar_bt(cfg$names_from), arrumar_bt(cfg$values_from)))
  } else if (identical(cfg$modo, "separar")) {
    if (identical(cfg$metodo, "delim")) {
      c(sprintf("# Etapa %d: separar '%s' pelo delimitador '%s'", n, cfg$col_separar, cfg$delim),
        "dados_arrumados <- dados_arrumados |>",
        "  separate_wider_delim(",
        sprintf("    cols = %s,", arrumar_bt(cfg$col_separar)),
        sprintf('    delim = "%s",', esc(cfg$delim)),
        sprintf("    names = c(%s),", q(cfg$novas)),
        sprintf("    cols_remove = %s", if (isTRUE(cfg$manter_original)) "FALSE" else "TRUE"),
        "  )")
    } else {
      c(sprintf("# Etapa %d: separar '%s' por regex", n, cfg$col_separar),
        "dados_arrumados <- dados_arrumados |>",
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
    linhas <- c(
      sprintf("# Etapa %d: empilhar colunas largas (largo -> longo)", n),
      "cols_medida <- c(",
      paste0("  ", q(cfg$cols_medida)),
      ")",
      "dados_arrumados <- dados_arrumados |>",
      "  pivot_longer(",
      "    cols = all_of(cols_medida),",
      sprintf("    names_to = c(%s),", q(cfg$novas)),
      sep_linha,
      sprintf('    values_to = "%s"%s', cfg$values_to, if (cfg$como_numero) "," else ""),
      if (cfg$como_numero) sprintf("    values_transform = list(%s = as.numeric)", cfg$values_to) else NULL,
      "  )")
    if (identical(cfg$saida, "largo")) {
      linhas <- c(linhas,
        "dados_arrumados <- dados_arrumados |>",
        sprintf("  pivot_wider(names_from = %s, values_from = %s)", cfg$wider_names, cfg$values_to))
    }
    linhas
  }
}

# Gera o texto do script .R a partir da PILHA de etapas (`passos`, lista de cfgs)
# aplicadas em sequência, mais os passos finais de polimento.
# `renomear`  : vetor nomeado names = nome antigo, valor = nome novo.
# `tipos`     : lista/vetor nomeado coluna -> token de tipo (texto/numero/...).
# `recodes`   : lista coluna -> vetor nomeado nivel_antigo -> nivel_novo.
# `selecionar`: vetor de colunas a manter (no espaço já renomeado); NULL = todas.
arrumar_gerar_codigo <- function(passos, info, renomear = character(0),
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

  linhas <- c(
    "# Script gerado pela CatalyseR — Arrumacao dos dados",
    "library(tidyverse)",
    if (usa_readxl) "library(readxl)" else NULL,
    "",
    "# 1. Dados de entrada (ajuste o caminho/aba se necessario)",
    leitura,
    "dados_arrumados <- dados_largo",
    ""
  )
  for (i in seq_along(passos)) {
    linhas <- c(linhas, arrumar_codigo_transformacao(passos[[i]], i, esc, q), "")
  }
  if (length(renomear)) {
    pares <- vapply(seq_along(renomear), function(i)
      sprintf("%s = %s", arrumar_bt(unname(renomear[i])), arrumar_bt(names(renomear)[i])),
      character(1))
    linhas <- c(linhas, "",
      "# Renomear colunas",
      "dados_arrumados <- dados_arrumados |>",
      paste0("  rename(", paste(pares, collapse = ", "), ")"))
  }
  if (length(recodes)) {
    linhas <- c(linhas, "",
      "# Padronizar (recodificar) niveis de fator",
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
      "# Tipar colunas (definir o tipo de cada variavel)",
      "dados_arrumados <- dados_arrumados |>",
      "  mutate(", pares, "  )")
  }
  if (!is.null(selecionar) && length(selecionar)) {
    linhas <- c(linhas, "",
      "# Selecionar apenas as colunas desejadas",
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

  # No menu "Empilhar" também oferecemos ALARGAR (pivot_wider avulso) como
  # operação da etapa. op_emp() mostra os cartões de empilhar só quando op=='empilhar'.
  reshape_fixo <- identical(modo_fixo, "empilhar")
  op_emp <- function(ui) if (reshape_fixo)
    conditionalPanel(sprintf("input['%s'] == 'empilhar'", ns("op")), ui) else ui

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

        # --- Escolha da operação (só no menu Empilhar): empilhar ou alargar ---
        if (reshape_fixo) card(
          card_header("Operação desta etapa"),
          card_body(
            style = "padding: 12px 15px;",
            radioButtons(ns("op"), NULL,
              choices = c("Empilhar (largo → longo)" = "empilhar",
                          "Alargar (longo → largo)"  = "alargar"),
              selected = "empilhar"),
            helpText(HTML("<b>Empilhar</b>: junta colunas largas numa só.<br><b>Alargar</b>: espalha os níveis de uma coluna em várias (útil após empilhar)."))
          )
        ),

        # --- Modo ALARGAR (pivot_wider avulso): só no menu Empilhar ---
        if (reshape_fixo) conditionalPanel(
          condition = sprintf("input['%s'] == 'alargar'", ns("op")),
          card(
            card_header("Alargar (longo → largo)"),
            card_body(
              style = "padding: 12px 15px;",
              selectInput(ns("wider_names2"), "Coluna que vira novas colunas (names_from):", choices = NULL),
              selectInput(ns("wider_values"), "Coluna com os valores (values_from):", choices = NULL),
              helpText("Espalha os níveis da 1ª coluna em colunas novas, preenchidas pela 2ª.")
            )
          )
        ),

        # --- Modo EMPILHAR: colunas de medida ---
        if (mostrar_emp) so_modo("empilhar", op_emp(card(
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
        ))),

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

        # --- Extração: comum a separar e empilhar (delimitador ou regex) ---
        op_emp(card(
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
        )),

        # (O "alargar" agora é uma OPERAÇÃO à parte — não mais um modo de saída do
        # empilhar. Empilhar sempre gera formato LONGO; para alargar, escolha a
        # operação "Alargar (longo → largo)" acima e aplique como nova etapa.)

        actionButton(ns("aplicar"), "Aplicar transformação",
                     icon = icon("play"), class = "btn-primary w-100 mt-2"),
        div(class = "d-flex gap-2 mt-2",
          actionButton(ns("desfazer"), "Desfazer última etapa",
                       icon = icon("rotate-left"), class = "btn-outline-secondary btn-sm w-100"),
          uiOutput(ns("passos_indicador"), inline = TRUE)),
        helpText("As etapas se acumulam: você separa uma coluna, depois outra, e assim por diante. 'Desfazer' remove a última.")
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
            downloadButton(ns("baixar_script"), "Baixar script .R",
                           class = "btn-outline-secondary btn-sm w-100 mb-2"),
            downloadButton(ns("baixar_dados"), "Baixar dados arrumados (.xlsx)",
                           class = "btn-outline-primary btn-sm w-100 mb-2"),
            hr(style = "margin: 10px 0;"),
            actionButton(ns("usar_analises"), "Aplicar à Base Compartilhada",
                         icon = icon("share-from-square"), class = "btn-primary w-100"),
            helpText("Esta tabela passa a compor a Base Compartilhada (dados_analise). Você pode voltar aos dados importados no painel de importação.")
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
            tags$p(
              style = "margin: 0;",
              "Depois de aplicar à Base Compartilhada, use ",
              strong("Organizar Variáveis"),
              " para selecionar, renomear, tipar ou recodificar."
            )
          )
        )
      )
    )
  )
}

# --- SERVER ------------------------------------------------------------------

mod_arrumar_server <- function(id, data_rv, import_info, modo_fixo = NULL, on_usar = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    base_data <- reactive({ req(data_rv()); data_rv() })

    # Modo efetivo: no menu Empilhar, a operação vem do seletor (empilhar/alargar);
    # no menu Separar é fixo; no modo combinado, do radio principal.
    modo_atual <- reactive({
      if (identical(modo_fixo, "empilhar")) (input$op %||% "empilhar")
      else if (!is.null(modo_fixo)) modo_fixo
      else input$modo
    })

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
    passos_rv    <- reactiveVal(list())                # PILHA de transformações aplicadas (cfgs)

    # Entrada da PRÓXIMA transformação = resultado acumulado (ou o original, se
    # nada foi aplicado ainda). É isso que torna as separações ENCADEÁVEIS.
    entrada_atual <- reactive({
      r <- resultado_rv(); if (is.null(r)) base_data() else r
    })

    # Atualiza os seletores de coluna para refletir a entrada atual (encadeada).
    atualizar_seletores <- function(df) {
      updateSelectizeInput(session, "cols_medida", choices = names(df),
                           selected = character(0), server = TRUE)
      updateSelectInput(session, "col_separar", choices = names(df))
      # Alargar: names_from (coluna que vira colunas) e values_from (os valores).
      updateSelectInput(session, "wider_names2", choices = names(df))
      updateSelectInput(session, "wider_values", choices = names(df))
    }

    # Popular seletores de coluna quando os dados chegam (reinicia tudo)
    observeEvent(base_data(), {
      atualizar_seletores(base_data())
      resultado_rv(NULL)
      passos_rv(list())
      cfg_rv(NULL)
      renomear_rv(character(0))
      tipos_rv(list())
      recode_rv(list())
      sel_cols_rv(NULL)
      codigo_rv("# Configure à esquerda e clique em 'Aplicar transformação'.")
    })

    # Detecção automática (heurística: começa com 4 dígitos)
    observeEvent(input$auto_detectar, {
      med <- grep("^\\d{4}", names(entrada_atual()), value = TRUE)
      if (length(med) == 0) {
        showNotification("Nenhuma coluna começando com 4 dígitos foi encontrada.", type = "warning")
      } else {
        updateSelectizeInput(session, "cols_medida", selected = med)
      }
    })

    # Identificadores = complemento (como o setdiff do script)
    output$cols_id_preview <- renderText({
      req(entrada_atual())
      ids <- setdiff(names(entrada_atual()), input$cols_medida)
      if (length(ids) == 0) "—" else paste(ids, collapse = ", ")
    })

    # Ajuda "?" ao lado do seletor: abre o guia rápido de regex
    observeEvent(input$ajuda_regex, { showModal(arrumar_ajuda_regex_modal()) })

    # Preset -> sugerir regex e nomes das colunas novas (só no método regex)
    observeEvent(input$regex_preset, {
      req(identical(input$metodo, "regex"))
      if (input$regex_preset != "custom") {
        p <- arrumar_presets[[input$regex_preset]]
        updateTextInput(session, "novas_cols", value = p$novas)
        updateTextInput(session, "regex", value = p$regex)
      }
    }, ignoreInit = TRUE)

    # Regex efetiva (preset ou personalizada)
    regex_efetiva <- reactive({
      if ((input$regex_preset %||% "custom") == "custom") input$regex %||% ""
      else arrumar_presets[[input$regex_preset]]$regex
    })

    # Separador efetivo: opção comum ou o campo "Outro (digitar)".
    delim_efetivo <- reactive({
      if ((input$delim_comum %||% "") == "__custom__") input$delim_custom %||% ""
      else input$delim_comum %||% ""
    })

    # Nº de colunas -> gera nomes-rascunho (parte1, parte2, ...) no método delim.
    observeEvent(input$n_cols, {
      req(identical(input$metodo, "delim"))
      nn <- input$n_cols; if (is.null(nn) || is.na(nn)) nn <- 2
      updateTextInput(session, "novas_cols", value = arrumar_nomes_padrao(as.integer(nn)))
    })

    # Detectar o separador automaticamente, olhando os valores (separar) ou os
    # nomes das colunas de medida (empilhar).
    observeEvent(input$auto_detectar_sep, {
      if (identical(modo_atual(), "separar")) {
        if (is.null(input$col_separar) || !(input$col_separar %in% names(entrada_atual()))) {
          showNotification("Escolha primeiro a coluna a separar.", type = "warning"); return()
        }
        x <- as.character(entrada_atual()[[input$col_separar]]); contexto <- "os valores"
      } else {
        if (length(input$cols_medida) == 0) {
          showNotification("Selecione primeiro as colunas de medida.", type = "warning"); return()
        }
        x <- input$cols_medida; contexto <- "os nomes das colunas"
      }
      det <- arrumar_detectar_delim(x)
      if (is.null(det)) {
        showNotification(sprintf(
          "Não encontrei um separador que divida %s no mesmo número de partes. Tente o modo regex.",
          contexto), type = "warning", duration = 9)
        return()
      }
      opcoes <- c("_", "-", " - ", ".", ";", ",", " ")
      if (det$delim %in% opcoes) {
        updateSelectInput(session, "delim_comum", selected = det$delim)
      } else {
        updateSelectInput(session, "delim_comum", selected = "__custom__")
        updateTextInput(session, "delim_custom", value = det$delim)
      }
      updateNumericInput(session, "n_cols", value = det$n)
      updateTextInput(session, "novas_cols", value = arrumar_nomes_padrao(det$n))
      showNotification(sprintf("Separador detectado: '%s' → %d colunas.",
                               arrumar_delim_rotulo(det$delim), det$n),
                       type = "message", duration = 6)
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
        metodo      = input$metodo %||% "delim",
        delim       = delim_efetivo(),
        regex       = if (identical(input$metodo, "regex")) regex_efetiva() else "",
        novas       = trimws(strsplit(input$novas_cols %||% "", ",")[[1]]),
        # campos do modo EMPILHAR:
        cols_medida = input$cols_medida,
        values_to   = trimws(input$values_to %||% ""),
        como_numero = isTRUE(input$como_numero),
        saida       = input$saida,
        wider_names = input$wider_names %||% "",
        # campos do modo SEPARAR:
        col_separar     = input$col_separar %||% "",
        manter_original = isTRUE(input$manter_original),
        # campos do modo ALARGAR (pivot_wider avulso):
        names_from  = input$wider_names2 %||% "",
        values_from = input$wider_values %||% ""
      )
      cfg$novas <- cfg$novas[nzchar(cfg$novas)]

      # Valida e aplica sobre a ENTRADA ATUAL (resultado acumulado), não o original.
      entrada <- entrada_atual()
      msg <- arrumar_validar(cfg, entrada)
      if (!is.null(msg)) { showNotification(msg, type = "error", duration = 9); return() }

      res <- arrumar_aplicar(
        cfg, entrada,
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

      # Empilha a etapa na pilha e guarda o resultado acumulado.
      passos_rv(c(passos_rv(), list(cfg)))
      resultado_rv(res)
      cfg_rv(cfg)
      # Modelo "primeiro estrutura, depois polir": o polimento (renomear/
      # recodificar/tipar/selecionar) reinicia a cada nova etapa estrutural, para
      # que os nomes exibidos nunca divirjam dos que a próxima etapa enxerga.
      renomear_rv(character(0))
      tipos_rv(list())
      recode_rv(list())
      sel_cols_rv(NULL)
      atualizar_seletores(res)                          # próxima etapa pode mirar as colunas criadas
      codigo_rv(arrumar_gerar_codigo(passos_rv(), import_info()))
      showNotification(sprintf("Etapa %d aplicada.", length(passos_rv())),
                       type = "message", duration = 3)
    })

    # --- Desfazer: remove a última etapa da pilha e recalcula do zero ---
    observeEvent(input$desfazer, {
      passos <- passos_rv()
      if (!length(passos)) {
        showNotification("Não há etapas para desfazer.", type = "warning"); return()
      }
      passos <- passos[-length(passos)]                 # remove a última
      # Recalcula a partir do original, aplicando as etapas restantes em ordem.
      res <- base_data()
      ok <- TRUE
      for (cfg in passos) {
        res <- arrumar_aplicar(cfg, res,
          on_warn = function(m) NULL,
          on_err  = function(m) { showNotification(paste("Erro ao recalcular:", m), type = "error"); NULL })
        if (is.null(res)) { ok <- FALSE; break }
      }
      passos_rv(passos)
      resultado_rv(if (length(passos) && ok) res else NULL)
      renomear_rv(character(0)); tipos_rv(list()); recode_rv(list()); sel_cols_rv(NULL)
      atualizar_seletores(if (is.null(resultado_rv())) base_data() else resultado_rv())
      if (length(passos)) {
        codigo_rv(arrumar_gerar_codigo(passos, import_info()))
      } else {
        codigo_rv("# Configure à esquerda e clique em 'Aplicar transformação'.")
      }
      showNotification("Última etapa desfeita.", type = "message", duration = 3)
    })

    # Nomes das colunas do resultado no espaço JÁ renomeado (base do modal de seleção)
    nomes_renomeados <- reactive({
      r <- resultado_rv(); req(r)
      nm <- names(r); mp <- renomear_rv()
      if (length(mp)) for (o in names(mp)) nm[nm == o] <- unname(mp[o])
      nm
    })

    # Cadeia do resultado: pivot -> renomear -> recodificar -> tipar -> selecionar.
    # Cada etapa é um reactive, então os modais leem o estado da etapa anterior.
    res_renomeado <- reactive({
      r <- resultado_rv(); req(r)
      mp <- renomear_rv()
      if (length(mp)) {
        nm <- names(r)
        for (o in names(mp)) nm[nm == o] <- unname(mp[o])
        names(r) <- nm
      }
      r
    })
    res_recodificado <- reactive({
      r <- res_renomeado()
      rc <- recode_rv()
      for (col in names(rc)) if (col %in% names(r)) {
        mapa <- rc[[col]]; x <- as.character(r[[col]])
        for (o in names(mapa)) x[!is.na(x) & x == o] <- unname(mapa[o])
        r[[col]] <- x
      }
      r
    })
    res_tipado <- reactive({
      r <- res_recodificado()
      tp <- tipos_rv()
      for (col in names(tp)) if (col %in% names(r))
        r[[col]] <- arrumar_converter_tipo(r[[col]], tp[[col]])
      r
    })
    resultado_final <- reactive({
      r <- res_tipado()
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
      # nomes mudaram: tipagem, recodificação e seleção (que dependem dos nomes) reiniciam
      tipos_rv(list())
      recode_rv(list())
      sel_cols_rv(NULL)
      codigo_rv(arrumar_gerar_codigo(passos_rv(), import_info(), mp))
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
      codigo_rv(arrumar_gerar_codigo(passos_rv(), import_info(), renomear_rv(),
                                     tipos = tipos_rv(), recodes = recode_rv(),
                                     selecionar = sel_cols_rv()))
      removeModal()
      showNotification("Seleção de variáveis aplicada.", type = "message", duration = 3)
    })

    # --- Tipar colunas (modal): definir o tipo de cada variável ---
    observeEvent(input$abrir_tipar, {
      if (is.null(resultado_rv())) {
        showNotification("Aplique uma transformação antes de tipar.", type = "warning"); return()
      }
      r <- res_recodificado()                 # tipagem age depois da recodificação
      cols <- names(r); tp_cols(cols)
      atual <- tipos_rv()
      showModal(modalDialog(
        title = "Tipar colunas", size = "l", easyClose = TRUE,
        helpText("Defina o tipo de cada coluna. O padrão já reflete o tipo atual; mude só o que precisar (ex.: 'ano' para Inteiro, 'periodo' para Fator)."),
        div(style = "max-height: 430px; overflow-y: auto; padding-right: 6px;",
          lapply(seq_along(cols), function(i) {
            sel_tipo <- if (!is.null(atual[[cols[i]]])) atual[[cols[i]]] else arrumar_detectar_tipo(r[[cols[i]]])
            div(style = "display:flex; gap:10px; align-items:center; margin-bottom:6px;",
              div(style = "flex:1; font-size:0.82rem; color:#666; word-break:break-word;", cols[i]),
              div(style = "flex:0 0 20px; text-align:center; color:#aaa;", "→"),
              div(style = "flex:1;", selectInput(ns(paste0("tp_", i)), NULL,
                    choices = arrumar_tipo_choices, selected = sel_tipo, width = "100%")))
          })),
        footer = tagList(modalButton("Cancelar"),
                         actionButton(ns("confirmar_tipar"), "Aplicar", class = "btn-primary"))
      ))
    })

    observeEvent(input$confirmar_tipar, {
      cols <- tp_cols(); req(length(cols) > 0)
      r <- res_recodificado()
      novo <- list()
      for (i in seq_along(cols)) {
        sel <- input[[paste0("tp_", i)]]
        if (is.null(sel)) next
        natural <- if (cols[i] %in% names(r)) arrumar_detectar_tipo(r[[cols[i]]]) else "texto"
        if (!identical(sel, natural)) novo[[cols[i]]] <- sel   # guarda só as mudanças
      }
      tipos_rv(novo)
      codigo_rv(arrumar_gerar_codigo(passos_rv(), import_info(), renomear_rv(),
                                     tipos = novo, recodes = recode_rv(),
                                     selecionar = sel_cols_rv()))
      removeModal()
      showNotification(if (length(novo)) sprintf("%d coluna(s) tipada(s).", length(novo))
                       else "Nenhuma mudança de tipo.", type = "message", duration = 3)
    })

    # --- Recodificar níveis (modal): padronizar rótulos de uma coluna ---
    observeEvent(input$abrir_recodificar, {
      if (is.null(resultado_rv())) {
        showNotification("Aplique uma transformação antes de recodificar.", type = "warning"); return()
      }
      r <- res_renomeado()
      cand <- names(r)[vapply(r, function(x) is.character(x) || is.factor(x), logical(1))]
      if (!length(cand)) {
        showNotification("Nenhuma coluna de texto/fator para recodificar.", type = "warning"); return()
      }
      showModal(modalDialog(
        title = "Recodificar níveis", size = "l", easyClose = TRUE,
        helpText("Escolha a coluna e ajuste os níveis (antigo → novo). 'Detectar' sugere correções de caixa, acentos e espaços; junções por significado você faz à mão."),
        selectInput(ns("recode_col"), "Coluna:", choices = cand),
        actionButton(ns("recode_detectar"), "Detectar variações automaticamente",
                     icon = icon("wand-magic-sparkles"), class = "btn-outline-primary btn-sm mb-2"),
        uiOutput(ns("recode_niveis")),
        footer = tagList(modalButton("Cancelar"),
                         actionButton(ns("confirmar_recodificar"), "Aplicar", class = "btn-primary"))
      ))
    })

    # Lista os níveis da coluna escolhida (antigo -> campo editável com o novo)
    output$recode_niveis <- renderUI({
      req(input$recode_col)
      r <- res_renomeado(); req(input$recode_col %in% names(r))
      niveis <- sort(unique(as.character(r[[input$recode_col]])))
      niveis <- niveis[!is.na(niveis) & nzchar(niveis)]
      recode_niveis_atual(niveis)
      mp <- recode_rv()[[input$recode_col]]
      div(style = "max-height: 340px; overflow-y: auto; padding-right: 6px;",
        lapply(seq_along(niveis), function(i) {
          valor <- if (!is.null(mp) && niveis[i] %in% names(mp)) unname(mp[niveis[i]]) else niveis[i]
          div(style = "display:flex; gap:10px; align-items:center; margin-bottom:6px;",
            div(style = "flex:1; font-size:0.82rem; color:#666; word-break:break-word;", niveis[i]),
            div(style = "flex:0 0 20px; text-align:center; color:#aaa;", "→"),
            div(style = "flex:1;", textInput(ns(paste0("rec_", i)), NULL, value = valor, width = "100%")))
        }))
    })

    observeEvent(input$recode_detectar, {
      req(input$recode_col)
      r <- res_renomeado(); req(input$recode_col %in% names(r))
      sug <- arrumar_sugerir_recode(r[[input$recode_col]])
      if (!length(sug)) {
        showNotification("Nenhuma variação óbvia (caixa/acentos/espaços) encontrada.",
                         type = "message", duration = 5); return()
      }
      niveis <- recode_niveis_atual()
      for (i in seq_along(niveis))
        if (niveis[i] %in% names(sug))
          updateTextInput(session, paste0("rec_", i), value = unname(sug[niveis[i]]))
      showNotification(sprintf("%d nível(is) com sugestão de padronização.", length(sug)),
                       type = "message", duration = 5)
    })

    observeEvent(input$confirmar_recodificar, {
      col <- input$recode_col; req(col)
      niveis <- recode_niveis_atual(); req(length(niveis) > 0)
      novos <- vapply(seq_along(niveis), function(i) {
        v <- input[[paste0("rec_", i)]]; if (is.null(v)) niveis[i] else trimws(v)
      }, character(1))
      mapa <- stats::setNames(novos, niveis)
      mapa <- mapa[nzchar(mapa) & unname(mapa) != names(mapa)]   # só mudanças reais
      rc <- recode_rv()
      if (length(mapa)) rc[[col]] <- mapa else rc[[col]] <- NULL
      recode_rv(rc)
      codigo_rv(arrumar_gerar_codigo(passos_rv(), import_info(), renomear_rv(),
                                     tipos = tipos_rv(), recodes = rc,
                                     selecionar = sel_cols_rv()))
      removeModal()
      showNotification(if (length(mapa)) sprintf("%d nível(is) recodificado(s) em '%s'.", length(mapa), col)
                       else "Nenhuma alteração de nível.", type = "message", duration = 3)
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
        r <- resultado_final(); np <- length(passos_rv())
        div(style = "font-size:0.85rem;",
            span(style = "color:#2E7D8F; font-weight:600;", "✓ Dados arrumados"), br(),
            sprintf("%d etapa(s) · %d linhas × %d colunas", np, nrow(r), ncol(r)))
      }
    })

    # Indicador compacto de nº de etapas ao lado do botão Desfazer
    output$passos_indicador <- renderUI({
      np <- length(passos_rv())
      span(style = "font-size:0.78rem; color:#2E7D8F; white-space:nowrap; align-self:center;",
           sprintf("%d etapa(s)", np))
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

    # Promover o resultado arrumado para ser o dataset ativo das análises.
    observeEvent(input$usar_analises, {
      if (is.null(resultado_rv())) {
        showNotification("Aplique uma transformação antes de usar nas análises.", type = "warning")
        return()
      }
      if (is.function(on_usar)) {
        fonte <- if (identical(modo_fixo, "separar")) "resultado do Separar" else "resultado do Empilhar"
        on_usar(resultado_final(), fonte, codigo_rv())
      }
    })
  })
}

# Operador auxiliar (caso não exista no escopo do app)
if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a
