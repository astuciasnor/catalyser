# Registro de tratamentos + pipeline de preparo para a CatalyseR
# ---------------------------------------------------------------------------
# A ESPINHA do preparo de dados: uma trilha ORDENADA e capturavel de etapas.
# Cada TIPO de tratamento e uma entrada do registro `tratamentos`, com quatro
# funcoes puras (contrato):
#   rotulo(p)      -> texto curto para o chip da trilha
#   validar(df, p) -> NULL se OK, ou mensagem de erro (checa antes de aplicar)
#   aplicar(df, p) -> data.frame transformado (df -> df)
#   codigo(p)      -> string com o fragmento de codigo R equivalente
# O dataset ativo e o REPLAY do pipeline sobre os dados brutos (ordem LOGICA).

# --- Helpers puros -----------------------------------------------------------

trat_bt <- function(x) {
  ok <- grepl("^[A-Za-z.][A-Za-z0-9._]*$", x) & !grepl("^\\.[0-9]", x)
  ifelse(ok, x, paste0("`", x, "`"))
}

trat_num_txt <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return("0")
  format(x, scientific = FALSE, trim = TRUE)
}

trat_q <- function(v) paste(sprintf('"%s"', v), collapse = ", ")

trat_moda <- function(x) {
  ux <- unique(x[!is.na(x)])
  if (!length(ux)) return(NA)
  ux[which.max(tabulate(match(x, ux)))]
}

trat_cols_num <- function(df) names(df)[vapply(df, is.numeric, logical(1))]

trat_aplica_op <- function(x, op, limiar) {
  switch(op,
    ">=" = x >= limiar, ">" = x > limiar,
    "<=" = x <= limiar, "<" = x < limiar,
    x >= limiar)
}

# Fator de um prefixo SI (potencia de dez). Vazio/NULL = base (1).
trat_fator_si <- function(sim) {
  if (is.null(sim) || !nzchar(sim)) return(1)
  switch(sim,
    "p" = 1e-12, "n" = 1e-9, "µ" = 1e-6, "u" = 1e-6, "m" = 1e-3,
    "k" = 1e3, "M" = 1e6, "G" = 1e9, "T" = 1e12, 1)
}

# Operadores de filtro de linha (6 comparacoes).
trat_filtro_op <- function(x, op, v) {
  switch(op, ">=" = x >= v, ">" = x > v, "<=" = x <= v,
            "<" = x < v, "==" = x == v, "!=" = x != v, x == v)
}

trat_contingencia_tidy <- function(df, linha, coluna, percentual = "none") {
  dados <- df[!is.na(df[[linha]]) & !is.na(df[[coluna]]), c(linha, coluna), drop = FALSE]
  tab <- table(
    factor(dados[[linha]], levels = unique(dados[[linha]])),
    factor(dados[[coluna]], levels = unique(dados[[coluna]]))
  )
  tidy <- as.data.frame(tab, responseName = "n", stringsAsFactors = FALSE)
  names(tidy)[1:2] <- c(linha, coluna)
  tidy$n <- as.integer(tidy$n)

  if (!identical(percentual, "none")) {
    denominador <- switch(
      percentual,
      row = ave(tidy$n, tidy[[linha]], FUN = sum),
      col = ave(tidy$n, tidy[[coluna]], FUN = sum),
      total = rep(sum(tidy$n), nrow(tidy))
    )
    tidy$percentual <- ifelse(denominador > 0, 100 * tidy$n / denominador, 0)
  }

  attr(tidy, "catalyser_contingencia") <- list(
    var_row = linha,
    var_col = coluna,
    freq = "n",
    percentual = percentual
  )
  tidy
}

trat_contingencia_codigo <- function(p) {
  linha <- trat_bt(p$linha)
  coluna <- trat_bt(p$coluna)
  linhas <- c(
    sprintf("dados <- dados |> dplyr::filter(!is.na(%s), !is.na(%s))", linha, coluna),
    sprintf(
      "dados <- dados |> dplyr::count(%s, %s, name = \"n\") |> tidyr::complete(%s, %s, fill = list(n = 0))",
      linha, coluna, linha, coluna
    )
  )
  if (!identical(p$percentual, "none")) {
    escopo <- switch(
      p$percentual,
      row = sprintf("dplyr::group_by(%s)", linha),
      col = sprintf("dplyr::group_by(%s)", coluna),
      total = NULL
    )
    if (!is.null(escopo)) linhas <- c(linhas, sprintf("dados <- dados |> %s", escopo))
    linhas <- c(
      linhas,
      "dados <- dados |> dplyr::mutate(percentual = 100 * n / sum(n))",
      if (!is.null(escopo)) "dados <- dados |> dplyr::ungroup()" else NULL
    )
  }
  paste(linhas, collapse = "\n")
}

# =============================================================================
# REGISTRO DE TRATAMENTOS
# =============================================================================

tratamentos <- list(

  tratar_na = list(
    rotulo = function(p) {
      alvo <- if (identical(p$coluna, "__num__")) "colunas numericas" else p$coluna
      met <- c(remover = "remover linhas", media = "imputar media",
               mediana = "imputar mediana", moda = "imputar moda",
               constante = "valor constante")[p$metodo]
      sprintf("Tratar NA de %s (%s)", alvo, met)
    },
    validar = function(df, p) {
      if (identical(p$coluna, "__num__")) {
        if (!length(trat_cols_num(df))) return("Nao ha colunas numericas para tratar.")
      } else if (!p$coluna %in% names(df)) {
        return(sprintf("A coluna '%s' nao existe neste ponto do pipeline.", p$coluna))
      }
      if (identical(p$metodo, "constante") && (is.null(p$valor) || is.na(p$valor)))
        return("Informe o valor constante para imputacao.")
      if (p$metodo %in% c("media", "mediana") && !identical(p$coluna, "__num__") &&
          !is.numeric(df[[p$coluna]]))
        return("Media/mediana exigem coluna numerica.")
      NULL
    },
    aplicar = function(df, p) {
      cols <- if (identical(p$coluna, "__num__")) trat_cols_num(df) else p$coluna
      if (identical(p$metodo, "remover")) {
        keep <- stats::complete.cases(df[, cols, drop = FALSE])
        return(df[keep, , drop = FALSE])
      }
      for (c in cols) {
        x <- df[[c]]
        imp <- switch(p$metodo,
          media     = mean(x, na.rm = TRUE),
          mediana   = stats::median(x, na.rm = TRUE),
          moda      = trat_moda(x),
          constante = p$valor)
        x[is.na(x)] <- imp
        df[[c]] <- x
      }
      df
    },
    codigo = function(p) {
      if (identical(p$metodo, "remover")) {
        if (identical(p$coluna, "__num__"))
          return("dados <- tidyr::drop_na(dados, dplyr::where(is.numeric))")
        return(sprintf("dados <- tidyr::drop_na(dados, %s)", trat_bt(p$coluna)))
      }
      fun <- switch(p$metodo,
        media     = "mean(., na.rm = TRUE)",
        mediana   = "median(., na.rm = TRUE)",
        moda      = "trat_moda(.)",
        constante = trat_num_txt(p$valor))
      if (identical(p$coluna, "__num__")) {
        sprintf("dados <- dados |> dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ ifelse(is.na(.), %s, .)))", fun)
      } else {
        col <- trat_bt(p$coluna)
        sprintf("dados <- dados |> dplyr::mutate(%s = ifelse(is.na(%s), %s, %s))",
                col, col, sub("\\.", col, fun), col)
      }
    }
  ),

  dicotomizar = list(
    rotulo = function(p) {
      if (identical(p$origem, "numerica"))
        sprintf("Dicotomizar %s = (%s %s %s)", p$nome, p$coluna, p$operador, trat_num_txt(p$limiar))
      else
        sprintf("Dicotomizar %s = (%s em {%s})", p$nome, p$coluna, paste(p$niveis_1, collapse = ", "))
    },
    validar = function(df, p) {
      if (is.null(p$nome) || !nzchar(trimws(p$nome))) return("De um nome a variavel 0/1.")
      if (!p$coluna %in% names(df))
        return(sprintf("A coluna '%s' nao existe neste ponto do pipeline.", p$coluna))
      if (identical(p$origem, "numerica")) {
        if (is.null(p$limiar) || is.na(p$limiar)) return("Informe o limiar numerico.")
        if (!is.numeric(suppressWarnings(as.numeric(df[[p$coluna]]))))
          return("Dicotomizacao por limiar exige coluna numerica.")
      } else {
        if (is.null(p$niveis_1) || !length(p$niveis_1)) return("Escolha ao menos um nivel para virar 1.")
      }
      NULL
    },
    aplicar = function(df, p) {
      if (identical(p$origem, "numerica")) {
        x <- suppressWarnings(as.numeric(df[[p$coluna]]))
        df[[p$nome]] <- as.integer(trat_aplica_op(x, p$operador, p$limiar))
      } else {
        df[[p$nome]] <- as.integer(as.character(df[[p$coluna]]) %in% p$niveis_1)
      }
      df
    },
    codigo = function(p) {
      col <- trat_bt(p$coluna); nome <- trat_bt(p$nome)
      if (identical(p$origem, "numerica"))
        sprintf("dados <- dados |> dplyr::mutate(%s = as.integer(%s %s %s))",
                nome, col, p$operador, trat_num_txt(p$limiar))
      else
        sprintf("dados <- dados |> dplyr::mutate(%s = as.integer(%s %%in%% c(%s)))",
                nome, col, trat_q(p$niveis_1))
    }
  ),

  padronizar = list(
    rotulo = function(p) {
      met <- c(zscore = "z-score", centralizar = "centralizar",
               normalizar = "normalizar 0-1")[p$metodo]
      sprintf("Padronizar '%s' (%s) -> %s", p$coluna, met, p$nome)
    },
    validar = function(df, p) {
      if (is.null(p$nome) || !nzchar(trimws(p$nome))) return("De um nome a coluna nova.")
      if (!p$coluna %in% names(df)) return(sprintf("A coluna '%s' nao existe neste ponto.", p$coluna))
      x <- suppressWarnings(as.numeric(df[[p$coluna]]))
      if (all(is.na(x))) return("A coluna precisa ser numerica.")
      if (identical(p$metodo, "zscore") && stats::sd(x, na.rm = TRUE) == 0)
        return("Desvio-padrao zero: nao da para calcular o z-score.")
      if (identical(p$metodo, "normalizar") && diff(range(x, na.rm = TRUE)) == 0)
        return("Maximo igual ao minimo: nao da para normalizar 0-1.")
      NULL
    },
    aplicar = function(df, p) {
      x <- suppressWarnings(as.numeric(df[[p$coluna]]))
      df[[p$nome]] <- switch(p$metodo,
        zscore      = (x - mean(x, na.rm = TRUE)) / stats::sd(x, na.rm = TRUE),
        centralizar = x - mean(x, na.rm = TRUE),
        normalizar  = (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
      df
    },
    codigo = function(p) {
      col <- trat_bt(p$coluna); nome <- trat_bt(p$nome)
      expr <- switch(p$metodo,
        zscore      = sprintf("(%s - mean(%s, na.rm = TRUE)) / sd(%s, na.rm = TRUE)", col, col, col),
        centralizar = sprintf("%s - mean(%s, na.rm = TRUE)", col, col),
        normalizar  = sprintf("(%s - min(%s, na.rm = TRUE)) / (max(%s, na.rm = TRUE) - min(%s, na.rm = TRUE))", col, col, col, col))
      sprintf("dados <- dados |> dplyr::mutate(%s = %s)", nome, expr)
    }
  ),

  binning = list(
    rotulo = function(p) {
      met <- c(igual = "amplitude igual", quantil = "por quantis")[p$metodo]
      sprintf("Classes de '%s' (%d, %s) -> %s", p$coluna, as.integer(p$n), met, p$nome)
    },
    validar = function(df, p) {
      if (is.null(p$nome) || !nzchar(trimws(p$nome))) return("De um nome a coluna de classes.")
      if (!p$coluna %in% names(df)) return(sprintf("A coluna '%s' nao existe neste ponto.", p$coluna))
      if (all(is.na(suppressWarnings(as.numeric(df[[p$coluna]]))))) return("A coluna precisa ser numerica.")
      if (is.null(p$n) || is.na(p$n) || p$n < 2) return("Escolha ao menos 2 classes.")
      NULL
    },
    aplicar = function(df, p) {
      x <- suppressWarnings(as.numeric(df[[p$coluna]])); n <- as.integer(p$n)
      df[[p$nome]] <- if (identical(p$metodo, "quantil")) {
        br <- stats::quantile(x, probs = seq(0, 1, length.out = n + 1), na.rm = TRUE)
        cut(x, breaks = unique(br), include.lowest = TRUE)
      } else {
        cut(x, breaks = n, include.lowest = TRUE)
      }
      df
    },
    codigo = function(p) {
      col <- trat_bt(p$coluna); nome <- trat_bt(p$nome); n <- as.integer(p$n)
      if (identical(p$metodo, "quantil"))
        sprintf("dados <- dados |> dplyr::mutate(%s = cut(%s, breaks = quantile(%s, probs = seq(0, 1, length.out = %d), na.rm = TRUE), include.lowest = TRUE))",
                nome, col, col, n + 1L)
      else
        sprintf("dados <- dados |> dplyr::mutate(%s = cut(%s, breaks = %d, include.lowest = TRUE))",
                nome, col, n)
    }
  ),

  remover_duplicatas = list(
    rotulo = function(p) {
      if (is.null(p$colunas) || !length(p$colunas)) "Remover duplicatas (linhas identicas)"
      else sprintf("Remover duplicatas por %s", paste(p$colunas, collapse = ", "))
    },
    validar = function(df, p) {
      falta <- setdiff(p$colunas %||% character(0), names(df))
      if (length(falta)) return(paste("Colunas inexistentes:", paste(falta, collapse = ", ")))
      NULL
    },
    aplicar = function(df, p) {
      if (is.null(p$colunas) || !length(p$colunas)) dplyr::distinct(df)
      else dplyr::distinct(df, dplyr::across(tidyselect::all_of(p$colunas)), .keep_all = TRUE)
    },
    codigo = function(p) {
      if (is.null(p$colunas) || !length(p$colunas)) "dados <- dplyr::distinct(dados)"
      else sprintf("dados <- dplyr::distinct(dados, %s, .keep_all = TRUE)",
                   paste(vapply(p$colunas, trat_bt, character(1)), collapse = ", "))
    }
  ),

  padronizar_texto = list(
    rotulo = function(p) {
      met <- c(squish = "remover espacos extras", minusculas = "minusculas",
               maiusculas = "MAIUSCULAS", titulo = "Iniciais Maiusculas")[p$metodo]
      sprintf("Padronizar texto de '%s' (%s)", p$coluna, met)
    },
    validar = function(df, p) {
      if (!p$coluna %in% names(df)) return(sprintf("A coluna '%s' nao existe neste ponto.", p$coluna))
      NULL
    },
    aplicar = function(df, p) {
      x <- as.character(df[[p$coluna]])
      df[[p$coluna]] <- switch(p$metodo,
        squish     = trimws(gsub("\\s+", " ", x)),
        minusculas = tolower(x),
        maiusculas = toupper(x),
        titulo     = gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", tolower(x), perl = TRUE))
      df
    },
    codigo = function(p) {
      col <- trat_bt(p$coluna)
      fun <- switch(p$metodo,
        squish = "stringr::str_squish", minusculas = "stringr::str_to_lower",
        maiusculas = "stringr::str_to_upper", titulo = "stringr::str_to_title")
      sprintf("dados <- dados |> dplyr::mutate(%s = %s(as.character(%s)))", col, fun, col)
    }
  ),

  # ---- Calcular variavel (expressao livre sobre as colunas) ------------------
  calcular = list(
    rotulo = function(p) sprintf("Calcular %s = %s", p$nome, p$expr),
    validar = function(df, p) {
      if (is.null(p$nome) || !nzchar(trimws(p$nome))) return("De um nome a variavel calculada.")
      if (is.null(p$expr) || !nzchar(trimws(p$expr))) return("A formula esta vazia.")
      val <- tryCatch(eval(parse(text = p$expr), envir = df, enclos = baseenv()), error = function(e) e)
      if (inherits(val, "error")) return(paste0("Formula invalida: ", conditionMessage(val)))
      if (!is.numeric(val) && !is.logical(val)) return("O resultado precisa ser numerico.")
      if (length(val) != nrow(df) && length(val) != 1L) return("O resultado nao tem o tamanho dos dados.")
      NULL
    },
    aplicar = function(df, p) {
      val <- eval(parse(text = p$expr), envir = df, enclos = baseenv())
      if (length(val) == 1L) val <- rep(val, nrow(df))
      df[[p$nome]] <- val
      df
    },
    codigo = function(p) sprintf("dados <- dados |> dplyr::mutate(%s = %s)", trat_bt(p$nome), p$expr)
  ),

  # ---- Reescalar por prefixo SI (dividir por potencia de dez) -----------------
  reescalar = list(
    rotulo = function(p) {
      pf <- if (nzchar(p$simbolo %||% "")) p$simbolo else "base"
      sprintf("Reescalar %s (prefixo %s) -> %s", p$coluna, pf, p$nome)
    },
    validar = function(df, p) {
      if (is.null(p$nome) || !nzchar(trimws(p$nome))) return("De um nome a coluna reescalada.")
      if (!p$coluna %in% names(df)) return(sprintf("A coluna '%s' nao existe neste ponto.", p$coluna))
      if (all(is.na(suppressWarnings(as.numeric(df[[p$coluna]]))))) return("A coluna precisa ser numerica.")
      NULL
    },
    aplicar = function(df, p) {
      df[[p$nome]] <- suppressWarnings(as.numeric(df[[p$coluna]])) / trat_fator_si(p$simbolo)
      df
    },
    codigo = function(p) sprintf("dados <- dados |> dplyr::mutate(%s = %s / %s)",
                                 trat_bt(p$nome), trat_bt(p$coluna), trat_num_txt(trat_fator_si(p$simbolo)))
  ),

  # ---- Agrupar / sumarizar (base derivada reduzida) --------------------------
  agrupar_sumarizar = list(
    rotulo = function(p) {
      sprintf(
        "Agrupar por %s; resumir %s (%s)",
        paste(p$grupos, collapse = ", "),
        if (length(p$variaveis)) paste(p$variaveis, collapse = ", ") else "contagens",
        paste(p$funcoes, collapse = ", ")
      )
    },
    validar = function(df, p) {
      agrupar_validar(
        df,
        p$grupos %||% character(0),
        p$variaveis %||% character(0),
        p$funcoes %||% character(0)
      )
    },
    aplicar = function(df, p) {
      resultado <- agrupar_aplicar(df, p$grupos, p$variaveis, p$funcoes)
      if (isTRUE(p$ordenar) && nrow(resultado))
        resultado <- resultado[do.call(order, resultado[p$grupos]), , drop = FALSE]
      rownames(resultado) <- NULL
      resultado
    },
    codigo = function(p) {
      grupos <- paste(vapply(p$grupos, trat_bt, character(1)), collapse = ", ")
      exprs <- agrupar_exprs_codigo(p$variaveis, p$funcoes)
      linhas <- c(
        sprintf("dados <- dados |> dplyr::group_by(%s) |> dplyr::summarise(", grupos),
        paste0("  ", exprs, ","),
        "  .groups = \"drop\"",
        ")"
      )
      if (isTRUE(p$ordenar))
        linhas <- c(linhas, sprintf(
          "dados <- dados |> dplyr::arrange(%s)",
          paste(vapply(p$grupos, trat_bt, character(1)), collapse = ", ")
        ))
      paste(linhas, collapse = "\n")
    }
  ),

  # ---- Tabela de contingência em formato tidy -------------------------------
  contingencia = list(
    rotulo = function(p) {
      pct <- c(
        none = "somente contagens",
        row = "percentual por linha",
        col = "percentual por coluna",
        total = "percentual do total"
      )[[p$percentual %||% "none"]]
      sprintf("Contingência tidy: %s × %s (%s)", p$linha, p$coluna, pct)
    },
    validar = function(df, p) {
      if (is.null(p$linha) || !p$linha %in% names(df))
        return("Escolha uma variável válida para as linhas.")
      if (is.null(p$coluna) || !p$coluna %in% names(df))
        return("Escolha uma variável válida para as colunas.")
      if (identical(p$linha, p$coluna))
        return("As variáveis de linha e coluna precisam ser diferentes.")
      validos <- df[!is.na(df[[p$linha]]) & !is.na(df[[p$coluna]]), , drop = FALSE]
      if (length(unique(validos[[p$linha]])) < 2L ||
          length(unique(validos[[p$coluna]])) < 2L)
        return("A contingência exige pelo menos dois níveis válidos em cada variável.")
      if (!p$percentual %in% c("none", "row", "col", "total"))
        return("Tipo de percentual inválido.")
      NULL
    },
    aplicar = function(df, p) {
      trat_contingencia_tidy(df, p$linha, p$coluna, p$percentual)
    },
    codigo = trat_contingencia_codigo
  ),

  # ---- Filtrar linhas (por condicao numerica ou por niveis categoricos) ------
  filtrar = list(
    rotulo = function(p) {
      if (identical(p$origem, "numerica"))
        sprintf("Filtrar: manter %s %s %s", p$coluna, p$operador, trat_num_txt(p$valor))
      else
        sprintf("Filtrar: manter %s em {%s}", p$coluna, paste(p$niveis, collapse = ", "))
    },
    validar = function(df, p) {
      if (!p$coluna %in% names(df)) return(sprintf("A coluna '%s' nao existe neste ponto.", p$coluna))
      if (identical(p$origem, "numerica")) {
        if (is.null(p$valor) || is.na(p$valor)) return("Informe o valor de comparacao.")
      } else if (is.null(p$niveis) || !length(p$niveis)) return("Escolha ao menos um nivel para manter.")
      NULL
    },
    aplicar = function(df, p) {
      if (identical(p$origem, "numerica")) {
        x <- suppressWarnings(as.numeric(df[[p$coluna]]))
        keep <- !is.na(x) & trat_filtro_op(x, p$operador, p$valor)
      } else {
        keep <- as.character(df[[p$coluna]]) %in% p$niveis
      }
      df[keep, , drop = FALSE]
    },
    codigo = function(p) {
      col <- trat_bt(p$coluna)
      if (identical(p$origem, "numerica"))
        sprintf("dados <- dados |> dplyr::filter(%s %s %s)", col, p$operador, trat_num_txt(p$valor))
      else
        sprintf("dados <- dados |> dplyr::filter(%s %%in%% c(%s))", col, trat_q(p$niveis))
    }
  )
)

# =============================================================================
# PIPELINE: replay + geracao do script de preparo
# =============================================================================

replay_pipeline <- function(base, pipeline, reg = tratamentos) {
  df <- base
  erros <- list()
  for (i in seq_along(pipeline)) {
    et <- pipeline[[i]]
    if (!isTRUE(et$ativa)) next
    tt <- reg[[et$tipo]]
    if (is.null(tt)) { erros[[as.character(i)]] <- "tipo de tratamento desconhecido"; next }
    msg <- tryCatch(tt$validar(df, et$params), error = function(e) conditionMessage(e))
    if (!is.null(msg)) { erros[[as.character(i)]] <- msg; next }
    df <- tryCatch(tt$aplicar(df, et$params),
                   error = function(e) { erros[[as.character(i)]] <<- conditionMessage(e); df })
  }
  list(df = df, erros = erros)
}

gerar_script_preparo <- function(pipeline, info, reg = tratamentos, base_extra = NULL) {
  if (!is.null(base_extra) && nzchar(base_extra)) {
    linhas <- c(
      "# Script gerado pela CatalyseR - Preparacao dos dados (trilha)",
      "# --- Base preparada externamente (Arrumar) ---",
      base_extra,
      "dados <- dados_arrumados   # a base ja vem arrumada",
      ""
    )
  } else {
    if (!is.null(info) && identical(info$source, "package")) {
      leitura <- c("library(EAPADados)", sprintf("data(%s)", info$package_dataset),
                   sprintf("dados <- %s", info$package_dataset))
      usa_readxl <- FALSE
    } else {
      fn  <- if (!is.null(info)) info$file_name   else "SEU_ARQUIVO.xlsx"
      abn <- if (!is.null(info)) info$excel_sheet else "SUA_ABA"
      leitura <- sprintf('dados <- read_excel("%s", sheet = "%s")', fn, abn)
      usa_readxl <- TRUE
    }
    linhas <- c(
      "# Script gerado pela CatalyseR - Preparacao dos dados (trilha)",
      "library(tidyverse)",
      if (usa_readxl) "library(readxl)" else NULL,
      "",
      "# 1. Dados de entrada (ajuste o caminho/aba se necessario)",
      leitura, ""
    )
  }
  ativos <- Filter(function(et) isTRUE(et$ativa), pipeline)
  if (!length(ativos)) {
    linhas <- c(linhas, "# (nenhuma etapa de preparo ainda)")
  }
  n <- 0L
  for (et in ativos) {
    tt <- reg[[et$tipo]]; if (is.null(tt)) next
    n <- n + 1L
    linhas <- c(linhas,
      sprintf("# Etapa %d: %s", n, tt$rotulo(et$params)),
      tt$codigo(et$params), "")
  }
  linhas <- c(linhas, "print(dados)")
  paste(linhas, collapse = "\n")
}

# =============================================================================
# DESENHO da trilha como SVG (renderiza ao vivo no modulo)
# =============================================================================

trat_xml_esc <- function(s) {
  s <- gsub("&", "&amp;", s, fixed = TRUE)
  s <- gsub("<", "&lt;",  s, fixed = TRUE)
  s <- gsub(">", "&gt;",  s, fixed = TRUE)
  gsub('"', "&quot;", s, fixed = TRUE)
}

trat_encurta <- function(s, n = 42) {
  if (nchar(s) > n) paste0(substr(s, 1, n - 1), "…") else s
}

desenhar_trilha_svg <- function(pipeline, erros = list(), reg = tratamentos, base_label = "dados brutos") {
  W  <- 340; bh <- 40; gap <- 10; x <- 10; bw <- W - 2 * x; ry <- 8
  y0 <- 8
  n  <- length(pipeline)
  total <- n + 1L
  H  <- y0 + total * (bh + gap) + 2

  cx <- function(txt, y, fill, accent, tcol, strike = FALSE, glyph = "") {
    dec <- if (strike) ' text-decoration="line-through"' else ""
    paste0(
      sprintf('<rect x="%d" y="%d" width="%d" height="%d" rx="%d" fill="%s" stroke="%s" stroke-width="1.5"/>',
              x, y, bw, bh, ry, fill, accent),
      sprintf('<rect x="%d" y="%d" width="6" height="%d" rx="3" fill="%s"/>', x, y, bh, accent),
      sprintf('<text x="%d" y="%d" font-family="Calibri, system-ui, sans-serif" font-size="14" font-weight="600" fill="%s"%s>%s%s</text>',
              x + 16, y + 25, tcol, dec, glyph, trat_xml_esc(txt)))
  }

  partes <- character(0)
  partes <- c(partes, cx(base_label, y0, "#0F3B5F", "#0F3B5F", "#FFFFFF"))

  if (n == 0) {
    y <- y0 + (bh + gap)
    partes <- c(partes, sprintf(
      '<rect x="%d" y="%d" width="%d" height="%d" rx="%d" fill="none" stroke="#B9C6CE" stroke-width="1.5" stroke-dasharray="5 4"/><text x="%d" y="%d" font-family="Calibri, system-ui, sans-serif" font-size="13" fill="#9AA7AE" font-style="italic">+ adicione um tratamento acima</text>',
      x, y, bw, bh, ry, x + 16, y + 25))
  } else {
    for (i in seq_len(n)) {
      et <- pipeline[[i]]
      rot <- tryCatch(reg[[et$tipo]]$rotulo(et$params), error = function(e) et$tipo)
      rot <- trat_encurta(sprintf("%d - %s", i, rot))
      y <- y0 + i * (bh + gap)
      err <- erros[[as.character(i)]]
      if (!is.null(err)) {
        partes <- c(partes, cx(rot, y, "#FDEDE8", "#E76F51", "#B23A22", glyph = "! "))
      } else if (!isTRUE(et$ativa)) {
        partes <- c(partes, cx(rot, y, "#F0F0F0", "#BBBBBB", "#8A8A8A", strike = TRUE))
      } else {
        partes <- c(partes, cx(rot, y, "#EAF2F3", "#2E7D8F", "#0F3B5F"))
      }
    }
  }

  paste0(sprintf('<svg viewBox="0 0 %d %d" width="100%%" xmlns="http://www.w3.org/2000/svg" role="img">', W, H),
         paste(partes, collapse = ""), '</svg>')
}

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a
