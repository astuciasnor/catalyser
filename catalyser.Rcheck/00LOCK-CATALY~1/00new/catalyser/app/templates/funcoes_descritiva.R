# =============================================================================
# funcoes_descritiva.R
# -----------------------------------------------------------------------------
# Funções de apoio para análise descritiva e relato de dados.
#
# Conteúdo:
#   mostrar_descr()   - tabela estruturada de estatísticas resumo (tibble)
#   relatar_descr()   - descrição narrativa em português das estatísticas resumo
#   flextable_ocean() - formata a tabela na identidade Ocean Gradient (docx)
# =============================================================================

library(tibble)
library(flextable)

# ---- TABELA estruturada de estatísticas resumo ------------------------------
mostrar_descr <- function(dados, vars, grupo = "none") {
  if (is.null(grupo) || grupo == "none") {
    res <- do.call(rbind, lapply(vars, function(v) {
      x <- dados[[v]]
      tibble::tibble(
        `Variável`      = v,
        `N`             = sum(!is.na(x)),
        `NAs`           = sum(is.na(x)),
        `Média`         = round(mean(x, na.rm = TRUE), 2),
        `Mediana`       = round(median(x, na.rm = TRUE), 2),
        `Desvio Padrão` = round(sd(x, na.rm = TRUE), 2),
        `Variância`     = round(var(x, na.rm = TRUE), 2),
        `Mínimo`        = round(min(x, na.rm = TRUE), 2),
        `Máximo`        = round(max(x, na.rm = TRUE), 2),
        `Q25`           = round(quantile(x, 0.25, na.rm = TRUE), 2),
        `Q75`           = round(quantile(x, 0.75, na.rm = TRUE), 2)
      )
    }))
  } else {
    g_factor <- as.factor(dados[[grupo]])
    levels_g <- levels(g_factor)
    
    res <- do.call(rbind, lapply(vars, function(v) {
      x <- dados[[v]]
      do.call(rbind, lapply(levels_g, function(lvl) {
        x_g <- x[g_factor == lvl]
        tibble::tibble(
          `Variável`      = v,
          `Grupo`         = lvl,
          `N`             = sum(!is.na(x_g)),
          `NAs`           = sum(is.na(x_g)),
          `Média`         = round(mean(x_g, na.rm = TRUE), 2),
          `Mediana`       = round(median(x_g, na.rm = TRUE), 2),
          `Desvio Padrão` = round(sd(x_g, na.rm = TRUE), 2),
          `Variância`     = round(var(x_g, na.rm = TRUE), 2),
          `Mínimo`        = round(min(x_g, na.rm = TRUE), 2),
          `Máximo`        = round(max(x_g, na.rm = TRUE), 2),
          `Q25`           = round(quantile(x_g, 0.25, na.rm = TRUE), 2),
          `Q75`           = round(quantile(x_g, 0.75, na.rm = TRUE), 2)
        )
      }))
    }))
  }
  return(res)
}

# ---- RELATÓRIO NARRATIVO em português ---------------------------------------
relatar_descr <- function(dados, vars, grupo = "none") {
  res_text <- c()
  if (is.null(grupo) || grupo == "none") {
    res_text <- c(res_text, "A análise descritiva global das variáveis selecionadas revelou os seguintes padrões:")
    for (v in vars) {
      x <- dados[[v]]
      n <- sum(!is.na(x))
      m <- mean(x, na.rm = TRUE)
      s <- sd(x, na.rm = TRUE)
      med <- median(x, na.rm = TRUE)
      min_v <- min(x, na.rm = TRUE)
      max_v <- max(x, na.rm = TRUE)
      
      res_text <- c(res_text, sprintf(
        "A variável **%s** (N = %d) apresentou uma média amostral de %s (desvio padrão = %s) e mediana de %s, com valores variando entre um mínimo de %s e um máximo de %s.",
        v, n, formatC(m, format = "f", digits = 2, decimal.mark = ","),
        formatC(s, format = "f", digits = 2, decimal.mark = ","),
        formatC(med, format = "f", digits = 2, decimal.mark = ","),
        formatC(min_v, format = "f", digits = 2, decimal.mark = ","),
        formatC(max_v, format = "f", digits = 2, decimal.mark = ",")
      ))
    }
  } else {
    res_text <- c(res_text, sprintf("A análise descritiva agrupada pela variável de agrupamento **%s** revelou os seguintes resultados:", grupo))
    g_factor <- as.factor(dados[[grupo]])
    levels_g <- levels(g_factor)
    for (v in vars) {
      x <- dados[[v]]
      res_text <- c(res_text, sprintf("Para a variável **%s**:", v))
      for (lvl in levels_g) {
        x_g <- x[g_factor == lvl]
        n <- sum(!is.na(x_g))
        m <- mean(x_g, na.rm = TRUE)
        s <- sd(x_g, na.rm = TRUE)
        med <- median(x_g, na.rm = TRUE)
        
        res_text <- c(res_text, sprintf(
          "- No grupo **%s** (N = %d), a média foi de %s (desvio padrão = %s) e a mediana foi de %s.",
          lvl, n, formatC(m, format = "f", digits = 2, decimal.mark = ","),
          formatC(s, format = "f", digits = 2, decimal.mark = ","),
          formatC(med, format = "f", digits = 2, decimal.mark = ",")
        ))
      }
    }
  }
  paste(res_text, collapse = "\n\n")
}

# ---- Formatação da tabela (identidade Ocean Gradient, saída docx) -----------
flextable_ocean <- function(tab) {
  flextable::flextable(tab) |>
    flextable::theme_booktabs() |>
    flextable::bg(part = "header", bg = "#0F3B5F") |>
    flextable::color(part = "header", color = "white") |>
    flextable::bold(part = "header") |>
    flextable::font(fontname = "Times New Roman", part = "all") |>
    flextable::fontsize(size = 9, part = "all") |>
    flextable::align(align = "center", part = "all") |>
    flextable::align(j = 1, align = "left", part = "all") |>
    flextable::padding(padding = 4, part = "all") |>
    flextable::autofit()
}
