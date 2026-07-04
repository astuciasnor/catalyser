# =============================================================================
# funcoes_contingency.R
# -----------------------------------------------------------------------------
# Funções de apoio para Tabelas de Contingência e Testes de Associação (Chi-Quadrado).
#
# Arquitetura (fonte canônica única):
#   calcular_contingencia() -> calcula frequências cruzadas, percentuais e qui-quadrado
#   relatar_contingencia()  -> gera relatório verbal em português
#   flextable_ocean()       -> formata tabela com a paleta Ocean Gradient para Word
# =============================================================================

library(tibble)
library(flextable)

# Evitar erros de 'req' não encontrado fora do Shiny
if (!exists("req", mode = "function")) {
  req <- function(...) {
    invisible(TRUE)
  }
}

# ---- Utilitário: Formato numérico brasileiro ---------------------------------
fmt <- function(x, dig = 2) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return("-")
  formatC(x, format = "f", digits = dig, decimal.mark = ",")
}

#' Calcula a tabela de contingência, percentuais e teste de Qui-Quadrado
calcular_contingencia <- function(df, var_row, var_col, pct_type = "none") {
  req(df, var_row, var_col)
  
  # Remover NAs das duas variáveis
  df_clean <- df[!is.na(df[[var_row]]) & !is.na(df[[var_col]]), ]
  N <- nrow(df_clean)
  
  # Converter para factor se não for
  row_factor <- as.factor(df_clean[[var_row]])
  col_factor <- as.factor(df_clean[[var_col]])
  
  row_levels <- levels(row_factor)
  col_levels <- levels(col_factor)
  
  # Tabela de frequências absolutas
  tab <- table(row_factor, col_factor)
  
  # Totais marginais
  row_sums <- rowSums(tab)
  col_sums <- colSums(tab)
  tot_sum <- sum(tab)
  
  # Teste do Qui-Quadrado de Independência
  # Tratar caso de tabelas muito pequenas ou zeros para evitar avisos ou erros catastróficos
  chi_test <- tryCatch({
    chisq.test(tab)
  }, error = function(e) {
    list(statistic = NA, parameter = NA, p.value = NA, method = "Teste Qui-Quadrado")
  })
  
  # Criar a tabela formatada com contagens e percentuais
  formatted_grid <- matrix("", nrow = length(row_levels) + 1, ncol = length(col_levels) + 2)
  colnames(formatted_grid) <- c(var_row, col_levels, "Total")
  
  for (i in 1:length(row_levels)) {
    formatted_grid[i, 1] <- row_levels[i]
    r_sum <- row_sums[i]
    
    for (j in 1:length(col_levels)) {
      cell_val <- tab[i, j]
      c_sum <- col_sums[j]
      
      # Calcular percentual
      pct_str <- ""
      if (pct_type == "row" && r_sum > 0) {
        pct_str <- sprintf(" (%s%%)", fmt(cell_val / r_sum * 100, 1))
      } else if (pct_type == "col" && c_sum > 0) {
        pct_str <- sprintf(" (%s%%)", fmt(cell_val / c_sum * 100, 1))
      } else if (pct_type == "total" && tot_sum > 0) {
        pct_str <- sprintf(" (%s%%)", fmt(cell_val / tot_sum * 100, 1))
      }
      
      formatted_grid[i, j + 1] <- paste0(cell_val, pct_str)
    }
    
    # Adicionar total da linha
    row_pct_str <- ""
    if (pct_type == "row") row_pct_str <- " (100,0%)"
    else if (pct_type == "total") row_pct_str <- sprintf(" (%s%%)", fmt(r_sum / tot_sum * 100, 1))
    
    formatted_grid[i, length(col_levels) + 2] <- paste0(r_sum, row_pct_str)
  }
  
  # Adicionar a linha de Totais (marginal das colunas)
  formatted_grid[length(row_levels) + 1, 1] <- "Total"
  for (j in 1:length(col_levels)) {
    c_sum <- col_sums[j]
    col_pct_str <- ""
    if (pct_type == "col") col_pct_str <- " (100,0%)"
    else if (pct_type == "total") col_pct_str <- sprintf(" (%s%%)", fmt(c_sum / tot_sum * 100, 1))
    
    formatted_grid[length(row_levels) + 1, j + 1] <- paste0(c_sum, col_pct_str)
  }
  
  # Canto inferior direito: Total Geral
  tot_pct_str <- ""
  if (pct_type == "row" || pct_type == "col" || pct_type == "total") tot_pct_str <- " (100,0%)"
  formatted_grid[length(row_levels) + 1, length(col_levels) + 2] <- paste0(tot_sum, tot_pct_str)
  
  formatted_df <- as.data.frame(formatted_grid)
  
  list(
    var_row = var_row,
    var_col = var_col,
    pct_type = pct_type,
    tab = tab,
    chi_statistic = chi_test$statistic,
    chi_df = chi_test$parameter,
    chi_p = chi_test$p.value,
    formatted_df = formatted_df,
    N = N
  )
}

#' Formata a tabela para exibição na IDE
mostrar_contingencia <- function(r) {
  tibble::as_tibble(r$formatted_df)
}

#' Gera o relato estatístico do Qui-Quadrado em português
relatar_contingencia <- function(r) {
  if (is.na(r$chi_p)) {
    return("Não foi possível calcular o teste do Qui-Quadrado de associação devido à insuficiência de dados nas classes cruzadas.")
  }
  
  p_str <- if (r$chi_p < 0.001) "p < 0,001" else paste0("p = ", fmt(r$chi_p, 4))
  sig_str <- if (r$chi_p < 0.05) "revela uma associação estatisticamente significativa" else "não revela associação estatisticamente significativa (independência)"
  
  paste0(
    "Foi analisada a tabela de contingência cruzada entre as variáveis categóricas '", r$var_row, "' (linhas) e '", r$var_col, "' (colunas) ",
    "para um total de N = ", r$N, " observações válidas. O teste de Qui-Quadrado de Independência de Pearson ",
    sig_str, " entre as duas variáveis [X² = ", fmt(r$chi_statistic, 2), "; df = ", r$chi_df, "; ", p_str, "]. ",
    ifelse(r$chi_p < 0.05, 
           "Isso indica que a distribuição das frequências de uma variável varia de acordo com as categorias da outra.", 
           "Isso sugere que a distribuição observada nas linhas é independente das categorias representadas nas colunas.")
  )
}

# ---- Formatação da tabela (identidade Ocean Gradient, saída docx) -----------
flextable_ocean <- function(tab) {
  # Destacar a última linha e coluna de Total
  nc <- ncol(tab)
  nr <- nrow(tab)
  
  ft <- flextable::flextable(tab) |>
    flextable::theme_booktabs() |>
    flextable::bg(part = "header", bg = "#0F3B5F") |>
    flextable::color(part = "header", color = "white") |>
    flextable::bold(part = "header") |>
    flextable::font(fontname = "Times New Roman", part = "all") |>
    flextable::fontsize(size = 9, part = "all") |>
    flextable::align(align = "center", part = "all") |>
    flextable::align(j = 1, align = "left", part = "all") |>
    flextable::padding(padding = 5, part = "all")
  
  # Destacar totais marginais
  ft <- ft |>
    flextable::bold(i = nr, part = "body") |>
    flextable::bold(j = nc, part = "body") |>
    flextable::bg(i = nr, bg = "#f8f9fa", part = "body") |>
    flextable::bg(j = nc, bg = "#f8f9fa", part = "body") |>
    flextable::autofit()
    
  ft
}
