# =============================================================================
# funcoes_pca.R
# -----------------------------------------------------------------------------
# Funções de apoio para Análise de Componentes Principais (PCA / ACP).
#
# Arquitetura (fonte canônica única):
#   calcular_pca()        -> executa o prcomp, extrai autovalores e cargas.
#   mostrar_pca_var()     -> formata a tabela de variância explicada.
#   mostrar_pca_loadings()-> formata a tabela de cargas dos componentes.
#   relatar_pca()         -> frase-relatório em português sintetizando os resultados.
# =============================================================================

library(tibble)
library(flextable)

# ---- Utilitário: Formato numérico brasileiro ---------------------------------
fmt <- function(x, dig = 2) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return("-")
  formatC(x, format = "f", digits = dig, decimal.mark = ",")
}

#' Executa a PCA, extrai autovalores, variância explicada e cargas dos componentes
calcular_pca <- function(df, vars_selected, scale = TRUE) {
  req(df, length(vars_selected) >= 2)
  
  # Filtrar e escalar dados
  X <- df[, vars_selected, drop = FALSE]
  
  # Executa PCA
  fit <- prcomp(X, center = TRUE, scale. = scale)
  
  # Autovalores (variâncias dos componentes)
  eigenvals <- fit$sdev^2
  var_pct <- (eigenvals / sum(eigenvals)) * 100
  cum_var_pct <- cumsum(var_pct)
  
  # Tabela de variância explicada
  var_df <- data.frame(
    PC = paste0("PC", 1:length(eigenvals)),
    Autovalor = eigenvals,
    Variancia_Pct = var_pct,
    Acumulada_Pct = cum_var_pct,
    stringsAsFactors = FALSE
  )
  
  # Matriz de cargas (rotation)
  loadings_mat <- fit$rotation
  loadings_df <- as.data.frame(loadings_mat)
  loadings_df <- cbind(Variavel = rownames(loadings_mat), loadings_df)
  rownames(loadings_df) <- NULL
  
  list(
    vars = vars_selected,
    scale = scale,
    var_df = var_df,
    loadings_df = loadings_df,
    scores = as.data.frame(fit$x),
    rotation = fit$rotation,
    sdev = fit$sdev,
    N = nrow(X)
  )
}

#' Formata a tabela de variância explicada pelos componentes
mostrar_pca_var <- function(r) {
  tibble::tibble(
    `Componente Principal` = r$var_df$PC,
    `Autovalor (Variância)` = round(r$var_df$Autovalor, 4),
    `Variância Explicada (%)` = round(r$var_df$Variancia_Pct, 2),
    `Variância Acumulada (%)` = round(r$var_df$Acumulada_Pct, 2)
  )
}

#' Formata a tabela de cargas dos componentes (primeiros PCs)
mostrar_pca_loadings <- function(r) {
  # Limita a exibição até as colunas correspondentes aos PCs calculados
  tab <- r$loadings_df
  # Arredondar valores numéricos
  num_cols <- names(tab)[-1]
  tab[num_cols] <- lapply(tab[num_cols], function(x) round(x, 4))
  names(tab)[1] <- "Variável"
  
  tibble::as_tibble(tab)
}

#' Gera o relato estatístico de PCA em português
relatar_pca <- function(r) {
  # Determinar quantos componentes explicam >70% ou 80% da variância
  cum_var <- r$var_df$Acumulada_Pct
  num_pcs_70 <- which(cum_var >= 70)[1]
  if(is.na(num_pcs_70)) num_pcs_70 <- length(cum_var)
  
  # Coletar informações do PC1 e PC2
  var_pc1 <- r$var_df$Variancia_Pct[1]
  var_pc2 <- if(length(cum_var) > 1) r$var_df$Variancia_Pct[2] else 0
  
  # Variável com maior carga absoluta no PC1
  loadings_pc1 <- r$loadings_df[[2]] # PC1 está na col 2
  max_var_pc1 <- r$loadings_df$Variavel[which.max(abs(loadings_pc1))]
  
  paste0(
    "Foi realizada uma Análise de Componentes Principais (PCA) utilizando as variáveis numéricas [", 
    paste(r$vars, collapse = "; "), "] com N = ", r$N, " observações. Os dados foram ", 
    ifelse(r$scale, "padronizados (escala unitária) ", "centrados "), "antes da análise. ",
    "Os resultados indicam que o primeiro Componente Principal (PC1) explica ", fmt(var_pc1), "% da variância total, ",
    "sendo a variável '", max_var_pc1, "' a de maior contribuição linear neste eixo. ",
    ifelse(length(cum_var) > 1, 
           paste0("O segundo Componente Principal (PC2) responde por mais ", fmt(var_pc2), "% da variabilidade. "), ""),
    "Conjuntamente, os primeiros ", num_pcs_70, " componentes explicam ", fmt(cum_var[num_pcs_70]), 
    "% de toda a variância dos dados, fornecendo uma excelente redução de dimensionalidade do conjunto original."
  )
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
