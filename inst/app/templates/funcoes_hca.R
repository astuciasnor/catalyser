# =============================================================================
# funcoes_hca.R
# -----------------------------------------------------------------------------
# Funções de apoio para Análise de Agrupamento Hierárquico (AAH / HCA).
#
# Arquitetura (fonte canônica única):
#   calcular_hca()         -> executa dist(), hclust() e cutree()
#   mostrar_hca_perfil()   -> calcula o perfil de médias por cluster
#   mostrar_hca_resumo()   -> tabela flextable formatada de perfil
#   relatar_hca()          -> relatório verbal em português sintetizando os resultados
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

#' Executa a AAH, calcula distâncias, ligação e agrupamento
calcular_hca <- function(df, vars_selected, distance_method = "euclidean", linkage_method = "ward.D2", k_groups = 3, scale = TRUE) {
  req(df, length(vars_selected) >= 2)
  
  # Filtrar e limpar observações com NAs nas variáveis selecionadas
  X <- df[, vars_selected, drop = FALSE]
  complete_idx <- complete.cases(X)
  X_clean <- X[complete_idx, , drop = FALSE]
  df_clean <- df[complete_idx, , drop = FALSE]
  
  # Padronizar se necessário
  X_scaled <- X_clean
  if (scale) {
    X_scaled <- scale(X_clean)
  }
  
  # Matriz de distâncias
  d_mat <- dist(X_scaled, method = distance_method)
  
  # Ligação hierárquica
  fit <- hclust(d_mat, method = linkage_method)
  
  # Corte da árvore (clusters)
  clusters <- cutree(fit, k = k_groups)
  
  # Adiciona cluster aos dados originais (apenas os completos)
  df_result <- df_clean
  df_result$Cluster <- paste0("Grupo ", clusters)
  
  # Calcular perfil de médias
  profile_list <- list()
  for (v in vars_selected) {
    means <- tapply(df_result[[v]], df_result$Cluster, mean, na.rm = TRUE)
    profile_list[[v]] <- means
  }
  
  # Contagem por cluster
  counts <- as.vector(table(df_result$Cluster))
  names(counts) <- names(profile_list[[1]])
  
  profile_df <- data.frame(
    Cluster = names(counts),
    N = counts,
    stringsAsFactors = FALSE
  )
  for (v in vars_selected) {
    profile_df[[v]] <- as.numeric(profile_list[[v]])
  }
  
  list(
    vars = vars_selected,
    distance_method = distance_method,
    linkage_method = linkage_method,
    k_groups = k_groups,
    scale = scale,
    fit = fit,
    clusters = clusters,
    df_clustered = df_result,
    profile_df = profile_df,
    N = nrow(X_clean),
    N_total = nrow(df)
  )
}

#' Formata a tabela de perfil de médias dos clusters
mostrar_hca_perfil <- function(r) {
  tab <- r$profile_df
  # Arredondar médias para 3 casas decimais
  num_cols <- names(tab)[-c(1, 2)] # Pula Cluster e N
  tab[num_cols] <- lapply(tab[num_cols], function(x) round(x, 3))
  tibble::as_tibble(tab)
}

#' Formata a tabela de pertinência de grupo de cada observação (primeiras linhas)
mostrar_hca_pertinencia <- function(r) {
  tab <- r$df_clustered
  res <- data.frame(
    Observacao = 1:nrow(tab),
    Cluster = tab$Cluster
  )
  # Se o dataset original tiver alguma coluna identificadora (ex: primeiro character ou ID)
  char_cols <- names(tab)[sapply(tab, function(x) is.character(x) || is.factor(x))]
  char_cols <- setdiff(char_cols, "Cluster")
  if (length(char_cols) > 0) {
    res$Identificador <- as.character(tab[[char_cols[1]]])
    res <- res[, c("Observacao", "Identificador", "Cluster")]
  }
  tibble::as_tibble(res)
}

#' Gera o relato estatístico de AAH em português
relatar_hca <- function(r) {
  dist_label <- switch(r$distance_method,
                       "euclidean" = "Euclidiana",
                       "manhattan" = "Manhattan",
                       r$distance_method)
                       
  link_label <- switch(r$linkage_method,
                       "ward.D2" = "Ward (critério de mínima variância)",
                       "complete" = "Ligação Completa (Complete Linkage)",
                       "single" = "Ligação Simples (Single Linkage)",
                       "average" = "Ligação Média (UPGMA)",
                       r$linkage_method)
  
  # Encontrar o maior e o menor cluster
  counts <- r$profile_df$N
  names(counts) <- r$profile_df$Cluster
  max_cluster <- names(which.max(counts))
  min_cluster <- names(which.min(counts))
  
  paste0(
    "Foi realizada uma Análise de Agrupamento Hierárquico (AAH) utilizando ", r$N, " observações completas (de um total de ", r$N_total, "). ",
    "A matriz de distância foi calculada por meio da distância ", dist_label, 
    ifelse(r$scale, " aplicada aos dados previamente padronizados. ", " aplicada aos dados brutos. "),
    "O método de aglomeração utilizado foi o algoritmo de ", link_label, ". ",
    "A árvore de decisão (dendrograma) foi particionada em k = ", r$k_groups, " grupos (clusters). ",
    "O maior grupo formado foi o '", max_cluster, "' com ", max(counts), " observações (", round(max(counts)/sum(counts)*100, 1), "%), ",
    "enquanto o menor grupo foi o '", min_cluster, "' contendo ", min(counts), " observações. ",
    "O perfil de médias de cada grupo revela a diferenciação multivariada das classes formadas e pode ser verificado na tabela de perfil."
  )
}

# ---- Formatação de tabela flextable (identidade Ocean Gradient, saída docx) ---
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
