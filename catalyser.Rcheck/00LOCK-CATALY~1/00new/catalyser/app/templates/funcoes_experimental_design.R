# Lógica de Casualização e Visualização de Delineamentos Experimentais - EAPA
library(ggplot2)
library(flextable)

# ---- Helper: Encurtar Rótulo para Grade --------------------------------------
make_cell_label <- function(treatment, rep_val = NULL) {
  clean_t <- as.character(treatment)
  if (nchar(clean_t) > 3) {
    clean_t <- substr(clean_t, 1, 3)
  }
  if (!is.null(rep_val)) {
    paste0(clean_t, rep_val)
  } else {
    clean_t
  }
}

# =============================================================================
# 1. DELINEAMENTO INTEIRAMENTE CASUALIZADO (DIC)
# =============================================================================
gerar_delineamento_dic <- function(factor_name, levels_vec, reps, nrows, ncols, seed, response_var = "Resposta") {
  set.seed(seed)
  k <- length(levels_vec)
  total_units <- k * reps
  
  if (nrows * ncols < total_units) {
    return(list(
      error = TRUE,
      message = sprintf("A grade de %d x %d (%d células) é menor que o número total de unidades experimentais (%d). Aumente o número de linhas ou colunas.", nrows, ncols, nrows * ncols, total_units)
    ))
  }
  
  # Criar tratamentos e repetições
  treats <- rep(levels_vec, each = reps)
  rep_indices <- rep(1:reps, times = k)
  
  # Embaralhar
  ord <- sample(total_units)
  shuffled_treats <- treats[ord]
  shuffled_reps <- rep_indices[ord]
  
  # Mapeamento na grade
  grid_positions <- expand.grid(Coluna = 1:ncols, Linha = 1:nrows)
  grid_positions <- grid_positions[1:total_units, ]
  
  df <- data.frame(
    UE = 1:total_units,
    Linha = grid_positions$Linha,
    Coluna = grid_positions$Coluna,
    Tratamento = shuffled_treats,
    Repeticao = shuffled_reps,
    Rotulo = mapply(make_cell_label, shuffled_treats, shuffled_reps),
    stringsAsFactors = FALSE
  )
  
  # Coluna da variável de resposta (vazia)
  df[[response_var]] <- NA_real_
  
  # Paleta de cores vibrantes/pastéis
  palette <- c("#FF6B6B", "#4D96FF", "#6BCB77", "#FFE66D", "#B983FF", "#FF9F29", "#35858B", "#FF8AAE", "#95CD41", "#8758FF", "#54BAB9", "#FF9F43")
  level_colors <- palette[1:min(k, length(palette))]
  if (k > length(palette)) {
    level_colors <- c(level_colors, rainbow(k - length(palette)))
  }
  names(level_colors) <- levels_vec
  df$Cor <- level_colors[df$Tratamento]
  
  return(list(
    error = FALSE,
    df = df,
    level_colors = level_colors,
    factor_name = factor_name,
    levels_vec = levels_vec,
    reps = reps,
    nrows = nrows,
    ncols = ncols,
    seed = seed,
    response_var = response_var
  ))
}

# =============================================================================
# 2. DELINEAMENTO EM BLOCOS CASUALIZADOS (DBC)
# =============================================================================
gerar_delineamento_dbc <- function(factor_name, levels_vec, blocks, nrows, ncols, seed, response_var = "Resposta") {
  set.seed(seed)
  k <- length(levels_vec)
  total_units <- k * blocks
  
  if (nrows * ncols < total_units) {
    return(list(
      error = TRUE,
      message = sprintf("A grade de %d x %d (%d células) é menor que o número total de unidades experimentais (%d). Aumente o número de linhas ou colunas.", nrows, ncols, nrows * ncols, total_units)
    ))
  }
  
  # Blocos independentes
  df_list <- list()
  for (b in 1:blocks) {
    shuffled_levels <- sample(levels_vec)
    df_list[[b]] <- data.frame(
      Bloco = b,
      Tratamento = shuffled_levels,
      Repeticao = b,
      stringsAsFactors = FALSE
    )
  }
  df <- do.call(rbind, df_list)
  df$UE <- 1:total_units
  
  # Posicionamento na grade
  if (nrows == blocks && ncols == k) {
    df$Linha <- df$Bloco
    df$Coluna <- ave(df$Bloco, df$Bloco, FUN = seq_along)
  } else {
    grid_positions <- expand.grid(Coluna = 1:ncols, Linha = 1:nrows)
    grid_positions <- grid_positions[1:total_units, ]
    df$Linha <- grid_positions$Linha
    df$Coluna <- grid_positions$Coluna
  }
  
  # Rótulo com indicação do bloco
  df$Rotulo <- mapply(make_cell_label, df$Tratamento, df$Bloco)
  
  # Coluna da variável de resposta
  df[[response_var]] <- NA_real_
  
  # Paleta de cores
  palette <- c("#FF6B6B", "#4D96FF", "#6BCB77", "#FFE66D", "#B983FF", "#FF9F29", "#35858B", "#FF8AAE", "#95CD41", "#8758FF", "#54BAB9", "#FF9F43")
  level_colors <- palette[1:min(k, length(palette))]
  if (k > length(palette)) {
    level_colors <- c(level_colors, rainbow(k - length(palette)))
  }
  names(level_colors) <- levels_vec
  df$Cor <- level_colors[df$Tratamento]
  
  return(list(
    error = FALSE,
    df = df,
    level_colors = level_colors,
    factor_name = factor_name,
    levels_vec = levels_vec,
    blocks = blocks,
    nrows = nrows,
    ncols = ncols,
    seed = seed,
    response_var = response_var
  ))
}

# =============================================================================
# 3. DELINEAMENTO EM QUADRADO LATINO (DQL)
# =============================================================================
gerar_delineamento_dql <- function(factor_name, levels_vec, seed, response_var = "Resposta") {
  set.seed(seed)
  k <- length(levels_vec)
  total_units <- k * k
  
  # Quadrado Latino padrão
  sq <- matrix(0, nrow = k, ncol = k)
  for (i in 1:k) {
    for (j in 1:k) {
      sq[i, j] <- ((i + j - 2) %% k) + 1
    }
  }
  
  # Permutações aleatórias
  row_perm <- sample(k)
  sq <- sq[row_perm, , drop = FALSE]
  
  col_perm <- sample(k)
  sq <- sq[, col_perm, drop = FALSE]
  
  treat_perm <- sample(levels_vec)
  
  df_list <- list()
  idx <- 1
  for (i in 1:k) {
    for (j in 1:k) {
      treat_idx <- sq[i, j]
      df_list[[idx]] <- data.frame(
        UE = idx,
        Linha = i,
        Coluna = j,
        Tratamento = treat_perm[treat_idx],
        Repeticao = 1, # Não numerado em DQL
        stringsAsFactors = FALSE
      )
      idx <- idx + 1
    }
  }
  df <- do.call(rbind, df_list)
  
  df$Rotulo <- sapply(df$Tratamento, make_cell_label)
  df[[response_var]] <- NA_real_
  
  palette <- c("#FF6B6B", "#4D96FF", "#6BCB77", "#FFE66D", "#B983FF", "#FF9F29", "#35858B", "#FF8AAE", "#95CD41", "#8758FF", "#54BAB9", "#FF9F43")
  level_colors <- palette[1:min(k, length(palette))]
  if (k > length(palette)) {
    level_colors <- c(level_colors, rainbow(k - length(palette)))
  }
  names(level_colors) <- levels_vec
  df$Cor <- level_colors[df$Tratamento]
  
  return(list(
    error = FALSE,
    df = df,
    level_colors = level_colors,
    factor_name = factor_name,
    levels_vec = levels_vec,
    nrows = k,
    ncols = k,
    seed = seed,
    response_var = response_var
  ))
}

# =============================================================================
# 4. DELINEAMENTO FATORIAL
# =============================================================================
gerar_delineamento_fatorial <- function(fator_a_name, fator_a_levels, fator_b_name, fator_b_levels, reps, nrows, ncols, seed, response_var = "Resposta") {
  set.seed(seed)
  comb <- expand.grid(B = fator_b_levels, A = fator_a_levels)
  treats_comb <- paste0(comb$A, "-", comb$B)
  k <- length(treats_comb)
  total_units <- k * reps
  
  if (nrows * ncols < total_units) {
    return(list(
      error = TRUE,
      message = sprintf("A grade de %d x %d (%d células) é menor que o número total de unidades experimentais (%d). Aumente o número de linhas ou colunas.", nrows, ncols, nrows * ncols, total_units)
    ))
  }
  
  treats <- rep(treats_comb, each = reps)
  rep_indices <- rep(1:reps, times = k)
  
  ord <- sample(total_units)
  shuffled_treats <- treats[ord]
  shuffled_reps <- rep_indices[ord]
  
  grid_positions <- expand.grid(Coluna = 1:ncols, Linha = 1:nrows)
  grid_positions <- grid_positions[1:total_units, ]
  
  df <- data.frame(
    UE = 1:total_units,
    Linha = grid_positions$Linha,
    Coluna = grid_positions$Coluna,
    Tratamento = shuffled_treats,
    Repeticao = shuffled_reps,
    stringsAsFactors = FALSE
  )
  
  make_fatorial_label <- function(treat, r_val) {
    parts <- strsplit(treat, "-")[[1]]
    p1 <- parts[1]
    p2 <- parts[2]
    if (nchar(p1) > 2) p1 <- substr(p1, 1, 2)
    if (nchar(p2) > 2) p2 <- substr(p2, 1, 2)
    paste0(p1, p2, r_val)
  }
  df$Rotulo <- mapply(make_fatorial_label, df$Tratamento, df$Repeticao)
  df[[response_var]] <- NA_real_
  
  palette <- c("#FF6B6B", "#4D96FF", "#6BCB77", "#FFE66D", "#B983FF", "#FF9F29", "#35858B", "#FF8AAE", "#95CD41", "#8758FF", "#54BAB9", "#FF9F43")
  level_colors <- palette[1:min(k, length(palette))]
  if (k > length(palette)) {
    level_colors <- c(level_colors, rainbow(k - length(palette)))
  }
  names(level_colors) <- treats_comb
  df$Cor <- level_colors[df$Tratamento]
  
  return(list(
    error = FALSE,
    df = df,
    level_colors = level_colors,
    fator_a_name = fator_a_name,
    fator_a_levels = fator_a_levels,
    fator_b_name = fator_b_name,
    fator_b_levels = fator_b_levels,
    reps = reps,
    nrows = nrows,
    ncols = ncols,
    seed = seed,
    response_var = response_var
  ))
}

# =============================================================================
# 5. DELINEAMENTO EM PARCELAS SUBDIVIDIDAS (SPLIT-PLOT)
# =============================================================================
gerar_delineamento_split_plot <- function(fator_main_name, fator_main_levels, fator_sub_name, fator_sub_levels, blocks, nrows, ncols, seed, response_var = "Resposta") {
  set.seed(seed)
  a <- length(fator_main_levels)
  s <- length(fator_sub_levels)
  total_units <- blocks * a * s
  
  df_list <- list()
  idx <- 1
  for (b in 1:blocks) {
    # Randomiza parcelas principais
    shuffled_main <- sample(fator_main_levels)
    for (m in 1:a) {
      main_level <- shuffled_main[m]
      # Randomiza subparcelas dentro da parcela
      shuffled_sub <- sample(fator_sub_levels)
      for (sub in 1:s) {
        sub_level <- shuffled_sub[sub]
        df_list[[idx]] <- data.frame(
          UE = idx,
          Bloco = b,
          Fator_Main = main_level,
          Fator_Sub = sub_level,
          Tratamento = paste0(main_level, "-", sub_level),
          stringsAsFactors = FALSE
        )
        idx <- idx + 1
      }
    }
  }
  df <- do.call(rbind, df_list)
  
  if (nrows == blocks && ncols == a * s) {
    df$Linha <- df$Bloco
    df$Coluna <- ave(df$Bloco, df$Bloco, FUN = seq_along)
  } else {
    grid_positions <- expand.grid(Coluna = 1:ncols, Linha = 1:nrows)
    grid_positions <- grid_positions[1:total_units, ]
    df$Linha <- grid_positions$Linha
    df$Coluna <- grid_positions$Coluna
  }
  
  make_split_label <- function(main, sub, block) {
    m_lbl <- as.character(main)
    if (nchar(m_lbl) > 2) m_lbl <- substr(m_lbl, 1, 2)
    s_lbl <- as.character(sub)
    if (nchar(s_lbl) > 2) s_lbl <- substr(s_lbl, 1, 2)
    paste0(m_lbl, s_lbl, block)
  }
  df$Rotulo <- mapply(make_split_label, df$Fator_Main, df$Fator_Sub, df$Bloco)
  df[[response_var]] <- NA_real_
  
  comb_levels <- unique(df$Tratamento)
  k <- length(comb_levels)
  palette <- c("#FF6B6B", "#4D96FF", "#6BCB77", "#FFE66D", "#B983FF", "#FF9F29", "#35858B", "#FF8AAE", "#95CD41", "#8758FF", "#54BAB9", "#FF9F43")
  level_colors <- palette[1:min(k, length(palette))]
  if (k > length(palette)) {
    level_colors <- c(level_colors, rainbow(k - length(palette)))
  }
  names(level_colors) <- comb_levels
  df$Cor <- level_colors[df$Tratamento]
  
  return(list(
    error = FALSE,
    df = df,
    level_colors = level_colors,
    fator_main_name = fator_main_name,
    fator_main_levels = fator_main_levels,
    fator_sub_name = fator_sub_name,
    fator_sub_levels = fator_sub_levels,
    blocks = blocks,
    nrows = nrows,
    ncols = ncols,
    seed = seed,
    response_var = response_var
  ))
}

# =============================================================================
# 6. FUNÇÃO DE PLOTAGEM DO CROQUI (ggplot2)
# =============================================================================
plotar_croqui <- function(delineamento_res, design_type) {
  df <- delineamento_res$df
  nrows <- delineamento_res$nrows
  ncols <- delineamento_res$ncols
  level_colors <- delineamento_res$level_colors
  
  # Base do plot
  p <- ggplot(df) +
    # Grade de células
    geom_rect(aes(xmin = Coluna - 0.5, xmax = Coluna + 0.5, 
                  ymin = Linha - 0.5, ymax = Linha + 0.5), 
              fill = "#FAFAFA", color = "#C0C0C0", linewidth = 0.5) +
    # Unidade Experimental (Círculo colorido)
    geom_point(aes(x = Coluna, y = Linha, fill = Tratamento), 
               shape = 21, size = 16, color = "#222222", stroke = 1) +
    # Rótulo interno (ex: A1, B2)
    geom_text(aes(x = Coluna, y = Linha, label = Rotulo), 
              fontface = "bold", size = 4.2, color = "black")
  
  # Customizar os eixos
  y_breaks <- 1:nrows
  y_labels <- as.character(y_breaks)
  x_breaks <- 1:ncols
  x_labels <- as.character(x_breaks)
  
  if (design_type == "DBC" && nrows == delineamento_res$blocks) {
    y_labels <- paste("Bloco", y_breaks)
  } else if (design_type == "split_plot" && nrows == delineamento_res$blocks) {
    y_labels <- paste("Bloco", y_breaks)
  } else if (design_type == "DQL") {
    y_labels <- paste("Linha", y_breaks)
    x_labels <- paste("Coluna", x_breaks)
  }
  
  p <- p +
    scale_y_reverse(breaks = y_breaks, labels = y_labels) +
    scale_x_continuous(breaks = x_breaks, labels = x_labels) +
    scale_fill_manual(values = level_colors) +
    labs(
      title = "Esquema de Distribuição (Área Experimental)",
      x = ifelse(design_type == "DQL", "Colunas de Controle", "Colunas da Área"),
      y = ifelse(design_type %in% c("DBC", "split_plot") && nrows == delineamento_res$blocks, "Blocos (Gradiente)", ifelse(design_type == "DQL", "Linhas de Controle", "Linhas da Área")),
      fill = "Tratamento"
    ) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(face = "bold", size = 13, color = "#0F3B5F", hjust = 0.5, margin = margin(b = 10)),
      axis.title = element_text(face = "bold", size = 10, color = "#0F3B5F"),
      axis.text = element_text(face = "bold", size = 9, color = "#555555"),
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 9, color = "#0F3B5F"),
      legend.text = element_text(size = 8),
      plot.margin = margin(12, 12, 12, 12)
    )
  
  return(p)
}

# =============================================================================
# 7. FUNÇÃO DE RELATO TEXTUAL (Quarto / Word)
# =============================================================================
relatar_delineamento <- function(r, design_type) {
  factor_text <- ""
  levels_text <- ""
  design_name <- ""
  
  if (design_type == "DIC") {
    design_name <- "Delineamento Inteiramente Casualizado (DIC)"
    factor_text <- r$factor_name
    levels_text <- paste(r$levels_vec, collapse = ", ")
    
    txt <- sprintf(
      "O experimento foi planejado utilizando o %s. O estudo avalia o fator '%s' com %d níveis (%s). Cada tratamento possui %d repetições, totalizando %d unidades experimentais. A variável de resposta principal a ser medida é '%s'. A distribuição espacial das unidades foi totalmente casualizada com a semente aleatória %d.",
      design_name, factor_text, length(r$levels_vec), levels_text, r$reps, nrow(r$df), r$response_var, r$seed
    )
  } else if (design_type == "DBC") {
    design_name <- "Delineamento em Blocos Casualizados (DBC)"
    factor_text <- r$factor_name
    levels_text <- paste(r$levels_vec, collapse = ", ")
    
    txt <- sprintf(
      "O experimento foi planejado sob o %s, indicado para controlar gradientes locais de variabilidade. O estudo avalia o fator '%s' com %d níveis (%s) distribuídos em %d blocos. Cada bloco contém todos os tratamentos em ordem casualizada, totalizando %d unidades experimentais. A variável de resposta avaliada é '%s'. O sorteio foi realizado com a semente aleatória %d.",
      design_name, factor_text, length(r$levels_vec), levels_text, r$blocks, nrow(r$df), r$response_var, r$seed
    )
  } else if (design_type == "DQL") {
    design_name <- "Delineamento em Quadrado Latino (DQL)"
    factor_text <- r$factor_name
    levels_text <- paste(r$levels_vec, collapse = ", ")
    k <- length(r$levels_vec)
    
    txt <- sprintf(
      "O experimento foi estruturado no %s, indicado para controlar duas fontes independentes de variação local (linhas e colunas). O estudo avalia o fator '%s' com %d níveis (%s) em uma grade de %d linhas por %d colunas, totalizando %d unidades experimentais. Cada tratamento aparece exatamente uma vez em cada linha e em cada coluna. A variável de resposta avaliada é '%s'. O sorteio foi realizado com a semente aleatória %d.",
      design_name, factor_text, k, levels_text, k, k, nrow(r$df), r$response_var, r$seed
    )
  } else if (design_type == "fatorial") {
    design_name <- "Delineamento Fatorial"
    levels_a <- paste(r$fator_a_levels, collapse = ", ")
    levels_b <- paste(r$fator_b_levels, collapse = ", ")
    k <- length(r$fator_a_levels) * length(r$fator_b_levels)
    
    txt <- sprintf(
      "O experimento foi planejado em %s para avaliar a interação entre dois fatores. O Fator A '%s' possui %d níveis (%s) e o Fator B '%s' possui %d níveis (%s), totalizando %d combinações de tratamentos. O experimento conta com %d repetições por combinação, totalizando %d unidades experimentais organizadas de forma casualizada. A variável de resposta avaliada é '%s'. A semente de randomização foi %d.",
      design_name, r$fator_a_name, length(r$fator_a_levels), levels_a, r$fator_b_name, length(r$fator_b_levels), levels_b, k, r$reps, nrow(r$df), r$response_var, r$seed
    )
  } else if (design_type == "split_plot") {
    design_name <- "Delineamento em Parcelas Subdivididas (Split-Plot)"
    levels_main <- paste(r$fator_main_levels, collapse = ", ")
    levels_sub <- paste(r$fator_sub_levels, collapse = ", ")
    
    txt <- sprintf(
      "O experimento foi estruturado em %s. O Fator Principal '%s' possui %d níveis (%s) alocados às parcelas principais, e o Fator Subdividido '%s' possui %d níveis (%s) alocados às subparcelas. O experimento foi conduzido em %d blocos, totalizando %d unidades experimentais. A casualização das parcelas principais ocorreu dentro de cada bloco, e a casualização das subparcelas ocorreu dentro de cada parcela principal. A variável de resposta medida é '%s'. A semente de randomização foi %d.",
      design_name, r$fator_main_name, length(r$fator_main_levels), levels_main, r$fator_sub_name, length(r$fator_sub_levels), levels_sub, r$blocks, nrow(r$df), r$response_var, r$seed
    )
  } else {
    txt <- "Planejamento experimental estruturado."
  }
  
  return(txt)
}

# =============================================================================
# 8. FORMATAÇÃO DA TABELA (Identidade Ocean Gradient + Colorida)
# =============================================================================
flextable_croqui <- function(df, design_type, level_colors) {
  # Remover coluna Cor interna para apresentação limpa
  clean_df <- df
  if ("Cor" %in% names(clean_df)) {
    clean_df$Cor <- NULL
  }
  
  # Trocar NAs na coluna da Resposta por texto vazio "" para edição manual no Word
  # (flextable lida com caracteres vazios perfeitamente, deixando a célula livre para digitação)
  for (col in names(clean_df)) {
    if (any(is.na(clean_df[[col]]))) {
      clean_df[[col]][is.na(clean_df[[col]])] <- ""
    }
  }
  
  ft <- flextable::flextable(clean_df) |>
    flextable::theme_booktabs() |>
    flextable::bg(part = "header", bg = "#0F3B5F") |>
    flextable::color(part = "header", color = "white") |>
    flextable::bold(part = "header") |>
    flextable::font(fontname = "Times New Roman", part = "all") |>
    flextable::fontsize(size = 9, part = "all") |>
    flextable::align(align = "center", part = "all") |>
    flextable::align(j = 1, align = "left", part = "all") |>
    flextable::padding(padding = 4, part = "all")
  
  # Colorir a coluna do tratamento com a respectiva cor do croqui
  for (treat_val in names(level_colors)) {
    row_indices <- which(clean_df$Tratamento == treat_val)
    if (length(row_indices) > 0) {
      ft <- flextable::bg(ft, i = row_indices, j = "Tratamento", bg = level_colors[treat_val])
      # Para destacar, colocamos a fonte em negrito na coluna colorida do tratamento
      ft <- flextable::bold(ft, i = row_indices, j = "Tratamento")
    }
  }
  
  ft <- flextable::autofit(ft)
  return(ft)
}
