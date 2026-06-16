# =============================================================================
# funcoes_sampling.R
# -----------------------------------------------------------------------------
# Funções auxiliares para cálculo, tabelas e relatos dos métodos de amostragem:
#   AAS: Amostragem Aleatória Simples
#   AEP: Amostragem Estratificada Proporcional
#   AS: Amostragem Sistemática
#
# Arquitetura (fonte canônica única):
#   calcular_*() -> Realiza o sorteio e os cálculos estatísticos (média, total, IC)
#   mostrar_*()  -> Formata as estatísticas em tabelas (tibbles) para o Shiny/Quarto
#   relatar_*()  -> Gera a narrativa formal em português para os resultados
# =============================================================================

library(tibble)
library(flextable)

# ---- Utilitário: Formato numérico brasileiro ---------------------------------
fmt <- function(x, dig = 2) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return("-")
  formatC(x, format = "f", digits = dig, decimal.mark = ",")
}

# ---- 1. AMOSTRAGEM ALEATÓRIA SIMPLES (AAS) -----------------------------------

#' Executa o sorteio e calcula as estimativas para AAS
calcular_aas <- function(df, var_y, n, seed = 42) {
  if (!is.null(seed) && !is.na(seed) && is.numeric(seed) && length(seed) > 0) {
    set.seed(as.integer(seed))
  } else {
    set.seed(42)
  }
  N <- nrow(df)
  n <- min(n, N)
  
  # Sorteio dos índices
  indices <- sample(1:N, size = n, replace = FALSE)
  sample_data <- df[indices, , drop = FALSE]
  y <- sample_data[[var_y]]
  
  # Estimativas pontuais e de variância
  m_amostral <- mean(y, na.rm = TRUE)
  s2 <- var(y, na.rm = TRUE)
  
  # Variância da média amostral com FPC (Correção de População Finita)
  fpc <- 1 - (n / N)
  var_media <- fpc * (s2 / n)
  se_media <- sqrt(var_media)
  
  # Intervalo de Confiança para a Média (t de Student)
  df_t <- max(n - 1, 1)
  t_crit <- qt(0.975, df = df_t)
  me_media <- t_crit * se_media
  ic_media <- c(m_amostral - me_media, m_amostral + me_media)
  
  # Projeção do Total Populacional
  total_est <- N * m_amostral
  var_total <- (N^2) * var_media
  se_total <- sqrt(var_total)
  me_total <- t_crit * se_total
  ic_total <- c(total_est - me_total, total_est + me_total)
  
  list(
    N = N,
    n = n,
    seed = seed,
    var_y = var_y,
    sample_data = sample_data,
    indices = indices,
    media = m_amostral,
    variancia = s2,
    fpc = fpc,
    se_media = se_media,
    ic_media = ic_media,
    total = total_est,
    se_total = se_total,
    ic_total = ic_total,
    df_t = df_t
  )
}

#' Mostra a tabela estruturada de estimativas de AAS
mostrar_aas <- function(r) {
  tibble::tibble(
    `Parâmetro` = c("Média da População (Estimada)", "Total da População (Projetado)"),
    `Estimativa` = c(round(r$media, 2), round(r$total, 2)),
    `Erro Padrão` = c(round(r$se_media, 3), round(r$se_total, 2)),
    `IC 95% Inferior` = c(round(r$ic_media[1], 2), round(r$ic_total[1], 2)),
    `IC 95% Superior` = c(round(r$ic_media[2], 2), round(r$ic_total[2], 2)),
    `FPC (Fator Pop. Finita)` = c(round(r$fpc, 4), round(r$fpc, 4))
  )
}

#' Relatório descritivo para AAS em português
relatar_aas <- function(r, label_y = "produção") {
  paste0(
    "A partir de uma população de N = ", r$N, " unidades, foi sorteada uma Amostragem Aleatória Simples (AAS) de n = ", r$n,
    " observações, utilizando uma semente de randomização de controle igual a ", r$seed, ". ",
    "A média amostral obtida para ", label_y, " foi de ", fmt(r$media), " (EP = ", fmt(r$se_media, 3), 
    "), resultando em um intervalo de confiança de 95% para a média de [", fmt(r$ic_media[1]), "; ", fmt(r$ic_media[2]), "]. ",
    "A projeção para o total da população foi estimada em ", fmt(r$total), " unidades (EP = ", fmt(r$se_total), 
    "), com o intervalo de confiança de 95% para o total populacional estimado entre [", fmt(r$ic_total[1]), "; ", fmt(r$ic_total[2]), "]. ",
    "Os cálculos incorporaram a Correção de População Finita (FPC = ", fmt(r$fpc, 4), ") devido ao caráter exaustivo da amostragem sem reposição."
  )
}


# ---- 2. AMOSTRAGEM ESTRATIFICADA PROPORCIONAL (AEP) --------------------------

#' Executa o sorteio e calcula as estimativas para AEP
calcular_aep <- function(df, var_y, var_strata, n, seed = 42) {
  if (!is.null(seed) && !is.na(seed) && is.numeric(seed) && length(seed) > 0) {
    set.seed(as.integer(seed))
  } else {
    set.seed(42)
  }
  N <- nrow(df)
  n <- min(n, N)
  
  # Tamanho dos estratos na população (Nh)
  tbl_N <- table(df[[var_strata]])
  strata_names <- names(tbl_N)
  N_h <- as.numeric(tbl_N)
  names(N_h) <- strata_names
  
  # Alocação Proporcional inicial
  n_h <- round(n * (N_h / N))
  
  # Ajuste para garantir que todo estrato com N_h > 0 tenha pelo menos 1 amostra
  # e que a soma de n_h seja exatamente igual a n
  n_h[N_h > 0] <- pmax(n_h[N_h > 0], 1)
  
  # Se estourou ou faltou do n desejado devido aos arredondamentos
  diff_n <- n - sum(n_h)
  if (diff_n != 0) {
    # Ajusta nos estratos maiores de forma ordenada
    sorted_strata <- order(N_h, decreasing = TRUE)
    for (i in 1:abs(diff_n)) {
      idx <- sorted_strata[(i - 1) %% length(sorted_strata) + 1]
      if (diff_n > 0) {
        if (n_h[idx] < N_h[idx]) {
          n_h[idx] <- n_h[idx] + 1
        }
      } else {
        if (n_h[idx] > 1) {
          n_h[idx] <- n_h[idx] - 1
        }
      }
    }
  }
  
  # Executa o sorteio estratificado
  sample_indices <- c()
  for (h in strata_names) {
    indices_h <- which(df[[var_strata]] == h)
    drawn_h <- sample(indices_h, size = n_h[h], replace = FALSE)
    sample_indices <- c(sample_indices, drawn_h)
  }
  
  sample_data <- df[sample_indices, , drop = FALSE]
  
  # Cálculos por estrato
  y_lst <- split(sample_data[[var_y]], sample_data[[var_strata]])
  mean_h <- sapply(y_lst, mean, na.rm = TRUE)
  var_h <- sapply(y_lst, function(x) {
    if (length(x) > 1) var(x, na.rm = TRUE) else 0
  })
  
  # Pesos dos estratos Wh = Nh / N
  W_h <- N_h / N
  
  # Estimador da Média Estratificada
  m_estratificada <- sum(W_h * mean_h)
  
  # Variância da Média Estratificada com FPC para cada estrato
  fpc_h <- 1 - (n_h / N_h)
  var_h_mean <- (var_h / n_h)
  var_media <- sum((W_h^2) * fpc_h * var_h_mean)
  se_media <- sqrt(var_media)
  
  # Graus de liberdade aproximados
  df_t <- max(sum(n_h - 1), 1)
  t_crit <- qt(0.975, df = df_t)
  me_media <- t_crit * se_media
  ic_media <- c(m_estratificada - me_media, m_estratificada + me_media)
  
  # Projeção do Total Populacional
  total_est <- N * m_estratificada
  var_total <- (N^2) * var_media
  se_total <- sqrt(var_total)
  me_total <- t_crit * se_total
  ic_total <- c(total_est - me_total, total_est + me_total)
  
  # Tabela de alocação formatada
  allocation_table <- tibble::tibble(
    `Estrato` = strata_names,
    `População (Nh)` = N_h,
    `Peso (Wh)` = round(W_h, 4),
    `Amostra Sorteada (nh)` = n_h,
    `Média Amostral (yh)` = round(mean_h, 2),
    `Variância Amostral (sh²)` = round(var_h, 2)
  )
  
  list(
    N = N,
    n = n,
    seed = seed,
    var_y = var_y,
    var_strata = var_strata,
    sample_data = sample_data,
    allocation = allocation_table,
    media = m_estratificada,
    se_media = se_media,
    ic_media = ic_media,
    total = total_est,
    se_total = se_total,
    ic_total = ic_total,
    df_t = df_t
  )
}

#' Mostra a tabela estruturada de estimativas de AEP
mostrar_aep <- function(r) {
  tibble::tibble(
    `Parâmetro` = c("Média Estratificada (Estimada)", "Total Populacional (Projetado)"),
    `Estimativa` = c(round(r$media, 2), round(r$total, 2)),
    `Erro Padrão` = c(round(r$se_media, 3), round(r$se_total, 2)),
    `IC 95% Inferior` = c(round(r$ic_media[1], 2), round(r$ic_total[1], 2)),
    `IC 95% Superior` = c(round(r$ic_media[2], 2), round(r$ic_total[2], 2))
  )
}

#' Relatório descritivo para AEP em português
relatar_aep <- function(r, label_y = "produção", label_strata = "embarcações") {
  paste0(
    "Para estimar a produção total e a média na população de tamanho N = ", r$N, 
    " unidades, aplicou-se um plano de Amostragem Estratificada Proporcional (AEP) com base na variável de estratificação ", 
    label_strata, ". A amostra total sorteada foi de n = ", r$n, " unidades (semente = ", r$seed, "). ",
    "A alocação das subamostras respeitou a proporção de cada subpopulação nos estratos. ",
    "A média global estratificada calculada foi de ", fmt(r$media), " (EP = ", fmt(r$se_media, 3), 
    "), gerando um intervalo de confiança de 95% para a média estratificada de [", fmt(r$ic_media[1]), "; ", fmt(r$ic_media[2]), "]. ",
    "Com base nisso, a produção total populacional projetada foi de ", fmt(r$total), " (EP = ", fmt(r$se_total), 
    "), com o intervalo de confiança de 95% para o total estimado entre [", fmt(r$ic_total[1]), "; ", fmt(r$ic_total[2]), "]. ",
    "Os cálculos utilizaram ponderações de estratos e incluíram a Correção de População Finita (FPC) em nível de estrato para maior precisão das variâncias."
  )
}


# ---- 3. AMOSTRAGEM SISTEMÁTICA (AS) ------------------------------------------

#' Executa o sorteio e calcula as estimativas para AS
calcular_as <- function(df, var_y, n, seed = 42) {
  if (!is.null(seed) && !is.na(seed) && is.numeric(seed) && length(seed) > 0) {
    set.seed(as.integer(seed))
  } else {
    set.seed(42)
  }
  N <- nrow(df)
  n <- min(n, N)
  
  # Intervalo de amostragem k
  k <- max(floor(N / n), 1)
  
  # Sorteio do ponto de partida r entre 1 e k
  r <- sample(1:k, size = 1)
  
  # Seleção dos índices sistemáticos
  indices <- seq(r, by = k, length.out = n)
  # Garante que os índices não ultrapassem N
  indices <- indices[indices <= N]
  n_real <- length(indices)
  
  sample_data <- df[indices, , drop = FALSE]
  y <- sample_data[[var_y]]
  
  # Estimativas (tratadas sob a aproximação de AAS)
  m_amostral <- mean(y, na.rm = TRUE)
  s2 <- var(y, na.rm = TRUE)
  
  fpc <- 1 - (n_real / N)
  var_media <- fpc * (s2 / n_real)
  se_media <- sqrt(var_media)
  
  df_t <- max(n_real - 1, 1)
  t_crit <- qt(0.975, df = df_t)
  me_media <- t_crit * se_media
  ic_media <- c(m_amostral - me_media, m_amostral + me_media)
  
  total_est <- N * m_amostral
  var_total <- (N^2) * var_media
  se_total <- sqrt(var_total)
  me_total <- t_crit * se_total
  ic_total <- c(total_est - me_total, total_est + me_total)
  
  list(
    N = N,
    n = n_real,
    seed = seed,
    k = k,
    r = r,
    var_y = var_y,
    sample_data = sample_data,
    indices = indices,
    media = m_amostral,
    variancia = s2,
    fpc = fpc,
    se_media = se_media,
    ic_media = ic_media,
    total = total_est,
    se_total = se_total,
    ic_total = ic_total,
    df_t = df_t
  )
}

#' Mostra a tabela estruturada de estimativas de AS
mostrar_as <- function(r) {
  mostrar_aas(r) # Compartilha a mesma estrutura de exibição estatística da AAS
}

#' Relatório descritivo para AS em português
relatar_as <- function(r, label_y = "produção") {
  paste0(
    "Foi implementada uma Amostragem Sistemática (AS) para avaliar ", label_y, " a partir de uma população de tamanho N = ", r$N, 
    ". Com base no tamanho de amostra planejado, determinou-se o intervalo sistemático de seleção k = ", r$k, ". ",
    "Utilizando uma semente de randomização igual a ", r$seed, ", sorteou-se a partida inicial r = ", r$r, 
    ", resultando na seleção de uma amostra efetiva de n = ", r$n, " unidades. ",
    "Sob a aproximação canônica de Amostragem Aleatória Simples, a média estimada para a população foi de ", fmt(r$media), 
    " (EP = ", fmt(r$se_media, 3), "), gerando um intervalo de confiança de 95% para a média de [", fmt(r$ic_media[1]), "; ", fmt(r$ic_media[2]), "]. ",
    "A projeção do total populacional foi calculada em ", fmt(r$total), " (EP = ", fmt(r$se_total), 
    "), com o intervalo de confiança de 95% para o total populacional estimado entre [", fmt(r$ic_total[1]), "; ", fmt(r$ic_total[2]), "]. ",
    "O fator de correção de população finita correspondente foi FPC = ", fmt(r$fpc, 4), "."
  )
}


# ---- 4. FORMATAÇÃO DA TABELA (Identidade Ocean Gradient) ---------------------
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
