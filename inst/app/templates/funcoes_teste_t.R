# =============================================================================
# funcoes_teste_t.R
# -----------------------------------------------------------------------------
# Funções de apoio para análise e relato de teste t para uma amostra,
# duas amostras independentes e amostras pareadas.
#
# Arquitetura (fonte canônica única):
#   calcular_*()  -> executa o teste e devolve UMA lista com todas as
#                    estatísticas. É a definição canônica de cada análise.
#   mostrar_*()   -> apenas FORMATA a lista canônica como tabela (tibble).
#   relatar_*()   -> apenas FORMATA a lista canônica como frase em português.
# Assim, tabela e narrativa nunca podem divergir: ambas leem o mesmo cálculo.
#
# Conteúdo:
#   fmt()                     - formato numérico brasileiro (vírgula decimal)
#   interpretar_d()           - classifica o tamanho de efeito (d de Cohen)
#   rotulo_ic()               - rótulo dinâmico do IC a partir de conf
#   calcular_teste_t()        - cálculo canônico (uma amostra)
#   mostrar_teste_t()         - tabela estruturada (uma amostra)
#   relatar_teste_t()         - frase-relatório em português (uma amostra)
#   calcular_teste_t_ind()    - cálculo canônico (duas amostras independentes)
#   mostrar_teste_t_ind()     - tabela estruturada (duas amostras independentes)
#   relatar_teste_t_ind()     - frase-relatório em português (duas amostras independentes)
#   calcular_teste_t_pareado()- cálculo canônico (amostras pareadas)
#   mostrar_teste_t_pareado() - tabela estruturada (amostras pareadas)
#   relatar_teste_t_pareado() - frase-relatório em português (amostras pareadas)
#   flextable_ocean()         - formata a tabela na identidade Ocean Gradient (docx)
# =============================================================================

library(tibble)
library(flextable)

# ---- Utilitário: formato numérico brasileiro (vírgula decimal) --------------
fmt <- function(x, dig = 2) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return("-")
  formatC(x, format = "f", digits = dig, decimal.mark = ",")
}


# ---- Interpretação do tamanho de efeito (d de Cohen, em módulo) -------------
interpretar_d <- function(d) {
  if (is.null(d) || length(d) == 0 || is.na(d)) return("indefinido")
  a <- abs(d)
  if (a < 0.20)      "desprezível"
  else if (a < 0.50) "pequeno"
  else if (a < 0.80) "moderado"
  else               "grande"
}


# ---- Rótulo dinâmico do intervalo de confiança ------------------------------
# Garante que o cabeçalho da tabela acompanhe o conf escolhido (ex.: 90%, 99%).
rotulo_ic <- function(conf = 0.95, sufixo = "") {
  paste0("IC ", round(conf * 100), "%", sufixo)
}


# ---- TESTE T PARA UMA AMOSTRA -----------------------------------------------

# Cálculo canônico (uma amostra). Devolve a lista de estatísticas que tanto a
# tabela quanto a narrativa consomem.
#   x    : vetor numérico
#   mu   : valor de referência (média hipotética sob H0)
#   conf : nível de confiança do IC (padrão 0,95)
calcular_teste_t <- function(x, mu = 0, conf = 0.95) {
  teste <- t.test(x, mu = mu, conf.level = conf)
  m     <- unname(teste$estimate)
  d     <- (mean(x, na.rm = TRUE) - mu) / sd(x, na.rm = TRUE)
  list(
    mu          = mu,
    media       = m,
    t           = unname(teste$statistic),
    gl          = unname(teste$parameter),
    p           = teste$p.value,
    ic          = unname(teste$conf.int),
    d           = d,
    efeito      = interpretar_d(d),
    conf        = conf,
    significativo = teste$p.value < 0.05
  )
}

# x     : vetor numérico
# mu    : valor de referência (média hipotética sob H0)
# nome  : rótulo da variável
# conf  : nível de confiança do IC (padrão 0,95)
# Retorna um tibble de uma linha.
mostrar_teste_t <- function(x, mu = 0, nome = "Variável", conf = 0.95) {
  r <- calcular_teste_t(x, mu = mu, conf = conf)
  tab <- tibble::tibble(
    `Variável`          = nome,
    `Média amostral`    = round(r$media, 2),
    `Média hipotética`  = r$mu,
    `t`                 = round(r$t, 3),
    `gl`                = r$gl,
    `p-valor`           = round(r$p, 4),
    `__IC__`            = paste0(round(r$ic[1], 2), " a ", round(r$ic[2], 2)),
    `d de Cohen`        = round(r$d, 2),
    `Tamanho do efeito` = r$efeito,
    `Conclusão`         = ifelse(r$significativo,
                                 "Diferença significativa",
                                 "Diferença não significativa")
  )
  names(tab)[names(tab) == "__IC__"] <- rotulo_ic(conf)
  tab
}

# Mesmos argumentos de mostrar_teste_t().
# Retorna uma string com marcação Markdown (*t*, *d*) para itálico no docx.
relatar_teste_t <- function(x, mu = 0, nome = "a variável", conf = 0.95) {
  r       <- calcular_teste_t(x, mu = mu, conf = conf)
  nivel   <- paste0(round(conf * 100), "%")
  p_txt   <- if (r$p < 0.001) "p < 0,001" else paste0("p = ", fmt(r$p, 3))
  signif_txt <- if (r$significativo)
    "indicaram uma diferença estatisticamente significativa em relação ao valor de referência"
  else
    "não indicaram diferença estatisticamente significativa em relação ao valor de referência"
  direcao <- if (r$media < r$mu) "inferior" else if (r$media > r$mu) "superior" else "igual"

  paste0(
    "Foi conduzido um teste *t* para uma amostra com o objetivo de comparar a média de ",
    nome, " com o valor de referência de ", fmt(r$mu), ". A média amostral foi de ",
    fmt(r$media), " (IC ", nivel, " [", fmt(r$ic[1]), "; ", fmt(r$ic[2]), "]). Os resultados ",
    signif_txt, ", *t*(", r$gl, ") = ", fmt(r$t, 3), ", ", p_txt,
    ", sendo a média amostral ", direcao, " ao valor hipotético. ",
    "O tamanho do efeito, estimado pelo *d* de Cohen, foi de ", fmt(r$d),
    ", correspondendo a um efeito ", r$efeito, "."
  )
}


# ---- TESTE T PARA DUAS AMOSTRAS INDEPENDENTES -------------------------------

# Cálculo canônico (duas amostras independentes).
#   formula_obj : fórmula y ~ x (ex: peso ~ grupo)
#   data        : data.frame
#   equal_var   : lógico (FALSE => Welch, variâncias desiguais)
#   conf        : nível de confiança do IC (padrão 0,95)
calcular_teste_t_ind <- function(formula_obj, data, equal_var = FALSE, conf = 0.95) {
  teste <- t.test(formula_obj, data = data, var.equal = equal_var, conf.level = conf)

  vars <- all.vars(formula_obj)
  y <- data[[vars[1]]]
  x <- as.factor(data[[vars[2]]])
  levels_x <- levels(x)
  if (length(levels_x) < 2) levels_x <- c(levels_x, "Grupo 2")

  y1 <- y[x == levels_x[1]]
  y2 <- y[x == levels_x[2]]

  n1 <- sum(!is.na(y1)); n2 <- sum(!is.na(y2))
  m1 <- mean(y1, na.rm = TRUE); m2 <- mean(y2, na.rm = TRUE)
  v1 <- var(y1, na.rm = TRUE);  v2 <- var(y2, na.rm = TRUE)

  # Cohen's d com desvio padrão combinado (pooled)
  sd_pooled <- sqrt(((n1 - 1) * v1 + (n2 - 1) * v2) / (n1 + n2 - 2))
  d <- (m1 - m2) / sd_pooled

  list(
    levels_x = levels_x,
    m1 = m1, m2 = m2, n1 = n1, n2 = n2,
    t  = unname(teste$statistic),
    gl = unname(teste$parameter),
    p  = teste$p.value,
    ic = unname(teste$conf.int),
    d  = d,
    efeito = interpretar_d(d),
    equal_var = equal_var,
    conf = conf,
    significativo = teste$p.value < 0.05
  )
}

mostrar_teste_t_ind <- function(formula_obj, data, equal_var = FALSE, conf = 0.95) {
  r <- calcular_teste_t_ind(formula_obj, data = data, equal_var = equal_var, conf = conf)
  tab <- tibble::tibble(
    `Grupo 1`           = r$levels_x[1],
    `Média G1`          = round(r$m1, 2),
    `Grupo 2`           = r$levels_x[2],
    `Média G2`          = round(r$m2, 2),
    `t`                 = round(r$t, 3),
    `gl`                = round(r$gl, 2),
    `p-valor`           = round(r$p, 4),
    `__IC__`            = paste0(round(r$ic[1], 2), " a ", round(r$ic[2], 2)),
    `d de Cohen`        = round(r$d, 2),
    `Tamanho do efeito` = r$efeito,
    `Conclusão`         = ifelse(r$significativo,
                                 "Diferença significativa",
                                 "Diferença não significativa")
  )
  names(tab)[names(tab) == "__IC__"] <- rotulo_ic(conf, " (Diferença)")
  tab
}

# Gera o relatório descritivo textual para teste t independente
relatar_teste_t_ind <- function(formula_obj, data, equal_var = FALSE, conf = 0.95,
                                label_y = "a variável de resposta", label_x = "a variável de agrupamento") {
  r        <- calcular_teste_t_ind(formula_obj, data = data, equal_var = equal_var, conf = conf)
  levels_x <- r$levels_x
  nivel    <- paste0(round(conf * 100), "%")
  p_txt    <- if (r$p < 0.001) "p < 0,001" else paste0("p = ", fmt(r$p, 3))

  signif_txt <- if (r$significativo)
    "indicaram uma diferença estatisticamente significativa entre as médias dos grupos"
  else
    "não indicaram diferença estatisticamente significativa entre as médias dos grupos"

  direcao <- if (r$m1 < r$m2) "inferior" else if (r$m1 > r$m2) "superior" else "igual"
  tipo_teste <- if (r$equal_var) "para variâncias iguais" else "de Welch (para variâncias desiguais)"

  paste0(
    "Foi conduzido um teste *t* de Student para duas amostras independentes ", tipo_teste,
    " para comparar a variável ", label_y, " entre os grupos definidos por ", label_x,
    " (", levels_x[1], " vs ", levels_x[2], "). A média no grupo ", levels_x[1],
    " foi de ", fmt(r$m1), " e a média no grupo ", levels_x[2], " foi de ", fmt(r$m2),
    " (IC ", nivel, " da diferença [", fmt(r$ic[1]), "; ", fmt(r$ic[2]), "]). ",
    "Os resultados ", signif_txt, ", *t*(", fmt(r$gl, 1), ") = ", fmt(r$t, 3),
    ", ", p_txt, ", sendo a média amostral do grupo ", levels_x[1], " ", direcao, " à média do grupo ", levels_x[2],
    ". O tamanho do efeito, estimado pelo *d* de Cohen, foi de ", fmt(r$d),
    ", correspondendo a um efeito ", r$efeito, "."
  )
}


# ---- TESTE T PAREADO --------------------------------------------------------

# Cálculo canônico (amostras pareadas).
#   x1   : vetor numérico (momento 1 / antes)
#   x2   : vetor numérico (momento 2 / depois)
#   conf : nível de confiança do IC (padrão 0,95)
calcular_teste_t_pareado <- function(x1, x2, conf = 0.95) {
  teste  <- t.test(x1, x2, paired = TRUE, conf.level = conf)
  dif    <- x1 - x2
  m_dif  <- mean(dif, na.rm = TRUE)
  sd_dif <- sd(dif, na.rm = TRUE)
  d      <- m_dif / sd_dif
  list(
    m1 = mean(x1, na.rm = TRUE),
    m2 = mean(x2, na.rm = TRUE),
    m_dif = m_dif,
    t  = unname(teste$statistic),
    gl = unname(teste$parameter),
    p  = teste$p.value,
    ic = unname(teste$conf.int),
    d  = d,
    efeito = interpretar_d(d),
    conf = conf,
    significativo = teste$p.value < 0.05
  )
}

mostrar_teste_t_pareado <- function(x1, x2, nome1 = "Antes", nome2 = "Depois", conf = 0.95) {
  r <- calcular_teste_t_pareado(x1, x2, conf = conf)
  tab <- tibble::tibble(
    `Variável 1`        = nome1,
    `Média V1`          = round(r$m1, 2),
    `Variável 2`        = nome2,
    `Média V2`          = round(r$m2, 2),
    `Diferença Média`   = round(r$m_dif, 2),
    `t`                 = round(r$t, 3),
    `gl`                = r$gl,
    `p-valor`           = round(r$p, 4),
    `__IC__`            = paste0(round(r$ic[1], 2), " a ", round(r$ic[2], 2)),
    `d de Cohen`        = round(r$d, 2),
    `Tamanho do efeito` = r$efeito,
    `Conclusão`         = ifelse(r$significativo,
                                 "Diferença significativa",
                                 "Diferença não significativa")
  )
  names(tab)[names(tab) == "__IC__"] <- rotulo_ic(conf, " (Diferença)")
  tab
}

# Gera o relatório descritivo textual para teste t pareado
relatar_teste_t_pareado <- function(x1, x2, nome1 = "Antes", nome2 = "Depois", conf = 0.95, label_y = "a variável de interesse") {
  r      <- calcular_teste_t_pareado(x1, x2, conf = conf)
  nivel  <- paste0(round(conf * 100), "%")
  p_txt  <- if (r$p < 0.001) "p < 0,001" else paste0("p = ", fmt(r$p, 3))

  signif_txt <- if (r$significativo)
    "indicaram uma diferença estatisticamente significativa entre os momentos pareados"
  else
    "não indicaram diferença estatisticamente significativa entre os momentos pareados"

  direcao <- if (r$m_dif < 0) "inferior (redução)" else if (r$m_dif > 0) "superior (aumento)" else "igual"

  paste0(
    "Foi conduzido um teste *t* pareado para comparar a média de ", label_y, " entre os momentos ", nome1, " e ", nome2,
    ". A média no momento ", nome1, " foi de ", fmt(r$m1), " e a média no momento ", nome2,
    " foi de ", fmt(r$m2), ", resultando em uma diferença média de ", fmt(r$m_dif),
    " (IC ", nivel, " da diferença [", fmt(r$ic[1]), "; ", fmt(r$ic[2]), "]). Os resultados ",
    signif_txt, ", *t*(", r$gl, ") = ", fmt(r$t, 3), ", ", p_txt,
    ", sendo a média de ", nome1, " caracterizada como ", direcao, " em relação a ", nome2,
    ". O tamanho do efeito, estimado pelo *d* de Cohen pareado, foi de ", fmt(r$d),
    ", correspondendo a um efeito ", r$efeito, "."
  )
}


# ---- Formatação da tabela (identidade Ocean Gradient, saída docx) -----------
# Recebe o tibble de qualquer um dos mostrar_teste_t e devolve um flextable formatado.
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
