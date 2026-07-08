# =============================================================================
# funcoes_frequencia.R
# -----------------------------------------------------------------------------
# Tabela de Distribuição de Frequência (dados discretos e contínuos) da CatalyseR.
#
# Arquitetura (fonte canônica única):
#   calcular_freq()        -> executa a tabulação e devolve UMA lista canônica.
#   mostrar_freq()         -> formata a lista como data.frame (com linha de Total).
#   flextable_freq_ocean() -> tabela no tema Ocean Gradient (Viewer/HTML e .docx).
#   grafico_freq()         -> histograma + polígono de frequência (contínuo) ou
#                             barras (discreto), no tema Ocean.
#   relatar_freq()         -> frase-relatório em português.
#
# Técnicas modernas de classes (contínuo): Sturges, Scott, Freedman-Diaconis e
# raiz de n (todas em base R via grDevices::nclass.*), OU número de classes /
# amplitude definidos pelo usuário.
# =============================================================================

suppressWarnings(suppressMessages({
  library(tibble)
  library(flextable)
}))

# Paleta Ocean Gradient
.freq_ocean <- c(NAVY = "#0F3B5F", TEAL = "#2E7D8F", SEAFOAM = "#62B6B7",
                 AMBER = "#E89B3C", CORAL = "#E76F51")

# ---- Utilitário: formato numérico brasileiro (vírgula decimal) --------------
if (!exists("fmt")) {
  fmt <- function(x, dig = 2) {
    if (is.null(x) || length(x) == 0 || is.na(x)) return("-")
    formatC(x, format = "f", digits = dig, decimal.mark = ",")
  }
}

# ---- Escolha do número de classes (técnicas modernas) -----------------------
escolher_n_classes <- function(x, metodo = "sturges") {
  x <- x[!is.na(x)]
  metodo <- tolower(metodo)
  k <- tryCatch(switch(metodo,
    "sturges" = grDevices::nclass.Sturges(x),
    "scott"   = grDevices::nclass.scott(x),
    "fd"      = grDevices::nclass.FD(x),
    "raiz"    = ceiling(sqrt(length(x))),
    grDevices::nclass.Sturges(x)), error = function(e) grDevices::nclass.Sturges(x))
  max(1L, as.integer(k))
}

.freq_label_metodo <- function(m) {
  switch(tolower(m),
    "sturges" = "Sturges", "scott" = "Scott",
    "fd" = "Freedman-Diaconis", "raiz" = "raiz de n", m)
}

# ---- Frequência de dados DISCRETOS (valores/categorias) ---------------------
.freq_discreta <- function(x, nome) {
  n <- length(x)
  if (is.numeric(x)) {
    valores <- sort(unique(x))
    fi <- as.integer(tabulate(match(x, valores), nbins = length(valores)))
    rot <- as.character(valores)
    pts_x <- valores
  } else {
    xf <- as.factor(x)
    rot <- levels(xf)
    fi <- as.integer(table(xf))
    pts_x <- seq_along(rot)
  }
  fr <- fi / n * 100
  Fi <- cumsum(fi)
  Fr <- cumsum(fr)
  tab <- data.frame(Valor = rot, fi = fi, `fr (%)` = round(fr, 2),
                    Fi = Fi, `Fr (%)` = round(Fr, 2),
                    check.names = FALSE, stringsAsFactors = FALSE)
  list(tipo = "discreto", nome = nome, n = n, tabela = tab,
       pontos_x = pts_x, pontos_y = fi, rotulos = rot)
}

# ---- Frequência de dados CONTÍNUOS (classes) --------------------------------
.freq_continua <- function(x, nome, metodo_classes, n_classes, amplitude, digitos) {
  n <- length(x)
  minx <- min(x); maxx <- max(x); intervalo <- maxx - minx
  if (intervalo <= 0) intervalo <- 1  # todos iguais: evita divisão por zero

  if (!is.null(amplitude) && is.finite(amplitude) && amplitude > 0) {
    k <- max(1L, as.integer(ceiling(intervalo / amplitude)))
    amp <- amplitude
    metodo_usado <- "amplitude definida"
  } else if (!is.null(n_classes) && is.finite(n_classes) && n_classes >= 1) {
    k <- as.integer(n_classes)
    amp <- intervalo / k
    metodo_usado <- "nº de classes definido"
  } else {
    k <- escolher_n_classes(x, metodo_classes)
    amp <- intervalo / k
    metodo_usado <- .freq_label_metodo(metodo_classes)
  }

  breaks <- minx + (0:k) * amp
  breaks[length(breaks)] <- max(breaks[length(breaks)], maxx)  # garante o máximo
  # separa em [Li, Ls), última classe fechada à direita
  classes <- cut(x, breaks = breaks, include.lowest = TRUE, right = FALSE, dig.lab = 12)
  fi <- as.integer(table(classes))
  Li <- breaks[-length(breaks)]
  Ls <- breaks[-1]
  xi <- (Li + Ls) / 2
  fr <- fi / n * 100
  Fi <- cumsum(fi)
  Fr <- cumsum(fr)

  rot <- sprintf(paste0("[%s ; %s)"),
                 formatC(Li, format = "f", digits = digitos),
                 formatC(Ls, format = "f", digits = digitos))
  rot[length(rot)] <- sub(")$", "]", rot[length(rot)])  # última fechada

  tab <- data.frame(
    Classe = rot,
    `Ponto médio` = round(xi, digitos),
    fi = fi, `fr (%)` = round(fr, 2), Fi = Fi, `Fr (%)` = round(Fr, 2),
    check.names = FALSE, stringsAsFactors = FALSE)

  list(tipo = "continuo", nome = nome, n = n, k = k, amplitude = amp,
       metodo = metodo_usado, minimo = minx, maximo = maxx,
       tabela = tab, pontos_x = xi, pontos_y = fi, Li = Li, Ls = Ls)
}

# ---- CÁLCULO canônico -------------------------------------------------------
# tipo: "auto" | "discreto" | "continuo"
calcular_freq <- function(x, tipo = "auto", metodo_classes = "sturges",
                          n_classes = NULL, amplitude = NULL, digitos = 2,
                          nome = "Variável") {
  x_raw <- x
  x <- x[!is.na(x)]
  if (length(x) == 0) stop("A variável não tem valores válidos para tabular.")

  if (tipo == "auto") {
    if (!is.numeric(x)) tipo <- "discreto"
    else if (length(unique(x)) <= 12 && all(x == round(x))) tipo <- "discreto"
    else tipo <- "continuo"
  }

  if (tipo == "discreto") {
    .freq_discreta(x, nome)
  } else {
    if (!is.numeric(x)) stop("A tabela por classes exige uma variável numérica.")
    .freq_continua(x, nome, metodo_classes, n_classes, amplitude, digitos)
  }
}

# ---- TABELA para exibição (acrescenta linha de Total) -----------------------
mostrar_freq <- function(r) {
  tab <- r$tabela
  n <- r$n
  total <- tab[1, ]
  total[1, ] <- NA
  total[[1]] <- "Total"
  total[["fi"]] <- n
  total[["fr (%)"]] <- 100
  # colunas acumuladas ficam em branco no Total
  if ("Fi" %in% names(total)) total[["Fi"]] <- NA
  if ("Fr (%)" %in% names(total)) total[["Fr (%)"]] <- NA
  if ("Ponto médio" %in% names(total)) total[["Ponto médio"]] <- NA
  rbind(tab, total)
}

# ---- FLEXTABLE no tema Ocean Gradient ---------------------------------------
# Cabeçalho NAVY, faixa zebrada SEAFOAM claro, linha de Total destacada.
flextable_freq_ocean <- function(r) {
  tab <- mostrar_freq(r)
  nr <- nrow(tab)
  # substitui NA por vazio na exibição
  tab_disp <- tab
  tab_disp[] <- lapply(tab_disp, function(col) {
    if (is.numeric(col)) ifelse(is.na(col), "", formatC(col, format = "f",
                        digits = ifelse(all(col == round(col), na.rm = TRUE), 0, 2),
                        decimal.mark = ",", big.mark = "")) else ifelse(is.na(col), "", col)
  })

  ft <- flextable::flextable(tab_disp) |>
    flextable::theme_booktabs() |>
    flextable::bg(part = "header", bg = .freq_ocean["NAVY"]) |>
    flextable::color(part = "header", color = "white") |>
    flextable::bold(part = "header") |>
    flextable::bg(i = seq(1, nr - 1, by = 2), bg = "#EAF3F3", part = "body") |>
    flextable::bold(i = nr, part = "body") |>
    flextable::bg(i = nr, bg = "#C5D8CF", part = "body") |>
    flextable::font(fontname = "Times New Roman", part = "all") |>
    flextable::fontsize(size = 10, part = "all") |>
    flextable::align(align = "center", part = "all") |>
    flextable::align(j = 1, align = "left", part = "all") |>
    flextable::padding(padding = 5, part = "all") |>
    flextable::autofit()
  ft
}

# ---- GRÁFICO: histograma + polígono (contínuo) ou barras (discreto) ---------
grafico_freq <- function(r) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  oc <- .freq_ocean
  if (identical(r$tipo, "continuo")) {
    dfc <- data.frame(Li = r$Li, Ls = r$Ls, xi = r$pontos_x, fi = r$pontos_y)
    amp <- r$amplitude
    # polígono de frequência: ancora em fi = 0 antes da 1ª e depois da última classe
    poly <- data.frame(
      xi = c(r$pontos_x[1] - amp, r$pontos_x, r$pontos_x[length(r$pontos_x)] + amp),
      fi = c(0, r$pontos_y, 0))
    ggplot2::ggplot() +
      ggplot2::geom_rect(data = dfc,
        ggplot2::aes(xmin = Li, xmax = Ls, ymin = 0, ymax = fi),
        fill = oc[["SEAFOAM"]], color = "white", alpha = 0.85) +
      ggplot2::geom_line(data = poly, ggplot2::aes(xi, fi),
                         color = oc[["CORAL"]], linewidth = 1) +
      ggplot2::geom_point(data = poly, ggplot2::aes(xi, fi),
                          color = oc[["CORAL"]], size = 2) +
      ggplot2::labs(x = r$nome, y = "Frequência (fᵢ)",
                    title = "Histograma e polígono de frequência") +
      ggplot2::theme_minimal(base_size = 12)
  } else {
    dfd <- data.frame(valor = factor(r$rotulos, levels = r$rotulos), fi = r$pontos_y)
    ggplot2::ggplot(dfd, ggplot2::aes(valor, fi)) +
      ggplot2::geom_col(fill = oc[["TEAL"]], color = "white", width = 0.8) +
      ggplot2::labs(x = r$nome, y = "Frequência (fᵢ)",
                    title = "Distribuição de frequências") +
      ggplot2::theme_minimal(base_size = 12)
  }
}

# ---- RELATÓRIO narrativo em português ---------------------------------------
relatar_freq <- function(r) {
  # classe/valor modal
  idx_modal <- which.max(r$pontos_y)
  if (identical(r$tipo, "continuo")) {
    classe_modal <- r$tabela$Classe[idx_modal]
    paste0(
      "A distribuição de frequência de ", r$nome, " foi organizada em ", r$k,
      " classes de amplitude ", fmt(r$amplitude), " (método: ", r$metodo,
      "), a partir de ", r$n, " observações válidas (mínimo = ", fmt(r$minimo),
      "; máximo = ", fmt(r$maximo), "). A classe modal — a de maior frequência — foi ",
      classe_modal, ", com ", r$pontos_y[idx_modal], " observações (",
      fmt(r$pontos_y[idx_modal] / r$n * 100, 1), "%). O polígono de frequência ",
      "resume o formato da distribuição (simetria/assimetria e concentração)."
    )
  } else {
    valor_modal <- r$rotulos[idx_modal]
    paste0(
      "A distribuição de frequência de ", r$nome, " (dados discretos) reuniu ", r$n,
      " observações em ", length(r$rotulos), " categorias. A categoria modal foi '",
      valor_modal, "', com ", r$pontos_y[idx_modal], " ocorrências (",
      fmt(r$pontos_y[idx_modal] / r$n * 100, 1), "%). As colunas de frequência ",
      "acumulada (Fᵢ e Fr) permitem ler quantas observações estão até cada categoria."
    )
  }
}
