# Funções de apresentação ---------------------------------------------------
# Este arquivo só define funções. Os cálculos estão em analise.R.
# Usamos pacote::funcao para deixar clara a origem de cada recurso.

fmt <- function(x, dig = 2) {
  # Mantém a precisão dos objetos; arredonda apenas a exibição.
  saida <- formatC(x, format = "f", digits = dig, decimal.mark = ",")
  saida[is.na(x)] <- "não calculado"
  saida
}

formatar_p <- function(p, no_texto = FALSE) {
  # Um p muito pequeno nunca é mostrado como zero.
  saida <- ifelse(p < 0.001, "< 0,001", fmt(p, 3))
  if (no_texto) saida <- ifelse(p < 0.001, paste("p", saida), paste("p =", saida))
  saida[is.na(p)] <- "não calculado"
  saida
}

tema_projeto <- function() {
  ggplot2::theme_classic(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", colour = "#0F3B5F"),
      plot.title.position = "plot",
      legend.position = "bottom",
      plot.background = ggplot2::element_rect(fill = "white", colour = NA)
    )
}

flextable_ocean <- function(tab) {
  # Largura limitada à área útil do modelo Word; linhas não se dividem.
  flextable::flextable(tab) |>
    flextable::theme_booktabs() |>
    flextable::bg(bg = "#0F3B5F", part = "header") |>
    flextable::color(color = "white", part = "header") |>
    flextable::bold(part = "header") |>
    flextable::font(fontname = "Times New Roman", part = "all") |>
    flextable::fontsize(size = 10, part = "all") |>
    flextable::align(align = "center", part = "all") |>
    flextable::align(j = 1, align = "left", part = "all") |>
    flextable::padding(padding = 4, part = "all") |>
    flextable::autofit() |>
    flextable::fit_to_width(max_width = 6.1) |>
    flextable::set_table_properties(layout = "autofit", width = 1,
      opts_word = list(split = FALSE, repeat_headers = TRUE))
}
