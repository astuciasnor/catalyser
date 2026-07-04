# ============================================================================
#  Sorteio e visualização de Delineamento em Parcelas Subdivididas
#  Pacote de sorteio: agricolae  |  Desenho: ggplot2
#  EAPA / CatalyseR — Estatística Aplicada à Pesca e Aquicultura
# ============================================================================

library(agricolae)
library(ggplot2)
library(dplyr)


# ---- 1. PARÂMETROS DO EXPERIMENTO ----------------------------------------

niveis_A <- c("A1", "A2", "A3")          # parcelas principais (Fator A)
niveis_B <- c("B1", "B2", "B3", "B4")    # subparcelas        (Fator B)
n_blocos <- 3                             # blocos (repetições)
semente  <- 2026                          # reprodutibilidade


# ---- 2. SORTEIO COM agricolae --------------------------------------------
# design.split() com design = "rcbd" sorteia o Fator A em blocos
# casualizados e o Fator B dentro de cada parcela principal.

plano <- design.split(
  trt1   = niveis_A,
  trt2   = niveis_B,
  r      = n_blocos,
  design = "rcbd",
  seed   = semente
)

# Padronizar nomes das colunas (plots, splots, block, A, B)
livro <- plano$book
names(livro) <- c("parcela", "subparcela", "bloco", "FatorA", "FatorB")
livro$bloco <- as.integer(as.character(livro$bloco))

# Mostrar a ordem sorteada do Fator A em cada bloco
livro |>
  dplyr::distinct(bloco, parcela, FatorA) |>
  dplyr::group_by(bloco) |>
  dplyr::summarise(
    ordem_FatorA = paste(FatorA, collapse = " → "),
    .groups = "drop"
  ) |>
  print()


# ---- 3. FUNÇÃO DE DESENHO ESQUEMÁTICO ------------------------------------

desenhar_split_plot <- function(
    livro,
    cor_borda = c(A1 = "#62B6B7",   # SEAFOAM
                  A2 = "#2E7D8F",   # TEAL
                  A3 = "#E89B3C"),  # AMBER
    cor_fundo = c(A1 = "#D4ECEC",
                  A2 = "#C9DDE4",
                  A3 = "#FCE5C2"),
    titulo    = "Delineamento em Parcelas Subdivididas",
    subtitulo = "Sorteio: agricolae::design.split() — RCBD para o Fator A"
) {

  # Índices de posição: linha = parcela principal; coluna = subparcela
  dados <- livro |>
    dplyr::arrange(bloco, parcela, subparcela) |>
    dplyr::group_by(bloco) |>
    dplyr::mutate(pos_principal = dplyr::dense_rank(parcela)) |>
    dplyr::group_by(bloco, parcela) |>
    dplyr::mutate(pos_sub = dplyr::row_number()) |>
    dplyr::ungroup()

  n_sub <- max(dados$pos_sub)
  n_pp  <- max(dados$pos_principal)

  ggplot(dados) +
    # Retângulo da parcela principal (fundo colorido)
    geom_rect(
      aes(xmin = 0.4, xmax = n_sub + 0.6,
          ymin = -pos_principal - 0.45,
          ymax = -pos_principal + 0.45,
          fill = FatorA),
      color = NA
    ) +
    # Retângulo das subparcelas (borda tracejada)
    geom_rect(
      aes(xmin = pos_sub - 0.4, xmax = pos_sub + 0.4,
          ymin = -pos_principal - 0.32,
          ymax = -pos_principal + 0.32,
          color = FatorA),
      fill = "white", linetype = "dashed", linewidth = 0.5
    ) +
    # Rótulo do Fator A (à esquerda da linha)
    geom_text(
      aes(x = -0.05, y = -pos_principal,
          label = FatorA, color = FatorA),
      fontface = "bold", size = 4.5
    ) +
    # Rótulo do Fator B (dentro de cada subparcela)
    geom_text(
      aes(x = pos_sub, y = -pos_principal,
          label = FatorB, color = FatorA),
      size = 3.4, fontface = "bold"
    ) +
    facet_wrap(
      ~ bloco,
      labeller = labeller(bloco = function(x) paste("Bloco", x))
    ) +
    scale_fill_manual(values  = cor_fundo) +
    scale_color_manual(values = cor_borda) +
    coord_fixed(
      xlim = c(-0.4, n_sub + 0.7),
      ylim = c(-(n_pp + 0.6), -0.4)
    ) +
    theme_void(base_size = 11) +
    theme(
      strip.text       = element_text(face = "bold", size = 12,
                                       color = "white",
                                       margin = margin(5, 5, 5, 5)),
      strip.background = element_rect(fill = "#0F3B5F", color = NA),
      legend.position  = "none",
      panel.spacing    = unit(1.2, "lines"),
      plot.title       = element_text(face = "bold", size = 15, hjust = 0.5,
                                       color = "#0F3B5F",
                                       margin = margin(b = 4)),
      plot.subtitle    = element_text(size = 10, hjust = 0.5,
                                       color = "#555555",
                                       margin = margin(b = 14)),
      plot.margin      = margin(15, 15, 15, 15)
    ) +
    labs(title = titulo, subtitle = subtitulo)
}


# ---- 4. GERAR E SALVAR O DESENHO -----------------------------------------

p <- desenhar_split_plot(livro)
print(p)

# Exportar como PNG (descomente para salvar):
# ggsave("split_plot_sorteado.png", p,
#        width = 11, height = 4.5, dpi = 300, bg = "white")


# ============================================================================
#  OBSERVAÇÕES DIDÁTICAS
# ----------------------------------------------------------------------------
#  • design.split() executa DOIS sorteios independentes:
#      1) níveis de A entre as parcelas principais de cada bloco (RCBD);
#      2) níveis de B entre as subparcelas dentro de cada parcela principal.
#  • Cada execução com `seed` diferente gera uma nova realização legítima
#    do delineamento — útil para mostrar aos alunos que o esquema visual
#    NÃO é único: é apenas uma das muitas aleatorizações possíveis.
#  • Para inspecionar o plano completo: View(livro) ou print(livro).
# ============================================================================
