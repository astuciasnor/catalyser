# =============================================================================
# instalar_dependencias.R — rode ISTO uma vez ANTES de abrir a CatalyseR.
# Instala tudo o que a IDE precisa. Seguro rodar de novo (só instala o que falta).
#   source("instalar_dependencias.R")
# =============================================================================

# --- Pacotes de CRAN (essenciais) --------------------------------------------
cran <- c(
  "shiny", "bslib", "DT", "ggplot2", "readxl", "readr", "writexl",
  "markdown", "zip", "remotes",
  "flextable", "ggpubr", "tibble", "stringr", "dplyr", "tidyr", "tidyselect",
  "scales", "cowplot", "ggrepel",
  "car", "emmeans", "effectsize", "rstatix", "rcompanion", "vistributions"
)
faltando <- cran[!vapply(cran, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltando)) {
  message("Instalando (CRAN): ", paste(faltando, collapse = ", "))
  install.packages(faltando)
} else message("CRAN essencial: tudo presente.")

# --- Séries temporais (opcional) ---------------------------------------------
serie <- c("tsibble", "feasts", "fabletools")
faltando_s <- serie[!vapply(serie, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltando_s)) try(install.packages(faltando_s))

# --- Mapas (opcional; pesados, podem exigir GDAL no sistema) ------------------
mapas <- c("sf", "geobr", "ggspatial", "leaflet")
faltando_m <- mapas[!vapply(mapas, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltando_m)) {
  message("Mapas ausentes (opcional): ", paste(faltando_m, collapse = ", "),
          " — instale se for usar o menu Mapas.")
  # install.packages(faltando_m)   # descomente para instalar
}

# --- Dados do curso (GitHub) --------------------------------------------------
if (!requireNamespace("EAPADados", quietly = TRUE)) {
  message("Instalando EAPADados (GitHub)...")
  remotes::install_github("astuciasnor/EAPADados")
}

# --- Quarto (para exportar relatorios .docx) ---------------------------------
if (nchar(Sys.which("quarto")) == 0)
  message("Aviso: o Quarto CLI nao foi encontrado. Instale em https://quarto.org ",
          "se for exportar relatorios .docx (as analises funcionam sem ele).")

message("\nPronto! Agora rode:  catalyser::run_app()")
