# =============================================================================
# instalar_dependencias.R — rode ISTO uma vez antes de abrir a CatalyseR.
#
#   source("instalar_dependencias.R")
#
# Seguro rodar de novo: instala apenas o que falta.
#
# A lista de pacotes NAO e escrita a mao: vem do campo Imports do DESCRIPTION,
# que e a fonte da verdade. Uma lista paralela desatualiza em silencio e a falha
# so aparece numa maquina limpa, que e o pior lugar para descobrir.
# =============================================================================

localizar_descricao <- function() {
  candidatos <- file.path(c(".", "catalyser", "..", file.path("..", "..")), "DESCRIPTION")
  encontrado <- candidatos[file.exists(candidatos)]
  if (!length(encontrado)) {
    stop(paste0(
      "Nao encontrei o DESCRIPTION a partir de '", getwd(), "'.\n",
      "  Solucao: rode este script a partir da pasta do pacote catalyser."
    ), call. = FALSE)
  }
  encontrado[[1]]
}

descricao <- read.dcf(localizar_descricao())

campo_pacotes <- function(campo) {
  if (!campo %in% colnames(descricao)) return(character())
  bruto <- strsplit(descricao[1, campo], ",", fixed = TRUE)[[1]]
  nomes <- trimws(gsub("\\(.*?\\)", "", bruto))
  setdiff(nomes[nzchar(nomes)], "R")
}

exigidos <- campo_pacotes("Imports")

# --- 1. Versao do R -----------------------------------------------------------
r_minimo <- {
  if ("Depends" %in% colnames(descricao)) {
    alvo <- grep("^\\s*R\\s*\\(",
                 strsplit(descricao[1, "Depends"], ",", fixed = TRUE)[[1]], value = TRUE)
    if (length(alvo)) gsub("[^0-9.]", "", alvo[[1]]) else ""
  } else ""
}
if (nzchar(r_minimo) && getRversion() < r_minimo) {
  stop(sprintf("Este R e %s, mas a CatalyseR exige >= %s. Atualize o R primeiro.",
               getRversion(), r_minimo), call. = FALSE)
}
cat("R          :", as.character(getRversion()),
    if (nzchar(r_minimo)) sprintf("(exigido >= %s) OK", r_minimo) else "OK", "\n")
cat("Pacotes    :", length(exigidos), "exigidos pelo DESCRIPTION\n")

# --- 2. CRAN ------------------------------------------------------------------
# EAPADados nao esta no CRAN; e tratado no passo 3.
do_cran <- setdiff(exigidos, "EAPADados")
faltando <- do_cran[!vapply(do_cran, requireNamespace, logical(1), quietly = TRUE)]

if (length(faltando)) {
  cat("\nInstalando do CRAN:", paste(faltando, collapse = ", "), "\n")
  # Binario primeiro: quem so quer usar a IDE nao precisa de Rtools.
  try(install.packages(faltando, type = "binary"))
  ainda <- faltando[!vapply(faltando, requireNamespace, logical(1), quietly = TRUE)]
  if (length(ainda)) {
    cat("\nSem binario disponivel para:", paste(ainda, collapse = ", "), "\n")
    cat("Tentando compilar da fonte (aqui o Rtools pode ser necessario)...\n")
    install.packages(ainda)
  }
} else {
  cat("\nCRAN       : os", length(do_cran), "pacotes ja estao instalados.\n")
}

# --- 3. EAPADados (GitHub) ----------------------------------------------------
if ("EAPADados" %in% exigidos && !requireNamespace("EAPADados", quietly = TRUE)) {
  cat("\nInstalando EAPADados (nao esta no CRAN)...\n")
  if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
  remotes::install_github("astuciasnor/EAPADados")
}

# --- 4. Opcionais -------------------------------------------------------------
# Fora do Imports de proposito: a IDE funciona sem eles e sao pesados.
opcionais <- list(
  "Series temporais" = c("tsibble", "feasts", "fabletools"),
  "Mapas (podem exigir GDAL)" = c("sf", "geobr", "ggspatial", "leaflet")
)
cat("\n--- Opcionais ---------------------------------------------------------\n")
for (grupo in names(opcionais)) {
  pacotes <- opcionais[[grupo]]
  ausentes <- pacotes[!vapply(pacotes, requireNamespace, logical(1), quietly = TRUE)]
  if (!length(ausentes)) {
    cat(grupo, ": presente\n", sep = "")
  } else {
    cat(grupo, ": faltam ", paste(ausentes, collapse = ", "), "\n", sep = "")
    cat("  install.packages(c(",
        paste(sprintf('"%s"', ausentes), collapse = ", "), "))\n", sep = "")
  }
}

# --- 5. Quarto ----------------------------------------------------------------
# Mesma logica do diagnostico da suite: o Quarto embutido no RStudio nao entra
# no PATH, entao "ausente" e "fora do PATH" sao problemas diferentes.
quarto_localizar <- function() {
  variavel <- Sys.getenv("QUARTO_PATH", "")
  if (nzchar(variavel) && file.exists(variavel)) return(list(estado = "ok", caminho = variavel))
  no_path <- unname(Sys.which("quarto"))
  if (nzchar(no_path)) return(list(estado = "ok", caminho = no_path))
  local_app <- Sys.getenv("LOCALAPPDATA", "")
  arquivos_prog <- Sys.getenv("ProgramFiles", "C:/Program Files")
  candidatos <- c(
    file.path(local_app, "Programs", "Quarto", "bin", "quarto.exe"),
    file.path(arquivos_prog, "Quarto", "bin", "quarto.exe"),
    file.path(arquivos_prog, "RStudio", "resources", "app", "bin", "quarto", "bin", "quarto.exe"),
    file.path(local_app, "Programs", "RStudio", "resources", "app", "bin", "quarto", "bin", "quarto.exe")
  )
  candidatos <- candidatos[nzchar(candidatos)]
  achados <- candidatos[file.exists(candidatos)]
  if (length(achados)) return(list(estado = "fora_do_path", caminho = achados[[1]]))
  list(estado = "ausente", caminho = "")
}

cat("\n--- Quarto (necessario para exportar .docx) ---------------------------\n")
q <- quarto_localizar()
if (identical(q$estado, "ok")) {
  cat("OK: ", q$caminho, "\n", sep = "")
} else if (identical(q$estado, "fora_do_path")) {
  writeLines(c(
    paste0("Instalado, mas FORA DO PATH: ", q$caminho),
    "",
    "Solucao rapida, no PowerShell:",
    paste0("  [Environment]::SetEnvironmentVariable('QUARTO_PATH', '",
           gsub("/", "\\\\", q$caminho), "', 'User')"),
    "Depois FECHE e reabra o RStudio e o terminal.",
    "",
    "Solucao limpa: instale o Quarto CLI de https://quarto.org/docs/get-started/"
  ))
} else {
  writeLines(c(
    "NAO ENCONTRADO.",
    "As analises funcionam sem ele, mas o relatorio .docx nao e gerado.",
    "Instale de https://quarto.org/docs/get-started/ e reabra o terminal."
  ))
}

cat("\n=======================================================================\n")
cat("Pronto. Para conferir o ambiente inteiro:\n")
cat("  Rscript inst/app/tests/run_tests.R --diagnostico\n")
cat("Para abrir a IDE:\n")
cat("  catalyser::run_app()\n")
cat("=======================================================================\n")
