# =============================================================================
#  CatalyseR - Instalador oficial  (EAPADados + catalyser)
#  Estatistica Aplicada a Pesca e Aquicultura com R
# -----------------------------------------------------------------------------
#  COMO USAR (super simples):
#    1) Abra ESTE arquivo no RStudio (duplo clique nele).
#    2) Clique no botao "Source" (canto superior direito do editor)  [ou Ctrl+Shift+S].
#    3) Espere os "ok". No fim, a CatalyseR abre sozinha.
#
#  So precisa de R >= 4.3 e internet. NAO precisa de git, conta no GitHub, nem
#  Rtools (os pacotes sao de R puro). Pode clicar em Source de novo quando quiser:
#  ele preserva os pacotes CRAN compatíveis, atualiza a IDE e a reabre.
# =============================================================================

instalar_catalyser <- function(iniciar = FALSE) {

  options(repos = c(CRAN = "https://cloud.r-project.org"))
  # NUNCA compilar da fonte (sem Rtools, sem o prompt "compilar?"): so binarios prontos.
  options(install.packages.check.source = "no")
  .win_mac <- .Platform$OS.type == "windows" || identical(Sys.info()[["sysname"]], "Darwin")
  tipo_pkg <- if (.win_mac) "binary" else getOption("pkgType")

  # Simbolos bonitos onde o terminal suporta UTF-8; ASCII caso contrario.
  utf8  <- isTRUE(l10n_info()[["UTF-8"]])
  OK    <- if (utf8) "✓" else "[ok]"
  FALHA <- if (utf8) "✗" else "[!!]"
  SETA  <- if (utf8) "→" else "->"
  barra <- function() cat(strrep("=", 65), "\n", sep = "")
  secao <- function(t) { cat("\n"); barra(); cat("  ", t, "\n", sep = ""); barra() }

  cat("\n"); barra()
  cat("            C a t a l y s e R   -   Instalador\n")
  cat("     Estatistica Aplicada a Pesca e Aquicultura com R\n")
  barra()

  # --- 1. Versao do R -------------------------------------------------------
  rv <- getRversion()
  cat(sprintf("\n%s Versao do R detectada: %s\n", SETA, rv))
  if (rv < "4.3.0") {
    cat(sprintf("\n%s O seu R e %s, mas a CatalyseR precisa de R >= 4.3.0.\n", FALHA, rv))
    cat("   Atualize o R em https://cran.r-project.org e rode este script de novo.\n\n")
    return(invisible(FALSE))
  }
  cat(sprintf("%s R compativel (>= 4.3.0).\n", OK))

  # helper: instala se faltar e, quando pedido, atualiza a partir do GitHub.
  # Usa packageVersion() para não carregar o namespace antes da reinstalação.
  garante <- function(pkg, github = NULL, versao_minima = NULL, atualizar = FALSE) {
    versao_instalada <- tryCatch(utils::packageVersion(pkg), error = function(e) NULL)
    atende_versao <- !is.null(versao_instalada) &&
      (is.null(versao_minima) || versao_instalada >= base::package_version(versao_minima))
    if (atende_versao && !isTRUE(atualizar)) {
      cat(sprintf("  %s %-14s ja instalado (%s)\n", OK, pkg, versao_instalada))
      return(TRUE)
    }
    acao <- if (is.null(versao_instalada)) "instalando" else "atualizando"
    cat(sprintf("  %s %-14s %s...\n", SETA, pkg, acao))
    res <- tryCatch({
      if (is.null(github)) install.packages(pkg, quiet = TRUE, type = tipo_pkg)
      else remotes::install_github(github, quiet = TRUE, upgrade = "never", force = TRUE)
      nova_versao <- utils::packageVersion(pkg)
      is.null(versao_minima) || nova_versao >= base::package_version(versao_minima)
    }, error = function(e) { cat("       ", conditionMessage(e), "\n", sep = ""); FALSE })
    cat(sprintf("  %s %-14s %s\n", if (isTRUE(res)) OK else FALHA, pkg,
                if (isTRUE(res)) "instalado" else "FALHOU"))
    isTRUE(res)
  }

  # --- 2. Ferramenta de instalacao (remotes) --------------------------------
  secao("1/4  Ferramenta de instalacao")
  tem_remotes <- garante("remotes")

  # --- 3. Pacotes essenciais (CRAN) -----------------------------------------
  secao("2/4  Pacotes essenciais (CRAN)")
  cran <- c("shiny","bslib","DT","ggplot2","readxl","readr","writexl","markdown","zip",
            "flextable","ggpubr","tibble","stringr","dplyr","tidyr","tidyselect",
            "scales","cowplot","ggrepel",
            "car","emmeans","effectsize","rstatix","rcompanion","vistributions")
  falhas_cran <- cran[!vapply(cran, garante, logical(1))]

  # --- 4. Dados e IDE (GitHub) ----------------------------------------------
  secao("3/4  Dados e IDE (GitHub)")
  ok_dados <- ok_ide <- FALSE
  if (!tem_remotes) {
    cat(sprintf("  %s 'remotes' nao instalou -> nao da para baixar do GitHub.\n", FALHA))
  } else {
    ok_dados <- garante(
      "EAPADados", github = "astuciasnor/EAPADados",
      versao_minima = "0.1.10"
    )
    # A IDE é sempre atualizada: assim executar novamente este instalador
    # realmente traz o conteúdo mais recente da branch main.
    ok_ide <- garante(
      "catalyser", github = "astuciasnor/catalyser",
      versao_minima = "0.1.4", atualizar = TRUE
    )
  }

  # --- 5. Extras opcionais (so avisa) ---------------------------------------
  secao("4/4  Extras opcionais (Mapas e Series Temporais)")
  op <- c("sf","geobr","ggspatial","leaflet","tsibble","feasts","fabletools")
  faltando_op <- op[!vapply(op, requireNamespace, logical(1), quietly = TRUE)]
  if (length(faltando_op))
    cat(sprintf("  %s Ausentes (opcional): %s\n     Instale com install.packages(...) so se for usar Mapas/Series.\n",
                SETA, paste(faltando_op, collapse = ", ")))
  else
    cat(sprintf("  %s Todos os extras presentes.\n", OK))
  if (nchar(Sys.which("quarto")) == 0)
    cat(sprintf("  %s Quarto CLI nao encontrado (opcional; so p/ exportar .docx): https://quarto.org\n", SETA))

  # --- Resumo ---------------------------------------------------------------
  secao("Resumo")
  falhou <- c(falhas_cran, if (!ok_dados) "EAPADados", if (!ok_ide) "catalyser")
  if (length(falhou) == 0) {
    cat(sprintf("\n  %s Tudo pronto! Para abrir a CatalyseR, rode:\n\n", OK))
    cat("      catalyser::run_app()\n\n")
    if (isTRUE(iniciar)) { cat("  Abrindo a IDE...\n\n"); try(catalyser::run_app()) }
    return(invisible(TRUE))
  }
  cat(sprintf("\n  %s Nao foi possivel instalar: %s\n\n", FALHA, paste(falhou, collapse = ", ")))
  cat("  Dicas:\n")
  cat("   - Se o erro mencionar 'Rtools': instale o Rtools (Windows) em\n")
  cat("     https://cran.r-project.org/bin/windows/Rtools/  e rode de novo.\n")
  cat("   - Sem internet ou atras de proxy: verifique a conexao e tente de novo.\n")
  cat("   - Rode este script novamente: ele preserva as dependencias compativeis e atualiza a IDE.\n\n")
  invisible(FALSE)
}

# Ao clicar em Source: instala o que falta e, se tudo der certo, abre a IDE.
instalar_catalyser(iniciar = TRUE)
