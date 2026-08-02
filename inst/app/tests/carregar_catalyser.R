# =============================================================================
# carregar_catalyser.R — deixa as funções de análise disponíveis para os testes
# -----------------------------------------------------------------------------
# As funções `catalyser_*` vivem em R/ e são exportadas pelo pacote. Um teste
# pode rodar em dois momentos:
#
#   - DEPOIS de instalar  -> usa o pacote instalado, que é o que o aluno terá;
#   - ANTES de instalar   -> carrega os arquivos de R/ direto do código-fonte.
#
# Este helper tenta a primeira via e cai para a segunda, dizendo em voz alta
# qual usou. Assim o mesmo teste serve para o ciclo rápido de desenvolvimento e
# para conferir a instalação de verdade.
# =============================================================================

carregar_catalyser <- function() {
  if (isTRUE(requireNamespace("catalyser", quietly = TRUE))) {
    versao <- tryCatch(as.character(utils::packageVersion("catalyser")),
                       error = function(e) "?")
    # `attachNamespace` falha se já estiver anexado; `library` é idempotente.
    suppressMessages(library("catalyser", character.only = TRUE))
    cat(sprintf("[carregar] pacote catalyser %s instalado.\n", versao))
    return(invisible("instalado"))
  }

  # Sem o pacote instalado: procurar a pasta R/ subindo a partir de inst/app.
  candidatos <- c(file.path("..", "..", "R"), file.path("..", "R"), "R")
  encontrado <- candidatos[dir.exists(candidatos)]
  encontrado <- encontrado[
    vapply(encontrado, function(d) length(list.files(d, pattern = "[.]R$")) > 0L,
           logical(1))
  ]
  if (!length(encontrado)) {
    stop(
      paste0(
        "Não encontrei as funções de análise.\n",
        "  Solução: instale o pacote com  remotes::install_local('.')  ou\n",
        "  rode os testes a partir de inst/app dentro do repositório."
      ),
      call. = FALSE
    )
  }
  arquivos <- list.files(encontrado[[1]], pattern = "[.]R$", full.names = TRUE)
  # `run_app.R` depende do shiny instalado como pacote; aqui só interessam as
  # funções de análise.
  arquivos <- arquivos[!grepl("run_app[.]R$", arquivos)]
  for (arquivo in arquivos) sys.source(arquivo, envir = globalenv())
  cat(sprintf("[carregar] pacote NÃO instalado: usando o código-fonte de %s (%d arquivo(s)).\n",
              normalizePath(encontrado[[1]], winslash = "/"), length(arquivos)))
  invisible("fonte")
}

carregar_catalyser()
