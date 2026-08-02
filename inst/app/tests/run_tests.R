# =============================================================================
# Suíte de testes automatizados da CatalyseR
# -----------------------------------------------------------------------------
# Uso:
#   Rscript inst/app/tests/run_tests.R                # diagnóstico + testes
#   Rscript inst/app/tests/run_tests.R --diagnostico  # só o diagnóstico
#   Rscript inst/app/tests/run_tests.R --estrito      # lacuna de ambiente falha
#
# Pode ser chamado de qualquer pasta: o script se localiza sozinho.
#
# Regra desta suíte: nada de pulo silencioso. Alguns testes dependem de coisas
# que podem faltar na máquina (o Quarto, o dataset de treino). Quando faltam, a
# verificação NÃO acontece — e isso é dito em voz alta duas vezes, no
# diagnóstico e no resumo final, sempre com a solução ao lado.
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)
modo_estrito <- any(args %in% c("--estrito", "--exigir-ambiente"))
so_diagnostico <- any(args %in% c("--diagnostico", "--diagnostic"))

# ---- Localização --------------------------------------------------------------
# Inclui a pasta do próprio script, para que rodar da pasta errada não seja erro.
localizar_app <- function() {
  arquivo <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  raiz_script <- if (length(arquivo)) {
    caminho <- normalizePath(sub("^--file=", "", arquivo[[1]]),
                             winslash = "/", mustWork = FALSE)
    dirname(dirname(caminho))   # .../inst/app/tests/run_tests.R -> .../inst/app
  } else NULL

  candidatos <- c(
    ".",
    file.path("inst", "app"),
    file.path("..", "inst", "app"),
    file.path("catalyser", "inst", "app"),
    raiz_script
  )
  candidatos <- candidatos[!is.na(candidatos) & nzchar(candidatos)]
  encontrados <- candidatos[file.exists(file.path(candidatos, "app.R"))]
  if (!length(encontrados)) {
    stop(paste0(
      "Não encontrei inst/app/app.R a partir de '", getwd(), "'.\n",
      "  Solução: rode a partir da raiz do pacote (a pasta com DESCRIPTION),\n",
      "  ou informe o caminho completo do run_tests.R."
    ), call. = FALSE)
  }
  normalizePath(encontrados[[1]], winslash = "/", mustWork = TRUE)
}

app_dir <- localizar_app()
setwd(app_dir)

# =============================================================================
# DIAGNÓSTICO DE AMBIENTE
# =============================================================================

linha <- function(char = "-") cat(strrep(char, 70), "\n", sep = "")
titulo <- function(txt) { cat("\n"); linha("="); cat(txt, "\n"); linha("=") }

# ---- 1. Metadados do pacote ---------------------------------------------------
preflight_descricao <- function() {
  candidatos <- file.path(c(".", "..", file.path("..", "..")), "DESCRIPTION")
  encontrado <- candidatos[file.exists(candidatos)]
  if (!length(encontrado)) return(NULL)
  tryCatch(read.dcf(encontrado[[1]]), error = function(e) NULL)
}

preflight_lista <- function(descricao, campo, limpar = TRUE) {
  if (is.null(descricao) || !campo %in% colnames(descricao)) return(character())
  bruto <- strsplit(descricao[1, campo], ",", fixed = TRUE)[[1]]
  saida <- if (limpar) trimws(gsub("\\(.*?\\)", "", bruto)) else trimws(bruto)
  saida[nzchar(saida)]
}

descricao <- preflight_descricao()

# ---- 2. Estado de cada pacote -------------------------------------------------
# Três estados possíveis, com soluções diferentes:
#   ok       -> instalado e carrega;
#   ausente  -> não está instalado;
#   quebrado -> está instalado mas falha ao carregar (instalação corrompida,
#               dependência de sistema faltando, versão incompatível).
preflight_pacote <- function(nome) {
  if (isTRUE(requireNamespace(nome, quietly = TRUE))) {
    return(list(nome = nome, estado = "ok", detalhe = as.character(utils::packageVersion(nome))))
  }
  instalado <- length(tryCatch(find.package(nome, quiet = TRUE),
                               error = function(e) character())) > 0L
  if (!instalado) return(list(nome = nome, estado = "ausente", detalhe = ""))
  motivo <- tryCatch({ loadNamespace(nome); "carregou na segunda tentativa" },
                     error = function(e) conditionMessage(e))
  list(nome = nome, estado = "quebrado", detalhe = motivo)
}

# ---- 3. Quarto ----------------------------------------------------------------
# A ordem segue a do pacote `quarto` do R: QUARTO_PATH e depois o PATH. Se as
# duas falharem, procuramos nos locais conhecidos de instalação — inclusive o
# Quarto embutido no RStudio, que o instalador NÃO coloca no PATH.
preflight_quarto <- function() {
  variavel <- Sys.getenv("QUARTO_PATH", "")
  if (nzchar(variavel) && file.exists(variavel)) {
    return(list(estado = "ok", caminho = variavel, origem = "variável QUARTO_PATH"))
  }
  no_path <- unname(Sys.which("quarto"))
  if (nzchar(no_path)) {
    return(list(estado = "ok", caminho = no_path, origem = "PATH"))
  }
  local_app <- Sys.getenv("LOCALAPPDATA", "")
  arquivos_prog <- Sys.getenv("ProgramFiles", "C:/Program Files")
  candidatos <- c(
    file.path(local_app, "Programs", "Quarto", "bin", "quarto.exe"),
    file.path(arquivos_prog, "Quarto", "bin", "quarto.exe"),
    file.path(arquivos_prog, "RStudio", "resources", "app", "bin", "quarto", "bin", "quarto.exe"),
    file.path(local_app, "Programs", "RStudio", "resources", "app", "bin", "quarto", "bin", "quarto.exe"),
    "/usr/local/bin/quarto", "/opt/quarto/bin/quarto"
  )
  candidatos <- candidatos[nzchar(candidatos)]
  achados <- candidatos[file.exists(candidatos)]
  if (length(achados)) {
    return(list(estado = "fora_do_path", caminho = achados[[1]], origem = "disco"))
  }
  list(estado = "ausente", caminho = "", origem = "")
}

preflight_versao_quarto <- function(caminho) {
  if (!nzchar(caminho)) return("")
  tryCatch(
    paste(system2(caminho, "--version", stdout = TRUE, stderr = TRUE), collapse = " "),
    error = function(e) "versão não obtida"
  )
}

# =============================================================================
# Execução do diagnóstico
# =============================================================================

titulo("CATALYSER — DIAGNÓSTICO DE AMBIENTE")
cat("Pasta do app : ", app_dir, "\n", sep = "")
cat("Rodado em    : ", format(Sys.time(), "%d/%m/%Y %H:%M:%S"), "\n", sep = "")

problemas <- character()   # impedem rodar
lacunas <- list()          # não impedem, mas deixam buracos na verificação

# ---- R ------------------------------------------------------------------------
r_minimo <- {
  bruto <- preflight_lista(descricao, "Depends", limpar = FALSE)
  alvo <- grep("^R\\s*\\(", bruto, value = TRUE)
  if (length(alvo)) gsub("[^0-9.]", "", alvo[[1]]) else ""
}
cat("\n[1] R\n")
cat("    Versão instalada : ", as.character(getRversion()), "\n", sep = "")
cat("    Versão exigida   : ", if (nzchar(r_minimo)) paste(">=", r_minimo) else "(não declarada)", "\n", sep = "")
cat("    Executável       : ", R.home("bin"), "\n", sep = "")
if (nzchar(r_minimo) && getRversion() < r_minimo) {
  cat("    ESTADO           : FALHA — versão abaixo da exigida\n")
  cat("    Solução          : instale um R >= ", r_minimo, " e rode de novo.\n", sep = "")
  problemas <- c(problemas, "versão do R")
} else {
  cat("    ESTADO           : OK\n")
}

# ---- Pacotes ------------------------------------------------------------------
pacotes <- setdiff(preflight_lista(descricao, "Imports"), "R")
cat("\n[2] PACOTES (Imports do DESCRIPTION)\n")
if (!length(pacotes)) {
  cat("    ESTADO           : não foi possível ler o DESCRIPTION\n")
} else {
  estados <- lapply(pacotes, preflight_pacote)
  ausentes <- Filter(function(x) identical(x$estado, "ausente"), estados)
  quebrados <- Filter(function(x) identical(x$estado, "quebrado"), estados)
  cat("    Exigidos         : ", length(pacotes), "\n", sep = "")
  cat("    Instalados e OK  : ", length(pacotes) - length(ausentes) - length(quebrados), "\n", sep = "")
  cat("    Ausentes         : ", length(ausentes), "\n", sep = "")
  cat("    Com falha ao carregar : ", length(quebrados), "\n", sep = "")

  if (length(ausentes)) {
    cat("\n    NÃO INSTALADOS:\n")
    for (p in ausentes) cat("      - ", p$nome, "\n", sep = "")
    do_cran <- setdiff(vapply(ausentes, `[[`, character(1), "nome"), "EAPADados")
    cat("\n    Solução — cole no Console do R:\n")
    if (length(do_cran)) {
      cat("      install.packages(c(", paste(sprintf('"%s"', do_cran), collapse = ", "), "))\n", sep = "")
    }
    if ("EAPADados" %in% vapply(ausentes, `[[`, character(1), "nome")) {
      cat("      # EAPADados não está no CRAN:\n")
      cat("      remotes::install_github(\"astuciasnor/EAPADados\")\n")
      cat("      # ou, a partir do .tar.gz que acompanha o projeto:\n")
      cat("      # install.packages(\"EAPADados_x.y.z.tar.gz\", repos = NULL, type = \"source\")\n")
    }
    problemas <- c(problemas, sprintf("%d pacote(s) ausente(s)", length(ausentes)))
  }

  if (length(quebrados)) {
    cat("\n    INSTALADOS MAS COM FALHA AO CARREGAR:\n")
    for (p in quebrados) {
      cat("      - ", p$nome, "\n", sep = "")
      cat("        motivo: ", p$detalhe, "\n", sep = "")
    }
    cat("\n    Solução — reinstale os pacotes acima:\n")
    cat("      install.packages(c(",
        paste(sprintf('"%s"', vapply(quebrados, `[[`, character(1), "nome")), collapse = ", "),
        "))\n", sep = "")
    cat("      Se o erro citar outro pacote, reinstale esse também.\n")
    cat("      Feche todas as sessões de R antes: no Windows, um pacote em uso\n")
    cat("      não pode ser sobrescrito.\n")
    problemas <- c(problemas, sprintf("%d pacote(s) quebrado(s)", length(quebrados)))
  }

  if (!length(ausentes) && !length(quebrados)) cat("    ESTADO           : OK\n")
}

# ---- Quarto -------------------------------------------------------------------
cat("\n[3] QUARTO (necessário para gerar o Word)\n")
q <- preflight_quarto()
if (identical(q$estado, "ok")) {
  cat("    Encontrado em    : ", q$caminho, "\n", sep = "")
  cat("    Descoberto por   : ", q$origem, "\n", sep = "")
  cat("    Versão           : ", preflight_versao_quarto(q$caminho), "\n", sep = "")
  cat("    ESTADO           : OK\n")
} else if (identical(q$estado, "fora_do_path")) {
  cat("    Encontrado em    : ", q$caminho, "\n", sep = "")
  cat("    ESTADO           : INSTALADO, MAS FORA DO PATH\n")
  # Bloco impresso de uma vez, em ASCII: menos chamadas e nenhum caractere
  # especial que o console do Windows possa engolir.
  # A causa provável depende de ONDE o executável foi encontrado: o embutido do
  # RStudio nunca entra no PATH; um Quarto autônomo fora do PATH indica sessão
  # aberta antes da instalação.
  embutido_rstudio <- grepl("RStudio", q$caminho, fixed = TRUE)
  writeLines(c(
    "    Causa provavel:",
    if (embutido_rstudio) {
      "      - e o Quarto EMBUTIDO no RStudio, que nunca entra no PATH."
    } else {
      "      - e um Quarto autonomo; o terminal foi aberto ANTES de instala-lo."
    },
    "",
    "    Solucao rapida (apontar a variavel para o executavel encontrado):",
    "      No PowerShell, permanente para o seu usuario:",
    paste0("        [Environment]::SetEnvironmentVariable('QUARTO_PATH', '",
           gsub("/", "\\\\", q$caminho), "', 'User')"),
    "      Depois FECHE e reabra o terminal.",
    "",
    "    Solucao limpa (recomendada):",
    "      Instale o Quarto CLI de https://quarto.org/docs/get-started/",
    "      O instalador do Windows adiciona ao PATH, mas isso so vale para",
    "      terminais abertos DEPOIS da instalacao."
  ))
  lacunas[[length(lacunas) + 1L]] <- list(
    o_que = "Render do relatório em Word",
    porque = "o Quarto existe no disco mas não está no PATH nem em QUARTO_PATH",
    onde = "test_anova_exportacao.R e test_exportacao_comunicacao.R pulam essa etapa",
    solucao = sprintf("defina QUARTO_PATH=%s e reabra o terminal", q$caminho)
  )
} else {
  cat("    ESTADO           : NÃO ENCONTRADO\n")
  cat("    Procurei em      : QUARTO_PATH, PATH e nos locais padrão de instalação\n")
  cat("    Solução          : instale de https://quarto.org/docs/get-started/\n")
  cat("                       e reabra o terminal depois de instalar.\n")
  lacunas[[length(lacunas) + 1L]] <- list(
    o_que = "Render do relatório em Word",
    porque = "o Quarto CLI não foi encontrado em lugar nenhum",
    onde = "test_anova_exportacao.R e test_exportacao_comunicacao.R pulam essa etapa",
    solucao = "instale o Quarto (https://quarto.org/docs/get-started/) e reabra o terminal"
  )
}

# ---- Dados de teste -----------------------------------------------------------
dataset_treino <- file.path("dados", "Treino-Transformacoes.xlsx")
cat("\n[4] DADOS DE TESTE\n")
cat("    Arquivo          : ", dataset_treino, "\n", sep = "")
if (file.exists(dataset_treino)) {
  cat("    ESTADO           : OK\n")
} else {
  cat("    ESTADO           : AUSENTE\n")
  cat("    Solução          : restaure o arquivo em inst/app/dados/ a partir do repositório.\n")
  lacunas[[length(lacunas) + 1L]] <- list(
    o_que = "Benchmarks estatísticos da ANOVA (n = 68, F = 2,831, p = 0,0318)",
    porque = sprintf("'%s' não foi encontrado", dataset_treino),
    onde = "test_anova_integrada.R pula a conferência contra o dataset real",
    solucao = "restaure inst/app/dados/Treino-Transformacoes.xlsx"
  )
}

# ---- Veredito do diagnóstico ---------------------------------------------------
titulo("VEREDITO DO AMBIENTE")
if (length(problemas)) {
  cat("IMPEDITIVO: ", paste(problemas, collapse = "; "), ".\n", sep = "")
  cat("Os testes não vão rodar. Resolva o que está acima e tente de novo.\n")
  linha("=")
  stop("Ambiente incompleto.", call. = FALSE)
}
if (length(lacunas)) {
  cat("UTILIZÁVEL, COM LACUNAS. Estas verificações NÃO serão feitas:\n")
  for (l in lacunas) cat("  - ", l$o_que, " (", l$porque, ")\n", sep = "")
  if (isTRUE(modo_estrito)) {
    cat("\nModo --estrito: lacuna de ambiente é falha.\n")
    for (l in lacunas) cat("  Solução: ", l$solucao, "\n", sep = "")
    linha("=")
    stop("Ambiente incompleto para homologação.", call. = FALSE)
  }
} else {
  cat("COMPLETO: nenhuma verificação será pulada.\n")
}
linha("=")

if (isTRUE(so_diagnostico)) {
  cat("\nModo --diagnostico: os testes não foram executados.\n")
  quit(status = 0L)
}

# =============================================================================
# SINTAXE
# =============================================================================
titulo("SINTAXE DOS ARQUIVOS R")
arquivos_r <- list.files(".", pattern = "[.]R$", recursive = TRUE, full.names = TRUE)
erros_parse <- vapply(
  arquivos_r,
  function(arquivo) tryCatch({
    parse(arquivo)
    ""
  }, error = function(e) conditionMessage(e)),
  character(1)
)
if (any(nzchar(erros_parse))) {
  print(erros_parse[nzchar(erros_parse)])
  stop("Há arquivos R com erro de sintaxe.", call. = FALSE)
}
cat("[OK] Sintaxe de ", length(arquivos_r), " arquivos R.\n", sep = "")

# =============================================================================
# TESTES
# =============================================================================
testes <- c(
  "test_bases_derivadas.R",
  "test_menu_preparando_dados.R",
  "test_pivotar_organizar.R",
  "test_organizar_variaveis.R",
  "test_registro_execucoes.R",
  "test_execucao_explicita.R",
  "test_estados_execucao.R",
  "test_logisticas_separadas.R",
  "test_comunicacao_resultados.R",
  "test_funcoes_analise.R",
  "test_exportacao_comunicacao.R",
  "test_anova_integrada.R",
  "test_anova_exportacao.R",
  "test_grafico_linhas_troca_y.R"
)

faltando <- testes[!file.exists(file.path("tests", testes))]
if (length(faltando)) {
  stop(sprintf("Testes ausentes: %s", paste(faltando, collapse = ", ")), call. = FALSE)
}

rscript <- file.path(R.home("bin"), "Rscript")
if (.Platform$OS.type == "windows") rscript <- paste0(rscript, ".exe")

titulo(sprintf("EXECUTANDO %d ARQUIVOS DE TESTE", length(testes)))
for (teste in testes) {
  expressao <- sprintf("source(%s)", dQuote(file.path("tests", teste)))
  status <- system2(rscript, c("-e", shQuote(expressao)))
  if (!identical(status, 0L)) {
    stop(sprintf("Falha em %s (status %s).", teste, status), call. = FALSE)
  }
  cat("[OK] ", teste, "\n", sep = "")
}

# =============================================================================
# RESUMO FINAL
# =============================================================================
titulo("RESUMO")
cat("Todos os ", length(testes), " arquivos de teste passaram.\n", sep = "")

if (length(lacunas)) {
  linha("-")
  cat("MAS ATENÇÃO: a suíte passou COM LACUNAS. Isto NÃO foi verificado:\n")
  linha("-")
  for (l in lacunas) {
    cat("\n  * ", l$o_que, "\n", sep = "")
    cat("    Motivo  : ", l$porque, "\n", sep = "")
    cat("    Efeito  : ", l$onde, "\n", sep = "")
    cat("    Solução : ", l$solucao, "\n", sep = "")
  }
  cat("\n  Um \"passou\" com lacuna não homologa uma instalação.\n")
  cat("  Para exigir o ambiente completo, rode com --estrito.\n")
} else {
  cat("Ambiente completo: nenhuma verificação foi pulada.\n")
}
linha("=")
