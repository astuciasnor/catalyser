# Rodar a partir de inst/app: Rscript tests/test_contrato_tipos.R
# Teste isolado do contrato que bloqueia um ZIP com execução sem reconstrução.
Sys.setlocale("LC_CTYPE", "English_United States.utf8")
`%||%` <- function(a, b) if (is.null(a) || !length(a)) b else a
source(file.path("modules", "registro_execucoes.R"), encoding = "UTF-8")
source(file.path("modules", "mod_proporcoes.R"), encoding = "UTF-8")
source(file.path("modules", "exportacao_comunicacao.R"), encoding = "UTF-8")

tipos <- vapply(c("uma", "duas", "aderencia"), proporcoes_tipo_execucao, character(1))
stopifnot(identical(tipos, c(
  uma = "proporcao_uma", duas = "proporcao_duas",
  aderencia = "qui_quadrado_aderencia"
)))
stopifnot(all(vapply(tipos, execucoes_tipo_reconstruivel, logical(1))))

manifesto <- list(execucoes = list(execucao_0001 = list(
  tipo = "proporcao_uma", estado_dependencia = "Atualizada",
  incluir_word = FALSE, saidas_word = character()
)))
stopifnot(exportacao_validar_manifesto(manifesto)$ok)
manifesto$execucoes$execucao_0001$tipo <- "frequencias_proporcoes"
validacao <- exportacao_validar_manifesto(manifesto)
stopifnot(
  !validacao$ok,
  any(grepl("execucao_0001 (frequencias_proporcoes)", validacao$mensagens, fixed = TRUE))
)
cat("OK: tipos das proporções e bloqueio de execução sem replay\n")
