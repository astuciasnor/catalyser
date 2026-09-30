# Rodar de inst/app. Nomes explícitos não perdem palavras; bases legadas não mudam.
`%||%` <- function(x, y) if (is.null(x)) y else x
source("modules/exportacao_comunicacao.R", encoding = "UTF-8")
stopifnot(
  identical(exportacao_nome_projeto("Massa de peixes por lago"), "massa_de_peixes_por_lago"),
  exportacao_nome_projeto("Massa de peixes") != exportacao_nome_projeto("Massa de bagres"),
  identical(exportacao_nome_projeto("CON"), "con_analise"),
  identical(exportacao_nome_projeto("../../Dados brutos"), "dados_brutos"),
  identical(exportacao_nome_projeto(""), "analise"),
  identical(exportacao_nome_curto("Massa de peixes por lago"), "massa_de"),
  inherits(try(exportacao_nome_projeto(strrep("a", 81)), silent = TRUE), "try-error")
)
cat("OK: nomes completos, reservados, caminhos, limite e compatibilidade legada.\n")
