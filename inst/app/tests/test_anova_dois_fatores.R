# Teste do núcleo canônico da ANOVA de dois fatores com o conjunto real
# incorporado ao EAPADados.

source("app.R", encoding = "UTF-8")

arquivo_salvelino <- file.path("..", "..", "..", "EAPADados", "data-raw",
                               "curados", "salvelino_formalina_remocao.csv")
stopifnot(file.exists(arquivo_salvelino))
salvelino <- read.csv(arquivo_salvelino, stringsAsFactors = FALSE)

stopifnot(
  is.null(anova2_validar_entrada(salvelino, "sobrevivencia_eclosao_pct",
                                 "formalin", "remocao_semanal")),
  nrow(salvelino) == 30L,
  all(salvelino$ovos_eclodidos + salvelino$mortalidade_total ==
        salvelino$ovos_iniciais)
)

estado_registro <- list(
  analise_id = "anova_dois_fatores",
  tipo = "anova_dois_fatores",
  titulo = "ANOVA de dois fatores",
  parametros = list(
    resposta = "sobrevivencia_eclosao_pct",
    fator_a = "formalin",
    fator_b = "remocao_semanal"
  ),
  saidas_disponiveis = c("narrativa", "celulas", "tabela", "efeito",
                         "comparacoes", "grafico", "grafico_combinacoes",
                         "pressupostos", "diagnosticos")
)
stopifnot(
  isTRUE(execucoes_validar_estado(estado_registro)),
  identical(
    unname(execucoes_rotulos_saidas[["grafico_combinacoes"]]),
    "Gráfico das combinações"
  ),
  identical(
    exportacao_sufixo_componente("anova_dois_fatores", "grafico_combinacoes"),
    "combinacoes"
  )
)

resultado <- calcular_anova_dois_fatores(
  salvelino,
  dep_var = "sobrevivencia_eclosao_pct",
  fator_a = "formalin",
  fator_b = "remocao_semanal"
)

tabela <- arrumar_tabela_anova_dois_fatores(resultado)
celulas <- arrumar_celulas_anova_dois_fatores(resultado)
pressupostos <- arrumar_pressupostos_anova_dois_fatores(resultado)
grafico_interacao <- grafico_anova_dois_fatores(resultado)
grafico_combinacoes <- grafico_combinacoes_anova_dois_fatores(resultado)

stopifnot(
  resultado$n == 30L,
  resultado$excluidos == 0L,
  !isTRUE(resultado$delineamento_balanceado),
  identical(as.integer(resultado$tamanhos_celula), c(12L, 8L, 3L, 7L)),
  nrow(tabela) == 5L,
  grepl("formalin", tabela[["Fonte de variação"]][1], fixed = TRUE),
  grepl("remocao_semanal", tabela[["Fonte de variação"]][2], fixed = TRUE),
  nrow(celulas) == 4L,
  all(c("Pressuposto", "Estatística", "p-valor") %in% names(pressupostos)),
  inherits(grafico_interacao, "ggplot"),
  inherits(grafico_combinacoes, "ggplot"),
  grepl("IC 95%", grafico_interacao$labels$subtitle, fixed = TRUE),
  grepl("IC 95%", grafico_combinacoes$labels$subtitle, fixed = TRUE),
  is.null(grafico_interacao$scales$get_scales("y")$limits),
  is.null(grafico_combinacoes$scales$get_scales("y")$limits),
  identical(grafico_interacao$labels$colour, "remocao_semanal"),
  identical(grafico_interacao$labels$shape, "remocao_semanal"),
  grepl("desequilibrado", resultado$narrativa, fixed = TRUE),
  grepl("remocao_semanal", resultado$narrativa, fixed = TRUE),
  is.character(resultado$console),
  inherits(resultado$modelo, "aov")
)

# O mesmo conjunto precisa ser reproduzível pelo pacote que o Projeto R usa,
# e não apenas pelo módulo Shiny. Carregamos o código-fonte local para que este
# teste continue útil antes da instalação de uma nova versão do pacote.
source(file.path("..", "..", "R", "analises.R"), encoding = "UTF-8")
execucao_anova2 <- list(
  id = "execucao_0001",
  tipo = "anova_dois_fatores",
  titulo = "ANOVA de dois fatores — salvelino",
  parametros = list(
    resposta = "sobrevivencia_eclosao_pct",
    fator_a = "formalin",
    fator_b = "remocao_semanal",
    nivel_confianca = 0.95
  )
)
replay <- catalyser_executar(execucao_anova2, salvelino)
stopifnot(
  is.data.frame(replay$celulas),
  nrow(replay$celulas) == 4L,
  is.data.frame(replay$efeito),
  nrow(replay$efeito) == 3L,
  is.data.frame(replay$tabela),
  nrow(replay$tabela) == 5L,
  inherits(replay$grafico, "ggplot"),
  inherits(replay$grafico_combinacoes, "ggplot"),
  grepl("IC 95%", replay$grafico$labels$subtitle, fixed = TRUE),
  grepl("IC 95%", replay$grafico_combinacoes$labels$subtitle, fixed = TRUE),
  inherits(replay$objeto, "aov")
)

cat("OK: ANOVA a dois fatores calculada e documentada com o salvelino real\n")
