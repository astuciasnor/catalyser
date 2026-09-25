# Executar a partir de catalyser/inst/app.
library(shiny)

source(file.path("..", "..", "R", "anova_mista.R"), encoding = "UTF-8")
source(file.path("modules", "ficha_planejamento.R"), encoding = "UTF-8")
source(file.path("modules", "registro_execucoes.R"), encoding = "UTF-8")
source(file.path("modules", "mod_execucao_explicita.R"), encoding = "UTF-8")
source(file.path("modules", "mod_anova_mista.R"), encoding = "UTF-8")
source(file.path("modules", "mod_planejamento_variaveis.R"), encoding = "UTF-8")
# Este arquivo é UTF-8; o source sem argumento segue a mesma forma usada no app.
source(file.path("modules", "mod_planejamento_observacional.R"))

arquivo <- file.path("..", "..", "..", "EAPACadernos", "projeto_anova_mista",
                     "dados", "brutos", "arrastos.csv")
dados <- utils::read.csv(arquivo, fileEncoding = "UTF-8")
parametros <- list(
  resposta = "biomassa_g", fator = "condicao",
  unidade = "praia", subamostra = "arrasto"
)

resultado <- catalyser_anova_mista(dados, parametros)
stopifnot(
  resultado$n == 27L,
  resultado$n_unidades == 9L,
  is.finite(resultado$f_misto),
  is.finite(resultado$p_misto),
  nrow(resultado$tabela) == 3L,
  grepl("modelo misto", resultado$narrativa, ignore.case = TRUE),
  grepl("random = ~1", resultado$codigo, fixed = TRUE),
  inherits(resultado$grafico, "ggplot"),
  inherits(resultado$diagnosticos, "ggplot")
)

# O caminho ingênuo precisa aparecer como aviso e ser mais confiante neste exemplo.
stopifnot(
  grepl("evitar", resultado$tabela$Abordagem[3], fixed = TRUE),
  resultado$tabela$F[3] > resultado$tabela$F[2]
)

# Uma unidade não pode atravessar níveis do fator.
dados_invalidos <- dados
dados_invalidos$praia[1] <- dados_invalidos$praia[dados_invalidos$condicao != dados_invalidos$condicao[1]][1]
erro <- tryCatch({
  catalyser_anova_mista(dados_invalidos, parametros)
  NULL
}, error = conditionMessage)
stopifnot(is.character(erro), grepl("único nível", erro, fixed = TRUE))

dados_rv <- reactive(dados)
ficha_rv <- reactiveVal(ficha_mesclar(NULL, list(
  origem = "Planejamento observacional — Comparativo",
  unidade_coluna = "praia",
  hierarquia = list(subamostra_coluna = "arrasto"),
  resposta_coluna = "biomassa_g",
  analise_sugerida = "anova_mista_subamostras"
)))

testServer(
  mod_anova_mista_server,
  args = list(dados_rv = dados_rv, ficha_rv = ficha_rv),
  {
    session$setInputs(
      resposta = "biomassa_g", fator = "condicao",
      unidade = "praia", subamostra = "arrasto"
    )
    stopifnot(identical(exec_ctrl$estado(), "aguardando"))
    session$setInputs(executar_analise = 1)
    stopifnot(identical(exec_ctrl$estado(), "atualizada"))
    estado <- estado_execucao()
    stopifnot(
      identical(estado$tipo, "anova_mista_subamostras"),
      identical(estado$parametros$unidade, "praia"),
      identical(estado$parametros$subamostra, "arrasto"),
      estado$resultado_resumo$n_unidades == 9L,
      isTRUE(execucoes_validar_estado(estado))
    )
    session$setInputs(unidade = "arrasto")
    stopifnot(identical(exec_ctrl$estado(), "pendente"))
  }
)

# A prioridade da etapa: a ficha do planejamento chega à análise sem redigitação.
ficha_compartilhada <- reactiveVal(NULL)
testServer(
  mod_planejamento_observacional_server,
  args = list(tipo = "comparativo", ficha_destino_rv = ficha_compartilhada),
  {
    session$setInputs(
      resposta = "Biomassa g",
      eixos = "grupos",
      n_niveis = 3,
      sitios_por_nivel = 3,
      subamostras_por_sitio = 3,
      coluna_unidade = "praia",
      coluna_subamostra = "arrasto"
    )
    session$flushReact()
    session$setInputs(enviar_ficha = 1)
    enviada <- ficha_compartilhada()
    stopifnot(
      identical(names(enviada), ficha_campos),
      identical(enviada$unidade_coluna, "praia"),
      identical(ficha_subamostra_coluna(enviada), "arrasto"),
      ficha_tem_subamostras(enviada),
      identical(names(enviada$eixos), "grupos"),
      enviada$hierarquia$sitios_por_nivel == 3,
      is.null(enviada$n_planejado), is.null(enviada$sorteio),
      identical(enviada$analise_sugerida, "anova_mista_subamostras")
    )
    session$setInputs(subamostras_por_sitio = 1)
    session$flushReact()
    session$setInputs(enviar_ficha = 2)
    simples <- ficha_compartilhada()
    stopifnot(
      !ficha_tem_subamostras(simples),
      identical(ficha_subamostra_coluna(simples), ""),
      identical(simples$analise_sugerida, "anova_um_fator")
    )
  }
)

cat("OK: ANOVA mista — cálculo, pseudorreplicação, ficha e estado Shiny\n")
