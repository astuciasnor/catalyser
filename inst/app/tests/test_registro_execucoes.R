# Testes unitários leves da Fase 3C (sem iniciar o Shiny)

`%||%` <- function(x, y) if (is.null(x)) y else x
source(file.path("modules", "registro_bases.R"), encoding = "UTF-8")
source(file.path("modules", "registro_execucoes.R"), encoding = "UTF-8")

estado_linhas <- function(y = "captura_t") list(
  analise_id = "lines",
  tipo = "grafico_linhas",
  titulo = paste("Série de", y),
  parametros = list(x = "ano", y = y),
  saidas_disponiveis = "grafico",
  resultado_resumo = list(x = "ano", y = y)
)

base_raiz <- list(
  base_id = "dados_analise",
  base_objeto = "dados_analise",
  nome_amigavel = "Base compartilhada",
  derivada = FALSE
)

agora <- as.POSIXct("2026-07-13 12:00:00", tz = "UTC")
e1 <- execucoes_criar("execucao_0001", estado_linhas("captura_t"), base_raiz, 3L,
                      agora = agora)
e2 <- execucoes_criar("execucao_0002", estado_linhas("esforco_h"), base_raiz, 3L,
                      agora = agora + 1)

registro <- execucoes_adicionar(execucoes_vazio(), e1)
registro <- execucoes_adicionar(registro, e2)
stopifnot(
  length(registro) == 2L,
  identical(registro$execucao_0001$parametros$y, "captura_t"),
  identical(registro$execucao_0002$parametros$y, "esforco_h"),
  identical(execucao_ids <- names(execucoes_da_analise(registro, "lines")),
            c("execucao_0001", "execucao_0002"))
)

# Atualizar uma execução não sobrescreve a outra nem troca seu ID.
registro <- execucoes_atualizar(
  registro, "execucao_0001", estado_linhas("cpue"), base_raiz, 3L,
  titulo = "CPUE por ano", agora = agora + 2
)
stopifnot(
  identical(registro$execucao_0001$id, "execucao_0001"),
  identical(registro$execucao_0001$parametros$y, "cpue"),
  identical(registro$execucao_0001$versao, 2L),
  identical(registro$execucao_0001$criada_em, agora),
  identical(registro$execucao_0002$parametros$y, "esforco_h")
)

registro <- execucoes_remover(registro, "execucao_0002")
stopifnot(length(registro) == 1L, is.null(registro$execucao_0002))

# A revisão da origem marca o registro como desatualizado sem apagá-lo.
stopifnot(
  identical(execucoes_estado_dependencia(e1, list(), list(), 3L), "Atualizada"),
  identical(execucoes_estado_dependencia(e1, list(), list(), 4L), "Precisa atualizar")
)

# Ramos dependem da receita e do cache que estavam atuais no clique.
base_ramo <- bases_novo_registro(
  "base_0001", "Gráficos anuais", "base_graficos", "graficos", revisao_origem = 3L
)
base_ramo$estado <- "pronta"
contexto_ramo <- list(
  base_id = base_ramo$id,
  base_objeto = base_ramo$nome_r,
  nome_amigavel = base_ramo$nome_amigavel,
  finalidade = base_ramo$finalidade,
  versao_receita = base_ramo$versao,
  derivada = TRUE
)
e_ramo <- execucoes_criar("execucao_0003", estado_linhas(), contexto_ramo, 3L)
bases <- list(base_ramo)
cache <- list(base_0001 = list(
  df = data.frame(ano = 2024, captura_t = 1), erros = list(),
  revisao_origem = 3L, versao_receita = 1L, resultado_em = agora
))
stopifnot(
  identical(execucoes_estado_dependencia(e_ramo, bases, cache, 3L), "Atualizada"),
  identical(execucoes_estado_dependencia(e_ramo, list(), cache, 3L), "Base excluída")
)
bases[[1]]$versao <- 2L
stopifnot(identical(
  execucoes_estado_dependencia(e_ramo, bases, cache, 3L), "Precisa atualizar"
))

# Entrada manual não depende da revisão de dados_analise.
e_manual <- execucoes_criar(
  "execucao_0004", estado_linhas(),
  list(base_id = "entrada_manual", base_objeto = "tabela_manual",
       nome_amigavel = "Entrada manual", base_tipo = "entrada_manual"),
  3L
)
stopifnot(identical(
  execucoes_estado_dependencia(e_manual, list(), list(), 99L), "Atualizada"
))

# Contratos inválidos falham de forma explícita.
estado_invalido <- estado_linhas()
estado_invalido$saidas_disponiveis <- "video"
erro <- try(execucoes_criar("execucao_0005", estado_invalido, base_raiz, 3L), silent = TRUE)
stopifnot(inherits(erro, "try-error"))

cat("OK: contrato puro do registro de execuções\n")

# Integração mínima do módulo Shiny: adicionar novo, atualizar anterior e apagar.
library(shiny)
library(bslib)
source(file.path("modules", "mod_registrar_execucao.R"), encoding = "UTF-8")

html_fluxo <- htmltools::renderTags(
  mod_analise_registravel_ui(
    "teste_fluxo",
    div("CONTEUDO_ANALISE"),
    div("CONTEUDO_REGISTRO")
  )
)$html
stopifnot(
  grepl("1. Configurar e executar", html_fluxo, fixed = TRUE),
  grepl("2. Adicionar aos resultados", html_fluxo, fixed = TRUE),
  grepl("teste_fluxo-fluxo_registro_subabas", html_fluxo, fixed = TRUE)
)

estado_atual <- reactiveVal(estado_linhas("captura_t"))
registro_rv <- reactiveVal(execucoes_vazio())
contador_rv <- reactiveVal(0L)
revisao_rv <- reactiveVal(3L)
bases_rv <- reactiveVal(list())
cache_rv <- reactiveVal(list())

testServer(
  mod_registrar_execucao_server,
  args = list(
    estado_execucao_rv = reactive(estado_atual()),
    base_contexto_rv = reactive(base_raiz),
    registro_execucoes_rv = registro_rv,
    contador_execucoes_rv = contador_rv,
    revisao_origem_rv = revisao_rv,
    registro_bases_rv = bases_rv,
    cache_bases_rv = cache_rv,
    analise_id = "lines",
    nome_analise = "O Gráfico de Linhas"
  ),
  {
    stopifnot(
      grepl("0 registradas", output$contador$html, fixed = TRUE),
      grepl("Adicionar Novo Resultado", output$gerenciamento$html, fixed = TRUE),
      grepl("Prévia executada, ainda não registrada", output$dependencia$html, fixed = TRUE)
    )

    session$setInputs(titulo = "Captura por ano", adicionar = 1)
    stopifnot(length(registro_rv()) == 1L, identical(contador_rv(), 1L))
    stopifnot(
      grepl("Atualizar Resultado Anterior", output$gerenciamento$html, fixed = TRUE),
      grepl("Adicionar Novo Resultado", output$gerenciamento$html, fixed = TRUE),
      grepl("Apagar Resultado Selecionado", output$gerenciamento$html, fixed = TRUE)
    )

    estado_atual(estado_linhas("esforco_h"))
    session$setInputs(titulo = "Esforço por ano", salvar_novo = 1)
    stopifnot(
      length(registro_rv()) == 2L,
      identical(registro_rv()$execucao_0001$parametros$y, "captura_t"),
      identical(registro_rv()$execucao_0002$parametros$y, "esforco_h")
    )

    estado_atual(estado_linhas("cpue"))
    session$setInputs(titulo = "CPUE por ano", atualizar = 1)
    stopifnot(
      identical(registro_rv()$execucao_0002$parametros$y, "cpue"),
      identical(registro_rv()$execucao_0002$versao, 2L),
      identical(registro_rv()$execucao_0001$parametros$y, "captura_t")
    )
  }
)

cat("OK: módulo Shiny do registro de execuções\n")

# O registro não aceita uma configuração ainda não executada ou já pendente.
pronta_rv <- reactiveVal(FALSE)
registro_bloqueado_rv <- reactiveVal(execucoes_vazio())
contador_bloqueado_rv <- reactiveVal(0L)

testServer(
  mod_registrar_execucao_server,
  args = list(
    estado_execucao_rv = reactive({
      req(pronta_rv())
      estado_linhas("captura_t")
    }),
    base_contexto_rv = reactive(base_raiz),
    registro_execucoes_rv = registro_bloqueado_rv,
    contador_execucoes_rv = contador_bloqueado_rv,
    revisao_origem_rv = reactiveVal(3L),
    registro_bases_rv = reactiveVal(list()),
    cache_bases_rv = reactiveVal(list()),
    analise_id = "lines",
    nome_analise = "O Gráfico de Linhas"
  ),
  {
    stopifnot(
      grepl("disabled=\"disabled\"", output$gerenciamento$html, fixed = TRUE),
      grepl("Aguardando execução da análise", output$dependencia$html, fixed = TRUE)
    )

    pronta_rv(TRUE)
    session$flushReact()
    stopifnot(
      !grepl("disabled=\"disabled\"", output$gerenciamento$html, fixed = TRUE),
      grepl("Prévia executada, ainda não registrada", output$dependencia$html, fixed = TRUE)
    )
  }
)

cat("OK: registro bloqueia rascunho não executado ou pendente\n")
