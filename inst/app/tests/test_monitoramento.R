# Executar a partir de catalyser/inst/app.
# Delineamento Monitoramento (série temporal): calendário, nomes de coluna,
# tabela tidy, metadados, Excel com fórmula e os rótulos da ficha.
library(shiny)
if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a) || !length(a)) b else a
source(file.path("modules", "ficha_planejamento.R"), encoding = "UTF-8")
source(file.path("modules", "mod_monitoramento.R"), encoding = "UTF-8")

# 1. Nomes de coluna limpos: sem acento, sem sinal, minúsculos, unidade junto.
stopifnot(
  identical(limpar_nome("Captura (kg)"), "captura_kg"),
  identical(limpar_nome("Produção"), "producao"),
  identical(limpar_nome("Peso  médio"), "peso_medio"),
  identical(limpar_nome("CPUE"), "cpue")
)

# 2. O calendário segue a frequência, sempre com seq() de datas.
mensal <- datas_da_serie(as.Date("2026-01-15"), as.Date("2026-06-20"), "mensal")
stopifnot(
  length(mensal) == 6L,
  all(format(mensal, "%d") == "01"),
  identical(min(mensal), as.Date("2026-01-01")),
  identical(max(mensal), as.Date("2026-06-01"))
)
quinzenal <- datas_da_serie(as.Date("2026-01-10"), as.Date("2026-03-20"), "quinzenal")
stopifnot(
  length(quinzenal) == 6L,
  identical(format(quinzenal, "%d"), c("01", "16", "01", "16", "01", "16"))
)
semanal <- datas_da_serie(as.Date("2026-01-05"), as.Date("2026-01-25"), "semanal")
stopifnot(
  length(semanal) == 3L,
  all(weekdays(semanal) == weekdays(as.Date("2026-01-05")))  # 05/01/2026 é segunda
)
diaria <- datas_da_serie(as.Date("2026-03-01"), as.Date("2026-03-10"), "diaria")
stopifnot(length(diaria) == 10L, identical(diaria, seq(as.Date("2026-03-01"), as.Date("2026-03-10"), by = "1 day")))
# Pontas trocadas não quebram: o período fecha na data de início.
stopifnot(length(datas_da_serie(as.Date("2026-05-10"), as.Date("2026-05-01"), "diaria")) == 1L)

# 3. A configuração reúne a aba 1: locais sem nome ganham rótulo, medida vazia sai,
#    esforço vira coluna e o índice marcado vira coluna _por_ com marca de CPUE.
cfg <- montar_config(
  inicio = as.Date("2026-01-01"), fim = as.Date("2027-12-01"), frequencia = "mensal",
  horario = "06h", hora_real = TRUE,
  locais_raw = c("Porto", ""),
  tem_esforco = TRUE, unidade_esforco = "viagens",
  medidas_raw = data.frame(nome = c("captura", ""), unidade = c("kg", ""), stringsAsFactors = FALSE),
  indices_marcados = c(TRUE, FALSE)
)
stopifnot(
  length(cfg$datas) == 24L,
  identical(cfg$locais, c("Porto", "local_2")),
  nrow(cfg$medidas) == 1L, identical(cfg$medidas$coluna, "captura_kg"),
  identical(cfg$esforco_col, "esforco_viagens"),
  nrow(cfg$indices) == 1L,
  identical(cfg$indices$coluna, "captura_kg_por_viagens"),
  identical(cfg$indices$coluna_medida, "captura_kg"),
  isTRUE(cfg$indices$cpue)
)
# Sem esforço não há coluna de esforço nem índice, mesmo com a caixa marcada.
sem_esforco <- montar_config(
  inicio = as.Date("2026-01-01"), fim = as.Date("2026-06-01"), frequencia = "mensal",
  horario = "", hora_real = FALSE, locais_raw = "Rio",
  tem_esforco = FALSE, unidade_esforco = "viagens",
  medidas_raw = data.frame(nome = "captura", unidade = "kg", stringsAsFactors = FALSE),
  indices_marcados = TRUE
)
stopifnot(is.null(sem_esforco$esforco_col), nrow(sem_esforco$indices) == 0L)
# Regressão: sem nenhum índice marcado (o estado em que a tela abre), a tabela
# de índices nasce vazia sem erro, com esforço desligado ou ligado.
padrao <- montar_config(
  inicio = as.Date("2026-01-01"), fim = as.Date("2027-12-01"), frequencia = "mensal",
  horario = "", hora_real = FALSE, locais_raw = c("Porto", "Praia"),
  tem_esforco = FALSE, unidade_esforco = "viagens",
  medidas_raw = data.frame(nome = "captura", unidade = "kg", stringsAsFactors = FALSE),
  indices_marcados = FALSE
)
sem_marcar <- montar_config(
  inicio = as.Date("2026-01-01"), fim = as.Date("2027-12-01"), frequencia = "mensal",
  horario = "", hora_real = FALSE, locais_raw = "Porto",
  tem_esforco = TRUE, unidade_esforco = "viagens",
  medidas_raw = data.frame(nome = "captura", unidade = "kg", stringsAsFactors = FALSE),
  indices_marcados = FALSE
)
stopifnot(
  nrow(padrao$indices) == 0L, nrow(sem_marcar$indices) == 0L,
  identical(names(padrao$indices), c("coluna", "coluna_medida", "coluna_esforco", "cpue"))
)

# 4. A tabela tidy: 24 datas × 2 locais = 48 linhas; hora logo após local;
#    observação sempre existe; a data anda devagar e o local percorre.
tab <- montar_tabela(cfg$datas, cfg$locais, cfg$hora, cfg$medidas$coluna,
                     cfg$esforco_col, cfg$indices$coluna)
stopifnot(
  nrow(tab) == 48L,
  identical(names(tab), c("data", "local", "hora", "captura_kg",
                          "esforco_viagens", "captura_kg_por_viagens", "observacao")),
  identical(tab$data[1:2], rep(format(cfg$datas[1], "%Y-%m-%d"), 2)),
  identical(tab$local[1:2], c("Porto", "local_2")),
  all(is.na(tab$captura_kg)), all(tab$observacao == "")
)

# 5. Os metadados reproduzem a aba 1, com o índice descrito em palavras.
met <- montar_metadados(cfg)
stopifnot(
  any(met$campo == "delineamento" & met$valor == "Monitoramento (série temporal)"),
  any(met$campo == "frequência" & met$valor == "Mensal"),
  any(met$campo == "locais" & grepl("Porto", met$valor, fixed = TRUE)),
  any(met$campo == "unidade do esforço" & met$valor == "viagens"),
  any(grepl("^índice: captura_kg_por_viagens$", met$campo) &
        grepl("dividido por esforco_viagens (CPUE)", met$valor, fixed = TRUE))
)

# 6. O Excel sai com as duas abas e a fórmula viva nas colunas de índice.
if (requireNamespace("openxlsx", quietly = TRUE)) {
  alvo <- tempfile(fileext = ".xlsx")
  escrever_excel(alvo, tab, met, cfg$indices)
  partes <- utils::unzip(alvo, list = TRUE)$Name
  stopifnot(all(c("xl/worksheets/sheet1.xml", "xl/worksheets/sheet2.xml") %in% partes))
  extraido <- tempfile("xlsx_"); dir.create(extraido)
  utils::unzip(alvo, exdir = extraido)
  folha_dados <- readLines(file.path(extraido, "xl", "worksheets", "sheet1.xml"),
                           encoding = "UTF-8", warn = FALSE)
  stopifnot(any(grepl("IF(OR(", folha_dados, fixed = TRUE)))
  unlink(c(alvo, extraido), recursive = TRUE)
} else {
  message("openxlsx ausente: teste do Excel pulado.")
}

# 7. Configuração reativa e resumo ao vivo: 12 datas, 2 locais e um índice;
#    a série curta mostra os dois avisos gentis (nada bloqueia).
testServer(mod_monitoramento_server, args = list(), {
  session$setInputs(
    inicio = as.Date("2026-01-01"), fim = as.Date("2026-12-01"), frequencia = "mensal",
    horario = "", hora_real = "nao", tem_esforco = "sim", unidade_esforco = "viagens",
    local_1 = "Porto", local_2 = "Praia",
    medida_nome_1 = "captura", medida_unidade_1 = "kg", indice_1 = TRUE
  )
  session$flushReact()
  c0 <- cfg()
  stopifnot(length(c0$datas) == 12L, length(c0$locais) == 2L, nrow(c0$indices) == 1L)
  # Série curta: os dois avisos gentis aparecem no resumo (nada bloqueia).
  res <- output$resumo
  html <- if (is.list(res) && !is.null(res$html)) as.character(res$html) else as.character(res)
  stopifnot(
    any(grepl("12 datas", html, fixed = TRUE)),
    any(grepl("sazonalidade", html, fixed = TRUE)),
    any(grepl("tendência", html, fixed = TRUE))
  )
})

# 7b. O estado em que a aba abre (esforço desligado, nenhum índice marcado)
#     também rende o resumo e a tabela completa sem erro de R.
testServer(mod_monitoramento_server, args = list(), {
  session$setInputs(
    inicio = as.Date("2026-01-01"), fim = as.Date("2027-12-01"), frequencia = "mensal",
    horario = "", hora_real = "nao", tem_esforco = "nao",
    local_1 = "Porto", local_2 = "Praia",
    medida_nome_1 = "captura", medida_unidade_1 = "kg", indice_1 = FALSE
  )
  session$flushReact()
  c0 <- cfg()
  stopifnot(nrow(c0$indices) == 0L, is.null(c0$esforco_col))
  res <- output$resumo
  html <- if (is.list(res) && !is.null(res$html)) as.character(res$html) else as.character(res)
  stopifnot(
    any(grepl("24 datas", html, fixed = TRUE)),
    any(grepl("48 linhas", html, fixed = TRUE)),
    !any(grepl("differing number of rows", html, fixed = TRUE))
  )
  # A tabela paginada traz todas as linhas, não só as primeiras: no JSON do
  # DT, cada local aparece uma vez por data (24 datas × 2 locais = 48).
  dt <- output$tabela
  stopifnot(
    is.character(dt), length(dt) == 1,
    length(gregexpr('"Porto"', dt, fixed = TRUE)[[1]]) == 24,
    length(gregexpr('"Praia"', dt, fixed = TRUE)[[1]]) == 24
  )
})

# 8. A ficha monitoramento ganha nome e rótulo por extenso no cabeçalho.
f_mon <- ficha_mesclar(NULL, list(origem = "Monitoramento (série temporal)",
                                  tipo = "monitoramento", analise_sugerida = "series_temporais"))
stopifnot(
  identical(ficha_nome_delineamento(f_mon$tipo), "Monitoramento (série temporal)"),
  grepl("séries temporais", ficha_rotulo_analise(f_mon$analise_sugerida), fixed = TRUE)
)

cat("OK: monitoramento — calendário, tabela, metadados, Excel e ficha\n")
