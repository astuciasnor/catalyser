# Teste de "Como sortear a amostra": marco amostral -> sorteio -> planilha de coleta.
# Rodar a partir de inst/app:  Rscript tests/test_sortear_marco.R
# Não carrega o app inteiro: só a função de sorteio e os módulos da tela.
suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
})
if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a) || !length(a)) b else a
source("templates/funcoes_sorteio.R", encoding = "UTF-8")
source("modules/ficha_planejamento.R", encoding = "UTF-8")
source("modules/mod_n_poder.R", encoding = "UTF-8")
source("modules/mod_planejamento_variaveis.R", encoding = "UTF-8")
source("modules/mod_sampling.R", encoding = "UTF-8")

marco <- data.frame(
  embarcacao = sprintf("B%02d", 1:40),
  porto = rep(c("Bragança", "Ajuruteua", "Vila"), c(20, 15, 5)),
  km = 40:1,
  stringsAsFactors = FALSE
)

# --- 1. Função pura: os quatro métodos, com semente ----------------------------
a1 <- sortear_marco(marco, "aas", semente = 2026, id = "embarcacao", n = 10)
a2 <- sortear_marco(marco, "aas", semente = 2026, id = "embarcacao", n = 10)
stopifnot(
  identical(a1$sorteados, a2$sorteados),                 # mesma semente, mesmo sorteio
  nrow(a1$sorteados) == 10,
  identical(names(a1$sorteados), names(marco)),          # colunas estruturais herdadas
  !anyDuplicated(a1$sorteados$embarcacao),
  !identical(a1$sorteados, sortear_marco(marco, "aas", 7, "embarcacao", n = 10)$sorteados)
)

# Correção para população finita: n = 30 com N = 40 vira 18.
f <- sortear_marco(marco, "aas", 1, "embarcacao", n = 30, corrigir_finita = TRUE)
stopifnot(corrigir_n_finito(30, 40) == 18L, f$registro$n == 18L, f$registro$n_pedido == 30L)

# Estratificada proporcional: alocação soma n e respeita os tamanhos.
e <- sortear_marco(marco, "estratificada", 7, "embarcacao", n = 12, estrato = "porto")
stopifnot(
  sum(e$alocacao$n_h) == 12,
  all(table(e$sorteados$porto)[e$alocacao$estrato] == e$alocacao$n_h),
  identical(e$alocacao$n_h, c(5L, 6L, 1L))               # 12*15/40, 12*20/40, 12*5/40
)
# Estrato que fica sem unidade gera aviso, não erro.
e0 <- sortear_marco(marco, "estratificada", 7, "embarcacao", n = 3, estrato = "porto")
stopifnot(length(e0$avisos) == 1, grepl("Vila", e0$avisos))
# Alocação igual (uso nos delineamentos, ex.: n indivíduos por espécie).
ig <- sortear_marco(marco, "estratificada", 7, "embarcacao", n = 4, estrato = "porto", alocacao = "igual")
stopifnot(all(table(ig$sorteados$porto) == 4))
# Mesmo sorteio em qualquer idioma do sistema.
velho <- Sys.getlocale("LC_COLLATE")
invisible(Sys.setlocale("LC_COLLATE", "C"))
stopifnot(identical(sortear_marco(marco, "estratificada", 7, "embarcacao", n = 12, estrato = "porto")$sorteados, e$sorteados))
invisible(Sys.setlocale("LC_COLLATE", velho))

# Sistemática: de k em k na ordem da coluna, a partir de um início sorteado.
s <- sortear_marco(marco, "sistematica", 3, "embarcacao", ordem = "km", k = 6)
stopifnot(
  s$registro$partida >= 1, s$registro$partida <= 6,
  all(diff(s$sorteados$km) == 6),                        # km crescente de 6 em 6
  s$sorteados$km[1] == s$registro$partida
)

# Erros claros para marcos com problema.
erro <- function(expr) tryCatch({ expr; "" }, error = conditionMessage)
stopifnot(
  grepl("repetidos", erro(sortear_marco(rbind(marco, marco[1, ]), "aas", 1, "embarcacao", n = 3))),
  grepl("semente", erro(sortear_marco(marco, "aas", NA, "embarcacao", n = 3))),
  grepl("não dá para sortear", erro(sortear_marco(marco, "aas", 1, "embarcacao", n = 41))),
  grepl("uma só unidade", erro(sortear_marco(marco, "estratificada", 1, "embarcacao", n = 6, estrato = "embarcacao"))),
  grepl("menos unidades", erro(sortear_marco(marco, "estratificada", 1, "embarcacao", n = 6, estrato = "porto", alocacao = "igual")))
)

# --- 2. O código exportado refaz exatamente o mesmo sorteio ------------------
pasta <- tempfile("sorteio_"); dir.create(file.path(pasta, "R"), recursive = TRUE); dir.create(file.path(pasta, "dados"))
invisible(file.copy("templates/funcoes_sorteio.R", file.path(pasta, "R", "funcoes_sorteio.R")))
writexl::write_xlsx(list(marco_amostral = marco), file.path(pasta, "dados", "marco_amostral.xlsx"))
for (res in list(a1, f, e, s)) {
  amb <- new.env(parent = globalenv())
  antigo <- setwd(pasta)
  eval(parse(text = codigo_sorteio(res$registro)), envir = amb)
  setwd(antigo)
  stopifnot(identical(amb$sorteio$sorteados$embarcacao, res$sorteados$embarcacao))
}

# --- 3. Leitura do marco: arquivo, texto colado e geração de 1 a N ---------------------------------
lista <- ler_marco_texto("Praia do Farol\nPraia de Ajuruteua\n\nPraia de Bonifácio")
stopifnot(nrow(lista) == 3, names(lista) == "id_unidade")
colado <- ler_marco_texto("praia\tsetor\nFarol\tleste\nAjuruteua\toeste", cabecalho = TRUE)
stopifnot(identical(names(colado), c("praia", "setor")), nrow(colado) == 2)
stopifnot(identical(gerar_marco_numerado(12, "E")$id_unidade[c(1, 12)], c("E01", "E12")))
csv <- tempfile(fileext = ".csv")
writeLines(c("estacao;profundidade", "E1;2,5", "E2;3,0", ";"), csv)
lido <- ler_marco_arquivo(csv, "marco.csv")
stopifnot(nrow(lido) == 2, is.numeric(lido$profundidade), lido$profundidade[1] == 2.5)

# --- 4. A tela de ponta a ponta ----------------------------------------------
testServer(mod_sortear_amostra_server, {
  # Gera o marco de 1 a N e acrescenta uma coluna de estrato criada ali mesmo.
  # Antes de confirmar, a prévia já mostra a lista, mas as abas seguintes seguem fechadas.
  session$setInputs(modo_entrada = "gerar", gerar_N = 30, gerar_prefixo = "E")
  stopifnot(isTRUE(pendente()), nrow(tabela_mostrada()) == 30, is.null(marco_rv()))
  session$setInputs(usar_marco = 1)
  stopifnot(nrow(marco_rv()) == 30, is.null(sorteio_rv()), !pendente(), isTRUE(recem_confirmado_rv()))
  # Com uma coluna só, ela é o identificador sem perguntar nada.
  stopifnot(id_atual() == "id_unidade")
  # A coluna de estrato nasce pela janela "+ Coluna", por níveis: 20 norte e 10 sul.
  session$setInputs(abrir_coluna = 1, nc_papel = "estrato", nc_nome = "Setor", nc_modo = "niveis",
                    nc_n_niveis = 2, nc_nivel_1 = "norte", nc_rep_1 = 20, nc_nivel_2 = "sul", nc_rep_2 = 10)
  stopifnot(length(nc_valores()) == 30, nrow(nc_resultado()) == 30)
  session$setInputs(nc_criar = 1)
  stopifnot("setor" %in% names(marco_rv()), sum(marco_rv()$setor == "sul") == 10)
  # Corrige células como o pesquisador faria na tabela (colunas começam em 0 no DT).
  for (i in 21:30) session$setInputs(marco_tabela_cell_edit = data.frame(row = i, col = 1, value = "sul"))
  stopifnot(sum(marco_rv()$setor == "sul") == 10)

  # Estratificada proporcional com n = 6 -> 4 do norte e 2 do sul.
  session$setInputs(metodo = "estratificada", semente = 42, n = 6, estrato = "setor", sortear = 1)
  r <- sorteio_rv()
  stopifnot(!is.null(r), nrow(r$sorteados) == 6,
            identical(r$alocacao$n_h, c(4L, 2L)),
            identical(names(r$sorteados), c("id_unidade", "setor")))

  # Editar o marco invalida o sorteio anterior.
  session$setInputs(marco_tabela_cell_edit = data.frame(row = 1, col = 1, value = "sul"))
  stopifnot(is.null(sorteio_rv()))

  # Sorteia de outra forma sobre a mesma tabela, sem reconstruir nada.
  session$setInputs(metodo = "aas", n = 5, corrigir_finita = FALSE, sortear = 2)
  stopifnot(nrow(sorteio_rv()$sorteados) == 5)

  # Passo 3: declara duas respostas no Planejamento de Variáveis reaproveitado.
  session$setInputs(`variaveis-n_variaveis` = 2,
                    `variaveis-nome_1` = "Comprimento total", `variaveis-reduzido_1` = "comp_total_cm",
                    `variaveis-tipo_1` = "Quantitativa contínua", `variaveis-unidade_1` = "cm",
                    `variaveis-nome_2` = "Sexo", `variaveis-reduzido_2` = "sexo",
                    `variaveis-tipo_2` = "Qualitativa nominal", `variaveis-unidade_2` = "")
})

# O Planejamento de Variáveis no modo sorteio, isolado, para inspecionar as saídas.
sorteio_fixo <- reactiveVal(c(e, list(marco = marco)))
testServer(mod_planejamento_variaveis_server, args = list(estrutura_rv = sorteio_fixo, modo = "sorteio"), {
  session$setInputs(n_variaveis = 2,
                    nome_1 = "Captura por viagem", reduzido_1 = "captura_kg", tipo_1 = "Quantitativa contínua", unidade_1 = "kg",
                    nome_2 = "Arte de pesca", reduzido_2 = "arte", tipo_2 = "Qualitativa nominal", unidade_2 = "",
                    faixa_1 = "0 a 500", descricao_1 = "Peso desembarcado")
  p <- planilha()
  # Duas famílias de coluna: estruturais preenchidas e respostas em branco.
  stopifnot(
    nrow(p) == 12,
    identical(names(p), c("embarcacao", "porto", "km", "captura_kg", "arte")),
    identical(p$embarcacao, e$sorteados$embarcacao),
    all(is.na(p$captura_kg)), all(is.na(p$arte))
  )
  d <- dicionario_completo()
  stopifnot(
    identical(d$nome_reduzido, c("embarcacao", "porto", "km", "captura_kg", "arte")),
    identical(d$papel, c(rep("Estrutural (marco amostral)", 3), "Resposta", "Resposta")),
    d$tipo[1] == "Identificador", d$unidade[4] == "kg", d$faixa_ou_niveis[4] == "0 a 500"
  )
  ficha <- ficha_consolidada()
  stopifnot(
    identical(names(ficha), ficha_campos),
    ficha$unidade_coluna == "embarcacao",
    ficha$resposta_coluna == "captura_kg",
    ficha$sorteio$semente == 7L,
    identical(ficha$sorteio$unidades_selecionadas, as.character(e$sorteados$embarcacao))
  )
  abas <- abas_planilha()
  stopifnot(identical(names(abas), c("coleta", "dicionario", "registro_sorteio", "ficha_planejamento")))
})

# --- 5. Conglomerados em um e dois estágios ----------------------------------
peixes <- data.frame(id_peixe = sprintf("P%03d", 1:120), embarcacao = rep(sprintf("E%02d", 1:40), each = 3),
                     stringsAsFactors = FALSE)
peixes <- rbind(peixes, data.frame(id_peixe = sprintf("Q%02d", 1:10), embarcacao = "E05"))
unico <- sortear_marco(peixes, "conglomerados", 2026, "id_peixe", conglomerado = "embarcacao", n_conglomerados = 6)
dois <- sortear_marco(peixes, "conglomerados", 2026, "id_peixe", conglomerado = "embarcacao",
                      n_conglomerados = 6, estagios = "dois", m = 2)
stopifnot(
  nrow(unico$alocacao) == 6,
  all(unico$alocacao$m_i == unico$alocacao$M_i),                  # estágio único: todas as unidades
  identical(unico$alocacao$conglomerado, dois$alocacao$conglomerado), # mesma semente, mesmos grupos
  all(dois$alocacao$m_i == 2),
  identical(unique(dois$sorteados$embarcacao), dois$alocacao$conglomerado),  # agrupados por conglomerado
  "embarcacao" %in% names(dois$sorteados),
  isTRUE(unico$registro$aninhado), isTRUE(dois$registro$aninhado),
  identical(dois$sorteados, sortear_marco(peixes, "conglomerados", 2026, "id_peixe", conglomerado = "embarcacao",
                                         n_conglomerados = 6, estagios = "dois", m = 2)$sorteados)
)
# Conglomerado menor que m: entra inteiro, com aviso.
pequeno <- sortear_marco(peixes, "conglomerados", 2026, "id_peixe", conglomerado = "embarcacao",
                         n_conglomerados = 6, estagios = "dois", m = 5)
stopifnot(length(pequeno$avisos) == 1)
# m = 1: uma unidade por grupo, sem aninhamento.
um <- sortear_marco(peixes, "conglomerados", 2026, "id_peixe", conglomerado = "embarcacao",
                    n_conglomerados = 6, estagios = "dois", m = 1)
stopifnot(!isTRUE(um$registro$aninhado))
stopifnot(grepl("uma só unidade", erro(sortear_marco(peixes, "conglomerados", 1, "id_peixe", conglomerado = "id_peixe", n_conglomerados = 3))))
# O código exportado refaz o sorteio em dois estágios.
writexl::write_xlsx(list(marco_amostral = peixes), file.path(pasta, "dados", "marco_amostral.xlsx"))
amb <- new.env(parent = globalenv()); antigo <- setwd(pasta)
invisible(eval(parse(text = codigo_sorteio(dois$registro)), envir = amb)); setwd(antigo)
stopifnot(identical(amb$sorteio$sorteados$id_peixe, dois$sorteados$id_peixe))
# A ficha registra o aninhamento para o modelo misto.
sorteio_cong <- reactiveVal(c(dois, list(marco = peixes)))
testServer(mod_planejamento_variaveis_server, args = list(estrutura_rv = sorteio_cong, modo = "sorteio"), {
  session$setInputs(n_variaveis = 1, nome_1 = "Comprimento", reduzido_1 = "comp_cm", tipo_1 = "Quantitativa contínua", unidade_1 = "cm")
  f <- ficha_consolidada()
  stopifnot(
    f$unidade_coluna == "embarcacao", ficha_subamostra_coluna(f) == "id_peixe",
    ficha_tem_subamostras(f), f$analise_sugerida == "anova_mista_subamostras",
    f$hierarquia$efeito_aleatorio == "embarcacao",
    nrow(planilha()) == 12
  )
})
# Na tela: o método aparece no seletor e sorteia sobre o mesmo marco.
testServer(mod_sortear_amostra_server, {
  session$setInputs(modo_entrada = "importar",
                    texto = paste(c("peixe\tbarco", sprintf("P%02d\tB%d", 1:30, rep(1:10, each = 3))), collapse = "\n"),
                    texto_cabecalho = TRUE)
  session$elapse(400)   # espera a leitura do texto (debounce)
  session$setInputs(usar_marco = 1)
  session$setInputs(coluna_id = "peixe", metodo = "conglomerados", semente = 5, conglomerado = "barco",
                    n_conglomerados = 4, estagios = "dois", m = 2, sortear = 1)
  stopifnot(nrow(sorteio_rv()$sorteados) == 8, isTRUE(sorteio_rv()$registro$aninhado))
})

# --- 6. Lista colada sem cabeçalho e aviso na própria linha -----------------
stopifnot(identical(problemas_por_linha(data.frame(id = c("A", "", "B", "B")), "id") != "",
                    c(FALSE, TRUE, TRUE, TRUE)))
testServer(mod_sortear_amostra_server, {
  # Caso comum: lista pura, caixa de cabeçalho desmarcada por padrão.
  session$setInputs(modo_entrada = "importar", texto = "Barco A\nBarco B\nBarco C", texto_cabecalho = FALSE)
  session$elapse(400)
  stopifnot(nrow(tabela_mostrada()) == 3, tabela_mostrada()[[1]][1] == "Barco A")
  session$setInputs(usar_marco = 1)
  stopifnot(nrow(marco_rv()) == 3, id_atual() == "id_unidade")
  # Uma linha nova sem nome é marcada ali mesmo, com o motivo.
  session$setInputs(add_linha = 1)
  stopifnot(sum(nzchar(problemas_por_linha(marco_rv(), id_atual()))) == 1)
  # Mudar o texto depois de confirmar volta a mostrar a prévia, sem trocar a lista.
  session$setInputs(texto = "Barco X\nBarco Y")
  session$elapse(400)
  stopifnot(isTRUE(pendente()), nrow(marco_rv()) == 4)
})

# --- 7. Janela "+ Coluna": níveis, conferência com N, ordem e conglomerados ---
testServer(mod_sortear_amostra_server, {
  # Lista pura de quatro barcos.
  session$setInputs(modo_entrada = "importar", texto = "Barco A\nBarco B\nBarco C\nBarco D", texto_cabecalho = FALSE)
  session$elapse(400)
  session$setInputs(usar_marco = 1)
  # O exemplo do enunciado: porto com Bragança 2 e Ajuruteua 2.
  session$setInputs(abrir_coluna = 1, nc_papel = "estrato", nc_nome = "porto", nc_modo = "niveis",
                    nc_n_niveis = 2, nc_nivel_1 = "Bragança", nc_rep_1 = 2, nc_nivel_2 = "Ajuruteua", nc_rep_2 = 2, nc_criar = 1)
  stopifnot(identical(marco_rv()$porto, c("Bragança", "Bragança", "Ajuruteua", "Ajuruteua")))
  # Soma maior que N: sem escolher a saída, nada é criado.
  session$setInputs(abrir_coluna = 2, nc_papel = "outra", nc_nome = "zona", nc_modo = "niveis",
                    nc_n_niveis = 1, nc_nivel_1 = "costeira", nc_rep_1 = 6, nc_ajuste = NULL, nc_criar = 2)
  stopifnot(!"zona" %in% names(marco_rv()), nrow(marco_rv()) == 4)
  # Escolhendo "cortar", a coluna usa só as 4 primeiras repetições.
  session$setInputs(nc_ajuste = "cortar", nc_criar = 3)
  stopifnot("zona" %in% names(marco_rv()), nrow(marco_rv()) == 4)
  # Ordem: sequência de 1 a N, e ela já fica escolhida no seletor da sistemática.
  session$setInputs(abrir_coluna = 3, nc_papel = "ordem", nc_nome = "ordem", nc_modo_ordem = "sequencia", nc_ajuste = NULL, nc_criar = 4)
  stopifnot(identical(marco_rv()$ordem, 1:4))
})
testServer(mod_sortear_amostra_server, {
  # Lista vazia: 5 embarcações com 20 unidades montam o marco inteiro.
  session$setInputs(abrir_coluna = 1, nc_papel = "conglomerado", nc_nome = "embarcacao",
                    nc_n_grupos = 5, nc_por_grupo = 20, nc_prefixo = "E", nc_criar = 1)
  m <- marco_rv()
  stopifnot(nrow(m) == 100, identical(names(m), c("id_unidade", "embarcacao")),
            identical(as.vector(table(m$embarcacao)), rep(20L, 5)), id_atual() == "id_unidade")
  # E já dá para sortear por conglomerados em dois estágios.
  session$setInputs(metodo = "conglomerados", semente = 3, conglomerado = "embarcacao",
                    n_conglomerados = 2, estagios = "dois", m = 4, sortear = 1)
  stopifnot(nrow(sorteio_rv()$sorteados) == 8)
})
# Linhas criadas continuam a numeração dos identificadores.
stopifnot(identical(continuar_ids(c("E01", "E02"), 2), c("E03", "E04")),
          identical(continuar_ids(1:4, 2), c(5, 6)),
          all(is.na(continuar_ids(c("Barco A", "Barco B"), 1))))

# --- 8. Importar reúne arquivo e texto colado: vale o último usado ------------
testServer(mod_sortear_amostra_server, {
  # Abre em Importar; colar uma lista já mostra a prévia.
  session$setInputs(modo_entrada = "importar", texto = "Barco A\nBarco B", texto_cabecalho = FALSE)
  session$elapse(400)
  stopifnot(nrow(tabela_mostrada()) == 2, previa()$origem == "da lista colada")
  # Escolher um arquivo depois passa a valer o arquivo.
  csv2 <- tempfile(fileext = ".csv"); writeLines(c("estacao", "E1", "E2", "E3"), csv2)
  session$setInputs(arquivo = data.frame(name = "estacoes.csv", size = 10, type = "text/csv", datapath = csv2))
  stopifnot(nrow(tabela_mostrada()) == 3, grepl("estacoes.csv", previa()$origem))
  # Voltar a mexer no texto faz o texto valer de novo.
  session$setInputs(texto = "Barco A\nBarco B\nBarco C\nBarco D")
  session$elapse(400)
  stopifnot(nrow(tabela_mostrada()) == 4)
})

cat("OK: Como sortear a amostra conferido de ponta a ponta (marco, quatro métodos, código, planilha de coleta)\n")
