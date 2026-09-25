# Executar a partir de catalyser/inst/app.
# Contrato 1 do ARQUITETURA.md: a ficha é um objeto só, com dez campos,
# preenchida por partes e levada à planilha e ao Projeto R.
library(shiny)
if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a) || !length(a)) b else a
source(file.path("modules", "ficha_planejamento.R"), encoding = "UTF-8")
source(file.path("modules", "mod_n_poder.R"), encoding = "UTF-8")

# 1. Ficha nova: os dez campos existem e começam em NULL.
f <- ficha_nova()
stopifnot(identical(names(f), ficha_campos), all(vapply(f, is.null, logical(1))))

# 2. Partes de módulos diferentes somam sem apagar o que a outra trouxe.
f <- ficha_mesclar(f, list(tipo = "comparativo", unidade_coluna = "praia",
                           hierarquia = list(sitios_por_nivel = 3, subamostra_coluna = "arrasto"),
                           analise_sugerida = "anova_mista_subamostras"))
res_n <- calcular_n_poder("anova", c(10, 12, 14), 3, 0.8, 0.05)
f <- ficha_mesclar(f, ficha_parte_n(res_n))
f <- ficha_mesclar(f, list(hierarquia = list(colunas_id = c("praia", "arrasto"))))
stopifnot(
  identical(names(f), ficha_campos),
  f$n_planejado$valor == res_n$n_por_grupo,
  f$hierarquia$sitios_por_nivel == 3,           # a hierarquia foi mesclada por dentro
  identical(f$hierarquia$colunas_id, c("praia", "arrasto")),
  is.null(f$sorteio), is.null(f$pergunta)       # desconhecidos seguem NULL
)

# 3. O sorteio entra com marco, identificador, método, semente e unidades.
s <- list(registro = list(metodo = "aas", semente = 7L, id = "peixe", N = 5, n = 2),
          sorteados = data.frame(peixe = c("P2", "P4")), marco = data.frame(peixe = paste0("P", 1:5)))
f <- ficha_mesclar(f, ficha_parte_sorteio(s))
stopifnot(f$sorteio$semente == 7L, identical(f$sorteio$unidades_selecionadas, c("P2", "P4")),
          nrow(f$sorteio$marco) == 5, f$sorteio$parametros$N == 5)

# 4. Tabela para a planilha e objeto salvo no projeto.
tab <- ficha_tabela(f)
stopifnot(identical(tab$campo, ficha_campos), grepl("NULL", tab$valor[tab$campo == "pergunta"]))
pasta <- tempfile("ficha_"); dir.create(pasta)
linhas <- ficha_salvar_projeto(f, pasta)
relida <- readRDS(file.path(pasta, "dados", "ficha_planejamento.rds"))
stopifnot(identical(relida, f), !dir.exists(file.path(pasta, "metadados")), file.exists(file.path(pasta, "dados", "ficha_planejamento.csv")),
          any(grepl("readRDS", linhas, fixed = TRUE)))
unlink(pasta, recursive = TRUE)

# 5. O n calculado em Quantos coletar chega à ficha compartilhada.
compartilhada <- reactiveVal(ficha_mesclar(NULL, list(tipo = "comparativo")))
testServer(mod_n_poder_server, args = list(ficha_destino_rv = compartilhada), {
  session$setInputs(metodo = "t", media_a = 10, media_b = 12, desvio = 3, poder = 0.8, alfa = "0.05", calcular = 1)
  session$flushReact()
  stopifnot(identical(compartilhada()$tipo, "comparativo"), compartilhada()$n_planejado$valor >= 2)
})

# 5b. As curvas t da visualização seguem as médias digitadas, na ordem, com a
# grade de quatro desvios-padrão para cada lado e densidade não negativa.
curvas_t <- dados_curvas_n_poder(calcular_n_poder("t", c(10, 12), 3, 0.8, 0.05))
stopifnot(nlevels(curvas_t$grupo) == 2L, startsWith(levels(curvas_t$grupo)[1], "Grupo A"),
          identical(sort(unique(curvas_t$media)), c(10, 12)), all(curvas_t$y >= 0),
          max(curvas_t$x) <= 12 + 4 * 3 + 1e-8, min(curvas_t$x) >= 10 - 4 * 3 - 1e-8)
curvas_a <- dados_curvas_n_poder(calcular_n_poder("anova", c(10, 12, 14), 3, 0.8, 0.05))
stopifnot(nlevels(curvas_a$grupo) == 3L, startsWith(levels(curvas_a$grupo)[3], "Grupo 3"))

# 6. Fio visível: cabeçalho e dica leem a ficha; sem delineamento, aviso gentil.
vazio <- as.character(ficha_cabecalho_ui(ficha_nova()))
stopifnot(grepl("monte o delineamento primeiro", vazio, ignore.case = TRUE))
comparativo <- ficha_mesclar(NULL, list(tipo = "dic", eixos = list(grupos = list(coluna = "especie", niveis = 5)),
                                        analise_sugerida = "anova_um_fator"))
cab <- as.character(ficha_cabecalho_ui(comparativo))
stopifnot(grepl("Planejamento atual", cab), grepl("fator especie", cab), grepl("5 níveis", cab),
          grepl("Análise futura", cab), ficha_niveis(comparativo) == 5L)
sem_dica <- comparativo; sem_dica["analise_sugerida"] <- list(NULL)
stopifnot(is.null(ficha_dica_analise_ui(sem_dica)))

# 7. Os níveis do planejamento preenchem o número de grupos em Quantos coletar.
compartilhada(comparativo)
testServer(mod_n_poder_server, args = list(ficha_destino_rv = compartilhada), {
  session$flushReact()
  stopifnot(identical(niveis_ficha(), 5L))
})

# 8. Estimar uma média: n = teto((z * desvio / margem)^2); a correção de
# população finita reduz o n e o bruto fica guardado.
res_m <- calcular_n_media(desvio = 3, margem = 1, confianca = 0.95)
stopifnot(identical(res_m$n, 35L), identical(res_m$n_total, 35L), is.null(res_m$N))
res_mf <- calcular_n_media(desvio = 3, margem = 1, confianca = 0.95, N = 100)
stopifnot(res_mf$n < res_mf$n_bruto, identical(res_mf$N, 100))

# 9. Estimar uma proporção: p = 0,5, E = 0,1 e 95% dá 97; com N = 200 cai a 66.
res_p <- calcular_n_proporcao(p = 0.5, margem = 0.1, confianca = 0.95)
stopifnot(identical(res_p$n, 97L))
res_pf <- calcular_n_proporcao(p = 0.5, margem = 0.1, confianca = 0.95, N = 200)
stopifnot(identical(res_pf$n, 66L), identical(res_pf$n_bruto, 97L))

# 10. Comparar proporções: 0,4 x 0,6 com poder 0,8 e alfa 0,05, coerente com
# pwr::pwr.2p.test; o total é o dobro do n por grupo.
res_2p <- calcular_n_duas_prop(0.4, 0.6, 0.8, 0.05)
esperado_2p <- as.integer(ceiling(pwr::pwr.2p.test(h = abs(pwr::ES.h(0.4, 0.6)), sig.level = 0.05, power = 0.8)$n))
stopifnot(identical(res_2p$n, esperado_2p), identical(res_2p$n_total, 2L * res_2p$n))

# 11. Relação entre variáveis: r = 0,3, coerente com pwr::pwr.r.test; a ficha
# guarda o n total e o método, e o cabeçalho lê a unidade "no total".
res_r <- calcular_n_correlacao(0.3, 0.8, 0.05)
esperado_r <- as.integer(ceiling(pwr::pwr.r.test(r = 0.3, sig.level = 0.05, power = 0.8)$n))
stopifnot(identical(res_r$n, esperado_r))
f_r <- ficha_mesclar(NULL, list(tipo = "comparativo"))
f_r <- ficha_mesclar(f_r, ficha_parte_n_calculo(res_r))
stopifnot(identical(f_r$n_planejado$valor, res_r$n), grepl("pwr.r.test", f_r$n_planejado$metodo),
          grepl("no total", as.character(ficha_cabecalho_ui(f_r)), fixed = TRUE))

# 12. Aba de estimação grava o n total na ficha compartilhada.
compartilhada <- reactiveVal(ficha_mesclar(NULL, list(tipo = "comparativo")))
testServer(mod_n_proporcao_server, args = list(ficha_destino_rv = compartilhada), {
  session$setInputs(p = 0.5, margem = 0.1, confianca = "0.95", pf_usar = FALSE, calcular = 1)
  session$flushReact()
  stopifnot(identical(compartilhada()$n_planejado$valor, 97L),
            identical(compartilhada()$n_planejado$total, 97L))
})

# 13. Aba de duas proporções grava por grupo e total; ligar a população finita
# reduz o n gravado.
compartilhada(ficha_mesclar(NULL, list(tipo = "comparativo")))
testServer(mod_n_duas_prop_server, args = list(ficha_destino_rv = compartilhada), {
  session$setInputs(p1 = 0.4, p2 = 0.6, poder = 0.8, alfa = "0.05", pf_usar = FALSE, calcular = 1)
  session$flushReact()
  stopifnot(identical(compartilhada()$n_planejado$valor, res_2p$n),
            identical(compartilhada()$n_planejado$total, 2L * res_2p$n))
  session$setInputs(pf_usar = TRUE, pf_N = 200, calcular = 2)
  session$flushReact()
  stopifnot(compartilhada()$n_planejado$valor < res_2p$n)
})

# 14. Curva da distribuição do estimador com a faixa do IC: a grade cobre
# centro ± 4 EP, a densidade não é negativa, a meia largura é z * EP, os
# limites [0, 1] da proporção são respeitados e a população finita estreita
# o erro-padrão.
ep_livre <- erro_padrao_ic(3, 66)
ep_finito <- erro_padrao_ic(3, 66, N = 200)
stopifnot(ep_finito < ep_livre,
          isTRUE(all.equal(ep_livre, 3 / sqrt(66))))
info <- dados_curva_ic(10, ep_livre, 0.95)
stopifnot(nrow(info$curva) == 200,
          min(info$curva$x) >= 10 - 4 * ep_livre - 1e-9,
          max(info$curva$x) <= 10 + 4 * ep_livre + 1e-9,
          all(info$curva$y >= 0),
          isTRUE(all.equal(info$meia_largura, stats::qnorm(0.975) * ep_livre)))
info_p <- dados_curva_ic(0.5, 0.4, 0.95, limites = c(0, 1))
stopifnot(min(info_p$curva$x) >= 0, max(info_p$curva$x) <= 1)

# 15. Aba "Uma média": o centro da curva é a média esperada digitada e a
# ficha recebe o n (desvio 3, margem 1 e 95% dão 35).
compartilhada(ficha_mesclar(NULL, list(tipo = "comparativo")))
testServer(mod_n_media_server, args = list(ficha_destino_rv = compartilhada), {
  session$setInputs(media = 10, desvio = 3, margem = 1, confianca = "0.95",
                    pf_usar = FALSE, calcular = 1)
  session$flushReact()
  stopifnot(identical(resultado()$centro, 10),
            identical(compartilhada()$n_planejado$valor, 35L))
})

# 16. Curvas de poder das abas de comparação: as proporções amostrais ficam
# em [0, 1] com uma curva por grupo; a correlação junta as curvas sob H0 e
# sob a esperada na escala r (jacobiano da z de Fisher), com as marcas nos
# dois centros; os gráficos montam sem erro e a sub-aba gráfica responde
# depois do cálculo.
curvas_2p <- dados_curvas_duas_prop(res_2p)
stopifnot(nlevels(curvas_2p$grupo) == 2L,
          nrow(curvas_2p) == 400L,
          all(curvas_2p$x >= 0 & curvas_2p$x <= 1),
          all(curvas_2p$y >= 0),
          isTRUE(all.equal(curvas_2p$marca, rep(c(0.4, 0.6), each = 200))))
stopifnot(inherits(grafico_poder_duas_prop(res_2p), "ggplot"))
curvas_r <- dados_curvas_correlacao(res_r)
stopifnot(nlevels(curvas_r$curva) == 2L,
          nrow(curvas_r) == 400L,
          all(curvas_r$y >= 0),
          all(abs(curvas_r$x) < 1),
          isTRUE(all.equal(curvas_r$marca, rep(c(0, 0.3), each = 200))))
stopifnot(inherits(grafico_poder_correlacao(res_r), "ggplot"))
compartilhada(ficha_mesclar(NULL, list(tipo = "comparativo")))
testServer(mod_n_duas_prop_server, args = list(ficha_destino_rv = compartilhada), {
  session$setInputs(p1 = 0.4, p2 = 0.6, poder = 0.8, alfa = "0.05", pf_usar = FALSE, calcular = 1)
  session$flushReact()
  # O eventReactive antes do primeiro clique suspende a saída (req silencioso),
  # então a sub-aba só é lida depois do cálculo.
  stopifnot(any(grepl("grafico_ic", as.character(output$grafico_ui), fixed = TRUE)))
})
testServer(mod_n_correlacao_server, args = list(ficha_destino_rv = compartilhada), {
  session$setInputs(r = 0.3, poder = 0.8, alfa = "0.05", pf_usar = FALSE, calcular = 1)
  session$flushReact()
  stopifnot(any(grepl("grafico_ic", as.character(output$grafico_ui), fixed = TRUE)),
            identical(compartilhada()$n_planejado$valor, res_r$n))
})

cat("OK: ficha de planejamento — objeto único, partes, planilha e projeto\n")
