# Execute a partir de inst/app, em uma sessão R nova.
Sys.setlocale("LC_CTYPE", "Portuguese_Brazil.utf8")
source("app.R", encoding = "UTF-8")
load("../../../EAPADados/data/pinguins.rda")
vars <- c("especie", "sexo", "comprimento_bico_mm", "profundidade_bico_mm", "comprimento_nadadeira_mm", "massa_g")
dados <- pinguins[vars]
original <- dados
tipos <- exploracao_tipos(dados)
stopifnot(nrow(dados) == 344L, sum(is.na(dados)) == 19L,
          sum(tipos == "Numérica contínua") == 4L,
          exploracao_tipo_variavel(pinguins$ano, "ano") == "Categórica ordinal",
          exploracao_tipo_variavel(1:30, "aneis") == "Numérica discreta",
          exploracao_tipo_variavel(1:30, "diametro_mm") == "Numérica contínua")

# Sem sexo registrado, nove indivíduos ainda têm massa e devem permanecer visíveis.
g <- desenhar_caixa_ocean(dados, "massa_g", "sexo", "pontos")
oculto <- desenhar_caixa_ocean(dados, "massa_g", "sexo", "pontos", mostrar_ausentes = FALSE)
stopifnot(nrow(g$data) == 342L, sum(g$data$grupo_visual == "sem registro") == 9L,
          nrow(oculto$data) == 333L)
ggplot2::ggplot_build(g)
pequeno <- data.frame(resposta = c(4, 6, 8), grupo = c("A", "B", "B"))
ggplot2::ggplot_build(desenhar_caixa_ocean(pequeno, "resposta", "grupo", "pontos"))

# O mesmo par revela associação negativa no total e positiva em cada espécie.
total <- stats::cor(dados$comprimento_bico_mm, dados$profundidade_bico_mm, use = "complete.obs")
por_especie <- vapply(split(dados, dados$especie), function(d) stats::cor(d$comprimento_bico_mm, d$profundidade_bico_mm, use = "complete.obs"), numeric(1))
stopifnot(total < 0, all(por_especie > 0))
por_grupo <- desenhar_dispersao_ocean(dados, "comprimento_bico_mm", "profundidade_bico_mm", "especie", "lm", linha_por_grupo = TRUE)
geral <- desenhar_dispersao_ocean(dados, "comprimento_bico_mm", "profundidade_bico_mm", "especie", "lm", linha_por_grupo = FALSE)
stopifnot(length(unique(ggplot2::ggplot_build(por_grupo)$data[[2]]$group)) == 3L,
          length(unique(ggplot2::ggplot_build(geral)$data[[2]]$group)) == 1L)

# As leituras confirmadas sobrevivem ao código independente, sem depender da sessão Shiny.
p <- list(analise = "panorama", leituras = tipos)
resultado <- catalyser_descricao(dados, p)
ambiente <- new.env(parent = baseenv()); ambiente$dados <- dados
eval(parse(text = catalyser_codigo_descricao(p)), ambiente)
stopifnot(identical(resultado$tabela, ambiente$resultado$tabela))
normalidade <- catalyser_descricao(dados, list(analise = "normalidade", variavel = "massa_g", grupo = "especie"))
stopifnot(nrow(normalidade$tabela) == 3L, sum(normalidade$tabela$n) == 342L)
ficha <- shiny::reactiveVal(list())
shiny::isolate({
  exploracao_ficha_gravar(ficha, pinguins, "ano", "Categórica nominal")
  stopifnot(exploracao_tipos(pinguins, exploracao_ficha_ler(ficha, pinguins))[["ano"]] == "Categórica nominal")
  alterada <- pinguins; alterada$massa_g[1] <- 9999
  stopifnot(is.null(exploracao_ficha_ler(ficha, alterada)))
})

# A matriz mista e os gráficos grandes precisam ser desenháveis com dados reais.
matriz <- GGally::ggpairs(dados, progress = FALSE)
load("../../../EAPADados/data/abalone_adultos.rda")
ggplot2::ggplot_build(desenhar_dispersao_ocean(abalone_adultos, "comprimento_mm", "peso_total_g", tendencia = "nenhuma", muitos_pontos = "hex"))
stopifnot(identical(dados, original))

# Verifique os controles, o código exibido e a passagem da ficha no servidor visual.
shiny::testServer(mod_exploracao_visual_server,
  args = list(data_rv = shiny::reactiveVal(dados), tipo = "dispersao", ficha_rv = ficha), {
    session$setInputs(x = "comprimento_bico_mm", y = "profundidade_bico_mm", grupo = "especie", suavizacao = "lm", linha_por_grupo = FALSE, muitos_pontos = "pontos", faceta = "nenhuma")
    stopifnot(grepl("linha_por_grupo = FALSE", output$codigo, fixed = TRUE))
  })

pasta <- "../../../APOIO/verificacao-exploracao"
dir.create(pasta, recursive = TRUE, showWarnings = FALSE)
ggplot2::ggsave(file.path(pasta, "retratos.png"), resultado$grafico, width = 13, height = 7, dpi = 120)
ggplot2::ggsave(file.path(pasta, "comparar-grupos.png"), g, width = 10, height = 6, dpi = 120)
ggplot2::ggsave(file.path(pasta, "ausentes.png"), exploracao_mapa_ausentes(dados), width = 10, height = 6, dpi = 120)
ggplot2::ggsave(file.path(pasta, "matriz.png"), matriz, width = 13, height = 12, dpi = 100)
cat("OK: guia v2, pontos, ausências, tipos, grupos, ficha, matriz e replay.\n")
