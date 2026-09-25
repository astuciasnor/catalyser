source("app.R", local = TRUE)
load("../../../EAPADados/data/abalone_adultos.rda")
base_teste <- shiny::reactiveVal(abalone_adultos)
bases_teste <- shiny::reactiveVal(bases_vazio())
cache_teste <- shiny::reactiveVal(bases_cache_vazio())
revisao_teste <- shiny::reactiveVal(1L)
registros_teste <- shiny::reactiveVal(execucoes_vazio())
contador_teste <- shiny::reactiveVal(0L)
num <- names(abalone_adultos)[vapply(abalone_adultos, is.numeric, logical(1))]
# Os títulos confirmam o percurso por perguntas da interface reformulada.
titulos <- c(explorar = "Explorar Dataset", descrever = "Conhecer as Variáveis",
             relacoes = "Encontrar Relações", pressupostos = "Avaliar Pressupostos",
             transformar = "Transformar Variáveis")
stopifnot(identical(names(descricao_catalogo()), names(titulos)),
          identical(unname(descricao_catalogo()$explorar), "panorama"),
          identical(unname(descricao_catalogo()$descrever), "retrato"),
          identical(unname(descricao_catalogo()$relacoes), "relacao"),
          identical(unname(descricao_catalogo()$transformar), "transformacoes"))
# Confere os rótulos e a ordem dos links no HTML gerado pela aplicação.
pagina <- xml2::read_html(as.character(ui))
links_menu <- xml2::xml_find_all(pagina, "//ul[@id='main_navbar']/li/a")
rotulos_menu <- trimws(xml2::xml_text(links_menu))
rotulos_menu <- rotulos_menu[nzchar(rotulos_menu) & rotulos_menu != "Sobre"]
# Compara posições pelo nome porque o Laboratório usa um atalho próprio no fim da barra.
pos_comunicacao <- which(grepl("Comunicação", rotulos_menu, fixed = TRUE))
pos_ajuda <- which(rotulos_menu == "Ajuda")
stopifnot(grepl("Planejando", rotulos_menu[1], fixed = TRUE),
          length(pos_comunicacao) == 1L, length(pos_ajuda) == 1L,
          pos_comunicacao < pos_ajuda)
links_secoes <- xml2::xml_find_all(pagina,
  "//ul[@id='main_navbar']/li[contains(., 'Explorando')]/ul/li/a")
stopifnot(identical(trimws(xml2::xml_text(links_secoes)), unname(titulos)))
for (area in names(descricao_catalogo())) {
  ui_area <- mod_descrevendo_dados_ui(paste0("teste_", area), area)
  html <- as.character(ui_area)
  if (identical(area, "descrever")) {
    stopifnot(grepl("Conhecer as Variáveis", html, fixed = TRUE),
              grepl("Resumo das contínuas", html, fixed = TRUE),
              grepl("Tabelas de frequência", html, fixed = TRUE),
              grepl("Inserir análise", html, fixed = TRUE),
              grepl("col_widths = c(7, 5)", paste(deparse(body(mod_conhecer_variaveis_ui)), collapse = " "), fixed = TRUE))
    next
  }
  # O título é o primeiro elemento, fora das sub-abas e antes da base.
  # Assim ele também permanece presente na aba Inserir análise, sem dados.
  stopifnot(identical(ui_area[[1]]$name, "h2"),
            identical(ui_area[[1]]$children[[1]], unname(titulos[[area]])),
            identical(ui_area[[1]]$attribs$id, paste0("teste_", area, "-titulo")))
  stopifnot(grepl("Inserir análise", html, fixed = TRUE),
            all(vapply(names(descricao_catalogo()[[area]]), function(s) grepl(s, html, fixed = TRUE), logical(1))))
  # O contêiner comum evita que a janela comprima gráficos e botões.
  doc <- xml2::read_html(html)
  conteudo <- xml2::xml_find_first(doc, sprintf("//*[@id='teste_%s-conteudo']", area))
  stopifnot(identical(xml2::xml_attr(conteudo, "class"), "descricao-conteudo"))
  # Todas as perguntas usam a faixa curta, sem cartão ou status repetido da base.
  seletor_compacto <- xml2::xml_find_all(conteudo, ".//*[contains(@class, 'catalyser-base-selector-compact')]")
  status_base <- xml2::xml_find_all(conteudo, sprintf(".//*[@id='teste_%s-base-status']", area))
  stopifnot(length(seletor_compacto) >= 1L, length(status_base) == 0L,
            !grepl("catalyser-base-selector card", html, fixed = TRUE))
  laterais <- xml2::xml_find_all(conteudo, ".//*[contains(@class, 'bslib-sidebar-layout')]")
  stopifnot(length(laterais) == length(descricao_catalogo()[[area]]),
            !any(grepl("html-fill-item|html-fill-container", xml2::xml_attr(laterais, "class"))))
  for (modo in unname(descricao_catalogo()[[area]])) {
    grafico <- paste0("teste_", area, "-", modo, "_painel_grafico")
    tabela <- paste0("teste_", area, "-", modo, "_tabela")
    stopifnot(regexpr(grafico, html, fixed = TRUE)[1] < regexpr(tabela, html, fixed = TRUE)[1])
  }
}
# A janela gráfica compartilhada mantém todos os gráficos menores e centralizados.
codigo_servidor <- paste(deparse(body(mod_descrevendo_dados_server)), collapse = " ")
stopifnot(grepl("descricao-grafico-compacto", codigo_servidor, fixed = TRUE),
          grepl('height = "320px"', codigo_servidor, fixed = TRUE))
shiny::testServer(mod_descrevendo_dados_server, args = list(
  area = "descrever", dados_rv = base_teste, registro_bases_rv = bases_teste,
  cache_bases_rv = cache_teste, revisao_origem_rv = revisao_teste,
  registro_execucoes_rv = registros_teste, contador_execucoes_rv = contador_teste
), {
  session$setInputs(`base-base_id` = "dados_analise", abas_conhecer = "resumo")
  session$flushReact()
  stopifnot(nrow(resumo_bruto()) == length(variaveis_continuas()),
            identical(estado_execucao()$parametros$analise, "resumo_continuas"))
  session$setInputs(abas_conhecer = "frequencias", variavel_frequencia = "sexo")
  session$flushReact()
  stopifnot(identical(estado_execucao()$parametros$analise, "frequencia_exploratoria"),
            sum(frequencia_bruta()$frequencia) == nrow(abalone_adultos))
  session$setInputs(abas_conhecer = "inserir")
  session$flushReact()
  stopifnot(identical(ultima_saida(), "frequencias"),
            identical(estado_execucao()$parametros$analise, "frequencia_exploratoria"))
})
cat("OK: cinco seções, ordem dos menus e retrato consolidado de variáveis.\n")

# Todas as sub-abas conseguem produzir uma fotografia registrável, inclusive
# usando o ramo com sorteio de 200 indivíduos por sexo.
reg_base <- bases_adicionar(bases_vazio(), bases_novo_registro("amostra", "200 por sexo", "base_amostra", "geral"))
reg_base <- bases_adicionar_etapa(reg_base, "amostra", "sortear_amostra",
  list(coluna = "sexo", n = 200L, semente = 42L), abalone_adultos)
cache_base <- list(amostra = bases_recalcular_cache(abalone_adultos, bases_obter(reg_base, "amostra"), 2L))
reg_base <- bases_finalizar(reg_base, "amostra", cache_base, 2L)
bases_teste(reg_base); cache_teste(cache_base)
for (area_teste in setdiff(names(descricao_catalogo()), "descrever")) {
  shiny::testServer(mod_descrevendo_dados_server, args = list(
    area = area_teste, dados_rv = base_teste, registro_bases_rv = bases_teste,
    cache_bases_rv = cache_teste, revisao_origem_rv = revisao_teste,
    registro_execucoes_rv = registros_teste, contador_execucoes_rv = contador_teste
  ), {
    session$setInputs(`base-base_id` = "amostra")
    stopifnot(nrow(seletor$dados()) == 400L)
    for (m in unname(descricao_catalogo()[[area_teste]])) {
      entradas <- list(variavel = if (m == "frequencias") "sexo" else num[1], outra = num[2],
                       grupo = "sexo", metodo = "spearman", forma = "ambos", classes = 15,
                       tendencia = "linear", limite_z = 3)
      names(entradas) <- paste0(m, "_", names(entradas))
      do.call(session$setInputs, entradas)
      do.call(session$setInputs, stats::setNames(list(1L), paste0(m, "_executar")))
      stopifnot(identical(estado_execucao()$parametros$analise, m))
    }
    stopifnot(length(historico()) == length(descricao_catalogo()[[area_teste]]))
    session$setInputs(`registrar-adicionar` = 1L)
    stopifnot(identical(tail(registros_teste(), 1)[[1]]$analise_id,
                        paste0("descricao_", area_teste)))
    # Uma alteração em outro ramo não invalida estas fotografias.
    bases_teste(bases_adicionar(bases_teste(), bases_novo_registro(
      paste0("outro_", area_teste), "Outro ramo", paste0("base_outro_", area_teste), "geral")))
    session$flushReact()
    stopifnot(length(historico()) == length(descricao_catalogo()[[area_teste]]))
    session$setInputs(`base-base_id` = "dados_analise")
    stopifnot(length(historico()) == 0L)
  })
}
cat("OK: seis retratos e checagens Shiny na base de 400 adultos e isolamento entre ramos.\n")
