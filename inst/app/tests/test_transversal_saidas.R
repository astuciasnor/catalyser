# Executar de catalyser/inst/app. Exercita o formulário e as duas saídas reais.
library(shiny)
if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a) || !length(a)) b else a
source("modules/ficha_planejamento.R", encoding = "UTF-8")
source("modules/mod_planejamento_variaveis.R", encoding = "UTF-8")
source("modules/mod_planejamento_observacional.R", encoding = "UTF-8")

destino <- Sys.getenv("TRANSVERSAL_PREVIA", unset = tempfile("transversal_"))
dir.create(destino, recursive = TRUE, showWarnings = FALSE)
testServer(mod_planejamento_observacional_server,
  args = list(tipo = "transversal_comparativo", ficha_destino_rv = NULL), {
    session$setInputs(fator_nome = "especie",
      fator_niveis = "Tambaqui, Gurijuba, Pescada-amarela, Pescada-corvina, Uritinga",
      n_uas = 5, tipo_pool = "desigual", pool_grupo_1 = 2, pool_grupo_2 = 1,
      pool_grupo_3 = 1, pool_grupo_4 = 3, pool_grupo_5 = 3,
      pergunta = "Como a composição lipídica das bexigas natatórias varia entre espécies?",
      n_vars_resposta = 1, var_nome_1 = "lipidios", var_unidade_1 = "%")
    session$flushReact()
    coleta <- tabela_coleta_dados()
    resumo <- resumo_transversal()
    stopifnot(nrow(coleta) == 25L, !anyDuplicated(coleta$ua),
      sum(resumo[["Itens previstos"]]) == 50L,
      identical(resumo[["Itens por UA"]], c(2L, 1L, 1L, 3L, 3L)),
      all(coleta$lipidios == ""), all(coleta$lote_origem == ""))
    texto <- texto_metodologia_artigo_str()
    stopifnot(grepl("Será realizado", texto), grepl("A definir", texto),
      !grepl("foi aplicada|Foram analisadas", texto))
    excel <- file.path(destino, "coleta_transversal.xlsx")
    word <- file.path(destino, "metodologia_transversal.docx")
    escrever_excel_transversal(coleta, orientacoes_transversal(), excel)
    escrever_word_transversal(word, input$pergunta, texto, resumo,
      dicionario_dados(), cuidados_transversal, coleta, esquema_transversal())
    ggplot2::ggsave(file.path(destino, "esquema_transversal.png"), esquema_transversal(),
      width = 10, height = 6.1, dpi = 150, bg = "white")
    stopifnot(identical(openxlsx::getSheetNames(excel), c("coleta", "orientacoes")),
      nrow(openxlsx::read.xlsx(excel, sheet = "coleta")) == 25L)
    conteudo <- officer::docx_summary(officer::read_docx(word))$text
    stopifnot(any(grepl("Distribuição prevista", conteudo)),
      any(grepl("50 itens", conteudo)),
      any(grepl("Planilha de coleta", conteudo)))
    # O Word contém a figura e duas seções horizontais, seguidas de retrato.
    pasta_doc <- tempfile("word_xml_")
    dir.create(pasta_doc)
    utils::unzip(word, exdir = pasta_doc)
    xml <- xml2::read_xml(file.path(pasta_doc, "word", "document.xml"))
    ns <- xml2::xml_ns(xml)
    paginas <- xml2::xml_find_all(xml, "//w:sectPr/w:pgSz", ns)
    stopifnot(sum(xml2::xml_attr(paginas, "orient") == "landscape", na.rm = TRUE) == 2L,
      length(xml2::xml_find_all(xml, "//w:drawing", ns)) == 1L,
      length(xml2::xml_find_all(xml, "//w:tbl", ns)) == 3L)
    unlink(pasta_doc, recursive = TRUE)
    # Uma resposta não pode sobrescrever o identificador da UA.
    session$setInputs(var_nome_1 = "ua")
    stopifnot(inherits(try(tabela_coleta_dados(), silent = TRUE), "try-error"))
    session$setInputs(var_nome_1 = "lipidios")
    session$flushReact()
    # Categorias podem ter números distintos de UAs, com pool próprio.
    session$setInputs(uas_grupo_1 = 2, uas_grupo_2 = 3, uas_grupo_3 = 4,
      uas_grupo_4 = 5, uas_grupo_5 = 6)
    session$flushReact()
    coleta_desigual <- tabela_coleta_dados()
    resumo_desigual <- resumo_transversal()
    stopifnot(nrow(coleta_desigual) == 20L,
      identical(resumo_desigual$UAs, 2:6),
      sum(resumo_desigual[["Itens previstos"]]) == 44L,
      identical(as.integer(table(factor(coleta_desigual$especie,
        levels = resumo_desigual$Grupo))), 2:6),
      !anyDuplicated(coleta_desigual$ua),
      grepl("20 unidades amostrais", texto_metodologia_artigo_str(), fixed = TRUE),
      !grepl("unidades amostrais por grupo", texto_metodologia_artigo_str(), fixed = TRUE))
    escrever_excel_transversal(coleta_desigual, orientacoes_transversal(),
      file.path(destino, "coleta_transversal_uas_desiguais.xlsx"))
    stopifnot(nrow(openxlsx::read.xlsx(file.path(destino,
      "coleta_transversal_uas_desiguais.xlsx"), sheet = "coleta")) == 20L)
    word_desigual <- file.path(destino, "metodologia_transversal_uas_desiguais.docx")
    escrever_word_transversal(word_desigual, input$pergunta,
      texto_metodologia_artigo_str(), resumo_desigual, dicionario_dados(),
      cuidados_transversal, coleta_desigual, esquema_transversal())
    stopifnot(any(grepl("20 unidades amostrais",
      officer::docx_summary(officer::read_docx(word_desigual))$text, fixed = TRUE)))
    session$setInputs(usar_fator2 = TRUE, fator_nome = "especie",
      fator_niveis = "Espécie A, Espécie B", fator2_nome = "sexo",
      fator2_niveis = "Fêmea, Macho")
    session$flushReact()
    dupla <- tabela_coleta_dados()
    stopifnot(nrow(dupla) == 20L, all(c("especie", "sexo") %in% names(dupla)),
      all(table(dupla$especie, dupla$sexo) == 5L),
      sum(resumo_transversal()[["Itens previstos"]]) == 20L,
      "sexo" %in% dicionario_dados()$coluna,
      grepl("especie e sexo", texto_metodologia_artigo_str(), fixed = TRUE))
    session$setInputs(pool_combinacao_1 = 2)
    session$flushReact()
    stopifnot(sum(resumo_transversal()[["Itens previstos"]]) == 25L,
      sum(tabela_coleta_dados()$pool) == 25L)
    escrever_excel_transversal(tabela_coleta_dados(), orientacoes_transversal(),
      file.path(destino, "coleta_transversal_dois_fatores.xlsx"))
    escrever_word_transversal(file.path(destino, "metodologia_transversal_dois_fatores.docx"),
      input$pergunta, texto_metodologia_artigo_str(), resumo_transversal(),
      dicionario_dados(), cuidados_transversal, tabela_coleta_dados(), esquema_transversal())
    ggplot2::ggsave(file.path(destino, "esquema_transversal_dois_fatores.png"), esquema_transversal(),
      width = 10, height = 6.1, dpi = 150, bg = "white")
    session$setInputs(usar_fator2 = FALSE)
    session$flushReact()
    stopifnot(!"sexo" %in% names(tabela_coleta_dados()))
})
ui <- as.character(mod_planejamento_observacional_ui("teste", "transversal_comparativo"))
stopifnot(!grepl("teste-baixar_projeto|teste-baixar_dicionario", ui),
  grepl("teste-baixar_planilha", ui), grepl("teste-baixar_relatorio", ui))
stopifnot(grepl("teste-ui_amostra_categorias", ui),
  !grepl('id="teste-n_uas"|id="teste-tipo_pool"', ui))
stopifnot(grepl('data-value="Definições"', ui), grepl('data-value="Desenho"', ui), grepl('data-value="Resumo"', ui),
  lengths(regmatches(ui, gregexpr('id="teste-baixar_planilha"', ui))) == 1L,
  lengths(regmatches(ui, gregexpr('id="teste-baixar_relatorio"', ui))) == 1L)
cat("Transversal: planilha e Word verificados. Prévia em", destino, "\n")
