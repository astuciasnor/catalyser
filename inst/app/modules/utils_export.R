# Funções Utilitárias para Exportação de Dados - EAPA

#' Exporta um Data Frame para Excel (.xlsx) com Dicionário de Variáveis
#'
#' @param df Data frame de dados limpos.
#' @param dataset_name Nome do conjunto de dados (ou sheet) para obter metadados.
#' @param file_path Caminho do arquivo de destino .xlsx.
export_to_xlsx <- function(df, dataset_name, file_path) {
  # Limpa o nome do dataset para comparação case-insensitive e sem espaços
  ds_name <- tolower(trimws(dataset_name))
  cols <- names(df)
  
  # Dicionário estático para os datasets conhecidos da EAPA e colunas comuns
  descriptions <- sapply(cols, function(col) {
    # Mapeamento padrão para nomes comuns (independente de dataset)
    desc <- switch(col,
      "id" = "Identificador único do espécime ou da observação.",
      "sexo" = "Sexo do organismo (Macho, Fêmea).",
      "Sexo" = "Sexo do organismo (fator: Macho, Fêmea).",
      "comp_cm" = "Comprimento total do indivíduo, em centímetros (cm).",
      "comprimento_cm" = "Comprimento total do indivíduo, em centímetros (cm).",
      "peso_g" = "Peso total do indivíduo, em gramas (g).",
      "estacao" = "Estação ou período do ano correspondente à coleta (Ex: Seca, Chuvosa).",
      "Estacao" = "Estação do ano na captura (fator: Seca, Chuvosa).",
      NA_character_
    )
    
    # Se não mapeou por nome genérico, tenta mapear especificando o dataset
    if (is.na(desc)) {
      if (ds_name == "artemia") {
        desc <- switch(col,
          "racao" = "Tipo de ração utilizada no experimento (fator: A - farelo de arroz, B - farelo de babaçu).",
          "taxa_crescimento_mg_dia" = "Taxa de crescimento média diária das artêmias, em miligramas por dia (mg/dia).",
          NA_character_
        )
      } else if (ds_name %in% c("biometria_caranguejos", "crabs")) {
        desc <- switch(col,
          "Local" = "Local de captura dos caranguejos (fator: Ajuruteua, Caratateua).",
          "LC" = "Largura da Carapaça do caranguejo, em milímetros (mm).",
          "CC" = "Comprimento da Carapaça do caranguejo, em milímetros (mm).",
          NA_character_
        )
      } else if (ds_name == "camaroes_sexo") {
        desc <- switch(col,
          "especie" = "Espécie do camarão (fator: P. brasiliensis, P. schmitti).",
          NA_character_
        )
      } else if (ds_name == "cangulo_crescimento") {
        desc <- switch(col,
          "p1" = "Primeira medição individual de peso do peixe, em gramas (g).",
          "p2" = "Segunda medição individual de peso do peixe, em gramas (g).",
          "p3" = "Terceira medição individual de peso do peixe, em gramas (g).",
          "ln_peso" = "Logaritmo natural do peso médio (ln(peso_g)).",
          "ln_comp" = "Logaritmo natural do comprimento (ln(comprimento_cm)).",
          NA_character_
        )
      } else if (ds_name == "captura_petrechos") {
        desc <- switch(col,
          "Especie" = "Espécie de peixe capturada (fator: Sardinha, Corvina, Pescada).",
          "Petrecho" = "Aparelho ou petrecho de pesca utilizado (fator: Rede de Emalhe, Arrasto de Fundo, Linha de Anzol).",
          "CPUE" = "Captura por Unidade de Esforço (número de indivíduos capturados por hora de pesca).",
          NA_character_
        )
      } else if (ds_name == "isoproteica_bagre") {
        desc <- switch(col,
          "racao" = "Ração comercial oferecida (fator: A, B, C, D - todas isoproteicas mas com diferentes níveis de lipídios).",
          "peso_g" = "Peso médio final dos bagres por gaiola, em gramas (g).",
          NA_character_
        )
      } else if (ds_name == "tilapia_crescimento") {
        desc <- switch(col,
          "Tratamento" = "Formulação de ração testada (fator: A, B, C).",
          "Semana" = "Semana de coleta das medições de biometria (inteiro, 1 a 10).",
          "PesoMedio" = "Peso médio semanal das tilápias no respectivo tratamento, em gramas (g).",
          "DataColeta" = "Data da realização da medição do peso médio.",
          NA_character_
        )
      }
    }
    
    # Se ainda estiver sem descrição, gera uma descrição genérica com base no tipo
    if (is.na(desc)) {
      type_val <- class(df[[col]])[1]
      if (type_val %in% c("numeric", "integer", "double")) {
        desc <- "Variável numérica carregada do arquivo original."
      } else if (type_val %in% c("factor", "character", "logical")) {
        desc <- "Variável categórica/fator carregada do arquivo original."
      } else {
        desc <- "Variável de dados carregada do arquivo original."
      }
    }
    
    return(desc)
  })
  
  # Cria o data frame do Dicionário de Variáveis
  dict_df <- data.frame(
    Variavel = cols,
    Tipo = sapply(df, function(x) class(x)[1]),
    Descricao = descriptions,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  
  # Exporta ambas as planilhas usando o writexl
  writexl::write_xlsx(
    x = list(
      Dados = df,
      Dicionario_Variaveis = dict_df
    ),
    path = file_path
  )
}

# =============================================================================
# Helpers de exportação COMPARTILHADOS (refatoração — antes copiados em ~11 módulos)
# -----------------------------------------------------------------------------
# Reúnem o pipeline comum: copiar template + funções + dados para um diretório
# temporário, (opcionalmente) customizar os parâmetros do .qmd e renderizar/zipar.
# Cada módulo passa só o que é seu: nome do .qmd, nome do funcoes_*.R, os dados,
# uma função de customização do .qmd e o código R do script.
# =============================================================================

#' Renderiza um relatório .docx a partir de um template .qmd (via Quarto CLI).
#'
#' @param file        arquivo de saída (do downloadHandler).
#' @param qmd_name    nome do template em templates/ (ex.: "relatorio_teste_t.qmd").
#' @param funcoes_name nome do arquivo de funções em templates/ (ex.: "funcoes_teste_t.R").
#' @param df_clean    data.frame de dados limpos (salvo como dados_limpos.rda).
#' @param customizar_qmd função opcional (caminho_qmd) -> character com as linhas
#'   já ajustadas do .qmd; se NULL, usa o template como está.
render_relatorio_docx <- function(file, qmd_name, funcoes_name, df_clean, customizar_qmd = NULL) {
  temp_dir <- tempdir()
  temp_qmd <- file.path(temp_dir, qmd_name)
  file.copy("templates/custom-reference.docx", file.path(temp_dir, "custom-reference.docx"), overwrite = TRUE)
  file.copy(file.path("templates", funcoes_name), file.path(temp_dir, funcoes_name), overwrite = TRUE)
  file.copy(file.path("templates", qmd_name), temp_qmd, overwrite = TRUE)
  save(df_clean, file = file.path(temp_dir, "dados_limpos.rda"))

  if (is.function(customizar_qmd)) writeLines(customizar_qmd(temp_qmd), temp_qmd)

  old_wd <- getwd(); on.exit(setwd(old_wd), add = TRUE); setwd(temp_dir)
  system2("quarto", args = c("render", qmd_name, "--to", "docx"))
  gerado <- file.path(temp_dir, sub("\\.qmd$", ".docx", qmd_name))
  if (file.exists(gerado)) file.copy(gerado, file, overwrite = TRUE)
  else writeLines("Erro: nao foi possivel renderizar o relatorio .docx com o Quarto CLI.", file)
}

#' Empacota um Projeto R (.zip) de estudo com dados/, scripts/ e relatorios/.
#'
#' @param file        arquivo de saída (do downloadHandler).
#' @param prefix      prefixo do projeto/arquivo (ex.: "teste_t").
#' @param qmd_name    nome do template .qmd em templates/.
#' @param funcoes_name nome do funcoes_*.R em templates/.
#' @param df_clean    data.frame de dados limpos.
#' @param info        import_info() — para nomear a planilha no dicionário.
#' @param r_code      character único (o script .R de reprodutibilidade).
#' @param customizar_qmd função opcional (caminho_qmd) -> character; se NULL, copia o template.
#' @param readme      linhas do README.txt (opcional; usa um padrão se NULL).
exportar_projeto_zip <- function(file, prefix, qmd_name, funcoes_name, df_clean, info,
                                 r_code, customizar_qmd = NULL, readme = NULL) {
  proj <- paste0("projeto_", prefix, "_", format(Sys.Date(), "%Y-%m-%d"))
  temp_dir <- tempdir(); pd <- file.path(temp_dir, proj)
  dir.create(pd, showWarnings = FALSE)
  dd <- file.path(pd, "dados"); sc <- file.path(pd, "scripts"); rl <- file.path(pd, "relatorios")
  for (x in c(dd, sc, rl)) dir.create(x, showWarnings = FALSE)

  save(df_clean, file = file.path(dd, "dados_limpos.rda"))
  utils::write.csv(df_clean, file = file.path(dd, "dados_limpos.csv"), row.names = FALSE)
  ds_name <- if (!is.null(info) && identical(info$source, "package")) info$package_dataset
             else if (!is.null(info)) info$excel_sheet else "dados"
  tryCatch(export_to_xlsx(df_clean, dataset_name = ds_name, file_path = file.path(dd, "dados_limpos.xlsx")),
           error = function(e) NULL)

  writeLines(r_code, file.path(sc, paste0(prefix, ".R")))
  file.copy("templates/custom-reference.docx", file.path(rl, "custom-reference.docx"), overwrite = TRUE)
  file.copy(file.path("templates", funcoes_name), file.path(sc, funcoes_name), overwrite = TRUE)
  if (is.function(customizar_qmd)) writeLines(customizar_qmd(file.path("templates", qmd_name)), file.path(rl, qmd_name))
  else file.copy(file.path("templates", qmd_name), file.path(rl, qmd_name), overwrite = TRUE)

  writeLines(c("Version: 1.0", "RestoreWorkspace: Default", "SaveWorkspace: Default", "Encoding: UTF-8"),
             file.path(pd, "projeto_analise.Rproj"))
  if (is.null(readme)) readme <- c(
    paste0("PACOTE DE ESTUDO: ", toupper(prefix), " (CatalyseR)"),
    "- projeto_analise.Rproj: duplo clique para abrir no RStudio.",
    "- dados/     : dados limpos em .rda, .csv e .xlsx.",
    "- scripts/   : script da analise e funcoes de apoio.",
    "- relatorios/: relatorio Quarto (.qmd) e template Word.")
  writeLines(readme, file.path(pd, "README.txt"))

  old_wd <- getwd(); on.exit(setwd(old_wd), add = TRUE); setwd(temp_dir)
  zip::zip(file, files = proj)
}
