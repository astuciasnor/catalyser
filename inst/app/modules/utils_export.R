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
