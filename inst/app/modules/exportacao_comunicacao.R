# Exportação integrada da Comunicação de Resultados — Fase 3E
# -----------------------------------------------------------------------------
# O manifesto editorial escolhe o conteúdo do Word. O registro central de
# execuções, por sua vez, sempre é preservado integralmente no Projeto R.

exportacao_nome_seguro <- function(x, padrao = "analise") {
  x <- trimws(as.character(x %||% ""))
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT", sub = "")
  x <- tolower(gsub("[^A-Za-z0-9]+", "_", x))
  x <- gsub("^_+|_+$", "", x)
  if (!nzchar(x)) padrao else x
}

exportacao_dput_texto <- function(x) {
  paste(capture.output(dput(x)), collapse = "\n")
}

exportacao_validar_manifesto <- function(manifesto, exigir_word = FALSE) {
  mensagens <- character()
  if (!is.list(manifesto) || !length(manifesto$execucoes %||% list())) {
    mensagens <- c(mensagens, "Registre ao menos uma execução antes de exportar.")
  }
  execucoes <- manifesto$execucoes %||% list()
  desatualizadas <- names(Filter(
    function(x) !identical(x$estado_dependencia, "Atualizada"), execucoes
  ))
  if (length(desatualizadas)) {
    mensagens <- c(
      mensagens,
      sprintf(
        "Atualize as execuções dependentes antes de exportar: %s.",
        paste(desatualizadas, collapse = ", ")
      )
    )
  }
  if (isTRUE(exigir_word)) {
    incluidas <- Filter(function(x) isTRUE(x$incluir_word), execucoes)
    if (!length(incluidas)) mensagens <- c(mensagens, "Selecione ao menos uma execução para o Word.")
    sem_conteudo <- names(Filter(
      function(x) isTRUE(x$incluir_word) && !length(x$saidas_word), execucoes
    ))
    if (length(sem_conteudo)) {
      mensagens <- c(
        mensagens,
        sprintf("Escolha o conteúdo do Word para: %s.", paste(sem_conteudo, collapse = ", "))
      )
    }
  }
  list(ok = !length(mensagens), mensagens = mensagens)
}

# ---- Script único da Base Compartilhada -------------------------------------
# O relatório não constrói a Base Compartilhada: ele chama este script com
# source(). O script percorre planilha bruta -> operações estruturais -> trilha
# de tratamentos e deixa `dados_analise` na memória.

exportacao_nome_planilha <- function(import_info = list()) {
  origem <- if (identical(import_info$source, "package")) {
    import_info$package_dataset
  } else {
    sub("[.][A-Za-z0-9]+$", "", import_info$file_name %||% "")
  }
  paste0(exportacao_nome_seguro(origem, "dados_brutos"), ".xlsx")
}

exportacao_aba_planilha <- function(import_info = list()) {
  aba <- if (identical(import_info$source, "package")) {
    import_info$package_dataset
  } else {
    import_info$excel_sheet
  }
  aba <- trimws(as.character(aba %||% ""))
  if (!nzchar(aba)) "dados" else substr(aba, 1, 31)
}

# Passo 1: da planilha ao data.frame bruto.
exportacao_bloco_planilha <- function(import_info = list()) {
  planilha <- exportacao_nome_planilha(import_info)
  aba <- exportacao_aba_planilha(import_info)
  c(
    "# -----------------------------------------------------------------------",
    "# 1. Planilha bruta",
    "# -----------------------------------------------------------------------",
    "# A CatalyseR exportou os dados de entrada como planilha para que o projeto",
    "# comece onde a sua análise realmente começou. Troque o caminho abaixo se",
    "# quiser apontar para a sua própria planilha.",
    sprintf("caminho_planilha <- file.path('dados', '%s')", planilha),
    sprintf("aba_planilha <- '%s'", aba),
    "",
    "if (!requireNamespace('readxl', quietly = TRUE)) {",
    "  stop(",
    "    \"Instale o pacote readxl para ler a planilha: install.packages('readxl')\",",
    "    call. = FALSE",
    "  )",
    "}",
    "dados_brutos <- as.data.frame(",
    "  readxl::read_excel(caminho_planilha, sheet = aba_planilha)",
    ")",
    ""
  )
}

# Passo 2: operações estruturais (Pivotar/Separar/Organizar).
#
# O código registrado por esses módulos é R executável, mas cada bloco promovido
# recomeça pela leitura da planilha. Blocos acumulados, portanto, não rodam em
# sequência. Por isso o script usa a fotografia da base resolvida e mantém o
# código como referência comentada — a conferência final acusa qualquer desvio.
exportacao_bloco_estrutural <- function(base_externa = NULL) {
  codigo <- trimws(as.character(base_externa$codigo %||% ""))
  if (!nzchar(codigo)) {
    return(c(
      "# -----------------------------------------------------------------------",
      "# 2. Operações estruturais",
      "# -----------------------------------------------------------------------",
      "# Nenhuma mudança estrutural foi promovida: a base resolvida é a própria",
      "# planilha bruta.",
      "base_resolvida <- dados_brutos",
      ""
    ))
  }
  c(
    "# -----------------------------------------------------------------------",
    "# 2. Operações estruturais",
    "# -----------------------------------------------------------------------",
    "# Houve mudança estrutural promovida na CatalyseR (Pivotar/Separar ou",
    "# Criar e Editar Variáveis e Níveis). Para o projeto reproduzir exatamente a base que você",
    "# viu na tela, o script carrega a fotografia materializada na exportação.",
    "base_resolvida <- as.data.frame(readRDS(file.path('dados', 'base_resolvida.rds')))",
    "",
    "# Código registrado da operação estrutural, para estudo. Ele parte da",
    "# planilha; se houver mais de um bloco, execute um de cada vez.",
    paste0("# > ", strsplit(codigo, "\n", fixed = TRUE)[[1]]),
    ""
  )
}

# Passo 3: a trilha de tratamentos compartilhados.
exportacao_bloco_trilha <- function(pipeline, reg = tratamentos) {
  linhas <- c(
    "# -----------------------------------------------------------------------",
    "# 3. Trilha de tratamentos compartilhados",
    "# -----------------------------------------------------------------------",
    "# A ordem abaixo é a ordem lógica registrada na Trilha de Preparo.",
    "dados <- base_resolvida",
    "trat_moda <- catalyser_moda  # a trilha chama a moda por este nome",
    ""
  )
  ativas <- Filter(function(et) isTRUE(et$ativa), pipeline %||% list())
  if (!length(ativas)) {
    linhas <- c(linhas, "# Nenhum tratamento compartilhado foi registrado.", "")
  }
  for (i in seq_along(ativas)) {
    etapa <- ativas[[i]]
    tratamento <- reg[[etapa$tipo]]
    if (is.null(tratamento)) next
    linhas <- c(
      linhas,
      sprintf("# Etapa %d: %s", i, tratamento$rotulo(etapa$params)),
      tratamento$codigo(etapa$params),
      ""
    )
  }
  c(linhas, "dados_analise <- dados", "")
}

exportacao_codigo_base_compartilhada <- function(pipeline, base_externa = NULL,
                                                 import_info = list(),
                                                 reg = tratamentos) {
  c(
    "# =======================================================================",
    "# Base Compartilhada do projeto",
    "# -----------------------------------------------------------------------",
    "# Este é o ÚNICO arquivo responsável por produzir `dados_analise`.",
    "# O relatório apenas o executa com source(); todas as bases derivadas",
    "# nascem do objeto que ele deixa na memória.",
    "#",
    "# Percurso: planilha bruta -> operações estruturais -> trilha de",
    "# tratamentos -> conferência contra a fotografia exportada.",
    "#",
    "# Gerado pela CatalyseR. Pode ser lido, executado e adaptado livremente.",
    "# =======================================================================",
    "",
    "# As funcoes usadas aqui vem do pacote catalyser. Digite ?catalyser_anova",
    "# no console para ver a ajuda de qualquer uma delas.",
    "if (!requireNamespace('catalyser', quietly = TRUE)) {",
    "  stop(",
    "    'Este projeto usa o pacote catalyser. Para instalar:\\n',",
    "    '  remotes::install_github(\"astuciasnor/catalyser\")',",
    "    call. = FALSE",
    "  )",
    "}",
    "library(catalyser)",
    "",
    exportacao_bloco_planilha(import_info),
    exportacao_bloco_estrutural(base_externa),
    exportacao_bloco_trilha(pipeline, reg),
    "# -----------------------------------------------------------------------",
    "# 4. Conferência",
    "# -----------------------------------------------------------------------",
    "# A CatalyseR também exportou uma fotografia de `dados_analise`. A função",
    "# abaixo compara a base reconstruída com ela e avisa se algo divergir.",
    "#",
    "# Para olhar a base tratada fora do R, abra 'dados/base_compartilhada.xlsx'.",
    "catalyser_conferir_base(",
    "  dados_analise,",
    "  file.path('dados', 'dados_analise.rds'),",
    "  rotulo = 'Base Compartilhada'",
    ")"
  )
}

# ---- Nomes dos arquivos de análise -------------------------------------------
# O aluno lê a pasta R/ antes de abrir qualquer coisa. Os nomes precisam dizer
# a ordem e o que cada arquivo faz, sem IDs internos:
#
#   01_base_compartilhada.R
#   02_execucao_01_anova_um_fator.R
#   02_execucao_02_grafico_de_linhas_01.R
#   02_execucao_03_grafico_de_linhas_02.R
#
# O sufixo numérico do tipo só aparece quando a mesma análise se repete.

exportacao_tipo_legivel <- function(tipo) {
  legiveis <- c(
    anova_um_fator = "anova_um_fator",
    anova_dois_fatores = "anova_dois_fatores",
    grafico_linhas = "grafico_de_linhas",
    regressao_linear = "regressao_linear",
    regressao_logistica = "regressao_logistica",
    teste_t_one_val = "teste_t_uma_amostra",
    teste_t_two_ind = "teste_t_duas_amostras",
    teste_t_paired = "teste_t_pareado",
    estatistica_descritiva = "estatistica_descritiva",
    qui_quadrado = "qui_quadrado",
    pca = "pca",
    hca = "agrupamentos"
  )
  chave <- as.character(tipo %||% "")
  if (chave %in% names(legiveis)) return(unname(legiveis[[chave]]))
  exportacao_nome_seguro(chave, "analise")
}

#' Nome de arquivo de uma execução, já com a numeração da sequência
exportacao_arquivo_execucao <- function(execucao, indice, sufixo_tipo = NULL) {
  tipo <- exportacao_tipo_legivel(execucao$tipo)
  if (!is.null(sufixo_tipo)) tipo <- sprintf("%s_%02d", tipo, as.integer(sufixo_tipo))
  sprintf("R/02_execucao_%02d_%s.R", as.integer(indice), tipo)
}

#' Resolve os nomes de todas as execuções de uma vez
#'
#' Precisa ser feito em conjunto: o sufixo de repetição de um arquivo depende
#' de quantas execuções do mesmo tipo existem no projeto.
exportacao_arquivos_execucao <- function(execucoes) {
  ids <- names(execucoes)
  if (is.null(ids) || !length(ids)) return(stats::setNames(character(), character()))
  tipos <- vapply(execucoes, function(x) as.character(x$tipo %||% ""), character(1),
                  USE.NAMES = FALSE)
  repete <- tipos %in% tipos[duplicated(tipos)]
  contador <- stats::setNames(integer(length(unique(tipos))), unique(tipos))
  caminhos <- character(length(ids))
  for (i in seq_along(ids)) {
    contador[[tipos[i]]] <- contador[[tipos[i]]] + 1L
    caminhos[i] <- exportacao_arquivo_execucao(
      execucoes[[i]], i,
      sufixo_tipo = if (repete[i]) contador[[tipos[i]]] else NULL
    )
  }
  stats::setNames(caminhos, ids)
}

# Script numerado de uma execução. Serve para estudar e rodar uma análise
# isolada, fora do Quarto. A receita da base derivada não é repetida aqui: ela
# vive no chunk correspondente do relatório, e este script usa a fotografia da
# base para chegar ao mesmo resultado sem duplicar código.
exportacao_codigo_execucao <- function(execucao, indice, registro_bases,
                                       raiz_chunk = NULL) {
  variavel <- exportacao_nome_seguro(paste0("resultado_", execucao$id), "resultado_execucao")
  base <- bases_obter(registro_bases, execucao$base_id)
  raiz_chunk <- raiz_chunk %||% exportacao_raiz_chunk(execucao)
  codigo_pedagogico <- if (execucao$tipo %in% c("anova_um_fator", "anova_dois_fatores", "grafico_linhas")) {
    exportacao_codigo_estudo(
      execucao,
      incluir_carregamento = FALSE,
      incluir_cabecalho = FALSE
    )
  } else {
    character()
  }
  carregar <- if (identical(execucao$base_tipo, "derivada") && !is.null(base)) {
    c(
      "# A Base Compartilhada é reconstruída pelo seu script dedicado.",
      "source(file.path('R', '01_base_compartilhada.R'), local = TRUE)",
      "",
      sprintf("# Base derivada: um salto a partir de dados_analise. Esta é a mesma"),
      sprintf("# receita do chunk '%s-base' do relatorio.qmd.", raiz_chunk),
      strsplit(bases_codigo(base, incluir_print = FALSE), "\n", fixed = TRUE)[[1]],
      "",
      sprintf("dados <- %s", base$nome_r)
    )
  } else if (identical(execucao$base_id, "dados_analise") || identical(execucao$base_tipo, "compartilhada")) {
    c(
      "# A Base Compartilhada é reconstruída pelo seu script dedicado.",
      "source(file.path('R', '01_base_compartilhada.R'), local = TRUE)",
      "dados <- dados_analise"
    )
  } else {
    c(
      "# Esta execução usa uma tabela preparada ou entrada manual guardada nos parâmetros.",
      "dados <- NULL"
    )
  }
  regua <- strrep("-", 75)
  c(
    paste0("# ", strrep("=", 75)),
    sprintf("# ANÁLISE %d — %s", as.integer(indice), execucao$titulo),
    paste0("# ", strrep("=", 75)),
    "#",
    sprintf("# Pergunta   : %s", exportacao_pergunta(execucao)),
    sprintf("# Base usada : %s", execucao$base_objeto),
    sprintf("# Método     : %s", exportacao_tipo_legivel(execucao$tipo)),
    "#",
    "# Este arquivo é independente: rode-o do início ao fim e você chega ao",
    "# mesmo resultado que viu na CatalyseR. Ele tem três partes:",
    "#",
    "#   PARTE 1 - preparar os dados desta análise",
    "#   PARTE 2 - a análise em si, o código que interessa aprender",
    "#   PARTE 3 - refazer o resultado do relatório (parte técnica)",
    paste0("# ", strrep("=", 75)),
    "",
    "# As funcoes desta analise vem do pacote catalyser. Para ver a ajuda de",
    "# qualquer uma delas, digite no console: ?catalyser_executar",
    "if (!requireNamespace('catalyser', quietly = TRUE)) {",
    "  stop(",
    "    'Este projeto usa o pacote catalyser. Para instalar:\\n',",
    "    '  remotes::install_github(\"astuciasnor/catalyser\")',",
    "    call. = FALSE",
    "  )",
    "}",
    "library(catalyser)",
    "",
    paste0("# ", regua),
    "# PARTE 1 - PREPARAR OS DADOS DESTA ANÁLISE",
    paste0("# ", regua),
    carregar,
    "",
    if (length(codigo_pedagogico)) c(
      paste0("# ", regua),
      "# PARTE 2 - A ANÁLISE",
      paste0("# ", regua),
      "# Esta é a parte que vale ler linha a linha: é o que você escreveria no",
      "# RStudio para fazer esta análise sem a CatalyseR.",
      "",
      codigo_pedagogico,
      ""
    ) else NULL,
    paste0("# ", regua),
    "# PARTE 3 - REFAZER O RESULTADO DO RELATÓRIO",
    paste0("# ", regua),
    "# Daqui para baixo é maquinaria, não matéria de estudo. A CatalyseR guardou",
    "# a configuração exata desta análise num arquivo de metadados; a função",
    "# abaixo a reexecuta e devolve as tabelas e figuras já formatadas, do jeito",
    "# que aparecem no relatório. O resultado é o mesmo da PARTE 2.",
    "",
    "registro_execucoes <- readRDS(file.path('metadados', 'registro_execucoes.rds'))",
    sprintf("configuracao <- registro_execucoes[[%s]]", exportacao_dput_texto(execucao$id)),
    "",
    sprintf("%s <- catalyser_executar(configuracao, dados)", variavel),
    "",
    "# Para ver um componente isolado, por exemplo:",
    sprintf("#   %s$grafico", variavel),
    sprintf("#   %s$tabela", variavel)
  )
}

#' Frase curta com a pergunta que a análise responde
#'
#' Vai no cabeçalho do script para o aluno saber, na primeira linha, o que está
#' prestes a ler.
exportacao_pergunta <- function(execucao) {
  p <- execucao$parametros %||% list()
  switch(
    as.character(execucao$tipo %||% ""),
    anova_dois_fatores = sprintf("'%s' varia conforme '%s' e '%s'?",
                                 p$resposta, p$fator_a, p$fator_b),
    anova_um_fator = sprintf("a média de '%s' difere entre os grupos de '%s'?",
                             p$resposta, p$fator),
    grafico_linhas = sprintf("como '%s' se comporta ao longo de '%s'?", p$y, p$x),
    regressao_linear = sprintf("'%s' varia em função de '%s'?", p$resposta, p$preditor),
    regressao_logistica = sprintf("o que prevê a ocorrência de '%s'?", p$resposta),
    teste_t_two_ind = sprintf("a média de '%s' difere entre os dois grupos de '%s'?",
                              p$resposta, p$grupo),
    teste_t_one_val = sprintf("a média de '%s' difere do valor de referência?", p$variavel),
    teste_t_paired = sprintf("houve mudança entre '%s' e '%s'?", p$variavel_1, p$variavel_2),
    estatistica_descritiva = "como se distribuem as variáveis escolhidas?",
    qui_quadrado = sprintf("'%s' e '%s' são independentes?", p$var_row, p$var_col),
    execucao$titulo %||% "ver o título acima"
  )
}

exportacao_yaml_texto <- function(x) {
  x <- as.character(x %||% "")
  paste0('"', gsub('"', '\\"', x, fixed = TRUE), '"')
}

exportacao_codigo_estudo <- function(execucao, incluir_carregamento = TRUE,
                                     incluir_cabecalho = TRUE) {
  p <- execucao$parametros %||% list()
  texto_r <- function(x) exportacao_dput_texto(as.character(x))
  numero_r <- function(x) exportacao_dput_texto(as.numeric(x))
  vetor_r <- function(x) exportacao_dput_texto(as.character(x %||% character()))
  # Este bloco só aparece quando o código de estudo é lido isolado. No relatório
  # e nos scripts numerados a base já está montada, e `incluir_carregamento` é
  # FALSE — nada de instrução duplicada.
  carregar <- if (identical(execucao$base_tipo, "derivada")) {
    c(
      "# Antes de rodar este bloco, construa a base desta análise:",
      "#   1. source(file.path('R', '01_base_compartilhada.R'))",
      sprintf("#   2. a receita de '%s' (chunk '%s-base' do relatorio.qmd)",
              execucao$base_objeto, exportacao_raiz_chunk(execucao)),
      sprintf("dados <- %s", execucao$base_objeto)
    )
  } else if (identical(execucao$base_tipo, "compartilhada") ||
             identical(execucao$base_id, "dados_analise")) {
    c(
      "source(file.path('R', '01_base_compartilhada.R'), local = TRUE)",
      "dados <- dados_analise"
    )
  } else {
    "# A tabela desta execução está preservada nos parâmetros registrados."
  }

  codigo <- switch(
    execucao$tipo,
    regressao_linear = c(
      sprintf(
        "formula_modelo <- stats::reformulate(%s, response = %s)",
        texto_r(p$preditor), texto_r(p$resposta)
      ),
      "modelo <- stats::lm(formula_modelo, data = dados)",
      "summary(modelo)"
    ),
    regressao_logistica = c(
      sprintf(
        "formula_modelo <- stats::reformulate(%s, response = %s)",
        texto_r(p$preditor), texto_r(p$resposta)
      ),
      "modelo <- stats::glm(formula_modelo, data = dados, family = stats::binomial())",
      "summary(modelo)"
    ),
    teste_t_one_val = c(
      sprintf(
        paste0(
          "resultado <- stats::t.test(dados[[%s]], mu = %s, ",
          "alternative = %s, conf.level = %s)"
        ),
        texto_r(p$variavel), numero_r(p$media_hipotetica),
        texto_r(p$alternativa), numero_r(p$nivel_confianca)
      ),
      "resultado"
    ),
    teste_t_two_ind = c(
      sprintf(
        "formula_teste <- stats::reformulate(%s, response = %s)",
        texto_r(p$grupo), texto_r(p$resposta)
      ),
      sprintf(
        paste0(
          "resultado <- stats::t.test(formula_teste, data = dados, ",
          "alternative = %s, conf.level = %s, var.equal = %s)"
        ),
        texto_r(p$alternativa), numero_r(p$nivel_confianca),
        if (isTRUE(p$variancias_iguais)) "TRUE" else "FALSE"
      ),
      "resultado"
    ),
    teste_t_paired = c(
      sprintf(
        paste0(
          "resultado <- stats::t.test(dados[[%s]], dados[[%s]], paired = TRUE, ",
          "alternative = %s, conf.level = %s)"
        ),
        texto_r(p$variavel_1), texto_r(p$variavel_2),
        texto_r(p$alternativa), numero_r(p$nivel_confianca)
      ),
      "resultado"
    ),
    anova_um_fator = c(
      "# 1. Declarar as variáveis e o nível de confiança.",
      sprintf("variavel_resposta <- %s", texto_r(p$resposta)),
      sprintf("variavel_fator <- %s", texto_r(p$fator)),
      sprintf("nivel_confianca <- %s", numero_r(p$nivel_confianca %||% 0.95)),
      "",
      "# 2. Manter casos completos e declarar o fator.",
      "dados_anova <- dados[",
      "  stats::complete.cases(dados[c(variavel_resposta, variavel_fator)]),",
      "  ,",
      "  drop = FALSE",
      "]",
      "dados_anova[[variavel_fator]] <- droplevels(as.factor(dados_anova[[variavel_fator]]))",
      "",
      "# 3. Resumir a resposta em cada grupo antes do teste.",
      "resumo_por_grupo <- dados_anova |>",
      "  dplyr::group_by(.data[[variavel_fator]]) |>",
      "  dplyr::summarise(",
      "    n = dplyr::n(),",
      "    media = mean(.data[[variavel_resposta]]),",
      "    desvio_padrao = stats::sd(.data[[variavel_resposta]]),",
      "    .groups = 'drop'",
      "  )",
      "",
      "# 4. Ajustar a ANOVA de um fator.",
      sprintf(
        "formula_anova <- stats::reformulate(%s, response = %s)",
        "variavel_fator", "variavel_resposta"
      ),
      "modelo_anova <- stats::aov(formula_anova, data = dados_anova)",
      "tabela_anova <- summary(modelo_anova)",
      "",
      "# 5. Comparar pares e verificar os pressupostos.",
      "comparacoes_tukey <- stats::TukeyHSD(modelo_anova, conf.level = nivel_confianca)",
      "teste_levene <- car::leveneTest(",
      "  dados_anova[[variavel_resposta]],",
      "  dados_anova[[variavel_fator]],",
      "  center = stats::median",
      ")",
      "teste_shapiro <- stats::shapiro.test(stats::residuals(modelo_anova))",
      "",
      "# 6. Calcular tamanhos de efeito.",
      "eta_quadrado <- effectsize::eta_squared(modelo_anova)",
      "omega_quadrado <- effectsize::omega_squared(modelo_anova)",
      "",
      "# 7. Letras de diferenca: grupos com a mesma letra nao diferiram.",
      "#    Ajuda completa: ?catalyser_letras_tukey",
      "combinacoes <- utils::combn(levels(dados_anova[[variavel_fator]]), 2)",
      "letras_diferenca <- catalyser_letras_tukey(",
      "  pares = combinacoes[c(2, 1), , drop = FALSE],",
      "  p_ajustado = comparacoes_tukey[[1]][, 'p adj'],",
      "  medias = tapply(",
      "    dados_anova[[variavel_resposta]],",
      "    dados_anova[[variavel_fator]],",
      "    mean",
      "  )",
      ")",
      "",
      "# 8. Examinar os objetos principais.",
      "resumo_por_grupo",
      "tabela_anova",
      "comparacoes_tukey",
      "letras_diferenca",
      "teste_levene",
      "teste_shapiro",
      "eta_quadrado",
      "omega_quadrado"
    ),
    anova_dois_fatores = c(
      "# 1. Declarar a resposta, os dois fatores e o nível de confiança.",
      sprintf("variavel_resposta <- %s", texto_r(p$resposta)),
      sprintf("variavel_fator_a <- %s", texto_r(p$fator_a)),
      sprintf("variavel_fator_b <- %s", texto_r(p$fator_b)),
      sprintf("nivel_confianca <- %s", numero_r(p$nivel_confianca %||% 0.95)),
      "",
      "# 2. Manter casos completos e transformar os fatores em categorias.",
      "colunas_anova2 <- c(variavel_resposta, variavel_fator_a, variavel_fator_b)",
      "dados_anova2 <- dados[stats::complete.cases(dados[colunas_anova2]), , drop = FALSE]",
      "dados_anova2[[variavel_fator_a]] <- droplevels(as.factor(dados_anova2[[variavel_fator_a]]))",
      "dados_anova2[[variavel_fator_b]] <- droplevels(as.factor(dados_anova2[[variavel_fator_b]]))",
      "",
      "# 3. Ajustar a ANOVA fatorial com interação.",
      "dados_anova2$.anova2_resposta <- dados_anova2[[variavel_resposta]]",
      "dados_anova2$.anova2_fator_a <- dados_anova2[[variavel_fator_a]]",
      "dados_anova2$.anova2_fator_b <- dados_anova2[[variavel_fator_b]]",
      "modelo_anova2 <- stats::aov(.anova2_resposta ~ .anova2_fator_a * .anova2_fator_b, data = dados_anova2)",
      "tabela_anova2 <- summary(modelo_anova2)",
      "",
      "# 4. Resumir médias por célula e comparar células.",
      "medias_celulas <- aggregate(dados_anova2[[variavel_resposta]], dados_anova2[c(variavel_fator_a, variavel_fator_b)], function(x) c(n = length(x), media = mean(x), dp = stats::sd(x)))",
      "comparacoes_tukey <- stats::TukeyHSD(modelo_anova2, which = '.anova2_fator_a:.anova2_fator_b', conf.level = nivel_confianca)",
      "",
      "# 5. Verificar pressupostos antes de interpretar os efeitos.",
      "teste_shapiro <- stats::shapiro.test(stats::residuals(modelo_anova2))",
      "teste_levene <- if (requireNamespace('car', quietly = TRUE)) car::leveneTest(dados_anova2[[variavel_resposta]], interaction(dados_anova2[[variavel_fator_a]], dados_anova2[[variavel_fator_b]]), center = stats::median) else NULL",
      "",
      "tabela_anova2",
      "medias_celulas",
      "comparacoes_tukey",
      "teste_shapiro",
      "teste_levene"
    ),
    grafico_linhas = c(
      "# 1. Declarar as variáveis e os textos do gráfico.",
      sprintf("variavel_x <- %s", texto_r(p$x)),
      sprintf("variavel_y <- %s", texto_r(p$y)),
      sprintf("variavel_grupo <- %s", texto_r(p$grupo %||% "none")),
      sprintf("titulo_grafico <- %s",
              texto_r(p$titulo_grafico %||% execucao$titulo %||%
                        sprintf("%s ao longo de %s", p$y, p$x))),
      sprintf("rotulo_x <- %s", texto_r(p$rotulo_x %||% p$x)),
      sprintf("rotulo_y <- %s", texto_r(p$rotulo_y %||% p$y)),
      "",
      "# 2. Manter só as observações com os dois eixos preenchidos.",
      "#    O ggplot2 descartaria as incompletas com um aviso discreto; aqui a",
      "#    exclusão fica explícita e contada.",
      if (identical(p$grupo %||% "none", "none")) {
        "colunas_grafico <- c(variavel_x, variavel_y)"
      } else {
        "colunas_grafico <- c(variavel_x, variavel_y, variavel_grupo)"
      },
      "dados_grafico <- dados[",
      "  stats::complete.cases(dados[colunas_grafico]),",
      "  ,",
      "  drop = FALSE",
      "]",
      "cat(",
      "  nrow(dados_grafico), 'observações plotadas;',",
      "  nrow(dados) - nrow(dados_grafico), 'descartadas por dados faltantes.\\n'",
      ")",
      "",
      "# 3. Construir o mapeamento estético.",
      if (identical(p$grupo %||% "none", "none")) {
        "mapeamento <- ggplot2::aes(x = .data[[variavel_x]], y = .data[[variavel_y]], group = 1)"
      } else {
        c(
          "dados_grafico[[variavel_grupo]] <- as.factor(dados_grafico[[variavel_grupo]])",
          "mapeamento <- ggplot2::aes(",
          "  x = .data[[variavel_x]], y = .data[[variavel_y]],",
          "  color = .data[[variavel_grupo]], group = .data[[variavel_grupo]]",
          ")"
        )
      },
      "",
      "# 4. Montar o gráfico em camadas.",
      "grafico_linhas <- ggplot2::ggplot(dados_grafico, mapeamento) +",
      if (identical(p$grupo %||% "none", "none")) {
        sprintf("  ggplot2::geom_line(linewidth = %s, color = '#0F3B5F') +",
                numero_r(p$espessura_linha %||% 1))
      } else {
        sprintf("  ggplot2::geom_line(linewidth = %s) +",
                numero_r(p$espessura_linha %||% 1))
      },
      if (isTRUE(p$mostrar_pontos)) {
        if (identical(p$grupo %||% "none", "none")) {
          "  ggplot2::geom_point(size = 2.4, color = '#2E7D8F') +"
        } else {
          "  ggplot2::geom_point(size = 2.4) +"
        }
      } else NULL,
      if (!identical(p$grupo %||% "none", "none"))
        "  ggplot2::scale_color_manual(values = c('#0F3B5F', '#2E7D8F', '#62B6B7', '#E89B3C', '#E76F51')) +" else NULL,
      sprintf(
        "  %s +",
        switch(
          as.character(p$tema %||% "minimal"),
          classic = "ggplot2::theme_classic(base_size = 14)",
          bw = "ggplot2::theme_bw(base_size = 14)",
          gray = "ggplot2::theme_gray(base_size = 14)",
          light = "ggplot2::theme_light(base_size = 14)",
          "ggplot2::theme_minimal(base_size = 14)"
        )
      ),
      "  ggplot2::theme(",
      "    plot.title = ggplot2::element_text(face = 'bold', size = 16, color = '#0F3B5F'),",
      sprintf("    legend.position = %s", texto_r(p$posicao_legenda %||% "right")),
      "  ) +",
      "  ggplot2::labs(",
      "    title = titulo_grafico, x = rotulo_x, y = rotulo_y,",
      if (identical(p$grupo %||% "none", "none"))
        "    color = NULL" else "    color = variavel_grupo",
      "  )",
      "",
      "# 5. Exibir o gráfico.",
      "grafico_linhas"
    ),
    estatistica_descritiva = c(
      sprintf("variaveis <- %s", vetor_r(p$variaveis)),
      "summary(dados[variaveis])"
    ),
    qui_quadrado = c(
      if (identical(p$fonte, "tidy"))
        sprintf(
          "tabela <- stats::xtabs(n ~ dados[[%s]] + dados[[%s]], data = dados)",
          texto_r(p$var_row), texto_r(p$var_col)
        ) else
        sprintf(
          "tabela <- table(dados[[%s]], dados[[%s]])",
          texto_r(p$var_row), texto_r(p$var_col)
        ),
      sprintf(
        "resultado <- stats::chisq.test(tabela, correct = %s)",
        if (isTRUE(p$yates)) "TRUE" else "FALSE"
      ),
      "resultado"
    ),
    pca = c(
      sprintf("variaveis <- %s", vetor_r(p$variaveis)),
      sprintf(
        "modelo_pca <- stats::prcomp(dados[variaveis], center = TRUE, scale. = %s)",
        if (isTRUE(p$padronizar)) "TRUE" else "FALSE"
      ),
      "summary(modelo_pca)"
    ),
    hca = c(
      sprintf("variaveis <- %s", vetor_r(p$variaveis)),
      "matriz <- scale(dados[variaveis])",
      sprintf("distancias <- stats::dist(matriz, method = %s)", texto_r(p$distancia)),
      sprintf("grupos <- stats::hclust(distancias, method = %s)", texto_r(p$ligacao)),
      sprintf("stats::cutree(grupos, k = %s)", numero_r(p$numero_grupos))
    ),
    NULL
  )

  if (is.null(codigo) || !length(codigo)) {
    codigo_registrado <- trimws(as.character(execucao$codigo_r %||% ""))
    codigo <- if (nzchar(codigo_registrado)) {
      strsplit(codigo_registrado, "\n", fixed = TRUE)[[1]]
    } else {
      c(
        "# Consulte o script numerado desta execução para o replay integral.",
        "resultado <- catalyser_executar(execucao, dados)"
      )
    }
  }
  c(
    if (isTRUE(incluir_cabecalho)) c(
      "# Código R essencial desta execução.",
      "# Este chunk pode ser executado manualmente no RStudio."
    ) else NULL,
    if (isTRUE(incluir_carregamento)) c(carregar, "") else NULL,
    Filter(Negate(is.null), codigo)
  )
}

# ---- Labels dos chunks do QMD ----------------------------------------------
# Um chunk chamado `execucao_0001_narrativa` não ajuda ninguém a se localizar no
# arquivo. Os labels passam a dizer a intenção científica: `anova-modelo`,
# `anova-tukey`, `linhas-comprimento-cm-grafico`.

exportacao_slug_chunk <- function(x, padrao = "analise") {
  gsub("_", "-", exportacao_nome_seguro(x, padrao), fixed = TRUE)
}

# Sufixo por componente. `tabela` vira `modelo` na ANOVA porque é a tabela do
# modelo ajustado, e `comparacoes` vira `tukey` pelo nome do método.
exportacao_sufixo_componente <- function(tipo, componente) {
  especificos <- if (identical(tipo, "anova_um_fator")) {
    c(tabela = "modelo", comparacoes = "tukey", descritivos = "resumo-grupos")
  } else if (identical(tipo, "anova_dois_fatores")) {
    c(tabela = "modelo", celulas = "celulas", efeito = "efeitos", comparacoes = "tukey")
  } else {
    c(comparacoes = "comparacoes", descritivos = "resumo-grupos")
  }
  if (componente %in% names(especificos)) return(unname(especificos[[componente]]))
  exportacao_slug_chunk(componente, "resultado")
}

# Raiz do label de uma execução: tipo abreviado + a variável que a distingue.
# Duas execuções do gráfico de linhas com Y diferente recebem raízes diferentes.
exportacao_raiz_chunk <- function(execucao) {
  p <- execucao$parametros %||% list()
  partes <- switch(
    as.character(execucao$tipo %||% ""),
    anova_um_fator = c("anova", p$resposta),
    anova_dois_fatores = c("anova2", p$resposta),
    grafico_linhas = c("linhas", p$y),
    regressao_linear = c("regressao", p$resposta),
    regressao_logistica = c("regressao-logistica", p$resposta),
    teste_t_one_val = c("teste-t", p$variavel),
    teste_t_two_ind = c("teste-t", p$resposta),
    teste_t_paired = c("teste-t-pareado", p$variavel_1),
    estatistica_descritiva = c("descritiva", head(p$variaveis, 1)),
    qui_quadrado = c("qui-quadrado", p$var_row),
    c(exportacao_slug_chunk(execucao$tipo, "analise"))
  )
  partes <- Filter(function(x) length(x) && nzchar(as.character(x)[[1]]), partes)
  exportacao_slug_chunk(paste(unlist(partes), collapse = "-"), "analise")
}

# O Quarto falha com labels repetidos. Esta função resolve as raízes de todas as
# execuções de uma vez e desempata com o ID quando duas coincidem.
exportacao_raizes_chunk <- function(execucoes) {
  ids <- names(execucoes)
  if (is.null(ids) || !length(ids)) return(stats::setNames(character(), character()))
  raizes <- vapply(execucoes, exportacao_raiz_chunk, character(1), USE.NAMES = FALSE)
  duplicadas <- raizes %in% raizes[duplicated(raizes)]
  # exportacao_slug_chunk() trata um nome por chamada; percorrer é mais seguro
  # do que assumir vetorização.
  if (any(duplicadas)) {
    sufixos <- vapply(
      ids[duplicadas], exportacao_slug_chunk, character(1),
      padrao = "execucao", USE.NAMES = FALSE
    )
    raizes[duplicadas] <- paste0(raizes[duplicadas], "-", sufixos)
  }
  stats::setNames(raizes, ids)
}

exportacao_bloco_componente <- function(variavel, execucao_id, componente,
                                        raiz_chunk = NULL, tipo = NULL) {
  rotulo <- comunicacao_rotulos_saidas[[componente]] %||% componente
  id_chunk <- if (is.null(raiz_chunk)) {
    exportacao_slug_chunk(paste(execucao_id, componente), "resultado")
  } else {
    paste0(raiz_chunk, "-", exportacao_sufixo_componente(tipo, componente))
  }
  expressao <- if (identical(componente, "console")) {
    sprintf(
      "cat('```text\\n', paste(%s[['console']], collapse = '\\n'), '\\n```\\n')",
      variavel
    )
  } else {
    sprintf("catalyser_mostrar(%s[['%s']])", variavel, componente)
  }
  c(
    sprintf("### %s", rotulo),
    "",
    "```{r}",
    sprintf("#| label: %s", id_chunk),
    "#| echo: false",
    "#| results: asis",
    expressao,
    "```",
    ""
  )
}

# Chunk que constrói a base de uma análise.
#
# A Base Compartilhada NÃO é construída aqui: ela vem do script dedicado. Este
# chunk cuida apenas do salto de um passo até a base derivada, com a receita
# visível no fonte do QMD e silenciosa no Word.
exportacao_bloco_base_analise <- function(item, raiz, registro_bases) {
  base <- bases_obter(registro_bases, item$base_id)
  if (identical(item$base_tipo, "derivada") && !is.null(base)) {
    receita <- strsplit(
      bases_codigo(base, incluir_print = FALSE), "\n", fixed = TRUE
    )[[1]]
    return(c(
      "<!-- Base desta análise: um salto a partir de dados_analise. A receita",
      "     roda para alimentar o resultado, mas não aparece no Word. -->",
      "```{r}",
      sprintf("#| label: %s-base", raiz),
      "#| echo: false",
      receita,
      sprintf("dados_da_analise <- %s", base$nome_r),
      "```",
      ""
    ))
  }
  c(
    "<!-- Esta análise usa a própria Base Compartilhada. -->",
    "```{r}",
    sprintf("#| label: %s-base", raiz),
    "#| echo: false",
    "dados_da_analise <- dados_analise",
    "```",
    ""
  )
}

exportacao_gerar_qmd <- function(manifesto, titulo_projeto = "Relatório de análise",
                                 registro_bases = list()) {
  globais <- manifesto$secoes_globais %||% list()
  linhas <- c(
    "---",
    paste0("title: ", exportacao_yaml_texto(titulo_projeto)),
    "lang: pt-BR",
    "format:",
    "  docx:",
    "    reference-doc: custom-reference.docx",
    "    toc: true",
    "    number-sections: true",
    "execute:",
    "  warning: false",
    "  message: false",
    "---",
    "",
    "<!--",
    "  Este primeiro bloco só prepara o terreno; ele não faz nenhuma análise.",
    "  São três linhas, e cada uma carrega uma coisa:",
    "",
    "    1. o pacote catalyser, que traz as funções de análise;",
    "    2. a configuração congelada de cada análise, guardada quando você",
    "       clicou em 'Adicionar aos resultados' na CatalyseR;",
    "    3. a lista das bases usadas, para a tabela da seção seguinte.",
    "",
    "  Ele roda escondido (include: false) porque é preparação, não resultado.",
    "-->",
    "```{r}",
    "#| label: configuracao",
    "#| include: false",
    "# 1. Pacote com as funções de análise (?catalyser_anova mostra a ajuda).",
    "library(catalyser)",
    "",
    "# 2. Configuração de cada análise, do jeito que foi registrada na CatalyseR.",
    "analises_registradas <- readRDS(file.path('metadados', 'registro_execucoes.rds'))",
    "",
    "# 3. Lista das bases do projeto, para a tabela de Preparação dos dados.",
    "bases_projeto <- utils::read.csv(",
    "  file.path('metadados', 'bases.csv'),",
    "  check.names = FALSE",
    ")",
    "```",
    "",
    "# Preparação dos dados",
    "",
    "A Base Compartilhada é construída por um script dedicado, `R/01_base_compartilhada.R`, que parte da planilha bruta exportada com o projeto, aplica as operações estruturais e a trilha de tratamentos e confere o resultado contra a fotografia registrada na CatalyseR. Este relatório apenas o executa: nenhuma transformação compartilhada acontece dentro do `.qmd`.",
    "",
    "```{r}",
    "#| label: base-compartilhada",
    "#| include: false",
    "# Único ponto de entrada dos dados: o script deixa `dados_analise` na memória.",
    "source(file.path('R', '01_base_compartilhada.R'), local = TRUE)",
    "```",
    "",
    "As bases derivadas nascem diretamente de `dados_analise`, em um único salto, e cada análise constrói a sua no chunk que a antecede.",
    "",
    "```{r}",
    "#| label: bases-projeto",
    "#| echo: false",
    "catalyser_tabela_ocean(bases_projeto)",
    "```",
    ""
  )
  if (nzchar(trimws(globais$introducao %||% ""))) {
    linhas <- c(linhas, "# Introdução", "", globais$introducao, "")
  }
  linhas <- c(linhas, "# Métodos", "")
  if (nzchar(trimws(globais$metodos %||% ""))) linhas <- c(linhas, globais$metodos, "")
  linhas <- c(
    linhas,
    "As configurações analíticas foram registradas explicitamente na CatalyseR. Cada script informa a base e os parâmetros usados.",
    "",
    "# Resultados",
    ""
  )

  incluidas <- Filter(function(x) isTRUE(x$incluir_word), manifesto$execucoes %||% list())
  if (!length(incluidas)) {
    linhas <- c(linhas, "*Nenhuma execução foi selecionada para o relatório Word.*", "")
  }
  raizes <- exportacao_raizes_chunk(manifesto$execucoes %||% list())
  for (item in incluidas) {
    variavel <- exportacao_nome_seguro(paste0("resultado_", item$id), "resultado_execucao")
    raiz <- if (item$id %in% names(raizes)) unname(raizes[[item$id]]) else
      exportacao_slug_chunk(item$id, "analise")
    codigo_estudo <- exportacao_codigo_estudo(item, incluir_carregamento = FALSE)
    linhas <- c(
      linhas,
      paste0("## ", item$titulo),
      "",
      sprintf("**Base utilizada:** `%s`  ", item$base_objeto),
      sprintf("**Execução registrada:** `%s`", item$id),
      "",
      exportacao_bloco_base_analise(item, raiz, registro_bases),
      "<!-- Método: o código abaixo fica explícito no QMD para estudo e pode ser",
      "     copiado para o console. Não entra no Word. -->",
      "```{r}",
      sprintf("#| label: %s-codigo", raiz),
      "#| eval: false",
      "#| include: false",
      "dados <- dados_da_analise",
      "",
      codigo_estudo,
      "```",
      "",
      "<!-- Mecanismo editorial: reexecuta a configuração congelada sobre a base",
      "     construída acima, para alimentar as saídas escolhidas no manifesto. -->",
      "```{r}",
      sprintf("#| label: %s-replay", raiz),
      "#| include: false",
      sprintf("%s <- catalyser_executar(analises_registradas[[%s]], dados_da_analise)",
              variavel, exportacao_dput_texto(item$id)),
      "```",
      ""
    )
    for (componente in item$saidas_word) {
      linhas <- c(
        linhas,
        exportacao_bloco_componente(variavel, item$id, componente,
                                    raiz_chunk = raiz, tipo = item$tipo)
      )
    }
  }
  if (nzchar(trimws(globais$discussao %||% ""))) {
    linhas <- c(linhas, "# Discussão", "", globais$discussao, "")
  }
  if (nzchar(trimws(globais$conclusao %||% ""))) {
    linhas <- c(linhas, "# Conclusão", "", globais$conclusao, "")
  }
  linhas
}

exportacao_tabela_bases <- function(registro_bases, cache_bases, registro_execucoes,
                                    revisao_origem) {
  usos <- function(id) sum(vapply(
    registro_execucoes,
    function(execucao) identical(execucao$base_id, id), logical(1)
  ))
  linhas <- list(data.frame(
    Base = "Base compartilhada", Objeto_R = "dados_analise",
    Tipo = "compartilhada", Estado = "Atualizada", Execucoes = usos("dados_analise"),
    check.names = FALSE
  ))
  for (base in registro_bases) {
    estado <- if (identical(base$estado, "pronta")) {
      bases_estado_cache(base, bases_cache_obter(cache_bases, base$id), revisao_origem)
    } else {
      "Em preparo"
    }
    linhas[[length(linhas) + 1L]] <- data.frame(
      Base = base$nome_amigavel, Objeto_R = base$nome_r,
      Tipo = "derivada", Estado = estado, Execucoes = usos(base$id),
      check.names = FALSE
    )
  }
  do.call(rbind, linhas)
}

exportacao_manifesto_markdown <- function(manifesto) {
  linhas <- c(
    "# Manifesto editorial",
    "",
    sprintf("- Execuções preservadas no Projeto R: %d", manifesto$total_execucoes),
    sprintf("- Execuções incluídas no Word: %d", manifesto$total_word),
    ""
  )
  for (item in manifesto$execucoes) {
    linhas <- c(
      linhas,
      sprintf("## %s — %s", item$id, item$titulo),
      "",
      sprintf("- Base: `%s`", item$base_objeto),
      sprintf("- Incluída no Word: %s", if (item$incluir_word) "sim" else "não"),
      sprintf("- Conteúdo do Word: %s", if (length(item$saidas_word)) paste(item$saidas_word, collapse = ", ") else "nenhum"),
      sprintf("- Dependência: %s", item$estado_dependencia),
      ""
    )
  }
  linhas
}

exportacao_salvar_dataframe <- function(df, caminho_rds, caminho_csv = NULL) {
  saveRDS(as.data.frame(df), caminho_rds)
  if (!is.null(caminho_csv)) {
    utils::write.csv(as.data.frame(df), caminho_csv, row.names = FALSE, fileEncoding = "UTF-8")
  }
}

#' Grava a planilha de entrada do projeto exportado
#'
#' Falhar aqui não pode derrubar a exportação: o script da Base Compartilhada
#' recorre ao `.rds` quando a planilha não existe.
#'
#' @return `TRUE` se a planilha foi gravada (invisível).
exportacao_salvar_planilha <- function(df, caminho, aba = "dados") {
  if (!requireNamespace("writexl", quietly = TRUE)) return(invisible(FALSE))
  conteudo <- list(as.data.frame(df))
  names(conteudo) <- aba
  gravou <- tryCatch({
    writexl::write_xlsx(conteudo, path = caminho)
    TRUE
  }, error = function(e) FALSE)
  invisible(isTRUE(gravou))
}

#' README do projeto exportado
#'
#' Explica o percurso dos dados em uma leitura: onde a base compartilhada nasce,
#' onde cada base derivada é construída e o que o relatório faz.
exportacao_leiame_projeto <- function(nome_projeto, import_info = list()) {
  c(
    paste0("# ", nome_projeto), "",
    "Projeto de análise gerado pela CatalyseR.", "",
    "## Os arquivos de `dados/`", "",
    sprintf("- `%s` — a planilha bruta, **ponto de entrada** de tudo.",
            exportacao_nome_planilha(import_info)),
    "- `dados_analise.rds` — fotografia da Base Compartilhada, usada **só para",
    "  conferência**: o projeto compara o que reconstruiu com o que você viu.",
    "- `base_compartilhada.xlsx` — a base já tratada, para abrir no Excel ou",
    "  enviar a quem não usa R. É entrega, não fonte do relatório.",
    "- `base_resolvida.rds` — aparece apenas se houve mudança estrutural",
    "  promovida (Pivotar/Separar ou Criar e Editar Variáveis e Níveis).", "",
    "Não há cópia das bases derivadas: elas são um salto reproduzível a partir",
    "de `dados_analise`, e a receita aparece no código.", "",
    "## O caminho dos dados", "",
    "1. A planilha bruta.",
    "2. `R/01_base_compartilhada.R` — **o único** arquivo que constrói a Base",
    "   Compartilhada (`dados_analise`). Lê a planilha, aplica as operações",
    "   estruturais e a trilha de tratamentos e confere o resultado contra a",
    "   fotografia.",
    "3. `relatorio.qmd` — chama aquele script uma única vez e, a partir dali,",
    "   cada análise constrói a sua base derivada no chunk `...-base`.",
    "4. `R/02_execucao_*.R` — um script por execução, para estudar ou rodar uma análise",
    "   isolada fora do Quarto. Ele reconstrói o que precisa: a Base",
    "   Compartilhada e, quando for o caso, a receita da base derivada.", "",
    "As bases derivadas nascem diretamente de `dados_analise`, em um único salto:",
    "não existem ramos de ramos.", "",
    "## Como usar", "",
    "1. Abra `projeto_analise.Rproj` no RStudio.",
    "2. Rode `R/01_base_compartilhada.R` e leia a mensagem de conferência.",
    "3. Abra `relatorio.qmd`: cada análise traz um chunk `...-codigo` com o",
    "   código R essencial, pronto para copiar para o console.",
    "4. Os chunks de código usam `eval: false` e `include: false`; os de base",
    "   derivada rodam com `echo: false`. O método fica visível no fonte e o",
    "   Word, limpo.",
    "5. Clique em **Render** para gerar o Word.",
    "6. O projeto preserva todas as execuções registradas; o relatório mostra",
    "   somente as escolhas do manifesto editorial.", "",
    "## A pasta `metadados/`", "",
    "Essa pasta guarda a memória técnica da exportação: origem dos dados, receitas",
    "das bases, execuções registradas, escolhas do relatório e informações da sessão R.",
    "Ela permite auditar ou reconstruir o projeto, mas **não precisa ser aberta nem",
    "editada para executar as análises**. Para entender o que entrou no Word, comece",
    "por `metadados/MANIFESTO.md`; os arquivos `.rds` são lidos pela CatalyseR.", "",
    "## Funções de apoio", "",
    "As funções vêm do pacote `catalyser`, com ajuda em português. Digite",
    "`?catalyser_anova` no console para ver qualquer uma delas. As principais:",
    "", "- `catalyser_executar()` — reproduz uma execução registrada;",
    "- `catalyser_conferir_base()` — compara a base reconstruída com a fotografia;",
    "- `catalyser_completos()` — remove e conta casos incompletos;",
    "- `catalyser_mostrar()` e `catalyser_tabela_ocean()` — camada de apresentação."
  )
}

exportacao_criar_projeto <- function(destino, nome_projeto, dados_brutos,
                                     base_resolvida, dados_analise, pipeline,
                                     base_externa, registro_bases, cache_bases,
                                     registro_execucoes, manifesto, revisao_origem,
                                     import_info = list(), templates_dir = "templates") {
  validacao <- exportacao_validar_manifesto(manifesto, exigir_word = FALSE)
  if (!validacao$ok) stop(paste(validacao$mensagens, collapse = " "), call. = FALSE)

  nome_projeto <- paste0("projeto_", exportacao_nome_seguro(nome_projeto, "analise"))
  projeto <- file.path(destino, nome_projeto)
  if (dir.exists(projeto)) {
    stop("O diretório temporário do projeto já existe; gere a exportação novamente.", call. = FALSE)
  }
  dir.create(projeto, recursive = TRUE, showWarnings = FALSE)
  dirs <- file.path(projeto, c("dados", "R", "metadados", "resultados"))
  vapply(dirs, dir.create, logical(1), recursive = TRUE, showWarnings = FALSE)

  # A pasta `dados/` é deliberadamente enxuta. No caso comum ela tem três
  # arquivos, cada um com um papel distinto:
  #
  #   1. a planilha bruta   -> ponto de entrada do script da Base Compartilhada;
  #   2. dados_analise.rds  -> fotografia, usada apenas para conferência;
  #   3. base_compartilhada.xlsx -> entrega, para uso fora do R.
  #
  # Nada de cópias redundantes: o que o projeto sabe reconstruir, ele reconstrói.
  exportacao_salvar_planilha(
    dados_brutos,
    file.path(projeto, "dados", exportacao_nome_planilha(import_info)),
    aba = exportacao_aba_planilha(import_info)
  )
  exportacao_salvar_dataframe(dados_analise, file.path(projeto, "dados", "dados_analise.rds"))
  exportacao_salvar_planilha(
    dados_analise,
    file.path(projeto, "dados", "base_compartilhada.xlsx"),
    aba = "dados_analise"
  )
  # A fotografia pós-estrutural só é necessária quando houve Pivotar/Organizar
  # promovido: é o único trecho que o script não consegue reconstruir sozinho.
  if (nzchar(trimws(as.character(base_externa$codigo %||% "")))) {
    exportacao_salvar_dataframe(base_resolvida, file.path(projeto, "dados", "base_resolvida.rds"))
  }

  # As funções de análise não viajam mais como arquivo: vêm do pacote catalyser,
  # documentadas e com ajuda em português. O projeto ficou 42 KB mais leve e o
  # aluno ganhou `?catalyser_anova`.
  template_word <- file.path(templates_dir, "custom-reference.docx")
  if (!file.exists(template_word)) {
    stop("O template Word do exportador não foi encontrado.", call. = FALSE)
  }
  file.copy(template_word, file.path(projeto, "custom-reference.docx"), overwrite = TRUE)

  # Um único script constrói a Base Compartilhada. O relatório e os scripts de
  # execução apenas o chamam.
  writeLines(
    exportacao_codigo_base_compartilhada(pipeline, base_externa, import_info),
    file.path(projeto, "R", "01_base_compartilhada.R"), useBytes = TRUE
  )

  # As bases derivadas não têm script próprio nem fotografia em disco: são um
  # salto reproduzível a partir de `dados_analise`. A receita vem sempre do mesmo
  # gerador, `bases_codigo()`, tanto no chunk do relatório quanto no script
  # numerado da execução.

  ordem <- names(manifesto$execucoes)
  # Raízes de chunk e nomes de arquivo são resolvidos de uma vez: os dois
  # dependem do conjunto (desempate de repetidos) e precisam bater entre si.
  raizes_chunk <- exportacao_raizes_chunk(manifesto$execucoes %||% list())
  arquivos_execucao <- exportacao_arquivos_execucao(manifesto$execucoes %||% list())
  for (i in seq_along(ordem)) {
    id <- ordem[[i]]
    execucao <- registro_execucoes[[id]]
    arquivo <- if (id %in% names(arquivos_execucao)) {
      unname(arquivos_execucao[[id]])
    } else {
      exportacao_arquivo_execucao(execucao, i)
    }
    writeLines(
      exportacao_codigo_execucao(
        execucao, i, registro_bases,
        raiz_chunk = if (id %in% names(raizes_chunk)) unname(raizes_chunk[[id]]) else NULL
      ),
      file.path(projeto, arquivo), useBytes = TRUE
    )
  }

  tabela_bases <- exportacao_tabela_bases(
    registro_bases, cache_bases, registro_execucoes, revisao_origem
  )
  utils::write.csv(
    tabela_bases, file.path(projeto, "metadados", "bases.csv"),
    row.names = FALSE, fileEncoding = "UTF-8"
  )
  saveRDS(registro_execucoes, file.path(projeto, "metadados", "registro_execucoes.rds"))
  saveRDS(registro_bases, file.path(projeto, "metadados", "registro_bases.rds"))
  saveRDS(manifesto, file.path(projeto, "metadados", "manifesto_editorial.rds"))
  saveRDS(import_info, file.path(projeto, "metadados", "origem_dados.rds"))
  writeLines(
    exportacao_manifesto_markdown(manifesto),
    file.path(projeto, "metadados", "MANIFESTO.md"), useBytes = TRUE
  )

  titulo <- paste("Relatório de análise —", nome_projeto)
  writeLines(
    exportacao_gerar_qmd(manifesto, titulo, registro_bases = registro_bases),
    file.path(projeto, "relatorio.qmd"), useBytes = TRUE
  )
  writeLines(
    c(
      "Version: 1.0", "RestoreWorkspace: No", "SaveWorkspace: No",
      "AlwaysSaveHistory: No", "Encoding: UTF-8"
    ),
    file.path(projeto, "projeto_analise.Rproj"), useBytes = TRUE
  )
  writeLines(
    exportacao_leiame_projeto(nome_projeto, import_info),
    file.path(projeto, "README.md"), useBytes = TRUE
  )
  writeLines(capture.output(utils::sessionInfo()), file.path(projeto, "metadados", "sessionInfo.txt"))
  file.create(file.path(projeto, "resultados", ".gitkeep"))
  projeto
}

exportacao_empacotar_projeto <- function(file, ...) {
  raiz <- tempfile("catalyser_projeto_")
  dir.create(raiz, recursive = TRUE)
  on.exit(unlink(raiz, recursive = TRUE, force = TRUE), add = TRUE)
  projeto <- exportacao_criar_projeto(destino = raiz, ...)
  zip::zipr(
    zipfile = file, files = basename(projeto), root = dirname(projeto),
    include_directories = TRUE
  )
  invisible(file)
}

exportacao_renderizar_word <- function(file, ...) {
  raiz <- tempfile("catalyser_word_")
  dir.create(raiz, recursive = TRUE)
  on.exit(unlink(raiz, recursive = TRUE, force = TRUE), add = TRUE)
  args <- list(...)
  validacao <- exportacao_validar_manifesto(args$manifesto, exigir_word = TRUE)
  if (!validacao$ok) stop(paste(validacao$mensagens, collapse = " "), call. = FALSE)
  projeto <- do.call(exportacao_criar_projeto, c(list(destino = raiz), args))
  quarto <- unname(Sys.which("quarto"))
  if (!nzchar(quarto)) stop("O Quarto CLI não foi encontrado. Instale-o para gerar o Word.", call. = FALSE)
  anterior <- getwd()
  on.exit(setwd(anterior), add = TRUE)
  setwd(projeto)
  saida <- system2(
    quarto,
    c("render", "relatorio.qmd", "--to", "docx", "--output", "relatorio.docx"),
    stdout = TRUE, stderr = TRUE
  )
  status <- attr(saida, "status") %||% 0L
  gerado <- file.path(projeto, "relatorio.docx")
  if (!identical(as.integer(status), 0L) || !file.exists(gerado)) {
    stop(
      paste("O Quarto não conseguiu gerar o Word.", paste(saida, collapse = "\n")),
      call. = FALSE
    )
  }
  if (!file.copy(gerado, file, overwrite = TRUE)) {
    stop("Não foi possível copiar o relatório Word para o destino.", call. = FALSE)
  }
  invisible(file)
}
