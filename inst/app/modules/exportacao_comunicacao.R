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

# Escreve uma lista de parâmetros como código R legível, um item por linha,
# com listas aninhadas recuadas. É o que aparece no chunk de apresentação do
# relatório: os parâmetros exatamente como foram escolhidos na tela.
exportacao_lista_r <- function(x, recuo = 0L) {
  espaco <- strrep(" ", recuo)
  if (!is.list(x) || is.data.frame(x) || !length(x)) {
    valor <- paste(capture.output(dput(x)), collapse = " ")
    return(gsub("\\s+", " ", valor))
  }
  nomes <- names(x) %||% rep("", length(x))
  itens <- vapply(seq_along(x), function(i) {
    valor <- exportacao_lista_r(x[[i]], recuo + 2L)
    if (nzchar(nomes[[i]])) {
      sprintf("%s  %s = %s", espaco, nomes[[i]], valor)
    } else {
      sprintf("%s  %s", espaco, valor)
    }
  }, character(1))
  paste0("list(\n", paste(itens, collapse = ",\n"), "\n", espaco, ")")
}

# Nome de objeto R a partir da raiz do chunk: `anova-profundidade-m` vira
# `anova_profundidade_m`. É o objeto que guarda o resultado de uma análise.
exportacao_nome_resultado <- function(raiz) {
  nome <- gsub("-", "_", raiz, fixed = TRUE)
  if (!grepl("^[A-Za-z]", nome)) nome <- paste0("resultado_", nome)
  nome
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

# ---- Chunks de preparo: importar e tratar ------------------------------------
# O Projeto R exportado segue o EAPACaderno, o projeto-modelo do ecossistema
# EAPA, em programação literária: um único `relatorios/relatorio.qmd` faz tudo,
# com caminhos por here(). O chunk `importar` lê a planilha e deixa
# `dados_brutos` na memória; o chunk `tratar` aplica as operações estruturais e
# a trilha de tratamentos, confere contra a fotografia e deixa `dados_analise`.
# Não há scripts em R/: a análise mora no relatório (Fase C, set/2026).

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

# Chunk `importar`: da planilha ao data.frame bruto, sem mexer em nada.
exportacao_chunk_importar <- function(import_info = list()) {
  planilha <- exportacao_nome_planilha(import_info)
  aba <- exportacao_aba_planilha(import_info)
  c(
    "```{r}",
    "#| label: importar",
    "#| output: false",
    "# Ler a planilha como ela veio, sem mexer em nada. É a mesma que você",
    "# importou na CatalyseR, exportada junto com o projeto. Se quiser rodar o",
    "# projeto com outra planilha de mesma estrutura, troque o caminho abaixo.",
    sprintf("caminho_planilha <- here(\"dados\", \"brutos\", \"%s\")", planilha),
    sprintf("aba_planilha <- \"%s\"", aba),
    "",
    "dados_brutos <- as.data.frame(read_excel(caminho_planilha, sheet = aba_planilha))",
    "",
    "# Primeira olhada: quantas linhas e colunas vieram, e o tipo de cada coluna.",
    "str(dados_brutos)",
    "```"
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
    "base_resolvida <- as.data.frame(readRDS(here(\"dados\", \"processados\", \"base_resolvida.rds\")))",
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

# Chunk `tratar`: da planilha bruta à Base Compartilhada (`dados_analise`).
exportacao_chunk_tratar <- function(pipeline, base_externa = NULL,
                                    reg = tratamentos) {
  c(
    "```{r}",
    "#| label: tratar",
    "#| output: false",
    "# Aqui a planilha vira a Base Compartilhada (`dados_analise`), exatamente",
    "# como aconteceu na CatalyseR: primeiro as operações estruturais, depois a",
    "# Trilha de Preparo, na ordem lógica registrada. No fim, o resultado é",
    "# conferido contra a fotografia que a IDE exportou. O passo 1 (ler a",
    "# planilha) é o chunk `importar`, logo acima.",
    "",
    exportacao_bloco_estrutural(base_externa),
    exportacao_bloco_trilha(pipeline, reg),
    "# -----------------------------------------------------------------------",
    "# 4. Conferência",
    "# -----------------------------------------------------------------------",
    "# A CatalyseR também exportou uma fotografia de `dados_analise`. A função",
    "# abaixo compara a base reconstruída com ela e avisa se algo divergir.",
    "catalyser_conferir_base(",
    "  dados_analise,",
    "  here(\"dados\", \"processados\", \"dados_analise.rds\"),",
    "  rotulo = \"Base Compartilhada\"",
    ")",
    "```"
  )
}

# Nome legível de cada tipo de análise (para textos e rótulos).
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

#' Frase curta com a pergunta que a análise responde
#'
#' Abre a seção de cada análise no relatório, para o leitor saber, na primeira
#' linha, o que está prestes a ler.
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
  # Este bloco só aparece quando o código de estudo é lido isolado (fora do
  # relatório). No relatório a base já está montada pelos chunks anteriores, e
  # `incluir_carregamento` é FALSE — nada de instrução duplicada.
  carregar <- if (identical(execucao$base_tipo, "derivada")) {
    c(
      "# Antes de rodar este bloco, construa a base desta análise: rode, em",
      "# relatorios/relatorio.qmd, os chunks `configuracao`, `importar`, `tratar`",
      sprintf("# e depois o chunk '%s-base', que deixa '%s' na memória.",
              exportacao_raiz_chunk(execucao), execucao$base_objeto),
      sprintf("dados <- %s", execucao$base_objeto)
    )
  } else if (identical(execucao$base_tipo, "compartilhada") ||
             identical(execucao$base_id, "dados_analise")) {
    c(
      "# Antes de rodar este bloco, rode os chunks `configuracao`, `importar` e",
      "# `tratar` de relatorios/relatorio.qmd: eles deixam `dados_analise` na memória.",
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
    c(tabela = "modelo", celulas = "celulas", efeito = "efeitos",
      comparacoes = "tukey", grafico_combinacoes = "combinacoes")
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

# Uma linha de comentário por componente, para o chunk dizer o que mostra.
exportacao_comentario_componente <- function(componente, rotulo) {
  frases <- c(
    narrativa = "O texto em português, pronto para o relatório.",
    descritivos = "Resumo por grupo: n, média e desvio padrão.",
    tabela = "A tabela do modelo ajustado.",
    comparacoes = "Comparações par a par: quem difere de quem.",
    grafico = "O gráfico, como apareceu na CatalyseR.",
    pressupostos = "Os testes de pressupostos do modelo.",
    diagnosticos = "Os gráficos de diagnóstico dos resíduos."
  )
  if (componente %in% names(frases)) return(paste0("# ", frases[[componente]]))
  sprintf("# %s desta análise, como na CatalyseR.", rotulo)
}

exportacao_bloco_componente <- function(variavel, execucao_id, componente,
                                        raiz_chunk = NULL, tipo = NULL) {
  rotulo <- comunicacao_rotulos_saidas[[componente]] %||% componente
  id_chunk <- if (is.null(raiz_chunk)) {
    exportacao_slug_chunk(paste(execucao_id, componente), "resultado")
  } else {
    paste0(raiz_chunk, "-", exportacao_sufixo_componente(tipo, componente))
  }
  # [[ ]] em vez de $: o $ do R completa nomes pela metade, e `grafico`
  # poderia virar `grafico_combinacoes` sem ninguém perceber.
  expressao <- if (identical(componente, "console")) {
    sprintf(
      "cat('```text\\n', paste(%s[[\"console\"]], collapse = '\\n'), '\\n```\\n')",
      variavel
    )
  } else {
    sprintf("catalyser_mostrar(%s[[\"%s\"]])", variavel, componente)
  }
  c(
    sprintf("### %s", rotulo),
    "",
    "```{r}",
    sprintf("#| label: %s", id_chunk),
    "#| echo: false",
    "#| results: asis",
    exportacao_comentario_componente(componente, rotulo),
    expressao,
    "```",
    ""
  )
}

# Chunk de apresentação: a análise refeita pela função da CatalyseR, com os
# parâmetros escritos por extenso. É o que alimenta as tabelas, os gráficos e
# a narrativa do Word, e não depende de nenhum arquivo de metadados.
exportacao_bloco_apresentacao <- function(item, raiz, variavel) {
  configuracao <- list(
    tipo = item$tipo,
    titulo = item$titulo,
    parametros = item$parametros %||% list()
  )
  lista <- strsplit(exportacao_lista_r(configuracao, 0L), "\n", fixed = TRUE)[[1]]
  lista[length(lista)] <- paste0(lista[length(lista)], ",")
  c(
    "<!-- Apresentação: a mesma análise, agora pela função da CatalyseR, que",
    "     devolve narrativa, tabelas e gráficos já formatados para o Word. Os",
    "     parâmetros abaixo são exatamente os que você escolheu na tela. -->",
    "```{r}",
    sprintf("#| label: %s-resultado", raiz),
    "#| include: false",
    sprintf("%s <- catalyser_executar(", variavel),
    paste0("  ", lista),
    "  dados_da_analise",
    ")",
    "```",
    ""
  )
}

# Tipos cujo código de estudo já foi humanizado e testado de ponta a ponta.
# Só esses rodam de verdade no relatório; os demais ficam como código para
# leitura (eval: false), até ganharem o mesmo tratamento.
exportacao_tipos_com_codigo_vivo <- c("anova_um_fator", "anova_dois_fatores", "grafico_linhas")

# Chunk da análise passo a passo: o código que o aluno escreveria no RStudio.
exportacao_bloco_analise <- function(item, raiz) {
  vivo <- as.character(item$tipo %||% "") %in% exportacao_tipos_com_codigo_vivo
  codigo_estudo <- exportacao_codigo_estudo(item, incluir_carregamento = FALSE)
  c(
    if (vivo) c(
      "<!-- A análise, passo a passo: é o que você escreveria no RStudio para",
      "     fazer esta análise sem a CatalyseR. Roda de verdade, mas não imprime",
      "     nada no Word (output: false); as saídas formatadas vêm do bloco",
      "     seguinte. Para ver os objetos crus, rode este chunk no RStudio. -->"
    ) else c(
      "<!-- A análise, passo a passo: é o que você escreveria no RStudio para",
      "     fazer esta análise sem a CatalyseR. Este tipo de análise ainda não",
      "     tem o código passo a passo validado, então o chunk fica só para",
      "     leitura (eval: false); as saídas vêm do bloco seguinte. -->"
    ),
    "```{r}",
    sprintf("#| label: %s-analise", raiz),
    "#| echo: false",
    if (vivo) "#| output: false" else "#| eval: false",
    "dados <- dados_da_analise",
    "",
    codigo_estudo,
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
                                 registro_bases = list(), pipeline = list(),
                                 base_externa = NULL, import_info = list()) {
  globais <- manifesto$secoes_globais %||% list()
  planilha <- exportacao_nome_planilha(import_info)
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
    "  Este documento é o projeto inteiro: os dados entram, são preparados e",
    "  analisados aqui dentro, e o texto é escrito em cima dos resultados. Não há",
    "  scripts separados. Para refazer tudo do zero, reinicie o R e clique em",
    "  Render. Para ver qualquer passo, rode o chunk correspondente no RStudio",
    "  (Ctrl+Shift+Enter): a saída aparece no console, como sempre.",
    "",
    "  O primeiro bloco só prepara o terreno. Cada linha carrega uma coisa:",
    "    1. o pacote here, que monta os caminhos a partir da raiz do projeto;",
    "    2. o pacote readxl, que lê a planilha;",
    "    3. o pacote catalyser, que traz as funções de análise;",
    "    4. a lista das bases usadas, para a tabela de Preparação dos dados.",
    "  Depois vêm os chunks importar e tratar (a planilha vira a Base",
    "  Compartilhada) e, em Resultados, cada análise em três blocos: a base que",
    "  ela usa, a análise passo a passo e a apresentação.",
    "-->",
    "```{r}",
    "#| label: configuracao",
    "#| include: false",
    "# 1. Caminhos a partir da raiz do projeto (onde está o .Rproj).",
    "library(here)",
    "",
    "# 2. Leitura da planilha.",
    "library(readxl)",
    "",
    "# 3. Pacote com as funções de análise (?catalyser_anova mostra a ajuda).",
    "if (!requireNamespace(\"catalyser\", quietly = TRUE)) {",
    "  stop(\"Este projeto usa o pacote catalyser. Para instalar: \",",
    "       \"remotes::install_github('astuciasnor/catalyser')\", call. = FALSE)",
    "}",
    "library(catalyser)",
    "",
    "# 4. Lista das bases do projeto, para a tabela de Preparação dos dados.",
    "bases_projeto <- utils::read.csv(",
    "  here(\"metadados\", \"bases.csv\"),",
    "  check.names = FALSE",
    ")",
    "```",
    "",
    "# Preparação dos dados",
    "",
    "## Os dados",
    "",
    sprintf("Os dados entram pela planilha `dados/brutos/%s`, a mesma que você importou na CatalyseR. Ela é somente-leitura: toda transformação acontece em código, no chunk `tratar`, e fica registrada.", planilha),
    "",
    exportacao_chunk_importar(import_info),
    "",
    "## Preparo dos dados",
    "",
    "Aqui a planilha vira a Base Compartilhada (`dados_analise`): as operações estruturais e a Trilha de Preparo, na mesma ordem lógica que você registrou na CatalyseR. No fim, o chunk confere o resultado contra a fotografia exportada com o projeto e avisa se algo divergir. Nenhuma transformação acontece fora deste documento.",
    "",
    exportacao_chunk_tratar(pipeline, base_externa),
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
    "As configurações analíticas foram registradas explicitamente na CatalyseR. Cada análise, abaixo, informa a pergunta, a base e os parâmetros usados.",
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
    raiz <- if (item$id %in% names(raizes)) unname(raizes[[item$id]]) else
      exportacao_slug_chunk(item$id, "analise")
    # O objeto do resultado leva o nome da análise (`anova_profundidade_m`),
    # não o ID interno da execução.
    variavel <- exportacao_nome_resultado(raiz)
    linhas <- c(
      linhas,
      paste0("## ", item$titulo),
      "",
      sprintf("**Pergunta:** %s  ", exportacao_pergunta(item)),
      sprintf("**Base utilizada:** `%s`  ", item$base_objeto),
      sprintf("**Execução registrada:** `%s`", item$id),
      "",
      exportacao_bloco_base_analise(item, raiz, registro_bases),
      exportacao_bloco_analise(item, raiz),
      exportacao_bloco_apresentacao(item, raiz, variavel)
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
  planilha <- exportacao_nome_planilha(import_info)
  c(
    paste0("# ", nome_projeto), "",
    "Projeto de análise gerado pela CatalyseR. Ele tem a mesma estrutura do",
    "EAPACaderno, o projeto-modelo do ecossistema EAPA: uma planilha entra, um",
    "documento faz tudo, um relatório em Word sai. A diferença é que aqui a",
    "análise já foi feita: você a montou na CatalyseR, e o documento a refaz em",
    "código, para você ler, rodar e adaptar.", "",
    "## A estrutura", "",
    "```",
    paste0(nome_projeto, "/"),
    "│",
    "├── projeto_analise.Rproj     abra o projeto por aqui (define a raiz para o here)",
    "├── README.md                 este arquivo",
    "│",
    "├── dados/",
    sprintf("│   ├── brutos/%s", planilha),
    "│   │                         a planilha que você importou na CatalyseR. SOMENTE LEITURA",
    "│   └── processados/",
    "│       ├── dados_analise.rds        fotografia da base tratada, só para conferência",
    "│       └── base_compartilhada.xlsx  a mesma base, para abrir no Excel",
    "│",
    "├── imagens/                  fotos, esquemas e mapas que NÃO vêm do código (começa vazia)",
    "│",
    "├── relatorios/",
    "│   ├── relatorio.qmd         O PROJETO: importa, trata e refaz cada análise",
    "│   ├── custom-reference.docx modelo de página do Word",
    "│   └── relatorio.docx        o relatório pronto (gerado)",
    "│",
    "└── metadados/                memória técnica da exportação (não precisa abrir)",
    "```", "",
    "Não há scripts em `R/`: a análise inteira mora no `relatorio.qmd`, em chunks",
    "nomeados na ordem do trabalho. É o mesmo desenho do EAPACaderno.", "",
    "## Como rodar", "",
    "1. Abra `projeto_analise.Rproj` no RStudio. Isso define a raiz do projeto,",
    "   que é o que o `here()` usa para montar os caminhos.",
    "2. Se faltar algum pacote, instale uma vez:",
    "   `install.packages(c(\"here\", \"readxl\"))` e",
    "   `remotes::install_github(\"astuciasnor/catalyser\")`.",
    "3. Abra `relatorios/relatorio.qmd` e clique em **Render**. O documento lê a",
    "   planilha, refaz a Base Compartilhada e cada análise; o Word sai em",
    "   `relatorios/relatorio.docx`.",
    "4. Para ver um passo isolado, rode o chunk correspondente no RStudio",
    "   (Ctrl+Shift+Enter), na ordem: `configuracao`, `importar`, `tratar` e, em",
    "   cada análise, `...-base`, `...-analise` e `...-resultado`. A saída aparece",
    "   no console, como sempre.",
    "5. Quer um script puro? `knitr::purl(\"relatorios/relatorio.qmd\",",
    "   output = \"relatorios/relatorio.R\")` extrai o código dos chunks, na",
    "   ordem, com o nome de cada um. O `.R` é um subproduto: a fonte é o `.qmd`.", "",
    "Antes de renderizar, reinicie o R (Ctrl+Shift+F10). Se o relatório sai com",
    "a memória limpa, a análise é reprodutível.", "",
    "## O caminho dos dados", "",
    sprintf("1. A planilha bruta, em `dados/brutos/%s`.", planilha),
    "2. O chunk `importar` lê a planilha e deixa `dados_brutos` na memória.",
    "3. O chunk `tratar` aplica as operações estruturais e a trilha de",
    "   tratamentos, confere o resultado contra a fotografia e deixa",
    "   `dados_analise`. É **o único** lugar que produz a Base Compartilhada.",
    "4. Em Resultados, cada análise aparece em três chunks: a base (`...-base`),",
    "   a análise passo a passo (`...-analise`) e a apresentação",
    "   (`...-resultado`).", "",
    "As bases derivadas nascem diretamente de `dados_analise`, em um único salto:",
    "não existem ramos de ramos. Não há cópia delas em disco: a receita está no",
    "código.", "",
    "## Os arquivos de `dados/processados/`", "",
    "- `dados_analise.rds` — fotografia da Base Compartilhada, usada **só para",
    "  conferência**: o projeto compara o que reconstruiu com o que você viu.",
    "- `base_compartilhada.xlsx` — a base já tratada, para abrir no Excel ou",
    "  enviar a quem não usa R. É entrega, não fonte do relatório.",
    "- `base_resolvida.rds` — aparece apenas se houve mudança estrutural",
    "  promovida (Pivotar/Separar ou Criar e Editar Variáveis e Níveis).", "",
    "## O relatório", "",
    "Abra `relatorios/relatorio.qmd`. A análise mora dentro dele, e cada uma",
    "aparece em três chunks:", "",
    "- `...-base`: o salto da Base Compartilhada até a base desta análise;",
    "- `...-analise`: a análise passo a passo, o código que você escreveria no",
    "  RStudio (`aov`, `TukeyHSD`, `ggplot`...). Ela roda de verdade, mas não",
    "  imprime no Word (`output: false`). Rode o chunk no RStudio",
    "  (Ctrl+Shift+Enter) e a saída aparece no console, como sempre;",
    "- `...-resultado`: a mesma análise pela função da CatalyseR, com os",
    "  parâmetros escritos por extenso. É ela que alimenta as tabelas, os",
    "  gráficos e a narrativa do Word.", "",
    "Todos os chunks usam `echo: false`: o código fica visível no fonte e o",
    "Word sai limpo. O projeto preserva todas as execuções registradas; o",
    "relatório mostra somente as escolhas do manifesto editorial.", "",
    "## Relação com o EAPACaderno", "",
    "Quem conhece o EAPACaderno (o projeto-modelo do ecossistema) reconhece tudo",
    "aqui: `here()`, um documento que importa, trata e analisa, dados brutos",
    "intocáveis, Word no mesmo modelo de página. Duas diferenças, de propósito:",
    "as funções de análise vêm do pacote `catalyser`, para que o resultado seja",
    "idêntico ao que você viu na tela; e a pasta `metadados/` guarda a memória",
    "da exportação, para a CatalyseR poder reabrir o projeto.", "",
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
  # A árvore é a do EAPACaderno: dados/ (brutos e processados), imagens/,
  # relatorios/ e, só aqui, metadados/. Sem R/: a análise mora no relatório.
  dirs <- file.path(projeto, c(
    file.path("dados", "brutos"), file.path("dados", "processados"),
    "imagens", "relatorios", "metadados"
  ))
  vapply(dirs, dir.create, logical(1), recursive = TRUE, showWarnings = FALSE)

  # A pasta `dados/` é deliberadamente enxuta:
  #
  #   brutos/      a planilha bruta -> ponto de entrada do chunk importar;
  #   processados/ dados_analise.rds -> fotografia, usada apenas para conferência;
  #                base_compartilhada.xlsx -> entrega, para uso fora do R.
  #
  # Nada de cópias redundantes: o que o projeto sabe reconstruir, ele reconstrói.
  exportacao_salvar_planilha(
    dados_brutos,
    file.path(projeto, "dados", "brutos", exportacao_nome_planilha(import_info)),
    aba = exportacao_aba_planilha(import_info)
  )
  exportacao_salvar_dataframe(
    dados_analise, file.path(projeto, "dados", "processados", "dados_analise.rds")
  )
  exportacao_salvar_planilha(
    dados_analise,
    file.path(projeto, "dados", "processados", "base_compartilhada.xlsx"),
    aba = "dados_analise"
  )
  # A fotografia pós-estrutural só é necessária quando houve Pivotar/Organizar
  # promovido: é o único trecho que o script não consegue reconstruir sozinho.
  if (nzchar(trimws(as.character(base_externa$codigo %||% "")))) {
    exportacao_salvar_dataframe(
      base_resolvida, file.path(projeto, "dados", "processados", "base_resolvida.rds")
    )
  }

  # As funções de análise não viajam como arquivo: vêm do pacote catalyser,
  # documentadas e com ajuda em português (`?catalyser_anova`). O modelo de
  # página do Word vai junto com o relatório, como no projeto-modelo.
  template_word <- file.path(templates_dir, "custom-reference.docx")
  if (!file.exists(template_word)) {
    stop("O template Word do exportador não foi encontrado.", call. = FALSE)
  }
  file.copy(template_word, file.path(projeto, "relatorios", "custom-reference.docx"), overwrite = TRUE)

  # Não há scripts: o preparo (chunks importar e tratar) e cada análise moram
  # no relatorio.qmd. As bases derivadas não têm fotografia em disco: são um
  # salto reproduzível a partir de `dados_analise`, com a receita de
  # `bases_codigo()` no chunk da própria análise.

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
    exportacao_gerar_qmd(
      manifesto, titulo, registro_bases = registro_bases,
      pipeline = pipeline, base_externa = base_externa, import_info = import_info
    ),
    file.path(projeto, "relatorios", "relatorio.qmd"), useBytes = TRUE
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
    c("render", file.path("relatorios", "relatorio.qmd"), "--to", "docx",
      "--output", "relatorio.docx"),
    stdout = TRUE, stderr = TRUE
  )
  status <- attr(saida, "status") %||% 0L
  # O Quarto grava a saída ao lado do .qmd; versões antigas gravavam na pasta
  # de trabalho. Aceitamos os dois lugares.
  gerado <- file.path(projeto, "relatorios", "relatorio.docx")
  if (!file.exists(gerado) && file.exists(file.path(projeto, "relatorio.docx"))) {
    gerado <- file.path(projeto, "relatorio.docx")
  }
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
