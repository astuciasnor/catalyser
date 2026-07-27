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

exportacao_codigo_preparo_compartilhado <- function(pipeline, reg = tratamentos) {
  linhas <- c(
    "# Tratamentos adicionados à Base Compartilhada — gerados pela CatalyseR",
    "source('R/01_operacoes_estruturais.R', local = TRUE)",
    "dados <- base_resolvida",
    "trat_moda <- function(x) {",
    "  valores <- x[!is.na(x)]",
    "  if (!length(valores)) return(NA)",
    "  nomes <- names(sort(table(valores), decreasing = TRUE))",
    "  type.convert(nomes[[1]], as.is = TRUE)",
    "}",
    ""
  )
  ativas <- Filter(function(et) isTRUE(et$ativa), pipeline %||% list())
  if (!length(ativas)) linhas <- c(linhas, "# Nenhuma etapa compartilhada foi registrada.")
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
  c(linhas, "dados_analise <- dados", "", "print(dados_analise)")
}

exportacao_codigo_operacoes_estruturais <- function(base_externa = NULL) {
  linhas <- c(
    "# Operações estruturais anteriores à Trilha",
    "source('R/00_importar.R', local = TRUE)",
    "",
    "# A fotografia materializada garante que o projeto reproduza exatamente",
    "# a base promovida que alimentou a Trilha no momento da exportação.",
    "base_resolvida <- readRDS('dados/base_resolvida.rds')"
  )
  codigo <- base_externa$codigo %||% ""
  if (nzchar(codigo)) {
    linhas <- c(
      linhas, "", "# Código registrado na operação estrutural (referência pedagógica):",
      paste0("# > ", strsplit(codigo, "\n", fixed = TRUE)[[1]])
    )
  } else {
    linhas <- c(linhas, "", "# Nenhuma operação estrutural promovida foi registrada.")
  }
  c(linhas, "", "print(base_resolvida)")
}

exportacao_arquivo_base <- function(base, indice) {
  sprintf("R/03_%03d_%s.R", as.integer(indice), exportacao_nome_seguro(base$nome_r, base$id))
}

exportacao_arquivo_execucao <- function(execucao, indice) {
  sprintf(
    "R/04_%03d_%s.R", as.integer(indice),
    exportacao_nome_seguro(paste(execucao$id, execucao$tipo), execucao$id)
  )
}

exportacao_codigo_execucao <- function(execucao, indice, registro_bases, arquivos_bases) {
  variavel <- exportacao_nome_seguro(paste0("resultado_", execucao$id), "resultado_execucao")
  base <- bases_obter(registro_bases, execucao$base_id)
  carregar <- if (identical(execucao$base_tipo, "derivada") && !is.null(base)) {
    arquivo <- arquivos_bases[[base$id]]
    c(sprintf("source('%s', local = TRUE)", arquivo), sprintf("dados <- %s", base$nome_r))
  } else if (identical(execucao$base_id, "dados_analise") || identical(execucao$base_tipo, "compartilhada")) {
    c("source('R/02_preparo_compartilhado.R', local = TRUE)", "dados <- dados_analise")
  } else {
    c(
      "# Esta execução usa uma tabela preparada ou entrada manual guardada nos parâmetros.",
      "dados <- NULL"
    )
  }
  c(
    sprintf("# Execução %03d — %s", as.integer(indice), execucao$titulo),
    "source('R/00_funcoes_projeto.R', local = TRUE)",
    carregar,
    "",
    "# Configuração congelada pelo clique 'Adicionar aos resultados'.",
    paste0("execucao <- ", exportacao_dput_texto(execucao)),
    "",
    sprintf("%s <- catalyser_executar(execucao, dados)", variavel),
    sprintf("if (!is.null(%s$console)) cat(paste(%s$console, collapse = '\\n'), '\\n')", variavel, variavel)
  )
}

exportacao_yaml_texto <- function(x) {
  x <- as.character(x %||% "")
  paste0('"', gsub('"', '\\"', x, fixed = TRUE), '"')
}

exportacao_codigo_estudo <- function(execucao) {
  p <- execucao$parametros %||% list()
  texto_r <- function(x) exportacao_dput_texto(as.character(x))
  numero_r <- function(x) exportacao_dput_texto(as.numeric(x))
  vetor_r <- function(x) exportacao_dput_texto(as.character(x %||% character()))
  carregar <- if (identical(execucao$base_tipo, "derivada")) {
    sprintf(
      "dados <- readRDS('dados/%s.rds')",
      exportacao_nome_seguro(execucao$base_objeto, "base_derivada")
    )
  } else if (identical(execucao$base_tipo, "compartilhada") ||
             identical(execucao$base_id, "dados_analise")) {
    "dados <- readRDS('dados/dados_analise.rds')"
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
      sprintf(
        "formula_anova <- stats::reformulate(%s, response = %s)",
        texto_r(p$fator), texto_r(p$resposta)
      ),
      "modelo_anova <- stats::aov(formula_anova, data = dados)",
      "",
      "summary(modelo_anova)",
      sprintf("stats::TukeyHSD(modelo_anova, conf.level = %s)",
              numero_r(p$nivel_confianca %||% 0.95)),
      sprintf(
        "car::leveneTest(dados[[%s]], as.factor(dados[[%s]]), center = stats::median)",
        texto_r(p$resposta), texto_r(p$fator)
      ),
      "stats::shapiro.test(stats::residuals(modelo_anova))",
      "effectsize::eta_squared(modelo_anova)",
      "effectsize::omega_squared(modelo_anova)"
    ),
    grafico_linhas = c(
      sprintf(
        "grafico <- ggplot2::ggplot(dados, ggplot2::aes(x = .data[[%s]], y = .data[[%s]], group = 1)) +",
        texto_r(p$x), texto_r(p$y)
      ),
      sprintf("  ggplot2::geom_line(linewidth = %s, color = '#0F3B5F') +",
              numero_r(p$espessura_linha %||% 1)),
      if (isTRUE(p$mostrar_pontos))
        "  ggplot2::geom_point(size = 2.2, color = '#2E7D8F') +" else NULL,
      "  ggplot2::theme_minimal()",
      "grafico"
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
    "# Código R essencial desta execução.",
    "# Este chunk pode ser executado manualmente no RStudio.",
    carregar,
    "",
    Filter(Negate(is.null), codigo)
  )
}

exportacao_bloco_componente <- function(variavel, execucao_id, componente) {
  rotulo <- comunicacao_rotulos_saidas[[componente]] %||% componente
  id_chunk <- exportacao_nome_seguro(paste(execucao_id, componente), "resultado")
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

exportacao_gerar_qmd <- function(manifesto, titulo_projeto = "Relatório de análise") {
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
    "```{r}",
    "#| label: configuracao",
    "#| include: false",
    "source('R/00_funcoes_projeto.R', local = TRUE)",
    "manifesto <- readRDS('metadados/manifesto_editorial.rds')",
    "bases_projeto <- utils::read.csv('metadados/bases.csv', check.names = FALSE)",
    "```",
    "",
    "# Preparação dos dados",
    "",
    "A análise partiu de uma base compartilhada reproduzível. As bases derivadas, quando existentes, nasceram diretamente dela em um único salto.",
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
  ordem_completa <- names(manifesto$execucoes %||% list())
  for (item in incluidas) {
    indice <- match(item$id, ordem_completa)
    arquivo <- exportacao_arquivo_execucao(item, indice)
    variavel <- exportacao_nome_seguro(paste0("resultado_", item$id), "resultado_execucao")
    codigo_estudo <- exportacao_codigo_estudo(item)
    linhas <- c(
      linhas,
      paste0("## ", item$titulo),
      "",
      sprintf("**Base utilizada:** `%s`  ", item$base_objeto),
      sprintf("**Execução registrada:** `%s`", item$id),
      "",
      "<!-- O código abaixo fica explícito no QMD para estudo, mas não entra no Word. -->",
      "```{r}",
      sprintf("#| label: codigo-%s", exportacao_nome_seguro(item$id)),
      "#| eval: false",
      "#| include: false",
      codigo_estudo,
      "```",
      "",
      "```{r}",
      sprintf("#| label: executar-%s", exportacao_nome_seguro(item$id)),
      "#| include: false",
      sprintf("ambiente_%s <- new.env(parent = globalenv())", exportacao_nome_seguro(item$id)),
      sprintf("sys.source('%s', envir = ambiente_%s)", arquivo, exportacao_nome_seguro(item$id)),
      sprintf("%s <- get('%s', envir = ambiente_%s)", variavel, variavel, exportacao_nome_seguro(item$id)),
      "```",
      ""
    )
    for (componente in item$saidas_word) {
      linhas <- c(linhas, exportacao_bloco_componente(variavel, item$id, componente))
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

  exportacao_salvar_dataframe(dados_brutos, file.path(projeto, "dados", "dados_brutos.rds"))
  exportacao_salvar_dataframe(base_resolvida, file.path(projeto, "dados", "base_resolvida.rds"))
  exportacao_salvar_dataframe(
    dados_analise,
    file.path(projeto, "dados", "dados_analise.rds"),
    file.path(projeto, "dados", "dados_analise.csv")
  )

  template_funcoes <- file.path(templates_dir, "funcoes_projeto_integrado.R")
  template_word <- file.path(templates_dir, "custom-reference.docx")
  if (!file.exists(template_funcoes) || !file.exists(template_word)) {
    stop("Os templates do exportador integrado não foram encontrados.", call. = FALSE)
  }
  file.copy(template_funcoes, file.path(projeto, "R", "00_funcoes_projeto.R"), overwrite = TRUE)
  file.copy(template_word, file.path(projeto, "custom-reference.docx"), overwrite = TRUE)

  writeLines(
    c(
      "# Dados brutos preservados pela CatalyseR",
      "dados_brutos <- readRDS('dados/dados_brutos.rds')",
      "dados <- dados_brutos",
      "print(dados_brutos)"
    ),
    file.path(projeto, "R", "00_importar.R"), useBytes = TRUE
  )
  writeLines(
    exportacao_codigo_operacoes_estruturais(base_externa),
    file.path(projeto, "R", "01_operacoes_estruturais.R"), useBytes = TRUE
  )
  writeLines(
    exportacao_codigo_preparo_compartilhado(pipeline),
    file.path(projeto, "R", "02_preparo_compartilhado.R"), useBytes = TRUE
  )

  arquivos_bases <- list()
  if (length(registro_bases)) {
    for (i in seq_along(registro_bases)) {
      base <- registro_bases[[i]]
      arquivo <- exportacao_arquivo_base(base, i)
      arquivos_bases[[base$id]] <- arquivo
      writeLines(
        c("source('R/02_preparo_compartilhado.R', local = TRUE)", "", strsplit(bases_codigo(base), "\n", fixed = TRUE)[[1]]),
        file.path(projeto, arquivo), useBytes = TRUE
      )
      entrada_cache <- bases_cache_obter(cache_bases, base$id)
      if (!is.null(entrada_cache$df)) {
        exportacao_salvar_dataframe(
          entrada_cache$df,
          file.path(projeto, "dados", paste0(exportacao_nome_seguro(base$nome_r), ".rds")),
          file.path(projeto, "dados", paste0(exportacao_nome_seguro(base$nome_r), ".csv"))
        )
      }
    }
  }

  ordem <- names(manifesto$execucoes)
  for (i in seq_along(ordem)) {
    id <- ordem[[i]]
    execucao <- registro_execucoes[[id]]
    arquivo <- exportacao_arquivo_execucao(execucao, i)
    writeLines(
      exportacao_codigo_execucao(execucao, i, registro_bases, arquivos_bases),
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
    exportacao_gerar_qmd(manifesto, titulo),
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
    c(
      paste0("# ", nome_projeto), "",
      "Projeto de análise gerado pela CatalyseR.", "",
      "## Como usar", "",
      "1. Abra `projeto_analise.Rproj` no RStudio.",
      "2. Execute os arquivos da pasta `R/` na ordem numérica para estudar o código.",
      "3. Abra `relatorio.qmd`: cada análise contém um chunk pedagógico com o código R essencial.",
      "4. Os chunks pedagógicos usam `eval: false` e `include: false`: podem ser executados manualmente, mas não poluem o Word.",
      "5. Clique em **Render** para gerar o Word.",
      "6. O Projeto R preserva todas as execuções; o relatório mostra somente as escolhas do manifesto.", "",
      "As bases derivadas nascem diretamente de `dados_analise`; não existem ramos de ramos."
    ),
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
