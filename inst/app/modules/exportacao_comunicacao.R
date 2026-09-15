# Exportação da Comunicação de Resultados — o Projeto R
# -----------------------------------------------------------------------------
# O manifesto editorial escolhe o que entra no relatório. O registro central de
# execuções, por sua vez, sempre é preservado integralmente no Projeto R.
#
# Desde a Fase D (set/2026) a CatalyseR não gera o Word: ela gera só o Projeto
# R, e o Word (e o caderno HTML) nascem no RStudio, quando o pesquisador clica
# em Render. O projeto exportado é o par do EAPACaderno:
#
#   R/analise.R              o código, comentado, em trechos "## ---- nome ----"
#   relatorios/relatorio.qmd o texto, com o código limpo copiado do script;
#                            a primeira linha de cada chunk diz "# fonte: ..."
#   R/funcoes.R              atualizar_codigo() e conferir_codigo(), que ligam
#                            os dois (template inst/app/templates/funcoes.R)
#
# O exportador gera o script, gera o relatório com as cascas dos chunks e
# chama o MESMO atualizar_codigo() que o pesquisador vai usar depois.

exportacao_nome_seguro <- function(x, padrao = "analise") {
  x <- trimws(as.character(x %||% ""))
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT", sub = "")
  x <- tolower(gsub("[^A-Za-z0-9]+", "_", x))
  x <- gsub("^_+|_+$", "", x)
  if (!nzchar(x)) padrao else x
}

# Nome curto independente do título: no máximo duas palavras.
exportacao_nome_curto <- function(x, padrao = "analise") {
  nome <- exportacao_nome_seguro(x, padrao)
  palavras <- strsplit(nome, "_", fixed = TRUE)[[1]]
  nome <- paste(head(palavras[nzchar(palavras)], 2L), collapse = "_")
  # Nomes reservados do Windows não podem identificar pastas.
  if (toupper(nome) %in% c("CON", "PRN", "AUX", "NUL", paste0("COM", 1:9), paste0("LPT", 1:9)))
    nome <- paste0(nome, "_analise")
  nome
}

exportacao_origem_texto <- function(info = list()) {
  !identical(info$source, "package") &&
    tolower(tools::file_ext(info$file_name %||% "")) %in% c("csv", "txt", "tsv")
}

exportacao_sugerir_nome_projeto <- function(info = list()) {
  origem <- if (identical(info$source, "package")) info$package_dataset else {
    aba <- if (exportacao_origem_texto(info)) "" else as.character(info$excel_sheet %||% "")
    if (nzchar(aba) && !grepl("^[0-9]+$", aba)) sub("^[0-9]+[ ._-]*", "", aba)
    else tools::file_path_sans_ext(basename(info$file_name %||% "analise"))
  }
  exportacao_nome_curto(origem)
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
    if (!length(incluidas)) mensagens <- c(mensagens, "Selecione ao menos uma execução para o relatório.")
    sem_conteudo <- names(Filter(
      function(x) isTRUE(x$incluir_word) && !length(x$saidas_word), execucoes
    ))
    if (length(sem_conteudo)) {
      mensagens <- c(
        mensagens,
        sprintf("Escolha o conteúdo do relatório para: %s.", paste(sem_conteudo, collapse = ", "))
      )
    }
  }
  list(ok = !length(mensagens), mensagens = mensagens)
}

# ---- Trechos do script: instalar, pacotes, importar e tratar -----------------
# O Projeto R exportado segue o EAPACaderno, o projeto-modelo do ecossistema
# EAPA, em programação literária. Cada função abaixo devolve um TRECHO de
# R/analise.R: o marcador "## ---- nome ----", os comentários que explicam e o
# código. O relatório recebe só o código (atualizar_codigo() tira as linhas de
# comentário), num chunk cuja primeira linha diz de que trecho ele veio.
# O trecho `importar` lê a planilha e deixa `dados_brutos` na memória; o
# `tratar` aplica as operações estruturais e a trilha de tratamentos, confere
# contra a fotografia e deixa `dados_analise`.

exportacao_marcador <- function(nome) sprintf("## ---- %s ----", nome)

# Casca de um chunk do relatório: as opções e a linha "# fonte:". O corpo é
# preenchido depois, por atualizar_codigo(), a partir do script.
exportacao_casca_chunk <- function(label, fontes = label, opcoes = character()) {
  c(
    "```{r}",
    sprintf("#| label: %s", label),
    opcoes,
    sprintf("# fonte: %s", paste(fontes, collapse = ", ")),
    "```"
  )
}

exportacao_trecho_instalar <- function() {
  c(
    exportacao_marcador("instalar"),
    "# Instala só o que ainda falta. Rode uma única vez, ao preparar um computador",
    "# novo, antes do primeiro Render (o relatório precisa do here logo no início).",
    "# No relatório o chunk é eval: false, então nunca instala nada no Render.",
    "",
    "# Pacotes do CRAN usados na leitura, no preparo, nas análises e nas saídas.",
    "pacotes <- c(",
    "  \"here\", \"readxl\", \"readr\", \"writexl\", \"dplyr\", \"tidyr\", \"tibble\",",
    "  \"ggplot2\", \"stringr\", \"purrr\", \"lubridate\", \"knitr\", \"rmarkdown\",",
    "  \"flextable\", \"car\", \"multcompView\", \"effectsize\", \"remotes\",",
    "  \"broom\", \"performance\"",
    ")",
    "",
    "faltando <- pacotes[!pacotes %in% rownames(installed.packages())]",
    "",
    "if (length(faltando)) {",
    "  install.packages(faltando, repos = \"https://cloud.r-project.org\")",
    "}",
    "",
    "# Os dois pacotes do ecossistema vêm do GitHub. As dependências acompanham.",
    "if (!\"EAPADados\" %in% rownames(installed.packages()) ||",
    "    packageVersion(\"EAPADados\") < package_version(\"0.1.10\")) {",
    "  remotes::install_github(\"astuciasnor/EAPADados\", upgrade = \"never\")",
    "}",
    "if (!\"catalyser\" %in% rownames(installed.packages()) ||",
    "    packageVersion(\"catalyser\") < package_version(\"0.1.8\")) {",
    "  remotes::install_github(\"astuciasnor/catalyser\", upgrade = \"never\")",
    "}",
    "# Depois de instalar ou atualizar, reinicie o R antes de executar a análise.",
    ""
  )
}

exportacao_trecho_pacotes <- function() {
  c(
    exportacao_marcador("pacotes"),
    "# Carrega os pacotes e as funções de apoio.",
    "# Rode antes de qualquer etapa da análise.",
    "",
    "# here() monta os caminhos a partir da raiz do projeto (onde está o .Rproj),",
    "# permitindo que o mesmo arquivo funcione em computadores diferentes.",
    "library(here)",
    "source(here(\"R\", \"funcoes.R\"))",
    "",
    "# Leitura da planilha.",
    "library(readxl)",
    "library(dplyr)",
    "library(tidyr)",
    "library(lubridate)",
    "library(ggplot2)",
    "library(car)",
    "library(multcompView)",
    "library(effectsize)",
    "library(EAPADados)",
    "",
    "# As funções de análise: as mesmas que a CatalyseR usou na tela, para o",
    "# resultado ser idêntico. ?catalyser_anova mostra a ajuda de qualquer uma.",
    "if (!requireNamespace(\"catalyser\", quietly = TRUE)) {",
    "  stop(\"Este projeto usa o pacote catalyser. Para instalar: \",",
    "       \"remotes::install_github('astuciasnor/catalyser')\", call. = FALSE)",
    "}",
    "library(catalyser)",
    "",
    "# As receitas das bases estão nos trechos de preparo deste script."
  )
}

exportacao_nome_planilha <- function(import_info = list()) {
  origem <- if (identical(import_info$source, "package")) {
    import_info$package_dataset
  } else if (!exportacao_origem_texto(import_info) &&
             nzchar(as.character(import_info$excel_sheet %||% ""))) {
    sub("^[0-9]+[ ._-]*", "", as.character(import_info$excel_sheet))
  } else {
    tools::file_path_sans_ext(basename(import_info$file_name %||% ""))
  }
  paste0(exportacao_nome_seguro(origem, "dados_brutos"), ".xlsx")
}

exportacao_aba_planilha <- function(import_info = list()) {
  aba <- if (identical(import_info$source, "package")) {
    import_info$package_dataset
  } else if (exportacao_origem_texto(import_info)) {
    "dados"
  } else {
    import_info$excel_sheet
  }
  aba <- trimws(as.character(aba %||% ""))
  if (!nzchar(aba)) "dados" else substr(aba, 1, 31)
}

# Trecho `importar`: da planilha ao data.frame bruto, sem mexer em nada.
exportacao_trecho_importar <- function(import_info = list()) {
  planilha <- exportacao_nome_planilha(import_info)
  aba <- exportacao_aba_planilha(import_info)
  c(
    exportacao_marcador("importar"),
    "# Lê a planilha como ela veio, sem mexer em nada. Sai dados_brutos.",
    sprintf("# Entrada: dados/brutos/%s, com somente a aba utilizada.", planilha),
    sprintf("# Arquivo de origem: %s.", basename(import_info$file_name %||% import_info$package_dataset %||% planilha)),
    "",
    "# Se quiser rodar o projeto com outra planilha de mesma estrutura, troque",
    "# o caminho e a aba abaixo. A planilha é somente-leitura: nunca a edite.",
    sprintf("caminho_planilha <- here(\"dados\", \"brutos\", \"%s\")", planilha),
    sprintf("aba_planilha <- \"%s\"", aba),
    "",
    "dados_brutos <- as.data.frame(read_excel(caminho_planilha, sheet = aba_planilha))",
    "",
    "# Primeira olhada: quantas linhas e colunas vieram, e o tipo de cada coluna.",
    "# O QUE CONFERIR: números lidos como texto (chr) são o sinal de problema",
    "# mais comum, e vêm de vírgula decimal ou de um traço no lugar do vazio.",
    "str(dados_brutos)",
    ""
  )
}

# Passo 2: operações estruturais (Pivotar/Separar/Organizar).
#
# As promoções atuais guardam a sequência executável, sem repetir a importação.
# Registros antigos sem essa sequência ainda usam a base salva e conservam o
# código original comentado, com essa limitação explícita no projeto.
exportacao_bloco_estrutural <- function(base_externa = NULL, import_info = list()) {
  sequencia <- exportacao_organizacao_anova(base_externa)
  if (!exportacao_anova_usa_base_resolvida(base_externa) &&
      (length(sequencia) || length(import_info$preparo_importacao))) {
    return(c(
      "# Escolhas da importação e reestruturações, na ordem registrada na IDE.",
      exportacao_preparo_importacao(import_info),
      if (length(sequencia)) c("library(dplyr)", "library(tidyr)", sequencia),
      "base_resolvida <- dados", ""
    ))
  }
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
    ""
  )
  ativas <- Filter(function(et) isTRUE(et$ativa), pipeline %||% list())
  if (!length(ativas)) {
    linhas <- c(linhas, "# Nenhum tratamento compartilhado foi registrado.", "")
  }
  for (i in seq_along(ativas)) {
    etapa <- ativas[[i]]
    tratamento <- reg[[etapa$tipo]]
    if (is.null(tratamento)) stop("Há um tratamento sem gerador de código na trilha.", call. = FALSE)
    linhas <- c(
      linhas,
      sprintf("# Etapa %d: %s", i, tratamento$rotulo(etapa$params)),
      gsub("trat_moda(", "catalyser::catalyser_moda(", tratamento$codigo(etapa$params), fixed = TRUE),
      ""
    )
  }
  c(linhas, "dados_analise <- dados", "")
}

# Trecho `tratar`: da planilha bruta à Base Compartilhada (`dados_analise`).
exportacao_trecho_tratar <- function(pipeline, base_externa = NULL,
                                     reg = tratamentos, import_info = list()) {
  c(
    exportacao_marcador("tratar"),
    "# Transforma a planilha na Base Compartilhada, exatamente como aconteceu na",
    "# CatalyseR: primeiro as operações estruturais, depois a Trilha de Preparo, na",
    "# ordem lógica registrada. Sai dados_analise, conferido contra a fotografia da IDE.",
    "",
    exportacao_encadear_preparo(c(
      exportacao_bloco_estrutural(base_externa, import_info),
      exportacao_bloco_trilha(pipeline, reg)), saida = "dados_analise"),
    "# -----------------------------------------------------------------------",
    "# 4. Conferência",
    "# -----------------------------------------------------------------------",
    "# A CatalyseR também exportou uma fotografia de `dados_analise`. A função",
    "# abaixo compara a base reconstruída com ela e avisa se algo divergir.",
    "# O QUE CONFERIR: a mensagem deve dizer que a base é idêntica à fotografia.",
    "catalyser_conferir_base(",
    "  dados_analise,",
    "  here(\"dados\", \"processados\", \"base_compartilhada.rds\"),",
    "  rotulo = \"Base Compartilhada\"",
    ")",
    ""
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
  encodeString(as.character(x %||% ""), quote = '"')
}

# Identificação preenchida em Comunicação de Resultados, comum aos modelos.
exportacao_identificacao_documento <- function(globais = list(), titulo_padrao,
                                                subtitulo_padrao = "") {
  titulo <- trimws(as.character(globais$titulo %||% ""))
  if (!nzchar(titulo)) titulo <- titulo_padrao
  subtitulo <- trimws(as.character(globais$subtitulo %||% subtitulo_padrao))
  autores <- unlist(strsplit(as.character(globais$autores %||% ""), "\r\n|\r|\n"))
  autores <- trimws(autores[nzchar(trimws(autores))])
  author_yaml <- if (length(autores)) c(
    "author:",
    unlist(lapply(autores, function(nome) c(
      paste0("  - name: ", exportacao_yaml_texto(nome)),
      '    affiliation: ""'
    )), use.names = FALSE)
  ) else "author: []"
  list(titulo = titulo, subtitulo = subtitulo, autores_yaml = author_yaml)
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
      "# R/analise.R, os trechos `pacotes`, `importar`, `tratar` e depois o",
      sprintf("# trecho '%s-base', que deixa '%s' na memória.",
              exportacao_raiz_chunk(execucao), execucao$base_objeto),
      sprintf("dados <- %s", execucao$base_objeto)
    )
  } else if (identical(execucao$base_tipo, "compartilhada") ||
             identical(execucao$base_id, "dados_analise")) {
    c(
      "# Antes de rodar este bloco, rode os trechos `pacotes`, `importar` e",
      "# `tratar` de R/analise.R: eles deixam `dados_analise` na memória.",
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
      "# 7. Letras de diferença: grupos com a mesma letra não diferiram.",
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
        "# Este tipo de análise ainda não tem código passo a passo; o trecho",
        "# -resultado, logo abaixo, a refaz pela função da CatalyseR.",
        "resultado <- catalyser_executar(execucao, dados)"
      )
    }
  }
  c(
    if (isTRUE(incluir_cabecalho)) c(
      "# Código R essencial desta execução.",
      "# Este trecho pode ser executado linha a linha no RStudio."
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

# Uma linha de comentário por componente, para o trecho dizer o que mostra.
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

# Nome do trecho (e do chunk) de um componente: `anova-profundidade-m-tukey`.
exportacao_nome_componente <- function(execucao_id, componente,
                                       raiz_chunk = NULL, tipo = NULL) {
  if (is.null(raiz_chunk)) {
    exportacao_slug_chunk(paste(execucao_id, componente), "resultado")
  } else {
    paste0(raiz_chunk, "-", exportacao_sufixo_componente(tipo, componente))
  }
}

# Trecho de um componente: a chamada que mostra a narrativa, uma tabela ou um
# gráfico do resultado. [[ ]] em vez de $: o $ do R completa nomes pela
# metade, e `grafico` poderia virar `grafico_combinacoes` sem ninguém perceber.
exportacao_trecho_componente <- function(variavel, execucao_id, componente,
                                         raiz_chunk = NULL, tipo = NULL) {
  rotulo <- comunicacao_rotulos_saidas[[componente]] %||% componente
  nome <- exportacao_nome_componente(execucao_id, componente, raiz_chunk, tipo)
  expressao <- if (identical(componente, "console")) {
    sprintf(
      "cat('```text\\n', paste(%s[[\"console\"]], collapse = '\\n'), '\\n```\\n')",
      variavel
    )
  } else {
    sprintf("catalyser_mostrar(%s[[\"%s\"]])", variavel, componente)
  }
  c(
    exportacao_marcador(nome),
    exportacao_comentario_componente(componente, rotulo),
    sprintf("# Usa %s, do trecho %s-resultado.", variavel, raiz_chunk %||% "..."),
    expressao,
    ""
  )
}

# O chunk do componente no relatório: um título de terceiro nível e a casca.
# `results: asis` porque catalyser_mostrar() escreve markdown direto.
exportacao_chunk_componente <- function(execucao_id, componente,
                                        raiz_chunk = NULL, tipo = NULL) {
  rotulo <- comunicacao_rotulos_saidas[[componente]] %||% componente
  nome <- exportacao_nome_componente(execucao_id, componente, raiz_chunk, tipo)
  c(
    sprintf("### %s", rotulo),
    "",
    exportacao_casca_chunk(nome, opcoes = "#| results: asis"),
    ""
  )
}

# Trecho de apresentação: a análise refeita pela função da CatalyseR, com os
# parâmetros escritos por extenso. É o que alimenta as tabelas, os gráficos e
# a narrativa do relatório, e não depende de nenhum arquivo de metadados.
exportacao_trecho_resultado <- function(item, raiz, variavel) {
  configuracao <- list(
    tipo = item$tipo,
    titulo = item$titulo,
    parametros = item$parametros %||% list()
  )
  lista <- strsplit(exportacao_lista_r(configuracao, 0L), "\n", fixed = TRUE)[[1]]
  lista[length(lista)] <- paste0(lista[length(lista)], ",")
  c(
    exportacao_marcador(paste0(raiz, "-resultado")),
    "# A mesma análise, agora pela função da CatalyseR, que devolve narrativa,",
    "# tabelas e gráficos já formatados, com os parâmetros abaixo exatamente como",
    "# você escolheu na tela.",
    sprintf("# Sai %s, o objeto que os trechos de apresentação mostram.", variavel),
    sprintf("%s <- catalyser_executar(", variavel),
    paste0("  ", lista),
    "  dados_da_analise",
    ")",
    ""
  )
}

# Tipos cujo código de estudo já foi humanizado e testado de ponta a ponta.
# Só esses rodam de verdade no relatório; os demais ficam como código para
# leitura (eval: false), até ganharem o mesmo tratamento.
exportacao_tipos_com_codigo_vivo <- c("anova_um_fator", "anova_dois_fatores", "grafico_linhas")

exportacao_codigo_vivo <- function(item) {
  as.character(item$tipo %||% "") %in% exportacao_tipos_com_codigo_vivo
}

# Trecho da análise passo a passo: o código que o aluno escreveria no RStudio.
exportacao_trecho_analise <- function(item, raiz) {
  codigo_estudo <- exportacao_codigo_estudo(
    item, incluir_carregamento = FALSE, incluir_cabecalho = FALSE
  )
  c(
    exportacao_marcador(paste0(raiz, "-analise")),
    "# A análise passo a passo, o que você escreveria no RStudio para fazê-la sem",
    "# a CatalyseR. Usa dados_da_analise, do trecho anterior.",
    if (exportacao_codigo_vivo(item)) c(
      "# NO RELATÓRIO: roda de verdade, em silêncio (output: false); as saídas",
      "# formatadas vêm do trecho -resultado. Para ver os objetos crus, rode as",
      "# linhas aqui, uma a uma."
    ) else c(
      "# NO RELATÓRIO: este tipo de análise ainda não tem o código passo a passo",
      "# validado, então o chunk fica só para leitura (eval: false); as saídas",
      "# vêm do trecho -resultado."
    ),
    "dados <- dados_da_analise",
    "",
    codigo_estudo,
    ""
  )
}

# Trecho que constrói a base de uma análise.
#
# A Base Compartilhada NÃO é construída aqui: ela vem do trecho tratar. Este
# cuida apenas do salto de um passo até a base derivada.
exportacao_trecho_base <- function(item, raiz, registro_bases) {
  base <- bases_obter(registro_bases, item$base_id)
  if (identical(item$base_tipo, "derivada") && !is.null(base)) {
    receita <- strsplit(
      bases_codigo(base, incluir_print = FALSE), "\n", fixed = TRUE
    )[[1]]
    receita <- exportacao_encadear_preparo(receita, entrada = "dados_analise", saida = base$nome_r)
    receita <- gsub("trat_moda(", "catalyser::catalyser_moda(", receita, fixed = TRUE)
    return(c(
      exportacao_marcador(paste0(raiz, "-base")),
      "# Constrói a base desta análise num salto só a partir de dados_analise, com",
      "# a receita registrada na CatalyseR. Sai dados_da_analise, lido adiante.",
      receita,
      sprintf("dados_da_analise <- %s", base$nome_r),
      "# Se alterar esta receita, confira a base antes de salvar e renderizar:",
      sprintf('# saveRDS(dados_da_analise, here("dados", "processados", "%s"))', exportacao_rds_base(item)),
      ""
    ))
  }
  c(
    exportacao_marcador(paste0(raiz, "-base")),
    "# Esta análise usa a própria Base Compartilhada, sem preparo adicional.",
    "# Sai dados_da_analise, lido pelos trechos seguintes.",
    "dados_da_analise <- dados_analise",
    ""
  )
}

# Os IDs das derivadas são únicos e estáveis, inclusive se os nomes se parecem.
exportacao_rds_base <- function(item) {
  if (identical(item$base_tipo, "derivada")) {
    paste0(exportacao_nome_seguro(item$base_id), ".rds")
  } else "base_compartilhada.rds"
}

exportacao_trecho_carregar_base <- function(item = NULL, raiz = NULL) {
  if (is.null(item)) return(c(
    exportacao_marcador("carregar-compartilhada"),
    "# O relatório lê a base preparada; a receita completa fica acima, para estudo.",
    "# Depois de alterar o preparo, confira os dados e salve o RDS para adotá-los:",
    '# saveRDS(dados_analise, here("dados", "processados", "base_compartilhada.rds"))',
    'dados_analise <- readRDS(here("dados", "processados", "base_compartilhada.rds"))', ""
  ))
  arquivo <- exportacao_rds_base(item)
  c(exportacao_marcador(paste0(raiz, "-carregar-base")),
    "# O Render começa pela base já preparada, preservando os tipos das colunas.",
    "# Para adotar uma mudança na receita acima, confira os dados e execute:",
    sprintf('# saveRDS(dados_da_analise, here("dados", "processados", "%s"))', arquivo),
    sprintf('dados_da_analise <- readRDS(here("dados", "processados", "%s"))', arquivo), "")
}

# ---- O script: R/analise.R ---------------------------------------------------
# Cabeçalho na voz do EAPACaderno, depois os trechos na ordem do relatório.

exportacao_cabecalho_script <- function(nome_projeto) {
  c(
    sprintf("# %s — a análise, comentada ----", nome_projeto),
    "#",
    "# Este script é o código do relatório (relatorios/relatorio.qmd) com as",
    "# explicações que o relatório não mostra. Aqui se aprende; lá se apresenta.",
    "# Foi gerado pela CatalyseR a partir do que você fez na tela: cada análise",
    "# aparece em código R, passo a passo, para você ler, rodar e adaptar.",
    "#",
    "# COMO RODAR",
    "# Reinicie o R (Session > Restart R) e execute as linhas em ordem, com",
    "# Ctrl+Enter; o resultado aparece no Console, no Plots ou no Viewer.",
    "# Ctrl+Shift+O abre o menu de seções e lista todos os trechos.",
    "#",
    "# SCRIPT E RELATÓRIO",
    "# Cada trecho começa com um marcador \"## ---- nome ----\", e no relatório a",
    "# primeira linha de cada chunk diz de quais trechos ele vem (\"# fonte: ...\").",
    "# O código se edita aqui, nunca no relatório; depois rode o chunk \"atualizar\"",
    "# do relatório, que copia o código sem os comentários. Se algo ficar diferente,",
    "# o Render para e avisa qual chunk está defasado.",
    "#",
    "# PEQUENO VOCABULÁRIO",
    "# <- guarda um resultado em um objeto; |> passa o resultado à próxima função.",
    "# mutate()/summarise() criam colunas; filter() escolhe linhas; select() colunas.",
    "# ~ escreve uma relação em modelos; $ e [[ ]] acessam um componente nomeado.",
    "# NA indica ausência de dado, não zero.",
    "#",
    "# O CAMINHO DOS DADOS",
    "# importar -> tratar (a Base Compartilhada, dados_analise) -> por análise:",
    "# leitura do RDS, análise passo a passo e apresentação dos resultados.",
    "# Na ANOVA, modelo, pressupostos e Tukey ficam em trechos separados.",
    "#",
    "# SEGURANÇA E REPRODUÇÃO",
    "# O trecho instalar só instala o que falta; dar Source no script inteiro",
    "# instala pacotes ausentes, o que é aceitável, mas leva tempo e pede internet.",
    "# A planilha bruta nunca é alterada; a conferência do trecho tratar compara",
    "# a base reconstruída com a fotografia exportada pela CatalyseR.",
    "# Não use Source como substituto de Render: o script não produz HTML/DOCX.",
    "#",
    "",
    ""
  )
}

# ---- Caminho dedicado: ANOVA de um fator (modelo EAPACaderno, CRAN puro) -----
# Quando a exportacao e uma unica ANOVA de um fator, o script e o relatorio saem
# de arquivos-modelo em templates/anova_um_fator/, com os marcadores trocados
# pelos nomes reais (planilha, aba, resposta, fator). O restante do fluxo (copiar
# templates e sincronizar os chunks) é compartilhado. O código comentado do
# script alimenta o relatório; a ANOVA isolada dispensa a pasta metadados.
exportacao_execucoes_incluidas <- function(manifesto) {
  Filter(function(x) isTRUE(x$incluir_word), manifesto$execucoes %||% list())
}

# Cliques repetidos em versões anteriores podem ter guardado a mesma análise.
# Só reúne cópias exatas: outras bases, parâmetros ou escolhas editoriais ficam.
exportacao_sem_execucoes_repetidas <- function(manifesto) {
  campos <- c("tipo", "titulo", "parametros", "base_id", "base_tipo", "base_objeto",
    "base_versao_receita", "revisao_origem", "codigo_r", "incluir_word", "saidas_word",
    "estado_dependencia")
  vistos <- list()
  manter <- vapply(manifesto$execucoes %||% list(), function(item) {
    chave <- item[campos]
    if (any(vapply(vistos, identical, logical(1), chave))) return(FALSE)
    vistos[[length(vistos) + 1L]] <<- chave
    TRUE
  }, logical(1))
  manifesto$execucoes <- manifesto$execucoes[manter]
  manifesto$total_execucoes <- length(manifesto$execucoes)
  manifesto$total_word <- length(exportacao_execucoes_incluidas(manifesto))
  manifesto
}

exportacao_anova_simples <- function(manifesto) {
  inc <- exportacao_execucoes_incluidas(manifesto)
  # Outras execuções desmarcadas continuam como código de estudo no script.
  length(manifesto$execucoes) == 1L && length(inc) == 1L &&
    identical(as.character(inc[[1]]$tipo %||% ""), "anova_um_fator")
}

# A função de datas mora no pacote CatalyseR. Retira somente sua
# definição dos blocos registrados, preservando chamadas e demais operações.
# Usa as linhas da expressão R, inclusive em receitas salvas antes deste ajuste.
exportacao_preparo_sem_funcao_data <- function(codigo) {
  linhas <- strsplit(paste(codigo, collapse = "\n"), "\n", fixed = TRUE)[[1]]
  expressoes <- parse(text = linhas, keep.source = TRUE)
  referencias <- attr(expressoes, "srcref")
  remover <- integer()
  for (i in seq_along(expressoes)) {
    x <- expressoes[[i]]
    if (is.call(x) && identical(x[[1]], as.name("<-")) &&
        identical(x[[2]], as.name("converter_data")) &&
        is.call(x[[3]]) && identical(x[[3]][[1]], as.name("function"))) {
      inicio <- referencias[[i]][1]
      # Leva junto apenas o cabeçalho explicativo imediatamente anterior.
      while (inicio > 1L && grepl("^\\s*(#|$)", linhas[inicio - 1L]) &&
             !grepl("^## ---- ", linhas[inicio - 1L])) inicio <- inicio - 1L
      remover <- c(remover, seq.int(inicio, referencias[[i]][3]))
    }
  }
  if (length(remover)) linhas <- linhas[-unique(remover)]
  # Atualiza também receitas guardadas quando a função viajava no script.
  gsub("\\bconverter_data\\s*\\(", "catalyser::converter_datas(", linhas)
}

# As edições de variáveis já trazem código que parte da base corrente.
# Cada confirmação deve alimentar a próxima, em vez de apenas imprimir a prévia.
exportacao_organizacao_anova <- function(base_externa) {
  if (!is.null(base_externa$codigo_sequencial)) return(base_externa$codigo_sequencial)
  codigo <- trimws(as.character(base_externa$codigo %||% ""))
  if (!nzchar(codigo)) return(character())
  expressoes <- tryCatch(parse(text = codigo), error = function(e) NULL)
  if (!length(expressoes)) return(NULL)
  organizacao <- vapply(expressoes, function(x) {
    is.call(x) && (
      (identical(x[[1]], as.name("<-")) && identical(x[[2]], as.name("dados_organizados"))) ||
      identical(x, quote(print(dados_organizados)))
    )
  }, logical(1))
  if (!all(organizacao) ||
      !identical(expressoes[[length(expressoes)]], quote(print(dados_organizados)))) return(NULL)
  linhas <- strsplit(codigo, "\n", fixed = TRUE)[[1]]
  linhas[trimws(linhas) == "print(dados_organizados)"] <- "dados <- dados_organizados"
  linhas
}

exportacao_anova_usa_base_resolvida <- function(base_externa) {
  isTRUE(base_externa$usar_snapshot_anova) ||
    is.null(exportacao_organizacao_anova(base_externa))
}

# A mesma receita de importação alimenta a ANOVA e o exportador geral.
exportacao_preparo_importacao <- function(import_info = list()) {
  texto_r <- function(x) encodeString(as.character(x), quote = '"')
  coluna_r <- function(x) if (identical(make.names(x), x)) x else encodeString(x, quote = "`")
  vetor_r <- function(x) paste0("c(", paste(texto_r(x), collapse = ", "), ")")
  linhas <- "dados <- dados_brutos"
  preparo <- import_info$preparo_importacao %||% list()
  if ("Date" %in% unlist(preparo$tipos)) linhas <- c(preparo_codigo_data(), "", linhas)
  colunas <- preparo$colunas %||% character()
  if (length(colunas) && !identical(colunas, preparo$colunas_originais)) linhas <- c(linhas,
    "# Colunas escolhidas na importação.",
    sprintf("dados <- dados |> dplyr::select(dplyr::all_of(%s))", vetor_r(colunas)))
  for (nome in names(preparo$recodificacoes)) {
    if (length(colunas) && !nome %in% colunas) next
    mapa <- unlist(preparo$recodificacoes[[nome]])
    if (!length(mapa)) next
    pares <- paste(paste0(texto_r(names(mapa)), " = ", texto_r(unname(mapa))), collapse = ", ")
    col <- coluna_r(nome)
    linhas <- c(linhas, paste("# Recodificação de", nome),
      sprintf("dados <- dados |> dplyr::mutate(%s = dplyr::recode(as.character(%s), %s))", col, col, pares))
  }
  for (nome in names(preparo$tipos)) {
    if (length(colunas) && !nome %in% colunas) next
    # Evita reconverter colunas que já chegaram no tipo solicitado.
    original <- preparo$classes_originais[[nome]]
    if (identical(original, preparo$tipos[[nome]]) &&
        !length(preparo$recodificacoes[[nome]])) next
    col <- coluna_r(nome)
    expr <- switch(preparo$tipos[[nome]],
      numeric = sprintf('as.numeric(gsub(",", ".", as.character(%s), fixed = TRUE))', col),
      integer = sprintf('as.integer(gsub(",", ".", as.character(%s), fixed = TRUE))', col),
      factor = sprintf("factor(%s)", col), character = sprintf("as.character(%s)", col),
      logical = sprintf("as.logical(%s)", col), Date = sprintf("catalyser::converter_datas(%s)", col), NULL)
    if (!is.null(expr)) linhas <- c(linhas, sprintf("dados <- dados |> dplyr::mutate(%s = %s)", col, expr))
  }
  for (nome in names(preparo$filtros_niveis)) {
    manter <- preparo$filtros_niveis[[nome]]
    if (length(colunas) && !nome %in% colunas) next
    if (!length(manter) || !identical(preparo$tipos[[nome]], "factor")) next
    linhas <- c(linhas, paste("# Níveis selecionados de", nome),
      sprintf("dados <- dados |> dplyr::filter(%s %%in%% %s)", coluna_r(nome), vetor_r(manter)))
  }
  for (nome in names(preparo$filtros_faixas)) {
    faixa <- preparo$filtros_faixas[[nome]]
    if (length(colunas) && !nome %in% colunas) next
    if (length(faixa) != 2L || !preparo$tipos[[nome]] %in% c("numeric", "integer")) next
    col <- coluna_r(nome)
    linhas <- c(linhas, paste("# Faixa selecionada de", nome),
      sprintf("dados <- dados |> dplyr::filter(%s >= %s, %s <= %s)",
              col, format(faixa[1], digits = 17, decimal.mark = "."),
              col, format(faixa[2], digits = 17, decimal.mark = ".")))
  }
  for (nome in names(preparo$renomes)) {
    if (length(colunas) && !nome %in% colunas) next
    novo <- unname(preparo$renomes[[nome]])
    if (!identical(nome, novo)) linhas <- c(linhas,
      sprintf("dados <- dados |> dplyr::rename(%s = %s)", coluna_r(novo), coluna_r(nome)))
  }
  linhas
}

# Enxuga somente sequências de transformações conhecidas, sem alterar a receita.
# Operações com lógica própria (if, for, funções, contingência) ficam como vieram.
exportacao_encadear_preparo <- function(linhas, entrada = "dados_brutos", saida = "dados") {
  expressoes <- tryCatch(parse(text = linhas), error = function(e) NULL)
  if (!length(expressoes)) return(linhas)
  aliases <- unique(c(entrada, saida, "dados", "dados_brutos", "dados_organizados", "dados_arrumados", "base_resolvida",
               "dados_analise", "base_compartilhada"))
  pacotes <- character()
  funcoes_dplyr <- c("select", "rename", "mutate", "filter", "arrange", "distinct")
  funcoes_tidyr <- c("drop_na", "pivot_longer", "pivot_wider", "separate_wider_delim", "extract")
  receitas <- list()
  receitas[[entrada]] <- list(origem = entrada, passos = list())
  reconhecer <- function(x) {
    if (is.symbol(x)) return(receitas[[as.character(x)]])
    if (!is.call(x) || length(x) < 2L) return(NULL)
    funcao <- paste(deparse(x[[1]]), collapse = "")
    permitidas <- c(paste0("dplyr::", funcoes_dplyr), paste0("tidyr::", funcoes_tidyr),
      funcoes_dplyr, funcoes_tidyr, "as.data.frame")
    if (!funcao %in% permitidas) return(NULL)
    anterior <- reconhecer(x[[2]])
    if (is.null(anterior)) return(NULL)
    argumentos <- as.list(x)[-(1:2)]
    if (any(vapply(argumentos, function(a) any(all.names(a) %in% aliases), logical(1)))) return(NULL)
    passo <- as.call(c(list(x[[1]]), argumentos))
    if (funcao %in% c(funcoes_dplyr, funcoes_tidyr)) {
      pacote <- if (funcao %in% funcoes_dplyr) "dplyr" else "tidyr"
      passo[[1]] <- as.call(list(as.name("::"), as.name(pacote), as.name(funcao)))
    }
    anterior$passos <- c(anterior$passos, list(passo))
    anterior
  }
  for (x in expressoes) {
    # A seleção de medidas do empilhamento é um vetor literal, independente
    # dos dados. Conserva essa configuração antes da cadeia, sem movê-la da IDE.
    if (is.call(x) && identical(x[[1]], as.name("<-")) &&
        identical(x[[2]], as.name("cols_medida")) && is.call(x[[3]]) &&
        identical(x[[3]][[1]], as.name("c")) &&
        all(vapply(as.list(x[[3]])[-1L], is.character, logical(1)))) {
      if (any(grepl("^cols_medida <-", pacotes))) return(linhas)
      pacotes <- c(pacotes, deparse(x))
      next
    }
    if (is.call(x) && identical(x[[1]], as.name("library")) && length(x) == 2L &&
        as.character(x[[2]]) %in% c("dplyr", "tidyr")) {
      pacotes <- unique(c(pacotes, deparse(x)))
      next
    }
    if (!is.call(x) || !identical(x[[1]], as.name("<-")) ||
        !is.symbol(x[[2]]) || !as.character(x[[2]]) %in% aliases) return(linhas)
    receita <- reconhecer(x[[3]])
    if (is.null(receita)) return(linhas)
    receitas[[as.character(x[[2]])]] <- receita
  }
  receita <- receitas[[saida]]
  if (is.null(receita)) return(linhas)
  descricoes <- grep("^# (Etapa |Manter |Renomear |Definir |Colunas |Recodifica|Níveis |Faixa )", linhas, value = TRUE)
  if (any(grepl("^# Etapa ", descricoes))) descricoes <- grep("^# Etapa ", descricoes, value = TRUE)
  comentar <- function(x) {
    funcao <- sub("^.*::", "", paste(deparse(x[[1]]), collapse = ""))
    args <- as.list(x)[-1L]
    nomes <- names(args)
    colunas <- paste(nomes[nzchar(nomes)], collapse = ", ")
    switch(funcao,
      select = "Mantém as colunas indicadas, na ordem escolhida.",
      rename = "Renomeia as colunas: nome novo = nome anterior.",
      mutate = {
        fator <- length(args) == 1L && is.call(args[[1]]) &&
          as.character(args[[1]][[1]])[1] %in% c("as.factor", "factor")
        if (fator && nzchar(colunas)) paste0("Define ", colunas, " como fator (categorias).")
        else if (nzchar(colunas)) paste0("Calcula ou transforma: ", colunas, ".")
        else "Aplica a transformação às colunas selecionadas."
      },
      filter = "Mantém somente as linhas que atendem à condição.",
      arrange = "Ordena as linhas pelas variáveis indicadas.",
      distinct = "Remove repetições conforme as colunas indicadas.",
      drop_na = "Remove linhas com valores ausentes nas colunas indicadas.",
      pivot_longer = "Empilha as colunas de medidas em linhas.",
      pivot_wider = "Distribui os valores em novas colunas.",
      separate_wider_delim = "Separa a coluna usando o delimitador escolhido.",
      extract = "Extrai as partes do texto para as colunas indicadas.",
      as.data.frame = "Mantém a base no formato data.frame.")
  }
  passos <- unlist(lapply(seq_along(receita$passos), function(i) {
    x <- receita$passos[[i]]
    codigo <- deparse(x, width.cutoff = 90L)
    if (i < length(receita$passos)) codigo[length(codigo)] <- paste0(tail(codigo, 1L), " |>")
    c(paste0("  # ", comentar(x)), paste0("  ", codigo))
  }), use.names = FALSE)
  c(pacotes, descricoes, if (length(descricoes)) "",
    paste0("# Parte de ", receita$origem, " e guarda o resultado em ", saida, "."),
    if (length(passos)) c(paste0(saida, " <- ", receita$origem, " |>"), passos)
    else paste0(saida, " <- ", receita$origem), "")
}

# Preparo em R comum: as escolhas da importação, a trilha e o ramo da análise.
exportacao_preparo_anova <- function(manifesto, import_info = list(), pipeline = list(),
                                      registro_bases = list(), base_externa = NULL) {
  linhas <- exportacao_preparo_importacao(import_info)
  etapas_codigo <- function(etapas) {
    codigo <- character()
    for (etapa in etapas %||% list()) {
      if (!isTRUE(etapa$ativa)) next
      tratamento <- tratamentos[[etapa$tipo]]
      if (is.null(tratamento)) stop("Há um tratamento sem gerador de código na trilha.", call. = FALSE)
      codigo <- c(codigo, paste0("# ", tratamento$rotulo(etapa$params)), tratamento$codigo(etapa$params))
    }
    codigo
  }
  if (exportacao_anova_usa_base_resolvida(base_externa)) {
    # A fotografia é anterior à trilha: os tratamentos abaixo rodam uma só vez.
    linhas <- c(
      "# Partimos da base preparada e salva pela CatalyseR antes dos tratamentos.",
      "# Para refazer essas operações a partir do Excel, adapte o registro abaixo.",
      "dados <- base_resolvida",
      "# Registro das operações anteriores, mantido como referência:",
      paste0("# > ", strsplit(base_externa$codigo %||% "", "\n", fixed = TRUE)[[1]])
    )
  } else {
    sequencia <- exportacao_organizacao_anova(base_externa)
    linhas <- c(linhas, if (length(sequencia)) c("library(dplyr)", "library(tidyr)", sequencia))
  }
  linhas <- exportacao_encadear_preparo(
    c(linhas, etapas_codigo(pipeline), "base_compartilhada <- dados"),
    saida = "base_compartilhada")
  linhas <- c(linhas, "dados <- base_compartilhada")
  item <- exportacao_execucoes_incluidas(manifesto)[[1]]
  if (identical(item$base_tipo, "derivada")) {
    base <- Filter(function(x) identical(x$id, item$base_id), registro_bases)
    if (length(base) != 1L) stop("A base derivada desta ANOVA não foi encontrada.", call. = FALSE)
    ramo <- c("dados <- base_compartilhada", etapas_codigo(base[[1]]$etapas))
    linhas <- c(linhas, "# Preparo específico da base escolhida para a ANOVA.",
      exportacao_encadear_preparo(ramo, entrada = "base_compartilhada", saida = "dados"))
  }
  gsub("trat_moda(", "catalyser::catalyser_moda(", linhas, fixed = TRUE)
}

# Confere ainda na IDE, antes do ZIP, sem acrescentar manutenção ao projeto.
exportacao_conferir_preparo_anova <- function(codigo, dados_brutos, dados_analise,
                                               manifesto, cache_bases, base_resolvida = NULL) {
  ambiente <- new.env(parent = asNamespace("stats"))
  ambiente$dados_brutos <- as.data.frame(dados_brutos)
  ambiente$base_resolvida <- as.data.frame(base_resolvida)
  tryCatch(eval(parse(text = codigo), envir = ambiente), error = function(e) {
    stop(paste("O preparo não pôde ser reproduzido em R:", conditionMessage(e)), call. = FALSE)
  })
  iguais <- function(a, b) {
    a <- as.data.frame(a); b <- as.data.frame(b)
    if (!identical(names(a), names(b)) || nrow(a) != nrow(b)) return(FALSE)
    all(vapply(names(a), function(nome) {
      x <- a[[nome]]; y <- b[[nome]]
      if (is.factor(x)) x <- as.character(x)
      if (is.factor(y)) y <- as.character(y)
      isTRUE(all.equal(x, y, check.attributes = FALSE, tolerance = 1e-8))
    }, logical(1)))
  }
  if (!iguais(ambiente$base_compartilhada, dados_analise)) {
    stop(paste("O preparo exportado não reproduziu a base compartilhada da IDE.",
               "Confira os filtros e a tipagem; a exportação foi interrompida para não gerar resultados diferentes."), call. = FALSE)
  }
  item <- exportacao_execucoes_incluidas(manifesto)[[1]]
  if (identical(item$base_tipo, "derivada")) {
    esperada <- cache_bases[[item$base_id]]$df
    if (is.null(esperada) || !iguais(ambiente$dados, esperada)) {
      stop("O preparo exportado não reproduziu a base derivada desta análise. Atualize a base e execute a análise novamente.", call. = FALSE)
    }
  }
  invisible(TRUE)
}

exportacao_modelo_anova <- function(arquivo, manifesto, import_info,
                                    templates_dir = "templates", pipeline = list(),
                                    registro_bases = list(), base_externa = NULL) {
  item     <- exportacao_execucoes_incluidas(manifesto)[[1]]
  resposta <- as.character(item$parametros$resposta %||% "resposta")
  fator    <- as.character(item$parametros$fator %||% "grupo")
  identificacao <- exportacao_identificacao_documento(
    manifesto$secoes_globais %||% list(),
    as.character(item$titulo %||% "Analise de variancia"), "ANOVA de um fator"
  )
  titulo <- identificacao$titulo
  # Os rótulos mudam a apresentação; os nomes das colunas continuam no código.
  rotulo_x <- as.character(item$parametros$rotulo_x %||% "")
  rotulo_y <- as.character(item$parametros$rotulo_y %||% "")
  if (!nzchar(trimws(rotulo_x))) rotulo_x <- fator
  if (!nzchar(trimws(rotulo_y))) rotulo_y <- resposta
  planilha <- exportacao_nome_planilha(import_info)
  aba      <- exportacao_aba_planilha(import_info)
  confianca <- as.numeric(item$parametros$nivel_confianca %||% 0.95)
  if (length(confianca) != 1L || !is.finite(confianca) || confianca <= 0 || confianca >= 1)
    stop("O nível de confiança deve estar entre zero e um.", call. = FALSE)
  nome_r <- function(x) if (identical(make.names(x), x)) x else encodeString(x, quote = "`")
  titulo_grafico <- as.character(item$parametros$titulo_grafico %||% "")
  codigo_preparo <- exportacao_preparo_anova(manifesto, import_info, pipeline, registro_bases, base_externa)
  codigo_preparo <- exportacao_preparo_sem_funcao_data(codigo_preparo)
  usa_base_salva <- exportacao_anova_usa_base_resolvida(base_externa)
  if (usa_base_salva) codigo_preparo <- c(
    'base_resolvida <- as.data.frame(readRDS(here("dados", "processados", "base_resolvida.rds")))',
    codigo_preparo
  )

  linhas <- readLines(file.path(templates_dir, "anova_um_fator", arquivo),
                       encoding = "UTF-8", warn = FALSE)
  troca <- c("{{RESPOSTA}}" = resposta, "{{FATOR}}" = fator,
             "{{BASE_RDS}}" = encodeString(exportacao_rds_base(item), quote = '"'),
             "{{PLANILHA}}" = planilha, "{{ABA}}" = aba, "{{TITULO}}" = titulo,
             "{{ARQUIVO_ORIGEM}}" = basename(import_info$file_name %||% import_info$package_dataset %||% planilha),
             "{{TITULO_YAML}}" = exportacao_yaml_texto(titulo),
             "{{SUBTITULO_YAML}}" = exportacao_yaml_texto(identificacao$subtitulo),
             "{{AUTORES_YAML}}" = paste(identificacao$autores_yaml, collapse = "\n"),
             "{{DESCRICAO_PREPARO}}" = if (usa_base_salva)
               paste("O preparo utiliza a base reorganizada e salva na CatalyseR, seguida dos tratamentos registrados.",
                     "A planilha original é preservada. As operações que produziram a base salva ficam documentadas no código.") else
               paste("O preparo reproduz as escolhas de importação, as edições de variáveis e os tratamentos registrados.",
                     "A planilha de entrada é preservada."),
             "{{ROTULO_X_R}}" = encodeString(rotulo_x, quote = '"'),
             "{{ROTULO_Y_R}}" = encodeString(rotulo_y, quote = '"'),
             "{{FATOR_R}}" = nome_r(fator), "{{RESPOSTA_R}}" = nome_r(resposta),
             "{{FATOR_STRING}}" = encodeString(fator, quote = '"'),
             "{{RESPOSTA_STRING}}" = encodeString(resposta, quote = '"'),
             "{{PLANILHA_R}}" = encodeString(planilha, quote = '"'),
             "{{ABA_R}}" = encodeString(aba, quote = '"'),
             "{{NIVEL_CONFIANCA}}" = format(confianca, digits = 15, decimal.mark = "."),
             "{{IC_PERCENTUAL}}" = format(100 * confianca, trim = TRUE, decimal.mark = ","),
             "{{TITULO_GRAFICO_R}}" = if (nzchar(trimws(titulo_grafico))) encodeString(titulo_grafico, quote = '"') else "NULL")
  for (marcador in names(troca)) {
    linhas <- gsub(marcador, troca[[marcador]], linhas, fixed = TRUE)
  }
  if (identical(arquivo, "analise.R")) {
    instalar <- which(linhas == "{{INSTALAR}}")
    if (length(instalar) != 1L) stop("Confira o marcador de instalação no modelo ANOVA.", call. = FALSE)
    linhas <- c(head(linhas, instalar - 1L), exportacao_trecho_instalar(),
                tail(linhas, length(linhas) - instalar))
    posicao <- which(linhas == "{{PREPARO}}")
    if (length(posicao) != 1L) stop("Confira o marcador de preparo no modelo ANOVA.", call. = FALSE)
    linhas <- c(head(linhas, posicao - 1L), codigo_preparo, tail(linhas, length(linhas) - posicao))
  }
  if (identical(arquivo, "analise.R") &&
      (!identical(make.names(fator), fator) || !identical(make.names(resposta), resposta))) {
    posicao <- grep("^tukey <- TukeyHSD", linhas)
    codigo_tukey <- c(
      "# Nomes auxiliares permitem usar cabeçalhos de Excel com espaços no Tukey e nas letras.",
      sprintf("dados_tukey <- data.frame(resposta = dados$%s, grupo = dados$%s)", nome_r(resposta), nome_r(fator)),
      "modelo_tukey <- aov(resposta ~ grupo, data = dados_tukey)",
      "tukey <- TukeyHSD(modelo_tukey, conf.level = nivel_confianca)"
    )
    linhas <- c(head(linhas, posicao - 1L), codigo_tukey, tail(linhas, length(linhas) - posicao))
    posicao_letras <- grep("^letras <- multcompView::multcompLetters4", linhas)
    linhas[posicao_letras] <- "letras <- multcompView::multcompLetters4(modelo_tukey, tukey)$grupo$Letters"
  }
  if (identical(arquivo, "relatorio.qmd")) {
    # Os campos preenchidos substituem a orientação inicial de cada seção.
    # Métodos gerais complementam a descrição estatística, que fica no modelo.
    globais <- manifesto$secoes_globais %||% list()
    for (campo in c("introducao", "metodos", "discussao", "conclusao")) {
      inicio <- which(linhas == paste0("<!-- inicio-", campo, " -->"))
      fim <- which(linhas == paste0("<!-- fim-", campo, " -->"))
      if (length(inicio) != 1L || length(fim) != 1L || fim <= inicio) {
        stop(sprintf("Confira os delimitadores da seção '%s' no modelo ANOVA.", campo), call. = FALSE)
      }
      texto <- paste(as.character(globais[[campo]] %||% ""), collapse = "\n")
      conteudo <- if (nzchar(trimws(texto))) {
        strsplit(gsub("\r\n?", "\n", texto), "\n", fixed = TRUE)[[1]]
      } else if (fim > inicio + 1L) {
        linhas[seq.int(inicio + 1L, fim - 1L)]
      } else character()
      linhas <- c(head(linhas, inicio - 1L), conteudo, tail(linhas, length(linhas) - fim))
    }
  }
  linhas
}

# A ANOVA usa os mesmos trechos didáticos tanto sozinha quanto acompanhada.
# Apenas os nomes dos chunks mudam, para distinguir as execuções no documento.
exportacao_anova_trechos <- function(item, raiz, import_info, templates_dir) {
  item$incluir_word <- TRUE
  # Aqui só precisamos dos trechos posteriores à leitura da base; a receita
  # real é gerada uma vez no preparo do projeto e no ramo de cada análise.
  modelo_item <- item
  modelo_item$base_tipo <- "compartilhada"
  manifesto <- list(execucoes = list(modelo_item))
  script <- exportacao_modelo_anova("analise.R", manifesto, import_info, templates_dir)
  qmd <- exportacao_modelo_anova("relatorio.qmd", manifesto, import_info, templates_dir)
  script <- script[seq.int(match("## ---- carregar-bases ----", script),
    match("## ---- fim-do-codigo ----", script) - 1L)]
  script <- sub('"base_compartilhada.rds"',
    encodeString(exportacao_rds_base(item), quote = '"'), script, fixed = TRUE)
  script <- append(script, sprintf("nivel_confianca <- %s",
    format(item$parametros$nivel_confianca %||% .95, digits = 15, decimal.mark = ".")), after = 1L)
  script <- c(script, "## ---- tbl-comparacoes ----",
    "# Diferenças entre pares, intervalos de confiança e p-valores ajustados.",
    "tabela_tukey <- as.data.frame(tukey[[1]])",
    "tabela_tukey$Comparacao <- rownames(tabela_tukey)",
    "tabela_tukey |> dplyr::select(Comparacao, dplyr::everything()) |> flextable_ocean()", "")
  nome <- function(x) {
    if (grepl("^(tbl|fig)-", x)) sub("^(tbl|fig)-", paste0("\\1-", raiz, "-"), x)
    else paste0(raiz, "-", x)
  }
  marcas <- grep("^## ---- .* ----$", script)
  originais <- sub("^## ---- (.*) ----$", "\\1", script[marcas])
  script[marcas] <- paste0("## ---- ", vapply(originais, nome, character(1)), " ----")
  list(script = script, qmd = qmd, nome = nome)
}

exportacao_qmd_anova_acompanhada <- function(item, raiz, import_info, templates_dir) {
  modelo <- exportacao_anova_trechos(item, raiz, import_info, templates_dir)
  nome <- modelo$nome
  qmd <- modelo$qmd
  chunk <- function(rotulo, fonte = rotulo, opcoes = NULL) {
    exportacao_casca_chunk(nome(rotulo), fontes = nome(fonte), opcoes = opcoes)
  }
  apresentacao <- function(rotulo) {
    pos <- match(paste0("#| label: ", rotulo), qmd)
    fim <- pos + match("```", qmd[seq.int(pos + 1L, length(qmd))])
    opcoes <- grep("^#\\| (fig-|tbl-)", qmd[seq.int(pos, fim)], value = TRUE)
    chunk(rotulo, opcoes = opcoes)
  }
  linhas <- c(paste0("## ", item$titulo), "",
    sprintf("Base utilizada: `%s`.", item$base_objeto), "",
    chunk("carregar-bases", opcoes = "#| output: false"), "",
    chunk("preparo", "preparar-analise", "#| output: false"), "",
    chunk("analise-modelo", "analisar", "#| output: false"), "",
    chunk("analise-pressupostos", "analisar-pressupostos", "#| output: false"), "",
    chunk("analise-tukey", "analisar-tukey", "#| output: false"), "",
    chunk("analise-texto", "preparar-resultados-texto", "#| output: false"), "")
  for (componente in item$saidas_word) {
    trecho <- switch(componente,
      narrativa = {
        inicio <- match("`r frase_anova`", qmd)
        fim <- grep("^A @tbl-anova", qmd)[1] - 1L
        c("### Análise de variância", "", qmd[seq.int(inicio, fim)])
      },
      descritivos = c("### Resumo por grupo", "", apresentacao("tbl-resumo")),
      tabela = apresentacao("tbl-anova"),
      comparacoes = c("### Comparações de Tukey", "", chunk("tbl-comparacoes",
        opcoes = '#| tbl-cap: "Comparações entre pares pelo teste de Tukey."')),
      grafico = apresentacao("fig-barras"),
      pressupostos = {
        inicio <- match("## Testes dos pressupostos {.unnumbered}", qmd)
        fim <- match("## Diagnóstico do modelo {.unnumbered}", qmd) - 1L
        c('::::: {.content-visible when-format="html"}',
          sub("^## ", "### ", qmd[seq.int(inicio, fim)]), ":::::")
      },
      diagnosticos = {
        inicio <- match("## Diagnóstico do modelo {.unnumbered}", qmd)
        fim <- inicio + match(":::::", qmd[seq.int(inicio + 1L, length(qmd))]) - 1L
        trecho <- qmd[seq.int(inicio, fim)]
        for (rotulo in c("diagnostico-variancia", "diagnostico-normalidade")) {
          trecho <- sub(paste0("#| label: ", rotulo), paste0("#| label: ", nome(rotulo)), trecho, fixed = TRUE)
          trecho <- sub(paste0("# fonte: ", rotulo), paste0("# fonte: ", nome(rotulo)), trecho, fixed = TRUE)
        }
        c('::::: {.content-visible when-format="html"}', sub("^## ", "### ", trecho), ":::::")
      }, character())
    linhas <- c(linhas, trecho, "")
  }
  linhas
}

# A reta simples tem um roteiro explícito, como a ANOVA, também quando faz
# parte de um relatório com várias análises. Retas por grupo conservam sua rota.
exportacao_regressao_simples <- function(item) {
  identical(item$tipo, "regressao_linear") &&
    !(isTRUE(item$parametros$regressao_por_grupo) &&
      !identical(item$parametros$grupo %||% "none", "none"))
}

exportacao_regressao_trechos <- function(item, raiz, templates_dir = "templates") {
  p <- item$parametros
  linhas <- readLines(file.path(templates_dir, "regressao_linear", "analise.R"),
    encoding = "UTF-8", warn = FALSE)
  formula <- paste(deparse(call("~", as.name(p$resposta), as.name(p$preditor))), collapse = " ")
  # Os rótulos mudam a apresentação; os nomes das colunas seguem no código.
  rotulo_resposta <- as.character(p$rotulo_resposta %||% "")
  rotulo_preditor <- as.character(p$rotulo_preditor %||% "")
  if (!nzchar(trimws(rotulo_resposta))) rotulo_resposta <- p$resposta
  if (!nzchar(trimws(rotulo_preditor))) rotulo_preditor <- p$preditor
  trocas <- list(RESPOSTA = encodeString(p$resposta, quote = '"'),
    PREDITOR = encodeString(p$preditor, quote = '"'), FORMULA = formula,
    CONFIANCA = format(p$nivel_confianca %||% .95, digits = 15, decimal.mark = "."),
    EQUACAO = if (isFALSE(p$mostrar_equacao)) "FALSE" else "TRUE",
    TEMA = encodeString(p$tema %||% "minimal", quote = '"'),
    AUTOCORRELACAO = if (isTRUE(p$avaliar_autocorrelacao)) "TRUE" else "FALSE",
    ROTULO_RESPOSTA_R = encodeString(rotulo_resposta, quote = '"'),
    ROTULO_PREDITOR_R = encodeString(rotulo_preditor, quote = '"'),
    TITULO_R = encodeString(as.character(p$titulo_personalizado %||% ""), quote = '"'))
  for (chave in names(trocas)) {
    linhas <- gsub(paste0("{{", chave, "}}"), trocas[[chave]], linhas, fixed = TRUE)
  }
  marcas <- grep("^## ---- .* ----$", linhas)
  nomes <- sub("^## ---- (.*) ----$", "\\1", linhas[marcas])
  linhas[marcas] <- paste0("## ---- ", raiz, "-", nomes, " ----")
  linhas
}

exportacao_qmd_regressao <- function(item, raiz) {
  chunk <- function(nome, opcoes = "#| output: false", prefixo = "") {
    exportacao_casca_chunk(paste0(prefixo, raiz, "-", nome),
      fontes = paste0(raiz, "-", nome), opcoes = opcoes)
  }
  linhas <- c(paste0("## ", item$titulo), "",
    sprintf("Base utilizada: `%s`.", item$base_objeto), "",
    exportacao_casca_chunk(paste0(raiz, "-carregar-base"), opcoes = "#| output: false"),
    chunk("configurar"), chunk("preparar"), chunk("modelo"),
    chunk("pressupostos"), chunk("texto"), "")
  if ("narrativa" %in% item$saidas_word) linhas <- c(linhas,
    "`r texto_resultados`", "", "`r texto_pressupostos`", "", "`r alerta_pressupostos`", "")
  if ("tabela" %in% item$saidas_word) linhas <- c(linhas,
    chunk("tabela", '#| tbl-cap: "Coeficientes da regressão linear simples, erros padrão e intervalos de confiança."', "tbl-"),
    chunk("metricas", '#| tbl-cap: "Métricas de ajuste da regressão linear simples."', "tbl-"), "")
  if ("grafico" %in% item$saidas_word) linhas <- c(linhas,
    chunk("grafico", c('#| fig-cap: "Reta ajustada e intervalo de confiança da resposta média. O nível de confiança é definido no roteiro."',
      "#| fig-width: 6", "#| fig-height: 4"), "fig-"), "")
  if ("pressupostos" %in% item$saidas_word) linhas <- c(linhas,
    ':::: {.content-visible when-format="html"}', "### Leitura dos pressupostos", "",
    "Um p-valor acima do nível de significância não comprova o pressuposto. Leia os testes junto aos gráficos e ao delineamento.", "",
    exportacao_casca_chunk(paste0(raiz, "-mostrar-pressupostos")), "::::", "")
  if ("diagnosticos" %in% item$saidas_word) linhas <- c(linhas,
    ':::: {.content-visible when-format="html"}', "### Diagnóstico do modelo", "",
    "**Linearidade e variância.** Procure curvatura e formato de funil nos resíduos versus ajustados.", "",
    chunk("diagnostico-variancia", c(
      '#| fig-cap: "Resíduos versus valores ajustados. Curvatura sugere que a reta não descreve bem a média; formato de funil sugere variância não constante."',
      "#| fig-width: 6", "#| fig-height: 4"), "fig-"), "",
    "**Normalidade.** No gráfico Q-Q, procure desvios sistemáticos da reta, sobretudo nas caudas.", "",
    chunk("diagnostico-normalidade", c(
      '#| fig-cap: "Gráfico Q-Q dos resíduos padronizados. Desvios sistemáticos da reta, sobretudo nas caudas, pedem investigação."',
      "#| fig-width: 6", "#| fig-height: 4"), "fig-"), "",
    "**Independência.** Confira a unidade amostral, medidas repetidas e a ordem de coleta. O gráfico de ordem e Durbin-Watson só são executados quando essa ordem foi confirmada no roteiro.", "",
    chunk("diagnostico-ordem", c(
      '#| fig-cap: "Resíduos na ordem das linhas utilizadas. Só se lê como sequência de coleta quando essa ordem for real no delineamento."',
      "#| fig-width: 6", "#| fig-height: 4"), "fig-"), "",
    "**Influência (complementar).** Cook destaca observações que merecem conferência. A linha 4/n não autoriza excluir dados automaticamente.", "",
    chunk("diagnostico-influencia", c(
      '#| fig-cap: "Distância de Cook por observação. A linha tracejada marca 4/n, uma referência de triagem e não um teste de hipótese."',
      "#| fig-width: 6", "#| fig-height: 4"), "fig-"), "::::", "")
  linhas
}

exportacao_gerar_script <- function(manifesto, nome_projeto = "projeto",
                                    registro_bases = list(), pipeline = list(),
                                    base_externa = NULL, import_info = list(),
                                    templates_dir = "templates") {
  manifesto <- exportacao_sem_execucoes_repetidas(manifesto)
  if (exportacao_anova_simples(manifesto)) {
    return(exportacao_modelo_anova("analise.R", manifesto, import_info, templates_dir,
                                    pipeline, registro_bases, base_externa))
  }
  linhas <- c(
    exportacao_cabecalho_script(nome_projeto),
    exportacao_trecho_instalar(),
    "",
    exportacao_trecho_pacotes(),
    "",
    exportacao_trecho_importar(import_info),
    "",
    exportacao_preparo_sem_funcao_data(
      exportacao_trecho_tratar(pipeline, base_externa, import_info = import_info)),
    exportacao_trecho_carregar_base(),
    "",
    ""
  )
  incluidas <- manifesto$execucoes %||% list()
  raizes <- exportacao_raizes_chunk(manifesto$execucoes %||% list())
  for (item in incluidas) {
    raiz <- if (item$id %in% names(raizes)) unname(raizes[[item$id]]) else
      exportacao_slug_chunk(item$id, "analise")
    variavel <- exportacao_nome_resultado(raiz)
    if (identical(item$tipo, "anova_um_fator")) {
      linhas <- c(linhas, paste0("# ", item$titulo),
        if (!isTRUE(item$incluir_word)) "# Para estudo: execução não incluída no relatório.",
        if (identical(item$base_tipo, "derivada")) exportacao_trecho_base(item, raiz, registro_bases),
        exportacao_anova_trechos(item, raiz, import_info, templates_dir)$script)
      next
    }
    if (exportacao_regressao_simples(item)) {
      linhas <- c(linhas, paste0("# ", item$titulo),
        if (!isTRUE(item$incluir_word)) "# Para estudo: execução não incluída no relatório.",
        exportacao_trecho_base(item, raiz, registro_bases),
        exportacao_trecho_carregar_base(item, raiz),
        exportacao_regressao_trechos(item, raiz, templates_dir), "",
        exportacao_marcador(paste0(raiz, "-mostrar-pressupostos")),
        "flextable_ocean(tabela_pressupostos)", "")
      next
    }
    linhas <- c(
      linhas,
      sprintf("# ============================================================================"),
      sprintf("# %s", item$titulo),
      if (!isTRUE(item$incluir_word)) "# Para estudo: esta execução não foi incluída no relatório.",
      sprintf("# ============================================================================"),
      "",
      exportacao_trecho_base(item, raiz, registro_bases),
      exportacao_trecho_carregar_base(item, raiz),
      "",
      exportacao_trecho_analise(item, raiz),
      "",
      exportacao_trecho_resultado(item, raiz, variavel),
      ""
    )
    for (componente in item$saidas_word) {
      linhas <- c(
        linhas,
        exportacao_trecho_componente(variavel, item$id, componente,
                                     raiz_chunk = raiz, tipo = item$tipo),
        ""
      )
    }
  }
  c(
    linhas,
    exportacao_marcador("fim-do-codigo"),
    "# (nada além daqui entra no relatório)"
  )
}

# ---- O relatório: relatorios/relatorio.qmd ------------------------------------
# O texto e as cascas dos chunks. Os corpos são preenchidos a partir do script
# por atualizar_codigo(), em exportacao_criar_projeto(). Os comentários HTML
# (<!-- -->) não saem em nenhuma das duas saídas: são a camada didática sobre
# programação literária, para quem está lendo o fonte. O R se explica no
# script; aqui se explica a organização do documento.

exportacao_nota <- function(...) {
  texto <- c(...)
  c(paste0("<!-- ", texto[1]), if (length(texto) > 1) paste0("     ", texto[-1]), "-->")
}

exportacao_yaml_qmd <- function(titulo_projeto, globais = list()) {
  identificacao <- exportacao_identificacao_documento(
    globais, titulo_projeto, "Projeto R exportado pela CatalyseR"
  )
  c(
    "---",
    paste0("title: ", exportacao_yaml_texto(identificacao$titulo)),
    paste0("subtitle: ", exportacao_yaml_texto(identificacao$subtitulo)),
    identificacao$autores_yaml,
    "date: today",
    "date-format: \"D [de] MMMM [de] YYYY\"",
    "lang: pt-BR",
    "bibliography: referencias.bib",
    "csl: abnt.csl",
    "crossref:",
    "  # Usa “Tabela 1 – ...” em vez de “Tabela 1: ...”, conforme a ABNT.",
    "  title-delim: \" – \"",
    "format:",
    "  # 1. Word: relatório para o leitor, somente com texto, tabelas e figuras.",
    "  docx:",
    "    # Modelo de página com aparência de artigo, o mesmo da CatalyseR.",
    "    reference-doc: custom-reference.docx",
    "    toc: false",
    "    number-sections: true",
    "    fig-width: 4.9",
    "    fig-height: 3.0",
    "    fig-dpi: 300",
    "    fig-align: center",
    "  # 2. HTML: caderno com código recolhido, para quem quer ver como se fez.",
    "  html:",
    "    format-links: false",
    "    # Cores e fontes do ecossistema EAPA.",
    "    theme: [cosmo, ocean.scss]",
    "    title-block-banner: \"#0F3B5F\"",
    "    title-block-banner-color: \"white\"",
    "    toc: true",
    "    toc-location: left",
    "    toc-depth: 2",
    "    toc-title: \"Neste caderno\"",
    "    number-sections: true",
    "    # Um único arquivo HTML, fácil de compartilhar.",
    "    embed-resources: true",
    "    # Exibe o código, recolhido; o menu no canto mostra ou esconde tudo.",
    "    echo: true",
    "    code-fold: true",
    "    code-summary: \"Código\"",
    "    code-tools: true",
    "    code-copy: true",
    "    df-print: kable",
    "    fig-width: 5.2",
    "    fig-height: 3.2",
    "    fig-align: center",
    "    smooth-scroll: true",
    "execute:",
    "  # No Word, apresenta os resultados sem exibir o código.",
    "  echo: false",
    "  warning: false",
    "  message: false",
    "editor_options:",
    "  chunk_output_type: console",
    "---",
    ""
  )
}

exportacao_gerar_qmd <- function(manifesto, titulo_projeto = "Relatório de análise",
                                 registro_bases = list(), pipeline = list(),
                                 base_externa = NULL, import_info = list(),
                                 templates_dir = "templates") {
  manifesto <- exportacao_sem_execucoes_repetidas(manifesto)
  if (exportacao_anova_simples(manifesto)) {
    return(exportacao_modelo_anova("relatorio.qmd", manifesto, import_info, templates_dir,
                                    pipeline, registro_bases, base_externa))
  }
  globais <- manifesto$secoes_globais %||% list()
  planilha <- exportacao_nome_planilha(import_info)
  texto_ou_lembrete <- function(texto, lembrete) {
    if (nzchar(trimws(texto %||% ""))) texto else exportacao_nota(lembrete)
  }

  linhas <- c(
    exportacao_yaml_qmd(titulo_projeto, globais),
    exportacao_nota(
      "GUIA DE LEITURA DESTE ARQUIVO",
      "Este é um documento de programação literária: texto e código no mesmo",
      "arquivo, na ordem em que a análise é pensada. Na seta do Render, escolha",
      "Word, para o leitor, ou HTML, para o caderno do pesquisador. O HTML é para",
      "quem quer ver o código. Comentários como este não saem em nenhum dos dois.",
      "",
      "O código dos chunks é copiado de R/analise.R, onde mora com as",
      "explicações. Aqui ele aparece limpo, e a primeira linha de cada chunk",
      "(# fonte: ...) diz de quais trechos do script ele veio. Edita-se o",
      "código lá, nunca aqui; depois, roda-se o chunk `atualizar`.",
      "",
      "As opções #| no começo de cada chunk dizem ao Quarto o que fazer com ele:",
      "  include: false  roda, mas não mostra nem código nem resultado;",
      "  output: false   roda e esconde o resultado (o código aparece no HTML);",
      "  eval: false     não roda no Render, só fica para leitura;",
      "  results: asis   o resultado é markdown pronto (tabelas e narrativa)."
    ),
    "",
    "```{r}",
    "#| label: codigo-do-script",
    "#| include: false",
    "# Conferência, no Render: para se o código daqui estiver diferente do que",
    "# está em R/analise.R, para o relatório nunca sair desatualizado.",
    "source(here::here(\"R\", \"funcoes.R\"))",
    "conferir_codigo()",
    "```",
    "",
    "```{r}",
    "#| label: atualizar",
    "#| eval: false",
    "# Editou R/analise.R? Rode este chunk (Ctrl+Shift+Enter). Ele traz o código",
    "# novo, sem os comentários, para dentro dos chunks abaixo. Não roda no Render.",
    "source(here::here(\"R\", \"funcoes.R\"))",
    "atualizar_codigo()",
    "```",
    "",
    exportacao_nota(
      "Os dois chunks abaixo preparam o terreno e não aparecem em nenhuma saída",
      "(include: false). O `instalar` roda uma vez, em computador novo; o",
      "`pacotes` roda a cada Render."
    ),
    exportacao_casca_chunk("instalar", opcoes = c("#| include: false", "#| eval: false")),
    "",
    exportacao_casca_chunk("pacotes", opcoes = "#| include: false"),
    "",
    "# Introdução",
    "",
    texto_ou_lembrete(
      globais$introducao,
      "Escreva aqui a introdução: o contexto, a pergunta e por que ela importa."
    ),
    "",
    "# Material e métodos",
    "",
    "## Os dados",
    "",
    sprintf("A entrada original está em `dados/brutos/%s`. A receita de importação e preparo está em `R/analise.R`, para estudo e conferência; este relatório usa as bases preparadas em RDS.", planilha),
    "",
    exportacao_nota(
      "Um chunk de trabalho: roda no Render, mas o leitor do Word vê só o",
      "parágrafo acima (output: false). No caderno HTML o código aparece,",
      "recolhido. É o padrão de todo chunk que prepara sem apresentar."
    ),
    exportacao_casca_chunk("carregar-compartilhada", opcoes = "#| output: false"),
    "",
    "## Preparo dos dados",
    "",
    "As operações estruturais e a Trilha de Preparo foram conferidas na exportação. O script permite refazê-las e comparar os dados com a base compartilhada salva. Alterar a receita não altera os RDS automaticamente: confira e salve as bases antes de renderizar com o novo preparo.",
    "",
    "Cada análise lê sua base compartilhada ou derivada já preparada, sem repetir os tratamentos no relatório.",
    "",
    "## Análise dos dados",
    "",
    texto_ou_lembrete(
      globais$metodos,
      "Descreva aqui os métodos: o que cada análise testa e a que nível de significância."
    ),
    "",
    "As configurações analíticas foram registradas explicitamente na CatalyseR. Cada análise, abaixo, informa a pergunta, a base e os parâmetros usados.",
    "",
    "# Resultados",
    ""
  )

  incluidas <- Filter(function(x) isTRUE(x$incluir_word), manifesto$execucoes %||% list())
  if (!length(incluidas)) {
    linhas <- c(linhas, "*Nenhuma execução foi selecionada para o relatório.*", "")
  }
  raizes <- exportacao_raizes_chunk(manifesto$execucoes %||% list())
  primeira <- TRUE
  for (item in incluidas) {
    raiz <- if (item$id %in% names(raizes)) unname(raizes[[item$id]]) else
      exportacao_slug_chunk(item$id, "analise")
    # O objeto do resultado leva o nome da análise (`anova_profundidade_m`),
    # não o ID interno da execução.
    variavel <- exportacao_nome_resultado(raiz)
    vivo <- exportacao_codigo_vivo(item)
    if (identical(item$tipo, "anova_um_fator")) {
      linhas <- c(linhas, exportacao_qmd_anova_acompanhada(item, raiz, import_info, templates_dir))
      next
    }
    if (exportacao_regressao_simples(item)) {
      linhas <- c(linhas, exportacao_qmd_regressao(item, raiz))
      next
    }
    linhas <- c(
      linhas,
      paste0("## ", item$titulo),
      "",
      sprintf("**Pergunta:** %s  ", exportacao_pergunta(item)),
      sprintf("**Base utilizada:** `%s`  ", item$base_objeto),
      sprintf("**Execução registrada:** `%s`", item$id),
      "",
      if (primeira) exportacao_nota(
        "Cada análise segue o mesmo desenho. Um chunk de trabalho reúne três",
        "trechos do script, porque não há texto entre eles: a base da análise,",
        "a análise passo a passo e o resultado pela função da CatalyseR. Depois,",
        "um chunk por componente mostra o que você escolheu para o relatório,",
        "cada um sob o seu título."
      ) else NULL,
      if (vivo) {
        exportacao_casca_chunk(
          raiz,
          fontes = paste0(raiz, c("-carregar-base", "-analise", "-resultado")),
          opcoes = "#| output: false"
        )
      } else {
        c(
          exportacao_casca_chunk(
            raiz,
            fontes = paste0(raiz, c("-carregar-base", "-resultado")),
            opcoes = "#| output: false"
          ),
          "",
          exportacao_nota(
            "O passo a passo desta análise ainda não foi validado: o chunk fica",
            "só para leitura (eval: false) e o resultado vem do chunk acima."
          ),
          exportacao_casca_chunk(paste0(raiz, "-analise"), opcoes = "#| eval: false")
        )
      },
      ""
    )
    primeira <- FALSE
    for (componente in item$saidas_word) {
      linhas <- c(
        linhas,
        exportacao_chunk_componente(item$id, componente,
                                    raiz_chunk = raiz, tipo = item$tipo)
      )
    }
  }
  c(
    linhas,
    "# Discussão",
    "",
    texto_ou_lembrete(
      globais$discussao,
      "Escreva aqui a discussão: o que os resultados dizem, comparados ao que se esperava."
    ),
    "",
    "# Conclusão",
    "",
    texto_ou_lembrete(
      globais$conclusao,
      "Escreva aqui a conclusão, em poucas frases, respondendo à pergunta da introdução."
    ),
    "",
    exportacao_nota(
      "Uma cerca de dois-pontos com a condição when-format=\"html\" faz um trecho",
      "existir só no caderno HTML; no Word o Quarto o remove antes de gerar."
    ),
    ":::: {.content-visible when-format=\"html\"}",
    "::: callout-note",
    "## Sobre este documento",
    "",
    "Este relatório lê as bases preparadas em RDS e refaz cada análise. A importação e o preparo completos estão em `R/analise.R`, para estudo e conferência. O mesmo `.qmd` gera o Word para o leitor e este caderno em HTML. Reinicie o R e escolha o formato na seta do Render. Para ver um passo intermediário, rode seu chunk no RStudio.",
    ":::",
    "::::",
    ""
  )
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
    sprintf("- Execuções incluídas no relatório: %d", manifesto$total_word),
    ""
  )
  for (item in manifesto$execucoes) {
    linhas <- c(
      linhas,
      sprintf("## %s — %s", item$id, item$titulo),
      "",
      sprintf("- Base: `%s`", item$base_objeto),
      sprintf("- Incluída no relatório: %s", if (item$incluir_word) "sim" else "não"),
      sprintf("- Conteúdo do relatório: %s", if (length(item$saidas_word)) paste(item$saidas_word, collapse = ", ") else "nenhum"),
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
#' Uma falha interrompe a geração: o projeto precisa de sua planilha de entrada.
#'
#' @return `TRUE` se a planilha foi gravada (invisível).
exportacao_salvar_planilha <- function(df, caminho, aba = "dados") {
  if (!requireNamespace("writexl", quietly = TRUE)) stop("Instale writexl antes de exportar o projeto.", call. = FALSE)
  conteudo <- list(as.data.frame(df))
  names(conteudo) <- aba
  gravou <- tryCatch({
    writexl::write_xlsx(conteudo, path = caminho)
    TRUE
  }, error = function(e) stop(paste("Não foi possível gravar", basename(caminho), ":", conditionMessage(e)), call. = FALSE))
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
    "análise já foi feita: você a montou na CatalyseR, e o projeto a refaz em",
    "código, para você ler, rodar e adaptar. O Word não vem pronto de",
    "propósito: ele nasce aqui, no RStudio, quando você clica em Render, e é",
    "assim que se vê de onde cada tabela e cada frase saem.", "",
    sprintf("Origem: `%s`, aba `%s`. O Excel exportado contém somente os valores dessa aba, antes do preparo.",
      basename(import_info$file_name %||% import_info$package_dataset %||% planilha), exportacao_aba_planilha(import_info)), "",
    "Use R 4.3 ou posterior, RStudio e Quarto. A instalação inicial requer internet.", "",
    "## A estrutura", "",
    "```",
    paste0(nome_projeto, "/"),
    "│",
    "├── projeto_analise.Rproj     abra o projeto por aqui (define a raiz para o here)",
    "├── README.md                 este arquivo",
    "│",
    "├── dados/",
    sprintf("│   ├── brutos/%s", planilha),
    "│   │                         valores da aba utilizada, antes do preparo. SOMENTE LEITURA",
    "│   └── processados/",
    "│       ├── base_compartilhada.rds   base tratada usada pelo relatório",
    "│       ├── base_0001.rds           cada derivada utilizada, quando houver",
    "│       ├── base_compartilhada.xlsx  a mesma base, para abrir no Excel",
    "│       └── base_<nome>.xlsx         cada derivada utilizada, quando houver",
    "│",
    "├── R/",
    "│   ├── analise.R             O CÓDIGO, comentado passo a passo: é aqui que se edita",
    "│   └── funcoes.R             funções de apoio e ligação entre script e relatório",
    "│",
    "├── imagens/                  fotos, esquemas e mapas que NÃO vêm do código (começa vazia)",
    "│",
    "├── relatorios/",
    "│   ├── relatorio.qmd         O RELATÓRIO: o texto e o código (copiado de analise.R)",
    "│   ├── custom-reference.docx modelo de página do Word",
    "│   ├── ocean.scss            cores e fontes do caderno HTML",
    "│   ├── relatorio.docx        o relatório para o leitor (gerado pelo Render)",
    "│   └── relatorio.html        o caderno do pesquisador, com o código (gerado)",
    "│",
    "```", "",
    "A separação que importa é entre **o que entra** (`dados/brutos/`,",
    "`imagens/`), **o que a gente escreve** (`R/analise.R`,",
    "`relatorios/relatorio.qmd`) e **o que o código gera** (`relatorio.docx`,",
    "`relatorio.html`). Tudo da terceira categoria pode ser apagado e refeito",
    "escolhendo cada formato na seta do Render.", "",
    "## Onde o código mora: o script e o relatório", "",
    "O código aparece em dois lugares, mas só se **escreve** em um. Em",
    "`R/analise.R` ele vem com os comentários que explicam cada passo: o que o",
    "trecho recebe, o que produz, o que conferir. No `relatorio.qmd` ele vem",
    "limpo, só as linhas que fazem alguma coisa, para que o caderno HTML mostre",
    "o que se fez sem a aula no meio, e para que qualquer chunk possa ser rodado",
    "linha a linha no RStudio.", "",
    "A ligação entre os dois é a primeira linha de cada chunk:", "",
    "```r",
    "# fonte: importar",
    "```", "",
    "Ela diz de quais trechos do script (os marcados com `## ---- nome ----`) o",
    "chunk é feito. A regra que sustenta tudo: **o código se edita no script,",
    "nunca no relatório.** Depois de editar, rode o chunk `atualizar`, logo no",
    "começo do `.qmd`: ele copia o código novo para os chunks, sem os",
    "comentários, e diz quais mudaram. Se alguém esquecer, o Render para na",
    "primeira linha, dizendo qual chunk está diferente do script. Assim os dois",
    "nunca divergem em silêncio.", "",
    "Quem prefere estudar no script, estuda no script (o menu de seções do",
    "RStudio, Ctrl+Shift+O, lista os trechos). Quem prefere o caderno, lê o",
    "caderno e abre o script no trecho de mesmo nome quando quer saber o porquê.", "",
    "## Como rodar", "",
    "1. Abra `projeto_analise.Rproj` no RStudio. Isso define a raiz do projeto,",
    "   que é o que o `here()` usa para montar os caminhos.",
    "2. Em computador novo, rode o chunk `instalar` de `relatorios/relatorio.qmd`",
    "   uma vez (Ctrl+Shift+Enter com o cursor nele). Ele instala só o que",
    "   falta: `catalyser` e `EAPADados` vêm do GitHub; `dplyr`, `tidyr`,",
    "   `ggplot2`, `stringr`, `purrr`, `lubridate`, `readxl` e os demais vêm do CRAN. Faça isso",
    "   antes do primeiro Render: o relatório usa o `here` logo na primeira linha.",
    "3. Reinicie o R (Ctrl+Shift+F10). Na seta do **Render**, escolha **Word**",
    "   para gerar `relatorios/relatorio.docx` e depois **HTML** para gerar",
    "   `relatorios/relatorio.html`. Cada escolha atualiza somente aquele formato.",
    "4. Para ver um passo isolado, rode o chunk correspondente no RStudio",
    "   (Ctrl+Shift+Enter) ou, no script, as linhas do trecho (Ctrl+Enter). Com o",
    "   cursor num chunk mais abaixo, Ctrl+Alt+P roda todos os anteriores.", "",
    "Se o relatório sai com a memória limpa, a análise é reprodutível.", "",
    "## O caminho dos dados", "",
    sprintf("1. A planilha bruta, em `dados/brutos/%s`.", planilha),
    "2. No script, os trechos `importar` e `tratar` reconstroem e conferem a base.",
    "3. No relatório, `carregar-compartilhada` lê o RDS já preparado.",
    "4. Em Resultados, cada análise lê a sua base e executa o código do script.",
    "   Na ANOVA, há chunks separados para modelo, pressupostos, Tukey e texto.",
    "   As tabelas e figuras usam esses objetos diretamente, como na ANOVA isolada.",
    "   As outras análises mantêm seu código e suas funções de apresentação.", "",
    "As bases derivadas nascem diretamente de `dados_analise`, em um único salto:",
    "não existem ramos de ramos. Cada derivada utilizada também tem uma cópia",
    "em Excel para consulta; a receita que a reconstrói permanece no código.", "",
    "## Os arquivos de `dados/processados/`", "",
    "- `base_compartilhada.rds` — base preparada, usada pelo relatório e pela conferência no script.",
    "- `base_0001.rds` (e outros IDs) — cada derivada utilizada, preservando os tipos do R.",
    "- `base_compartilhada.xlsx` — a base já tratada, para abrir no Excel ou",
    "  enviar a quem não usa R. É entrega, não fonte do relatório.",
    "- `base_<nome>.xlsx` — fotografia de cada base derivada utilizada, para consulta.",
    "- `base_resolvida.rds` — somente registros antigos sem sequência estrutural executável.", "",
    "O Render lê os RDS e refaz as análises. Para mudar o preparo, execute a",
    "receita no script, confira os dados e use as linhas comentadas de `saveRDS()`",
    "para adotar a mudança. Atualize a compartilhada e cada derivada afetada.",
    "`atualizar_codigo()` só copia código; ele não grava bases. Os Excel continuam",
    "representando a exportação original. Para apenas mudar a análise ou o gráfico,",
    "basta editar o script, salvar e rodar o chunk `atualizar`.", "",
    "## O relatório", "",
    "Abra `relatorios/relatorio.qmd`. É um documento de programação literária:",
    "o texto e o código na mesma ordem em que a análise é pensada, e um único",
    "arquivo que gera o Word (para o leitor) e o caderno HTML (para quem quer",
    "ver como se fez). Os comentários `<!-- -->` espalhados pelo fonte explicam",
    "a organização do documento e não saem em nenhuma das saídas; o R se",
    "explica no script.", "",
    "O que o leitor vê e o que o pesquisador vê: chunks de trabalho rodam com",
    "`output: false` e não entram no Word; no HTML o código deles aparece,",
    "recolhido. Só os chunks de componente (tabelas, gráficos, narrativa) e o",
    "texto aparecem nos dois. O projeto preserva todas as execuções",
    "registradas no script; o relatório mostra somente as execuções selecionadas.", "",
    "## Relação com o EAPACaderno", "",
    "Quem conhece o EAPACaderno (o projeto-modelo do ecossistema) reconhece tudo",
    "aqui: `here()`, o par script + relatório, dados brutos intocáveis, Word no",
    "mesmo modelo de página. As receitas e os parâmetros estão no script;",
    "as escolhas de apresentação estão no relatório. Não há pasta de metadados.", "",
    "## Funções de apoio", "",
    "A ANOVA usa `aov()`, `TukeyHSD()` e os ajudantes de `R/funcoes.R`, como",
    "`resumir_grupo()`, `fmt()` e `flextable_ocean()`. Outras análises também",
    "usam funções do pacote `catalyser`, com ajuda em português:",
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
  manifesto <- exportacao_sem_execucoes_repetidas(manifesto)
  validacao <- exportacao_validar_manifesto(manifesto, exigir_word = FALSE)
  if (!validacao$ok) stop(paste(validacao$mensagens, collapse = " "), call. = FALSE)

  if (exportacao_anova_simples(manifesto)) {
    codigo <- exportacao_preparo_anova(manifesto, import_info, pipeline, registro_bases, base_externa)
    erro_preparo <- tryCatch({
      exportacao_conferir_preparo_anova(codigo, dados_brutos, dados_analise, manifesto, cache_bases, base_resolvida)
      NULL
    }, error = function(e) e)
    if (!is.null(erro_preparo)) {
      tem_estrutura <- nzchar(trimws(as.character(base_externa$codigo %||% "")))
      if (!tem_estrutura || !is.null(base_externa$codigo_sequencial) ||
          exportacao_anova_usa_base_resolvida(base_externa)) stop(erro_preparo)
      # Uma edição pode ter partido de uma configuração de importação anterior.
      # Nesse caso usamos a base salva, mantendo a conferência da trilha e do ramo.
      base_externa$usar_snapshot_anova <- TRUE
      codigo <- exportacao_preparo_anova(manifesto, import_info, pipeline, registro_bases, base_externa)
      exportacao_conferir_preparo_anova(codigo, dados_brutos, dados_analise, manifesto, cache_bases, base_resolvida)
    }
  } else {
    # Confere também o exportador geral antes de escrever os arquivos.
    # Cada ramo parte da compartilhada; nunca reutiliza o resultado do anterior.
    itens <- manifesto$execucoes
    if (!length(itens)) itens <- list(list(incluir_word = TRUE, base_tipo = "compartilhada"))
    for (item in itens) {
      item$incluir_word <- TRUE
      plano <- list(execucoes = list(item))
      codigo <- exportacao_preparo_anova(plano, import_info, pipeline, registro_bases, base_externa)
      exportacao_conferir_preparo_anova(codigo, dados_brutos, dados_analise, plano, cache_bases, base_resolvida)
    }
  }

  nome_projeto <- exportacao_nome_curto(nome_projeto)
  projeto <- file.path(destino, nome_projeto)
  if (dir.exists(projeto)) {
    stop("O diretório temporário do projeto já existe; gere a exportação novamente.", call. = FALSE)
  }
  dir.create(projeto, recursive = TRUE, showWarnings = FALSE)
  # Imagens recebe fotos e esquemas do pesquisador, também no caminho ANOVA.
  pastas <- c(file.path("dados", "brutos"),
              file.path("dados", "processados"), "R", "imagens", "relatorios")
  dirs <- file.path(projeto, pastas)
  vapply(dirs, dir.create, logical(1), recursive = TRUE, showWarnings = FALSE)

  # A pasta `dados/` é deliberadamente enxuta:
  #
  #   brutos/      a planilha bruta -> ponto de entrada do chunk importar;
  #   processados/ base_compartilhada.rds -> fotografia, usada apenas para conferência;
  #                base_compartilhada.xlsx -> entrega, para uso fora do R.
  #
  # Nada de cópias redundantes: o que o projeto sabe reconstruir, ele reconstrói.
  caminho_bruto <- file.path(projeto, "dados", "brutos", exportacao_nome_planilha(import_info))
  # Exporta somente os valores da aba importada, antes de qualquer preparo.
  # A pasta de trabalho original e suas outras abas ficam com o pesquisador.
  exportacao_salvar_planilha(dados_brutos, caminho_bruto, aba = exportacao_aba_planilha(import_info))
  exportacao_salvar_dataframe(
    dados_analise, file.path(projeto, "dados", "processados", "base_compartilhada.rds")
  )
  exportacao_salvar_planilha(
    dados_analise,
    file.path(projeto, "dados", "processados", "base_compartilhada.xlsx"),
    aba = "dados_analise"
  )
  # Uma cópia Excel de cada derivada usada pelas execuções do projeto.
  # São fotografias da IDE; o Render reconstrói as bases e não as sobrescreve.
  ids_usados <- unique(vapply(Filter(function(e) identical(e$base_tipo, "derivada"),
    registro_execucoes), function(e) e$base_id, character(1)))
  nomes_usados <- "base_compartilhada"
  for (id in ids_usados) {
    base <- bases_obter(registro_bases, id)
    df <- cache_bases[[id]]$df
    if (is.null(base) || is.null(df)) stop("Atualize a base derivada antes de exportar.", call. = FALSE)
    nome <- exportacao_nome_curto(base$nome_r, padrao = "base_derivada")
    if (nome %in% nomes_usados) nome <- paste0(nome, "_", exportacao_nome_curto(id))
    nomes_usados <- c(nomes_usados, nome)
    exportacao_salvar_planilha(df, file.path(projeto, "dados", "processados", paste0(nome, ".xlsx")))
    saveRDS(as.data.frame(df), file.path(projeto, "dados", "processados",
      exportacao_rds_base(list(base_tipo = "derivada", base_id = id))))
  }
  # Somente registros legados precisam da fotografia pós-estrutural.
  if (exportacao_anova_usa_base_resolvida(base_externa)) {
    exportacao_salvar_dataframe(
      base_resolvida, file.path(projeto, "dados", "processados", "base_resolvida.rds")
    )
  }

  # As funções de análise não viajam como arquivo: vêm do pacote catalyser,
  # documentadas e com ajuda em português (`?catalyser_anova`). Viajam três
  # templates: o modelo de página do Word e o tema do HTML, ao lado do
  # relatório, e o funcoes.R com a ligação script <-> relatório.
  templates <- c(
    "custom-reference.docx" = file.path("relatorios", "custom-reference.docx"),
    "ocean.scss" = file.path("relatorios", "ocean.scss"),
    "abnt.csl" = file.path("relatorios", "abnt.csl"),
    "referencias.bib" = file.path("relatorios", "referencias.bib"),
    "funcoes.R" = file.path("R", "funcoes.R")
  )
  for (nome in names(templates)) {
    origem <- if (nome == "funcoes.R" && exportacao_anova_simples(manifesto))
      file.path(templates_dir, "anova_um_fator", "funcoes.R")
    else file.path(templates_dir, nome)
    if (!file.exists(origem)) {
      stop(sprintf("O template '%s' do exportador não foi encontrado.", nome), call. = FALSE)
    }
    file.copy(origem, file.path(projeto, templates[[nome]]), overwrite = TRUE)
  }

  if (exportacao_anova_simples(manifesto)) {
    # A ANOVA mantém seus ajudantes de apresentação e recebe a mesma ligação
    # script -> relatório do exportador geral, sem uma segunda implementação.
    comuns <- readLines(file.path(templates_dir, "funcoes.R"), encoding = "UTF-8", warn = FALSE)
    inicio <- grep("^# 5[.] Manutenção do relatório", comuns)
    if (length(inicio) != 1L) stop("Confira as funções de atualização do relatório.", call. = FALSE)
    cat(paste(c("", comuns[seq.int(inicio, length(comuns))], ""), collapse = "\n"),
        file = file.path(projeto, "R", "funcoes.R"), append = TRUE)
  }

  # O preparo (trechos importar e tratar) e cada análise moram em R/analise.R;
  # o relatório recebe o código limpo. As bases derivadas não têm fotografia
  # em disco: são um salto reproduzível a partir de `dados_analise`, com a
  # receita de `bases_codigo()` no trecho -base da própria análise.


  titulo <- paste("Relatório de análise —", nome_projeto)
  caminho_script <- file.path(projeto, "R", "analise.R")
  caminho_qmd <- file.path(projeto, "relatorios", "relatorio.qmd")
  writeLines(
    exportacao_gerar_script(
      manifesto, nome_projeto, registro_bases = registro_bases,
      pipeline = pipeline, base_externa = base_externa, import_info = import_info,
      templates_dir = templates_dir
    ),
    caminho_script, useBytes = TRUE
  )
  writeLines(
    exportacao_gerar_qmd(
      manifesto, titulo, registro_bases = registro_bases,
      pipeline = pipeline, base_externa = base_externa, import_info = import_info,
      templates_dir = templates_dir
    ),
    caminho_qmd, useBytes = TRUE
  )
  # Os chunks do relatório nascem vazios (só a linha "# fonte:") e são
  # preenchidos pelo MESMO atualizar_codigo() que o pesquisador vai usar
  # depois de editar o script. Um caminho só, sem segunda implementação.
  ligacao <- new.env(parent = baseenv())
  sys.source(file.path(projeto, "R", "funcoes.R"), envir = ligacao)
  suppressMessages(ligacao$atualizar_codigo(qmd = caminho_qmd, script = caminho_script))
  ligacao$conferir_codigo(qmd = caminho_qmd, script = caminho_script)
  writeLines(
    c(
      "Version: 1.0", "RestoreWorkspace: No", "SaveWorkspace: No",
      "AlwaysSaveHistory: No", "Encoding: UTF-8"
    ),
    file.path(projeto, paste0(nome_projeto, ".Rproj")), useBytes = TRUE
  )
  leiame <- if (exportacao_anova_simples(manifesto)) {
    exportacao_modelo_anova("README.md", manifesto, import_info, templates_dir, pipeline, registro_bases, base_externa)
  } else {
    exportacao_leiame_projeto(nome_projeto, import_info)
  }
  leiame <- gsub("projeto_analise.Rproj", paste0(nome_projeto, ".Rproj"), leiame, fixed = TRUE)
  leiame <- gsub("projeto.Rproj", paste0(nome_projeto, ".Rproj"), leiame, fixed = TRUE)
  leiame <- sub("^projeto/$", paste0(nome_projeto, "/"), leiame)
  writeLines(leiame, file.path(projeto, "README.md"), useBytes = TRUE)

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



# Idioma e dimensões usados pelas tabelas de preparo; filtros são apenas visuais.
preparo_idioma_tabela <- function(colunas = NULL) {
  list(search = "Buscar:", lengthMenu = "Mostrar _MENU_ linhas", zeroRecords = "Nenhuma ocorrência encontrada",
    emptyTable = "Nenhum dado disponível", infoEmpty = "0 linhas",
    info = paste0("Mostrando _START_ a _END_ de _TOTAL_ linhas", if (!is.null(colunas)) paste0(" · ", colunas, " colunas")),
    infoFiltered = "(consulta sobre _MAX_ linhas)",
    paginate = list(first = "Primeira", previous = "Anterior", "next" = "Próxima", last = "Última"))
}

preparo_leitura_entrada <- function(info) {
  q <- function(x) encodeString(as.character(x), quote = '"')
  arquivo <- info$file_name %||% "dados.xlsx"
  if (identical(info$source, "package")) {
    c(sprintf("data(list = %s, package = \"EAPADados\")", q(info$package_dataset)),
      sprintf("dados_brutos <- as.data.frame(get(%s))", q(info$package_dataset)))
  } else if (tolower(tools::file_ext(arquivo)) %in% c("csv", "txt", "tsv")) {
    preparo_leitura_csv(info, "dados_brutos")
  } else {
    sprintf("dados_brutos <- as.data.frame(readxl::read_excel(%s, sheet = %s))", q(arquivo), if (is.null(info$excel_sheet)) "1" else q(info$excel_sheet))
  }
}

# A aba de importação e a sequência completa usam a mesma leitura.
preparo_codigo_importacao <- function(info) {
  paste(c("# Importação — CatalyseR",
    "# Guarde o arquivo original junto ao script ou ajuste o caminho da leitura.",
    "", preparo_leitura_entrada(info), "", exportacao_preparo_importacao(info),
    "", "# Confira dimensões e tipos antes de preparar os dados.", "dim(dados)", "str(dados)"), collapse = "\n")
}

preparo_codigo_completo <- function(info, pipeline = list(), base_externa = NULL) {
  leitura <- preparo_leitura_entrada(info)
  manifesto <- list(execucoes = list(preparo = list(incluir_word = TRUE, base_tipo = "compartilhada")))
  codigo <- exportacao_preparo_anova(manifesto, info, pipeline, base_externa = base_externa)
  codigo <- exportacao_preparo_sem_funcao_data(codigo)
  paste(c("# Preparo completo — CatalyseR", "# Guarde o arquivo original junto ao script ou ajuste o caminho da leitura.",
    "library(dplyr)", "library(tidyr)", "", leitura, "", codigo, "", "dados_analise <- base_compartilhada"), collapse = "\n")
}
