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

# O título informado para o projeto conserva todas as palavras.
# O helper curto continua servindo aos nomes legados de bases e às sugestões.
exportacao_nome_projeto <- function(x) {
  nome <- exportacao_nome_seguro(x)
  if (toupper(nome) %in% c("CON", "PRN", "AUX", "NUL", paste0("COM", 1:9), paste0("LPT", 1:9)))
    nome <- paste0(nome, "_analise")
  if (nchar(nome) > 80L) stop("Use um nome de projeto com até 80 caracteres após retirar os acentos.", call. = FALSE)
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
  sem_replay <- which(!vapply(execucoes, function(x)
    execucoes_tipo_reconstruivel(x$tipo), logical(1)))
  if (length(sem_replay)) {
    ids <- names(execucoes)
    if (is.null(ids)) ids <- rep("", length(execucoes))
    detalhes <- vapply(sem_replay, function(i) {
      id <- if (nzchar(ids[[i]])) ids[[i]] else paste0("item ", i)
      sprintf("%s (%s)", id, as.character(execucoes[[i]]$tipo %||% "sem tipo"))
    }, character(1))
    mensagens <- c(mensagens, sprintf(
      "Estas execuções não têm reconstrução no Projeto R: %s. Corrija o tipo ou retire a execução do registro antes de exportar.",
      paste(detalhes, collapse = ", ")
    ))
  }
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
    "  \"ggplot2\", \"GGally\", \"patchwork\", \"stringr\", \"purrr\", \"lubridate\", \"knitr\", \"rmarkdown\",",
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
    "    packageVersion(\"catalyser\") < package_version(\"0.1.18\")) {",
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
    "library(GGally)",
    "library(patchwork)",
    "library(car)",
    "library(multcompView)",
    "library(effectsize)",
    "library(EAPADados)",
    "",
    "# As funções de análise: as mesmas que a CatalyseR usou na tela, para o",
    "# resultado ser idêntico. ?catalyser_anova mostra a ajuda de qualquer uma.",
    "if (!requireNamespace(\"catalyser\", quietly = TRUE) ||",
    "    getNamespaceVersion(\"catalyser\") < package_version(\"0.1.18\")) {",
    "  stop(\"Este projeto requer catalyser >= 0.1.18. Atualize e reinicie o R: \",",
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
    anova_mista_subamostras = "anova_mista_com_subamostras",
    anova_medidas_repetidas = "anova_medidas_repetidas",
    friedman = "friedman_grupos_pareados",
    anova_dois_fatores = "anova_dois_fatores",
    qui_quadrado_variancia = "qui_quadrado_para_variancia",
    teste_f_variancias = "teste_f_duas_variancias",
    grafico_linhas = "grafico_de_linhas",
    regressao_linear = "regressao_linear",
    regressao_logistica = "regressao_logistica",
    regressao_poisson = "regressao_poisson",
    regressao_binomial_negativa = "regressao_binomial_negativa",
    teste_t_one_val = "teste_t_uma_amostra",
    teste_t_two_ind = "teste_t_duas_amostras",
    teste_t_paired = "teste_t_pareado",
    estatistica_descritiva = "estatistica_descritiva",
    qui_quadrado = "qui_quadrado",
    proporcao_uma = "uma_proporcao",
    proporcao_duas = "duas_proporcoes",
    qui_quadrado_aderencia = "qui_quadrado_aderencia",
    mcnemar = "mcnemar_pares_binarios",
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
    anova_mista_subamostras = sprintf(
      "a média de '%s' difere entre os grupos de '%s', respeitando '%s' como unidade?",
      p$resposta, p$fator, p$unidade
    ),
    anova_medidas_repetidas = sprintf("a média de '%s' muda entre as ocasiões de '%s' na mesma unidade?",
                                      p$resposta, p$momento),
    friedman = sprintf("'%s' difere entre as condições de '%s' nos mesmos blocos de '%s'?",
                       p$resposta, p$condicao, p$bloco),
    grafico_linhas = sprintf("como '%s' se comporta ao longo de '%s'?", p$y, p$x),
    regressao_linear = sprintf("'%s' varia em função de '%s'?", p$resposta, p$preditor),
    regressao_logistica = sprintf("o que prevê a ocorrência de '%s'?", p$resposta),
    regressao_poisson = sprintf("como a contagem de '%s' varia com os preditores escolhidos?", p$resposta),
    regressao_binomial_negativa = sprintf("como a contagem de '%s' varia quando há superdispersão?", p$resposta),
    teste_t_two_ind = sprintf("a média de '%s' difere entre os dois grupos de '%s'?",
                              p$resposta, p$grupo),
    teste_t_one_val = sprintf("a média de '%s' difere do valor de referência?", p$variavel),
    teste_t_paired = sprintf("houve mudança entre '%s' e '%s'?", p$variavel_1, p$variavel_2),
    qui_quadrado_variancia = sprintf("a variância de '%s' difere da referência definida?", p$variavel),
    teste_f_variancias = sprintf("a variância de '%s' difere entre os grupos de '%s'?", p$resposta, p$grupo),
    estatistica_descritiva = "como se distribuem as variáveis escolhidas?",
    qui_quadrado = sprintf("'%s' e '%s' são independentes?", p$var_row, p$var_col),
    proporcao_uma = sprintf("a proporção de '%s' difere da referência definida?", p$sucesso),
    proporcao_duas = sprintf("a proporção de '%s' difere entre os grupos de '%s'?", p$sucesso, p$grupo),
    qui_quadrado_aderencia = sprintf("as contagens de '%s' seguem as proporções esperadas?", p$variavel),
    mcnemar = sprintf("a resposta binária muda entre '%s' e '%s' nos mesmos pares?", p$variavel_1, p$variavel_2),
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

  # Poisson e Binomial Negativa compartilham a preparação; só muda o motor do ajuste.
  codigo_contagem <- function(familia) {
    preditores <- vetor_r(p$preditores)
    colunas <- vetor_r(c(p$resposta, p$preditores, if (isTRUE(p$usar_offset)) p$offset))
    offset <- if (isTRUE(p$usar_offset)) c(
      sprintf("dados_modelo$.catalyser_offset_log <- log(dados_modelo[[%s]])", texto_r(p$offset))
    ) else character()
    termos <- if (isTRUE(p$usar_offset)) {
      sprintf("c(%s, %s)", preditores, texto_r("offset(.catalyser_offset_log)"))
    } else {
      preditores
    }
    ajuste <- if (identical(familia, "poisson")) {
      "modelo <- stats::glm(formula_modelo, data = dados_modelo, family = stats::poisson())"
    } else {
      "modelo <- MASS::glm.nb(formula_modelo, data = dados_modelo)"
    }
    c(
      sprintf("variaveis_modelo <- %s", colunas),
      "dados_modelo <- dados[stats::complete.cases(dados[variaveis_modelo]), variaveis_modelo, drop = FALSE]",
      offset,
      sprintf("formula_modelo <- stats::reformulate(%s, response = %s)", termos, texto_r(p$resposta)),
      ajuste,
      "dispersao_pearson <- sum(stats::residuals(modelo, type = 'pearson')^2) / stats::df.residual(modelo)",
      "summary(modelo)"
    )
  }

  codigo <- switch(
    execucao$tipo,
    descricao_exploratoria = catalyser_codigo_descricao(p),
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
    regressao_poisson = codigo_contagem("poisson"),
    regressao_binomial_negativa = codigo_contagem("binomial_negativa"),
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
    friedman = c(
      "# 1. Declarar a resposta, a condição e o bloco que se repete.",
      sprintf("resposta <- %s", texto_r(p$resposta)),
      sprintf("condicao <- %s", texto_r(p$condicao)),
      sprintf("bloco <- %s", texto_r(p$bloco)),
      "",
      "# 2. Conferir se todos os blocos têm todas as condições.",
      "dados_friedman <- dados[stats::complete.cases(dados[c(resposta, condicao, bloco)]), c(resposta, condicao, bloco)]",
      "names(dados_friedman) <- c('valor', 'condicao', 'bloco')",
      "presencas <- stats::xtabs(~ bloco + condicao, data = dados_friedman) > 0",
      "stopifnot(all(rowSums(presencas) == ncol(presencas)))",
      "",
      "# 3. Aplicar o teste de Friedman para condições pareadas.",
      "resultado <- stats::friedman.test(valor ~ condicao | bloco, data = dados_friedman)",
      "",
      "# 4. Comparar pares com Holm quando a diferença global justificar.",
      "posteste <- stats::pairwise.wilcox.test(dados_friedman$valor, dados_friedman$condicao, paired = TRUE, p.adjust.method = 'holm')",
      "resultado",
      "posteste"
    ),
    mcnemar = c(
      "# 1. Declarar as duas medições binárias feitas nos mesmos pares.",
      sprintf("primeira_medicao <- %s", texto_r(p$variavel_1)),
      sprintf("segunda_medicao <- %s", texto_r(p$variavel_2)),
      "",
      "# 2. Manter apenas os pares completos e montar a tabela 2 por 2.",
      "dados_mcnemar <- dados[stats::complete.cases(dados[c(primeira_medicao, segunda_medicao)]), ]",
      "tabela_pares <- table(dados_mcnemar[[primeira_medicao]], dados_mcnemar[[segunda_medicao]])",
      "",
      "# 3. Testar se as proporções mudaram entre as medições.",
      sprintf("resultado <- stats::mcnemar.test(tabela_pares, correct = %s)", if (isTRUE(p$correcao)) "TRUE" else "FALSE"),
      "resultado"
    ),
    anova_medidas_repetidas = c(
      sprintf("resposta <- %s", texto_r(p$resposta)),
      sprintf("sujeito <- %s", texto_r(p$sujeito)),
      sprintf("momento <- %s", texto_r(p$momento)),
      "dados_repetidos <- dados[stats::complete.cases(dados[c(resposta, sujeito, momento)]), c(resposta, sujeito, momento)]",
      "names(dados_repetidos) <- c('valor', 'sujeito', 'momento')",
      "dados_repetidos$sujeito <- factor(dados_repetidos$sujeito)",
      "dados_repetidos$momento <- factor(dados_repetidos$momento)",
      "presencas <- stats::xtabs(~ sujeito + momento, data = dados_repetidos) > 0",
      "sujeitos_completos <- rownames(presencas)[rowSums(presencas) == ncol(presencas)]",
      "dados_repetidos <- droplevels(dados_repetidos[dados_repetidos$sujeito %in% sujeitos_completos, ])",
      "modelo <- stats::aov(valor ~ sujeito + momento, data = dados_repetidos)",
      "summary(modelo)"
    ),
    qui_quadrado_variancia = c(
      sprintf("variavel <- %s", texto_r(p$variavel)),
      sprintf("desvio_referencia <- %s", numero_r(p$desvio_hipotetico)),
      sprintf("alternativa <- %s", texto_r(p$alternativa)),
      "x <- dados[[variavel]]",
      "x <- x[is.finite(x)]",
      "gl <- length(x) - 1L",
      "qui_quadrado <- gl * stats::var(x) / desvio_referencia^2",
      "p_inferior <- stats::pchisq(qui_quadrado, gl)",
      "p_superior <- stats::pchisq(qui_quadrado, gl, lower.tail = FALSE)",
      "p_valor <- if (alternativa == 'less') p_inferior else if (alternativa == 'greater') p_superior else min(1, 2 * min(p_inferior, p_superior))",
      "c(qui_quadrado = qui_quadrado, gl = gl, p_valor = p_valor)"
    ),
    teste_f_variancias = c(
      sprintf("resposta <- %s", texto_r(p$resposta)),
      sprintf("grupo <- %s", texto_r(p$grupo)),
      sprintf("alternativa <- %s", texto_r(p$alternativa)),
      sprintf("nivel_confianca <- %s", numero_r(p$nivel_confianca %||% 0.95)),
      "formula_teste <- stats::reformulate(grupo, response = resposta)",
      "resultado <- stats::var.test(formula_teste, data = dados, alternative = alternativa, conf.level = nivel_confianca)",
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
    anova_mista_subamostras = c(
      "# 1. Declarar o que cada coluna representa no estudo.",
      sprintf("variavel_resposta <- %s", texto_r(p$resposta)),
      sprintf("variavel_fator <- %s", texto_r(p$fator)),
      sprintf("variavel_unidade <- %s", texto_r(p$unidade)),
      sprintf("variavel_subamostra <- %s", texto_r(p$subamostra)),
      "",
      "# 2. Manter casos completos sem perder os identificadores.",
      "variaveis_modelo <- c(variavel_resposta, variavel_fator, variavel_unidade, variavel_subamostra)",
      "dados_modelo <- dados[stats::complete.cases(dados[variaveis_modelo]), variaveis_modelo, drop = FALSE]",
      "names(dados_modelo) <- c('resposta', 'fator', 'unidade', 'subamostra')",
      "dados_modelo$fator <- factor(dados_modelo$fator)",
      "dados_modelo$unidade <- factor(dados_modelo$unidade)",
      "",
      "# 3. Caminho simples: uma média por unidade independente.",
      "medias_por_unidade <- stats::aggregate(",
      "  dados_modelo$resposta,",
      "  list(unidade = dados_modelo$unidade, fator = dados_modelo$fator),",
      "  mean",
      ")",
      "names(medias_por_unidade)[3] <- 'resposta_media'",
      "modelo_simples <- stats::aov(resposta_media ~ fator, data = medias_por_unidade)",
      "",
      "# 4. Caminho misto: mantém as subamostras e declara a unidade.",
      "modelo_misto <- nlme::lme(",
      "  resposta ~ fator, random = ~1 | unidade,",
      "  data = dados_modelo, method = 'REML'",
      ")",
      "",
      "# 5. O caminho ingênuo fica somente como aviso didático.",
      "modelo_ingenuo <- stats::aov(resposta ~ fator, data = dados_modelo)",
      "",
      "# 6. Examinar lado a lado; a conclusão vem dos dois caminhos corretos.",
      "summary(modelo_simples)",
      "nlme::anova.lme(modelo_misto)",
      "nlme::VarCorr(modelo_misto)",
      "summary(modelo_ingenuo)"
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
  # Na rota legada de várias análises, o roteiro clássico não pode ensinar
  # um cálculo diferente do método novo que o painel registrou.
  if (identical(execucao$tipo, "anova_um_fator") &&
      !identical(as.character(p$metodo %||% "classica"), "classica")) {
    parametros <- strsplit(exportacao_lista_r(p, 0L), "\n", fixed = TRUE)[[1]]
    codigo <- c(
      "# Esta comunicação reúne análises na rota legada de um QMD.",
      "# Welch e a escolha automática são reproduzidos pelo motor do pacote.",
      "# Para estudar os cálculos linha a linha, exporte esta ANOVA sozinha:",
      "# o molde novo mostra todo o percurso em R/analise.R.",
      "parametros_anova <-", parametros,
      "resultado_anova <- catalyser::catalyser_anova(dados, parametros_anova)",
      "tabela_anova <- resultado_anova$tabela",
      "comparacoes_anova <- resultado_anova$comparacoes",
      "metodo_anova <- resultado_anova$metodo_usado",
      "resultado_anova$narrativa"
    )
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
    anova_mista_subamostras = c("anova-mista", p$resposta),
    anova_medidas_repetidas = c("anova-repetidas", p$resposta),
    friedman = c("friedman", p$resposta),
    anova_dois_fatores = c("anova2", p$resposta),
    grafico_linhas = c("linhas", p$y),
    regressao_linear = c("regressao", p$resposta),
    regressao_logistica = c("regressao-logistica", p$resposta),
    regressao_poisson = c("regressao-poisson", p$resposta),
    regressao_binomial_negativa = c("regressao-binomial-negativa", p$resposta),
    teste_t_one_val = c("teste-t", p$variavel),
    teste_t_two_ind = c("teste-t", p$resposta),
    teste_t_paired = c("teste-t-pareado", p$variavel_1),
    qui_quadrado_variancia = c("qui-quadrado-variancia", p$variavel),
    teste_f_variancias = c("teste-f-variancias", p$resposta),
    estatistica_descritiva = c("descritiva", head(p$variaveis, 1)),
    descricao_exploratoria = c(p$analise, p$variavel, p$outra, p$grupo),
    qui_quadrado = c("qui-quadrado", p$var_row),
    mcnemar = c("mcnemar", p$variavel_1),
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
# Figuras de estudo exclusivas do HTML para o teste t independente acompanhado.
exportacao_figuras_estudo_t <- list(
  grafico_caixa = "Exploração: distribuição por grupo",
  grafico_residuos = "Diagnóstico: resíduos versus valores ajustados",
  grafico_qq = "Diagnóstico: normalidade dentro de cada grupo",
  grafico_dispersao = "Diagnóstico: dispersão por grupo",
  grafico_influencia = "Diagnóstico: influência das observações"
)

exportacao_trecho_componente <- function(variavel, execucao_id, componente,
                                         raiz_chunk = NULL, tipo = NULL) {
  rotulo <- as.list(comunicacao_rotulos_saidas)[[componente]] %||% exportacao_figuras_estudo_t[[componente]] %||% componente
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
  rotulo <- as.list(comunicacao_rotulos_saidas)[[componente]] %||% exportacao_figuras_estudo_t[[componente]] %||% componente
  nome <- exportacao_nome_componente(execucao_id, componente, raiz_chunk, tipo)
  c(
    sprintf("### %s", rotulo),
    "",
    exportacao_casca_chunk(nome, opcoes = c("#| results: asis",
      if (isTRUE(tipo %in% c("teste_t_two_ind", "regressao_linear")) &&
          componente %in% c("grafico", names(exportacao_figuras_estudo_t)))
        c("#| fig-width: 6", "#| fig-height: 4"))),
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
exportacao_tipos_com_codigo_vivo <- c(
  "anova_um_fator", "anova_mista_subamostras", "anova_medidas_repetidas", "anova_dois_fatores",
  "friedman", "mcnemar",
  "qui_quadrado_variancia", "teste_f_variancias", "grafico_linhas", "descricao_exploratoria",
  "proporcao_uma", "proporcao_duas", "qui_quadrado_aderencia",
  "regressao_poisson", "regressao_binomial_negativa"
)

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
  itens <- manifesto$execucoes %||% list()
  inc <- exportacao_execucoes_incluidas(manifesto)
  # Como os seletores de regressão e teste t: uma única execução, incluída no
  # Word, do tipo migrado. O isTRUE() é redundante (length(inc) == 1L já exige
  # a marcação), mas mantém os três seletores com a mesma forma de ler.
  length(itens) == 1L && length(inc) == 1L &&
    isTRUE(itens[[1]]$incluir_word) &&
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

# ---- Molde do Projeto R: um gerador único guiado por registro ---------------
# Cada análise migrada para a árvore nova (R/analise.R como fonte da verdade,
# dois QMDs que o executam, _quarto.yml, apa.csl e saida/) é uma entrada de
# molde_projeto_registro. Adicionar uma análise são dois passos: criar a pasta
# de templates e registrar a entrada com seletor, prefixo e tabelas de
# marcadores. O gerador único cuida do resto; análises não migradas seguem o
# exportador geral, sem alteração.

# Substitui tanto valores curtos dentro de uma linha quanto blocos inteiros
# marcados por uma linha {{CHAVE}}. Isso mantém os QMDs legíveis como templates.
exportacao_preencher_template <- function(linhas, valores) {
  for (chave in names(valores)) {
    marca <- paste0("{{", chave, "}}")
    valor <- as.character(valores[[chave]] %||% "")
    posicoes <- which(trimws(linhas) == marca)
    if (length(posicoes)) {
      for (i in rev(posicoes)) {
        antes <- if (i > 1L) linhas[seq_len(i - 1L)] else character()
        depois <- if (i < length(linhas)) linhas[seq.int(i + 1L, length(linhas))] else character()
        linhas <- c(antes, valor, depois)
      }
    }
    linhas <- gsub(marca, paste(valor, collapse = " "), linhas, fixed = TRUE)
  }
  linhas
}

# Os trechos gerados nascem com marcadores "## ---- nome ----", da rota legada.
# No molde eles entram DENTRO das seções numeradas do template; o marcador
# perde o sentido e sai, ficando só o corpo comentado.
exportacao_sem_marcador <- function(linhas) {
  linhas[!grepl("^## ---- ", linhas)]
}

# Os blocos gerados herdam títulos internos numerados da rota legada
# ("# 2. Operações estruturais", "# 4. Conferência"). Dentro das seções do
# molde essa numeração colide com a do roteiro; aqui ela sai, fica o título.
exportacao_preparo_sem_numeracao <- function(linhas) {
  sub("^(# )([0-9]+)(\\. )", "\\1", linhas)
}

# No molde, o template já apresenta cada passo com uma frase; as frases
# equivalentes dos trechos legados saem, para o aluno não ler a explicação
# duas vezes (uma no template, outra no bloco gerado).
exportacao_molde_importar <- function(import_info = list()) {
  linhas <- exportacao_sem_marcador(exportacao_trecho_importar(import_info))
  linhas[!grepl("^# Lê a planilha como ela veio", linhas)]
}

# Seção 3 do molde, etapas 3.1 e 3.2: reconstrói o preparo a partir da
# planilha bruta e confere o resultado contra a fotografia que acompanha o
# projeto. Um objeto por etapa: base_reconstruida (a receita) e, na etapa
# 3.3, a base adotada, lida uma única vez de cada RDS.
exportacao_molde_tratar <- function(pipeline, base_externa = NULL,
                                    import_info = list()) {
  linhas <- exportacao_preparo_sem_numeracao(exportacao_sem_marcador(
    exportacao_preparo_sem_funcao_data(
      exportacao_trecho_tratar(pipeline, base_externa, import_info = import_info))))
  inicio <- grep("^# Transforma a planilha na Base Compartilhada", linhas)
  if (length(inicio) && length(linhas) >= inicio[1] + 2L &&
      grepl("^# CatalyseR: primeiro as operações estruturais", linhas[inicio[1] + 1L]) &&
      grepl("^# ordem lógica registrada", linhas[inicio[1] + 2L])) {
    linhas <- linhas[-(inicio[1]:(inicio[1] + 2L))]
  }
  corte <- grep("^# Conferência", linhas)
  if (length(corte) != 1L) stop("Não localizei a conferência no trecho tratar.", call. = FALSE)
  cadeia <- linhas[seq_len(corte[1] - 1L)]
  cadeia <- cadeia[nzchar(trimws(cadeia))]
  # A régua que abria o bloco legado de conferência fica fora da etapa 3.1.
  cadeia <- cadeia[!grepl("^# -+$", cadeia)]
  # As bibliotecas que o bloco estrutural coleta são carregadas na seção 1.
  cadeia <- cadeia[!grepl("^library\\((dplyr|tidyr)\\)$", trimws(cadeia))]
  # O encadeamento termina em "dados_analise <- dados_brutos" (sem preparo
  # adicional) ou em "... |>" (com os passos da receita).
  sem_preparo <- !any(grepl("\\|>$", cadeia))
  usa_base_resolvida <- any(grepl("base_resolvida", cadeia))
  cadeia <- sub("^dados_analise <- ", "base_reconstruida <- ", cadeia)
  cadeia <- sub("^# Parte de .*$", "", cadeia)
  cadeia <- cadeia[nzchar(cadeia)]
  c(
    "",
    "# 3.1 Reconstruir. A receita registrada, aplicada à planilha bruta: cada",
    if (usa_base_resolvida) "# operação é uma linha do encadeamento, lida de cima para baixo. (A base" else
      "# operação é uma linha do encadeamento, lida de cima para baixo.",
    if (usa_base_resolvida) "# já vem da fotografia estrutural salva pela CatalyseR.)" else if (sem_preparo)
      "# (Sem preparo adicional, a receita é a própria planilha.)" else NULL,
    cadeia,
    "",
    "# 3.2 Conferir. O projeto traz uma fotografia da base preparada em",
    "# dados/processados/. catalyser_conferir_base() compara a receita com ela",
    "# e avisa se algo divergir; a análise segue com a fotografia em qualquer",
    "# caso.",
    "# O QUE CONFERIR: a mensagem deve dizer que as duas bases são idênticas.",
    "catalyser_conferir_base(",
    "  base_reconstruida,",
    '  here("dados", "processados", "base_compartilhada.rds"),',
    '  rotulo = "Base Compartilhada"',
    ")"
  )
}

# Etapa 3.3: adota a fotografia como base da análise. A única leitura do RDS
# da Base Compartilhada no roteiro; o ramo da etapa 3.4 repete o padrão.
exportacao_molde_adotar <- function() {
  c(
    "# 3.3 Adotar. A análise parte da fotografia, que preserva os tipos das",
    "# colunas e impede que uma mudança silenciosa no preparo entre no Render.",
    "# Para adotar a sua receita, confira-a acima e grave-a na fotografia:",
    '# saveRDS(base_reconstruida, here("dados", "processados", "base_compartilhada.rds"))',
    'dados_analise <- readRDS(here("dados", "processados", "base_compartilhada.rds"))'
  )
}

# Etapa 3.4: a base desta análise. Base compartilhada pura vira um apelido;
# um ramo derivado ganha receita, conferência e adoção próprias — a mesma
# garantia da etapa 3.2, agora para a fotografia do ramo.
exportacao_molde_base_analise <- function(item, raiz, registro_bases) {
  base <- bases_obter(registro_bases, item$base_id)
  if (identical(item$base_tipo, "derivada") && !is.null(base)) {
    receita <- strsplit(
      bases_codigo(base, incluir_print = FALSE), "\n", fixed = TRUE
    )[[1]]
    receita <- exportacao_encadear_preparo(receita, entrada = "dados_analise", saida = base$nome_r)
    receita <- gsub("trat_moda(", "catalyser::catalyser_moda(", receita, fixed = TRUE)
    receita <- receita[!grepl("^library\\((dplyr|tidyr)\\)$", trimws(receita))]
    arquivo <- exportacao_rds_base(item)
    rotulo <- trimws(base$nome_amigavel %||% "")
    if (!nzchar(rotulo)) rotulo <- base$nome_r
    return(c(
      "# 3.4 Base desta análise. A análise parte de um ramo da Base",
      "# Compartilhada: a receita abaixo reconstrói o ramo e a conferência",
      "# repete a garantia da etapa 3.2 para a fotografia do próprio ramo.",
      receita,
      "# Confere o ramo reconstruído contra a fotografia antes de adotá-lo.",
      "catalyser_conferir_base(",
      sprintf("  %s,", base$nome_r),
      sprintf('  here("dados", "processados", "%s"),', arquivo),
      sprintf('  rotulo = "%s"', rotulo),
      ")",
      "# Para adotar uma mudança na receita do ramo, confira-a e grave a fotografia:",
      sprintf('# saveRDS(%s, here("dados", "processados", "%s"))', base$nome_r, arquivo),
      sprintf('dados_da_analise <- readRDS(here("dados", "processados", "%s"))', arquivo)
    ))
  }
  c(
    "# 3.4 Base desta análise. Aqui ela é a própria Base Compartilhada; quando",
    "# a análise parte de um ramo, esta etapa recebe a receita do ramo.",
    "dados_da_analise <- dados_analise"
  )
}

# Bibliotecas da seção 1, derivadas do uso real: o núcleo que os roteiros
# usam sem qualificar (leitura, manipulação e figuras) mais tudo o que o
# escâner encontra no script e em R/funcoes.R. O que o template já declara
# (here, os pacotes fixos de cada análise e os dois pacotes do GitHub, com
# aviso de instalação) não se repete; cada pacote sai uma única vez.
exportacao_molde_bloco_bibliotecas <- function(pacotes, declaradas = character()) {
  nucleo <- c("readxl", "dplyr", "ggplot2")
  lista <- sort(setdiff(unique(c(nucleo, pacotes)),
                        c(declaradas, "catalyser", "EAPADados", "remotes")))
  c(
    "# Bibliotecas da leitura, do preparo e dos gráficos: só o que este",
    "# projeto usa. O README traz a linha de instalação completa.",
    paste0("library(", lista, ")")
  )
}

# Pacotes que o template declara fora do bloco derivado: as chamadas
# library() da seção 1 do próprio template, para não saírem duas vezes.
exportacao_molde_pacotes_declarados <- function(modelo) {
  codigo <- sub("#.*$", "", modelo)
  texto <- paste(codigo, collapse = "\n")
  achados <- unlist(regmatches(texto,
    gregexpr("library\\(\\s*[\"']?([A-Za-z][A-Za-z0-9.]*)", texto, perl = TRUE)),
    use.names = FALSE)
  sub("^library\\(\\s*[\"']?", "", achados)
}

# A conferência fica no pacote catalyser (catalyser_conferir_base), como na
# IDE; as demais chamadas que a rota legada escreve com o pacote local são
# reescritas aqui para as funções equivalentes de R/funcoes.R (moda,
# converter_datas), antes de o script ser gravado no projeto. A rota legada
# continua intacta.
exportacao_sanitizar_molde <- function(linhas) {
  # O comentário de datas vem antes das trocas de código: ele também contém
  # "catalyser::converter_datas(", que a troca seguinte reescreveria primeiro,
  # impedindo a substituição do comentário inteiro.
  linhas <- gsub(
    "# Datas: use catalyser::converter_datas\\(\\); ajuda em \\?catalyser::converter_datas\\.",
    "# Datas: use converter_datas(), definida em R/funcoes.R.",
    linhas
  )
  linhas <- gsub("catalyser::catalyser_moda\\s*\\(", "moda(", linhas)
  linhas <- gsub("catalyser::converter_datas\\s*\\(", "converter_datas(", linhas)
  linhas <- gsub("trat_moda\\s*\\(", "moda(", linhas)
  linhas <- gsub("\\bconverter_data\\s*\\(", "converter_datas(", linhas)
  # Comentários dos trechos compartilhados que citam a IDE: o molde os
  # reaproveita com texto neutro; o exportador legado mantém o original.
  trocas <- c(
    "# A CatalyseR também exportou uma fotografia de `dados_analise`. A função" =
      "# O projeto traz uma fotografia de `dados_analise` em dados/processados/. A função",
    "# Houve mudança estrutural promovida na CatalyseR (Pivotar/Separar ou" =
      "# Houve mudança estrutural (Pivotar/Separar ou",
    "# viu na tela, o script carrega a fotografia materializada na exportação." =
      "# viu na tela, o script carrega a fotografia que acompanha o projeto.",
    "# Escolhas da importação e reestruturações, na ordem registrada na IDE." =
      "# Escolhas da importação e reestruturações, nesta ordem:",
    "# a receita registrada na CatalyseR. Sai dados_da_analise, lido adiante." =
      "# a receita registrada. Sai dados_da_analise, lido adiante."
  )
  for (alvo in names(trocas)) linhas[linhas == alvo] <- trocas[[alvo]]
  linhas
}

# Os dois prefixos do molde devolvem dois blocos, que o gerador único encaixa
# nos marcadores do template: a leitura da planilha (seção 2) e o preparo com
# a conferência e a adoção da base desta análise (seção 3, etapas 3.1 a 3.4).
# A seção 1 é montada depois, a partir dos pacotes que esses blocos usam.

# A regressão reaproveita os trechos homologados do exportador geral, sem o
# cabeçalho nem a instalação, que pertencem ao template e ao README.
exportacao_regressao_projeto_prefixo <- function(manifesto, nome_projeto,
                                                 registro_bases = list(), pipeline = list(),
                                                 base_externa = NULL, import_info = list(),
                                                 templates_dir = "templates") {
  item <- manifesto$execucoes[[1]]
  raizes <- exportacao_raizes_chunk(manifesto$execucoes)
  raiz <- unname(raizes[[item$id]])
  list(
    importar = exportacao_molde_importar(import_info),
    preparo = c(
      exportacao_molde_tratar(pipeline, base_externa, import_info),
      "",
      exportacao_molde_adotar(),
      "",
      exportacao_molde_base_analise(item, raiz, registro_bases)
    )
  )
}

# ANOVA e teste t montam os mesmos dois blocos; as etapas 3.1 a 3.3 são as
# mesmas para toda análise, e só a 3.4 depende da base desta análise.
exportacao_molde_projeto_prefixo_preparo <- function(manifesto, nome_projeto, banner,
                                                     registro_bases = list(),
                                                     pipeline = list(), base_externa = NULL,
                                                     import_info = list()) {
  item <- exportacao_execucoes_incluidas(manifesto)[[1]]
  raizes <- exportacao_raizes_chunk(manifesto$execucoes)
  raiz <- unname(raizes[[item$id]])
  list(
    importar = exportacao_molde_importar(import_info),
    preparo = c(
      exportacao_molde_tratar(pipeline, base_externa, import_info),
      "",
      exportacao_molde_adotar(),
      "",
      exportacao_molde_base_analise(item, raiz, registro_bases)
    )
  )
}

# ---- Geradores únicos --------------------------------------------------------
exportacao_molde_projeto_entrada <- function(manifesto) {
  for (entrada in molde_projeto_registro) {
    if (isTRUE(entrada$seleciona(manifesto))) return(entrada)
  }
  NULL
}

exportacao_molde_projeto_script <- function(entrada, manifesto, nome_projeto,
                                            registro_bases = list(), pipeline = list(),
                                            base_externa = NULL, import_info = list(),
                                            templates_dir = "templates") {
  item <- exportacao_execucoes_incluidas(manifesto)[[1]]
  modelo <- readLines(
    file.path(templates_dir, entrada$pasta, "analise_projeto.R"),
    encoding = "UTF-8", warn = FALSE
  )
  blocos <- entrada$prefixo(manifesto, nome_projeto, registro_bases, pipeline,
                            base_externa, import_info, templates_dir)
  declaradas <- exportacao_molde_pacotes_declarados(modelo)
  # Duas passadas: o corpo nasce com um sentinela no lugar das bibliotecas;
  # o escâner lê o corpo pronto (e R/funcoes.R) e a seção 1 recebe library()
  # só para o que o projeto usa.
  sentinela <- "BIBLIOTECAS_PREPARO_PENDENTES"
  corpo <- exportacao_sanitizar_molde(exportacao_preencher_template(modelo, c(
    entrada$marcadores_script(item),
    list(BIBLIOTECAS_PREPARO = sentinela,
         TRECHO_IMPORTAR = blocos$importar,
         TRECHO_PREPARO = blocos$preparo)
  )))
  funcoes <- readLines(
    file.path(templates_dir, entrada$apoio %||% "regressao_linear", "funcoes.R"),
    encoding = "UTF-8", warn = FALSE
  )
  pacotes <- exportacao_molde_pacotes(c(corpo, funcoes))
  bloco <- exportacao_molde_bloco_bibliotecas(pacotes, declaradas)
  posicao <- which(corpo == sentinela)
  if (length(posicao) != 1L) stop("Sentinela das bibliotecas não localizada.", call. = FALSE)
  posicao <- posicao[1]
  linhas <- c(corpo[seq_len(posicao - 1L)], bloco, corpo[seq.int(posicao + 1L, length(corpo))])
  if (isTRUE(entrada$clara)) linhas <- exportacao_clara_ajustar_script(linhas)
  linhas
}

# Rota ClaRa: as bibliotecas que R/funcoes.R já chama com pacote:: saem da
# seção 1 do roteiro, e a planilha é lida direto de dados/, sem subpasta.
exportacao_clara_ajustar_script <- function(linhas) {
  linhas <- gsub('here("dados", "brutos", ', 'here("dados", ', linhas, fixed = TRUE)
  linhas <- gsub("# Entrada: dados/brutos/", "# Entrada: dados/", linhas, fixed = TRUE)
  linhas[!grepl("^library\\((flextable|lubridate)\\)$", linhas)]
}

exportacao_molde_projeto_qmd <- function(entrada, arquivo, manifesto, titulo_projeto,
                                         import_info = list(), templates_dir = "templates") {
  item <- exportacao_execucoes_incluidas(manifesto)[[1]]
  valores <- c(list(
    TITULO = as.character(item$titulo %||% titulo_projeto),
    ARQUIVO_BRUTO = exportacao_nome_planilha(import_info),
    ARQUIVO_BASE = exportacao_rds_base(item)
  ), entrada$marcadores_qmd(item, manifesto, import_info))
  linhas <- readLines(
    file.path(templates_dir, entrada$pasta, arquivo),
    encoding = "UTF-8", warn = FALSE
  )
  exportacao_preencher_template(linhas, valores)
}

# Pacotes que o projeto usa, lidos do script e das funções de apresentação:
# library(pacote) e pacote::funcao(). A lista alimenta o install.packages()
# do README; comentários não contam e os pacotes que acompanham o R
# (base, recommended) ficam de fora.
exportacao_molde_pacotes <- function(linhas) {
  codigo <- sub("#.*$", "", linhas)
  texto <- paste(codigo, collapse = "\n")
  achados <- unlist(regmatches(texto,
    gregexpr("library\\(\\s*[\"']?([A-Za-z][A-Za-z0-9.]*)", texto, perl = TRUE)),
    use.names = FALSE)
  achados <- sub("^library\\(\\s*[\"']?", "", achados)
  qualificados <- unlist(regmatches(texto,
    gregexpr("([A-Za-z][A-Za-z0-9.]*)::", texto, perl = TRUE)), use.names = FALSE)
  achados <- c(achados, sub(":+$", "", qualificados))
  base <- c("base", "stats", "graphics", "grDevices", "utils", "datasets",
            "methods", "grid", "splines", "stats4", "tools", "parallel",
            "compiler", "tcltk")
  sort(setdiff(unique(achados[nzchar(achados)]), base))
}

# O mesmo vetor, pronto para o README: c("a", "b", ...) quebrado em linhas.
# O marcador fica em linha própria no template, para o bloco manter as quebras.
exportacao_molde_pacotes_texto <- function(pacotes) {
  if (!length(pacotes)) return("character()")
  resto <- paste(sprintf("\"%s\"", pacotes), collapse = ", ")
  corpo <- character()
  while (nchar(resto) > 66) {
    cortes <- gregexpr(", ", resto, fixed = TRUE)[[1]]
    cortes <- cortes[cortes > 0 & cortes < 66]
    if (!length(cortes)) break
    corte <- max(cortes)
    corpo <- c(corpo, substr(resto, 1, corte))
    resto <- trimws(substr(resto, corte + 2, nchar(resto)))
  }
  corpo <- c(corpo, resto)
  saida <- c(
    paste0("c(", corpo[1]),
    if (length(corpo) > 1L) paste0("  ", corpo[-1]) else NULL
  )
  saida[length(saida)] <- paste0(saida[length(saida)], ")")
  saida
}

# Metadados da máquina exportadora; o script registra novamente no Render.
exportacao_ambiente_computacional <- function(pacotes) {
  nomes <- sort(unique(c("catalyser", "EAPADados", pacotes)))
  linhas <- vapply(nomes, function(nome) {
    descricao <- suppressWarnings(utils::packageDescription(nome))
    if (!is.list(descricao)) return(paste("|", nome, "| não instalado | não registrado |"))
    revisao <- descricao$RemoteSha %||% "não registrado"
    paste("|", nome, "|", descricao$Version, "|", revisao, "|")
  }, character(1))
  quarto <- Sys.getenv("QUARTO_PATH", unname(Sys.which("quarto")))
  versao <- if (nzchar(quarto) && file.exists(quarto)) {
    paste(system2(quarto, "--version", stdout = TRUE), collapse = " ")
  } else "não encontrado na exportação"
  c("| Componente | Versão | Revisão GitHub |", "|---|---|---|",
    paste("| R |", getRversion(), "| |"), paste("| Quarto |", versao, "| |"), linhas)
}

exportacao_molde_projeto_readme <- function(entrada, manifesto, nome_projeto,
                                            import_info = list(),
                                            templates_dir = "templates",
                                            pacotes = NULL) {
  item <- exportacao_execucoes_incluidas(manifesto)[[1]]
  linhas <- readLines(
    file.path(templates_dir, entrada$pasta, "README.md"),
    encoding = "UTF-8", warn = FALSE
  )
  exportacao_preencher_template(linhas, c(
    entrada$marcadores_readme(item, nome_projeto, import_info),
    list(PACOTES_INSTALAR = exportacao_molde_pacotes_texto(pacotes),
         AMBIENTE_COMPUTACIONAL = exportacao_ambiente_computacional(pacotes))
  ))
}

# ---- Tabelas de marcadores por análise --------------------------------------
# Cada análise migrada fornece três tabelas: marcadores do script (nomes de
# variáveis, rótulos e confiança que entram em R/analise.R), marcadores dos
# QMDs (seções autorais, com sugestões padrão quando vazias) e marcadores do
# README. Os textos padrão de cada tipo moram em exportacao_textos_<tipo>.
exportacao_regressao_marcadores_script <- function(item) {
  p <- item$parametros
  rotulo <- function(x, padrao) {
    x <- as.character(x %||% "")
    if (nzchar(trimws(x))) x else padrao
  }
  resposta <- rotulo(p$rotulo_resposta, p$resposta)
  preditor <- rotulo(p$rotulo_preditor, p$preditor)
  grupo <- as.character(p$grupo %||% "none")
  grupo_r <- if (identical(grupo, "none")) "NA_character_" else encodeString(grupo, quote = '"')
  rotulo_grupo <- if (identical(grupo, "none")) "Grupo" else grupo
  pergunta <- sprintf("como %s varia, em média, em função de %s?", resposta, preditor)
  list(
    TITULO_COMENTARIO = toupper(as.character(item$titulo %||% "REGRESSÃO LINEAR SIMPLES")),
    PERGUNTA_COMENTARIO = pergunta,
    RESPOSTA_R = encodeString(p$resposta, quote = '"'),
    PREDITOR_R = encodeString(p$preditor, quote = '"'),
    GRUPO_R = grupo_r,
    RETAS_POR_GRUPO = if (isTRUE(p$regressao_por_grupo)) "TRUE" else "FALSE",
    ROTULO_RESPOSTA_R = encodeString(resposta, quote = '"'),
    ROTULO_PREDITOR_R = encodeString(preditor, quote = '"'),
    ROTULO_GRUPO_R = encodeString(rotulo_grupo, quote = '"'),
    CONFIANCA = format(p$nivel_confianca %||% .95, digits = 15, decimal.mark = "."),
    EQUACAO = if (isFALSE(p$mostrar_equacao)) "FALSE" else "TRUE",
    AUTOCORRELACAO = if (isTRUE(p$avaliar_autocorrelacao)) "TRUE" else "FALSE",
    TITULO_R = encodeString(as.character(p$titulo_personalizado %||% ""), quote = '"')
  )
}

exportacao_regressao_marcadores_qmd <- function(item, manifesto, import_info) {
  globais <- manifesto$secoes_globais %||% list()
  sugestoes <- exportacao_textos_regressao(item, import_info)
  secao <- function(nome, padrao) {
    texto <- globais[[nome]] %||% ""
    if (nzchar(trimws(texto))) texto else padrao
  }
  barbo <- identical(import_info$source, "package") &&
    identical(import_info$package_dataset, "morfometria_barbo")
  conclusao_padrao <- c(
    "*Sugestão de escrita: responda à pergunta na escala observada e revise a síntese depois de examinar os diagnósticos.*",
    "",
    if (barbo) paste(
      "As medidas do barbo já estavam corrigidas pelo comprimento padrão.",
      "A associação descreve covariação de forma nesta amostra; não demonstra",
      "causalidade nem representa crescimento individual."
    ) else paste(
      "A associação estimada descreve a variação média observada na faixa dos dados.",
      "A interpretação causal, a extrapolação e a generalização dependem do",
      "delineamento e do contexto científico."
    )
  )
  list(
    INTRODUCAO = secao("introducao", sugestoes$introducao),
    METODOS = secao("metodos", sugestoes$metodos),
    DISCUSSAO = secao("discussao", sugestoes$discussao),
    CONCLUSAO = secao("conclusao", conclusao_padrao)
  )
}

exportacao_regressao_marcadores_readme <- function(item, nome_projeto, import_info) {
  p <- item$parametros
  grupo <- as.character(p$grupo %||% "none")
  list(
    TITULO = as.character(item$titulo %||% nome_projeto),
    PROJETO_RPROJ = paste0(nome_projeto, ".Rproj"),
    ARQUIVO_BRUTO = exportacao_nome_planilha(import_info),
    RESPOSTA = as.character(p$rotulo_resposta %||% p$resposta),
    PREDITOR = as.character(p$rotulo_preditor %||% p$preditor),
    GRUPO = if (identical(grupo, "none")) "nenhum" else grupo,
    IC = format(100 * (p$nivel_confianca %||% .95), trim = TRUE, decimal.mark = ",")
  )
}

exportacao_anova_marcadores_script <- function(item) {
  p <- item$parametros
  rotulo <- function(x, padrao) {
    x <- as.character(x %||% "")
    if (nzchar(trimws(x))) x else padrao
  }
  resposta <- as.character(p$resposta %||% "resposta")
  fator <- as.character(p$fator %||% "grupo")
  rotulo_resposta <- rotulo(p$rotulo_y, resposta)
  rotulo_fator <- rotulo(p$rotulo_x, fator)
  list(
    TITULO_COMENTARIO = toupper(as.character(item$titulo %||% "ANOVA DE UM FATOR")),
    PERGUNTA_COMENTARIO = sprintf("a média de %s difere entre os grupos de %s?", rotulo_resposta, rotulo_fator),
    RESPOSTA_R = encodeString(resposta, quote = '"'),
    FATOR_R = encodeString(fator, quote = '"'),
    ROTULO_RESPOSTA_R = encodeString(rotulo_resposta, quote = '"'),
    ROTULO_FATOR_R = encodeString(rotulo_fator, quote = '"'),
    METODO_R = encodeString(as.character(p$metodo %||% "classica"), quote = '"'),
    CONFIANCA = format(p$nivel_confianca %||% .95, digits = 15, decimal.mark = "."),
    TITULO_R = encodeString(as.character(p$titulo_grafico %||% ""), quote = '"')
  )
}

exportacao_anova_marcadores_qmd <- function(item, manifesto, import_info) {
  globais <- manifesto$secoes_globais %||% list()
  sugestoes <- exportacao_textos_anova(item)
  secao <- function(nome, padrao) {
    texto <- paste(as.character(globais[[nome]] %||% ""), collapse = "\n")
    if (nzchar(trimws(texto))) texto else padrao
  }
  list(
    INTRODUCAO = secao("introducao", sugestoes$introducao),
    METODOS = secao("metodos", sugestoes$metodos),
    DISCUSSAO = secao("discussao", sugestoes$discussao),
    CONCLUSAO = secao("conclusao", sugestoes$conclusao)
  )
}

exportacao_anova_marcadores_readme <- function(item, nome_projeto, import_info) {
  p <- item$parametros
  rotulo <- function(x, padrao) {
    x <- as.character(x %||% "")
    if (nzchar(trimws(x))) x else padrao
  }
  list(
    TITULO = as.character(item$titulo %||% nome_projeto),
    PROJETO_RPROJ = paste0(nome_projeto, ".Rproj"),
    ARQUIVO_BRUTO = exportacao_nome_planilha(import_info),
    RESPOSTA = rotulo(p$rotulo_y, as.character(p$resposta %||% "resposta")),
    FATOR = rotulo(p$rotulo_x, as.character(p$fator %||% "grupo")),
    IC = format(100 * (p$nivel_confianca %||% .95), trim = TRUE, decimal.mark = ",")
  )
}

# ---- ANOVA em ClaRa (opção experimental) ------------------------------------
# A rota ClaRa escreve R/analise.R e os dois QMDs com as funções da ClaRa
# (templates/clara/), em vez do roteiro passo a passo. Só vale para a ANOVA
# de um fator com o método clássico: a ClaRa ainda não faz a ANOVA de Welch.
# As outras situações continuam no molde da ANOVA, sem mudança.
exportacao_anova_clara_aceita <- function(manifesto) {
  if (!isTRUE(manifesto$codigo_clara) || !isTRUE(exportacao_anova_simples(manifesto))) {
    return(FALSE)
  }
  item <- exportacao_execucoes_incluidas(manifesto)[[1]]
  identical(as.character(item$parametros$metodo %||% "classica"), "classica")
}

# Nome de coluna para uma chamada da ClaRa: sem aspas quando é um nome
# válido no R (peso_g); entre crases quando não é, para a ClaRa poder
# recusá-lo com a mensagem amigável dela.
exportacao_nome_clara <- function(x) {
  x <- as.character(x)
  if (identical(make.names(x), x)) x else paste0("`", gsub("`", "", x, fixed = TRUE), "`")
}

# Título do relatório na rota ClaRa: o que o pesquisador escreveu na tela;
# sem isso, um lugar marcado para preencher. O título automático da tela
# ("peso_g entre grupos de racao") usa os nomes crus das colunas e não serve
# para um documento.
exportacao_anova_clara_titulo <- function(item) {
  titulo <- trimws(as.character(item$parametros$titulo_grafico %||% ""))
  if (nzchar(titulo)) titulo else "Título do trabalho (preencher)"
}

exportacao_anova_clara_marcadores <- function(item) {
  p <- item$parametros
  base <- exportacao_anova_marcadores_script(item)
  titulo_tela <- trimws(as.character(p$titulo_grafico %||% ""))
  base$TITULO_COMENTARIO <- toupper(if (nzchar(titulo_tela)) titulo_tela else "ANOVA de um fator")
  rotulo_fator <- as.character(p$rotulo_x %||% "")
  if (!nzchar(trimws(rotulo_fator))) rotulo_fator <- as.character(p$fator %||% "grupo")
  titulo <- as.character(p$titulo_grafico %||% "")
  ic <- format(100 * (p$nivel_confianca %||% .95), trim = TRUE, decimal.mark = ",")
  c(base, list(
    RESPOSTA_CLARA = exportacao_nome_clara(p$resposta %||% "resposta"),
    FATOR_CLARA = exportacao_nome_clara(p$fator %||% "grupo"),
    ROTULO_FATOR_COL = paste0("`", gsub("`", "", rotulo_fator, fixed = TRUE), "`"),
    IC_COL = paste0("`IC ", ic, "%`"),
    TITULO_CLARA = if (nzchar(trimws(titulo))) encodeString(titulo, quote = '"') else "NULL"
  ))
}

exportacao_anova_clara_marcadores_qmd <- function(item, manifesto, import_info) {
  c(exportacao_anova_marcadores_qmd(item, manifesto, import_info),
    exportacao_anova_clara_marcadores(item),
    list(TRECHO_PREPARO_QMD = manifesto$clara$qmd,
         TITULO_RELATORIO = exportacao_anova_clara_titulo(item)))
}

exportacao_anova_clara_marcadores_readme <- function(item, nome_projeto, import_info) {
  valores <- exportacao_anova_marcadores_readme(item, nome_projeto, import_info)
  titulo_tela <- trimws(as.character(item$parametros$titulo_grafico %||% ""))
  valores$TITULO <- if (nzchar(titulo_tela)) titulo_tela else nome_projeto
  valores
}

# Rota ClaRa: a base nasce da planilha e da receita, sem fotografia. A receita
# é a mesma que exportacao_conferir_preparo_anova() já confere contra a base
# da tela antes do ZIP; aqui ela vira um só encadeamento, da planilha até a
# `base` desta análise (trilha, ramo e grupos como fator na ordem mostrada
# na tela), sem nomes intermediários. Em vez da fotografia, o roteiro leva
# um carimbo: linhas, contagem e média por grupo, tirados da base que a
# CatalyseR mostrou, para o aluno conferir com o R. Só o número de linhas é
# travado com stopifnot(). Devolve os blocos do script e do chunk dos QMDs.
exportacao_clara_receita <- function(manifesto, import_info, pipeline, registro_bases,
                                     base_externa, dados_analise, cache_bases) {
  item <- exportacao_execucoes_incluidas(manifesto)[[1]]
  resposta <- as.character(item$parametros$resposta %||% "resposta")
  fator <- as.character(item$parametros$fator %||% "grupo")
  ramo <- identical(item$base_tipo, "derivada")

  receita <- exportacao_preparo_anova(manifesto, import_info, pipeline, registro_bases, base_externa)
  # dplyr já é carregado na seção 1; sem o prefixo, a receita se lê melhor.
  receita <- receita[!grepl("^library\\((dplyr|tidyr)\\)$", trimws(receita))]
  receita <- gsub("dplyr::", "", receita, fixed = TRUE)
  # Os passos de uma cadeia "destino <- origem |>" são as linhas recuadas logo
  # abaixo dela. Sem passos, a cadeia é só "destino <- origem". NULL quando a
  # receita não tem essa forma (preparo com lógica própria).
  passos_de <- function(cabecalho) {
    i <- which(receita == paste(cabecalho, "|>"))
    if (length(i) != 1L) return(if (any(receita == cabecalho)) character() else NULL)
    fim <- i
    while (fim < length(receita) && grepl("^  ", receita[fim + 1L])) fim <- fim + 1L
    receita[seq.int(i + 1L, fim)]
  }
  # Encadeia blocos de passos, pondo o pipe no fim de cada bloco menos o último.
  encadear <- function(blocos) {
    blocos <- Filter(length, blocos)
    unlist(lapply(seq_along(blocos), function(k) {
      bloco <- blocos[[k]]
      if (k < length(blocos)) bloco[length(bloco)] <- paste0(bloco[length(bloco)], " |>")
      bloco
    }), use.names = FALSE)
  }
  compartilhada <- passos_de("base_compartilhada <- dados_brutos")
  especificos <- if (ramo) passos_de("dados <- base_compartilhada") else character()
  # Linhas de código soltas, fora das duas cadeias (ex.: cols_medida <- c(...)).
  cabecalhos <- c("base_compartilhada <- dados_brutos", "base_compartilhada <- dados_brutos |>",
                  "dados <- base_compartilhada", "dados <- base_compartilhada |>")
  soltas <- receita[nzchar(trimws(receita)) & !grepl("^\\s*#", receita) &
                      !grepl("^  ", receita) & !receita %in% cabecalhos]
  cadeia_unica <- !is.null(compartilhada) && !is.null(especificos) &&
    all(grepl("^cols_medida <- c\\(", soltas))

  col_fator <- exportacao_nome_clara(fator)
  col_resposta <- exportacao_nome_clara(resposta)
  base_tela <- as.data.frame(if (ramo) cache_bases[[item$base_id]]$df else dados_analise)
  grupos <- base_tela[[fator]]
  niveis <- if (is.factor(grupos)) levels(grupos) else sort(unique(as.character(stats::na.omit(grupos))))
  grupos <- factor(as.character(grupos), levels = niveis)
  contagem <- as.vector(table(grupos))
  medias <- as.vector(tapply(base_tela[[resposta]], grupos, mean, na.rm = TRUE))
  numero <- function(x) ifelse(is.finite(x), formatC(x, format = "fg", digits = 4, decimal.mark = ","), "sem dados")
  niveis_r <- paste0("c(", paste(encodeString(niveis, quote = '"'), collapse = ", "), ")")

  passo_fator <- c(
    "  # Os grupos como fator, na ordem em que a CatalyseR os mostrou.",
    sprintf("  mutate(%s = factor(%s, levels = %s))", col_fator, col_fator, niveis_r)
  )
  if (cadeia_unica) {
    if (ramo && length(especificos)) {
      registro <- bases_obter(registro_bases, item$base_id)
      nome_ramo <- trimws(registro$nome_amigavel %||% registro$nome_r %||% "")
      especificos <- c(sprintf("  # Ramo%s: o preparo específico desta análise.",
                               if (nzchar(nome_ramo)) paste0(" \"", nome_ramo, "\"") else ""),
                       especificos)
    }
    receita_final <- c(soltas, "base <- dados_brutos |>",
                       encadear(list(compartilhada, especificos, passo_fator)))
  } else {
    # Preparo com lógica própria: a receita fica como veio e a base sai no fim.
    while (length(receita) && !nzchar(trimws(receita[length(receita)]))) receita <- receita[-length(receita)]
    receita_final <- c(receita, "",
                       sprintf("base <- %s |>", if (ramo) "dados" else "base_compartilhada"),
                       passo_fator)
  }
  travar <- sprintf("stopifnot(nrow(base) == %dL)", nrow(base_tela))
  carimbo <- c(
    "# 3.2 Conferir com a tela. O carimbo é o que a CatalyseR mostrou ao exportar.",
    "# O QUE CONFERIR: a tabela calculada pelo R deve repetir o carimbo.",
    sprintf("# Carimbo: %d linhas.", nrow(base_tela)),
    strwrap(paste0("Contagem por ", fator, ": ",
                   paste(niveis, contagem, collapse = "; "), "."),
            width = 76, initial = "# ", prefix = "#   "),
    strwrap(paste0("Média de ", resposta, ": ",
                   paste(niveis, numero(medias), collapse = "; "), "."),
            width = 76, initial = "# ", prefix = "#   "),
    "base |>",
    sprintf("  group_by(%s) |>", col_fator),
    sprintf("  summarise(n = n(), media = mean(%s, na.rm = TRUE))", col_resposta),
    "",
    "# Se a receita mudar o número de linhas, o Render para aqui.",
    travar
  )
  script <- c(
    "# 3.1 A receita. Da planilha à base desta análise, num só encadeamento:",
    "# cada passo é uma linha, lida de cima para baixo.",
    receita_final, "", carimbo
  )

  # Nos QMDs, a mesma leitura e a mesma receita, sem os comentários.
  sem_comentario <- function(x) x[nzchar(trimws(x)) & !grepl("^\\s*#", x)]
  qmd <- c(
    "# A planilha e a receita de R/analise.R (seções 2 e 3).",
    sprintf('dados_brutos <- as.data.frame(read_excel(here("dados", "%s"), sheet = "%s"))',
            exportacao_nome_planilha(import_info), exportacao_aba_planilha(import_info)),
    sem_comentario(receita_final),
    travar
  )
  list(script = script, qmd = exportacao_sanitizar_molde(qmd))
}

exportacao_teste_t_marcadores_script <- function(item) {
  p <- item$parametros
  rotulo <- function(x, padrao) {
    x <- as.character(x %||% "")
    if (nzchar(trimws(x))) x else padrao
  }
  resposta <- as.character(p$resposta %||% "resposta")
  grupo <- as.character(p$grupo %||% "grupo")
  rotulo_resposta <- rotulo(p$rotulo_y, resposta)
  rotulo_grupo <- rotulo(p$rotulo_x, grupo)
  list(
    TITULO_COMENTARIO = toupper(as.character(item$titulo %||% "TESTE T DE DUAS AMOSTRAS")),
    PERGUNTA_COMENTARIO = sprintf("a média de %s difere entre os dois grupos de %s?", rotulo_resposta, rotulo_grupo),
    RESPOSTA_R = encodeString(resposta, quote = '"'),
    GRUPO_R = encodeString(grupo, quote = '"'),
    ROTULO_RESPOSTA_R = encodeString(rotulo_resposta, quote = '"'),
    ROTULO_GRUPO_R = encodeString(rotulo_grupo, quote = '"'),
    CONFIANCA = format(p$nivel_confianca %||% .95, digits = 15, decimal.mark = "."),
    ALTERNATIVA_R = encodeString(p$alternativa %||% "two.sided", quote = '"'),
    VARIANCIAS_IGUAIS = if (isTRUE(p$variancias_iguais)) "TRUE" else "FALSE",
    TITULO_R = encodeString(as.character(p$titulo_grafico %||% ""), quote = '"')
  )
}

exportacao_teste_t_marcadores_qmd <- function(item, manifesto, import_info) {
  globais <- manifesto$secoes_globais %||% list()
  sugestoes <- exportacao_textos_teste_t(item)
  secao <- function(nome, padrao) {
    texto <- paste(as.character(globais[[nome]] %||% ""), collapse = "\n")
    if (nzchar(trimws(texto))) texto else padrao
  }
  list(
    INTRODUCAO = secao("introducao", sugestoes$introducao),
    METODOS = secao("metodos", sugestoes$metodos),
    DISCUSSAO = secao("discussao", sugestoes$discussao),
    CONCLUSAO = secao("conclusao", sugestoes$conclusao)
  )
}

exportacao_teste_t_marcadores_readme <- function(item, nome_projeto, import_info) {
  p <- item$parametros
  rotulo <- function(x, padrao) {
    x <- as.character(x %||% "")
    if (nzchar(trimws(x))) x else padrao
  }
  list(
    TITULO = as.character(item$titulo %||% nome_projeto),
    PROJETO_RPROJ = paste0(nome_projeto, ".Rproj"),
    ARQUIVO_BRUTO = exportacao_nome_planilha(import_info),
    RESPOSTA = rotulo(p$rotulo_y, p$resposta),
    GRUPO = rotulo(p$rotulo_x, p$grupo),
    IC = format(100 * (p$nivel_confianca %||% .95), trim = TRUE, decimal.mark = ",")
  )
}

# Um teste t de duas amostras independentes usa a árvore nova quando está
# sozinho no projeto. Testes t acompanhados de outras análises continuam no
# exportador geral.
exportacao_teste_t_simples <- function(item) {
  identical(item$tipo, "teste_t_two_ind")
}

# ---- Registro do molde ------------------------------------------------------
# Uma entrada por análise migrada: seletor (quando o manifesto usa a árvore
# nova), pasta de templates, pasta dos arquivos de apoio, prefixo do script e
# as três tabelas de marcadores. Adicionar uma análise = criar a pasta de
# templates e registrar uma entrada aqui.
# Os desenhos curtos compartilham a apresentação, mas cada script explica seu teste.
exportacao_t_degrau_marcadores_script <- function(item) {
  p <- item$parametros
  pareado <- identical(item$tipo, "teste_t_paired")
  v1 <- as.character(if (pareado) p$variavel_1 else p$variavel)
  v2 <- as.character(p$variavel_2 %||% "")
  list(TITULO_COMENTARIO = toupper(as.character(item$titulo)),
    VARIAVEL_1_R = encodeString(v1, quote = '"'),
    VARIAVEL_2_R = encodeString(v2, quote = '"'),
    ROTULO_1_R = encodeString(as.character(p$rotulo_1 %||% v1), quote = '"'),
    ROTULO_2_R = encodeString(as.character(p$rotulo_2 %||% v2), quote = '"'),
    MU0 = format(as.numeric(p$media_hipotetica %||% 0), digits = 15, decimal.mark = "."),
    CONFIANCA = format(p$nivel_confianca %||% .95, digits = 15, decimal.mark = "."),
    ALTERNATIVA_R = encodeString(p$alternativa %||% "two.sided", quote = '"'))
}

exportacao_t_degrau_marcadores_qmd <- function(item, manifesto, import_info) {
  globais <- manifesto$secoes_globais %||% list()
  pareado <- identical(item$tipo, "teste_t_paired")
  secao <- function(nome, padrao) {
    texto <- paste(as.character(globais[[nome]] %||% ""), collapse = "\n")
    if (nzchar(trimws(texto))) texto else padrao
  }
  list(
    INTRODUCAO = secao("introducao", c(
      "*Complete a pergunta biológica e acrescente referências do organismo estudado.*",
      if (pareado) "Duas medidas da mesma unidade permitem estudar a diferença entre elas. O teste t pareado analisa a média dessas diferenças."
      else "O teste t de uma amostra compara a média de uma resposta numérica com um valor de referência definido antes da análise.")),
    METODOS = secao("metodos", c(
      "*Descreva origem, local, período, unidades e delineamento. Verifique independência e, no pareado, a correspondência real das medidas.*",
      "O teste t foi executado em R [@rcore2025], com a alternativa e a confiança registradas na CatalyseR. Os casos incompletos foram excluídos apenas nas medidas necessárias. O efeito padronizado usa o DP da resposta, na amostra única, ou o DP das diferenças, no pareado. O IC do efeito é bilateral por t não central. A interpretação exige examinar normalidade e delineamento [@zar2010].")),
    DISCUSSAO = secao("discussao", "*Compare a magnitude e o IC com a questão biológica. Um p-valor não mede importância prática e não demonstra causalidade. Acrescente estudos do seu tema.*"),
    CONCLUSAO = secao("conclusao", "*Responda à pergunta com a estimativa, sua incerteza e os limites do delineamento. A síntese automática precisa da revisão do pesquisador.*")
  )
}

exportacao_t_degrau_marcadores_readme <- function(item, nome_projeto, import_info) {
  list(TITULO = as.character(item$titulo %||% nome_projeto),
    PROJETO_RPROJ = paste0(nome_projeto, ".Rproj"),
    ARQUIVO_BRUTO = exportacao_nome_planilha(import_info))
}

molde_projeto_registro <- list(
  teste_t_one_val = list(
    tipo = "teste_t_one_val", pasta = "teste_t_uma_amostra", apoio = "regressao_linear",
    seleciona = function(manifesto) {
      itens <- manifesto$execucoes %||% list()
      length(itens) == 1L && isTRUE(itens[[1]]$incluir_word) &&
        identical(itens[[1]]$tipo, "teste_t_one_val")
    },
    prefixo = function(manifesto, nome_projeto, registro_bases, pipeline,
                       base_externa, import_info, templates_dir) {
      exportacao_molde_projeto_prefixo_preparo(manifesto, nome_projeto,
        "# TESTE T ", registro_bases, pipeline, base_externa, import_info)
    },
    marcadores_script = exportacao_t_degrau_marcadores_script,
    marcadores_qmd = exportacao_t_degrau_marcadores_qmd,
    marcadores_readme = exportacao_t_degrau_marcadores_readme
  ),
  teste_t_paired = list(
    tipo = "teste_t_paired", pasta = "teste_t_pareado", apoio = "regressao_linear",
    seleciona = function(manifesto) {
      itens <- manifesto$execucoes %||% list()
      length(itens) == 1L && isTRUE(itens[[1]]$incluir_word) &&
        identical(itens[[1]]$tipo, "teste_t_paired")
    },
    prefixo = function(manifesto, nome_projeto, registro_bases, pipeline,
                       base_externa, import_info, templates_dir) {
      exportacao_molde_projeto_prefixo_preparo(manifesto, nome_projeto,
        "# TESTE T ", registro_bases, pipeline, base_externa, import_info)
    },
    marcadores_script = exportacao_t_degrau_marcadores_script,
    marcadores_qmd = exportacao_t_degrau_marcadores_qmd,
    marcadores_readme = exportacao_t_degrau_marcadores_readme
  ),

  regressao_linear = list(
    tipo = "regressao_linear",
    pasta = "regressao_linear",
    apoio = "regressao_linear",
    seleciona = function(manifesto) {
      itens <- manifesto$execucoes %||% list()
      length(itens) == 1L &&
        isTRUE(itens[[1]]$incluir_word) &&
        identical(itens[[1]]$tipo, "regressao_linear")
    },
    prefixo = exportacao_regressao_projeto_prefixo,
    marcadores_script = exportacao_regressao_marcadores_script,
    marcadores_qmd = exportacao_regressao_marcadores_qmd,
    marcadores_readme = exportacao_regressao_marcadores_readme
  ),
  # Vem antes da entrada da ANOVA: quando o pesquisador escolhe a ClaRa e a
  # análise é a ANOVA clássica, esta entrada é a primeira a aceitar.
  anova_clara = list(
    tipo = "anova_um_fator",
    pasta = "anova_clara",
    apoio = "regressao_linear",
    clara = TRUE,
    # Um só QMD: o relatório Word. O caderno de estudo é o próprio R/analise.R.
    documentos = c(relatorio.qmd = "relatorio.qmd"),
    seleciona = exportacao_anova_clara_aceita,
    # A receita e o carimbo já vêm prontos no manifesto (exportacao_clara_receita).
    prefixo = function(manifesto, nome_projeto, registro_bases, pipeline,
                       base_externa, import_info, templates_dir) {
      list(importar = exportacao_molde_importar(import_info),
           preparo = manifesto$clara$script)
    },
    marcadores_script = exportacao_anova_clara_marcadores,
    marcadores_qmd = exportacao_anova_clara_marcadores_qmd,
    marcadores_readme = exportacao_anova_clara_marcadores_readme
  ),
  anova_um_fator = list(
    tipo = "anova_um_fator",
    pasta = "anova_projeto",
    apoio = "regressao_linear",
    seleciona = exportacao_anova_simples,
    prefixo = function(manifesto, nome_projeto, registro_bases, pipeline,
                       base_externa, import_info, templates_dir) {
      exportacao_molde_projeto_prefixo_preparo(manifesto, nome_projeto,
        "# ANOVA DE UM FATOR — ", registro_bases, pipeline, base_externa,
        import_info)
    },
    marcadores_script = exportacao_anova_marcadores_script,
    marcadores_qmd = exportacao_anova_marcadores_qmd,
    marcadores_readme = exportacao_anova_marcadores_readme
  ),
  teste_t_two_ind = list(
    tipo = "teste_t_two_ind",
    pasta = "teste_t_duas_amostras",
    apoio = "regressao_linear",
    seleciona = function(manifesto) {
      itens <- manifesto$execucoes %||% list()
      length(itens) == 1L &&
        isTRUE(itens[[1]]$incluir_word) &&
        exportacao_teste_t_simples(itens[[1]])
    },
    prefixo = function(manifesto, nome_projeto, registro_bases, pipeline,
                       base_externa, import_info, templates_dir) {
      exportacao_molde_projeto_prefixo_preparo(manifesto, nome_projeto,
        "# TESTE T DE DUAS AMOSTRAS — ", registro_bases, pipeline, base_externa,
        import_info)
    },
    marcadores_script = exportacao_teste_t_marcadores_script,
    marcadores_qmd = exportacao_teste_t_marcadores_qmd,
    marcadores_readme = exportacao_teste_t_marcadores_readme
  )
)

# Procedimentos por execução no relatório integrado. O texto descreve as
# escolhas registradas, sem inferir delineamento ou resultados pela planilha.
exportacao_metodos_por_analise <- function(manifesto) {
  incluidas <- Filter(function(x) isTRUE(x$incluir_word), manifesto$execucoes %||% list())
  linhas <- character()
  for (item in incluidas) {
    p <- item$parametros %||% list()
    conf <- p$nivel_confianca %||% .95
    percentual <- format(100 * conf, trim = TRUE, decimal.mark = ",")
    alfa <- format(1 - conf, trim = TRUE, decimal.mark = ",")
    texto <- NULL
    if (identical(item$tipo, "teste_t_two_ind")) {
      metodo <- if (isTRUE(p$variancias_iguais)) "de Student, assumindo variâncias iguais" else "de Welch, sem assumir variâncias iguais"
      hipotese <- switch(p$alternativa %||% "two.sided",
        greater = "unilateral (a média do primeiro nível do grupo maior que a do segundo)",
        less = "unilateral (a média do primeiro nível do grupo menor que a do segundo)",
        "bilateral (médias diferentes)")
      texto <- paste0(
        "As médias de `", p$resposta, "` foram comparadas entre os dois níveis de `", p$grupo,
        "` pelo teste t ", metodo, ", com hipótese alternativa ", hipotese,
        ", nível de significância de ", alfa, " e confiança de ", percentual,
        "%. A diferença de médias segue a ordem dos níveis do grupo registrada no script. ",
        "Foram apresentados a estatística t, os graus de liberdade, o p-valor e o intervalo de confiança da diferença. ",
        "A normalidade dentro dos grupos foi examinada pelo teste de Shapiro-Wilk e a igualdade de variâncias pelo teste de Levene, quando calculáveis. ",
        "Levene complementa a avaliação dos pressupostos; o método aplicado segue a escolha registrada. A independência das observações deve ser fundamentada no delineamento.")
    } else if (identical(item$tipo, "regressao_linear") &&
               identical(p$tipo_modelo %||% "linear", "linear")) {
      grupos <- isTRUE(p$regressao_por_grupo) && nzchar(p$grupo %||% "")
      texto <- paste0(
        "A relação entre `", p$resposta, "` (resposta) e `", p$preditor,
        "` (preditor) foi analisada por regressão linear simples, ajustada por mínimos quadrados ordinários",
        if (grupos) paste0(", separadamente para cada nível de `", p$grupo, "`") else " em um ajuste global",
        ". Foram apresentados intercepto, inclinação e R²", if (grupos) " de cada ajuste" else " do ajuste",
        "; a hipótese de inclinação nula foi examinada pelo teste t com nível de significância de ", alfa,
        ". A normalidade dos resíduos foi examinada pelo teste de Shapiro-Wilk, quando calculável",
        if (grupos) ", em cada ajuste" else "",
        ". A interpretação também exige avaliar linearidade, homogeneidade da variância e independência pelo delineamento. ",
        if (grupos) "As retas separadas descrevem as relações em cada grupo; esse procedimento não testa a igualdade das inclinações." else
          "Observações influentes devem ser conferidas no contexto do estudo antes de qualquer exclusão.")
    }
    linhas <- c(linhas, paste0("### ", item$titulo), "",
      if (is.null(texto)) "<!-- Descreva o procedimento desta análise conforme as escolhas do script, seus pressupostos e a pergunta do estudo. -->" else texto, "")
  }
  linhas
}

# Sugestões entram apenas nas seções vazias de um relatório com uma reta.
# Não inferimos local, período, unidade amostral ou causalidade a partir da planilha.
exportacao_textos_regressao <- function(item, import_info = list()) {
  p <- item$parametros
  rotulo <- function(nome, padrao) {
    if (nzchar(trimws(nome %||% ""))) nome else padrao
  }
  resposta <- rotulo(p$rotulo_resposta, p$resposta)
  preditor <- rotulo(p$rotulo_preditor, p$preditor)
  ic <- format(100 * (p$nivel_confianca %||% .95), trim = TRUE, decimal.mark = ",")
  alfa <- format(1 - (p$nivel_confianca %||% .95), trim = TRUE, decimal.mark = ",")
  barbo <- identical(import_info$source, "package") &&
    identical(import_info$package_dataset, "morfometria_barbo")
  list(
    introducao = c(
      "*Sugestão de redação: adapte a pergunta e acrescente referências do seu tema antes de compartilhar o relatório.*", "",
      if (barbo) {
        "A forma corporal dos peixes pode ser descrita pela relação entre suas medidas morfométricas. No conjunto `morfometria_barbo`, do EAPADados, as medidas de *Barbus petenyi* já foram corrigidas alometricamente pelo comprimento padrão. Assim, a pergunta se refere à associação entre medidas de forma corporal, e não à velocidade de crescimento dos peixes. O conjunto é um recorte didático de dados disponibilizados no Mendeley Data [@takacs2022]."
      } else {
        "Na pesca e na aquicultura, estudar a relação entre duas medidas ajuda a formular perguntas sobre os organismos e os sistemas de produção. Uma regressão linear simples descreve como o valor médio de uma resposta varia ao longo de um preditor. A interpretação biológica depende da origem dos dados e de como as observações foram obtidas."
      }, "",
      sprintf("Neste estudo, investigou-se a associação entre %s e %s. O objetivo foi estimar a direção e a magnitude dessa associação, quantificar sua incerteza e avaliar se uma reta representa adequadamente a relação na faixa observada.", resposta, preditor)),
    metodos = c(
      "*Sugestão de redação: complete a origem dos dados, o período, o local, a unidade amostral, as unidades de medida e os critérios de seleção. Não declare independência sem conferir o delineamento.*", "",
      if (barbo) "Utilizou-se o recorte `morfometria_barbo` do EAPADados. A base de origem reúne indivíduos de cinco populações; filtros e exclusões aplicados neste projeto devem ser descritos. As medidas já corrigidas pelo tamanho foram mantidas nessa escala. Uma reta conjunta descreve a associação no conjunto analisado e não separa relações dentro de cada população de diferenças entre populações." else NULL, "",
      sprintf("Ajustou-se uma regressão linear simples por mínimos quadrados ordinários, com %s como resposta e %s como preditor. Foram utilizados os pares completos dessas duas variáveis, com registro das exclusões por dados ausentes. Estimaram-se o intercepto e a inclinação, seus erros padrão e intervalos de confiança de %s%%. A hipótese de inclinação nula foi avaliada pelo teste t, com nível de significância de %s. O ajuste foi descrito por R², R² ajustado e teste F global.", resposta, preditor, ic, alfa), "",
      "A adequação da reta foi examinada por resíduos versus valores ajustados, procurando curvatura e mudanças de dispersão. A normalidade dos erros foi examinada por gráfico Q-Q e teste de Shapiro-Wilk, quando calculável; a constância da variância, pelo teste de Breusch-Pagan. A influência das observações foi investigada pela distância de Cook e pela alavancagem em conjunto com os resíduos padronizados. Esses diagnósticos orientam a investigação dos dados e não constituem regras automáticas de exclusão [@fox2019; @zuur2010].", "",
      if (isTRUE(p$avaliar_autocorrelacao))
        "A autocorrelação foi examinada pelo teste de Durbin-Watson e pelos resíduos na ordem informada. Essa leitura depende de as linhas representarem uma sequência real de coleta, a ser descrita pelo pesquisador; o teste não estabelece independência entre unidades amostrais."
      else "O teste de Durbin-Watson não foi aplicado, pois não foi indicada uma ordem real de coleta. A independência deve ser justificada pela unidade amostral e pelo delineamento, considerando repetições do mesmo indivíduo e agrupamentos por tanque, rio ou população."),
    discussao = c(
      "*Sugestão para desenvolver a discussão: compare a magnitude e o intervalo de confiança da inclinação com estudos do mesmo organismo e na mesma escala. Explique a plausibilidade biológica, os sinais encontrados nos diagnósticos e os limites da amostragem. Inclua as referências consultadas.*", "",
      if (barbo) "Neste exemplo, uma associação entre medidas corrigidas descreve covariação da forma corporal. Ela não demonstra crescimento individual nem um mecanismo causal. Como a base reúne cinco populações, diferenças entre elas podem contribuir para a associação conjunta. A sobreposição de tamanhos não elimina essa possibilidade nem comprova independência; confira a dispersão por população e o delineamento antes de generalizar." else
        "Uma associação estatística, mesmo com R² elevado, não demonstra que alterar o preditor causará uma mudança na resposta. Considere fatores não incluídos no modelo, erros de medição e dependência entre observações. O intervalo ao redor da reta descreve a incerteza da resposta média; ele não representa a dispersão esperada de um peixe individual."),
    conclusao = c(
      "*Sugestão de redação: retome a pergunta da introdução e revise esta síntese depois de examinar os diagnósticos. Acrescente a implicação biológica que o delineamento permite sustentar.*", "",
      "`r texto_conclusao`", "",
      if (barbo) "Para o barbo, a interpretação se restringe às medidas de forma corrigidas pelo tamanho e à composição populacional da amostra analisada." else NULL)
  )
}

# Sugestões entram apenas nas seções que o pesquisador deixou vazias na
# Comunicação. Não inferimos local, período, unidade amostral nem causalidade.
exportacao_textos_anova <- function(item) {
  p <- item$parametros
  rotulo <- function(x, padrao) {
    x <- as.character(x %||% "")
    if (nzchar(trimws(x))) x else padrao
  }
  resposta <- rotulo(p$rotulo_y, as.character(p$resposta %||% "a resposta"))
  fator <- rotulo(p$rotulo_x, as.character(p$fator %||% "o fator"))
  ic <- format(100 * (p$nivel_confianca %||% .95), trim = TRUE, decimal.mark = ",")
  alfa <- format(1 - (p$nivel_confianca %||% .95), trim = TRUE, decimal.mark = ",")
  list(
    introducao = c(
      "*Sugestão de redação: adapte a pergunta e acrescente referências do seu tema antes de compartilhar o relatório.*", "",
      "Na pesca e na aquicultura, comparar grupos é uma das perguntas mais frequentes: rações, densidades, espécies, locais ou épocas. A análise de variância de um fator avalia se a média de uma resposta numérica difere entre os grupos definidos por um fator.", "",
      sprintf("Neste estudo, comparou-se %s entre os grupos de %s. O objetivo foi verificar se as médias diferem, identificar quais grupos se separam e estimar a magnitude dessa diferença.", resposta, fator)),
    metodos = c(
      "*Sugestão de redação: complete a origem dos dados, o período, o local, a unidade experimental, as unidades de medida e os critérios de seleção. Não declare independência sem conferir o delineamento.*", "",
      sprintf("A comparação de %s entre os grupos de %s usou análise de variância de um fator, com as funções de base do R [@rcore2025]. Foram utilizados os casos com resposta e grupo preenchidos. Os intervalos de confiança das médias e das comparações foram de %s%%, e adotou-se nível de significância de %s [@zar2010].", resposta, fator, ic, alfa), "",
      "A homogeneidade das variâncias foi avaliada pelo teste de Levene, do pacote `car` [@fox2019], e a normalidade dos resíduos pelo teste de Shapiro-Wilk, ambos acompanhados dos gráficos de resíduos. A ANOVA clássica usa Tukey; Welch usa Games-Howell. A escolha registrada em `R/analise.R` e sua justificativa aparecem no relatório. As letras resumem os p-valores ajustados no alfa adotado, preservando os nomes dos grupos. As figuras foram construídas com o `ggplot2` [@wickham2016]. Na clássica, o efeito é descrito por η² e ω²; no Welch, usa-se uma conversão aproximada do F em ω², com IC bilateral também aproximado [@effectsizeConversao]."),
    discussao = c(
      "*Sugestão para desenvolver a discussão: comente quais grupos se separam, o tamanho das diferenças e a leitura à luz do fenômeno investigado. Compare com estudos do mesmo organismo e inclua as referências consultadas.*", "",
      "O p-valor expressa a compatibilidade dos dados com a hipótese nula; o tamanho de efeito descreve a magnitude da associação. A importância prática depende do contexto do estudo. O teste descreve diferenças entre os grupos; a atribuição de causa depende do delineamento e de como os dados foram obtidos."),
    conclusao = c(
      "*Sugestão de redação: responda ao objetivo em poucas frases, considerando a magnitude das diferenças, a incerteza e os limites do delineamento. Não acrescente resultados que não tenham sido apresentados.*")
  )
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
  if ("pressupostos" %in% item$saidas_word) linhas <- c(linhas,
    ':::: {.content-visible when-format="html"}', "### Leitura dos pressupostos", "",
    "Um p-valor acima do nível de significância não comprova o pressuposto. Leia os testes junto aos gráficos e ao delineamento.", "",
    exportacao_casca_chunk(paste0(raiz, "-mostrar-pressupostos")), "::::", "")
  linhas <- c(linhas,
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
    "**Influência (complementar).** Cook destaca observações que merecem conferência. A linha 4/n não autoriza excluir dados automaticamente. Os números identificam linhas da base preparada, antes de retirar pares incompletos.", "",
    chunk("diagnostico-influencia", c(
      '#| fig-cap: "Distância de Cook por observação. A linha tracejada marca 4/n, uma referência de triagem e não um teste de hipótese."',
      "#| fig-width: 6", "#| fig-height: 4"), "fig-"), "",
    "**Alavancagem e resíduos.** Um peixe com valor extremo do preditor pode ter grande alavancagem mesmo com resíduo pequeno. Leia este gráfico junto com Cook. As linhas de referência (2p/n, com p parâmetros incluindo o intercepto, e resíduos em ±2) servem apenas para triagem; os rótulos indicam linhas da base que merecem conferência [@nistdiagnosticos].", "",
    chunk("diagnostico-alavancagem", c(
      '#| fig-cap: "Resíduos padronizados versus alavancagem. Rótulos identificam linhas sinalizadas por alavancagem, resíduo ou Cook; não são uma decisão de exclusão."',
      "#| fig-width: 6", "#| fig-height: 4"), "fig-"), "",
    "**Como decidir o próximo passo.** Curvatura pede revisar a forma da relação; um funil pede revisar a variância. Valores sinalizados pedem conferir digitação, medição e contexto, e justificar uma análise de sensibilidade quando necessária. Medidas repetidas ou peixes agrupados podem exigir um modelo que represente essa dependência. Não tente resolver esses problemas apenas acumulando testes [@zuur2010].", "",
    "Na reta simples com intercepto e um preditor, o teste F global e o teste t bilateral da inclinação avaliam a mesma hipótese (F = t²). Não é necessário acrescentar Pearson para confirmar o resultado, nem VIF, que se refere à colinearidade entre múltiplos preditores. Testes adicionais de falta de ajuste dependem do delineamento; não são uma exigência automática deste roteiro.", "::::", "")
  linhas <- c(linhas, "### Resultados finais", "")
  if ("narrativa" %in% item$saidas_word) linhas <- c(linhas,
    exportacao_nota("Sugestão para os Resultados: apresente a associação e sua incerteza após conferir os diagnósticos. Os números abaixo são recalculados no Render; revise a interpretação, sem copiá-los como valores fixos."),
    "`r texto_resultados`", "", "`r texto_pressupostos`", "", "`r alerta_pressupostos`", "")
  if ("tabela" %in% item$saidas_word) linhas <- c(linhas,
    chunk("tabela", '#| tbl-cap: "Coeficientes da regressão linear simples, erros padrão e intervalos de confiança."', "tbl-"),
    chunk("metricas", '#| tbl-cap: "Métricas de ajuste da regressão linear simples."', "tbl-"), "")
  if ("grafico" %in% item$saidas_word) linhas <- c(linhas,
    chunk("grafico", c('#| fig-cap: "Reta ajustada e intervalo de confiança da resposta média. O nível de confiança é definido no roteiro."',
      "#| fig-width: 6", "#| fig-height: 4"), "fig-"), "")

  linhas
}

# Sugestões entram apenas nas seções vazias de um relatório com um único teste t.
# Não inferimos local, período, unidade amostral ou causalidade a partir da planilha.
exportacao_textos_teste_t <- function(item) {
  p <- item$parametros
  rotulo <- function(nome, padrao) {
    if (nzchar(trimws(nome %||% ""))) nome else padrao
  }
  resposta <- rotulo(p$rotulo_y, p$resposta)
  grupo <- rotulo(p$rotulo_x, p$grupo)
  ic <- format(100 * (p$nivel_confianca %||% .95), trim = TRUE, decimal.mark = ",")
  alfa <- format(1 - (p$nivel_confianca %||% .95), trim = TRUE, decimal.mark = ",")
  list(
    introducao = c(
      "*Sugestão de redação: adapte a pergunta e acrescente referências do seu tema antes de compartilhar o relatório.*", "",
      "O teste t para duas amostras independentes compara a média de uma variável numérica entre dois grupos e avalia se a diferença observada escapa ao acaso. É um teste paramétrico: pede normalidade dentro de cada grupo e, na versão clássica, variâncias parecidas entre os grupos.", "",
      sprintf("Neste estudo, comparou-se %s entre os dois grupos de %s. O objetivo foi verificar se as médias diferem e, em caso afirmativo, qual grupo apresenta a maior média, quantificando a incerteza e o tamanho da diferença.", resposta, grupo)),
    metodos = c(
      "*Sugestão de redação: complete a origem dos dados, o período, o local, a unidade amostral, as unidades de medida e os critérios de seleção.*", "",
      sprintf("Compararam-se as médias de %s entre os dois grupos de %s por teste t para amostras independentes, com intervalo de confiança de %s%% e nível de significância de %s. A normalidade dentro de cada grupo foi examinada pelo teste de Shapiro-Wilk e a igualdade de variâncias pelo teste de Levene. Conforme esse resultado, adotou-se o t de Student (variâncias iguais) ou o t de Welch (variâncias diferentes). O tamanho do efeito foi quantificado pelo d de Cohen.", resposta, grupo, ic, alfa), "",
      "A independência das observações depende do delineamento e deve ser justificada pela unidade amostral, considerando repetições e agrupamentos."),
    discussao = c(
      "*Sugestão para desenvolver a discussão: interprete a magnitude da diferença e o tamanho do efeito no contexto do estudo, com as referências consultadas.*", "",
      "Uma diferença estatisticamente significativa indica que as médias dos grupos diferem além do esperado pelo acaso, mas não descreve, sozinha, o mecanismo. Considere o tamanho do efeito ao lado do p-valor: a significância diz que a diferença existe; o tamanho do efeito diz o quanto ela importa."),
    conclusao = c(
      "*Sugestão de redação: retome a pergunta da introdução e revise esta síntese depois de examinar os pressupostos.*", "")
  )
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
      linhas <- c(linhas,
        "# ========================================================================",
        paste0("# REGRESSÃO LINEAR SIMPLES — ", item$titulo),
        "# ========================================================================",
        "# Percurso: conferir a base -> ajustar -> diagnosticar -> apresentar.",
        "# Cada tabela, gráfico e texto tem um nome para ser consultado no R.",
        "",
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
    componentes <- item$saidas_word
    if (identical(item$tipo, "teste_t_two_ind")) componentes <- unique(c(componentes,
      setdiff(names(exportacao_figuras_estudo_t), "grafico_influencia")))
    if (identical(item$tipo, "regressao_linear")) componentes <- unique(c(componentes,
      setdiff(names(exportacao_figuras_estudo_t), "grafico_caixa")))
    for (componente in componentes) {
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
  incluidas <- Filter(function(x) isTRUE(x$incluir_word), manifesto$execucoes %||% list())
  sugestoes <- if (length(incluidas) == 1L && exportacao_regressao_simples(incluidas[[1]]))
    exportacao_textos_regressao(incluidas[[1]], import_info) else list()
  texto_ou_lembrete <- function(texto, lembrete, secao) {
    if (nzchar(trimws(texto %||% ""))) texto else
      sugestoes[[secao]] %||% exportacao_nota(lembrete)
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
      "Escreva aqui a introdução: o contexto, a pergunta e por que ela importa.", "introducao"
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
      "Descreva aqui os métodos: o que cada análise testa e a que nível de significância.", "metodos"
    ),
    "",
    exportacao_metodos_por_analise(manifesto),
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
    # O caderno examina os pressupostos antes de apresentar os resultados.
    componentes_estudo <- intersect(item$saidas_word, c("pressupostos", "diagnosticos"))
    for (componente in componentes_estudo) {
      linhas <- c(linhas, exportacao_chunk_componente(item$id, componente,
        raiz_chunk = raiz, tipo = item$tipo))
    }
    if (item$tipo %in% c("teste_t_two_ind", "regressao_linear")) {
      linhas <- c(linhas, ':::: {.content-visible when-format="html"}', "",
        paste("### Exploração e pressupostos:", item$titulo), "",
        if (identical(item$tipo, "teste_t_two_ind")) "O boxplot permite conferir a distribuição e os pontos de cada grupo. Nos resíduos versus valores ajustados, compare a dispersão nas duas faixas e procure valores extremos. No Q-Q por grupo, desvios da reta podem indicar assimetria ou caudas diferentes da normal. A dispersão dos resíduos absolutos complementa a avaliação de variâncias pelo Levene; ela não escolhe automaticamente Student ou Welch. A independência depende do delineamento." else
        "Os diagnósticos correspondem aos modelos realmente ajustados, separados por categoria quando há retas por grupo. Procure curvatura e funil nos resíduos versus ajustados, desvios da reta no Q-Q e mudanças de dispersão no gráfico de homocedasticidade. Distâncias de Cook elevadas sinalizam observações para investigação, não remoção automática. A independência depende do delineamento.", "")
      figuras <- setdiff(names(exportacao_figuras_estudo_t),
        if (identical(item$tipo, "teste_t_two_ind")) "grafico_influencia" else "grafico_caixa")
      for (componente in figuras) {
        linhas <- c(linhas, exportacao_chunk_componente(item$id, componente, raiz_chunk = raiz, tipo = item$tipo))
      }
      linhas <- c(linhas, "::::", "")
    }
    linhas <- c(linhas, "### Resultados finais", "")
    for (componente in setdiff(item$saidas_word, componentes_estudo)) {
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
      "Escreva aqui a discussão: o que os resultados dizem, comparados ao que se esperava.", "discussao"
    ),
    "",
    "# Conclusão",
    "",
    texto_ou_lembrete(
      globais$conclusao,
      "Escreva aqui a conclusão, em poucas frases, respondendo à pergunta da introdução.", "conclusao"
    ),
    "",
    if (any(vapply(incluidas, exportacao_regressao_simples, logical(1))))
      c("# Referências {.unnumbered}", "", "::: {#refs}", ":::", ""),
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
                                     import_info = list(), templates_dir = "templates",
                                     ficha_planejamento = NULL) {
  manifesto <- exportacao_sem_execucoes_repetidas(manifesto)
  validacao <- exportacao_validar_manifesto(manifesto, exigir_word = FALSE)
  if (!validacao$ok) stop(paste(validacao$mensagens, collapse = " "), call. = FALSE)
  # A rota ClaRa parte da planilha e da receita, sem fotografia. Registros
  # antigos que só se reproduzem pela base salva continuam no molde da ANOVA.
  if (isTRUE(manifesto$codigo_clara) && exportacao_anova_usa_base_resolvida(base_externa)) {
    manifesto$codigo_clara <- FALSE
  }
  # A árvore nova (R/analise.R como fonte da verdade, dois QMDs) é escolhida
  # pela entrada do registro do molde cujo seletor aceita este manifesto.
  molde <- exportacao_molde_projeto_entrada(manifesto)
  if (isTRUE(molde$clara)) {
    manifesto$clara <- exportacao_clara_receita(manifesto, import_info, pipeline,
      registro_bases, base_externa, dados_analise, cache_bases)
  }
  anova_nova <- !is.null(molde) && identical(molde$tipo, "anova_um_fator")

  if (exportacao_anova_simples(manifesto) && !anova_nova) {
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

  nome_projeto <- exportacao_nome_projeto(nome_projeto)
  projeto <- file.path(destino, nome_projeto)
  if (dir.exists(projeto)) {
    stop("O diretório temporário do projeto já existe; gere a exportação novamente.", call. = FALSE)
  }
  dir.create(projeto, recursive = TRUE, showWarnings = FALSE)
  # Imagens recebe fotos e esquemas do pesquisador, também no caminho ANOVA.
  # Na rota ClaRa, dados/ guarda só a planilha: a base nasce da receita e a
  # cópia dela para o Excel é gravada pelo script em saida/tabelas/.
  pastas <- if (isTRUE(molde$clara)) c("dados", "R", "imagens", "relatorios") else
    c(file.path("dados", "brutos"), file.path("dados", "processados"), "R", "imagens", "relatorios")
  # A pasta saida/ nasce na primeira execução do script (seção 2), que cria
  # dados/processados e saida/{tabelas,figuras,relatorios}. O projeto viaja sem
  # ela para o ZIP não carregar pastas vazias nem artefatos de build.
  dirs <- file.path(projeto, pastas)
  vapply(dirs, dir.create, logical(1), recursive = TRUE, showWarnings = FALSE)
  writeLines(
    c(
      "Esta pasta guarda fotos e esquemas do pesquisador (foto do local de",
      "coleta, esquema do delineamento, mapa). Ela não é usada pelo código:",
      "os arquivos entram nos relatórios com markdown, por exemplo:",
      "",
      "    ![](imagens/minha_foto.jpg)",
      "",
      "Nenhum arquivo daqui é alterado pela análise."
    ),
    file.path(projeto, "imagens", "LEIA-ME.txt"), useBytes = TRUE
  )

  # A pasta `dados/` é deliberadamente enxuta:
  #
  #   brutos/      a planilha bruta -> ponto de entrada do chunk importar;
  #   processados/ base_compartilhada.rds -> fotografia, usada apenas para conferência;
  #                base_compartilhada.xlsx -> entrega, para uso fora do R.
  #
  # Nada de cópias redundantes: o que o projeto sabe reconstruir, ele reconstrói.
  pasta_bruta <- if (isTRUE(molde$clara)) "dados" else file.path("dados", "brutos")
  caminho_bruto <- file.path(projeto, pasta_bruta, exportacao_nome_planilha(import_info))
  # Exporta somente os valores da aba importada, antes de qualquer preparo.
  # A pasta de trabalho original e suas outras abas ficam com o pesquisador.
  exportacao_salvar_planilha(dados_brutos, caminho_bruto, aba = exportacao_aba_planilha(import_info))
  # Fotografias e cópias da IDE em dados/processados/; a rota ClaRa não as leva.
  if (!isTRUE(molde$clara)) {
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
  templates <- if (isTRUE(molde$clara)) c(
    # Rota ClaRa: só o Word; o tema do HTML (ocean.scss) não viaja.
    "custom-reference.docx" = file.path("relatorios", "custom-reference.docx"),
    "referencias.bib" = file.path("relatorios", "referencias.bib")
  ) else if (!is.null(molde)) c(
    "custom-reference.docx" = file.path("relatorios", "custom-reference.docx"),
    "ocean.scss" = file.path("relatorios", "ocean.scss"),
    "referencias.bib" = file.path("relatorios", "referencias.bib")
  ) else c(
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
  if (!is.null(molde)) {
    # Os arquivos de apoio (funções de apresentação, estilo APA e _quarto.yml
    # que renderiza os dois QMDs) são os da pasta de apoio da entrada do
    # registro; as três análises migradas compartilham os da regressão.
    apoio <- file.path(templates_dir, molde$apoio %||% "regressao_linear")
    file.copy(
      file.path(apoio, "funcoes.R"),
      file.path(projeto, "R", "funcoes.R"), overwrite = TRUE
    )
    file.copy(
      file.path(apoio, "apa.csl"),
      file.path(projeto, "relatorios", "apa.csl"), overwrite = TRUE
    )
    file.copy(
      file.path(apoio, "_quarto.yml"),
      file.path(projeto, "_quarto.yml"), overwrite = TRUE
    )
    # Uma análise com _quarto.yml próprio (a rota ClaRa, de um só QMD) usa o seu.
    proprio <- file.path(templates_dir, molde$pasta, "_quarto.yml")
    if (file.exists(proprio)) file.copy(proprio, file.path(projeto, "_quarto.yml"), overwrite = TRUE)
  }
  # Rota ClaRa: o projeto leva a sua cópia da ClaRa em R/, ao lado do script.
  if (isTRUE(molde$clara)) {
    arquivos_clara <- list.files(file.path(templates_dir, "clara"),
                                 pattern = "^clara.*[.]R$", full.names = TRUE)
    if (!length(arquivos_clara)) stop("Os arquivos da ClaRa não foram encontrados.", call. = FALSE)
    # A ClaRa fica numa pasta própria, R/clara/: em R/ o aluno vê só o seu
    # roteiro (analise.R) e as funções de apresentação (funcoes.R).
    dir.create(file.path(projeto, "R", "clara"), showWarnings = FALSE)
    file.copy(arquivos_clara, file.path(projeto, "R", "clara"), overwrite = TRUE)
  }

  if (exportacao_anova_simples(manifesto) && !anova_nova) {
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
  if (!is.null(molde)) {
    linhas_script <- exportacao_molde_projeto_script(
      molde, manifesto, nome_projeto, registro_bases, pipeline, base_externa,
      import_info, templates_dir
    )
    writeLines(linhas_script, caminho_script, useBytes = TRUE)
    # Cada entrada diz quais QMDs leva: de template para o projeto. O padrão
    # é o par HTML + Word; a rota ClaRa leva só o relatório Word.
    documentos <- molde$documentos %||% c(relatorio_completo.qmd = "relatorio_completo.qmd",
                                          relatorio_artigo.qmd = "relatorio_artigo.qmd")
    for (arquivo in names(documentos)) {
      writeLines(
        exportacao_molde_projeto_qmd(molde, arquivo, manifesto, titulo, import_info, templates_dir),
        file.path(projeto, "relatorios", documentos[[arquivo]]), useBytes = TRUE
      )
    }
    # A lista exata de pacotes do projeto (script + funções) alimenta o
    # install.packages() do README, para bater com o que o Render vai usar.
    # catalyser e EAPADados ficam de fora: são instalados do GitHub, nas
    # linhas seguintes do README, com remotes.
    funcoes_molde <- file.path(templates_dir, molde$apoio %||% "regressao_linear", "funcoes.R")
    pacotes_projeto <- setdiff(
      exportacao_molde_pacotes(c(
        linhas_script,
        readLines(funcoes_molde, encoding = "UTF-8", warn = FALSE)
      )),
      c("catalyser", "EAPADados", "remotes")
    )
    # A ClaRa carrega os seus pacotes ao rodar cada análise; eles entram na
    # lista de instalação do README. Só do CRAN: a rota ClaRa dispensa remotes.
    if (isTRUE(molde$clara)) {
      pacotes_projeto <- sort(union(pacotes_projeto, c("dplyr", "ggplot2", "broom", "car",
        "multcompView", "effectsize", "rlang", "stringr", "pwr")))
    } else {
      pacotes_projeto <- sort(c(pacotes_projeto, "remotes"))
    }
  } else {
    writeLines(
      exportacao_gerar_script(
        manifesto, nome_projeto, registro_bases = registro_bases,
        pipeline = pipeline, base_externa = base_externa, import_info = import_info,
        templates_dir = templates_dir
      ), caminho_script, useBytes = TRUE
    )
    qmd_integrado <- exportacao_gerar_qmd(
      manifesto, titulo, registro_bases = registro_bases,
      pipeline = pipeline, base_externa = base_externa, import_info = import_info,
      templates_dir = templates_dir
    )
    ligacao <- new.env(parent = baseenv())
    sys.source(file.path(projeto, "R", "funcoes.R"), envir = ligacao)
    for (arquivo in c("relatorio_completo.qmd", "relatorio_artigo.qmd")) {
      caminho_qmd <- file.path(projeto, "relatorios", arquivo)
      writeLines(exportacao_qmd_integrado_formato(qmd_integrado, arquivo), caminho_qmd, useBytes = TRUE)
      suppressMessages(ligacao$atualizar_codigo(qmd = caminho_qmd, script = caminho_script))
      ligacao$conferir_codigo(qmd = caminho_qmd, script = caminho_script)
    }
    writeLines(c(
      "project:", "  type: default", "  output-dir: saida", "  execute-dir: project",
      "  render:", "    - relatorios/relatorio_completo.qmd", "    - relatorios/relatorio_artigo.qmd"
    ), file.path(projeto, "_quarto.yml"), useBytes = TRUE)
  }
  writeLines(
    c(
      "Version: 1.0", "RestoreWorkspace: No", "SaveWorkspace: No",
      "AlwaysSaveHistory: No", "Encoding: UTF-8"
    ),
    file.path(projeto, paste0(nome_projeto, ".Rproj")), useBytes = TRUE
  )
  leiame <- if (!is.null(molde)) {
    exportacao_molde_projeto_readme(molde, manifesto, nome_projeto, import_info,
                                    templates_dir, pacotes = pacotes_projeto)
  } else if (exportacao_anova_simples(manifesto)) {
    exportacao_modelo_anova("README.md", manifesto, import_info, templates_dir, pipeline, registro_bases, base_externa)
  } else {
    exportacao_leiame_projeto(nome_projeto, import_info)
  }
  if (is.null(molde)) {
    leiame <- gsub("relatorio.qmd", "relatorio_completo.qmd", leiame, fixed = TRUE)
    leiame <- sub("Na seta do **Render**, escolha **Word**", "Para Word, abra relatorios/relatorio_artigo.qmd e clique em **Render**", leiame, fixed = TRUE)
    leiame <- sub("o texto e o código na mesma ordem em que a análise é pensada, e um único", "O texto e o código seguem a ordem da análise; cada formato tem seu próprio", leiame, fixed = TRUE)
    leiame <- sub("arquivo que gera o Word (para o leitor) e o caderno HTML (para quem quer", "arquivo: relatorio_artigo.qmd gera Word; relatorio_completo.qmd gera HTML para", leiame, fixed = TRUE)
    leiame <- c(
      "# Projeto R integrado",
      "",
      "Abra o .Rproj. O código comentado fica em R/analise.R.",
      "Renderize relatorios/relatorio_completo.qmd para HTML e relatorios/relatorio_artigo.qmd para Word.",
      "Os documentos gerados ficam em saida/relatorios/. Cada Render reconstrói os resultados da seleção.",
      "Nesta rota, após editar o script, atualize os chunks dos dois QMDs com atualizar_codigo(qmd = ...).",
      "A rota integrada ainda usa funções do pacote catalyser para reconstruir as análises.",
      "", leiame
    )
  }
  leiame <- gsub("projeto_analise.Rproj", paste0(nome_projeto, ".Rproj"), leiame, fixed = TRUE)
  leiame <- gsub("projeto.Rproj", paste0(nome_projeto, ".Rproj"), leiame, fixed = TRUE)
  leiame <- sub("^projeto/$", paste0(nome_projeto, "/"), leiame)
  # A ficha de planejamento viaja como objeto salvo em dados/ (contrato 1), sem pasta nova.
  # A ficha permanece na IDE; o Projeto R conserva apenas os arquivos do molde.
  writeLines(leiame, file.path(projeto, "README.md"), useBytes = TRUE)

  projeto
}

# A rota integrada conserva seus cálculos e publica dois documentos independentes.
# O nome explícito em cada ligação evita conferir acidentalmente o outro QMD.
exportacao_qmd_integrado_formato <- function(linhas, arquivo) {
  inicio_docx <- match("  docx:", linhas)
  inicio_html <- match("  html:", linhas)
  fim_formatos <- match("execute:", linhas)
  if (anyNA(c(inicio_docx, inicio_html, fim_formatos))) stop("Confira os formatos do relatório integrado.")
  remover <- if (identical(arquivo, "relatorio_completo.qmd")) inicio_docx else inicio_html
  limites <- sort(c(inicio_docx, inicio_html, fim_formatos))
  proximo <- limites[limites > remover][1]
  linhas <- linhas[-seq.int(remover, proximo - 1L)]
  linhas <- sub('conferir_codigo()', sprintf('conferir_codigo(qmd = here::here("relatorios", "%s"))', arquivo), linhas, fixed = TRUE)
  linhas <- sub('atualizar_codigo()', sprintf('atualizar_codigo(qmd = here::here("relatorios", "%s"))', arquivo), linhas, fixed = TRUE)
  linhas
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
