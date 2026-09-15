# Regressão: a segunda ANOVA não deve trocar o código didático por wrappers.
for (arquivo in c("mod_arrumar.R", "mod_organizar_variaveis.R", "registro_tratamentos.R",
                  "registro_bases.R", "registro_execucoes.R", "registro_comunicacao.R",
                  "exportacao_comunicacao.R")) source(file.path("modules", arquivo), encoding = "UTF-8")

verificar_exportacao_enxuta <- function() {
  destino <- tempfile("exportacao_enxuta_")
  dir.create(destino)
  on.exit(unlink(destino, recursive = TRUE), add = TRUE)
  brutos <- data.frame(Var = rep(LETTERS[1:4], each = 4),
    Prod = c(30, 28, 33, 31, 33, 35, 32, 33, 32, 37, 34, 34, 33, 36, 36, 34))
  pipeline <- list(
    list(tipo = "organizar", ativa = TRUE, params = list(selecionar = c("Var", "Prod"))),
    list(tipo = "organizar", ativa = TRUE, params = list(renomear = c(Var = "Variedade", Prod = "Producao"))),
    list(tipo = "organizar", ativa = TRUE, params = list(tipos = list(Variedade = "fator"))))
  preparada <- replay_pipeline(brutos, pipeline)$df
  info <- list(source = "local", file_name = "aulas_bioestatistica.xlsx", excel_sheet = "3.milho")
  item <- list(id = "execucao_0001", tipo = "anova_um_fator", titulo = "Produção entre variedades",
    incluir_word = TRUE, estado_dependencia = "Atualizada", base_tipo = "compartilhada",
    base_id = "dados_analise", base_objeto = "dados_analise",
    parametros = list(resposta = "Producao", fator = "Variedade", nivel_confianca = .95),
    saidas_word = c("narrativa", "descritivos", "tabela", "comparacoes", "grafico", "pressupostos", "diagnosticos"))
  outra <- item
  outra$id <- "execucao_0002"
  outra$parametros$nivel_confianca <- .90
  for (quantidade in 1:2) {
    itens <- list(execucao_0001 = item)
    if (quantidade == 2) itens$execucao_0002 <- outra
    manifesto <- list(execucoes = itens, secoes_globais = list(introducao = "Texto do pesquisador."))
    projeto <- exportacao_criar_projeto(destino, paste0("milho_", quantidade), brutos, brutos,
      preparada, pipeline, NULL, list(), list(), itens, manifesto, 1L, info)
    stopifnot(!dir.exists(file.path(projeto, "metadados")),
      identical(readxl::excel_sheets(file.path(projeto, "dados/brutos/milho.xlsx")), "3.milho"))
    script <- file.path(projeto, "R/analise.R")
    qmd <- file.path(projeto, "relatorios/relatorio.qmd")
    env <- new.env(parent = globalenv())
    sys.source(file.path(projeto, "R/funcoes.R"), env)
    env$here <- function(...) file.path(projeto, ...)
    env$dados_brutos <- brutos
    trechos <- env$trechos_do_script(script)
    tratar <- trechos[["tratar"]]
    stopifnot(!any(grepl("trat_moda|dados_organizados|base_resolvida <-", tratar)))
    eval(parse(text = tratar), env)
    reconstruida <- if (quantidade == 1) env$base_compartilhada else env$dados_analise
    stopifnot(identical(as.data.frame(reconstruida), as.data.frame(preparada)))
    linhas <- readLines(qmd, encoding = "UTF-8")
    stopifnot(!any(grepl("metadados|catalyser_anova\\(|catalyser_executar\\(", linhas)))
    chunks <- env$chunks_do_relatorio(linhas)
    rotulos <- vapply(chunks, `[[`, character(1), "rotulo")
    stopifnot(!anyDuplicated(rotulos), !any(grepl("fig-grupos|diagnostico-influencia", linhas)),
              !any(grepl("fig-grupos|diagnostico-influencia", names(trechos))))
    grDevices::pdf(tempfile(fileext = ".pdf"))
    modelos <- 0L
    for (ch in chunks) {
      if (ch$rotulo %in% c("codigo-do-script", "atualizar", "instalar")) next
      codigo <- linhas[ch$ini:ch$fim]
      if (any(grepl("#| eval: false", codigo, fixed = TRUE))) next
      codigo <- codigo[!grepl("^library\\(here\\)", codigo)]
      valor <- eval(parse(text = codigo), env)
      if (grepl("analise-modelo$", ch$rotulo)) {
        modelos <- modelos + 1L
        esperado <- stats::aov(Producao ~ Variedade, data = preparada)
        stopifnot(isTRUE(all.equal(coef(env$modelo), coef(esperado))))
      }
      if (grepl("analise-pressupostos$", ch$rotulo)) {
        stopifnot(isTRUE(all.equal(env$teste_normalidade$p.value,
          stats::shapiro.test(residuals(env$modelo))$p.value)))
      }
      if (grepl("analise-tukey$", ch$rotulo)) {
        esperado_tukey <- stats::TukeyHSD(env$modelo, conf.level = env$nivel_confianca)
        stopifnot(isTRUE(all.equal(env$tukey, esperado_tukey)),
          !anyNA(env$resumo$letra),
          identical(unname(env$resumo$letra), unname(env$letras[as.character(env$resumo$Variedade)])))
      }
      if (grepl("fig-barras$", ch$rotulo) && inherits(valor, "ggplot")) {
        stopifnot(all(grepl("±", ggplot2::ggplot_build(valor)$data[[4]]$label, fixed = TRUE)))
      }
    }
    grDevices::dev.off()
    stopifnot(modelos == quantidade, env$nivel_confianca == if (quantidade == 1) .95 else .90,
      isTRUE(env$conferir_codigo(qmd, script)))
    # Editar uma análise continua sendo detectado, atualizado e preserva a prosa.
    original <- readLines(script, encoding = "UTF-8")
    stopifnot(any(grepl("# Renomeia as colunas: nome novo = nome anterior.", original, fixed = TRUE)),
      any(grepl("# Define Variedade como fator (categorias).", original, fixed = TRUE)),
      !any(grepl("# Renomeia as colunas:", linhas, fixed = TRUE)))
    writeLines(sub("n_preparadas <- nrow(base_da_anova)", "n_preparadas <- NROW(base_da_anova)", original, fixed = TRUE), script)
    stopifnot(is.character(tryCatch(env$conferir_codigo(qmd, script), error = conditionMessage)))
    env$atualizar_codigo(qmd, script)
    stopifnot(isTRUE(env$conferir_codigo(qmd, script)),
      any(grepl("Texto do pesquisador.", readLines(qmd, encoding = "UTF-8"), fixed = TRUE)))
  }
  # Uma operação com efeitos fora da cadeia não pode ser eliminada.
  complexo <- c("dados <- dados_brutos", "limite <- mean(dados$Prod)", "dados <- dados[dados$Prod > limite, ]")
  stopifnot(identical(exportacao_encadear_preparo(complexo), complexo))
  item$saidas_word <- "tabela"
  parcial <- exportacao_qmd_anova_acompanhada(item, "anova-milho", info, "templates")
  stopifnot(any(grepl("#| label: tbl-anova-milho-anova", parcial, fixed = TRUE)),
    !any(grepl("^#\\| label: (fig-|.*diagnostico-)", parcial)))
  # Moda só aparece quando o preparo realmente usa a imputação por moda.
  com_na <- brutos
  com_na$Prod[1] <- NA_real_
  imputar <- list(list(tipo = "tratar_na", ativa = TRUE,
    params = list(coluna = "Prod", metodo = "moda")))
  codigo <- exportacao_preparo_anova(list(execucoes = list(item)), pipeline = imputar)
  env <- new.env(parent = globalenv())
  env$dados_brutos <- com_na
  eval(parse(text = codigo), env)
  stopifnot(identical(as.data.frame(env$dados), as.data.frame(replay_pipeline(com_na, imputar)$df)),
    any(grepl("catalyser::catalyser_moda(", codigo, fixed = TRUE)),
    !any(grepl("moda <- function", codigo, fixed = TRUE)))
  # Outras escolhas da interface também devem formar uma cadeia comentada,
  # mantendo valores, nomes, tipos e ordem das linhas iguais ao replay da IDE.
  pesca <- data.frame(peso_g = c(100, 200, 200, NA, 400, 500),
    especie = c(" bagre ", " corvina ", " corvina ", " bagre ", " pescada ", " pescada "))
  tratamentos_pesca <- list(
    list(tipo = "tratar_na", params = list(coluna = "peso_g", metodo = "remover"), ativa = TRUE),
    list(tipo = "padronizar_texto", params = list(coluna = "especie", metodo = "squish"), ativa = TRUE),
    list(tipo = "remover_duplicatas", params = list(), ativa = TRUE),
    list(tipo = "reescalar", params = list(coluna = "peso_g", simbolo = "k", nome = "peso_kg"), ativa = TRUE),
    list(tipo = "calcular", params = list(nome = "peso_dobro", expr = "peso_g * 2"), ativa = TRUE),
    list(tipo = "padronizar", params = list(coluna = "peso_g", metodo = "centralizar", nome = "peso_centrado"), ativa = TRUE),
    list(tipo = "filtrar", params = list(coluna = "peso_g", origem = "numerica", operador = ">=", valor = 200), ativa = TRUE))
  codigo <- exportacao_preparo_anova(list(execucoes = list(item)), pipeline = tratamentos_pesca)
  env$dados_brutos <- pesca
  eval(parse(text = codigo), env)
  esperado <- as.data.frame(replay_pipeline(pesca, tratamentos_pesca)$df)
  obtido <- as.data.frame(env$dados)
  # Confere também contra o código anterior ao enxugamento, inclusive row.names.
  sem_enxugar <- c("dados <- dados_brutos", unlist(lapply(tratamentos_pesca,
    function(et) tratamentos[[et$tipo]]$codigo(et$params)), use.names = FALSE))
  eval(parse(text = sem_enxugar), env)
  stopifnot(identical(obtido, as.data.frame(env$dados)))
  # O drop_na do código já renumera os índices; eles não são uma coluna da base.
  rownames(obtido) <- rownames(esperado) <- NULL
  if (!identical(obtido, esperado)) stop(paste(all.equal(obtido, esperado), collapse = "\n"))
  stopifnot(
    sum(grepl("^base_compartilhada <-", codigo)) == 1L,
    !any(grepl("^dados <- dados", codigo)),
    any(grepl("# Remove linhas com valores ausentes", codigo, fixed = TRUE)),
    any(grepl("# Calcula ou transforma: peso_kg.", codigo, fixed = TRUE)))
  cat("OK: preparo encadeado, uma e duas ANOVAs, IC distinto, RDS, gráficos e vínculo script–relatório.\n")
  copia <- item
  copia$id <- "execucao_0002"
  repetido <- list(execucoes = list(execucao_0001 = item, execucao_0002 = copia))
  unico <- exportacao_sem_execucoes_repetidas(repetido)
  stopifnot(length(unico$execucoes) == 1L, unico$total_word == 1L)
  qmd_unico <- exportacao_gerar_qmd(repetido)
  stopifnot(sum(grepl("#| label: analise-modelo", qmd_unico, fixed = TRUE)) == 1L)
  copia$incluir_word <- FALSE
  repetido$execucoes$execucao_0002 <- copia
  stopifnot(length(exportacao_sem_execucoes_repetidas(repetido)$execucoes) == 2L)
  stopifnot(exportacao_nome_planilha(list(source = "local", excel_sheet = "biometria")) == "biometria.xlsx")
  cat("OK: cópias exatas saem uma vez; escolhas distintas permanecem; nome segue a aba.\n")

  # CSV não tem aba. Mesmo um registro antigo com excel_sheet não pode herdá-la.
  for (extensao in c("csv", "CSV", "tsv", "txt")) {
    origem_texto <- list(source = "local", file_name = paste0("cultivo.", extensao),
                        excel_sheet = "regressao")
    stopifnot(exportacao_nome_planilha(origem_texto) == "cultivo.xlsx",
              exportacao_aba_planilha(origem_texto) == "dados",
              exportacao_sugerir_nome_projeto(origem_texto) == "cultivo")
  }
  origem_excel <- list(source = "local", file_name = "aulas.xlsx", excel_sheet = "3.milho")
  stopifnot(exportacao_nome_planilha(origem_excel) == "milho.xlsx",
            exportacao_aba_planilha(origem_excel) == "3.milho")
  origem_pacote <- list(source = "package", package_dataset = "biometria", file_name = "cultivo.csv")
  stopifnot(exportacao_nome_planilha(origem_pacote) == "biometria.xlsx")

  arquivo_csv <- file.path(destino, "cultivo.csv")
  utils::write.csv(brutos, arquivo_csv, row.names = FALSE)
  entrada_csv <- utils::read.csv(arquivo_csv)
  info_csv <- list(source = "local", file_name = "cultivo.csv", datapath = arquivo_csv,
                   excel_sheet = "regressao", csv_sep = ",", csv_dec = ".", csv_header = TRUE)
  zip_csv <- file.path(destino, "origem_csv.zip")
  exportacao_empacotar_projeto(zip_csv, nome_projeto = "origem_csv",
    dados_brutos = entrada_csv, base_resolvida = entrada_csv, dados_analise = preparada,
    pipeline = pipeline, base_externa = NULL, registro_bases = list(), cache_bases = list(),
    registro_execucoes = list(item), manifesto = list(execucoes = list(item)),
    revisao_origem = 1L, import_info = info_csv)
  utils::unzip(zip_csv, exdir = destino)
  pasta_csv <- file.path(destino, "origem_csv")
  excel_csv <- file.path(pasta_csv, "dados/brutos/cultivo.xlsx")
  stopifnot(file.exists(excel_csv), identical(readxl::excel_sheets(excel_csv), "dados"),
    isTRUE(all.equal(as.data.frame(readxl::read_excel(excel_csv)), entrada_csv, check.attributes = FALSE)))
  script_csv <- readLines(file.path(pasta_csv, "R/analise.R"), encoding = "UTF-8")
  stopifnot(any(grepl("cultivo.xlsx", script_csv, fixed = TRUE)),
            !any(grepl("regressao|fig-grupos", script_csv)))
  cat("OK: ZIP de CSV preserva nome, dados e leitura; não herda aba de Excel.\n")
}
verificar_exportacao_enxuta()
