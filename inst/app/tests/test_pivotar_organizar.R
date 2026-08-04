# Integração pedagógica de Pivotar/Separar e Criar e Editar Variáveis e Níveis
# Executar a partir de inst/app.

source("app.R", local = TRUE)

dados_largos <- data.frame(
  porto = c("Bragança", "Vigia"),
  `2022 - Captura (t)` = c(10, 20),
  `2023 - Captura (t)` = c(12, 23),
  check.names = FALSE
)

cfg_empilhar_ok <- list(
  modo = "empilhar",
  metodo = "delim",
  delim = " - ",
  regex = "",
  novas = c("ano", "medida"),
  cols_medida = c("2022 - Captura (t)", "2023 - Captura (t)"),
  values_to = "captura_t",
  como_numero = TRUE,
  saida = "longo",
  wider_names = ""
)

stopifnot(is.null(arrumar_validar(cfg_empilhar_ok, dados_largos)))
resultado_empilhar <- arrumar_aplicar(cfg_empilhar_ok, dados_largos)
stopifnot(
  identical(names(resultado_empilhar), c("porto", "ano", "medida", "captura_t")),
  nrow(resultado_empilhar) == 4L,
  identical(resultado_empilhar$ano, c("2022", "2023", "2022", "2023"))
)

cfg_nome_repetido <- cfg_empilhar_ok
cfg_nome_repetido$novas <- c("ano", "captura")
cfg_nome_repetido$values_to <- "Captura"
msg_nome_repetido <- arrumar_validar(cfg_nome_repetido, dados_largos)
stopifnot(
  grepl("deve ser diferente", msg_nome_repetido, fixed = TRUE),
  grepl("ano, medida", msg_nome_repetido, fixed = TRUE),
  grepl("captura_t", msg_nome_repetido, fixed = TRUE)
)

dados_com_captura <- dados_largos
dados_com_captura$Captura <- c("observada", "observada")
cfg_coluna_existente <- cfg_empilhar_ok
cfg_coluna_existente$values_to <- "Captura"
msg_coluna_existente <- arrumar_validar(cfg_coluna_existente, dados_com_captura)
stopifnot(
  grepl("já existe na base", msg_coluna_existente, fixed = TRUE),
  grepl("captura_t", msg_coluna_existente, fixed = TRUE)
)

info_rv <- reactive(list(
  source = "default",
  file_name = "teste.csv",
  datapath = "dados/teste.csv",
  excel_sheet = "Dados",
  csv_sep = ",",
  csv_dec = ".",
  csv_header = TRUE,
  package_dataset = NULL
))

dados_longos <- data.frame(
  id = rep(1:3, each = 2),
  medida = rep(c("comprimento_cm", "peso_g"), 3),
  valor = c(20, 100, 25, 160, 30, 240),
  stringsAsFactors = FALSE
)

promovido_wider_rv <- reactiveVal(NULL)
testServer(
  mod_arrumar_server,
  args = list(
    data_rv = reactive(dados_longos),
    import_info = info_rv,
    modo_fixo = "alargar",
    on_usar = function(df, fonte, codigo) {
      promovido_wider_rv(list(df = df, fonte = fonte, codigo = codigo))
    }
  ),
  {
    session$flushReact()
    session$setInputs(
      wider_names2 = "medida",
      wider_values = "valor",
      aplicar = 1
    )
    session$flushReact()

    resultado <- resultado_final()
    stopifnot(
      identical(names(resultado), c("id", "comprimento_cm", "peso_g")),
      nrow(resultado) == 3L,
      identical(resultado$peso_g, c(100, 160, 240))
    )

    session$setInputs(usar_analises = 1)
    session$flushReact()
    promovido <- promovido_wider_rv()
    stopifnot(
      identical(promovido$fonte, "Alargar Dados — pivot_wider()"),
      grepl("pivot_wider", promovido$codigo, fixed = TRUE),
      identical(promovido$df, resultado)
    )
  }
)

dados_criacao <- data.frame(
  comprimento_cm = c(20, 25, 30),
  peso_g = c(100, 160, 240)
)

promovido_criacao_rv <- reactiveVal(NULL)
testServer(
  mod_calcular_server,
  args = list(
    data_rv = reactive(dados_criacao),
    import_info = info_rv,
    on_usar = function(df, fonte, codigo) {
      promovido_criacao_rv(list(df = df, fonte = fonte, codigo = codigo))
    }
  ),
  {
    session$flushReact()
    session$setInputs(
      acao = "calcular",
      modo_calc = "livre",
      l_expr = "peso_g / comprimento_cm",
      nome_calc = "fator_condicao",
      aplicar = 1
    )
    session$flushReact()

    session$setInputs(
      acao = "reescalar",
      r_col = "peso_g",
      r_modo = "manual",
      r_prefixo = "k",
      nome_re = "peso_kg",
      aplicar = 2
    )
    session$flushReact()

    resultado <- resultado_rv()
    stopifnot(
      all(c("fator_condicao", "peso_kg") %in% names(resultado)),
      isTRUE(all.equal(resultado$fator_condicao, c(5, 6.4, 8))),
      isTRUE(all.equal(resultado$peso_kg, dados_criacao$peso_g / 1000))
    )

    session$setInputs(usar_analises = 1)
    session$flushReact()
    promovido <- promovido_criacao_rv()
    stopifnot(
      identical(promovido$fonte, "Criação de Variáveis"),
      grepl("mutate", promovido$codigo, fixed = TRUE),
      identical(promovido$df, resultado)
    )
  }
)

cat("OK: pivot_wider e criação de variáveis alimentam a trilha compartilhada\n")
