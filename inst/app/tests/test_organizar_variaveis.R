source("app.R", local = TRUE)

dados_entrada <- data.frame(
  especie = c("Corvina", " corvina ", "Pargo"),
  comprimento = c("25.5", "31.0", "28.0"),
  peso = c(200, 310, 260),
  observacao = c("a", "b", "c"),
  stringsAsFactors = FALSE
)

codigo <- organizar_variaveis_codigo(
  renomear = c(comprimento = "comprimento_cm"),
  tipos = list(comprimento_cm = "numero"),
  recodes = list(especie = c(" corvina " = "Corvina")),
  selecionar = c("especie", "comprimento_cm", "peso")
)

stopifnot(
  grepl("dplyr::rename", codigo, fixed = TRUE),
  grepl("comprimento_cm = comprimento", codigo, fixed = TRUE),
  grepl("dplyr::recode", codigo, fixed = TRUE),
  grepl("as.numeric", codigo, fixed = TRUE),
  grepl("dplyr::select", codigo, fixed = TRUE)
)

dados_rv <- reactiveVal(dados_entrada)
promovido_rv <- reactiveVal(NULL)

testServer(
  mod_organizar_variaveis_server,
  args = list(
    data_rv = reactive(dados_rv()),
    on_usar = function(df, fonte, codigo) {
      promovido_rv(list(df = df, fonte = fonte, codigo = codigo))
    }
  ),
  {
    renomear_rv(c(comprimento = "comprimento_cm"))
    recodes_rv(list(especie = c(" corvina " = "Corvina")))
    tipos_rv(list(comprimento_cm = "numero"))
    selecionar_rv(c("especie", "comprimento_cm", "peso"))
    session$flushReact()

    resultado <- resultado_final()
    stopifnot(
      identical(names(resultado), c("especie", "comprimento_cm", "peso")),
      is.numeric(resultado$comprimento_cm),
      identical(resultado$especie, c("Corvina", "Corvina", "Pargo")),
      identical(nrow(resultado), nrow(dados_entrada)),
      isTRUE(tem_alteracoes())
    )

    session$setInputs(usar_base = 1)
    session$flushReact()
    promovido <- promovido_rv()
    stopifnot(
      identical(promovido$fonte, "Criar e Editar Variáveis e Níveis"),
      identical(promovido$df, resultado),
      grepl("dados_organizados", promovido$codigo, fixed = TRUE)
    )
  }
)

cat("OK: Criar e Editar Variáveis e Níveis centraliza seleção, nomes, tipos e categorias\n")
