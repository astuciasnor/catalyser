source("app.R", local = TRUE, encoding = "UTF-8")
df <- as.data.frame(readxl::read_excel("dados/Treino-Transformacoes.xlsx", sheet = "biometria"))
stopifnot(nrow(df) == 71L)
pipeline <- reactiveVal(list())
base <- reactive(df)
replay <- reactive(replay_pipeline(base(), pipeline()))
final <- reactive(replay()$df)
# Cálculo guiado e reescala entram na mesma sequência de preparo.
testServer(mod_tratar_server, args = list(base_rv = base, replay_rv = replay, pipeline_rv = pipeline,
 import_info = reactive(list()), grupo_rv = reactive("calculos")), {
 session$flushReact()
 session$setInputs(tipo = "calcular", calc_modo = "guiado", calc_nome = "peso_dobro", calc_a = "peso_g",
  calc_funcao = "nenhuma", calc_op = "*", calc_b_tipo = "numero", calc_b_num = 2, add_etapa = 1)
 stopifnot(length(pipeline()) == 1L, identical(final()$peso_dobro, df$peso_g * 2))
 session$setInputs(tipo = "reescalar", re_col = "peso_g", re_nome = "peso_kg", re_modo = "manual", re_prefixo = "k", add_etapa = 2)
 stopifnot(length(pipeline()) == 2L, identical(final()$peso_kg, df$peso_g / 1000))
})
# Renomear uma coluna criada pelo cálculo usa a base já preparada.
testServer(mod_organizar_variaveis_server, args = list(data_rv = final,
 on_etapa = function(et) pipeline(c(pipeline(), list(et)))), {
 session$flushReact()
 renomear_rv(c(peso_kg = "massa_kg"))
 tipos_rv(list(sexo = "fator"))
 session$setInputs(usar_base = 1)
 session$flushReact()
 stopifnot(length(pipeline()) == 3L, "massa_kg" %in% names(final()), !"peso_kg" %in% names(final()),
  is.factor(final()$sexo), identical(final()$massa_kg, df$peso_g / 1000), !tem_alteracoes())
})
# A limpeza subsequente atualiza a tabela e o download final uma única vez.
testServer(mod_tratar_server, args = list(base_rv = base, replay_rv = replay, pipeline_rv = pipeline,
 import_info = reactive(list()), grupo_rv = reactive("limpeza")), {
 session$flushReact()
 session$setInputs(tipo = "tratar_na", na_col = "massa_kg", na_metodo = "remover", add_etapa = 1)
 stopifnot(length(pipeline()) == 4L, nrow(final()) == sum(!is.na(df$peso_g)), !length(replay()$erros))
})
org <- list(previa = final, pendente = reactive(FALSE))
testServer(mod_preparar_compartilhada_server, args = list(dados_analise = final, replay_res = replay,
 pipeline_rv = pipeline, base_externa_rv = reactive(NULL), organizacao = org), {
 session$flushReact()
 planilha <- readxl::read_excel(output$baixar_base)
 stopifnot(nrow(planilha) == nrow(final()), identical(planilha$massa_kg, final()$massa_kg))
 ambiente <- new.env(parent = globalenv()); ambiente$dados <- df
 eval(parse(text = codigo()), envir = ambiente)
 stopifnot(isTRUE(all.equal(unname(as.list(ambiente$dados_analise)), unname(as.list(final())))))
})
# O código do exportador reproduz a nova etapa sem depender da CatalyseR.
local({
 manifesto <- list(execucoes = list(e = list(incluir_relatorio = TRUE, base_tipo = "compartilhada")))
 # Usa diretamente os fragmentos que o exportador incorpora ao relatório.
 codigo <- unlist(lapply(isolate(pipeline()), function(et) tratamentos[[et$tipo]]$codigo(et$params)))
 ambiente <- new.env(parent = baseenv()); ambiente$dados <- df
 eval(parse(text = codigo), envir = ambiente)
 stopifnot(isTRUE(all.equal(as.list(ambiente$dados), as.list(isolate(final())))))
})
stopifnot(identical(exportacao_nome_curto("Leite Tubérculo"), "leite_tuberculo"),
 identical(exportacao_nome_curto("leite tuberculo aula"), "leite_tuberculo"),
 identical(exportacao_nome_curto(""), "analise"),
 identical(exportacao_nome_curto("CON"), "con_analise"),
 identical(exportacao_sugerir_nome_projeto(list(source = "local", file_name = "aulas_bioestatistica.xlsx", excel_sheet = "11.leite_tuber")), "leite_tuber"))
cat("PASSOU: cálculo, reescala, renomeação após cálculo, tipos, limpeza, download e reprodução do preparo na planilha de treino.\n")

entrada_tipos <- data.frame(numero = factor(c("10", "20")), categoria = factor(c('A "um"', "B")))
p_tipos <- list(renomear = character(), tipos = list(numero = "numero"),
  recodes = list(categoria = setNames('Grupo "um"', 'A "um"')), selecionar = NULL)
saida_tipos <- trat_organizar_aplicar(entrada_tipos, p_tipos)
stopifnot(identical(saida_tipos$numero, c(10, 20)),
          identical(saida_tipos$categoria, c('Grupo "um"', "B")))
cat("PASSOU: converter fatores numéricos e recodificar categorias com aspas preserva os valores da prévia.\n")

# A recodificação usa os rótulos originais, sem substituições em cascata.
testServer(mod_organizar_variaveis_server, args=list(data_rv=reactive(data.frame(grupo=c("A","B")))), {
 session$flushReact()
 recodes_rv(list(grupo=c(A="B",B="C")))
 esperado <- resultado_final()
 obtido <- trat_organizar_aplicar(base_data(),list(recodes=recodes_rv()))
 stopifnot(identical(esperado,obtido),identical(esperado$grupo,c("B","C")))
})

# Selecionar pela tabela e pelo campo superior mantém a mesma base ativa.
local({
 registros <- reactiveVal(list(
  bases_novo_registro("base_0001","Primeira","base_primeira"),
  bases_novo_registro("base_0002","Segunda","base_segunda")))
 caches <- reactiveVal(list())
 revisao <- reactiveVal(1L)
 mock <- MockShinySession$new()
 mensagens <- list()
 mock$sendInputMessage <- function(id, message) mensagens[[id]] <<- message
 testServer(mod_bases_derivadas_server, session=mock, args=list(
  dados_analise_rv=reactive(df), registro_bases_rv=registros,
  cache_bases_rv=caches, revisao_origem_rv=revisao), {
   session$flushReact()
   session$setInputs(base_escolhida="base_0002")
   stopifnot(identical(base_selecionada()$id,"base_0002"))
   session$setInputs(tabela_rows_selected=1L)
   stopifnot(identical(base_selecionada()$id,"base_0001"),
             identical(mensagens$base_escolhida$value,"base_0001"))
   session$setInputs(recalcular=1,finalizar=1)
   anterior <- caches()[["base_0001"]]$df
   revisao(2L);session$flushReact()
   stopifnot(grepl("A base compartilhada ou a receita mudou", output$editor_base$html, fixed=TRUE),
             grepl("última execução", output$editor_base$html, fixed=TRUE),
             grepl("Recalcular dados", output$editor_base$html, fixed=TRUE),
             identical(caches()[["base_0001"]]$df, anterior),
             !length(bases_disponiveis_analise(registros(),caches(),revisao())))
   registro_bases_rv(list());session$flushReact()
   stopifnot(is.null(base_selecionada()))
 })
 cat("PASSOU: seleção sincronizada e aviso de origem desatualizada nas bases derivadas.\n")
})
