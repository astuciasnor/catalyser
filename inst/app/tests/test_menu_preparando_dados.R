source("app.R", local = TRUE)

codigo_app <- paste(readLines("app.R", encoding = "UTF-8"), collapse = "\n")
codigo_tratar <- paste(
  readLines("modules/mod_tratar.R", encoding = "UTF-8"),
  collapse = "\n"
)
codigo_bases <- paste(
  readLines("modules/mod_bases_derivadas.R", encoding = "UTF-8"),
  collapse = "\n"
)
codigo_seletor_base <- paste(
  readLines("modules/mod_seletor_base_analise.R", encoding = "UTF-8"),
  collapse = "\n"
)
codigo_organizar <- paste(
  readLines("modules/mod_organizar_variaveis.R", encoding = "UTF-8"),
  collapse = "\n"
)
codigo_nao_parametrico <- paste(
  readLines("modules/mod_nonparametric.R", encoding = "UTF-8"),
  collapse = "\n"
)

html_tratamentos <- htmltools::renderTags(
  mod_tratar_ui(
    "teste_tratar",
    checagem_ui = mod_organizar_variaveis_checagem_ui("teste_organizar")
  )
)$html

html_bases <- htmltools::renderTags(
  mod_bases_derivadas_ui("teste_bases_menu")
)$html
html_arrumar_emp <- htmltools::renderTags(
  mod_arrumar_ui("teste_arrumar_emp", modo_fixo = "empilhar")
)$html
html_arrumar_sep <- htmltools::renderTags(
  mod_arrumar_ui("teste_arrumar_sep", modo_fixo = "separar")
)$html
html_arrumar_wider <- htmltools::renderTags(
  mod_arrumar_ui("teste_arrumar_wider", modo_fixo = "alargar")
)$html
html_organizar <- htmltools::renderTags(
  mod_organizar_variaveis_ui(
    "teste_organizar",
    criacao_ui = mod_calcular_ui("calcular")
  )
)$html

stopifnot(
  grepl('title = "Pivotar e Separar Dados"', codigo_app, fixed = TRUE),
  grepl("Empilhar Dados — pivot_longer()", codigo_app, fixed = TRUE),
  grepl("Alargar Dados — pivot_wider()", codigo_app, fixed = TRUE),
  grepl("Separar Dados em Colunas", codigo_app, fixed = TRUE),
  grepl('modo_fixo = "alargar"', codigo_app, fixed = TRUE),
  grepl('title = "Organizar Variáveis"', codigo_app, fixed = TRUE),
  regexpr('title = "Pivotar e Separar Dados"', codigo_app, fixed = TRUE) <
    regexpr('title = "Organizar Variáveis"', codigo_app, fixed = TRUE),
  regexpr('title = "Organizar Variáveis"', codigo_app, fixed = TRUE) <
    regexpr('title = "Adicionar Tratamentos à Base"', codigo_app, fixed = TRUE),
  !grepl('uiOutput("dataset_vars_selector"', codigo_app, fixed = TRUE),
  !grepl('uiOutput("variable_type_converter_ui")', codigo_app, fixed = TRUE),
  grepl('title = "Adicionar Tratamentos à Base"', codigo_app, fixed = TRUE),
  !grepl('title = "Trilha de Preparo"', codigo_app, fixed = TRUE),
  !grepl('title = "Calcular / Reescalar Variável"', codigo_app, fixed = TRUE),
  !grepl('title = "Agrupar / Sumarizar"', codigo_app, fixed = TRUE),
  !grepl('title = "Criando Tabela de Contingência"', codigo_app, fixed = TRUE),
  grepl('input[["bases_derivadas-bases_derivadas_subabas"]]',
        codigo_app, fixed = TRUE),
  grepl('input[["bases_derivadas-ramo_tipo"]]',
        codigo_app, fixed = TRUE),
  !grepl('mod_agrupar_sumarizar_server("agrupar_sumarizar"',
         codigo_app, fixed = TRUE),
  !grepl("Calculadora guiada", html_tratamentos, fixed = TRUE),
  grepl('id="teste_tratar-tipo"', html_tratamentos, fixed = TRUE),
  grepl("max-height: min(30rem", html_tratamentos, fixed = TRUE),
  grepl("options = list(maxOptions = 10)", codigo_tratar, fixed = TRUE),
  !grepl("Calcular variável", html_tratamentos, fixed = TRUE),
  !grepl("Reescalar (prefixo SI)", html_tratamentos, fixed = TRUE),
  grepl('id="teste_tratar-tratamentos_subabas"',
        html_tratamentos, fixed = TRUE),
  grepl("Tratamentos e trilha", html_tratamentos, fixed = TRUE),
  grepl("Checagem Final da Base Compartilhada",
        html_tratamentos, fixed = TRUE),
  grepl("Adicionar à Trilha da Base Compartilhada",
        html_tratamentos, fixed = TRUE),
  !grepl('value="filtrar"', html_tratamentos, fixed = TRUE),
  grepl("redução de linhas fica restrita à remoção de duplicatas",
        html_tratamentos, fixed = TRUE),
  grepl('id="teste_bases_menu-bases_derivadas_subabas"',
        html_bases, fixed = TRUE),
  !grepl('tabPanel(\n          title = "Agrupar / Sumarizar"',
         codigo_bases, fixed = TRUE),
  !grepl('tabPanel(\n          title = "Tabela de Contingência"',
         codigo_bases, fixed = TRUE),
  grepl('value="agrupar_sumarizar"', html_bases, fixed = TRUE),
  grepl('value="contingencia"', html_bases, fixed = TRUE),
  grepl('value="filtrar"', html_bases, fixed = TRUE),
  grepl("Agrupar / Sumarizar", html_bases, fixed = TRUE),
  grepl("Tabela de Contingência", html_bases, fixed = TRUE),
  grepl("recalcular_receita", codigo_bases, fixed = TRUE),
  grepl("Recalcular a Base", codigo_bases, fixed = TRUE),
  grepl("recalcular_base_selecionada", codigo_bases, fixed = TRUE),
  grepl("acumular_codigo = TRUE", codigo_app, fixed = TRUE),
  grepl("# Etapa seguinte:", codigo_app, fixed = TRUE),
  grepl("adicionar_mudanca_compartilhada", codigo_app, fixed = TRUE),
  grepl('mod_arrumar_server(\n    "arrumar_wider"', codigo_app, fixed = TRUE),
  !grepl("Renomear colunas", html_arrumar_emp, fixed = TRUE),
  !grepl("Recodificar níveis", html_arrumar_emp, fixed = TRUE),
  grepl("Nomes das colunas que descrevem os cabeçalhos",
        html_arrumar_emp, fixed = TRUE),
  grepl("Nome da coluna que receberá os valores",
        html_arrumar_emp, fixed = TRUE),
  grepl("ano = 2022", html_arrumar_emp, fixed = TRUE),
  grepl('value="ano, medida"', html_arrumar_emp, fixed = TRUE),
  grepl('value="captura_t"', html_arrumar_emp, fixed = TRUE),
  !grepl("Tipar colunas", html_arrumar_sep, fixed = TRUE),
  !grepl("Selecionar variáveis", html_arrumar_sep, fixed = TRUE),
  grepl('id="teste_organizar-abrir_selecionar"', html_organizar, fixed = TRUE),
  grepl('id="teste_organizar-abrir_renomear"', html_organizar, fixed = TRUE),
  grepl('id="teste_organizar-abrir_tipar"', html_organizar, fixed = TRUE),
  grepl('id="teste_organizar-abrir_recodificar"', html_organizar, fixed = TRUE),
  grepl("Criação de Variáveis", html_organizar, fixed = TRUE),
  grepl("Arrumação de Variáveis", html_organizar, fixed = TRUE),
  !grepl("Checagem Final da Base Compartilhada", html_organizar, fixed = TRUE),
  grepl("grid-template-columns:minmax(210px,20%) minmax(0,80%)",
        html_tratamentos, fixed = TRUE),
  grepl('id="teste_organizar-preview_final"', html_tratamentos, fixed = TRUE),
  grepl("Adicionar Mudança à Trilha da Base Compartilhada",
        html_tratamentos, fixed = TRUE),
  grepl("Adicionar Mudança à Trilha da Base Compartilhada",
        html_arrumar_emp, fixed = TRUE),
  grepl("Adicionar Mudança à Trilha da Base Compartilhada",
        html_arrumar_wider, fixed = TRUE),
  grepl("Adicionar Mudança à Trilha da Base Compartilhada",
        html_arrumar_sep, fixed = TRUE),
  grepl("não filtram linhas", html_organizar, fixed = TRUE),
  grepl("organizar_variaveis_codigo", codigo_organizar, fixed = TRUE),
  grepl("Se a base derivada não aparecer", codigo_app, fixed = TRUE),
  grepl("Base(s) ainda fora da lista", codigo_seletor_base, fixed = TRUE),
  grepl("falta Finalizar preparo", codigo_seletor_base, fixed = TRUE),
  grepl('class = "row g-2 align-items-start"', codigo_seletor_base, fixed = TRUE),
  grepl("catalyser-base-selector", codigo_seletor_base, fixed = TRUE),
  grepl('class = "col-12"', codigo_seletor_base, fixed = TRUE),
  grepl("selectizeInput", codigo_seletor_base, fixed = TRUE),
  grepl('fill = FALSE', codigo_seletor_base, fixed = TRUE),
  grepl("não é uma contingência tidy", codigo_nao_parametrico, fixed = TRUE),
  grepl("não use a coluna id como contagem", codigo_nao_parametrico, fixed = TRUE),
  grepl("fillable = FALSE", codigo_nao_parametrico, fixed = TRUE),
  grepl("align-items:start !important", codigo_nao_parametrico, fixed = TRUE),
  grepl('updateSelectInput(session, "chi_tidy_n", choices = character(0))',
        codigo_nao_parametrico, fixed = TRUE),
  !grepl('id="calcular-', html_tratamentos, fixed = TRUE),
  grepl('id="calcular-', html_organizar, fixed = TRUE),
  grepl('mod_organizar_variaveis_checagem_ui("organizar_variaveis")',
        codigo_app, fixed = TRUE),
  !grepl('id="agrupar_sumarizar-', html_bases, fixed = TRUE),
  !grepl('id="contingency-', html_bases, fixed = TRUE)
)

dados_grafias <- data.frame(
  especie = c("Corvina", "corvina", " CORVINA "),
  peso_g = c(100, 200, 300),
  stringsAsFactors = FALSE
)
trilha_grafias <- list(
  list(tipo = "padronizar_texto",
       params = list(coluna = "especie", metodo = "squish"), ativa = TRUE),
  list(tipo = "padronizar_texto",
       params = list(coluna = "especie", metodo = "minusculas"), ativa = TRUE)
)
base_compartilhada_teste <- replay_pipeline(dados_grafias, trilha_grafias)$df
etapa_resumo <- list(
  tipo = "agrupar_sumarizar",
  params = list(
    grupos = "especie",
    variaveis = "peso_g",
    funcoes = c("n", "media"),
    ordenar = TRUE
  ),
  ativa = TRUE
)
resumo_especie <- replay_pipeline(
  base_compartilhada_teste, list(etapa_resumo)
)$df
stopifnot(
  nrow(resumo_especie) == 1L,
  identical(resumo_especie$especie, "corvina"),
  resumo_especie$n == 3L,
  resumo_especie$peso_g_media == 200
)

dados_contingencia <- data.frame(
  especie = c("bagre", "bagre", "corvina", "corvina", "corvina"),
  local = c("norte", "sul", "norte", "sul", "sul"),
  stringsAsFactors = FALSE
)
etapa_contingencia <- list(
  tipo = "contingencia",
  params = list(
    linha = "especie",
    coluna = "local",
    percentual = "row"
  ),
  ativa = TRUE
)
contingencia_tidy <- replay_pipeline(
  dados_contingencia, list(etapa_contingencia)
)$df
matriz_contingencia <- np_contingencia_tidy_matriz(
  contingencia_tidy, "especie", "local", "n"
)
stopifnot(
  nrow(contingencia_tidy) == 4L,
  identical(names(contingencia_tidy), c("especie", "local", "n", "percentual")),
  sum(contingencia_tidy$n) == 5L,
  all(abs(ave(
    contingencia_tidy$percentual,
    contingencia_tidy$especie,
    FUN = sum
  ) - 100) < 1e-8),
  identical(
    unname(matriz_contingencia),
    matrix(c(1, 1, 1, 2), nrow = 2, byrow = TRUE)
  )
)

cat("OK: menu Preparando Dados reorganizado em estúdios pedagógicos\n")
