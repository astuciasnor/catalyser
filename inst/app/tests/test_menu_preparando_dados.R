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

html_compartilhada <- htmltools::renderTags(mod_preparar_compartilhada_ui("teste_preparo"))$html
stopifnot(
  all(vapply(c("Importar Dados", "Reestruturar Planilha", "Preparar Base Compartilhada", "Preparar Bases Derivadas"),
    function(x) grepl(paste0('title = "', x, '"'), codigo_app, fixed = TRUE), logical(1))),
  all(vapply(c("Variáveis e categorias", "Cálculos e transformações", "Limpeza", "Etapas do Preparo", "Dados preparados", "Código R"),
    function(x) grepl(x, html_compartilhada, fixed = TRUE), logical(1))),
  !grepl("Checagem Final da Base Compartilhada", html_compartilhada, fixed = TRUE),
  grepl('id="organizar_variaveis-abrir_renomear"', html_compartilhada, fixed = TRUE),
  grepl('id="tratar-calc_modo"', html_compartilhada, fixed = TRUE),
  grepl('id="tratar-re_modo"', html_compartilhada, fixed = TRUE),
  grepl('id="teste_bases_menu-base_escolhida"', html_bases, fixed = TRUE),
  grepl('id="teste_bases_menu-grupo_acoes"', html_bases, fixed = TRUE),
  grepl("acumular_codigo = TRUE", codigo_app, fixed = TRUE),
  grepl("adicionar_mudanca_compartilhada", codigo_app, fixed = TRUE),
  !grepl("Renomear colunas", html_arrumar_emp, fixed = TRUE),
  !grepl("Tipar colunas", html_arrumar_sep, fixed = TRUE)
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
