# Testes do núcleo canônico e do estado Shiny da ANOVA — V16
# Executar a partir de inst/app.

source("app.R", encoding = "UTF-8")

quase_igual <- function(x, y, tol = 1e-3) {
  isTRUE(is.finite(x) && is.finite(y) && abs(x - y) <= tol)
}

# =============================================================================
# 1. Validação da configuração
# =============================================================================
dados_base <- data.frame(
  profundidade_m = c(10, 12, 11, 20, 22, 21, 30, 33, 31, 15, 16, 14),
  especie = rep(c("bagre", "corvina", "pargo", "sardinha"), each = 3),
  rotulo = rep(c("a", "b", "c", "d"), each = 3),
  stringsAsFactors = FALSE
)

stopifnot(is.null(anova_validar_entrada(dados_base, "profundidade_m", "especie")))

# Coluna inexistente
stopifnot(grepl("não contém", anova_validar_entrada(dados_base, "profundidade_m", "local"), fixed = TRUE))
# Resposta não numérica
stopifnot(grepl("precisa ser numérica", anova_validar_entrada(dados_base, "especie", "rotulo"), fixed = TRUE))
# Resposta e fator iguais
stopifnot(grepl("variáveis diferentes", anova_validar_entrada(dados_base, "especie", "especie"), fixed = TRUE))
# Fator com um único nível
dados_um_nivel <- dados_base[dados_base$especie == "bagre", ]
stopifnot(grepl("pelo menos dois grupos", anova_validar_entrada(dados_um_nivel, "profundidade_m", "especie"), fixed = TRUE))
# Grupo com uma só observação
dados_grupo_minimo <- rbind(
  dados_base[dados_base$especie %in% c("bagre", "corvina"), ],
  data.frame(profundidade_m = 40, especie = "pescada", rotulo = "e", stringsAsFactors = FALSE)
)
stopifnot(grepl("menos de duas observações",
                anova_validar_entrada(dados_grupo_minimo, "profundidade_m", "especie"), fixed = TRUE))

# =============================================================================
# 2. Exclusão explícita de casos incompletos e componentes do resultado
# =============================================================================
dados_com_na <- dados_base
dados_com_na$profundidade_m[c(2, 7)] <- NA
r_na <- calcular_anova(dados_com_na, "profundidade_m", "especie")
stopifnot(
  r_na$n == 10L,
  r_na$excluidos == 2L,
  sum(r_na$descritivos_df$Ausentes) == 2L
)

r <- calcular_anova(dados_base, "profundidade_m", "especie")
descritivos <- arrumar_descritivos_anova(r)
tabela <- arrumar_tabela_anova(r)
efeito <- arrumar_tamanho_efeito_anova(r)
pressupostos <- arrumar_pressupostos_anova(r)
tukey <- arrumar_tukey_anova(r)
narrativa <- relatar_anova(r)

stopifnot(
  is.data.frame(descritivos), nrow(descritivos) == 4L,
  all(c("Grupo", "n", "Média ± DP", "IC 95% da média", "Diferença",
        "Ausentes excluídos") %in% names(descritivos)),
  # As letras de Tukey: mesma letra = sem evidência de diferença.
  all(nzchar(descritivos[["Diferença"]])),
  all(grepl("^[a-z]+$", descritivos[["Diferença"]])),
  # A letra "a" fica com o grupo de maior média.
  identical(
    descritivos$Grupo[grepl("a", descritivos[["Diferença"]], fixed = TRUE)][1],
    r$descritivos_df$Grupo[which.max(r$descritivos_df$Media)]
  ),
  all(grepl("±", descritivos[["Média ± DP"]], fixed = TRUE)),
  nrow(tabela) == 3L,
  all(c("Fonte de variação", "Graus de liberdade", "Soma de quadrados",
        "Quadrado médio", "F", "p-valor") %in% names(tabela)),
  nrow(efeito) == 2L,
  any(grepl("Eta quadrado", efeito$Medida, fixed = TRUE)),
  any(grepl("Ômega quadrado", efeito$Medida, fixed = TRUE)),
  "Leitura convencional" %in% names(efeito),
  all(efeito[["Leitura convencional"]] %in%
        c("muito pequeno", "pequeno", "médio", "grande", "-")),
  nrow(pressupostos) == 3L,
  any(grepl("Levene", pressupostos$Pressuposto, fixed = TRUE)),
  nrow(tukey) == choose(4L, 2L),
  any(grepl("^IC 95% inferior$", names(tukey))),
  is.character(narrativa), nzchar(narrativa)
)

# --- Apresentação em português: vírgula decimal e p-valor com limiar ----------
colunas_numericas <- c(
  descritivos[["Média ± DP"]], descritivos[["IC 95% da média"]],
  tabela[["Soma de quadrados"]], tabela[["F"]],
  efeito[["Valor"]], tukey[["Diferença estimada"]]
)
stopifnot(
  all(vapply(colunas_numericas, is.character, logical(1))),
  # Nenhum número exibido usa ponto decimal.
  !any(grepl(".", colunas_numericas, fixed = TRUE)),
  any(grepl(",", colunas_numericas, fixed = TRUE)),
  # A linha "Total" não inventa quadrado médio, F nem p.
  identical(tabela[["Quadrado médio"]][3], "-"),
  identical(tabela[["F"]][2], "-"),
  identical(tabela[["p-valor"]][3], "-")
)
stopifnot(
  identical(anova_p_col(c(0.0004, 0.0318, NA)), c("< 0,001", "0,0318", "-")),
  identical(anova_num_col(c(2.8309, NA), 3), c("2,831", "-")),
  identical(
    anova_leitura_efeito(c(0.005, 0.03, 0.10, 0.20)),
    c("muito pequeno", "pequeno", "médio", "grande")
  )
)

# --- A narrativa não repete o que as tabelas já mostram ----------------------
# Médias por grupo ficam no resumo por grupo; Shapiro e Levene, nos pressupostos.
stopifnot(
  !grepl("Shapiro", narrativa, fixed = TRUE),
  !grepl("Levene", narrativa, fixed = TRUE),
  !grepl("média = ", narrativa, fixed = TRUE),
  # Mas a narrativa continua dizendo onde encontrar cada coisa.
  grepl("resumo por grupo", narrativa, fixed = TRUE),
  grepl("pressupostos", narrativa, fixed = TRUE),
  # E mantém pergunta, amostra, decisão e efeito.
  grepl(r$dep_var, narrativa, fixed = TRUE),
  grepl(r$ind_var, narrativa, fixed = TRUE),
  grepl("observações completas", narrativa, fixed = TRUE),
  grepl("η²", narrativa, fixed = TRUE),
  grepl("ω²", narrativa, fixed = TRUE),
  grepl("F(3; 8)", narrativa, fixed = TRUE)
)

# A soma de quadrados fecha e o efeito é coerente
stopifnot(
  quase_igual(r$sq_entre + r$sq_dentro,
              sum((r$dados$resposta - mean(r$dados$resposta))^2), 1e-6),
  quase_igual(r$eta2, r$sq_entre / (r$sq_entre + r$sq_dentro), 1e-9),
  r$omega2 < r$eta2
)

# =============================================================================
# 3. Linguagem: nunca "H0 aceita"
# =============================================================================
textos <- c(
  narrativa,
  relatar_anova(r_na),
  pressupostos$Leitura,
  tukey[["Evidência"]],
  readLines("templates/funcoes_anova.R", warn = FALSE, encoding = "UTF-8"),
  readLines("modules/mod_anova.R", warn = FALSE, encoding = "UTF-8")
)
stopifnot(
  !any(grepl(paste("H0", "aceita"), textos, fixed = TRUE)),
  !any(grepl("H0 mantida", textos, fixed = TRUE)),
  any(grepl("não houve evidência suficiente para rejeitar h0",
            tolower(narrativa), fixed = TRUE)) ||
    any(grepl("rejeitou-se h0", tolower(narrativa), fixed = TRUE))
)

# =============================================================================
# 4. Gráficos
# =============================================================================
g_principal <- grafico_anova(r)
g_residuos <- grafico_diagnosticos_anova(r, "residuos")
g_qq <- grafico_diagnosticos_anova(r, "qq")
geoms_principal <- vapply(g_principal$layers, function(camada) class(camada$geom)[1],
                          character(1))
dados_barras <- ggplot2::ggplot_build(g_principal)$data[[1]]
stopifnot(
  inherits(g_principal, "ggplot"),
  inherits(g_residuos, "ggplot"),
  inherits(g_qq, "ggplot"),
  # Barras, hastes de IC e letras — não mais boxplot.
  "GeomCol" %in% geoms_principal,
  "GeomErrorbar" %in% geoms_principal,
  "GeomText" %in% geoms_principal,
  !("GeomBoxplot" %in% geoms_principal),
  # Nenhuma camada conecta as médias por linha entre níveis nominais.
  !("GeomLine" %in% geoms_principal),
  # Em gráfico de barras o eixo Y começa em zero.
  isTRUE(min(dados_barras$ymin, na.rm = TRUE) == 0)
)

# --- Núcleo das letras: casos conhecidos --------------------------------------
medias_teste <- c(alto = 30, medio = 20, baixo = 10)
pares_teste <- matrix(c("medio", "alto", "baixo", "alto", "baixo", "medio"), nrow = 2)
# Nada difere -> todos partilham a letra "a".
stopifnot(identical(
  unname(anova_letras_tukey(pares_teste, c(0.9, 0.9, 0.9), medias_teste)),
  c("a", "a", "a")
))
# Tudo difere -> uma letra por grupo, "a" no maior.
stopifnot(identical(
  anova_letras_tukey(pares_teste, c(0.001, 0.001, 0.001), medias_teste),
  c(alto = "a", medio = "b", baixo = "c")
))
# Só os extremos diferem -> o do meio compartilha letra com os dois.
letras_meio <- anova_letras_tukey(pares_teste, c(0.9, 0.001, 0.9), medias_teste)
stopifnot(
  nchar(letras_meio[["medio"]]) == 2L,
  letras_meio[["alto"]] != letras_meio[["baixo"]],
  grepl(letras_meio[["alto"]], letras_meio[["medio"]], fixed = TRUE),
  grepl(letras_meio[["baixo"]], letras_meio[["medio"]], fixed = TRUE)
)

# =============================================================================
# 5. Benchmarks do dataset de homologação
# =============================================================================
arquivo_treino <- file.path("dados", "Treino-Transformacoes.xlsx")
if (file.exists(arquivo_treino) && requireNamespace("readxl", quietly = TRUE)) {
  bruto <- as.data.frame(readxl::read_excel(arquivo_treino, sheet = "biometria"))
  # Base Compartilhada: padronizar texto de especie/local e remover duplicatas.
  arrumar_texto <- function(x) tolower(gsub("\\s+", " ", trimws(as.character(x))))
  bruto$especie <- arrumar_texto(bruto$especie)
  bruto$local <- arrumar_texto(bruto$local)
  compartilhada <- bruto[!duplicated(bruto), , drop = FALSE]
  # Base Derivada A: remover linhas com NA em profundidade_m.
  base_anova <- compartilhada[!is.na(compartilhada$profundidade_m), , drop = FALSE]

  rb <- calcular_anova(base_anova, "profundidade_m", "especie")
  medias <- stats::setNames(rb$descritivos_df$Media, rb$descritivos_df$Grupo)
  contagens <- stats::setNames(rb$descritivos_df$N, rb$descritivos_df$Grupo)

  stopifnot(
    nrow(compartilhada) == 68L,
    rb$n == 68L,
    rb$n_grupos == 5L,
    rb$df_entre == 4L,
    rb$df_dentro == 63L,
    quase_igual(rb$f_anova, 2.831, 0.01),
    quase_igual(rb$p_anova, 0.0318, 0.001),
    quase_igual(rb$eta2, 0.152, 0.01),
    quase_igual(rb$omega2, 0.097, 0.01),
    quase_igual(rb$levene_p, 0.2458, 0.005),
    quase_igual(unname(contagens[["bagre"]]), 6, 0),
    quase_igual(unname(contagens[["corvina"]]), 26, 0),
    quase_igual(unname(contagens[["pargo"]]), 7, 0),
    quase_igual(unname(contagens[["pescada amarela"]]), 14, 0),
    quase_igual(unname(contagens[["sardinha"]]), 15, 0),
    quase_igual(unname(medias[["bagre"]]), 28.1, 0.1),
    quase_igual(unname(medias[["corvina"]]), 19.7, 0.1),
    quase_igual(unname(medias[["pargo"]]), 26.5, 0.1),
    quase_igual(unname(medias[["pescada amarela"]]), 29.9, 0.1),
    quase_igual(unname(medias[["sardinha"]]), 21.6, 0.1)
  )
  if (!is.na(rb$sh_p)) stopifnot(quase_igual(rb$sh_p, 0.7035, 0.01))

  # O benchmark é conferido na camada numérica; a tabela é apresentação.
  par_alvo <- rb$tukey_df[rb$tukey_df$Comparacao == "pescada amarela-corvina", ]
  stopifnot(nrow(par_alvo) == 1L, quase_igual(par_alvo$p_adj, 0.0300, 0.005))
  tukey_b <- arrumar_tukey_anova(rb)
  stopifnot(
    "pescada amarela-corvina" %in% tukey_b[["Par comparado"]],
    # Ordenada por p ajustado: o par com evidência aparece antes dos demais.
    identical(tukey_b[["Par comparado"]][1], "pescada amarela-corvina")
  )

  # Base Derivada B — gráficos: corvina com comprimento e peso completos.
  base_graficos <- compartilhada[
    compartilhada$especie == "corvina" &
      !is.na(compartilhada$comprimento_cm) &
      !is.na(compartilhada$peso_g), , drop = FALSE
  ]
  stopifnot(
    nrow(base_graficos) == 19L,
    quase_igual(min(base_graficos$comprimento_cm), 13.4, 0.05),
    quase_igual(max(base_graficos$comprimento_cm), 33.1, 0.05),
    quase_igual(min(base_graficos$peso_g), 33, 0.5),
    quase_igual(max(base_graficos$peso_g), 340, 0.5)
  )
  cat("[OK] Benchmarks do Treino-Transformacoes.xlsx conferem.\n")
} else {
  cat("[AVISO] Treino-Transformacoes.xlsx indisponível: benchmarks não verificados.\n")
}

# =============================================================================
# 6. Estado Shiny: execução explícita e pendência
# =============================================================================
dados_shiny <- data.frame(
  profundidade_m = c(10, 12, 11, 20, 22, 21, 30, 33, 31, 15, 16, 14),
  cpue = c(1, 2, 1.5, 3, 3.4, 2.8, 5, 5.2, 4.8, 2, 2.2, 1.9),
  especie = rep(c("bagre", "corvina", "pargo", "sardinha"), each = 3),
  stringsAsFactors = FALSE
)
dados_rv <- reactive(dados_shiny)
info_rv <- reactive(list(
  source = "local", file_name = "teste.xlsx", excel_sheet = "biometria",
  csv_header = TRUE, csv_sep = ",", csv_dec = "."
))

testServer(mod_anova_server, args = list(data_rv = dados_rv, import_info = info_rv), {
  session$setInputs(
    var_y = "profundidade_m", var_x = "especie", conf_level = 95,
    graph_theme = "minimal", custom_title = "", custom_label_x = "",
    custom_label_y = ""
  )
  # Configurar não executa.
  stopifnot(identical(exec_ctrl$estado(), "aguardando"))

  session$setInputs(executar_analise = 1)
  stopifnot(identical(exec_ctrl$estado(), "atualizada"))

  estado <- estado_execucao()
  stopifnot(
    identical(estado$analise_id, "anova"),
    identical(estado$tipo, "anova_um_fator"),
    identical(estado$parametros$resposta, "profundidade_m"),
    identical(estado$parametros$fator, "especie"),
    identical(estado$parametros$ajuste_comparacoes, "tukey"),
    quase_igual(estado$parametros$nivel_confianca, 0.95),
    all(c("narrativa", "descritivos", "tabela", "comparacoes", "grafico",
          "pressupostos", "diagnosticos") %in% estado$saidas_disponiveis),
    !("console" %in% estado$saidas_disponiveis),
    estado$resultado_resumo$n == 12L,
    estado$resultado_resumo$grupos == 4L,
    estado$resultado_resumo$gl_1 == 3L,
    estado$resultado_resumo$gl_2 == 8L,
    is.finite(estado$resultado_resumo$f),
    is.finite(estado$resultado_resumo$p),
    is.finite(estado$resultado_resumo$eta2)
  )
  # Todas as saídas declaradas são aceitas pelo registro central.
  stopifnot(isTRUE(execucoes_validar_estado(estado)))

  # Mudar Y deixa pendente.
  session$setInputs(var_y = "cpue")
  stopifnot(identical(exec_ctrl$estado(), "pendente"))
  session$setInputs(executar_analise = 2)
  stopifnot(
    identical(exec_ctrl$estado(), "atualizada"),
    identical(estado_execucao()$parametros$resposta, "cpue")
  )

  # Mudar X deixa pendente.
  session$setInputs(var_x = "profundidade_m")
  stopifnot(identical(exec_ctrl$estado(), "pendente"))
  session$setInputs(var_x = "especie")
  session$setInputs(executar_analise = 3)
  stopifnot(identical(exec_ctrl$estado(), "atualizada"))

  # Mudar o nível de confiança deixa pendente.
  session$setInputs(conf_level = 99)
  stopifnot(identical(exec_ctrl$estado(), "pendente"))
})

# Mudar a base deixa a execução pendente.
base_shiny_rv <- reactiveVal(dados_shiny)
testServer(mod_anova_server, args = list(data_rv = base_shiny_rv, import_info = info_rv), {
  session$setInputs(
    var_y = "profundidade_m", var_x = "especie", conf_level = 95,
    graph_theme = "minimal", custom_title = "", custom_label_x = "",
    custom_label_y = ""
  )
  session$setInputs(executar_analise = 1)
  stopifnot(identical(exec_ctrl$estado(), "atualizada"))
  base_shiny_rv(dados_shiny[dados_shiny$especie != "sardinha", , drop = FALSE])
  session$flushReact()
  stopifnot(identical(exec_ctrl$estado(), "pendente"))
})

# Configuração inválida não produz estado registrável.
dados_invalidos <- data.frame(
  profundidade_m = c(10, 12, 11),
  especie = rep("bagre", 3),
  stringsAsFactors = FALSE
)
testServer(
  mod_anova_server,
  args = list(data_rv = reactive(dados_invalidos), import_info = info_rv),
  {
    session$setInputs(
      var_y = "profundidade_m", var_x = "especie", conf_level = 95,
      graph_theme = "minimal", custom_title = "", custom_label_x = "",
      custom_label_y = ""
    )
    session$setInputs(executar_analise = 1)
    stopifnot(!identical(exec_ctrl$estado(), "atualizada"))
    registravel <- tryCatch({ estado_execucao(); TRUE }, error = function(e) FALSE)
    stopifnot(!isTRUE(registravel))
  }
)

# =============================================================================
# 7. Registro central aceita a ANOVA e vincula a base derivada
# =============================================================================
estado_anova <- list(
  analise_id = "anova",
  tipo = "anova_um_fator",
  titulo = "Profundidade de captura entre espécies",
  parametros = list(
    resposta = "profundidade_m", fator = "especie",
    nivel_confianca = 0.95, ajuste_comparacoes = "tukey"
  ),
  saidas_disponiveis = c("narrativa", "descritivos", "tabela", "comparacoes",
                         "grafico", "pressupostos", "diagnosticos"),
  resultado_resumo = list(n = 68L, grupos = 5L, f = 2.831, gl_1 = 4L, gl_2 = 63L, p = 0.0318)
)
contexto_derivada <- list(
  base_id = "base_0001", base_objeto = "base_anova_profundidade_especie",
  nome_amigavel = "Profundidade de captura por espécie", base_tipo = "derivada",
  derivada = TRUE, finalidade = "anova", versao_receita = 2L
)
execucao <- execucoes_criar("execucao_0001", estado_anova, contexto_derivada, 7L)
stopifnot(
  identical(execucao$tipo, "anova_um_fator"),
  identical(execucao$base_objeto, "base_anova_profundidade_especie"),
  identical(execucao$base_tipo, "derivada"),
  identical(execucao$base_finalidade, "anova"),
  all(c("descritivos", "comparacoes") %in% execucao$saidas_disponiveis),
  "anova" %in% bases_finalidades,
  all(c("descritivos", "comparacoes") %in% names(comunicacao_rotulos_saidas))
)

estado_editorial <- comunicacao_sincronizar(comunicacao_estado_vazio(), list(execucao_0001 = execucao))
estado_editorial <- comunicacao_definir_item(
  estado_editorial, "execucao_0001",
  saidas_selecionadas = execucao$saidas_disponiveis,
  saidas_disponiveis = execucao$saidas_disponiveis
)
stopifnot(
  length(estado_editorial$itens$execucao_0001$saidas_selecionadas) == 7L,
  identical(unname(comunicacao_rotulos_saidas[["descritivos"]]), "Resumo por grupo"),
  identical(unname(comunicacao_rotulos_saidas[["comparacoes"]]), "Comparações múltiplas")
)

cat("OK: ANOVA integrada — núcleo, validações, benchmarks e estado Shiny\n")
