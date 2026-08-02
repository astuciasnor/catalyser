# Testes de replay, QMD pedagógico e exportação da ANOVA — V16
# Executar a partir de inst/app.

source("app.R", local = TRUE)
# As funcoes de analise agora vivem no pacote. Este helper usa a versao
# instalada quando ela existe e, senao, carrega os arquivos de R/ direto do
# codigo-fonte - assim o teste roda antes e depois de instalar.
source(file.path("tests", "carregar_catalyser.R"), chdir = FALSE)

quase_igual <- function(x, y, tol = 1e-6) {
  isTRUE(is.finite(x) && is.finite(y) && abs(x - y) <= tol)
}

dados <- data.frame(
  id = seq_len(12),
  profundidade_m = c(10, 12, 11, 20, 22, 21, 30, 33, 31, 15, 16, 14),
  comprimento_cm = c(20, 22, 21, 25, 27, 26, 31, 33, 32, 18, 19, 17),
  peso_g = c(80, 95, 88, 150, 172, 161, 260, 305, 282, 110, 121, 102),
  especie = rep(c("bagre", "corvina", "pargo", "sardinha"), each = 3),
  stringsAsFactors = FALSE
)

parametros_anova <- list(
  resposta = "profundidade_m", fator = "especie",
  nivel_confianca = 0.95, ajuste_comparacoes = "tukey",
  tema = "classic", titulo_grafico = "",
  rotulo_x = "Espécie", rotulo_y = "Profundidade (m)"
)

# =============================================================================
# 1. catalyser_executar() reconhece anova_um_fator
# =============================================================================
execucao_replay <- list(
  id = "execucao_0001", tipo = "anova_um_fator",
  titulo = "Profundidade de captura entre espécies",
  parametros = parametros_anova
)
replay <- catalyser_executar(execucao_replay, dados)

stopifnot(
  inherits(replay, "resultado_catalyser"),
  inherits(replay$objeto, "aov"),
  is.character(replay$narrativa), nzchar(replay$narrativa),
  is.data.frame(replay$descritivos), nrow(replay$descritivos) == 4L,
  is.data.frame(replay$tabela), nrow(replay$tabela) == 3L,
  is.data.frame(replay$comparacoes), nrow(replay$comparacoes) == choose(4L, 2L),
  inherits(replay$grafico, "ggplot"),
  is.data.frame(replay$pressupostos), nrow(replay$pressupostos) == 3L,
  is.data.frame(replay$diagnosticos),
  is.character(replay$console), length(replay$console) > 0L,
  !any(grepl(paste("H0", "aceita"), replay$narrativa, fixed = TRUE)),
  identical(replay$grafico$labels$title, "profundidade_m por especie"),
  identical(replay$grafico$labels$x, "Espécie"),
  identical(replay$grafico$labels$y, "Profundidade (m)"),
  inherits(replay$grafico$theme$panel.grid, "element_blank")
)

# Os nomes dos componentes coincidem com as saídas declaradas pelo módulo.
saidas_modulo <- c("narrativa", "descritivos", "tabela", "comparacoes",
                   "grafico", "pressupostos", "diagnosticos")
stopifnot(all(saidas_modulo %in% names(replay)))

# =============================================================================
# 2. O replay reproduz os valores da interface
# =============================================================================
r_interface <- calcular_anova(dados, "profundidade_m", "especie", nivel_confianca = 0.95)
f_replay <- suppressWarnings(as.numeric(replay$tabela[["F"]][1]))
p_replay <- suppressWarnings(as.numeric(replay$tabela[["p-valor"]][1]))
indicador <- function(tabela, padrao) {
  tabela$Valor[grepl(padrao, tabela$Indicador, fixed = TRUE)][1]
}
eta_replay <- indicador(replay$diagnosticos, "Eta quadrado")
omega_replay <- indicador(replay$diagnosticos, "mega quadrado")

# A narrativa do Word (replay) segue a mesma regra da interface: não repete o
# que as tabelas mostram e remete a elas. Se as duas divergirem, o relatório
# passa a contar uma história diferente da tela.
narrativa_interface <- relatar_anova(r_interface)
for (texto in list(narrativa_interface, replay$narrativa)) {
  stopifnot(
    grepl("resumo por grupo", texto, fixed = TRUE),
    grepl("tabela de pressupostos", texto, fixed = TRUE),
    grepl("convenção de Cohen", texto, fixed = TRUE),
    !grepl("Shapiro", texto, fixed = TRUE),
    !grepl("Levene", texto, fixed = TRUE),
    !grepl("média = ", texto, fixed = TRUE),
    !grepl("H0 aceita", texto, fixed = TRUE)
  )
}
gl_esperado <- sprintf("F(%d; %d)", r_interface$df_entre, r_interface$df_dentro)
stopifnot(
  grepl(gl_esperado, narrativa_interface, fixed = TRUE),
  grepl(gl_esperado, replay$narrativa, fixed = TRUE)
)

stopifnot(
  quase_igual(f_replay, r_interface$f_anova, 1e-8),
  quase_igual(p_replay, r_interface$p_anova, 1e-10),
  quase_igual(eta_replay, r_interface$eta2, 1e-10),
  quase_igual(omega_replay, r_interface$omega2, 1e-10),
  indicador(replay$diagnosticos, "n analisado") == r_interface$n,
  identical(as.character(replay$descritivos$Grupo), r_interface$grupos)
)

# O replay usa somente os parâmetros congelados: trocar a resposta muda o resultado.
execucao_outra <- execucao_replay
execucao_outra$parametros$resposta <- "comprimento_cm"
replay_outra <- catalyser_executar(execucao_outra, dados)
stopifnot(!quase_igual(
  suppressWarnings(as.numeric(replay_outra$tabela[["F"]][1])), f_replay, 1e-8
))

# Casos incompletos são removidos e informados
dados_na <- dados
dados_na$profundidade_m[c(1, 5)] <- NA
replay_na <- catalyser_executar(execucao_replay, dados_na)
stopifnot(
  indicador(replay_na$diagnosticos, "n analisado") == 10,
  indicador(replay_na$diagnosticos, "Casos exclu") == 2
)

# Validações do replay
stopifnot(
  inherits(tryCatch(catalyser_executar(execucao_replay, dados[c("especie")]),
                    error = function(e) e), "error"),
  inherits(tryCatch(catalyser_anova(dados, list(resposta = "especie", fator = "especie")),
                    error = function(e) e), "error")
)

# =============================================================================
# 3. Código pedagógico no QMD
# =============================================================================
codigo <- exportacao_codigo_estudo(list(
  tipo = "anova_um_fator",
  base_tipo = "derivada",
  base_objeto = "base_anova_profundidade_especie",
  parametros = parametros_anova
))

stopifnot(
  any(grepl("dados <- base_anova_profundidade_especie", codigo, fixed = TRUE)),
  any(grepl("anova-profundidade-m-base", codigo, fixed = TRUE)),
  any(grepl("variavel_resposta <-", codigo, fixed = TRUE)),
  any(grepl("resumo_por_grupo <-", codigo, fixed = TRUE)),
  any(grepl("stats::reformulate", codigo, fixed = TRUE)),
  any(grepl("stats::aov(formula_anova, data = dados_anova)", codigo, fixed = TRUE)),
  any(grepl("stats::TukeyHSD", codigo, fixed = TRUE)),
  any(grepl("car::leveneTest", codigo, fixed = TRUE)),
  any(grepl("dados_anova[[variavel_resposta]]", codigo, fixed = TRUE)),
  any(grepl("dados_anova[[variavel_fator]]", codigo, fixed = TRUE)),
  any(grepl("center = stats::median", codigo, fixed = TRUE)),
  any(grepl("stats::shapiro.test", codigo, fixed = TRUE)),
  any(grepl("effectsize::eta_squared", codigo, fixed = TRUE)),
  any(grepl("effectsize::omega_squared", codigo, fixed = TRUE))
)

# =============================================================================
# 4. Projeto R completo com ANOVA + duas execuções gráficas
# =============================================================================
criar_execucao <- function(id, tipo, titulo, parametros, saidas,
                           base_id = "dados_analise", base_objeto = "dados_analise",
                           base_tipo = "compartilhada", base_versao = NULL) {
  list(
    id = id, analise_id = tipo, tipo = tipo, titulo = titulo,
    parametros = parametros, saidas_disponiveis = saidas,
    resultado_resumo = list(), codigo_r = NULL, revisao_origem = 7L,
    criada_em = Sys.time(), atualizada_em = Sys.time(), versao = 1L,
    base_id = base_id, base_objeto = base_objeto, base_nome = base_objeto,
    base_tipo = base_tipo, base_derivada = identical(base_tipo, "derivada"),
    base_finalidade = if (identical(tipo, "anova_um_fator")) "anova" else "graficos",
    base_versao_receita = base_versao, depende_origem = TRUE
  )
}

ramo_anova <- bases_novo_registro(
  "base_0001", "Profundidade de captura por espécie",
  "base_anova_profundidade_especie", finalidade = "anova", revisao_origem = 7L
)
ramo_anova <- bases_adicionar_etapa(
  list(ramo_anova), ramo_anova$id, "filtrar",
  list(coluna = "profundidade_m", origem = "numerica", operador = ">=", valor = 0),
  dados_validacao = dados
)[[1]]
ramo_anova$estado <- "pronta"

cache <- list(base_0001 = list(
  df = dados, erros = list(), revisao_origem = 7L,
  versao_receita = ramo_anova$versao, linhas = nrow(dados), colunas = ncol(dados)
))

e_anova <- criar_execucao(
  "execucao_0001", "anova_um_fator", "Profundidade de captura entre espécies",
  parametros_anova, saidas_modulo,
  base_id = ramo_anova$id, base_objeto = ramo_anova$nome_r,
  base_tipo = "derivada", base_versao = ramo_anova$versao
)
e_grafico_1 <- criar_execucao(
  "execucao_0002", "grafico_linhas", "Comprimento das corvinas por observação",
  list(x = "id", y = "comprimento_cm", grupo = "none",
       mostrar_pontos = TRUE, espessura_linha = 1, tema = "minimal",
       posicao_legenda = "right", rotulo_x = "Observação", rotulo_y = "Comprimento (cm)"),
  "grafico"
)
e_grafico_2 <- criar_execucao(
  "execucao_0003", "grafico_linhas", "Peso das corvinas por observação",
  list(x = "id", y = "peso_g", grupo = "none",
       mostrar_pontos = TRUE, espessura_linha = 1.4, tema = "minimal",
       posicao_legenda = "right", rotulo_x = "Observação", rotulo_y = "Peso (g)"),
  "grafico"
)

registro <- list(
  execucao_0001 = e_anova, execucao_0002 = e_grafico_1, execucao_0003 = e_grafico_2
)
estado <- comunicacao_sincronizar(comunicacao_estado_vazio(), registro)
estado <- comunicacao_definir_item(
  estado, "execucao_0001", saidas_selecionadas = saidas_modulo,
  saidas_disponiveis = e_anova$saidas_disponiveis
)
manifesto <- comunicacao_manifesto(
  estado, registro,
  stats::setNames(as.list(rep("Atualizada", 3L)), names(registro)),
  list(metodos = "ANOVA de um fator sobre a Base Derivada.")
)

stopifnot(
  exportacao_validar_manifesto(manifesto)$ok,
  exportacao_validar_manifesto(manifesto, exigir_word = TRUE)$ok,
  # A ANOVA vem primeiro; os dois gráficos coexistem depois.
  identical(names(manifesto$execucoes), c("execucao_0001", "execucao_0002", "execucao_0003")),
  length(manifesto$execucoes$execucao_0001$saidas_word) == 7L
)

raiz <- tempfile("teste_anova_v16_")
dir.create(raiz)
on.exit(unlink(raiz, recursive = TRUE, force = TRUE), add = TRUE)

argumentos <- list(
  nome_projeto = "anova_v16",
  dados_brutos = dados, base_resolvida = dados, dados_analise = dados,
  pipeline = list(), base_externa = NULL,
  registro_bases = list(ramo_anova), cache_bases = cache,
  registro_execucoes = registro, manifesto = manifesto, revisao_origem = 7L,
  import_info = list(source = "package", package_dataset = "biometria"),
  templates_dir = "templates"
)

projeto <- do.call(exportacao_criar_projeto, c(list(destino = raiz), argumentos))
qmd <- readLines(file.path(projeto, "relatorio.qmd"), warn = FALSE, encoding = "UTF-8")
scripts <- list.files(file.path(projeto, "R"), pattern = "^02_execucao_.*\\.R$", full.names = TRUE)
script_anova <- readLines(scripts[[1]], warn = FALSE, encoding = "UTF-8")
base_compartilhada <- readLines(
  file.path(projeto, "R", "01_base_compartilhada.R"), warn = FALSE, encoding = "UTF-8"
)
scripts_texto <- lapply(
  scripts,
  readLines,
  warn = FALSE,
  encoding = "UTF-8"
)

rotulos <- trimws(sub("^#\\|\\s*label:", "", grep("^#\\|\\s*label:", qmd, value = TRUE)))
stopifnot(
  # Labels dizem a intenção científica, não o número interno da execução.
  "anova-profundidade-m-codigo" %in% rotulos,
  "anova-profundidade-m-modelo" %in% rotulos,
  "anova-profundidade-m-tukey" %in% rotulos,
  "anova-profundidade-m-resumo-grupos" %in% rotulos,
  "anova-profundidade-m-replay" %in% rotulos,
  # Nenhum label sobrou com sublinhado ou com o ID cru como raiz.
  !any(grepl("_", rotulos, fixed = TRUE)),
  !any(grepl("^codigo-execucao", rotulos)),
  !any(grepl("^executar-execucao", rotulos)),
  # Os dois gráficos têm Y diferente, então a própria variável já distingue os
  # labels — sem precisar do ID da execução.
  "linhas-comprimento-cm-grafico" %in% rotulos,
  "linhas-peso-g-grafico" %in% rotulos,
  !any(duplicated(rotulos))
)

stopifnot(
  length(scripts) == 3L,
  # Um único script constrói a Base Compartilhada; os antigos 00/02 e os 03 saíram.
  file.exists(file.path(projeto, "R", "01_base_compartilhada.R")),
  # A pasta R/ tem apenas 01_base_compartilhada.R e os 02_execucao_*.R.
  # Os antigos 00_funcoes_projeto.R e 03_<base derivada>.R nao existem mais.
  length(list.files(file.path(projeto, "R"), pattern = "^0[03]_.*\\.R$")) == 0L,
  length(list.files(file.path(projeto, "R"), pattern = "^01_.*\\.R$")) == 1L,
  # E o console nao aparece em nenhum chunk do relatorio.
  !any(grepl("[['console']]", qmd, fixed = TRUE)),
  !any(grepl("-console", qmd, fixed = TRUE)),
  any(grepl("catalyser_conferir_base(", base_compartilhada, fixed = TRUE)),
  # O QMD chama esse script uma única vez.
  sum(grepl("source(file.path('R', '01_base_compartilhada.R'), local = TRUE)",
            qmd, fixed = TRUE)) == 1L,
  # E constrói a base derivada da ANOVA no chunk da própria análise.
  any(grepl("#| label: anova-profundidade-m-base", qmd, fixed = TRUE)),
  any(grepl("base_anova_profundidade_especie <- dados", qmd, fixed = TRUE)),
  any(grepl("dados_da_analise <- base_anova_profundidade_especie", qmd, fixed = TRUE)),
  # O replay usa a base do chunk; o sys.source opaco saiu do relatório.
  any(grepl("catalyser_executar(analises_registradas[[", qmd, fixed = TRUE)),
  # As funcoes vem do pacote instalado, nao mais de um arquivo copiado.
  any(grepl("library(catalyser)", qmd, fixed = TRUE)),
  !file.exists(file.path(projeto, "R", "00_funcoes_projeto.R")),
  !any(grepl("sys.source(", qmd, fixed = TRUE)),
  all(vapply(scripts_texto, function(x)
    any(grepl("registro_execucoes.rds", x, fixed = TRUE)), logical(1))),
  all(vapply(scripts_texto, function(x)
    !any(grepl("execucao <- structure(", x, fixed = TRUE)), logical(1))),
  any(grepl("base_anova_profundidade_especie", script_anova, fixed = TRUE)),
  # O script conta a historia em tres partes nomeadas.
  any(grepl("PARTE 1 - PREPARAR OS DADOS DESTA ANÁLISE", script_anova, fixed = TRUE)),
  any(grepl("PARTE 2 - A ANÁLISE", script_anova, fixed = TRUE)),
  any(grepl("PARTE 3 - REFAZER O RESULTADO DO RELATÓRIO", script_anova, fixed = TRUE)),
  any(grepl("# Pergunta   :", script_anova, fixed = TRUE)),
  any(grepl("library(catalyser)", script_anova, fixed = TRUE)),
  any(grepl("variavel_resposta <-", script_anova, fixed = TRUE)),
  any(grepl("registro_execucoes.rds", script_anova, fixed = TRUE)),
  !any(grepl("execucao <- structure(", script_anova, fixed = TRUE)),
  any(grepl("catalyser_executar(configuracao, dados)", script_anova, fixed = TRUE)),
  any(grepl("stats::aov(formula_anova, data = dados_anova)", qmd, fixed = TRUE)),
  any(grepl("variavel_resposta <-", qmd, fixed = TRUE)),
  any(grepl("effectsize::eta_squared", qmd, fixed = TRUE)),
  any(grepl("### Resumo por grupo", qmd, fixed = TRUE)),
  any(grepl("### Comparações múltiplas", qmd, fixed = TRUE)),
  # O comentário separa método (para estudo) de mecanismo editorial.
  any(grepl("Método: o código abaixo fica explícito", qmd, fixed = TRUE)),
  any(grepl("Mecanismo editorial", qmd, fixed = TRUE)),
  any(grepl("Profundidade de captura entre espécies", qmd, fixed = TRUE)),
  # As duas execuções gráficas continuam separadas no relatório.
  any(grepl("Comprimento das corvinas por observação", qmd, fixed = TRUE)),
  any(grepl("Peso das corvinas por observação", qmd, fixed = TRUE)),
  any(grepl("grafico_linhas <-", qmd, fixed = TRUE)),
  any(grepl("titulo_grafico <-", qmd, fixed = TRUE)),
  any(grepl("comprimento_cm", qmd, fixed = TRUE)),
  any(grepl("peso_g", qmd, fixed = TRUE))
)

# Todos os scripts numerados executam fora da CatalyseR.
anterior <- getwd()
setwd(projeto)
for (arquivo in scripts) {
  ambiente_script <- new.env(parent = globalenv())
  sys.source(arquivo, envir = ambiente_script)
  stopifnot(length(ls(ambiente_script, pattern = "^resultado_execucao_")) == 1L)
}
setwd(anterior)

zip_saida <- file.path(raiz, "projeto_anova.zip")
do.call(exportacao_empacotar_projeto, c(list(file = zip_saida), argumentos))
stopifnot(file.exists(zip_saida), file.info(zip_saida)$size > 0)

if (nzchar(unname(Sys.which("quarto")))) {
  word_saida <- file.path(raiz, "relatorio_anova.docx")
  do.call(exportacao_renderizar_word, c(list(file = word_saida), argumentos))
  stopifnot(file.exists(word_saida), file.info(word_saida)$size > 0)
  cat("[OK] Word da ANOVA renderizado pelo Quarto.\n")
} else {
  cat("[AVISO] Quarto CLI ausente: renderização do Word não verificada.\n")
}

cat("OK: ANOVA reproduzida no Projeto R, no QMD pedagógico e no Word integrado\n")
