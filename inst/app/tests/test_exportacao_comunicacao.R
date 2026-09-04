source("app.R", local = TRUE)
# As funcoes de analise agora vivem no pacote. Este helper usa a versao
# instalada quando ela existe e, senao, carrega os arquivos de R/ direto do
# codigo-fonte - assim o teste roda antes e depois de instalar.
source(file.path("tests", "carregar_catalyser.R"), chdir = FALSE)

bloco_console <- exportacao_bloco_componente(
  "resultado_execucao_0001", "execucao_0001", "console"
)
stopifnot(any(grepl("```text", bloco_console, fixed = TRUE)))
stopifnot(any(grepl("collapse = '\\n'", bloco_console, fixed = TRUE)))

criar_execucao_exportacao <- function(id, tipo, titulo, parametros, saidas,
                                      base_id = "dados_analise",
                                      base_objeto = "dados_analise",
                                      base_tipo = "compartilhada",
                                      base_versao = NULL) {
  list(
    id = id,
    analise_id = tipo,
    tipo = tipo,
    titulo = titulo,
    parametros = parametros,
    saidas_disponiveis = saidas,
    resultado_resumo = list(),
    codigo_r = NULL,
    revisao_origem = 7L,
    criada_em = Sys.time(),
    atualizada_em = Sys.time(),
    versao = 1L,
    base_id = base_id,
    base_objeto = base_objeto,
    base_nome = base_objeto,
    base_tipo = base_tipo,
    base_derivada = identical(base_tipo, "derivada"),
    base_finalidade = "geral",
    base_versao_receita = base_versao,
    depende_origem = TRUE
  )
}

codigo_regressao <- exportacao_codigo_estudo(list(
  tipo = "regressao_linear",
  base_tipo = "derivada",
  base_objeto = "base_regressao",
  parametros = list(resposta = "captura", preditor = "esforco")
))
codigo_t <- exportacao_codigo_estudo(list(
  tipo = "teste_t_two_ind",
  base_tipo = "derivada",
  base_objeto = "base_teste_t",
  parametros = list(
    resposta = "captura", grupo = "sexo", alternativa = "two.sided",
    nivel_confianca = 0.95, variancias_iguais = FALSE
  )
))
stopifnot(
  any(grepl("dados <- base_regressao", codigo_regressao, fixed = TRUE)),
  any(grepl("`tratar`", codigo_regressao, fixed = TRUE)),
  any(grepl("stats::lm", codigo_regressao, fixed = TRUE)),
  any(grepl("stats::t.test", codigo_t, fixed = TRUE)),
  any(grepl("var.equal = FALSE", codigo_t, fixed = TRUE))
)

dados <- data.frame(
  ano = 2019:2024,
  captura = c(10, 12, 13, 15, 18, 20),
  esforco = c(5, 5, 6, 7, 8, 9),
  sexo = factor(c("F", "M", "F", "M", "F", "M"))
)
dados_analise_fixture <- dados
dados_analise_fixture$captura_dobro <- dados$captura * 2

# --- A conferência da base reconhece equivalência e detecta divergência -------
fotografia <- tempfile(fileext = ".rds")
saveRDS(dados, fotografia)
divergente <- dados
divergente$coluna_extra <- 1
invisible(utils::capture.output({
  conferencia_igual <- catalyser_conferir_base(dados, fotografia, rotulo = "teste")
  conferencia_extra <- catalyser_conferir_base(divergente, fotografia, rotulo = "teste")
  conferencia_curta <- catalyser_conferir_base(dados[1:3, ], fotografia, rotulo = "teste")
  conferencia_ausente <- catalyser_conferir_base(dados, tempfile(), rotulo = "teste")
}))
stopifnot(
  isTRUE(conferencia_igual),
  !isTRUE(conferencia_extra),
  !isTRUE(conferencia_curta),
  !isTRUE(conferencia_ausente)
)
unlink(fotografia)

ramo <- bases_novo_registro(
  "base_0001", "Base da regressão", "base_regressao",
  finalidade = "geral", revisao_origem = 7L
)
ramo <- bases_adicionar_etapa(
  list(ramo), ramo$id, "filtrar",
  list(coluna = "ano", origem = "numerica", operador = ">=", valor = 2020),
  dados_validacao = dados
)[[1]]
ramo$estado <- "pronta"
cache <- list(base_0001 = list(
  df = dados[dados$ano >= 2020, ], erros = list(), revisao_origem = 7L,
  versao_receita = ramo$versao, linhas = sum(dados$ano >= 2020), colunas = ncol(dados)
))

e1 <- criar_execucao_exportacao(
  "execucao_0001", "grafico_linhas", "Captura ao longo dos anos",
  list(
    x = "ano", y = "captura", grupo = "none", mostrar_pontos = TRUE,
    espessura_linha = 1, tema = "minimal", posicao_legenda = "right",
    rotulo_x = "Ano", rotulo_y = "Captura"
  ),
  "grafico"
)
e2 <- criar_execucao_exportacao(
  "execucao_0002", "regressao_linear", "Captura por esforço",
  list(
    resposta = "captura", preditor = "esforco", grupo = "none",
    tipo_modelo = "linear", regressao_por_grupo = FALSE,
    mostrar_equacao = TRUE, tema = "minimal"
  ),
  c("narrativa", "tabela", "grafico", "pressupostos", "diagnosticos", "console"),
  base_id = ramo$id, base_objeto = ramo$nome_r, base_tipo = "derivada",
  base_versao = ramo$versao
)
e3 <- criar_execucao_exportacao(
  "execucao_0003", "estatistica_descritiva", "Resumo da captura",
  list(
    variaveis = "captura", grupo = "none",
    metricas = list(
      n = TRUE, nas = TRUE, media = TRUE, mediana = TRUE,
      desvio_padrao = TRUE, variancia = FALSE,
      minimo_maximo = TRUE, quartis = TRUE
    )
  ),
  "tabela"
)
registro <- list(execucao_0001 = e1, execucao_0002 = e2, execucao_0003 = e3)

estado <- comunicacao_sincronizar(comunicacao_estado_vazio(), registro)

# O console nunca entra no relatorio, para nenhuma analise: e regra do projeto,
# nao escolha por execucao. A e2 declara "console" entre as saidas disponiveis;
# ainda assim ele nao pode aparecer como conteudo do Word.
stopifnot(
  "console" %in% e2$saidas_disponiveis,
  !("console" %in% comunicacao_saidas_relatorio()),
  !any(vapply(estado$itens, function(x) "console" %in% x$saidas_selecionadas, logical(1)))
)
estado <- comunicacao_definir_item(
  estado, "execucao_0001", saidas_selecionadas = "grafico",
  saidas_disponiveis = e1$saidas_disponiveis
)
estado <- comunicacao_definir_item(
  estado, "execucao_0002", incluir_word = FALSE,
  saidas_disponiveis = e2$saidas_disponiveis
)
estado <- comunicacao_definir_item(
  estado, "execucao_0003", saidas_selecionadas = "tabela",
  saidas_disponiveis = e3$saidas_disponiveis
)
manifesto <- comunicacao_manifesto(
  estado, registro,
  stats::setNames(as.list(rep("Atualizada", 3L)), names(registro)),
  list(
    introducao = "A captura pesqueira foi acompanhada entre 2019 e 2024.",
    metodos = "Foram usados resumos, gráficos e regressão linear.",
    discussao = "Os resultados devem ser interpretados no contexto do esforço.",
    conclusao = "O projeto preserva todas as execuções."
  )
)

stopifnot(
  exportacao_validar_manifesto(manifesto)$ok,
  exportacao_validar_manifesto(manifesto, exigir_word = TRUE)$ok
)
manifesto_ruim <- manifesto
manifesto_ruim$execucoes$execucao_0001$estado_dependencia <- "Precisa atualizar"
stopifnot(!exportacao_validar_manifesto(manifesto_ruim)$ok)

raiz <- tempfile("teste_exportacao_3e_")
dir.create(raiz)
on.exit(unlink(raiz, recursive = TRUE, force = TRUE), add = TRUE)

argumentos <- list(
  nome_projeto = "captura_teste",
  dados_brutos = dados,
  base_resolvida = dados,
  # `dados_analise` é o replay do pipeline sobre a base resolvida. Precisa conter
  # a coluna criada pelo tratamento `calcular`, senão a conferência do projeto
  # exportado acusa divergência — e estaria certa em acusar.
  dados_analise = dados_analise_fixture,
  pipeline = list(list(
    tipo = "calcular", params = list(nome = "captura_dobro", expr = "captura * 2"),
    ativa = TRUE
  )),
  base_externa = NULL,
  registro_bases = list(ramo),
  cache_bases = cache,
  registro_execucoes = registro,
  manifesto = manifesto,
  revisao_origem = 7L,
  import_info = list(source = "package", package_dataset = "captura_teste"),
  templates_dir = "templates"
)

projeto <- do.call(exportacao_criar_projeto, c(list(destino = raiz), argumentos))
caminho_qmd <- file.path(projeto, "relatorios", "relatorio.qmd")
qmd <- readLines(caminho_qmd, warn = FALSE, encoding = "UTF-8")
leiame <- readLines(file.path(projeto, "README.md"), warn = FALSE, encoding = "UTF-8")

stopifnot(
  dir.exists(projeto),
  # Fase C: a árvore é a do EAPACaderno. Sem R/ (a análise mora no relatório),
  # sem resultados/ (tabelas e figuras nascem no Word), com imagens/ vazia.
  !dir.exists(file.path(projeto, "R")),
  !dir.exists(file.path(projeto, "resultados")),
  dir.exists(file.path(projeto, "imagens")),
  file.exists(file.path(projeto, "metadados", "manifesto_editorial.rds")),
  any(grepl("chunk `importar`", leiame, fixed = TRUE)),
  any(grepl("clique em **Render**", leiame, fixed = TRUE)),
  !any(grepl("02_execucao", leiame, fixed = TRUE)),
  !any(grepl("04_analisar", leiame, fixed = TRUE)),
  any(grepl("A pasta `metadados/`", leiame, fixed = TRUE)),
  any(grepl("não precisa ser aberta nem", leiame, fixed = TRUE)),
  # O modelo de página do Word fica ao lado do relatório, como no projeto-modelo.
  file.exists(file.path(projeto, "relatorios", "custom-reference.docx")),
  !file.exists(file.path(projeto, "custom-reference.docx")),
  !file.exists(file.path(projeto, "relatorio.qmd")),
  any(grepl("Captura ao longo dos anos", qmd, fixed = TRUE)),
  any(grepl("Resumo da captura", qmd, fixed = TRUE)),
  any(grepl("Código R essencial desta execução", qmd, fixed = TRUE)),
  any(grepl("ggplot2::ggplot", qmd, fixed = TRUE)),
  any(grepl("summary(dados[variaveis])", qmd, fixed = TRUE)),
  # Relatório orgânico: o gráfico de linhas (código validado) roda de verdade,
  # em silêncio; a descritiva ainda fica só para leitura. Os chunks importar e
  # tratar também rodam em silêncio (output: false).
  sum(grepl("#| eval: false", qmd, fixed = TRUE)) == 1L,
  sum(grepl("#| output: false", qmd, fixed = TRUE)) == 3L,
  any(grepl("#| label: linhas-captura-analise", qmd, fixed = TRUE)),
  any(grepl("#| label: linhas-captura-resultado", qmd, fixed = TRUE)),
  any(grepl("A análise, passo a passo", qmd, fixed = TRUE)),
  any(grepl("Apresentação: a mesma análise", qmd, fixed = TRUE)),
  any(grepl("**Pergunta:** como 'captura' se comporta ao longo de 'ano'?", qmd, fixed = TRUE)),
  sum(grepl("#| include: false", qmd, fixed = TRUE)) >= 3L,
  !any(grepl("## Captura por esforço", qmd, fixed = TRUE))
)

# --- Os chunks importar e tratar levam a planilha até a conferência -----------
stopifnot(
  any(grepl("#| label: importar", qmd, fixed = TRUE)),
  any(grepl("#| label: tratar", qmd, fixed = TRUE)),
  any(grepl("read_excel(caminho_planilha, sheet = aba_planilha)", qmd, fixed = TRUE)),
  any(grepl('here("dados", "brutos", "captura_teste.xlsx")', qmd, fixed = TRUE)),
  any(grepl("dados_analise <- dados", qmd, fixed = TRUE)),
  any(grepl("catalyser_conferir_base(", qmd, fixed = TRUE)),
  any(grepl('here("dados", "processados", "dados_analise.rds")', qmd, fixed = TRUE)),
  any(grepl("trat_moda <- catalyser_moda", qmd, fixed = TRUE)),
  # Sem operação estrutural promovida, a base resolvida é a própria planilha.
  any(grepl("base_resolvida <- dados_brutos", qmd, fixed = TRUE)),
  # Nada é lido de scripts: o preparo está inteiro no documento.
  !any(grepl("source(", qmd, fixed = TRUE))
)

# --- A pasta dados/ é enxuta: brutos/ com a planilha, processados/ com dois ----
stopifnot(
  identical(list.files(file.path(projeto, "dados", "brutos")), "captura_teste.xlsx"),
  identical(
    sort(list.files(file.path(projeto, "dados", "processados"))),
    sort(c("dados_analise.rds", "base_compartilhada.xlsx"))
  ),
  # Sem operação estrutural promovida, a fotografia pós-estrutural não é gerada.
  !file.exists(file.path(projeto, "dados", "processados", "base_resolvida.rds")),
  # Nem cópia dos dados brutos, nem csv redundante, nem fotografia de derivada.
  !file.exists(file.path(projeto, "dados", "processados", "dados_brutos.rds")),
  !file.exists(file.path(projeto, "dados", "processados", "dados_analise.csv")),
  !file.exists(file.path(projeto, "dados", "processados", "base_regressao.rds")),
  # A compartilhada em planilha é entrega, não fonte: o QMD não a lê.
  !any(grepl("base_compartilhada.xlsx", qmd, fixed = TRUE))
)

# --- O QMD constrói cada base derivada no chunk da própria análise -------------
stopifnot(
  any(grepl("library(here)", qmd, fixed = TRUE)),
  any(grepl("library(readxl)", qmd, fixed = TRUE)),
  any(grepl("#| label: descritiva-captura-base", qmd, fixed = TRUE)),
  any(grepl("dados_da_analise <- dados_analise", qmd, fixed = TRUE)),
  # A apresentação escreve os parâmetros por extenso; nada de metadados no QMD.
  any(grepl("linhas_captura <- catalyser_executar(", qmd, fixed = TRUE)),
  any(grepl('tipo = "grafico_linhas"', qmd, fixed = TRUE)),
  any(grepl('x = "ano"', qmd, fixed = TRUE)),
  any(grepl('catalyser_mostrar(linhas_captura[["grafico"]])', qmd, fixed = TRUE)),
  !any(grepl("analises_registradas", qmd, fixed = TRUE)),
  !any(grepl("registro_execucoes.rds", qmd, fixed = TRUE)),
  # As funcoes vem do pacote instalado, nao mais de um arquivo copiado.
  any(grepl("library(catalyser)", qmd, fixed = TRUE)),
  # O replay opaco por sys.source saiu do relatório.
  !any(grepl("sys.source(", qmd, fixed = TRUE))
)

# O relatório roda inteiro fora do Quarto: knitr::purl() extrai os chunks na
# ordem, como um aluno que os executa um a um no RStudio. A saída é capturada
# para verificar que a Base Compartilhada reconstruída a partir da planilha
# bate com a fotografia exportada.
codigo_relatorio <- tempfile("relatorio_", fileext = ".R")
knitr::purl(caminho_qmd, output = codigo_relatorio, quiet = TRUE)
anterior <- getwd()
setwd(projeto)
saida_relatorio <- utils::capture.output(
  sys.source(codigo_relatorio, envir = new.env(parent = globalenv()))
)
setwd(anterior)
stopifnot(
  any(grepl("idêntica à fotografia", saida_relatorio, fixed = TRUE)),
  !any(grepl("divergiu da fotografia", saida_relatorio, fixed = TRUE))
)

zip_saida <- file.path(raiz, "projeto.zip")
do.call(exportacao_empacotar_projeto, c(list(file = zip_saida), argumentos))
stopifnot(file.exists(zip_saida), file.info(zip_saida)$size > 0)

if (nzchar(unname(Sys.which("quarto")))) {
  word_saida <- file.path(raiz, "relatorio.docx")
  do.call(exportacao_renderizar_word, c(list(file = word_saida), argumentos))
  stopifnot(file.exists(word_saida), file.info(word_saida)$size > 0)
  cat("[OK] Word do projeto renderizado pelo Quarto.\n")
} else {
  cat("[AVISO] Quarto CLI ausente: o render do Word NÃO foi verificado.\n")
}

cat("OK: Fase 3E exporta Word seletivo e preserva todas as execuções no Projeto R\n")
