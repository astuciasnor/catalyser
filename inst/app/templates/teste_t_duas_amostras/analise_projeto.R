# {{TITULO_COMENTARIO}} — ROTEIRO DE ANÁLISE
# x========================================================================x
# Pergunta: {{PERGUNTA_COMENTARIO}}
#
# COMO ESTUDAR
# Abra o arquivo .Rproj e execute as seções na ordem, de cima para baixo.
# No RStudio, Ctrl+Enter executa a linha ou a seleção. Digite o nome de um
# objeto no console para examiná-lo, por exemplo: tabela_teste.
# O sumário do editor (Ctrl+Shift+O) permite navegar entre as seções numeradas.
#
# MAPA DO ROTEIRO
#  1–3. Preparar o ambiente, receber a base e organizar os dois grupos.
#  4–6. Explorar, conferir os pressupostos e aplicar o teste t.
#  7–8. Preparar as tabelas e construir os gráficos.
#    9. Preparar os textos que serão usados nos relatórios.
# 10–11. Salvar cópias dos resultados e registrar as versões utilizadas.
#
# OBJETOS QUE OS RELATÓRIOS VÃO USAR
# dados                      base com a resposta e o grupo (fator de dois níveis)
# teste_t                    resultado do t.test escolhido conforme as variâncias
# d_cohen                    tamanho do efeito (referência estatística)
# tabela_descritiva_exibir   resumo por grupo, formatado
# tabela_teste               método, diferença, IC, t, gl, p e d, formatados
# grafico_caixa              boxplot com os pontos de cada observação
# texto_resultado            frase com o resultado do teste
#
# Cada QMD executa este script numa sessão nova e usa os objetos em memória.
# Os CSVs e PNGs salvos são cópias para consulta; não alimentam os QMDs.
# Edite os cálculos aqui e a argumentação científica nos documentos Quarto.
# Instale os pacotes uma única vez conforme o README, antes de executar.

# 1. Preparar o ambiente ---------------------------------------------------
# A importação, o preparo e a adoção da base escolhida aparecem acima deste
# roteiro. A partir daqui, trabalhamos com dados_da_analise.
# here::i_am declara onde este arquivo está, para os caminhos partirem da raiz.
here::i_am("R/analise.R")
library(car)        # teste de Levene (homocedasticidade)
library(stringr)    # str_glue monta as frases dos relatórios
# As funções abaixo cuidam da apresentação; os cálculos continuam neste script.
source(here::here("R", "funcoes.R"), encoding = "UTF-8")

# 2. Definir as escolhas e organizar as saídas -----------------------------
# Os nomes e rótulos abaixo vêm das escolhas feitas na CatalyseR.
variavel_resposta <- {{RESPOSTA_R}}
variavel_grupo <- {{GRUPO_R}}
# Os rótulos são textos de apresentação: alterá-los não renomeia as colunas.
rotulo_resposta <- {{ROTULO_RESPOSTA_R}}
rotulo_grupo <- {{ROTULO_GRUPO_R}}
nivel_confianca <- {{CONFIANCA}}
alfa <- 1 - nivel_confianca
titulo_grafico <- {{TITULO_R}}
# Duas cores da paleta Ocean, uma por ração/grupo, com bom contraste.
cores_grupo <- c("#0F3B5F", "#E89B3C")

# Estas pastas guardam produtos regeneráveis. Os dados brutos ficam intactos.
# recursive = TRUE cria as pastas intermediárias; showWarnings = FALSE evita
# o aviso de pasta já existente; dir.create() não apaga nada que esteja nelas.
for (pasta in c("dados/processados", "saida/tabelas", "saida/figuras", "saida/relatorios")) {
  dir.create(here::here(pasta), showWarnings = FALSE, recursive = TRUE)
}

# 3. Preparar a base -------------------------------------------------------
# Recebe a base preparada e adotada pela análise (dados_da_analise).
dados <- as.data.frame(dados_da_analise)
# factor() marca o grupo como categoria; numero_obs localiza cada linha depois.
dados[[variavel_grupo]] <- factor(dados[[variavel_grupo]])
dados$numero_obs <- seq_len(nrow(dados))

# Conferências mínimas: a resposta precisa ser numérica e o grupo ter 2 níveis.
if (!is.numeric(dados[[variavel_resposta]])) {
  stop("A resposta precisa ser numérica. Confira a tipagem no preparo.")
}
if (nlevels(dados[[variavel_grupo]]) != 2) {
  stop("O teste t compara exatamente dois grupos; o fator escolhido não tem dois níveis.")
}
if (nivel_confianca <= 0 || nivel_confianca >= 1) stop("Confira o nível de confiança.")
# Mantém apenas linhas com resposta e grupo preenchidos; conta as excluídas.
n_total <- nrow(dados)
completos <- !is.na(dados[[variavel_resposta]]) & !is.na(dados[[variavel_grupo]])
dados <- dados[completos, , drop = FALSE]
dados[[variavel_grupo]] <- droplevels(dados[[variavel_grupo]])
n_utilizado <- nrow(dados)
n_excluido <- n_total - n_utilizado
# split() separa a resposta por grupo, para conferir cada um isoladamente.
grupos <- split(dados[[variavel_resposta]], dados[[variavel_grupo]])
if (any(lengths(grupos) < 2)) stop("Cada grupo precisa de pelo menos duas observações.")
if (any(vapply(grupos, function(x) length(unique(x)) < 2, logical(1)))) {
  stop("A resposta precisa variar dentro de cada grupo.")
}
# Nomes dos dois níveis, na ordem do fator (o primeiro entra como referência).
niveis <- levels(dados[[variavel_grupo]])
nivel_1 <- niveis[1]; nivel_2 <- niveis[2]

# 4. Explorar: resumo por grupo --------------------------------------------
# Uma linha por grupo com n, média, desvio padrão, erro padrão e amplitude.
# O DP mede a dispersão entre observações; o EP mede a incerteza da média.
tabela_descritiva <- dados |>
  dplyr::group_by(.data[[variavel_grupo]]) |>
  dplyr::summarise(
    n = dplyr::n(),
    media = mean(.data[[variavel_resposta]]),
    dp = sd(.data[[variavel_resposta]]),
    ep = dp / sqrt(n),
    minimo = min(.data[[variavel_resposta]]),
    maximo = max(.data[[variavel_resposta]]),
    .groups = "drop"
  )
# Guarda o nome da coluna de grupo devolvida pelo summarise, para reusá-la.
nome_col_grupo <- names(tabela_descritiva)[1]
# Versão formatada para exibição, com vírgula decimal e rótulo do grupo.
tabela_descritiva_exibir <- tabela_descritiva |>
  dplyr::transmute(
    Grupo = as.character(.data[[nome_col_grupo]]),
    n = n,
    Média = fmt(media),
    DP = fmt(dp),
    EP = fmt(ep),
    Mínimo = fmt(minimo),
    Máximo = fmt(maximo)
  )
names(tabela_descritiva_exibir)[1] <- rotulo_grupo

# 5. Conferir os pressupostos ----------------------------------------------
# O teste t clássico pede normalidade DENTRO de cada grupo e variâncias
# parecidas ENTRE os grupos. Usamos Shapiro-Wilk por grupo e o teste de Levene.
# tapply aplica shapiro.test a cada grupo e guarda o p-valor de cada um.
p_shapiro <- tapply(dados[[variavel_resposta]], dados[[variavel_grupo]],
                    function(x) shapiro.test(x)$p.value)
p_shapiro_1 <- p_shapiro[[nivel_1]]; p_shapiro_2 <- p_shapiro[[nivel_2]]
# Levene compara as variâncias entre os grupos (mais robusto que o teste F).
formula_teste <- reformulate(variavel_grupo, response = variavel_resposta)
teste_levene <- car::leveneTest(formula_teste, data = dados)
p_levene <- teste_levene[["Pr(>F)"]][1]
# Decisão honesta: só tratamos as variâncias como iguais quando Levene NÃO dá
# evidência de diferença (p maior ou igual a alfa).
variancias_iguais <- !is.na(p_levene) && p_levene >= alfa
# A normalidade fica "ok" quando nenhum dos dois grupos dá evidência de desvio.
normalidade_ok <- all(p_shapiro >= alfa)

# 6. Aplicar o teste t -----------------------------------------------------
# A escolha do método segue Levene: variâncias iguais levam ao t de Student;
# variâncias diferentes levam ao t de Welch. Guardamos os dois para comparar.
teste_t <- t.test(formula_teste, data = dados, var.equal = variancias_iguais)
teste_welch <- t.test(formula_teste, data = dados, var.equal = FALSE)
# resumo_console guarda a saída bruta do teste, exibida uma vez no relatório.
resumo_console <- teste_t
# Nome do método em português, para as tabelas e o texto.
metodo_teste <- if (variancias_iguais) "t de Student (variâncias iguais)" else "t de Welch (variâncias diferentes)"

# Médias de cada grupo e a diferença entre elas (nível 1 menos nível 2).
media_1 <- tabela_descritiva$media[tabela_descritiva[[nome_col_grupo]] == nivel_1]
media_2 <- tabela_descritiva$media[tabela_descritiva[[nome_col_grupo]] == nivel_2]
diferenca_medias <- media_1 - media_2
ic_diferenca <- teste_t$conf.int

# Tamanho do efeito (d de Cohen) calculado à mão, para ficar transparente.
# sp é o desvio padrão combinado: pondera a variância de cada grupo pelos
# seus graus de liberdade (n - 1). O d mede a diferença em desvios padrão.
n_1 <- tabela_descritiva$n[tabela_descritiva[[nome_col_grupo]] == nivel_1]
n_2 <- tabela_descritiva$n[tabela_descritiva[[nome_col_grupo]] == nivel_2]
dp_1 <- tabela_descritiva$dp[tabela_descritiva[[nome_col_grupo]] == nivel_1]
dp_2 <- tabela_descritiva$dp[tabela_descritiva[[nome_col_grupo]] == nivel_2]
sp <- sqrt(((n_1 - 1) * dp_1^2 + (n_2 - 1) * dp_2^2) / (n_1 + n_2 - 2))
d_cohen <- diferenca_medias / sp
# case_when lê como uma escada de decisões, avaliada de cima para baixo.
classe_efeito <- dplyr::case_when(
  is.na(d_cohen)     ~ "não calculado",
  abs(d_cohen) < 0.2 ~ "insignificante",
  abs(d_cohen) < 0.5 ~ "pequeno",
  abs(d_cohen) < 0.8 ~ "médio",
  TRUE               ~ "grande"
)

# 7. Preparar as tabelas de apresentação -----------------------------------
ic_percentual <- fmt(100 * nivel_confianca, 0)
# Tabela enxuta com o essencial do teste.
tabela_teste <- data.frame(
  Indicador = c(
    "Método",
    paste0("Diferença de médias (", nivel_1, " menos ", nivel_2, ")"),
    paste0("IC ", ic_percentual, "% da diferença"),
    "t",
    "Graus de liberdade",
    "p-valor",
    "d de Cohen (tamanho do efeito)"
  ),
  Valor = c(
    metodo_teste,
    fmt(diferenca_medias),
    paste0("[", fmt(ic_diferenca[1]), "; ", fmt(ic_diferenca[2]), "]"),
    fmt(unname(teste_t$statistic)),
    fmt(unname(teste_t$parameter)),
    formatar_p(teste_t$p.value),
    paste0(fmt(d_cohen), " (", classe_efeito, ")")
  ),
  check.names = FALSE
)
# Tabela dos pressupostos, com leitura honesta linha a linha.
leitura_shapiro_1 <- if (p_shapiro_1 >= alfa) "Sem evidência de desvio da normalidade." else "Evidência de desvio da normalidade."
leitura_shapiro_2 <- if (p_shapiro_2 >= alfa) "Sem evidência de desvio da normalidade." else "Evidência de desvio da normalidade."
leitura_levene <- if (variancias_iguais) "Sem evidência de variâncias diferentes." else "Há evidência de variâncias diferentes."
tabela_pressupostos <- data.frame(
  Teste = c(paste0("Shapiro-Wilk (", nivel_1, ")"), paste0("Shapiro-Wilk (", nivel_2, ")"), "Levene (variâncias)"),
  `p-valor` = formatar_p(c(p_shapiro_1, p_shapiro_2, p_levene)),
  Leitura = c(leitura_shapiro_1, leitura_shapiro_2, leitura_levene),
  check.names = FALSE
)

# 8. Construir os gráficos -------------------------------------------------
# 8.1 Boxplot com os pontos de cada observação: mostra dispersão e sobreposição.
# outlier.shape = NA evita desenhar o ponto extremo duas vezes; ele vem do jitter.
grafico_caixa <- ggplot2::ggplot(dados,
  ggplot2::aes(x = .data[[variavel_grupo]], y = .data[[variavel_resposta]],
               fill = .data[[variavel_grupo]])) +
  ggplot2::geom_boxplot(width = 0.5, alpha = 0.65, outlier.shape = NA) +
  ggplot2::geom_jitter(width = 0.12, size = 2, colour = "grey15", alpha = 0.8) +
  ggplot2::scale_fill_manual(values = cores_grupo) +
  ggplot2::labs(
    x = rotulo_grupo, y = rotulo_resposta,
    title = if (nzchar(titulo_grafico)) titulo_grafico else NULL
  ) +
  tema_projeto() +
  ggplot2::theme(legend.position = "none")

# 8.2 Médias por grupo em barras, com barras de erro (intervalo de confiança).
# qt() dá o t crítico de cada grupo a partir dos seus graus de liberdade (n - 1).
resumo_ic <- tabela_descritiva |>
  dplyr::mutate(
    t_critico = qt(1 - alfa / 2, df = n - 1),
    ic_baixo = media - t_critico * ep,
    ic_alto = media + t_critico * ep
  )
# geom_col desenha a barra da média (base em zero); geom_errorbar acrescenta o IC.
# scale_y_continuous com expansão (0, 0.12) faz o eixo y começar exatamente em zero.
grafico_medias <- ggplot2::ggplot(resumo_ic,
  ggplot2::aes(x = .data[[nome_col_grupo]], y = media, fill = .data[[nome_col_grupo]])) +
  ggplot2::geom_col(width = 0.6) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = ic_baixo, ymax = ic_alto), width = 0.15, linewidth = 0.6) +
  ggplot2::scale_fill_manual(values = cores_grupo) +
  ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.12))) +
  ggplot2::labs(
    x = rotulo_grupo,
    y = paste0("Média de ", rotulo_resposta),
    subtitle = paste0("Barras: média; hastes: intervalo de confiança de ", ic_percentual, "% da média")
  ) +
  tema_projeto() +
  ggplot2::theme(legend.position = "none")

# 9. Preparar os textos dos relatórios -------------------------------------
# Qual grupo teve a maior média? Comparação direta entre os dois valores.
grupo_maior <- if (media_1 > media_2) nivel_1 else nivel_2
# A evidência compara o p do teste com alfa, sem exagerar a conclusão.
evidencia <- if (teste_t$p.value < alfa) {
  "houve diferença significativa entre as médias dos dois grupos"
} else "não houve diferença significativa entre as médias dos dois grupos"
# Frases de pressupostos escritas conforme o resultado real de cada teste.
frase_normalidade <- if (normalidade_ok) {
  "não houve evidência contra a normalidade dentro dos grupos"
} else "houve evidência de afastamento da normalidade em pelo menos um grupo"
frase_variancia <- if (variancias_iguais) {
  "não houve evidência de variâncias diferentes"
} else "houve evidência de variâncias diferentes"

texto_amostra <- stringr::str_glue(
  "Foram analisadas {n_utilizado} das {n_total} observações disponíveis ",
  "({n_excluido} excluídas por ausência de resposta ou grupo), ",
  "sendo {n_1} no grupo {nivel_1} e {n_2} no grupo {nivel_2}."
)
print(texto_amostra)

texto_pressupostos <- stringr::str_glue(
  "Quanto aos pressupostos, {frase_normalidade} ",
  "(Shapiro-Wilk: {formatar_p(p_shapiro_1, no_texto = TRUE)} para {nivel_1} e ",
  "{formatar_p(p_shapiro_2, no_texto = TRUE)} para {nivel_2}) e {frase_variancia} ",
  "(Levene: {formatar_p(p_levene, no_texto = TRUE)}). ",
  "Um p acima de {fmt(alfa, 2)} não prova o pressuposto; apenas não dá ",
  "evidência para rejeitá-lo."
)
print(texto_pressupostos)

texto_resultado <- stringr::str_glue(
  "Pelo {metodo_teste}, {evidencia} ",
  "(t = {fmt(unname(teste_t$statistic))}; gl = {fmt(unname(teste_t$parameter))}; ",
  "{formatar_p(teste_t$p.value, no_texto = TRUE)}). ",
  "O grupo {nivel_1} teve média {fmt(media_1)} e o grupo {nivel_2}, {fmt(media_2)}; ",
  "a diferença foi de {fmt(diferenca_medias)} ",
  "(IC {ic_percentual}% [{fmt(ic_diferenca[1])}; {fmt(ic_diferenca[2])}])."
)
print(texto_resultado)

texto_efeito <- stringr::str_glue(
  "O tamanho do efeito foi {classe_efeito} (d de Cohen = {fmt(d_cohen)}). ",
  "A significância diz que a diferença existe; o d diz o quanto ela importa. ",
  "O rótulo é uma referência estatística, não uma leitura biológica direta."
)
print(texto_efeito)

# O caderno recebe também a comparação honesta com o t de Welch.
texto_welch <- stringr::str_glue(
  "Como referência, o t de Welch (que não assume variâncias iguais) dá ",
  "t = {fmt(unname(teste_welch$statistic))}; gl = {fmt(unname(teste_welch$parameter))}; ",
  "{formatar_p(teste_welch$p.value, no_texto = TRUE)}. Quando a igualdade de ",
  "variâncias é duvidosa, o Welch é a escolha segura."
)
print(texto_welch)

texto_sintese_estatistica <- stringr::str_glue(
  "Na amostra de {n_utilizado} observações, {evidencia}. A maior média foi do ",
  "grupo {grupo_maior}, com diferença de {fmt(diferenca_medias)} ",
  "(IC {ic_percentual}% [{fmt(ic_diferenca[1])}; {fmt(ic_diferenca[2])}]; ",
  "{formatar_p(teste_t$p.value, no_texto = TRUE)}) e tamanho de efeito {classe_efeito} ",
  "(d = {fmt(d_cohen)}). A interpretação depende dos pressupostos e do delineamento."
)
print(texto_sintese_estatistica)

# alerta_modelo acompanha a conclusão, honesto quanto aos pressupostos.
alerta_modelo <- if (!normalidade_ok || !variancias_iguais) {
  "Os testes indicaram sinais de atenção nos pressupostos; considere o t de Welch e examine os gráficos antes de concluir."
} else "Os testes não detectaram desvios nos pressupostos, mas os gráficos e o delineamento continuam necessários."
print(alerta_modelo)

# 10. Salvar cópias para consulta e compartilhamento ------------------------
# CSV com ponto e vírgula e vírgula decimal abre bem no Excel em português.
write.csv2(dados, here::here("dados", "processados", "base_teste_t.csv"),
           row.names = FALSE, fileEncoding = "UTF-8")
tabelas <- list(descritiva = tabela_descritiva, teste = tabela_teste, pressupostos = tabela_pressupostos)
for (nome in names(tabelas)) {
  write.csv2(tabelas[[nome]], here::here("saida", "tabelas", paste0(nome, ".csv")),
             row.names = FALSE, fileEncoding = "UTF-8")
}
figuras <- list(caixa = grafico_caixa, medias = grafico_medias)
for (nome in names(figuras)) {
  ggplot2::ggsave(here::here("saida", "figuras", paste0(nome, ".png")),
    plot = figuras[[nome]], width = 7, height = 4.6, dpi = 300, bg = "white")
}

# 11. Registrar o ambiente computacional -----------------------------------
versao_quarto <- if (nzchar(Sys.which("quarto"))) {
  system2("quarto", "--version", stdout = TRUE)
} else "Quarto não encontrado no PATH desta sessão."
registro_ambiente <- c(paste("Quarto:", versao_quarto), capture.output(sessionInfo()))
writeLines(registro_ambiente, here::here("saida", "sessionInfo.txt"), useBytes = TRUE)
