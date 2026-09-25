# {{TITULO_COMENTARIO}} — ROTEIRO DE ANÁLISE
# x========================================================================x
# Pergunta: {{PERGUNTA_COMENTARIO}}
#
# COMO ESTUDAR
# Abra o arquivo .Rproj e execute as seções na ordem, de cima para baixo.
# No RStudio, Ctrl+Enter executa a linha ou a seleção. Digite o nome de um
# objeto no console para examiná-lo, por exemplo: tabela_anova.
# O sumário do editor (Ctrl+Shift+O) permite navegar entre as seções numeradas.
#
# MAPA DO ROTEIRO
#  1–3. Preparar o ambiente, definir as escolhas e montar a base da ANOVA.
#  4–6. Explorar os grupos, ajustar o modelo e examinar os pressupostos.
#    7. Comparar os grupos dois a dois (Tukey) e medir o tamanho do efeito.
#  8–9. Preparar as tabelas e construir os gráficos.
#   10. Preparar os textos que serão usados nos relatórios.
# 11–12. Salvar cópias dos resultados e registrar as versões utilizadas.
#
# OBJETOS QUE OS RELATÓRIOS VÃO USAR
# base_anova               dados e identificadores das observações analisadas
# modelo_anova             modelo ajustado por aov()
# tabela_anova             a tabela da ANOVA, sem arredondamento
# tabela_resumo            n, média, DP, EP, IC e letras de Tukey por grupo
# tabela_resumo_exibir     versão formatada da tabela-resumo
# grafico_barras           figura principal, pronta para exibir ou salvar
# texto_anova              frase com o resultado calculado
#
# Cada QMD executa este script numa sessão nova e usa os objetos em memória.
# Os CSVs e PNGs salvos são cópias para consulta; não alimentam os QMDs.
# Edite os cálculos aqui e a argumentação científica nos documentos Quarto.
# Instale os pacotes uma única vez conforme o README, antes de executar.

# 1. Preparar o ambiente ---------------------------------------------------
# A importação, o preparo e a adoção da base escolhida aparecem acima deste
# roteiro. A partir daqui, trabalhamos com dados_da_analise.
# Declara: "este arquivo está em R/analise.R, dentro do meu projeto".
# Assim, here() monta caminhos a partir da raiz do projeto, acima da pasta R/.
here::i_am("R/analise.R")
library(broom)
library(flextable)
library(stringr)
# As funções abaixo cuidam da apresentação; os cálculos continuam neste script.
source(here::here("R", "funcoes.R"), encoding = "UTF-8")

# 2. Definir as escolhas e organizar as saídas -----------------------------
# Os nomes e rótulos abaixo vêm das escolhas feitas na CatalyseR.
variavel_resposta <- {{RESPOSTA_R}}
variavel_fator <- {{FATOR_R}}
# Os rótulos são textos de apresentação: alterá-los não renomeia as colunas.
rotulo_resposta <- {{ROTULO_RESPOSTA_R}}
rotulo_fator <- {{ROTULO_FATOR_R}}
nivel_confianca <- {{CONFIANCA}}
alfa <- 1 - nivel_confianca
ic_percentual <- fmt(100 * nivel_confianca, 0)
titulo_grafico <- {{TITULO_R}}
# Paleta Ocean, a mesma da CatalyseR e do livro. Uma cor para cada grupo.
cores_tratamento <- c("#0F3B5F", "#2E7D8F", "#62B6B7", "#E89B3C", "#E76F51",
                      "#8FB8C8", "#1F5673", "#B5654A")

# Estas pastas guardam produtos regeneráveis. Os dados brutos ficam intactos.
# O laço cria cada pasta dentro do projeto, caso ela ainda não exista.
# recursive = TRUE cria também as pastas intermediárias, como saida/.
# showWarnings = FALSE silencia o aviso de pasta já existente;
# dir.create() não apaga arquivos que estejam nessas pastas.
for (pasta in c(
  "dados/processados",
  "saida/tabelas",
  "saida/figuras",
  "saida/relatorios"
)) {
  dir.create(
    here::here(pasta),
    showWarnings = FALSE,
    recursive = TRUE
  )
}

# 3. Preparar a base da ANOVA ----------------------------------------------
# Mantemos identificação e medidas JUNTAS. O modelo usará a resposta e o fator.
# |> encaminha uma tabela para a próxima operação; mutate() cria/altera colunas.
# linha_original conserva a posição na base preparada adotada pela análise.
dados_preparados <- as.data.frame(dados_da_analise) |>
  mutate(linha_original = row_number(), .before = 1)

# if (...) verifica uma condição; quando ela é TRUE, executa o bloco.
# Aqui, stop() interrompe a análise e mostra a mensagem do problema encontrado.
if (!all(c(variavel_resposta, variavel_fator) %in% names(dados_preparados))) {
  stop("Confira os nomes da resposta e do fator na base preparada.")
}
if (!is.numeric(dados_preparados[[variavel_resposta]])) {
  stop("A resposta precisa ser numérica. Confira a tipagem no preparo.")
}
if (nivel_confianca <= 0 || nivel_confianca >= 1) stop("Confira o nível de confiança.")

# A ANOVA utiliza só a resposta e o fator; faltantes em outras colunas
# não excluem linhas. TRUE marca uma linha com as duas informações.
linhas_completas <- !is.na(dados_preparados[[variavel_resposta]]) &
  !is.na(dados_preparados[[variavel_fator]])

# Copiamos as duas variáveis para colunas de nomes curtos, grupo e resposta.
# Nomes com espaço ou acento atrapalham TukeyHSD() e as letras dos grupos;
# assim o código funciona com qualquer cabeçalho vindo do Excel.
# droplevels() retira grupos que ficaram sem nenhuma observação.
base_anova <- dados_preparados[linhas_completas, ] |>
  mutate(
    grupo = factor(.data[[variavel_fator]]),
    resposta = .data[[variavel_resposta]]
  ) |>
  droplevels()

n_total <- nrow(dados_preparados)
n_utilizado <- nrow(base_anova)
# ! inverte TRUE/FALSE; na soma, TRUE vale 1. Contamos as linhas que saíram.
n_excluido <- sum(!linhas_completas)
n_grupos <- nlevels(base_anova$grupo)
if (n_grupos < 2) stop("A ANOVA precisa de pelo menos dois grupos com dados.")
# As letras de Tukey usam o hífen para separar os pares; ele não pode
# aparecer no nome de um grupo. Renomeie esses grupos no preparo.
if (any(grepl("-", levels(base_anova$grupo), fixed = TRUE))) {
  stop("Há hífen no nome de um grupo. Renomeie os grupos no preparo.")
}
# Se houver mais grupos que cores, criamos cores intermediárias.
cores_grupos <- if (n_grupos <= length(cores_tratamento)) {
  cores_tratamento[seq_len(n_grupos)]
} else {
  grDevices::colorRampPalette(cores_tratamento)(n_grupos)
}

# 4. Explorar os grupos -----------------------------------------------------
# Compare tamanho, centro e dispersão da resposta entre os grupos.
# group_by() separa a tabela em grupos; summarise() calcula uma linha por grupo.
# O IC da média usa o t crítico com n - 1 graus de liberdade, como na CatalyseR.
tabela_resumo <- base_anova |>
  group_by(grupo) |>
  summarise(
    n = n(),
    media = mean(resposta),
    dp = sd(resposta),
    ep = dp / sqrt(n),
    t_critico = qt(1 - alfa / 2, df = n - 1),
    ic_inf = media - t_critico * ep,
    ic_sup = media + t_critico * ep,
    .groups = "drop"
  ) |>
  select(-t_critico)
# Execute tabela_resumo no console. Confira o n de cada grupo e se as
# dispersões (dp) são parecidas: grupos muito diferentes pedem atenção.

# 5. Ajustar a ANOVA e extrair os resultados --------------------------------
# aov() ajusta o modelo resposta ~ grupo, como na CatalyseR. A ANOVA responde à
# pergunta global: a média difere entre os grupos? Ainda não diz quais diferem.
modelo_anova <- aov(resposta ~ grupo, data = base_anova)
resumo_console <- summary(modelo_anova)
# Ao estudar o script, execute resumo_console para conhecer a saída bruta uma vez.
# tidy() transforma a tabela da ANOVA em um data.frame com nomes claros.
tabela_anova <- broom::tidy(modelo_anova)
gl_fator <- tabela_anova$df[1]
gl_residuo <- tabela_anova$df[2]
f_anova <- tabela_anova$statistic[1]
p_anova <- tabela_anova$p.value[1]

# 6. Examinar os pressupostos -----------------------------------------------
# Os pressupostos dizem respeito aos erros; os resíduos ajudam a examiná-los.
# p > alfa indica ausência de evidência contra o pressuposto; não o prova.
dados_diagnostico <- data.frame(
  linha_original = base_anova$linha_original,
  grupo = base_anova$grupo,
  ajustado = fitted(modelo_anova),
  residuo = residuals(modelo_anova),
  residuo_padronizado = rstandard(modelo_anova)
)
# shapiro.test() aceita de 3 a 5000 resíduos com alguma variação. Fora dessas
# condições, registramos NA e a leitura fica com o gráfico Q-Q.
teste_shapiro <- NULL
if (n_utilizado >= 3 && n_utilizado <= 5000 && sd(dados_diagnostico$residuo) > 0) {
  teste_shapiro <- shapiro.test(dados_diagnostico$residuo)
}
w_shapiro <- if (is.null(teste_shapiro)) NA_real_ else unname(teste_shapiro$statistic)
p_shapiro <- if (is.null(teste_shapiro)) NA_real_ else teste_shapiro$p.value
# Levene compara as variâncias entre os grupos (pacote car).
teste_levene <- car::leveneTest(resposta ~ grupo, data = base_anova)
f_levene <- teste_levene$`F value`[1]
p_levene <- teste_levene$`Pr(>F)`[1]

# 7. Comparar os grupos e medir o tamanho do efeito --------------------------
# Tukey compara todos os pares, com p-valores ajustados para comparações múltiplas.
tukey <- TukeyHSD(modelo_anova, conf.level = nivel_confianca)
tabela_tukey <- as.data.frame(tukey$grupo)
tabela_tukey$Comparação <- rownames(tabela_tukey)
# multcompLetters4() resume o Tukey em letras: grupos que compartilham uma
# letra não diferiram ao nível escolhido. $grupo$Letters extrai as letras.
letras <- multcompView::multcompLetters4(modelo_anova, tukey)$grupo$Letters
# Juntamos a letra pelo NOME do grupo, nunca pela posição da linha.
tabela_resumo <- tabela_resumo |>
  mutate(letra = unname(letras[as.character(grupo)]))
# Eta² é a fração da variação da resposta associada ao fator;
# ômega² corrige o viés do eta² em amostras pequenas.
eta2 <- effectsize::eta_squared(modelo_anova, partial = FALSE)$Eta2[1]
omega2 <- effectsize::omega_squared(modelo_anova, partial = FALSE)$Omega2[1]
# case_when() escolhe, de cima para baixo, a primeira condição verdadeira.
# A convenção de Cohen é uma referência estatística, não biológica.
classe_efeito <- case_when(
  is.na(eta2) ~ "indeterminado",
  eta2 < 0.01 ~ "muito pequeno",
  eta2 < 0.06 ~ "pequeno",
  eta2 < 0.14 ~ "médio",
  TRUE ~ "grande"
)

# 8. Preparar as tabelas de apresentação ------------------------------------
# fmt() e formatar_p() mudam só a exibição; os objetos numéricos ficam intactos.
tabela_resumo_exibir <- tabela_resumo |>
  transmute(
    Grupo = grupo,
    n,
    Média = fmt(media),
    DP = fmt(dp),
    EP = fmt(ep),
    IC = str_glue("{fmt(ic_inf)} a {fmt(ic_sup)}"),
    Tukey = letra
  )
names(tabela_resumo_exibir)[names(tabela_resumo_exibir) == "Grupo"] <- rotulo_fator
names(tabela_resumo_exibir)[names(tabela_resumo_exibir) == "IC"] <- paste0("IC ", ic_percentual, "%")

# GL = graus de liberdade; SQ = soma de quadrados; QM = quadrado médio.
# A linha do resíduo não tem F nem p: as células ficam vazias.
tabela_anova_exibir <- tabela_anova |>
  transmute(
    Fonte = c(rotulo_fator, "Resíduo"),
    GL = df,
    SQ = fmt(sumsq),
    QM = fmt(meansq),
    F = ifelse(is.na(statistic), "", fmt(statistic)),
    p = ifelse(is.na(p.value), "", formatar_p(p.value))
  )

tabela_tukey_exibir <- tabela_tukey |>
  transmute(
    Comparação,
    Diferença = fmt(diff),
    IC = str_glue("{fmt(lwr)} a {fmt(upr)}"),
    `p ajustado` = formatar_p(`p adj`)
  )
names(tabela_tukey_exibir)[names(tabela_tukey_exibir) == "IC"] <- paste0("IC ", ic_percentual, "%")

tabela_testes <- data.frame(
  Pressuposto = c("Normalidade dos resíduos", "Homogeneidade das variâncias"),
  Teste = c("Shapiro-Wilk", "Levene"),
  Estatística = c(
    paste0("W = ", fmt(w_shapiro, 3)),
    paste0("F(", teste_levene$Df[1], ", ", teste_levene$Df[2], ") = ", fmt(f_levene))
  ),
  p = formatar_p(c(p_shapiro, p_levene)),
  check.names = FALSE
)

tabela_efeito <- data.frame(
  Medida = c("η²", "ω²"),
  Valor = fmt(c(eta2, omega2), 3),
  Leitura = c(classe_efeito, "correção do η² para amostras pequenas")
)

# 9. Construir os gráficos --------------------------------------------------
# Cada gráfico recebe um nome: o QMD mostra o objeto e ggsave() salva uma cópia.

# 9.1. Exploração: caixas com as observações por cima.
# O QUE CONFERIR: caixas de alturas parecidas e pontos muito afastados.
grafico_boxplot <- ggplot(
  base_anova,
  aes(x = grupo, y = resposta)
) +
  geom_boxplot(width = 0.5, outlier.shape = NA, colour = "grey50") +
  geom_jitter(width = 0.1, height = 0, size = 1.5, alpha = 0.5) +
  labs(x = rotulo_fator, y = rotulo_resposta) +
  tema_projeto()

# 9.2. Figura principal: média, IC, média ± DP e letras de Tukey.
grafico_barras <- ggplot(
  tabela_resumo,
  aes(x = grupo, y = media, fill = grupo)
) +
  geom_col(width = 0.7) +
  geom_errorbar(aes(ymin = ic_inf, ymax = ic_sup), width = 0.2, linewidth = 0.6) +
  geom_text(
    aes(y = ic_sup, label = letra),
    vjust = -0.6,
    size = 4
  ) +
  geom_text(
    aes(y = media, label = paste0(fmt(media, 1), " ± ", fmt(dp, 1))),
    nudge_x = 0.08,
    hjust = 0,
    vjust = -0.4,
    size = 3.5
  ) +
  scale_x_discrete(expand = expansion(add = c(0.6, 0.9))) +
  scale_fill_manual(values = cores_grupos, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    x = rotulo_fator,
    y = rotulo_resposta,
    title = if (nzchar(titulo_grafico)) titulo_grafico else NULL
  ) +
  tema_projeto()

# 9.3. Resíduos versus ajustados. Na ANOVA aparecem faixas verticais, uma por
# grupo. Faixas de alturas muito diferentes sugerem variâncias desiguais.
grafico_residuos <- ggplot(
  dados_diagnostico,
  aes(x = ajustado, y = residuo, colour = grupo)
) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  geom_jitter(width = 0.02, height = 0, alpha = 0.7) +
  scale_colour_manual(values = cores_grupos) +
  labs(x = "Valores ajustados (médias dos grupos)", y = "Resíduos", colour = rotulo_fator) +
  tema_projeto()

# 9.4. Q-Q: os pontos devem acompanhar a reta; caudas que se descolam
# indicam assimetria ou valores extremos.
grafico_qq <- ggplot(
  dados_diagnostico,
  aes(sample = residuo_padronizado)
) +
  stat_qq(colour = "#2E7D8F", alpha = 0.7) +
  stat_qq_line(colour = "#E76F51") +
  labs(x = "Quantis teóricos", y = "Resíduos padronizados") +
  tema_projeto()

# 10. Preparar os textos que serão usados nos relatórios --------------------
# Os textos retomam os resultados depois de tabelas e gráficos.
# A atribuição com <- guarda a frase no objeto; print() mostra seu conteúdo.
# Execute a criação e a linha print() para calcular e conferir cada texto.

# 10.1 Evidência estatística da ANOVA
evidencia <- case_when(
  is.na(p_anova) ~ "a ANOVA não forneceu um p-valor válido",
  p_anova < alfa ~ "houve evidência de diferença entre as médias dos grupos",
  TRUE ~ "não houve evidência de diferença entre as médias dos grupos"
)
print(evidencia)

# 10.2 Leitura dos pressupostos. NA significa teste não calculado, não atendido.
leitura_shapiro <- case_when(
  is.na(p_shapiro) ~ "O teste de normalidade não foi calculado; use o gráfico Q-Q.",
  p_shapiro >= alfa ~ "Não houve evidência para rejeitar a normalidade dos resíduos.",
  TRUE ~ "Houve evidência de afastamento da normalidade dos resíduos."
)
leitura_levene <- case_when(
  is.na(p_levene) ~ "O teste de Levene não forneceu um p-valor válido.",
  p_levene >= alfa ~ "Não houve evidência para rejeitar a igualdade das variâncias.",
  TRUE ~ "Houve evidência de variâncias diferentes entre os grupos."
)

# 10.3 Textos que serão usados nos relatórios
texto_amostra <- str_glue(
  "Após o preparo, havia {n_total} observações. A análise utilizou ",
  "{n_utilizado} casos completos em {n_grupos} grupos; {n_excluido} ",
  "observações foram excluídas por ausência de resposta ou grupo."
)
print(texto_amostra)

texto_anova <- str_glue(
  "Em {rotulo_resposta}, {evidencia} de {rotulo_fator} ",
  "(F({gl_fator}, {gl_residuo}) = {fmt(f_anova)}; ",
  "{formatar_p(p_anova, no_texto = TRUE)})."
)
print(texto_anova)

texto_efeito <- str_glue(
  "O tamanho de efeito foi {classe_efeito} pela convenção de Cohen ",
  "(η² = {fmt(eta2, 3)}; ω² = {fmt(omega2, 3)}), uma referência estatística, ",
  "não biológica."
)
print(texto_efeito)

texto_tukey <- if (!is.na(p_anova) && p_anova < alfa) {
  "Pelo teste de Tukey, grupos que compartilham uma letra não diferiram entre si ao nível adotado."
} else {
  "Como a ANOVA não indicou diferença global, as comparações de Tukey servem apenas para descrição."
}
print(texto_tukey)

# O artigo recebe frases curtas; o caderno recebe também a orientação de leitura.
texto_pressupostos_artigo <- str_glue(
  "{leitura_shapiro} Shapiro-Wilk: W = {fmt(w_shapiro, 3)}; ",
  "{formatar_p(p_shapiro, no_texto = TRUE)}. {leitura_levene} ",
  "Levene: F({teste_levene$Df[1]}, {teste_levene$Df[2]}) = {fmt(f_levene)}; ",
  "{formatar_p(p_levene, no_texto = TRUE)}."
)
print(texto_pressupostos_artigo)

texto_pressupostos <- paste(
  texto_pressupostos_artigo,
  "Esses resultados não comprovam os pressupostos; a avaliação deve incluir",
  "os gráficos e o delineamento. A independência das observações depende",
  "de como os dados foram obtidos."
)
print(texto_pressupostos)

alerta_modelo <- if (!is.na(p_levene) && p_levene < alfa) {
  "As variâncias diferiram entre os grupos. Considere a ANOVA de Welch com comparações de Games-Howell antes de concluir."
} else if (!is.na(p_shapiro) && p_shapiro < alfa) {
  "Os resíduos se afastaram da normalidade. Avalie a intensidade do desvio nos gráficos e, se necessário, uma alternativa como Kruskal-Wallis."
} else "Os testes formais não detectaram os desvios examinados, mas os gráficos e o delineamento continuam necessários."
print(alerta_modelo)

# Síntese estatística: os argumentos científicos serão escritos no QMD.
texto_sintese_estatistica <- str_glue(
  "Na amostra de {n_utilizado} observações em {n_grupos} grupos, {evidencia} ",
  "(F({gl_fator}, {gl_residuo}) = {fmt(f_anova)}; ",
  "{formatar_p(p_anova, no_texto = TRUE)}; η² = {fmt(eta2, 3)}). ",
  "A interpretação deve considerar os pressupostos e o delineamento."
)
print(texto_sintese_estatistica)

# 11. Salvar cópias para consulta e compartilhamento ------------------------
# CSV com ponto e vírgula e vírgula decimal abre bem no Excel em português.
# Estes arquivos são saídas: edite a análise no script, não o CSV gerado.

# Uma única base processada, com identificadores, resposta e grupo analisados.
# Os QMDs não precisam dela: eles executam este script e usam base_anova.
write.csv2(
  base_anova,
  here::here("dados", "processados", "base_anova.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# A lista associa o nome do arquivo ao objeto já calculado.
# O laço repete somente a gravação, sem repetir nenhuma análise.
tabelas <- list(
  resumo_grupos = tabela_resumo,
  anova = tabela_anova,
  tukey = tabela_tukey,
  testes_pressupostos = tabela_testes,
  tamanho_efeito = tabela_efeito,
  diagnosticos_observacoes = dados_diagnostico
)
for (nome in names(tabelas)) {
  caminho_csv <- here::here("saida", "tabelas", paste0(nome, ".csv"))
  write.csv2(
    tabelas[[nome]],
    caminho_csv,
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
}

# ggsave() salva os mesmos gráficos que os QMDs mostram a partir da memória.
figuras <- list(
  barras = grafico_barras,
  boxplot = grafico_boxplot,
  residuos = grafico_residuos,
  qq = grafico_qq
)
for (nome in names(figuras)) {
  caminho_png <- here::here("saida", "figuras", paste0(nome, ".png"))
  ggsave(
    caminho_png,
    plot = figuras[[nome]],
    width = 7,
    height = 4.6,
    dpi = 300,
    bg = "white"
  )
}

# 12. Registrar o ambiente computacional -----------------------------------
# sessionInfo() informa o R e os pacotes; a versão do Quarto é consultada à parte.
# Este registro documenta o ambiente. Não instala nem fixa versões por si só.
versao_quarto <- if (nzchar(Sys.which("quarto"))) {
  system2("quarto", "--version", stdout = TRUE)
} else "Quarto não encontrado no PATH desta sessão."
registro_ambiente <- c(
  paste("Quarto:", versao_quarto),
  capture.output(sessionInfo())
)
writeLines(
  registro_ambiente,
  here::here("saida", "sessionInfo.txt"),
  useBytes = TRUE
)
