# {{TITULO_COMENTARIO}} — ROTEIRO DE ANÁLISE
# x========================================================================x
# Pergunta: {{PERGUNTA_COMENTARIO}}
#
# COMO ESTUDAR
# Abra o arquivo .Rproj e execute as seções na ordem, de cima para baixo.
# No RStudio, Ctrl+Enter executa a linha ou a seleção. Digite o nome de um
# objeto no console para examiná-lo, por exemplo: tabela_coeficientes.
# O sumário do editor (Ctrl+Shift+O) permite navegar entre as seções numeradas.
#
# MAPA DO ROTEIRO
#  1–3. Preparar o ambiente, ler a planilha e selecionar o par de medidas.
#  4–6. Explorar, ajustar a reta e examinar seus diagnósticos.
#  7–8. Preparar as tabelas e construir os gráficos.
#    9. Preparar os textos que serão usados nos relatórios.
# 10–11. Salvar cópias dos resultados e registrar as versões utilizadas.
#
# OBJETOS QUE OS RELATÓRIOS VÃO USAR
# base_regressao              dados e identificadores das observações analisadas
# modelo_lm                   modelo ajustado por lm()
# tabela_coeficientes         estimativas numéricas, sem arredondamento
# tabela_coeficientes_exibir  versão formatada da tabela
# grafico_regressao           figura pronta para exibir ou salvar
# texto_ajuste                frase com os resultados calculados
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
# Não muda a pasta de trabalho como setwd(). Abra o projeto antes de rodar.
here::i_am("R/analise.R")
library(broom)
library(performance)
library(flextable)
library(stringr)
# As funções abaixo cuidam da apresentação; os cálculos continuam neste script.
source(here::here("R", "funcoes.R"), encoding = "UTF-8")

# 2. Definir as escolhas e organizar as saídas -----------------------------
# Os nomes e rótulos abaixo vêm das escolhas feitas na CatalyseR.
variavel_resposta <- {{RESPOSTA_R}}
variavel_preditor <- {{PREDITOR_R}}
variavel_grupo <- {{GRUPO_R}}
# Os rótulos são textos de apresentação: alterá-los não renomeia as colunas.
rotulo_resposta <- {{ROTULO_RESPOSTA_R}}
rotulo_preditor <- {{ROTULO_PREDITOR_R}}
rotulo_grupo <- {{ROTULO_GRUPO_R}}
# Os nomes curtos são usados nas tabelas; os rótulos acima, nos eixos.
nome_resposta <- rotulo_resposta
nome_preditor <- rotulo_preditor
nivel_confianca <- {{CONFIANCA}}
alfa <- 1 - nivel_confianca
mostrar_equacao <- {{EQUACAO}}
avaliar_autocorrelacao <- {{AUTOCORRELACAO}}
titulo_grafico <- {{TITULO_R}}
cores_grupo <- c("#0F3B5F", "#2E7D8F", "#62B6B7", "#E89B3C", "#E76F51")

# Estas pastas guardam produtos regeneráveis. Os dados brutos ficam intactos.
# O laço cria cada pasta dentro do projeto, caso ela ainda não exista.
# recursive = TRUE cria também as pastas intermediárias, como saida/.
# showWarnings = FALSE silencia avisos, inclusive o de pasta já existente;
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

# 3. Preparar a base da regressão ------------------------------------------
# Mantemos identificação e medidas JUNTAS. O modelo usará só o par escolhido.
# |> encaminha uma tabela para a próxima operação; mutate() cria/altera colunas.
# linha_original conserva a posição na base preparada adotada pela análise.
dados_preparados <- as.data.frame(dados_da_analise) |>
  mutate(linha_original = row_number(), .before = 1)

tem_grupo <- !is.na(variavel_grupo) &&
  nzchar(variavel_grupo) &&
  variavel_grupo %in% names(dados_preparados)
if (tem_grupo) {
  dados_preparados[[variavel_grupo]] <- factor(dados_preparados[[variavel_grupo]])
}

# Este objeto serve apenas para conferir as medidas numéricas na exploração.
# where(is.numeric) seleciona colunas do tipo numérico. Não é outra base salva.
# Retiramos linha_original porque ela é uma identificação criada pelo roteiro.
medidas_numericas <- dados_preparados |>
  select(where(is.numeric), -linha_original)

# if (...) verifica uma condição; quando ela é TRUE, executa o bloco.
# Aqui, stop() interrompe a análise e mostra a mensagem do problema encontrado.
if (variavel_resposta == variavel_preditor ||
    !all(c(variavel_resposta, variavel_preditor) %in% names(medidas_numericas))) {
  stop("Escolha duas colunas numéricas diferentes e confira seus nomes.")
}
if (nivel_confianca <= 0 || nivel_confianca >= 1) stop("Confira o nível de confiança.")
# A regressão utiliza apenas Y e X; faltantes em outras colunas não excluem linhas.
variaveis_modelo <- c(variavel_resposta, variavel_preditor)
# all_of() lê os nomes guardados no vetor acima, evitando repetir os nomes.
pares <- dados_preparados |> select(all_of(variaveis_modelo))
# TRUE marca um par completo; FALSE marca uma linha com pelo menos um NA.
linhas_completas <- complete.cases(pares)
# [linhas_completas, ] conserva essas linhas e todas as colunas antes do select.
colunas_identificacao <- names(dados_preparados)[
  !vapply(dados_preparados, is.numeric, logical(1))
]
colunas_rastreio <- unique(c("linha_original", colunas_identificacao))
base_regressao <- dados_preparados[linhas_completas, ] |>
  select(all_of(colunas_rastreio), all_of(variaveis_modelo))
n_total <- nrow(dados_preparados)
n_utilizado <- nrow(base_regressao)
# ! inverte TRUE/FALSE: pares incompletos passam a TRUE. Na soma, TRUE vale 1
# e FALSE vale 0; assim contamos quantas observações ficaram fora do ajuste.
n_excluido <- sum(!linhas_completas)
if (n_utilizado < 3) stop("São necessários pelo menos três pares completos.")
# Conferimos uma variável de cada vez. [[nome]] pega a coluna com aquele nome.
for (nome in variaveis_modelo) {
  valores <- base_regressao[[nome]]
  if (!all(is.finite(valores))) {
    stop("Há valores infinitos em ", nome, "; confira o preparo.")
  }
  if (length(unique(valores)) < 2) {
    stop("A variável ", nome, " precisa variar para ajustar a reta.")
  }
}

# 4. Explorar as medidas e o possível agrupamento --------------------------
# Veja os tipos, as ausências e as amplitudes antes de interpretar a reta.
# Criamos uma linha por medida e preenchemos cada resumo no laço abaixo.
# 0L é um zero inteiro; NA_real_ reserva uma posição numérica ainda sem valor.
tabela_colunas <- data.frame(
  Coluna = names(medidas_numericas),
  Ausentes = 0L,
  Minimo = NA_real_,
  Maximo = NA_real_
)
# seq_along() percorre as posições das medidas: 1, 2, ...
# Em cada passagem, preenchemos ausências, mínimo e máximo daquela medida.
# na.rm = TRUE ignora faltantes; se todos faltarem, mantemos mínimo/máximo em NA.
for (i in seq_along(medidas_numericas)) {
  valores <- medidas_numericas[[i]]
  tabela_colunas$Ausentes[i] <- sum(is.na(valores))
  if (!all(is.na(valores))) {
    tabela_colunas$Minimo[i] <- min(valores, na.rm = TRUE)
    tabela_colunas$Maximo[i] <- max(valores, na.rm = TRUE)
  }
}
# fmt() muda só a apresentação. tabela_colunas preserva os valores numéricos.
tabela_colunas_exibir <- tabela_colunas |>
  mutate(Minimo = fmt(Minimo), Maximo = fmt(Maximo)) |>
  rename(Mínimo = Minimo, Máximo = Maximo)
# Se uma variável de agrupamento foi escolhida para a exploração, contamos
# quantas observações chegaram e quantas entraram na reta em cada nível.
tabela_grupos <- NULL
if (tem_grupo) {
  contagem_utilizados <- base_regressao |>
    count(.data[[variavel_grupo]], name = "N utilizado")
  tabela_grupos <- dados_preparados |>
    count(.data[[variavel_grupo]], name = "N recebido") |>
    left_join(contagem_utilizados, by = variavel_grupo) |>
    mutate(`N utilizado` = coalesce(`N utilizado`, 0L))
  names(tabela_grupos)[1] <- rotulo_grupo
}
# A mesma lógica, agora só para as duas medidas usadas na regressão.
tabela_descritiva <- data.frame(
  Variável = c(nome_resposta, nome_preditor),
  n = n_utilizado,
  Média = NA_real_,
  DP = NA_real_,
  Mínimo = NA_real_,
  Máximo = NA_real_,
  check.names = FALSE
)
for (i in seq_along(variaveis_modelo)) {
  valores <- base_regressao[[variaveis_modelo[i]]]
  tabela_descritiva$Média[i] <- mean(valores)
  tabela_descritiva$DP[i] <- sd(valores)
  tabela_descritiva$Mínimo[i] <- min(valores)
  tabela_descritiva$Máximo[i] <- max(valores)
}
# across() aplica a mesma formatação a todas as colunas indicadas.
tabela_descritiva_exibir <- tabela_descritiva |>
  mutate(across(c(Média, DP, Mínimo, Máximo), fmt))

# 5. Ajustar a reta e extrair os resultados --------------------------------
# lm ajusta Y ~ X por mínimos quadrados ordinários, como na CatalyseR.
# reformulate() monta resposta ~ preditor a partir dos nomes definidos na
# seção 2. Digite formula_modelo no console para ver a fórmula resultante.
formula_modelo <- reformulate(variavel_preditor, response = variavel_resposta)
modelo_lm <- lm(formula_modelo, data = base_regressao)
resumo_console <- summary(modelo_lm)
# Ao estudar o script, execute resumo_console para conhecer a saída bruta uma vez.
# tidy() dá uma linha por coeficiente; glance() resume o modelo inteiro.
tabela_coeficientes <- broom::tidy(
  modelo_lm,
  conf.int = TRUE,
  conf.level = nivel_confianca
)
metricas_modelo <- broom::glance(modelo_lm)
# fstatistic guarda F e seus dois graus de liberdade. [2] pega o grau de
# liberdade do numerador; unname() retira seu nome ("numdf"), mantendo o valor.
gl_modelo <- unname(resumo_console$fstatistic[2])
gl_residuo <- df.residual(modelo_lm)

# 6. Examinar os resíduos e a influência -----------------------------------
# Os pressupostos dizem respeito aos erros; os resíduos ajudam a examiná-los.
# augment() acrescenta .fitted (ajustado), .resid (resíduo), .std.resid
# (resíduo padronizado), .hat (alavancagem) e .cooksd (distância de Cook).
# O ponto no início faz parte do nome da coluna, não é uma operação.
dados_diagnostico <- broom::augment(modelo_lm, data = base_regressao)
teste_shapiro <- NULL
if (n_utilizado <= 5000 && sd(residuals(modelo_lm)) > 0) {
  teste_shapiro <- shapiro.test(residuals(modelo_lm))
}
w_shapiro <- if (is.null(teste_shapiro)) NA_real_ else unname(teste_shapiro$statistic)
p_shapiro <- if (is.null(teste_shapiro)) NA_real_ else teste_shapiro$p.value
teste_hetero <- performance::check_heteroscedasticity(modelo_lm)
p_hetero <- as.numeric(teste_hetero)

# Durbin-Watson só é calculado quando a ordem das linhas representa uma
# sequência real de coleta. A independência também depende do delineamento.
teste_autocorr <- NULL
if (avaliar_autocorrelacao) {
  teste_autocorr <- performance::check_autocorrelation(modelo_lm)
}
p_autocorr <- if (is.null(teste_autocorr)) NA_real_ else as.numeric(teste_autocorr)
# Cook e alavancagem são sinais para investigar, nunca ordens para apagar linhas.
limite_cook <- 4 / n_utilizado
limite_alavancagem <- 2 * length(coef(modelo_lm)) / n_utilizado
dados_diagnostico <- dados_diagnostico |>
  mutate(
    cook_alto = .cooksd > limite_cook,
    alavancagem_alta = .hat > limite_alavancagem,
    residuo_extremo = abs(.std.resid) > 3,
    investigar = cook_alto | alavancagem_alta | residuo_extremo
  )
pontos_investigar <- dados_diagnostico |> filter(investigar)
n_cook <- sum(dados_diagnostico$cook_alto)
n_alavancagem <- sum(dados_diagnostico$alavancagem_alta)
tabela_influencia <- pontos_investigar |>
  transmute(
    Linha = linha_original,
    Cook = fmt(.cooksd, 3),
    Alavancagem = fmt(.hat, 3),
    `Resíduo padronizado` = fmt(.std.resid)
  )
if (tem_grupo) {
  tabela_influencia[[rotulo_grupo]] <- as.character(
    pontos_investigar[[variavel_grupo]]
  )
  tabela_influencia <- tabela_influencia |>
    select(Linha, all_of(rotulo_grupo), everything())
}

leitura_shapiro <- if (is.na(p_shapiro)) {
  "Teste não calculado; examine o Q-Q."
} else if (p_shapiro < alfa) {
  "Há evidência de desvio da normalidade dos resíduos."
} else "Não houve evidência de desvio da normalidade dos resíduos."
leitura_hetero <- if (is.na(p_hetero)) {
  "Teste sem resultado válido; examine os resíduos."
} else if (p_hetero < alfa) {
  "Há evidência de variância não constante."
} else "Não houve evidência de variância não constante."
leitura_autocorr <- if (!avaliar_autocorrelacao) {
  "Não avaliado: confirme a independência pelo delineamento."
} else if (is.na(p_autocorr)) {
  "Teste sem resultado válido; examine a ordem documentada."
} else if (p_autocorr < alfa) {
  "Há evidência de autocorrelação na ordem informada."
} else "Não houve evidência de autocorrelação na ordem informada."
tabela_testes <- data.frame(
  Teste = c("Shapiro-Wilk", "Breusch-Pagan", "Durbin-Watson"),
  `p-valor` = formatar_p(c(p_shapiro, p_hetero, p_autocorr)),
  Leitura = c(leitura_shapiro, leitura_hetero, leitura_autocorr),
  check.names = FALSE
)

# 7. Preparar as tabelas de apresentação -----------------------------------
# Formatação fica aqui também: os QMDs apenas mostram os objetos prontos.
# A primeira linha é o intercepto; a segunda, o coeficiente do preditor.
coef_preditor <- tabela_coeficientes[2, ]
ic_percentual <- fmt(100 * nivel_confianca, 0)
tabela_coeficientes_exibir <- tabela_coeficientes |>
  transmute(
    Parâmetro = c("Intercepto", nome_preditor),
    `β estimado` = fmt(estimate, 3),
    EP = fmt(std.error, 3),
    `IC` = paste0("[", fmt(conf.low, 3), "; ", fmt(conf.high, 3), "]"),
    t = fmt(statistic),
    `p-valor` = formatar_p(p.value)
  )
names(tabela_coeficientes_exibir)[4] <- paste0("IC ", ic_percentual, "%")
tabela_ajuste <- data.frame(
  Indicador = c(
    "N",
    "R²",
    "R² ajustado",
    "F (gl do modelo; gl residual)",
    "p do modelo",
    "Desvio padrão residual"
  ),
  Valor = c(
    n_utilizado,
    fmt(metricas_modelo$r.squared, 3),
    fmt(metricas_modelo$adj.r.squared, 3),
    paste0(fmt(metricas_modelo$statistic), " (", gl_modelo, "; ", gl_residuo, ")"),
    formatar_p(metricas_modelo$p.value),
    fmt(metricas_modelo$sigma, 3)
  )
)

# 8. Construir os gráficos ------------------------------------------------
# Cada gráfico recebe um nome. Digite esse nome para vê-lo no painel Plots;
# o QMD usa a mesma chamada, e ggsave() salva uma cópia no final do script.
# Calculamos a reta e seu IC a partir do MESMO modelo, sem um segundo ajuste.
# A grade tem 100 posições de X para desenhar uma linha contínua; não cria
# novas observações nem aumenta o tamanho da amostra.
valores_x <- base_regressao[[variavel_preditor]]
grade_predicao <- data.frame(
  x = seq(min(valores_x), max(valores_x), length.out = 100)
)
# predict() precisa de uma coluna com o mesmo nome do preditor no modelo.
names(grade_predicao) <- variavel_preditor
predicoes <- predict(
  modelo_lm,
  newdata = grade_predicao,
  interval = "confidence",
  level = nivel_confianca
)
# A saída tem fit (média prevista), lwr (limite inferior) e upr (superior).
grade_predicao <- bind_cols(grade_predicao, as.data.frame(predicoes))

# A equação é uma anotação do gráfico; fica junto da sua construção.
sinal_inclinacao <- if (coef_preditor$estimate < 0) "−" else "+"
equacao_texto <- stringr::str_glue(
  "ŷ = {fmt(coef(modelo_lm)[1], 3)} {sinal_inclinacao} ",
  "{fmt(abs(coef_preditor$estimate), 3)} × x; ",
  "R² = {fmt(metricas_modelo$r.squared, 3)}"
)

# 8.1. Reta e faixa de confiança
# .data[[...]] informa ao ggplot qual coluna usar a partir de um nome em texto.
grafico_regressao <- ggplot(
  base_regressao,
  aes(
    x = .data[[variavel_preditor]],
    y = .data[[variavel_resposta]]
  )
) +
  # geom_ribbon() desenha a faixa entre lwr e upr: o IC da resposta média.
  # Não é o intervalo de predição para uma nova observação individual.
  # inherit.aes = FALSE usa só os eixos desta camada; alpha controla a transparência.
  # .data[[variavel_preditor]] busca, nos dados desta camada,
  # a coluna cujo nome foi escolhido no início do script.
  geom_ribbon(
    data = grade_predicao,
    aes(
      x = .data[[variavel_preditor]],
      ymin = lwr,
      ymax = upr
    ),
    inherit.aes = FALSE,
    fill = "#62B6B7",
    alpha = 0.3
  ) +
  geom_point(colour = "#0F3B5F", alpha = 0.75, size = 2) +
  # A linha herda o mapeamento de X definido em ggplot(), agora avaliado em
  # grade_predicao. aes(y = fit) troca apenas Y pela média prevista pelo modelo.
  geom_line(data = grade_predicao, aes(y = fit), colour = "#E76F51", linewidth = 0.8) +
  labs(
    title = if (nzchar(titulo_grafico)) titulo_grafico else NULL,
    x = rotulo_preditor,
    y = rotulo_resposta,
    subtitle = if (mostrar_equacao) equacao_texto else NULL
  ) +
  tema_projeto()

# 8.2. Dispersão por grupo (exploração, sem outro modelo)
grafico_grupos <- NULL
if (tem_grupo) {
  paleta_grupos <- rep(cores_grupo, length.out = nlevels(base_regressao[[variavel_grupo]]))
  grafico_grupos <- ggplot(
    base_regressao,
    aes(
      x = .data[[variavel_preditor]],
      y = .data[[variavel_resposta]],
      colour = .data[[variavel_grupo]],
      shape = .data[[variavel_grupo]]
    )
  ) +
    geom_point(size = 2.4, alpha = 0.85) +
    scale_colour_manual(values = paleta_grupos) +
    labs(
      x = rotulo_preditor,
      y = rotulo_resposta,
      colour = rotulo_grupo,
      shape = rotulo_grupo
    ) +
    tema_projeto()
}

# 8.3. Resíduos versus ajustados: procure curvas e mudança de dispersão.
# A curva loess é uma suavização para revelar padrões nos resíduos.
# Ela auxilia o diagnóstico e não substitui a reta ajustada em modelo_lm.
grafico_residuos <- ggplot(dados_diagnostico, aes(.fitted, .resid)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_point(colour = "#2E7D8F", alpha = 0.8) +
  geom_smooth(
    method = "loess",
    formula = y ~ x,
    se = FALSE,
    colour = "#E76F51"
  ) +
  labs(x = "Valores ajustados", y = "Resíduos") +
  tema_projeto()

# 8.4. Q-Q: procure afastamentos sistemáticos da reta de referência.
grafico_qq <- ggplot(dados_diagnostico, aes(sample = .std.resid)) +
  stat_qq(colour = "#2E7D8F") +
  stat_qq_line(colour = "#E76F51") +
  labs(x = "Quantis normais teóricos", y = "Resíduos padronizados") +
  tema_projeto()

# 8.5. Escala-localização: procure dispersão aproximadamente estável.
grafico_escala <- ggplot(
  dados_diagnostico,
  aes(.fitted, sqrt(abs(.std.resid)))
) +
  geom_point(colour = "#2E7D8F") +
  geom_smooth(
    method = "loess",
    formula = y ~ x,
    se = FALSE,
    colour = "#E76F51"
  ) +
  labs(x = "Valores ajustados", y = "Raiz de |resíduo padronizado|") +
  tema_projeto()

# 8.6. Cook: confira as observações sinalizadas antes de qualquer exclusão.
grafico_cook <- ggplot(dados_diagnostico, aes(linha_original, .cooksd)) +
  geom_segment(aes(xend = linha_original, yend = 0), colour = "#2E7D8F") +
  geom_hline(yintercept = limite_cook, linetype = "dashed", colour = "#E76F51") +
  geom_text(
    data = dados_diagnostico |> filter(cook_alto),
    aes(label = linha_original),
    vjust = -0.4,
    size = 3,
    check_overlap = TRUE
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(x = "Linha da base preparada", y = "Distância de Cook") +
  tema_projeto()

# 8.7. Alavancagem: valores extremos de X podem ter resíduos pequenos.
grafico_alavancagem <- ggplot(dados_diagnostico, aes(.hat, .std.resid)) +
  geom_hline(yintercept = c(-3, 0, 3), linetype = "dashed", colour = "grey60") +
  geom_vline(xintercept = limite_alavancagem, linetype = "dashed", colour = "#E76F51") +
  geom_point(aes(size = .cooksd), colour = "#2E7D8F", alpha = 0.75) +
  geom_text(
    data = pontos_investigar,
    aes(label = linha_original),
    vjust = -0.8,
    size = 3,
    check_overlap = TRUE
  ) +
  labs(x = "Alavancagem", y = "Resíduo padronizado", size = "Cook") +
  tema_projeto()

# 8.8. Resíduos por grupo: confira estrutura não representada na reta global.
grafico_residuos_grupo <- NULL
if (tem_grupo) {
  grafico_residuos_grupo <- ggplot(
    dados_diagnostico,
    aes(x = .data[[variavel_grupo]], y = .resid)
  ) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_boxplot(fill = "#62B6B7", width = 0.5, outlier.shape = NA) +
    geom_point(colour = "#0F3B5F", alpha = 0.6, size = 1.5) +
    labs(x = rotulo_grupo, y = "Resíduos da reta global") +
    tema_projeto()
}

# 8.9. Resíduos na ordem informada, apenas quando essa ordem tem significado.
grafico_ordem <- NULL
if (avaliar_autocorrelacao) {
  grafico_ordem <- ggplot(dados_diagnostico, aes(x = seq_along(.resid), y = .resid)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_line(colour = "#2E7D8F") +
    geom_point(colour = "#0F3B5F") +
    labs(x = "Ordem das observações", y = "Resíduos") +
    tema_projeto()
}

# 9. Preparar os textos que serão usados nos relatórios --------------------
# Estes textos organizam resultados calculados em frases para os relatórios.
#
# str_glue() substitui as expressões entre {chaves} pelos resultados dos objetos.
# A atribuição com <- guarda a frase no objeto; print() mostra seu conteúdo.
# Execute a criação e a linha print() para calcular e conferir cada texto.
#
# 9.1 Direção
# A direção vem do sinal da inclinação estimada: positiva indica que Y aumenta
# com X; negativa, que Y diminui; nula corresponde a uma inclinação igual a zero.
direcao <- if (coef_preditor$estimate > 0) {
  "positiva"
} else if (coef_preditor$estimate < 0) {
  "negativa"
} else {
  "nula"
}
print(direcao)

# 9.2 Evidência estatística
# A evidência compara o p-valor da inclinação com alfa, definido na seção 2.
# Se p < alfa, a frase indica evidência de associação linear; caso contrário,
# indica que não houve evidência suficiente, sem afirmar ausência de associação.
evidencia <- if (coef_preditor$p.value < alfa) {
  "houve evidência de associação linear"
} else {
  "não houve evidência de associação linear"
}
print(evidencia)

# 9.3 Textos que serão usados nos relatórios
texto_amostra <- stringr::str_glue(
  "Foram utilizadas {n_utilizado} das {n_total} observações disponíveis; ",
  "{n_excluido} pares incompletos foram excluídos."
)
print(texto_amostra)

texto_ajuste <- stringr::str_glue(
  "O modelo explicou {fmt(100 * metricas_modelo$r.squared, 1)}% da ",
  "variabilidade observada em {rotulo_resposta} ",
  "(R² = {fmt(metricas_modelo$r.squared, 3)}; ",
  "R² ajustado = {fmt(metricas_modelo$adj.r.squared, 3)}; ",
  "F({gl_modelo}, {gl_residuo}) = {fmt(metricas_modelo$statistic)}; ",
  "{formatar_p(metricas_modelo$p.value, no_texto = TRUE)})."
)
print(texto_ajuste)

texto_coeficiente <- stringr::str_glue(
  "Na reta global, {evidencia}. A inclinação estimada foi {direcao} ",
  "(β = {fmt(coef_preditor$estimate, 3)}; ",
  "EP = {fmt(coef_preditor$std.error, 3)}; ",
  "IC {ic_percentual}% [{fmt(coef_preditor$conf.low, 3)}; ",
  "{fmt(coef_preditor$conf.high, 3)}]; ",
  "t({gl_residuo}) = {fmt(coef_preditor$statistic)}; ",
  "{formatar_p(coef_preditor$p.value, no_texto = TRUE)}). ",
  "Uma unidade adicional em {rotulo_preditor} correspondeu a uma ",
  "variação média estimada de {fmt(coef_preditor$estimate, 3)} unidade em ",
  "{rotulo_resposta}, dentro da faixa observada."
)
print(texto_coeficiente)

# O artigo recebe frases mais curtas; o caderno recebe também a orientação de leitura.
texto_diagnosticos_artigo <- stringr::str_glue(
  "{leitura_shapiro} Shapiro-Wilk: W = {fmt(w_shapiro, 3)}; ",
  "{formatar_p(p_shapiro, no_texto = TRUE)}. {leitura_hetero} ",
  "Breusch-Pagan: {formatar_p(p_hetero, no_texto = TRUE)}. ",
  "{leitura_autocorr}"
)
print(texto_diagnosticos_artigo)

texto_diagnosticos <- paste(
  texto_diagnosticos_artigo,
  "Esses resultados não comprovam os pressupostos; a avaliação deve incluir",
  "os gráficos e o delineamento."
)
print(texto_diagnosticos)

texto_influencia_artigo <- stringr::str_glue(
  "A triagem de influência, alavancagem e resíduos sinalizou ",
  "{nrow(pontos_investigar)} observações para conferência; todas foram ",
  "mantidas no ajuste. Os diagnósticos detalhados acompanham o caderno HTML."
)
print(texto_influencia_artigo)

texto_influencia <- stringr::str_glue(
  "A triagem sinalizou {n_cook} observações com Cook > {fmt(limite_cook, 3)} ",
  "e {n_alavancagem} com alavancagem > {fmt(limite_alavancagem, 3)}. ",
  "Há {nrow(pontos_investigar)} observações distintas para conferir ao combinar ",
  "esses critérios e |resíduo padronizado| > 3. ",
  "Nenhuma observação foi removida por esses indicadores."
)
print(texto_influencia)

alerta_modelo <- if (any(c(p_shapiro, p_hetero) < alfa, na.rm = TRUE)) {
  "Os testes indicaram sinais de inadequação que exigem revisar a inferência usual do modelo."
} else if (tem_grupo) {
  "Os testes formais não detectaram os desvios examinados, mas os gráficos, o delineamento e a estrutura dos grupos continuam necessários."
} else "Os testes formais não detectaram os desvios examinados, mas os gráficos e o delineamento continuam necessários."
print(alerta_modelo)

# Síntese estatística: os argumentos científicos serão escritos no QMD.
texto_sintese_estatistica <- stringr::str_glue(
  "Na amostra de {n_utilizado} observações, {evidencia} entre as variáveis analisadas. ",
  "A inclinação estimada foi {direcao} (β = {fmt(coef_preditor$estimate, 3)}; ",
  "IC {ic_percentual}% [{fmt(coef_preditor$conf.low, 3)}; ",
  "{fmt(coef_preditor$conf.high, 3)}]; ",
  "{formatar_p(coef_preditor$p.value, no_texto = TRUE)}). ",
  "O ajuste apresentou R² = {fmt(metricas_modelo$r.squared, 3)}. ",
  "A interpretação deve considerar os diagnósticos e a estrutura da amostra."
)
print(texto_sintese_estatistica)

# 10. Salvar cópias para consulta e compartilhamento ------------------------
# CSV com ponto e vírgula e vírgula decimal abre bem no Excel em português.
# Estes arquivos são saídas: edite a análise no script, não o CSV gerado.

# Uma única base processada, com identificadores e o par efetivamente analisado.
# Os QMDs não precisam de RDS: eles executam este script e usam base_regressao.
write.csv2(
  base_regressao,
  here::here("dados", "processados", "base_regressao.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# A lista associa o nome do arquivo ao objeto já calculado.
# O laço repete somente a gravação, sem repetir nenhuma análise.
tabelas <- list(
  coeficientes = tabela_coeficientes,
  ajuste = metricas_modelo,
  diagnosticos_observacoes = dados_diagnostico,
  testes_diagnosticos = tabela_testes,
  descritiva = tabela_descritiva
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
  regressao = grafico_regressao,
  grupos = grafico_grupos,
  residuos = grafico_residuos,
  qq = grafico_qq,
  escala = grafico_escala,
  cook = grafico_cook,
  alavancagem = grafico_alavancagem,
  residuos_grupo = grafico_residuos_grupo,
  ordem = grafico_ordem
)
# Gráficos opcionais permanecem NULL quando não há grupo ou ordem documentada.
figuras <- Filter(Negate(is.null), figuras)
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

# 11. Registrar o ambiente computacional -----------------------------------
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
