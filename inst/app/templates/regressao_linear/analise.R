## ---- configurar ----
# 1. Pacotes e escolhas da análise -------------------------------------------
# Leia este roteiro de cima para baixo. No RStudio, Ctrl+Enter executa a linha
# ou a seleção; Ctrl+Shift+O abre o sumário de seções do script.
#
# Mapa dos objetos desta regressão:
#   base_regressao       par de medidas usado no ajuste
#   modelo_lm            modelo criado por lm()
#   tabela_coeficientes  estimativas numéricas, sem arredondamento
#   metricas_modelo      R², teste F e outras medidas de ajuste
#   dados_diagnostico    resíduos e medidas de influência por observação
#   grafico_regressao    figura principal (digite o nome para exibi-la)
#   texto_resultados    frase com números calculados pelo modelo
#
# <- guarda o resultado com um nome; |> passa o objeto para a próxima função.
# Os rótulos e o nível de confiança abaixo vêm das escolhas feitas na IDE.
# broom organiza os resultados; performance oferece o teste de variância.
# Carregamos só as partes do tidyverse usadas neste roteiro.
library(dplyr)
library(ggplot2)
library(broom)
library(performance)
variavel_resposta <- {{RESPOSTA}}
variavel_preditor <- {{PREDITOR}}
# Os rótulos mudam só a apresentação; os nomes das colunas continuam no código.
# Se o pesquisador não escreveu um rótulo, a variável aparece pelo próprio nome.
rotulo_resposta <- {{ROTULO_RESPOSTA_R}}
if (!nzchar(trimws(rotulo_resposta))) rotulo_resposta <- variavel_resposta
rotulo_preditor <- {{ROTULO_PREDITOR_R}}
if (!nzchar(trimws(rotulo_preditor))) rotulo_preditor <- variavel_preditor
titulo_analise <- {{TITULO_R}}
nivel_confianca <- {{CONFIANCA}}
if (length(nivel_confianca) != 1 || !is.finite(nivel_confianca) ||
    nivel_confianca <= 0 || nivel_confianca >= 1) {
  stop("O nível de confiança deve estar entre 0 e 1, por exemplo 0.95.")
}
alfa <- 1 - nivel_confianca
# Ative somente se as linhas estiverem na ordem real de coleta (tempo/espaço).
# Uma ordenação arbitrária, como ordenar pelo tamanho do peixe, não serve.
avaliar_autocorrelacao <- {{AUTOCORRELACAO}}
mostrar_equacao <- {{EQUACAO}}
tema_grafico <- {{TEMA}}

## ---- preparar ----
# 2. Conferir os dados antes de ajustar -------------------------------------
# A base preparada vem do trecho anterior; os dados brutos ficam intactos.
base_regressao <- dados_da_analise |>
  select(all_of(c(variavel_resposta, variavel_preditor)))
# all_of() seleciona pelos nomes guardados em variavel_resposta e variavel_preditor.
if (variavel_resposta == variavel_preditor) {
  stop("Escolha variáveis diferentes para a resposta e o preditor.")
}
if (!all(vapply(base_regressao, is.numeric, logical(1)))) {
  stop("Resposta e preditor precisam ser numéricos. Confira a tipagem no preparo.")
}
# Retiramos apenas pares incompletos; registramos quantas linhas saíram.
n_antes <- nrow(base_regressao)
# Guarda a linha da base preparada para localizar os pontos nos diagnósticos.
linhas_utilizadas <- which(complete.cases(base_regressao))
base_regressao <- base_regressao |> filter(if_all(everything(), ~ !is.na(.x)))
n_excluidos <- n_antes - nrow(base_regressao)
if (nrow(base_regressao) < 3) {
  stop("A regressão precisa de pelo menos três pares completos de observações.")
}
if (!all(vapply(base_regressao, function(x) all(is.finite(x)), logical(1)))) {
  stop("Há valores infinitos. Confira os cálculos no preparo antes da regressão.")
}
if (any(vapply(base_regressao, function(x) length(unique(x)) < 2, logical(1)))) {
  stop("Resposta e preditor precisam variar; uma coluna constante não permite esta análise.")
}

## ---- modelo ----
# 3. Ajustar a reta ---------------------------------------------------------
# Y ~ X diz: explicar a resposta a partir do preditor.
modelo_lm <- lm({{FORMULA}}, data = base_regressao)
# Veja o console cru uma vez para reconhecer a saída fora da CatalyseR.
summary(modelo_lm)
# tidy: uma linha por coeficiente. glance: uma linha para o modelo inteiro.
tabela_coeficientes <- broom::tidy(modelo_lm, conf.int = TRUE, conf.level = nivel_confianca)
metricas_modelo <- broom::glance(modelo_lm)
# augment acrescenta ajustados, resíduos e influência às observações utilizadas.
# .fitted = ajustado; .resid = resíduo; .std.resid = resíduo padronizado;
# .hat = alavancagem; .cooksd = distância de Cook. O ponto faz parte do nome.
dados_diagnostico <- broom::augment(modelo_lm)
dados_diagnostico$.linha_base <- linhas_utilizadas

## ---- pressupostos ----
# 4. Examinar os pressupostos -----------------------------------------------
# Normalidade é uma suposição sobre os erros, examinada pelos resíduos.
# Shapiro aceita de 3 a 5000 resíduos; fora disso, usamos o gráfico Q-Q.
residuos <- dados_diagnostico$.resid
teste_shapiro <- NULL
if (length(residuos) >= 3 && length(residuos) <= 5000 && sd(residuos) > 0) {
  teste_shapiro <- shapiro.test(residuos)
}
w_shapiro <- if (is.null(teste_shapiro)) NA_real_ else unname(teste_shapiro$statistic)
p_shapiro <- if (is.null(teste_shapiro)) NA_real_ else teste_shapiro$p.value
# Breusch-Pagan examina se a variância dos erros muda com o ajuste.
# Uma falha de cálculo fica como ausente, nunca como pressuposto atendido.
teste_hetero <- tryCatch(performance::check_heteroscedasticity(modelo_lm),
  error = function(e) NA_real_)
p_hetero <- as.numeric(teste_hetero)
# Independência depende primeiro do delineamento: há medidas repetidas do
# mesmo tanque/peixe? O teste abaixo só investiga autocorrelação na ordem dada.
teste_autocorr <- NA_real_
if (avaliar_autocorrelacao) {
  set.seed(2026)
  teste_autocorr <- tryCatch(performance::check_autocorrelation(modelo_lm),
    error = function(e) NA_real_)
}
p_autocorr <- as.numeric(teste_autocorr)
frase_normalidade <- case_when(
  is.na(p_shapiro) ~ "Shapiro-Wilk não foi calculado; examine o gráfico Q-Q",
  p_shapiro < alfa ~ "houve evidência de desvio da normalidade dos resíduos",
  TRUE ~ "não houve evidência de desvio da normalidade dos resíduos"
)
frase_variancia <- case_when(
  is.na(p_hetero) ~ "Breusch-Pagan não forneceu p-valor válido; examine os resíduos",
  p_hetero < alfa ~ "houve evidência de variância não constante",
  TRUE ~ "não houve evidência de variância não constante"
)
frase_independencia <- case_when(
  !avaliar_autocorrelacao ~ "Autocorrelação não testada: avalie a independência pelo delineamento.",
  is.na(p_autocorr) ~ "Durbin-Watson não forneceu p-valor válido.",
  p_autocorr < alfa ~ "Houve evidência de autocorrelação na ordem informada.",
  TRUE ~ "Não houve evidência de autocorrelação na ordem informada; isso não comprova independência."
)
tabela_pressupostos <- tibble::tibble(
  Teste = c("Shapiro-Wilk", "Breusch-Pagan", "Durbin-Watson"),
  `p-valor` = formatar_p(c(p_shapiro, p_hetero, p_autocorr)),
  Leitura = c(frase_normalidade, frase_variancia, frase_independencia)
)

## ---- texto ----
# 5. Preparar o texto a partir dos números, sem decisões por arredondamento ---
# str_glue() substitui expressões entre {chaves} pelos valores dos objetos.
# A segunda linha de tabela_coeficientes é a inclinação; a primeira, o intercepto.
inclinacao <- tabela_coeficientes[2, ]
beta <- inclinacao$estimate
p_inclinacao <- inclinacao$p.value
frase_associacao <- case_when(
  is.na(p_inclinacao) ~ "não foi possível avaliar a evidência de associação linear",
  p_inclinacao < alfa ~ "houve evidência de associação linear",
  TRUE ~ "não houve evidência de associação linear"
)
direcao <- case_when(beta > 0 ~ "positiva", beta < 0 ~ "negativa", TRUE ~ "nula")
mudanca <- case_when(beta > 0 ~ "aumenta", beta < 0 ~ "diminui", TRUE ~ "não muda")
ic_percentual <- fmt(100 * nivel_confianca, 0)
gl_modelo <- unname(summary(modelo_lm)$fstatistic[2])
gl_residuo <- df.residual(modelo_lm)
# Associação descreve os dados; causalidade exige justificativa no estudo.
texto_resultados <- stringr::str_glue(
  "Foi ajustada uma regressão linear simples de {rotulo_resposta} em função de {rotulo_preditor}, ",
  "com {nrow(base_regressao)} observações completas ({n_excluidos} pares incompletos excluídos). ",
  "O modelo explicou {fmt(100 * metricas_modelo$r.squared)}% da variabilidade da resposta ",
  "(R² = {fmt(metricas_modelo$r.squared)}; R² ajustado = {fmt(metricas_modelo$adj.r.squared)}; ",
  "F({gl_modelo}, {gl_residuo}) = {fmt(metricas_modelo$statistic)}; ",
  "{formatar_p(metricas_modelo$p.value, no_texto = TRUE)}). ",
  "Para o preditor, {frase_associacao}; a inclinação estimada foi {direcao} ",
  "(β = {fmt(beta)}; EP = {fmt(inclinacao$std.error)}; ",
  "IC {ic_percentual}% [{fmt(inclinacao$conf.low)}; {fmt(inclinacao$conf.high)}]; ",
  "t({gl_residuo}) = {fmt(inclinacao$statistic)}; {formatar_p(p_inclinacao, no_texto = TRUE)}). ",
  "No modelo ajustado, para cada unidade adicional de {rotulo_preditor}, ",
  "o valor médio esperado de {rotulo_resposta} {mudanca} em {fmt(abs(beta))} unidades, ",
  "dentro da faixa observada."
)
texto_pressupostos <- stringr::str_glue(
  "Na análise dos resíduos, {frase_normalidade} ",
  "(Shapiro-Wilk: W = {fmt(w_shapiro)}; {formatar_p(p_shapiro, no_texto = TRUE)}); ",
  "{frase_variancia} (Breusch-Pagan: {formatar_p(p_hetero, no_texto = TRUE)}). ",
  "{frase_independencia}"
)
if (avaliar_autocorrelacao && !is.na(p_autocorr)) {
  texto_pressupostos <- paste(texto_pressupostos,
    paste0("Durbin-Watson: ", formatar_p(p_autocorr, no_texto = TRUE), "."))
}
alerta_pressupostos <- case_when(
  any(c(p_shapiro, p_hetero, p_autocorr) < alfa, na.rm = TRUE) ~
    "Há sinais de inadequação: examine os gráficos e o delineamento antes de interpretar os testes e intervalos usuais do modelo.",
  TRUE ~ "A ausência de evidência nos testes não comprova os pressupostos; complete a avaliação com os gráficos e o delineamento."
)
# A síntese acompanha a análise ao renderizar; não congela números no texto.
# Não declara os pressupostos atendidos, nem converte associação em causalidade.
texto_conclusao <- stringr::str_glue(
  "Na amostra analisada, {frase_associacao} entre {rotulo_preditor} e {rotulo_resposta}. ",
  "A inclinação estimada foi {fmt(beta)} (IC {ic_percentual}% ",
  "[{fmt(inclinacao$conf.low)}; {fmt(inclinacao$conf.high)}]), ",
  "e a reta descreveu {fmt(100 * metricas_modelo$r.squared)}% da variabilidade da resposta. ",
  "A interpretação se limita à faixa observada e depende da adequação do modelo ",
  "e do delineamento; não estabelece uma relação causal. {alerta_pressupostos}"
)

## ---- tabela ----
# 6. Tabela para o artigo ---------------------------------------------------
# Os objetos anteriores conservam precisão; só a apresentação é arredondada.
tabela_artigo <- tabela_coeficientes |>
  transmute(
    Parâmetro = c("Intercepto", variavel_preditor),
    `β estimado` = fmt(estimate),
    `EP` = fmt(std.error),
    `IC` = paste0("[", fmt(conf.low), "; ", fmt(conf.high), "]"),
    t = fmt(statistic),
    `p-valor` = formatar_p(p.value)
  )
names(tabela_artigo)[4] <- paste0("IC ", ic_percentual, "%")
flextable_ocean(tabela_artigo)

## ---- metricas ----
# AIC serve para comparar modelos compatíveis ajustados às mesmas observações.
# Seu valor isolado não classifica o modelo como bom ou ruim.
tabela_metricas <- tibble::tibble(
  Métrica = c("N", "R²", "R² ajustado", "Erro padrão residual", "F", "GL do modelo", "GL residual", "p do modelo", "AIC"),
  Valor = c(fmt(nrow(base_regressao), 0), fmt(metricas_modelo$r.squared),
    fmt(metricas_modelo$adj.r.squared), fmt(metricas_modelo$sigma),
    fmt(metricas_modelo$statistic), fmt(gl_modelo, 0), fmt(gl_residuo, 0),
    formatar_p(metricas_modelo$p.value), fmt(metricas_modelo$AIC))
)
flextable_ocean(tabela_metricas)

## ---- grafico ----
# 7. Reta e intervalo de confiança da resposta média ------------------------
# A faixa não é um intervalo de predição para um peixe individual.
# A equação vem do mesmo modelo que alimenta a tabela, sem novo ajuste.
equacao <- stringr::str_glue(
  "ŷ = {fmt(coef(modelo_lm)[1])} {ifelse(beta < 0, '−', '+')} {fmt(abs(beta))} x; R² = {fmt(metricas_modelo$r.squared)}"
)
tema_escolhido <- switch(tema_grafico,
  minimal = theme_minimal(base_size = 12), classic = theme_classic(base_size = 12),
  bw = theme_bw(base_size = 12), gray = theme_gray(base_size = 12),
  light = theme_light(base_size = 12), theme_classic(base_size = 12))
grafico_regressao <- ggplot(base_regressao,
  aes(x = .data[[variavel_preditor]], y = .data[[variavel_resposta]])) +
  geom_point(alpha = 0.7, size = 2.5, color = "#0F3B5F") +
  geom_smooth(method = "lm", formula = y ~ x, level = nivel_confianca,
    color = "#E76F51", fill = "#E76F51", alpha = 0.15, se = TRUE) +
  labs(x = rotulo_preditor, y = rotulo_resposta,
    title = if (nzchar(trimws(titulo_analise))) titulo_analise else NULL,
    subtitle = if (mostrar_equacao) equacao else NULL) +
  tema_escolhido
grafico_regressao

## ---- diagnostico-variancia ----
# 8.1. Linearidade e variância: resíduos versus ajustados ----------------------
# Curvatura sugere que uma reta não descreve bem a média; um funil sugere
# variância não constante. Procure uma nuvem sem padrão em torno de zero.
grafico_residuos <- ggplot(dados_diagnostico, aes(x = .fitted, y = .resid)) +
  geom_point(color = "#2E7D8F", alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(x = "Valores ajustados", y = "Resíduos") + theme_classic()
grafico_residuos

## ---- diagnostico-normalidade ----
# 8.2. Normalidade: gráfico Q-Q ----------------------------------------------
# Desvios sistemáticos da reta, sobretudo nas caudas, merecem investigação.
grafico_qq <- ggplot(dados_diagnostico, aes(sample = .std.resid)) +
  stat_qq(color = "#2E7D8F") + stat_qq_line(color = "#E76F51") +
  labs(x = "Quantis teóricos", y = "Resíduos padronizados") + theme_classic()
grafico_qq

## ---- diagnostico-ordem ----
# 8.3. Ordem de coleta, quando documentada -----------------------------------
# A ordem abaixo é a das linhas utilizadas. Só interprete como sequência de
# coleta se ela realmente representar tempo ou posição no estudo.
grafico_ordem <- NULL
if (avaliar_autocorrelacao) {
  grafico_ordem <- ggplot(dados_diagnostico, aes(x = seq_along(.resid), y = .resid)) +
    geom_line(color = "#2E7D8F") + geom_point(color = "#0F3B5F") +
    geom_hline(yintercept = 0, linetype = 2) +
    labs(x = "Ordem das observações", y = "Resíduos") + theme_classic()
  grafico_ordem
}

## ---- diagnostico-influencia ----
# 8.4. Influência: distância de Cook -----------------------------------------
# Cook é complementar: pontos altos pedem conferência, não exclusão automática.
# O limite 4/n é uma referência de triagem, não um teste de hipótese.
grafico_cook <- ggplot(dados_diagnostico, aes(x = .linha_base, y = .cooksd)) +
  geom_col(fill = "#2E7D8F") +
  geom_hline(yintercept = 4 / nrow(base_regressao), linetype = 2, color = "#E76F51") +
  labs(x = "Linha da base preparada", y = "Distância de Cook") + theme_classic()
grafico_cook

## ---- diagnostico-alavancagem ----
# 8.5. Alavancagem e resíduos padronizados -----------------------------------
# Alavancagem mede o quanto X está distante do centro dos valores observados.
# Um X extremo pode ter resíduo pequeno: por isso lemos alavancagem e Cook juntos.
# 2p/n e |resíduo padronizado| > 2 são referências de triagem, não testes.
limite_alavancagem <- 2 * modelo_lm$rank / nrow(base_regressao)
diagnostico_alavancagem <- dados_diagnostico |>
  mutate(sinalizado = .hat > limite_alavancagem | abs(.std.resid) > 2 |
    .cooksd > 4 / nrow(base_regressao))
grafico_alavancagem <- ggplot(diagnostico_alavancagem,
  aes(x = .hat, y = .std.resid)) +
  geom_point(color = "#2E7D8F", alpha = 0.7) +
  geom_hline(yintercept = c(-2, 0, 2), linetype = 2, color = "#0F3B5F") +
  geom_vline(xintercept = limite_alavancagem, linetype = 2, color = "#E76F51") +
  geom_text(data = filter(diagnostico_alavancagem, sinalizado),
    aes(label = .linha_base), vjust = -0.7, check_overlap = TRUE, size = 3) +
  labs(x = "Alavancagem", y = "Resíduos padronizados") + theme_classic()
grafico_alavancagem
