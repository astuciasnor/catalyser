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
#  1–3. Preparar o ambiente, ler a planilha e organizar os dois grupos.
#  4–6. Explorar, conferir os pressupostos e aplicar o teste t.
#  7–8. Preparar as tabelas e construir os gráficos.
#    9. Preparar os textos que serão usados nos relatórios.
# 10–11. Salvar cópias dos resultados e registrar as versões utilizadas.
#
# OBJETOS QUE OS RELATÓRIOS VÃO USAR
# dados                      base com a resposta e o grupo (fator de dois níveis)
# teste_t                    resultado do t.test com as escolhas feitas no painel
# d_cohen                    tamanho do efeito (referência estatística)
# d_ic                       intervalo de confiança do d de Cohen
# tabela_descritiva_exibir   resumo por grupo, formatado
# tabela_teste               método, diferença, IC, t, gl, p e d, formatados
# grafico_caixa              boxplot com os pontos de cada observação
# grafico_medias             pontos, média (losango), rótulo média ± DP e letras
# grafico_residuos           resíduos contra os valores ajustados (caderno HTML)
# grafico_qq                 Q-Q dos resíduos (caderno HTML)
# texto_resultado            frase com o resultado do teste
# alerta_poder               ressalva quando não há evidência e o poder é baixo
# texto_levene_decisao       decisão do Levene ligada ao teste escolhido
#
# Cada QMD executa este script numa sessão nova e usa os objetos em memória.
# Os CSVs e PNGs salvos são cópias para consulta; não alimentam os QMDs.
# Edite os cálculos aqui e a argumentação científica nos documentos Quarto.
# Instale os pacotes uma única vez conforme o README, antes de executar.

# 1. Preparar o ambiente ---------------------------------------------------
library(here)
# Declara: "este arquivo está em R/analise.R, dentro do meu projeto".
# Assim, here() monta caminhos a partir da raiz do projeto, acima da pasta R/.
# Não muda a pasta de trabalho como setwd(). Abra o projeto antes de rodar.
here::i_am("R/analise.R")
{{BIBLIOTECAS_PREPARO}}
library(stringr)    # str_glue monta as frases dos relatórios
# Dois pacotes do ecossistema EAPA, hospedados no GitHub (não estão no CRAN).
# EAPADados: pacote complementar do ecossistema CatalyseR, com dados de
# contexto da pesca e da aquicultura. Este projeto não o chama diretamente
# (os dados vêm da planilha em dados/brutos/), mas ele é carregado por
# compatibilidade com o ecossistema e exigido na instalação.
if (!requireNamespace("EAPADados", quietly = TRUE)) {
  stop(
    "Este projeto faz parte do ecossistema CatalyseR e pede o pacote ",
    "complementar EAPADados para compatibilidade, mas ele não está instalado.",
    " Instale uma vez, no console: remotes::install_github('astuciasnor/EAPADados')",
    call. = FALSE
  )
}
library(EAPADados)
# catalyser: catalyser_conferir_base(), a conferência das bases na seção 3.
if (!requireNamespace("catalyser", quietly = TRUE)) {
  stop(
    "Este projeto usa o pacote catalyser, que não está instalado.",
    " Instale uma vez, no console: remotes::install_github('astuciasnor/catalyser')",
    call. = FALSE
  )
}
library(catalyser)
# As funções abaixo cuidam da apresentação; os cálculos continuam neste script.
source(here::here("R", "funcoes.R"), encoding = "UTF-8")

# 2. Definir as escolhas e ler os dados ------------------------------------
# Nomes das colunas usadas na análise; devem existir na base preparada.
variavel_resposta <- {{RESPOSTA_R}}
variavel_grupo <- {{GRUPO_R}}
# Os rótulos são textos de apresentação: alterá-los não renomeia as colunas.
rotulo_resposta <- {{ROTULO_RESPOSTA_R}}
rotulo_grupo <- {{ROTULO_GRUPO_R}}
nivel_confianca <- {{CONFIANCA}}
alfa <- 1 - nivel_confianca
# Mantemos a hipótese e a escolha Student/Welch registradas na CatalyseR.
alternativa <- {{ALTERNATIVA_R}}
variancias_iguais <- {{VARIANCIAS_IGUAIS}}
titulo_grafico <- {{TITULO_R}}
# Duas cores da paleta Ocean, uma por grupo, com bom contraste.
cores_grupo <- c("#0F3B5F", "#E89B3C")

# Estas pastas guardam produtos regeneráveis. Os dados brutos ficam intactos.
# recursive = TRUE cria as pastas intermediárias; showWarnings = FALSE evita
# o aviso de pasta já existente; dir.create() não apaga nada que esteja nelas.
for (pasta in c("dados/processados", "saida/tabelas", "saida/figuras", "saida/relatorios")) {
  dir.create(here::here(pasta), showWarnings = FALSE, recursive = TRUE)
}

# A planilha que viajou no projeto entra aqui, sem nenhuma alteração.
# Sai dados_brutos, a tabela lida.
{{TRECHO_IMPORTAR}}

# 3. Preparar a base -------------------------------------------------------
# Quatro etapas, um objeto por etapa: reconstruir o preparo, conferir com a
# fotografia que acompanha o projeto, adotar a base e montar a base da análise.
# Sai dados_da_analise, a base desta análise.
{{TRECHO_PREPARO}}
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
# Estatística e graus de liberdade do Levene, para o texto de decisão.
f_levene <- teste_levene[["F value"]][1]
gl_levene <- teste_levene[["Df"]][1:2]
# Levene orienta a avaliação, sem substituir a escolha registrada no painel.
levene_sem_evidencia <- !is.na(p_levene) && p_levene >= alfa
# A normalidade fica "ok" quando nenhum dos dois grupos dá evidência de desvio.
normalidade_ok <- all(p_shapiro >= alfa)

# 6. Aplicar o teste t -----------------------------------------------------
# Student/Welch, confiança e hipótese seguem as escolhas do pesquisador.
# A fórmula compara o primeiro nível do fator com o segundo.
teste_t <- t.test(formula_teste, data = dados, var.equal = variancias_iguais,
                  conf.level = nivel_confianca, alternative = alternativa)
# A comparação com Welch mantém a mesma hipótese e o mesmo nível de confiança.
teste_welch <- t.test(formula_teste, data = dados, var.equal = FALSE,
                      conf.level = nivel_confianca, alternative = alternativa)
# resumo_console guarda a saída bruta do teste; digite o nome no console
# para conhecê-la uma vez — os relatórios não a exibem.
resumo_console <- teste_t
# Nome do método em português, para as tabelas e o texto.
metodo_teste <- if (variancias_iguais) "t de Student (assumindo variâncias iguais)" else "t de Welch (sem assumir variâncias iguais)"
# A direção da hipótese diz respeito ao nível 1 menos o nível 2, nesta ordem.
hipotese_alternativa <- dplyr::case_when(
  alternativa == "greater" ~ paste("a média de", nivel_1, "é maior que a de", nivel_2),
  alternativa == "less" ~ paste("a média de", nivel_1, "é menor que a de", nivel_2),
  TRUE ~ paste("as médias de", nivel_1, "e", nivel_2, "são diferentes")
)
texto_escolhas <- paste0(
  "Aplicou-se o ", metodo_teste, ", conforme a escolha registrada no painel, ",
  "com confiança de ", fmt(100 * nivel_confianca, 0), "% e H1: ",
  hipotese_alternativa, ". O teste de Levene é uma verificação recomendatória; ",
  "seu resultado não troca automaticamente o método escolhido."
)

# Médias de cada grupo e a diferença entre elas (nível 1 menos nível 2).
media_1 <- tabela_descritiva$media[tabela_descritiva[[nome_col_grupo]] == nivel_1]
media_2 <- tabela_descritiva$media[tabela_descritiva[[nome_col_grupo]] == nivel_2]
diferenca_medias <- media_1 - media_2
ic_diferenca <- teste_t$conf.int
# Testes direcionais têm apenas um limite finito; o símbolo indica o lado aberto.
limites_ic_texto <- dplyr::case_when(
  is.infinite(ic_diferenca) & ic_diferenca < 0 ~ "-∞",
  is.infinite(ic_diferenca) & ic_diferenca > 0 ~ "∞",
  TRUE ~ fmt(ic_diferenca)
)
descricao_ic <- if (alternativa == "two.sided") "IC" else "IC unilateral"

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
# O intervalo de confiança do d vem do effectsize::cohens_d(), que usa o mesmo
# desvio padrão combinado do cálculo manual acima. Ter o IC ao lado da
# estimativa pontual mostra o quanto ela é precisa nesta amostra.
efeito_d <- effectsize::cohens_d(formula_teste, data = dados, ci = nivel_confianca)
d_ic <- c(efeito_d$CI_low[1], efeito_d$CI_high[1])

# 7. Preparar as tabelas de apresentação -----------------------------------
ic_percentual <- fmt(100 * nivel_confianca, 0)
# Tabela enxuta com o essencial do teste.
tabela_teste <- data.frame(
  Indicador = c(
    "Método",
    "Hipótese alternativa (H1)",
    paste0("Diferença de médias (", nivel_1, " menos ", nivel_2, ")"),
    paste0(descricao_ic, " ", ic_percentual, "% da diferença"),
    "t",
    "Graus de liberdade",
    "p-valor",
    "d de Cohen (tamanho do efeito)",
    paste0("IC bilateral ", ic_percentual, "% do d de Cohen")
  ),
  Valor = c(
    metodo_teste,
    hipotese_alternativa,
    fmt(diferenca_medias),
    paste0("[", limites_ic_texto[1], "; ", limites_ic_texto[2], "]"),
    fmt(unname(teste_t$statistic)),
    fmt(unname(teste_t$parameter)),
    formatar_p(teste_t$p.value),
    paste0(fmt(d_cohen), " (", classe_efeito, ")"),
    paste0("[", fmt(d_ic[1]), "; ", fmt(d_ic[2]), "]")
  ),
  check.names = FALSE
)
# Tabela dos pressupostos, com leitura honesta linha a linha.
leitura_shapiro_1 <- if (p_shapiro_1 >= alfa) "Sem evidência de desvio da normalidade." else "Evidência de desvio da normalidade."
leitura_shapiro_2 <- if (p_shapiro_2 >= alfa) "Sem evidência de desvio da normalidade." else "Evidência de desvio da normalidade."
leitura_levene <- if (levene_sem_evidencia) "Sem evidência de variâncias diferentes." else "Há evidência de variâncias diferentes."
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

# 8.2 Médias por grupo com as observações: cada ponto é uma observação
# (jitter), o losango é a média do grupo, o rótulo ao lado dele escreve a
# média ± DP; as hastes mostram IC bilateral da média. O DP descreve a
# dispersão das observações. As hastes mostram IC bilateral de cada média,
# na confiança escolhida; o IC da diferença testada permanece na tabela.
# Barras transparentes partem de zero e mantêm os indivíduos visíveis.
resumo_medias <- tabela_descritiva |>
  dplyr::mutate(
    ic_inf_media = media - qt((1 + nivel_confianca) / 2, n - 1) * ep,
    ic_sup_media = media + qt((1 + nivel_confianca) / 2, n - 1) * ep
  )
# As letras resumem o p do teste realmente escolhido (Student ou Welch):
# sem diferença (p >= alfa), os dois grupos recebem "a"; com diferença, o
# grupo de maior média recebe "a" e o outro, "b". Leitura: letras iguais,
# grupos sem diferença significativa; letras diferentes, médias diferentes.
# which.max localiza a maior média pelo índice, sem comparar números de
# ponto flutuante por igualdade.
resumo_medias$letra <- dplyr::case_when(
  teste_t$p.value >= alfa ~ "a",
  seq_along(resumo_medias$media) == which.max(resumo_medias$media) ~ "a",
  TRUE ~ "b"
)
# O rótulo ao lado do losango traz a média ± DP no formato das tabelas
# (vírgula decimal, mesmas casas), o DP é amostral; as hastes mostram IC, uma medida diferente.
resumo_medias$rotulo_media <- paste0(fmt(resumo_medias$media), " ± ", fmt(resumo_medias$dp))
# Alturas do texto: a letra fica acima do ponto mais alto e da haste mais
# alta; o rótulo, ao lado do losango, na altura da média. A folga é aditiva
# (6% da amplitude da figura), e não multiplicativa: com valores todos
# negativos, max * 1.06 colocaria o texto dentro dos dados.
valores_figura <- c(resumo_medias$ic_sup_media, resumo_medias$ic_inf_media, dados[[variavel_resposta]])
folga_y <- 0.06 * diff(range(valores_figura, na.rm = TRUE))
y_letra <- max(valores_figura, na.rm = TRUE) + folga_y
resumo_medias$y_rotulo <- resumo_medias$media
grafico_medias <- ggplot2::ggplot(resumo_medias,
  ggplot2::aes(x = .data[[nome_col_grupo]])) +
  ggplot2::geom_col(ggplot2::aes(y = media, fill = .data[[nome_col_grupo]]),
    width = 0.30, alpha = 0.22, show.legend = FALSE) +
  ggplot2::scale_fill_manual(values = cores_grupo) +
  ggplot2::geom_jitter(
    data = dados,
    ggplot2::aes(y = .data[[variavel_resposta]], colour = .data[[variavel_grupo]]),
    width = 0.10,
    size = 2.2,
    alpha = 0.7
  ) +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = ic_inf_media, ymax = ic_sup_media),
    width = 0.08,
    linewidth = 0.8,
    colour = "#0F3B5F"
  ) +
  ggplot2::geom_point(ggplot2::aes(y = media), shape = 18, size = 4.4, colour = "#0F3B5F") +
  # Rótulo à direita do losango, com fundo transparente e sem borda, para
  # continuar legível sobre pontos próximos.
  ggplot2::geom_label(
    ggplot2::aes(y = y_rotulo, label = rotulo_media),
    nudge_x = 0.05, hjust = 0, vjust = 0.5, fontface = "bold",
    linewidth = 0, label.padding = grid::unit(0.12, "lines"),
    fill = NA, colour = "#0F3B5F",
    size = 3.2
  ) +
  ggplot2::geom_text(
    ggplot2::aes(y = y_letra, label = letra),
    size = 5, fontface = "bold", colour = "#0F3B5F"
  ) +
  ggplot2::scale_colour_manual(values = cores_grupo, guide = "none") +
  # A folga à direita evita que o rótulo ao lado do segundo grupo seja cortado.
  ggplot2::scale_x_discrete(expand = ggplot2::expansion(add = c(0.6, 0.9))) +
  ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.15))) +
  ggplot2::labs(
    x = rotulo_grupo,
    y = rotulo_resposta,
    title = if (nzchar(titulo_grafico)) titulo_grafico else NULL,
    subtitle = paste0("Pontos: observações; losango: média; rótulo: média ± DP; hastes: IC bilateral da média.\n",
                      "Letras iguais: sem diferença significativa")
  ) +
  tema_projeto()

# 8.3 Diagnósticos dos pressupostos (o caderno HTML os apresenta; o Word não).
# O resíduo é a distância de cada observação à média do seu grupo, e o valor
# ajustado é essa média: o valor que o modelo "esperava" para cada grupo.
dados$valor_ajustado <- ave(dados[[variavel_resposta]], dados[[variavel_grupo]])
dados$residuo <- dados[[variavel_resposta]] - dados$valor_ajustado

# Resíduos contra os valores ajustados. Procura-se uma nuvem de pontos sem
# forma, espalhada por igual acima e abaixo da linha do resíduo zero, com
# alturas parecidas nas duas faixas verticais.
grafico_residuos <- ggplot2::ggplot(dados,
  ggplot2::aes(x = valor_ajustado, y = residuo, colour = .data[[variavel_grupo]])) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  # A largura do jitter segue a distância entre as médias dos grupos, com um
  # piso de 0.05: se as médias forem iguais, o espalhamento não zera.
  ggplot2::geom_jitter(width = pmax(0.02 * diff(range(dados$valor_ajustado)), 0.05),
                       height = 0, size = 2.2, alpha = 0.7) +
  ggplot2::scale_colour_manual(values = cores_grupo) +
  ggplot2::labs(x = "Valores ajustados (médias dos grupos)", y = "Resíduos",
                colour = rotulo_grupo) +
  tema_projeto()

# 8.4 Q-Q dos resíduos: os pontos devem acompanhar a reta; caudas que se
# descolam dela indicam assimetria ou valores extremos.
grafico_qq <- ggplot2::ggplot(dados, ggplot2::aes(sample = residuo)) +
  ggplot2::stat_qq(colour = "#2E7D8F", alpha = 0.7) +
  ggplot2::stat_qq_line(colour = "#E76F51") +
  ggplot2::labs(x = "Quantis teóricos", y = "Resíduos") +
  tema_projeto()

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
frase_variancia <- if (levene_sem_evidencia) {
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

# Decisão do Levene escrita com o resultado e ligada ao método aplicado.
# A leitura compara p com alfa e informa separadamente o método registrado.
texto_levene_decisao <- dplyr::case_when(
  is.na(p_levene) ~ "O teste de Levene não pôde ser calculado.",
  levene_sem_evidencia ~ stringr::str_glue(
    "O teste de Levene não rejeitou H0 ",
    "(F({gl_levene[1]}, {gl_levene[2]}) = {fmt(f_levene)}; ",
    "{formatar_p(p_levene, no_texto = TRUE)}). Como o p é maior ou igual a ",
    "alfa ({fmt(alfa, 2)}), não rejeitamos H0."
  ),
  TRUE ~ stringr::str_glue(
    "O teste de Levene rejeitou H0 ",
    "(F({gl_levene[1]}, {gl_levene[2]}) = {fmt(f_levene)}; ",
    "{formatar_p(p_levene, no_texto = TRUE)}). Como o p é menor que alfa ",
    "({fmt(alfa, 2)}), rejeitamos H0. Welch é uma opção recomendada ",
    "quando há evidência de variâncias diferentes."
  )
)
texto_levene_decisao <- paste(texto_levene_decisao,
  "Esta leitura é recomendatória; foi aplicado", metodo_teste,
  "conforme a escolha registrada no painel.")
print(texto_levene_decisao)

texto_resultado <- stringr::str_glue(
  "Pelo {metodo_teste}, {evidencia} ",
  "para H1: {hipotese_alternativa} ",
  "(t = {fmt(unname(teste_t$statistic))}; gl = {fmt(unname(teste_t$parameter))}; ",
  "{formatar_p(teste_t$p.value, no_texto = TRUE)}). ",
  "O grupo {nivel_1} teve média {fmt(media_1)} e o grupo {nivel_2}, {fmt(media_2)}; ",
  "a diferença foi de {fmt(diferenca_medias)} ",
  "({descricao_ic} {ic_percentual}% [{limites_ic_texto[1]}; {limites_ic_texto[2]}])."
)
print(texto_resultado)

# Quando Welch é escolhido, o d de Cohen continua usando o desvio padrão combinado (sp) dos
# dois grupos; a frase abaixo deixa isso explícito para o leitor. No ramo do t
# de Student (variâncias iguais) a ressalva é dispensável, pois a igualdade de
# variâncias já é o pressuposto do método.
frase_dp_combinado <- dplyr::case_when(
  # Ramo Welch: o teste dispensa a igualdade; o d ainda usa o sp combinado.
  !variancias_iguais ~ paste0(
    "Embora o teste escolhido seja Welch, que não assume variâncias iguais, o d de ",
    "Cohen usa o desvio padrão combinado dos dois grupos, que pondera a ",
    "variância de cada um pelos seus graus de liberdade. "
  ),
  # Ramo Student: nenhuma ressalva adicional.
  TRUE ~ ""
)

texto_efeito <- stringr::str_glue(
  "O tamanho do efeito foi {classe_efeito} ",
  "(d de Cohen = {fmt(d_cohen)}; IC bilateral {ic_percentual}% ",
  "[{fmt(d_ic[1])}; {fmt(d_ic[2])}]). ",
  "{frase_dp_combinado}",
  "O p informa a evidência estatística e o d quantifica a diferença padronizada. ",
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
  "({descricao_ic} {ic_percentual}% [{limites_ic_texto[1]}; {limites_ic_texto[2]}]; ",
  "{formatar_p(teste_t$p.value, no_texto = TRUE)}) e tamanho de efeito {classe_efeito} ",
  "(d = {fmt(d_cohen)}). A interpretação depende dos pressupostos e do delineamento."
)
print(texto_sintese_estatistica)

# alerta_modelo acompanha a conclusão, honesto quanto aos pressupostos.
alerta_modelo <- if (!normalidade_ok || !levene_sem_evidencia) {
  "Os testes indicaram sinais de atenção nos pressupostos; considere o t de Welch e examine os gráficos antes de concluir."
} else "Os testes não detectaram desvios nos pressupostos, mas os gráficos e o delineamento continuam necessários."
print(alerta_modelo)

# Poder do teste: a probabilidade de detectar um efeito do tamanho observado.
# pwr.t.test() supõe grupos de mesmo tamanho — usamos o n médio por grupo —
# Nas hipóteses direcionais, o sinal do d preserva nível 1 menos nível 2.
# Esse cálculo é aproximado: não reproduz a correção de Welch.
poder_teste_t <- if (!is.na(d_cohen) && abs(d_cohen) > 0) {
  pwr::pwr.t.test(n = mean(c(n_1, n_2)), d = d_cohen, sig.level = alfa,
                  type = "two.sample", alternative = alternativa)$power
} else {
  NA_real_
}
# Ressalva apenas quando ela muda a leitura: teste sem evidência E poder
# baixo. Com p < alfa o efeito já foi detectado; o poder observado, aí,
# não acrescenta informação.
alerta_poder <- if (teste_t$p.value >= alfa && !is.na(poder_teste_t) &&
                    poder_teste_t < 0.80) {
  stringr::str_glue(
    "A ausência de evidência não deve ser lida como ausência de efeito: ",
    "para o tamanho de efeito observado, o poder do teste foi de apenas ",
    "{fmt(100 * poder_teste_t, 0)}%. Este cálculo depende do efeito observado ",
    "e das premissas adotadas; confira a direção da hipótese e o delineamento. ",
    "Ele não substitui o planejamento amostral."
  )
} else {
  ""
}
print(alerta_poder)

# 10. Salvar cópias para consulta e compartilhamento ------------------------
# CSV com ponto e vírgula e vírgula decimal abre bem no Excel em português.
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
# O executável pode estar no PATH ou ser indicado por QUARTO_PATH.
quarto_bin <- Sys.getenv("QUARTO_PATH", unname(Sys.which("quarto")))
versao_quarto <- if (nzchar(quarto_bin) && file.exists(quarto_bin)) {
  paste(system2(quarto_bin, "--version", stdout = TRUE), collapse = " ")
} else "não encontrado nesta sessão"
# Incluímos dependências carregadas indiretamente, além dos pacotes da análise.
pacotes_ambiente <- sort(unique(c(loadedNamespaces(), "catalyser", "EAPADados")))
# RemoteSha só existe quando a instalação preservou o commit do GitHub.
# Sua ausência fica explícita: a versão não identifica sozinha uma revisão local.
tabela_ambiente <- do.call(rbind, lapply(pacotes_ambiente, function(pacote) {
  descricao <- utils::packageDescription(pacote)
  revisao <- descricao$RemoteSha
  if (is.null(revisao) || !nzchar(revisao)) revisao <- "não registrado"
  data.frame(Componente = pacote, Versão = descricao$Version, Revisão = revisao,
             check.names = FALSE)
}))
tabela_ambiente <- rbind(
  data.frame(Componente = c("R", "Quarto"),
    Versão = c(as.character(getRversion()), versao_quarto), Revisão = c("", "")),
  tabela_ambiente
)
# A tabela é legível no relatório; sessionInfo conserva o registro técnico completo.
registro_ambiente <- c(paste("Quarto:", versao_quarto), capture.output(sessionInfo()),
  "", apply(tabela_ambiente, 1, paste, collapse = " | "))
writeLines(registro_ambiente, here::here("saida", "sessionInfo.txt"), useBytes = TRUE)
