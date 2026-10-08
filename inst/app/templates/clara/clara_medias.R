# =============================================================================
#  ClaRa: comparar médias
# =============================================================================
#
#  Tudo da pergunta "a média da resposta difere entre os grupos?": a análise
#  (comparar_medias()), os três gráficos dela, as receitas que as funções
#  para qualquer análise usam com as médias e o relatório no console.
#
#  Carregado por R/clara.R; no roteiro, basta source("R/clara.R").
# =============================================================================


# Comparar médias --------------------------------------------------------------
#
# Pergunta: a média da resposta difere entre os grupos?
# comparar_medias() faz a análise; três gráficos a mostram, cada um com o
# seu nome: grafico_medias() (a figura principal), grafico_boxplot() (a
# exploração) e grafico_pares() (a diferença de cada par).
# A ClaRa escolhe o teste: teste t (de Welch ou de Student) com dois grupos,
# ANOVA (clássica ou de Welch) com três ou mais.

# comparar_medias() ------------------------------------------------------------
#
# Pergunta: a média da resposta difere entre os grupos?
#
# A ClaRa escolhe o teste pelo número de grupos com dados:
#   dois grupos ........ teste t: de Welch (padrão, não supõe variâncias
#                        iguais) ou de Student (com variancias_iguais = TRUE);
#   três ou mais ....... ANOVA de um fator: clássica, com Tukey (padrão),
#                        ou de Welch, com Games-Howell (com
#                        variancias_iguais = FALSE).
# Em qualquer caso, faz a análise na ordem em que a estudamos: resumo dos
# grupos, teste, pressupostos e letras.
#
# Argumentos:
#   dados ............... a tabela, com uma linha por observação
#   resposta ............ a coluna numérica medida (peso, comprimento...)
#   grupos .............. a coluna que diz o grupo de cada observação
#   rotulo_resposta ..... como chamar a resposta nos gráficos e nos textos
#                         (ex.: "Peso (g)"); sem ele, vai o nome da coluna.
#   rotulo_grupos ....... como chamar os grupos nos gráficos e nos textos
#                         (ex.: "Tipo de Ração"); sem ele, vai o nome da coluna.
#                         Os rótulos ficam guardados no resultado: as funções
#                         que vêm depois já os usam, sem precisar repeti-los
#   confianca ........... nível de confiança dos intervalos (padrão 0.95);
#                         a significância dos testes é 1 - confianca
#   variancias_iguais ... TRUE supõe variâncias iguais nos grupos; FALSE não
#                         supõe. Sem ele, vale o padrão de cada teste: com
#                         dois grupos, FALSE (teste t de Welch; TRUE dá o
#                         de Student); com três ou mais, TRUE (ANOVA
#                         clássica com Tukey; FALSE dá a ANOVA de Welch com
#                         Games-Howell). Escreva a escolha no roteiro: ela
#                         pode mudar a conclusão e assim fica registrada
#   mostrar_codigo ...... TRUE imprime o código R antes do resultado
#
# Devolve uma lista. Cada parte se abre com $:
#   resultado$resumo        n, média, DP, EP, IC e letra de cada grupo
#   resultado$pressupostos  testes dos pressupostos, com uma leitura de cada um
#   resultado$amostra       observações no total, usadas e excluídas
#   resultado$dados         as observações usadas na análise
# Com três ou mais grupos (ANOVA):
#   resultado$anova         a tabela da ANOVA (na de Welch, sem SQ e QM)
#   resultado$pares         todas as comparações de Tukey (ou de Games-Howell)
#   resultado$modelo        o modelo aov(), só na ANOVA clássica
# Com dois grupos (teste t):
#   resultado$teste         diferença das médias, IC, t, gl e p
#
# Exemplo:
#   resultado <- plantas |>
#     comparar_medias(resposta        = peso,
#                     grupos          = tratamento,
#                     rotulo_resposta = "Peso seco (g)",
#                     rotulo_grupos   = "Tratamento")
#
#   resultado |>
#     grafico_medias()          # os rótulos já vêm do resultado
#
comparar_medias <- function(dados,
                            resposta,
                            grupos,
                            rotulo_resposta   = NULL,
                            rotulo_grupos     = NULL,
                            confianca         = 0.95,
                            variancias_iguais = NULL,
                            mostrar_codigo    = FALSE) {

  # Guardamos o nome da tabela que o aluno usou (plantas, por exemplo).
  nome_dados <- nome_do_objeto(substitute(dados), padrao = "dados")

  # Guardamos os nomes das duas colunas, como o aluno as escreveu.
  coluna_resposta <- rlang::as_name(rlang::ensym(resposta))
  coluna_grupos   <- rlang::as_name(rlang::ensym(grupos))

  # Antes de calcular, conferimos se as colunas existem e servem para comparar.
  conferir_colunas(dados, coluna_resposta, coluna_grupos)
  conferir_rotulo(rotulo_resposta, "rotulo_resposta")
  conferir_rotulo(rotulo_grupos, "rotulo_grupos")

  # Contamos os grupos que têm resposta: dois pedem o teste t; mais, a ANOVA.
  quantos_grupos <- dplyr::n_distinct(
    dados[[coluna_grupos]][!is.na(dados[[coluna_resposta]])],
    na.rm = TRUE
  )

  # Sem escolha do aluno, vale o padrão de cada teste: Welch no teste t,
  # ANOVA clássica com três ou mais grupos.
  variancias_iguais <- ou_entao(variancias_iguais, quantos_grupos > 2)
  conferir_sim_ou_nao(variancias_iguais, "variancias_iguais")

  # Com três ou mais grupos, as variâncias decidem entre a ANOVA clássica e
  # a de Welch.
  analise <- dplyr::case_when(quantos_grupos == 2 ~ "teste_t",
                              variancias_iguais   ~ "anova",
                              .default            = "anova_welch")

  # No teste t, as variâncias decidem entre Welch e Student.
  variante <- dplyr::case_when(variancias_iguais ~ "student",
                               .default          = "welch")

  # Trocamos os marcadores da receita escolhida pelos nomes desta análise.
  # Os trechos da variante vêm primeiro, porque também têm marcadores.
  receita <- preencher_receita(receitas_medias[[analise]], c(
    variantes_teste_t[[variante]],
    list(
      DADOS             = nome_dados,
      RESPOSTA          = coluna_resposta,
      GRUPOS            = coluna_grupos,
      CONFIANCA         = confianca,
      ALFA              = 1 - confianca,
      QUANTIL           = 1 - (1 - confianca) / 2,
      VARIANCIAS_IGUAIS = variancias_iguais
    )
  ))

  # Rodamos a receita, mostrando o código antes, se o aluno pediu.
  resultado <- executar_receita(
    receita        = receita,
    objetos        = rlang::set_names(list(dados), nome_dados),
    pacotes        = pacotes_medias[[analise]],
    mostrar_codigo = mostrar_codigo
  )

  # Anotamos no resultado o que foi analisado; as outras funções vão precisar.
  # Os rótulos também ficam: gráficos e textos os usam sem o aluno repeti-los.
  resultado$nomes <- list(
    dados             = nome_dados,
    resposta          = coluna_resposta,
    grupos            = coluna_grupos,
    rotulo_resposta   = ou_entao(rotulo_resposta, coluna_resposta),
    rotulo_grupos     = ou_entao(rotulo_grupos, coluna_grupos),
    confianca         = confianca,
    analise           = analise,
    variancias_iguais = variancias_iguais
  )

  # Damos ao resultado uma etiqueta, para o console saber como exibi-lo.
  structure(resultado, class = "clara_medias")
}


# As receitas de comparar_medias() ---------------------------------------------
#
# Uma receita para cada caso. comparar_medias() escolhe uma e a preenche.

receitas_medias <- list()

# Três ou mais grupos: ANOVA de um fator, pressupostos, Tukey e letras.
receitas_medias$anova <- "
# 1. As observações da análise: só as linhas com resposta e grupo.
#    O droplevels() tira da lista os grupos que ficaram sem dados.
dados_analise <- <<DADOS>> |>
  filter(!is.na(<<RESPOSTA>>), !is.na(<<GRUPOS>>)) |>
  mutate(<<GRUPOS>> = factor(<<GRUPOS>>)) |>
  droplevels()

# Quantas observações havia, quantas entraram e quantas ficaram de fora.
amostra <- tibble(
  total     = nrow(<<DADOS>>),
  usadas    = nrow(dados_analise),
  excluidas = total - usadas
)

# 2. Resumo de cada grupo: tamanho, centro e dispersão da resposta.
resumo <- dados_analise |>
  group_by(<<GRUPOS>>) |>
  summarise(
    n      = n(),
    media  = mean(<<RESPOSTA>>),
    dp     = sd(<<RESPOSTA>>),
    ep     = dp / sqrt(n),
    ic_inf = media - qt(<<QUANTIL>>, df = n - 1) * ep,
    ic_sup = media + qt(<<QUANTIL>>, df = n - 1) * ep
  )

# 3. ANOVA de um fator: <<RESPOSTA>> explicado por <<GRUPOS>>.
modelo <- aov(<<RESPOSTA>> ~ <<GRUPOS>>, data = dados_analise)

anova <- modelo |>
  tidy() |>
  transmute(
    fonte = if_else(term == \"Residuals\", \"Resíduos\", term),
    gl    = df,
    sq    = sumsq,
    qm    = meansq,
    f     = statistic,
    p     = p.value
  )

# 4. Pressupostos: normalidade dos resíduos e igualdade das variâncias.
normalidade <- shapiro.test(residuals(modelo)) |> tidy()
variancias  <- leveneTest(<<RESPOSTA>> ~ <<GRUPOS>>, data = dados_analise) |> tidy()

pressupostos <- tibble(
  pressuposto = c(\"Normalidade dos resíduos\", \"Homogeneidade das variâncias\"),
  teste       = c(\"Shapiro-Wilk\", \"Levene\"),
  estatistica = c(normalidade$statistic, variancias$statistic),
  p           = c(normalidade$p.value, variancias$p.value)
) |>
  mutate(leitura = case_when(
    p >= <<ALFA>> ~ \"sem evidência contra o pressuposto\",
    p <  <<ALFA>> ~ \"pressuposto rejeitado: interpretar com cautela\"
  ))

# 5. Tukey: todos os pares de grupos, com p ajustado.
tukey <- TukeyHSD(modelo, conf.level = <<CONFIANCA>>)

pares <- tukey |>
  tidy() |>
  transmute(
    comparacao = contrast,
    diferenca  = estimate,
    ic_inf     = conf.low,
    ic_sup     = conf.high,
    p_ajustado = adj.p.value
  )

# 6. Letras: grupos com a mesma letra não diferiram. A maior média leva \"a\".
letras <- multcompLetters4(modelo, tukey, threshold = <<ALFA>>)$<<GRUPOS>>$Letters

resumo <- resumo |>
  mutate(letra = letras[as.character(<<GRUPOS>>)])

# 7. Tudo junto, numa lista com partes nomeadas.
list(
  resumo       = resumo,
  anova        = anova,
  pressupostos = pressupostos,
  pares        = pares,
  modelo       = modelo,
  amostra      = amostra,
  dados        = dados_analise
)"

# Três ou mais grupos, sem supor variâncias iguais: ANOVA de Welch,
# normalidade em cada grupo, Games-Howell e letras.
receitas_medias$anova_welch <- "
# 1. As observações da análise: só as linhas com resposta e grupo.
#    O droplevels() tira da lista os grupos que ficaram sem dados.
dados_analise <- <<DADOS>> |>
  filter(!is.na(<<RESPOSTA>>), !is.na(<<GRUPOS>>)) |>
  mutate(<<GRUPOS>> = factor(<<GRUPOS>>)) |>
  droplevels()

# Quantas observações havia, quantas entraram e quantas ficaram de fora.
amostra <- tibble(
  total     = nrow(<<DADOS>>),
  usadas    = nrow(dados_analise),
  excluidas = total - usadas
)

# 2. Resumo de cada grupo: tamanho, centro e dispersão da resposta.
resumo <- dados_analise |>
  group_by(<<GRUPOS>>) |>
  summarise(
    n      = n(),
    media  = mean(<<RESPOSTA>>),
    dp     = sd(<<RESPOSTA>>),
    ep     = dp / sqrt(n),
    ic_inf = media - qt(<<QUANTIL>>, df = n - 1) * ep,
    ic_sup = media + qt(<<QUANTIL>>, df = n - 1) * ep
  )

# 3. ANOVA de Welch: compara as médias sem supor variâncias iguais. Cada
#    grupo pesa conforme a sua variância, e os graus de liberdade do
#    denominador são corrigidos (por isso têm casas decimais). Não há soma
#    de quadrados nem quadrado médio, como na ANOVA clássica.
welch <- oneway.test(<<RESPOSTA>> ~ <<GRUPOS>>,
                     data      = dados_analise,
                     var.equal = FALSE)

anova <- tibble(
  fonte = c(\"<<GRUPOS>>\", \"Resíduos\"),
  gl    = unname(welch$parameter),
  f     = c(unname(welch$statistic), NA),
  p     = c(welch$p.value, NA)
)

# 4. Pressuposto: normalidade da resposta dentro de cada grupo.
#    A ANOVA de Welch não exige variâncias iguais; por isso não há Levene.
pressupostos <- dados_analise |>
  group_by(<<GRUPOS>>) |>
  reframe(tidy(shapiro.test(<<RESPOSTA>>))) |>
  transmute(
    pressuposto = paste(\"Normalidade no grupo\", <<GRUPOS>>),
    teste       = \"Shapiro-Wilk\",
    estatistica = statistic,
    p           = p.value
  ) |>
  mutate(leitura = case_when(
    p >= <<ALFA>> ~ \"sem evidência contra o pressuposto\",
    p <  <<ALFA>> ~ \"pressuposto rejeitado: interpretar com cautela\"
  ))

# 5. Games-Howell: cada par de grupos, como no Tukey, mas com o erro e os
#    graus de liberdade do próprio par, porque as variâncias diferem. A
#    amplitude studentizada (qtukey, ptukey) já ajusta os p para as várias
#    comparações. Em \"B-A\", a média de B menos a de A, na ordem do Tukey.
k <- nrow(resumo)

pares <- cross_join(resumo, resumo, suffix = c(\"_1\", \"_2\")) |>
  filter(as.integer(<<GRUPOS>>_1) > as.integer(<<GRUPOS>>_2)) |>
  arrange(<<GRUPOS>>_2, <<GRUPOS>>_1) |>
  mutate(
    var_1  = dp_1^2 / n_1,
    var_2  = dp_2^2 / n_2,
    erro   = sqrt(var_1 + var_2),
    gl     = (var_1 + var_2)^2 / (var_1^2 / (n_1 - 1) + var_2^2 / (n_2 - 1)),
    margem = qtukey(<<CONFIANCA>>, nmeans = k, df = gl) * erro / sqrt(2)
  ) |>
  transmute(
    comparacao = paste(<<GRUPOS>>_1, <<GRUPOS>>_2, sep = \"-\"),
    diferenca  = media_1 - media_2,
    ic_inf     = diferenca - margem,
    ic_sup     = diferenca + margem,
    p_ajustado = ptukey(abs(diferenca) / erro * sqrt(2), nmeans = k, df = gl,
                        lower.tail = FALSE)
  )

# 6. Letras: grupos com a mesma letra não diferiram. A maior média leva \"a\".
matriz_p <- vec2mat(setNames(pares$p_ajustado, pares$comparacao))
ordem    <- resumo |> arrange(desc(media)) |> pull(<<GRUPOS>>) |> as.character()
letras   <- multcompLetters(matriz_p[ordem, ordem], threshold = <<ALFA>>)$Letters

resumo <- resumo |>
  mutate(letra = letras[as.character(<<GRUPOS>>)])

# 7. Tudo junto, numa lista com partes nomeadas.
list(
  resumo       = resumo,
  anova        = anova,
  pressupostos = pressupostos,
  pares        = pares,
  amostra      = amostra,
  dados        = dados_analise
)"

# Dois grupos: teste t de Welch, normalidade em cada grupo e letras.
receitas_medias$teste_t <- "
# 1. As observações da análise: só as linhas com resposta e grupo.
#    O droplevels() tira da lista os grupos que ficaram sem dados.
dados_analise <- <<DADOS>> |>
  filter(!is.na(<<RESPOSTA>>), !is.na(<<GRUPOS>>)) |>
  mutate(<<GRUPOS>> = factor(<<GRUPOS>>)) |>
  droplevels()

# Quantas observações havia, quantas entraram e quantas ficaram de fora.
amostra <- tibble(
  total     = nrow(<<DADOS>>),
  usadas    = nrow(dados_analise),
  excluidas = total - usadas
)

# 2. Resumo de cada grupo: tamanho, centro e dispersão da resposta.
resumo <- dados_analise |>
  group_by(<<GRUPOS>>) |>
  summarise(
    n      = n(),
    media  = mean(<<RESPOSTA>>),
    dp     = sd(<<RESPOSTA>>),
    ep     = dp / sqrt(n),
    ic_inf = media - qt(<<QUANTIL>>, df = n - 1) * ep,
    ic_sup = media + qt(<<QUANTIL>>, df = n - 1) * ep
  )

# 3. <<TITULO_T>>
#    A diferença é a média do primeiro grupo menos a do segundo.
teste_t <- t.test(<<RESPOSTA>> ~ <<GRUPOS>>,
                  data       = dados_analise,
                  var.equal  = <<VARIANCIAS_IGUAIS>>,
                  conf.level = <<CONFIANCA>>)

teste <- teste_t |>
  tidy() |>
  transmute(
    comparacao = paste(levels(dados_analise$<<GRUPOS>>), collapse = \"-\"),
    diferenca  = estimate,
    ic_inf     = conf.low,
    ic_sup     = conf.high,
    t          = statistic,
    gl         = parameter,
    p          = p.value
  )

<<PRESSUPOSTOS_T>>

# 5. Letras, como no Tukey: grupos com a mesma letra não diferiram.
#    Havendo diferença, a maior média leva \"a\".
resumo <- resumo |>
  mutate(letra = case_when(
    teste$p >= <<ALFA>>     ~ \"a\",
    media == max(media) ~ \"a\",
    .default            = \"b\"
  ))

# 6. Tudo junto, numa lista com partes nomeadas.
list(
  resumo       = resumo,
  teste        = teste,
  pressupostos = pressupostos,
  amostra      = amostra,
  dados        = dados_analise
)"

# O que muda na receita do teste t entre Welch e Student: o título do
# passo 3 e os pressupostos (o Student supõe variâncias iguais e confere
# isso com o Levene; o Welch não precisa).
variantes_teste_t <- list()

variantes_teste_t$welch <- list(
  TITULO_T = "Teste t de Welch: compara as duas médias sem supor variâncias iguais.",
  PRESSUPOSTOS_T = "
# 4. Pressuposto: normalidade da resposta dentro de cada grupo.
#    O teste de Welch não exige variâncias iguais; por isso não há Levene.
pressupostos <- dados_analise |>
  group_by(<<GRUPOS>>) |>
  reframe(tidy(shapiro.test(<<RESPOSTA>>))) |>
  transmute(
    pressuposto = paste(\"Normalidade no grupo\", <<GRUPOS>>),
    teste       = \"Shapiro-Wilk\",
    estatistica = statistic,
    p           = p.value
  ) |>
  mutate(leitura = case_when(
    p >= <<ALFA>> ~ \"sem evidência contra o pressuposto\",
    p <  <<ALFA>> ~ \"pressuposto rejeitado: interpretar com cautela\"
  ))"
)

variantes_teste_t$student <- list(
  TITULO_T = "Teste t de Student: compara as duas médias supondo variâncias iguais.",
  PRESSUPOSTOS_T = "
# 4. Pressupostos: normalidade em cada grupo e igualdade das variâncias,
#    que o teste de Student supõe.
normalidade <- dados_analise |>
  group_by(<<GRUPOS>>) |>
  reframe(tidy(shapiro.test(<<RESPOSTA>>))) |>
  transmute(
    pressuposto = paste(\"Normalidade no grupo\", <<GRUPOS>>),
    teste       = \"Shapiro-Wilk\",
    estatistica = statistic,
    p           = p.value
  )

variancias <- leveneTest(<<RESPOSTA>> ~ <<GRUPOS>>, data = dados_analise) |>
  tidy() |>
  transmute(
    pressuposto = \"Homogeneidade das variâncias\",
    teste       = \"Levene\",
    estatistica = statistic,
    p           = p.value
  )

pressupostos <- bind_rows(normalidade, variancias) |>
  mutate(leitura = case_when(
    p >= <<ALFA>> ~ \"sem evidência contra o pressuposto\",
    p <  <<ALFA>> ~ \"pressuposto rejeitado: interpretar com cautela\"
  ))"
)

# Os pacotes que cada receita usa.
pacotes_medias <- list(
  anova       = c("dplyr", "broom", "car", "multcompView"),
  anova_welch = c("dplyr", "broom", "multcompView"),
  teste_t     = c("dplyr", "broom", "car")
)


# grafico_medias() -------------------------------------------------------------
#
# Pergunta: como mostrar a comparação numa só figura?
#
# A figura principal, a que vai para o relatório, o artigo ou a tese. Por
# padrão: a barra clara vai do zero até a média; cada ponto colorido é uma
# observação; o losango é a média do grupo, com "média ± DP" ao lado; a
# haste é o intervalo de confiança da média; as letras, todas na mesma
# altura, vêm do Tukey (do Games-Howell, na ANOVA de Welch; do teste t, com
# dois grupos). Cada parte pode ser
# ligada ou desligada, e a receita mostrada traz só as partes pedidas.
#
# Argumentos:
#   resultado ........... o que comparar_medias() devolveu
#   rotulo_resposta ..... texto do eixo y; sem ele, vai o de comparar_medias()
#   rotulo_grupos ....... texto do eixo x; sem ele, vai o de comparar_medias()
#   titulo .............. título da figura (padrão: sem título)
#   explicacao .......... TRUE (padrão) escreve no topo o que cada elemento
#                         mostra; FALSE tira, para a explicação ir para a
#                         legenda do artigo
#   haste ............... o que a haste mostra: "ic" (padrão, intervalo de
#                         confiança da média), "ep" (média ± erro padrão) ou
#                         "dp" (média ± desvio padrão). No RStudio, digite
#                         haste = e aperte Tab para ver as opções
#   mostrar_barras ...... TRUE (padrão) desenha a barra clara até a média
#   largura_barras ...... largura das barras, de 0 a 1 (padrão 0.3; 1 encosta
#                         uma barra na outra); o rótulo em pé acompanha
#   mostrar_pontos ...... TRUE (padrão) desenha cada observação
#   mostrar_media_dp .... TRUE (padrão) escreve "média ± DP" ao lado do losango
#   casas ............... casas decimais do rótulo média ± DP (padrão 2)
#   angulo_rotulo ....... ângulo do rótulo média ± DP, em graus. Sem ele: 0
#                         (deitado, ao lado do losango) até 6 grupos e -90
#                         com mais: em pé, dentro da barra, no meio da
#                         altura dela, lido de cima para baixo, sem invadir
#                         a coluna vizinha (90 lê de baixo para cima). Com
#                         mais de 8 grupos, a função avisa e sugere mostrar
#                         as médias numa tabela.
#   altura_rotulo ....... altura do rótulo em pé dentro da coluna, como
#                         fração da altura dela: 0.5 (padrão) no meio, 0.2
#                         perto da base, 0.8 perto do topo (com haste
#                         longa, valores altos encostam nela)
#                         Sem barras, fica deitado: em pé, sairia cortado
#                         embaixo. Com muitos grupos e sem barras, prefira
#                         mostrar_media_dp = FALSE
#   mostrar_letras ...... TRUE (padrão) escreve as letras das comparações
#   cores ............... "ocean" (padrão), "cinza" (para impressão em preto e
#                         branco) ou um vetor de cores, como
#                         c("darkgreen", "orange"); Tab mostra as paletas
#   tamanho_texto ....... tamanho base do texto (padrão 12); o rótulo, as
#                         letras e a explicação acompanham
#   fonte ............... "sans" (padrão; Arial no Windows) ou "serif"
#                         (Times New Roman no Windows), para todos os textos
#                         da figura, inclusive o rótulo e as letras
#   mostrar_codigo ...... TRUE imprime o código R antes do gráfico
#
# Devolve um gráfico do ggplot2. Por isso dá para continuar com +:
#   resultado |>
#     grafico_medias() +
#     theme(axis.text = element_text(size = 14))
#
# Exemplo:
#   resultado |>
#     grafico_medias(rotulo_resposta = "Peso seco (g)",
#                    rotulo_grupos   = "Tratamento")
#   resultado |>
#     grafico_medias(rotulo_resposta = "Peso seco (g)",
#                    explicacao      = FALSE,
#                    haste           = "ep",
#                    cores           = "cinza")
#
grafico_medias <- function(resultado,
                           rotulo_resposta  = NULL,
                           rotulo_grupos    = NULL,
                           titulo           = NULL,
                           explicacao       = TRUE,
                           haste            = c("ic", "ep", "dp"),
                           mostrar_barras   = TRUE,
                           largura_barras   = 0.3,
                           mostrar_pontos   = TRUE,
                           mostrar_media_dp = TRUE,
                           casas            = 2,
                           angulo_rotulo    = NULL,
                           altura_rotulo    = 0.5,
                           mostrar_letras   = TRUE,
                           cores            = c("ocean", "cinza"),
                           tamanho_texto    = 12,
                           fonte            = c("sans", "serif"),
                           mostrar_codigo   = FALSE) {

  # Guardamos o nome do resultado, como o aluno o chamou.
  nome_resultado <- nome_do_objeto(substitute(resultado), padrao = "resultado")

  # Este gráfico desenha o resultado de comparar_medias().
  conferir_resultado(resultado, "clara_medias",
                     "grafico_medias() desenha o resultado de comparar_medias(). Para as medianas, use grafico_medianas().")

  # Conferimos as escolhas antes de desenhar. Nas opções listadas no padrão
  # (que o Tab do RStudio mostra), sem escolha vale a primeira.
  haste <- escolher_opcao(haste, c("ic", "ep", "dp"), "haste")
  fonte <- escolher_opcao(fonte, c("sans", "serif"), "fonte")
  if (identical(cores, c("ocean", "cinza"))) cores <- "ocean"
  conferir_fracao(largura_barras, "largura_barras")
  conferir_fracao(altura_rotulo,  "altura_rotulo")
  conferir_sim_ou_nao(explicacao,       "explicacao")
  conferir_sim_ou_nao(mostrar_barras,   "mostrar_barras")
  conferir_sim_ou_nao(mostrar_pontos,   "mostrar_pontos")
  conferir_sim_ou_nao(mostrar_media_dp, "mostrar_media_dp")
  conferir_sim_ou_nao(mostrar_letras,   "mostrar_letras")
  vetor_cores <- escolher_cores(cores)

  # Sem rótulo informado aqui, valem os que a análise guardou.
  rotulo_resposta <- ou_entao(rotulo_resposta, resultado$nomes$rotulo_resposta)
  rotulo_grupos   <- ou_entao(rotulo_grupos,   resultado$nomes$rotulo_grupos)

  # Com mais de 8 grupos, a figura fica carregada: avisamos e sugerimos tabela.
  quantos_grupos <- nrow(resultado$resumo)
  if (quantos_grupos > 8) {
    warning("Com ", quantos_grupos, " grupos, a figura fica carregada e os rótulos ",
            "ficam pequenos e apertados. Considere mostrar as médias numa tabela ",
            "(resultado$resumo) e usar mostrar_media_dp = FALSE na figura.",
            call. = FALSE)
  }

  # Com muitos grupos, o rótulo fica em pé, para não invadir a coluna vizinha.
  # Deitado, fica ao lado do losango. Em pé, fica dentro da barra, no meio da
  # altura dela, lido de cima para baixo (-90 graus); sem barras, fica ao
  # lado do losango, na altura da média.
  angulo_rotulo <- ou_entao(angulo_rotulo,
                            dplyr::case_when(mostrar_barras & quantos_grupos > 6 ~ -90,
                                             .default = 0))
  deitado      <- angulo_rotulo == 0
  dentro_barra <- !deitado & mostrar_barras

  # Os limites da haste e o nome dela na explicação.
  limites <- list(
    ic = c("ic_inf",     "ic_sup",     paste0("IC ", resultado$nomes$confianca * 100, "% da média")),
    ep = c("media - ep", "media + ep", "média ± EP"),
    dp = c("media - dp", "media + dp", "média ± DP")
  )[[haste]]

  # A explicação do topo cita só as partes que aparecem na figura.
  partes <- c(if (mostrar_pontos) "pontos: observações",
              "losango: média",
              if (mostrar_media_dp) "rótulo: média ± DP",
              paste("hastes:", limites[3]))
  texto_explicacao <- paste0(paste(partes, collapse = "; "), ".",
                             if (mostrar_letras) "\\nLetras iguais: sem diferença significativa.")
  texto_explicacao <- paste0(toupper(substr(texto_explicacao, 1, 1)),
                             substring(texto_explicacao, 2))

  # O preparo: só os objetos que as camadas pedidas vão usar.
  t <- trechos_grafico_medias
  preparo <- c(
    if (mostrar_barras || mostrar_pontos) trecho_de_cores(vetor_cores),
    if (mostrar_pontos) t$pontos,
    t$medias,
    if (mostrar_media_dp) t$media_dp,
    if (mostrar_letras && mostrar_pontos) t$altura_com_pontos,
    if (mostrar_letras && !mostrar_pontos) t$altura_sem_pontos
  )

  # Os textos da figura: título, explicação e eixos.
  linhas_labs <- c(
    if (!is.null(titulo)) "    title    = \"<<TITULO>>\",",
    if (explicacao) "    subtitle = \"<<EXPLICACAO>>\",",
    "    x        = \"<<ROTULO_GRUPOS>>\",",
    "    y        = \"<<ROTULO_RESPOSTA>>\""
  )

  # As camadas pedidas, na ordem em que se empilham, ligadas por +.
  camadas <- c(
    t$base,
    if (mostrar_barras) t$barras,
    if (mostrar_pontos) t$pontos_camada,
    t$haste,
    t$media,
    if (mostrar_media_dp) t$rotulo,
    if (mostrar_letras) t$letras,
    if (mostrar_pontos) t$escala_cor,
    if (mostrar_barras) t$escala_preenchimento,
    t$escala_x,
    t$escala_y,
    paste(c("  labs(", linhas_labs, "  )"), collapse = "\n"),
    t$tema
  )
  receita <- paste(c(trimws(preparo),
                     paste0("# A figura, camada por camada.\n",
                            paste(camadas, collapse = " +\n"))),
                   collapse = "\n\n")

  # Trocamos os marcadores pelos nomes e pelas escolhas desta figura.
  receita <- preencher_receita(receita, list(
    RESULTADO          = nome_resultado,
    RESPOSTA           = resultado$nomes$resposta,
    GRUPOS             = resultado$nomes$grupos,
    ROTULO_RESPOSTA    = rotulo_resposta,
    ROTULO_GRUPOS      = rotulo_grupos,
    TITULO             = ou_entao(titulo, ""),
    EXPLICACAO         = texto_explicacao,
    NOME_HASTE         = limites[3],
    HASTE_INF          = limites[1],
    HASTE_SUP          = limites[2],
    CASAS              = casas,
    ANGULO             = angulo_rotulo,
    Y_ROTULO           = dplyr::case_when(dentro_barra ~ paste("media *", altura_rotulo),
                                          .default     = "media"),
    LARGURA            = largura_barras,
    NUDGE_ROTULO       = dplyr::case_when(deitado      ~ 0.06,
                                          dentro_barra ~ 0,
                                          .default     = 0.2),
    HJUST              = dplyr::case_when(deitado ~ 0, .default = 0.5),
    TAMANHO            = tamanho_texto,
    FAMILIA_TEXTO      = dplyr::case_when(fonte == "serif" ~ ", family = \"serif\"",
                                          .default         = ""),
    FAMILIA_TEMA       = dplyr::case_when(fonte == "serif" ~ ", base_family = \"serif\"",
                                          .default         = ""),
    TAMANHO_ROTULO     = round(3.6 * tamanho_texto / 12, 1),
    TAMANHO_LETRAS     = round(6 * tamanho_texto / 12, 1),
    TAMANHO_EXPLICACAO = round(10 * tamanho_texto / 12, 1),
    FOLGA_BAIXO        = dplyr::case_when(mostrar_barras ~ 0, .default = 0.05)
  ))

  # Rodamos a receita, mostrando o código antes, se o aluno pediu.
  executar_receita(
    receita        = receita,
    objetos        = rlang::set_names(list(resultado), nome_resultado),
    pacotes        = c("dplyr", "ggplot2"),
    mostrar_codigo = mostrar_codigo
  )
}

# Os trechos da receita de grafico_medias(). A função escolhe os que a
# figura pedida usa e os junta, na ordem, numa receita só.
trechos_grafico_medias <- list()

# Preparo: as observações de cada grupo.
trechos_grafico_medias$pontos <- "
# As observações de cada grupo.
pontos <- <<RESULTADO>>$dados"

# Preparo: as médias, com os limites da haste.
trechos_grafico_medias$medias <- "
# As médias de cada grupo, com os limites da haste (<<NOME_HASTE>>).
medias <- <<RESULTADO>>$resumo |>
  mutate(
    haste_inf = <<HASTE_INF>>,
    haste_sup = <<HASTE_SUP>>
  )"

# Preparo: o rótulo média ± DP.
trechos_grafico_medias$media_dp <- "
# O rótulo \"média ± DP\" de cada grupo, com vírgula decimal.
medias <- medias |>
  mutate(
    media_dp = paste(
      formatC(media, format = \"f\", digits = <<CASAS>>, decimal.mark = \",\"),
      \"±\",
      formatC(dp, format = \"f\", digits = <<CASAS>>, decimal.mark = \",\")
    )
  )"

# Preparo: a altura das letras, com e sem os pontos na figura.
trechos_grafico_medias$altura_com_pontos <- "
# Todas as letras na mesma altura, acima do ponto ou da haste mais alta.
altura_letras <- max(pontos$<<RESPOSTA>>, medias$haste_sup)"

trechos_grafico_medias$altura_sem_pontos <- "
# Todas as letras na mesma altura, acima da haste mais alta.
altura_letras <- max(medias$haste_sup)"

# Camadas da figura, uma por trecho.
trechos_grafico_medias$base <- "ggplot(medias, aes(x = <<GRUPOS>>))"

trechos_grafico_medias$barras <- "  geom_col(
    aes(y = media, fill = <<GRUPOS>>),
    width = <<LARGURA>>, alpha = 0.25
  )"

trechos_grafico_medias$pontos_camada <- "  geom_point(
    data     = pontos,
    aes(y = <<RESPOSTA>>, colour = <<GRUPOS>>),
    position = position_jitter(width = 0.08, height = 0, seed = 1),
    size     = 2.2,
    alpha    = 0.7
  )"

trechos_grafico_medias$haste <- "  geom_errorbar(
    aes(ymin = haste_inf, ymax = haste_sup),
    width = 0.08, linewidth = 0.8, colour = \"#0F3B5F\"
  )"

trechos_grafico_medias$media <- "  geom_point(aes(y = media), shape = 18, size = 4.4, colour = \"#0F3B5F\")"

trechos_grafico_medias$rotulo <- "  geom_text(
    aes(y = <<Y_ROTULO>>, label = media_dp),
    nudge_x = <<NUDGE_ROTULO>>, angle = <<ANGULO>>, hjust = <<HJUST>>, vjust = 0.5,
    size = <<TAMANHO_ROTULO>>, fontface = \"bold\", colour = \"#0F3B5F\"<<FAMILIA_TEXTO>>
  )"

trechos_grafico_medias$letras <- "  geom_text(
    aes(label = letra),
    y = altura_letras, vjust = -0.6,
    size = <<TAMANHO_LETRAS>>, fontface = \"bold\", colour = \"#0F3B5F\"<<FAMILIA_TEXTO>>
  )"

trechos_grafico_medias$escala_cor <- "  scale_colour_manual(values = cores, guide = \"none\")"

trechos_grafico_medias$escala_preenchimento <- "  scale_fill_manual(values = cores, guide = \"none\")"

trechos_grafico_medias$escala_x <- "  scale_x_discrete(expand = expansion(add = c(0.6, 0.9)))"

trechos_grafico_medias$escala_y <- "  scale_y_continuous(expand = expansion(mult = c(<<FOLGA_BAIXO>>, 0.15)))"

trechos_grafico_medias$tema <- "  theme_classic(base_size = <<TAMANHO>><<FAMILIA_TEMA>>) +
  theme(
    plot.title          = element_text(colour = \"#0F3B5F\"),
    plot.subtitle       = element_text(size = <<TAMANHO_EXPLICACAO>>, colour = \"grey30\"),
    plot.title.position = \"plot\"
  )"

# grafico_boxplot() ------------------------------------------------------------
#
# Pergunta: como se distribuem as observações em cada grupo?
#
# A figura da exploração, para olhar os dados antes de concluir. A caixa
# mostra a mediana e a metade central dos dados; os pontos são as
# observações. Caixas de alturas muito diferentes sugerem variâncias
# diferentes; pontos muito afastados merecem uma conferência na planilha.
# Serve às médias e às medianas.
#
# Argumentos:
#   resultado ........... o que comparar_medias() ou comparar_medianas()
#                         devolveu
#   rotulo_resposta ..... texto do eixo y; sem ele, vai o de comparar_medias()
#   rotulo_grupos ....... texto do eixo x; sem ele, vai o de comparar_medias()
#   mostrar_codigo ...... TRUE imprime o código R antes do gráfico
#
# Devolve um gráfico do ggplot2; ajustes extras entram com +.
#
# Exemplo:
#   resultado |>
#     grafico_boxplot(rotulo_resposta = "Peso (g)",
#                     rotulo_grupos   = "Tipo de Ração")
#
grafico_boxplot <- function(resultado,
                            rotulo_resposta = NULL,
                            rotulo_grupos   = NULL,
                            mostrar_codigo  = FALSE) {

  # Guardamos o nome do resultado, como o aluno o chamou.
  nome_resultado <- nome_do_objeto(substitute(resultado), padrao = "resultado")

  # Sem rótulo informado aqui, valem os que a análise guardou.
  rotulo_resposta <- ou_entao(rotulo_resposta, resultado$nomes$rotulo_resposta)
  rotulo_grupos   <- ou_entao(rotulo_grupos,   resultado$nomes$rotulo_grupos)

  # Trocamos os marcadores pelos nomes desta análise.
  receita <- preencher_receita(receita_grafico_boxplot, list(
    RESULTADO       = nome_resultado,
    RESPOSTA        = resultado$nomes$resposta,
    GRUPOS          = resultado$nomes$grupos,
    ROTULO_RESPOSTA = rotulo_resposta,
    ROTULO_GRUPOS   = rotulo_grupos
  ))

  # Rodamos a receita, mostrando o código antes, se o aluno pediu.
  executar_receita(
    receita        = receita,
    objetos        = rlang::set_names(list(resultado), nome_resultado),
    pacotes        = c("dplyr", "ggplot2"),
    mostrar_codigo = mostrar_codigo
  )
}

# A receita de grafico_boxplot(): caixas com as observações por cima.
receita_grafico_boxplot <- "
# Caixas com as observações por cima: mediana, metade central dos dados e
# cada observação de cada grupo.
ggplot(<<RESULTADO>>$dados, aes(x = <<GRUPOS>>, y = <<RESPOSTA>>)) +
  geom_boxplot(width = 0.5, outlier.shape = NA, colour = \"grey50\") +
  geom_point(
    position = position_jitter(width = 0.05, height = 0, seed = 1),
    size     = 1.5,
    alpha    = 0.5
  ) +
  labs(
    title = paste(\"Caixa: mediana e metade central dos dados; pontos: as observações.\",
                  \"Confira se as caixas têm alturas parecidas e se há pontos muito afastados.\",
                  sep = \"\\n\"),
    x     = \"<<ROTULO_GRUPOS>>\",
    y     = \"<<ROTULO_RESPOSTA>>\"
  ) +
  theme_classic(base_size = 12) +
  theme(plot.title = element_text(size = 10, colour = \"grey30\"),
        plot.title.position = \"plot\")"

# grafico_pares() --------------------------------------------------------------
#
# Pergunta: quais pares de grupos diferem, e por quanto?
#
# Cada linha compara dois grupos: o ponto é a diferença entre as médias e a
# haste, o seu intervalo de confiança. A linha tracejada vertical marca a
# diferença zero: haste que cruza a linha, sem diferença entre os dois
# grupos; haste toda de um lado, diferença. Em "B-A", a diferença é a média
# de B menos a média de A. Na ANOVA, os pares e os intervalos vêm do Tukey
# (do Games-Howell, na ANOVA de Welch); no teste t, há um par só, com o
# intervalo do próprio teste.
#
# Argumentos:
#   resultado ........... o que comparar_medias() devolveu
#   rotulo_resposta ..... a resposta no texto do eixo x; sem ele, vai o de comparar_medias()
#   rotulo_grupos ....... os grupos no texto do eixo y; sem ele, vai o de comparar_medias()
#   mostrar_codigo ...... TRUE imprime o código R antes do gráfico
#
# Devolve um gráfico do ggplot2; ajustes extras entram com +. Com muitos
# grupos (10 grupos dão 45 pares), salve a figura mais alta.
#
# Exemplo:
#   resultado |>
#     grafico_pares(rotulo_resposta = "Peso (g)",
#                   rotulo_grupos   = "Tipo de Ração")
#
grafico_pares <- function(resultado,
                          rotulo_resposta = NULL,
                          rotulo_grupos   = NULL,
                          mostrar_codigo  = FALSE) {

  # Guardamos o nome do resultado, como o aluno o chamou.
  nome_resultado <- nome_do_objeto(substitute(resultado), padrao = "resultado")

  # Sem rótulo informado aqui, valem os que a análise guardou.
  rotulo_resposta <- ou_entao(rotulo_resposta, resultado$nomes$rotulo_resposta)
  rotulo_grupos   <- ou_entao(rotulo_grupos,   resultado$nomes$rotulo_grupos)

  # Este gráfico mostra diferenças entre médias, com IC.
  conferir_resultado(resultado, "clara_medias",
                     paste("grafico_pares() mostra a diferença entre as médias de cada par.",
                           "Nas medianas, as comparações de Dunn estão em resultado$pares."))

  # Na ANOVA, os pares vêm do Tukey (ou do Games-Howell); no teste t, do
  # próprio teste.
  analise <- resultado$nomes$analise
  parte_pares <- dplyr::case_when(analise == "teste_t" ~ "teste", .default = "pares")
  metodo <- dplyr::case_when(
    analise == "anova"                ~ "Tukey",
    analise == "anova_welch"          ~ "Games-Howell",
    resultado$nomes$variancias_iguais ~ "teste t de Student",
    .default                          = "teste t de Welch"
  )

  # Trocamos os marcadores pelos nomes desta análise.
  receita <- preencher_receita(receita_grafico_pares, list(
    RESULTADO       = nome_resultado,
    ROTULO_RESPOSTA = rotulo_resposta,
    ROTULO_GRUPOS   = rotulo_grupos,
    NIVEL           = resultado$nomes$confianca * 100,
    PARTE_PARES     = parte_pares,
    METODO          = metodo
  ))

  # Rodamos a receita, mostrando o código antes, se o aluno pediu.
  executar_receita(
    receita        = receita,
    objetos        = rlang::set_names(list(resultado), nome_resultado),
    pacotes        = c("dplyr", "ggplot2"),
    mostrar_codigo = mostrar_codigo
  )
}

# A receita de grafico_pares(): a diferença de cada par, com o IC.
receita_grafico_pares <- "
# As comparações entre pares, na ordem da tabela, de cima para baixo.
# Em \"B-A\", a diferença é a média de B menos a média de A.
pares <- <<RESULTADO>>$<<PARTE_PARES>> |>
  mutate(comparacao = factor(comparacao, levels = rev(comparacao)))

# Ponto: a diferença; haste: o IC; linha tracejada: a diferença zero.
ggplot(pares, aes(x = diferenca, y = comparacao)) +
  geom_vline(xintercept = 0, linetype = \"dashed\", colour = \"grey60\") +
  geom_errorbar(
    aes(xmin = ic_inf, xmax = ic_sup),
    width = 0.2, linewidth = 0.8, colour = \"#2E7D8F\", orientation = \"y\"
  ) +
  geom_point(size = 3, colour = \"#0F3B5F\") +
  labs(
    title = paste(\"Ponto: diferença entre as médias do par; haste: IC <<NIVEL>>% (<<METODO>>).\",
                  \"Linha tracejada: diferença zero. Haste que cruza a linha: sem diferença entre os dois grupos.\",
                  sep = \"\\n\"),
    x     = \"Diferença entre as médias, <<ROTULO_RESPOSTA>>\",
    y     = \"<<ROTULO_GRUPOS>>\"
  ) +
  theme_classic(base_size = 12) +
  theme(plot.title = element_text(size = 10, colour = \"grey30\"),
        plot.title.position = \"plot\")"


# As receitas das médias para qualquer análise ---------------------------------
#
# medir_efeito(), escrever_resultados(), grafico_residuos() e grafico_qq()
# moram em clara_qualquer_analise.R e escolhem a receita pela análise feita.
# Estas são as receitas da ANOVA e do teste t.

# ANOVA: η² e ω² com intervalo de confiança, e a leitura de Cohen.
receitas_efeito$anova <- "
# Tamanho de efeito da ANOVA, com intervalo de confiança.
# alternative = \"two.sided\" dá um intervalo com os dois limites, como o do
# d de Cohen no teste t; sem ele, o limite de cima sairia sempre 1.
eta   <- eta_squared(<<RESULTADO>>$modelo,
                     partial     = FALSE,
                     ci          = <<CONFIANCA>>,
                     alternative = \"two.sided\")
omega <- omega_squared(<<RESULTADO>>$modelo,
                       partial     = FALSE,
                       ci          = <<CONFIANCA>>,
                       alternative = \"two.sided\")

tibble(
  medida = c(\"η²\", \"ω²\"),
  valor  = c(eta$Eta2, omega$Omega2),
  ic_inf = c(eta$CI_low, omega$CI_low),
  ic_sup = c(eta$CI_high, omega$CI_high)
) |>
  mutate(leitura = case_when(
    valor < 0.01 ~ \"muito pequeno\",
    valor < 0.06 ~ \"pequeno\",
    valor < 0.14 ~ \"médio\",
    .default     = \"grande\"
  ))"

# ANOVA de Welch: ω² aproximado, convertido do F de Welch, com intervalo.
receitas_efeito$anova_welch <- "
# Tamanho de efeito da ANOVA de Welch: ω² aproximado, convertido do F e dos
# graus de liberdade do Welch. É uma aproximação: o η² clássico supõe
# variâncias iguais e por isso não entra aqui. O intervalo também é
# aproximado; alternative = \"two.sided\" dá os dois limites.
welch <- <<RESULTADO>>$anova
omega <- F_to_omega2(f           = welch$f[1],
                     df          = welch$gl[1],
                     df_error    = welch$gl[2],
                     ci          = <<CONFIANCA>>,
                     alternative = \"two.sided\")

tibble(
  medida = \"ω² aproximado (Welch)\",
  valor  = omega$Omega2_partial,
  ic_inf = omega$CI_low,
  ic_sup = omega$CI_high
) |>
  mutate(leitura = case_when(
    valor < 0.01 ~ \"muito pequeno\",
    valor < 0.06 ~ \"pequeno\",
    valor < 0.14 ~ \"médio\",
    .default     = \"grande\"
  ))"

# Teste t: d de Cohen e g de Hedges com intervalo, e a leitura de Cohen.
receitas_efeito$teste_t <- "
# Tamanho de efeito do teste t, com intervalo de confiança.
# pooled_sd = TRUE junta os DPs dos grupos, como o Student; FALSE não junta,
# como o Welch.
d <- cohens_d(<<RESPOSTA>> ~ <<GRUPOS>>,
              data      = <<RESULTADO>>$dados,
              pooled_sd = <<VARIANCIAS_IGUAIS>>,
              ci        = <<CONFIANCA>>)
g <- hedges_g(<<RESPOSTA>> ~ <<GRUPOS>>,
              data      = <<RESULTADO>>$dados,
              pooled_sd = <<VARIANCIAS_IGUAIS>>,
              ci        = <<CONFIANCA>>)

tibble(
  medida = c(\"d de Cohen\", \"g de Hedges\"),
  valor  = c(d$Cohens_d, g$Hedges_g),
  ic_inf = c(d$CI_low, g$CI_low),
  ic_sup = c(d$CI_high, g$CI_high)
) |>
  mutate(leitura = case_when(
    abs(valor) < 0.2 ~ \"muito pequeno\",
    abs(valor) < 0.5 ~ \"pequeno\",
    abs(valor) < 0.8 ~ \"médio\",
    .default         = \"grande\"
  ))"

# ANOVA: amostra, F, η² e ω², Tukey, pressupostos, alerta, poder e síntese.
receitas_textos$anova <- "
# 1. Números com vírgula decimal, e o p como se escreve em texto científico.
com_virgula <- function(x, casas = 2) {
  formatC(x, format = \"f\", digits = casas, decimal.mark = \",\")
}
texto_p <- function(p) {
  case_when(p < 0.001 ~ \"p < 0,001\",
            .default  = paste(\"p =\", com_virgula(p, 3)))
}

# 2. As peças do resultado que as frases usam.
alfa         <- <<ALFA>>
amostra      <- <<RESULTADO>>$amostra
resumo       <- <<RESULTADO>>$resumo
linha_f      <- <<RESULTADO>>$anova[1, ]
gl_residuos  <- <<RESULTADO>>$anova$gl[2]
pares        <- <<RESULTADO>>$pares
pressupostos <- <<RESULTADO>>$pressupostos

# 3. A amostra: quantas observações entraram e quantas ficaram de fora.
excluidas <- case_when(
  amostra$excluidas == 0 ~ \"nenhuma foi excluída\",
  amostra$excluidas == 1 ~ \"1 foi excluída por não ter resposta ou grupo\",
  .default = paste(amostra$excluidas,
                   \"foram excluídas por não terem resposta ou grupo\")
)
texto_amostra <- str_glue(
  \"Após o preparo, havia {amostra$total} observações. A análise usou \",
  \"{amostra$usadas} delas, em {nrow(resumo)} grupos; {excluidas}.\"
)

# 4. A ANOVA numa frase, com a conclusão escrita conforme o p.
evidencia <- case_when(
  linha_f$p <  alfa ~ \"houve evidência de diferença entre as médias\",
  linha_f$p >= alfa ~ \"não houve evidência de diferença entre as médias\"
)
texto_teste <- str_glue(
  \"Em <<ROTULO_RESPOSTA>>, {evidencia} dos grupos de <<ROTULO_GRUPOS>> \",
  \"(F({linha_f$gl}, {gl_residuos}) = {com_virgula(linha_f$f)}; {texto_p(linha_f$p)}).\"
)

# 5. Tamanho de efeito, com a leitura de Cohen.
eta2   <- eta_squared(<<RESULTADO>>$modelo, partial = FALSE)$Eta2
omega2 <- omega_squared(<<RESULTADO>>$modelo, partial = FALSE)$Omega2
classe <- case_when(
  eta2 < 0.01 ~ \"muito pequeno\",
  eta2 < 0.06 ~ \"pequeno\",
  eta2 < 0.14 ~ \"médio\",
  .default    = \"grande\"
)
texto_efeito <- str_glue(
  \"O tamanho de efeito foi {classe} pela convenção de Cohen \",
  \"(η² = {com_virgula(eta2, 3)}; ω² = {com_virgula(omega2, 3)}), \",
  \"uma referência estatística, não biológica.\"
)

# 6. Tukey: quais pares de grupos diferiram.
diferentes <- pares |>
  filter(p_ajustado < alfa) |>
  mutate(frase = paste0(str_replace(comparacao, \"-\", \" e \"),
                        \" (\", texto_p(p_ajustado), \")\"))
texto_comparacoes <- case_when(
  linha_f$p >= alfa ~
    \"Como a ANOVA não indicou diferença global, as comparações de Tukey servem só para descrição.\",
  nrow(diferentes) == 0 ~
    \"Pelo teste de Tukey, nenhum par de grupos diferiu ao nível adotado, apesar da diferença global indicada pela ANOVA.\",
  nrow(diferentes) == 1 ~
    paste0(\"Pelo teste de Tukey, só diferiram \", diferentes$frase[1],
           \". Grupos que compartilham uma letra não diferiram entre si.\"),
  .default =
    paste0(\"Pelo teste de Tukey, diferiram os pares \",
           paste(diferentes$frase, collapse = \"; \"),
           \". Grupos que compartilham uma letra não diferiram entre si.\")
)

# 7. Pressupostos: o que cada teste mostrou, sem transformar p alto em prova.
frases <- pressupostos |>
  mutate(
    avaliacao = case_when(p >= alfa ~ \"sem evidência contra o pressuposto\",
                          .default  = \"o teste rejeitou o pressuposto\"),
    frase = str_glue(\"{pressuposto}, pelo {teste} (estatística = \",
                     \"{com_virgula(estatistica, 3)}; {texto_p(p)}): {avaliacao}.\")
  )
texto_pressupostos <- paste(
  paste(frases$frase, collapse = \" \"),
  \"Esses testes não comprovam os pressupostos; os gráficos dos resíduos e o delineamento completam a avaliação.\"
)

# 8. Alerta: o que pede cuidado antes de concluir. Com todos os grupos
#    grandes (regra prática: 30 ou mais), a ANOVA é pouco sensível a
#    desvios moderados da normalidade.
p_levene  <- pressupostos$p[pressupostos$teste == \"Levene\"]
p_shapiro <- pressupostos$p[pressupostos$teste == \"Shapiro-Wilk\"]
razao_dp  <- max(resumo$dp) / min(resumo$dp)
texto_alerta <- case_when(
  any(p_levene < alfa) ~
    paste0(\"As variâncias diferiram entre os grupos (o maior desvio padrão é \",
           com_virgula(razao_dp, 1), \" vezes o menor). Antes de concluir, \",
           \"compare com a ANOVA de Welch, que não supõe variâncias iguais (variancias_iguais = FALSE).\"),
  any(p_shapiro < alfa) & min(resumo$n) >= 30 ~
    paste0(\"Os resíduos se afastaram da normalidade, mas com todos os grupos grandes \",
           \"(30 ou mais observações) a ANOVA é pouco sensível a esse desvio. Confira no \",
           \"gráfico Q-Q se não há assimetria forte ou valores extremos.\"),
  any(p_shapiro < alfa) ~
    paste0(\"Os resíduos se afastaram da normalidade. Avalie o tamanho do desvio \",
           \"no gráfico Q-Q e, se for preciso, uma alternativa como o teste de Kruskal-Wallis.\"),
  .default =
    \"Os testes formais não detectaram os desvios examinados, mas os gráficos e o delineamento continuam necessários.\"
)

# 9. Poder do teste: só entra no texto quando muda a leitura, isto é, sem
#    evidência e com poder abaixo de 80%. pwr.anova.test() supõe grupos do
#    mesmo tamanho; usamos o n médio por grupo.
poder <- pwr.anova.test(
  k         = nrow(resumo),
  n         = mean(resumo$n),
  f         = sqrt(eta2 / (1 - eta2)),
  sig.level = alfa
)$power
texto_poder <- case_when(
  linha_f$p >= alfa & poder < 0.80 ~
    paste0(\"A ausência de evidência não deve ser lida como ausência de efeito: \",
           \"para o tamanho de efeito observado, o poder do teste foi de apenas \",
           com_virgula(100 * poder, 0), \"%. Uma amostra maior daria mais segurança à conclusão.\"),
  .default = \"\"
)

# 10. Síntese: o resultado principal numa frase só.
texto_sintese <- str_glue(
  \"Na amostra de {amostra$usadas} observações em {nrow(resumo)} grupos, {evidencia} \",
  \"(F({linha_f$gl}, {gl_residuos}) = {com_virgula(linha_f$f)}; {texto_p(linha_f$p)}; \",
  \"η² = {com_virgula(eta2, 3)}). A interpretação deve considerar os pressupostos e o delineamento.\"
)

# 11. Todas as frases, numa lista com partes nomeadas.
list(
  amostra      = texto_amostra,
  teste        = texto_teste,
  efeito       = texto_efeito,
  comparacoes  = texto_comparacoes,
  pressupostos = texto_pressupostos,
  alerta       = texto_alerta,
  poder        = texto_poder,
  sintese      = texto_sintese
) |>
  lapply(as.character)"

# ANOVA de Welch: amostra, F de Welch, ω² aproximado, Games-Howell,
# pressupostos, alerta, poder e síntese.
receitas_textos$anova_welch <- "
# 1. Números com vírgula decimal, e o p como se escreve em texto científico.
com_virgula <- function(x, casas = 2) {
  formatC(x, format = \"f\", digits = casas, decimal.mark = \",\")
}
texto_p <- function(p) {
  case_when(p < 0.001 ~ \"p < 0,001\",
            .default  = paste(\"p =\", com_virgula(p, 3)))
}

# 2. As peças do resultado que as frases usam.
alfa         <- <<ALFA>>
amostra      <- <<RESULTADO>>$amostra
resumo       <- <<RESULTADO>>$resumo
linha_f      <- <<RESULTADO>>$anova[1, ]
gl_residuos  <- <<RESULTADO>>$anova$gl[2]
pares        <- <<RESULTADO>>$pares
pressupostos <- <<RESULTADO>>$pressupostos

# 3. A amostra: quantas observações entraram e quantas ficaram de fora.
excluidas <- case_when(
  amostra$excluidas == 0 ~ \"nenhuma foi excluída\",
  amostra$excluidas == 1 ~ \"1 foi excluída por não ter resposta ou grupo\",
  .default = paste(amostra$excluidas,
                   \"foram excluídas por não terem resposta ou grupo\")
)
texto_amostra <- str_glue(
  \"Após o preparo, havia {amostra$total} observações. A análise usou \",
  \"{amostra$usadas} delas, em {nrow(resumo)} grupos; {excluidas}.\"
)

# 4. A ANOVA de Welch numa frase. O gl do denominador leva casas decimais,
#    por causa da correção de Welch.
evidencia <- case_when(
  linha_f$p <  alfa ~ \"houve evidência de diferença entre as médias\",
  linha_f$p >= alfa ~ \"não houve evidência de diferença entre as médias\"
)
gl_texto <- paste0(linha_f$gl, \"; \", com_virgula(gl_residuos))
texto_teste <- str_glue(
  \"Em <<ROTULO_RESPOSTA>>, {evidencia} dos grupos de <<ROTULO_GRUPOS>> pela \",
  \"ANOVA de Welch, que não supõe variâncias iguais \",
  \"(F({gl_texto}) = {com_virgula(linha_f$f)}; {texto_p(linha_f$p)}).\"
)

# 5. Tamanho de efeito: ω² aproximado, convertido do F de Welch.
omega2 <- F_to_omega2(f = linha_f$f, df = linha_f$gl, df_error = gl_residuos)$Omega2_partial
classe <- case_when(
  omega2 < 0.01 ~ \"muito pequeno\",
  omega2 < 0.06 ~ \"pequeno\",
  omega2 < 0.14 ~ \"médio\",
  .default      = \"grande\"
)
texto_efeito <- str_glue(
  \"O tamanho de efeito foi {classe} pela convenção de Cohen \",
  \"(ω² aproximado = {com_virgula(omega2, 3)}, convertido do F de Welch), \",
  \"uma referência estatística, não biológica.\"
)

# 6. Games-Howell: quais pares de grupos diferiram. Com menos de 6
#    observações num grupo, o teste fica menos confiável: avisamos.
diferentes <- pares |>
  filter(p_ajustado < alfa) |>
  mutate(frase = paste0(str_replace(comparacao, \"-\", \" e \"),
                        \" (\", texto_p(p_ajustado), \")\"))
pequenos <- as.character(resumo$<<GRUPOS>>[resumo$n < 6])
aviso_pequenos <- case_when(
  length(pequenos) == 0 ~ \"\",
  length(pequenos) == nrow(resumo) ~
    \" Com menos de seis observações em cada grupo, essas comparações pedem cautela.\",
  .default = paste0(\" Com menos de seis observações em \",
                    paste(pequenos, collapse = \", \"),
                    \", essas comparações pedem cautela.\")
)
texto_comparacoes <- paste0(case_when(
  linha_f$p >= alfa ~
    \"Como a ANOVA de Welch não indicou diferença global, as comparações de Games-Howell servem só para descrição.\",
  nrow(diferentes) == 0 ~
    \"Pelo teste de Games-Howell, nenhum par de grupos diferiu ao nível adotado, apesar da diferença global indicada pela ANOVA de Welch.\",
  nrow(diferentes) == 1 ~
    paste0(\"Pelo teste de Games-Howell, só diferiram \", diferentes$frase[1],
           \". Grupos que compartilham uma letra não diferiram entre si.\"),
  .default =
    paste0(\"Pelo teste de Games-Howell, diferiram os pares \",
           paste(diferentes$frase, collapse = \"; \"),
           \". Grupos que compartilham uma letra não diferiram entre si.\")
), aviso_pequenos)

# 7. Pressupostos: o que cada teste mostrou, sem transformar p alto em prova.
frases <- pressupostos |>
  mutate(
    avaliacao = case_when(p >= alfa ~ \"sem evidência contra o pressuposto\",
                          .default  = \"o teste rejeitou o pressuposto\"),
    frase = str_glue(\"{pressuposto}, pelo {teste} (estatística = \",
                     \"{com_virgula(estatistica, 3)}; {texto_p(p)}): {avaliacao}.\")
  )
texto_pressupostos <- paste(
  paste(frases$frase, collapse = \" \"),
  \"A ANOVA de Welch não supõe variâncias iguais.\",
  \"Esses testes não comprovam os pressupostos; os gráficos e o delineamento completam a avaliação.\"
)

# 8. Alerta: o que pede cuidado antes de concluir. Com todos os grupos
#    grandes (regra prática: 30 ou mais), a ANOVA é pouco sensível a
#    desvios moderados da normalidade.
p_shapiro <- pressupostos$p[pressupostos$teste == \"Shapiro-Wilk\"]
texto_alerta <- case_when(
  any(p_shapiro < alfa) & min(resumo$n) >= 30 ~
    paste0(\"A resposta se afastou da normalidade em pelo menos um grupo, mas com \",
           \"todos os grupos grandes (30 ou mais observações) a ANOVA de Welch é pouco \",
           \"sensível a esse desvio. Confira nos gráficos se não há assimetria forte ou \",
           \"valores extremos.\"),
  any(p_shapiro < alfa) ~
    paste0(\"A resposta se afastou da normalidade em pelo menos um grupo. Avalie o \",
           \"tamanho do desvio nos gráficos e, se for preciso, uma alternativa como o \",
           \"teste de Kruskal-Wallis.\"),
  .default =
    \"Os testes formais não detectaram os desvios examinados, mas os gráficos e o delineamento continuam necessários.\"
)

# 9. Poder do teste, aproximado: só entra no texto quando muda a leitura,
#    isto é, sem evidência e com poder abaixo de 80%. O pwr.anova.test()
#    supõe variâncias iguais; aqui usamos a F não central com os pesos do
#    próprio Welch (n / variância de cada grupo), para o efeito observado.
pesos            <- resumo$n / resumo$dp^2
media_ponderada  <- sum(pesos * resumo$media) / sum(pesos)
nao_centralidade <- sum(pesos * (resumo$media - media_ponderada)^2)
f_critico        <- qf(1 - alfa, df1 = linha_f$gl, df2 = gl_residuos)
poder <- pf(f_critico,
            df1        = linha_f$gl,
            df2        = gl_residuos,
            ncp        = nao_centralidade,
            lower.tail = FALSE)
texto_poder <- case_when(
  linha_f$p >= alfa & poder < 0.80 ~
    paste0(\"A ausência de evidência não deve ser lida como ausência de efeito: \",
           \"para o tamanho de efeito observado, o poder aproximado do teste foi de apenas \",
           com_virgula(100 * poder, 0), \"%. Uma amostra maior daria mais segurança à conclusão.\"),
  .default = \"\"
)

# 10. Síntese: o resultado principal numa frase só.
texto_sintese <- str_glue(
  \"Na amostra de {amostra$usadas} observações em {nrow(resumo)} grupos, {evidencia} \",
  \"pela ANOVA de Welch (F({gl_texto}) = {com_virgula(linha_f$f)}; \",
  \"{texto_p(linha_f$p)}; ω² aproximado = {com_virgula(omega2, 3)}). \",
  \"A interpretação deve considerar os pressupostos e o delineamento.\"
)

# 11. Todas as frases, numa lista com partes nomeadas.
list(
  amostra      = texto_amostra,
  teste        = texto_teste,
  efeito       = texto_efeito,
  comparacoes  = texto_comparacoes,
  pressupostos = texto_pressupostos,
  alerta       = texto_alerta,
  poder        = texto_poder,
  sintese      = texto_sintese
) |>
  lapply(as.character)"

# Teste t: amostra, t com a diferença e o IC, d e g, pressupostos, alerta,
# poder e síntese.
receitas_textos$teste_t <- "
# 1. Números com vírgula decimal, e o p como se escreve em texto científico.
com_virgula <- function(x, casas = 2) {
  formatC(x, format = \"f\", digits = casas, decimal.mark = \",\")
}
texto_p <- function(p) {
  case_when(p < 0.001 ~ \"p < 0,001\",
            .default  = paste(\"p =\", com_virgula(p, 3)))
}

# 2. As peças do resultado que as frases usam.
alfa         <- <<ALFA>>
amostra      <- <<RESULTADO>>$amostra
resumo       <- <<RESULTADO>>$resumo
teste        <- <<RESULTADO>>$teste
pressupostos <- <<RESULTADO>>$pressupostos
grupos       <- levels(<<RESULTADO>>$dados$<<GRUPOS>>)

# 3. A amostra: quantas observações entraram e quantas ficaram de fora.
excluidas <- case_when(
  amostra$excluidas == 0 ~ \"nenhuma foi excluída\",
  amostra$excluidas == 1 ~ \"1 foi excluída por não ter resposta ou grupo\",
  .default = paste(amostra$excluidas,
                   \"foram excluídas por não terem resposta ou grupo\")
)
texto_amostra <- str_glue(
  \"Após o preparo, havia {amostra$total} observações. A análise usou \",
  \"{amostra$usadas} delas, em {nrow(resumo)} grupos; {excluidas}.\"
)

# 4. O teste t numa frase, com a diferença entre as médias e seu intervalo.
#    Os graus de liberdade só levam casas decimais quando não são inteiros.
evidencia <- case_when(
  teste$p <  alfa ~ \"houve evidência de diferença entre as médias\",
  teste$p >= alfa ~ \"não houve evidência de diferença entre as médias\"
)
casas_gl <- case_when(teste$gl == round(teste$gl) ~ 0, .default = 2)
texto_teste <- str_glue(
  \"Em <<ROTULO_RESPOSTA>>, {evidencia} de {grupos[1]} e {grupos[2]} \",
  \"pelo teste t <<NOME_T>> (t({com_virgula(teste$gl, casas_gl)}) = \",
  \"{com_virgula(teste$t)}; {texto_p(teste$p)}). A média de {grupos[1]} \",
  \"menos a de {grupos[2]} foi {com_virgula(teste$diferenca)}, com IC <<NIVEL>>% \",
  \"de {com_virgula(teste$ic_inf)} a {com_virgula(teste$ic_sup)}.\"
)

# 5. Tamanho de efeito, com a leitura de Cohen. pooled_sd segue o teste:
#    TRUE junta os DPs dos grupos (Student); FALSE não junta (Welch).
d <- cohens_d(<<RESPOSTA>> ~ <<GRUPOS>>,
              data      = <<RESULTADO>>$dados,
              pooled_sd = <<VARIANCIAS_IGUAIS>>)$Cohens_d
g <- hedges_g(<<RESPOSTA>> ~ <<GRUPOS>>,
              data      = <<RESULTADO>>$dados,
              pooled_sd = <<VARIANCIAS_IGUAIS>>)$Hedges_g
classe <- case_when(
  abs(d) < 0.2 ~ \"muito pequeno\",
  abs(d) < 0.5 ~ \"pequeno\",
  abs(d) < 0.8 ~ \"médio\",
  .default     = \"grande\"
)
texto_efeito <- str_glue(
  \"O tamanho de efeito foi {classe} pela convenção de Cohen \",
  \"(d = {com_virgula(d)}; g = {com_virgula(g)}), \",
  \"uma referência estatística, não biológica.\"
)

# 6. Pressupostos: o que cada teste mostrou, sem transformar p alto em prova.
frases <- pressupostos |>
  mutate(
    avaliacao = case_when(p >= alfa ~ \"sem evidência contra o pressuposto\",
                          .default  = \"o teste rejeitou o pressuposto\"),
    frase = str_glue(\"{pressuposto}, pelo {teste} (estatística = \",
                     \"{com_virgula(estatistica, 3)}; {texto_p(p)}): {avaliacao}.\")
  )
texto_pressupostos <- paste(
  paste(frases$frase, collapse = \" \"),
  \"Esses testes não comprovam os pressupostos; os gráficos e o delineamento completam a avaliação.\"
)

# 7. Alerta: o que pede cuidado antes de concluir. Com os dois grupos
#    grandes (regra prática: 30 ou mais), o teste t é pouco sensível a
#    desvios moderados da normalidade.
p_levene  <- pressupostos$p[pressupostos$teste == \"Levene\"]
p_shapiro <- pressupostos$p[pressupostos$teste == \"Shapiro-Wilk\"]
razao_dp  <- max(resumo$dp) / min(resumo$dp)
texto_alerta <- case_when(
  any(p_levene < alfa) ~
    paste0(\"As variâncias diferiram entre os grupos (o maior desvio padrão é \",
           com_virgula(razao_dp, 1), \" vezes o menor). Prefira o teste t de Welch, \",
           \"que não supõe variâncias iguais (variancias_iguais = FALSE).\"),
  any(p_shapiro < alfa) & min(resumo$n) >= 30 ~
    paste0(\"A resposta se afastou da normalidade, mas com os dois grupos grandes \",
           \"(30 ou mais observações) o teste t é pouco sensível a esse desvio. Confira \",
           \"nos gráficos se não há assimetria forte ou valores extremos.\"),
  any(p_shapiro < alfa) ~
    paste0(\"A resposta se afastou da normalidade em pelo menos um grupo. Avalie o \",
           \"tamanho do desvio nos gráficos e, se for preciso, uma alternativa como o \",
           \"teste de Mann-Whitney.\"),
  .default =
    \"Os testes formais não detectaram os desvios examinados, mas os gráficos e o delineamento continuam necessários.\"
)

# 8. Poder do teste: só entra no texto quando muda a leitura, isto é, sem
#    evidência e com poder abaixo de 80%.
poder <- pwr.t2n.test(
  n1        = resumo$n[1],
  n2        = resumo$n[2],
  d         = abs(d),
  sig.level = alfa
)$power
texto_poder <- case_when(
  teste$p >= alfa & poder < 0.80 ~
    paste0(\"A ausência de evidência não deve ser lida como ausência de efeito: \",
           \"para o tamanho de efeito observado, o poder do teste foi de apenas \",
           com_virgula(100 * poder, 0), \"%. Uma amostra maior daria mais segurança à conclusão.\"),
  .default = \"\"
)

# 9. Síntese: o resultado principal numa frase só.
texto_sintese <- str_glue(
  \"Na amostra de {amostra$usadas} observações em dois grupos, {evidencia} \",
  \"(t({com_virgula(teste$gl, casas_gl)}) = {com_virgula(teste$t)}; {texto_p(teste$p)}; \",
  \"d = {com_virgula(d)}). A interpretação deve considerar os pressupostos e o delineamento.\"
)

# 10. Todas as frases, numa lista com partes nomeadas.
list(
  amostra      = texto_amostra,
  teste        = texto_teste,
  efeito       = texto_efeito,
  pressupostos = texto_pressupostos,
  alerta       = texto_alerta,
  poder        = texto_poder,
  sintese      = texto_sintese
) |>
  lapply(as.character)"

# ANOVA: os resíduos vêm do modelo.
trechos_diagnostico$preparo_anova <- "
# Resíduos e valores ajustados do modelo, ao lado de cada observação.
diagnostico <- <<RESULTADO>>$dados |>
  mutate(
    ajustado    = fitted(<<RESULTADO>>$modelo),
    residuo     = residuals(<<RESULTADO>>$modelo),
    padronizado = rstandard(<<RESULTADO>>$modelo)
  )"

# Teste t: o resíduo é a distância de cada observação até a média do grupo.
trechos_diagnostico$preparo_teste_t <- "
# Resíduo: o quanto cada observação se afasta da média do seu grupo.
diagnostico <- <<RESULTADO>>$dados |>
  group_by(<<GRUPOS>>) |>
  mutate(
    ajustado = mean(<<RESPOSTA>>),
    residuo  = <<RESPOSTA>> - ajustado
  ) |>
  ungroup()"

# Q-Q da ANOVA: todos os resíduos padronizados juntos.
trechos_diagnostico$qq_anova <- "
# Q-Q: os resíduos padronizados contra os quantis da distribuição normal.
ggplot(diagnostico, aes(sample = padronizado)) +
  stat_qq(size = 2.2, alpha = 0.7, colour = \"#2E7D8F\") +
  stat_qq_line(colour = \"#E76F51\") +
  labs(
    title = paste(\"Pontos perto da reta: resíduos compatíveis com a normal.\",
                  \"Caudas que se afastam indicam assimetria ou valores extremos.\",
                  sep = \"\\n\"),
    x     = \"Quantis teóricos\",
    y     = \"Resíduos padronizados\"
  ) +
  theme_classic(base_size = 12) +
  theme(plot.title = element_text(size = 10, colour = \"grey30\"),
        plot.title.position = \"plot\")"

# Q-Q do teste t: um painel por grupo, como o Shapiro-Wilk em cada grupo.
trechos_diagnostico$qq_teste_t <- "
# Q-Q em cada grupo: os resíduos contra os quantis da distribuição normal.
ggplot(diagnostico, aes(sample = residuo, colour = <<GRUPOS>>)) +
  stat_qq(size = 2.2, alpha = 0.7) +
  stat_qq_line() +
  facet_wrap(vars(<<GRUPOS>>)) +
  scale_colour_manual(values = cores, guide = \"none\") +
  labs(
    title = paste(\"Pontos perto da reta: resíduos compatíveis com a normal.\",
                  \"Caudas que se afastam indicam assimetria ou valores extremos.\",
                  sep = \"\\n\"),
    x     = \"Quantis teóricos\",
    y     = \"Resíduos (observação menos a média do grupo)\"
  ) +
  theme_classic(base_size = 12) +
  theme(plot.title = element_text(size = 10, colour = \"grey30\"),
        plot.title.position = \"plot\",
        strip.background = element_blank(),
        panel.spacing = unit(1.5, \"lines\"))"

# As receitas de grafico_residuos(), montadas com os trechos, uma por análise.
receitas_residuos$anova   <- paste(trimws(c(trecho_cores,
                                            trechos_diagnostico$preparo_anova,
                                            trechos_diagnostico$residuos)), collapse = "\n\n")
receitas_residuos$teste_t <- paste(trimws(c(trecho_cores,
                                            trechos_diagnostico$preparo_teste_t,
                                            trechos_diagnostico$residuos)), collapse = "\n\n")

# ANOVA de Welch: sem modelo aov(), o resíduo é a distância até a média do
# grupo, como no teste t; o Q-Q fica um painel por grupo, porque as
# variâncias diferem e o Shapiro é feito em cada grupo.
receitas_residuos$anova_welch <- receitas_residuos$teste_t

# As receitas de grafico_qq(), montadas com os trechos, uma por análise.
receitas_qq$anova   <- paste(trimws(c(trechos_diagnostico$preparo_anova,
                                      trechos_diagnostico$qq_anova)), collapse = "\n\n")
receitas_qq$teste_t <- paste(trimws(c(trecho_cores,
                                      trechos_diagnostico$preparo_teste_t,
                                      trechos_diagnostico$qq_teste_t)), collapse = "\n\n")
receitas_qq$anova_welch <- receitas_qq$teste_t


# Exibição no console ----------------------------------------------------------
#
# Ao digitar o nome do resultado no console, o R chama esta função.
# Ela mostra a análise como um pequeno relatório, na ordem do estudo.

print.clara_medias <- function(x, ...) {

  # Título e tamanho da amostra.
  cat("\nComparação de médias:", x$nomes$rotulo_resposta, "por", x$nomes$rotulo_grupos, "\n")
  cat(nrow(x$dados), "observações em", nrow(x$resumo), "grupos")

  # Avisamos quando linhas ficaram de fora por falta de resposta ou grupo.
  if (x$amostra$excluidas > 0) {
    cat(" (", x$amostra$excluidas, " de ", x$amostra$total,
        " excluídas por falta de resposta ou grupo)", sep = "")
  }
  cat("\n")

  # Resumo dos grupos, com as letras.
  cat("\n# Resumo dos grupos\n")
  print(x$resumo)

  # O restante depende do teste que a ClaRa escolheu.
  if (x$nomes$analise == "teste_t") imprimir_teste_t(x) else imprimir_anova(x)

  invisible(x)
}

# A parte do relatório da ANOVA (clássica ou de Welch): F, pressupostos e
# comparações entre pares.
imprimir_anova <- function(x) {

  # A linha do fator na tabela da ANOVA, e qual das duas ANOVAs rodou.
  linha_f <- x$anova[1, ]
  welch   <- x$nomes$analise == "anova_welch"

  # O resultado da ANOVA numa frase. No Welch, o gl do denominador tem
  # casas decimais e, por causa da vírgula, os dois gl se separam por ";".
  cat(dplyr::case_when(welch ~ "\n# ANOVA de Welch\n", .default = "\n# ANOVA\n"))
  cat("F(", linha_f$gl, dplyr::case_when(welch ~ "; ", .default = ", "),
      numero(x$anova$gl[2], dplyr::case_when(welch ~ 2, .default = 0)),
      ") = ", numero(linha_f$f), "; ", escrever_p(linha_f$p), ": ",
      concluir_medias(linha_f$p, x$nomes$confianca), "\n", sep = "")

  # Os pressupostos, com a leitura de cada teste.
  cat("\n# Pressupostos\n")
  print(x$pressupostos)
  if (welch) cat("A ANOVA de Welch não exige variâncias iguais; por isso não há Levene.\n")

  # Os pares: Tukey na clássica, Games-Howell na de Welch.
  cat(dplyr::case_when(welch ~ "\n# Comparações de Games-Howell\n",
                       .default = "\n# Comparações de Tukey\n"))
  print(x$pares)

  # Um lembrete das partes que podem ser abertas com $.
  cat("\nPartes do resultado: $resumo $anova $pressupostos $pares",
      if (!welch) "$modelo", "$amostra $dados\n")
}

# A parte do relatório do teste t: t, diferença com IC e pressupostos.
imprimir_teste_t <- function(x) {

  # A única linha da tabela do teste, e qual dos dois testes t rodou.
  teste <- x$teste
  nome_teste <- dplyr::case_when(x$nomes$variancias_iguais ~ "Student",
                                 .default                  = "Welch")

  # Por que não há ANOVA, e o resultado do teste numa frase.
  cat("\n# Teste t de ", nome_teste, "\n", sep = "")
  cat("Com dois grupos, a ClaRa usa o teste t no lugar da ANOVA.\n")

  # Os graus de liberdade só levam casas decimais quando não são inteiros (Welch).
  casas_gl <- dplyr::case_when(teste$gl == round(teste$gl) ~ 0, .default = 2)
  cat("t(", numero(teste$gl, casas_gl), ") = ", numero(teste$t), "; ",
      escrever_p(teste$p), ": ",
      concluir_medias(teste$p, x$nomes$confianca), "\n", sep = "")

  # A diferença entre as médias, com o intervalo de confiança.
  cat("Diferença ", teste$comparacao, ": ", numero(teste$diferenca),
      " (IC ", x$nomes$confianca * 100, "%: ", numero(teste$ic_inf),
      " a ", numero(teste$ic_sup), ")\n", sep = "")

  # Os pressupostos, com a leitura de cada teste.
  cat("\n# Pressupostos\n")
  print(x$pressupostos)

  # Um lembrete sobre as variâncias, conforme o teste e o que o Levene mostrou.
  levene_rejeitou <- any(x$pressupostos$teste == "Levene" &
                           x$pressupostos$p < 1 - x$nomes$confianca)
  cat(dplyr::case_when(
    !x$nomes$variancias_iguais ~
      "O teste de Welch não exige variâncias iguais; por isso não há Levene.",
    levene_rejeitou ~
      "O Levene rejeitou a igualdade das variâncias: prefira o Welch (variancias_iguais = FALSE).",
    .default =
      "O teste de Student supõe variâncias iguais; o Levene não mostrou evidência contra."
  ), "\n", sep = "")

  # Um lembrete das partes que podem ser abertas com $.
  cat("\nPartes do resultado: $resumo $teste $pressupostos $amostra $dados\n")
}

# A conclusão da comparação, escrita conforme o p-valor.
concluir_medias <- function(p, confianca) {
  dplyr::case_when(
    p <  1 - confianca ~ "há evidência de diferença entre as médias dos grupos.",
    p >= 1 - confianca ~ "não há evidência de diferença entre as médias dos grupos."
  )
}
