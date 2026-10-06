# =============================================================================
#  ClaRa: comparar medianas
# =============================================================================
#
#  Tudo da pergunta "a mediana da resposta difere entre os grupos?": a
#  análise (comparar_medianas()), o gráfico dela, as receitas que as funções
#  para qualquer análise usam com as medianas e o relatório no console.
#
#  Carregado por R/clara.R; no roteiro, basta source("R/clara.R").
# =============================================================================


# Comparar medianas ------------------------------------------------------------
#
# Pergunta: a mediana da resposta difere entre os grupos?
# comparar_medianas() faz a análise; grafico_medianas() a mostra. Para
# explorar as observações, grafico_boxplot() serve às medianas também.
# A ClaRa escolhe o teste: Mann-Whitney com dois grupos, Kruskal-Wallis
# com três ou mais. Os dois comparam as posições (postos) das observações,
# sem supor distribuição normal.

# comparar_medianas() ----------------------------------------------------------
#
# Pergunta: a mediana da resposta difere entre os grupos?
#
# Use quando a média não representa bem os grupos: resposta assimétrica,
# valores extremos ou resíduos longe da normal com grupos pequenos. Os
# testes ordenam todas as observações, da menor para a maior, e comparam as
# posições (postos) de cada grupo. Não supõem distribuição normal, mas
# supõem dispersão parecida nos grupos: só assim a diferença entre as
# posições é uma diferença entre as medianas.
#
# A ClaRa escolhe o teste pelo número de grupos com dados:
#   dois grupos ........ Mann-Whitney (também chamado de Wilcoxon);
#   três ou mais ....... Kruskal-Wallis, com Dunn para os pares.
# Em qualquer caso, faz a análise na ordem em que a estudamos: resumo dos
# grupos, teste, pressuposto e letras.
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
#   mostrar_codigo ...... TRUE imprime o código R antes do resultado
#
# Devolve uma lista. Cada parte se abre com $:
#   resultado$resumo        n, mediana, quartis (q1 e q3), amplitude
#                           interquartil (aiq = q3 - q1) e letra de cada grupo
#   resultado$teste         o teste, numa linha
#   resultado$pressupostos  o teste da dispersão, com a leitura dele
#   resultado$amostra       observações no total, usadas e excluídas
#   resultado$dados         as observações usadas na análise
# Com três ou mais grupos (Kruskal-Wallis):
#   resultado$pares         todas as comparações de Dunn, com p ajustado
# Com dois grupos (Mann-Whitney), resultado$teste traz também o
# deslocamento: a mediana das diferenças entre uma observação de cada
# grupo (estimativa de Hodges-Lehmann), com o seu intervalo de confiança.
#
# Exemplo:
#   resultado <- tilapias |>
#     comparar_medianas(resposta        = peso,
#                       grupos          = microalga,
#                       rotulo_resposta = "Peso (g)",
#                       rotulo_grupos   = "Microalga")
#
#   resultado |>
#     grafico_medianas()        # os rótulos já vêm do resultado
#
comparar_medianas <- function(dados,
                              resposta,
                              grupos,
                              rotulo_resposta = NULL,
                              rotulo_grupos   = NULL,
                              confianca       = 0.95,
                              mostrar_codigo  = FALSE) {

  # Guardamos o nome da tabela que o aluno usou (tilapias, por exemplo).
  nome_dados <- nome_do_objeto(substitute(dados), padrao = "dados")

  # Guardamos os nomes das duas colunas, como o aluno as escreveu.
  coluna_resposta <- rlang::as_name(rlang::ensym(resposta))
  coluna_grupos   <- rlang::as_name(rlang::ensym(grupos))

  # Antes de calcular, conferimos se as colunas existem e servem para comparar.
  conferir_colunas(dados, coluna_resposta, coluna_grupos)
  conferir_rotulo(rotulo_resposta, "rotulo_resposta")
  conferir_rotulo(rotulo_grupos, "rotulo_grupos")

  # Contamos os grupos que têm resposta: dois pedem o Mann-Whitney; mais, o
  # Kruskal-Wallis.
  quantos_grupos <- dplyr::n_distinct(
    dados[[coluna_grupos]][!is.na(dados[[coluna_resposta]])],
    na.rm = TRUE
  )
  analise <- dplyr::case_when(quantos_grupos == 2 ~ "mann_whitney",
                              .default            = "kruskal")

  # Trocamos os marcadores da receita escolhida pelos nomes desta análise.
  receita <- preencher_receita(receitas_medianas[[analise]], list(
    DADOS     = nome_dados,
    RESPOSTA  = coluna_resposta,
    GRUPOS    = coluna_grupos,
    CONFIANCA = confianca,
    ALFA      = 1 - confianca
  ))

  # Rodamos a receita, mostrando o código antes, se o aluno pediu.
  resultado <- executar_receita(
    receita        = receita,
    objetos        = rlang::set_names(list(dados), nome_dados),
    pacotes        = pacotes_medianas[[analise]],
    mostrar_codigo = mostrar_codigo
  )

  # Anotamos no resultado o que foi analisado; as outras funções vão precisar.
  # Os rótulos também ficam: gráficos e textos os usam sem o aluno repeti-los.
  resultado$nomes <- list(
    dados           = nome_dados,
    resposta        = coluna_resposta,
    grupos          = coluna_grupos,
    rotulo_resposta = ou_entao(rotulo_resposta, coluna_resposta),
    rotulo_grupos   = ou_entao(rotulo_grupos, coluna_grupos),
    confianca       = confianca,
    analise         = analise
  )

  # Damos ao resultado uma etiqueta, para o console saber como exibi-lo.
  structure(resultado, class = "clara_medianas")
}


# As receitas de comparar_medianas() -------------------------------------------
#
# Uma receita para cada caso. comparar_medianas() escolhe uma e a preenche.

receitas_medianas <- list()

# O começo das duas receitas: as observações da análise e o resumo dos grupos.
trecho_resumo_medianas <- "
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

# 2. Resumo de cada grupo: tamanho, mediana e quartis da resposta.
#    Metade das observações fica entre o 1º quartil (q1) e o 3º (q3); a
#    amplitude interquartil (aiq) é a altura dessa faixa central.
resumo <- dados_analise |>
  group_by(<<GRUPOS>>) |>
  summarise(
    n       = n(),
    mediana = median(<<RESPOSTA>>),
    q1      = quantile(<<RESPOSTA>>, 0.25, names = FALSE),
    q3      = quantile(<<RESPOSTA>>, 0.75, names = FALSE),
    aiq     = q3 - q1
  )"

# O pressuposto das duas receitas: dispersão parecida nos grupos.
trecho_dispersao_medianas <- "
# 4. Pressuposto: dispersão parecida nos grupos, pelo teste de Fligner-Killeen,
#    que também usa postos. Com dispersões parecidas, o teste compara as
#    medianas; com dispersões diferentes, compara as distribuições inteiras.
dispersao <- fligner.test(<<RESPOSTA>> ~ <<GRUPOS>>, data = dados_analise) |> tidy()

pressupostos <- tibble(
  pressuposto = \"Dispersão parecida nos grupos\",
  teste       = \"Fligner-Killeen\",
  estatistica = dispersao$statistic,
  p           = dispersao$p.value
) |>
  mutate(leitura = case_when(
    p >= <<ALFA>> ~ \"sem evidência contra o pressuposto\",
    p <  <<ALFA>> ~ \"pressuposto rejeitado: o teste compara as distribuições, não só as medianas\"
  ))"

# Três ou mais grupos: Kruskal-Wallis, dispersão, Dunn e letras.
receitas_medianas$kruskal <- paste0(trecho_resumo_medianas, "

# 3. Kruskal-Wallis: as posições (postos) das observações diferem entre os grupos?
kruskal <- kruskal.test(<<RESPOSTA>> ~ <<GRUPOS>>, data = dados_analise)

teste <- kruskal |>
  tidy() |>
  transmute(
    h  = statistic,
    gl = parameter,
    p  = p.value
  )
", trecho_dispersao_medianas, "

# 5. Dunn: cada par de grupos, comparando os postos médios, com o p ajustado
#    pelo método de Holm. O posto é a posição de cada observação, da menor
#    (1) para a maior; valores repetidos (empates) dividem as posições.
postos <- dados_analise |>
  mutate(posto = rank(<<RESPOSTA>>))

# A variância dos postos, com a correção para os empates.
n_total   <- nrow(postos)
empates   <- table(postos$<<RESPOSTA>>)
variancia <- n_total * (n_total + 1) / 12 - sum(empates^3 - empates) / (12 * (n_total - 1))

# O posto médio de cada grupo.
postos_medios <- postos |>
  group_by(<<GRUPOS>>) |>
  summarise(n = n(), posto_medio = mean(posto))

# Todos os pares, na ordem do Tukey: em \"B-A\", o posto médio de B menos o de A.
pares <- cross_join(postos_medios, postos_medios, suffix = c(\"_1\", \"_2\")) |>
  filter(as.integer(<<GRUPOS>>_1) > as.integer(<<GRUPOS>>_2)) |>
  arrange(<<GRUPOS>>_2, <<GRUPOS>>_1) |>
  transmute(
    comparacao       = paste(<<GRUPOS>>_1, <<GRUPOS>>_2, sep = \"-\"),
    diferenca_postos = posto_medio_1 - posto_medio_2,
    z                = diferenca_postos / sqrt(variancia * (1 / n_1 + 1 / n_2)),
    p                = 2 * pnorm(-abs(z)),
    p_ajustado       = p.adjust(p, method = \"holm\")
  )

# 6. Letras: grupos com a mesma letra não diferiram. A maior mediana leva \"a\".
matriz_p <- vec2mat(setNames(pares$p_ajustado, pares$comparacao))
ordem    <- resumo |> arrange(desc(mediana)) |> pull(<<GRUPOS>>) |> as.character()
letras   <- multcompLetters(matriz_p[ordem, ordem], threshold = <<ALFA>>)$Letters

resumo <- resumo |>
  mutate(letra = letras[as.character(<<GRUPOS>>)])

# 7. Tudo junto, numa lista com partes nomeadas.
list(
  resumo       = resumo,
  teste        = teste,
  pressupostos = pressupostos,
  pares        = pares,
  amostra      = amostra,
  dados        = dados_analise
)")

# Dois grupos: Mann-Whitney, dispersão e letras.
receitas_medianas$mann_whitney <- paste0(trecho_resumo_medianas, "

# 3. Mann-Whitney: as posições (postos) das observações diferem entre os dois grupos?
#    Sem empates e com menos de 50 observações, o p é exato; senão, vem da
#    aproximação normal. O deslocamento é a mediana das diferenças entre uma
#    observação do primeiro grupo e uma do segundo (Hodges-Lehmann).
exato <- nrow(dados_analise) < 50 &
  n_distinct(dados_analise$<<RESPOSTA>>) == nrow(dados_analise)

mann_whitney <- wilcox.test(<<RESPOSTA>> ~ <<GRUPOS>>,
                            data       = dados_analise,
                            exact      = exato,
                            conf.int   = TRUE,
                            conf.level = <<CONFIANCA>>)

teste <- mann_whitney |>
  tidy() |>
  transmute(
    comparacao   = paste(levels(dados_analise$<<GRUPOS>>), collapse = \"-\"),
    deslocamento = estimate,
    ic_inf       = conf.low,
    ic_sup       = conf.high,
    w            = statistic,
    p            = p.value
  )
", trecho_dispersao_medianas, "

# 5. Letras: grupos com a mesma letra não diferiram.
#    Havendo diferença, a maior mediana leva \"a\".
resumo <- resumo |>
  mutate(letra = case_when(
    teste$p >= <<ALFA>>       ~ \"a\",
    mediana == max(mediana) ~ \"a\",
    .default                = \"b\"
  ))

# 6. Tudo junto, numa lista com partes nomeadas.
list(
  resumo       = resumo,
  teste        = teste,
  pressupostos = pressupostos,
  amostra      = amostra,
  dados        = dados_analise
)")

# Os pacotes que cada receita usa.
pacotes_medianas <- list(
  kruskal      = c("dplyr", "broom", "multcompView"),
  mann_whitney = c("dplyr", "broom")
)


# grafico_medianas() -----------------------------------------------------------
#
# Pergunta: como mostrar a comparação das medianas numa só figura?
#
# A figura principal da comparação de medianas. Por padrão: cada ponto
# colorido é uma observação; o losango é a mediana do grupo; a haste vai do
# 1º ao 3º quartil (a metade central dos dados); as letras, todas na mesma
# altura, vêm do Dunn (ou do Mann-Whitney, com dois grupos). A explicação
# no topo cita só as partes que aparecem.
#
# Argumentos:
#   resultado ........... o que comparar_medianas() devolveu
#   rotulo_resposta ..... texto do eixo y; sem ele, vai o de comparar_medianas()
#   rotulo_grupos ....... texto do eixo x; sem ele, vai o de comparar_medianas()
#   titulo .............. título da figura (padrão: sem título)
#   explicacao .......... TRUE (padrão) escreve no topo o que cada elemento
#                         mostra; FALSE tira, para a explicação ir para a
#                         legenda do artigo
#   mostrar_pontos ...... TRUE (padrão) desenha cada observação
#   mostrar_letras ...... TRUE (padrão) escreve as letras das comparações
#   cores ............... "ocean" (padrão), "cinza" (para impressão em preto e
#                         branco) ou um vetor de cores, como
#                         c("darkgreen", "orange"); Tab mostra as paletas
#   tamanho_texto ....... tamanho base do texto (padrão 12); as letras e a
#                         explicação acompanham
#   fonte ............... "sans" (padrão; Arial no Windows) ou "serif"
#                         (Times New Roman no Windows), para todos os textos
#   mostrar_codigo ...... TRUE imprime o código R antes do gráfico
#
# Devolve um gráfico do ggplot2; ajustes extras entram com +.
#
# Exemplo:
#   resultado |>
#     grafico_medianas(rotulo_resposta = "Peso (g)",
#                      rotulo_grupos   = "Microalga")
#
grafico_medianas <- function(resultado,
                             rotulo_resposta = NULL,
                             rotulo_grupos   = NULL,
                             titulo          = NULL,
                             explicacao      = TRUE,
                             mostrar_pontos  = TRUE,
                             mostrar_letras  = TRUE,
                             cores           = c("ocean", "cinza"),
                             tamanho_texto   = 12,
                             fonte           = c("sans", "serif"),
                             mostrar_codigo  = FALSE) {

  # Guardamos o nome do resultado, como o aluno o chamou.
  nome_resultado <- nome_do_objeto(substitute(resultado), padrao = "resultado")

  # Este gráfico desenha o resultado de comparar_medianas().
  conferir_resultado(resultado, "clara_medianas",
                     "grafico_medianas() desenha o resultado de comparar_medianas(). Para as médias, use grafico_medias().")

  # Conferimos as escolhas antes de desenhar. Nas opções listadas no padrão
  # (que o Tab do RStudio mostra), sem escolha vale a primeira.
  fonte <- escolher_opcao(fonte, c("sans", "serif"), "fonte")
  if (identical(cores, c("ocean", "cinza"))) cores <- "ocean"
  conferir_sim_ou_nao(explicacao,     "explicacao")
  conferir_sim_ou_nao(mostrar_pontos, "mostrar_pontos")
  conferir_sim_ou_nao(mostrar_letras, "mostrar_letras")
  vetor_cores <- escolher_cores(cores)

  # Sem rótulo informado aqui, valem os que a análise guardou.
  rotulo_resposta <- ou_entao(rotulo_resposta, resultado$nomes$rotulo_resposta)
  rotulo_grupos   <- ou_entao(rotulo_grupos,   resultado$nomes$rotulo_grupos)

  # A explicação do topo cita só as partes que aparecem na figura.
  partes <- c(if (mostrar_pontos) "pontos: observações",
              "losango: mediana",
              "hastes: do 1º ao 3º quartil (metade central dos dados)")
  texto_explicacao <- paste0(paste(partes, collapse = "; "), ".",
                             if (mostrar_letras) "\\nLetras iguais: sem diferença significativa.")
  texto_explicacao <- paste0(toupper(substr(texto_explicacao, 1, 1)),
                             substring(texto_explicacao, 2))

  # O preparo: só os objetos que as camadas pedidas vão usar.
  t <- trechos_grafico_medianas
  preparo <- c(
    if (mostrar_pontos) trecho_de_cores(vetor_cores),
    if (mostrar_pontos) t$pontos,
    t$medianas,
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
    if (mostrar_pontos) t$pontos_camada,
    t$haste,
    t$mediana,
    if (mostrar_letras) t$letras,
    if (mostrar_pontos) t$escala_cor,
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
    TAMANHO            = tamanho_texto,
    FAMILIA_TEXTO      = dplyr::case_when(fonte == "serif" ~ ", family = \"serif\"",
                                          .default         = ""),
    FAMILIA_TEMA       = dplyr::case_when(fonte == "serif" ~ ", base_family = \"serif\"",
                                          .default         = ""),
    TAMANHO_LETRAS     = round(6 * tamanho_texto / 12, 1),
    TAMANHO_EXPLICACAO = round(10 * tamanho_texto / 12, 1)
  ))

  # Rodamos a receita, mostrando o código antes, se o aluno pediu.
  executar_receita(
    receita        = receita,
    objetos        = rlang::set_names(list(resultado), nome_resultado),
    pacotes        = c("dplyr", "ggplot2"),
    mostrar_codigo = mostrar_codigo
  )
}

# Os trechos da receita de grafico_medianas(). A função escolhe os que a
# figura pedida usa e os junta, na ordem, numa receita só.
trechos_grafico_medianas <- list()

# Preparo: as observações de cada grupo.
trechos_grafico_medianas$pontos <- "
# As observações de cada grupo.
pontos <- <<RESULTADO>>$dados"

# Preparo: as medianas, com os quartis.
trechos_grafico_medianas$medianas <- "
# As medianas de cada grupo, com os quartis (a haste vai de q1 a q3).
medianas <- <<RESULTADO>>$resumo"

# Preparo: a altura das letras, com e sem os pontos na figura.
trechos_grafico_medianas$altura_com_pontos <- "
# Todas as letras na mesma altura, acima do ponto ou da haste mais alta.
altura_letras <- max(pontos$<<RESPOSTA>>, medianas$q3)"

trechos_grafico_medianas$altura_sem_pontos <- "
# Todas as letras na mesma altura, acima da haste mais alta.
altura_letras <- max(medianas$q3)"

# Camadas da figura, uma por trecho.
trechos_grafico_medianas$base <- "ggplot(medianas, aes(x = <<GRUPOS>>))"

trechos_grafico_medianas$pontos_camada <- "  geom_point(
    data     = pontos,
    aes(y = <<RESPOSTA>>, colour = <<GRUPOS>>),
    position = position_jitter(width = 0.08, height = 0, seed = 1),
    size     = 2.2,
    alpha    = 0.7
  )"

trechos_grafico_medianas$haste <- "  geom_errorbar(
    aes(ymin = q1, ymax = q3),
    width = 0.08, linewidth = 0.8, colour = \"#0F3B5F\"
  )"

trechos_grafico_medianas$mediana <- "  geom_point(aes(y = mediana), shape = 18, size = 4.4, colour = \"#0F3B5F\")"

trechos_grafico_medianas$letras <- "  geom_text(
    aes(label = letra),
    y = altura_letras, vjust = -0.6,
    size = <<TAMANHO_LETRAS>>, fontface = \"bold\", colour = \"#0F3B5F\"<<FAMILIA_TEXTO>>
  )"

trechos_grafico_medianas$escala_cor <- "  scale_colour_manual(values = cores, guide = \"none\")"

trechos_grafico_medianas$escala_x <- "  scale_x_discrete(expand = expansion(add = 0.6))"

trechos_grafico_medianas$escala_y <- "  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15)))"

trechos_grafico_medianas$tema <- "  theme_classic(base_size = <<TAMANHO>><<FAMILIA_TEMA>>) +
  theme(
    plot.title          = element_text(colour = \"#0F3B5F\"),
    plot.subtitle       = element_text(size = <<TAMANHO_EXPLICACAO>>, colour = \"grey30\"),
    plot.title.position = \"plot\"
  )"


# As receitas das medianas para qualquer análise ------------------------------
#
# medir_efeito() e escrever_resultados() moram em clara_qualquer_analise.R e
# escolhem a receita pela análise feita. Estas são as receitas do
# Kruskal-Wallis e do Mann-Whitney. grafico_residuos() e grafico_qq() não
# têm receita aqui: os testes das medianas não supõem resíduos normais nem
# variâncias iguais.

# Kruskal-Wallis: ε² dos postos com intervalo de confiança, e a leitura.
receitas_efeito$kruskal <- "
# Tamanho de efeito do Kruskal-Wallis: ε² dos postos, a fração da variação
# das posições que acompanha os grupos. O intervalo vem de reamostragem
# (bootstrap); a semente faz o resultado se repetir a cada execução.
# alternative = \"two.sided\" dá os dois limites, como no η² da ANOVA.
set.seed(1)
epsilon <- rank_epsilon_squared(<<RESPOSTA>> ~ <<GRUPOS>>,
                                data        = <<RESULTADO>>$dados,
                                ci          = <<CONFIANCA>>,
                                alternative = \"two.sided\")

tibble(
  medida = \"ε² (postos)\",
  valor  = epsilon$rank_epsilon_squared,
  ic_inf = epsilon$CI_low,
  ic_sup = epsilon$CI_high
) |>
  mutate(leitura = case_when(
    valor < 0.01 ~ \"muito pequeno\",
    valor < 0.06 ~ \"pequeno\",
    valor < 0.14 ~ \"médio\",
    .default     = \"grande\"
  ))"

# Mann-Whitney: correlação bisserial dos postos com intervalo, e a leitura.
receitas_efeito$mann_whitney <- "
# Tamanho de efeito do Mann-Whitney: a correlação bisserial dos postos (r),
# de -1 a 1. Ela compara cada observação de um grupo com cada uma do outro:
# r = 1 quando todas do primeiro grupo superam todas do segundo; r = 0
# quando cada grupo supera o outro metade das vezes.
r <- rank_biserial(<<RESPOSTA>> ~ <<GRUPOS>>,
                   data = <<RESULTADO>>$dados,
                   ci   = <<CONFIANCA>>)

tibble(
  medida = \"r (bisserial dos postos)\",
  valor  = r$r_rank_biserial,
  ic_inf = r$CI_low,
  ic_sup = r$CI_high
) |>
  mutate(leitura = case_when(
    abs(valor) < 0.1 ~ \"muito pequeno\",
    abs(valor) < 0.3 ~ \"pequeno\",
    abs(valor) < 0.5 ~ \"médio\",
    .default         = \"grande\"
  ))"

# O começo das receitas de texto: vírgula decimal, p e amostra.
trecho_textos_medianas <- "
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

# 4. O que o teste compara depende da dispersão: parecida nos grupos, as
#    medianas; diferente, as distribuições inteiras.
dispersao_parecida <- pressupostos$p >= alfa
alvo <- case_when(dispersao_parecida ~ \"as medianas\",
                  .default           = \"as distribuições\")
evidencia <- case_when(
  teste$p <  alfa ~ paste(\"houve evidência de diferença entre\", alvo),
  teste$p >= alfa ~ paste(\"não houve evidência de diferença entre\", alvo)
)

# 5. Pressuposto: o que o teste mostrou, sem transformar p alto em prova.
frases <- pressupostos |>
  mutate(
    avaliacao = case_when(p >= alfa ~ \"sem evidência contra o pressuposto\",
                          .default  = \"o teste rejeitou o pressuposto\"),
    frase = str_glue(\"{pressuposto}, pelo {teste} (estatística = \",
                     \"{com_virgula(estatistica, 3)}; {texto_p(p)}): {avaliacao}.\")
  )
texto_pressupostos <- paste(
  paste(frases$frase, collapse = \" \"),
  \"Esse teste não comprova o pressuposto; o gráfico das observações e o delineamento completam a avaliação.\"
)

# 6. Alerta: o que pede cuidado antes de concluir.
texto_alerta <- case_when(
  !dispersao_parecida ~
    paste0(\"As dispersões diferiram entre os grupos. Nesse caso, o teste compara as \",
           \"distribuições inteiras, não só as medianas: um grupo pode diferir por ser \",
           \"mais espalhado, sem ter a mediana diferente. Confira no grafico_boxplot().\"),
  .default =
    paste0(\"O teste não supõe distribuição normal, mas supõe observações independentes; \",
           \"o gráfico das observações e o delineamento continuam necessários.\")
)

# 7. Poder do teste: a ClaRa ainda não o calcula para as medianas.
texto_poder <- \"\""

# Kruskal-Wallis: amostra, H, ε², Dunn, pressuposto, alerta e síntese.
receitas_textos$kruskal <- paste0(trecho_textos_medianas, "

# 8. O Kruskal-Wallis numa frase.
texto_teste <- str_glue(
  \"Em <<ROTULO_RESPOSTA>>, {evidencia} dos grupos de <<ROTULO_GRUPOS>> pelo teste \",
  \"de Kruskal-Wallis (H({teste$gl}) = {com_virgula(teste$h)}; {texto_p(teste$p)}).\"
)

# 9. Tamanho de efeito: ε² dos postos, calculado direto do H.
epsilon2 <- teste$h / (amostra$usadas - 1)
classe <- case_when(
  epsilon2 < 0.01 ~ \"muito pequeno\",
  epsilon2 < 0.06 ~ \"pequeno\",
  epsilon2 < 0.14 ~ \"médio\",
  .default        = \"grande\"
)
texto_efeito <- str_glue(
  \"O tamanho de efeito foi {classe} pelos limites usuais \",
  \"(ε² dos postos = {com_virgula(epsilon2, 3)}), uma referência estatística, não biológica.\"
)

# 10. Dunn: quais pares de grupos diferiram.
diferentes <- <<RESULTADO>>$pares |>
  filter(p_ajustado < alfa) |>
  mutate(frase = paste0(str_replace(comparacao, \"-\", \" e \"),
                        \" (\", texto_p(p_ajustado), \")\"))
texto_comparacoes <- case_when(
  teste$p >= alfa ~
    \"Como o Kruskal-Wallis não indicou diferença global, as comparações de Dunn servem só para descrição.\",
  nrow(diferentes) == 0 ~
    \"Pelo teste de Dunn (p ajustado por Holm), nenhum par de grupos diferiu ao nível adotado, apesar da diferença global indicada pelo Kruskal-Wallis.\",
  nrow(diferentes) == 1 ~
    paste0(\"Pelo teste de Dunn (p ajustado por Holm), só diferiram \", diferentes$frase[1],
           \". Grupos que compartilham uma letra não diferiram entre si.\"),
  .default =
    paste0(\"Pelo teste de Dunn (p ajustado por Holm), diferiram os pares \",
           paste(diferentes$frase, collapse = \"; \"),
           \". Grupos que compartilham uma letra não diferiram entre si.\")
)

# 11. Síntese: o resultado principal numa frase só.
texto_sintese <- str_glue(
  \"Na amostra de {amostra$usadas} observações em {nrow(resumo)} grupos, {evidencia} \",
  \"(H({teste$gl}) = {com_virgula(teste$h)}; {texto_p(teste$p)}; \",
  \"ε² = {com_virgula(epsilon2, 3)}). A interpretação deve considerar o pressuposto e o delineamento.\"
)

# 12. Todas as frases, numa lista com partes nomeadas.
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
  lapply(as.character)")

# Mann-Whitney: amostra, W com o deslocamento e o IC, r, pressuposto,
# alerta e síntese.
receitas_textos$mann_whitney <- paste0(trecho_textos_medianas, "

# 8. O Mann-Whitney numa frase, com o deslocamento e o seu intervalo.
texto_teste <- str_glue(
  \"Em <<ROTULO_RESPOSTA>>, {evidencia} de {grupos[1]} e {grupos[2]} pelo teste \",
  \"de Mann-Whitney (W = {com_virgula(teste$w, 1)}; {texto_p(teste$p)}). A mediana \",
  \"das diferenças entre uma observação de {grupos[1]} e uma de {grupos[2]} foi \",
  \"{com_virgula(teste$deslocamento)}, com IC <<NIVEL>>% de {com_virgula(teste$ic_inf)} \",
  \"a {com_virgula(teste$ic_sup)}.\"
)

# 9. Tamanho de efeito: a correlação bisserial dos postos.
r <- rank_biserial(<<RESPOSTA>> ~ <<GRUPOS>>, data = <<RESULTADO>>$dados)$r_rank_biserial
classe <- case_when(
  abs(r) < 0.1 ~ \"muito pequeno\",
  abs(r) < 0.3 ~ \"pequeno\",
  abs(r) < 0.5 ~ \"médio\",
  .default     = \"grande\"
)
texto_efeito <- str_glue(
  \"O tamanho de efeito foi {classe} pelos limites usuais \",
  \"(r bisserial dos postos = {com_virgula(r)}), uma referência estatística, não biológica.\"
)

# 10. Síntese: o resultado principal numa frase só.
texto_sintese <- str_glue(
  \"Na amostra de {amostra$usadas} observações em dois grupos, {evidencia} \",
  \"(W = {com_virgula(teste$w, 1)}; {texto_p(teste$p)}; r = {com_virgula(r)}). \",
  \"A interpretação deve considerar o pressuposto e o delineamento.\"
)

# 11. Todas as frases, numa lista com partes nomeadas.
list(
  amostra      = texto_amostra,
  teste        = texto_teste,
  efeito       = texto_efeito,
  pressupostos = texto_pressupostos,
  alerta       = texto_alerta,
  poder        = texto_poder,
  sintese      = texto_sintese
) |>
  lapply(as.character)")


# Exibição no console ----------------------------------------------------------
#
# Ao digitar o nome do resultado no console, o R chama esta função.
# Ela mostra a análise como um pequeno relatório, na ordem do estudo.

print.clara_medianas <- function(x, ...) {

  # Título e tamanho da amostra.
  cat("\nComparação de medianas:", x$nomes$rotulo_resposta, "por", x$nomes$rotulo_grupos, "\n")
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

  # O teste que a ClaRa escolheu, numa frase.
  teste <- x$teste
  if (x$nomes$analise == "mann_whitney") {
    cat("\n# Mann-Whitney\n")
    cat("Com dois grupos, a ClaRa usa o Mann-Whitney no lugar do Kruskal-Wallis.\n")
    cat("W = ", numero(teste$w, 1), "; ", escrever_p(teste$p), ": ",
        concluir_medianas(teste$p, x$nomes$confianca), "\n", sep = "")
    cat("Deslocamento ", teste$comparacao, ": ", numero(teste$deslocamento),
        " (IC ", x$nomes$confianca * 100, "%: ", numero(teste$ic_inf),
        " a ", numero(teste$ic_sup), ")\n", sep = "")
  } else {
    cat("\n# Kruskal-Wallis\n")
    cat("H(", teste$gl, ") = ", numero(teste$h), "; ", escrever_p(teste$p), ": ",
        concluir_medianas(teste$p, x$nomes$confianca), "\n", sep = "")
  }

  # O pressuposto, com a leitura do teste e o que ela muda na conclusão.
  cat("\n# Pressuposto\n")
  print(x$pressupostos)
  cat(dplyr::case_when(
    x$pressupostos$p >= 1 - x$nomes$confianca ~
      "Com dispersões parecidas, o teste compara as medianas.",
    .default =
      "As dispersões diferem: o teste compara as distribuições inteiras, não só as medianas."
  ), "\n", sep = "")

  # Os pares do Dunn, quando há três ou mais grupos.
  if (x$nomes$analise == "kruskal") {
    cat("\n# Comparações de Dunn (p ajustado por Holm)\n")
    print(x$pares)
  }

  # Um lembrete das partes que podem ser abertas com $.
  cat("\nPartes do resultado: $resumo $teste $pressupostos",
      if (x$nomes$analise == "kruskal") "$pares", "$amostra $dados\n")

  invisible(x)
}

# A conclusão da comparação, escrita conforme o p-valor.
concluir_medianas <- function(p, confianca) {
  dplyr::case_when(
    p <  1 - confianca ~ "há evidência de diferença entre os grupos.",
    p >= 1 - confianca ~ "não há evidência de diferença entre os grupos."
  )
}
