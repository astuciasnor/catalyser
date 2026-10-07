# =============================================================================
#  ClaRa: para qualquer análise
# =============================================================================
#
#  medir_efeito(), escrever_resultados(), grafico_residuos() e grafico_qq()
#  agem sobre o resultado de qualquer análise da ClaRa. Aqui ficam só as
#  funções e as listas de receitas, vazias; o arquivo de cada pergunta
#  (clara_medias.R...) acrescenta as suas: receitas_efeito$anova <- "...".
#  registrar_ambiente() fecha qualquer roteiro, gravando as versões usadas.
#
#  Carregado por R/clara.R; no roteiro, basta source("R/clara.R").
# =============================================================================


# Para qualquer análise --------------------------------------------------------
#
# Estas funções agem sobre o resultado de qualquer análise da ClaRa. Cada
# uma olha qual análise o resultado guarda e usa a receita daquela análise.
# Uma análise nova não cria funções aqui: só acrescenta receitas às listas.

# medir_efeito() ---------------------------------------------------------------
#
# Pergunta: o efeito encontrado é grande ou pequeno?
#
# O p-valor diz se há evidência de diferença; o tamanho de efeito diz quanto
# ela pesa. A medida depende da análise feita em comparar_medias():
#   ANOVA ....... η² é a fração da variação da resposta que acompanha os
#                 grupos; ω² corrige o exagero do η² em amostras pequenas.
#   teste t ..... d de Cohen é a diferença das médias medida em desvios
#                 padrão; g de Hedges corrige o exagero do d em amostras
#                 pequenas. O sinal segue a ordem da comparação.
# A leitura segue a convenção de Cohen, uma referência estatística, não
# biológica.
#
# Argumentos:
#   resultado ........... o que comparar_medias() ou comparar_medianas()
#                         devolveu
#   mostrar_codigo ...... TRUE imprime o código R antes do resultado
#
# Devolve uma tabela: medida, valor, intervalo de confiança e leitura.
#
# Exemplo:  resultado |> medir_efeito()
#
medir_efeito <- function(resultado,
                         mostrar_codigo = FALSE) {

  # Guardamos o nome do resultado, como o aluno o chamou.
  nome_resultado <- nome_do_objeto(substitute(resultado), padrao = "resultado")

  # Trocamos os marcadores da receita da análise feita pelos nomes dela.
  receita <- preencher_receita(escolher_receita(receitas_efeito, resultado, "medir_efeito"), list(
    RESULTADO         = nome_resultado,
    RESPOSTA          = resultado$nomes$resposta,
    GRUPOS            = resultado$nomes$grupos,
    CONFIANCA         = resultado$nomes$confianca,
    VARIANCIAS_IGUAIS = resultado$nomes$variancias_iguais
  ))

  # Rodamos a receita, mostrando o código antes, se o aluno pediu.
  executar_receita(
    receita        = receita,
    objetos        = rlang::set_names(list(resultado), nome_resultado),
    pacotes        = c("dplyr", "effectsize"),
    mostrar_codigo = mostrar_codigo
  )
}

# As receitas de medir_efeito(), uma para cada análise. Cada arquivo de
# pergunta acrescenta as suas.
receitas_efeito <- list()


# escrever_resultados() --------------------------------------------------------
#
# Pergunta: como contar o resultado em frases, para o relatório?
#
# Escreve as frases de um relatório a partir do resultado: a amostra, o
# teste, o tamanho de efeito, as comparações entre pares (ANOVA), os
# pressupostos, um alerta sobre o que pede cuidado e, quando muda a leitura,
# o poder do teste. Os números e as conclusões vêm do resultado: se os dados
# mudam, as frases mudam junto.
#
# É uma função só para todas as análises. Ela olha qual análise o resultado
# guarda (ANOVA ou teste t) e escolhe a receita daquela análise. Por isso,
# com mostrar_codigo = TRUE, aparece só o código da análise que você fez,
# nunca o das outras. Quando a ClaRa ganhar uma análise nova, a função
# continua a mesma: só a lista de receitas cresce.
#
# Argumentos:
#   resultado ........... o que comparar_medias() ou comparar_medianas()
#                         devolveu
#   rotulo_resposta ..... como chamar a resposta no texto (ex.: "Peso (g)");
#                         sem ele, vai o da análise
#   rotulo_grupos ....... como chamar os grupos no texto (ex.: "Tipo de Ração");
#                         sem ele, vai o da análise
#   mostrar_codigo ...... TRUE imprime o código R antes das frases
#
# Devolve uma lista de frases. Cada parte se abre com $:
#   textos$amostra, textos$teste, textos$efeito, textos$pressupostos,
#   textos$alerta, textos$poder (vazio quando não se aplica), textos$sintese
#   e, na ANOVA, textos$comparacoes.
# No Quarto, a frase entra no meio do parágrafo com `r textos$teste`.
#
# Exemplo:
#   textos <- resultado |>
#     escrever_resultados(rotulo_resposta = "Peso seco (g)",
#                         rotulo_grupos   = "Tratamento")
#
escrever_resultados <- function(resultado,
                                rotulo_resposta = NULL,
                                rotulo_grupos   = NULL,
                                mostrar_codigo  = FALSE) {

  # Guardamos o nome do resultado, como o aluno o chamou.
  nome_resultado <- nome_do_objeto(substitute(resultado), padrao = "resultado")

  # Sem rótulo informado aqui, valem os que a análise guardou.
  rotulo_resposta <- ou_entao(rotulo_resposta, resultado$nomes$rotulo_resposta)
  rotulo_grupos   <- ou_entao(rotulo_grupos,   resultado$nomes$rotulo_grupos)

  # Trocamos os marcadores da receita da análise feita pelos nomes dela.
  receita <- preencher_receita(escolher_receita(receitas_textos, resultado, "escrever_resultados"), list(
    RESULTADO         = nome_resultado,
    RESPOSTA          = resultado$nomes$resposta,
    GRUPOS            = resultado$nomes$grupos,
    ROTULO_RESPOSTA   = rotulo_resposta,
    ROTULO_GRUPOS     = rotulo_grupos,
    ALFA              = 1 - resultado$nomes$confianca,
    NIVEL             = resultado$nomes$confianca * 100,
    VARIANCIAS_IGUAIS = resultado$nomes$variancias_iguais,
    NOME_T            = dplyr::case_when(isTRUE(resultado$nomes$variancias_iguais) ~ "de Student",
                                         .default = "de Welch")
  ))

  # Rodamos a receita, mostrando o código antes, se o aluno pediu.
  textos <- executar_receita(
    receita        = receita,
    objetos        = rlang::set_names(list(resultado), nome_resultado),
    pacotes        = c("dplyr", "stringr", "effectsize", "pwr"),
    mostrar_codigo = mostrar_codigo
  )

  # Damos às frases uma etiqueta, para o console saber como exibi-las.
  structure(textos, class = "clara_textos")
}

# As receitas de escrever_resultados(), uma para cada análise. Cada arquivo
# de pergunta acrescenta as suas.
receitas_textos <- list()


# grafico_residuos() -----------------------------------------------------------
#
# Pergunta: as variâncias dos grupos são parecidas?
#
# Resíduos contra os valores ajustados (as médias dos grupos): cada grupo
# forma uma faixa vertical. Faixas de alturas parecidas indicam variâncias
# parecidas; uma faixa muito mais alta que as outras pede cautela. O gráfico
# completa o teste de Levene: o teste dá um p, o gráfico mostra qual grupo
# se afasta e quanto.
#
# Argumentos:
#   resultado ........... o que comparar_medias() devolveu
#   rotulo_grupos ....... nome dos grupos na legenda; sem ele, vai o da análise
#   mostrar_codigo ...... TRUE imprime o código R antes do gráfico
#
# Devolve um gráfico do ggplot2; ajustes extras entram com +.
#
# Exemplo:
#   resultado |>
#     grafico_residuos(rotulo_grupos = "Tipo de Ração")
#
grafico_residuos <- function(resultado,
                             rotulo_grupos  = NULL,
                             mostrar_codigo = FALSE) {

  # Guardamos o nome do resultado, como o aluno o chamou.
  nome_resultado <- nome_do_objeto(substitute(resultado), padrao = "resultado")

  # Sem rótulo informado aqui, vale o que a análise guardou.
  rotulo_grupos <- ou_entao(rotulo_grupos, resultado$nomes$rotulo_grupos)

  # Trocamos os marcadores da receita da análise feita.
  receita <- preencher_receita(escolher_receita(receitas_residuos, resultado, "grafico_residuos"), list(
    RESULTADO     = nome_resultado,
    RESPOSTA      = resultado$nomes$resposta,
    GRUPOS        = resultado$nomes$grupos,
    ROTULO_GRUPOS = rotulo_grupos
  ))

  # Rodamos a receita, mostrando o código antes, se o aluno pediu.
  executar_receita(
    receita        = receita,
    objetos        = rlang::set_names(list(resultado), nome_resultado),
    pacotes        = c("dplyr", "ggplot2"),
    mostrar_codigo = mostrar_codigo
  )
}


# grafico_qq() -----------------------------------------------------------------
#
# Pergunta: os resíduos são compatíveis com a distribuição normal?
#
# Gráfico Q-Q: os resíduos contra os quantis da distribuição normal. Pontos
# perto da reta indicam resíduos compatíveis com a normal; caudas que se
# afastam indicam assimetria ou valores extremos. Na ANOVA, usa os resíduos
# padronizados do modelo; no teste t, há um painel por grupo, como o
# Shapiro-Wilk em cada grupo. O gráfico completa o Shapiro-Wilk: o teste dá
# um p, o gráfico mostra onde está o desvio e de que tamanho ele é.
#
# Argumentos:
#   resultado ........... o que comparar_medias() devolveu
#   mostrar_codigo ...... TRUE imprime o código R antes do gráfico
#
# Devolve um gráfico do ggplot2; ajustes extras entram com +.
#
# Exemplo:
#   resultado |>
#     grafico_qq()
#
grafico_qq <- function(resultado,
                       mostrar_codigo = FALSE) {

  # Guardamos o nome do resultado, como o aluno o chamou.
  nome_resultado <- nome_do_objeto(substitute(resultado), padrao = "resultado")

  # Trocamos os marcadores da receita da análise feita.
  receita <- preencher_receita(escolher_receita(receitas_qq, resultado, "grafico_qq"), list(
    RESULTADO = nome_resultado,
    RESPOSTA  = resultado$nomes$resposta,
    GRUPOS    = resultado$nomes$grupos
  ))

  # Rodamos a receita, mostrando o código antes, se o aluno pediu.
  executar_receita(
    receita        = receita,
    objetos        = rlang::set_names(list(resultado), nome_resultado),
    pacotes        = c("dplyr", "ggplot2"),
    mostrar_codigo = mostrar_codigo
  )
}

# Os trechos das receitas de grafico_residuos() e grafico_qq(): o preparo
# dos resíduos depende da análise e mora no arquivo de cada pergunta; o
# desenho dos resíduos contra os ajustados serve a todas e mora aqui.
trechos_diagnostico <- list()

# Resíduos contra ajustados: o mesmo gráfico para as duas análises.
trechos_diagnostico$residuos <- "
# Resíduos contra os ajustados: uma faixa vertical por grupo.
ggplot(diagnostico, aes(x = ajustado, y = residuo, colour = <<GRUPOS>>)) +
  geom_hline(yintercept = 0, linetype = \"dashed\", colour = \"grey40\") +
  geom_point(size = 2.2, alpha = 0.7) +
  scale_colour_manual(values = cores) +
  labs(
    title  = \"Faixas de alturas parecidas indicam variâncias parecidas entre os grupos.\",
    x      = \"Valores ajustados (médias dos grupos)\",
    y      = \"Resíduos\",
    colour = \"<<ROTULO_GRUPOS>>\"
  ) +
  theme_classic(base_size = 12) +
  theme(plot.title = element_text(size = 10, colour = \"grey30\"),
        plot.title.position = \"plot\")"

# As receitas de grafico_residuos() e grafico_qq(), uma por análise. Cada
# arquivo de pergunta acrescenta as suas, montadas com os trechos.
receitas_residuos <- list()
receitas_qq       <- list()


# registrar_ambiente() ---------------------------------------------------------
#
# Pergunta: com que versões do R e dos pacotes esta análise rodou?
#
# Grava num arquivo de texto a versão da ClaRa, a do R, o sistema e a versão
# de cada pacote carregado. Se, daqui a um ano ou em outro computador, um
# número não bater, este arquivo mostra o que mudou. Chame no fim do
# roteiro, depois de tudo rodar: assim todos os pacotes usados aparecem.
#
# Argumentos:
#   arquivo ............. onde gravar; a pasta é criada se ainda não existir
#   mostrar_codigo ...... TRUE imprime o código R antes de gravar
#
# Devolve o caminho do arquivo, sem imprimi-lo.
#
# Exemplo:  registrar_ambiente(arquivo = here("saida", "sessionInfo.txt"))
#
registrar_ambiente <- function(arquivo        = "saida/sessionInfo.txt",
                               mostrar_codigo = FALSE) {

  # O caminho entra na receita entre aspas, como o aluno o escreveria.
  receita <- preencher_receita(receita_ambiente, list(
    ARQUIVO = encodeString(arquivo, quote = "\"")
  ))

  # Rodamos a receita, mostrando o código antes, se o aluno pediu. A versão
  # da ClaRa vai junto, porque a receita a escreve na primeira linha.
  executar_receita(
    receita        = receita,
    objetos        = list(versao_clara = versao_clara),
    pacotes        = character(),
    mostrar_codigo = mostrar_codigo
  )

  # Avisamos onde ficou o registro.
  message("Ambiente registrado em ", arquivo, ".")
  invisible(arquivo)
}

# A receita de registrar_ambiente(): a mesma para qualquer análise.
receita_ambiente <- "
# 1. A pasta do arquivo, criada se ainda não existir.
dir.create(dirname(<<ARQUIVO>>), recursive = TRUE, showWarnings = FALSE)

# 2. A versão da ClaRa e o retrato da sessão: R, sistema e pacotes carregados.
ambiente <- c(paste(\"ClaRa\", versao_clara), capture.output(sessionInfo()))

# 3. As linhas, gravadas no arquivo de texto.
writeLines(ambiente, <<ARQUIVO>>)"


# Exibição no console ----------------------------------------------------------
# As frases de escrever_resultados(), uma parte por bloco, na largura da tela.
print.clara_textos <- function(x, ...) {

  # Título.
  cat("\nTextos para o relatório\n=======================\n")

  # Cada frase com o nome da parte; partes vazias (como $poder) não aparecem.
  for (parte in names(x)) {
    if (nzchar(x[[parte]])) {
      cat("\n$", parte, "\n", sep = "")
      cat(strwrap(x[[parte]], width = 76), sep = "\n")
    }
  }

  # Como levar uma frase para o Quarto.
  cat("\nNo Quarto, use dentro do texto: `r textos$teste` (troque textos pelo nome do seu objeto).\n")

  invisible(x)
}
