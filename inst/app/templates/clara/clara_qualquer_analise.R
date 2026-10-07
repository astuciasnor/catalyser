# =============================================================================
#  ClaRa: para qualquer análise
# =============================================================================
#
#  medir_efeito(), escrever_resultados(), grafico_residuos() e grafico_qq()
#  agem sobre o resultado de qualquer análise da ClaRa. Aqui ficam só as
#  funções e as listas de receitas, vazias; o arquivo de cada pergunta
#  (clara_medias.R...) acrescenta as suas: receitas_efeito$anova <- "...".
#  salvar_tabelas() e salvar_figuras() guardam cópias em CSV e PNG, e
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


# salvar_tabelas() -------------------------------------------------------------
#
# Pergunta: como levar as tabelas da análise para o Excel?
#
# Grava cada tabela num arquivo CSV, na pasta indicada. O nome escrito antes
# do = vira o nome do arquivo: anova = resultado$anova grava anova.csv. O CSV
# sai com ponto e vírgula entre as colunas e vírgula decimal, o formato que o
# Excel em português abre direto. A pasta é criada se ainda não existir, e um
# arquivo com o mesmo nome é substituído.
#
# Argumentos:
#   ... ................. as tabelas, cada uma com o nome do seu arquivo
#   pasta ............... onde gravar; criada se ainda não existir
#   mostrar_codigo ...... TRUE imprime o código R antes de gravar
#
# Devolve os caminhos dos arquivos, sem imprimi-los.
#
# Exemplo:  salvar_tabelas(resumo_grupos = resultado$resumo,
#                          anova         = resultado$anova,
#                          pasta         = here("saida", "tabelas"))
#
salvar_tabelas <- function(...,
                           pasta          = "saida/tabelas",
                           mostrar_codigo = FALSE) {

  # Conferimos as escolhas antes de gravar qualquer arquivo.
  conferir_pasta(pasta)
  conferir_sim_ou_nao(mostrar_codigo, "mostrar_codigo")
  itens <- itens_para_salvar(rlang::enquos(...), "uma tabela",
                             "anova = resultado$anova", is.data.frame)

  # A receita: a pasta, e uma linha write.csv2() por tabela, com o código que
  # o aluno escreveu (resultado$anova) e o nome que ele deu ao arquivo.
  receita <- paste(c(
    "# 1. A pasta das tabelas, criada se ainda não existir.",
    paste("pasta <-", encodeString(pasta, quote = "\"")),
    "dir.create(pasta, recursive = TRUE, showWarnings = FALSE)",
    "",
    "# 2. Uma tabela por arquivo CSV, com ponto e vírgula e vírgula decimal.",
    sprintf("write.csv2(%s, file.path(pasta, \"%s.csv\"),\n           row.names = FALSE, fileEncoding = \"UTF-8\")",
            itens$codigos, itens$nomes)
  ), collapse = "\n")

  # Rodamos a receita a partir de onde o aluno chamou a função: é lá que
  # moram os objetos dele.
  executar_receita(
    receita           = receita,
    objetos           = list(),
    pacotes           = character(),
    mostrar_codigo    = mostrar_codigo,
    ambiente_do_aluno = parent.frame()
  )

  # Avisamos o que foi gravado e onde.
  arquivos <- paste0(itens$nomes, ".csv")
  message("Tabelas salvas em ", pasta, ": ", toString(arquivos), ".")
  invisible(file.path(pasta, arquivos))
}


# salvar_figuras() -------------------------------------------------------------
#
# Pergunta: como guardar os gráficos em arquivos de imagem?
#
# Grava cada gráfico num arquivo PNG, na pasta indicada, com fundo branco e
# resolução de impressão. O nome escrito antes do = vira o nome do arquivo:
# barras = grafico_barras grava barras.png. A pasta é criada se ainda não
# existir, e um arquivo com o mesmo nome é substituído.
#
# Argumentos:
#   ... ................. os gráficos, cada um com o nome do seu arquivo
#   pasta ............... onde gravar; criada se ainda não existir
#   largura ............. largura da imagem, em centímetros
#   altura .............. altura da imagem, em centímetros
#   resolucao ........... pontos por polegada (300 serve para impressão)
#   mostrar_codigo ...... TRUE imprime o código R antes de gravar
#
# Devolve os caminhos dos arquivos, sem imprimi-los.
#
# Exemplo:  salvar_figuras(barras  = grafico_barras,
#                          pasta   = here("saida", "figuras"),
#                          largura = 18,
#                          altura  = 12)
#
salvar_figuras <- function(...,
                           pasta          = "saida/figuras",
                           largura        = 18,
                           altura         = 12,
                           resolucao      = 300,
                           mostrar_codigo = FALSE) {

  # Conferimos as escolhas antes de gravar qualquer arquivo.
  conferir_pasta(pasta)
  conferir_sim_ou_nao(mostrar_codigo, "mostrar_codigo")
  for (medida in c("largura", "altura", "resolucao")) {
    valor <- get(medida)
    if (!is.numeric(valor) || length(valor) != 1 || is.na(valor) || valor <= 0) {
      stop(medida, " aceita um número maior que zero.", call. = FALSE)
    }
  }
  itens <- itens_para_salvar(rlang::enquos(...), "um gráfico",
                             "barras = grafico_barras", ggplot2::is_ggplot)

  # A receita: a pasta, e uma linha ggsave() por gráfico, com o objeto que o
  # aluno escreveu e o nome que ele deu ao arquivo.
  receita <- paste(c(
    "# 1. A pasta das figuras, criada se ainda não existir.",
    paste("pasta <-", encodeString(pasta, quote = "\"")),
    "dir.create(pasta, recursive = TRUE, showWarnings = FALSE)",
    "",
    "# 2. Um gráfico por arquivo PNG, com fundo branco; medidas em centímetros.",
    sprintf("ggsave(file.path(pasta, \"%s.png\"), plot = %s,\n       width = %s, height = %s, units = \"cm\", dpi = %s, bg = \"white\")",
            itens$nomes, itens$codigos, format(largura), format(altura), format(resolucao))
  ), collapse = "\n")

  # Rodamos a receita a partir de onde o aluno chamou a função.
  executar_receita(
    receita           = receita,
    objetos           = list(),
    pacotes           = "ggplot2",
    mostrar_codigo    = mostrar_codigo,
    ambiente_do_aluno = parent.frame()
  )

  # Avisamos o que foi gravado e onde.
  arquivos <- paste0(itens$nomes, ".png")
  message("Figuras salvas em ", pasta, ": ", toString(arquivos), ".")
  invisible(file.path(pasta, arquivos))
}


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
