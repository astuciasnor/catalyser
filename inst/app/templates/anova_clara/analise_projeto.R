# {{TITULO_COMENTARIO}}: ROTEIRO DE ANÁLISE EM ClaRa
# x========================================================================x
# Pergunta: {{PERGUNTA_COMENTARIO}}
#
# Este roteiro faz a ANOVA de um fator com a ClaRa, o R escrito com clareza:
# cada função responde a uma pergunta da pesquisa, em português, e por trás
# dela roda R comum, no estilo do tidyverse.
#
# COMO ESTUDAR
# Rode as seções em ordem (Ctrl+Enter; sumário: Ctrl+Shift+O).
# Para entender uma função da ClaRa: ajuda(comparar_medias).
# Para ver o R por trás dela, acrescente mostrar_codigo = TRUE à chamada:
# a função imprime o código que vai rodar, com os nomes das suas colunas.
#
# Os dois relatórios em relatorios/ repetem as chamadas principais deste
# roteiro e rodam sozinhos, sem ler este arquivo. Se mudar uma escolha aqui
# (um rótulo, a confiança, um detalhe da figura), mude também nos relatórios.

# 1. Preparar o ambiente ---------------------------------------------------
library(here)
# Declara: "este arquivo está em R/analise.R, dentro do meu projeto".
# Assim, here() monta caminhos a partir da raiz do projeto.
here::i_am("R/analise.R")
{{BIBLIOTECAS_PREPARO}}
# Dois pacotes do ecossistema EAPA, hospedados no GitHub (não estão no CRAN).
# EAPADados: dados de contexto da pesca e da aquicultura do curso.
if (!requireNamespace("EAPADados", quietly = TRUE)) {
  stop(
    "Este projeto faz parte do ecossistema CatalyseR e pede o pacote complementar EAPADados para compatibilidade, mas ele não está instalado.",
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
# Apresentação de números e tabelas (fmt, formatar_p, flextable_ocean).
source(here("R", "funcoes.R"), encoding = "UTF-8")
# A ClaRa: as funções da análise, uma por pergunta da pesquisa.
source(here("R", "clara.R"), encoding = "UTF-8")

# 2. Ler a planilha --------------------------------------------------------
# A planilha que viajou no projeto entra aqui, sem nenhuma alteração.
# Sai dados_brutos, a tabela lida.
{{TRECHO_IMPORTAR}}

# 3. Preparar a base da ANOVA ----------------------------------------------
# Quatro etapas: reconstruir o preparo, conferir com a fotografia que
# acompanha o projeto, adotar a base e montar a base desta análise.
# Sai dados_da_analise, a base que a ClaRa vai analisar.
{{TRECHO_PREPARO}}

# Primeira olhada: a resposta deve ser número (dbl) e os grupos, fator (fct).
glimpse(dados_da_analise)

# 4 a 7. Comparar as médias ------------------------------------------------
# Resumo por grupo, ANOVA, pressupostos (Shapiro-Wilk e Levene), Tukey e
# letras, numa função só. Os rótulos ficam guardados no resultado: gráficos
# e textos já os usam.
resultado <- dados_da_analise |>
  comparar_medias(resposta        = {{RESPOSTA_CLARA}},
                  grupos          = {{FATOR_CLARA}},
                  rotulo_resposta = {{ROTULO_RESPOSTA_R}},
                  rotulo_grupos   = {{ROTULO_FATOR_R}},
                  confianca       = {{CONFIANCA}})

# Um pequeno relatório no console: teste, pressupostos e comparações.
resultado

# Tamanho de efeito: η² e ω², com intervalo e a leitura de Cohen.
efeito <- resultado |>
  medir_efeito()

efeito

# 8. Tabelas ---------------------------------------------------------------
# As tabelas já saem prontas do resultado. Digite cada uma no console:
resultado$resumo          # n, média, DP, EP, IC e letras por grupo
resultado$anova           # a tabela da ANOVA
resultado$pares           # as comparações de Tukey, par a par
resultado$pressupostos    # Shapiro-Wilk e Levene, com a leitura

# 9. Gráficos --------------------------------------------------------------
# Cada gráfico fica guardado com um nome, para ser salvo na seção 11.

# 9.1 Exploração: caixas com as observações por cima.
grafico_caixas <- resultado |>
  grafico_boxplot()

grafico_caixas

# 9.2 Figura principal: barras, pontos, média ± DP, IC e letras.
# A figura que vai para o artigo: todas as escolhas escritas, com as opções
# ao lado.
grafico_barras <- resultado |>
  grafico_medias(titulo           = {{TITULO_CLARA}},
                 explicacao       = TRUE,           # FALSE tira a explicação do topo
                 haste            = "ic",           # "ic", "ep" ou "dp"
                 mostrar_barras   = TRUE,
                 largura_barras   = 0.3,            # de 0 a 1
                 mostrar_pontos   = TRUE,
                 mostrar_media_dp = TRUE,
                 casas            = 2,              # casas decimais do rótulo
                 mostrar_letras   = TRUE,
                 cores            = "ocean",        # "ocean", "cinza" ou um vetor
                 tamanho_texto    = 12,
                 fonte            = "sans")         # "sans" (Arial) ou "serif" (Times)

grafico_barras

# 9.3 Resíduos contra ajustados: faixas de alturas parecidas, variâncias parecidas.
grafico_ajustados <- resultado |>
  grafico_residuos()

grafico_ajustados

# 9.4 Q-Q: pontos perto da reta, resíduos compatíveis com a normal.
grafico_normal <- resultado |>
  grafico_qq()

grafico_normal

# 9.5 Diferenças entre pares: haste que cruza a linha tracejada (zero),
#     sem diferença entre os dois grupos.
grafico_diferencas <- resultado |>
  grafico_pares()

grafico_diferencas

# 10. Textos para o relatório ----------------------------------------------
# Frases que mudam junto com os dados. No Quarto: `r textos$teste`.
textos <- resultado |>
  escrever_resultados()

textos

# 11. Salvar cópias --------------------------------------------------------
# CSV com ponto e vírgula e vírgula decimal abre bem no Excel em português.
# São cópias para compartilhar: os relatórios não leem estes arquivos.
dir.create(here("saida", "tabelas"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("saida", "figuras"), recursive = TRUE, showWarnings = FALSE)

tabelas <- list(resumo_grupos       = resultado$resumo,
                anova               = resultado$anova,
                tukey               = resultado$pares,
                testes_pressupostos = resultado$pressupostos,
                tamanho_efeito      = efeito)

# Uma volta para cada tabela: resumo_grupos.csv, anova.csv...
for (nome in names(tabelas)) {
  write.csv2(tabelas[[nome]],
             here("saida", "tabelas", paste0(nome, ".csv")),
             row.names    = FALSE,
             fileEncoding = "UTF-8")
}

figuras <- list(barras   = grafico_barras,
                boxplot  = grafico_caixas,
                pares    = grafico_diferencas,
                residuos = grafico_ajustados,
                qq       = grafico_normal)

# Uma volta para cada figura: barras.png, boxplot.png...
for (nome in names(figuras)) {
  ggsave(here("saida", "figuras", paste0(nome, ".png")),
         plot   = figuras[[nome]],
         width  = 7,
         height = 4.6,
         dpi    = 300,
         bg     = "white")
}

# 12. Registrar o ambiente -------------------------------------------------
# As versões do R, dos pacotes e da ClaRa usadas nesta execução.
writeLines(c(paste("ClaRa", versao_clara), capture.output(sessionInfo())),
           here("saida", "sessionInfo.txt"))
