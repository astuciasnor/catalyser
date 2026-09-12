# {{TITULO}} - a análise, comentada -------------------------------------------|

# COMO RODAR
# Reinicie o R (Session > Restart R) e execute as linhas em ordem, com
# Ctrl+Enter; o resultado aparece no Console, no Plots ou no Viewer.
# Ctrl+Shift+O abre o menu de seções e lista todos os trechos.

# SCRIPT E RELATÓRIO
# Este script é uma cópia comentada, para estudo e rascunho. A fonte da verdade
# é o relatorio.qmd: é lá que o código roda e é lá que você edita o que entra no
# relatório. Os trechos abaixo, marcados com "## ---- nome ----", correspondem
# aos chunks do relatório pelo nome. Nesta versão não há sincronização
# automática: se mudar algo aqui, leve a mudança ao chunk correspondente à mão.

# PEQUENO VOCABULÁRIO
# <- guarda um resultado em um objeto; |> passa o resultado à próxima função.
# mutate()/summarise() criam colunas; filter() escolhe linhas; select() colunas.
# ~ escreve uma relação em modelos; $ e [[ ]] acessam um componente nomeado.
# NA indica ausência de dado, não zero.

# As funções do projeto estão em R/funcoes.R: resumir_grupo(), tema_projeto(),
# cores_tratamento, fmt(), formatar_p() e flextable_ocean().

# ----------------------------------------------------------------------------|
## ---- instalar ----

# Instala só os pacotes que ainda faltam. Rode uma única vez, ao preparar um
# computador novo, antes do primeiro Render.
# No relatório este chunk é eval: false, então nunca instala nada no Render.

pacotes <- c(
  "here", "readxl", "dplyr", "tidyr", "ggplot2",
  "car", "multcompView", "effectsize", "flextable", "stringr"
)

faltando <- pacotes[!pacotes %in% rownames(installed.packages())]

if (length(faltando)) {
  install.packages(faltando)
}

# ----------------------------------------------------------------------------|
## ---- pacotes ----

# Carrega os pacotes e as funções próprias. Rode antes de qualquer etapa.

# here() monta os caminhos a partir da raiz do projeto (onde está o .Rproj),
# para o mesmo arquivo funcionar em computadores diferentes.

library(here)
library(readxl)        # leitura da planilha
library(dplyr)         # manipulação de dados
library(tidyr)         # reorganização de dados
library(ggplot2)       # gráficos
library(car)           # teste de Levene
library(multcompView)  # letras dos grupos após o Tukey
library(effectsize)    # tamanho de efeito (eta² e omega²)
library(flextable)     # tabelas para o Word

# Funções próprias: resumos, formatação, tema e tabelas.
source(here("R", "funcoes.R"))

# Três algarismos significativos deixam as saídas exploratórias mais limpas.
# As tabelas finais usam fmt() e formatar_p() e não dependem desta opção.
options(digits = 3)
nivel_confianca <- {{NIVEL_CONFIANCA}}

# ----------------------------------------------------------------------------|
## ---- importar ----

# Lê a planilha como ela veio, sem mexer em nada. Sai dados_brutos.

# A planilha é somente-leitura: nunca a edite. Conferir a aba é uma medida
# simples contra erros no nome da planilha.

arquivo <- here("dados", "brutos", {{PLANILHA_R}})

excel_sheets(arquivo)

dados_brutos <- read_excel(arquivo, sheet = {{ABA_R}})

# ----------------------------------------------------------------------------|
## ---- conferir-importacao ----

# Confere como o R interpretou cada coluna. Tipo errado aqui compromete a
# análise adiante. glimpse() mostra uma coluna por linha, com o tipo ao lado.

glimpse(dados_brutos)

# ----------------------------------------------------------------------------|
## ---- tratar ----

# Reproduz o preparo confirmado e recebe a base escolhida para esta ANOVA.
# A resposta {{RESPOSTA}} já deve ser numérica, como na IDE.

# Observações sem a resposta viram NA e ficam de fora do teste, que usa apenas
# os casos completos. A planilha original nunca é alterada.

{{PREPARO}}

# A análise usa apenas observações com resposta e grupo preenchidos.
# Guarda a base preparada antes da exclusão de casos incompletos da ANOVA.
base_da_anova <- dados
if (!is.numeric(dados${{RESPOSTA_R}})) {
  stop("A resposta precisa ser numérica. Confira a tipagem no preparo antes da ANOVA.")
}
n_preparadas <- nrow(dados)
dados <- dados |>
  mutate(
    {{FATOR_R}} = factor({{FATOR_R}})
  ) |>
  tidyr::drop_na({{FATOR_R}}, {{RESPOSTA_R}}) |>
  droplevels()
n_excluidas <- n_preparadas - nrow(dados)

# Mantém a paleta Ocean; amplia as cores quando há mais grupos na análise.
n_grupos <- nlevels(dados${{FATOR_R}})
cores_grupos <- if (n_grupos <= length(cores_tratamento)) {
  cores_tratamento[seq_len(n_grupos)]
} else {
  grDevices::colorRampPalette(cores_tratamento)(n_grupos)
}

# ----------------------------------------------------------------------------|
## ---- conferir-dados ----

# Procura NA inesperado, valor absurdo ou grupo incompleto na base tratada.

glimpse(dados)
summary(dados)

count(dados, {{FATOR_R}})

# ----------------------------------------------------------------------------|
## ---- explora-resumo ----

# Compara tamanho, centro e dispersão da resposta entre os grupos.

# O QUE CONFERIR:
#   - o n de cada grupo e se há NA na resposta;
#   - a distância entre as médias comparada à dispersão dentro de cada grupo;
#   - dispersões parecidas entre grupos (senão, atenção ao teste de Levene).

resumir_grupo(dados, {{RESPOSTA_R}}, {{FATOR_R}}, conf = nivel_confianca)

# ----------------------------------------------------------------------------|
## ---- explora-boxplot ----

# Boxplot com as observações por cima, para ver forma, dispersão e extremos.

# O QUE CONFERIR:
#   - caixas de tamanho parecido (variâncias homogêneas);
#   - nenhum ponto muito fora da sua caixa (possível erro de registro);
#   - a mediana sugere diferença? A ANOVA compara médias, não medianas.

ggplot(dados, aes(x = {{FATOR_R}}, y = {{RESPOSTA_R}})) +
  geom_boxplot(width = 0.5, outlier.shape = NA, colour = "grey50") +
  geom_jitter(width = 0.1, height = 0, size = 1.5, alpha = 0.5) +
  labs(
    x = {{ROTULO_X_R}},
    y = {{ROTULO_Y_R}},
    title = "Exploração dos grupos"
  ) +
  tema_projeto()

# ----------------------------------------------------------------------------|
## ---- analisar ----

# Ajusta a ANOVA de um fator: a média da resposta difere entre os grupos? A
# ANOVA responde à pergunta global; ainda não diz quais grupos diferem entre
# si. Casos com NA na resposta saem sozinhos (na.omit padrão).

modelo <- aov({{RESPOSTA_R}} ~ {{FATOR_R}}, data = dados)
tabela_anova <- anova(modelo)
tabela_anova

# ----------------------------------------------------------------------------|
## ---- analisar-pressupostos ----

# Normalidade dos resíduos (Shapiro) e homogeneidade das variâncias (Levene).
# p > 0,05 significa que não há evidência para rejeitar o pressuposto avaliado;
# não é prova de que ele seja verdadeiro.

residuos <- residuals(modelo)
teste_normalidade <- if (length(residuos) >= 3 && length(residuos) <= 5000 && sd(residuos) > 0) {
  shapiro.test(residuos)
} else {
  list(statistic = c(W = NA_real_), p.value = NA_real_)
}
teste_levene <- leveneTest({{RESPOSTA_R}} ~ {{FATOR_R}}, data = dados)

teste_normalidade
teste_levene

# ----------------------------------------------------------------------------|
## ---- analisar-tukey ----

# Compara os grupos par a par pelo teste de Tukey. Uma letra compartilhada
# indica que não se detectou diferença a 5 %; os p-valores são ajustados.

tukey <- TukeyHSD(modelo, conf.level = nivel_confianca)
tukey

# A matriz identifica os pares sem confundir hífens nos nomes dos grupos.
grupo_nomes <- levels(dados${{FATOR_R}})
pares <- combn(grupo_nomes, 2)
p_pares <- matrix(1, length(grupo_nomes), length(grupo_nomes),
                  dimnames = list(grupo_nomes, grupo_nomes))
p_ajustados <- tukey[[1]][paste(pares[2, ], pares[1, ], sep = "-"), "p adj"]
p_pares[cbind(pares[1, ], pares[2, ])] <- p_ajustados
p_pares[cbind(pares[2, ], pares[1, ])] <- p_ajustados
ordem <- names(sort(tapply(dados${{RESPOSTA_R}}, dados${{FATOR_R}}, mean), decreasing = TRUE))
letras <- multcompView::multcompLetters(p_pares[ordem, ordem])$Letters

resumo <- resumir_grupo(dados, {{RESPOSTA_R}}, {{FATOR_R}}, conf = nivel_confianca) |>
  mutate(letra = letras[as.character({{FATOR_R}})]) |>
  arrange({{FATOR_R}})

resumo

# ----------------------------------------------------------------------------|
## ---- preparar-resultados-texto ----

# Extrai, uma única vez, os números citados no texto dos Resultados.
# fmt() e formatar_p() aplicam vírgula decimal e a escrita convencional de p.

f_valor   <- fmt(tabela_anova$`F value`[1])
gl_trat   <- tabela_anova$Df[1]
gl_res    <- tabela_anova$Df[2]
p_anova   <- formatar_p(tabela_anova$`Pr(>F)`[1], no_texto = TRUE)
p_shapiro <- formatar_p(teste_normalidade$p.value, no_texto = TRUE)
p_levene  <- formatar_p(teste_levene$`Pr(>F)`[1], no_texto = TRUE)

# A conclusão acompanha o teste, sem afirmar diferença quando ela não apareceu.
frase_anova <- if (is.na(tabela_anova$`Pr(>F)`[1]))
  "A ANOVA não forneceu um p-valor válido para comparar as médias de {{RESPOSTA}} entre os grupos" else
  if (tabela_anova$`Pr(>F)`[1] < 0.05)
    "Houve evidência de diferença na média de {{RESPOSTA}} entre os grupos" else
    "Não houve evidência de diferença na média de {{RESPOSTA}} entre os grupos"

# Tamanho de efeito: a fração da variação de {{RESPOSTA}} associada a {{FATOR}}.
# omega² corrige o viés do eta² em amostras pequenas.
efeito_eta   <- eta_squared(modelo)
efeito_omega <- omega_squared(modelo)
eta2   <- fmt(efeito_eta$Eta2[1])
omega2 <- fmt(efeito_omega$Omega2[1])

# Leitura do efeito pela convenção de Cohen (η²): abaixo de 0,01 muito pequeno,
# abaixo de 0,06 pequeno, abaixo de 0,14 médio e, a partir daí, grande.
# É referência estatística, não biológica, como na tela da CatalyseR.
eta_val <- efeito_eta$Eta2[1]
classe_efeito <- if (is.na(eta_val)) "indeterminado" else
  if (eta_val < 0.01) "muito pequeno" else
  if (eta_val < 0.06) "pequeno" else if (eta_val < 0.14) "médio" else "grande"

# Frases dos pressupostos, escritas conforme o resultado de cada teste, para o
# texto nunca afirmar o que os dados não sustentam.
normal_ok <- !is.na(teste_normalidade$p.value) && teste_normalidade$p.value >= 0.05
levene_ok <- !is.na(teste_levene$`Pr(>F)`[1]) && teste_levene$`Pr(>F)`[1] >= 0.05
frase_normalidade <- if (is.na(teste_normalidade$p.value))
  "o teste de normalidade não foi calculado; use o gráfico Q-Q" else if (normal_ok)
  "não houve evidência para rejeitar a normalidade dos resíduos" else
  "houve evidência de afastamento da normalidade dos resíduos"
frase_levene <- if (is.na(teste_levene$`Pr(>F)`[1]))
  "o teste de Levene não forneceu um p-valor válido" else if (levene_ok)
  "não houve evidência para rejeitar a igualdade das variâncias entre os grupos" else
  "houve evidência de variâncias diferentes entre os grupos"

# ----------------------------------------------------------------------------|
## ---- diagnostico-variancia ----

# Resíduos versus valores ajustados.
# O QUE CONFERIR: nuvem de mesma altura entre grupos; a linha perto de zero,
# sem curva; um funil indica variância heterogênea.

plot(modelo, which = 1)

# ----------------------------------------------------------------------------|
## ---- diagnostico-normalidade ----

# Gráfico quantil-quantil dos resíduos.
# O QUE CONFERIR: os pontos seguindo a linha; um "S" ou uma cauda que se
# descola é sinal de assimetria ou de valor extremo.

plot(modelo, which = 2)

# ----------------------------------------------------------------------------|
## ---- diagnostico-influencia ----

# Distância de Cook: observações com maior influência sobre o modelo.
# O QUE CONFERIR: picos que merecem revisão do registro e do contexto da coleta.
# Um ponto influente não deve ser excluído automaticamente.

plot(modelo, which = 4)

# ----------------------------------------------------------------------------|
## ---- tbl-resumo ----

# Estatísticas descritivas e os grupos do teste de Tukey.

resumo |>
  mutate(
    `IC {{IC_PERCENTUAL}} %` = paste0(fmt(ic_inf, 1), " a ", fmt(ic_sup, 1)),
    across(c(media, dp, ep), ~ fmt(.x, 1))
  ) |>
  select(
    {{FATOR_R}} = {{FATOR_R}},
    n,
    `Média` = media,
    DP = dp,
    EP = ep,
    `IC {{IC_PERCENTUAL}} %`,
    Tukey = letra
  ) |>
  flextable_ocean()

# ----------------------------------------------------------------------------|
## ---- tbl-anova ----

# A saída da ANOVA como tabela de artigo científico.

tabela_anova |>
  as.data.frame() |>
  mutate(
    Fonte = c({{FATOR_STRING}}, "Resíduo"),
    GL = Df,
    SQ = fmt(`Sum Sq`, 1),
    QM = fmt(`Mean Sq`, 1),
    F = fmt(`F value`),
    p = formatar_p(`Pr(>F)`)
  ) |>
  select(Fonte, GL, SQ, QM, F, p) |>
  # A linha do resíduo não tem F nem p; deixamos as células vazias.
  mutate(across(c(F, p), ~ ifelse(is.na(.x) | .x == "-", "", .x))) |>
  flextable_ocean()

# ----------------------------------------------------------------------------|
## ---- fig-barras ----

# Figura de barras: a média de cada grupo, com barra de erro (IC {{IC_PERCENTUAL}} %) e as
# letras do teste de Tukey, no mesmo estilo da tela da CatalyseR.

ggplot(resumo, aes(x = {{FATOR_R}}, y = media, fill = {{FATOR_R}})) +
  geom_col(width = 0.7) +
  geom_errorbar(aes(ymin = ic_inf, ymax = ic_sup), width = 0.2, linewidth = 0.6) +
  geom_text(aes(y = ic_sup, label = letra), vjust = -0.6, size = 4) +
  scale_fill_manual(values = cores_grupos, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    x = {{ROTULO_X_R}},
    y = {{ROTULO_Y_R}},
    title = {{TITULO_GRAFICO_R}}
  ) +
  tema_projeto()

# ----------------------------------------------------------------------------|
## ---- fig-grupos ----

# Figura de pontos: cada observação em torno da média, com IC {{IC_PERCENTUAL}} % e as letras
# do teste de Tukey. Mostra a dispersão que as barras escondem.

ggplot() +
  geom_jitter(
    data = dados,
    aes(x = {{FATOR_R}}, y = {{RESPOSTA_R}}, colour = {{FATOR_R}}),
    width = 0.12, height = 0, size = 1.5, alpha = 0.4
  ) +
  geom_errorbar(
    data = resumo,
    aes(x = {{FATOR_R}}, ymin = ic_inf, ymax = ic_sup),
    width = 0.2, linewidth = 0.6
  ) +
  geom_point(
    data = resumo,
    aes(x = {{FATOR_R}}, y = media),
    size = 3, shape = 21, fill = "white", stroke = 1
  ) +
  geom_text(
    data = resumo,
    aes(x = {{FATOR_R}}, y = ic_sup, label = letra),
    vjust = -0.8, size = 4
  ) +
  scale_colour_manual(values = cores_grupos, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.12))) +
  labs(
    x = {{ROTULO_X_R}},
    y = {{ROTULO_Y_R}},
    title = {{TITULO_GRAFICO_R}}
  ) +
  tema_projeto()

# ----------------------------------------------------------------------------|
## ---- fim-do-codigo ----

# (nada além daqui entra no relatório)
