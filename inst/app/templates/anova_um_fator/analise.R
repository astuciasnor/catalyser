# {{TITULO}} - a análise, comentada -------------------------------------------|

# COMO RODAR
# Reinicie o R (Session > Restart R) e execute as linhas em ordem, com
# Ctrl+Enter; o resultado aparece no Console, no Plots ou no Viewer.
# Ctrl+Shift+O abre o menu de seções e lista todos os trechos.

# SCRIPT E RELATÓRIO
# Este script é o lugar de estudar e editar o código da análise. Os trechos
# marcados com "## ---- nome ----" alimentam os chunks de relatorio.qmd.
# Depois de salvar uma mudança aqui, rode o chunk `atualizar` do relatório:
# ele copia o código, sem as linhas de comentário, e preserva o texto científico.
# No Render, conferir_codigo() avisa se ainda falta trazer alguma mudança.
# O texto e as legendas são editados no .qmd; o código é editado aqui.

# PEQUENO VOCABULÁRIO
# <- guarda um resultado em um objeto; |> passa o resultado à próxima função.
# mutate()/summarise() criam colunas; filter() escolhe linhas; select() colunas.
# ~ escreve uma relação em modelos; $ e [[ ]] acessam um componente nomeado.
# NA indica ausência de dado, não zero.
# case_when() associa condições a resultados, na ordem em que foram escritos.

# As funções do projeto estão em R/funcoes.R: resumir_grupo(), tema_projeto(),
# cores_tratamento, fmt(), formatar_p() e flextable_ocean().

# ----------------------------------------------------------------------------|
{{INSTALAR}}

# ----------------------------------------------------------------------------|
## ---- pacotes ----

# --- 1. Carregar os pacotes --------------------------------
# Rode este trecho antes das etapas de importação e análise.

# here() monta os caminhos a partir da raiz do projeto (onde está o .Rproj),
# para o mesmo arquivo funcionar em computadores diferentes.

library(EAPADados)     # dados e funções do ecossistema
library(catalyser)     # mesmas funções usadas na IDE, incluindo converter_datas()
library(here)
library(readxl)        # leitura da planilha
library(dplyr)         # manipulação de dados
library(tidyr)         # reorganização de dados
library(lubridate)     # tratamento de datas
library(ggplot2)       # gráficos
library(car)           # teste de Levene
library(multcompView)  # letras dos grupos após o Tukey
library(effectsize)    # tamanho de efeito (eta² e omega²)
library(flextable)     # tabelas para o Word

# --- 2. Carregar as funções do projeto ---------------------
# Resumos, formatação, tema e tabelas ficam reunidos em R/funcoes.R.
source(here("R", "funcoes.R"))

# --- 3. Definir a apresentação e o nível de confiança -------
# Três algarismos significativos deixam as saídas exploratórias mais limpas.
# As tabelas finais usam fmt() e formatar_p() e não dependem desta opção.
options(digits = 3)
# Nível usado nos intervalos de confiança das médias e do Tukey.
nivel_confianca <- {{NIVEL_CONFIANCA}}

# ----------------------------------------------------------------------------|
## ---- importar ----

# Lê a planilha como ela veio, sem mexer em nada. Sai dados_brutos.

# A planilha é somente-leitura: nunca a edite. Conferir a aba é uma medida
# simples contra erros no nome da planilha.

# --- 1. Localizar a planilha e conferir suas abas -----------
arquivo <- here("dados", "brutos", {{PLANILHA_R}})

excel_sheets(arquivo)

# --- 2. Ler a aba escolhida na CatalyseR --------------------
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

# Casos sem resposta ou grupo preenchidos serão excluídos no preparo da
# ANOVA. A planilha original nunca é alterada.

# --- 1. Reproduzir as etapas salvas na CatalyseR -------------

{{PREPARO}}

# --- 2. Conferir a base reproduzida -------------------------
# Compara a base refeita com a fotografia da IDE, sem sobrescrever a referência.
# Se você mudar o preparo de propósito, confira a diferença informada no console.
catalyser::catalyser_conferir_base(
  base_compartilhada,
  here("dados", "processados", "base_compartilhada.rds"),
  rotulo = "Base Compartilhada"
)

# --- 3. Adotar mudanças no preparo, se necessário -----------
# O preparo acima serve para reproduzir e conferir os dados no script.
# O relatório começa pelas bases já salvas. Se mudar o preparo, confira os
# resultados antes de atualizar os RDS. Execute estas linhas só quando quiser
# adotar a mudança (retire o # do início):
# saveRDS(base_compartilhada, here("dados", "processados", "base_compartilhada.rds"))
# saveRDS(dados, here("dados", "processados", {{BASE_RDS}}))
# Os Excel permanecem como fotografias da exportação original.

# ----------------------------------------------------------------------------|
## ---- carregar-bases ----

# O RDS conserva os tipos das colunas e já contém o preparo feito na IDE.
# Este é o ponto de entrada do relatório: não repete importação nem tratamentos.
dados <- readRDS(here("dados", "processados", {{BASE_RDS}}))

# ----------------------------------------------------------------------------|
## ---- preparar-analise ----

# ============================================================
# Preparo da base para a ANOVA
# ============================================================

# --- 1. Conferir a variável resposta ------------------------
# A resposta precisa ser numérica. Se a tipagem estiver incorreta,
# interrompemos aqui para que ela seja corrigida no preparo.
if (!is.numeric(dados${{RESPOSTA_R}})) {
  stop(paste0("A resposta (", {{RESPOSTA_STRING}}, ") precisa ser numérica. Confira a tipagem no preparo antes da ANOVA."))
}

# --- 2. Guardar a base antes da exclusão --------------------
# Preserva a base como ela chegou, para comparar antes e depois.
base_da_anova <- dados
n_preparadas <- nrow(base_da_anova)  # número de linhas antes da exclusão

# --- 3. Preparar os grupos e remover casos incompletos ------
# Converte o grupo em fator e mantém apenas observações com grupo
# e resposta preenchidos. Níveis sem observações são descartados.
dados <- base_da_anova |>
  dplyr::mutate({{FATOR_R}} = factor({{FATOR_R}})) |>  # grupo como fator
  tidyr::drop_na({{FATOR_R}}, {{RESPOSTA_R}}) |>       # só casos completos
  droplevels()                                     # remove níveis vazios

n_excluidas <- n_preparadas - nrow(dados)  # casos incompletos removidos

# --- 4. Escolher as cores dos grupos (paleta Ocean) ---------
# Usa uma cor da paleta por grupo. Se houver mais grupos que cores,
# cria cores intermediárias para cobrir todos os grupos.
n_grupos <- nlevels(dados${{FATOR_R}})

cores_grupos <- if (n_grupos <= length(cores_tratamento)) {
  cores_tratamento[seq_len(n_grupos)]  # seleciona as cores necessárias
} else {
  grDevices::colorRampPalette(cores_tratamento)(n_grupos)  # interpola as cores
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
# si. Os casos incompletos já foram retirados no preparo acima.

# --- 1. Ajustar o modelo -----------------------------------
modelo <- aov({{RESPOSTA_R}} ~ {{FATOR_R}}, data = dados)

# --- 2. Extrair e mostrar a tabela da ANOVA ------------------
tabela_anova <- anova(modelo)
tabela_anova

# ----------------------------------------------------------------------------|
## ---- analisar-pressupostos ----

# ============================================================
# Verificação dos pressupostos da ANOVA
# ============================================================
# Normalidade dos resíduos (Shapiro-Wilk) e homogeneidade das
# variâncias entre grupos (Levene).
#
# Leitura dos p-valores: p > 0,05 indica que não há evidência para
# rejeitar o pressuposto avaliado; não prova que ele seja verdadeiro.

# --- 1. Normalidade dos resíduos (Shapiro-Wilk) -------------
residuos <- residuals(modelo)

# shapiro.test() aceita de 3 a 5000 observações e precisa de resíduos
# com alguma variação (sd > 0). Fora dessas condições, registramos NA
# (teste não calculado) para continuar a análise e conferir o gráfico Q-Q.
n_residuos <- length(residuos)
shapiro_valido <- n_residuos >= 3 && n_residuos <= 5000 && sd(residuos) > 0

teste_normalidade <- if (shapiro_valido) {
  shapiro.test(residuos)
} else {
  list(statistic = c(W = NA_real_), p.value = NA_real_)  # teste não calculado
}

# --- 2. Homogeneidade das variâncias (Levene) ---------------
teste_levene <- car::leveneTest({{RESPOSTA_R}} ~ {{FATOR_R}}, data = dados)

# --- 3. Conferir os resultados -----------------------------
teste_normalidade
teste_levene

# ----------------------------------------------------------------------------|
## ---- analisar-tukey ----

# ============================================================
# Comparações múltiplas (Tukey HSD) e letras dos grupos
# ============================================================
# Tukey compara todos os grupos par a par, com p-valores ajustados.
# Uma letra compartilhada indica diferença não detectada a 5 %;
# não prova que os grupos sejam iguais.

# --- 1. Teste de Tukey -------------------------------------
# Calcula as comparações e mostra a tabela completa.
tukey <- TukeyHSD(modelo, conf.level = nivel_confianca)
tukey

# --- 2. Letras dos grupos ----------------------------------
# A versão curta abaixo exige nomes de grupos sem hífen.
# Se houver hífen, ajuste os nomes no preparo antes de prosseguir.
if (any(grepl("-", levels(dados${{FATOR_R}}), fixed = TRUE))) {
  stop("As letras de Tukey exigem nomes de grupos sem hífen nesta versão. Renomeie os grupos no preparo e execute novamente.")
}

# multcompLetters4() recebe o modelo e o Tukey, ordena os grupos
# pelas médias e devolve as letras. O primeiro $ seleciona o fator;
# $Letters extrai o vetor de letras, identificado pelo nome dos grupos.
letras <- multcompView::multcompLetters4(modelo, tukey)${{FATOR_R}}$Letters

# --- 3. Tabela-resumo com as letras -------------------------
# Junta a letra às estatísticas pelo nome do grupo, não pela posição,
# para manter cada letra associada ao grupo correto. Depois ordena a tabela.
resumo <- resumir_grupo(dados, {{RESPOSTA_R}}, {{FATOR_R}}, conf = nivel_confianca) |>
  dplyr::mutate(letra = letras[as.character({{FATOR_R}})]) |>
  dplyr::arrange({{FATOR_R}})

# Mostra as estatísticas e as letras de cada grupo.
resumo

# ----------------------------------------------------------------------------|
## ---- preparar-resultados-texto ----

# ============================================================
# Números e frases para o texto dos Resultados
# ============================================================

# --- 1. Guardar os p-valores brutos -------------------------
# Extrai cada p-valor uma única vez. As decisões usam esses valores
# completos; o arredondamento fica apenas na apresentação do texto.
p_anova_bruto   <- tabela_anova$`Pr(>F)`[1]
p_shapiro_bruto <- teste_normalidade$p.value
p_levene_bruto  <- teste_levene$`Pr(>F)`[1]

# --- 2. Formatar os números para o texto --------------------
# fmt() aplica a vírgula decimal; formatar_p() usa a escrita convencional de p.
f_valor   <- fmt(tabela_anova$`F value`[1])  # estatística F da ANOVA
gl_trat   <- tabela_anova$Df[1]             # graus de liberdade do fator
gl_res    <- tabela_anova$Df[2]             # graus de liberdade do resíduo
p_anova   <- formatar_p(p_anova_bruto, no_texto = TRUE)
p_shapiro <- formatar_p(p_shapiro_bruto, no_texto = TRUE)
p_levene  <- formatar_p(p_levene_bruto, no_texto = TRUE)

# --- 3. Escrever a conclusão da ANOVA -----------------------
# A frase acompanha o resultado, sem afirmar diferença que não apareceu.
# case_when() escolhe, para cada valor, a primeira condição verdadeira,
# de cima para baixo. TRUE define a resposta para os demais casos.
frase_anova <- dplyr::case_when(
  is.na(p_anova_bruto) ~ "A ANOVA não forneceu um p-valor válido para comparar as médias de {{RESPOSTA}} entre os grupos",
  p_anova_bruto < 0.05 ~ "Houve evidência de diferença na média de {{RESPOSTA}} entre os grupos",
  TRUE ~ "Não houve evidência de diferença na média de {{RESPOSTA}} entre os grupos"
)

# --- 4. Calcular o tamanho de efeito -----------------------
# Fração da variação de {{RESPOSTA}} associada a {{FATOR}}.
# Omega² corrige o viés do eta² em amostras pequenas.
efeito_eta   <- effectsize::eta_squared(modelo)
efeito_omega <- effectsize::omega_squared(modelo)
eta_val <- efeito_eta$Eta2[1]           # valor bruto, usado na classificação
eta2    <- fmt(eta_val)                # eta² formatado para o texto
omega2  <- fmt(efeito_omega$Omega2[1])  # omega² formatado para o texto

# --- 5. Classificar o efeito (convenção de Cohen para η²) ---
# Referência estatística, não biológica, igual à tela da CatalyseR:
# < 0,01 muito pequeno; < 0,06 pequeno; < 0,14 médio; daí em diante grande.
classe_efeito <- dplyr::case_when(
  is.na(eta_val) ~ "indeterminado",
  eta_val < 0.01 ~ "muito pequeno",
  eta_val < 0.06 ~ "pequeno",
  eta_val < 0.14 ~ "médio",
  TRUE ~ "grande"
)

# --- 6. Escrever as frases dos pressupostos ----------------
# Cada frase acompanha o resultado do teste. NA significa que o teste
# não forneceu um p-valor válido, não que o pressuposto foi atendido.
frase_normalidade <- dplyr::case_when(
  is.na(p_shapiro_bruto) ~ "o teste de normalidade não foi calculado; use o gráfico Q-Q",
  p_shapiro_bruto >= 0.05 ~ "não houve evidência para rejeitar a normalidade dos resíduos",
  TRUE ~ "houve evidência de afastamento da normalidade dos resíduos"
)
frase_levene <- dplyr::case_when(
  is.na(p_levene_bruto) ~ "o teste de Levene não forneceu um p-valor válido",
  p_levene_bruto >= 0.05 ~ "não houve evidência para rejeitar a igualdade das variâncias entre os grupos",
  TRUE ~ "houve evidência de variâncias diferentes entre os grupos"
)

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
## ---- tbl-resumo ----

# Estatísticas descritivas e os grupos do teste de Tukey.
# A sequência formata os números, escolhe e nomeia as colunas
# e aplica o tema Ocean. O objeto resumo conserva os valores numéricos.

resumo |>
  mutate(
    `IC {{IC_PERCENTUAL}} %` = as.character(stringr::str_glue("{fmt(ic_inf, 1)} a {fmt(ic_sup, 1)}")),
    across(c(media, dp, ep), ~ fmt(.x, 1))
  ) |>
  select(
    {{FATOR_R}},
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
# GL = graus de liberdade; SQ = soma de quadrados; QM = quadrado médio.
# Primeiro formata os valores; depois escolhe as colunas e aplica o tema Ocean.

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
  mutate(across(c(F, p), ~ dplyr::case_when(
    is.na(.x) | .x == "-" ~ "",
    TRUE ~ .x
  ))) |>
  flextable_ocean()

# ----------------------------------------------------------------------------|
## ---- fig-barras ----

# Figura de barras: a média de cada grupo, com barra de erro (IC {{IC_PERCENTUAL}} %) e as
# letras do teste de Tukey, no mesmo estilo da tela da CatalyseR.

ggplot(resumo, aes(x = {{FATOR_R}}, y = media, fill = {{FATOR_R}})) +
  # Barras: altura igual à média de cada grupo.
  geom_col(width = 0.7) +
  # Hastes: limites inferior e superior do intervalo de confiança.
  geom_errorbar(aes(ymin = ic_inf, ymax = ic_sup), width = 0.2, linewidth = 0.6) +
  # Letras: posicionadas acima do limite superior do intervalo.
  geom_text(aes(y = ic_sup, label = letra), vjust = -0.6, size = 4) +
  # Média ± DP pouco acima da barra, à direita da haste; as hastes seguem sendo IC.
  geom_text(
    aes(y = media, label = paste0(fmt(media, 1), " ± ", fmt(dp, 1))),
    nudge_x = 0.08, hjust = 0, vjust = -0.4, size = 3.5
  ) +
  # Espaço para os rótulos, cores dos grupos e folga no alto do gráfico.
  scale_x_discrete(expand = expansion(add = c(0.6, 0.9))) +
  scale_fill_manual(values = cores_grupos, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  # Nomes dos eixos e título escolhidos na CatalyseR.
  labs(
    x = {{ROTULO_X_R}},
    y = {{ROTULO_Y_R}},
    title = {{TITULO_GRAFICO_R}}
  ) +
  tema_projeto()

# ----------------------------------------------------------------------------|
## ---- fim-do-codigo ----

# (nada além daqui entra no relatório)
