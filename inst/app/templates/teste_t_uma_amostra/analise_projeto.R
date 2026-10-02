# {{TITULO_COMENTARIO}}
# Roteiro: ambiente → dados → exploração → teste → comunicação.
# Abra o .Rproj e execute as seções em ordem. Ctrl+Enter executa uma linha.
# Os dois QMDs executam este mesmo script em sessões novas.
# CSVs e PNGs são cópias para compartilhar; os relatórios usam objetos em memória.

# 1. Preparar o ambiente ---------------------------------------------------
library(here)
# Declara: "este arquivo está em R/analise.R, dentro do meu projeto".
# Assim, here() monta caminhos a partir da raiz do projeto, acima da pasta R/.
# Não muda a pasta de trabalho como setwd(). Abra o projeto antes de rodar.
here::i_am("R/analise.R")
{{BIBLIOTECAS_PREPARO}}
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
nivel_confianca <- {{CONFIANCA}}
alfa <- 1 - nivel_confianca
ic_percentual <- fmt(100 * nivel_confianca, 0)
alternativa <- {{ALTERNATIVA_R}}
alternativa <- match.arg(alternativa, c("two.sided", "greater", "less"))
if (!is.finite(nivel_confianca) || nivel_confianca <= 0 || nivel_confianca >= 1) {
  stop("Confira o nível de confiança, entre zero e um.")
}
# O nome aponta para a coluna; o rótulo só muda a apresentação.
variavel_resposta <- {{VARIAVEL_1_R}}
rotulo_analisado <- {{ROTULO_1_R}}
valor_nulo <- {{MU0}}
if (!is.finite(valor_nulo)) stop("O valor de referência precisa ser finito.")
nome_efeito <- "d de Cohen"
tipo_poder <- "one.sample"
descricao_desenho <- paste0("Teste t de uma amostra: ", rotulo_analisado,
  " comparada com uma referência fixa. As unidades amostrais devem ser independentes.")
# Produtos regeneráveis ficam separados da planilha de entrada.
for (pasta in c("dados/processados", "saida/tabelas", "saida/figuras", "saida/relatorios")) {
  dir.create(here::here(pasta), recursive = TRUE, showWarnings = FALSE)
}
{{TRECHO_IMPORTAR}}

# 3. Preparar a base -------------------------------------------------------
{{TRECHO_PREPARO}}
dados <- as.data.frame(dados_da_analise)
n_total <- nrow(dados)
if (!variavel_resposta %in% names(dados) || !is.numeric(dados[[variavel_resposta]])) {
  stop("Confira a coluna numérica da resposta.")
}
# Faltantes em outras colunas não excluem observações desta análise.
completos <- !is.na(dados[[variavel_resposta]])
n_excluido <- sum(!completos)
base_t <- data.frame(linha_original = which(completos), valor = dados[[variavel_resposta]][completos])
valores <- base_t$valor

# 4. Explorar a variável analisada -----------------------------------------
# n conta as observações completas; sd() calcula o desvio padrão amostral.
n_utilizado <- length(valores)
if (n_utilizado < 2L || any(!is.finite(valores))) {
  stop("São necessárias pelo menos duas observações completas e finitas.")
}
media <- mean(valores)
dp <- sd(valores)
if (!is.finite(dp) || dp <= 0) stop("Não há variação suficiente para o teste t.")
# O erro padrão descreve a incerteza da média, não a dispersão dos indivíduos.
ep <- dp / sqrt(n_utilizado)
# O resumo descritivo usa IC bilateral, mesmo quando o teste é unilateral.
margem_media <- qt(1 - alfa / 2, n_utilizado - 1) * ep
tabela_descritiva <- data.frame(n = n_utilizado, media = media, dp = dp,
  ep = ep, ic_inferior = media - margem_media, ic_superior = media + margem_media)
tabela_descritiva_exibir <- data.frame(n = n_utilizado, Média = fmt(media),
  DP = fmt(dp), EP = fmt(ep), `IC bilateral da média` = paste0("[",
    fmt(media - margem_media), "; ", fmt(media + margem_media), "]"), check.names = FALSE)

# 5. Aplicar o teste t ------------------------------------------------------
# Mantemos a alternativa, a referência e a confiança registradas na CatalyseR.
teste_t <- stats::t.test(valores, mu = valor_nulo,
  alternative = alternativa, conf.level = nivel_confianca)
# A saída crua aparece no console para estudo, uma vez; não entra nos QMDs.
print(teste_t)
estimativa <- media - valor_nulo
estatistica_t <- unname(teste_t$statistic)
graus_liberdade <- unname(teste_t$parameter)
p_teste <- teste_t$p.value
ic_estimativa <- unname(teste_t$conf.int)
# Uma amostra: d = (média - referência)/DP; pareado: d_z = média das diferenças/DP delas.
d_cohen <- estimativa / dp
# O IC do efeito é bilateral e usa a distribuição t não central.
efeito_d <- effectsize::cohens_d(valores - valor_nulo,
  ci = nivel_confianca, alternative = "two.sided")
d_ic <- c(efeito_d$CI_low[1], efeito_d$CI_high[1])
# O poder observado descreve uma hipótese de efeito, não prova a hipótese nula.
poder_teste <- pwr::pwr.t.test(n = n_utilizado, d = d_cohen, sig.level = alfa,
  type = tipo_poder, alternative = alternativa)$power

# 6. Conferir os pressupostos ---------------------------------------------
# Uma amostra usa a resposta; o pareado usa as diferenças entre as duas medidas.
# Shapiro-Wilk aceita entre três e cinco mil valores. Fora disso, consulte o Q-Q.
teste_shapiro <- if (n_utilizado >= 3L && n_utilizado <= 5000L)
  stats::shapiro.test(valores) else NULL
p_shapiro <- if (is.null(teste_shapiro)) NA_real_ else teste_shapiro$p.value
w_shapiro <- if (is.null(teste_shapiro)) NA_real_ else unname(teste_shapiro$statistic)
leitura_shapiro <- dplyr::case_when(
  is.na(p_shapiro) ~ "Shapiro-Wilk não calculado; examine o Q-Q.",
  p_shapiro < alfa ~ "Há evidência de afastamento da normalidade.",
  TRUE ~ "Não houve evidência para rejeitar a normalidade; isso não a comprova."
)
tabela_pressupostos <- data.frame(Teste = "Shapiro-Wilk", W = fmt(w_shapiro, 3),
  p = formatar_p(p_shapiro), Leitura = leitura_shapiro)
# Centrar não altera a forma da distribuição; permite visualizar desvios da média.
diagnosticos <- data.frame(linha_original = base_t$linha_original,
  residuo = valores - media, residuo_padronizado = (valores - media) / dp)

# 7. Preparar as tabelas ----------------------------------------------------
# Inf aparece em IC unilateral; o símbolo infinito deixa a leitura explícita.
ic_texto <- dplyr::case_when(
  is.infinite(ic_estimativa) & ic_estimativa < 0 ~ "-∞",
  is.infinite(ic_estimativa) & ic_estimativa > 0 ~ "∞",
  TRUE ~ fmt(ic_estimativa)
)
tipo_ic <- if (alternativa == "two.sided") "bilateral" else "unilateral"
tabela_t_exibir <- data.frame(t = fmt(estatistica_t, 3), gl = fmt(graus_liberdade, 0),
  p = formatar_p(p_teste), `Estimativa testada` = fmt(media),
  `Referência de H0` = fmt(valor_nulo),
  IC = paste0("[", ic_texto[1], "; ", ic_texto[2], "]"), check.names = FALSE)
names(tabela_t_exibir)[6] <- paste0("IC ", ic_percentual, "% ", tipo_ic)
tabela_efeito <- data.frame(Medida = nome_efeito, Valor = fmt(d_cohen, 3),
  `IC bilateral` = paste0("[", fmt(d_ic[1], 3), "; ", fmt(d_ic[2], 3), "]"), check.names = FALSE)

# 8. Construir as figuras --------------------------------------------------
# Pontos mostram os indivíduos ou as diferenças; haste mostra IC e rótulo mostra média ± DP.
dados_figura <- data.frame(valor = valores, coluna = "Observações")
resumo_figura <- data.frame(coluna = "Observações", media = media, dp = dp)
# IC bilateral da média na figura, inclusive quando o teste é unilateral.
margem_figura <- qt((1 + nivel_confianca) / 2, n_utilizado - 1) * dp / sqrt(n_utilizado)
resumo_figura$ic_inf <- media - margem_figura
resumo_figura$ic_sup <- media + margem_figura
grafico_principal <- ggplot2::ggplot(dados_figura, ggplot2::aes(x = coluna, y = valor)) +
  ggplot2::geom_col(data = resumo_figura, ggplot2::aes(y = media),
    width = .30, fill = "#2E7D8F", alpha = .22) +
  ggplot2::geom_jitter(width = .08, height = 0, colour = "#2E7D8F", alpha = .65) +
  ggplot2::geom_hline(yintercept = valor_nulo, linetype = "dashed", colour = "#E76F51") +
  ggplot2::geom_errorbar(data = resumo_figura,
    ggplot2::aes(y = media, ymin = ic_inf, ymax = ic_sup),
    width = .08, colour = "#0F3B5F") +
  ggplot2::geom_point(data = resumo_figura, ggplot2::aes(y = media),
    shape = 18, size = 4, colour = "#0F3B5F") +
  ggplot2::geom_label(data = resumo_figura,
    ggplot2::aes(y = media, label = paste0(fmt(media), " ± ", fmt(dp))),
    nudge_x = .16, hjust = 0, linewidth = 0,
    fill = ggplot2::alpha("white", .75), colour = "#0F3B5F") +
  ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, .15))) +
  ggplot2::scale_x_discrete(expand = ggplot2::expansion(add = c(.4, .7))) +
  ggplot2::labs(x = NULL, y = rotulo_analisado,
    subtitle = paste0("Losango = média; rótulo = média ± DP; haste = IC bilateral da média; referência = ", fmt(valor_nulo))) +
  tema_projeto()
# Procure valores isolados ou um padrão na sequência, sem excluir observações automaticamente.
grafico_residuos <- ggplot2::ggplot(diagnosticos,
  ggplot2::aes(x = linha_original, y = residuo)) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "#E76F51") +
  ggplot2::geom_point(colour = "#2E7D8F") +
  ggplot2::labs(x = "Linha original na base", y = "Desvio da média") + tema_projeto()
# Pontos próximos à reta apoiam a leitura de normalidade; caudas afastadas pedem atenção.
grafico_qq <- ggplot2::ggplot(diagnosticos, ggplot2::aes(sample = residuo_padronizado)) +
  ggplot2::stat_qq(colour = "#2E7D8F") +
  ggplot2::stat_qq_line(colour = "#E76F51") +
  ggplot2::labs(x = "Quantis teóricos", y = "Desvios padronizados") + tema_projeto()


# 9. Preparar os textos ----------------------------------------------------
# H1 mantém o sentido escolhido; no pareado o sinal é medida 1 menos medida 2.
hipotese_alternativa <- dplyr::case_when(
  alternativa == "greater" ~ "a média analisada é maior que a referência",
  alternativa == "less" ~ "a média analisada é menor que a referência",
  TRUE ~ "a média analisada difere da referência"
)
decisao <- dplyr::case_when(
  p_teste < alfa ~ "Rejeitamos H0 no alfa adotado.",
  TRUE ~ "Não rejeitamos H0; isso não comprova ausência de efeito."
)
texto_amostra <- paste0("A análise utilizou ", n_utilizado, " casos completos de ",
  n_total, "; ", n_excluido, " foram excluídos por ausência de medida necessária. ")
texto_metodo <- paste0(descricao_desenho, " H0: média analisada = ", fmt(valor_nulo),
  ". H1: ", hipotese_alternativa, ". Confiança = ", ic_percentual,
  "%; alfa = ", fmt(alfa, 3), ". O IC do teste é ", tipo_ic,
  "; os ICs da média descritiva e do efeito são bilaterais.")
texto_teste <- paste0("t(", fmt(graus_liberdade, 0), ") = ", fmt(estatistica_t, 3),
  "; ", formatar_p(p_teste, no_texto = TRUE), ". ", decisao)
texto_efeito <- paste0(nome_efeito, " = ", fmt(d_cohen, 3),
  ", IC bilateral ", ic_percentual, "% [", fmt(d_ic[1], 3), "; ", fmt(d_ic[2], 3),
  "]. A medida padroniza a diferença pelo DP da variável analisada; não mede importância biológica.")
texto_pressupostos <- paste(leitura_shapiro,
  "A independência entre unidades depende do delineamento; os gráficos não a comprovam.")
alerta_poder <- paste0("Poder para o efeito observado e a alternativa adotada: ",
  fmt(100 * poder_teste, 1), "%. Este cálculo não prova H0 nem substitui planejar",
  " o tamanho da amostra a partir de um efeito biologicamente relevante.")
texto_sintese_estatistica <- paste(texto_teste, texto_efeito)
print(texto_teste)

# 10. Salvar cópias para compartilhar --------------------------------------
# As cópias não alimentam os QMDs. A fonte dos cálculos continua neste script.
write.csv2(base_t, here::here("dados/processados/base_t.csv"),
  row.names = FALSE, fileEncoding = "UTF-8")
tabelas <- list(descritiva = tabela_descritiva, teste_t = tabela_t_exibir,
  efeito = tabela_efeito, pressupostos = tabela_pressupostos)
for (nome in names(tabelas)) {
  write.csv2(tabelas[[nome]], here::here("saida/tabelas", paste0(nome, ".csv")),
    row.names = FALSE, fileEncoding = "UTF-8")
}
figuras <- list(principal = grafico_principal, residuos = grafico_residuos, qq = grafico_qq)

for (nome in names(figuras)) {
  ggplot2::ggsave(here::here("saida/figuras", paste0(nome, ".png")),
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
write.csv2(tabela_ambiente, here::here("saida", "ambiente.csv"),
  row.names = FALSE, fileEncoding = "UTF-8")
