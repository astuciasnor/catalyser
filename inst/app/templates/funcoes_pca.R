# =============================================================================
# funcoes_pca.R
# -----------------------------------------------------------------------------
# Motor da Análise de Componentes Principais (PCA / ACP) da CatalyseR.
# Adaptado do roteiro didático da curadoria (Multivariada_02_PCA), seguindo o
# fluxo: correlações -> PCA padronizada (FactoMineR) -> retenção por três
# critérios (Kaiser apenas como referência; bastão quebrado; permutação em
# destaque) -> círculo de correlações, contribuições, mapa de indivíduos e
# biplot -> cargas com limite de permutação -> comparação com imputação
# (missMDA), quando houver dados faltantes.
#
# A permutação (teste de significância dos eixos e das cargas) é implementada
# aqui mesmo, em R base, na função permutar_pca() — reproduz a lógica de
# Camargo (2022), sem depender do pacote PCAtest (que saiu do CRAN). Assim o
# projeto fica 100 % CRAN e a permutação está sempre disponível.
#
# Arquitetura (fonte canônica única):
#   permutar_pca()      -> teste de permutação dos eixos e das cargas (R base).
#   calcular_pca()      -> executa tudo; devolve uma lista com os resultados.
#   mostrar_pca_*()     -> tabelas formatadas para exibição e relatório.
#   grafico_pca_*()     -> figuras prontas no padrão Ocean Gradient.
#   relatar_pca()       -> síntese dos resultados em português.
#
# Este arquivo é copiado para os pacotes de estudo exportados pela IDE e deve
# rodar sozinho; por isso os pacotes usados são carregados aqui no topo.
# =============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)
library(flextable)
library(FactoMineR)
library(factoextra)
library(ggcorrplot)
library(patchwork)

# O missMDA só entra em cena quando o usuário pede a comparação com imputação.
# É o único opcional: a PCA e a permutação rodam sem ele (a permutação é R base).
if (requireNamespace("missMDA", quietly = TRUE)) {
  library(missMDA)
}

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a) || !length(a)) b else a

# ---- Utilitário: Formato numérico brasileiro ---------------------------------
fmt_pca <- function(x, dig = 2) {
  vapply(x, function(v) {
    if (is.null(v) || length(v) == 0 || is.na(v)) return("-")
    formatC(v, format = "f", digits = dig, decimal.mark = ",")
  }, character(1))
}

# ---- Paleta e tema (identidade visual da curadoria) --------------------------
# Guardamos a paleta Ocean Gradient usada em todas as figuras. Os nomes usam o
# prefixo pca_ para não colidir com as funções de tema globais da IDE.
pca_ocean <- c("#0F3B5F", "#2E7D8F", "#62B6B7", "#E89B3C", "#E76F51")

# Fixamos uma cor por trecho do rio, as mesmas do guia de agrupamento hierárquico.
pca_cores_trecho <- c("Alto curso" = "#0F3B5F", "Médio curso" = "#E76F51",
                      "Baixo curso" = "#E89B3C", "Trecho impactado" = "#2E7D8F")

# As suplementares quantitativas não definem os eixos: fora dos gradientes de
# contribuição e cos2, elas entram em cinza neutro, com seta tracejada.
pca_cor_suplementar <- "grey40"

# Definimos um tema limpo, de fundo branco, para todas as figuras.
pca_tema <- theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", color = "#0F3B5F"),
        panel.grid.minor = element_blank())

# Escolhemos cores estáveis para os níveis do grupo: se os níveis forem os
# trechos do rio, usamos as cores fixas do guia de HCA; senão, a paleta ocean.
paleta_grupos <- function(niveis) {
  if (all(niveis %in% names(pca_cores_trecho))) {
    return(pca_cores_trecho[niveis])
  }
  if (length(niveis) <= length(pca_ocean)) return(pca_ocean[seq_along(niveis)])
  grDevices::colorRampPalette(pca_ocean)(length(niveis))
}

#' Teste de permutação da PCA em R base — substitui o pacote PCAtest.
#'
#' Reproduz a lógica de Camargo (2022): embaralha cada variável de forma
#' independente, recalcula a PCA sobre os dados permutados e compara os
#' autovalores e as cargas observados com a distribuição nula obtida. Serve
#' para duas perguntas: (1) quantos eixos carregam mais estrutura do que o
#' acaso? (2) quais cargas (variáveis) pesam mais do que o acaso em cada eixo?
#'
#' @param dados data.frame ou matriz apenas com as variáveis ativas e as linhas
#'   completas (as mesmas que entram na PCA).
#' @param padronizar TRUE padroniza as colunas (PCA de correlação); FALSE apenas
#'   centraliza (PCA de covariância). Deve espelhar o que a PCA fizer.
#' @param nperm número de permutações (padrão 999).
#' @param seed semente registrada, para o teste ser reprodutível.
#' @param n_eixos_cargas quantos eixos terão as cargas testadas (padrão 2).
#' @return lista com autovalores e percentuais observados, p-valor por eixo,
#'   limites nulos da variância (2,5 % e 97,5 %), índice de carga observado e
#'   limite permutado das cargas (97,5 %). Tudo em R base, sem pacotes extras.
permutar_pca <- function(dados, padronizar = TRUE, nperm = 999L, seed = 2026L,
                         n_eixos_cargas = 2L) {
  # Convertemos a entrada em matriz numérica, que é o que a PCA consome.
  X <- as.matrix(dados)
  # Guardamos o número de observações (linhas) e de variáveis ativas (colunas).
  n <- nrow(X)
  p <- ncol(X)
  # Número de eixos que a PCA realmente produz: o posto da matriz centrada é
  # no máximo n - 1, e nunca há mais eixos do que variáveis. Usar esse limite
  # evita erros quando há mais variáveis do que observações completas (n <= p),
  # situação comum depois da remoção de linhas com dados faltantes.
  n_axes <- min(p, n - 1L)
  # Sem ao menos um eixo (n < 2), não há o que permutar.
  if (n_axes < 1L) {
    stop("Observações insuficientes para o teste de permutação.", call. = FALSE)
  }

  # Preparamos a matriz exatamente como a PCA a vê: padronizada (correlação)
  # ou apenas centralizada (covariância). Permutar colunas preserva média e
  # desvio de cada uma, então padronizar antes ou depois dá o mesmo resultado.
  Z <- if (isTRUE(padronizar)) scale(X) else scale(X, center = TRUE, scale = FALSE)

  # Função auxiliar que faz uma PCA por decomposição em valores singulares (SVD),
  # o método mais estável e disponível no R base (sem depender de pacote).
  pca_svd <- function(Zm) {
    # Decompomos a matriz preparada: Zm = u %*% diag(d) %*% t(v).
    sv <- svd(Zm)
    # Os autovalores são os desvios ao quadrado, na convenção de variância (n - 1).
    eig <- sv$d^2 / (nrow(Zm) - 1)
    # Recortamos para o número de eixos viáveis (posto da matriz).
    eig <- eig[seq_len(min(n_axes, length(eig)))]
    # Recortamos os autovetores (cargas) para os mesmos eixos.
    rot <- sv$v[, seq_len(min(n_axes, ncol(sv$v))), drop = FALSE]
    # Devolvemos autovalores e autovetores: é o suficiente para o teste.
    list(eig = eig, rot = rot)
  }

  # Calculamos a PCA observada uma única vez, sobre os dados reais.
  obs <- pca_svd(Z)
  # O percentual de variância de cada eixo é o autovalor dividido pelo total.
  pct_obs <- obs$eig / sum(obs$eig) * 100

  # Índice de carga observado: autovetor ao quadrado vezes o autovalor ao
  # quadrado. É o mesmo que (correlação da variável com o eixo)² vezes o
  # autovalor — uma grandeza SEM SINAL, o que já resolve a indeterminação de
  # sinal dos eixos (o sinal de um autovetor é arbitrário; o quadrado, não).
  # Fica organizado como variáveis nas linhas e eixos nas colunas.
  idx_obs <- sweep(obs$rot^2, 2, obs$eig^2, "*")

  # Fixamos a semente antes do laço, para as permutações serem reprodutíveis.
  set.seed(as.integer(seed))
  # Preparamos os recipientes que guardam os resultados de cada permutação.
  eig_null <- matrix(NA_real_, nrow = nperm, ncol = n_axes)     # autovalores nulos
  pct_null <- matrix(NA_real_, nrow = nperm, ncol = n_axes)     # percentuais nulos
  idx_null <- array(NA_real_, dim = c(p, n_axes, nperm))        # cargas nulas (var x eixo x perm)

  # Repetimos o sorteio nperm vezes, cada vez com uma permutação independente.
  for (b in seq_len(nperm)) {
    # Embaralhamos CADA coluna de forma independente: é isso que destrói a
    # estrutura de correlação e cria um dado nulo (só o acaso, sem padrão).
    Zp <- apply(Z, 2, sample)
    # Recalculamos a PCA sobre a matriz permutada.
    pb <- pca_svd(Zp)
    # Guardamos os autovalores e o percentual de variância desta permutação.
    eig_null[b, ] <- pb$eig
    pct_null[b, ] <- pb$eig / sum(pb$eig) * 100
    # Guardamos o índice de carga nulo com a MESMA fórmula do observado, mas
    # usando os autovetores e autovalores da própria PCA permutada.
    idx_null[, , b] <- sweep(pb$rot^2, 2, pb$eig^2, "*")
  }

  # p-valor de cada eixo: quantas permutações tiveram autovalor >= o observado,
  # com a correção de pseudo-contagem (+1 no numerador e no denominador), que
  # evita p-valor exatamente zero — a prática padrão recomendada em testes de
  # permutação. É a mesma fórmula pedida para reproduzir a lógica do PCAtest.
  p_valor <- (1 + colSums(sweep(eig_null, 2, obs$eig, ">="))) / (nperm + 1)

  # Limites nulos da variância explicada: o intervalo de 95 % (2,5 % e 97,5 %)
  # da distribuição permutada. O limite superior (97,5 %) é a linha de
  # referência no gráfico de retenção — acima dela, o eixo supera o acaso.
  limite_pct <- t(apply(pct_null, 2, stats::quantile, probs = c(0.025, 0.975)))

  # Limite permutado das cargas: para cada variável (linha) e cada eixo
  # (coluna), o quantil 97,5 % do índice de carga nulo. Uma carga observada é
  # significativa quando supera esse limite — ou seja, quando é maior do que
  # 97,5 % das cargas que o acaso produziu para aquela variável naquele eixo.
  n_eixos_cargas <- min(n_eixos_cargas, n_axes)
  limite_cargas <- matrix(NA_real_, nrow = p, ncol = n_axes)
  for (j in seq_len(p)) {
    for (k in seq_len(n_eixos_cargas)) {
      limite_cargas[j, k] <- stats::quantile(idx_null[j, k, ], 0.975)
    }
  }

  # Devolvemos tudo numa lista organizada, sem guardar as permutações brutas
  # (que ocupariam memória à toa): só os resumos que as tabelas e figuras usam.
  list(
    nperm = as.integer(nperm),
    seed = as.integer(seed),
    eig_obs = obs$eig,
    pct_obs = pct_obs,
    p_valor = p_valor,
    limite_pct = limite_pct,
    idx_obs = idx_obs,
    limite_cargas = limite_cargas,
    n_eixos_cargas = as.integer(n_eixos_cargas)
  )
}

#' Executa a PCA completa: retenção de eixos, permutação, descrição dos eixos e,
#' quando solicitado, comparação com a imputação por missMDA.
#'
#' @param df data.frame com a base de análise.
#' @param vars_selected nomes das variáveis ativas (numéricas, mínimo 2).
#' @param scale TRUE padroniza as variáveis (PCA de correlação); FALSE usa
#'   apenas a centralização (PCA de covariância).
#' @param quanti_sup nomes de variáveis suplementares quantitativas (opcional).
#' @param quali_sup nome da variável suplementar de grupo (opcional).
#' @param seed semente gravada para a permutação (reprodutibilidade).
#' @param comparar_imputacao quando TRUE e houver NA nas ativas, compara o eixo 1
#'   da PCA completa com o eixo 1 da PCA sobre dados imputados (missMDA).
calcular_pca <- function(df, vars_selected, scale = TRUE, quanti_sup = NULL,
                         quali_sup = NULL, seed = 2026,
                         comparar_imputacao = FALSE) {
  # Exigimos uma base de dados em formato de tabela.
  if (!is.data.frame(df)) {
    stop("A base de dados precisa ser um data.frame.", call. = FALSE)
  }
  # Exigimos uma semente válida: a permutação depende dela para ser reprodutível.
  if (length(seed) != 1 || is.na(seed)) {
    stop("Informe uma semente válida para a permutação.", call. = FALSE)
  }
  # Exigimos pelo menos duas variáveis ativas para existir um plano de projeção.
  if (length(vars_selected) < 2) {
    stop("Selecione pelo menos duas variáveis ativas.", call. = FALSE)
  }
  # Mantemos apenas variáveis ativas que existem na base; as demais são ignoradas.
  ativas_nomes <- intersect(vars_selected, names(df))
  # Recusamos nomes que não são numéricos, pois a PCA exige variáveis contínuas.
  ativas_nomes <- ativas_nomes[vapply(ativas_nomes, function(nm) is.numeric(df[[nm]]), logical(1))]
  if (length(ativas_nomes) < 2) {
    stop("As variáveis ativas precisam ser numéricas e existir na base.", call. = FALSE)
  }
  # Guardamos o número de variáveis ativas para os critérios de retenção.
  p <- length(ativas_nomes)

  # Separamos as suplementares quantitativas válidas (numéricas e fora das ativas).
  suplementares_nomes <- intersect(quanti_sup %||% character(), names(df))
  suplementares_nomes <- setdiff(suplementares_nomes, ativas_nomes)
  suplementares_nomes <- suplementares_nomes[
    vapply(suplementares_nomes, function(nm) is.numeric(df[[nm]]), logical(1))
  ]
  # Guardamos a variável de grupo (uma única coluna), se alguma foi escolhida.
  variavel_grupo <- if (is.null(quali_sup) || !nzchar(quali_sup)) NULL else quali_sup
  if (!is.null(variavel_grupo) && !variavel_grupo %in% names(df)) {
    variavel_grupo <- NULL
  }

  # Montamos a tabela com as variáveis ativas, preservando as linhas originais.
  ativas_bruto <- as.data.frame(lapply(df[, ativas_nomes, drop = FALSE], as.numeric))
  names(ativas_bruto) <- ativas_nomes
  # Contamos as células faltantes antes de qualquer remoção de linhas.
  n_faltantes <- sum(is.na(ativas_bruto))

  # Localizamos as linhas completas: a PCA não imputa sozinha, então removemos
  # por lista (listwise) e informamos quantas observações ficaram de fora.
  linhas_ok <- stats::complete.cases(ativas_bruto)
  ativas_ok <- ativas_bruto[linhas_ok, , drop = FALSE]
  # Guardamos o tamanho final da amostra analisada.
  n_usados <- nrow(ativas_ok)
  # Exigimos pelo menos três observações completas para a PCA ser estável.
  if (n_usados < 3) {
    stop("Após remover as linhas com dados faltantes restaram menos de três observações.",
         call. = FALSE)
  }
  # Guardamos quantas linhas foram excluídas por dados faltantes.
  n_excluidos <- nrow(df) - n_usados

  # Iniciamos a lista de avisos que acompanhará o resultado.
  avisos <- character()
  # Avisamos sobre variáveis constantes, que não carregam informação na PCA.
  variaveis_constantes <- ativas_nomes[
    vapply(ativas_ok, function(x) stats::sd(x) == 0, logical(1))
  ]
  if (length(variaveis_constantes)) {
    avisos <- c(avisos, sprintf(
      "Variável(is) constante(s) detectada(s): %s. Elas não contribuem para a PCA.",
      paste(variaveis_constantes, collapse = ", ")
    ))
  }
  # Avisamos sobre exclusões por dados faltantes.
  if (n_excluidos > 0) {
    avisos <- c(avisos, sprintf(
      "%d observação(ões) foi(ram) excluída(s) por dados faltantes nas variáveis ativas (%d célula(s) com NA).",
      n_excluidos, n_faltantes
    ))
  }

  # Definimos quantos eixos o FactoMineR deve guardar: no máximo p, e nunca
  # mais do que o posto da matriz (n - 1). Sem esse limite explícito, versões
  # recentes do FactoMineR truncam a tabela de autovalores em 5 eixos.
  ncp_val <- min(p, n_usados - 1L)

  # Montamos a tabela de trabalho: ativas + suplementares quantitativas + grupo.
  dados_pca <- ativas_ok
  # Acrescentamos as suplementares quantitativas, nas mesmas linhas completas.
  if (length(suplementares_nomes)) {
    suplementares_ok <- df[linhas_ok, suplementares_nomes, drop = FALSE]
    suplementares_ok <- as.data.frame(
      lapply(suplementares_ok, as.numeric)
    )
    names(suplementares_ok) <- suplementares_nomes
    dados_pca <- cbind(dados_pca, suplementares_ok)
  }
  # Guardamos os índices das suplementares dentro da tabela de trabalho.
  indices_quanti <- if (length(suplementares_nomes)) {
    p + seq_along(suplementares_nomes)
  } else {
    NULL
  }
  # Transformamos o grupo em fator; valores ausentes viram um nível explícito,
  # porque o dimdesc ignora grupos com NA.
  grupo_fator <- NULL
  indice_grupo <- NULL
  if (!is.null(variavel_grupo)) {
    grupo_bruto <- as.character(df[[variavel_grupo]][linhas_ok])
    grupo_bruto[is.na(grupo_bruto)] <- "(sem informação)"
    grupo_fator <- as.factor(grupo_bruto)
    # Exigimos pelo menos duas categorias para o grupo fazer sentido.
    if (nlevels(grupo_fator) < 2) {
      stop("A variável de grupo precisa ter pelo menos duas categorias.", call. = FALSE)
    }
    dados_pca[[variavel_grupo]] <- grupo_fator
    # Guardamos o índice da coluna de grupo na tabela de trabalho.
    indice_grupo <- ncol(dados_pca)
  }

  # Rodamos a PCA padronizada (ou só centralizada), com o grupo e as demais
  # suplementares declaradas como tal — elas não influenciam os eixos.
  args_pca <- list(
    X = dados_pca, scale.unit = isTRUE(scale), ncp = ncp_val, graph = FALSE
  )
  # Só passamos os índices das suplementares quando elas existem de fato.
  if (!is.null(indices_quanti)) args_pca$quanti.sup <- indices_quanti
  if (!is.null(indice_grupo)) args_pca$quali.sup <- indice_grupo
  pca <- do.call(FactoMineR::PCA, args_pca)

  # Guardamos a tabela de autovalores (variância de cada componente).
  autovalores <- data.frame(
    componente = seq_len(ncp_val),
    autovalor = pca$eig[, 1],
    pct_variancia = pca$eig[, 2],
    pct_acumulada = pca$eig[, 3]
  )

  # Conferimos com a função básica do R: os autovalores são os desvios ao quadrado.
  pca_base <- stats::prcomp(ativas_ok, center = TRUE, scale. = isTRUE(scale))
  # Comparamos as duas contas lado a lado (devem ser iguais).
  n_conf <- min(ncp_val, 4)
  conferencia <- data.frame(
    componente = seq_len(n_conf),
    factominer = pca$eig[seq_len(n_conf), 1],
    prcomp = pca_base$sdev[seq_len(n_conf)]^2
  )

  # Calculamos o modelo do bastão quebrado (broken stick) para cada componente.
  bastao_quebrado <- sapply(seq_len(ncp_val), function(j) sum(1 / (j:p)) / p * 100)

  # O Kaiser é apenas uma referência: autovalor médio 1 equivale a 100/p %.
  kaiser_referencia <- 100 / p

  # ---- Permutação: significância dos eixos e das cargas (R base) -------------
  # A permutação é o critério em destaque. Ela roda em R base (permutar_pca),
  # então está sempre disponível; só um erro inesperado (base degenerada) a
  # derruba, e nesse caso recuamos para o bastão quebrado com um aviso.
  limite_permutacao <- rep(NA_real_, ncp_val)
  p_permutacao <- rep(NA_real_, ncp_val)
  permutacao <- NULL
  permutacao_mensagem <- NULL
  permutacao_disponivel <- FALSE

  # Rodamos a permutação sobre as mesmas linhas completas que entraram na PCA,
  # com o mesmo preparo (padronizar ou só centralizar) e a semente registrada.
  permutacao <- tryCatch(
    permutar_pca(ativas_ok, padronizar = isTRUE(scale), nperm = 999L,
                 seed = as.integer(seed), n_eixos_cargas = min(2L, ncp_val)),
    error = function(e) NULL
  )

  # Se a permutação rodou, extraímos o limite superior (97,5 %) da variância e
  # o p-valor de cada eixo; senão, avisamos e seguimos com o bastão quebrado.
  if (!is.null(permutacao)) {
    permutacao_disponivel <- TRUE
    n_eixos_disp <- min(ncp_val, length(permutacao$p_valor))
    limite_permutacao[seq_len(n_eixos_disp)] <-
      permutacao$limite_pct[seq_len(n_eixos_disp), 2]
    p_permutacao[seq_len(n_eixos_disp)] <- permutacao$p_valor[seq_len(n_eixos_disp)]
  } else {
    permutacao_mensagem <- "A permutação não pôde ser calculada para esta base; a retenção usa apenas o bastão quebrado e o Kaiser (referência)."
    avisos <- c(avisos, permutacao_mensagem)
  }

  # Juntamos os três critérios numa tabela única de retenção.
  retencao <- data.frame(
    componente = seq_len(ncp_val),
    observado = autovalores$pct_variancia,
    bastao = bastao_quebrado,
    permutacao = limite_permutacao,
    p_permutacao = p_permutacao,
    kaiser = kaiser_referencia
  )
  # Decidimos a retenção com case_when: a permutação manda quando disponível;
  # sem ela, recuamos para o bastão quebrado. O Kaiser fica só como referência.
  retencao$decisao <- dplyr::case_when(
    !permutacao_disponivel & retencao$observado > retencao$bastao ~ "Reter (bastão)",
    !permutacao_disponivel                                        ~ "Não reter (bastão)",
    is.na(retencao$p_permutacao)                                  ~ "Sem teste",
    retencao$p_permutacao < 0.05                                  ~ "Reter",
    TRUE                                                          ~ "Não reter"
  )
  # Contamos quantos eixos a permutação sustenta (p < 0,05).
  eixos_significativos <- if (permutacao_disponivel) {
    sum(!is.na(p_permutacao) & p_permutacao < 0.05)
  } else {
    NULL
  }

  # Montamos a tabela de cargas com o limite permutado (97,5 %), quando houver.
  cargas_df <- NULL
  if (!is.null(permutacao)) {
    # Quantos eixos tiveram as cargas testadas (em geral, os dois primeiros).
    n_eixos_cargas <- permutacao$n_eixos_cargas
    nomes_eixos <- paste("Eixo", seq_len(n_eixos_cargas))
    # Índice de carga observado: variáveis nas linhas, eixos nas colunas.
    cargas_obs <- permutacao$idx_obs[, seq_len(n_eixos_cargas), drop = FALSE]
    # Limite permutado (97,5 %) das cargas, na mesma disposição.
    limites_obs <- permutacao$limite_cargas[, seq_len(n_eixos_cargas), drop = FALSE]
    # Empilhamos variável x eixo numa tabela longa; carga supera o limite = significativa.
    cargas_df <- data.frame(
      variavel = rep(colnames(ativas_ok), n_eixos_cargas),
      eixo = rep(nomes_eixos, each = p),
      carga = as.numeric(cargas_obs),
      limite = as.numeric(limites_obs)
    ) |>
      mutate(significativa = case_when(
        carga > limite ~ "Significativa",
        TRUE           ~ "Não significativa"
      ))
  }

  # Pedimos ao FactoMineR a descrição automática dos dois primeiros eixos.
  descricao_eixos <- tryCatch(
    dimdesc(pca, axes = 1:2, proba = 0.05),
    error = function(e) NULL
  )

  # Guardamos a matriz de correlações entre as variáveis ativas.
  correlacoes <- suppressWarnings(stats::cor(ativas_ok))

  # Montamos a tabela de correlações das variáveis (ativas e suplementares) com os eixos.
  correlacoes_eixos <- as.data.frame(pca$var$cor[, 1:2, drop = FALSE])
  # Acrescentamos a linha de cada suplementar quantitativa, quando houver.
  if (!is.null(indices_quanti)) {
    linhas_sup <- as.data.frame(pca$quanti.sup$cor[, 1:2, drop = FALSE])
    rownames(linhas_sup) <- suplementares_nomes
    correlacoes_eixos <- rbind(correlacoes_eixos, linhas_sup)
  }
  names(correlacoes_eixos) <- c("Eixo 1", "Eixo 2")

  # Guardamos a razão de correlação (eta2) do grupo com cada eixo, se houver grupo.
  eta2 <- if (!is.null(indice_grupo)) pca$quali.sup$eta2 else NULL

  # Listamos os grupos com menos de três unidades: sem elipse possível.
  grupos_pequenos <- NULL
  if (!is.null(grupo_fator)) {
    contagem <- table(grupo_fator)
    grupos_pequenos <- contagem[contagem < 3]
    if (!length(grupos_pequenos)) grupos_pequenos <- NULL
  }

  # Comparamos a PCA completa com a PCA imputada, quando pedido e possível.
  imputacao <- NULL
  imputacao_mensagem <- NULL
  if (isTRUE(comparar_imputacao)) {
    if (n_faltantes == 0) {
      # Sem NA não há o que imputar; registramos o motivo.
      imputacao_mensagem <- "Não há dados faltantes nas variáveis ativas; a comparação com imputação não se aplica."
    } else if (!requireNamespace("missMDA", quietly = TRUE)) {
      # Sem o pacote, registramos o motivo.
      imputacao_mensagem <- "O pacote missMDA não está instalado (install.packages('missMDA')); a comparação com imputação foi ignorada."
    } else {
      # Estimamos, por validação cruzada, quantos eixos usar na imputação.
      n_eixos_imp <- tryCatch(
        missMDA::estim_ncpPCA(as.matrix(ativas_bruto), scale = TRUE,
                              ncp.max = min(5, p, nrow(ativas_bruto) - 2))$ncp,
        error = function(e) 2
      )
      # Preenchemos as falhas com a PCA iterativa regularizada.
      imputado_ok <- tryCatch(
        missMDA::imputePCA(as.matrix(ativas_bruto), ncp = n_eixos_imp,
                           scale = TRUE)$completeObs,
        error = function(e) NULL
      )
      if (!is.null(imputado_ok)) {
        # Rodamos a PCA na tabela imputada, sem suplementares.
        pca_imputada <- tryCatch(
          FactoMineR::PCA(imputado_ok, scale.unit = TRUE, ncp = ncp_val, graph = FALSE),
          error = function(e) NULL
        )
        if (!is.null(pca_imputada)) {
          # Comparamos as coordenadas do eixo 1 nas mesmas linhas completas.
          comparacao_imputacao <- data.frame(
            completo = pca$ind$coord[, 1],
            imputado = pca_imputada$ind$coord[linhas_ok, 1]
          )
          # Medimos a concordância (o sinal do eixo é arbitrário, por isso o abs).
          concordancia_imputacao <- abs(stats::cor(
            comparacao_imputacao$completo, comparacao_imputacao$imputado
          ))
          # Guardamos tudo num único objeto para a figura e para o relato.
          imputacao <- list(
            n_faltantes = n_faltantes,
            n_eixos = n_eixos_imp,
            concordancia = concordancia_imputacao,
            comparacao = comparacao_imputacao
          )
        } else {
          imputacao_mensagem <- "A imputação (missMDA) não convergiu para esta base."
          avisos <- c(avisos, imputacao_mensagem)
        }
      } else {
        imputacao_mensagem <- "A imputação (missMDA) não convergiu para esta base."
        avisos <- c(avisos, imputacao_mensagem)
      }
    }
  }

  # Devolvemos todos os resultados num único objeto organizado.
  list(
    variaveis = ativas_nomes,
    variaveis_suplementares = suplementares_nomes,
    variavel_grupo = variavel_grupo,
    padronizar = isTRUE(scale),
    seed = as.integer(seed),
    comparar_imputacao = isTRUE(comparar_imputacao),
    n_original = nrow(df),
    n_usados = n_usados,
    n_excluidos = n_excluidos,
    n_faltantes = n_faltantes,
    p = p,
    n_eixos = ncp_val,
    pca = pca,
    conferencia = conferencia,
    autovalores = autovalores,
    retencao = retencao,
    eixos_significativos = eixos_significativos,
    permutacao_disponivel = permutacao_disponivel,
    permutacao_mensagem = permutacao_mensagem,
    cargas = cargas_df,
    descricao_eixos = descricao_eixos,
    correlacoes = correlacoes,
    correlacoes_eixos = correlacoes_eixos,
    eta2 = eta2,
    grupo_fator = grupo_fator,
    indice_grupo = indice_grupo,
    grupos_pequenos = grupos_pequenos,
    imputacao = imputacao,
    imputacao_mensagem = imputacao_mensagem,
    avisos = avisos
  )
}

#' Formata a tabela de autovalores e variância explicada.
mostrar_pca_var <- function(r) {
  tibble::tibble(
    `Componente` = paste0("PC", r$autovalores$componente),
    `Autovalor` = round(r$autovalores$autovalor, 4),
    `Variância explicada (%)` = round(r$autovalores$pct_variancia, 2),
    `Variância acumulada (%)` = round(r$autovalores$pct_acumulada, 2)
  )
}

#' Formata a conferência entre FactoMineR e a função básica prcomp.
mostrar_pca_conferencia <- function(r) {
  tibble::tibble(
    `Componente` = paste0("PC", r$conferencia$componente),
    `Autovalor (FactoMineR)` = round(r$conferencia$factominer, 4),
    `Autovalor (prcomp)` = round(r$conferencia$prcomp, 4)
  )
}

#' Formata a tabela de retenção com os três critérios e a decisão.
mostrar_pca_retencao <- function(r) {
  tibble::tibble(
    `Componente` = paste0("PC", r$retencao$componente),
    `Observado (%)` = round(r$retencao$observado, 2),
    `Bastão quebrado (%)` = round(r$retencao$bastao, 2),
    `Permutação (95 %)` = ifelse(is.na(r$retencao$permutacao), "-",
                                 fmt_pca(r$retencao$permutacao)),
    `p (permutação)` = ifelse(is.na(r$retencao$p_permutacao), "-",
                              formatC(r$retencao$p_permutacao, format = "f",
                                      digits = 3, decimal.mark = ",")),
    `Kaiser (referência) (%)` = round(r$retencao$kaiser, 2),
    `Decisão` = r$retencao$decisao
  )
}

#' Formata a tabela de cargas com o limite dos dados permutados; sem permutação,
#' recorre às correlações das variáveis com os dois primeiros eixos.
mostrar_pca_cargas <- function(r) {
  if (!is.null(r$cargas)) {
    tibble::tibble(
      `Eixo` = r$cargas$eixo,
      `Variável` = r$cargas$variavel,
      `Índice de carga` = round(r$cargas$carga, 3),
      `Limite permutado (97,5 %)` = round(r$cargas$limite, 3),
      `Significância` = r$cargas$significativa
    )
  } else {
    tab <- as.data.frame(r$correlacoes_eixos)
    tab$Variável <- rownames(tab)
    rownames(tab) <- NULL
    tibble::as_tibble(tab[, c("Variável", "Eixo 1", "Eixo 2")]) |>
      dplyr::rename(`Correlação com o eixo 1` = `Eixo 1`,
                    `Correlação com o eixo 2` = `Eixo 2`)
  }
}

#' Formata a descrição automática dos eixos (dimdesc) numa tabela única.
mostrar_pca_dimdesc <- function(r) {
  dd <- r$descricao_eixos
  if (is.null(dd)) return(NULL)
  linhas <- list()
  # Percorremos os dois eixos descritos pelo dimdesc.
  for (eixo in c("Dim.1", "Dim.2")) {
    bloco <- dd[[eixo]]
    # Pulamos eixos sem descrição (não deveria acontecer, mas protege).
    if (is.null(bloco)) next
    # Juntamos as variáveis quantitativas significativas do eixo.
    if (!is.null(bloco$quanti) && nrow(bloco$quanti) > 0) {
      linhas[[length(linhas) + 1]] <- data.frame(
        Eixo = eixo,
        Tipo = "Variável",
        Item = rownames(bloco$quanti),
        `Correlação` = round(bloco$quanti$correlation, 3),
        `p-valor` = round(bloco$quanti$p.value, 4),
        check.names = FALSE
      )
    }
    # Juntamos os grupos significativos do eixo (R² da variável de grupo).
    if (!is.null(bloco$quali)) {
      # Com uma única variável de grupo o dimdesc devolve um vetor c(R2, p.value).
      tab_quali <- if (is.numeric(bloco$quali)) {
        data.frame(R2 = bloco$quali[["R2"]], p.value = bloco$quali[["p.value"]],
                   row.names = r$variavel_grupo %||% "grupo")
      } else {
        as.data.frame(bloco$quali)
      }
      if (nrow(tab_quali) > 0 && all(c("R2", "p.value") %in% names(tab_quali))) {
        linhas[[length(linhas) + 1]] <- data.frame(
          Eixo = eixo,
          Tipo = "Grupo",
          Item = rownames(tab_quali),
          `Correlação` = round(tab_quali$R2, 3),
          `p-valor` = round(tab_quali$p.value, 4),
          check.names = FALSE
        )
      }
    }
  }
  # Devolvemos a tabela empilhada (ou NULL se nada foi significativo).
  if (!length(linhas)) return(NULL)
  do.call(rbind, linhas)
}

#' Desenha a matriz de correlações ordenada, só com o triângulo inferior.
grafico_pca_correlacoes <- function(r) {
  # Sem correlações válidas (ex.: variável constante) não há figura.
  if (any(!is.finite(r$correlacoes))) return(NULL)
  ggcorrplot(r$correlacoes, hc.order = TRUE, type = "lower",
             lab = TRUE, lab_size = 2.6,
             colors = c("#0F3B5F", "white", "#E76F51"),
             outline.color = "white", legend.title = "r") +
    labs(title = "Correlações entre as variáveis ativas") +
    theme(plot.title = element_text(face = "bold", color = "#0F3B5F"))
}

#' Desenha o gráfico de sedimentação com os três critérios sobrepostos, com a
#' permutação em destaque (linha mais grossa e cor principal da paleta).
grafico_pca_retencao <- function(r) {
  # Passamos os três critérios de referência para o formato longo.
  criterios <- r$retencao |>
    select(componente, bastao, permutacao, kaiser) |>
    pivot_longer(-componente, names_to = "criterio", values_to = "limite") |>
    filter(!is.na(limite)) |>
    mutate(criterio = case_when(
      criterio == "bastao"     ~ "Bastão quebrado",
      criterio == "permutacao" ~ "Permutação (95 %)",
      criterio == "kaiser"     ~ "Kaiser (autovalor = 1)"
    ))
  # Separamos a permutação das demais linhas para dar a ela o destaque visual.
  criterios_perm <- filter(criterios, criterio == "Permutação (95 %)")
  criterios_outros <- filter(criterios, criterio != "Permutação (95 %)")
  # Desenhamos as barras observadas.
  fig <- ggplot(r$retencao, aes(x = componente, y = observado)) +
    geom_col(fill = "#2E7D8F", width = 0.7)
  # Acrescentamos as linhas dos critérios de referência (bastão e Kaiser).
  if (nrow(criterios_outros)) {
    fig <- fig +
      geom_line(data = criterios_outros,
                aes(y = limite, color = criterio, group = criterio),
                linewidth = 0.9) +
      geom_point(data = criterios_outros,
                 aes(y = limite, color = criterio), size = 1.8)
  }
  # Acrescentamos a linha da permutação, mais grossa e em cor de destaque.
  if (nrow(criterios_perm)) {
    fig <- fig +
      geom_line(data = criterios_perm,
                aes(y = limite, color = criterio, group = criterio),
                linewidth = 1.4) +
      geom_point(data = criterios_perm,
                 aes(y = limite, color = criterio), size = 2.4)
  }
  # Fixamos as cores das linhas e aplicamos o tema da curadoria.
  fig +
    scale_color_manual(values = c(
      "Bastão quebrado" = "#E76F51",
      "Kaiser (autovalor = 1)" = "#E89B3C",
      "Permutação (95 %)" = "#0F3B5F"
    )) +
    labs(title = "Quantos componentes reter?",
         x = "Componente principal", y = "Variância explicada (%)", color = NULL) +
    pca_tema + theme(legend.position = "top")
}

#' Monta as camadas das suplementares quantitativas: seta tracejada em cor
#' neutra, rótulo na ponta e entrada própria ("suplementar") na legenda.
#' Como elas não contribuem para os eixos, não entram no gradiente de
#' contribuição nem no de cos2. `fator` reproduz a escala das setas ativas no
#' biplot; no círculo de correlações vale 1.
camadas_sup_quanti <- function(r, fator = 1) {
  sup <- r$variaveis_suplementares
  if (!length(sup) || is.null(r$pca$quanti.sup)) return(NULL)
  cor <- as.data.frame(r$pca$quanti.sup$cor[, 1:2, drop = FALSE])
  names(cor) <- c("x", "y")
  cor$variavel <- rownames(cor)
  # Levamos as suplementares à mesma escala das setas ativas do biplot.
  cor$x <- cor$x * fator
  cor$y <- cor$y * fator
  # Rótulo um pouco além da ponta, do lado para onde a seta aponta.
  cor$hjust <- ifelse(cor$x >= 0, -0.2, 1.2)
  cor$vjust <- ifelse(cor$y >= 0, -0.4, 1.3)
  list(
    geom_segment(data = cor,
                 aes(x = 0, y = 0, xend = x, yend = y,
                     linetype = "suplementar"),
                 color = pca_cor_suplementar, linewidth = 0.7,
                 arrow = grid::arrow(length = grid::unit(0.08, "inches"))),
    geom_text(data = cor, aes(x = x, y = y, label = variavel),
              color = pca_cor_suplementar, size = 3.6,
              hjust = cor$hjust, vjust = cor$vjust),
    scale_linetype_manual(name = NULL, values = c("suplementar" = "dashed"))
  )
}

#' Recupera do gráfico pronto o fator de escala que o factoextra aplica às
#' setas das variáveis ativas no biplot (ponta da seta dividido pela correlação
#' com os eixos). Sem ele, as suplementares cairiam numa escala diferente das
#' ativas; com ele, todas compartilham o mesmo plano.
fator_setas_biplot <- function(p, r) {
  cor_var <- r$pca$var$cor[, 1:2, drop = FALSE]
  for (camada in p$layers) {
    d <- camada$data
    if (!inherits(camada$geom, "GeomSegment")) next
    if (!is.data.frame(d) || !all(c("name", "x", "y") %in% names(d))) next
    if (!all(d$name %in% rownames(cor_var))) next
    # A variável mais bem representada no plano evita divisão por valor ~zero.
    i <- which.max(cor_var[d$name, 1]^2 + cor_var[d$name, 2]^2)
    norma_seta <- sqrt(d$x[i]^2 + d$y[i]^2)
    norma_cor <- sqrt(sum(cor_var[d$name[i], 1:2]^2))
    if (norma_cor > 1e-8) return(norma_seta / norma_cor)
  }
  1
}

#' Desenha o círculo de correlações, colorido pela qualidade de representação
#' (cos2); as suplementares quantitativas entram como setas tracejadas neutras,
#' com entrada própria na legenda.
grafico_pca_circulo <- function(r) {
  p <- fviz_pca_var(r$pca, col.var = "cos2",
                    gradient.cols = c("#62B6B7", "#2E7D8F", "#0F3B5F"),
                    repel = TRUE) +
    labs(title = "Círculo de correlações", color = "cos2") + pca_tema
  p + camadas_sup_quanti(r)
}

#' Desenha as contribuições das variáveis aos eixos 1 e 2, lado a lado.
grafico_pca_contribuicoes <- function(r) {
  # Contribuições ao eixo 1.
  fig_contrib_1 <- fviz_contrib(r$pca, choice = "var", axes = 1,
                                fill = "#2E7D8F", color = "#2E7D8F") +
    labs(title = "Contribuições ao eixo 1", x = NULL, y = "Contribuição (%)") +
    pca_tema + theme(axis.text.x = element_text(angle = 0))
  # Contribuições ao eixo 2.
  fig_contrib_2 <- fviz_contrib(r$pca, choice = "var", axes = 2,
                                fill = "#E89B3C", color = "#E89B3C") +
    labs(title = "Contribuições ao eixo 2", x = NULL, y = "Contribuição (%)") +
    pca_tema + theme(axis.text.x = element_text(angle = 0))
  # Juntamos as duas contribuições lado a lado.
  fig_contrib_1 + fig_contrib_2
}

#' Desenha os indivíduos no plano 1-2; com grupo, pinta pelos níveis e desenha
#' as envoltórias escolhidas. Se a envoltória falhar (grupos muito pequenos),
#' recua para o mapa sem envoltórias em vez de interromper a análise.
grafico_pca_individuos <- function(r, ellipse_type = "convex") {
  # Com grupo: colorimos os pontos e desenhamos as envoltórias.
  if (!is.null(r$grupo_fator)) {
    cores <- paleta_grupos(levels(r$grupo_fator))
    fig_com_elipses <- tryCatch(
      fviz_pca_ind(r$pca, habillage = r$indice_grupo, addEllipses = TRUE,
                   ellipse.type = ellipse_type, palette = cores,
                   repel = TRUE, labelsize = 3, pointsize = 2) +
        labs(title = "Indivíduos no plano da PCA",
             color = r$variavel_grupo, fill = r$variavel_grupo,
             shape = r$variavel_grupo) + pca_tema,
      error = function(e) NULL
    )
    # Se as envoltórias falharam, desenhamos só os pontos coloridos.
    if (!is.null(fig_com_elipses)) return(fig_com_elipses)
    fviz_pca_ind(r$pca, habillage = r$indice_grupo, addEllipses = FALSE,
                 palette = cores, repel = TRUE, labelsize = 3, pointsize = 2) +
      labs(title = "Indivíduos no plano da PCA",
           color = r$variavel_grupo, fill = r$variavel_grupo,
           shape = r$variavel_grupo) + pca_tema
  }
  # Sem grupo: pontos uniformes na cor principal da paleta.
  fviz_pca_ind(r$pca, col.ind = "#2E7D8F", repel = TRUE,
               labelsize = 3, pointsize = 2) +
    labs(title = "Indivíduos no plano da PCA") + pca_tema
}

#' Desenha o biplot em duas versões: a clássica (setas cinza) e a avançada
#' (pontos preenchidos pelo grupo e setas coloridas pela contribuição).
#' Em ambas, as suplementares quantitativas entram como setas tracejadas
#' neutras — elas não contribuem para os eixos, então não podem ser coloridas
#' pela escala de contribuição.
grafico_pca_biplot <- function(r, versao = "classica", ellipse_type = "norm") {
  # Versão avançada exige grupo para preencher os pontos; sem grupo, clássica.
  if (identical(versao, "contribuicao") && !is.null(r$grupo_fator)) {
    cores <- paleta_grupos(levels(r$grupo_fator))
    p <- fviz_pca_biplot(r$pca,
                         geom.ind = "point",
                         fill.ind = r$grupo_fator,
                         col.ind = "black",
                         pointshape = 21,
                         pointsize = 2.8,
                         palette = cores,
                         addEllipses = TRUE,
                         ellipse.type = ellipse_type,
                         ellipse.level = 0.95,
                         ellipse.alpha = 0.12,
                         col.var = "contrib",
                         gradient.cols = c("#62B6B7", "#2E7D8F", "#0F3B5F"),
                         repel = TRUE,
                         invisible = "quali",
                         legend.title = list(fill = r$variavel_grupo,
                                             color = "Contribuição (%)")) +
      labs(title = "Biplot com elipses e contribuição das variáveis") + pca_tema
  } else if (!is.null(r$grupo_fator)) {
    # Versão clássica do Kassambara: pontos coloridos pelo grupo e elipses.
    cores <- paleta_grupos(levels(r$grupo_fator))
    p <- fviz_pca_biplot(r$pca,
                         col.ind = r$grupo_fator,
                         palette = cores,
                         addEllipses = TRUE,
                         ellipse.type = ellipse_type,
                         ellipse.level = 0.95,
                         ellipse.alpha = 0.12,
                         label = "var",
                         col.var = "grey20",
                         repel = TRUE,
                         invisible = "quali",
                         legend.title = r$variavel_grupo) +
      labs(title = "Biplot com elipses dos grupos") + pca_tema
  } else {
    # Sem grupo: biplot simples, só variáveis e indivíduos.
    p <- fviz_pca_biplot(r$pca, label = "var", col.var = "grey20",
                         repel = TRUE) +
      labs(title = "Biplot (indivíduos e variáveis)") + pca_tema
  }
  p + camadas_sup_quanti(r, fator = fator_setas_biplot(p, r))
}

#' Desenha as cargas das variáveis com o limite nulo marcado.
grafico_pca_cargas <- function(r) {
  if (is.null(r$cargas)) return(NULL)
  ggplot(r$cargas, aes(x = reorder(variavel, carga), y = carga,
                       fill = significativa)) +
    geom_col(width = 0.7) +
    geom_point(aes(y = limite), shape = 124, size = 5, color = "grey20") +
    coord_flip() +
    facet_wrap(~ eixo) +
    scale_fill_manual(values = c("Não significativa" = "grey75",
                                 "Significativa" = "#2E7D8F")) +
    labs(title = "Cargas das variáveis e limite dos dados permutados",
         x = NULL, y = "Índice de carga", fill = NULL) +
    pca_tema + theme(legend.position = "top")
}

#' Desenha a comparação entre o eixo 1 com dados completos e com imputação.
grafico_pca_imputacao <- function(r) {
  if (is.null(r$imputacao)) return(NULL)
  comparacao <- r$imputacao$comparacao
  ggplot(comparacao, aes(x = completo, y = imputado)) +
    geom_abline(slope = sign(stats::cor(comparacao))[1, 2], intercept = 0,
                color = "grey60", linetype = "dashed") +
    geom_point(color = "#2E7D8F", size = 2.2) +
    labs(title = sprintf("Eixo 1 com %d célula(s) imputada(s)",
                         r$imputacao$n_faltantes),
         x = "Coordenada com dados completos", y = "Coordenada com imputação") +
    pca_tema
}

#' Gera a síntese dos resultados da PCA em português, sem concluir além do que
#' a análise sustenta.
relatar_pca <- function(r) {
  # Frase de abertura: dimensão do problema e variáveis analisadas.
  frases <- sprintf(
    "Foi realizada uma Análise de Componentes Principais sobre %d variáveis ativas (%s) com N = %d observações.",
    r$p, paste(r$variaveis, collapse = ", "), r$n_usados
  )
  # Informamos as exclusões por dados faltantes, quando houve.
  if (r$n_excluidos > 0) {
    frases <- c(frases, sprintf(
      "%d observação(ões) foi(ram) excluída(s) por dados faltantes nas variáveis ativas.",
      r$n_excluidos
    ))
  }
  # Declaramos a escolha de padronização, que muda a interpretação.
  frases <- c(frases, if (r$padronizar) {
    "Os dados foram padronizados (PCA de correlação), o que dá o mesmo peso a variáveis de unidades diferentes."
  } else {
    "Os dados foram apenas centralizados (PCA de covariância), preservando as unidades originais das variáveis."
  })
  # Conclusão de retenção guiada pela permutação, com os demais critérios como referência.
  if (r$permutacao_disponivel) {
    k <- r$eixos_significativos
    if (!is.null(k) && k >= 1) {
      frases <- c(frases, sprintf(
        "A permutação (999 repetições, semente %d) sustenta a retenção de %d componente(s), que explicam conjuntamente %s %% da variância.",
        r$seed, k, fmt_pca(sum(r$autovalores$pct_variancia[seq_len(k)]))
      ))
    } else {
      frases <- c(frases,
        "Nenhum componente superou o limite dos dados permutados; a estrutura linear dos dados é fraca e os eixos devem ser interpretados com reserva.")
    }
    frases <- c(frases,
      "O bastão quebrado e o critério de Kaiser são apresentados apenas como referência.")
  } else {
    # Sem permutação, declaramos o critério substituto com transparência.
    bastao_k <- sum(r$retencao$observado > r$retencao$bastao)
    frases <- c(frases, sprintf(
      "A permutação não pôde ser calculada; a retenção foi avaliada pelo bastão quebrado, que sugere %d componente(s), com o Kaiser apenas como referência.",
      bastao_k
    ))
  }
  # Mencionamos as variáveis suplementares quantitativas, quando houver.
  if (length(r$variaveis_suplementares)) {
    correlacao_e1 <- r$correlacoes_eixos[r$variaveis_suplementares[1], "Eixo 1"]
    frases <- c(frases, sprintf(
      "A variável suplementar %s correlacionou-se r = %s com o eixo 1.",
      r$variaveis_suplementares[1], fmt_pca(correlacao_e1)
    ))
  }
  # Mencionamos o grupo suplementar e sua razão de correlação, quando houver.
  if (!is.null(r$variavel_grupo) && !is.null(r$eta2)) {
    frases <- c(frases, sprintf(
      "O grupo suplementar %s apresentou razão de correlação eta² = %s no eixo 1.",
      r$variavel_grupo, fmt_pca(r$eta2[1, 1])
    ))
  }
  # Mencionamos a concordância com a imputação, quando a comparação ocorreu.
  if (!is.null(r$imputacao)) {
    frases <- c(frases, sprintf(
      "Com %d célula(s) imputada(s) pela PCA iterativa (missMDA, %d eixo(s)), a concordância das coordenadas do eixo 1 com os dados completos foi r = %s; a imputação não cria informação nova, apenas estabiliza a projeção.",
      r$imputacao$n_faltantes, r$imputacao$n_eixos, fmt_pca(r$imputacao$concordancia)
    ))
  }
  # Devolvemos o relato como um parágrafo único.
  paste(frases, collapse = " ")
}

# ---- Formatação da tabela (identidade Ocean Gradient, saída docx) -----------
flextable_ocean_pca <- function(tab) {
  flextable::flextable(tab) |>
    flextable::theme_booktabs() |>
    flextable::bg(part = "header", bg = "#0F3B5F") |>
    flextable::color(part = "header", color = "white") |>
    flextable::bold(part = "header") |>
    flextable::font(fontname = "Times New Roman", part = "all") |>
    flextable::fontsize(size = 9, part = "all") |>
    flextable::align(align = "center", part = "all") |>
    flextable::align(j = 1, align = "left", part = "all") |>
    flextable::padding(padding = 4, part = "all") |>
    flextable::autofit()
}
