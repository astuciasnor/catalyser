# =============================================================================

utils::globalVariables(c(
  ".data", "Coluna", "Frequ\u00eancia", "Linha", "ic_inferior",
  "ic_superior", "id", "media", "momento", "valor"
))
# Funcoes de analise da CatalyseR
# -----------------------------------------------------------------------------
# Estas sao as funcoes que o ALUNO usa. Elas aparecem nos scripts do Projeto R
# exportado e tem ajuda em portugues: digite ?catalyser_anova, por exemplo.
#
# Nao confundir com as funcoes de inst/app/templates/, que sao internas da
# interface: aquelas a IDE chama, estas a pessoa chama.
#
# Convencao dos nomes:
#   catalyser_<analise>()  executa uma analise e devolve todos os componentes
#   catalyser_executar()   escolhe a analise certa a partir da configuracao
#   as demais sao apoio: preparo, conferencia e apresentacao
# =============================================================================

# Funções canônicas do Projeto R integrado da CatalyseR
# -----------------------------------------------------------------------------
# Este arquivo é copiado para o projeto exportado. Cada script de execução
# chama catalyser_executar() com a configuração registrada na IDE e a base
# correspondente. A camada de apresentação é deliberadamente separada.

catalyser_ou <- function(x, padrao) {
  if (is.null(x) || !length(x)) padrao else x
}

catalyser_num <- function(x, digitos = 3L) {
  if (!length(x) || is.na(x[[1]])) return("NA")
  formatC(as.numeric(x[[1]]), digits = digitos, format = "f", decimal.mark = ",")
}

catalyser_p <- function(x) {
  if (!length(x) || is.na(x[[1]])) return("p não disponível")
  if (x[[1]] < 0.001) "p < 0,001" else paste0("p = ", catalyser_num(x[[1]], 3L))
}

catalyser_colunas <- function(dados, colunas) {
  ausentes <- setdiff(unique(colunas), names(dados))
  if (length(ausentes)) {
    stop(
      sprintf("A base não contém a(s) coluna(s): %s.", paste(ausentes, collapse = ", ")),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

catalyser_formula <- function(resposta, preditor) {
  stats::reformulate(preditor, response = resposta)
}

#' Letras de diferenca a partir das comparacoes de Tukey
#'
#' Traduz uma tabela de comparacoes par a par nas letrinhas que aparecem sobre as
#' barras: **grupos que compartilham ao menos uma letra nao apresentaram
#' evidencia de diferenca entre si**. A letra "a" fica com o grupo de maior media.
#'
#' E a convencao usada em artigos de pesca e agronomia. O algoritmo e o classico
#' "inserir e absorver": comeca com todos os grupos numa letra so e, a cada par
#' com evidencia de diferenca, quebra as letras que contem os dois.
#'
#' @param pares Matriz 2 x n com os nomes dos dois grupos de cada comparacao.
#' @param p_ajustado Vetor de p ajustados, na mesma ordem das colunas de `pares`.
#' @param medias Vetor **nomeado** com a media de cada grupo.
#' @param alfa Limiar de decisao (padrao 0,05).
#' @return Vetor de caracteres nomeado por grupo.
#' @examples
#' medias <- c(alto = 30, medio = 20, baixo = 10)
#' pares <- matrix(
#'   c("medio", "alto", "baixo", "alto", "baixo", "medio"),
#'   nrow = 2
#' )
#' # Apenas os extremos diferem entre si:
#' catalyser_letras_tukey(pares, c(0.9, 0.001, 0.9), medias)
#' @export
catalyser_letras_tukey <- function(pares, p_ajustado, medias, alfa = 0.05) {
  grupos <- names(medias)
  if (!length(grupos)) return(character())
  if (length(grupos) == 1L) return(stats::setNames("a", grupos))

  colunas <- list(grupos)
  for (k in which(!is.na(p_ajustado) & p_ajustado < alfa)) {
    a <- pares[1, k]
    b <- pares[2, k]
    if (!(a %in% grupos) || !(b %in% grupos)) next
    novas <- list()
    for (col in colunas) {
      if (a %in% col && b %in% col) {
        novas[[length(novas) + 1L]] <- setdiff(col, a)
        novas[[length(novas) + 1L]] <- setdiff(col, b)
      } else {
        novas[[length(novas) + 1L]] <- col
      }
    }
    novas <- unique(lapply(Filter(function(x) length(x) > 0L, novas), sort))
    manter <- rep(TRUE, length(novas))
    for (i in seq_along(novas)) {
      for (j in seq_along(novas)) {
        if (i != j && manter[i] && manter[j] &&
            length(novas[[i]]) < length(novas[[j]]) &&
            all(novas[[i]] %in% novas[[j]])) {
          manter[i] <- FALSE
        }
      }
    }
    colunas <- novas[manter]
  }

  chave <- vapply(colunas, function(col) max(medias[col], na.rm = TRUE), numeric(1))
  colunas <- colunas[order(chave, decreasing = TRUE)]
  alfabeto <- if (length(colunas) <= length(letters)) letters else
    c(letters, paste0(rep(letters, each = length(letters)), letters))

  saida <- stats::setNames(rep("", length(grupos)), grupos)
  for (k in seq_along(colunas)) {
    for (g in colunas[[k]]) saida[[g]] <- paste0(saida[[g]], alfabeto[k])
  }
  saida
}

#' Valor mais frequente de um vetor
#'
#' O R tem `mean()` e `median()`, mas nao tem moda. Esta funcao preenche a lacuna
#' e e usada quando a trilha de preparo imputa um dado faltante pela moda.
#'
#' @param x Um vetor de qualquer tipo.
#' @return O valor mais frequente, ignorando os ausentes.
#' @examples
#' catalyser_moda(c("norte", "sul", "norte", NA))
#' @export
catalyser_moda <- function(x) {
  valores <- x[!is.na(x)]
  if (!length(valores)) return(NA)
  nomes <- names(sort(table(valores), decreasing = TRUE))
  utils::type.convert(nomes[[1]], as.is = TRUE)
}

#' Conferir uma base reconstruida contra a fotografia exportada
#'
#' O script `R/01_base_compartilhada.R` reconstroi a base a partir da planilha
#' bruta. A CatalyseR tambem exportou uma fotografia do que estava na tela. Esta
#' funcao compara as duas e diz, em portugues, se o projeto reproduz o que voce
#' viu.
#'
#' A comparacao e tolerante ao que a ida e volta pelo Excel muda sem alterar o
#' significado (um inteiro que volta como decimal, por exemplo) e intolerante ao
#' que importa: numero de linhas, nomes de colunas e valores.
#'
#' @param reconstruida A base que o script acabou de montar.
#' @param caminho_fotografia Caminho do `.rds` exportado pela CatalyseR.
#' @param rotulo Nome que aparece nas mensagens.
#' @return `TRUE` quando as bases sao equivalentes, `FALSE` caso contrario
#'   (invisivel nos dois casos). Nunca interrompe o relatorio.
#' @export
catalyser_conferir_base <- function(reconstruida, caminho_fotografia,
                                    rotulo = "base compartilhada") {
  if (!file.exists(caminho_fotografia)) {
    cat(sprintf("[%s] Fotografia ausente em '%s'; conferência não realizada.\n",
                rotulo, caminho_fotografia))
    return(invisible(FALSE))
  }
  fotografia <- as.data.frame(readRDS(caminho_fotografia))
  reconstruida <- as.data.frame(reconstruida)

  divergencias <- character()
  if (!identical(nrow(reconstruida), nrow(fotografia))) {
    divergencias <- c(divergencias, sprintf(
      "número de linhas (reconstruída: %d; fotografia: %d)",
      nrow(reconstruida), nrow(fotografia)
    ))
  }
  faltando <- setdiff(names(fotografia), names(reconstruida))
  sobrando <- setdiff(names(reconstruida), names(fotografia))
  if (length(faltando))
    divergencias <- c(divergencias, paste("colunas ausentes:", paste(faltando, collapse = ", ")))
  if (length(sobrando))
    divergencias <- c(divergencias, paste("colunas a mais:", paste(sobrando, collapse = ", ")))

  comuns <- intersect(names(fotografia), names(reconstruida))
  if (identical(nrow(reconstruida), nrow(fotografia))) {
    for (coluna in comuns) {
      a <- reconstruida[[coluna]]
      b <- fotografia[[coluna]]
      igual <- if (is.numeric(a) && is.numeric(b)) {
        isTRUE(all.equal(as.numeric(a), as.numeric(b), tolerance = 1e-8))
      } else {
        isTRUE(all.equal(as.character(a), as.character(b)))
      }
      if (!igual) divergencias <- c(divergencias, sprintf("valores da coluna '%s'", coluna))
    }
  }

  if (!length(divergencias)) {
    cat(sprintf(
      "[%s] Reconstruída a partir da planilha e idêntica à fotografia: %d linhas e %d colunas.\n",
      rotulo, nrow(reconstruida), ncol(reconstruida)
    ))
    return(invisible(TRUE))
  }
  cat(sprintf("[%s] ATENÇÃO — a reconstrução divergiu da fotografia em: %s.\n",
              rotulo, paste(divergencias, collapse = "; ")))
  cat(sprintf("[%s] Use a fotografia ('%s') como referência e reveja o preparo.\n",
              rotulo, caminho_fotografia))
  invisible(FALSE)
}

# Toda exclusão de dado faltante é explícita e contada. As análises que dependem
# de casos completos usam este helper para nunca descartar linha em silêncio.
#' Manter apenas os casos completos, contando o que saiu
#'
#' Remove as linhas que tem dado faltante em qualquer uma das colunas indicadas e
#' devolve, junto, quantas ficaram e quantas sairam. E o habito que a CatalyseR
#' adota em todas as analises: **nenhuma linha e descartada em silencio**.
#'
#' @param dados Um data.frame.
#' @param colunas Nomes das colunas que precisam estar preenchidas.
#' @return Lista com `dados` (o subconjunto completo), `n` e `descartadas`.
#' @examples
#' dados <- data.frame(x = c(1, 2, NA, 4), y = c("a", "b", "c", NA))
#' preparo <- catalyser_completos(dados, c("x", "y"))
#' preparo$n
#' preparo$descartadas
#' @export
catalyser_completos <- function(dados, colunas) {
  colunas <- unique(as.character(colunas))
  catalyser_colunas(dados, colunas)
  mantidas <- stats::complete.cases(dados[colunas])
  list(
    dados = dados[mantidas, , drop = FALSE],
    n = sum(mantidas),
    descartadas = sum(!mantidas)
  )
}

#' Formatar uma tabela no tema visual da CatalyseR
#'
#' Aplica a identidade Ocean Gradient a um data.frame, para que a tabela saia no
#' Word com a mesma cara das demais.
#'
#' @param x Um data.frame ou matriz.
#' @return Um objeto `flextable`, ou uma tabela `knitr::kable` se o pacote
#'   flextable nao estiver instalado.
#' @export
catalyser_tabela_ocean <- function(x) {
  x <- as.data.frame(x, check.names = FALSE)
  if (!requireNamespace("flextable", quietly = TRUE)) {
    return(knitr::kable(x, digits = 3))
  }
  ft <- flextable::flextable(x)
  ft <- flextable::theme_vanilla(ft)
  ft <- flextable::bg(ft, bg = "#0F3B5F", part = "header")
  ft <- flextable::color(ft, color = "white", part = "header")
  ft <- flextable::bold(ft, bold = TRUE, part = "header")
  ft <- flextable::bg(ft, bg = "#EAF4F4", part = "body")
  ft <- flextable::font(ft, fontname = "Calibri", part = "all")
  ft <- flextable::align(ft, align = "center", part = "all")
  ft <- flextable::autofit(ft)
  flextable::set_table_properties(ft, layout = "autofit")
}

#' Exibir um componente de resultado no formato certo
#'
#' Cada componente de uma analise pede uma forma de exibicao: tabela vira
#' flextable, grafico e impresso, texto e escrito. Esta funcao olha o que
#' recebeu e escolhe sozinha.
#'
#' @param x Um componente devolvido por [catalyser_executar()].
#' @return O proprio objeto, de forma invisivel, depois de exibi-lo.
#' @export
catalyser_mostrar <- function(x) {
  if (is.null(x)) {
    cat("*Componente não produzido por esta execução.*\n")
    return(invisible(NULL))
  }
  if (is.data.frame(x) || is.matrix(x) || is.table(x)) {
    return(catalyser_tabela_ocean(as.data.frame(x, check.names = FALSE)))
  }
  if (inherits(x, "ggplot")) {
    print(x)
    return(invisible(x))
  }
  if (is.function(x)) {
    x()
    return(invisible(NULL))
  }
  if (is.character(x)) {
    cat(paste(x, collapse = "\n"), "\n")
    return(invisible(x))
  }
  print(x)
  invisible(x)
}

#' Estatistica descritiva de uma ou mais variaveis
#'
#' @param dados Um data.frame.
#' @param p Lista com `variaveis` (nomes das colunas), `grupo` (opcional) e
#'   `metricas` (quais colunas exibir).
#' @return Lista com `narrativa`, `tabela` e `console`.
#' @export
catalyser_resumo_descritivo <- function(dados, p) {
  variaveis <- unique(as.character(p$variaveis))
  grupo <- catalyser_ou(p$grupo, "none")
  catalyser_colunas(dados, c(variaveis, if (!identical(grupo, "none")) grupo))

  uma_linha <- function(x, variavel, grupo_valor = NULL) {
    x_num <- suppressWarnings(as.numeric(x))
    data.frame(
      Variável = variavel,
      Grupo = if (is.null(grupo_valor)) "Todos" else as.character(grupo_valor),
      N = sum(!is.na(x_num)),
      Ausentes = sum(is.na(x_num)),
      Média = mean(x_num, na.rm = TRUE),
      Mediana = stats::median(x_num, na.rm = TRUE),
      `Desvio-padrão` = stats::sd(x_num, na.rm = TRUE),
      Variância = stats::var(x_num, na.rm = TRUE),
      Mínimo = min(x_num, na.rm = TRUE),
      Máximo = max(x_num, na.rm = TRUE),
      Q1 = unname(stats::quantile(x_num, 0.25, na.rm = TRUE)),
      Q3 = unname(stats::quantile(x_num, 0.75, na.rm = TRUE)),
      check.names = FALSE
    )
  }

  linhas <- list()
  for (variavel in variaveis) {
    if (identical(grupo, "none")) {
      linhas[[length(linhas) + 1L]] <- uma_linha(dados[[variavel]], variavel)
    } else {
      indices <- split(seq_len(nrow(dados)), as.character(dados[[grupo]]), drop = TRUE)
      for (nivel in names(indices)) {
        linhas[[length(linhas) + 1L]] <- uma_linha(
          dados[[variavel]][indices[[nivel]]], variavel, nivel
        )
      }
    }
  }
  tabela <- do.call(rbind, linhas)
  metricas <- p$metricas
  manter <- c("Variável", "Grupo")
  if (is.null(metricas) || isTRUE(metricas$n)) manter <- c(manter, "N")
  if (is.null(metricas) || isTRUE(metricas$nas)) manter <- c(manter, "Ausentes")
  if (is.null(metricas) || isTRUE(metricas$media)) manter <- c(manter, "Média")
  if (is.null(metricas) || isTRUE(metricas$mediana)) manter <- c(manter, "Mediana")
  if (is.null(metricas) || isTRUE(metricas$desvio_padrao)) manter <- c(manter, "Desvio-padrão")
  if (is.null(metricas) || isTRUE(metricas$variancia)) manter <- c(manter, "Variância")
  if (is.null(metricas) || isTRUE(metricas$minimo_maximo)) manter <- c(manter, "Mínimo", "Máximo")
  if (is.null(metricas) || isTRUE(metricas$quartis)) manter <- c(manter, "Q1", "Q3")
  tabela <- tabela[intersect(unique(manter), names(tabela))]

  list(
    narrativa = sprintf(
      "Foram resumidas %d variável(is) em %d linha(s) de resultado.",
      length(variaveis), nrow(tabela)
    ),
    tabela = tabela,
    console = utils::capture.output(print(tabela))
  )
}

#' Regressao linear ou logistica
#'
#' @param dados Um data.frame.
#' @param p Lista com `resposta`, `preditor` e, opcionalmente, `grupo` e
#'   `regressao_por_grupo`.
#' @param logistica `TRUE` para regressao logistica binaria.
#' @return Lista com `narrativa`, `tabela`, `grafico`, `pressupostos`,
#'   `diagnosticos`, `console` e `objeto`.
#' @export
catalyser_regressao <- function(dados, p, logistica = FALSE) {
  resposta <- p$resposta
  preditor <- p$preditor
  grupo <- catalyser_ou(p$grupo, "none")
  por_grupo <- isTRUE(p$regressao_por_grupo) && !identical(grupo, "none")
  colunas <- c(resposta, preditor, if (por_grupo) grupo)
  catalyser_colunas(dados, colunas)
  d <- dados[stats::complete.cases(dados[colunas]), , drop = FALSE]
  if (nrow(d) < 3L) stop("A regressão precisa de pelo menos três observações completas.", call. = FALSE)

  if (logistica) {
    y <- d[[resposta]]
    if (is.logical(y)) y <- as.integer(y)
    if (is.factor(y) || is.character(y)) {
      niveis <- unique(as.character(y[!is.na(y)]))
      if (length(niveis) != 2L) stop("A resposta logística precisa ter exatamente dois níveis.", call. = FALSE)
      y <- as.integer(factor(y, levels = niveis)) - 1L
    }
    if (!all(y %in% c(0, 1))) stop("A resposta logística deve estar codificada em 0/1.", call. = FALSE)
    d[[resposta]] <- y
  }

  ajustar <- function(df) {
    if (logistica) {
      stats::glm(catalyser_formula(resposta, preditor), data = df, family = stats::binomial())
    } else {
      stats::lm(catalyser_formula(resposta, preditor), data = df)
    }
  }
  tabela_coeficientes <- function(ajuste, nome_grupo = NULL) {
    tabela <- as.data.frame(summary(ajuste)$coefficients, check.names = FALSE)
    tabela$Termo <- rownames(tabela)
    rownames(tabela) <- NULL
    tabela <- tabela[c("Termo", setdiff(names(tabela), "Termo"))]
    if (!is.null(nome_grupo)) tabela <- cbind(Grupo = nome_grupo, tabela)
    tabela
  }

  if (por_grupo) {
    partes <- split(d, as.character(d[[grupo]]), drop = TRUE)
    insuficientes <- names(Filter(function(x) {
      nrow(x) < 3L || (logistica && length(unique(x[[resposta]])) < 2L)
    }, partes))
    if (length(insuficientes)) {
      stop(
        sprintf("Os grupos sem dados suficientes para a regressão são: %s.", paste(insuficientes, collapse = ", ")),
        call. = FALSE
      )
    }
    ajustes <- lapply(partes, ajustar)
    coeficientes <- do.call(rbind, Map(tabela_coeficientes, ajustes, names(ajustes)))
    rownames(coeficientes) <- NULL
    linhas_narrativa <- Map(function(ajuste, nome) {
      sm <- summary(ajuste)
      p_modelo <- sm$coefficients[min(2L, nrow(sm$coefficients)), ncol(sm$coefficients)]
      if (logistica) {
        sprintf("%s: N = %d, AIC = %s e %s para o preditor", nome, stats::nobs(ajuste), catalyser_num(stats::AIC(ajuste), 2L), catalyser_p(p_modelo))
      } else {
        sprintf("%s: N = %d, R² = %s e %s para o preditor", nome, stats::nobs(ajuste), catalyser_num(sm$r.squared), catalyser_p(p_modelo))
      }
    }, ajustes, names(ajustes))
    narrativa <- sprintf(
      "Foram ajustadas regressões de %s em função de %s por %s (%s).",
      resposta, preditor, grupo, paste(unlist(linhas_narrativa), collapse = "; ")
    )
    diagnosticos <- do.call(rbind, Map(function(ajuste, nome) {
      sm <- summary(ajuste)
      data.frame(
        Grupo = nome, N = stats::nobs(ajuste),
        R2 = if (logistica) NA_real_ else sm$r.squared,
        AIC = stats::AIC(ajuste), check.names = FALSE
      )
    }, ajustes, names(ajustes)))
    if (logistica) {
      pressupostos <- "A resposta foi validada como binária (0/1) em cada grupo. Examine observações influentes e a forma funcional."
    } else {
      pressupostos <- do.call(rbind, Map(function(ajuste, nome) {
        residuos <- stats::residuals(ajuste)
        shapiro <- if (length(residuos) >= 3L && length(residuos) <= 5000L) stats::shapiro.test(residuos) else NULL
        data.frame(
          Grupo = nome,
          W = if (is.null(shapiro)) NA_real_ else unname(shapiro$statistic),
          `p (Shapiro-Wilk)` = if (is.null(shapiro)) NA_real_ else shapiro$p.value,
          check.names = FALSE
        )
      }, ajustes, names(ajustes)))
    }
    console <- unlist(Map(function(ajuste, nome) {
      c(paste0("--- Grupo: ", nome, " ---"), utils::capture.output(print(summary(ajuste))))
    }, ajustes, names(ajustes)), use.names = FALSE)
    objeto <- ajustes
  } else {
    ajuste <- ajustar(d)
    sm <- summary(ajuste)
    coeficientes <- tabela_coeficientes(ajuste)
    p_modelo <- sm$coefficients[min(2L, nrow(sm$coefficients)), ncol(sm$coefficients)]

    if (logistica) {
      narrativa <- sprintf(
        "A regressão logística de %s em função de %s foi ajustada com %d observações (AIC = %s; %s para o preditor).",
        resposta, preditor, nrow(d), catalyser_num(stats::AIC(ajuste), 2L), catalyser_p(p_modelo)
      )
      diagnosticos <- data.frame(
        Indicador = c("N", "AIC", "Desvio residual"),
        Valor = c(nrow(d), stats::AIC(ajuste), stats::deviance(ajuste))
      )
      pressupostos <- "A resposta foi validada como binária (0/1). Examine observações influentes e a forma funcional antes da interpretação final."
    } else {
      narrativa <- sprintf(
        "A regressão linear de %s em função de %s foi ajustada com %d observações (R² = %s; %s para o preditor).",
        resposta, preditor, nrow(d), catalyser_num(sm$r.squared, 3L), catalyser_p(p_modelo)
      )
      shapiro <- if (nrow(d) >= 3L && nrow(d) <= 5000L) stats::shapiro.test(stats::residuals(ajuste)) else NULL
      pressupostos <- if (is.null(shapiro)) {
        "O teste de Shapiro-Wilk dos resíduos não foi calculado para este tamanho amostral."
      } else {
        sprintf("Normalidade dos resíduos (Shapiro-Wilk): W = %s; %s.", catalyser_num(shapiro$statistic), catalyser_p(shapiro$p.value))
      }
      diagnosticos <- data.frame(
        Indicador = c("N", "R²", "R² ajustado", "AIC"),
        Valor = c(nrow(d), sm$r.squared, sm$adj.r.squared, stats::AIC(ajuste))
      )
    }
    console <- utils::capture.output(print(sm))
    objeto <- ajuste
  }

  grafico <- NULL
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    estetica <- if (por_grupo) {
      ggplot2::aes(x = .data[[preditor]], y = .data[[resposta]], color = .data[[grupo]])
    } else {
      ggplot2::aes(x = .data[[preditor]], y = .data[[resposta]])
    }
    pontos <- if (por_grupo) {
      ggplot2::geom_point(alpha = 0.75)
    } else {
      ggplot2::geom_point(color = "#2E7D8F", alpha = 0.75)
    }
    curva <- if (por_grupo) {
      ggplot2::geom_smooth(
        method = if (logistica) "glm" else "lm",
        method.args = if (logistica) list(family = "binomial") else list(),
        se = TRUE
      )
    } else {
      ggplot2::geom_smooth(
        method = if (logistica) "glm" else "lm",
        method.args = if (logistica) list(family = "binomial") else list(),
        se = TRUE, color = "#E76F51"
      )
    }
    grafico <- ggplot2::ggplot(d, estetica) +
      pontos + curva +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::labs(x = preditor, y = resposta)
  }

  list(
    narrativa = narrativa,
    tabela = coeficientes,
    grafico = grafico,
    pressupostos = pressupostos,
    diagnosticos = diagnosticos,
    console = console,
    objeto = objeto
  )
}

#' Teste t de Student, nas tres variantes
#'
#' @param dados Um data.frame.
#' @param p Lista com `tipo_teste` (`"one_val"`, `"two_ind"` ou `"paired"`) e as
#'   variaveis correspondentes.
#' @return Lista com `narrativa`, `tabela`, `grafico`, `pressupostos`,
#'   `diagnosticos`, `console` e `objeto`.
#' @export
catalyser_teste_t <- function(dados, p) {
  tipo <- p$tipo_teste
  alternativa <- catalyser_ou(p$alternativa, "two.sided")
  conf <- as.numeric(catalyser_ou(p$nivel_confianca, 0.95))
  grafico <- NULL
  pressupostos <- NULL

  if (identical(tipo, "one_val")) {
    catalyser_colunas(dados, p$variavel)
    x <- dados[[p$variavel]]
    teste <- stats::t.test(x, mu = as.numeric(p$media_hipotetica), alternative = alternativa, conf.level = conf)
    valido <- x[!is.na(x)]
    if (length(valido) >= 3L && length(valido) <= 5000L) {
      sh <- stats::shapiro.test(valido)
      pressupostos <- sprintf("Normalidade da variável (Shapiro-Wilk): W = %s; %s.", catalyser_num(sh$statistic), catalyser_p(sh$p.value))
    }
    if (requireNamespace("ggplot2", quietly = TRUE)) {
      dg <- data.frame(valor = valido)
      grafico <- ggplot2::ggplot(dg, ggplot2::aes(x = valor)) +
        ggplot2::geom_histogram(bins = 15, fill = "#62B6B7", color = "white") +
        ggplot2::geom_vline(xintercept = as.numeric(p$media_hipotetica), color = "#E76F51", linetype = 2) +
        ggplot2::theme_minimal(base_size = 12) + ggplot2::labs(x = p$variavel, y = "Frequência")
    }
  } else if (identical(tipo, "two_ind")) {
    catalyser_colunas(dados, c(p$resposta, p$grupo))
    teste <- stats::t.test(
      catalyser_formula(p$resposta, p$grupo), data = dados,
      var.equal = isTRUE(p$variancias_iguais), alternative = alternativa, conf.level = conf
    )
    grupos <- split(dados[[p$resposta]], dados[[p$grupo]], drop = TRUE)
    sh <- lapply(grupos, function(x) {
      x <- x[!is.na(x)]
      if (length(x) >= 3L && length(x) <= 5000L) stats::shapiro.test(x)$p.value else NA_real_
    })
    pressupostos <- data.frame(Grupo = names(sh), `p (Shapiro-Wilk)` = unlist(sh), check.names = FALSE)
    if (requireNamespace("ggplot2", quietly = TRUE)) {
      grafico <- ggplot2::ggplot(dados, ggplot2::aes(x = .data[[p$grupo]], y = .data[[p$resposta]], fill = .data[[p$grupo]])) +
        ggplot2::geom_boxplot(alpha = 0.75, show.legend = FALSE) +
        ggplot2::scale_fill_manual(values = c("#2E7D8F", "#E89B3C", "#62B6B7", "#E76F51")) +
        ggplot2::theme_minimal(base_size = 12)
    }
  } else if (identical(tipo, "paired")) {
    catalyser_colunas(dados, c(p$variavel_1, p$variavel_2))
    teste <- stats::t.test(
      dados[[p$variavel_1]], dados[[p$variavel_2]], paired = TRUE,
      alternative = alternativa, conf.level = conf
    )
    diferencas <- dados[[p$variavel_1]] - dados[[p$variavel_2]]
    diferencas <- diferencas[!is.na(diferencas)]
    if (length(diferencas) >= 3L && length(diferencas) <= 5000L) {
      sh <- stats::shapiro.test(diferencas)
      pressupostos <- sprintf("Normalidade das diferenças (Shapiro-Wilk): W = %s; %s.", catalyser_num(sh$statistic), catalyser_p(sh$p.value))
    }
    if (requireNamespace("ggplot2", quietly = TRUE)) {
      dg <- data.frame(
        id = rep(seq_len(nrow(dados)), 2L),
        momento = factor(rep(c(p$variavel_1, p$variavel_2), each = nrow(dados)), levels = c(p$variavel_1, p$variavel_2)),
        valor = c(dados[[p$variavel_1]], dados[[p$variavel_2]])
      )
      grafico <- ggplot2::ggplot(dg, ggplot2::aes(x = momento, y = valor, group = id)) +
        ggplot2::geom_line(alpha = 0.25, color = "#2E7D8F") +
        ggplot2::geom_point(color = "#0F3B5F") + ggplot2::theme_minimal(base_size = 12)
    }
  } else {
    stop("Tipo de teste t não reconhecido.", call. = FALSE)
  }

  tabela <- data.frame(
    Estatística = unname(as.numeric(teste$statistic)),
    `Graus de liberdade` = unname(as.numeric(teste$parameter)),
    `p-valor` = teste$p.value,
    `IC inferior` = unname(teste$conf.int[1]),
    `IC superior` = unname(teste$conf.int[2]),
    check.names = FALSE
  )
  narrativa <- sprintf(
    "O teste t resultou em t = %s, gl = %s e %s; IC de %.0f%% [%s; %s].",
    catalyser_num(teste$statistic), catalyser_num(teste$parameter, 1L), catalyser_p(teste$p.value),
    100 * conf, catalyser_num(teste$conf.int[1]), catalyser_num(teste$conf.int[2])
  )
  list(
    narrativa = narrativa, tabela = tabela, grafico = grafico,
    pressupostos = pressupostos,
    diagnosticos = data.frame(Indicador = "Estimativa", Valor = unname(teste$estimate)[1]),
    console = utils::capture.output(print(teste)), objeto = teste
  )
}

#' ANOVA de um fator
#'
#' Compara a media de uma variavel numerica entre tres ou mais grupos e devolve,
#' de uma vez, tudo o que um relatorio precisa: resumo por grupo com media e
#' desvio-padrao, tabela da ANOVA, tamanhos de efeito, comparacoes de Tukey com
#' letras de diferenca, testes de pressupostos, grafico de barras e a narrativa
#' em portugues.
#'
#' Linhas com dado faltante na resposta ou no fator sao removidas e **contadas**:
#' o numero aparece em `diagnosticos`. Nada some em silencio.
#'
#' @param dados Um data.frame com as observacoes individuais (uma linha por
#'   individuo, nao medias ja calculadas).
#' @param p Lista de parametros com, no minimo, `resposta` (a variavel numerica)
#'   e `fator` (a variavel de grupo). Aceita ainda `nivel_confianca` (padrao
#'   0,95), `titulo_grafico`, `rotulo_x`, `rotulo_y` e `tema`.
#' @return Lista com `narrativa`, `descritivos`, `tabela`, `comparacoes`,
#'   `grafico`, `pressupostos`, `diagnosticos`, `console` e `objeto`.
#' @seealso [catalyser_letras_tukey()] para as letras de diferenca.
#' @examples
#' \dontrun{
#' resultado <- catalyser_anova(
#'   dados_analise,
#'   list(resposta = "profundidade_m", fator = "especie")
#' )
#' resultado$grafico       # barras com IC e letras
#' resultado$descritivos   # media +- DP e letras
#' cat(resultado$narrativa)
#' }
#' @export
catalyser_anova <- function(dados, p) {
  resposta <- p$resposta
  fator <- p$fator
  conf <- as.numeric(catalyser_ou(p$nivel_confianca, 0.95))
  texto_ou <- function(x, padrao) {
    x <- as.character(catalyser_ou(x, ""))
    if (!length(x) || !nzchar(trimws(x[[1]]))) padrao else x[[1]]
  }
  catalyser_colunas(dados, c(resposta, fator))
  if (!is.numeric(dados[[resposta]]))
    stop(sprintf("A resposta '%s' precisa ser numérica para a ANOVA.", resposta), call. = FALSE)
  if (identical(resposta, fator))
    stop("A resposta e o fator precisam ser variáveis diferentes.", call. = FALSE)

  preparo <- catalyser_completos(dados, c(resposta, fator))
  d <- preparo$dados[c(resposta, fator)]
  names(d) <- c("resposta", "fator")
  d$fator <- droplevels(as.factor(d$fator))
  excluidos <- preparo$descartadas
  if (nlevels(d$fator) < 2L)
    stop("A ANOVA precisa de pelo menos dois grupos com dados.", call. = FALSE)
  if (any(table(d$fator) < 2L))
    stop("Cada grupo precisa de pelo menos duas observações.", call. = FALSE)

  modelo <- stats::aov(resposta ~ fator, data = d)
  resumo <- summary(modelo)[[1]]
  df_entre <- resumo$Df[1]; df_dentro <- resumo$Df[2]
  sq_entre <- resumo$`Sum Sq`[1]; sq_dentro <- resumo$`Sum Sq`[2]
  qm_entre <- resumo$`Mean Sq`[1]; qm_dentro <- resumo$`Mean Sq`[2]
  f_anova <- resumo$`F value`[1]; p_anova <- resumo$`Pr(>F)`[1]
  sq_total <- sq_entre + sq_dentro
  eta2 <- sq_entre / sq_total
  omega2 <- (sq_entre - df_entre * qm_dentro) / (sq_total + qm_dentro)

  niveis <- levels(d$fator)
  resumo_grupos <- do.call(rbind, lapply(niveis, function(nivel) {
    valores <- d$resposta[d$fator == nivel]
    n_grupo <- length(valores)
    margem <- if (n_grupo > 1L) {
      stats::qt(1 - (1 - conf) / 2, df = n_grupo - 1L) *
        stats::sd(valores) / sqrt(n_grupo)
    } else NA_real_
    data.frame(
      grupo = nivel, n = n_grupo, media = mean(valores),
      desvio = stats::sd(valores),
      ic_inferior = mean(valores) - margem,
      ic_superior = mean(valores) + margem,
      stringsAsFactors = FALSE
    )
  }))
  rownames(resumo_grupos) <- NULL

  tabela <- data.frame(
    `Fonte de variação` = c("Entre grupos (fator)", "Dentro dos grupos (resíduos)", "Total"),
    `Graus de liberdade` = c(df_entre, df_dentro, df_entre + df_dentro),
    `Soma de quadrados` = c(sq_entre, sq_dentro, sq_total),
    `Quadrado médio` = c(qm_entre, qm_dentro, NA_real_),
    `F` = c(f_anova, NA_real_, NA_real_),
    `p-valor` = c(p_anova, NA_real_, NA_real_),
    check.names = FALSE, stringsAsFactors = FALSE
  )

  tukey <- tryCatch(stats::TukeyHSD(modelo, conf.level = conf), error = function(e) NULL)
  comparacoes <- if (is.null(tukey)) NULL else {
    bruto <- as.data.frame(tukey[[1]])
    saida <- data.frame(
      `Par comparado` = rownames(bruto),
      `Diferença estimada` = bruto$diff,
      `IC inferior` = bruto$lwr,
      `IC superior` = bruto$upr,
      `p ajustado` = bruto$`p adj`,
      `Evidência` = ifelse(bruto$`p adj` < 0.05,
                           "Há evidência de diferença", "Sem evidência de diferença"),
      check.names = FALSE, stringsAsFactors = FALSE
    )
    rownames(saida) <- NULL
    saida
  }

  # --- Letras de diferença ---------------------------------------------------
  # Os pares são reconstruídos a partir dos níveis, e não quebrando o nome
  # "b-a" no hífen: nomes de espécie podem conter hífen ou espaço.
  medias_por_grupo <- stats::setNames(resumo_grupos$media, resumo_grupos$grupo)
  letras <- if (!is.null(comparacoes) && length(niveis) > 1L) {
    combos <- utils::combn(niveis, 2L)
    posicao <- match(paste0(combos[2, ], "-", combos[1, ]), comparacoes[["Par comparado"]])
    catalyser_letras_tukey(
      pares = combos[c(2L, 1L), , drop = FALSE],
      p_ajustado = comparacoes[["p ajustado"]][posicao],
      medias = medias_por_grupo
    )
  } else {
    stats::setNames(rep("a", length(niveis)), niveis)
  }
  resumo_grupos$letras <- unname(letras[resumo_grupos$grupo])

  formatar <- function(x, dig = 2L) vapply(
    x, function(v) if (is.na(v)) "-" else
      formatC(as.numeric(v), format = "f", digits = dig, decimal.mark = ","),
    character(1), USE.NAMES = FALSE
  )
  descritivos <- data.frame(
    Grupo = resumo_grupos$grupo,
    n = resumo_grupos$n,
    `Média ± DP` = paste(formatar(resumo_grupos$media), "±", formatar(resumo_grupos$desvio)),
    `IC da média` = ifelse(
      is.na(resumo_grupos$ic_inferior), "não estimável",
      sprintf("[%s; %s]", formatar(resumo_grupos$ic_inferior), formatar(resumo_grupos$ic_superior))
    ),
    `Diferença` = resumo_grupos$letras,
    check.names = FALSE, stringsAsFactors = FALSE
  )
  names(descritivos)[4] <- sprintf("IC %.0f%% da média", 100 * conf)

  residuos <- stats::residuals(modelo)
  shapiro <- if (length(residuos) >= 3L && length(residuos) <= 5000L) {
    tryCatch(stats::shapiro.test(residuos), error = function(e) NULL)
  } else NULL
  levene <- if (requireNamespace("car", quietly = TRUE)) {
    tryCatch(car::leveneTest(resposta ~ fator, data = d, center = stats::median),
             error = function(e) NULL)
  } else NULL
  bartlett <- tryCatch(stats::bartlett.test(resposta ~ fator, data = d), error = function(e) NULL)

  estat <- function(x, campo) {
    if (is.null(x)) return(NA_real_)
    unname(as.numeric(x[[campo]][[1]]))
  }
  pressupostos <- data.frame(
    Pressuposto = c(
      "Normalidade dos resíduos (Shapiro-Wilk)",
      "Homogeneidade de variâncias (Levene, centro na mediana)",
      "Homogeneidade de variâncias (Bartlett — informação adicional)"
    ),
    `Estatística` = c(
      estat(shapiro, "statistic"),
      if (is.null(levene)) NA_real_ else suppressWarnings(as.numeric(levene[["F value"]][1])),
      estat(bartlett, "statistic")
    ),
    `p-valor` = c(
      estat(shapiro, "p.value"),
      if (is.null(levene)) NA_real_ else suppressWarnings(as.numeric(levene[["Pr(>F)"]][1])),
      estat(bartlett, "p.value")
    ),
    check.names = FALSE, stringsAsFactors = FALSE
  )

  diagnosticos <- data.frame(
    Indicador = c("n analisado", "Casos excluídos", "Grupos", "Eta quadrado (η²)", "Ômega quadrado (ω²)"),
    Valor = c(nrow(d), excluidos, nlevels(d$fator), eta2, omega2),
    check.names = FALSE
  )

  grafico <- NULL
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    grafico_dados <- resumo_grupos
    grafico_dados$fator <- factor(grafico_dados$grupo, levels = niveis)
    cores <- rep(c("#0F3B5F", "#2E7D8F", "#62B6B7", "#E89B3C", "#E76F51"),
                 length.out = length(niveis))
    tema <- switch(
      texto_ou(p$tema, "minimal"),
      classic = ggplot2::theme_classic(base_size = 12),
      bw = ggplot2::theme_bw(base_size = 12),
      gray = ggplot2::theme_gray(base_size = 12),
      light = ggplot2::theme_light(base_size = 12),
      ggplot2::theme_minimal(base_size = 12)
    )
    # Barras com IC e letras. O eixo Y começa em zero porque, em barras, o que se
    # compara é o comprimento; médias negativas desativam esse piso.
    topo <- max(c(grafico_dados$ic_superior, grafico_dados$media), na.rm = TRUE)
    piso <- if (min(c(grafico_dados$ic_inferior, grafico_dados$media), na.rm = TRUE) < 0) {
      NA_real_
    } else 0
    grafico <- ggplot2::ggplot(grafico_dados, ggplot2::aes(x = fator, y = media)) +
      ggplot2::geom_col(ggplot2::aes(fill = fator), width = 0.66,
                        show.legend = FALSE, alpha = 0.92) +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = ic_inferior, ymax = ic_superior),
        width = 0.16, linewidth = 0.8, color = "#0F3B5F"
      ) +
      ggplot2::geom_text(
        ggplot2::aes(y = ic_superior, label = letras),
        vjust = -0.7, fontface = "bold", size = 4.6, color = "#0F3B5F"
      ) +
      ggplot2::scale_fill_manual(values = cores) +
      ggplot2::scale_y_continuous(
        limits = c(piso, topo * 1.18),
        expand = ggplot2::expansion(mult = c(0, 0.02))
      ) +
      tema +
      ggplot2::labs(
        title = texto_ou(p$titulo_grafico, sprintf("%s por %s", resposta, fator)),
        x = texto_ou(p$rotulo_x, fator),
        y = texto_ou(p$rotulo_y, resposta),
        subtitle = sprintf(
          "Barras = média; hastes = IC %.0f%% da média; mesma letra = sem evidência de diferença (Tukey)",
          100 * conf
        )
      )
  }

  pares <- if (is.null(comparacoes)) character() else
    comparacoes[["Par comparado"]][comparacoes[["p ajustado"]] < 0.05]

  # A narrativa segue a mesma regra da interface (relatar_anova em
  # templates/funcoes_anova.R): não repete o que as tabelas ao lado já mostram.
  # As médias por grupo ficam no resumo por grupo, e os p de Shapiro-Wilk e
  # Levene, na tabela de pressupostos. Se um dos dois textos mudar, o outro
  # precisa mudar junto — o relatório Word usa este.
  leitura_efeito <- if (is.na(eta2)) "-" else if (eta2 < 0.01) "muito pequeno" else
    if (eta2 < 0.06) "pequeno" else if (eta2 < 0.14) "médio" else "grande"

  narrativa <- paste0(
    sprintf(
      "A pergunta analisada foi se a média de '%s' difere entre os %d grupos de '%s' (%s). ",
      resposta, nlevels(d$fator), fator, paste(niveis, collapse = ", ")
    ),
    sprintf(
      "Entraram %d observações completas%s. ",
      nrow(d),
      if (excluidos > 0)
        sprintf(", depois de excluir %d linha(s) com dados faltantes na resposta ou no fator", excluidos)
      else " (nenhuma linha foi excluída por dados faltantes)"
    ),
    if (!is.na(p_anova) && p_anova < 0.05)
      "Rejeitou-se H0 de igualdade das médias: "
    else
      "Não houve evidência suficiente para rejeitar H0 de igualdade das médias: ",
    sprintf("F(%d; %d) = %s, %s. ", df_entre, df_dentro, catalyser_num(f_anova), catalyser_p(p_anova)),
    sprintf(
      "O fator explicou %s%% da variação da resposta (η² = %s; ω² = %s), efeito %s pela convenção de Cohen. ",
      catalyser_num(100 * eta2, 1L), catalyser_num(eta2), catalyser_num(omega2), leitura_efeito
    ),
    if (!is.na(p_anova) && p_anova < 0.05) {
      if (length(pares))
        sprintf(
          paste0(
            "Entre os %d pares comparados por Tukey, houve evidência de diferença em %s. ",
            "A interpretação principal decorre da ANOVA global e do plano analítico definido antes da coleta, não de uma varredura de pares. "
          ),
          nrow(comparacoes), paste(pares, collapse = "; ")
        )
      else
        paste0(
          "Nenhum par isolado apresentou evidência de diferença no teste de Tukey — ",
          "situação possível quando o efeito global é modesto e o ajuste para comparações múltiplas é conservador. "
        )
    } else {
      "Isso não significa que as médias sejam iguais; significa que estes dados não permitiram detectar diferença. "
    },
    "As médias e dispersões de cada grupo estão no resumo por grupo; os testes de ",
    "normalidade dos resíduos e de homogeneidade de variâncias, na tabela de pressupostos, ",
    "que deve ser lida junto com os gráficos de resíduos e o Q-Q plot. ",
    "A ANOVA compara médias entre grupos observados; por si só, não estabelece relação de causa e efeito."
  )

  console <- c(
    utils::capture.output(print(summary(modelo))),
    "",
    if (is.null(tukey)) "Tukey HSD indisponível." else utils::capture.output(print(tukey)),
    "",
    if (is.null(shapiro)) "Shapiro-Wilk não calculado." else utils::capture.output(print(shapiro)),
    "",
    if (is.null(levene)) "Teste de Levene indisponível (pacote 'car' ausente)." else utils::capture.output(print(levene))
  )

  list(
    narrativa = narrativa,
    descritivos = descritivos,
    tabela = tabela,
    comparacoes = comparacoes,
    grafico = grafico,
    pressupostos = pressupostos,
    diagnosticos = diagnosticos,
    console = console,
    objeto = modelo
  )
}

#' Grafico de linhas
#'
#' Monta um grafico de linhas com o visual da CatalyseR. Antes de desenhar,
#' descarta as observacoes sem valor em X ou em Y e informa quantas foram: o
#' ggplot2 faria isso com um aviso discreto, e aqui a exclusao fica explicita.
#'
#' @param dados Um data.frame.
#' @param p Lista com `x` e `y` (nomes das colunas) e, opcionalmente, `grupo`
#'   para uma linha por categoria, `mostrar_pontos`, `espessura_linha`, `tema`,
#'   `posicao_legenda`, `titulo_grafico`, `rotulo_x` e `rotulo_y`.
#' @return Lista com `grafico` (objeto ggplot) e `observacoes` (quantas entraram
#'   e quantas foram descartadas).
#' @examples
#' \dontrun{
#' resultado <- catalyser_linhas(
#'   dados_analise,
#'   list(x = "id", y = "comprimento_cm", grupo = "none", mostrar_pontos = TRUE)
#' )
#' resultado$grafico
#' resultado$observacoes
#' }
#' @export
catalyser_linhas <- function(dados, p) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("O pacote ggplot2 é necessário para o gráfico.", call. = FALSE)
  texto_ou <- function(x, padrao) {
    x <- as.character(catalyser_ou(x, ""))
    if (!length(x) || !nzchar(trimws(x[[1]]))) padrao else x[[1]]
  }
  grupo <- catalyser_ou(p$grupo, "none")
  # O ggplot2 descartaria as linhas incompletas com um aviso discreto. Aqui a
  # exclusão é explícita e contada, como na ANOVA.
  preparo <- catalyser_completos(dados, c(p$x, p$y, if (!identical(grupo, "none")) grupo))
  dados <- preparo$dados
  if (!nrow(dados))
    stop("Nenhuma observação tem os dois eixos preenchidos; o gráfico ficaria vazio.", call. = FALSE)
  aes <- if (identical(grupo, "none")) {
    ggplot2::aes(x = .data[[p$x]], y = .data[[p$y]], group = 1)
  } else {
    ggplot2::aes(x = .data[[p$x]], y = .data[[p$y]], color = .data[[grupo]], group = .data[[grupo]])
  }
  camada_linha <- if (identical(grupo, "none")) {
    ggplot2::geom_line(
      linewidth = as.numeric(catalyser_ou(p$espessura_linha, 1)), color = "#0F3B5F"
    )
  } else {
    ggplot2::geom_line(linewidth = as.numeric(catalyser_ou(p$espessura_linha, 1)))
  }
  grafico <- ggplot2::ggplot(dados, aes) +
    camada_linha
  if (isTRUE(p$mostrar_pontos)) {
    grafico <- grafico + if (identical(grupo, "none")) {
      ggplot2::geom_point(size = 2.4, color = "#2E7D8F")
    } else {
      ggplot2::geom_point(size = 2.4)
    }
  }
  if (!identical(grupo, "none")) {
    cores <- rep(
      c("#0F3B5F", "#2E7D8F", "#62B6B7", "#E89B3C", "#E76F51"),
      length.out = length(unique(dados[[grupo]]))
    )
    grafico <- grafico + ggplot2::scale_color_manual(values = cores)
  }
  tema <- switch(
    texto_ou(p$tema, "minimal"),
    classic = ggplot2::theme_classic(base_size = 14),
    bw = ggplot2::theme_bw(base_size = 14),
    gray = ggplot2::theme_gray(base_size = 14),
    light = ggplot2::theme_light(base_size = 14),
    ggplot2::theme_minimal(base_size = 14)
  )
  grafico <- grafico +
    tema +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 16, color = "#0F3B5F"),
      plot.subtitle = ggplot2::element_text(size = 12, color = "#495057"),
      axis.title = ggplot2::element_text(color = "#212529"),
      legend.position = texto_ou(p$posicao_legenda, "right"),
      legend.title = ggplot2::element_text(face = "bold")
    ) +
    ggplot2::labs(
      title = texto_ou(p$titulo_grafico, sprintf("%s ao longo de %s", p$y, p$x)),
      x = texto_ou(p$rotulo_x, p$x),
      y = texto_ou(p$rotulo_y, p$y),
      color = if (!identical(grupo, "none")) grupo else NULL
    )
  list(
    grafico = grafico,
    observacoes = data.frame(
      Indicador = c("Observações plotadas", "Descartadas por dados faltantes"),
      Valor = c(preparo$n, preparo$descartadas),
      check.names = FALSE
    )
  )
}

#' Teste qui-quadrado de independencia
#'
#' @param dados Um data.frame.
#' @param p Lista com `var_row` e `var_col`, ou `tabela` ja pronta.
#' @return Lista com `narrativa`, `tabela`, `grafico`, `diagnosticos`, `console`
#'   e `objeto`.
#' @export
catalyser_qui_quadrado <- function(dados, p) {
  if (!is.null(p$tabela)) {
    tab <- as.table(p$tabela)
  } else {
    catalyser_colunas(dados, c(p$var_row, p$var_col))
    tab <- table(dados[[p$var_row]], dados[[p$var_col]], useNA = "no")
  }
  teste <- suppressWarnings(stats::chisq.test(tab, correct = isTRUE(p$yates)))
  tabela <- as.data.frame.matrix(tab, stringsAsFactors = FALSE)
  tabela <- cbind(Categoria = rownames(tabela), tabela, row.names = NULL)
  residuos <- as.data.frame.matrix(round(teste$stdres, 3), stringsAsFactors = FALSE)
  residuos <- cbind(Categoria = rownames(residuos), residuos, row.names = NULL)
  grafico <- NULL
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    dg <- as.data.frame(tab)
    names(dg) <- c("Linha", "Coluna", "Frequência")
    grafico <- ggplot2::ggplot(dg, ggplot2::aes(x = Linha, y = Frequência, fill = Coluna)) +
      ggplot2::geom_col(position = "dodge") +
      ggplot2::scale_fill_manual(values = c("#2E7D8F", "#E89B3C", "#62B6B7", "#E76F51")) +
      ggplot2::theme_minimal(base_size = 12)
  }
  list(
    narrativa = sprintf(
      "O teste qui-quadrado resultou em χ² = %s, gl = %s e %s.",
      catalyser_num(teste$statistic), catalyser_num(teste$parameter, 0L), catalyser_p(teste$p.value)
    ),
    tabela = tabela, grafico = grafico, diagnosticos = residuos,
    console = utils::capture.output(print(teste)), objeto = teste
  )
}

#' Analise de componentes principais
#'
#' @param dados Um data.frame.
#' @param p Lista com `variaveis` e `padronizar`.
#' @return Lista com `narrativa`, `tabela`, `grafico`, `diagnosticos`, `console`
#'   e `objeto`.
#' @export
catalyser_pca <- function(dados, p) {
  variaveis <- unique(as.character(p$variaveis))
  catalyser_colunas(dados, variaveis)
  x <- dados[variaveis]
  x <- x[stats::complete.cases(x), , drop = FALSE]
  x[] <- lapply(x, as.numeric)
  if (nrow(x) < 3L || ncol(x) < 2L) stop("A PCA precisa de pelo menos três linhas e duas variáveis.", call. = FALSE)
  ajuste <- stats::prcomp(x, center = TRUE, scale. = isTRUE(p$padronizar))
  variancia <- 100 * ajuste$sdev^2 / sum(ajuste$sdev^2)
  tabela <- data.frame(
    Componente = paste0("PC", seq_along(variancia)),
    `Variância (%)` = variancia,
    `Acumulada (%)` = cumsum(variancia),
    check.names = FALSE
  )
  grafico <- function() stats::biplot(ajuste, main = "Análise de Componentes Principais")
  diagnosticos <- as.data.frame(ajuste$rotation, check.names = FALSE)
  diagnosticos <- cbind(Variável = rownames(diagnosticos), diagnosticos, row.names = NULL)
  list(
    narrativa = sprintf("Os dois primeiros componentes explicaram %s%% da variância total.", catalyser_num(sum(utils::head(variancia, 2L)), 1L)),
    tabela = tabela, grafico = grafico, diagnosticos = diagnosticos,
    console = utils::capture.output(summary(ajuste)), objeto = ajuste
  )
}

#' Agrupamento hierarquico
#'
#' @param dados Um data.frame.
#' @param p Lista com `variaveis`, `distancia`, `ligacao`, `numero_grupos`,
#'   `padronizar` e, opcionalmente, `variavel_rotulo`.
#' @return Lista com `narrativa`, `tabela`, `grafico`, `diagnosticos`, `console`
#'   e `objeto`.
#' @export
catalyser_hca <- function(dados, p) {
  variaveis <- unique(as.character(p$variaveis))
  coluna_rotulo <- catalyser_ou(p$variavel_rotulo, "none")
  usar_rotulos <- isTRUE(p$mostrar_rotulos) && !identical(coluna_rotulo, "none")
  catalyser_colunas(dados, c(variaveis, if (usar_rotulos) coluna_rotulo))
  x <- dados[variaveis]
  completos <- stats::complete.cases(x)
  x <- x[completos, , drop = FALSE]
  x[] <- lapply(x, as.numeric)
  rotulos <- if (usar_rotulos) {
    make.unique(as.character(dados[[coluna_rotulo]][completos]))
  } else {
    make.unique(rownames(x))
  }
  rownames(x) <- rotulos
  if (isTRUE(p$padronizar)) x <- as.data.frame(scale(x))
  distancia <- stats::dist(x, method = catalyser_ou(p$distancia, "euclidean"))
  ajuste <- stats::hclust(distancia, method = catalyser_ou(p$ligacao, "complete"))
  k <- min(max(2L, as.integer(catalyser_ou(p$numero_grupos, 2L))), nrow(x))
  grupos <- stats::cutree(ajuste, k = k)
  tabela <- data.frame(Observação = rownames(x), Grupo = grupos, row.names = NULL)
  diagnosticos <- as.data.frame(table(Grupo = grupos), stringsAsFactors = FALSE)
  grafico <- function() {
    plot(
      ajuste, main = "Agrupamento hierárquico", xlab = "", sub = "",
      labels = if (isTRUE(p$mostrar_rotulos)) ajuste$labels else FALSE
    )
    stats::rect.hclust(ajuste, k = k, border = c("#2E7D8F", "#E89B3C", "#E76F51", "#62B6B7"))
  }
  list(
    narrativa = sprintf("O agrupamento hierárquico classificou %d observações em %d grupos.", nrow(x), k),
    tabela = tabela, grafico = grafico, diagnosticos = diagnosticos,
    console = utils::capture.output(print(ajuste)), objeto = ajuste
  )
}

#' Executar uma analise registrada na CatalyseR
#'
#' Cada vez que voce clica em "Adicionar aos resultados" na CatalyseR, a
#' configuracao daquela analise e congelada: qual base, quais variaveis, qual
#' nivel de confianca. Esta funcao pega essa configuracao e refaz a analise,
#' devolvendo tudo pronto para o relatorio.
#'
#' E a funcao que os scripts do projeto exportado chamam na ultima secao. Voce
#' raramente precisa dela para aprender: para isso, leia a PARTE 2 do script,
#' onde a analise aparece escrita passo a passo.
#'
#' @param execucao A configuracao congelada, lida de
#'   `metadados/registro_execucoes.rds`.
#' @param dados A base sobre a qual a analise roda.
#' @return Uma lista com os componentes da analise. Quais existem depende do
#'   metodo; na ANOVA, por exemplo: `narrativa`, `descritivos`, `tabela`,
#'   `comparacoes`, `grafico`, `pressupostos`, `diagnosticos` e `objeto` (o
#'   modelo do R, para quem quiser inspecionar).
#' @examples
#' \dontrun{
#' analises <- readRDS(file.path("metadados", "registro_execucoes.rds"))
#' resultado <- catalyser_executar(analises[["execucao_0001"]], dados)
#' resultado$grafico
#' resultado$tabela
#' }
#' @export
catalyser_executar <- function(execucao, dados = NULL) {
  p <- execucao$parametros
  tipo <- execucao$tipo
  if (identical(tipo, "grafico_linhas") &&
      (is.null(p$titulo_grafico) || !nzchar(trimws(as.character(p$titulo_grafico))))) {
    p$titulo_grafico <- execucao$titulo
  }
  resultado <- switch(
    tipo,
    estatistica_descritiva = catalyser_resumo_descritivo(dados, p),
    regressao_linear = catalyser_regressao(dados, p, logistica = FALSE),
    regressao_logistica = catalyser_regressao(dados, p, logistica = TRUE),
    teste_t_one_val = catalyser_teste_t(dados, p),
    teste_t_two_ind = catalyser_teste_t(dados, p),
    teste_t_paired = catalyser_teste_t(dados, p),
    anova_um_fator = catalyser_anova(dados, p),
    grafico_linhas = catalyser_linhas(dados, p),
    qui_quadrado = catalyser_qui_quadrado(dados, p),
    pca = catalyser_pca(dados, p),
    hca = catalyser_hca(dados, p),
    stop(sprintf("O tipo de execução '%s' ainda não possui replay no exportador integrado.", tipo), call. = FALSE)
  )
  resultado$execucao <- execucao
  class(resultado) <- c("resultado_catalyser", "list")
  resultado
}
