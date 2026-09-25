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
#' O chunk `tratar` do `relatorio.qmd` exportado reconstroi a base a partir da
#' planilha bruta. A CatalyseR tambem exportou uma fotografia do que estava na tela. Esta
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
#' Aplica a identidade Ocean Gradient a um data.frame: cabecalho azul-marinho
#' com letras brancas, sem grade interna, primeira coluna a esquerda. E o mesmo
#' tema de `flextable_ocean()`, no EAPADados e no EAPACaderno, para que a tabela
#' saia igual no livro, no caderno do pesquisador e no Word exportado pela IDE.
#'
#' A fonte e Times New Roman porque e a fonte do modelo de pagina do Word
#' (`custom-reference.docx`): assim a tabela nao destoa do corpo do texto.
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
  ft <- flextable::theme_booktabs(ft)                          # linha so em cima e embaixo
  ft <- flextable::bg(ft, bg = "#0F3B5F", part = "header")     # cabecalho azul-marinho (NAVY)
  ft <- flextable::color(ft, color = "white", part = "header")
  ft <- flextable::bold(ft, bold = TRUE, part = "header")
  ft <- flextable::font(ft, fontname = "Times New Roman", part = "all")
  ft <- flextable::fontsize(ft, size = 10, part = "all")
  ft <- flextable::align(ft, align = "center", part = "all")   # tudo centralizado ...
  ft <- flextable::align(ft, j = 1, align = "left", part = "all")  # ... menos a primeira coluna
  ft <- flextable::padding(ft, padding = 4, part = "all")
  flextable::autofit(ft)
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
        formula = y ~ x,
        se = TRUE
      )
    } else {
      ggplot2::geom_smooth(
        method = if (logistica) "glm" else "lm",
        method.args = if (logistica) list(family = "binomial") else list(),
        formula = y ~ x,
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

#' Regressão de Poisson ou Binomial Negativa para uma contagem
#'
#' Uma contagem não se comporta como uma medida contínua: zero é possível, os
#' valores são inteiros e a variabilidade costuma crescer junto com a média.
#' Esta função ajusta Poisson como ponto de partida e Binomial Negativa quando a
#' dispersão observada pede uma margem de variação maior.
#'
#' @param dados Um data.frame.
#' @param p Lista com `resposta`, `preditores`, `offset` opcional e
#'   `nivel_confianca`.
#' @param familia `"poisson"` ou `"binomial_negativa"`.
#' @return Lista com narrativa, tabela de coeficientes, diagnóstico, gráfico,
#'   pressupostos, console e o objeto do modelo.
#' @export
catalyser_regressao_contagem <- function(dados, p,
                                          familia = c("poisson", "binomial_negativa")) {
  # Escolha explicitamente qual motor estatístico será usado.
  familia <- match.arg(familia)
  # Leia os nomes escolhidos na interface e descarte repetições acidentais.
  resposta <- as.character(catalyser_ou(p$resposta, ""))
  preditores <- unique(as.character(catalyser_ou(p$preditores, character())))
  offset <- as.character(catalyser_ou(p$offset, ""))
  usar_offset <- isTRUE(p$usar_offset) && nzchar(offset)
  # A resposta e pelo menos um preditor são necessários para a pergunta proposta.
  if (!nzchar(resposta)) stop("Escolha a variável de contagem.", call. = FALSE)
  if (!length(preditores)) stop("Escolha ao menos um preditor.", call. = FALSE)
  if (resposta %in% preditores) {
    stop("A resposta não pode ser usada também como preditor.", call. = FALSE)
  }
  if (usar_offset && offset %in% preditores) {
    stop("O offset representa exposição e não deve ser usado também como preditor.", call. = FALSE)
  }
  # Confira todos os nomes antes de filtrar linhas da base.
  colunas <- c(resposta, preditores, if (usar_offset) offset)
  catalyser_colunas(dados, colunas)
  # Remova somente os casos incompletos nas variáveis que entram neste ajuste.
  preparo <- catalyser_completos(dados, colunas)
  d <- preparo$dados
  if (nrow(d) < 3L) {
    stop("A regressão de contagem precisa de pelo menos três observações completas.", call. = FALSE)
  }
  # Garanta que a resposta realmente seja uma contagem inteira e não negativa.
  y <- d[[resposta]]
  if (!is.numeric(y) || any(!is.finite(y)) || any(y < 0) || any(abs(y - round(y)) > 1e-8)) {
    stop("A resposta deve conter contagens inteiras não negativas, como número de indivíduos ou ocorrências.", call. = FALSE)
  }
  # O offset representa exposição e, por isso, precisa ser numérico e estritamente positivo.
  offset_log <- NULL
  if (usar_offset) {
    exposicao <- d[[offset]]
    if (!is.numeric(exposicao) || any(!is.finite(exposicao)) || any(exposicao <= 0)) {
      stop("O offset precisa ser numérico e maior que zero em todas as linhas usadas no modelo.", call. = FALSE)
    }
    offset_log <- log(exposicao)
    d$.catalyser_offset_log <- offset_log
  }
  # Monte a fórmula com os nomes reais das colunas, sem concatenar texto inseguro.
  termos_formula <- c(preditores, if (usar_offset) "offset(.catalyser_offset_log)")
  formula_modelo <- stats::reformulate(termos_formula, response = resposta)
  # Ajuste o Poisson ou a Binomial Negativa com o offset explicitamente na fórmula.
  modelo <- tryCatch(
    if (identical(familia, "poisson")) {
      stats::glm(formula_modelo, data = d, family = stats::poisson())
    } else {
      MASS::glm.nb(formula_modelo, data = d)
    },
    error = function(e) {
      stop(sprintf("Não foi possível ajustar o modelo de contagem: %s", conditionMessage(e)), call. = FALSE)
    }
  )
  # A dispersão de Pearson compara a variação residual com a esperada pelo modelo.
  graus_liberdade <- stats::df.residual(modelo)
  if (graus_liberdade <= 0L) {
    stop("Faltam graus de liberdade residuais. Reduza o número de preditores ou use mais observações.", call. = FALSE)
  }
  residuos_pearson <- stats::residuals(modelo, type = "pearson")
  dispersao <- sum(residuos_pearson^2, na.rm = TRUE) / graus_liberdade
  # Construa intervalos de Wald na escala do log e depois volte à razão de taxas.
  nivel <- as.numeric(catalyser_ou(p$nivel_confianca, 0.95))
  if (!is.finite(nivel) || nivel <= 0 || nivel >= 1) nivel <- 0.95
  z_critico <- stats::qnorm((1 + nivel) / 2)
  coeficientes_brutos <- as.data.frame(summary(modelo)$coefficients, check.names = FALSE)
  nomes_colunas <- names(coeficientes_brutos)
  estimativa <- coeficientes_brutos[[1]]
  erro_padrao <- coeficientes_brutos[[2]]
  estatistica_z <- coeficientes_brutos[[3]]
  p_valor <- coeficientes_brutos[[length(nomes_colunas)]]
  tabela <- data.frame(
    Termo = rownames(coeficientes_brutos),
    Estimativa = estimativa,
    `Erro-padrão` = erro_padrao,
    `z` = estatistica_z,
    `p-valor` = p_valor,
    `Razão de taxas` = exp(estimativa),
    `IC 95% inferior` = exp(estimativa - z_critico * erro_padrao),
    `IC 95% superior` = exp(estimativa + z_critico * erro_padrao),
    check.names = FALSE
  )
  rownames(tabela) <- NULL
  # Resuma ajuste e dispersão em uma tabela que também segue para o Projeto R.
  indicadores <- c("N", "Linhas excluídas", "AIC", "Desvio residual", "GL residuais", "Dispersão de Pearson")
  valores <- c(nrow(d), preparo$descartadas, stats::AIC(modelo), stats::deviance(modelo), graus_liberdade, dispersao)
  if (identical(familia, "binomial_negativa")) {
    indicadores <- c(indicadores, "Theta")
    valores <- c(valores, modelo$theta)
  }
  diagnosticos <- data.frame(Indicador = indicadores, Valor = valores, check.names = FALSE)
  # Traduza a dispersão em uma próxima decisão, sem declarar que o modelo é verdade final.
  if (identical(familia, "poisson")) {
    orientacao <- dplyr::case_when(
      dispersao <= 1.5 ~ sprintf("A dispersão de Pearson foi %s, próxima de 1. Não há sinal forte de superdispersão por este diagnóstico; ainda examine o desenho e os resíduos.", catalyser_num(dispersao, 2L)),
      dispersao <= 2 ~ sprintf("A dispersão de Pearson foi %s, acima de 1. Há variação residual extra; compare com a Regressão Binomial Negativa antes de concluir.", catalyser_num(dispersao, 2L)),
      TRUE ~ sprintf("A dispersão de Pearson foi %s, bem acima de 1. O Poisson pode subestimar a incerteza; ajuste também a Regressão Binomial Negativa.", catalyser_num(dispersao, 2L))
    )
  } else {
    orientacao <- sprintf("A Binomial Negativa permite variação maior que a média. A dispersão de Pearson foi %s; interprete-a junto aos resíduos, ao theta e ao delineamento.", catalyser_num(dispersao, 2L))
  }
  # Descreva o que foi ajustado sem transformar associação estatística em causalidade.
  nome_familia <- if (identical(familia, "poisson")) "Poisson" else "Binomial Negativa"
  narrativa <- sprintf(
    "A regressão de %s para '%s' foi ajustada com %d observações e %d preditor(es)%s. AIC = %s e dispersão de Pearson = %s. %s",
    nome_familia, resposta, nrow(d), length(preditores),
    if (usar_offset) sprintf(", com offset log(%s)", offset) else "",
    catalyser_num(stats::AIC(modelo), 2L), catalyser_num(dispersao, 2L), orientacao
  )
  # Liste as verificações que dependem de ciência do estudo, não apenas de software.
  pressupostos <- c(
    "Cada linha deve representar uma unidade de contagem definida pelo delineamento.",
    "A independência vem da amostragem, do experimento ou do tratamento de agrupamentos, não de um p-valor.",
    "A forma log-linear e a ausência de padrões fortes nos resíduos devem ser avaliadas antes da interpretação final.",
    if (usar_offset) sprintf("O offset '%s' precisa representar exposição mensurada sem erro relevante.", offset),
    if (identical(familia, "poisson")) "A dispersão de Pearson orienta a comparação com Binomial Negativa." else "A Binomial Negativa acomoda superdispersão, mas não corrige dependência entre observações."
  )
  # Desenhe os resíduos de Pearson contra os valores ajustados com a paleta Ocean.
  grafico <- NULL
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    dados_grafico <- data.frame(
      ajustados = stats::fitted(modelo),
      residuos_pearson = residuos_pearson
    )
    grafico <- ggplot2::ggplot(dados_grafico, ggplot2::aes(x = .data$ajustados, y = .data$residuos_pearson)) +
      ggplot2::geom_hline(yintercept = 0, color = "#0F3B5F", linewidth = 0.6) +
      ggplot2::geom_point(color = "#2E7D8F", alpha = 0.78, size = 2.3) +
      ggplot2::geom_smooth(method = "loess", formula = y ~ x, se = FALSE, color = "#E76F51", linewidth = 0.9) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::labs(
        title = paste("Diagnóstico de resíduos:", nome_familia),
        subtitle = "Resíduos de Pearson versus contagens ajustadas",
        x = "Contagem ajustada",
        y = "Resíduo de Pearson"
      )
  }
  # Preserve a saída original para quem quiser conferir o ajuste no console.
  console <- utils::capture.output(summary(modelo))
  # Entregue componentes separados para a interface, o relatório e o Projeto R.
  list(
    narrativa = narrativa,
    tabela = tabela,
    grafico = grafico,
    pressupostos = pressupostos,
    diagnosticos = diagnosticos,
    console = console,
    objeto = modelo,
    dispersao = dispersao,
    orientacao_dispersao = orientacao,
    resumo = list(n = nrow(d), aic = stats::AIC(modelo), dispersao = dispersao,
                  theta = if (identical(familia, "binomial_negativa")) modelo$theta else NA_real_)
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
    # Normalidade dentro de cada grupo (Shapiro-Wilk): um p-valor por grupo.
    sh <- lapply(grupos, function(x) {
      x <- x[!is.na(x)]
      if (length(x) >= 3L && length(x) <= 5000L) stats::shapiro.test(x)$p.value else NA_real_
    })
    # Homocedasticidade: teste de Levene (igualdade de variancias entre os grupos),
    # o mesmo usado na ANOVA e no projeto exportado. O grupo entra como fator para
    # o Levene. Quando a igualdade de variancias for duvidosa, o t de Welch
    # (var.equal = FALSE) e a escolha segura.
    p_variancia <- tryCatch({
      dados_levene <- data.frame(
        resposta = dados[[p$resposta]],
        grupo = factor(dados[[p$grupo]])
      )
      car::leveneTest(resposta ~ grupo, data = dados_levene)[["Pr(>F)"]][1]
    }, error = function(e) NA_real_)
    # Reune normalidade (por grupo) e homocedasticidade numa so tabela de pressupostos.
    pressupostos <- data.frame(
      Pressuposto = c(paste0("Normalidade (Shapiro-Wilk) - grupo ", names(sh)),
                      "Homocedasticidade (Levene)"),
      `p-valor` = c(unlist(sh), p_variancia),
      check.names = FALSE
    )
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

#' ANOVA de medidas repetidas
#'
#' Compara a média da mesma unidade em duas ou mais ocasiões ou condições. Os
#' dados devem estar em formato longo, com uma linha por unidade e ocasião.
#' @param dados Um data.frame.
#' @param p Lista com `resposta`, `sujeito`, `momento` e `nivel_confianca`.
#' @return Lista com narrativa, tabela, gráfico, pressupostos e modelo.
#' @export
catalyser_anova_medidas_repetidas <- function(dados, p) {
  resposta <- as.character(catalyser_ou(p$resposta, ""))
  sujeito <- as.character(catalyser_ou(p$sujeito, ""))
  momento <- as.character(catalyser_ou(p$momento, ""))
  catalyser_colunas(dados, c(resposta, sujeito, momento))
  if (length(unique(c(resposta, sujeito, momento))) != 3L) {
    stop("Resposta, unidade e ocasião precisam ser três variáveis diferentes.", call. = FALSE)
  }
  if (!is.numeric(dados[[resposta]])) {
    stop("A resposta da ANOVA de medidas repetidas precisa ser numérica.", call. = FALSE)
  }
  preparo <- catalyser_completos(dados, c(resposta, sujeito, momento))
  d <- preparo$dados[c(resposta, sujeito, momento)]
  names(d) <- c("valor", "sujeito", "momento")
  d$sujeito <- droplevels(as.factor(d$sujeito))
  d$momento <- droplevels(as.factor(d$momento))
  if (nlevels(d$momento) < 2L) stop("Escolha uma ocasião com pelo menos dois níveis.", call. = FALSE)
  if (any(duplicated(d[c("sujeito", "momento")]))) {
    stop("Cada unidade deve ter uma única observação em cada ocasião. Agregue réplicas internas antes da ANOVA.", call. = FALSE)
  }
  presencas <- stats::xtabs(~ sujeito + momento, data = d) > 0
  sujeitos_completos <- rownames(presencas)[rowSums(presencas) == ncol(presencas)]
  linhas_antes_completude <- nrow(d)
  d <- droplevels(d[d$sujeito %in% sujeitos_completos, , drop = FALSE])
  if (nlevels(d$sujeito) < 2L) stop("São necessárias pelo menos duas unidades medidas em todas as ocasiões.", call. = FALSE)

  modelo <- stats::aov(valor ~ sujeito + momento, data = d)
  resumo <- summary(modelo)[[1]]
  linha_momento <- match("momento", trimws(rownames(resumo)))
  linha_residuo <- nrow(resumo)
  f_valor <- resumo[["F value"]][linha_momento]
  p_valor <- resumo[["Pr(>F)"]][linha_momento]
  gl_momento <- resumo[["Df"]][linha_momento]
  gl_residuo <- resumo[["Df"]][linha_residuo]
  ss_momento <- resumo[["Sum Sq"]][linha_momento]
  ss_residuo <- resumo[["Sum Sq"]][linha_residuo]
  eta_parcial <- ss_momento / (ss_momento + ss_residuo)
  tabela <- data.frame(
    `Fonte de variação` = c("Ocasião ou condição", "Erro dentro das unidades"),
    `Graus de liberdade` = c(gl_momento, gl_residuo),
    `Soma de quadrados` = c(ss_momento, ss_residuo),
    `Quadrado médio` = c(resumo[["Mean Sq"]][linha_momento], resumo[["Mean Sq"]][linha_residuo]),
    F = c(f_valor, NA_real_), `p-valor` = c(p_valor, NA_real_),
    check.names = FALSE
  )
  residuos <- stats::residuals(modelo)
  shapiro <- if (length(residuos) >= 3L && length(residuos) <= 5000L && stats::sd(residuos) > 0) {
    stats::shapiro.test(residuos)
  } else NULL
  pressupostos <- data.frame(
    Verificação = c("Unidades completas", "Normalidade dos resíduos", "Esfericidade"),
    Resultado = c(
      sprintf("%d unidades em %d ocasiões", nlevels(d$sujeito), nlevels(d$momento)),
      if (is.null(shapiro)) "Não estimável" else sprintf("W = %s; %s", catalyser_num(shapiro$statistic), catalyser_p(shapiro$p.value)),
      if (nlevels(d$momento) == 2L) "Automática com duas ocasiões" else "Precisa ser avaliada antes da conclusão; correções de Greenhouse-Geisser ficam para a versão avançada"
    ), check.names = FALSE
  )
  grafico <- NULL
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    medias <- stats::aggregate(valor ~ momento, d, mean)
    grafico <- ggplot2::ggplot(d, ggplot2::aes(x = .data$momento, y = .data$valor, group = .data$sujeito)) +
      ggplot2::geom_line(color = "#62B6B7", alpha = 0.35) +
      ggplot2::geom_point(color = "#2E7D8F", alpha = 0.55) +
      ggplot2::geom_line(data = medias, ggplot2::aes(x = .data$momento, y = .data$valor, group = 1),
                         color = "#E76F51", linewidth = 1.2, inherit.aes = FALSE) +
      ggplot2::geom_point(data = medias, ggplot2::aes(x = .data$momento, y = .data$valor),
                          color = "#E76F51", size = 3, inherit.aes = FALSE) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::labs(title = "Trajetórias das unidades e média em cada ocasião", x = momento, y = resposta)
  }
  narrativa <- sprintf(
    "A ANOVA de medidas repetidas comparou %d ocasiões em %d unidades completas: F(%d, %d) = %s, %s. Eta quadrado parcial = %s.",
    nlevels(d$momento), nlevels(d$sujeito), gl_momento, gl_residuo,
    catalyser_num(f_valor), catalyser_p(p_valor), catalyser_num(eta_parcial)
  )
  list(narrativa = narrativa, tabela = tabela, grafico = grafico,
       pressupostos = pressupostos,
       diagnosticos = data.frame(Indicador = c("Linhas excluídas", "Unidades completas", "Eta quadrado parcial"),
                                 Valor = c(preparo$descartadas + linhas_antes_completude - nrow(d),
                                           nlevels(d$sujeito), eta_parcial)),
       console = utils::capture.output(summary(modelo)), objeto = modelo)
}

#' Teste de Friedman para k condições pareadas
#'
#' Compara três ou mais condições observadas nos mesmos indivíduos ou blocos.
#' É o correspondente não paramétrico da ANOVA de medidas repetidas. A base deve
#' estar no formato longo e completa: cada bloco precisa aparecer uma vez em cada
#' condição.
#'
#' @param dados Um data.frame no formato longo.
#' @param p Lista com `resposta`, `condicao`, `bloco`, `posteste` e
#'   `nivel_confianca`.
#' @return Lista com narrativa, tabela, comparações, gráfico e diagnóstico.
#' @export
catalyser_friedman <- function(dados, p) {
  resposta <- as.character(catalyser_ou(p$resposta, ""))
  condicao <- as.character(catalyser_ou(p$condicao, ""))
  bloco <- as.character(catalyser_ou(p$bloco, ""))
  catalyser_colunas(dados, c(resposta, condicao, bloco))
  if (length(unique(c(resposta, condicao, bloco))) != 3L) {
    stop("Resposta, condição e bloco precisam ser três variáveis diferentes.", call. = FALSE)
  }
  if (!is.numeric(dados[[resposta]])) {
    stop("A resposta do teste de Friedman precisa ser numérica.", call. = FALSE)
  }

  d <- dados[c(resposta, condicao, bloco)]
  names(d) <- c("valor", "condicao", "bloco")
  incompletas <- !stats::complete.cases(d) | !is.finite(d$valor)
  if (any(incompletas)) {
    stop(sprintf(
      "Há %d linha(s) incompleta(s). O Friedman exige cada bloco completo em todas as condições; complete ou retire o bloco antes de analisar.",
      sum(incompletas)
    ), call. = FALSE)
  }
  d$condicao <- droplevels(as.factor(d$condicao))
  d$bloco <- droplevels(as.factor(d$bloco))
  if (nlevels(d$condicao) < 3L) {
    stop("O Friedman compara pelo menos três condições pareadas.", call. = FALSE)
  }
  if (nlevels(d$bloco) < 2L) {
    stop("São necessários ao menos dois blocos ou indivíduos repetidos.", call. = FALSE)
  }
  if (any(duplicated(d[c("bloco", "condicao")]))) {
    stop("Cada bloco deve ter apenas uma observação por condição. Agregue subamostras antes do Friedman.", call. = FALSE)
  }
  presencas <- stats::xtabs(~ bloco + condicao, data = d) > 0
  blocos_incompletos <- rownames(presencas)[rowSums(presencas) != ncol(presencas)]
  if (length(blocos_incompletos)) {
    exemplos <- paste(utils::head(blocos_incompletos, 5L), collapse = ", ")
    complemento <- if (length(blocos_incompletos) > 5L) ", ..." else ""
    stop(sprintf(
      "Os blocos precisam ser balanceados: %d bloco(s) não têm todas as condições (%s%s).",
      length(blocos_incompletos), exemplos, complemento
    ), call. = FALSE)
  }

  d <- d[order(d$bloco, d$condicao), , drop = FALSE]
  teste <- stats::friedman.test(y = d$valor, groups = d$condicao, blocks = d$bloco)
  alfa <- 1 - as.numeric(catalyser_ou(p$nivel_confianca, 0.95))
  if (!is.finite(alfa) || alfa <= 0 || alfa >= 1) alfa <- 0.05
  posteste <- NULL
  pares <- NULL
  metodo_posteste <- "Não aplicado: o teste global não indicou diferença."
  if (is.finite(teste$p.value) && teste$p.value < alfa) {
    metodo_escolhido <- as.character(catalyser_ou(p$posteste, "holm"))
    usar_nemenyi <- identical(metodo_escolhido, "nemenyi") &&
      requireNamespace("PMCMRplus", quietly = TRUE)
    if (usar_nemenyi) {
      pares <- PMCMRplus::frdAllPairsNemenyiTest(
        y = d$valor, groups = d$condicao, blocks = d$bloco
      )
      matriz_p <- pares$p.value
      metodo_posteste <- "Nemenyi para todas as comparações pareadas."
    } else {
      pares <- stats::pairwise.wilcox.test(
        d$valor, d$condicao, paired = TRUE, p.adjust.method = "holm"
      )
      matriz_p <- pares$p.value
      metodo_posteste <- if (identical(metodo_escolhido, "nemenyi")) {
        "Holm com Wilcoxon pareado: PMCMRplus não está instalado para executar Nemenyi."
      } else {
        "Wilcoxon pareado, com correção de Holm."
      }
    }
    posicoes <- which(!is.na(matriz_p), arr.ind = TRUE)
    if (nrow(posicoes)) {
      posteste <- data.frame(
        `Condição 1` = rownames(matriz_p)[posicoes[, 1]],
        `Condição 2` = colnames(matriz_p)[posicoes[, 2]],
        `p ajustado` = matriz_p[posicoes],
        `Leitura` = ifelse(matriz_p[posicoes] < alfa, "Há evidência de diferença", "Sem evidência de diferença"),
        check.names = FALSE, row.names = NULL
      )
    }
  }
  tabela <- data.frame(
    `Qui-quadrado de Friedman` = unname(teste$statistic),
    `Graus de liberdade` = unname(teste$parameter),
    `p-valor` = teste$p.value,
    `Condições` = nlevels(d$condicao),
    `Blocos completos` = nlevels(d$bloco),
    check.names = FALSE
  )
  pressupostos <- data.frame(
    Verificação = c("Formato da base", "Blocos balanceados", "Independência entre blocos", "Escala da resposta"),
    Resultado = c(
      "Formato longo: uma linha por bloco e condição",
      sprintf("%d blocos, todos com %d condições", nlevels(d$bloco), nlevels(d$condicao)),
      "Depende do delineamento e da coleta",
      "Ao menos ordinal; o teste trabalha com postos e não exige normalidade"
    ), check.names = FALSE
  )
  grafico <- NULL
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    medias <- stats::aggregate(valor ~ condicao, d, mean)
    grafico <- ggplot2::ggplot(d, ggplot2::aes(x = .data$condicao, y = .data$valor, group = .data$bloco)) +
      ggplot2::geom_line(color = "#62B6B7", alpha = 0.38) +
      ggplot2::geom_point(color = "#2E7D8F", alpha = 0.62) +
      ggplot2::geom_line(data = medias, ggplot2::aes(x = .data$condicao, y = .data$valor, group = 1),
                          inherit.aes = FALSE, color = "#E76F51", linewidth = 1.2) +
      ggplot2::geom_point(data = medias, ggplot2::aes(x = .data$condicao, y = .data$valor),
                           inherit.aes = FALSE, color = "#E76F51", size = 3) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::labs(
        title = "Condições pareadas: trajetórias dos blocos e médias",
        subtitle = "Linhas claras: blocos individuais. Linha coral: média por condição.",
        x = condicao, y = resposta
      )
  }
  houve_diferenca <- is.finite(teste$p.value) && teste$p.value < alfa
  leitura <- if (houve_diferenca) {
    "Há evidência de que ao menos uma condição difere; consulte o pós-teste para identificar os pares."
  } else {
    "Não há evidência de diferença entre as condições."
  }
  narrativa <- sprintf(
    "O teste de Friedman, correspondente não paramétrico da ANOVA de medidas repetidas, comparou %d condições em %d blocos completos: qui-quadrado(%d) = %s e %s. %s",
    nlevels(d$condicao), nlevels(d$bloco), as.integer(teste$parameter),
    catalyser_num(teste$statistic), catalyser_p(teste$p.value), leitura
  )
  list(
    narrativa = narrativa, tabela = tabela, comparacoes = posteste, grafico = grafico,
    pressupostos = pressupostos,
    diagnosticos = data.frame(
      Indicador = c("Linhas analisadas", "Blocos completos", "Condições", "Pós-teste"),
      Valor = c(nrow(d), nlevels(d$bloco), nlevels(d$condicao), metodo_posteste),
      check.names = FALSE
    ),
    console = c(utils::capture.output(print(teste)), "", "Pós-teste:",
                if (is.null(posteste)) metodo_posteste else utils::capture.output(print(posteste))),
    objeto = list(friedman = teste, comparacoes = pares)
  )
}

#' Teste de McNemar para duas respostas binárias pareadas
#'
#' Compara proporções em duas medições feitas nas mesmas unidades, como antes e
#' depois ou dois métodos aplicados ao mesmo indivíduo.
#'
#' @param dados Um data.frame, com uma linha por par.
#' @param p Lista com `variavel_1`, `variavel_2` e `correcao`.
#' @return Lista com narrativa, tabela, gráfico e resultado do teste.
#' @export
catalyser_mcnemar <- function(dados, p) {
  variavel_1 <- as.character(catalyser_ou(p$variavel_1, ""))
  variavel_2 <- as.character(catalyser_ou(p$variavel_2, ""))
  catalyser_colunas(dados, c(variavel_1, variavel_2))
  if (identical(variavel_1, variavel_2)) {
    stop("Escolha duas variáveis binárias diferentes.", call. = FALSE)
  }
  d <- dados[stats::complete.cases(dados[c(variavel_1, variavel_2)]), c(variavel_1, variavel_2), drop = FALSE]
  if (!nrow(d)) stop("Não há pares completos para o teste de McNemar.", call. = FALSE)
  antes <- as.character(d[[variavel_1]])
  depois <- as.character(d[[variavel_2]])
  niveis <- sort(unique(c(antes, depois)))
  if (length(niveis) != 2L) {
    stop("As duas respostas precisam compartilhar exatamente duas categorias.", call. = FALSE)
  }
  tabela_pares <- table(factor(antes, levels = niveis), factor(depois, levels = niveis))
  if (any(dim(tabela_pares) != 2L)) stop("O McNemar exige uma tabela 2 por 2.", call. = FALSE)
  teste <- stats::mcnemar.test(tabela_pares, correct = isTRUE(p$correcao))
  discordantes <- tabela_pares[1, 2] + tabela_pares[2, 1]
  tabela <- as.data.frame.matrix(tabela_pares, stringsAsFactors = FALSE, check.names = FALSE)
  tabela <- data.frame(`Primeira medição` = rownames(tabela), tabela,
                       row.names = NULL, check.names = FALSE)
  names(tabela)[-1L] <- paste0("Segunda: ", names(tabela)[-1L])
  grafico <- NULL
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    proporcoes <- data.frame(
      Medição = c(variavel_1, variavel_2),
      Categoria = niveis[2],
      `Proporção` = c(mean(antes == niveis[2]), mean(depois == niveis[2])),
      check.names = FALSE
    )
    grafico <- ggplot2::ggplot(proporcoes, ggplot2::aes(x = .data$Medição, y = .data$Proporção, fill = .data$Medição)) +
      ggplot2::geom_col(show.legend = FALSE) +
      ggplot2::scale_fill_manual(values = c("#0F3B5F", "#2E7D8F")) +
      ggplot2::scale_y_continuous(labels = function(x) paste0(round(100 * x), "%"), limits = c(0, 1)) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::labs(title = "Proporção da segunda categoria em duas medições pareadas", x = NULL, y = "Proporção")
  }
  leitura <- if (teste$p.value < 0.05) {
    "Há evidência de mudança nas proporções marginais entre as duas medições."
  } else {
    "Não há evidência de mudança nas proporções marginais entre as duas medições."
  }
  list(
    narrativa = sprintf("O teste de McNemar comparou %d pares completos: qui-quadrado(1) = %s e %s. %s",
                        nrow(d), catalyser_num(teste$statistic), catalyser_p(teste$p.value), leitura),
    tabela = tabela, grafico = grafico,
    pressupostos = data.frame(
      Verificação = c("Pares completos", "Resposta binária", "Independência entre pares"),
      Resultado = c(nrow(d), paste(niveis, collapse = " e "), "Depende do planejamento da coleta"),
      check.names = FALSE
    ),
    diagnosticos = data.frame(Indicador = c("Pares discordantes", "Correção de continuidade"),
                              Valor = c(discordantes, if (isTRUE(p$correcao)) "Usada" else "Não usada")),
    console = utils::capture.output(print(teste)), objeto = teste
  )
}

#' Teste qui-quadrado para uma variância
#' @param dados Um data.frame.
#' @param p Lista com `variavel`, `desvio_hipotetico`, `alternativa` e `nivel_confianca`.
#' @return Lista com narrativa, tabela, gráfico, pressupostos e estatísticas do teste.
#' @export
catalyser_variancia_uma <- function(dados, p) {
  variavel <- as.character(catalyser_ou(p$variavel, ""))
  catalyser_colunas(dados, variavel)
  if (!is.numeric(dados[[variavel]])) stop("Escolha uma variável numérica.", call. = FALSE)
  x <- dados[[variavel]]
  x <- x[is.finite(x)]
  if (length(x) < 2L || stats::sd(x) == 0) stop("São necessários pelo menos dois valores e variação observada.", call. = FALSE)
  desvio0 <- as.numeric(catalyser_ou(p$desvio_hipotetico, NA_real_))
  if (!is.finite(desvio0) || desvio0 <= 0) stop("O desvio padrão de referência precisa ser maior que zero.", call. = FALSE)
  alternativa <- as.character(catalyser_ou(p$alternativa, "two.sided"))
  if (!alternativa %in% c("two.sided", "less", "greater")) stop("Escolha uma hipótese alternativa válida.", call. = FALSE)
  conf <- as.numeric(catalyser_ou(p$nivel_confianca, 0.95))
  n <- length(x); gl <- n - 1L; variancia <- stats::var(x)
  estatistica <- gl * variancia / desvio0^2
  p_inferior <- stats::pchisq(estatistica, gl)
  p_superior <- stats::pchisq(estatistica, gl, lower.tail = FALSE)
  p_valor <- switch(alternativa, less = p_inferior, greater = p_superior,
                    two.sided = min(1, 2 * min(p_inferior, p_superior)))
  alfa <- 1 - conf
  ic_var <- c(gl * variancia / stats::qchisq(1 - alfa / 2, gl),
              gl * variancia / stats::qchisq(alfa / 2, gl))
  tabela <- data.frame(
    n = n, `Desvio padrão amostral` = sqrt(variancia), `Desvio padrão de referência` = desvio0,
    `Qui-quadrado` = estatistica, `Graus de liberdade` = gl, `p-valor` = p_valor,
    `IC da variância inferior` = ic_var[1], `IC da variância superior` = ic_var[2],
    check.names = FALSE
  )
  shapiro <- if (n >= 3L && n <= 5000L) stats::shapiro.test(x) else NULL
  pressupostos <- data.frame(
    Pressuposto = c("Observações independentes", "Normalidade da variável"),
    Leitura = c("Depende do planejamento e da unidade amostral",
                if (is.null(shapiro)) "Shapiro-Wilk não calculado" else sprintf("W = %s; %s", catalyser_num(shapiro$statistic), catalyser_p(shapiro$p.value))),
    check.names = FALSE
  )
  grafico <- NULL
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    limite <- max(stats::qchisq(0.995, gl), estatistica * 1.15)
    curva <- data.frame(x = seq(0, limite, length.out = 500L))
    curva$densidade <- stats::dchisq(curva$x, gl)
    grafico <- ggplot2::ggplot(curva, ggplot2::aes(x = .data$x, y = .data$densidade)) +
      ggplot2::geom_area(fill = "#62B6B7", alpha = 0.55) +
      ggplot2::geom_line(color = "#0F3B5F", linewidth = 0.9) +
      ggplot2::geom_vline(xintercept = estatistica, color = "#E76F51", linewidth = 1) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::labs(title = "Distribuição qui-quadrado sob a hipótese nula", x = "Qui-quadrado", y = "Densidade")
  }
  narrativa <- sprintf(
    "O teste qui-quadrado para a variância de '%s' comparou o desvio padrão amostral %s com %s: χ²(%d) = %s e %s.",
    variavel, catalyser_num(sqrt(variancia)), catalyser_num(desvio0), gl,
    catalyser_num(estatistica), catalyser_p(p_valor)
  )
  objeto <- list(statistic = c(`X-squared` = estatistica), parameter = c(df = gl),
                 p.value = p_valor, estimate = c(variance = variancia), null.value = c(variance = desvio0^2),
                 alternative = alternativa, method = "Qui-quadrado para uma variância")
  class(objeto) <- "htest"
  list(narrativa = narrativa, tabela = tabela, grafico = grafico,
       pressupostos = pressupostos,
       diagnosticos = data.frame(Indicador = c("Valores válidos", "Ausentes ou não finitos"),
                                 Valor = c(n, length(dados[[variavel]]) - n)),
       console = utils::capture.output(print(objeto)), objeto = objeto)
}

#' Teste F para a razão de duas variâncias
#' @param dados Um data.frame.
#' @param p Lista com `resposta`, `grupo`, `alternativa` e `nivel_confianca`.
#' @return Lista com narrativa, tabela, gráfico, pressupostos e objeto `htest`.
#' @export
catalyser_variancias_duas <- function(dados, p) {
  resposta <- as.character(catalyser_ou(p$resposta, ""))
  grupo <- as.character(catalyser_ou(p$grupo, ""))
  catalyser_colunas(dados, c(resposta, grupo))
  if (!is.numeric(dados[[resposta]])) stop("A resposta precisa ser numérica.", call. = FALSE)
  preparo <- catalyser_completos(dados, c(resposta, grupo))
  d <- preparo$dados[c(resposta, grupo)]
  names(d) <- c("resposta", "grupo")
  d$grupo <- droplevels(as.factor(d$grupo))
  if (nlevels(d$grupo) != 2L) stop("O teste F compara exatamente dois grupos.", call. = FALSE)
  if (any(table(d$grupo) < 2L)) stop("Cada grupo precisa de pelo menos duas observações.", call. = FALSE)
  alternativa <- as.character(catalyser_ou(p$alternativa, "two.sided"))
  conf <- as.numeric(catalyser_ou(p$nivel_confianca, 0.95))
  teste <- stats::var.test(resposta ~ grupo, data = d, ratio = 1,
                           alternative = alternativa, conf.level = conf)
  partes <- split(d$resposta, d$grupo)
  variancias <- vapply(partes, stats::var, numeric(1))
  tabela <- data.frame(
    `Primeiro grupo` = names(partes)[1], `Segundo grupo` = names(partes)[2],
    `Variância do primeiro` = variancias[1], `Variância do segundo` = variancias[2],
    `Razão de variâncias` = unname(teste$estimate), F = unname(teste$statistic),
    `GL numerador` = unname(teste$parameter[1]), `GL denominador` = unname(teste$parameter[2]),
    `p-valor` = teste$p.value, `IC inferior` = teste$conf.int[1], `IC superior` = teste$conf.int[2],
    check.names = FALSE
  )
  p_shapiro <- vapply(partes, function(x) {
    if (length(x) >= 3L && length(x) <= 5000L && stats::sd(x) > 0) stats::shapiro.test(x)$p.value else NA_real_
  }, numeric(1))
  pressupostos <- data.frame(
    Grupo = names(partes), n = lengths(partes), `p (Shapiro-Wilk)` = p_shapiro,
    Leitura = ifelse(is.na(p_shapiro), "Normalidade não estimável", ifelse(p_shapiro < 0.05,
      "Há sinal de desvio da normalidade; o teste F é sensível", "Sem sinal forte de desvio pelo Shapiro-Wilk")),
    check.names = FALSE
  )
  grafico <- NULL
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    grafico <- ggplot2::ggplot(d, ggplot2::aes(x = .data$grupo, y = .data$resposta, fill = .data$grupo)) +
      ggplot2::geom_boxplot(alpha = 0.78, show.legend = FALSE, outlier.color = "#E76F51") +
      ggplot2::scale_fill_manual(values = c("#2E7D8F", "#E89B3C")) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::labs(title = "Dispersão observada nos dois grupos", x = grupo, y = resposta)
  }
  narrativa <- sprintf(
    "O teste F comparou as variâncias de '%s' entre %s e %s: F(%d, %d) = %s e %s. Razão estimada = %s.",
    resposta, names(partes)[1], names(partes)[2], unname(teste$parameter[1]), unname(teste$parameter[2]),
    catalyser_num(teste$statistic), catalyser_p(teste$p.value), catalyser_num(teste$estimate)
  )
  list(narrativa = narrativa, tabela = tabela, grafico = grafico,
       pressupostos = pressupostos,
       diagnosticos = data.frame(Indicador = "Linhas excluídas", Valor = preparo$descartadas),
       console = utils::capture.output(print(teste)), objeto = teste)
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

# Helpers gráficos da ANOVA fatorial. Mantêm a identidade Ocean Gradient sem
# acrescentar dependências: a composição em dois painéis é feita pela interface,
# enquanto o replay preserva cada gráfico como um objeto ggplot independente.
anova2_paleta_ocean <- function(n, nomes = NULL) {
  n <- max(1L, as.integer(n))
  base <- c("#2E7D8F", "#E76F51", "#0F3B5F", "#E89B3C", "#62B6B7")
  cores <- if (n <= length(base)) base[seq_len(n)] else
    grDevices::colorRampPalette(base)(n)
  if (!is.null(nomes) && length(nomes) == n) names(cores) <- nomes
  cores
}

anova2_tema_ocean <- function(tema = "minimal", base_size = 12) {
  tema_base <- switch(
    as.character(catalyser_ou(tema, "minimal")),
    classic = ggplot2::theme_classic(base_size = base_size),
    bw = ggplot2::theme_bw(base_size = base_size),
    gray = ggplot2::theme_gray(base_size = base_size),
    light = ggplot2::theme_light(base_size = base_size),
    ggplot2::theme_minimal(base_size = base_size)
  )
  tema_base + ggplot2::theme(
    plot.title = ggplot2::element_text(
      colour = "#0F3B5F", face = "bold", size = base_size
    ),
    plot.subtitle = ggplot2::element_text(colour = "#2E7D8F"),
    axis.title = ggplot2::element_text(colour = "#0F3B5F"),
    axis.text = ggplot2::element_text(colour = "#0F3B5F"),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(colour = "#E6EDF2"),
    legend.position = "bottom",
    legend.title = ggplot2::element_text(colour = "#0F3B5F", face = "bold")
  )
}

anova2_rotulo_media <- function(x, dig = 2L) {
  ifelse(
    is.na(x), "-",
    formatC(as.numeric(x), format = "f", digits = dig, decimal.mark = ",")
  )
}

anova2_grafico_interacao_celulas <- function(
    celulas, fator_a, fator_b, resposta, niveis_a, niveis_b,
    nivel_confianca = 0.95, titulo = NULL, rotulo_x = NULL, rotulo_y = NULL,
    tema = "minimal") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  d <- celulas
  d$fator_a <- factor(d$fator_a, levels = niveis_a)
  d$fator_b <- factor(d$fator_b, levels = niveis_b)
  cores <- anova2_paleta_ocean(length(niveis_b), niveis_b)
  formas <- stats::setNames(rep(c(16, 15, 17, 18, 8, 3), length.out = length(niveis_b)),
                            niveis_b)

  ggplot2::ggplot(
    d,
    ggplot2::aes(
      x = fator_a, y = media, colour = fator_b, shape = fator_b,
      group = fator_b
    )
  ) +
    ggplot2::geom_line(linewidth = 1.05) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = ic_inferior, ymax = ic_superior),
      width = 0.12, linewidth = 0.75
    ) +
    ggplot2::scale_colour_manual(values = cores) +
    ggplot2::scale_shape_manual(values = formas) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.06, 0.12))) +
    anova2_tema_ocean(tema) +
    ggplot2::labs(
      title = titulo,
      subtitle = sprintf(
        "Médias observadas por célula; hastes = IC %.0f%%",
        100 * nivel_confianca
      ),
      x = rotulo_x,
      y = rotulo_y,
      colour = fator_b,
      shape = fator_b
    )
}

anova2_grafico_combinacoes_celulas <- function(
    celulas, fator_a, fator_b, resposta, niveis_a, niveis_b,
    nivel_confianca = 0.95, titulo = NULL, rotulo_x = NULL, rotulo_y = NULL,
    tema = "minimal") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  d <- celulas
  d$fator_a <- factor(d$fator_a, levels = niveis_a)
  d$fator_b <- factor(d$fator_b, levels = niveis_b)
  d$rotulo_media <- anova2_rotulo_media(d$media)
  cores <- anova2_paleta_ocean(length(niveis_b), niveis_b)
  posicao <- ggplot2::position_dodge(width = 0.78)

  ggplot2::ggplot(
    d,
    ggplot2::aes(x = fator_a, y = media, fill = fator_b, group = fator_b)
  ) +
    ggplot2::geom_col(
      position = posicao, width = 0.68, colour = "white", linewidth = 0.3
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = ic_inferior, ymax = ic_superior),
      position = posicao, width = 0.13, linewidth = 0.7, colour = "#0F3B5F"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = rotulo_media),
      position = posicao, vjust = -0.55, size = 3, colour = "#0F3B5F"
    ) +
    ggplot2::scale_fill_manual(values = cores) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.04, 0.18))) +
    ggplot2::expand_limits(y = 0) +
    anova2_tema_ocean(tema) +
    ggplot2::labs(
      title = titulo,
      subtitle = sprintf(
        "Valores sobre as barras; hastes = IC %.0f%%",
        100 * nivel_confianca
      ),
      x = rotulo_x,
      y = rotulo_y,
      fill = fator_b
    )
}

#' ANOVA de dois fatores com interação
#'
#' Ajusta um modelo fatorial (`resposta ~ fator_a * fator_b`) e devolve os
#' componentes que a CatalyseR usa na Comunicação de Resultados. A função
#' trabalha com observações individuais, remove e conta casos incompletos e
#' mantém as médias por célula separadas da tabela do modelo.
#'
#' @param dados Um data.frame com uma linha por observação.
#' @param p Lista com `resposta`, `fator_a` e `fator_b`; aceita ainda
#'   `nivel_confianca`, `titulo_grafico`, `rotulo_x`, `rotulo_y` e `tema`.
#' @return Lista com narrativa, médias por célula, tabela da ANOVA, tamanhos de
#'   efeito, Tukey da interação, gráfico de perfis, gráfico das combinações,
#'   pressupostos, diagnósticos, console e o objeto `aov` em `objeto`.
#' @export
catalyser_anova_dois_fatores <- function(dados, p) {
  resposta <- as.character(catalyser_ou(p$resposta, ""))[[1]]
  fator_a <- as.character(catalyser_ou(p$fator_a, ""))[[1]]
  fator_b <- as.character(catalyser_ou(p$fator_b, ""))[[1]]
  if (!nzchar(resposta) || !nzchar(fator_a) || !nzchar(fator_b))
    stop("A ANOVA de dois fatores precisa de resposta, fator_a e fator_b.", call. = FALSE)
  catalyser_colunas(dados, c(resposta, fator_a, fator_b))
  if (!is.numeric(dados[[resposta]]))
    stop(sprintf("A resposta '%s' precisa ser numérica para a ANOVA.", resposta), call. = FALSE)
  if (length(unique(c(resposta, fator_a, fator_b))) < 3L)
    stop("A resposta e os dois fatores precisam ser variáveis diferentes.", call. = FALSE)

  conf <- suppressWarnings(as.numeric(catalyser_ou(p$nivel_confianca, 0.95))[[1]])
  if (!is.finite(conf) || conf <= 0 || conf >= 1) conf <- 0.95
  preparo <- catalyser_completos(dados, c(resposta, fator_a, fator_b))
  d <- data.frame(
    resposta = as.numeric(preparo$dados[[resposta]]),
    fator_a = droplevels(as.factor(preparo$dados[[fator_a]])),
    fator_b = droplevels(as.factor(preparo$dados[[fator_b]])),
    stringsAsFactors = FALSE
  )
  if (nrow(d) < 6L)
    stop("A ANOVA de dois fatores precisa de pelo menos seis observações completas.", call. = FALSE)
  if (nlevels(d$fator_a) < 2L || nlevels(d$fator_b) < 2L)
    stop("Cada fator precisa ter pelo menos dois níveis com dados.", call. = FALSE)
  contagens <- table(d$fator_a, d$fator_b)
  if (any(contagens < 2L)) {
    ruins <- which(contagens < 2L, arr.ind = TRUE)
    nomes <- apply(ruins, 1L, function(i)
      paste(rownames(contagens)[i[[1]]], colnames(contagens)[i[[2]]], sep = " \u00d7 "))
    stop(sprintf("Cada célula do fatorial precisa de pelo menos duas observações: %s.",
                 paste(nomes, collapse = "; ")), call. = FALSE)
  }

  modelo <- stats::aov(resposta ~ fator_a * fator_b, data = d)
  an <- summary(modelo)[[1]]
  nomes_an <- trimws(rownames(an))
  linhas <- match(c("fator_a", "fator_b", "fator_a:fator_b"), nomes_an)
  residuo <- match("Residuals", nomes_an)
  pegar <- function(coluna, indices) {
    if (!coluna %in% names(an)) return(rep(NA_real_, length(indices)))
    as.numeric(an[[coluna]][indices])
  }
  df_efeitos <- pegar("Df", linhas)
  ss_efeitos <- pegar("Sum Sq", linhas)
  qm_efeitos <- pegar("Mean Sq", linhas)
  f_efeitos <- pegar("F value", linhas)
  p_efeitos <- pegar("Pr(>F)", linhas)
  df_res <- as.numeric(an$Df[residuo])
  ss_res <- as.numeric(an$`Sum Sq`[residuo])
  qm_res <- as.numeric(an$`Mean Sq`[residuo])
  ss_total <- sum((d$resposta - mean(d$resposta))^2)

  nomes_tabela <- c("Fonte de varia\u00e7\u00e3o", "Graus de liberdade",
                    "Soma de quadrados", "Quadrado m\u00e9dio", "F", "p-valor")
  tabela <- data.frame(
    fonte = c(sprintf("Fator A (%s)", fator_a), sprintf("Fator B (%s)", fator_b),
              sprintf("Intera\u00e7\u00e3o %s \u00d7 %s", fator_a, fator_b),
              "Res\u00edduos", "Total"),
    gl = c(df_efeitos, df_res, nrow(d) - 1L),
    sq = c(ss_efeitos, ss_res, ss_total),
    qm = c(qm_efeitos, qm_res, NA_real_),
    f = c(f_efeitos, NA_real_, NA_real_),
    p = c(p_efeitos, NA_real_, NA_real_),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  names(tabela) <- nomes_tabela

  niveis_a <- levels(d$fator_a)
  niveis_b <- levels(d$fator_b)
  celulas <- do.call(rbind, lapply(niveis_a, function(a) {
    do.call(rbind, lapply(niveis_b, function(b) {
      valores <- d$resposta[d$fator_a == a & d$fator_b == b]
      n <- length(valores)
      media <- mean(valores)
      margem <- stats::qt(1 - (1 - conf) / 2, df = n - 1L) *
        stats::sd(valores) / sqrt(n)
      data.frame(fator_a = a, fator_b = b, n = n, media = media,
                 desvio = stats::sd(valores), ic_inferior = media - margem,
                 ic_superior = media + margem, stringsAsFactors = FALSE)
    }))
  }))
  rownames(celulas) <- NULL
  tamanhos_celula <- stats::setNames(celulas$n,
                                     paste(celulas$fator_a, celulas$fator_b, sep = " \u00d7 "))
  balanceado <- length(unique(celulas$n)) == 1L

  efeito <- data.frame(
    efeito = c("Fator A", "Fator B", "Intera\u00e7\u00e3o A \u00d7 B"),
    eta = ss_efeitos / (ss_efeitos + ss_res),
    omega = (ss_efeitos - df_efeitos * qm_res) / (ss_total + qm_res),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  names(efeito) <- c("Efeito", "Eta\u00b2 parcial", "\u00d4mega\u00b2")

  residuos <- stats::residuals(modelo)
  ajustados <- stats::fitted(modelo)
  shapiro <- if (length(residuos) >= 3L && length(residuos) <= 5000L)
    tryCatch(stats::shapiro.test(residuos), error = function(e) NULL) else NULL
  grupo_levene <- interaction(d$fator_a, d$fator_b, drop = TRUE, sep = " \u00d7 ")
  levene <- if (requireNamespace("car", quietly = TRUE))
    tryCatch(car::leveneTest(d$resposta, grupo_levene, center = stats::median),
             error = function(e) NULL) else NULL
  estat <- function(x, campo) if (is.null(x)) NA_real_ else
    suppressWarnings(unname(as.numeric(x[[campo]][[1]])))
  pressupostos <- data.frame(
    pressuposto = c("Normalidade dos resíduos (Shapiro-Wilk)",
                    "Homogeneidade das variâncias por célula (Levene)"),
    estatistica = c(estat(shapiro, "statistic"),
                    if (is.null(levene)) NA_real_ else
                      suppressWarnings(as.numeric(levene[["F value"]][1]))),
    p = c(estat(shapiro, "p.value"),
          if (is.null(levene)) NA_real_ else
            suppressWarnings(as.numeric(levene[["Pr(>F)"]][1]))),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  names(pressupostos) <- c("Pressuposto", "Estatística", "p-valor")

  comparacoes <- tryCatch({
    tukey <- stats::TukeyHSD(modelo, which = "fator_a:fator_b", conf.level = conf)[[1]]
    bruto <- as.data.frame(tukey)
    saida <- data.frame(par = rownames(bruto), diff = bruto$diff, lwr = bruto$lwr,
                        upr = bruto$upr, p_adj = bruto$`p adj`,
                        check.names = FALSE, stringsAsFactors = FALSE)
    names(saida) <- c("Célula comparada", "Diferença estimada", "IC inferior",
                      "IC superior", "p ajustado")
    rownames(saida) <- NULL
    saida
  }, error = function(e) NULL)

  texto_ou <- function(x, padrao) {
    x <- as.character(catalyser_ou(x, ""))
    if (!length(x) || !nzchar(trimws(x[[1]]))) padrao else x[[1]]
  }
  grafico <- NULL
  grafico_combinacoes <- NULL
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    titulo_interacao <- texto_ou(
      p$titulo_grafico,
      sprintf("Perfis de médias: %s × %s", fator_a, fator_b)
    )
    rotulo_x_grafico <- texto_ou(p$rotulo_x, fator_a)
    rotulo_y_grafico <- texto_ou(p$rotulo_y, resposta)
    tema_grafico <- texto_ou(p$tema, "minimal")
    grafico <- anova2_grafico_interacao_celulas(
      celulas, fator_a, fator_b, resposta, niveis_a, niveis_b,
      nivel_confianca = conf, titulo = titulo_interacao,
      rotulo_x = rotulo_x_grafico, rotulo_y = rotulo_y_grafico,
      tema = tema_grafico
    )
    grafico_combinacoes <- anova2_grafico_combinacoes_celulas(
      celulas, fator_a, fator_b, resposta, niveis_a, niveis_b,
      nivel_confianca = conf,
      titulo = sprintf("Médias das combinações: %s × %s", fator_a, fator_b),
      rotulo_x = rotulo_x_grafico, rotulo_y = rotulo_y_grafico,
      tema = tema_grafico
    )
  }

  p_int <- p_efeitos[[3]]
  leitura <- if (is.na(p_int)) "não disponível" else
    if (p_int < 0.05) "há evidência de interação" else
      "não há evidência suficiente de interação"
  narrativa <- paste0(
    sprintf("A pergunta foi se '%s' varia conforme '%s' e '%s'. ", resposta, fator_a, fator_b),
    sprintf("Entraram %d observações completas%s. ", nrow(d),
            if (preparo$descartadas > 0L)
              sprintf("; %d linha(s) foram excluídas por dados faltantes", preparo$descartadas)
            else " (nenhuma linha foi excluída por dados faltantes)"),
    if (balanceado) "As células têm o mesmo tamanho amostral. " else
      sprintf("O delineamento é desequilibrado (n por célula: %s). ",
              paste(names(tamanhos_celula), tamanhos_celula, sep = " = ", collapse = "; ")),
    sprintf("O modelo fatorial encontrou %s (F = %s; %s). ", leitura,
            catalyser_num(f_efeitos[[3]]), catalyser_p(p_int)),
    sprintf("Os efeitos principais devem ser interpretados junto com a interação: quando ela é relevante, o efeito de '%s' depende de '%s'. ", fator_a, fator_b),
    "As médias por célula, os testes de pressupostos e o gráfico de interação completam a leitura."
  )
  tukey_console <- tryCatch(stats::TukeyHSD(modelo, which = "fator_a:fator_b", conf.level = conf),
                            error = function(e) NULL)
  console <- c(utils::capture.output(print(summary(modelo))), "",
               if (is.null(shapiro)) "Shapiro-Wilk não calculado." else utils::capture.output(print(shapiro)), "",
               if (is.null(levene)) "Levene indisponível (pacote 'car' ausente)." else utils::capture.output(print(levene)), "",
               if (is.null(tukey_console)) "Tukey das células indisponível." else utils::capture.output(print(tukey_console)))
  diagnosticos <- data.frame(
    Indicador = c("n analisado", "Casos excluídos", "Células", "Delineamento"),
    Valor = c(nrow(d), preparo$descartadas, nrow(celulas), if (balanceado) "balanceado" else "desequilibrado"),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  list(narrativa = narrativa, celulas = celulas, tabela = tabela, efeito = efeito,
       comparacoes = comparacoes, grafico = grafico,
       grafico_combinacoes = grafico_combinacoes, pressupostos = pressupostos,
       diagnosticos = diagnosticos, console = console, objeto = modelo,
       dados = d, resposta = resposta, fator_a = fator_a, fator_b = fator_b,
       nivel_confianca = conf, residuos = residuos, ajustados = ajustados,
       tamanhos_celula = tamanhos_celula, delineamento_balanceado = balanceado)
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

#' Uma proporcao com intervalo de confianca e teste binomial
#'
#' @param dados Um data.frame com uma variavel categórica binária.
#' @param p Lista com `variavel`, `sucesso`, `referencia`, `confianca`,
#'   `alternativa` e `desenho`.
#' @return Lista com narrativa, tabela, grafico, diagnosticos, console e objeto.
catalyser_proporcao_uma <- function(dados, p) {
  if (!identical(p$desenho, "independente"))
    stop("Esta análise atende somente unidades independentes; pares, blocos e agrupamentos precisam de outro caminho.", call. = FALSE)
  catalyser_colunas(dados, p$variavel)
  x <- dados[[p$variavel]]
  x <- as.character(x[!is.na(x)])
  niveis <- unique(x)
  if (length(niveis) != 2L)
    stop("Uma proporção exige uma variável categórica com exatamente duas categorias válidas.", call. = FALSE)
  if (!p$sucesso %in% niveis)
    stop("Escolha qual categoria representa o sucesso.", call. = FALSE)
  n <- length(x)
  sucessos <- sum(x == p$sucesso)
  referencia <- as.numeric(p$referencia)
  confianca <- as.numeric(p$confianca)
  if (!is.finite(referencia) || referencia < 0 || referencia > 1)
    stop("A proporção de referência deve estar entre 0 e 1.", call. = FALSE)
  if (!is.finite(confianca) || confianca <= 0 || confianca >= 1)
    stop("O nível de confiança deve estar entre 0 e 1.", call. = FALSE)
  teste <- stats::binom.test(sucessos, n, p = referencia,
                             alternative = catalyser_ou(p$alternativa, "two.sided"),
                             conf.level = confianca)
  proporcao <- sucessos / n
  tabela <- data.frame(
    Resultado = c(p$sucesso, setdiff(niveis, p$sucesso)),
    Contagem = c(sucessos, n - sucessos),
    `Proporção` = c(proporcao, 1 - proporcao),
    check.names = FALSE
  )
  diagnosticos <- data.frame(
    Indicador = c("Unidades válidas", "Sucessos", "Denominador", "Referência", "Método do IC e teste"),
    Valor = c(n, sucessos, n, referencia, "Binomial exato"),
    check.names = FALSE
  )
  grafico <- NULL
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    grafico <- ggplot2::ggplot(tabela, ggplot2::aes(x = .data$Resultado, y = .data$Proporção, fill = .data$Resultado)) +
      ggplot2::geom_col(show.legend = FALSE) +
      ggplot2::geom_hline(yintercept = referencia, linetype = 2, colour = "#E76F51") +
      ggplot2::scale_y_continuous(labels = function(z) paste0(round(100 * z), "%"), limits = c(0, 1)) +
      ggplot2::scale_fill_manual(values = c("#2E7D8F", "#62B6B7")) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::labs(x = NULL, y = "Proporção", title = "Uma proporção", subtitle = "Linha tracejada: proporção de referência")
  }
  list(
    narrativa = sprintf("Entre %d unidades independentes válidas, %d foram classificadas como %s (%s%%; IC de %d%%: %s a %s). Contra a referência de %s, o teste binomial exato %s.",
      n, sucessos, p$sucesso, catalyser_num(100 * proporcao, 1L), round(100 * confianca),
      catalyser_num(100 * teste$conf.int[1], 1L), catalyser_num(100 * teste$conf.int[2], 1L),
      catalyser_num(referencia, 3L), catalyser_p(teste$p.value)),
    tabela = tabela, grafico = grafico, diagnosticos = diagnosticos,
    console = utils::capture.output(print(teste)), objeto = teste
  )
}

#' Comparacao entre duas proporcoes independentes
#'
#' @param dados Um data.frame com resposta binária e dois grupos independentes.
#' @param p Lista com `resposta`, `grupo`, `sucesso`, `confianca`, `correcao` e
#'   `desenho`.
#' @return Lista com narrativa, tabela, grafico, diagnosticos, console e objeto.
catalyser_proporcao_duas <- function(dados, p) {
  if (!identical(p$desenho, "independente"))
    stop("Esta análise atende dois grupos independentes; pares, blocos e agrupamentos precisam de outro caminho.", call. = FALSE)
  catalyser_colunas(dados, c(p$resposta, p$grupo))
  d <- dados[stats::complete.cases(dados[c(p$resposta, p$grupo)]), c(p$resposta, p$grupo), drop = FALSE]
  resposta <- as.character(d[[p$resposta]])
  grupo <- as.character(d[[p$grupo]])
  niveis_resposta <- unique(resposta)
  niveis_grupo <- unique(grupo)
  if (length(niveis_resposta) != 2L)
    stop("A resposta deve ter exatamente duas categorias válidas.", call. = FALSE)
  if (length(niveis_grupo) != 2L)
    stop("A comparação exige exatamente dois grupos independentes.", call. = FALSE)
  if (!p$sucesso %in% niveis_resposta)
    stop("Escolha qual categoria representa o sucesso.", call. = FALSE)
  contagens <- vapply(niveis_grupo, function(g) sum(grupo == g), numeric(1))
  sucessos <- vapply(niveis_grupo, function(g) sum(grupo == g & resposta == p$sucesso), numeric(1))
  if (any(contagens == 0)) stop("Cada grupo precisa ter ao menos uma unidade válida.", call. = FALSE)
  confianca <- as.numeric(p$confianca)
  if (!is.finite(confianca) || confianca <= 0 || confianca >= 1)
    stop("O nível de confiança deve estar entre 0 e 1.", call. = FALSE)
  teste <- stats::prop.test(sucessos, contagens, correct = isTRUE(p$correcao), conf.level = confianca)
  tab <- rbind(sucessos, contagens - sucessos)
  rownames(tab) <- c(p$sucesso, setdiff(niveis_resposta, p$sucesso))
  colnames(tab) <- niveis_grupo
  fisher <- stats::fisher.test(tab)
  proporcoes <- sucessos / contagens
  tabela <- data.frame(
    Grupo = niveis_grupo, Sucessos = sucessos, Denominador = contagens,
    `Proporção` = proporcoes, check.names = FALSE
  )
  esperado_minimo <- min(stats::chisq.test(tab, correct = FALSE)$expected)
  usar_fisher <- esperado_minimo < 5
  diagnosticos <- data.frame(
    Indicador = c("Diferença (primeiro − segundo)", "Menor frequência esperada", "Teste de Fisher (p)", "Método principal"),
    Valor = c(proporcoes[1] - proporcoes[2], esperado_minimo, fisher$p.value,
              if (usar_fisher) "Teste exato de Fisher para o p; teste de duas proporções para IC" else if (isTRUE(p$correcao)) "Teste de duas proporções com correção de continuidade" else "Teste de duas proporções sem correção de continuidade"),
    check.names = FALSE
  )
  grafico <- NULL
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    grafico <- ggplot2::ggplot(tabela, ggplot2::aes(x = .data$Grupo, y = .data$Proporção, fill = .data$Grupo)) +
      ggplot2::geom_col(show.legend = FALSE) +
      ggplot2::scale_y_continuous(labels = function(z) paste0(round(100 * z), "%"), limits = c(0, 1)) +
      ggplot2::scale_fill_manual(values = c("#0F3B5F", "#E89B3C")) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::labs(x = NULL, y = "Proporção de sucesso", title = "Comparação entre duas proporções")
  }
  alerta <- if (usar_fisher) sprintf(" Como há frequência esperada menor que 5, o p do teste exato de Fisher (%s) é a referência para a decisão; o intervalo da diferença continua sendo apresentado como aproximação.", catalyser_p(fisher$p.value)) else ""
  list(
    narrativa = sprintf("A proporção de %s foi %s%% em %s e %s%% em %s; a diferença estimada (primeiro − segundo) foi %s ponto(s) percentual(is) (IC de %d%%: %s a %s). O teste de duas proporções %s.%s",
      p$sucesso, catalyser_num(100 * proporcoes[1], 1L), niveis_grupo[1], catalyser_num(100 * proporcoes[2], 1L), niveis_grupo[2],
      catalyser_num(100 * (proporcoes[1] - proporcoes[2]), 1L), round(100 * confianca),
      catalyser_num(100 * teste$conf.int[1], 1L), catalyser_num(100 * teste$conf.int[2], 1L), catalyser_p(teste$p.value), alerta),
    tabela = tabela, grafico = grafico, diagnosticos = diagnosticos,
    console = c(utils::capture.output(print(teste)), "", "Teste exato de Fisher:", utils::capture.output(print(fisher))),
    objeto = list(teste_proporcoes = teste, fisher = fisher)
  )
}

#' Qui-quadrado de aderencia
#'
#' @param dados Um data.frame com uma variável categórica.
#' @param p Lista com `variavel`, `esperadas` e `desenho`.
#' @return Lista com narrativa, tabela, grafico, diagnosticos, console e objeto.
catalyser_aderencia <- function(dados, p) {
  if (!identical(p$desenho, "independente"))
    stop("Esta análise atende unidades independentes; pares, blocos e agrupamentos precisam de outro caminho.", call. = FALSE)
  catalyser_colunas(dados, p$variavel)
  x <- as.character(dados[[p$variavel]])
  x <- x[!is.na(x)]
  categorias <- unique(x)
  if (length(categorias) < 2L) stop("A aderência exige ao menos duas categorias válidas.", call. = FALSE)
  esperadas <- as.numeric(p$esperadas[categorias])
  if (length(esperadas) != length(categorias) || any(!is.finite(esperadas)) || any(esperadas < 0))
    stop("Informe uma proporção esperada não negativa para cada categoria.", call. = FALSE)
  if (abs(sum(esperadas) - 1) > 1e-8)
    stop("As proporções esperadas devem somar 1 (ou 100%).", call. = FALSE)
  observadas <- as.numeric(table(factor(x, levels = categorias)))
  teste <- stats::chisq.test(observadas, p = esperadas)
  esperadas_n <- as.numeric(teste$expected)
  if (any(esperadas_n < 1) || mean(esperadas_n < 5) > .2) {
    stop("A aproximação qui-quadrado não é adequada: há frequência esperada menor que 1 ou mais de 20% das frequências esperadas são menores que 5. Reúna categorias de modo justificável ou use outro procedimento.", call. = FALSE)
  }
  tabela <- data.frame(
    Categoria = categorias, Observada = observadas, Esperada = esperadas_n,
    `Proporção esperada` = esperadas, check.names = FALSE
  )
  diagnosticos <- data.frame(
    Indicador = c("Unidades válidas", "Menor frequência esperada", "Regra de aproximação"),
    Valor = c(sum(observadas), min(esperadas_n), if (min(esperadas_n) < 5) "Aproximação qui-quadrado fragilizada" else "Frequências esperadas não menores que 5"),
    check.names = FALSE
  )
  grafico <- NULL
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    longo <- rbind(data.frame(Categoria = categorias, Tipo = "Observada", Frequência = observadas),
                   data.frame(Categoria = categorias, Tipo = "Esperada", Frequência = esperadas_n))
    grafico <- ggplot2::ggplot(longo, ggplot2::aes(x = .data$Categoria, y = .data$Frequência, fill = .data$Tipo)) +
      ggplot2::geom_col(position = "dodge") +
      ggplot2::scale_fill_manual(values = c("Observada" = "#2E7D8F", "Esperada" = "#E89B3C")) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::labs(x = NULL, y = "Frequência", title = "Contagens observadas e esperadas")
  }
  alerta <- if (min(esperadas_n) < 5) " Atenção: há frequência esperada menor que 5; interprete a aproximação com cautela." else ""
  list(
    narrativa = sprintf("O qui-quadrado de aderência comparou %d unidades independentes às proporções esperadas definidas pela pergunta: χ² = %s, gl = %d e %s.%s",
      sum(observadas), catalyser_num(teste$statistic), as.integer(teste$parameter), catalyser_p(teste$p.value), alerta),
    tabela = tabela, grafico = grafico, diagnosticos = diagnosticos,
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
    descricao_exploratoria = catalyser_descricao(dados, p),
    estatistica_descritiva = catalyser_resumo_descritivo(dados, p),
    regressao_linear = catalyser_regressao(dados, p, logistica = FALSE),
    regressao_logistica = catalyser_regressao(dados, p, logistica = TRUE),
    regressao_poisson = catalyser_regressao_contagem(dados, p, familia = "poisson"),
    regressao_binomial_negativa = catalyser_regressao_contagem(dados, p, familia = "binomial_negativa"),
    teste_t_one_val = catalyser_teste_t(dados, p),
    teste_t_two_ind = catalyser_teste_t(dados, p),
    teste_t_paired = catalyser_teste_t(dados, p),
    anova_um_fator = catalyser_anova(dados, p),
    anova_mista_subamostras = catalyser_anova_mista(dados, p),
    anova_medidas_repetidas = catalyser_anova_medidas_repetidas(dados, p),
    friedman = catalyser_friedman(dados, p),
    anova_dois_fatores = catalyser_anova_dois_fatores(dados, p),
    qui_quadrado_variancia = catalyser_variancia_uma(dados, p),
    teste_f_variancias = catalyser_variancias_duas(dados, p),
    grafico_linhas = catalyser_linhas(dados, p),
    qui_quadrado = catalyser_qui_quadrado(dados, p),
    proporcao_uma = catalyser_proporcao_uma(dados, p),
    proporcao_duas = catalyser_proporcao_duas(dados, p),
    qui_quadrado_aderencia = catalyser_aderencia(dados, p),
    mcnemar = catalyser_mcnemar(dados, p),
    pca = catalyser_pca(dados, p),
    hca = catalyser_hca(dados, p),
    stop(sprintf("O tipo de execução '%s' ainda não possui replay no exportador integrado.", tipo), call. = FALSE)
  )
  resultado$execucao <- execucao
  class(resultado) <- c("resultado_catalyser", "list")
  resultado
}
