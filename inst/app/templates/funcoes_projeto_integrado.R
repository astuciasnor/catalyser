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
    console = capture.output(print(tabela))
  )
}

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
      c(paste0("--- Grupo: ", nome, " ---"), capture.output(print(summary(ajuste))))
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
    console <- capture.output(print(sm))
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
    console = capture.output(print(teste)), objeto = teste
  )
}

catalyser_linhas <- function(dados, p) {
  catalyser_colunas(dados, c(p$x, p$y, if (!identical(catalyser_ou(p$grupo, "none"), "none")) p$grupo))
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("O pacote ggplot2 é necessário para o gráfico.", call. = FALSE)
  grupo <- catalyser_ou(p$grupo, "none")
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
    camada_linha +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::labs(x = catalyser_ou(p$rotulo_x, p$x), y = catalyser_ou(p$rotulo_y, p$y))
  if (isTRUE(p$mostrar_pontos)) grafico <- grafico + ggplot2::geom_point(size = 2.2)
  list(grafico = grafico)
}

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
    console = capture.output(print(teste)), objeto = teste
  )
}

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
    narrativa = sprintf("Os dois primeiros componentes explicaram %s%% da variância total.", catalyser_num(sum(head(variancia, 2L)), 1L)),
    tabela = tabela, grafico = grafico, diagnosticos = diagnosticos,
    console = capture.output(summary(ajuste)), objeto = ajuste
  )
}

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
    console = capture.output(print(ajuste)), objeto = ajuste
  )
}

catalyser_executar <- function(execucao, dados = NULL) {
  p <- execucao$parametros
  tipo <- execucao$tipo
  resultado <- switch(
    tipo,
    estatistica_descritiva = catalyser_resumo_descritivo(dados, p),
    regressao_linear = catalyser_regressao(dados, p, logistica = FALSE),
    regressao_logistica = catalyser_regressao(dados, p, logistica = TRUE),
    teste_t_one_val = catalyser_teste_t(dados, p),
    teste_t_two_ind = catalyser_teste_t(dados, p),
    teste_t_paired = catalyser_teste_t(dados, p),
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
