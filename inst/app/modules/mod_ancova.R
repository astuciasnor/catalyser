# Modulo: ANCOVA (Analise de Covariancia) — versao basica, BOAS PRATICAS.
# Modelo lm(y ~ covariavel + fator). Fluxo recomendado:
#   1) homogeneidade de inclinacoes (testar a interacao covariavel*fator);
#   2) tabela de efeitos com Soma de Quadrados Tipo II (car::Anova);
#   3) medias marginais ajustadas + comparacoes par a par Tukey (emmeans);
#   4) tamanho de efeito eta^2 parcial (effectsize);
#   5) grafico com retas paralelas (efeito do grupo ajustado pela covariavel).
# Degrada com aviso se car/emmeans/effectsize nao estiverem instalados.
library(shiny)
library(bslib)

anc_ocean <- c("#0F3B5F", "#2E7D8F", "#E89B3C", "#E76F51", "#62B6B7", "#9AD1D4")

anc_pval <- function(p) if (is.null(p) || length(p) == 0 || is.na(p)) "—" else format.pval(p, digits = 3, eps = 1e-4)

# Narrativa em PT a partir do resultado calculado.
relatar_ancova <- function(r) {
  if (!isTRUE(r$ok)) return(r$msg)
  slope <- if (isTRUE(r$slopes_ok))
    sprintf("Inclinações homogêneas (interação p = %s ≥ 0,05): a ANCOVA é apropriada.", anc_pval(r$p_slopes))
  else
    sprintf("ATENÇÃO — inclinações diferem entre grupos (interação p = %s < 0,05): o pressuposto de paralelismo falhou; a ANCOVA padrão não é adequada (considere modelar a interação).", anc_pval(r$p_slopes))
  ig <- which(rownames(r$tab) == "grp")
  Fv <- if (length(ig)) r$tab[ig, "F value"] else NA
  pv <- if (length(ig)) r$tab[ig, grep("Pr", colnames(r$tab))[1]] else NA
  eta <- if (!is.null(r$eta2)) { ie <- which(r$eta2$Parameter == "grp"); if (length(ie)) r$eta2$Eta2_partial[ie] else NA } else NA
  efeito <- if (!is.na(pv) && pv < 0.05) "significativo" else "não significativo"
  et <- if (!is.na(eta)) sprintf(" (eta² parcial = %.3f)", eta) else ""
  sprintf("%s Ajustando '%s' pela covariável '%s', o efeito do fator '%s' foi %s: F = %.2f, p = %s%s.",
          slope, r$vy, r$vcov, r$vf, efeito, ifelse(is.na(Fv), NA, Fv), anc_pval(pv), et)
}

mod_ancova_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(1, 1, 1),
      style = "grid-template-columns: 2.9fr 6.3fr 2.8fr !important;",
      div(
        card(card_header("1. Variáveis"),
          card_body(style = "padding:12px 15px;",
            selectInput(ns("var_y"), "Resposta (numérica):", choices = NULL),
            selectInput(ns("var_fator"), "Fator (grupos):", choices = NULL),
            selectInput(ns("var_cov"), "Covariável (numérica):", choices = NULL),
            helpText("ANCOVA compara a resposta entre os grupos do fator, controlando (ajustando) pela covariável."))),
        card(card_header("Exportar"),
          card_body(style = "padding:12px 15px;",
            downloadButton(ns("baixar_script"), "Baixar script .R", class = "btn-outline-secondary btn-sm w-100")))
      ),
      navset_card_tab(
        title = "ANCOVA",
        nav_panel(title = "Resultado", icon = icon("table"),
          card_body(style = "padding:12px 15px;",
            uiOutput(ns("relato")),
            tags$h6("Tabela de efeitos (Soma de Quadrados Tipo II)", style = "color:#0F3B5F; margin-top:10px;"),
            tags$pre(style = "white-space:pre-wrap; font-size:0.85rem;", verbatimTextOutput(ns("tabela"))),
            tags$h6("Tamanho de efeito (eta² parcial)", style = "color:#0F3B5F; margin-top:6px;"),
            tags$pre(style = "white-space:pre-wrap; font-size:0.85rem;", verbatimTextOutput(ns("eta2"))))),
        nav_panel(title = "Pressupostos", icon = icon("clipboard-check"),
          card_body(style = "padding:12px 15px;",
            uiOutput(ns("pressupostos")))),
        nav_panel(title = "Médias ajustadas", icon = icon("equals"),
          card_body(style = "padding:12px 15px;",
            tags$h6("Médias marginais ajustadas (emmeans)", style = "color:#0F3B5F;"),
            tags$pre(style = "white-space:pre-wrap; font-size:0.85rem;", verbatimTextOutput(ns("emm"))),
            tags$h6("Comparações par a par (Tukey)", style = "color:#0F3B5F; margin-top:6px;"),
            tags$pre(style = "white-space:pre-wrap; font-size:0.85rem;", verbatimTextOutput(ns("pares"))))),
        nav_panel(title = "Gráfico", icon = icon("chart-line"),
          card_body(style = "padding:12px 15px;", plotOutput(ns("grafico"), height = "430px"))),
        nav_panel(title = "Script", icon = icon("code"),
          card_body(style = "padding:12px 15px;",
            tags$pre(style = "white-space:pre-wrap; font-size:0.82rem;", verbatimTextOutput(ns("script")))))
      ),
      div(
        card(card_header("Como ler"),
          card_body(style = "padding:12px 15px; font-size:0.82rem; line-height:1.5;",
            tags$ol(style = "padding-left:16px; margin:0;",
              tags$li("Confira o pressuposto: inclinações paralelas (aba Pressupostos)."),
              tags$li("Tabela Tipo II: o efeito do fator já é ajustado pela covariável."),
              tags$li("Médias ajustadas: as médias de cada grupo no mesmo valor da covariável."),
              tags$li("Comparações Tukey: quais grupos diferem entre si."))))
      )
    )
  )
}

mod_ancova_server <- function(id, data_rv, import_info) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    is_num <- function(df) names(df)[vapply(df, is.numeric, logical(1))]
    is_cat <- function(df) names(df)[vapply(df, function(x)
      is.character(x) || is.factor(x) || (is.numeric(x) && length(unique(x[!is.na(x)])) <= 10), logical(1))]

    observeEvent(data_rv(), {
      df <- data_rv(); req(df)
      num <- is_num(df); cat <- is_cat(df); if (!length(cat)) cat <- names(df)
      updateSelectInput(session, "var_y", choices = num, selected = isolate(input$var_y) %||% num[1])
      updateSelectInput(session, "var_cov", choices = num,
                        selected = isolate(input$var_cov) %||% (if (length(num) > 1) num[2] else num[1]))
      updateSelectInput(session, "var_fator", choices = cat, selected = isolate(input$var_fator) %||% cat[1])
    })

    ancova_res <- reactive({
      df <- data_rv(); req(df, input$var_y, input$var_fator, input$var_cov)
      vy <- input$var_y; vf <- input$var_fator; vcov <- input$var_cov
      if (!all(c(vy, vf, vcov) %in% names(df))) return(list(ok = FALSE, msg = "Selecione variáveis válidas."))
      if (length(unique(c(vy, vf, vcov))) < 3) return(list(ok = FALSE, msg = "Escolha três variáveis diferentes."))
      d <- data.frame(y = suppressWarnings(as.numeric(df[[vy]])),
                      cov = suppressWarnings(as.numeric(df[[vcov]])),
                      grp = factor(df[[vf]]), stringsAsFactors = FALSE)
      d <- d[stats::complete.cases(d), , drop = FALSE]
      if (nrow(d) < 5) return(list(ok = FALSE, msg = "Poucas observações completas (mínimo 5)."))
      if (nlevels(droplevels(d$grp)) < 2) return(list(ok = FALSE, msg = "O fator precisa de ao menos 2 grupos."))
      d$grp <- droplevels(d$grp)
      mod_add <- stats::lm(y ~ cov + grp, data = d)
      mod_int <- stats::lm(y ~ cov * grp, data = d)
      p_slopes <- tryCatch(stats::anova(mod_add, mod_int)[2, "Pr(>F)"], error = function(e) NA_real_)
      if (requireNamespace("car", quietly = TRUE)) {
        tab <- tryCatch(as.data.frame(car::Anova(mod_add, type = 2)), error = function(e) NULL)
        tipo <- "II (car::Anova)"
      } else { tab <- NULL; tipo <- NULL }
      if (is.null(tab)) { tab <- as.data.frame(stats::anova(mod_add)); tipo <- "I (anova base — instale 'car' para Tipo II)" }
      eta2 <- if (requireNamespace("effectsize", quietly = TRUE))
        tryCatch(as.data.frame(effectsize::eta_squared(mod_add, partial = TRUE)), error = function(e) NULL) else NULL
      emm <- NULL; pares <- NULL; emm_msg <- NULL
      if (requireNamespace("emmeans", quietly = TRUE)) {
        em <- tryCatch(emmeans::emmeans(mod_add, specs = "grp"), error = function(e) NULL)
        if (!is.null(em)) {
          emm <- tryCatch(as.data.frame(em), error = function(e) NULL)
          pares <- tryCatch(as.data.frame(emmeans::contrast(em, method = "pairwise", adjust = "tukey")),
                            error = function(e) NULL)
        }
      } else emm_msg <- "Instale o pacote 'emmeans' para as médias ajustadas e comparações."
      list(ok = TRUE, d = d, mod_add = mod_add, vy = vy, vcov = vcov, vf = vf,
           n = nrow(d), niveis = levels(d$grp), p_slopes = p_slopes,
           slopes_ok = is.na(p_slopes) || p_slopes >= 0.05,
           tab = tab, tipo = tipo, eta2 = eta2, emm = emm, pares = pares, emm_msg = emm_msg)
    })

    output$relato <- renderUI({
      r <- ancova_res(); req(r)
      if (!isTRUE(r$ok)) return(div(class = "alert alert-warning", r$msg))
      cor <- if (isTRUE(r$slopes_ok)) "alert-info" else "alert-warning"
      div(class = paste("alert", cor), style = "font-size:0.9rem;", relatar_ancova(r))
    })
    output$tabela <- renderPrint({
      r <- ancova_res(); req(r); if (!isTRUE(r$ok)) return(cat(r$msg))
      cat("Tipo de Soma de Quadrados:", r$tipo, "\n\n"); print(r$tab)
    })
    output$eta2 <- renderPrint({
      r <- ancova_res(); req(r, isTRUE(r$ok))
      if (is.null(r$eta2)) cat("Instale o pacote 'effectsize' para o eta² parcial.") else print(r$eta2)
    })
    output$pressupostos <- renderUI({
      r <- ancova_res(); req(r)
      if (!isTRUE(r$ok)) return(div(class = "alert alert-warning", r$msg))
      badge <- if (isTRUE(r$slopes_ok)) span(class = "badge bg-success", "OK") else span(class = "badge bg-danger", "violado")
      tagList(
        tags$h6("1. Homogeneidade das inclinações (paralelismo)", style = "color:#0F3B5F;"),
        tags$p(badge, sprintf(" Interação covariável × fator: p = %s. ", anc_pval(r$p_slopes)),
               if (isTRUE(r$slopes_ok)) "Inclinações compatíveis com paralelas → ANCOVA apropriada."
               else "Inclinações diferentes → o efeito do grupo depende da covariável (paralelismo falhou)."),
        tags$h6("2. Demais pressupostos", style = "color:#0F3B5F; margin-top:8px;"),
        tags$ul(style = "padding-left:16px; font-size:0.88rem;",
          tags$li("Relação linear entre covariável e resposta dentro de cada grupo."),
          tags$li("Resíduos aproximadamente normais e com variância homogênea."),
          tags$li("Observações independentes; covariável medida sem erro relevante."),
          tags$li("Use a aba Gráfico para inspecionar visualmente as retas.")))
    })
    output$emm <- renderPrint({
      r <- ancova_res(); req(r, isTRUE(r$ok))
      if (!is.null(r$emm_msg)) return(cat(r$emm_msg))
      if (is.null(r$emm)) cat("Não foi possível calcular as médias ajustadas.") else print(r$emm)
    })
    output$pares <- renderPrint({
      r <- ancova_res(); req(r, isTRUE(r$ok))
      if (is.null(r$pares)) cat("Sem comparações (verifique o pacote 'emmeans').") else print(r$pares)
    })
    output$grafico <- renderPlot({
      r <- ancova_res(); validate(need(isTRUE(r$ok), if (is.list(r)) r$msg else "Configure as variáveis."))
      d <- r$d; d$ajustado <- stats::fitted(r$mod_add)
      if (!requireNamespace("ggplot2", quietly = TRUE)) {
        plot(d$cov, d$y, col = anc_ocean[as.integer(d$grp)], pch = 19, xlab = r$vcov, ylab = r$vy); return(invisible())
      }
      ggplot2::ggplot(d, ggplot2::aes(x = cov, y = y, color = grp)) +
        ggplot2::geom_point(size = 2.2, alpha = 0.85) +
        ggplot2::geom_line(ggplot2::aes(y = ajustado), linewidth = 1.1) +
        ggplot2::scale_color_manual(values = rep(anc_ocean, length.out = nlevels(d$grp))) +
        ggplot2::labs(title = sprintf("ANCOVA: %s ~ %s + %s", r$vy, r$vcov, r$vf),
                      subtitle = "Retas paralelas = efeito do grupo ajustado pela covariável",
                      x = r$vcov, y = r$vy, color = r$vf) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", color = "#0F3B5F"),
                       plot.subtitle = ggplot2::element_text(size = 9, color = "#495057"))
    })

    codigo <- reactive({
      req(input$var_y, input$var_fator, input$var_cov)
      info <- if (is.function(import_info)) import_info() else NULL
      if (!is.null(info) && identical(info$source, "package")) {
        leitura <- c("library(EAPADados)", sprintf("data(%s)", info$package_dataset), sprintf("dados <- %s", info$package_dataset))
      } else {
        fn <- if (!is.null(info)) info$file_name else "SEU_ARQUIVO.xlsx"
        abn <- if (!is.null(info)) info$excel_sheet else "SUA_ABA"
        leitura <- sprintf('dados <- readxl::read_excel("%s", sheet = "%s")', fn, abn)
      }
      y <- input$var_y; f <- input$var_fator; cv <- input$var_cov
      paste(c(
        "# Script gerado pela CatalyseR — ANCOVA (boas praticas)",
        "library(car); library(emmeans); library(effectsize); library(ggplot2)",
        leitura, "",
        sprintf('dados[["%s"]] <- factor(dados[["%s"]])', f, f),
        sprintf('mod <- lm(`%s` ~ `%s` + `%s`, data = dados)', y, cv, f),
        "",
        "# 1) Homogeneidade das inclinacoes (a interacao NAO deve ser significativa):",
        sprintf('anova(mod, lm(`%s` ~ `%s` * `%s`, data = dados))', y, cv, f),
        "",
        "# 2) Tabela ANCOVA (Soma de Quadrados Tipo II):",
        "car::Anova(mod, type = 2)",
        "",
        "# 3) Tamanho de efeito (eta^2 parcial):",
        "effectsize::eta_squared(mod, partial = TRUE)",
        "",
        "# 4) Medias marginais ajustadas + comparacoes Tukey:",
        sprintf('emm <- emmeans(mod, ~ `%s`); emm', f),
        'contrast(emm, method = "pairwise", adjust = "tukey")',
        "",
        "# 5) Grafico com retas paralelas:",
        sprintf('ggplot(dados, aes(`%s`, `%s`, color = `%s`)) +', cv, y, f),
        "  geom_point() + geom_line(aes(y = fitted(mod))) + theme_minimal()"
      ), collapse = "\n")
    })
    output$script <- renderText(codigo())
    output$baixar_script <- downloadHandler(
      filename = function() paste0("ancova_", Sys.Date(), ".R"),
      content = function(file) writeLines(codigo(), file))
  })
}

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a
