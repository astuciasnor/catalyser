# Módulo de Testes Não Paramétricos para a CatalyseR
# Qui-quadrado de independência (3 fontes: tabela preparada, duas variáveis,
# entrada manual), Mann-Whitney, Wilcoxon pareado e Kruskal-Wallis.

if (file.exists("templates/funcoes_nonparametric.R")) {
  source("templates/funcoes_nonparametric.R")
}

# Coalescência de nulos (uso interno)
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

np_contingencia_tidy_matriz <- function(df, linha, coluna, frequencia = "n") {
  faltantes <- setdiff(c(linha, coluna, frequencia), names(df))
  if (length(faltantes))
    stop(sprintf("Colunas ausentes na base tidy: %s.", paste(faltantes, collapse = ", ")),
         call. = FALSE)
  if (identical(linha, coluna))
    stop("As variáveis de linha e coluna precisam ser diferentes.", call. = FALSE)

  freq <- suppressWarnings(as.numeric(df[[frequencia]]))
  if (anyNA(freq) || any(!is.finite(freq)) || any(freq < 0))
    stop("A coluna de frequência deve conter contagens não negativas.", call. = FALSE)

  linha_val <- as.character(df[[linha]])
  coluna_val <- as.character(df[[coluna]])
  manter <- !is.na(linha_val) & !is.na(coluna_val)
  linha_val <- linha_val[manter]
  coluna_val <- coluna_val[manter]
  freq <- freq[manter]
  niveis_linha <- unique(linha_val)
  niveis_coluna <- unique(coluna_val)
  if (length(niveis_linha) < 2L || length(niveis_coluna) < 2L)
    stop("A base tidy precisa representar ao menos duas linhas e duas colunas.",
         call. = FALSE)

  tab <- matrix(
    0,
    nrow = length(niveis_linha),
    ncol = length(niveis_coluna),
    dimnames = list(niveis_linha, niveis_coluna)
  )
  for (i in seq_along(freq)) {
    tab[linha_val[[i]], coluna_val[[i]]] <-
      tab[linha_val[[i]], coluna_val[[i]]] + freq[[i]]
  }
  tab
}

# Ajusta os parâmetros do relatório Quarto dos testes não paramétricos
customize_np_qmd_params <- function(qmd_path, test_type, var_y, var_x,
                                    var1, var2, var_row, var_col, alternative, yates) {
  lines <- readLines(qmd_path, warn = FALSE)
  lines <- gsub('test_type: ".*"', sprintf('test_type: "%s"', test_type), lines)
  lines <- gsub('var_y: ".*"', sprintf('var_y: "%s"', var_y), lines)
  lines <- gsub('var_x: ".*"', sprintf('var_x: "%s"', var_x), lines)
  lines <- gsub('var1: ".*"', sprintf('var1: "%s"', var1), lines)
  lines <- gsub('var2: ".*"', sprintf('var2: "%s"', var2), lines)
  lines <- gsub('var_row: ".*"', sprintf('var_row: "%s"', var_row), lines)
  lines <- gsub('var_col: ".*"', sprintf('var_col: "%s"', var_col), lines)
  lines <- gsub('alternative: ".*"', sprintf('alternative: "%s"', alternative), lines)
  lines <- gsub('yates: .*', sprintf('yates: %s', tolower(as.character(yates))), lines)
  lines
}

# Título curto do teste (para o cabeçalho do card)
np_titulo <- function(fixed_test) {
  switch(fixed_test,
         quiquadrado = "Qui-quadrado de independência",
         mannwhitney = "Mann-Whitney",
         wilcoxon    = "Wilcoxon (pareado)",
         kruskal     = "Kruskal-Wallis",
         "Teste não paramétrico")
}

# Corpo de configuração conforme o teste fixado pelo item de menu
np_config_ui <- function(ns, fixed_test) {
  if (fixed_test == "quiquadrado") {
    tagList(
      radioButtons(ns("chi_source"), "Fonte dos dados:",
                   choices = c("Base tidy de contingência" = "tidy",
                               "Duas variáveis dos dados" = "vars",
                               "Entrada manual (digitar a tabela)" = "manual"),
                   selected = "vars"),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'tidy'", ns("chi_source")),
        selectInput(ns("chi_tidy_row"), "Variável de linha:", choices = NULL),
        selectInput(ns("chi_tidy_col"), "Variável de coluna:", choices = NULL),
        selectInput(ns("chi_tidy_n"), "Coluna de contagens:", choices = NULL),
        uiOutput(ns("chi_tidy_status")),
        helpText("Selecione, acima, uma Base Derivada criada pela etapa Tabela de Contingência.")
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'vars'", ns("chi_source")),
        selectInput(ns("chi_row"), "Variável de Linha (categórica):", choices = NULL),
        selectInput(ns("chi_col"), "Variável de Coluna (categórica):", choices = NULL)
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'manual'", ns("chi_source")),
        textInput(ns("man_rowname"), "Nome da 1ª variável (linhas):", value = ""),
        textInput(ns("man_colname"), "Nome da 2ª variável (colunas):", value = ""),
        fluidRow(
          column(6, numericInput(ns("man_nrow"), "Nº de níveis (linhas):", value = 2, min = 2, max = 10, step = 1)),
          column(6, numericInput(ns("man_ncol"), "Nº de níveis (colunas):", value = 2, min = 2, max = 10, step = 1))
        ),
        helpText("Digite as contagens de cada célula (sem os totais)."),
        uiOutput(ns("manual_grid"))
      ),
      hr(style = "margin: 8px 0;"),
      checkboxInput(ns("chi_yates"), "Correção de continuidade de Yates (2×2)", value = TRUE),
      checkboxInput(ns("chi_fisher"), "Também calcular o teste exato de Fisher", value = FALSE)
    )
  } else if (fixed_test == "mannwhitney") {
    tagList(
      selectInput(ns("mw_y"), "Variável Numérica (resposta):", choices = NULL),
      selectInput(ns("mw_x"), "Variável de Agrupamento (2 níveis):", choices = NULL),
      selectInput(ns("mw_alt"), "Hipótese Alternativa (H1):",
                  choices = c("Bilateral (≠)" = "two.sided", "Maior (>)" = "greater", "Menor (<)" = "less"))
    )
  } else if (fixed_test == "wilcoxon") {
    tagList(
      selectInput(ns("wil_1"), "Variável 1 (Antes):", choices = NULL),
      selectInput(ns("wil_2"), "Variável 2 (Depois):", choices = NULL),
      selectInput(ns("wil_alt"), "Hipótese Alternativa (H1):",
                  choices = c("Bilateral (≠)" = "two.sided", "Maior (>)" = "greater", "Menor (<)" = "less"))
    )
  } else {
    tagList(
      selectInput(ns("kw_y"), "Variável Numérica (resposta):", choices = NULL),
      selectInput(ns("kw_x"), "Variável de Agrupamento (categórica):", choices = NULL),
      checkboxInput(ns("kw_posthoc"), "Pós-teste de Dunn + letras (quais grupos diferem)", value = TRUE),
      conditionalPanel(
        condition = sprintf("input['%s']", ns("kw_posthoc")),
        selectInput(ns("kw_padjust"), "Ajuste de p (pós-teste):",
                    choices = c("Bonferroni" = "bonferroni", "Holm" = "holm", "BH (FDR)" = "BH"),
                    selected = "bonferroni")
      )
    )
  }
}

mod_nonparametric_ui <- function(id, fixed_test = "quiquadrado") {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(1, 1, 1),
      fill = FALSE,
      fillable = FALSE,
      style = paste(
        "grid-template-columns: 3fr 6fr 2.5fr !important;",
        "align-items:start !important;"
      ),

      # COLUNA 1: CONFIGURAÇÃO DO TESTE
      div(
        card(
          fill = FALSE,
          card_header(paste0("Configuração — ", np_titulo(fixed_test))),
          card_body(
            style = "padding: 12px 15px;",
            np_config_ui(ns, fixed_test),
            execucao_explicita_controles_ui(
              ns, ativo = identical(fixed_test, "quiquadrado")
            )
          )
        ),
        card(
          fill = FALSE,
          card_header("Relatório e Pacote de Estudo"),
          card_body(
            style = "padding: 12px 15px;",
            execucao_explicita_downloads_ui(ns, tagList(
              downloadButton(ns("download_report_docx"), "Baixar Relatório Word (.docx)", class = "btn-success w-100"),
              div(style = "margin-top: 8px;"),
              downloadButton(ns("download_project_zip"), "Exportar Projeto R (.zip)", class = "btn-primary w-100"),
              helpText("Gera o relatório em DOCX ou exporta um projeto Quarto completo.", style = "margin-top: 10px; font-size: 0.85rem;")
            ), ativo = identical(fixed_test, "quiquadrado"))
          )
        )
      ),

      # COLUNA 2: RESULTADOS
      execucao_explicita_resultados_ui(ns, navset_card_tab(
        id = ns("active_tab"),
        title = "Painel de Resultados",
        nav_panel(
          title = "Resultado do Teste",
          icon = icon("table"),
          card_body(
            uiOutput(ns("chi_table_ui")),
            div(style = "margin-bottom: -10px;", DTOutput(ns("results_table"), height = "auto")),
            uiOutput(ns("fisher_ui")),
            hr(style = "margin: 15px 0; border-color: #dee2e6;"),
            uiOutput(ns("relato_ui"))
          )
        ),
        nav_panel(
          title = "Gráfico",
          icon = icon("chart-bar"),
          card_body(
            plotOutput(ns("test_plot"), height = "450px")
          )
        )
      ), ativo = identical(fixed_test, "quiquadrado")),

      # COLUNA 3: AJUDA
      card(
        fill = FALSE,
        card_header("Como interpretar"),
        card_body(
          helpText(HTML(
            "<p style='font-size:0.9rem;'>Os testes não paramétricos não exigem normalidade — comparam <b>postos</b> (ordens) ou <b>frequências</b>.</p>
             <ul style='font-size:0.88rem; padding-left:16px;'>
               <li><b>p &lt; 0,05:</b> rejeita-se H0 (há diferença/associação).</li>
               <li><b>p &ge; 0,05:</b> não se rejeita H0.</li>
               <li><b>Qui-quadrado:</b> use só as <b>contagens</b>, sem a coluna/linha de Total; se algum esperado &lt; 5, prefira Fisher.</li>
             </ul>"
          ))
        )
      )
    )
  )
}

mod_nonparametric_server <- function(id, data_rv, import_info, contingency_shared = NULL, fixed_test = "quiquadrado") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    modo_explicito <- identical(fixed_test, "quiquadrado")
    revisao_execucao <- if (modo_explicito) execucao_revisao_dados(data_rv) else reactive(0L)

    # Atualiza os seletores de variáveis
    observe({
      df <- data_rv()
      req(df)
      num_cols <- names(df)[sapply(df, is.numeric)]
      cat_cols <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x) || is.logical(x) || length(unique(x)) < 15)]
      if (length(cat_cols) == 0) cat_cols <- names(df)
      if (length(num_cols) == 0) num_cols <- names(df)

      manter <- function(atual, escolhas, padrao = NULL) {
        if (!is.null(atual) && atual %in% escolhas) atual else padrao
      }
      updateSelectInput(session, "chi_row", choices = cat_cols,
                        selected = manter(isolate(input$chi_row), cat_cols, cat_cols[1]))
      updateSelectInput(session, "chi_col", choices = cat_cols,
                        selected = manter(isolate(input$chi_col), cat_cols,
                                          if (length(cat_cols) > 1) cat_cols[2] else cat_cols[1]))
      meta_contingencia <- attr(df, "catalyser_contingencia", exact = TRUE)
      tem_meta_contingencia <- !is.null(meta_contingencia)
      linha_tidy <- meta_contingencia$var_row %||% cat_cols[1]
      coluna_tidy <- meta_contingencia$var_col %||%
        if (length(cat_cols) > 1) cat_cols[2] else cat_cols[1]
      frequencia_tidy <- meta_contingencia$freq %||%
        if ("n" %in% num_cols) "n" else num_cols[1]
      if (tem_meta_contingencia) {
        updateSelectInput(
          session, "chi_tidy_row", choices = cat_cols,
          selected = linha_tidy
        )
        updateSelectInput(
          session, "chi_tidy_col", choices = cat_cols,
          selected = coluna_tidy
        )
        updateSelectInput(
          session, "chi_tidy_n", choices = num_cols,
          selected = frequencia_tidy
        )
      } else {
        updateSelectInput(session, "chi_tidy_row", choices = character(0))
        updateSelectInput(session, "chi_tidy_col", choices = character(0))
        updateSelectInput(session, "chi_tidy_n", choices = character(0))
      }
      updateSelectInput(session, "mw_y", choices = num_cols,
                        selected = manter(isolate(input$mw_y), num_cols, num_cols[1]))
      updateSelectInput(session, "mw_x", choices = cat_cols,
                        selected = manter(isolate(input$mw_x), cat_cols, cat_cols[1]))
      updateSelectInput(session, "wil_1", choices = num_cols,
                        selected = manter(isolate(input$wil_1), num_cols, num_cols[1]))
      updateSelectInput(session, "wil_2", choices = num_cols,
                        selected = manter(isolate(input$wil_2), num_cols,
                                          if (length(num_cols) > 1) num_cols[2] else num_cols[1]))
      updateSelectInput(session, "kw_y", choices = num_cols,
                        selected = manter(isolate(input$kw_y), num_cols, num_cols[1]))
      updateSelectInput(session, "kw_x", choices = cat_cols,
                        selected = manter(isolate(input$kw_x), cat_cols, cat_cols[1]))
    })

    observeEvent(data_rv(), {
      df <- data_rv()
      req(df)
      if (!is.null(attr(df, "catalyser_contingencia", exact = TRUE)))
        updateRadioButtons(session, "chi_source", selected = "tidy")
    }, ignoreInit = FALSE)

    output$chi_tidy_status <- renderUI({
      df <- data_rv()
      req(df)
      meta <- attr(df, "catalyser_contingencia", exact = TRUE)
      if (is.null(meta)) {
        div(
          class = "alert alert-warning py-2 mb-2",
          style = "font-size:0.8rem;",
          icon("triangle-exclamation"),
          " A Base utilizada não é uma contingência tidy. Escolha, no topo, uma ",
          "Base Derivada recalculada e finalizada; não use a coluna id como contagem."
        )
      } else {
        div(
          class = "alert alert-success py-2 mb-2",
          style = "font-size:0.8rem;",
          icon("circle-check"),
          " Contingência tidy reconhecida: ",
          tags$code(meta$var_row), " × ", tags$code(meta$var_col),
          "; frequência em ", tags$code(meta$freq), "."
        )
      }
    })

    # Grade de entrada manual da tabela de contingência
    output$manual_grid <- renderUI({
      nr <- input$man_nrow %||% 2
      nc <- input$man_ncol %||% 2
      nr <- max(2, min(10, as.integer(nr)))
      nc <- max(2, min(10, as.integer(nc)))

      header <- div(
        div(style = "display:inline-block; width:110px; margin:2px; font-weight:600; font-size:0.8rem;", "Linha \\ Coluna"),
        lapply(seq_len(nc), function(j) {
          div(style = "display:inline-block; width:74px; margin:2px;",
              textInput(ns(paste0("collab_", j)), NULL, value = "", placeholder = paste0("Col ", j)))
        })
      )
      grid_rows <- lapply(seq_len(nr), function(i) {
        rowlab <- div(style = "display:inline-block; width:110px; margin:2px; vertical-align:top;",
                      textInput(ns(paste0("rowlab_", i)), NULL, value = "", placeholder = paste0("Lin ", i)))
        cells <- lapply(seq_len(nc), function(j) {
          div(style = "display:inline-block; width:74px; margin:2px;",
              numericInput(ns(paste0("cell_", i, "_", j)), NULL, value = 0, min = 0, step = 1))
        })
        div(rowlab, cells)
      })
      tagList(header, grid_rows)
    })

    # Monta a matriz de contagens digitada
    build_manual_table <- reactive({
      nr <- max(2, min(10, as.integer(input$man_nrow %||% 2)))
      nc <- max(2, min(10, as.integer(input$man_ncol %||% 2)))
      m <- matrix(0, nrow = nr, ncol = nc)
      for (i in seq_len(nr)) {
        for (j in seq_len(nc)) {
          v <- input[[paste0("cell_", i, "_", j)]]
          m[i, j] <- if (is.null(v) || is.na(v)) 0 else as.numeric(v)
        }
      }
      rlabs <- vapply(seq_len(nr), function(i) {
        v <- input[[paste0("rowlab_", i)]]
        if (is.null(v) || !nzchar(v)) paste0("Linha ", i) else v
      }, character(1))
      clabs <- vapply(seq_len(nc), function(j) {
        v <- input[[paste0("collab_", j)]]
        if (is.null(v) || !nzchar(v)) paste0("Coluna ", j) else v
      }, character(1))
      rownames(m) <- rlabs
      colnames(m) <- clabs
      m
    })

    # Constrói a tabela de contagens do qui-quadrado conforme a fonte
    chi_input <- reactive({
      src <- input$chi_source %||% "vars"
      if (src == "tidy") {
        df <- data_rv()
        req(df)
        meta <- attr(df, "catalyser_contingencia", exact = TRUE)
        validate(need(
          !is.null(meta),
          paste(
            "A Base utilizada não é uma contingência tidy.",
            "Recalcule e finalize a Base Derivada e selecione-a no topo."
          )
        ))
        req(input$chi_tidy_row, input$chi_tidy_col, input$chi_tidy_n)
        tab <- tryCatch(
          np_contingencia_tidy_matriz(
            df, input$chi_tidy_row, input$chi_tidy_col, input$chi_tidy_n
          ),
          error = function(e) e
        )
        validate(need(!inherits(tab, "error"),
                      if (inherits(tab, "error")) conditionMessage(tab) else ""))
        list(
          tab = tab,
          var_row = input$chi_tidy_row,
          var_col = input$chi_tidy_col
        )
      } else if (src == "prepared") {
        validate(need(!is.null(contingency_shared), "Fonte 'tabela preparada' indisponível."))
        r <- contingency_shared()
        validate(need(!is.null(r) && !is.null(r$tab), "Nenhuma tabela de contingência foi construída ainda no menu de preparação."))
        list(tab = as.matrix(r$tab), var_row = r$var_row %||% "Linha", var_col = r$var_col %||% "Coluna")
      } else if (src == "manual") {
        m <- build_manual_table()
        validate(need(sum(m) > 0, "Digite as contagens da tabela para rodar o teste."))
        list(tab = m,
             var_row = if (nzchar(input$man_rowname %||% "")) input$man_rowname else "Variável 1",
             var_col = if (nzchar(input$man_colname %||% "")) input$man_colname else "Variável 2")
      } else {
        df <- data_rv()
        req(df, input$chi_row, input$chi_col)
        d <- df[!is.na(df[[input$chi_row]]) & !is.na(df[[input$chi_col]]), ]
        tab <- table(as.factor(d[[input$chi_row]]), as.factor(d[[input$chi_col]]))
        list(tab = as.matrix(tab), var_row = input$chi_row, var_col = input$chi_col)
      }
    })

    assinatura_execucao <- reactive({
      req(modo_explicito)
      ci <- chi_input()
      fonte <- input$chi_source %||% "vars"
      list(
        revisao_dados = if (fonte %in% c("vars", "tidy"))
          as.integer(revisao_execucao()) else 0L,
        parametros = list(
          fonte = fonte,
          linha = ci$var_row,
          coluna = ci$var_col,
          tabela = ci$tab,
          correcao_yates = isTRUE(input$chi_yates),
          teste_fisher = isTRUE(input$chi_fisher)
        )
      )
    })

    # Resultado do teste selecionado
    calcular_resultado <- function() {
      tt <- fixed_test

      if (tt == "quiquadrado") {
        ci <- chi_input()
        validate(need(!is.null(ci$tab) && nrow(ci$tab) >= 2 && ncol(ci$tab) >= 2,
                      "A tabela precisa de pelo menos 2 linhas e 2 colunas."))
        r <- calcular_qui_quadrado(ci$tab, correct = isTRUE(input$chi_yates),
                                   var_row = ci$var_row, var_col = ci$var_col)
        fisher <- if (isTRUE(input$chi_fisher)) calcular_fisher(ci$tab) else NULL
        list(type = "quiquadrado", r = r, arr = arrumar_qui_quadrado(r),
             relato = relatar_qui_quadrado(r), fisher = fisher, tab = ci$tab)

      } else if (tt == "mannwhitney") {
        df <- data_rv(); req(df, input$mw_y, input$mw_x)
        r <- tryCatch(calcular_mann_whitney(df, input$mw_y, input$mw_x, input$mw_alt),
                      error = function(e) e)
        validate(need(!inherits(r, "error"), if (inherits(r, "error")) conditionMessage(r) else ""))
        list(type = "mannwhitney", r = r, arr = arrumar_mann_whitney(r), relato = relatar_mann_whitney(r))

      } else if (tt == "wilcoxon") {
        df <- data_rv(); req(df, input$wil_1, input$wil_2)
        r <- tryCatch(calcular_wilcoxon(df, input$wil_1, input$wil_2, input$wil_alt),
                      error = function(e) e)
        validate(need(!inherits(r, "error"), if (inherits(r, "error")) conditionMessage(r) else ""))
        list(type = "wilcoxon", r = r, arr = arrumar_wilcoxon(r), relato = relatar_wilcoxon(r))

      } else if (tt == "kruskal") {
        df <- data_rv(); req(df, input$kw_y, input$kw_x)
        r <- tryCatch(calcular_kruskal(df, input$kw_y, input$kw_x), error = function(e) e)
        validate(need(!inherits(r, "error"), if (inherits(r, "error")) conditionMessage(r) else ""))
        metodo <- if (!is.null(input$kw_padjust) && nzchar(input$kw_padjust)) input$kw_padjust else "bonferroni"
        letras <- if (isTRUE(input$kw_posthoc)) letras_dunn(df, input$kw_y, input$kw_x, metodo) else NULL
        relato <- relatar_kruskal(r)
        if (!is.null(letras) && nrow(letras) > 0) {
          rot <- paste(paste0(letras$grupo, " (", letras$letra, ")"), collapse = "; ")
          relato <- paste0(relato, " Pós-teste de Dunn — grupos com a mesma letra não diferem (α = 0,05): ", rot, ".")
        } else if (isTRUE(input$kw_posthoc)) {
          relato <- paste0(relato, " (Para o pós-teste de Dunn e as letras, instale os pacotes rstatix e rcompanion.)")
        }
        list(type = "kruskal", r = r, arr = arrumar_kruskal(r), relato = relato, letras = letras)
      }
    }

    test_results <- if (modo_explicito) {
      eventReactive(input$executar_analise, calcular_resultado(), ignoreInit = TRUE)
    } else {
      reactive(calcular_resultado())
    }

    exec_ctrl <- if (modo_explicito) {
      execucao_explicita_server(
        input, output, session, assinatura_execucao, test_results,
        nome_analise = "O qui-quadrado"
      )
    } else {
      list(estado = reactive("atualizada"), atualizada = reactive(TRUE))
    }

    # Mostra a tabela cruzada (só no qui-quadrado)
    output$chi_table_ui <- renderUI({
      res <- test_results()
      req(res)
      if (res$type != "quiquadrado") return(NULL)
      m <- res$tab
      df_tab <- as.data.frame.matrix(m)
      df_tab <- cbind(" " = rownames(m), df_tab)
      tagList(
        h6(sprintf("Tabela de contingência: %s (linhas) × %s (colunas)", res$r$var_row, res$r$var_col),
           style = "font-weight:700; color:#0f3b5f; margin-bottom:8px;"),
        renderDT({
          datatable(df_tab, options = list(dom = "t", ordering = FALSE), rownames = FALSE, selection = "none")
        }),
        hr(style = "margin: 12px 0;")
      )
    })

    # Tabela de resultados do teste
    output$results_table <- renderDT({
      res <- test_results()
      req(res)
      datatable(res$arr, options = list(dom = "t", ordering = FALSE), rownames = FALSE, selection = "none")
    })

    # Fisher (opcional)
    output$fisher_ui <- renderUI({
      res <- test_results()
      req(res)
      if (res$type != "quiquadrado" || is.null(res$fisher)) return(NULL)
      p <- res$fisher$p
      p_str <- if (is.na(p)) "não pôde ser calculado" else if (p < 0.001) "p < 0,001" else paste0("p = ", formatC(p, format = "f", digits = 4, decimal.mark = ","))
      div(class = "alert alert-light",
          style = "border-left: 4px solid #2E7D8F; background:#f8f9fa; font-size:0.9rem; padding:10px 14px; margin-top:10px;",
          HTML(sprintf("<b>Teste exato de Fisher:</b> %s", p_str)))
    })

    # Relato automático em português
    output$relato_ui <- renderUI({
      res <- test_results()
      req(res)
      p_val <- res$r$p
      cor <- if (!is.na(p_val) && p_val < 0.05) "#0f5132" else "#495057"
      bg  <- if (!is.na(p_val) && p_val < 0.05) "#d1e7dd" else "#e9ecef"
      div(style = sprintf("background:%s; color:%s; border-radius:8px; padding:12px 15px; font-size:0.95rem; line-height:1.5;", bg, cor),
          res$relato)
    })

    # Gráfico do teste
    output$test_plot <- renderPlot({
      res <- test_results()
      req(res)
      ocean <- c("#0F3B5F", "#62B6B7", "#E89B3C", "#E76F51", "#2E7D8F")

      if (res$type == "quiquadrado") {
        dfp <- as.data.frame(as.table(as.matrix(res$tab)))
        names(dfp) <- c("Linha", "Coluna", "Freq")
        ncol_lv <- length(unique(dfp$Coluna))
        fill_cols <- rep(ocean, length.out = ncol_lv)
        ggplot(dfp, aes(x = Linha, y = Freq, fill = Coluna)) +
          geom_col(position = "dodge", color = "white", linewidth = 0.4, alpha = 0.9) +
          scale_fill_manual(values = fill_cols) +
          theme_minimal(base_size = 14) +
          labs(title = sprintf("Frequências: %s por %s", res$r$var_col, res$r$var_row),
               x = res$r$var_row, y = "Frequência (contagem)", fill = res$r$var_col) +
          theme(plot.title = element_text(face = "bold", color = "#0F3B5F"), legend.position = "bottom")

      } else if (res$type == "wilcoxon") {
        df <- data_rv(); req(df)
        d <- na.omit(df[, c(res$r$var1, res$r$var2)])
        d$ID <- seq_len(nrow(d))
        dlong <- data.frame(
          ID = rep(d$ID, 2),
          Condicao = factor(rep(c(res$r$var1, res$r$var2), each = nrow(d)), levels = c(res$r$var1, res$r$var2)),
          Valor = c(d[[res$r$var1]], d[[res$r$var2]])
        )
        ggplot(dlong, aes(x = Condicao, y = Valor, group = ID)) +
          geom_line(color = "gray70", alpha = 0.6) +
          geom_point(aes(color = Condicao), size = 2.5, alpha = 0.85) +
          geom_boxplot(aes(group = Condicao), fill = NA, color = "black", outlier.color = NA, width = 0.3) +
          scale_color_manual(values = ocean) +
          theme_minimal(base_size = 14) +
          labs(title = sprintf("Comparação pareada: %s vs %s", res$r$var1, res$r$var2), x = NULL, y = "Valores") +
          theme(plot.title = element_text(face = "bold", color = "#0F3B5F"), legend.position = "none")

      } else if (res$type == "kruskal") {
        # Kruskal-Wallis: boxplot por grupo + média (losango) + letras (CLD) no topo
        df <- data_rv(); req(df)
        vy <- res$r$var_y; vx <- res$r$var_x
        d <- na.omit(df[, c(vy, vx)])
        d[[vx]] <- as.factor(d[[vx]])
        p <- ggplot(d, aes(x = .data[[vx]], y = .data[[vy]], fill = .data[[vx]], color = .data[[vx]])) +
          geom_boxplot(outlier.shape = NA, alpha = 0.45, linewidth = 0.5) +
          geom_jitter(position = position_jitter(width = 0.2), alpha = 0.55, size = 1.6) +
          stat_summary(fun = mean, geom = "point", shape = 23, size = 2.7, fill = "black", color = "black") +
          labs(title = sprintf("%s por %s (Kruskal-Wallis)", vy, vx), x = vx, y = vy) +
          theme_classic(base_size = 14) +
          theme(legend.position = "none", panel.grid.major.x = element_blank(),
                plot.title = element_text(face = "bold", color = "#0F3B5F"))
        if (!is.null(res$letras) && nrow(res$letras) > 0) {
          ytop <- max(d[[vy]], na.rm = TRUE) * 1.06
          p <- p + geom_text(data = res$letras, aes(x = grupo, y = ytop, label = letra),
                             inherit.aes = FALSE, fontface = "bold", color = "grey20", size = 5)
        }
        p
      } else {
        # Mann-Whitney: boxplot por grupo
        df <- data_rv(); req(df)
        vy <- res$r$var_y; vx <- res$r$var_x
        d <- na.omit(df[, c(vy, vx)])
        d[[vx]] <- as.factor(d[[vx]])
        ggplot(d, aes(x = .data[[vx]], y = .data[[vy]], fill = .data[[vx]])) +
          geom_boxplot(alpha = 0.8, outlier.color = NA) +
          geom_jitter(color = "#495057", width = 0.15, alpha = 0.45, size = 2) +
          scale_fill_manual(values = rep(ocean, length.out = length(levels(d[[vx]])))) +
          theme_minimal(base_size = 14) +
          labs(title = sprintf("Distribuição de %s por %s", vy, vx), x = vx, y = vy) +
          theme(plot.title = element_text(face = "bold", color = "#0F3B5F"), legend.position = "none")
      }
    })

    # ---- Parâmetros correntes para exportação ----
    export_params <- reactive({
      tt <- fixed_test
      p <- list(test_type = tt, var_y = "", var_x = "", var1 = "", var2 = "",
                var_row = "Linha", var_col = "Coluna",
                alternative = "two.sided", yates = isTRUE(input$chi_yates))
      if (tt == "quiquadrado") {
        ci <- chi_input()
        p$var_row <- ci$var_row; p$var_col <- ci$var_col
      } else if (tt == "mannwhitney") {
        p$var_y <- input$mw_y; p$var_x <- input$mw_x; p$alternative <- input$mw_alt
      } else if (tt == "wilcoxon") {
        p$var1 <- input$wil_1; p$var2 <- input$wil_2; p$alternative <- input$wil_alt
      } else if (tt == "kruskal") {
        p$var_y <- input$kw_y; p$var_x <- input$kw_x
      }
      p
    })

    # Nome amigável do teste (para arquivos)
    nome_teste <- reactive({
      switch(fixed_test,
             quiquadrado = "qui_quadrado", mannwhitney = "mann_whitney",
             wilcoxon = "wilcoxon", kruskal = "kruskal_wallis")
    })

    # Salva a tabela do qui-quadrado (para o relatório reproduzir manual/preparada)
    salvar_tabela_np <- function(dir) {
      if (fixed_test == "quiquadrado") {
        tab_np <- chi_input()$tab
        save(tab_np, file = file.path(dir, "tabela_contingencia.rda"))
      }
    }

    # ---- Download do Relatório Word (.docx) ----
    output$download_report_docx <- downloadHandler(
      filename = function() paste0("relatorio_", nome_teste(), "_", format(Sys.Date(), "%Y-%m-%d"), ".docx"),
      content = function(file) {
        req(data_rv())
        temp_dir <- tempdir()
        temp_qmd  <- file.path(temp_dir, "relatorio_nonparametric.qmd")
        file.copy("templates/custom-reference.docx", file.path(temp_dir, "custom-reference.docx"), overwrite = TRUE)
        file.copy("templates/funcoes_nonparametric.R", file.path(temp_dir, "funcoes_nonparametric.R"), overwrite = TRUE)
        file.copy("templates/relatorio_nonparametric.qmd", temp_qmd, overwrite = TRUE)

        df_clean <- data_rv()
        save(df_clean, file = file.path(temp_dir, "dados_limpos.rda"))
        salvar_tabela_np(temp_dir)

        pp <- export_params()
        qmd_lines <- customize_np_qmd_params(temp_qmd, pp$test_type, pp$var_y, pp$var_x,
                                             pp$var1, pp$var2, pp$var_row, pp$var_col,
                                             pp$alternative, pp$yates)
        writeLines(qmd_lines, temp_qmd)

        old_wd <- getwd(); setwd(temp_dir)
        system2("quarto", args = c("render", "relatorio_nonparametric.qmd", "--to", "docx"))
        setwd(old_wd)

        generated <- file.path(temp_dir, "relatorio_nonparametric.docx")
        if (file.exists(generated)) file.copy(generated, file, overwrite = TRUE)
        else writeLines("Erro: não foi possível renderizar o relatório .docx com o Quarto CLI.", file)
      }
    )

    # Código R reprodutível
    r_code_np <- reactive({
      pp <- export_params()
      tt <- pp$test_type
      code <- c(
        "# --- Reprodutibilidade: Testes Não Paramétricos (CatalyseR) ---",
        "# install.packages(c('ggplot2'))",
        "source('scripts/funcoes_nonparametric.R')",
        "library(ggplot2)",
        ""
      )
      if (tt == "quiquadrado") {
        code <- c(code,
          "# Tabela de contingência (contagens, sem totais)",
          "load('dados/tabela_contingencia.rda')  # objeto: tab_np",
          sprintf("r <- calcular_qui_quadrado(tab_np, correct = %s, var_row = '%s', var_col = '%s')",
                  tolower(as.character(pp$yates)), pp$var_row, pp$var_col),
          "print(arrumar_qui_quadrado(r))",
          "cat(relatar_qui_quadrado(r))",
          "# Alternativa exata:", "print(calcular_fisher(tab_np))")
      } else {
        code <- c(code, "load('dados/dados_limpos.rda')", "dados <- df_clean", "")
        if (tt == "mannwhitney") {
          code <- c(code,
            sprintf("r <- calcular_mann_whitney(dados, '%s', '%s', alternative = '%s')", pp$var_y, pp$var_x, pp$alternative),
            "print(arrumar_mann_whitney(r)); cat(relatar_mann_whitney(r))")
        } else if (tt == "wilcoxon") {
          code <- c(code,
            sprintf("r <- calcular_wilcoxon(dados, '%s', '%s', alternative = '%s')", pp$var1, pp$var2, pp$alternative),
            "print(arrumar_wilcoxon(r)); cat(relatar_wilcoxon(r))")
        } else if (tt == "kruskal") {
          code <- c(code,
            sprintf("r <- calcular_kruskal(dados, '%s', '%s')", pp$var_y, pp$var_x),
            "print(arrumar_kruskal(r)); cat(relatar_kruskal(r))")
        }
      }
      paste(code, collapse = "\n")
    })

    # ---- Download do Projeto (.zip) ----
    output$download_project_zip <- downloadHandler(
      filename = function() paste0("projeto_", nome_teste(), "_", format(Sys.Date(), "%Y-%m-%d"), ".zip"),
      content = function(file) {
        info <- import_info()
        proj_dir_name <- paste0("projeto_", nome_teste(), "_", format(Sys.Date(), "%Y-%m-%d"))
        temp_dir <- tempdir()
        proj_dir <- file.path(temp_dir, proj_dir_name)
        dir.create(proj_dir, showWarnings = FALSE)
        dir_dados <- file.path(proj_dir, "dados")
        dir_scripts <- file.path(proj_dir, "scripts")
        dir_relatorios <- file.path(proj_dir, "relatorios")
        dir.create(dir_dados, showWarnings = FALSE)
        dir.create(dir_scripts, showWarnings = FALSE)
        dir.create(dir_relatorios, showWarnings = FALSE)

        df_clean <- data_rv()
        req(df_clean)
        save(df_clean, file = file.path(dir_dados, "dados_limpos.rda"))
        write.csv(df_clean, file = file.path(dir_dados, "dados_limpos.csv"), row.names = FALSE)
        ds_name <- if (info$source == "package") info$package_dataset else info$excel_sheet
        tryCatch(export_to_xlsx(df_clean, dataset_name = ds_name, file_path = file.path(dir_dados, "dados_limpos.xlsx")),
                 error = function(e) NULL)
        salvar_tabela_np(dir_dados)

        writeLines(r_code_np(), file.path(dir_scripts, paste0("analise_", nome_teste(), ".R")))
        file.copy("templates/custom-reference.docx", file.path(dir_relatorios, "custom-reference.docx"), overwrite = TRUE)
        file.copy("templates/funcoes_nonparametric.R", file.path(dir_scripts, "funcoes_nonparametric.R"), overwrite = TRUE)

        pp <- export_params()
        qmd_lines <- customize_np_qmd_params("templates/relatorio_nonparametric.qmd",
                                             pp$test_type, pp$var_y, pp$var_x, pp$var1, pp$var2,
                                             pp$var_row, pp$var_col, pp$alternative, pp$yates)
        writeLines(qmd_lines, file.path(dir_relatorios, "relatorio_nonparametric.qmd"))

        writeLines(c("Version: 1.0", "RestoreWorkspace: Default", "SaveWorkspace: Default", "Encoding: UTF-8"),
                   file.path(proj_dir, "projeto_analise.Rproj"))
        writeLines(c(
          "PACOTE DE ESTUDO: TESTES NÃO PARAMÉTRICOS (CatalyseR)",
          "- projeto_analise.Rproj: duplo clique para abrir no RStudio.",
          "- dados/     : dados limpos (.rda/.csv/.xlsx) e, no qui-quadrado, a tabela de contingência.",
          "- scripts/   : script da análise e funções de apoio.",
          "- relatorios/: relatório Quarto (.qmd) e template Word."
        ), file.path(proj_dir, "README.txt"))

        old_wd <- getwd(); setwd(temp_dir)
        zip::zip(file, files = proj_dir_name)
        setwd(old_wd)
      }
    )

    estado_execucao <- reactive({
      req(exec_ctrl$atualizada())
      res <- test_results()
      req(res)
      pp <- export_params()
      fonte <- if (identical(fixed_test, "quiquadrado")) input$chi_source %||% "vars" else "base"
      titulo <- switch(
        fixed_test,
        quiquadrado = paste("Qui-quadrado:", pp$var_row, "por", pp$var_col),
        mannwhitney = paste("Mann-Whitney:", pp$var_y, "por", pp$var_x),
        wilcoxon = paste("Wilcoxon pareado:", pp$var1, "e", pp$var2),
        kruskal = paste("Kruskal-Wallis:", pp$var_y, "por", pp$var_x)
      )
      parametros <- c(pp, list(fonte = fonte))
      if (identical(fixed_test, "quiquadrado")) parametros$tabela <- res$tab
      override <- NULL
      if (identical(fonte, "prepared")) {
        override <- list(
          base_id = "tabela_preparada", base_objeto = "tabela_contingencia",
          nome_amigavel = "Tabela de contingência preparada",
          base_tipo = "tabela_preparada", derivada = FALSE
        )
      } else if (identical(fonte, "manual")) {
        override <- list(
          base_id = "entrada_manual", base_objeto = "tabela_manual",
          nome_amigavel = "Entrada manual",
          base_tipo = "entrada_manual", derivada = FALSE
        )
      }
      list(
        analise_id = paste0("np_", nome_teste()),
        tipo = nome_teste(),
        titulo = titulo,
        parametros = parametros,
        saidas_disponiveis = c("narrativa", "tabela", "grafico", "diagnosticos", "console"),
        resultado_resumo = list(
          estatistica = unname(as.numeric(res$r$statistic)),
          graus_liberdade = unname(as.numeric(res$r$df)),
          p_valor = res$r$p,
          dimensoes_tabela = if (!is.null(res$tab)) dim(res$tab) else NULL
        ),
        codigo_r = paste(r_code_np(), collapse = "\n"),
        base_contexto_override = override
      )
    })

    invisible(list(
      resultado = test_results,
      estado_execucao = estado_execucao,
      estado_execucao_ui = exec_ctrl$estado,
      execucao_atualizada = exec_ctrl$atualizada
    ))
  })
}
