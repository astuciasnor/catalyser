# Módulo Agrupar / Sumarizar para a CatalyseR
# ---------------------------------------------------------------------------
# Reduz uma base a uma linha por grupo, calculando resumos frequentes. O módulo
# trabalha antes da Trilha global e pode promover o resultado para as análises.
# A transformação sempre gera código R reproduzível (mouse -> código).

library(shiny)
library(bslib)
library(DT)

agrupar_funcoes <- c(
  "Número de observações (n)" = "n",
  "Soma" = "soma",
  "Média" = "media",
  "Mediana" = "mediana",
  "Mínimo" = "minimo",
  "Máximo" = "maximo",
  "Desvio-padrão" = "desvio_padrao"
)

agrupar_bt <- function(x) {
  ok <- grepl("^[A-Za-z.][A-Za-z0-9._]*$", x) & !grepl("^\\.[0-9]", x)
  ifelse(ok, x, paste0("`", gsub("`", "\\\\`", x, fixed = TRUE), "`"))
}

agrupar_nome_saida <- function(variavel, funcao) {
  if (identical(funcao, "n")) return("n")
  paste0(variavel, "_", funcao)
}

agrupar_validar <- function(df, grupos, variaveis, funcoes) {
  if (!length(grupos)) return("Escolha pelo menos uma variável de agrupamento.")
  if (any(!grupos %in% names(df))) return("Uma variável de agrupamento não existe mais na base.")
  if (!length(funcoes)) return("Escolha pelo menos uma medida-resumo.")
  precisa_numerica <- any(funcoes != "n")
  if (precisa_numerica && !length(variaveis))
    return("Escolha pelo menos uma variável numérica para os resumos.")
  if (length(variaveis) && any(!variaveis %in% names(df)))
    return("Uma variável de resposta não existe mais na base.")
  nao_numericas <- variaveis[!vapply(df[variaveis], is.numeric, logical(1))]
  if (length(nao_numericas))
    return(sprintf("Estas variáveis não são numéricas: %s.", paste(nao_numericas, collapse = ", ")))
  NULL
}

agrupar_aplicar <- function(df, grupos, variaveis, funcoes) {
  msg <- agrupar_validar(df, grupos, variaveis, funcoes)
  if (!is.null(msg)) stop(msg, call. = FALSE)

  # addNA() mantém grupos cuja chave contém NA, como dplyr::group_by().
  fatores <- lapply(df[grupos], function(x) addNA(as.factor(x), ifany = TRUE))
  chaves <- do.call(interaction, c(fatores, list(drop = TRUE, lex.order = TRUE)))
  indices <- split(seq_len(nrow(df)), chaves, drop = TRUE)
  if (!length(indices)) return(df[FALSE, grupos, drop = FALSE])

  resumir_grupo <- function(idx) {
    linha <- as.list(df[idx[1], grupos, drop = FALSE])
    if ("n" %in% funcoes) linha$n <- length(idx)
    for (v in variaveis) {
      x <- df[[v]][idx]
      if ("soma" %in% funcoes) linha[[agrupar_nome_saida(v, "soma")]] <- sum(x, na.rm = TRUE)
      if ("media" %in% funcoes) linha[[agrupar_nome_saida(v, "media")]] <- mean(x, na.rm = TRUE)
      if ("mediana" %in% funcoes) linha[[agrupar_nome_saida(v, "mediana")]] <- stats::median(x, na.rm = TRUE)
      if ("minimo" %in% funcoes) linha[[agrupar_nome_saida(v, "minimo")]] <- if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
      if ("maximo" %in% funcoes) linha[[agrupar_nome_saida(v, "maximo")]] <- if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
      if ("desvio_padrao" %in% funcoes) linha[[agrupar_nome_saida(v, "desvio_padrao")]] <- stats::sd(x, na.rm = TRUE)
    }
    as.data.frame(linha, check.names = FALSE, stringsAsFactors = FALSE)
  }
  resultado <- do.call(rbind, lapply(indices, resumir_grupo))
  rownames(resultado) <- NULL
  resultado
}

agrupar_exprs_codigo <- function(variaveis, funcoes) {
  exprs <- character(0)
  if ("n" %in% funcoes) exprs <- c(exprs, "n = dplyr::n()")
  mapa <- c(soma = "sum", media = "mean", mediana = "median",
            minimo = "min", maximo = "max", desvio_padrao = "sd")
  for (v in variaveis) {
    for (f in intersect(names(mapa), funcoes)) {
      nome <- agrupar_bt(agrupar_nome_saida(v, f))
      coluna <- agrupar_bt(v)
      expressao <- if (f %in% c("minimo", "maximo")) {
        sprintf("if (all(is.na(%s))) NA_real_ else %s(%s, na.rm = TRUE)",
                coluna, mapa[[f]], coluna)
      } else {
        sprintf("%s(%s, na.rm = TRUE)", mapa[[f]], coluna)
      }
      exprs <- c(exprs, sprintf("%s = %s", nome, expressao))
    }
  }
  exprs
}

agrupar_gerar_codigo <- function(grupos, variaveis, funcoes, info = NULL) {
  if (!is.null(info) && identical(info$source, "package")) {
    leitura <- c("library(EAPADados)", sprintf("data(%s)", info$package_dataset),
                 sprintf("dados <- %s", info$package_dataset))
  } else {
    arquivo <- if (!is.null(info)) info$file_name else "SEU_ARQUIVO.xlsx"
    if (grepl("\\.csv$", arquivo, ignore.case = TRUE)) {
      sep <- if (!is.null(info$csv_sep)) info$csv_sep else ","
      dec <- if (!is.null(info$csv_dec)) info$csv_dec else "."
      leitura <- sprintf('dados <- read.csv("%s", sep = "%s", dec = "%s", check.names = FALSE)',
                         arquivo, sep, dec)
    } else {
      aba <- if (!is.null(info)) info$excel_sheet else "SUA_ABA"
      leitura <- c("library(readxl)", sprintf('dados <- read_excel("%s", sheet = "%s")', arquivo, aba))
    }
  }
  exprs <- agrupar_exprs_codigo(variaveis, funcoes)
  # Todas as expressões recebem vírgula porque `.groups` vem em seguida.
  linhas_resumo <- paste0("    ", exprs, ",")
  paste(c(
    "# Script gerado pela CatalyseR — Agrupar / Sumarizar",
    "library(dplyr)", leitura, "",
    "dados_agrupados <- dados |>",
    sprintf("  group_by(%s) |>", paste(agrupar_bt(grupos), collapse = ", ")),
    "  summarise(", linhas_resumo, "    .groups = \"drop\"", "  )", "",
    "print(dados_agrupados)"
  ), collapse = "\n")
}

mod_agrupar_sumarizar_ui <- function(id) {
  ns <- NS(id)
  layout_columns(
    col_widths = c(1, 1, 1),
    style = "grid-template-columns: 3fr 6.5fr 2.5fr !important;",
    div(
      card(
        card_header("Agrupar observações"),
        card_body(
          selectizeInput(ns("grupos"), "Agrupar por:", choices = NULL, multiple = TRUE,
                         options = list(plugins = list("remove_button"))),
          helpText("Ex.: ano, espécie, sexo ou local de desembarque."),
          selectizeInput(ns("variaveis"), "Variáveis numéricas a resumir:", choices = NULL,
                         multiple = TRUE, options = list(plugins = list("remove_button"))),
          checkboxGroupInput(ns("funcoes"), "Medidas-resumo:", choices = agrupar_funcoes,
                             selected = c("n", "media")),
          checkboxInput(ns("ordenar"), "Ordenar pelas variáveis de agrupamento", TRUE),
          actionButton(ns("aplicar"), "Gerar tabela agrupada", icon = icon("layer-group"),
                       class = "btn-primary w-100")
        )
      ),
      card(
        card_header("Quando usar"),
        card_body(
          tags$p("Use quando várias observações precisam virar uma linha por grupo."),
          tags$p(HTML("Exemplo: produção total e média por <b>ano</b> e <b>espécie</b>.")),
          tags$p(style = "margin-bottom:0;", strong("Atenção: "),
                 "a tabela resultante tem menos linhas e representa outra unidade de análise.")
        )
      )
    ),
    navset_card_tab(
      nav_panel("Resultado", icon = icon("table"), DTOutput(ns("resultado"))),
      nav_panel("Base de entrada", icon = icon("table-list"), DTOutput(ns("entrada"))),
      nav_panel("Script gerado", icon = icon("code"),
                tags$pre(style = "white-space:pre-wrap; padding:12px;",
                         verbatimTextOutput(ns("codigo"))))
    ),
    div(
      card(
        card_header("Resultado"),
        card_body(
          uiOutput(ns("status")),
          downloadButton(ns("baixar_script"), "Baixar script .R",
                         class = "btn-outline-secondary btn-sm w-100 mb-2"),
          downloadButton(ns("baixar_dados"), "Baixar dados (.csv)",
                         class = "btn-outline-primary btn-sm w-100 mb-2"),
          actionButton(ns("usar_analises"), "Usar este resultado nas análises",
                       icon = icon("share-from-square"), class = "btn-primary w-100"),
          helpText("O resultado passa a ser a base sobre a qual a Trilha de Preparo é aplicada.")
        )
      )
    )
  )
}

mod_agrupar_sumarizar_server <- function(id, data_rv, import_info, on_usar = NULL) {
  moduleServer(id, function(input, output, session) {
    base_data <- reactive({ req(data_rv()); as.data.frame(data_rv()) })
    resultado_rv <- reactiveVal(NULL)
    codigo_rv <- reactiveVal("# Configure o agrupamento e clique em 'Gerar tabela agrupada'.")

    observeEvent(base_data(), {
      df <- base_data()
      resultado_rv(NULL)
      updateSelectizeInput(session, "grupos", choices = names(df), selected = character(0), server = TRUE)
      numericas <- names(df)[vapply(df, is.numeric, logical(1))]
      updateSelectizeInput(session, "variaveis", choices = numericas, selected = character(0), server = TRUE)
    })

    observeEvent(input$aplicar, {
      df <- base_data()
      grupos <- input$grupos %||% character(0)
      variaveis <- input$variaveis %||% character(0)
      funcoes <- input$funcoes %||% character(0)
      msg <- agrupar_validar(df, grupos, variaveis, funcoes)
      if (!is.null(msg)) {
        showNotification(msg, type = "error", duration = 8)
        return()
      }
      novo <- tryCatch(agrupar_aplicar(df, grupos, variaveis, funcoes), error = function(e) e)
      if (inherits(novo, "error")) {
        showNotification(conditionMessage(novo), type = "error", duration = 10)
        return()
      }
      if (isTRUE(input$ordenar)) novo <- novo[do.call(order, novo[grupos]), , drop = FALSE]
      resultado_rv(novo)
      codigo_rv(agrupar_gerar_codigo(grupos, variaveis, funcoes, import_info()))
      showNotification(sprintf("Tabela criada: %d grupos e %d colunas.", nrow(novo), ncol(novo)),
                       type = "message", duration = 5)
    })

    output$entrada <- renderDT({
      datatable(head(base_data(), 200), options = list(scrollX = TRUE, pageLength = 12), rownames = FALSE)
    })
    output$resultado <- renderDT({
      validate(need(!is.null(resultado_rv()), "Configure e gere uma tabela agrupada."))
      datatable(resultado_rv(), options = list(scrollX = TRUE, pageLength = 15), rownames = FALSE)
    })
    output$codigo <- renderText(codigo_rv())
    output$status <- renderUI({
      if (is.null(resultado_rv())) return(div(style = "color:#777;", "Nenhuma tabela gerada."))
      r <- resultado_rv()
      tagList(strong("Tabela pronta"), br(), sprintf("%d linhas × %d colunas", nrow(r), ncol(r)))
    })

    observeEvent(input$usar_analises, {
      req(resultado_rv())
      if (is.function(on_usar))
        on_usar(resultado_rv(), "Agrupar / Sumarizar", codigo_rv())
    })

    output$baixar_script <- downloadHandler(
      filename = function() "agrupar_sumarizar.R",
      content = function(file) writeLines(enc2utf8(codigo_rv()), file, useBytes = TRUE)
    )
    output$baixar_dados <- downloadHandler(
      filename = function() "dados_agrupados.csv",
      content = function(file) {
        req(resultado_rv())
        utils::write.csv(resultado_rv(), file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )

    invisible(reactive(list(df = resultado_rv(), codigo = codigo_rv())))
  })
}
