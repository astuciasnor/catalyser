# =============================================================================
# Módulo: Tabela de Distribuição de Frequência (Descrevendo Dados)
# Dados discretos e contínuos, com técnicas modernas de classes (Sturges/Scott/
# Freedman-Diaconis) ou nº/amplitude manual, tabela flextable no tema Ocean e
# polígono de frequência. Fonte canônica em templates/funcoes_frequencia.R.
# =============================================================================

source("templates/funcoes_frequencia.R", encoding = "UTF-8")

mod_frequencia_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(1, 1, 1),
      style = "grid-template-columns: 2.8fr 6.4fr 2.8fr !important;",

      # COLUNA 1: CONFIGURAÇÃO
      div(
        card(
          card_header("1. Variável"),
          card_body(
            style = "padding: 12px 15px;",
            selectInput(ns("var"), "Variável:", choices = NULL),
            radioButtons(ns("tipo"), "Tipo de dado:",
              choices = c("Detectar automaticamente" = "auto",
                          "Discreto (valores/categorias)" = "discreto",
                          "Contínuo (classes)" = "continuo"),
              selected = "auto"),
            helpText("No automático: numérico com poucos valores inteiros → discreto; caso contrário → contínuo.")
          )
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] != 'discreto'", ns("tipo")),
          card(
            card_header("2. Classes (contínuo)"),
            card_body(
              style = "padding: 12px 15px;",
              selectInput(ns("metodo"), "Como definir as classes:",
                choices = c("Sturges (padrão)"          = "sturges",
                            "Scott"                       = "scott",
                            "Freedman-Diaconis"           = "fd",
                            "Raiz de n"                   = "raiz",
                            "Número de classes (manual)"  = "manual_n",
                            "Amplitude de classe (manual)"= "manual_amp")),
              conditionalPanel(
                condition = sprintf("input['%s'] == 'manual_n'", ns("metodo")),
                numericInput(ns("n_classes"), "Número de classes:", value = 6, min = 2, max = 40, step = 1)),
              conditionalPanel(
                condition = sprintf("input['%s'] == 'manual_amp'", ns("metodo")),
                numericInput(ns("amplitude"), "Amplitude de cada classe:", value = 1, min = 0.0001, step = 0.5)),
              numericInput(ns("digitos"), "Casas decimais dos limites:", value = 2, min = 0, max = 6, step = 1)
            )
          )
        ),
        card(
          card_header("Exportar"),
          card_body(
            style = "padding: 12px 15px;",
            downloadButton(ns("baixar_script"), "Baixar script .R", class = "btn-outline-secondary btn-sm w-100")
          )
        )
      ),

      # COLUNA 2: RESULTADOS
      navset_card_tab(
        title = "Distribuição de Frequência",
        nav_panel(
          title = "Tabela", icon = icon("table"),
          card_body(style = "padding: 12px 15px;",
                    uiOutput(ns("tabela")))
        ),
        nav_panel(
          title = "Gráfico", icon = icon("chart-column"),
          card_body(style = "padding: 12px 15px;",
                    plotOutput(ns("grafico"), height = "420px"))
        ),
        nav_panel(
          title = "Script gerado", icon = icon("code"),
          card_body(style = "padding: 10px 15px;",
            tags$pre(style = "white-space: pre-wrap; font-size: 0.82rem;",
                     verbatimTextOutput(ns("script"))))
        )
      ),

      # COLUNA 3: RELATO
      card(
        card_header("Relato"),
        card_body(
          style = "padding: 12px 15px; font-size: 0.9rem; line-height: 1.5;",
          uiOutput(ns("relato"))
        )
      )
    )
  )
}

mod_frequencia_server <- function(id, data_rv, import_info) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Popular o seletor de variáveis (todas as colunas)
    observeEvent(data_rv(), {
      df <- data_rv(); req(df)
      updateSelectInput(session, "var", choices = names(df),
                        selected = names(df)[1])
    })

    # Cálculo canônico (reativo, com tratamento de erro)
    freq_res <- reactive({
      df <- data_rv(); req(df, input$var)
      req(input$var %in% names(df))
      x <- df[[input$var]]
      met <- input$metodo %||% "sturges"
      nc  <- if (identical(met, "manual_n"))   input$n_classes else NULL
      amp <- if (identical(met, "manual_amp")) input$amplitude else NULL
      mc  <- if (met %in% c("sturges", "scott", "fd", "raiz")) met else "sturges"
      tryCatch(
        calcular_freq(x, tipo = input$tipo %||% "auto", metodo_classes = mc,
                      n_classes = nc, amplitude = amp,
                      digitos = input$digitos %||% 2, nome = input$var),
        error = function(e) structure(list(erro = conditionMessage(e)), class = "freq_erro"))
    })

    # Tabela (flextable Ocean renderizada como HTML no Viewer da IDE)
    output$tabela <- renderUI({
      r <- freq_res(); req(r)
      if (inherits(r, "freq_erro"))
        return(div(class = "alert alert-warning", r$erro))
      flextable::htmltools_value(flextable_freq_ocean(r))
    })

    # Gráfico (histograma + polígono, ou barras)
    output$grafico <- renderPlot({
      r <- freq_res(); req(r)
      validate(need(!inherits(r, "freq_erro"), r$erro))
      g <- grafico_freq(r)
      validate(need(!is.null(g), "Instale o ggplot2 para ver o gráfico."))
      g
    })

    # Relato
    output$relato <- renderUI({
      r <- freq_res(); req(r)
      if (inherits(r, "freq_erro"))
        return(div(class = "alert alert-warning", r$erro))
      tags$p(relatar_freq(r))
    })

    # Script gerado (reproduz a análise no RStudio)
    codigo_freq <- reactive({
      r <- freq_res(); req(r); if (inherits(r, "freq_erro")) return("# Configure a variável.")
      info <- if (is.function(import_info)) import_info() else NULL
      if (!is.null(info) && identical(info$source, "package")) {
        leitura <- c("library(EAPADados)", sprintf("data(%s)", info$package_dataset),
                     sprintf("dados <- %s", info$package_dataset))
      } else {
        fn  <- if (!is.null(info)) info$file_name   else "SEU_ARQUIVO.xlsx"
        abn <- if (!is.null(info)) info$excel_sheet else "SUA_ABA"
        leitura <- sprintf('dados <- readxl::read_excel("%s", sheet = "%s")', fn, abn)
      }
      met <- input$metodo %||% "sturges"
      arg_classe <- if (identical(met, "manual_n")) sprintf(", n_classes = %s", input$n_classes)
                    else if (identical(met, "manual_amp")) sprintf(", amplitude = %s", input$amplitude)
                    else sprintf(', metodo_classes = "%s"', met)
      paste(c(
        "# Script gerado pela CatalyseR — Tabela de Distribuicao de Frequencia",
        "source('scripts/funcoes_frequencia.R')",
        leitura,
        "",
        sprintf('r <- calcular_freq(dados[["%s"]], tipo = "%s"%s, digitos = %s, nome = "%s")',
                input$var, input$tipo %||% "auto", arg_classe, input$digitos %||% 2, input$var),
        "print(mostrar_freq(r))       # tabela (com linha de Total)",
        "flextable_freq_ocean(r)      # tabela no tema Ocean",
        "grafico_freq(r)              # histograma + poligono (ou barras)",
        "cat(relatar_freq(r))         # relato em portugues"
      ), collapse = "\n")
    })
    output$script <- renderText(codigo_freq())

    output$baixar_script <- downloadHandler(
      filename = function() paste0("tabela_frequencia_", Sys.Date(), ".R"),
      content  = function(file) writeLines(codigo_freq(), file)
    )
  })
}

# Operador auxiliar (caso não exista no escopo do app)
if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a
