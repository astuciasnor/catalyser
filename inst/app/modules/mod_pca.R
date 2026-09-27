# Módulo de Análise de Componentes Principais (PCA / ACP) para IDE_R
# -----------------------------------------------------------------------------
# Adaptado do roteiro didático da curadoria (Multivariada_02_PCA): PCA
# padronizada com variáveis suplementares (FactoMineR), retenção por três
# critérios com a permutação em destaque (teste próprio em R base), círculo de
# correlações por cos², contribuições, mapa de indivíduos, biplot com elipses,
# cargas com limite de permutação e comparação com imputação (missMDA).
library(shiny)
library(bslib)
library(ggplot2)
library(DT)

if (file.exists("templates/funcoes_pca.R")) {
  source("templates/funcoes_pca.R")
}

# Helper para customizar parâmetros do relatório Quarto de PCA
customize_pca_qmd_params <- function(qmd_path, vars_selected, scale,
                                     quanti_sup = "", quali_sup = "",
                                     seed = 2026, comparar_imputacao = FALSE) {
  lines <- readLines(qmd_path, warn = FALSE)
  lines <- gsub('vars_selected: ".*"', sprintf('vars_selected: "%s"', vars_selected), lines)
  lines <- gsub('quanti_sup: ".*"', sprintf('quanti_sup: "%s"', quanti_sup), lines)
  lines <- gsub('quali_sup: ".*"', sprintf('quali_sup: "%s"', quali_sup), lines)
  lines <- gsub('seed: .*', sprintf('seed: %s', as.integer(seed)), lines)
  lines <- gsub('comparar_imputacao: .*', sprintf('comparar_imputacao: %s', tolower(as.character(isTRUE(comparar_imputacao)))), lines)
  lines <- gsub('scale: .*', sprintf('scale: %s', tolower(as.character(isTRUE(scale)))), lines)
  return(lines)
}

# Opções de envoltória/elipse para indivíduos e biplot (guia da curadoria).
opcoes_elipse_pca <- c(
  "Elipse de concentração (normal, 95 %)" = "norm",
  "Elipse robusta (t)" = "t",
  "Elipse de confiança" = "confidence",
  "Envoltória convexa" = "convex"
)

mod_pca_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Cabeçalho do painel em duas linhas: o título fica sozinho na primeira e
    # as oito abas, compactas e com quebra de linha, na segunda.
    tags$style(HTML("
      .pca-painel .bslib-navs-card-title {
        flex-direction: column !important;
        align-items: stretch !important;
        gap: 2px !important;
        padding: 8px 12px 4px !important;
      }
      .pca-painel .bslib-navs-card-title > span {
        font-family: 'Outfit', sans-serif;
        font-weight: 700;
        font-size: 1rem;
        color: #0F3B5F;
      }
      .pca-painel .bslib-navs-card-title > .nav {
        flex-wrap: wrap;
        row-gap: 2px;
        margin: 0;
      }
      .pca-painel .bslib-navs-card-title > .nav .nav-link {
        padding: 3px 8px !important;
        font-size: 0.8rem !important;
      }
      .pca-painel .bslib-navs-card-title > .nav .nav-link i {
        margin-right: 4px;
      }
    ")),
    layout_columns(
      col_widths = c(1, 1, 1),
      style = "grid-template-columns: 2.5fr 7fr 2.5fr !important;",

      # COLUNA 1: CONFIGURAÇÃO DO MODELO
      div(
        card(
          card_header("Configuração da PCA"),
          card_body(
            style = "padding: 12px 15px;",
            helpText(HTML("As <b>ativas</b> formam os eixos da PCA. As <b>suplementares</b> são projetadas depois, só para interpretação, sem influenciar os eixos. Uma variável marcada em um campo sai do outro.")),
            layout_columns(
              col_widths = c(6, 6),
              style = "gap: 10px;",
              checkboxGroupInput(ns("vars_selected"), "Variáveis ativas:", choices = NULL),
              checkboxGroupInput(ns("quanti_sup"), "Suplementares quantitativas:", choices = NULL)
            ),
            actionButton(ns("sel_all_num"), "Selecionar todas as numéricas como ativas",
                         icon = icon("list-check"), class = "btn-outline-secondary btn-sm w-100 mb-2"),
            selectInput(ns("quali_sup"), "Variável de grupo (opcional):", choices = NULL),
            checkboxInput(ns("scale"), "Padronizar variáveis (PCA de correlação)", value = TRUE),
            numericInput(ns("seed"), "Semente da permutação:", value = 2026,
                         min = 1, max = 999999, step = 1),
            checkboxInput(ns("comparar_imputacao"), "Comparar com imputação (missMDA) se houver NA", value = FALSE),
            helpText(HTML("Em bases do EAPADados com papéis documentados (ex.: riqueza e trecho do rio Doubs), o painel abre com a configuração sugerida. A permutação (999 repetições) testa a significância dos eixos e das cargas.")),
            execucao_explicita_controles_ui(ns)
          )
        ),
        card(
          card_header("Relatório e Pacote de Estudo"),
          card_body(
            style = "padding: 12px 15px;",
            execucao_explicita_downloads_ui(ns, tagList(
              downloadButton(ns("download_report_docx"), "Baixar Relatório Word (.docx)", class = "btn-success w-100"),
              div(style = "margin-top: 8px;"),
              downloadButton(ns("download_project_zip"), "Exportar Projeto R (.zip)", class = "btn-primary w-100"),
              helpText("Gera relatórios de PCA e pacotes para compilação local.", style = "margin-top: 10px; font-size: 0.85rem;")
            ))
          )
        )
      ),

      # COLUNA 2: ABAS DE RESULTADOS (PRINCIPAL)
      execucao_explicita_resultados_ui(ns, div(
        class = "pca-painel",
        navset_card_tab(
          id = ns("active_tab"),
          title = "Painel de Resultados da PCA",
        nav_panel(
          title = "Correlações",
          icon = icon("table-cells"),
          card_body(
            uiOutput(ns("correlacoes_ui"))
          )
        ),
        nav_panel(
          title = "Autovalores",
          icon = icon("chart-pie"),
          card_body(
            uiOutput(ns("autovalores_ui"))
          )
        ),
        nav_panel(
          title = "Círculo",
          icon = icon("circle-dot"),
          card_body(
            plotOutput(ns("grafico_circulo"), height = "380px"),
            helpText(HTML("A cor de cada seta é a qualidade de representação (cos²). Uma seta <b>curta não é irrelevante</b>: indica que a variável varia em outras direções — veja o cos² antes de descartar."))
          )
        ),
        nav_panel(
          title = "Contribuições",
          icon = icon("chart-bar"),
          card_body(
            plotOutput(ns("grafico_contribuicoes"), height = "360px"),
            helpText(HTML("A linha tracejada vermelha marca a contribuição esperada se todas as variáveis contribuíssem igualmente (100/p %). Variáveis acima dela são as que mais constroem cada eixo."))
          )
        ),
        nav_panel(
          title = "Indivíduos",
          icon = icon("users"),
          card_body(
            uiOutput(ns("individuos_ui"))
          )
        ),
        nav_panel(
          title = "Biplot",
          icon = icon("diagram-project"),
          card_body(
            uiOutput(ns("biplot_ui"))
          )
        ),
        nav_panel(
          title = "Cargas",
          icon = icon("list-check"),
          card_body(
            uiOutput(ns("cargas_ui"))
          )
        ),
        nav_panel(
          title = "Imputação",
          icon = icon("droplet"),
          card_body(
            uiOutput(ns("imputacao_ui"))
          )
        )
        )
      )),

      # COLUNA 3: CONFIGURAÇÕES DE EXIBIÇÃO
      card(
        card_header("Configurações de Exibição"),
        card_body(
          conditionalPanel(
            condition = sprintf("input['%s'] == 'Indivíduos'", ns("active_tab")),
            selectInput(ns("ellipse_ind"), "Tipo de envoltória:",
                        choices = opcoes_elipse_pca, selected = "convex"),
            helpText("Elipses de concentração (norm) e de confiança exigem ao menos 3 unidades por grupo; a envoltória convexa ajusta qualquer grupo.")
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'Biplot'", ns("active_tab")),
            selectInput(ns("versao_biplot"), "Versão do biplot:",
                        choices = c("Clássica (setas cinza)" = "classica",
                                    "Avançada (setas pela contribuição)" = "contribuicao")),
            selectInput(ns("ellipse_biplot"), "Tipo de elipse:",
                        choices = opcoes_elipse_pca, selected = "norm")
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] != 'Indivíduos' && input['%s'] != 'Biplot'",
                                ns("active_tab"), ns("active_tab")),
            helpText("As tabelas de autovalores, cargas e descrições são calculadas de forma exata e não dependem de configurações gráficas.")
          )
        )
      )
    )
  )
}

mod_pca_server <- function(id, data_rv, import_info) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    revisao_execucao <- execucao_revisao_dados(data_rv)
    gatilho_execucao <- reactiveVal(0L)

    # Bases do EAPADados com papéis documentados na curadoria: ao carregar a
    # base, abrimos com a configuração registrada (o usuário pode mudar).
    # Não usamos critério automático (ex.: valores inteiros) para sugerir
    # suplementares: só vale o que a curadoria documentou.
    papeis_sugeridos_pca <- function(info) {
      if (is.null(info) || !identical(info$source, "package")) return(NULL)
      list(
        doubs_ambiente = list(quanti_sup = "riqueza", quali_sup = "trecho")
      )[[info$package_dataset %||% ""]]
    }

    # Guarda a sugestão de papéis da base atual para o observador do grupo.
    papeis_base_rv <- reactiveVal(NULL)

    # Quando a base muda, atualiza os dois campos de variáveis (ambos listam
    # todas as numéricas) e aplica a sugestão de papéis, quando houver.
    observeEvent(data_rv(), {
      df <- data_rv()
      req(df)
      numericas <- names(df)[vapply(df, is.numeric, logical(1))]
      numericas <- setdiff(numericas, c("id", "ID"))
      papeis <- papeis_sugeridos_pca(import_info())
      if (!is.null(papeis)) {
        sup_q <- intersect(papeis$quanti_sup, numericas)
        ativas <- setdiff(numericas, sup_q)
        if (length(ativas) < 2) ativas <- head(numericas, min(3, length(numericas)))
      } else {
        ativas <- intersect(isolate(input$vars_selected) %||% character(), numericas)
        if (length(ativas) < 2) ativas <- head(numericas, min(3, length(numericas)))
        sup_q <- setdiff(intersect(isolate(input$quanti_sup) %||% character(), numericas), ativas)
      }
      updateCheckboxGroupInput(session, "vars_selected", choices = numericas, selected = ativas)
      updateCheckboxGroupInput(session, "quanti_sup", choices = numericas, selected = sup_q)
      papeis_base_rv(papeis)
    }, ignoreInit = FALSE, priority = 500)

    # Botão: selecionar todas as numéricas como ativas, respeitando as
    # suplementares já escolhidas (elas permanecem suplementares).
    observeEvent(input$sel_all_num, {
      df <- data_rv()
      req(df)
      numericas <- names(df)[vapply(df, is.numeric, logical(1))]
      numericas <- setdiff(numericas, c("id", "ID"))
      sup <- isolate(input$quanti_sup) %||% character()
      updateCheckboxGroupInput(session, "vars_selected", selected = setdiff(numericas, sup))
    })

    # Exclusão mútua: uma variável não fica nos dois campos ao mesmo tempo.
    # Quem entra nas ativas sai das suplementares...
    observeEvent(input$vars_selected, {
      sup <- isolate(input$quanti_sup) %||% character()
      conflito <- intersect(input$vars_selected, sup)
      if (length(conflito))
        updateCheckboxGroupInput(session, "quanti_sup", selected = setdiff(sup, conflito))
    }, ignoreInit = TRUE)
    # ...e quem entra nas suplementares sai das ativas.
    observeEvent(input$quanti_sup, {
      ativas <- isolate(input$vars_selected) %||% character()
      conflito <- intersect(input$quanti_sup, ativas)
      if (length(conflito))
        updateCheckboxGroupInput(session, "vars_selected", selected = setdiff(ativas, conflito))
    }, ignoreInit = TRUE)

    # Atualiza a variável de grupo: colunas categóricas da base; se a curadoria
    # sugerir um grupo para a base carregada, a sugestão vence uma vez.
    observe({
      df <- data_rv()
      req(df)
      categorias <- names(df)[vapply(df, function(x) is.factor(x) || is.character(x), logical(1))]
      categorias <- setdiff(categorias, c("id", "ID"))
      papeis <- papeis_base_rv()
      sugerida <- if (!is.null(papeis)) papeis$quali_sup %||% "" else ""
      atuais <- isolate(input$quali_sup) %||% ""
      escolhida <- if (nzchar(sugerida) && sugerida %in% categorias) sugerida
        else if (atuais %in% categorias) atuais
        else ""
      updateSelectInput(session, "quali_sup",
                        choices = c("(nenhuma)" = "", stats::setNames(categorias, categorias)),
                        selected = escolhida)
    })

    assinatura_execucao <- reactive({
      req(length(input$vars_selected) >= 2)
      execucao_assinatura(
        input,
        c("vars_selected", "scale", "quanti_sup", "quali_sup", "seed",
          "comparar_imputacao"),
        revisao_execucao()
      )
    })

    # Executa a PCA apenas após confirmação explícita.
    result_rv <- eventReactive(gatilho_execucao(), {
      df <- data_rv()
      req(df, length(input$vars_selected) >= 2)
      calcular_pca(
        df = df,
        vars_selected = input$vars_selected,
        scale = isTRUE(input$scale),
        quanti_sup = input$quanti_sup %||% character(0),
        quali_sup = input$quali_sup %||% "",
        seed = input$seed,
        comparar_imputacao = isTRUE(input$comparar_imputacao)
      )
    }, ignoreInit = FALSE)

    exec_ctrl <- execucao_explicita_server(
      input, output, session, assinatura_execucao, result_rv,
      nome_analise = "A PCA",
      gatilho_rv = gatilho_execucao
    )

    # Avisos do motor (variáveis constantes, exclusões, permutação indisponível...).
    alerta_avisos <- function(r) {
      avisos <- r$avisos
      if (!length(avisos)) return(NULL)
      tagList(lapply(avisos, function(a) {
        div(class = "alert alert-warning py-2 mb-2", style = "font-size: 0.85rem;",
            icon("triangle-exclamation"), " ", a)
      }))
    }

    # Aviso automático quando algum grupo tem menos de três unidades.
    alerta_grupos_pequenos <- function(r) {
      gp <- r$grupos_pequenos
      if (is.null(gp) || !length(gp)) return(NULL)
      frases <- vapply(seq_along(gp), function(i) {
        sprintf("O grupo '%s' tem apenas %d unidade(s): elipses de concentração e de confiança não podem ser estimadas com menos de três pontos.",
                names(gp)[i], gp[[i]])
      }, character(1))
      div(class = "alert alert-warning py-2 mb-2", style = "font-size: 0.85rem;",
          icon("triangle-exclamation"), " ", paste(frases, collapse = " "))
    }

    # ---- Aba 1: correlações antes da PCA ------------------------------------
    output$correlacoes_ui <- renderUI({
      r <- result_rv()
      req(r)
      tagList(
        alerta_avisos(r),
        plotOutput(ns("grafico_correlacoes"), height = "420px"),
        helpText(HTML("Matriz de correlações de Pearson, ordenada por agrupamento hierárquico e com o triângulo inferior. É o diagnóstico pré-PCA: variáveis muito correlacionadas antecipam eixos com alta variância explicada."))
      )
    })

    output$grafico_correlacoes <- renderPlot({
      r <- result_rv()
      req(r)
      g <- grafico_pca_correlacoes(r)
      validate(need(!is.null(g), "A matriz de correlações não pôde ser desenhada (variável constante?)."))
      g
    })

    # ---- Aba 2: autovalores e retenção --------------------------------------
    output$autovalores_ui <- renderUI({
      r <- result_rv()
      req(r)
      tagList(
        alerta_avisos(r),
        layout_columns(
          col_widths = c(6, 6),
          card_body(
            h6("Gráfico de retenção", style = "font-family: 'Outfit'; font-weight: 700; color: #0F3B5F;"),
            plotOutput(ns("grafico_retencao"), height = "300px")
          ),
          card_body(
            h6("Autovalores e variância explicada", style = "font-family: 'Outfit'; font-weight: 700; color: #0F3B5F;"),
            tableOutput(ns("tabela_variancia")),
            h6("Conferência FactoMineR × prcomp", style = "font-family: 'Outfit'; font-weight: 700; color: #0F3B5F; margin-top: 10px;"),
            tableOutput(ns("tabela_conferencia"))
          )
        ),
        h6("Retenção: três critérios juntos", style = "font-family: 'Outfit'; font-weight: 700; color: #0F3B5F; margin-top: 12px;"),
        tableOutput(ns("tabela_retencao")),
        helpText(HTML("A <b>permutação</b> (999 repetições, semente fixa) é o critério em destaque: retém-se o eixo cujo p &lt; 0,05. O bastão quebrado é um critério de referência e o <b>Kaiser (autovalor = 1) é apenas referência</b> — sozinho, ele costuma reter eixos demais."))
      )
    })

    output$tabela_variancia <- renderTable({
      r <- result_rv()
      req(r)
      mostrar_pca_var(r)
    }, striped = TRUE, hover = TRUE, bordered = TRUE)

    output$tabela_conferencia <- renderTable({
      r <- result_rv()
      req(r)
      mostrar_pca_conferencia(r)
    }, striped = TRUE, hover = TRUE, bordered = TRUE)

    output$tabela_retencao <- renderTable({
      r <- result_rv()
      req(r)
      mostrar_pca_retencao(r)
    }, striped = TRUE, hover = TRUE, bordered = TRUE)

    output$grafico_retencao <- renderPlot({
      r <- result_rv()
      req(r)
      grafico_pca_retencao(r)
    })

    # ---- Aba 3: círculo de correlações --------------------------------------
    output$grafico_circulo <- renderPlot({
      r <- result_rv()
      req(r)
      grafico_pca_circulo(r)
    })

    # ---- Aba 4: contribuições -----------------------------------------------
    output$grafico_contribuicoes <- renderPlot({
      r <- result_rv()
      req(r)
      grafico_pca_contribuicoes(r)
    })

    # ---- Aba 5: mapa dos indivíduos -----------------------------------------
    output$individuos_ui <- renderUI({
      r <- result_rv()
      req(r)
      tagList(
        alerta_grupos_pequenos(r),
        plotOutput(ns("grafico_individuos"), height = "380px"),
        helpText(HTML("Quando a variável de grupo tem os níveis do guia de HCA (trechos do rio), as cores são <b>as mesmas do dendrograma</b> — as leituras dos dois menus se correspondem."))
      )
    })

    output$grafico_individuos <- renderPlot({
      r <- result_rv()
      req(r)
      grafico_pca_individuos(r, ellipse_type = input$ellipse_ind %||% "convex")
    })

    # ---- Aba 6: biplot ------------------------------------------------------
    output$biplot_ui <- renderUI({
      r <- result_rv()
      req(r)
      tagList(
        alerta_grupos_pequenos(r),
        plotOutput(ns("grafico_biplot"), height = "380px"),
        helpText(HTML("Leia o biplot pela <b>direção das setas</b>: projete cada ponto sobre a reta da variável de interesse, não pela proximidade direta entre ponto e seta."))
      )
    })

    output$grafico_biplot <- renderPlot({
      r <- result_rv()
      req(r)
      grafico_pca_biplot(r, versao = input$versao_biplot %||% "classica",
                         ellipse_type = input$ellipse_biplot %||% "norm")
    })

    # ---- Aba 7: cargas com limite de permutação e dimdesc -------------------
    output$cargas_ui <- renderUI({
      r <- result_rv()
      req(r)
      if (is.null(r$cargas)) {
        tagList(
          div(class = "alert alert-info py-2 mb-2", style = "font-size: 0.85rem;",
              icon("circle-info"),
              " A permutação não pôde ser calculada para esta base: as cargas são exibidas como correlações com os eixos, sem teste de significância."),
          tableOutput(ns("tabela_cargas"))
        )
      } else {
        tagList(
          plotOutput(ns("grafico_cargas"), height = "400px"),
          helpText(HTML("O <b>índice de carga</b> mede quanto cada variável contribui para o eixo: é a correlação da variável com o eixo elevada ao quadrado e ponderada pelo autovalor. Por ser ao quadrado, é sempre positivo — o <b>sinal do eixo é arbitrário</b>, então o que importa é a magnitude. O <b>limite permutado (97,5 %)</b> vem de 999 PCA sobre dados com as colunas embaralhadas: só 2,5 % das cargas do acaso o ultrapassam. Uma variável acima do limite contribui mais do que se esperaria pelo acaso.")),
          tableOutput(ns("tabela_cargas")),
          hr(),
          h6("Descrição automática dos eixos (dimdesc, p < 0,05)", style = "font-family: 'Outfit'; font-weight: 700; color: #0F3B5F;"),
          uiOutput(ns("tabela_dimdesc"))
        )
      }
    })

    output$grafico_cargas <- renderPlot({
      r <- result_rv()
      req(r)
      g <- grafico_pca_cargas(r)
      validate(need(!is.null(g), "Sem permutação não há limite nulo para desenhar."))
      g
    })

    output$tabela_cargas <- renderTable({
      r <- result_rv()
      req(r)
      mostrar_pca_cargas(r)
    }, striped = TRUE, hover = TRUE, bordered = TRUE)

    output$tabela_dimdesc <- renderUI({
      r <- result_rv()
      req(r)
      tab <- mostrar_pca_dimdesc(r)
      if (is.null(tab)) {
        return(helpText("Nenhuma variável ou grupo foi significativo (p < 0,05) na descrição automática dos eixos."))
      }
      tableOutput(ns("tabela_dimdesc_inner"))
    })

    output$tabela_dimdesc_inner <- renderTable({
      r <- result_rv()
      req(r)
      mostrar_pca_dimdesc(r)
    }, striped = TRUE, hover = TRUE, bordered = TRUE)

    # ---- Aba 8: comparação com imputação (missMDA) --------------------------
    output$imputacao_ui <- renderUI({
      r <- result_rv()
      req(r)
      if (!is.null(r$imputacao)) {
        tagList(
          plotOutput(ns("grafico_imputacao"), height = "360px"),
          div(class = "alert alert-light",
              style = "border-left: 4px solid #2E7D8F; background-color: #f8f9fa; color: #333333; font-size: 0.9rem; line-height: 1.5; padding: 12px 15px;",
              sprintf("Foram imputadas %d célula(s) com %d eixo(s) (missMDA). Concordância do eixo 1 com os dados completos: r = %s. A imputação não cria informação nova: serve para aproveitar observações incompletas e estabilizar a projeção.",
                      r$imputacao$n_faltantes, r$imputacao$n_eixos,
                      fmt_pca(r$imputacao$concordancia)))
        )
      }
      motivo <- r$imputacao_mensagem %||%
        "A comparação com imputação não foi solicitada (marque a opção na configuração)."
      div(class = "alert alert-info", icon("circle-info"), " ", motivo)
    })

    output$grafico_imputacao <- renderPlot({
      r <- result_rv()
      req(r)
      g <- grafico_pca_imputacao(r)
      validate(need(!is.null(g), "Sem comparação de imputação disponível."))
      g
    })

    # Handlers de Download
    output$download_report_docx <- downloadHandler(
      filename = function() {
        paste0("relatorio_pca_", format(Sys.Date(), "%Y-%m-%d"), ".docx")
      },
      content = function(file) {
        req(data_rv())

        temp_dir <- tempdir()
        temp_qmd <- file.path(temp_dir, "relatorio_pca.qmd")
        temp_ref <- file.path(temp_dir, "custom-reference.docx")
        temp_func <- file.path(temp_dir, "funcoes_pca.R")
        temp_data <- file.path(temp_dir, "dados_limpos.rda")

        file.copy("templates/custom-reference.docx", temp_ref, overwrite = TRUE)
        file.copy("templates/funcoes_pca.R", temp_func, overwrite = TRUE)
        file.copy("templates/relatorio_pca.qmd", temp_qmd, overwrite = TRUE)

        df_clean <- data_rv()
        save(df_clean, file = temp_data)

        vars_str <- paste(input$vars_selected, collapse = ",")
        sup_str <- paste(input$quanti_sup %||% character(0), collapse = ",")
        quali_str <- input$quali_sup %||% ""
        custom_qmd_lines <- customize_pca_qmd_params(
          temp_qmd,
          vars_selected = vars_str,
          scale = input$scale,
          quanti_sup = sup_str,
          quali_sup = quali_str,
          seed = input$seed %||% 2026,
          comparar_imputacao = input$comparar_imputacao %||% FALSE
        )
        writeLines(custom_qmd_lines, temp_qmd)

        old_wd <- getwd()
        setwd(temp_dir)
        system2("quarto", args = c("render", "relatorio_pca.qmd", "--to", "docx"))
        setwd(old_wd)

        generated_docx <- file.path(temp_dir, "relatorio_pca.docx")
        if (file.exists(generated_docx)) {
          file.copy(generated_docx, file, overwrite = TRUE)
        } else {
          writeLines("Erro ao compilar o Word.", file)
        }
      }
    )

    output$download_project_zip <- downloadHandler(
      filename = function() {
        paste0("projeto_pca_", format(Sys.Date(), "%Y-%m-%d"), ".zip")
      },
      content = function(file) {
        info <- import_info()
        proj_dir_name <- paste0("projeto_pca_", format(Sys.Date(), "%Y-%m-%d"))
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
        save(df_clean, file = file.path(dir_dados, "dados_limpos.rda"))
        write.csv(df_clean, file = file.path(dir_dados, "dados_limpos.csv"), row.names = FALSE)
        ds_name <- if (info$source == "package") info$package_dataset else info$excel_sheet
        export_to_xlsx(df_clean, dataset_name = ds_name, file_path = file.path(dir_dados, "dados_limpos.xlsx"))

        vars_str <- paste(paste0("'", input$vars_selected, "'"), collapse = ", ")
        quanti_sel <- input$quanti_sup %||% character(0)
        quali_sel <- input$quali_sup %||% ""
        r_script_content <- c(
          "# --- SCRIPT DE ANÁLISE DE COMPONENTES PRINCIPAIS (PCA) ---",
          "# Pacotes necessários (instale uma vez):",
          "#   install.packages(c('FactoMineR', 'factoextra', 'ggcorrplot', 'patchwork'))",
          "#   install.packages('missMDA')   # só se for usar a comparação com imputação",
          "source('scripts/funcoes_pca.R')",
          "",
          "# 1. CARREGAR OS DADOS LIMPOS",
          "load('dados/dados_limpos.rda')",
          "dados <- df_clean",
          "",
          "# 2. EXECUTAR A PCA (semente fixa = permutação reprodutível)",
          sprintf("vars_sel <- c(%s)", vars_str),
          sprintf("quanti_sup <- %s", if (length(quanti_sel)) {
            paste0("c(", paste(paste0("'", quanti_sel, "'"), collapse = ", "), ")")
          } else "NULL"),
          sprintf("quali_sup <- %s", if (nzchar(quali_sel)) sprintf("'%s'", quali_sel) else "NULL"),
          sprintf("r_pca <- calcular_pca(dados, vars_sel, scale = %s, quanti_sup = quanti_sup,", as.character(isTRUE(input$scale))),
          sprintf("                      quali_sup = quali_sup, seed = %d, comparar_imputacao = %s)", as.integer(input$seed %||% 2026), as.character(isTRUE(input$comparar_imputacao))),
          "",
          "# 3. TABELAS E RELATO",
          "print(mostrar_pca_var(r_pca))",
          "print(mostrar_pca_retencao(r_pca))",
          "print(mostrar_pca_cargas(r_pca))",
          "cat(relatar_pca(r_pca))",
          "",
          "# 4. FIGURAS PRINCIPAIS",
          "print(grafico_pca_correlacoes(r_pca))",
          "print(grafico_pca_retencao(r_pca))",
          "print(grafico_pca_circulo(r_pca))",
          "print(grafico_pca_contribuicoes(r_pca))",
          sprintf("print(grafico_pca_individuos(r_pca, ellipse_type = '%s'))", input$ellipse_ind %||% "convex"),
          sprintf("print(grafico_pca_biplot(r_pca, versao = '%s', ellipse_type = '%s'))", input$versao_biplot %||% "classica", input$ellipse_biplot %||% "norm"),
          "print(grafico_pca_cargas(r_pca))",
          "print(grafico_pca_imputacao(r_pca))"
        )

        writeLines(r_script_content, file.path(dir_scripts, "analise_pca.R"))
        file.copy("templates/custom-reference.docx", file.path(dir_relatorios, "custom-reference.docx"), overwrite = TRUE)
        file.copy("templates/funcoes_pca.R", file.path(dir_scripts, "funcoes_pca.R"), overwrite = TRUE)

        vars_str_qmd <- paste(input$vars_selected, collapse = ",")
        sup_str_qmd <- paste(quanti_sel, collapse = ",")
        custom_qmd_lines <- customize_pca_qmd_params(
          "templates/relatorio_pca.qmd",
          vars_selected = vars_str_qmd,
          scale = input$scale,
          quanti_sup = sup_str_qmd,
          quali_sup = quali_sel,
          seed = input$seed %||% 2026,
          comparar_imputacao = input$comparar_imputacao %||% FALSE
        )
        writeLines(custom_qmd_lines, file.path(dir_relatorios, "relatorio_pca.qmd"))

        rproj_content <- c("Version: 1.0", "RestoreWorkspace: Default", "SaveWorkspace: Default", "Encoding: UTF-8")
        writeLines(rproj_content, file.path(proj_dir, "projeto_analise.Rproj"))

        readme_content <- c(
          "PACOTE DE ANÁLISE DE COMPONENTES PRINCIPAIS (PCA)",
          "- projeto_analise.Rproj: Duplo clique para abrir no RStudio.",
          "- dados/               : Contém os dados limpos em .rda, .csv e .xlsx.",
          "- scripts/analise_pca.R : Script com a PCA completa (permutação, figuras, relato).",
          "- relatorios/           : relatorio_pca.qmd para compilar em .docx com Quarto."
        )
        writeLines(readme_content, file.path(proj_dir, "README.txt"))

        old_wd <- getwd()
        setwd(temp_dir)
        zip::zip(file, files = proj_dir_name)
        setwd(old_wd)
      }
    )

    estado_execucao <- reactive({
      req(exec_ctrl$atualizada())
      r <- result_rv()
      req(r)
      list(
        analise_id = "pca",
        tipo = "pca",
        titulo = paste("PCA:", paste(input$vars_selected, collapse = ", ")),
        parametros = list(
          variaveis = input$vars_selected,
          padronizar = isTRUE(input$scale),
          suplementares_quantitativas = input$quanti_sup %||% character(0),
          variavel_grupo = if (nzchar(input$quali_sup %||% "")) input$quali_sup else NULL,
          semente = as.integer(input$seed),
          comparar_imputacao = isTRUE(input$comparar_imputacao),
          tipo_elipse = input$ellipse_biplot %||% "norm",
          versao_biplot = input$versao_biplot %||% "classica"
        ),
        saidas_disponiveis = c("narrativa", "tabela", "grafico", "diagnosticos"),
        resultado_resumo = list(
          n = r$n_usados,
          n_excluidos = r$n_excluidos,
          variancia_pc1 = r$autovalores$pct_variancia[1],
          variancia_pc2 = if (nrow(r$autovalores) >= 2L) r$autovalores$pct_variancia[2] else NA_real_,
          variancia_acumulada_pc2 = if (nrow(r$autovalores) >= 2L) r$autovalores$pct_acumulada[2] else NA_real_,
          eixos_significativos = r$eixos_significativos,
          concordancia_imputacao = if (!is.null(r$imputacao)) r$imputacao$concordancia else NULL
        )
      )
    })

    invisible(list(
      resultado = result_rv,
      estado_execucao = estado_execucao,
      estado_execucao_ui = exec_ctrl$estado,
      execucao_atualizada = exec_ctrl$atualizada
    ))
  })
}
