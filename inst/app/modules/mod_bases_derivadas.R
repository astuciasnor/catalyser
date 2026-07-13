# Gerenciador de Bases Derivadas — Fases 3A a 3B
# ---------------------------------------------------------------------------
# Interface para criar e administrar ramos em estrela e editar sua receita.
# O painel ainda não altera a entrada dos módulos analíticos; essa integração é
# feita separadamente para manter cada mudança pequena e testável.

library(shiny)
library(bslib)
library(DT)

mod_bases_derivadas_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(4, 8),
      card(
        card_header("Criar base derivada"),
        card_body(
          textInput(ns("nome_amigavel"), "Nome amigável:",
                    placeholder = "Ex.: Dados anuais para gráficos"),
          selectInput(ns("finalidade"), "Finalidade:", choices = bases_finalidades),
          textInput(ns("nome_r"), "Nome no código R:", value = "base_derivada"),
          textAreaInput(ns("descricao"), "Descrição (opcional):", rows = 3,
                        placeholder = "Explique por que esta preparação é específica."),
          div(class = "alert alert-info", style = "font-size:0.82rem; padding:9px 11px;",
              icon("diagram-project"),
              " Toda base derivada nasce diretamente de dados_analise. Ramos de ramos não são permitidos."),
          actionButton(ns("criar"), "Criar base derivada", icon = icon("plus"),
                       class = "btn-primary w-100")
        )
      ),
      navset_card_tab(
        nav_panel("Registro de Bases", icon = icon("list"),
                  card_body(DTOutput(ns("tabela")))),
        nav_panel("Prévia da base selecionada", icon = icon("table"),
                  card_body(DTOutput(ns("preview")))),
        nav_panel("Código do ramo", icon = icon("code"),
                  card_body(tags$pre(style = "white-space:pre-wrap; font-size:0.82rem;",
                                     verbatimTextOutput(ns("codigo")))))
      )
    ),
    layout_columns(
      col_widths = c(8, 4),
      card(
        card_header("Base selecionada"),
        card_body(uiOutput(ns("detalhes")))
      ),
      card(
        card_header("Ações"),
        card_body(
          div(class = "d-grid gap-2",
              actionButton(ns("renomear"), "Renomear / editar descrição",
                           icon = icon("pen"), class = "btn-outline-primary"),
              actionButton(ns("recalcular"), "Recalcular esta base",
                           icon = icon("arrows-rotate"), class = "btn-primary"),
              actionButton(ns("finalizar"), "Finalizar preparo",
                           icon = icon("circle-check"), class = "btn-success"),
              actionButton(ns("reabrir"), "Reabrir como rascunho",
                           icon = icon("rotate-left"), class = "btn-outline-secondary"),
              actionButton(ns("excluir"), "Excluir base derivada",
                           icon = icon("trash"), class = "btn-outline-danger")),
          helpText("Selecione uma linha no Registro de Bases. dados_analise nunca pode ser excluída aqui.")
        )
      )
    ),
    card(
      card_header("Receita específica da base selecionada"),
      card_body(
        uiOutput(ns("editor_base")),
        layout_columns(
          col_widths = c(5, 7),
          div(
            selectInput(
              ns("ramo_tipo"), "Tratamento a adicionar:",
              choices = c(
                "Dados faltantes (NA)" = "tratar_na",
                "Dicotomizar (0/1)" = "dicotomizar",
                "Padronizar / Escalar" = "padronizar",
                "Classes de tamanho (binning)" = "binning",
                "Remover duplicatas" = "remover_duplicatas",
                "Padronizar texto" = "padronizar_texto",
                "Calcular variável" = "calcular",
                "Reescalar (prefixo SI)" = "reescalar",
                "Filtrar linhas" = "filtrar"
              )
            ),
            uiOutput(ns("parametros_etapa")),
            actionButton(ns("adicionar_etapa"), "Adicionar à receita do ramo",
                         icon = icon("plus"), class = "btn-primary w-100")
          ),
          div(
            uiOutput(ns("receita_ramo")),
            selectInput(ns("etapa_selecionada"), "Etapa selecionada:", choices = NULL),
            div(
              class = "d-flex gap-2 flex-wrap",
              actionButton(ns("etapa_subir"), "Subir", icon = icon("arrow-up"),
                           class = "btn-outline-secondary btn-sm"),
              actionButton(ns("etapa_descer"), "Descer", icon = icon("arrow-down"),
                           class = "btn-outline-secondary btn-sm"),
              actionButton(ns("etapa_alternar"), "Ativar/Desativar", icon = icon("power-off"),
                           class = "btn-outline-secondary btn-sm"),
              actionButton(ns("etapa_remover"), "Remover", icon = icon("trash"),
                           class = "btn-outline-danger btn-sm"),
              actionButton(ns("etapas_limpar"), "Limpar receita", icon = icon("broom"),
                           class = "btn-outline-danger btn-sm")
            ),
            div(class = "alert alert-info mt-3 mb-0",
                style = "font-size:0.82rem; padding:8px 10px;",
                icon("circle-info"),
                " Editar a receita marca apenas este ramo como desatualizado. Use Recalcular esta base quando terminar.")
          )
        )
      )
    )
  )
}

mod_bases_derivadas_server <- function(id, dados_analise_rv, registro_bases_rv,
                                       cache_bases_rv, revisao_origem_rv = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    contador <- reactiveVal(1L)
    id_selecionada_rv <- reactiveVal(NULL)

    dados_raiz <- reactive({ req(dados_analise_rv()); as.data.frame(dados_analise_rv()) })
    registros <- reactive({ registro_bases_rv() %||% list() })
    caches <- reactive({ cache_bases_rv() %||% list() })
    revisao_atual <- reactive({
      if (is.function(revisao_origem_rv)) as.integer(revisao_origem_rv()) else 1L
    })

    proximo_id <- function() {
      usados <- vapply(registros(), `[[`, character(1), "id")
      repeat {
        n <- contador(); contador(n + 1L)
        id_novo <- sprintf("base_%04d", n)
        if (!(id_novo %in% usados)) return(id_novo)
      }
    }

    observeEvent(input$nome_amigavel, {
      atual <- isolate(input$nome_r %||% "")
      if (!nzchar(atual) || identical(atual, "base_derivada"))
        updateTextInput(session, "nome_r",
                        value = bases_sugerir_nome_r(input$nome_amigavel, input$finalidade))
    }, ignoreInit = TRUE)

    observeEvent(input$finalidade, {
      if (!nzchar(input$nome_amigavel %||% ""))
        updateTextInput(session, "nome_r",
                        value = bases_sugerir_nome_r("", input$finalidade))
    }, ignoreInit = TRUE)

    observeEvent(input$criar, {
      msg <- bases_validar_metadados(registros(), input$nome_amigavel, input$nome_r)
      if (!is.null(msg)) {
        showNotification(msg, type = "error", duration = 8)
        return()
      }
      revisao <- if (is.function(revisao_origem_rv)) revisao_origem_rv() else 1L
      nova <- bases_novo_registro(
        id = proximo_id(), nome_amigavel = input$nome_amigavel,
        nome_r = input$nome_r, finalidade = input$finalidade,
        descricao = input$descricao, revisao_origem = revisao
      )
      registro_bases_rv(bases_adicionar(registros(), nova))
      updateTextInput(session, "nome_amigavel", value = "")
      updateTextInput(session, "nome_r", value = "base_derivada")
      updateTextAreaInput(session, "descricao", value = "")
      showNotification(sprintf("Base '%s' criada como rascunho.", nova$nome_r),
                       type = "message", duration = 5)
    })

    # A tabela lê somente metadados do cache. Não executa replay de nenhum ramo.
    tabela_atual <- reactive({ bases_tabela(registros(), caches(), revisao_atual()) })

    output$tabela <- renderDT({
      tab <- tabela_atual()
      validate(need(nrow(tab) > 0, "Nenhuma base derivada criada. dados_analise continua sendo a base compartilhada."))
      datatable(tab, selection = "single", rownames = FALSE,
                options = list(pageLength = 8, scrollX = TRUE, dom = "tip"))
    })

    observeEvent(input$tabela_rows_selected, {
      linha <- input$tabela_rows_selected
      if (!length(linha)) return()
      tab <- tabela_atual()
      if (linha <= nrow(tab)) id_selecionada_rv(tab$ID[[linha]])
    })

    base_selecionada <- reactive({
      id <- id_selecionada_rv()
      if (is.null(id)) return(NULL)
      bases_obter(registros(), id)
    })

    cache_selecionado <- reactive({
      base <- base_selecionada()
      if (is.null(base)) return(NULL)
      bases_cache_obter(caches(), base$id)
    })

    # O formulário nunca dispara replay. Quando o cache está atualizado, usa
    # suas classes e colunas; caso contrário, parte de dados_analise e acrescenta
    # apenas os nomes de colunas que a própria receita declara criar.
    colunas_editor <- reactive({
      base <- base_selecionada()
      raiz <- dados_raiz()
      if (is.null(base))
        return(list(todas = names(raiz), numericas = trat_cols_num(raiz),
                    texto = names(raiz)[vapply(raiz, function(x)
                      is.character(x) || is.factor(x), logical(1))], df = raiz,
                    fonte = "dados_analise"))
      entrada <- cache_selecionado()
      if (identical(bases_estado_cache(base, entrada, revisao_atual()), "Atualizada") &&
          !is.null(entrada$df)) {
        df <- entrada$df
        return(list(todas = names(df), numericas = trat_cols_num(df),
                    texto = names(df)[vapply(df, function(x)
                      is.character(x) || is.factor(x), logical(1))], df = df,
                    fonte = "cache atualizado"))
      }
      etapas <- Filter(function(et) isTRUE(et$ativa), base$etapas %||% list())
      criadas_num <- vapply(Filter(function(et)
        et$tipo %in% c("dicotomizar", "padronizar", "calcular", "reescalar"), etapas),
        function(et) et$params$nome %||% "", character(1))
      criadas_cat <- vapply(Filter(function(et) identical(et$tipo, "binning"), etapas),
        function(et) et$params$nome %||% "", character(1))
      todas <- unique(c(names(raiz), criadas_num, criadas_cat))
      list(
        todas = todas[nzchar(todas)],
        numericas = unique(c(trat_cols_num(raiz), criadas_num[nzchar(criadas_num)])),
        texto = unique(c(names(raiz)[vapply(raiz, function(x)
          is.character(x) || is.factor(x), logical(1))], criadas_cat[nzchar(criadas_cat)])),
        df = raiz,
        fonte = "nomes previstos; recalcule para atualizar valores e tipos"
      )
    })

    valor_selecionado <- function(id, escolhas, padrao = NULL) {
      atual <- isolate(input[[id]])
      if (!is.null(atual) && length(atual) && atual %in% escolhas) return(atual)
      if (!is.null(padrao) && padrao %in% escolhas) return(padrao)
      if (length(escolhas)) escolhas[[1]] else character(0)
    }

    niveis_coluna <- function(coluna, info) {
      if (is.null(coluna) || !coluna %in% names(info$df)) return(character(0))
      sort(unique(as.character(info$df[[coluna]][!is.na(info$df[[coluna]])])))
    }

    output$editor_base <- renderUI({
      base <- base_selecionada()
      if (is.null(base))
        return(div(class = "alert alert-secondary", "Selecione uma base no Registro de Bases para editar sua receita."))
      info <- colunas_editor()
      tagList(
        div(class = "d-flex justify-content-between align-items-center mb-2",
            div(strong(base$nome_amigavel), " ", tags$code(base$nome_r)),
            tags$span(class = paste("badge", if (identical(base$estado, "rascunho"))
              "bg-warning text-dark" else "bg-success"), base$estado)),
        if (!identical(base$estado, "rascunho"))
          div(class = "alert alert-warning mb-2", style = "font-size:0.82rem; padding:8px 10px;",
              "Esta base está pronta. Clique em Reabrir como rascunho antes de alterar a receita."),
        if (identical(base$estado, "rascunho") && length(base$etapas %||% list()) &&
            !identical(bases_estado_cache(base, cache_selecionado(), revisao_atual()), "Atualizada"))
          div(class = "alert alert-warning mb-2", style = "font-size:0.82rem; padding:8px 10px;",
              "Recalcule esta receita antes de adicionar outra etapa. Ordenar, desativar ou remover continua permitido."),
        div(style = "font-size:0.78rem; color:#666;",
            sprintf("Colunas do formulário: %s. Nenhum replay é executado ao abrir o editor.", info$fonte))
      )
    })

    output$parametros_etapa <- renderUI({
      base <- base_selecionada()
      if (is.null(base) || !identical(base$estado, "rascunho")) return(NULL)
      info <- colunas_editor()
      todas <- info$todas
      numericas <- info$numericas
      texto <- if (length(info$texto)) info$texto else todas
      tipo <- input$ramo_tipo %||% "tratar_na"

      switch(tipo,
        tratar_na = tagList(
          selectInput(ns("ramo_na_col"), "Coluna:",
                      choices = c("Todas as numéricas" = "__num__",
                                  stats::setNames(todas, todas)),
                      selected = valor_selecionado("ramo_na_col", c("__num__", todas), "__num__")),
          radioButtons(ns("ramo_na_metodo"), "O que fazer com os NA?",
            choices = c("Remover as linhas" = "remover", "Imputar a média" = "media",
                        "Imputar a mediana" = "mediana", "Imputar a moda" = "moda",
                        "Valor constante" = "constante"),
            selected = input$ramo_na_metodo %||% "mediana"),
          if (identical(input$ramo_na_metodo %||% "mediana", "constante"))
            numericInput(ns("ramo_na_valor"), "Valor constante:", value = 0)
        ),
        dicotomizar = {
          col <- valor_selecionado("ramo_dic_col", todas)
          origem <- input$ramo_dic_origem %||% if (col %in% numericas) "numerica" else "categorica"
          tagList(
            selectInput(ns("ramo_dic_col"), "Coluna de origem:", choices = todas, selected = col),
            radioButtons(ns("ramo_dic_origem"), "Como definir o 1?",
              choices = c("Por limiar" = "numerica", "Por níveis" = "categorica"),
              selected = origem, inline = TRUE),
            if (identical(origem, "numerica"))
              layout_columns(
                selectInput(ns("ramo_dic_op"), "Operador:", c(">=", ">", "<=", "<"), selected = ">="),
                numericInput(ns("ramo_dic_limiar"), "Limiar:", value = 0)
              ) else
              selectizeInput(ns("ramo_dic_niveis"), "Níveis que viram 1:",
                             choices = niveis_coluna(col, info), multiple = TRUE),
            textInput(ns("ramo_dic_nome"), "Nome da variável 0/1:",
                      value = isolate(input$ramo_dic_nome) %||% paste0(col, "_bin"))
          )
        },
        padronizar = {
          col <- valor_selecionado("ramo_pad_col", numericas)
          metodo <- input$ramo_pad_metodo %||% "zscore"
          suf <- c(zscore = "_z", centralizar = "_c", normalizar = "_norm")[[metodo]]
          tagList(
            selectInput(ns("ramo_pad_col"), "Coluna numérica:", choices = numericas, selected = col),
            radioButtons(ns("ramo_pad_metodo"), "Método:",
              choices = c("Escore z" = "zscore", "Centralizar" = "centralizar",
                          "Normalizar 0–1" = "normalizar"), selected = metodo),
            textInput(ns("ramo_pad_nome"), "Nome da coluna nova:",
                      value = isolate(input$ramo_pad_nome) %||% paste0(col, suf))
          )
        },
        binning = {
          col <- valor_selecionado("ramo_bin_col", numericas)
          tagList(
            selectInput(ns("ramo_bin_col"), "Coluna numérica:", choices = numericas, selected = col),
            numericInput(ns("ramo_bin_n"), "Número de classes:", value = 4, min = 2, max = 20),
            radioButtons(ns("ramo_bin_metodo"), "Cortes:",
              choices = c("Amplitude igual" = "igual", "Por quantis" = "quantil"),
              selected = "igual", inline = TRUE),
            textInput(ns("ramo_bin_nome"), "Nome da coluna de classes:",
                      value = isolate(input$ramo_bin_nome) %||% paste0(col, "_classe"))
          )
        },
        remover_duplicatas = tagList(
          selectizeInput(ns("ramo_dup_cols"), "Colunas-chave (vazio = linha inteira):",
                         choices = todas, multiple = TRUE,
                         selected = intersect(isolate(input$ramo_dup_cols) %||% character(0), todas)),
          helpText("Mantém a primeira ocorrência de cada combinação.")
        ),
        padronizar_texto = tagList(
          selectInput(ns("ramo_txt_col"), "Coluna de texto:", choices = texto,
                      selected = valor_selecionado("ramo_txt_col", texto)),
          radioButtons(ns("ramo_txt_metodo"), "Ação:",
            choices = c("Remover espaços extras" = "squish", "minúsculas" = "minusculas",
                        "MAIÚSCULAS" = "maiusculas", "Iniciais Maiúsculas" = "titulo"),
            selected = "squish")
        ),
        calcular = tagList(
          textInput(ns("ramo_calc_nome"), "Nome da variável nova:", value = "nova_variavel"),
          textInput(ns("ramo_calc_expr"), "Fórmula:",
                    placeholder = "100 * peso_g / comprimento_cm^3"),
          div(style = "font-size:0.78rem; color:#666;", strong("Colunas: "), paste(todas, collapse = ", "))
        ),
        reescalar = {
          col <- valor_selecionado("ramo_re_col", numericas)
          tagList(
            selectInput(ns("ramo_re_col"), "Coluna numérica:", choices = numericas, selected = col),
            selectInput(ns("ramo_re_prefixo"), "Prefixo de destino:",
              choices = c("base (×1)" = "", "quilo k (×10³)" = "k", "mega M (×10⁶)" = "M",
                          "mili m (×10⁻³)" = "m", "micro µ (×10⁻⁶)" = "µ"), selected = "k"),
            textInput(ns("ramo_re_nome"), "Nome da coluna nova:",
                      value = isolate(input$ramo_re_nome) %||% paste0(col, "_k"))
          )
        },
        filtrar = {
          col <- valor_selecionado("ramo_fil_col", todas)
          origem <- input$ramo_fil_origem %||% if (col %in% numericas) "numerica" else "categorica"
          tagList(
            selectInput(ns("ramo_fil_col"), "Coluna:", choices = todas, selected = col),
            radioButtons(ns("ramo_fil_origem"), "Condição:",
              choices = c("Numérica" = "numerica", "Categórica" = "categorica"),
              selected = origem, inline = TRUE),
            if (identical(origem, "numerica"))
              layout_columns(
                selectInput(ns("ramo_fil_op"), "Operador:",
                            c(">=", ">", "<=", "<", "==", "!="), selected = ">="),
                numericInput(ns("ramo_fil_valor"), "Valor:", value = 0)
              ) else
              selectizeInput(ns("ramo_fil_niveis"), "Níveis a manter:",
                             choices = niveis_coluna(col, info), multiple = TRUE)
          )
        }
      )
    })

    output$receita_ramo <- renderUI({
      base <- base_selecionada()
      if (is.null(base)) return(NULL)
      etapas <- base$etapas %||% list()
      if (!length(etapas))
        return(div(class = "alert alert-light border", "Receita vazia: o ramo ainda é idêntico a dados_analise."))
      tags$ol(
        class = "mb-3",
        lapply(seq_along(etapas), function(i) {
          et <- etapas[[i]]
          tt <- tratamentos[[et$tipo]]
          rotulo <- if (is.null(tt)) et$tipo else tt$rotulo(et$params)
          tags$li(
            style = if (isTRUE(et$ativa)) NULL else "opacity:0.55; text-decoration:line-through;",
            tags$code(et$tipo), " — ", rotulo,
            if (!isTRUE(et$ativa)) tags$span(class = "badge bg-secondary ms-1", "inativa")
          )
        })
      )
    })

    observeEvent(base_selecionada(), {
      base <- base_selecionada()
      escolhas <- if (is.null(base)) character(0) else bases_rotulos_etapas(base)
      atual <- isolate(input$etapa_selecionada)
      selecionada <- if (!is.null(atual) && atual %in% unname(escolhas)) atual else
        if (length(escolhas)) tail(unname(escolhas), 1) else character(0)
      updateSelectInput(session, "etapa_selecionada", choices = escolhas, selected = selecionada)
    }, ignoreNULL = FALSE)

    coletar_params <- function(tipo) {
      switch(tipo,
        tratar_na = list(coluna = input$ramo_na_col, metodo = input$ramo_na_metodo,
                         valor = input$ramo_na_valor),
        dicotomizar = list(coluna = input$ramo_dic_col, origem = input$ramo_dic_origem,
                           operador = input$ramo_dic_op, limiar = input$ramo_dic_limiar,
                           niveis_1 = input$ramo_dic_niveis,
                           nome = trimws(input$ramo_dic_nome %||% "")),
        padronizar = list(coluna = input$ramo_pad_col, metodo = input$ramo_pad_metodo,
                          nome = trimws(input$ramo_pad_nome %||% "")),
        binning = list(coluna = input$ramo_bin_col, n = input$ramo_bin_n,
                       metodo = input$ramo_bin_metodo,
                       nome = trimws(input$ramo_bin_nome %||% "")),
        remover_duplicatas = list(colunas = input$ramo_dup_cols),
        padronizar_texto = list(coluna = input$ramo_txt_col, metodo = input$ramo_txt_metodo),
        calcular = list(nome = trimws(input$ramo_calc_nome %||% ""),
                        expr = input$ramo_calc_expr %||% ""),
        reescalar = list(coluna = input$ramo_re_col, simbolo = input$ramo_re_prefixo,
                         nome = trimws(input$ramo_re_nome %||% "")),
        filtrar = list(coluna = input$ramo_fil_col, origem = input$ramo_fil_origem,
                       operador = input$ramo_fil_op, valor = input$ramo_fil_valor,
                       niveis = input$ramo_fil_niveis)
      )
    }

    dados_validacao_etapa <- function(base) {
      if (!length(base$etapas %||% list())) return(dados_raiz())
      entrada <- bases_cache_obter(caches(), base$id)
      if (!identical(bases_estado_cache(base, entrada, revisao_atual()), "Atualizada") ||
          is.null(entrada$df))
        stop("Recalcule esta base antes de adicionar outra etapa à receita.", call. = FALSE)
      entrada$df
    }

    observeEvent(input$adicionar_etapa, {
      base <- base_selecionada()
      if (is.null(base)) { showNotification("Selecione uma base.", type = "warning"); return() }
      tipo <- input$ramo_tipo
      novo <- tryCatch(
        bases_adicionar_etapa(registros(), base$id, tipo, coletar_params(tipo),
                              dados_validacao_etapa(base)),
        error = function(e) e
      )
      if (inherits(novo, "error")) {
        showNotification(conditionMessage(novo), type = "error", duration = 10)
      } else {
        registro_bases_rv(novo)
        updateSelectInput(session, "etapa_selecionada",
                          selected = as.character(length(base$etapas %||% list()) + 1L))
        showNotification("Etapa adicionada. O cache deste ramo está desatualizado até o recálculo manual.",
                         type = "message", duration = 6)
      }
    })

    indice_etapa <- reactive({
      i <- suppressWarnings(as.integer(input$etapa_selecionada))
      if (length(i)) i else NA_integer_
    })

    executar_edicao <- function(funcao, mensagem, selecionar = NULL) {
      base <- base_selecionada()
      if (is.null(base)) { showNotification("Selecione uma base.", type = "warning"); return(FALSE) }
      novo <- tryCatch(funcao(registros(), base), error = function(e) e)
      if (inherits(novo, "error")) {
        showNotification(conditionMessage(novo), type = "error", duration = 9)
        return(FALSE)
      }
      registro_bases_rv(novo)
      if (!is.null(selecionar)) updateSelectInput(session, "etapa_selecionada", selected = as.character(selecionar))
      showNotification(mensagem, type = "message", duration = 4)
      TRUE
    }

    observeEvent(input$etapa_subir, {
      i <- indice_etapa()
      if (is.na(i) || i <= 1L) return()
      executar_edicao(function(rs, base) bases_mover_etapa(rs, base$id, i, "subir"),
                       "Etapa movida. Recalcule o ramo quando terminar.", i - 1L)
    })
    observeEvent(input$etapa_descer, {
      i <- indice_etapa(); base <- base_selecionada()
      if (is.null(base) || is.na(i) || i >= length(base$etapas %||% list())) return()
      executar_edicao(function(rs, base) bases_mover_etapa(rs, base$id, i, "descer"),
                       "Etapa movida. Recalcule o ramo quando terminar.", i + 1L)
    })
    observeEvent(input$etapa_alternar, {
      i <- indice_etapa(); if (is.na(i)) return()
      executar_edicao(function(rs, base) bases_alternar_etapa(rs, base$id, i),
                       "Estado da etapa alterado. Recalcule o ramo quando terminar.", i)
    })
    observeEvent(input$etapa_remover, {
      i <- indice_etapa(); if (is.na(i)) return()
      executar_edicao(function(rs, base) bases_remover_etapa(rs, base$id, i),
                       "Etapa removida. Recalcule o ramo quando terminar.", max(1L, i - 1L))
    })
    observeEvent(input$etapas_limpar, {
      base <- base_selecionada()
      if (is.null(base) || !length(base$etapas %||% list())) return()
      showModal(modalDialog(
        title = "Limpar a receita deste ramo?",
        tags$p("Todas as etapas específicas de ", tags$code(base$nome_r), " serão removidas."),
        tags$p("dados_analise e os outros ramos não serão alterados."),
        footer = tagList(modalButton("Cancelar"),
                         actionButton(ns("confirmar_limpar_etapas"), "Limpar receita",
                                      icon = icon("broom"), class = "btn-danger"))
      ))
    })
    observeEvent(input$confirmar_limpar_etapas, {
      concluido <- executar_edicao(function(rs, base) bases_limpar_etapas(rs, base$id),
                                    "Receita limpa. O ramo voltou a ser equivalente a dados_analise.")
      if (isTRUE(concluido)) removeModal()
    })

    output$preview <- renderDT({
      entrada <- cache_selecionado()
      validate(
        need(!is.null(base_selecionada()), "Selecione uma base no registro."),
        need(!is.null(entrada), "Esta base ainda não foi calculada. Clique em Recalcular esta base."),
        need(!is.null(entrada$df), "O último recálculo não produziu uma tabela válida.")
      )
      datatable(head(entrada$df, 300), rownames = FALSE,
                options = list(pageLength = 12, scrollX = TRUE))
    })

    output$codigo <- renderText({
      base <- base_selecionada()
      if (is.null(base)) return("# Selecione uma base no registro.")
      bases_codigo(base)
    })

    output$detalhes <- renderUI({
      base <- base_selecionada()
      if (is.null(base))
        return(div(style = "color:#777;", icon("circle-info"), " Selecione uma linha na tabela."))
      entrada <- cache_selecionado()
      estado_cache <- bases_estado_cache(base, entrada, revisao_atual())
      finalidade_rotulo <- names(bases_finalidades)[match(base$finalidade, bases_finalidades)]
      classe_badge <- switch(estado_cache,
        "Atualizada" = "bg-success",
        "Desatualizada" = "bg-warning text-dark",
        "Com erro" = "bg-danger",
        "bg-secondary")
      tagList(
        tags$dl(class = "row mb-0",
          tags$dt(class = "col-sm-3", "Nome"), tags$dd(class = "col-sm-9", base$nome_amigavel),
          tags$dt(class = "col-sm-3", "Objeto R"), tags$dd(class = "col-sm-9", tags$code(base$nome_r)),
          tags$dt(class = "col-sm-3", "Origem"), tags$dd(class = "col-sm-9", tags$code("dados_analise")),
          tags$dt(class = "col-sm-3", "Finalidade"), tags$dd(class = "col-sm-9", finalidade_rotulo %||% base$finalidade),
          tags$dt(class = "col-sm-3", "Estado"), tags$dd(class = "col-sm-9", strong(base$estado)),
          tags$dt(class = "col-sm-3", "Cache"),
          tags$dd(class = "col-sm-9", tags$span(class = paste("badge", classe_badge), estado_cache)),
          tags$dt(class = "col-sm-3", "Etapas"), tags$dd(class = "col-sm-9", length(base$etapas %||% list())),
          tags$dt(class = "col-sm-3", "Dimensões"),
          tags$dd(class = "col-sm-9", if (is.null(entrada) || is.null(entrada$df)) "—" else
                    sprintf("%d linhas × %d colunas", nrow(entrada$df), ncol(entrada$df))),
          tags$dt(class = "col-sm-3", "Última tentativa"),
          tags$dd(class = "col-sm-9", if (is.null(entrada$calculada_em)) "—" else
                    format(entrada$calculada_em, "%d/%m/%Y %H:%M:%S")),
          tags$dt(class = "col-sm-3", "Prévia válida"),
          tags$dd(class = "col-sm-9", if (is.null(entrada$resultado_em)) "—" else
                    format(entrada$resultado_em, "%d/%m/%Y %H:%M:%S")),
          tags$dt(class = "col-sm-3", "Descrição"), tags$dd(class = "col-sm-9", if (nzchar(base$descricao)) base$descricao else "—")
        ),
        if (identical(estado_cache, "Desatualizada"))
          div(class = "alert alert-warning mt-2 mb-0", style = "font-size:0.82rem; padding:8px 10px;",
              "dados_analise ou a receita mudou. A última prévia foi preservada, mas esta base não poderá alimentar análises antes de ser recalculada."),
        if (identical(estado_cache, "Com erro"))
          div(class = "alert alert-danger mt-2 mb-0", style = "font-size:0.82rem; padding:8px 10px;",
              strong("Erro isolado neste ramo: "), paste(unlist(entrada$erros), collapse = "; "),
              if (isTRUE(entrada$preview_anterior))
                tags$span(" A tabela exibida é a última prévia válida e não poderá alimentar análises.")),
        if (!length(base$etapas %||% list()))
          div(class = "alert alert-warning mt-2 mb-0", style = "font-size:0.82rem; padding:8px 10px;",
              "Este ramo ainda é idêntico a dados_analise. Use o editor de receita para adicionar preparo específico.")
      )
    })

    observeEvent(input$recalcular, {
      base <- base_selecionada()
      if (is.null(base)) { showNotification("Selecione uma base.", type = "warning"); return() }
      anterior <- bases_cache_obter(caches(), base$id)
      entrada <- bases_recalcular_cache(
        dados_raiz(), base, revisao_atual(), entrada_anterior = anterior
      )
      cache_bases_rv(bases_cache_gravar(caches(), base$id, entrada))
      if (length(entrada$erros)) {
        showNotification(
          paste("O ramo foi isolado com erro:", paste(unlist(entrada$erros), collapse = "; ")),
          type = "error", duration = 12
        )
      } else {
        showNotification(sprintf("Base '%s' recalculada: %d linhas × %d colunas.",
                                 base$nome_r, entrada$linhas, entrada$colunas),
                         type = "message", duration = 5)
      }
    })

    observeEvent(input$finalizar, {
      base <- base_selecionada()
      if (is.null(base)) { showNotification("Selecione uma base.", type = "warning"); return() }
      novo <- tryCatch(
        bases_finalizar(registros(), base$id, caches(), revisao_atual()),
        error = function(e) e
      )
      if (inherits(novo, "error")) {
        showNotification(conditionMessage(novo), type = "error", duration = 10)
      } else {
        registro_bases_rv(novo)
        showNotification(sprintf("Base '%s' marcada como pronta.", base$nome_r), type = "message")
      }
    })

    observeEvent(input$reabrir, {
      base <- base_selecionada()
      if (is.null(base)) { showNotification("Selecione uma base.", type = "warning"); return() }
      registro_bases_rv(bases_reabrir(registros(), base$id))
      showNotification(sprintf("Base '%s' reaberta como rascunho.", base$nome_r), type = "message")
    })

    observeEvent(input$renomear, {
      base <- base_selecionada()
      if (is.null(base)) { showNotification("Selecione uma base.", type = "warning"); return() }
      showModal(modalDialog(
        title = "Renomear base derivada",
        textInput(ns("editar_nome_amigavel"), "Nome amigável:", value = base$nome_amigavel),
        textInput(ns("editar_nome_r"), "Nome no código R:", value = base$nome_r),
        textAreaInput(ns("editar_descricao"), "Descrição:", value = base$descricao, rows = 3),
        footer = tagList(modalButton("Cancelar"),
                         actionButton(ns("confirmar_renomear"), "Salvar", class = "btn-primary"))
      ))
    })

    observeEvent(input$confirmar_renomear, {
      base <- base_selecionada()
      if (is.null(base)) { removeModal(); return() }
      novo <- tryCatch(
        bases_renomear(registros(), base$id, input$editar_nome_amigavel,
                       input$editar_nome_r, input$editar_descricao),
        error = function(e) e
      )
      if (inherits(novo, "error")) {
        showNotification(conditionMessage(novo), type = "error", duration = 8)
      } else {
        registro_bases_rv(novo)
        removeModal()
        showNotification("Metadados da base atualizados.", type = "message")
      }
    })

    observeEvent(input$excluir, {
      base <- base_selecionada()
      if (is.null(base)) { showNotification("Selecione uma base.", type = "warning"); return() }
      showModal(modalDialog(
        title = "Excluir base derivada?",
        tags$p("Esta ação remove apenas o ramo ", tags$code(base$nome_r),
               ". A base compartilhada dados_analise não será alterada."),
        tags$p(strong("A exclusão não pode ser desfeita nesta sessão.")),
        footer = tagList(modalButton("Cancelar"),
                         actionButton(ns("confirmar_excluir"), "Excluir base",
                                      icon = icon("trash"), class = "btn-danger"))
      ))
    })

    observeEvent(input$confirmar_excluir, {
      base <- base_selecionada()
      if (is.null(base)) { removeModal(); return() }
      registro_bases_rv(bases_excluir(registros(), base$id))
      cache_bases_rv(bases_cache_excluir(caches(), base$id))
      id_selecionada_rv(NULL)
      removeModal()
      showNotification(sprintf("Base '%s' excluída.", base$nome_r), type = "message")
    })

    disponiveis <- reactive({
      bases_disponiveis_analise(registros(), caches(), revisao_atual())
    })
    invisible(list(registro = registros, cache = caches, disponiveis = disponiveis,
                   selecionada = base_selecionada))
  })
}
