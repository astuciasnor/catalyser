# Comunicação de Resultados — estúdio editorial e exportador integrado
# ---------------------------------------------------------------------------
# Consome as execuções registradas na Fase 3C. A Fase 3D organiza a seleção
# editorial; na Fase 3E, o Word obedece ao manifesto e o Projeto R preserva
# todas as execuções registradas.

library(shiny)
library(bslib)

comunicacao_classe_estado <- function(estado) {
  if (identical(estado, "Atualizada")) "text-bg-success" else "text-bg-warning"
}

mod_comunicacao_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "d-flex justify-content-between align-items-start gap-3 flex-wrap",
      style = "padding:4px 2px 10px;",
      div(
        h4(
          "Comunicação de Resultados",
          style = "font-family:'Outfit',sans-serif; font-weight:700; color:#0F3B5F; margin-bottom:2px;"
        ),
        p(
          style = "color:#495057; font-size:0.9rem; margin:0;",
          "Organize o que foi executado e escolha o que aparecerá no Word. ",
          strong("O Projeto R preservará todas as execuções registradas.")
        )
      ),
      div(
        class = "align-self-center",
        uiOutput(ns("contador"))
      )
    ),
    navset_card_tab(
      id = ns("comunicacao_subabas"),
      nav_panel(
        title = "1. Esboço do documento",
        icon = icon("list-ol"),
        card_body(
          style = "padding:14px 16px;",
          layout_columns(
            col_widths = c(7, 5),
            fill = FALSE,
            fillable = FALSE,
            div(
              h5("Estrutura prevista", class = "mb-1"),
              p(
                class = "small text-muted mb-3",
                "O esboço acompanha a seleção e a ordem definidas em Execuções registradas."
              ),
              div(
                class = "border rounded p-3 bg-light",
                uiOutput(ns("esboco"))
              ),
              tags$details(
                class = "border rounded mt-3",
                tags$summary(
                  class = "px-3 py-2 fw-semibold",
                  style = "cursor:pointer; color:#0F3B5F;",
                  icon("code"), " Manifesto editorial — detalhe técnico"
                ),
                div(
                  class = "px-3 pb-3",
                  tags$pre(
                    style = paste(
                      "white-space:pre-wrap; font-size:0.80rem;",
                      "color:#334155; max-height:28rem; overflow:auto;"
                    ),
                    textOutput(ns("manifesto_texto"), container = span)
                  )
                )
              )
            ),
            card(
              fill = FALSE,
              class = "mb-0",
              card_header("Seções globais do documento"),
              card_body(
                style = "padding:12px 15px;",
                textAreaInput(ns("introducao"), "Introdução (opcional):", rows = 3),
                textAreaInput(ns("metodos"), "Métodos gerais (opcional):", rows = 3),
                textAreaInput(ns("discussao"), "Discussão (opcional):", rows = 3),
                textAreaInput(ns("conclusao"), "Conclusão (opcional):", rows = 2)
              )
            )
          )
        )
      ),
      nav_panel(
        title = "2. Execuções registradas",
        icon = icon("list-check"),
        card_body(
          style = "padding:14px 16px;",
          h5("Seleção editorial e ordem dos resultados", class = "mb-1"),
          p(
            class = "small text-muted mb-3",
            "Abra uma execução por vez, escolha o conteúdo do Word e use as setas para definir a ordem do relatório."
          ),
          uiOutput(ns("acoes_fila")),
          uiOutput(ns("fila")),
          div(
            class = "alert alert-light border small mt-3 mb-0",
            icon("circle-info"), " ",
            "Desmarcar “Incluir esta execução no Word” não apaga a execução: ",
            "ela permanece preservada no Projeto R."
          )
        )
      ),
      nav_panel(
        title = "3. Bases do projeto",
        icon = icon("database"),
        card_body(
          style = "padding:14px 16px;",
          layout_columns(
            col_widths = c(7, 5),
            fill = FALSE,
            fillable = FALSE,
            div(
              h5("Proveniência dos resultados", class = "mb-1"),
              p(
                class = "small text-muted mb-3",
                "Confira a Base Compartilhada e as Bases Derivadas vinculadas às execuções."
              ),
              uiOutput(ns("bases_projeto"))
            ),
            div(
              class = "alert alert-info small mb-0",
              icon("diagram-project"), " ",
              strong("Percurso preservado. "),
              "Cada execução mantém a referência à base utilizada. ",
              "O Projeto R leva todas as bases e execuções registradas, ",
              "inclusive as que não forem incluídas no Word."
            )
          )
        )
      ),
      nav_panel(
        title = "4. Saída planejada",
        icon = icon("file-export"),
        card_body(
          style = "padding:14px 16px;",
          layout_columns(
            col_widths = c(7, 5),
            fill = FALSE,
            fillable = FALSE,
            card(
              fill = FALSE,
              class = "mb-0",
              card_header("Conferência antes da exportação"),
              card_body(
                style = "padding:12px 15px;",
                uiOutput(ns("resumo_saida")),
                radioButtons(
                  ns("formato"), "Formato principal:",
                  choices = c("Word (.docx) — tema Ocean" = "docx"),
                  selected = "docx"
                )
              )
            ),
            card(
              fill = FALSE,
              class = "mb-0",
              card_header("Arquivos do projeto"),
              card_body(
                style = "padding:12px 15px;",
                p(
                  class = "small text-muted",
                  "Baixe o documento final ou o projeto reproduzível para continuar no RStudio."
                ),
                uiOutput(ns("acoes_exportacao"))
              )
            )
          )
        )
      )
    )
  )
}

mod_comunicacao_server <- function(id, dados_analise, import_info,
                                   registro_execucoes_rv,
                                   registro_bases_rv, cache_bases_rv,
                                   revisao_origem_rv, projeto_rv = NULL,
                                   dados_brutos_rv = NULL,
                                   base_resolvida_rv = NULL,
                                   pipeline_rv = NULL,
                                   base_externa_rv = NULL) {
  moduleServer(id, function(input, output, session) {
    estado_editorial_rv <- reactiveVal(comunicacao_estado_vazio())
    observadores_instalados_rv <- reactiveVal(character())
    geracao_projeto_rv <- reactiveVal(1L)

    id_controle <- function(tipo, execucao_id, geracao = geracao_projeto_rv()) {
      sprintf("%s_p%d_%s", tipo, as.integer(geracao), execucao_id)
    }

    # Um novo arquivo bruto inicia outro projeto. Mudanças de preparo dentro do
    # mesmo projeto não limpam os textos nem as decisões editoriais.
    if (!is.null(projeto_rv)) {
      observeEvent(projeto_rv(), {
        geracao_projeto_rv(geracao_projeto_rv() + 1L)
        estado_editorial_rv(comunicacao_estado_vazio())
        for (campo in c("introducao", "metodos", "discussao", "conclusao"))
          updateTextAreaInput(session, campo, value = "")
      }, ignoreInit = TRUE)
    }

    # Novas execuções entram no fim; removidas desaparecem; escolhas existentes
    # sobrevivem às atualizações analíticas enquanto continuarem válidas.
    observe({
      sincronizado <- comunicacao_sincronizar(
        estado_editorial_rv(), registro_execucoes_rv()
      )
      if (!identical(sincronizado, estado_editorial_rv()))
        estado_editorial_rv(sincronizado)
    })

    estados_dependencia <- reactive({
      execucoes <- registro_execucoes_rv()
      estados <- lapply(execucoes, function(execucao) {
        execucoes_estado_dependencia(
          execucao, registro_bases_rv(), cache_bases_rv(), revisao_origem_rv()
        )
      })
      estados
    })

    # Captura checkboxes criados dinamicamente na fila.
    observe({
      estado <- estado_editorial_rv()
      registro <- registro_execucoes_rv()
      geracao <- geracao_projeto_rv()
      novo <- estado
      for (execucao_id in estado$ordem) {
        execucao <- registro[[execucao_id]]
        if (is.null(execucao)) next
        incluir <- input[[id_controle("incluir", execucao_id, geracao)]]
        saidas <- input[[id_controle("saidas", execucao_id, geracao)]]
        if (!is.null(incluir)) {
          novo <- comunicacao_definir_item(
            novo, execucao_id, incluir_word = incluir,
            saidas_disponiveis = execucao$saidas_disponiveis
          )
        }
        if (!is.null(saidas)) {
          novo <- comunicacao_definir_item(
            novo, execucao_id, saidas_selecionadas = saidas,
            saidas_disponiveis = execucao$saidas_disponiveis
          )
        }
      }
      if (!identical(novo, estado)) estado_editorial_rv(novo)
    })

    # Instala uma vez os controles de ordenação de cada ID monotônico.
    observe({
      ids <- names(registro_execucoes_rv())
      geracao <- geracao_projeto_rv()
      chaves <- sprintf("p%d::%s", geracao, ids)
      novos <- setdiff(chaves, observadores_instalados_rv())
      for (chave in novos) {
        local({
          partes <- strsplit(chave, "::", fixed = TRUE)[[1]]
          geracao_local <- as.integer(sub("^p", "", partes[[1]]))
          id_local <- partes[[2]]
          observeEvent(input[[id_controle("subir", id_local, geracao_local)]], {
            estado_editorial_rv(comunicacao_mover(estado_editorial_rv(), id_local, -1L))
          }, ignoreInit = TRUE)
          observeEvent(input[[id_controle("descer", id_local, geracao_local)]], {
            estado_editorial_rv(comunicacao_mover(estado_editorial_rv(), id_local, 1L))
          }, ignoreInit = TRUE)
        })
      }
      if (length(novos))
        observadores_instalados_rv(union(observadores_instalados_rv(), novos))
    })

    observeEvent(input$incluir_todas, {
      for (id_execucao in estado_editorial_rv()$ordem)
        updateCheckboxInput(session, id_controle("incluir", id_execucao), value = TRUE)
    })

    observeEvent(input$excluir_todas, {
      for (id_execucao in estado_editorial_rv()$ordem)
        updateCheckboxInput(session, id_controle("incluir", id_execucao), value = FALSE)
    })

    output$contador <- renderUI({
      estado <- estado_editorial_rv()
      selecionadas <- sum(vapply(
        estado$itens, function(item) isTRUE(item$incluir_word), logical(1)
      ))
      span(
        class = "badge text-bg-info",
        sprintf("%d no Word / %d no Projeto R", selecionadas, length(estado$ordem))
      )
    })

    output$fila <- renderUI({
      registro <- registro_execucoes_rv()
      estado <- estado_editorial_rv()
      dependencias <- estados_dependencia()
      if (!length(estado$ordem)) {
        return(div(
          class = "alert alert-light border text-center",
          icon("bookmark"), br(),
          strong("Nenhuma execução registrada."), br(),
          "Execute uma análise e clique em Adicionar aos resultados."
        ))
      }

      paineis <- lapply(seq_along(estado$ordem), function(indice) {
        execucao_id <- estado$ordem[[indice]]
        execucao <- registro[[execucao_id]]
        item <- estado$itens[[execucao_id]]
        dependencia <- dependencias[[execucao_id]] %||% "Não verificada"
        # O console fica fora da lista: não é conteúdo de relatório.
        rotulos <- comunicacao_rotulos_saidas[
          intersect(execucao$saidas_disponiveis, comunicacao_saidas_relatorio())
        ]
        rotulos <- rotulos[!is.na(rotulos)]
        choices <- stats::setNames(names(rotulos), unname(rotulos))
        accordion_panel(
          value = execucao_id,
          title = div(
            class = "d-flex justify-content-between align-items-center gap-2 w-100 pe-2",
            div(
              span(class = "badge text-bg-secondary", indice), " ",
              strong(execucao$titulo),
              tags$small(
                class = "text-muted ms-2",
                tags$code(execucao_id), " · ", tags$code(execucao$base_objeto)
              )
            ),
            span(class = paste("badge", comunicacao_classe_estado(dependencia)), dependencia)
          ),
          div(
            style = if (isTRUE(item$incluir_word))
              "border-left:4px solid #2E7D8F; padding-left:12px;" else
              "border-left:4px solid #adb5bd; padding-left:12px; opacity:0.82;",
            div(
              class = "small text-muted mb-2",
              "Base utilizada: ", tags$code(execucao$base_objeto)
            ),
            checkboxInput(
              session$ns(id_controle("incluir", execucao_id)),
              "Incluir esta execução no Word", value = isTRUE(item$incluir_word)
            ),
            checkboxGroupInput(
              session$ns(id_controle("saidas", execucao_id)),
              "Conteúdo do Word:", choices = choices,
              selected = item$saidas_selecionadas, inline = TRUE
            ),
            if (isTRUE(item$incluir_word) && !length(item$saidas_selecionadas))
              div(class = "small text-warning mb-2", icon("triangle-exclamation"),
                  " Escolha ao menos um conteúdo ou retire esta execução do Word."),
            div(
              class = "d-flex gap-2 flex-wrap",
              actionButton(
                session$ns(id_controle("subir", execucao_id)), "Subir",
                icon = icon("arrow-up"), class = "btn-sm btn-outline-secondary",
                disabled = if (indice == 1L) "disabled" else NULL
              ),
              actionButton(
                session$ns(id_controle("descer", execucao_id)), "Descer",
                icon = icon("arrow-down"), class = "btn-sm btn-outline-secondary",
                disabled = if (indice == length(estado$ordem)) "disabled" else NULL
              )
            )
          )
        )
      })
      do.call(
        accordion,
        c(
          list(
            id = session$ns("fila_execucoes"),
            open = estado$ordem[[1]],
            multiple = FALSE
          ),
          paineis
        )
      )
    })

    output$acoes_fila <- renderUI({
      if (!length(estado_editorial_rv()$ordem)) return(NULL)
      div(
        class = "d-flex gap-2 flex-wrap mb-3",
        actionButton(session$ns("incluir_todas"), "Incluir todas", icon = icon("check-double"),
                     class = "btn-sm btn-outline-success"),
        actionButton(session$ns("excluir_todas"), "Nenhuma no Word", icon = icon("eye-slash"),
                     class = "btn-sm btn-outline-secondary")
      )
    })

    output$bases_projeto <- renderUI({
      execucoes <- registro_execucoes_rv()
      bases <- registro_bases_rv()
      cache <- cache_bases_rv()
      revisao <- revisao_origem_rv()
      usos <- function(base_id) sum(vapply(
        execucoes,
        function(execucao) identical(execucao$base_id, base_id), logical(1)
      ))
      item_base <- function(nome, objeto, estado, n_usos) {
        classe <- if (estado %in% c("Compartilhada", "Atualizada"))
          "text-bg-success" else "text-bg-warning"
        div(
          class = "border rounded px-2 py-1 mb-1 small",
          div(class = "d-flex justify-content-between gap-2",
              span(strong(nome), br(), tags$code(objeto)),
              span(class = paste("badge align-self-start", classe), estado)),
          tags$small(class = "text-muted", sprintf("%d execução(ões) vinculada(s)", n_usos))
        )
      }
      derivados <- lapply(bases, function(base) {
        estado_cache <- bases_estado_cache(
          base, bases_cache_obter(cache, base$id), revisao
        )
        estado <- if (identical(base$estado, "pronta")) estado_cache else "Em preparo"
        item_base(base$nome_amigavel, base$nome_r, estado, usos(base$id))
      })
      tagList(
        item_base("Base compartilhada", "dados_analise", "Compartilhada", usos("dados_analise")),
        derivados
      )
    })

    secoes_globais <- reactive({
      list(
        introducao = input$introducao %||% "",
        metodos = input$metodos %||% "",
        discussao = input$discussao %||% "",
        conclusao = input$conclusao %||% ""
      )
    })

    manifesto <- reactive({
      comunicacao_manifesto(
        estado_editorial_rv(), registro_execucoes_rv(),
        estados_dependencia(), secoes_globais()
      )
    })

    valor_reativo <- function(x, padrao = NULL) {
      if (is.function(x)) x() else if (is.null(x)) padrao else x
    }

    nome_projeto <- reactive({
      info <- import_info() %||% list()
      nome <- if (identical(info$source, "package")) {
        info$package_dataset
      } else {
        tools::file_path_sans_ext(basename(info$file_name %||% "analise"))
      }
      exportacao_nome_seguro(nome, "analise")
    })

    argumentos_exportacao <- reactive({
      compartilhada <- as.data.frame(dados_analise())
      list(
        nome_projeto = nome_projeto(),
        dados_brutos = as.data.frame(valor_reativo(dados_brutos_rv, compartilhada)),
        base_resolvida = as.data.frame(valor_reativo(base_resolvida_rv, compartilhada)),
        dados_analise = compartilhada,
        pipeline = valor_reativo(pipeline_rv, list()) %||% list(),
        base_externa = valor_reativo(base_externa_rv, NULL),
        registro_bases = registro_bases_rv() %||% list(),
        cache_bases = cache_bases_rv() %||% list(),
        registro_execucoes = registro_execucoes_rv() %||% list(),
        manifesto = manifesto(),
        revisao_origem = revisao_origem_rv(),
        import_info = import_info() %||% list(),
        templates_dir = "templates"
      )
    })

    output$esboco <- renderUI({
      plano <- manifesto()
      incluidas <- Filter(function(x) isTRUE(x$incluir_word), plano$execucoes)
      tagList(
        tags$ol(
          style = "padding-left:20px; font-size:0.88rem; line-height:1.55;",
          tags$li(strong("Preparação dos dados"), " — trilha e bases usadas"),
          tags$li(strong("Introdução")),
          tags$li(strong("Métodos")),
          tags$li(
            strong("Resultados"),
            if (!length(incluidas))
              tags$div(class = "text-warning", "Nenhuma execução selecionada para o Word.")
            else tags$ol(lapply(incluidas, function(item) {
              tags$li(
                item$titulo, " ", tags$code(item$base_objeto), br(),
                tags$small(
                  class = "text-muted",
                  paste(unname(comunicacao_rotulos_saidas[item$saidas_word]), collapse = " · ")
                )
              )
            }))
          ),
          tags$li(strong("Discussão")),
          tags$li(strong("Conclusão"))
        )
      )
    })

    output$manifesto_texto <- renderText({
      plano <- manifesto()
      linhas <- c(
        "# Manifesto editorial — Comunicação de Resultados",
        sprintf("# Projeto R preserva: %d execução(ões)", plano$total_execucoes),
        sprintf("# Relatório Word inclui: %d execução(ões)", plano$total_word),
        ""
      )
      for (i in seq_along(plano$execucoes)) {
        item <- plano$execucoes[[i]]
        linhas <- c(
          linhas,
          sprintf("%d. %s [%s]", i, item$titulo, item$base_objeto),
          sprintf("   Word: %s", if (item$incluir_word) "sim" else "não"),
          sprintf(
            "   Conteúdo: %s",
            if (length(item$saidas_word)) paste(item$saidas_word, collapse = ", ") else "nenhum"
          ),
          sprintf("   Dependência: %s", item$estado_dependencia),
          ""
        )
      }
      paste(linhas, collapse = "\n")
    })

    output$resumo_saida <- renderUI({
      plano <- manifesto()
      pendentes <- sum(vapply(
        plano$execucoes,
        function(x) !identical(x$estado_dependencia, "Atualizada"), logical(1)
      ))
      sem_conteudo <- sum(vapply(
        plano$execucoes,
        function(x) isTRUE(x$incluir_word) && !length(x$saidas_word), logical(1)
      ))
      tagList(
        div(class = "alert alert-info py-2 small",
            strong(plano$total_word), " execução(ões) no Word; ",
            strong(plano$total_execucoes), " preservada(s) no Projeto R."),
        if (pendentes > 0L)
          div(class = "alert alert-warning py-2 small", icon("triangle-exclamation"),
              " ", pendentes, " execução(ões) precisam ser atualizadas antes da exportação.")
        else if (plano$total_execucoes > 0L)
          div(class = "alert alert-success py-2 small", icon("check"),
              " Todas as dependências estão atualizadas."),
        if (sem_conteudo > 0L)
          div(class = "alert alert-warning py-2 small", icon("triangle-exclamation"),
              " ", sem_conteudo, " seção(ões) do Word ainda não têm conteúdo selecionado."),
        div(class = "small text-muted mb-2",
            "O Word segue a seleção editorial; o Projeto R preserva todas as execuções registradas.")
      )
    })

    output$acoes_exportacao <- renderUI({
      plano <- manifesto()
      valida_projeto <- exportacao_validar_manifesto(plano, exigir_word = FALSE)
      valida_word <- exportacao_validar_manifesto(plano, exigir_word = TRUE)
      quarto_ok <- nzchar(unname(Sys.which("quarto")))

      if (!valida_projeto$ok) {
        return(div(
          class = "alert alert-light border small mb-0",
          icon("circle-info"), " ", paste(valida_projeto$mensagens, collapse = " ")
        ))
      }

      tagList(
        if (valida_word$ok && quarto_ok) {
          downloadButton(
            session$ns("baixar_word"), "Baixar Relatório Word (.docx)",
            class = "btn-success w-100 mb-2"
          )
        } else {
          tags$button(
            type = "button", class = "btn btn-outline-secondary w-100 mb-2",
            disabled = "disabled", icon("file-word"),
            " Word indisponível"
          )
        },
        downloadButton(
          session$ns("baixar_projeto"), "Baixar Projeto R (.zip)",
          class = "btn-primary w-100"
        ),
        if (!valida_word$ok)
          div(class = "small text-warning mt-2", paste(valida_word$mensagens, collapse = " ")),
        if (valida_word$ok && !quarto_ok)
          div(class = "small text-warning mt-2",
              "O Projeto R pode ser baixado, mas o Quarto CLI é necessário para gerar o Word.")
      )
    })

    output$baixar_projeto <- downloadHandler(
      filename = function() {
        paste0("projeto_", nome_projeto(), "_", format(Sys.Date(), "%Y-%m-%d"), ".zip")
      },
      content = function(file) {
        do.call(exportacao_empacotar_projeto, c(list(file = file), argumentos_exportacao()))
      }
    )

    output$baixar_word <- downloadHandler(
      filename = function() {
        paste0("relatorio_", nome_projeto(), "_", format(Sys.Date(), "%Y-%m-%d"), ".docx")
      },
      content = function(file) {
        do.call(exportacao_renderizar_word, c(list(file = file), argumentos_exportacao()))
      }
    )

    invisible(list(
      estado_editorial = estado_editorial_rv,
      manifesto = manifesto
    ))
  })
}
