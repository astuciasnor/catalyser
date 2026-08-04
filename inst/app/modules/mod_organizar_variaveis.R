# Criar e Editar Variáveis e Níveis — seleção, nomes, tipos e categorias
# ---------------------------------------------------------------------------
# Centraliza o polimento de colunas que antes aparecia repetido nos módulos de
# importação, Empilhar e Separar. A entrada é a base resolvida atual; o usuário
# prepara uma prévia e decide explicitamente quando promovê-la à Base
# Compartilhada.

library(shiny)
library(bslib)
library(DT)

organizar_variaveis_codigo <- function(renomear = character(0),
                                       tipos = list(),
                                       recodes = list(),
                                       selecionar = NULL) {
  esc <- function(s) gsub("\\", "\\\\", s, fixed = TRUE)
  linhas <- c(
    "# Organização de variáveis — gerada pela CatalyseR",
    "# A entrada desta etapa é a Base Compartilhada ativa.",
    "dados_organizados <- dados"
  )

  if (length(renomear)) {
    pares <- vapply(seq_along(renomear), function(i) {
      sprintf(
        "%s = %s",
        arrumar_bt(unname(renomear[[i]])),
        arrumar_bt(names(renomear)[[i]])
      )
    }, character(1))
    linhas <- c(
      linhas, "",
      "# Renomear variáveis",
      "dados_organizados <- dados_organizados |>",
      paste0("  dplyr::rename(", paste(pares, collapse = ", "), ")")
    )
  }

  if (length(recodes)) {
    linhas <- c(
      linhas, "",
      "# Recodificar categorias",
      "dados_organizados <- dados_organizados |>",
      "  dplyr::mutate("
    )
    colunas <- names(recodes)
    for (ci in seq_along(colunas)) {
      coluna <- colunas[[ci]]
      mapa <- recodes[[coluna]]
      pares <- vapply(seq_along(mapa), function(i) {
        sprintf(
          '      "%s" = "%s"',
          esc(names(mapa)[[i]]),
          esc(unname(mapa[[i]]))
        )
      }, character(1))
      linhas <- c(
        linhas,
        sprintf("    %s = dplyr::recode(%s,", arrumar_bt(coluna), arrumar_bt(coluna)),
        paste(pares, collapse = ",\n"),
        sprintf("    )%s", if (ci < length(colunas)) "," else "")
      )
    }
    linhas <- c(linhas, "  )")
  }

  if (length(tipos)) {
    colunas <- names(tipos)
    pares <- vapply(seq_along(colunas), function(i) {
      coluna <- colunas[[i]]
      funcao <- arrumar_tipo_fun[[tipos[[coluna]]]]
      sprintf(
        "    %s = %s(%s)%s",
        arrumar_bt(coluna), funcao, arrumar_bt(coluna),
        if (i < length(colunas)) "," else ""
      )
    }, character(1))
    linhas <- c(
      linhas, "",
      "# Definir tipos das variáveis",
      "dados_organizados <- dados_organizados |>",
      "  dplyr::mutate(",
      pares,
      "  )"
    )
  }

  if (!is.null(selecionar) && length(selecionar)) {
    linhas <- c(
      linhas, "",
      "# Selecionar as variáveis que permanecerão na base",
      "dados_organizados <- dados_organizados |>",
      paste0(
        "  dplyr::select(",
        paste(arrumar_bt(selecionar), collapse = ", "),
        ")"
      )
    )
  }

  paste(c(linhas, "", "print(dados_organizados)"), collapse = "\n")
}

mod_organizar_variaveis_ui <- function(id, criacao_ui = NULL, somente_checagem = FALSE) {
  ns <- NS(id)

  arrumacao_ui <- layout_columns(
    col_widths = c(3, 7, 2),
    fill = FALSE,
    fillable = FALSE,

    div(
      card(
        fill = FALSE,
        card_header("Edição de Variáveis e Níveis"),
        card_body(
          fill = FALSE,
          fillable = FALSE,
          div(
            class = "alert alert-info",
            style = "font-size:0.83rem; padding:9px 11px;",
            icon("circle-info"),
            " Use este painel depois de importar, pivotar ou separar os dados."
          ),
          div(
            class = "d-grid gap-2",
            actionButton(
              ns("abrir_selecionar"), "Selecionar variáveis",
              icon = icon("list-check"), class = "btn-outline-secondary"
            ),
            actionButton(
              ns("abrir_renomear"), "Renomear variáveis",
              icon = icon("i-cursor"), class = "btn-outline-secondary"
            ),
            actionButton(
              ns("abrir_tipar"), "Definir tipos",
              icon = icon("sliders"), class = "btn-outline-secondary"
            ),
            actionButton(
              ns("abrir_recodificar"), "Recodificar níveis",
              icon = icon("tags"), class = "btn-outline-secondary"
            )
          ),
          helpText("Estas ações organizam colunas e categorias; não filtram linhas.")
        )
      ),
      card(
        fill = FALSE,
        card_header("Ordem aplicada"),
        card_body(
          fill = FALSE,
          style = "font-size:0.8rem; line-height:1.45;",
          tags$ol(
            style = "padding-left:18px; margin-bottom:0;",
            tags$li("Renomear"),
            tags$li("Recodificar"),
            tags$li("Definir tipos"),
            tags$li("Selecionar")
          )
        )
      )
    ),

    navset_card_tab(
      height = "auto",
      nav_panel(
        "Resultado", icon = icon("table"),
        card_body(
          fill = FALSE,
          style = "padding:10px 15px;",
          DTOutput(ns("preview_depois"))
        )
      ),
      nav_panel(
        "Base de entrada", icon = icon("table-list"),
        card_body(
          fill = FALSE,
          style = "padding:10px 15px;",
          DTOutput(ns("preview_antes"))
        )
      ),
      nav_panel(
        "Script gerado", icon = icon("code"),
        card_body(
          fill = FALSE,
          style = "padding:10px 15px;",
          tags$pre(
            style = "white-space:pre-wrap; font-size:0.82rem;",
            verbatimTextOutput(ns("script_preview"))
          )
        )
      )
    ),

    div(
      card(
        fill = FALSE,
        card_header("Adicionar à trilha"),
        card_body(
          fill = FALSE,
          fillable = FALSE,
          uiOutput(ns("status")),
          uiOutput(ns("resumo_acoes")),
          div(
            class = "d-grid gap-2 mt-2",
            actionButton(
              ns("limpar"), "Desfazer organizações",
              icon = icon("rotate-left"), class = "btn-outline-secondary btn-sm"
            ),
            downloadButton(
              ns("baixar_script"), "Baixar script .R",
              class = "btn-outline-secondary btn-sm"
            ),
            downloadButton(
              ns("baixar_dados"), "Baixar dados (.xlsx)",
              class = "btn-outline-primary btn-sm"
            ),
            actionButton(
              ns("usar_base"), "Adicionar Mudança à Trilha da Base Compartilhada",
              icon = icon("share-from-square"), class = "btn-primary"
            )
          ),
          helpText("A Base Compartilhada só muda quando a prévia é adicionada à trilha.")
        )
      )
    )
  )

  clicar <- function(alvo) {
    sprintf(
      "var alvo=document.getElementById('%s'); if(alvo){alvo.click();}",
      ns(alvo)
    )
  }

  checagem_ui <- div(
    id = ns("checagem_layout"),
    class = "catalyser-checagem-final-grid",

    card(
      fill = FALSE,
      card_header("Ajustes finais"),
      card_body(
        fill = FALSE,
        div(
          class = "d-grid gap-2",
          actionButton(
            ns("final_renomear"), "Renomear variáveis",
            icon = icon("i-cursor"), class = "btn-outline-secondary btn-sm",
            onclick = clicar("abrir_renomear")
          ),
          actionButton(
            ns("final_selecionar"), "Selecionar variáveis",
            icon = icon("list-check"), class = "btn-outline-secondary btn-sm",
            onclick = clicar("abrir_selecionar")
          ),
          actionButton(
            ns("final_remover"), "Remover variáveis",
            icon = icon("trash"), class = "btn-outline-secondary btn-sm",
            onclick = clicar("abrir_selecionar")
          ),
          actionButton(
            ns("final_recodificar"), "Recodificar níveis",
            icon = icon("tags"), class = "btn-outline-secondary btn-sm",
            onclick = clicar("abrir_recodificar")
          ),
          actionButton(
            ns("final_tipar"), "Definir tipos",
            icon = icon("sliders"), class = "btn-outline-secondary btn-sm",
            onclick = clicar("abrir_tipar")
          ),
          actionButton(
            ns("final_revisar_categorias"), "Revisar categorias",
            icon = icon("magnifying-glass"), class = "btn-outline-secondary btn-sm",
            onclick = clicar("abrir_recodificar")
          ),
          actionButton(
            ns("final_desfazer"), "Desfazer ajustes",
            icon = icon("rotate-left"), class = "btn-outline-secondary btn-sm",
            onclick = clicar("limpar")
          ),
          downloadButton(
            ns("baixar_dados_final"), "Baixar base final",
            class = "btn-outline-primary btn-sm"
          ),
          actionButton(
            ns("usar_base_final"), "Adicionar Mudança à Trilha da Base Compartilhada",
            icon = icon("share-from-square"), class = "btn-primary btn-sm"
          )
        ),
        hr(style = "margin:10px 0;"),
        uiOutput(ns("status_final")),
        uiOutput(ns("resumo_acoes_final"))
      )
    ),

    card(
      fill = FALSE,
      card_header("Tabela final da Base Compartilhada"),
      card_body(
        fill = FALSE,
        style = "padding:10px 12px;",
        div(
          class = "alert alert-light border",
          style = "font-size:0.8rem; padding:7px 10px;",
          "Use a paginação para muitas linhas e a rolagem horizontal para muitas colunas."
        ),
        DTOutput(ns("preview_final"))
      )
    )
  )

  if (isTRUE(somente_checagem)) {
    return(tagList(
      tags$style(HTML(sprintf(
        paste0(
          "#%s { display:grid; grid-template-columns:minmax(210px,20%%) minmax(0,80%%);",
          "gap:12px; align-items:start; width:100%%; }",
          "@media (max-width: 900px) { #%s { grid-template-columns:1fr; } }"
        ),
        ns("checagem_layout"),
        ns("checagem_layout")
      ))),
      checagem_ui
    ))
  }

  tagList(
    tags$style(HTML(sprintf(
      "#%s > .nav { margin-bottom: 10px; }",
      ns("organizar_subabas")
    ))),
    tabsetPanel(
      id = ns("organizar_subabas"),
      tabPanel(
        "Criação de Variáveis",
        if (is.null(criacao_ui)) {
          div(class = "alert alert-info", "O painel de criação de variáveis não foi configurado.")
        } else {
          criacao_ui
        }
      ),
      tabPanel("Edição de Variáveis e Níveis", arrumacao_ui)
    )
  )
}

mod_organizar_variaveis_checagem_ui <- function(id) {
  mod_organizar_variaveis_ui(id, somente_checagem = TRUE)
}

mod_organizar_variaveis_server <- function(id, data_rv, on_usar = NULL) {
  moduleServer(id, function(input, output, session) {
    base_data <- reactive({
      req(data_rv())
      as.data.frame(data_rv())
    })

    renomear_rv <- reactiveVal(character(0))
    tipos_rv <- reactiveVal(list())
    recodes_rv <- reactiveVal(list())
    selecionar_rv <- reactiveVal(NULL)
    colunas_renomear_rv <- reactiveVal(character(0))
    colunas_tipar_rv <- reactiveVal(character(0))
    niveis_recode_rv <- reactiveVal(character(0))

    limpar_estado <- function() {
      renomear_rv(character(0))
      tipos_rv(list())
      recodes_rv(list())
      selecionar_rv(NULL)
    }

    observeEvent(base_data(), {
      limpar_estado()
    }, ignoreInit = TRUE)

    nomes_renomeados <- reactive({
      nomes <- names(base_data())
      mapa <- renomear_rv()
      if (length(mapa)) {
        for (original in names(mapa)) {
          nomes[nomes == original] <- unname(mapa[[original]])
        }
      }
      nomes
    })

    res_renomeado <- reactive({
      dados <- base_data()
      mapa <- renomear_rv()
      if (length(mapa)) {
        nomes <- names(dados)
        for (original in names(mapa)) {
          nomes[nomes == original] <- unname(mapa[[original]])
        }
        names(dados) <- nomes
      }
      dados
    })

    res_recodificado <- reactive({
      dados <- res_renomeado()
      recodes <- recodes_rv()
      for (coluna in names(recodes)) {
        if (!coluna %in% names(dados)) next
        mapa <- recodes[[coluna]]
        valores <- as.character(dados[[coluna]])
        for (original in names(mapa)) {
          valores[!is.na(valores) & valores == original] <- unname(mapa[[original]])
        }
        dados[[coluna]] <- valores
      }
      dados
    })

    res_tipado <- reactive({
      dados <- res_recodificado()
      tipos <- tipos_rv()
      for (coluna in names(tipos)) {
        if (coluna %in% names(dados)) {
          dados[[coluna]] <- arrumar_converter_tipo(dados[[coluna]], tipos[[coluna]])
        }
      }
      dados
    })

    resultado_final <- reactive({
      dados <- res_tipado()
      selecao <- selecionar_rv()
      if (!is.null(selecao)) {
        manter <- intersect(selecao, names(dados))
        if (length(manter)) dados <- dados[, manter, drop = FALSE]
      }
      dados
    })

    tem_alteracoes <- reactive({
      length(renomear_rv()) > 0 ||
        length(tipos_rv()) > 0 ||
        length(recodes_rv()) > 0 ||
        !is.null(selecionar_rv())
    })

    codigo_rv <- reactive({
      organizar_variaveis_codigo(
        renomear = renomear_rv(),
        tipos = tipos_rv(),
        recodes = recodes_rv(),
        selecionar = selecionar_rv()
      )
    })

    observeEvent(input$abrir_selecionar, {
      nomes <- nomes_renomeados()
      atual <- selecionar_rv()
      if (is.null(atual)) atual <- nomes
      showModal(modalDialog(
        title = "Selecionar variáveis",
        size = "l",
        easyClose = TRUE,
        helpText(
          "Desmarque somente as colunas que não devem permanecer na Base Compartilhada."
        ),
        checkboxGroupInput(
          session$ns("selecionar_check"), NULL,
          choices = nomes, selected = atual
        ),
        footer = tagList(
          actionButton(
            session$ns("selecionar_todas"), "Marcar todas",
            class = "btn btn-sm btn-outline-secondary"
          ),
          modalButton("Cancelar"),
          actionButton(
            session$ns("confirmar_selecionar"), "Aplicar",
            class = "btn-primary"
          )
        )
      ))
    })

    observeEvent(input$selecionar_todas, {
      updateCheckboxGroupInput(
        session, "selecionar_check",
        selected = nomes_renomeados()
      )
    })

    observeEvent(input$confirmar_selecionar, {
      selecao <- input$selecionar_check
      if (is.null(selecao) || !length(selecao)) {
        showNotification("Selecione ao menos uma variável.", type = "error")
        return()
      }
      nomes <- nomes_renomeados()
      selecao <- nomes[nomes %in% selecao]
      selecionar_rv(if (setequal(selecao, nomes)) NULL else selecao)
      removeModal()
      showNotification("Seleção de variáveis atualizada.", type = "message")
    })

    observeEvent(input$abrir_renomear, {
      colunas <- names(base_data())
      colunas_renomear_rv(colunas)
      atuais <- colunas
      mapa <- renomear_rv()
      if (length(mapa)) {
        for (original in names(mapa)) {
          atuais[colunas == original] <- unname(mapa[[original]])
        }
      }
      showModal(modalDialog(
        title = "Renomear variáveis",
        size = "l",
        easyClose = TRUE,
        helpText(
          "Use nomes curtos e informativos. O nome amigável pode ter espaços; o nome no código R fica mais simples com letras, números e underscore."
        ),
        div(
          style = "max-height:430px; overflow-y:auto; padding-right:6px;",
          lapply(seq_along(colunas), function(i) {
            div(
              style = "display:flex; gap:10px; align-items:center; margin-bottom:6px;",
              div(
                style = "flex:1; font-size:0.82rem; color:#666; word-break:break-word;",
                colunas[[i]]
              ),
              div(style = "flex:0 0 20px; text-align:center; color:#aaa;", "→"),
              div(
                style = "flex:1;",
                textInput(
                  session$ns(paste0("renomear_", i)), NULL,
                  value = atuais[[i]], width = "100%"
                )
              )
            )
          })
        ),
        footer = tagList(
          modalButton("Cancelar"),
          actionButton(
            session$ns("confirmar_renomear"), "Aplicar",
            class = "btn-primary"
          )
        )
      ))
    })

    observeEvent(input$confirmar_renomear, {
      colunas <- colunas_renomear_rv()
      req(length(colunas))
      novos <- vapply(seq_along(colunas), function(i) {
        valor <- input[[paste0("renomear_", i)]]
        if (is.null(valor)) colunas[[i]] else trimws(valor)
      }, character(1))
      if (any(!nzchar(novos))) {
        showNotification("Os nomes não podem ficar vazios.", type = "error")
        return()
      }
      if (anyDuplicated(novos)) {
        showNotification("Há nomes de variável duplicados.", type = "error")
        return()
      }
      mudou <- novos != colunas
      renomear_rv(stats::setNames(novos[mudou], colunas[mudou]))
      tipos_rv(list())
      recodes_rv(list())
      selecionar_rv(NULL)
      removeModal()
      showNotification(
        if (any(mudou)) sprintf("%d variável(is) renomeada(s).", sum(mudou))
        else "Nenhum nome foi alterado.",
        type = "message"
      )
    })

    observeEvent(input$abrir_tipar, {
      dados <- res_recodificado()
      colunas <- names(dados)
      colunas_tipar_rv(colunas)
      atuais <- tipos_rv()
      showModal(modalDialog(
        title = "Definir tipos",
        size = "l",
        easyClose = TRUE,
        helpText(
          "O tipo atual já vem selecionado. Altere somente o que for necessário para a análise."
        ),
        div(
          style = "max-height:430px; overflow-y:auto; padding-right:6px;",
          lapply(seq_along(colunas), function(i) {
            tipo <- atuais[[colunas[[i]]]]
            if (is.null(tipo)) tipo <- arrumar_detectar_tipo(dados[[colunas[[i]]]])
            div(
              style = "display:flex; gap:10px; align-items:center; margin-bottom:6px;",
              div(
                style = "flex:1; font-size:0.82rem; color:#666; word-break:break-word;",
                colunas[[i]]
              ),
              div(style = "flex:0 0 20px; text-align:center; color:#aaa;", "→"),
              div(
                style = "flex:1;",
                selectInput(
                  session$ns(paste0("tipo_", i)), NULL,
                  choices = arrumar_tipo_choices,
                  selected = tipo, width = "100%"
                )
              )
            )
          })
        ),
        footer = tagList(
          modalButton("Cancelar"),
          actionButton(
            session$ns("confirmar_tipar"), "Aplicar",
            class = "btn-primary"
          )
        )
      ))
    })

    observeEvent(input$confirmar_tipar, {
      colunas <- colunas_tipar_rv()
      dados <- res_recodificado()
      req(length(colunas))
      novos <- list()
      for (i in seq_along(colunas)) {
        escolhido <- input[[paste0("tipo_", i)]]
        if (is.null(escolhido)) next
        natural <- arrumar_detectar_tipo(dados[[colunas[[i]]]])
        if (!identical(escolhido, natural)) novos[[colunas[[i]]]] <- escolhido
      }
      tipos_rv(novos)
      removeModal()
      showNotification(
        if (length(novos)) sprintf("%d tipo(s) alterado(s).", length(novos))
        else "Nenhum tipo foi alterado.",
        type = "message"
      )
    })

    observeEvent(input$abrir_recodificar, {
      dados <- res_renomeado()
      candidatas <- names(dados)[vapply(
        dados,
        function(x) is.character(x) || is.factor(x),
        logical(1)
      )]
      if (!length(candidatas)) {
        showNotification(
          "Nenhuma variável de texto ou fator está disponível para recodificar.",
          type = "warning"
        )
        return()
      }
      showModal(modalDialog(
        title = "Recodificar categorias",
        size = "l",
        easyClose = TRUE,
        helpText(
          "Escolha uma variável e ajuste categorias equivalentes. Detectar sugere diferenças de caixa, acentos, pontuação e espaços."
        ),
        selectInput(
          session$ns("recode_coluna"), "Variável:",
          choices = candidatas
        ),
        actionButton(
          session$ns("recode_detectar"), "Detectar variações",
          icon = icon("wand-magic-sparkles"),
          class = "btn-outline-primary btn-sm mb-2"
        ),
        uiOutput(session$ns("recode_niveis")),
        footer = tagList(
          modalButton("Cancelar"),
          actionButton(
            session$ns("confirmar_recodificar"), "Aplicar",
            class = "btn-primary"
          )
        )
      ))
    })

    output$recode_niveis <- renderUI({
      req(input$recode_coluna)
      dados <- res_renomeado()
      req(input$recode_coluna %in% names(dados))
      niveis <- sort(unique(as.character(dados[[input$recode_coluna]])))
      niveis <- niveis[!is.na(niveis) & nzchar(niveis)]
      niveis_recode_rv(niveis)
      mapa <- recodes_rv()[[input$recode_coluna]]
      div(
        style = "max-height:340px; overflow-y:auto; padding-right:6px;",
        lapply(seq_along(niveis), function(i) {
          valor <- niveis[[i]]
          if (!is.null(mapa) && niveis[[i]] %in% names(mapa)) {
            valor <- unname(mapa[[niveis[[i]]]])
          }
          div(
            style = "display:flex; gap:10px; align-items:center; margin-bottom:6px;",
            div(
              style = "flex:1; font-size:0.82rem; color:#666; word-break:break-word;",
              niveis[[i]]
            ),
            div(style = "flex:0 0 20px; text-align:center; color:#aaa;", "→"),
            div(
              style = "flex:1;",
              textInput(
                session$ns(paste0("recode_", i)), NULL,
                value = valor, width = "100%"
              )
            )
          )
        })
      )
    })

    observeEvent(input$recode_detectar, {
      req(input$recode_coluna)
      dados <- res_renomeado()
      sugestoes <- arrumar_sugerir_recode(dados[[input$recode_coluna]])
      if (!length(sugestoes)) {
        showNotification(
          "Nenhuma variação óbvia foi encontrada.",
          type = "message"
        )
        return()
      }
      niveis <- niveis_recode_rv()
      for (i in seq_along(niveis)) {
        if (niveis[[i]] %in% names(sugestoes)) {
          updateTextInput(
            session, paste0("recode_", i),
            value = unname(sugestoes[[niveis[[i]]]])
          )
        }
      }
      showNotification(
        sprintf("%d sugestão(ões) aplicada(s) ao formulário.", length(sugestoes)),
        type = "message"
      )
    })

    observeEvent(input$confirmar_recodificar, {
      coluna <- input$recode_coluna
      niveis <- niveis_recode_rv()
      req(coluna, length(niveis))
      novos <- vapply(seq_along(niveis), function(i) {
        valor <- input[[paste0("recode_", i)]]
        if (is.null(valor)) niveis[[i]] else trimws(valor)
      }, character(1))
      mapa <- stats::setNames(novos, niveis)
      mapa <- mapa[nzchar(mapa) & unname(mapa) != names(mapa)]
      recodes <- recodes_rv()
      if (length(mapa)) recodes[[coluna]] <- mapa else recodes[[coluna]] <- NULL
      recodes_rv(recodes)
      removeModal()
      showNotification(
        if (length(mapa)) sprintf("%d categoria(s) recodificada(s).", length(mapa))
        else "Nenhuma categoria foi alterada.",
        type = "message"
      )
    })

    observeEvent(input$limpar, {
      limpar_estado()
      showNotification("As organizações locais foram desfeitas.", type = "message")
    })

    output$preview_antes <- renderDT({
      datatable(
        head(base_data(), 200),
        rownames = FALSE,
        options = list(scrollX = TRUE, pageLength = 12)
      )
    })

    output$preview_depois <- renderDT({
      datatable(
        head(resultado_final(), 200),
        rownames = FALSE,
        options = list(scrollX = TRUE, pageLength = 12)
      )
    })

    output$preview_final <- renderDT({
      datatable(
        resultado_final(),
        rownames = FALSE,
        filter = "top",
        options = list(
          scrollX = TRUE,
          autoWidth = TRUE,
          pageLength = 10,
          lengthMenu = list(c(5, 10, 25, 50, 100), c("5", "10", "25", "50", "100"))
        )
      )
    }, server = TRUE)

    output$script_preview <- renderText(codigo_rv())

    output$status <- renderUI({
      dados <- resultado_final()
      if (!tem_alteracoes()) {
        div(
          style = "font-size:0.84rem; color:#666;",
          icon("circle-info"),
          sprintf(" Nenhuma organização aplicada — %d linhas × %d colunas.",
                  nrow(dados), ncol(dados))
        )
      } else {
        div(
          class = "alert alert-success",
          style = "font-size:0.82rem; padding:8px 10px;",
          icon("check"),
          sprintf(" Prévia organizada: %d linhas × %d colunas.",
                  nrow(dados), ncol(dados))
        )
      }
    })

    output$status_final <- renderUI({
      dados <- resultado_final()
      if (!tem_alteracoes()) {
        div(
          style = "font-size:0.78rem; color:#666;",
          sprintf("Base atual: %d linhas × %d colunas.", nrow(dados), ncol(dados))
        )
      } else {
        div(
          class = "alert alert-success",
          style = "font-size:0.78rem; padding:7px 9px; margin-bottom:6px;",
          sprintf("Prévia final: %d linhas × %d colunas.", nrow(dados), ncol(dados))
        )
      }
    })

    output$resumo_acoes <- renderUI({
      itens <- c(
        if (length(renomear_rv())) sprintf("%d nome(s)", length(renomear_rv())),
        if (length(recodes_rv())) sprintf("%d variável(is) recodificada(s)", length(recodes_rv())),
        if (length(tipos_rv())) sprintf("%d tipo(s)", length(tipos_rv())),
        if (!is.null(selecionar_rv())) sprintf("%d variável(is) mantida(s)", length(selecionar_rv()))
      )
      if (!length(itens)) return(NULL)
      tags$ul(
        style = "font-size:0.78rem; padding-left:18px; margin:8px 0;",
        lapply(itens, tags$li)
      )
    })

    output$resumo_acoes_final <- renderUI({
      itens <- c(
        if (length(renomear_rv())) sprintf("%d nome(s)", length(renomear_rv())),
        if (length(recodes_rv())) sprintf("%d recodificação(ões)", length(recodes_rv())),
        if (length(tipos_rv())) sprintf("%d tipo(s)", length(tipos_rv())),
        if (!is.null(selecionar_rv())) sprintf("%d variável(is) mantida(s)", length(selecionar_rv()))
      )
      if (!length(itens)) return(NULL)
      tags$ul(
        style = "font-size:0.75rem; padding-left:16px; margin:6px 0;",
        lapply(itens, tags$li)
      )
    })

    output$baixar_script <- downloadHandler(
      filename = function() paste0("organizar_variaveis_", Sys.Date(), ".R"),
      content = function(file) writeLines(codigo_rv(), file, useBytes = TRUE)
    )

    output$baixar_dados <- downloadHandler(
      filename = function() paste0("dados_organizados_", Sys.Date(), ".xlsx"),
      content = function(file) writexl::write_xlsx(resultado_final(), file)
    )

    output$baixar_dados_final <- downloadHandler(
      filename = function() paste0("base_compartilhada_final_", Sys.Date(), ".xlsx"),
      content = function(file) writexl::write_xlsx(resultado_final(), file)
    )

    adicionar_trilha <- function() {
      if (!tem_alteracoes()) {
        showNotification(
          "Faça ao menos uma organização antes de adicionar a mudança à trilha.",
          type = "warning"
        )
        return(invisible(FALSE))
      }
      if (is.function(on_usar)) {
        on_usar(
          resultado_final(),
          "Criar e Editar Variáveis e Níveis",
          codigo_rv()
        )
      }
      invisible(TRUE)
    }

    observeEvent(input$usar_base, {
      adicionar_trilha()
    })

    observeEvent(input$usar_base_final, {
      adicionar_trilha()
    })

    invisible(list(
      resultado = resultado_final,
      codigo = codigo_rv,
      alterada = tem_alteracoes
    ))
  })
}
