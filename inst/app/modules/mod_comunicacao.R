# Modulo "Comunicacao de Resultados" — o estudio de montagem do Projeto de
# Comunicacao. CASCA / placeholder (a montagem chega nas proximas etapas).
# A ponte Mouse -> Codigo -> Relatorio. Spec: MODULO_COMUNICACAO_RESULTADOS.md
library(shiny)
library(bslib)

mod_comunicacao_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(style = "padding: 4px 2px 10px;",
      h4("Comunicação de Resultados",
         style = "font-family:'Outfit',sans-serif; font-weight:700; color:#0F3B5F; margin-bottom:2px;"),
      p(style = "color:#495057; font-size:0.9rem; margin:0;",
        "O estúdio de montagem do ", strong("Projeto de Comunicação"),
        ": a ponte entre o ", strong("mouse"), " (as análises que você fez), o ",
        strong("código"), " que a IDE gera, e o ", strong("relatório"),
        " final — que sai como projeto (.docx no tema Ocean + Projeto R .zip).")
    ),
    layout_columns(
      col_widths = c(1, 1, 1),
      style = "grid-template-columns: 3fr 6fr 3fr !important;",

      card(
        card_header("1. Fila do relatório"),
        card_body(style = "padding:12px 15px;",
          uiOutput(ns("fila")),
          hr(style = "margin:10px 0;"),
          helpText("Cada análise ganhará um botão \"Adicionar ao relatório\". Aqui você reordena e remove; a Seção 0 (Preparação) vem da Trilha de Preparo.")
        )
      ),

      navset_card_tab(
        nav_panel(title = "Esboço do documento", icon = icon("list-ol"),
          card_body(style = "padding:12px 15px;", uiOutput(ns("esboco")))
        ),
        nav_panel(title = "Código gerado", icon = icon("code"),
          card_body(style = "padding:12px 15px;",
            tags$pre(style = "white-space:pre-wrap; font-size:0.82rem; color:#6c757d;",
                     "# O script R integrado aparecerá aqui\n# (Seção 0: preparação -> análises, na ordem da fila)."))
        )
      ),

      div(
        card(
          card_header("2. Saída"),
          card_body(style = "padding:12px 15px;",
            radioButtons(ns("formato"), "Formato:",
              choices = c("Word (.docx) — tema Ocean" = "docx",
                          "Projeto R (.zip) — para o aluno" = "zip"),
              selected = "docx"),
            actionButton(ns("gerar"), "Gerar projeto de comunicação",
                         icon = icon("wand-magic-sparkles"), class = "btn-primary w-100"),
            div(class = "alert alert-info", style = "margin-top:10px; padding:8px 10px; font-size:0.82rem;",
              icon("hammer"),
              " Em construção: a montagem (fila, esboço, código e geração) chega nas próximas etapas. Ver ",
              tags$code("MODULO_COMUNICACAO_RESULTADOS.md"), ".")
          )
        ),
        card(
          card_header("A ponte, em 3 estados"),
          card_body(style = "padding:12px 15px; font-size:0.82rem; line-height:1.5;",
            tags$ol(style = "padding-left:18px; margin:0;",
              tags$li(strong("Mouse:"), " você fez as análises clicando."),
              tags$li(strong("Código:"), " a IDE gera o R equivalente."),
              tags$li(strong("Relatório:"), " tudo vira um documento/projeto.")))
        )
      )
    )
  )
}

mod_comunicacao_server <- function(id, dados_analise, import_info) {
  moduleServer(id, function(input, output, session) {
    output$fila <- renderUI({
      div(style = "color:#888; font-size:0.85rem; padding:8px 0;",
          "Nenhuma análise na fila ainda.")
    })
    output$esboco <- renderUI({
      tags$ol(style = "padding-left:18px; color:#6c757d; font-size:0.88rem;",
        tags$li("Seção 0 — Preparação dos dados (da Trilha)"),
        tags$li("Introdução / Métodos"),
        tags$li("Resultados — uma seção por análise da fila"),
        tags$li("Discussão / Conclusão"))
    })
    observeEvent(input$gerar, {
      showNotification("Comunicação de Resultados: montagem em construção (casca da Fase 0).",
                       type = "message", duration = 5)
    })
  })
}
