# Modulo "Laboratorio de Conceitos" (CASCA) — visualizadores pedagogicos.
# So a lista de funcionalidades por enquanto; cada visualizador entra depois,
# como modulo interativo proprio. Spec: MODULO_LABORATORIO_CONCEITOS.md
library(shiny)
library(bslib)

# Card placeholder reutilizavel para um visualizador "em construcao".
lab_placeholder <- function(titulo, desc, icone = "flask") {
  card(
    card_header(tagList(icon(icone), " ", titulo)),
    card_body(
      div(class = "alert alert-info", style = "margin-bottom:10px;",
          icon("hammer"), " Visualizador em construção."),
      p(desc),
      helpText("Entrará como módulo interativo: sliders + gráfico reativo no tema Ocean, sem depender de dados importados.")
    )
  )
}

# Painel "Visao geral": a lista de funcionalidades planejadas do laboratorio.
mod_laboratorio_ui <- function(id) {
  ns <- NS(id)
  itens <- list(
    c("Teorema do Limite Central (TLC)", "Médias de qualquer distribuição viram um sino conforme n cresce."),
    c("Lei dos Grandes Números", "A média amostral converge para o valor esperado à medida que n aumenta."),
    c("Cobertura do Intervalo de Confiança", "100 amostras, 100 intervalos: ~95% contêm a média verdadeira."),
    c("Distribuição sob H0 / p-valor", "A distribuição nula, a estatística observada e a área do p-valor."),
    c("Curvas z / t / F / qui-quadrado", "Forma das curvas × graus de liberdade; áreas e quantis."),
    c("Distribuições (Normal, Binomial)", "Explorar forma, parâmetros e áreas das distribuições clássicas.")
  )
  cartoes <- lapply(itens, function(it) {
    div(class = "card", style = "margin-bottom:8px;",
      div(class = "card-body", style = "padding:10px 14px;",
        tags$b(it[1]),
        tags$span(class = "badge bg-secondary", style = "margin-left:8px;", "em construção"),
        tags$p(style = "margin:4px 0 0; font-size:0.88rem; color:#495057;", it[2])))
  })
  tagList(
    div(style = "padding: 6px 2px 12px;",
      h4("Laboratório de Conceitos",
         style = "font-family:'Outfit',sans-serif; font-weight:700; color:#0F3B5F; margin-bottom:2px;"),
      p(style = "color:#495057; font-size:0.92rem; margin:0;",
        "Um ", strong("laboratório visual"), " para o professor mostrar e o aluno ",
        strong("ver"), " os fundamentos da estatística — sem depender de dados importados. ",
        "Abaixo, as funcionalidades planejadas (cada uma vira um menu ao lado)."),
      tags$hr(style = "margin:12px 0 10px;")
    ),
    div(cartoes)
  )
}

mod_laboratorio_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Casca: sem lógica ainda. Os visualizadores entram um a um (TLC primeiro).
  })
}
