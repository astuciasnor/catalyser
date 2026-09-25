# Teste focal da coleção de gráficos por intenção.
# Execute a partir de inst/app: Rscript tests/test_exploracao_visual.R

Sys.setlocale("LC_CTYPE", "pt_BR.UTF-8")
source("app.R", encoding = "UTF-8")

# A segunda série deve ir para o primeiro eixo e voltar à sua escala original.
escala <- reescalar_para_eixo(y2 = c(10, 20, 30), y1 = c(100, 150, 200))
stopifnot(
  isTRUE(all.equal(escala$y2_no_eixo_esquerdo, c(100, 150, 200))),
  isTRUE(all.equal((escala$y2_no_eixo_esquerdo - escala$b) / escala$a, c(10, 20, 30)))
)

# As oito intenções devem possuir uma tela própria e informar o código ao aluno.
tipos <- c("histograma", "caixa_violino", "dispersao", "duplo_eixo", "barras", "rosca", "matriz", "calor")
html <- vapply(tipos, function(tipo) htmltools::renderTags(mod_exploracao_visual_ui(paste0("teste_", tipo), tipo))$html, character(1))
stopifnot(all(grepl("Ver o código R", html, fixed = TRUE)))

# O menu visual vem logo após a exploração dos dados e antes das frequências.
codigo_app <- paste(readLines("app.R", encoding = "UTF-8"), collapse = "\n")
pos_explorar <- regexpr('title = HTML("Explorando<br>os Dados")', codigo_app, fixed = TRUE)[[1]]
pos_visual <- regexpr('title = HTML("Visualização<br>dos Dados")', codigo_app, fixed = TRUE)[[1]]
pos_frequencias <- regexpr('title = HTML("Frequências<br>e Proporções")', codigo_app, fixed = TRUE)[[1]]
stopifnot(pos_explorar > 0, pos_visual > pos_explorar, pos_frequencias > pos_visual)

cat("OK: coleção visual, reescalonamento e posição do menu validados.\n")
