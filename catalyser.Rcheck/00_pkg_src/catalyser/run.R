# Script utilitário para rodar a IDE_R Científica em desenvolvimento
library(shiny)

cat("Iniciando a IDE_R Científica (Desenvolvimento)...\n")
runApp("inst/app", port = 3838, launch.browser = TRUE)

