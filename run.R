# Script utilitário para rodar a IDE_R Científica
library(shiny)

cat("Iniciando a IDE_R Científica...\n")
runApp(".", port = 3838, launch.browser = TRUE)
