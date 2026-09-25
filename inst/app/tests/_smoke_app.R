options(shiny.autoreload = FALSE)
app <- shiny::shinyAppFile("app.R")
srv <- shiny::runApp(app, port = 8123, launch.browser = FALSE)
