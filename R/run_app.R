#' Run the CatalyseR Shiny Application
#'
#' @param ... arguments passed to runApp
#' @export
#' @importFrom shiny runApp
run_app <- function(...) {
  app_dir <- system.file("app", package = "catalyser")
  if (app_dir == "") {
    stop(
      "N\u00e3o foi poss\u00edvel encontrar o diret\u00f3rio do aplicativo. ",
      "Tente reinstalar o pacote `catalyser`.",
      call. = FALSE
    )
  }
  shiny::runApp(app_dir, ...)
}
