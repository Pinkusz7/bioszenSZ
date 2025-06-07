#' Arranca la aplicación Shiny
#' @export
run_app <- function(...) {
  shiny::runApp(system.file("app", package = "MiApp"), ...)
}
