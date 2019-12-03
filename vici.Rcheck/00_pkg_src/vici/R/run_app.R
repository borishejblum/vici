#' Launch VICI Shiny App
#'
#'@param ... additional arguments to be passed to the \link[shiny]{runApp} function.
#'
#'@examples
#'if(interactive()){
#' vici::run_app()
#'}
#'
#' @export
#' @importFrom shiny runApp
run_app <- function(...) {
  shiny::runApp(system.file("app", package = "vici"),port=3838 ,host = "0.0.0.0")
}

