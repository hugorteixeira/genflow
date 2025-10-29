#' Launch the Genflow agent interface
#'
#' This helper starts the interactive agent management interface shipped with
#' the package. It simply wraps \code{shiny::runApp()} around the internal
#' Shiny application object, making it easy to launch from scripts, the console,
#' or an RStudio addin.
#'
#' @param launch.browser Logical; should the interface open in your default web
#'   browser? Defaults to the value of \code{getOption("shiny.launch.browser")}
#'   or \code{interactive()} when unset.
#' @param ... Additional arguments passed through to \code{shiny::runApp()},
#'   such as \code{port}, \code{host}, or \code{display.mode}.
#'
#' @return Invisibly returns the result of \code{shiny::runApp()}.
#' @examples
#' \dontrun{
#' gen_interface()
#' }
#' @export
gen_interface <- function(launch.browser = getOption("shiny.launch.browser", interactive()), ...) {
  shiny::runApp(genflow_agent_app, launch.browser = launch.browser, ...)
}
