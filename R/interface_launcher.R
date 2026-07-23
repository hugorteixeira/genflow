.genflow_validate_interface_host <- function(host, allow_remote = FALSE) {
  host <- tolower(trimws(as.character(host %||% "")[1]))
  if (is.na(host) || !nzchar(host)) {
    stop("`host` must be a non-empty bind address.", call. = FALSE)
  }
  loopback <- host %in% c("127.0.0.1", "localhost", "::1")
  if (!loopback && !isTRUE(allow_remote)) {
    stop(
      "Refusing to expose the genflow interface on non-loopback host '",
      host,
      "'. It can manage credentials and read local paths. Set ",
      "`allow_remote = TRUE` only behind access controls you operate.",
      call. = FALSE
    )
  }
  if (!loopback) {
    warning(
      "The genflow interface is being exposed on a non-loopback address. ",
      "genflow does not provide authentication; use a secured reverse proxy ",
      "and network policy.",
      call. = FALSE
    )
  }
  host
}

#' Launch the Genflow agent interface
#'
#' This helper starts the interactive agent management interface shipped with
#' the package. It simply wraps \code{shiny::runApp()} around the internal
#' Shiny application object, making it easy to launch from scripts, the console,
#' or an RStudio addin.
#'
#' The Models tab includes credential management for provider API keys/tokens,
#' optional base URL overrides, model catalog updates, and custom provider
#' definitions. Credential edits are written to the user \code{.Renviron},
#' backed up first, and loaded into the current R session immediately. Model
#' selection remains part of setup/agent configuration after a provider catalog
#' is refreshed.
#'
#' @param launch.browser Logical; should the interface open in your default web
#'   browser? Defaults to the value of \code{getOption("shiny.launch.browser")}
#'   or \code{interactive()} when unset.
#' @param host Interface bind address. Defaults to the IPv4 loopback address.
#' @param allow_remote Logical; explicitly acknowledge the security risk of
#'   binding to a non-loopback address. The interface can read local paths and
#'   manage credentials, so genflow does not expose it remotely by default.
#' @param ... Additional arguments passed through to \code{shiny::runApp()},
#'   such as \code{port} or \code{display.mode}.
#'
#' @return Invisibly returns the result of \code{shiny::runApp()}.
#' @examples
#' \dontrun{
#' gen_interface()
#' }
#' @export
gen_interface <- function(
  launch.browser = getOption("shiny.launch.browser", interactive()),
  host = "127.0.0.1",
  allow_remote = FALSE,
  ...
) {
  host <- .genflow_validate_interface_host(host, allow_remote)
  shiny::runApp(
    genflow_agent_app,
    launch.browser = launch.browser,
    host = host,
    ...
  )
}
