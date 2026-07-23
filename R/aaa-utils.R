#' Fallback infix operator (internal)
#'
#' @param a Any R object.
#' @param b Value returned when `a` is NULL.
#' @return `a` unless it is NULL, otherwise `b`.
#' @keywords internal
#' @noRd
`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}

.genflow_raw_md5 <- function(value) {
  if (!is.raw(value) || !length(value)) {
    return(NULL)
  }
  path <- tempfile("genflow-hash-")
  on.exit(unlink(path), add = TRUE)
  written <- tryCatch(
    {
      connection <- file(path, open = "wb")
      tryCatch(
        writeBin(value, connection),
        finally = close(connection)
      )
      TRUE
    },
    error = function(e) FALSE
  )
  if (!isTRUE(written) || !file.exists(path)) {
    return(NULL)
  }
  unname(tools::md5sum(path)[[1]])
}

.genflow_regex_escape <- function(value) {
  gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", value, perl = TRUE)
}
