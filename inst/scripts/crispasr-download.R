args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) {
  stop("Expected spec and status paths.", call. = FALSE)
}

spec_path <- normalizePath(args[[1]], winslash = "/", mustWork = TRUE)
status_path <- normalizePath(args[[2]], winslash = "/", mustWork = FALSE)
spec <- readRDS(spec_path)
library_paths <- if (is.null(spec$library_paths)) {
  character()
} else {
  unlist(spec$library_paths, use.names = FALSE)
}
library_paths <- as.character(library_paths)
library_paths <- library_paths[nzchar(library_paths) & dir.exists(library_paths)]
if (length(library_paths)) {
  .libPaths(unique(c(library_paths, .libPaths())))
}

source_root <- if (is.null(spec$source_root)) {
  ""
} else {
  trimws(as.character(spec$source_root)[1])
}
if (nzchar(source_root) &&
    file.exists(file.path(source_root, "DESCRIPTION")) &&
    requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(source_root, quiet = TRUE)
} else {
  suppressPackageStartupMessages(library(genflow))
}

worker <- getFromNamespace(
  ".genflow_native_download_job_worker",
  "genflow"
)
ok <- isTRUE(worker(spec_path, status_path))
quit(save = "no", status = if (ok) 0L else 1L, runLast = FALSE)
