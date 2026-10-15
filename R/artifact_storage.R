# One publication boundary for generated files; labels never become paths.
.genflow_replace_file <- function(staging, path) {
  if (.Platform$OS.type != "windows" || !file.exists(path)) {
    if (!file.rename(staging, path)) stop("Could not publish file: ", path, call. = FALSE)
    return(invisible(path))
  }
  # Windows cannot rename over an existing file. Keep a recoverable old copy
  # until the complete replacement has been published.
  backup <- tempfile(".genflow-backup-", tmpdir = dirname(path))
  if (!file.rename(path, backup)) stop("Could not protect existing file: ", path, call. = FALSE)
  if (!file.rename(staging, path)) {
    restored <- file.rename(backup, path)
    stop("Could not publish replacement.", if (!restored) paste0(" Old file retained at ", backup), call. = FALSE)
  }
  unlink(backup)
  invisible(path)
}

.genflow_artifact_path <- function(directory, label, service, model, extension) {
  if (is.null(directory) || length(directory) != 1L || is.na(directory) || !nzchar(directory)) {
    stop("An artifact directory is required.", call. = FALSE)
  }
  directory <- path.expand(directory)
  if (!dir.exists(directory)) dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(directory)) stop("Could not create artifact directory: ", directory, call. = FALSE)
  extension <- tolower(as.character(extension)[1])
  if (is.na(extension) || !grepl("^[a-z0-9]+$", extension)) stop("Invalid artifact extension.", call. = FALSE)
  parts <- vapply(list(label, service, model), function(x) {
    x <- as.character(x %||% "output")[1]
    if (is.na(x) || !nzchar(x)) x <- "output"
    substr(.sanitize_filename(x), 1L, 48L)
  }, character(1))
  id <- paste(format(openssl::rand_bytes(16L)), collapse = "")
  file.path(normalizePath(directory, winslash = "/", mustWork = TRUE),
            paste0(paste(parts, collapse = "_"), "_", id, ".", extension))
}

.genflow_publish_artifact <- function(path, writer) {
  staging <- tempfile(".genflow-staging-", tmpdir = dirname(path))
  on.exit(unlink(staging), add = TRUE)
  tryCatch({
    writer(staging)
    if (!file.exists(staging) || isTRUE(file.info(staging)$isdir) ||
        is.na(file.info(staging)$size) || file.info(staging)$size == 0) {
      stop("Generated artifact is empty or missing.")
    }
    # The name has a cryptographically random identity. Never replace a
    # pre-existing result, including when this helper is called directly.
    if (file.exists(path)) stop("Refusing to overwrite an existing artifact.")
    if (!file.rename(staging, path)) stop("Could not publish generated artifact.")
    path
  }, error = function(e) {
    stop(structure(list(message = conditionMessage(e), call = NULL),
                   class = c("genflow_persistence_error", "error", "condition")))
  })
}

.genflow_write_artifact <- function(content, directory, label, service, model, extension) {
  writer <- function(staging) {
    if (is.raw(content)) writeBin(content, staging) else writeLines(enc2utf8(content), staging, useBytes = TRUE)
  }
  tryCatch({
    path <- .genflow_artifact_path(directory, label, service, model, extension)
    .genflow_publish_artifact(path, writer)
  }, error = function(e) {
    recovery <- tempfile("genflow-unsaved-", fileext = paste0(".", extension))
    recovered <- tryCatch({ writer(recovery); TRUE }, error = function(e) FALSE)
    stop(structure(list(message = conditionMessage(e), call = NULL,
                        recovery_file = if (recovered) recovery else NULL),
                   class = c("genflow_persistence_error", "error", "condition")))
  })
}

.genflow_copy_artifact <- function(source, directory, label, service, model, extension) {
  path <- .genflow_artifact_path(directory, label, service, model, extension)
  .genflow_publish_artifact(path, function(staging) {
    if (!file.copy(source, staging, overwrite = FALSE)) stop("Could not stage generated artifact.")
  })
}

.genflow_persistence <- function(path = NULL, requested = TRUE, error = NULL, temporary = FALSE) {
  if (!is.null(error)) return(list(status = "failed", path = NULL, message = conditionMessage(error)))
  if (!is.character(path) || length(path) != 1L || is.na(path) || !file.exists(path)) {
    return(list(status = "not_requested", path = NULL))
  }
  connection <- file(path, "rb")
  on.exit(close(connection), add = TRUE)
  list(status = if (temporary || !requested) "temporary" else "saved",
       path = path, bytes = unname(file.info(path)$size),
       sha256 = as.character(openssl::sha256(connection)))
}

.genflow_media_directory <- function(directory, kind, persist) {
  .genflow_validate_flag(persist, "persist")
  if (!persist) return(tempdir())
  directory %||% .genflow_default_dir(kind)
}
