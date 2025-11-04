#' Locate the models directory used by genflow
#'
#' When `models_dir` is `NULL`, this helper resolves the directory used to
#' persist downloaded model catalogs. It prefers the current option
#' `genflow.models_dir` when set, otherwise falls back to the user data
#' directory employed across the package.
#'
#' @param models_dir Optional explicit directory supplied by the caller.
#' @return Normalized path to the models directory (may not exist yet).
#' @keywords internal
#' @noRd
.genflow_resolve_models_dir <- function(models_dir = NULL) {
  if (!is.null(models_dir) && nzchar(models_dir)) {
    return(normalizePath(models_dir, winslash = "/", mustWork = FALSE))
  }
  option_path <- getOption("genflow.models_dir")
  if (!is.null(option_path) && nzchar(option_path)) {
    return(normalizePath(option_path, winslash = "/", mustWork = FALSE))
  }
  normalizePath(tools::R_user_dir("agent_models", which = "data"), winslash = "/", mustWork = FALSE)
}

#' Internal utility to copy a directory tree
#'
#' Copies every file found under `src` into the destination directory while
#' preserving the relative structure. When `overwrite` is `FALSE`, the function
#' aborts if any destination file already exists.
#'
#' @param src Source directory.
#' @param dest Destination directory.
#' @param overwrite Whether to overwrite existing files at the destination.
#' @return List with fields `files` (number of copied files) and `paths`
#'   (relative paths of copied files).
#' @keywords internal
#' @noRd
.genflow_copy_tree <- function(src, dest, overwrite = TRUE) {
  if (!dir.exists(src)) {
    return(list(files = 0L, paths = character()))
  }
  all_entries <- list.files(src, all.files = TRUE, recursive = TRUE, include.dirs = TRUE, no.. = TRUE)
  if (!length(all_entries)) {
    dir.create(dest, recursive = TRUE, showWarnings = FALSE)
    return(list(files = 0L, paths = character()))
  }

  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  src_full <- file.path(src, all_entries)
  rel_paths <- all_entries
  info <- file.info(src_full, extra_cols = FALSE)
  is_dir <- info$isdir %||% rep(FALSE, length(src_full))

  if (any(is_dir)) {
    dir_rel <- rel_paths[is_dir]
    dir_rel <- dir_rel[dir_rel != ""]
    if (length(dir_rel)) {
      for (d in dir_rel) {
        dir.create(file.path(dest, d), recursive = TRUE, showWarnings = FALSE)
      }
    }
  }

  file_idx <- which(!is_dir)
  if (!length(file_idx)) {
    return(list(files = 0L, paths = character()))
  }

  copied_paths <- character(length(file_idx))
  for (i in seq_along(file_idx)) {
    idx <- file_idx[[i]]
    from <- src_full[[idx]]
    rel <- rel_paths[[idx]]
    target <- file.path(dest, rel)
    parent_dir <- dirname(target)
    if (!dir.exists(parent_dir)) {
      dir.create(parent_dir, recursive = TRUE, showWarnings = FALSE)
    }
    if (!overwrite && file.exists(target)) {
      stop(
        sprintf("Destination file '%s' already exists. Enable overwrite to replace it.", target),
        call. = FALSE
      )
    }
    success <- file.copy(
      from = from,
      to = target,
      overwrite = overwrite,
      copy.date = TRUE,
      copy.mode = TRUE
    )
    if (!isTRUE(success)) {
      stop(sprintf("Failed to copy '%s' to '%s'.", from, target), call. = FALSE)
    }
    copied_paths[[i]] <- rel
  }

  list(files = length(file_idx), paths = copied_paths)
}

#' Count matching files in a directory
#'
#' @param dir Directory to scan.
#' @param pattern Optional regular expression to filter files.
#' @return Integer count of matching files.
#' @keywords internal
#' @noRd
.genflow_count_files <- function(dir, pattern = NULL) {
  if (!dir.exists(dir)) {
    return(0L)
  }
  files <- list.files(dir, pattern = pattern, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  length(files)
}

#' Export a genflow resource bundle
#'
#' Creates a portable `.zip` archive containing cached setups, agents, content,
#' and downloaded model catalogs so a workspace can be replicated on another
#' machine.
#'
#' @param path Output zip file path. When missing, a timestamped file in
#'   `tempdir()` is created.
#' @param include_setups,include_agents,include_content,include_models Logical
#'   flags controlling which components are exported.
#' @param models_dir Optional models directory to bundle (defaults to the active
#'   models directory).
#' @param overwrite Overwrite the destination file when it already exists.
#' @param quiet Suppress informational messages.
#'
#' @return Invisibly returns a list with the resolved `path`, `counts`,
#'   `includes`, and `metadata`.
#' @export
gen_export_bundle <- function(path,
                              include_setups = TRUE,
                              include_agents = TRUE,
                              include_content = TRUE,
                              include_models = TRUE,
                              models_dir = NULL,
                              overwrite = FALSE,
                              quiet = FALSE) {
  include_flags <- c(
    setups = isTRUE(include_setups),
    agents = isTRUE(include_agents),
    content = isTRUE(include_content),
    models = isTRUE(include_models)
  )
  if (!any(include_flags)) {
    stop("Select at least one component to export.", call. = FALSE)
  }

  if (missing(path) || is.null(path) || !nzchar(path)) {
    path <- file.path(tempdir(), paste0("genflow_bundle_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip"))
  }
  file_ext <- tolower(tools::file_ext(path))
  if (!nzchar(file_ext) || file_ext != "zip") {
    path <- paste0(path, ".zip")
  }
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)

  if (file.exists(path)) {
    if (!isTRUE(overwrite)) {
      stop(sprintf("File '%s' already exists. Set overwrite = TRUE to replace it.", path), call. = FALSE)
    }
    unlink(path)
  }

  dest_dir <- dirname(path)
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)

  staging_root <- tempfile("genflow_export_")
  dir.create(staging_root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(staging_root, recursive = TRUE, force = TRUE), add = TRUE)

  bundle_root_name <- "genflow_bundle"
  bundle_root <- file.path(staging_root, bundle_root_name)
  dir.create(bundle_root, showWarnings = FALSE)

  cache_dir <- normalizePath(.genflow_cache_dir(), winslash = "/", mustWork = TRUE)
  models_dir_resolved <- .genflow_resolve_models_dir(models_dir)

  counts <- list(
    setups = 0L,
    agents = 0L,
    content = 0L,
    models = 0L
  )

  if (include_flags[["setups"]]) {
    src <- file.path(cache_dir, "setups")
    counts$setups <- .genflow_count_files(src, pattern = "\\.rds$")
    if (counts$setups > 0L) {
      .genflow_copy_tree(src, file.path(bundle_root, "cache", "setups"), overwrite = TRUE)
    }
  }

  if (include_flags[["agents"]]) {
    src <- file.path(cache_dir, "agents")
    counts$agents <- .genflow_count_files(src, pattern = "\\.rds$")
    if (counts$agents > 0L) {
      .genflow_copy_tree(src, file.path(bundle_root, "cache", "agents"), overwrite = TRUE)
    }
  }

  if (include_flags[["content"]]) {
    src <- file.path(cache_dir, "content")
    counts$content <- .genflow_count_files(src, pattern = "\\.rds$")
    if (counts$content > 0L) {
      .genflow_copy_tree(src, file.path(bundle_root, "cache", "content"), overwrite = TRUE)
    }
  }

  if (include_flags[["models"]]) {
    if (dir.exists(models_dir_resolved)) {
      copy_result <- .genflow_copy_tree(models_dir_resolved, file.path(bundle_root, "models"), overwrite = TRUE)
      counts$models <- as.integer(copy_result$files)
    } else {
      counts$models <- 0L
    }
  }

  metadata <- list(
    package_version = tryCatch(as.character(utils::packageVersion("genflow")), error = function(e) NA_character_),
    exported_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    includes = as.list(include_flags),
    counts = counts,
    source_paths = list(
      cache_dir = cache_dir,
      models_dir = models_dir_resolved
    )
  )
  jsonlite::write_json(metadata, file.path(bundle_root, "metadata.json"), auto_unbox = TRUE, pretty = TRUE, digits = NA)

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(staging_root)
  zip_result <- utils::zip(zipfile = path, files = bundle_root_name, flags = "-r9Xq")
  if (!identical(zip_result, 0L)) {
    stop("Failed to create export archive with utils::zip().", call. = FALSE)
  }
  setwd(old_wd)

  if (!quiet) {
    message(sprintf("genflow bundle exported to %s", path))
  }

  invisible(list(
    path = normalizePath(path, winslash = "/", mustWork = TRUE),
    counts = counts,
    includes = as.list(include_flags),
    metadata = metadata
  ))
}

#' Import a genflow resource bundle
#'
#' Restores cached setups, agents, content, and models from a bundle generated
#' by [gen_export_bundle()].
#'
#' @param path Path to the bundle zip file.
#' @param include_setups,include_agents,include_content,include_models Logical
#'   flags indicating which components should be imported.
#' @param models_dir Optional models directory to receive imported files.
#' @param overwrite Overwrite existing files if they already exist.
#' @param quiet Suppress informational messages.
#'
#' @return Invisibly returns a list with `counts`, `includes`, `metadata`, and
#'   the resolved `paths`.
#' @export
gen_import_bundle <- function(path,
                              include_setups = TRUE,
                              include_agents = TRUE,
                              include_content = TRUE,
                              include_models = TRUE,
                              models_dir = NULL,
                              overwrite = FALSE,
                              quiet = FALSE) {
  if (missing(path) || is.null(path) || !file.exists(path)) {
    stop("Provide a valid path to a genflow bundle (.zip).", call. = FALSE)
  }

  include_flags <- c(
    setups = isTRUE(include_setups),
    agents = isTRUE(include_agents),
    content = isTRUE(include_content),
    models = isTRUE(include_models)
  )
  if (!any(include_flags)) {
    stop("Select at least one component to import.", call. = FALSE)
  }

  tmp_dir <- tempfile("genflow_import_")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  utils::unzip(path, exdir = tmp_dir)

  candidate_root <- file.path(tmp_dir, "genflow_bundle")
  if (dir.exists(candidate_root)) {
    bundle_root <- candidate_root
  } else {
    top_entries <- list.files(tmp_dir, full.names = TRUE)
    dirs <- top_entries[file.info(top_entries, extra_cols = FALSE)$isdir]
    if (length(dirs) != 1L) {
      stop("Could not locate the bundle root inside the archive.", call. = FALSE)
    }
    bundle_root <- dirs[[1]]
  }

  cache_dir <- normalizePath(.genflow_cache_dir(), winslash = "/", mustWork = TRUE)
  models_dir_resolved <- .genflow_resolve_models_dir(models_dir)

  counts <- list(
    setups = 0L,
    agents = 0L,
    content = 0L,
    models = 0L
  )

  if (include_flags[["setups"]]) {
    src <- file.path(bundle_root, "cache", "setups")
    counts$setups <- .genflow_count_files(src, pattern = "\\.rds$")
    if (counts$setups > 0L) {
      .genflow_copy_tree(src, file.path(cache_dir, "setups"), overwrite = overwrite)
    }
  }

  if (include_flags[["agents"]]) {
    src <- file.path(bundle_root, "cache", "agents")
    counts$agents <- .genflow_count_files(src, pattern = "\\.rds$")
    if (counts$agents > 0L) {
      .genflow_copy_tree(src, file.path(cache_dir, "agents"), overwrite = overwrite)
    }
  }

  if (include_flags[["content"]]) {
    src <- file.path(bundle_root, "cache", "content")
    counts$content <- .genflow_count_files(src, pattern = "\\.rds$")
    if (counts$content > 0L) {
      .genflow_copy_tree(src, file.path(cache_dir, "content"), overwrite = overwrite)
    }
  }

  if (include_flags[["models"]]) {
    src <- file.path(bundle_root, "models")
    if (dir.exists(src)) {
      copy_result <- .genflow_copy_tree(src, models_dir_resolved, overwrite = overwrite)
      counts$models <- as.integer(copy_result$files)
    } else {
      counts$models <- 0L
    }
  }

  metadata_path <- file.path(bundle_root, "metadata.json")
  metadata <- if (file.exists(metadata_path)) {
    tryCatch(jsonlite::fromJSON(metadata_path, simplifyVector = TRUE), error = function(e) NULL)
  } else {
    NULL
  }

  if (!quiet) {
    message(sprintf("genflow bundle imported from %s", path))
  }

  invisible(list(
    path = normalizePath(path, winslash = "/", mustWork = TRUE),
    counts = counts,
    includes = as.list(include_flags),
    metadata = metadata,
    destinations = list(
      cache_dir = cache_dir,
      models_dir = models_dir_resolved
    )
  ))
}

