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

.genflow_validate_zip_entries <- function(entries,
                                          max_entries = 10000L,
                                          max_file_bytes = 128 * 1024^2,
                                          max_bundle_bytes = 512 * 1024^2) {
  limits <- c(
    max_entries = suppressWarnings(as.numeric(max_entries)[1]),
    max_file_bytes = suppressWarnings(as.numeric(max_file_bytes)[1]),
    max_bundle_bytes = suppressWarnings(as.numeric(max_bundle_bytes)[1])
  )
  if (anyNA(limits) || any(!is.finite(limits)) || any(limits <= 0)) {
    stop("Bundle safety limits must be positive finite numbers.", call. = FALSE)
  }
  max_entries <- limits[["max_entries"]]
  max_file_bytes <- limits[["max_file_bytes"]]
  max_bundle_bytes <- limits[["max_bundle_bytes"]]

  if (!is.data.frame(entries) || !all(c("Name", "Length") %in% names(entries))) {
    stop("Could not read the bundle archive directory.", call. = FALSE)
  }
  if (!nrow(entries)) {
    stop("The bundle archive is empty.", call. = FALSE)
  }
  if (nrow(entries) > max_entries) {
    stop(
      "The bundle contains too many entries (", nrow(entries), ").",
      call. = FALSE
    )
  }

  archive_names <- as.character(entries$Name)
  lengths <- suppressWarnings(as.numeric(entries$Length))
  if (anyNA(archive_names) || anyNA(lengths) || any(lengths < 0)) {
    stop("The bundle archive has an invalid directory entry.", call. = FALSE)
  }
  if (any(grepl(intToUtf8(92L), archive_names, fixed = TRUE))) {
    stop("Bundle paths must use forward slashes.", call. = FALSE)
  }
  normalized <- sub("^\\./+", "", archive_names)
  normalized <- sub("/+$", "", normalized)
  path_parts <- strsplit(normalized, "/", fixed = TRUE)
  invalid_path <- vapply(seq_along(normalized), function(i) {
    path <- normalized[[i]]
    parts <- path_parts[[i]]
    !nzchar(path) ||
      startsWith(path, "/") ||
      grepl("^[A-Za-z]:", path) ||
      grepl("[[:cntrl:]]", path) ||
      any(parts %in% c("", ".", ".."))
  }, logical(1))
  if (any(invalid_path)) {
    stop(
      "Unsafe path in bundle archive: ",
      archive_names[[which(invalid_path)[1]]],
      call. = FALSE
    )
  }
  if (anyDuplicated(normalized)) {
    stop("The bundle archive contains duplicate paths.", call. = FALSE)
  }

  roots <- unique(vapply(path_parts, `[[`, character(1), 1L))
  if (length(roots) != 1L) {
    stop("The bundle archive must contain exactly one root directory.", call. = FALSE)
  }
  root <- roots[[1]]
  root_pattern <- .genflow_regex_escape(root)
  is_directory <- grepl("/$", archive_names)
  file_paths <- normalized[!is_directory]
  file_lengths <- lengths[!is_directory]
  allowed <- grepl(
    paste0(
      "^", root_pattern,
      "/(?:metadata\\.json|",
      "cache/(?:setups|agents|content)/[^/]+\\.rds|",
      "models/(?:[^/]+/)*[^/]+\\.(?:csv|json|rds))$"
    ),
    file_paths,
    perl = TRUE,
    ignore.case = TRUE
  )
  if (any(!allowed)) {
    stop(
      "Unexpected file in bundle archive: ",
      file_paths[[which(!allowed)[1]]],
      call. = FALSE
    )
  }
  if (any(file_lengths > max_file_bytes)) {
    stop(
      "A bundle entry exceeds the per-file size limit: ",
      file_paths[[which(file_lengths > max_file_bytes)[1]]],
      call. = FALSE
    )
  }
  if (sum(file_lengths) > max_bundle_bytes) {
    stop("The expanded bundle exceeds the allowed size.", call. = FALSE)
  }
  metadata_name <- paste0(root, "/metadata.json")
  if (!metadata_name %in% file_paths) {
    stop("The bundle is missing metadata.json.", call. = FALSE)
  }

  list(
    names = normalized,
    original_names = archive_names,
    lengths = lengths,
    is_directory = is_directory,
    root = root
  )
}

.genflow_safe_extract_zip <- function(path,
                                      destination,
                                      max_entries = 10000L,
                                      max_file_bytes = 128 * 1024^2,
                                      max_bundle_bytes = 512 * 1024^2) {
  entries <- tryCatch(
    utils::unzip(path, list = TRUE),
    error = function(e) {
      stop("Could not inspect bundle archive: ", conditionMessage(e), call. = FALSE)
    }
  )
  validated <- .genflow_validate_zip_entries(
    entries,
    max_entries = max_entries,
    max_file_bytes = max_file_bytes,
    max_bundle_bytes = max_bundle_bytes
  )
  dir.create(destination, recursive = TRUE, showWarnings = FALSE)

  for (index in seq_len(nrow(entries))) {
    if (validated$is_directory[[index]]) {
      next
    }
    relative <- validated$names[[index]]
    target <- do.call(
      file.path,
      as.list(c(destination, strsplit(relative, "/", fixed = TRUE)[[1]]))
    )
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)

    input <- unz(path, validated$original_names[[index]], open = "rb")
    output <- file(target, open = "wb")
    bytes_written <- 0
    copy_error <- NULL
    tryCatch(
      {
        repeat {
          chunk <- readBin(input, what = "raw", n = 64L * 1024L)
          if (!length(chunk)) {
            break
          }
          bytes_written <- bytes_written + length(chunk)
          if (bytes_written > max_file_bytes) {
            stop("Expanded entry exceeded its size limit.")
          }
          writeBin(chunk, output)
        }
      },
      error = function(e) {
        copy_error <<- e
      },
      finally = {
        close(input)
        close(output)
      }
    )
    if (!is.null(copy_error)) {
      stop(
        "Could not safely extract ", relative, ": ",
        conditionMessage(copy_error),
        call. = FALSE
      )
    }
    declared <- validated$lengths[[index]]
    if (!identical(as.numeric(bytes_written), as.numeric(declared))) {
      stop(
        "Expanded size does not match the archive directory for ",
        relative,
        ".",
        call. = FALSE
      )
    }
  }
  validated
}

.genflow_validate_bundle_rds <- function(path, type) {
  value <- tryCatch(
    readRDS(path),
    error = function(e) {
      stop(
        "Invalid ", type, " RDS file ", basename(path), ": ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
  valid <- switch(type,
    setups = is.list(value) &&
      is.character(value$sname) && length(value$sname) == 1L &&
      !is.null(value$service) && !is.null(value$model),
    agents = is.list(value) &&
      is.character(value$name) && length(value$name) == 1L &&
      !is.null(value$service) && !is.null(value$model),
    content = is.list(value) &&
      is.character(value$cname) && length(value$cname) == 1L &&
      is.list(value$data),
    FALSE
  )
  if (!isTRUE(valid)) {
    stop(
      "Bundle contains an invalid ", type, " object: ",
      basename(path),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.genflow_validate_bundle_tree <- function(bundle_root) {
  metadata_path <- file.path(bundle_root, "metadata.json")
  metadata <- tryCatch(
    jsonlite::fromJSON(metadata_path, simplifyVector = TRUE),
    error = function(e) {
      stop("Invalid bundle metadata: ", conditionMessage(e), call. = FALSE)
    }
  )
  if (!is.list(metadata) || is.null(metadata$includes) || is.null(metadata$counts)) {
    stop("Bundle metadata is missing required fields.", call. = FALSE)
  }

  for (type in c("setups", "agents", "content")) {
    directory <- file.path(bundle_root, "cache", type)
    files <- if (dir.exists(directory)) {
      list.files(directory, pattern = "\\.rds$", full.names = TRUE)
    } else {
      character()
    }
    for (path in files) {
      .genflow_validate_bundle_rds(path, type)
    }
  }

  models_dir <- file.path(bundle_root, "models")
  if (dir.exists(models_dir)) {
    model_files <- list.files(models_dir, recursive = TRUE, full.names = TRUE)
    for (path in model_files) {
      extension <- tolower(tools::file_ext(path))
      if (extension == "csv") {
        header <- tryCatch(
          utils::read.csv(path, nrows = 1L, stringsAsFactors = FALSE),
          error = function(e) NULL
        )
        if (
          is.null(header) ||
          !any(tolower(names(header)) %in% c("model", "id"))
        ) {
          stop("Invalid model catalog: ", basename(path), call. = FALSE)
        }
      } else if (extension == "json") {
        tryCatch(
          jsonlite::fromJSON(path, simplifyVector = FALSE),
          error = function(e) {
            stop("Invalid model JSON file: ", basename(path), call. = FALSE)
          }
        )
      } else if (extension == "rds") {
        value <- tryCatch(readRDS(path), error = function(e) NULL)
        if (!is.data.frame(value)) {
          stop("Invalid model RDS file: ", basename(path), call. = FALSE)
        }
      }
    }
  }
  metadata
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
#' @param max_entries Maximum number of archive entries accepted.
#' @param max_file_bytes Maximum expanded size of one archive file.
#' @param max_bundle_bytes Maximum total expanded size of archive files.
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
                              quiet = FALSE,
                              max_entries = 10000L,
                              max_file_bytes = 128 * 1024^2,
                              max_bundle_bytes = 512 * 1024^2) {
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

  archive <- .genflow_safe_extract_zip(
    path,
    destination = tmp_dir,
    max_entries = max_entries,
    max_file_bytes = max_file_bytes,
    max_bundle_bytes = max_bundle_bytes
  )
  bundle_root <- file.path(tmp_dir, archive$root)
  metadata <- .genflow_validate_bundle_tree(bundle_root)

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
