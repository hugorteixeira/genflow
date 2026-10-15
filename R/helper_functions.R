## roxygen docs for helper functions

#' Sanitize a filename (internal)
#'
#' Replaces unsafe characters with underscores and truncates length.
#'
#' @param text Character string.
#' @return Sanitized character string (lowercased) limited to 100 chars.
#' @keywords internal
#' @noRd
.sanitize_filename <- function(text) {
  sanitized <- gsub("[/\\\\:*?\"<>|]+", "_", text)
  sanitized <- gsub("\\s+", "_", sanitized)
  substr(tolower(sanitized), 1, 100)
}

#' Default base directory for genflow outputs (internal)
#'
#' The location can be overridden with `options(genflow.output_dir = ...)` or
#' `GENFLOW_OUTPUT_DIR`.
#'
#' @return Path to the configured genflow output directory.
#' @keywords internal
#' @noRd
.genflow_base_dir <- function() {
  base <- getOption("genflow.output_dir", NULL)
  if (is.null(base) || !is.character(base) || length(base) != 1L ||
      is.na(base) || !nzchar(trimws(base))) {
    base <- Sys.getenv("GENFLOW_OUTPUT_DIR", unset = "")
  }
  if (!nzchar(trimws(base))) base <- "~/.genflow"
  base <- path.expand(base)
  if (!dir.exists(base) &&
      !dir.create(base, recursive = TRUE, showWarnings = FALSE)) {
    stop("Could not create genflow output directory: ", base, call. = FALSE)
  }
  base
}

#' Default output directory by type (internal)
#'
#' @param kind Optional subdirectory name (e.g., "texts", "imgs", "audios").
#' @return Path below the configured genflow output directory.
#' @keywords internal
#' @noRd
.genflow_default_dir <- function(kind = NULL) {
  base <- .genflow_base_dir()
  if (is.null(kind) || !nzchar(kind)) return(base)
  out <- file.path(base, kind)
  if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)
  out
}
.batch_cache_env <- new.env(parent = emptyenv())

.batch_cache_make_key <- function(agent_prefix,
                                 qty,
                                 instructions,
                                 add,
                                 one_item_each,
                                 add_img,
                                 add_img_each = NULL,
                                 checkpoint_each = NULL,
                                 directory,
                                 directory_img,
                                 append_modes = NULL,
                                 agent_signature = NULL,
                                 persist = TRUE) {
  sanitize_prefix <- function(prefix) {
    cleaned <- gsub("[^A-Za-z0-9_]", "_", prefix %||% "agent")
    if (!nzchar(cleaned)) cleaned <- "agent"
    cleaned
  }
  payload <- list(
    agent_prefix = agent_prefix,
    qty = qty,
    instructions = instructions,
    add = add,
    one_item_each = one_item_each,
    add_img = add_img,
    add_img_each = add_img_each,
    checkpoint_each = checkpoint_each,
    directory = directory,
    directory_img = directory_img,
    append_modes = append_modes,
    agent_signature = agent_signature,
    persist = persist
  )
  input_fields <- c("instructions", "add", "one_item_each", "add_img", "add_img_each", "agent_signature")
  payload[input_fields] <- lapply(payload[input_fields], .genflow_file_fingerprint)
  payload$runtime_context <- .genflow_runtime_context(fingerprint = TRUE)
  raw_payload <- tryCatch(serialize(payload, NULL, ascii = FALSE), error = function(e) NULL)
  if (is.null(raw_payload) || length(raw_payload) == 0) {
    return(NULL)
  }
  checksum <- .genflow_raw_md5(raw_payload)
  if (is.null(checksum) || !nzchar(checksum)) {
    return(NULL)
  }
  prefix_clean <- sanitize_prefix(agent_prefix)
  paste0("batch_", prefix_clean, "_", checksum)
}

.genflow_atomic_save_rds <- function(object,
                                     path,
                                     save_fn = saveRDS,
                                     rename_fn = file.rename,
                                     portable_replace = identical(.Platform$OS.type, "windows")) {
  if (is.null(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    return(invisible(FALSE))
  }
  parent <- dirname(path)
  if (!dir.exists(parent)) {
    dir.create(parent, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(parent)) {
    stop("Could not create the directory for an RDS file.", call. = FALSE)
  }

  staging <- .genflow_unique_sidecar_path(path, "staging", ".tmp")
  on.exit(if (file.exists(staging)) unlink(staging, force = TRUE), add = TRUE)
  wrote <- tryCatch({
    save_fn(object, staging)
    file.exists(staging) && isTRUE(file.info(staging)$size[[1]] > 0)
  }, error = function(e) FALSE)
  if (!isTRUE(wrote)) {
    stop("Could not write the RDS staging file.", call. = FALSE)
  }
  .genflow_set_private_file_mode(staging)

  target_exists <- file.exists(path)
  if (!target_exists || !isTRUE(portable_replace)) {
    replaced <- tryCatch(
      rename_fn(staging, path),
      error = function(e) FALSE
    )
    if (!isTRUE(replaced)) {
      stop("Could not atomically replace the RDS file.", call. = FALSE)
    }
    .genflow_set_private_file_mode(path)
    return(invisible(TRUE))
  }

  rollback <- .genflow_unique_sidecar_path(path, "rollback", ".tmp")
  moved_original <- tryCatch(
    rename_fn(path, rollback),
    error = function(e) FALSE
  )
  if (!isTRUE(moved_original)) {
    stop("Could not prepare a recoverable RDS replacement.", call. = FALSE)
  }
  .genflow_set_private_file_mode(rollback)

  replaced <- tryCatch(
    rename_fn(staging, path),
    error = function(e) FALSE
  )
  if (!isTRUE(replaced)) {
    restored <- tryCatch(
      rename_fn(rollback, path),
      error = function(e) FALSE
    )
    if (isTRUE(restored)) {
      .genflow_set_private_file_mode(path)
      stop("Could not replace the RDS file; the original was restored.", call. = FALSE)
    }
    stop(
      "Could not replace the RDS file; the original remains in private recovery file ",
      rollback,
      ".",
      call. = FALSE
    )
  }

  .genflow_set_private_file_mode(path)
  if (file.exists(rollback)) {
    unlink(rollback, force = TRUE)
  }
  invisible(TRUE)
}
.batch_cache_get <- function(key) {
  if (is.null(key)) {
    return(NULL)
  }
  get0(key, envir = .batch_cache_env, inherits = FALSE)
}
.batch_cache_set <- function(key, value) {
  if (is.null(key)) {
    return(invisible(FALSE))
  }
  assign(key, value, envir = .batch_cache_env)
  max_entries <- suppressWarnings(as.integer(
    getOption("genflow.batch_cache_max_entries", 32L)
  )[1])
  if (is.na(max_entries) || max_entries < 1L) {
    max_entries <- 32L
  }
  keys <- ls(envir = .batch_cache_env, all.names = TRUE)
  if (length(keys) > max_entries) {
    timestamps <- vapply(keys, function(cache_key) {
      entry <- get(cache_key, envir = .batch_cache_env, inherits = FALSE)
      timestamp <- entry$timestamp %||% as.POSIXct(NA)
      numeric <- suppressWarnings(as.numeric(timestamp)[1])
      if (is.na(numeric)) -Inf else numeric
    }, numeric(1))
    remove_count <- length(keys) - max_entries
    remove_keys <- keys[order(timestamps, keys)][seq_len(remove_count)]
    rm(list = remove_keys, envir = .batch_cache_env)
  }
  invisible(TRUE)
}
.batch_cache_clear <- function(key) {
  if (is.null(key)) {
    return(invisible(FALSE))
  }
  if (exists(key, envir = .batch_cache_env, inherits = FALSE)) {
    rm(list = key, envir = .batch_cache_env)
    return(TRUE)
  }
  FALSE
}
#' Encode image file as base64 (internal)
#'
#' @param image_path Character path to an image file.
#' @return Base64 string of the file contents.
#' @keywords internal
#' @noRd
.encode_image <- function(image_path) {
  if (!is.character(image_path) || length(image_path) != 1L ||
      is.na(image_path) || !nzchar(image_path) || !file.exists(image_path) ||
      isTRUE(file.info(image_path)$isdir)) {
    stop("`image_path` must point to one readable image file.", call. = FALSE)
  }
  if (!requireNamespace("base64enc", quietly = TRUE)) {
    stop(
      "Package 'base64enc' is required for inline image inputs.",
      call. = FALSE
    )
  }
  image_data <- readBin(image_path, "raw", file.info(image_path)$size)
  base64enc::base64encode(image_data)
}
#' Naive token estimate (internal)
#'
#' Approximates token count by dividing character count by 4.
#'
#' @param text Character string.
#' @return Integer estimate of token count.
#' @keywords internal
#' @noRd
.estimate_tokens <- function(text) {
  num_tokens <- ceiling(nchar(text) / 4)
  return(num_tokens)
}
#' Save API response to disk and print summary (internal)
#'
#' Writes the response (and optionally context) to a timestamped file and
#' prints a short one-line summary.
#'
#' @param response_api Character or object representing the response.
#' @param context Optional context (character) to append.
#' @param res_context Logical; include context in saved file if TRUE.
#' @param label Character label for filenames.
#' @param label_cat Sanitized label.
#' @param service Provider/service name.
#' @param model Model identifier.
#' @param temp Numeric temperature/guidance parameter.
#' @param duration_response Numeric duration in seconds.
#' @param directory Output directory.
#' @param status "SUCCESS" or "ERROR".
#' @param tokens_sent,tokens_received Optional token counts.
#' @return Invisibly returns either the saved response or an edited variant.
#' @keywords internal
#' @noRd
.save_response <- function(response_api, context, res_context, label, label_cat,
                           service, model, temp, duration_response, directory,
                           status, tokens_sent, tokens_received) {
  content <- if (is.character(response_api)) response_api else
    jsonlite::toJSON(response_api, auto_unbox = TRUE, null = "null", pretty = TRUE)
  if (identical(status, "ERROR")) {
    content <- c(paste0("STATUS: ", status), "MESSAGE:", content)
  } else if (isTRUE(res_context)) {
    content <- c(content, "\n\n--- provided context ---", .genflow_input_text(context))
  }
  path <- .genflow_write_artifact(content, directory, label_cat %||% label,
                                 service, model, "txt")
  cat(sprintf("[%s] %s | %s | %s\n   -> File: %s\n", status, label %||% "text", service, model, path))
  if (Sys.getenv("genflow_SKIP_PERSIST_LOG", "0") != "1") {
    tryCatch(.persist_stats_row(list(
      label = label_cat %||% label %||% NA_character_, model = model,
      temp = temp %||% NA_real_, duration = duration_response,
      tks_envia = tokens_sent, tks_recebe = tokens_received, status_api = status
    )), error = function(e) warning("The response was saved, but its statistics could not be persisted: ",
                                   conditionMessage(e), call. = FALSE))
  }
  invisible(path)
}

#' Get persistent log directory (internal)
#'
#' Uses option `genflow.log_dir` when set, otherwise
#' tools::R_user_dir("genflow", which = "data").
#'
#' @param create Logical; create the directory when it does not exist.
#' @keywords internal
#' @noRd
.get_log_dir <- function(create = TRUE) {
  override <- getOption("genflow.log_dir", NULL)
  if (is.null(override)) {
    directory <- tools::R_user_dir("genflow", which = "data")
  } else {
    if (!is.character(override) || length(override) != 1L ||
        is.na(override) || !nzchar(trimws(override))) {
      stop("Option `genflow.log_dir` must be one non-empty path.", call. = FALSE)
    }
    directory <- path.expand(override)
  }

  if (isTRUE(create) && !dir.exists(directory)) {
    dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  }
  if (isTRUE(create) && !dir.exists(directory)) {
    stop("Could not create the genflow statistics directory.", call. = FALSE)
  }
  directory
}

.genflow_stats_columns <- function() {
  c("label", "model", "temp", "duration", "tks_envia", "tks_recebe", "status_api")
}

.genflow_empty_stats <- function() {
  data.frame(
    label = character(),
    model = character(),
    temp = numeric(),
    duration = numeric(),
    tks_envia = numeric(),
    tks_recebe = numeric(),
    status_api = character(),
    stringsAsFactors = FALSE
  )
}

.genflow_normalize_stats_frame <- function(value,
                                           keep_extra = FALSE,
                                           allow_empty = FALSE) {
  if (is.list(value) && !is.data.frame(value)) {
    value <- tryCatch(
      as.data.frame(value, stringsAsFactors = FALSE),
      error = function(e) NULL
    )
  }
  if (!is.data.frame(value) || anyDuplicated(names(value)) ||
      (!isTRUE(allow_empty) && !nrow(value))) {
    return(NULL)
  }

  expected <- .genflow_stats_columns()
  for (nm in setdiff(expected, names(value))) {
    value[[nm]] <- NA
  }
  extras <- if (isTRUE(keep_extra)) setdiff(names(value), expected) else character()
  value <- value[, c(expected, extras), drop = FALSE]
  value$label <- as.character(value$label)
  value$model <- as.character(value$model)
  value$temp <- suppressWarnings(as.numeric(value$temp))
  value$duration <- suppressWarnings(as.numeric(value$duration))
  value$tks_envia <- suppressWarnings(as.numeric(value$tks_envia))
  value$tks_recebe <- suppressWarnings(as.numeric(value$tks_recebe))
  value$status_api <- as.character(value$status_api)
  value
}

.genflow_acquire_stats_lock <- function(file_path) {
  .genflow_acquire_file_lock(
    file_path,
    timeout = getOption("genflow.stats_lock_timeout", 10),
    poll = getOption("genflow.stats_lock_poll", 0.05),
    stale_after = getOption("genflow.stats_lock_stale_after", 300),
    lock_label = "statistics file"
  )
}

#' Append a single stats row to today's RDS (internal)
#'
#' Accepts a named list or one-row data.frame with fields:
#' label, model, temp, duration, tks_envia, tks_recebe, status_api
#' @keywords internal
#' @noRd
.persist_stats_row <- function(row,
                               write_fn = .genflow_atomic_save_rds,
                               date = Sys.Date()) {
  row <- .genflow_normalize_stats_frame(row)
  if (is.null(row)) {
    return(invisible(FALSE))
  }
  if (!inherits(date, "Date") || length(date) != 1L || is.na(date)) {
    stop("Statistics date must be one valid Date.", call. = FALSE)
  }

  log_dir <- .get_log_dir()
  file_path <- file.path(log_dir, paste0(format(date, "%Y%m%d"), ".rds"))
  lock <- .genflow_acquire_stats_lock(file_path)
  on.exit(.genflow_release_file_lock(lock), add = TRUE)

  existing <- .genflow_empty_stats()
  if (file.exists(file_path)) {
    existing <- tryCatch(
      readRDS(file_path),
      error = function(e) {
        stop(
          "Existing statistics log is unreadable; it was left unchanged.",
          call. = FALSE
        )
      }
    )
    if (!is.data.frame(existing)) {
      stop(
        "Existing statistics log has an invalid schema; it was left unchanged.",
        call. = FALSE
      )
    }
    existing <- .genflow_normalize_stats_frame(
      existing,
      keep_extra = TRUE,
      allow_empty = TRUE
    )
    if (is.null(existing)) {
      stop(
        "Existing statistics log has an invalid schema; it was left unchanged.",
        call. = FALSE
      )
    }
  }

  for (nm in setdiff(names(existing), names(row))) {
    row[[nm]] <- NA
  }
  combined <- rbind(existing, row[, names(existing), drop = FALSE])
  committed <- write_fn(combined, file_path)
  if (!isTRUE(committed) || !file.exists(file_path)) {
    stop("Statistics log was not committed.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Append many stats rows (internal)
#' @keywords internal
#' @noRd
.persist_many_stats <- function(results_list) {
  if (!is.list(results_list)) {
    return(invisible(FALSE))
  }
  rows <- lapply(seq_along(results_list), function(i) {
    item <- results_list[[i]]
    if (is.list(item) && !is.null(item$status_api)) {
      return(list(
        label      = item$label %||% NA_character_,
        model      = item$model %||% NA_character_,
        temp       = item$temp %||% NA_real_,
        duration   = item$duration %||% NA_real_,
        tks_envia  = item$tokens_sent %||% NA_real_,
        tks_recebe = item$tokens_received %||% NA_real_,
        status_api = item$status_api %||% "UNKNOWN"
      ))
    }
    NULL
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) {
    return(invisible(FALSE))
  }
  rows <- do.call(
    rbind,
    lapply(rows, as.data.frame, stringsAsFactors = FALSE)
  )
  .persist_stats_row(rows)
}

#' Detect agent suffix style (internal)
#'
#' Checks whether agent variables exist as alphabetic (a,b,...) or numeric (1,2,...)
#' suffixes in `.GlobalEnv`.
#'
#' @param agent_prefix Prefix for agent variables.
#' @param qty Number of agents to check.
#' @return "alphabetic" or "numeric".
#' @keywords internal
#' @noRd
.detect_suffix_type <- function(agent_prefix, qty) {
  first_agent_num <- paste0(agent_prefix, 1)
  first_agent_alpha <- paste0(agent_prefix, "a")
  if (exists(first_agent_alpha, envir = .GlobalEnv)) {
    suffix_type <- "alphabetic"
    if (qty > 26) {
      warning("number (", qty, ") > 26 with alphabetic suffix. Limiting to 'z'.")
    }
  } else if (exists(first_agent_num, envir = .GlobalEnv)) {
    suffix_type <- "numeric"
  } else {
    stop("Initial agent not found (e.g., '", first_agent_alpha, "' or '", first_agent_num, "').")
  }
  suffix_type
}
#' Normalize instructions to a single character string (internal)
#'
#' @param instructions Character vector or other object.
#' @return Single character string (possibly empty).
#' @keywords internal
#' @noRd
.sanitize_instructions <- function(instructions) {
  if (is.null(instructions)) {
    return(NULL)
  }
  if (!is.character(instructions)) {
    warning("'instructions' was not character. Converting...")
    instructions <- as.character(instructions)
  }
  if (length(instructions) > 1) {
    message("'instructions' had multiple elements. collapsing...")
    instructions <- paste(instructions, collapse = "\n")
  } else if (length(instructions) == 0) {
    warning("'instructions' is empty.")
    instructions <- ""
  }
  instructions
}
#' Ensure output directories exist (internal)
#'
#' @param directory Chat/text directory.
#' @param directory_img Images directory.
#' @keywords internal
#' @noRd
.create_directories <- function(directory, directory_img) {
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE, showWarnings = FALSE)
    message("Created chat directory: ", directory)
  }
  if (!dir.exists(directory_img)) {
    dir.create(directory_img, recursive = TRUE, showWarnings = FALSE)
    message("Created images directory: ", directory_img)
  }
}
#' Export variables to cluster workers (internal)
#'
#' @keywords internal
#' @noRd
.export_cluster_vars <- function(cl,
                                 qty,
                                 agent_prefix,
                                 suffix_type,
                                 instructions,
                                 add,
                                 add_img,
                                 add_img_each,
                                 one_item_each,
                                 append_modes,
                                 directory,
                                 directory_img,
                                 agent = NULL,
                                 persist = TRUE,
                                 checkpoint_each = NULL) {
  # Keep this list limited to values captured by the PSOCK task closure. The
  # worker function retains its package namespace, so exporting every provider
  # helper is both unnecessary and fragile when internal names change.
  varlist_base <- c(
    ".execute_agent_task", "one_item_each", "instructions", "add",
    "add_img", "add_img_each", "append_modes", "directory",
    "directory_img", "agent_prefix", "suffix_type", "agent", "persist",
    "checkpoint_each"
  )

  parallel::clusterExport(cl, varlist = varlist_base, envir = environment())
  context <- .genflow_runtime_context()
  parallel::clusterCall(cl, function(context) {
    options(context$options)
    invisible(NULL)
  }, context)

  if (!is.null(agent)) {
    return(invisible(TRUE))
  }

  # Export legacy per-task agent configurations from .GlobalEnv.
  config_vars_needed <- sapply(1:qty, function(idx) {
    if (suffix_type == "alphabetic") paste0(agent_prefix, letters[idx]) else paste0(agent_prefix, idx)
  })
  config_vars_exist <- config_vars_needed[sapply(config_vars_needed, exists, where = .GlobalEnv)]
  if (length(config_vars_exist) < qty) {
    warning("Missing configurations in .GlobalEnv: ", paste(config_vars_needed[!config_vars_needed %in% config_vars_exist], collapse = ", "))
  }
  if (length(config_vars_exist) > 0) {
    parallel::clusterExport(cl, varlist = config_vars_exist, envir = .GlobalEnv)
  }
}
#' Worker execution: build prompt and call APIs (internal)
#'
#' @keywords internal
#' @noRd
# Shared argument preparation is the only route into generator methods.
.execute_agent_task <- function(i, one_item_each, instructions, add, add_img,
                                directory, directory_img, agent_prefix, suffix_type,
                                append_modes, agent = NULL, add_img_each = NULL,
                                persist = TRUE, checkpoint_each = NULL) {
  started <- Sys.time()
  previous <- Sys.getenv("genflow_SKIP_PERSIST_LOG", unset = NA_character_)
  on.exit(if (is.na(previous)) Sys.unsetenv("genflow_SKIP_PERSIST_LOG") else
    Sys.setenv(genflow_SKIP_PERSIST_LOG = previous), add = TRUE)
  Sys.setenv(genflow_SKIP_PERSIST_LOG = "1")
  label <- paste0(agent_prefix, if (suffix_type == "alphabetic") letters[i] else i)
  task_name <- if (!is.null(names(add_img_each))) names(add_img_each)[i] else NA_character_
  task_id <- if (!is.na(task_name) && nzchar(task_name)) task_name else as.character(i)
  response <- NULL
  error <- NULL
  type <- "unknown"
  logs <- paste0("[Starting] ", label, "\n")
  tryCatch({
    config <- if (!is.null(agent)) agent else get(label, envir = .GlobalEnv, inherits = FALSE)
    if (!is.list(config)) stop("Agent configuration must be a list.")
    config <- .genflow_normalize_agent_fields(config)
    type <- tolower(trimws(config$type %||% "Chat"))
    if (!type %in% c("chat", "vision", "image")) stop("Agent type not supported: ", type)
    if (is.null(config$service) || !nzchar(config$service)) stop("Field 'service' cannot be empty.")
    if (type != "image" && (is.null(config$model) || !nzchar(config$model))) stop("A non-empty 'model' is required.")
    field <- if (type == "image") "prompt" else "context"
    original <- config[[field]] %||% config$context %||% config$instructions
    config[[field]] <- .genflow_batch_combine(original, instructions, append_modes$instructions, text = TRUE)
    if (is.null(config[[field]])) stop("No instructions/context available for agent '", label, "'.")
    config$add <- .genflow_batch_combine(config$add, add, append_modes$add)
    if (!is.null(one_item_each) && i <= length(one_item_each)) {
      config$add <- c(list(one_item_each[[i]]), .genflow_add_items(config$add))
    }
    overrides <- list(directory = if (type == "image") directory_img else directory,
                      label = label, persist = persist)
    if (type != "image" && (!is.null(add_img_each) || !is.null(add_img))) {
      overrides$add_img <- if (!is.null(add_img_each)) add_img_each[[i]] else add_img
    }
    target <- if (type == "image") gen_img.default else gen_txt.default
    args <- .genflow_prepare_agent_args(config, overrides, formals(target), required = field)
    response <- do.call(target, args, quote = TRUE)
    if (!is.list(response) || is.null(response$status_api)) stop("Generator returned an invalid result.")
  }, error = function(e) {
    error <<- conditionMessage(e)
  })
  finished <- Sys.time()
  duration <- as.numeric(difftime(finished, started, units = "secs"))
  if (!is.null(checkpoint_each)) {
    checkpoint <- list(schema_version = 1L, task_id = task_id,
                       completed_at = format(finished, "%Y-%m-%dT%H:%M:%OS6%z"),
                       response = response, error = error, duration_seconds = duration)
    tryCatch(.genflow_atomic_save_rds(checkpoint, checkpoint_each[[i]]),
      error = function(e) error <<- paste(c(error, paste0("Could not save task checkpoint: ", conditionMessage(e))), collapse = "; "))
  }
  logs <- c(logs, paste0(if (!is.null(error)) error else response$status_msg, "\n"))
  list(index = i, task_id = task_id, label = label, tipo_agente = type,
       response = response, erro = error, logs = logs, duration_individual = duration)
}

#' Normalize and map worker results (internal)
#'
#' @keywords internal
#' @noRd
.process_parallel_results <- function(raw_results,
                                      qty,
                                      agent_prefix,
                                      suffix_type,
                                      expected_indices = seq_len(qty)) {
  # Initialization
  results_finais <- vector("list", qty) # Will hold the LISTS from workers (or NULLs)
  final_errors <- vector("list", qty) # Stores final error status message for each index
  logs_list <- vector("list", qty)
  single_durations <- rep(NA_real_, qty)
  agent_types <- rep("unknown", qty) # Default until read from worker result
  indice_processado <- rep(FALSE, qty)
  valid_results_count <- 0
  n_results_recebidos <- length(raw_results)

  if (n_results_recebidos == 0 && qty > 0) { # Handle no results if qty > 0
    if (length(expected_indices)) warning("No raw results received from workers.")
    for (i in expected_indices) {
      final_errors[[i]] <- paste0("Index ", i, ": No result received from worker.")
    }
    # Return empty/NA structure matching the expected output format
    return(list(
      results = results_finais, erros = final_errors, logs_list = logs_list,
      single_durations = single_durations, agent_types = agent_types,
      valid_results_count = 0
    ))
  } else if (n_results_recebidos == 0 && qty == 0) {
    # Handle edge case qty = 0
    return(list(
      results = list(), erros = list(), logs_list = list(),
      single_durations = numeric(), agent_types = character(),
      valid_results_count = 0
    ))
  }


  # Loop through raw results from workers
  for (k in 1:n_results_recebidos) {
    res_item <- raw_results[[k]]
    original_i <- NA
    worker_k_error <- NULL # Store errors related to processing worker k's result

    # --- Basic Checks on Worker Result Item ---
    if (inherits(res_item, "try-error")) {
      worker_k_error <- paste("Worker", k, "failed (try-error):", as.character(res_item))
      warning(worker_k_error)
      # Cannot determine original_i, log the error generally if needed later
      # We don't assign to final_errors[NA]
      next # Skip to next worker result
    }
    if (is.null(res_item)) {
      worker_k_error <- paste("Worker", k, "returned NULL.")
      warning(worker_k_error)
      next # Skip
    }
    # Check if it's the expected list structure from .execute_agent_task
    expected_names <- c("label", "tipo_agente", "response", "erro", "logs", "duration_individual")
    if (!is.list(res_item) || !all(expected_names %in% names(res_item))) {
      worker_k_error <- paste0("Worker ", k, " returned unexpected object (Type: ", class(res_item)[1], ", Names: ", paste(names(res_item), collapse = ", "), "). Expected: list with ", paste(expected_names, collapse = ", "))
      warning(worker_k_error)
      next # Skip
    }

    # --- Map Label to Original Index ---
    current_label <- res_item$label
    if (!is.null(res_item$index) && length(res_item$index) == 1L) {
      original_i <- suppressWarnings(as.integer(res_item$index))
      if (is.na(original_i) || original_i < 1L || original_i > qty) original_i <- NA_integer_
    }
    if (is.na(original_i)) {
      if (!is.null(current_label) && is.character(current_label) && nchar(current_label) > 0) {
        label_suffix <- if (startsWith(current_label, agent_prefix)) {
          substring(current_label, nchar(agent_prefix) + 1L)
        } else {
          current_label
        }
        if (suffix_type == "alphabetic") {
          original_i <- match(label_suffix, letters)
        } else {
          original_i <- suppressWarnings(as.integer(label_suffix))
        }
        # Validate the derived index
        if (is.na(original_i) || original_i < 1 || original_i > qty) {
          worker_k_error <- paste0("Worker ", k, ": failed to map label '", current_label, "' to a valid index (1-", qty, "). Suffix: '", label_suffix, "'")
          warning(worker_k_error)
          original_i <- NA # Invalidate index
        }
      } else {
        worker_k_error <- paste0("Worker ", k, ": Missing task index and valid label in result.")
        warning(worker_k_error)
        original_i <- NA # Invalidate index
      }
    }

    # --- Process if Index Mapping was Successful ---
    if (!is.na(original_i)) {
      if (indice_processado[original_i]) {
        warning("Index ", original_i, " already processed (duplicate/late result from worker ", k, "? Label: '", current_label, "'). Ignoring.")
        next # Skip this duplicate/late result
      }

      # Mark as processed immediately to prevent duplicates
      indice_processado[original_i] <- TRUE

      # Extract components from the worker's result list
      response_estrutura_api <- res_item$response # This is the LIST from gen_txt/gen_img (or NULL)
      if (is.list(response_estrutura_api) && !is.null(res_item$task_id)) {
        response_estrutura_api$task_id <- res_item$task_id
      }
      worker_internal_error <- res_item$erro # Error string from worker's tryCatch (or NULL)
      # Keep list indices stable even when the stored value is NULL
      logs_list[original_i] <- list(res_item$logs)
      agent_types[original_i] <- res_item$tipo_agente %||% "unknown" # Use type from worker
      single_durations[original_i] <- res_item$duration_individual %||% NA_real_ # Use worker time

      # --- STORE THE RESULT STRUCTURE ---
      # Store the whole list (or NULL) directly
      results_finais[original_i] <- list(response_estrutura_api)

      # --- Determine Final Error Status for this index ---
      final_error_msg_for_index <- NULL

      # Priority 1: Worker internal fatal error
      if (!is.null(worker_internal_error)) {
        final_error_msg_for_index <- paste("Internal worker error:", worker_internal_error)
      }

      # Priority 2: Check API status *if* we received a valid list structure
      if (is.list(response_estrutura_api)) {
        # Update time if available from API call metadata inside the list
        if (!is.null(response_estrutura_api$duration) && is.numeric(response_estrutura_api$duration)) {
          single_durations[original_i] <- response_estrutura_api$duration
        }
        # Check API status
        if (!is.null(response_estrutura_api$status_api) && response_estrutura_api$status_api == "ERROR") {
          api_error_detail <- response_estrutura_api$status_msg %||% "Detail not available"
          api_error_msg <- paste("API error:", api_error_detail)
          # Combine with worker error if it exists
          final_error_msg_for_index <- if (is.null(final_error_msg_for_index)) api_error_msg else paste(final_error_msg_for_index, ";", api_error_msg)
        }
      } else if (is.null(response_estrutura_api) && is.null(final_error_msg_for_index)) {
        # Handle case where API response is NULL and no worker error occurred
        final_error_msg_for_index <- "Worker returned NULL (possible silent failure in API or internal function)."
      } else if (!is.list(response_estrutura_api) && !is.null(response_estrutura_api) && is.null(final_error_msg_for_index)) {
        # Handle case where API response is some non-list value (e.g., simple error string) and no worker error
        final_error_msg_for_index <- paste("Worker returned unexpected value:", as.character(response_estrutura_api))
      }

      # Assign the determined error message (or NULL if success)
      final_errors[original_i] <- list(final_error_msg_for_index)

      # Increment valid count only if no final error was recorded
      if (is.null(final_error_msg_for_index)) {
        valid_results_count <- valid_results_count + 1
      }
    } # End if !is.na(original_i)
  } # End of loop for k

  # --- Post-processing: Check for any indices that were never processed ---
  indices_nao_processados <- setdiff(expected_indices, which(indice_processado[seq_len(qty)]))
  if (length(indices_nao_processados) > 0) {
    for (i_faltante in indices_nao_processados) {
      if (is.null(final_errors[[i_faltante]])) { # If no specific error was logged yet
        final_errors[i_faltante] <- list(paste0("Index ", i_faltante, ": No valid result received or mapped from worker."))
        warning(final_errors[[i_faltante]])
      }
    }
  }

  # Return the final processed lists
  list(
    results = results_finais, # List of LISTS (or NULLs)
    erros = final_errors, # List of error messages (or NULLs)
    logs_list = logs_list,
    single_durations = single_durations,
    agent_types = agent_types,
    valid_results_count = valid_results_count
  )
}
