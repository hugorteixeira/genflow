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
.save_response <- function(response_api,
                           context,
                           res_context,
                           label,
                           label_cat,
                           service,
                           model,
                           temp,
                           duration_response,
                           directory,
                           status,
                           tokens_sent,
                           tokens_received) {
  # --- Generate the filename ---
  data_hora <- paste0(
    gsub("[^0-9]", "", format(Sys.time(), "%Y%m%d_%H%M%OS6")),
    "_p",
    Sys.getpid(),
    "_",
    sprintf("%06d", sample.int(999999L, 1L))
  )

  # Determine a sanitized label to use in filenames. Prefer the provided
  # `label_cat` (already sanitized by callers). If not provided, derive from
  # `label` (or `context`) and sanitize it here to avoid path separators or
  # other unsafe characters ending up in filenames.
  if (is.null(label) || !nzchar(label)) {
    label <- deparse(substitute(context))
  }
  if (is.null(label_cat) || !nzchar(label_cat)) {
    label_cat <- .sanitize_filename(label)
  }

  # Cleanup and formatting of the model name for the filename
  # Ensure that model is a string before using gsub
  model_str <- ifelse(is.null(model) || !is.character(model), "unknown_model", model)
  model_fn <- .sanitize_filename(model_str)

  # Ensure service and temp are usable in the filename
  service_fn <- ifelse(is.null(service) || !is.character(service), "unknown_service", service)
  temp_fn <- ifelse(is.null(temp) || is.na(temp) || !is.numeric(temp), "unknown_temp", temp)


  # Use the sanitized label (`label_cat`) for filenames so we don't end up
  # with embedded path separators or other unsafe chars in `nome_arquivo`.
  nome_arquivo <- paste0(label_cat, "_", service_fn, "_", model_fn, "_tp", temp_fn, "_", data_hora, ".txt")
  caminho_resultado_final <- file.path(directory, nome_arquivo)

  # --- Write the result file ---
  conteudo_para_save <- if (is.character(response_api)) {
    response_api
  } else {
    # Try to convert to character if not (e.g., error, NULL)
    tryCatch(as.character(response_api), error = function(e) {
      warning("Could not convert response_api to character when saving: ", conditionMessage(e))
      return("ERROR_CONVERSAO_save")
    })
  }

  # Ensure the parent directory of the target file exists before writing
  parent_dir <- dirname(caminho_resultado_final)
  if (!dir.exists(parent_dir)) {
    dir.create(parent_dir, recursive = TRUE, showWarnings = FALSE)
    cat("Directory created:", parent_dir, "\n")
  }

  if (status == "ERROR") {
    # In case of error, save only the error message in the main file
    write_result <- try(writeLines(paste("STATUS:", status, "\nMESSAGE:", conteudo_para_save),
      con = caminho_resultado_final, useBytes = TRUE
    ))
  } else if (label == "pedido_dir_mkt" || label == "response_dir_mkt" || res_context == TRUE) {
    # Save response + context if SUCCESS and applicable
    context_str <- tryCatch(as.character(context), error = function(e) {
      warning("Could not convert context to character: ", conditionMessage(e))
      return("ERROR_CONVERSAO_context")
    })
    write_result <- try(writeLines(c(conteudo_para_save, "\n\n--- provided context ---", context_str),
      con = caminho_resultado_final, useBytes = TRUE
    ))
  } else {
    # Save only the response if SUCCESS and res_context = FALSE
    write_result <- try(writeLines(conteudo_para_save,
      con = caminho_resultado_final, useBytes = TRUE
    ))
  }

  # Check if writing the file failed
  if (inherits(write_result, "try-error")) {
    warning("Failed to write the main file: ", caminho_resultado_final, " - Error: ", write_result)
  }


  # --- Console message ---
  preview_response <- if (is.character(conteudo_para_save)) {
    substr(gsub("\n", " ", conteudo_para_save), 1, 150) # Limit and remove line breaks
  } else {
    typeof(conteudo_para_save) # Show the type if not a string
  }

  # Ensure numeric values for sprintf are valid
  duration_print <- ifelse(is.null(duration_response) || is.na(duration_response) || !is.numeric(duration_response), 0, duration_response)
  tokens_env_print <- ifelse(is.null(tokens_sent) || is.na(tokens_sent) || !is.numeric(tokens_sent), "?", tokens_sent)
  tokens_rec_print <- ifelse(is.null(tokens_received) || is.na(tokens_received) || !is.numeric(tokens_received), "?", tokens_received)

  message_line <- sprintf(
    "[%s] %s | %s | %s | Temp: %s | Time: %.2fs | Tk_Env: %s | Tk_Rec: %s \n   -> File: %s\n   -> Response: %s...\n",
    status,
    label_cat, service_fn, model_str, temp_fn, # Use safe versions for printing
    duration_print,
    tokens_env_print,
    tokens_rec_print,
    basename(caminho_resultado_final), # Show the saved file base name
    preview_response
  )
  cat(message_line)

  # --- Persist stats row to daily RDS (unless suppressed in batch workers) ---
  # To avoid race conditions when running in parallel (gen_batch), workers set
  # genflow_SKIP_PERSIST_LOG=1 and the main process persists after aggregation.
  should_persist <- tryCatch(
    {
      Sys.getenv("genflow_SKIP_PERSIST_LOG", unset = "0") != "1"
    },
    error = function(e) TRUE
  )
  if (isTRUE(should_persist)) {
    tryCatch(
      .persist_stats_row(list(
          label      = label_cat %||% label %||% NA_character_,
          model      = model %||% NA_character_,
          temp       = temp %||% NA_real_,
          duration   = duration_response %||% NA_real_,
          tks_envia  = tokens_sent %||% NA_real_,
          tks_recebe = tokens_received %||% NA_real_,
          status_api = status %||% "UNKNOWN"
      )),
      error = function(e) {
        warning(
          "The response was generated, but its statistics could not be persisted: ",
          conditionMessage(e),
          call. = FALSE
        )
      }
    )
  }

  # --- Processing specific to email and return ---
  value_retorno <- response_api # by default, return what was received (response or Error)

  # Process email only if the call was SUCCESS, is the correct type, and response_api is character
  # if (status == "SUCCESS" && label == "response_dir_mkt" && is.character(response_api)) {
  #   caminho_resultado_final_email <- file.path(directory, paste0("email_", nome_arquivo))
  #   # Assumes separar_email exists and works with response_api
  #   response_api_editada <- tryCatch(
  #     separar_email(response_api),
  #     error = function(e) {
  #       warning("Failed to execute separar_email: ", conditionMessage(e))
  #       return(paste("ERROR_SEPARAR_EMAIL:", conditionMessage(e)))
  #     }
  #   )
  #
  #   # Try writing the email file
  #   write_email_result <- try(writeLines(as.character(response_api_editada),
  #                                        con = caminho_resultado_final_email, useBytes = TRUE))
  #
  #   if (inherits(write_email_result, "try-error")) {
  #     warning("Failed to write the email file: ", caminho_resultado_final_email, " - Error: ", write_email_result)
  #     # Decide whether to return the error or the edited attempt
  #     # Here we return the edited attempt even if saving failed
  #     value_retorno <- response_api_editada
  #   } else {
  #     # If saved with SUCCESS, return the edited version
  #     value_retorno <- response_api_editada
  #   }
  # }

  # Return the determined value (original response, error, or edited response)
  return(invisible(value_retorno))
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
.execute_agent_task <- function(i,
                                one_item_each,
                                instructions,
                                add,
                                add_img,
                                directory,
                                directory_img,
                                agent_prefix,
                                suffix_type,
                                append_modes,
                                agent = NULL,
                                add_img_each = NULL,
                                persist = TRUE,
                                checkpoint_each = NULL) {
  inicio_duration <- Sys.time()
  # Signal to downstream generators to skip per-call persistence (avoid parallel write races)
  previous_skip_log <- Sys.getenv("genflow_SKIP_PERSIST_LOG", unset = NA_character_)
  on.exit({
    if (is.na(previous_skip_log)) {
      Sys.unsetenv("genflow_SKIP_PERSIST_LOG")
    } else {
      Sys.setenv(genflow_SKIP_PERSIST_LOG = previous_skip_log)
    }
  }, add = TRUE)
  try(Sys.setenv(genflow_SKIP_PERSIST_LOG = "1"), silent = TRUE)
  logs_internos <- c()
  is_missing_field <- function(x) {
    if (is.null(x)) {
      return(TRUE)
    }
    if (length(x) == 0) {
      return(TRUE)
    }
    if (length(x) > 1) {
      return(FALSE)
    }
    if (is.atomic(x) && length(x) == 1 && is.na(x)) {
      return(TRUE)
    }
    if (is.character(x) && nchar(trimws(x)) == 0) {
      return(TRUE)
    }
    FALSE
  }
  get_config_value <- function(cfg, ...) {
    for (nm in c(...)) {
      value <- tryCatch(cfg[[nm]], error = function(e) NULL)
      if (!is_missing_field(value)) {
        return(value)
      }
    }
    NULL
  }
  get_formal_default <- function(fn, arg) {
    fmls <- tryCatch(formals(fn), error = function(e) NULL)
    if (is.null(fmls) || is.null(fmls[[arg]])) {
      return(NULL)
    }
    eval(fmls[[arg]], envir = environment(fn))
  }
  allowed_append_modes <- c("before", "after", "replace")
  normalize_mode <- function(value) {
    val <- tolower(value %||% "replace")
    if (!val %in% allowed_append_modes) {
      val <- "replace"
    }
    val
  }
  append_modes <- append_modes %||% list()
  append_modes <- list(
    instructions = normalize_mode(append_modes$instructions),
    add = normalize_mode(append_modes$add)
  )
  coerce_instruction_block <- function(value) {
    if (is.null(value) || length(value) == 0) {
      return(NULL)
    }
    vec <- tryCatch(unlist(value, use.names = FALSE), error = function(e) {
      tryCatch(as.character(value), error = function(e2) NULL)
    })
    if (is.null(vec)) {
      return(NULL)
    }
    vec <- as.character(vec)
    vec <- vec[!is.na(vec)]
    if (length(vec) == 0) {
      return(NULL)
    }
    vec <- trimws(vec)
    vec <- vec[nzchar(vec)]
    if (length(vec) == 0) {
      return(NULL)
    }
    paste(vec, collapse = "\n")
  }
  combine_instruction_blocks <- function(agent_block, user_block, mode) {
    mode <- normalize_mode(mode)
    if (mode == "replace") {
      if (!is.null(user_block)) {
        return(user_block)
      }
      return(agent_block)
    }
    if (is.null(agent_block)) {
      return(user_block)
    }
    if (is.null(user_block)) {
      return(agent_block)
    }
    if (mode == "before") {
      return(paste(user_block, agent_block, sep = "\n\n"))
    }
    paste(agent_block, user_block, sep = "\n\n")
  }
  coerce_add_list <- function(value) {
    if (is.null(value) || length(value) == 0) {
      return(list())
    }
    lst <- if (is.list(value)) value else as.list(value)
    if (length(lst) == 0) {
      return(list())
    }
    lst[!vapply(lst, function(item) {
      if (is.null(item)) {
        return(TRUE)
      }
      if (is.atomic(item) && length(item) == 1) {
        if (is.na(item)) {
          return(TRUE)
        }
        if (is.character(item) && nchar(trimws(item)) == 0) {
          return(TRUE)
        }
      }
      FALSE
    }, logical(1))]
  }
  combine_add_blocks <- function(agent_block, user_block, mode) {
    agent_list <- coerce_add_list(agent_block)
    user_list <- coerce_add_list(user_block)
    mode <- normalize_mode(mode)
    if (length(user_list) == 0) {
      return(if (length(agent_list) == 0) NULL else agent_list)
    }
    if (length(agent_list) == 0) {
      return(user_list)
    }
    combined <- switch(mode,
      before = c(user_list, agent_list),
      after = c(agent_list, user_list),
      replace = user_list,
      user_list
    )
    if (length(combined) == 0) {
      NULL
    } else {
      combined
    }
  }
  erro_interno <- NULL
  response_estrutura_api <- NULL
  tipo_agente <- "unknown"
  task_img <- if (!is.null(add_img_each)) add_img_each[[i]] else add_img
  task_name <- if (!is.null(add_img_each) && !is.null(names(add_img_each))) names(add_img_each)[i] else NA_character_
  task_id <- if (!is.na(task_name) && nzchar(task_name)) task_name else as.character(i)
  checkpoint_path <- if (!is.null(checkpoint_each)) checkpoint_each[[i]] else NULL

  # Define label early for logging and agent lookup
  label_agente <- tryCatch(
    {
      if (suffix_type == "alphabetic") {
        if (i > 0 && i <= length(letters)) paste0(agent_prefix, letters[i]) else paste0(agent_prefix, "invalid_idx_", i)
      } else {
        paste0(agent_prefix, i)
      }
    },
    error = function(e) paste0(agent_prefix, "error_fmt_", i)
  ) # Fallback label if suffix logic fails

  logs_internos <- c(logs_internos, paste0("[Starting] Worker i=", i, " -> label='", label_agente, "'\n"))

  tryCatch(
    {
      # --- 1. Get Agent Configuration ---
      config_agente <- if (!is.null(agent)) {
        agent
      } else {
        if (!exists(label_agente, envir = .GlobalEnv)) {
          stop(paste0("Agent configuration '", label_agente, "' not found in .GlobalEnv."))
        }
        get(label_agente, envir = .GlobalEnv)
      }
      if (!is.list(config_agente)) {
        stop(paste0("Invalid configuration '", label_agente, "'. Must be a list."))
      }
      service_value <- get_config_value(config_agente, "service", "Service")
      if (is_missing_field(service_value)) {
        stop(paste0("Invalid configuration '", label_agente, "'. Field 'service' cannot be empty."))
      }
      tipo_agente_raw <- get_config_value(config_agente, "type", "Type")
      if (is_missing_field(tipo_agente_raw)) {
        tipo_agente_raw <- "Chat"
      }
      tipo_agente_raw <- as.character(tipo_agente_raw)
      tipo_agente <- tolower(trimws(tipo_agente_raw))
      model_value <- get_config_value(config_agente, "model", "Model")
      if (tipo_agente %in% c("chat", "vision")) {
        if (is_missing_field(model_value)) {
          stop(paste0("Configuration '", label_agente, "' requires a non-empty 'model' for agent type '", tipo_agente_raw, "'."))
        }
      }
      model_log_value <- if (!is_missing_field(model_value)) model_value else "<default>"
      temp_value <- get_config_value(config_agente, "temp", "Temp")
      temp_log_value <- if (!is_missing_field(temp_value)) temp_value else "<default>"
      logs_internos <- c(logs_internos, paste0(
        "  -> configuration '", label_agente, "' loaded: ",
        service_value, "/", model_log_value,
        " (Type: ", tipo_agente_raw, ", temp: ", temp_log_value, ")\n"
      ))

      # --- 2. Determine Agent Type ---
      if (!tipo_agente %in% c("chat", "vision", "image")) {
        stop(
          "Agent type '", tipo_agente_raw, "' is not supported for '",
          label_agente, "'. Choose Chat, Vision, or Image."
        )
      } else {
        logs_internos <- c(logs_internos, paste0("  -> Agent type set: '", tipo_agente_raw, "'\n"))
      }

      service_cfg <- get_config_value(config_agente, "service", "Service")
      model_cfg <- get_config_value(config_agente, "model", "Model")
      temp_cfg <- get_config_value(config_agente, "temp", "Temp")
      tools_cfg <- get_config_value(config_agente, "tools", "Tools")
      timeout_cfg <- get_config_value(config_agente, "timeout_api", "timeoutApi", "timeout", "Timeout")


      # --- 3. Prepare Context/Prompt --- ## Integration of processar_add logic here ##
      logs_internos <- c(logs_internos, "  -> Preparing final prompt...\n")
      prompt_final_parts <- list()
      agent_context_raw <- config_agente$context %||% config_agente$instructions %||% NULL
      agent_context_block <- coerce_instruction_block(agent_context_raw)
      user_context_block <- coerce_instruction_block(instructions)
      final_instructions <- combine_instruction_blocks(agent_context_block, user_context_block, append_modes$instructions)
      if (is.null(final_instructions)) {
        stop(paste0("No instructions/context available for agent '", label_agente, "'. Provide `instructions` or ensure the agent has a `context`."))
      }
      prompt_final_parts$instructions <- final_instructions
      logs_internos <- c(
        logs_internos,
        paste0(
          "     -> Instructions assembled (agent source: ",
          ifelse(is.null(agent_context_block), "no", "yes"),
          ", supplied: ",
          ifelse(is.null(user_context_block), "no", "yes"),
          ", mode: ",
          append_modes$instructions,
          ").\n"
        )
      )
      agent_add_raw <- config_agente$add %||% NULL
      user_add_raw <- add
      agent_add_list_preview <- coerce_add_list(agent_add_raw)
      user_add_list_preview <- coerce_add_list(user_add_raw)
      add <- combine_add_blocks(agent_add_raw, user_add_raw, append_modes$add)
      combined_add_list_preview <- coerce_add_list(add)
      logs_internos <- c(
        logs_internos,
        paste0(
          "     -> Additional 'add' context assembled (agent items: ",
          length(agent_add_list_preview),
          ", supplied items: ",
          length(user_add_list_preview),
          ", result items: ",
          length(combined_add_list_preview),
          ", mode: ",
          append_modes$add,
          ").\n"
        )
      )

      # 3.1 Process one_item_each (item specific)
      if (!is.null(one_item_each) && i <= length(one_item_each)) {
        item_atual <- one_item_each[[i]]
        if (!is.null(item_atual)) {
          item_especifico_str <- NULL
          # Use a similar logic as processar_add for robust conversion, but without file reading
          if (is.character(item_atual)) {
            item_especifico_str <- paste(item_atual, collapse = "\n") # Join potentially multi-line chars
          } else if (is.data.frame(item_atual) || is.matrix(item_atual)) {
            item_especifico_str <- paste(capture.output(print(item_atual)), collapse = "\n")
          } else if (is.list(item_atual)) {
            # Use capture.output for lists as well, more reliable than rapply+unlist
            item_especifico_str <- paste(capture.output(print(item_atual)), collapse = "\n")
          } else {
            item_especifico_str <- as.character(item_atual)
          }

          if (!is.null(item_especifico_str) && nchar(trimws(item_especifico_str)) > 0) {
            prompt_final_parts$item_especifico <- paste0("\n\n### Specific Data for this Task (", label_agente, "):\n", item_especifico_str)
            item_log_preview <- substr(gsub("\\s+", " ", item_especifico_str), 1, 70)
            logs_internos <- c(logs_internos, paste0("     -> Added specific item (", class(item_atual)[1], "): ", item_log_preview, "...\n"))
          } else {
            logs_internos <- c(logs_internos, "     -> Processed specific item resulted in empty/NULL text.\n")
          }
        } else {
          logs_internos <- c(logs_internos, "     -> Specific item was NULL.\n")
        }
      } else {
        logs_internos <- c(logs_internos, "     -> No specific item provided or index out of bounds.\n")
      }


      # 3.2 Process add (general additional context) - Integrated logic from processar_add
      if (!is.null(add)) {
        elements_add_str_list <- list()
        # Normalize 'add' input: make it a list if it isn't already
        items_to_process <- if (is.list(add)) {
          add
        } else if (is.vector(add) && length(add) > 1) {
          as.list(add)
        } else {
          list(add) # Handles single atomic elements (character, numeric, etc.)
        }

        if (length(items_to_process) > 0) {
          logs_internos <- c(logs_internos, paste0("     -> Processing ", length(items_to_process), " item(s) do context 'add'...\n"))
          elements_add_str_list <- lapply(seq_along(items_to_process), function(idx) { # Use index for better logging
            item <- items_to_process[[idx]]
            item_text <- NULL # Initialize item text result

            if (is.null(item)) {
              logs_internos <<- c(logs_internos, paste0("        -> Item add[", idx, "] is NULL. Skipping.\n"))
              return(NULL) # Skip null items
            }

            # --- Logic ported from processar_add for file handling and object conversion ---
            logs_internos <<- c(logs_internos, paste0("        -> Processing item add[", idx, "] (Type: ", class(item)[1], ")...\n"))
            if (is.character(item) && length(item) == 1 && file.exists(item)) {
              # It's a file path string
              extensao <- tolower(tools::file_ext(item))
              file_log_preview <- item # Use full path in log for files

              if (extensao == "txt") {
                tryCatch(
                  {
                    text <- readLines(item, encoding = "UTF-8", warn = FALSE)
                    item_text <- paste(text, collapse = "\n")
                    logs_internos <<- c(logs_internos, paste0("           -> Read TXT file: '", file_log_preview, "' (", nchar(item_text), " chars).\n"))
                  },
                  error = function(e) {
                    err_msg <- paste0("[ERROR: Could not read the TXT file ", file_log_preview, " - ", conditionMessage(e), "]")
                    logs_internos <<- c(logs_internos, paste0("           -> ERROR to read TXT: ", err_msg, "\n"))
                    stop(err_msg)
                  }
                )
              } else if (extensao == "csv") {
                tryCatch(
                  {
                    # read.csv default encoding might differ; specify for consistency if needed
                    dados <- read.csv(item, stringsAsFactors = FALSE)
                    item_text <- paste(capture.output(print(dados)), collapse = "\n")
                    logs_internos <<- c(logs_internos, paste0("           -> Read CSV file: '", file_log_preview, "' (", nrow(dados), " rows, ", ncol(dados), " columns).\n"))
                  },
                  error = function(e) {
                    err_msg <- paste0("[ERROR: Could not read the CSV file ", file_log_preview, " - ", conditionMessage(e), "]")
                    logs_internos <<- c(logs_internos, paste0("           -> ERROR to read CSV: ", err_msg, "\n"))
                    stop(err_msg)
                  }
                )
              } else {
                err_msg <- paste0("[ERROR: Unsupported file format in 'add' (item ", idx, "): '", file_log_preview, "' (extension .", extensao, "). Use .txt or .csv.]")
                logs_internos <<- c(logs_internos, paste0("           -> ", err_msg, "\n"))
                stop(err_msg)
              }
            } else {
              # It's not a file path, convert the R object to string
              # Use capture.output for robustness with data.frames, lists, nested structures etc.
              item_text <- paste(capture.output(print(item)), collapse = "\n")
              item_log_preview <- substr(gsub("\\s+", " ", item_text), 1, 50)
              logs_internos <<- c(logs_internos, paste0("           -> Converted R object (", class(item)[1], ") to text: ", item_log_preview, "...\n"))
            }
            # --- End of Logic ported from processar_add ---

            # Ensure item_text is character and not NULL/NA before returning
            if (is.null(item_text) || is.na(item_text)) {
              return(NULL)
            }
            if (!is.character(item_text)) item_text <- as.character(item_text)
            if (nchar(trimws(item_text)) == 0) {
              return(NULL)
            } # Skip if result is empty string

            return(item_text) # Return the processed text for this item
          })

          # Remove any NULLs that resulted from processing (e.g., null items, items that couldn't be processed)
          elements_add_str_list <- elements_add_str_list[!sapply(elements_add_str_list, is.null)]

          if (length(elements_add_str_list) > 0) {
            # Join the processed string elements with a clear separator
            context_add_str <- paste(unlist(elements_add_str_list), collapse = "\n\n---\n\n") # Separator between items
            # Add the header for the entire 'add' block
            prompt_final_parts$context_add <- paste0("\n\n### Additional Context:\n", context_add_str)
            logs_internos <- c(logs_internos, paste0("     -> Compiled 'add' additional context (", length(elements_add_str_list), " item(s) processed).\n"))
          } else {
            logs_internos <- c(logs_internos, "     -> 'add' additional context was empty or contained no valid items after processing.\n")
          }
        } else {
          logs_internos <- c(logs_internos, "     -> Additional 'add' context provided, but it was an empty list/vector.\n")
        }
      } else {
        logs_internos <- c(logs_internos, "     -> No additional 'add' context provided.\n")
      }

      # Combine all parts into the final prompt string
      # unlist preserves the order defined above (instructions, item_especifico, context_add)
      context_final <- paste(unlist(prompt_final_parts), collapse = "")

      # --- Estimate tokens using .estimate_tokens if available, otherwise simple word count ---
      logs_internos <- c(logs_internos, "  -> Estimating tokens...\n")
      tokens_estimados_envio <- tryCatch(
        {
          # Check if .estimate_tokens function exists in the global environment or package namespace
          if (exists(".estimate_tokens", mode = "function", envir = .GlobalEnv)) {
            # If .estimate_tokens takes only one argument (the prompt)
            formals_est <- formals(.estimate_tokens)
            if (length(formals_est) == 1 || (length(formals_est) > 1 && names(formals_est)[1] == "prompt")) {
              .estimate_tokens(context_final)
            } else {
              # Handle .estimate_tokens with different signature if necessary, or fall back
              logs_internos <<- c(logs_internos, "     -> Unexpected signature of '.estimate_tokens'. Using a simple estimate.\n")
              length(gregexpr("\\W+", context_final)[[1]]) + 1
            }
          } else {
            logs_internos <<- c(logs_internos, "     -> Function '.estimate_tokens' not found. Using simple word-based estimate.\n")
            length(gregexpr("\\W+", context_final)[[1]]) + 1
          }
        },
        error = function(e) {
          logs_internos <<- c(logs_internos, paste0("     -> ERROR estimating tokens: ", conditionMessage(e), ". Using a simple estimate.\n"))
          length(gregexpr("\\W+", context_final)[[1]]) + 1
        }
      )
      logs_internos <- c(logs_internos, paste0("  -> Final prompt built (Estimated Tokens: ~", tokens_estimados_envio, "). Ready for API.\n"))
      # --- End Use .estimate_tokens ---


      # --- 4. Main API Call (Chat or Image) ---
      if (tipo_agente %in% c("chat", "vision")) {
        logs_internos <- c(logs_internos, paste0("  -> Calling gen_txt for ", label_agente, "...\n"))
        runtime_agent <- config_agente
        runtime_agent$context <- context_final
        runtime_agent$instructions <- NULL
        runtime_agent$add <- NULL
        runtime_agent$service <- service_cfg
        runtime_agent$model <- model_cfg
        runtime_agent$temp <- temp_cfg
        runtime_agent$tools <- tools_cfg
        runtime_agent$timeout_api <- timeout_cfg
        class(runtime_agent) <- unique(c("genflow_agent", class(runtime_agent)))
        runtime_args <- .genflow_prepare_agent_args(
          agent = runtime_agent,
          overrides = list(
            context = context_final,
            res_context = FALSE,
            add_img = task_img,
            directory = directory,
            label = label_agente,
            persist = persist
          ),
          target_formals = formals(gen_txt.default),
          required = c("context", "service", "model")
        )
        response_estrutura_api <- do.call(gen_txt.default, runtime_args, quote = TRUE)
      } else if (tipo_agente == "image") {
        logs_internos <- c(logs_internos, paste0("  -> Calling gen_img for ", label_agente, "...\n"))
        h_img <- if (!is_missing_field(config_agente$h)) config_agente$h else get_formal_default(gen_img, "h")
        w_img <- if (!is_missing_field(config_agente$y)) config_agente$y else get_formal_default(gen_img, "y")
        steps_img <- if (!is_missing_field(config_agente$steps)) config_agente$steps else get_formal_default(gen_img, "steps")
        seed_img <- config_agente$seed %||% NULL # Allow seed via config

        model_img <- get_config_value(config_agente, "model", "Model")
        if (is_missing_field(model_img)) {
          model_img <- get_formal_default(gen_img, "model")
          logs_internos <- c(logs_internos, paste0("     -> Model not provided; using default from gen_img(): ", model_img, "\n"))
        }
        model_img <- as.character(model_img)

        temp_img <- if (!is_missing_field(temp_cfg)) temp_cfg else get_formal_default(gen_img, "temp")
        if (is_missing_field(temp_cfg)) {
          logs_internos <- c(logs_internos, paste0("     -> Temp not provided; using default from gen_img(): ", temp_img, "\n"))
        }
        temp_img <- suppressWarnings(as.numeric(temp_img))
        if (length(temp_img) == 0 || is.na(temp_img)) {
          temp_img <- get_formal_default(gen_img, "temp")
        }

        h_img <- suppressWarnings(as.numeric(h_img))
        if (length(h_img) == 0 || is.na(h_img)) h_img <- get_formal_default(gen_img, "h")
        w_img <- suppressWarnings(as.numeric(w_img))
        if (length(w_img) == 0 || is.na(w_img)) w_img <- get_formal_default(gen_img, "y")
        steps_img <- suppressWarnings(as.integer(steps_img))
        if (length(steps_img) == 0 || is.na(steps_img)) steps_img <- get_formal_default(gen_img, "steps")

        logs_internos <- c(logs_internos, paste0("     -> dimensions: h=", h_img, ", w=", w_img, ", Steps=", steps_img, if (!is.null(seed_img)) paste0(", Seed=", seed_img) else "", "\n"))

        # Use the correctly constructed context_final as the prompt for image generation
        response_estrutura_api <- gen_img(
          prompt = context_final, # Use the processed prompt text
          # add = NULL, # Handled
          # item_especifico = NULL, # Handled
          directory = directory_img, # Image directory
          label = label_agente, # Pass the specific agent label
          service = service_cfg,
          model = model_img,
          temp = temp_img,
          steps = steps_img,
          h = h_img,
          y = w_img
        )
      } else {
        # This case should technically not be reached due to earlier check and fallback
        stop(paste0("Agent type logic failed. Unexpected type: '", tipo_agente, "' for label '", label_agente, "'."))
      }

      # --- 5. Basic Check of Result ---
      if (is.null(response_estrutura_api)) {
        logs_internos <- c(logs_internos, "[WARNING] API call (gen_txt/gen_img) returned NULL.\n")
        # If API function returns NULL on *any* error, you might want to set erro_interno here
        # depending on how robust gen_txt/gen_img are in returning a list with status="ERROR".
        # If they always return a list structure even on API errors, this check is mostly for unexpected NULLs.
      } else if (!is.list(response_estrutura_api)) {
        logs_internos <- c(logs_internos, paste0("[STRUCTURAL ERROR] API call returned unexpected type: ", class(response_estrutura_api)[1], ". Expected: list.\n"))
        erro_interno <- paste("Invalid return from API/internal function for", label_agente, ":", class(response_estrutura_api)[1])
      } else if (!is.null(response_estrutura_api$status_api) && response_estrutura_api$status_api == "ERROR") {
        logs_internos <- c(logs_internos, paste0("[INFO] API call status: ERROR. Message: ", response_estrutura_api$status_msg %||% "No details", "\n"))
        # This is an expected API error status, processed by .process_parallel_results
      } else {
        logs_internos <- c(logs_internos, "[INFO] API call returned a valid list structure with OK/unknown status.\n")
      }
    },
    error = function(e) {
      # Captura erros DENTRO de .execute_agent_task (config, file reading issues *if stop() is used*, prompt building issues)
      # Note: If file reading returns an error *string* instead of stopping, it won't trigger this block for that item.
      msg_erro <- paste("Fatal error (tryCatch) in .execute_agent_task for", label_agente, ":", conditionMessage(e))
      # Use <<- to assign the variable in the function's environment
      erro_interno <<- msg_erro
      logs_internos <<- c(logs_internos, paste("!! FATAL INTERNAL ERROR:", msg_erro, "\n"))
      # warning(msg_erro) # Optional: may clutter the console in mclapply/parLapply
    }
  )

  fim_duration <- Sys.time()
  duration <- as.numeric(difftime(fim_duration, inicio_duration, units = "secs"))

  if (!is.null(checkpoint_path)) {
    checkpoint <- list(
      schema_version = 1L,
      task_id = task_id,
      completed_at = format(fim_duration, "%Y-%m-%dT%H:%M:%OS6%z"),
      response = response_estrutura_api,
      error = erro_interno,
      duration_seconds = duration
    )
    tryCatch(
      .genflow_atomic_save_rds(checkpoint, checkpoint_path),
      error = function(e) {
        checkpoint_error <- paste0("Could not save task checkpoint: ", conditionMessage(e))
        erro_interno <<- if (is.null(erro_interno)) checkpoint_error else paste(erro_interno, checkpoint_error, sep = "; ")
        logs_internos <<- c(logs_internos, paste0("!! CHECKPOINT ERROR: ", checkpoint_error, "\n"))
      }
    )
  }

  # Determine final worker status for logging
  status_final_worker <- if (!is.null(erro_interno)) {
    "[Internal Worker Failure]"
  } else if (is.null(response_estrutura_api)) {
    "[API Return Failure (NULL)]"
  } else if (!is.list(response_estrutura_api)) {
    "[API Return Failure (Invalid Type)]"
  } else if (!is.null(response_estrutura_api$status_api) && response_estrutura_api$status_api == "ERROR") {
    "[completed with API ERROR]"
  } else {
    "[completed OK]"
  }

  logs_internos <- c(logs_internos, sprintf(
    "%s Worker label='%s' (%s) finished in %.2fs.\n",
    status_final_worker, label_agente, tipo_agente, duration
  ))


  # --- Return the result structure ---
  # Includes the API response (which may itself be an API error) and any internal worker error
  return(list(
    index = i,
    task_id = task_id,
    label = label_agente,
    tipo_agente = tipo_agente,
    response = response_estrutura_api, # The LIST returned by gen_txt/gen_img (may have status="ERROR") or NULL/wrong type
    erro = erro_interno, # Error that occurred INSIDE this function (or NULL)
    logs = logs_internos,
    duration_individual = duration
  ))
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
    warning("No raw results received from workers.")
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
