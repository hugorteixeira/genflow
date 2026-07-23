#' Summarize a run's results (internal)
#'
#' Prints a simple data frame of key metrics from a list of generation results
#' (e.g., items returned by text/image/video functions). Does not touch global env.
#'
#' @param objects List of result objects. The last element may be a named
#'   `combined_stats` list, which is excluded from row aggregation.
#' @return Invisibly returns a list with:
#'   - `tabela`: data.frame with columns like label, model, temp, duration,
#'     tokens sent/received (if available) and status.
#'   - `total`: total duration for the current call.
#' @keywords internal
#' @noRd
.summarize_results <- function(objects) {
  # 1. Input Validation & Identify Result Elements
  if (!is.list(objects)) {
    warning("gen_stats expects a list as input.")
    return(invisible(list(tabela = data.frame(), total = 0)))
  }

  num_total_elements <- length(objects)
  num_results <- 0 # How many potential result slots are there

  # Check if the last element is the statistics block and adjust count
  if (num_total_elements > 0 && is.list(objects[[num_total_elements]]) &&
    !is.null(names(objects)) && names(objects)[num_total_elements] == "combined_stats") {
    num_results <- num_total_elements - 1
  } else {
    # Assumes all elements are results if no named stats block is found
    # This might happen if qty=0 was called, or if the structure is unexpected
    num_results <- num_total_elements
    if (num_total_elements > 0) {
      warning("The last element does not appear to be the 'combined_stats' block. Processing all ", num_total_elements, " elements as results.")
    }
  }

  # Handle case where there are no result elements (e.g., called with qty=0)
  if (num_results <= 0) {
    stats_vazio <- data.frame(
      label = character(0),
      model = character(0),
      temp = numeric(0),
      duration = numeric(0),
      tks_envia = numeric(0),
      tks_recebe = numeric(0),
      status_api = character(0), # Add status for context
      stringsAsFactors = FALSE
    )
    # Print empty stats summary
    print(stats_vazio)
    cat("Total duration: 0\n")
    return(invisible(list(tabela = stats_vazio, total = 0)))
  }


  # 2. Initialize Data Frame for Local Results
  stats_local <- data.frame(
    label = character(num_results),
    model = character(num_results),
    temp = numeric(num_results),
    duration = numeric(num_results),
    tks_envia = numeric(num_results), # Renamed from tokens_sent
    tks_recebe = numeric(num_results), # Renamed from tokens_received
    status_api = character(num_results), # Add status
    stringsAsFactors = FALSE
  )

  # 3. Iterate Through Result Elements and Extract Data
  for (i in 1:num_results) {
    res_item <- objects[[i]]

    # Check if it's a valid result list (returned by gen_txt/gen_img)
    if (is.list(res_item) && !is.null(res_item$status_api)) { # Check for a key field
      stats_local$label[i] <- res_item$label %||% NA_character_
      stats_local$model[i] <- res_item$model %||% NA_character_
      stats_local$temp[i] <- res_item$temp %||% NA_real_
      stats_local$duration[i] <- res_item$duration %||% NA_real_
      stats_local$tks_envia[i] <- res_item$tokens_sent %||% NA_real_ # Map from new name
      stats_local$tks_recebe[i] <- res_item$tokens_received %||% NA_real_ # Map from new name
      stats_local$status_api[i] <- res_item$status_api %||% "UNKNOWN"
    } else {
      # Handle cases where res_item is NULL or not the expected list
      stats_local$label[i] <- paste0("Index_", i, "Invalid")
      stats_local$status_api[i] <- "ERROR_ESTRUTURA_INTERNA"
      # Fill others with NA
      stats_local$model[i] <- NA_character_
      stats_local$temp[i] <- NA_real_
      stats_local$duration[i] <- NA_real_
      stats_local$tks_envia[i] <- NA_real_
      stats_local$tks_recebe[i] <- NA_real_
    }
  }

  # 4. Calculate Local Total Time
  total_duration_local <- sum(stats_local$duration, na.rm = TRUE)

  # 5. Handle Saving/Printing (Logic remains similar, but acts on the new stats_local)
  # Print only the stats from this run
  print(stats_local)
  cat("Total duration (this call):", total_duration_local, "\n")


  # 6. Return Local Results Invisibly
  invisible(list(tabela = stats_local, total = total_duration_local))
}

#' Read daily generation logs
#'
#' Loads and prints the saved stats for a given date from the directory set by
#' option `genflow.log_dir`, or from
#' `tools::R_user_dir("genflow", which = "data")` when the option is unset.
#'
#' @param date Date or character. If NULL, uses today's date. Accepts
#'   Date objects, "YYYYMMDD", or ISO "YYYY-MM-DD" strings.
#' @return Invisibly returns the data.frame of logs for the date (or empty if none).
#' @examples
#' # gen_stats()              # show today's logs
#' # gen_stats("20250921")    # show logs for 2025-09-21
#' # gen_stats(Sys.Date()-1)  # show yesterday's logs
#' @export
gen_stats <- function(date = NULL) {
  d <- .genflow_parse_stats_date(date)

  dir <- .get_log_dir(create = FALSE)
  fp <- file.path(dir, paste0(format(d, "%Y%m%d"), ".rds"))
  if (!dir.exists(dir) || !file.exists(fp)) {
    message("No logs found for ", format(d, "%Y-%m-%d"), ".")
    return(invisible(.genflow_empty_stats()))
  }

  lock <- .genflow_acquire_stats_lock(fp)
  on.exit(.genflow_release_file_lock(lock), add = TRUE)
  if (!file.exists(fp)) {
    message("No logs found for ", format(d, "%Y-%m-%d"), ".")
    return(invisible(.genflow_empty_stats()))
  }
  df <- tryCatch(
    readRDS(fp),
    error = function(e) NULL
  )
  if (!is.data.frame(df)) {
    warning("Log file corrupted or invalid: ", fp)
    df <- .genflow_empty_stats()
  }
  print(df)
  invisible(df)
}

.genflow_parse_stats_date <- function(date = NULL) {
  if (is.null(date)) {
    value <- Sys.Date()
  } else if (inherits(date, "Date") && length(date) == 1L) {
    value <- date
  } else if (is.character(date) && length(date) == 1L && !is.na(date)) {
    if (grepl("^\\d{8}$", date)) {
      value <- as.Date(date, format = "%Y%m%d")
    } else {
      value <- suppressWarnings(as.Date(date))
    }
  } else {
    stop("date must be NULL, one Date, or one character value", call. = FALSE)
  }
  if (length(value) != 1L || is.na(value)) {
    stop(
      "Could not parse 'date'. Use YYYYMMDD, YYYY-MM-DD, or a Date object.",
      call. = FALSE
    )
  }
  value
}

.genflow_remove_stats_file <- function(path) {
  lock <- .genflow_acquire_stats_lock(path)
  on.exit(.genflow_release_file_lock(lock), add = TRUE)
  if (!file.exists(path)) {
    return(FALSE)
  }
  isTRUE(file.remove(path))
}

#' Remove saved logs
#'
#' Deletes one day's log file or all logs from the directory set by option
#' `genflow.log_dir`, or from
#' `tools::R_user_dir("genflow", which = "data")` when the option is unset.
#'
#' @param date Date or character. If NULL, deletes all logs. If set, deletes only that day's file.
#' @return Invisibly returns TRUE if deletion occurred, FALSE otherwise.
#' @export
gen_stats_rm <- function(date = NULL) {
  dir <- .get_log_dir(create = FALSE)
  if (!dir.exists(dir)) {
    return(invisible(FALSE))
  }
  if (is.null(date)) {
    files <- list.files(dir, pattern = "^[0-9]{8}\\.rds$", full.names = TRUE)
    if (length(files) == 0) {
      return(invisible(FALSE))
    }
    removed <- vapply(files, .genflow_remove_stats_file, logical(1))
    if (any(removed)) {
      message("Removed ", sum(removed), " log file(s).")
    }
    if (any(!removed)) {
      warning("Some statistics log files could not be removed.", call. = FALSE)
    }
    return(invisible(all(removed)))
  }
  d <- .genflow_parse_stats_date(date)
  fp <- file.path(dir, paste0(format(d, "%Y%m%d"), ".rds"))
  if (!file.exists(fp)) {
    return(invisible(FALSE))
  }
  ok <- .genflow_remove_stats_file(fp)
  if (ok) message("Removed ", basename(fp), ".")
  invisible(ok)
}

#' Print timing metrics and (optionally) detailed logs (internal)
#'
#' @keywords internal
#' @noRd
.print_metric_logs <- function(logs_completos, inicio_geral, final_geral,
                               single_durations, agent_types, qty, log = TRUE) {

  # --- Conditionally Print Detailed Logs ---
  if (log) {
    cat("\n--- Detailed Logs ---\n")
    # Check if logs_completos is NULL or empty before printing
    if (!is.null(logs_completos) && nzchar(trimws(logs_completos))) {
      cat(logs_completos, sep = "")
    } else {
      cat("(No detailed log to display)\n")
    }
    cat("--- End of Logs ---\n\n")
  } else {
    # Optionally, print a message indicating suppression
    cat("\n(Detailed logs suppressed by log=FALSE)\n\n")
  }
  # --- End Conditional Log Printing ---

  # --- Print Metrics ---
  cat("--- Timing Metrics ---\n")
  duration_real_decorrido <- difftime(final_geral, inicio_geral, units = "secs")
  duration_total_somado_num <- sum(single_durations, na.rm = TRUE) # Ensure numeric sum

  cat("Elapsed Wall Time (Parallel):", round(as.numeric(duration_real_decorrido), 2), "seconds.\n")
  cat("Total Summed Time (Estimated):", round(duration_total_somado_num, 2), "seconds.\n")

  # Check for valid, positive times before calculating speedup
  if (!is.na(duration_real_decorrido) && as.numeric(duration_real_decorrido) > 0 &&
    !is.na(duration_total_somado_num) && duration_total_somado_num > 0) {
    fator_speedup <- duration_total_somado_num / as.numeric(duration_real_decorrido)
    cat("Speedup Factor (approx.):", round(fator_speedup, 2), "x\n")
  } else {
    cat("Speedup Factor: not computable (zero or NA duration).\n")
  }

  cat("\n--- Task Summary ---\n")
  # Check if agent_types has valid data before creating table
  if (!is.null(agent_types) && length(agent_types) > 0 && any(!is.na(agent_types))) {
    # Filter out potential NA values before table creation if necessary
    tipos_validos <- agent_types[!is.na(agent_types)]
    if (length(tipos_validos) > 0) {
      task_summary <- table(tipos_validos)
      for (type_name in names(task_summary)) {
        cat("tasks '", type_name, "': ", task_summary[[type_name]], "\n", sep = "")
      }
    } else {
      cat("(No valid agent type recorded after filtering NA)\n")
    }
  } else {
    cat("(No agent type recorded or all are NA)\n")
  }

  cat("Total tasks requested:", qty, "\n")
  cat("--- End of Metrics ---\n\n")
}
#' Report worker errors in a readable format (internal)
#'
#' @keywords internal
#' @noRd
.report_errors <- function(erros, qty, agent_prefix, suffix_type) { # Adicionado agent_prefix e suffix_type
  indices_erro <- which(!sapply(erros, is.null))
  if (length(indices_erro) > 0) {
    cat("--- Found Errors or Worker Failures (", length(indices_erro), "/", qty, ") ---\n")
    for (idx in indices_erro) {
      # Rebuild the expected label for the index with error
      label_esperado <- if (suffix_type == "alphabetic") {
        if (idx <= length(letters)) paste0(agent_prefix, letters[idx]) else paste0(agent_prefix, "extra", idx)
      } else {
        paste0(agent_prefix, idx)
      }
      # Attempt to get the agent type (may be 'worker_failed_or_unknown') - requires processed types
      # For simplicity, report the error and expected label
      cat("Index ", idx, " (Label esperado '", label_esperado, "'): ", erros[[idx]], "\n", sep = "")
    }
    cat("--- End of Errors ---\n\n")
  } else {
    cat("--- Status: No errors detected. ---\n\n")
  }
}
#' Post-process results for quick visualization (internal)
#'
#' @keywords internal
#' @noRd
.pos_process_results <- function(results,
                                 persist = TRUE,
                                 verbose = TRUE,
                                 persist_results = results) {
  if (verbose) cat("--- Post-Processing and results ---\n")
  # cat("WARNING: results may be text/images. 'mostrar_text'/'gen_stats' may need adaptation.\n\n")
  if (verbose && exists("gen_view")) {
    # cat("Tentando 'mostrar_text'...\n")
    try(gen_view(results), silent = TRUE)
  } else if (verbose) {
    warning("'mostrar_text' not found.")
  }
  # Sys.sleep(0.5)
  if (verbose && exists(".summarize_results")) {
    # Print local summary without touching persisted logs/global env
    try(.summarize_results(results), silent = TRUE)
  } else if (verbose) {
    warning("'.summarize_results' not found.")
  }
  # Persist stats for all results in this batch (single-writer in main process)
  if (isTRUE(persist)) {
    tryCatch(
      .persist_many_stats(persist_results),
      error = function(e) {
        warning(
          "Batch results were produced, but their statistics could not be persisted: ",
          conditionMessage(e),
          call. = FALSE
        )
      }
    )
  }
  if (verbose) cat("--- End of Post-Processing ---\n\n")
}

.genflow_positive_integer <- function(value, arg) {
  valid <- is.numeric(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    is.finite(value) &&
    value >= 1 &&
    value <= .Machine$integer.max &&
    value == floor(value)
  if (!isTRUE(valid)) {
    stop("`", arg, "` must be a positive integer.", call. = FALSE)
  }
  as.integer(value)
}

.genflow_resolve_workers <- function(workers, qty) {
  qty <- .genflow_positive_integer(qty, "qty")
  if (is.null(workers)) {
    detected <- suppressWarnings(tryCatch(
      parallel::detectCores(),
      error = function(e) NA_real_
    ))
    detected_is_valid <- is.numeric(detected) &&
      length(detected) == 1L &&
      !is.na(detected) &&
      is.finite(detected) &&
      detected >= 2 &&
      detected == floor(detected)
    available <- if (!isTRUE(detected_is_valid)) {
      1L
    } else {
      as.integer(min(detected - 1, .Machine$integer.max))
    }
    default_cap_raw <- getOption("genflow.batch_max_workers", 4L)
    default_cap <- tryCatch(
      .genflow_positive_integer(default_cap_raw, "genflow.batch_max_workers"),
      error = function(e) {
        warning(
          "Option `genflow.batch_max_workers` must be a positive integer; using 4.",
          call. = FALSE
        )
        4L
      }
    )
    return(min(qty, available, default_cap))
  }
  workers <- tryCatch(
    .genflow_positive_integer(workers, "workers"),
    error = function(e) {
      stop("`workers` must be NULL or a positive integer.", call. = FALSE)
    }
  )
  min(qty, workers)
}

#' Resolve the parallel backend used by a batch
#'
#' Socket workers are the safe default for provider calls. Forking a process
#' after libcurl or another multithreaded native library has been initialized
#' can crash the child before R has a chance to return a structured error.
#' @noRd
.genflow_resolve_backend <- function(backend = c("psock", "fork"), workers) {
  backend <- match.arg(backend)
  if (as.integer(workers) <= 1L) return("serial")
  if (identical(backend, "fork") && .Platform$OS.type == "windows") {
    stop("`backend = \"fork\"` is not available on Windows.", call. = FALSE)
  }
  backend
}

#' Run forked tasks with hard cleanup on interruption
#'
#' Provider calls can remain blocked in native network code after the main R
#' process is interrupted. `mclapply()` normally sends `SIGTERM`, which can
#' leave those children alive long enough to block an RStudio session restart.
#' Use `SIGKILL` only for interrupted fork cleanup; normal completion and
#' result collection are unchanged.
#' @noRd
.genflow_mclapply <- function(indices,
                              task,
                              workers,
                              mclapply_fn = parallel::mclapply) {
  mclapply_fn(
    indices,
    task,
    mc.cores = workers,
    mc.silent = FALSE,
    mc.preschedule = FALSE,
    mc.cleanup = tools::SIGKILL
  )
}

.genflow_normalize_each <- function(x, qty, arg, paths = FALSE) {
  if (is.null(x)) return(NULL)
  if (is.atomic(x) && !is.list(x)) x <- as.list(x)
  if (!is.list(x)) stop("`", arg, "` must be a list or atomic vector.", call. = FALSE)
  if (length(x) != qty) {
    stop("`", arg, "` must contain exactly ", qty, " item(s).", call. = FALSE)
  }
  if (paths) {
    valid <- vapply(x, function(item) {
      is.null(item) || (is.character(item) && length(item) == 1L && !is.na(item) && nzchar(item))
    }, logical(1))
    if (!all(valid)) {
      stop("Every non-NULL `", arg, "` item must be one non-empty file path.", call. = FALSE)
    }
  }
  x
}
#' Run multiple generation tasks in parallel
#'
#' Orchestrates parallel generation tasks using `parallel`, collects results,
#' prints timing metrics and errors, and returns a list that optionally includes
#' a `combined_stats` block. Parallel batches use independent PSOCK processes by
#' default on every operating system.
#'
#' @details PSOCK is the safe default for HTTP/API workloads because it does not
#'   fork the current R process after native networking libraries have already
#'   been initialized. On Unix-like systems, `backend = "fork"` remains
#'   available as an explicit opt-in for workloads known to be fork-safe.
#'   Interrupting an opted-in fork batch forcefully cleans up its child
#'   processes. Completed per-task checkpoints remain recoverable.
#'
#'   `always_fix_errors` reuses successful results from a matching failed batch
#'   within the current R session. Files in `checkpoint_each` provide durable,
#'   caller-owned recovery after an interruption; they are not loaded
#'   automatically because a file path alone cannot prove that a checkpoint
#'   belongs to the current task payload.
#'
#' @param qty Integer number of tasks to run.
#' @param instructions Character base prompt/context text. When `NULL`, the
#'   agent's stored `context` (if any) is used.
#' @param add Optional additional context mixed into the prompt per worker. When
#'   omitted, the agent's stored `add` value (if any) is used.
#' @param one_item_each Optional list of per-worker items to include in prompts.
#' @param add_img Optional image input for vision-capable providers.
#' @param add_img_each Optional list or character vector containing one image
#'   path per task. It is mutually exclusive with `add_img`. Names, when
#'   supplied, must be complete and unique, are preserved on the returned task
#'   results, and cannot use the reserved name `combined_stats`.
#' @param append Character vector or named list (values: `"before"`, `"after"`,
#'   `"replace"`) controlling how supplied `instructions` and `add` merge with
#'   each agent's stored context/addition. The first entry applies to
#'   instructions, the second to add data.
#' @param agent_prefix Character prefix used to locate legacy per-task agent
#'   configs in `.GlobalEnv`, or to label results when `agent` is supplied.
#' @param agent Optional single `genflow_agent` used for every task. Prefer
#'   [gen_batch_agent()] when starting from an agent object.
#' @param workers Maximum number of tasks to execute simultaneously. `NULL`
#'   uses automatic detection capped by option `genflow.batch_max_workers`
#'   (default: 4). An explicit value is capped by `qty`, not by the number of
#'   CPU cores, because API calls are generally I/O-bound.
#' @param backend Parallel process backend. `"psock"` (the default) starts
#'   independent R worker processes and is safe for HTTP clients such as
#'   `curl`/`httr`. `"fork"` is an explicit Unix-only opt-in for workloads known
#'   to be fork-safe. A batch with one worker always runs serially.
#' @param persist Logical; whether generator response artifacts and aggregate
#'   generation statistics should be saved.
#' @param verbose Logical; whether to print batch progress and summaries.
#' @param checkpoint_each Optional list or character vector with one `.rds`
#'   path per task. Each worker atomically writes its structured result there
#'   as soon as it finishes, allowing a caller-owned cache to recover after an
#'   interrupted batch.
#' @param directory Character path to save chat/text artifacts.
#' @param directory_img Character path to save images.
#' @param log Logical; if TRUE, prints detailed logs.
#' @param always_fix_errors Logical; when TRUE (default) reuses successful results
#'   from the previous run with the same signature and only re-executes failed
#'   indices until the batch completes without errors.
#'
#' @return A list of length `qty + 1`, where the last element is named
#'   `combined_stats` with aggregated timing and counts.
#'
#' @examples
#' # gen_batch(2, instructions = "Describe a cat", agent_prefix = "agent")
#'
#' @export
gen_batch <- function(qty = 8,
                      instructions = NULL,
                      add = NULL,
                      one_item_each = NULL,
                      add_img = NULL,
                      append = c("before", "before"),
                      agent_prefix = NULL,
                      directory = "content",
                      directory_img = "content",
                      log = FALSE,
                      always_fix_errors = TRUE,
                      agent = NULL,
                      workers = NULL,
                      backend = c("psock", "fork"),
                      add_img_each = NULL,
                      persist = TRUE,
                      verbose = TRUE,
                      checkpoint_each = NULL) {

  inicio_geral <- Sys.time()
  if (!requireNamespace("parallel", quietly = TRUE)) stop("Package 'parallel' needed.")
  qty <- .genflow_positive_integer(qty, "qty")
  if (!is.logical(persist) || length(persist) != 1L || is.na(persist)) {
    stop("`persist` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) {
    stop("`verbose` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(log) || length(log) != 1L || is.na(log)) {
    stop("`log` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(always_fix_errors) || length(always_fix_errors) != 1L ||
    is.na(always_fix_errors)) {
    stop("`always_fix_errors` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.null(agent) && !inherits(agent, "genflow_agent")) {
    stop("`agent` must be a genflow_agent object.", call. = FALSE)
  }
  valid_agent_prefix <- function(value) {
    is.character(value) &&
      length(value) == 1L &&
      !is.na(value) &&
      nzchar(value)
  }
  if (is.null(agent) && !valid_agent_prefix(agent_prefix)) {
    stop("`agent_prefix` is required when `agent` is not supplied.", call. = FALSE)
  }
  if (!is.null(agent) && !valid_agent_prefix(agent_prefix)) {
    agent_label <- .genflow_agent_label(agent)
    agent_prefix <- if (valid_agent_prefix(agent_label)) agent_label else "agent"
  }
  worker_limit <- .genflow_resolve_workers(workers, qty)
  backend_requested <- match.arg(backend)
  add_img_each <- .genflow_normalize_each(add_img_each, qty, "add_img_each", paths = TRUE)
  checkpoint_each <- .genflow_normalize_each(checkpoint_each, qty, "checkpoint_each", paths = TRUE)
  if (!is.null(checkpoint_each)) {
    checkpoint_paths <- unlist(checkpoint_each[!vapply(checkpoint_each, is.null, logical(1))], use.names = FALSE)
    if (length(checkpoint_paths)) {
      canonical_checkpoints <- normalizePath(
        path.expand(checkpoint_paths),
        winslash = "/",
        mustWork = FALSE
      )
      if (anyDuplicated(canonical_checkpoints)) {
        stop("Non-NULL `checkpoint_each` paths must be unique.", call. = FALSE)
      }
    }
  }
  if (!is.null(add_img) && !is.null(add_img_each)) {
    stop("Use either `add_img` or `add_img_each`, not both.", call. = FALSE)
  }
  if (!is.null(add_img_each) && !is.null(names(add_img_each))) {
    item_names <- names(add_img_each)
    named <- !is.na(item_names) & nzchar(item_names)
    if (!all(named) || anyDuplicated(item_names)) {
      stop("Names on `add_img_each` must be complete and unique.", call. = FALSE)
    }
    if ("combined_stats" %in% item_names) {
      stop("`combined_stats` is reserved and cannot name an `add_img_each` task.", call. = FALSE)
    }
  }
  add_missing <- missing(add)
  add_img_missing <- missing(add_img)
  one_item_each_missing <- missing(one_item_each)

  normalize_append_argument <- function(arg) {
    allowed <- c("before", "after", "replace")
    default <- list(instructions = "replace", add = "replace")
    if (is.null(arg)) {
      return(default)
    }
    values <- arg
    if (is.list(values)) {
      values <- unlist(values, use.names = TRUE)
    }
    if (!is.vector(values)) {
      stop("`append` must be NULL, a character vector, or a named list.", call. = FALSE)
    }
    if (length(values) == 0) {
      return(default)
    }
    if (is.null(names(values)) || all(names(values) == "")) {
      values <- as.character(values)
      if (length(values) == 1) {
        values <- c(instructions = values[1], add = values[1])
      } else {
        values <- values[seq_len(min(2, length(values)))]
        names(values) <- c("instructions", "add")[seq_along(values)]
        if (length(values) == 1) {
          values <- c(values, add = values[1])
        }
      }
    } else {
      values <- as.character(values)
      missing_names <- names(values) == ""
      if (any(missing_names)) {
        fallback_names <- c("instructions", "add")
        for (idx in which(missing_names)) {
          fallback_idx <- min(idx, length(fallback_names))
          names(values)[idx] <- fallback_names[fallback_idx]
        }
      }
    }
    result <- default
    for (nm_raw in names(values)) {
      nm <- tolower(nm_raw)
      target <- if (startsWith(nm, "instr")) {
        "instructions"
      } else if (startsWith(nm, "add")) {
        "add"
      } else {
        NULL
      }
      if (is.null(target)) {
        next
      }
      val <- tolower(values[[nm_raw]])
      if (!val %in% allowed) {
        warning("Invalid append mode '", values[[nm_raw]], "' for ", target, ". Using 'replace'.")
        val <- "replace"
      }
      result[[target]] <- val
    }
    result
  }
  append_modes <- normalize_append_argument(append)

  if (is.list(instructions)) {
    instructions_obj <- instructions
    context_candidate <- instructions_obj$context %||% instructions_obj$instructions %||% NULL
    if (!is.null(context_candidate)) {
      if (verbose) message("Detected list-based instructions. Extracting context and optional fields...")
      instructions <- context_candidate
      if ((add_missing || is.null(add)) && !is.null(instructions_obj$add)) {
        add <- instructions_obj$add
      }
      if ((add_img_missing || is.null(add_img)) && !is.null(instructions_obj$add_img)) {
        add_img <- instructions_obj$add_img
      }
      if ((one_item_each_missing || is.null(one_item_each)) && !is.null(instructions_obj$one_item_each)) {
        one_item_each <- instructions_obj$one_item_each
      }
    }
  }

  if (!is.null(add_img) && !is.null(add_img_each)) {
    stop("Use either `add_img` or `add_img_each`, not both.", call. = FALSE)
  }
  if (verbose) {
    cat("Preparing to execute", qty, "tasks with prefix '", agent_prefix, "' using up to", worker_limit, "workers.\n")
  }

  # Input validation for one_item_each
  if (!is.null(one_item_each)) {
    if (!is.list(one_item_each)) stop("'one_item_each' must be a list.")
    if (length(one_item_each) < qty) {
      stop(paste0("'one_item_each' (", length(one_item_each), ") has fewer elements than 'qty' (", qty, ")."))
    } else if (length(one_item_each) > qty) {
      if (verbose) message(paste0("WARNING: 'one_item_each' (", length(one_item_each), ") has more elements than 'qty' (", qty, "). Using only the first ", qty, "."))
      # Loop 1:qty will handle using only the first qty
    }
    if (verbose) message("Using 'one_item_each' to provide individual data to each worker.")
    one_item_each <- one_item_each[seq_len(qty)]
  }

  # Detect suffix, sanitize instructions, create directories
  suffix_type <- if (!is.null(agent)) {
    "numeric"
  } else {
    tryCatch(.detect_suffix_type(agent_prefix, qty), error = function(e) {
      stop("Error detecting agent suffix (check if agents like '", agent_prefix, "1' or '", agent_prefix, "a' exist in .GlobalEnv): ", conditionMessage(e))
    })
  }
  instructions <- .sanitize_instructions(instructions)
  if (isTRUE(persist)) .create_directories(directory, directory_img)

  pad_list <- function(lst, len) {
    if (is.null(lst)) lst <- list()
    if (length(lst) < len) lst <- c(lst, vector("list", len - length(lst)))
    lst
  }
  pad_numeric <- function(vec, len, fill = NA_real_) {
    if (is.null(vec)) vec <- numeric()
    if (length(vec) < len) vec <- c(vec, rep(fill, len - length(vec)))
    vec
  }
  pad_character <- function(vec, len, fill = "unknown") {
    if (is.null(vec)) vec <- character()
    if (length(vec) < len) vec <- c(vec, rep(fill, len - length(vec)))
    vec
  }

  agent_signature <- NULL
  use_cache <- isTRUE(always_fix_errors)
  cache_key <- NULL
  cache_entry <- NULL
  indices_to_run <- seq_len(qty)
  reused_indices <- integer(0)
  if (use_cache) {
    agent_signature <- if (!is.null(agent)) {
      as.list(agent)
    } else {
      lapply(seq_len(qty), function(idx) {
        label <- if (suffix_type == "alphabetic") {
          if (idx > 0 && idx <= length(letters)) paste0(agent_prefix, letters[idx]) else paste0(agent_prefix, "invalid_idx_", idx)
        } else {
          paste0(agent_prefix, idx)
        }
        if (!exists(label, envir = .GlobalEnv)) return(NULL)
        cfg <- get(label, envir = .GlobalEnv)
        if (!is.list(cfg)) return(NULL)
        as.list(cfg)
      })
    }
    cache_key <- .batch_cache_make_key(
      agent_prefix = agent_prefix,
      qty = qty,
      instructions = instructions,
      add = add,
      one_item_each = if (!is.null(one_item_each)) one_item_each else NULL,
      add_img = add_img,
      add_img_each = add_img_each,
      checkpoint_each = checkpoint_each,
      directory = directory,
      directory_img = directory_img,
      append_modes = append_modes,
      agent_signature = agent_signature,
      persist = persist
    )
    if (is.null(cache_key)) {
      if (verbose) message("always_fix_errors enabled but cache key could not be generated; running full batch.")
      use_cache <- FALSE
    } else {
      cache_entry <- .batch_cache_get(cache_key)
      if (!is.null(cache_entry)) {
        cache_entry$results <- pad_list(cache_entry$results, qty)
        cache_entry$errors <- pad_list(cache_entry$errors, qty)
        cache_entry$logs <- pad_list(cache_entry$logs, qty)
        cache_entry$agent_types <- pad_character(cache_entry$agent_types, qty, "unknown")
        cache_entry$durations <- pad_numeric(cache_entry$durations, qty)
        if (!identical(cache_entry$qty, qty)) {
          .batch_cache_clear(cache_key)
          cache_entry <- NULL
        } else {
          pending_indices <- which(vapply(seq_len(qty), function(idx) {
            is.null(cache_entry$results[[idx]]) || !is.null(cache_entry$errors[[idx]])
          }, logical(1)))
          if (length(pending_indices) > 0) {
            indices_to_run <- pending_indices
            reused_indices <- setdiff(seq_len(qty), pending_indices)
            if (verbose && length(reused_indices) > 0) {
              message(
                "always_fix_errors: reusing ", length(reused_indices), " successful result(s): ",
                paste(reused_indices, collapse = ", ")
              )
            }
          } else {
            .batch_cache_clear(cache_key)
            cache_entry <- NULL
          }
        }
      }
    }
  }

  execution_workers <- if (length(indices_to_run) == 0L) {
    0L
  } else {
    min(worker_limit, length(indices_to_run))
  }
  parallel_backend <- if (execution_workers == 0L) {
    "serial"
  } else {
    .genflow_resolve_backend(backend_requested, execution_workers)
  }

  # Parallel execution setup
  raw_results <- list()
  cl <- NULL

  # Execute tasks. PSOCK is deliberately the cross-platform default because
  # provider clients rely on native networking libraries that are not fork-safe.
  if (length(indices_to_run) == 0) {
    if (verbose) cat("No pending indices detected; skipping execution and reusing cached results.\n")
  } else if (identical(parallel_backend, "psock")) {
    if (verbose) cat("Using PSOCK parLapplyLB with", execution_workers, "workers...\n")
    cl <- parallel::makeCluster(execution_workers, type = "PSOCK")
    tryCatch({
      .export_cluster_vars(
        cl = cl,
        qty = qty,
        agent_prefix = agent_prefix,
        suffix_type = suffix_type,
        instructions = instructions,
        add = add,
        add_img = add_img,
        add_img_each = add_img_each,
        one_item_each = one_item_each,
        append_modes = append_modes,
        directory = directory,
        directory_img = directory_img,
        agent = agent,
        persist = persist,
        checkpoint_each = checkpoint_each
      )
      raw_results <- parallel::parLapplyLB(cl, indices_to_run, function(i) {
        tryCatch(
          .execute_agent_task(
            i, one_item_each, instructions, add, add_img, directory,
            directory_img, agent_prefix, suffix_type, append_modes,
            agent = agent, add_img_each = add_img_each, persist = persist,
            checkpoint_each = checkpoint_each
          ),
          error = function(e) {
            structure(paste("Error in worker", i, ":", conditionMessage(e)), class = "try-error")
          }
        )
      })
    }, finally = {
      if (!is.null(cl)) try(parallel::stopCluster(cl), silent = TRUE)
      cl <- NULL
    })
  } else if (identical(parallel_backend, "fork")) {
    if (verbose) {
      cat("Using", execution_workers, "fork workers (explicit opt-in)...\n")
    }
    raw_results <- .genflow_mclapply(indices_to_run, function(i) {
      tryCatch(
        .execute_agent_task(
          i, one_item_each, instructions, add, add_img, directory,
          directory_img, agent_prefix, suffix_type, append_modes,
          agent = agent, add_img_each = add_img_each, persist = persist,
          checkpoint_each = checkpoint_each
        ),
        error = function(e) {
          structure(paste("Error in worker", i, ":", conditionMessage(e)), class = "try-error")
        }
      )
    }, workers = execution_workers)
  } else {
    if (verbose) cat("Using serial worker...\n")
    raw_results <- lapply(indices_to_run, function(i) {
      tryCatch(
        .execute_agent_task(
          i, one_item_each, instructions, add, add_img, directory,
          directory_img, agent_prefix, suffix_type, append_modes,
          agent = agent, add_img_each = add_img_each, persist = persist,
          checkpoint_each = checkpoint_each
        ),
        error = function(e) {
          structure(paste("Error in worker", i, ":", conditionMessage(e)), class = "try-error")
        }
      )
    })
  }
  final_geral <- Sys.time()
  if (verbose) cat("Parallel processing completed.\n")

  if (verbose) cat("Processing worker results...\n")
  results_processed <- .process_parallel_results(
    raw_results,
    qty,
    agent_prefix,
    suffix_type,
    expected_indices = indices_to_run
  )
  results_processed$results <- pad_list(results_processed$results, qty)
  results_processed$erros <- pad_list(results_processed$erros, qty)
  results_processed$logs_list <- pad_list(results_processed$logs_list, qty)
  results_processed$single_durations <- pad_numeric(results_processed$single_durations, qty)
  results_processed$agent_types <- pad_character(results_processed$agent_types, qty, "unknown")
  if (!is.null(cache_entry) && length(reused_indices) > 0) {
    for (idx in reused_indices) {
      results_processed$results[idx] <- cache_entry$results[idx]
      results_processed$erros[idx] <- cache_entry$errors[idx]
      if (!is.null(cache_entry$agent_types[idx]) && !is.na(cache_entry$agent_types[idx])) {
        results_processed$agent_types[idx] <- cache_entry$agent_types[idx]
      }
      if (!is.null(cache_entry$durations[idx]) && !is.na(cache_entry$durations[idx])) {
        results_processed$single_durations[idx] <- cache_entry$durations[idx]
      }
      cached_logs <- cache_entry$logs[idx]
      if (is.null(cached_logs) || length(cached_logs) == 0) {
        cached_logs <- list("[INFO] Reused cached result (always_fix_errors enabled).\n")
      }
      results_processed$logs_list[idx] <- cached_logs
    }
  }
  results_processed$valid_results_count <- sum(vapply(results_processed$erros, is.null, logical(1)))

  # Extrai os componentes principais
  results_individual_lists <- results_processed$results
  final_errors <- results_processed$erros
  logs_list <- results_processed$logs_list
  single_durations <- results_processed$single_durations
  agent_types <- results_processed$agent_types

  # ----- Imprimir Logs e Metrics -----
  logs_for_print <- paste(unlist(logs_list), collapse = "")
  # Pass the 'log' argument here
  if (verbose) {
    .print_metric_logs(logs_for_print, inicio_geral, final_geral, single_durations, agent_types, qty, log = log)
    .report_errors(erros = final_errors, qty = qty, agent_prefix = agent_prefix, suffix_type = suffix_type)
  }

  # ----- Build Final Return Object -----
  if (verbose) cat("--- Building Final Return Object (", qty + 1, " elements) ---\n")
  parallel_mode <- switch(
    parallel_backend,
    psock = "parLapplyLB",
    fork = "mclapply",
    serial = "serial"
  )
  combined_stats <- list(
    duration_real_secs = as.numeric(difftime(final_geral, inicio_geral, units = "secs")),
    duration_sum_secs = sum(single_durations, na.rm = TRUE),
    cores_number = execution_workers,
    workers_requested = if (is.null(workers)) NA_integer_ else as.integer(workers),
    workers_used = execution_workers,
    backend_requested = backend_requested,
    parallel_mode = parallel_mode,
    valid_results = results_processed$valid_results_count,
    qty_solicited = qty,
    detailed_errors = final_errors,
    single_durations = single_durations,
    agent_types = agent_types,
    executed_indices = indices_to_run,
    reused_indices = reused_indices
    # logs_detalhados = logs_list # Optionally include raw logs here if needed, even if not printed
  )
  results_final_obj <- vector("list", qty + 1)
  for (i in 1:qty) {
    results_final_obj[i] <- results_individual_lists[i]
  }
  results_final_obj[[qty + 1]] <- combined_stats
  result_names <- rep("", qty)
  if (!is.null(add_img_each) && !is.null(names(add_img_each)) &&
    all(!is.na(names(add_img_each)) & nzchar(names(add_img_each)))) {
    result_names <- names(add_img_each)
  }
  names(results_final_obj) <- c(result_names, "combined_stats")

  if (use_cache && !is.null(cache_key)) {
    existing_entry <- cache_entry
    merged_results <- pad_list(if (!is.null(existing_entry) && !is.null(existing_entry$results)) existing_entry$results else list(), qty)
    merged_errors <- pad_list(if (!is.null(existing_entry) && !is.null(existing_entry$errors)) existing_entry$errors else list(), qty)
    merged_agent_types <- pad_character(if (!is.null(existing_entry) && !is.null(existing_entry$agent_types)) existing_entry$agent_types else character(), qty, "unknown")
    merged_durations <- pad_numeric(if (!is.null(existing_entry) && !is.null(existing_entry$durations)) existing_entry$durations else numeric(), qty)
    merged_logs <- pad_list(if (!is.null(existing_entry) && !is.null(existing_entry$logs)) existing_entry$logs else list(), qty)
    for (idx in seq_len(qty)) {
      merged_results[idx] <- results_processed$results[idx]
      merged_errors[idx] <- results_processed$erros[idx]
      merged_agent_types[idx] <- results_processed$agent_types[idx]
      merged_durations[idx] <- results_processed$single_durations[idx]
      merged_logs[idx] <- results_processed$logs_list[idx]
    }
    previous_attempts <- if (!is.null(existing_entry) && !is.null(existing_entry$attempts)) existing_entry$attempts else 0
    entry_to_store <- list(
      qty = qty,
      results = merged_results,
      errors = merged_errors,
      agent_types = merged_agent_types,
      durations = merged_durations,
      logs = merged_logs,
      timestamp = Sys.time(),
      attempts = previous_attempts + 1
    )
    if (any(!sapply(merged_errors, is.null))) {
      .batch_cache_set(cache_key, entry_to_store)
    } else {
      .batch_cache_clear(cache_key)
    }
  }

  # ----- Post-Processing -----
  .pos_process_results(
    results_final_obj,
    persist = persist,
    verbose = verbose,
    persist_results = results_final_obj[indices_to_run]
  )

  if (verbose) cat("--- End of content generation ---\n")
  return(results_final_obj)
}
