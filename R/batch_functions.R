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
      label      = character(0),
      model     = character(0),
      temp       = numeric(0),
      duration      = numeric(0),
      tks_envia  = numeric(0), # Match original column names
      tks_recebe = numeric(0), # Match original column names
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
    label      = character(num_results),
    model     = character(num_results),
    temp       = numeric(num_results),
    duration      = numeric(num_results),
    tks_envia  = numeric(num_results), # Renamed from tokens_sent
    tks_recebe = numeric(num_results), # Renamed from tokens_received
    status_api = character(num_results), # Add status
    stringsAsFactors = FALSE
  )

  # 3. Iterate Through Result Elements and Extract Data
  for (i in 1:num_results) {
    res_item <- objects[[i]]

    # Check if it's a valid result list (returned by gen_txt/gen_img)
    if (is.list(res_item) && !is.null(res_item$status_api)) { # Check for a key field
      stats_local$label[i]      <- res_item$label %||% NA_character_
      stats_local$model[i]     <- res_item$model %||% NA_character_
      stats_local$temp[i]       <- res_item$temp %||% NA_real_
      stats_local$duration[i]      <- res_item$duration %||% NA_real_
      stats_local$tks_envia[i]  <- res_item$tokens_sent %||% NA_real_ # Map from new name
      stats_local$tks_recebe[i] <- res_item$tokens_received %||% NA_real_ # Map from new name
      stats_local$status_api[i] <- res_item$status_api %||% "UNKNOWN"
    } else {
      # Handle cases where res_item is NULL or not the expected list
      stats_local$label[i]      <- paste0("Index_", i, "Invalid")
      stats_local$status_api[i] <- "ERROR_ESTRUTURA_INTERNA"
      # Fill others with NA
      stats_local$model[i]     <- NA_character_
      stats_local$temp[i]       <- NA_real_
      stats_local$duration[i]      <- NA_real_
      stats_local$tks_envia[i]  <- NA_real_
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
#' Loads and prints the saved stats for a given date from
#' `tools::R_user_dir("genflow", which = "data")`.
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
  # Parse date
  if (is.null(date)) {
    d <- Sys.Date()
  } else if (inherits(date, "Date")) {
    d <- date
  } else if (is.character(date)) {
    if (grepl("^\\d{8}$", date)) {
      d <- as.Date(date, format = "%Y%m%d")
    } else {
      d <- as.Date(date)
    }
  } else {
    stop("date must be NULL, Date, or character")
  }
  if (is.na(d)) stop("Could not parse 'date'. Use YYYYMMDD or YYYY-MM-DD or Date object.")

  dir <- tools::R_user_dir("genflow", which = "data")
  fp <- file.path(dir, paste0(format(d, "%Y%m%d"), ".rds"))
  if (!file.exists(fp)) {
    message("No logs found for ", format(d, "%Y-%m-%d"), ".")
    df_empty <- data.frame(label=character(0), model=character(0), temp=numeric(0), duration=numeric(0), tks_envia=numeric(0), tks_recebe=numeric(0), status_api=character(0), stringsAsFactors = FALSE)
    return(invisible(df_empty))
  }
  df <- tryCatch(readRDS(fp), error = function(e) NULL)
  if (!is.data.frame(df)) {
    warning("Log file corrupted or invalid: ", fp)
    df <- data.frame(label=character(0), model=character(0), temp=numeric(0), duration=numeric(0), tks_envia=numeric(0), tks_recebe=numeric(0), status_api=character(0), stringsAsFactors = FALSE)
  }
  print(df)
  invisible(df)
}

#' Remove saved logs
#'
#' Deletes one day's log file or all logs from
#' `tools::R_user_dir("genflow", which = "data")`.
#'
#' @param date Date or character. If NULL, deletes all logs. If set, deletes only that day's file.
#' @return Invisibly returns TRUE if deletion occurred, FALSE otherwise.
#' @export
gen_stats_rm <- function(date = NULL) {
  dir <- tools::R_user_dir("genflow", which = "data")
  if (!dir.exists(dir)) return(invisible(FALSE))
  if (is.null(date)) {
    files <- list.files(dir, pattern = "^[0-9]{8}\\.rds$", full.names = TRUE)
    if (length(files) == 0) return(invisible(FALSE))
    ok <- all(file.remove(files))
    if (ok) message("Removed ", length(files), " log file(s).")
    return(invisible(ok))
  }
  # Parse single date
  if (inherits(date, "Date")) {
    d <- date
  } else if (is.character(date)) {
    if (grepl("^\\d{8}$", date)) {
      d <- as.Date(date, format = "%Y%m%d")
    } else {
      d <- as.Date(date)
    }
  } else {
    stop("date must be NULL, Date, or character")
  }
  if (is.na(d)) stop("Could not parse 'date'. Use YYYYMMDD or YYYY-MM-DD or Date object.")
  fp <- file.path(dir, paste0(format(d, "%Y%m%d"), ".rds"))
  if (!file.exists(fp)) return(invisible(FALSE))
  ok <- file.remove(fp)
  if (ok) message("Removed ", basename(fp), ".")
  invisible(ok)
}

#' Print timing metrics and (optionally) detailed logs (internal)
#'
#' @keywords internal
#' @noRd
.print_metric_logs <- function(logs_completos, inicio_geral, final_geral, single_durations, agent_types, qty, log = TRUE) { # Add log = TRUE here

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

  # --- Print Metrics (code remains the same) ---
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
      for(type_name in names(task_summary)) {
        cat("tasks '", type_name, "': ", task_summary[[type_name]], "\n", sep="")
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
    for(idx in indices_erro) {
      # Rebuild the expected label for the index with error
      label_esperado <- if (suffix_type == "alphabetic") {
        if (idx <= length(letters)) paste0(agent_prefix, letters[idx]) else paste0(agent_prefix, "extra", idx)
      } else {
        paste0(agent_prefix, idx)
      }
      # Attempt to get the agent type (may be 'worker_failed_or_unknown') - requires processed types
      # For simplicity, report the error and expected label
      cat("Index ", idx, " (Label esperado '", label_esperado, "'): ", erros[[idx]], "\n", sep="")
    }
    cat("--- End of Errors ---\n\n")
  } else {
    cat("--- Status: Nenhuma tarefa resultou em Error direto ou failure de worker detectada. ---\n\n")
  }
}
#' Post-process results for quick visualization (internal)
#'
#' @keywords internal
#' @noRd
.pos_process_results <- function(results) {
  cat("--- Post-Processing and results ---\n")
  #cat("WARNING: results may be text/images. 'mostrar_text'/'gen_stats' may need adaptation.\n\n")
  if(exists("gen_view")) {
    #cat("Tentando 'mostrar_text'...\n")
    try(gen_view(results), silent = TRUE)
  } else {
    warning("'mostrar_text' not found.")
  }
  #Sys.sleep(0.5)
  if (exists(".summarize_results")) {
    # Print local summary without touching persisted logs/global env
    try(.summarize_results(results), silent = TRUE)
  } else {
    warning("'.summarize_results' not found.")
  }
  # Persist stats for all results in this batch (single-writer in main process)
  try(.persist_many_stats(results), silent = TRUE)
  cat("--- End of Post-Processing ---\n\n")
}
#' Run multiple generation tasks in parallel
#'
#' Orchestrates parallel generation tasks using `parallel` (parLapply on Windows,
#' mclapply on Unix-likes), collects results, prints timing metrics and errors,
#' and returns a list that optionally includes an `combined_stats` block.
#'
#' @param qty Integer number of workers/tasks to launch.
#' @param instructions Character base prompt/context text.
#' @param add Optional additional context mixed into the prompt per worker.
#' @param one_item_each Optional list of per-worker items to include in prompts.
#' @param add_img Optional image input for vision-capable providers.
#' @param agent_prefix Character prefix used to locate agent configs in `.GlobalEnv`.
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
gen_batch <- function(qty = 8, instructions, add = NULL, one_item_each = NULL, add_img = NULL, agent_prefix, directory = "content", directory_img = "content", log = FALSE, always_fix_errors = TRUE) { # Add log = TRUE here

  inicio_geral <- Sys.time()
  if (!requireNamespace("parallel", quietly = TRUE)) stop("Package 'parallel' needed.")
  if (missing(agent_prefix)) stop("'agent_prefix' is required.")
  if (!is.numeric(qty) || qty < 1) stop("'qty' must be a positive integer.")
  qty <- as.integer(qty)

  n_cores <- min(qty, max(1, parallel::detectCores() - 1))
  cat("Preparing to execute", qty, "tasks with prefix '", agent_prefix, "' using", n_cores, "cores.\n")

  # Input validation for one_item_each
  if (!is.null(one_item_each)) {
    if (!is.list(one_item_each)) stop("'one_item_each' must be a list.")
    if (length(one_item_each) < qty) {
      stop(paste0("'one_item_each' (", length(one_item_each), ") has fewer elements than 'qty' (", qty, ")."))
    } else if (length(one_item_each) > qty) {
      message(paste0("WARNING: 'one_item_each' (", length(one_item_each), ") has more elements than 'qty' (", qty, "). Using only the first ", qty, "."))
      # Loop 1:qty will handle using only the first qty
    }
    message("Using 'one_item_each' to provide individual data to each worker.")
    one_item_each <- one_item_each[seq_len(qty)]
  }

  # Detect suffix, sanitize instructions, create directories
  suffix_type <- tryCatch(.detect_suffix_type(agent_prefix, qty), error = function(e) {
    stop("Error detecting agent suffix (check if agents like '", agent_prefix, "1' or '", agent_prefix, "a' exist in .GlobalEnv): ", conditionMessage(e))
  })
  instructions <- .sanitize_instructions(instructions)
  .create_directories(directory, directory_img)

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

  use_cache <- isTRUE(always_fix_errors)
  cache_key <- NULL
  cache_entry <- NULL
  indices_to_run <- seq_len(qty)
  reused_indices <- integer(0)
  if (use_cache) {
    cache_key <- .batch_cache_make_key(
      agent_prefix = agent_prefix,
      qty = qty,
      instructions = instructions,
      add = add,
      one_item_each = if (!is.null(one_item_each)) one_item_each else NULL,
      add_img = add_img,
      directory = directory,
      directory_img = directory_img
    )
    if (is.null(cache_key)) {
      message("always_fix_errors enabled but cache key could not be generated; running full batch.")
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
            if (length(reused_indices) > 0) {
              message("always_fix_errors: reusing ", length(reused_indices), " successful result(s): ",
                      paste(reused_indices, collapse = ", "))
            }
          } else {
            .batch_cache_clear(cache_key)
            cache_entry <- NULL
          }
        }
      }
    }
  }

  # Parallel execution setup
  is_windows <- .Platform$OS.type == "windows"
  use_parlapply <- is_windows && n_cores > 1
  raw_results <- list()
  cl <- NULL

  # Execute tasks in parallel (Removed on.exit, using try/finally for parLapply)
  if (length(indices_to_run) == 0) {
    cat("No pending indices detected; skipping execution and reusing cached results.\n")
  } else if (use_parlapply) {
    cat("Using parLapply with", n_cores, "cores (Windows)...\n")
    cl <- parallel::makeCluster(n_cores)
    tryCatch({
      .export_cluster_vars(cl, qty, agent_prefix, suffix_type, instructions, add, add_img, directory, directory_img)
      raw_results <- parallel::parLapply(cl, indices_to_run, function(i) {
        tryCatch(
          .execute_agent_task(i, one_item_each, instructions, add, add_img, directory, directory_img, agent_prefix, suffix_type),
          error = function(e) {
            structure(paste("Error in worker", i, ":", conditionMessage(e)), class = "try-error")
          }
        )
      })
    }, finally = {
      if (!is.null(cl)) parallel::stopCluster(cl)
      cl <- NULL
    })
  } else {
    cat("Using mclapply with", n_cores, "cores (Non-Windows)...\n")
    raw_results <- parallel::mclapply(indices_to_run, function(i) {
      tryCatch(
        .execute_agent_task(i, one_item_each, instructions, add, add_img, directory, directory_img, agent_prefix, suffix_type),
        error = function(e) {
          structure(paste("Error in worker", i, ":", conditionMessage(e)), class = "try-error")
        }
      )
    }, mc.cores = n_cores, mc.silent = FALSE, mc.preschedule = FALSE)
  }
  final_geral <- Sys.time()
  cat("Parallel processing completed.\n")

  cat("Processing worker results...\n")
  results_processed <- .process_parallel_results(raw_results, qty, agent_prefix, suffix_type)
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
  final_errors                   <- results_processed$erros
  logs_list                      <- results_processed$logs_list
  single_durations             <- results_processed$single_durations
  agent_types                   <- results_processed$agent_types

  # ----- Imprimir Logs e Metrics -----
  logs_for_print <- paste(unlist(logs_list), collapse="")
  # Pass the 'log' argument here
  .print_metric_logs(logs_for_print, inicio_geral, final_geral, single_durations, agent_types, qty, log = log)
  .report_errors(erros = final_errors, qty = qty, agent_prefix = agent_prefix, suffix_type = suffix_type)

  # ----- Build Final Return Object -----
  cat("--- Building Final Return Object (", qty + 1, " elements) ---\n")
  combined_stats <- list(
    duration_real_secs = as.numeric(difftime(final_geral, inicio_geral, units = "secs")),
    duration_sum_secs = sum(single_durations, na.rm = TRUE),
    cores_number = n_cores,
    parallel_mode = if(use_parlapply) "parLapply" else "mclapply",
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
  for(i in 1:qty) {
    results_final_obj[[i]] <- results_individual_lists[[i]]
  }
  results_final_obj[[qty + 1]] <- combined_stats
  names(results_final_obj) <- c(rep("", qty), "combined_stats")

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
  .pos_process_results(results_final_obj)

  cat("--- End of content generation ---\n")
  return(results_final_obj)
}
