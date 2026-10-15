# Common boundaries for all generators. Public legacy fields remain intact.
.genflow_error <- function(code, message, retryable = FALSE, stage = "inference") {
  list(code = code, message = message, retryable = isTRUE(retryable), stage = stage)
}

.genflow_error_from_message <- function(message, stage = "inference") {
  message <- paste(as.character(message), collapse = "\n")
  code <- if (grepl("^[A-Z][A-Z0-9_]+:", message)) sub(":.*", "", message) else "GENERATION_ERROR"
  transient <- grepl("TIMEOUT|HF_MODEL_LOADING|EMPTY_OR_NULL_RESPONSE|\\b(429|500|502|503|504)\\b", message)
  .genflow_error(code, message, transient && stage == "inference", stage)
}

.genflow_complete_result <- function(result, inference_status = result$status_api,
                                     persistence = NULL, error = NULL) {
  result$schema_version <- 1L
  result$inference_status <- inference_status
  result$persistence <- persistence %||% list(status = "not_requested", path = NULL)
  if (identical(result$persistence$status, "failed")) {
    result$status_api <- "ERROR"
    result$status_msg <- paste0("PERSISTENCE_ERROR: ", result$persistence$message)
    error <- .genflow_error("PERSISTENCE_ERROR", result$status_msg, stage = "persistence")
  }
  result["error"] <- list(error %||% if (identical(result$status_api, "ERROR")) .genflow_error_from_message(result$status_msg) else NULL)
  if (is.null(result$usage)) {
    estimated <- !is.null(result$tokens_sent) || !is.null(result$tokens_received)
    result$usage <- list(source = if (estimated) "estimated" else "unknown",
                         input_tokens = result$tokens_sent %||% NA_real_,
                         output_tokens = result$tokens_received %||% NA_real_)
  }
  result
}

.genflow_provider_success <- function(value, metadata = list()) {
  if (is.null(value) || !length(value) ||
      (is.character(value) && !any(nzchar(trimws(value)), na.rm = TRUE))) {
    result <- .genflow_provider_failure("EMPTY_OR_NULL_RESPONSE", code = "EMPTY_OR_NULL_RESPONSE", retryable = TRUE)
    result$metadata <- metadata
    return(result)
  }
  structure(list(value = value, error = NULL, metadata = metadata), class = "genflow_provider_result")
}

.genflow_provider_failure <- function(message, code = NULL, retryable = NULL) {
  error <- .genflow_error_from_message(message)
  if (!is.null(code)) error$code <- code
  if (!is.null(retryable)) error$retryable <- isTRUE(retryable)
  structure(list(value = NULL, error = error, metadata = list()), class = "genflow_provider_result")
}

# Provider-reported usage stays separate from the legacy token estimates.
.genflow_response_metadata <- function(result) {
  first <- function(x) if (is.list(x) && length(x)) x[[1L]] else list()
  choice <- first(result$choices)
  candidate <- first(result$candidates)
  usage <- result$usage %||% list()
  input <- usage$input_tokens %||% usage$prompt_tokens %||%
    result$usageMetadata$promptTokenCount %||% result$prompt_eval_count
  output <- usage$output_tokens %||% usage$completion_tokens %||%
    result$usageMetadata$candidatesTokenCount %||% result$eval_count
  finish <- choice$finish_reason %||% result$stop_reason %||%
    candidate$finishReason %||% result$done_reason %||% result$status
  out <- list(provider_request_id = result$id %||% result$responseId,
              provider_model = result$model %||% result$modelVersion,
              finish_reason = finish,
              truncated = !is.null(finish) && finish %in% c("length", "max_tokens", "MAX_TOKENS", "incomplete"))
  if (!is.null(input) || !is.null(output)) {
    out$usage <- list(source = "provider", input_tokens = input %||% NA_real_,
                      output_tokens = output %||% NA_real_,
                      total_tokens = usage$total_tokens %||% result$usageMetadata$totalTokenCount %||% NA_real_)
  }
  out
}

.genflow_chat_result <- function(result) {
  if (!is.list(result)) return(.genflow_provider_failure("API_RESPONSE_ERROR: Invalid chat response."))
  metadata <- .genflow_response_metadata(result)
  choice <- if (is.list(result$choices) && length(result$choices)) result$choices[[1L]] else list()
  message <- choice$message %||% result$message %||% list()
  if (!is.null(message$refusal) || identical(choice$finish_reason, "content_filter")) {
    value <- .genflow_provider_failure("CONTENT_FILTERED: Response refused or blocked by provider.")
  } else if (!is.null(message$tool_calls) || !is.null(message$function_call)) {
    value <- .genflow_provider_success(result)
  } else {
    value <- .genflow_provider_success(message$content %||% choice$text %||% result$content %||% result$response)
  }
  value$metadata <- metadata
  value
}

.genflow_responses_result <- function(result) {
  if (!is.list(result)) return(.genflow_provider_failure("API_RESPONSE_ERROR: Invalid Responses payload."))
  blocks <- unlist(lapply(result[["output"]] %||% list(), function(item) item$content %||% list()), recursive = FALSE)
  refused <- any(vapply(blocks, function(item) identical(item$type, "refusal"), logical(1)))
  if (refused) {
    value <- .genflow_provider_failure("CONTENT_FILTERED: Response refused by provider.")
  } else if (!is.null(result$error) || identical(result$status, "failed")) {
    value <- .genflow_provider_failure(paste0("API_RESPONSE_ERROR: ", result$error$message %||% "Response failed."))
  } else {
    text <- result$output_text %||% result$response$output_text
    if (is.null(text)) text <- paste(vapply(blocks, function(item) item$text %||% "", character(1)), collapse = "\n")
    value <- .genflow_provider_success(text)
  }
  value$metadata <- .genflow_response_metadata(result)
  value
}

# Compatibility is confined to this boundary, including third-party mocks.
# Real adapter success paths wrap data before entering it, so error-like text
# generated by a model is never interpreted as a transport error.
.genflow_provider_result <- function(value) {
  if (inherits(value, "genflow_provider_result")) return(value)
  if (is.null(value) || !length(value) ||
      (is.character(value) && !any(nzchar(trimws(value)), na.rm = TRUE)) ||
      identical(value, "EMPTY_OR_NULL_RESPONSE")) {
    return(.genflow_provider_failure("EMPTY_OR_NULL_RESPONSE"))
  }
  if (is.character(value) && length(value) == 1L &&
      grepl("^(TIMEOUT_ERRORR?(_HTTR)?|HTTR_ERRORR?|HF_MODEL_LOADING|API_ERRORR?|CONTENT_FILTERED|PROMPT_BLOCKED|SERVICE_NOT_IMPLEMENTED|API_RESPONSE_ERRORR?|FUNCTION_CALL_ERRORR?|TOOL_CALL_ERRORR?):", value)) {
    return(.genflow_provider_failure(value))
  }
  .genflow_provider_success(value)
}

.genflow_try_next_route <- function(value) {
  if (!is.character(value) || length(value) != 1L) return(FALSE)
  grepl("HTTP (404|405)\\b|Could not resolve host|Couldn't connect|Failed to connect|Connection refused",
        value, ignore.case = TRUE)
}

.genflow_validate_dots <- function(dots, generator) {
  if (!length(dots)) return(invisible(NULL))
  fields <- names(dots) %||% rep("", length(dots))
  fields[is.na(fields) | !nzchar(fields)] <- "<unnamed>"
  stop("`...` is reserved and is not forwarded silently. Unsupported argument(s): ",
       paste(unique(fields), collapse = ", "), ". Use an explicit `", generator,
       "()` argument instead.", call. = FALSE)
}

.genflow_validate_flag <- function(value, name) {
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    stop("`", name, "` must be TRUE or FALSE.", call. = FALSE)
  }
  value
}

.genflow_input_text <- function(item) {
  if (is.null(item)) return("")
  if (is.character(item) && length(item) == 1L && !is.na(item) && file.exists(item) && !dir.exists(item)) {
    ext <- tolower(tools::file_ext(item))
    if (identical(ext, "txt")) return(paste(readLines(item, encoding = "UTF-8", warn = FALSE), collapse = "\n"))
    if (identical(ext, "csv")) return(paste(capture.output(print(utils::read.csv(item, stringsAsFactors = FALSE))), collapse = "\n"))
    stop("Unsupported file format. Use .txt or .csv.", call. = FALSE)
  }
  if (is.data.frame(item) || is.matrix(item)) return(paste(capture.output(print(item)), collapse = "\n"))
  if (is.list(item)) return(paste(vapply(item, .genflow_input_text, character(1)), collapse = "\n"))
  paste(as.character(item), collapse = "\n")
}

.genflow_prepare_prompt <- function(context, add = NULL) {
  if (is.null(add)) return(context)
  items <- if (is.data.frame(add) || is.matrix(add)) list(add) else if (is.list(add)) add else as.list(add)
  full_text <- paste0("\n", paste(vapply(items, .genflow_input_text, character(1)), collapse = "\n"))
  paste(context, "\n\nText:\n", full_text)
}

.genflow_file_fingerprint <- function(value) {
  if (is.list(value)) return(lapply(value, .genflow_file_fingerprint))
  if (!is.character(value)) return(value)
  lapply(value, function(item) {
    if (is.na(item) || !file.exists(item) || dir.exists(item)) return(item)
    input <- file(item, "rb")
    on.exit(close(input), add = TRUE)
    list(path = normalizePath(item, winslash = "/", mustWork = TRUE),
         sha256 = as.character(openssl::sha256(input)))
  })
}

.genflow_validate_portable <- function(value, path = "configuration", depth = 0L) {
  if (depth > 64L) stop("Configuration nesting is too deep.", call. = FALSE)
  if (is.environment(value) || is.function(value) || is.language(value) || typeof(value) == "externalptr") {
    stop("Non-portable value at ", path, ". Store data, not executable R objects.", call. = FALSE)
  }
  attrs <- attributes(value)
  attrs[c("names", "class", "dim", "row.names")] <- NULL
  if (length(attrs)) .genflow_validate_portable(attrs, paste0(path, "@attributes"), depth + 1L)
  if (!is.list(value)) {
    if (is.null(names(value))) return(invisible(TRUE))
    value <- as.list(value)
  }
  keys <- names(value) %||% rep("", length(value))
  for (i in seq_along(value)) {
    key <- tolower(gsub("[-.]", "_", keys[i]))
    if (!is.na(key) && key %in% c("api_key", "api_token", "access_token", "refresh_token", "password", "secret", "authorization", "x_api_key") &&
        !is.null(value[[i]]) && length(value[[i]])) {
      stop("Secret field at ", path, ".", keys[i],
           ". Use an environment variable or credential reference instead.", call. = FALSE)
    }
    .genflow_validate_portable(value[[i]], paste0(path, ".", keys[i]), depth + 1L)
  }
  invisible(TRUE)
}
