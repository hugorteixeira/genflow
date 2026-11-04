# Diagnostics are data. Translations never decide retries or execution state.
.gen_diagnostics_cache <- new.env(parent = emptyenv())

#' Read the versioned diagnostic catalog
#' @return The diagnostic schema and pt-BR message catalog, without I/O to providers.
#' @export
gen_error_catalog <- function() {
  if (!exists("catalog", .gen_diagnostics_cache, inherits = FALSE)) {
    path <- system.file("diagnostics", "1.json", package = "genflow")
    .gen_diagnostics_cache$catalog <- jsonlite::read_json(path, simplifyVector = FALSE)
  }
  .gen_diagnostics_cache$catalog
}

.gen_diagnostic_text <- function(x, fallback = "") {
  if (is.character(x) && length(x) == 1L && !is.na(x)) x else fallback
}

.gen_message_entry <- function(key, catalog = NULL) {
  seen <- character()
  repeat {
    if (!.gen_scalar(key) || key %in% seen) stop("Refer\u00eancia inv\u00e1lida no cat\u00e1logo de diagn\u00f3sticos.",call.=FALSE)
    seen <- c(seen,key)
    entry <- catalog[[key]] %||% gen_error_catalog()$messages[[key]]
    if (is.null(entry$alias)) return(entry)
    key <- entry$alias
  }
}

#' Render a catalog message with named parameters
#' @param key Stable message key.
#' @param params Named scalar parameters; never include credentials or full content.
#' @param catalog Optional application catalog of message entries.
#' @return A pt-BR string carrying its key and parameters until diagnostic creation.
#' @export
gen_error_message <- function(key, params = list(), catalog = NULL) {
  entry <- .gen_message_entry(key,catalog)
  if (is.null(entry)) stop("Chave de diagn\u00f3stico n\u00e3o cadastrada: ", key, call. = FALSE)
  display <- params
  if (!is.null(params$expected_types)) display$expected <- .gen_type_label(unlist(params$expected_types))
  if (!is.null(params$actual_type)) display$actual <- .gen_type_label(params$actual_type)
  if (is.list(params$allowed_values)) display$allowed <- paste(vapply(params$allowed_values,
    function(x) as.character(jsonlite::toJSON(x,auto_unbox=TRUE,null="null")),character(1)),collapse=", ")
  if (!is.null(params$bound_rule)) display$comparison <- switch(params$bound_rule,
    minimum="maior ou igual a", maximum="menor ou igual a", exclusiveMinimum="maior que", exclusiveMaximum="menor que")
  if (identical(params$unit,"characters")) display$unit <- "caracteres"
  render <- function(text) {
    for (name in names(display)) {
      value <- display[[name]]
      if (is.atomic(value) && length(value) == 1L && !is.na(value))
        text <- gsub(paste0("{", name, "}"), as.character(value), text, fixed = TRUE)
    }
    text
  }
  placeholders <- regmatches(entry$message, gregexpr("[{][a-zA-Z_]+[}]", entry$message))[[1L]]
  required <- gsub("[{}]", "", placeholders)
  text <- if (all(required %in% names(display))) entry$message else entry$fallback %||% "Confira os valores exigidos por este campo."
  structure(render(text), message_key = key, params = params,
    hint = render(entry$hint %||% ""))
}

#' Normalize an error without changing execution policy
#'
#' Known diagnostics preserve their causes and context. Unclassified exceptions
#' receive a safe message; arbitrary exception text is not returned to clients.
#' @param error A code, condition, or existing diagnostic list.
#' @param message Optional catalog message made by [gen_error_message()].
#' @param source Component that produced the error.
#' @param stage Phase in which it occurred.
#' @param category Optional category override.
#' @param path JSON pointer into the author's document.
#' @param params Safe named parameters for a catalog message.
#' @param retryable Whether the caller's policy permits considering a retry.
#' @param outcome Execution outcome, independent of the translated message.
#' @param catalog Optional application message catalog.
#' @param log Record the original failure in a bounded, redacted server log once.
#' @return A JSON-serializable version 1 diagnostic in Brazilian Portuguese.
#' @export
gen_diagnostic <- function(error, message = NULL, source = "genflow", stage = NULL,
                           category = NULL, path = NULL, params = NULL,
                           retryable = NULL, outcome = NULL, catalog = NULL, log = FALSE) {
  scalar <- .gen_diagnostic_text
  condition <- inherits(error, "condition")
  old <- if (is.list(error)) error else list()
  if (condition && is.list(old$diagnostic)) old <- old$diagnostic
  application_catalog <- catalog
  # An application catalog must never replace a library-owned explanation with
  # an unrelated message that happens to use the same key.
  if (nzchar(scalar(old$source)) && !identical(old$source,source)) catalog <- NULL
  code <- scalar(if (is.character(error)) error else old$code,
    if (inherits(error, "genflow_validation_error")) "validation_failed" else "internal_error")
  if (!grepl("^[A-Za-z][A-Za-z0-9_.-]{0,127}$", code)) code <- "internal_error"
  key <- scalar(attr(message, "message_key") %||% old$message_key, code)
  entries <- gen_error_catalog()$messages
  entry <- .gen_message_entry(key,catalog)
  if (is.null(entry)) { key <- "unclassified_error"; entry <- entries[[key]] }
  params <- params %||% attr(message, "params") %||% old$params %||% list()
  if (!is.list(params) || (length(params) && is.null(names(params)))) params <- list()
  params <- params[!grepl("(?i)password|secret|token|api.?key|authorization",names(params),perl=TRUE)]
  # All parameters come from rules, not from the original exception text.
  rendered <- gen_error_message(key, params, catalog)
  category <- scalar(category %||% old$category, entry$category %||% "internal")
  if (!category %in% unlist(gen_error_catalog()$schema$properties$category$enum)) category <- "internal"
  outcome <- scalar(outcome %||% old$outcome, "not_started")
  if (!outcome %in% c("not_started", "failed", "unknown", "succeeded")) outcome <- "unknown"
  out <- list(diagnostic_version = 1L, code = code,
    source = scalar(old$source, source), category = category,
    stage = scalar(stage %||% old$stage, entry$stage %||% "validation"),
    message_key = key, message = as.character(rendered), hint = attr(rendered, "hint"),
    params = if (length(params)) params else structure(list(), names = character()),
    retryable = isTRUE(retryable %||% old$retryable),
    outcome = outcome)
  for (name in c("path", "node_id", "node_label", "field", "schema_path", "block", "reference", "condition_class", "condition_call"))
    if (is.character(old[[name]]) && length(old[[name]]) == 1L && !is.na(old[[name]])) out[[name]] <- scalar(old[[name]])
  version <- old$contract_version
  if (is.numeric(version) && length(version) == 1L && is.finite(version) && version >= 1 && version == floor(version)) out$contract_version <- version
  if (!is.null(path)) out$path <- scalar(path)
  if (condition) {
    if (is.null(out$condition_class)) out$condition_class <- class(error)[[1L]]
    call <- conditionCall(error)
    if (is.call(call)) out$condition_call <- paste(deparse(call[[1L]]),collapse="")
  }
  causes <- old$causes %||% old$errors %||% if (is.list(old$details)) old$details$errors else NULL
  if (is.list(causes) && length(causes)) out$causes <- lapply(causes, gen_diagnostic, source = source, catalog = application_catalog)
  if (isTRUE(log) && is.null(out$reference)) out <- .gen_log_diagnostic(out, error)
  out
}

.gen_log_diagnostic <- function(value, error) {
  id <- paste(format(openssl::rand_bytes(16)), collapse = "")
  value$reference <- paste(substr(id,1,8),substr(id,9,12),substr(id,13,16),substr(id,17,20),substr(id,21,32),sep="-")
  raw <- if (inherits(error, "condition")) {
    conditionMessage(if (inherits(error$parent, "condition")) error$parent else error)
  } else if (is.list(error)) error$message else error
  raw <- paste(as.character(raw), collapse = " ")
  raw <- gsub("(?i)(bearer\\s+)[^\\s,;]+", "\\1[oculto]", raw, perl=TRUE)
  raw <- gsub("(?i)([\"']?(?:api[_-]?key|password|passwd|secret|(?:access[_-]?)?token|authorization)[\"']?\\s*[:=]\\s*)(\"[^\"]*\"|'[^']*'|[^,;\\s&]+)", "\\1[oculto]", raw, perl=TRUE)
  raw <- gsub("(?i)(https?://)[^/@\\s]+@", "\\1[oculto]@", raw, perl=TRUE)
  message("Diagnostic ", value$reference, " [", value$code, "]: ", substr(gsub("[\r\n]", " ", raw),1L,2000L))
  value
}

#' Format a diagnostic for console output
#' @param error A diagnostic or condition accepted by [gen_diagnostic()].
#' @return A readable summary including the context of each cause.
#' @export
gen_error_summary <- function(error) {
  item <- if (is.list(error) && isTRUE(error$diagnostic_version == 1L)) error else gen_diagnostic(error)
  context <- c(if (!is.null(item$node_label %||% item$node_id)) paste0("Pe\u00e7a \u201c", item$node_label %||% item$node_id, "\u201d"),
    if (!is.null(item$field)) paste0("campo \u201c", item$field, "\u201d") else item$path)
  paste(c(paste(c(context, item$message), collapse = ": "),
    vapply(item$causes %||% list(), gen_error_summary, character(1))), collapse = "\n")
}

# Preserve ordinary R unwinding and generator frames. In particular, argument
# validation still inspects the method's own formals, not a wrapper closure.
.gen_boundary_error <- function(error) {
  if (inherits(error, c("genflow_error", "genflow_validation_error"))) return(invisible(NULL))
  .gen_stop(error, stage="execution", outcome="unknown", log=TRUE)
}

.gen_stop <- function(error, ...) {
  diagnostic <- gen_diagnostic(error, ...)
  stop(structure(list(message=gen_error_summary(diagnostic),call=NULL,
    diagnostic=diagnostic,parent=if (inherits(error,"condition")) error else NULL),class=c("genflow_error","error","condition")))
}

.gen_value_type <- function(value) {
  if (is.null(value)) return("null")
  if (is.list(value)) return(if (is.null(names(value))) "array" else "object")
  if (length(value) != 1L) return("array")
  if (is.character(value)) return("string")
  if (is.logical(value) && !is.na(value)) return("boolean")
  if (is.numeric(value) && !is.complex(value) && is.finite(value)) return("number")
  "invalid"
}
.gen_type_label <- function(type) {
  labels <- c(string = "texto", number = "n\u00famero", integer = "n\u00famero inteiro", boolean = "verdadeiro ou falso",
    object = "objeto", array = "lista", null = "nulo", invalid = "valor inv\u00e1lido")
  paste(unname(labels[type]), collapse = " ou ")
}

# Enrich only from the original document. Never resolve an ID across unrelated
# subworkflows: the pointer identifies its scope even when IDs are reused.
.gen_diagnostic_context <- function(errors, document = NULL, block = NULL, version = NULL) {
  if (is.character(document) && length(document) == 1L)
    document <- tryCatch(.gen_parse_document(document), error = function(e) NULL)
  lapply(errors, function(e) {
    parts <- strsplit(sub("^/", "", e$path %||% ""), "/", fixed = TRUE)[[1L]]
    parts <- gsub("~0", "~", gsub("~1", "/", parts, fixed = TRUE), fixed = TRUE)
    current <- document; node <- NULL
    for (i in seq_along(parts)) {
      if (!is.list(current)) break
      part <- parts[[i]]
      current <- if (is.null(names(current))) {
        index <- suppressWarnings(as.integer(part))
        if (length(index) && !is.na(index) && index >= 0L && index < length(current)) current[[index + 1L]] else NULL
      } else current[[part]]
      if (i > 1L && identical(parts[[i - 1L]], "nodes") && .gen_object(current)) node <- current
    }
    id <- if (.gen_object(node)) node$id else NULL
    if (.gen_scalar(id)) e$node_id <- id
    if (.gen_object(node) && .gen_scalar(node$label) && nzchar(node$label)) e$node_label <- node$label
    type <- if (.gen_object(node)) node$block %||% node$type else block
    if (.gen_scalar(type)) {
      type <- switch(type, validate = "gen_validate_structure", materialize = "gen_materialize_structure", type)
      selected_version <- if (.gen_object(node)) node$contract_version %||% 1L else version %||% e$contract_version %||% 1L
      contract <- tryCatch(gen_block_contracts(type, selected_version), error = function(e) NULL)
      if (!is.null(contract)) {
        e$block <- type; e$contract_version <- selected_version
        at <- which(parts %in% c("args", "config", "parameters"))
        if (length(at) && utils::tail(at, 1L) < length(parts)) {
          property <- contract$arguments$properties[[parts[[utils::tail(at, 1L) + 1L]]]]
          if (.gen_scalar(property$title)) e$field <- property$title
        }
      }
    }
    if (is.null(e$field) && length(parts)) e$field <- utils::tail(parts, 1L)
    e
  })
}
