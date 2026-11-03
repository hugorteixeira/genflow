# Parameter profiles extend the existing catalogs; they never replace providers,
# credential storage or block contracts. Imported snapshots are data, not code.
.gen_parameter_object <- function() structure(list(), names = character())
.gen_parameter_blocks <- c("gen_txt", "gen_img", "gen_stt", "gen_tts", "gen_sfx", "gen_music")

.gen_parameter_path <- function(directory = NULL) {
  directory <- .gen_management_path(directory, "directory")
  file.path(.genflow_resolve_models_dir(directory), "parameter-schemas.json")
}

.gen_parameter_identity <- function(service, model = NULL, block = NULL) {
  service <- .gen_management_string(service, "service")
  service <- if (identical(block, "gen_stt")) .stt_normalize_service(service) else .genflow_normalize_service_alias(service)
  .gen_management_assert(grepl("^[a-z][a-z0-9_-]*$", service) && !.genflow_is_retired_service(service), "service")
  if (!is.null(model)) model <- .gen_management_string(model, "model")
  if (!is.null(block)) .gen_management_assert(.gen_scalar(block) && block %in% .gen_parameter_blocks, "block")
  list(service = service, model = model, block = block)
}

.gen_parameter_key <- function(x) as.character(jsonlite::toJSON(x[c("service", "model", "block")], auto_unbox = TRUE, null = "null"))
.gen_parameter_hash <- function(x) unclass(as.character(openssl::sha256(charToRaw(as.character(
  jsonlite::toJSON(.gen_json_canonical(x), auto_unbox = TRUE, null = "null", digits = NA))))))

.gen_parameter_portable <- function(x) {
  tryCatch(.genflow_validate_portable(x), error = function(e) .gen_stop("parameter_schema_invalid"))
  visit <- function(value) {
    if (is.atomic(value) && (anyNA(value) || is.numeric(value) && any(!is.finite(value))))
      .gen_stop("parameter_schema_invalid")
    if (is.list(value)) {
      if (!is.null(names(value)) && (anyNA(names(value)) || any(!nzchar(names(value))) || anyDuplicated(names(value))))
        .gen_stop("parameter_schema_invalid")
      invisible(lapply(value, visit))
    }
  }
  visit(x)
  invisible(TRUE)
}

# Profiles are independent of block versions. Keep their historical fields
# readable when a later block changes; the selected block validates each call.
.gen_parameter_contract_properties <- function(block) {
  entries <- Filter(function(x) identical(x$id, block), gen_block_contracts())
  lapply(entries, function(x) x$arguments$properties)
}

# Configurations may describe endpoint/executable fields. Portable generation
# profiles cannot change connection, identity, input ownership or consent.
.gen_parameter_protected <- c("parameter_schema", "base_url", "executable", "native_engine", "native_backend",
  "plugins", "my_tools", "tools", "reference_consent", "reference_identity", "spoken_disclaimer", "marking_responsibility")
.gen_option_protected <- function(block) unique(c(
  unlist(lapply(.gen_parameter_contract_properties(block), names), use.names = FALSE), .gen_parameter_protected,
  "api_key", "api_token", "authorization", "headers", "url", "endpoint", "command", "env",
  "messages", "input", "text", "prompt", "model", "stream", "temperature", "reasoning_effort",
  "num_outputs", "num_images", "n", "image", "images", "image_urls", "input_image", "input_images",
  "max_tokens", "max_completion_tokens", "tool_choice"))

.gen_parameter_schema_errors <- function(schema, block = NULL) {
  checked <- gen_check_schema(schema)
  if (!checked$valid) return(checked$errors)
  errors <- list()
  add <- function(path, code = "parameter_schema_invalid") errors[[length(errors) + 1L]] <<- .gen_issue(code, path)
  if (!identical(schema$type, "object") || !identical(schema$additionalProperties, FALSE)) add("")
  for (key in setdiff(names(schema), c("type", "properties", "required", "additionalProperties", "title", "description", "$comment")))
    add(.gen_pointer("", key))
  visit <- function(s, path = "") {
    if (is.null(s$type)) add(path)
    if (!is.null(names(s$required))) add(paste0(path, "/required"))
    for (key in intersect(names(s), c("x-owner", "x-r-types", "x-runtime-only", "x-default-by"))) add(.gen_pointer(path, key))
    if ("object" %in% unlist(s$type) && !identical(s$additionalProperties, FALSE)) add(path)
    if ("default" %in% names(s)) {
      for (error in .gen_schema_errors(.gen_apply_defaults(s$default, s), s)) {
        error$path <- paste0(path, "/default", error$path); errors[[length(errors) + 1L]] <<- error
      }
    }
    for (key in names(s$properties)) visit(s$properties[[key]], .gen_pointer(paste0(path, "/properties"), key))
    if (!is.null(s$items)) visit(s$items, paste0(path, "/items"))
  }
  visit(schema)
  if (!is.null(block)) {
    versions <- .gen_parameter_contract_properties(block)
    known <- unique(unlist(lapply(versions, names), use.names = FALSE))
    owned <- unique(unlist(lapply(versions, function(p) names(Filter(function(x) identical(x[["x-owner"]], "runtime"), p)))))
    allowed <- setdiff(known, c(owned, .gen_parameter_protected))
    for (key in setdiff(names(schema$properties), allowed)) add(.gen_pointer("/properties", key), "parameter_schema_reserved")
    # A profile only adds constraints to canonical arguments. Model options are
    # the dedicated extensible object, validated separately against its schema.
    for (key in intersect(names(schema$properties), known)) {
      prop <- schema$properties[[key]]
      if ("default" %in% names(prop) && key != "model_options") {
        definitions <- Filter(Negate(is.null), lapply(versions, `[[`, key))
        checks <- lapply(definitions, function(p) .gen_schema_errors(prop$default, p))
        if (all(lengths(checks) > 0L)) for (e in checks[[length(checks)]]) {
          e$path <- paste0("/properties/", key, "/default", e$path); errors[[length(errors) + 1L]] <- e
        }
      }
    }
    options <- schema$properties$model_options
    if (!is.null(options)) {
      if (!identical(options$type, "object") || !identical(options$additionalProperties, FALSE)) add("/properties/model_options")
      for (key in intersect(names(options$properties), .gen_option_protected(block)))
        add(.gen_pointer("/properties/model_options/properties", key), "parameter_schema_reserved")
    }
  }
  errors
}

# R users can supply character vectors for required/enum. Always persist actual
# JSON arrays and objects, including singleton arrays and empty properties.
.gen_parameter_schema_json <- function(schema) {
  for (key in intersect(names(schema), c("required", "enum"))) schema[[key]] <- unname(as.list(schema[[key]]))
  if (!is.null(schema$properties)) {
    schema$properties <- lapply(schema$properties, .gen_parameter_schema_json)
    if (!length(schema$properties)) schema$properties <- .gen_parameter_object()
  }
  if (!is.null(schema$items)) schema$items <- .gen_parameter_schema_json(schema$items)
  schema
}

.gen_parameter_records <- function(directory = NULL, snapshot = TRUE) {
  frozen <- getOption("genflow.runtime_snapshot")$parameter_schemas
  if (snapshot && is.null(directory) && !is.null(frozen)) return(frozen)
  path <- .gen_parameter_path(directory)
  if (!file.exists(path)) return(list())
  fail <- function() .gen_stop("parameter_registry_corrupt", stage = "management")
  if (dir.exists(path) || file.info(path)$size > 16 * 1024^2) fail()
  data <- tryCatch(jsonlite::read_json(path, simplifyVector = FALSE), error = function(e) fail())
  if (!.gen_object(data) || !setequal(names(data), c("schema_version", "records")) ||
    !.gen_scalar(data$schema_version, c("integer", "double")) || data$schema_version != 1L ||
    !is.list(data$records) || !is.null(names(data$records))) fail()
  seen <- list()
  for (entry in data$records) {
    valid <- tryCatch({
      identity <- .gen_parameter_identity(entry$service, entry$model, entry$block)
      .gen_parameter_portable(entry)
      .gen_json_equal(entry[names(identity)], identity) &&
        isTRUE(entry$revision >= 1) && .gen_scalar(entry$revision, c("integer", "double")) && entry$revision == floor(entry$revision) &&
        .gen_scalar(entry$created_at) && setequal(names(entry), c(names(identity), "revision", "schema", "created_at")) &&
        !length(.gen_parameter_schema_errors(entry$schema, entry$block))
    }, error = function(e) FALSE)
    if (!isTRUE(valid)) fail()
    key <- .gen_parameter_key(entry)
    if (entry$revision != (seen[[key]] %||% 0) + 1) fail()
    seen[[key]] <- entry$revision
  }
  data$records
}

#' List versioned service and model parameter schemas
#' @param service Optional provider ID. Aliases are normalized.
#' @param model Optional exact model ID. NULL includes all models when listing;
#'   when saving/resolving it identifies the service-wide layer.
#' @param block Generator ID (for example `gen_tts`). NULL identifies service
#'   configuration when saving/resolving and includes all scopes when listing.
#' @param history Include historical revisions, rather than only the latest.
#' @param directory Existing model catalog directory, as in [gen_models()].
#' @return A list of immutable records. Reading is offline and never writes.
#' @export
gen_parameter_schemas <- function(service = NULL, model = NULL, block = NULL, history = FALSE, directory = NULL) .gen_management({
  .gen_management_flag(history, "history")
  if (!is.null(service)) service <- .gen_parameter_identity(service, model, block)$service
  if (!is.null(model)) .gen_management_string(model, "model")
  if (!is.null(block)) .gen_management_assert(.gen_scalar(block) && block %in% .gen_parameter_blocks, "block")
  records <- .gen_parameter_records(directory)
  records <- Filter(function(r) (is.null(service) || r$service == service) &&
    (is.null(model) || identical(r$model, model)) && (is.null(block) || identical(r$block, block)), records)
  if (!history && length(records)) records <- records[!duplicated(vapply(records, .gen_parameter_key, character(1)), fromLast = TRUE)]
  unname(records)
})

#' Save a new immutable parameter schema revision
#' @inheritParams gen_parameter_schemas
#' @param schema Closed JSON object schema for service configuration (block=NULL)
#'   or generator parameters. Specific options belong under `model_options`.
#' @param expected_revision Last revision read; 0 creates a new registration.
#'   Stale edits are rejected. Retrying the same successful write is idempotent.
#' @return The saved record. Existing provider/catalog files are not modified.
#' @export
gen_save_parameter_schema <- function(service, schema, model = NULL, block = NULL, expected_revision = 0L, directory = NULL) .gen_management({
  identity <- .gen_parameter_identity(service, model, block)
  .gen_management_assert(.gen_scalar(expected_revision, c("integer", "double")) && is.finite(expected_revision) &&
    expected_revision >= 0 && expected_revision == floor(expected_revision), "expected_revision")
  .gen_parameter_portable(schema)
  errors <- .gen_parameter_schema_errors(schema, block)
  if (length(errors)) .gen_validation_stop(errors, block)
  # Canonical JSON representation removes incidental R numeric/vector types.
  schema <- jsonlite::parse_json(jsonlite::toJSON(.gen_parameter_schema_json(schema), auto_unbox = TRUE, null = "null", digits = NA), simplifyVector = FALSE)
  path <- .gen_parameter_path(directory)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  lock <- .genflow_acquire_file_lock(path)
  on.exit(.genflow_release_file_lock(lock), add = TRUE)
  records <- .gen_parameter_records(directory, snapshot = FALSE)
  matches <- Filter(function(r) identical(.gen_parameter_key(r), .gen_parameter_key(identity)), records)
  latest <- if (length(matches)) matches[[length(matches)]] else NULL
  current <- latest$revision %||% 0L
  if (!is.null(latest) && expected_revision %in% c(current, current - 1L) && .gen_json_equal(schema, latest$schema)) return(latest)
  if (expected_revision != current) .gen_stop("parameter_schema_conflict", stage = "management")
  entry <- c(identity, list(revision = current + 1L, schema = schema, created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")))
  records[[length(records) + 1L]] <- entry
  serialized <- as.character(jsonlite::toJSON(list(schema_version = 1L, records = records),
    auto_unbox = TRUE, null = "null", pretty = TRUE, digits = NA))
  .gen_management_assert(nchar(serialized, type = "bytes") <= 16 * 1024^2, "schema")
  .genflow_atomic_write_lines(serialized, path)
  entry
})

#' Resolve service and model schemas into a portable immutable snapshot
#' @inheritParams gen_parameter_schemas
#' @param revisions Optional named list with service/model revision numbers.
#'   An omitted layer selects its latest revision; 0 excludes that layer.
#' @return A snapshot containing identity, selected revisions, effective schema
#'   and content hash. Model properties replace complete service properties;
#'   `model_options` composes named option definitions with whole-option
#'   replacement. Required names accumulate. NULL means neither layer is registered.
#' @export
gen_resolve_parameter_schema <- function(service, model = NULL, block = NULL, revisions = list(), directory = NULL) .gen_management({
  identity <- .gen_parameter_identity(service, model, block)
  .gen_management_assert(.gen_object(revisions) && all(names(revisions) %in% c("service", "model")) &&
    all(vapply(revisions, function(n) .gen_scalar(n, c("integer", "double")) && is.finite(n) && n >= 0 && n == floor(n), logical(1))), "revisions")
  records <- .gen_parameter_records(directory)
  properties <- .gen_parameter_object(); required <- character(); selected <- list(service = 0L, model = 0L)
  for (layer in c("service", "model")) {
    if (layer == "model" && is.null(model)) {
      if (!is.null(revisions$model) && revisions$model != 0) .gen_stop("parameter_schema_revision")
      next
    }
    target <- identity; target["model"] <- list(if (layer == "service") NULL else model)
    rows <- Filter(function(r) identical(.gen_parameter_key(r), .gen_parameter_key(target)), records)
    revision <- revisions[[layer]]
    if (identical(as.numeric(revision), 0)) next
    if (!is.null(revision)) rows <- Filter(function(r) r$revision == revision, rows)
    if (!length(rows)) {
      if (!is.null(revision)) .gen_stop("parameter_schema_revision")
      next
    }
    entry <- rows[[length(rows)]]; selected[[layer]] <- entry$revision
    for (key in names(entry$schema$properties)) {
      property <- entry$schema$properties[[key]]
      if (key == "model_options" && !is.null(properties[[key]])) {
        inherited <- properties[[key]]
        property$properties <- .genflow_merge_fields(inherited$properties, property$properties)
        property$required <- as.list(union(unlist(inherited$required), unlist(property$required)))
      }
      properties[key] <- list(property)
    }
    required <- union(required, unlist(entry$schema$required, use.names = FALSE))
  }
  if (!any(unlist(selected) > 0)) return(NULL)
  schema <- list(type = "object", properties = properties, required = as.list(required), additionalProperties = FALSE)
  errors <- .gen_parameter_schema_errors(schema, block)
  if (length(errors)) .gen_validation_stop(errors, block)
  out <- c(list(schema_version = 1L), identity, list(revisions = selected, schema = schema))
  out$sha256 <- .gen_parameter_hash(out)
  out
})

.gen_parameter_snapshot_errors <- function(profile, service = NULL, model = NULL, block = NULL, match_identity = FALSE) {
  issue <- function(code) list(.gen_issue(code, ""))
  if (!.gen_object(profile)) return(issue("parameter_schema_invalid"))
  shape <- list(schema_version = list(type = "integer", enum = list(1)), service = list(type = "string", pattern = "^[a-z][a-z0-9_-]*$"),
    model = list(type = c("string", "null"), pattern = "\\S"), block = list(type = c("string", "null"), enum = c(as.list(.gen_parameter_blocks), list(NULL))),
    revisions = list(type = "object", properties = list(service = list(type = "integer", minimum = 0), model = list(type = "integer", minimum = 0)),
      required = list("service", "model"), additionalProperties = FALSE), schema = list(type = "object"), sha256 = list(type = "string", pattern = "^[a-f0-9]{64}$"))
  errors <- .gen_schema_errors(profile, list(type = "object", properties = shape, required = as.list(names(shape)), additionalProperties = FALSE))
  if (length(errors)) return(errors)
  if (!any(unlist(profile$revisions) > 0) || is.null(profile$model) && profile$revisions$model > 0) return(issue("parameter_schema_invalid"))
  if (!identical(profile$sha256, .gen_parameter_hash(profile[setdiff(names(profile), "sha256")]))) return(issue("parameter_schema_integrity"))
  errors <- .gen_parameter_schema_errors(profile$schema, profile$block)
  if (match_identity && !.gen_json_equal(profile[c("service", "model", "block")], .gen_parameter_identity(service, model, block)))
    errors <- c(errors, issue("parameter_schema_mismatch"))
  errors
}

#' Validate values against a portable parameter schema snapshot
#' @param values Named list of configuration or generation parameters.
#' @param profile Snapshot returned by [gen_resolve_parameter_schema()].
#' @return `valid`, all structured `errors`, and `values` with omitted defaults
#'   filled. Explicit values, including NULL, are never overwritten. This checks
#'   declared rules, not whether a remote model honors an option.
#' @export
gen_validate_parameters <- function(values, profile) {
  errors <- tryCatch({ .gen_parameter_portable(profile); .gen_parameter_portable(values); .gen_parameter_snapshot_errors(profile) },
    error = function(e) list(.gen_issue("parameter_schema_invalid", "")))
  if (!length(errors)) {
    if (is.list(values) && !length(values)) values <- .gen_parameter_object()
    values <- .gen_apply_defaults(values, profile$schema)
    errors <- .gen_schema_errors(values, profile$schema)
  }
  list(valid = !length(errors), errors = errors, values = values)
}
