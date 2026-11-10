# Installed transport capabilities complement common and registered schemas.
.gen_adapter_schema <- function(id) {
  key <- paste0("adapter:", id)
  if (!exists(key, .gen_contract_cache, inherits = FALSE)) {
    path <- system.file("adapter-schemas", paste0(id, ".json"), package = "genflow")
    schema <- tryCatch(jsonlite::read_json(path, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(schema) || !gen_check_schema(schema)$valid)
      .gen_validation_stop(list(.gen_issue("invalid_contract_catalog", "")))
    .gen_contract_cache[[key]] <- schema
  }
  .gen_contract_cache[[key]]
}

# Adapter restrictions are shared by validation and form projection. Schemas
# supplied by users can narrow these limits, never enable an unsupported route.
.gen_argument_rules <- function(contract, args, bound = character()) {
  rules <- list(); props <- contract$arguments$properties
  put <- function(key, schema, code, disabled = FALSE, message = code, unsupported = FALSE) {
    if (key %in% names(props)) rules[[key]] <<- list(schema = schema, code = code, message = message, disabled = disabled, unsupported = unsupported)
  }
  fixed <- function(key, values, code, message = code, unsupported = TRUE) put(key, list(enum = values), code, TRUE, message, unsupported = unsupported)
  service <- args$service; block <- contract$id
  if (!.gen_scalar(service)) return(rules)
  if (block == "gen_tts") {
    if (service == "local-native") {
      native <- .gen_adapter_schema("voxcpm2-tts")$properties
      for (key in names(native))
        put(key, native[[key]], "tts_native_parameter", disabled = isTRUE(native[[key]][["x-ui"]]$disabled),
          unsupported = isTRUE(native[[key]][["x-ui"]]$unsupported))
    } else {
      native <- c("reference_audio", names(Filter(function(p) "local-native" %in% unlist(p[["x-ui"]]$service), props)))
      for (key in native) {
        # These policy flags do not affect remote transports.
        policy <- key %in% c("reference_consent", "spoken_disclaimer", "marking_responsibility")
        fixed(key, if (policy) list(TRUE, FALSE) else list(props[[key]]$default), "tts_native_parameter")
      }
    }
  }
  if (block %in% c("gen_txt","gen_img")) {
    cap <- tryCatch(gen_capabilities(service,args$model),error=function(e) NULL)
    supported <- if(block=="gen_txt") isTRUE(unname(cap$adapter_features["vision"])) else isTRUE(cap$image_references$supported)
    if (!is.null(cap) && !supported) fixed("add_img",list(NULL),"image_not_supported")
  }
  if (block == "gen_txt") {
    if (service != "openai") fixed("max_output_tokens", list(NULL), "invalid_combination", "only_openai_tokens")
    if (!service %in% c("ollama", "llamacpp")) fixed("base_url", list(NULL), "invalid_combination", "only_local_base_url")
  }
  if (block %in% c("gen_music", "gen_sfx")) {
    if (service == "elevenlabs") {
      put("format", list(enum = as.list(if (block == "gen_sfx") c("mp3", "wav", "opus") else c("mp3", "opus"))), "sound_format_unsupported")
      if (block == "gen_music") {
        if (!is.null(args$lyrics)) put("duration_seconds", list(maximum = 120), "sound_duration_unsupported")
        else if (!"lyrics" %in% bound || isTRUE(args$instrumental)) fixed("seed", list(NULL), "sound_seed_unsupported", unsupported = FALSE)
      }
    }
    if (service == "acestep") put("duration_seconds", list(minimum = 10), "sound_duration_unsupported")
  }
  rules
}

.gen_validate_argument_rules <- function(contract, args, usable, add, bound, bindings_known = TRUE) {
  rules <- .gen_argument_rules(contract, args, bound)
  for (key in names(rules)) {
    rule <- rules[[key]]
    if (bindings_known && key %in% bound && isTRUE(rule$disabled) || usable(key) && length(.gen_schema_errors(args[[key]], rule$schema)))
      add(key, gen_error_message(rule$message), rule$code)
  }
}

# This is a control projection, not a second validator. Preserve common labels
# and ownership; intersect choices/ranges instead of broadening the base schema.
.gen_form_property <- function(base, extra) {
  if (is.null(extra)) return(base)
  out <- .genflow_merge_fields(base, extra)
  for (key in c("x-owner", "x-runtime-only", "x-ui")) if (key %in% names(base)) out[key] <- base[key]
  # Presentation metadata may hide an unsupported control, but a profile
  # cannot turn an adapter restriction back into an available feature.
  if (!is.null(base[["x-ui"]]$unsupported) || !is.null(extra[["x-ui"]]$unsupported))
    out[["x-ui"]]$unsupported <- isTRUE(base[["x-ui"]]$unsupported) || isTRUE(extra[["x-ui"]]$unsupported)
  if (!is.null(base$type) && !is.null(extra$type)) {
    types <- intersect(unlist(base$type), unlist(extra$type))
    if ("integer" %in% unlist(base$type) && "number" %in% unlist(extra$type) ||
        "number" %in% unlist(base$type) && "integer" %in% unlist(extra$type)) types <- union(types, "integer")
    out$type <- as.list(types)
  }
  for (key in c("minimum", "exclusiveMinimum", "minLength", "minItems"))
    if (!is.null(base[[key]]) && !is.null(extra[[key]])) out[[key]] <- max(base[[key]], extra[[key]])
  for (key in c("maximum", "exclusiveMaximum", "maxLength", "maxItems"))
    if (!is.null(base[[key]]) && !is.null(extra[[key]])) out[[key]] <- min(base[[key]], extra[[key]])
  choices <- extra$enum %||% base$enum
  if (!is.null(choices)) out$enum <- Filter(function(x) !length(.gen_schema_errors(x, base)) && !length(.gen_schema_errors(x, extra)), choices)
  if (length(base$properties) || length(extra$properties)) {
    keys <- union(names(base$properties), names(extra$properties))
    out$properties <- stats::setNames(lapply(keys, function(key) .gen_form_property(base$properties[[key]] %||% list(), extra$properties[[key]])), keys)
    out$required <- as.list(union(unlist(base$required), unlist(extra$required)))
  }
  out
}

#' Resolve the editable configuration of a block
#'
#' Offline projection of the immutable block, optional service/model snapshot,
#' and installed adapter restrictions. This never generates content or writes
#' configuration. Execution and imports still use [gen_validate_call()].
#' @param block Exact generator/block identifier.
#' @param service Provider ID.
#' @param model Exact model ID or NULL to use the block default.
#' @param version Pinned block contract version.
#' @param parameters Explicit editable arguments, including an optional frozen
#'   `parameter_schema`. Values are never discarded or repaired.
#' @param resolve Look up the current service/model profile only when no frozen
#'   profile is supplied. FALSE uses only the supplied parameter configuration.
#' @param directory Optional parameter registry directory, as in [gen_models()].
#' @return `arguments` for controls (including `x-ui.unsupported`, `disabled` and `reason`), the
#'   selected `parameter_schema`, `valid`, and canonical `errors`. The projection
#'   is not a standalone validation schema: cross-field rules and multiple
#'   patterns remain enforced by the canonical validator. Complex values are
#'   editable as JSON. Absence of a profile property inherits the base contract.
#' @export
gen_block_configuration <- function(block, service, model = NULL, version = NULL,
                                    parameters = list(), resolve = FALSE, directory = NULL) {
  contract <- .gen_call_contract(block, version)
  .gen_management_flag(resolve, "resolve")
  .gen_management_assert(.gen_object(parameters), "parameters")
  props <- contract$arguments$properties
  args <- parameters; args$service <- service
  if (!is.null(model)) args$model <- model
  args <- .gen_argument_defaults(.gen_apply_defaults(args, contract$arguments), contract)
  if (resolve && "parameter_schema" %in% names(props) && is.null(parameters$parameter_schema)) {
    args$parameter_schema <- gen_resolve_parameter_schema(args$service, args$model, block, directory = directory)
    parameters["parameter_schema"] <- list(args$parameter_schema)
  }
  explicit <- parameters
  for (key in intersect(c("service", "model"), names(props))) explicit[key] <- args[key]
  checked <- .gen_prepare_call(block, explicit, version,
    bound = setdiff(names(contract$inputs), names(explicit)), bindings_known = FALSE)
  owned <- names(Filter(function(p) identical(p[["x-owner"]], "runtime"), props))
  for (key in intersect(names(parameters), owned)) checked$errors <- c(checked$errors,
    list(.gen_issue("backend_owned_argument", .gen_pointer("/args", key), params = list(argument = key))))
  checked$errors <- .gen_diagnostic_context(checked$errors, block = block, version = contract$version)
  checked$valid <- !length(checked$errors)
  profile <- checked$args$parameter_schema
  usable_profile <- tryCatch(!is.null(profile) && !length(.gen_parameter_snapshot_errors(profile,
    checked$args$service, checked$args$model, block, TRUE)), error = function(e) FALSE)
  schema <- contract$arguments
  if (block == "gen_tts" && identical(checked$args$service, "local-native"))
    schema[["x-ui"]]$help <- .gen_adapter_schema("voxcpm2-tts")$description
  rules <- .gen_argument_rules(contract, checked$args, unlist(checked$pending))
  for (key in names(rules)) {
    schema$properties[[key]] <- .gen_form_property(schema$properties[[key]], rules[[key]]$schema)
    if (isTRUE(rules[[key]]$unsupported)) schema$properties[[key]][["x-ui"]]$unsupported <- TRUE
    if (rules[[key]]$disabled) {
      schema$properties[[key]][["x-ui"]]$disabled <- TRUE
      schema$properties[[key]][["x-ui"]]$reason <- gen_error_message(rules[[key]]$message)
    }
  }
  if (usable_profile) {
    for (key in intersect(names(props), names(profile$schema$properties)))
      schema$properties[[key]] <- .gen_form_property(schema$properties[[key]], profile$schema$properties[[key]])
    schema$required <- as.list(union(unlist(schema$required), unlist(profile$schema$required)))
  }
  if ("model_options" %in% names(props)) {
    options <- schema$properties$model_options
    options[["x-ui"]]$hidden <- FALSE
    transport <- if (.gen_scalar(checked$args$service)) .gen_model_options_transport(block, checked$args$service) else "none"
    if (transport == "none" || !is.null(profile) && !usable_profile ||
        transport != "voxcpm2" && (!usable_profile || is.null(profile$schema$properties$model_options))) {
      if (transport == "none") options[["x-ui"]]$unsupported <- TRUE
      options[["x-ui"]]$disabled <- TRUE
      options[["x-ui"]]$reason <- gen_error_message(if (transport == "none") "model_options_unsupported" else "parameter_schema_required")
    } else if (transport == "voxcpm2") for (key in names(options$properties)) {
      if (key == "cfg") options$properties[[key]] <- .gen_form_property(options$properties[[key]], list(type = "number", minimum = 0))
      else options$properties[[key]][["x-ui"]] <- list(disabled = TRUE, unsupported = TRUE, reason = gen_error_message("model_options_unsupported"))
    }
    schema$properties$model_options <- options
  }
  list(block = block, contract_version = contract$version, arguments = schema,
    parameter_schema = profile, valid = checked$valid, errors = checked$errors)
}
