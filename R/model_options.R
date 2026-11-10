# Profiles govern data. Transport support is owned by executable library code;
# an imported schema cannot introduce shell flags, routes, headers or R code.
.gen_model_options_transport <- function(block, service) {
  if (block == "gen_tts" && service == "local-native") return("voxcpm2")
  if (block == "gen_img" && service == "replicate") return("replicate")
  if (block == "gen_txt" && (service %in% c("openai", "openrouter", "llamacpp", names(.GENFLOW_COMPAT_SPECS)) ||
      !is.null(.genflow_get_custom_provider(service)))) return("json")
  "none"
}

.gen_prepare_parameter_args <- function(args, contract, supplied, bound, check_capabilities) {
  errors <- list()
  finish <- function() list(args = args, errors = errors)
  if (!"parameter_schema" %in% names(contract$arguments$properties)) return(finish())
  add <- function(code, path) errors[[length(errors) + 1L]] <<- .gen_issue(code, path)
  profile <- args$parameter_schema
  native_options <- contract$id == "gen_tts" && identical(args$service,"local-native")
  # An absent profile and absent options preserve historical behavior. Explicit
  # nonempty options opt into lookup; the returned snapshot pins that lookup.
  if (is.null(profile) && .gen_object(args$model_options) && length(args$model_options) && .gen_scalar(args$service)) {
    profile <- tryCatch(gen_resolve_parameter_schema(args$service, args$model, contract$id), error = function(e) {
      error <- gen_diagnostic(e); error$path <- "/args/parameter_schema"
      errors[[length(errors) + 1L]] <<- error; NULL
    })
    if (is.null(profile) && !length(errors) && !native_options) add("parameter_schema_required", "/args/parameter_schema")
  }
  if (!is.null(profile)) {
    checked <- tryCatch({ .gen_parameter_portable(profile)
      .gen_parameter_snapshot_errors(profile, args$service, args$model, contract$id, TRUE)
    }, error = function(e) list(.gen_issue("parameter_schema_invalid", "")))
    for (e in checked) { e$path <- paste0("/args/parameter_schema", e$path); errors[[length(errors) + 1L]] <- e }
    if (!length(checked)) {
      schema <- profile$schema
      # The immutable block still owns canonical types and hard runtime rules.
      # A profile can supply omitted defaults and add model-specific constraints.
      defaults <- setdiff(names(schema$properties), c(supplied, bound))
      args[defaults] <- NULL
      args <- .gen_apply_defaults(args, schema, bound)
      args <- .gen_apply_defaults(args, contract$arguments, bound)
      args <- .gen_apply_defaults(args, schema, bound)
      schema$required <- as.list(setdiff(unlist(schema$required), bound))
      selected <- args[intersect(names(args), names(schema$properties))]
      errors <- c(errors, .gen_schema_errors(selected, schema, "/args", schema_path = "/parameter_schema/schema"))
      if (length(args$model_options) && is.null(schema$properties$model_options) && !native_options) add("parameter_schema_required", "/args/model_options")
      args$parameter_schema <- profile
    }
  }
  options <- args$model_options
  if (.gen_object(options) && length(options)) {
    for (key in intersect(names(options), .gen_option_protected(contract$id)))
      add("parameter_schema_reserved", .gen_pointer("/args/model_options", key))
    if (contract$id == "gen_img" && length(args$replicate_input))
      add("model_options_conflict", "/args/replicate_input")
    if (isTRUE(check_capabilities) && .gen_scalar(args$service)) {
      transport <- .gen_model_options_transport(contract$id, args$service)
      unsupported <- if (transport == "none") names(options) else if (transport == "voxcpm2") setdiff(names(options), "cfg") else character()
      for (key in unsupported) add("model_options_unsupported", .gen_pointer("/args/model_options", key))
      if (transport == "voxcpm2" && "cfg" %in% names(options)) errors <- c(errors,
        .gen_schema_errors(options$cfg, list(type = "number", minimum = 0), "/args/model_options/cfg"))
    }
  }
  finish()
}

# An option must never override fields the adapter has already constructed.
.gen_model_payload <- function(body, options) {
  if (!length(options)) return(body)
  if (length(intersect(names(body), names(options)))) .gen_stop("model_options_conflict", stage = "preflight")
  c(body, options)
}
