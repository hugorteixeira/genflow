# The installed package is the sole owner of block contracts. Loading a catalog
# never probes a provider, evaluates user code, or writes runtime configuration.
.gen_contract_cache <- new.env(parent = emptyenv())

.gen_issue <- function(code, path, message = NULL, node_id = NULL, params = NULL, schema_path = NULL) {
  value <- gen_diagnostic(code, message = message, path = path, params = params)
  if (!is.null(node_id)) value$node_id <- node_id
  if (!is.null(schema_path)) value$schema_path <- schema_path
  value
}

.gen_pointer <- function(path, key) paste0(path, "/", gsub("/", "~1", gsub("~", "~0", key, fixed = TRUE), fixed = TRUE))
.gen_object <- function(x) is.list(x) && !isTRUE(attr(x,"genflow.json_array")) && (!length(x) || (!is.null(names(x)) && !anyNA(names(x)) && !anyDuplicated(names(x)) && all(nzchar(names(x)))))
.gen_scalar <- function(x, type = "character") typeof(x) %in% type && length(x) == 1L && !is.na(x)

# JSON object order is irrelevant; property names, array order and exact values
# are not. Integer/double storage is an R detail, not a different JSON number.
.gen_json_canonical <- function(x) {
  if (is.list(x)) {
    if (!is.null(names(x))) x <- x[order(names(x))]
    return(lapply(x, .gen_json_canonical))
  }
  if (is.numeric(x) && !is.complex(x)) return(as.numeric(x))
  x
}
.gen_json_equal <- function(a, b) identical(.gen_json_canonical(a), .gen_json_canonical(b))

# NULL means "whole catalog" only in gen_block_contracts(), never in a call.
.gen_call_contract <- function(block, version) {
  if (!.gen_scalar(block) || !nzchar(block)) .gen_validation_stop(list(.gen_issue("unknown_contract","/block")))
  gen_block_contracts(block, version)
}

#' Read versioned contracts for workflow blocks
#'
#' Contracts include argument schemas and defaults, typed ports, and optional
#' presentation annotations. Reading them is offline and has no side effects.
#' @param block Optional exact block identifier, for example `"gen_img"`.
#' @param version Positive contract version. Defaults to version 1.
#' @return A contract, or a named list of all installed contracts when `block`
#'   is NULL. Names in the catalog have the form `block@version`.
#' @export
gen_block_contracts <- function(block = NULL, version = 1L) {
  if (!exists("catalog", .gen_contract_cache, inherits = FALSE)) {
    directory <- system.file("contracts", package = "genflow")
    if (!nzchar(directory)) .gen_validation_stop(list(.gen_issue("contract_catalog_unavailable","")))
    files <- list.files(directory, "[.]json$", recursive = TRUE, full.names = TRUE)
    catalog <- lapply(files, function(path) tryCatch(jsonlite::parse_json(paste(readLines(path, warn = FALSE), collapse = "\n"), simplifyVector = FALSE),
      error=function(e) .gen_validation_stop(list(.gen_issue("invalid_contract_catalog","")),basename(dirname(path)))))
    keys <- vapply(catalog, function(x) paste0(x$id, "@", x$version), character(1))
    if (!length(keys) || anyDuplicated(keys)) .gen_validation_stop(list(.gen_issue("invalid_contract_catalog","")))
    for (entry in catalog) {
      checked <- gen_check_schema(entry$arguments)
      if (!checked$valid) .gen_validation_stop(checked$errors,entry$id)
    }
    .gen_contract_cache$catalog <- stats::setNames(catalog, keys)
  }
  if (is.null(block)) return(.gen_contract_cache$catalog)
  if (!.gen_scalar(block) || !is.numeric(version) || length(version) != 1L ||
      !is.finite(version) || version < 1 || version != floor(version))
    .gen_validation_stop(list(.gen_issue("invalid_contract_reference","/contract_version")))
  result <- .gen_contract_cache$catalog[[paste0(block, "@", version)]]
  if (is.null(result)) .gen_validation_stop(list(.gen_issue("unknown_contract","/contract_version")),block)
  result
}

.gen_default <- function(block, argument, version = 1L) {
  property <- gen_block_contracts(block, version)$arguments$properties[[argument]]
  if (!"default" %in% names(property)) .gen_validation_stop(list(.gen_issue("missing_default",.gen_pointer("/args",argument))),block)
  value <- property$default
  # R vector arguments retain their R representation; document arrays stay lists.
  if (identical(property$type, "array") && !is.null(property$items$type) &&
      property$items$type %in% c("number", "integer", "string", "boolean")) {
    if (!length(value)) return(switch(property$items$type, number = numeric(), integer = integer(), string = character(), boolean = logical()))
    return(unlist(value, use.names = FALSE))
  }
  value
}

.gen_schema_keywords <- c("type", "properties", "required", "additionalProperties", "items", "enum",
  "minimum", "maximum", "exclusiveMinimum", "exclusiveMaximum", "minLength", "maxLength", "pattern",
  "minItems", "maxItems", "uniqueItems", "default", "title", "description", "$comment",
  "x-ui", "x-owner", "x-content-kind", "x-r-types", "x-default-by", "x-runtime-only")

#' Validate the supported JSON Schema vocabulary
#' @param schema A schema expressed as an R list. The supported vocabulary is
#'   documented in `inst/doc/block-contracts.md`; unsupported keywords are errors.
#' @return A list with `valid`, all independently detected `errors`, and the
#'   supported `keywords` (including Genflow annotations).
#' @export
gen_check_schema <- function(schema) {
  errors <- list()
  add <- function(path, message) errors[[length(errors) + 1L]] <<- .gen_issue("invalid_schema", path, message)
  visit <- function(s, path = "", depth = 0L) {
    if (!.gen_object(s) || depth > 32L) { add(path, gen_error_message("schema_object")); return() }
    for (key in setdiff(names(s), .gen_schema_keywords)) add(.gen_pointer(path, key), gen_error_message("schema_keyword"))
    types <- unlist(s$type, use.names = FALSE)
    if ("type" %in% names(s) && (!length(types) || !is.character(types) || anyDuplicated(types) || !all(types %in% c("object","array","string","number","integer","boolean","null")))) add(path, gen_error_message("schema_type"))
    if (!is.null(s$properties)) {
      if (!.gen_object(s$properties)) add(path, gen_error_message("schema_properties")) else
        for (key in names(s$properties)) visit(s$properties[[key]], .gen_pointer(paste0(path,"/properties"),key), depth+1L)
    }
    if (!is.null(s$required) && (!is.list(s$required) && !is.character(s$required) ||
        !all(vapply(as.list(s$required), function(x) .gen_scalar(x), logical(1))) ||
        anyDuplicated(unlist(s$required)) || !all(unlist(s$required) %in% names(s$properties)))) add(path, gen_error_message("schema_required"))
    if (!is.null(s$additionalProperties) && !identical(s$additionalProperties,FALSE) && !identical(s$additionalProperties,TRUE)) add(path,gen_error_message("schema_additional_properties"))
    if (!is.null(s$items)) visit(s$items,paste0(path,"/items"),depth+1L)
    if (!is.null(s$pattern) && (!.gen_scalar(s$pattern) || inherits(try(suppressWarnings(grepl(s$pattern,"",perl=TRUE)),silent=TRUE),"try-error"))) add(path,gen_error_message("schema_pattern"))
    for (key in intersect(names(s),c("minimum","maximum","exclusiveMinimum","exclusiveMaximum","minLength","maxLength","minItems","maxItems"))) {
      value <- s[[key]]
      if (!is.numeric(value) || is.complex(value) || length(value)!=1L || !is.finite(value) ||
          (key %in% c("minLength","maxLength","minItems","maxItems") && (value<0 || value!=floor(value)))) add(.gen_pointer(path,key),gen_error_message("schema_bound"))
    }
    for (pair in list(c("minimum","maximum"),c("minLength","maxLength"),c("minItems","maxItems")))
      if (is.numeric(s[[pair[1]]]) && length(s[[pair[1]]])==1L && is.numeric(s[[pair[2]]]) && length(s[[pair[2]]])==1L && isTRUE(s[[pair[1]]] > s[[pair[2]]])) add(path,gen_error_message("schema_range"))
    if ("enum" %in% names(s) && (!length(s$enum) || !is.null(names(s$enum)) || anyDuplicated(lapply(s$enum, .gen_json_canonical)))) add(path,gen_error_message("schema_enum"))
    if ("x-owner" %in% names(s) && (!.gen_scalar(s[["x-owner"]]) || !s[["x-owner"]] %in% c("runtime","caller"))) add(path,gen_error_message("schema_owner"))
    if ("uniqueItems" %in% names(s) && !.gen_scalar(s$uniqueItems,"logical")) add(path,gen_error_message("schema_unique"))
  }
  visit(schema)
  list(valid = !length(errors), errors = errors, keywords = as.list(.gen_schema_keywords))
}

.gen_schema_errors <- function(value, schema, path = "", portable = TRUE, depth = 0L, schema_path = "") {
  issue <- function(code, message = NULL, params = NULL, keyword = NULL) {
    e <- .gen_issue(code, path, message, params = params,
      schema_path = if (is.null(keyword)) schema_path else .gen_pointer(schema_path, keyword))
    if (.gen_scalar(schema$title)) e$field <- schema$title
    e
  }
  error <- function(code, message = NULL, params = NULL, keyword = NULL) list(issue(code, message, params, keyword))
  if (depth>64L) return(error("depth_limit",gen_error_message("depth_limit")))
  if (!portable && length(schema[["x-r-types"]])) {
    accepted <- unlist(schema[["x-r-types"]])
    if (is.function(value) && "function" %in% accepted || any(vapply(accepted,function(type) inherits(value,type),logical(1)))) return(list())
  }
  types <- unlist(schema$type, use.names=FALSE)
  matches <- function(type) switch(type,
    null=is.null(value), object=.gen_object(value) && (!portable || !is.null(names(value))),
    array=(is.list(value) && is.null(names(value))) || (!portable && is.atomic(value) && !is.null(value) && is.null(names(value)) && !is.object(value)),
    string=.gen_scalar(value), boolean=.gen_scalar(value,"logical"),
    number=is.numeric(value) && !is.complex(value) && length(value)==1L && is.finite(value),
    integer=is.numeric(value) && !is.complex(value) && length(value)==1L && is.finite(value) && value==floor(value), FALSE)
  if (length(types) && !any(vapply(types,matches,logical(1)))) return(error("invalid_type", params=list(expected_types=as.list(types), actual_type=.gen_value_type(value)), keyword="type"))
  errors <- list()
  add <- function(code, message = NULL, params = NULL, keyword = NULL) errors[[length(errors)+1L]] <<- issue(code, message, params, keyword)
  if (!is.null(schema$enum) && !any(vapply(schema$enum,function(x) .gen_json_equal(value,x),logical(1)))) {
    simple <- length(schema$enum) <= 20L && all(vapply(schema$enum,function(x)
      is.null(x) || (is.atomic(x) && length(x)==1L && nchar(as.character(x)) <= 80L),logical(1)))
    params <- list(allowed_count=length(schema$enum))
    if (simple) params$allowed_values <- schema$enum
    add("invalid_enum", if (simple) gen_error_message("invalid_enum_choices",params) else NULL, params=params, keyword="enum")
  }
  if (is.null(value)) return(errors)
  if (.gen_scalar(value)) {
    if (!is.null(schema$pattern) && !grepl(schema$pattern,value,perl=TRUE)) add("invalid_pattern", if (identical(schema$pattern,"\\S")) gen_error_message("blank_text") else NULL, params=list(pattern=schema$pattern), keyword="pattern")
    size <- nchar(value,type="chars")
    if (!is.null(schema$minLength) && size<schema$minLength) add("too_short", params=list(limit=schema$minLength, unit="characters"), keyword="minLength")
    if (!is.null(schema$maxLength) && size>schema$maxLength) add("too_long", params=list(limit=schema$maxLength, unit="characters"), keyword="maxLength")
  }
  if (is.numeric(value) && !is.complex(value) && length(value)==1L && is.finite(value)) {
    for (key in intersect(names(schema),c("minimum","maximum","exclusiveMinimum","exclusiveMaximum"))) {
      bound <- schema[[key]]
      ok <- switch(key,minimum=value>=bound,maximum=value<=bound,exclusiveMinimum=value>bound,exclusiveMaximum=value<bound)
      if (!ok) add("out_of_range", params=list(limit=bound, bound_rule=key), keyword=key)
    }
  }
  if (.gen_object(value) && ("object" %in% types || length(schema$properties))) {
    for (key in setdiff(unlist(schema$required),names(value))) errors <- c(errors,list(.gen_issue("required",.gen_pointer(path,key),schema_path=.gen_pointer(schema_path,"required"))))
    if (identical(schema$additionalProperties,FALSE)) for (key in setdiff(names(value),names(schema$properties))) errors <- c(errors,list(.gen_issue("unknown_argument",.gen_pointer(path,key),schema_path=.gen_pointer(schema_path,"additionalProperties"))))
    for (key in intersect(names(value),names(schema$properties))) errors <- c(errors,.gen_schema_errors(value[[key]],schema$properties[[key]],.gen_pointer(path,key),portable,depth+1L,.gen_pointer(paste0(schema_path,"/properties"),key)))
  } else if ("array" %in% types && matches("array")) {
    if (!is.null(schema$minItems) && length(value)<schema$minItems) add("too_few_items", params=list(limit=schema$minItems), keyword="minItems")
    if (!is.null(schema$maxItems) && length(value)>schema$maxItems) add("too_many_items", params=list(limit=schema$maxItems), keyword="maxItems")
    if (isTRUE(schema$uniqueItems) && anyDuplicated(lapply(value, .gen_json_canonical))) add("duplicate_item", keyword="uniqueItems")
    if (length(schema$items)) for (i in seq_along(value)) errors <- c(errors,.gen_schema_errors(value[[i]],schema$items,.gen_pointer(path,as.character(i-1L)),portable,depth+1L,paste0(schema_path,"/items")))
  }
  errors
}

#' Validate data against a supported JSON schema
#' @param value Data to check. No values are coerced or defaulted.
#' @param schema A supported JSON schema.
#' @return A list with `valid` and all independently detected `errors`.
#' @export
gen_validate_schema <- function(value, schema) {
  checked <- gen_check_schema(schema)
  if (!checked$valid) return(checked)
  errors <- .gen_schema_errors(value,schema)
  list(valid=!length(errors),errors=errors)
}

.gen_apply_defaults <- function(args, schema, bound = character()) {
  if (!.gen_object(args)) return(args)
  for (key in setdiff(names(schema$properties),bound)) {
    property <- schema$properties[[key]]
    if (!key %in% names(args) && "default" %in% names(property)) args[key] <- list(property$default)
    if (key %in% names(args) && .gen_object(args[[key]]) && length(property$properties)) args[key] <- list(.gen_apply_defaults(args[[key]],property))
  }
  args
}

#' Prepare and validate arguments without executing a block
#' @param block Exact block identifier.
#' @param args Named list of explicit arguments. Omitted fields receive defaults;
#'   explicit NULL values are retained and validated.
#' @param version Contract version.
#' @param bound Names of arguments supplied by upstream connections. They receive
#'   no defaults and their concrete values must be validated at execution.
#' @param check_capabilities Check known provider modality restrictions offline.
#' @return `valid`, `errors`, prepared `args`, `defaulted`, `pending`, and the
#'   selected `contract_version`. Invalid values are never silently repaired.
#' @export
gen_validate_call <- function(block, args = list(), version = 1L, bound = character(), check_capabilities = TRUE) {
  .gen_prepare_call(block,args,version,bound,check_capabilities,portable=TRUE)
}

.gen_prepare_call <- function(block, args, version = 1L, bound = character(), check_capabilities = TRUE, portable = TRUE, bindings_known = TRUE) {
  contract <- tryCatch(.gen_call_contract(block,version),error=function(e)e)
  if (inherits(contract,"error")) return(list(valid=FALSE,errors=list(.gen_issue("unknown_contract","",conditionMessage(contract))),args=args,defaulted=list(),pending=list()))
  schema <- contract$arguments
  if (!.gen_object(args)) return(list(valid=FALSE,errors=list(.gen_issue("invalid_arguments","/args",gen_error_message("invalid_arguments"))),args=args,defaulted=list(),pending=list()))
  if (!length(args)) args <- structure(list(),names=character())
  errors <- list()
  if (is.null(bound)) bound <- character()
  if (!is.character(bound) || anyNA(bound) || anyDuplicated(bound)) {
    errors <- list(.gen_issue("invalid_bindings","/inputs",gen_error_message("invalid_bindings"))); bound <- character()
  }
  for (key in setdiff(bound,names(schema$properties))) errors <- c(errors,list(.gen_issue("unknown_argument",.gen_pointer("/inputs",key),gen_error_message("unknown_argument",list(argument=key)))))
  for (key in intersect(bound,names(args))) errors <- c(errors,list(.gen_issue("duplicate_binding",.gen_pointer("/args",key),gen_error_message("duplicate_binding"))))
  if (portable) {
    failure <- tryCatch({ .genflow_validate_portable(args); NULL },error=function(e)e)
    if (!is.null(failure)) errors <- c(errors,list(.gen_issue("nonportable_arguments","/args",conditionMessage(failure))))
  }
  supplied <- names(args)
  runtime_only <- names(Filter(function(p) isTRUE(p[["x-runtime-only"]]),schema$properties))
  args <- .gen_apply_defaults(args,schema,c(bound,if (portable) runtime_only else character()))
  if (.gen_scalar(args$service)) {
    if (!identical(block,"gen_stt")) args$service <- .genflow_normalize_service_alias(args$service)
  }
  for (key in setdiff(names(schema$properties),bound)) {
    mapping <- schema$properties[[key]][["x-default-by"]]
    replace <- is.null(args[[key]]) || .gen_scalar(args[[key]]) && args[[key]] %in% unlist(mapping$replace)
    if (!is.null(mapping) && replace && .gen_scalar(args[[mapping$field]])) {
      value <- mapping$values[[args[[mapping$field]]]]
      if (!is.null(value)) args[key] <- list(value)
    }
  }
  parameters <- .gen_prepare_parameter_args(args, contract, supplied, bound, check_capabilities)
  args <- parameters$args
  errors <- c(errors, parameters$errors)
  concrete_schema <- schema
  concrete_schema$required <- as.list(setdiff(unlist(schema$required),bound))
  errors <- c(errors,.gen_schema_errors(args,concrete_schema,"/args",portable,schema_path="/arguments"))
  bad <- vapply(errors,function(e)e$path,character(1))
  usable <- function(key) key %in% names(args) && !key %in% bound && !any(startsWith(bad,paste0("/args/",key)))
  add <- function(key,message,code="invalid_combination") errors[[length(errors)+1L]] <<- .gen_issue(code,.gen_pointer("/args",key),message)
  if (isTRUE(check_capabilities) && !is.null(contract$modality) && usable("service") && .gen_scalar(args$service)) {
    cap <- tryCatch(gen_capabilities(if (block=="gen_stt") .stt_normalize_service(args$service) else args$service,
      if (usable("model")) args$model else NULL),error=function(e)e)
    if (inherits(cap,"error")) add("service",conditionMessage(cap),"unsupported_provider") else
      if (!isTRUE(cap$modalities[[contract$modality]])) add("service",gen_error_message("unsupported_provider"),"unsupported_provider")
  }
  if (identical(block,"gen_img") && version >= 2L) {
    if (isTRUE(check_capabilities) && usable("add_img") && length(args$add_img) && usable("service") && usable("model")) {
      spec <- .gen_image_reference_spec(args$service,args$model)
      if (!isTRUE(spec$supported)) add("add_img",gen_error_message("image_references_unsupported"),"image_references_unsupported") else
        if (length(args$add_img) > spec$max_images) add("add_img",gen_error_message("image_reference_limit",list(limit=spec$max_images)),"image_reference_limit")
    }
    if (usable("replicate_input") && length(args$replicate_input)) {
      protected <- c("prompt","num_outputs","num_images","n","image","images","image_urls","input_image","input_images")
      if (length(intersect(names(args$replicate_input),protected))) add("replicate_input",gen_error_message("image_override_conflict"),"image_override_conflict")
    }
  }
  if (identical(block,"gen_tts")) .gen_tts_rules(args,usable,add,version,check_capabilities,if (bindings_known) bound else character())
  if (block %in% c("gen_sfx","gen_music")) .gen_sound_rules(sub("^gen_","",block),args,usable,add,check_capabilities,if (bindings_known) bound else character())
  if (identical(block,"gen_txt")) {
    if (usable("max_output_tokens") && !is.null(args$max_output_tokens) && usable("service") && args$service!="openai") add("max_output_tokens",gen_error_message("only_openai_tokens"))
    if (usable("base_url") && !is.null(args$base_url) && usable("service") && !args$service %in% c("ollama","llamacpp")) add("base_url",gen_error_message("only_local_base_url"))
    if (isTRUE(args$tools) && usable("my_tools") && is.null(args$my_tools)) add("my_tools",gen_error_message("required_tool_definitions"))
  }
  if (identical(block,"gen_vote") && identical(args$return_winner,"content") && usable("underlying_list") && is.null(args$underlying_list)) add("underlying_list",gen_error_message("required_underlying_list"))
  if (block %in% c("json.get","list.split") && usable("pointer") && .gen_scalar(args$pointer)) {
    if (nzchar(args$pointer) && (!startsWith(args$pointer,"/") || grepl("~([^01]|$)",args$pointer,perl=TRUE))) add("pointer",gen_error_message("invalid_pointer"))
  }
  errors <- .gen_diagnostic_context(errors, block=block)
  list(valid=!length(errors), errors=errors, args=args, defaulted=as.list(setdiff(names(args),supplied)),
       pending=as.list(bound), block=block, contract_version=contract$version)
}

.gen_validation_stop <- function(errors, block = NULL) {
  message <- paste(vapply(errors,gen_error_summary,character(1)),collapse="\n")
  diagnostic <- gen_diagnostic(list(code="validation_failed", errors=errors))
  if (.gen_scalar(block)) diagnostic$block <- block
  stop(structure(list(message=message,call=NULL,block=block,errors=errors,diagnostic=diagnostic),class=c("genflow_validation_error","error","condition")))
}

# Called at the start of each public execution boundary. Default expressions
# read the same catalog as offline validation; missing values remain errors.
.gen_internal_validate <- function(block, frame = parent.frame(), version = 1L) {
  contract <- gen_block_contracts(block,version)
  args <- list()
  for (key in names(contract$arguments$properties)) {
    # Preserve omission until model/profile defaults have been resolved.
    omitted <- tryCatch(eval(call("missing", as.name(key)), envir = frame), error = function(e) FALSE)
    if (omitted && "default" %in% names(contract$arguments$properties[[key]])) next
    value <- tryCatch(list(get(key,envir=frame,inherits=FALSE)),error=function(e) {
      if (!"default" %in% names(contract$arguments$properties[[key]]) &&
          grepl("is missing",conditionMessage(e),fixed=TRUE)) return(NULL)
      .gen_validation_stop(list(.gen_issue("argument_evaluation",.gen_pointer("/args",key),conditionMessage(e))),block)
    })
    if (!is.null(value)) args[key] <- value
  }
  if (exists("...",envir=frame,inherits=FALSE)) {
    dots <- eval(quote(list(...)),envir=frame)
    if (length(dots)) args <- c(args,dots)
  }
  prepared <- .gen_prepare_call(block,args,version=version,portable=FALSE)
  if (!prepared$valid) .gen_validation_stop(prepared$errors,block)
  list2env(prepared$args,envir=frame)
  invisible(prepared)
}
