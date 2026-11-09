# Fixed-purpose pieces share one content contract. All binding consumers use
# this traversal; text, assets and graph references are never guessed from strings.
.gen_content_spec <- function(block, args) {
  catalog <- gen_block_contracts("content")
  if (block == "content") return(if(.gen_scalar(args$content_type)) catalog$pieces[[args$content_type]] else NULL)
  function_id <- if (block == "transcribe") "gen_stt" else if (block == "judge") "gen_txt" else NULL
  if (is.null(function_id)) return(NULL)
  contract <- gen_block_contracts(function_id)
  list(label=if(block=="judge") "Avaliar alternativas" else "Transcrever", output="text",
    sources=list("ai"), operation=contract$studio$id, function_version=contract$version,
    `function`=function_id, primary=if(block=="judge") "context" else "audio", roles=if(block=="judge") list("context_text","context_image") else list())
}

.gen_piece_values <- function(block, args) {
  out <- list()
  push <- function(value,path,type,argument=NULL,role=NULL) {
    out[[length(out)+1L]] <<- list(value=value,path=path,type=type,argument=argument,role=role)
  }
  textual <- "asset:text|asset:json|list:text|list:json"
  if (block == "text.join") for (i in seq_along(args$parts))
    push(args$parts[[i]],paste0("/parts/",i-1L),textual)
  spec <- .gen_content_spec(block,args)
  ai <- block %in% c("judge","transcribe") || block=="content" && identical(args$source,"ai")
  if (ai) {
    if (block=="transcribe") {
      if (!is.null(args$audio)) push(args$audio,"/audio",gen_block_contracts("gen_stt")$inputs$audio,"audio")
    } else if (.gen_object(args$ai)) for (i in seq_along(args$ai$content))
      push(args$ai$content[[i]],paste0("/ai/content/",i-1L),textual,spec$primary)
    roles <- gen_block_contracts("content")$roles
    for (i in seq_along(args$additions)) {
      entry <- args$additions[[i]]
      role <- if (.gen_object(entry) && .gen_scalar(entry$role)) roles[[entry$role]] else NULL
      if (!is.null(role)) push(entry$value,paste0("/additions/",i-1L,"/value"),role$type,role$argument,entry$role)
    }
  }
  out
}

.gen_piece_value_errors <- function(value,path) {
  fields <- if (.gen_object(value)) names(value) else character()
  if (length(fields)!=1L || !fields %in% c("text","asset","input"))
    return(list(.gen_issue("content_value",path)))
  schema <- list(type="string",maxLength=if(fields=="text") 1000000 else 100)
  if (fields!="text") schema$pattern <- "\\S"
  .gen_schema_errors(value[[fields]],schema,.gen_pointer(path,fields))
}

.gen_piece_check <- function(block,args,version,check_capabilities) {
  errors <- list(); spec <- .gen_content_spec(block,args)
  add <- function(code,path) errors[[length(errors)+1L]] <<- .gen_issue(code,paste0("/args",path))
  ai <- block %in% c("judge","transcribe") || block=="content" && identical(args$source,"ai")
  if (block=="content") {
    if (is.null(spec)) return(list(errors=errors,args=args)) # base schema reports unknown purpose
    if (!args$source %in% unlist(spec$sources)) add("content_source","/source")
    allowed <- c("content_type","source",if(args$content_type=="text") c("format","output_schema"),
      switch(args$source,inline="text",asset="asset_id",ai=c("ai","additions")))
    for (key in setdiff(names(args),allowed)) add("content_source_fields",paste0("/",key))
    if (identical(args$source,"inline")) {
      if (is.null(args$text)) add("required","/text")
      if (.gen_scalar(args$text) && identical(args$format,"json")) {
        value <- tryCatch(list(jsonlite::parse_json(args$text,simplifyVector=FALSE)),error=function(e) NULL)
        if (is.null(value)) add("literal_json","/text") else if (!is.null(args$output_schema))
          errors <- c(errors,.gen_schema_errors(value[[1L]],args$output_schema,"/args/text"))
      }
    }
    if (identical(args$source,"asset") && is.null(args$asset_id)) add("required","/asset_id")
    if (!is.null(args$output_schema)) {
      if (!identical(args$format,"json")) add("content_format","/format")
      for (e in gen_check_schema(args$output_schema)$errors) { e$path <- paste0("/args/output_schema",e$path); errors[[length(errors)+1L]] <- e }
    }
  }
  values <- .gen_piece_values(block,args)
  for (entry in values) errors <- c(errors,.gen_piece_value_errors(entry$value,paste0("/args",entry$path)))
  for (i in seq_along(args$additions)) {
    entry <- args$additions[[i]]
    if (.gen_object(entry) && .gen_scalar(entry$role) && !entry$role %in% unlist(spec$roles))
      add("content_role",paste0("/additions/",i-1L,"/role"))
  }
  for (role in names(gen_block_contracts("content")$roles)) {
    limit <- gen_block_contracts("content")$roles[[role]]$max
    count <- sum(vapply(args$additions,function(x) .gen_object(x) && identical(x$role,role),logical(1)))
    if (!is.null(limit) && count>limit) add("content_cardinality","/additions")
  }
  if (ai && !is.null(spec)) {
    if (!.gen_object(args$ai)) { add("required","/ai"); return(list(errors=errors,args=args)) }
    a <- args$ai
    if (block=="transcribe") { if (is.null(args$audio)) add("required","/audio") }
    else if (!length(a$content) || all(vapply(a$content,function(v) .gen_object(v) && .gen_scalar(v$text) && !nzchar(trimws(v$text)),logical(1)))) add("required","/ai/content")
    # Piece content belongs to ordered values/adendos; function parameters are settings.
    role_args <- unlist(lapply(gen_block_contracts("content")$roles[unlist(spec$roles)],`[[`,"argument"),use.names=FALSE)
    supplied_bindings <- intersect(c(spec$primary,role_args),names(a$parameters))
    for (key in supplied_bindings) if (!is.null(a$parameters[[key]]))
      add("content_parameter_binding",paste0("/ai/parameters/",key))
    agent <- a[intersect(names(a),c("service","model","contract_version","parameters","fixture"))]
    agent$operation <- spec$operation
    agent$contract_version <- a$contract_version %||% spec$function_version
    if (!is.null(args$output_schema)) agent$output_schema <- args$output_schema
    # Shared agent validator owns all function/service/model rules and defaults.
    checked <- gen_validate_agent_definition(agent,check_capabilities,purpose="authoring")
    for (e in c(checked$errors,checked$requirements)) { e$path <- paste0("/args/ai",e$path); errors[[length(errors)+1L]] <- e }
    args$ai$contract_version <- agent$contract_version
    # Do not serialize defaults as authored values; freeze only the profile pin.
    if ("parameter_schema" %in% names(checked$body$parameters)) args$ai$parameters["parameter_schema"] <- checked$body$parameters["parameter_schema"]
    if (checked$valid && !length(checked$requirements)) {
      bound <- unique(c(spec$primary,unlist(lapply(values,`[[`,"argument"))))
      params <- c(list(service=checked$body$service,model=checked$body$model),a$parameters %||% list())
      duplicate <- intersect(bound,names(params))
      for (key in duplicate) if (is.null(params[[key]])) params[key] <- NULL
      if (check_capabilities && "add_img" %in% bound) {
        cap <- tryCatch(gen_capabilities(a$service,a$model),error=function(e) NULL)
        support <- if(spec[["function"]]=="gen_txt") isTRUE(unname(cap$adapter_features["vision"])) else isTRUE(cap$image_references$supported)
        if (!support) add("image_not_supported","/additions")
      }
      call <- gen_validate_call(spec[["function"]],params,agent$contract_version,bound,check_capabilities)
      for (e in call$errors) { e$path <- sub("^/args","/args/ai/parameters",e$path); errors[[length(errors)+1L]] <- e }
    }
  }
  list(errors=errors,args=args)
}

.gen_piece_ports <- function(block,args,inputs) {
  errors <- list(); wanted <- list(); ports <- list()
  need <- function(key,type) {
    wanted[[key]] <<- list(type=type,optional=FALSE)
    if (is.null(inputs[[key]])) errors[[length(errors)+1L]] <<- .gen_issue("required_input",paste0("/inputs/",key))
    else if (!.gen_type_compatible(inputs[[key]],type)) errors[[length(errors)+1L]] <<- .gen_issue("incompatible_input",paste0("/inputs/",key),
      gen_error_message("port_type",list(expected=type,actual=inputs[[key]])))
  }
  for (entry in .gen_piece_values(block,args)) {
    v <- entry$value
    if (length(.gen_piece_value_errors(v,entry$path))) next
    if (!is.null(v$input)) {
      if(block=="judge" && identical(v$input,"candidates")) errors[[length(errors)+1L]] <- .gen_issue("content_value",paste0("/args",entry$path))
      else need(v$input,entry$type)
    }
    if (!is.null(v$text) && !.gen_type_compatible("asset:text",entry$type))
      errors[[length(errors)+1L]] <- .gen_issue("incompatible_input",paste0("/args",entry$path))
  }
  if (block=="judge") { need("candidates","list:text|list:json"); ports <- list(asset="asset:ballot") }
  if (block=="transcribe" || block=="text.join") ports <- list(asset="asset:text")
  if (block=="content") {
    spec <- .gen_content_spec(block,args)
    if (!is.null(spec)) ports <- list(asset=paste0("asset:",if(args$content_type=="text") args$format %||% "text" else spec$output))
  }
  for (key in setdiff(names(inputs),names(wanted))) errors[[length(errors)+1L]] <- .gen_issue("unused_input",paste0("/inputs/",key))
  list(valid=!length(errors),errors=errors,ports=ports,inputs=wanted)
}
