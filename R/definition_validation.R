# The portable definition envelope is data, not a dependency on the Studio app.
# Adapt its bodies to the existing graph validator; keep diagnostics and the
# prepared document in the caller's format. No resolver performs I/O.
# Studio v1 historically serialized empty R object fields as []. Normalize only
# fields whose shape is known to be an object; graph/list arrays remain arrays.
.gen_definition_objects <- function(value, fields = character()) {
  object <- function(x) if (is.list(x) && !length(x)) structure(list(),names=character()) else x
  value <- object(value)
  if (.gen_object(value)) for (key in intersect(names(value),fields)) value[key] <- list(object(value[[key]]))
  value
}

.gen_definition_shape <- function(value, properties, required = character(), path = "") {
  # Definition envelope limits are UTF-8 bytes, matching the v1 persistence API.
  bytes <- function(x, schema, at) {
    errors <- list()
    if (.gen_scalar(x) && !is.null(schema$maxLength) && nchar(x)<=schema$maxLength && nchar(x,type="bytes")>schema$maxLength)
      errors <- list(.gen_issue("too_long",at,params=list(limit=schema$maxLength,unit="bytes")))
    if (is.list(x) && !is.null(schema$items)) for (i in seq_along(x)) errors <- c(errors,bytes(x[[i]],schema$items,.gen_pointer(at,as.character(i-1L))))
    errors
  }
  limits <- list()
  if (.gen_object(value)) for (key in intersect(names(value),names(properties))) limits <- c(limits,bytes(value[[key]],properties[[key]],.gen_pointer(path,key)))
  fields <- names(Filter(function(p) "object" %in% unlist(p$type) && !"array" %in% unlist(p$type),properties))
  c(limits,.gen_schema_errors(.gen_definition_objects(value,fields),list(type="object",properties=properties,
    required=as.list(required),additionalProperties=FALSE),path))
}

#' Validate and prepare a portable agent definition
#'
#' Checks the operation, pinned contract, parameters and output schema without
#' resolving a prompt reference or contacting a provider. Use
#' [gen_validate_structure()] for a complete bundle and its dependencies.
#' @param body A portable agent definition, not a resolved runtime snapshot.
#' @param purpose `execution` (default) requires complete configuration;
#'   `authoring` returns configuration `requirements` separately from errors.
#' @param check_capabilities Check installed provider capabilities. FALSE is
#'   intended for isolated fixture runtimes.
#' @return A list with `valid`, structured `errors`, prepared `body` and `contract`.
#' @export
gen_validate_agent_definition <- function(body, check_capabilities = TRUE, purpose = "execution") {
  requirements <- list()
  body <- .gen_definition_objects(body,c("parameters","fixture","output_schema"))
  text <- list(type="string",pattern="\\S")
  errors <- .gen_definition_shape(body,list(operation=text,service=text,
    model=list(type=c("string","null"),pattern="\\S"),parameters=list(type="object"),
    prompt_version_id=list(type=c("string","null"),pattern="\\S",maxLength=100),
    contract_version=list(type="integer",minimum=1),output_schema=list(type=c("object","null")),
    fixture=list(type="object")),"operation")
  finish <- function(contract=NULL) list(valid=!length(errors),ready=!length(errors) && !length(requirements),
    requirements=.gen_diagnostic_context(requirements,body,contract$id,contract$version),errors=.gen_diagnostic_context(errors,body,contract$id,contract$version),body=body,contract=contract)
  portable <- tryCatch({ .genflow_validate_portable(body); NULL }, error=function(e)e)
  if (!is.null(portable)) errors <- c(errors,list(.gen_issue("nonportable_document","",conditionMessage(portable))))
  policy <- c(.gen_schema_errors(check_capabilities,list(type="boolean"),"/check_capabilities"),
    .gen_schema_errors(purpose,list(type="string",enum=list("execution","authoring")),"/purpose"))
  errors <- c(errors,policy)
  if (!.gen_object(body) || !is.null(portable) || length(policy)) return(finish())
  add <- function(code,path,message) errors[[length(errors)+1L]] <<- .gen_issue(code,path,message)
  version <- body$contract_version %||% 1L
  if (!is.numeric(version) || length(version)!=1L || !is.finite(version) || version<1 || version!=floor(version)) return(finish())
  catalog <- Filter(function(c) !is.null(c$studio) && identical(c$studio$id,body$operation) &&
    isTRUE(c$version == version),gen_block_contracts())
  if (length(catalog)!=1L) { add("unknown_operation","/operation",gen_error_message("unknown_operation")); return(finish()) }
  contract <- catalog[[1L]]; params <- body$parameters %||% list()
  owned <- names(Filter(function(p) identical(p[["x-owner"]],"runtime"),contract$arguments$properties))
  for (key in intersect(names(params),owned)) add("backend_owned_argument",.gen_pointer("/parameters",key),gen_error_message("backend_owned_argument",list(argument=key)))
  if (!.gen_object(params)) return(finish(contract))
  args <- params
  if (isTRUE(contract$studio$available)) for (key in intersect(names(body),c("service","model"))) args[key] <- body[key]
  # Optional bindable fields may be authored literally or supplied by the graph.
  bindings <- unlist(contract$studio$bindings,use.names=FALSE)
  for (key in bindings) if (is.null(args[[key]])) args[key] <- NULL
  identity_missing <- if (.gen_authoring(purpose)) setdiff(c("service","model"),names(args)) else character()
  requirements <- lapply(identity_missing,function(key) .gen_requirement("required",paste0("/",key)))
  checked <- .gen_prepare_call(contract$id,args,contract$version,
    bound=union(if (isTRUE(contract$studio$available)) setdiff(names(contract$inputs),names(args)) else character(),identity_missing),
    check_capabilities=check_capabilities && !length(identity_missing),bindings_known=FALSE)
  for (e in checked$errors) {
    e$path <- sub("^/args","/parameters",e$path)
    if (e$path %in% c("/parameters/service","/parameters/model")) e$path <- sub("/parameters","",e$path)
    errors[[length(errors)+1L]] <- e
  }
  if (!is.null(body$output_schema)) {
    if (!identical(body$operation,"text.generate")) add("invalid_schema","/output_schema",gen_error_message("text_output_schema"))
    for (e in gen_check_schema(body$output_schema)$errors) { e$path <- paste0("/output_schema",e$path); errors[[length(errors)+1L]] <- e }
  }
  if (!isTRUE(contract$studio$available) && !identical(body$service,contract$studio$planned_service))
    add("unsupported_provider","/service",gen_error_message("reserved_provider",list(service=contract$studio$planned_service)))
  if (check_capabilities && !is.null(body$fixture)) add("fixture_disabled","/fixture",gen_error_message("fixture_disabled"))
  body$contract_version <- contract$version
  body$parameters <- checked$args[setdiff(names(checked$args),owned)]
  if (isTRUE(contract$studio$available)) for (key in intersect(c("service","model"),names(checked$args))) body[key] <- checked$args[key]
  finish(contract)
}

.gen_definition_workflow <- function(body, agents, interfaces, assets, require_output, check_capabilities, purpose = "execution", prompts = list()) {
  body <- .gen_definition_objects(body,"ui")
  errors <- list(); requirements <- list(); unresolved <- character(); nodes <- list(); used <- list(); derived <- list()
  require_config <- function(code,path,id=NULL) {
    issue <- .gen_requirement(code,path,id)
    if (.gen_authoring(purpose)) requirements[[length(requirements)+1L]] <<- issue else errors[[length(errors)+1L]] <<- issue
  }
  add <- function(code,path,message,id=NULL) errors[[length(errors)+1L]] <<- .gen_issue(code,path,message,id)
  object <- list(type="object"); text <- list(type="string")
  errors <- .gen_definition_shape(body,list(schema_version=list(type="integer",enum=list(1L)),
    nodes=list(type="array",items=object),ui=object),c("schema_version","nodes"))
  finish <- function(report=list(ports=list(),order=list(),pending=list())) {
    report$errors <- c(errors,report$errors); report$valid <- !length(report$errors); report$document <- body
    report$requirements <- c(requirements,report$requirements); report$ready <- report$valid && !length(report$requirements)
    report
  }
  if (!.gen_object(body)) return(finish())
  if (!is.list(body$nodes) || !is.null(names(body$nodes)) || !length(body$nodes) || length(body$nodes)>100L) {
    add("invalid_nodes","/nodes",gen_error_message("studio_node_count")); return(finish())
  }
  coordinates <- function(value,path,viewport=FALSE) {
    props <- list(x=list(type="number",minimum=-1e6,maximum=1e6),y=list(type="number",minimum=-1e6,maximum=1e6))
    if (viewport) props$zoom <- list(type="number",exclusiveMinimum=0,maximum=1e6)
    errors <<- c(errors,.gen_definition_shape(value,props,names(props),path))
  }
  if (.gen_object(body$ui) && !is.null(body$ui$viewport)) coordinates(body$ui$viewport,"/ui/viewport",TRUE)
  for (i in seq_along(body$nodes)) {
    n <- .gen_definition_objects(body$nodes[[i]],c("config","inputs","ui")); path <- paste0("/nodes/",i-1L)
    # Normalize legacy empty objects for validation only; keep authored bindings
    # unchanged so snapshots and existing result identities retain their shape.
    errors <- c(errors,.gen_definition_shape(n,list(id=list(type="string",maxLength=64),type=text,
      contract_version=list(type="integer",minimum=1),config=object,inputs=object,
      position=object,label=list(type="string",maxLength=200),notes=list(type="string",maxLength=10000),ui=object),c("id","type"),path))
    if (!.gen_object(n)) { nodes[i] <- list(n); next }
    if (!is.null(n$position)) coordinates(n$position,paste0(path,"/position"))
    if (.gen_object(n$inputs)) for (key in names(n$inputs)) {
      ip <- .gen_pointer(paste0(path,"/inputs"),key)
      if (nchar(key,type="bytes")>64L) add("invalid_port",ip,gen_error_message("input_name_length"),n$id)
      errors <- c(errors,.gen_definition_shape(n$inputs[[key]],list(node=list(type="string",maxLength=64),
        port=list(type="string",maxLength=64)),c("node","port"),ip))
    }
    block <- if (identical(n$type,"validate")) "gen_validate_structure" else if (identical(n$type,"materialize")) "gen_materialize_structure" else n$type
    contract <- if (.gen_scalar(block)) tryCatch(gen_block_contracts(block,n$contract_version %||% 1L),error=function(e)NULL) else NULL
    if (!is.null(contract) && contract$kind!="workflow" && !n$type %in% c("validate","materialize"))
      add("invalid_node_type",paste0(path,"/type"),gen_error_message("invalid_node_type"),n$id)
    config <- n$config %||% list()
    if (.gen_object(config)) {
      if (!is.null(config$agent_drafts) && !(n$type=="judge" || n$type=="generate" && identical(config$source %||% "ai","ai")))
        add("agent_configuration_conflict",paste0(path,"/config/agent_drafts"),gen_error_message("agent_configuration_conflict"),n$id)
      if (.gen_scalar(n$type) && n$type %in% c("generate","judge") &&
          (n$type=="judge" || identical(config$source %||% contract$arguments$properties$source$default,"ai"))) {
        ids <- config$agent_version_ids %||% if (!is.null(config$agent_version_id)) list(config$agent_version_id) else list()
        proposals <- config$agent_drafts
        if (!is.null(proposals) && length(ids)) add("agent_configuration_conflict",paste0(path,"/config/agent_drafts"),gen_error_message("agent_configuration_conflict"),n$id)
        if (!length(ids) && is.null(proposals)) require_config("required_agent",paste0(path,"/config/agent_version_id"),n$id)
        if (!is.null(proposals)) require_config("agent_draft_pending",paste0(path,"/config/agent_drafts"),n$id)
        # v1 saves can retain the singular field after editing candidate slots.
        # The plural form has always taken precedence in execution.
        if (!is.null(config$agent_version_ids)) config$agent_version_id <- NULL
        selected <- if (is.list(ids)) lapply(ids,function(key) if (.gen_scalar(key)) agents[[key]] else NULL) else list()
        if (is.list(proposals) && is.null(names(proposals))) {
          selected <- lapply(seq_along(proposals),function(j) {
            a <- .gen_agent_proposal(proposals[[j]],prompts,check_capabilities)
            for (e in a$errors) { e$path <- paste0(path,"/config/agent_drafts/",j-1L,e$path); e$node_id <- n$id; errors[[length(errors)+1L]] <<- e }
            if (a$valid) a else NULL
          })
        }
        used[[as.character(i)]] <- Filter(Negate(is.null),selected)
        if (!length(selected) && is.null(config$kind) && n$type=="generate") unresolved <- c(unresolved,n$id)
        if (length(selected) && all(vapply(selected,function(a) !is.null(a),logical(1)))) {
          kinds <- vapply(selected,function(a) if (!is.null(a$body$output_schema)) "json" else a$contract$studio$output,character(1))
          if (n$type=="generate") {
            # Validate author-supplied fields before replacing derived values.
            for (field in intersect(names(config),c("kind","output_schema"))) {
              for (e in .gen_schema_errors(config[[field]],contract$arguments$properties[[field]],paste0(path,"/config/",field)))
                { e$node_id <- n$id; errors[[length(errors)+1L]] <- e }
            }
            if (!"count" %in% names(config)) config$count <- contract$arguments$properties$count$default %||% length(selected)
            if (length(selected)!=1L && !isTRUE(length(selected)==config$count)) add("agent_count",paste0(path,"/config/count"),gen_error_message("agent_count"),n$id)
            if (length(unique(kinds))!=1L) add("mixed_outputs",paste0(path,"/config"),gen_error_message("mixed_outputs"),n$id) else config$kind <- kinds[[1L]]
            schemas <- lapply(selected,function(a) a$body$output_schema)
            if (!all(vapply(schemas,function(s) .gen_json_equal(s,schemas[[1L]]),logical(1)))) add("mixed_schemas",paste0(path,"/config"),gen_error_message("mixed_schemas"),n$id)
            config["output_schema"] <- schemas[1L]
          } else {
            if (length(selected)!=1L || any(vapply(selected,function(a) !identical(a$body$operation,"text.generate") || !is.null(a$body$output_schema),logical(1))))
              add("invalid_judge",paste0(path,"/config"),gen_error_message("invalid_judge"),n$id)
          }
        }
      }
      if (identical(n$type,"generate") && identical(config$source,"provided")) {
        if (!length(config$artifact_ids)) require_config("required_assets",paste0(path,"/config/artifact_ids"),n$id)
        for (key in config$artifact_ids) if (.gen_scalar(key) && !is.null(assets[[key]]) && !identical(assets[[key]]$kind,config$kind))
          add("asset_kind",paste0(path,"/config/artifact_ids"),gen_error_message("asset_kind"),n$id)
      }
      if (identical(n$type,"generate") && .gen_scalar(config$source) && config$source %in% c("manual","provided") && is.null(config$kind))
        add("required_kind",paste0(path,"/config/kind"),gen_error_message("required_kind"),n$id)
      if (!is.null(config$output_schema)) {
        if (!identical(config$kind,"json")) add("invalid_schema",paste0(path,"/config/output_schema"),gen_error_message("json_output_schema"),n$id)
        for (e in gen_check_schema(config$output_schema)$errors) { e$path <- paste0(path,"/config/output_schema",e$path); e$node_id <- n$id; errors[[length(errors)+1L]] <- e }
      }
    }
    nodes[[i]] <- list(id=n$id,block=block,contract_version=n$contract_version %||% 1L,args=config,inputs=n$inputs %||% list())
  }
  report <- .gen_validate_structure_graph(list(schema_version=body$schema_version,nodes=nodes,workflows=interfaces),require_output,check_capabilities,purpose,unresolved)
  report$errors <- lapply(report$errors,function(e) {
    e$path <- sub("^(/nodes/[0-9]+)/args","\\1/config",e$path)
    e$path <- sub("^(/nodes/[0-9]+)/block","\\1/type",e$path); e
  })
  report$requirements <- lapply(report$requirements,function(e) { e$path <- sub("^(/nodes/[0-9]+)/args","\\1/config",e$path); e })
  # Cross-resource constraints need the inferred ports, but never generated values.
  for (i in seq_along(nodes)) {
    n <- report$document$nodes[[i]]; path <- paste0("/nodes/",i-1L)
    if (!.gen_object(n) || !.gen_scalar(n$block) || !.gen_object(n$args)) next
    types <- lapply(n$inputs,function(ref) if (.gen_object(ref) && .gen_scalar(ref$node) && .gen_scalar(ref$port)) report$ports[[ref$node]][[ref$port]] else NULL)
    for (a in used[[as.character(i)]]) {
      template <- a$template %||% "{{input}}"
      vars <- .gen_template_variables(template)
      textual <- names(Filter(function(t) !is.null(t) && t %in% c("asset:text","asset:json"),types))
      if (n$block=="judge") textual <- setdiff(textual,"candidates")
      if (a$body$operation!="audio.transcribe" && !all(vars %in% textual)) {
        if (.gen_authoring(purpose) && all(vapply(setdiff(vars,textual),function(key) is.null(types[[key]]),logical(1)))) require_config("prompt_binding",paste0(path,"/inputs"),n$id)
        else add("prompt_binding",paste0(path,"/inputs"),gen_error_message("prompt_binding",list(variables=paste(setdiff(vars,textual),collapse=", "))),n$id)
      }
      if (n$block=="generate") {
        bindings <- a$contract$studio$bindings
        if (length(bindings) && (!isTRUE(a$proposal) || all(c("service","model") %in% names(a$body)))) {
          args <- c(list(service=a$body$service,model=a$body$model),a$body$parameters)
          connected <- intersect(names(bindings),names(n$inputs))
          bound <- c(setdiff(names(a$contract$inputs),unlist(bindings,use.names=FALSE)),unlist(bindings[connected],use.names=FALSE))
          for (key in bound) if (is.null(args[[key]])) args[key] <- NULL
          for (key in connected) {
            expected <- strsplit(a$contract$inputs[[bindings[[key]]]],"|",fixed=TRUE)[[1L]]
            if (!is.null(types[[key]]) && !types[[key]] %in% expected)
              add("incompatible_input",paste0(path,"/inputs/",key),gen_error_message("incompatible_input"),n$id)
          }
          checked <- gen_validate_call(a$contract$id,args,a$contract$version,bound,check_capabilities)
          for (error in checked$errors) {
            key <- sub("^/args/","",error$path)
            error$path <- paste0(path,if (key %in% connected) "/inputs/" else "/config/",key)
            error$node_id <- n$id; errors[[length(errors)+1L]] <- error
          }
        }
        if (a$body$operation=="audio.transcribe" && is.null(types$audio)) require_config("required_audio",paste0(path,"/inputs/audio"),n$id)
        if (!is.null(types$audio) && a$body$operation!="audio.transcribe") add("incompatible_input",paste0(path,"/inputs/audio"),gen_error_message("audio_only_transcription"),n$id)
        if (!is.null(types$image)) {
          declared <- strsplit(a$contract$studio$inputs$image %||% "","|",fixed=TRUE)[[1]]
          declared <- ifelse(grepl(":",declared,fixed=TRUE),declared,paste0("asset:",declared))
          accepts <- types$image %in% declared
          if (accepts && check_capabilities && (!isTRUE(a$proposal) || all(c("service","model") %in% names(a$body)))) {
            cap <- tryCatch(gen_capabilities(a$body$service,a$body$model),error=function(e) NULL)
            if (a$body$operation=="text.generate") accepts <- isTRUE(cap$adapter_features$vision)
            if (a$body$operation=="image.generate") accepts <- isTRUE(cap$image_references$supported)
          }
          if (!accepts) add("incompatible_input",paste0(path,"/inputs/image"),gen_error_message("image_not_supported"),n$id)
        }
      }
    }
    if (n$block=="judge" && .gen_object(n$args$policy) && is.numeric(n$args$policy$quorum) && is.numeric(n$args$count) &&
        isTRUE(n$args$policy$quorum > n$args$count)) add("invalid_quorum",paste0(path,"/config/policy/quorum"),gen_error_message("invalid_quorum"),n$id)
    if (n$block %in% c("judge","vote","select") && .gen_scalar(n$id) && .gen_scalar(types$candidates))
      derived[[n$id]] <- list(kind=sub("^candidates:","",types$candidates))
    if (n$block=="generate" && identical(n$args$source,"provided") && is.list(n$args$artifact_ids))
      n$args$count <- length(n$args$artifact_ids)
    # Prepared defaults stay in the Studio body. Layout and IDs are untouched.
    body$nodes[[i]]$config <- n$args; body$nodes[[i]]$contract_version <- n$contract_version
  }
  schemas <- list()
  ids <- vapply(nodes,function(n) if (.gen_object(n) && .gen_scalar(n$id)) n$id else "",character(1))
  for (id in unlist(report$order,use.names=FALSE)) {
    i <- match(id,ids); n <- report$document$nodes[[i]]
    if (!.gen_object(n$args) || is.null(report$ports[[id]])) next
    incoming <- lapply(n$inputs,function(r) if (.gen_object(r) && .gen_scalar(r$node) && .gen_scalar(r$port)) schemas[[r$node]][r$port][[1L]] else NULL)
    out <- stats::setNames(rep(list(NULL),length(report$ports[[id]])),names(report$ports[[id]]))
    if (n$block %in% c("input","generate")) out[] <- rep(list(n$args$output_schema),length(out))
    if (n$block %in% c("list.pack","list.concat") && length(incoming)) {
      if (!all(vapply(incoming,function(s) .gen_json_equal(s,incoming[[1L]]),logical(1))))
        add("mixed_schemas",paste0("/nodes/",i-1L,"/inputs"),gen_error_message("mixed_schemas"),id)
      out[] <- list(incoming[[1L]])
    }
    if (n$block %in% c("list.item","candidates")) out[] <- list(incoming$list)
    if (n$block=="select") out[] <- list(incoming$candidates)
    if (n$block %in% c("list.split","json.get") && identical(n$args$pointer,"") && identical(n$args$kind,"json"))
      out[] <- list(if (n$block=="list.split") incoming$asset$items else incoming$asset)
    if (n$block %in% c("subworkflow","foreach") && .gen_scalar(n$args$workflow_id))
      out <- lapply(interfaces[[n$args$workflow_id]]$exports,function(x)x$schema)
    schemas[[id]] <- out
    if (n$block=="select") derived[[id]]["output_schema"] <- list(out$asset)
  }
  incoming <- list(); outgoing <- list()
  for (n in body$nodes) if (.gen_object(n) && .gen_scalar(n$id)) {
    if (identical(n$type,"input") && .gen_object(n$config) && .gen_scalar(n$config$key) && length(report$ports[[n$id]]))
      incoming[[n$config$key]] <- list(type=unname(unlist(report$ports[[n$id]]))[[1L]],schema=n$config$output_schema)
    if (identical(n$type,"output") && .gen_object(n$inputs)) for (key in names(n$inputs)) {
      r <- n$inputs[[key]]
      if (.gen_object(r) && .gen_scalar(r$node) && .gen_scalar(r$port) && !is.null(report$ports[[r$node]][[r$port]]))
        outgoing[[key]] <- c(r,list(type=report$ports[[r$node]][[r$port]],schema=schemas[[r$node]][[r$port]]))
    }
  }
  report$interface <- incoming; report$exports <- outgoing; report$derived <- derived
  report$schemas <- schemas
  finish(report)
}

.gen_validate_definition_bundle <- function(document, require_output, check_capabilities, purpose = "execution") {
  requirements <- list(); prompts <- list()
  errors <- list(); bundle_errors <- NULL; resource_order <- character(); reports <- list(); agents <- list(); interfaces <- list(); plans <- list()
  add <- function(code,path,message) errors[[length(errors)+1L]] <<- .gen_issue(code,path,message)
  collect <- function(items,prefix) for (e in items) { e$path <- paste0(prefix,e$path); errors[[length(errors)+1L]] <<- e }
  finish <- function() {
    root <- reports[["1"]] %||% list()
    list(valid=!length(errors),ready=!length(errors) && !length(requirements),requirements=requirements,errors=errors,document=document,ports=root$ports %||% list(),
      order=root$order %||% list(),pending=root$pending %||% list(),resource_order=as.list(resource_order),
      assets_verified=FALSE, bundle_errors=bundle_errors %||% errors,
      graph=root[c("order","ports","schemas","interface","exports","height","derived")], plans=plans)
  }
  portable <- tryCatch({ .genflow_validate_portable(document); NULL },error=function(e)e)
  if (!is.null(portable)) { add("nonportable_document","",conditionMessage(portable)); return(finish()) }
  object <- list(type="object")
  errors <- .gen_definition_shape(document,list(format=list(type="string",enum=list("genflow-studio-definition")),
    schema_version=list(type="integer",enum=list(1L)),source=list(type="string",enum=list("draft","version")),
    root=object,resources=list(type="array",maxItems=3300,items=object),assets=list(type="array",maxItems=3200,items=object)),c("format","schema_version","root"))
  if (nchar(jsonlite::toJSON(document,auto_unbox=TRUE,null="null",force=TRUE,digits=NA),type="bytes")>3*1024^2) { add("document_limit","",gen_error_message("document_limit")); return(finish()) }
  resources <- document$resources %||% list(); manifest <- document$assets %||% list()
  array <- function(x,limit) is.list(x) && is.null(names(x)) && length(x)<=limit
  if (!array(resources,3300) || !array(manifest,3200)) return(finish())
  entries <- c(list(document$root),resources); keys <- rep("",length(entries)); kinds <- rep("",length(entries))
  paths <- c("/root",paste0("/resources/",seq_along(resources)-1L))
  meta <- list(kind=list(type="string",enum=as.list(c("prompt","agent","workflow"))),name=list(type="string",pattern="\\S",maxLength=200),
    description=list(type="string",maxLength=10000),tags=list(type="array",maxItems=20,items=list(type="string",pattern="\\S",maxLength=64)),body=object)
  for (i in seq_along(entries)) {
    e <- entries[[i]]; properties <- meta
    if (i>1L) properties$key <- list(type="string",pattern="\\S",maxLength=100)
    collect(.gen_definition_shape(e,properties,c("kind","name","body",if(i>1L) "key")),paths[[i]])
    if (!.gen_object(e)) next
    if (.gen_scalar(e$key)) keys[[i]] <- e$key
    if (.gen_scalar(e$kind)) kinds[[i]] <- e$kind
  }
  duplicate <- duplicated(keys) | duplicated(keys,fromLast=TRUE)
  for (i in which(duplicate & nzchar(keys))) add("duplicate_resource",paste0(paths[[i]],"/key"),gen_error_message("duplicate_resource"))
  index <- stats::setNames(as.list(which(nzchar(keys) & !duplicate)),keys[nzchar(keys) & !duplicate])
  assets <- list()
  for (i in seq_along(manifest)) {
    a <- manifest[[i]]; path <- paste0("/assets/",i-1L)
    collect(.gen_definition_shape(a,list(key=list(type="string",pattern="\\S",maxLength=100),
      kind=list(type="string",enum=as.list(c("text","json","image","audio","video"))),
      sha256=list(type="string",pattern="^[a-f0-9]{64}$")),c("key","kind","sha256")),path)
    if (.gen_object(a) && .gen_scalar(a$key)) {
      if (!is.null(assets[[a$key]])) add("duplicate_asset",paste0(path,"/key"),gen_error_message("duplicate_asset"))
      assets[[a$key]] <- a
    }
  }
  dependencies <- vector("list",length(entries))
  ref <- function(i,key,kind,path) {
    if (!.gen_scalar(key) || !nzchar(key)) { add("invalid_reference",path,gen_error_message("invalid_reference")); return() }
    target <- index[[key]]
    if (is.null(target)) { add("unknown_resource",path,gen_error_message("unknown_resource",list(resource=key))); return() }
    if (kinds[[target]]!=kind) add("resource_kind",path,gen_error_message("resource_kind",list(expected=kind)))
    dependencies[[i]] <<- unique(c(dependencies[[i]],target))
  }
  for (i in seq_along(entries)) {
    e <- entries[[i]]; path <- paste0(paths[[i]],"/body")
    if (!.gen_object(e) || !.gen_object(e$body)) next
    for (reference in gen_definition_references(e$body,kinds[[i]])) {
      at <- paste0(path,reference$path)
      if (reference$kind=="asset") {
        key <- reference$key
        if (.gen_scalar(key) && is.null(assets[[key]])) add("unknown_asset",at,gen_error_message("unknown_asset",list(asset=key)))
      } else ref(i,reference$key,reference$kind,at)
    }
  }
  state <- integer(length(entries)); ordered <- integer(); heights <- integer(length(entries))
  visit <- function(i,depth=0L) {
    if (state[[i]]==2L) return()
    if (state[[i]]==1L || depth>10L) { add("recursive_resource",paths[[i]],gen_error_message("recursive_resource")); return() }
    state[[i]] <<- 1L
    for (j in dependencies[[i]]) visit(j,depth+1L)
    child <- dependencies[[i]][kinds[dependencies[[i]]]=="workflow"]
    if (length(child)) heights[[i]] <<- 1L+max(heights[child])
    if (heights[[i]]>8L) add("workflow_depth",paths[[i]],gen_error_message("workflow_depth"))
    state[[i]] <<- 2L; ordered <<- c(ordered,i)
  }
  for (i in seq_along(entries)) visit(i)
  if (sum(kinds[-1L]=="workflow")>64L) add("workflow_limit","/resources",gen_error_message("workflow_limit"))
  # Envelope, references and published dependencies must remain valid even
  # when the root is an incomplete draft. Root execution diagnostics stay in errors.
  bundle_errors <- errors
  for (i in ordered) {
    before <- length(errors)
    e <- entries[[i]]; path <- paste0(paths[[i]],"/body")
    if (!.gen_object(e) || !.gen_object(e$body)) next
    if (kinds[[i]]=="prompt") collect(.gen_definition_shape(e$body,list(template=list(type="string",pattern="\\S",maxLength=100000)),"template"),path)
    if (kinds[[i]]=="prompt" && nzchar(keys[[i]]) && .gen_scalar(e$body$template)) prompts[[keys[[i]]]] <- e$body$template
    if (kinds[[i]]=="agent") {
      checked <- gen_validate_agent_definition(e$body,check_capabilities); collect(checked$errors,path)
      e$body <- checked$body
      if (checked$valid && nzchar(keys[[i]])) {
        prompt <- if (.gen_scalar(e$body$prompt_version_id)) index[[e$body$prompt_version_id]] else NULL
        checked$template <- if (is.null(e$body$prompt_version_id)) "{{input}}" else if (!is.null(prompt) &&
          .gen_object(entries[[prompt]]$body) && .gen_scalar(entries[[prompt]]$body$template)) entries[[prompt]]$body$template else ""
        agents[[keys[[i]]]] <- checked
      }
    }
    if (kinds[[i]]=="workflow") {
      # Bundle resources represent published dependencies. Only the root is an
      # editable draft; an incomplete dependency cannot be materialized as a version.
      checked <- .gen_definition_workflow(e$body,agents,interfaces,assets,if(i==1L) require_output else TRUE,
        check_capabilities,if(i==1L) purpose else "execution",prompts)
      for (r in checked$requirements) { r$path <- paste0(path,r$path); requirements[[length(requirements)+1L]] <- r }
      checked$height <- heights[[i]]
      collect(checked$errors,path); reports[[as.character(i)]] <- checked; e$body <- checked$document
      if (nzchar(keys[[i]])) plans[[keys[[i]]]] <- checked[c("order","ports","schemas","interface","exports","height","derived")]
      if (checked$valid && nzchar(keys[[i]])) {
        interfaces[[keys[[i]]]] <- list(interface=checked$interface,exports=checked$exports)
      }
    }
    if (i>1L && length(errors)>before) bundle_errors <- c(bundle_errors,errors[seq.int(before+1L,length(errors))])
    entries[[i]] <- e
  }
  document$root <- entries[[1L]]; document$resources <- unname(entries[-1L])
  resource_order <- keys[ordered[ordered>1L]]
  if (nchar(jsonlite::toJSON(document,auto_unbox=TRUE,null="null",force=TRUE,digits=NA),type="bytes")>3*1024^2) {
    add("document_limit","",gen_error_message("document_limit"))
    bundle_errors <- c(bundle_errors,errors[length(errors)])
  }
  finish()
}
