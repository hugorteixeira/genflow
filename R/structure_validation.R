.gen_type_compatible <- function(source, target) {
  if (!.gen_scalar(source) || !.gen_scalar(target)) return(FALSE)
  any(vapply(strsplit(target,"|",fixed=TRUE)[[1L]], function(t)
    t == "any" || t == source || endsWith(t,":*") && startsWith(source,substring(t,1L,nchar(t)-1L)),logical(1)))
}

.gen_template_variables <- function(text) {
  if (!.gen_scalar(text)) return(character())
  hits <- regmatches(text,gregexpr("\\{\\{[a-zA-Z][a-zA-Z0-9_]*\\}\\}",text,perl=TRUE))[[1L]]
  unique(gsub("[{}]","",hits))
}

# R's list() is convenient for empty argument objects. JSON explicitly
# distinguishes [] from {}, so retain that distinction at the JSON boundary.
.gen_parse_document <- function(text) {
  mark_arrays <- function(value, depth=0L) {
    if (!is.list(value) || depth>64L) return(value)
    if (!length(value) && is.null(names(value))) attr(value,"genflow.json_array") <- TRUE
    for (key in seq_along(value)) value[key] <- list(mark_arrays(value[[key]],depth+1L))
    value
  }
  mark_arrays(jsonlite::parse_json(text,simplifyVector=FALSE))
}

#' Validate the typed connections of a block
#' @param block Block identifier.
#' @param args Prepared block configuration or arguments.
#' @param inputs Named list of source port types, such as `asset:image`.
#' @param version Contract version.
#' @param interface Declared input ports of a referenced subworkflow.
#' @param exports Declared output ports of a referenced subworkflow.
#' @return A list with `valid`, `errors`, and inferred output `ports`.
#' @export
gen_block_ports <- function(block, args = list(), inputs = list(), version = 1L, interface = list(), exports = list()) {
  contract <- tryCatch(gen_block_contracts(block,version),error=function(e)e)
  if (inherits(contract,"error")) return(list(valid=FALSE,errors=list(.gen_issue("unknown_contract","",conditionMessage(contract))),ports=list()))
  errors <- list(); ports <- list()
  add <- function(key,message,code="incompatible_input") errors[[length(errors)+1L]] <<- .gen_issue(code,.gen_pointer("/inputs",key),message)
  if (!.gen_object(args) || !.gen_object(inputs)) return(list(valid=FALSE,errors=list(.gen_issue("invalid_arguments","","Arguments and inputs must be objects.")),ports=list()))
  for (key in names(inputs)) if (!.gen_scalar(inputs[[key]])) add(key,"Input must have one known content type.")
  if (length(errors)) return(list(valid=FALSE,errors=errors,ports=ports))
  need <- function(key,type,required=TRUE) {
    if (!key %in% names(inputs)) { if (required) add(key,"Required input connection is missing.","required_input"); return(NULL) }
    if (!.gen_type_compatible(inputs[[key]],type)) add(key,paste("Expected",type,"but received",inputs[[key]]))
    inputs[[key]]
  }
  only <- function(allowed) for (key in setdiff(names(inputs),allowed)) add(key,"Unknown input port.","unknown_input")
  kind <- function(type) if (is.null(type)) NULL else sub("^[^:]+:","",type)
  config_kind <- args$kind %||% "text"
  if (contract$kind %in% c("function","reserved")) {
    allowed <- names(contract$arguments$properties)
    only(allowed)
    for (key in intersect(names(inputs),allowed)) {
      expected <- contract$inputs[[key]]
      if (is.null(expected)) {
        types <- setdiff(unlist(contract$arguments$properties[[key]]$type),"null")
        expected <- paste0("value:",types,collapse="|")
      }
      if (nzchar(expected)) need(key,expected)
    }
    ports <- contract$outputs %||% list()
    if (block=="gen_vote") {
      if (identical(args$return_winner,"content") && !is.null(inputs$underlying_list)) ports$content <- sub("^list:","asset:",inputs$underlying_list)
      if (identical(args$return_winner,"id") || identical(args$return_winner,TRUE)) ports$content <- "asset:text"
    }
  } else if (block=="input") {
    only(character()); type <- args$value_type %||% "asset"
    ports <- stats::setNames(list(paste0(type,":",config_kind)),type)
  } else if (block=="generate") {
    for (key in names(inputs)) need(key,if (key=="image") "asset:image" else if (key=="audio") "asset:audio" else "asset:text|asset:json")
    ports <- list(candidates=paste0("candidates:",config_kind))
    count <- if (identical(args$source,"provided")) length(args$artifact_ids) else args$count
    if (!is.null(count) && count==1) ports$asset <- paste0("asset:",config_kind)
  } else if (block %in% c("judge","vote","select")) {
    candidates <- need("candidates","candidates:*")
    k <- kind(candidates)
    if (block=="judge") {
      for (key in setdiff(names(inputs),"candidates")) need(key,"asset:text|asset:json")
      ports <- list(asset="asset:json")
    } else if (block=="vote") {
      only(c("candidates","votes")); need("votes","list:json")
      if (!is.null(k)) ports <- list(round=paste0("round:",k))
    } else {
      only(c("candidates","evaluation"))
      if (!is.null(k)) { need("evaluation",paste0("round:",k),FALSE); ports <- list(asset=paste0("asset:",k)) }
    }
  } else if (block %in% c("list.pack","list.concat")) {
    prefix <- if (block=="list.pack") "asset" else "list"
    for (key in names(inputs)) need(key,paste0(prefix,":*"))
    if (length(unique(unlist(inputs)))>1L) add("","List inputs must have the same content type.")
    order <- unlist(args$order,use.names=FALSE)
    if (anyDuplicated(order) || !setequal(order,names(inputs))) add("","order must name each input exactly once.","invalid_order")
    k <- if (length(inputs)) kind(inputs[[1L]]) else config_kind
    ports <- list(list=paste0("list:",k))
  } else if (block %in% c("list.split","json.get")) {
    only("asset"); need("asset","asset:json")
    ports <- if (block=="list.split") list(list=paste0("list:",config_kind)) else list(asset=paste0("asset:",config_kind))
  } else if (block %in% c("list.item","candidates","text.join")) {
    only("list"); source <- need("list",if (block=="text.join") "list:text|list:json" else "list:*")
    if (block=="text.join") ports <- list(asset="asset:text") else if (!is.null(source))
      ports <- if (block=="list.item") list(asset=paste0("asset:",kind(source))) else list(candidates=paste0("candidates:",kind(source)))
  } else if (block=="template") {
    for (key in unique(c(.gen_template_variables(args$template),names(inputs)))) need(key,"asset:text|asset:json")
    ports <- list(asset="asset:text")
  } else if (block=="output") {
    if (!length(inputs)) add("","Output requires at least one input.","required_input")
    for (key in names(inputs)) need(key,"asset:*|list:*|candidates:*")
  } else if (block %in% c("subworkflow","foreach")) {
    wanted <- lapply(interface,function(x) if (is.list(x)) x$type else x)
    exported <- lapply(exports,function(x) if (is.list(x)) x$type else x)
    if (block=="foreach") {
      item <- args$item_input
      if (!.gen_scalar(item) || !item %in% names(wanted)) add("items","Unknown child item input.") else {
        need("items",sub("^asset:","list:",wanted[[item]])); wanted[[item]] <- NULL
      }
      only(c(names(wanted),"items"))
      for (key in names(exported)) {
        if (!startsWith(exported[[key]],"asset:")) add(key,"foreach children must export individual artifacts.")
        exported[[key]] <- sub("^asset:","list:",exported[[key]])
      }
    } else only(names(wanted))
    for (key in names(wanted)) need(key,wanted[[key]])
    ports <- exported
  }
  list(valid=!length(errors),errors=errors,ports=ports)
}

#' Validate a declarative workflow without running its operations
#'
#' A document contains `schema_version = 1`, `nodes`, and optional named
#' `outputs` and `workflows`. Each node contains `id`, `block`,
#' `contract_version`, `args`, and `inputs` (references with `node` and `port`).
#' A single node or an array of nodes is also accepted. Nested workflows are
#' resolved only from the document's `workflows` map. No source code, file paths,
#' URLs, providers, or workflow operations are executed during validation.
#'
#' Independent errors are accumulated across nodes, arguments, and connections.
#' Invalid prerequisites suppress checks that would only produce cascading
#' errors. Connected argument values remain pending until execution.
#' @param document A JSON string or parsed list containing a declarative graph.
#' @param require_output Require an explicit final output for a complete graph.
#' @return A report with `valid`, `errors`, `document` (a prepared copy), `ports`,
#'   `order`, and `pending`. The supplied input is never modified.
#' @export
gen_validate_structure <- function(document, require_output = .gen_default("gen_validate_structure","require_output")) {
  args <- list(require_output=require_output)
  if (!missing(document)) args["document"] <- list(document)
  call <- gen_validate_call("gen_validate_structure",args)
  if (!call$valid) return(list(valid=FALSE,errors=call$errors,document=NULL,ports=list(),order=list(),pending=list()))
  parsed <- if (.gen_scalar(document)) tryCatch(.gen_parse_document(document),error=function(e)NULL) else document
  if (!.gen_object(parsed) || is.null(parsed$workflows) || !length(parsed$workflows)) return(.gen_validate_structure_graph(document,require_output))
  definitions <- parsed$workflows
  if (!.gen_object(definitions) || length(definitions)>64L) return(list(valid=FALSE,errors=list(.gen_issue("invalid_workflows","/workflows","workflows must contain at most 64 named definitions.")),document=parsed,ports=list(),order=list(),pending=list()))
  reports <- list(); interfaces <- list(); errors <- list(); active <- character()
  resolve <- function(key) {
    if (!is.null(reports[[key]])) return(reports[[key]])
    if (key %in% active || length(active)>=8L) {
      issue <- .gen_issue("recursive_workflow",.gen_pointer("/workflows",key),"Subworkflows must be acyclic and at most 8 levels deep.")
      errors[[length(errors)+1L]] <<- issue
      return(list(valid=FALSE,errors=list(issue)))
    }
    value <- definitions[[key]]
    if (is.null(value)) return(NULL)
    if (.gen_object(value) && is.null(value$nodes) && isTRUE(value$external)) {
      # Explicit external interfaces are useful to clients that resolve immutable
      # versions themselves. Their implementations remain an external dependency.
      ok <- .gen_object(value$interface %||% list()) && .gen_object(value$exports %||% list())
      for (port in c(value$interface %||% list(),value$exports %||% list())) {
        type <- if (is.list(port)) port$type else port
        ok <- ok && .gen_scalar(type) && grepl("^(asset|list|candidates|round):(text|json|image|audio|video)$",type)
      }
      report <- list(valid=ok,errors=if(ok) list() else list(.gen_issue("invalid_interface","","Invalid external subworkflow interface.")),document=value)
      if (ok) interfaces[[key]] <<- value
    } else {
      active <<- c(active,key); on.exit(active <<- head(active,-1L))
      if (.gen_object(value) && is.list(value$nodes)) for (n in value$nodes) {
        if (.gen_object(n) && .gen_scalar(n$block) && n$block %in% c("subworkflow","foreach") && .gen_object(n$args) && .gen_scalar(n$args$workflow_version_id)) resolve(n$args$workflow_version_id)
      }
      if (.gen_object(value)) value$workflows <- interfaces
      report <- .gen_validate_structure_graph(value,TRUE)
      if (report$valid) {
        incoming <- list(); outgoing <- list()
        for (n in report$document$nodes) {
          if (n$block=="input") incoming[[n$args$key]] <- list(type=unname(unlist(report$ports[[n$id]]))[[1L]])
          if (n$block=="output") for (port in names(n$inputs)) {
            ref <- n$inputs[[port]]; outgoing[[port]] <- list(type=report$ports[[ref$node]][[ref$port]])
          }
        }
        for (port in names(report$document$outputs)) {
          ref <- report$document$outputs[[port]]; outgoing[[port]] <- list(type=report$ports[[ref$node]][[ref$port]])
        }
        interfaces[[key]] <<- list(interface=incoming,exports=outgoing)
        # Preserve the source definitions, never replace them with inferred ports.
        report$document$workflows <- definitions[[key]]$workflows
      }
    }
    for (e in report$errors) {
      e$path <- paste0(.gen_pointer("/workflows",key),e$path %||% "")
      errors[[length(errors)+1L]] <<- e
    }
    reports[[key]] <<- report
    report
  }
  for (key in names(definitions)) resolve(key)
  parsed$workflows <- interfaces
  result <- .gen_validate_structure_graph(parsed,require_output)
  result$errors <- c(errors,result$errors); result$valid <- !length(result$errors)
  result$document$workflows <- lapply(names(definitions),function(key) reports[[key]]$document %||% definitions[[key]])
  names(result$document$workflows) <- names(definitions)
  result
}

.gen_validate_structure_graph <- function(document, require_output = FALSE) {
  errors <- list(); ports <- list(); prepared <- list(); ordered <- character(); pending <- list()
  finish <- function(value=NULL) list(valid=!length(errors),errors=errors,document=value,ports=ports,order=as.list(ordered),pending=pending)
  add <- function(code,path,message,node=NULL) errors[[length(errors)+1L]] <<- .gen_issue(code,path,message,node)
  if (.gen_scalar(document)) {
    document <- tryCatch(.gen_parse_document(document),error=function(e)e)
    if (inherits(document,"error")) { add("invalid_json","",conditionMessage(document)); return(finish()) }
  }
  portable <- tryCatch({ .genflow_validate_portable(document); TRUE },error=function(e)e)
  if (inherits(portable,"error")) { add("nonportable_document","",conditionMessage(portable)); return(finish()) }
  if (is.list(document) && is.null(names(document))) document <- list(schema_version=1L,nodes=document)
  if (.gen_object(document) && !is.null(document$block)) {
    if (is.null(document$id)) document$id <- "call"
    document <- list(schema_version=1L,nodes=list(document))
  }
  if (!.gen_object(document)) { add("invalid_document","","Expected a workflow object."); return(finish()) }
  for (key in setdiff(names(document),c("schema_version","nodes","outputs","workflows","metadata"))) add("unknown_field",.gen_pointer("",key),"Unknown workflow field.")
  if (!is.numeric(document$schema_version) || length(document$schema_version)!=1L || !identical(as.numeric(document$schema_version),1)) add("unknown_format","/schema_version","Supported workflow schema_version is 1.")
  nodes <- document$nodes
  if (!is.list(nodes) || !is.null(names(nodes)) || !length(nodes) || length(nodes)>10000L) {
    add("invalid_nodes","/nodes","nodes must be an array of 1 to 10000 blocks."); return(finish())
  }
  ids <- vapply(nodes,function(n) if (.gen_object(n) && .gen_scalar(n$id)) n$id else "",character(1))
  prepared <- vector("list",length(nodes))
  duplicate <- duplicated(ids) | duplicated(ids,fromLast=TRUE)
  valid_ids <- nzchar(ids) & grepl("^[a-zA-Z][a-zA-Z0-9_-]*$",ids) & !duplicate
  usable <- rep(FALSE,length(nodes)); refs <- vector("list",length(nodes)); contracts <- vector("list",length(nodes))
  for (i in seq_along(nodes)) {
    n <- nodes[[i]]; path <- paste0("/nodes/",i-1L); id <- if (nzchar(ids[[i]])) ids[[i]] else NULL
    if (!.gen_object(n)) { add("invalid_node",path,"A block must be an object.",id); next }
    if (!valid_ids[[i]]) add(if (duplicate[[i]] && nzchar(ids[[i]])) "duplicate_id" else "invalid_id",paste0(path,"/id"),"Node IDs must be unique identifiers.",id)
    for (key in setdiff(names(n),c("id","block","contract_version","args","inputs","label","metadata"))) add("unknown_field",.gen_pointer(path,key),"Unknown block field.",id)
    version <- n$contract_version %||% 1L
    contract <- tryCatch(gen_block_contracts(n$block,version),error=function(e)e)
    if (is.null(n$block) || inherits(contract,"error")) { add("unknown_contract",paste0(path,"/block"),if (inherits(contract,"error")) conditionMessage(contract) else "A block identifier is required.",id); next }
    contracts[[i]] <- contract
    bindings <- n$inputs %||% list()
    if (!.gen_object(bindings)) { add("invalid_bindings",paste0(path,"/inputs"),"Inputs must be an object with unique port names.",id); bindings <- list() }
    for (key in names(bindings)) {
      ref <- bindings[[key]]; rp <- .gen_pointer(paste0(path,"/inputs"),key)
      if (!.gen_object(ref) || !setequal(names(ref),c("node","port")) || !.gen_scalar(ref$node) || !.gen_scalar(ref$port)) {
        add("invalid_reference",rp,"A connection must contain node and port strings.",id); next
      }
      source <- which(ids==ref$node & valid_ids)
      if (length(source)!=1L) { add("unknown_node",paste0(rp,"/node"),paste("Unknown or ambiguous source node:",ref$node),id); next }
      refs[[i]][key] <- list(ref)
    }
    checked <- gen_validate_call(n$block,n$args %||% list(),version,
      bound=if (contract$kind=="function") names(bindings) else character())
    for (e in checked$errors) add(e$code,paste0(path,e$path),e$message,id)
    n$args <- checked$args; n$inputs <- bindings; n$contract_version <- version
    prepared[[i]] <- n
    pending[[ids[[i]] %||% as.character(i)]] <- checked$pending
    usable[[i]] <- checked$valid && valid_ids[[i]] && length(refs[[i]])==length(bindings)
  }
  # One adjacency traversal, including invalid branches, so long linear scripts
  # do not repeatedly scan every remaining node.
  dependency_ids <- lapply(refs,function(bindings) unique(vapply(bindings,function(r) r$node,character(1))))
  degrees <- lengths(dependency_ids)
  sources <- match(unlist(dependency_ids,use.names=FALSE),ids)
  successors <- split(rep(seq_along(nodes),degrees),sources)
  queue <- integer(length(nodes)); ready <- which(valid_ids & degrees==0L)
  tail <- length(ready); if (tail) queue[seq_len(tail)] <- ready
  head <- 1L
  while (head <= tail) {
    i <- queue[[head]]; head <- head + 1L
    for (next_node in successors[[as.character(i)]] %||% integer()) {
      degrees[[next_node]] <- degrees[[next_node]] - 1L
      if (degrees[[next_node]]==0L && valid_ids[[next_node]]) { tail <- tail+1L; queue[[tail]] <- next_node }
    }
  }
  traversal <- if (tail) queue[seq_len(tail)] else integer()
  remaining <- setdiff(which(valid_ids),traversal)
  for (i in traversal) {
      n <- prepared[[i]]; path <- paste0("/nodes/",i-1L); id <- ids[[i]]
      if (is.null(n)) next
      input_types <- list(); complete <- TRUE
      for (key in names(refs[[i]])) {
        ref <- refs[[i]][[key]]; source <- match(ref$node,ids)
        type <- ports[[ref$node]][[ref$port]]
        if (is.null(type)) {
          if (usable[[source]]) add("unknown_port",paste0(.gen_pointer(paste0(path,"/inputs"),key),"/port"),paste("Unknown output port:",ref$port),id)
          complete <- FALSE
        } else input_types[[key]] <- type
      }
      if (!complete) usable[[i]] <- FALSE
      if (usable[[i]] && complete) {
        child <- list()
        if (n$block %in% c("subworkflow","foreach")) {
          child <- document$workflows[[n$args$workflow_version_id]]
          if (is.null(child)) { add("unknown_workflow",paste0(path,"/args/workflow_version_id"),"Subworkflow must be declared in workflows.",id); usable[[i]] <- FALSE; next }
          # Definitions supply contracts, not arbitrary R resolvers or DB access.
          if (!.gen_object(child) || !.gen_object(child$interface %||% list()) || !.gen_object(child$exports %||% list())) {
            add("invalid_workflow",paste0(path,"/args/workflow_version_id"),"Subworkflow requires interface and exports objects.",id); usable[[i]] <- FALSE; next
          }
        }
        checked <- gen_block_ports(n$block,n$args,input_types,n$contract_version,child$interface %||% list(),child$exports %||% list())
        for (e in checked$errors) add(e$code,paste0(path,e$path),e$message,id)
        usable[[i]] <- checked$valid
        ports[id] <- list(checked$ports)
        if (n$block=="select" && !is.null(n$inputs$evaluation)) {
          producer <- prepared[[match(n$inputs$evaluation$node,ids)]]
          if (!identical(producer$inputs$candidates,n$inputs$candidates)) add("candidate_set_mismatch",paste0(path,"/inputs/evaluation"),"Selection and voting must reference the same candidate set.",id)
        }
      }
      ordered <- c(ordered,id)
  }
  for (i in remaining) add("cycle",paste0("/nodes/",i-1L,"/inputs"),"Dependency cycle prevents this block from being ordered.",ids[[i]])
  output <- document$outputs %||% list()
  if (!.gen_object(output)) add("invalid_outputs","/outputs","Outputs must be named references.") else for (key in names(output)) {
    ref <- output[[key]]
    if (!.gen_object(ref) || !.gen_scalar(ref$node) || !.gen_scalar(ref$port) || is.null(ports[[ref$node]][[ref$port]])) add("invalid_output",.gen_pointer("/outputs",key),"Output refers to an unavailable node or port.")
  }
  if (require_output && !length(output) && !any(vapply(nodes,function(n) .gen_object(n) && identical(n$block,"output"),logical(1)))) add("required_output","/outputs","A complete workflow requires a final output.")
  document$nodes <- lapply(seq_along(nodes),function(i) prepared[[i]] %||% nodes[[i]])
  finish(document)
}
