#' Extract an exact selection as a reusable workflow
#'
#' Keeps selected nodes and replaces incoming boundary connections with typed
#' input nodes. Outgoing boundary connections and terminal outputs become public
#' outputs. Unlike [gen_materialize_structure()], ancestors are not included.
#' References are pruned; no operation, provider or persistence is invoked.
#' @param document A valid Studio body or complete definition bundle, as a list
#'   or JSON string.
#' @param node_ids Exact node IDs to extract. NULL selects the whole workflow.
#' @param check_capabilities Check installed provider capabilities.
#' @return A validation report with the extracted `document` and `boundary`
#'   maps from public input/output names to original source ports.
#' @export
gen_extract_structure <- function(document, node_ids = .gen_default("gen_extract_structure","node_ids"),
                                  check_capabilities = TRUE) {
  args <- list(node_ids=node_ids)
  if (!missing(document)) args["document"] <- list(document)
  call <- gen_validate_call("gen_extract_structure",args)
  if (!call$valid) return(list(valid=FALSE,errors=call$errors,document=NULL))
  checked <- gen_validate_structure(document,check_capabilities=check_capabilities)
  if (!checked$valid) return(checked)
  value <- if (.gen_scalar(document)) .gen_parse_document(document) else document
  bundled <- identical(value$format,"genflow-studio-definition")
  body <- if(bundled) value$root$body else value
  fail <- function(code,path) { checked$valid <- FALSE; checked$errors <- list(.gen_issue(code,path)); checked }
  if (bundled && !identical(value$root$kind,"workflow") || !length(body$nodes) ||
      any(!vapply(body$nodes,function(n) .gen_scalar(n$type),logical(1)))) return(fail("unsupported_document",""))
  ids <- vapply(body$nodes,`[[`,character(1),"id")
  selected <- if(is.null(node_ids)) ids else unlist(call$args$node_ids,use.names=FALSE)
  if (any(!selected %in% ids)) return(fail("selected_node_missing","/node_ids"))
  graph <- checked$graph
  nodes <- body$nodes[ids %in% selected]
  boundary <- list(inputs=list(),outputs=list())
  occupied_ids <- selected
  input_keys <- unlist(lapply(nodes,function(n) if(n$type=="input") n$config$key),use.names=FALSE)
  output_keys <- unlist(lapply(nodes,function(n) if(n$type=="output") names(n$inputs)),use.names=FALSE)
  unique_name <- function(base,used) {
    base <- substr(base,1L,54L); result <- base; suffix <- 1L
    while(result %in% used) { suffix <- suffix+1L; result <- paste0(base,"_",suffix) }
    result
  }
  same_ref <- function(a,b) identical(a$node,b$node) && identical(a$port,b$port)
  inputs <- list()
  for (i in seq_along(nodes)) for (key in names(nodes[[i]]$inputs)) {
    ref <- nodes[[i]]$inputs[[key]]
    if (ref$node %in% selected) next
    reused <- which(vapply(boundary$inputs,function(old) same_ref(old,ref),logical(1)))
    if (length(reused)) {
      input <- inputs[[reused[[1L]]]]
    } else {
      type <- graph$ports[[ref$node]][[ref$port]]
      if (!grepl("^(asset|list|candidates):",type)) return(fail("component_boundary_type",paste0("/nodes/",match(nodes[[i]]$id,ids)-1L,"/inputs/",key)))
      parts <- strsplit(type,":",fixed=TRUE)[[1L]]
      name <- unique_name(key,input_keys); input_keys <- c(input_keys,name)
      id <- unique_name(paste0("component_input_",length(inputs)+1L),occupied_ids); occupied_ids <- c(occupied_ids,id)
      input <- list(id=id,type="input",config=list(key=name,value_type=parts[[1L]],kind=parts[[2L]]))
      schema <- graph$schemas[[ref$node]][[ref$port]]
      if (!is.null(schema)) input$config$output_schema <- schema
      inputs[[length(inputs)+1L]] <- input
      boundary$inputs[[name]] <- ref
    }
    nodes[[i]]$inputs[[key]] <- list(node=input$id,port=input$config$value_type)
  }
  for (n in body$nodes[ids %in% selected]) if(n$type=="output")
    for(key in names(n$inputs)) boundary$outputs[[key]] <- n$inputs[[key]]
  exports <- list()
  expose <- function(ref,name) {
    if (any(vapply(boundary$outputs,function(old) same_ref(old,ref),logical(1)))) return()
    name <- unique_name(name,c(output_keys,names(exports)))
    exports[[name]] <<- ref; boundary$outputs[[name]] <<- ref
  }
  for (n in body$nodes[!ids %in% selected]) for(key in names(n$inputs)) {
    ref <- n$inputs[[key]]
    if(ref$node %in% selected) expose(ref,if(n$type=="output") key else paste0(ref$node,"_",ref$port))
  }
  internal_sources <- unlist(lapply(nodes,function(n) lapply(n$inputs,`[[`,"node")),use.names=FALSE)
  for (n in nodes) if(!n$id %in% internal_sources && n$type!="output") for(port in names(graph$ports[[n$id]]))
    expose(list(node=n$id,port=port),paste0(n$id,"_",port))
  if(length(exports)) nodes[[length(nodes)+1L]] <- list(id=unique_name("component_output",occupied_ids),type="output",inputs=exports)
  body$nodes <- c(inputs,nodes); body$ui <- NULL
  if(bundled) value$root$body <- body else value <- body
  value <- .gen_prune_definition(value)
  result <- gen_validate_structure(value,require_output=TRUE,check_capabilities=check_capabilities)
  result$document <- value; result$boundary <- boundary
  result
}
