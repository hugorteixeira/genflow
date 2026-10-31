#' Prepare a workflow or fragment for deterministic materialization
#'
#' Uses the same portable definition format and validator as
#' [gen_validate_structure()]. Selection includes all upstream nodes. Unused
#' resources and assets are removed. A non-empty prefix namespaces root node
#' IDs, root input/output names, and resource/asset keys with `prefix_`.
#' Nested workflow interfaces retain their own scope. Authored argument omissions
#' are preserved; the report's graph metadata contains effective defaults.
#' No operation is executed
#' and no definitions, files or database records are created.
#' @param document A Studio workflow body or complete definition bundle, as a
#'   parsed list or JSON string. Compact `block/args` graphs are not accepted.
#' @param prefix Optional namespace, containing letters, digits and underscores
#'   and starting with a letter. Empty preserves identifiers.
#' @param node_ids Optional root node IDs to select, including their ancestors.
#'   NULL selects all nodes.
#' @param check_capabilities Check installed provider capabilities. FALSE is for
#'   isolated fixture runtimes.
#' @return A validation report with the prepared `document`, `node_map` and
#'   `input_map` (original to prepared), and `resource_sources`/`asset_sources`
#'   (prepared key to original key). Invalid input returns all validation errors.
#' @export
gen_materialize_structure <- function(document, prefix = .gen_default("gen_materialize_structure","prefix"),
                                      node_ids = .gen_default("gen_materialize_structure","node_ids"),
                                      check_capabilities = TRUE) {
  args <- list(prefix=prefix,node_ids=node_ids)
  if (!missing(document)) args["document"] <- list(document)
  call <- gen_validate_call("gen_materialize_structure",args)
  if (!call$valid) return(list(valid=FALSE,errors=call$errors,document=NULL))
  checked <- gen_validate_structure(document,check_capabilities=check_capabilities)
  if (!checked$valid) return(checked)
  value <- if (.gen_scalar(document)) .gen_parse_document(document) else document
  bundled <- identical(value$format,"genflow-studio-definition")
  body <- if (bundled) value$root$body else value
  fail <- function(code,path,message) {
    checked$valid <- FALSE; checked$errors <- list(.gen_issue(code,path,message)); checked
  }
  if (bundled && !identical(value$root$kind,"workflow") ||
      !is.list(body$nodes) || !length(body$nodes) ||
      any(!vapply(body$nodes,function(n) .gen_object(n) && .gen_scalar(n$type),logical(1))))
    return(fail("unsupported_document","","Materialization requires a Studio workflow body or workflow definition bundle."))
  ids <- vapply(body$nodes,`[[`,character(1),"id")
  selected <- if (is.null(node_ids)) ids else unlist(call$args$node_ids,use.names=FALSE)
  if (any(!selected %in% ids)) return(fail("unknown_node","/node_ids","Selection contains an unknown root node."))
  repeat {
    parents <- unlist(lapply(body$nodes[ids %in% selected],function(n) lapply(n$inputs,`[[`,"node")),use.names=FALSE)
    next_ids <- union(selected,parents)
    if (setequal(next_ids,selected)) break
    selected <- next_ids
  }
  body$nodes <- body$nodes[ids %in% selected]
  namespace <- function(key) if (nzchar(prefix)) paste0(prefix,"_",key) else key
  node_map <- stats::setNames(lapply(ids[ids %in% selected],namespace),ids[ids %in% selected])
  input_map <- list()
  body$nodes <- lapply(body$nodes,function(n) {
    n$id <- node_map[[n$id]]
    if (length(n$inputs)) n$inputs <- lapply(n$inputs,function(ref) { ref$node <- node_map[[ref$node]]; ref })
    if (n$type=="input") {
      input_map[[n$config$key]] <<- namespace(n$config$key)
      n$config$key <- namespace(n$config$key)
    }
    if (n$type=="output") names(n$inputs) <- vapply(names(n$inputs),namespace,character(1))
    n
  })
  resource_sources <- list(); asset_sources <- list()
  if (bundled) {
    resources <- value$resources %||% list(); assets <- value$assets %||% list()
    names(resources) <- vapply(resources,`[[`,character(1),"key")
    refs <- function(kind,b) {
      if (kind=="agent") return(b$prompt_version_id %||% character())
      if (kind!="workflow") return(character())
      unique(unlist(lapply(b$nodes,function(n) c(n$config$agent_version_id,
        unlist(n$config$agent_version_ids,use.names=FALSE),n$config$workflow_version_id)),use.names=FALSE))
    }
    used <- refs("workflow",body)
    repeat {
      next_used <- union(used,unlist(lapply(resources[used],function(r) refs(r$kind,r$body)),use.names=FALSE))
      if (setequal(used,next_used)) break
      used <- next_used
    }
    resources <- resources[names(resources) %in% used]
    asset_refs <- function(kind,b) if (kind=="workflow") unlist(lapply(b$nodes,function(n)n$config$artifact_ids),use.names=FALSE) else character()
    used_assets <- c(asset_refs("workflow",body),unlist(lapply(resources,function(r)asset_refs(r$kind,r$body)),use.names=FALSE))
    assets <- Filter(function(a)a$key %in% used_assets,assets)
    remap <- function(kind,b) {
      if (kind=="agent" && !is.null(b$prompt_version_id)) b$prompt_version_id <- namespace(b$prompt_version_id)
      if (kind=="workflow") b$nodes <- lapply(b$nodes,function(n) {
        for (field in c("agent_version_id","workflow_version_id")) if (!is.null(n$config[[field]])) n$config[[field]] <- namespace(n$config[[field]])
        for (field in c("agent_version_ids","artifact_ids")) if (!is.null(n$config[[field]])) n$config[[field]] <- lapply(n$config[[field]],namespace)
        n
      })
      b
    }
    value$root$body <- remap("workflow",body)
    value$resources <- unname(lapply(resources,function(r) {
      key <- r$key; r$key <- namespace(key); resource_sources[[r$key]] <<- key
      r$body <- remap(r$kind,r$body); r
    }))
    value$assets <- unname(lapply(assets,function(a) {
      key <- a$key; a$key <- namespace(key); asset_sources[[a$key]] <<- key; a
    }))
  } else value <- body
  checked <- gen_validate_structure(value,check_capabilities=check_capabilities)
  checked$document <- value
  c(checked,list(node_map=node_map,input_map=input_map,resource_sources=resource_sources,asset_sources=asset_sources))
}
