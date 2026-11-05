# One traversal owns the reference fields of portable definitions. Consumers
# provide storage identities; this module never resolves or reads resources.
.gen_definition_walk <- function(body, kind, visit) {
  if (!.gen_object(body)) return(body)
  if (identical(kind,"agent") && !is.null(body$prompt_version_id))
    body$prompt_version_id <- visit(body$prompt_version_id,"prompt","/prompt_version_id",NULL)
  if (identical(kind,"workflow") && is.list(body$nodes)) for (i in seq_along(body$nodes)) {
    n <- body$nodes[[i]]
    if (!.gen_object(n) || !.gen_object(n$config)) next
    path <- paste0("/nodes/",i-1L,"/config/")
    for (field in c("agent_version_id","workflow_id")) if (!is.null(n$config[[field]]))
      n$config[field] <- list(visit(n$config[[field]],if(field=="workflow_id") "workflow" else "agent",paste0(path,field),n))
    for (field in c("agent_version_ids","artifact_ids")) if (is.list(n$config[[field]]))
      for (j in seq_along(n$config[[field]])) n$config[[field]][j] <- list(visit(n$config[[field]][[j]],
        if(field=="artifact_ids") "asset" else "agent",paste0(path,field,"/",j-1L),n))
    body$nodes[[i]] <- n
  }
  body
}

#' Inspect references in a portable definition
#'
#' Traverses only declared reference fields, including artifact references.
#' Incomplete values are retained so validators can report their original paths.
#' @param body A portable definition body.
#' @param kind Definition kind: `workflow`, `agent`, or `prompt`.
#' @return A list of references with `key`, `kind`, JSON pointer `path` and,
#'   for node references, `node_id` and `node_label`.
#' @export
gen_definition_references <- function(body, kind) {
  refs <- list()
  .gen_definition_walk(body,kind,function(key,kind,path,node) {
    refs[[length(refs)+1L]] <<- list(key=key,kind=kind,path=path,node_id=node$id,node_label=node$label)
    key
  })
  refs
}

#' Remap references in a portable definition
#'
#' Replaces only declared references, leaving prompts, labels and arbitrary JSON
#' unchanged. Workflow resource mappings contain definition identities; agent
#' and prompt mappings contain version identities. Missing mappings are errors.
#' @param body A portable definition body.
#' @param kind Definition kind: `workflow`, `agent`, or `prompt`.
#' @param resources Named list mapping resource keys to new keys or identities.
#' @param assets Named list mapping artifact keys to new keys or identities.
#' @return A remapped body. Does not mutate the supplied value or perform I/O.
#' @export
gen_remap_definition <- function(body, kind, resources = list(), assets = list()) {
  .gen_definition_walk(body,kind,function(key,kind,path,node) {
    map <- if(kind=="asset") assets else resources
    mapped <- if(.gen_scalar(key)) map[[key]] else NULL
    if (!.gen_scalar(mapped) || !nzchar(mapped)) .gen_validation_stop(list(.gen_issue(
      if(kind=="asset") "unknown_asset" else "unknown_resource",path,
      params=if(kind=="asset") list(asset=key) else list(resource=key),node_id=node$id)))
    mapped
  })
}

.gen_prune_definition <- function(value) {
  if (!identical(value$format,"genflow-studio-definition")) return(value)
  resources <- value$resources %||% list()
  names(resources) <- vapply(resources,`[[`,character(1),"key")
  keys <- function(body,kind,assets=FALSE) unique(vapply(Filter(function(r) identical(r$kind,"asset")==assets,
    gen_definition_references(body,kind)),`[[`,character(1),"key"))
  used <- keys(value$root$body,value$root$kind)
  repeat {
    more <- union(used,unlist(lapply(resources[used],function(r) keys(r$body,r$kind)),use.names=FALSE))
    if (setequal(used,more)) break
    used <- more
  }
  resources <- resources[names(resources) %in% used]
  assets <- c(keys(value$root$body,value$root$kind,TRUE),unlist(lapply(resources,function(r) keys(r$body,r$kind,TRUE)),use.names=FALSE))
  value$resources <- unname(resources)
  value$assets <- Filter(function(a) a$key %in% assets,value$assets %||% list())
  value
}
