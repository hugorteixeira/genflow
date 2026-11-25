# Identify the material ports only when a real scene produces the public edit.
.gen_scene_interface <- function(nodes,ports) {
  by_id <- stats::setNames(nodes,vapply(nodes,`[[`,character(1),"id"))
  for (n in nodes) if (identical(n$type %||% n$block,"output")) {
    ref <- n$inputs[["cena"]]
    if (is.null(ref) || !identical(ref$port,"asset")) next
    producer <- by_id[[ref$node]]
    if (identical(producer$type %||% producer$block,"scene"))
      return(lapply(producer$inputs,function(r) ports[[r$node]][[r$port]]))
  }
  NULL
}

# Timed composition is portable data. Material inputs are named graph ports,
# so graph extraction can rename producer nodes without rewriting the edit.
.gen_scene_errors <- function(doc,path) {
  errors <- list()
  issue <- function(at) errors[[length(errors)+1L]] <<- .gen_issue("scene_document",at)
  integer_in <- function(x,min,max) is.numeric(x) && length(x)==1L && is.finite(x) && x==floor(x) && x>=min && x<=max
  array <- function(x) is.list(x) && is.null(names(x))
  key <- function(x) .gen_scalar(x) && nzchar(x) && nchar(x)<=128L
  keys <- function(x) array(x) && all(vapply(x,key,logical(1))) && !anyDuplicated(unlist(x,use.names=FALSE))
  if (!.gen_object(doc) || !identical(doc$format,"genflow-composition") ||
      !integer_in(doc$schema_version,1,1) || !identical(doc$layout,"timed") ||
      !.gen_object(doc$canvas) || !integer_in(doc$canvas$width,1,16384) ||
      !integer_in(doc$canvas$height,1,16384) || !array(doc$parts) || !keys(doc$ignored)) {
    issue(path); return(errors)
  }
  seen <- character()
  for (i in seq_along(doc$parts)) {
    p <- doc$parts[[i]]; at <- paste0(path,"/parts/",i-1L)
    if (!.gen_object(p) || !key(p$id) || p$id %in% seen || !.gen_scalar(p$name) ||
        !integer_in(p$minimum_ms,0,.Machine$integer.max) || !array(p$elements) || !keys(p$ignored) || !keys(p$edited)) {
      issue(at); next
    }
    seen <- c(seen,p$id); layers <- character()
    for (j in seq_along(p$elements)) {
      e <- p$elements[[j]]; ep <- paste0(at,"/elements/",j-1L)
      if (!.gen_object(e) || !key(e$id) || e$id %in% layers || !.gen_scalar(e$name) ||
          !.gen_scalar(e$role) || !e$role %in% c("image","character","video","voice","music","sound","text","title","caption") ||
          !integer_in(e$start_ms,0,.Machine$integer.max) || !integer_in(e$duration_ms,1,.Machine$integer.max-as.double(e$start_ms)) ||
          !integer_in(e$in_ms,0,.Machine$integer.max) || !integer_in(e$x,0,100) || !integer_in(e$y,0,100) ||
          !integer_in(e$width,1,100) || !integer_in(e$volume,0,100) ||
          (!is.null(e$speed_percent) && !integer_in(e$speed_percent,25,400)) ||
          !keys(e$edited) || !is.logical(e$linked) || length(e$linked)!=1L || is.na(e$linked) ||
          (isTRUE(e$linked) && !key(e[["source"]])) ||
          (e$role %in% c("text","title","caption") && !.gen_scalar(e$text)) ||
          (!is.null(e$input) && (!key(e$input) || nchar(e$input,type="bytes")>64L)) ||
          (!is.null(e$asset_id) && !key(e$asset_id)) ||
          ((is.null(e$input) || !isTRUE(e$linked)) && is.null(e$asset_id) && !e$role %in% c("text","title","caption"))) issue(ep)
      if (.gen_object(e) && key(e$id)) layers <- c(layers,e$id)
    }
  }
  errors
}
