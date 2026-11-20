# Only these REST endpoints are used for discovery. Pagination tokens are
# opaque; never pass an authenticated request to a server-selected host.
.update_models_openrouter <- function(directory=NULL,verbose=TRUE) {
  groups <- lapply(c("models","images/models","videos/models","speech"),function(kind) {
    path <- if(kind=="speech") "models?output_modalities=speech" else kind
    url <- paste0("https://openrouter.ai/api/v1/",path)
    rows <- list(); seen <- character()
    repeat {
      page <- .gen_catalog_get(url,"openrouter")
      if (!length(page$data)) stop("Missing OpenRouter model list: ",kind)
      rows <- c(rows,lapply(page$data,.gen_openrouter_row,kind=kind))
      url <- page$links[["next"]]
      if(is.null(url)) break
      if(startsWith(url,"/api/v1/models?")) url <- paste0("https://openrouter.ai",url)
      if(!startsWith(url,"https://openrouter.ai/api/v1/models?") || url %in% seen) stop("Invalid OpenRouter catalog cursor.")
      seen <- c(seen,url)
    }
    rows
  })
  .gen_catalog_write(do.call(c,groups),"openrouter",directory)
}

.gen_fal_category <- c("text-to-video"="Video","image-to-video"="Video","video-to-video"="Video",
  "text-to-image"="Image","image-to-image"="Image","text-to-speech"="TTS","speech-to-text"="STT",
  "text-to-music"="Music","text-to-sound-effects"="SFX")

.update_models_fal <- function(directory=NULL,verbose=TRUE) {
  rows <- list(); seen <- character(); cursor <- NULL
  repeat {
    page <- .gen_catalog_get("https://api.fal.ai/v1/models","fal",
      Filter(Negate(is.null),list(limit=10L,cursor=cursor,status="active",expand="openapi-3.0")))
    if (!is.list(page$models)) stop("Missing fal model list.")
    rows <- c(rows,lapply(page$models,function(x) {
      category <- x$metadata$category %||% ""
      type <- unname(.gen_fal_category[category])
      routes <- if(length(x$openapi)) tryCatch(.gen_fal_routes(x),error=function(e) list()) else .gen_reviewed_routes("fal",x$endpoint_id)
      if (!length(routes)) routes <- list(.gen_route(category,character(),if(is.na(type)) "unknown" else tolower(type),source=x$endpoint_id))
      .gen_catalog_row("fal",x$endpoint_id,if(is.na(type)) "Unknown" else type,x$metadata$description,routes)
    }))
    cursor <- page$next_cursor
    if (is.null(cursor) || !nzchar(cursor)) break
    if (cursor %in% seen) stop("Repeated fal catalog cursor.")
    seen <- c(seen,cursor)
  }
  .gen_catalog_write(rows,"fal",directory)
}

# A URI alone has no media type. A declared MIME or a published example
# identifies the output; the input schema independently qualifies the adapter.
.gen_schema_media <- function(service,id,input,output,category=NULL,revision=NULL,example=NULL) {
  props <- input$properties
  required <- unlist(input$required)
  reviewed <- .gen_reviewed_routes(service,id)
  fields <- if(service=="fal") c(image="image_url",audio="audio_url",source_video="video_url") else c(image="image",audio="audio",source_video="video")
  fields <- fields[fields %in% names(props)]
  output_kind <- "unknown"
  if(service=="fal" && !is.null(output$properties$video)) output_kind <- "video"
  if(service=="replicate" && identical(output$type,"string") && identical(output$format,"uri")) {
    mime <- output$contentMediaType %||% ""
    ref <- if (.gen_scalar(example)) sub("[?#].*$","",example) else ""
    if(startsWith(mime,"video/") || grepl("[.]mp4$",ref,ignore.case=TRUE)) output_kind <- "video"
    if(startsWith(mime,"image/") || grepl("[.](png|webp|jpe?g)$",ref,ignore.case=TRUE)) output_kind <- "image"
    if(startsWith(mime,"audio/") || grepl("[.](mp3|wav|flac)$",ref,ignore.case=TRUE)) output_kind <- "audio"
  }
  if(length(reviewed) && (is.null(input) || reviewed[[1]]$operation!="video.generate")) return(reviewed)
  if(length(reviewed)) {
    valid_output <- output_kind=="video" || service=="replicate" && identical(output$type,"string") && identical(output$format,"uri")
    if(valid_output && !length(setdiff(required,c("prompt",fields))) &&
       all(unlist(reviewed[[1]]$inputs) %in% names(fields)) && !is.null(props$prompt)) {
      for(i in seq_along(reviewed)) {
        reviewed[[i]]$revision <- revision
        reviewed[[i]]$required <- as.list(union(unlist(reviewed[[i]]$required),names(fields)[fields %in% required]))
        reviewed[[i]]$limits$wire <- as.list(fields)
      }
      return(reviewed)
    }
    return(list(.gen_route("unknown",character(),output_kind,source=id,revision=revision)))
  }
  if(service=="replicate" && identical(output$type,"array") && identical(output$items$format,"uri") && length(example)) {
    refs <- unlist(example,use.names=FALSE)
    if(is.character(refs) && all(grepl("[.](png|webp|jpe?g)([?#].*)?$",refs,ignore.case=TRUE))) output_kind <- "image"
  }
  if(service=="replicate") {
    voice <- .tts_replicate_pick_voice_field(props)
    text <- intersect(c("text","input_text"),names(props))
    if(output_kind=="audio" && length(text)==1L && !is.null(voice) &&
       !length(setdiff(required,text)) && identical(props[[text]]$type,"string")) {
      instructions <- .tts_replicate_pick_instructions_field(props)
      return(list(.gen_route("audio.speak",if(!is.null(instructions)) "instructions","audio",transport=service,source=id,revision=revision)))
    }
    audio <- intersect(c("audio","input_audio"),names(props))
    transcript <- !is.null(output$properties$transcription) || !is.null(output$properties$text) && !is.null(output$properties$segments)
    if(length(audio)==1L && transcript && !length(setdiff(required,audio)))
      return(list(.gen_route("audio.transcribe","audio","text","audio",service,id,revision)))
  }
  image <- if(service=="fal") category %in% c("text-to-image","image-to-image") && !is.null(output$properties$images) else output_kind=="image"
  if(isTRUE(image) && identical(props$prompt$type,"string")) {
    field <- if(service=="fal") intersect(c("image_urls","image_url"),names(props)) else intersect(c("input_images","input_image"),names(props))
    if(length(field)>1L) field <- character()
    if(!length(setdiff(required,c("prompt",field))) && (!identical(category,"image-to-image") || length(field))) {
      multiple <- length(field) && identical(props[[field]]$type,"array")
      n <- if(!length(field)) 0L else if(multiple) props[[field]]$maxItems %||% 1L else 1L
      return(list(.gen_route("image.generate",if(n>0) "add_img","image",if(any(field %in% required) || identical(category,"image-to-image")) "add_img",service,id,revision,
        limits=list(reference_field=if(length(field)) field, multiple=multiple,max_images=n,properties=as.list(names(props))))))
    }
  }
  if(service=="replicate") {
    # Unqualified image/audio/video fields can represent arbitrary references.
    # The prompt-only route is valid when no other input is required.
    fields <- character()
    compatible <- output_kind=="video" && .gen_scalar(revision)
  } else {
    fields <- fields[intersect(names(fields),switch(category %||% "", "image-to-video"="image","video-to-video"="source_video",character()))]
    compatible <- output_kind=="video" && category %in% c("text-to-video","image-to-video","video-to-video") &&
      (category=="text-to-video" || length(fields)>0L)
  }
  compatible <- isTRUE(compatible) && identical(props$prompt$type,"string") && !length(setdiff(required,c("prompt",fields)))
  required_roles <- names(fields)[fields %in% required]
  if(identical(category,"video-to-video")) required_roles <- union(required_roles,"source_video")
  list(.gen_route(if(compatible) "video.generate" else "unknown",names(fields),output_kind,required_roles,
    transport=if(compatible) service,source=id,revision=revision,limits=list(wire=as.list(fields),required_fields=as.list(required))))
}

.update_models_replicate <- function(directory=NULL,verbose=TRUE) {
  url <- "https://api.replicate.com/v1/models"; seen <- character(); rows <- list()
  repeat {
    page <- .gen_catalog_get(url,"replicate")
    if (!is.list(page$results)) stop("Missing Replicate model list.")
    rows <- c(rows,lapply(page$results,function(x) {
      id <- paste(x$owner,x$name,sep="/"); version <- x$latest_version
      schema <- version$openapi_schema$components$schemas
      routes <- .gen_schema_media("replicate",id,schema$Input,schema$Output,revision=version$id,example=x$default_example$output)
      type <- switch(routes[[1]]$operation,audio.speak="TTS",audio.transcribe="STT",
        switch(routes[[1]]$output,video="Video",image="Image",audio="Audio","Unknown"))
      .gen_catalog_row("replicate",id,type,x$description,routes)
    }))
    url <- page[["next"]]
    if (is.null(url)) break
    if (!startsWith(url,"https://api.replicate.com/v1/models?") || url %in% seen) stop("Invalid Replicate catalog cursor.")
    seen <- c(seen,url)
  }
  .gen_catalog_write(rows,"replicate",directory)
}

.gen_fal_model_schema <- function(model) {
  page <- .gen_catalog_get("https://api.fal.ai/v1/models","fal",list(endpoint_id=model,expand="openapi-3.0"))
  matches <- Filter(function(m) identical(m$endpoint_id,model),page$models)
  if(length(matches)!=1L) stop("fal model schema unavailable.")
  .gen_fal_routes(matches[[1L]])
}

.gen_fal_routes <- function(x) {
  model <- x$endpoint_id; spec <- x$openapi
  post <- spec$paths[[paste0("/",model)]]$post
  resolve <- function(schema) {
    ref <- schema[["$ref"]]
    if(is.null(ref)) return(schema)
    spec$components$schemas[[sub("^#/components/schemas/","",ref)]]
  }
  input <- resolve(post$requestBody$content[["application/json"]]$schema)
  out <- spec$paths[[paste0("/",model,"/requests/{request_id}")]]$get$responses[["200"]]$content[["application/json"]]$schema
  output <- resolve(out %||% post$responses[["200"]]$content[["application/json"]]$schema)
  if (!length(input$properties) || is.null(output)) stop("Incomplete fal OpenAPI schema.")
  .gen_schema_media("fal",model,input,output,x$metadata$category,
    paste0(openssl::sha256(charToRaw(as.character(jsonlite::toJSON(spec,auto_unbox=TRUE))))))
}

#' Inspect and cache a selected provider schema without running inference
#' @inheritParams gen_model_routes
#' @return Documented model routes.
#' @export
gen_refresh_model_schema <- function(service,model,directory=NULL) {
  if (!.gen_scalar(model) || !grepl("^[A-Za-z0-9_-]+(/[A-Za-z0-9_.-]+)+$",model) ||
      any(strsplit(model,"/",fixed=TRUE)[[1L]] %in% c(".",".."))) stop("Invalid model identifier.")
  routes <- switch(service,fal=.gen_fal_model_schema(model),replicate={
    x <- .gen_catalog_get(paste0("https://api.replicate.com/v1/models/",model),"replicate")
    v <- x$latest_version; schemas <- v$openapi_schema$components$schemas
    if (is.null(schemas$Input) || is.null(schemas$Output)) stop("Missing Replicate version schema.")
    .gen_schema_media(service,model,schemas$Input,schemas$Output,revision=v$id,example=x$default_example$output)
  },stop("Detailed OpenAPI lookup is available for fal and Replicate."))
  path <- .gen_route_file(service,model,directory)
  dir.create(dirname(path),recursive=TRUE,showWarnings=FALSE)
  tmp <- tempfile(tmpdir=dirname(path)); on.exit(unlink(tmp),add=TRUE)
  jsonlite::write_json(routes,tmp,auto_unbox=TRUE,null="null")
  if(!file.rename(tmp,path)) stop("Could not publish model schema.")
  routes
}

.gen_route_file <- function(service,model,directory=NULL) file.path(.genflow_resolve_models_dir(directory),"schemas",
  paste0(service,"-",as.character(openssl::sha256(charToRaw(model))),".json"))
