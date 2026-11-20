# OpenRouter queues a video job. Gemini URI delivery returns a file resource
# which can be polled and downloaded independently after generation.
.gen_remote_video_submit <- function(service,model,prompt,image,source_video,budget,reference_clip=NULL,reference_sound=NULL) {
  encode <- function(path,kind) {
    info <- .gen_video_probe(path,kind,budget())
    if (kind=="video" && info$bytes > 50*1024^2) stop("Source video exceeds 50 MiB.")
    if(kind=="video" && model=="black-forest-labs/flux-video-edit" && info$duration_seconds>15) stop("Source video exceeds 15 seconds.")
    if(kind=="video" && !grepl("mp4",info$container,fixed=TRUE)) stop("Use an MP4 source video.")
    .gen_video_upload(path,info,service,budget())
  }
  if(service=="openrouter") {
    body <- list(prompt=prompt,model=model)
    if(!is.null(image)) body$frame_images <- list(list(type="image_url",frame_type="first_frame",image_url=list(url=encode(image,"image"))))
    if(!is.null(source_video)) body$input_references <- list(list(type="video_url",video_url=list(url=encode(source_video,"video"))))
    if(!is.null(reference_clip)) body$input_references <- list(list(type="video_url",video_url=list(url=encode(reference_clip,"video"))))
    if(!is.null(reference_sound)) body$input_references <- c(body$input_references,
      list(list(type="audio_url",audio_url=list(url=encode(reference_sound,"audio")))))
    x <- .gen_media_request(service,"POST","/api/v1/videos",body,budget())
    id <- x$id
  } else {
    parts <- list(list(type="text",text=prompt))
    if(!is.null(image)) parts <- c(parts,list(.gen_inline_part(encode(image,"image"),"image")))
    if(!is.null(source_video)) parts <- c(parts,list(.gen_inline_part(encode(source_video,"video"),"video")))
    x <- .gen_media_request(service,"POST","/v1beta/interactions",list(model=model,input=parts,
      response_format=list(type="video",delivery="uri")),budget())
    uri <- .gen_interaction_output(x,"video")$uri
    if(!.gen_scalar(uri) || !grepl("^(https://generativelanguage[.]googleapis[.]com/v1beta/)?files/[a-zA-Z0-9_-]+$",uri)) stop("Invalid Gemini file receipt.")
    id <- sub("^.*/","",uri)
  }
  .gen_video_check_receipt(list(schema_version=1L,service=service,model=model,id=id))
}

.gen_remote_video_poll <- function(r,timeout) {
  if(r$service=="openrouter") {
    x <- .gen_media_request("openrouter","GET",paste0("/api/v1/videos/",r$id),timeout=timeout)
    states <- c(pending="pending",processing="pending",in_progress="pending",completed="ready",failed="failed",expired="failed",cancelled="failed")
    state <- states[x$status]
  } else {
    x <- .gen_media_request("gemini","GET",paste0("/v1beta/files/",r$id),timeout=timeout)
    state <- c(PROCESSING="pending",ACTIVE="ready",FAILED="failed")[x$state]
  }
  if(length(state)!=1L || is.na(state)) stop("Unexpected video job status.")
  list(state=unname(state),receipt=r)
}
.gen_remote_video_file <- function(r,budget) {
  route <- if(r$service=="gemini") paste0("/v1beta/files/",r$id,":download?alt=media") else paste0("/api/v1/videos/",r$id,"/content?index=0")
  url <- paste0(.gen_media_origin(r$service),route)
  path <- tempfile(fileext=".mp4"); success <- FALSE
  on.exit(if(!success) unlink(path),add=TRUE)
  response <- httr::GET(url,.gen_media_headers(r$service),httr::timeout(budget()),httr::write_disk(path),
    httr::config(followlocation=FALSE,maxfilesize=512*1024^2))
  if(httr::status_code(response) %in% c(301L,302L,303L,307L,308L)) {
    location <- httr::headers(response)$location
    if(!.gen_scalar(location) || !startsWith(location,"https://")) stop("Invalid provider download redirect.")
    response <- httr::GET(location,httr::timeout(budget()),httr::write_disk(path,overwrite=TRUE),
      httr::config(followlocation=FALSE,maxfilesize=512*1024^2))
  }
  .gen_provider_check(response,expected=200L,stage="download")
  success <- TRUE; path
}
