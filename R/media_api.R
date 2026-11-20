# Fixed provider origins for credentialed media calls. Do not repeat a POST.
.gen_media_origin <- function(service) switch(service,gemini="https://generativelanguage.googleapis.com",
  openrouter="https://openrouter.ai",stop("Unsupported media API."))
.gen_media_headers <- function(service) {
  key <- if(service=="gemini") .gemini_api_key() else Sys.getenv("OPENROUTER_API_KEY")
  if (!nzchar(key)) .gen_stop("missing_credential",params=list(credential=service),stage="preflight")
  if(service=="gemini") httr::add_headers(`x-goog-api-key`=key,`Api-Revision`="2026-05-20")
  else httr::add_headers(Authorization=paste("Bearer",key))
}
.gen_media_request <- function(service,method,path,body=NULL,timeout=60,stream=FALSE,binary=FALSE) {
  url <- paste0(.gen_media_origin(service),path)
  args <- list(url,.gen_media_headers(service),httr::timeout(timeout),
    httr::config(followlocation=FALSE,maxfilesize=200*1024^2))
  if (method=="POST") args <- c(args,list(body=body,encode="json"))
  r <- do.call(if(method=="GET") httr::GET else httr::POST,args)
  .gen_provider_check(r,expected=c(200L,201L,202L))
  if(stream) return(httr::content(r,as="text",encoding="UTF-8"))
  if(binary) return(httr::content(r,as="raw"))
  .gen_provider_body(r,max_bytes=200*1024^2)
}
.gen_interaction_output <- function(reply,kind) {
  result <- reply[[paste0("output_",kind)]]
  if(!is.null(result)) return(result)
  for(step in reply$steps) if(identical(step$type,"model_output"))
    for(part in step$content) if(identical(part$type,kind)) return(part)
  .gen_stop("provider_response_invalid",stage="output_validation",outcome="succeeded")
}
.gen_inline_part <- function(uri,type) list(type=type,mime_type=sub("^data:([^;]+);.*","\\1",uri),
  data=sub("^data:[^,]+,","",uri))
.gen_img_media <- function(service,prompt,model,temp,steps,h,y,directory,label_sanitized,budget,references=list()) {
  if(service=="gemini") {
    input <- c(list(list(type="text",text=prompt)),lapply(references,.gen_inline_part,type="image"))
    ratios <- c("1:1"=1,"16:9"=16/9,"9:16"=9/16,"4:3"=4/3,"3:4"=3/4,"3:2"=3/2,"2:3"=2/3)
    ratio <- names(ratios)[which.min(abs(log(ratios/(y/h))))]
    body <- list(model=model,input=input,response_format=list(type="image",aspect_ratio=ratio))
    part <- .gen_interaction_output(.gen_media_request(service,"POST","/v1beta/interactions",body,budget()),"image")
    data <- part$data; sent <- list(aspect_ratio=ratio)
  } else {
    body <- list(model=model,prompt=prompt)
    limits <- Filter(function(r) r$operation=="image.generate" && !is.null(r$transport),gen_model_routes(service,model))[[1]]$limits
    if(!is.null(limits$output_format)) body$output_format <- limits$output_format
    params <- limits$parameters
    ratios <- unlist(params$aspect_ratio$values); ratios <- grep("^[0-9]+:[0-9]+$",ratios,value=TRUE)
    if(length(ratios)) {
      values <- vapply(strsplit(ratios,":",fixed=TRUE),function(x) as.numeric(x[1])/as.numeric(x[2]),numeric(1))
      body$aspect_ratio <- ratios[which.min(abs(log(values/(y/h))))]
    }
    if(length(references)) body$input_references <- lapply(references,function(uri) list(type="image_url",image_url=list(url=uri)))
    reply <- .gen_media_request(service,"POST","/api/v1/images",body,budget())
    if(length(reply$data)!=1L) .gen_stop("image_output_count",stage="output_validation")
    data <- reply$data[[1]]$b64_json; sent <- list(aspect_ratio=body$aspect_ratio)
  }
  if(!.gen_scalar(data) || !nzchar(data)) .gen_stop("provider_response_invalid",stage="output_validation",outcome="succeeded")
  source <- .gen_image_source(openssl::base64_decode(data))
  .gen_image_publish(.gen_image_receipt(list(source),service,model,h,y,sent),directory,label_sanitized,budget)
}
.gen_media_music <- function(a,budget) {
  prompt <- paste(a$prompt, if(isTRUE(a$instrumental)) "Instrumental only, no vocals." else paste("Lyrics:",a$lyrics),
    sprintf("Requested duration: %s seconds.",a$duration_seconds),sep="\n")
  if(a$service=="gemini") {
    body <- list(model=a$model,input=prompt,response_format=list(type="audio",mime_type="audio/mpeg"))
    return(.gen_interaction_output(.gen_media_request("gemini","POST","/v1beta/interactions",body,budget()),"audio")$data)
  }
  body <- list(model=a$model,messages=list(list(role="user",content=prompt)),modalities=list("text","audio"),
    stream=TRUE,audio=list(format="mp3"))
  sse <- .gen_media_request("openrouter","POST","/api/v1/chat/completions",body,budget(),stream=TRUE)
  events <- sub("^data: *","",grep("^data:",strsplit(sse,"\r?\n")[[1]],value=TRUE))
  if (!length(events) || tail(events,1)!="[DONE]") .gen_stop("sound_transport",stage="inference",outcome="unknown")
  parts <- lapply(head(events,-1),function(e) {
    x <- jsonlite::fromJSON(e,simplifyVector=FALSE)
    if(!is.null(x$error)) .gen_stop("sound_response",stage="inference",outcome="unknown")
    if(length(x$choices)) x$choices[[1]]$delta$audio$data
  })
  paste(unlist(parts),collapse="")
}


.gen_media_speech <- function(service,model,text,voice,format,instructions,timeout) {
  .gen_sound_tools()
  budget <- .gen_image_budget(timeout)
  route <- Filter(function(r) identical(r$operation,"audio.speak") && !is.null(r$transport),
                  gen_model_routes(service,model))[[1L]]
  voice <- voice %||% route$limits$voices[[1L]]
  if(service=="gemini") {
    part <- list(type="text",text=text)
    if(!is.null(instructions)) part$annotations <- list(list(type="speech_metadata",style=instructions))
    body <- list(model=model,input=list(list(type="user_input",content=list(part))),
      response_format=list(type="audio",mime_type="audio/wav"),
      generation_config=list(speech_config=list(list(voice=voice))))
    x <- .gen_interaction_output(.gen_media_request(service,"POST","/v1beta/interactions",body,budget()),"audio")
    bytes <- openssl::base64_decode(x$data); wire <- "wav"
  } else {
    wire <- route$limits$wire_format
    body <- list(model=model,input=text,voice=voice,response_format=wire)
    if(!is.null(instructions)) body$instructions <- instructions
    bytes <- .gen_media_request(service,"POST","/api/v1/audio/speech",body,budget(),binary=TRUE)
  }
  if(!length(bytes)) .gen_stop("sound_invalid",stage="output_validation",outcome="succeeded")
  file <- tempfile(fileext=paste0(".",wire)); writeBin(bytes,file)
  if(wire!="pcm" && !identical(.gen_audio_magic_format(file),wire)) {
    unlink(file); .gen_stop("sound_invalid",stage="output_validation",outcome="succeeded")
  }
  if(format==wire) return(file)
  on.exit(unlink(file),add=TRUE)
  out <- tempfile(fileext=paste0(".",format))
  input <- if(wire=="pcm") c("-f","s16le","-ar",as.character(route$limits$sample_rate),"-ac","1")
  encoding <- if(format=="pcm") c("-f","s16le") else if(format=="opus") c("-c:a","libopus")
  result <- processx::run("ffmpeg",c("-v","error","-nostdin",input,"-i",file,encoding,out),timeout=budget(),error_on_status=FALSE)
  if(result$status!=0L) { unlink(out); .gen_stop("sound_invalid",stage="output_validation",outcome="succeeded") }
  out
}
