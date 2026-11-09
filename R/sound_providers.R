.gen_sound_base_url <- function(service,base_url=NULL) {
  value <- base_url %||% .genflow_first_env(paste0(toupper(service),"_BASE_URL"),
    if (service=="elevenlabs") "https://api.elevenlabs.io" else "http://127.0.0.1:8001")
  if (!.gen_sound_url_valid(value)) .gen_stop("sound_base_url_invalid",stage="preflight")
  sub("/+$","",value)
}

.gen_sound_headers <- function(service) {
  env <- paste0(toupper(service),"_API_KEY"); key <- Sys.getenv(env,"")
  if (service=="elevenlabs" && !nzchar(key)) .gen_stop("missing_credential",params=list(credential=env),stage="preflight")
  if (!nzchar(key)) return(httr::add_headers())
  if (service=="elevenlabs") httr::add_headers(`xi-api-key`=key) else httr::add_headers(Authorization=paste("Bearer",key))
}

# A POST to query_result is a read; only the two generation routes can create
# work. Never retry submissions, follow redirects with credentials, or log bodies.
.gen_sound_http <- function(service,base_url,method,route,body=NULL,budget,path=NULL) {
  headers <- .gen_sound_headers(service)
  url <- if (startsWith(route,"http")) route else paste0(base_url,route)
  generation <- method=="POST" && sub("[?].*$","",route) %in% c("/release_task","/v1/music","/v1/sound-generation")
  seconds <- budget()
  response <- tryCatch({
    args <- list(url=url,headers,httr::timeout(seconds),httr::config(followlocation=FALSE,maxfilesize=.gen_sound_limit))
    if (!is.null(path)) args <- c(args,list(httr::write_disk(path,overwrite=TRUE)))
    if (method=="POST") args <- c(args,list(body=body,encode="json"))
    do.call(if (method=="POST") httr::POST else httr::GET,args)
  },error=function(e) .gen_stop("sound_transport",stage=if (generation) "inference" else "download",
    retryable=!generation,outcome=if (generation) "unknown" else "not_started"))
  .gen_provider_check(response,stage=if (generation) "inference" else if (route=="/query_result") "poll" else if (route=="/v1/models") "management" else "download")
  response
}

.gen_sound_json <- function(response) {
  value <- tryCatch(httr::content(response,as="text",encoding="UTF-8"),error=function(e) NULL)
  value <- tryCatch(jsonlite::fromJSON(value,simplifyVector=FALSE),error=function(e) NULL)
  if (!is.list(value) || !identical(value$code,200L) || !is.null(value$error) || is.null(value$data))
    .gen_stop("sound_response",stage="inference",outcome="unknown")
  value$data
}

.gen_sound_generate <- function(a,budget,record) {
  receipt <- list(schema_version=1L,service=a$service,model=a$model,kind=a$kind,
    base_url=a$base_url,state="pending",duration_seconds=a$duration_seconds,format=a$format,
    poll_interval=a$poll_interval %||% 2)
  if (a$service=="acestep") {
    body <- list(prompt=a$prompt,lyrics=if (a$instrumental) "[Instrumental]" else a$lyrics,
      model=a$model,audio_duration=a$duration_seconds,audio_format=a$format,
      batch_size=1L,task_type="text2music",thinking=FALSE,sample_mode=FALSE,
      use_format=FALSE,use_cot_caption=FALSE,use_cot_language=FALSE,
      use_random_seed=is.null(a$seed))
    if (!is.null(a$seed)) body$seed <- a$seed
    submitted <- .gen_sound_json(.gen_sound_http(a$service,a$base_url,"POST","/release_task",body,budget))
    if (!.gen_scalar(submitted$task_id) || !nzchar(submitted$task_id)) .gen_stop("sound_response",stage="inference",outcome="unknown")
    receipt$task_id <- submitted$task_id
    record(receipt)
    return(receipt)
  }
  route <- if (a$kind=="sfx") "/v1/sound-generation" else "/v1/music"
  if (a$kind=="sfx") {
    body <- list(text=a$prompt,model_id=a$model,loop=a$loop,prompt_influence=a$prompt_influence)
    if (!is.null(a$duration_seconds)) body$duration_seconds <- a$duration_seconds
  } else {
    body <- list(model_id=a$model)
    ms <- as.integer(round(a$duration_seconds*1000))
    if (!is.null(a$lyrics)) {
      body$composition_plan <- if (a$model=="music_v1") list(positive_global_styles=list(a$prompt),negative_global_styles=list(),
        sections=list(list(section_name="Song",positive_local_styles=list(),negative_local_styles=list(),
          duration_ms=ms,lines=as.list(strsplit(a$lyrics,"\n",fixed=TRUE)[[1L]])))) else
        list(chunks=list(list(text=a$lyrics,duration_ms=ms,positive_styles=list(a$prompt),negative_styles=list())))
      if (!is.null(a$seed)) body$seed <- a$seed
    } else { body$prompt <- a$prompt; body$music_length_ms <- ms; body$force_instrumental <- a$instrumental }
  }
  # Format is a query parameter, not part of the provider JSON body.
  format <- switch(a$format,mp3="mp3_44100_128",wav="wav_44100",opus="opus_48000_128")
  path <- tempfile("genflow-sound-response-")
  on.exit(unlink(path),add=TRUE)
  response <- .gen_sound_http(a$service,paste0(a$base_url),"POST",paste0(route,"?output_format=",format),body,budget,path)
  receipt$state <- "generated"
  receipt$source <- .gen_sound_file_source(path)
  record(receipt)
  receipt
}

.gen_sound_poll <- function(receipt,budget,record) {
  repeat {
    data <- .gen_sound_json(.gen_sound_http(receipt$service,receipt$base_url,"POST","/query_result",
      list(task_id_list=list(receipt$task_id)),budget))
    if (!is.list(data)) .gen_stop("sound_response",stage="inference",outcome="unknown")
    rows <- Filter(function(x) is.list(x) && identical(x$task_id,receipt$task_id),data)
    if (length(rows)!=1L || !is.numeric(rows[[1L]]$status) || !rows[[1L]]$status %in% c(0,1,2))
      .gen_stop("sound_response",stage="inference",outcome="unknown")
    row <- rows[[1L]]
    if (row$status==2) .gen_stop("sound_task_failed",stage="inference",outcome="failed")
    if (row$status==1) {
      outputs <- tryCatch(jsonlite::fromJSON(row$result,simplifyVector=FALSE),error=function(e) NULL)
      if (length(outputs)!=1L || !is.list(outputs[[1L]]) || !.gen_scalar(outputs[[1L]]$file))
        .gen_stop("sound_response",stage="output_validation",outcome="succeeded")
      # Validate before retaining a locator: rejected URLs must not put
      # embedded credentials or arbitrary paths into a persisted receipt.
      url <- tryCatch(.gen_sound_download_url(outputs[[1L]]$file,receipt$base_url),
        error=function(e) .gen_stop(gen_diagnostic(e,outcome="succeeded")))
      receipt$source <- list(kind="url",value=url)
      receipt$state <- "generated"; record(receipt)
      return(receipt)
    }
    Sys.sleep(min(receipt$poll_interval,budget()))
  }
}

.gen_sound_download_url <- function(value,base_url) {
  if (!.gen_scalar(value) || !nzchar(value)) .gen_stop("sound_download_url",stage="download")
  if (startsWith(value,"/v1/audio?")) value <- paste0(base_url,value)
  url <- tryCatch(httr::parse_url(value),error=function(e) NULL)
  base <- httr::parse_url(base_url)
  origin <- function(x) paste(x$scheme,x$hostname,x$port %||% if (identical(x$scheme,"https")) 443 else 80,sep=":")
  if (is.null(url) || !identical(origin(url),origin(base)) || !identical(url$path,paste0(sub("/$","",base$path),if (nzchar(base$path)) "/" else "","v1/audio")) ||
      length(url$username) || length(url$password) || length(url$fragment) ||
      !identical(names(url$query),"path") || !.gen_scalar(url$query$path)) .gen_stop("sound_download_url",stage="download")
  value
}

.update_models_elevenlabs <- function(directory,verbose=TRUE) {
  models <- .gen_sound_models()
  rows <- do.call(rbind,lapply(names(models),function(kind) data.frame(service="elevenlabs",model=models[[kind]],
    type=if (kind=="sfx") "SFX" else "Music",pricing="",description="Documented adapter model; account availability is not verified.")))
  dir.create(directory,recursive=TRUE,showWarnings=FALSE)
  readr::write_csv(rows,file.path(directory,"elevenlabs.csv")); invisible(rows)
}

.update_models_acestep <- function(directory,verbose=TRUE) {
  base <- .gen_sound_base_url("acestep")
  data <- .gen_sound_json(.gen_sound_http("acestep",base,"GET","/v1/models",budget=.gen_sound_budget(10)))
  models <- vapply(data$models,function(x) if (.gen_scalar(x$name)) x$name else "",character(1))
  if (!length(models) || any(!nzchar(models)) || anyDuplicated(models)) .gen_stop("sound_response")
  rows <- data.frame(service="acestep",model=models,type="Music",pricing="local",description="Model loaded in the ACE-Step server.")
  dir.create(directory,recursive=TRUE,showWarnings=FALSE)
  readr::write_csv(rows,file.path(directory,"acestep.csv")); invisible(rows)
}
