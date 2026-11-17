# Qualified endpoints only. A Video catalog row alone is not a transport contract.
.gen_video_profile <- function(service, model) {
  if (service == 'fal' && model %in% c('fal-ai/kling-video/ai-avatar/v2/standard','fal-ai/kling-video/ai-avatar/v2/pro'))
    return(list(image='image_url',audio='audio_url',max_seconds=300))
  if (service == 'replicate' && model == 'wan-video/wan-2.2-s2v')
    return(list(image='image',audio='audio',max_seconds=60))
  NULL
}
.gen_video_check_receipt <- function(receipt) {
  if (!is.list(receipt) || !is.numeric(receipt$schema_version) || length(receipt$schema_version)!=1L || is.na(receipt$schema_version) || receipt$schema_version!=1L ||
      !.gen_scalar(receipt$id) || !grepl('^[A-Za-z0-9_-]{1,200}$',receipt$id) ||
      !.gen_scalar(receipt$service) || !.gen_scalar(receipt$model) ||
      is.null(.gen_video_profile(receipt$service,receipt$model))) stop('Invalid video receipt.',call.=FALSE)
  receipt
}
.gen_video_url <- function(receipt, status=FALSE) {
  r <- .gen_video_check_receipt(receipt)
  if (r$service=='replicate') return(paste0('https://api.replicate.com/v1/predictions/',r$id))
  # Fal queue lifecycle addresses use the application id, not its subpath.
  app <- paste(head(strsplit(r$model,'/',fixed=TRUE)[[1]],2),collapse='/')
  paste0('https://queue.fal.run/',app,'/requests/',r$id,if(status) '/status' else '')
}
.gen_video_http <- function(service, method, url, body=NULL, timeout=60, file=NULL, mime=NULL) {
  host <- if(service=='fal') 'queue.fal.run' else 'api.replicate.com'
  parsed <- httr::parse_url(url)
  if (!identical(parsed$scheme,'https') || !identical(parsed$hostname,host) || length(parsed$port) || length(parsed$username) || length(parsed$password)) stop('Invalid provider URL.')
  token <- if(service=='fal') .genflow_first_env(c('FAL_API_KEY','FAL_KEY')) else Sys.getenv('REPLICATE_API_TOKEN')
  if (!nzchar(token)) .gen_stop('missing_credential',params=list(credential=if(service=='fal') 'FAL_API_KEY' else 'REPLICATE_API_TOKEN'),stage='preflight')
  headers <- httr::add_headers(Authorization=paste(if(service=='fal') 'Key' else 'Bearer',token))
  args <- list(url=url,headers,httr::timeout(timeout),httr::config(followlocation=0))
  if(method=='POST') {
    args$body <- if(is.null(file)) body else list(content=httr::upload_file(file,type=mime))
    args$encode <- if(is.null(file)) 'json' else 'multipart'
  }
  response <- do.call(if(method=='POST') httr::POST else httr::GET,args)
  .gen_provider_check(response,expected=if(method=='POST') c(200L,201L,202L) else 200L,stage=if(method=='POST') 'submit' else 'poll')
  .gen_provider_body(response)
}
.gen_video_probe <- function(path, kind, timeout=30) {
  info <- file.info(path)
  limit <- if(kind=='image') 10*1024^2 else if(kind=='audio') 100*1024^2 else 512*1024^2
  if (!.gen_scalar(path) || nrow(info)!=1L || is.na(info$size) || info$isdir || info$size<=0 || info$size>limit) stop('Invalid or oversized media file.')
  probe <- processx::run('ffprobe',c('-v','error','-show_streams','-show_format','-of','json',normalizePath(path)),timeout=timeout,error_on_status=FALSE)
  if(probe$status!=0L) stop('Media could not be inspected.')
  data <- jsonlite::fromJSON(probe$stdout,simplifyVector=FALSE)
  streams <- Filter(function(x) identical(x$codec_type,if(kind=='audio') 'audio' else 'video'),data$streams)
  if(!length(streams)) stop('Media stream is missing.')
  duration <- suppressWarnings(as.numeric(data$format$duration))
  if(kind!='image' && (length(duration)!=1L || !is.finite(duration) || duration<=0)) stop('Invalid media duration.')
  if(kind=='image') mime <- .gen_image_inspect(.gen_image_read_file(path),timeout)$metadata$mime else if(kind=='audio') {
    fmt <- .gen_audio_magic_format(path)
    if(is.null(fmt)) stop('Unsupported audio format.')
    mime <- .gen_audio_format_content_type(fmt)
  } else mime <- 'video/mp4'
  list(duration_seconds=if(length(duration)) duration else NULL,width=streams[[1]]$width,height=streams[[1]]$height,mime=mime,bytes=unname(info$size),has_audio=any(vapply(data$streams,function(s) identical(s$codec_type,'audio'),logical(1))))
}
.gen_video_upload <- function(path, meta, service, timeout, request=.gen_video_http) {
  if(service=='replicate') {
    uploaded <- request(service,'POST','https://api.replicate.com/v1/files',timeout=timeout,file=path,mime=meta$mime)
    url <- uploaded$urls$get
    if(!.gen_scalar(url) || !startsWith(url,'https://api.replicate.com/v1/files/')) stop('Invalid upload receipt.')
    return(url)
  }
  paste0('data:',meta$mime,';base64,',openssl::base64_encode(readBin(path,'raw',n=meta$bytes)))
}

#' Generate video from an approved scene image and custom audio
#' @param prompt Movement description, or a genflow_agent.
#' @param image Local scene image. Exactly one composed frame is required.
#' @param audio Local speech audio, including custom TTS output.
#' @param service fal or replicate.
#' @param model Qualified endpoint returned by the video contract.
#' @param add Additional textual instructions.
#' @param directory Output directory.
#' @param label Output filename label.
#' @param timeout_api Total time budget in seconds.
#' @param poll_interval Poll interval in seconds.
#' @param persist Save the output in the requested directory.
#' @param ... Named agent overrides.
#' @return A common Genflow result. A video_receipt permits recovery without submission.
#' @export
gen_video <- function(prompt, ...) { if(missing(prompt)) return(gen_video.default(...)); UseMethod('gen_video') }
#' @rdname gen_video
#' @export
gen_video.default <- function(prompt,image,audio,service='fal',model=NULL,add=NULL,directory=NULL,label=NULL,timeout_api=1800,poll_interval=3,persist=TRUE,...) {
  withCallingHandlers({
    .gen_internal_validate('gen_video',version=1L)
    started <- Sys.time(); receipt <- NULL; completed <- FALSE
    result <- tryCatch({
      receipt <- gen_video_submit(prompt,image,audio,service,model,add,timeout=min(120,timeout_api))
      repeat {
        left <- timeout_api-as.numeric(difftime(Sys.time(),started,units='secs'))
        if(left<=0) stop('Video deadline reached; resume using the retained receipt.')
        polled <- gen_video_poll(receipt,timeout=min(60,left)); receipt <- polled$receipt
        if(polled$state=='failed') .gen_stop('provider_task_failed',stage='poll',outcome='failed')
        if(polled$state=='ready') { completed <- TRUE; break }
        Sys.sleep(min(poll_interval,left))
      }
      gen_video_collect(receipt,directory,label,persist,timeout=max(1,timeout_api-as.numeric(difftime(Sys.time(),started,units='secs'))),audio=audio)
    },error=function(e) .genflow_complete_result(list(status_api='ERROR',status_msg=conditionMessage(e),response_value=NULL,service=service,model=model,video_receipt=receipt),inference_status=if(completed) 'SUCCESS' else 'ERROR',error=.genflow_provider_failure(e)$error))
    result$duration <- as.numeric(difftime(Sys.time(),started,units='secs')); result
  },error=.gen_boundary_error)
}
#' @rdname gen_video
#' @export
gen_video.genflow_agent <- function(prompt,...) {
  a <- .genflow_normalize_agent_fields(prompt); a$prompt <- a$prompt %||% a$context %||% a$instructions
  do.call(gen_video.default,.genflow_prepare_agent_args(a,list(...),formals(gen_video.default),required='prompt'),quote=TRUE)
}

#' Submit one video task; never retry generation automatically
#' @inheritParams gen_video
#' @param timeout Submission time budget in seconds.
#' @return A portable receipt without credentials or input media.
#' @export
gen_video_submit <- function(prompt,image,audio,service='fal',model=NULL,add=NULL,timeout=120) {
  .gen_video_submit(prompt,image,audio,service,model,add,timeout)
}
.gen_video_submit <- function(prompt,image,audio,service,model,add=NULL,timeout=120,request=.gen_video_http) {
  checked <- gen_validate_call('gen_video',list(prompt=prompt,image=image,audio=audio,service=service,model=model,add=add),1L)
  if(!checked$valid) .gen_validation_stop(checked$errors,'gen_video')
  service <- checked$args$service; model <- checked$args$model
  .gen_sound_tools(); budget <- .gen_image_budget(timeout)
  im <- .gen_video_probe(image,'image',budget()); au <- .gen_video_probe(audio,'audio',budget())
  profile <- .gen_video_profile(service,model)
  if(au$duration_seconds>profile$max_seconds) stop('Audio exceeds the qualified clip duration; split the scene explicitly.')
  input <- list(prompt=.genflow_prepare_prompt(prompt,add))
  input[[profile$image]] <- .gen_video_upload(image,im,service,budget(),request)
  input[[profile$audio]] <- .gen_video_upload(audio,au,service,budget(),request)
  body <- if(service=='fal') input else list(input=input)
  url <- if(service=='fal') paste0('https://queue.fal.run/',model) else paste0('https://api.replicate.com/v1/models/',model,'/predictions')
  reply <- request(service,'POST',url,body=body,timeout=budget())
  id <- if(service=='fal') reply$request_id else reply$id
  .gen_video_check_receipt(list(schema_version=1L,service=service,model=model,id=id,audio_seconds=au$duration_seconds,audio_sha256=.genflow_persistence(audio,FALSE)$sha256))
}

#' Poll an existing video task without submitting another
#' @param receipt Receipt from gen_video_submit or gen_video_poll.
#' @param timeout HTTP time budget in seconds.
#' @return state (pending, ready, failed) and updated receipt.
#' @export
gen_video_poll <- function(receipt,timeout=60) .gen_video_poll(receipt,timeout)
.gen_video_poll <- function(receipt,timeout=60,request=.gen_video_http) {
  r <- .gen_video_check_receipt(receipt)
  body <- request(r$service,'GET',.gen_video_url(r,TRUE),timeout=timeout)
  status <- body$status
  pending <- if(r$service=='fal') c('IN_QUEUE','IN_PROGRESS') else c('starting','processing')
  ready <- if(r$service=='fal') 'COMPLETED' else 'succeeded'
  failed <- if(r$service=='fal') c('FAILED','CANCELLED','CANCELED') else c('failed','canceled')
  if(!.gen_scalar(status) || !status %in% c(pending,ready,failed)) stop('Invalid provider task status.')
  list(state=if(status %in% failed || !is.null(body$error)) 'failed' else if(status==ready) 'ready' else 'pending',receipt=r)
}

#' Download and validate an existing video; never submit generation
#' @inheritParams gen_video
#' @param receipt Receipt for the completed video task.
#' @param timeout Download and validation time budget.
#' @param audio Optional original audio path. When supplied, its hash must match
#'   the submission receipt; the final MP4 uses this audio instead of the provider track.
#' @return Common Genflow result with measured video metadata.
#' @export
gen_video_collect <- function(receipt,directory=NULL,label=NULL,persist=TRUE,timeout=300,audio=NULL) .gen_video_collect(receipt,directory,label,persist,timeout,audio=audio)
.gen_video_collect <- function(receipt,directory=NULL,label=NULL,persist=TRUE,timeout=300,request=.gen_video_http,download=.gen_video_download,audio=NULL) {
  r <- .gen_video_check_receipt(receipt); budget <- .gen_image_budget(timeout); retained <- NULL
  body <- request(r$service,'GET',.gen_video_url(r),timeout=budget())
  if(r$service=='replicate' && !identical(body$status,'succeeded')) stop('Video is not ready.')
  url <- if(r$service=='fal') body$video$url else body$output
  if(is.list(url) && length(url)==1L) url <- url[[1L]]
  path <- download(url,r$service,budget()); downloaded <- path; on.exit(if(!identical(retained,downloaded)) unlink(downloaded),add=TRUE)
  meta <- .gen_video_probe(path,'video',budget())
  if(!meta$has_audio && is.null(audio)) stop('The returned video has no custom audio stream.')
  if(!is.null(r$audio_seconds) && abs(meta$duration_seconds-r$audio_seconds)>max(1,r$audio_seconds*.1)) stop('Returned video duration does not match the supplied speech; output was not accepted.')
  decoded <- processx::run('ffmpeg',c('-v','error','-xerror','-threads','1','-i',path,'-map','0:v:0','-map','0:a:0?','-f','null','-'),timeout=budget(),error_on_status=FALSE)
  if(decoded$status!=0L || nzchar(decoded$stderr)) stop('Generated video could not be fully decoded.')
  if(!is.null(audio)) {
    .gen_video_probe(audio,'audio',budget())
    if(is.null(r$audio_sha256) || !identical(.genflow_persistence(audio,FALSE)$sha256,r$audio_sha256)) stop('Original audio does not match the submitted audio.')
    remux<-tempfile(fileext='.mp4');on.exit(if(!identical(retained,remux)) unlink(remux),add=TRUE)
    .gen_video_ffmpeg(c('-i',path,'-i',normalizePath(audio),'-map','0:v:0','-map','1:a:0','-c:v','copy','-c:a','aac','-b:a','192k','-shortest','-movflags','+faststart',remux),budget)
    path<-remux;meta<-.gen_video_probe(path,'video',budget());meta$original_audio_preserved<-TRUE
  }
  dir <- .genflow_media_directory(directory,'videos',persist)
  target <- tryCatch(.genflow_copy_artifact(path,dir,label,r$service,r$model,'mp4'),error=identity)
  if(inherits(target,'error')) {
    retained <- path
    return(.genflow_complete_result(list(status_api='SUCCESS',status_msg='OK',response_value=path,service=r$service,model=r$model,video=meta,video_receipt=r),inference_status='SUCCESS',persistence=.genflow_persistence(error=target)))
  }
  .genflow_complete_result(list(status_api='SUCCESS',status_msg='OK',response_value=target,service=r$service,model=r$model,video=meta,video_receipt=r),inference_status='SUCCESS',persistence=.genflow_persistence(target,requested=persist))
}
.gen_video_download <- function(url,service,timeout) {
  u <- if(.gen_scalar(url)) httr::parse_url(url) else NULL
  hosts <- if(service=='fal') c('fal.media') else c('replicate.delivery')
  valid <- !is.null(u) && identical(u$scheme,'https') && !length(u$username) && !length(u$password) && !length(u$port) &&
    any(vapply(hosts,function(h) identical(u$hostname,h) || endsWith(u$hostname,paste0('.',h)),logical(1)))
  if(!valid) stop('Video URL is outside the provider delivery hosts.')
  path <- tempfile('genflow-video-',fileext='.mp4'); success <- FALSE
  on.exit(if(!success) unlink(path),add=TRUE)
  response <- httr::GET(url,httr::write_disk(path),httr::timeout(timeout),httr::config(followlocation=0,maxfilesize=512*1024^2))
  .gen_provider_check(response,expected=200L,stage='download'); success <- TRUE; path
}
