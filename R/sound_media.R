.gen_sound_limit <- 150 * 1024^2

.gen_sound_budget <- function(seconds, clock=Sys.time) {
  deadline <- clock() + seconds
  function() {
    remaining <- as.numeric(difftime(deadline,clock(),units="secs"))
    if (!is.finite(remaining) || remaining <= 0) .gen_stop("sound_deadline",stage="inference",outcome="unknown")
    remaining
  }
}

.gen_sound_tools <- function() {
  if (any(!nzchar(Sys.which(c("ffmpeg","ffprobe"))))) .gen_stop("sound_tools_missing",stage="preflight")
}

.gen_sound_inspect <- function(path,budget) {
  info <- file.info(path)
  if (nrow(info)!=1L || is.na(info$size) || info$isdir || info$size<=0 || info$size>.gen_sound_limit)
    .gen_stop("sound_invalid",stage="output_validation",outcome="succeeded")
  format <- .gen_audio_magic_format(path)
  if (is.null(format)) .gen_stop("sound_invalid",stage="output_validation",outcome="succeeded")
  probe <- tryCatch(processx::run("ffprobe",c("-v","error","-show_entries",
    "format=duration:stream=codec_type,codec_name,sample_rate,channels","-of","json",path),
    timeout=min(30,budget()),error_on_status=FALSE),error=function(e) NULL)
  data <- if (!is.null(probe) && probe$status==0L) tryCatch(jsonlite::fromJSON(probe$stdout,simplifyVector=FALSE),error=function(e) NULL) else NULL
  streams <- data$streams; duration <- suppressWarnings(as.numeric(data$format$duration))
  if (length(streams)!=1L || !identical(streams[[1L]]$codec_type,"audio") ||
      length(duration)!=1L || !is.finite(duration) || duration<=0 || duration>660)
    .gen_stop("sound_invalid",stage="output_validation",outcome="succeeded")
  decoded <- tryCatch(processx::run("ffmpeg",c("-v","error","-xerror","-threads","1","-i",path,
    "-map","0:a:0","-f","null","-"),timeout=min(30,budget()),error_on_status=FALSE),error=function(e) NULL)
  if (is.null(decoded) || decoded$status!=0L || nzchar(decoded$stderr))
    .gen_stop("sound_invalid",stage="output_validation",outcome="succeeded")
  list(format=format,mime=.gen_audio_format_content_type(format),duration_seconds=duration,
    sample_rate=as.integer(streams[[1L]]$sample_rate),channels=streams[[1L]]$channels,codec=streams[[1L]]$codec_name,
    bytes=unname(info$size))
}

.gen_sound_source_file <- function(source,receipt,budget) {
  path <- tempfile("genflow-audio-")
  if (identical(source$kind,"base64")) {
    b64 <- source$value
    if (!.gen_scalar(b64) || nchar(b64,type="bytes")>4*ceiling(.gen_sound_limit/3) ||
        nchar(b64)%%4!=0 || !grepl("^[A-Za-z0-9+/]*={0,2}$",b64)) .gen_stop("sound_invalid")
    bytes <- tryCatch(openssl::base64_decode(b64),error=function(e) .gen_stop("sound_invalid"))
    writeBin(bytes,path)
  } else if (identical(source$kind,"url") && receipt$service=="acestep") {
    url <- .gen_sound_download_url(source$value,receipt$base_url)
    .gen_sound_http(receipt$service,receipt$base_url,"GET",url,budget=budget,path=path)
  } else .gen_stop("sound_receipt_invalid")
  path
}

.gen_sound_file_source <- function(path) {
  size <- file.info(path)$size
  if (length(size)!=1L || is.na(size) || size<=0 || size>.gen_sound_limit) .gen_stop("sound_invalid")
  list(kind="base64",value=openssl::base64_encode(readBin(path,"raw",n=.gen_sound_limit+1)))
}

# Recovery only reads an existing receipt. It never invokes a generation route.
#' Recover an existing music or sound-effect result
#' @param result Original result containing an `audio_receipt`.
#' @param directory Optional output directory.
#' @param persist Publish to the configured output directory instead of a temporary file.
#' @param timeout Total recovery budget in seconds.
#' @return A common Genflow result. No new generation is submitted.
#' @export
gen_audio_recover <- function(result,directory=NULL,persist=TRUE,timeout=600) {
  withCallingHandlers({
    if (!is.list(result)) .gen_stop("sound_receipt_invalid")
    r <- result$audio_receipt
    text <- list(type="string",pattern="\\S")
    schema <- list(type="object",required=as.list(c("schema_version","service","kind","model","base_url","state","poll_interval")),
      properties=list(schema_version=list(type="integer",enum=list(1L)),service=list(type="string",enum=list("elevenlabs","acestep")),
        kind=list(type="string",enum=list("music","sfx")),model=text,base_url=text,
        state=list(type="string",enum=list("pending","generated")),task_id=text,
        poll_interval=list(type="number",exclusiveMinimum=0,maximum=30),source=list(type="object",required=list("kind","value"),
          properties=list(kind=list(type="string",enum=list("base64","url")),value=text))))
    if (!gen_validate_schema(r,schema)$valid || !.gen_sound_url_valid(r$base_url) ||
        (r$state=="pending" && (r$service!="acestep" || !.gen_scalar(r$task_id))) ||
        (r$state=="generated" && !is.list(r$source))) .gen_stop("sound_receipt_invalid")
    check <- gen_validate_schema(list(persist=persist,timeout=timeout,directory=directory),list(type="object",properties=list(
      persist=list(type="boolean"),timeout=list(type="number",exclusiveMinimum=0),directory=list(type=c("string","null"),pattern="\\S"))))
    if (!check$valid) .gen_validation_stop(check$errors)
    .gen_sound_tools()
    result$recovery_duration <- NULL
    elapsed <- result$duration
    recovered <- .gen_sound_run(NULL,result,directory,persist,.gen_sound_budget(timeout),receipt=r)
    recovered$recovery_duration <- recovered$duration; recovered$duration <- elapsed
    invisible(recovered)
  },error=.gen_boundary_error)
}

.gen_sound_run <- function(args,result,directory,persist,budget,receipt=NULL) {
  started <- Sys.time(); failure <- NULL; path <- NULL; metadata <- NULL; saved <- NULL
  record <- function(value) { receipt <<- value; invisible(value) }
  tryCatch({
    if (is.null(receipt)) receipt <- .gen_sound_generate(args,budget,record)
    if (receipt$state=="pending") receipt <- .gen_sound_poll(receipt,budget,record)
    path <- .gen_sound_source_file(receipt$source,receipt,budget)
    # Keep actual bytes before decoding/publication, including an invalid payload.
    receipt$source <- .gen_sound_file_source(path)
    metadata <- .gen_sound_inspect(path,budget)
    saved <- tryCatch({
      target <- .genflow_media_directory(directory,"audio",persist)
      .genflow_copy_artifact(path,target,result$label,receipt$service,receipt$model,metadata$format)
    },error=function(e) .gen_stop("PERSISTENCE_ERROR",stage="persistence",outcome="succeeded",retryable=TRUE))
    unlink(path); path <- saved
  },error=function(e) failure <<- e)
  # A definitive provider failure permits a new attempt. A timeout does not.
  if (identical(failure$diagnostic$code,"sound_task_failed")) receipt <- NULL
  inferred <- is.null(failure) || identical(receipt$state,"generated") || identical(failure$diagnostic$outcome,"succeeded")
  error <- if (is.null(failure)) NULL else gen_diagnostic(failure,
    outcome=if (inferred) "succeeded" else if (!is.null(receipt) && !identical(failure$diagnostic$outcome,"failed")) "unknown" else NULL)
  result$status_api <- if (is.null(failure)) "SUCCESS" else "ERROR"
  result$status_msg <- if (is.null(failure)) "OK" else gen_error_summary(error)
  result$response_value <- path
  result$saved_file <- if (is.null(failure) && persist) saved else NA_character_
  result$duration <- as.numeric(difftime(Sys.time(),started,units="secs"))
  result$audio <- if (is.null(metadata)) NULL else c(metadata,list(requested_duration_seconds=receipt$duration_seconds,requested_format=receipt$format))
  result$audio_receipt <- if (is.null(failure)) NULL else receipt
  .genflow_complete_result(result,inference_status=if (inferred) "SUCCESS" else "ERROR",
    persistence=.genflow_persistence(path,requested=persist,
      error=if (identical(error$stage,"persistence")) failure else NULL,
      temporary=!is.null(failure) || !persist),error=error)
}
