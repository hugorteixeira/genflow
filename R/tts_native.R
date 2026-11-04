# Native TTS adapter: one CLI invocation, no server lifecycle or auto-download.
# Syntax/combination rules are also used by offline workflow validation.
.gen_tts_rules <- function(args,usable,add,version,providers=TRUE,bound=character()) {
  if (!providers || !usable("service")) return(invisible(NULL))
  local <- identical(args$service,"local-native")
  rule <- function(key,code="tts_native_parameter") add(key,gen_error_message(code),code)
  if (local && version<2L) { rule("service","tts_native_v2"); return(invisible(NULL)) }
  if (version<2L) return(invisible(NULL))
  reference <- "reference_audio" %in% bound || usable("reference_audio") && !is.null(args$reference_audio)
  if (!local) return(invisible(NULL))
  if (reference && usable("reference_consent") && !isTRUE(args$reference_consent)) rule("reference_consent","tts_reference_consent")
  if (identical(args$spoken_disclaimer,FALSE) && usable("marking_responsibility") && !isTRUE(args$marking_responsibility)) rule("marking_responsibility","tts_marking_required")
  if (usable("text")) {
    text <- args$text
    if (usable("add") && !is.null(args$add) && nzchar(args$add)) text <- paste(text,args$add)
    if (nchar(text,type="chars")>4096L) add("text",gen_error_message("too_long",list(limit=4096,unit="characters")),"too_long")
  }
  invisible(NULL)
}

.tts_native_budget <- function(seconds) {
  deadline <- Sys.time()+seconds
  function() {
    remaining <- as.numeric(difftime(deadline,Sys.time(),units="secs"))
    if (!is.finite(remaining) || remaining<=0) .gen_stop("tts_native_timeout",stage="inference",outcome="failed")
    remaining
  }
}

# processx owns the process tree on timeout and interruption; no shell parsing.
.tts_native_process <- function(command,args,budget,environment=NULL) {
  result <- tryCatch(processx::run(command,args,timeout=budget(),error_on_status=FALSE,
    cleanup_tree=TRUE,env=environment),error=function(e) {
      if (inherits(e,"genflow_error")) stop(e)
      if (inherits(e,"system_command_timeout_error")) .gen_stop("tts_native_timeout",stage="inference",outcome="failed")
      .gen_stop("tts_native_failed",stage="inference")
    })
  if (isTRUE(result$timeout)) .gen_stop("tts_native_timeout",stage="inference",outcome="failed")
  result
}

.tts_native_setting <- function(value,field,config,default="") {
  .stt_argument_or_local_setting(value,field,toupper(paste0("GENFLOW_",field)),default,config)
}

.tts_local_native <- function(text,model,reference_audio,format,speed,timeout,
                              executable,native_device,threads,seed,steps,
                              reference_consent,reference_identity,spoken_disclaimer,marking_responsibility,model_options=list()) {
  budget <- .tts_native_budget(timeout)
  .gen_sound_tools()
  config <- .genflow_read_local_config()
  if (is.null(model) || identical(model,"auto")) model <- .tts_native_setting(NULL,"tts_native_model",config)
  path <- .tts_native_model_path(model)
  executable <- .tts_native_executable(executable,config)
  device <- .tts_native_setting(native_device,"tts_native_device",config,"auto")
  if (!device %in% c("auto","cpu","vulkan","cuda","metal")) .gen_stop("tts_device_invalid",path="/args/native_device",stage="preflight")
  work <- tempfile("genflow-tts-"); dir.create(work,mode="0700")
  on.exit(unlink(work,recursive=TRUE),add=TRUE)
  reference <- NULL
  if (!is.null(reference_audio)) {
    ref <- path.expand(reference_audio)
    tryCatch(.gen_sound_inspect(ref,budget),error=function(e) {
      budget()
      if (identical(e$diagnostic$code,"tts_native_timeout")) stop(e)
      .gen_stop("tts_reference_invalid",path="/args/reference_audio",stage="preflight")
    })
    reference <- file.path(work,"reference.wav")
    decoded <- .tts_native_process("ffmpeg",c("-v","error","-xerror","-threads","1","-i",normalizePath(ref,mustWork=TRUE),
      "-map","0:a:0","-ac","1","-ar","16000","-c:a","pcm_s16le",reference),budget)
    if (decoded$status!=0L || !file.exists(reference)) .gen_stop("tts_reference_invalid",path="/args/reference_audio",stage="preflight")
  }
  engine_build <- .tts_native_probe(executable,budget)
  output <- tempfile("genflow-native-voice-",fileext=paste0(".",format))
  keep_output <- FALSE
  candidates <- c(output,if (format %in% c("opus","aac")) paste0(tools::file_path_sans_ext(output),c(".mp4",".m4a")))
  on.exit(unlink(if (keep_output) setdiff(candidates,output) else candidates),add=TRUE)
  args <- c("--backend","voxcpm2-tts","-m",path,"--threads",as.character(threads %||% 4L),
    "--seed",as.character(seed %||% 0L),"--tts-steps",as.character(steps %||% 10L),
    if (device=="cpu") "--no-gpu" else if (device!="auto") c("--gpu-backend",device),
    "--speaker-identity",reference_identity,"--tts",text,"--tts-output",output)
  if (!is.null(reference)) args <- c(args,"--voice",reference,"--i-have-rights")
  if (!spoken_disclaimer) args <- c(args,"--no-spoken-disclaimer","--accept-marking-responsibility")
  # The CLI CFG flag is ignored by this backend. Set the validated value
  # only in the child process, retaining the historical default when omitted.
  environment <- Sys.getenv()
  environment <- environment[setdiff(names(environment),"CRISPASR_VOXCPM2_MAX_LEN")]
  environment["CRISPASR_VOXCPM2_CFG_VALUE"] <- as.character(model_options$cfg %||% 2)
  generated <- .tts_native_process(executable,args,budget,environment)
  log <- paste(generated$stdout,generated$stderr)
  fail <- function(code,stage="output_validation",path=NULL) {
    diagnostic <- gen_diagnostic(code,stage=stage,path=path,outcome="failed")
    # Retain a bounded, redacted engine diagnostic, never the prompt in the UI.
    diagnostic <- .gen_log_diagnostic(diagnostic,simpleError(substr(log,max(1L,nchar(log)-1999L),nchar(log))))
    .gen_stop(diagnostic)
  }
  if (generated$status!=0L) fail("tts_native_failed",stage="inference")
  if (!is.null(reference)) {
    lines <- trimws(strsplit(generated$stderr,"\n",fixed=TRUE)[[1L]])
    loaded <- any(startsWith(lines,paste0("crispasr[voxcpm2-tts]: loaded reference audio '",reference,"' (")))
    fallback <- any(startsWith(lines,"crispasr[voxcpm2-tts]: failed to load reference audio"))
    if (!loaded || fallback) fail("tts_reference_ignored",path="/args/reference_audio")
  }
  # CrispASR may remux Opus/AAC into MP4 to retain content credentials.
  # Resolve only known siblings, then inspect bytes and codec; never trust logs
  # claiming that the originally requested path was written.
  available <- candidates[file.exists(candidates) & !dir.exists(candidates)]
  if (length(available)!=1L) fail("tts_native_audio_invalid")
  output <- available[[1L]]
  metadata <- tryCatch(.gen_sound_inspect(output,budget),error=function(e) {
    budget()
    if (identical(e$diagnostic$code,"tts_native_timeout")) stop(e)
    fail("tts_native_audio_invalid")
  })
  remux <- format %in% c("opus","aac") && identical(metadata$format,"m4a") && identical(metadata$codec,format)
  if (!identical(metadata$format,format) && !remux) fail("tts_native_audio_invalid")
  result <- .tts_tag_audio_path(output,metadata$mime,metadata$format)
  attr(result,"tts_native") <- list(engine="crispasr",backend="voxcpm2-tts",executable=executable,build=engine_build,model=path,
    device_requested=device,device=.stt_crispasr_runtime_device(log,device),
    threads=threads %||% 4L,seed=seed %||% 0L,steps=steps %||% 10L,
    reference_used=!is.null(reference),cfg=model_options$cfg %||% 2,audio=metadata)
  keep_output <- TRUE
  result
}

.tts_native_model_path <- function(model) {
  path <- path.expand(model)
  if (identical(model,basename(model))) path <- tryCatch(.genflow_crispasr_managed_model(model),error=function(e) "")
  if (!nzchar(path) || !identical(.gen_native_architecture(path),"voxcpm2"))
    .gen_stop("tts_model_invalid",path="/args/model",stage="preflight")
  path <- normalizePath(path,mustWork=TRUE)
  path
}

.tts_native_executable <- function(executable,config) {
  executable <- .tts_native_setting(executable,"tts_native_executable",config)
  if (!nzchar(executable)) executable <- config$stt_native_crispasr_executable %||% ""
  executable <- .genflow_resolve_executable(executable,if (!nzchar(executable)) "crispasr" else character())
  if (!nzchar(executable) || file.access(executable,1L)!=0L) .gen_stop("tts_engine_missing",path="/args/executable",stage="preflight")
  executable
}

.tts_native_probe <- function(executable,budget) {
  capabilities <- .tts_native_process(executable,"--list-backends-json",budget)
  backends <- if (capabilities$status==0L) tryCatch(jsonlite::fromJSON(capabilities$stdout,simplifyVector=FALSE)$backends,error=function(e) NULL) else NULL
  supported <- any(vapply(backends,function(b) is.list(b) && identical(b$name,"voxcpm2-tts") && all(c("tts","voice-cloning") %in% unlist(b$caps)),logical(1)))
  if (!supported) .gen_stop("tts_engine_unsupported",path="/args/executable",stage="preflight")
  build <- .tts_native_process(executable,"--version",budget)
  lines <- strsplit(paste(build$stdout,build$stderr),"\n",fixed=TRUE)[[1L]]
  paste(trimws(lines[grepl("version\\s*:|git sha\\s*:",lines,perl=TRUE)]),collapse="; ")
}

.genflow_native_tts_diagnostics <- function(config,timeout) {
  if (!nzchar(config$tts_native_model)) return(.genflow_diagnostic_row("Native TTS","info","No default TTS model configured; pass model explicitly or set tts_native_model."))
  tryCatch({
    model <- .tts_native_model_path(config$tts_native_model)
    executable <- .tts_native_executable(NULL,config)
    build <- .tts_native_probe(executable,.tts_native_budget(timeout))
    .genflow_diagnostic_row("Native TTS","ok",paste("VoxCPM2:",model,"|",executable,"|",build))
  },error=function(e) .genflow_diagnostic_row("Native TTS","error",gen_error_summary(gen_diagnostic(e))))
}
