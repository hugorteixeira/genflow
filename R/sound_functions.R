#' Generate sound effects with a selected provider
#'
#' One generation request; provider limits are checked before submission.
#' Audio is inspected and decoded before publication. The requested duration
#' is a target; measured duration is returned in `audio$duration_seconds`.
#' `duration` retains the common meaning of elapsed execution time.
#' @param prompt Description of the desired sound, or a genflow_agent.
#' @param add Optional text appended to the prompt.
#' @param directory Output directory; defaults to the configured audio directory.
#' @param label Optional filename label.
#' @param service Provider: elevenlabs.
#' @param model Provider model ID. Defaults come from the versioned contract.
#' @param duration_seconds Requested duration in seconds. NULL for SFX lets the provider choose.
#' @param format Requested audio container/codec. Actual format is detected from bytes.
#' @param loop Request a seamless looping effect.
#' @param prompt_influence Description adherence from zero to one.
#' @param base_url Optional provider API base URL; otherwise ELEVENLABS_BASE_URL or ACESTEP_BASE_URL. Credentials come only from environment variables.
#' @param timeout_api Total budget for submission, polling, download and audio validation.
#' @param persist Publish and record statistics. FALSE returns a temporary audio file.
#' @param ... Named agent overrides or reserved method arguments.
#' @return A common Genflow result with audio metadata. On recoverable failure,
#'   `audio_receipt` can be passed with the result to [gen_audio_recover()].
#' @export
gen_sfx <- function(prompt, ...) {
  if (missing(prompt)) return(gen_sfx.default(...))
  UseMethod("gen_sfx")
}

#' @rdname gen_sfx
#' @method gen_sfx default
#' @export
gen_sfx.default <- function(
  prompt,
  add = .gen_default("gen_sfx", "add"),
  directory = .gen_default("gen_sfx", "directory"),
  label = .gen_default("gen_sfx", "label"),
  service = .gen_default("gen_sfx", "service"),
  model = .gen_default("gen_sfx", "model"),
  duration_seconds = .gen_default("gen_sfx", "duration_seconds"),
  format = .gen_default("gen_sfx", "format"),
  loop = .gen_default("gen_sfx", "loop"),
  prompt_influence = .gen_default("gen_sfx", "prompt_influence"),
  base_url = .gen_default("gen_sfx", "base_url"),
  timeout_api = .gen_default("gen_sfx", "timeout_api"),
  persist = .gen_default("gen_sfx", "persist"),
  ...
) {
  withCallingHandlers({
    .gen_internal_validate("gen_sfx")
    args <- mget(names(gen_block_contracts("gen_sfx",1L)$arguments$properties),envir=environment())
    invisible(.gen_sound_call("sfx",args))
  },error=.gen_boundary_error)
}

#' @rdname gen_sfx
#' @method gen_sfx genflow_agent
#' @export
gen_sfx.genflow_agent <- function(prompt, ...) {
  agent <- .genflow_normalize_agent_fields(prompt)
  agent$prompt <- agent$prompt %||% agent$context %||% agent$instructions
  args <- .genflow_prepare_agent_args(agent,list(...),formals(gen_sfx.default),required="prompt",
    override_aliases=c(prompt_override="prompt"))
  do.call(gen_sfx.default,args,quote=TRUE)
}

#' Generate music with a selected provider
#'
#' One generation request; provider limits are checked before submission.
#' Audio is inspected and decoded before publication. The requested duration
#' is a target; measured duration is returned in `audio$duration_seconds`.
#' `duration` retains the common meaning of elapsed execution time.
#' @param prompt Description of the desired sound, or a genflow_agent.
#' @param add Optional text appended to the prompt.
#' @param directory Output directory; defaults to the configured audio directory.
#' @param label Optional filename label.
#' @param service Provider: elevenlabs or acestep (ACE-Step 1.5 REST).
#' @param model Provider model ID. Defaults come from the versioned contract.
#' @param duration_seconds Requested duration in seconds. NULL for SFX lets the provider choose.
#' @param format Requested audio container/codec. Actual format is detected from bytes.
#' @param instrumental Generate without vocals. FALSE requires explicit lyrics.
#' @param lyrics Exact lyrics; must be NULL for instrumental music. ElevenLabs uses one section, at most 120 seconds, 30 lines and 200 characters per line.
#' @param seed Optional integer seed. ElevenLabs accepts it only with explicit lyrics.
#' @param poll_interval Seconds between ACE-Step status queries.
#' @param base_url Optional provider API base URL; otherwise ELEVENLABS_BASE_URL or ACESTEP_BASE_URL. Credentials come only from environment variables.
#' @param timeout_api Total budget for submission, polling, download and audio validation.
#' @param persist Publish and record statistics. FALSE returns a temporary audio file.
#' @param ... Named agent overrides or reserved method arguments.
#' @return A common Genflow result with audio metadata. On recoverable failure,
#'   `audio_receipt` can be passed with the result to [gen_audio_recover()].
#' @export
gen_music <- function(prompt, ...) {
  if (missing(prompt)) return(gen_music.default(...))
  UseMethod("gen_music")
}

#' @rdname gen_music
#' @method gen_music default
#' @export
gen_music.default <- function(
  prompt,
  add = .gen_default("gen_music", "add"),
  directory = .gen_default("gen_music", "directory"),
  label = .gen_default("gen_music", "label"),
  service = .gen_default("gen_music", "service"),
  model = .gen_default("gen_music", "model"),
  duration_seconds = .gen_default("gen_music", "duration_seconds"),
  format = .gen_default("gen_music", "format"),
  instrumental = .gen_default("gen_music", "instrumental"),
  lyrics = .gen_default("gen_music", "lyrics"),
  seed = .gen_default("gen_music", "seed"),
  poll_interval = .gen_default("gen_music", "poll_interval"),
  base_url = .gen_default("gen_music", "base_url"),
  timeout_api = .gen_default("gen_music", "timeout_api"),
  persist = .gen_default("gen_music", "persist"),
  ...
) {
  withCallingHandlers({
    .gen_internal_validate("gen_music")
    args <- mget(names(gen_block_contracts("gen_music",1L)$arguments$properties),envir=environment())
    invisible(.gen_sound_call("music",args))
  },error=.gen_boundary_error)
}

#' @rdname gen_music
#' @method gen_music genflow_agent
#' @export
gen_music.genflow_agent <- function(prompt, ...) {
  agent <- .genflow_normalize_agent_fields(prompt)
  agent$prompt <- agent$prompt %||% agent$context %||% agent$instructions
  args <- .genflow_prepare_agent_args(agent,list(...),formals(gen_music.default),required="prompt",
    override_aliases=c(prompt_override="prompt"))
  do.call(gen_music.default,args,quote=TRUE)
}

.gen_sound_call <- function(kind,args) {
  args$prompt <- .genflow_prepare_prompt(args$prompt,args$add)
  args$add <- NULL
  # The complete assembled prompt must meet the same provider limits.
  checked <- gen_validate_call(paste0("gen_",kind),args)
  if (!checked$valid) .gen_validation_stop(checked$errors,paste0("gen_",kind))
  args <- checked$args; args$kind <- kind
  .gen_sound_tools()
  args$base_url <- .gen_sound_base_url(args$service,args$base_url)
  .gen_sound_headers(args$service)
  result <- .gen_sound_run(args,list(label=args$label,service=args$service,model=args$model,
    prompt_usado=args$prompt,content_type="audio",sound_kind=kind),args$directory,args$persist,.gen_sound_budget(args$timeout_api))
  if (args$persist && Sys.getenv("genflow_SKIP_PERSIST_LOG","0")!="1") try(.persist_stats_row(list(
    label=args$label,model=args$model,temp=NA_real_,duration=result$duration,tks_envia=NA_real_,
    tks_recebe=NA_real_,status_api=result$status_api)),silent=TRUE)
  result
}
