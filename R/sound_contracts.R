# Adapter facts supplement the common schemas; neither validation nor catalog
# enumeration needs credentials or a generation request.
.gen_sound_models <- function() list(
  sfx=c("eleven_text_to_sound_v2", "eleven_text_to_sound_v3"),
  music=c("music_v1", "music_v2", "music_v2_5"))

.gen_sound_url_valid <- function(value) {
  if (!.gen_scalar(value) || !grepl("^https?://[^[:space:]]+$",value)) return(FALSE)
  x <- tryCatch(httr::parse_url(value),error=function(e) NULL)
  !is.null(x) && .gen_scalar(x$hostname) && nzchar(x$hostname) &&
    !length(x$username) && !length(x$password) && !length(x$query) && !length(x$fragment)
}

.gen_sound_rules <- function(kind, args, usable, add, providers=TRUE, bound=character()) {
  if (usable("base_url") && !is.null(args$base_url) && !.gen_sound_url_valid(args$base_url))
    add("base_url",gen_error_message("sound_base_url_invalid"),"sound_base_url_invalid")
  lyrics <- usable("lyrics") && !is.null(args$lyrics)
  rule <- function(key,code) add(key,gen_error_message(code),code)
  if (kind == "music" && usable("instrumental")) {
    if (isTRUE(args$instrumental) && (lyrics || "lyrics" %in% bound)) rule("lyrics","sound_lyrics_conflict")
    if (identical(args$instrumental,FALSE) && usable("lyrics") && !lyrics) rule("lyrics","sound_lyrics_required")
  }
  if (!providers || !usable("service")) return(invisible(NULL))
  if (args$service == "elevenlabs") {
    if (usable("model") && !args$model %in% .gen_sound_models()[[kind]]) rule("model","sound_model_unsupported")
    if (usable("format") && !args$format %in% if (kind=="sfx") c("mp3","wav","opus") else c("mp3","opus")) rule("format","sound_format_unsupported")
    if (kind == "music") {
      if (usable("prompt") && nchar(args$prompt)>4100L) add("prompt",gen_error_message("too_long",list(limit=4100,unit="characters")),"too_long")
      if (lyrics) {
        lines <- strsplit(args$lyrics,"\n",fixed=TRUE)[[1L]]
        if (length(lines)>30L || any(nchar(lines)>200L)) rule("lyrics","sound_lyrics_limit")
        if (usable("duration_seconds") && args$duration_seconds>120) rule("duration_seconds","sound_duration_unsupported")
      }
      if (usable("seed") && !is.null(args$seed) && usable("lyrics") && !lyrics) rule("seed","sound_seed_unsupported")
    }
  }
  if (args$service == "acestep" && usable("duration_seconds") && args$duration_seconds<10)
    rule("duration_seconds","sound_duration_unsupported")
  invisible(NULL)
}
