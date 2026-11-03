# Shared byte/container inspection for speech, music and sound effects.
.gen_audio_normalize_format <- function(value) {
  value <- tolower(.gen_audio_scalar_text(value))
  aliases <- c(
    mpeg = "mp3",
    mpga = "mp3",
    wave = "wav",
    x_wav = "wav",
    oga = "ogg",
    mp4 = "m4a"
  )
  if (value %in% names(aliases)) {
    value <- unname(aliases[[value]])
  }
  known <- c("mp3", "opus", "ogg", "aac", "flac", "wav", "pcm", "webm", "m4a")
  if (nzchar(value) && value %in% known) value else NULL
}

.gen_audio_content_type_format <- function(content_type) {
  content_type <- tolower(.gen_audio_scalar_text(content_type))
  content_type <- sub(";.*$", "", content_type)
  mapping <- c(
    "audio/mpeg" = "mp3",
    "audio/mp3" = "mp3",
    "audio/x-mpeg" = "mp3",
    "audio/wav" = "wav",
    "audio/wave" = "wav",
    "audio/x-wav" = "wav",
    "audio/ogg" = "ogg",
    "application/ogg" = "ogg",
    "audio/opus" = "opus",
    "audio/aac" = "aac",
    "audio/x-aac" = "aac",
    "audio/flac" = "flac",
    "audio/x-flac" = "flac",
    "audio/webm" = "webm",
    "audio/mp4" = "m4a",
    "audio/x-m4a" = "m4a",
    "audio/pcm" = "pcm",
    "audio/x-pcm" = "pcm",
    "audio/l16" = "pcm"
  )
  if (content_type %in% names(mapping)) unname(mapping[[content_type]]) else NULL
}

.gen_audio_format_content_type <- function(format) {
  format <- .gen_audio_normalize_format(format)
  mapping <- c(
    mp3 = "audio/mpeg",
    wav = "audio/wav",
    ogg = "audio/ogg",
    opus = "audio/ogg",
    aac = "audio/aac",
    flac = "audio/flac",
    webm = "audio/webm",
    m4a = "audio/mp4",
    pcm = "audio/pcm"
  )
  if (!is.null(format) && format %in% names(mapping)) {
    unname(mapping[[format]])
  } else {
    "application/octet-stream"
  }
}

.gen_audio_raw_starts_with <- function(bytes, signature) {
  signature <- as.raw(signature)
  length(bytes) >= length(signature) &&
    identical(bytes[seq_along(signature)], signature)
}

.gen_audio_raw_contains <- function(bytes, signature) {
  signature <- as.raw(signature)
  if (!length(signature) || length(bytes) < length(signature)) {
    return(FALSE)
  }
  last_start <- length(bytes) - length(signature) + 1L
  any(vapply(
    seq_len(last_start),
    function(index) {
      identical(
        bytes[index:(index + length(signature) - 1L)],
        signature
      )
    },
    logical(1)
  ))
}

.gen_audio_magic_format <- function(path) {
  bytes <- tryCatch(
    readBin(path, what = "raw", n = 256L),
    error = function(e) raw()
  )
  if (!length(bytes)) {
    return(NULL)
  }

  if (.gen_audio_raw_starts_with(bytes, charToRaw("RIFF")) &&
      length(bytes) >= 12L &&
      identical(bytes[9:12], charToRaw("WAVE"))) {
    return("wav")
  }
  if (.gen_audio_raw_starts_with(bytes, charToRaw("fLaC"))) {
    return("flac")
  }
  if (.gen_audio_raw_starts_with(bytes, charToRaw("OggS"))) {
    if (.gen_audio_raw_contains(bytes, charToRaw("OpusHead"))) "opus" else "ogg"
  } else if (.gen_audio_raw_starts_with(bytes, charToRaw("ID3"))) {
    "mp3"
  } else if (length(bytes) >= 8L &&
      identical(bytes[5:8], charToRaw("ftyp"))) {
    "m4a"
  } else if (.gen_audio_raw_starts_with(bytes, as.raw(c(0x1a, 0x45, 0xdf, 0xa3)))) {
    "webm"
  } else if (.gen_audio_raw_contains(bytes, charToRaw("OpusHead"))) {
    "opus"
  } else if (length(bytes) >= 2L &&
      as.integer(bytes[[1]]) == 0xffL &&
      bitwAnd(as.integer(bytes[[2]]), 0xf6L) == 0xf0L) {
    "aac"
  } else if (length(bytes) >= 2L &&
      as.integer(bytes[[1]]) == 0xffL &&
      bitwAnd(as.integer(bytes[[2]]), 0xe0L) == 0xe0L &&
      bitwAnd(as.integer(bytes[[2]]), 0x06L) != 0L) {
    "mp3"
  } else {
    NULL
  }
}

.gen_audio_scalar_text <- function(value) {
  if (is.null(value) || !length(value)) {
    return("")
  }
  value <- as.character(value)[1]
  if (is.na(value)) "" else trimws(value)
}
