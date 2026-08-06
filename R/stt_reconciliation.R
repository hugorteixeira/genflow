#' Semantic version of final chunk merging
#'
#' The historical helper name is retained because it participates in public
#' STT fingerprints. The implementation no longer reconciles speaker
#' identities across independently transcribed chunks.
#'
#' @keywords internal
#' @noRd
.stt_reconciliation_version <- function() {
  "sequential-chunks-v1"
}

#' Merge independently transcribed STT chunks without guessing speaker identity
#'
#' Speaker labels produced for a single input remain recording-scoped. When an
#' external recording is split into multiple inputs, every label is namespaced
#' by chunk (`C01:S01`, `C02:S01`, ...), and the provider label is retained in
#' `speaker_local`. Chunks are concatenated in source order without overlap,
#' text deduplication, or cross-chunk speaker inference.
#'
#' @keywords internal
#' @noRd
.stt_reconcile_chunk_results <- function(results,
                                         chunk_starts_seconds = NULL,
                                         include_timestamps = FALSE) {
  if (!is.list(results) || !length(results)) {
    stop("`results` must be a non-empty list of gen_stt results.", call. = FALSE)
  }
  include_timestamps <- .stt_reconcile_logical(
    include_timestamps,
    "include_timestamps"
  )

  chunk_count <- length(results)
  chunk_starts_seconds <- .stt_reconcile_chunk_starts(
    chunk_starts_seconds,
    chunk_count
  )
  chunks <- lapply(seq_along(results), function(index) {
    .stt_reconcile_normalize_chunk(
      results[[index]],
      chunk_index = index,
      chunk_start_seconds = chunk_starts_seconds[[index]]
    )
  })
  has_diarization <- any(lengths(lapply(chunks, `[[`, "speakers")) > 0L)
  speaker_scope <- if (!has_diarization) {
    "none"
  } else if (chunk_count == 1L) {
    "recording"
  } else {
    "chunk-local"
  }

  chunk_speaker_labels <- lapply(seq_along(chunks), function(index) {
    speakers <- chunks[[index]]$speakers
    if (!length(speakers)) return(character())
    labels <- if (identical(speaker_scope, "chunk-local")) {
      prefix <- .stt_reconcile_chunk_prefix(index, chunk_count)
      paste0(prefix, ":", speakers)
    } else {
      speakers
    }
    stats::setNames(labels, speakers)
  })
  for (index in seq_along(chunks)) {
    chunks[[index]]$segments <- .stt_reconcile_apply_chunk_labels(
      chunks[[index]]$segments,
      chunk_speaker_labels[[index]],
      speaker_scope
    )
  }

  merged_segments <- unlist(
    lapply(chunks, `[[`, "segments"),
    recursive = FALSE,
    use.names = FALSE
  )

  plain_text <- .stt_reconcile_plain_text(merged_segments)
  diarized_transcript <- if (has_diarization) {
    .stt_render_diarized_transcript(
      merged_segments,
      fallback_text = plain_text,
      include_timestamps = include_timestamps
    )
  } else {
    NULL
  }

  source_status <- vapply(
    results,
    function(result) {
      status <- .stt_reconcile_scalar_text(result$status_api)
      if (nzchar(status)) toupper(status) else "SUCCESS"
    },
    character(1)
  )
  failed_chunks <- which(source_status != "SUCCESS")
  status_api <- if (length(failed_chunks)) "ERROR" else "SUCCESS"
  status_msg <- if (length(failed_chunks)) {
    paste0(
      "Cannot fully merge failed STT chunk(s): ",
      paste(failed_chunks, collapse = ", "),
      "."
    )
  } else {
    "OK"
  }

  result <- list(
    response_value = plain_text,
    service = .stt_reconcile_common_field(results, "service", "mixed"),
    model = .stt_reconcile_common_field(results, "model", "mixed"),
    duration = sum(vapply(
      results,
      function(value) {
        duration <- suppressWarnings(as.numeric(value$duration %||% 0)[1])
        if (is.finite(duration)) duration else 0
      },
      numeric(1)
    )),
    status_api = status_api,
    status_msg = status_msg,
    saved_file = NA_character_,
    audio = vapply(
      results,
      function(value) .stt_reconcile_scalar_text(value$audio),
      character(1)
    ),
    content_type = "text",
    metadata = list(
      segments = merged_segments,
      chunk_count = as.integer(chunk_count),
      chunk_starts_seconds = chunk_starts_seconds,
      chunk_merge = list(
        method = .stt_reconciliation_version(),
        order = "source",
        overlap_seconds = 0,
        text_deduplication = FALSE,
        speaker_scope = speaker_scope,
        cross_chunk_identity_tracking = FALSE,
        chunk_speaker_labels = chunk_speaker_labels
      )
    )
  )
  if (!is.null(diarized_transcript)) {
    result <- append(
      result,
      list(diarized_transcript = diarized_transcript),
      after = 1L
    )
    result <- append(
      result,
      list(saved_metadata_file = NA_character_),
      after = match("saved_file", names(result))
    )
  }
  result
}

#' @keywords internal
#' @noRd
.stt_reconcile_logical <- function(value, name) {
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    stop("`", name, "` must be TRUE or FALSE.", call. = FALSE)
  }
  value
}

#' @keywords internal
#' @noRd
.stt_reconcile_scalar_text <- function(value) {
  if (is.null(value) || !length(value) || is.na(value[[1]])) return("")
  trimws(as.character(value[[1]]))
}

#' @keywords internal
#' @noRd
.stt_reconcile_chunk_starts <- function(value, chunk_count) {
  if (is.null(value)) return(rep(NA_real_, chunk_count))
  starts <- suppressWarnings(as.numeric(value))
  if (length(starts) != chunk_count ||
      any(!is.finite(starts)) ||
      any(starts < 0)) {
    stop(
      "`chunk_starts_seconds` must contain one non-negative value per chunk.",
      call. = FALSE
    )
  }
  starts
}

#' @keywords internal
#' @noRd
.stt_reconcile_chunk_prefix <- function(index, chunk_count) {
  width <- max(2L, nchar(as.character(as.integer(chunk_count))))
  sprintf(paste0("C%0", width, "d"), as.integer(index))
}

#' @keywords internal
#' @noRd
.stt_reconcile_normalize_chunk <- function(result,
                                            chunk_index,
                                            chunk_start_seconds) {
  if (!is.list(result)) {
    stop("Every entry in `results` must be a gen_stt result.", call. = FALSE)
  }
  metadata <- if (is.list(result$metadata)) result$metadata else list()
  raw_segments <- metadata$segments
  if (!is.list(raw_segments) || inherits(raw_segments, "data.frame")) {
    raw_segments <- list()
  }

  segments <- Filter(
    Negate(is.null),
    lapply(raw_segments, function(segment) {
      .stt_reconcile_normalize_segment(
        segment,
        chunk_index = chunk_index,
        chunk_start_seconds = chunk_start_seconds
      )
    })
  )
  if (!length(segments)) {
    segments <- .stt_reconcile_parse_diarized_text(
      result$diarized_transcript,
      chunk_index = chunk_index,
      chunk_start_seconds = chunk_start_seconds
    )
  }
  if (!length(segments)) {
    text <- .stt_reconcile_scalar_text(result$response_value)
    if (nzchar(text)) {
      segments <- list(list(
        text = text,
        speaker_local = "",
        chunk_index = as.integer(chunk_index)
      ))
    }
  }

  speakers <- unique(vapply(
    segments,
    .stt_reconcile_segment_speaker,
    character(1)
  ))
  speakers <- speakers[nzchar(speakers)]
  list(
    segments = segments,
    speakers = speakers,
    duration_seconds = .stt_reconcile_chunk_duration(result, segments),
    chunk_start_seconds = chunk_start_seconds
  )
}

#' @keywords internal
#' @noRd
.stt_reconcile_normalize_segment <- function(segment,
                                              chunk_index,
                                              chunk_start_seconds) {
  if (!is.list(segment)) return(NULL)
  text <- .stt_reconcile_scalar_text(
    segment$text %||% segment$transcript %||% segment$transcription
  )
  if (!nzchar(text)) return(NULL)

  speaker <- .stt_normalize_speaker_label(
    segment$speaker %||% segment$speaker_id %||% segment$speaker_label
  )
  segment$text <- text
  segment$speaker_local <- speaker
  segment$chunk_index <- as.integer(chunk_index)
  if (nzchar(speaker)) {
    segment$speaker <- speaker
  } else {
    segment$speaker <- NULL
  }

  if (is.finite(chunk_start_seconds)) {
    interval <- .stt_reconcile_segment_interval(segment)
    if (all(is.finite(interval))) {
      segment$start_local <- interval[["start"]]
      segment$end_local <- interval[["end"]]
      segment$start <- chunk_start_seconds + interval[["start"]]
      segment$end <- chunk_start_seconds + interval[["end"]]
      if (is.list(segment$timestamps)) {
        segment$timestamps_local <- segment$timestamps
        segment$timestamps <- NULL
      }
      if (is.list(segment$offsets)) {
        segment$offsets_local <- segment$offsets
        segment$offsets <- NULL
      }
    }
  }
  segment
}

#' @keywords internal
#' @noRd
.stt_reconcile_parse_diarized_text <- function(value,
                                                chunk_index,
                                                chunk_start_seconds) {
  text <- .stt_reconcile_scalar_text(value)
  if (!nzchar(text)) return(list())
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  parsed <- lapply(lines, function(line) {
    matched <- regmatches(
      line,
      regexec(
        paste0(
          "^\\s*(?:\\[[0-9:.]+\\s*-->\\s*[0-9:.]+\\]\\s*)?",
          "\\[([^]]+)\\]\\s*(.+?)\\s*$"
        ),
        line,
        perl = TRUE
      )
    )[[1]]
    if (length(matched) != 3L) return(NULL)
    .stt_reconcile_normalize_segment(
      list(speaker = matched[[2]], text = matched[[3]]),
      chunk_index = chunk_index,
      chunk_start_seconds = chunk_start_seconds
    )
  })
  Filter(Negate(is.null), parsed)
}

#' @keywords internal
#' @noRd
.stt_reconcile_segment_interval <- function(segment) {
  numeric_value <- function(value) {
    number <- suppressWarnings(as.numeric(value %||% NA_real_)[1])
    if (length(number) && is.finite(number)) number else NA_real_
  }
  parse_clock <- function(value) {
    value <- .stt_reconcile_scalar_text(value)
    matched <- regmatches(
      value,
      regexec(
        "^([0-9]+):([0-9]{1,2}):([0-9]{1,2})(?:[,.]([0-9]+))?$",
        value,
        perl = TRUE
      )
    )[[1]]
    if (length(matched) != 5L) return(NA_real_)
    fraction <- if (nzchar(matched[[5]])) {
      suppressWarnings(as.numeric(paste0("0.", matched[[5]])))
    } else {
      0
    }
    suppressWarnings(
      as.numeric(matched[[2]]) * 3600 +
        as.numeric(matched[[3]]) * 60 +
        as.numeric(matched[[4]]) +
        fraction
    )
  }

  start <- numeric_value(segment$start %||% segment$start_time)
  end <- numeric_value(segment$end %||% segment$end_time)
  if (!(is.finite(start) && is.finite(end) && start >= 0 && end >= start)) {
    timestamps <- if (is.list(segment$timestamps)) segment$timestamps else list()
    start <- parse_clock(timestamps$from)
    end <- parse_clock(timestamps$to)
  }
  if (!(is.finite(start) && is.finite(end) && start >= 0 && end >= start)) {
    offsets <- if (is.list(segment$offsets)) segment$offsets else list()
    start <- numeric_value(offsets$from) / 1000
    end <- numeric_value(offsets$to) / 1000
  }
  if (!(is.finite(start) && is.finite(end) && start >= 0 && end >= start)) {
    return(c(start = NA_real_, end = NA_real_))
  }
  c(start = start, end = end)
}

#' @keywords internal
#' @noRd
.stt_reconcile_chunk_duration <- function(result, segments) {
  metadata <- if (is.list(result$metadata)) result$metadata else list()
  candidates <- c(
    suppressWarnings(as.numeric(metadata$input_duration_seconds %||% NA_real_)[1]),
    suppressWarnings(as.numeric(metadata$audio_duration_seconds %||% NA_real_)[1])
  )
  candidates <- candidates[is.finite(candidates) & candidates >= 0]
  if (length(candidates)) return(candidates[[1]])
  ends <- vapply(
    segments,
    function(segment) .stt_reconcile_local_interval(segment)[["end"]],
    numeric(1)
  )
  ends <- ends[is.finite(ends)]
  if (length(ends)) max(ends) else NA_real_
}

#' @keywords internal
#' @noRd
.stt_reconcile_segment_speaker <- function(segment) {
  if (!is.list(segment)) return("")
  .stt_normalize_speaker_label(
    segment$speaker_local %||% segment$speaker %||%
      segment$speaker_id %||% segment$speaker_label
  )
}

#' @keywords internal
#' @noRd
.stt_reconcile_apply_chunk_labels <- function(segments, labels, scope) {
  if (!length(segments)) return(segments)
  lapply(segments, function(segment) {
    local <- .stt_reconcile_segment_speaker(segment)
    segment$speaker_local <- local
    if (nzchar(local)) {
      segment$speaker <- if (local %in% names(labels)) {
        unname(labels[[local]])
      } else {
        local
      }
      segment$speaker_scope <- scope
    } else {
      segment$speaker <- NULL
    }
    segment
  })
}

#' @keywords internal
#' @noRd
.stt_reconcile_local_interval <- function(segment) {
  numeric_value <- function(value) {
    number <- suppressWarnings(as.numeric(value %||% NA_real_)[1])
    if (length(number) && is.finite(number)) number else NA_real_
  }
  local_start <- numeric_value(segment$start_local)
  local_end <- numeric_value(segment$end_local)
  if (is.finite(local_start) &&
      is.finite(local_end) &&
      local_start >= 0 &&
      local_end >= local_start) {
    return(c(start = local_start, end = local_end))
  }
  .stt_reconcile_segment_interval(segment)
}

#' @keywords internal
#' @noRd
.stt_reconcile_plain_text <- function(segments) {
  text <- vapply(
    segments,
    function(segment) .stt_reconcile_scalar_text(segment$text),
    character(1)
  )
  text <- text[nzchar(text)]
  trimws(paste(text, collapse = " "))
}

#' @keywords internal
#' @noRd
.stt_reconcile_common_field <- function(results, field, fallback) {
  values <- vapply(
    results,
    function(value) .stt_reconcile_scalar_text(value[[field]]),
    character(1)
  )
  values <- unique(values[nzchar(values)])
  if (length(values) == 1L) values[[1]] else fallback
}
