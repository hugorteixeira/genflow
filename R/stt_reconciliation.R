#' Reconcile speaker labels across independently transcribed STT chunks
#'
#' This internal helper keeps every chunk's original speaker label in
#' `speaker_local`, and only applies a cross-chunk two-speaker permutation when
#' a conservative boundary detector supplies evidence for it. Textual overlap
#' is detected independently and can be removed even when the speaker roster is
#' not eligible for reconciliation.
#'
#' @keywords internal
#' @noRd
.stt_reconcile_chunk_results <- function(results,
                                         chunk_starts_seconds = NULL,
                                         chunk_overlap_seconds = 0,
                                         include_timestamps = FALSE,
                                         stable_roster = TRUE) {
  if (!is.list(results) || !length(results)) {
    stop("`results` must be a non-empty list of gen_stt results.", call. = FALSE)
  }
  include_timestamps <- .stt_reconcile_logical(
    include_timestamps,
    "include_timestamps"
  )
  stable_roster <- .stt_reconcile_logical(stable_roster, "stable_roster")

  chunk_count <- length(results)
  chunk_starts_seconds <- .stt_reconcile_chunk_starts(
    chunk_starts_seconds,
    chunk_count
  )
  overlap_by_boundary <- .stt_reconcile_overlap_values(
    chunk_overlap_seconds,
    chunk_count
  )

  chunks <- lapply(seq_along(results), function(index) {
    .stt_reconcile_normalize_chunk(
      results[[index]],
      chunk_index = index,
      chunk_start_seconds = chunk_starts_seconds[[index]]
    )
  })

  first_speakers <- chunks[[1]]$speakers
  speaker_maps <- vector("list", chunk_count)
  speaker_maps[[1]] <- stats::setNames(first_speakers, first_speakers)
  unresolved_global_speakers <- character()
  used_global_speakers <- unname(speaker_maps[[1]])
  chunks[[1]]$segments <- .stt_reconcile_apply_map(
    chunks[[1]]$segments,
    speaker_maps[[1]]
  )

  boundaries <- vector("list", max(0L, chunk_count - 1L))
  merged_segments <- chunks[[1]]$segments

  if (chunk_count > 1L) {
    for (right_index in 2:chunk_count) {
      left_index <- right_index - 1L
      left_chunk <- chunks[[left_index]]
      right_chunk <- chunks[[right_index]]
      left_segments <- left_chunk$segments
      right_segments <- right_chunk$segments
      overlap_seconds <- overlap_by_boundary[[left_index]]

      overlap <- .stt_reconcile_overlap_evidence(
        left_segments,
        right_segments,
        overlap_seconds = overlap_seconds,
        left_duration_seconds = left_chunk$duration_seconds,
        left_chunk_start_seconds = left_chunk$chunk_start_seconds,
        right_chunk_start_seconds = right_chunk$chunk_start_seconds
      )
      continuity <- if (overlap_seconds <= 0) {
        .stt_reconcile_continuity_evidence(
          left_segments,
          right_segments,
          left_duration_seconds = left_chunk$duration_seconds
        )
      } else {
        list(
          accepted = FALSE,
          score = 0,
          left_speaker = "",
          right_speaker = ""
        )
      }

      mapping <- .stt_reconcile_two_speaker_map(
        left_speakers = .stt_reconcile_mapped_roster(
          left_chunk$speakers,
          speaker_maps[[left_index]]
        ),
        right_speakers = right_chunk$speakers,
        overlap = overlap,
        continuity = continuity,
        stable_roster = stable_roster,
        unresolved_map = .stt_reconcile_unresolved_map(
          right_chunk$speakers,
          chunk_index = right_index,
          used_global_speakers = used_global_speakers
        )
      )
      inherited_unresolved <- mapping$map[
        unname(mapping$map) %in% unresolved_global_speakers
      ]
      unresolved_values <- unique(c(
        unname(mapping$unresolved),
        unname(inherited_unresolved)
      ))
      mapping$unresolved <- mapping$map[
        unname(mapping$map) %in% unresolved_values
      ]
      unresolved_global_speakers <- unique(c(
        unresolved_global_speakers,
        unname(mapping$unresolved)
      ))
      used_global_speakers <- unique(c(
        used_global_speakers,
        unname(mapping$map)
      ))
      speaker_maps[[right_index]] <- mapping$map

      right_segments <- .stt_reconcile_apply_map(
        right_segments,
        mapping$map
      )
      deduplicated_tokens <- 0L
      if (isTRUE(overlap$deduplicate)) {
        deduplicated_tokens <- overlap$right_prefix_tokens
        right_segments <- .stt_reconcile_drop_prefix_tokens(
          right_segments,
          deduplicated_tokens
        )
      }

      chunks[[right_index]]$segments <- right_segments
      merged_segments <- c(merged_segments, right_segments)
      boundaries[[left_index]] <- list(
        left_chunk = as.integer(left_index),
        right_chunk = as.integer(right_index),
        status = mapping$status,
        method = mapping$method,
        score = mapping$score,
        map = mapping$map,
        direct = mapping$direct,
        inferred = mapping$inferred,
        unresolved = mapping$unresolved,
        resolved = !length(mapping$unresolved),
        overlap_seconds = overlap_seconds,
        overlap_tokens = overlap$matched_tokens,
        overlap_informative_tokens = overlap$informative_tokens,
        overlap_identity = overlap$identity,
        overlap_timing_verified = overlap$timing_verified,
        overlap_timing_unverified = overlap$timing_unverified,
        timing_overlap_accepted = isTRUE(mapping$timing$accepted),
        timing_overlap_reason = mapping$timing$reason %||% "unavailable",
        timing_overlap_support_seconds =
          mapping$timing$best_support %||% 0,
        timing_overlap_total_support_seconds =
          mapping$timing$total_support %||% 0,
        timing_overlap_required_support_seconds =
          overlap$timing_required_support_seconds %||% 0,
        timing_overlap_purity = mapping$timing$purity %||% 0,
        timing_overlap_margin = mapping$timing$margin %||% 0,
        deduplicated_tokens = as.integer(deduplicated_tokens),
        continuation_score = continuity$score,
        continuation_detected = continuity$accepted
      )
    }
  }

  plain_text <- .stt_reconcile_plain_text(merged_segments)
  has_diarization <- any(vapply(
    merged_segments,
    function(segment) nzchar(.stt_reconcile_segment_speaker(segment)),
    logical(1)
  ))
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
      "Cannot fully reconcile failed STT chunk(s): ",
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
      speaker_maps = speaker_maps,
      boundaries = boundaries,
      unresolved_global_speakers = unresolved_global_speakers,
      chunk_count = as.integer(chunk_count),
      chunk_starts_seconds = chunk_starts_seconds,
      chunk_overlap_seconds = overlap_by_boundary,
      reconciliation = "deterministic-boundary-v1"
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
.stt_reconcile_overlap_values <- function(value, chunk_count) {
  boundary_count <- max(0L, chunk_count - 1L)
  if (!boundary_count) return(numeric())
  overlap <- suppressWarnings(as.numeric(value))
  if (length(overlap) == 1L) overlap <- rep(overlap, boundary_count)
  if (length(overlap) != boundary_count ||
      any(!is.finite(overlap)) ||
      any(overlap < 0)) {
    stop(
      paste0(
        "`chunk_overlap_seconds` must be one non-negative value or one ",
        "value per chunk boundary."
      ),
      call. = FALSE
    )
  }
  overlap
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
  duration_seconds <- .stt_reconcile_chunk_duration(result, segments)
  list(
    segments = segments,
    speakers = speakers,
    duration_seconds = duration_seconds,
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
.stt_reconcile_apply_map <- function(segments, map) {
  if (!length(segments)) return(segments)
  lapply(segments, function(segment) {
    local <- .stt_reconcile_segment_speaker(segment)
    segment$speaker_local <- local
    if (nzchar(local) && length(map) && local %in% names(map)) {
      global <- unname(map[[local]])
      if (nzchar(global)) segment$speaker <- global
    } else if (!nzchar(local)) {
      segment$speaker <- NULL
    }
    segment
  })
}

#' @keywords internal
#' @noRd
.stt_reconcile_mapped_roster <- function(local_speakers, map) {
  if (!length(local_speakers)) return(character())
  mapped <- vapply(local_speakers, function(local) {
    if (length(map) && local %in% names(map)) {
      unname(map[[local]])
    } else {
      local
    }
  }, character(1))
  unique(mapped[nzchar(mapped)])
}

#' Allocate an explicit, chunk-scoped namespace for unresolved speakers
#'
#' An abstained boundary must not silently reuse local labels such as `S01` as
#' though they were already linked to the previous chunk's global `S01`.
#' Chunk-scoped unresolved ids remain deterministic and can still be propagated
#' by strong evidence at a later boundary without contaminating an established
#' global identity.
#'
#' @keywords internal
#' @noRd
.stt_reconcile_unresolved_map <- function(local_speakers,
                                          chunk_index,
                                          used_global_speakers) {
  if (!length(local_speakers)) return(character())
  allocated <- character(length(local_speakers))
  names(allocated) <- local_speakers
  used <- unique(as.character(used_global_speakers))
  for (index in seq_along(local_speakers)) {
    local <- local_speakers[[index]]
    local_tag <- gsub(
      "^_+|_+$",
      "",
      gsub("[^[:alnum:]]+", "_", local, perl = TRUE),
      perl = TRUE
    )
    if (!nzchar(local_tag)) local_tag <- sprintf("L%02d", index)
    base <- sprintf("U%04d_%s", as.integer(chunk_index), local_tag)
    candidate <- base
    suffix <- 1L
    while (candidate %in% c(used, allocated)) {
      suffix <- suffix + 1L
      candidate <- paste0(base, "_", suffix)
    }
    allocated[[index]] <- candidate
  }
  allocated
}

#' @keywords internal
#' @noRd
.stt_reconcile_continuity_evidence <- function(left_segments,
                                                right_segments,
                                                left_duration_seconds) {
  left <- .stt_reconcile_last_speaker_segment(left_segments)
  right <- .stt_reconcile_first_speaker_segment(right_segments)
  if (is.null(left) || is.null(right)) {
    return(list(
      accepted = FALSE,
      score = 0,
      left_speaker = "",
      right_speaker = ""
    ))
  }

  left_text <- .stt_reconcile_scalar_text(left$text)
  right_text <- .stt_reconcile_scalar_text(right$text)
  terminal <- grepl(
    "[.!?\u2026][\"'\u201d\u2019\\)\\]}]*$",
    left_text,
    perl = TRUE
  )
  soft_ending <- grepl(
    "[,;:\u2014\u2013-][\"'\u201d\u2019\\)\\]}]*$",
    left_text,
    perl = TRUE
  )
  left_word <- .stt_reconcile_boundary_word(left_text, last = TRUE)
  right_word <- .stt_reconcile_boundary_word(right_text, last = FALSE)
  incomplete_end_words <- c(
    "a", "an", "the", "and", "or", "but", "because", "that", "of", "to",
    "from", "for", "with", "by", "is", "are", "was", "were", "it", "its",
    "it's", "i'm", "we're", "you're", "uma", "um", "o", "a", "os", "as",
    "e", "ou", "mas", "porque", "que", "de", "do", "da", "dos", "das",
    "para", "por", "com", "em", "no", "na", "nos", "nas",
    "\u00e9", "s\u00e3o"
  )
  continuation_start_words <- c(
    "and", "or", "but", "because", "of", "to", "from", "for", "with",
    "which", "that", "who", "where", "when", "e", "ou", "mas", "porque",
    "de", "do", "da", "dos", "das", "para", "por", "com", "que", "qual",
    "onde", "quando"
  )
  incomplete_end <- soft_ending ||
    .stt_reconcile_has_unclosed_delimiter(left_text) ||
    left_word %in% incomplete_end_words
  continuation_start <- right_word %in% continuation_start_words ||
    .stt_reconcile_starts_lowercase(right_text)

  score <- 0
  if (!terminal) score <- score + 0.55
  if (incomplete_end) score <- score + 0.25
  if (continuation_start) score <- score + 0.20
  if (.stt_reconcile_segments_touch_boundary(
      left,
      right,
      left_duration_seconds
    )) {
    score <- score + 0.10
  }
  if (terminal) score <- score - 0.35
  score <- max(0, min(1, score))
  accepted <- !terminal &&
    (incomplete_end || continuation_start) &&
    score >= 0.80

  list(
    accepted = accepted,
    score = score,
    left_speaker = .stt_reconcile_segment_global_speaker(left),
    right_speaker = .stt_reconcile_segment_speaker(right)
  )
}

#' @keywords internal
#' @noRd
.stt_reconcile_first_speaker_segment <- function(segments) {
  if (!length(segments)) return(NULL)
  for (segment in segments) {
    if (nzchar(.stt_reconcile_segment_speaker(segment)) &&
        nzchar(.stt_reconcile_scalar_text(segment$text))) {
      return(segment)
    }
  }
  NULL
}

#' @keywords internal
#' @noRd
.stt_reconcile_last_speaker_segment <- function(segments) {
  if (!length(segments)) return(NULL)
  for (index in rev(seq_along(segments))) {
    segment <- segments[[index]]
    if (nzchar(.stt_reconcile_segment_speaker(segment)) &&
        nzchar(.stt_reconcile_scalar_text(segment$text))) {
      return(segment)
    }
  }
  NULL
}

#' @keywords internal
#' @noRd
.stt_reconcile_segment_global_speaker <- function(segment) {
  if (!is.list(segment)) return("")
  .stt_normalize_speaker_label(segment$speaker)
}

#' @keywords internal
#' @noRd
.stt_reconcile_boundary_word <- function(text, last = FALSE) {
  tokens <- .stt_reconcile_tokenize_text(text)
  if (!length(tokens$token)) return("")
  if (isTRUE(last)) {
    tokens$token[[length(tokens$token)]]
  } else {
    tokens$token[[1]]
  }
}

#' @keywords internal
#' @noRd
.stt_reconcile_starts_lowercase <- function(text) {
  match <- regexpr("\\p{L}", text, perl = TRUE)
  if (match[[1]] < 0L) return(FALSE)
  letter <- regmatches(text, match)
  identical(letter, tolower(letter)) && !identical(letter, toupper(letter))
}

#' @keywords internal
#' @noRd
.stt_reconcile_has_unclosed_delimiter <- function(text) {
  count_fixed <- function(pattern) {
    matches <- gregexpr(pattern, text, fixed = TRUE)[[1]]
    if (identical(matches[[1]], -1L)) 0L else length(matches)
  }
  count_fixed("(") > count_fixed(")") ||
    count_fixed("[") > count_fixed("]") ||
    count_fixed("{") > count_fixed("}")
}

#' @keywords internal
#' @noRd
.stt_reconcile_segments_touch_boundary <- function(left,
                                                    right,
                                                    left_duration_seconds) {
  if (!is.finite(left_duration_seconds)) return(FALSE)
  local_value <- function(value, fallback) {
    number <- suppressWarnings(as.numeric(value %||% NA_real_)[1])
    if (length(number) && is.finite(number)) number else fallback
  }
  left_interval <- .stt_reconcile_segment_interval(left)
  right_interval <- .stt_reconcile_segment_interval(right)
  left_end <- local_value(left$end_local, left_interval[["end"]])
  right_start <- local_value(right$start_local, right_interval[["start"]])
  is.finite(left_end) &&
    is.finite(right_start) &&
    left_duration_seconds - left_end <= 0.75 &&
    right_start <= 0.75
}

#' Build duration-weighted speaker votes inside the shared audio window
#'
#' @keywords internal
#' @noRd
.stt_reconcile_timing_overlap_evidence <- function(
    left_segments,
    right_segments,
    overlap_seconds,
    left_duration_seconds,
    left_chunk_start_seconds = NA_real_,
    right_chunk_start_seconds = NA_real_) {
  empty_votes <- data.frame(
    left_speaker = character(),
    right_speaker = character(),
    weight = numeric(),
    stringsAsFactors = FALSE
  )
  required <- if (is.finite(overlap_seconds) && overlap_seconds > 0) {
    max(1.5, 0.35 * overlap_seconds)
  } else {
    Inf
  }
  empty <- function(verified = FALSE, frame = "unavailable") {
    list(
      verified = verified,
      votes = empty_votes,
      total_support_seconds = 0,
      required_support_seconds = required,
      frame = frame
    )
  }
  if (!is.finite(overlap_seconds) || overlap_seconds <= 0 ||
      !is.finite(left_duration_seconds) || left_duration_seconds <= 0) {
    return(empty())
  }

  global_frame <- is.finite(left_chunk_start_seconds) &&
    is.finite(right_chunk_start_seconds)
  if (global_frame) {
    left_end <- left_chunk_start_seconds + left_duration_seconds
    window_start <- max(left_end - overlap_seconds, right_chunk_start_seconds)
    window_end <- min(
      left_end,
      right_chunk_start_seconds + overlap_seconds
    )
    frame <- "global"
  } else {
    window_start <- left_duration_seconds - overlap_seconds
    window_end <- left_duration_seconds
    frame <- "local-offset"
  }
  if (window_end <= window_start) return(empty(TRUE, frame))

  collect <- function(segments, side) {
    Filter(Negate(is.null), lapply(segments, function(segment) {
      speaker <- if (identical(side, "left")) {
        .stt_reconcile_segment_global_speaker(segment)
      } else {
        .stt_reconcile_segment_speaker(segment)
      }
      if (!nzchar(speaker)) return(NULL)
      local <- .stt_reconcile_local_interval(segment)
      if (!all(is.finite(local))) return(list(valid = FALSE))
      if (global_frame && !is.null(segment$start_local)) {
        interval <- .stt_reconcile_segment_interval(segment)
      } else {
        offset <- if (identical(side, "right")) {
          left_duration_seconds - overlap_seconds
        } else {
          0
        }
        interval <- local + offset
      }
      list(
        valid = all(is.finite(interval)),
        speaker = speaker,
        start = interval[["start"]],
        end = interval[["end"]]
      )
    }))
  }
  left <- collect(left_segments, "left")
  right <- collect(right_segments, "right")
  if (!length(left) || !length(right) ||
      any(!vapply(left, `[[`, logical(1), "valid")) ||
      any(!vapply(right, `[[`, logical(1), "valid"))) {
    return(empty(FALSE, frame))
  }

  raw_votes <- list()
  for (left_segment in left) {
    left_start <- max(left_segment$start, window_start)
    left_end <- min(left_segment$end, window_end)
    if (left_end <= left_start) next
    for (right_segment in right) {
      right_start <- max(right_segment$start, window_start)
      right_end <- min(right_segment$end, window_end)
      support <- min(left_end, right_end) - max(left_start, right_start)
      if (!is.finite(support) || support <= 0) next
      raw_votes[[length(raw_votes) + 1L]] <- data.frame(
        left_speaker = left_segment$speaker,
        right_speaker = right_segment$speaker,
        weight = support,
        stringsAsFactors = FALSE
      )
    }
  }
  if (!length(raw_votes)) return(empty(TRUE, frame))
  votes <- do.call(rbind, raw_votes)
  votes <- stats::aggregate(
    votes$weight,
    by = list(
      left_speaker = votes$left_speaker,
      right_speaker = votes$right_speaker
    ),
    FUN = sum
  )
  names(votes)[[3]] <- "weight"
  list(
    verified = TRUE,
    votes = votes,
    total_support_seconds = sum(votes$weight),
    required_support_seconds = required,
    frame = frame
  )
}

#' @keywords internal
#' @noRd
.stt_reconcile_two_speaker_map <- function(left_speakers,
                                            right_speakers,
                                            overlap,
                                            continuity,
                                            stable_roster,
                                            unresolved_map) {
  unresolved_map <- unresolved_map[right_speakers]
  timing <- list(
    accepted = FALSE,
    reason = "unavailable",
    best_support = 0,
    total_support = overlap$timing_total_support_seconds %||% 0,
    purity = 0,
    margin = 0
  )
  if (isTRUE(stable_roster) &&
      length(left_speakers) == 2L &&
      length(right_speakers) == 2L &&
      isTRUE(overlap$timing_verified) &&
      nrow(overlap$timing_votes)) {
    timing <- .stt_reconcile_overlap_assignment(
      overlap$timing_votes,
      left_speakers,
      right_speakers,
      source = "timing_overlap",
      minimum_support = overlap$timing_required_support_seconds
    )
  }
  attach_timing <- function(value) {
    value$timing <- timing
    value
  }
  if (!isTRUE(stable_roster) ||
      length(left_speakers) != 2L ||
      length(right_speakers) != 2L) {
    timing$reason <- if (!isTRUE(stable_roster)) {
      "unstable_roster"
    } else {
      "unsupported_roster_size"
    }
    return(attach_timing(list(
      map = unresolved_map,
      status = "abstained",
      method = if (!isTRUE(stable_roster)) {
        "unstable_roster"
      } else {
        "unsupported_roster_size"
      },
      score = 0,
      direct = character(),
      inferred = character(),
      unresolved = unresolved_map
    )))
  }

  direct_left <- ""
  direct_right <- ""
  score <- 0
  method <- "none"
  text_assignment <- list(accepted = FALSE)
  if (isTRUE(overlap$accepted) && nrow(overlap$votes)) {
    text_assignment <- .stt_reconcile_overlap_assignment(
      overlap$votes,
      left_speakers,
      right_speakers
    )
  }
  if (isTRUE(text_assignment$accepted) && isTRUE(timing$accepted) &&
      !identical(
        unname(text_assignment$map[right_speakers]),
        unname(timing$map[right_speakers])
      )) {
    return(attach_timing(list(
      map = unresolved_map,
      status = "abstained",
      method = "conflicting_overlap_evidence",
      score = max(text_assignment$score, timing$score),
      direct = character(),
      inferred = character(),
      unresolved = unresolved_map
    )))
  }
  if (isTRUE(text_assignment$accepted)) {
    return(attach_timing(text_assignment))
  }
  if (isTRUE(timing$accepted)) return(attach_timing(timing))
  if (isTRUE(continuity$accepted) &&
      continuity$left_speaker %in% left_speakers &&
      continuity$right_speaker %in% right_speakers) {
    direct_left <- continuity$left_speaker
    direct_right <- continuity$right_speaker
    score <- continuity$score
    method <- "continuation"
  }
  if (!nzchar(direct_left) || !nzchar(direct_right)) {
    return(attach_timing(list(
      map = unresolved_map,
      status = "abstained",
      method = "no_evidence",
      score = max(overlap$score, continuity$score),
      direct = character(),
      inferred = character(),
      unresolved = unresolved_map
    )))
  }

  remaining_left <- setdiff(left_speakers, direct_left)
  remaining_right <- setdiff(right_speakers, direct_right)
  map <- stats::setNames(c(direct_left, remaining_left), c(
    direct_right,
    remaining_right
  ))
  inferred_score <- score * 0.85
  attach_timing(list(
    map = map[right_speakers],
    status = "accepted",
    method = .stt_reconcile_map_method(map, right_speakers, method),
    score = score,
    direct = stats::setNames(direct_left, direct_right),
    inferred = stats::setNames(remaining_left, remaining_right),
    inferred_score = inferred_score,
    unresolved = character()
  ))
}

#' @keywords internal
#' @noRd
.stt_reconcile_overlap_assignment <- function(votes,
                                               left_speakers,
                                               right_speakers,
                                               source = "overlap",
                                               minimum_support = 0) {
  vote_value <- function(left, right) {
    matches <- votes$left_speaker == left & votes$right_speaker == right
    sum(votes$weight[matches])
  }
  first_map <- stats::setNames(left_speakers, right_speakers)
  second_map <- stats::setNames(rev(left_speakers), right_speakers)
  map_score <- function(map) {
    sum(vapply(names(map), function(right) {
      vote_value(map[[right]], right)
    }, numeric(1)))
  }
  first_score <- map_score(first_map)
  second_score <- map_score(second_map)
  total <- sum(votes$weight)
  best_score <- max(first_score, second_score)
  runner_up <- min(first_score, second_score)
  purity <- if (total > 0) best_score / total else 0
  margin <- if (total > 0) (best_score - runner_up) / total else 0
  rejected <- function(reason) {
    list(
      accepted = FALSE,
      reason = reason,
      best_support = best_score,
      total_support = total,
      purity = purity,
      margin = margin
    )
  }
  if (total <= 0) return(rejected("no_support"))
  if (identical(first_score, second_score)) return(rejected("tied"))
  if (purity < 0.80 || margin < 0.25) {
    return(rejected("ambiguous"))
  }
  if (best_score < minimum_support) {
    return(rejected("insufficient_support"))
  }

  best <- if (first_score > second_score) first_map else second_map
  direct_support <- vapply(names(best), function(right) {
    vote_value(best[[right]], right)
  }, numeric(1))
  direct_names <- names(direct_support)[direct_support > 0]
  inferred_names <- setdiff(names(best), direct_names)
  method <- .stt_reconcile_map_method(best, right_speakers, source)
  list(
    accepted = TRUE,
    reason = "accepted",
    map = best,
    status = "accepted",
    method = method,
    score = min(1, purity * (0.5 + 0.5 * margin)),
    direct = best[direct_names],
    inferred = best[inferred_names],
    unresolved = character(),
    best_support = best_score,
    total_support = total,
    purity = purity,
    margin = margin
  )
}

#' @keywords internal
#' @noRd
.stt_reconcile_map_method <- function(map, right_speakers, source) {
  ordered <- map[right_speakers]
  permutation <- if (all(unname(ordered) == right_speakers)) {
    "identity"
  } else if (length(right_speakers) == 2L) {
    "swap"
  } else {
    "permutation"
  }
  paste(source, permutation, sep = "_")
}

#' @keywords internal
#' @noRd
.stt_reconcile_overlap_evidence <- function(left_segments,
                                             right_segments,
                                             overlap_seconds,
                                             left_duration_seconds,
                                             left_chunk_start_seconds = NA_real_,
                                             right_chunk_start_seconds = NA_real_) {
  timing <- .stt_reconcile_timing_overlap_evidence(
    left_segments = left_segments,
    right_segments = right_segments,
    overlap_seconds = overlap_seconds,
    left_duration_seconds = left_duration_seconds,
    left_chunk_start_seconds = left_chunk_start_seconds,
    right_chunk_start_seconds = right_chunk_start_seconds
  )
  empty_result <- function(
      timing_verified = isTRUE(timing$verified),
      timing_unverified = !isTRUE(timing$verified)) {
    list(
      accepted = FALSE,
      deduplicate = FALSE,
      score = 0,
      matched_tokens = 0L,
      informative_tokens = 0L,
      identity = 0,
      timing_verified = timing_verified,
      timing_unverified = timing_unverified,
      timing_votes = timing$votes,
      timing_total_support_seconds = timing$total_support_seconds,
      timing_required_support_seconds = timing$required_support_seconds,
      timing_frame = timing$frame,
      right_prefix_tokens = 0L,
      votes = data.frame(
        left_speaker = character(),
        right_speaker = character(),
        weight = numeric(),
        stringsAsFactors = FALSE
      )
    )
  }
  if (!is.finite(overlap_seconds) || overlap_seconds <= 0) {
    return(empty_result())
  }

  left_all <- .stt_reconcile_segment_tokens(left_segments, global = TRUE)
  right_all <- .stt_reconcile_segment_tokens(right_segments, global = FALSE)
  if (!length(left_all$token) || !length(right_all$token)) {
    return(empty_result())
  }

  left_has_timing <- is.finite(left_duration_seconds) &&
    any(is.finite(left_all$time))
  right_has_timing <- any(is.finite(right_all$time))
  left_complete_timing <- left_has_timing &&
    all(is.finite(left_all$time))
  right_complete_timing <- right_has_timing &&
    all(is.finite(right_all$time))
  timing_verified <- isTRUE(timing$verified) &&
    left_complete_timing && right_complete_timing
  timing_unverified <- !timing_verified

  subset_tokens <- function(value, keep) {
    lapply(value, function(column) column[keep])
  }
  left <- if (left_has_timing) {
    window_start <- max(0, left_duration_seconds - overlap_seconds)
    keep <- is.finite(left_all$time) &
      left_all$time >= window_start &
      left_all$time <= left_duration_seconds
    subset_tokens(left_all, keep)
  } else {
    left_all
  }
  right <- if (right_has_timing) {
    keep <- is.finite(right_all$time) &
      right_all$time >= 0 &
      right_all$time <= overlap_seconds
    subset_tokens(right_all, keep)
  } else {
    right_all
  }
  if (!length(left$token) || !length(right$token)) {
    return(empty_result(timing_verified, timing_unverified))
  }

  max_tokens <- 200L
  if (length(left$token) > max_tokens) {
    keep <- seq.int(length(left$token) - max_tokens + 1L, length(left$token))
    left <- lapply(left, function(value) value[keep])
  }
  if (length(right$token) > max_tokens) {
    keep <- seq_len(max_tokens)
    right <- lapply(right, function(value) value[keep])
  }

  # Even after temporal filtering, matching is only safe when it still reaches
  # the actual transcript edges. Otherwise dropping `match_size` tokens from
  # the right chunk could remove text that was never aligned.
  if (utils::tail(left$ordinal, 1L) != length(left_all$token) ||
      head(right$ordinal, 1L) != 1L) {
    return(empty_result(timing_verified, timing_unverified))
  }

  max_match <- min(length(left$token), length(right$token))
  match_size <- 0L
  minimum_match <- if (timing_verified) 4L else 5L
  if (max_match >= minimum_match) {
    for (candidate in seq.int(max_match, minimum_match, by = -1L)) {
      if (identical(
          utils::tail(left$token, candidate),
          head(right$token, candidate)
        )) {
        match_size <- candidate
        break
      }
    }
  }
  if (!match_size) {
    return(empty_result(timing_verified, timing_unverified))
  }

  left_index <- seq.int(length(left$token) - match_size + 1L, length(left$token))
  right_index <- seq_len(match_size)
  if (!identical(
      right$ordinal[right_index],
      seq_len(match_size)
    )) {
    return(empty_result(timing_verified, timing_unverified))
  }
  matched_tokens <- left$token[left_index]
  informative <- .stt_reconcile_informative_tokens(matched_tokens)
  informative_count <- sum(informative)
  matched_characters <- sum(nchar(matched_tokens, type = "chars"))
  insufficient <- if (timing_verified) {
    informative_count < 2L ||
      (match_size < 6L && matched_characters < 18L)
  } else {
    informative_count < 3L ||
      (match_size < 6L &&
        (informative_count < 4L || matched_characters < 28L))
  }
  if (insufficient) {
    return(empty_result(timing_verified, timing_unverified))
  }

  pairs <- data.frame(
    left_speaker = left$speaker[left_index],
    right_speaker = right$speaker[right_index],
    stringsAsFactors = FALSE
  )
  pairs <- pairs[
    nzchar(pairs$left_speaker) & nzchar(pairs$right_speaker),
    ,
    drop = FALSE
  ]
  votes <- if (nrow(pairs)) {
    weights <- stats::aggregate(
      rep(1, nrow(pairs)),
      by = list(
        left_speaker = pairs$left_speaker,
        right_speaker = pairs$right_speaker
      ),
      FUN = sum
    )
    names(weights)[[3]] <- "weight"
    weights
  } else {
    empty_result()$votes
  }
  score <- min(
    1,
    (match_size / 8) * 0.6 +
      (informative_count / 4) * 0.4
  )
  list(
    accepted = TRUE,
    deduplicate = TRUE,
    score = score,
    matched_tokens = as.integer(match_size),
    informative_tokens = as.integer(informative_count),
    identity = 1,
    timing_verified = timing_verified,
    timing_unverified = timing_unverified,
    timing_votes = timing$votes,
    timing_total_support_seconds = timing$total_support_seconds,
    timing_required_support_seconds = timing$required_support_seconds,
    timing_frame = timing$frame,
    right_prefix_tokens = as.integer(match_size),
    votes = votes
  )
}

#' @keywords internal
#' @noRd
.stt_reconcile_segment_tokens <- function(segments, global = FALSE) {
  token <- character()
  speaker <- character()
  segment_index <- integer()
  time <- numeric()
  for (index in seq_along(segments)) {
    current <- .stt_reconcile_tokenize_text(segments[[index]]$text)
    if (!length(current$token)) next
    current_speaker <- if (isTRUE(global)) {
      .stt_reconcile_segment_global_speaker(segments[[index]])
    } else {
      .stt_reconcile_segment_speaker(segments[[index]])
    }
    token <- c(token, current$token)
    speaker <- c(speaker, rep(current_speaker, length(current$token)))
    segment_index <- c(segment_index, rep(index, length(current$token)))
    interval <- .stt_reconcile_local_interval(segments[[index]])
    token_count <- length(current$token)
    token_time <- if (all(is.finite(interval))) {
      if (interval[["end"]] > interval[["start"]]) {
        interval[["start"]] +
          ((seq_len(token_count) - 0.5) / token_count) *
            (interval[["end"]] - interval[["start"]])
      } else {
        rep(interval[["start"]], token_count)
      }
    } else {
      rep(NA_real_, token_count)
    }
    time <- c(time, token_time)
  }
  list(
    token = token,
    speaker = speaker,
    segment_index = segment_index,
    time = time,
    ordinal = seq_along(token)
  )
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
.stt_reconcile_tokenize_text <- function(text) {
  text <- .stt_reconcile_scalar_text(text)
  if (!nzchar(text)) {
    return(list(
      token = character(),
      start = integer(),
      length = integer()
    ))
  }
  matches <- gregexpr(
    "[[:alnum:]]+(?:['\u2019][[:alnum:]]+)*",
    text,
    perl = TRUE
  )[[1]]
  if (identical(matches[[1]], -1L)) {
    return(list(
      token = character(),
      start = integer(),
      length = integer()
    ))
  }
  lengths <- attr(matches, "match.length")
  raw <- substring(text, matches, matches + lengths - 1L)
  normalized <- tolower(chartr("\u2019", "'", raw))
  list(
    token = normalized,
    start = as.integer(matches),
    length = as.integer(lengths)
  )
}

#' @keywords internal
#' @noRd
.stt_reconcile_informative_tokens <- function(tokens) {
  stopwords <- c(
    "a", "an", "the", "and", "or", "of", "to", "in", "is", "it",
    "um", "uma", "o", "os", "as", "e", "ou", "de", "do", "da", "em"
  )
  nchar(tokens, type = "chars") >= 4L & !(tokens %in% stopwords)
}

#' @keywords internal
#' @noRd
.stt_reconcile_drop_prefix_tokens <- function(segments, token_count) {
  remaining <- as.integer(token_count)
  if (!length(segments) || !is.finite(remaining) || remaining <= 0L) {
    return(segments)
  }
  kept <- list()
  for (segment in segments) {
    tokens <- .stt_reconcile_tokenize_text(segment$text)
    segment_token_count <- length(tokens$token)
    if (remaining >= segment_token_count && segment_token_count > 0L) {
      remaining <- remaining - segment_token_count
      next
    }
    if (remaining > 0L && segment_token_count > 0L) {
      removed_tokens <- remaining
      cut_at <- tokens$start[[remaining]] + tokens$length[[remaining]] - 1L
      remainder <- substring(segment$text, cut_at + 1L)
      remainder <- sub(
        "^[[:space:][:punct:]]+",
        "",
        remainder,
        perl = TRUE
      )
      segment$text <- trimws(remainder)
      remaining <- 0L
      if (!nzchar(segment$text)) next
      segment$trimmed <- TRUE
      segment$trimmed_prefix_tokens <- as.integer(removed_tokens)
      segment$trimmed_bounds <- list(
        start = segment$start,
        end = segment$end,
        start_time = segment$start_time,
        end_time = segment$end_time,
        timestamps = segment$timestamps,
        offsets = segment$offsets,
        start_local = segment$start_local,
        end_local = segment$end_local,
        timestamps_local = segment$timestamps_local,
        offsets_local = segment$offsets_local
      )
      segment$bounds_removed_after_trim <- TRUE
      for (field in c(
        "start", "end", "start_time", "end_time", "timestamps", "offsets",
        "start_local", "end_local", "timestamps_local", "offsets_local",
        "tokens"
      )) {
        segment[[field]] <- NULL
      }
    }
    kept[[length(kept) + 1L]] <- segment
  }
  kept
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
