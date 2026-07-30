stt_reconcile_segment <- function(speaker,
                                  text,
                                  start = NULL,
                                  end = NULL) {
  segment <- list(speaker = speaker, text = text)
  if (!is.null(start)) segment$start <- start
  if (!is.null(end)) segment$end <- end
  segment
}

stt_reconcile_result <- function(segments,
                                 duration_seconds = 10,
                                 status_api = "SUCCESS") {
  list(
    response_value = paste(
      vapply(segments, `[[`, character(1), "text"),
      collapse = " "
    ),
    service = "local-native",
    model = "test-diarizer.gguf",
    duration = 1,
    status_api = status_api,
    status_msg = if (identical(status_api, "SUCCESS")) "OK" else "failed",
    audio = tempfile(fileext = ".wav"),
    metadata = list(
      input_duration_seconds = duration_seconds,
      segments = segments
    )
  )
}

test_that("continuation preserves an identity mapping for two speakers", {
  first <- stt_reconcile_result(list(
    stt_reconcile_segment("S01", "Welcome to the discussion.", 0, 2),
    stt_reconcile_segment("S02", "It's", 8.8, 10)
  ))
  second <- stt_reconcile_result(list(
    stt_reconcile_segment("S02", "from the company report.", 0, 1.4),
    stt_reconcile_segment("S01", "That makes sense.", 2, 3)
  ))

  reconciled <- genflow:::.stt_reconcile_chunk_results(list(first, second))

  expect_identical(
    reconciled$metadata$speaker_maps[[2]],
    c(S02 = "S02", S01 = "S01")
  )
  expect_identical(
    reconciled$metadata$boundaries[[1]]$method,
    "continuation_identity"
  )
  expect_identical(
    vapply(
      reconciled$metadata$segments,
      `[[`,
      character(1),
      "speaker_local"
    ),
    c("S01", "S02", "S02", "S01")
  )
  expect_match(
    reconciled$diarized_transcript,
    "\\[S02\\] It's from the company report\\."
  )
})

test_that("continuation swaps chunk-local labels for two stable speakers", {
  first <- stt_reconcile_result(list(
    stt_reconcile_segment("S01", "Could you clarify that?", 0, 2),
    stt_reconcile_segment("S02", "Tomorrow's prediction", 8.5, 10)
  ))
  second <- stt_reconcile_result(list(
    stt_reconcile_segment("S01", "of portfolio risk remains elevated.", 0, 2),
    stt_reconcile_segment("S02", "I agree with that.", 3, 4)
  ))

  reconciled <- genflow:::.stt_reconcile_chunk_results(list(first, second))
  second_chunk <- Filter(
    function(segment) identical(segment$chunk_index, 2L),
    reconciled$metadata$segments
  )

  expect_identical(
    reconciled$metadata$speaker_maps[[2]],
    c(S01 = "S02", S02 = "S01")
  )
  expect_identical(
    reconciled$metadata$boundaries[[1]]$method,
    "continuation_swap"
  )
  expect_identical(
    vapply(second_chunk, `[[`, character(1), "speaker_local"),
    c("S01", "S02")
  )
  expect_identical(
    vapply(second_chunk, `[[`, character(1), "speaker"),
    c("S02", "S01")
  )
  expect_match(
    reconciled$diarized_transcript,
    "\\[S02\\] Tomorrow's prediction of portfolio risk remains elevated\\."
  )
})

test_that("speaker maps are propagated across more than one boundary", {
  first <- stt_reconcile_result(list(
    stt_reconcile_segment("S01", "Start.", 0, 1),
    stt_reconcile_segment("S02", "The result of", 8, 10)
  ))
  second <- stt_reconcile_result(list(
    stt_reconcile_segment("S01", "the test was clear.", 0, 2),
    stt_reconcile_segment("S02", "We also know that", 8, 10)
  ))
  third <- stt_reconcile_result(list(
    stt_reconcile_segment("S01", "the total increased.", 0, 2),
    stt_reconcile_segment("S02", "Agreed.", 3, 4)
  ))

  reconciled <- genflow:::.stt_reconcile_chunk_results(list(
    first,
    second,
    third
  ))

  expect_identical(
    reconciled$metadata$speaker_maps[[2]],
    c(S01 = "S02", S02 = "S01")
  )
  expect_identical(
    reconciled$metadata$speaker_maps[[3]],
    c(S01 = "S01", S02 = "S02")
  )
  expect_identical(
    vapply(
      reconciled$metadata$segments,
      `[[`,
      character(1),
      "speaker"
    ),
    c("S01", "S02", "S02", "S01", "S01", "S02")
  )
  expect_identical(
    vapply(
      reconciled$metadata$boundaries,
      `[[`,
      character(1),
      "method"
    ),
    c("continuation_swap", "continuation_identity")
  )
})

test_that("future evidence propagates an unresolved namespace without contaminating known ids", {
  first <- stt_reconcile_result(list(
    stt_reconcile_segment("S01", "Known first speaker.", 0, 2),
    stt_reconcile_segment("S02", "A complete boundary.", 8, 10)
  ))
  second <- stt_reconcile_result(list(
    stt_reconcile_segment("S01", "Unlinked local speaker.", 0, 2),
    stt_reconcile_segment("S02", "The later result of", 8, 10)
  ))
  third <- stt_reconcile_result(list(
    stt_reconcile_segment("S01", "the experiment was positive.", 0, 2),
    stt_reconcile_segment("S02", "Understood.", 3, 4)
  ))

  reconciled <- genflow:::.stt_reconcile_chunk_results(list(
    first,
    second,
    third
  ))

  expect_identical(
    reconciled$metadata$speaker_maps[[2]],
    c(S01 = "U0002_S01", S02 = "U0002_S02")
  )
  expect_identical(
    reconciled$metadata$speaker_maps[[3]],
    c(S01 = "U0002_S02", S02 = "U0002_S01")
  )
  expect_identical(
    reconciled$metadata$boundaries[[2]]$method,
    "continuation_swap"
  )
  expect_false(reconciled$metadata$boundaries[[2]]$resolved)
  expect_setequal(
    unname(reconciled$metadata$boundaries[[2]]$unresolved),
    c("U0002_S01", "U0002_S02")
  )
  expect_false(any(
    unname(reconciled$metadata$speaker_maps[[3]]) %in% c("S01", "S02")
  ))
})

test_that("a complete sentence does not trigger a boundary permutation", {
  first <- stt_reconcile_result(list(
    stt_reconcile_segment("S01", "Opening.", 0, 1),
    stt_reconcile_segment("S02", "The forecast is complete.", 8, 10)
  ))
  second <- stt_reconcile_result(list(
    stt_reconcile_segment("S01", "of course we can revisit it.", 0, 2),
    stt_reconcile_segment("S02", "Next topic.", 3, 4)
  ))

  reconciled <- genflow:::.stt_reconcile_chunk_results(list(first, second))

  expect_identical(
    reconciled$metadata$speaker_maps[[2]],
    c(S01 = "U0002_S01", S02 = "U0002_S02")
  )
  expect_identical(
    reconciled$metadata$boundaries[[1]]$status,
    "abstained"
  )
  expect_identical(
    reconciled$metadata$boundaries[[1]]$method,
    "no_evidence"
  )
  expect_identical(
    reconciled$metadata$boundaries[[1]]$unresolved,
    c(S01 = "U0002_S01", S02 = "U0002_S02")
  )
})

test_that("missing punctuation alone is insufficient for continuation", {
  first <- stt_reconcile_result(list(
    stt_reconcile_segment("S01", "Question?", 0, 1),
    stt_reconcile_segment("S02", "This seems complete", 8, 10)
  ))
  second <- stt_reconcile_result(list(
    stt_reconcile_segment("S01", "We should change subjects", 0, 2),
    stt_reconcile_segment("S02", "Fine.", 3, 4)
  ))

  reconciled <- genflow:::.stt_reconcile_chunk_results(list(first, second))

  expect_identical(
    reconciled$metadata$boundaries[[1]]$status,
    "abstained"
  )
  expect_identical(
    reconciled$metadata$speaker_maps[[2]],
    c(S01 = "U0002_S01", S02 = "U0002_S02")
  )
})

test_that("three-speaker chunks abstain instead of inventing a permutation", {
  first <- stt_reconcile_result(list(
    stt_reconcile_segment("S01", "Host introduction.", 0, 1),
    stt_reconcile_segment("S03", "A short aside.", 2, 3),
    stt_reconcile_segment("S02", "The unfinished result of", 8, 10)
  ))
  second <- stt_reconcile_result(list(
    stt_reconcile_segment("S01", "the experiment was surprising.", 0, 2),
    stt_reconcile_segment("S02", "I saw that.", 3, 4),
    stt_reconcile_segment("S03", "So did I.", 5, 6)
  ))

  reconciled <- genflow:::.stt_reconcile_chunk_results(list(first, second))

  expect_identical(
    reconciled$metadata$boundaries[[1]]$method,
    "unsupported_roster_size"
  )
  expect_identical(
    reconciled$metadata$speaker_maps[[2]],
    c(
      S01 = "U0002_S01",
      S02 = "U0002_S02",
      S03 = "U0002_S03"
    )
  )
  expect_identical(
    vapply(
      Filter(
        function(segment) identical(segment$chunk_index, 2L),
        reconciled$metadata$segments
      ),
      `[[`,
      character(1),
      "speaker"
    ),
    c("U0002_S01", "U0002_S02", "U0002_S03")
  )
})

test_that("stable roster can be disabled without disabling normalization", {
  first <- stt_reconcile_result(list(
    stt_reconcile_segment("S01", "First.", 0, 1),
    stt_reconcile_segment("S02", "The value of", 8, 10)
  ))
  second <- stt_reconcile_result(list(
    stt_reconcile_segment("S01", "the account rose.", 0, 1),
    stt_reconcile_segment("S02", "Yes.", 2, 3)
  ))

  reconciled <- genflow:::.stt_reconcile_chunk_results(
    list(first, second),
    stable_roster = FALSE
  )

  expect_identical(
    reconciled$metadata$boundaries[[1]]$method,
    "unstable_roster"
  )
  expect_identical(
    reconciled$metadata$speaker_maps[[2]],
    c(S01 = "U0002_S01", S02 = "U0002_S02")
  )
  expect_true(all(vapply(
    reconciled$metadata$segments,
    function(segment) {
      !is.null(segment$speaker_local) && !is.null(segment$chunk_index)
    },
    logical(1)
  )))
})

test_that("declared textual overlap maps speakers and removes one copy", {
  overlap_text <- "shared overlap words identify the exact speaker"
  first <- stt_reconcile_result(list(
    stt_reconcile_segment("S01", "An earlier answer.", 0, 3),
    stt_reconcile_segment(
      "S02",
      paste("Before the edge", overlap_text),
      6,
      10
    )
  ))
  second <- stt_reconcile_result(list(
    stt_reconcile_segment(
      "S01",
      paste0(overlap_text, ", then adds unique material."),
      0,
      4
    ),
    stt_reconcile_segment("S02", "A final response.", 5, 6)
  ))

  reconciled <- genflow:::.stt_reconcile_chunk_results(
    list(first, second),
    chunk_overlap_seconds = 5
  )

  expect_identical(
    reconciled$metadata$speaker_maps[[2]],
    c(S01 = "S02", S02 = "S01")
  )
  expect_identical(
    reconciled$metadata$boundaries[[1]]$method,
    "overlap_swap"
  )
  expect_identical(
    reconciled$metadata$boundaries[[1]]$deduplicated_tokens,
    7L
  )
  expect_true(
    reconciled$metadata$boundaries[[1]]$overlap_timing_verified
  )
  expect_false(
    reconciled$metadata$boundaries[[1]]$overlap_timing_unverified
  )
  expect_identical(
    lengths(regmatches(
      tolower(reconciled$response_value),
      gregexpr(overlap_text, tolower(reconciled$response_value), fixed = TRUE)
    )),
    1L
  )
  expect_match(
    reconciled$diarized_transcript,
    "\\[S02\\].*then adds unique material\\."
  )
  trimmed <- Filter(
    function(segment) {
      identical(segment$chunk_index, 2L) &&
        grepl("then adds unique material", segment$text, fixed = TRUE)
    },
    reconciled$metadata$segments
  )[[1]]
  expect_true(trimmed$trimmed)
  expect_identical(trimmed$trimmed_prefix_tokens, 7L)
  expect_true(trimmed$bounds_removed_after_trim)
  expect_identical(trimmed$trimmed_bounds$start, 0)
  expect_identical(trimmed$trimmed_bounds$end, 4)
  expect_null(trimmed$start)
  expect_null(trimmed$end)
  expect_null(trimmed$start_local)
  expect_null(trimmed$end_local)
})

test_that("overlap normalization ignores case and punctuation", {
  first <- stt_reconcile_result(list(
    stt_reconcile_segment("S01", "First answer.", 0, 2),
    stt_reconcile_segment(
      "S02",
      "Shared overlap, words identify exact speaker!",
      6,
      10
    )
  ))
  second <- stt_reconcile_result(list(
    stt_reconcile_segment(
      "S02",
      "shared OVERLAP words identify exact speaker; unique continuation.",
      0,
      4
    ),
    stt_reconcile_segment("S01", "Closing.", 5, 6)
  ))

  reconciled <- genflow:::.stt_reconcile_chunk_results(
    list(first, second),
    chunk_overlap_seconds = 4
  )

  expect_identical(
    reconciled$metadata$boundaries[[1]]$method,
    "overlap_identity"
  )
  expect_identical(
    reconciled$metadata$boundaries[[1]]$deduplicated_tokens,
    6L
  )
  expect_match(reconciled$response_value, "unique continuation\\.")
  expect_equal(
    lengths(regmatches(
      tolower(reconciled$response_value),
      gregexpr(
        "shared overlap",
        tolower(reconciled$response_value),
        fixed = TRUE
      )
    )),
    1L
  )
})

test_that("timing overlap maps identity despite small text differences", {
  first <- stt_reconcile_result(list(
    stt_reconcile_segment("S01", "The market outlook stays constructive.", 12, 16),
    stt_reconcile_segment("S02", "Risk remains concentrated in energy.", 16, 20)
  ), duration_seconds = 20)
  second <- stt_reconcile_result(list(
    stt_reconcile_segment("S01", "Market outlook is still constructive.", 0, 4),
    stt_reconcile_segment("S02", "The risk is concentrated in oil.", 4, 8)
  ), duration_seconds = 20)

  reconciled <- genflow:::.stt_reconcile_chunk_results(
    list(first, second),
    chunk_starts_seconds = c(100, 112),
    chunk_overlap_seconds = 8
  )
  boundary <- reconciled$metadata$boundaries[[1]]

  expect_identical(boundary$method, "timing_overlap_identity")
  expect_identical(
    reconciled$metadata$speaker_maps[[2]],
    c(S01 = "S01", S02 = "S02")
  )
  expect_equal(boundary$timing_overlap_support_seconds, 8)
  expect_equal(boundary$timing_overlap_required_support_seconds, 2.8)
  expect_identical(boundary$deduplicated_tokens, 0L)
})

test_that("timing overlap maps a chunk-local speaker swap", {
  first <- stt_reconcile_result(list(
    stt_reconcile_segment("S01", "First wording from the host.", 12, 16),
    stt_reconcile_segment("S02", "First wording from the guest.", 16, 20)
  ), duration_seconds = 20)
  second <- stt_reconcile_result(list(
    stt_reconcile_segment("S02", "A revised host transcription.", 0, 4),
    stt_reconcile_segment("S01", "A revised guest transcription.", 4, 8)
  ), duration_seconds = 20)

  reconciled <- genflow:::.stt_reconcile_chunk_results(
    list(first, second),
    chunk_overlap_seconds = 8
  )
  boundary <- reconciled$metadata$boundaries[[1]]

  expect_identical(boundary$method, "timing_overlap_swap")
  expect_identical(
    reconciled$metadata$speaker_maps[[2]],
    c(S02 = "S01", S01 = "S02")
  )
  expect_equal(boundary$timing_overlap_purity, 1)
  expect_equal(boundary$timing_overlap_margin, 1)
  expect_identical(boundary$deduplicated_tokens, 0L)
})

test_that("timing overlap abstains below its absolute and relative floor", {
  first <- stt_reconcile_result(list(
    stt_reconcile_segment("S01", "Brief first voice.", 2, 2.4),
    stt_reconcile_segment("S02", "Brief second voice.", 8, 9)
  ))
  second <- stt_reconcile_result(list(
    stt_reconcile_segment("S01", "Different first wording.", 0, 0.4),
    stt_reconcile_segment("S02", "Different second wording.", 6, 7)
  ))

  reconciled <- genflow:::.stt_reconcile_chunk_results(
    list(first, second),
    chunk_overlap_seconds = 8
  )
  boundary <- reconciled$metadata$boundaries[[1]]

  expect_identical(boundary$method, "no_evidence")
  expect_identical(boundary$timing_overlap_reason, "insufficient_support")
  expect_lt(
    boundary$timing_overlap_support_seconds,
    boundary$timing_overlap_required_support_seconds
  )
})

test_that("timing overlap abstains for a three-speaker roster", {
  first <- stt_reconcile_result(list(
    stt_reconcile_segment("S01", "Host one.", 2, 4),
    stt_reconcile_segment("S02", "Guest one.", 4, 7),
    stt_reconcile_segment("S03", "Guest two.", 7, 10)
  ))
  second <- stt_reconcile_result(list(
    stt_reconcile_segment("S01", "Host revised.", 0, 2),
    stt_reconcile_segment("S02", "Guest revised.", 2, 5),
    stt_reconcile_segment("S03", "Other guest revised.", 5, 8)
  ))

  reconciled <- genflow:::.stt_reconcile_chunk_results(
    list(first, second),
    chunk_overlap_seconds = 8
  )
  boundary <- reconciled$metadata$boundaries[[1]]

  expect_identical(boundary$method, "unsupported_roster_size")
  expect_false(boundary$timing_overlap_accepted)
  expect_identical(
    boundary$timing_overlap_reason,
    "unsupported_roster_size"
  )
})

test_that("conflicting exact-text and timing maps abstain", {
  exact <- "shared exact phrase belongs to the first speaker"
  first <- stt_reconcile_result(list(
    stt_reconcile_segment("S02", "Other left material.", 16, 20),
    stt_reconcile_segment("S01", exact, 12, 16)
  ), duration_seconds = 20)
  second <- stt_reconcile_result(list(
    stt_reconcile_segment("S02", exact, 4, 8),
    stt_reconcile_segment("S01", "Different right material.", 0, 4)
  ), duration_seconds = 20)

  reconciled <- genflow:::.stt_reconcile_chunk_results(
    list(first, second),
    chunk_overlap_seconds = 8
  )
  boundary <- reconciled$metadata$boundaries[[1]]

  expect_identical(boundary$method, "conflicting_overlap_evidence")
  expect_identical(boundary$status, "abstained")
  expect_gt(boundary$deduplicated_tokens, 0L)
})

test_that("short generic repetitions are not treated as overlap", {
  first <- stt_reconcile_result(list(
    stt_reconcile_segment("S01", "Earlier.", 0, 1),
    stt_reconcile_segment("S02", "Thank you", 8, 10)
  ))
  second <- stt_reconcile_result(list(
    stt_reconcile_segment("S01", "Thank you for returning.", 0, 2),
    stt_reconcile_segment("S02", "Sure.", 3, 4)
  ))

  reconciled <- genflow:::.stt_reconcile_chunk_results(
    list(first, second),
    chunk_overlap_seconds = 3
  )

  expect_identical(
    reconciled$metadata$boundaries[[1]]$overlap_tokens,
    0L
  )
  expect_identical(
    reconciled$metadata$boundaries[[1]]$deduplicated_tokens,
    0L
  )
  expect_equal(
    lengths(regmatches(
      tolower(reconciled$response_value),
      gregexpr("thank you", tolower(reconciled$response_value), fixed = TRUE)
    )),
    2L
  )
})

test_that("timed overlap ignores matching text outside the declared window", {
  overlap_text <- "matching words occur outside the overlap window"
  first <- stt_reconcile_result(
    list(
      stt_reconcile_segment("S01", "Earlier speaker.", 0, 2),
      stt_reconcile_segment("S02", overlap_text, 5, 10)
    ),
    duration_seconds = 20
  )
  second <- stt_reconcile_result(
    list(
      stt_reconcile_segment("S01", overlap_text, 0, 4),
      stt_reconcile_segment("S02", "New text.", 6, 8)
    ),
    duration_seconds = 20
  )

  reconciled <- genflow:::.stt_reconcile_chunk_results(
    list(first, second),
    chunk_overlap_seconds = 5
  )

  expect_true(
    reconciled$metadata$boundaries[[1]]$overlap_timing_verified
  )
  expect_false(
    reconciled$metadata$boundaries[[1]]$overlap_timing_unverified
  )
  expect_identical(
    reconciled$metadata$boundaries[[1]]$overlap_tokens,
    0L
  )
  expect_identical(
    reconciled$metadata$boundaries[[1]]$deduplicated_tokens,
    0L
  )
  expect_equal(
    lengths(regmatches(
      reconciled$response_value,
      gregexpr(overlap_text, reconciled$response_value, fixed = TRUE)
    )),
    2L
  )
})

test_that("failed overlap alignment does not use the right chunk start as a boundary", {
  first <- stt_reconcile_result(list(
    stt_reconcile_segment("S01", "Earlier observation.", 0, 2),
    stt_reconcile_segment("S02", "The value of", 8, 10)
  ))
  second <- stt_reconcile_result(list(
    stt_reconcile_segment(
      "S01",
      "the old record was already discussed.",
      0,
      2
    ),
    stt_reconcile_segment("S02", "Completely unrelated material.", 3, 5)
  ))

  reconciled <- genflow:::.stt_reconcile_chunk_results(
    list(first, second),
    chunk_overlap_seconds = 3
  )

  expect_identical(
    reconciled$metadata$boundaries[[1]]$status,
    "abstained"
  )
  expect_identical(
    reconciled$metadata$boundaries[[1]]$method,
    "no_evidence"
  )
  expect_false(
    reconciled$metadata$boundaries[[1]]$continuation_detected
  )
  expect_identical(
    reconciled$metadata$boundaries[[1]]$deduplicated_tokens,
    0L
  )
  expect_identical(
    reconciled$metadata$speaker_maps[[2]],
    c(S01 = "U0002_S01", S02 = "U0002_S02")
  )
})

test_that("overlap deduplicates content but does not map three speakers", {
  overlap_text <- "these repeated words belong to one overlap"
  first <- stt_reconcile_result(list(
    stt_reconcile_segment("S01", "Host.", 0, 1),
    stt_reconcile_segment("S03", "Guest two.", 2, 3),
    stt_reconcile_segment("S02", overlap_text, 6, 10)
  ))
  second <- stt_reconcile_result(list(
    stt_reconcile_segment(
      "S01",
      paste(overlap_text, "with unique ending."),
      0,
      3
    ),
    stt_reconcile_segment("S02", "Reply.", 4, 5),
    stt_reconcile_segment("S03", "Aside.", 6, 7)
  ))

  reconciled <- genflow:::.stt_reconcile_chunk_results(
    list(first, second),
    chunk_overlap_seconds = 5
  )

  expect_identical(
    reconciled$metadata$boundaries[[1]]$method,
    "unsupported_roster_size"
  )
  expect_gt(
    reconciled$metadata$boundaries[[1]]$deduplicated_tokens,
    0L
  )
  expect_identical(
    reconciled$metadata$speaker_maps[[2]],
    c(
      S01 = "U0002_S01",
      S02 = "U0002_S02",
      S03 = "U0002_S03"
    )
  )
  expect_equal(
    lengths(regmatches(
      reconciled$response_value,
      gregexpr(overlap_text, reconciled$response_value, fixed = TRUE)
    )),
    1L
  )
})

test_that("chunk starts rebase timestamps while preserving local bounds", {
  first <- stt_reconcile_result(list(
    stt_reconcile_segment("S01", "First.", 0, 1),
    stt_reconcile_segment("S02", "Done.", 8, 9)
  ))
  second <- stt_reconcile_result(list(
    stt_reconcile_segment("S01", "Next.", 0.5, 1.5),
    stt_reconcile_segment("S02", "End.", 2, 3)
  ))

  reconciled <- genflow:::.stt_reconcile_chunk_results(
    list(first, second),
    chunk_starts_seconds = c(0, 10),
    include_timestamps = TRUE
  )
  second_chunk <- Filter(
    function(segment) identical(segment$chunk_index, 2L),
    reconciled$metadata$segments
  )

  expect_identical(second_chunk[[1]]$start_local, 0.5)
  expect_identical(second_chunk[[1]]$end_local, 1.5)
  expect_identical(second_chunk[[1]]$start, 10.5)
  expect_identical(second_chunk[[1]]$end, 11.5)
  expect_match(reconciled$diarized_transcript, "00:00:10\\.500")
})

test_that("plain STT chunks deduplicate overlap without inventing diarization", {
  first <- stt_reconcile_result(list(
    list(text = "Opening shared overlap words remain stable.")
  ))
  second <- stt_reconcile_result(list(
    list(text = "shared overlap words remain stable. Unique ending.")
  ))

  reconciled <- genflow:::.stt_reconcile_chunk_results(
    list(first, second),
    chunk_overlap_seconds = 3
  )

  expect_false("diarized_transcript" %in% names(reconciled))
  expect_identical(
    reconciled$response_value,
    "Opening shared overlap words remain stable. Unique ending."
  )
  expect_identical(
    reconciled$metadata$boundaries[[1]]$deduplicated_tokens,
    5L
  )
  expect_false(
    reconciled$metadata$boundaries[[1]]$overlap_timing_verified
  )
  expect_true(
    reconciled$metadata$boundaries[[1]]$overlap_timing_unverified
  )
})

test_that("reconciliation validates boundary configuration", {
  result <- stt_reconcile_result(list(
    stt_reconcile_segment("S01", "Only.", 0, 1)
  ))

  expect_error(
    genflow:::.stt_reconcile_chunk_results(
      list(result, result),
      chunk_starts_seconds = 0
    ),
    "one non-negative value per chunk"
  )
  expect_error(
    genflow:::.stt_reconcile_chunk_results(
      list(result, result, result),
      chunk_overlap_seconds = c(1, -1)
    ),
    "non-negative"
  )
  expect_error(
    genflow:::.stt_reconcile_chunk_results(list()),
    "non-empty list"
  )
})
