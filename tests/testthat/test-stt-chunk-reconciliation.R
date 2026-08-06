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

test_that("one input keeps recording-scoped provider speaker labels", {
  result <- stt_reconcile_result(list(
    stt_reconcile_segment("Speaker 1", "Opening statement.", 0, 2),
    stt_reconcile_segment("S02", "A response.", 2, 4)
  ))

  merged <- genflow:::.stt_reconcile_chunk_results(list(result))
  segments <- merged$metadata$segments

  expect_identical(
    vapply(segments, `[[`, character(1), "speaker"),
    c("S01", "S02")
  )
  expect_true(all(vapply(
    segments,
    function(segment) identical(segment$speaker_scope, "recording"),
    logical(1)
  )))
  expect_identical(merged$metadata$chunk_merge$speaker_scope, "recording")
  expect_false(merged$metadata$chunk_merge$cross_chunk_identity_tracking)
})

test_that("multiple inputs keep honest chunk-local speaker namespaces", {
  first <- stt_reconcile_result(list(
    stt_reconcile_segment("S01", "First host turn.", 0, 2),
    stt_reconcile_segment("S02", "First guest turn.", 2, 4)
  ))
  second <- stt_reconcile_result(list(
    stt_reconcile_segment("S01", "A locally numbered voice.", 0, 2),
    stt_reconcile_segment("S02", "Another local voice.", 2, 4)
  ))

  merged <- genflow:::.stt_reconcile_chunk_results(list(first, second))
  segments <- merged$metadata$segments

  expect_identical(
    vapply(segments, `[[`, character(1), "speaker"),
    c("C01:S01", "C01:S02", "C02:S01", "C02:S02")
  )
  expect_identical(
    vapply(segments, `[[`, character(1), "speaker_local"),
    c("S01", "S02", "S01", "S02")
  )
  expect_identical(
    merged$metadata$chunk_merge$chunk_speaker_labels,
    list(
      c(S01 = "C01:S01", S02 = "C01:S02"),
      c(S01 = "C02:S01", S02 = "C02:S02")
    )
  )
  expect_match(merged$diarized_transcript, "\\[C01:S01\\]")
  expect_match(merged$diarized_transcript, "\\[C02:S01\\]")
})

test_that("sequential merge never removes repeated boundary text", {
  first <- stt_reconcile_result(list(
    stt_reconcile_segment("S01", "Repeated boundary sentence.", 8, 10)
  ))
  second <- stt_reconcile_result(list(
    stt_reconcile_segment("S01", "Repeated boundary sentence.", 0, 2)
  ))

  merged <- genflow:::.stt_reconcile_chunk_results(list(first, second))

  expect_identical(
    merged$response_value,
    "Repeated boundary sentence. Repeated boundary sentence."
  )
  expect_identical(merged$metadata$chunk_merge$order, "source")
  expect_identical(merged$metadata$chunk_merge$overlap_seconds, 0)
  expect_false(merged$metadata$chunk_merge$text_deduplication)
})

test_that("chunk-local timestamps are rebased to the recording timeline", {
  first <- stt_reconcile_result(list(
    stt_reconcile_segment("S01", "First.", 1, 2)
  ))
  second <- stt_reconcile_result(list(
    stt_reconcile_segment("S01", "Second.", 1.5, 3)
  ))

  merged <- genflow:::.stt_reconcile_chunk_results(
    list(first, second),
    chunk_starts_seconds = c(0, 10),
    include_timestamps = TRUE
  )
  second_segment <- merged$metadata$segments[[2]]

  expect_equal(second_segment$start_local, 1.5)
  expect_equal(second_segment$end_local, 3)
  expect_equal(second_segment$start, 11.5)
  expect_equal(second_segment$end, 13)
  expect_match(merged$diarized_transcript, "00:00:11.500")
})

test_that("plain chunk results merge without manufacturing diarization", {
  result <- function(text) {
    list(
      response_value = text,
      service = "local-native",
      model = "plain.gguf",
      duration = 1,
      status_api = "SUCCESS",
      status_msg = "OK",
      audio = tempfile(fileext = ".wav"),
      metadata = list(input_duration_seconds = 10)
    )
  }

  merged <- genflow:::.stt_reconcile_chunk_results(list(
    result("First plain chunk."),
    result("Second plain chunk.")
  ))

  expect_identical(
    merged$response_value,
    "First plain chunk. Second plain chunk."
  )
  expect_null(merged$diarized_transcript)
  expect_identical(merged$metadata$chunk_merge$speaker_scope, "none")
})

test_that("failed source chunks remain failed after sequential merge", {
  first <- stt_reconcile_result(list(
    stt_reconcile_segment("S01", "Complete chunk.", 0, 1)
  ))
  second <- stt_reconcile_result(
    list(stt_reconcile_segment("S01", "Failed chunk.", 0, 1)),
    status_api = "ERROR"
  )

  merged <- genflow:::.stt_reconcile_chunk_results(list(first, second))

  expect_identical(merged$status_api, "ERROR")
  expect_match(merged$status_msg, "failed STT chunk\\(s\\): 2")
})

test_that("chunk-local summaries never claim global speaker identity", {
  first <- stt_reconcile_result(list(
    stt_reconcile_segment("S01", "One.", 0, 1),
    stt_reconcile_segment("S02", "Two.", 1, 2)
  ))
  second <- stt_reconcile_result(list(
    stt_reconcile_segment("S01", "Three.", 0, 1),
    stt_reconcile_segment("S02", "Four.", 1, 2)
  ))
  merged <- genflow:::.stt_reconcile_chunk_results(list(first, second))

  summary <- genflow:::.stt_diarization_summary(merged$metadata$segments)

  expect_identical(summary$speaker_scope, "chunk-local")
  expect_identical(summary$speaker_count, 4L)
  expect_identical(summary$chunk_count, 2L)
  expect_false(summary$cross_chunk_identity_tracking)
  expect_null(summary$cross_chunk_identity_method)
})

test_that("chunk merge validates only sequential inputs and start offsets", {
  result <- stt_reconcile_result(list(
    stt_reconcile_segment("S01", "A result.", 0, 1)
  ))

  expect_error(
    genflow:::.stt_reconcile_chunk_results(list()),
    "non-empty list",
    fixed = TRUE
  )
  expect_error(
    genflow:::.stt_reconcile_chunk_results(list("invalid")),
    "Every entry",
    fixed = TRUE
  )
  expect_error(
    genflow:::.stt_reconcile_chunk_results(
      list(result, result),
      chunk_starts_seconds = 0
    ),
    "one non-negative value per chunk",
    fixed = TRUE
  )
  expect_error(
    genflow:::.stt_reconcile_chunk_results(
      list(result),
      include_timestamps = NA
    ),
    "must be TRUE or FALSE",
    fixed = TRUE
  )
})
