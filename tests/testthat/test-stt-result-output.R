stt_output_fixture <- function(status = "SUCCESS",
                               response = "hello from speech",
                               status_msg = "OK",
                               saved_file = "/tmp/sample.txt") {
  list(
    response_value = if (identical(status, "SUCCESS")) response else NULL,
    label = "sample",
    label_cat = "sample",
    service = "local-native",
    model = "auto",
    duration = 1.25,
    status_api = status,
    status_msg = status_msg,
    saved_file = saved_file,
    audio = "/tmp/sample.wav",
    content_type = "text",
    metadata = list(
      engine = "crispasr",
      segments = list(list(text = response))
    )
  )
}

test_that("STT reports generation status without changing the result object", {
  result <- stt_output_fixture()
  serialized <- serialize(result, NULL)

  output <- paste(
    capture.output(reported <- genflow:::.stt_report_result(result)),
    collapse = "\n"
  )

  expect_match(
    output,
    "[SUCCESS] sample | local-native | auto | Time: 1.25s",
    fixed = TRUE
  )
  expect_match(output, "-> File: sample.txt", fixed = TRUE)
  expect_match(output, "-> Response: hello from speech...", fixed = TRUE)
  expect_identical(reported, result)
  expect_identical(serialize(result, NULL), serialized)
  expect_null(attr(result, "class", exact = TRUE))

  printed <- paste(capture.output(print(result)), collapse = "\n")
  expect_match(printed, "$response_value", fixed = TRUE)
  expect_match(printed, "$metadata", fixed = TRUE)
  expect_false(grepl("<genflow_stt_result>", printed, fixed = TRUE))
})

test_that("STT reports errors through the same concise call-time format", {
  result <- stt_output_fixture(
    status = "ERROR",
    response = "",
    status_msg = "native engine failed",
    saved_file = NA_character_
  )

  output <- paste(
    capture.output(genflow:::.stt_report_result(result)),
    collapse = "\n"
  )

  expect_match(
    output,
    "[ERROR] sample | local-native | auto | Time: 1.25s",
    fixed = TRUE
  )
  expect_match(output, "-> Response: native engine failed...", fixed = TRUE)
  expect_false(grepl("-> File:", output, fixed = TRUE))
})
