stt_large_audio_file <- function(bytes = 256L, extension = ".wav") {
  path <- tempfile("genflow-large-audio-", fileext = extension)
  writeBin(as.raw(rep(1L, bytes)), path)
  path
}

stt_large_result <- function(text,
                             speaker = "S01",
                             start = 0,
                             end = 1,
                             native_kv_quant = NULL,
                             native_kv_quant_source = NULL) {
  metadata <- list(
    segments = list(list(
      text = text,
      speaker = speaker,
      start = start,
      end = end
    ))
  )
  if (!is.null(native_kv_quant)) {
    metadata$native_kv_quant <- native_kv_quant
  }
  if (!is.null(native_kv_quant_source)) {
    metadata$native_kv_quant_source <- native_kv_quant_source
  }
  list(
    response_value = text,
    service = "local-native",
    model = "mock.gguf",
    duration = 0.1,
    status_api = "SUCCESS",
    status_msg = "OK",
    saved_file = NA_character_,
    audio = "part.wav",
    content_type = "text",
    metadata = metadata
  )
}

stt_large_plan_fixture <- function(directory) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  audio_paths <- file.path(directory, c("part_0001.wav", "part_0002.wav"))
  lapply(audio_paths, function(path) {
    writeBin(as.raw(rep(c(1, 2, 3), length.out = 9000L)), path)
  })
  result_paths <- file.path(
    directory,
    c("part_0001.result.rds", "part_0002.result.rds")
  )
  parts <- lapply(seq_along(audio_paths), function(index) {
    list(
      index = as.integer(index),
      audio_path = audio_paths[[index]],
      audio_fingerprint = genflow:::.stt_chunk_file_fingerprint(
        audio_paths[[index]]
      ),
      start_seconds = c(0, 10)[[index]],
      duration_seconds = 10,
      requested_duration_seconds = 10,
      size_bytes = as.numeric(file.info(audio_paths[[index]])$size[[1]]),
      result_path = result_paths[[index]],
      status = "pending",
      attempts = 0L,
      last_error = ""
    )
  })
  manifest <- list(
    schema_version = 3L,
    key = "fixture",
    config_fingerprint = "fixture-config",
    parts = parts
  )
  manifest_path <- file.path(directory, "manifest.rds")
  genflow:::.stt_chunk_write_manifest(manifest, manifest_path)
  list(
    chunked = TRUE,
    audio_path = audio_paths[[1]],
    cleanup_dir = NULL,
    prepared = TRUE,
    chunk_format = "wav",
    prepared_format = "wav",
    prepared_size_bytes = 6,
    input_duration_seconds = 20,
    segment_seconds = 10,
    decision_reason = "requested-segment",
    parts = parts,
    manifest = manifest,
    manifest_path = manifest_path,
    checkpoint_dir = directory
  )
}

test_that("large-audio public controls validate without touching a backend", {
  public_formals <- names(formals(genflow:::gen_stt.default))
  expect_true(all(c(
    "chunking", "chunk_bitrate_kbps", "chunk_segment_seconds",
    "checkpoint_dir", "resume", "chunk_retry_forever", "chunk_max_retries",
    "chunk_retry_wait_seconds", "output", "chunk_format",
    "checkpoint_retention"
  ) %in% public_formals))
  expect_false(any(c(
    "chunk_max_mb", "chunk_overlap_seconds", "chunk_speaker_linking",
    "diarize_speakers", "diarize_embedder"
  ) %in% public_formals))
  expect_identical(utils::tail(public_formals, 1L), "...")

  expect_error(
    genflow:::.stt_chunk_validate_options(chunk_segment_seconds = 0),
    "greater than 0"
  )
  expect_error(
    genflow:::.stt_chunk_validate_options(resume = NA),
    "must be TRUE or FALSE"
  )
  expect_error(
    genflow:::.stt_chunk_validate_options(chunk_max_retries = 1.5),
    "whole number"
  )
  expect_error(
    genflow:::.stt_chunk_validate_options(chunk_format = "flac"),
    "arg"
  )
  expect_error(
    genflow:::.stt_chunk_validate_options(checkpoint_retention = "none"),
    "arg"
  )

  options <- genflow:::.stt_chunk_validate_options(
    chunking = "auto",
    chunk_bitrate_kbps = "48",
    chunk_segment_seconds = "300",
    chunk_format = "mp3",
    checkpoint_retention = "results",
    output = "transcript"
  )
  expect_identical(options$chunking, "auto")
  expect_identical(options$chunk_bitrate_kbps, 48L)
  expect_identical(options$chunk_segment_seconds, 300)
  expect_identical(options$chunk_format, "mp3")
  expect_identical(options$checkpoint_retention, "results")
  expect_identical(options$output, "transcript")
  expect_equal(
    genflow:::.stt_chunk_starts(1000, 300),
    c(0, 300, 600, 900)
  )
})

test_that("chunk format auto preserves defaults and explicit MP3 reaches native planning", {
  expect_identical(
    genflow:::.stt_chunk_resolve_format("local-native", "auto"),
    "wav"
  )
  expect_identical(
    genflow:::.stt_chunk_resolve_format("openai", "auto"),
    "mp3"
  )

  source <- stt_large_audio_file()
  checkpoint <- tempfile("genflow-native-mp3-checkpoint-")
  on.exit(unlink(c(source, checkpoint), recursive = TRUE), add = TRUE)
  formats <- character()

  testthat::local_mocked_bindings(
    .stt_chunk_prepare_media = function(source,
                                        target,
                                        format,
                                        bitrate_kbps) {
      formats <<- c(formats, format)
      writeBin(as.raw(rep(2L, 1000L)), target)
      invisible(target)
    },
    .stt_chunk_extract_media = function(source,
                                        target,
                                        start_seconds,
                                        duration_seconds,
                                        format,
                                        bitrate_kbps) {
      formats <<- c(formats, format)
      writeBin(as.raw(rep(3L, 100L)), target)
      invisible(target)
    },
    .stt_audio_duration_seconds = function(path) 100,
    .package = "genflow"
  )

  options <- genflow:::.stt_chunk_validate_options(
    chunk_segment_seconds = 30,
    chunk_format = "mp3",
    checkpoint_dir = checkpoint
  )
  plan <- genflow:::.stt_chunk_plan_audio(
    source,
    service = "local-native",
    config_fingerprint = "native-mp3-config",
    options = options
  )
  on.exit(genflow:::.stt_chunk_release_lock(plan$lock), add = TRUE)

  expect_true(plan$chunked)
  expect_identical(plan$chunk_format, "mp3")
  expect_identical(plan$prepared_format, "mp3")
  expect_true(all(formats == "mp3"))
  expect_identical(readRDS(plan$manifest_path)$prepared_format, "mp3")
})

test_that("moss-transcribe rejects explicit MP3 chunks before inference", {
  expect_error(
    genflow:::.stt_chunk_validate_native_format(
      service = "local-native",
      format = "mp3",
      engine = "moss-transcribe"
    ),
    "requires WAV"
  )
  expect_invisible(
    genflow:::.stt_chunk_validate_native_format(
      service = "local-native",
      format = "mp3",
      engine = "crispasr"
    )
  )
})

test_that("chunk planning prepares native WAV and validates every chunk", {
  source <- stt_large_audio_file()
  checkpoint <- tempfile("genflow-large-checkpoint-")
  on.exit(unlink(c(source, checkpoint), recursive = TRUE), add = TRUE)
  prepared_formats <- character()
  extracted <- list()

  testthat::local_mocked_bindings(
    .stt_chunk_prepare_media = function(source,
                                        target,
                                        format,
                                        bitrate_kbps) {
      prepared_formats <<- c(prepared_formats, format)
      writeBin(as.raw(rep(2L, 1000L)), target)
      invisible(target)
    },
    .stt_chunk_extract_media = function(source,
                                        target,
                                        start_seconds,
                                        duration_seconds,
                                        format,
                                        bitrate_kbps) {
      extracted[[length(extracted) + 1L]] <<- list(
        start = start_seconds,
        duration = duration_seconds,
        format = format
      )
      writeBin(as.raw(rep(3L, 100L)), target)
      invisible(target)
    },
    .stt_audio_duration_seconds = function(path) 100,
    .package = "genflow"
  )

  options <- genflow:::.stt_chunk_validate_options(
    chunk_segment_seconds = 30,
    checkpoint_dir = checkpoint
  )
  plan <- genflow:::.stt_chunk_plan_audio(
    source,
    service = "local-native",
    config_fingerprint = "config",
    options = options
  )
  on.exit(genflow:::.stt_chunk_release_lock(plan$lock), add = TRUE)

  expect_true(plan$chunked)
  expect_identical(
    genflow:::.stt_chunk_lock_state(plan$lock$path)$state,
    "active"
  )
  expect_identical(plan$prepared_format, "wav")
  expect_identical(prepared_formats, "wav")
  expect_gt(length(plan$parts), 1L)
  expect_equal(
    vapply(extracted, `[[`, character(1), "format"),
    rep("wav", length(extracted))
  )
  expect_true(all(vapply(
    plan$parts,
    function(part) genflow:::.stt_chunk_nonempty_file(part$audio_path),
    logical(1)
  )))
  expect_true(file.exists(plan$manifest_path))
})

test_that("no requested duration leaves audio and checkpoint storage untouched", {
  source <- stt_large_audio_file()
  checkpoint <- tempfile("genflow-prepared-run-")
  on.exit(unlink(c(source, checkpoint), recursive = TRUE), add = TRUE)
  preparations <- 0L

  testthat::local_mocked_bindings(
    .stt_chunk_prepare_media = function(source,
                                        target,
                                        format,
                                        bitrate_kbps) {
      preparations <<- preparations + 1L
      stop("unexpected preparation")
    },
    .stt_audio_duration_seconds = function(path) 10,
    .package = "genflow"
  )
  plan <- genflow:::.stt_chunk_plan_audio(
    source,
    service = "local-native",
    config_fingerprint = "prepared-run",
    options = genflow:::.stt_chunk_validate_options(
      checkpoint_dir = checkpoint
    )
  )
  expect_false(plan$chunked)
  expect_identical(plan$audio_path, source)
  expect_identical(plan$decision_reason, "chunk-duration-not-requested")
  expect_identical(preparations, 0L)
  expect_false(dir.exists(checkpoint))
})

test_that("successful opaque chunk results resume without backend calls", {
  checkpoint <- tempfile("genflow-large-resume-")
  on.exit(unlink(checkpoint, recursive = TRUE), add = TRUE)
  plan <- stt_large_plan_fixture(checkpoint)
  calls <- 0L

  testthat::local_mocked_bindings(
    .stt_chunk_call_backend = function(audio, arguments) {
      calls <<- calls + 1L
      expect_identical(arguments$chunking, "never")
      expect_false(arguments$save_txt)
      expect_identical(arguments$output, "full")
      if (calls == 1L) {
        stt_large_result("The first sentence is", "S02", 0, 10)
      } else {
        stt_large_result("continued here.", "S01", 0, 10)
      }
    },
    .package = "genflow"
  )
  options <- genflow:::.stt_chunk_validate_options(
    checkpoint_dir = checkpoint,
    chunk_retry_wait_seconds = 0
  )
  call_arguments <- list(
    service = "local-native",
    model = "mock.gguf",
    label = "meeting"
  )

  first <- genflow:::.stt_chunk_transcribe_parts(
    plan,
    call_arguments,
    options,
    timestamps = FALSE
  )
  expect_identical(calls, 2L)
  expect_match(first$text, "continued here", fixed = TRUE)
  expect_identical(first$metadata$chunking$resumed_part_count, 0L)

  plan$manifest <- readRDS(plan$manifest_path)
  second <- genflow:::.stt_chunk_transcribe_parts(
    plan,
    call_arguments,
    options,
    timestamps = FALSE
  )
  expect_identical(calls, 2L)
  expect_identical(second$text, first$text)
  expect_identical(second$metadata$chunking$resumed_part_count, 2L)
})

test_that("result checkpoints recover the crash window and bind to audio", {
  checkpoint <- tempfile("genflow-large-crash-window-")
  on.exit(unlink(checkpoint, recursive = TRUE), add = TRUE)
  plan <- stt_large_plan_fixture(checkpoint)
  plan$manifest$parts <- plan$manifest$parts[1]
  plan$parts <- plan$parts[1]
  genflow:::.stt_chunk_write_manifest(plan$manifest, plan$manifest_path)
  cached_result <- stt_large_result("Recovered without rerunning.", "S01")
  envelope <- genflow:::.stt_chunk_result_checkpoint(
    result = cached_result,
    status = "done",
    manifest = plan$manifest,
    part = plan$manifest$parts[[1]],
    attempts = 1L
  )
  genflow:::.genflow_atomic_save_rds(
    envelope,
    plan$manifest$parts[[1]]$result_path
  )
  calls <- 0L
  testthat::local_mocked_bindings(
    .stt_chunk_call_backend = function(audio, arguments) {
      calls <<- calls + 1L
      stt_large_result("Unexpected rerun.", "S01")
    },
    .package = "genflow"
  )
  options <- genflow:::.stt_chunk_validate_options(
    checkpoint_dir = checkpoint,
    chunk_retry_wait_seconds = 0
  )
  recovered <- genflow:::.stt_chunk_transcribe_parts(
    plan,
    list(service = "local-native", model = "mock.gguf", label = "recover"),
    options
  )
  expect_identical(calls, 0L)
  expect_identical(recovered$text, "Recovered without rerunning.")
  expect_identical(recovered$metadata$chunking$resumed_part_count, 1L)
  expect_identical(
    readRDS(plan$manifest_path)$parts[[1]]$status,
    "done"
  )

  plan <- stt_large_plan_fixture(tempfile("genflow-large-invalid-envelope-"))
  on.exit(unlink(dirname(plan$manifest_path), recursive = TRUE), add = TRUE)
  plan$manifest$parts <- plan$manifest$parts[1]
  plan$parts <- plan$parts[1]
  genflow:::.stt_chunk_write_manifest(plan$manifest, plan$manifest_path)
  envelope <- genflow:::.stt_chunk_result_checkpoint(
    result = cached_result,
    status = "done",
    manifest = plan$manifest,
    part = plan$manifest$parts[[1]],
    attempts = 1L
  )
  envelope$audio_fingerprint <- "different-audio"
  genflow:::.genflow_atomic_save_rds(
    envelope,
    plan$manifest$parts[[1]]$result_path
  )
  calls <- 0L
  rerun <- genflow:::.stt_chunk_transcribe_parts(
    plan,
    list(service = "local-native", model = "mock.gguf", label = "rerun"),
    options
  )
  expect_identical(calls, 1L)
  expect_identical(rerun$text, "Unexpected rerun.")
})

test_that("prepared media and chunks are reused only after full validation", {
  source <- stt_large_audio_file()
  checkpoint <- tempfile("genflow-large-validation-")
  on.exit(unlink(c(source, checkpoint), recursive = TRUE), add = TRUE)
  prepare_calls <- 0L
  extract_calls <- 0L

  testthat::local_mocked_bindings(
    .stt_chunk_prepare_media = function(source,
                                        target,
                                        format,
                                        bitrate_kbps) {
      prepare_calls <<- prepare_calls + 1L
      writeBin(as.raw(rep(2L, 1000L)), target)
      invisible(target)
    },
    .stt_chunk_extract_media = function(source,
                                        target,
                                        start_seconds,
                                        duration_seconds,
                                        format,
                                        bitrate_kbps) {
      extract_calls <<- extract_calls + 1L
      writeBin(as.raw(rep(3L, 100L)), target)
      invisible(target)
    },
    .stt_audio_duration_seconds = function(path) {
      if (grepl("part_", basename(path), fixed = TRUE)) 10 else 100
    },
    .package = "genflow"
  )
  options <- genflow:::.stt_chunk_validate_options(
    chunk_segment_seconds = 30,
    checkpoint_dir = checkpoint
  )
  first <- genflow:::.stt_chunk_plan_audio(
    source,
    service = "local-native",
    config_fingerprint = "validation-config",
    options = options
  )
  initial_extracts <- extract_calls
  expect_identical(prepare_calls, 1L)
  expect_gt(initial_extracts, 1L)
  expect_true(genflow:::.stt_chunk_release_lock(first$lock))

  second <- genflow:::.stt_chunk_plan_audio(
    source,
    service = "local-native",
    config_fingerprint = "validation-config",
    options = options
  )
  expect_identical(prepare_calls, 1L)
  expect_identical(extract_calls, initial_extracts)
  expect_identical(
    vapply(first$parts, `[[`, character(1), "audio_fingerprint"),
    vapply(second$parts, `[[`, character(1), "audio_fingerprint")
  )
  expect_true(genflow:::.stt_chunk_release_lock(second$lock))

  writeBin(as.raw(9L), second$audio_path, useBytes = TRUE)
  third <- genflow:::.stt_chunk_plan_audio(
    source,
    service = "local-native",
    config_fingerprint = "validation-config",
    options = options
  )
  expect_identical(prepare_calls, 2L)
  expect_identical(extract_calls, initial_extracts)
  expect_true(genflow:::.stt_chunk_release_lock(third$lock))

  writeBin(as.raw(9L), third$parts[[1]]$audio_path, useBytes = TRUE)
  fourth <- genflow:::.stt_chunk_plan_audio(
    source,
    service = "local-native",
    config_fingerprint = "validation-config",
    options = options
  )
  expect_identical(prepare_calls, 2L)
  expect_identical(extract_calls, initial_extracts + 1L)
  expect_true(genflow:::.stt_chunk_release_lock(fourth$lock))
})

test_that("recognized empty output is accepted only for a tiny final tail", {
  checkpoint <- tempfile("genflow-large-tiny-tail-")
  on.exit(unlink(checkpoint, recursive = TRUE), add = TRUE)
  plan <- stt_large_plan_fixture(checkpoint)
  plan$manifest$parts[[2]]$duration_seconds <- 0.5
  plan$parts <- plan$manifest$parts
  genflow:::.stt_chunk_write_manifest(plan$manifest, plan$manifest_path)
  calls <- 0L
  testthat::local_mocked_bindings(
    .stt_chunk_call_backend = function(audio, arguments) {
      calls <<- calls + 1L
      if (calls == 1L) {
        return(stt_large_result(
          "Useful transcript.",
          "S01",
          native_kv_quant = "q8_0",
          native_kv_quant_source = "model-default"
        ))
      }
      list(
        response_value = NULL,
        status_api = "ERROR",
        status_msg = "Provider returned an empty transcript."
      )
    },
    .package = "genflow"
  )
  options <- genflow:::.stt_chunk_validate_options(
    checkpoint_dir = checkpoint,
    chunk_retry_wait_seconds = 0,
    chunk_retry_forever = FALSE,
    chunk_max_retries = 1
  )
  result <- genflow:::.stt_chunk_transcribe_parts(
    plan,
    list(service = "local-native", model = "mock.gguf", label = "tiny"),
    options
  )
  expect_identical(result$text, "Useful transcript.")
  expect_identical(result$metadata$native_kv_quant, "q8_0")
  expect_identical(
    result$metadata$native_kv_quant_source,
    "model-default"
  )
  expect_identical(
    readRDS(plan$manifest_path)$parts[[2]]$status,
    "done_empty"
  )

  plan <- stt_large_plan_fixture(tempfile("genflow-large-normal-empty-"))
  on.exit(unlink(dirname(plan$manifest_path), recursive = TRUE), add = TRUE)
  plan$manifest$parts <- plan$manifest$parts[1]
  plan$parts <- plan$parts[1]
  genflow:::.stt_chunk_write_manifest(plan$manifest, plan$manifest_path)
  testthat::local_mocked_bindings(
    .stt_chunk_call_backend = function(audio, arguments) {
      list(
        response_value = NULL,
        status_api = "ERROR",
        status_msg = "Provider returned an empty transcript."
      )
    },
    .package = "genflow"
  )
  expect_error(
    genflow:::.stt_chunk_transcribe_parts(
      plan,
      list(service = "local-native", model = "mock.gguf", label = "normal"),
      options
    ),
    "failed after 1 attempt"
  )
})

test_that("chunk retry distinguishes transient and permanent failures", {
  checkpoint <- tempfile("genflow-large-retry-")
  on.exit(unlink(checkpoint, recursive = TRUE), add = TRUE)
  plan <- stt_large_plan_fixture(checkpoint)
  plan$manifest$parts <- plan$manifest$parts[1]
  plan$parts <- plan$parts[1]
  genflow:::.stt_chunk_write_manifest(plan$manifest, plan$manifest_path)
  attempts <- 0L

  testthat::local_mocked_bindings(
    .stt_chunk_call_backend = function(audio, arguments) {
      attempts <<- attempts + 1L
      if (attempts <= 2L) {
        return(list(
          response_value = NULL,
          status_api = "ERROR",
          status_msg = "temporary network timeout"
        ))
      }
      stt_large_result("Recovered.", "S01")
    },
    .stt_chunk_sleep = function(seconds) invisible(NULL),
    .package = "genflow"
  )
  options <- genflow:::.stt_chunk_validate_options(
    checkpoint_dir = checkpoint,
    chunk_retry_wait_seconds = 0,
    chunk_retry_forever = FALSE,
    chunk_max_retries = 2
  )
  result <- genflow:::.stt_chunk_transcribe_parts(
    plan,
    list(service = "local-native", model = "mock.gguf", label = "retry"),
    options
  )
  expect_identical(attempts, 3L)
  expect_identical(result$text, "Recovered.")

  plan <- stt_large_plan_fixture(tempfile("genflow-large-permanent-"))
  on.exit(unlink(dirname(plan$manifest_path), recursive = TRUE), add = TRUE)
  plan$manifest$parts <- plan$manifest$parts[1]
  plan$parts <- plan$parts[1]
  genflow:::.stt_chunk_write_manifest(plan$manifest, plan$manifest_path)
  testthat::local_mocked_bindings(
    .stt_chunk_call_backend = function(audio, arguments) {
      list(
        response_value = NULL,
        status_api = "ERROR",
        status_msg = "unsupported model"
      )
    },
    .package = "genflow"
  )
  expect_error(
    genflow:::.stt_chunk_transcribe_parts(
      plan,
      list(service = "local-native", model = "bad.gguf", label = "bad"),
      options
    ),
    "after 1 attempt"
  )
})

test_that("temporary planning artifacts are removed after preparation failure", {
  source <- stt_large_audio_file()
  root <- tempfile("genflow-large-failed-plan-")
  on.exit(unlink(c(source, root), recursive = TRUE), add = TRUE)
  testthat::local_mocked_bindings(
    .stt_chunk_temp_root = function() root,
    .stt_audio_duration_seconds = function(path) 100,
    .stt_chunk_prepare_media = function(...) {
      stop("synthetic preparation failure", call. = FALSE)
    },
    .package = "genflow"
  )
  options <- genflow:::.stt_chunk_validate_options(
    chunk_segment_seconds = 30
  )
  expect_error(
    genflow:::.stt_chunk_plan_audio(
      source,
      service = "local-native",
      config_fingerprint = "cleanup-config",
      options = options
    ),
    "synthetic preparation failure"
  )
  expect_false(dir.exists(root))
})

test_that("ffmpeg arguments quote paths containing spaces", {
  seen <- NULL
  raw_args <- c(
    "-nostdin", "-i", "/tmp/input audio.wav",
    "-t", "1", "/tmp/output audio.wav"
  )
  testthat::local_mocked_bindings(
    .stt_chunk_ffmpeg = function() "/tmp/ffmpeg executable",
    .stt_chunk_system2 = function(command, args) {
      seen <<- list(command = command, args = args)
      character()
    },
    .package = "genflow"
  )
  genflow:::.stt_chunk_run_ffmpeg(raw_args)
  expect_identical(seen$command, "/tmp/ffmpeg executable")
  expect_identical(seen$args, vapply(raw_args, shQuote, character(1)))
  expect_match(seen$args[[3]], "input audio", fixed = TRUE)
})

test_that("remote MP3 preparation encodes once and chunks use stream copy", {
  source <- stt_large_audio_file(extension = ".m4a")
  directory <- tempfile("genflow-single-encode-")
  dir.create(directory, recursive = TRUE)
  prepared <- file.path(directory, "prepared.mp3")
  part <- file.path(directory, "part_0001.mp3")
  on.exit(unlink(c(source, directory), recursive = TRUE), add = TRUE)
  calls <- list()

  testthat::local_mocked_bindings(
    .stt_chunk_run_ffmpeg = function(args) {
      calls[[length(calls) + 1L]] <<- args
      writeBin(as.raw(c(1, 2, 3)), tail(args, 1L))
      invisible(character())
    },
    .package = "genflow"
  )
  genflow:::.stt_chunk_prepare_media(
    source,
    prepared,
    format = "mp3",
    bitrate_kbps = 48
  )
  genflow:::.stt_chunk_extract_media(
    prepared,
    part,
    start_seconds = 10,
    duration_seconds = 20,
    format = "mp3",
    bitrate_kbps = 48
  )

  expect_identical(length(calls), 2L)
  expect_true("libmp3lame" %in% calls[[1]])
  expect_false("copy" %in% calls[[1]])
  expect_true("copy" %in% calls[[2]])
  expect_false("libmp3lame" %in% calls[[2]])
  expect_false("-ac" %in% calls[[2]])
  expect_false("-ar" %in% calls[[2]])
})

test_that("checkpoint locks reject live owners and recover dead owners", {
  run_dir <- tempfile("genflow-lock-run-")
  dir.create(run_dir, recursive = TRUE)
  on.exit(unlink(run_dir, recursive = TRUE), add = TRUE)

  first <- genflow:::.stt_chunk_acquire_lock(run_dir)
  owner <- readRDS(file.path(first$path, "owner.rds"))
  expect_identical(owner$pid, as.integer(Sys.getpid()))
  expect_identical(owner$token, first$token)
  expect_error(
    genflow:::.stt_chunk_acquire_lock(run_dir),
    "already in use.*owner PID"
  )

  owner$pid <- 99999999L
  owner$created_at <- as.numeric(Sys.time()) - 24 * 60 * 60
  genflow:::.genflow_atomic_save_rds(
    owner,
    file.path(first$path, "owner.rds")
  )
  testthat::local_mocked_bindings(
    .stt_chunk_process_exists = function(pid) FALSE,
    .package = "genflow"
  )
  recovered <- genflow:::.stt_chunk_acquire_lock(run_dir)
  expect_false(identical(recovered$token, first$token))
  expect_false(genflow:::.stt_chunk_release_lock(first))
  expect_true(dir.exists(recovered$path))
  expect_true(genflow:::.stt_chunk_release_lock(recovered))
})

test_that("checkpoint lock release never removes another owner's lock", {
  run_dir <- tempfile("genflow-lock-token-")
  dir.create(run_dir, recursive = TRUE)
  on.exit(unlink(run_dir, recursive = TRUE), add = TRUE)
  lock <- genflow:::.stt_chunk_acquire_lock(run_dir)
  owner_path <- file.path(lock$path, "owner.rds")
  owner <- readRDS(owner_path)
  owner$token <- "replacement-owner-token"
  genflow:::.genflow_atomic_save_rds(owner, owner_path)

  expect_false(genflow:::.stt_chunk_release_lock(lock))
  expect_true(dir.exists(lock$path))
})

test_that("foreign-host locks are never reclaimed automatically", {
  run_dir <- tempfile("genflow-lock-age-")
  dir.create(run_dir, recursive = TRUE)
  on.exit(unlink(run_dir, recursive = TRUE), add = TRUE)
  lock <- genflow:::.stt_chunk_acquire_lock(run_dir)
  owner_path <- file.path(lock$path, "owner.rds")
  owner <- readRDS(owner_path)
  owner$host <- paste0(genflow:::.stt_chunk_lock_host(), "-remote")
  owner$created_at <- as.numeric(Sys.time())
  genflow:::.genflow_atomic_save_rds(owner, owner_path)

  expect_error(
    genflow:::.stt_chunk_acquire_lock(
      run_dir,
      stale_after_seconds = 60
    ),
    "already in use"
  )
  owner$created_at <- as.numeric(Sys.time()) - 120
  genflow:::.genflow_atomic_save_rds(owner, owner_path)
  expect_error(
    genflow:::.stt_chunk_acquire_lock(
      run_dir,
      stale_after_seconds = 60
    ),
    "already in use"
  )

  owner$host <- ""
  genflow:::.genflow_atomic_save_rds(owner, owner_path)
  recovered <- genflow:::.stt_chunk_acquire_lock(
    run_dir,
    stale_after_seconds = 60
  )
  expect_false(identical(recovered$token, lock$token))
  expect_true(genflow:::.stt_chunk_release_lock(recovered))
})

test_that("fixed-duration planning never adapts chunk length to byte size", {
  source <- stt_large_audio_file()
  checkpoint <- tempfile("genflow-fixed-plan-")
  on.exit(unlink(c(source, checkpoint), recursive = TRUE), add = TRUE)
  durations <- new.env(parent = emptyenv())
  extract_calls <- 0L

  testthat::local_mocked_bindings(
    .stt_chunk_prepare_media = function(source,
                                        target,
                                        format,
                                        bitrate_kbps) {
      writeBin(as.raw(rep(2L, 1000L)), target)
      assign(target, 100, envir = durations)
      invisible(target)
    },
    .stt_chunk_extract_media = function(source,
                                        target,
                                        start_seconds,
                                        duration_seconds,
                                        format,
                                        bitrate_kbps) {
      extract_calls <<- extract_calls + 1L
      bytes <- max(1L, as.integer(ceiling(duration_seconds * 1000)))
      writeBin(as.raw(rep(3L, bytes)), target)
      assign(target, duration_seconds, envir = durations)
      invisible(target)
    },
    .stt_audio_duration_seconds = function(path) {
      get(path, envir = durations, inherits = FALSE)
    },
    .package = "genflow"
  )
  options <- genflow:::.stt_chunk_validate_options(
    chunk_segment_seconds = 30,
    checkpoint_dir = checkpoint
  )
  first <- genflow:::.stt_chunk_plan_audio(
    source,
    service = "local-native",
    config_fingerprint = "fixed-config",
    options = options,
    input_duration_seconds = 100
  )
  expect_identical(first$segment_seconds, 30)
  expect_equal(
    vapply(first$parts, `[[`, numeric(1), "start_seconds"),
    c(0, 30, 60, 90)
  )
  expect_equal(
    vapply(first$parts, `[[`, numeric(1), "requested_duration_seconds"),
    c(30, 30, 30, 10)
  )
  expect_identical(extract_calls, 4L)
  manifest <- readRDS(first$manifest_path)
  expect_identical(manifest$segment_seconds, first$segment_seconds)
  expect_false(any(c(
    "planning_attempts", "effective_max_bytes", "overlap_seconds",
    "model_segment_seconds"
  ) %in% names(manifest)))
  calls_after_first <- extract_calls
  expect_true(genflow:::.stt_chunk_release_lock(first$lock))

  second <- genflow:::.stt_chunk_plan_audio(
    source,
    service = "local-native",
    config_fingerprint = "fixed-config",
    options = options,
    input_duration_seconds = 100
  )
  expect_identical(second$segment_seconds, first$segment_seconds)
  expect_identical(extract_calls, calls_after_first)
  expect_true(genflow:::.stt_chunk_release_lock(second$lock))
})

test_that("checkpoint pruning keeps current and newest valid previous run", {
  checkpoint <- tempfile("genflow-prune-")
  outside <- tempfile("genflow-prune-outside-")
  dir.create(checkpoint, recursive = TRUE)
  dir.create(outside, recursive = TRUE)
  marker <- file.path(outside, "keep.txt")
  writeLines("keep", marker)
  on.exit(unlink(c(checkpoint, outside), recursive = TRUE), add = TRUE)

  create_run <- function(key,
                         updated_at,
                         valid = TRUE,
                         source_fingerprint = "recording-a") {
    path <- file.path(checkpoint, paste0("run-", key))
    dir.create(path, recursive = TRUE)
    manifest <- list(
      schema_version = if (valid) 3L else 99L,
      key = key,
      source_fingerprint = source_fingerprint,
      parts = list(),
      updated_at = updated_at
    )
    genflow:::.genflow_atomic_save_rds(
      manifest,
      file.path(path, "manifest.rds")
    )
    path
  }
  run_a <- create_run("a", "2026-01-01T00:00:00Z")
  run_b <- create_run("b", "2026-01-02T00:00:00Z")
  run_c <- create_run("c", "2026-01-03T00:00:00Z")
  run_d <- create_run("d", "2026-01-04T00:00:00Z")
  other_source <- create_run(
    "e",
    "2025-01-01T00:00:00Z",
    source_fingerprint = "recording-b"
  )
  invalid <- create_run("deadbeef", "2025-01-01T00:00:00Z", valid = FALSE)
  nonmatching <- file.path(checkpoint, "run-not-hex")
  dir.create(nonmatching)
  link <- file.path(checkpoint, "run-feed")
  linked <- isTRUE(file.symlink(outside, link))

  active <- genflow:::.stt_chunk_acquire_lock(run_a)
  deleted <- genflow:::.stt_chunk_prune_runs(
    checkpoint,
    current_run_dir = run_d,
    keep_previous = 1L
  )
  expect_identical(deleted, "run-b")
  expect_true(dir.exists(run_a))
  expect_false(dir.exists(run_b))
  expect_true(dir.exists(run_c))
  expect_true(dir.exists(run_d))
  expect_true(dir.exists(other_source))
  expect_true(dir.exists(invalid))
  expect_true(dir.exists(nonmatching))
  if (linked) expect_true(file.exists(marker))

  expect_true(genflow:::.stt_chunk_release_lock(active))
  deleted <- genflow:::.stt_chunk_prune_runs(
    checkpoint,
    current_run_dir = run_d,
    keep_previous = 1L
  )
  expect_identical(deleted, "run-a")
  expect_false(dir.exists(run_a))
  expect_true(dir.exists(run_c))
  expect_true(dir.exists(run_d))
  expect_true(dir.exists(other_source))
  expect_true(dir.exists(invalid))
  if (linked) expect_true(file.exists(marker))
})

test_that("results-only retention removes safe media and preserves checkpoints", {
  checkpoint <- tempfile("genflow-results-retention-")
  outside <- tempfile("genflow-results-retention-outside-")
  dir.create(checkpoint, recursive = TRUE)
  dir.create(outside, recursive = TRUE)
  on.exit(unlink(c(checkpoint, outside), recursive = TRUE), add = TRUE)

  create_run <- function(key, source_fingerprint) {
    run <- file.path(checkpoint, paste0("run-", key))
    dir.create(run, recursive = TRUE)
    prepared <- file.path(run, "prepared.wav")
    part <- file.path(run, "part_0001.wav")
    result <- file.path(run, "part_0001.result.rds")
    writeBin(as.raw(c(1, 2, 3)), prepared)
    writeBin(as.raw(c(4, 5, 6)), part)
    saveRDS(list(response_value = "kept"), result)
    manifest <- list(
      schema_version = 3L,
      key = key,
      source_fingerprint = source_fingerprint,
      prepared_path = prepared,
      prepared_format = "wav",
      parts = list(list(
        audio_path = part,
        result_path = result
      ))
    )
    genflow:::.genflow_atomic_save_rds(
      manifest,
      file.path(run, "manifest.rds")
    )
    list(
      run = run,
      prepared = prepared,
      part = part,
      result = result,
      manifest = file.path(run, "manifest.rds")
    )
  }

  current <- create_run("aa", "recording-a")
  previous <- create_run("bb", "recording-a")
  other <- create_run("cc", "recording-b")
  outside_part <- file.path(outside, "part_0002.wav")
  writeBin(as.raw(c(7, 8, 9)), outside_part)
  marker <- file.path(outside, "marker.wav")
  writeBin(as.raw(c(10, 11, 12)), marker)
  linked_part <- file.path(current$run, "part_0003.wav")
  linked <- isTRUE(file.symlink(marker, linked_part))

  manifest <- readRDS(current$manifest)
  manifest$parts[[2]] <- list(audio_path = outside_part)
  if (linked) manifest$parts[[3]] <- list(audio_path = linked_part)
  genflow:::.genflow_atomic_save_rds(manifest, current$manifest)

  cleaned <- genflow:::.stt_chunk_cleanup_checkpoint_media(
    checkpoint,
    current_run_dir = current$run
  )

  expect_length(cleaned$deleted, 4L)
  expect_length(cleaned$remaining, if (linked) 2L else 1L)
  expect_true(outside_part %in% cleaned$remaining)
  if (linked) expect_true(linked_part %in% cleaned$remaining)
  expect_false(any(file.exists(c(
    current$prepared, current$part,
    previous$prepared, previous$part
  ))))
  expect_true(all(file.exists(c(
    current$result, current$manifest,
    previous$result, previous$manifest,
    other$prepared, other$part, other$result, other$manifest,
    outside_part, marker
  ))))
  if (linked) expect_true(nzchar(Sys.readlink(linked_part)))
})

test_that("results-only retention leaves an active run untouched", {
  checkpoint <- tempfile("genflow-results-retention-active-")
  dir.create(checkpoint, recursive = TRUE)
  on.exit(unlink(checkpoint, recursive = TRUE), add = TRUE)
  run <- file.path(checkpoint, "run-aa")
  dir.create(run)
  prepared <- file.path(run, "prepared.wav")
  writeBin(as.raw(c(1, 2, 3)), prepared)
  manifest <- list(
    schema_version = 3L,
    key = "aa",
    source_fingerprint = "recording-a",
    prepared_path = prepared,
    prepared_format = "wav",
    parts = list()
  )
  genflow:::.genflow_atomic_save_rds(
    manifest,
    file.path(run, "manifest.rds")
  )
  lock <- genflow:::.stt_chunk_acquire_lock(run)
  on.exit(genflow:::.stt_chunk_release_lock(lock), add = TRUE)

  cleaned <- genflow:::.stt_chunk_cleanup_checkpoint_media(
    checkpoint,
    current_run_dir = run
  )
  expect_identical(cleaned$skipped_runs, "run-aa")
  expect_length(cleaned$remaining, 0L)
  expect_true(file.exists(prepared))
})

test_that("results-only retention reports regular files that unlink cannot remove", {
  checkpoint <- tempfile("genflow-results-retention-unlink-")
  dir.create(checkpoint, recursive = TRUE)
  on.exit(unlink(checkpoint, recursive = TRUE), add = TRUE)
  run <- file.path(checkpoint, "run-aa")
  dir.create(run)
  prepared <- file.path(run, "prepared.wav")
  writeBin(as.raw(c(1, 2, 3)), prepared)
  manifest <- list(
    schema_version = 3L,
    key = "aa",
    source_fingerprint = "recording-a",
    prepared_path = prepared,
    prepared_format = "wav",
    parts = list()
  )
  genflow:::.genflow_atomic_save_rds(
    manifest,
    file.path(run, "manifest.rds")
  )
  testthat::local_mocked_bindings(
    .stt_chunk_remove_media_file = function(path) invisible(1L),
    .package = "genflow"
  )

  cleaned <- genflow:::.stt_chunk_cleanup_checkpoint_media(
    checkpoint,
    current_run_dir = run
  )
  expect_length(cleaned$deleted, 0L)
  expect_identical(cleaned$remaining, prepared)
  expect_true(file.exists(prepared))
})

test_that("results-only retention rejects indeterminate current run state", {
  checkpoint <- tempfile("genflow-results-retention-invalid-")
  dir.create(checkpoint, recursive = TRUE)
  on.exit(unlink(checkpoint, recursive = TRUE), add = TRUE)

  unsafe_name <- file.path(checkpoint, "current")
  dir.create(unsafe_name)
  expect_error(
    genflow:::.stt_chunk_cleanup_checkpoint_media(
      checkpoint,
      current_run_dir = unsafe_name
    ),
    "safe direct run"
  )

  invalid_run <- file.path(checkpoint, "run-aa")
  dir.create(invalid_run)
  genflow:::.genflow_atomic_save_rds(
    list(
      schema_version = 99L,
      key = "aa",
      source_fingerprint = "recording-a",
      parts = list()
    ),
    file.path(invalid_run, "manifest.rds")
  )
  expect_error(
    genflow:::.stt_chunk_cleanup_checkpoint_media(
      checkpoint,
      current_run_dir = invalid_run
    ),
    "manifest is invalid"
  )
})

test_that("MOSS runtime discovery does not create an implicit chunk policy", {
  config <- genflow:::.genflow_local_config_defaults()
  config$stt_native_model <- "moss-transcribe-diarize-0.9b-q8_0.gguf"
  withr::local_envvar(c(
    GENFLOW_STT_NATIVE_MODEL = NA,
    GENFLOW_STT_NATIVE_BACKEND = NA,
    GENFLOW_STT_NATIVE_ENGINE = NA,
    GENFLOW_STT_NATIVE_EXECUTABLE = NA
  ))
  testthat::local_mocked_bindings(
    .genflow_read_local_config = function() config,
    .genflow_crispasr_managed_model = function(filename) "",
    .package = "genflow"
  )
  runtime <- genflow:::.stt_chunk_runtime_artifacts(
    service = "local-native"
  )
  expect_identical(runtime$model_value, config$stt_native_model)
  expect_identical(runtime$backend, "moss-diarize")
  expect_false(exists(
    ".stt_chunk_model_policy",
    envir = asNamespace("genflow"),
    inherits = FALSE
  ))
})

test_that("only explicit seconds activate fixed-duration chunking", {
  source <- stt_large_audio_file()
  checkpoint <- tempfile("genflow-large-model-cap-")
  on.exit(unlink(c(source, checkpoint), recursive = TRUE), add = TRUE)
  durations <- new.env(parent = emptyenv())

  testthat::local_mocked_bindings(
    .stt_chunk_prepare_media = function(source,
                                        target,
                                        format,
                                        bitrate_kbps) {
      writeBin(as.raw(rep(2L, 1000L)), target)
      assign(target, 7200, envir = durations)
      invisible(target)
    },
    .stt_chunk_extract_media = function(source,
                                        target,
                                        start_seconds,
                                        duration_seconds,
                                        format,
                                        bitrate_kbps) {
      writeBin(as.raw(rep(3L, 100L)), target)
      assign(target, duration_seconds, envir = durations)
      invisible(target)
    },
    .stt_audio_duration_seconds = function(path) {
      get(path, envir = durations, inherits = FALSE)
    },
    .package = "genflow"
  )

  model_options <- genflow:::.stt_chunk_validate_options(
    checkpoint_dir = checkpoint
  )
  model_plan <- genflow:::.stt_chunk_plan_audio(
    source,
    service = "local-native",
    config_fingerprint = "model-cap",
    options = model_options,
    input_duration_seconds = 7200
  )
  expect_false(model_plan$chunked)
  expect_identical(model_plan$audio_path, source)
  expect_identical(model_plan$decision_reason, "chunk-duration-not-requested")

  explicit_options <- genflow:::.stt_chunk_validate_options(
    chunk_segment_seconds = 1800,
    checkpoint_dir = checkpoint
  )
  explicit_plan <- genflow:::.stt_chunk_plan_audio(
    source,
    service = "local-native",
    config_fingerprint = "explicit-and-model-cap",
    options = explicit_options,
    input_duration_seconds = 7200
  )
  expect_identical(explicit_plan$segment_seconds, 1800)
  expect_identical(explicit_plan$decision_reason, "requested-segment")
  expect_equal(
    vapply(explicit_plan$parts, `[[`, numeric(1), "start_seconds"),
    c(0, 1800, 3600, 5400)
  )
  expect_true(genflow:::.stt_chunk_release_lock(explicit_plan$lock))

  disabled <- genflow:::.stt_chunk_plan_audio(
    source,
    service = "local-native",
    config_fingerprint = "disabled",
    options = genflow:::.stt_chunk_validate_options(
      chunking = "never",
      chunk_segment_seconds = 1800
    )
  )
  expect_false(disabled$chunked)
  expect_identical(disabled$audio_path, source)
  expect_identical(disabled$decision_reason, "chunking-disabled")
})

test_that("gen_stt integrates chunk orchestration and transcript projection", {
  audio <- stt_large_audio_file()
  checkpoint <- tempfile("genflow-large-public-")
  plan <- stt_large_plan_fixture(checkpoint)
  on.exit(unlink(c(audio, checkpoint), recursive = TRUE), add = TRUE)
  calls <- 0L
  cleanup_calls <- 0L
  plan$checkpoint_root <- checkpoint
  plan$run_dir <- file.path(checkpoint, "run-fixture")
  plan$lock <- NULL

  testthat::local_mocked_bindings(
    .stt_audio_duration_seconds = function(path) 19,
    .stt_chunk_runtime_artifacts = function(...) {
      list(
        backend = "moss-diarize",
        model_value = "moss-transcribe-diarize-0.9b-q8_0.gguf"
      )
    },
    .stt_chunk_plan_audio = function(audio_path,
                                     service,
                                     config_fingerprint,
                                     options,
                                     input_duration_seconds = NA_real_) {
      expect_identical(service, "local-native")
      expect_identical(options$chunk_segment_seconds, 10)
      plan
    },
    .stt_chunk_call_backend = function(audio, arguments) {
      calls <<- calls + 1L
      if (calls == 1L) {
        stt_large_result(
          "One unfinished thought",
          "S02",
          0,
          10,
          native_kv_quant = "q8_0",
          native_kv_quant_source = "model-default"
        )
      } else {
        stt_large_result(
          "continues now.",
          "S01",
          0,
          10,
          native_kv_quant = "q8_0",
          native_kv_quant_source = "model-default"
        )
      }
    },
    .stt_chunk_prune_runs = function(...) character(),
    .stt_chunk_cleanup_checkpoint_media = function(...) {
      cleanup_calls <<- cleanup_calls + 1L
      list(deleted = c("prepared.wav", "part_0001.wav"), skipped_runs = character())
    },
    .package = "genflow"
  )

  capture.output(result <- gen_stt(
    audio,
    service = "local-native",
    model = "mock.gguf",
    directory = checkpoint,
    save_txt = TRUE,
    chunk_segment_seconds = 10,
    checkpoint_dir = checkpoint,
    checkpoint_retention = "results",
    output = "transcript"
  ))

  expect_identical(result$status_api, "SUCCESS")
  expect_identical(calls, 2L)
  expect_identical(cleanup_calls, 1L)
  expect_match(result$response_value, "continues now", fixed = TRUE)
  expect_true("diarized_transcript" %in% names(result))
  expect_identical(result$metadata$chunking$part_count, 2L)
  expect_identical(result$metadata$chunking$checkpoint_retention, "results")
  expect_identical(result$metadata$native_kv_quant, "q8_0")
  expect_identical(
    result$metadata$native_kv_quant_source,
    "model-default"
  )
  expect_true(
    result$metadata$chunking$checkpoint_media_cleanup_complete
  )
  expect_false(result$metadata$chunking$checkpoint_media_retained)
  expect_identical(
    result$metadata$chunking$checkpoint_media_removed_count,
    2L
  )
  expect_identical(result$metadata$diarization$speaker_count, 2L)
  expect_true("saved_file" %in% names(result))
  expect_true(file.exists(result$saved_file))
  expect_true(file.exists(result$saved_metadata_file))
  sidecar <- jsonlite::fromJSON(
    result$saved_metadata_file,
    simplifyVector = FALSE
  )
  expect_identical(
    sidecar$metadata$chunking$checkpoint_media_cleanup_complete,
    result$metadata$chunking$checkpoint_media_cleanup_complete
  )
  expect_identical(
    sidecar$metadata$chunking$checkpoint_media_retained,
    result$metadata$chunking$checkpoint_media_retained
  )
  expect_equal(
    sidecar$metadata$chunking$checkpoint_media_removed_count,
    result$metadata$chunking$checkpoint_media_removed_count
  )
  expect_identical(result$service, "local-native")
  expect_identical(result$model, "mock.gguf")
  expect_identical(result$label, tools::file_path_sans_ext(basename(audio)))
  expect_identical(result$audio, audio)
  expect_true(is.numeric(result$duration))
})

test_that("chunk metadata refuses divergent native KV-cache policies", {
  results <- list(
    stt_large_result(
      "First.",
      native_kv_quant = "q8_0",
      native_kv_quant_source = "model-default"
    ),
    stt_large_result(
      "Second.",
      native_kv_quant = "f16",
      native_kv_quant_source = "explicit"
    )
  )

  expect_error(
    genflow:::.stt_chunk_common_metadata_scalar(
      results,
      "native_kv_quant"
    ),
    "diverged across parts"
  )
})

test_that("results-only retention never cleans checkpoint media after failure", {
  audio <- stt_large_audio_file()
  checkpoint <- tempfile("genflow-retention-failure-")
  on.exit(unlink(c(audio, checkpoint), recursive = TRUE), add = TRUE)
  cleanup_calls <- 0L

  testthat::local_mocked_bindings(
    .stt_audio_duration_seconds = function(path) 10,
    .stt_chunk_runtime_artifacts = function(...) list(),
    .stt_chunk_plan_audio = function(audio_path, ...) {
      list(
        chunked = FALSE,
        audio_path = audio_path,
        cleanup_dir = NULL,
        prepared = TRUE,
        chunk_format = "mp3",
        prepared_format = "mp3",
        prepared_size_bytes = 100,
        input_duration_seconds = 10,
        decision_reason = "within-requested-segment",
        checkpoint_root = checkpoint,
        run_dir = file.path(checkpoint, "run-fixture"),
        lock = NULL
      )
    },
    .stt_local_openai = function(...) NULL,
    .stt_chunk_cleanup_checkpoint_media = function(...) {
      cleanup_calls <<- cleanup_calls + 1L
      list(deleted = character(), skipped_runs = character())
    },
    .package = "genflow"
  )

  capture.output(result <- gen_stt(
    audio,
    service = "local-openai",
    save_txt = FALSE,
    chunk_segment_seconds = 10,
    chunk_format = "mp3",
    checkpoint_dir = checkpoint,
    checkpoint_retention = "results"
  ))

  expect_identical(result$status_api, "ERROR")
  expect_identical(cleanup_calls, 0L)
})

test_that("results-only retention exposes incomplete cleanup to callers", {
  audio <- stt_large_audio_file()
  checkpoint <- tempfile("genflow-retention-incomplete-")
  on.exit(unlink(c(audio, checkpoint), recursive = TRUE), add = TRUE)

  testthat::local_mocked_bindings(
    .stt_audio_duration_seconds = function(path) 10,
    .stt_chunk_runtime_artifacts = function(...) list(),
    .stt_chunk_plan_audio = function(audio_path, ...) {
      list(
        chunked = FALSE,
        audio_path = audio_path,
        cleanup_dir = NULL,
        prepared = TRUE,
        chunk_format = "mp3",
        prepared_format = "mp3",
        prepared_size_bytes = 100,
        input_duration_seconds = 10,
        decision_reason = "within-requested-segment",
        checkpoint_root = checkpoint,
        run_dir = file.path(checkpoint, "run-fixture"),
        lock = NULL
      )
    },
    .stt_local_openai = function(...) "Transcript.",
    .stt_chunk_prune_runs = function(...) character(),
    .stt_chunk_cleanup_checkpoint_media = function(...) {
      list(
        deleted = character(),
        remaining = "prepared.mp3",
        skipped_runs = character()
      )
    },
    .package = "genflow"
  )

  expect_warning(
    capture.output(result <- gen_stt(
      audio,
      service = "local-openai",
      save_txt = FALSE,
      chunk_segment_seconds = 10,
      chunk_format = "mp3",
      checkpoint_dir = checkpoint,
      checkpoint_retention = "results"
    )),
    "managed STT checkpoint media"
  )

  expect_identical(result$status_api, "SUCCESS")
  expect_false(
    result$metadata$chunking$checkpoint_media_cleanup_complete
  )
  expect_true(result$metadata$chunking$checkpoint_media_retained)
  expect_identical(
    result$metadata$chunking$checkpoint_media_cleanup_remaining_paths,
    "prepared.mp3"
  )
})

test_that("invalid current manifests expose incomplete cleanup to callers", {
  audio <- stt_large_audio_file()
  checkpoint <- tempfile("genflow-retention-invalid-public-")
  run <- file.path(checkpoint, "run-aa")
  dir.create(run, recursive = TRUE)
  on.exit(unlink(c(audio, checkpoint), recursive = TRUE), add = TRUE)
  genflow:::.genflow_atomic_save_rds(
    list(
      schema_version = 99L,
      key = "aa",
      source_fingerprint = "recording-a",
      parts = list()
    ),
    file.path(run, "manifest.rds")
  )

  testthat::local_mocked_bindings(
    .stt_audio_duration_seconds = function(path) 10,
    .stt_chunk_runtime_artifacts = function(...) list(),
    .stt_chunk_plan_audio = function(audio_path, ...) {
      list(
        chunked = FALSE,
        audio_path = audio_path,
        cleanup_dir = NULL,
        prepared = TRUE,
        chunk_format = "mp3",
        prepared_format = "mp3",
        prepared_size_bytes = 100,
        input_duration_seconds = 10,
        decision_reason = "within-requested-segment",
        checkpoint_root = checkpoint,
        run_dir = run,
        lock = NULL
      )
    },
    .stt_local_openai = function(...) "Transcript.",
    .stt_chunk_prune_runs = function(...) character(),
    .package = "genflow"
  )

  expect_warning(
    capture.output(result <- gen_stt(
      audio,
      service = "local-openai",
      save_txt = FALSE,
      chunk_segment_seconds = 10,
      chunk_format = "mp3",
      checkpoint_dir = checkpoint,
      checkpoint_retention = "results"
    )),
    "current STT checkpoint manifest is invalid"
  )

  expect_identical(result$status_api, "SUCCESS")
  expect_false(
    result$metadata$chunking$checkpoint_media_cleanup_complete
  )
  expect_true(is.na(
    result$metadata$chunking$checkpoint_media_retained
  ))
  expect_match(
    result$metadata$chunking$checkpoint_media_cleanup_error,
    "manifest is invalid"
  )
})

test_that("checkpoint fingerprints follow effective endpoint and model", {
  audio <- stt_large_audio_file()
  on.exit(unlink(audio), add = TRUE)
  fingerprints <- character()

  testthat::local_mocked_bindings(
    .stt_audio_duration_seconds = function(path) 19,
    .stt_chunk_runtime_artifacts = function(...) list(),
    .stt_chunk_plan_audio = function(audio_path,
                                     service,
                                     config_fingerprint,
                                     options,
                                     ...) {
      fingerprints <<- c(fingerprints, config_fingerprint)
      list(
        chunked = FALSE,
        audio_path = audio_path,
        cleanup_dir = NULL,
        prepared = FALSE,
        decision_reason = NULL,
        lock = NULL
      )
    },
    .stt_local_openai = function(...) "Transcript.",
    .package = "genflow"
  )
  withr::local_envvar(c(
    GENFLOW_STT_BASE_URL = "http://127.0.0.1:8090",
    GENFLOW_STT_MODEL = "model-a"
  ))

  run <- function() capture.output(gen_stt(
    audio,
    service = "local-openai",
    save_txt = FALSE
  ))
  run()
  Sys.setenv(GENFLOW_STT_BASE_URL = "http://127.0.0.1:8091")
  run()
  Sys.setenv(GENFLOW_STT_MODEL = "model-b")
  run()

  expect_length(fingerprints, 3L)
  expect_false(identical(fingerprints[[1]], fingerprints[[2]]))
  expect_false(identical(fingerprints[[2]], fingerprints[[3]]))
})

test_that("checkpoint fingerprints follow effective native auto quant", {
  audio <- stt_large_audio_file()
  on.exit(unlink(audio), add = TRUE)
  fingerprints <- character()

  testthat::local_mocked_bindings(
    .stt_audio_duration_seconds = function(path) 19,
    .stt_chunk_runtime_artifacts = function(...) list(
      engine = "crispasr",
      backend = "whisper",
      model_value = "auto",
      model = NULL,
      executable = NULL
    ),
    .stt_chunk_plan_audio = function(audio_path,
                                     service,
                                     config_fingerprint,
                                     options,
                                     ...) {
      fingerprints <<- c(fingerprints, config_fingerprint)
      list(
        chunked = FALSE,
        audio_path = audio_path,
        cleanup_dir = NULL,
        prepared = FALSE,
        decision_reason = NULL,
        lock = NULL
      )
    },
    .stt_local_native = function(...) "Transcript.",
    .package = "genflow"
  )
  withr::local_envvar(c(GENFLOW_STT_NATIVE_QUANT = "q8_0"))

  run <- function() capture.output(gen_stt(
    audio,
    service = "local-native",
    model = "auto",
    native_engine = "crispasr",
    native_backend = "whisper",
    save_txt = FALSE
  ))
  run()
  Sys.setenv(GENFLOW_STT_NATIVE_QUANT = "q4_0")
  run()

  expect_length(fingerprints, 2L)
  expect_false(identical(fingerprints[[1]], fingerprints[[2]]))
})
test_that("transcript projection preserves saved artifact fields", {
  full <- list(
    response_value = "Plain.",
    diarized_transcript = "[S01] Plain.",
    label = "audio",
    label_cat = "audio",
    service = "local-native",
    model = "model.gguf",
    duration = 1,
    status_api = "SUCCESS",
    status_msg = "OK",
    saved_file = "/tmp/transcript.txt",
    saved_metadata_file = "/tmp/transcript.json",
    audio = "/tmp/audio.wav",
    content_type = "text",
    metadata = list(segments = list())
  )
  projected <- genflow:::.stt_project_output(full, "transcript")
  expect_identical(projected$saved_file, full$saved_file)
  expect_identical(
    projected$saved_metadata_file,
    full$saved_metadata_file
  )
})
