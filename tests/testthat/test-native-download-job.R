test_that("native download worker reports a local model result", {
  model <- tempfile(fileext = ".gguf")
  job_dir <- tempfile("genflow-native-download-worker-")
  dir.create(job_dir)
  on.exit(unlink(c(model, job_dir), recursive = TRUE, force = TRUE), add = TRUE)
  writeBin(as.raw(1:4), model)

  spec_path <- file.path(job_dir, "spec.rds")
  status_path <- file.path(job_dir, "status.json")
  saveRDS(
    list(
      id = "worker-test",
      selector = model,
      backend = "",
      quant = "",
      executable = ""
    ),
    spec_path
  )

  expect_true(genflow:::.genflow_native_download_job_worker(
    spec_path,
    status_path
  ))
  status <- genflow:::.genflow_native_download_job_read(status_path)
  expect_identical(status$state, "complete")
  expect_identical(status$result$path, normalizePath(model))
  expect_equal(status$result$size_bytes, 4)
})

test_that("native download bootstrap uses base R before private library paths", {
  script <- readLines(
    genflow:::.genflow_native_download_job_script(),
    warn = FALSE
  )
  read_index <- grep("readRDS\\(spec_path\\)", script)
  library_path_index <- grep("\\.libPaths\\(", script)
  package_load_index <- grep(
    "pkgload::load_all|library\\(genflow\\)",
    script
  )

  expect_length(read_index, 1L)
  expect_true(read_index < min(library_path_index))
  expect_true(min(library_path_index) < min(package_load_index))
  expect_false(any(grepl(
    "jsonlite",
    script[seq_len(min(package_load_index) - 1L)],
    fixed = TRUE
  )))

  private_library <- tempfile("genflow-private-library-")
  dir.create(private_library)
  old_paths <- .libPaths()
  on.exit(.libPaths(old_paths), add = TRUE)
  .libPaths(c(private_library, old_paths))

  model <- tempfile(fileext = ".gguf")
  writeBin(as.raw(1:4), model)
  on.exit(unlink(c(model, private_library), recursive = TRUE), add = TRUE)
  job <- genflow:::.genflow_native_download_job_start(model)
  on.exit({
    if (genflow:::.genflow_native_download_job_alive(job)) {
      genflow:::.genflow_native_download_job_cancel(job)
    }
    if (dir.exists(job$dir)) {
      genflow:::.genflow_native_download_job_cleanup(job)
    }
  }, add = TRUE)

  spec <- readRDS(job$spec_path)
  expect_identical(spec$library_paths[[1]], private_library)
})

test_that("native download job runs outside the calling R process", {
  model <- tempfile(fileext = ".gguf")
  writeBin(as.raw(1:4), model)
  on.exit(unlink(model), add = TRUE)

  job <- genflow:::.genflow_native_download_job_start(model)
  on.exit({
    if (genflow:::.genflow_native_download_job_alive(job)) {
      genflow:::.genflow_native_download_job_cancel(job)
    }
    if (dir.exists(job$dir)) {
      genflow:::.genflow_native_download_job_cleanup(job)
    }
  }, add = TRUE)

  deadline <- Sys.time() + 15
  while (genflow:::.genflow_native_download_job_alive(job) &&
         Sys.time() < deadline) {
    Sys.sleep(0.05)
  }
  expect_false(genflow:::.genflow_native_download_job_alive(job))
  status <- genflow:::.genflow_native_download_job_read(job)
  if (!identical(status$state, "complete")) {
    detail <- if (file.exists(job$stderr_path)) {
      paste(readLines(job$stderr_path, warn = FALSE), collapse = "\n")
    } else {
      status$message %||% ""
    }
    fail(paste("Background worker failed:", detail))
  }
  expect_identical(status$result$path, normalizePath(model))
  preserved <- genflow:::.genflow_native_download_job_cancel(job)
  expect_identical(preserved$state, "complete")
  expect_identical(preserved$result$path, normalizePath(model))
  expect_true(genflow:::.genflow_native_download_job_cleanup(job))
})

test_that("cancelling any terminal job preserves its terminal state", {
  for (state in c("complete", "error", "cancelled")) {
    job_dir <- tempfile("genflow-native-download-", tmpdir = tempdir())
    dir.create(job_dir)
    job <- structure(
      list(
        id = paste0("terminal-test-", state),
        dir = normalizePath(job_dir),
        status_path = file.path(job_dir, "status.json")
      ),
      class = "genflow_native_download_job"
    )
    genflow:::.genflow_native_download_job_write(
      job$status_path,
      list(
        id = job$id,
        state = state,
        stage = state,
        message = state,
        marker = paste0("preserve-", state)
      )
    )
    preserved <- genflow:::.genflow_native_download_job_cancel(job)
    expect_identical(preserved$state, state)
    expect_identical(preserved$marker, paste0("preserve-", state))
    ignored <- genflow:::.genflow_native_download_job_write(
      job$status_path,
      list(state = "running", marker = "replace-terminal")
    )
    expect_identical(ignored$state, state)
    expect_identical(ignored$marker, paste0("preserve-", state))
    unlink(job_dir, recursive = TRUE, force = TRUE)
  }
})

test_that("job read re-reads status after observing a dead worker", {
  job_dir <- tempfile("genflow-native-download-", tmpdir = tempdir())
  dir.create(job_dir)
  status_path <- file.path(job_dir, "status.json")
  on.exit(unlink(job_dir, recursive = TRUE, force = TRUE), add = TRUE)
  genflow:::.genflow_native_download_job_write(
    status_path,
    list(
      id = "read-race-test",
      state = "running",
      stage = "downloading",
      bytes_received = 10,
      bytes_total = 20
    )
  )

  process <- structure(
    list(
      is_alive = function() {
        genflow:::.genflow_native_download_job_write(
          status_path,
          list(
            id = "read-race-test",
            state = "complete",
            stage = "complete",
            result = list(path = "/cache/final.gguf")
          )
        )
        FALSE
      },
      get_exit_status = function() 0L
    ),
    class = "process"
  )
  job <- structure(
    list(
      id = "read-race-test",
      dir = normalizePath(job_dir),
      pid = 12345L,
      process = process,
      status_path = status_path,
      gate_path = file.path(job_dir, "terminal.gate")
    ),
    class = "genflow_native_download_job"
  )

  status <- genflow:::.genflow_native_download_job_read(job)
  expect_identical(status$state, "complete")
  expect_identical(status$result$path, "/cache/final.gguf")
})

test_that("worker errors preserve the latest progress snapshot", {
  job_dir <- tempfile("genflow-native-download-worker-")
  dir.create(job_dir)
  on.exit(unlink(job_dir, recursive = TRUE, force = TRUE), add = TRUE)
  spec_path <- file.path(job_dir, "spec.rds")
  status_path <- file.path(job_dir, "status.json")
  gate_path <- file.path(job_dir, "terminal.gate")
  saveRDS(
    list(
      id = "worker-error-test",
      selector = "auto",
      backend = "granite",
      quant = "q8_0",
      executable = "",
      gate_path = gate_path
    ),
    spec_path
  )
  testthat::local_mocked_bindings(
    .genflow_crispasr_download = function(selector,
                                          backend,
                                          quant,
                                          executable,
                                          progress) {
      progress(list(
        stage = "downloading",
        filename = "model-q8_0.gguf",
        bytes_received = 123,
        bytes_total = 1000,
        proportion = 0.123
      ))
      stop("download boom", call. = FALSE)
    },
    .package = "genflow"
  )

  expect_false(genflow:::.genflow_native_download_job_worker(
    spec_path,
    status_path
  ))
  status <- genflow:::.genflow_native_download_job_read(status_path)
  expect_identical(status$state, "error")
  expect_identical(status$stage, "downloading")
  expect_identical(status$filename, "model-q8_0.gguf")
  expect_equal(status$bytes_received, 123)
  expect_equal(status$bytes_total, 1000)
  expect_equal(status$proportion, 0.123)
  expect_match(status$message, "download boom", fixed = TRUE)
  expect_false(dir.exists(gate_path))
})

test_that("dead non-terminal workers are reconciled with stderr and exit status", {
  job_dir <- tempfile("genflow-native-download-", tmpdir = tempdir())
  dir.create(job_dir)
  stderr_path <- file.path(job_dir, "stderr.log")
  status_path <- file.path(job_dir, "status.json")
  process <- processx::process$new(
    command = unname(Sys.which("Rscript")),
    args = c(
      "--vanilla",
      "-e",
      "writeLines('bootstrap failed', stderr()); quit(save='no', status=7L)"
    ),
    stdout = file.path(job_dir, "stdout.log"),
    stderr = stderr_path,
    cleanup = TRUE,
    cleanup_tree = TRUE
  )
  job <- structure(
    list(
      id = "dead-worker-test",
      dir = normalizePath(job_dir),
      pid = process$get_pid(),
      process = process,
      status_path = status_path,
      stderr_path = stderr_path
    ),
    class = "genflow_native_download_job"
  )
  on.exit({
    if (genflow:::.genflow_native_download_job_alive(job)) {
      try(process$kill_tree(), silent = TRUE)
    }
    if (dir.exists(job_dir)) {
      genflow:::.genflow_native_download_job_cleanup(job)
    }
  }, add = TRUE)
  genflow:::.genflow_native_download_job_write(
    status_path,
    list(
      id = job$id,
      state = "queued",
      stage = "verifying",
      message = "verifying",
      filename = "model.gguf",
      bytes_received = 40,
      bytes_total = 40,
      proportion = 1
    )
  )
  process$wait(timeout = 5000)

  status <- genflow:::.genflow_native_download_job_read(job)
  expect_identical(status$state, "error")
  expect_identical(status$stage, "verifying")
  expect_identical(status$filename, "model.gguf")
  expect_identical(status$bytes_received, 40L)
  expect_identical(status$bytes_total, 40L)
  expect_match(status$message, "bootstrap failed", fixed = TRUE)
  expect_identical(status$error$class, "worker_exit")
  expect_identical(status$error$exit_status, 7L)
})

test_that("dead worker gate is reclaimed before error reconciliation", {
  job_dir <- tempfile("genflow-native-download-", tmpdir = tempdir())
  dir.create(job_dir)
  status_path <- file.path(job_dir, "status.json")
  gate_path <- file.path(job_dir, "terminal.gate")
  process <- processx::process$new(
    command = unname(Sys.which("Rscript")),
    args = c("--vanilla", "-e", "quit(save='no', status=9L)"),
    cleanup = TRUE,
    cleanup_tree = TRUE
  )
  pid <- process$get_pid()
  process$wait(timeout = 5000)
  dir.create(gate_path)
  saveRDS(
    list(
      token = "abandoned-worker-token",
      pid = pid,
      role = "worker",
      job_id = "stale-gate-test"
    ),
    file.path(gate_path, "owner.rds")
  )
  genflow:::.genflow_native_download_job_write(
    status_path,
    list(
      id = "stale-gate-test",
      state = "publishing",
      stage = "publishing",
      filename = "model.gguf",
      bytes_received = 99,
      bytes_total = 100
    )
  )
  job <- structure(
    list(
      id = "stale-gate-test",
      dir = normalizePath(job_dir),
      pid = pid,
      process = process,
      status_path = status_path,
      gate_path = gate_path
    ),
    class = "genflow_native_download_job"
  )
  on.exit({
    if (dir.exists(job_dir)) {
      genflow:::.genflow_native_download_job_cleanup(job)
    }
  }, add = TRUE)

  status <- genflow:::.genflow_native_download_job_read(job)
  expect_identical(status$state, "error")
  expect_identical(status$stage, "publishing")
  expect_equal(status$bytes_received, 99)
  expect_equal(status$bytes_total, 100)
  expect_identical(status$error$exit_status, 9L)
  expect_false(dir.exists(gate_path))
})

test_that("cancel waits through running-to-publishing race and never kills", {
  job_dir <- tempfile("genflow-native-download-", tmpdir = tempdir())
  dir.create(job_dir)
  status_path <- file.path(job_dir, "status.json")
  gate_path <- file.path(job_dir, "terminal.gate")
  ready_path <- file.path(job_dir, "gate-ready")
  status_literal <- jsonlite::toJSON(
    list(
      id = "publishing-test",
      state = "complete",
      stage = "complete",
      message = "ready",
      result = list(path = "/cache/model.gguf")
    ),
    auto_unbox = TRUE
  )
  code <- paste0(
    "dir.create(",
    encodeString(gate_path, quote = "\""),
    ");",
    "saveRDS(list(token='worker-token',pid=Sys.getpid(),",
    "role='worker',job_id='publishing-test'),",
    encodeString(file.path(gate_path, "owner.rds"), quote = "\""),
    ");",
    "file.create(",
    encodeString(ready_path, quote = "\""),
    ");",
    "Sys.sleep(0.2);",
    "writeLines(",
    encodeString(status_literal, quote = "\""),
    ", ",
    encodeString(status_path, quote = "\""),
    ");",
    "unlink(",
    encodeString(file.path(gate_path, "owner.rds"), quote = "\""),
    ");",
    "unlink(",
    encodeString(gate_path, quote = "\""),
    ",recursive=TRUE);",
    "Sys.sleep(5)"
  )
  genflow:::.genflow_native_download_job_write(
    status_path,
    list(
      id = "publishing-test",
      state = "running",
      stage = "downloading",
      message = "downloading"
    )
  )
  process <- processx::process$new(
    command = unname(Sys.which("Rscript")),
    args = c("--vanilla", "-e", code),
    cleanup = TRUE,
    cleanup_tree = TRUE
  )
  job <- structure(
    list(
      id = "publishing-test",
      dir = normalizePath(job_dir),
      pid = process$get_pid(),
      process = process,
      status_path = status_path,
      gate_path = gate_path,
      stderr_path = file.path(job_dir, "stderr.log")
    ),
    class = "genflow_native_download_job"
  )
  on.exit({
    if (genflow:::.genflow_native_download_job_alive(job)) {
      try(process$kill_tree(), silent = TRUE)
      try(process$wait(timeout = 5000), silent = TRUE)
    }
    if (dir.exists(job_dir)) {
      genflow:::.genflow_native_download_job_cleanup(job)
    }
  }, add = TRUE)
  deadline <- Sys.time() + 5
  while (!file.exists(ready_path) && Sys.time() < deadline) Sys.sleep(0.01)
  expect_true(file.exists(ready_path))
  expect_identical(
    genflow:::.genflow_native_download_job_read(status_path)$state,
    "running"
  )

  status <- genflow:::.genflow_native_download_job_cancel(
    job,
    publishing_wait_ms = 3000L
  )
  expect_identical(status$state, "complete")
  expect_identical(status$result$path, "/cache/model.gguf")
  expect_true(genflow:::.genflow_native_download_job_alive(job))
})

test_that("terminal cleanup removes only regular PID-scoped model parts", {
  cache_dir <- tempfile("genflow-crispasr-cache-")
  job_dir <- tempfile("genflow-native-download-", tmpdir = tempdir())
  dir.create(cache_dir)
  dir.create(job_dir)
  withr::local_envvar(c(CRISPASR_CACHE_DIR = cache_dir))
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE), add = TRUE)
  pid <- 424242L
  filename <- "model-q8_0.gguf"
  payload_part <- file.path(
    cache_dir,
    paste0(".", filename, ".part.", pid, ".payload")
  )
  source_part <- file.path(
    cache_dir,
    paste0(".", filename, ".src.part.", pid, ".source")
  )
  other_part <- file.path(
    cache_dir,
    paste0(".", filename, ".part.", pid + 1L, ".other")
  )
  directory_part <- file.path(
    cache_dir,
    paste0(".", filename, ".part.", pid, ".directory")
  )
  target <- file.path(cache_dir, "symlink-target")
  symlink_part <- file.path(
    cache_dir,
    paste0(".", filename, ".src.part.", pid, ".symlink")
  )
  writeBin(as.raw(1:2), payload_part)
  writeBin(as.raw(3:4), source_part)
  writeBin(as.raw(5:6), other_part)
  writeBin(as.raw(7:8), target)
  dir.create(directory_part)
  expect_true(file.symlink(target, symlink_part))

  status_path <- file.path(job_dir, "status.json")
  genflow:::.genflow_native_download_job_write(
    status_path,
    list(
      id = "cleanup-parts-test",
      state = "error",
      stage = "downloading",
      filename = filename,
      bytes_received = 2,
      bytes_total = 10
    )
  )
  job <- structure(
    list(
      id = "cleanup-parts-test",
      dir = normalizePath(job_dir),
      pid = pid,
      status_path = status_path,
      gate_path = file.path(job_dir, "terminal.gate")
    ),
    class = "genflow_native_download_job"
  )

  expect_true(genflow:::.genflow_native_download_job_cleanup(job))
  expect_false(file.exists(payload_part))
  expect_false(file.exists(source_part))
  expect_true(file.exists(other_part))
  expect_true(dir.exists(directory_part))
  expect_true(nzchar(Sys.readlink(symlink_part)))
  expect_true(file.exists(target))
})

test_that("native download jobs can be cancelled and cleaned safely", {
  job_dir <- tempfile("genflow-native-download-", tmpdir = tempdir())
  dir.create(job_dir)
  process <- processx::process$new(
    command = unname(Sys.which("Rscript")),
    args = c("--vanilla", "-e", "Sys.sleep(30)"),
    cleanup = TRUE,
    cleanup_tree = TRUE
  )
  job <- structure(
    list(
      id = "cancel-test",
      dir = normalizePath(job_dir),
      pid = process$get_pid(),
      process = process,
      status_path = file.path(job_dir, "status.json")
    ),
    class = "genflow_native_download_job"
  )
  genflow:::.genflow_native_download_job_write(
    job$status_path,
    list(
      id = job$id,
      state = "running",
      stage = "downloading",
      message = "running"
    )
  )

  cancelled <- genflow:::.genflow_native_download_job_cancel(job)
  expect_identical(cancelled$state, "cancelled")
  expect_false(genflow:::.genflow_native_download_job_alive(job))
  expect_true(genflow:::.genflow_native_download_job_cleanup(job))
})
