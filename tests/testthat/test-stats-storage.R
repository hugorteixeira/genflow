make_stats_row <- function(label = "test", status = "SUCCESS") {
  list(
    label = label,
    model = "test-model",
    temp = 0.2,
    duration = 1.5,
    tks_envia = 10,
    tks_recebe = 5,
    status_api = status
  )
}

test_that("statistics directory override is shared by persist, read, and remove", {
  td <- tempfile("genflow_stats_")
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  old_options <- options(genflow.log_dir = td)
  on.exit(options(old_options), add = TRUE)
  date <- as.Date("2026-07-23")

  expect_true(genflow:::.persist_stats_row(make_stats_row("first"), date = date))
  expect_true(dir.exists(td))

  path <- file.path(td, "20260723.rds")
  expect_true(file.exists(path))
  expect_false(dir.exists(genflow:::.genflow_file_lock_path(path)))

  output <- capture.output(saved <- gen_stats(date))
  expect_match(paste(output, collapse = "\n"), "first", fixed = TRUE)
  expect_identical(saved$label, "first")

  expect_message(expect_true(gen_stats_rm(date)), "Removed 20260723.rds")
  expect_false(file.exists(path))
  expect_false(gen_stats_rm(date))
})

test_that("many statistics rows are committed in one append", {
  td <- tempfile("genflow_stats_many_")
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  old_options <- options(genflow.log_dir = td)
  on.exit(options(old_options), add = TRUE)

  results <- list(
    list(
      label = "one", model = "m1", temp = 0.1, duration = 1,
      tokens_sent = 2, tokens_received = 3, status_api = "SUCCESS"
    ),
    list(ignored = TRUE),
    list(
      label = "two", model = "m2", temp = 0.3, duration = 2,
      tokens_sent = 4, tokens_received = 5, status_api = "ERROR"
    )
  )

  expect_true(genflow:::.persist_many_stats(results))
  stored <- readRDS(file.path(td, paste0(format(Sys.Date(), "%Y%m%d"), ".rds")))
  expect_identical(stored$label, c("one", "two"))
  expect_identical(stored$status_api, c("SUCCESS", "ERROR"))
  expect_false(genflow:::.persist_many_stats(list(list(ignored = TRUE))))
})

test_that("an unreadable or invalid existing log is never overwritten", {
  td <- tempfile("genflow_stats_corrupt_")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  old_options <- options(genflow.log_dir = td)
  on.exit(options(old_options), add = TRUE)
  date <- as.Date("2026-07-22")
  path <- file.path(td, "20260722.rds")

  writeBin(as.raw(c(1, 2, 3, 4, 5)), path)
  original <- readBin(path, "raw", n = file.info(path)$size)
  expect_error(
    genflow:::.persist_stats_row(make_stats_row("new"), date = date),
    "Existing statistics log is unreadable",
    fixed = TRUE
  )
  expect_identical(readBin(path, "raw", n = file.info(path)$size), original)
  expect_false(dir.exists(genflow:::.genflow_file_lock_path(path)))

  output <- capture.output(
    expect_warning(
      visible <- gen_stats(date),
      "Log file corrupted or invalid",
      fixed = TRUE
    )
  )
  expect_gte(length(output), 1L)
  expect_identical(nrow(visible), 0L)

  saveRDS(list(not = "a data frame"), path)
  original <- readBin(path, "raw", n = file.info(path)$size)
  expect_error(
    genflow:::.persist_stats_row(make_stats_row("new"), date = date),
    "invalid schema",
    fixed = TRUE
  )
  expect_identical(readBin(path, "raw", n = file.info(path)$size), original)
})

test_that("statistics persistence reports failed commits without changing data", {
  td <- tempfile("genflow_stats_commit_")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  old_options <- options(genflow.log_dir = td)
  on.exit(options(old_options), add = TRUE)
  date <- as.Date("2026-07-21")
  path <- file.path(td, "20260721.rds")

  expect_true(genflow:::.persist_stats_row(make_stats_row("original"), date = date))
  original <- readRDS(path)
  expect_error(
    genflow:::.persist_stats_row(
      make_stats_row("not-written"),
      date = date,
      write_fn = function(object, path) FALSE
    ),
    "Statistics log was not committed",
    fixed = TRUE
  )
  expect_identical(readRDS(path), original)
  expect_false(dir.exists(genflow:::.genflow_file_lock_path(path)))
})

test_that("portable atomic RDS replacement restores the previous log", {
  td <- tempfile("genflow_stats_atomic_")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  path <- file.path(td, "stats.rds")
  original <- data.frame(label = "original", stringsAsFactors = FALSE)
  saveRDS(original, path)

  rename_calls <- 0L
  fail_commit_once <- function(from, to) {
    rename_calls <<- rename_calls + 1L
    if (identical(rename_calls, 2L)) {
      return(FALSE)
    }
    file.rename(from, to)
  }

  expect_error(
    genflow:::.genflow_atomic_save_rds(
      data.frame(label = "replacement", stringsAsFactors = FALSE),
      path,
      rename_fn = fail_commit_once,
      portable_replace = TRUE
    ),
    "the original was restored",
    fixed = TRUE
  )
  expect_identical(readRDS(path), original)
  expect_identical(rename_calls, 3L)
  expect_length(list.files(td, pattern = "staging|rollback"), 0L)
})

test_that("statistics lock timeout does not create a false success", {
  td <- tempfile("genflow_stats_timeout_")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  old_options <- options(
    genflow.log_dir = td,
    genflow.stats_lock_timeout = 0.03,
    genflow.stats_lock_poll = 0.005,
    genflow.stats_lock_stale_after = Inf
  )
  on.exit(options(old_options), add = TRUE)
  date <- as.Date("2026-07-20")
  path <- file.path(td, "20260720.rds")
  lock <- genflow:::.genflow_acquire_file_lock(
    path,
    timeout = 0.1,
    poll = 0.005,
    stale_after = Inf,
    lock_label = "statistics file"
  )
  on.exit(genflow:::.genflow_release_file_lock(lock), add = TRUE)

  expect_error(
    genflow:::.persist_stats_row(make_stats_row("blocked"), date = date),
    "Timed out acquiring the statistics file lock",
    fixed = TRUE
  )
  expect_false(file.exists(path))
})

test_that("concurrent read-modify-write appends do not lose rows", {
  skip_on_os("windows")

  td <- tempfile("genflow_stats_race_")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  old_options <- options(
    genflow.log_dir = td,
    genflow.stats_lock_timeout = 5,
    genflow.stats_lock_poll = 0.005,
    genflow.stats_lock_stale_after = Inf
  )
  on.exit(options(old_options), add = TRUE)
  date <- as.Date("2026-07-19")
  entered <- file.path(td, "first-writer-entered")
  release <- file.path(td, "release-first-writer")

  first <- parallel::mcparallel(
    genflow:::.persist_stats_row(
      make_stats_row("first"),
      date = date,
      write_fn = function(object, path) {
        file.create(entered)
        deadline <- Sys.time() + 5
        while (!file.exists(release) && Sys.time() < deadline) {
          Sys.sleep(0.005)
        }
        genflow:::.genflow_atomic_save_rds(object, path)
      }
    ),
    silent = TRUE
  )
  on.exit({
    if (dir.exists(td)) {
      file.create(release)
    }
    if (!is.null(first)) {
      try(parallel::mccollect(first, wait = FALSE), silent = TRUE)
    }
  }, add = TRUE)

  deadline <- Sys.time() + 5
  while (!file.exists(entered) && Sys.time() < deadline) {
    Sys.sleep(0.005)
  }
  expect_true(file.exists(entered))

  second <- parallel::mcparallel(
    genflow:::.persist_stats_row(make_stats_row("second"), date = date),
    silent = TRUE
  )
  on.exit({
    if (!is.null(second)) {
      try(parallel::mccollect(second, wait = FALSE), silent = TRUE)
    }
  }, add = TRUE)
  Sys.sleep(0.05)
  file.create(release)

  results <- parallel::mccollect(list(first, second), wait = TRUE)
  first <- NULL
  second <- NULL
  expect_true(all(vapply(results, isTRUE, logical(1))))
  stored <- readRDS(file.path(td, "20260719.rds"))
  expect_setequal(stored$label, c("first", "second"))
  expect_identical(nrow(stored), 2L)
})

test_that("statistics files are private on Unix-like systems", {
  skip_on_os("windows")

  td <- tempfile("genflow_stats_mode_")
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  old_options <- options(genflow.log_dir = td)
  on.exit(options(old_options), add = TRUE)

  expect_true(genflow:::.persist_stats_row(make_stats_row()))
  path <- file.path(td, paste0(format(Sys.Date(), "%Y%m%d"), ".rds"))
  expect_identical(as.character(file.info(path)$mode), "600")
})
