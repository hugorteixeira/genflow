restore_test_env <- function(values) {
  for (env in names(values)) {
    value <- values[[env]]
    if (is.na(value)) {
      Sys.unsetenv(env)
    } else {
      do.call(Sys.setenv, stats::setNames(list(value), env))
    }
  }
}

test_that("credential save, update, and delete preserve the public behavior", {
  td <- tempfile("genflow_credentials_")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  target <- file.path(td, ".Renviron")
  alpha <- "GENFLOW_TEST_CREDENTIAL_ALPHA"
  beta <- "GENFLOW_TEST_CREDENTIAL_BETA"
  old_env <- c(
    GENFLOW_TEST_CREDENTIAL_ALPHA = Sys.getenv(alpha, unset = NA_character_),
    GENFLOW_TEST_CREDENTIAL_BETA = Sys.getenv(beta, unset = NA_character_)
  )
  on.exit(restore_test_env(old_env), add = TRUE)
  Sys.unsetenv(c(alpha, beta))

  writeLines(
    c(
      "# keep this comment",
      paste0(alpha, "=old"),
      paste0(alpha, "=duplicate"),
      "UNRELATED_SETTING=keep"
    ),
    target
  )

  first_values <- stats::setNames(
    c("new secret with spaces # one", "second-token"),
    c(alpha, beta)
  )
  first <- genflow:::.genflow_save_credentials(
    first_values,
    path = target,
    backup = TRUE,
    set_session = TRUE
  )

  expect_identical(first$updated, alpha)
  expect_identical(first$added, beta)
  expect_true(file.exists(first$backup_path))
  expect_identical(Sys.getenv(alpha), first_values[[alpha]])
  expect_identical(Sys.getenv(beta), first_values[[beta]])
  expect_false(any(first_values %in% unlist(first, use.names = FALSE)))
  expect_false(dir.exists(genflow:::.genflow_file_lock_path(target)))

  stored <- genflow:::.genflow_read_env_assignments(
    target,
    vars = c(alpha, beta, "UNRELATED_SETTING")
  )
  expect_identical(stored$value[stored$env == alpha], first_values[[alpha]])
  expect_identical(stored$value[stored$env == beta], first_values[[beta]])
  expect_identical(stored$value[stored$env == "UNRELATED_SETTING"], "keep")
  expect_equal(sum(stored$env == alpha), 1L)

  backed_up <- genflow:::.genflow_read_env_assignments(first$backup_path, vars = alpha)
  expect_identical(backed_up$value, c("old", "duplicate"))

  second_values <- stats::setNames("rotated-secret", alpha)
  second <- genflow:::.genflow_save_credentials(
    second_values,
    path = target,
    backup = TRUE,
    set_session = TRUE
  )
  expect_identical(second$updated, alpha)
  expect_false(identical(first$backup_path, second$backup_path))
  expect_true(all(file.exists(c(first$backup_path, second$backup_path))))
  expect_identical(Sys.getenv(alpha), second_values[[alpha]])

  deleted <- genflow:::.genflow_delete_credentials(
    c(alpha, beta),
    path = target,
    backup = TRUE,
    unset_session = TRUE
  )
  expect_setequal(deleted$removed, c(alpha, beta))
  expect_true(file.exists(deleted$backup_path))
  expect_false(deleted$backup_path %in% c(first$backup_path, second$backup_path))
  expect_identical(Sys.getenv(alpha, unset = ""), "")
  expect_identical(Sys.getenv(beta, unset = ""), "")
  expect_false(dir.exists(genflow:::.genflow_file_lock_path(target)))

  remaining <- genflow:::.genflow_read_env_assignments(
    target,
    vars = c(alpha, beta, "UNRELATED_SETTING")
  )
  expect_false(any(remaining$env %in% c(alpha, beta)))
  expect_identical(remaining$value[remaining$env == "UNRELATED_SETTING"], "keep")
})

test_that("credential targets and backups are private files", {
  skip_on_os("windows")

  td <- tempfile("genflow_credentials_mode_")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  target <- file.path(td, ".Renviron")
  writeLines("EXISTING=value", target)
  Sys.chmod(target, mode = "0644", use_umask = FALSE)

  saved <- genflow:::.genflow_save_credentials(
    c(GENFLOW_TEST_PRIVATE_KEY = "private-value"),
    path = target,
    backup = TRUE,
    set_session = FALSE
  )
  expect_identical(as.character(file.info(target)$mode), "600")
  expect_identical(as.character(file.info(saved$backup_path)$mode), "600")

  deleted <- genflow:::.genflow_delete_credentials(
    "GENFLOW_TEST_PRIVATE_KEY",
    path = target,
    backup = TRUE,
    unset_session = FALSE
  )
  expect_identical(as.character(file.info(target)$mode), "600")
  expect_identical(as.character(file.info(deleted$backup_path)$mode), "600")
})

test_that("portable replacement restores the original after a failed commit", {
  td <- tempfile("genflow_credentials_recovery_")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  target <- file.path(td, ".Renviron")
  writeLines(c("ORIGINAL=value", "UNCHANGED=yes"), target)
  original <- readLines(target)

  rename_calls <- 0L
  fail_commit_once <- function(from, to) {
    rename_calls <<- rename_calls + 1L
    if (identical(rename_calls, 2L)) {
      return(FALSE)
    }
    file.rename(from, to)
  }

  error <- tryCatch(
    {
      genflow:::.genflow_atomic_write_lines(
        c("NEW_SECRET=must-not-leak"),
        target,
        rename_fn = fail_commit_once,
        portable_replace = TRUE
      )
      NULL
    },
    error = identity
  )
  expect_s3_class(error, "error")
  expect_match(conditionMessage(error), "original was restored", fixed = TRUE)
  expect_false(grepl("must-not-leak", conditionMessage(error), fixed = TRUE))
  expect_identical(readLines(target), original)
  expect_identical(rename_calls, 3L)
  expect_length(list.files(td, pattern = "staging|rollback"), 0L)
})

test_that("an interrupted portable replacement is recovered before the next edit", {
  skip_on_os("windows")

  td <- tempfile("genflow_credentials_interrupted_")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  target <- file.path(td, ".Renviron")
  rollback <- genflow:::.genflow_unique_sidecar_path(target, "rollback", ".tmp")
  writeLines(c("RECOVERED=value", "KEEP=yes"), rollback)
  Sys.chmod(rollback, mode = "0644", use_umask = FALSE)

  recovered_from <- genflow:::.genflow_recover_credentials_file(target)
  expect_identical(recovered_from, rollback)
  expect_true(file.exists(target))
  expect_false(file.exists(rollback))
  expect_identical(readLines(target), c("RECOVERED=value", "KEEP=yes"))
  expect_identical(as.character(file.info(target)$mode), "600")
})

test_that("credential locks time out, release, and recover stale owners", {
  td <- tempfile("genflow_credentials_lock_")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  target <- file.path(td, ".Renviron")

  first <- genflow:::.genflow_acquire_credentials_lock(
    target,
    timeout = 0.1,
    poll = 0.005,
    stale_after = Inf
  )
  expect_true(dir.exists(first$path))
  expect_error(
    genflow:::.genflow_acquire_credentials_lock(
      target,
      timeout = 0.03,
      poll = 0.005,
      stale_after = Inf
    ),
    "Timed out acquiring the credential file lock",
    fixed = TRUE
  )
  expect_true(genflow:::.genflow_release_credentials_lock(first))
  expect_false(dir.exists(first$path))

  lock_path <- genflow:::.genflow_file_lock_path(target)
  dir.create(lock_path, mode = "0700")
  writeLines("token=abandoned", file.path(lock_path, "owner"))
  recovered <- genflow:::.genflow_acquire_credentials_lock(
    target,
    timeout = 0.1,
    poll = 0.005,
    stale_after = 0
  )
  expect_true(dir.exists(recovered$path))
  expect_true(genflow:::.genflow_release_credentials_lock(recovered))
  expect_false(dir.exists(lock_path))
})
