write_catalog_fixture <- function(directory,
                                  provider,
                                  model = "model-one",
                                  provider_column = "service",
                                  include_description = TRUE) {
  catalog <- data.frame(
    provider_value = provider,
    model = model,
    type = "Chat",
    pricing = "",
    description = "fixture",
    stringsAsFactors = FALSE
  )
  names(catalog)[[1]] <- provider_column
  if (!isTRUE(include_description)) {
    catalog$description <- NULL
  }
  utils::write.csv(
    catalog,
    file.path(directory, paste0(provider, ".csv")),
    row.names = FALSE,
    na = ""
  )
  invisible(catalog)
}

test_that("catalog validation accepts service or provider and rejects unsafe data", {
  td <- tempfile("genflow_catalog_validate_")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)

  write_catalog_fixture(td, "openai", provider_column = "service")
  expect_silent(
    genflow:::.genflow_validate_catalog_file(
      file.path(td, "openai.csv"),
      "openai"
    )
  )

  write_catalog_fixture(td, "acme", provider_column = "provider")
  expect_silent(
    genflow:::.genflow_validate_catalog_file(
      file.path(td, "acme.csv"),
      "acme"
    )
  )

  write_catalog_fixture(td, "groq", include_description = FALSE)
  expect_error(
    genflow:::.genflow_validate_catalog_file(
      file.path(td, "groq.csv"),
      "groq"
    ),
    "invalid catalog schema",
    fixed = TRUE
  )

  write_catalog_fixture(td, "anthropic", model = "")
  expect_error(
    genflow:::.genflow_validate_catalog_file(
      file.path(td, "anthropic.csv"),
      "anthropic"
    ),
    "empty model id",
    fixed = TRUE
  )

  write_catalog_fixture(td, "openrouter")
  expect_error(
    genflow:::.genflow_validate_catalog_file(
      file.path(td, "openrouter.csv"),
      "openai"
    ),
    "different provider",
    fixed = TRUE
  )
})

test_that("gen_update_models publishes a validated staged catalog", {
  td <- tempfile("genflow_catalog_success_")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  final_path <- file.path(td, "openai.csv")
  writeLines("previous catalog", final_path)
  updater_directory <- ""

  testthat::local_mocked_bindings(
    .update_models_openai = function(directory, verbose) {
      updater_directory <<- directory
      write_catalog_fixture(directory, "openai", model = "new-model")
      invisible(data.frame(source = "mock"))
    },
    .genflow_list_custom_provider_configs = function() list(),
    .package = "genflow"
  )

  result <- gen_update_models(
    provider = "openai",
    directory = td,
    verbose = FALSE,
    fail_on_error = TRUE
  )

  expect_false(identical(updater_directory, td))
  expect_identical(dirname(updater_directory), td)
  expect_false(dir.exists(updater_directory))
  expect_identical(utils::read.csv(final_path)$model, "new-model")
  expect_identical(attr(result, "updated_providers"), "openai")
  expect_length(attr(result, "failed_providers"), 0L)
  expect_length(attr(result, "failures"), 0L)
  expect_false(dir.exists(genflow:::.genflow_file_lock_path(final_path)))
})

test_that("invalid or failing built-in updates preserve the previous catalog", {
  td <- tempfile("genflow_catalog_failure_")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  final_path <- file.path(td, "openai.csv")
  writeLines(c("previous", "catalog", "bytes"), final_path)
  previous <- readBin(final_path, "raw", n = file.info(final_path)$size)

  testthat::local_mocked_bindings(
    .update_models_openai = function(directory, verbose) {
      write_catalog_fixture(
        directory,
        "openai",
        model = "unsafe",
        include_description = FALSE
      )
      invisible(NULL)
    },
    .genflow_list_custom_provider_configs = function() list(),
    .package = "genflow"
  )

  result <- gen_update_models(
    provider = "openai",
    directory = td,
    verbose = FALSE,
    fail_on_error = FALSE
  )
  expect_length(attr(result, "updated_providers"), 0L)
  expect_identical(attr(result, "failed_providers"), "openai")
  expect_match(
    attr(result, "failures")$openai,
    "invalid catalog schema",
    fixed = TRUE
  )
  expect_identical(
    readBin(final_path, "raw", n = file.info(final_path)$size),
    previous
  )

  expect_error(
    gen_update_models(
      provider = "openai",
      directory = td,
      verbose = FALSE,
      fail_on_error = TRUE
    ),
    "Model update failed for openai",
    fixed = TRUE
  )
  expect_identical(
    readBin(final_path, "raw", n = file.info(final_path)$size),
    previous
  )
})

test_that("an updater error after staging never publishes its file", {
  td <- tempfile("genflow_catalog_updater_error_")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  final_path <- file.path(td, "openai.csv")
  writeLines("old-catalog", final_path)

  testthat::local_mocked_bindings(
    .update_models_openai = function(directory, verbose) {
      write_catalog_fixture(directory, "openai", model = "not-published")
      stop("simulated parser failure", call. = FALSE)
    },
    .genflow_list_custom_provider_configs = function() list(),
    .package = "genflow"
  )

  result <- gen_update_models(
    provider = "openai",
    directory = td,
    verbose = FALSE
  )
  expect_identical(readLines(final_path), "old-catalog")
  expect_identical(attr(result, "failed_providers"), "openai")
  expect_match(
    attr(result, "failures")$openai,
    "simulated parser failure",
    fixed = TRUE
  )
})

test_that("custom provider catalogs use the same staging and validation", {
  td <- tempfile("genflow_catalog_custom_")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  updater_directory <- ""

  testthat::local_mocked_bindings(
    .genflow_list_custom_provider_configs = function() {
      list(acme = list(id = "acme", label = "Acme"))
    },
    .update_models_custom_openai_compat = function(provider_id,
                                                    directory,
                                                    verbose) {
      updater_directory <<- directory
      write_catalog_fixture(
        directory,
        provider_id,
        model = "acme-chat",
        provider_column = "provider"
      )
      invisible(NULL)
    },
    .package = "genflow"
  )

  result <- gen_update_models(
    provider = "acme",
    directory = td,
    verbose = FALSE,
    fail_on_error = TRUE
  )
  catalog <- utils::read.csv(file.path(td, "acme.csv"))
  expect_identical(catalog$provider, "acme")
  expect_identical(catalog$model, "acme-chat")
  expect_false(identical(updater_directory, td))
  expect_false(dir.exists(updater_directory))
  expect_identical(attr(result, "updated_providers"), "acme")
  expect_length(attr(result, "failed_providers"), 0L)
})

test_that("invalid custom provider output preserves its previous catalog", {
  td <- tempfile("genflow_catalog_custom_failure_")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  final_path <- file.path(td, "acme.csv")
  writeLines("custom-previous", final_path)

  testthat::local_mocked_bindings(
    .genflow_list_custom_provider_configs = function() {
      list(acme = list(id = "acme", label = "Acme"))
    },
    .update_models_custom_openai_compat = function(provider_id,
                                                    directory,
                                                    verbose) {
      utils::write.csv(
        data.frame(service = provider_id, model = "missing-columns"),
        file.path(directory, paste0(provider_id, ".csv")),
        row.names = FALSE
      )
      invisible(NULL)
    },
    .package = "genflow"
  )

  result <- gen_update_models(
    provider = "acme",
    directory = td,
    verbose = FALSE
  )
  expect_identical(readLines(final_path), "custom-previous")
  expect_identical(attr(result, "failed_providers"), "acme")
  expect_match(
    attr(result, "failures")$acme,
    "invalid catalog schema",
    fixed = TRUE
  )
})

test_that("portable catalog promotion restores the previous file on failure", {
  td <- tempfile("genflow_catalog_atomic_")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  staged <- file.path(td, "staged.csv")
  final <- file.path(td, "openai.csv")
  writeLines("new-catalog", staged)
  writeLines("old-catalog", final)

  rename_calls <- 0L
  fail_commit_once <- function(from, to) {
    rename_calls <<- rename_calls + 1L
    if (identical(rename_calls, 2L)) {
      return(FALSE)
    }
    file.rename(from, to)
  }

  expect_error(
    genflow:::.genflow_promote_catalog_file(
      staged,
      final,
      rename_fn = fail_commit_once,
      portable_replace = TRUE
    ),
    "previous catalog was restored",
    fixed = TRUE
  )
  expect_identical(readLines(final), "old-catalog")
  expect_identical(readLines(staged), "new-catalog")
  expect_identical(rename_calls, 3L)
  expect_false(dir.exists(genflow:::.genflow_file_lock_path(final)))
  expect_length(list.files(td, pattern = "rollback"), 0L)
})
