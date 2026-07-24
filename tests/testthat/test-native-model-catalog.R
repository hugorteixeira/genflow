native_catalog_inventory <- function(managed = c(TRUE, FALSE)) {
  data.frame(
    path = c(
      "/cache/granite-speech-q8_0.gguf",
      "/shared/whisper-tiny.bin"
    ),
    filename = c(
      "granite-speech-q8_0.gguf",
      "whisper-tiny.bin"
    ),
    quant = c("q8_0", ""),
    size_bytes = c(2048, 1024),
    size = c("2.0 KiB", "1.0 KiB"),
    source_url = c(
      paste0(
        "https://huggingface.co/example/granite/resolve/",
        paste(rep("a", 40), collapse = ""),
        "/granite-speech-q8_0.gguf"
      ),
      ""
    ),
    managed = managed,
    selected = c(FALSE, FALSE),
    stringsAsFactors = FALSE
  )
}

test_that("native model updater catalogs only managed cache basenames", {
  directory <- tempfile("genflow-native-catalog-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)

  output <- genflow:::.update_models_local_native(
    directory = directory,
    verbose = FALSE,
    inventory_fn = function() native_catalog_inventory()
  )

  expect_identical(output$service, "local-native")
  expect_identical(output$model, "granite-speech-q8_0.gguf")
  expect_identical(output$type, "Audio")
  expect_identical(output$pricing, "")
  expect_match(output$description, "engine=crispasr", fixed = TRUE)
  expect_match(output$description, "quant=q8_0", fixed = TRUE)
  expect_match(output$description, "size=2.0 KiB", fixed = TRUE)
  expect_match(output$description, "source=https://huggingface.co/", fixed = TRUE)
  expect_false(grepl("/cache/", output$model, fixed = TRUE))

  cached <- utils::read.csv(
    file.path(directory, "local-native.csv"),
    stringsAsFactors = FALSE
  )
  expect_identical(cached$model, "granite-speech-q8_0.gguf")
  expect_false("whisper-tiny.bin" %in% cached$model)
})

test_that("local-native explicitly publishes an empty managed catalog", {
  directory <- tempfile("genflow-native-empty-catalog-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  final_path <- file.path(directory, "local-native.csv")
  utils::write.csv(
    data.frame(
      service = "local-native",
      model = "stale-model.gguf",
      type = "Audio",
      pricing = "",
      description = "",
      stringsAsFactors = FALSE
    ),
    final_path,
    row.names = FALSE
  )

  testthat::local_mocked_bindings(
    .genflow_crispasr_inventory = function(...) {
      genflow:::.genflow_crispasr_empty_inventory()
    },
    .genflow_list_custom_provider_configs = function() list(),
    .package = "genflow"
  )

  result <- gen_update_models(
    provider = "local-native",
    directory = directory,
    verbose = FALSE,
    fail_on_error = TRUE
  )

  cached <- utils::read.csv(final_path, stringsAsFactors = FALSE)
  expect_identical(
    names(cached),
    c("service", "model", "type", "pricing", "description")
  )
  expect_equal(nrow(cached), 0L)
  expect_identical(attr(result, "updated_providers"), "local-native")
  expect_length(attr(result, "failed_providers"), 0L)
})

test_that("invalid managed filenames replace stale catalogs with an empty catalog", {
  directory <- tempfile("genflow-native-invalid-catalog-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  final_path <- file.path(directory, "local-native.csv")
  utils::write.csv(
    data.frame(
      service = "local-native",
      model = "stale-model.gguf",
      type = "Audio",
      pricing = "",
      description = "",
      stringsAsFactors = FALSE
    ),
    final_path,
    row.names = FALSE
  )

  invalid_inventory <- native_catalog_inventory()[1, , drop = FALSE]
  invalid_inventory$path <- "/cache/bad model.gguf"
  invalid_inventory$filename <- "bad model.gguf"

  testthat::local_mocked_bindings(
    .genflow_crispasr_inventory = function(...) invalid_inventory,
    .genflow_list_custom_provider_configs = function() list(),
    .package = "genflow"
  )

  result <- gen_update_models(
    provider = "local-native",
    directory = directory,
    verbose = FALSE,
    fail_on_error = TRUE
  )

  cached <- utils::read.csv(final_path, stringsAsFactors = FALSE)
  expect_identical(
    names(cached),
    c("service", "model", "type", "pricing", "description")
  )
  expect_equal(nrow(cached), 0L)
  expect_false("stale-model.gguf" %in% cached$model)
  expect_identical(attr(result, "updated_providers"), "local-native")
  expect_length(attr(result, "failed_providers"), 0L)
})

test_that("empty catalogs remain forbidden without an explicit mapping opt-in", {
  directory <- tempfile("genflow-empty-catalog-validation-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  path <- file.path(directory, "openai.csv")
  utils::write.csv(
    data.frame(
      service = character(),
      model = character(),
      type = character(),
      pricing = character(),
      description = character(),
      stringsAsFactors = FALSE
    ),
    path,
    row.names = FALSE
  )

  expect_error(
    genflow:::.genflow_validate_catalog_file(
      path,
      provider = "openai"
    ),
    "without model rows",
    fixed = TRUE
  )
  expect_silent(
    genflow:::.genflow_validate_catalog_file(
      path,
      provider = "local-native",
      allow_empty = TRUE
    )
  )
})
