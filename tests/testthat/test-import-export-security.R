test_that("bundle archive validation rejects traversal and unexpected files", {
  valid <- data.frame(
    Name = c(
      "genflow_bundle/",
      "genflow_bundle/metadata.json",
      "genflow_bundle/cache/setups/setup.rds"
    ),
    Length = c(0, 100, 200),
    stringsAsFactors = FALSE
  )
  result <- genflow:::.genflow_validate_zip_entries(valid)
  expect_equal(result$root, "genflow_bundle")

  traversal <- valid
  traversal$Name[[3]] <- "genflow_bundle/../outside.rds"
  expect_error(
    genflow:::.genflow_validate_zip_entries(traversal),
    "Unsafe path"
  )

  executable <- valid
  executable$Name[[3]] <- "genflow_bundle/models/payload.R"
  expect_error(
    genflow:::.genflow_validate_zip_entries(executable),
    "Unexpected file"
  )

  duplicate <- rbind(valid, valid[2, , drop = FALSE])
  expect_error(
    genflow:::.genflow_validate_zip_entries(duplicate),
    "duplicate paths"
  )
})

test_that("bundle safety limits are enforced before extraction", {
  entries <- data.frame(
    Name = c("bundle/", "bundle/metadata.json"),
    Length = c(0, 1024),
    stringsAsFactors = FALSE
  )
  expect_error(
    genflow:::.genflow_validate_zip_entries(entries, max_file_bytes = 100),
    "per-file size limit"
  )
  expect_error(
    genflow:::.genflow_validate_zip_entries(entries, max_entries = 1),
    "too many entries"
  )
})

test_that("exported bundles pass safe extraction and schema validation", {
  skip_if(!nzchar(Sys.which("zip")), "The zip executable is unavailable")

  source_cache <- tempfile("genflow-source-cache-")
  target_cache <- tempfile("genflow-target-cache-")
  source_models <- tempfile("genflow-source-models-")
  target_models <- tempfile("genflow-target-models-")
  bundle <- tempfile(fileext = ".zip")
  dir.create(source_models, recursive = TRUE)
  write.csv(
    data.frame(service = "openai", model = "test-model"),
    file.path(source_models, "openai.csv"),
    row.names = FALSE
  )
  old_cache <- getOption("genflow.cache_dir")
  on.exit(options(genflow.cache_dir = old_cache), add = TRUE)
  on.exit(
    unlink(
      c(source_cache, target_cache, source_models, target_models, bundle),
      recursive = TRUE
    ),
    add = TRUE
  )

  options(genflow.cache_dir = source_cache)
  set_setup("safe-setup", "openai", "test-model", assign = FALSE)
  set_content("safe-content", context = "safe", assign = FALSE)
  set_agent(
    "safe-agent",
    setup = "safe-setup",
    content = "safe-content",
    assign = FALSE
  )
  gen_export_bundle(bundle, models_dir = source_models, quiet = TRUE)

  options(genflow.cache_dir = target_cache)
  imported <- gen_import_bundle(
    bundle,
    models_dir = target_models,
    quiet = TRUE
  )
  expect_equal(imported$counts$setups, 1L)
  expect_equal(imported$counts$agents, 1L)
  expect_equal(imported$counts$content, 1L)
  expect_equal(get_agent("safe-agent")$sname, "safe-setup")
  expect_true(file.exists(file.path(target_models, "openai.csv")))
})
