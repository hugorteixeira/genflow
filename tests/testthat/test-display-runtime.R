test_that("viewer media sources embed small files and copy large files", {
  assets <- tempfile("genflow-view-assets-")
  dir.create(assets)
  on.exit(unlink(assets, recursive = TRUE), add = TRUE)

  media <- tempfile(fileext = ".wav")
  writeBin(as.raw(1:16), media)
  on.exit(unlink(media), add = TRUE)

  embedded <- genflow:::.genflow_view_media_source(
    media,
    "audio/wav",
    assets_dir = assets,
    inline_limit = 1024
  )
  expect_true(embedded$embedded)
  expect_match(embedded$src, "^data:audio/wav;base64,")

  copied <- genflow:::.genflow_view_media_source(
    media,
    "audio/wav",
    assets_dir = assets,
    inline_limit = 0
  )
  expect_false(copied$embedded)
  expect_true(copied$relative)
  expect_match(copied$src, "^\\./genflow_media_")
  expect_true(file.exists(file.path(assets, sub("^\\./", "", copied$src))))
})

test_that("viewer file URIs preserve separators and escape reserved characters", {
  root <- tempfile("genflow view #")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  path <- file.path(root, "media #1.wav")
  writeBin(as.raw(1), path)

  uri <- genflow:::.genflow_view_file_uri(path)
  expect_match(uri, "^file:///")
  expect_match(uri, "%20")
  expect_match(uri, "%23")
  expect_false(grepl("%2F", uri, fixed = TRUE))
})

test_that("viewer pruning only removes old generated asset directories", {
  root <- tempfile("genflow-view-history-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  generated <- file.path(
    root,
    sprintf("view_20260101_00000%d_abc12%d", 1:4, 1:4)
  )
  vapply(generated, dir.create, logical(1))
  unrelated <- file.path(root, "keep-me")
  dir.create(unrelated)
  times <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC") + seq_along(generated)
  invisible(Map(Sys.setFileTime, generated, times))

  removed <- genflow:::.genflow_prune_viewer_assets(root, keep = 2L)
  expect_equal(removed, 2L)
  expect_false(any(dir.exists(generated[1:2])))
  expect_true(all(dir.exists(generated[3:4])))
  expect_true(dir.exists(unrelated))
})

test_that("console gen_view does not create assets and restores caller options", {
  output_root <- tempfile("genflow-view-output-")
  dir.create(output_root)
  on.exit(unlink(output_root, recursive = TRUE), add = TRUE)

  previous <- options(
    genflow.output_dir = output_root,
    genflow_viewer_assets_dir = "caller-assets",
    genflow_view_in_rstudio = "caller-mode"
  )
  on.exit(options(previous), add = TRUE)

  expect_invisible(gen_view("hello", stats = "hide"))
  expect_false(dir.exists(file.path(output_root, "viewer_assets")))
  expect_identical(getOption("genflow_viewer_assets_dir"), "caller-assets")
  expect_identical(getOption("genflow_view_in_rstudio"), "caller-mode")
})
