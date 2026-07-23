test_that("Python bridge propagates Hugging Face revisions to both loaders", {
  python <- unname(Sys.which(c("python3", "python")))
  python <- python[nzchar(python)]
  skip_if(length(python) == 0L, "Python is not available")

  bridge <- genflow:::.stt_bridge_script()
  test_script <- normalizePath(
    testthat::test_path("..", "python", "test_genflow_stt_revision.py"),
    winslash = "/",
    mustWork = TRUE
  )
  output <- suppressWarnings(system2(
    python[[1]],
    c(shQuote(test_script), shQuote(bridge)),
    stdout = TRUE,
    stderr = TRUE,
    env = "PYTHONDONTWRITEBYTECODE=1"
  ))
  status <- as.integer(attr(output, "status") %||% 0L)

  expect_identical(
    status,
    0L,
    info = paste(output, collapse = "\n")
  )
  expect_true(
    any(grepl("^OK$", trimws(output))),
    info = paste(output, collapse = "\n")
  )
})
