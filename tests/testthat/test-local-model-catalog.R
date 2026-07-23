test_that("Ollama catalog updates use the saved local inference URL", {
  root <- tempfile("genflow-ollama-catalog-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  config_path <- file.path(root, "local-inference.json")
  catalog_dir <- file.path(root, "models")
  old_options <- options(genflow.local_config_path = config_path)
  on.exit(options(old_options), add = TRUE)
  withr::local_envvar(OLLAMA_BASE_URL = NA)

  gen_local_config(ollama_base_url = "http://127.0.0.1:22434")
  requested_url <- NULL
  fetch_tags <- function(api_url) {
    requested_url <<- api_url
    list(models = data.frame(
      name = "local-test:latest",
      stringsAsFactors = FALSE
    ))
  }

  result <- genflow:::.update_models_ollama(
    directory = catalog_dir,
    verbose = FALSE,
    fetch_tags = fetch_tags
  )

  expect_identical(
    requested_url,
    "http://127.0.0.1:22434/api/tags"
  )
  expect_identical(result$model, "local-test:latest")
  expect_true(file.exists(file.path(catalog_dir, "ollama.csv")))
})
