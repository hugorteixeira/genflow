hf_mapping <- function(provider = "test-provider", status = "live") {
  list(list(
    provider = provider,
    providerId = "provider/model",
    status = status
  ))
}

hf_model <- function(id,
                     task,
                     mappings = hf_mapping(),
                     downloads = 100,
                     likes = 10) {
  list(
    id = id,
    pipeline_tag = task,
    library_name = "transformers",
    downloads = downloads,
    likes = likes,
    lastModified = "2026-07-22T09:00:52.000Z",
    gated = FALSE,
    inferenceProviderMapping = mappings
  )
}

test_that("Hugging Face Link headers expose only the next page URL", {
  link <- paste0(
    "<https://huggingface.co/api/models?cursor=previous>; rel=\"prev\", ",
    "<https://huggingface.co/api/models?cursor=next>; rel=\"next\""
  )

  expect_identical(
    genflow:::.hf_next_page_url(link),
    "https://huggingface.co/api/models?cursor=next"
  )
  expect_null(genflow:::.hf_next_page_url(NULL))
  expect_null(genflow:::.hf_next_page_url("<https://example.test>; rel=\"prev\""))
})

test_that("remote Hugging Face catalogs keep only live inference models", {
  remote_live <- hf_model("org/chat-live", "text-generation")
  remote_dead <- hf_model(
    "org/chat-unavailable",
    "text-generation",
    mappings = hf_mapping(status = "error")
  )

  remote <- genflow:::.hf_models_to_catalog(
    list(remote_live, remote_dead),
    service = "hf",
    require_live_provider = TRUE
  )
  expect_identical(remote$model, "org/chat-live")
  expect_identical(remote$service, "hf")
  expect_identical(remote$type, "Chat")
  expect_match(remote$description, "providers=test-provider", fixed = TRUE)
})

test_that("remote Hugging Face updater follows pagination and writes safe CSV", {
  td <- tempfile("genflow_hf_remote_")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)

  calls <- list()
  pages <- list(
    list(
      items = list(
        hf_model("org/model-one", "text-generation", downloads = 30),
        hf_model("org/model-two", "text-generation", downloads = 20)
      ),
      next_url = "https://huggingface.co/api/models?cursor=next"
    ),
    list(
      items = list(hf_model("org/model-three", "text-generation", downloads = 10)),
      next_url = NULL
    )
  )
  fetch_page <- function(url, query, ...) {
    calls[[length(calls) + 1L]] <<- list(url = url, query = query)
    pages[[length(calls)]]
  }

  output <- genflow:::.update_models_hf(
    directory = td,
    verbose = FALSE,
    tasks = "text-generation",
    limit_per_query = 3L,
    page_size = 2L,
    fetch_page = fetch_page
  )

  expect_equal(nrow(output), 3L)
  expect_length(calls, 2L)
  expect_identical(calls[[1]]$query$inference_provider, "all")
  expect_true(all(lengths(calls[[1]]$query) == 1L))
  expect_equal(sum(names(calls[[1]]$query) == "expand[]"), 7L)
  expect_null(calls[[2]]$query)

  catalog_path <- file.path(td, "hf.csv")
  expect_true(file.exists(catalog_path))
  cached <- utils::read.csv(catalog_path, stringsAsFactors = FALSE)
  expect_setequal(cached$model, c("org/model-one", "org/model-two", "org/model-three"))
  expect_true(all(cached$service == "hf"))
})

test_that("empty Hub responses do not replace an existing Hugging Face catalog", {
  td <- tempfile("genflow_hf_preserve_")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  catalog_path <- file.path(td, "hf.csv")
  writeLines("existing catalog", catalog_path)

  fetch_page <- function(...) list(items = list(), next_url = NULL)
  expect_error(
    genflow:::.update_models_hf(
      directory = td,
      verbose = FALSE,
      tasks = "text-generation",
      limit_per_query = 1L,
      page_size = 1L,
      fetch_page = fetch_page
    ),
    "existing catalog was not changed",
    fixed = TRUE
  )
  expect_identical(readLines(catalog_path), "existing catalog")
})

test_that("gen_update_models dispatches remote Hugging Face aliases only", {
  td <- tempfile("genflow_hf_dispatch_")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  updated <- character()

  testthat::local_mocked_bindings(
    .update_models_hf = function(directory, verbose) {
      updated <<- c(updated, "hf")
      utils::write.csv(
        data.frame(
          service = "hf",
          model = "remote/model",
          type = "Chat",
          pricing = "",
          description = "",
          stringsAsFactors = FALSE
        ),
        file.path(directory, "hf.csv"),
        row.names = FALSE
      )
      invisible(NULL)
    },
    .genflow_list_custom_provider_configs = function() list(),
    .package = "genflow"
  )

  result <- gen_update_models(
    provider = "huggingface",
    directory = td,
    verbose = FALSE,
    fail_on_error = TRUE
  )

  expect_identical(updated, "hf")
  expect_identical(attr(result, "updated_providers"), "hf")
  expect_length(attr(result, "failed_providers"), 0L)
  expect_error(
    gen_update_models(
      provider = c("hf-local", "huggingface-local"),
      directory = td,
      verbose = FALSE
    ),
    "Invalid provider(s): hf-local, huggingface-local",
    fixed = TRUE
  )
})

test_that("retired local Hugging Face aliases never appear in model listings", {
  td <- tempfile("genflow_hf_retired_catalogs_")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)

  retired <- c(
    "hf-local",
    "hf_local",
    "huggingface-local",
    "huggingface_local",
    "transformers"
  )
  catalog_row <- function(provider, model) {
    data.frame(
      provider = provider,
      model = model,
      type = "Audio",
      pricing = "",
      description = "",
      stringsAsFactors = FALSE
    )
  }

  active_catalog <- rbind(
    catalog_row("hf", "remote/kept"),
    do.call(
      rbind,
      lapply(retired, function(alias) {
        catalog_row(alias, paste0("retired-row/", alias))
      })
    )
  )
  utils::write.csv(
    active_catalog,
    file.path(td, "hf.csv"),
    row.names = FALSE
  )

  for (alias in retired) {
    utils::write.csv(
      catalog_row("hf", paste0("retired-file/", alias)),
      file.path(td, paste0(alias, ".csv")),
      row.names = FALSE
    )
  }

  all_models <- gen_show_models(directory = td, verbose = FALSE)
  expect_identical(all_models$provider, "hf")
  expect_identical(all_models$model, "remote/kept")
  expect_false(any(all_models$provider %in% retired))
  expect_false(any(startsWith(all_models$model, "retired-row/")))
  expect_false(any(startsWith(all_models$model, "retired-file/")))

  for (alias in retired) {
    explicit <- suppressMessages(
      gen_show_models(
        provider = alias,
        directory = td,
        verbose = FALSE
      )
    )
    expect_equal(nrow(explicit), 0L, info = alias)
  }
})
