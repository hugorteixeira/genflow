image_test_envvar <- function(values) {
  names_values <- names(values)
  old <- Sys.getenv(names_values, unset = NA_character_)
  names(old) <- names_values
  do.call(Sys.setenv, as.list(values))
  function() {
    missing <- names_values[is.na(old)]
    if (length(missing)) Sys.unsetenv(missing)
    present <- old[!is.na(old)]
    if (length(present)) do.call(Sys.setenv, as.list(present))
  }
}

test_that("gen_img dispatches OpenAI and selects its provider default", {
  output_dir <- tempfile("genflow-openai-dispatch-")
  dir.create(output_dir)
  on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)
  restore_env <- image_test_envvar(c(genflow_SKIP_PERSIST_LOG = "1"))
  on.exit(restore_env(), add = TRUE)
  seen <- NULL

  testthat::local_mocked_bindings(
    .gen_img_openai = function(prompt,
                               model,
                               temp,
                               steps,
                               h,
                               y,
                               directory,
                               label_sanitized,
                               ...) {
      seen <<- list(
        prompt = prompt,
        model = model,
        width = y,
        height = h
      )
      path <- file.path(directory, "openai-dispatch.png")
      writeBin(as.raw(c(1, 2, 3)), path)
      path
    },
    .package = "genflow"
  )

  result <- gen_img(
    "A test image",
    service = "OPENAI",
    model = NULL,
    directory = output_dir
  )

  expect_identical(result$status_api, "SUCCESS")
  expect_identical(result$service, "openai")
  expect_identical(result$model, "gpt-image-2")
  expect_true(file.exists(result$response_value))
  expect_identical(seen$model, "gpt-image-2")
})

test_that("OpenAI adapter sends a model-valid size and saves URL output", {
  output_dir <- tempfile("genflow-openai-request-")
  dir.create(output_dir)
  on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)
  restore_env <- image_test_envvar(c(OPENAI_API_KEY = "test-openai-key"))
  on.exit(restore_env(), add = TRUE)
  calls <- list()

  fake_request <- function(method,
                           url,
                           api_key = NULL,
                           body = NULL,
                           timeout_secs = 300) {
    calls[[length(calls) + 1L]] <<- list(
      method = method,
      url = url,
      api_key = api_key,
      body = body
    )
    if (identical(method, "POST")) {
      return(list(
        status = 200L,
        text = '{"data":[{"url":"https://images.example/result.png"}]}'
      ))
    }
    list(status = 200L, raw = as.raw(c(137, 80, 78, 71)))
  }

  path <- genflow:::.gen_img_openai(
    prompt = "A portrait",
    model = "gpt-image-2",
    temp = 5,
    steps = 18,
    h = 1536,
    y = 1024,
    directory = output_dir,
    label_sanitized = "portrait",
    request = fake_request
  )

  expect_true(file.exists(path))
  expect_identical(readBin(path, "raw", n = 4L), as.raw(c(137, 80, 78, 71)))
  expect_identical(calls[[1]]$method, "POST")
  expect_identical(
    calls[[1]]$url,
    "https://api.openai.com/v1/images/generations"
  )
  expect_identical(calls[[1]]$body$size, "1024x1536")
  expect_identical(calls[[1]]$body$model, "gpt-image-2")
  expect_identical(calls[[2]]$method, "GET")
})

test_that("OpenAI image sizes are resolved per model family", {
  expect_identical(
    genflow:::.gen_img_openai_size("gpt-image-1-mini", 1500, 1000),
    "1536x1024"
  )
  expect_identical(
    genflow:::.gen_img_openai_size("dall-e-3", 1800, 1000),
    "1792x1024"
  )
  expect_identical(
    genflow:::.gen_img_openai_size("dall-e-2", 500, 700),
    "512x512"
  )
  expect_error(
    genflow:::.gen_img_openai_size("not-an-image-model", 1024, 1024),
    "Unsupported OpenAI image model"
  )
})

test_that("FAL queue flow uses the current model path and initial status", {
  output_dir <- tempfile("genflow-fal-queue-")
  dir.create(output_dir)
  on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)
  restore_env <- image_test_envvar(c(FAL_API_KEY = "test-fal-key"))
  on.exit(restore_env(), add = TRUE)
  calls <- list()

  fake_request <- function(method,
                           url,
                           token = NULL,
                           body = NULL,
                           timeout_secs = 300) {
    calls[[length(calls) + 1L]] <<- list(
      method = method,
      url = url,
      token = token,
      body = body
    )
    switch(method,
      "POST" = list(
        status = 202L,
        content = list(
          request_id = "fal-1",
          status_url = "https://queue.fal.run/status/fal-1",
          response_url = "https://queue.fal.run/response/fal-1"
        ),
        text = ""
      ),
      "POLL" = list(
        status = 200L,
        content = list(status = "COMPLETED"),
        text = ""
      ),
      "RESULT" = list(
        status = 200L,
        content = list(
          data = list(
            images = list(list(url = "https://files.example/fal.png"))
          )
        ),
        text = ""
      ),
      "DOWNLOAD" = list(
        status = 200L,
        raw = as.raw(c(137, 80, 78, 71))
      )
    )
  }

  path <- genflow:::.gen_img_fal(
    prompt = "A fast image",
    model = "fal-ai/flux/schnell",
    temp = 5,
    steps = 18,
    h = 1024,
    y = 1536,
    directory = output_dir,
    label_sanitized = "fal",
    poll_interval = 0,
    request = fake_request,
    sleep = function(...) NULL
  )

  expect_true(file.exists(path))
  expect_identical(
    calls[[1]]$url,
    "https://queue.fal.run/fal-ai/flux/schnell"
  )
  expect_true(is.list(calls[[1]]$body))
  expect_identical(calls[[1]]$body$num_inference_steps, 12L)
  expect_identical(calls[[1]]$body$output_format, "png")
  expect_identical(calls[[1]]$body$num_images, 1L)
  expect_identical(
    vapply(calls, `[[`, character(1), "method"),
    c("POST", "POLL", "RESULT", "DOWNLOAD")
  )
})

test_that("FAL model ids accept catalog and shorthand forms", {
  expect_identical(
    genflow:::.gen_img_default_model("fal"),
    "fal-ai/flux/schnell"
  )
  expect_identical(
    genflow:::.gen_img_fal_model_path("fal-ai/flux/schnell"),
    "flux/schnell"
  )
  expect_identical(
    genflow:::.gen_img_fal_model_path("flux/schnell"),
    "flux/schnell"
  )
  expect_identical(
    genflow:::.gen_img_fal_model_path("fal-ai/fast-sdxl"),
    "fast-sdxl"
  )
  expect_error(
    genflow:::.gen_img_fal_model_path(" "),
    "must be a path"
  )
})

test_that("Replicate accepts an already-succeeded initial prediction", {
  output_dir <- tempfile("genflow-replicate-sync-")
  dir.create(output_dir)
  on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)
  restore_env <- image_test_envvar(c(REPLICATE_API_TOKEN = "test-replicate-token"))
  on.exit(restore_env(), add = TRUE)
  calls <- list()

  fake_request <- function(method,
                           url,
                           token = NULL,
                           body = NULL,
                           timeout_secs = 300) {
    calls[[length(calls) + 1L]] <<- list(
      method = method,
      url = url,
      token = token,
      body = body
    )
    if (identical(method, "POST")) {
      return(list(
        status = 201L,
        content = list(
          status = "succeeded",
          output = list("https://files.example/custom.png")
        ),
        text = ""
      ))
    }
    if (identical(method, "DOWNLOAD")) {
      return(list(status = 200L, raw = as.raw(c(1, 2, 3, 4))))
    }
    stop("Polling must not run for an already-succeeded prediction.")
  }

  path <- genflow:::.gen_img_replicate(
    prompt = "A custom model prompt",
    model = "acme/custom-image",
    temp = 6,
    steps = 22,
    h = 768,
    y = 1024,
    directory = output_dir,
    label_sanitized = "custom",
    replicate_input = list(seed = 42L),
    request = fake_request,
    sleep = function(...) stop("sleep must not run")
  )

  expect_true(file.exists(path))
  expect_identical(
    calls[[1]]$url,
    "https://api.replicate.com/v1/predictions"
  )
  expect_identical(calls[[1]]$body$version, "acme/custom-image")
  expect_identical(calls[[1]]$body$input$prompt, "A custom model prompt")
  expect_identical(calls[[1]]$body$input$seed, 42L)
  expect_false("guidance_scale" %in% names(calls[[1]]$body$input))
  expect_false("aspect_ratio" %in% names(calls[[1]]$body$input))
  expect_identical(vapply(calls, `[[`, character(1), "method"), c("POST", "DOWNLOAD"))
  expect_identical(calls[[2]]$token, "test-replicate-token")
})

test_that("Replicate sends an explicit model version to the universal endpoint", {
  output_dir <- tempfile("genflow-replicate-version-")
  dir.create(output_dir)
  on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)
  restore_env <- image_test_envvar(c(REPLICATE_API_TOKEN = "test-replicate-token"))
  on.exit(restore_env(), add = TRUE)
  initial <- NULL

  fake_request <- function(method,
                           url,
                           token = NULL,
                           body = NULL,
                           timeout_secs = 300) {
    if (identical(method, "POST")) {
      initial <<- list(url = url, body = body)
      return(list(
        status = 201L,
        content = list(
          status = "successful",
          output = "https://files.example/versioned.png"
        ),
        text = ""
      ))
    }
    list(status = 200L, raw = as.raw(c(5, 6, 7)))
  }

  path <- genflow:::.gen_img_replicate(
    prompt = "Pinned generation",
    model = "acme/versioned-image",
    model_version = "abc123",
    temp = 5,
    steps = 18,
    h = 1024,
    y = 1024,
    directory = output_dir,
    label_sanitized = "versioned",
    request = fake_request,
    sleep = function(...) NULL
  )

  expect_true(file.exists(path))
  expect_identical(initial$url, "https://api.replicate.com/v1/predictions")
  expect_identical(initial$body$version, "acme/versioned-image:abc123")
  expect_identical(
    genflow:::.gen_img_replicate_model_ref(
      "acme/versioned-image:abc123",
      NULL
    )$version,
    "abc123"
  )
  expect_error(
    genflow:::.gen_img_replicate_model_ref(
      "acme/versioned-image:abc123",
      "different"
    ),
    "conflicts"
  )
})

test_that("Replicate polling carries the final prediction object forward", {
  output_dir <- tempfile("genflow-replicate-poll-")
  dir.create(output_dir)
  on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)
  restore_env <- image_test_envvar(c(REPLICATE_API_TOKEN = "test-replicate-token"))
  on.exit(restore_env(), add = TRUE)
  methods <- character()

  fake_request <- function(method,
                           url,
                           token = NULL,
                           body = NULL,
                           timeout_secs = 300) {
    methods <<- c(methods, method)
    switch(method,
      "POST" = list(
        status = 201L,
        content = list(
          status = "starting",
          output = NULL,
          urls = list(get = "https://api.replicate.com/v1/predictions/p1")
        ),
        text = ""
      ),
      "POLL" = list(
        status = 200L,
        content = list(
          status = "succeeded",
          output = list(list(url = "https://files.example/polled.png"))
        ),
        text = ""
      ),
      "DOWNLOAD" = list(status = 200L, raw = as.raw(c(8, 9, 10)))
    )
  }

  path <- genflow:::.gen_img_replicate(
    prompt = "Polling",
    model = "acme/async-image",
    temp = 5,
    steps = 18,
    h = 1024,
    y = 1024,
    directory = output_dir,
    label_sanitized = "polling",
    poll_interval = 0,
    request = fake_request,
    sleep = function(...) NULL
  )

  expect_true(file.exists(path))
  expect_identical(methods, c("POST", "POLL", "DOWNLOAD"))
})

test_that("Replicate Flux schema is limited to the known Flux model", {
  flux <- genflow:::.gen_img_replicate_input(
    model = "black-forest-labs/flux-schnell",
    prompt = "Flux",
    temp = 7,
    steps = 18,
    h = 1024,
    y = 1920
  )
  generic <- genflow:::.gen_img_replicate_input(
    model = "acme/other",
    prompt = "Generic",
    temp = 7,
    steps = 18,
    h = 1024,
    y = 1920
  )

  expect_identical(flux$num_inference_steps, 4L)
  expect_identical(flux$aspect_ratio, "16:9")
  expect_true(all(c(
    "go_fast",
    "megapixels",
    "num_outputs",
    "output_format",
    "output_quality",
    "disable_safety_checker"
  ) %in% names(flux)))
  expect_false(any(c(
    "height",
    "width",
    "guidance_scale",
    "safety_tolerance"
  ) %in% names(flux)))
  expect_identical(generic, list(prompt = "Generic"))
  expect_error(
    genflow:::.gen_img_replicate_input(
      "acme/other", "Generic", 7, 18, 1024, 1024,
      overrides = list(42)
    ),
    "named list"
  )
})
