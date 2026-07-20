test_that("worker limits are independent from task quantity", {
  expect_equal(genflow:::.genflow_resolve_workers(2, 5), 2L)
  expect_equal(genflow:::.genflow_resolve_workers(20, 5), 5L)
  expect_error(genflow:::.genflow_resolve_workers(0, 5), "positive integer")
  expect_error(genflow:::.genflow_resolve_workers(1.5, 5), "positive integer")
  expect_error(genflow:::.genflow_resolve_workers(NA, 5), "positive integer")
})

test_that("provider lookup and text runtime share service aliases", {
  aliases <- c(
    claude = "anthropic",
    `llama-cpp` = "llamacpp",
    samba_nova = "sambanova",
    togetherai = "together",
    `deep-seek` = "deepseek",
    deep_infra = "deepinfra",
    `fireworks-ai` = "fireworks",
    pplx = "perplexity"
  )
  resolved <- vapply(names(aliases), function(alias) {
    get_provider(alias)$id
  }, character(1))
  expect_identical(unname(resolved), unname(aliases))
})

test_that("PSOCK workers receive only current batch state", {
  cl <- parallel::makeCluster(2L)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  agent <- set_agent(
    "psock_agent",
    setup = list(service = "mock", model = "mock-model", type = "Vision"),
    content = list(context = "Describe"),
    save = FALSE,
    assign = FALSE
  )

  expect_no_error(genflow:::.export_cluster_vars(
    cl = cl,
    qty = 2L,
    agent_prefix = "psock_agent",
    suffix_type = "numeric",
    instructions = NULL,
    add = NULL,
    add_img = NULL,
    add_img_each = list("one.jpg", "two.jpg"),
    one_item_each = list("one", "two"),
    append_modes = list(instructions = "replace", add = "replace"),
    directory = tempdir(),
    directory_img = tempdir(),
    agent = agent,
    persist = FALSE,
    checkpoint_each = NULL
  ))

  worker_state <- parallel::clusterEvalQ(cl, {
    c(
      task_function = is.function(.execute_agent_task),
      item_count = length(one_item_each) == 2L,
      image_count = length(add_img_each) == 2L,
      has_agent = inherits(agent, "genflow_agent")
    )
  })
  expect_true(all(vapply(worker_state, all, logical(1))))

  raw_results <- parallel::parLapplyLB(cl, 1:2, function(i) {
    .execute_agent_task(
      i, one_item_each, instructions, add, add_img, directory,
      directory_img, agent_prefix, suffix_type, append_modes,
      agent = agent, add_img_each = add_img_each, persist = persist,
      checkpoint_each = checkpoint_each
    )
  })
  expect_length(raw_results, 2L)
  expect_true(all(vapply(raw_results, function(result) {
    is.list(result) && is.null(result$erro)
  }, logical(1))))
  expect_identical(vapply(raw_results, `[[`, character(1), "task_id"), c("1", "2"))
})

mock_text_runtime <- function(context,
                              res_context = TRUE,
                              add = NULL,
                              add_img = NULL,
                              directory = NULL,
                              label = NULL,
                              service = "mock",
                              model = "mock-model",
                              temp = 1,
                              reasoning = NULL,
                              tools = FALSE,
                              plugins = NULL,
                              my_tools = NULL,
                              timeout_api = 240,
                              null_repeat = TRUE,
                              persist = TRUE,
                              ...) {
  list(
    response_value = basename(add_img),
    label = label,
    service = service,
    model = model,
    temp = temp,
    duration = 0.01,
    status_api = "SUCCESS",
    status_msg = "OK",
    reasoning_seen = reasoning,
    timeout_seen = timeout_api,
    persist_seen = persist
  )
}

test_that("one agent runs distinct named image tasks without global clones", {
  td <- tempfile("genflow_batch_")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  images <- file.path(td, c("one.jpg", "two.jpg", "three.jpg"))
  invisible(file.create(images))
  image_items <- stats::setNames(as.list(images), c("image_one", "image_two", "image_three"))
  checkpoints <- file.path(td, paste0("checkpoint_", seq_along(images), ".rds"))
  prefix <- "genflow_single_agent_test"
  agent <- set_agent(
    prefix,
    setup = list(
      service = "mock",
      model = "mock-model",
      type = "Vision",
      reasoning = "high",
      timeout_api = 77
    ),
    content = list(context = "Describe the image"),
    save = FALSE,
    assign = FALSE
  )
  persisted <- 0L
  testthat::local_mocked_bindings(
    gen_txt.default = mock_text_runtime,
    .persist_many_stats = function(...) persisted <<- persisted + 1L,
    .package = "genflow"
  )

  result <- gen_batch_agent(
    agent,
    qty = length(images),
    add_img_each = image_items,
    workers = 2,
    persist = FALSE,
    verbose = FALSE,
    always_fix_errors = FALSE,
    checkpoint_each = checkpoints
  )

  expect_identical(names(result), c(names(image_items), "combined_stats"))
  expect_identical(
    unname(vapply(result[seq_along(images)], `[[`, character(1), "response_value")),
    basename(images)
  )
  expect_true(all(vapply(result[seq_along(images)], `[[`, character(1), "reasoning_seen") == "high"))
  expect_true(all(vapply(result[seq_along(images)], `[[`, numeric(1), "timeout_seen") == 77))
  expect_false(any(vapply(result[seq_along(images)], `[[`, logical(1), "persist_seen")))
  expect_equal(result$combined_stats$workers_used, 2L)
  expect_equal(result$combined_stats$qty_solicited, 3L)
  expect_equal(persisted, 0L)
  expect_true(all(file.exists(checkpoints)))
  expect_identical(
    vapply(lapply(checkpoints, readRDS), `[[`, character(1), "task_id"),
    names(image_items)
  )
  expect_false(any(vapply(seq_along(images), function(i) {
    exists(paste0(prefix, i), envir = .GlobalEnv, inherits = FALSE)
  }, logical(1))))
})

test_that("per-task images validate length, names, and broadcast conflicts", {
  td <- tempfile("genflow_batch_validation_")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  image <- file.path(td, "one.jpg")
  invisible(file.create(image))
  agent <- set_agent(
    "validation_agent",
    setup = list(service = "mock", model = "mock-model", type = "Vision"),
    content = list(context = "Describe"),
    save = FALSE,
    assign = FALSE
  )

  expect_error(
    gen_batch_agent(agent, qty = 2, add_img_each = list(image), verbose = FALSE),
    "exactly 2"
  )
  expect_error(
    gen_batch_agent(
      agent,
      qty = 2,
      add_img = image,
      add_img_each = list(image, image),
      verbose = FALSE
    ),
    "either `add_img` or `add_img_each`"
  )
  expect_error(
    gen_batch_agent(
      agent,
      qty = 2,
      add_img_each = stats::setNames(list(image, image), c("same", "same")),
      verbose = FALSE
    ),
    "complete and unique"
  )
  expect_error(
    gen_batch_agent(
      agent,
      qty = 2,
      add_img_each = stats::setNames(list(image, image), c("combined_stats", "image_two")),
      verbose = FALSE
    ),
    "reserved"
  )
  expect_error(
    gen_batch_agent(
      agent,
      qty = 2,
      add_img_each = stats::setNames(list(image, image), c(NA_character_, NA_character_)),
      verbose = FALSE
    ),
    "complete and unique"
  )
  duplicate_checkpoint <- file.path(td, "same-checkpoint.rds")
  expect_error(
    gen_batch_agent(
      agent,
      qty = 2,
      add_img_each = list(image, image),
      checkpoint_each = list(duplicate_checkpoint, duplicate_checkpoint),
      verbose = FALSE
    ),
    "must be unique"
  )
})

test_that("persist controls direct text response saving", {
  saves <- 0L
  testthat::local_mocked_bindings(
    .gen_txt_openai = function(...) "mock response",
    .save_response = function(...) saves <<- saves + 1L,
    .package = "genflow"
  )

  invisible(gen_txt(
    "hello",
    service = "openai",
    model = "mock-model",
    null_repeat = FALSE,
    persist = FALSE
  ))
  expect_equal(saves, 0L)

  invisible(gen_txt(
    "hello",
    service = "openai",
    model = "mock-model",
    null_repeat = FALSE,
    persist = TRUE
  ))
  expect_equal(saves, 1L)
})

test_that("a failed worker does not shift later named results", {
  td <- tempfile("genflow_batch_error_")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  images <- file.path(td, c("one.jpg", "two.jpg", "three.jpg"))
  invisible(file.create(images))
  image_items <- stats::setNames(as.list(images), c("one", "two", "three"))
  agent <- set_agent(
    "worker_error_agent",
    setup = list(service = "mock", model = "mock-model", type = "Vision"),
    content = list(context = "Describe"),
    save = FALSE,
    assign = FALSE
  )
  failing_runtime <- mock_text_runtime
  body(failing_runtime) <- substitute({
    if (basename(add_img) == "two.jpg") stop("simulated worker error")
    BODY
  }, list(BODY = body(mock_text_runtime)))
  testthat::local_mocked_bindings(
    gen_txt.default = failing_runtime,
    .package = "genflow"
  )

  result <- gen_batch_agent(
    agent,
    qty = 3,
    add_img_each = image_items,
    workers = 1,
    persist = FALSE,
    verbose = FALSE,
    always_fix_errors = FALSE
  )

  expect_identical(names(result), c("one", "two", "three", "combined_stats"))
  expect_equal(result$one$response_value, "one.jpg")
  expect_null(result$two)
  expect_equal(result$three$response_value, "three.jpg")
  expect_match(result$combined_stats$detailed_errors[[2]], "simulated worker error")
})

test_that("always_fix_errors retries only failures without duplicate persistence", {
  td <- tempfile("genflow_retry_")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  images <- file.path(td, c("one.jpg", "two.jpg", "three.jpg"))
  invisible(file.create(images))
  image_items <- stats::setNames(as.list(images), c("one", "two", "three"))
  agent <- set_agent(
    "retry_agent",
    setup = list(service = "mock", model = "mock-model", type = "Vision"),
    content = list(context = "Describe"),
    save = FALSE,
    assign = FALSE
  )
  second_attempts <- 0L
  persisted <- character(0)
  retry_runtime <- mock_text_runtime
  body(retry_runtime) <- substitute({
    if (basename(add_img) == "two.jpg") {
      second_attempts <<- second_attempts + 1L
      if (second_attempts == 1L) stop("temporary provider failure")
    }
    BODY
  }, list(BODY = body(mock_text_runtime)))
  environment(retry_runtime) <- environment()
  testthat::local_mocked_bindings(
    gen_txt.default = retry_runtime,
    .persist_many_stats = function(results) {
      values <- vapply(results, function(item) {
        if (is.list(item) && !is.null(item$response_value)) item$response_value else NA_character_
      }, character(1))
      persisted <<- c(persisted, stats::na.omit(values))
      invisible(TRUE)
    },
    .package = "genflow"
  )

  first <- gen_batch_agent(
    agent,
    qty = 3,
    add_img_each = image_items,
    workers = 1,
    directory = td,
    directory_img = td,
    persist = TRUE,
    verbose = FALSE,
    always_fix_errors = TRUE
  )
  expect_null(first$two)
  expect_warning(
    second <- gen_batch_agent(
      agent,
      qty = 3,
      add_img_each = image_items,
      workers = 1,
      directory = td,
      directory_img = td,
      persist = TRUE,
      verbose = FALSE,
      always_fix_errors = TRUE
    ),
    NA
  )

  expect_equal(second$one$response_value, "one.jpg")
  expect_equal(second$two$response_value, "two.jpg")
  expect_equal(second$three$response_value, "three.jpg")
  expect_identical(second$combined_stats$executed_indices, 2L)
  expect_identical(second$combined_stats$reused_indices, c(1L, 3L))
  expect_identical(unname(sort(persisted)), sort(c("one.jpg", "two.jpg", "three.jpg")))
})
