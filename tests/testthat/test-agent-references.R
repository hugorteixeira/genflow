test_that("setup and content renames keep saved agent references valid", {
  cache_dir <- tempfile("genflow-cache-")
  old_cache <- getOption("genflow.cache_dir")
  options(genflow.cache_dir = cache_dir)
  on.exit(options(genflow.cache_dir = old_cache), add = TRUE)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  set_setup(
    "setup-old",
    service = "openai",
    model = "gpt-test",
    assign = FALSE
  )
  set_content("content-old", context = "context", assign = FALSE)
  set_agent(
    "agent-one",
    setup = "setup-old",
    content = "content-old",
    assign = FALSE
  )

  mv_setup("setup-old", "setup-new")
  mv_content("content-old", "content-new")

  agent <- get_agent("agent-one")
  expect_equal(agent$sname, "setup-new")
  expect_equal(agent$cname, "content-new")
  expect_equal(agent$service, "openai")
  expect_equal(agent$context, "context")
  expect_error(get_setup("setup-old"), "No cached setup")
  expect_error(get_content("content-old"), "No cached content")
})

test_that("referenced resources cannot be deleted accidentally", {
  cache_dir <- tempfile("genflow-cache-")
  old_cache <- getOption("genflow.cache_dir")
  options(genflow.cache_dir = cache_dir)
  on.exit(options(genflow.cache_dir = old_cache), add = TRUE)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  set_setup("guarded-setup", "openai", "gpt-test", assign = FALSE)
  set_content("guarded-content", context = "context", assign = FALSE)
  set_agent(
    "dependent-agent",
    setup = "guarded-setup",
    content = "guarded-content",
    assign = FALSE
  )

  expect_error(
    rm_setup("guarded-setup"),
    "referenced by 1 saved agent"
  )
  expect_error(
    rm_content("guarded-content"),
    "referenced by 1 saved agent"
  )

  expect_true(rm_setup("guarded-setup", force = TRUE))
  expect_true(rm_content("guarded-content", force = TRUE))
  agent <- get_agent("dependent-agent")
  expect_null(agent$sname)
  expect_null(agent$cname)
  expect_equal(agent$service, "openai")
  expect_equal(agent$context, "context")
})

test_that("cache filenames do not collide after sanitizing, case folding, or truncation", {
  cache_dir <- tempfile("genflow-cache-")
  old_cache <- getOption("genflow.cache_dir")
  options(genflow.cache_dir = cache_dir)
  on.exit(options(genflow.cache_dir = old_cache), add = TRUE)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  names <- c(
    "Same/Name",
    "Same\\Name",
    "same_name",
    "SAME_NAME",
    paste0(strrep("long", 40), "-one"),
    paste0(strrep("long", 40), "-two")
  )
  for (index in seq_along(names)) {
    set_setup(
      names[[index]],
      service = "mock",
      model = paste0("model-", index),
      assign = FALSE,
      overwrite = FALSE
    )
  }

  loaded <- vapply(
    names,
    function(name) get_setup(name)$model,
    character(1)
  )
  expect_identical(unname(loaded), paste0("model-", seq_along(names)))
  expect_equal(
    length(list.files(file.path(cache_dir, "setups"), pattern = "\\.rds$")),
    length(names)
  )
})

test_that("legacy cache entries are read and migrated on update", {
  cache_dir <- tempfile("genflow-cache-")
  old_cache <- getOption("genflow.cache_dir")
  options(genflow.cache_dir = cache_dir)
  on.exit(options(genflow.cache_dir = old_cache), add = TRUE)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  name <- "Legacy Setup"
  legacy_path <- genflow:::.genflow_legacy_entity_path("setups", name)
  saveRDS(
    list(sname = name, service = "mock", model = "old"),
    legacy_path
  )
  expect_equal(get_setup(name)$model, "old")

  set_setup(name, "mock", "new", assign = FALSE, overwrite = TRUE)
  expect_equal(get_setup(name)$model, "new")
  expect_false(file.exists(legacy_path))
  expect_true(file.exists(genflow:::.genflow_setup_path(name)))
})

test_that("cache paths and names fail clearly on invalid configuration", {
  old_cache <- getOption("genflow.cache_dir")
  on.exit(options(genflow.cache_dir = old_cache), add = TRUE)

  options(genflow.cache_dir = c("/tmp/one", "/tmp/two"))
  expect_error(
    genflow:::.genflow_cache_dir(),
    "must be one non-empty path",
    fixed = TRUE
  )
  expect_error(
    genflow:::.genflow_validate_name(NA_character_, "agent"),
    "must be a non-empty character string",
    fixed = TRUE
  )
  expect_error(
    genflow:::.genflow_validate_name("   ", "agent"),
    "must be a non-empty character string",
    fixed = TRUE
  )
})

test_that("agent rename is atomic and corrupt cache entries fail clearly", {
  cache_dir <- tempfile("genflow-cache-agent-")
  dir.create(cache_dir)
  old_cache <- getOption("genflow.cache_dir")
  options(genflow.cache_dir = cache_dir)
  on.exit(options(genflow.cache_dir = old_cache), add = TRUE)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  agent <- set_agent(
    "before",
    setup = list(service = "openai", model = "test"),
    assign = FALSE
  )
  expect_s3_class(agent, "genflow_agent")
  expect_true(mv_agent("before", "after"))
  expect_false(file.exists(
    genflow:::.genflow_agent_path("before", existing = TRUE)
  ))
  expect_identical(get_agent("after")$name, "after")
  expect_true(rm_agent("after"))
  expect_warning(expect_false(rm_agent("after")), "No cached agent")

  corrupt <- genflow:::.genflow_agent_path("broken")
  writeBin(as.raw(c(1, 2, 3)), corrupt)
  expect_error(
    get_agent("broken"),
    "Could not read cached agent",
    fixed = TRUE
  )
})
