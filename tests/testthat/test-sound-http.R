test_that("real HTTP, credentials, agents and PSOCK sound batches use one submission each", {
  skip_if(!nzchar(Sys.which("python3")))
  skip_if(!nzchar(Sys.which("ffmpeg")))
  root <- tempfile("sound-http-"); dir.create(root); on.exit(unlink(root,recursive=TRUE),add=TRUE)
  wav <- file.path(root,"fixture.wav")
  processx::run("ffmpeg",c("-v","error","-y","-f","lavfi","-i","sine=frequency=220:duration=0.2",wav),timeout=10000)
  port_file <- file.path(root,"port"); log <- file.path(root,"calls.jsonl")
  server <- processx::process$new("python3",c(test_path("fixtures","sound-api.py"),wav,port_file,log),stdout="|",stderr="|")
  on.exit(server$kill(),add=TRUE)
  for (i in 1:100) { if (file.exists(port_file) || !server$is_alive()) break; Sys.sleep(0.02) }
  if (!file.exists(port_file)) stop(server$read_all_error())
  url <- paste0("http://127.0.0.1:",readLines(port_file,warn=FALSE))
  withr::local_envvar(c(ELEVENLABS_API_KEY="fixture-eleven",ELEVENLABS_BASE_URL=url,
    ACESTEP_API_KEY="fixture-local",ACESTEP_BASE_URL=url,genflow_SKIP_PERSIST_LOG="1"))
  withr::local_options(genflow.output_dir=root,genflow.cache_dir=file.path(root,"cache"),genflow.log_dir=root)
  expect_identical(gen_sfx("asas",persist=FALSE)$status_api,"SUCCESS")
  expect_identical(gen_music("piano",service="acestep",persist=FALSE)$status_api,"SUCCESS")
  for (kind in c("Music","SFX")) {
    agent <- set_agent(paste0("sound-",kind),setup=list(service="elevenlabs",type=kind),
      content=list(context="piano"),save=FALSE,assign=FALSE)
    checkpoints <- file.path(root,paste0(kind,1:2,".rds"))
    value <- gen_batch_agent(agent,qty=2L,workers=2L,persist=FALSE,verbose=FALSE,
      always_fix_errors=FALSE,checkpoint_each=checkpoints)
    expect_equal(value$combined_stats$workers_used,2L)
    expect_true(all(file.exists(checkpoints)))
    for (cp in checkpoints) {
      result <- readRDS(cp)$response
      expect_identical(result$status_api,"SUCCESS")
      expect_identical(result$content_type,"audio")
      expect_true(file.exists(result$response_value))
      expect_true(startsWith(result$response_value,tempdir()))
    }
  }
  # Unknown generation outcome is distinct from a rejected request and from
  # successful generation whose downloaded payload is invalid.
  failed <- gen_music("http503",persist=FALSE)
  expect_identical(failed$error$code,"sound_http"); expect_identical(failed$error$outcome,"unknown")
  expect_null(failed$audio_receipt)
  invalid <- gen_sfx("html",persist=FALSE)
  expect_identical(invalid$inference_status,"SUCCESS"); expect_identical(invalid$error$code,"sound_invalid")
  expect_identical(gen_audio_recover(invalid,persist=FALSE)$error$code,"sound_invalid")
  redirect <- gen_sfx("redirect",persist=FALSE)
  expect_identical(redirect$error$code,"sound_http")
  rows <- lapply(readLines(log,warn=FALSE),jsonlite::fromJSON)
  expect_equal(sum(vapply(rows,function(r) r$path=="/release_task",logical(1))),1)
  expect_equal(sum(vapply(rows,function(r) startsWith(r$path,"/v1/music?"),logical(1))),3)
  expect_false(any(vapply(rows,function(r) r$path=="/redirect-target",logical(1))))
})
