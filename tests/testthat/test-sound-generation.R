sound_fixture <- function(format="wav") {
  skip_if(!nzchar(Sys.which("ffmpeg")))
  path <- tempfile(fileext=paste0(".",format))
  processx::run("ffmpeg",c("-v","error","-y","-f","lavfi","-i","sine=frequency=220:duration=0.2",path),timeout=10000)
  path
}
sound_response <- function(data) structure(list(url="http://fixture.invalid/",status_code=200L,headers=list(`content-type`="application/json"),
  content=charToRaw(jsonlite::toJSON(list(code=200L,data=data,error=NULL),auto_unbox=TRUE,null="null"))),class="response")

test_that("sound schemas and provider rules accumulate errors before inference", {
  expect_true(gen_validate_call("gen_sfx",list(prompt="asas"))$valid)
  expect_identical(gen_validate_call("gen_music",list(prompt="piano",service="acestep"))$args$model,"acestep-v15-turbo")
  cases <- list(
    list("gen_sfx",list(prompt="",duration_seconds=31,loop="yes",prompt_influence=-1,typo=1),5),
    list("gen_music",list(prompt="piano",instrumental=TRUE,lyrics="Oi",seed=-1),2),
    list("gen_music",list(prompt="piano",instrumental=FALSE),1),
    list("gen_music",list(prompt="piano",seed=1,format="wav"),2),
    list("gen_music",list(prompt="piano",service="acestep",duration_seconds=3),1),
    list("gen_sfx",list(prompt="asa",service="openai"),1),
    list("gen_sfx",list(prompt="asa",model="music_v2"),1),
    list("gen_music",list(prompt="piano",instrumental=FALSE,lyrics=paste(rep("x",31),collapse="\n"),duration_seconds=121),2))
  for (item in cases) {
    checked <- gen_validate_call(item[[1]],item[[2]])
    expect_false(checked$valid); expect_gte(length(checked$errors),item[[3]])
    for (error in checked$errors) {
      expect_false(error$message_key %in% c("unclassified_error","internal_error"))
      expect_match(error$path,"^/args/")
    }
  }
  for (url in c("file:///tmp/audio","http://user:pass@localhost","http://localhost?key=bad","http://localhost/#x"))
    expect_false(gen_validate_call("gen_music",list(prompt="piano",base_url=url))$valid)
  expect_true(gen_validate_call("gen_music",list(prompt="piano",instrumental=FALSE),bound="lyrics")$valid)
  expect_false(gen_validate_call("gen_music",list(prompt="piano",lyrics="oi"),bound="lyrics")$valid)
  expect_false(gen_validate_call("gen_music",list(prompt="piano",instrumental=TRUE),bound="lyrics")$valid)
  expect_true(gen_validate_agent_definition(list(operation="audio.music",service="elevenlabs"))$valid)
  expect_error(gen_music(),class="genflow_validation_error")
  expect_error(gen_sfx(prompt="",loop=2),class="genflow_validation_error")
})

test_that("audio bytes are validated and format and elapsed time are distinct", {
  path <- sound_fixture(); on.exit(unlink(path))
  budget <- genflow:::.gen_sound_budget(30)
  meta <- genflow:::.gen_sound_inspect(path,budget)
  expect_equal(meta$duration_seconds,0.2,tolerance=0.02)
  expect_identical(meta$format,"wav")
  for (bytes in list(raw(),charToRaw('{"error":"not audio"}'),charToRaw("RIFF0000WAVEbad"))) {
    bad <- tempfile(); writeBin(bytes,bad)
    expect_error(genflow:::.gen_sound_inspect(bad,budget),class="genflow_error"); unlink(bad)
  }
  time <- as.POSIXct("2026-01-01",tz="UTC")
  b <- genflow:::.gen_sound_budget(2,function() time)
  expect_equal(b(),2); time <- time+3
  expect_error(b(),class="genflow_error")
})

test_that("ElevenLabs bodies preserve explicit lyrics and contain no invented options", {
  withr::local_envvar(c(ELEVENLABS_API_KEY="fixture"))
  path <- sound_fixture(); on.exit(unlink(path)); calls <- list()
  local_mocked_bindings(.gen_sound_http=function(service,base_url,method,route,body=NULL,budget,path=NULL) {
    calls[[length(calls)+1L]] <<- list(route=route,body=body)
    file.copy(.env$audio,path); sound_response(list())
  },.package="genflow")
  .env <- new.env(); .env$audio <- path
  sfx <- gen_sfx("asas",duration_seconds=2,loop=TRUE,persist=FALSE)
  expect_identical(sfx$status_api,"SUCCESS"); expect_identical(sfx$inference_status,"SUCCESS")
  expect_identical(sfx$content_type,"audio"); expect_identical(sfx$persistence$status,"temporary")
  expect_identical(sfx$audio$format,"wav"); expect_identical(sfx$audio$requested_format,"mp3")
  expect_equal(sfx$audio$duration_seconds,0.2,tolerance=0.02)
  expect_match(calls[[1]]$route,"sound-generation[?]output_format=mp3_44100_128$")
  expect_true(calls[[1]]$body$loop); expect_equal(calls[[1]]$body$duration_seconds,2)
  music <- gen_music("piano",persist=FALSE)
  expect_identical(music$status_api,"SUCCESS")
  expect_true(calls[[2]]$body$force_instrumental); expect_equal(calls[[2]]$body$music_length_ms,30000)
  expect_null(calls[[2]]$body$composition_plan)
  for (model in c("music_v1","music_v2","music_v2_5")) {
    value <- gen_music("piano",instrumental=FALSE,lyrics="Olá\nAmigo",model=model,seed=9,persist=FALSE)
    expect_identical(value$status_api,"SUCCESS")
    body <- calls[[length(calls)]]$body
    expect_null(body$prompt); expect_null(body$force_instrumental); expect_null(body$music_length_ms)
    expect_equal(body$seed,9)
    if (model=="music_v1") expect_identical(body$composition_plan$sections[[1]]$lines,list("Olá","Amigo")) else
      expect_identical(body$composition_plan$chunks[[1]]$text,"Olá\nAmigo")
  }
  count <- length(calls)
  expect_error(gen_music(paste(rep("a",4101),collapse=""),persist=FALSE),class="genflow_validation_error")
  expect_identical(length(calls),count)
  agent <- structure(list(type="Music",service="elevenlabs",context="piano"),class="genflow_agent")
  expect_identical(gen_music(agent,persist=FALSE)$status_api,"SUCCESS")
  expect_identical(gen_music(agent,prompt_override="flauta",persist=FALSE)$prompt_usado,"flauta")
})

test_that("ACE-Step polling and recovery never resubmit a completed generation", {
  withr::local_envvar(c(ACESTEP_API_KEY=""))
  wav <- sound_fixture(); on.exit(unlink(wav)); posts <- 0L; polls <- 0L; downloads <- 0L; body_sent <- NULL
  local_mocked_bindings(.gen_sound_http=function(service,base_url,method,route,body=NULL,budget,path=NULL) {
    if (route=="/release_task") { posts <<- posts+1L; body_sent <<- body; return(sound_response(list(task_id="task-one"))) }
    if (route=="/query_result") {
      polls <<- polls+1L; expect_identical(body$task_id_list,list("task-one"))
      return(sound_response(list(list(task_id="task-one",status=1L,result='[{"file":"/v1/audio?path=%2Ftmp%2Fgenerated.wav"}]'))))
    }
    downloads <<- downloads+1L
    if (downloads==1L) genflow:::.gen_stop("sound_transport",stage="download")
    file.copy(wav,path); sound_response(list())
  },.package="genflow")
  failed <- gen_music("piano",service="acestep",seed=4,persist=FALSE)
  expect_identical(failed$status_api,"ERROR"); expect_identical(failed$inference_status,"SUCCESS")
  expect_identical(failed$error$outcome,"succeeded")
  expect_true(is.list(failed$audio_receipt)); expect_equal(posts,1)
  expect_identical(body_sent$batch_size,1L); expect_identical(body_sent$lyrics,"[Instrumental]")
  for (flag in c("thinking","sample_mode","use_format","use_cot_caption","use_cot_language","use_random_seed")) expect_false(body_sent[[flag]])
  recovered <- gen_audio_recover(failed,persist=FALSE)
  expect_identical(recovered$status_api,"SUCCESS"); expect_equal(posts,1); expect_equal(polls,1); expect_equal(downloads,2)
  expect_null(recovered$audio_receipt); expect_true(file.exists(recovered$response_value))
  expect_identical(recovered$duration,failed$duration)
})

test_that("pending audio receipts resume queries and do not create a second job", {
  wav <- sound_fixture(); on.exit(unlink(wav)); submissions <- 0L; queries <- 0L
  local_mocked_bindings(.gen_sound_http=function(service,base_url,method,route,body=NULL,budget,path=NULL) {
    if (route=="/release_task") { submissions <<- submissions+1L; return(sound_response(list(task_id="pending"))) }
    if (route=="/query_result") {
      queries <<- queries+1L
      if (queries==1L) genflow:::.gen_stop("sound_deadline",outcome="unknown")
      return(sound_response(list(list(task_id="pending",status=1L,result='[{"file":"/v1/audio?path=a.wav"}]'))))
    }
    file.copy(wav,path); sound_response(list())
  },.package="genflow")
  result <- gen_music("piano",service="acestep",persist=FALSE)
  expect_identical(result$inference_status,"ERROR"); expect_identical(result$error$outcome,"unknown")
  expect_identical(result$audio_receipt$state,"pending")
  expect_identical(gen_audio_recover(result,persist=FALSE)$status_api,"SUCCESS")
  expect_equal(submissions,1); expect_equal(queries,2)
})

test_that("audio publication failures preserve bytes and inference success", {
  withr::local_envvar(c(ELEVENLABS_API_KEY="fixture"))
  wav <- sound_fixture(); on.exit(unlink(wav)); posts <- 0L
  real_copy <- genflow:::.genflow_copy_artifact; fail <- TRUE
  local_mocked_bindings(.gen_sound_http=function(service,base_url,method,route,body=NULL,budget,path=NULL) {
    posts <<- posts+1L; file.copy(wav,path); sound_response(list())
  },.genflow_copy_artifact=function(...) {
    if (fail) stop(structure(list(message="disk full",call=NULL),class=c("genflow_persistence_error","error","condition")))
    real_copy(...)
  },.package="genflow")
  value <- gen_sfx("asa",persist=FALSE)
  expect_identical(value$inference_status,"SUCCESS"); expect_identical(value$status_api,"ERROR")
  expect_true(file.exists(value$response_value)); expect_identical(value$audio_receipt$source$kind,"base64")
  fail <- FALSE
  expect_identical(gen_audio_recover(value,persist=FALSE)$status_api,"SUCCESS"); expect_equal(posts,1)
  expect_error(gen_audio_recover(list()),class="genflow_error")
})

test_that("download URLs cannot cross hosts or become local files", {
  base <- "http://127.0.0.1:8001"
  expect_identical(genflow:::.gen_sound_download_url("/v1/audio?path=a.wav",base),paste0(base,"/v1/audio?path=a.wav"))
  for (value in c("/etc/passwd","file:///tmp/sound.wav","http://example.org/v1/audio?path=x", "http://127.0.0.1:8002/v1/audio?path=x",
                  "http://secret@127.0.0.1:8001/v1/audio?path=x","http://127.0.0.1:8001/v1/audio?token=key"))
    expect_error(genflow:::.gen_sound_download_url(value,base),class="genflow_error")
})

test_that("sound capabilities, credentials and catalogs agree", {
  expect_true(gen_capabilities("elevenlabs")$modalities[["sfx"]])
  expect_true(gen_capabilities("elevenlabs")$modalities[["music"]])
  expect_false(gen_capabilities("acestep")$modalities[["sfx"]])
  expect_true(gen_capabilities("acestep")$modalities[["music"]])
  directory <- tempfile(); dir.create(directory); on.exit(unlink(directory,recursive=TRUE))
  rows <- genflow:::.update_models_elevenlabs(directory,FALSE)
  expect_setequal(rows$type,c("Music","SFX"))
  expect_true(file.exists(file.path(directory,"elevenlabs.csv")))
  local_mocked_bindings(.gen_sound_http=function(...) sound_response(list(models=list(list(name="acestep-v15-turbo",is_default=TRUE)))),.package="genflow")
  rows <- genflow:::.update_models_acestep(directory,FALSE)
  expect_identical(rows$type,"Music"); expect_identical(rows$model,"acestep-v15-turbo")
  env <- genflow:::.genflow_credential_specs(c("elevenlabs","acestep"))$env
  expect_true(all(c("ELEVENLABS_API_KEY","ACESTEP_API_KEY","ACESTEP_BASE_URL") %in% env))
})

test_that("failed and malformed asynchronous responses never masquerade as valid audio", {
  status <- 2L; count <- 0L
  local_mocked_bindings(.gen_sound_http=function(service,base_url,method,route,body=NULL,budget,path=NULL) {
    if (route=="/release_task") { count <<- count+1L; return(sound_response(list(task_id="task"))) }
    sound_response(list(list(task_id="task",status=status,result='[{"file":"/v1/audio?path=a"},{"file":"/v1/audio?path=b"}]')))
  },.package="genflow")
  failed <- gen_music("piano",service="acestep",persist=FALSE)
  expect_identical(failed$error$code,"sound_task_failed"); expect_identical(failed$error$outcome,"failed")
  expect_null(failed$audio_receipt)
  status <- 1L
  malformed <- gen_music("piano",service="acestep",persist=FALSE)
  expect_identical(malformed$inference_status,"SUCCESS")
  expect_identical(malformed$error$code,"sound_response")
  expect_identical(gen_audio_recover(malformed,persist=FALSE)$status_api,"ERROR")
  expect_equal(count,2L)
  for (bad in list(NULL,1,"audio",list(),list(audio_receipt=list(service=c("a","b")))))
    expect_error(gen_audio_recover(bad,persist=FALSE),class="genflow_error")
  malformed$audio_receipt$poll_interval <- -1
  expect_error(gen_audio_recover(malformed,persist=FALSE),class="genflow_error")
})


test_that("rejected provider locators are not retained in an audio receipt", {
  local_mocked_bindings(.gen_sound_http=function(service,base_url,method,route,body=NULL,budget,path=NULL) {
    if (route=="/release_task") return(sound_response(list(task_id="task")))
    sound_response(list(list(task_id="task",status=1L,result='[{"file":"http://secret@elsewhere.invalid/v1/audio?path=x"}]')))
  },.package="genflow")
  result <- gen_music("piano",service="acestep",persist=FALSE)
  expect_identical(result$inference_status,"SUCCESS")
  expect_identical(result$error$code,"sound_download_url")
  expect_false(grepl("secret",jsonlite::toJSON(result,auto_unbox=TRUE,null="null")))
  expect_identical(result$audio_receipt$task_id,"task")
})
