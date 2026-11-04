test_that("native TTS contracts are versioned and reject unsupported controls offline", {
  expect_identical(gen_block_contracts("gen_tts",1)$inputs,list(text="asset:text|asset:json"))
  expect_false(gen_validate_call("gen_tts",list(text="Olá",service="local-native"))$valid)
  good <- gen_validate_call("gen_tts",list(text="Olá",service="local-native"),2)
  expect_true(good$valid); expect_identical(good$args$model,"auto")
  expect_identical(gen_block_contracts("gen_tts",2)$inputs$reference_audio,"asset:audio")
  for (args in list(list(voice="mimi"),list(instructions=42),list(speed=2),list(format="pcm"),
      list(format="flac"),list(cfg_scale=2),list(steps=0),list(threads=0),list(seed=-1),list(native_device="hip"),
      list(reference_audio=42),list(reference_audio="missing.wav"),list(spoken_disclaimer=FALSE))) {
    report <- gen_validate_call("gen_tts",c(list(text="Olá",service="local-native"),args),2)
    expect_false(report$valid,info=names(args))
  }
  for (service in c("openai","replicate")) {
    expect_true(gen_validate_call("gen_tts",list(text="Olá",service=service),2)$valid)
    expect_false(gen_validate_call("gen_tts",list(text="Olá",service=service,seed=1),2)$valid)
    expect_false(gen_validate_call("gen_tts",list(text="Olá",service=service),2,bound="reference_audio")$valid)
  }
  report <- gen_validate_call("gen_tts",list(text="Olá",service="local-native",voice="mimi",speed=2,steps=0,cfg=4),2)
  expect_gte(length(report$errors),4L)
  expect_true(gen_validate_call("gen_tts",list(text="Olá",service="local-native",reference_consent=TRUE),2,bound="reference_audio")$valid)
  expect_identical(gen_tts_voices("local-native"),character())
})

test_that("GGUF metadata determines modality, independent of filename", {
  model <- native_tts_model(); withr::defer(unlink(model))
  expect_identical(genflow:::.gen_native_architecture(model),"voxcpm2")
  expect_identical(genflow:::.gen_native_model_type(model),"TTS")
  native_tts_model("llama",model)
  expect_identical(genflow:::.gen_native_model_type(model),"Unknown")
  native_tts_model("granite_speech",model)
  expect_identical(genflow:::.gen_native_model_type(model),"STT")
  writeBin(charToRaw("GGUF"),model)
  expect_identical(genflow:::.gen_native_architecture(model),"")
  expect_identical(genflow:::.gen_native_model_type("voxcpm2-q8_0.gguf"),"TTS")
  expect_identical(genflow:::.gen_native_model_type("unclassified.gguf"),"Unknown")
})

test_that("version 5 configuration keeps STT settings and gains independent TTS settings", {
  f <- tempfile(); withr::defer(unlink(f))
  old <- list(version=5L,stt_native_engine="crispasr",stt_native_crispasr_executable="/old/crispasr",stt_native_model="whisper.bin",stt_native_device="vulkan")
  jsonlite::write_json(old,f,auto_unbox=TRUE)
  before <- readBin(f,"raw",file.info(f)$size)
  current <- gen_local_config(path=f)
  expect_identical(current$version,6L); expect_identical(current$tts_native_model,"")
  expect_identical(current$tts_native_device,"auto")
  expect_identical(readBin(f,"raw",file.info(f)$size),before)
  next_config <- gen_local_config(tts_native_model="voice.gguf",tts_native_executable="/new/crispasr",path=f)
  expect_equal(next_config[names(old)[-1]],old[-1])
  expect_identical(gen_local_config(path=f)$tts_native_executable,"/new/crispasr")
})

test_that("native TTS preserves public results, literal arguments and explicit seed", {
  f <- native_tts_fixture()
  withr::local_envvar(c(CRISPASR_VOXCPM2_CFG_VALUE="99",CRISPASR_VOXCPM2_MAX_LEN="1"))
  text <- 'Olá "Mimi"; $(touch impossible)!\nVamos brincar?'
  result <- gen_tts(text,service="local-native",format="wav",seed=2,steps=10,threads=2,persist=FALSE)
  expect_identical(result$status_api,"SUCCESS"); expect_identical(result$inference_status,"SUCCESS")
  expect_true(file.exists(result$response_value)); withr::defer(unlink(result$response_value))
  expect_true(is.na(result$saved_file)); expect_identical(result$format,"wav")
  expect_identical(result$native$model,normalizePath(f$model)); expect_false(result$native$reference_used)
  expect_equal(result$native$audio$sample_rate,48000)
  record <- jsonlite::read_json(f$record)
  args <- unlist(record$args)
  expect_identical(args[match("--tts",args)+1L],text)
  expect_identical(args[match("--seed",args)+1L],"2")
  expect_identical(record$cfg,"2"); expect_null(record$max_len)
  expect_false("--tts-cfg-scale" %in% args)
  expect_true("--no-gpu" %in% args)
  expect_false(file.exists(args[match("--tts-output",args)+1L]))
  agent <- structure(list(service="local-native",model=f$model,text="Outra fala",format="wav"),class="genflow_agent")
  via_agent <- gen_tts(agent,persist=FALSE)
  expect_identical(via_agent$status_api,"SUCCESS"); unlink(via_agent$response_value)
})

test_that("reference audio is decoded before inference and its use must be confirmed", {
  f <- native_tts_fixture()
  reference <- gen_tts("Referência",service="local-native",format="wav",persist=FALSE)$response_value
  withr::defer(unlink(reference))
  result <- gen_tts("Ela disse falling back to zero-shot.",service="local-native",format="wav",instructions="Calma e alegre",reference_audio=reference,reference_consent=TRUE,
    reference_identity="synthetic",spoken_disclaimer=FALSE,marking_responsibility=TRUE,persist=FALSE)
  expect_identical(result$status_api,"SUCCESS"); expect_true(result$native$reference_used); unlink(result$response_value)
  args <- unlist(jsonlite::read_json(f$record)$args)
  expect_identical(args[match("--tts",args)+1L],"(Calma e alegre) Ela disse falling back to zero-shot.")
  expect_true(all(c("--voice","--i-have-rights","--speaker-identity","--no-spoken-disclaimer","--accept-marking-responsibility") %in% args))
  expect_false(file.exists(args[match("--voice",args)+1L]))
  for (mode in c("fallback","unconfirmed")) {
    withr::with_envvar(c(GENFLOW_TEST_TTS_MODE=mode), {
      bad <- gen_tts("Olá",service="local-native",format="wav",reference_audio=reference,reference_consent=TRUE,persist=FALSE)
      expect_identical(bad$status_api,"ERROR"); expect_identical(bad$error$code,"tts_reference_ignored")
      expect_null(bad$response_value)
    })
  }
  unlink(f$record)
  for (ref in c(file.path(f$root,"missing.wav"),f$model,f$root)) {
    bad <- gen_tts("Olá",service="local-native",format="wav",reference_audio=ref,reference_consent=TRUE,persist=FALSE)
    expect_identical(bad$error$code,"tts_reference_invalid"); expect_false(file.exists(f$record))
  }
})

test_that("native failures have canonical diagnostics and kill timed out children", {
  f <- native_tts_fixture()
  cases <- c(unsupported="tts_engine_unsupported",malformed="tts_engine_unsupported",fail="tts_native_failed",missing="tts_native_audio_invalid",invalid="tts_native_audio_invalid",timeout="tts_native_timeout")
  for (mode in names(cases)) withr::with_envvar(c(GENFLOW_TEST_TTS_MODE=mode), {
    bad <- gen_tts("Olá",service="local-native",format="wav",timeout_api=if (mode=="timeout") 1 else 10,persist=FALSE)
    expect_identical(bad$status_api,"ERROR"); expect_identical(bad$error$code,unname(cases[[mode]]))
    expect_null(bad$response_value)
  })
  pid <- as.integer(readLines(paste0(f$record,".pid"),warn=FALSE))
  expect_false(isTRUE(tools::pskill(pid,signal=0L)))
  bad <- gen_tts("Olá",service="local-native",model=f$root,format="wav",persist=FALSE)
  expect_identical(bad$error$code,"tts_model_invalid")
  bad <- gen_tts("Olá",service="local-native",executable="/does/not/exist/crispasr",format="wav",persist=FALSE)
  expect_identical(bad$error$code,"tts_engine_missing")
})

test_that("native TTS diagnostics inspect configuration without generation", {
  f <- native_tts_fixture()
  rows <- genflow:::.genflow_native_tts_diagnostics(gen_local_config(),10)
  expect_identical(rows$status,"ok")
  expect_match(rows$detail,"fixture")
  expect_false(file.exists(f$record))
  config <- gen_local_config(); config$tts_native_model <- "missing.gguf"
  expect_identical(genflow:::.genflow_native_tts_diagnostics(config,10)$status,"error")
})

test_that("native remuxed audio retains its real container and codec", {
  f <- native_tts_fixture()
  for (format in c("opus","aac","mp3")) {
    result <- gen_tts("Olá",service="local-native",format=format,persist=FALSE)
    expect_identical(result$status_api,"SUCCESS")
    expect_identical(result$requested_format,format)
    expect_identical(result$format,if (format=="mp3") "mp3" else "m4a")
    expect_identical(result$native$audio$codec,format)
    expect_true(file.exists(result$response_value)); unlink(result$response_value)
    args <- unlist(jsonlite::read_json(f$record)$args)
    original <- args[match("--tts-output",args)+1L]
    expect_false(any(file.exists(c(original,paste0(tools::file_path_sans_ext(original),c(".mp4",".m4a"))))))
  }
})

test_that("catalog migration never promotes auxiliary audio models to STT", {
  root <- tempfile(); dir.create(root); withr::defer(unlink(root,recursive=TRUE))
  withr::local_envvar(c(CRISPASR_CACHE_DIR=file.path(root,"cache")))
  rows <- data.frame(service="local-native",model=c("voxcpm2-q8_0.gguf","ggml-large-v3.bin","ggml-silero-v6.2.0.bin","fireredpunc-q4_k.gguf","unclassified.gguf","ark-asr-3b.gguf","funasr-nano.gguf"),type="Audio",pricing="",description="")
  path <- file.path(root,"local-native.csv"); write.csv(rows,path,row.names=FALSE)
  before <- readBin(path,"raw",file.info(path)$size)
  catalog <- gen_models("local-native",root)
  expect_identical(catalog$type,c("TTS","STT","Unknown","Unknown","Unknown","STT","STT"))
  expect_true(all(catalog$source_file=="local-native.csv"))
  expect_identical(readBin(path,"raw",file.info(path)$size),before)
})

test_that("the configured TTS model is protected from removal", {
  root <- tempfile(); dir.create(root); withr::defer(unlink(root,recursive=TRUE))
  cache <- file.path(root,"cache"); dir.create(cache)
  withr::local_envvar(c(CRISPASR_CACHE_DIR=cache))
  withr::local_options(list(genflow.local_config_path=file.path(root,"config.json"),genflow.objects_dir=file.path(root,"objects")))
  path <- native_tts_model(path=file.path(cache,"voxcpm2.gguf"))
  gen_local_config(tts_native_model="voxcpm2.gguf")
  inventory <- gen_native_models(cache_dirs=cache)
  expect_identical(inventory$modality,"tts"); expect_true(inventory$selected)
  expect_error(gen_native_model_remove(path,config=list(tts_native_model="")),class="genflow_error")
  expect_true(file.exists(path))
})

test_that("native TTS preserves successful inference on publication failure", {
  f <- native_tts_fixture()
  testthat::local_mocked_bindings(.genflow_copy_artifact=function(...) stop("disk fixture"),.package="genflow")
  result <- gen_tts("Olá",service="local-native",format="wav",persist=TRUE)
  expect_identical(result$status_api,"ERROR"); expect_identical(result$inference_status,"SUCCESS")
  expect_identical(result$error$stage,"persistence")
  expect_true(file.exists(result$response_value)); unlink(result$response_value)
  expect_true(gen_validate_call("gen_tts",list(text="Olá",service="local-native",reference_identity="synthetic"),2)$valid)
  too_long <- gen_validate_call("gen_tts",list(text=paste(rep("a",4090),collapse=""),add="more words",service="local-native"),2)
  expect_false(too_long$valid)
})
