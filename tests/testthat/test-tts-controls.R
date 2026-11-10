test_that("native steps override inherited engine settings without mutating the parent", {
  f <- native_tts_fixture()
  withr::local_envvar(CRISPASR_VOXCPM2_INFERENCE_STEPS="37")
  for (steps in list(NULL,20L,7L)) {
    result <- gen_tts("Olá. Vamos brincar?",instructions="A friendly voice",steps=steps,
      service="local-native",format="wav",persist=FALSE)
    expect_identical(result$status_api,"SUCCESS");unlink(result$response_value)
    record <- jsonlite::read_json(f$record)
    expected <- as.character(steps %||% 10L)
    expect_identical(record$steps,expected)
    args <- unlist(record$args)
    expect_identical(args[match("--tts-steps",args)+1L],expected)
    expect_identical(args[match("--tts",args)+1L],"(A friendly voice) Olá. Vamos brincar?")
    expect_identical(Sys.getenv("CRISPASR_VOXCPM2_INFERENCE_STEPS"),"37")
    expect_identical(result$native$text_segments,1L)
  }
})

test_that("CFG is declared by the adapter and works without a registered profile", {
  f <- native_tts_fixture()
  withr::local_options(list(genflow.models_dir=file.path(f$root,"empty"),genflow.runtime_snapshot=NULL))
  form <- gen_block_configuration("gen_tts","local-native",f$model,4)
  options <- form$arguments$properties$model_options
  expect_false(isTRUE(options[["x-ui"]]$disabled))
  expect_equal(options$properties$cfg$default,2)
  expect_identical(options$properties$cfg$title,"CFG")
  for (value in list(structure(list(),names=character()),list(cfg=1.5))) {
    result <- gen_tts("Olá, amigos!",service="local-native",format="wav",model_options=value,persist=FALSE)
    expect_identical(result$status_api,"SUCCESS");unlink(result$response_value)
    expect_equal(result$native$cfg,value$cfg %||% 2)
    expect_identical(jsonlite::read_json(f$record)$cfg,as.character(value$cfg %||% 2))
  }
  for(value in list(list(cfg=-1),list(cfg="bad"),list(unknown=1)))
    expect_false(gen_validate_call("gen_tts",list(text="Olá",service="local-native",model_options=value),4)$valid)
})

test_that("native controls and complete instructions agree in direct and agent calls", {
  f <- native_tts_fixture()
  withr::local_options(list(genflow.models_dir=file.path(f$root,"empty"),genflow.runtime_snapshot=NULL))
  setup <- set_setup("native-controls",service="local-native",model="auto",type="TTS",
    format="wav",steps=20L,model_options=list(cfg=1.5),instructions="Friendly. Happy voice",
    save=FALSE,assign=FALSE)
  agent <- set_agent("native-controls",setup=setup,content=list(text="Olá. Vamos brincar?"),save=FALSE,assign=FALSE)
  calls <- list(
    function() gen_tts("Olá. Vamos brincar?",service="local-native",format="wav",steps=20L,
      model_options=list(cfg=1.5),instructions="Friendly. Happy voice",persist=FALSE),
    function() gen_tts(agent,persist=FALSE))
  for (call in calls) {
    result <- call()
    expect_identical(result$status_api,"SUCCESS");unlink(result$response_value)
    record <- jsonlite::read_json(f$record)
    expect_identical(record$steps,"20");expect_identical(record$cfg,"1.5")
    args <- unlist(record$args)
    expect_identical(args[match("--tts",args)+1L],"(Friendly. Happy voice) Olá. Vamos brincar?")
  }
})
