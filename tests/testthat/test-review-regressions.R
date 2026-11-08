test_that("JSON arrays cannot masquerade as empty objects at any workflow boundary", {
  empty <- structure(list(), names=character())
  base <- list(schema_version=1L,nodes=list(list(id="g",type="content",config=list(content_type="text",source="ai",
    ai=list(service="openai",model="gpt-5-mini",parameters=empty,content=list(list(text="Olá")))),inputs=empty)))
  check <- function(d) gen_validate_structure(as.character(jsonlite::toJSON(d,auto_unbox=TRUE,null="null")),purpose="authoring",check_capabilities=FALSE)
  expect_true(check(base)$valid)
  for (field in c("config","inputs","ai","parameters")) {
    d <- base
    if (field %in% c("config","inputs")) d$nodes[[1]][field] <- list(list())
    if (field=="ai") d$nodes[[1]]$config$ai <- list()
    if (field=="parameters") d$nodes[[1]]$config$ai$parameters <- list()
    report <- check(d)
    expect_false(report$valid,info=field)
    expect_true(length(report$errors)>0,info=field)
  }
  base$nodes[[1]]$config$ai$parameters <- list()
  expect_true(gen_validate_structure(base,purpose="authoring",check_capabilities=FALSE)$valid)
  prepared <- gen_validate_structure(base,purpose="authoring",check_capabilities=FALSE)$document
  expect_true(check(prepared)$valid)
})

test_that("Replicate overrides are rejected before a foreign adapter can run", {
  for (service in c("openai","fal")) {
    args <- list(prompt="Jardim",service=service,replicate_input=list(style="ignored"))
    checked <- gen_validate_call("gen_img",args,check_capabilities=FALSE)
    expect_false(checked$valid,info=service)
    expect_true(any(vapply(checked$errors,function(e)e$path=="/args/replicate_input",logical(1))))
  }
  expect_true(gen_validate_call("gen_img",list(prompt="Jardim",service="replicate",replicate_input=list(style="watercolor")),check_capabilities=FALSE)$valid)
  expect_true(gen_validate_call("gen_img",list(prompt="Jardim",service="openai",replicate_input=NULL),check_capabilities=FALSE)$valid)
  called <- FALSE
  local_mocked_bindings(.gen_image_tools=function()invisible(NULL),
    .gen_img_result=function(run,result,persist){run();list(status_api="SUCCESS")},
    .gen_img_openai=function(...){called <<- TRUE;"fixture-only"},.package="genflow")
  expect_error(gen_img(prompt="Jardim",service="openai",replicate_input=list(style="ignored"),persist=FALSE,directory=tempdir()),class="genflow_validation_error")
  expect_false(called)
  expect_true(isTRUE(gen_block_contracts("gen_img")$arguments$properties$replicate_input[["x-ui"]]$hidden))
})

test_that("piece lyrics have one binding while direct functions and saved agents accept literal lyrics", {
  ai <- list(service="elevenlabs",model="music_v2_5",parameters=list(instrumental=FALSE,lyrics="Minha letra"),content=list(list(text="Canção alegre")))
  args <- list(content_type="music",source="ai",ai=ai)
  expect_false(gen_validate_call("content",args,check_capabilities=FALSE)$valid)
  args$ai$parameters$lyrics <- NULL
  args$additions <- list(list(role="lyrics",value=list(text="Minha letra")))
  expect_true(gen_validate_call("content",args,check_capabilities=FALSE)$valid)
  agent <- ai[c("service","model","parameters")];agent$operation <- "audio.music"
  expect_true(gen_validate_agent_definition(agent,check_capabilities=FALSE)$valid)
  expect_true(gen_validate_call("gen_music",c(list(prompt="Canção",service=ai$service),ai$parameters),check_capabilities=FALSE)$valid)
})

test_that("visible contract metadata is Portuguese and separates infrastructure", {
  prop <- function(id,key) gen_block_contracts(id)$arguments$properties[[key]]
  expect_identical(prop("list","kind")$title,"Tipo de conteúdo")
  expect_identical(prop("vote","policy")$title,"Política de votação")
  expect_true(grepl("/cenas/0/texto",prop("json.get","pointer")$description %||% "",fixed=TRUE))
  for (pair in list(c("gen_tts","reference_identity"),c("gen_tts","native_device"),c("gen_stt","native_engine"),c("gen_stt","chunking"))) {
    p <- prop(pair[1],pair[2]); values <- intersect(unlist(p$enum),c("auto","never","unknown","synthetic","real_person"))
    expect_true(all(values %in% names(p[["x-ui"]]$enum_labels)),info=paste(pair,collapse="/"))
    for (v in values) expect_false(identical(p[["x-ui"]]$enum_labels[[v]],v))
  }
  for (key in c("native_engine","native_backend","native_quant","native_kv_quant","timeout_per_audio_minute","max_poll_seconds","chunk_retry_wait_seconds"))
    expect_identical(prop("gen_stt",key)[["x-ui"]]$group,"infrastructure",info=key)
})
