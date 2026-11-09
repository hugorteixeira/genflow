# All transport is mocked. These regressions exercise the existing adapters,
# not provider accounts, and validate the public diagnostic v1 contract.
pd_response <- function(status=429L, code="credit_balance_exhausted") {
  body <- list(error=list(code=code,type="insufficient_quota",message="private-response fixture-key"))
  structure(list(status_code=as.integer(status),headers=list(`content-type`="application/json"),
    content=charToRaw(jsonlite::toJSON(body,auto_unbox=TRUE)),url="http://fixture.invalid"),class="response")
}
pd_setup <- function(env=parent.frame()) {
  keys <- c("OPENAI_API_KEY","OPENROUTER_API_KEY","ANTHROPIC_API_KEY","GOOGLE_API_KEY","GEMINI_API_KEY",
    "GROQ_API_KEY","CEREBRAS_API_KEY","TOGETHER_API_KEY","SAMBANOVA_API_KEY","NEBIUS_API_KEY","DEEPSEEK_API_KEY",
    "PERPLEXITY_API_KEY","FIREWORKS_API_KEY","DEEPINFRA_API_KEY","HYPERBOLIC_API_KEY","HUGGINGFACE_API_TOKEN",
    "FAL_API_KEY","REPLICATE_API_TOKEN","ASSEMBLYAI_API_KEY","CLOUDFLARE_API_TOKEN","CLOUDFLARE_ACCOUNT_ID","VOICEGAIN_API_KEY","ELEVENLABS_API_KEY")
  withr::local_envvar(c(setNames(rep("fixture-key",length(keys)),keys),genflow_SKIP_PERSIST_LOG="1"),.local_envir=env)
  root <- tempfile("provider-diagnostics-");dir.create(root)
  withr::defer(unlink(root,recursive=TRUE),envir=env)
  withr::local_options(list(genflow.output_dir=root,genflow.log_dir=root,genflow.models_dir=file.path(root,"models"),
    genflow.providers_path=file.path(root,"providers.json"),genflow.local_config_path=file.path(root,"local.json")),.local_envir=env)
  root
}
pd_assert <- function(d,code,outcome="failed",retryable=FALSE) {
  expect_identical(d$code,code);expect_identical(d$outcome,outcome);expect_identical(d$retryable,retryable)
  expect_identical(d$source,"genflow")
  expect_true(gen_validate_schema(d,gen_error_catalog()$schema)$valid)
  expect_false(grepl("private-response|fixture-key",jsonlite::toJSON(d,auto_unbox=TRUE)))
}
pd_capture <- function(expr) suppressWarnings(suppressMessages(tryCatch(force(expr),error=identity)))

test_that("HTTP status, structured codes and execution stage determine the diagnostic", {
  classify <- getFromNamespace(".gen_provider_http_error","genflow")
  cases <- list(
    list(401L,NULL,"provider_unauthorized",FALSE),list(403L,NULL,"provider_forbidden",FALSE),
    list(402L,NULL,"provider_payment_required",FALSE),list(404L,NULL,"provider_not_found",FALSE),
    list(422L,NULL,"provider_request_rejected",FALSE),
    list(429L,list(error=list(code="credit_balance_exhausted",type="insufficient_quota")),"provider_credit_exhausted",FALSE),
    list(429L,list(error=list(type="insufficient_quota")),"provider_quota_exhausted",FALSE),
    list(429L,list(detail=list(status="quota_exceeded")),"provider_quota_exhausted",FALSE),
    list(429L,list(error=list(code="rate_limit_exceeded")),"provider_rate_limit",TRUE))
  for (c in cases) pd_assert(classify(c[[1]],c[[2]]),c[[3]],retryable=c[[4]])
  for (status in c(408L,500L,502L,503L,504L)) {
    d <- classify(status);expect_identical(d$outcome,"unknown");expect_false(d$retryable)
  }
  for (body in list(NULL,"<html>private-response</html>",list(error=list(message="credit_balance_exhausted")),
                    list(error=list(code=list("invalid"))),list(error="fixture-key"))) {
    pd_assert(classify(401L,body),"provider_unauthorized")
  }
  expect_identical(classify(429L,list(error=list(message="credit_balance_exhausted")))$code,"provider_rate_limit")
  pd_assert(classify(401L,stage="poll"),"provider_unauthorized","unknown")
  pd_assert(classify(503L,stage="management"),"provider_unavailable","failed",TRUE)
  pd_assert(classify(503L,stage="download"),"provider_unavailable","succeeded",TRUE)
})

for (service in c("openai","openrouter","anthropic","gemini","groq","cerebras","together","sambanova",
                  "nebius","deepseek","perplexity","fireworks","deepinfra","hyperbolic","hf","ollama","llamacpp")) local({
  id <- service
  test_that(paste("text adapter preserves exhausted-credit code:",id), {
    pd_setup(); calls <- 0L
    request <- function(...) {calls <<- calls+1L;pd_response()}
    local_mocked_bindings(GET=request,POST=request,RETRY=request,VERB=function(...) stop("Unexpected transport in offline test"),.package="httr")
    result <- pd_capture(gen_txt("fixture",service=id,model="fixture-model",persist=FALSE,null_repeat=FALSE))
    expect_type(result,"list");pd_assert(result$error,"provider_credit_exhausted")
    expect_identical(calls,1L)
  })
})
for (service in c("openai","fal","replicate","hf")) local({
  id <- service
  test_that(paste("image adapter preserves exhausted-credit code:",id), {
    root <- pd_setup();calls <- 0L
    request <- function(...) {calls <<- calls+1L;pd_response()}
    local_mocked_bindings(GET=request,POST=request,RETRY=request,VERB=function(...) stop("Unexpected transport in offline test"),.package="httr")
    model <- switch(id,openai="gpt-image-2",fal="fal-ai/flux/schnell",replicate="black-forest-labs/flux-schnell",hf="black-forest-labs/FLUX.1-schnell")
    result <- pd_capture(gen_img("fixture",service=id,model=model,persist=FALSE,directory=root))
    pd_assert(result$error,"provider_credit_exhausted");expect_identical(calls,1L)
    expect_length(list.files(root,pattern="[.](png|jpg|webp)$",recursive=TRUE),0L)
  })
})
for (service in c("openai","groq","assemblyai","cloudflare","voicegain","hf")) local({
  id <- service
  test_that(paste("transcription adapter preserves authentication failure:",id), {
    root<-pd_setup();path<-file.path(root,"audio.wav");writeBin(as.raw(0:15),path)
    request<-function(...)pd_response(401L,"invalid_api_key")
    local_mocked_bindings(GET=request,POST=request,RETRY=request,VERB=function(...) stop("Unexpected transport in offline test"),.package="httr")
    fn<-getFromNamespace(paste0(".stt_",id),"genflow")
    args<-list(audio_path=path,audio_url="https://fixture.invalid/audio.wav",model=NULL,language=NULL,prompt=NULL,
      timeout_secs=5,poll_interval=0,max_poll_seconds=5)
    e<-pd_capture(do.call(fn,args[intersect(names(args),names(formals(fn)))]))
    expect_s3_class(e,"genflow_error")
    d<-gen_diagnostic(e);expect_identical(d$code,"provider_unauthorized");expect_false(d$retryable)
    expect_true(d$outcome %in% c("failed","not_started"))
  })
})
test_that("voice response helper distinguishes rejected submission from failed polling", {
  pd_setup()
  fn<-getFromNamespace(".tts_expect_http_status","genflow")
  e<-pd_capture(fn(pd_response(),200L,"OpenAI TTS request"))
  pd_assert(gen_diagnostic(e),"provider_credit_exhausted")
  e<-pd_capture(fn(pd_response(401L,"invalid_api_key"),200L,"Replicate prediction polling",stage="poll"))
  pd_assert(gen_diagnostic(e),"provider_unauthorized","unknown")
})
test_that("sound adapters retain structured account failures without resubmitting", {
  pd_setup();calls<-0L
  local_mocked_bindings(POST=function(...) {calls<<-calls+1L;pd_response()},.package="httr")
  result<-pd_capture(gen_music("fixture",service="elevenlabs",persist=FALSE))
  pd_assert(result$error,"provider_credit_exhausted");expect_identical(calls,1L)
})
for (service in c("openai","openrouter","anthropic","gemini","groq","fal","replicate","hf","ollama","llamacpp")) local({
  id<-service
  test_that(paste("catalog adapter preserves authentication failure and its prior file:",id), {
    root<-pd_setup();path<-file.path(root,paste0(id,".csv"));writeLines("previous-catalog",path)
    request<-function(...)pd_response(401L,"invalid_api_key")
    local_mocked_bindings(GET=request,POST=request,RETRY=request,VERB=function(...) stop("Unexpected transport in offline test"),.package="httr")
    e<-pd_capture(gen_update_models(id,directory=root,verbose=FALSE,fail_on_error=TRUE))
    expect_s3_class(e,"genflow_error");pd_assert(gen_diagnostic(e),"provider_unauthorized")
    expect_identical(readLines(path),"previous-catalog")
  })
})
test_that("catalog batches retain each structured cause", {
  root<-pd_setup()
  request<-function(...)pd_response(401L,"invalid_api_key")
  local_mocked_bindings(GET=request,.package="httr")
  e<-pd_capture(gen_update_models(c("openai","openrouter"),directory=root,verbose=FALSE,fail_on_error=TRUE))
  expect_s3_class(e,"genflow_error");d<-gen_diagnostic(e)
  expect_identical(d$code,"catalog_update_failed");expect_length(d$causes,2L)
  expect_true(all(vapply(d$causes,function(c)c$code=="provider_unauthorized",logical(1))))
})

test_that("malformed responses do not turn uncertainty into a rejected generation", {
  check <- getFromNamespace(".gen_provider_check","genflow")
  for (response in list(NULL,"not a response",list(status="200"),list(status=NA_integer_))) {
    e <- pd_capture(check(response));pd_assert(gen_diagnostic(e),"provider_response_invalid","unknown")
  }
})
for (service in c("openai","fal","replicate","hf")) local({
  id<-service
  test_that(paste("image HTTP errors survive invalid JSON and content type:",id), {
    root<-pd_setup();calls<-0L
    request<-function(...) {calls<<-calls+1L;r<-pd_response(401L);r$content<-charToRaw("{private-response");r}
    local_mocked_bindings(GET=request,POST=request,.package="httr")
    result<-pd_capture(gen_img("fixture",service=id,model=if(id=="openai") "gpt-image-2" else "owner/model",persist=FALSE,directory=root))
    pd_assert(result$error,"provider_unauthorized");expect_identical(calls,1L)
  })
})
test_that("a malformed successful text response cannot trigger a second inference", {
  pd_setup();calls<-0L
  local_mocked_bindings(POST=function(...) {calls<<-calls+1L;r<-pd_response(200L);r$content<-charToRaw("{private-response");r},.package="httr")
  result<-pd_capture(gen_txt("fixture",service="openai",model="gpt-5-mini",persist=FALSE,null_repeat=TRUE,retry_delays=0))
  pd_assert(result$error,"provider_response_invalid","unknown");expect_identical(calls,1L)
})
test_that("Replicate STT rejects credit exhaustion before polling", {
  root<-pd_setup();path<-file.path(root,"audio.wav");writeBin(as.raw(0:15),path);calls<-0L
  local_mocked_bindings(.stt_replicate_model_info=function(...) list(version="fixture",input_field="audio"),.package="genflow")
  local_mocked_bindings(POST=function(...) {calls<<-calls+1L;pd_response()},.package="httr")
  e<-pd_capture(genflow:::.stt_replicate(path,"owner/model",5,0,5))
  pd_assert(gen_diagnostic(e),"provider_credit_exhausted");expect_identical(calls,1L)
})
test_that("local compatible STT shares the same HTTP classification", {
  root<-pd_setup();path<-file.path(root,"audio.wav");writeBin(as.raw(0:15),path)
  e<-pd_capture(genflow:::.stt_local_openai(path,"fixture",NULL,NULL,5,
    request=function(...) list(status=429L,text='{"error":{"code":"credit_balance_exhausted"}}')))
  pd_assert(gen_diagnostic(e),"provider_credit_exhausted")
})
test_that("Replicate TTS records a terminal failure without exposing the provider message", {
  pd_setup();r<-pd_response(201L);r$content<-charToRaw('{"id":"fixture","urls":{"get":"https://fixture.invalid/task"},"status":"failed","error":"private-response"}')
  local_mocked_bindings(POST=function(...)r,.package="httr")
  e<-pd_capture(genflow:::.tts_replicate(text="Fixture",model="owner/model",voice=NULL,format="mp3",speed=NULL,instructions=NULL,timeout_secs=5,poll_interval=0,max_poll_seconds=5,
    model_info=list(version="fixture",properties=list(text=list(type="string")))))
  pd_assert(gen_diagnostic(e),"provider_task_failed")
})

test_that("route selection accepts typed success and only falls back on route absence", {
  route<-genflow:::.genflow_try_next_route
  expect_identical(route(genflow:::.genflow_provider_success("OK")),FALSE)
  expect_identical(route(genflow:::.genflow_provider_failure("HTTR_ERROR: uncertain")),FALSE)
  expect_identical(route(genflow:::.genflow_provider_failure(genflow:::.gen_provider_http_error(404L))),TRUE)
})
test_that("successful JSON does not inherit the diagnostic body size limit", {
  r<-pd_response(200L);value<-strrep("a",1048577L)
  r$content<-charToRaw(jsonlite::toJSON(list(text=value),auto_unbox=TRUE))
  expect_identical(genflow:::.gen_provider_json(r)$text,value)
})
test_that("the common image downloader preserves HTTP facts for receipt recovery", {
  pd_setup();local_mocked_bindings(GET=function(...)pd_response(401L),.package="httr")
  e<-pd_capture(genflow:::.gen_image_download("https://fixture.invalid/image",5))
  pd_assert(gen_diagnostic(e),"provider_unauthorized","succeeded")
})

test_that("TTS without a recovery receipt cannot retry a completed generation", {
  pd_setup();calls<-0L
  local_mocked_bindings(.tts_replicate_model_info=function(...) list(version="fixture",properties=list()),
    .tts_replicate=function(...) {calls<<-calls+1L;genflow:::.gen_stop(genflow:::.gen_provider_http_error(503L,stage="download"))},.package="genflow")
  result<-pd_capture(gen_tts("fixture",service="replicate",model="owner/model",persist=FALSE,preview=FALSE))
  pd_assert(result$error,"provider_unavailable","succeeded",FALSE)
  expect_identical(calls,1L);expect_null(result$response_value)
})
