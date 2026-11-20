test_that("catalog decisions use endpoint, operation and connected inputs", {
  d <- withr::local_tempdir()
  rows <- list(
    .gen_openrouter_row(list(id="vendor/shared",architecture=list(input_modalities=list("text","image"),output_modalities=list("text"))),"models"),
    .gen_openrouter_row(list(id="vendor/shared",supported_parameters=list(input_references=list(max=2))),"images/models"),
    .gen_openrouter_row(list(id="vendor/video",supported_frame_images=list("first_frame"),generate_audio=TRUE),"videos/models"))
  .gen_catalog_write(rows,"openrouter",d)
  match <- function(id,op,inputs=character(),complete=TRUE) gen_model_match("openrouter",id,op,inputs,complete,d)$compatible
  expect_true(match("vendor/shared","text.generate","add_img"))
  expect_false(match("vendor/shared","text.generate","pdf"))
  expect_true(match("vendor/shared","image.generate","add_img"))
  expect_false(match("vendor/video","video.generate","audio"))
  expect_false(match("vendor/video","video.generate"))
  expect_true(match("vendor/video","video.generate","image"))
  choices <- gen_model_choices("openrouter","text.generate","pdf",d)
  expect_false(choices$compatible[choices$model=="vendor/shared"])
  expect_equal(sum(choices$model=="vendor/shared"),1)
  seed <- "bytedance/seedance-2.0"
  expect_true(match(seed,"video.generate",c("reference_clip","reference_sound")))
  expect_false(match(seed,"video.generate","reference_sound"))
  expect_false(match(seed,"video.generate",c("image","reference_clip")))
  expect_false(match("black-forest-labs/flux-video-edit","video.generate"))
  expect_true(match("black-forest-labs/flux-video-edit","video.generate","source_video"))
})

test_that("schemas qualify only recognized input and output shapes", {
  input <- list(properties=list(prompt=list(type="string"),video_url=list(type="string")),required=list("prompt","video_url"))
  out <- list(properties=list(video=list(type="object")))
  route <- .gen_schema_media("fal","fal-ai/example",input,out,"video-to-video")
  expect_true(.gen_match_routes(route,"video.generate","source_video",TRUE)$compatible)
  input$required <- c(input$required,"mask")
  expect_false(.gen_match_routes(.gen_schema_media("fal","fal-ai/example",input,out,"video-to-video"),"video.generate","source_video",TRUE)$compatible)
  input <- list(properties=list(prompt=list(type="string")),required=list("prompt"))
  uri <- list(type="string",format="uri")
  qualify <- function(example) .gen_schema_media("replicate","test/model",input,uri,revision="v1",example=example)
  expect_null(qualify(NULL)[[1]]$transport)
  expect_true(.gen_match_routes(qualify("https://replicate.delivery/test.mp4"),"video.generate",character(),TRUE)$compatible)
  expect_true(.gen_match_routes(qualify("https://replicate.delivery/test.png"),"image.generate",character(),TRUE)$compatible)
})

test_that("new video adapters submit once and poll independently", {
  calls <- list()
  local_mocked_bindings(.gen_media_request=function(service,method,path,body=NULL,timeout=60,...) {
    calls[[length(calls)+1L]] <<- list(method=method,path=path,body=body)
    if(service=="gemini") {
      if(method=="POST") return(list(steps=list(list(type="model_output",content=list(list(type="video",uri="files/file-one"))))))
      return(list(state="ACTIVE"))
    }
    if(method=="POST") list(id="gen-one",status="pending") else list(status="completed")
  },.package="genflow")
  for(s in c("openrouter","gemini")) {
    receipt <- gen_video_submit("Move",service=s)
    expect_identical(gen_video_poll(receipt)$state,"ready")
  }
  expect_identical(vapply(calls,`[[`,"","method"),c("POST","GET","POST","GET"))
  expect_identical(calls[[1]]$path,"/api/v1/videos")
  expect_identical(calls[[3]]$body$response_format$delivery,"uri")
})

test_that("music reads complete streams and speech uses its own endpoint", {
  calls <- list()
  local_mocked_bindings(.gen_media_request=function(service,method,path,body,timeout,...) {
    calls[[length(calls)+1L]] <<- list(path=path,body=body)
    if(service=="gemini") return(list(steps=list(list(type="model_output",content=list(list(type="audio",data="YWJj"))))))
    paste('data: {"choices":[{"delta":{"audio":{"data":"YW"}}}]}','data: {"choices":[{"delta":{"audio":{"data":"Jj"}}}]}','data: [DONE]',sep="\n")
  },.package="genflow")
  for(s in c("openrouter","gemini")) expect_identical(.gen_media_music(list(service=s,model="music",prompt="Song",duration_seconds=30,instrumental=TRUE),function() 10),"YWJj")
  expect_identical(calls[[1]]$path,"/api/v1/chat/completions")
  expect_identical(calls[[2]]$path,"/v1beta/interactions")
  expect_false(gen_model_match("openrouter","google/lyria-3-clip-preview","audio.speak")$compatible)
  expect_true(gen_model_match("gemini","gemini-3.8-flash-tts","audio.speak","instructions")$compatible)
})


test_that("fal discovery serializes an expanded OpenAPI revision in the catalog", {
  json_schema <- function(schema) list(content=list("application/json"=list(schema=schema)))
  id <- "fal-ai/test"
  post <- list(requestBody=json_schema(list(properties=list(prompt=list(type="string")))),
    responses=list("200"=json_schema(list(properties=list(video=list(type="object"))))))
  item <- list(endpoint_id=id,metadata=list(category="text-to-video"),openapi=list(paths=setNames(list(list(post=post)),paste0("/",id))))
  routes <- .gen_fal_routes(item)
  row <- .gen_catalog_row("fal",id,"Video","",routes)
  parsed <- jsonlite::fromJSON(row$routes,simplifyVector=FALSE)
  expect_match(parsed[[1]]$revision,"^[a-f0-9]{64}$")
  expect_identical(parsed[[1]]$transport,"fal")
})


test_that("Replicate transcription qualifies its primary audio argument", {
  withr::local_options(list(genflow.models_dir=withr::local_tempdir()))
  expect_true(gen_validate_call("gen_stt",list(audio="recording.wav",service="replicate",model="openai/whisper"))$valid)
})


test_that("editing and raster delivery follow the image endpoint schema", {
  row <- function(formats) {
    x <- list(id="a/edit",supported_parameters=list(input_references=list(min=1L,max=3L),output_format=list(values=formats)))
    jsonlite::fromJSON(.gen_openrouter_row(x,"images/models")$routes,simplifyVector=FALSE)
  }
  expect_false(.gen_match_routes(row("png"),"image.generate",character(),TRUE)$compatible)
  expect_true(.gen_match_routes(row("png"),"image.generate","add_img",TRUE)$compatible)
  expect_false(.gen_match_routes(row("svg"),"image.generate","add_img",TRUE)$compatible)
})
