test_that("installed contracts own every public argument and default", {
  for (name in c("gen_txt","gen_img","gen_stt","gen_tts","gen_vote")) {
    contract <- gen_block_contracts(name)
    fun <- get(if (name=="gen_vote") name else paste0(name,".default"),asNamespace("genflow"))
    expect_setequal(setdiff(names(formals(fun)),"..."),names(contract$arguments$properties))
    for (key in names(contract$arguments$properties)) {
      property <- contract$arguments$properties[[key]]
      if ("default" %in% names(property)) {
        expected <- genflow:::.gen_default(name,key)
        expect_equal(eval(formals(fun)[[key]],environment(fun)),expected,info=paste(name,key))
        expect_true(gen_validate_schema(property$default,property)$valid,info=paste(name,key))
      }
    }
  }
  expect_error(gen_block_contracts("gen_img",999),"Unknown block contract")
  expect_error(gen_block_contracts("gen_img","../1"),"positive integer")
})

test_that("omission, explicit NULL, invalid values, and connections stay distinct", {
  base <- gen_validate_call("gen_img",list(prompt="garden"))
  expect_true(base$valid)
  expect_equal(base$args$steps,18)
  expect_equal(base$args$model,"black-forest-labs/FLUX.1-schnell")
  expect_true("steps" %in% unlist(base$defaulted))
  wrong <- gen_validate_call("gen_img",list(prompt=" ",steps="18",h=0,typo=TRUE))
  expect_false(wrong$valid)
  expect_setequal(vapply(wrong$errors,`[[`,character(1),"path"),c("/args/prompt","/args/steps","/args/h","/args/typo"))
  expect_identical(wrong$args$steps,"18")
  expect_false(gen_validate_call("gen_img",list(prompt="garden",steps=NULL))$valid)
  expect_true(gen_validate_call("gen_tts",list(text="Hello",voice=NULL))$valid)
  pending <- gen_validate_call("gen_img",list(),bound=c("prompt","steps"))
  expect_true(pending$valid)
  expect_false("steps" %in% names(pending$args))
  expect_false("prompt" %in% names(pending$args))
  expect_false(gen_validate_call("gen_img",list(prompt="garden"),bound="prompt")$valid)
  expect_false(gen_validate_call("gen_tts",list(text="Hello",speed=10))$valid)
})

test_that("all execution entry points reject invalid calls before side effects", {
  testthat::local_mocked_bindings(
    .gen_img_openai=function(...) stop("PROVIDER WAS CALLED"),
    .gen_txt_openai=function(...) stop("PROVIDER WAS CALLED"),
    .tts_openai=function(...) stop("PROVIDER WAS CALLED"),
    .stt_prepare_audio=function(...) stop("AUDIO WAS OPENED"),
    .package="genflow")
  calls <- list(function() gen_img("",h=0,steps="bad",service="openai"),
                function() gen_img(h=0,steps="bad"),
                function() gen_txt("",timeout_api=-1),
                function() gen_tts("",speed=20),
                function() gen_stt("no-file.wav",timeout_api=-1,convert="yes"),
                function() gen_vote(trigger="",min_votes=0))
  for (call in calls) {
    error <- tryCatch(call(),error=function(e)e)
    expect_s3_class(error,"genflow_validation_error")
    expect_gte(length(error$errors),2)
    expect_false(grepl("WAS CALLED|WAS OPENED",conditionMessage(error)))
  }
})

test_that("schema validation collects nested errors and rejects unsupported vocabulary", {
  schema <- list(type="object",additionalProperties=FALSE,required=list("name"),
    properties=list(name=list(type="string",pattern="\\S"),
      values=list(type="array",items=list(type="number",minimum=1))))
  checked <- gen_validate_schema(list(name="",values=list(0,"bad"),extra=TRUE),schema)
  expect_false(checked$valid)
  expect_length(checked$errors,4)
  expect_false(gen_check_schema(list(type="string",unrecognized=TRUE))$valid)
  expect_false(gen_check_schema(list(type="string",pattern="["))$valid)
  expect_false(gen_check_schema(list(type=NULL,enum=list(),uniqueItems=c(TRUE,FALSE)))$valid)
  expect_true("minimum" %in% unlist(gen_check_schema(list())$keywords))
  expect_false(gen_validate_call("gen_txt",list(context="hello",my_tools=function() 1))$valid)
  expect_false(gen_validate_schema(jsonlite::parse_json("[]"),list(type="object"))$valid)
  expect_false(gen_validate_schema(jsonlite::parse_json("{}"),list(type="array"))$valid)
  expect_false(gen_validate_structure('{"schema_version":1,"nodes":[{"id":"out","block":"output","args":[]}]}')$valid)
  expect_false(gen_validate_structure()$valid)
})

test_that("workflow validation reports independent errors without running a node", {
  testthat::local_mocked_bindings(gen_img=function(...) stop("RUN"),gen_tts=function(...) stop("RUN"),.package="genflow")
  document <- list(schema_version=1,nodes=list(
    list(id="image",block="gen_img",args=list(prompt="",steps="bad",h=0)),
    list(id="voice",block="gen_tts",args=list(text="hello",speed=10)),
    list(id="missing",block="gen_txt",inputs=list(context=list(node="absent",port="content")))))
  before <- serialize(document,NULL)
  checked <- gen_validate_structure(document)
  expect_false(checked$valid)
  expect_gte(length(checked$errors),5)
  expect_setequal(unique(vapply(checked$errors,`[[`,character(1),"node_id")),c("image","voice","missing"))
  expect_identical(serialize(document,NULL),before)
  expect_false(gen_validate_structure('{"nodes": [}')$valid)
  expect_false(gen_validate_structure("system('echo SHOULD_NOT_RUN')")$valid)
  expect_false(gen_validate_structure("https://example.test/workflow.json")$valid)
})

test_that("connections validate type, port, ordering, cycles and contract version", {
  expect_equal(gen_block_ports("generate",list(source="provided",kind="text",artifact_ids=list("one")))$ports$asset,"asset:text")
  expect_null(gen_block_ports("generate",list(source="manual",kind="text"))$ports$asset)
  document <- list(schema_version=1,nodes=list(
    list(id="text",block="gen_txt",args=list(context="Tell a story")),
    list(id="voice",block="gen_tts",inputs=list(text=list(node="text",port="content")))) ,
    outputs=list(audio=list(node="voice",port="content")))
  checked <- gen_validate_structure(document,require_output=TRUE)
  expect_true(checked$valid,info=paste(vapply(checked$errors,`[[`,character(1),"message"),collapse="; "))
  expect_equal(unlist(checked$order),c("text","voice"))
  expect_equal(checked$ports$voice$content,"asset:audio")
  document$nodes[[1]]$block <- "gen_img"
  document$nodes[[1]]$args <- list(prompt="garden")
  expect_true(any(vapply(gen_validate_structure(document)$errors,function(e)e$code=="incompatible_input",logical(1))))
  document$nodes[[2]]$inputs$text$port <- "missing"
  expect_true(any(vapply(gen_validate_structure(document)$errors,function(e)e$code=="unknown_port",logical(1))))
  document$nodes[[1]]$args <- list()
  document$nodes[[1]]$inputs <- list(prompt=list(node="voice",port="content"))
  expect_true(any(vapply(gen_validate_structure(document)$errors,function(e)e$code=="cycle",logical(1))))
  document$nodes[[1]]$contract_version <- 99
  expect_false(gen_validate_structure(document)$valid)
  expect_false(gen_validate_structure(list(schema_version=1,nodes=list(list(id="bad",block="not_a_block"))))$valid)
  expect_false(gen_validate_structure(list(schema_version=1,nodes=list(list(block="gen_img"))))$valid)
})

test_that("all declared subworkflows are checked and nested interfaces are inferred", {
  child <- list(schema_version=1,nodes=list(
    list(id="brief",block="input",args=list(key="brief",kind="text")),
    list(id="image",block="gen_img",inputs=list(prompt=list(node="brief",port="asset")))),
    outputs=list(image=list(node="image",port="content")))
  parent <- list(schema_version=1,workflows=list(scene=child),nodes=list(
    list(id="text",block="gen_txt",args=list(context="A garden")),
    list(id="scene",block="subworkflow",args=list(workflow_version_id="scene"),inputs=list(brief=list(node="text",port="content")))),
    outputs=list(image=list(node="scene",port="image")))
  good <- gen_validate_structure(parent,TRUE)
  expect_true(good$valid,info=paste(vapply(good$errors,`[[`,character(1),"message"),collapse="; "))
  expect_identical(good$ports$scene$image,"asset:image")
  expect_true(gen_validate_structure(good$document,TRUE)$valid)
  parent$workflows$unused <- list(schema_version=1,nodes=list(list(id="bad",block="gen_tts",args=list(text="",speed=10))))
  bad <- gen_validate_structure(parent)
  expect_false(bad$valid)
  expect_gte(sum(vapply(bad$errors,function(e)startsWith(e$path,"/workflows/unused"),logical(1))),2)
  parent$workflows$scene$nodes[[2]] <- list(id="image",block="subworkflow",args=list(workflow_version_id="scene"))
  expect_true(any(vapply(gen_validate_structure(parent)$errors,function(e)e$code=="recursive_workflow",logical(1))))
})

test_that("prepared STT scripts stay portable and schema arrays do not accept scalars", {
  result <- gen_validate_structure(list(id="stt",block="gen_stt",args=list(audio="input.wav")))
  expect_true(result$valid)
  expect_false("api_key" %in% names(result$document$nodes[[1]]$args))
  expect_true(gen_validate_structure(result$document)$valid)
  expect_false(gen_validate_schema("scalar",list(type="array",items=list(type="string")))$valid)
})
test_that("JSON equality preserves keys and exact values and ignores object order", {
  schema <- list(type="object",enum=list(list(right=1)))
  expect_false(gen_validate_schema(list(wrong=1),schema)$valid)
  expect_false(gen_validate_schema(1+1e-9,list(type="number",enum=list(1)))$valid)
  expect_true(gen_validate_schema(1L,list(type="number",enum=list(1)))$valid)
  expect_true(gen_validate_schema(list(b=2,a=1),list(type="object",enum=list(list(a=1,b=2))))$valid)
  expect_false(gen_validate_schema(list(2,1),list(type="array",enum=list(list(1,2))))$valid)
  expect_false(gen_check_schema(list(enum=list(list(a=1,b=2),list(b=2,a=1))))$valid)
  expect_false(gen_validate_schema(list(list(a=1,b=2),list(b=2,a=1)),list(type="array",uniqueItems=TRUE))$valid)
})

test_that("call and port validation require exactly one identified contract", {
  for (block in list(NULL,"",NA_character_,c("gen_txt","gen_img"),list("gen_txt"),FALSE)) {
    call <- gen_validate_call(block)
    ports <- gen_block_ports(block)
    expect_false(call$valid)
    expect_false(ports$valid)
    expect_identical(call$errors[[1]]$code,"unknown_contract")
    expect_identical(ports$errors[[1]]$code,"unknown_contract")
  }
  expect_length(gen_block_contracts(),24L)
})
