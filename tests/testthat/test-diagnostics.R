assert_diagnostic <- function(error) {
  checked <- gen_validate_schema(error, gen_error_catalog()$schema)
  expect_true(checked$valid, info=paste(vapply(checked$errors, `[[`, character(1), "message"),collapse="; "))
  expect_identical(error$diagnostic_version, 1L)
  expect_false(grepl("Expected | must | should |Unknown ",error$message))
}

test_that("schema rules produce complete typed diagnostics without values", {
  schema <- list(type="object", additionalProperties=FALSE, required=list("missing"), properties=list(
    missing=list(type="string"), text=list(type="string", minLength=3, maxLength=4, pattern="^a"),
    num=list(type="integer",minimum=1,maximum=2), flag=list(type="boolean"),
    items=list(type="array",minItems=2,maxItems=3,uniqueItems=TRUE,items=list(type="string")),
    choice=list(enum=list("a","b")), `a/~b`=list(type="string")))
  value <- list(text="",num=0,flag="password=do-not-return",items=list(1,1),choice="c",`a/~b`=42,extra=TRUE)
  report <- gen_validate_schema(value,schema)
  expect_false(report$valid)
  expect_true(all(c("required","unknown_argument","invalid_pattern","too_short","out_of_range","invalid_type","duplicate_item","invalid_enum") %in% vapply(report$errors,`[[`,character(1),"code")))
  for (e in report$errors) assert_diagnostic(e)
  expect_true(any(vapply(report$errors,function(e)identical(e$path,"/a~1~0b"),logical(1))))
  expect_false(grepl("do-not-return",jsonlite::toJSON(report,auto_unbox=TRUE)))
  for (case in list(list("too_long","abcde",list(type="string",maxLength=4)),
    list("too_many_items",list(1,2),list(type="array",maxItems=1)),
    list("too_few_items",list(),list(type="array",minItems=1)),
    list("invalid_type",NULL,list(type="string")),
    list("out_of_range",1,list(type="number",exclusiveMaximum=1)),
    list("out_of_range",1,list(type="number",exclusiveMinimum=1)))) {
    e <- gen_validate_schema(case[[2]],case[[3]])$errors[[1]]
    expect_identical(e$code,case[[1]]); assert_diagnostic(e)
    expect_false(grepl("[{]",e$message))
  }
})

test_that("schema mistakes are distinct from bad values", {
  schemas <- list(NULL, list(type="bogus"),list(type="object",properties="bad"),
    list(type="string",pattern="["), list(type="number",minimum="x"),
    list(type="number",minimum=2,maximum=1),list(type="array",uniqueItems=1),
    list(type="string",anyOf=list()),list(type="object",required=list("absent")),
    list(type="string",enum=list("same","same")))
  for (schema in schemas) {
    report <- gen_validate_schema("anything",schema)
    expect_false(report$valid)
    for (e in report$errors) {
      expect_identical(e$code,"invalid_schema"); expect_identical(e$category,"schema")
      expect_false(identical(e$message_key,"invalid_schema")); assert_diagnostic(e)
    }
  }
})

test_that("original paths identify the right node and field in each scope", {
  body <- list(schema_version=1,nodes=list(
    list(id="same",type="content",label="Primeira peça",config=list(content_type="text",source="inline",text=123)),
    list(id="same",type="content",label="Segunda peça",config=list(content_type="text",source="inline",text=FALSE))))
  bundle <- list(format="genflow-studio-definition",schema_version=1,
    root=list(kind="workflow",name="Root",body=body), resources=list(
      list(key="child",kind="workflow",name="Child",body=list(schema_version=1,nodes=list(
        list(id="same",type="content",label="Peça filha",config=list(content_type="text",source="inline",text=123)))))),assets=list())
  report <- gen_validate_structure(bundle,check_capabilities=FALSE)
  typed <- Filter(function(e)e$code=="invalid_type",report$errors)
  expect_length(typed,3)
  for (e in typed) {
    assert_diagnostic(e)
    expect_identical(e$node_id,"same"); expect_identical(e$field,"Texto")
    expect_identical(e$schema_path,"/arguments/properties/text/type")
    expect_identical(e$block,"content")
  }
  expect_setequal(vapply(typed,`[[`,character(1),"node_label"),c("Primeira peça","Segunda peça","Peça filha"))
  expect_true(any(vapply(typed,function(e)startsWith(e$path,"/resources/0/body/nodes/0/config/text"),logical(1))))
  expect_match(gen_error_summary(typed[[1]]),"Peça.*campo.*Esperado")
  broken <- gen_validate_structure('{"nodes":[}',check_capabilities=FALSE)
  expect_identical(broken$errors[[1]]$code,"invalid_json")
  assert_diagnostic(broken$errors[[1]])
})

test_that("conditions and runtime outcomes preserve codes independently of translations", {
  error <- tryCatch(gen_img(prompt=123,persist=FALSE),error=identity)
  expect_s3_class(error,"genflow_validation_error")
  diagnostic <- gen_diagnostic(error)
  expect_identical(diagnostic$code,"validation_failed")
  expect_identical(diagnostic$causes[[1]]$code,"invalid_type")
  assert_diagnostic(diagnostic)
  for (case in list(c("TIMEOUT_ERROR: timeout","unknown"), c("API_ERROR: HTTP 400","failed"),
    c("EMPTY_OR_NULL_RESPONSE","failed"),c("CONTENT_FILTERED: refusal","failed"))) {
    e <- genflow:::.genflow_provider_failure(case[[1]])$error
    expect_identical(e$outcome,case[[2]]); assert_diagnostic(e)
    changed <- e; changed$message <- "Texto traduzido sem palavras do provedor."
    expect_identical(gen_diagnostic(changed)$outcome,e$outcome)
    expect_identical(gen_diagnostic(changed)$retryable,e$retryable)
  }
  success <- genflow:::.genflow_provider_success("TIMEOUT_ERROR: This is generated fiction.")
  expect_null(success$error)
  result <- genflow:::.genflow_complete_result(list(status_api="SUCCESS",status_msg="OK",response_value="preserved"),
    persistence=list(status="failed",message="password=private"))
  expect_identical(result$inference_status,"SUCCESS")
  expect_identical(result$response_value,"preserved")
  expect_identical(result$error$outcome,"succeeded")
  expect_false(result$error$retryable)
  expect_false(grepl("private",jsonlite::toJSON(result$error,auto_unbox=TRUE)))
  assert_diagnostic(result$error)
})

test_that("unknown exceptions are safe and legacy diagnostics remain readable", {
  e <- gen_diagnostic(simpleError("SQL password=private provider-body=private"))
  assert_diagnostic(e)
  expect_identical(e$condition_class,"simpleError")
  expect_false(grepl("private",jsonlite::toJSON(e,auto_unbox=TRUE)))
  for (value in list(list(code="invalid_type",path="/a",message="Expected string"),
    list(code="future_provider_failure",message="private"),list(code="secret=private",details="not an object"))) {
    adapted <- gen_diagnostic(value); assert_diagnostic(adapted)
    expect_false(grepl("private|[{]",adapted$message))
  }
})

test_that("origin logging preserves references and redacts credential formats", {
  logs <- character()
  raw <- simpleError('password="secret one" token=secret-two {"api_key":"secret-three"} Bearer secret-four https://user:secret-five@host/path')
  value <- withCallingHandlers(gen_diagnostic(raw,log=TRUE),message=function(m) {
    logs <<- c(logs,conditionMessage(m)); invokeRestart("muffleMessage")
  })
  assert_diagnostic(value)
  expect_match(value$reference,"^[a-f0-9-]{36}$")
  expect_true(any(grepl(value$reference,logs,fixed=TRUE)))
  expect_false(any(grepl("secret",c(logs,jsonlite::toJSON(value,auto_unbox=TRUE)))))
  expect_message(gen_diagnostic(value,log=TRUE),NA)
  for (status in c(400,401,403,404,429,503)) {
    error <- suppressMessages(genflow:::.genflow_provider_failure(paste("API_ERROR: HTTP",status,"secret-provider-detail")))$error
    expect_identical(error$params$status_code,as.integer(status))
    expect_identical(error$code,"API_ERROR")
    expect_false(grepl("secret-provider-detail",jsonlite::toJSON(error,auto_unbox=TRUE)))
    assert_diagnostic(error)
  }
})

test_that("malformed diagnostics cannot break error presentation", {
  values <- list(list(code=list("bad"),category=list(),source=1,stage=NA,outcome="typo",node_id=list(),params=NULL),
    list(code="invalid_type",message_key=list(),field=list(),contract_version="one",causes=list(list(code="required"))))
  for (value in values) {
    result <- gen_diagnostic(value); assert_diagnostic(result)
    expect_type(gen_error_summary(result),"character")
  }
  choice <- gen_validate_schema("wrong",list(type="string",enum=list("first","second")))$errors[[1]]
  expect_identical(choice$params$allowed_values,list("first","second"))
  expect_match(choice$message,'"first", "second"',fixed=TRUE)
})

test_that("batch summaries preserve structured worker causes", {
  error <- gen_diagnostic("invalid_type",path="/args/prompt",params=list(expected_types=list("string"),actual_type="number"))
  worker <- list(label="worker1",index=1,tipo_agente="text",response=NULL,erro=error$message,
    diagnostic=error,logs="",duration_individual=0)
  result <- genflow:::.process_parallel_results(list(worker),1,"worker","numeric")
  expect_identical(result$diagnostics[[1]],error)
})

test_that("typed provider conditions survive generator adapters", {
  failure <- function(...) genflow:::.gen_stop("missing_credential",params=list(credential="FIXTURE_KEY"),stage="preflight")
  testthat::local_mocked_bindings(.gen_txt_openai=failure,.gen_img_openai=failure,
    .package="genflow")
  text <- gen_txt("fixture",service="openai",model="fixture",persist=FALSE)
  invisible(capture.output(img <- suppressMessages(gen_img("fixture",service="openai",model="fixture",persist=FALSE))))
  for (result in list(text,img)) {
    expect_identical(result$status_api,"ERROR")
    expect_identical(result$error$code,"missing_credential")
    expect_identical(result$error$outcome,"not_started")
    expect_match(result$error$message,"FIXTURE_KEY")
    assert_diagnostic(result$error)
  }
  tie <- tryCatch(gen_vote(c("option 1","option 2"),trigger="option",ties="error"),error=identity)
  expect_identical(tie$diagnostic$code,"vote_tie")
  expect_match(conditionMessage(tie),"empatada")
})
