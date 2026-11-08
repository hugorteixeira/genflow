definition_bundle <- function() list(format="genflow-studio-definition",schema_version=1L,source="draft",
  root=list(kind="workflow",name="Story",body=list(schema_version=1L,ui=list(viewport=list(x=5,y=9,zoom=1)),nodes=list(
    list(id="brief",type="input",contract_version=2L,config=list(key="brief"),position=list(x=2,y=4)),
    list(id="draft",type="content",config=list(content_type="text",source="ai",ai=list(service="openai",model="gpt-4.1-mini",contract_version=2L,content=list(list(text="Write a story: "),list(input="input")))),inputs=list(input=list(node="brief",port="asset"))),
    list(id="out",type="output",inputs=list(story=list(node="draft",port="asset")))))),
  resources=list(
    list(key="writer",kind="agent",name="Writer",body=list(operation="text.generate",prompt_version_id="prompt",parameters=list(temp=0.7))),
    list(key="prompt",kind="prompt",name="Prompt",body=list(template="Write a story: {{input}}"))),assets=list())

test_that("complete definitions reuse contracts and preserve their portable format", {
  document <- definition_bundle(); before <- serialize(document,NULL)
  testthat::local_mocked_bindings(gen_txt=function(...) stop("PROVIDER CALLED"),.package="genflow")
  report <- gen_validate_structure(document,TRUE)
  expect_true(report$valid,info=paste(vapply(report$errors,`[[`,character(1),"message"),collapse="; "))
  expect_identical(serialize(document,NULL),before)
  expect_identical(report$ports$draft$asset,"asset:text")
  expect_identical(unlist(report$resource_order),c("prompt","writer"))
  expect_identical(report$document$root$body$ui,document$root$body$ui)
  expect_identical(report$document$root$body$nodes[[1]]$position,document$root$body$nodes[[1]]$position)
  expect_equal(report$document$resources[[1]]$body$parameters$temp,0.7)
  expect_identical(report$document$resources[[1]]$body$service,"openai")
  expect_true(gen_validate_structure(report$document,TRUE)$valid)
  json <- jsonlite::toJSON(report$document,auto_unbox=TRUE,null="null",force=TRUE)
  expect_true(gen_validate_structure(as.character(json),TRUE)$valid)
  expect_false(report$assets_verified)
})

test_that("resource diagnostics accumulate with original JSON paths including unused resources", {
  d <- definition_bundle()
  d$resources[[1]]$body$parameters <- list(temp="hot",timeout_api=-1,persist=TRUE)
  d$resources[[2]]$body$template <- " "
  d$resources[[3]] <- list(key="unused",kind="agent",name="Unused",body=list(operation="audio.speak",parameters=list(speed=10)))
  d$root$body$nodes[[1]]$config$kind <- "banana"
  report <- gen_validate_structure(d)
  expect_false(report$valid)
  paths <- vapply(report$errors,`[[`,character(1),"path")
  expect_true(all(c("/resources/0/body/parameters/temp","/resources/0/body/parameters/timeout_api",
    "/resources/0/body/parameters/persist","/resources/1/body/template","/resources/2/body/parameters/speed",
    "/root/body/nodes/0/config/kind") %in% paths))
})

test_that("resource IDs, expected kinds, asset manifests and cycles are checked offline", {
  d <- definition_bundle(); d$resources <- d$resources[-2]
  expect_true(any(vapply(gen_validate_structure(d)$errors,function(e)e$code=="unknown_resource",logical(1))))
  d <- definition_bundle(); d$resources[[1]]$body$prompt_version_id <- "writer"
  expect_true(any(vapply(gen_validate_structure(d)$errors,function(e)e$code=="resource_kind",logical(1))))
  d <- definition_bundle(); d$resources[[2]]$key <- "writer"
  expect_true(any(vapply(gen_validate_structure(d)$errors,function(e)e$code=="duplicate_resource",logical(1))))
  child <- list(schema_version=1L,nodes=list(list(id="recurse",type="subworkflow",config=list(workflow_id="child"))))
  d <- definition_bundle(); d$resources[[3]] <- list(key="child",kind="workflow",name="Child",body=child)
  expect_true(any(vapply(gen_validate_structure(d)$errors,function(e)e$code=="recursive_resource",logical(1))))
  d <- definition_bundle(); d$root$body$nodes[[2]]$config <- list(content_type="text",source="asset",asset_id="file"); d$root$body$nodes[[2]]$inputs <- list()
  expect_true(any(vapply(gen_validate_structure(d)$errors,function(e)e$code=="unknown_asset",logical(1))))
  d$assets <- list(list(key="file",kind="image",sha256=strrep("a",64)))
  expect_true(any(vapply(gen_validate_structure(d)$errors,function(e)e$code=="asset_kind",logical(1))))
  d$assets[[1]]$kind <- "text"
  expect_true(gen_validate_structure(d)$valid)
  d$assets[[1]]$sha256 <- "bad"
  expect_false(gen_validate_structure(d)$valid)
})

test_that("subworkflow bodies and interfaces resolve transitively without UUIDs or layout", {
  d <- definition_bundle(); child <- d$root$body; child$ui <- NULL
  d$resources[[3]] <- list(key="scene",kind="workflow",name="Scene",body=child)
  d$root$body$nodes[[2]] <- list(id="scene",type="subworkflow",config=list(workflow_id="scene"),inputs=list(brief=list(node="brief",port="asset")))
  d$root$body$nodes[[3]]$inputs$story <- list(node="scene",port="story")
  report <- gen_validate_structure(d,TRUE)
  expect_true(report$valid,info=paste(vapply(report$errors,`[[`,character(1),"message"),collapse="; "))
  expect_identical(report$ports$scene$story,"asset:text")
  expect_setequal(unlist(report$resource_order),c("prompt","writer","scene"))
  d$resources[[3]]$body$nodes[[2]]$inputs$input$port <- "missing"
  report <- gen_validate_structure(d)
  expect_false(report$valid)
  expect_true(any(vapply(report$errors,function(e) e$path=="/resources/2/body/nodes/1/inputs/input/port",logical(1))))
})

test_that("piece values, modalities and explicit cardinality agree with bindings", {
  d <- definition_bundle(); d$root$body$nodes[[2]]$config$ai$content <- list(list(input="missing"))
  expect_true(any(vapply(gen_validate_structure(d)$errors,function(e)e$code=="required_input",logical(1))))
  d <- definition_bundle(); d$root$body$nodes[[2]] <- list(id="draft",type="transcribe",config=list(ai=list(service="openai",model="whisper-1")))
  expect_false(gen_validate_structure(d)$valid)
  d <- definition_bundle(); d$root$body$nodes[[2]]$config$count <- 3
  expect_true(any(vapply(gen_validate_structure(d)$errors,function(e)e$code=="unknown_argument",logical(1))))
  d <- definition_bundle(); expect_true(gen_validate_structure(d)$valid)
  expect_identical(gen_validate_structure(d)$ports$draft,list(asset="asset:text"))
})

test_that("bare Studio bodies work when self-contained and retain the source shape", {
  body <- definition_bundle()$root$body
  body$nodes[[2]] <- list(id="draft",type="text.join",contract_version=2L,config=list(parts=list(list(input="input"))),inputs=list(input=list(node="brief",port="asset")))
  checked <- gen_validate_structure(body,TRUE)
  expect_true(checked$valid)
  expect_null(checked$document$root)
  expect_identical(checked$document$nodes[[2]]$type,"text.join")
  body$nodes[[2]]$config$parts <- list(list(input="missing"))
  expect_true(any(vapply(gen_validate_structure(body)$errors,function(e)startsWith(e$path,"/nodes/1"),logical(1))))
  expect_true(gen_validate_structure(definition_bundle()$root$body)$valid)
})

test_that("malformed bundle sections produce reports instead of throwing", {
  mutations <- list(
    function(d) { d$root <- list(); d },
    function(d) { d$resources[[1]]$body$contract_version <- list(bad=1); d },
    function(d) { d$root$body$nodes[[1]] <- "bad"; d },
    function(d) { d$root$body$nodes[[2]]$config <- "bad"; d },
    function(d) { d$root$body$nodes[[2]]$inputs <- list(input=list(node=list(bad=1),port="asset")); d },
    function(d) { d$resources[[1]] <- "bad"; d },
    function(d) { d$root$body$nodes[[1]]$type <- NULL; d },
    function(d) { d$resources[[2]]$body <- FALSE; d },
    function(d) { d$assets <- list(file="bad"); d },
    function(d) { d$root$body$nodes[[2]]$config$source <- list("ai","manual"); d },
    function(d) { d$root$body$nodes[[2]]$config$count <- "bad"; d },
    function(d) { d$root$body$nodes[[2]]$config$artifact_ids <- list(list(bad=1)); d })
  for (mutate in mutations) expect_false(gen_validate_structure(mutate(definition_bundle()))$valid)
  d <- definition_bundle(); d$root$body$ui$api_key <- "a-literal-secret"
  expect_false(gen_validate_structure(as.character(jsonlite::toJSON(d,auto_unbox=TRUE)))$valid)
})

test_that("fixture policies are caller owned and not enabled by document JSON", {
  d <- definition_bundle(); d$resources[[1]]$body$service <- "fixture"; d$resources[[1]]$body$fixture <- list()
  expect_false(gen_validate_structure(d)$valid)
  expect_true(gen_validate_structure(d,check_capabilities=FALSE)$valid)
  d$check_capabilities <- FALSE
  expect_false(gen_validate_structure(d)$valid)
  expect_false(gen_validate_structure(d,check_capabilities="no")$valid)
})

test_that("JSON schemas are checked across collection and subworkflow boundaries", {
  schema <- list(type="object",properties=list(name=list(type="string")))
  d <- definition_bundle(); d$resources <- list()
  d$root$body$nodes <- list(
    list(id="one",type="input",contract_version=2L,config=list(key="one",kind="json",output_schema=schema)),
    list(id="two",type="input",contract_version=2L,config=list(key="two",kind="json",output_schema=list(properties=schema$properties,type="object"))),
    list(id="list",type="list",config=list(order=list("a","b")),inputs=list(a=list(node="one",port="asset"),b=list(node="two",port="asset"))),
    list(id="out",type="output",inputs=list(items=list(node="list",port="list"))))
  expect_true(gen_validate_structure(d)$valid)
  d$root$body$nodes[[2]]$config$output_schema$properties$name$type <- "integer"
  expect_true(any(vapply(gen_validate_structure(d)$errors,function(e)e$code=="mixed_schemas",logical(1))))
  d <- definition_bundle(); d$root$body$nodes[[2]]$config$kind <- "invalid"
  expect_false(gen_validate_structure(d)$valid)
})

test_that("R empty object fields round trip through JSON without confusing true arrays", {
  d <- definition_bundle(); d$resources[[1]]$body$parameters <- list()
  d$root$body$nodes[[3]]$config <- list(); d$root$body$nodes[[1]]$inputs <- list()
  d$resources[[1]]$body$parameters <- structure(list(), names=character())
  d$root$body$nodes[[3]]$config <- structure(list(), names=character())
  d$root$body$nodes[[1]]$inputs <- structure(list(), names=character())
  encoded <- as.character(jsonlite::toJSON(d,auto_unbox=TRUE,null="null"))
  report <- gen_validate_structure(encoded)
  expect_true(report$valid)
  expect_identical(report$document$root$body$nodes[[3]]$config,structure(list(),names=character()))
  expect_null(names(report$document$root$body$nodes))
  expect_true(gen_validate_structure(report$document)$valid)
})


test_that("definition text limits use bytes without restricting ordinary multilingual content", {
  d <- definition_bundle(); d$root$name <- strrep("名",70)
  expect_true(any(vapply(gen_validate_structure(d)$errors,function(e)e$path=="/root/name" && e$code=="too_long",logical(1))))
  d$root$name <- "قصة قصيرة"
  d$resources[[2]]$body$template <- "اكتب قصة قصيرة: {{input}}"
  expect_true(gen_validate_structure(d)$valid)
})
test_that("shared definition cases produce prepared plans without losing invalid literals", {
  cases <- jsonlite::read_json(system.file("examples","definition-parity.json",package="genflow"),simplifyVector=FALSE)
  for (case in cases) {
    checked <- gen_validate_structure(case$document,TRUE,check_capabilities=FALSE)
    expect_identical(checked$valid,case$valid,info=case$name)
    if (!case$valid) expect_true(case$error_code %in% vapply(checked$errors,`[[`,character(1),"code"),info=case$name)
    if (case$valid) {
      expect_equal(checked$graph$ports,checked$ports)
      expect_equal(checked$graph$order,checked$order)
      expect_true(gen_validate_structure(checked$document,TRUE,check_capabilities=FALSE)$valid,info=case$name)
    }

  }
})

test_that("agent preparation pins versions and runtime ownership is independent of visibility", {
  original <- gen_block_contracts
  catalog <- original(); v2 <- original("gen_txt")
  v2$version <- 2L; v2$arguments$properties$temp$default <- 0.25
  v2$arguments$properties$persist[["x-ui"]]$hidden <- FALSE
  catalog[["gen_txt@2"]] <- v2
  testthat::local_mocked_bindings(gen_block_contracts=function(block=NULL,version=NULL) {
    if (is.null(block)) return(catalog)
    value <- catalog[[paste0(block,"@",version %||% original(block)$version)]]
    if (is.null(value)) return(original(block,version))
    value
  },.package="genflow")
  a <- list(operation="text.generate",service="fixture")
  expect_equal(gen_validate_agent_definition(a,FALSE)$body$parameters$temp,0.25)
  a$contract_version <- 1L
  expect_false(gen_validate_agent_definition(a,FALSE)$valid)
  a$contract_version <- 2L
  expect_equal(gen_validate_agent_definition(a,FALSE)$body$parameters$temp,0.25)
  a$parameters <- list(persist=TRUE)
  expect_false(gen_validate_agent_definition(a,FALSE)$valid)
  a$parameters <- list(); a$contract_version <- 99L
  expect_false(gen_validate_agent_definition(a,FALSE)$valid)
})

test_that("bundle diagnostics separate incomplete roots from invalid dependencies", {
  d <- definition_bundle(); d$root$body$nodes <- list()
  checked <- gen_validate_structure(d,TRUE)
  expect_false(checked$valid)
  expect_length(checked$bundle_errors,0L)
  d$resources[[1]]$body$parameters$temp <- "invalid"
  checked <- gen_validate_structure(d,TRUE)
  expect_false(checked$valid)
  expect_true(any(vapply(checked$bundle_errors,function(e)grepl("/resources/0/body/parameters/temp",e$path,fixed=TRUE),logical(1))))
  d <- definition_bundle(); d$resources <- d$resources[-2]
  checked <- gen_validate_structure(d,TRUE)
  expect_true(any(vapply(checked$bundle_errors,function(e)e$code=="unknown_resource",logical(1))))
})

test_that("invalid node configuration remains a structured diagnostic", {
  for (value in list(7, TRUE, "bad", list("bad"))) {
    d <- list(schema_version=1L,nodes=list(list(id="in",type="input",contract_version=2L,config=value)))
    checked <- gen_validate_structure(d)
    expect_false(checked$valid)
    expect_gt(length(checked$errors),0L)
  }
})

test_that("preparation preserves authored empty connection objects", {
  for (empty in list(list(), structure(list(), names=character()))) {
    d <- list(schema_version=1L, nodes=list(
      list(id="text",type="content",config=list(content_type="text",source="inline",text="Hello"),inputs=empty),
      list(id="out",type="output",inputs=list(result=list(node="text",port="asset")))))
    before <- serialize(d,NULL)
    checked <- gen_validate_structure(d,TRUE)
    expect_true(checked$valid)
    expect_identical(checked$document$nodes[[1]]$inputs,structure(list(),names=character()))
    expect_identical(serialize(d,NULL),before)
  }
})
