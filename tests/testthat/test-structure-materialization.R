materialization_bundle <- function() list(format="genflow-studio-definition",schema_version=1L,
  root=list(kind="workflow",name="Scene",body=list(schema_version=1L,nodes=list(
    list(id="brief",type="input",config=list(key="brief",kind="text")),
    list(id="write",type="generate",config=list(agent_version_id="writer"),inputs=list(input=list(node="brief",port="asset"))),
    list(id="out",type="output",inputs=list(story=list(node="write",port="asset"))),
    list(id="unused",type="template",config=list(template="Unused"))))),
  resources=list(list(key="writer",kind="agent",name="Writer",body=list(operation="text.generate",prompt_version_id="prompt")),
    list(key="prompt",kind="prompt",name="Prompt",body=list(template="Write: {{input}}")),
    list(key="unused",kind="prompt",name="Unused",body=list(template="Unused"))),assets=list())

test_that("materialization includes ancestors, prunes resources and namespaces exact references", {
  d <- materialization_bundle(); before <- serialize(d,NULL)
  local_mocked_bindings(gen_txt=function(...)stop("provider called"),.package="genflow")
  report <- gen_materialize_structure(d,"scene1",list("out"))
  expect_true(report$valid)
  expect_identical(serialize(d,NULL),before)
  expect_identical(unlist(report$node_map),c(brief="scene1_brief",write="scene1_write",out="scene1_out"))
  expect_identical(report$input_map,list(brief="scene1_brief"))
  expect_identical(report$resource_sources,list(scene1_writer="writer",scene1_prompt="prompt"))
  expect_length(report$document$resources,2L)
  expect_identical(report$document$resources[[1]]$body$prompt_version_id,"scene1_prompt")
  expect_identical(report$document$root$body$nodes[[2]]$inputs$input,list(node="scene1_brief",port="asset"))
  expect_identical(report$document$root$body$nodes[[2]]$config$agent_version_id,"scene1_writer")
  expect_identical(names(report$document$root$body$nodes[[3]]$inputs),"scene1_story")
  expect_true(gen_validate_structure(report$document,TRUE)$valid)
  expect_identical(report,gen_materialize_structure(d,"scene1",list("out")))
  expect_equal(gen_materialize_structure(as.character(jsonlite::toJSON(d,auto_unbox=TRUE,null="null")),"scene1",list("out"))$document,report$document)
})

test_that("materialization retains local scopes, assets and caller data", {
  d <- materialization_bundle()
  child <- d$root; child$key <- "child"
  d$resources <- c(d$resources,list(child))
  d$root$body$nodes <- list(list(id="in",type="input",config=list(key="brief")),
    list(id="call",type="subworkflow",config=list(workflow_version_id="child"),inputs=list(brief=list(node="in",port="asset"))))
  r <- gen_materialize_structure(d,"s")
  expect_true(r$valid)
  child <- Filter(function(x)x$key=="s_child",r$document$resources)[[1]]
  expect_identical(child$body$nodes[[1]]$id,"brief")
  expect_identical(child$body$nodes[[1]]$config$key,"brief")
  expect_identical(child$body$nodes[[2]]$config$agent_version_id,"s_writer")
  expect_identical(names(r$document$root$body$nodes[[2]]$inputs),"brief")
  d <- list(schema_version=1L,nodes=list(list(id="a",type="template",config=list(template="Hello"),notes="Keep me",position=list(x=1,y=2))))
  expect_equal(gen_materialize_structure(d)$document$nodes[[1]]$position,d$nodes[[1]]$position)
  bundle <- list(format="genflow-studio-definition",schema_version=1L,root=list(kind="workflow",name="Assets",body=list(schema_version=1L,
    nodes=list(list(id="a",type="generate",config=list(source="provided",kind="text",artifact_ids=list("a")))))),
    assets=list(list(key="a",kind="text",sha256=paste(rep("a",64),collapse=""))))
  r <- gen_materialize_structure(bundle,"s")
  expect_true(r$valid)
  expect_identical(r$asset_sources,list(s_a="a"))
  expect_identical(r$document$root$body$nodes[[1]]$config$artifact_ids,list("s_a"))
})

test_that("invalid selections and documents stop before materialization", {
  d <- materialization_bundle()
  for (args in list(list(document="{"),list(document=d,prefix="bad!"),list(document=d,node_ids=list()),
    list(document=d,node_ids=list("out","out")),list(document=d,node_ids=list("missing")),list(document=d,check_capabilities="no"),list()))
    expect_false(do.call(gen_materialize_structure,args)$valid)
  expect_false(gen_materialize_structure(list(schema_version=1L,nodes=list(list(id="a",block="template",args=list(template="x")))))$valid)
  d$root$body$nodes[[1]]$id <- paste(rep("a",64),collapse="")
  d$root$body$nodes[[2]]$inputs$input$node <- d$root$body$nodes[[1]]$id
  expect_false(gen_materialize_structure(d,"scene")$valid)
  d <- materialization_bundle(); d$root$body$nodes[[2]]$inputs$input$port <- "missing"
  d$resources[[1]]$body$parameters <- list(temp="bad")
  expect_gte(length(gen_materialize_structure(d)$errors),2L)
  node <- list(schema_version=1L,nodes=list(list(id="mount",type="materialize",config=list(document=materialization_bundle()))))
  expect_true(gen_validate_structure(node)$valid)
  expect_equal(gen_validate_structure(node)$ports$mount,list(report="asset:json",document="asset:json"))
})
