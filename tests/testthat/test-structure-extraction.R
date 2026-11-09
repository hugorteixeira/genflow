component_node <- function(id,type,config=structure(list(),names=character()),inputs=structure(list(),names=character()))
 list(id=id,type=type,contract_version=gen_block_contracts(type)$version,config=config,inputs=inputs)
component_ref <- function(node,port="asset") list(node=node,port=port)
component_body <- function(...) list(schema_version=1L,nodes=list(...))

test_that("exact selection declares boundary inputs and outputs without ancestors", {
  d <- component_body(
    component_node("brief","input",list(key="brief")),
    component_node("write","text.join",list(parts=list(list(input="text"))),list(text=component_ref("brief"))),
    component_node("outside","output",inputs=list(story=component_ref("write"))))
  original <- serialize(d,NULL)
  r <- gen_extract_structure(d,list("write"))
  expect_true(r$valid)
  expect_identical(serialize(d,NULL),original)
  expect_identical(vapply(r$document$nodes,`[[`,character(1),"type"),c("input","text.join","output"))
  expect_false("brief" %in% vapply(r$document$nodes,`[[`,character(1),"id"))
  expect_identical(r$boundary$inputs,list(text=component_ref("brief")))
  expect_identical(r$boundary$outputs,list(story=component_ref("write")))
  expect_equal(r,gen_extract_structure(d,list("write")))
  expect_equal(r$document,gen_extract_structure(as.character(jsonlite::toJSON(d,auto_unbox=TRUE,null="null")),list("write"))$document)
})

test_that("shared incoming edges stay shared and generated names avoid collisions", {
  d <- component_body(component_node("brief","input",list(key="brief")),
    component_node("component_input_1","text.join",list(parts=list(list(input="a"),list(text=" "),list(input="b"))),
      list(a=component_ref("brief"),b=component_ref("brief"))))
  r <- gen_extract_structure(d,list("component_input_1"))
  expect_true(r$valid)
  expect_length(r$boundary$inputs,1L)
  expect_identical(r$document$nodes[[1]]$id,"component_input_1_2")
  expect_identical(r$document$nodes[[2]]$inputs$a,r$document$nodes[[2]]$inputs$b)
  expect_length(r$boundary$outputs,1L)
})

test_that("typed collection boundaries retain JSON schemas and existing exports", {
  schema <- list(type="object",properties=list(name=list(type="string")))
  d <- component_body(component_node("items","input",list(key="items",kind="json",value_type="list",output_schema=schema)),
    component_node("pick","list.item",list(index=1L),list(list=component_ref("items","list"))),
    component_node("out","output",inputs=list(person=component_ref("pick"))))
  r <- gen_extract_structure(d,list("pick","out"))
  expect_true(r$valid,info=paste(vapply(r$errors,`[[`,character(1),"message"),collapse="; "))
  expect_identical(r$document$nodes[[1]]$config$value_type,"list")
  expect_identical(r$document$nodes[[1]]$config$output_schema,schema)
  expect_identical(names(r$graph$exports),"person")
  expect_identical(r$graph$exports$person$schema,schema)
  only_output <- gen_extract_structure(d,list("out"))
  expect_true(only_output$valid)
  expect_identical(only_output$boundary$outputs,list(person=component_ref("pick")))
  expect_identical(only_output$boundary$inputs,list(person=component_ref("pick")))
})

test_that("bundled extraction prunes unrelated resources and keeps shared components", {
  child <- component_body(component_node("voice","content",list(content_type="voice",source="asset",asset_id="voice")),
    component_node("out","output",inputs=list(voice=component_ref("voice"))))
  d <- list(format="genflow-studio-definition",schema_version=1L,
    root=list(kind="workflow",name="Story",body=component_body(
      component_node("mimi","subworkflow",list(workflow_id="character")),
      component_node("irrelevant","content",list(content_type="text",source="inline",text="hello")))),
    resources=list(list(key="character",kind="workflow",name="Mimi",body=child),
      list(key="unused",kind="prompt",name="Unused",body=list(template="Unused"))),
    assets=list(list(key="voice",kind="audio",sha256=strrep("a",64)),list(key="unused",kind="text",sha256=strrep("b",64))))
  r <- gen_extract_structure(d,list("mimi"))
  expect_true(r$valid)
  expect_length(r$document$resources,1L); expect_length(r$document$assets,1L)
  expect_identical(r$ports$mimi$voice,"asset:audio")
  mounted <- gen_materialize_structure(r$document,"scene")
  expect_true(mounted$valid)
  expect_identical(mounted$document$root$body$nodes[[1]]$config$workflow_id,"scene_character")
})

test_that("reference traversal and mapping touch only current declared fields", {
  b <- component_body(component_node("one","subworkflow",list(workflow_id="child")),
    component_node("two","content",list(content_type="text",source="asset",asset_id="file")),
    component_node("text","content",list(content_type="text",source="inline",text="child file")))
  refs <- gen_definition_references(b,"workflow")
  expect_identical(vapply(refs,`[[`,character(1),"kind"),c("workflow","asset"))
  expect_identical(refs[[1]]$path,"/nodes/0/config/workflow_id")
  renamed <- gen_remap_definition(b,"workflow",list(child="new_child"),list(file="new_file"))
  expect_identical(renamed$nodes[[3]]$config$text,"child file")
  expect_identical(renamed$nodes[[1]]$config$workflow_id,"new_child")
  expect_identical(renamed$nodes[[2]]$config$asset_id,"new_file")
  expect_error(gen_remap_definition(b,"workflow"),class="genflow_validation_error")
})

test_that("bad selections and unsupported boundary types produce canonical errors", {
  d <- component_body(component_node("a","content",list(content_type="text",source="inline",text="hello")))
  for (ids in list(list(),list("missing"),list("a","a"),42)) expect_false(gen_extract_structure(d,ids)$valid)
  expect_false(gen_extract_structure("{")$valid)
  expect_false(gen_extract_structure(d,check_capabilities="no")$valid)
  expect_false(gen_extract_structure()$valid)
  d <- component_body(
    component_node("choices","input",list(key="choices",value_type="list")),
    component_node("votes","input",list(key="votes",value_type="list",kind="ballot")),
    component_node("vote","vote",inputs=list(candidates=component_ref("choices","list"),votes=component_ref("votes","list"))),
    component_node("select","select",inputs=list(candidates=component_ref("choices","list"),evaluation=component_ref("vote","round"))))
  expect_true(gen_validate_structure(d)$valid)
  r <- gen_extract_structure(d,list("select"))
  expect_false(r$valid)
  expect_identical(r$errors[[1]]$code,"component_boundary_type")
})
