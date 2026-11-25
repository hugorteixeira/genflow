test_that("scene ports, recipes and per-occurrence asset references travel together", {
  layer <- list(id="picture",name="Frame",role="image",asset_id="file",input="picture",source="picture",
    linked=TRUE,edited=list(),start_ms=0L,duration_ms=1200L,in_ms=0L,x=50L,y=50L,width=100L,volume=100L)
  part <- list(id="shot",name="Shot",minimum_ms=0L,ignored=list(),edited=list(),elements=list(layer))
  doc <- list(format="genflow-composition",schema_version=1L,layout="timed",canvas=list(width=640L,height=360L),
    ignored=list(),parts=list(part))
  expect_true(gen_validate_call("scene",list(document=doc))$valid)
  expect_equal(gen_block_ports("scene",list(document=doc),list(picture="asset:image",voice="asset:audio"))$ports$asset,"asset:json")
  expect_false(gen_block_ports("scene",list(document=doc),list(picture="asset:json"))$valid)
  expect_false(gen_block_ports("scene",inputs=list(vote="round:image"))$valid)
  expect_false(gen_block_ports("scene",list(document=list(parts=list(2))))$valid)
  bad <- doc; bad$parts[[1]]$elements[[1]]$speed_percent <- 0
  expect_false(gen_validate_call("scene",list(document=bad))$valid)
  expect_true(gen_validate_call("subworkflow",list(workflow_id="group"),1L)$valid)
  expect_false(gen_block_ports("subworkflow",list(workflow_id="group",scene_edit=doc),exports=list(cenas="asset:json"))$valid)
  body <- list(nodes=list(list(id="a",type="output",config=list()),list(id="b",type="scene",config=list(document=doc)),
    list(id="c",type="subworkflow",config=list(workflow_id="group",scene_edit=doc))))
  mapped <- gen_remap_definition(body,"workflow",list(group="group2"),list(file="file2"))
  expect_equal(vapply(mapped$nodes,`[[`,character(1),"id"),c("a","b","c"))
  expect_equal(mapped$nodes[[2]]$config$document$parts[[1]]$elements[[1]]$asset_id,"file2")
  expect_equal(mapped$nodes[[3]]$config$scene_edit$parts[[1]]$elements[[1]]$asset_id,"file2")
  expect_equal(mapped$nodes[[3]]$config$workflow_id,"group2")
})
