voice_content <- function() list(content_type="voice",source="ai",ai=list(
  service="local-native",model="voice.gguf",contract_version=4L,
  parameters=list(),content=list(list(text="Olá, amigos!"))),
  additions=list(list(role="voice_instructions",value=list(input="style"))))

test_that("voice direction is a separate declared content input", {
  args <- voice_content()
  expect_true(gen_validate_call("content",args,check_capabilities=FALSE)$valid)
  expect_identical(gen_block_contracts("gen_tts",4)$studio$bindings$instructions,"instructions")
  for (type in c("asset:text","asset:json"))
    expect_true(gen_block_ports("content",args,list(style=type))$valid)
  for (type in c("asset:audio","asset:image","list:text"))
    expect_false(gen_block_ports("content",args,list(style=type))$valid)
  args$additions[[2]] <- args$additions[[1]]
  expect_true("content_cardinality" %in% vapply(gen_validate_call("content",args,check_capabilities=FALSE)$errors,`[[`,"","code"))
})

test_that("agent literal direction is allowed only without a content direction", {
  args <- voice_content();args$ai$parameters$instructions <- "Voz jovem e suave"
  expect_false(gen_validate_call("content",args,check_capabilities=FALSE)$valid)
  args$additions <- NULL
  expect_true(gen_validate_call("content",args,check_capabilities=FALSE)$valid)
  args$additions <- list(list(value=list(text="inválido")))
  expect_false(gen_validate_call("content",args,check_capabilities=FALSE)$valid)
})

test_that("voice direction survives template extraction and materialization", {
  n <- list(id="voice",type="content",contract_version=1L,config=voice_content(),inputs=list(style=list(node="style",port="asset")))
  d <- list(schema_version=1L,nodes=list(
    list(id="style",type="content",contract_version=1L,config=list(content_type="text",source="inline",text="Voz alegre")),n))
  extracted <- gen_extract_structure(d,list("voice"),check_capabilities=FALSE,purpose="authoring")
  expect_true(extracted$valid,info=toString(extracted$errors))
  expect_identical(extracted$boundary$inputs$style,list(node="style",port="asset"))
  mounted <- gen_materialize_structure(extracted$document,"mimi",check_capabilities=FALSE,purpose="authoring")
  expect_true(mounted$valid,info=toString(mounted$errors))
  node <- Filter(function(x) x$type=="content",mounted$document$nodes)[[1]]
  expect_identical(node$config$additions,n$config$additions)
  expect_match(node$inputs$style$node,"^mimi_")
})
