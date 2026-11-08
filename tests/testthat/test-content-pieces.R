.content_node <- function(id, kind="text", source="inline", ...) list(id=id,type="content",contract_version=1L,
  config=c(list(content_type=kind,source=source),list(...)),inputs=list())
.content_workflow <- function(...) list(schema_version=1L,nodes=list(...))
.content_ai <- function(...) c(list(service="openai",model="gpt-5-mini",contract_version=2L,content=list(list(text="Leia.")),parameters=structure(list(),names=character())),list(...))

test_that("fixed purpose limits sources and returns exactly one typed asset", {
  catalog <- gen_block_contracts("content")
  for (kind in names(catalog$pieces)) for (source in c("inline","asset","ai")) {
    args <- list(content_type=kind,source=source)
    if (source=="inline") args$text <- "Olá {{literal}}"
    if (source=="asset") args$asset_id <- "saved"
    if (source=="ai") {
      spec <- catalog$pieces[[kind]]
      args$ai <- list(service="openai",model="test-model",contract_version=spec$function_version %||% 1L,
        content=list(list(text="Olá")))
    }
    checked <- gen_validate_call("content",args,check_capabilities=FALSE)
    if (!source %in% unlist(catalog$pieces[[kind]]$sources)) expect_false(checked$valid,info=paste(kind,source))
    if (source=="inline" && kind=="text") expect_true(checked$valid)
    ports <- gen_block_ports("content",args)
    expect_named(ports$ports,"asset")
    expect_identical(ports$ports$asset,paste0("asset:",catalog$pieces[[kind]]$output))
  }
})

test_that("inline text stays literal and JSON is checked before execution", {
  n <- .content_node("literal",text="{{foo}}")
  report <- gen_validate_structure(.content_workflow(n))
  expect_true(report$valid,info=toString(report$errors)); expect_true(report$ready)
  expect_identical(report$document$nodes[[1]]$config$text,"{{foo}}")
  n$config$format <- "json"; n$config$text <- "{bad}"
  expect_true("literal_json" %in% vapply(gen_validate_structure(.content_workflow(n))$errors,`[[`,character(1),"code"))
  n$config$text <- '{"idade":5}'; n$config$output_schema <- list(type="object",properties=list(idade=list(type="string")))
  expect_false(gen_validate_structure(.content_workflow(n))$valid)
})

test_that("missing configuration is authoring requirement, not a provider call", {
  n <- .content_node("todo",source="ai",ai=structure(list(),names=character()))
  report <- gen_validate_structure(.content_workflow(n),purpose="authoring")
  expect_true(report$valid,info=toString(report$errors)); expect_false(report$ready)
  expect_true(length(report$requirements)>=3)
  expect_false(gen_validate_structure(.content_workflow(n))$valid)
})

test_that("adendos consume explicit bindings and enforce their role", {
  image <- .content_node("image",kind="image",source="asset",asset_id="img")
  text <- .content_node("read",source="ai",ai=.content_ai(),additions=list(list(role="context_image",value=list(input="foto"))))
  text$inputs <- list(foto=list(node="image",port="asset"))
  bundle <- list(format="genflow-studio-definition",schema_version=1L,root=list(kind="workflow",name="OCR",body=.content_workflow(image,text)),
    assets=list(list(key="img",kind="image",sha256=paste(rep("a",64),collapse=""))))
  report <- gen_validate_structure(bundle)
  expect_true(report$valid,info=toString(report$errors))
  expect_identical(report$ports$read$asset,"asset:text")
  text$config$additions <- list()
  expect_true("unused_input" %in% vapply(gen_block_ports("content",text$config,list(foto="asset:image"))$errors,`[[`,character(1),"code"))
  text$config$additions <- list(list(role="reference_audio",value=list(input="foto")))
  expect_true("content_role" %in% vapply(gen_validate_call("content",text$config,check_capabilities=FALSE)$errors,`[[`,character(1),"code"))
})

test_that("bindings are data and only declared asset locations are remapped", {
  n <- .content_node("read",source="ai",ai=.content_ai(),additions=list(list(role="context_image",value=list(asset="picture"))))
  n$config$ai$content <- list(list(text="picture"),list(asset="words"))
  body <- .content_workflow(n)
  refs <- gen_definition_references(body,"workflow")
  expect_setequal(vapply(refs,`[[`,character(1),"key"),c("picture","words"))
  mapped <- gen_remap_definition(body,"workflow",assets=list(picture="p2",words="w2"))
  expect_identical(mapped$nodes[[1]]$config$ai$content[[1]]$text,"picture")
  expect_identical(mapped$nodes[[1]]$config$ai$content[[2]]$asset,"w2")
  bad <- n$config; bad$ai$content <- list(list(text="x",input="a"))
  expect_false(gen_validate_call("content",bad,check_capabilities=FALSE)$valid)
})

test_that("join and list accept items and lists without a converter", {
  args <- list(parts=list(list(input="many"),list(asset="file"),list(text="literal")))
  expect_true(gen_validate_call("text.join",args,version=2)$valid)
  expect_true(gen_block_ports("text.join",args,list(many="list:text"),version=2)$valid)
  expect_false(gen_block_ports("text.join",args,list(many="list:image"),version=2)$valid)
  list_args <- list(order=list("a","b"))
  expect_identical(gen_block_ports("list",list_args,list(a="asset:text",b="list:text"))$ports$list,"list:text")
  expect_false(gen_block_ports("list",list_args,list(a="asset:text",b="list:image"))$valid)
  list_args$kind <- "image"
  expect_false(gen_block_ports("list",list_args,list(a="asset:text",b="list:text"))$valid)
})

test_that("judging consumes collections and counting requires ballot receipts", {
  judge <- list(ai=.content_ai())
  expect_true(gen_validate_call("judge",judge,2L,check_capabilities=FALSE)$valid)
  expect_identical(gen_block_ports("judge",judge,list(candidates="list:text"),2L)$ports$asset,"asset:ballot")
  expect_false(gen_block_ports("vote",list(),list(candidates="list:text",votes="list:json"),2L)$valid)
  expect_true(gen_block_ports("vote",list(),list(candidates="list:text",votes="list:ballot"),2L)$valid)
})

test_that("authoring preserves explicit absent profiles and rejects reserved judge bindings", {
  n <- .content_node("voice",kind="voice",source="ai",ai=list(service="local-native",model="voxcpm2-q8_0.gguf",
    contract_version=4L,parameters=list(parameter_schema=NULL),content=list(list(text="Olá"))))
  r <- gen_validate_structure(.content_workflow(n),check_capabilities=FALSE,purpose="authoring")
  expect_true(r$valid)
  params <- r$document$nodes[[1]]$config$ai$parameters
  expect_true("parameter_schema" %in% names(params));expect_null(params$parameter_schema)
  expect_identical(gen_validate_structure(r$document,check_capabilities=FALSE,purpose="authoring")$document,r$document)
  a <- .content_ai();a$content <- list(list(input="candidates"))
  expect_false(gen_block_ports("judge",list(ai=a),list(candidates="list:text"),2)$valid)
})

test_that("every declared schema default matches its own value contract", {
  walk <- function(schema,path) {
    if("default" %in% names(schema)) expect_equal(length(.gen_schema_errors(schema$default,schema,path)),0L,info=path)
    for(key in names(schema$properties)) walk(schema$properties[[key]],paste0(path,"/",key))
    if(is.list(schema$items)) walk(schema$items,paste0(path,"/items"))
  }
  for(contract in gen_block_contracts()) walk(contract$arguments,paste0(contract$id,"@",contract$version))
})
