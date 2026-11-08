# Regressions from the six-point boundary audit. No provider calls.
boundary_json <- function(x) as.character(jsonlite::toJSON(x,auto_unbox=TRUE,null="null",digits=NA))
boundary_node <- function() list(id="text",type="content",config=list(content_type="text",source="ai",
  ai=list(service="openai",model="gpt-5-mini",parameters=structure(list(),names=character()),content=list(list(text="Olá")))),inputs=structure(list(),names=character()))
boundary_check <- function(x) gen_validate_structure(x,purpose="authoring",check_capabilities=FALSE)

test_that("malformed workflow fields always yield diagnostics rather than exceptions", {
  variants <- list(null=NULL,array=list(),object=structure(list(),names=character()),string="bad",number=1,bool=TRUE,list_scalar=list("bad"),list_object=list(list(bad=1)))
  for (field in c("config","inputs","id","type","position","ui","contract_version","ai","parameters","content","additions","addition_value","role","content_type","source","format","output_schema")) for (key in names(variants)) {
    n <- boundary_node(); v <- variants[key]
    if (field %in% c("config","inputs","id","type","position","ui","contract_version")) n[field] <- v
    else if(field %in% c("parameters","content")) n$config$ai[field] <- v
    else if(field %in% c("addition_value","role")) {
      n$config$additions <- list(list(role="context_text",value=list(text="Contexto")))
      n$config$additions[[1]][if(field=="role") "role" else "value"] <- v
    } else n$config[field] <- v
    r <- tryCatch(boundary_check(boundary_json(list(schema_version=1L,nodes=list(n)))),error=identity)
    expect_false(inherits(r,"error"),info=paste(field,key))
    if (!inherits(r,"error")) {
      expect_type(r$valid,"logical")
      if(!r$valid) expect_gt(length(r$errors),0)
    }
  }
  missing <- boundary_node(); missing$id <- NULL
  expect_false(boundary_check(list(schema_version=1L,nodes=list(missing)))$valid)
})

test_that("invalid independent nodes accumulate errors and retain useful graph metadata", {
  missing <- boundary_node(); missing["id"] <- list(NULL)
  bad <- boundary_node(); bad$id <- "bad"; bad$config$ai <- "bad"
  good <- boundary_node(); good$id <- "good"
  r <- boundary_check(boundary_json(list(schema_version=1L,nodes=list(missing,bad,good))))
  expect_false(r$valid)
  paths <- vapply(r$errors,`[[`,character(1),"path")
  expect_true(any(grepl("/nodes/0/id$",paths)))
  expect_true(any(grepl("/nodes/1/config/ai$",paths)))
  expect_equal(r$graph$ports$good$asset,"asset:text")
})

test_that("prepared native R objects survive JSON without changing authored omissions", {
  for (field in c("inputs","ui","parameters")) for (fn in c("gen_validate_structure","gen_materialize_structure","gen_extract_structure")) {
    n <- boundary_node()
    if(field=="parameters") n$config$ai$parameters <- list() else n[field] <- list(list())
    d <- list(schema_version=1L,nodes=list(n)); original <- serialize(d,NULL)
    r <- get(fn)(d,purpose="authoring",check_capabilities=FALSE)
    expect_true(r$valid,info=paste(fn,field))
    again <- boundary_check(boundary_json(r$document))
    expect_true(again$valid,info=paste(fn,field,boundary_json(again$errors)))
    expect_identical(serialize(d,NULL),original)
    expect_identical(r$document$nodes[[1]]$config$ai$content,n$config$ai$content)
    if(fn!="gen_validate_structure") {
      expect_false("format" %in% names(r$document$nodes[[1]]$config))
      expect_false("temperature" %in% names(r$document$nodes[[1]]$config$ai$parameters))
    }
    explicit <- gen_parse_json(boundary_json(d))
    expect_false(boundary_check(explicit)$valid,info=paste("explicit array",field))
  }
})

test_that("transcription rejects unused textual content but accepts the audio binding", {
  args <- list(audio=list(input="audio"),ai=list(service="openai",model="whisper-1"))
  expect_true(gen_validate_call("transcribe",args,check_capabilities=FALSE)$valid)
  args$ai$content <- list(list(text="Traduza tudo para árabe"))
  r <- gen_validate_call("transcribe",args,check_capabilities=FALSE)
  expect_false(r$valid)
  expect_true(any(vapply(r$errors,function(e) identical(e$path,"/args/ai/content"),logical(1))))
})
