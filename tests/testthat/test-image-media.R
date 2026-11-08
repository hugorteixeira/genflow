test_that("the current image contract validates ordered references and rejects retired contracts", {
  args <- list(prompt="Garden",service="openai",model="gpt-image-2",add_img=list("one.png","two.png"))
  expect_false(gen_validate_call("gen_img",args,1)$valid)
  expect_true(gen_validate_call("gen_img", args, 3)$valid)
  expect_identical(gen_block_contracts("gen_img", 3)$inputs$add_img,"asset:image|list:image")
  expect_true(gen_validate_call("gen_img", list(prompt="Garden"), 3)$valid)
  for (value in list(NA_character_, " ", list("ok",2), list("ok",""))) {
    args$add_img <- value
    expect_false(gen_validate_call("gen_img", args, 3)$valid)
  }
  args$add_img <- as.list(rep("image.png",17))
  expect_identical(gen_validate_call("gen_img", args, 3)$errors[[1]]$code,"image_reference_limit")
  args$add_img <- "image.png"; args$service <- "replicate"
  expect_identical(gen_validate_call("gen_img", args, 3)$errors[[1]]$code,"image_references_unsupported")
  expect_false(gen_validate_call("gen_img", list(prompt="ok",replicate_input=list(prompt="")), 3)$valid)
  expect_true(gen_validate_call("gen_img", list(prompt="ok",service="replicate",replicate_input=list(seed=42)), 3)$valid)
})

test_that("OpenAI edits preserve reference order and accept all supported output encodings", {
  withr::local_envvar(c(OPENAI_API_KEY="test",genflow_SKIP_PERSIST_LOG="1"))
  files <- vapply(c("png","jpeg"),function(f) { p<-tempfile(); writeBin(.image_fixture(f),p); p },character(1))
  withr::defer(unlink(files))
  calls <- list(); payload <- NULL
  local_mocked_bindings(.gen_img_openai_request=function(method,url,api_key=NULL,body=NULL,timeout_secs=300) {
    calls[[length(calls)+1L]] <<- list(method=method,url=url,body=body)
    if (method=="POST") return(list(status=200L,text=jsonlite::toJSON(list(data=list(payload)),auto_unbox=TRUE)))
    list(status=200L,raw=.image_fixture("jpeg"))
  },.package="genflow")
  for (format in c("png","jpeg","webp")) {
    payload <- list(b64_json=openssl::base64_encode(.image_fixture(format)))
    result <- gen_img("one then two",service="openai",add_img=unname(files),persist=FALSE)
    expect_identical(result$status_api,"SUCCESS")
    expect_identical(result$image$format,format)
    expect_identical(result$dimensoes,"32x24")
    expect_equal(result$image$width,32)
    expect_identical(result$image$sent_size,"1536x1024")
    expect_identical(readBin(result$response_value,"raw",1e6),.image_fixture(format))
    unlink(result$response_value)
  }
  expect_true(all(vapply(calls,function(x) endsWith(x$url,"/edits"),logical(1))))
  expect_match(calls[[1]]$body$images[[1]]$image_url,"^data:image/png;base64,")
  expect_match(calls[[1]]$body$images[[2]]$image_url,"^data:image/jpeg;base64,")
  payload <- list(url="https://example.invalid/picture.png")
  result <- gen_img("no references",service="openai",persist=FALSE)
  expect_match(calls[[length(calls)-1L]]$url,"/generations$")
  expect_match(result$response_value,"[.]jpg$")
  unlink(result$response_value)
})

test_that("bad references and unsupported models fail before any paid request", {
  local_mocked_bindings(.gen_img_openai_request=function(...) stop("PAID REQUEST"),.package="genflow")
  invalid <- tempfile(); writeLines("<html>error</html>",invalid); withr::defer(unlink(invalid))
  for (ref in c(invalid,"missing.png","data:image/png;base64,@@@")) {
    e <- tryCatch(gen_img("scene",service="openai",add_img=ref,persist=FALSE),error=identity)
    expect_s3_class(e,"genflow_error")
    expect_identical(e$diagnostic$stage,"preflight")
    expect_identical(e$diagnostic$outcome,"not_started")
    expect_false(grepl("PAID REQUEST",conditionMessage(e)))
  }
  expect_error(gen_img("scene",service="hf",add_img=invalid,persist=FALSE),class="genflow_validation_error")
})

test_that("bad provider output is never successful media and remains inspectable", {
  withr::local_envvar(c(OPENAI_API_KEY="test"))
  bytes <- raw(); entries <- 1L
  local_mocked_bindings(.gen_img_openai_request=function(method,...) {
    if (method=="POST") return(list(status=200L,text=jsonlite::toJSON(list(data=rep(list(list(b64_json=openssl::base64_encode(bytes))),entries)),auto_unbox=TRUE)))
    stop("unexpected download")
  },.package="genflow")
  for (bad in list(charToRaw("<html>Error</html>"),.image_fixture()[1:8],raw())) {
    bytes <- bad
    result <- gen_img("scene",service="openai",persist=FALSE)
    expect_identical(result$status_api,"ERROR")
    expect_identical(result$inference_status,"SUCCESS")
    expect_null(result$response_value)
    expect_true(is.list(result$image_receipt))
    expect_identical(result$error$outcome,"succeeded")
    expect_false(result$error$retryable)
    expect_identical(gen_img_recover(result,persist=FALSE)$status_api,"ERROR")
  }
  bytes <- .image_fixture(); entries <- 2L
  result <- gen_img("scene",service="openai",persist=FALSE)
  expect_identical(result$error$code,"image_output_count")
  expect_length(result$image_receipt$sources,2)
})

test_that("download and publication recovery do not resubmit image generation", {
  withr::local_envvar(c(OPENAI_API_KEY="test"))
  posts <- 0L
  local_mocked_bindings(.gen_img_openai_request=function(method,...) {
    if (method=="POST") { posts <<- posts+1L; return(list(status=200L,text='{"data":[{"url":"https://example.invalid/image"}]}')) }
    list(status=503L)
  }, .gen_image_download=function(url,timeout) .image_fixture(), .package="genflow")
  result <- gen_img("scene",service="openai",persist=FALSE)
  expect_identical(result$status_api,"ERROR")
  expect_true(result$error$retryable)
  restored <- gen_img_recover(result,persist=FALSE)
  expect_identical(restored$status_api,"SUCCESS")
  expect_identical(posts,1L)
  expect_true(file.exists(restored$response_value)); unlink(restored$response_value)
  local_mocked_bindings(.genflow_publish_artifact=function(...) stop("disk full"),.package="genflow")
  failed <- gen_img_recover(result,persist=FALSE)
  expect_identical(failed$inference_status,"SUCCESS")
  expect_identical(failed$persistence$status,"failed")
  expect_true(file.exists(failed$response_value)); unlink(failed$response_value)
  expect_identical(posts,1L)
})

test_that("FAL references use the same entrypoint and handle data URI output", {
  withr::local_envvar(c(FAL_API_KEY="test"))
  ref <- paste0("data:image/png;base64,",openssl::base64_encode(.image_fixture()))
  submitted <- NULL
  local_mocked_bindings(.gen_img_fal_request=function(method,url,token=NULL,body=NULL,timeout_secs=300) {
    if (method=="POST") { submitted <<- body; return(list(status=202L,content=list(status="COMPLETED",status_url="https://queue.fal.run/status",response_url="https://queue.fal.run/result"))) }
    if (method=="RESULT") return(list(status=200L,content=list(images=list(list(url=ref)))))
    stop("unexpected download or poll")
  },.package="genflow")
  result <- gen_img("scene",service="fal",model="fal-ai/flux-2/edit",add_img=c(ref,ref),persist=FALSE)
  expect_identical(result$status_api,"SUCCESS")
  expect_identical(submitted$image_urls,list(ref,ref))
  expect_identical(submitted$num_images,1L)
  unlink(result$response_value)
  expect_error(gen_img("scene",service="fal",model="fal-ai/flux-2/edit",add_img=rep(ref,5),persist=FALSE),class="genflow_validation_error")
})

test_that("ZIP is a bounded transport containing exactly one decodable image", {
  skip_if_not(nzchar(Sys.which("zip")))
  dir <- withr::local_tempdir(); withr::local_dir(dir)
  writeBin(.image_fixture(),"one.png"); utils::zip("one.zip","one.png",flags="-q")
  read <- function(p) readBin(p,"raw",1e6)
  checked <- genflow:::.gen_image_inspect(read("one.zip"))
  expect_identical(checked$bytes,.image_fixture())
  uri <- paste0("data:application/zip;base64,",openssl::base64_encode(read("one.zip")))
  expect_identical(genflow:::.gen_image_inspect(genflow:::.gen_image_bytes(genflow:::.gen_image_source(uri)))$bytes,.image_fixture())
  expect_error(genflow:::.gen_image_inspect(charToRaw("PK\003\004broken")),class="genflow_error")
  writeBin(.image_fixture(),"two.png"); utils::zip("two.zip",c("one.png","two.png"),flags="-q")
  expect_error(genflow:::.gen_image_inspect(read("two.zip")),class="genflow_error")
  utils::zip("nested.zip","one.zip",flags="-q")
  expect_error(genflow:::.gen_image_inspect(read("nested.zip")),class="genflow_error")
})

test_that("missing image tools fail during preflight", {
  withr::local_envvar(c(PATH=withr::local_tempdir()))
  error <- tryCatch(gen_img("scene",service="openai",persist=FALSE),error=identity)
  expect_s3_class(error,"genflow_error")
  expect_identical(error$diagnostic$code,"image_decoder_missing")
  expect_identical(error$diagnostic$outcome,"not_started")
})

test_that("HF bytes use the same validation and report rounded request dimensions", {
  withr::local_envvar(c(HUGGINGFACE_API_TOKEN="test"))
  bytes <- .image_fixture("jpeg"); submitted <- NULL
  local_mocked_bindings(POST=function(url,...,body=NULL) {
    submitted <<- body
    structure(list(status_code=200L,headers=list(`content-type`="image/png"),content=bytes,url=url),class="response")
  },.package="httr")
  result <- gen_img("scene",service="hf",h=1,y=1,persist=FALSE)
  expect_identical(result$status_api,"SUCCESS")
  expect_identical(result$image$format,"jpeg")
  expect_equal(result$image$sent_size,list(width=8,height=8))
  expect_equal(submitted$parameters$height,8)
  unlink(result$response_value)
  bytes <- raw()
  failed <- gen_img("scene",service="hf",persist=FALSE)
  expect_identical(failed$status_api,"ERROR")
  expect_identical(failed$inference_status,"SUCCESS")
  expect_identical(failed$error$stage,"output_validation")
  expect_identical(failed$error$category,"provider")
})

test_that("image agents and batches forward common and per-task reference lists", {
  withr::local_envvar(c(OPENAI_API_KEY="test"))
  ref <- paste0("data:image/png;base64,",openssl::base64_encode(.image_fixture()))
  counts <- integer()
  local_mocked_bindings(.gen_img_openai_request=function(method,url,api_key=NULL,body=NULL,timeout_secs=300) {
    counts <<- c(counts,length(body$images))
    list(status=200L,text=jsonlite::toJSON(list(data=list(list(b64_json=openssl::base64_encode(.image_fixture())))),auto_unbox=TRUE))
  },.package="genflow")
  agent <- set_agent("scene",setup=list(service="openai",model="gpt-image-2",type="Image"),
    content=list(context="scene"),save=FALSE,assign=FALSE)
  direct <- gen_img(agent,add_img=c(ref,ref),persist=FALSE)
  batch <- gen_batch_agent(agent,qty=2,workers=1,add_img_each=list(ref,c(ref,ref)),persist=FALSE,verbose=FALSE,always_fix_errors=FALSE)
  expect_identical(direct$status_api,"SUCCESS")
  expect_identical(counts,c(2L,1L,2L))
  expect_true(all(vapply(batch[1:2],function(x) identical(x$status_api,"SUCCESS"),logical(1))))
  unlink(c(direct$response_value,vapply(batch[1:2],`[[`,character(1),"response_value")))
})

test_that("automatic batch repair retains a completed image instead of paying again", {
  withr::local_envvar(c(OPENAI_API_KEY="test"))
  posts <- 0L
  local_mocked_bindings(.gen_img_openai_request=function(method,...) {
    posts <<- posts+1L
    list(status=200L,text='{"data":[{"b64_json":"PGh0bWw+"}]}')
  },.package="genflow")
  agent <- set_agent(paste0("receipt-",tempfile()),setup=list(service="openai",model="gpt-image-2",type="Image"),
    content=list(context=paste0("unique-scene-",tempfile())),save=FALSE,assign=FALSE)
  run <- function() gen_batch_agent(agent,qty=1,workers=1,persist=FALSE,verbose=FALSE,always_fix_errors=TRUE)
  first <- run(); second <- run()
  expect_identical(first[[1]]$inference_status,"SUCCESS")
  expect_identical(second[[1]]$status_api,"ERROR")
  expect_identical(posts,1L)
  expect_identical(second[[1]]$image_receipt,first[[1]]$image_receipt)
})

test_that("FAL completed-result retrieval survives a failed GET without a new POST", {
  withr::local_envvar(c(FAL_API_KEY="test"))
  posts <- 0L; results <- 0L
  ref <- paste0("data:image/png;base64,",openssl::base64_encode(.image_fixture()))
  local_mocked_bindings(.gen_img_fal_request=function(method,...) {
    if (method=="POST") { posts <<- posts+1L; return(list(status=202L,content=list(status="COMPLETED",status_url="https://queue.fal.run/status",response_url="https://queue.fal.run/result"))) }
    results <<- results+1L
    if (results==1L) return(list(status=503L))
    list(status=200L,content=list(images=list(list(url=ref))))
  },.package="genflow")
  failed <- gen_img("scene",service="fal",persist=FALSE)
  expect_identical(failed$inference_status,"SUCCESS")
  expect_identical(failed$image_receipt$result_url,"https://queue.fal.run/result")
  done <- gen_img_recover(failed,persist=FALSE)
  expect_identical(done$status_api,"SUCCESS")
  expect_identical(posts,1L)
  expect_identical(results,2L)
  unlink(done$response_value)
})

test_that("the real downloader follows HTTP redirects but trusts bytes, not MIME or extension", {
  skip_if_not(nzchar(Sys.which("python3")))
  root <- withr::local_tempdir()
  writeBin(.image_fixture("jpeg"),file.path(root,"image"))
  script <- file.path(root,"server.py"); port <- file.path(root,"port")
  writeLines(c(
    "import http.server, pathlib, sys",
    "root = pathlib.Path(sys.argv[1])",
    "class Handler(http.server.BaseHTTPRequestHandler):",
    "  def log_message(self,*args): pass",
    "  def do_GET(self):",
    "    if self.path == '/redirect':",
    "      self.send_response(302); self.send_header('Location','/wrong.png'); self.end_headers(); return",
    "    self.send_response(200)",
    "    self.send_header('Content-Type','image/png')",
    "    data = b'<html>error</html>' if self.path == '/html' else (root/'image').read_bytes()",
    "    self.send_header('Content-Length',str(100000000 if self.path == '/huge' else len(data)))",
    "    self.end_headers(); self.wfile.write(data)",
    "server = http.server.ThreadingHTTPServer(('127.0.0.1',0), Handler)",
    "(root/'port').write_text(str(server.server_port))",
    "server.serve_forever()"),script)
  server <- processx::process$new(Sys.which("python3"),c(script,root),cleanup=TRUE)
  withr::defer(server$kill_tree())
  deadline <- Sys.time()+5
  while (!file.exists(port) && server$is_alive() && Sys.time()<deadline) Sys.sleep(.01)
  expect_true(file.exists(port))
  base <- paste0("http://127.0.0.1:",readLines(port,warn=FALSE))
  bytes <- genflow:::.gen_image_download(paste0(base,"/redirect"),5)
  expect_identical(bytes,.image_fixture("jpeg"))
  expect_identical(genflow:::.gen_image_inspect(bytes)$metadata$format,"jpeg")
  expect_error(genflow:::.gen_image_inspect(genflow:::.gen_image_download(paste0(base,"/html"),5)),class="genflow_error")
  expect_error(genflow:::.gen_image_download(paste0(base,"/huge"),5),class="genflow_error")
  expect_error(genflow:::.gen_image_download("file:///etc/passwd",5),class="genflow_error")
})

test_that("archive traversal and expanded-size limits fail before extraction", {
  skip_if_not(nzchar(Sys.which("python3")))
  root <- withr::local_tempdir(); writeBin(.image_fixture(),file.path(root,"image"))
  script <- file.path(root,"zip.py")
  writeLines(c("import pathlib,sys,zipfile", "p=pathlib.Path(sys.argv[1])",
    "with zipfile.ZipFile(p/'traversal.zip','w') as z: z.writestr('../escaped.png',(p/'image').read_bytes())",
    "with zipfile.ZipFile(p/'expanded.zip','w',compression=zipfile.ZIP_DEFLATED) as z: z.writestr('image.png',b'x'*20000)"),script)
  processx::run(Sys.which("python3"),c(script,root))
  expect_error(genflow:::.gen_image_inspect(readBin(file.path(root,"traversal.zip"),"raw",1e6)),class="genflow_error")
  expect_false(file.exists(file.path(dirname(root),"escaped.png")))
  local_mocked_bindings(.gen_image_limit=1024,.package="genflow")
  expect_error(genflow:::.gen_image_inspect(readBin(file.path(root,"expanded.zip"),"raw",1e6)),class="genflow_error")
})
