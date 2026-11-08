test_that("text images preserve order and detected MIME across transports", {
  a <- tempfile(fileext=".jpg"); b <- tempfile(fileext=".png")
  on.exit(unlink(c(a,b)))
  writeBin(.image_fixture("png"),a); writeBin(.image_fixture("jpeg"),b)
  images <- genflow:::.gen_text_image_uris(c(a,b))
  expect_match(images[[1]],"^data:image/png;"); expect_match(images[[2]],"^data:image/jpeg;")
  for (format in c("chat","responses","gemini","anthropic","ollama")) {
    parts <- genflow:::.gen_text_image_parts(images,format)
    expect_length(parts,2)
    if (format=="responses") expect_identical(parts[[2]]$image_url,images[[2]])
    if (format=="gemini") expect_identical(parts[[1]]$inline_data$mime_type,"image/png")
    if (format=="anthropic") expect_identical(parts[[2]]$source$media_type,"image/jpeg")
    if (format=="ollama") expect_identical(parts[[1]],sub("^[^,]+,","",images[[1]]))
  }
  expect_error(genflow:::.gen_text_image_uris("/not/an/image"),class="genflow_error")
})

test_that("Responses and chat send every image instead of discarding it", {
  withr::local_envvar(OPENAI_API_KEY="fixture-only")
  images <- c(paste0("data:image/png;base64,",openssl::base64_encode(.image_fixture())),
              paste0("data:image/jpeg;base64,",openssl::base64_encode(.image_fixture("jpeg"))))
  seen <- NULL
  testthat::local_mocked_bindings(POST=function(url, ..., body) { seen <<- list(url=url,body=jsonlite::fromJSON(body,simplifyVector=FALSE)); structure(list(status_code=200L,headers=list(`content-type`="application/json"),content=charToRaw('{"choices":[{"message":{"content":"ok"}}],"output":[]}')),class="response") },
    http_status=function(...) list(category="Success"),content=function(...) list(output=list(),choices=list(list(message=list(content="ok")))), .package="httr")
  for (reason in list(NULL,"low")) {
    genflow:::.gen_txt_openai("Read",model="gpt-5-mini",temp_v=1,reasoning=reason,add_img=images)
    parts <- if(is.null(reason)) seen$body$messages[[1]]$content else seen$body$input[[1]]$content
    expect_length(parts,3)
    expect_identical(parts[[2]]$type,if(is.null(reason)) "image_url" else "input_image")
    uri <- if(is.null(reason)) parts[[3]]$image_url$url else parts[[3]]$image_url
    expect_identical(uri,images[[2]])
  }
})

test_that("text validation rejects unsupported vision before inference", {
  expect_true(gen_validate_call("gen_txt",list(context="x",add_img=list("a","b")),2)$valid)
  testthat::local_mocked_bindings(gen_capabilities=function(...) list(modalities=c(text=TRUE),adapter_features=c(vision=FALSE)),.package="genflow")
  report <- gen_validate_call("gen_txt",list(context="x",service="openai",add_img="a"),2)
  expect_false(report$valid)
  expect_true("image_not_supported" %in% vapply(report$errors,`[[`,character(1),"code"))
})
