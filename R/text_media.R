# Ordered image input for text models; no transport may silently discard it.
.gen_text_image_uris <- function(images, timeout = 80) {
  if (is.null(images) || !length(images)) return(list())
  budget <- .gen_image_budget(timeout)
  lapply(seq_along(images), function(i) tryCatch({
    value <- images[[i]]
    media <- .gen_image_inspect(.gen_image_bytes(.gen_image_source(value), budget(), allow_file=TRUE), budget())
    paste0("data:", media$metadata$mime, ";base64,", openssl::base64_encode(media$bytes))
  }, error=function(e) .gen_stop(gen_diagnostic(e, stage="preflight", outcome="not_started",
    path=paste0("/args/add_img/", i-1L)))))
}

.gen_text_image_parts <- function(images, format = "chat") {
  # Runtime sends normalized data URIs. The internal adapters also accept paths
  # for direct transport tests; MIME is always read from bytes, never extension.
  if (is.null(images) || !length(images)) return(list())
  if (!all(vapply(as.list(images),function(x) .gen_scalar(x) && grepl("^data:image/",x),logical(1))))
    images <- .gen_text_image_uris(images)
  lapply(images, function(uri) {
    mime <- sub("^data:([^;]+);.*$", "\\1", uri)
    data <- sub("^[^,]+,", "", uri)
    switch(format,
      chat=list(type="image_url", image_url=list(url=uri)),
      responses=list(type="input_image", image_url=uri),
      gemini=list(inline_data=list(mime_type=mime, data=data)),
      anthropic=list(type="image", source=list(type="base64", media_type=mime, data=data)),
      ollama=data)
  })
}

.gen_text_user_content <- function(prompt, images) {
  if (!length(images)) prompt else c(list(list(type="text", text=prompt)), .gen_text_image_parts(images))
}
