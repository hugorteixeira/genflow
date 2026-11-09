# One image boundary for every adapter. Provider envelopes are decoded by the
# adapter; this module only handles explicitly identified media sources.
.gen_image_limit <- 50 * 1024^2

.gen_image_tools <- function() {
  if (any(!nzchar(Sys.which(c("ffmpeg", "ffprobe")))))
    .gen_stop("image_decoder_missing", stage="preflight", outcome="not_started")
}

.gen_image_budget <- function(seconds, clock = Sys.time) {
  end <- clock() + seconds
  function() {
    left <- as.numeric(difftime(end, clock(), units="secs"))
    if (!is.finite(left) || left <= 0) .gen_stop("DEADLINE_EXCEEDED", outcome="unknown")
    min(left, 300)
  }
}

.gen_image_download <- function(url, timeout) {
  if (!.gen_scalar(url) || !grepl("^https?://", url, ignore.case=TRUE)) .gen_stop("image_source_invalid")
  path <- tempfile("genflow-download-"); on.exit(unlink(path), add=TRUE)
  response <- tryCatch(httr::GET(url, httr::write_disk(path), httr::timeout(timeout),
    httr::config(maxfilesize=.gen_image_limit, protocols=3L, redir_protocols=3L)),
    error=function(e) .gen_stop("image_download_failed", stage="download", retryable=TRUE))
  .gen_provider_check(response,expected=200L,stage="download")
  .gen_image_read_file(path)
}

.gen_image_read_file <- function(path) {
  info <- file.info(path)
  if (is.na(info$size) || isTRUE(info$isdir) || info$size <= 0 || info$size > .gen_image_limit)
    .gen_stop("image_source_invalid")
  readBin(path, "raw", n=.gen_image_limit + 1)
}

.gen_image_base64 <- function(value) {
  if (!.gen_scalar(value) || nchar(value, type="bytes") > 4 * ceiling(.gen_image_limit / 3) ||
      !grepl("^[A-Za-z0-9+/]*={0,2}$", value) || nchar(value) %% 4 != 0)
    .gen_stop("image_source_invalid")
  tryCatch(openssl::base64_decode(value), error=function(e) .gen_stop("image_source_invalid"))
}

.gen_image_source <- function(value) {
  if (is.raw(value)) return(list(kind="base64", value=openssl::base64_encode(value)))
  if (is.list(value)) {
    if (!is.null(value$b64_json)) return(list(kind="base64", value=value$b64_json))
    value <- value$url %||% value$uri
  }
  list(kind="locator", value=value)
}

.gen_image_bytes <- function(source, timeout=300, download=.gen_image_download, allow_file=FALSE) {
  if (!is.list(source)) .gen_stop("image_source_invalid")
  if (identical(source$kind, "base64")) bytes <- .gen_image_base64(source$value) else {
    value <- source$value
    if (!identical(source$kind, "locator") || !.gen_scalar(value) || !nzchar(value)) .gen_stop("image_source_invalid")
    bytes <- if (grepl("^https?://", value, ignore.case=TRUE)) download(value, timeout) else
      if (grepl("^data:[A-Za-z0-9.+-]+/[A-Za-z0-9.+-]+;base64,", value)) .gen_image_base64(sub("^[^,]+,", "", value)) else
      if (allow_file && file.exists(path.expand(value))) .gen_image_read_file(path.expand(value)) else .gen_stop("image_source_invalid")
  }
  if (!is.raw(bytes) || !length(bytes) || length(bytes) > .gen_image_limit) .gen_stop("image_source_invalid")
  bytes
}

.gen_image_signature <- function(bytes) {
  starts <- function(signature) length(bytes) >= length(signature) && identical(bytes[seq_along(signature)], as.raw(signature))
  if (starts(c(137,80,78,71,13,10,26,10))) return("png")
  if (starts(c(255,216,255))) return("jpeg")
  if (length(bytes) >= 12 && starts(charToRaw("RIFF")) && identical(bytes[9:12], charToRaw("WEBP"))) return("webp")
  if (starts(c(80,75,3,4))) return("zip")
  .gen_stop("image_invalid", stage="output_validation")
}

.gen_image_unzip <- function(bytes) {
  path <- tempfile(fileext=".zip"); on.exit(unlink(path), add=TRUE); writeBin(bytes, path)
  tryCatch(withCallingHandlers({
    entries <- utils::unzip(path, list=TRUE)
    names <- entries$Name
    if (!nrow(entries) || nrow(entries) > 100L || anyNA(entries$Length) ||
        any(entries$Length < 0) || sum(entries$Length) > .gen_image_limit ||
        any(grepl("(^/|^[A-Za-z]:|\\\\|(^|/)\\.\\.(/|$))", names)) || anyDuplicated(names))
      .gen_stop("image_archive_invalid")
    selected <- which(!grepl("/$|(^|/)__MACOSX/|(^|/)\\.DS_Store$", names))
    if (length(selected) != 1L) .gen_stop("image_output_count")
    # Stream one member; never extract provider-controlled paths or symlinks.
    con <- unz(path, names[[selected]], open="rb"); on.exit(close(con), add=TRUE)
    value <- readBin(con, "raw", n=.gen_image_limit + 1)
    if (length(value) != entries$Length[[selected]] || length(value) > .gen_image_limit ||
        identical(.gen_image_signature(value), "zip")) .gen_stop("image_archive_invalid")
    value
  },warning=function(w) .gen_stop("image_archive_invalid")), error=function(e) {
    if (inherits(e,"genflow_error")) stop(e)
    .gen_stop("image_archive_invalid")
  })
}

.gen_image_inspect <- function(bytes, timeout=30) {
  .gen_image_tools()
  budget <- .gen_image_budget(timeout)
  format <- .gen_image_signature(bytes)
  if (format == "zip") { bytes <- .gen_image_unzip(bytes); format <- .gen_image_signature(bytes) }
  extension <- if (format == "jpeg") "jpg" else format
  path <- tempfile("genflow-inspect-", fileext=paste0(".", extension))
  on.exit(unlink(path), add=TRUE); writeBin(bytes, path)
  probe <- tryCatch(processx::run("ffprobe", c("-v","error","-show_entries",
    "stream=codec_name,width,height,nb_frames", "-of","json",path),
    timeout=min(budget(),30)*1000, error_on_status=FALSE), error=function(e) NULL)
  streams <- if (!is.null(probe) && probe$status == 0) tryCatch(jsonlite::fromJSON(probe$stdout,
    simplifyVector=FALSE)$streams, error=function(e) NULL) else NULL
  s <- if (length(streams) == 1L) streams[[1L]] else NULL
  codec <- switch(format, png="png", jpeg="mjpeg", webp="webp")
  if (is.null(s) || !identical(s$codec_name,codec) || is.null(s$width) || is.null(s$height) ||
      s$width <= 0 || s$height <= 0 || s$width * s$height > 64e6 ||
      (!is.null(s$nb_frames) && !identical(s$nb_frames,"N/A") && as.numeric(s$nb_frames) != 1))
    .gen_stop("image_invalid", stage="output_validation")
  decoded <- tryCatch(processx::run("ffmpeg", c("-v","error","-xerror","-threads","1",
    "-i",path,"-frames:v","1","-f","null","-"), timeout=min(budget(),30)*1000,
    error_on_status=FALSE), error=function(e) NULL)
  if (is.null(decoded) || decoded$status != 0L || nzchar(decoded$stderr))
    .gen_stop("image_invalid", stage="output_validation")
  list(bytes=bytes, metadata=list(format=format, extension=extension, mime=paste0("image/",format),
    width=s$width, height=s$height))
}

.gen_image_references <- function(add_img, service, model, budget) {
  if (is.null(add_img) || !length(add_img)) return(list())
  spec <- .gen_image_reference_spec(service, model)
  if (!isTRUE(spec$supported)) .gen_stop("image_references_unsupported", path="/args/add_img", stage="preflight")
  if (length(add_img) > spec$max_images) .gen_stop("image_reference_limit", params=list(limit=spec$max_images), path="/args/add_img", stage="preflight")
  lapply(seq_along(add_img), function(i) tryCatch({
    image <- .gen_image_inspect(.gen_image_bytes(.gen_image_source(add_img[[i]]), budget(), allow_file=TRUE), budget())
    uri <- paste0("data:",image$metadata$mime,";base64,",openssl::base64_encode(image$bytes))
    if (nchar(uri,type="bytes") > spec$max_data_uri) .gen_stop("image_reference_size")
    uri
  }, error=function(e) .gen_stop(gen_diagnostic(e, stage="preflight", outcome="not_started",
    path=paste0("/args/add_img/",i-1L)))))
}

.gen_image_publish <- function(receipt, directory, label, budget, download=.gen_image_download, resolve=.gen_img_resolve_image_receipt) {
  tryCatch({
    receipt <- resolve(receipt,budget)
    if (length(receipt$sources) != 1L) .gen_stop("image_output_count", stage="output_validation")
    bytes <- .gen_image_bytes(receipt$sources[[1L]], budget(), download)
    # Preserve actual bytes even if decoding/publication fails or the URL expires.
    receipt$sources <- list(.gen_image_source(bytes))
    image <- .gen_image_inspect(bytes, budget())
    path <- .genflow_write_artifact(image$bytes, directory, label, receipt$service, receipt$model, image$metadata$extension)
    attr(path,"image_metadata") <- c(image$metadata, list(requested_size=receipt$requested_size, sent_size=receipt$sent_size))
    path
  }, error=function(e) {
    e$image_receipt <- receipt
    if (is.null(e$diagnostic)) e$diagnostic <- gen_diagnostic("image_invalid", stage="output_validation")
    e$diagnostic$outcome <- "succeeded"
    if (!e$diagnostic$stage %in% c("download","persistence")) e$diagnostic$stage <- "output_validation"
    if (e$diagnostic$category == "validation") e$diagnostic$category <- "provider"
    class(e) <- unique(c("genflow_image_output_error",class(e)))
    stop(e)
  })
}

.gen_image_receipt <- function(sources, service, model, h, y, sent_size=NULL) {
  list(version=1L, sources=sources, service=service, model=model,
    requested_size=list(width=y,height=h), sent_size=sent_size)
}

#' Recover an image after generation, without submitting another inference
#' @param result A gen_img result containing an image_receipt.
#' @param directory Output directory, as in gen_img.
#' @param persist Whether to publish in the output directory.
#' @param timeout Maximum recovery time in seconds.
#' @return The normalized generation result. Only media download, validation and
#'   publication are retried. Invalid image content remains an error.
#' @export
gen_img_recover <- function(result, directory=NULL, persist=TRUE, timeout=300) {
  withCallingHandlers({
    .genflow_validate_flag(persist,"persist")
    if (!is.list(result) || !identical(result$inference_status,"SUCCESS") ||
        !is.list(result$image_receipt) || !isTRUE(result$image_receipt$version == 1L) ||
        !is.numeric(timeout) || length(timeout)!=1L || !is.finite(timeout) || timeout<=0)
      .gen_stop("image_receipt_invalid", stage="preflight")
    .gen_image_tools()
    receipt <- result$image_receipt
    .gen_img_result(function() .gen_image_publish(receipt, .genflow_media_directory(directory,"imgs",persist),
      result$label_cat %||% "recovered", .gen_image_budget(timeout)), result, persist)
  }, error=.gen_boundary_error)
}
