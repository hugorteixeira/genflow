#' Generate an image via a selected provider
#'
#' High-level image generation wrapper that dispatches to provider-specific
#' implementations (Hugging Face, FAL, Replicate, and OpenAI) and saves the
#' resulting image to disk.
#'
#' @param prompt Character. Main text prompt describing the desired image.
#' @param add Optional character or object(s) to append to the prompt. If provided and
#'   character, it will be concatenated to `prompt`.
#' @param directory Optional output directory. Defaults to the configured
#'   Genflow output directory's `imgs` subdirectory.
#' @param label Optional short label to be used in filenames. If NULL, derived from prompt.
#' @param service Provider identifier: `"hf"`, `"fal"`, `"replicate"`, or
#'   `"openai"`.
#' @param model Provider model identifier. When NULL, a provider-specific
#'   default is selected.
#' @param temp Numeric guidance/temperature parameter (provider-specific meaning).
#' @param steps Integer inference steps (if supported by provider/model).
#' @param h Integer output height in pixels.
#' @param y Integer output width in pixels (named `y` here to match internal calls).
#' @param model_version Optional Replicate model version. It can also be
#'   supplied as part of `model` using `"owner/name:version"`.
#' @param replicate_input Optional named list of Replicate input overrides.
#'   Unknown Replicate models receive only `prompt` by default; use this
#'   argument for model-specific schema fields.
#' @param poll_interval Polling interval in seconds for asynchronous providers.
#' @param max_poll_seconds Total budget in seconds for reference preparation,
#'   provider requests, polling and image processing.
#' @param persist Logical; save to the output directory and record statistics.
#'   FALSE leaves the image in the R session's temporary directory, returns its
#'   path in `response_value`, and sets `saved_file` to NA.
#' @param add_img Optional image path, HTTP(S) URL, data URI, or ordered vector/list
#'   of these references. Support and limits depend on the selected model.
#' @param model_options Named object of model-specific options. Validated against
#'   `parameter_schema`; unsupported transports reject options before inference.
#' @param parameter_schema Portable snapshot from [gen_resolve_parameter_schema()].
#'   Nonempty options without a snapshot resolve the current registered schema.
#' @param ... Reserved for S3 methods.
#'
#' @return Invisibly returns a list with fields such as `response_value` (saved file path
#'   on success or NULL on error), `status_api`, `status_msg`, `service`, `model`, `temp`,
#'   `duration`, `saved_file`, and other metadata.
#'
#' @examples
#' # Minimal example (requires valid provider credentials)
#' # gen_img("A cute robot painting a landscape", service = "hf",
#' #         model = "black-forest-labs/FLUX.1-schnell", h = 1024, y = 1024)
#'
#' @export
gen_img <- function(prompt, ...) {
  if (missing(prompt)) return(gen_img.default(...))
  UseMethod("gen_img")
}

#' @rdname gen_img
#' @method gen_img default
#' @export
gen_img.default <- function(
  prompt,
  add = .gen_default("gen_img", "add", 3L),
  directory = .gen_default("gen_img", "directory", 3L),
  label = .gen_default("gen_img", "label", 3L),
  service = .gen_default("gen_img", "service", 3L),
  model = .gen_default("gen_img", "model", 3L),
  temp = .gen_default("gen_img", "temp", 3L),
  steps = .gen_default("gen_img", "steps", 3L),
  h = .gen_default("gen_img", "h", 3L),
  y = .gen_default("gen_img", "y", 3L),
  model_version = .gen_default("gen_img", "model_version", 3L),
  replicate_input = .gen_default("gen_img", "replicate_input", 3L),
  poll_interval = .gen_default("gen_img", "poll_interval", 3L),
  max_poll_seconds = .gen_default("gen_img", "max_poll_seconds", 3L),
  persist = .gen_default("gen_img", "persist", 3L),
  add_img = .gen_default("gen_img", "add_img", 3L),
  model_options = .gen_default("gen_img", "model_options", 3L),
  parameter_schema = .gen_default("gen_img", "parameter_schema", 3L),
  ...
) {
  withCallingHandlers({
    .gen_internal_validate("gen_img", version = 3L)
    if (length(model_options)) replicate_input <- model_options
    .gen_image_tools()
    budget <- .gen_image_budget(max_poll_seconds)
    references <- .gen_image_references(add_img,service,model,budget)
    directory <- .genflow_media_directory(directory,"imgs",persist)
    final_prompt <- .genflow_prepare_prompt(prompt,add)
    label_processed <- substr(label %||% paste(head(strsplit(final_prompt,"[[:space:]]+")[[1]],5),collapse="_"),1,36)
    common <- list(prompt=final_prompt,model=model,temp=temp,steps=steps,h=h,y=y,
      directory=directory,label_sanitized=.sanitize_filename(label_processed),budget=budget)
    if (service %in% c("openai","fal")) common$references <- references
    if (service %in% c("fal","replicate")) common <- c(common,list(poll_interval=poll_interval,max_poll_seconds=max_poll_seconds))
    if (service == "replicate") common <- c(common,list(model_version=model_version,replicate_input=replicate_input))
    result <- .gen_img_result(function() do.call(get(paste0(".gen_img_",service),envir=environment(gen_img.default)),common),
      list(label=label,label_cat=label_processed,service=service,model=model,temp=temp,steps=steps,
        prompt_usado=final_prompt,content_type="image"),persist)
    if (persist && Sys.getenv("genflow_SKIP_PERSIST_LOG","0") != "1") try(.persist_stats_row(list(
      label=label_processed,model=model,temp=temp,duration=result$duration,
      tks_envia=NA_real_,tks_recebe=NA_real_,status_api=result$status_api)),silent=TRUE)
    if (!is.null(parameter_schema)) result$parameters <- list(model_options = model_options, parameter_schema = parameter_schema)
    invisible(result)
  }, error=.gen_boundary_error)
}

.gen_img_result <- function(run, result, persist) {
  started <- Sys.time(); failure <- NULL
  path <- tryCatch(run(),error=function(e) { failure <<- e; NULL })
  meta <- attr(path,"image_metadata")
  # Validate even a third-party adapter returning an unannotated file.
  if (is.null(failure) && is.null(meta)) {
    meta <- tryCatch(.gen_image_inspect(.gen_image_read_file(path))$metadata,
      error=function(e) { failure <<- e; NULL })
  }
  result$status_api <- if (is.null(failure)) "SUCCESS" else "ERROR"
  result$status_msg <- if (is.null(failure)) "OK" else gen_error_summary(gen_diagnostic(failure))
  result$response_value <- if (is.null(failure)) as.character(path) else failure$recovery_file
  result$saved_file <- if (is.null(failure) && persist) as.character(path) else NA_character_
  result$duration <- as.numeric(difftime(Sys.time(),started,units="secs"))
  result$dimensoes <- if (!is.null(meta)) paste0(meta$width,"x",meta$height) else NA_character_
  result$image <- meta
  result$image_receipt <- failure$image_receipt
  persistence_error <- if (inherits(failure,"genflow_persistence_error")) failure else NULL
  result <- .genflow_complete_result(result,
    inference_status=if (is.null(failure) || inherits(failure,c("genflow_image_output_error","genflow_persistence_error"))) "SUCCESS" else "ERROR",
    persistence=.genflow_persistence(if (is.null(path)) NULL else as.character(path),requested=persist,error=persistence_error),
    error=if (is.null(failure)) NULL else .genflow_provider_failure(failure)$error)
  invisible(result)
}

#' Hugging Face image generation (internal)
#'
#' @keywords internal
#' @noRd
.gen_img_hf <- function(prompt, model = "black-forest-labs/FLUX.1-schnell", temp = NULL, steps = 18, h = NULL, y = NULL, directory = NULL, label_sanitized = NULL, budget = .gen_image_budget(600)) {
  service_name <- "hf"
  # Get HF token
  hf_token <- Sys.getenv("HUGGINGFACE_API_TOKEN")
  if (!nzchar(hf_token)) .gen_stop("missing_credential", params=list(credential="HUGGINGFACE_API_TOKEN"),stage="preflight")

  # Use 'model' directly as the model path
  model_path <- model

  # URL and Body
  url <- sprintf("https://router.huggingface.co/hf-inference/models/%s", model_path)
  hx <- max(8L, round(h / 8) * 8)
  wx <- max(8L, round(y / 8) * 8)

  body <- list(
    inputs = prompt,
    parameters = list(guidance_scale = temp, height = hx, width = wx, num_inference_steps = steps)
  )

  # Request
  response <- httr::POST(
    url,
    httr::accept("image/png"),
    httr::add_headers('Authorization' = paste("Bearer", hf_token)),
    body = body,
    encode = "json",
    httr::timeout(budget())
  )
  .gen_provider_check(response)
  content_type <- tolower(as.character(httr::headers(response)[["content-type"]] %||% "")[1])
  if (grepl("application/json",content_type)) {
    body <- .gen_provider_body(response)
    if (!is.null(body$error)) .gen_stop(.gen_provider_http_error(400L,body))
    .gen_stop("provider_response_invalid",outcome="unknown",stage="inference")
  }
  img_binary <- httr::content(response,as="raw")

  # Construct filename & path
  .gen_image_publish(.gen_image_receipt(list(.gen_image_source(img_binary)),service_name,model,h,y,
    list(width=wx,height=hx)),directory,label_sanitized,budget)
}

#' @rdname gen_img
#' @method gen_img genflow_agent
#' @details For a `genflow_agent`, a saved `context` is used as the image prompt
#'   when the agent has no explicit `prompt`. Supply `prompt_override` through
#'   `...` to replace the saved prompt for one call.
#' @export
gen_img.genflow_agent <- function(prompt, ...) {
  withCallingHandlers({
  agent <- prompt
  if (is.null(agent$prompt) && !is.null(agent$context)) {
    agent$prompt <- agent$context
  }
  overrides <- list(...)
  formals_default <- formals(gen_img.default)
  agent_args <- .genflow_prepare_agent_args(
    agent = agent,
    overrides = overrides,
    target_formals = formals_default,
    required = "prompt",
    override_aliases = c(prompt_override = "prompt"),
    override_label = "gen_img()"
  )
  do.call(gen_img.default, agent_args, quote = TRUE)
  }, error = .gen_boundary_error)
}
#' FAL image generation (internal)
#'
#' @keywords internal
#' @noRd
.gen_img_fal <- function(prompt,
                         model = "fal-ai/flux/schnell",
                         temp,
                         steps = 18,
                         h,
                         y,
                         directory,
                         label_sanitized,
                         poll_interval = 3,
                         max_poll_seconds = 600,
                         request = .gen_img_fal_request,
                         sleep = Sys.sleep,
                         clock = Sys.time,
                         references = list(),
                         budget = .gen_image_budget(max_poll_seconds,clock)) {
  service_name <- "fal"
  fal_token <- Sys.getenv("FAL_API_KEY")
  if (!nzchar(fal_token)) .gen_stop("missing_credential", params=list(credential="FAL_API_KEY"),stage="preflight")

  poll_interval <- suppressWarnings(as.numeric(poll_interval)[1])
  max_poll_seconds <- suppressWarnings(as.numeric(max_poll_seconds)[1])
  if (is.na(poll_interval) || !is.finite(poll_interval) ||
      poll_interval < 0) {
    stop("`poll_interval` must be a non-negative number.", call. = FALSE)
  }
  if (is.na(max_poll_seconds) || !is.finite(max_poll_seconds) ||
      max_poll_seconds <= 0) {
    stop("`max_poll_seconds` must be a positive number.", call. = FALSE)
  }

  model_path <- .gen_img_fal_model_path(model)
  url <- sprintf("https://queue.fal.run/fal-ai/%s", model_path)
  body <- .gen_img_fal_input(model_path, prompt, temp, steps, h, y)
  if (length(references)) { body$image_urls <- references; body$num_images <- 1L }

  response <- request(
    method = "POST",
    url = url,
    token = fal_token,
    body = body,
    timeout_secs = budget()
  )
  .gen_provider_check(response,expected=c(200L,201L,202L))

  content <- response$content
  if (!is.list(content)) {
    .gen_stop("provider_response_invalid",stage="inference",outcome="unknown")
  }
  status_url <- as.character(content$status_url %||% "")[1]
  response_url <- as.character(content$response_url %||% "")[1]
  if (is.na(status_url) || !nzchar(status_url) ||
      is.na(response_url) || !nzchar(response_url)) {
    stop("FAL response is missing `status_url` or `response_url`.")
  }

  status <- .gen_img_fal_status(content[["status"]] %||% "IN_QUEUE")
  status_content <- content
  started <- clock()
  while (status %in% c("IN_QUEUE", "IN_PROGRESS")) {
    elapsed <- as.numeric(difftime(clock(), started, units = "secs"))
    if (elapsed >= max_poll_seconds) {
      stop(
        "FAL image generation timed out after ",
        max_poll_seconds,
        " seconds."
      )
    }

    sleep(min(poll_interval,budget()))
    status_response <- request(
      method = "POLL",
      url = status_url,
      token = fal_token,
      body = NULL,
      timeout_secs = budget()
    )
    .gen_provider_check(status_response,expected=200L,stage="poll")
    status_content <- status_response$content
    if (!is.list(status_content)) {
    .gen_stop("provider_response_invalid",stage="inference",outcome="unknown")
  }
    status <- .gen_img_fal_status(status_content[["status"]])
  }

  if (status %in% c("FAILED", "CANCELLED", "CANCELED")) {
    .gen_stop(if (status=="FAILED") "provider_task_failed" else "provider_task_cancelled",stage="poll",outcome="failed")
  }
  if (!identical(status, "COMPLETED")) {
    stop("FAL generation did not complete. Final status: ", status)
  }
  if (!is.null(status_content$error)) .gen_stop("provider_task_failed",stage="poll",outcome="failed")

  receipt <- .gen_image_receipt(list(),service_name,model,h,y,body$image_size)
  receipt$result_url <- response_url
  .gen_image_publish(receipt,directory,label_sanitized,budget,
    download=function(url,timeout) {
      response <- request("DOWNLOAD",url,token=NULL,body=NULL,timeout_secs=timeout)
      .gen_provider_check(response,expected=200L,stage="download")
      response$raw
    }, resolve=function(receipt,budget) .gen_img_resolve_image_receipt(receipt,budget,request))
}

#' Normalize a FAL model id for queue URLs
#'
#' @keywords internal
#' @noRd
.gen_img_fal_model_path <- function(model) {
  value <- trimws(as.character(model)[1])
  value <- sub("^fal-ai/", "", value, ignore.case = TRUE)
  if (is.na(value) || !nzchar(value) || grepl("[[:space:]]", value) ||
      grepl("^/|/$", value)) {
    stop(
      "FAL `model` must be a path such as \"fal-ai/flux/schnell\".",
      call. = FALSE
    )
  }
  value
}

#' Build the FAL image request body
#'
#' @keywords internal
#' @noRd
.gen_img_fal_input <- function(model, prompt, temp, steps, h, y) {
  height <- max(8L, as.integer(round(as.numeric(h)[1] / 8) * 8))
  width <- max(8L, as.integer(round(as.numeric(y)[1] / 8) * 8))
  step_count <- suppressWarnings(as.numeric(steps)[1])
  if (is.na(step_count) || !is.finite(step_count)) step_count <- 18

  if (identical(tolower(model), "flux/schnell")) {
    step_count <- min(max(round(step_count), 1L), 12L)
  }

  input <- list(
    prompt = prompt,
    guidance_scale = temp,
    num_inference_steps = as.integer(step_count),
    enable_safety_checker = FALSE,
    image_size = list(height = height, width = width)
  )
  if (identical(tolower(model), "flux/schnell")) {
    input$num_images <- 1L
    input$output_format <- "png"
  }
  if (grepl("imagen|kontext", model, ignore.case = TRUE)) {
    input$aspect_ratio <- .gen_img_replicate_aspect_ratio(
      width = width,
      height = height
    )
  }
  input
}

#' Normalize a FAL queue status
#'
#' @keywords internal
#' @noRd
.gen_img_fal_status <- function(status) {
  toupper(trimws(as.character(status %||% "")[1]))
}

#' Execute one FAL image HTTP request
#'
#' @keywords internal
#' @noRd
.gen_img_fal_request <- function(method,
                                 url,
                                 token = NULL,
                                 body = NULL,
                                 timeout_secs = 300) {
  method <- match.arg(
    toupper(method),
    c("POST", "POLL", "RESULT", "DOWNLOAD")
  )
  if (identical(method, "POST")) {
    response <- httr::POST(
      url,
      httr::add_headers(
        `Content-Type` = "application/json",
        Authorization = paste("Key", token)
      ),
      body = body,
      encode = "json",
      httr::timeout(timeout_secs)
    )
  } else if (method %in% c("POLL", "RESULT")) {
    response <- httr::GET(
      url,
      httr::add_headers(Authorization = paste("Key", token)),
      httr::timeout(timeout_secs)
    )
  } else {
    return(list(status=200L,raw=.gen_image_download(url,timeout_secs)))
  }

  list(
    status = httr::status_code(response),
    content = .gen_provider_body(response),
    text = httr::content(response, "text", encoding = "UTF-8")
  )
}
#' OpenAI image generation (internal)
#'
#' @keywords internal
#' @noRd
.gen_img_openai <- function(prompt,
                            model,
                            temp,
                            steps = NULL,
                            h,
                            y,
                            directory,
                            label_sanitized,
                            request = .gen_img_openai_request,
                            references = list(),
                            budget = .gen_image_budget(600)) {
  openai_key <- Sys.getenv("OPENAI_API_KEY")
  if (!nzchar(openai_key)) .gen_stop("missing_credential", params=list(credential="OPENAI_API_KEY"),stage="preflight")

  size_str <- .gen_img_openai_size(model, width = y, height = h)
  body <- list(
    model = model,
    prompt = prompt,
    n = 1L,
    size = size_str
  )

  url_api <- paste0("https://api.openai.com/v1/images/",if (length(references)) "edits" else "generations")
  if (length(references)) body$images <- lapply(references,function(uri) list(image_url=uri))
  response <- request(
    method = "POST",
    url = url_api,
    api_key = openai_key,
    body = body,
    timeout_secs = budget()
  )

  status <- as.integer(response$status %||% 0L)
  resp_txt <- as.character(response$text %||% "")[1]
  .gen_provider_check(response)

  cont <- tryCatch(
    jsonlite::fromJSON(resp_txt, simplifyVector = FALSE),
    error = function(e) {
      stop("OpenAI returned invalid JSON: ", conditionMessage(e), call. = FALSE)
    }
  )
  if (!is.null(cont$error)) .gen_stop(.gen_provider_http_error(400L,cont))

  .gen_image_publish(.gen_image_receipt(lapply(cont$data %||% list(),.gen_image_source),"openai",model,h,y,size_str),
    directory,label_sanitized,budget,function(url,timeout) {
      response <- request("GET",url,api_key=NULL,body=NULL,timeout_secs=timeout)
      .gen_provider_check(response,expected=200L,stage="download")
      response$raw
    })
}

#' Resolve an OpenAI image size for the selected model family
#'
#' @keywords internal
#' @noRd
.gen_img_openai_size <- function(model, width, height) {
  model_id <- tolower(trimws(as.character(model)[1]))
  candidates <- if (grepl("^(gpt-image-|chatgpt-image-)", model_id)) {
    data.frame(
      size = c("1024x1024", "1024x1536", "1536x1024"),
      width = c(1024, 1024, 1536),
      height = c(1024, 1536, 1024)
    )
  } else if (grepl("^dall-e-3($|-)", model_id)) {
    data.frame(
      size = c("1024x1024", "1792x1024", "1024x1792"),
      width = c(1024, 1792, 1024),
      height = c(1024, 1024, 1792)
    )
  } else if (grepl("^dall-e-2($|-)", model_id)) {
    data.frame(
      size = c("256x256", "512x512", "1024x1024"),
      width = c(256, 512, 1024),
      height = c(256, 512, 1024)
    )
  } else {
    stop(
      "Unsupported OpenAI image model: ", model,
      ". Use a GPT Image, DALL-E 3, or DALL-E 2 model id.",
      call. = FALSE
    )
  }

  distance <- (candidates$width - as.numeric(width))^2 +
    (candidates$height - as.numeric(height))^2
  candidates$size[[which.min(distance)]]
}

#' Execute one OpenAI image HTTP request
#'
#' @keywords internal
#' @noRd
.gen_img_openai_request <- function(method,
                                    url,
                                    api_key = NULL,
                                    body = NULL,
                                    timeout_secs = 300) {
  method <- match.arg(toupper(method), c("POST", "GET"))
  if (identical(method, "POST")) {
    response <- httr::POST(
      url,
      httr::add_headers(
        Authorization = paste("Bearer", api_key),
        `Content-Type` = "application/json"
      ),
      body = body,
      encode = "json",
      httr::timeout(timeout_secs)
    )
    return(list(
      status = httr::status_code(response),
      text = httr::content(response, "text", encoding = "UTF-8")
    ))
  }

  list(status=200L,raw=.gen_image_download(url,timeout_secs))
}
#' Replicate image generation (internal)
#'
#' @keywords internal
#' @noRd
.gen_img_replicate <- function(prompt,
                               model = "black-forest-labs/flux-schnell",
                               temp,
                               steps = 18,
                               h,
                               y,
                               directory,
                               label_sanitized,
                               model_version = NULL,
                               replicate_input = NULL,
                               poll_interval = 3,
                               max_poll_seconds = 600,
                               request = .gen_img_replicate_request,
                               sleep = Sys.sleep,
                               clock = Sys.time,
                               budget = .gen_image_budget(max_poll_seconds,clock)) {
  service_name <- "replicate"
  replicate_token <- Sys.getenv("REPLICATE_API_TOKEN")
  if (!nzchar(replicate_token)) .gen_stop("missing_credential", params=list(credential="REPLICATE_API_TOKEN"),stage="preflight")

  poll_interval <- suppressWarnings(as.numeric(poll_interval)[1])
  max_poll_seconds <- suppressWarnings(as.numeric(max_poll_seconds)[1])
  if (is.na(poll_interval) || !is.finite(poll_interval) || poll_interval < 0) {
    stop("`poll_interval` must be a non-negative number.", call. = FALSE)
  }
  if (is.na(max_poll_seconds) || !is.finite(max_poll_seconds) ||
      max_poll_seconds <= 0) {
    stop("`max_poll_seconds` must be a positive number.", call. = FALSE)
  }

  model_ref <- .gen_img_replicate_model_ref(model, model_version)
  model_path <- model_ref$model

  input_params <- .gen_img_replicate_input(
    model = model_path,
    prompt = prompt,
    temp = temp,
    steps = steps,
    h = h,
    y = y,
    overrides = replicate_input
  )
  url <- "https://api.replicate.com/v1/predictions"
  body_list <- list(
    version = model_ref$reference,
    input = input_params
  )

  response <- request(
    method = "POST",
    url = url,
    token = replicate_token,
    body = body_list,
    timeout_secs = budget()
  )
  .gen_provider_check(response,expected=c(200L,201L))

  poll_content <- response$content
  if (!is.list(poll_content)) {
    .gen_stop("provider_response_invalid",stage="inference",outcome="unknown")
  }
  status <- .gen_img_replicate_status(poll_content$status)
  get_url <- poll_content$urls$get
  started <- clock()

  while (status %in% c("starting", "processing")) {
    if (is.null(get_url) || !nzchar(as.character(get_url)[1])) {
      stop("Polling URL (urls$get) not found in Replicate response.")
    }
    elapsed <- as.numeric(difftime(clock(), started, units = "secs"))
    if (elapsed >= max_poll_seconds) {
      stop(
        "Replicate image generation timed out after ",
        max_poll_seconds,
        " seconds."
      )
    }

    sleep(min(poll_interval,budget()))
    poll_response <- request(
      method = "POLL",
      url = as.character(get_url)[1],
      token = replicate_token,
      body = NULL,
      timeout_secs = budget()
    )
    .gen_provider_check(poll_response,expected=200L,stage="poll")
    poll_content <- poll_response$content
    if (!is.list(poll_content)) {
    .gen_stop("provider_response_invalid",stage="inference",outcome="unknown")
  }
    status <- .gen_img_replicate_status(poll_content$status)
  }

  if (identical(status,"failed")) .gen_stop("provider_task_failed",stage="poll",outcome="failed")
  if (identical(status,"canceled")) .gen_stop("provider_task_cancelled",stage="poll",outcome="failed")
  if (!identical(status,"succeeded")) .gen_stop("provider_response_invalid",stage="poll",outcome="unknown")

  result <- poll_content$output %||% list()
  if (is.list(result) && !is.null(names(result))) result <- list(result)
  .gen_image_publish(.gen_image_receipt(lapply(result,.gen_image_source),service_name,model,h,y),
    directory,label_sanitized,budget,function(url,timeout) {
      response <- request("DOWNLOAD",url,token=NULL,body=NULL,timeout_secs=timeout)
      .gen_provider_check(response,expected=200L,stage="download")
      response$raw
    })
}

#' Parse a Replicate model id and optional pinned version
#'
#' @keywords internal
#' @noRd
.gen_img_replicate_model_ref <- function(model, model_version = NULL) {
  model_value <- trimws(as.character(model)[1])
  parts <- strsplit(model_value, ":", fixed = TRUE)[[1]]
  if (length(parts) > 2L || !nzchar(parts[[1]])) {
    stop(
      "`model` must use \"owner/name\" or \"owner/name:version\".",
      call. = FALSE
    )
  }
  model_path <- parts[[1]]
  embedded_version <- if (length(parts) == 2L) trimws(parts[[2]]) else NULL
  explicit_version <- if (is.null(model_version) || length(model_version) == 0L) {
    NULL
  } else {
    trimws(as.character(model_version)[1])
  }
  if (!is.null(explicit_version) &&
      (is.na(explicit_version) || !nzchar(explicit_version))) {
    explicit_version <- NULL
  }
  if (!is.null(embedded_version) && !nzchar(embedded_version)) {
    stop("Embedded Replicate model version cannot be empty.", call. = FALSE)
  }
  if (!is.null(embedded_version) && !is.null(explicit_version) &&
      !identical(embedded_version, explicit_version)) {
    stop(
      "`model_version` conflicts with the version embedded in `model`.",
      call. = FALSE
    )
  }
  if (!grepl("^[^/[:space:]]+/[^/[:space:]]+$", model_path)) {
    stop("Replicate `model` must use the form \"owner/name\".", call. = FALSE)
  }

  version <- explicit_version %||% embedded_version
  list(
    model = model_path,
    version = version,
    reference = if (is.null(version)) model_path else
      paste0(model_path, ":", version)
  )
}

#' Build model-aware Replicate image inputs
#'
#' @keywords internal
#' @noRd
.gen_img_replicate_input <- function(model,
                                     prompt,
                                     temp,
                                     steps,
                                     h,
                                     y,
                                     overrides = NULL) {
  input <- list(prompt = prompt)

  if (identical(tolower(model), "black-forest-labs/flux-schnell")) {
    step_count <- suppressWarnings(as.numeric(steps)[1])
    if (is.na(step_count) || !is.finite(step_count)) step_count <- 4
    input <- list(
      prompt = prompt,
      go_fast = TRUE,
      megapixels = "1",
      num_outputs = 1L,
      aspect_ratio = .gen_img_replicate_aspect_ratio(
        width = y,
        height = h
      ),
      output_format = "png",
      output_quality = 100L,
      num_inference_steps = as.integer(
        min(max(round(step_count), 1L), 4L)
      ),
      disable_safety_checker = FALSE
    )
  }

  if (!is.null(overrides)) {
    if (!is.list(overrides) || is.null(names(overrides)) ||
        any(!nzchar(names(overrides)))) {
      stop("`replicate_input` must be a named list.", call. = FALSE)
    }
    input <- utils::modifyList(input, overrides, keep.null = TRUE)
  }
  input
}

#' Choose the closest supported FLUX aspect ratio
#'
#' @keywords internal
#' @noRd
.gen_img_replicate_aspect_ratio <- function(width, height) {
  labels <- c(
    "1:1", "16:9", "21:9", "3:2", "2:3", "4:5",
    "5:4", "3:4", "4:3", "9:16", "9:21"
  )
  ratios <- c(
    1, 16 / 9, 21 / 9, 3 / 2, 2 / 3, 4 / 5,
    5 / 4, 3 / 4, 4 / 3, 9 / 16, 9 / 21
  )
  width <- suppressWarnings(as.numeric(width)[1])
  height <- suppressWarnings(as.numeric(height)[1])
  if (is.na(width) || !is.finite(width) || width <= 0 ||
      is.na(height) || !is.finite(height) || height <= 0) {
    stop("Replicate image dimensions must be positive numbers.", call. = FALSE)
  }
  requested <- width / height
  labels[[which.min(abs(log(ratios) - log(requested)))]]
}

#' Normalize a Replicate prediction status
#'
#' @keywords internal
#' @noRd
.gen_img_replicate_status <- function(status) {
  value <- tolower(trimws(as.character(status %||% "")[1]))
  if (identical(value, "successful")) "succeeded" else value
}

#' Execute one Replicate image HTTP request
#'
#' @keywords internal
#' @noRd
.gen_img_replicate_request <- function(method,
                                       url,
                                       token = NULL,
                                       body = NULL,
                                       timeout_secs = 300) {
  method <- match.arg(toupper(method), c("POST", "POLL", "DOWNLOAD"))
  if (identical(method, "POST")) {
    response <- httr::POST(
      url,
      httr::add_headers(
        `Content-Type` = "application/json",
        Authorization = paste("Bearer", token)
      ),
      body = body,
      encode = "json",
      httr::timeout(timeout_secs)
    )
    return(list(
      status = httr::status_code(response),
      content = .gen_provider_body(response),
      text = httr::content(response, "text", encoding = "UTF-8")
    ))
  }

  if (identical(method, "POLL")) {
    response <- httr::GET(
      url,
      httr::add_headers(Authorization = paste("Bearer", token)),
      httr::timeout(timeout_secs)
    )
    return(list(
      status = httr::status_code(response),
      content = .gen_provider_body(response),
      text = httr::content(response, "text", encoding = "UTF-8")
    ))
  }

  list(status=200L,raw=.gen_image_download(url,timeout_secs))
}

# FAL may finish inference before its result endpoint becomes readable. Persist
# that endpoint as a receipt; recovery only GETs it, never resubmits the job.
.gen_img_resolve_image_receipt <- function(receipt,budget,request=.gen_img_fal_request) {
  if (is.null(receipt$result_url)) return(receipt)
  if (!identical(receipt$service,"fal") || !.gen_scalar(receipt$result_url) ||
      !startsWith(receipt$result_url,"https://queue.fal.run/")) .gen_stop("image_receipt_invalid")
  key <- Sys.getenv("FAL_API_KEY")
  if (!nzchar(key)) .gen_stop("missing_credential",params=list(credential="FAL_API_KEY"))
  response <- tryCatch(request("RESULT",receipt$result_url,token=key,body=NULL,timeout_secs=budget()),
    error=function(e) .gen_stop("image_download_failed",retryable=TRUE,stage="download"))
  .gen_provider_check(response,expected=200L,stage="download")
  content <- response$content$data %||% response$content
  sources <- content$images %||% if (!is.null(content$image)) list(content$image) else list()
  receipt$sources <- lapply(sources,.gen_image_source)
  receipt$result_url <- NULL
  receipt
}
