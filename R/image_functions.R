#' Generate an image via a selected provider
#'
#' High-level image generation wrapper that dispatches to provider-specific
#' implementations (Hugging Face, FAL, Replicate, and OpenAI) and saves the
#' resulting image to disk.
#'
#' @param prompt Character. Main text prompt describing the desired image.
#' @param add Optional character or object(s) to append to the prompt. If provided and
#'   character, it will be concatenated to `prompt`.
#' @param directory Optional output directory. Defaults to `getwd()/imgs` if NULL.
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
#' @param max_poll_seconds Maximum time in seconds to poll an asynchronous
#'   provider before returning an error.
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
  UseMethod("gen_img")
}

#' @rdname gen_img
#' @method gen_img default
#' @export
gen_img.default <- function(prompt,
                            add = NULL,
                            directory = NULL,
                            label = NULL,
                            service = "hf",
                            model = NULL,
                            temp = 5,
                            steps = 18,
                            h = 1072,
                            y = 1920,
                            model_version = NULL,
                            replicate_input = NULL,
                            poll_interval = 3,
                            max_poll_seconds = 600,
                            ...) {

  start_time <- Sys.time()

  # --- Input Processing & Setup ---
  if (is.list(service)) {
    service <- service$service %||% if (length(service)) service[[1]] else NULL
  }
  if (is.vector(service)) service <- as.character(service[1])
  service <- tolower(trimws(as.character(service %||% "")[1]))
  if (is.na(service) || !nzchar(service)) {
    stop("`service` must be a non-empty provider identifier.", call. = FALSE)
  }
  if (is.list(model)) {
    model <- model$model %||% if (length(model)) model[[1]] else NULL
  }
  if (is.vector(model)) model <- as.character(model[1])
  if (is.null(model) || length(model) == 0L || is.na(model) || !nzchar(model)) {
    model <- .gen_img_default_model(service)
  }
  if (is.list(temp)) temp <- as.numeric(temp$temperature %||% temp$temp %||% temp[[1]]) else if (is.vector(temp)) temp <- as.numeric(temp[1])
  if (length(temp) != 1 || !is.numeric(temp) || is.na(temp)) temp <- 7.0 # Default guidance
  if (is.list(steps)) steps <- as.numeric(steps$steps %||% steps[[1]]) else if (is.vector(steps)) steps <- as.numeric(steps[1])
  if (length(steps) != 1 || !is.numeric(steps) || is.na(steps)) steps <- 18 # Default steps
  if (is.list(h)) h <- as.numeric(h$h %||% h[[1]]) else if (is.vector(h)) h <- as.numeric(h[1])
  if (length(h) != 1 || !is.numeric(h) || is.na(h) || h <=0) h <- 1024 # Default h
  if (is.list(y)) y <- as.numeric(y$y %||% y$w %||% y[[1]]) else if (is.vector(y)) y <- as.numeric(y[1]) # Allow w or y
  if (length(y) != 1 || !is.numeric(y) || is.na(y) || y <= 0) y <- 1024 # Default y (width)

  if (is.null(directory)) directory <- .genflow_default_dir("imgs")
  if (!dir.exists(directory)) dir.create(directory, recursive = TRUE, showWarnings = FALSE)

  final_prompt <- if (!is.null(add)) paste(prompt, add) else prompt

  label_processed <- label %||% paste(strsplit(final_prompt, "[[:space:]]+")[[1]][1:min(5, length(strsplit(final_prompt, "[[:space:]]+")[[1]]))], collapse = "_")
  label_processed <- substr(label_processed, 1, 36)
  label_sanitized <- .sanitize_filename(label_processed)

  # --- Call Service Function ---
  cat("Generating image via", service, "/", model, "...\n" )
  file_path <- NULL
  error_occurred <- FALSE
  api_call_error_msg <- "" # Store specific error

  tryCatch({
    file_path <- switch(service,
      "hf" = .gen_img_hf(final_prompt, model, temp, steps, h, y, directory, label_sanitized),
      "fal" = .gen_img_fal(
        final_prompt,
        model,
        temp,
        steps,
        h,
        y,
        directory,
        label_sanitized,
        poll_interval = poll_interval,
        max_poll_seconds = max_poll_seconds
      ),
      "openai" = .gen_img_openai(final_prompt, model, temp, steps, h, y, directory, label_sanitized),
      "replicate" = .gen_img_replicate(
        final_prompt,
        model,
        temp,
        steps,
        h,
        y,
        directory,
        label_sanitized,
        model_version = model_version,
        replicate_input = replicate_input,
        poll_interval = poll_interval,
        max_poll_seconds = max_poll_seconds
      ),
      stop(paste0("Image service not supported: ", service))
    )
  }, error = function(e) {
    error_occurred <<- TRUE
    api_call_error_msg <<- conditionMessage(e) # Capture the specific error
    # Error message printed later
  })

  # --- Post-Processing & Status Determination ---
  end_time <- Sys.time()
  duration_response <- as.numeric(difftime(end_time, start_time, units = "secs"))

  final_status <- "SUCCESS"
  final_msg_status <- "OK"
  final_result_value <- NULL # Will hold file path if successful

  if(error_occurred || is.null(file_path) || !file.exists(file_path)) {
    final_status <- "ERROR"
    # Use the specific error message if available, otherwise a generic one
    final_msg_status <- if (nchar(api_call_error_msg) > 0) {
      api_call_error_msg
    } else if (is.null(file_path)) {
      "Failed: file_path is NULL."
    } else if (!file.exists(file_path)) {
      "Failed: file not found."
    } else {
      "Failed: unknown error."
    }
    # Log the failure
    message("\n-----------------------------------")
    message("ERROR generating image with '", service, "' / '", model,"':")
    message(final_msg_status)
    message(sprintf("Time until the error: %.2f s", duration_response))
    message("-----------------------------------\n")
  } else {
    # Success Case
    final_result_value <- file_path # Store the path
    message_line <- sprintf(
      "\nImage '%s' (%s / %s / temp:%.1f) generated in %.2f s.\nSaved at: %s\n",
      label_processed, service, model, temp, duration_response, file_path
    )
    cat(message_line)
    # Optional: Display image if magick is available and session is interactive
    # tryCatch({ if(interactive() && requireNamespace("magick", quietly = TRUE)) print(magick::image_read(file_path)) }, error = function(e) { warning("...") })
  }


  # --- Construct the Return List ---
  resultado_com_atributos <- list(
    response_value   = final_result_value, # File path if success, NULL if error
    label            = label, # Original label passed in
    label_cat        = label_processed, # Processed label used
    service          = service,
    model           = model,
    temp             = temp, # Guidance scale
    steps            = steps,
    duration            = duration_response,
    status_api       = final_status,
    status_msg  = final_msg_status, # "OK" or error message
    prompt_usado     = final_prompt,
    saved_file    = if(final_status == "SUCCESS") file_path else NA_character_,
    dimensoes        = if(final_status == "SUCCESS") paste0(y, "x", h) else NA_character_, # WxH
    content_type     = "image"
    # Add other relevant attributes like 'steps' if needed
  )

  # Persist stats to daily logs unless suppressed in batch workers
  should_persist <- tryCatch({ Sys.getenv("genflow_SKIP_PERSIST_LOG", unset = "0") != "1" }, error = function(e) TRUE)
  if (isTRUE(should_persist)) {
    try({
      .persist_stats_row(list(
        label      = label_processed %||% label %||% NA_character_,
        model      = model %||% NA_character_,
        temp       = temp %||% NA_real_,
        duration   = duration_response %||% NA_real_,
        tks_envia  = NA_real_,
        tks_recebe = NA_real_,
        status_api = final_status %||% "UNKNOWN"
      ))
    }, silent = TRUE)
  }

  return(invisible(resultado_com_atributos)) # Return the LIST in both success/error cases
}

#' Resolve the default image model for a provider
#'
#' @keywords internal
#' @noRd
.gen_img_default_model <- function(service) {
  switch(tolower(service),
    "hf" = "black-forest-labs/FLUX.1-schnell",
    "fal" = "fal-ai/flux/schnell",
    "replicate" = "black-forest-labs/flux-schnell",
    "openai" = "gpt-image-2",
    "default"
  )
}

#' Hugging Face image generation (internal)
#'
#' @keywords internal
#' @noRd
.gen_img_hf <- function(prompt, model = "black-forest-labs/FLUX.1-schnell", temp = NULL, steps = 18, h = NULL, y = NULL, directory = NULL, label_sanitized = NULL) {
  service_name <- "hf"
  # Get HF token
  hf_token <- Sys.getenv("HUGGINGFACE_API_TOKEN")
  if (!nzchar(hf_token)) stop("HUGGINGFACE_API_TOKEN must be set.")

  # Use 'model' directly as the model path
  model_path <- model
  model_sanitized_name <- .sanitize_filename(model) # Sanitize provided name

  # URL and Body
  url <- sprintf("https://router.huggingface.co/hf-inference/models/%s", model_path)
  hx <- round(h / 8) * 8
  wx <- round(y / 8) * 8

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
    httr::timeout(300)
  )
  # Process response & get binary
  content_type <- tolower(
    as.character(httr::headers(response)[["content-type"]] %||% "")[1]
  )
  img_binary <- NULL
  if (httr::status_code(response) >= 200 && httr::status_code(response) < 300) {
    if (grepl("^image/", content_type)) {
      img_binary <- httr::content(response, as = "raw")
    } else if (grepl("application/json", content_type)) {
      content <- httr::content(response, as = "parsed", simplifyVector = TRUE)
      if (!is.null(content$error)) stop(sprintf("Hugging Face JSON API error: %s", paste(content$error, collapse="; ")))
      else stop("Unexpected JSON response (successful?) from Hugging Face API.")
    } else {
      warning("Unexpected content type: ", content_type)
      img_binary <- httr::content(response, as = "raw") # Attempt
    }
  } else {
    erro_text <- httr::content(response, "text", encoding = "UTF-8")
    error_detail <- erro_text
    if (grepl("application/json", content_type)) {
      try({
        content <- jsonlite::fromJSON(erro_text)
        if (!is.null(content$error)) error_detail <- paste(content$error, collapse="; ")
      }, silent = TRUE)
    }
    stop(sprintf("HuggingFace API error (%s): %s", httr::status_code(response), error_detail))
  }

  if (!is.raw(img_binary) || length(img_binary) == 0L) {
    stop("Empty or invalid HuggingFace image content.")
  }

  # Construct filename & path
  datetime_str <- format(Sys.time(), "%Y%m%d_%H%M%S")
  filename <- paste(label_sanitized, service_name, model_sanitized_name, datetime_str, sep = "_")
  filename <- paste0(filename, ".png")
  file_path <- file.path(directory, filename)

  # Save the image
  tryCatch({
    writeBin(img_binary, file_path)
    if (!file.exists(file_path) || file.info(file_path)$size == 0) stop("Failed to validate saved file.")
  }, error = function(e) {
    stop("Failed to save HuggingFace image at ", file_path, ": ", e$message)
  })

  return(file_path)
}

#' @rdname gen_img
#' @method gen_img genflow_agent
#' @details For a `genflow_agent`, a saved `context` is used as the image prompt
#'   when the agent has no explicit `prompt`. Supply `prompt_override` through
#'   `...` to replace the saved prompt for one call.
#' @export
gen_img.genflow_agent <- function(prompt, ...) {
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
                         clock = Sys.time) {
  service_name <- "fal"
  fal_token <- Sys.getenv("FAL_API_KEY")
  if (!nzchar(fal_token)) stop("FAL_API_KEY must be set.")

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
  model_sanitized_name <- .sanitize_filename(model)
  url <- sprintf("https://queue.fal.run/fal-ai/%s", model_path)
  body <- .gen_img_fal_input(model_path, prompt, temp, steps, h, y)

  response <- request(
    method = "POST",
    url = url,
    token = fal_token,
    body = body,
    timeout_secs = 300
  )
  response_status <- suppressWarnings(as.integer(response$status)[1])
  if (is.na(response_status) ||
      !(response_status %in% c(200L, 201L, 202L))) {
    stop(sprintf(
      "Initial FAL API error (%s): %s",
      response$status %||% "unknown",
      .gen_img_fal_error_detail(response)
    ))
  }

  content <- response$content
  if (!is.list(content)) {
    stop("FAL returned an invalid queue submission response.", call. = FALSE)
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

    sleep(poll_interval)
    status_response <- request(
      method = "POLL",
      url = status_url,
      token = fal_token,
      body = NULL,
      timeout_secs = 300
    )
    poll_status <- suppressWarnings(as.integer(status_response$status)[1])
    if (is.na(poll_status) || !identical(poll_status, 200L)) {
      stop(sprintf(
        "FAL API status error (%s): %s",
        status_response$status %||% "unknown",
        .gen_img_fal_error_detail(status_response)
      ))
    }
    status_content <- status_response$content
    if (!is.list(status_content)) {
      stop("FAL returned an invalid queue status response.", call. = FALSE)
    }
    status <- .gen_img_fal_status(status_content[["status"]])
  }

  if (status %in% c("FAILED", "CANCELLED", "CANCELED")) {
    stop(
      "FAL generation failed: ",
      status_content$error %||% status_content$detail %||% status
    )
  }
  if (!identical(status, "COMPLETED")) {
    stop("FAL generation did not complete. Final status: ", status)
  }
  if (!is.null(status_content$error)) {
    stop("FAL generation failed: ", status_content$error)
  }

  result_response <- request(
    method = "RESULT",
    url = response_url,
    token = fal_token,
    body = NULL,
    timeout_secs = 300
  )
  result_status <- suppressWarnings(as.integer(result_response$status)[1])
  if (is.na(result_status) || !identical(result_status, 200L)) {
    stop(sprintf(
      "FAL API result error (%s): %s",
      result_response$status %||% "unknown",
      .gen_img_fal_error_detail(result_response)
    ))
  }
  img_url <- .gen_img_fal_output_url(result_response$content)

  img_response <- request(
    method = "DOWNLOAD",
    url = img_url,
    token = NULL,
    body = NULL,
    timeout_secs = 300
  )
  download_status <- suppressWarnings(as.integer(img_response$status)[1])
  if (is.na(download_status) || !identical(download_status, 200L)) {
    stop(
      "Error downloading FAL image: ",
      img_response$status %||% "unknown"
    )
  }
  img_binary <- img_response$raw
  if (!is.raw(img_binary) || length(img_binary) == 0L) {
    stop("Empty or invalid FAL image content.")
  }

  datetime_str <- format(Sys.time(), "%Y%m%d_%H%M%S")
  filename <- paste(label_sanitized, service_name, model_sanitized_name, datetime_str, sep = "_")
  filename <- paste0(filename, ".png")
  file_path <- file.path(directory, filename)

  tryCatch({
    writeBin(img_binary, file_path)
    if (!file.exists(file_path) || file.info(file_path)$size == 0) stop("Failed to validate saved file.")
  }, error = function(e) stop("Failed to save FAL image: ", e$message))
  return(file_path)
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

#' Extract a FAL image URL from a result response
#'
#' @keywords internal
#' @noRd
.gen_img_fal_output_url <- function(content) {
  if (!is.list(content)) {
    stop("FAL returned an invalid result response.", call. = FALSE)
  }
  content <- content$data %||% content
  images <- content$images
  img_url <- if (is.data.frame(images) && "url" %in% names(images) &&
                 nrow(images) > 0L) {
    images$url[[1]]
  } else if (is.list(images) && length(images) > 0L) {
    first <- images[[1]]
    if (is.character(first)) first[[1]] else first$url %||% first$uri
  } else {
    content$image$url %||% content$image$uri
  }
  img_url <- as.character(img_url %||% "")[1]
  if (is.na(img_url) || !nzchar(img_url)) {
    stop("Image URL not found in FAL response.")
  }
  img_url
}

#' Extract a readable FAL API error
#'
#' @keywords internal
#' @noRd
.gen_img_fal_error_detail <- function(response) {
  content <- response$content
  if (is.list(content)) {
    detail <- content$detail %||% content$error
    if (!is.null(detail)) return(paste(as.character(detail), collapse = "; "))
  }
  text <- as.character(response$text %||% "")[1]
  if (is.na(text) || !nzchar(text)) "unknown error" else text
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
    response <- httr::GET(url, httr::timeout(timeout_secs))
    return(list(
      status = httr::status_code(response),
      raw = httr::content(response, as = "raw")
    ))
  }

  list(
    status = httr::status_code(response),
    content = httr::content(response, as = "parsed", simplifyVector = FALSE),
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
                            request = .gen_img_openai_request) {
  openai_key <- Sys.getenv("OPENAI_API_KEY")
  if (!nzchar(openai_key)) stop("OPENAI_API_KEY must be set.")

  size_str <- .gen_img_openai_size(model, width = y, height = h)
  body <- list(
    model = model,
    prompt = prompt,
    n = 1L,
    size = size_str
  )

  url_api <- "https://api.openai.com/v1/images/generations"
  response <- request(
    method = "POST",
    url = url_api,
    api_key = openai_key,
    body = body,
    timeout_secs = 300
  )

  status <- as.integer(response$status %||% 0L)
  resp_txt <- as.character(response$text %||% "")[1]
  if (status < 200 || status >= 300) {
    stop(sprintf("OpenAI API error (%s): %s", status, resp_txt))
  }

  cont <- tryCatch(
    jsonlite::fromJSON(resp_txt, simplifyVector = FALSE),
    error = function(e) {
      stop("OpenAI returned invalid JSON: ", conditionMessage(e), call. = FALSE)
    }
  )
  if (!is.null(cont$error)) {
    stop("OpenAI API error: ", cont$error$message %||% "unknown error")
  }

  if (!is.list(cont$data) || length(cont$data) < 1) {
    stop("OpenAI response missing 'data' field.")
  }
  item <- cont$data[[1]]
  img_bin <- NULL

  if (!is.null(item$url)) {
    download <- request(
      method = "GET",
      url = as.character(item$url)[1],
      api_key = NULL,
      body = NULL,
      timeout_secs = 300
    )
    if (!identical(as.integer(download$status), 200L)) {
      stop("Failed to download the image from the returned URL.")
    }
    img_bin <- download$raw
  } else if (!is.null(item$b64_json)) {
    if (!requireNamespace("base64enc", quietly = TRUE)) {
      stop("Package 'base64enc' is required to decode OpenAI image output.")
    }
    img_bin <- base64enc::base64decode(item$b64_json)
  } else {
    stop("Unexpected response: neither url nor b64_json.")
  }
  if (!is.raw(img_bin) || length(img_bin) == 0L) {
    stop("OpenAI returned empty or invalid image content.")
  }

  datetime_str <- format(Sys.time(), "%Y%m%d_%H%M%S")
  model_sani <- .sanitize_filename(model)
  filename <- paste(label_sanitized, "openai", model_sani, datetime_str, sep = "_")
  file_path <- file.path(directory, paste0(filename, ".png"))

  writeBin(img_bin, file_path)
  if (!file.exists(file_path) || file.info(file_path)$size == 0) {
    stop("Failed to save image to disk.")
  }

  return(file_path)
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

  response <- httr::GET(url, httr::timeout(timeout_secs))
  list(
    status = httr::status_code(response),
    raw = httr::content(response, as = "raw")
  )
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
                               clock = Sys.time) {
  service_name <- "replicate"
  replicate_token <- Sys.getenv("REPLICATE_API_TOKEN")
  if (!nzchar(replicate_token)) stop("REPLICATE_API_TOKEN must be set.")

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
  model_sanitized_name <- .sanitize_filename(model)

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
    timeout_secs = 300
  )
  response_status <- suppressWarnings(as.integer(response$status)[1])
  if (is.na(response_status) ||
      !(response_status %in% c(200L, 201L))) {
    stop(sprintf(
      "Initial Replicate API error (%s): %s",
      response$status %||% "unknown",
      .gen_img_replicate_error_detail(response)
    ))
  }

  poll_content <- response$content
  if (!is.list(poll_content)) {
    stop("Replicate returned an invalid prediction object.", call. = FALSE)
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

    sleep(poll_interval)
    poll_response <- request(
      method = "POLL",
      url = as.character(get_url)[1],
      token = replicate_token,
      body = NULL,
      timeout_secs = 300
    )
    poll_status <- suppressWarnings(as.integer(poll_response$status)[1])
    if (is.na(poll_status) || !identical(poll_status, 200L)) {
      stop(sprintf(
        "Replicate API status error (%s): %s",
        poll_response$status %||% "unknown",
        .gen_img_replicate_error_detail(poll_response)
      ))
    }
    poll_content <- poll_response$content
    if (!is.list(poll_content)) {
      stop("Replicate returned an invalid prediction object while polling.")
    }
    status <- .gen_img_replicate_status(poll_content$status)
  }

  if (identical(status, "failed")) {
    stop("Replicate prediction failed: ", poll_content$error %||% "unknown error")
  }
  if (identical(status, "canceled")) {
    stop("Replicate prediction canceled.")
  }
  if (!identical(status, "succeeded")) {
    stop("Replicate prediction did not succeed. Final status: ", status)
  }

  result <- poll_content$output
  if (is.null(result) || length(result) == 0L) {
    stop("Output not found in Replicate response.")
  }
  first_output <- if (is.list(result)) result[[1]] else result[[1]]
  img_url <- if (is.character(first_output)) {
    first_output[[1]]
  } else if (is.list(first_output)) {
    first_output$url %||% first_output$uri
  } else {
    NULL
  }
  if (is.null(img_url) || !is.character(img_url) || !nzchar(img_url[[1]])) {
    stop("Invalid image URL in Replicate result.")
  }

  img_response <- request(
    method = "DOWNLOAD",
    url = img_url[[1]],
    token = replicate_token,
    body = NULL,
    timeout_secs = 300
  )
  download_status <- suppressWarnings(as.integer(img_response$status)[1])
  if (is.na(download_status) || !identical(download_status, 200L)) {
    stop(
      "Error downloading Replicate image: ",
      img_response$status %||% "unknown"
    )
  }
  img_binary <- img_response$raw
  if (!is.raw(img_binary) || length(img_binary) == 0L) {
    stop("Empty or invalid Replicate image content.")
  }

  datetime_str <- format(Sys.time(), "%Y%m%d_%H%M%S")
  filename <- paste(label_sanitized, service_name, model_sanitized_name, datetime_str, sep = "_")
  filename <- paste0(filename, ".png")
  file_path <- file.path(directory, filename)

  tryCatch({
    writeBin(img_binary, file_path)
    if (!file.exists(file_path) || file.info(file_path)$size == 0) stop("Failed to validate saved file.")
  }, error = function(e) stop("Failed to save Replicate image: ", e$message))
  return(file_path)
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

#' Extract a readable Replicate API error
#'
#' @keywords internal
#' @noRd
.gen_img_replicate_error_detail <- function(response) {
  content <- response$content
  if (is.list(content)) {
    detail <- content$detail %||% content$error
    if (!is.null(detail)) return(paste(as.character(detail), collapse = "; "))
  }
  text <- as.character(response$text %||% "")[1]
  if (is.na(text) || !nzchar(text)) "unknown error" else text
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
      content = httr::content(response, as = "parsed", simplifyVector = FALSE),
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
      content = httr::content(response, as = "parsed", simplifyVector = FALSE),
      text = httr::content(response, "text", encoding = "UTF-8")
    ))
  }

  headers <- if (is.null(token) || !nzchar(as.character(token)[1])) {
    NULL
  } else {
    httr::add_headers(Authorization = paste("Bearer", token))
  }
  response <- httr::GET(url, headers, httr::timeout(timeout_secs))
  list(
    status = httr::status_code(response),
    raw = httr::content(response, as = "raw")
  )
}
