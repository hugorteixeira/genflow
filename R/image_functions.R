#' Generate an image via a selected provider
#'
#' High-level image generation wrapper that dispatches to provider-specific
#' implementations (Hugging Face, FAL, Replicate, OpenAI, etc.) and saves the
#' resulting image to disk.
#'
#' @param prompt Character. Main text prompt describing the desired image.
#' @param add Optional character or object(s) to append to the prompt. If provided and
#'   character, it will be concatenated to `prompt`.
#' @param directory Optional output directory. Defaults to `getwd()/imgs` if NULL.
#' @param label Optional short label to be used in filenames. If NULL, derived from prompt.
#' @param service Provider identifier (e.g., "hf", "fal", "replicate", "together", "cloudflare", "bfl").
#' @param model Provider model identifier (e.g., "black-forest-labs/FLUX.1-schnell").
#' @param temp Numeric guidance/temperature parameter (provider-specific meaning).
#' @param steps Integer inference steps (if supported by provider/model).
#' @param h Integer output height in pixels.
#' @param y Integer output width in pixels (named `y` here to match internal calls).
#' @param ... Additional arguments forwarded to method-specific implementations.
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
gen_img.default <- function(prompt, add = NULL, directory = NULL, label = NULL, service = "hf", model = "black-forest-labs/FLUX.1-schnell", temp = 5, steps = 18, h = 1072, y = 1920, ...) { # Ensure 'y' matches internal calls (or use 'w')

  start_time <- Sys.time()

  # --- Input Processing & Setup (Keep existing code) ---
  if (is.list(service)) service <- as.character(service$service %||% service[[1]]) else if (is.vector(service)) service <- as.character(service[1])
  if (is.list(model)) model <- as.character(model$model %||% model$model %||% model[[1]]) else if (is.vector(model)) model <- as.character(model[1])
  if (is.list(temp)) temp <- as.numeric(temp$temperature %||% temp$temp %||% temp[[1]]) else if (is.vector(temp)) temp <- as.numeric(temp[1])
  if (length(temp) != 1 || !is.numeric(temp) || is.na(temp)) temp <- 7.0 # Default guidance
  if (is.list(steps)) steps <- as.numeric(steps$steps %||% steps[[1]]) else if (is.vector(steps)) steps <- as.numeric(steps[1])
  if (length(steps) != 1 || !is.numeric(steps) || is.na(steps)) steps <- 18 # Default steps
  if (is.list(h)) h <- as.numeric(h$h %||% h[[1]]) else if (is.vector(h)) h <- as.numeric(h[1])
  if (length(h) != 1 || !is.numeric(h) || is.na(h) || h <=0) h <- 1024 # Default h
  if (is.list(y)) y <- as.numeric(y$y %||% y$w %||% y[[1]]) else if (is.vector(y)) y <- as.numeric(y[1]) # Allow w or y
  if (length(y) != 1 || !is.numeric(y) || is.na(y) || y <= 0) y <- 1024 # Default y (width)

  if (is.null(directory)) directory <- file.path(getwd(), "imgs")
  if (!dir.exists(directory)) dir.create(directory, recursive = TRUE, showWarnings = FALSE)

  final_prompt <- if (!is.null(add)) paste(prompt, add) else prompt

  label_processed <- label %||% paste(strsplit(final_prompt, "[[:space:]]+")[[1]][1:min(5, length(strsplit(final_prompt, "[[:space:]]+")[[1]]))], collapse = "_")
  label_processed <- substr(label_processed, 1, 36)
  label_sanitized <- .sanitize_filename(label_processed)
  model_sanitized_name <- .sanitize_filename(model) # For filename

  # --- Call Service Function ---
  cat("Generating image via", service, "/", model, "...\n" )
  file_path <- NULL
  error_occurred <- FALSE
  api_call_error_msg <- "" # Store specific error

  tryCatch({
    # Switch calls the appropriate internal function
    file_path <- switch(tolower(service),
                    #    "cloudflare" = gen_img_cloudflare(final_prompt, model, temp, steps, h, y, directory, label_sanitized),
                        "hf" = .gen_img_hf(final_prompt, model, temp, steps, h, y, directory, label_sanitized),
                        "fal" = .gen_img_fal(final_prompt, model, temp, steps, h, y, directory, label_sanitized),
                     #   "together" = gen_img_together(final_prompt, model, temp, steps, h, y, directory, label_sanitized),
                        "replicate" = .gen_img_replicate(final_prompt, model, temp, steps, h, y, directory, label_sanitized),
                      #  "bfl" = gen_img_bfl(final_prompt, model, temp, steps, h, y, directory, label_sanitized),
                        # Add other image services here
                        stop(paste0("Image service not supported: ", service)) # Use stop for unsupported
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
    duration            = duration_response,
    status_api       = final_status,
    status_msg  = final_msg_status, # "OK" or error message
    prompt_usado     = final_prompt,
    saved_file    = if(final_status == "SUCCESS") file_path else NA_character_,
    dimensoes        = if(final_status == "SUCCESS") paste0(y, "x", h) else NA_character_ # WxH
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
#' Hugging Face image generation (internal)
#'
#' @keywords internal
#' @noRd
.gen_img_hf <- function(prompt, model = "black-forest-labs/FLUX.1-schnell", temp = NULL, steps = 18, h = NULL, y = NULL, directory = NULL, label_sanitized = NULL) {
  service_name <- "hf"
  # Get HF token
  hf_token <- Sys.getenv("HUGGINGFACE_API_TOKEN")
  if (hf_token == "") stop("HUGGINGFACE_API_TOKEN must be set.")

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
  content_type <- httr::headers(response)[["content-type"]]
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

  if (is.null(img_binary) || length(img_binary) == 0) {
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
#' @export
gen_img.genflow_agent <- function(prompt, ...) {
  agent <- prompt
  overrides <- list(...)
  formals_default <- formals(gen_img.default)
  agent_args <- .genflow_prepare_agent_args(
    agent = agent,
    overrides = overrides,
    target_formals = formals_default,
    required = "prompt"
  )
  do.call(gen_img.default, agent_args, quote = TRUE)
}
#' FAL image generation (internal)
#'
#' @keywords internal
#' @noRd
.gen_img_fal <- function(prompt, model = "flux-1/schnell", temp, steps = 18, h, y, directory, label_sanitized) {
  # (Internal logic matches the _srv version; mapping removed)
  service_name <- "fal"
  fal_token <- Sys.getenv("FAL_API_KEY")
  if (fal_token == "") stop("FAL_API_KEY must be set.")

  model_path <- model
  model_sanitized_name <- .sanitize_filename(model)

  url <- sprintf("https://queue.fal.run/fal-ai/%s", model_path)
  hx <- round(h / 8) * 8
  wx <- round(y / 8) * 8
  safety = FALSE

  # model  -> string containing the model id/name
  # hx, wx  -> desired height and width

  body <- if (grepl("imagen|kontext", model, ignore.case = TRUE)) {

    ## --- model contains 'imagen'/'kontext' -> use aspect_ratio ---------------
    list(
      prompt              = prompt,
      guidance_scale      = temp,
      num_inference_steps = steps,
      enable_safety_checker = FALSE,
      image_size          = list(height = hx, width = wx),
      aspect_ratio = "16:9"
    )

  } else {

    ## --- other models -> use image_size -------------------------
    list(
      prompt              = prompt,
      guidance_scale      = temp,
      num_inference_steps = steps,
      enable_safety_checker = FALSE,
      image_size          = list(height = hx, width = wx)
    )
  }
  response <- httr::POST(url, httr::add_headers('Content-Type' = 'application/json', 'Authorization' = paste("Key", fal_token)),
                         body = jsonlite::toJSON(body, auto_unbox = TRUE), encode = "json")

  if (httr::status_code(response) >= 400) {
    erro <- httr::content(response, "text", encoding = "UTF-8")
      stop(sprintf("Initial FAL API error (%s): %s", httr::status_code(response), tryCatch(jsonlite::fromJSON(erro)$detail, error=function(e) erro)))
  }
  content <- httr::content(response, as = "parsed", simplifyVector = TRUE)

  # DEBUG: Print the initial response structure if links are missing
  status_url <- content$`_links`$status$href
  response_url <- content$`_links`$response$href
  if (is.null(status_url) || is.null(response_url)) {
    # Try alternative common structure (older API?)
    status_url <- content$status_url
    response_url <- content$response_url
    if (is.null(status_url) || is.null(response_url)){
      stop("Status/response links not found in FAL API (see debug above).")
    } else {
      #warning("Usando campos 'status_url'/'response_url' legados da API FAL.")
    }
  }
  status <- content$status
  #cat("Initial FAL status:", status, "\nWaiting for completion...\n")
  while (status %in% c("IN_QUEUE", "IN_PROGRESS")) {
    Sys.sleep(3)
    status_response <- httr::GET(status_url, httr::add_headers('Authorization' = paste("Key", fal_token)))
    if (httr::status_code(status_response) >= 400) {
      erro <- httr::content(status_response, "text", encoding = "UTF-8")
      stop(sprintf("FAL API status error (%s): %s", httr::status_code(status_response), tryCatch(jsonlite::fromJSON(erro)$detail, error=function(e) erro)))
    }
    status_content <- httr::content(status_response, as = "parsed", simplifyVector = TRUE)
    status <- status_content$status
    #cat("Current FAL status:", status, "\n")
    if (status == "FAILED" || status == "CANCELLED") stop("FAL generation failed: ", status_content$error %||% status)
  }
  if (status != "COMPLETED") stop("FAL generation did not complete. Final status: ", status)

  #cat("Imagem FAL gerada. Buscando resultado...\n")
  response_response <- httr::GET(response_url, httr::add_headers('Authorization' = paste("Key", fal_token)))
  if (httr::status_code(response_response) >= 400) {
    erro <- httr::content(response_response, "text", encoding = "UTF-8")
    stop(sprintf("FAL API result error (%s): %s", httr::status_code(response_response), tryCatch(jsonlite::fromJSON(erro)$detail, error=function(e) erro)))
  }
  response_content <- httr::content(response_response, as = "parsed", simplifyVector = TRUE)

  img_url <- NULL
  if (!is.null(response_content$images) && is.data.frame(response_content$images) && "url" %in% names(response_content$images)) img_url <- response_content$images$url[1]
  else if (!is.null(response_content$image$url)) img_url <- response_content$image$url
  else if (is.list(response_content) && length(response_content)>0 && !is.null(response_content[[1]]$url)) img_url <- response_content[[1]]$url # Another possible structure

  if (is.null(img_url)) { message("DEBUG FAL: Final response structure without URL:"); print(str(response_content)); stop("Image URL not found in FAL response.") }
  #cat("Baixando imagem FAL de:", img_url, "\n")
  img_response <- httr::GET(img_url)
  if (img_response$status_code != 200) stop("Error downloading FAL image: ", img_response$status_code)
  img_binary <- httr::content(img_response, as = "raw")
  if (is.null(img_binary) || length(img_binary) == 0) stop("Empty FAL image content.")

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
#' OpenAI image generation (internal)
#'
#' @keywords internal
#' @noRd
.gen_img_openai <- function(prompt, model, temp, steps = NULL, h, y, directory, label_sanitized) {
  # checa API key
  openai_key <- Sys.getenv("OPENAI_API_KEY")
  if (openai_key == "") stop("OPENAI_API_KEY must be set.")

  if(is.null(h)){
    h = 1024
  }
  if(is.null(y))
  {
    y = 1536
  }
  # force dimensions to 256, 512, or 1024
  allowed <- c(256, 512, 1024, 1536)
  hx <- allowed[which.min(abs(allowed - round(h/8)*8))]
  wx <- allowed[which.min(abs(allowed - round(y/8)*8))]
  size_str <- paste0(wx, "x", hx)

  # monta body (sem response_format!)
  body <- list(
    model  = model,   # ex: "gpt-image-1" ou "dall-e-3"
    prompt = prompt,
    n      = 1,
    size   = size_str
  )

  url_api <- "https://api.openai.com/v1/images/generations"
  resp <- httr::POST(
    url_api,
    httr::add_headers(
      Authorization  = paste("Bearer", openai_key),
      `Content-Type` = "application/json"
    ),
    body   = jsonlite::toJSON(body, auto_unbox = TRUE),
    encode = "json",
    httr::timeout(300)
  )

  status   <- httr::status_code(resp)
  resp_txt <- httr::content(resp, "text", encoding = "UTF-8")
  if (status < 200 || status >= 300) {
    stop(sprintf("OpenAI API error (%s): %s", status, resp_txt))
  }

  # parse JSON de forma segura
  cont <- jsonlite::fromJSON(resp_txt, simplifyVector = FALSE)
  if (!is.null(cont$error)) {
    stop("OpenAI API error: ", cont$error$message)
  }

  # extrai url ou b64_json
  if (!is.list(cont$data) || length(cont$data) < 1) {
    stop("OpenAI response missing 'data' field")
  }
  item <- cont$data[[1]]
  img_bin <- NULL

  if (!is.null(item$url)) {
    # baixa a imagem da URL
    dl <- httr::GET(item$url, httr::timeout(300))
    if (httr::status_code(dl) != 200) {
      stop("Failed to download the image from the returned URL.")
    }
    img_bin <- httr::content(dl, as = "raw")

  } else if (!is.null(item$b64_json)) {
    img_bin <- base64enc::base64decode(item$b64_json)

  } else {
    stop("Unexpected response: neither url nor b64_json.")
  }

  # monta nome e salva em disco
  datetime_str <- format(Sys.time(), "%Y%m%d_%H%M%S")
  model_sani    <- .sanitize_filename(model)
  filename      <- paste(label_sanitized, "openai", model_sani, datetime_str, sep = "_")
  filename      <- paste0(filename, ".png")
  file_path     <- file.path(directory, filename)

  writeBin(img_bin, file_path)
  if (!file.exists(file_path) || file.info(file_path)$size == 0) {
    stop("Failed to save image to disk.")
  }

  return(file_path)
}
#' Replicate image generation (internal)
#'
#' @keywords internal
#' @noRd
.gen_img_replicate <- function(prompt, model = "black-forest-labs/flux-schnell", temp, steps = 18, h, y, directory, label_sanitized) {
  service_name <- "replicate"
  replicate_token <- Sys.getenv("REPLICATE_API_TOKEN")
  if (replicate_token == "") stop("REPLICATE_API_TOKEN must be set.")

  model_version <- NULL
  model_path <- model
  if (grepl(":", model)) {
    parts <- strsplit(model, ":")[[1]]
    model_path <- parts[1]
    model_version <- parts[2]
  }
  model_sanitized_name <- .sanitize_filename(model)

  url <- paste0("https://api.replicate.com/v1/models/", model_path, "/predictions")
  body_list <- list()

  hx <- round(h / 8) * 8
  wx <- round(y / 8) * 8
  if(wx > 1440){
    wx = 1440
  }

  if(model == "black-forest-labs/flux-schnell"){
    steps = 4
  }

  input_params <- list(prompt = prompt, height = hx, width = wx, num_outputs = 1,
                       output_format = "png", aspect_ratio = "16:9", safety_tolerance = 6, guidance_scale = temp, num_inference_steps = steps)
  body_list$input <- input_params

  #cat("Sending initial request to Replicate (model:", model_path, ")...\n")
  response <- httr::POST(
    url,
    httr::add_headers('Content-Type' = 'application/json', 'Authorization' = paste("Token", replicate_token)),
    body = jsonlite::toJSON(body_list, auto_unbox = TRUE, null = "null"), # Era null="json"
    encode = "json"
  )

  if (!(httr::status_code(response) %in% c(200, 201))) {
    erro <- httr::content(response, "text", encoding = "UTF-8")
    # Tentativa de extrair o detalhe do erro JSON
    detail_error <- tryCatch(jsonlite::fromJSON(erro)$detail, error = function(e) erro)
    stop(sprintf("Initial Replicate API error (%s): %s", httr::status_code(response), detail_error))
  }
  content <- httr::content(response, as = "parsed", simplifyVector = TRUE)
  get_url <- content$urls$get
  if(is.null(get_url)) { print(content); stop("Polling URL (urls$get) not found in Replicate response.") }

  status <- content$status
  #cat("Status inicial Replicate:", status, ". Aguardando...\n")
  while (status %in% c("starting", "processing")) {
    Sys.sleep(3)
    poll_response <- httr::GET(get_url, httr::add_headers('Authorization' = paste("Token", replicate_token)))
    if (httr::status_code(poll_response) != 200) {
      erro <- httr::content(poll_response, "text", encoding = "UTF-8")
      detail_error <- tryCatch(jsonlite::fromJSON(erro)$detail, error = function(e) erro)
      stop(sprintf("Replicate API status error (%s): %s", httr::status_code(poll_response), detail_error))
    }
    poll_content <- httr::content(poll_response, as = "parsed", simplifyVector = TRUE)
    status <- poll_content$status
    #cat("Status atual Replicate:", status, "\n")
    if (status == "failed") stop("Replicate prediction failed: ", poll_content$error)
    if (status == "canceled") stop("Replicate prediction canceled.")
  }
  if (status != "succeeded") { print(poll_content); stop("Replicate prediction did not succeed. Final status: ", status) }

  result <- poll_content$output
  if (is.null(result) || length(result) == 0) { print(poll_content); stop("Output not found in Replicate response.") }
  img_url <- result[[1]]
  if (!is.character(img_url) || nchar(img_url) == 0) { print(poll_content); stop("Invalid image URL in Replicate result.") }
  #cat("Baixando imagem Replicate de:", img_url, "\n")
  img_response <- httr::GET(img_url)
  if (img_response$status_code != 200) stop("Error downloading Replicate image: ", img_response$status_code)
  img_binary <- httr::content(img_response, as = "raw")
  if (is.null(img_binary) || length(img_binary) == 0) stop("Empty Replicate image content.")

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
