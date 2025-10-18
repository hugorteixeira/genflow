#' Internal: OpenAI chat completions call
#'
#' Calls the OpenAI Chat Completions API and returns either the textual
#' response, a tool/function-call result, or an error sentinel string.
#'
#' @param prompt Character; user prompt to send.
#' @param model Character; model identifier (defaults to `"gpt-5-mini"`).
#' @param temp_v Numeric; sampling temperature.
#' @param add_img Optional character; path to an image file to include (multimodal models).
#' @param tools Logical; whether to enable tool/function calling support.
#' @param my_tools Function; your function with tools definitions.
#' @param timeout_secs Numeric; total request timeout in seconds.
#' @return Character or object; text content, tool-call result, or an error sentinel string.
#' @keywords internal
#' @noRd
.gen_txt_openai <- function(prompt, model, temp_v, add_img, tools = FALSE, my_tools = NULL, timeout_secs = 80) {
  api_url <- "https://api.openai.com/v1/chat/completions"
  if (is.null(model)) {
    model <- "gpt-5-mini"
  }
  api_key <- Sys.getenv("OPENAI_API_KEY")
  if (is.null(api_key) || api_key == "") {
    stop("Environment variable OPENAI_API_KEY not set.")
  }
  headers <- add_headers(Authorization = paste("Bearer", api_key), "Content-Type" = "application/json")
  # Build initial_message (same logic as before)
  initial_message <- list(role = "user", content = if (!is.null(add_img)) { list(list(type = "text", text = prompt), list(type = "image_url", image_url = list(url = paste0("data:image/jpeg;base64,", .encode_image(add_img))))) } else { prompt })
  # Build request body (same logic as before; tools/tool_choice when applicable)
  if(grepl("^o",model)){
    temp_v <- 1
  }
  if (tools) {
    body <- list(model = model, messages = list(initial_message), tools = my_tools, tool_choice = "auto", temperature = temp_v)
  } else if(grepl("search", model)) {
    body <- list(model = model, messages = list(initial_message))
  } else {
    body <- list(model = model, messages = list(initial_message), temperature = temp_v)
  }


  # --- API call with httr::timeout() and tryCatch ---
  response <- tryCatch({
    httr::POST(
      url = api_url,
      headers,
      body = toJSON(body, auto_unbox = TRUE, null = "null"),
      encode = "json",
      config = httr::timeout(timeout_secs) # <--- Built-in httr timeout
    )
  }, error = function(e) {
    err_msg <- conditionMessage(e)
    if (grepl("Timeout was reached|Operation timed out", err_msg, ignore.case = TRUE)) {
      warning(paste("HTTR timeout in .gen_txt_openai after", timeout_secs, "seconds:", err_msg))
      return(paste0("TIMEOUT_ERRORR_HTTR: API call exceeded ", timeout_secs, " seconds."))
    } else {
      warning(paste("HTTR error in .gen_txt_openai:", err_msg))
      return(paste0("HTTR_ERRORR: ", err_msg))
    }
  })
  # --------------------------------------------------

  # Check whether 'response' is an error string
  if (is.character(response) && (startsWith(response, "TIMEOUT_ERRORR_HTTR:") || startsWith(response, "HTTR_ERRORR:"))) {
    return(response)
  }

  # --- Response processing (if no httr error) ---
  # (Same code as before to check HTTP status and parse JSON)
  # ... (check http_status, content, result$choices, tool_calls, function_call, content_filter) ...
  if (http_status(response)$category != "Success") {
    error_content <- content(response, "text", encoding = "UTF-8")
    error_msg <- paste("OpenAI API error:", http_status(response)$reason, "-", error_content)
    warning(error_msg)
    error_details <- tryCatch(fromJSON(error_content)$error$message, error = function(e) error_content)
    return(paste("API_ERRORR:", http_status(response)$reason, "-", error_details)) # Keep API error details
  }

  result <- content(response, as = "parsed", type = "application/json", encoding = "UTF-8")
  # ... (rest of processing for result, tool_calls, function_call, content, content_filter) ...
  message_content <- result$choices[[1]]$message
  function_result <- NULL
  if (!is.null(message_content$tool_calls)) {
    function_result <- result
  } else if (!is.null(message_content$function_call)) {
    function_result <- result
  }

  if (!is.null(function_result)) {
    if (inherits(function_result, "htmlwidget")) { return(as.character(function_result)) } else { return(function_result) }
  } else if (!is.null(message_content$content)) {
    return(message_content$content)
  } else if (!is.null(result$choices[[1]]$finish_reason) && result$choices[[1]]$finish_reason == "content_filter") {
    warning("Content blocked by OpenAI content filter.")
    return("CONTENT_FILTERED: Response blocked by OpenAI content filter.")
  } else {
    warning("Unexpected OpenAI API response. Finish reason: ", result$choices[[1]]$finish_reason)
    return("API_RESPONSE_ERRORR: No valid content, tool_calls or function_call found.")
  }
}
#' Internal: OpenRouter chat completions call
#'
#' Calls the OpenRouter-compatible Chat Completions API and returns either
#' the textual response, a tool-call result, or an error sentinel string.
#'
#' @param prompt Character; user prompt to send.
#' @param model Character; model identifier (defaults to a Mistral instruct model).
#' @param temp_v Numeric; sampling temperature.
#' @param add_img Optional character; path to an image file to include (multimodal models).
#' @param tools Logical; whether to enable tool/function calling support.
#' @param my_tools Function; your function with tools definitions.
#' @param timeout_secs Numeric; total request timeout in seconds.
#' @return Character or object; text content, tool-call result, or an error sentinel string.
#' @keywords internal
#' @noRd
.gen_txt_openrouter <- function(prompt, model, temp_v, add_img, tools = FALSE, my_tools = NULL, timeout_secs = 80) {
  # ... (initial code: default model, API key, URL, headers, initial_message, body) ...
  if (is.null(model)) {
    model <- "mistralai/mistral-7b-instruct"
  }
  api_key_openrouter <- Sys.getenv("OPENROUTER_API_KEY")
  if (is.null(api_key_openrouter) || api_key_openrouter == "") {
    stop("Environment variable OPENROUTER_API_KEY not set.")
  }
  api_url <- "https://openrouter.ai/api/v1/chat/completions"
  your_app_url <- Sys.getenv("YOUR_APP_URL", "Voting_LLMs")
  your_app_name <- Sys.getenv("YOUR_APP_NAME", "Voting_LLMs")
  headers <- add_headers("Content-Type" = "application/json", "Authorization" = paste("Bearer", api_key_openrouter), "HTTP-Referer" = your_app_url, "X-Title" = your_app_name)
  # Build initial_message (same logic as before)
  initial_message <- list(role = "user", content = if (!is.null(add_img)) { list(list(type = "text", text = prompt), list(type = "image_url", image_url = list(url = paste0("data:image/jpeg;base64,", .encode_image(add_img))))) } else { prompt })
  # Build request body (same logic as before; tools/tool_choice when applicable)
  if (tools) {
    body <- list(model = model, messages = list(initial_message), tools = my_tools(), tool_choice = "auto", temperature = temp_v)
  } else {
    body <- list(model = model, messages = list(initial_message), temperature = temp_v)
  }


  # --- API call with httr::timeout() and tryCatch ---
  response <- tryCatch({
    httr::POST(
      url = api_url,
      headers,
      body = toJSON(body, auto_unbox = TRUE, null = "null"),
      encode = "json",
      config = httr::timeout(timeout_secs) # <--- Built-in httr timeout
    )
  }, error = function(e) {
    err_msg <- conditionMessage(e)
    if (grepl("Timeout was reached|Operation timed out", err_msg, ignore.case = TRUE)) {
      warning(paste("HTTR timeout in .gen_txt_openrouter after", timeout_secs, "seconds:", err_msg))
      return(paste0("TIMEOUT_ERRORR_HTTR: API call exceeded ", timeout_secs, " seconds."))
    } else {
      warning(paste("HTTR error in .gen_txt_openrouter:", err_msg))
      return(paste0("HTTR_ERRORR: ", err_msg))
    }
  })
  # --------------------------------------------------

  # Check whether 'response' is an error string
  if (is.character(response) && (startsWith(response, "TIMEOUT_ERROR_HTTR:") || startsWith(response, "HTTR_ERROR:"))) {
    return(response)
  }

  # --- Response processing (if no httr error) ---
  # (Same code as before to check HTTP status and parse JSON)
  # ... (check http_status, content, result$choices, tool_calls, content_filter) ...
  if (http_status(response)$category != "Success") {
    error_content <- content(response, "text", encoding = "UTF-8")
    error_msg <- paste("OpenRouter API error:", http_status(response)$reason, "-", error_content)
    warning(error_msg)
    error_details <- tryCatch(fromJSON(error_content)$error$message, error = function(e) error_content)
    return(paste("API_ERROR:", http_status(response)$reason, "-", error_details)) # Keep API error details
  }
  result <- content(response, as = "parsed", type = "application/json", encoding = "UTF-8")
  # ... (rest of processing for result, tool_calls, content, content_filter) ...
  message_content <- result$choices[[1]]$message
  function_result <- NULL
  if (!is.null(message_content$tool_calls)) {
    function_result <- result
  }
  if (!is.null(function_result)) {
    if (inherits(function_result, "htmlwidget")) { return(as.character(function_result)) } else { return(function_result) }
  } else if (!is.null(message_content$content)) {
    return(message_content$content)
  } else if (!is.null(result$choices[[1]]$finish_reason) && result$choices[[1]]$finish_reason == "content_filter") {
    warning("Content blocked by filter (OpenRouter/model).")
    return("CONTENT_FILTERED: Response blocked by content filter.")
  } else {
    warning("Unexpected OpenRouter API response. Finish reason: ", result$choices[[1]]$finish_reason)
    return("API_RESPONSE_ERROR: No valid content or tool_calls found.")
  }
}
#' Internal: HuggingFace TGI chat call
#'
#' Calls a HuggingFace Inference (TGI) OpenAI-compatible chat endpoint and
#' returns the textual response, a tool-call result, or an error sentinel string.
#'
#' @param prompt Character; user prompt to send.
#' @param model Character; model identifier (default varies per deployment).
#' @param temp_v Numeric; sampling temperature.
#' @param add_img Optional character; path to an image file to include (if supported).
#' @param tools Logical; whether to enable tool/function calling support (if supported).
#' @param my_tools Function; your function with tools definitions.
#' @param timeout_secs Numeric; total request timeout in seconds.
#' @return Character or object; text content, tool-call result, or an error sentinel string.
#' @keywords internal
#' @noRd
.gen_txt_hf <- function(prompt, model, temp_v, add_img, tools = FALSE, my_tools = NULL, timeout_secs = 80) {
  # --- Configuration ---
  if (is.null(model)) {
    model <- "mistralai/Mixtral-8x7B-Instruct-v0.1" # Default model (example)
    # model <- "Qwen/Qwen2.5-72B-Instruct" # Original default
  }
  api_key_hf <- Sys.getenv("HUGGINGFACE_API_TOKEN") # Env var name used in original
  if (is.null(api_key_hf) || api_key_hf == "") {
    stop("Environment variable HUGGINGFACE_API_TOKEN not set.")
  }
  # Using the TGI OpenAI-compatible endpoint format
  api_url <- paste0("https://api-inference.hf.co/models/", model) # Base URL
  # Check if the model supports the chat completions endpoint
  # Some models might only support /generate or require a specific task endpoint
  # Assuming /v1/chat/completions is supported by the chosen model
  api_url <- paste0(api_url, "/v1/chat/completions")

  headers <- add_headers(
    "Authorization" = paste("Bearer", api_key_hf),
    "Content-Type" = "application/json"
  )

  # --- Body Construction ---
  # Using OpenAI-compatible structure for TGI endpoint
  # Check image support for TGI chat completions endpoint - might not be standard
  # Note: TGI might support tools via OpenAI format.
  if (tools) {
    tool_declarations <- tryCatch(my_tools, # Assuming OpenAI format
                                  error = function(e) {
                                    warning("Could not get function declarations for HuggingFace TGI. Tools disabled.")
                                    return(NULL)
                                  })
    if (!is.null(tool_declarations)) {
      body$tools <- tool_declarations
      body$tool_choice <- "auto"
    } else {
      tools <- FALSE
    }
  }

  if (!is.null(add_img)) {
    # TGI chat completions endpoint might not support images this way.
    # Image support often requires specific multimodal models and potentially different endpoints/formats.
    warning("HuggingFace chat endpoint image support not standard. Sending image data, but it might be ignored or cause errors.")
    user_content <- list(
      list(type = "text", text = prompt),
      # Original used 'image', OpenAI uses 'image_url'. Let's try 'image_url'.
      list(type = "image_url", image_url = list(url = paste0("data:image/jpeg;base64,", .encode_image(add_img))))
    )
  } else {
    user_content <- prompt
  }

  max_t <- 4096 # A common default max_tokens

  body <- list(
    model = model, # May not be strictly needed if it's in the URL, but often included
    messages = list(
      list(role = "user", content = user_content)
    ),
    temperature = temp_v,
    max_tokens = max_t,
    stream = FALSE
  )
  # Add tools if requested
  if (tools && !is.null(body$tools)) {
    # Body already has tools and tool_choice added above
  } else {
    body$tools <- NULL
    body$tool_choice <- NULL
  }

  # TGI might prefer top_p or other sampling parameters
  if (temp_v <= 0.01) {
    body$temperature <- NULL
    body$top_p <- 0.9 # Example
  }

  json_body <- toJSON(body, auto_unbox = TRUE, null = "null")

  # --- API call with timeout and error handling ---
  response <- tryCatch({
    httr::POST(
      url = api_url,
      headers,
      body = json_body,
      encode = "json",
      config = httr::timeout(timeout_secs)
    )
  }, error = function(e) {
    err_msg <- conditionMessage(e)
    # HF Inference API might return 503 if model is loading
    if (grepl("Service Unavailable|503", err_msg, ignore.case = TRUE)) {
      warning(paste("HuggingFace model potentially loading (503):", model, "-", err_msg))
      return(paste0("HF_MODEL_LOADING: Model may be loading (503 error). ", err_msg))
    } else if (grepl("Timeout was reached|Operation timed out", err_msg, ignore.case = TRUE)) {
      warning(paste("HTTR timeout in .gen_txt_hf after", timeout_secs, "seconds:", err_msg))
      return(paste0("TIMEOUT_ERRORR_HTTR: API call exceeded ", timeout_secs, " seconds."))
    } else {
      warning(paste("HTTR error in .gen_txt_hf:", err_msg))
      return(paste0("HTTR_ERRORR: ", err_msg))
    }
  })

  # --- Response Processing ---
  if (is.character(response) && (startsWith(response, "TIMEOUT_ERRORR_HTTR:") || startsWith(response, "HTTR_ERRORR:") || startsWith(response, "HF_MODEL_LOADING:"))) {
    return(response)
  }

  if (http_status(response)$category != "Success") {
    error_content <- content(response, "text", encoding = "UTF-8")
    # HF errors can be plain text or JSON { "error": "..." }
    error_msg <- paste("HuggingFace API error:", http_status(response)$reason, "-", error_content)
    warning(error_msg)
    error_details <- tryCatch(fromJSON(error_content)$error, error = function(e) error_content)
    # Sometimes error is in 'detail' field
    if (is.null(error_details) || error_details == error_content) {
      error_details <- tryCatch(fromJSON(error_content)$detail, error = function(e) error_content)
    }
    return(paste("API_ERRORR:", http_status(response)$reason, "-", error_details))
  }

  result <- tryCatch({
    content(response, as = "parsed", type = "application/json", encoding = "UTF-8")
  }, error = function(e){
    warning(paste("Error parsing HuggingFace response JSON:", conditionMessage(e)))
    return(NULL)
  })

  if (is.null(result)) {
    raw_content <- content(response, "text", encoding = "UTF-8")
    return(paste("API_RESPONSE_ERRORR: Failed to parse JSON response. Raw content:", substr(raw_content, 1, 500)))
  }

  # Check for tool calls (OpenAI format)
  message_content <- result$choices[[1]]$message
  function_result <- NULL

  if (!is.null(message_content$tool_calls)) {
    if (tools) {
      function_result <- result
    } else {
      warning("HuggingFace API unexpectedly returned 'tool_calls' (tools=FALSE).")
    }
  }

  # Return function result or text content
  if (!is.null(function_result)) {
    if (is.character(function_result) && startsWith(function_result, "TOOL_CALL_ERRORR:")) {
      return(function_result)
    }
    if (inherits(function_result, "htmlwidget")) { return(as.character(function_result)) } else { return(function_result) }
  } else if (!is.null(message_content$content)) {
    return(message_content$content)
    # Check finish reason (OpenAI format)
  } else if (!is.null(result$choices[[1]]$finish_reason) && result$choices[[1]]$finish_reason == "length") {
    warning("HuggingFace TGI response truncated (finish_reason: length).")
    # Return empty string or error? Let's return error if content is null.
    return("API_RESPONSE_ERRORR: Max tokens reached (finish_reason: length), but no content found.")
  } else if (!is.null(result$choices[[1]]$finish_reason) && result$choices[[1]]$finish_reason == "tool_calls") {
    warning("HuggingFace TGI API finished with 'tool_calls' but no valid tool call processed.")
    return("API_RESPONSE_ERRORR: Tool call expected but not processed correctly.")
  } else {
    warning("Unexpected HuggingFace TGI API response. Finish reason: ", result$choices[[1]]$finish_reason)
    return("API_RESPONSE_ERRORR: No valid content or tool_calls found.")
  }
}

#' Generate text via multiple providers
#'
#' High-level helper to send a prompt (and optional additional content and image)
#' to a selected provider/model using a unified interface. Handles timeouts,
#' retries on empty results, basic token estimation, and optional persistence of
#' requests and responses through `.save_response`.
#'
#' @param context Character or object; main prompt or context. Non-character values
#'   are stringified.
#' @param res_context Logical; whether to persist the original `context` alongside
#'   the response when saving.
#' @param add Optional; additional content to append to `context`. Accepts character
#'   strings, file paths (`.txt`/`.csv`), data frames, matrices, or lists.
#' @param add_img Optional character; path to an image file to include (for
#'   multimodal-capable models).
#' @param directory Character; directory to save responses and metadata. Defaults
#'   to `tools::R_user_dir("agent_models", which = "data")` when `NULL`.
#' @param label Optional character; label for the saved response. Defaults to a
#'   sanitized derivation of `context`.
#' @param service Character; provider identifier (e.g. `"openai"`,,
#'   `"openrouter"`, `"hf"`, etc.).
#' @param model Character; model identifier for the chosen `service`.
#' @param temp Optional numeric; sampling temperature. If `NULL`, defaults to 0.7.
#' @param tools Logical; whether to enable tool/function calling for providers
#'   that support it.
#' @param my_tools Function; your function with tools definitions.
#' @param timeout_api Numeric; request timeout (seconds) passed to the provider
#'   call.
#' @param null_repeat Logical; if `TRUE`, retries on empty responses with
#'   progressive waits (10s, 60s, 600s).
#' @param ... Additional arguments passed to method-specific implementations.
#' @return A list with elements: `response_value`, `label`, `label_cat`,
#'   `service`, `model`, `temp`, `duration`, `status_api`, `status_msg`,
#'   `tokens_sent`, `tokens_received`.
#' @examples
#' \dontrun{
#' gen_txt(
#'   context = "Summarize this paragraph:",
#'   add = "Large language models can help with many tasks...",
#'   service = "openai",
#'   model = "gpt-4o-mini",
#'   temp = 0.3
#' )
#' }
#' @export
gen_txt <- function(context, ...) {
  UseMethod("gen_txt")
}

#' @rdname gen_txt
#' @method gen_txt default
#' @export
gen_txt.default <- function(
    context,
    res_context = TRUE,
    add = NULL,
    add_img = NULL,
    directory = NULL,
    label = NULL,
    service = "openai",
    model = "gpt-5-mini",
    temp = 1,
    tools = FALSE,
    my_tools = NULL,
    timeout_api = 240,
    null_repeat = TRUE
) {
  # Helpers
  is_emptyish <- function(x) {
    if (is.null(x)) return(TRUE)
    if (length(x) == 0) return(TRUE)
    if (is.character(x)) return(nchar(trimws(paste(x, collapse = ""))) == 0)
    FALSE
  }

  # Ensure output directory exists
  if (is.null(directory) || is.na(directory)) {
    directory <- tools::R_user_dir("agent_models", which = "data")
  }

  # Normalize inputs possibly coming as lists/vectors
  if (is.list(service)) service <- as.character(service$service %||% service[[1]]) else if (is.vector(service)) service <- as.character(service[1])
  if (is.list(model))  model  <- as.character(model$model %||% model$model %||% model[[1]]) else if (is.vector(model)) model <- as.character(model[1])
  if (is.list(temp))    temp    <- as.numeric(temp$temperature %||% temp$temp %||% temp[[1]]) else if (is.vector(temp)) temp <- as.numeric(temp[1])
  temp_v <- ifelse(is.null(temp) || !is.numeric(temp) || is.na(temp), 0.7, temp)

  # Build prompt and estimate tokens (supports strings, files, data frames, nested lists)
  process_add <- function(context, add) {
    if (is.null(add)) {
      prompt <- context
      tokens <- tryCatch(.estimate_tokens(prompt), error = function(e) NA_integer_)
      return(list(prompt = prompt, tokens = tokens))
    }

    if (!is.list(add)) add <- as.list(add)
    full_text <- ""
    for (item in add) {
      item_text <- ""
      if (is.character(item) && length(item) == 1 && file.exists(item)) {
        ext <- tools::file_ext(item)
        if (ext %in% c("txt", "csv")) {
          if (ext == "txt") {
            txt <- readLines(item, encoding = "UTF-8", warn = FALSE)
            item_text <- paste(txt, collapse = "\n")
          } else {
            df <- read.csv(item, stringsAsFactors = FALSE)
            item_text <- paste(capture.output(print(df)), collapse = "\n")
          }
        } else {
          stop("Unsupported file format. Use .txt or .csv.")
        }
      } else if (is.character(item)) {
        item_text <- item
      } else if (is.data.frame(item) || is.matrix(item)) {
        item_text <- paste(capture.output(print(item)), collapse = "\n")
      } else if (is.list(item)) {
        item_text <- paste(unlist(rapply(item, function(x) as.character(x))), collapse = "\n")
      } else {
        item_text <- as.character(item)
      }
      full_text <- paste(full_text, item_text, sep = "\n")
    }

    prompt <- paste(context, "\n\nText:\n", full_text)
    tokens <- tryCatch(.estimate_tokens(prompt), error = function(e) NA_integer_)
    list(prompt = prompt, tokens = tokens)
  }

  info <- process_add(context, add)
  prompt <- info$prompt
  tokens_sent <- info$tokens

  # Wrapper that dispatches to provider-specific functions (kept consistent and fixes 'fal' call)
  .do_call <- function(service, prompt, model, temp_v, add_img, tools, my_tools, timeout_api) {
    switch(
      tolower(service),
      "openai"      = .gen_txt_openai(prompt, model, temp_v, add_img, tools = tools, my_tools = my_tools, timeout_secs = timeout_api),
    #  "gemini"      = .gen_txt_gemini(prompt, model, temp_v, add_img, tools = tools, timeout_secs = timeout_api),
    #  "geminicheck" = gen_txt_geminiCHECK(prompt, model, temp_v, add_img, tools = tools, timeout_secs = timeout_api),
    #  "vertexai"    = gen_txt_vertexai(prompt, model, temp_v, timeout_secs = timeout_api), # safest signature
      "openrouter"  = .gen_txt_openrouter(prompt, model, temp_v, add_img, tools = tools, my_tools = my_tools, timeout_secs = timeout_api),
    #  "claude"      = gen_txt_claude(prompt, model, temp_v, add_img, tools = tools, timeout_secs = timeout_api),
    #  "azure"       = gen_txt_azure(prompt, model, temp_v, add_img, tools = tools, timeout_secs = timeout_api),
    #  "mistral"     = gen_txt_mistral(prompt, model, temp_v, add_img, tools = tools, timeout_secs = timeout_api),
    #  "oracle"      = gen_txt_oracle(prompt, model, temp_v, timeout_secs = timeout_api),
    #  "groq"        = gen_txt_groq(prompt, model, temp_v, add_img, tools = tools, timeout_secs = timeout_api),
     # "zhipu"       = gen_txt_zhipu(prompt, model, temp_v, add_img, tools = tools, timeout_secs = timeout_api),
      "hf" = .gen_txt_hf(prompt, model, temp_v, add_img, tools = tools, my_tools = my_tools, timeout_secs = timeout_api),
  #    "cohere"      = gen_txt_cohere(prompt, model, temp_v, add_img, tools = tools, timeout_secs = timeout_api),
   #   "grok"        = gen_txt_grok(prompt, model, temp_v, add_img, tools = tools, timeout_secs = timeout_api),
    #  "nebius"      = gen_txt_nebius(prompt, model, temp_v, add_img, tools = tools, timeout_secs = timeout_api),
     # "sambanova"   = gen_txt_sambanova(prompt, model, temp_v, add_img, tools = tools, timeout_secs = timeout_api),
  #    "cerebras"    = gen_txt_cerebras(prompt, model, temp_v, add_img, tools = tools, timeout_secs = timeout_api),
   #   "fal"         = gen_txt_fal(prompt, model, temp_v = temp_v, add_img = add_img, timeout_secs = timeout_api), # fixed
    #  "hyperbolic"  = gen_txt_hyperbolic(prompt, model, temp_v, add_img, tools = tools, timeout_secs = timeout_api),
  #    "deepinfra"   = gen_txt_deepinfra(prompt, model, temp_v, add_img, tools = tools, timeout_secs = timeout_api),
   #   "fireworks"   = gen_txt_fireworks(prompt, model, temp_v, add_img, tools = tools, timeout_secs = timeout_api),
    #  "perplexity"  = gen_txt_perplexity(prompt, model, temp_v, tools = tools, timeout_secs = timeout_api),
    #  "deepseek"    = gen_txt_deepseek(prompt, model, temp_v, add_img, tools = tools, timeout_secs = timeout_api),
    #  "together"    = gen_txt_together(prompt, model, temp_v, add_img, tools = tools, timeout_secs = timeout_api),
      paste0("SERVICE_NOT_IMPLEMENTED: ", service)
    )
  }

  # Call provider with timing
  sent_at <- Sys.time()
  api_call_error <- NULL

  response_api <- tryCatch(
    .do_call(service, prompt, model, temp_v, add_img, tools, my_tools, timeout_api),
    error = function(e) {
      api_call_error <<- paste("Error during API call execution:", conditionMessage(e))
      api_call_error
    }
  )

  # Optional retry on empty/null (or sentinel value)
  if (isTRUE(null_repeat) && (identical(response_api, "EMPTY_OR_NULL_RESPONSE") || is_emptyish(response_api))) {
    for (wait_sec in c(10, 60, 600)) {
      message(sprintf("Empty response detected; waiting %ds and trying again...", wait_sec))
      Sys.sleep(wait_sec)
      response_api <- tryCatch(
        .do_call(service, prompt, model, temp_v, add_img, tools, my_tools, timeout_api),
        error = function(e) paste("Error during API call execution:", conditionMessage(e))
      )
      if (!(identical(response_api, "EMPTY_OR_NULL_RESPONSE") || is_emptyish(response_api))) {
        message("Received a non-empty response on retry. Proceeding.")
        break
      }
    }
    if (identical(response_api, "EMPTY_OR_NULL_RESPONSE") || is_emptyish(response_api)) {
      stop("EMPTY_OR_NULL_RESPONSE persisted after retries (0s, 10s, 60s, 600s). Aborting.")
    }
  }

  received_at <- Sys.time()
  duration_response <- as.numeric(difftime(received_at, sent_at, units = "secs"))

  # Classify response and compute received token estimate
  tokens_received <- NA_integer_
  houve_erro_api <- FALSE
  status_msg <- "OK"
  response_value_final <- NULL

  if (!is.null(api_call_error)) {
    houve_erro_api <- TRUE
    status_msg <- api_call_error
    response_value_final <- status_msg
  } else if (is.character(response_api) &&
             (startsWith(response_api, "TIMEOUT_ERRORR_HTTR:") ||
              startsWith(response_api, "HTTR_ERRORR:") ||
              startsWith(response_api, "API_ERRORR:") ||
              startsWith(response_api, "CONTENT_FILTERED:") ||
              startsWith(response_api, "PROMPT_BLOCKED:") ||
              startsWith(response_api, "SERVICE_NOT_IMPLEMENTED:") ||
              startsWith(response_api, "API_RESPONSE_ERRORR:") ||
              startsWith(response_api, "FUNCTION_CALL_ERRORR:") ||
              startsWith(response_api, "TOOL_CALL_ERRORR:"))) {
    houve_erro_api <- TRUE
    status_msg <- response_api
    response_value_final <- status_msg
  } else if (is_emptyish(response_api)) {
    houve_erro_api <- TRUE
    status_msg <- "EMPTY_OR_NULL_RESPONSE"
    response_value_final <- status_msg
  } else {
    response_value_final <- response_api
    if (is.character(response_value_final)) {
      tokens_received <- tryCatch(.estimate_tokens(response_value_final), error = function(e) NA_integer_)
    } else {
      tokens_received <- tryCatch(
        .estimate_tokens(paste(capture.output(str(response_value_final)), collapse = "\n")),
        error = function(e) NA_integer_
      )
    }
    status_msg <- "OK"
  }

  # Define labels
  label_base <- label
  if (is.null(label_base)) {
    label_base <- deparse(substitute(context))
    label_base <- substr(gsub("[^A-Za-z0-9_.-]", "_", label_base), 1, 50)
  }
  label_cat <- label_base

  # Persist response (assumes .save_response exists and handles logging/saving)
  try({
    .save_response(
      response_api     = response_value_final,
      context         = context,
      res_context     = res_context,
      label            = label_base,
      label_cat        = label_cat,
      service          = service,
      model           = model,
      temp             = temp_v,
      duration_response   = duration_response,
      directory        = directory,
      status           = ifelse(houve_erro_api, "ERROR", "SUCCESS"),
      tokens_sent  = tokens_sent,
      tokens_received = tokens_received %||% NA_integer_
    )
  }, silent = TRUE)

  # Final structured return
  result <- list(
    response_value   = response_value_final,
    label            = label_base,
    label_cat        = label_cat,
    service          = service,
    model           = model,
    temp             = temp_v,
    duration            = duration_response,
    status_api       = ifelse(houve_erro_api, "ERROR", "SUCCESS"),
    status_msg  = status_msg,
    tokens_sent  = tokens_sent,
    tokens_received = tokens_received
  )
  return(result)
}

#' @rdname gen_txt
#' @method gen_txt genflow_agent
#' @export
gen_txt.genflow_agent <- function(context, ...) {
  agent <- context
  overrides <- list(...)
  formals_default <- formals(gen_txt.default)
  agent_args <- .genflow_prepare_agent_args(
    agent = agent,
    overrides = overrides,
    target_formals = formals_default,
    required = "context"
  )
  do.call(gen_txt.default, agent_args, quote = TRUE)
}
