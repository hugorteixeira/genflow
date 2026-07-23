.text_tools_contains_function <- function(value) {
  if (is.function(value)) {
    return(TRUE)
  }
  if (!is.list(value) || !length(value)) {
    return(FALSE)
  }
  any(vapply(value, .text_tools_contains_function, logical(1)))
}

.text_normalize_tools_payload <- function(value, argument = "`my_tools`") {
  if (is.function(value)) {
    value <- tryCatch(
      value(),
      error = function(e) {
        stop(
          argument,
          " failed while building tool definitions: ",
          conditionMessage(e),
          call. = FALSE
        )
      }
    )
  }
  if (is.character(value) && length(value) == 1L) {
    value <- tryCatch(
      jsonlite::fromJSON(value, simplifyVector = FALSE),
      error = function(e) {
        stop(
          argument,
          " must return a list or valid JSON tool definitions: ",
          conditionMessage(e),
          call. = FALSE
        )
      }
    )
  }
  if (is.null(value)) {
    return(NULL)
  }
  if (!is.list(value)) {
    stop(argument, " must contain tool definitions as a list or valid JSON.", call. = FALSE)
  }
  if (.text_tools_contains_function(value)) {
    stop(
      argument,
      " contains an R function. Tool payloads must contain serializable schema data only.",
      call. = FALSE
    )
  }
  value
}

.text_retry_sleep <- function(seconds) {
  Sys.sleep(seconds)
}

.text_is_hf_loading <- function(value) {
  is.character(value) &&
    length(value) == 1L &&
    startsWith(value, "HF_MODEL_LOADING:")
}

.HF_TEXT_DEFAULT_MODEL <- "mistralai/Mixtral-8x7B-Instruct-v0.1"

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
.gen_txt_openai <- function(prompt,
                            model,
                            temp_v,
                            reasoning,
                            add_img,
                            tools = FALSE,
                            my_tools = NULL,
                            plugins = NULL,
                            timeout_secs = 80) {
  use_responses <- !is.null(reasoning)
  api_url <- if (use_responses) {
    "https://api.openai.com/v1/responses"
  } else {
    "https://api.openai.com/v1/chat/completions"
  }
  if (is.null(model)) {
    model <- "gpt-5-mini"
  }
  api_key <- Sys.getenv("OPENAI_API_KEY")
  if (is.null(api_key) || api_key == "") {
    stop("Environment variable OPENAI_API_KEY not set.")
  }
  header_args <- list(
    Authorization = paste("Bearer", api_key),
    "Content-Type" = "application/json"
  )
  if (!is.null(reasoning)) {
    beta_header <- Sys.getenv("OPENAI_BETA_HEADER", "")
    if (!nzchar(beta_header)) {
      beta_header <- "reasoning=1"
    }
    header_args[["OpenAI-Beta"]] <- beta_header
  }
  headers <- do.call(httr::add_headers, header_args)
  # Build the user message, including an inline image when requested.
  initial_message <- list(role = "user", content = if (!is.null(add_img)) {
    list(list(type = "text", text = prompt), list(type = "image_url", image_url = list(url = paste0("data:image/jpeg;base64,", .encode_image(add_img)))))
  } else {
    prompt
  })
  # Build the endpoint-specific request body.
  if (grepl("^o", model)) {
    temp_v <- 1
  }
  tools_payload <- if (isTRUE(tools)) {
    .text_normalize_tools_payload(my_tools)
  } else {
    NULL
  }
  if (use_responses) {
    if (!is.null(add_img)) {
      warning("Image attachments are not currently supported with reasoning-enabled OpenAI responses; ignoring `add_img`.")
    }
    input_payload <- list(
      list(
        role = "user",
        content = list(
          list(type = "input_text", text = prompt)
        )
      )
    )
    body <- list(
      model = model,
      input = input_payload,
      temperature = temp_v,
      reasoning = list(effort = reasoning)
    )
    if (isTRUE(tools)) {
      warning("OpenAI reasoning via the responses endpoint currently ignores `tools`.")
    }
  } else {
    body <- list(
      model = model,
      messages = list(initial_message)
    )
    if (isTRUE(tools)) {
      if (is.null(tools_payload)) {
        stop("`tools = TRUE` requires tool definitions in `my_tools`.", call. = FALSE)
      }
      body$tools <- tools_payload
      body$tool_choice <- "auto"
      body$temperature <- temp_v
    } else if (!grepl("search", model)) {
      body$temperature <- temp_v
    }
    if (!is.null(reasoning)) {
      body$reasoning <- list(effort = reasoning)
    }
  }


  # --- API call with httr::timeout() and tryCatch ---
  response <- tryCatch(
    {
      httr::POST(
        url = api_url,
        headers,
        body = toJSON(body, auto_unbox = TRUE, null = "null"),
        encode = "json",
        config = httr::timeout(timeout_secs)
      )
    },
    error = function(e) {
      err_msg <- conditionMessage(e)
      if (grepl("Timeout was reached|Operation timed out", err_msg, ignore.case = TRUE)) {
        warning(paste("HTTR timeout in .gen_txt_openai after", timeout_secs, "seconds:", err_msg))
        return(paste0("TIMEOUT_ERRORR_HTTR: API call exceeded ", timeout_secs, " seconds."))
      } else {
        warning(paste("HTTR error in .gen_txt_openai:", err_msg))
        return(paste0("HTTR_ERRORR: ", err_msg))
      }
    }
  )
  # --------------------------------------------------

  # Check whether 'response' is an error string
  if (is.character(response) && (startsWith(response, "TIMEOUT_ERRORR_HTTR:") || startsWith(response, "HTTR_ERRORR:"))) {
    return(response)
  }

  # --- Response processing (if no httr error) ---
  if (http_status(response)$category != "Success") {
    error_content <- content(response, "text", encoding = "UTF-8")
    error_msg <- paste("OpenAI API error:", http_status(response)$reason, "-", error_content)
    warning(error_msg)
    error_details <- tryCatch(fromJSON(error_content)$error$message, error = function(e) error_content)
    return(paste("API_ERRORR:", http_status(response)$reason, "-", error_details)) # Keep API error details
  }

  result <- content(response, as = "parsed", type = "application/json", encoding = "UTF-8")
  if (use_responses) {
    if (!is.null(result$output_text)) {
      return(result$output_text)
    }
    if (!is.null(result$output)) {
      extract_segment <- function(seg) {
        if (is.null(seg)) {
          return("")
        }
        if (!is.null(seg$text)) {
          return(seg$text)
        }
        if (!is.null(seg$content)) {
          nested <- vapply(seg$content, function(item) {
            if (!is.null(item$text)) {
              item$text
            } else {
              ""
            }
          }, character(1), USE.NAMES = FALSE)
          return(paste(nested[nzchar(nested)], collapse = ""))
        }
        ""
      }
      pieces <- vapply(result$output, extract_segment, character(1), USE.NAMES = FALSE)
      pieces <- pieces[nzchar(pieces)]
      if (length(pieces) > 0) {
        return(paste(pieces, collapse = "\n"))
      }
    }
    if (!is.null(result$response) && !is.null(result$response$output_text)) {
      return(result$response$output_text)
    }
    warning("Unexpected OpenAI responses payload; returning raw structure.")
    return(result)
  } else {
    message_content <- result$choices[[1]]$message
    function_result <- NULL
    if (!is.null(message_content$tool_calls)) {
      function_result <- result
    } else if (!is.null(message_content$function_call)) {
      function_result <- result
    }

    if (!is.null(function_result)) {
      if (inherits(function_result, "htmlwidget")) {
        return(as.character(function_result))
      } else {
        return(function_result)
      }
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
.gen_txt_openrouter <- function(prompt,
                                model,
                                temp_v,
                                reasoning,
                                add_img,
                                tools = FALSE,
                                my_tools = NULL,
                                plugins = NULL,
                                timeout_secs = 80) {
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
  # Build the user message, including an inline image when requested.
  initial_message <- list(role = "user", content = if (!is.null(add_img)) {
    list(list(type = "text", text = prompt), list(type = "image_url", image_url = list(url = paste0("data:image/jpeg;base64,", .encode_image(add_img)))))
  } else {
    prompt
  })
  # Build the request body and optional tool definitions.
  body <- list(
    model = model,
    messages = list(initial_message),
    temperature = temp_v
  )
  if (tools) {
    tools_payload <- NULL
    if (is.function(my_tools)) {
      tools_payload <- tryCatch(my_tools(), error = function(e) {
        warning("Error while building tools for OpenRouter: ", conditionMessage(e))
        NULL
      })
    } else if (!is.null(my_tools)) {
      tools_payload <- my_tools
    }
    if (!is.null(tools_payload)) {
      body$tools <- tools_payload
      body$tool_choice <- "auto"
    }
  }
  if (!is.null(plugins)) {
    body$plugins <- plugins
  }
  if (!is.null(reasoning)) {
    body$reasoning <- list(effort = reasoning)
  }


  # --- API call with httr::timeout() and tryCatch ---
  response <- tryCatch(
    {
      httr::POST(
        url = api_url,
        headers,
        body = toJSON(body, auto_unbox = TRUE, null = "null"),
        encode = "json",
        config = httr::timeout(timeout_secs)
      )
    },
    error = function(e) {
      err_msg <- conditionMessage(e)
      if (grepl("Timeout was reached|Operation timed out", err_msg, ignore.case = TRUE)) {
        warning(paste("HTTR timeout in .gen_txt_openrouter after", timeout_secs, "seconds:", err_msg))
        return(paste0("TIMEOUT_ERRORR_HTTR: API call exceeded ", timeout_secs, " seconds."))
      } else {
        warning(paste("HTTR error in .gen_txt_openrouter:", err_msg))
        return(paste0("HTTR_ERRORR: ", err_msg))
      }
    }
  )
  # --------------------------------------------------

  # Check whether 'response' is an error string
  if (is.character(response) && (startsWith(response, "TIMEOUT_ERRORR_HTTR:") || startsWith(response, "HTTR_ERRORR:"))) {
    return(response)
  }

  # --- Response processing (if no httr error) ---
  # ... (check http_status, content, result$choices, tool_calls, content_filter) ...
  if (http_status(response)$category != "Success") {
    error_content <- content(response, "text", encoding = "UTF-8")
    error_msg <- paste("OpenRouter API error:", http_status(response)$reason, "-", error_content)
    warning(error_msg)
    error_details <- tryCatch(fromJSON(error_content)$error$message, error = function(e) error_content)
    return(paste("API_ERRORR:", http_status(response)$reason, "-", error_details)) # Keep API error details
  }
  result <- content(response, as = "parsed", type = "application/json", encoding = "UTF-8")
  message_content <- result$choices[[1]]$message
  function_result <- NULL
  if (!is.null(message_content$tool_calls)) {
    function_result <- result
  }
  if (!is.null(function_result)) {
    if (inherits(function_result, "htmlwidget")) {
      return(as.character(function_result))
    } else {
      return(function_result)
    }
  } else if (!is.null(message_content$content)) {
    return(message_content$content)
  } else if (!is.null(result$choices[[1]]$finish_reason) && result$choices[[1]]$finish_reason == "content_filter") {
    warning("Content blocked by filter (OpenRouter/model).")
    return("CONTENT_FILTERED: Response blocked by content filter.")
  } else {
    warning("Unexpected OpenRouter API response. Finish reason: ", result$choices[[1]]$finish_reason)
    return("API_RESPONSE_ERRORR: No valid content or tool_calls found.")
  }
}

#' @keywords internal
#' @noRd
.groq_base_url <- function() {
  base_url <- Sys.getenv("GROQ_BASE_URL", "https://api.groq.com")
  base_url <- trimws(base_url)
  if (!nzchar(base_url)) {
    base_url <- "https://api.groq.com"
  }
  sub("/+$", "", base_url)
}

#' @keywords internal
#' @noRd
.groq_api_key <- function() {
  trimws(Sys.getenv("GROQ_API_KEY", ""))
}

#' @keywords internal
#' @noRd
.groq_resolve_model <- function(model, timeout_secs = 10) {
  model_chr <- as.character(model %||% "")[1]
  model_chr <- trimws(model_chr)
  if (nzchar(model_chr) && !identical(model_chr, "gpt-5-mini")) {
    return(model_chr)
  }

  model_env <- trimws(Sys.getenv("GROQ_MODEL", ""))
  if (nzchar(model_env)) {
    return(model_env)
  }

  api_key <- .groq_api_key()
  if (!nzchar(api_key)) {
    return("llama-3.3-70b-versatile")
  }

  discovery_timeout <- if (is.numeric(timeout_secs) && length(timeout_secs) == 1 && !is.na(timeout_secs) && timeout_secs > 0) {
    min(timeout_secs, 10)
  } else {
    10
  }

  base_url <- .groq_base_url()
  headers <- httr::add_headers(
    "Content-Type" = "application/json",
    "Authorization" = paste("Bearer", api_key)
  )

  discovered_model <- tryCatch(
    {
      models_response <- httr::GET(
        url = paste0(base_url, "/openai/v1/models"),
        headers,
        httr::timeout(discovery_timeout)
      )
      if (httr::status_code(models_response) != 200) {
        stop(
          "Groq model discovery returned HTTP ",
          httr::status_code(models_response),
          call. = FALSE
        )
      }

      models_content <- httr::content(models_response, as = "parsed", type = "application/json", encoding = "UTF-8")
      models <- models_content$data %||% models_content$models
      model_values <- character()
      if (is.data.frame(models) && nrow(models) > 0) {
        model_values <- as.character(models$id %||% models$model %||% models$name %||% "")
      } else if (is.list(models) && length(models) > 0) {
        model_values <- vapply(models, function(entry) {
          if (!is.list(entry)) {
            return("")
          }
          as.character(entry$id %||% entry$model %||% entry$name %||% "")
        }, character(1), USE.NAMES = FALSE)
      }
      model_values <- unique(trimws(model_values[!is.na(model_values) & nzchar(model_values)]))
      if (length(model_values) == 0) {
        ""
      } else {
        model_values[[1]]
      }
    },
    error = function(e) ""
  )

  if (nzchar(discovered_model)) discovered_model else "llama-3.3-70b-versatile"
}

#' Internal: Groq OpenAI-compatible chat call
#'
#' Calls Groq's OpenAI-compatible chat endpoint and returns textual content,
#' tool-call payload, or an error sentinel string.
#'
#' @keywords internal
#' @noRd
.gen_txt_groq <- function(prompt,
                          model,
                          temp_v,
                          reasoning,
                          add_img,
                          tools = FALSE,
                          my_tools = NULL,
                          plugins = NULL,
                          timeout_secs = 80) {
  base_url <- .groq_base_url()
  api_key <- .groq_api_key()
  if (!nzchar(api_key)) {
    stop("Environment variable GROQ_API_KEY not set.")
  }
  model <- .groq_resolve_model(model, timeout_secs = timeout_secs)
  api_url <- paste0(base_url, "/openai/v1/chat/completions")

  if (!is.null(reasoning)) {
    warning("`reasoning` is currently ignored for service = 'groq'.")
  }
  if (!is.null(plugins)) {
    warning("`plugins` is currently ignored for service = 'groq'.")
  }

  initial_message <- list(role = "user", content = if (!is.null(add_img)) {
    list(
      list(type = "text", text = prompt),
      list(type = "image_url", image_url = list(url = paste0("data:image/jpeg;base64,", .encode_image(add_img))))
    )
  } else {
    prompt
  })

  body <- list(
    model = model,
    messages = list(initial_message),
    temperature = temp_v
  )
  if (isTRUE(tools)) {
    tools_payload <- NULL
    if (is.function(my_tools)) {
      tools_payload <- tryCatch(my_tools(), error = function(e) {
        warning("Error while building tools for Groq: ", conditionMessage(e))
        NULL
      })
    } else if (!is.null(my_tools)) {
      tools_payload <- my_tools
    }
    if (!is.null(tools_payload)) {
      body$tools <- tools_payload
      body$tool_choice <- "auto"
    }
  }

  headers <- httr::add_headers(
    "Content-Type" = "application/json",
    "Authorization" = paste("Bearer", api_key)
  )

  response <- tryCatch(
    {
      httr::POST(
        url = api_url,
        headers,
        body = toJSON(body, auto_unbox = TRUE, null = "null"),
        encode = "json",
        config = httr::timeout(timeout_secs)
      )
    },
    error = function(e) {
      err_msg <- conditionMessage(e)
      if (grepl("Timeout was reached|Operation timed out", err_msg, ignore.case = TRUE)) {
        warning(paste("HTTR timeout in .gen_txt_groq after", timeout_secs, "seconds:", err_msg))
        return(paste0("TIMEOUT_ERRORR_HTTR: API call exceeded ", timeout_secs, " seconds."))
      }
      warning(paste("HTTR error in .gen_txt_groq:", err_msg))
      paste0("HTTR_ERRORR: ", err_msg)
    }
  )

  if (is.character(response) && (startsWith(response, "TIMEOUT_ERRORR_HTTR:") || startsWith(response, "HTTR_ERRORR:"))) {
    return(response)
  }

  if (http_status(response)$category != "Success") {
    error_content <- content(response, "text", encoding = "UTF-8")
    error_details <- tryCatch(
      {
        parsed <- fromJSON(error_content)
        parsed$error$message %||% parsed$error %||% parsed$message %||% error_content
      },
      error = function(e) error_content
    )
    return(paste("API_ERRORR:", http_status(response)$reason, "-", error_details))
  }

  result <- tryCatch(
    {
      content(response, as = "parsed", type = "application/json", encoding = "UTF-8")
    },
    error = function(e) {
      warning("Failed to parse Groq response JSON: ", conditionMessage(e))
      NULL
    }
  )

  if (is.null(result)) {
    return("API_RESPONSE_ERRORR: Invalid JSON returned by Groq.")
  }

  first_choice <- NULL
  if (is.list(result$choices) && length(result$choices) >= 1) {
    first_choice <- result$choices[[1]]
  }

  message_content <- first_choice$message %||% result$message
  function_result <- NULL
  if (!is.null(message_content$tool_calls) || !is.null(message_content$function_call)) {
    function_result <- result
  }

  if (!is.null(function_result)) {
    if (inherits(function_result, "htmlwidget")) {
      return(as.character(function_result))
    }
    return(function_result)
  }

  response_text <- message_content$content %||% first_choice$text %||% result$content %||% result$response
  if (!is.null(response_text) && nzchar(trimws(paste(response_text, collapse = "")))) {
    return(response_text)
  }

  finish_reason <- first_choice$finish_reason %||% ""
  if (identical(finish_reason, "content_filter")) {
    warning("Content blocked by filter (Groq/model).")
    return("CONTENT_FILTERED: Response blocked by content filter.")
  }

  warning("Unexpected Groq API response. Finish reason: ", finish_reason)
  "API_RESPONSE_ERRORR: No valid content, tool_calls or function_call found."
}

#' @keywords internal
#' @noRd
.cerebras_base_url <- function() {
  base_url <- Sys.getenv("CEREBRAS_BASE_URL", "https://api.cerebras.ai")
  base_url <- trimws(base_url)
  if (!nzchar(base_url)) {
    base_url <- "https://api.cerebras.ai"
  }
  sub("/+$", "", base_url)
}

#' @keywords internal
#' @noRd
.cerebras_api_key <- function() {
  trimws(Sys.getenv("CEREBRAS_API_KEY", ""))
}

#' @keywords internal
#' @noRd
.cerebras_resolve_model <- function(model, timeout_secs = 10) {
  model_chr <- as.character(model %||% "")[1]
  model_chr <- trimws(model_chr)
  if (nzchar(model_chr) && !identical(model_chr, "gpt-5-mini")) {
    return(model_chr)
  }

  model_env <- trimws(Sys.getenv("CEREBRAS_MODEL", ""))
  if (nzchar(model_env)) {
    return(model_env)
  }

  api_key <- .cerebras_api_key()
  if (!nzchar(api_key)) {
    return("llama-3.3-70b")
  }

  discovery_timeout <- if (is.numeric(timeout_secs) && length(timeout_secs) == 1 && !is.na(timeout_secs) && timeout_secs > 0) {
    min(timeout_secs, 10)
  } else {
    10
  }

  base_url <- .cerebras_base_url()
  headers <- httr::add_headers(
    "Content-Type" = "application/json",
    "Authorization" = paste("Bearer", api_key)
  )

  discovered_model <- tryCatch(
    {
      models_response <- httr::GET(
        url = paste0(base_url, "/v1/models"),
        headers,
        httr::timeout(discovery_timeout)
      )
      if (httr::status_code(models_response) != 200) {
        stop(
          "Cerebras model discovery returned HTTP ",
          httr::status_code(models_response),
          call. = FALSE
        )
      }

      models_content <- httr::content(models_response, as = "parsed", type = "application/json", encoding = "UTF-8")
      models <- models_content$data %||% models_content$models
      model_values <- character()
      if (is.data.frame(models) && nrow(models) > 0) {
        model_values <- as.character(models$id %||% models$model %||% models$name %||% "")
      } else if (is.list(models) && length(models) > 0) {
        model_values <- vapply(models, function(entry) {
          if (!is.list(entry)) {
            return("")
          }
          as.character(entry$id %||% entry$model %||% entry$name %||% "")
        }, character(1), USE.NAMES = FALSE)
      }
      model_values <- unique(trimws(model_values[!is.na(model_values) & nzchar(model_values)]))
      if (length(model_values) == 0) {
        ""
      } else {
        model_values[[1]]
      }
    },
    error = function(e) ""
  )

  if (nzchar(discovered_model)) discovered_model else "llama-3.3-70b"
}

#' Internal: Cerebras OpenAI-compatible chat call
#'
#' Calls Cerebras' OpenAI-compatible chat endpoint and returns textual content,
#' tool-call payload, or an error sentinel string.
#'
#' @keywords internal
#' @noRd
.gen_txt_cerebras <- function(prompt,
                              model,
                              temp_v,
                              reasoning,
                              add_img,
                              tools = FALSE,
                              my_tools = NULL,
                              plugins = NULL,
                              timeout_secs = 80) {
  base_url <- .cerebras_base_url()
  api_key <- .cerebras_api_key()
  if (!nzchar(api_key)) {
    stop("Environment variable CEREBRAS_API_KEY not set.")
  }
  model <- .cerebras_resolve_model(model, timeout_secs = timeout_secs)
  api_url <- paste0(base_url, "/v1/chat/completions")

  if (!is.null(reasoning)) {
    warning("`reasoning` is currently ignored for service = 'cerebras'.")
  }
  if (!is.null(plugins)) {
    warning("`plugins` is currently ignored for service = 'cerebras'.")
  }

  initial_message <- list(role = "user", content = if (!is.null(add_img)) {
    list(
      list(type = "text", text = prompt),
      list(type = "image_url", image_url = list(url = paste0("data:image/jpeg;base64,", .encode_image(add_img))))
    )
  } else {
    prompt
  })

  body <- list(
    model = model,
    messages = list(initial_message),
    temperature = temp_v
  )
  if (isTRUE(tools)) {
    tools_payload <- NULL
    if (is.function(my_tools)) {
      tools_payload <- tryCatch(my_tools(), error = function(e) {
        warning("Error while building tools for Cerebras: ", conditionMessage(e))
        NULL
      })
    } else if (!is.null(my_tools)) {
      tools_payload <- my_tools
    }
    if (!is.null(tools_payload)) {
      body$tools <- tools_payload
      body$tool_choice <- "auto"
    }
  }

  headers <- httr::add_headers(
    "Content-Type" = "application/json",
    "Authorization" = paste("Bearer", api_key)
  )

  response <- tryCatch(
    {
      httr::POST(
        url = api_url,
        headers,
        body = toJSON(body, auto_unbox = TRUE, null = "null"),
        encode = "json",
        config = httr::timeout(timeout_secs)
      )
    },
    error = function(e) {
      err_msg <- conditionMessage(e)
      if (grepl("Timeout was reached|Operation timed out", err_msg, ignore.case = TRUE)) {
        warning(paste("HTTR timeout in .gen_txt_cerebras after", timeout_secs, "seconds:", err_msg))
        return(paste0("TIMEOUT_ERRORR_HTTR: API call exceeded ", timeout_secs, " seconds."))
      }
      warning(paste("HTTR error in .gen_txt_cerebras:", err_msg))
      paste0("HTTR_ERRORR: ", err_msg)
    }
  )

  if (is.character(response) && (startsWith(response, "TIMEOUT_ERRORR_HTTR:") || startsWith(response, "HTTR_ERRORR:"))) {
    return(response)
  }

  if (http_status(response)$category != "Success") {
    error_content <- content(response, "text", encoding = "UTF-8")
    error_details <- tryCatch(
      {
        parsed <- fromJSON(error_content)
        parsed$error$message %||% parsed$error %||% parsed$message %||% error_content
      },
      error = function(e) error_content
    )
    return(paste("API_ERRORR:", http_status(response)$reason, "-", error_details))
  }

  result <- tryCatch(
    {
      content(response, as = "parsed", type = "application/json", encoding = "UTF-8")
    },
    error = function(e) {
      warning("Failed to parse Cerebras response JSON: ", conditionMessage(e))
      NULL
    }
  )

  if (is.null(result)) {
    return("API_RESPONSE_ERRORR: Invalid JSON returned by Cerebras.")
  }

  first_choice <- NULL
  if (is.list(result$choices) && length(result$choices) >= 1) {
    first_choice <- result$choices[[1]]
  }

  message_content <- first_choice$message %||% result$message
  function_result <- NULL
  if (!is.null(message_content$tool_calls) || !is.null(message_content$function_call)) {
    function_result <- result
  }

  if (!is.null(function_result)) {
    if (inherits(function_result, "htmlwidget")) {
      return(as.character(function_result))
    }
    return(function_result)
  }

  response_text <- message_content$content %||% first_choice$text %||% result$content %||% result$response
  if (!is.null(response_text) && nzchar(trimws(paste(response_text, collapse = "")))) {
    return(response_text)
  }

  finish_reason <- first_choice$finish_reason %||% ""
  if (identical(finish_reason, "content_filter")) {
    warning("Content blocked by filter (Cerebras/model).")
    return("CONTENT_FILTERED: Response blocked by content filter.")
  }

  warning("Unexpected Cerebras API response. Finish reason: ", finish_reason)
  "API_RESPONSE_ERRORR: No valid content, tool_calls or function_call found."
}

#' @keywords internal
#' @noRd
.together_base_url <- function() {
  base_url <- Sys.getenv("TOGETHER_BASE_URL", "https://api.together.xyz")
  base_url <- trimws(base_url)
  if (!nzchar(base_url)) {
    base_url <- "https://api.together.xyz"
  }
  sub("/+$", "", base_url)
}

#' @keywords internal
#' @noRd
.together_api_key <- function() {
  trimws(Sys.getenv("TOGETHER_API_KEY", ""))
}

#' @keywords internal
#' @noRd
.together_resolve_model <- function(model, timeout_secs = 10) {
  model_chr <- as.character(model %||% "")[1]
  model_chr <- trimws(model_chr)
  if (nzchar(model_chr) && !identical(model_chr, "gpt-5-mini")) {
    return(model_chr)
  }

  model_env <- trimws(Sys.getenv("TOGETHER_MODEL", ""))
  if (nzchar(model_env)) {
    return(model_env)
  }

  api_key <- .together_api_key()
  if (!nzchar(api_key)) {
    return("meta-llama/Meta-Llama-3.1-8B-Instruct-Turbo")
  }

  discovery_timeout <- if (is.numeric(timeout_secs) && length(timeout_secs) == 1 && !is.na(timeout_secs) && timeout_secs > 0) {
    min(timeout_secs, 10)
  } else {
    10
  }

  base_url <- .together_base_url()
  headers <- httr::add_headers(
    "Content-Type" = "application/json",
    "Authorization" = paste("Bearer", api_key)
  )

  discovered_model <- tryCatch(
    {
      models_response <- httr::GET(
        url = paste0(base_url, "/v1/models"),
        headers,
        httr::timeout(discovery_timeout)
      )
      if (httr::status_code(models_response) != 200) {
        stop(
          "Together model discovery returned HTTP ",
          httr::status_code(models_response),
          call. = FALSE
        )
      }

      models_content <- httr::content(models_response, as = "parsed", type = "application/json", encoding = "UTF-8")
      models <- models_content$data %||% models_content$models
      model_values <- character()
      if (is.data.frame(models) && nrow(models) > 0) {
        model_values <- as.character(models$id %||% models$model %||% models$name %||% "")
      } else if (is.list(models) && length(models) > 0) {
        model_values <- vapply(models, function(entry) {
          if (!is.list(entry)) {
            return("")
          }
          as.character(entry$id %||% entry$model %||% entry$name %||% "")
        }, character(1), USE.NAMES = FALSE)
      }
      model_values <- unique(trimws(model_values[!is.na(model_values) & nzchar(model_values)]))
      if (length(model_values) == 0) {
        ""
      } else {
        model_values[[1]]
      }
    },
    error = function(e) ""
  )

  if (nzchar(discovered_model)) discovered_model else "meta-llama/Meta-Llama-3.1-8B-Instruct-Turbo"
}

#' Internal: Together OpenAI-compatible chat call
#'
#' Calls Together's OpenAI-compatible chat endpoint and returns textual content,
#' tool-call payload, or an error sentinel string.
#'
#' @keywords internal
#' @noRd
.gen_txt_together <- function(prompt,
                              model,
                              temp_v,
                              reasoning,
                              add_img,
                              tools = FALSE,
                              my_tools = NULL,
                              plugins = NULL,
                              timeout_secs = 80) {
  base_url <- .together_base_url()
  api_key <- .together_api_key()
  if (!nzchar(api_key)) {
    stop("Environment variable TOGETHER_API_KEY not set.")
  }
  model <- .together_resolve_model(model, timeout_secs = timeout_secs)
  api_url <- paste0(base_url, "/v1/chat/completions")

  if (!is.null(reasoning)) {
    warning("`reasoning` is currently ignored for service = 'together'.")
  }
  if (!is.null(plugins)) {
    warning("`plugins` is currently ignored for service = 'together'.")
  }

  initial_message <- list(role = "user", content = if (!is.null(add_img)) {
    list(
      list(type = "text", text = prompt),
      list(type = "image_url", image_url = list(url = paste0("data:image/jpeg;base64,", .encode_image(add_img))))
    )
  } else {
    prompt
  })

  body <- list(
    model = model,
    messages = list(initial_message),
    temperature = temp_v
  )
  if (isTRUE(tools)) {
    tools_payload <- NULL
    if (is.function(my_tools)) {
      tools_payload <- tryCatch(my_tools(), error = function(e) {
        warning("Error while building tools for Together: ", conditionMessage(e))
        NULL
      })
    } else if (!is.null(my_tools)) {
      tools_payload <- my_tools
    }
    if (!is.null(tools_payload)) {
      body$tools <- tools_payload
      body$tool_choice <- "auto"
    }
  }

  headers <- httr::add_headers(
    "Content-Type" = "application/json",
    "Authorization" = paste("Bearer", api_key)
  )

  response <- tryCatch(
    {
      httr::POST(
        url = api_url,
        headers,
        body = toJSON(body, auto_unbox = TRUE, null = "null"),
        encode = "json",
        config = httr::timeout(timeout_secs)
      )
    },
    error = function(e) {
      err_msg <- conditionMessage(e)
      if (grepl("Timeout was reached|Operation timed out", err_msg, ignore.case = TRUE)) {
        warning(paste("HTTR timeout in .gen_txt_together after", timeout_secs, "seconds:", err_msg))
        return(paste0("TIMEOUT_ERRORR_HTTR: API call exceeded ", timeout_secs, " seconds."))
      }
      warning(paste("HTTR error in .gen_txt_together:", err_msg))
      paste0("HTTR_ERRORR: ", err_msg)
    }
  )

  if (is.character(response) && (startsWith(response, "TIMEOUT_ERRORR_HTTR:") || startsWith(response, "HTTR_ERRORR:"))) {
    return(response)
  }

  if (http_status(response)$category != "Success") {
    error_content <- content(response, "text", encoding = "UTF-8")
    error_details <- tryCatch(
      {
        parsed <- fromJSON(error_content)
        parsed$error$message %||% parsed$error %||% parsed$message %||% error_content
      },
      error = function(e) error_content
    )
    return(paste("API_ERRORR:", http_status(response)$reason, "-", error_details))
  }

  result <- tryCatch(
    {
      content(response, as = "parsed", type = "application/json", encoding = "UTF-8")
    },
    error = function(e) {
      warning("Failed to parse Together response JSON: ", conditionMessage(e))
      NULL
    }
  )

  if (is.null(result)) {
    return("API_RESPONSE_ERRORR: Invalid JSON returned by Together.")
  }

  first_choice <- NULL
  if (is.list(result$choices) && length(result$choices) >= 1) {
    first_choice <- result$choices[[1]]
  }

  message_content <- first_choice$message %||% result$message
  function_result <- NULL
  if (!is.null(message_content$tool_calls) || !is.null(message_content$function_call)) {
    function_result <- result
  }

  if (!is.null(function_result)) {
    if (inherits(function_result, "htmlwidget")) {
      return(as.character(function_result))
    }
    return(function_result)
  }

  response_text <- message_content$content %||% first_choice$text %||% result$content %||% result$response
  if (!is.null(response_text) && nzchar(trimws(paste(response_text, collapse = "")))) {
    return(response_text)
  }

  finish_reason <- first_choice$finish_reason %||% ""
  if (identical(finish_reason, "content_filter")) {
    warning("Content blocked by filter (Together/model).")
    return("CONTENT_FILTERED: Response blocked by content filter.")
  }

  warning("Unexpected Together API response. Finish reason: ", finish_reason)
  "API_RESPONSE_ERRORR: No valid content, tool_calls or function_call found."
}

#' @keywords internal
#' @noRd
.sambanova_base_url <- function() {
  base_url <- Sys.getenv("SAMBANOVA_BASE_URL", "https://api.sambanova.ai")
  base_url <- trimws(base_url)
  if (!nzchar(base_url)) {
    base_url <- "https://api.sambanova.ai"
  }
  sub("/+$", "", base_url)
}

#' @keywords internal
#' @noRd
.sambanova_api_key <- function() {
  api_key <- trimws(Sys.getenv("SAMBANOVA_API_KEY", ""))
  if (!nzchar(api_key)) {
    api_key <- trimws(Sys.getenv("SAMBA_API_KEY", ""))
  }
  api_key
}

#' @keywords internal
#' @noRd
.sambanova_resolve_model <- function(model, timeout_secs = 10) {
  model_chr <- as.character(model %||% "")[1]
  model_chr <- trimws(model_chr)
  if (nzchar(model_chr) && !identical(model_chr, "gpt-5-mini")) {
    return(model_chr)
  }

  model_env <- trimws(Sys.getenv("SAMBANOVA_MODEL", ""))
  if (!nzchar(model_env)) {
    model_env <- trimws(Sys.getenv("SAMBA_MODEL", ""))
  }
  if (nzchar(model_env)) {
    return(model_env)
  }

  api_key <- .sambanova_api_key()
  if (!nzchar(api_key)) {
    return("Meta-Llama-3.1-8B-Instruct")
  }

  discovery_timeout <- if (is.numeric(timeout_secs) && length(timeout_secs) == 1 && !is.na(timeout_secs) && timeout_secs > 0) {
    min(timeout_secs, 10)
  } else {
    10
  }

  base_url <- .sambanova_base_url()
  headers <- httr::add_headers(
    "Content-Type" = "application/json",
    "Authorization" = paste("Bearer", api_key)
  )

  discovered_model <- tryCatch(
    {
      models_response <- httr::GET(
        url = paste0(base_url, "/v1/models"),
        headers,
        httr::timeout(discovery_timeout)
      )
      if (httr::status_code(models_response) != 200) {
        stop(
          "SambaNova model discovery returned HTTP ",
          httr::status_code(models_response),
          call. = FALSE
        )
      }

      models_content <- httr::content(models_response, as = "parsed", type = "application/json", encoding = "UTF-8")
      models <- models_content$data %||% models_content$models
      model_values <- character()
      if (is.data.frame(models) && nrow(models) > 0) {
        model_values <- as.character(models$id %||% models$model %||% models$name %||% "")
      } else if (is.list(models) && length(models) > 0) {
        model_values <- vapply(models, function(entry) {
          if (!is.list(entry)) {
            return("")
          }
          as.character(entry$id %||% entry$model %||% entry$name %||% "")
        }, character(1), USE.NAMES = FALSE)
      }
      model_values <- unique(trimws(model_values[!is.na(model_values) & nzchar(model_values)]))
      if (length(model_values) == 0) {
        ""
      } else {
        model_values[[1]]
      }
    },
    error = function(e) ""
  )

  if (nzchar(discovered_model)) discovered_model else "Meta-Llama-3.1-8B-Instruct"
}

#' Internal: SambaNova OpenAI-compatible chat call
#'
#' Calls SambaNova's OpenAI-compatible chat endpoint and returns textual content,
#' tool-call payload, or an error sentinel string.
#'
#' @keywords internal
#' @noRd
.gen_txt_sambanova <- function(prompt,
                               model,
                               temp_v,
                               reasoning,
                               add_img,
                               tools = FALSE,
                               my_tools = NULL,
                               plugins = NULL,
                               timeout_secs = 80) {
  base_url <- .sambanova_base_url()
  api_key <- .sambanova_api_key()
  if (!nzchar(api_key)) {
    stop("Environment variable SAMBANOVA_API_KEY not set.")
  }
  model <- .sambanova_resolve_model(model, timeout_secs = timeout_secs)
  api_url <- paste0(base_url, "/v1/chat/completions")

  if (!is.null(reasoning)) {
    warning("`reasoning` is currently ignored for service = 'sambanova'.")
  }
  if (!is.null(plugins)) {
    warning("`plugins` is currently ignored for service = 'sambanova'.")
  }

  initial_message <- list(role = "user", content = if (!is.null(add_img)) {
    list(
      list(type = "text", text = prompt),
      list(type = "image_url", image_url = list(url = paste0("data:image/jpeg;base64,", .encode_image(add_img))))
    )
  } else {
    prompt
  })

  body <- list(
    model = model,
    messages = list(initial_message),
    temperature = temp_v
  )
  if (isTRUE(tools)) {
    tools_payload <- NULL
    if (is.function(my_tools)) {
      tools_payload <- tryCatch(my_tools(), error = function(e) {
        warning("Error while building tools for SambaNova: ", conditionMessage(e))
        NULL
      })
    } else if (!is.null(my_tools)) {
      tools_payload <- my_tools
    }
    if (!is.null(tools_payload)) {
      body$tools <- tools_payload
      body$tool_choice <- "auto"
    }
  }

  headers <- httr::add_headers(
    "Content-Type" = "application/json",
    "Authorization" = paste("Bearer", api_key)
  )

  response <- tryCatch(
    {
      httr::POST(
        url = api_url,
        headers,
        body = toJSON(body, auto_unbox = TRUE, null = "null"),
        encode = "json",
        config = httr::timeout(timeout_secs)
      )
    },
    error = function(e) {
      err_msg <- conditionMessage(e)
      if (grepl("Timeout was reached|Operation timed out", err_msg, ignore.case = TRUE)) {
        warning(paste("HTTR timeout in .gen_txt_sambanova after", timeout_secs, "seconds:", err_msg))
        return(paste0("TIMEOUT_ERRORR_HTTR: API call exceeded ", timeout_secs, " seconds."))
      }
      warning(paste("HTTR error in .gen_txt_sambanova:", err_msg))
      paste0("HTTR_ERRORR: ", err_msg)
    }
  )

  if (is.character(response) && (startsWith(response, "TIMEOUT_ERRORR_HTTR:") || startsWith(response, "HTTR_ERRORR:"))) {
    return(response)
  }

  if (http_status(response)$category != "Success") {
    error_content <- content(response, "text", encoding = "UTF-8")
    error_details <- tryCatch(
      {
        parsed <- fromJSON(error_content)
        parsed$error$message %||% parsed$error %||% parsed$message %||% error_content
      },
      error = function(e) error_content
    )
    return(paste("API_ERRORR:", http_status(response)$reason, "-", error_details))
  }

  result <- tryCatch(
    {
      content(response, as = "parsed", type = "application/json", encoding = "UTF-8")
    },
    error = function(e) {
      warning("Failed to parse SambaNova response JSON: ", conditionMessage(e))
      NULL
    }
  )

  if (is.null(result)) {
    return("API_RESPONSE_ERRORR: Invalid JSON returned by SambaNova.")
  }

  first_choice <- NULL
  if (is.list(result$choices) && length(result$choices) >= 1) {
    first_choice <- result$choices[[1]]
  }

  message_content <- first_choice$message %||% result$message
  function_result <- NULL
  if (!is.null(message_content$tool_calls) || !is.null(message_content$function_call)) {
    function_result <- result
  }

  if (!is.null(function_result)) {
    if (inherits(function_result, "htmlwidget")) {
      return(as.character(function_result))
    }
    return(function_result)
  }

  response_text <- message_content$content %||% first_choice$text %||% result$content %||% result$response
  if (!is.null(response_text) && nzchar(trimws(paste(response_text, collapse = "")))) {
    return(response_text)
  }

  finish_reason <- first_choice$finish_reason %||% ""
  if (identical(finish_reason, "content_filter")) {
    warning("Content blocked by filter (SambaNova/model).")
    return("CONTENT_FILTERED: Response blocked by content filter.")
  }

  warning("Unexpected SambaNova API response. Finish reason: ", finish_reason)
  "API_RESPONSE_ERRORR: No valid content, tool_calls or function_call found."
}

#' @keywords internal
#' @noRd
.openai_compat_resolve_model <- function(model,
                                         timeout_secs = 10,
                                         model_env_vars = character(),
                                         api_key = "",
                                         default_model = "local-model",
                                         base_url,
                                         base_urls = NULL,
                                         model_paths = c("/v1/models", "/models"),
                                         auth_header = "Authorization",
                                         auth_prefix = "Bearer",
                                         extra_headers = NULL,
                                         api_key_required = TRUE,
                                         discover_model = TRUE) {
  model_chr <- as.character(model %||% "")[1]
  model_chr <- trimws(model_chr)
  if (nzchar(model_chr) && !identical(model_chr, "gpt-5-mini")) {
    return(model_chr)
  }

  for (env_var in model_env_vars) {
    if (!nzchar(env_var)) {
      next
    }
    model_env <- trimws(Sys.getenv(env_var, ""))
    if (nzchar(model_env)) {
      return(model_env)
    }
  }

  if (isTRUE(api_key_required) && !nzchar(api_key)) {
    return(default_model)
  }

  if (!isTRUE(discover_model)) {
    return(default_model)
  }

  discovery_timeout <- if (is.numeric(timeout_secs) && length(timeout_secs) == 1 && !is.na(timeout_secs) && timeout_secs > 0) {
    min(timeout_secs, 10)
  } else {
    10
  }

  header_args <- list("Content-Type" = "application/json")
  if (!is.null(extra_headers) && length(extra_headers) > 0) {
    header_args <- c(header_args, extra_headers)
  }
  if (nzchar(auth_header) && nzchar(api_key)) {
    auth_value <- if (nzchar(auth_prefix)) paste(auth_prefix, api_key) else api_key
    header_args[[auth_header]] <- auth_value
  }
  headers <- do.call(httr::add_headers, header_args)

  base_url_candidates <- c(base_url, base_urls %||% character())
  base_url_candidates <- as.character(base_url_candidates)
  base_url_candidates <- trimws(base_url_candidates)
  base_url_candidates <- sub("/+$", "", base_url_candidates)
  base_url_candidates <- base_url_candidates[nzchar(base_url_candidates)]
  if (!length(base_url_candidates)) {
    return(default_model)
  }
  base_url_candidates <- unique(base_url_candidates)

  model_paths <- unique(as.character(model_paths))
  discovered_model <- ""
  for (base_candidate in base_url_candidates) {
    for (model_path in model_paths) {
      if (!nzchar(model_path)) {
        next
      }
      model_url <- if (grepl("^https?://", model_path, ignore.case = TRUE)) {
        model_path
      } else {
        paste0(base_candidate, model_path)
      }

      candidate <- tryCatch(
        {
          models_response <- httr::GET(
            url = model_url,
            headers,
            httr::timeout(discovery_timeout)
          )
          if (httr::status_code(models_response) != 200) {
            stop(
              "OpenAI-compatible model discovery returned HTTP ",
              httr::status_code(models_response),
              call. = FALSE
            )
          }

          models_content <- httr::content(models_response, as = "parsed", type = "application/json", encoding = "UTF-8")
          models <- models_content$data %||% models_content$models %||% models_content$result
          model_values <- character()
          if (is.data.frame(models) && nrow(models) > 0) {
            model_values <- as.character(models$id %||% models$model %||% models$name %||% "")
          } else if (is.list(models) && length(models) > 0) {
            model_values <- vapply(models, function(entry) {
              if (!is.list(entry)) {
                return("")
              }
              as.character(entry$id %||% entry$model %||% entry$name %||% "")
            }, character(1), USE.NAMES = FALSE)
          }
          model_values <- unique(trimws(model_values[!is.na(model_values) & nzchar(model_values)]))
          if (length(model_values) == 0) {
            ""
          } else {
            model_values[[1]]
          }
        },
        error = function(e) ""
      )

      if (nzchar(candidate)) {
        discovered_model <- candidate
        break
      }
    }
    if (nzchar(discovered_model)) {
      break
    }
  }

  if (nzchar(discovered_model)) discovered_model else default_model
}

#' @keywords internal
#' @noRd
.gen_txt_openai_compatible <- function(provider_label,
                                       prompt,
                                       model,
                                       temp_v,
                                       reasoning,
                                       add_img,
                                       tools = FALSE,
                                       my_tools = NULL,
                                       plugins = NULL,
                                       timeout_secs = 80,
                                       base_url,
                                       api_key,
                                       chat_path = "/v1/chat/completions",
                                       auth_header = "Authorization",
                                       auth_prefix = "Bearer",
                                       extra_headers = NULL,
                                       max_tokens = NULL,
                                       api_key_required = TRUE,
                                       warn_ignored_reasoning = TRUE,
                                       warn_ignored_plugins = TRUE,
                                       reasoning_field = NULL,
                                       plugins_field = NULL) {
  if (isTRUE(api_key_required) && !nzchar(api_key)) {
    stop("Environment variable for ", provider_label, " API key not set.")
  }

  if (!is.null(reasoning) && is.null(reasoning_field) && isTRUE(warn_ignored_reasoning)) {
    warning("`reasoning` is currently ignored for service = '", tolower(provider_label), "'.")
  }
  if (!is.null(plugins) && is.null(plugins_field) && isTRUE(warn_ignored_plugins)) {
    warning("`plugins` is currently ignored for service = '", tolower(provider_label), "'.")
  }

  api_url <- if (grepl("^https?://", chat_path, ignore.case = TRUE)) {
    chat_path
  } else {
    paste0(base_url, chat_path)
  }

  initial_message <- list(role = "user", content = if (!is.null(add_img)) {
    list(
      list(type = "text", text = prompt),
      list(type = "image_url", image_url = list(url = paste0("data:image/jpeg;base64,", .encode_image(add_img))))
    )
  } else {
    prompt
  })

  body <- list(
    model = model,
    messages = list(initial_message),
    temperature = temp_v
  )
  if (!is.null(max_tokens) && length(max_tokens) == 1 && is.finite(suppressWarnings(as.numeric(max_tokens)))) {
    body$max_tokens <- as.integer(max_tokens)
  }
  if (!is.null(reasoning) && !is.null(reasoning_field) && nzchar(reasoning_field)) {
    body[[reasoning_field]] <- reasoning
  }
  if (!is.null(plugins) && !is.null(plugins_field) && nzchar(plugins_field)) {
    body[[plugins_field]] <- plugins
  }
  if (isTRUE(tools)) {
    tools_payload <- NULL
    if (is.function(my_tools)) {
      tools_payload <- tryCatch(my_tools(), error = function(e) {
        warning("Error while building tools for ", provider_label, ": ", conditionMessage(e))
        NULL
      })
    } else if (!is.null(my_tools)) {
      tools_payload <- my_tools
    }
    if (!is.null(tools_payload)) {
      body$tools <- tools_payload
      body$tool_choice <- "auto"
    }
  }

  header_args <- list("Content-Type" = "application/json")
  if (!is.null(extra_headers) && length(extra_headers) > 0) {
    header_args <- c(header_args, extra_headers)
  }
  if (nzchar(auth_header) && nzchar(api_key)) {
    auth_value <- if (nzchar(auth_prefix)) paste(auth_prefix, api_key) else api_key
    header_args[[auth_header]] <- auth_value
  }
  headers <- do.call(httr::add_headers, header_args)

  response <- tryCatch(
    {
      httr::POST(
        url = api_url,
        headers,
        body = toJSON(body, auto_unbox = TRUE, null = "null"),
        encode = "json",
        config = httr::timeout(timeout_secs)
      )
    },
    error = function(e) {
      err_msg <- conditionMessage(e)
      if (grepl("Timeout was reached|Operation timed out", err_msg, ignore.case = TRUE)) {
        warning(paste("HTTR timeout in", paste0(".gen_txt_", tolower(provider_label)), "after", timeout_secs, "seconds:", err_msg))
        return(paste0("TIMEOUT_ERRORR_HTTR: API call exceeded ", timeout_secs, " seconds."))
      }
      warning(paste("HTTR error in", paste0(".gen_txt_", tolower(provider_label)), ":", err_msg))
      paste0("HTTR_ERRORR: ", err_msg)
    }
  )

  if (is.character(response) && (startsWith(response, "TIMEOUT_ERRORR_HTTR:") || startsWith(response, "HTTR_ERRORR:"))) {
    return(response)
  }

  if (http_status(response)$category != "Success") {
    error_content <- content(response, "text", encoding = "UTF-8")
    error_details <- tryCatch(
      {
        parsed <- fromJSON(error_content)
        parsed$error$message %||% parsed$error %||% parsed$message %||% error_content
      },
      error = function(e) error_content
    )
    return(paste("API_ERRORR:", http_status(response)$reason, "-", error_details))
  }

  result <- tryCatch(
    {
      content(response, as = "parsed", type = "application/json", encoding = "UTF-8")
    },
    error = function(e) {
      warning("Failed to parse ", provider_label, " response JSON: ", conditionMessage(e))
      NULL
    }
  )

  if (is.null(result)) {
    return(paste0("API_RESPONSE_ERRORR: Invalid JSON returned by ", provider_label, "."))
  }

  first_choice <- NULL
  if (is.list(result$choices) && length(result$choices) >= 1) {
    first_choice <- result$choices[[1]]
  }

  message_content <- first_choice$message %||% result$message
  function_result <- NULL
  if (!is.null(message_content$tool_calls) || !is.null(message_content$function_call)) {
    function_result <- result
  }

  if (!is.null(function_result)) {
    if (inherits(function_result, "htmlwidget")) {
      return(as.character(function_result))
    }
    return(function_result)
  }

  response_text <- message_content$content %||% first_choice$text %||% result$content %||% result$response
  if (!is.null(response_text) && nzchar(trimws(paste(response_text, collapse = "")))) {
    return(response_text)
  }

  finish_reason <- first_choice$finish_reason %||% ""
  if (identical(finish_reason, "content_filter")) {
    warning("Content blocked by filter (", provider_label, "/model).")
    return("CONTENT_FILTERED: Response blocked by content filter.")
  }

  warning("Unexpected ", provider_label, " API response. Finish reason: ", finish_reason)
  "API_RESPONSE_ERRORR: No valid content, tool_calls or function_call found."
}

#' @keywords internal
#' @noRd
.custom_openai_provider_resolve_model <- function(provider_cfg, model, timeout_secs = 10) {
  model_env_vars <- character()
  if (nzchar(as.character(provider_cfg$model_env %||% "")[1])) {
    model_env_vars <- as.character(provider_cfg$model_env)[1]
  }
  api_key <- ""
  api_key_env <- as.character(provider_cfg$api_key_env %||% "")[1]
  if (nzchar(api_key_env)) {
    api_key <- trimws(Sys.getenv(api_key_env, ""))
  }
  base_urls <- as.character(provider_cfg$base_urls %||% character())
  base_url <- if (length(base_urls)) base_urls[[1]] else ""
  .openai_compat_resolve_model(
    model = model,
    timeout_secs = timeout_secs,
    model_env_vars = model_env_vars,
    api_key = api_key,
    default_model = as.character(provider_cfg$default_model %||% "local-model")[1],
    base_url = base_url,
    base_urls = base_urls,
    model_paths = provider_cfg$model_paths %||% c("/v1/models", "/models"),
    auth_header = as.character(provider_cfg$auth_header %||% "Authorization")[1],
    auth_prefix = as.character(provider_cfg$auth_prefix %||% "Bearer")[1],
    extra_headers = provider_cfg$extra_headers %||% list(),
    api_key_required = isTRUE(provider_cfg$api_key_required),
    discover_model = FALSE
  )
}

#' @keywords internal
#' @noRd
.gen_txt_custom_openai_provider <- function(provider_cfg,
                                            prompt,
                                            model,
                                            temp_v,
                                            reasoning,
                                            add_img,
                                            tools = FALSE,
                                            my_tools = NULL,
                                            plugins = NULL,
                                            timeout_secs = 80) {
  provider_id <- as.character(provider_cfg$id %||% "")[1]
  provider_label <- as.character(provider_cfg$label %||% provider_id %||% "Custom")[1]

  api_key <- ""
  api_key_env <- as.character(provider_cfg$api_key_env %||% "")[1]
  if (nzchar(api_key_env)) {
    api_key <- trimws(Sys.getenv(api_key_env, ""))
  }

  supports_tools <- isTRUE(provider_cfg$supports_tools)
  supports_vision <- isTRUE(provider_cfg$supports_vision)
  supports_reasoning <- isTRUE(provider_cfg$supports_reasoning)
  supports_plugins <- isTRUE(provider_cfg$supports_plugins)

  reasoning_field <- as.character(provider_cfg$reasoning_field %||% "")[1]
  plugins_field <- as.character(provider_cfg$plugins_field %||% "")[1]
  if (!nzchar(reasoning_field)) {
    reasoning_field <- NULL
  }
  if (!nzchar(plugins_field)) {
    plugins_field <- NULL
  }

  if (!supports_vision && !is.null(add_img)) {
    warning("`add_img` is not supported for service = '", provider_id, "'. Ignoring image input.")
    add_img <- NULL
  }
  if (!supports_tools && (isTRUE(tools) || !is.null(my_tools))) {
    warning("`tools` is not supported for service = '", provider_id, "'. Ignoring tools.")
    tools <- FALSE
    my_tools <- NULL
  }
  if (!supports_reasoning && !is.null(reasoning)) {
    warning("`reasoning` is not supported for service = '", provider_id, "'. Ignoring reasoning.")
    reasoning <- NULL
  }
  if (!supports_plugins && !is.null(plugins)) {
    warning("`plugins` is not supported for service = '", provider_id, "'. Ignoring plugins.")
    plugins <- NULL
  }

  model <- .custom_openai_provider_resolve_model(provider_cfg, model, timeout_secs = timeout_secs)

  base_urls <- as.character(provider_cfg$base_urls %||% character())
  if (!length(base_urls)) {
    return("API_ERRORR: Custom provider has no base URL configured.")
  }
  chat_paths <- as.character(provider_cfg$chat_paths %||% c("/v1/chat/completions"))
  chat_paths <- chat_paths[nzchar(chat_paths)]
  if (!length(chat_paths)) {
    chat_paths <- c("/v1/chat/completions")
  }

  last_error <- paste0("API_ERRORR: Unable to get a valid response from custom provider '", provider_id, "'.")
  for (base_url in base_urls) {
    for (chat_path in chat_paths) {
      response <- .gen_txt_openai_compatible(
        provider_label = provider_label,
        prompt = prompt,
        model = model,
        temp_v = temp_v,
        reasoning = reasoning,
        add_img = add_img,
        tools = tools,
        my_tools = my_tools,
        plugins = plugins,
        timeout_secs = timeout_secs,
        base_url = base_url,
        api_key = api_key,
        chat_path = chat_path,
        auth_header = as.character(provider_cfg$auth_header %||% "Authorization")[1],
        auth_prefix = as.character(provider_cfg$auth_prefix %||% "Bearer")[1],
        extra_headers = provider_cfg$extra_headers %||% list(),
        max_tokens = provider_cfg$max_tokens %||% NULL,
        api_key_required = isTRUE(provider_cfg$api_key_required),
        warn_ignored_reasoning = !supports_reasoning,
        warn_ignored_plugins = !supports_plugins,
        reasoning_field = reasoning_field,
        plugins_field = plugins_field
      )

      if (is.character(response) && (
        startsWith(response, "API_ERRORR:") ||
          startsWith(response, "HTTR_ERRORR:") ||
          startsWith(response, "TIMEOUT_ERRORR_HTTR:") ||
          startsWith(response, "API_RESPONSE_ERRORR:")
      )) {
        last_error <- response
        next
      }
      return(response)
    }
  }

  last_error
}

#' @keywords internal
#' @noRd
.nebius_base_url <- function() {
  base_url <- Sys.getenv("NEBIUS_BASE_URL", "https://api.studio.nebius.ai")
  base_url <- trimws(base_url)
  if (!nzchar(base_url)) {
    base_url <- "https://api.studio.nebius.ai"
  }
  sub("/+$", "", base_url)
}

#' @keywords internal
#' @noRd
.nebius_api_key <- function() {
  trimws(Sys.getenv("NEBIUS_API_KEY", ""))
}

#' @keywords internal
#' @noRd
.nebius_resolve_model <- function(model, timeout_secs = 10) {
  .openai_compat_resolve_model(
    model = model,
    timeout_secs = timeout_secs,
    model_env_vars = c("NEBIUS_MODEL"),
    api_key = .nebius_api_key(),
    default_model = "meta-llama/Meta-Llama-3.1-70B-Instruct",
    base_url = .nebius_base_url(),
    model_paths = c("/v1/models", "/models")
  )
}

#' @keywords internal
#' @noRd
.gen_txt_nebius <- function(prompt,
                            model,
                            temp_v,
                            reasoning,
                            add_img,
                            tools = FALSE,
                            my_tools = NULL,
                            plugins = NULL,
                            timeout_secs = 80) {
  base_url <- .nebius_base_url()
  api_key <- .nebius_api_key()
  model <- .nebius_resolve_model(model, timeout_secs = timeout_secs)
  .gen_txt_openai_compatible(
    provider_label = "Nebius",
    prompt = prompt,
    model = model,
    temp_v = temp_v,
    reasoning = reasoning,
    add_img = add_img,
    tools = tools,
    my_tools = my_tools,
    plugins = plugins,
    timeout_secs = timeout_secs,
    base_url = base_url,
    api_key = api_key,
    chat_path = "/v1/chat/completions"
  )
}

#' @keywords internal
#' @noRd
.deepseek_base_url <- function() {
  base_url <- Sys.getenv("DEEPSEEK_BASE_URL", "https://api.deepseek.com")
  base_url <- trimws(base_url)
  if (!nzchar(base_url)) {
    base_url <- "https://api.deepseek.com"
  }
  sub("/+$", "", base_url)
}

#' @keywords internal
#' @noRd
.deepseek_api_key <- function() {
  trimws(Sys.getenv("DEEPSEEK_API_KEY", ""))
}

#' @keywords internal
#' @noRd
.deepseek_resolve_model <- function(model, timeout_secs = 10) {
  .openai_compat_resolve_model(
    model = model,
    timeout_secs = timeout_secs,
    model_env_vars = c("DEEPSEEK_MODEL"),
    api_key = .deepseek_api_key(),
    default_model = "deepseek-chat",
    base_url = .deepseek_base_url(),
    model_paths = c("/models", "/v1/models")
  )
}

#' @keywords internal
#' @noRd
.gen_txt_deepseek <- function(prompt,
                              model,
                              temp_v,
                              reasoning,
                              add_img,
                              tools = FALSE,
                              my_tools = NULL,
                              plugins = NULL,
                              timeout_secs = 80) {
  base_url <- .deepseek_base_url()
  api_key <- .deepseek_api_key()
  model <- .deepseek_resolve_model(model, timeout_secs = timeout_secs)
  .gen_txt_openai_compatible(
    provider_label = "DeepSeek",
    prompt = prompt,
    model = model,
    temp_v = temp_v,
    reasoning = reasoning,
    add_img = add_img,
    tools = tools,
    my_tools = my_tools,
    plugins = plugins,
    timeout_secs = timeout_secs,
    base_url = base_url,
    api_key = api_key,
    chat_path = "/chat/completions"
  )
}

#' @keywords internal
#' @noRd
.perplexity_base_url <- function() {
  base_url <- Sys.getenv("PERPLEXITY_BASE_URL", "https://api.perplexity.ai")
  base_url <- trimws(base_url)
  if (!nzchar(base_url)) {
    base_url <- "https://api.perplexity.ai"
  }
  sub("/+$", "", base_url)
}

#' @keywords internal
#' @noRd
.perplexity_api_key <- function() {
  trimws(Sys.getenv("PERPLEXITY_API_KEY", ""))
}

#' @keywords internal
#' @noRd
.perplexity_resolve_model <- function(model, timeout_secs = 10) {
  .openai_compat_resolve_model(
    model = model,
    timeout_secs = timeout_secs,
    model_env_vars = c("PERPLEXITY_MODEL"),
    api_key = .perplexity_api_key(),
    default_model = "sonar",
    base_url = .perplexity_base_url(),
    model_paths = c("/models", "/v1/models")
  )
}

#' @keywords internal
#' @noRd
.gen_txt_perplexity <- function(prompt,
                                model,
                                temp_v,
                                reasoning,
                                add_img,
                                tools = FALSE,
                                my_tools = NULL,
                                plugins = NULL,
                                timeout_secs = 80) {
  base_url <- .perplexity_base_url()
  api_key <- .perplexity_api_key()
  model <- .perplexity_resolve_model(model, timeout_secs = timeout_secs)
  .gen_txt_openai_compatible(
    provider_label = "Perplexity",
    prompt = prompt,
    model = model,
    temp_v = temp_v,
    reasoning = reasoning,
    add_img = add_img,
    tools = tools,
    my_tools = my_tools,
    plugins = plugins,
    timeout_secs = timeout_secs,
    base_url = base_url,
    api_key = api_key,
    chat_path = "/chat/completions"
  )
}

#' @keywords internal
#' @noRd
.fireworks_base_url <- function() {
  base_url <- Sys.getenv("FIREWORKS_BASE_URL", "https://api.fireworks.ai/inference")
  base_url <- trimws(base_url)
  if (!nzchar(base_url)) {
    base_url <- "https://api.fireworks.ai/inference"
  }
  sub("/+$", "", base_url)
}

#' @keywords internal
#' @noRd
.fireworks_api_key <- function() {
  trimws(Sys.getenv("FIREWORKS_API_KEY", ""))
}

#' @keywords internal
#' @noRd
.fireworks_resolve_model <- function(model, timeout_secs = 10) {
  .openai_compat_resolve_model(
    model = model,
    timeout_secs = timeout_secs,
    model_env_vars = c("FIREWORKS_MODEL"),
    api_key = .fireworks_api_key(),
    default_model = "accounts/fireworks/models/llama-v3p1-8b-instruct",
    base_url = .fireworks_base_url(),
    model_paths = c("/v1/models", "/models")
  )
}

#' @keywords internal
#' @noRd
.gen_txt_fireworks <- function(prompt,
                               model,
                               temp_v,
                               reasoning,
                               add_img,
                               tools = FALSE,
                               my_tools = NULL,
                               plugins = NULL,
                               timeout_secs = 80) {
  base_url <- .fireworks_base_url()
  api_key <- .fireworks_api_key()
  model <- .fireworks_resolve_model(model, timeout_secs = timeout_secs)
  .gen_txt_openai_compatible(
    provider_label = "Fireworks",
    prompt = prompt,
    model = model,
    temp_v = temp_v,
    reasoning = reasoning,
    add_img = add_img,
    tools = tools,
    my_tools = my_tools,
    plugins = plugins,
    timeout_secs = timeout_secs,
    base_url = base_url,
    api_key = api_key,
    chat_path = "/v1/chat/completions"
  )
}

#' @keywords internal
#' @noRd
.deepinfra_base_url <- function() {
  base_url <- Sys.getenv("DEEPINFRA_BASE_URL", "https://api.deepinfra.com/v1/openai")
  base_url <- trimws(base_url)
  if (!nzchar(base_url)) {
    base_url <- "https://api.deepinfra.com/v1/openai"
  }
  sub("/+$", "", base_url)
}

#' @keywords internal
#' @noRd
.deepinfra_api_key <- function() {
  trimws(Sys.getenv("DEEPINFRA_API_KEY", ""))
}

#' @keywords internal
#' @noRd
.deepinfra_resolve_model <- function(model, timeout_secs = 10) {
  .openai_compat_resolve_model(
    model = model,
    timeout_secs = timeout_secs,
    model_env_vars = c("DEEPINFRA_MODEL"),
    api_key = .deepinfra_api_key(),
    default_model = "meta-llama/Meta-Llama-3.1-8B-Instruct",
    base_url = .deepinfra_base_url(),
    model_paths = c("/models", "/v1/models")
  )
}

#' @keywords internal
#' @noRd
.gen_txt_deepinfra <- function(prompt,
                               model,
                               temp_v,
                               reasoning,
                               add_img,
                               tools = FALSE,
                               my_tools = NULL,
                               plugins = NULL,
                               timeout_secs = 80) {
  base_url <- .deepinfra_base_url()
  api_key <- .deepinfra_api_key()
  model <- .deepinfra_resolve_model(model, timeout_secs = timeout_secs)
  .gen_txt_openai_compatible(
    provider_label = "DeepInfra",
    prompt = prompt,
    model = model,
    temp_v = temp_v,
    reasoning = reasoning,
    add_img = add_img,
    tools = tools,
    my_tools = my_tools,
    plugins = plugins,
    timeout_secs = timeout_secs,
    base_url = base_url,
    api_key = api_key,
    chat_path = "/chat/completions"
  )
}

#' @keywords internal
#' @noRd
.hyperbolic_base_url <- function() {
  base_url <- Sys.getenv("HYPERBOLIC_BASE_URL", "https://api.hyperbolic.xyz")
  base_url <- trimws(base_url)
  if (!nzchar(base_url)) {
    base_url <- "https://api.hyperbolic.xyz"
  }
  sub("/+$", "", base_url)
}

#' @keywords internal
#' @noRd
.hyperbolic_api_key <- function() {
  trimws(Sys.getenv("HYPERBOLIC_API_KEY", ""))
}

#' @keywords internal
#' @noRd
.hyperbolic_resolve_model <- function(model, timeout_secs = 10) {
  .openai_compat_resolve_model(
    model = model,
    timeout_secs = timeout_secs,
    model_env_vars = c("HYPERBOLIC_MODEL"),
    api_key = .hyperbolic_api_key(),
    default_model = "meta-llama/Meta-Llama-3.1-8B-Instruct",
    base_url = .hyperbolic_base_url(),
    model_paths = c("/v1/models", "/models")
  )
}

#' @keywords internal
#' @noRd
.gen_txt_hyperbolic <- function(prompt,
                                model,
                                temp_v,
                                reasoning,
                                add_img,
                                tools = FALSE,
                                my_tools = NULL,
                                plugins = NULL,
                                timeout_secs = 80) {
  base_url <- .hyperbolic_base_url()
  api_key <- .hyperbolic_api_key()
  model <- .hyperbolic_resolve_model(model, timeout_secs = timeout_secs)
  .gen_txt_openai_compatible(
    provider_label = "Hyperbolic",
    prompt = prompt,
    model = model,
    temp_v = temp_v,
    reasoning = reasoning,
    add_img = add_img,
    tools = tools,
    my_tools = my_tools,
    plugins = plugins,
    timeout_secs = timeout_secs,
    base_url = base_url,
    api_key = api_key,
    chat_path = "/v1/chat/completions"
  )
}

.GEMINI_TEXT_DEFAULT_MODEL <- "gemini-3.6-flash"

#' @keywords internal
#' @noRd
.gemini_api_key <- function() {
  # Google documents GOOGLE_API_KEY as taking precedence when both aliases are
  # present.
  key <- trimws(Sys.getenv("GOOGLE_API_KEY", ""))
  if (!nzchar(key)) {
    key <- trimws(Sys.getenv("GEMINI_API_KEY", ""))
  }
  key
}

#' @keywords internal
#' @noRd
.gemini_resolve_model <- function(model) {
  model_chr <- trimws(as.character(model %||% "")[1])
  if (nzchar(model_chr) && !identical(model_chr, "gpt-5-mini")) {
    return(sub("^models/", "", model_chr))
  }

  configured <- trimws(Sys.getenv("GEMINI_MODEL", ""))
  if (!nzchar(configured)) {
    configured <- trimws(Sys.getenv("GOOGLE_MODEL", ""))
  }
  if (nzchar(configured)) {
    return(sub("^models/", "", configured))
  }
  .GEMINI_TEXT_DEFAULT_MODEL
}

#' @keywords internal
#' @noRd
.gemini_image_mime_type <- function(path) {
  ext <- tolower(tools::file_ext(path %||% ""))
  mime <- switch(ext,
    "jpg" = "image/jpeg",
    "jpeg" = "image/jpeg",
    "png" = "image/png",
    "webp" = "image/webp",
    "heic" = "image/heic",
    "heif" = "image/heif",
    ""
  )
  if (!nzchar(mime)) {
    stop(
      "Gemini inline images must be JPEG, PNG, WebP, HEIC, or HEIF.",
      call. = FALSE
    )
  }
  mime
}

#' @keywords internal
#' @noRd
.gemini_format_tools <- function(tools_payload) {
  if (is.null(tools_payload)) {
    return(NULL)
  }
  if (!is.list(tools_payload)) {
    stop("Gemini tool definitions must be a list.", call. = FALSE)
  }

  # Preserve a caller-supplied Gemini Tool object.
  if (!is.null(tools_payload$functionDeclarations)) {
    return(list(tools_payload))
  }
  if (length(tools_payload) &&
      all(vapply(tools_payload, function(x) {
        is.list(x) && !is.null(x$functionDeclarations)
      }, logical(1)))) {
    return(tools_payload)
  }

  single_named <- !is.null(names(tools_payload)) &&
    any(nzchar(names(tools_payload))) &&
    ("name" %in% names(tools_payload) || "function" %in% names(tools_payload))
  tools_list <- if (single_named) list(tools_payload) else tools_payload

  declarations <- lapply(tools_list, function(tool) {
    if (is.data.frame(tool)) {
      tool <- as.list(tool[1, , drop = FALSE])
    }
    if (!is.list(tool)) {
      return(NULL)
    }
    fn <- if (identical(tool$type %||% "", "function") &&
              is.list(tool[["function"]])) {
      tool[["function"]]
    } else {
      tool
    }
    name <- trimws(as.character(fn$name %||% "")[1])
    if (!nzchar(name)) {
      return(NULL)
    }
    list(
      name = name,
      description = as.character(fn$description %||% "")[1],
      parameters = fn$parameters %||%
        fn$input_schema %||%
        list(type = "object", properties = list())
    )
  })
  declarations <- Filter(Negate(is.null), declarations)
  if (!length(declarations)) {
    stop(
      "No valid Gemini function declarations were found in `tools`.",
      call. = FALSE
    )
  }
  list(list(functionDeclarations = declarations))
}

#' @keywords internal
#' @noRd
.gemini_response_parts <- function(result) {
  candidates <- result$candidates
  if (is.null(candidates) || !length(candidates)) {
    return(list())
  }
  candidate <- if (is.data.frame(candidates)) {
    as.list(candidates[1, , drop = FALSE])
  } else {
    candidates[[1]]
  }
  parts <- candidate$content$parts %||% list()
  if (is.data.frame(parts) && nrow(parts)) {
    parts <- lapply(seq_len(nrow(parts)), function(i) {
      as.list(parts[i, , drop = FALSE])
    })
  }
  if (!is.list(parts)) list() else parts
}

#' Internal: Gemini generateContent call
#'
#' Calls Google's Gemini `generateContent` endpoint and returns text, the raw
#' response for function calls, or a normalized error sentinel.
#'
#' @keywords internal
#' @noRd
.gen_txt_gemini <- function(prompt,
                            model,
                            temp_v,
                            reasoning,
                            add_img,
                            tools = FALSE,
                            my_tools = NULL,
                            plugins = NULL,
                            timeout_secs = 80) {
  api_key <- .gemini_api_key()
  if (!nzchar(api_key)) {
    stop("Environment variable GOOGLE_API_KEY or GEMINI_API_KEY not set.")
  }
  model <- .gemini_resolve_model(model)

  if (!is.null(reasoning)) {
    warning("`reasoning` is currently ignored for service = 'gemini'.")
  }
  if (!is.null(plugins)) {
    warning("`plugins` is currently ignored for service = 'gemini'.")
  }

  parts <- list(list(text = prompt))
  if (!is.null(add_img)) {
    parts <- c(
      list(list(
        inline_data = list(
          mime_type = .gemini_image_mime_type(add_img),
          data = .encode_image(add_img)
        )
      )),
      parts
    )
  }
  body <- list(
    contents = list(list(role = "user", parts = parts)),
    generationConfig = list(temperature = temp_v)
  )
  if (isTRUE(tools)) {
    body$tools <- .gemini_format_tools(my_tools)
  }

  api_url <- paste0(
    "https://generativelanguage.googleapis.com/v1beta/models/",
    utils::URLencode(model, reserved = TRUE),
    ":generateContent"
  )
  response <- tryCatch(
    httr::POST(
      url = api_url,
      httr::add_headers(
        "Content-Type" = "application/json",
        "x-goog-api-key" = api_key
      ),
      body = body,
      encode = "json",
      config = httr::timeout(timeout_secs)
    ),
    error = function(e) {
      detail <- conditionMessage(e)
      if (grepl("Timeout was reached|Operation timed out", detail, ignore.case = TRUE)) {
        return(paste0(
          "TIMEOUT_ERRORR_HTTR: Gemini API call exceeded ",
          timeout_secs,
          " seconds."
        ))
      }
      paste0("HTTR_ERRORR: ", detail)
    }
  )
  if (is.character(response)) {
    return(response)
  }

  if (httr::http_status(response)$category != "Success") {
    error_content <- httr::content(response, "text", encoding = "UTF-8")
    error_detail <- tryCatch({
      parsed <- jsonlite::fromJSON(error_content, simplifyVector = FALSE)
      parsed$error$message %||% parsed$message %||% error_content
    }, error = function(e) error_content)
    return(paste(
      "API_ERRORR:",
      httr::http_status(response)$reason,
      "-",
      as.character(error_detail)[1]
    ))
  }

  result <- tryCatch(
    httr::content(
      response,
      as = "parsed",
      type = "application/json",
      encoding = "UTF-8"
    ),
    error = function(e) NULL
  )
  if (!is.list(result)) {
    return("API_RESPONSE_ERRORR: Invalid JSON returned by Gemini.")
  }

  parts <- .gemini_response_parts(result)
  has_function_call <- any(vapply(parts, function(part) {
    is.list(part) && !is.null(part$functionCall %||% part$function_call)
  }, logical(1)))
  if (has_function_call) {
    return(result)
  }

  text <- vapply(parts, function(part) {
    if (!is.list(part)) return("")
    value <- as.character(part$text %||% "")[1]
    if (is.na(value)) "" else value
  }, character(1))
  text <- text[nzchar(text)]
  if (length(text)) {
    return(paste(text, collapse = ""))
  }

  block_reason <- as.character(result$promptFeedback$blockReason %||% "")[1]
  candidates <- result$candidates %||% list()
  finish_reason <- if (length(candidates) && is.list(candidates[[1]])) {
    as.character(candidates[[1]]$finishReason %||% "")[1]
  } else {
    ""
  }
  if (nzchar(block_reason) ||
      toupper(finish_reason) %in% c("SAFETY", "BLOCKLIST", "PROHIBITED_CONTENT")) {
    return(paste0(
      "CONTENT_FILTERED: Response blocked by Gemini",
      if (nzchar(block_reason)) paste0(" (", block_reason, ")") else "."
    ))
  }
  "API_RESPONSE_ERRORR: No text or function call found in Gemini response."
}

#' @keywords internal
#' @noRd
.anthropic_base_url <- function() {
  base_url <- Sys.getenv("ANTHROPIC_BASE_URL", "https://api.anthropic.com")
  base_url <- trimws(base_url)
  if (!nzchar(base_url)) {
    base_url <- "https://api.anthropic.com"
  }
  sub("/+$", "", base_url)
}

#' @keywords internal
#' @noRd
.anthropic_api_key <- function() {
  api_key <- trimws(Sys.getenv("ANTHROPIC_API_KEY", ""))
  if (!nzchar(api_key)) {
    api_key <- trimws(Sys.getenv("CLAUDE_API_KEY", ""))
  }
  api_key
}

#' @keywords internal
#' @noRd
.anthropic_api_version <- function() {
  api_version <- trimws(Sys.getenv("ANTHROPIC_API_VERSION", "2023-06-01"))
  if (!nzchar(api_version)) {
    api_version <- "2023-06-01"
  }
  api_version
}

#' @keywords internal
#' @noRd
.anthropic_resolve_model <- function(model, timeout_secs = 10) {
  model_chr <- as.character(model %||% "")[1]
  model_chr <- trimws(model_chr)
  if (nzchar(model_chr) && !identical(model_chr, "gpt-5-mini")) {
    return(model_chr)
  }

  model_env <- trimws(Sys.getenv("ANTHROPIC_MODEL", ""))
  if (!nzchar(model_env)) {
    model_env <- trimws(Sys.getenv("CLAUDE_MODEL", ""))
  }
  if (nzchar(model_env)) {
    return(model_env)
  }

  api_key <- .anthropic_api_key()
  if (!nzchar(api_key)) {
    return("claude-3-5-sonnet-latest")
  }

  discovery_timeout <- if (is.numeric(timeout_secs) && length(timeout_secs) == 1 && !is.na(timeout_secs) && timeout_secs > 0) {
    min(timeout_secs, 10)
  } else {
    10
  }

  base_url <- .anthropic_base_url()
  headers <- httr::add_headers(
    "Content-Type" = "application/json",
    "x-api-key" = api_key,
    "anthropic-version" = .anthropic_api_version()
  )

  discovered_model <- tryCatch(
    {
      models_response <- httr::GET(
        url = paste0(base_url, "/v1/models"),
        headers,
        httr::timeout(discovery_timeout)
      )
      if (httr::status_code(models_response) != 200) {
        stop(
          "Anthropic model discovery returned HTTP ",
          httr::status_code(models_response),
          call. = FALSE
        )
      }

      models_content <- httr::content(models_response, as = "parsed", type = "application/json", encoding = "UTF-8")
      models <- models_content$data %||% models_content$models
      model_values <- character()
      if (is.data.frame(models) && nrow(models) > 0) {
        model_values <- as.character(models$id %||% models$model %||% models$name %||% "")
      } else if (is.list(models) && length(models) > 0) {
        model_values <- vapply(models, function(entry) {
          if (!is.list(entry)) {
            return("")
          }
          as.character(entry$id %||% entry$model %||% entry$name %||% "")
        }, character(1), USE.NAMES = FALSE)
      }
      model_values <- unique(trimws(model_values[!is.na(model_values) & nzchar(model_values)]))
      if (length(model_values) == 0) {
        ""
      } else {
        model_values[[1]]
      }
    },
    error = function(e) ""
  )

  if (nzchar(discovered_model)) discovered_model else "claude-3-5-sonnet-latest"
}

#' @keywords internal
#' @noRd
.anthropic_media_type <- function(path) {
  ext <- tolower(tools::file_ext(path %||% ""))
  switch(ext,
    "jpg" = "image/jpeg",
    "jpeg" = "image/jpeg",
    "png" = "image/png",
    "webp" = "image/webp",
    "gif" = "image/gif",
    "image/jpeg"
  )
}

#' @keywords internal
#' @noRd
.anthropic_format_tools <- function(tools_payload) {
  if (is.null(tools_payload) || !is.list(tools_payload)) {
    return(NULL)
  }

  single_named <- !is.null(names(tools_payload)) &&
    any(nzchar(names(tools_payload))) &&
    ("name" %in% names(tools_payload) || "function" %in% names(tools_payload))
  tools_list <- if (single_named) list(tools_payload) else tools_payload

  normalize_single <- function(tool) {
    if (is.data.frame(tool)) {
      tool <- as.list(tool[1, , drop = FALSE])
    }
    if (!is.list(tool)) {
      return(NULL)
    }

    tool_fn <- tool[["function"]]
    if (!is.null(tool$type) && identical(tool$type, "function") && is.list(tool_fn)) {
      fn <- tool_fn
      name <- as.character(fn$name %||% "")[1]
      if (!nzchar(name)) {
        return(NULL)
      }
      description <- as.character(fn$description %||% "")[1]
      input_schema <- fn$parameters %||% fn$input_schema %||% list(type = "object", properties = list())
      return(list(name = name, description = description, input_schema = input_schema))
    }

    name <- as.character(tool$name %||% "")[1]
    if (!nzchar(name)) {
      return(NULL)
    }
    description <- as.character(tool$description %||% "")[1]
    input_schema <- tool$input_schema %||% tool$parameters %||% list(type = "object", properties = list())
    list(name = name, description = description, input_schema = input_schema)
  }

  formatted <- lapply(tools_list, normalize_single)
  formatted <- Filter(Negate(is.null), formatted)
  if (!length(formatted)) NULL else formatted
}

#' Internal: Anthropic Messages API call
#'
#' Calls Anthropic's `/v1/messages` endpoint and returns textual content,
#' tool-use payload, or an error sentinel string.
#'
#' @keywords internal
#' @noRd
.gen_txt_anthropic <- function(prompt,
                               model,
                               temp_v,
                               reasoning,
                               add_img,
                               tools = FALSE,
                               my_tools = NULL,
                               plugins = NULL,
                               timeout_secs = 80) {
  base_url <- .anthropic_base_url()
  api_key <- .anthropic_api_key()
  if (!nzchar(api_key)) {
    stop("Environment variable ANTHROPIC_API_KEY not set.")
  }
  model <- .anthropic_resolve_model(model, timeout_secs = timeout_secs)
  api_url <- paste0(base_url, "/v1/messages")

  if (!is.null(reasoning)) {
    warning("`reasoning` is currently ignored for service = 'anthropic'.")
  }
  if (!is.null(plugins)) {
    warning("`plugins` is currently ignored for service = 'anthropic'.")
  }

  user_blocks <- list(
    list(type = "text", text = prompt)
  )
  if (!is.null(add_img)) {
    user_blocks[[length(user_blocks) + 1]] <- list(
      type = "image",
      source = list(
        type = "base64",
        media_type = .anthropic_media_type(add_img),
        data = .encode_image(add_img)
      )
    )
  }

  body <- list(
    model = model,
    max_tokens = 4096,
    temperature = temp_v,
    messages = list(
      list(
        role = "user",
        content = user_blocks
      )
    )
  )

  if (isTRUE(tools)) {
    tools_payload <- NULL
    if (is.function(my_tools)) {
      tools_payload <- tryCatch(my_tools(), error = function(e) {
        warning("Error while building tools for Anthropic: ", conditionMessage(e))
        NULL
      })
    } else if (!is.null(my_tools)) {
      tools_payload <- my_tools
    }

    formatted_tools <- .anthropic_format_tools(tools_payload)
    if (!is.null(formatted_tools)) {
      body$tools <- formatted_tools
      body$tool_choice <- list(type = "auto")
    } else if (!is.null(tools_payload)) {
      warning("No valid Anthropic-compatible tool schema found; `tools` ignored.")
    }
  }

  headers <- httr::add_headers(
    "Content-Type" = "application/json",
    "x-api-key" = api_key,
    "anthropic-version" = .anthropic_api_version()
  )

  response <- tryCatch(
    {
      httr::POST(
        url = api_url,
        headers,
        body = toJSON(body, auto_unbox = TRUE, null = "null"),
        encode = "json",
        config = httr::timeout(timeout_secs)
      )
    },
    error = function(e) {
      err_msg <- conditionMessage(e)
      if (grepl("Timeout was reached|Operation timed out", err_msg, ignore.case = TRUE)) {
        warning(paste("HTTR timeout in .gen_txt_anthropic after", timeout_secs, "seconds:", err_msg))
        return(paste0("TIMEOUT_ERRORR_HTTR: API call exceeded ", timeout_secs, " seconds."))
      }
      warning(paste("HTTR error in .gen_txt_anthropic:", err_msg))
      paste0("HTTR_ERRORR: ", err_msg)
    }
  )

  if (is.character(response) && (startsWith(response, "TIMEOUT_ERRORR_HTTR:") || startsWith(response, "HTTR_ERRORR:"))) {
    return(response)
  }

  if (http_status(response)$category != "Success") {
    error_content <- content(response, "text", encoding = "UTF-8")
    error_details <- tryCatch(
      {
        parsed <- fromJSON(error_content)
        parsed$error$message %||% parsed$error %||% parsed$message %||% error_content
      },
      error = function(e) error_content
    )
    return(paste("API_ERRORR:", http_status(response)$reason, "-", error_details))
  }

  result <- tryCatch(
    {
      content(response, as = "parsed", type = "application/json", encoding = "UTF-8")
    },
    error = function(e) {
      warning("Failed to parse Anthropic response JSON: ", conditionMessage(e))
      NULL
    }
  )

  if (is.null(result)) {
    return("API_RESPONSE_ERRORR: Invalid JSON returned by Anthropic.")
  }

  blocks <- result$content
  if (is.null(blocks)) {
    blocks <- list()
  }
  if (is.data.frame(blocks) && nrow(blocks) > 0) {
    blocks <- split(blocks, seq_len(nrow(blocks)))
  }
  if (!is.list(blocks)) {
    blocks <- list()
  }

  has_tool_use <- FALSE
  text_parts <- character()
  for (block in blocks) {
    if (is.data.frame(block)) {
      block <- as.list(block[1, , drop = FALSE])
    }
    if (!is.list(block)) {
      next
    }
    block_type <- tolower(as.character(block$type %||% "")[1])
    if (identical(block_type, "tool_use")) {
      has_tool_use <- TRUE
    } else if (identical(block_type, "text")) {
      block_text <- as.character(block$text %||% "")[1]
      if (nzchar(block_text)) {
        text_parts <- c(text_parts, block_text)
      }
    }
  }

  if (has_tool_use || identical(result$stop_reason, "tool_use")) {
    return(result)
  }

  if (length(text_parts) > 0) {
    return(paste(text_parts, collapse = "\n"))
  }

  stop_reason <- tolower(as.character(result$stop_reason %||% ""))
  if (stop_reason %in% c("refusal", "safety")) {
    warning("Content blocked by filter (Anthropic/model).")
    return("CONTENT_FILTERED: Response blocked by content filter.")
  }

  warning("Unexpected Anthropic API response. Stop reason: ", stop_reason)
  "API_RESPONSE_ERRORR: No valid content or tool_use found."
}

#' @keywords internal
#' @noRd
.text_local_url <- function(value, field) {
  value <- as.character(value %||% "")[1]
  if (is.na(value)) {
    value <- ""
  }
  value <- sub("/+$", "", trimws(value))
  if (!nzchar(value) || !grepl("^https?://", value, ignore.case = TRUE)) {
    stop("`", field, "` must be a non-empty http(s) URL.", call. = FALSE)
  }
  value
}

.ollama_base_url <- function(base_url = NULL, config = NULL) {
  explicit <- as.character(base_url %||% "")[1]
  if (is.na(explicit)) {
    explicit <- ""
  }
  resolved <- if (nzchar(trimws(explicit))) {
    explicit
  } else {
    .genflow_local_setting(
      field = "ollama_base_url",
      env = "OLLAMA_BASE_URL",
      default = "http://127.0.0.1:11434",
      config = config
    )
  }
  .text_local_url(resolved, "base_url")
}

#' @keywords internal
#' @noRd
.ollama_resolve_model <- function(model,
                                  timeout_secs = 10,
                                  base_url = NULL,
                                  config = NULL) {
  model_chr <- as.character(model %||% "")[1]
  if (is.na(model_chr)) {
    model_chr <- ""
  }
  model_chr <- trimws(model_chr)
  if (nzchar(model_chr) && !identical(model_chr, "gpt-5-mini")) {
    return(model_chr)
  }

  configured_model <- .genflow_local_setting(
    field = "ollama_model",
    env = "OLLAMA_MODEL",
    default = "",
    config = config
  )
  if (nzchar(configured_model)) {
    return(configured_model)
  }

  discovery_timeout <- if (is.numeric(timeout_secs) && length(timeout_secs) == 1 && !is.na(timeout_secs) && timeout_secs > 0) {
    min(timeout_secs, 10)
  } else {
    10
  }
  base_url <- .ollama_base_url(base_url = base_url, config = config)

  discovered_model <- tryCatch(
    {
      tags_response <- httr::GET(
        url = paste0(base_url, "/api/tags"),
        httr::timeout(discovery_timeout)
      )
      if (httr::status_code(tags_response) != 200) {
        stop(
          "Ollama model discovery returned HTTP ",
          httr::status_code(tags_response),
          call. = FALSE
        )
      }

      tags_content <- httr::content(tags_response, as = "parsed", type = "application/json", encoding = "UTF-8")
      models <- tags_content$models
      model_values <- character()
      if (is.data.frame(models) && nrow(models) > 0) {
        model_values <- as.character(models$name %||% models$model %||% "")
      } else if (is.list(models) && length(models) > 0) {
        model_values <- vapply(models, function(entry) {
          if (!is.list(entry)) {
            return("")
          }
          as.character(entry$name %||% entry$model %||% "")
        }, character(1), USE.NAMES = FALSE)
      }
      model_values <- model_values[!is.na(model_values) & nzchar(model_values)]
      if (length(model_values) == 0) {
        ""
      } else {
        model_values[[1]]
      }
    },
    error = function(e) ""
  )

  if (nzchar(discovered_model)) discovered_model else "llama3.2"
}

#' Internal: Ollama chat call
#'
#' Calls a local Ollama server (`/api/chat`) and returns textual content
#' or an error sentinel string.
#'
#' @keywords internal
#' @noRd
.gen_txt_ollama <- function(prompt,
                            model,
                            temp_v,
                            reasoning,
                            add_img,
                            tools = FALSE,
                            my_tools = NULL,
                            plugins = NULL,
                            timeout_secs = 80,
                            base_url = NULL,
                            config = NULL) {
  base_url <- .ollama_base_url(base_url = base_url, config = config)
  model <- .ollama_resolve_model(
    model,
    timeout_secs = timeout_secs,
    base_url = base_url,
    config = config
  )
  api_url <- paste0(base_url, "/api/chat")

  if (!is.null(reasoning)) {
    warning("`reasoning` is currently ignored for service = 'ollama'.")
  }
  if (!is.null(plugins)) {
    warning("`plugins` is currently ignored for service = 'ollama'.")
  }
  if (isTRUE(tools) || !is.null(my_tools)) {
    warning("`tools` is currently ignored for service = 'ollama'.")
  }

  message_payload <- list(role = "user", content = prompt)
  if (!is.null(add_img)) {
    encoded_img <- tryCatch(
      .encode_image(add_img),
      error = function(e) {
        stop("Failed to encode `add_img` for Ollama: ", conditionMessage(e))
      }
    )
    message_payload$images <- list(encoded_img)
  }

  body <- list(
    model = model,
    messages = list(message_payload),
    stream = FALSE
  )
  if (!is.null(temp_v) && is.numeric(temp_v) && length(temp_v) == 1 && !is.na(temp_v)) {
    body$options <- list(temperature = temp_v)
  }

  response <- tryCatch(
    {
      httr::POST(
        url = api_url,
        httr::add_headers("Content-Type" = "application/json"),
        body = toJSON(body, auto_unbox = TRUE, null = "null"),
        encode = "raw",
        config = httr::timeout(timeout_secs)
      )
    },
    error = function(e) {
      err_msg <- conditionMessage(e)
      if (grepl("Timeout was reached|Operation timed out", err_msg, ignore.case = TRUE)) {
        warning(paste("HTTR timeout in .gen_txt_ollama after", timeout_secs, "seconds:", err_msg))
        return(paste0("TIMEOUT_ERRORR_HTTR: API call exceeded ", timeout_secs, " seconds."))
      }
      warning(paste("HTTR error in .gen_txt_ollama:", err_msg))
      paste0("HTTR_ERRORR: ", err_msg)
    }
  )

  if (is.character(response) && (startsWith(response, "TIMEOUT_ERRORR_HTTR:") || startsWith(response, "HTTR_ERRORR:"))) {
    return(response)
  }

  if (http_status(response)$category != "Success") {
    error_content <- content(response, "text", encoding = "UTF-8")
    error_details <- tryCatch(
      {
        parsed <- fromJSON(error_content)
        parsed$error %||% parsed$message %||% error_content
      },
      error = function(e) error_content
    )
    return(paste("API_ERRORR:", http_status(response)$reason, "-", error_details))
  }

  result <- tryCatch(
    {
      content(response, as = "parsed", type = "application/json", encoding = "UTF-8")
    },
    error = function(e) {
      warning("Failed to parse Ollama response JSON: ", conditionMessage(e))
      NULL
    }
  )

  if (is.null(result)) {
    return("API_RESPONSE_ERRORR: Invalid JSON returned by Ollama.")
  }

  if (!is.null(result$error) && nzchar(as.character(result$error))) {
    return(paste("API_ERRORR:", as.character(result$error)))
  }

  message_content <- result$message$content %||% result$response
  if (!is.null(message_content) && nzchar(trimws(paste(message_content, collapse = "")))) {
    return(message_content)
  }

  warning("Unexpected Ollama API response; no message content found.")
  "API_RESPONSE_ERRORR: No valid content found in Ollama response."
}

#' @keywords internal
#' @noRd
.llamacpp_base_url <- function(base_url = NULL, config = NULL) {
  explicit <- as.character(base_url %||% "")[1]
  if (is.na(explicit)) {
    explicit <- ""
  }
  resolved <- if (nzchar(trimws(explicit))) {
    explicit
  } else {
    .genflow_local_setting(
      field = "llamacpp_base_url",
      env = c("LLAMACPP_BASE_URL", "LLAMA_CPP_BASE_URL"),
      default = "http://127.0.0.1:8080",
      config = config
    )
  }
  base_url <- .text_local_url(resolved, "base_url")
  # Accept env values with trailing /v1 and normalize to service root.
  base_url <- sub("/v1$", "", base_url, ignore.case = TRUE)
  base_url
}

#' @keywords internal
#' @noRd
.llamacpp_base_url_candidates <- function(base_url = NULL, config = NULL) {
  primary <- .llamacpp_base_url(base_url = base_url, config = config)
  candidates <- c(primary)

  # Common local llama.cpp ports. Keep primary first, then fallback.
  if (grepl("^https?://(127\\.0\\.0\\.1|localhost):8080$", primary, ignore.case = TRUE)) {
    candidates <- c(candidates, sub(":8080$", ":8081", primary))
  } else if (grepl("^https?://(127\\.0\\.0\\.1|localhost):8081$", primary, ignore.case = TRUE)) {
    candidates <- c(candidates, sub(":8081$", ":8080", primary))
  } else {
    candidates <- c(candidates, "http://127.0.0.1:8080", "http://127.0.0.1:8081")
  }

  candidates <- trimws(as.character(candidates))
  candidates <- sub("/+$", "", candidates)
  candidates <- candidates[nzchar(candidates)]
  unique(candidates)
}

#' @keywords internal
#' @noRd
.llamacpp_api_key <- function() {
  api_key <- trimws(Sys.getenv("LLAMACPP_API_KEY", ""))
  if (!nzchar(api_key)) {
    api_key <- trimws(Sys.getenv("LLAMA_CPP_API_KEY", ""))
  }
  api_key
}

#' @keywords internal
#' @noRd
.llamacpp_resolve_model <- function(model,
                                    timeout_secs = 10,
                                    base_url = NULL,
                                    config = NULL) {
  model_chr <- as.character(model %||% "")[1]
  if (is.na(model_chr)) {
    model_chr <- ""
  }
  model_chr <- trimws(model_chr)
  if (nzchar(model_chr) && !identical(model_chr, "gpt-5-mini")) {
    return(model_chr)
  }

  configured_model <- .genflow_local_setting(
    field = "llamacpp_model",
    env = c("LLAMACPP_MODEL", "LLAMA_CPP_MODEL"),
    default = "",
    config = config
  )
  if (nzchar(configured_model)) {
    return(configured_model)
  }

  discovery_timeout <- if (is.numeric(timeout_secs) && length(timeout_secs) == 1 && !is.na(timeout_secs) && timeout_secs > 0) {
    min(timeout_secs, 10)
  } else {
    10
  }
  base_urls <- .llamacpp_base_url_candidates(
    base_url = base_url,
    config = config
  )
  api_key <- .llamacpp_api_key()

  header_args <- list("Content-Type" = "application/json")
  if (nzchar(api_key)) {
    header_args[["Authorization"]] <- paste("Bearer", api_key)
  }
  headers <- do.call(httr::add_headers, header_args)

  model_paths <- c("/v1/models", "/models")
  discovered_model <- ""
  for (base_url in base_urls) {
    for (model_path in model_paths) {
      candidate <- tryCatch(
        {
          models_response <- httr::GET(
            url = paste0(base_url, model_path),
            headers,
            httr::timeout(discovery_timeout)
          )
          if (httr::status_code(models_response) != 200) {
            stop(
              "llama-cpp model discovery returned HTTP ",
              httr::status_code(models_response),
              call. = FALSE
            )
          }

          models_content <- httr::content(models_response, as = "parsed", type = "application/json", encoding = "UTF-8")
          models <- models_content$data %||% models_content$models
          model_values <- character()
          if (is.data.frame(models) && nrow(models) > 0) {
            model_values <- as.character(models$id %||% models$model %||% models$name %||% "")
          } else if (is.list(models) && length(models) > 0) {
            model_values <- vapply(models, function(entry) {
              if (!is.list(entry)) {
                return("")
              }
              as.character(entry$id %||% entry$model %||% entry$name %||% "")
            }, character(1), USE.NAMES = FALSE)
          }
          model_values <- unique(trimws(model_values[!is.na(model_values) & nzchar(model_values)]))
          if (length(model_values) == 0) {
            ""
          } else {
            model_values[[1]]
          }
        },
        error = function(e) ""
      )
      if (nzchar(candidate)) {
        discovered_model <- candidate
        break
      }
    }
    if (nzchar(discovered_model)) {
      break
    }
  }

  if (nzchar(discovered_model)) discovered_model else "local-model"
}

#' Internal: llama.cpp OpenAI-compatible chat call
#'
#' Calls a local llama.cpp-compatible server (`/v1/chat/completions` or
#' `/chat/completions`) and
#' returns textual content, tool-call payload, or an error sentinel string.
#'
#' @keywords internal
#' @noRd
.gen_txt_llamacpp <- function(prompt,
                              model,
                              temp_v,
                              reasoning,
                              add_img,
                              tools = FALSE,
                              my_tools = NULL,
                              plugins = NULL,
                              timeout_secs = 80,
                              base_url = NULL,
                              config = NULL) {
  base_url_candidates <- .llamacpp_base_url_candidates(
    base_url = base_url,
    config = config
  )
  api_key <- .llamacpp_api_key()
  model <- .llamacpp_resolve_model(
    model,
    timeout_secs = timeout_secs,
    base_url = base_url_candidates[[1]],
    config = config
  )
  chat_paths <- c("/v1/chat/completions", "/chat/completions")

  if (!is.null(reasoning)) {
    warning("`reasoning` is currently ignored for service = 'llamacpp'.")
  }
  if (!is.null(plugins)) {
    warning("`plugins` is currently ignored for service = 'llamacpp'.")
  }

  initial_message <- list(role = "user", content = if (!is.null(add_img)) {
    list(
      list(type = "text", text = prompt),
      list(type = "image_url", image_url = list(url = paste0("data:image/jpeg;base64,", .encode_image(add_img))))
    )
  } else {
    prompt
  })

  body <- list(
    model = model,
    messages = list(initial_message),
    temperature = temp_v
  )
  if (isTRUE(tools)) {
    tools_payload <- NULL
    if (is.function(my_tools)) {
      tools_payload <- tryCatch(my_tools(), error = function(e) {
        warning("Error while building tools for llama-cpp: ", conditionMessage(e))
        NULL
      })
    } else if (!is.null(my_tools)) {
      tools_payload <- my_tools
    }
    if (!is.null(tools_payload)) {
      body$tools <- tools_payload
      body$tool_choice <- "auto"
    }
  }

  header_args <- list("Content-Type" = "application/json")
  if (nzchar(api_key)) {
    header_args[["Authorization"]] <- paste("Bearer", api_key)
  }
  headers <- do.call(httr::add_headers, header_args)
  last_error <- "API_ERRORR: Could not connect to a compatible llama-cpp chat endpoint."

  for (base_url in base_url_candidates) {
    for (chat_path in chat_paths) {
      api_url <- paste0(base_url, chat_path)
      response <- tryCatch(
        {
          httr::POST(
            url = api_url,
            headers,
            body = toJSON(body, auto_unbox = TRUE, null = "null"),
            encode = "json",
            config = httr::timeout(timeout_secs)
          )
        },
        error = function(e) {
          err_msg <- conditionMessage(e)
          if (grepl("Timeout was reached|Operation timed out", err_msg, ignore.case = TRUE)) {
            return(paste0("TIMEOUT_ERRORR_HTTR: API call exceeded ", timeout_secs, " seconds."))
          }
          paste0("HTTR_ERRORR: ", err_msg)
        }
      )

      if (is.character(response) && (startsWith(response, "TIMEOUT_ERRORR_HTTR:") || startsWith(response, "HTTR_ERRORR:"))) {
        last_error <- response
        next
      }

      if (http_status(response)$category != "Success") {
        error_content <- content(response, "text", encoding = "UTF-8")
        error_details <- tryCatch(
          {
            parsed <- fromJSON(error_content)
            parsed$error$message %||% parsed$error %||% parsed$message %||% error_content
          },
          error = function(e) error_content
        )
        last_error <- paste("API_ERRORR:", http_status(response)$reason, "-", error_details)
        next
      }

      result <- tryCatch(
        {
          content(response, as = "parsed", type = "application/json", encoding = "UTF-8")
        },
        error = function(e) NULL
      )
      if (is.null(result)) {
        last_error <- "API_RESPONSE_ERRORR: Invalid JSON returned by llama-cpp server."
        next
      }

      first_choice <- NULL
      if (is.list(result$choices) && length(result$choices) >= 1) {
        first_choice <- result$choices[[1]]
      }

      message_content <- first_choice$message %||% result$message
      function_result <- NULL
      if (!is.null(message_content$tool_calls) || !is.null(message_content$function_call)) {
        function_result <- result
      }

      if (!is.null(function_result)) {
        if (inherits(function_result, "htmlwidget")) {
          return(as.character(function_result))
        }
        return(function_result)
      }

      response_text <- message_content$content %||% first_choice$text %||% result$content %||% result$response
      if (!is.null(response_text) && nzchar(trimws(paste(response_text, collapse = "")))) {
        return(response_text)
      }

      finish_reason <- first_choice$finish_reason %||% ""
      if (identical(finish_reason, "content_filter")) {
        warning("Content blocked by filter (llama-cpp/model).")
        return("CONTENT_FILTERED: Response blocked by content filter.")
      }

      last_error <- "API_RESPONSE_ERRORR: No valid content, tool_calls or function_call found."
    }
  }

  last_error
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
.gen_txt_hf <- function(prompt,
                        model,
                        temp_v,
                        reasoning,
                        add_img,
                        tools = FALSE,
                        my_tools = NULL,
                        plugins = NULL,
                        timeout_secs = 80) {
  # --- Configuration ---
  if (is.null(model) || !length(model) || is.na(model[[1]]) || !nzchar(trimws(as.character(model[[1]])))) {
    model <- .HF_TEXT_DEFAULT_MODEL
  }
  api_key_hf <- Sys.getenv("HUGGINGFACE_API_TOKEN")
  if (is.null(api_key_hf) || api_key_hf == "") {
    stop("Environment variable HUGGINGFACE_API_TOKEN not set.")
  }
  # Using the TGI OpenAI-compatible endpoint format
  api_url <- paste0("https://router.huggingface.co/hf-inference/", model) # Base URL
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
  if (!is.null(reasoning)) {
    body$reasoning <- list(effort = reasoning)
  }
  if (tools) {
    tool_declarations <- tryCatch(
      {
        if (is.function(my_tools)) {
          my_tools()
        } else {
          my_tools
        }
      },
      error = function(e) {
        warning("Could not get function declarations for HuggingFace TGI. Tools disabled.")
        return(NULL)
      }
    )
    if (!is.null(tool_declarations)) {
      body$tools <- tool_declarations
      body$tool_choice <- "auto"
    } else {
      tools <- FALSE
    }
  }
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
  response <- tryCatch(
    {
      httr::POST(
        url = api_url,
        headers,
        body = json_body,
        encode = "json",
        config = httr::timeout(timeout_secs)
      )
    },
    error = function(e) {
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
    }
  )

  # --- Response Processing ---
  if (is.character(response) && (startsWith(response, "TIMEOUT_ERRORR_HTTR:") || startsWith(response, "HTTR_ERRORR:") || startsWith(response, "HF_MODEL_LOADING:"))) {
    return(response)
  }

  if (identical(httr::status_code(response), 503L)) {
    loading_content <- tryCatch(
      httr::content(response, "text", encoding = "UTF-8"),
      error = function(e) ""
    )
    loading_detail <- if (nzchar(loading_content)) {
      paste0(" ", substr(loading_content, 1L, 500L))
    } else {
      ""
    }
    return(paste0(
      "HF_MODEL_LOADING: Hugging Face returned HTTP 503 while loading model `",
      model,
      "`.",
      loading_detail
    ))
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

  result <- tryCatch(
    {
      content(response, as = "parsed", type = "application/json", encoding = "UTF-8")
    },
    error = function(e) {
      warning(paste("Error parsing HuggingFace response JSON:", conditionMessage(e)))
      return(NULL)
    }
  )

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
    if (inherits(function_result, "htmlwidget")) {
      return(as.character(function_result))
    } else {
      return(function_result)
    }
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
#' @param service Character; provider identifier (e.g. `"openai"`,
#'   `"openrouter"`, `"anthropic"`, `"groq"`, `"cerebras"`, `"together"`,
#'   `"sambanova"`, `"nebius"`, `"deepseek"`, `"perplexity"`, `"fireworks"`,
#'   `"deepinfra"`, `"hyperbolic"`, `"gemini"`, `"hf"`, `"ollama"`,
#'   `"llamacpp"`), or a custom id configured with
#'   [set_provider_openai_compat()].
#' @param model Character; model identifier for the chosen `service`.
#' @param temp Optional numeric; sampling temperature. If `NULL`, defaults to 1.
#' @param reasoning One of minimal, low, medium, high, or xhigh.
#' @param tools Logical, list, or JSON object. Use `TRUE` with definitions in
#'   `my_tools`, or pass tool definitions directly as a list/JSON object.
#' @param plugins Optional list or JSON string describing provider-specific
#'   plugins/extensions to include in the request.
#' @param my_tools Function or list containing tool definitions. Used when
#'   `tools = TRUE`.
#' @param timeout_api Numeric; request timeout (seconds) passed to the provider
#'   call.
#' @param base_url Optional explicit base URL for local Ollama or llama-cpp
#'   services. Explicit values take precedence over environment variables,
#'   local inference configuration, and built-in defaults.
#' @param null_repeat Logical; if `TRUE`, retries on empty responses with
#'   progressive waits. Hugging Face model-loading responses use shorter
#'   retries and remain structured errors if the model is still unavailable.
#' @param persist Logical; whether to save the response artifact and generation
#'   statistics. Set to `FALSE` when another package owns result persistence.
#' @param ... Reserved for future provider-specific arguments. Supplying values
#'   currently raises an error instead of silently ignoring them.
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
  reasoning = NULL,
  tools = FALSE,
  plugins = NULL,
  my_tools = NULL,
  timeout_api = 240,
  base_url = NULL,
  null_repeat = TRUE,
  persist = TRUE,
  ...
) {
  model_was_default <- missing(model)
  dots <- list(...)
  if (length(dots)) {
    dot_names <- names(dots)
    if (is.null(dot_names)) {
      dot_names <- rep("", length(dots))
    }
    dot_names[!nzchar(dot_names)] <- "<unnamed>"
    stop(
      "`...` is reserved and is not forwarded silently. Unsupported argument(s): ",
      paste(unique(dot_names), collapse = ", "),
      ". Use an explicit `gen_txt()` argument instead.",
      call. = FALSE
    )
  }
  if (!is.logical(persist) || length(persist) != 1L || is.na(persist)) {
    stop("`persist` must be TRUE or FALSE.", call. = FALSE)
  }

  # Helpers
  is_emptyish <- function(x) {
    if (is.null(x)) {
      return(TRUE)
    }
    if (length(x) == 0) {
      return(TRUE)
    }
    if (is.character(x)) {
      return(nchar(trimws(paste(x, collapse = ""))) == 0)
    }
    FALSE
  }

  # Ensure output directory exists
  if (is.null(directory) || is.na(directory)) {
    directory <- .genflow_default_dir("texts")
  }

  # Normalize inputs possibly coming as lists/vectors
  if (is.list(service)) service <- as.character(service$service %||% service[[1]]) else if (is.vector(service)) service <- as.character(service[1])
  service <- .genflow_normalize_service_alias(service)
  service_lower <- tolower(service)

  base_url_value <- as.character(base_url %||% "")[1]
  if (is.na(base_url_value)) {
    base_url_value <- ""
  }
  if (nzchar(trimws(base_url_value)) && !service_lower %in% c("ollama", "llamacpp")) {
    stop("`base_url` is only supported for local Ollama and llama-cpp text services.", call. = FALSE)
  }
  base_url <- if (nzchar(trimws(base_url_value))) base_url_value else NULL

  custom_provider_cfg <- .genflow_get_custom_provider(service)

  if (is.list(model)) {
    if (!length(model)) {
      model <- NULL
    } else {
      model_value <- model$model %||% model[[1]]
      model <- if (is.null(model_value) || !length(model_value)) NULL else as.character(model_value[[1]])
    }
  } else if (is.vector(model) && length(model)) {
    model <- as.character(model[[1]])
  }
  model_is_empty <- is.null(model) ||
    !length(model) ||
    is.na(model[[1]]) ||
    !nzchar(trimws(as.character(model[[1]])))
  if (identical(service_lower, "hf") &&
      (isTRUE(model_was_default) || model_is_empty || identical(as.character(model[[1]]), "gpt-5-mini"))) {
    model <- .HF_TEXT_DEFAULT_MODEL
  }
  if (identical(service_lower, "gemini") &&
      (isTRUE(model_was_default) || model_is_empty ||
       identical(as.character(model[[1]]), "gpt-5-mini"))) {
    model <- .gemini_resolve_model(NULL)
  }
  if (is.list(temp)) temp <- as.numeric(temp$temperature %||% temp$temp %||% temp[[1]]) else if (is.vector(temp)) temp <- as.numeric(temp[1])
  if (!is.null(custom_provider_cfg)) {
    model <- .custom_openai_provider_resolve_model(custom_provider_cfg, model, timeout_secs = timeout_api)
  } else if (identical(tolower(service), "anthropic")) {
    model <- .anthropic_resolve_model(model, timeout_secs = timeout_api)
  } else if (identical(tolower(service), "nebius")) {
    model <- .nebius_resolve_model(model, timeout_secs = timeout_api)
  } else if (identical(tolower(service), "deepseek")) {
    model <- .deepseek_resolve_model(model, timeout_secs = timeout_api)
  } else if (identical(tolower(service), "perplexity")) {
    model <- .perplexity_resolve_model(model, timeout_secs = timeout_api)
  } else if (identical(tolower(service), "fireworks")) {
    model <- .fireworks_resolve_model(model, timeout_secs = timeout_api)
  } else if (identical(tolower(service), "deepinfra")) {
    model <- .deepinfra_resolve_model(model, timeout_secs = timeout_api)
  } else if (identical(tolower(service), "hyperbolic")) {
    model <- .hyperbolic_resolve_model(model, timeout_secs = timeout_api)
  } else if (identical(tolower(service), "groq")) {
    model <- .groq_resolve_model(model, timeout_secs = timeout_api)
  } else if (identical(tolower(service), "cerebras")) {
    model <- .cerebras_resolve_model(model, timeout_secs = timeout_api)
  } else if (identical(tolower(service), "together")) {
    model <- .together_resolve_model(model, timeout_secs = timeout_api)
  } else if (identical(tolower(service), "sambanova")) {
    model <- .sambanova_resolve_model(model, timeout_secs = timeout_api)
  } else if (identical(service_lower, "gemini")) {
    model <- .gemini_resolve_model(model)
  } else if (identical(service_lower, "ollama")) {
    model <- .ollama_resolve_model(
      model,
      timeout_secs = timeout_api,
      base_url = base_url
    )
  } else if (identical(service_lower, "llamacpp")) {
    model <- .llamacpp_resolve_model(
      model,
      timeout_secs = timeout_api,
      base_url = base_url
    )
  }
  temp_v <- ifelse(is.null(temp) || !is.numeric(temp) || is.na(temp), 0.7, temp)
  reasoning_v <- NULL
  if (!is.null(reasoning)) {
    reasoning_value <- reasoning
    if (is.list(reasoning_value)) {
      reasoning_value <- reasoning_value[[1]]
    }
    reasoning_value <- as.character(reasoning_value)[1]
    reasoning_value <- tolower(trimws(reasoning_value))
    valid_reasoning <- c("minimal", "low", "medium", "high", "xhigh")
    if (is.na(reasoning_value) || !nzchar(reasoning_value) || !(reasoning_value %in% valid_reasoning)) {
      stop("Invalid `reasoning` value. Choose one of: minimal, low, medium, high, xhigh.")
    }
    reasoning_v <- reasoning_value
  }

  if (is.null(tools)) {
    tools_flag <- FALSE
    tools_payload <- NULL
  } else if (is.logical(tools)) {
    if (length(tools) != 1L || is.na(tools)) {
      stop("`tools` must be one TRUE/FALSE value, a list, or valid JSON.", call. = FALSE)
    }
    tools_flag <- isTRUE(tools)
    tools_payload <- if (tools_flag) my_tools else NULL
  } else {
    tools_flag <- TRUE
    tools_payload <- tools
  }
  if (tools_flag) {
    tools_payload <- .text_normalize_tools_payload(
      tools_payload,
      argument = if (isTRUE(tools)) "`my_tools`" else "`tools`"
    )
    if (is.null(tools_payload)) {
      stop("`tools = TRUE` requires serializable tool definitions in `my_tools`.", call. = FALSE)
    }
  }

  plugins_payload <- NULL
  if (!is.null(plugins)) {
    candidate <- plugins
    if (is.character(candidate) && length(candidate) == 1) {
      parsed <- tryCatch(fromJSON(candidate, simplifyVector = FALSE), error = function(e) NULL)
      if (!is.null(parsed)) {
        candidate <- parsed
      } else {
        stop("`plugins` must be a list or valid JSON describing plugins.", call. = FALSE)
      }
    }
    if (!is.list(candidate)) {
      stop("`plugins` must be a list or valid JSON describing plugins.", call. = FALSE)
    }
    plugins_payload <- candidate
  }

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

  # Wrapper that dispatches to provider-specific functions.
  .do_call <- function(service, prompt, model, temp_v, reasoning, add_img, tools,
                       my_tools, plugins, timeout_api, base_url) {
    custom_cfg <- .genflow_get_custom_provider(service)
    if (!is.null(custom_cfg)) {
      return(.gen_txt_custom_openai_provider(
        provider_cfg = custom_cfg,
        prompt = prompt,
        model = model,
        temp_v = temp_v,
        reasoning = reasoning,
        add_img = add_img,
        tools = tools,
        my_tools = my_tools,
        plugins = plugins,
        timeout_secs = timeout_api
      ))
    }
    switch(tolower(service),
      "openai" = .gen_txt_openai(prompt, model, temp_v, reasoning, add_img, tools = tools, my_tools = my_tools, plugins = plugins, timeout_secs = timeout_api),
      "openrouter" = .gen_txt_openrouter(prompt, model, temp_v, reasoning, add_img, tools = tools, my_tools = my_tools, plugins = plugins, timeout_secs = timeout_api),
      "anthropic" = .gen_txt_anthropic(prompt, model, temp_v, reasoning, add_img, tools = tools, my_tools = my_tools, plugins = plugins, timeout_secs = timeout_api),
      "nebius" = .gen_txt_nebius(prompt, model, temp_v, reasoning, add_img, tools = tools, my_tools = my_tools, plugins = plugins, timeout_secs = timeout_api),
      "deepseek" = .gen_txt_deepseek(prompt, model, temp_v, reasoning, add_img, tools = tools, my_tools = my_tools, plugins = plugins, timeout_secs = timeout_api),
      "perplexity" = .gen_txt_perplexity(prompt, model, temp_v, reasoning, add_img, tools = tools, my_tools = my_tools, plugins = plugins, timeout_secs = timeout_api),
      "fireworks" = .gen_txt_fireworks(prompt, model, temp_v, reasoning, add_img, tools = tools, my_tools = my_tools, plugins = plugins, timeout_secs = timeout_api),
      "deepinfra" = .gen_txt_deepinfra(prompt, model, temp_v, reasoning, add_img, tools = tools, my_tools = my_tools, plugins = plugins, timeout_secs = timeout_api),
      "hyperbolic" = .gen_txt_hyperbolic(prompt, model, temp_v, reasoning, add_img, tools = tools, my_tools = my_tools, plugins = plugins, timeout_secs = timeout_api),
      "cerebras" = .gen_txt_cerebras(prompt, model, temp_v, reasoning, add_img, tools = tools, my_tools = my_tools, plugins = plugins, timeout_secs = timeout_api),
      "together" = .gen_txt_together(prompt, model, temp_v, reasoning, add_img, tools = tools, my_tools = my_tools, plugins = plugins, timeout_secs = timeout_api),
      "sambanova" = .gen_txt_sambanova(prompt, model, temp_v, reasoning, add_img, tools = tools, my_tools = my_tools, plugins = plugins, timeout_secs = timeout_api),
      "groq" = .gen_txt_groq(prompt, model, temp_v, reasoning, add_img, tools = tools, my_tools = my_tools, plugins = plugins, timeout_secs = timeout_api),
      "gemini" = .gen_txt_gemini(prompt, model, temp_v, reasoning, add_img, tools = tools, my_tools = my_tools, plugins = plugins, timeout_secs = timeout_api),
      "hf" = .gen_txt_hf(prompt, model, temp_v, reasoning, add_img, tools = tools, my_tools = my_tools, plugins = plugins, timeout_secs = timeout_api),
      "ollama" = .gen_txt_ollama(prompt, model, temp_v, reasoning, add_img, tools = tools, my_tools = my_tools, plugins = plugins, timeout_secs = timeout_api, base_url = base_url),
      "llamacpp" = .gen_txt_llamacpp(prompt, model, temp_v, reasoning, add_img, tools = tools, my_tools = my_tools, plugins = plugins, timeout_secs = timeout_api, base_url = base_url),
      paste0("SERVICE_NOT_IMPLEMENTED: ", service)
    )
  }

  # Call provider with timing
  sent_at <- Sys.time()
  api_call_error <- NULL

  response_api <- tryCatch(
    .do_call(
      service, prompt, model, temp_v, reasoning_v, add_img, tools_flag,
      tools_payload, plugins_payload, timeout_api, base_url
    ),
    error = function(e) {
      api_call_error <<- paste("Error during API call execution:", conditionMessage(e))
      api_call_error
    }
  )

  is_empty_response <- function(value) {
    identical(value, "EMPTY_OR_NULL_RESPONSE") || is_emptyish(value)
  }
  is_retryable_response <- function(value) {
    is_empty_response(value) || .text_is_hf_loading(value)
  }

  # Optional retry on empty/null and temporary Hugging Face model loading.
  if (isTRUE(null_repeat) && is_retryable_response(response_api)) {
    hf_loading_retry <- .text_is_hf_loading(response_api)
    retry_delays <- if (hf_loading_retry) c(2, 5, 10) else c(10, 60, 600)
    for (wait_sec in retry_delays) {
      retry_reason <- if (.text_is_hf_loading(response_api)) {
        "Hugging Face model is still loading"
      } else {
        "Empty response detected"
      }
      message(sprintf("%s; waiting %ds and trying again...", retry_reason, wait_sec))
      .text_retry_sleep(wait_sec)
      response_api <- tryCatch(
        .do_call(
          service, prompt, model, temp_v, reasoning_v, add_img, tools_flag,
          tools_payload, plugins_payload, timeout_api, base_url
        ),
        error = function(e) paste("HTTR_ERRORR: Error during API call execution:", conditionMessage(e))
      )
      if (!is_retryable_response(response_api)) {
        message("Received a terminal response on retry. Proceeding.")
        break
      }
    }
    if (is_empty_response(response_api)) {
      stop(
        "EMPTY_OR_NULL_RESPONSE persisted after retries (",
        paste(retry_delays, collapse = "s, "),
        "s). Aborting.",
        call. = FALSE
      )
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
      startsWith(response_api, "HF_MODEL_LOADING:") ||
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
  if (isTRUE(persist)) {
    try(
      {
        .save_response(
          response_api = response_value_final,
          context = context,
          res_context = res_context,
          label = label_base,
          label_cat = label_cat,
          service = service,
          model = model,
          temp = temp_v,
          duration_response = duration_response,
          directory = directory,
          status = ifelse(houve_erro_api, "ERROR", "SUCCESS"),
          tokens_sent = tokens_sent,
          tokens_received = tokens_received %||% NA_integer_
        )
      },
      silent = TRUE
    )
  }

  # Final structured return
  result <- list(
    response_value = response_value_final,
    label = label_base,
    label_cat = label_cat,
    service = service,
    model = model,
    temp = temp_v,
    duration = duration_response,
    status_api = ifelse(houve_erro_api, "ERROR", "SUCCESS"),
    status_msg = status_msg,
    tokens_sent = tokens_sent,
    tokens_received = tokens_received
  )
  return(result)
}

#' @rdname gen_txt
#' @method gen_txt genflow_agent
#' @details For a `genflow_agent`, supply `context_override` through `...` to
#'   replace the saved context for one call.
#' @export
gen_txt.genflow_agent <- function(context, ...) {
  agent <- context
  overrides <- list(...)
  formals_default <- formals(gen_txt.default)
  agent_args <- .genflow_prepare_agent_args(
    agent = agent,
    overrides = overrides,
    target_formals = formals_default,
    required = "context",
    override_aliases = c(context_override = "context"),
    override_label = "gen_txt()"
  )
  do.call(gen_txt.default, agent_args, quote = TRUE)
}
