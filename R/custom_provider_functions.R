#' @keywords internal
#' @noRd
.genflow_builtin_provider_labels <- function() {
  c(
    openai = "OpenAI",
    openrouter = "OpenRouter",
    anthropic = "Anthropic",
    groq = "Groq",
    cerebras = "Cerebras",
    together = "Together",
    sambanova = "SambaNova",
    nebius = "Nebius",
    deepseek = "DeepSeek",
    perplexity = "Perplexity",
    fireworks = "Fireworks",
    deepinfra = "DeepInfra",
    hyperbolic = "Hyperbolic",
    hf = "Hugging Face",
    ollama = "Ollama",
    llamacpp = "llama-cpp",
    gemini = "Gemini",
    fal = "FAL",
    replicate = "Replicate"
  )
}

#' @keywords internal
#' @noRd
.genflow_builtin_services <- function() {
  names(.genflow_builtin_provider_labels())
}

#' @keywords internal
#' @noRd
.genflow_provider_registry_path <- function() {
  option_path <- trimws(as.character(getOption("genflow.providers_path", ""))[1])
  if (nzchar(option_path)) {
    dir.create(dirname(option_path), recursive = TRUE, showWarnings = FALSE)
    return(option_path)
  }

  option_dir <- trimws(as.character(getOption("genflow.providers_dir", ""))[1])
  if (!nzchar(option_dir)) {
    option_dir <- tools::R_user_dir("genflow", which = "config")
  }
  if (!dir.exists(option_dir)) {
    dir.create(option_dir, recursive = TRUE, showWarnings = FALSE)
  }
  file.path(option_dir, "openai_compatible_providers.json")
}

#' @keywords internal
#' @noRd
.genflow_normalize_provider_id <- function(id, allow_empty = FALSE) {
  id_chr <- tolower(trimws(as.character(id %||% "")[1]))
  if (!nzchar(id_chr)) {
    if (isTRUE(allow_empty)) {
      return("")
    }
    stop("`id` must be a non-empty string.", call. = FALSE)
  }
  if (!grepl("^[a-z][a-z0-9_-]*$", id_chr)) {
    stop("`id` must match ^[a-z][a-z0-9_-]*$.", call. = FALSE)
  }
  reserved <- c("all", "favorites")
  if (id_chr %in% reserved) {
    stop("`id` is reserved and cannot be used as a provider id.", call. = FALSE)
  }
  id_chr
}

#' @keywords internal
#' @noRd
.genflow_normalize_base_urls <- function(base_urls) {
  vals <- as.character(unlist(base_urls %||% character(), use.names = FALSE))
  vals <- trimws(vals)
  vals <- vals[nzchar(vals)]
  if (!length(vals)) {
    return(character())
  }
  vals <- sub("/+$", "", vals)
  vals <- sub("/v1$", "", vals, ignore.case = TRUE)
  invalid <- vals[!grepl("^https?://", vals, ignore.case = TRUE)]
  if (length(invalid)) {
    stop("`base_url(s)` must be absolute http(s) URLs: ", paste(unique(invalid), collapse = ", "), call. = FALSE)
  }
  unique(vals)
}

#' @keywords internal
#' @noRd
.genflow_normalize_endpoint_paths <- function(paths, defaults) {
  vals <- as.character(unlist(paths %||% character(), use.names = FALSE))
  vals <- trimws(vals)
  vals <- vals[nzchar(vals)]
  if (!length(vals)) {
    vals <- defaults
  }
  vals <- vapply(vals, function(p) {
    if (grepl("^https?://", p, ignore.case = TRUE)) {
      return(sub("/+$", "", p))
    }
    if (!startsWith(p, "/")) {
      p <- paste0("/", p)
    }
    p
  }, character(1))
  unique(vals)
}

#' @keywords internal
#' @noRd
.genflow_normalize_headers <- function(extra_headers) {
  if (is.null(extra_headers)) {
    return(list())
  }
  if (is.data.frame(extra_headers) && all(c("name", "value") %in% tolower(names(extra_headers)))) {
    nms <- names(extra_headers)
    name_col <- nms[tolower(nms) == "name"][1]
    value_col <- nms[tolower(nms) == "value"][1]
    extra_headers <- stats::setNames(as.list(as.character(extra_headers[[value_col]])), as.character(extra_headers[[name_col]]))
  }
  if (!is.list(extra_headers)) {
    extra_headers <- as.list(extra_headers)
  }
  if (!length(extra_headers)) {
    return(list())
  }
  nms <- names(extra_headers)
  if (is.null(nms) || any(!nzchar(trimws(nms)))) {
    stop("`extra_headers` must be a named list/vector.", call. = FALSE)
  }
  out <- list()
  for (nm in nms) {
    value <- as.character(extra_headers[[nm]] %||% "")[1]
    value <- trimws(value)
    if (!nzchar(nm) || !nzchar(value)) {
      next
    }
    out[[nm]] <- value
  }
  out
}

#' @keywords internal
#' @noRd
.genflow_as_flag <- function(x, default = FALSE) {
  if (is.null(x) || length(x) == 0 || is.na(x[1])) {
    return(isTRUE(default))
  }
  isTRUE(as.logical(x[1]))
}

#' @keywords internal
#' @noRd
.genflow_normalize_custom_provider_config <- function(config, strict = TRUE) {
  if (!is.list(config)) {
    stop("Provider config must be a list.", call. = FALSE)
  }

  id <- .genflow_normalize_provider_id(config$id)
  if (id %in% .genflow_builtin_services()) {
    stop("`id` '", id, "' conflicts with a built-in provider id.", call. = FALSE)
  }

  label <- as.character(config$label %||% "")[1]
  label <- trimws(label)
  if (!nzchar(label)) {
    label <- tools::toTitleCase(gsub("[-_]+", " ", id))
  }

  candidate_base_urls <- config$base_urls %||% config$base_url
  base_urls <- .genflow_normalize_base_urls(candidate_base_urls)
  if (isTRUE(strict) && !length(base_urls)) {
    stop("At least one base URL must be provided.", call. = FALSE)
  }

  chat_paths <- .genflow_normalize_endpoint_paths(
    config$chat_paths,
    defaults = c("/v1/chat/completions", "/chat/completions")
  )
  model_paths <- .genflow_normalize_endpoint_paths(
    config$model_paths,
    defaults = c("/v1/models", "/models")
  )

  api_key_env <- trimws(as.character(config$api_key_env %||% "")[1])
  model_env <- trimws(as.character(config$model_env %||% "")[1])
  default_model <- trimws(as.character(config$default_model %||% "local-model")[1])
  if (!nzchar(default_model)) {
    default_model <- "local-model"
  }

  auth_header <- trimws(as.character(config$auth_header %||% "Authorization")[1])
  auth_prefix <- as.character(config$auth_prefix %||% "Bearer")[1]
  if (is.na(auth_prefix)) {
    auth_prefix <- "Bearer"
  }
  auth_prefix <- trimws(auth_prefix)

  extra_headers <- .genflow_normalize_headers(config$extra_headers)

  api_key_required <- config$api_key_required
  if (is.null(api_key_required) || length(api_key_required) == 0 || is.na(api_key_required[1])) {
    api_key_required <- nzchar(api_key_env)
  }
  api_key_required <- .genflow_as_flag(api_key_required, default = nzchar(api_key_env))

  supports_tools <- .genflow_as_flag(config$supports_tools, default = TRUE)
  supports_vision <- .genflow_as_flag(config$supports_vision, default = TRUE)
  supports_reasoning <- .genflow_as_flag(config$supports_reasoning, default = FALSE)
  supports_plugins <- .genflow_as_flag(config$supports_plugins, default = FALSE)

  reasoning_field <- trimws(as.character(config$reasoning_field %||% "")[1])
  if (!nzchar(reasoning_field)) {
    reasoning_field <- NULL
  }
  plugins_field <- trimws(as.character(config$plugins_field %||% "")[1])
  if (!nzchar(plugins_field)) {
    plugins_field <- NULL
  }

  max_tokens <- suppressWarnings(as.numeric(config$max_tokens %||% NA_real_))[1]
  if (is.na(max_tokens) || !is.finite(max_tokens) || max_tokens <= 0) {
    max_tokens <- NULL
  } else {
    max_tokens <- as.integer(max_tokens)
  }

  list(
    id = id,
    label = label,
    kind = "openai_compat",
    base_urls = base_urls,
    chat_paths = chat_paths,
    model_paths = model_paths,
    api_key_env = api_key_env,
    model_env = model_env,
    default_model = default_model,
    auth_header = auth_header,
    auth_prefix = auth_prefix,
    extra_headers = extra_headers,
    api_key_required = api_key_required,
    supports_tools = supports_tools,
    supports_vision = supports_vision,
    supports_reasoning = supports_reasoning,
    supports_plugins = supports_plugins,
    reasoning_field = reasoning_field,
    plugins_field = plugins_field,
    max_tokens = max_tokens
  )
}

#' @keywords internal
#' @noRd
.genflow_load_custom_providers <- function() {
  path <- .genflow_provider_registry_path()
  if (!file.exists(path)) {
    return(list())
  }

  parsed <- tryCatch(
    {
      jsonlite::fromJSON(path, simplifyVector = FALSE)
    },
    error = function(e) {
      warning("Failed to parse custom provider registry at ", path, ": ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(parsed)) {
    return(list())
  }

  raw_providers <- parsed$providers %||% parsed
  if (!is.list(raw_providers) || !length(raw_providers)) {
    return(list())
  }

  if (is.null(names(raw_providers)) || any(!nzchar(names(raw_providers)))) {
    normalized <- list()
    for (entry in raw_providers) {
      if (!is.list(entry)) {
        next
      }
      entry_id <- tryCatch(.genflow_normalize_provider_id(entry$id), error = function(e) "")
      if (!nzchar(entry_id)) {
        next
      }
      normalized[[entry_id]] <- entry
    }
    raw_providers <- normalized
  }

  providers <- list()
  for (nm in names(raw_providers)) {
    cfg <- tryCatch(
      {
        .genflow_normalize_custom_provider_config(
          config = c(list(id = nm), raw_providers[[nm]]),
          strict = TRUE
        )
      },
      error = function(e) {
        warning("Skipping invalid custom provider '", nm, "': ", conditionMessage(e))
        NULL
      }
    )
    if (!is.null(cfg)) {
      providers[[cfg$id]] <- cfg
    }
  }
  providers
}

#' @keywords internal
#' @noRd
.genflow_save_custom_providers <- function(providers) {
  providers <- providers %||% list()
  providers <- providers[vapply(providers, is.list, logical(1))]

  path <- .genflow_provider_registry_path()
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  payload <- list(schema_version = 1L, providers = providers)
  json_txt <- jsonlite::toJSON(payload, auto_unbox = TRUE, pretty = TRUE, null = "null")
  writeLines(enc2utf8(json_txt), path, useBytes = TRUE)
  invisible(path)
}

#' @keywords internal
#' @noRd
.genflow_list_custom_provider_configs <- function() {
  .genflow_load_custom_providers()
}

#' @keywords internal
#' @noRd
.genflow_get_custom_provider <- function(id) {
  id_norm <- tryCatch(.genflow_normalize_provider_id(id), error = function(e) "")
  if (!nzchar(id_norm)) {
    return(NULL)
  }
  providers <- .genflow_load_custom_providers()
  providers[[id_norm]]
}

#' @keywords internal
#' @noRd
.genflow_custom_provider_labels <- function() {
  providers <- .genflow_load_custom_providers()
  if (!length(providers)) {
    return(character())
  }
  labels <- vapply(providers, function(cfg) {
    label <- as.character(cfg$label %||% cfg$id)[1]
    if (!nzchar(trimws(label))) cfg$id else trimws(label)
  }, character(1))
  stats::setNames(labels, names(providers))
}

#' @keywords internal
#' @noRd
.genflow_clean_chr <- function(values) {
  vals <- as.character(values %||% character())
  vals <- trimws(vals)
  vals <- vals[nzchar(vals)]
  unique(vals)
}

#' @keywords internal
#' @noRd
.genflow_extract_openai_compat_model_ids <- function(parsed_content) {
  if (!is.list(parsed_content)) {
    return(character())
  }

  models_obj <- parsed_content$data %||% parsed_content$models %||% parsed_content$result
  if (is.null(models_obj)) {
    return(character())
  }

  ids <- character()
  if (is.data.frame(models_obj) && nrow(models_obj) > 0) {
    cols <- c("id", "model", "name")
    available_cols <- cols[cols %in% names(models_obj)]
    if (length(available_cols)) {
      ids <- vapply(seq_len(nrow(models_obj)), function(i) {
        candidate <- ""
        for (col_name in available_cols) {
          value <- models_obj[[col_name]][i]
          value_chr <- trimws(as.character(value %||% "")[1])
          if (nzchar(value_chr)) {
            candidate <- value_chr
            break
          }
        }
        candidate
      }, character(1))
    }
  } else if (is.list(models_obj) && length(models_obj) > 0) {
    ids <- vapply(models_obj, function(entry) {
      if (is.list(entry)) {
        value <- entry$id %||% entry$model %||% entry$name
      } else if (is.character(entry) && length(entry) == 1) {
        value <- entry
      } else {
        value <- ""
      }
      trimws(as.character(value %||% "")[1])
    }, character(1), USE.NAMES = FALSE)
  } else if (is.character(models_obj)) {
    ids <- as.character(models_obj)
  }

  .genflow_clean_chr(ids)
}

#' @keywords internal
#' @noRd
.genflow_probe_openai_compat_models <- function(provider_cfg, timeout_secs = 20) {
  provider_cfg <- .genflow_normalize_custom_provider_config(provider_cfg, strict = TRUE)

  timeout_num <- suppressWarnings(as.numeric(timeout_secs)[1])
  if (!is.finite(timeout_num) || timeout_num <= 0) {
    timeout_num <- 20
  }

  api_key_env <- trimws(as.character(provider_cfg$api_key_env %||% "")[1])
  api_key <- if (nzchar(api_key_env)) trimws(Sys.getenv(api_key_env, "")) else ""
  api_key_required <- isTRUE(provider_cfg$api_key_required)
  if (api_key_required && !nzchar(api_key)) {
    return(list(
      success = FALSE,
      error = paste0("Missing API key in environment variable '", api_key_env, "'."),
      attempted_endpoints = character(),
      endpoint = "",
      model_ids = character(),
      api_key_env = api_key_env,
      api_key_present = FALSE
    ))
  }

  base_urls <- .genflow_normalize_base_urls(provider_cfg$base_urls %||% character())
  model_paths <- .genflow_normalize_endpoint_paths(
    provider_cfg$model_paths,
    defaults = c("/v1/models", "/models")
  )
  if (!length(base_urls)) {
    return(list(
      success = FALSE,
      error = "No valid base URL configured.",
      attempted_endpoints = character(),
      endpoint = "",
      model_ids = character(),
      api_key_env = api_key_env,
      api_key_present = nzchar(api_key)
    ))
  }

  header_args <- list("Content-Type" = "application/json")
  extra_headers <- provider_cfg$extra_headers %||% list()
  if (length(extra_headers)) {
    header_args <- c(header_args, extra_headers)
  }
  auth_header <- trimws(as.character(provider_cfg$auth_header %||% "Authorization")[1])
  auth_prefix <- trimws(as.character(provider_cfg$auth_prefix %||% "Bearer")[1])
  if (nzchar(auth_header) && nzchar(api_key)) {
    auth_value <- if (nzchar(auth_prefix)) paste(auth_prefix, api_key) else api_key
    header_args[[auth_header]] <- auth_value
  }
  headers <- do.call(httr::add_headers, header_args)

  attempts <- character()
  last_error <- "Unable to reach a valid models endpoint."

  for (base_candidate in base_urls) {
    for (model_path in model_paths) {
      if (!nzchar(model_path)) {
        next
      }

      endpoint <- if (grepl("^https?://", model_path, ignore.case = TRUE)) {
        sub("/+$", "", model_path)
      } else {
        paste0(base_candidate, model_path)
      }
      attempts <- c(attempts, endpoint)

      response <- tryCatch(
        httr::GET(endpoint, headers, httr::timeout(timeout_num)),
        error = function(e) e
      )
      if (inherits(response, "error")) {
        last_error <- paste0("Connection error at ", endpoint, ": ", conditionMessage(response))
        next
      }

      status_code <- httr::status_code(response)
      if (status_code != 200) {
        body <- tryCatch(
          trimws(httr::content(response, "text", encoding = "UTF-8")),
          error = function(e) ""
        )
        body <- substr(body, 1, 220)
        if (nzchar(body)) {
          last_error <- paste0("HTTP ", status_code, " at ", endpoint, ": ", body)
        } else {
          last_error <- paste0("HTTP ", status_code, " at ", endpoint)
        }
        next
      }

      parsed <- tryCatch(
        {
          raw_content <- httr::content(response, "raw")
          jsonlite::fromJSON(rawToChar(raw_content), simplifyVector = FALSE)
        },
        error = function(e) e
      )
      if (inherits(parsed, "error")) {
        last_error <- paste0("Invalid JSON at ", endpoint, ": ", conditionMessage(parsed))
        next
      }

      model_ids <- .genflow_extract_openai_compat_model_ids(parsed)
      return(list(
        success = TRUE,
        error = "",
        attempted_endpoints = attempts,
        endpoint = endpoint,
        model_ids = model_ids,
        api_key_env = api_key_env,
        api_key_present = nzchar(api_key)
      ))
    }
  }

  list(
    success = FALSE,
    error = last_error,
    attempted_endpoints = attempts,
    endpoint = "",
    model_ids = character(),
    api_key_env = api_key_env,
    api_key_present = nzchar(api_key)
  )
}

#' Register a custom OpenAI-compatible provider
#'
#' Saves a provider configuration that can be used as `service` in `gen_txt()`
#' and in `gen_update_models(provider = "<id>")`.
#'
#' @param id Provider id (lowercase key used in `service`), e.g. `"mycloud"`.
#' @param label Display label used in interfaces and listings.
#' @param base_url Optional single base URL.
#' @param base_urls Optional character vector of base URLs (failover order).
#' @param api_key_env Environment variable name holding the API key.
#' @param model_env Optional environment variable name with a default model.
#' @param default_model Fallback model when none is provided/discovered.
#' @param chat_paths Candidate chat endpoints (relative paths or absolute URLs).
#' @param model_paths Candidate model-list endpoints.
#' @param auth_header Header name for auth (use `""` to disable auth header).
#' @param auth_prefix Auth scheme prefix, e.g. `"Bearer"` or `""`.
#' @param extra_headers Optional named headers added to every request.
#' @param api_key_required Logical; whether missing API key should error.
#' @param supports_tools Logical capability flag.
#' @param supports_vision Logical capability flag.
#' @param supports_reasoning Logical capability flag.
#' @param supports_plugins Logical capability flag.
#' @param reasoning_field Optional payload field for reasoning, e.g. `"reasoning"`.
#' @param plugins_field Optional payload field for plugins/extensions.
#' @param max_tokens Optional max tokens sent as `max_tokens`.
#' @param overwrite Logical; overwrite existing custom provider with same id.
#'
#' @return Invisibly returns the normalized provider configuration.
#' @export
set_provider_openai_compat <- function(id,
                                       label = NULL,
                                       base_url = NULL,
                                       base_urls = NULL,
                                       api_key_env = NULL,
                                       model_env = NULL,
                                       default_model = "local-model",
                                       chat_paths = c("/v1/chat/completions", "/chat/completions"),
                                       model_paths = c("/v1/models", "/models"),
                                       auth_header = "Authorization",
                                       auth_prefix = "Bearer",
                                       extra_headers = NULL,
                                       api_key_required = NULL,
                                       supports_tools = TRUE,
                                       supports_vision = TRUE,
                                       supports_reasoning = FALSE,
                                       supports_plugins = FALSE,
                                       reasoning_field = NULL,
                                       plugins_field = NULL,
                                       max_tokens = NULL,
                                       overwrite = TRUE) {
  cfg <- .genflow_normalize_custom_provider_config(
    config = list(
      id = id,
      label = label,
      base_url = base_url,
      base_urls = base_urls,
      api_key_env = api_key_env,
      model_env = model_env,
      default_model = default_model,
      chat_paths = chat_paths,
      model_paths = model_paths,
      auth_header = auth_header,
      auth_prefix = auth_prefix,
      extra_headers = extra_headers,
      api_key_required = api_key_required,
      supports_tools = supports_tools,
      supports_vision = supports_vision,
      supports_reasoning = supports_reasoning,
      supports_plugins = supports_plugins,
      reasoning_field = reasoning_field,
      plugins_field = plugins_field,
      max_tokens = max_tokens
    ),
    strict = TRUE
  )

  providers <- .genflow_load_custom_providers()
  if (!isTRUE(overwrite) && cfg$id %in% names(providers)) {
    stop("Custom provider '", cfg$id, "' already exists. Set overwrite = TRUE to replace it.", call. = FALSE)
  }

  providers[[cfg$id]] <- cfg
  .genflow_save_custom_providers(providers)
  invisible(cfg)
}

#' List built-in and custom providers
#'
#' @param include_builtin Logical; include built-in providers.
#' @param include_custom Logical; include custom providers.
#'
#' @return Data frame with provider metadata.
#' @export
list_providers <- function(include_builtin = TRUE, include_custom = TRUE) {
  rows <- list()

  if (isTRUE(include_builtin)) {
    labels <- .genflow_builtin_provider_labels()
    rows[[length(rows) + 1L]] <- data.frame(
      id = names(labels),
      label = unname(labels),
      kind = "builtin",
      base_urls = NA_character_,
      api_key_env = NA_character_,
      model_env = NA_character_,
      default_model = NA_character_,
      stringsAsFactors = FALSE
    )
  }

  if (isTRUE(include_custom)) {
    providers <- .genflow_load_custom_providers()
    if (length(providers)) {
      rows[[length(rows) + 1L]] <- do.call(rbind, lapply(providers, function(cfg) {
        data.frame(
          id = cfg$id,
          label = cfg$label %||% cfg$id,
          kind = "custom",
          base_urls = paste(cfg$base_urls %||% character(), collapse = ";"),
          api_key_env = cfg$api_key_env %||% "",
          model_env = cfg$model_env %||% "",
          default_model = cfg$default_model %||% "",
          stringsAsFactors = FALSE
        )
      }))
    }
  }

  if (!length(rows)) {
    return(data.frame(
      id = character(),
      label = character(),
      kind = character(),
      base_urls = character(),
      api_key_env = character(),
      model_env = character(),
      default_model = character(),
      stringsAsFactors = FALSE
    ))
  }

  out <- do.call(rbind, rows)
  out <- out[order(out$kind, out$id), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Get provider definition
#'
#' @param id Provider id.
#'
#' @return A list describing the provider.
#' @export
get_provider <- function(id) {
  id_norm <- .genflow_normalize_provider_id(id)
  custom_cfg <- .genflow_get_custom_provider(id_norm)
  if (!is.null(custom_cfg)) {
    return(custom_cfg)
  }

  builtins <- .genflow_builtin_provider_labels()
  if (id_norm %in% names(builtins)) {
    return(list(
      id = id_norm,
      label = builtins[[id_norm]],
      kind = "builtin"
    ))
  }

  stop("Provider '", id_norm, "' not found.", call. = FALSE)
}

#' Remove a custom provider definition
#'
#' @param id Custom provider id.
#' @param missing_ok Logical; when TRUE, returns silently if the provider is absent.
#'
#' @return Invisibly returns `TRUE` when removed, `FALSE` when absent and
#'   `missing_ok = TRUE`.
#' @export
rm_provider <- function(id, missing_ok = FALSE) {
  id_norm <- .genflow_normalize_provider_id(id)
  if (id_norm %in% .genflow_builtin_services()) {
    stop("Cannot remove built-in provider '", id_norm, "'.", call. = FALSE)
  }

  providers <- .genflow_load_custom_providers()
  if (!id_norm %in% names(providers)) {
    if (isTRUE(missing_ok)) {
      return(invisible(FALSE))
    }
    stop("Custom provider '", id_norm, "' not found.", call. = FALSE)
  }

  providers[[id_norm]] <- NULL
  .genflow_save_custom_providers(providers)
  invisible(TRUE)
}

#' Test a custom provider connection
#'
#' Probes configured model endpoints for a custom OpenAI-compatible provider and
#' returns connection diagnostics plus sample model ids.
#'
#' @param id Custom provider id.
#' @param timeout Timeout in seconds per endpoint attempt.
#' @param max_models Maximum number of model ids returned in `models`.
#'
#' @return A list with status fields and discovered model information.
#' @export
test_provider <- function(id, timeout = 20, max_models = 20) {
  id_norm <- .genflow_normalize_provider_id(id)
  provider_cfg <- .genflow_get_custom_provider(id_norm)
  if (is.null(provider_cfg)) {
    stop("Custom provider '", id_norm, "' not found.", call. = FALSE)
  }

  max_models_int <- suppressWarnings(as.integer(max_models)[1])
  if (is.na(max_models_int) || max_models_int < 1L) {
    max_models_int <- 20L
  }

  probe <- .genflow_probe_openai_compat_models(provider_cfg, timeout_secs = timeout)
  if (!isTRUE(probe$success)) {
    return(list(
      provider_id = id_norm,
      provider_label = provider_cfg$label %||% id_norm,
      status_api = "ERROR",
      status_msg = as.character(probe$error %||% "Unable to connect to provider endpoints.")[1],
      endpoint = NA_character_,
      model_count = 0L,
      models = character(),
      attempted_endpoints = as.character(probe$attempted_endpoints %||% character()),
      api_key_env = as.character(probe$api_key_env %||% "")[1],
      api_key_present = isTRUE(probe$api_key_present)
    ))
  }

  model_ids <- as.character(probe$model_ids %||% character())
  model_ids <- model_ids[nzchar(model_ids)]
  model_count <- as.integer(length(model_ids))
  sample_models <- utils::head(model_ids, n = max_models_int)

  status_msg <- if (model_count > 0L) {
    sprintf("Connected via %s and found %d model(s).", probe$endpoint, model_count)
  } else {
    sprintf("Connected via %s but no model ids were returned.", probe$endpoint)
  }

  list(
    provider_id = id_norm,
    provider_label = provider_cfg$label %||% id_norm,
    status_api = "SUCCESS",
    status_msg = status_msg,
    endpoint = as.character(probe$endpoint %||% "")[1],
    model_count = model_count,
    models = sample_models,
    attempted_endpoints = as.character(probe$attempted_endpoints %||% character()),
    api_key_env = as.character(probe$api_key_env %||% "")[1],
    api_key_present = isTRUE(probe$api_key_present)
  )
}
