.genflow_cred_or <- function(x, y) if (!is.null(x)) x else y

.genflow_credential_spec_row <- function(provider,
                                         provider_label,
                                         env,
                                         label,
                                         kind = "api_key",
                                         sensitive = TRUE,
                                         required_for_models = FALSE,
                                         required_group = NULL,
                                         default_value = "",
                                         source_hint = "") {
  env <- trimws(as.character(env)[1])
  data.frame(
    provider = tolower(trimws(as.character(provider)[1])),
    provider_label = trimws(as.character(provider_label)[1]),
    env = env,
    label = trimws(as.character(label)[1]),
    kind = trimws(as.character(kind)[1]),
    sensitive = isTRUE(sensitive),
    required_for_models = isTRUE(required_for_models),
    required_group = trimws(as.character(.genflow_cred_or(required_group, env))[1]),
    default_value = trimws(as.character(.genflow_cred_or(default_value, ""))[1]),
    source_hint = trimws(as.character(.genflow_cred_or(source_hint, ""))[1]),
    stringsAsFactors = FALSE
  )
}

.genflow_credential_specs <- function(providers = NULL) {
  rows <- list(
    .genflow_credential_spec_row("openai", "OpenAI", "OPENAI_API_KEY", "API key", required_for_models = TRUE),

    .genflow_credential_spec_row("openrouter", "OpenRouter", "OPENROUTER_API_KEY", "API key", required_for_models = TRUE),

    .genflow_credential_spec_row("anthropic", "Anthropic", "ANTHROPIC_API_KEY", "API key", required_for_models = TRUE, required_group = "anthropic_api_key"),
    .genflow_credential_spec_row("anthropic", "Anthropic", "CLAUDE_API_KEY", "Claude API key alias", required_for_models = TRUE, required_group = "anthropic_api_key"),
    .genflow_credential_spec_row("anthropic", "Anthropic", "ANTHROPIC_BASE_URL", "Base URL", "base_url", FALSE, default_value = "https://api.anthropic.com"),
    .genflow_credential_spec_row("anthropic", "Anthropic", "ANTHROPIC_API_VERSION", "API version", "metadata", FALSE),

    .genflow_credential_spec_row("groq", "Groq", "GROQ_API_KEY", "API key", required_for_models = TRUE),
    .genflow_credential_spec_row("groq", "Groq", "GROQ_BASE_URL", "Base URL", "base_url", FALSE, default_value = "https://api.groq.com"),

    .genflow_credential_spec_row("cerebras", "Cerebras", "CEREBRAS_API_KEY", "API key", required_for_models = TRUE),
    .genflow_credential_spec_row("cerebras", "Cerebras", "CEREBRAS_BASE_URL", "Base URL", "base_url", FALSE, default_value = "https://api.cerebras.ai"),

    .genflow_credential_spec_row("together", "Together", "TOGETHER_API_KEY", "API key", required_for_models = TRUE),
    .genflow_credential_spec_row("together", "Together", "TOGETHER_BASE_URL", "Base URL", "base_url", FALSE, default_value = "https://api.together.xyz"),

    .genflow_credential_spec_row("sambanova", "SambaNova", "SAMBANOVA_API_KEY", "API key", required_for_models = TRUE, required_group = "sambanova_api_key"),
    .genflow_credential_spec_row("sambanova", "SambaNova", "SAMBA_API_KEY", "Samba API key alias", required_for_models = TRUE, required_group = "sambanova_api_key"),
    .genflow_credential_spec_row("sambanova", "SambaNova", "SAMBANOVA_BASE_URL", "Base URL", "base_url", FALSE, default_value = "https://api.sambanova.ai"),

    .genflow_credential_spec_row("nebius", "Nebius", "NEBIUS_API_KEY", "API key", required_for_models = TRUE),
    .genflow_credential_spec_row("nebius", "Nebius", "NEBIUS_BASE_URL", "Base URL", "base_url", FALSE, default_value = "https://api.studio.nebius.ai"),

    .genflow_credential_spec_row("deepseek", "DeepSeek", "DEEPSEEK_API_KEY", "API key", required_for_models = TRUE),
    .genflow_credential_spec_row("deepseek", "DeepSeek", "DEEPSEEK_BASE_URL", "Base URL", "base_url", FALSE, default_value = "https://api.deepseek.com"),

    .genflow_credential_spec_row("perplexity", "Perplexity", "PERPLEXITY_API_KEY", "API key", required_for_models = TRUE),
    .genflow_credential_spec_row("perplexity", "Perplexity", "PERPLEXITY_BASE_URL", "Base URL", "base_url", FALSE, default_value = "https://api.perplexity.ai"),

    .genflow_credential_spec_row("fireworks", "Fireworks", "FIREWORKS_API_KEY", "API key", required_for_models = TRUE),
    .genflow_credential_spec_row("fireworks", "Fireworks", "FIREWORKS_BASE_URL", "Base URL", "base_url", FALSE, default_value = "https://api.fireworks.ai/inference"),

    .genflow_credential_spec_row("deepinfra", "DeepInfra", "DEEPINFRA_API_KEY", "API key", required_for_models = TRUE),
    .genflow_credential_spec_row("deepinfra", "DeepInfra", "DEEPINFRA_BASE_URL", "Base URL", "base_url", FALSE, default_value = "https://api.deepinfra.com/v1/openai"),

    .genflow_credential_spec_row("hyperbolic", "Hyperbolic", "HYPERBOLIC_API_KEY", "API key", required_for_models = TRUE),
    .genflow_credential_spec_row("hyperbolic", "Hyperbolic", "HYPERBOLIC_BASE_URL", "Base URL", "base_url", FALSE, default_value = "https://api.hyperbolic.xyz"),

    .genflow_credential_spec_row("gemini", "Gemini", "GOOGLE_API_KEY", "Google API key", required_for_models = TRUE, required_group = "gemini_api_key"),
    .genflow_credential_spec_row("gemini", "Gemini", "GEMINI_API_KEY", "Gemini API key alias", required_for_models = TRUE, required_group = "gemini_api_key"),
    .genflow_credential_spec_row("fal", "FAL", "FAL_API_KEY", "API key", required_for_models = TRUE),
    .genflow_credential_spec_row("replicate", "Replicate", "REPLICATE_API_TOKEN", "API token", "api_token", TRUE, TRUE),

    .genflow_credential_spec_row("hf", "Hugging Face", "HUGGINGFACE_API_TOKEN", "API token", "api_token", TRUE, FALSE),
    .genflow_credential_spec_row("assemblyai", "AssemblyAI", "ASSEMBLYAI_API_KEY", "API key"),
    .genflow_credential_spec_row("cloudflare", "Cloudflare", "CLOUDFLARE_ACCOUNT_ID", "Account ID", "account_id", FALSE),
    .genflow_credential_spec_row("cloudflare", "Cloudflare", "CLOUDFLARE_API_TOKEN", "API token", "api_token"),
    .genflow_credential_spec_row("voicegain", "Voicegain", "VOICEGAIN_API_KEY", "API key"),

    .genflow_credential_spec_row("ollama", "Ollama", "OLLAMA_BASE_URL", "Base URL", "base_url", FALSE, default_value = "http://127.0.0.1:11434"),

    .genflow_credential_spec_row("llamacpp", "llama-cpp", "LLAMACPP_BASE_URL", "Base URL", "base_url", FALSE, default_value = "http://127.0.0.1:8080"),
    .genflow_credential_spec_row("llamacpp", "llama-cpp", "LLAMA_CPP_BASE_URL", "Base URL alias", "base_url", FALSE),
    .genflow_credential_spec_row("llamacpp", "llama-cpp", "LLAMACPP_API_KEY", "Optional API key", "api_key", TRUE, FALSE, required_group = "llamacpp_api_key"),
    .genflow_credential_spec_row("llamacpp", "llama-cpp", "LLAMA_CPP_API_KEY", "Optional API key alias", "api_key", TRUE, FALSE, required_group = "llamacpp_api_key")
  )

  specs <- do.call(rbind, rows)

  custom_cfgs <- tryCatch(
    {
      if (exists(".genflow_list_custom_provider_configs", mode = "function", inherits = TRUE)) {
        .genflow_list_custom_provider_configs()
      } else {
        list()
      }
    },
    error = function(e) list()
  )
  specs <- unique(specs)
  if (!is.null(providers)) {
    providers <- tolower(trimws(as.character(providers)))
    providers <- providers[nzchar(providers)]
    specs <- specs[specs$provider %in% providers, , drop = FALSE]
  }
  rownames(specs) <- NULL
  specs
}

.genflow_credential_providers <- function() {
  specs <- .genflow_credential_specs()
  if (!nrow(specs)) {
    return(setNames(character(), character()))
  }
  labels <- tapply(specs$provider_label, specs$provider, function(x) x[[1]])
  ids <- names(labels)
  stats::setNames(ids, labels)
}

.genflow_sanitize_input_id <- function(value) {
  gsub("[^A-Za-z0-9_]", "_", as.character(value)[1], perl = TRUE)
}

.genflow_is_valid_env_name <- function(value) {
  grepl("^[A-Za-z_][A-Za-z0-9_]*$", as.character(value)[1], perl = TRUE)
}

.genflow_mask_secret <- function(value) {
  value <- as.character(.genflow_cred_or(value, ""))[1]
  if (!nzchar(value)) {
    return("")
  }
  n <- nchar(value, type = "chars")
  if (n <= 6) {
    return(paste0(substr(value, 1, 1), "...", substr(value, n, n)))
  }
  paste0(substr(value, 1, 4), "...", substr(value, max(1, n - 3), n))
}

.genflow_credentials_path <- function(path = NULL) {
  configured <- .genflow_cred_or(path, getOption("genflow.renviron_path", NULL))
  if (is.null(configured) || !nzchar(trimws(as.character(configured)[1]))) {
    configured <- "~/.Renviron"
  }
  path.expand(as.character(configured)[1])
}

.genflow_env_source_paths <- function() {
  paths <- c(
    user_renviron = path.expand("~/.Renviron"),
    project_renviron = file.path(getwd(), ".Renviron"),
    project_env = file.path(getwd(), ".env"),
    bashrc = path.expand("~/.bashrc"),
    zshrc = path.expand("~/.zshrc")
  )
  normalized <- normalizePath(paths, winslash = "/", mustWork = FALSE)
  paths[!duplicated(normalized)]
}

.genflow_unquote_env_value <- function(value) {
  value <- trimws(as.character(.genflow_cred_or(value, ""))[1])
  if (!nzchar(value)) {
    return("")
  }
  first <- substr(value, 1, 1)
  last <- substr(value, nchar(value), nchar(value))
  if (nchar(value) >= 2 && identical(first, last) && first %in% c("\"", "'")) {
    value <- substr(value, 2, nchar(value) - 1)
    if (identical(first, "\"")) {
      value <- gsub("\\\\([\"\\\\])", "\\1", value, perl = TRUE)
    }
  }
  value
}

.genflow_read_env_assignments <- function(path, vars = NULL, source = NULL) {
  path <- path.expand(as.character(path)[1])
  empty <- data.frame(
    env = character(),
    value = character(),
    source = character(),
    path = character(),
    line = integer(),
    stringsAsFactors = FALSE
  )
  if (!file.exists(path)) {
    return(empty)
  }
  vars <- unique(trimws(as.character(.genflow_cred_or(vars, character()))))
  vars <- vars[nzchar(vars)]
  source <- .genflow_cred_or(source, basename(path))
  lines <- readLines(path, warn = FALSE)
  rows <- list()
  for (idx in seq_along(lines)) {
    text <- trimws(lines[[idx]])
    if (!nzchar(text) || startsWith(text, "#")) {
      next
    }
    text <- sub("^export[[:space:]]+", "", text, perl = TRUE)
    match <- regexec("^([A-Za-z_][A-Za-z0-9_]*)[[:space:]]*=[[:space:]]*(.*)$", text, perl = TRUE)
    parts <- regmatches(text, match)[[1]]
    if (length(parts) < 3) {
      next
    }
    env <- parts[[2]]
    if (length(vars) && !env %in% vars) {
      next
    }
    value <- trimws(parts[[3]])
    if (grepl("`|\\$\\(", value, perl = TRUE)) {
      next
    }
    rows[[length(rows) + 1L]] <- data.frame(
      env = env,
      value = .genflow_unquote_env_value(value),
      source = as.character(source)[1],
      path = path,
      line = idx,
      stringsAsFactors = FALSE
    )
  }
  if (!length(rows)) {
    return(empty)
  }
  do.call(rbind, rows)
}

.genflow_detect_credentials <- function(providers = NULL, vars = NULL, include_values = FALSE) {
  specs <- .genflow_credential_specs(providers)
  spec_vars <- unique(specs$env)
  if (!is.null(vars)) {
    spec_vars <- intersect(spec_vars, unique(trimws(as.character(vars))))
  }
  spec_vars <- spec_vars[nzchar(spec_vars)]
  empty <- data.frame(
    env = character(),
    value = character(),
    masked = character(),
    source = character(),
    path = character(),
    line = integer(),
    stringsAsFactors = FALSE
  )
  if (!length(spec_vars)) {
    return(empty)
  }

  rows <- list()
  for (env in spec_vars) {
    value <- Sys.getenv(env, unset = "")
    if (nzchar(value)) {
      rows[[length(rows) + 1L]] <- data.frame(
        env = env,
        value = value,
        masked = .genflow_mask_secret(value),
        source = "current_session",
        path = "",
        line = NA_integer_,
        stringsAsFactors = FALSE
      )
    }
  }

  paths <- .genflow_env_source_paths()
  for (source_name in names(paths)) {
    detected <- .genflow_read_env_assignments(paths[[source_name]], vars = spec_vars, source = source_name)
    if (nrow(detected)) {
      detected$masked <- vapply(detected$value, .genflow_mask_secret, character(1))
      detected <- detected[, c("env", "value", "masked", "source", "path", "line"), drop = FALSE]
      rows[[length(rows) + 1L]] <- detected
    }
  }

  if (!length(rows)) {
    return(empty)
  }
  result <- do.call(rbind, rows)
  result <- result[nzchar(result$value), , drop = FALSE]
  if (!nrow(result)) {
    return(empty)
  }
  source_rank <- c(
    current_session = 1L,
    user_renviron = 2L,
    project_renviron = 3L,
    project_env = 4L,
    bashrc = 5L,
    zshrc = 6L
  )
  result$.rank <- source_rank[result$source]
  result$.rank[is.na(result$.rank)] <- 99L
  result <- result[order(result$env, result$.rank, result$line), , drop = FALSE]
  result$.rank <- NULL
  rownames(result) <- NULL
  if (!isTRUE(include_values)) {
    result$value <- NA_character_
  }
  result
}

.genflow_credential_status <- function(providers = NULL) {
  specs <- .genflow_credential_specs(providers)
  if (!nrow(specs)) {
    return(data.frame())
  }
  detected <- .genflow_detect_credentials(providers = providers, include_values = TRUE)
  rows <- vector("list", nrow(specs))
  for (idx in seq_len(nrow(specs))) {
    spec <- specs[idx, , drop = FALSE]
    active_value <- Sys.getenv(spec$env, unset = "")
    active <- nzchar(active_value)
    detected_rows <- detected[detected$env == spec$env, , drop = FALSE]
    detected_file_rows <- detected_rows[detected_rows$source != "current_session", , drop = FALSE]
    source <- ""
    masked <- ""
    detected_present <- nrow(detected_file_rows) > 0
    if (active) {
      source <- "current_session"
      masked <- if (isTRUE(spec$sensitive)) .genflow_mask_secret(active_value) else active_value
    } else if (nrow(detected_file_rows)) {
      source <- detected_file_rows$source[[1]]
      masked <- if (isTRUE(spec$sensitive)) detected_file_rows$masked[[1]] else detected_file_rows$value[[1]]
    }
    rows[[idx]] <- cbind(
      spec,
      data.frame(
        active = active,
        detected = detected_present,
        source = source,
        masked = masked,
        stringsAsFactors = FALSE
      )
    )
  }
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result
}

.genflow_required_credentials_missing <- function(providers = NULL) {
  specs <- .genflow_credential_specs(providers)
  specs <- specs[isTRUE(specs$required_for_models) | specs$required_for_models, , drop = FALSE]
  if (!nrow(specs)) {
    return(data.frame(
      provider = character(),
      provider_label = character(),
      required_group = character(),
      envs = character(),
      labels = character(),
      stringsAsFactors = FALSE
    ))
  }
  group_keys <- paste(specs$provider, specs$required_group, sep = "||")
  rows <- list()
  for (key in unique(group_keys)) {
    group_specs <- specs[group_keys == key, , drop = FALSE]
    envs <- unique(group_specs$env)
    active <- any(nzchar(Sys.getenv(envs, unset = "")))
    if (active) {
      next
    }
    rows[[length(rows) + 1L]] <- data.frame(
      provider = group_specs$provider[[1]],
      provider_label = group_specs$provider_label[[1]],
      required_group = group_specs$required_group[[1]],
      envs = paste(envs, collapse = " or "),
      labels = paste(unique(group_specs$label), collapse = " or "),
      stringsAsFactors = FALSE
    )
  }
  if (!length(rows)) {
    return(data.frame(
      provider = character(),
      provider_label = character(),
      required_group = character(),
      envs = character(),
      labels = character(),
      stringsAsFactors = FALSE
    ))
  }
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result
}

.genflow_format_missing_credentials <- function(missing) {
  if (is.null(missing) || !nrow(missing)) {
    return("No required model-update credentials are missing.")
  }
  lines <- paste0(
    missing$provider_label,
    " (",
    missing$provider,
    "): ",
    missing$envs
  )
  paste(lines, collapse = "\n")
}

.genflow_quote_renviron_value <- function(value) {
  value <- as.character(.genflow_cred_or(value, ""))[1]
  if (!nzchar(value)) {
    return("\"\"")
  }
  if (!grepl("[[:space:]#\"'\\\\]", value, perl = TRUE)) {
    return(value)
  }
  encodeString(value, quote = "\"")
}

.genflow_set_private_file_mode <- function(path) {
  if (!file.exists(path)) {
    stop("Cannot secure a file that does not exist.", call. = FALSE)
  }
  secured <- tryCatch(
    Sys.chmod(path, mode = "0600", use_umask = FALSE),
    error = function(e) FALSE
  )
  if (!identical(.Platform$OS.type, "windows") &&
      (!length(secured) || !all(secured))) {
    stop("Failed to set private permissions on a file.", call. = FALSE)
  }
  invisible(isTRUE(all(secured)))
}

.genflow_unique_sidecar_path <- function(path, tag, extension = "") {
  stamp <- format(Sys.time(), "%Y%m%d%H%M%OS6", tz = "UTC")
  stamp <- gsub("[^0-9]", "", stamp)
  prefix <- paste0(path, ".", stamp, "-", Sys.getpid(), "-", tag)
  for (idx in 0:9999) {
    candidate <- paste0(prefix, "-", sprintf("%04d", idx), extension)
    if (!file.exists(candidate) && !dir.exists(candidate)) {
      return(candidate)
    }
  }
  stop("Could not allocate a unique file sidecar path.", call. = FALSE)
}

.genflow_private_copy_file <- function(from, to) {
  if (!file.exists(from)) {
    stop("Cannot copy a credential file that does not exist.", call. = FALSE)
  }
  created <- file.create(to, showWarnings = FALSE)
  if (!isTRUE(created)) {
    stop("Failed to create a private credential copy.", call. = FALSE)
  }
  complete <- FALSE
  on.exit({
    if (!isTRUE(complete) && file.exists(to)) {
      unlink(to, force = TRUE)
    }
  }, add = TRUE)
  .genflow_set_private_file_mode(to)

  input <- NULL
  output <- NULL
  copied <- tryCatch({
    input <- file(from, open = "rb")
    output <- file(to, open = "wb")
    repeat {
      chunk <- readBin(input, what = "raw", n = 65536L)
      if (!length(chunk)) {
        break
      }
      writeBin(chunk, output)
    }
    close(output)
    output <- NULL
    close(input)
    input <- NULL
    TRUE
  }, error = function(e) FALSE)
  if (!is.null(output)) {
    try(close(output), silent = TRUE)
  }
  if (!is.null(input)) {
    try(close(input), silent = TRUE)
  }
  if (!isTRUE(copied)) {
    stop("Failed to create a private credential copy.", call. = FALSE)
  }
  .genflow_set_private_file_mode(to)
  complete <- TRUE
  invisible(to)
}

.genflow_file_lock_path <- function(path) {
  paste0(path, ".genflow.lock")
}

.genflow_lock_number <- function(value, default, minimum = 0, allow_infinite = FALSE) {
  value <- .genflow_cred_or(value, default)
  if (!length(value)) {
    value <- default
  }
  value <- suppressWarnings(as.numeric(value[[1]]))
  valid <- length(value) == 1L && !is.na(value) && value >= minimum
  if (!isTRUE(allow_infinite)) {
    valid <- valid && is.finite(value)
  }
  if (!valid) {
    stop("Invalid file lock timing configuration.", call. = FALSE)
  }
  value
}

.genflow_acquire_file_lock <- function(path,
                                       timeout = 10,
                                       poll = 0.05,
                                       stale_after = 300,
                                       lock_label = "file") {
  if (!is.character(lock_label) || length(lock_label) != 1L ||
      is.na(lock_label) || !nzchar(trimws(lock_label))) {
    stop("File lock label must be one non-empty string.", call. = FALSE)
  }
  timeout <- .genflow_lock_number(
    timeout,
    10,
    minimum = 0
  )
  poll <- .genflow_lock_number(
    poll,
    0.05,
    minimum = 0.001
  )
  stale_after <- .genflow_lock_number(
    stale_after,
    300,
    minimum = 0,
    allow_infinite = TRUE
  )

  lock_path <- .genflow_file_lock_path(path)
  dir.create(dirname(lock_path), recursive = TRUE, showWarnings = FALSE)
  started <- Sys.time()

  repeat {
    acquired <- dir.create(
      lock_path,
      showWarnings = FALSE,
      recursive = FALSE,
      mode = "0700"
    )
    if (isTRUE(acquired)) {
      token <- paste(
        Sys.getpid(),
        format(Sys.time(), "%Y%m%d%H%M%OS6", tz = "UTC"),
        sep = "-"
      )
      owner_path <- file.path(lock_path, "owner")
      owner_ready <- tryCatch({
        if (!isTRUE(file.create(owner_path, showWarnings = FALSE))) {
          stop("owner file")
        }
        .genflow_set_private_file_mode(owner_path)
        writeLines(
          c(
            paste0("token=", token),
            paste0("pid=", Sys.getpid()),
            paste0("created_utc=", format(Sys.time(), "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC"))
          ),
          owner_path,
          useBytes = TRUE
        )
        .genflow_set_private_file_mode(owner_path)
        TRUE
      }, error = function(e) FALSE)
      if (!isTRUE(owner_ready)) {
        unlink(lock_path, recursive = TRUE, force = TRUE)
        stop("Failed to initialize the ", lock_label, " lock.", call. = FALSE)
      }
      return(structure(
        list(path = lock_path, token = token),
        class = "genflow_file_lock"
      ))
    }

    if (dir.exists(lock_path) && is.finite(stale_after)) {
      lock_info <- file.info(lock_path)
      lock_age <- suppressWarnings(as.numeric(
        difftime(Sys.time(), lock_info$mtime[[1]], units = "secs")
      ))
      if (length(lock_age) && !is.na(lock_age) && lock_age >= stale_after) {
        stale_path <- .genflow_unique_sidecar_path(lock_path, "stale")
        moved <- file.rename(lock_path, stale_path)
        if (isTRUE(moved)) {
          unlink(stale_path, recursive = TRUE, force = TRUE)
          next
        }
      }
    }

    elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
    if (!is.finite(elapsed) || elapsed >= timeout) {
      stop("Timed out acquiring the ", lock_label, " lock.", call. = FALSE)
    }
    Sys.sleep(min(poll, max(0, timeout - elapsed)))
  }
}

.genflow_release_file_lock <- function(lock) {
  if (is.null(lock) || !inherits(lock, "genflow_file_lock")) {
    return(invisible(FALSE))
  }
  lock_path <- as.character(.genflow_cred_or(lock$path, ""))[[1]]
  token <- as.character(.genflow_cred_or(lock$token, ""))[[1]]
  if (!nzchar(lock_path) || !dir.exists(lock_path)) {
    return(invisible(FALSE))
  }

  owner_path <- file.path(lock_path, "owner")
  owner <- tryCatch(readLines(owner_path, warn = FALSE), error = function(e) character())
  expected <- paste0("token=", token)
  if (!length(owner) || !identical(owner[[1]], expected)) {
    return(invisible(FALSE))
  }
  invisible(isTRUE(unlink(lock_path, recursive = TRUE, force = TRUE) == 0L))
}

.genflow_acquire_credentials_lock <- function(path,
                                               timeout = NULL,
                                               poll = NULL,
                                               stale_after = NULL) {
  lock <- .genflow_acquire_file_lock(
    path,
    timeout = .genflow_cred_or(
      timeout,
      getOption("genflow.credentials_lock_timeout", 10)
    ),
    poll = .genflow_cred_or(
      poll,
      getOption("genflow.credentials_lock_poll", 0.05)
    ),
    stale_after = .genflow_cred_or(
      stale_after,
      getOption("genflow.credentials_lock_stale_after", 300)
    ),
    lock_label = "credential file"
  )
  class(lock) <- c("genflow_credentials_lock", class(lock))
  lock
}

.genflow_release_credentials_lock <- function(lock) {
  .genflow_release_file_lock(lock)
}

.genflow_atomic_write_lines <- function(lines,
                                        path,
                                        write_fn = writeLines,
                                        rename_fn = file.rename,
                                        portable_replace = identical(.Platform$OS.type, "windows")) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(dirname(path))) {
    stop("Failed to create the credential directory.", call. = FALSE)
  }

  staging <- .genflow_unique_sidecar_path(path, "staging", ".tmp")
  if (!isTRUE(file.create(staging, showWarnings = FALSE))) {
    stop("Failed to create a credential staging file.", call. = FALSE)
  }
  on.exit({
    if (file.exists(staging)) {
      unlink(staging, force = TRUE)
    }
  }, add = TRUE)
  .genflow_set_private_file_mode(staging)

  wrote <- tryCatch({
    write_fn(as.character(lines), staging, useBytes = TRUE)
    TRUE
  }, error = function(e) FALSE)
  if (!isTRUE(wrote)) {
    stop("Failed to write the credential staging file.", call. = FALSE)
  }
  .genflow_set_private_file_mode(staging)

  target_exists <- file.exists(path)
  if (!target_exists || !isTRUE(portable_replace)) {
    replaced <- tryCatch(
      rename_fn(staging, path),
      error = function(e) FALSE
    )
    if (!isTRUE(replaced)) {
      stop("Failed to atomically replace the credential file.", call. = FALSE)
    }
    .genflow_set_private_file_mode(path)
    return(invisible(path))
  }

  rollback <- .genflow_unique_sidecar_path(path, "rollback", ".tmp")
  moved_original <- tryCatch(
    rename_fn(path, rollback),
    error = function(e) FALSE
  )
  if (!isTRUE(moved_original)) {
    stop("Failed to prepare a recoverable credential file replacement.", call. = FALSE)
  }
  .genflow_set_private_file_mode(rollback)

  replaced <- tryCatch(
    rename_fn(staging, path),
    error = function(e) FALSE
  )
  if (!isTRUE(replaced)) {
    restored <- tryCatch(
      rename_fn(rollback, path),
      error = function(e) FALSE
    )
    if (isTRUE(restored)) {
      stop("Failed to replace the credential file; the original was restored.", call. = FALSE)
    }
    stop(
      "Failed to replace the credential file; the original remains in private recovery file ",
      rollback,
      ".",
      call. = FALSE
    )
  }

  .genflow_set_private_file_mode(path)
  if (file.exists(rollback)) {
    unlink(rollback, force = TRUE)
  }
  invisible(path)
}

.genflow_recover_credentials_file <- function(path) {
  if (file.exists(path)) {
    return(invisible(""))
  }
  directory <- dirname(path)
  if (!dir.exists(directory)) {
    return(invisible(""))
  }

  sidecars <- list.files(
    directory,
    all.files = TRUE,
    no.. = TRUE,
    full.names = TRUE
  )
  sidecar_names <- basename(sidecars)
  prefix <- paste0(basename(path), ".")
  rollback <- sidecars[
    startsWith(sidecar_names, prefix) &
      grepl("-rollback-[0-9]{4}\\.tmp$", sidecar_names, perl = TRUE)
  ]
  if (!length(rollback)) {
    return(invisible(""))
  }

  info <- file.info(rollback)
  rollback <- rollback[order(info$mtime, decreasing = TRUE, na.last = TRUE)]
  recovery_path <- rollback[[1]]
  .genflow_set_private_file_mode(recovery_path)
  restored <- file.rename(recovery_path, path)
  if (!isTRUE(restored)) {
    stop("An interrupted credential update could not be restored.", call. = FALSE)
  }
  .genflow_set_private_file_mode(path)
  invisible(recovery_path)
}

.genflow_backup_file <- function(path) {
  if (!file.exists(path)) {
    return("")
  }
  backup <- .genflow_unique_sidecar_path(path, "backup", ".bak")
  .genflow_private_copy_file(path, backup)
  backup
}

.genflow_save_credentials <- function(values,
                                      path = NULL,
                                      backup = TRUE,
                                      set_session = TRUE) {
  if (is.null(values) || !length(values)) {
    stop("No credential values were provided.", call. = FALSE)
  }
  value_names <- .genflow_cred_or(names(values), rep("", length(values)))
  values <- as.character(values)
  names(values) <- trimws(as.character(value_names))
  values <- values[nzchar(names(values))]
  if (!length(values)) {
    stop("No named credential values were provided.", call. = FALSE)
  }
  invalid <- names(values)[!vapply(names(values), .genflow_is_valid_env_name, logical(1))]
  if (length(invalid)) {
    stop("Invalid environment variable name(s): ", paste(invalid, collapse = ", "), call. = FALSE)
  }

  target <- .genflow_credentials_path(path)
  dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
  lock <- .genflow_acquire_credentials_lock(target)
  on.exit(.genflow_release_credentials_lock(lock), add = TRUE)
  .genflow_recover_credentials_file(target)
  lines <- if (file.exists(target)) readLines(target, warn = FALSE) else character()
  backup_path <- if (isTRUE(backup)) .genflow_backup_file(target) else ""
  added <- character()
  updated <- character()

  for (env in names(values)) {
    replacement <- paste0(env, "=", .genflow_quote_renviron_value(values[[env]]))
    pattern <- paste0("^[[:space:]]*(export[[:space:]]+)?", env, "[[:space:]]*=")
    matches <- grep(pattern, lines, perl = TRUE)
    if (length(matches)) {
      lines[matches[[1]]] <- replacement
      if (length(matches) > 1L) {
        lines <- lines[-matches[-1]]
      }
      updated <- c(updated, env)
    } else {
      lines <- c(lines, replacement)
      added <- c(added, env)
    }
  }
  .genflow_atomic_write_lines(lines, target)
  if (isTRUE(set_session)) {
    args <- as.list(values)
    do.call(Sys.setenv, args)
  }
  list(
    path = target,
    backup_path = backup_path,
    added = added,
    updated = updated
  )
}

.genflow_delete_credentials <- function(vars,
                                        path = NULL,
                                        backup = TRUE,
                                        unset_session = TRUE) {
  vars <- unique(trimws(as.character(.genflow_cred_or(vars, character()))))
  vars <- vars[nzchar(vars)]
  if (!length(vars)) {
    stop("No credential variables were provided.", call. = FALSE)
  }
  invalid <- vars[!vapply(vars, .genflow_is_valid_env_name, logical(1))]
  if (length(invalid)) {
    stop("Invalid environment variable name(s): ", paste(invalid, collapse = ", "), call. = FALSE)
  }

  target <- .genflow_credentials_path(path)
  dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
  lock <- .genflow_acquire_credentials_lock(target)
  on.exit(.genflow_release_credentials_lock(lock), add = TRUE)
  .genflow_recover_credentials_file(target)
  lines <- if (file.exists(target)) readLines(target, warn = FALSE) else character()
  backup_path <- if (isTRUE(backup)) .genflow_backup_file(target) else ""
  removed <- character()
  if (length(lines)) {
    keep <- rep(TRUE, length(lines))
    for (env in vars) {
      pattern <- paste0("^[[:space:]]*(export[[:space:]]+)?", env, "[[:space:]]*=")
      matches <- grep(pattern, lines, perl = TRUE)
      if (length(matches)) {
        keep[matches] <- FALSE
        removed <- c(removed, env)
      }
    }
    lines <- lines[keep]
  }
  .genflow_atomic_write_lines(lines, target)
  if (isTRUE(unset_session)) {
    Sys.unsetenv(vars)
  }
  list(
    path = target,
    backup_path = backup_path,
    removed = unique(removed)
  )
}
