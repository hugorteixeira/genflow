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

    .genflow_credential_spec_row("gemini", "Gemini", "GEMINI_API_KEY", "API key", required_for_models = TRUE),
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
  if (length(custom_cfgs)) {
    custom_rows <- list()
    for (id in names(custom_cfgs)) {
      cfg <- custom_cfgs[[id]]
      provider_id <- tolower(trimws(as.character(.genflow_cred_or(cfg$id, id))[1]))
      provider_label <- trimws(as.character(.genflow_cred_or(cfg$label, provider_id))[1])
      api_key_env <- trimws(as.character(.genflow_cred_or(cfg$api_key_env, ""))[1])
      if (nzchar(api_key_env)) {
        custom_rows[[length(custom_rows) + 1L]] <- .genflow_credential_spec_row(
          provider_id,
          provider_label,
          api_key_env,
          "API key",
          required_for_models = isTRUE(cfg$api_key_required),
          required_group = paste0(provider_id, "_api_key")
        )
      }
    }
    if (length(custom_rows)) {
      specs <- rbind(specs, do.call(rbind, custom_rows))
    }
  }

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

.genflow_backup_file <- function(path) {
  if (!file.exists(path)) {
    return("")
  }
  backup <- paste0(path, ".", format(Sys.time(), "%Y%m%d%H%M%S"), ".bak")
  ok <- file.copy(path, backup, overwrite = FALSE)
  if (!isTRUE(ok)) {
    stop("Failed to back up ", path, call. = FALSE)
  }
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
  writeLines(lines, target, useBytes = TRUE)
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
  dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, target, useBytes = TRUE)
  if (isTRUE(unset_session)) {
    Sys.unsetenv(vars)
  }
  list(
    path = target,
    backup_path = backup_path,
    removed = unique(removed)
  )
}
