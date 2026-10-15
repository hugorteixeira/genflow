.genflow_normalize_agent_fields <- function(config) {
  config <- .genflow_merge_fields(config)
  aliases <- c(Service = "service", Model = "model", Type = "type", Temp = "temp",
               Tools = "tools", timeoutApi = "timeout_api", timeout = "timeout_api", Timeout = "timeout_api")
  for (name in intersect(names(aliases), names(config))) {
    target <- aliases[[name]]
    if (is.null(config[[target]])) config[target] <- config[name]
    config[name] <- NULL
  }
  config
}

.genflow_add_items <- function(value) {
  if (is.null(value)) return(list())
  if (is.data.frame(value) || is.matrix(value)) return(list(value))
  if (is.list(value)) value else as.list(value)
}

.genflow_batch_combine <- function(original, supplied, mode = "replace", text = FALSE) {
  mode <- mode %||% "replace"
  if (is.null(supplied)) return(original)
  if (is.null(original) || identical(mode, "replace")) return(supplied)
  values <- if (identical(mode, "before")) list(supplied, original) else list(original, supplied)
  if (text) return(paste(vapply(values, .genflow_input_text, character(1)), collapse = "\n\n"))
  c(.genflow_add_items(values[[1]]), .genflow_add_items(values[[2]]))
}

.genflow_runtime_context <- function(fingerprint = FALSE) {
  keys <- c("genflow.providers_path", "genflow.providers_dir", "genflow.local_config_path",
            "genflow.local_config_dir", "genflow.models_dir", "genflow.output_dir",
            "genflow.cache_dir", "genflow.log_dir", "genflow.runtime_snapshot")
  values <- lapply(keys, getOption)
  names(values) <- keys
  values <- values[!vapply(values, is.null, logical(1))]
  out <- list(options = values)
  if (fingerprint) {
    # Endpoint/model selection is configuration; API keys are deliberately absent.
    env <- Sys.getenv()
    out$providers <- .genflow_list_custom_provider_configs()
    custom_model_env <- vapply(out$providers, function(cfg) cfg$model_env %||% "", character(1))
    select <- grepl("^(GENFLOW|OPENAI|OPENROUTER|ANTHROPIC|CLAUDE|GROQ|CEREBRAS|TOGETHER|SAMBANOVA|SAMBA|NEBIUS|DEEPSEEK|PERPLEXITY|FIREWORKS|DEEPINFRA|HYPERBOLIC|GEMINI|GOOGLE|OLLAMA|LLAMA_CPP|LLAMACPP)_(BASE_URL|MODEL|API_VERSION)$", names(env))
    out$environment <- as.list(env[select | names(env) %in% custom_model_env])
    out$local <- gen_local_config()
    out$schema_version <- 1L
  }
  out
}

.genflow_snapshot_config <- function() {
  getOption("genflow.runtime_snapshot") %||% list(
    providers = .genflow_list_custom_provider_configs(),
    local = .genflow_read_local_config()
  )
}
