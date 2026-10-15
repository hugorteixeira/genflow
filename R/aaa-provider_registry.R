# Static adapter facts. Model availability is discovered separately; a catalog
# entry does not imply that every feature is accepted by that model.
.GENFLOW_PROVIDER_LABELS <- c(
  openai = "OpenAI", openrouter = "OpenRouter", anthropic = "Anthropic",
  groq = "Groq", cerebras = "Cerebras", together = "Together", sambanova = "SambaNova",
  nebius = "Nebius", deepseek = "DeepSeek", perplexity = "Perplexity",
  fireworks = "Fireworks", deepinfra = "DeepInfra", hyperbolic = "Hyperbolic",
  hf = "Hugging Face", `local-native` = "Native STT (local)",
  `local-openai` = "OpenAI-compatible STT (local)", ollama = "Ollama",
  llamacpp = "llama-cpp", gemini = "Gemini", fal = "FAL", replicate = "Replicate",
  assemblyai = "AssemblyAI", cloudflare = "Cloudflare", voicegain = "Voicegain"
)

.GENFLOW_COMPAT_SPECS <- list(
  groq = list(base_url = "https://api.groq.com", chat_path = "/openai/v1/chat/completions",
              model_paths = "/openai/v1/models", default_model = "llama-3.3-70b-versatile"),
  cerebras = list(base_url = "https://api.cerebras.ai", default_model = "llama-3.3-70b", model_paths = "/v1/models"),
  together = list(base_url = "https://api.together.xyz", default_model = "meta-llama/Meta-Llama-3.1-8B-Instruct-Turbo", model_paths = "/v1/models"),
  sambanova = list(base_url = "https://api.sambanova.ai", default_model = "Meta-Llama-3.1-8B-Instruct",
                  model_paths = "/v1/models", api_key_env = c("SAMBANOVA_API_KEY", "SAMBA_API_KEY"),
                  model_env = c("SAMBANOVA_MODEL", "SAMBA_MODEL")),
  nebius = list(base_url = "https://api.studio.nebius.ai", default_model = "meta-llama/Meta-Llama-3.1-70B-Instruct"),
  deepseek = list(base_url = "https://api.deepseek.com", chat_path = "/chat/completions",
                  model_paths = c("/models", "/v1/models"), default_model = "deepseek-chat"),
  perplexity = list(base_url = "https://api.perplexity.ai", chat_path = "/chat/completions",
                    model_paths = c("/models", "/v1/models"), default_model = "sonar"),
  fireworks = list(base_url = "https://api.fireworks.ai/inference", default_model = "accounts/fireworks/models/llama-v3p1-8b-instruct"),
  deepinfra = list(base_url = "https://api.deepinfra.com/v1/openai", chat_path = "/chat/completions",
                   model_paths = c("/models", "/v1/models"), default_model = "meta-llama/Meta-Llama-3.1-8B-Instruct"),
  hyperbolic = list(base_url = "https://api.hyperbolic.xyz", default_model = "meta-llama/Meta-Llama-3.1-8B-Instruct")
)

.genflow_provider_registry <- function() {
  text <- c("openai", "openrouter", "anthropic", names(.GENFLOW_COMPAT_SPECS), "gemini", "hf", "ollama", "llamacpp")
  image <- c("hf", "fal", "openai", "replicate")
  stt <- c("openai", "groq", "assemblyai", "cloudflare", "voicegain", "hf", "replicate", "local-native", "local-openai")
  catalog <- setdiff(names(.GENFLOW_PROVIDER_LABELS), c("local-openai", "assemblyai", "cloudflare", "voicegain"))
  out <- lapply(names(.GENFLOW_PROVIDER_LABELS), function(id) {
    list(id = id, label = unname(.GENFLOW_PROVIDER_LABELS[[id]]), kind = "builtin",
         modalities = c(text = id %in% text, image = id %in% image, stt = id %in% stt,
                        tts = id %in% c("openai", "replicate"), video = FALSE),
         text_function = if (id %in% text) paste0(".gen_txt_", id) else NULL,
         model_resolver = if (id %in% setdiff(text, c("openai", "openrouter", "hf"))) paste0(".", id, "_resolve_model") else NULL,
         catalog_function = if (id %in% catalog) paste0(".update_models_", gsub("-", "_", id)) else NULL,
         catalog_allow_empty = identical(id, "local-native"))
  })
  stats::setNames(out, names(.GENFLOW_PROVIDER_LABELS))
}

.genflow_compat_spec <- function(id) {
  spec <- .GENFLOW_COMPAT_SPECS[[id]]
  if (is.null(spec)) stop("No compatible transport registered for ", id, call. = FALSE)
  defaults <- list(id = id, label = unname(.GENFLOW_PROVIDER_LABELS[[id]]),
                   api_key_env = paste0(toupper(id), "_API_KEY"),
                   model_env = paste0(toupper(id), "_MODEL"),
                   base_url_env = paste0(toupper(id), "_BASE_URL"),
                   chat_path = "/v1/chat/completions", model_paths = c("/v1/models", "/models"))
  utils::modifyList(defaults, spec)
}

.genflow_first_env <- function(names, default = "") {
  values <- trimws(Sys.getenv(names, unset = ""))
  values <- values[!is.na(values) & nzchar(values)]
  if (length(values)) unname(values[[1]]) else default
}

.genflow_compat_base_url <- function(id) {
  spec <- .genflow_compat_spec(id)
  sub("/+$", "", .genflow_first_env(spec$base_url_env, spec$base_url))
}

.genflow_compat_key <- function(id) .genflow_first_env(.genflow_compat_spec(id)$api_key_env)

.genflow_compat_model <- function(id, model, timeout_secs = 10) {
  spec <- .genflow_compat_spec(id)
  .openai_compat_resolve_model(model, timeout_secs = timeout_secs,
    model_env_vars = spec$model_env, api_key = .genflow_compat_key(id),
    default_model = spec$default_model, base_url = .genflow_compat_base_url(id), model_paths = spec$model_paths)
}

.genflow_compat_text <- function(id, prompt, model, temp_v, reasoning, add_img,
                                 tools = FALSE, my_tools = NULL, plugins = NULL, timeout_secs = 80) {
  spec <- .genflow_compat_spec(id)
  .gen_txt_openai_compatible(spec$label, prompt, .genflow_compat_model(id, model, timeout_secs),
    temp_v, reasoning, add_img, tools, my_tools, plugins, timeout_secs,
    base_url = .genflow_compat_base_url(id), api_key = .genflow_compat_key(id), chat_path = spec$chat_path)
}

.genflow_compat_catalog <- function(id, directory, verbose = TRUE) {
  spec <- .genflow_compat_spec(id)
  .update_models_openai_compat(directory, verbose, provider_id = id, provider_name = spec$label,
    api_key = .genflow_compat_key(id), base_url = .genflow_compat_base_url(id), model_paths = spec$model_paths)
}

.genflow_openai_sampling <- function(model, reasoning = NULL) {
  if (grepl("^o[0-9]|search", model)) return(FALSE)
  if (grepl("^gpt-5[.]([12])($|-)", model)) return(identical(reasoning, "none"))
  if (grepl("^gpt-5", model)) return(FALSE)
  TRUE
}

#' Inspect implemented provider capabilities without performing inference
#'
#' Describes adapter support, not a guarantee that an arbitrary catalog model
#' accepts every feature. Model-specific restrictions are applied when known;
#' otherwise `model_support` is `"unverified"`.
#' @param service Provider identifier or alias.
#' @param model Optional model identifier for known parameter restrictions.
#' @param reasoning Optional reasoning effort, for endpoint-specific support.
#' @return A list with supported modalities, adapter features, sampling support,
#'   and model qualification status. No credentials are returned.
#' @export
gen_capabilities <- function(service, model = NULL, reasoning = NULL) {
  id <- .genflow_normalize_service_alias(service)
  spec <- .genflow_provider_registry()[[id]]
  custom <- if (is.null(spec)) .genflow_get_custom_provider(id) else NULL
  if (is.null(spec) && is.null(custom)) stop("Unknown provider: ", id, call. = FALSE)
  text <- if (is.null(custom)) isTRUE(spec$modalities[["text"]]) else TRUE
  features <- c(vision = text, tools = text && id != "ollama",
                reasoning = id %in% c("openai", "openrouter", "hf"), plugins = id == "openrouter",
                structured_output = FALSE, streaming = FALSE, durable_jobs = FALSE)
  if (!is.null(custom)) {
    features[c("vision", "tools", "reasoning", "plugins")] <- vapply(
      c("supports_vision", "supports_tools", "supports_reasoning", "supports_plugins"),
      function(field) isTRUE(custom[[field]]), logical(1))
  }
  if (id == "openai" && !is.null(reasoning)) features[c("vision", "tools")] <- FALSE
  list(schema_version = 1L, service = id, model = model,
       modalities = if (is.null(custom)) spec$modalities else c(text = TRUE, image = FALSE, stt = FALSE, tts = FALSE, video = FALSE),
       adapter_features = features, model_support = "unverified",
       sampling_temperature = if (id == "openai" && !is.null(model)) .genflow_openai_sampling(model, reasoning) else NA)
}
