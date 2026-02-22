% Generated manually: custom provider registry
\name{set_provider_openai_compat}
\alias{set_provider_openai_compat}
\title{Register a custom OpenAI-compatible provider}
\usage{
set_provider_openai_compat(
  id,
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
  overwrite = TRUE
)
}
\arguments{
\item{id}{Provider id used in \code{service}. Must match \code{^[a-z][a-z0-9_-]*$}.}

\item{label}{Display label for interfaces/listings.}

\item{base_url}{Optional single base URL.}

\item{base_urls}{Optional vector of base URLs (failover order).}

\item{api_key_env}{Environment variable name that stores the API key.}

\item{model_env}{Optional environment variable name for default model.}

\item{default_model}{Fallback model when model is not explicitly provided.}

\item{chat_paths}{Candidate chat endpoints.}

\item{model_paths}{Candidate model-list endpoints.}

\item{auth_header}{Authentication header name (use \code{""} to disable auth header).}

\item{auth_prefix}{Authentication prefix (for example \code{"Bearer"}).}

\item{extra_headers}{Optional named headers included on all requests.}

\item{api_key_required}{Logical; whether missing API key should error.}

\item{supports_tools}{Logical capability flag.}

\item{supports_vision}{Logical capability flag.}

\item{supports_reasoning}{Logical capability flag.}

\item{supports_plugins}{Logical capability flag.}

\item{reasoning_field}{Optional JSON field name used to send reasoning.}

\item{plugins_field}{Optional JSON field name used to send plugins/extensions.}

\item{max_tokens}{Optional value sent as \code{max_tokens}.}

\item{overwrite}{Logical; overwrite existing custom provider with same \code{id}.}
}
\value{
Invisibly returns the normalized provider configuration list.
}
\description{
Registers a custom OpenAI-compatible provider. The provider becomes available
to \code{gen_txt(service = "<id>")} and model update workflows.
}
\examples{
\dontrun{
set_provider_openai_compat(
  id = "mycloud",
  label = "My Cloud",
  base_urls = c("https://llm.example.com"),
  api_key_env = "MYCLOUD_API_KEY",
  default_model = "my-model"
)
}
}
