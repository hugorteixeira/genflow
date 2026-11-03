# Shared boundary for the public management API. These operations manage data;
# they do not need a Shiny session and never perform model inference.
.gen_management <- function(expr) {
  withCallingHandlers(expr, error = function(error) {
    if (!inherits(error, c("genflow_error", "genflow_validation_error"))) {
      .gen_stop(error, message = gen_error_message("management_failed"),
        stage = "management", outcome = "unknown", log = TRUE)
    }
  })
}

.gen_management_assert <- function(ok, field) {
  if (!isTRUE(ok)) .gen_stop("management_argument", params = list(field = field),
    path = paste0("/", field), stage = "management", outcome = "not_started")
}

.gen_management_string <- function(value, field, empty = FALSE) {
  .gen_management_assert(.gen_scalar(value) && (empty || nzchar(trimws(value))) &&
    !grepl("[\r\n]", value), field)
  value
}

.gen_management_flag <- function(value, field) {
  .gen_management_assert(.gen_scalar(value, "logical"), field)
  value
}

.gen_management_providers <- function(providers) {
  if (is.null(providers)) return(NULL)
  .gen_management_assert(is.character(providers) && !anyNA(providers) &&
    all(nzchar(trimws(providers))), "providers")
  providers <- unique(vapply(providers, .genflow_normalize_service_alias, character(1)))
  known <- c(names(.genflow_provider_registry()), names(.genflow_list_custom_provider_configs()))
  .gen_management_assert(all(providers %in% known), "providers")
  providers
}

.gen_management_path <- function(path, field = "path") {
  if (is.null(path)) return(NULL)
  path.expand(.gen_management_string(path, field))
}
