# HTTP facts enter the existing diagnostic v1 contract here. Bodies are parsed
# only for recognized machine codes; arbitrary provider messages are never logged.
.gen_provider_body <- function(response, max_bytes=1048576L) {
  body <- if (!inherits(response,"response") && is.list(response)) response$content %||% response$text else NULL
  if (is.null(body)) body <- tryCatch(httr::content(response,"text",type="application/json",encoding="UTF-8"),error=function(e) NULL)
  if (is.character(body) && length(body)==1L && !is.na(body) && nchar(body,type="bytes")<=max_bytes)
    body <- tryCatch(jsonlite::fromJSON(body,simplifyVector=FALSE),error=function(e) NULL)
  if (is.list(body)) body else list()
}

.gen_provider_http_error <- function(status, body=NULL, stage="inference") {
  if (is.character(body) && length(body)==1L && !is.na(body) && nchar(body,type="bytes")<=1048576L)
    body <- tryCatch(jsonlite::fromJSON(body,simplifyVector=FALSE),error=function(e) NULL)
  if (!is.list(body)) body <- list()
  fields <- function(x) if (is.list(x)) x[c("code","type","status")] else list()
  values <- c(fields(body$error),fields(body$detail),fields(body))
  values <- unlist(Filter(function(x) is.character(x) && length(x)==1L && !is.na(x),values),use.names=FALSE)
  # Explicit codes take precedence over broad error types such as insufficient_quota.
  mappings <- c(credit_balance_exhausted="provider_credit_exhausted",insufficient_credits="provider_credit_exhausted",
    insufficient_credit="provider_credit_exhausted",insufficient_quota="provider_quota_exhausted",quota_exceeded="provider_quota_exhausted")
  known <- values[values %in% names(mappings)]
  valid <- is.numeric(status) && length(status)==1L && is.finite(status) && status>=100 && status<=599 && status==as.integer(status)
  code <- if (!valid) "provider_response_invalid" else if (status==401) "provider_unauthorized" else if (status==403) "provider_forbidden" else
    if (length(known)) unname(mappings[[known[[1L]]]]) else switch(as.character(status),
      `402`="provider_payment_required",`404`="provider_not_found",`429`="provider_rate_limit",
      if (status>=500) "provider_unavailable" else if (status==408) "provider_timeout" else "provider_request_rejected")
  uncertain <- !valid || status==408 || status>=500
  outcome <- if (stage=="download") "succeeded" else if (stage=="poll") "unknown" else if (stage=="preflight") "not_started" else
    if (stage=="management") "failed" else if (uncertain) "unknown" else "failed"
  transient <- code %in% c("provider_rate_limit","provider_unavailable","provider_timeout")
  retryable <- transient && outcome!="unknown" && (stage!="inference" || identical(code,"provider_rate_limit"))
  params <- if (valid) list(status_code=as.integer(status)) else list()
  if (length(known)) params$provider_code <- known[[1L]]
  gen_diagnostic(code,stage=stage,outcome=outcome,retryable=retryable,params=params,log=TRUE)
}

.gen_provider_check <- function(response, expected=200:299, stage="inference") {
  status <- tryCatch(httr::status_code(response),error=function(e) NULL)
  if (is.null(status) && is.list(response)) status <- response$status
  if (!is.numeric(status) || length(status)!=1L || is.na(status) || !status %in% expected)
    .gen_stop(.gen_provider_http_error(status,.gen_provider_body(response),stage))
  invisible(response)
}

# Parse independently of Content-Type: error responses often carry a proxy MIME
# type. A malformed successful response never authorizes another submission.
.gen_provider_json <- function(response, stage="inference") {
  body <- .gen_provider_body(response,max_bytes=Inf)
  if (!length(body)) .gen_stop(.gen_provider_http_error(NULL,stage=stage))
  body
}
