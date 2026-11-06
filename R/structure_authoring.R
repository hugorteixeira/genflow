# Authoring is a validation purpose, never a runtime bypass. Requirements are
# emitted only at the check that knows a value is absent, not by filtering errors.
.gen_authoring <- function(purpose) identical(purpose, "authoring")
.gen_requirement <- function(code, path, node = NULL) .gen_issue(code, path, node_id = node)

.gen_agent_proposal <- function(value, prompts, check_capabilities) {
  errors <- .gen_definition_shape(value, list(agent=list(type="object"), prompt=list(type="object")), "agent")
  if (!.gen_object(value) || !.gen_object(value$agent)) return(list(valid=FALSE, errors=errors))
  checked <- gen_validate_agent_definition(value$agent, check_capabilities, purpose="authoring")
  for (e in checked$errors) { e$path <- paste0("/agent", e$path); errors[[length(errors)+1L]] <- e }
  if (!is.null(value$prompt) && !is.null(value$agent$prompt_version_id))
    errors[[length(errors)+1L]] <- .gen_issue("agent_configuration_conflict", "/prompt")
  if (!is.null(value$prompt)) errors <- c(errors, .gen_definition_shape(value$prompt,
    list(template=list(type="string", pattern="\\S", maxLength=100000)), "template", "/prompt"))
  prompt_id <- value$agent$prompt_version_id
  template <- if (.gen_scalar(prompt_id)) prompts[[prompt_id]] else NULL
  checked$template <- (if (.gen_object(value$prompt)) value$prompt$template else NULL) %||% template %||% "{{input}}"
  checked$errors <- errors; checked$valid <- !length(errors); checked$proposal <- TRUE
  checked
}
