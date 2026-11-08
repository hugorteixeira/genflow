# Authoring is a validation purpose, never a runtime bypass. Requirements are
# emitted only at the check that knows a value is absent, not by filtering errors.
.gen_authoring <- function(purpose) identical(purpose, "authoring")
.gen_requirement <- function(code, path, node = NULL) .gen_issue(code, path, node_id = node)
