# Internal utilities ---------------------------------------------------------

.genflow_cache_dir <- function() {
  cache_dir <- getOption("genflow.cache_dir")
  if (is.null(cache_dir) || !nzchar(cache_dir)) {
    cache_dir <- tools::R_user_dir("genflow", which = "cache")
  }
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }
  cache_dir
}

.genflow_cache_subdir <- function(type) {
  dir_path <- file.path(.genflow_cache_dir(), type)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  }
  dir_path
}

.genflow_setup_path <- function(sname) {
  sanitized <- .sanitize_filename(sname)
  file.path(.genflow_cache_subdir("setups"), paste0(sanitized, ".rds"))
}

.genflow_agent_path <- function(name) {
  sanitized <- .sanitize_filename(name)
  file.path(.genflow_cache_subdir("agents"), paste0(sanitized, ".rds"))
}

.genflow_content_path <- function(cname) {
  sanitized <- .sanitize_filename(cname)
  file.path(.genflow_cache_subdir("content"), paste0(sanitized, ".rds"))
}

.genflow_validate_name <- function(name, what) {
  if (missing(name) || is.null(name)) {
    stop(sprintf("You must provide a name for the %s.", what), call. = FALSE)
  }
  if (!is.character(name) || length(name) != 1 || !nzchar(name)) {
    stop(sprintf("The %s name must be a non-empty character string.", what), call. = FALSE)
  }
  name
}

.genflow_drop_null <- function(x) {
  if (!length(x)) {
    return(x)
  }
  x[!vapply(x, is.null, logical(1))]
}

.genflow_load_object <- function(path, what) {
  if (!file.exists(path)) {
    name <- sub("\\.rds$", "", basename(path))
    stop(sprintf("No cached %s named '%s' was found.", what, name), call. = FALSE)
  }
  readRDS(path)
}

.genflow_collapse_value <- function(x, max_chars = 40) {
  if (is.null(x) || length(x) == 0) {
    return("")
  }
  if (is.list(x) && !is.data.frame(x)) {
    x <- unlist(x, recursive = TRUE, use.names = FALSE)
  }
  values <- as.character(x)
  if (length(values) > 3) {
    values <- c(values[1:3], "…")
  }
  txt <- paste(values, collapse = ",")
  txt <- gsub("\n", " ", txt)
  if (nchar(txt) > max_chars) {
    paste0(substr(txt, 1, max_chars), "…")
  } else {
    txt
  }
}

# Setup helpers ------------------------------------------------------------

#' Create or update a reusable setup
#'
#' @param sname Setup identifier.
#' @param service Provider name.
#' @param model Model identifier.
#' @param temp Optional numeric temperature.
#' @param type Optional interaction type.
#' @param ... Extra named fields to store with the setup.
#' @param save Persist the setup when TRUE.
#' @param assign Assign the setup to `envir` when TRUE.
#' @param envir Environment used for assignment.
#' @param overwrite Overwrite existing setup when TRUE.
#'
#' @return List containing setup fields (all lower case).
#' @export
set_setup <- function(sname,
                      service,
                      model,
                      temp = NULL,
                      type = NULL,
                      ...,
                      save = TRUE,
                      assign = TRUE,
                      envir = .GlobalEnv,
                      overwrite = TRUE) {
  sname <- .genflow_validate_name(sname, "setup")
  if (missing(service) || is.null(service)) {
    stop("`service` must be provided.", call. = FALSE)
  }
  if (missing(model) || is.null(model)) {
    stop("`model` must be provided.", call. = FALSE)
  }

  setup <- .genflow_drop_null(c(
    list(
      sname = sname,
      service = service,
      model = model,
      temp = temp,
      type = type
    ),
    list(...)
  ))

  if (isTRUE(save)) {
    path <- .genflow_setup_path(sname)
    if (!overwrite && file.exists(path)) {
      stop(sprintf("A setup named '%s' already exists. Set overwrite = TRUE to replace it.", sname), call. = FALSE)
    }
    saveRDS(setup, path)
  }

  if (isTRUE(assign)) {
    assign(sname, setup, envir = envir)
  }

  setup
}

#' Retrieve a saved setup
#'
#' @param sname Setup identifier.
#' @param assign Assign to environment when TRUE.
#' @param envir Target environment for assignment.
#'
#' @return Setup list.
#' @export
get_setup <- function(sname, assign = FALSE, envir = .GlobalEnv) {
  sname <- .genflow_validate_name(sname, "setup")
  setup <- .genflow_load_object(.genflow_setup_path(sname), "setup")
  setup$sname <- setup$sname %||% sname
  if (isTRUE(assign)) {
    assign(setup$sname, setup, envir = envir)
  }
  setup
}

#' Rename a cached setup
#' @param from Current setup name.
#' @param to New setup name.
#' @return Invisible TRUE when renamed.
#' @export
mv_setup <- function(from, to) {
  from <- .genflow_validate_name(from, "setup")
  to <- .genflow_validate_name(to, "setup")
  old_path <- .genflow_setup_path(from)
  new_path <- .genflow_setup_path(to)
  if (!file.exists(old_path)) {
    stop(sprintf("No cached setup named '%s' was found.", from), call. = FALSE)
  }
  if (file.exists(new_path)) {
    stop(sprintf("A setup named '%s' already exists.", to), call. = FALSE)
  }
  setup <- readRDS(old_path)
  setup$sname <- to
  saveRDS(setup, new_path)
  file.remove(old_path)
  invisible(TRUE)
}

#' Delete a cached setup
#' @param sname Setup identifier.
#' @return Invisible TRUE when removed.
#' @export
rm_setup <- function(sname) {
  sname <- .genflow_validate_name(sname, "setup")
  path <- .genflow_setup_path(sname)
  if (!file.exists(path)) {
    warning(sprintf("No cached setup named '%s' was found.", sname), call. = FALSE)
    return(invisible(FALSE))
  }
  file.remove(path)
  invisible(TRUE)
}

#' List cached setups
#'
#' @return Character vector with one line per setup.
#' @export
list_setups <- function() {
  dir_path <- .genflow_cache_subdir("setups")
  files <- list.files(dir_path, pattern = "\\.rds$", full.names = TRUE)
  if (length(files) == 0) {
    return(character())
  }
  vapply(files, function(path) {
    setup <- readRDS(path)
    sname <- setup$sname %||% sub("\\.rds$", "", basename(path))
    summary <- .genflow_drop_null(list(
      if (!is.null(setup$service)) sprintf("service=%s", setup$service) else NULL,
      if (!is.null(setup$model)) sprintf("model=%s", setup$model) else NULL,
      if (!is.null(setup$type)) sprintf("type=%s", setup$type) else NULL,
      if (!is.null(setup$temp)) sprintf("temp=%s", setup$temp) else NULL
    ))
    extras <- setdiff(names(setup), c("sname", "service", "model", "temp", "type"))
    if (length(extras) > 0) {
      extras_text <- paste(sprintf("%s=%s", extras, vapply(setup[extras], .genflow_collapse_value, character(1))), collapse = "; ")
      summary <- c(summary, sprintf("extra={%s}", extras_text))
    }
    paste(sname, "-", paste(summary, collapse = " | "))
  }, character(1))
}

# Content helpers ----------------------------------------------------------

#' Create or update reusable content
#'
#' @param cname Content identifier.
#' @param ... Arbitrary named fields (context, add, etc.).
#' @param save Persist content when TRUE.
#' @param assign Assign content to `envir` when TRUE.
#' @param envir Environment used for assignment.
#' @param overwrite Overwrite existing content when TRUE.
#'
#' @return Content list (without metadata).
#' @export
set_content <- function(cname,
                        ...,
                        save = TRUE,
                        assign = TRUE,
                        envir = .GlobalEnv,
                        overwrite = TRUE) {
  cname <- .genflow_validate_name(cname, "content")
  content <- list(...)

  if (isTRUE(save)) {
    path <- .genflow_content_path(cname)
    if (!overwrite && file.exists(path)) {
      stop(sprintf("Content named '%s' already exists. Set overwrite = TRUE to replace it.", cname), call. = FALSE)
    }
    saveRDS(list(cname = cname, data = content), path)
  }

  if (isTRUE(assign)) {
    assign(cname, content, envir = envir)
  }

  content
}

#' Retrieve saved content
#'
#' @param cname Content identifier.
#' @param assign Assign to environment when TRUE.
#' @param envir Environment used for assignment.
#'
#' @return Content list.
#' @export
get_content <- function(cname, assign = FALSE, envir = .GlobalEnv) {
  cname <- .genflow_validate_name(cname, "content")
  payload <- .genflow_load_object(.genflow_content_path(cname), "content")
  if (isTRUE(assign)) {
    assign(payload$cname, payload$data, envir = envir)
  }
  payload$data
}

#' Rename cached content
#' @param from Current content name.
#' @param to New content name.
#' @return Invisible TRUE when renamed.
#' @export
mv_content <- function(from, to) {
  from <- .genflow_validate_name(from, "content")
  to <- .genflow_validate_name(to, "content")
  old_path <- .genflow_content_path(from)
  new_path <- .genflow_content_path(to)
  if (!file.exists(old_path)) {
    stop(sprintf("No cached content named '%s' was found.", from), call. = FALSE)
  }
  if (file.exists(new_path)) {
    stop(sprintf("Content named '%s' already exists.", to), call. = FALSE)
  }
  payload <- readRDS(old_path)
  payload$cname <- to
  saveRDS(payload, new_path)
  file.remove(old_path)
  invisible(TRUE)
}

#' Delete cached content
#' @param cname Content identifier.
#' @return Invisible TRUE when removed.
#' @export
rm_content <- function(cname) {
  cname <- .genflow_validate_name(cname, "content")
  path <- .genflow_content_path(cname)
  if (!file.exists(path)) {
    warning(sprintf("No cached content named '%s' was found.", cname), call. = FALSE)
    return(invisible(FALSE))
  }
  file.remove(path)
  invisible(TRUE)
}

#' List cached content
#'
#' @return Character vector with one line per content entry.
#' @export
list_content <- function() {
  dir_path <- .genflow_cache_subdir("content")
  files <- list.files(dir_path, pattern = "\\.rds$", full.names = TRUE)
  if (length(files) == 0) {
    return(character())
  }
  vapply(files, function(path) {
    payload <- readRDS(path)
    cname <- payload$cname %||% sub("\\.rds$", "", basename(path))
    if (length(payload$data) == 0) {
      summary <- "empty"
    } else {
      keys <- names(payload$data)
      if (is.null(keys)) {
        keys <- paste0("item", seq_along(payload$data))
      }
      summary <- paste(sprintf("%s=%s", keys, vapply(payload$data, .genflow_collapse_value, character(1))), collapse = " | ")
    }
    paste(cname, "-", summary)
  }, character(1))
}

# Agent helpers ------------------------------------------------------------

.genflow_coerce_setup <- function(setup) {
  if (is.character(setup) && length(setup) == 1) {
    data <- get_setup(setup, assign = FALSE)
    return(list(data = data, sname = data$sname %||% setup))
  }
  if (is.list(setup)) {
    sname <- setup$sname %||% attr(setup, "setup_id") %||% NA_character_
    return(list(data = setup, sname = sname))
  }
  stop("`setup` must be a list or a single setup name.", call. = FALSE)
}

.genflow_coerce_content <- function(content) {
  if (is.null(content)) {
    return(list(data = NULL, cname = NA_character_))
  }
  if (is.character(content)) {
    if (length(content) != 1) {
      stop("Provide only one content name or a content list.", call. = FALSE)
    }
    data <- get_content(content, assign = FALSE)
    return(list(data = data, cname = content))
  }
  if (is.list(content)) {
    cname <- content$cname %||% NA_character_
    if (!is.null(content$cname)) {
      content$cname <- NULL
    }
    return(list(data = content, cname = cname))
  }
  stop("`content` must be NULL, a list, or a single content name.", call. = FALSE)
}

#' Create or update an agent definition
#'
#' @param name Agent identifier.
#' @param setup Setup list or setup name (`sname`).
#' @param content Optional content list or content name (`cname`).
#' @param ... Additional manual fields to include in the agent.
#' @param save Persist agent when TRUE.
#' @param assign Assign agent to `envir` when TRUE.
#' @param envir Environment used for assignment.
#' @param overwrite Overwrite existing agent when TRUE.
#'
#' @return Agent list flattened for direct use with other genflow helpers.
#' @export
set_agent <- function(name,
                      setup,
                      content = NULL,
                      ...,
                      save = TRUE,
                      assign = TRUE,
                      envir = .GlobalEnv,
                      overwrite = TRUE) {
  name <- .genflow_validate_name(name, "agent")
  setup_info <- .genflow_coerce_setup(setup)
  content_info <- .genflow_coerce_content(content)
  extra_fields <- list(...)

  setup_fields <- setup_info$data
  sname <- setup_info$sname
  if (length(setup_fields) == 0L) {
    setup_fields <- list()
  }
  if (!is.null(names(setup_fields))) {
    setup_fields <- setup_fields[setdiff(names(setup_fields), c("sname", "name"))]
  }
  setup_fields <- .genflow_drop_null(setup_fields)

  content_fields <- content_info$data
  cname <- content_info$cname
  if (is.null(content_fields) || length(content_fields) == 0L) {
    content_fields <- list()
  } else {
    if (is.null(names(content_fields))) {
      names(content_fields) <- paste0("content", seq_along(content_fields))
    }
    content_fields <- content_fields[setdiff(names(content_fields), "cname")]
    content_fields <- .genflow_drop_null(content_fields)
  }

  extra_fields <- .genflow_drop_null(extra_fields)

  agent <- .genflow_drop_null(c(
    list(
      name = name,
      sname = if (!is.null(sname) && !is.na(sname)) sname else NULL,
      cname = if (!is.null(cname) && !is.na(cname)) cname else NULL
    ),
    setup_fields,
    content_fields,
    extra_fields
  ))

  class(agent) <- unique(c("genflow_agent", class(agent)))

  if (isTRUE(save)) {
    path <- .genflow_agent_path(name)
    if (!overwrite && file.exists(path)) {
      stop(sprintf("An agent named '%s' already exists. Set overwrite = TRUE to replace it.", name), call. = FALSE)
    }
    saveRDS(agent, path)
  }

  if (isTRUE(assign)) {
    assign(name, agent, envir = envir)
  }

  agent
}

#' Retrieve a saved agent
#'
#' @param name Agent identifier.
#' @param assign Assign to environment when TRUE.
#' @param envir Environment used for assignment.
#'
#' @return Agent list.
#' @export
get_agent <- function(name, assign = FALSE, envir = .GlobalEnv) {
  name <- .genflow_validate_name(name, "agent")
  agent <- .genflow_load_object(.genflow_agent_path(name), "agent")
  agent$name <- agent$name %||% name
  if (!inherits(agent, "genflow_agent")) {
    class(agent) <- unique(c("genflow_agent", class(agent)))
  }
  if (isTRUE(assign)) {
    assign(agent$name, agent, envir = envir)
  }
  agent
}

#' Rename a cached agent
#' @param from Current agent name.
#' @param to New agent name.
#' @return Invisible TRUE when renamed.
#' @export
mv_agent <- function(from, to) {
  from <- .genflow_validate_name(from, "agent")
  to <- .genflow_validate_name(to, "agent")
  old_path <- .genflow_agent_path(from)
  new_path <- .genflow_agent_path(to)
  if (!file.exists(old_path)) {
    stop(sprintf("No cached agent named '%s' was found.", from), call. = FALSE)
  }
  if (file.exists(new_path)) {
    stop(sprintf("An agent named '%s' already exists.", to), call. = FALSE)
  }
  agent <- readRDS(old_path)
  agent$name <- to
  saveRDS(agent, new_path)
  file.remove(old_path)
  invisible(TRUE)
}

#' Delete a cached agent
#' @param name Agent identifier.
#' @return Invisible TRUE when removed.
#' @export
rm_agent <- function(name) {
  name <- .genflow_validate_name(name, "agent")
  path <- .genflow_agent_path(name)
  if (!file.exists(path)) {
    warning(sprintf("No cached agent named '%s' was found.", name), call. = FALSE)
    return(invisible(FALSE))
  }
  file.remove(path)
  invisible(TRUE)
}

#' List cached agents
#'
#' @return Character vector with one line per agent.
#' @export
list_agents <- function() {
  dir_path <- .genflow_cache_subdir("agents")
  files <- list.files(dir_path, pattern = "\\.rds$", full.names = TRUE)
  if (length(files) == 0) {
    return(character())
  }
  vapply(files, function(path) {
    agent <- readRDS(path)
    name <- agent$name %||% sub("\\.rds$", "", basename(path))
    summary <- .genflow_drop_null(list(
      if (!is.null(agent$service)) sprintf("service=%s", agent$service) else NULL,
      if (!is.null(agent$model)) sprintf("model=%s", agent$model) else NULL,
      if (!is.null(agent$type)) sprintf("type=%s", agent$type) else NULL,
      if (!is.null(agent$temp)) sprintf("temp=%s", agent$temp) else NULL,
      if (!is.null(agent$sname)) sprintf("sname=%s", agent$sname) else NULL,
      if (!is.null(agent$cname)) sprintf("cname=%s", agent$cname) else NULL
    ))
    paste(name, "-", paste(summary, collapse = " | "))
  }, character(1))
}


# Summary -----------------------------------------------------------------

#' Summarise cached setups, agents, and content
#'
#' @return Named list containing character vectors for each category.
#' @export
gen_list <- function() {
  list(
    setups = list_setups(),
    agents = list_agents(),
    content = list_content()
  )
}
