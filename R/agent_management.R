# Internal utilities ---------------------------------------------------------

.genflow_cache_dir <- function() {
  cache_dir <- getOption("genflow.cache_dir")
  if (is.null(cache_dir)) {
    cache_dir <- tools::R_user_dir("genflow", which = "cache")
  } else if (!is.character(cache_dir) ||
      length(cache_dir) != 1L ||
      is.na(cache_dir) ||
      !nzchar(trimws(cache_dir))) {
    stop(
      "`options(\"genflow.cache_dir\")` must be one non-empty path.",
      call. = FALSE
    )
  }
  cache_dir <- path.expand(cache_dir)
  if (!dir.exists(cache_dir)) {
    created <- dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    if (!isTRUE(created) && !dir.exists(cache_dir)) {
      stop("Could not create genflow cache directory: ", cache_dir, call. = FALSE)
    }
  }
  cache_dir
}

.genflow_cache_subdir <- function(type) {
  dir_path <- file.path(.genflow_cache_dir(), type)
  if (!dir.exists(dir_path)) {
    created <- dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
    if (!isTRUE(created) && !dir.exists(dir_path)) {
      stop("Could not create genflow cache subdirectory: ", dir_path, call. = FALSE)
    }
  }
  dir_path
}

.genflow_cache_filename <- function(name) {
  name <- enc2utf8(as.character(name)[1])
  prefix <- tolower(gsub("[^A-Za-z0-9._-]+", "_", name, perl = TRUE))
  prefix <- gsub("^_+|_+$", "", prefix)
  if (!nzchar(prefix)) {
    prefix <- "item"
  }
  prefix <- substr(prefix, 1L, 60L)
  digest <- .genflow_raw_md5(charToRaw(name))
  if (is.null(digest) || !nzchar(digest)) {
    stop("Could not derive a stable cache key.", call. = FALSE)
  }
  paste0(prefix, "--", digest, ".rds")
}

.genflow_legacy_entity_path <- function(type, name) {
  file.path(
    .genflow_cache_subdir(type),
    paste0(.sanitize_filename(name), ".rds")
  )
}

.genflow_entity_stored_name <- function(path, type) {
  value <- tryCatch(readRDS(path), error = function(e) NULL)
  if (is.null(value)) {
    return(NULL)
  }
  field <- switch(
    type,
    setups = "sname",
    agents = "name",
    content = "cname",
    stop("Unsupported cache entity type.", call. = FALSE)
  )
  stored <- value[[field]]
  if (is.null(stored) || length(stored) != 1L || is.na(stored)) {
    return(NULL)
  }
  as.character(stored)
}

.genflow_entity_path <- function(type, name, existing = FALSE) {
  current <- file.path(
    .genflow_cache_subdir(type),
    .genflow_cache_filename(name)
  )
  if (!isTRUE(existing) || file.exists(current)) {
    return(current)
  }
  legacy <- .genflow_legacy_entity_path(type, name)
  if (
    file.exists(legacy) &&
    identical(.genflow_entity_stored_name(legacy, type), as.character(name)[1])
  ) {
    return(legacy)
  }
  current
}

.genflow_setup_path <- function(sname, existing = FALSE) {
  .genflow_entity_path("setups", sname, existing)
}

.genflow_agent_path <- function(name, existing = FALSE) {
  .genflow_entity_path("agents", name, existing)
}

.genflow_content_path <- function(cname, existing = FALSE) {
  .genflow_entity_path("content", cname, existing)
}

.genflow_save_entity <- function(object, type, name, overwrite) {
  existing_path <- .genflow_entity_path(type, name, existing = TRUE)
  target_path <- .genflow_entity_path(type, name, existing = FALSE)
  if (!isTRUE(overwrite) && file.exists(existing_path)) {
    return(list(saved = FALSE, path = existing_path))
  }
  .genflow_atomic_save_rds(object, target_path)
  if (
    file.exists(existing_path) &&
    !identical(
      normalizePath(existing_path, winslash = "/", mustWork = FALSE),
      normalizePath(target_path, winslash = "/", mustWork = FALSE)
    )
  ) {
    if (!file.remove(existing_path)) {
      warning(
        "Saved the migrated cache entry but could not remove legacy file ",
        existing_path,
        ".",
        call. = FALSE
      )
    }
  }
  list(saved = TRUE, path = target_path)
}

.genflow_validate_name <- function(name, what) {
  if (missing(name) || is.null(name)) {
    stop(sprintf("You must provide a name for the %s.", what), call. = FALSE)
  }
  if (!is.character(name) ||
      length(name) != 1L ||
      is.na(name) ||
      !nzchar(trimws(name))) {
    stop(sprintf("The %s name must be a non-empty character string.", what), call. = FALSE)
  }
  trimws(name)
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
  value <- tryCatch(
    readRDS(path),
    error = function(e) {
      stop(
        "Could not read cached ", what, " from ", path, ": ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
  if (!is.list(value)) {
    stop("Cached ", what, " has an invalid schema: ", path, call. = FALSE)
  }
  value
}

.genflow_agent_references <- function(field, value) {
  if (!field %in% c("sname", "cname")) {
    stop("Unsupported agent reference field: ", field, call. = FALSE)
  }
  dir_path <- .genflow_cache_subdir("agents")
  paths <- list.files(dir_path, pattern = "\\.rds$", full.names = TRUE)
  if (!length(paths)) {
    return(list())
  }

  references <- list()
  for (path in paths) {
    agent <- tryCatch(
      readRDS(path),
      error = function(e) {
        stop(
          "Could not inspect saved agent ", basename(path), ": ",
          conditionMessage(e),
          call. = FALSE
        )
      }
    )
    reference <- agent[[field]]
    if (
      !is.null(reference) &&
      length(reference) == 1L &&
      !is.na(reference) &&
      identical(as.character(reference), as.character(value))
    ) {
      name <- as.character(agent$name %||%
        sub("\\.rds$", "", basename(path)))[1]
      references[[name]] <- list(path = path, agent = agent)
    }
  }
  references
}

.genflow_retarget_agent_references <- function(field, from, to = NULL) {
  references <- .genflow_agent_references(field, from)
  if (!length(references)) {
    return(character())
  }

  updated <- character()
  tryCatch(
    {
      for (name in names(references)) {
        agent <- references[[name]]$agent
        if (is.null(to)) {
          agent[[field]] <- NULL
        } else {
          agent[[field]] <- as.character(to)[1]
        }
        .genflow_atomic_save_rds(agent, references[[name]]$path)
        updated <- c(updated, name)
      }
    },
    error = function(e) {
      for (name in updated) {
        try(
          .genflow_atomic_save_rds(
            references[[name]]$agent,
            references[[name]]$path
          ),
          silent = TRUE
        )
      }
      stop(
        "Could not update dependent agent references: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
  updated
}

.genflow_reference_error <- function(resource, name, references) {
  agents <- names(references)
  paste0(
    "Cannot delete ", resource, " '", name, "' because it is referenced by ",
    length(agents), " saved agent", if (length(agents) == 1L) "" else "s",
    ": ", paste(agents, collapse = ", "),
    ". Update those agents first or use `force = TRUE`."
  )
}

.genflow_collapse_value <- function(x, max_chars = 40) {
  if (is.null(x) || length(x) == 0) {
    return("")
  }
  if (is.list(x) && !is.data.frame(x)) {
    x <- unlist(x, recursive = TRUE, use.names = FALSE)
  }
  values <- as.character(x)
  if (length(values)) {
    normalize_entry <- function(entry) {
      entry <- trimws(entry)
      if (!nzchar(entry)) {
        return(entry)
      }
      if (!grepl("\n", entry, fixed = TRUE) && (
        grepl("[/\\\\]", entry) ||
          startsWith(entry, "~") ||
          grepl("^[A-Za-z]:", entry)
      )) {
        base <- basename(entry)
        if (nzchar(base)) {
          return(base)
        }
      }
      entry
    }
    values <- vapply(values, normalize_entry, character(1))
  }
  if (length(values) > 3) {
    values <- c(values[1:3], "...")
  }
  txt <- paste(values, collapse = ",")
  txt <- gsub("\n", " ", txt)
  if (nchar(txt) > max_chars) {
    paste0(substr(txt, 1, max_chars), "...")
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
    saved <- .genflow_save_entity(setup, "setups", sname, overwrite)
    if (!isTRUE(saved$saved)) {
      stop(sprintf("A setup named '%s' already exists. Set overwrite = TRUE to replace it.", sname), call. = FALSE)
    }
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
  setup <- .genflow_load_object(
    .genflow_setup_path(sname, existing = TRUE),
    "setup"
  )
  setup$sname <- setup$sname %||% sname
  if (isTRUE(assign)) {
    assign(setup$sname, setup, envir = envir)
  }
  setup
}

#' Rename a cached setup
#' @param from Current setup name.
#' @param to New setup name.
#' @param update_agents Update saved agents that reference the setup.
#' @return Invisible TRUE when renamed.
#' @export
mv_setup <- function(from, to, update_agents = TRUE) {
  from <- .genflow_validate_name(from, "setup")
  to <- .genflow_validate_name(to, "setup")
  old_path <- .genflow_setup_path(from, existing = TRUE)
  new_path <- .genflow_setup_path(to)
  if (!file.exists(old_path)) {
    stop(sprintf("No cached setup named '%s' was found.", from), call. = FALSE)
  }
  if (file.exists(.genflow_setup_path(to, existing = TRUE))) {
    stop(sprintf("A setup named '%s' already exists.", to), call. = FALSE)
  }
  setup <- readRDS(old_path)
  setup$sname <- to
  .genflow_atomic_save_rds(setup, new_path)
  if (isTRUE(update_agents)) {
    tryCatch(
      .genflow_retarget_agent_references("sname", from, to),
      error = function(e) {
        unlink(new_path)
        stop(conditionMessage(e), call. = FALSE)
      }
    )
  }
  if (!file.remove(old_path)) {
    stop("Could not remove the old setup file after renaming.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Delete a cached setup
#' @param sname Setup identifier.
#' @param force Delete even when saved agents reference the setup. Referencing
#'   agents keep their flattened runtime fields, but the stale `sname` metadata
#'   is removed.
#' @return Invisible TRUE when removed.
#' @export
rm_setup <- function(sname, force = FALSE) {
  sname <- .genflow_validate_name(sname, "setup")
  path <- .genflow_setup_path(sname, existing = TRUE)
  if (!file.exists(path)) {
    warning(sprintf("No cached setup named '%s' was found.", sname), call. = FALSE)
    return(invisible(FALSE))
  }
  references <- .genflow_agent_references("sname", sname)
  if (length(references) && !isTRUE(force)) {
    stop(.genflow_reference_error("setup", sname, references), call. = FALSE)
  }
  if (length(references)) {
    .genflow_retarget_agent_references("sname", sname, to = NULL)
  }
  if (!file.remove(path)) {
    stop("Could not delete setup '", sname, "'.", call. = FALSE)
  }
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
    setup <- .genflow_load_object(path, "setup")
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
    saved <- .genflow_save_entity(
      list(cname = cname, data = content),
      "content",
      cname,
      overwrite
    )
    if (!isTRUE(saved$saved)) {
      stop(sprintf("Content named '%s' already exists. Set overwrite = TRUE to replace it.", cname), call. = FALSE)
    }
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
  payload <- .genflow_load_object(
    .genflow_content_path(cname, existing = TRUE),
    "content"
  )
  if (isTRUE(assign)) {
    assign(payload$cname, payload$data, envir = envir)
  }
  payload$data
}

#' Rename cached content
#' @param from Current content name.
#' @param to New content name.
#' @param update_agents Update saved agents that reference the content.
#' @return Invisible TRUE when renamed.
#' @export
mv_content <- function(from, to, update_agents = TRUE) {
  from <- .genflow_validate_name(from, "content")
  to <- .genflow_validate_name(to, "content")
  old_path <- .genflow_content_path(from, existing = TRUE)
  new_path <- .genflow_content_path(to)
  if (!file.exists(old_path)) {
    stop(sprintf("No cached content named '%s' was found.", from), call. = FALSE)
  }
  if (file.exists(.genflow_content_path(to, existing = TRUE))) {
    stop(sprintf("Content named '%s' already exists.", to), call. = FALSE)
  }
  payload <- readRDS(old_path)
  payload$cname <- to
  .genflow_atomic_save_rds(payload, new_path)
  if (isTRUE(update_agents)) {
    tryCatch(
      .genflow_retarget_agent_references("cname", from, to),
      error = function(e) {
        unlink(new_path)
        stop(conditionMessage(e), call. = FALSE)
      }
    )
  }
  if (!file.remove(old_path)) {
    stop("Could not remove the old content file after renaming.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Delete cached content
#' @param cname Content identifier.
#' @param force Delete even when saved agents reference the content. Referencing
#'   agents keep their flattened runtime fields, but the stale `cname` metadata
#'   is removed.
#' @return Invisible TRUE when removed.
#' @export
rm_content <- function(cname, force = FALSE) {
  cname <- .genflow_validate_name(cname, "content")
  path <- .genflow_content_path(cname, existing = TRUE)
  if (!file.exists(path)) {
    warning(sprintf("No cached content named '%s' was found.", cname), call. = FALSE)
    return(invisible(FALSE))
  }
  references <- .genflow_agent_references("cname", cname)
  if (length(references) && !isTRUE(force)) {
    stop(.genflow_reference_error("content", cname, references), call. = FALSE)
  }
  if (length(references)) {
    .genflow_retarget_agent_references("cname", cname, to = NULL)
  }
  if (!file.remove(path)) {
    stop("Could not delete content '", cname, "'.", call. = FALSE)
  }
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
    payload <- .genflow_load_object(path, "content")
    if (is.null(payload$data) || !is.list(payload$data)) {
      stop("Cached content has an invalid schema: ", path, call. = FALSE)
    }
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
    saved <- .genflow_save_entity(agent, "agents", name, overwrite)
    if (!isTRUE(saved$saved)) {
      stop(sprintf("An agent named '%s' already exists. Set overwrite = TRUE to replace it.", name), call. = FALSE)
    }
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
  agent <- .genflow_load_object(
    .genflow_agent_path(name, existing = TRUE),
    "agent"
  )
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
  old_path <- .genflow_agent_path(from, existing = TRUE)
  new_path <- .genflow_agent_path(to)
  if (!file.exists(old_path)) {
    stop(sprintf("No cached agent named '%s' was found.", from), call. = FALSE)
  }
  if (file.exists(.genflow_agent_path(to, existing = TRUE))) {
    stop(sprintf("An agent named '%s' already exists.", to), call. = FALSE)
  }
  agent <- .genflow_load_object(old_path, "agent")
  agent$name <- to
  .genflow_atomic_save_rds(agent, new_path)
  if (!file.remove(old_path)) {
    unlink(new_path, force = TRUE)
    stop("Could not remove the old agent file after renaming.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Delete a cached agent
#' @param name Agent identifier.
#' @return Invisible TRUE when removed.
#' @export
rm_agent <- function(name) {
  name <- .genflow_validate_name(name, "agent")
  path <- .genflow_agent_path(name, existing = TRUE)
  if (!file.exists(path)) {
    warning(sprintf("No cached agent named '%s' was found.", name), call. = FALSE)
    return(invisible(FALSE))
  }
  if (!file.remove(path)) {
    stop("Could not delete agent '", name, "'.", call. = FALSE)
  }
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
    agent <- .genflow_load_object(path, "agent")
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
