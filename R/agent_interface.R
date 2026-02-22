`%||%` <- function(x, y) if (!is.null(x)) x else y

.MODEL_PROVIDER_LABELS <- c(
  openai = "OpenAI",
  openrouter = "OpenRouter",
  anthropic = "Anthropic",
  groq = "Groq",
  cerebras = "Cerebras",
  together = "Together",
  sambanova = "SambaNova",
  nebius = "Nebius",
  deepseek = "DeepSeek",
  perplexity = "Perplexity",
  fireworks = "Fireworks",
  deepinfra = "DeepInfra",
  hyperbolic = "Hyperbolic",
  gemini = "Gemini",
  fal = "FAL",
  replicate = "Replicate",
  ollama = "Ollama",
  llamacpp = "llama-cpp"
)

.MODEL_SPECIAL_LABELS <- c(
  all = "All providers",
  favorites = "Favorites"
)

.MODEL_ALL_OPTION <- "all"

.MODEL_PROVIDERS <- names(.MODEL_PROVIDER_LABELS)
.DEFAULT_MODEL_SERVICE <- "openai"
.DEFAULT_MODEL_NAME <- "gpt-5"
.DEFAULT_MODEL_TYPE <- "Chat"
.DEFAULT_THINKING_LEVEL <- "medium"
.THINKING_LEVEL_CHOICES <- c("minimal", "low", "medium", "high")

.model_provider_labels <- function() {
  labels <- .MODEL_PROVIDER_LABELS
  custom_labels <- tryCatch(
    {
      if (exists(".genflow_custom_provider_labels", mode = "function", inherits = TRUE)) {
        .genflow_custom_provider_labels()
      } else {
        character()
      }
    },
    error = function(e) character()
  )
  if (length(custom_labels)) {
    labels <- c(labels, custom_labels[setdiff(names(custom_labels), names(labels))])
  }
  labels
}

.model_provider_ids <- function() {
  names(.model_provider_labels())
}

.default_models_dir <- function() {
  tools::R_user_dir("agent_models", which = "data")
}

.favorites_path <- function(directory = NULL) {
  dir <- directory %||% .default_models_dir()
  file.path(dir, "favorites.rds")
}

.empty_favorites <- function() {
  data.frame(
    service = character(),
    model = character(),
    type = character(),
    stringsAsFactors = FALSE
  )
}

.load_favorites <- function(directory = NULL) {
  path <- .favorites_path(directory)
  if (!file.exists(path)) {
    return(.empty_favorites())
  }
  favs <- tryCatch(readRDS(path), error = function(e) NULL)
  if (!is.data.frame(favs) || !all(c("service", "model") %in% names(favs))) {
    return(.empty_favorites())
  }
  if (!"type" %in% names(favs)) {
    favs$type <- NA_character_
  }
  favs$service <- tolower(as.character(favs$service %||% ""))
  favs$model <- as.character(favs$model %||% "")
  favs$type <- as.character(favs$type %||% "")
  favs[!nzchar(favs$service) | !nzchar(favs$model), ] <- NULL
  if (!nrow(favs)) {
    return(.empty_favorites())
  }
  unique(favs)
}

.save_favorites <- function(favorites, directory = NULL) {
  path <- .favorites_path(directory)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(favorites, path)
  invisible(favorites)
}

.normalize_favorites <- function(favorites, catalog) {
  if (!nrow(favorites) || !nrow(catalog)) {
    return(favorites[ , c("service", "model", "type"), drop = FALSE])
  }
  catalog_keys <- paste(tolower(catalog$service), catalog$model, sep = "||")
  fav_keys <- paste(tolower(favorites$service), favorites$model, sep = "||")
  keep <- fav_keys %in% catalog_keys
  favorites <- favorites[keep, , drop = FALSE]
  if (!nrow(favorites)) {
    return(.empty_favorites())
  }
  matched_idx <- match(paste(tolower(favorites$service), favorites$model, sep = "||"), catalog_keys)
  catalog_types <- catalog$type[matched_idx]
  favorites$type[!nzchar(favorites$type) | is.na(favorites$type)] <- catalog_types[!nzchar(favorites$type) | is.na(favorites$type)]
  favorites$type[is.na(favorites$type)] <- ""
  unique(favorites)
}

.resolve_favorite_selection <- function(service, model, type, favorites, catalog) {
  if (!identical(tolower(service), "favorites")) {
    return(list(service = service, model = model, type = type))
  }
  if (!nzchar(model)) {
    stop("Select a favorite model before saving.")
  }
  if (!nrow(favorites)) {
    stop("No favorites available. Add a favorite model first.")
  }
  candidate <- favorites[tolower(favorites$model) == tolower(model), , drop = FALSE]
  if (!nrow(candidate)) {
    stop(sprintf("Model '%s' is not marked as favorite.", model))
  }
  if (nzchar(type)) {
    candidate_type <- candidate[tolower(candidate$type) == tolower(type), , drop = FALSE]
    if (nrow(candidate_type) > 0) {
      candidate <- candidate_type
    }
  }
  resolved_service <- candidate$service[1]
  resolved_type <- candidate$type[1]
  if (!nzchar(resolved_service)) {
    stop("Favorite entry is missing provider information.")
  }
  if (!nzchar(resolved_type)) {
    match_idx <- which(tolower(catalog$service) == resolved_service & catalog$model == model)[1]
    if (!is.na(match_idx)) {
      resolved_type <- catalog$type[match_idx]
    }
  }
  list(
    service = resolved_service,
    model = model,
    type = if (nzchar(resolved_type)) resolved_type else type
  )
}

.model_label <- function(service) {
  if (length(service) == 0) {
    return(character())
  }
  svc <- tolower(service)
  label_lookup <- c(.model_provider_labels(), .MODEL_SPECIAL_LABELS)
  labels <- label_lookup[svc]
  defaults <- tools::toTitleCase(service)
  labels[is.na(labels)] <- defaults[is.na(labels)]
  unname(labels)
}

.load_models_catalog <- function(directory = NULL) {
  dir <- directory %||% .default_models_dir()
  if (!dir.exists(dir)) {
    return(data.frame())
  }
  files <- list.files(dir, pattern = "\\.csv$", full.names = TRUE)
  if (!length(files)) {
    return(data.frame())
  }

  catalogs <- lapply(files, function(path) {
    df <- tryCatch(
      read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
      error = function(e) NULL
    )
    if (is.null(df) || nrow(df) == 0) {
      return(NULL)
    }
    names(df) <- tolower(names(df))
    if (!"service" %in% names(df)) {
      df$service <- tools::file_path_sans_ext(basename(path))
    }
    df$service <- tolower(as.character(df$service))
    df$service[df$service == ""] <- tools::file_path_sans_ext(basename(path))

    if (!"model" %in% names(df)) {
      if ("id" %in% names(df)) {
        df$model <- df$id
      } else {
        df$model <- ""
      }
    }
    df$model <- as.character(df$model)

    if (!"type" %in% names(df)) {
      df$type <- NA_character_
    }
    df$type <- as.character(df$type)

    if (!"pricing" %in% names(df)) {
      df$pricing <- NA_character_
    } else {
      df$pricing <- as.character(df$pricing)
    }

    if (!"description" %in% names(df)) {
      df$description <- NA_character_
    } else {
      df$description <- as.character(df$description)
    }

    df$source_file <- basename(path)
    df
  })

  catalogs <- Filter(Negate(is.null), catalogs)
  if (!length(catalogs)) {
    return(data.frame())
  }

  out <- do.call(rbind, catalogs)
  rownames(out) <- NULL
  out
}

.model_service_choices <- function(catalog, include = character()) {
  services <- character()
  if (nrow(catalog) > 0) {
    services <- unique(catalog$service)
  }
  services <- c(services, include)
  services <- services[nzchar(services)]
  if (!length(services)) {
    return(character())
  }
  services <- unique(services)
  services <- services[order(services)]
  labels <- vapply(services, .model_label, character(1))
  stats::setNames(services, labels)
}

.model_model_choices <- function(catalog, service = NULL, include = character()) {
  rows <- catalog
  if (!is.null(service) && nzchar(service) && nrow(catalog) > 0) {
    rows <- rows[tolower(rows$service) == tolower(service), , drop = FALSE]
  }
  models <- character()
  if (nrow(rows) > 0 && "model" %in% names(rows)) {
    models <- rows$model
  }
  models <- c(models, include)
  models <- models[nzchar(models)]
  if (!length(models)) {
    return(character())
  }
  models <- unique(models)
  models[order(tolower(models))]
}

.model_type_choices <- function(catalog, service = NULL, model = NULL, include = character()) {
  rows <- catalog
  if (!is.null(service) && nzchar(service) && nrow(rows) > 0) {
    rows <- rows[tolower(rows$service) == tolower(service), , drop = FALSE]
  }
  if (!is.null(model) && nzchar(model) && nrow(rows) > 0) {
    rows_model <- rows[tolower(rows$model) == tolower(model), , drop = FALSE]
    if (nrow(rows_model) > 0) {
      rows <- rows_model
    }
  }
  types <- character()
  if (nrow(rows) > 0 && "type" %in% names(rows)) {
    types <- rows$type
  }
  types <- c(types, include)
  types <- types[!is.na(types) & nzchar(types)]
  if (!length(types)) {
    return(character())
  }
  types <- unique(types)
  types[order(tolower(types))]
}

.pick_preferred_choice <- function(options, preferred = NULL) {
  if (!length(options)) {
    return("")
  }
  if (!is.null(preferred) && nzchar(preferred)) {
    match_idx <- match(tolower(preferred), tolower(options))
    if (!is.na(match_idx)) {
      return(options[[match_idx]])
    }
  }
  options[[1]]
}

.normalize_choice_value <- function(value) {
  if (is.null(value) || length(value) == 0) {
    return("")
  }
  scalar <- value[[1]]
  if (is.na(scalar)) {
    return("")
  }
  as.character(scalar)
}

.label_choices <- function(values) {
  if (is.null(values) || !length(values)) {
    return(setNames(character(0), character(0)))
  }
  values_chr <- as.character(values)
  labels <- vapply(values_chr, function(val) {
    if (!nzchar(val)) {
      return("")
    }
    base <- basename(val)
    if (!nzchar(base)) {
      base <- val
    }
    base
  }, character(1), USE.NAMES = FALSE)
  duplicates <- duplicated(labels) | duplicated(labels, fromLast = TRUE)
  if (any(duplicates)) {
    labels[duplicates] <- sprintf("%s (%s)", labels[duplicates], values_chr[duplicates])
  }
  stats::setNames(values_chr, labels)
}

.rand_id <- function(prefix = "id") {
  paste0(prefix, "-", paste(sample(c(letters, LETTERS, 0:9), 8, replace = TRUE), collapse = ""))
}

.default_content_keys <- c("context", "add", "add_img", "label")

.guess_content_mode <- function(value) {
  if (is.null(value)) {
    return("null")
  }
  if (is.character(value) && length(value) == 1) {
    trimmed <- trimws(value)
    if (!nzchar(trimmed)) {
      return("text")
    }
    looks_like_path <- grepl("^[/~]", trimmed) ||
      grepl("^[A-Za-z]:\\\\", trimmed) ||
      grepl("\\.(txt|md|markdown|json|yaml|yml|r|py|csv|tsv)$", trimmed, ignore.case = TRUE)
    if (looks_like_path || file.exists(trimmed)) {
      return("path")
    }
  }
  "text"
}

.safe_read_text <- function(path, max_size = 5 * 1024 * 1024) {
  if (!nzchar(path)) {
    stop("Path is empty.", call. = FALSE)
  }
  expanded <- tryCatch(normalizePath(path, mustWork = TRUE), error = function(e) path)
  if (!file.exists(expanded)) {
    stop(sprintf("File not found: %s", expanded), call. = FALSE)
  }
  info <- file.info(expanded)
  if (is.na(info$size) || info$size > max_size) {
    stop(sprintf("File is too large to load (%s bytes).", format(info$size, big.mark = " ")), call. = FALSE)
  }
  read_file(expanded)
}

.format_preview <- function(text, max_chars = 280) {
  if (is.null(text)) {
    return("(NULL)")
  }
  if (is.list(text) && !is.data.frame(text)) {
    text <- unlist(text, recursive = TRUE, use.names = FALSE)
  }
  if (length(text) == 0) {
    return("(empty)")
  }
  text_chr <- as.character(text)
  if (!length(text_chr)) {
    return("(empty)")
  }
  process_entry <- function(entry) {
    entry <- trimws(entry)
    if (!nzchar(entry)) {
      return("(empty)")
    }
    looks_like_path <- !grepl("\n", entry, fixed = TRUE) && (
      grepl("[/\\\\]", entry) ||
        startsWith(entry, "~") ||
        grepl("^[A-Za-z]:", entry)
    )
    if (looks_like_path) {
      entry <- basename(entry)
    }
    entry
  }
  processed <- vapply(text_chr, process_entry, character(1))
  txt <- trimws(paste(processed, collapse = "\n"))
  if (!nzchar(txt)) {
    return("(empty)")
  }
  if (nchar(txt) > max_chars) {
    paste0(substr(txt, 1, max_chars), "...")
  } else {
    txt
  }
}

.summaries_to_names <- function(entries) {
  if (!length(entries)) {
    return(character())
  }
  sub("\\s+-.*$", "", entries)
}

.convert_value <- function(value_str, mode) {
  if (mode %in% c("null", "na")) {
    return(if (identical(mode, "na")) NA else NULL)
  }
  if (mode == "numeric") {
    out <- suppressWarnings(as.numeric(value_str))
    if (is.na(out)) {
      stop("Value must be numeric.", call. = FALSE)
    }
    return(out)
  }
  if (mode == "logical") {
    val <- tolower(trimws(value_str))
    if (val %in% c("true", "t", "1", "yes")) {
      return(TRUE)
    }
    if (val %in% c("false", "f", "0", "no")) {
      return(FALSE)
    }
    stop("Value must be TRUE or FALSE.", call. = FALSE)
  }
  if (mode == "json") {
    return(fromJSON(value_str, simplifyVector = FALSE))
  }
  value_str
}

.kv_modes <- c(
  "Character" = "text",
  "Numeric" = "numeric",
  "Logical" = "logical",
  "JSON" = "json",
  "NA" = "na",
  "NULL" = "null"
)

.content_modes <- c(
  "Text value" = "text",
  "File path string" = "path",
  "NA" = "na",
  "NULL" = "null"
)

.load_setup_names <- function() {
  .summaries_to_names(list_setups())
}

.load_content_names <- function() {
  .summaries_to_names(list_content())
}

.load_agent_names <- function() {
  .summaries_to_names(list_agents())
}

.theme_css <- "
  body.gf-app { background-color: #f4f6fb; color: #1f2937; }
  .gf-header { margin-bottom: 16px; }
  .gf-header h1 { margin: 0; font-size: 1.8rem; font-weight: 600; }
  .gf-subtitle { color: #4b5563; margin-top: 4px; }
  .gf-tab-body { padding-top: 8px; }
  .gf-panel { background: #ffffff; border-radius: 16px; border: 1px solid #dbe1ea; box-shadow: 0 18px 40px rgba(15, 23, 42, 0.08); padding: 16px 18px; margin-bottom: 16px; }
  .gf-panel h3 { font-size: 1.1rem; font-weight: 600; margin-bottom: 12px; }
  .gf-form-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(240px, 1fr)); gap: 12px; }
  .gf-field-card { border: 1px solid #dbe1ea; border-radius: 12px; padding: 12px 14px; background-color: #f9fafb; margin-bottom: 12px; box-shadow: inset 0 1px 0 rgba(255,255,255,0.6); }
  .gf-field-card h4 { font-size: 1rem; font-weight: 600; margin-bottom: 10px; display: flex; align-items: center; justify-content: space-between; }
  .gf-field-actions { display: flex; flex-wrap: wrap; gap: 8px; margin-top: 8px; }
  .gf-pill { font-size: 0.75rem; color: #4f46e5; background: rgba(79, 70, 229, 0.1); padding: 2px 8px; border-radius: 999px; margin-left: 8px; }
  .gf-list-card { max-height: 420px; overflow-y: auto; padding-right: 4px; }
  .gf-entity-item { padding: 8px 10px; border-radius: 10px; border: 1px solid transparent; cursor: pointer; margin-bottom: 6px; }
  .gf-entity-item.active { border-color: #4f46e5; background-color: rgba(79, 70, 229, 0.08); }
  .gf-entity-item:hover { background-color: rgba(148, 163, 184, 0.18); }
  .gf-entity-name { font-weight: 600; }
  .gf-entity-summary { font-size: 0.8rem; color: #4b5563; }
  .gf-btn-row { display: flex; gap: 10px; flex-wrap: wrap; }
  .gf-empty { padding: 12px; text-align: center; color: #6b7280; border: 1px dashed #cbd5f5; border-radius: 12px; background: rgba(79, 70, 229, 0.04); }
  .gf-preview { font-size: 0.85rem; background: rgba(15, 23, 42, 0.04); padding: 10px 12px; border-radius: 10px; margin-top: 10px; white-space: pre-wrap; max-height: 160px; overflow-y: auto; }
  .gf-section-header { display: flex; align-items: baseline; justify-content: space-between; margin-bottom: 12px; }
  .gf-section-header h3 { margin: 0; }
  #main_tabs .nav-link { font-weight: 600; }
  .gf-cell-scroll { max-height: 96px; overflow-y: auto; padding-right: 4px; white-space: normal; word-break: break-word; }
  .gf-cell-scroll::-webkit-scrollbar { width: 6px; height: 6px; }
  .gf-cell-scroll::-webkit-scrollbar-thumb { background: rgba(79, 70, 229, 0.35); border-radius: 4px; }
  .favorite-star { cursor: pointer; color: #cfd8f3; font-size: 1.1rem; transition: color 0.15s ease-in-out; }
  .favorite-star:hover { color: #facc15; }
  .favorite-star.is-fav { color: #facc15; }
"

.setups_tab_ui <- function() {
  tabPanel(
    "Setups",
    div(
      class = "gf-tab-body",
      fluidRow(
        column(
          width = 4,
          div(
            class = "gf-panel",
            div(
              class = "gf-section-header",
              h3("Saved Setups"),
              actionButton("setup_refresh", label = NULL, icon = icon("rotate"), class = "btn btn-outline-secondary btn-sm")
            ),
            div(
              class = "gf-list-card",
              uiOutput("setup_list_ui")
            ),
            tags$hr(),
            div(
              class = "gf-btn-row",
              actionButton("setup_new", "New", icon = icon("file-circle-plus"), class = "btn-primary"),
              actionButton("setup_duplicate", "Duplicate", icon = icon("copy")),
              actionButton("setup_delete", "Delete", icon = icon("trash"), class = "btn-danger")
            )
          )
        ),
        column(
          width = 8,
          div(
            class = "gf-panel",
            div(class = "gf-section-header", h3("Setup Details")),
            div(
              class = "gf-form-grid",
              textInput("setup_name", "Setup name"),
              selectizeInput("setup_service", "Service", choices = NULL, options = list(create = TRUE)),
              selectizeInput("setup_model", "Model", choices = NULL, options = list(create = TRUE)),
              selectizeInput("setup_type", "Type", choices = NULL, options = list(create = TRUE)),
              numericInput("setup_temp", "Temperature", value = 1, min = 0, max = 5, step = 0.1)
            ),
            conditionalPanel(
              condition = "input.setup_service === 'openrouter'",
              div(
                class = "gf-field-card",
                h4("Online / web search"),
                checkboxInput("setup_openrouter_online", "Add :online to model name", value = FALSE)
              )
            ),
            tags$hr(),
            div(
              class = "gf-section-header",
              h3("Additional fields"),
              actionButton("setup_add_field", "Add field", icon = icon("plus"))
            ),
            div(
              class = "gf-field-card",
              h4("Reasoning / thinking"),
              checkboxInput("setup_thinking_enabled", "Enable thinking / reasoning features", value = FALSE),
              conditionalPanel(
                condition = "input.setup_thinking_enabled",
                selectInput(
                  "setup_thinking_level",
                  "Thinking level",
                  choices = setNames(.THINKING_LEVEL_CHOICES, tools::toTitleCase(.THINKING_LEVEL_CHOICES)),
                  selected = .DEFAULT_THINKING_LEVEL
                )
              )
            ),
            uiOutput("setup_extra_fields"),
            tags$hr(),
            div(
              class = "gf-btn-row",
              actionButton("setup_save", "Save setup", icon = icon("floppy-disk"), class = "btn-success"),
              actionButton("setup_reset", "Reset changes", icon = icon("arrow-rotate-left"))
            )
          )
        )
      )
    )
  )
}

.content_tab_ui <- function() {
  tabPanel(
    "Content",
    div(
      class = "gf-tab-body",
      fluidRow(
        column(
          width = 4,
          div(
            class = "gf-panel",
            div(
              class = "gf-section-header",
              h3("Saved Content"),
              actionButton("content_refresh", label = NULL, icon = icon("rotate"), class = "btn btn-outline-secondary btn-sm")
            ),
            div(
              class = "gf-list-card",
              uiOutput("content_list_ui")
            ),
            tags$hr(),
            div(
              class = "gf-btn-row",
              actionButton("content_new", "New", icon = icon("file-circle-plus"), class = "btn-primary"),
              actionButton("content_duplicate", "Duplicate", icon = icon("copy")),
              actionButton("content_delete", "Delete", icon = icon("trash"), class = "btn-danger")
            )
          )
        ),
        column(
          width = 8,
          div(
            class = "gf-panel",
            div(class = "gf-section-header", h3("Content Definition")),
            textInput("content_name", "Content name"),
            tags$p("Manage reusable prompt fragments or context blocks. Use file import to load large text snippets or set a field to reference a file path."),
            tags$hr(),
            div(
              class = "gf-section-header",
              h3("Fields"),
              div(
                class = "gf-btn-row",
                actionButton("content_add_field", "Add field", icon = icon("plus")),
                actionButton("content_reset_fields", "Restore defaults", icon = icon("arrow-rotate-left"))
              )
            ),
            uiOutput("content_fields_ui"),
            tags$hr(),
            div(
              class = "gf-btn-row",
              actionButton("content_save", "Save content", icon = icon("floppy-disk"), class = "btn-success"),
              actionButton("content_reset", "Reset changes", icon = icon("arrow-rotate-left"))
            )
          )
        )
      )
    )
  )
}

.agents_tab_ui <- function() {
  tabPanel(
    "Agents",
    div(
      class = "gf-tab-body",
      fluidRow(
        column(
          width = 4,
          div(
            class = "gf-panel",
            div(
              class = "gf-section-header",
              h3("Saved Agents"),
              actionButton("agent_refresh", label = NULL, icon = icon("rotate"), class = "btn btn-outline-secondary btn-sm")
            ),
            div(
              class = "gf-list-card",
              uiOutput("agent_list_ui")
            ),
            tags$hr(),
            div(
              class = "gf-btn-row",
              actionButton("agent_new", "New", icon = icon("user-plus"), class = "btn-primary"),
              actionButton("agent_duplicate", "Duplicate", icon = icon("copy")),
              actionButton("agent_delete", "Delete", icon = icon("trash"), class = "btn-danger")
            )
          )
        ),
        column(
          width = 8,
          div(
            class = "gf-panel",
            div(class = "gf-section-header", h3("Agent Configuration")),
            textInput("agent_name", "Agent name"),
            tags$hr(),
            h3("Setup"),
            radioButtons(
              "agent_setup_mode",
              label = NULL,
              choices = c("Use existing setup" = "existing", "Custom setup" = "custom"),
              inline = TRUE
            ),
            conditionalPanel(
              condition = "input.agent_setup_mode === 'existing'",
              selectInput("agent_setup_select", "Setup", choices = character()),
              uiOutput("agent_setup_preview")
            ),
            conditionalPanel(
              condition = "input.agent_setup_mode === 'custom'",
              div(
                class = "gf-form-grid",
                selectizeInput("agent_setup_service", "Service", choices = NULL, options = list(create = TRUE)),
                selectizeInput("agent_setup_model", "Model", choices = NULL, options = list(create = TRUE)),
                selectizeInput("agent_setup_type", "Type", choices = NULL, options = list(create = TRUE)),
                numericInput("agent_setup_temp", "Temperature", value = 1, min = 0, max = 5, step = 0.1)
              ),
              tags$hr(),
              div(
                class = "gf-section-header",
                h3("Custom setup extras"),
                actionButton("agent_setup_add_field", "Add field", icon = icon("plus"))
              ),
              uiOutput("agent_setup_extra_fields")
            ),
            tags$hr(),
            h3("Content"),
            radioButtons(
              "agent_content_mode",
              label = NULL,
              choices = c("None" = "none", "Use existing content" = "existing", "Custom inline content" = "custom"),
              inline = TRUE
            ),
            conditionalPanel(
              condition = "input.agent_content_mode === 'existing'",
              selectInput("agent_content_select", "Content", choices = character()),
              uiOutput("agent_content_preview")
            ),
            conditionalPanel(
              condition = "input.agent_content_mode === 'custom'",
              uiOutput("agent_content_fields_ui"),
              div(
                class = "gf-btn-row",
                actionButton("agent_content_add_field", "Add field", icon = icon("plus")),
                actionButton("agent_content_reset_fields", "Restore defaults", icon = icon("arrow-rotate-left"))
              )
            ),
            tags$hr(),
            div(
              class = "gf-section-header",
              h3("Agent overrides"),
              actionButton("agent_add_override", "Add override", icon = icon("plus"))
            ),
            uiOutput("agent_override_fields"),
            tags$hr(),
            div(
              class = "gf-btn-row",
              actionButton("agent_save", "Save agent", icon = icon("floppy-disk"), class = "btn-success"),
              actionButton("agent_reset", "Reset changes", icon = icon("arrow-rotate-left"))
            )
          )
        )
      )
    )
  )
}

.models_tab_ui <- function() {
  tabPanel(
    "Models",
    div(
      class = "gf-tab-body",
      fluidRow(
        column(
          width = 4,
          div(
            class = "gf-panel",
            div(class = "gf-section-header", h3("Model sources")),
            textInput("models_directory", "Models directory", value = ""),
            div(
              class = "gf-btn-row",
              actionButton("models_refresh", "Reload", icon = icon("rotate")),
              actionButton("models_update_all", "Update all", icon = icon("cloud-arrow-down"))
            ),
            selectInput(
              "models_update_provider",
              "Update single provider",
              choices = setNames(.model_provider_ids(), .model_label(.model_provider_ids())),
              selected = .DEFAULT_MODEL_SERVICE
            ),
            actionButton("models_update_selected", "Update selected provider", icon = icon("download")),
            tags$hr(),
            verbatimTextOutput("models_status", placeholder = TRUE)
          ),
          div(
            class = "gf-panel",
            div(class = "gf-section-header", h3("Custom providers")),
            selectInput(
              "models_custom_provider",
              "Registered custom providers",
              choices = setNames(character(), character()),
              selected = ""
            ),
            div(
              class = "gf-btn-row",
              actionButton("models_custom_add", "Add", icon = icon("plus")),
              actionButton("models_custom_edit", "Edit", icon = icon("pen")),
              actionButton("models_custom_remove", "Remove", icon = icon("trash"), class = "btn-danger")
            ),
            actionButton("models_custom_test", "Test provider", icon = icon("plug")),
            tags$hr(),
            verbatimTextOutput("models_custom_status", placeholder = TRUE)
          )
        ),
        column(
          width = 8,
          div(
            class = "gf-panel",
            div(
              class = "gf-section-header",
              h3("Available models"),
              selectInput(
                "models_view_provider",
                NULL,
                choices = c(
                  setNames(.MODEL_ALL_OPTION, .model_label(.MODEL_ALL_OPTION)),
                  setNames(.model_provider_ids(), .model_label(.model_provider_ids()))
                ),
                selected = .DEFAULT_MODEL_SERVICE
              )
            ),
            textOutput("models_summary"),
            DTOutput("models_table")
          )
        )
      )
    )
  )
}

.transfer_tab_ui <- function() {
  tabPanel(
    "Import / Export",
    div(
      class = "gf-tab-body",
      fluidRow(
        column(
          width = 6,
          div(
            class = "gf-panel",
            div(class = "gf-section-header", h3("Export resources")),
            tags$p("Create a portable bundle with your saved setups, content, agents, and model catalogs."),
            textInput(
              "transfer_export_filename",
              "Bundle filename",
              value = paste0("genflow_bundle_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
            ),
            div(
              class = "gf-field-card",
              h4("Include components"),
              checkboxInput("transfer_include_setups", "Setups", value = TRUE),
              checkboxInput("transfer_include_content", "Content", value = TRUE),
              checkboxInput("transfer_include_agents", "Agents", value = TRUE),
              checkboxInput("transfer_include_models", "Models catalog", value = TRUE)
            ),
            div(
              class = "gf-btn-row",
              downloadButton("transfer_export_download", "Download bundle", icon = icon("download"))
            ),
            uiOutput("transfer_export_summary")
          )
        ),
        column(
          width = 6,
          div(
            class = "gf-panel",
            div(class = "gf-section-header", h3("Import resources")),
            tags$p("Restore a bundle generated on another machine. Select the components to bring into this installation."),
            fileInput("transfer_import_file", "Bundle (.zip)", accept = c(".zip")),
            div(
              class = "gf-field-card",
              h4("Import components"),
              checkboxInput("transfer_import_setups", "Setups", value = TRUE),
              checkboxInput("transfer_import_content", "Content", value = TRUE),
              checkboxInput("transfer_import_agents", "Agents", value = TRUE),
              checkboxInput("transfer_import_models", "Models catalog", value = TRUE)
            ),
            checkboxInput("transfer_import_overwrite", "Overwrite existing files when duplicates are found", value = TRUE),
            div(
              class = "gf-btn-row",
              actionButton("transfer_import_run", "Import bundle", icon = icon("upload"))
            ),
            uiOutput("transfer_import_summary")
          )
        )
      )
    )
  )
}

.app_ui <- function() {
  fluidPage(
    theme = bs_theme(
      version = 5L,
      base_font = "Segoe UI",
      heading_font = "Segoe UI Semibold",
      bg = "#f4f6fb",
      fg = "#1f2937",
      primary = "#4f46e5",
      secondary = "#64748b",
      success = "#10b981",
      danger = "#ef4444",
      warning = "#f59e0b"
    ),
    class = "gf-app",
    tags$head(
      tags$style(HTML(.theme_css))
    ),
    div(
      class = "gf-header",
      h1("genflow agent studio"),
      div(class = "gf-subtitle", "Create, review, and manage setups, content, and agents visually.")
    ),
    tabsetPanel(
      id = "main_tabs",
      .setups_tab_ui(),
      .content_tab_ui(),
      .agents_tab_ui(),
      .models_tab_ui(),
      .transfer_tab_ui()
    )
  )
}

.render_kv_fields <- function(fields, prefix, input, session) {
  if (!length(fields)) {
    return(div(class = "gf-empty", "No additional fields."))
  }
  tagList(lapply(fields, function(field) {
    field_id <- field$id
    name_id <- paste0(prefix, "_", field_id, "_name")
    value_id <- paste0(prefix, "_", field_id, "_value")
    mode_id <- paste0(prefix, "_", field_id, "_mode")
    current_name <- isolate(input[[name_id]]) %||% field$name
    current_value <- isolate(input[[value_id]]) %||% field$value
    current_mode <- isolate(input[[mode_id]]) %||% field$mode
    locked <- isTRUE(field$locked)

    remove_btn <- if (!locked) {
      tags$button(
        type = "button",
        class = "btn btn-link text-danger p-0",
        onclick = sprintf(
          "Shiny.setInputValue('%s', {field: '%s', action: 'remove', nonce: Math.random()}, {priority: 'event'});",
          paste0(prefix, "_action"), field_id
        ),
        icon("trash")
      )
    } else {
      span(class = "gf-pill", "default")
    }

    div(
      class = "gf-field-card",
      h4(
        div(
          if (locked) {
            span(current_name, class = "gf-locked-name")
          } else {
            textInput(
              inputId = name_id,
              label = NULL,
              value = current_name,
              width = "100%",
              placeholder = "Field name"
            )
          },
          if (locked) span(class = "gf-pill", "default")
        ),
        remove_btn
      ),
      selectInput(
        inputId = mode_id,
        label = "Value mode",
        choices = .kv_modes,
        selected = current_mode
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] !== 'null'", mode_id),
        textAreaInput(
          inputId = value_id,
          label = "Value",
          value = current_value,
          rows = 3,
          resize = "vertical"
        )
      )
    )
  }))
}

.render_content_fields <- function(fields, prefix, input) {
  if (!length(fields)) {
    return(div(class = "gf-empty", "No fields defined."))
  }
  tagList(lapply(fields, function(field) {
    field_id <- field$id
    label_id <- paste0(prefix, "_", field_id, "_label")
    mode_id <- paste0(prefix, "_", field_id, "_mode")
    text_id <- paste0(prefix, "_", field_id, "_text")
    path_id <- paste0(prefix, "_", field_id, "_path")
    current_label <- isolate(input[[label_id]]) %||% field$name
    current_mode <- isolate(input[[mode_id]]) %||% field$mode
    current_text <- isolate(input[[text_id]]) %||% field$text_value
    current_path <- isolate(input[[path_id]]) %||% field$path_value
    locked <- isTRUE(field$locked)

    div(
      class = "gf-field-card",
      h4(
        div(
          if (locked) {
            span(current_label)
          } else {
            textInput(
              inputId = label_id,
              label = NULL,
              value = current_label,
              placeholder = "Field name"
            )
          },
          if (locked) span(class = "gf-pill", "default")
        ),
        if (!locked) {
          tags$button(
            type = "button",
            class = "btn btn-link text-danger p-0",
            onclick = sprintf(
              "Shiny.setInputValue('%s', {field: '%s', action: 'remove', nonce: Math.random()}, {priority: 'event'});",
              paste0(prefix, "_action"), field_id
            ),
            icon("trash")
          )
        } else {
          NULL
        }
      ),
      selectInput(
        inputId = mode_id,
        label = "Value mode",
        choices = .content_modes,
        selected = current_mode
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] === 'text'", mode_id),
        tagList(
          textAreaInput(
            inputId = text_id,
            label = "Text content",
            value = current_text,
            rows = 5,
            resize = "vertical"
          ),
          div(
            class = "gf-field-actions",
            tags$button(
              type = "button",
              class = "btn btn-outline-secondary btn-sm",
              onclick = sprintf(
                "Shiny.setInputValue('%s', {field: '%s', action: 'load_file', nonce: Math.random()}, {priority: 'event'});",
                paste0(prefix, "_action"), field_id
              ),
              icon("file-arrow-up"), " Load text from file"
            ),
            tags$button(
              type = "button",
              class = "btn btn-outline-secondary btn-sm",
              onclick = sprintf(
                "Shiny.setInputValue('%s', {field: '%s', action: 'load_path', nonce: Math.random()}, {priority: 'event'});",
                paste0(prefix, "_action"), field_id
              ),
              icon("folder-open"), " Load text from path"
            )
          )
        )
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] === 'path'", mode_id),
        tagList(
          textInput(
            inputId = path_id,
            label = "File path",
            value = current_path,
            placeholder = "/path/to/file.txt"
          ),
          div(
            class = "gf-field-actions",
            tags$button(
              type = "button",
              class = "btn btn-outline-secondary btn-sm",
              onclick = sprintf(
                "Shiny.setInputValue('%s', {field: '%s', action: 'check_path', nonce: Math.random()}, {priority: 'event'});",
                paste0(prefix, "_action"), field_id
              ),
              icon("magnifying-glass"), " Preview file"
            )
          )
        )
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] === 'na'", mode_id),
        div(class = "gf-pill", "Value will be stored as NA.")
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] === 'null'", mode_id),
        div(class = "gf-pill", "Field will be omitted (NULL).")
      )
    )
  }))
}

.setup_summary_text <- function(setup) {
  if (is.null(setup)) {
    return("")
  }
  values <- c(setup$service %||% "", setup$model %||% "", setup$type %||% "")
  temp_value <- setup$temp %||% setup$temperature %||% setup$Temp
  if (!is.null(temp_value) && length(temp_value)) {
    temp_scalar <- temp_value[[1]]
    if (!is.null(temp_scalar) && !is.na(temp_scalar) && nzchar(as.character(temp_scalar))) {
      temp_numeric <- suppressWarnings(as.numeric(temp_scalar))
      temp_label <- if (!is.na(temp_numeric)) {
        sprintf("Temp %s", format(temp_numeric, trim = TRUE, digits = 3))
      } else {
        sprintf("Temp %s", as.character(temp_scalar))
      }
      values <- c(values, temp_label)
    }
  }
  values <- values[nzchar(values)]
  if (length(values)) paste(values, collapse = " * ") else ""
}

.content_summary_text <- function(content) {
  if (is.null(content) || !length(content)) {
    return("")
  }
  keys <- names(content)
  if (is.null(keys)) {
    keys <- paste0("item", seq_along(content))
  }
  previews <- vapply(content, function(x) .format_preview(x, 80), character(1))
  paste(keys, previews, sep = ": ", collapse = " | ")
}

.agent_summary_text <- function(agent) {
  if (is.null(agent)) {
    return("")
  }
  values <- c(agent$service %||% "", agent$model %||% "", agent$sname %||% "", agent$cname %||% "")
  values <- values[nzchar(values)]
  if (length(values)) paste(values, collapse = " * ") else ""
}

.build_content_fields <- function(content_list = list()) {
  existing_names <- names(content_list)
  fields <- list()
  for (name in .default_content_keys) {
    value <- if (name %in% existing_names) content_list[[name]] else ""
    mode <- .guess_content_mode(value)
    fields[[length(fields) + 1]] <- list(
      id = .rand_id("content"),
      name = name,
      locked = TRUE,
      mode = mode,
      text_value = if (identical(mode, "text")) paste(value, collapse = "\n") else "",
      path_value = if (identical(mode, "path")) paste(value, collapse = "\n") else ""
    )
  }
  if (length(existing_names)) {
    extras <- setdiff(existing_names, .default_content_keys)
    if (length(extras)) {
      for (nm in extras) {
        value <- content_list[[nm]]
        mode <- .guess_content_mode(value)
        fields[[length(fields) + 1]] <- list(
          id = .rand_id("content"),
          name = nm,
          locked = FALSE,
          mode = mode,
          text_value = if (identical(mode, "text")) paste(value, collapse = "\n") else "",
          path_value = if (identical(mode, "path")) paste(value, collapse = "\n") else ""
        )
      }
    }
  }
  fields
}

.build_setup_extras <- function(setup_list = list()) {
  base_fields <- c("sname", "service", "model", "temp", "type")
  special_fields <- c("thinking", "thinking_budget", "thinking_level", "reasoning", "reasoning_effort")
  extras <- setdiff(names(setup_list), c(base_fields, special_fields))
  if (!length(extras)) {
    return(list())
  }
  lapply(extras, function(nm) {
    value <- setup_list[[nm]]
    mode <- "text"
    if (is.numeric(value)) mode <- "numeric"
    if (is.logical(value)) mode <- "logical"
    if (mode == "text" && is.list(value)) {
      mode <- "json"
    }
    if (mode == "text" && is.data.frame(value)) {
      mode <- "json"
    }
    if (mode == "text" && is.character(value) && length(value) == 1) {
      parsed <- tryCatch(fromJSON(value, simplifyVector = FALSE), error = function(e) NULL)
      if (!is.null(parsed)) {
        value <- parsed
        mode <- "json"
      }
    }
    value_str <- if (identical(mode, "json")) {
      toJSON(value, auto_unbox = TRUE, pretty = TRUE)
    } else if (is.atomic(value)) {
      paste(value, collapse = "\n")
    } else {
      toJSON(value, auto_unbox = TRUE, pretty = TRUE)
    }
    list(
      id = .rand_id("setup"),
      name = nm,
      value = value_str,
      mode = mode,
      locked = FALSE
    )
  })
}

.build_override_fields <- function(agent_list, setup_fields = list(), content_fields = list()) {
  base_fields <- c("name", "sname", "cname")
  inherited <- c(names(setup_fields), names(content_fields))
  extras <- setdiff(names(agent_list), c(base_fields, inherited))
  if (!length(extras)) {
    return(list())
  }
  lapply(extras, function(nm) {
    value <- agent_list[[nm]]
    mode <- "text"
    if (is.numeric(value)) mode <- "numeric"
    if (is.logical(value)) mode <- "logical"
    if (mode == "text" && is.list(value)) {
      mode <- "json"
    }
    if (mode == "text" && is.data.frame(value)) {
      mode <- "json"
    }
    if (mode == "text" && is.character(value) && length(value) == 1) {
      parsed <- tryCatch(fromJSON(value, simplifyVector = FALSE), error = function(e) NULL)
      if (!is.null(parsed)) {
        value <- parsed
        mode <- "json"
      }
    }
    value_str <- if (identical(mode, "json")) {
      toJSON(value, auto_unbox = TRUE, pretty = TRUE)
    } else if (is.atomic(value)) {
      paste(value, collapse = "\n")
    } else {
      toJSON(value, auto_unbox = TRUE, pretty = TRUE)
    }
    list(
      id = .rand_id("override"),
      name = nm,
      value = value_str,
      mode = mode,
      locked = FALSE
    )
  })
}

ui <- .app_ui()

server <- function(input, output, session) {
  setup_state <- reactiveValues(
    list = character(),
    selected = NULL,
    original = NULL,
    extras = list(),
    desired_service = NULL,
    desired_model = NULL,
    desired_type = NULL,
    thinking_enabled = FALSE,
    thinking_level = .DEFAULT_THINKING_LEVEL,
    loading = FALSE,
    draft_name = NULL,
    draft_summary = NULL,
    draft_source = NULL,
    skip_service_event = FALSE,
    openrouter_online = FALSE,
    openrouter_model_base = NULL
  )

  content_state <- reactiveValues(
    list = character(),
    selected = NULL,
    original = NULL,
    fields = .build_content_fields(list()),
    draft_name = NULL,
    draft_summary = NULL,
    draft_source = NULL
  )

  agent_state <- reactiveValues(
    list = character(),
    selected = NULL,
    original = NULL,
    setup_extras = list(),
    custom_content_fields = .build_content_fields(list()),
    overrides = list(),
    content_modal_field = NULL,
    custom_desired_service = NULL,
    custom_desired_model = NULL,
    custom_desired_type = NULL,
    loading = FALSE,
    draft_name = NULL,
    draft_summary = NULL,
    draft_source = NULL
  )

  setup_summary_cache <- new.env(parent = emptyenv())
  content_preview_cache <- new.env(parent = emptyenv())
  agent_summary_cache <- new.env(parent = emptyenv())

  initial_models_dir <- .default_models_dir()
  initial_catalog <- .load_models_catalog(initial_models_dir)
  initial_favorites <- .normalize_favorites(.load_favorites(initial_models_dir), initial_catalog)

  models_state <- reactiveValues(
    directory = initial_models_dir,
    catalog = initial_catalog,
    favorites = initial_favorites,
    favorites_present = nrow(initial_favorites) > 0,
    status = "",
    custom_status = ""
  )

  custom_provider_state <- reactiveValues(
    mode = "add",
    edit_id = NULL
  )

  transfer_state <- reactiveValues(
    export_summary = NULL,
    import_summary = NULL
  )

  current_models_dir <- function() {
    dir <- models_state$directory %||% ""
    if (!nzchar(dir)) {
      dir <- .default_models_dir()
    }
    dir
  }

  format_transfer_counts <- function(counts) {
    if (is.null(counts) || !length(counts)) {
      return("No items processed.")
    }
    parts <- character()
    add_part <- function(value, singular, plural = NULL) {
      if (is.null(value) || is.na(value) || value <= 0) {
        return()
      }
      label <- if (identical(value, 1L) || identical(value, 1)) {
        singular
      } else {
        plural %||% paste0(singular, "s")
      }
      parts <<- c(parts, sprintf("%d %s", value, label))
    }
    add_part(counts$setups, "setup")
    add_part(counts$content, "content entry", "content entries")
    add_part(counts$agents, "agent")
    add_part(counts$models, "model file")
    if (!length(parts)) {
      "No items processed."
    } else {
      paste(parts, collapse = " | ")
    }
  }

  parse_multi_values <- function(text) {
    value <- as.character(text %||% "")[1]
    if (!nzchar(trimws(value))) {
      return(character())
    }
    parts <- unlist(strsplit(value, "[,;\\n\\r]+", perl = TRUE), use.names = FALSE)
    parts <- trimws(parts)
    unique(parts[nzchar(parts)])
  }

  format_header_lines <- function(headers) {
    if (is.null(headers) || !length(headers)) {
      return("")
    }
    header_names <- names(headers)
    if (is.null(header_names)) {
      return("")
    }
    lines <- character()
    for (nm in header_names) {
      key <- trimws(as.character(nm %||% "")[1])
      value <- trimws(as.character(headers[[nm]] %||% "")[1])
      if (!nzchar(key) || !nzchar(value)) {
        next
      }
      lines <- c(lines, paste0(key, ": ", value))
    }
    paste(lines, collapse = "\n")
  }

  parse_header_lines <- function(text) {
    value <- as.character(text %||% "")[1]
    if (!nzchar(trimws(value))) {
      return(list())
    }
    lines <- unlist(strsplit(value, "\n", fixed = TRUE), use.names = FALSE)
    headers <- list()
    for (line in lines) {
      line <- trimws(line)
      if (!nzchar(line)) {
        next
      }
      delim <- regexpr(":", line, fixed = TRUE)[1]
      if (is.na(delim) || delim < 1) {
        delim <- regexpr("=", line, fixed = TRUE)[1]
      }
      if (is.na(delim) || delim < 1) {
        stop("Invalid header line: '", line, "'. Use `Header: value`.", call. = FALSE)
      }
      key <- trimws(substr(line, 1, delim - 1))
      val <- trimws(substr(line, delim + 1, nchar(line)))
      if (!nzchar(key) || !nzchar(val)) {
        stop("Invalid header line: '", line, "'.", call. = FALSE)
      }
      headers[[key]] <- val
    }
    headers
  }

  custom_provider_choices <- function() {
    providers <- .genflow_list_custom_provider_configs()
    if (!length(providers)) {
      return(setNames(character(), character()))
    }
    ids <- names(providers)
    labels <- vapply(providers, function(cfg) {
      label <- trimws(as.character(cfg$label %||% cfg$id)[1])
      if (!nzchar(label)) {
        label <- cfg$id
      }
      sprintf("%s (%s)", label, cfg$id)
    }, character(1))
    stats::setNames(ids, labels)
  }

  refresh_provider_selectors <- function(preferred_custom = NULL) {
    provider_ids <- .model_provider_ids()
    base_choices <- setNames(provider_ids, .model_label(provider_ids))
    provider_choices_view <- c(setNames(.MODEL_ALL_OPTION, .model_label(.MODEL_ALL_OPTION)), base_choices)
    if (models_state$favorites_present) {
      provider_choices_view <- c(setNames("favorites", .model_label("favorites")), provider_choices_view)
    }

    current_view <- isolate(input$models_view_provider)
    if (is.null(current_view) || !current_view %in% provider_choices_view) {
      if (models_state$favorites_present) {
        current_view <- "favorites"
      } else if (.DEFAULT_MODEL_SERVICE %in% provider_ids) {
        current_view <- .DEFAULT_MODEL_SERVICE
      } else {
        current_view <- .MODEL_ALL_OPTION
      }
    }
    updateSelectInput(session, "models_view_provider", choices = provider_choices_view, selected = current_view)

    current_update <- isolate(input$models_update_provider)
    if (is.null(current_update) || !current_update %in% base_choices) {
      if (.DEFAULT_MODEL_SERVICE %in% provider_ids) {
        current_update <- .DEFAULT_MODEL_SERVICE
      } else if (length(provider_ids)) {
        current_update <- provider_ids[[1]]
      } else {
        current_update <- ""
      }
    }
    updateSelectInput(session, "models_update_provider", choices = base_choices, selected = current_update)

    custom_choices <- custom_provider_choices()
    current_custom <- preferred_custom %||% isolate(input$models_custom_provider) %||% ""
    if (!length(custom_choices)) {
      current_custom <- ""
    } else if (!nzchar(current_custom) || !current_custom %in% custom_choices) {
      current_custom <- unname(custom_choices[[1]])
    }
    updateSelectInput(session, "models_custom_provider", choices = custom_choices, selected = current_custom)

    update_setup_service_choices()
    update_agent_service_choices()
  }

  format_custom_provider_test <- function(test_result) {
    provider_id <- as.character(test_result$provider_id %||% "")[1]
    if (is.na(provider_id)) provider_id <- ""
    provider_label <- as.character(test_result$provider_label %||% provider_id)[1]
    if (is.na(provider_label)) provider_label <- provider_id
    status_api <- as.character(test_result$status_api %||% "ERROR")[1]
    if (is.na(status_api)) status_api <- "ERROR"
    status_msg <- as.character(test_result$status_msg %||% "")[1]
    if (is.na(status_msg)) status_msg <- ""
    endpoint <- as.character(test_result$endpoint %||% "")[1]
    if (is.na(endpoint)) endpoint <- ""
    model_count <- as.integer(test_result$model_count %||% 0L)
    models <- as.character(test_result$models %||% character())
    attempts <- as.character(test_result$attempted_endpoints %||% character())
    attempts <- attempts[nzchar(attempts)]
    attempts <- utils::head(attempts, 8)

    lines <- c(
      sprintf("Provider: %s (%s)", provider_label, provider_id),
      paste0("Status: ", status_api),
      paste0("Message: ", status_msg)
    )
    if (nzchar(endpoint)) {
      lines <- c(lines, paste0("Endpoint: ", endpoint))
    }
    lines <- c(lines, paste0("Model count: ", model_count))
    if (length(models)) {
      lines <- c(lines, paste0("Sample models: ", paste(models, collapse = ", ")))
    }
    if (length(attempts)) {
      lines <- c(lines, paste0("Tried: ", paste(attempts, collapse = " | ")))
    }
    paste(lines, collapse = "\n")
  }

  show_custom_provider_modal <- function(mode = c("add", "edit"), cfg = NULL) {
    mode <- match.arg(mode)
    cfg <- cfg %||% list()
    custom_provider_state$mode <- mode
    custom_provider_state$edit_id <- if (identical(mode, "edit")) trimws(as.character(cfg$id %||% "")[1]) else NULL

    id_value <- trimws(as.character(cfg$id %||% "")[1])
    label_value <- trimws(as.character(cfg$label %||% "")[1])
    base_urls_value <- paste(as.character(cfg$base_urls %||% character()), collapse = "\n")
    api_key_env_value <- trimws(as.character(cfg$api_key_env %||% "")[1])
    model_env_value <- trimws(as.character(cfg$model_env %||% "")[1])
    default_model_value <- trimws(as.character(cfg$default_model %||% "local-model")[1])
    chat_paths_value <- paste(as.character(cfg$chat_paths %||% c("/v1/chat/completions", "/chat/completions")), collapse = "\n")
    model_paths_value <- paste(as.character(cfg$model_paths %||% c("/v1/models", "/models")), collapse = "\n")
    auth_header_value <- as.character(cfg$auth_header %||% "Authorization")[1]
    auth_prefix_value <- as.character(cfg$auth_prefix %||% "Bearer")[1]
    extra_headers_value <- format_header_lines(cfg$extra_headers %||% list())
    api_key_required_value <- if (is.null(cfg$api_key_required)) nzchar(api_key_env_value) else isTRUE(cfg$api_key_required)
    supports_tools_value <- if (is.null(cfg$supports_tools)) TRUE else isTRUE(cfg$supports_tools)
    supports_vision_value <- if (is.null(cfg$supports_vision)) TRUE else isTRUE(cfg$supports_vision)
    supports_reasoning_value <- if (is.null(cfg$supports_reasoning)) FALSE else isTRUE(cfg$supports_reasoning)
    supports_plugins_value <- if (is.null(cfg$supports_plugins)) FALSE else isTRUE(cfg$supports_plugins)
    reasoning_field_value <- as.character(cfg$reasoning_field %||% "")[1]
    plugins_field_value <- as.character(cfg$plugins_field %||% "")[1]
    max_tokens_value <- if (is.null(cfg$max_tokens) || is.na(cfg$max_tokens)) "" else as.character(cfg$max_tokens)

    showModal(modalDialog(
      title = if (identical(mode, "add")) "Add custom provider" else sprintf("Edit custom provider: %s", id_value),
      div(
        class = "gf-form-grid",
        textInput("models_custom_id", "Provider id", value = id_value, placeholder = "myprovider"),
        textInput("models_custom_label", "Label", value = label_value, placeholder = "My Provider"),
        textInput("models_custom_api_key_env", "API key env var", value = api_key_env_value, placeholder = "MYPROVIDER_API_KEY"),
        textInput("models_custom_model_env", "Default model env var", value = model_env_value, placeholder = "MYPROVIDER_MODEL"),
        textInput("models_custom_default_model", "Fallback model", value = default_model_value),
        textInput("models_custom_auth_header", "Auth header", value = auth_header_value, placeholder = "Authorization"),
        textInput("models_custom_auth_prefix", "Auth prefix", value = auth_prefix_value, placeholder = "Bearer"),
        textInput("models_custom_reasoning_field", "Reasoning field (optional)", value = reasoning_field_value, placeholder = "reasoning"),
        textInput("models_custom_plugins_field", "Plugins field (optional)", value = plugins_field_value, placeholder = "plugins"),
        textInput("models_custom_max_tokens", "Max tokens (optional)", value = max_tokens_value, placeholder = "8192")
      ),
      tags$hr(),
      textAreaInput("models_custom_base_urls", "Base URLs (one per line)", value = base_urls_value, rows = 3, resize = "vertical"),
      textAreaInput("models_custom_chat_paths", "Chat endpoints (one per line)", value = chat_paths_value, rows = 2, resize = "vertical"),
      textAreaInput("models_custom_model_paths", "Model endpoints (one per line)", value = model_paths_value, rows = 2, resize = "vertical"),
      textAreaInput("models_custom_extra_headers", "Extra headers (Header: value per line)", value = extra_headers_value, rows = 3, resize = "vertical"),
      div(
        class = "gf-btn-row",
        checkboxInput("models_custom_api_key_required", "Require API key", value = api_key_required_value),
        checkboxInput("models_custom_supports_tools", "Supports tools", value = supports_tools_value),
        checkboxInput("models_custom_supports_vision", "Supports vision", value = supports_vision_value),
        checkboxInput("models_custom_supports_reasoning", "Supports reasoning", value = supports_reasoning_value),
        checkboxInput("models_custom_supports_plugins", "Supports plugins", value = supports_plugins_value)
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("models_custom_save", "Save provider", class = "btn-primary")
      ),
      size = "l",
      easyClose = TRUE
    ))
  }

  update_setup_service_choices <- function() {
    catalog <- models_state$catalog
    current_raw <- isolate(input$setup_service)
    desired_raw <- setup_state$desired_service
    current <- .normalize_choice_value(current_raw)
    desired <- .normalize_choice_value(desired_raw)
    choices <- .model_service_choices(catalog, include = c(current, desired, .model_provider_ids()))
    choices <- choices[choices != "favorites"]
    if (nrow(models_state$favorites) > 0) {
      choices <- c(setNames("favorites", "Favorites"), choices)
    }
    if (!length(choices)) {
      selected_default <- if (!is.null(desired_raw)) desired else if (!is.null(current_raw)) current else ""
      updateSelectizeInput(session, "setup_service", choices = character(), selected = selected_default, server = TRUE)
      setup_state$desired_service <- NULL
      return()
    }
    selected <- if (!is.null(desired_raw)) desired else current
    if (!nzchar(selected) || !selected %in% choices) {
      selected <- .pick_preferred_choice(choices, .DEFAULT_MODEL_SERVICE)
    }
    updateSelectizeInput(session, "setup_service", choices = choices, selected = selected, server = TRUE)
    setup_state$desired_service <- NULL
  }

  update_setup_model_choices <- function() {
    catalog <- models_state$catalog
    service <- .normalize_choice_value(input$setup_service)
    current_raw <- isolate(input$setup_model)
    desired_raw <- setup_state$desired_model
    current <- .normalize_choice_value(current_raw)
    desired <- .normalize_choice_value(desired_raw)
    include_values <- c(current, desired)
    if (identical(tolower(service), "favorites")) {
      favs <- models_state$favorites
      base_choices <- if (nrow(favs)) favs$model else character()
      choices <- c(base_choices, include_values)
      choices <- choices[nzchar(choices)]
      choices <- unique(choices)
      if (!length(choices)) {
        updateSelectizeInput(session, "setup_model", choices = character(), selected = "", server = TRUE)
        if (!isTRUE(setup_state$loading)) setup_state$desired_model <- NULL
        return()
      }
      labels <- vapply(choices, function(m) {
        fav_row <- favs[favs$model == m, , drop = FALSE]
        if (!nrow(fav_row)) {
          return(m)
        }
        provider_label <- .model_label(fav_row$service[1])
        type_label <- fav_row$type[which(nzchar(fav_row$type))[1]]
        if (!is.null(type_label) && nzchar(type_label)) {
          sprintf("%s (%s - %s)", m, provider_label, type_label)
        } else {
          sprintf("%s (%s)", m, provider_label)
        }
      }, character(1))
      named_choices <- stats::setNames(choices, labels)
      selected <- if (!is.null(desired_raw)) desired else current
      if (!nzchar(selected) || !selected %in% choices) {
        selected <- choices[[1]]
      }
      updateSelectizeInput(session, "setup_model", choices = named_choices, selected = selected, server = TRUE)
      if (!isTRUE(setup_state$loading)) setup_state$desired_model <- NULL
      return()
    }

    choices <- .model_model_choices(catalog, service, include = include_values)
    if (!length(choices) && nzchar(desired)) {
      choices <- desired
    }
    if (!length(choices) && nzchar(current)) {
      choices <- current
    }
    selected <- if (!is.null(desired_raw)) desired else current
    if (!length(choices)) {
      selected_default <- if (nzchar(selected)) selected else ""
      updateSelectizeInput(session, "setup_model", choices = choices, selected = selected_default, server = TRUE)
      if (!isTRUE(setup_state$loading)) setup_state$desired_model <- NULL
      return()
    }
    if (!nzchar(selected) || !selected %in% choices) {
      preferred <- if (tolower(service) == .DEFAULT_MODEL_SERVICE) .DEFAULT_MODEL_NAME else NULL
      selected <- .pick_preferred_choice(choices, preferred)
    }
    updateSelectizeInput(session, "setup_model", choices = choices, selected = selected, server = TRUE)
    if (!isTRUE(setup_state$loading)) setup_state$desired_model <- NULL
  }

  update_setup_type_choices <- function() {
    catalog <- models_state$catalog
    service <- .normalize_choice_value(input$setup_service)
    model <- .normalize_choice_value(input$setup_model)
    current_raw <- isolate(input$setup_type)
    desired_raw <- setup_state$desired_type
    current <- .normalize_choice_value(current_raw)
    desired <- .normalize_choice_value(desired_raw)
    include_values <- c(current, desired)
    if (identical(tolower(service), "favorites")) {
      favs <- models_state$favorites
      fav_rows <- favs[favs$model == model, , drop = FALSE]
      fav_types <- fav_rows$type[nzchar(fav_rows$type)]
      if (!length(fav_types) && nrow(fav_rows) > 0) {
        actual_service <- fav_rows$service[1]
        fav_types <- .model_type_choices(catalog, actual_service, model)
      }
      choices <- c(fav_types, include_values)
      choices <- choices[nzchar(choices)]
      choices <- unique(choices)
      selected <- if (!is.null(desired_raw)) desired else current
      if (!length(choices)) {
        selected_default <- if (nzchar(selected)) selected else ""
        updateSelectizeInput(session, "setup_type", choices = character(), selected = selected_default, server = TRUE)
        if (!isTRUE(setup_state$loading)) setup_state$desired_type <- NULL
        return()
      }
      if (!nzchar(selected) || !selected %in% choices) {
        selected <- choices[[1]]
      }
      updateSelectizeInput(session, "setup_type", choices = choices, selected = selected, server = TRUE)
      if (!isTRUE(setup_state$loading)) setup_state$desired_type <- NULL
      return()
    }
    choices <- .model_type_choices(catalog, service, model, include = include_values)
    if (!length(choices) && nzchar(desired)) {
      choices <- desired
    }
    if (!length(choices) && nzchar(current)) {
      choices <- current
    }
    selected <- if (!is.null(desired_raw)) desired else current
    if (!length(choices)) {
      selected_default <- if (nzchar(selected)) selected else ""
      updateSelectizeInput(session, "setup_type", choices = character(), selected = selected_default, server = TRUE)
      if (!isTRUE(setup_state$loading)) setup_state$desired_type <- NULL
      return()
    }
    if (!nzchar(selected) || !selected %in% choices) {
      preferred <- if (tolower(model) == .DEFAULT_MODEL_NAME) .DEFAULT_MODEL_TYPE else NULL
      selected <- .pick_preferred_choice(choices, preferred)
    }
    updateSelectizeInput(session, "setup_type", choices = choices, selected = selected, server = TRUE)
    if (!isTRUE(setup_state$loading)) setup_state$desired_type <- NULL
  }

  update_agent_service_choices <- function() {
    catalog <- models_state$catalog
    current_raw <- isolate(input$agent_setup_service)
    desired_raw <- agent_state$custom_desired_service
    current <- .normalize_choice_value(current_raw)
    desired <- .normalize_choice_value(desired_raw)
    choices <- .model_service_choices(catalog, include = c(current, desired, .model_provider_ids()))
    choices <- choices[choices != "favorites"]
    if (nrow(models_state$favorites) > 0) {
      choices <- c(setNames("favorites", "Favorites"), choices)
    }
    if (!length(choices)) {
      selected_default <- if (!is.null(desired_raw)) desired else if (!is.null(current_raw)) current else ""
      updateSelectizeInput(session, "agent_setup_service", choices = character(), selected = selected_default, server = TRUE)
      agent_state$custom_desired_service <- NULL
      return()
    }
    selected <- if (!is.null(desired_raw)) desired else current
    if (!nzchar(selected) || !selected %in% choices) {
      selected <- .pick_preferred_choice(choices, .DEFAULT_MODEL_SERVICE)
    }
    updateSelectizeInput(session, "agent_setup_service", choices = choices, selected = selected, server = TRUE)
    agent_state$custom_desired_service <- NULL
  }

  update_agent_model_choices <- function() {
    catalog <- models_state$catalog
    service <- .normalize_choice_value(input$agent_setup_service)
    current_raw <- isolate(input$agent_setup_model)
    desired_raw <- agent_state$custom_desired_model
    current <- .normalize_choice_value(current_raw)
    desired <- .normalize_choice_value(desired_raw)
    include_values <- c(current, desired)
    if (identical(tolower(service), "favorites")) {
      favs <- models_state$favorites
      base_choices <- if (nrow(favs)) favs$model else character()
      choices <- c(base_choices, include_values)
      choices <- choices[nzchar(choices)]
      choices <- unique(choices)
      if (!length(choices)) {
        updateSelectizeInput(session, "agent_setup_model", choices = character(), selected = "", server = TRUE)
        agent_state$custom_desired_model <- NULL
        return()
      }
      labels <- vapply(choices, function(m) {
        fav_row <- favs[favs$model == m, , drop = FALSE]
        if (!nrow(fav_row)) {
          return(m)
        }
        provider_label <- .model_label(fav_row$service[1])
        type_label <- fav_row$type[which(nzchar(fav_row$type))[1]]
        if (!is.null(type_label) && nzchar(type_label)) {
          sprintf("%s (%s - %s)", m, provider_label, type_label)
        } else {
          sprintf("%s (%s)", m, provider_label)
        }
      }, character(1))
      named_choices <- stats::setNames(choices, labels)
      selected <- if (!is.null(desired_raw)) desired else current
      if (!nzchar(selected) || !selected %in% choices) {
        selected <- choices[[1]]
      }
      updateSelectizeInput(session, "agent_setup_model", choices = named_choices, selected = selected, server = TRUE)
      agent_state$custom_desired_model <- NULL
      return()
    }

    choices <- .model_model_choices(catalog, service, include = include_values)
    if (!length(choices) && nzchar(desired)) {
      choices <- desired
    }
    if (!length(choices) && nzchar(current)) {
      choices <- current
    }
    selected <- if (!is.null(desired_raw)) desired else current
    if (!length(choices)) {
      selected_default <- if (nzchar(selected)) selected else ""
      updateSelectizeInput(session, "agent_setup_model", choices = character(), selected = selected_default, server = TRUE)
      agent_state$custom_desired_model <- NULL
      return()
    }
    if (!nzchar(selected) || !selected %in% choices) {
      preferred <- if (tolower(service) == .DEFAULT_MODEL_SERVICE) .DEFAULT_MODEL_NAME else NULL
      selected <- .pick_preferred_choice(choices, preferred)
    }
    updateSelectizeInput(session, "agent_setup_model", choices = choices, selected = selected, server = TRUE)
    agent_state$custom_desired_model <- NULL
  }

  update_agent_type_choices <- function() {
    catalog <- models_state$catalog
    service <- .normalize_choice_value(input$agent_setup_service)
    model <- .normalize_choice_value(input$agent_setup_model)
    current_raw <- isolate(input$agent_setup_type)
    desired_raw <- agent_state$custom_desired_type
    current <- .normalize_choice_value(current_raw)
    desired <- .normalize_choice_value(desired_raw)
    include_values <- c(current, desired)
    if (identical(tolower(service), "favorites")) {
      favs <- models_state$favorites
      fav_rows <- favs[favs$model == model, , drop = FALSE]
      fav_types <- fav_rows$type[nzchar(fav_rows$type)]
      if (!length(fav_types) && nrow(fav_rows) > 0) {
        actual_service <- fav_rows$service[1]
        fav_types <- .model_type_choices(catalog, actual_service, model)
      }
      choices <- c(fav_types, include_values)
      choices <- choices[nzchar(choices)]
      choices <- unique(choices)
      selected <- if (!is.null(desired_raw)) desired else current
      if (!length(choices)) {
        selected_default <- if (nzchar(selected)) selected else ""
        updateSelectizeInput(session, "agent_setup_type", choices = character(), selected = selected_default, server = TRUE)
        agent_state$custom_desired_type <- NULL
        return()
      }
      if (!nzchar(selected) || !selected %in% choices) {
        selected <- choices[[1]]
      }
      updateSelectizeInput(session, "agent_setup_type", choices = choices, selected = selected, server = TRUE)
      agent_state$custom_desired_type <- NULL
      return()
    }
    choices <- .model_type_choices(catalog, service, model, include = include_values)
    if (!length(choices) && nzchar(desired)) {
      choices <- desired
    }
    if (!length(choices) && nzchar(current)) {
      choices <- current
    }
    selected <- if (!is.null(desired_raw)) desired else current
    if (!length(choices)) {
      selected_default <- if (nzchar(selected)) selected else ""
      updateSelectizeInput(session, "agent_setup_type", choices = character(), selected = selected_default, server = TRUE)
      agent_state$custom_desired_type <- NULL
      return()
    }
    if (!nzchar(selected) || !selected %in% choices) {
      preferred <- if (tolower(model) == .DEFAULT_MODEL_NAME) .DEFAULT_MODEL_TYPE else NULL
      selected <- .pick_preferred_choice(choices, preferred)
    }
    updateSelectizeInput(session, "agent_setup_type", choices = choices, selected = selected, server = TRUE)
    agent_state$custom_desired_type <- NULL
  }

  observeEvent(models_state$catalog, {
    update_setup_service_choices()
  })

  observeEvent(list(input$setup_service, models_state$catalog), {
    if (isTRUE(setup_state$loading)) return()
    update_setup_model_choices()
  })

  observeEvent(list(input$setup_service, input$setup_model, models_state$catalog), {
    if (isTRUE(setup_state$loading)) return()
    update_setup_type_choices()
  })

  observeEvent(input$setup_service,
    {
      if (isTRUE(setup_state$loading)) {
        return()
      }
      if (isTRUE(setup_state$skip_service_event)) {
        return()
      }
      svc <- input$setup_service %||% ""
      svc_lower <- tolower(svc)
      current_model_input <- isolate(input$setup_model) %||% ""
      if (!identical(svc_lower, "openrouter")) {
        if (isTRUE(setup_state$openrouter_online) || isTRUE(isolate(input$setup_openrouter_online))) {
          setup_state$openrouter_online <- FALSE
          updateCheckboxInput(session, "setup_openrouter_online", value = FALSE)
        }
        setup_state$openrouter_model_base <- NULL
      } else {
        base_model <- sub(":online$", "", current_model_input)
        setup_state$openrouter_model_base <- if (nzchar(base_model)) base_model else NULL
        desired_checkbox <- isTRUE(setup_state$openrouter_online)
        if (!identical(isTRUE(isolate(input$setup_openrouter_online)), desired_checkbox)) {
          updateCheckboxInput(session, "setup_openrouter_online", value = desired_checkbox)
        }
      }
      catalog <- models_state$catalog
      current_model <- isolate(input$setup_model) %||% ""
      preferred_model <- if (tolower(svc) == .DEFAULT_MODEL_SERVICE) .DEFAULT_MODEL_NAME else NULL
      desired_model <- setup_state$desired_model %||% current_model
      model_choices <- .model_model_choices(catalog, svc, include = c(current_model, desired_model))
      selected_model <- if (nzchar(desired_model) && desired_model %in% model_choices) {
        desired_model
      } else if (nzchar(current_model) && current_model %in% model_choices) {
        current_model
      } else if (length(model_choices)) {
        .pick_preferred_choice(model_choices, preferred_model)
      } else {
        ""
      }
      setup_state$desired_model <- selected_model

      current_type <- isolate(input$setup_type) %||% ""
      desired_type <- setup_state$desired_type %||% current_type
      preferred_type <- if (nzchar(selected_model) && tolower(selected_model) == tolower(.DEFAULT_MODEL_NAME)) .DEFAULT_MODEL_TYPE else NULL
      type_choices <- .model_type_choices(catalog, svc, selected_model, include = c(current_type, desired_type))
      selected_type <- if (nzchar(desired_type) && desired_type %in% type_choices) {
        desired_type
      } else if (nzchar(current_type) && current_type %in% type_choices) {
        current_type
      } else if (length(type_choices)) {
        .pick_preferred_choice(type_choices, preferred_type)
      } else {
        ""
      }
      setup_state$desired_type <- selected_type

      if (!isTRUE(setup_state$loading)) {
        if (!nzchar(setup_state$desired_model)) setup_state$desired_model <- NULL
        if (!nzchar(setup_state$desired_type)) setup_state$desired_type <- NULL
      }
  },
  priority = 5
)

  observeEvent(input$setup_openrouter_online, {
    if (isTRUE(setup_state$loading)) return()
    svc <- tolower(input$setup_service %||% "")
    if (!identical(svc, "openrouter")) {
      return()
    }
    enabled <- isTRUE(input$setup_openrouter_online)
    setup_state$openrouter_online <- enabled
    current_model <- input$setup_model %||% ""
    base_model <- setup_state$openrouter_model_base %||% sub(":online$", "", current_model)
    if (!nzchar(base_model)) {
      base_model <- ""
    }
    setup_state$openrouter_model_base <- if (nzchar(base_model)) base_model else NULL
    new_model <- if (enabled && nzchar(base_model)) paste0(base_model, ":online") else if (enabled) current_model else base_model
    if (!identical(current_model, new_model)) {
      setup_state$desired_model <- new_model
      updateSelectizeInput(session, "setup_model", selected = new_model, server = TRUE)
    } else {
      setup_state$desired_model <- current_model
    }
  })

  observeEvent(input$setup_model, {
    if (isTRUE(setup_state$loading)) return()
    svc <- tolower(input$setup_service %||% "")
    if (!identical(svc, "openrouter")) {
      setup_state$openrouter_model_base <- NULL
      return()
    }
    value <- input$setup_model %||% ""
    base_value <- sub(":online$", "", value)
    setup_state$openrouter_model_base <- if (nzchar(base_value)) base_value else NULL
    if (isTRUE(setup_state$openrouter_online)) {
      desired_value <- if (nzchar(base_value)) paste0(base_value, ":online") else value
      if (!identical(value, desired_value)) {
        setup_state$desired_model <- desired_value
        updateSelectizeInput(session, "setup_model", selected = desired_value, server = TRUE)
      } else {
        setup_state$desired_model <- value
      }
    } else {
      setup_state$desired_model <- value
    }
  }, ignoreNULL = FALSE)

  observeEvent(models_state$catalog, {
    update_agent_service_choices()
  })

  observeEvent(list(input$agent_setup_service, models_state$catalog), {
    if (isTRUE(agent_state$loading)) return()
    update_agent_model_choices()
  })

  observeEvent(list(input$agent_setup_service, input$agent_setup_model, models_state$catalog), {
    if (isTRUE(agent_state$loading)) return()
    update_agent_type_choices()
  })

  observeEvent(models_state$favorites, {
    favs <- models_state$favorites
    prev_has <- isolate(models_state$favorites_present)
    has_favs <- nrow(favs) > 0
    models_state$favorites_present <- has_favs
    provider_ids <- .model_provider_ids()
    base_provider_choices <- setNames(provider_ids, .model_label(provider_ids))
    view_choices <- c(setNames(.MODEL_ALL_OPTION, .model_label(.MODEL_ALL_OPTION)), base_provider_choices)
    if (has_favs) {
      view_choices <- c(setNames("favorites", .model_label("favorites")), view_choices)
    }
    current_view <- input$models_view_provider
    if (has_favs && !prev_has) {
      current_view <- "favorites"
    } else if (!has_favs && prev_has && identical(tolower(current_view), "favorites")) {
      current_view <- .DEFAULT_MODEL_SERVICE
    } else if (is.null(current_view) || !current_view %in% view_choices) {
      current_view <- if (has_favs) "favorites" else .DEFAULT_MODEL_SERVICE
    }
    updateSelectInput(session, "models_view_provider", choices = view_choices, selected = current_view)
    current_update <- input$models_update_provider
    if (is.null(current_update) || !current_update %in% base_provider_choices) {
      current_update <- .DEFAULT_MODEL_SERVICE
    }
    updateSelectInput(session, "models_update_provider", choices = base_provider_choices, selected = current_update)
    update_setup_service_choices()
    update_agent_service_choices()

    if (has_favs && !prev_has) {
      setup_state$loading <- TRUE
      session$onFlushed(function() {
        setup_state$loading <- FALSE
        setup_state$desired_model <- NULL
        setup_state$desired_type <- NULL
      }, once = TRUE)
      setup_state$desired_service <- "favorites"
      setup_state$desired_model <- favs$model[1]
      model_types <- favs$type[favs$model == favs$model[1]]
      model_types <- model_types[nzchar(model_types)]
      setup_state$desired_type <- if (length(model_types)) model_types[[1]] else ""
      setup_state$skip_service_event <- TRUE
      update_setup_service_choices()
      update_setup_model_choices()
      update_setup_type_choices()
      if (identical(input$agent_setup_mode, "custom")) {
        agent_state$loading <- TRUE
        agent_state$custom_desired_service <- "favorites"
        agent_state$custom_desired_model <- favs$model[1]
        agent_types <- model_types
        if (!length(agent_types)) {
          actual_service <- favs$service[1]
          agent_types <- .model_type_choices(models_state$catalog, actual_service, favs$model[1])
        }
        agent_state$custom_desired_type <- if (length(agent_types)) agent_types[[1]] else ""
        agent_state$loading <- FALSE
        update_agent_service_choices()
        update_agent_model_choices()
        update_agent_type_choices()
      }
    } else if (!has_favs && prev_has) {
      if (identical(tolower(input$setup_service %||% ""), "favorites")) {
        setup_state$loading <- TRUE
        session$onFlushed(function() {
          setup_state$loading <- FALSE
          setup_state$desired_model <- NULL
          setup_state$desired_type <- NULL
        }, once = TRUE)
        setup_state$desired_service <- .DEFAULT_MODEL_SERVICE
        setup_state$desired_model <- .DEFAULT_MODEL_NAME
        setup_state$desired_type <- .DEFAULT_MODEL_TYPE
        setup_state$skip_service_event <- TRUE
        update_setup_service_choices()
        update_setup_model_choices()
        update_setup_type_choices()
      }
      if (identical(input$agent_setup_mode, "custom") && identical(tolower(input$agent_setup_service %||% ""), "favorites")) {
        agent_state$loading <- TRUE
        agent_state$custom_desired_service <- .DEFAULT_MODEL_SERVICE
        agent_state$custom_desired_model <- .DEFAULT_MODEL_NAME
        agent_state$custom_desired_type <- .DEFAULT_MODEL_TYPE
        agent_state$loading <- FALSE
        update_agent_service_choices()
        update_agent_model_choices()
        update_agent_type_choices()
      }
    } else if (has_favs) {
      if (identical(tolower(input$setup_service %||% ""), "favorites")) {
        current_model <- input$setup_model %||% ""
        if (!current_model %in% favs$model) {
          setup_state$loading <- TRUE
          setup_state$desired_model <- favs$model[1]
          model_types <- favs$type[favs$model == favs$model[1]]
          model_types <- model_types[nzchar(model_types)]
          setup_state$desired_type <- if (length(model_types)) model_types[[1]] else ""
          setup_state$loading <- FALSE
          update_setup_model_choices()
          update_setup_type_choices()
        }
      }
      if (identical(input$agent_setup_mode, "custom") && identical(tolower(input$agent_setup_service %||% ""), "favorites")) {
        current_model <- input$agent_setup_model %||% ""
        if (!current_model %in% favs$model) {
          agent_state$loading <- TRUE
          agent_state$custom_desired_model <- favs$model[1]
          model_types <- favs$type[favs$model == favs$model[1]]
          model_types <- model_types[nzchar(model_types)]
          agent_state$custom_desired_type <- if (length(model_types)) model_types[[1]] else ""
          agent_state$loading <- FALSE
          update_agent_model_choices()
          update_agent_type_choices()
        }
      }
    }
  })


  observeEvent(input$agent_setup_service, {
    if (isTRUE(agent_state$loading)) return()
    if (!identical(input$agent_setup_mode, "custom")) return()
    svc <- input$agent_setup_service %||% ""
    catalog <- models_state$catalog
    if (identical(tolower(svc), "favorites")) {
      favs <- models_state$favorites
      if (nrow(favs) == 0) {
        agent_state$custom_desired_model <- ""
        agent_state$custom_desired_type <- ""
      } else {
        new_model <- favs$model[1]
        types_for_model <- favs$type[favs$model == new_model]
        types_for_model <- types_for_model[nzchar(types_for_model)]
        if (!length(types_for_model)) {
          actual_service <- favs$service[favs$model == new_model][1]
          type_choices <- .model_type_choices(catalog, actual_service, new_model)
          types_for_model <- type_choices
        }
        new_type <- if (length(types_for_model)) types_for_model[[1]] else ""
        agent_state$custom_desired_model <- new_model
        agent_state$custom_desired_type <- new_type
      }
      return()
    }
    preferred_model <- if (tolower(svc) == .DEFAULT_MODEL_SERVICE) .DEFAULT_MODEL_NAME else NULL
    model_choices <- .model_model_choices(catalog, svc)
    new_model <- if (length(model_choices)) .pick_preferred_choice(model_choices, preferred_model) else ""
    preferred_type <- if (tolower(new_model) == tolower(.DEFAULT_MODEL_NAME)) .DEFAULT_MODEL_TYPE else NULL
    type_choices <- .model_type_choices(catalog, svc, new_model)
    new_type <- if (length(type_choices)) .pick_preferred_choice(type_choices, preferred_type) else ""
    agent_state$custom_desired_model <- new_model
    agent_state$custom_desired_type <- new_type
  }, priority = 5)

  observeEvent(TRUE,
    {
      setup_state$loading <- TRUE
      on.exit(
        {
          setup_state$loading <- FALSE
        },
        add = TRUE
      )
      favs <- models_state$favorites
      has_favs <- nrow(favs) > 0
      default_service <- if (has_favs) "favorites" else .DEFAULT_MODEL_SERVICE
      setup_state$desired_service <- default_service
      if (has_favs) {
        new_model <- favs$model[1]
        model_types <- favs$type[favs$model == new_model]
        model_types <- model_types[nzchar(model_types)]
        new_type <- if (length(model_types)) model_types[[1]] else ""
        setup_state$desired_model <- new_model
        setup_state$desired_type <- if (nzchar(new_type)) new_type else .DEFAULT_MODEL_TYPE
      } else {
        setup_state$desired_model <- .DEFAULT_MODEL_NAME
        setup_state$desired_type <- .DEFAULT_MODEL_TYPE
      }
      update_setup_service_choices()
      update_setup_model_choices()
      update_setup_type_choices()
    },
    once = TRUE
  )

  observeEvent(TRUE,
    {
      agent_state$loading <- TRUE
      on.exit(
        {
          agent_state$loading <- FALSE
        },
        add = TRUE
      )
      favs <- models_state$favorites
      has_favs <- nrow(favs) > 0
      default_service <- if (has_favs) "favorites" else .DEFAULT_MODEL_SERVICE
      agent_state$custom_desired_service <- default_service
      if (has_favs) {
        new_model <- favs$model[1]
        model_types <- favs$type[favs$model == new_model]
        model_types <- model_types[nzchar(model_types)]
        new_type <- if (length(model_types)) model_types[[1]] else ""
        agent_state$custom_desired_model <- new_model
        agent_state$custom_desired_type <- if (nzchar(new_type)) new_type else .DEFAULT_MODEL_TYPE
      } else {
        agent_state$custom_desired_model <- .DEFAULT_MODEL_NAME
        agent_state$custom_desired_type <- .DEFAULT_MODEL_TYPE
      }
      update_agent_service_choices()
      update_agent_model_choices()
      update_agent_type_choices()
    },
    once = TRUE
  )

  observeEvent(input$setup_thinking_enabled, {
    setup_state$thinking_enabled <- isTRUE(input$setup_thinking_enabled)
  })

  observeEvent(input$setup_thinking_level, {
    setup_state$thinking_level <- input$setup_thinking_level %||% .DEFAULT_THINKING_LEVEL
  })

  observeEvent(TRUE,
    {
      updateTextInput(session, "models_directory", value = models_state$directory)
      refresh_provider_selectors()
      custom_count <- length(custom_provider_choices())
      if (custom_count > 0L) {
        models_state$custom_status <- sprintf("Custom providers registered: %d", custom_count)
      } else {
        models_state$custom_status <- "No custom providers registered."
      }
    },
    once = TRUE
  )

  observeEvent(models_state$catalog, {
    normalized_favs <- .normalize_favorites(models_state$favorites, models_state$catalog)
    if (!identical(normalized_favs, models_state$favorites)) {
      models_state$favorites <- normalized_favs
      .save_favorites(normalized_favs, models_state$directory)
    }
    models_state$favorites_present <- nrow(models_state$favorites) > 0
    refresh_provider_selectors()
  })

  observeEvent(input$models_directory, {
    dir <- trimws(input$models_directory)
    if (!nzchar(dir) || identical(dir, models_state$directory)) {
      return()
    }
    new_catalog <- .load_models_catalog(dir)
    new_favorites <- .normalize_favorites(.load_favorites(dir), new_catalog)
    models_state$directory <- dir
    models_state$catalog <- new_catalog
    models_state$favorites <- new_favorites
    models_state$favorites_present <- nrow(new_favorites) > 0
    .save_favorites(new_favorites, dir)
    models_state$status <- paste0("Loaded models from ", dir)
  })

  observeEvent(input$models_refresh, {
    dir <- models_state$directory
    new_catalog <- .load_models_catalog(dir)
    new_favorites <- .normalize_favorites(.load_favorites(dir), new_catalog)
    models_state$catalog <- new_catalog
    models_state$favorites <- new_favorites
    models_state$favorites_present <- nrow(new_favorites) > 0
    .save_favorites(new_favorites, dir)
    models_state$status <- paste0("Reloaded models from ", dir)
    showNotification("Models reloaded from disk.", type = "message")
  })

  observeEvent(input$models_update_all, {
    dir <- trimws(input$models_directory)
    if (!nzchar(dir)) {
      showNotification("Provide a directory for model CSV files.", type = "warning")
      return()
    }
    withProgress(message = "Updating all model providers...", value = 0, {
      tryCatch(
        {
          gen_update_models(provider = NULL, directory = dir, verbose = TRUE)
          incProgress(1)
          new_catalog <- .load_models_catalog(dir)
          new_favorites <- .normalize_favorites(.load_favorites(dir), new_catalog)
          models_state$directory <- dir
          models_state$catalog <- new_catalog
          models_state$favorites <- new_favorites
          models_state$favorites_present <- nrow(new_favorites) > 0
          .save_favorites(new_favorites, dir)
          models_state$status <- paste0("Updated all providers at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
          updateTextInput(session, "models_directory", value = dir)
          showNotification("All model providers updated.", type = "message")
        },
        error = function(e) {
          models_state$status <- paste("Update failed:", conditionMessage(e))
          showNotification(conditionMessage(e), type = "error")
        }
      )
    })
  })

  observeEvent(input$models_update_selected, {
    dir <- trimws(input$models_directory)
    provider <- input$models_update_provider %||% ""
    if (!nzchar(dir)) {
      showNotification("Provide a directory for model CSV files.", type = "warning")
      return()
    }
    if (!nzchar(provider)) {
      showNotification("Select a provider to update.", type = "warning")
      return()
    }
    withProgress(message = paste0("Updating ", .model_label(provider), " models..."), value = 0, {
      tryCatch(
        {
          gen_update_models(provider = provider, directory = dir, verbose = TRUE)
          incProgress(1)
          new_catalog <- .load_models_catalog(dir)
          new_favorites <- .normalize_favorites(.load_favorites(dir), new_catalog)
          models_state$directory <- dir
          models_state$catalog <- new_catalog
          models_state$favorites <- new_favorites
          models_state$favorites_present <- nrow(new_favorites) > 0
          .save_favorites(new_favorites, dir)
          models_state$status <- paste0("Updated ", .model_label(provider), " at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
          updateTextInput(session, "models_directory", value = dir)
          showNotification(paste(.model_label(provider), "models updated."), type = "message")
        },
        error = function(e) {
          models_state$status <- paste("Update failed:", conditionMessage(e))
          showNotification(conditionMessage(e), type = "error")
        }
      )
    })
  })

  observeEvent(input$models_custom_add, {
    show_custom_provider_modal("add", cfg = list(default_model = "local-model"))
  })

  observeEvent(input$models_custom_edit, {
    provider_id <- trimws(as.character(input$models_custom_provider %||% "")[1])
    if (!nzchar(provider_id)) {
      showNotification("Select a custom provider to edit.", type = "warning")
      return()
    }
    provider_cfg <- .genflow_get_custom_provider(provider_id)
    if (is.null(provider_cfg)) {
      showNotification("Selected custom provider was not found.", type = "error")
      refresh_provider_selectors()
      return()
    }
    show_custom_provider_modal("edit", cfg = provider_cfg)
  })

  observeEvent(input$models_custom_save, {
    mode <- as.character(custom_provider_state$mode %||% "add")[1]
    edit_id <- trimws(as.character(custom_provider_state$edit_id %||% "")[1])

    provider_id <- trimws(as.character(input$models_custom_id %||% "")[1])
    provider_label <- trimws(as.character(input$models_custom_label %||% "")[1])
    base_urls <- parse_multi_values(input$models_custom_base_urls)
    api_key_env <- trimws(as.character(input$models_custom_api_key_env %||% "")[1])
    model_env <- trimws(as.character(input$models_custom_model_env %||% "")[1])
    default_model <- trimws(as.character(input$models_custom_default_model %||% "local-model")[1])
    chat_paths <- parse_multi_values(input$models_custom_chat_paths)
    model_paths <- parse_multi_values(input$models_custom_model_paths)
    auth_header <- as.character(input$models_custom_auth_header %||% "Authorization")[1]
    auth_prefix <- as.character(input$models_custom_auth_prefix %||% "Bearer")[1]
    extra_headers <- tryCatch(
      parse_header_lines(input$models_custom_extra_headers),
      error = function(e) {
        showNotification(conditionMessage(e), type = "error")
        NULL
      }
    )
    if (is.null(extra_headers)) {
      return()
    }

    reasoning_field <- trimws(as.character(input$models_custom_reasoning_field %||% "")[1])
    plugins_field <- trimws(as.character(input$models_custom_plugins_field %||% "")[1])
    max_tokens_raw <- trimws(as.character(input$models_custom_max_tokens %||% "")[1])
    max_tokens <- NULL
    if (nzchar(max_tokens_raw)) {
      max_tokens_num <- suppressWarnings(as.numeric(max_tokens_raw)[1])
      if (!is.finite(max_tokens_num) || max_tokens_num <= 0) {
        showNotification("`max_tokens` must be a positive number.", type = "error")
        return()
      }
      max_tokens <- as.integer(max_tokens_num)
    }

    if (!nzchar(default_model)) {
      default_model <- "local-model"
    }
    if (!length(base_urls)) {
      showNotification("Provide at least one base URL.", type = "warning")
      return()
    }

    rename_provider <- identical(mode, "edit") && nzchar(edit_id) && !identical(provider_id, edit_id)
    if (identical(mode, "add") && !is.null(.genflow_get_custom_provider(provider_id))) {
      showNotification(sprintf("Custom provider '%s' already exists.", provider_id), type = "error")
      return()
    }
    if (isTRUE(rename_provider) && !is.null(.genflow_get_custom_provider(provider_id))) {
      showNotification(sprintf("Custom provider '%s' already exists.", provider_id), type = "error")
      return()
    }

    result <- tryCatch(
      {
        set_provider_openai_compat(
          id = provider_id,
          label = if (nzchar(provider_label)) provider_label else NULL,
          base_urls = base_urls,
          api_key_env = if (nzchar(api_key_env)) api_key_env else NULL,
          model_env = if (nzchar(model_env)) model_env else NULL,
          default_model = default_model,
          chat_paths = chat_paths,
          model_paths = model_paths,
          auth_header = auth_header,
          auth_prefix = auth_prefix,
          extra_headers = extra_headers,
          api_key_required = isTRUE(input$models_custom_api_key_required),
          supports_tools = isTRUE(input$models_custom_supports_tools),
          supports_vision = isTRUE(input$models_custom_supports_vision),
          supports_reasoning = isTRUE(input$models_custom_supports_reasoning),
          supports_plugins = isTRUE(input$models_custom_supports_plugins),
          reasoning_field = if (nzchar(reasoning_field)) reasoning_field else NULL,
          plugins_field = if (nzchar(plugins_field)) plugins_field else NULL,
          max_tokens = max_tokens,
          overwrite = TRUE
        )
        if (isTRUE(rename_provider)) {
          rm_provider(edit_id, missing_ok = TRUE)
        }
        list(ok = TRUE, error = NULL)
      },
      error = function(e) {
        list(ok = FALSE, error = conditionMessage(e))
      }
    )

    if (!isTRUE(result$ok)) {
      showNotification(result$error %||% "Failed to save custom provider.", type = "error")
      return()
    }

    removeModal()
    refresh_provider_selectors(preferred_custom = provider_id)
    custom_count <- length(custom_provider_choices())
    status_msg <- if (identical(mode, "add")) {
      sprintf("Custom provider '%s' created.", provider_id)
    } else {
      sprintf("Custom provider '%s' updated.", provider_id)
    }
    models_state$custom_status <- paste0(status_msg, "\nCustom providers registered: ", custom_count)
    models_state$status <- status_msg
    showNotification(status_msg, type = "message")
  })

  observeEvent(input$models_custom_remove, {
    provider_id <- trimws(as.character(input$models_custom_provider %||% "")[1])
    if (!nzchar(provider_id)) {
      showNotification("Select a custom provider to remove.", type = "warning")
      return()
    }

    provider_cfg <- .genflow_get_custom_provider(provider_id)
    provider_label <- if (is.null(provider_cfg)) provider_id else (provider_cfg$label %||% provider_id)
    showModal(modalDialog(
      title = "Remove custom provider",
      paste0("Remove provider '", provider_label, "' (", provider_id, ")?"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_models_custom_remove", "Remove", class = "btn-danger")
      )
    ))
  })

  observeEvent(input$confirm_models_custom_remove, {
    provider_id <- trimws(as.character(isolate(input$models_custom_provider) %||% "")[1])
    removeModal()
    if (!nzchar(provider_id)) {
      return()
    }

    removed <- tryCatch(
      {
        rm_provider(provider_id, missing_ok = TRUE)
      },
      error = function(e) {
        showNotification(conditionMessage(e), type = "error")
        FALSE
      }
    )
    if (!isTRUE(removed)) {
      return()
    }

    refresh_provider_selectors()
    custom_count <- length(custom_provider_choices())
    status_msg <- sprintf("Custom provider '%s' removed.", provider_id)
    models_state$custom_status <- paste0(status_msg, "\nCustom providers registered: ", custom_count)
    models_state$status <- status_msg
    showNotification(status_msg, type = "message")
  })

  observeEvent(input$models_custom_test, {
    provider_id <- trimws(as.character(input$models_custom_provider %||% "")[1])
    if (!nzchar(provider_id)) {
      showNotification("Select a custom provider to test.", type = "warning")
      return()
    }
    dir <- trimws(input$models_directory %||% "")
    if (!nzchar(dir)) {
      dir <- current_models_dir()
    }

    withProgress(message = paste0("Testing custom provider ", provider_id, "..."), value = 0, {
      test_result <- tryCatch(
        test_provider(provider_id, timeout = 20, max_models = 12),
        error = function(e) {
          list(
            provider_id = provider_id,
            provider_label = provider_id,
            status_api = "ERROR",
            status_msg = conditionMessage(e),
            endpoint = "",
            model_count = 0L,
            models = character(),
            attempted_endpoints = character()
          )
        }
      )
      incProgress(0.5)

      models_state$custom_status <- format_custom_provider_test(test_result)
      if (identical(as.character(test_result$status_api %||% "ERROR")[1], "SUCCESS")) {
        incProgress(0.2, detail = paste0("Updating ", provider_id, " models..."))
        update_ok <- tryCatch(
          {
            gen_update_models(provider = provider_id, directory = dir, verbose = TRUE)
            new_catalog <- .load_models_catalog(dir)
            new_favorites <- .normalize_favorites(.load_favorites(dir), new_catalog)
            models_state$directory <- dir
            models_state$catalog <- new_catalog
            models_state$favorites <- new_favorites
            models_state$favorites_present <- nrow(new_favorites) > 0
            .save_favorites(new_favorites, dir)
            updateTextInput(session, "models_directory", value = dir)
            models_state$status <- paste0("Tested and updated ", .model_label(provider_id), " at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
            TRUE
          },
          error = function(e) {
            models_state$status <- paste("Update after test failed:", conditionMessage(e))
            models_state$custom_status <- paste0(
              models_state$custom_status,
              "\nUpdate error: ",
              conditionMessage(e)
            )
            showNotification(conditionMessage(e), type = "error")
            FALSE
          }
        )
        incProgress(0.3)
        if (isTRUE(update_ok)) {
          models_state$custom_status <- paste0(
            models_state$custom_status,
            "\nUpdate: OK"
          )
          showNotification(sprintf("Provider '%s' test passed and models were updated.", provider_id), type = "message")
        }
      } else {
        incProgress(0.5)
        showNotification(as.character(test_result$status_msg %||% "Provider test failed.")[1], type = "error")
      }
    })
  })

  output$models_status <- renderText({
    models_state$status
  })

  output$models_custom_status <- renderText({
    models_state$custom_status
  })

  output$models_summary <- renderText({
    catalog <- models_state$catalog
    if (nrow(catalog) == 0) {
      return("No models cached yet. Use the controls on the left to fetch provider model lists.")
    }
    provider <- input$models_view_provider %||% .DEFAULT_MODEL_SERVICE
    provider_lower <- tolower(provider)
    if (identical(provider_lower, "favorites")) {
      favs <- models_state$favorites
      fav_count <- nrow(favs)
      if (!fav_count) {
        return("No favorite models selected yet. Use the stars beside each model to add favorites.")
      }
      services_count <- sort(table(.model_label(favs$service)), decreasing = TRUE)
      top_service <- names(services_count)[1]
      return(sprintf("Favorites saved: %d model(s). Top provider: %s.", fav_count, top_service))
    }
    model_values <- catalog$model
    model_values[is.na(model_values)] <- ""
    total <- sum(nzchar(model_values))
    if (identical(provider_lower, .MODEL_ALL_OPTION)) {
      services <- tolower(catalog$service)
      services[is.na(services)] <- ""
      providers <- unique(services[nzchar(services)])
      provider_count <- length(providers)
      return(sprintf(
        "%s: %d cached model(s) across %d provider(s).",
        .model_label(provider_lower),
        total,
        provider_count
      ))
    }
    provider_mask <- tolower(catalog$service) == provider_lower
    provider_models <- model_values[provider_mask]
    provider_count <- if (length(provider_models)) sum(nzchar(provider_models)) else 0
    sprintf(
      "%s models available: %d total cached (%d from %s).",
      .model_label(provider_lower),
      total,
      provider_count,
      .model_label(provider_lower)
    )
  })

  output$models_table <- renderDT({
    provider <- input$models_view_provider %||% .DEFAULT_MODEL_SERVICE
    catalog_all <- models_state$catalog
    favorites <- models_state$favorites
    provider_lower <- tolower(provider)
    if (identical(provider_lower, "favorites")) {
      if (nrow(favorites) == 0) {
        return(datatable(
          data.frame(Message = "No favorite models selected yet."),
          rownames = FALSE,
          options = list(dom = "t", ordering = FALSE)
        ))
      }
      merged <- merge(favorites, catalog_all, by = c("service", "model"), all.x = TRUE, suffixes = c("", ".catalog"))
      if (!"type" %in% names(merged)) merged$type <- ""
      merged$type <- ifelse(nzchar(merged$type), merged$type, merged$type.catalog %||% "")
      if (!"pricing" %in% names(merged)) merged$pricing <- ""
      merged$pricing <- if ("pricing.catalog" %in% names(merged)) ifelse(nzchar(merged$pricing), merged$pricing, merged$pricing.catalog %||% "") else merged$pricing %||% ""
      if (!"description" %in% names(merged)) merged$description <- ""
      merged$description <- if ("description.catalog" %in% names(merged)) ifelse(nzchar(merged$description), merged$description, merged$description.catalog %||% "") else merged$description %||% ""
      catalog <- merged[, c("service", "model", "type", "pricing", "description"), drop = FALSE]
    } else if (identical(provider_lower, .MODEL_ALL_OPTION)) {
      catalog <- catalog_all
    } else {
      catalog <- catalog_all[tolower(catalog_all$service) == provider_lower, , drop = FALSE]
    }
    if (nrow(catalog) == 0) {
      return(datatable(
        data.frame(Message = "No models found for this provider yet."),
        rownames = FALSE,
        options = list(dom = "t", ordering = FALSE)
      ))
    }

    fav_keys <- paste(favorites$service, favorites$model, sep = "||")
    row_service <- ifelse(is.na(catalog$service), "", tolower(catalog$service))
    row_keys <- paste(row_service, catalog$model, sep = "||")
    is_fav <- row_keys %in% fav_keys
    row_model <- ifelse(is.na(catalog$model), "", catalog$model)
    row_type <- if ("type" %in% names(catalog)) ifelse(is.na(catalog$type), "", catalog$type) else rep("", nrow(catalog))
    star_html <- sprintf(
      "<span class='favorite-star %s' data-service='%s' data-model='%s' data-type='%s' title='%s'>&#9733;</span>",
      ifelse(is_fav, "is-fav", ""),
      htmltools::htmlEscape(row_service),
      htmltools::htmlEscape(row_model),
      htmltools::htmlEscape(row_type),
      ifelse(is_fav, "Remove from favorites", "Add to favorites")
    )

    pricing_raw <- if ("pricing" %in% names(catalog)) catalog$pricing else rep("", nrow(catalog))
    description_raw <- if ("description" %in% names(catalog)) catalog$description else rep("", nrow(catalog))
    format_scroll_cell <- function(values) {
      if (!length(values)) {
        return(character())
      }
      vapply(values, function(val) {
        if (is.null(val) || !length(val)) {
          return("")
        }
        scalar <- val[[1]]
        if (is.null(scalar) || (length(scalar) && is.na(scalar))) {
          return("")
        }
        text <- as.character(scalar)
        if (!nzchar(text)) {
          ""
        } else {
          sprintf("<div class='gf-cell-scroll'>%s</div>", htmltools::htmlEscape(text))
        }
      }, character(1), USE.NAMES = FALSE)
    }
    display <- data.frame(
      Favorite = star_html,
      FavoriteRank = as.integer(is_fav),
      Provider = .model_label(row_service),
      Model = row_model,
      Type = row_type,
      Pricing = format_scroll_cell(pricing_raw),
      Description = format_scroll_cell(description_raw),
      stringsAsFactors = FALSE
    )

    datatable(
      display,
      escape = FALSE,
      selection = "none",
      rownames = FALSE,
      options = list(
        pageLength = 10,
        autoWidth = TRUE,
        order = list(list(0, "desc"), list(3, "asc")),
        columnDefs = list(
          list(visible = FALSE, searchable = FALSE, targets = 1),
          list(orderData = 1, orderSequence = c("desc", "asc"), targets = 0)
        ),
        stateSave = TRUE,
        stateDuration = 0
      ),
      callback = DT::JS(
        "table.on('click', 'span.favorite-star', function() {",
        "  var el = $(this);",
        "  var service = el.data('service');",
        "  var model = el.data('model');",
        "  var type = el.data('type');",
        "  Shiny.setInputValue('models_toggle_favorite', {service: service, model: model, type: type, nonce: Math.random()}, {priority: 'event'});",
        "});"
      )
    )
  })

  observeEvent(input$models_toggle_favorite, {
    info <- input$models_toggle_favorite
    svc <- tolower(info$service %||% "")
    model <- info$model %||% ""
    type <- info$type %||% ""
    if (!nzchar(svc) || !nzchar(model)) {
      return()
    }
    current_favs <- models_state$favorites
    key <- paste(svc, model, sep = "||")
    fav_keys <- paste(current_favs$service, current_favs$model, sep = "||")
    catalog <- models_state$catalog
    if (key %in% fav_keys) {
      current_favs <- current_favs[fav_keys != key, , drop = FALSE]
      action <- "removed"
    } else {
      if (!nzchar(type)) {
        match_idx <- which(tolower(catalog$service) == svc & catalog$model == model)[1]
        if (!is.na(match_idx)) {
          type <- catalog$type[match_idx]
        }
      }
      type <- type %||% ""
      new_entry <- data.frame(
        service = svc,
        model = model,
        type = type,
        stringsAsFactors = FALSE
      )
      current_favs <- unique(rbind(current_favs, new_entry))
      action <- "added"
    }
    current_favs <- .normalize_favorites(current_favs, catalog)
    models_state$favorites <- current_favs
    models_state$favorites_present <- nrow(current_favs) > 0
    .save_favorites(current_favs, models_state$directory)
    status_msg <- if (identical(action, "added")) {
      sprintf("Added %s / %s to favorites.", .model_label(svc), model)
    } else {
      sprintf("Removed %s / %s from favorites.", .model_label(svc), model)
    }
    models_state$status <- status_msg
  })

  refresh_setup_list <- function(selected = NULL) {
    saved_choices <- sort(.load_setup_names())
    draft_name <- isolate(setup_state$draft_name)
    draft_summary <- isolate(setup_state$draft_summary)
    draft_summary <- draft_summary %||% ""
    combined_choices <- saved_choices
    unsaved_names <- character()
    if (!is.null(draft_name) && nzchar(draft_name) && !(draft_name %in% saved_choices)) {
      combined_choices <- sort(c(saved_choices, draft_name))
      unsaved_names <- draft_name
    } else if (!is.null(draft_name) && draft_name %in% saved_choices) {
      setup_state$draft_name <- NULL
      setup_state$draft_summary <- NULL
      setup_state$draft_source <- NULL
    }
    setup_state$list <- combined_choices
    summary_lookup <- setNames(character(0), character(0))
    if (!length(saved_choices)) {
      existing_keys <- ls(envir = setup_summary_cache, all.names = TRUE)
      if (length(existing_keys)) {
        rm(list = existing_keys, envir = setup_summary_cache)
      }
    } else {
      existing_keys <- ls(envir = setup_summary_cache, all.names = TRUE)
      to_drop <- setdiff(existing_keys, saved_choices)
      if (length(to_drop)) {
        rm(list = to_drop, envir = setup_summary_cache)
      }
      summary_lookup <- setNames(character(length(saved_choices)), saved_choices)
      for (name in saved_choices) {
        if (exists(name, envir = setup_summary_cache, inherits = FALSE)) {
          summary_lookup[[name]] <- setup_summary_cache[[name]]
        } else {
          details <- suppressWarnings(tryCatch(get_setup(name, assign = FALSE), error = function(e) NULL))
          summary <- .setup_summary_text(details)
          summary_lookup[[name]] <- summary
          if (!is.null(details)) {
            setup_summary_cache[[name]] <- summary
          }
        }
      }
    }
    if (length(unsaved_names)) {
      unsaved_summary <- draft_summary
      if (!nzchar(unsaved_summary)) {
        source_label <- isolate(setup_state$draft_source)
        source_label <- source_label %||% ""
        unsaved_summary <- if (nzchar(source_label)) {
          sprintf("Unsaved copy of '%s'. Save to keep it.", source_label)
        } else {
          "Unsaved copy. Save to keep it."
        }
      }
      summary_lookup <- c(summary_lookup, setNames(unsaved_summary, draft_name))
    }
    output$setup_list_ui <- renderUI({
      if (!length(combined_choices)) {
        return(div(class = "gf-empty", "No setups saved yet."))
      }
      active <- isolate(setup_state$selected)
      if (is.null(active) && length(unsaved_names)) {
        active <- isolate(setup_state$draft_name)
      }
      tagList(lapply(combined_choices, function(name) {
        is_unsaved <- length(unsaved_names) && name %in% unsaved_names
        summary_text <- summary_lookup[[name]] %||% ""
        item_classes <- c("gf-entity-item")
        if (!is.null(active) && identical(active, name)) {
          item_classes <- c(item_classes, "active")
        }
        if (is_unsaved) {
          item_classes <- c(item_classes, "gf-entity-item-unsaved")
        }
        name_children <- list(name)
        if (is_unsaved) {
          name_children <- c(name_children, list(span(class = "gf-pill", "Unsaved copy")))
        }
        name_node <- do.call(span, c(list(class = "gf-entity-name"), name_children))
        summary_node <- if (nzchar(summary_text)) div(class = "gf-entity-summary", summary_text) else NULL
        item_args <- c(
          list(class = paste(item_classes, collapse = " ")),
          Filter(Negate(is.null), list(name_node, summary_node))
        )
        if (!is_unsaved) {
          item_args$onclick <- sprintf(
            "Shiny.setInputValue('%s', {name: '%s', nonce: Math.random()}, {priority: 'event'});",
            "setup_select_trigger", name
          )
        }
        do.call(div, item_args)
      }))
    })
    labelled_setups <- .label_choices(saved_choices)
    setup_choices <- if (length(labelled_setups)) c("", labelled_setups) else ""
    updateSelectInput(session, "agent_setup_select", choices = setup_choices)
    if (!is.null(selected)) {
      setup_state$selected <- selected
    }
  }

  refresh_content_list <- function(selected = NULL) {
    saved_choices <- sort(.load_content_names())
    draft_name <- isolate(content_state$draft_name)
    draft_summary <- isolate(content_state$draft_summary)
    draft_summary <- draft_summary %||% ""
    combined_choices <- saved_choices
    unsaved_names <- character()
    if (!is.null(draft_name) && nzchar(draft_name) && !(draft_name %in% saved_choices)) {
      combined_choices <- sort(c(saved_choices, draft_name))
      unsaved_names <- draft_name
    } else if (!is.null(draft_name) && draft_name %in% saved_choices) {
      content_state$draft_name <- NULL
      content_state$draft_summary <- NULL
      content_state$draft_source <- NULL
    }
    content_state$list <- combined_choices
    preview_lookup <- setNames(character(0), character(0))
    if (!length(saved_choices)) {
      existing_keys <- ls(envir = content_preview_cache, all.names = TRUE)
      if (length(existing_keys)) {
        rm(list = existing_keys, envir = content_preview_cache)
      }
    } else {
      existing_keys <- ls(envir = content_preview_cache, all.names = TRUE)
      to_drop <- setdiff(existing_keys, saved_choices)
      if (length(to_drop)) {
        rm(list = to_drop, envir = content_preview_cache)
      }
      preview_lookup <- setNames(character(length(saved_choices)), saved_choices)
      for (name in saved_choices) {
        if (exists(name, envir = content_preview_cache, inherits = FALSE)) {
          preview_lookup[[name]] <- content_preview_cache[[name]]
        } else {
          details <- suppressWarnings(tryCatch(get_content(name, assign = FALSE), error = function(e) NULL))
          preview <- .content_summary_text(details)
          preview_lookup[[name]] <- preview
          if (!is.null(details)) {
            content_preview_cache[[name]] <- preview
          }
        }
      }
    }
    if (length(unsaved_names)) {
      unsaved_summary <- draft_summary
      if (!nzchar(unsaved_summary)) {
        source_label <- isolate(content_state$draft_source)
        source_label <- source_label %||% ""
        unsaved_summary <- if (nzchar(source_label)) {
          sprintf("Unsaved copy of '%s'. Save to keep it.", source_label)
        } else {
          "Unsaved copy. Save to keep it."
        }
      }
      preview_lookup <- c(preview_lookup, setNames(unsaved_summary, draft_name))
    }
    output$content_list_ui <- renderUI({
      if (!length(combined_choices)) {
        return(div(class = "gf-empty", "No content saved yet."))
      }
      active <- isolate(content_state$selected)
      if (is.null(active) && length(unsaved_names)) {
        active <- isolate(content_state$draft_name)
      }
      tagList(lapply(combined_choices, function(name) {
        is_unsaved <- length(unsaved_names) && name %in% unsaved_names
        preview <- preview_lookup[[name]] %||% ""
        item_classes <- c("gf-entity-item")
        if (!is.null(active) && identical(active, name)) {
          item_classes <- c(item_classes, "active")
        }
        if (is_unsaved) {
          item_classes <- c(item_classes, "gf-entity-item-unsaved")
        }
        name_children <- list(name)
        if (is_unsaved) {
          name_children <- c(name_children, list(span(class = "gf-pill", "Unsaved copy")))
        }
        name_node <- do.call(span, c(list(class = "gf-entity-name"), name_children))
        preview_node <- if (nzchar(preview)) div(class = "gf-entity-summary", preview) else NULL
        item_args <- c(
          list(class = paste(item_classes, collapse = " ")),
          Filter(Negate(is.null), list(name_node, preview_node))
        )
        if (!is_unsaved) {
          item_args$onclick <- sprintf(
            "Shiny.setInputValue('%s', {name: '%s', nonce: Math.random()}, {priority: 'event'});",
            "content_select_trigger", name
          )
        }
        do.call(div, item_args)
      }))
    })
    labelled_content <- .label_choices(saved_choices)
    content_choices <- if (length(labelled_content)) c("", labelled_content) else ""
    updateSelectInput(session, "agent_content_select", choices = content_choices)
    if (!is.null(selected)) {
      content_state$selected <- selected
    }
  }

  refresh_agent_list <- function(selected = NULL) {
    saved_choices <- sort(.load_agent_names())
    draft_name <- isolate(agent_state$draft_name)
    draft_summary <- isolate(agent_state$draft_summary)
    draft_summary <- draft_summary %||% ""
    combined_choices <- saved_choices
    unsaved_names <- character()
    if (!is.null(draft_name) && nzchar(draft_name) && !(draft_name %in% saved_choices)) {
      combined_choices <- sort(c(saved_choices, draft_name))
      unsaved_names <- draft_name
    } else if (!is.null(draft_name) && draft_name %in% saved_choices) {
      agent_state$draft_name <- NULL
      agent_state$draft_summary <- NULL
      agent_state$draft_source <- NULL
    }
    agent_state$list <- combined_choices
    summary_lookup <- setNames(character(0), character(0))
    if (!length(saved_choices)) {
      existing_keys <- ls(envir = agent_summary_cache, all.names = TRUE)
      if (length(existing_keys)) {
        rm(list = existing_keys, envir = agent_summary_cache)
      }
    } else {
      existing_keys <- ls(envir = agent_summary_cache, all.names = TRUE)
      to_drop <- setdiff(existing_keys, saved_choices)
      if (length(to_drop)) {
        rm(list = to_drop, envir = agent_summary_cache)
      }
      summary_lookup <- setNames(character(length(saved_choices)), saved_choices)
      for (name in saved_choices) {
        if (exists(name, envir = agent_summary_cache, inherits = FALSE)) {
          summary_lookup[[name]] <- agent_summary_cache[[name]]
        } else {
          details <- suppressWarnings(tryCatch(get_agent(name, assign = FALSE), error = function(e) NULL))
          summary <- .agent_summary_text(details)
          summary_lookup[[name]] <- summary
          if (!is.null(details)) {
            agent_summary_cache[[name]] <- summary
          }
        }
      }
    }
    if (length(unsaved_names)) {
      unsaved_summary <- draft_summary
      if (!nzchar(unsaved_summary)) {
        source_label <- isolate(agent_state$draft_source)
        source_label <- source_label %||% ""
        unsaved_summary <- if (nzchar(source_label)) {
          sprintf("Unsaved copy of '%s'. Save to keep it.", source_label)
        } else {
          "Unsaved copy. Save to keep it."
        }
      }
      summary_lookup <- c(summary_lookup, setNames(unsaved_summary, draft_name))
    }
    output$agent_list_ui <- renderUI({
      if (!length(combined_choices)) {
        return(div(class = "gf-empty", "No agents saved yet."))
      }
      active <- isolate(agent_state$selected)
      if (is.null(active) && length(unsaved_names)) {
        active <- isolate(agent_state$draft_name)
      }
      tagList(lapply(combined_choices, function(name) {
        is_unsaved <- length(unsaved_names) && name %in% unsaved_names
        summary_text <- summary_lookup[[name]] %||% ""
        item_classes <- c("gf-entity-item")
        if (!is.null(active) && identical(active, name)) {
          item_classes <- c(item_classes, "active")
        }
        if (is_unsaved) {
          item_classes <- c(item_classes, "gf-entity-item-unsaved")
        }
        name_children <- list(name)
        if (is_unsaved) {
          name_children <- c(name_children, list(span(class = "gf-pill", "Unsaved copy")))
        }
        name_node <- do.call(span, c(list(class = "gf-entity-name"), name_children))
        summary_node <- if (nzchar(summary_text)) div(class = "gf-entity-summary", summary_text) else NULL
        item_args <- c(
          list(class = paste(item_classes, collapse = " ")),
          Filter(Negate(is.null), list(name_node, summary_node))
        )
        if (!is_unsaved) {
          item_args$onclick <- sprintf(
            "Shiny.setInputValue('%s', {name: '%s', nonce: Math.random()}, {priority: 'event'});",
            "agent_select_trigger", name
          )
        }
        do.call(div, item_args)
      }))
    })
    if (!is.null(selected)) agent_state$selected <- selected
  }

  refresh_setup_list()
  refresh_content_list()
  refresh_agent_list()

  update_agent_setup_preview <- function(name) {
    if (is.null(name) || !nzchar(name)) {
      output$agent_setup_preview <- renderUI(NULL)
      return()
    }
    details <- tryCatch(get_setup(name, assign = FALSE), error = function(e) NULL)
    output$agent_setup_preview <- renderUI({
      if (is.null(details)) {
        div(class = "gf-preview", "Setup not found.")
      } else {
        preview <- paste(names(details), vapply(details, .format_preview, character(1)), sep = ": ", collapse = " | ")
        div(class = "gf-preview", preview)
      }
    })
  }

  update_agent_content_preview <- function(name) {
    if (is.null(name) || !nzchar(name)) {
      output$agent_content_preview <- renderUI(NULL)
      return()
    }
    details <- tryCatch(get_content(name, assign = FALSE), error = function(e) NULL)
    output$agent_content_preview <- renderUI({
      if (is.null(details)) {
        div(class = "gf-preview", "Content not found.")
      } else {
        preview <- paste(names(details), vapply(details, .format_preview, character(1)), sep = ": ", collapse = " | ")
        div(class = "gf-preview", preview)
      }
    })
  }

  reset_setup_form <- function() {
    setup_state$loading <- TRUE
    on.exit(
      {
        session$onFlushed(function() {
          setup_state$loading <- FALSE
        }, once = TRUE)
      },
      add = TRUE
    )
    updateTextInput(session, "setup_name", value = "")
    if (models_state$favorites_present && nrow(models_state$favorites) > 0) {
      favs <- models_state$favorites
      setup_state$desired_service <- "favorites"
      setup_state$desired_model <- favs$model[1]
      fav_types <- favs$type[favs$model == favs$model[1]]
      fav_types <- fav_types[nzchar(fav_types)]
      setup_state$desired_type <- if (length(fav_types)) fav_types[[1]] else .DEFAULT_MODEL_TYPE
    } else {
      setup_state$desired_service <- .DEFAULT_MODEL_SERVICE
      setup_state$desired_model <- .DEFAULT_MODEL_NAME
      setup_state$desired_type <- .DEFAULT_MODEL_TYPE
    }
    update_setup_service_choices()
    update_setup_model_choices()
    update_setup_type_choices()
    updateNumericInput(session, "setup_temp", value = NA)
    setup_state$extras <- list()
    setup_state$thinking_enabled <- FALSE
    setup_state$thinking_level <- .DEFAULT_THINKING_LEVEL
    setup_state$openrouter_online <- FALSE
    setup_state$openrouter_model_base <- NULL
    updateCheckboxInput(session, "setup_thinking_enabled", value = FALSE)
    updateSelectInput(session, "setup_thinking_level", selected = .DEFAULT_THINKING_LEVEL)
    updateCheckboxInput(session, "setup_openrouter_online", value = FALSE)
    output$setup_extra_fields <- renderUI(.render_kv_fields(setup_state$extras, "setup_extra", input, session))
  }

  load_setup <- function(name) {
    if (is.null(name) || !nzchar(name)) {
      return()
    }
    setup_state$draft_name <- NULL
    setup_state$draft_summary <- NULL
    setup_state$draft_source <- NULL
    setup_state$loading <- TRUE
    on.exit(
      {
        session$onFlushed(function() {
          setup_state$loading <- FALSE
          setup_state$skip_service_event <- FALSE
        }, once = TRUE)
      },
      add = TRUE
    )
    setup <- tryCatch(get_setup(name, assign = FALSE), error = function(e) {
      showNotification(conditionMessage(e), type = "error")
      NULL
    })
    if (is.null(setup)) {
      return()
    }
    setup_state$selected <- name
    setup_state$original <- setup
    setup_summary_cache[[name]] <- .setup_summary_text(setup)
    updateTextInput(session, "setup_name", value = setup$sname %||% name)
    service_value <- setup$service %||% ""
    model_value <- setup$model %||% ""
    type_value <- setup$type %||% ""
    online_enabled <- FALSE
    base_model_value <- model_value %||% ""
    if (identical(tolower(service_value), "openrouter") && nzchar(model_value)) {
      if (grepl(":online$", model_value, fixed = TRUE)) {
        online_enabled <- TRUE
        base_model_value <- sub(":online$", "", model_value)
      }
    }
    if (!nzchar(base_model_value)) {
      base_model_value <- ""
    }
    setup_state$desired_service <- service_value
    setup_state$desired_model <- if (online_enabled && nzchar(base_model_value)) {
      paste0(base_model_value, ":online")
    } else {
      model_value
    }
    setup_state$desired_type <- type_value
    setup_state$openrouter_online <- online_enabled
    setup_state$openrouter_model_base <- if (identical(tolower(service_value), "openrouter") && nzchar(base_model_value)) base_model_value else NULL
    setup_state$skip_service_event <- TRUE
    updateCheckboxInput(session, "setup_openrouter_online", value = online_enabled)
    update_setup_service_choices()
    update_setup_model_choices()
    update_setup_type_choices()
    updateNumericInput(session, "setup_temp", value = setup$temp %||% NA)
    thinking_flag <- setup$thinking %||% setup$reasoning %||% FALSE
    thinking_level <- setup$thinking_budget %||% setup$thinking_level %||% setup$reasoning_effort %||% .DEFAULT_THINKING_LEVEL
    thinking_level <- tolower(thinking_level)
    if (!thinking_level %in% .THINKING_LEVEL_CHOICES) {
      thinking_level <- .DEFAULT_THINKING_LEVEL
    }
    setup_state$thinking_enabled <- isTRUE(thinking_flag)
    setup_state$thinking_level <- thinking_level
    updateCheckboxInput(session, "setup_thinking_enabled", value = setup_state$thinking_enabled)
    updateSelectInput(session, "setup_thinking_level", selected = setup_state$thinking_level)
    setup_state$extras <- .build_setup_extras(setup)
    output$setup_extra_fields <- renderUI(.render_kv_fields(setup_state$extras, "setup_extra", input, session))
    refresh_setup_list(selected = name)
  }

  observeEvent(input$setup_select_trigger, {
    load_setup(input$setup_select_trigger$name)
  })

  observeEvent(input$setup_refresh, {
    refresh_setup_list()
  })

  observeEvent(input$setup_new, {
    setup_state$selected <- NULL
    setup_state$original <- NULL
    setup_state$draft_name <- NULL
    setup_state$draft_summary <- NULL
    setup_state$draft_source <- NULL
    reset_setup_form()
    refresh_setup_list()
  })

  observeEvent(input$setup_duplicate, {
    if (is.null(setup_state$selected)) {
      showNotification("Select a setup to duplicate.", type = "warning")
      return()
    }
    source_name <- setup_state$selected
    load_setup(source_name)
    new_name <- paste0(source_name, "_copy")
    updateTextInput(session, "setup_name", value = new_name)
    setup_state$draft_name <- new_name
    setup_state$draft_source <- source_name
    setup_state$draft_summary <- sprintf("Unsaved copy of '%s'. Save to keep it.", source_name)
    setup_state$selected <- NULL
    refresh_setup_list()
  })

  observeEvent(input$setup_delete, {
    name <- setup_state$selected
    if (is.null(name)) {
      showNotification("Select a setup to delete.", type = "warning")
      return()
    }
    showModal(modalDialog(
      title = "Delete setup",
      paste0("Are you sure you want to delete setup '", name, "'?"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete_setup", "Delete", class = "btn-danger")
      )
    ))
  })

  observeEvent(input$confirm_delete_setup, {
    name <- isolate(setup_state$selected)
    removeModal()
    if (is.null(name)) {
      return()
    }
    res <- tryCatch(rm_setup(name), warning = function(w) {
      showNotification(conditionMessage(w), type = "warning")
      FALSE
    }, error = function(e) {
      showNotification(conditionMessage(e), type = "error")
      FALSE
    })
    if (isTRUE(res)) {
      showNotification(sprintf("Setup '%s' deleted.", name), type = "message")
    }
    setup_state$selected <- NULL
    setup_state$original <- NULL
    if (exists(name, envir = setup_summary_cache, inherits = FALSE)) {
      rm(list = name, envir = setup_summary_cache)
    }
    reset_setup_form()
    refresh_setup_list()
  })

  observeEvent(input$setup_add_field, {
    extras <- setup_state$extras
    new_name <- paste0("extra_", length(extras) + 1)
    extras[[length(extras) + 1]] <- list(
      id = .rand_id("setup"),
      name = new_name,
      value = "",
      mode = "text",
      locked = FALSE
    )
    setup_state$extras <- extras
    output$setup_extra_fields <- renderUI(.render_kv_fields(setup_state$extras, "setup_extra", input, session))
  })

  observeEvent(input$setup_extra_action, {
    action <- input$setup_extra_action$action
    field_id <- input$setup_extra_action$field
    if (!nzchar(field_id)) {
      return()
    }
    extras <- setup_state$extras
    idx <- which(vapply(extras, `[[`, character(1), "id") == field_id)
    if (!length(idx)) {
      return()
    }
    idx <- idx[[1]]
    if (action == "remove") {
      extras <- extras[-idx]
    }
    setup_state$extras <- extras
    output$setup_extra_fields <- renderUI(.render_kv_fields(setup_state$extras, "setup_extra", input, session))
  })

  observeEvent(input$setup_reset, {
    if (!is.null(setup_state$original)) {
      load_setup(setup_state$original$sname %||% setup_state$selected)
    } else {
      reset_setup_form()
    }
  })

  observeEvent(input$setup_save, {
    name <- trimws(input$setup_name)
    if (!nzchar(name)) {
      showNotification("Setup name is required.", type = "error")
      return()
    }
    service <- trimws(input$setup_service %||% "")
    model <- trimws(input$setup_model %||% "")
    if (!nzchar(service) || !nzchar(model)) {
      showNotification("Service and model are required.", type = "error")
      return()
    }
    temp <- input$setup_temp
    if (is.na(temp)) temp <- NULL
    type_input <- trimws(input$setup_type %||% "")
    resolved <- tryCatch(
      .resolve_favorite_selection(service, model, type_input, models_state$favorites, models_state$catalog),
      error = function(e) {
        showNotification(conditionMessage(e), type = "error")
        NULL
      }
    )
    if (is.null(resolved)) return()
    service <- resolved$service
    model <- resolved$model
    type <- resolved$type
    if (!nzchar(type)) type <- NULL

    if (identical(tolower(service), "openrouter")) {
      has_suffix <- grepl(":online$", model, fixed = TRUE)
      base_model <- if (has_suffix) sub(":online$", "", model) else model
      if (isTRUE(input$setup_openrouter_online)) {
        model <- if (has_suffix) model else paste0(base_model, ":online")
      } else {
        model <- base_model
      }
      setup_state$openrouter_online <- isTRUE(input$setup_openrouter_online)
      setup_state$openrouter_model_base <- if (nzchar(base_model)) base_model else NULL
      setup_state$desired_model <- model
      updateSelectizeInput(session, "setup_model", selected = model, server = TRUE)
      updateCheckboxInput(session, "setup_openrouter_online", value = setup_state$openrouter_online)
    } else {
      setup_state$openrouter_online <- FALSE
      setup_state$openrouter_model_base <- NULL
      if (isTRUE(isolate(input$setup_openrouter_online))) {
        updateCheckboxInput(session, "setup_openrouter_online", value = FALSE)
      }
    }

    extras <- list()
    reserved_setup_names <- tolower(c("sname", "service", "model", "temp", "type", "thinking", "thinking_budget", "thinking_level", "reasoning", "reasoning_effort"))
    seen_extra_names <- character()
    if (length(setup_state$extras)) {
      for (field in setup_state$extras) {
        name_id <- paste0("setup_extra_", field$id, "_name")
        field_name <- if (isTRUE(field$locked)) field$name else trimws(input[[name_id]] %||% field$name)
        if (!nzchar(field_name)) {
          showNotification("Extra field names cannot be empty.", type = "error")
          return()
        }
        field_key <- tolower(field_name)
        if (field_key %in% reserved_setup_names) {
          showNotification(sprintf("Field '%s' conflicts with a reserved setup attribute.", field_name), type = "error")
          return()
        }
        if (field_key %in% seen_extra_names) {
          showNotification(sprintf("Field '%s' is duplicated. Use unique names.", field_name), type = "error")
          return()
        }
        seen_extra_names <- c(seen_extra_names, field_key)
        mode_id <- paste0("setup_extra_", field$id, "_mode")
        value_id <- paste0("setup_extra_", field$id, "_value")
        mode <- input[[mode_id]] %||% field$mode
        value_raw <- input[[value_id]] %||% field$value
        if (mode %in% c("null", "na")) {
          extras[[field_name]] <- .convert_value("", mode)
        } else {
          tryCatch(
            {
              extras[[field_name]] <- .convert_value(value_raw, mode)
            },
            error = function(e) {
              showNotification(sprintf("Field '%s': %s", field_name, conditionMessage(e)), type = "error")
              stop("conversion_error")
            }
          )
        }
      }
    }

    extras[c("thinking", "thinking_budget", "thinking_level", "reasoning", "reasoning_effort")] <- NULL
    thinking_enabled <- isTRUE(input$setup_thinking_enabled)
    thinking_level <- input$setup_thinking_level %||% setup_state$thinking_level %||% .DEFAULT_THINKING_LEVEL
    thinking_level <- tolower(thinking_level)
    if (!thinking_level %in% .THINKING_LEVEL_CHOICES) {
      thinking_level <- .DEFAULT_THINKING_LEVEL
    }
    if (thinking_enabled) {
      extras$thinking <- TRUE
      extras$thinking_budget <- thinking_level
    }

    args <- c(
      list(
        sname = name,
        service = service,
        model = model,
        temp = temp,
        type = type,
        save = TRUE,
        assign = FALSE,
        overwrite = TRUE
      ),
      extras
    )

    res <- tryCatch(do.call(set_setup, args), error = function(e) {
      showNotification(conditionMessage(e), type = "error")
      NULL
    })
    if (is.null(res)) {
      return()
    }

    original_name <- setup_state$selected
    if (!is.null(original_name) && !identical(original_name, name)) {
      rm_setup(original_name)
      if (exists(original_name, envir = setup_summary_cache, inherits = FALSE)) {
        rm(list = original_name, envir = setup_summary_cache)
      }
    }

    setup_state$draft_name <- NULL
    setup_state$draft_summary <- NULL
    setup_state$draft_source <- NULL
    setup_state$selected <- name
    setup_state$original <- res
    setup_summary_cache[[name]] <- .setup_summary_text(res)
    showNotification(sprintf("Setup '%s' saved.", name), type = "message")
    refresh_setup_list(selected = name)
  })

  output$setup_extra_fields <- renderUI(.render_kv_fields(setup_state$extras, "setup_extra", input, session))

  reset_content_fields <- function(content = list()) {
    content_state$fields <- .build_content_fields(content)
    output$content_fields_ui <- renderUI(.render_content_fields(content_state$fields, "content_field", input))
  }

  load_content <- function(name) {
    if (is.null(name) || !nzchar(name)) {
      return()
    }
    content_state$draft_name <- NULL
    content_state$draft_summary <- NULL
    content_state$draft_source <- NULL
    content <- tryCatch(get_content(name, assign = FALSE), error = function(e) {
      showNotification(conditionMessage(e), type = "error")
      NULL
    })
    if (is.null(content)) {
      return()
    }
    content_state$selected <- name
    content_state$original <- content
    content_preview_cache[[name]] <- .content_summary_text(content)
    updateTextInput(session, "content_name", value = name)
    reset_content_fields(content)
    refresh_content_list(selected = name)
  }

  observeEvent(input$content_select_trigger, {
    load_content(input$content_select_trigger$name)
  })

  observeEvent(input$content_refresh, {
    refresh_content_list()
  })

  observeEvent(input$content_new, {
    content_state$selected <- NULL
    content_state$original <- NULL
    content_state$draft_name <- NULL
    content_state$draft_summary <- NULL
    content_state$draft_source <- NULL
    updateTextInput(session, "content_name", value = "")
    reset_content_fields(list())
    refresh_content_list()
  })

  observeEvent(input$content_duplicate, {
    if (is.null(content_state$selected)) {
      showNotification("Select content to duplicate.", type = "warning")
      return()
    }
    source_name <- content_state$selected
    load_content(source_name)
    new_name <- paste0(source_name, "_copy")
    updateTextInput(session, "content_name", value = new_name)
    content_state$draft_name <- new_name
    content_state$draft_source <- source_name
    content_state$draft_summary <- sprintf("Unsaved copy of '%s'. Save to keep it.", source_name)
    content_state$selected <- NULL
    refresh_content_list()
  })

  observeEvent(input$content_delete, {
    name <- content_state$selected
    if (is.null(name)) {
      showNotification("Select content to delete.", type = "warning")
      return()
    }
    showModal(modalDialog(
      title = "Delete content",
      paste0("Are you sure you want to delete '", name, "'?"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete_content", "Delete", class = "btn-danger")
      )
    ))
  })

  observeEvent(input$confirm_delete_content, {
    name <- isolate(content_state$selected)
    removeModal()
    if (is.null(name)) {
      return()
    }
    res <- tryCatch(rm_content(name), warning = function(w) {
      showNotification(conditionMessage(w), type = "warning")
      FALSE
    }, error = function(e) {
      showNotification(conditionMessage(e), type = "error")
      FALSE
    })
    if (isTRUE(res)) {
      showNotification(sprintf("Content '%s' deleted.", name), type = "message")
    }
    content_state$selected <- NULL
    content_state$original <- NULL
    if (exists(name, envir = content_preview_cache, inherits = FALSE)) {
      rm(list = name, envir = content_preview_cache)
    }
    updateTextInput(session, "content_name", value = "")
    reset_content_fields(list())
    refresh_content_list()
  })

  observeEvent(input$content_add_field, {
    fields <- content_state$fields
    new_name <- paste0("field_", length(fields) + 1)
    fields[[length(fields) + 1]] <- list(
      id = .rand_id("content"),
      name = new_name,
      locked = FALSE,
      mode = "text",
      text_value = "",
      path_value = ""
    )
    content_state$fields <- fields
    output$content_fields_ui <- renderUI(.render_content_fields(content_state$fields, "content_field", input))
  })

  observeEvent(input$content_reset_fields, {
    reset_content_fields(list())
  })

  observeEvent(input$content_field_action, {
    req(input$content_field_action$field)
    action <- input$content_field_action$action
    field_id <- input$content_field_action$field
    fields <- content_state$fields
    idx <- which(vapply(fields, `[[`, character(1), "id") == field_id)
    if (!length(idx)) {
      return()
    }
    idx <- idx[[1]]
    field <- fields[[idx]]
    label_id <- paste0("content_field_", field_id, "_label")
    current_label <- if (isTRUE(field$locked)) field$name else (input[[label_id]] %||% field$name)
    if (!isTRUE(field$locked)) {
      fields[[idx]]$name <- current_label
      field <- fields[[idx]]
    }

    if (action == "remove" && !isTRUE(field$locked)) {
      fields <- fields[-idx]
      content_state$fields <- fields
      output$content_fields_ui <- renderUI(.render_content_fields(fields, "content_field", input))
      return()
    }

    if (action == "load_file") {
      showModal(modalDialog(
        title = sprintf("Load text for '%s'", current_label),
        fileInput("content_modal_file", "Choose text file", accept = c(".txt", ".md", ".markdown", ".json", ".yaml", ".yml")),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("content_modal_use_file", "Load text", class = "btn-primary")
        )
      ))
      content_state$modal_field <- field_id
      return()
    }

    if (action == "load_path") {
      showModal(modalDialog(
        title = sprintf("Load text from path for '%s'", current_label),
        textInput("content_modal_path", "File path"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("content_modal_use_path_text", "Load text", class = "btn-primary"),
          actionButton("content_modal_use_path_store", "Store as path", class = "btn-secondary")
        )
      ))
      content_state$modal_field <- field_id
      return()
    }

    if (action == "check_path") {
      path_id <- paste0("content_field_", field_id, "_path")
      path_value <- trimws(input[[path_id]] %||% field$path_value)
      if (!nzchar(path_value)) {
        showNotification("Path is empty.", type = "warning")
        return()
      }
      preview <- tryCatch(.safe_read_text(path_value, max_size = 250000), error = function(e) {
        showNotification(conditionMessage(e), type = "error")
        NULL
      })
      if (is.null(preview)) {
        return()
      }
      showModal(modalDialog(
        title = "Preview",
        div(class = "gf-preview", .format_preview(preview, 800)),
        footer = modalButton("Close")
      ))
    }
  })

  observeEvent(input$content_modal_use_file, {
    field_id <- content_state$modal_field
    removeModal()
    if (is.null(field_id)) {
      return()
    }
    upload <- input$content_modal_file
    if (is.null(upload) || !file.exists(upload$datapath)) {
      showNotification("No file selected.", type = "warning")
      return()
    }
    text <- tryCatch(read_file(upload$datapath), error = function(e) {
      showNotification(conditionMessage(e), type = "error")
      NULL
    })
    if (is.null(text)) {
      return()
    }
    fields <- content_state$fields
    idx <- which(vapply(fields, `[[`, character(1), "id") == field_id)
    if (!length(idx)) {
      return()
    }
    idx <- idx[[1]]
    fields[[idx]]$mode <- "text"
    fields[[idx]]$text_value <- text
    content_state$fields <- fields
    updateTextAreaInput(session, paste0("content_field_", field_id, "_text"), value = text)
    updateSelectInput(session, paste0("content_field_", field_id, "_mode"), selected = "text")
    output$content_fields_ui <- renderUI(.render_content_fields(fields, "content_field", input))
  })

  observeEvent(input$content_modal_use_path_text, {
    field_id <- content_state$modal_field
    removeModal()
    if (is.null(field_id)) {
      return()
    }
    path_value <- trimws(input$content_modal_path)
    if (!nzchar(path_value)) {
      showNotification("Path is empty.", type = "warning")
      return()
    }
    text <- tryCatch(.safe_read_text(path_value), error = function(e) {
      showNotification(conditionMessage(e), type = "error")
      NULL
    })
    if (is.null(text)) {
      return()
    }
    fields <- content_state$fields
    idx <- which(vapply(fields, `[[`, character(1), "id") == field_id)
    if (!length(idx)) {
      return()
    }
    idx <- idx[[1]]
    fields[[idx]]$mode <- "text"
    fields[[idx]]$text_value <- text
    content_state$fields <- fields
    updateTextAreaInput(session, paste0("content_field_", field_id, "_text"), value = text)
    updateSelectInput(session, paste0("content_field_", field_id, "_mode"), selected = "text")
    output$content_fields_ui <- renderUI(.render_content_fields(fields, "content_field", input))
  })

  observeEvent(input$content_modal_use_path_store, {
    field_id <- content_state$modal_field
    removeModal()
    if (is.null(field_id)) {
      return()
    }
    path_value <- trimws(input$content_modal_path)
    if (!nzchar(path_value)) {
      showNotification("Path is empty.", type = "warning")
      return()
    }
    fields <- content_state$fields
    idx <- which(vapply(fields, `[[`, character(1), "id") == field_id)
    if (!length(idx)) {
      return()
    }
    idx <- idx[[1]]
    fields[[idx]]$mode <- "path"
    fields[[idx]]$path_value <- path_value
    content_state$fields <- fields
    updateTextInput(session, paste0("content_field_", field_id, "_path"), value = path_value)
    updateSelectInput(session, paste0("content_field_", field_id, "_mode"), selected = "path")
    output$content_fields_ui <- renderUI(.render_content_fields(fields, "content_field", input))
  })

  observeEvent(input$content_reset, {
    if (!is.null(content_state$original)) {
      reset_content_fields(content_state$original)
      updateTextInput(session, "content_name", value = content_state$selected)
    } else {
      updateTextInput(session, "content_name", value = "")
      reset_content_fields(list())
    }
  })

  observeEvent(input$content_save, {
    name <- trimws(input$content_name)
    if (!nzchar(name)) {
      showNotification("Content name is required.", type = "error")
      return()
    }

    fields <- content_state$fields
    payload <- list()
    for (field in fields) {
      field_name <- if (isTRUE(field$locked)) field$name else trimws(input[[paste0("content_field_", field$id, "_label")]] %||% field$name)
      if (!nzchar(field_name)) {
        showNotification("Field names cannot be empty.", type = "error")
        return()
      }
      mode <- input[[paste0("content_field_", field$id, "_mode")]] %||% field$mode
      if (mode == "null") {
        next
      }
      if (mode == "na") {
        payload[[field_name]] <- NA_character_
        next
      }
      if (mode == "path") {
        path_value <- trimws(input[[paste0("content_field_", field$id, "_path")]] %||% field$path_value)
        if (!nzchar(path_value)) {
          showNotification(sprintf("Field '%s': path is empty.", field_name), type = "error")
          return()
        }
        payload[[field_name]] <- path_value
      } else if (mode == "text") {
        text_value <- input[[paste0("content_field_", field$id, "_text")]] %||% field$text_value
        payload[[field_name]] <- text_value %||% ""
      }
    }

    args <- c(
      list(
        cname = name,
        save = TRUE,
        assign = FALSE,
        overwrite = TRUE
      ),
      payload
    )

    res <- tryCatch(do.call(set_content, args), error = function(e) {
      showNotification(conditionMessage(e), type = "error")
      NULL
    })

    if (is.null(res)) {
      return()
    }
    original_name <- content_state$selected
    if (!is.null(original_name) && !identical(original_name, name)) {
      rm_content(original_name)
      if (exists(original_name, envir = content_preview_cache, inherits = FALSE)) {
        rm(list = original_name, envir = content_preview_cache)
      }
    }
    content_state$draft_name <- NULL
    content_state$draft_summary <- NULL
    content_state$draft_source <- NULL
    content_state$selected <- name
    content_state$original <- payload
    content_preview_cache[[name]] <- .content_summary_text(res)
    showNotification(sprintf("Content '%s' saved.", name), type = "message")
    refresh_content_list(selected = name)
  })

  output$content_fields_ui <- renderUI(.render_content_fields(content_state$fields, "content_field", input))

  load_agent <- function(name) {
    if (is.null(name) || !nzchar(name)) {
      return()
    }
    agent_state$draft_name <- NULL
    agent_state$draft_summary <- NULL
    agent_state$draft_source <- NULL
    agent_state$loading <- TRUE
    on.exit(
      {
        agent_state$loading <- FALSE
      },
      add = TRUE
    )
    agent <- tryCatch(get_agent(name, assign = FALSE), error = function(e) {
      showNotification(conditionMessage(e), type = "error")
      NULL
    })
    if (is.null(agent)) {
      return()
    }
    agent_state$selected <- name
    agent_state$original <- agent
    agent_summary_cache[[name]] <- .agent_summary_text(agent)
    agent_state$content_modal_field <- NULL
    updateTextInput(session, "agent_name", value = agent$name %||% name)

    if (!is.null(agent$sname) && nzchar(agent$sname)) {
      updateRadioButtons(session, "agent_setup_mode", selected = "existing")
      updateSelectInput(session, "agent_setup_select", selected = agent$sname)
      update_agent_setup_preview(agent$sname)
      setup_details <- tryCatch(get_setup(agent$sname, assign = FALSE), error = function(e) NULL)
      setup_fields <- setup_details %||% list()
    } else {
      updateRadioButtons(session, "agent_setup_mode", selected = "custom")
      service_value <- agent$service %||% ""
      model_value <- agent$model %||% ""
      type_value <- agent$type %||% ""
      agent_state$custom_desired_service <- if (nzchar(service_value)) service_value else .DEFAULT_MODEL_SERVICE
      agent_state$custom_desired_model <- if (nzchar(model_value)) model_value else .DEFAULT_MODEL_NAME
      agent_state$custom_desired_type <- if (nzchar(type_value)) type_value else .DEFAULT_MODEL_TYPE
      update_agent_service_choices()
      update_agent_model_choices()
      update_agent_type_choices()
      updateNumericInput(session, "agent_setup_temp", value = agent$temp %||% NA)
      setup_fields <- list(
        service = if (nzchar(service_value)) service_value else NULL,
        model = if (nzchar(model_value)) model_value else NULL,
        temp = agent$temp,
        type = if (nzchar(type_value)) type_value else NULL
      )
    }

    if (!is.null(agent$cname) && nzchar(agent$cname)) {
      updateRadioButtons(session, "agent_content_mode", selected = "existing")
      updateSelectInput(session, "agent_content_select", selected = agent$cname)
      update_agent_content_preview(agent$cname)
      content_details <- tryCatch(get_content(agent$cname, assign = FALSE), error = function(e) NULL)
      content_fields <- content_details %||% list()
    } else {
      updateRadioButtons(session, "agent_content_mode", selected = "custom")
      custom_content <- list()
      known_fields <- intersect(names(agent), .default_content_keys)
      for (key in union(.default_content_keys, known_fields)) {
        if (!is.null(agent[[key]])) {
          custom_content[[key]] <- agent[[key]]
        }
      }
      agent_state$custom_content_fields <- .build_content_fields(custom_content)
      output$agent_content_fields_ui <- renderUI(.render_content_fields(agent_state$custom_content_fields, "agent_content_field", input))
      content_fields <- custom_content
    }

    agent_state$setup_extras <- .build_setup_extras(setup_fields)
    agent_state$overrides <- .build_override_fields(agent, setup_fields, content_fields)
    output$agent_setup_extra_fields <- renderUI(.render_kv_fields(agent_state$setup_extras, "agent_setup_extra", input, session))
    output$agent_override_fields <- renderUI(.render_kv_fields(agent_state$overrides, "agent_override", input, session))
    refresh_agent_list(selected = name)
  }

  observeEvent(input$agent_select_trigger, {
    load_agent(input$agent_select_trigger$name)
  })

  observeEvent(input$agent_refresh, {
    refresh_agent_list()
  })

  observeEvent(input$agent_setup_select, {
    if (!identical(input$agent_setup_mode, "existing")) {
      return()
    }
    update_agent_setup_preview(trimws(input$agent_setup_select))
  })

  observeEvent(input$agent_setup_mode, {
    if (!identical(input$agent_setup_mode, "existing")) {
      output$agent_setup_preview <- renderUI(NULL)
      if (identical(input$agent_setup_mode, "custom")) {
        agent_state$loading <- TRUE
        on.exit(
          {
            agent_state$loading <- FALSE
          },
          add = TRUE
        )
        current_service <- input$agent_setup_service %||% ""
        current_model <- input$agent_setup_model %||% ""
        current_type <- input$agent_setup_type %||% ""
        if (!nzchar(current_service)) {
          agent_state$custom_desired_service <- .DEFAULT_MODEL_SERVICE
        }
        if (!nzchar(current_model)) {
          agent_state$custom_desired_model <- .DEFAULT_MODEL_NAME
        }
        if (!nzchar(current_type)) {
          agent_state$custom_desired_type <- .DEFAULT_MODEL_TYPE
        }
        update_agent_service_choices()
        update_agent_model_choices()
        update_agent_type_choices()
      }
    } else {
      isolate(update_agent_setup_preview(trimws(input$agent_setup_select)))
    }
  })

  observeEvent(input$agent_content_select, {
    if (!identical(input$agent_content_mode, "existing")) {
      return()
    }
    update_agent_content_preview(trimws(input$agent_content_select))
  })

  observeEvent(input$agent_content_mode, {
    if (!identical(input$agent_content_mode, "existing")) {
      output$agent_content_preview <- renderUI(NULL)
    } else {
      isolate(update_agent_content_preview(trimws(input$agent_content_select)))
    }
  })

  observeEvent(input$agent_new, {
    agent_state$loading <- TRUE
    on.exit(
      {
        agent_state$loading <- FALSE
      },
      add = TRUE
    )
    agent_state$selected <- NULL
    agent_state$original <- NULL
    agent_state$draft_name <- NULL
    agent_state$draft_summary <- NULL
    agent_state$draft_source <- NULL
    updateTextInput(session, "agent_name", value = "")
    updateRadioButtons(session, "agent_setup_mode", selected = "existing")
    updateSelectInput(session, "agent_setup_select", selected = "")
    updateRadioButtons(session, "agent_content_mode", selected = "none")
    updateSelectInput(session, "agent_content_select", selected = "")
    if (models_state$favorites_present && nrow(models_state$favorites) > 0) {
      favs <- models_state$favorites
      agent_state$custom_desired_service <- "favorites"
      agent_state$custom_desired_model <- favs$model[1]
      fav_types <- favs$type[favs$model == favs$model[1]]
      fav_types <- fav_types[nzchar(fav_types)]
      agent_state$custom_desired_type <- if (length(fav_types)) fav_types[[1]] else .DEFAULT_MODEL_TYPE
    } else {
      agent_state$custom_desired_service <- .DEFAULT_MODEL_SERVICE
      agent_state$custom_desired_model <- .DEFAULT_MODEL_NAME
      agent_state$custom_desired_type <- .DEFAULT_MODEL_TYPE
    }
    update_agent_service_choices()
    update_agent_model_choices()
    update_agent_type_choices()
    agent_state$setup_extras <- list()
    agent_state$custom_content_fields <- .build_content_fields(list())
    agent_state$overrides <- list(
      list(id = .rand_id("override"), name = "label", value = "", mode = "text", locked = FALSE)
    )
    agent_state$content_modal_field <- NULL
    output$agent_setup_extra_fields <- renderUI(.render_kv_fields(agent_state$setup_extras, "agent_setup_extra", input, session))
    output$agent_content_fields_ui <- renderUI(.render_content_fields(agent_state$custom_content_fields, "agent_content_field", input))
    output$agent_override_fields <- renderUI(.render_kv_fields(agent_state$overrides, "agent_override", input, session))
    refresh_agent_list()
  })

  observeEvent(input$agent_duplicate, {
    if (is.null(agent_state$selected)) {
      showNotification("Select an agent to duplicate.", type = "warning")
      return()
    }
    source_name <- agent_state$selected
    load_agent(source_name)
    new_name <- paste0(source_name, "_copy")
    updateTextInput(session, "agent_name", value = new_name)
    agent_state$draft_name <- new_name
    agent_state$draft_source <- source_name
    agent_state$draft_summary <- sprintf("Unsaved copy of '%s'. Save to keep it.", source_name)
    agent_state$selected <- NULL
    refresh_agent_list()
  })

  observeEvent(input$agent_delete, {
    name <- agent_state$selected
    if (is.null(name)) {
      showNotification("Select an agent to delete.", type = "warning")
      return()
    }
    showModal(modalDialog(
      title = "Delete agent",
      paste0("Delete agent '", name, "'?"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete_agent", "Delete", class = "btn-danger")
      )
    ))
  })

  observeEvent(input$confirm_delete_agent, {
    name <- isolate(agent_state$selected)
    removeModal()
    if (is.null(name)) {
      return()
    }
    res <- tryCatch(rm_agent(name), warning = function(w) {
      showNotification(conditionMessage(w), type = "warning")
      FALSE
    }, error = function(e) {
      showNotification(conditionMessage(e), type = "error")
      FALSE
    })
    if (isTRUE(res)) {
      showNotification(sprintf("Agent '%s' deleted.", name), type = "message")
    }
    agent_state$selected <- NULL
    agent_state$original <- NULL
    if (exists(name, envir = agent_summary_cache, inherits = FALSE)) {
      rm(list = name, envir = agent_summary_cache)
    }
    refresh_agent_list()
  })

  observeEvent(input$agent_setup_add_field, {
    fields <- agent_state$setup_extras
    fields[[length(fields) + 1]] <- list(
      id = .rand_id("setup"),
      name = paste0("custom_", length(fields) + 1),
      value = "",
      mode = "text",
      locked = FALSE
    )
    agent_state$setup_extras <- fields
    output$agent_setup_extra_fields <- renderUI(.render_kv_fields(agent_state$setup_extras, "agent_setup_extra", input, session))
  })

  observeEvent(input$agent_setup_extra_action, {
    action <- input$agent_setup_extra_action$action
    field_id <- input$agent_setup_extra_action$field
    fields <- agent_state$setup_extras
    idx <- which(vapply(fields, `[[`, character(1), "id") == field_id)
    if (!length(idx)) {
      return()
    }
    idx <- idx[[1]]
    if (action == "remove") {
      fields <- fields[-idx]
    }
    agent_state$setup_extras <- fields
    output$agent_setup_extra_fields <- renderUI(.render_kv_fields(fields, "agent_setup_extra", input, session))
  })

  observeEvent(input$agent_content_add_field, {
    fields <- agent_state$custom_content_fields
    fields[[length(fields) + 1]] <- list(
      id = .rand_id("content"),
      name = paste0("field_", length(fields) + 1),
      locked = FALSE,
      mode = "text",
      text_value = "",
      path_value = ""
    )
    agent_state$custom_content_fields <- fields
    output$agent_content_fields_ui <- renderUI(.render_content_fields(fields, "agent_content_field", input))
  })

  observeEvent(input$agent_content_reset_fields, {
    agent_state$custom_content_fields <- .build_content_fields(list())
    output$agent_content_fields_ui <- renderUI(.render_content_fields(agent_state$custom_content_fields, "agent_content_field", input))
  })

  observeEvent(input$agent_content_field_action, {
    req(input$agent_content_field_action$field)
    action <- input$agent_content_field_action$action
    field_id <- input$agent_content_field_action$field
    fields <- agent_state$custom_content_fields
    idx <- which(vapply(fields, `[[`, character(1), "id") == field_id)
    if (!length(idx)) {
      return()
    }
    idx <- idx[[1]]
    field <- fields[[idx]]
    label_id <- paste0("agent_content_field_", field_id, "_label")
    current_label <- if (isTRUE(field$locked)) field$name else (input[[label_id]] %||% field$name)
    if (!isTRUE(field$locked)) {
      fields[[idx]]$name <- current_label
      field <- fields[[idx]]
    }

    if (action == "remove" && !isTRUE(field$locked)) {
      fields <- fields[-idx]
      agent_state$custom_content_fields <- fields
      output$agent_content_fields_ui <- renderUI(.render_content_fields(fields, "agent_content_field", input))
      return()
    }

    if (action == "load_file") {
      showModal(modalDialog(
        title = sprintf("Load text for '%s'", current_label),
        fileInput("agent_content_modal_file", "Choose text file", accept = c(".txt", ".md", ".markdown", ".json", ".yaml", ".yml")),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("agent_content_modal_use_file", "Load text", class = "btn-primary")
        )
      ))
      agent_state$content_modal_field <- field_id
      return()
    }

    if (action == "load_path") {
      showModal(modalDialog(
        title = sprintf("Load text from path for '%s'", current_label),
        textInput("agent_content_modal_path", "File path"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("agent_content_modal_use_path_text", "Load text", class = "btn-primary"),
          actionButton("agent_content_modal_use_path_store", "Store as path", class = "btn-secondary")
        )
      ))
      agent_state$content_modal_field <- field_id
      return()
    }

    if (action == "check_path") {
      path_id <- paste0("agent_content_field_", field_id, "_path")
      path_value <- trimws(input[[path_id]] %||% field$path_value)
      if (!nzchar(path_value)) {
        showNotification("Path is empty.", type = "warning")
        return()
      }
      preview <- tryCatch(.safe_read_text(path_value, max_size = 250000), error = function(e) {
        showNotification(conditionMessage(e), type = "error")
        NULL
      })
      if (is.null(preview)) {
        return()
      }
      showModal(modalDialog(
        title = "Preview",
        div(class = "gf-preview", .format_preview(preview, 800)),
        footer = modalButton("Close")
      ))
    }
  })

  observeEvent(input$agent_content_modal_use_file, {
    field_id <- agent_state$content_modal_field
    removeModal()
    agent_state$content_modal_field <- NULL
    if (is.null(field_id)) {
      return()
    }
    upload <- input$agent_content_modal_file
    if (is.null(upload) || !file.exists(upload$datapath)) {
      showNotification("No file selected.", type = "warning")
      return()
    }
    text <- tryCatch(read_file(upload$datapath), error = function(e) {
      showNotification(conditionMessage(e), type = "error")
      NULL
    })
    if (is.null(text)) {
      return()
    }
    fields <- agent_state$custom_content_fields
    idx <- which(vapply(fields, `[[`, character(1), "id") == field_id)
    if (!length(idx)) {
      return()
    }
    idx <- idx[[1]]
    fields[[idx]]$mode <- "text"
    fields[[idx]]$text_value <- text
    agent_state$custom_content_fields <- fields
    updateTextAreaInput(session, paste0("agent_content_field_", field_id, "_text"), value = text)
    updateSelectInput(session, paste0("agent_content_field_", field_id, "_mode"), selected = "text")
    output$agent_content_fields_ui <- renderUI(.render_content_fields(fields, "agent_content_field", input))
  })

  observeEvent(input$agent_content_modal_use_path_text, {
    field_id <- agent_state$content_modal_field
    removeModal()
    agent_state$content_modal_field <- NULL
    if (is.null(field_id)) {
      return()
    }
    path_value <- trimws(input$agent_content_modal_path)
    if (!nzchar(path_value)) {
      showNotification("Path is empty.", type = "warning")
      return()
    }
    text <- tryCatch(.safe_read_text(path_value), error = function(e) {
      showNotification(conditionMessage(e), type = "error")
      NULL
    })
    if (is.null(text)) {
      return()
    }
    fields <- agent_state$custom_content_fields
    idx <- which(vapply(fields, `[[`, character(1), "id") == field_id)
    if (!length(idx)) {
      return()
    }
    idx <- idx[[1]]
    fields[[idx]]$mode <- "text"
    fields[[idx]]$text_value <- text
    agent_state$custom_content_fields <- fields
    updateTextAreaInput(session, paste0("agent_content_field_", field_id, "_text"), value = text)
    updateSelectInput(session, paste0("agent_content_field_", field_id, "_mode"), selected = "text")
    output$agent_content_fields_ui <- renderUI(.render_content_fields(fields, "agent_content_field", input))
  })

  observeEvent(input$agent_content_modal_use_path_store, {
    field_id <- agent_state$content_modal_field
    removeModal()
    agent_state$content_modal_field <- NULL
    if (is.null(field_id)) {
      return()
    }
    path_value <- trimws(input$agent_content_modal_path)
    if (!nzchar(path_value)) {
      showNotification("Path is empty.", type = "warning")
      return()
    }
    fields <- agent_state$custom_content_fields
    idx <- which(vapply(fields, `[[`, character(1), "id") == field_id)
    if (!length(idx)) {
      return()
    }
    idx <- idx[[1]]
    fields[[idx]]$mode <- "path"
    fields[[idx]]$path_value <- path_value
    agent_state$custom_content_fields <- fields
    updateTextInput(session, paste0("agent_content_field_", field_id, "_path"), value = path_value)
    updateSelectInput(session, paste0("agent_content_field_", field_id, "_mode"), selected = "path")
    output$agent_content_fields_ui <- renderUI(.render_content_fields(fields, "agent_content_field", input))
  })

  observeEvent(input$agent_add_override, {
    fields <- agent_state$overrides
    fields[[length(fields) + 1]] <- list(
      id = .rand_id("override"),
      name = paste0("override_", length(fields) + 1),
      value = "",
      mode = "text",
      locked = FALSE
    )
    agent_state$overrides <- fields
    output$agent_override_fields <- renderUI(.render_kv_fields(agent_state$overrides, "agent_override", input, session))
  })

  observeEvent(input$agent_override_action, {
    action <- input$agent_override_action$action
    field_id <- input$agent_override_action$field
    fields <- agent_state$overrides
    idx <- which(vapply(fields, `[[`, character(1), "id") == field_id)
    if (!length(idx)) {
      return()
    }
    idx <- idx[[1]]
    if (action == "remove") {
      fields <- fields[-idx]
    }
    agent_state$overrides <- fields
    output$agent_override_fields <- renderUI(.render_kv_fields(fields, "agent_override", input, session))
  })

  observeEvent(input$agent_reset, {
    agent_state$content_modal_field <- NULL
    if (!is.null(agent_state$original)) {
      load_agent(agent_state$original$name %||% agent_state$selected)
    } else {
      agent_state$loading <- TRUE
      on.exit(
        {
          agent_state$loading <- FALSE
        },
        add = TRUE
      )
      agent_state$selected <- NULL
      agent_state$original <- NULL
      updateTextInput(session, "agent_name", value = "")
      updateRadioButtons(session, "agent_setup_mode", selected = "existing")
      updateSelectInput(session, "agent_setup_select", selected = "")
      updateRadioButtons(session, "agent_content_mode", selected = "none")
      updateSelectInput(session, "agent_content_select", selected = "")
      agent_state$custom_desired_service <- .DEFAULT_MODEL_SERVICE
      agent_state$custom_desired_model <- .DEFAULT_MODEL_NAME
      agent_state$custom_desired_type <- .DEFAULT_MODEL_TYPE
      update_agent_service_choices()
      update_agent_model_choices()
      update_agent_type_choices()
      agent_state$setup_extras <- list()
      agent_state$custom_content_fields <- .build_content_fields(list())
      agent_state$overrides <- list()
      output$agent_setup_extra_fields <- renderUI(.render_kv_fields(agent_state$setup_extras, "agent_setup_extra", input, session))
      output$agent_content_fields_ui <- renderUI(.render_content_fields(agent_state$custom_content_fields, "agent_content_field", input))
      output$agent_override_fields <- renderUI(.render_kv_fields(agent_state$overrides, "agent_override", input, session))
    }
  })

  observeEvent(input$agent_save, {
    name <- trimws(input$agent_name %||% "")
    if (!nzchar(name)) {
      showNotification("Agent name is required.", type = "error")
      return()
    }

    setup_mode <- input$agent_setup_mode
    content_mode <- input$agent_content_mode

    setup_argument <- NULL
    if (setup_mode == "existing") {
      setup_selected <- trimws(input$agent_setup_select %||% "")
      if (!nzchar(setup_selected)) {
        showNotification("Select a setup for the agent.", type = "error")
        return()
      }
      setup_argument <- setup_selected
    } else {
      service <- trimws(input$agent_setup_service %||% "")
      model <- trimws(input$agent_setup_model %||% "")
      if (!nzchar(service) || !nzchar(model)) {
        showNotification("Custom setup requires service and model.", type = "error")
        return()
      }
      type_input <- trimws(input$agent_setup_type %||% "")
      resolved <- tryCatch(
        .resolve_favorite_selection(service, model, type_input, models_state$favorites, models_state$catalog),
        error = function(e) {
          showNotification(conditionMessage(e), type = "error")
          NULL
        }
      )
      if (is.null(resolved)) {
        return()
      }
      service <- resolved$service
      model <- resolved$model
      resolved_type <- resolved$type %||% ""
      custom_setup <- list(
        sname = NULL,
        service = service,
        model = model,
        temp = if (is.na(input$agent_setup_temp)) NULL else input$agent_setup_temp,
        type = if (!nzchar(resolved_type)) NULL else resolved_type
      )
      reserved_setup_names <- tolower(c("sname", "service", "model", "temp", "type", "thinking", "thinking_budget", "thinking_level", "reasoning", "reasoning_effort"))
      seen_setup_names <- character()
      if (length(agent_state$setup_extras)) {
        for (field in agent_state$setup_extras) {
          name_id <- paste0("agent_setup_extra_", field$id, "_name")
          field_name <- if (isTRUE(field$locked)) field$name else trimws(input[[name_id]] %||% field$name)
          if (!nzchar(field_name)) {
            showNotification("Custom setup field names cannot be empty.", type = "error")
            return()
          }
          field_key <- tolower(field_name)
          if (field_key %in% reserved_setup_names) {
            showNotification(sprintf("Custom setup field '%s' conflicts with a reserved name.", field_name), type = "error")
            return()
          }
          if (field_key %in% seen_setup_names) {
            showNotification(sprintf("Custom setup field '%s' is duplicated. Use unique names.", field_name), type = "error")
            return()
          }
          seen_setup_names <- c(seen_setup_names, field_key)
          mode <- input[[paste0("agent_setup_extra_", field$id, "_mode")]] %||% field$mode
          value_raw <- input[[paste0("agent_setup_extra_", field$id, "_value")]] %||% field$value
          if (mode %in% c("null", "na")) {
            custom_setup[[field_name]] <- .convert_value("", mode)
          } else {
            tryCatch(
              {
                custom_setup[[field_name]] <- .convert_value(value_raw, mode)
              },
              error = function(e) {
                showNotification(sprintf("Custom setup '%s': %s", field_name, conditionMessage(e)), type = "error")
                stop("conversion_error")
              }
            )
          }
        }
      }
      setup_argument <- custom_setup
    }

    content_argument <- NULL
    if (content_mode == "existing") {
      content_selected <- trimws(input$agent_content_select %||% "")
      if (!nzchar(content_selected)) {
        showNotification("Select content or choose 'None'.", type = "error")
        return()
      }
      content_argument <- content_selected
    } else if (content_mode == "custom") {
      fields <- agent_state$custom_content_fields
      custom_content <- list()
      for (field in fields) {
        field_name <- if (isTRUE(field$locked)) field$name else trimws(input[[paste0("agent_content_field_", field$id, "_label")]] %||% field$name)
        if (!nzchar(field_name)) {
          showNotification("Custom content field names cannot be empty.", type = "error")
          return()
        }
        mode <- input[[paste0("agent_content_field_", field$id, "_mode")]] %||% field$mode
        if (mode == "null") next
        if (mode == "na") {
          custom_content[[field_name]] <- NA_character_
        } else if (mode == "path") {
          path_value <- trimws(input[[paste0("agent_content_field_", field$id, "_path")]] %||% field$path_value)
          if (!nzchar(path_value)) {
            showNotification(sprintf("Field '%s': path is empty.", field_name), type = "error")
            return()
          }
          custom_content[[field_name]] <- path_value
        } else {
          text_value <- input[[paste0("agent_content_field_", field$id, "_text")]] %||% field$text_value
          custom_content[[field_name]] <- text_value %||% ""
        }
      }
      content_argument <- custom_content
    }

    overrides <- list()
    reserved_override_names <- tolower(c(
      "name", "setup", "content", "save", "assign", "overwrite",
      "sname", "cname", "service", "model", "temp", "type",
      "thinking", "thinking_budget", "thinking_level", "reasoning", "reasoning_effort"
    ))
    seen_override_names <- character()
    if (length(agent_state$overrides)) {
      for (field in agent_state$overrides) {
        name_id <- paste0("agent_override_", field$id, "_name")
        field_name <- if (isTRUE(field$locked)) field$name else trimws(input[[name_id]] %||% field$name)
        if (!nzchar(field_name)) {
          showNotification("Override field names cannot be empty.", type = "error")
          return()
        }
        field_key <- tolower(field_name)
        if (field_key %in% reserved_override_names) {
          showNotification(sprintf("Override '%s' conflicts with a reserved agent attribute.", field_name), type = "error")
          return()
        }
        if (field_key %in% seen_override_names) {
          showNotification(sprintf("Override '%s' is duplicated. Use unique names.", field_name), type = "error")
          return()
        }
        seen_override_names <- c(seen_override_names, field_key)
        mode <- input[[paste0("agent_override_", field$id, "_mode")]] %||% field$mode
        value_raw <- input[[paste0("agent_override_", field$id, "_value")]] %||% field$value
        if (mode %in% c("null", "na")) {
          overrides[[field_name]] <- .convert_value("", mode)
        } else {
          tryCatch(
            {
              overrides[[field_name]] <- .convert_value(value_raw, mode)
            },
            error = function(e) {
              showNotification(sprintf("Override '%s': %s", field_name, conditionMessage(e)), type = "error")
              stop("conversion_error")
            }
          )
        }
      }
    }

    args <- c(
      list(
        name = name,
        setup = setup_argument,
        content = content_argument,
        save = TRUE,
        assign = FALSE,
        overwrite = TRUE
      ),
      overrides
    )

    res <- tryCatch(do.call(set_agent, args), error = function(e) {
      showNotification(conditionMessage(e), type = "error")
      NULL
    })
    if (is.null(res)) {
      return()
    }

    original <- agent_state$selected
    if (!is.null(original) && !identical(original, name)) {
      rm_agent(original)
      if (exists(original, envir = agent_summary_cache, inherits = FALSE)) {
        rm(list = original, envir = agent_summary_cache)
      }
    }

    agent_state$draft_name <- NULL
    agent_state$draft_summary <- NULL
    agent_state$draft_source <- NULL
    agent_state$selected <- name
    agent_state$original <- res
    agent_summary_cache[[name]] <- .agent_summary_text(res)
    showNotification(sprintf("Agent '%s' saved.", name), type = "message")
    refresh_agent_list(selected = name)
  })

  output$transfer_export_download <- downloadHandler(
    filename = function() {
      name <- trimws(input$transfer_export_filename %||% "")
      if (!nzchar(name)) {
        name <- paste0("genflow_bundle_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
      }
      if (!grepl("\\.zip$", name, ignore.case = TRUE)) {
        name <- paste0(name, ".zip")
      }
      name
    },
    content = function(file) {
      include_flags <- c(
        setups = isTRUE(input$transfer_include_setups),
        content = isTRUE(input$transfer_include_content),
        agents = isTRUE(input$transfer_include_agents),
        models = isTRUE(input$transfer_include_models)
      )
      if (!any(include_flags)) {
        showNotification("Select at least one component to export.", type = "warning")
        req(FALSE)
      }
      models_dir <- current_models_dir()
      result <- tryCatch(
        gen_export_bundle(
          path = file,
          include_setups = include_flags[["setups"]],
          include_agents = include_flags[["agents"]],
          include_content = include_flags[["content"]],
          include_models = include_flags[["models"]],
          models_dir = models_dir,
          overwrite = TRUE,
          quiet = TRUE
        ),
        error = function(e) {
          showNotification(conditionMessage(e), type = "error")
          req(FALSE)
        }
      )
      transfer_state$export_summary <- list(
        timestamp = Sys.time(),
        counts = result$counts,
        includes = result$includes,
        metadata = result$metadata
      )
      showNotification("Export bundle created.", type = "message")
    }
  )

  observeEvent(input$transfer_import_run, {
    file_info <- input$transfer_import_file
    if (is.null(file_info) || !nzchar(file_info$datapath)) {
      showNotification("Select a bundle (.zip) to import.", type = "warning")
      return()
    }
    include_flags <- c(
      setups = isTRUE(input$transfer_import_setups),
      content = isTRUE(input$transfer_import_content),
      agents = isTRUE(input$transfer_import_agents),
      models = isTRUE(input$transfer_import_models)
    )
    if (!any(include_flags)) {
      showNotification("Select at least one component to import.", type = "warning")
      return()
    }
    overwrite <- isTRUE(input$transfer_import_overwrite)
    models_dir <- current_models_dir()
    withProgress(message = "Importing bundle...", value = 0, {
      tryCatch(
        {
          result <- gen_import_bundle(
            path = file_info$datapath,
            include_setups = include_flags[["setups"]],
            include_agents = include_flags[["agents"]],
            include_content = include_flags[["content"]],
            include_models = include_flags[["models"]],
            models_dir = models_dir,
            overwrite = overwrite,
            quiet = TRUE
          )
          incProgress(1)
          transfer_state$import_summary <- list(
            timestamp = Sys.time(),
            counts = result$counts,
            includes = result$includes,
            metadata = result$metadata
          )
          if (include_flags[["setups"]]) refresh_setup_list()
          if (include_flags[["content"]]) refresh_content_list()
          if (include_flags[["agents"]]) refresh_agent_list()
          if (include_flags[["models"]]) {
            dir <- current_models_dir()
            new_catalog <- .load_models_catalog(dir)
            new_favorites <- .normalize_favorites(.load_favorites(dir), new_catalog)
            models_state$catalog <- new_catalog
            models_state$favorites <- new_favorites
            models_state$favorites_present <- nrow(new_favorites) > 0
            .save_favorites(new_favorites, dir)
            models_state$status <- paste0("Models reloaded after import at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
            updateTextInput(session, "models_directory", value = dir)
          }
          showNotification("Import completed successfully.", type = "message")
        },
        error = function(e) {
          transfer_state$import_summary <- list(
            timestamp = Sys.time(),
            error = conditionMessage(e)
          )
          showNotification(conditionMessage(e), type = "error")
        }
      )
    })
  })

  output$transfer_export_summary <- renderUI({
    summary <- transfer_state$export_summary
    if (is.null(summary)) {
      return(div(class = "gf-empty", "No export performed yet."))
    }
    counts_text <- format_transfer_counts(summary$counts)
    metadata <- summary$metadata
    exported_at <- ""
    pkg_version <- ""
    if (!is.null(metadata)) {
      exported_at <- metadata$exported_at %||% ""
      pkg_version <- metadata$package_version %||% ""
    }
    exported_at <- if (length(exported_at)) as.character(exported_at)[1] else ""
    pkg_version <- if (length(pkg_version)) as.character(pkg_version)[1] else ""
    tagList(
      div(
        class = "gf-preview",
        tags$strong("Last export ready for download."),
        tags$br(),
        tags$span(counts_text),
        if (nzchar(exported_at)) {
          tagList(tags$br(), tags$span(sprintf("Bundle created at %s", exported_at)))
        },
        if (nzchar(pkg_version)) {
          tagList(tags$br(), tags$span(sprintf("Source genflow version: %s", pkg_version)))
        }
      )
    )
  })

  output$transfer_import_summary <- renderUI({
    summary <- transfer_state$import_summary
    if (is.null(summary)) {
      return(div(class = "gf-empty", "No bundle imported yet."))
    }
    if (!is.null(summary$error)) {
      return(
        div(
          class = "gf-preview",
          tags$strong("Last import failed."),
          tags$br(),
          tags$span(summary$error)
        )
      )
    }
    counts_text <- format_transfer_counts(summary$counts)
    metadata <- summary$metadata
    exported_at <- ""
    pkg_version <- ""
    if (!is.null(metadata)) {
      exported_at <- metadata$exported_at %||% ""
      pkg_version <- metadata$package_version %||% ""
    }
    exported_at <- if (length(exported_at)) as.character(exported_at)[1] else ""
    pkg_version <- if (length(pkg_version)) as.character(pkg_version)[1] else ""
    tagList(
      div(
        class = "gf-preview",
        tags$strong("Last import completed."),
        tags$br(),
        tags$span(counts_text),
        if (nzchar(exported_at)) {
          tagList(tags$br(), tags$span(sprintf("Bundle created at %s", exported_at)))
        },
        if (nzchar(pkg_version)) {
          tagList(tags$br(), tags$span(sprintf("Source genflow version: %s", pkg_version)))
        }
      )
    )
  })

  output$agent_setup_extra_fields <- renderUI(.render_kv_fields(agent_state$setup_extras, "agent_setup_extra", input, session))
  output$agent_content_fields_ui <- renderUI(.render_content_fields(agent_state$custom_content_fields, "agent_content_field", input))
  output$agent_override_fields <- renderUI(.render_kv_fields(agent_state$overrides, "agent_override", input, session))
}

genflow_agent_app <- shinyApp(ui = ui, server = server)
