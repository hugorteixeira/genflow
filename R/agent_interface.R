library(shiny)
library(bslib)
library(readr)
library(jsonlite)

`%||%` <- function(x, y) if (!is.null(x)) x else y

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
  txt <- paste(text, collapse = "\n")
  txt <- trimws(txt)
  if (!nzchar(txt)) {
    return("(empty)")
  }
  if (nchar(txt) > max_chars) {
    paste0(substr(txt, 1, max_chars), "…")
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
    if (val %in% c("true", "t", "1", "yes")) return(TRUE)
    if (val %in% c("false", "f", "0", "no")) return(FALSE)
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
              textInput("setup_service", "Service"),
              textInput("setup_model", "Model"),
              textInput("setup_type", "Type"),
              numericInput("setup_temp", "Temperature", value = NA, min = 0, max = 5, step = 0.1)
            ),
            tags$hr(),
            div(
              class = "gf-section-header",
              h3("Additional fields"),
              actionButton("setup_add_field", "Add field", icon = icon("plus"))
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
                textInput("agent_setup_service", "Service"),
                textInput("agent_setup_model", "Model"),
                textInput("agent_setup_type", "Type"),
                numericInput("agent_setup_temp", "Temperature", value = NA, min = 0, max = 5, step = 0.1)
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
      .agents_tab_ui()
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
  extras <- setdiff(names(setup_list), base_fields)
  if (!length(extras)) return(list())
  lapply(extras, function(nm) {
    value <- setup_list[[nm]]
    mode <- "text"
    if (is.numeric(value)) mode <- "numeric"
    if (is.logical(value)) mode <- "logical"
    list(
      id = .rand_id("setup"),
      name = nm,
      value = if (is.atomic(value)) paste(value, collapse = "\n") else toJSON(value, auto_unbox = TRUE, pretty = TRUE),
      mode = mode,
      locked = FALSE
    )
  })
}

.build_override_fields <- function(agent_list, setup_fields = list(), content_fields = list()) {
  base_fields <- c("name", "sname", "cname")
  inherited <- c(names(setup_fields), names(content_fields))
  extras <- setdiff(names(agent_list), c(base_fields, inherited))
  if (!length(extras)) return(list())
  lapply(extras, function(nm) {
    value <- agent_list[[nm]]
    mode <- "text"
    if (is.numeric(value)) mode <- "numeric"
    if (is.logical(value)) mode <- "logical"
    list(
      id = .rand_id("override"),
      name = nm,
      value = if (is.atomic(value)) paste(value, collapse = "\n") else toJSON(value, auto_unbox = TRUE, pretty = TRUE),
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
    extras = list()
  )

  content_state <- reactiveValues(
    list = character(),
    selected = NULL,
    original = NULL,
    fields = .build_content_fields(list())
  )

  agent_state <- reactiveValues(
    list = character(),
    selected = NULL,
    original = NULL,
    setup_extras = list(),
    custom_content_fields = .build_content_fields(list()),
    overrides = list(),
    content_modal_field = NULL
  )

  refresh_setup_list <- function(selected = NULL) {
    choices <- sort(.load_setup_names())
    setup_state$list <- choices
    output$setup_list_ui <- renderUI({
      if (!length(choices)) {
        return(div(class = "gf-empty", "No setups saved yet."))
      }
      active <- isolate(setup_state$selected)
      tagList(lapply(choices, function(name) {
        summary_text <- ""
        suppressWarnings({
          details <- tryCatch(get_setup(name, assign = FALSE), error = function(e) NULL)
          if (!is.null(details)) {
            bits <- c(details$service, details$model, details$type)
            bits <- bits[nzchar(bits)]
            if (length(bits)) {
              summary_text <- paste(bits, collapse = " • ")
            }
          }
        })
        div(
          class = paste("gf-entity-item", if (!is.null(active) && active == name) "active"),
          onclick = sprintf(
            "Shiny.setInputValue('%s', {name: '%s', nonce: Math.random()}, {priority: 'event'});",
            "setup_select_trigger", name
          ),
          span(class = "gf-entity-name", name),
          if (nzchar(summary_text)) div(class = "gf-entity-summary", summary_text)
        )
      }))
    })
    updateSelectInput(session, "agent_setup_select", choices = c("", choices))
    if (!is.null(selected)) {
      setup_state$selected <- selected
    }
  }

  refresh_content_list <- function(selected = NULL) {
    choices <- sort(.load_content_names())
    content_state$list <- choices
    output$content_list_ui <- renderUI({
      if (!length(choices)) {
        return(div(class = "gf-empty", "No content saved yet."))
      }
      active <- isolate(content_state$selected)
      tagList(lapply(choices, function(name) {
        preview <- ""
        suppressWarnings({
          details <- tryCatch(get_content(name, assign = FALSE), error = function(e) NULL)
          if (!is.null(details)) {
            sample_values <- vapply(details, function(x) .format_preview(x, 80), character(1))
            preview <- paste(names(sample_values), sample_values, sep = ": ", collapse = " | ")
          }
        })
        div(
          class = paste("gf-entity-item", if (!is.null(active) && active == name) "active"),
          onclick = sprintf(
            "Shiny.setInputValue('%s', {name: '%s', nonce: Math.random()}, {priority: 'event'});",
            "content_select_trigger", name
          ),
          span(class = "gf-entity-name", name),
          if (nzchar(preview)) div(class = "gf-entity-summary", preview)
        )
      }))
    })
    updateSelectInput(session, "agent_content_select", choices = c("", choices))
    if (!is.null(selected)) {
      content_state$selected <- selected
    }
  }

  refresh_agent_list <- function(selected = NULL) {
    choices <- sort(.load_agent_names())
    agent_state$list <- choices
    output$agent_list_ui <- renderUI({
      if (!length(choices)) {
        return(div(class = "gf-empty", "No agents saved yet."))
      }
      active <- isolate(agent_state$selected)
      tagList(lapply(choices, function(name) {
        summary_text <- ""
        suppressWarnings({
          details <- tryCatch(get_agent(name, assign = FALSE), error = function(e) NULL)
          if (!is.null(details)) {
            bits <- c(details$service, details$model, details$sname, details$cname)
            bits <- bits[nzchar(bits)]
            if (length(bits)) summary_text <- paste(bits, collapse = " • ")
          }
        })
        div(
          class = paste("gf-entity-item", if (!is.null(active) && active == name) "active"),
          onclick = sprintf(
            "Shiny.setInputValue('%s', {name: '%s', nonce: Math.random()}, {priority: 'event'});",
            "agent_select_trigger", name
          ),
          span(class = "gf-entity-name", name),
          if (nzchar(summary_text)) div(class = "gf-entity-summary", summary_text)
        )
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
    updateTextInput(session, "setup_name", value = "")
    updateTextInput(session, "setup_service", value = "")
    updateTextInput(session, "setup_model", value = "")
    updateTextInput(session, "setup_type", value = "")
    updateNumericInput(session, "setup_temp", value = NA)
    setup_state$extras <- list()
    output$setup_extra_fields <- renderUI(.render_kv_fields(setup_state$extras, "setup_extra", input, session))
  }

  load_setup <- function(name) {
    if (is.null(name) || !nzchar(name)) return()
    setup <- tryCatch(get_setup(name, assign = FALSE), error = function(e) {
      showNotification(conditionMessage(e), type = "error")
      NULL
    })
    if (is.null(setup)) return()
    setup_state$selected <- name
    setup_state$original <- setup
    updateTextInput(session, "setup_name", value = setup$sname %||% name)
    updateTextInput(session, "setup_service", value = setup$service %||% "")
    updateTextInput(session, "setup_model", value = setup$model %||% "")
    updateTextInput(session, "setup_type", value = setup$type %||% "")
    updateNumericInput(session, "setup_temp", value = setup$temp %||% NA)
    setup_state$extras <- .build_setup_extras(setup)
    output$setup_extra_fields <- renderUI(.render_kv_fields(setup_state$extras, "setup_extra", input, session))
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
    reset_setup_form()
  })

  observeEvent(input$setup_duplicate, {
    if (is.null(setup_state$selected)) {
      showNotification("Select a setup to duplicate.", type = "warning")
      return()
    }
    load_setup(setup_state$selected)
    new_name <- paste0(setup_state$selected, "_copy")
    updateTextInput(session, "setup_name", value = new_name)
    setup_state$selected <- NULL
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
    if (is.null(name)) return()
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
    if (!nzchar(field_id)) return()
    extras <- setup_state$extras
    idx <- which(vapply(extras, `[[`, character(1), "id") == field_id)
    if (!length(idx)) return()
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
    service <- trimws(input$setup_service)
    model <- trimws(input$setup_model)
    if (!nzchar(service) || !nzchar(model)) {
      showNotification("Service and model are required.", type = "error")
      return()
    }
    temp <- input$setup_temp
    if (is.na(temp)) temp <- NULL
    type <- trimws(input$setup_type)
    if (!nzchar(type)) type <- NULL

    extras <- list()
    if (length(setup_state$extras)) {
      for (field in setup_state$extras) {
        name_id <- paste0("setup_extra_", field$id, "_name")
        field_name <- if (isTRUE(field$locked)) field$name else trimws(input[[name_id]] %||% field$name)
        if (!nzchar(field_name)) {
          showNotification("Extra field names cannot be empty.", type = "error")
          return()
        }
        mode_id <- paste0("setup_extra_", field$id, "_mode")
        value_id <- paste0("setup_extra_", field$id, "_value")
        mode <- input[[mode_id]] %||% field$mode
        value_raw <- input[[value_id]] %||% field$value
        if (mode %in% c("null", "na")) {
          extras[[field_name]] <- .convert_value("", mode)
        } else {
          tryCatch({
            extras[[field_name]] <- .convert_value(value_raw, mode)
          }, error = function(e) {
            showNotification(sprintf("Field '%s': %s", field_name, conditionMessage(e)), type = "error")
            stop("conversion_error")
          })
        }
      }
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
    if (is.null(res)) return()

    original_name <- setup_state$selected
    if (!is.null(original_name) && !identical(original_name, name)) {
      rm_setup(original_name)
    }

    setup_state$selected <- name
    setup_state$original <- res
    showNotification(sprintf("Setup '%s' saved.", name), type = "message")
    refresh_setup_list(selected = name)
  })

  output$setup_extra_fields <- renderUI(.render_kv_fields(setup_state$extras, "setup_extra", input, session))

  reset_content_fields <- function(content = list()) {
    content_state$fields <- .build_content_fields(content)
    output$content_fields_ui <- renderUI(.render_content_fields(content_state$fields, "content_field", input))
  }

  load_content <- function(name) {
    if (is.null(name) || !nzchar(name)) return()
    content <- tryCatch(get_content(name, assign = FALSE), error = function(e) {
      showNotification(conditionMessage(e), type = "error")
      NULL
    })
    if (is.null(content)) return()
    content_state$selected <- name
    content_state$original <- content
    updateTextInput(session, "content_name", value = name)
    reset_content_fields(content)
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
    updateTextInput(session, "content_name", value = "")
    reset_content_fields(list())
  })

  observeEvent(input$content_duplicate, {
    if (is.null(content_state$selected)) {
      showNotification("Select content to duplicate.", type = "warning")
      return()
    }
    load_content(content_state$selected)
    new_name <- paste0(content_state$selected, "_copy")
    updateTextInput(session, "content_name", value = new_name)
    content_state$selected <- NULL
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
    if (is.null(name)) return()
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
    if (!length(idx)) return()
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
      if (is.null(preview)) return()
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
    if (is.null(field_id)) return()
    upload <- input$content_modal_file
    if (is.null(upload) || !file.exists(upload$datapath)) {
      showNotification("No file selected.", type = "warning")
      return()
    }
    text <- tryCatch(read_file(upload$datapath), error = function(e) {
      showNotification(conditionMessage(e), type = "error")
      NULL
    })
    if (is.null(text)) return()
    fields <- content_state$fields
    idx <- which(vapply(fields, `[[`, character(1), "id") == field_id)
    if (!length(idx)) return()
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
    if (is.null(field_id)) return()
    path_value <- trimws(input$content_modal_path)
    if (!nzchar(path_value)) {
      showNotification("Path is empty.", type = "warning")
      return()
    }
    text <- tryCatch(.safe_read_text(path_value), error = function(e) {
      showNotification(conditionMessage(e), type = "error")
      NULL
    })
    if (is.null(text)) return()
    fields <- content_state$fields
    idx <- which(vapply(fields, `[[`, character(1), "id") == field_id)
    if (!length(idx)) return()
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
    if (is.null(field_id)) return()
    path_value <- trimws(input$content_modal_path)
    if (!nzchar(path_value)) {
      showNotification("Path is empty.", type = "warning")
      return()
    }
    fields <- content_state$fields
    idx <- which(vapply(fields, `[[`, character(1), "id") == field_id)
    if (!length(idx)) return()
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

    if (is.null(res)) return()
    original_name <- content_state$selected
    if (!is.null(original_name) && !identical(original_name, name)) {
      rm_content(original_name)
    }
    content_state$selected <- name
    content_state$original <- payload
    showNotification(sprintf("Content '%s' saved.", name), type = "message")
    refresh_content_list(selected = name)
  })

  output$content_fields_ui <- renderUI(.render_content_fields(content_state$fields, "content_field", input))

  load_agent <- function(name) {
    if (is.null(name) || !nzchar(name)) return()
    agent <- tryCatch(get_agent(name, assign = FALSE), error = function(e) {
      showNotification(conditionMessage(e), type = "error")
      NULL
    })
    if (is.null(agent)) return()
    agent_state$selected <- name
    agent_state$original <- agent
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
      updateTextInput(session, "agent_setup_service", value = agent$service %||% "")
      updateTextInput(session, "agent_setup_model", value = agent$model %||% "")
      updateTextInput(session, "agent_setup_type", value = agent$type %||% "")
      updateNumericInput(session, "agent_setup_temp", value = agent$temp %||% NA)
      setup_fields <- list(
        service = agent$service,
        model = agent$model,
        temp = agent$temp,
        type = agent$type
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
  }

  observeEvent(input$agent_select_trigger, {
    load_agent(input$agent_select_trigger$name)
  })

  observeEvent(input$agent_refresh, {
    refresh_agent_list()
  })

  observeEvent(input$agent_setup_select, {
    if (!identical(input$agent_setup_mode, "existing")) return()
    update_agent_setup_preview(trimws(input$agent_setup_select))
  })

  observeEvent(input$agent_setup_mode, {
    if (!identical(input$agent_setup_mode, "existing")) {
      output$agent_setup_preview <- renderUI(NULL)
    } else {
      isolate(update_agent_setup_preview(trimws(input$agent_setup_select)))
    }
  })

  observeEvent(input$agent_content_select, {
    if (!identical(input$agent_content_mode, "existing")) return()
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
    agent_state$selected <- NULL
    agent_state$original <- NULL
    updateTextInput(session, "agent_name", value = "")
    updateRadioButtons(session, "agent_setup_mode", selected = "existing")
    updateSelectInput(session, "agent_setup_select", selected = "")
    updateRadioButtons(session, "agent_content_mode", selected = "none")
    updateSelectInput(session, "agent_content_select", selected = "")
    agent_state$setup_extras <- list()
    agent_state$custom_content_fields <- .build_content_fields(list())
    agent_state$overrides <- list(
      list(id = .rand_id("override"), name = "label", value = "", mode = "text", locked = FALSE)
    )
    agent_state$content_modal_field <- NULL
    output$agent_setup_extra_fields <- renderUI(.render_kv_fields(agent_state$setup_extras, "agent_setup_extra", input, session))
    output$agent_content_fields_ui <- renderUI(.render_content_fields(agent_state$custom_content_fields, "agent_content_field", input))
    output$agent_override_fields <- renderUI(.render_kv_fields(agent_state$overrides, "agent_override", input, session))
  })

  observeEvent(input$agent_duplicate, {
    if (is.null(agent_state$selected)) {
      showNotification("Select an agent to duplicate.", type = "warning")
      return()
    }
    load_agent(agent_state$selected)
    new_name <- paste0(agent_state$selected, "_copy")
    updateTextInput(session, "agent_name", value = new_name)
    agent_state$selected <- NULL
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
    if (is.null(name)) return()
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
    if (!length(idx)) return()
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
    if (!length(idx)) return()
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
      if (is.null(preview)) return()
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
    if (is.null(field_id)) return()
    upload <- input$agent_content_modal_file
    if (is.null(upload) || !file.exists(upload$datapath)) {
      showNotification("No file selected.", type = "warning")
      return()
    }
    text <- tryCatch(read_file(upload$datapath), error = function(e) {
      showNotification(conditionMessage(e), type = "error")
      NULL
    })
    if (is.null(text)) return()
    fields <- agent_state$custom_content_fields
    idx <- which(vapply(fields, `[[`, character(1), "id") == field_id)
    if (!length(idx)) return()
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
    if (is.null(field_id)) return()
    path_value <- trimws(input$agent_content_modal_path)
    if (!nzchar(path_value)) {
      showNotification("Path is empty.", type = "warning")
      return()
    }
    text <- tryCatch(.safe_read_text(path_value), error = function(e) {
      showNotification(conditionMessage(e), type = "error")
      NULL
    })
    if (is.null(text)) return()
    fields <- agent_state$custom_content_fields
    idx <- which(vapply(fields, `[[`, character(1), "id") == field_id)
    if (!length(idx)) return()
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
    if (is.null(field_id)) return()
    path_value <- trimws(input$agent_content_modal_path)
    if (!nzchar(path_value)) {
      showNotification("Path is empty.", type = "warning")
      return()
    }
    fields <- agent_state$custom_content_fields
    idx <- which(vapply(fields, `[[`, character(1), "id") == field_id)
    if (!length(idx)) return()
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
    if (!length(idx)) return()
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
      agent_state$selected <- NULL
      agent_state$original <- NULL
      updateTextInput(session, "agent_name", value = "")
      updateRadioButtons(session, "agent_setup_mode", selected = "existing")
      updateSelectInput(session, "agent_setup_select", selected = "")
      updateRadioButtons(session, "agent_content_mode", selected = "none")
      updateSelectInput(session, "agent_content_select", selected = "")
      agent_state$setup_extras <- list()
      agent_state$custom_content_fields <- .build_content_fields(list())
      agent_state$overrides <- list()
      output$agent_setup_extra_fields <- renderUI(.render_kv_fields(agent_state$setup_extras, "agent_setup_extra", input, session))
      output$agent_content_fields_ui <- renderUI(.render_content_fields(agent_state$custom_content_fields, "agent_content_field", input))
      output$agent_override_fields <- renderUI(.render_kv_fields(agent_state$overrides, "agent_override", input, session))
    }
  })

  observeEvent(input$agent_save, {
    name <- trimws(input$agent_name)
    if (!nzchar(name)) {
      showNotification("Agent name is required.", type = "error")
      return()
    }

    setup_mode <- input$agent_setup_mode
    content_mode <- input$agent_content_mode

    setup_argument <- NULL
    if (setup_mode == "existing") {
      setup_selected <- trimws(input$agent_setup_select)
      if (!nzchar(setup_selected)) {
        showNotification("Select a setup for the agent.", type = "error")
        return()
      }
      setup_argument <- setup_selected
    } else {
      service <- trimws(input$agent_setup_service)
      model <- trimws(input$agent_setup_model)
      if (!nzchar(service) || !nzchar(model)) {
        showNotification("Custom setup requires service and model.", type = "error")
        return()
      }
      custom_setup <- list(
        sname = NULL,
        service = service,
        model = model,
        temp = if (is.na(input$agent_setup_temp)) NULL else input$agent_setup_temp,
        type = {
          x <- trimws(input$agent_setup_type)
          if (!nzchar(x)) NULL else x
        }
      )
      if (length(agent_state$setup_extras)) {
        for (field in agent_state$setup_extras) {
          name_id <- paste0("agent_setup_extra_", field$id, "_name")
          field_name <- if (isTRUE(field$locked)) field$name else trimws(input[[name_id]] %||% field$name)
          if (!nzchar(field_name)) {
            showNotification("Custom setup field names cannot be empty.", type = "error")
            return()
          }
          mode <- input[[paste0("agent_setup_extra_", field$id, "_mode")]] %||% field$mode
          value_raw <- input[[paste0("agent_setup_extra_", field$id, "_value")]] %||% field$value
          if (mode %in% c("null", "na")) {
            custom_setup[[field_name]] <- .convert_value("", mode)
          } else {
            tryCatch({
              custom_setup[[field_name]] <- .convert_value(value_raw, mode)
            }, error = function(e) {
              showNotification(sprintf("Custom setup '%s': %s", field_name, conditionMessage(e)), type = "error")
              stop("conversion_error")
            })
          }
        }
      }
      setup_argument <- custom_setup
    }

    content_argument <- NULL
    if (content_mode == "existing") {
      content_selected <- trimws(input$agent_content_select)
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
    if (length(agent_state$overrides)) {
      for (field in agent_state$overrides) {
        name_id <- paste0("agent_override_", field$id, "_name")
        field_name <- if (isTRUE(field$locked)) field$name else trimws(input[[name_id]] %||% field$name)
        if (!nzchar(field_name)) {
          showNotification("Override field names cannot be empty.", type = "error")
          return()
        }
        mode <- input[[paste0("agent_override_", field$id, "_mode")]] %||% field$mode
        value_raw <- input[[paste0("agent_override_", field$id, "_value")]] %||% field$value
        if (mode %in% c("null", "na")) {
          overrides[[field_name]] <- .convert_value("", mode)
        } else {
          tryCatch({
            overrides[[field_name]] <- .convert_value(value_raw, mode)
          }, error = function(e) {
            showNotification(sprintf("Override '%s': %s", field_name, conditionMessage(e)), type = "error")
            stop("conversion_error")
          })
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
    if (is.null(res)) return()

    original <- agent_state$selected
    if (!is.null(original) && !identical(original, name)) {
      rm_agent(original)
    }

    agent_state$selected <- name
    agent_state$original <- res
    showNotification(sprintf("Agent '%s' saved.", name), type = "message")
    refresh_agent_list(selected = name)
  })

  output$agent_setup_extra_fields <- renderUI(.render_kv_fields(agent_state$setup_extras, "agent_setup_extra", input, session))
  output$agent_content_fields_ui <- renderUI(.render_content_fields(agent_state$custom_content_fields, "agent_content_field", input))
  output$agent_override_fields <- renderUI(.render_kv_fields(agent_state$overrides, "agent_override", input, session))
}

shinyApp(ui = ui, server = server)
