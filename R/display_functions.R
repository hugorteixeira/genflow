#' Guess MIME type from file extension (internal)
#'
#' @param filepath Character path.
#' @return A best-effort MIME type string.
#' @keywords internal
#' @noRd
.get_mime_type <- function(filepath) {
  # ... (the .get_mime_type function implementation remains the same as in the previous version) ...
  if (!is.character(filepath) || length(filepath) != 1 || !nzchar(filepath)) {
    return("application/octet-stream") # Generic default
  }
  ext <- tolower(tools::file_ext(filepath))
  switch(ext,
         # Images
         "png"  = "image/png",
         "jpg"  = "image/jpeg",
         "jpeg" = "image/jpeg",
         "gif"  = "image/gif",
         "webp" = "image/webp",
         "svg"  = "image/svg+xml",
         # Videos
         "mp4"  = "video/mp4",
         "webm" = "video/webm",
         "ogg"  = "video/ogg", # Can also be audio/ogg
         "mov"  = "video/quicktime",
         "avi"  = "video/x-msvideo",
         # Audio
         "mp3"  = "audio/mpeg",
         "wav"  = "audio/wav",
         "aac"  = "audio/aac",
         "oga"  = "audio/ogg",
         # text/Outros
         "txt"  = "text/plain",
         "csv"  = "text/csv",
         "html" = "text/html",
         "json" = "application/json",
         # Default
         "application/octet-stream"
  )
}
#' Render plain text or object as HTML (internal)
#'
#' @keywords internal
#' @noRd
.gen_view_text <- function(value) {
  # ... (the .gen_view_text function implementation remains the same) ...
  content_html <- ""
  if (is.null(value)) {
    content_html <- "<pre><i>(NULL content)</i></pre>"
  } else if (is.character(value)) {
    # If it's a file path not recognized as media, show the path
    is_potential_path <- length(value) == 1 && grepl("\\.", value) && !grepl("^data:", value) # Evita tratar data URI como path
    if (is_potential_path && file.exists(value) && !grepl("\\.(png|jpe?g|gif|webp|svg|mp4|webm|ogg|mov|avi|mp3|wav|aac)$", value, ignore.case = TRUE)) {
      content_html <- paste0("<p><i>File (text/Other):</i><br/><code>", htmltools::htmlEscape(value), "</code></p>")
    } else {
      # Otherwise, treat as normal text
      formatted_text <- htmltools::htmlEscape(paste(value, collapse = "\n")) # Garante q vetores de char sejam juntados
      content_html <- paste0("<pre>", ifelse(nzchar(formatted_text), formatted_text, "<i>(Empty content)</i>"), "</pre>")
    }
  } else {
    # Para outros tipos (listas, data.frames, etc.)
    content_html <- paste0("<p><i>Non-textual result:</i></p><pre>", htmltools::htmlEscape(paste(capture.output(print(value)), collapse="\n")), "</pre>")
  }
  return(content_html)
}
#' Render a video as HTML (internal)
#'
#' @keywords internal
#' @noRd
.gen_view_video <- function(filepath) {
  #-----------------------------------------------------
  # 1) Unwrap list(vid) or objects with $saved_file
  #-----------------------------------------------------
  if (is.list(filepath)) {
    if (length(filepath) == 1) {
      filepath <- filepath[[1]]
    }
    if (is.list(filepath) && !is.null(filepath$saved_file)) {
      filepath <- filepath$saved_file
    } else if (is.list(filepath) && !is.null(filepath$response_value)) {
      filepath <- filepath$response_value
    }
  }

  #-----------------------------------------------------
  # 2) Basic validation
  #-----------------------------------------------------
  if (!is.character(filepath) ||
      length(filepath) != 1 ||
      !nzchar(filepath)) {
    return("<p class='error-message'>Invalid video path.</p>")
  }
  filepath <- tryCatch(
    normalizePath(filepath, winslash = "/", mustWork = FALSE),
    error = function(e) filepath
  )
  if (!file.exists(filepath)) {
    return(paste0(
      "<p class='error-message'>Video file not found:</p>",
      "<code>", htmltools::htmlEscape(filepath), "</code>"
    ))
  }

  #-----------------------------------------------------
  # 3) MIME???type
  #-----------------------------------------------------
  mime_type <- .get_mime_type(filepath)
  if (!grepl("^video/", mime_type)) {
    return(paste0(
      "<p class='error-message'>File does not appear to be a supported video:</p>",
      "<code>", htmltools::htmlEscape(filepath), "</code>"
    ))
  }

  #-----------------------------------------------------
  # 4) Converter p/ Base64 (fallback file:///)
  #-----------------------------------------------------
  video_src <- tryCatch(
    base64enc::dataURI(file = filepath, mime = mime_type),
    error = function(e) {
      warning(".gen_view_video: falha ao codificar Base64, usando file:///: ", e$message)
      paste0("file:///", filepath)
    }
  )

  #-----------------------------------------------------
  # 5) Montar a tag <video>
  #-----------------------------------------------------
  style <- "max-width:100%;height:auto;display:block;margin-top:5px;"
  content_html <- paste0(
    "<video controls style=\"", style, "\"",
    " title=\"", htmltools::htmlEscape(basename(filepath)), "\">",
    "<source src=\"", htmltools::htmlEscape(video_src),
    "\" type=\"", htmltools::htmlEscape(mime_type), "\">",
    "Your browser does not support this video or failed to load it.",
    "</video>"
  )

  #-----------------------------------------------------
  # 6) Detectar se vem de dentro de gen_view()
  #-----------------------------------------------------
  chamadas <- sys.calls()
  dentro_main <- any(sapply(chamadas, function(cc) {
    grepl("gen_view\\(", deparse(cc))
  }))
  if (dentro_main) {
    # only return the snippet for grid assembly
    return(content_html)
  }

  #-----------------------------------------------------
  # 7) Called directly: wrap in a page and open Viewer
  #-----------------------------------------------------
  full_page <- paste0(
    "<!DOCTYPE html><html><head><meta charset='UTF-8'></head><body>",
    content_html,
    "</body></html>"
  )
  tmpf <- tempfile(fileext = ".html")
  writeLines(full_page, tmpf, useBytes = TRUE)
  if (interactive() &&
      requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {
    rstudioapi::viewer(tmpf)
  } else {
    message("Video HTML generated at: ", tmpf)
  }

  invisible(content_html)
}
#' Render an audio file as HTML (internal)
#'
#' @keywords internal
#' @noRd
.gen_view_audio <- function(filepath) {
  #-----------------------------------------------------
  # 1) Unwrap list(file) or objects with $saved_file
  #-----------------------------------------------------
  if (is.list(filepath)) {
    # se for lista de comprimento 1, take the single element
    if (length(filepath) == 1) {
      filepath <- filepath[[1]]
    }
    # se vier com campo saved_file ou response_value
    if (is.list(filepath) && !is.null(filepath$saved_file)) {
      filepath <- filepath$saved_file
    } else if (is.list(filepath) && !is.null(filepath$response_value)) {
      filepath <- filepath$response_value
    }
  }

  #-----------------------------------------------------
  # 2) # 2) Basic path validation
  #-----------------------------------------------------
  if (!is.character(filepath) ||
      length(filepath) != 1 ||
      !nzchar(filepath)) {
    return("<p class='error-message'>Invalid audio path.</p>")
  }
  caminho_norm <- tryCatch(
    normalizePath(filepath, winslash = "/", mustWork = FALSE),
    error = function(e) filepath
  )
  if (!file.exists(caminho_norm)) {
    return(paste0(
      "<p class='error-message'>Audio file not found:</p>",
      "<code>", htmltools::htmlEscape(caminho_norm), "</code>"
    ))
  }

  #-----------------------------------------------------
  # 3) MIME???type
  #-----------------------------------------------------
  mime_type <- .get_mime_type(caminho_norm)
  # we accept audio/* or video/ogg (which is audio)
  if (!grepl("^audio/", mime_type) && mime_type != "video/ogg") {
    return(paste0(
      "<p class='error-message'>File does not appear to be a supported audio file:</p>",
      "<code>", htmltools::htmlEscape(caminho_norm), "</code>"
    ))
  }
  if (mime_type == "video/ogg") mime_type <- "audio/ogg"

  #-----------------------------------------------------
  # 4) Converter p/ Base64 (fallback file:///)
  #-----------------------------------------------------
  audio_src <- tryCatch(
    base64enc::dataURI(file = caminho_norm, mime = mime_type),
    error = function(e) {
      warning(".gen_view_audio: falha ao codificar Base64, usando file:///: ", e$message)
      paste0("file:///", caminho_norm)
    }
  )

  #-----------------------------------------------------
  # 5) Montar a tag <audio>
  #-----------------------------------------------------
  style <- "max-width:100%;height:auto;display:block;margin-top:5px;"
  content_html <- paste0(
    "<audio controls style=\"", style, "\"",
    " title=\"", htmltools::htmlEscape(basename(caminho_norm)), "\">",
    "<source src=\"", htmltools::htmlEscape(audio_src), "\"",
    " type=\"", htmltools::htmlEscape(mime_type), "\">",
    "Your browser does not support the audio element or failed to load it.",
    "</audio>"
  )

  #-----------------------------------------------------
  # 6) Detectar se vem de dentro de gen_view()
  #-----------------------------------------------------
  chamadas <- sys.calls()
  dentro_main <- any(sapply(chamadas, function(cc) {
    grepl("gen_view\\(", deparse(cc))
  }))
  if (dentro_main) {
    # only return the snippet for grid assembly
    return(content_html)
  }

  #-----------------------------------------------------
  # 7) Chamado direto: empacotar page e abrir Viewer
  #-----------------------------------------------------
  full_page <- paste0(
    "<!DOCTYPE html><html><head><meta charset='UTF-8'></head><body>",
    content_html,
    "</body></html>"
  )
  tmpf <- tempfile(fileext = ".html")
  writeLines(full_page, tmpf, useBytes = TRUE)

  if (interactive() &&
      requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {
    rstudioapi::viewer(tmpf)
  } else {
    message("Audio HTML generated at: ", tmpf)
  }

  invisible(content_html)
}
#' Format byte size as human-readable string (internal)
#'
#' @keywords internal
#' @noRd
.format_file_size <- function(size_bytes) {
  if (is.na(size_bytes)) return("N/A")
  if (size_bytes < 1024) {
    return(paste(size_bytes, "B"))
  } else if (size_bytes < 1024^2) {
    return(paste0(round(size_bytes / 1024, 1), " KB"))
  } else if (size_bytes < 1024^3) {
    return(paste0(round(size_bytes / (1024^2), 1), " MB"))
  } else {
    return(paste0(round(size_bytes / (1024^3), 1), " GB"))
  }
}
.flatten_results_for_view <- function(input, input_labels = NULL) {
  if (!is.list(input)) input <- list(input)
  collected_results <- list()
  origin_labels <- character(0)
  stats_blocks <- list()
  batch_ids <- character(0)
  batch_meta <- list()
  input_names <- names(input)
  get_label_hint <- function(idx) {
    lbl <- NULL
    if (!is.null(input_labels) && length(input_labels) >= idx) {
      cur <- input_labels[[idx]]
      if (!is.null(cur) && nzchar(cur)) lbl <- cur
    }
    if (is.null(lbl) && !is.null(input_names) && length(input_names) >= idx) {
      cur <- input_names[[idx]]
      if (!is.null(cur) && nzchar(cur)) lbl <- cur
    }
    lbl
  }
  add_result <- function(item, label_hint = NULL, batch_id = NA_character_) {
    collected_results[[length(collected_results) + 1]] <<- item
    origin_labels[length(origin_labels) + 1] <<- label_hint %||% ""
    batch_ids <<- c(batch_ids, if (!is.na(batch_id) && nzchar(batch_id)) batch_id else NA_character_)
  }
  add_stats <- function(stats_obj, label_hint = NULL, batch_id = NA_character_) {
    label_use <- NULL
    if (!is.null(label_hint) && nzchar(label_hint)) {
      label_use <- paste0(label_hint, " (combined stats)")
    }
    stats_blocks[[length(stats_blocks) + 1]] <<- list(
      label = label_use %||% paste0("Batch ", length(stats_blocks) + 1),
      data = stats_obj,
      category = NULL,
      batch_id = if (!is.na(batch_id) && nzchar(batch_id)) batch_id else NULL
    )
  }
  for (idx in seq_along(input)) {
    item <- input[[idx]]
    label_hint <- get_label_hint(idx)
    if (is.list(item) && length(item) > 0) {
      item_names <- names(item)
      stats_pos <- if (!is.null(item_names)) which(item_names == "combined_stats") else integer(0)
      if (length(stats_pos) > 0) {
        batch_id <- paste0("batch_", length(batch_meta) + 1L)
        batch_meta[[batch_id]] <- list(
          label = label_hint %||% paste0("Batch ", length(batch_meta) + 1L)
        )
        stats_obj <- item[[stats_pos[1]]]
        add_stats(stats_obj, label_hint, batch_id)
        result_indices <- setdiff(seq_along(item), stats_pos[1])
        if (length(result_indices) > 0) {
          for (j in result_indices) add_result(item[[j]], label_hint, batch_id)
        }
        next
      }
    }
    add_result(item, label_hint)
  }
  list(results = collected_results, stats = stats_blocks, origins = origin_labels, batch_ids = batch_ids, batches = batch_meta)
}
.infer_top_labels <- function(expr, input_list) {
  input_len <- length(input_list)
  if (input_len == 0) return(character(0))
  current_names <- names(input_list)
  if (!is.null(current_names) && any(nzchar(current_names))) return(current_names %||% rep("", input_len))
  labels <- rep("", input_len)
  if (missing(expr) || is.null(expr)) return(labels)
  if (is.name(expr)) {
    lbl <- as.character(expr)
    return(rep(lbl, input_len))
  }
  if (is.call(expr)) {
    fname <- as.character(expr[[1]])
    if (fname %in% c("c", "list")) {
      args <- as.list(expr)[-1]
      arg_names <- names(expr)
      arg_names <- if (!is.null(arg_names)) arg_names[-1] else NULL
      lbls <- character(length(args))
      for (i in seq_along(args)) {
        lbl <- ""
        if (!is.null(arg_names) && nzchar(arg_names[i])) {
          lbl <- arg_names[i]
        } else {
          arg <- args[[i]]
          if (is.name(arg)) {
            lbl <- as.character(arg)
          } else if (is.character(arg) && length(arg) == 1 && nzchar(arg)) {
            lbl <- arg
          }
        }
        if (!nzchar(lbl)) lbl <- paste0("Item ", i)
        lbls[i] <- lbl
      }
      if (length(lbls) == input_len) return(lbls)
    } else if (fname == "(" && length(expr) == 2) {
      return(.infer_top_labels(expr[[2]], input_list))
    }
  }
  labels
}
.sanitize_stat_value <- function(value) {
  if (is.null(value)) return(NULL)
  if (is.list(value) && !is.atomic(value)) return(NULL)
  if (is.atomic(value)) {
    if (length(value) == 0) return(NULL)
    if (is.logical(value)) value <- if (length(value) == 1) as.character(value) else value
    if (is.character(value)) {
      collapsed <- trimws(paste(value, collapse = ", "))
      if (!nzchar(collapsed)) return(NULL)
      return(collapsed)
    }
    if (is.numeric(value)) {
      if (length(value) == 1) return(round(value, 4))
      return(round(value, 4))
    }
    return(value)
  }
  NULL
}
.collect_result_stats <- function(results_list, origin_labels = NULL, item_categories = NULL, batch_ids = NULL) {
  if (!is.list(results_list) || length(results_list) == 0) return(list())
  candidate_fields <- c(
    "status_api", "status_msg", "service", "model",
    "temp", "steps", "duration", "tokens_sent",
    "tokens_received", "dimensoes"
  )
  stats_blocks <- list()
  for (i in seq_along(results_list)) {
    res <- results_list[[i]]
    if (!is.list(res)) next
    stats_data <- list()
    for (field in candidate_fields) {
      raw_value <- res[[field]]
      sanitized <- .sanitize_stat_value(raw_value)
      if (!is.null(sanitized)) {
        stats_data[[field]] <- sanitized
      }
    }
    if (length(stats_data) == 0) next
    origin_label <- if (!is.null(origin_labels) && length(origin_labels) >= i) origin_labels[[i]] else NULL
    if (is.character(origin_label) && (length(origin_label) == 0 || !nzchar(origin_label))) origin_label <- NULL
    if (is.na(origin_label)) origin_label <- NULL
    item_category <- if (!is.null(item_categories) && length(item_categories) >= i) item_categories[[i]] else "list"
    item_batch_id <- if (!is.null(batch_ids) && length(batch_ids) >= i) batch_ids[[i]] else NA_character_
    label_base <- res$label %||% origin_label %||% paste0("Item ", i)
    stats_blocks[[length(stats_blocks) + 1]] <- list(
      label = paste0(label_base, " (details)"),
      data = stats_data,
      category = item_category,
      index = i,
      batch_id = if (!is.na(item_batch_id) && nzchar(item_batch_id)) item_batch_id else NULL
    )
  }
  stats_blocks
}
#' Render an image as HTML with metadata (internal)
#'
#' @keywords internal
#' @noRd
.gen_view_image <- function(filepath) {
  # -------------------------------------------------------------------
  # 1) Unwrap a list(img) or an object with $saved_file / $response_value
  # -------------------------------------------------------------------
  if (is.list(filepath)) {
    if (length(filepath) == 1) {
      filepath <- filepath[[1]]
    }
    if (is.list(filepath) && !is.null(filepath$saved_file)) {
      filepath <- filepath$saved_file
    } else if (is.list(filepath) && !is.null(filepath$response_value)) {
      filepath <- filepath$response_value
    }
  }

  # -------------------------------------------------------------------
  # 2) Validate that it is now a simple string and that the file exists
  # -------------------------------------------------------------------
  if (!is.character(filepath) ||
      length(filepath) != 1 ||
      !nzchar(filepath)) {
    error_html <- "<p class='error-message'>Invalid image path.</p>"
    return(list(html = error_html, metadata_str = NULL, error = TRUE))
  }
  if (!file.exists(filepath)) {
    error_html <- paste0(
      "<p class='error-message'>Image file not found:</p>",
      "<code>", htmltools::htmlEscape(filepath), "</code>"
    )
    return(list(html = error_html, metadata_str = NULL, error = TRUE))
  }
  filepath_norm <- tryCatch(
    normalizePath(filepath, winslash = "/", mustWork = FALSE),
    error = function(e) filepath
  )

  # -------------------------------------------------------------------
  # 3) Check the MIME type???type
  # -------------------------------------------------------------------
  mime_type <- .get_mime_type(filepath)
  if (!grepl("^image/", mime_type)) {
    error_html <- paste0(
      "<p class='error-message'>File does not appear to be a supported image:</p>",
      "<code>", htmltools::htmlEscape(filepath), "</code>"
    )
    return(list(html = error_html, metadata_str = NULL, error = TRUE))
  }

  # -------------------------------------------------------------------
  # 4) Extract metadata: size and dimensions
  # -------------------------------------------------------------------
  metadata_pieces <- list()

  # tamanho em bytes
  file_size_bytes <- tryCatch(
    file.info(filepath)$size,
    error = function(e) NA
  )
  if (!is.na(file_size_bytes)) {
    metadata_pieces$size <- .format_file_size(file_size_bytes)
  }

  # dimensions (if magick available)
  if (requireNamespace("magick", quietly = TRUE)) {
    img_info <- tryCatch({
      img <- magick::image_read(filepath)
      magick::image_info(img)[1, c("width","height")]
    }, error = function(e) NULL)

    if (!is.null(img_info) && !any(is.na(img_info))) {
      metadata_pieces$dims <- paste0(img_info$width, "x", img_info$height, " px")
    }
  } else {
    warning(".gen_view_image: package 'magick' not available. Skipping dimensions.")
  }

  combined_metadata_str <- paste(
    metadata_pieces[c("dims","size")],
    collapse = " | "
  )
  if (!nzchar(trimws(combined_metadata_str))) combined_metadata_str <- NULL

  # -------------------------------------------------------------------
  # 5) Codifica em base64 e monta a tag <img>
  # -------------------------------------------------------------------
  fallback_src <- paste0("file:///", filepath_norm)
  img_src <- tryCatch(
    base64enc::dataURI(file = filepath, mime = mime_type),
    error = function(e) {
      warning(".gen_view_image: failed to encode Base64, using file:///: ", e$message)
      fallback_src
    }
  )

  alt_text <- paste("Image:", basename(filepath))
  content_html <- paste0(
    "<img src=\"", img_src,
    "\" alt=\"", htmltools::htmlEscape(alt_text),
    "\" class=\"zoomable-image\" ",
    "style=\"max-width:100%;height:auto;cursor:pointer\"/>"
  )

  result <- list(
    html         = content_html,
    metadata_str = combined_metadata_str,
    error        = FALSE
  )

  inside_gen_view <- any(vapply(sys.calls(), function(cc) {
    grepl("gen_view\\(", deparse(cc, nlines = 1), fixed = TRUE)
  }, logical(1)))
  if (inside_gen_view) {
    return(invisible(result))
  }

  # -------------------------------------------------------------------
  # 6) Build a minimal HTML page and display in RStudio Viewer
  # -------------------------------------------------------------------
  full_page <- paste0(
    "<!DOCTYPE html><html><head><meta charset='UTF-8'></head><body>",
    content_html,
    "</body></html>"
  )
  tmpf <- tempfile(fileext = ".html")
  writeLines(full_page, tmpf, useBytes = TRUE)
  if (interactive() &&
      requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {
    rstudioapi::viewer(tmpf)
  } else {
    message("Image HTML generated at: ", tmpf)
  }

  # -------------------------------------------------------------------
  # 7) Invisibly return the original data (in case it's needed)
  # -------------------------------------------------------------------
  invisible(result)
}


#' Visualize a list of generation results
#'
#' Displays generated images, videos, audios, and text in the RStudio Viewer.
#' Falls back to a readable console output when Viewer is unavailable.
#'
#' @param ... Optional individual generation results, plain vectors, or lists to
#'   display. Mixed inputs are flattened automatically.
#' @param results_generated Optional legacy argument accepting a list of results
#'   (such as the return value from `gen_batch()`). Used when `...` is empty.
#' @param grouped Logical; when TRUE (default) results and statistics are grouped by result type.
#' @param stats Character; choose where statistics are displayed. Use `"console"`
#'   (default) to print stats in the console while hiding them in the Viewer,
#'   `"viewer"` to render them inside the Viewer only, or `"hide"` to suppress
#'   stats entirely.
#' @return Invisibly returns NULL. Opens the Viewer or prints to console.
#'
#' @examples
#' # gen_view(list(list(status_api = "SUCCESS", response_value = tempfile(fileext=".png"))))
#'
#' @export
gen_view <- function(..., results_generated = NULL, grouped = TRUE,
                     stats = c("console", "viewer", "hide")) {

  call <- match.call(expand.dots = FALSE)
  dots_expr <- call$...
  expr_list <- if (is.null(dots_expr)) list() else as.list(dots_expr)
  expr_names <- if (is.null(dots_expr)) character(0) else names(dots_expr)
  stats <- match.arg(stats)

  args <- list(...)
  if (!missing(results_generated)) {
    args <- c(list(results_generated), args)
    expr_list <- c(list(substitute(results_generated)), expr_list)
    expr_names <- c("", expr_names)
  }

  if (length(args) == 0) {
    warning("gen_view: nothing to display.")
    return(invisible(NULL))
  }

  top_inputs <- list()
  top_labels <- character(0)

  append_input <- function(value, expr, name_hint) {
    lbl <- ""
    if (!is.null(name_hint) && nzchar(name_hint)) {
      lbl <- name_hint
    } else if (!is.null(expr)) {
      if (is.name(expr)) {
        lbl <- as.character(expr)
      } else if (is.character(expr) && length(expr) == 1 && nzchar(expr)) {
        lbl <- expr
      }
    }
    top_inputs[[length(top_inputs) + 1]] <<- value
    top_labels <<- c(top_labels, lbl)
  }

  process_arg <- function(value, expr, name_hint) {
    if (is.list(value) && !inherits(value, "data.frame") && !inherits(value, "matrix") &&
        !is.null(expr) && is.call(expr)) {
      fname <- as.character(expr[[1]])
      if (fname %in% c("list", "c")) {
        inner_exprs <- as.list(expr)[-1]
        inner_names <- names(expr)
        inner_names <- if (!is.null(inner_names)) inner_names[-1] else rep("", length(inner_exprs))
        if (length(inner_exprs) == length(value)) {
          for (j in seq_along(value)) {
            process_arg(value[[j]], inner_exprs[[j]], if (nzchar(inner_names[j])) inner_names[j] else NULL)
          }
          return()
        }
      }
    }
    append_input(value, expr, name_hint)
  }

  for (idx in seq_along(args)) {
    expr <- if (length(expr_list) >= idx) expr_list[[idx]] else NULL
    name_hint <- if (length(expr_names) >= idx) expr_names[[idx]] else NULL
    process_arg(args[[idx]], expr, name_hint)
  }

  input_list <- top_inputs
  if (length(top_labels) < length(input_list)) {
    top_labels <- c(top_labels, rep("", length(input_list) - length(top_labels)))
  }

  if (any(nzchar(top_labels))) {
    current_names <- names(input_list)
    if (is.null(current_names)) current_names <- rep("", length(input_list))
    if (length(current_names) < length(input_list)) {
      current_names <- c(current_names, rep("", length(input_list) - length(current_names)))
    }
    for (i in seq_along(input_list)) {
      if (!nzchar(current_names[i]) && nzchar(top_labels[i])) current_names[i] <- top_labels[i]
    }
    names(input_list) <- current_names
  }

  flattened <- .flatten_results_for_view(input_list, top_labels)
  results_list <- flattened$results
  stats_blocks <- flattened$stats
  origin_labels <- flattened$origins
  batch_ids <- flattened$batch_ids %||% character(0)
  batch_meta <- flattened$batches %||% list()

  if (length(batch_ids) == 0 && length(results_list) > 0) {
    batch_ids <- rep(NA_character_, length(results_list))
  } else if (length(batch_ids) < length(results_list)) {
    batch_ids <- c(batch_ids, rep(NA_character_, length(results_list) - length(batch_ids)))
  }

  extract_first_string <- function(value, depth = 0L) {
    if (depth > 4) return(NULL)
    if (is.null(value)) return(NULL)
    if (is.character(value) && length(value) >= 1) return(value[[1]])
    if (is.list(value) && length(value) > 0) {
      candidates <- c(
        value$saved_file,
        value$response_value,
        value$filepath,
        value$file,
        value$url
      )
      for (cand in candidates) {
        result <- extract_first_string(cand, depth + 1L)
        if (!is.null(result)) return(result)
      }
      if (length(value) == 1) {
        return(extract_first_string(value[[1]], depth + 1L))
      }
    }
    NULL
  }

  resolve_type_hint <- function(...) {
    hints <- list(...)
    for (hint in hints) {
      if (is.null(hint)) next
      if (is.character(hint) && length(hint) >= 1) {
        candidate <- tolower(hint[[1]])
        if (nzchar(candidate)) return(candidate)
      }
    }
    ""
  }

  classify_content_type <- function(item) {
    if (is.list(item)) {
      hint <- resolve_type_hint(item$content_type, item$type, item$response_type, item$output_type)
      if (grepl("image", hint, fixed = TRUE)) return("image")
      if (grepl("video", hint, fixed = TRUE) || grepl("mp4", hint, fixed = TRUE) || grepl("webm", hint, fixed = TRUE)) return("video")
      if (grepl("audio", hint, fixed = TRUE) || grepl("mp3", hint, fixed = TRUE) || grepl("wav", hint, fixed = TRUE)) return("audio")
      if (grepl("text", hint, fixed = TRUE) || grepl("json", hint, fixed = TRUE) || grepl("markdown", hint, fixed = TRUE) || grepl("html", hint, fixed = TRUE)) {
        return("text")
      }

      value <- item$response_value %||% item$saved_file %||% item$output %||% item$text %||% item$content
      path_candidate <- extract_first_string(value)
      if (!is.null(path_candidate)) {
        lower_val <- tolower(path_candidate)
        if (grepl("\\.(png|jpe?g|gif|webp|svg)$", lower_val)) return("image")
        if (grepl("\\.(mp4|webm|ogg|mov|avi)$", lower_val)) return("video")
        if (grepl("\\.(mp3|wav|aac|oga)$", lower_val)) return("audio")
      }

      if (is.character(value)) return("text")
      if (is.null(value)) return("text")
      if (is.list(value) && length(value) > 0 && all(vapply(value, is.character, logical(1)))) return("text")
      if (is.numeric(value) || is.logical(value)) return("text")
      return("other")
    }

    if (is.character(item) && length(item) >= 1) {
      lower_val <- tolower(item[[1]])
      if (grepl("\\.(png|jpe?g|gif|webp|svg)$", lower_val)) return("image")
      if (grepl("\\.(mp4|webm|ogg|mov|avi)$", lower_val)) return("video")
      if (grepl("\\.(mp3|wav|aac|oga)$", lower_val)) return("audio")
      return("text")
    }

    "other"
  }

  content_types <- if (length(results_list) > 0) {
    vapply(results_list, classify_content_type, character(1))
  } else {
    character(0)
  }

  primary_types <- c("text", "image", "video")
  base_types <- if (length(content_types) > 0) content_types else character(0)
  base_types[!(base_types %in% primary_types)] <- "other"
  is_batch_vec <- if (length(base_types) > 0) (!is.na(batch_ids) & nzchar(batch_ids)) else logical(0)

  final_categories <- base_types
  final_categories[is_batch_vec & base_types == "text"] <- "batch_text"
  final_categories[is_batch_vec & base_types == "image"] <- "batch_image"
  final_categories[is_batch_vec & base_types == "video"] <- "batch_video"

  if (length(batch_meta) > 0) {
    for (bid in names(batch_meta)) {
      idxs <- which(!is.na(batch_ids) & nzchar(batch_ids) & batch_ids == bid)
      if (length(idxs) == 0) next
      batch_type <- "other"
      for (ptype in primary_types) {
        if (any(base_types[idxs] == ptype)) {
          batch_type <- ptype
          break
        }
      }
      batch_meta[[bid]]$type <- batch_type
      batch_meta[[bid]]$first_index <- min(idxs)
    }
  }
  batch_type_map <- if (length(batch_meta) > 0) {
    vapply(batch_meta, function(x) x$type %||% "other", character(1))
  } else {
    character(0)
  }

  group_labels <- c(
    batch_text = "Text Batches",
    batch_image = "Image Batches",
    batch_video = "Video Batches",
    text = "Texts",
    image = "Images",
    video = "Videos",
    other = "Other"
  )

  item_categories <- final_categories

  detail_blocks <- .collect_result_stats(results_list, origin_labels, item_categories, batch_ids)
  if (length(detail_blocks) > 0) {
    stats_blocks <- c(stats_blocks, detail_blocks)
  }
  show_stats_console <- identical(stats, "console")
  show_stats_viewer <- identical(stats, "viewer")
  stats_blocks_for_view <- if (show_stats_viewer) stats_blocks else list()

  if (length(stats_blocks) > 0) {
    for (idx in seq_along(stats_blocks)) {
      entry <- stats_blocks[[idx]]
      if (!is.null(entry$index)) {
        item_idx <- entry$index
        entry$category <- if (length(item_categories) >= item_idx) item_categories[[item_idx]] else entry$category %||% "other"
        entry$batch_id <- if (length(batch_ids) >= item_idx && !is.na(batch_ids[[item_idx]]) && nzchar(batch_ids[[item_idx]])) batch_ids[[item_idx]] else entry$batch_id %||% NULL
      } else if (!is.null(entry$batch_id)) {
        batch_id <- entry$batch_id
        batch_type <- batch_type_map[[batch_id]] %||% "other"
        if (batch_type %in% primary_types) {
          entry$category <- paste0("batch_", batch_type)
        } else {
          entry$category <- "other"
        }
      } else {
        entry$category <- entry$category %||% "other"
      }
      stats_blocks[[idx]] <- entry
    }
  }

  viewer_available <- interactive() &&
    requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable() &&
    requireNamespace("htmltools", quietly = TRUE) &&
    requireNamespace("base64enc", quietly = TRUE)

  modal_html <- '
      <div id="imageModal" class="modal-overlay">
        <span class="modal-close" title="Close">&times;</span>
        <img class="modal-content" id="modalImage">
      </div>
    '

  modal_js <- "
      <script>
        document.addEventListener('DOMContentLoaded', function() {
          const modal = document.getElementById('imageModal');
          const modalImg = document.getElementById('modalImage');
          const closeBtn = modal.querySelector('.modal-close');
          const resultsContainer = document.getElementById('results-grid') || document.body;

          if (!modal || !modalImg || !closeBtn) {
             console.error('Modal elements not found!');
             return;
          }

          const openModal = (imgElement) => {
            modal.style.display = 'flex';
            modalImg.src = imgElement.src;
            modalImg.alt = imgElement.alt;
          };

          const closeModal = () => {
            modal.style.display = 'none';
            modalImg.src = '';
          };

          resultsContainer.addEventListener('click', function(event) {
            if (event.target.tagName === 'IMG' && event.target.classList.contains('zoomable-image')) {
              openModal(event.target);
            }
          });

          closeBtn.addEventListener('click', closeModal);
          modal.addEventListener('click', function(event) {
            if (event.target === modal) {
              closeModal();
            }
          });
          document.addEventListener('keydown', function(event) {
            if (event.key === 'Escape' && modal.style.display === 'flex') {
              closeModal();
            }
          });
        });
      </script>
    "

  if (!viewer_available) {
    if (show_stats_viewer && !show_stats_console) {
      cat("\nNOTE: Statistics are configured to display in the Viewer, but it is not available in this session.\n")
    }
    cat("\nWARNING: RStudio Viewer not available or session is not interactive.\n")
    cat("Showing results in console as fallback:\n")

    print_console_item <- function(i) {
      res_item <- results_list[[i]]
      origin_label <- origin_labels[i] %||% paste("Result", i)
      category <- if (length(item_categories) >= i) item_categories[[i]] else "other"
      category_base <- sub("^batch_", "", category)
      status_icon <- "???"
      label_info <- origin_label
      model_info_short <- "N/A"
      time_info <- "N/A"
      content_type_info <- "Data"
      extra_info <- ""

      if (is.list(res_item)) {
        if (!is.null(res_item$status_api)) status_icon <- ifelse(res_item$status_api == "SUCCESS", "[OK]", "[WARN]")
        label_info <- res_item$label %||% origin_label
        model_raw <- res_item$model %||% "?"
        if (grepl("/", model_raw)) model_info_short <- sub(".*/", "", model_raw) else model_info_short <- model_raw
        time_info <- paste0(round(res_item$duration %||% NA, 1), "s")
        if (!is.null(res_item$response_value) && is.character(res_item$response_value) && length(res_item$response_value) == 1) {
          path <- res_item$response_value
          if (grepl("\\.(png|jpe?g|gif|webp|svg)$", path, ignore.case = TRUE)) {
            content_type_info <- "Image"
            fsize <- tryCatch(file.info(path)$size, error = function(e) NA)
            if (!is.na(fsize)) extra_info <- paste0(" (", .format_file_size(fsize), ")")
          } else if (grepl("\\.(mp4|webm|ogg|mov|avi)$", path, ignore.case = TRUE)) {
            content_type_info <- "Video"
          } else if (grepl("\\.(mp3|wav|aac|oga)$", path, ignore.case = TRUE)) {
            content_type_info <- "Audio"
          } else if (grepl("\\.(txt|csv|json|html)$", path, ignore.case = TRUE)) {
            content_type_info <- "text/Doc"
          } else if (grepl("\\.", path) && file.exists(path)) {
            content_type_info <- "File"
          } else if (!grepl("\\.", path) || !file.exists(path)) {
            content_type_info <- "text"
          }
        } else if (is.character(res_item$response_value)) {
          content_type_info <- "text"
        } else if (!is.null(res_item$response_value)) {
          content_type_info <- paste("Value type:", class(res_item$response_value)[1])
        } else {
          content_type_info <- "Empty/NULL"
        }
      } else {
        status_icon <- "[INFO]"
        content_type_info <- switch(
          category_base,
          text = "text",
          image = "Image",
          video = "Video",
          audio = "Audio",
          other = paste("Value type:", class(res_item)[1]),
          "Data"
        )
      }

      cat("\n-- ", status_icon, " ", label_info, " (", model_info_short, " | ", time_info,
          " | Type: ", content_type_info, extra_info, ") --\n", sep = "")

      if (is.list(res_item) && !is.null(res_item$status_api)) {
        if (res_item$status_api == "SUCCESS") {
          cat("Content/File:\n")
          if (content_type_info %in% c("Image", "Video", "Audio", "File", "text/Doc")) {
            cat("  ", res_item$response_value, "\n")
          } else if (content_type_info == "text" && is.character(res_item$response_value) && length(res_item$response_value) == 1 && nchar(res_item$response_value) > 500) {
            cat("  ", substr(res_item$response_value, 1, 500), "... (truncated)\n")
          } else {
            if (is.list(res_item$response_value) || is.data.frame(res_item$response_value) || is.matrix(res_item$response_value) || length(res_item$response_value) > 1) {
              print(res_item$response_value)
            } else {
              cat("  ", as.character(res_item$response_value), "\n")
            }
          }
        } else {
          cat("Error Message:", res_item$status_msg %||% "(No details)", "\n")
        }
      } else if (is.list(res_item)) {
        print(res_item)
      } else if (is.null(res_item)) {
        cat("Invalid result structure.\n")
      } else if (is.character(res_item)) {
        preview <- paste(res_item, collapse = "\n")
        if (nchar(preview) > 500) preview <- paste0(substr(preview, 1, 500), "... (truncated)")
        cat("  ", preview, "\n", sep = "")
      } else {
        capture_out <- capture.output(print(res_item))
        cat(paste0("  ", capture_out), sep = "\n")
        cat("\n")
      }
      cat("------------------------------------\n")
    }

    print_stats_console <- function(stats_subset, header_label) {
      if (length(stats_subset) == 0) return()
      cat("\n-- ", header_label, " --\n", sep = "")
      for (idx in seq_along(stats_subset)) {
        block <- stats_subset[[idx]]
        block_label <- block$label %||% paste("Block", idx)
        cat(block_label, ":\n", sep = " ")
        stats_data <- block$data
        if (is.list(stats_data)) {
          stat_names <- names(stats_data)
          if (is.null(stat_names)) stat_names <- paste0("value_", seq_along(stats_data))
          for (nm_idx in seq_along(stats_data)) {
            val <- stats_data[[nm_idx]]
            if (is.numeric(val)) val <- round(val, 3)
            cat("  - ", stat_names[nm_idx], ": ", paste(val, collapse = ", "), "\n", sep = "")
          }
        } else {
          cat("  ", paste(stats_data, collapse = ", "), "\n", sep = "")
        }
      }
    }

    if (length(results_list) == 0) {
      if (show_stats_console && length(stats_blocks) > 0) {
        print_stats_console(stats_blocks, "Combined Statistics")
      } else {
        cat("(No individual result to display in console)\n")
      }
      return(invisible(NULL))
    }

    if (grouped) {
      item_categories_vec <- item_categories
      item_batch_vec <- if (length(batch_ids) >= length(item_categories_vec)) batch_ids[seq_along(item_categories_vec)] else rep(NA_character_, length(item_categories_vec))
      item_batch_vec <- ifelse(is.na(item_batch_vec) | !nzchar(item_batch_vec), NA_character_, item_batch_vec)

      collect_stats_subset <- function(item_idx, batch_id = NULL) {
        if (!show_stats_console) return(list())
        if (length(stats_blocks) == 0) return(list())
        subset <- Filter(function(entry) {
          entry_batch <- entry$batch_id
          if (is.null(entry_batch) || length(entry_batch) == 0) entry_batch <- NA_character_
          if (!is.null(batch_id)) {
            if (!is.na(entry_batch) && entry_batch == batch_id) return(TRUE)
            if (!is.null(entry$index) && entry$index %in% item_idx) return(TRUE)
            return(FALSE)
          }
          if (!is.null(entry$index)) {
            return(entry$index %in% item_idx)
          }
          is.na(entry_batch)
        }, stats_blocks)
        if (!is.null(batch_id) && length(subset) > 1) {
          agg_mask <- vapply(subset, function(entry) {
            entry_batch <- entry$batch_id
            !is.null(entry_batch) && length(entry_batch) > 0 && entry_batch == batch_id && is.null(entry$index)
          }, logical(1))
          subset <- c(subset[agg_mask], subset[!agg_mask])
        }
        subset
      }

      batch_category_order <- c("batch_text", "batch_image", "batch_video")
      for (cat in batch_category_order) {
        idx_cat <- which(item_categories_vec == cat)
        if (length(idx_cat) == 0) next
        cat_batch_ids <- unique(item_batch_vec[idx_cat])
        cat_batch_ids <- cat_batch_ids[!is.na(cat_batch_ids) & nzchar(cat_batch_ids)]
        if (length(cat_batch_ids) == 0) next
        order_metric <- vapply(cat_batch_ids, function(bid) {
          meta <- batch_meta[[bid]]
          if (!is.null(meta) && !is.null(meta$first_index)) {
            meta$first_index
          } else {
            min(which(item_batch_vec == bid))
          }
        }, numeric(1))
        cat_batch_ids <- cat_batch_ids[order(order_metric)]
        for (bid in cat_batch_ids) {
          batch_label <- batch_meta[[bid]]$label %||% paste0("Batch ", bid)
          cat("\n== ", batch_label, " (", group_labels[[cat]] %||% cat, ") ==\n", sep = "")
          item_idx <- which(item_categories_vec == cat & item_batch_vec == bid)
          for (idx in item_idx) print_console_item(idx)
          stats_subset <- collect_stats_subset(item_idx, batch_id = bid)
          if (show_stats_console && length(stats_subset) > 0) {
            print_stats_console(stats_subset, paste0(batch_label, " Stats"))
          }
        }
      }

      non_batch_categories <- c("text", "image", "video")
      for (cat in non_batch_categories) {
        item_idx <- which(item_categories_vec == cat & (is.na(item_batch_vec) | !nzchar(item_batch_vec)))
        stats_subset <- collect_stats_subset(item_idx)
        if (length(item_idx) == 0 && length(stats_subset) == 0) next
        header_label <- group_labels[[cat]] %||% cat
        cat("\n== ", header_label, " ==\n", sep = "")
        for (idx in item_idx) print_console_item(idx)
        if (show_stats_console && length(stats_subset) > 0) {
          print_stats_console(stats_subset, paste(header_label, "Stats"))
        }
      }

      other_idx <- which(item_categories_vec == "other")
      other_stats <- collect_stats_subset(other_idx)
      if (length(other_idx) > 0 || length(other_stats) > 0) {
        header_label <- group_labels[["other"]] %||% "Other"
        cat("\n== ", header_label, " ==\n", sep = "")
        for (idx in other_idx) print_console_item(idx)
        if (show_stats_console && length(other_stats) > 0) {
          print_stats_console(other_stats, paste(header_label, "Stats"))
        }
      }
    } else {
      cat("\n--- results (Console Fallback) ---\n")
      for (i in seq_along(results_list)) print_console_item(i)
      if (show_stats_console && length(stats_blocks) > 0) {
        print_stats_console(stats_blocks, "Combined Statistics")
      }
    }

    return(invisible(NULL))
  }
  num_elementos <- length(results_list)
  indices_results <- if (num_elementos > 0) seq_len(num_elementos) else integer(0)

  build_stats_blocks <- function(stats_subset) {
    if (length(stats_subset) == 0) return("")
    blocks <- vapply(seq_along(stats_subset), function(idx) {
      stat_entry <- stats_subset[[idx]]
      stat_label <- htmltools::htmlEscape(stat_entry$label %||% paste("Block", idx))
      stat_data <- stat_entry$data
      body_lines <- ""
      if (is.list(stat_data)) {
        stat_names <- names(stat_data)
        if (is.null(stat_names)) stat_names <- paste0("value_", seq_along(stat_data))
        entry_lines <- character(length(stat_data))
        for (k in seq_along(stat_data)) {
          value <- stat_data[[k]]
          if (is.numeric(value)) value <- round(value, 3)
          value_str <- htmltools::htmlEscape(paste(value, collapse = ", "))
          name_str <- htmltools::htmlEscape(gsub("_", " ", stat_names[k], fixed = TRUE))
          entry_lines[k] <- paste0("<dt>", name_str, "</dt><dd>", value_str, "</dd>")
        }
        body_lines <- paste(entry_lines, collapse = "")
      } else {
        value_str <- htmltools::htmlEscape(paste(stat_data, collapse = ", "))
        body_lines <- paste0("<dt>value</dt><dd>", value_str, "</dd>")
      }
      paste0(
        "<article class='stats-card'>",
        "<header class='stats-card-header'>", stat_label, "</header>",
        "<dl class='stats-card-body'>", body_lines, "</dl>",
        "</article>"
      )
    }, character(1), USE.NAMES = FALSE)
    paste(blocks, collapse = "\n")
  }

  render_stats_group <- function(stats_subset, header_label = NULL, container_class = "group-stats") {
    if (length(stats_subset) == 0) return("")
    blocks_html <- build_stats_blocks(stats_subset)
    header_html <- if (!is.null(header_label)) paste0("<h4 class='stats-header'>", htmltools::htmlEscape(header_label), "</h4>") else ""
    container_classes <- unique(c(container_class, "stats-collection"))
    paste0(
      "<div class='", paste(container_classes, collapse = " "), "'>",
      header_html,
      "<div class='stats-grid'>",
      blocks_html,
      "</div></div>"
    )
  }

  render_stats_unified <- function(stats_subset) {
    if (length(stats_subset) == 0) return("")
    blocks_html <- build_stats_blocks(stats_subset)
    paste0(
      "<div id='stats-section' class='stats-collection with-title'>",
      "<h3>Combined Statistics</h3>",
      "<div class='stats-grid'>",
      blocks_html,
      "</div></div>"
    )
  }

  num_items <- length(indices_results)
  num_cols <- 1
  if (num_items > 0) {
    num_cols <- ceiling(sqrt(num_items))
    if (num_items %in% c(2, 3)) num_cols <- num_items
    if (num_items == 4) num_cols <- 2
    if (num_items %in% c(5, 6)) num_cols <- 2
    if (num_items %in% c(7, 8, 9)) num_cols <- 3
    if (num_items %in% c(10, 11, 12)) num_cols <- 3
    if (num_items >= 13 && num_items <= 16) num_cols <- 4
    if (num_items > 16) num_cols <- 4
  }

  css <- sprintf(
      "\n      body { font-family: 'Segoe UI', sans-serif; background-color: #f4f6fb; margin: 0; padding: 12px; color: #1f2937; }\n      h2 { text-align: center; margin-bottom: 20px; color: #111827; font-weight: 600; }\n      #results-grid, .results-grid { display: grid; grid-template-columns: repeat(%d, minmax(240px, 1fr)); gap: 8px; align-items: stretch; }\n      .result-item { background: #ffffff; border: 1px solid #dbe1ea; border-radius: 10px; box-shadow: 0 12px 28px rgba(15, 23, 42, 0.08); padding: 14px; display: flex; flex-direction: column; transition: transform 0.18s ease, box-shadow 0.18s ease; min-height: 0; }\n      .result-item:hover { transform: translateY(-2px); box-shadow: 0 16px 32px rgba(15, 23, 42, 0.12); }\n      .result-item.result-from-batch { border-color: #c3d4f7; background: linear-gradient(135deg, rgba(223, 232, 255, 0.65), rgba(255, 255, 255, 0.95)); }\n      .result-item .item-header { display: flex; align-items: center; gap: 8px; justify-content: space-between; margin-bottom: 10px; }\n      .item-label { font-weight: 600; color: #0f172a; flex: 1 1 auto; min-width: 0; margin-right: 6px; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }\n      .item-metadata { font-size: 0.8em; color: #4b5563; flex: 0 1 34%%; min-width: 0; text-align: right; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
      .item-header > .item-metadata { margin-left: 6px; }\n      .item-content { flex: 1; border-top: 1px solid #e5e9f2; padding-top: 9px; display: flex; flex-direction: column; gap: 8px; min-height: 0; }\n      .item-content-body { position: relative; flex: 1; max-height: 16em; overflow-y: auto; overflow-x: auto; padding-right: 6px; }\n      .item-content-body.item-kind-text { overflow-x: hidden; }\n      .item-content-body.item-kind-media { max-height: 18em; }\n      .item-content-body.item-kind-image { max-height: none; overflow-y: hidden; overflow-x: auto; padding-right: 0; }\n      .item-content-body.item-kind-video { overflow-x: hidden; }\n      .item-content-body.item-kind-audio { overflow-x: hidden; }\n      .item-content-body::-webkit-scrollbar { width: 6px; height: 6px; }\n      .item-content-body::-webkit-scrollbar-thumb { background: rgba(148, 163, 184, 0.55); border-radius: 8px; }\n      .item-content-body::-webkit-scrollbar-track { background: transparent; }\n      .item-content-body pre { background-color: #f3f4f6; padding: 10px; border-radius: 8px; overflow-x: auto; overflow-y: visible; white-space: pre-wrap; word-break: break-word; font-size: 0.85em; margin: 0; }\n      .item-content-body code { background-color: #f3f4f6; padding: 2px 4px; border-radius: 4px; font-size: 0.85em; color: #1f2937; }\n      .media-scroll { position: relative; max-width: 100%%; }\n      .media-scroll-image { overflow-x: auto; overflow-y: hidden; padding-bottom: 6px; }\n      .media-scroll-image img { display: block; max-height: 26em; width: auto; height: auto !important; max-width: none !important; }\n      .media-scroll-video, .media-scroll-audio { overflow-x: hidden; }\n      .status-dot { width: 9px; height: 9px; border-radius: 50%%; display: inline-block; margin-right: 4px; }\n      .status-success { background-color: #22c55e; }\n      .status-error { background-color: #ef4444; }\n      .status-invalid { background-color: #9ca3af; }\n      .status-info { background-color: #3b82f6; }\n      .error-message { color: #ef4444; font-style: italic; font-size: 0.85em; word-wrap: break-word; margin: 0; }\n\n      .group-section { margin-bottom: 26px; }\n      .group-header { margin: 0 0 12px; color: #0f172a; font-size: 1.25em; font-weight: 600; display: flex; align-items: center; gap: 6px; }\n      .group-header::after { content: \"\"; flex: 1; height: 1px; background: linear-gradient(90deg, rgba(148, 163, 184, 0.4), rgba(148, 163, 184, 0)); }\n      .group-items { margin-bottom: 16px; }\n\n      #stats-section { background: transparent; padding: 0; border: none; }\n      .stats-collection { display: flex; flex-direction: column; gap: 8px; align-items: stretch; }\n      .stats-collection.with-title { margin-top: 25px; }\n      .stats-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(180px, 1fr)); gap: 9px; margin-top: 0; justify-items: stretch; }\n      #stats-section h3 { margin: 0 0 12px; color: #1f2937; text-align: left; font-size: 1em; font-weight: 600; }\n      .stats-header { margin: 0 0 6px; font-size: 0.9em; color: #1f2937; font-weight: 600; letter-spacing: 0.01em; }\n      .stats-card { background: linear-gradient(145deg, #ffffff, #f5f7fb); border: 1px solid #d2d9e5; border-radius: 10px; padding: 12px 14px; box-shadow: 0 10px 20px rgba(15, 23, 42, 0.06); display: flex; flex-direction: column; gap: 8px; min-height: 0; }\n      .stats-card-header { font-weight: 600; font-size: 0.95em; color: #0f172a; display: flex; align-items: center; gap: 6px; }\n      .stats-card-header::before { content: \"\"; width: 9px; height: 9px; border-radius: 50%%; background: #3b82f6; box-shadow: 0 0 0 4px rgba(59, 130, 246, 0.15); }\n      .stats-card-body { margin: 0; display: grid; grid-template-columns: auto 1fr; gap: 5px 8px; font-size: 0.8em; color: #1f2937; }\n      .stats-card-body dt { font-weight: 600; color: #4b5563; text-transform: capitalize; }\n      .stats-card-body dd { margin: 0; color: #111827; }\n\n      .modal-overlay { display: none; position: fixed; z-index: 1000; left: 0; top: 0; width: 100%%; height: 100%%; overflow: auto; background-color: rgba(15, 23, 42, 0.88); justify-content: center; align-items: center; padding: 20px; box-sizing: border-box; }\n      .modal-content { margin: auto; display: block; max-width: 92%%; max-height: 92vh; object-fit: contain; animation-name: zoomIn; animation-duration: 0.3s; border-radius: 10px; box-shadow: 0 16px 40px rgba(15, 23, 42, 0.35); }\n      .modal-close { position: absolute; top: 16px; right: 28px; color: #f9fafb; font-size: 34px; font-weight: bold; transition: 0.3s; cursor: pointer; }\n      .modal-close:hover, .modal-close:focus { color: #d1d5db; text-decoration: none; cursor: pointer; }\n      @keyframes zoomIn { from { transform: scale(0.82); opacity: 0; } to { transform: scale(1); opacity: 1; } }\n    ", num_cols)

  item_infos <- vector("list", length(indices_results))

  for (i in indices_results) {
    res_item <- results_list[[i]]
    origin_label <- origin_labels[i] %||% paste0("Item ", i)
    category <- if (length(item_categories) >= i) item_categories[[i]] else "other"
    item_batch_id <- if (length(batch_ids) >= i) batch_ids[[i]] else NA_character_
    item_html <- ""
    status_class <- "status-invalid"
    label_html <- htmltools::htmlEscape(origin_label)
    metadata_html <- ""
    metadata_tooltip <- ""
    content_html <- "<p class='error-message'><i>Invalid structure or NULL.</i></p>"
    image_metadata_str <- NULL
    res_is_list <- is.list(res_item)
    model_info_short <- "N/A"
    time_info <- "N/A"
    content_type_info <- "Data"
    extra_info <- ""
    content_kind <- "text"

    if (res_is_list) {
      status_api_val <- res_item$status_api %||% "INVALID"
      status_class <- switch(status_api_val, "SUCCESS" = "status-success", "ERROR" = "status-error", "FALHA" = "status-error", "status-invalid")
      label_html <- htmltools::htmlEscape(res_item$label %||% origin_label)
      model_raw <- res_item$model %||% "?"
      model_info_short <- if (grepl("/", model_raw)) sub(".*/", "", model_raw) else model_raw
      model_info_short_escaped <- htmltools::htmlEscape(model_info_short)
      time_info <- if (!is.null(res_item$duration)) paste0(round(res_item$duration, 1), "s") else ""
      metadata_parts <- c(model_info_short_escaped, time_info)
      metadata_html <- paste(metadata_parts[nzchar(metadata_parts)], collapse = " | ")
      metadata_tooltip <- metadata_html

      if (status_api_val == "SUCCESS") {
        value <- res_item$response_value
        if (!is.null(value) && is.character(value) && length(value) == 1 && nzchar(value)) {
          path <- value
          if (grepl("\\.(png|jpe?g|gif|webp|svg)$", path, ignore.case = TRUE)) {
            img_result <- .gen_view_image(path)
            content_html <- img_result$html
            image_metadata_str <- img_result$metadata_str
            if (img_result$error) status_class <- "status-error"
            content_type_info <- "Image"
            content_kind <- "image"
          } else if (grepl("\\.(mp4|webm|ogg|mov|avi)$", path, ignore.case = TRUE)) {
            content_html <- .gen_view_video(path)
            content_type_info <- "Video"
            content_kind <- "video"
          } else if (grepl("\\.(mp3|wav|aac|oga)$", path, ignore.case = TRUE)) {
            content_html <- .gen_view_audio(path)
            content_type_info <- "Audio"
            content_kind <- "audio"
          } else {
            content_html <- .gen_view_text(value)
            content_type_info <- "text"
            content_kind <- "text"
          }
        } else {
          content_html <- .gen_view_text(value)
          content_type_info <- "text"
          content_kind <- "text"
        }
      } else {
        error_msg <- htmltools::htmlEscape(res_item$status_msg %||% paste("Unrecognized status or no details:", status_api_val))
        content_html <- paste0("<p class='error-message'>", error_msg, "</p>")
        content_kind <- "text"
      }
    } else {
      status_class <- "status-info"
      if (is.character(res_item)) {
        if (length(res_item) == 1) {
          lower_val <- tolower(res_item)
          if (grepl("\\.(png|jpe?g|gif|webp|svg)$", lower_val)) {
            content_type_info <- "Image"
            content_kind <- "image"
          } else if (grepl("\\.(mp4|webm|ogg|mov|avi)$", lower_val)) {
            content_type_info <- "Video"
            content_kind <- "video"
          } else if (grepl("\\.(mp3|wav|aac|oga)$", lower_val)) {
            content_type_info <- "Audio"
            content_kind <- "audio"
          } else {
            content_type_info <- "text"
            content_kind <- "text"
          }
        } else {
          content_type_info <- "text"
          content_kind <- "text"
        }
      } else {
        content_type_info <- paste("Value type:", class(res_item)[1])
        content_kind <- "other"
      }
      metadata_html <- htmltools::htmlEscape(content_type_info)
      metadata_tooltip <- metadata_html
      if (is.character(res_item) && length(res_item) == 1 && nzchar(res_item)) {
        if (content_type_info == "Image") {
          img_result <- .gen_view_image(res_item)
          content_html <- img_result$html
          image_metadata_str <- img_result$metadata_str
          content_kind <- "image"
        } else if (content_type_info == "Video") {
          content_html <- .gen_view_video(res_item)
          content_kind <- "video"
        } else if (content_type_info == "Audio") {
          content_html <- .gen_view_audio(res_item)
          content_kind <- "audio"
        } else {
          content_html <- .gen_view_text(res_item)
          content_kind <- "text"
        }
      } else {
        content_html <- .gen_view_text(res_item)
        content_kind <- if (is.character(res_item) || is.numeric(res_item) || is.logical(res_item)) "text" else "other"
      }
    }

    if (!is.null(image_metadata_str)) {
      metadata_html <- paste(c(metadata_html, image_metadata_str), collapse = " | ")
      metadata_tooltip <- metadata_html
    }

    if (content_kind == "image") {
      content_html <- paste0("<div class='media-scroll media-scroll-image'>", content_html, "</div>")
    } else if (content_kind == "video") {
      content_html <- paste0("<div class='media-scroll media-scroll-video'>", content_html, "</div>")
    } else if (content_kind == "audio") {
      content_html <- paste0("<div class='media-scroll media-scroll-audio'>", content_html, "</div>")
    }

    status_dot_html <- paste0("<span class='status-dot ", status_class, "' title='Status: ", status_class, "'></span>")
    label_tooltip <- if (res_is_list && !is.null(res_item$label) && nchar(res_item$label) > 25) htmltools::htmlEscape(res_item$label) else metadata_tooltip

    header_html <- paste0(
      "<div class='item-header'>",
      status_dot_html,
      "<span class='item-label' title='", label_tooltip %||% label_html, "'>", label_html, "</span>",
      "<span class='item-metadata' title='", metadata_tooltip %||% metadata_html, "'>", metadata_html, "</span>",
      "</div>"
    )
    content_body_classes <- c("item-content-body", paste0("item-kind-", content_kind))
    if (content_kind %in% c("image", "video", "audio")) {
      content_body_classes <- c(content_body_classes, "item-kind-media")
    }
    content_div_html <- paste0(
      "<div class='item-content'>",
      "<div class='", paste(content_body_classes, collapse = " "), "'>",
      content_html,
      "</div>",
      "</div>"
    )

    item_classes <- c("result-item", paste0("result-category-", category))
    if (!is.null(item_batch_id) && !is.na(item_batch_id) && nzchar(item_batch_id)) {
      item_classes <- c(item_classes, "result-from-batch")
    }
    if (nzchar(content_kind)) {
      item_classes <- c(item_classes, paste0("result-kind-", content_kind))
    }
    item_html <- paste0("<div class='", paste(unique(item_classes), collapse = " "), "'>", header_html, content_div_html, "</div>")
    item_infos[[i]] <- list(
      html = item_html,
      category = category,
      batch_id = if (!is.null(item_batch_id) && !is.na(item_batch_id) && nzchar(item_batch_id)) item_batch_id else NA_character_,
      display_type = if (length(base_types) >= i) base_types[[i]] else "other",
      content_kind = content_kind
    )
  }

  if (grouped) {
    item_categories_vec <- if (length(item_infos) > 0) {
      vapply(item_infos, function(info) info$category %||% "other", character(1))
    } else {
      character(0)
    }
    item_batch_vec <- if (length(item_infos) > 0) {
      vapply(item_infos, function(info) {
        bid <- info$batch_id
        if (is.null(bid) || is.na(bid) || !nzchar(bid)) NA_character_ else bid
      }, character(1))
    } else {
      character(0)
    }

    stats_blocks_view <- stats_blocks_for_view
    gather_stats_for <- function(item_idx, batch_id = NULL) {
      if (!show_stats_viewer) return(list())
      if (length(stats_blocks_view) == 0) return(list())
      subset <- Filter(function(entry) {
        entry_batch <- entry$batch_id
        if (is.null(entry_batch) || length(entry_batch) == 0) entry_batch <- NA_character_
        if (!is.null(batch_id)) {
          if (!is.na(entry_batch) && entry_batch == batch_id) return(TRUE)
          if (!is.null(entry$index) && entry$index %in% item_idx) return(TRUE)
          return(FALSE)
        }
        if (!is.null(entry$index)) {
          return(entry$index %in% item_idx)
        }
        is.na(entry_batch)
      }, stats_blocks_view)
      if (!is.null(batch_id) && length(subset) > 1) {
        agg_mask <- vapply(subset, function(entry) {
          entry_batch <- entry$batch_id
          !is.null(entry_batch) && length(entry_batch) > 0 && entry_batch == batch_id && is.null(entry$index)
        }, logical(1))
        subset <- c(subset[agg_mask], subset[!agg_mask])
      }
      subset
    }

    build_section_html <- function(header_label, item_idx, stats_subset, stats_header = NULL) {
      section_parts <- c(paste0("<section class='group-section'><h3 class='group-header'>", htmltools::htmlEscape(header_label), "</h3>"))
      if (length(item_idx) > 0) {
        items_html <- paste(vapply(item_idx, function(idx) item_infos[[idx]]$html, character(1)), collapse = "\n")
        section_parts <- c(section_parts, paste0("<div class='group-items results-grid'>", items_html, "</div>"))
      }
      stats_html <- render_stats_group(stats_subset, stats_header %||% paste(header_label, "Stats"))
      if (nzchar(stats_html)) section_parts <- c(section_parts, stats_html)
      section_parts <- c(section_parts, "</section>")
      paste(section_parts, collapse = "\n")
    }

    group_sections <- character()
    batch_category_order <- c("batch_text", "batch_image", "batch_video")
    for (cat in batch_category_order) {
      idx_cat <- which(item_categories_vec == cat)
      if (length(idx_cat) == 0) next
      cat_batch_ids <- unique(item_batch_vec[idx_cat])
      cat_batch_ids <- cat_batch_ids[!is.na(cat_batch_ids) & nzchar(cat_batch_ids)]
      if (length(cat_batch_ids) == 0) next
      order_metric <- vapply(cat_batch_ids, function(bid) {
        meta <- batch_meta[[bid]]
        if (!is.null(meta) && !is.null(meta$first_index)) {
          meta$first_index
        } else {
          min(which(item_batch_vec == bid))
        }
      }, numeric(1))
      cat_batch_ids <- cat_batch_ids[order(order_metric)]
      for (bid in cat_batch_ids) {
        batch_label <- batch_meta[[bid]]$label %||% paste0("Batch ", bid)
        section_header <- paste0(batch_label, " (", group_labels[[cat]] %||% cat, ")")
        item_idx <- which(item_categories_vec == cat & item_batch_vec == bid)
        stats_subset <- gather_stats_for(item_idx, batch_id = bid)
        section_html <- build_section_html(section_header, item_idx, stats_subset, paste0(batch_label, " Stats"))
        group_sections <- c(group_sections, section_html)
      }
    }

    non_batch_categories <- c("text", "image", "video")
    for (cat in non_batch_categories) {
      item_idx <- which(item_categories_vec == cat & (is.na(item_batch_vec) | !nzchar(item_batch_vec)))
      stats_subset <- gather_stats_for(item_idx)
      if (length(item_idx) == 0 && length(stats_subset) == 0) next
      section_header <- group_labels[[cat]] %||% cat
      section_html <- build_section_html(section_header, item_idx, stats_subset, paste(section_header, "Stats"))
      group_sections <- c(group_sections, section_html)
    }

    other_idx <- which(item_categories_vec == "other")
    other_stats <- gather_stats_for(other_idx)
    if (length(other_idx) > 0 || length(other_stats) > 0) {
      section_header <- group_labels[["other"]] %||% "Other"
      section_html <- build_section_html(section_header, other_idx, other_stats, paste(section_header, "Stats"))
      group_sections <- c(group_sections, section_html)
    }

    if (length(group_sections) == 0) {
      html_body_content <- "<p style='text-align:center; margin-top: 20px;'><i>(No results to display)</i></p>"
    } else {
      html_body_content <- paste(group_sections, collapse = "\n")
    }
  } else {
    if (length(indices_results) > 0) {
      items_html <- paste(vapply(item_infos, function(info) info$html, character(1)), collapse = "\n")
      results_grid_html <- paste0("<div id='results-grid'>", items_html, "</div>")
    } else {
      results_grid_html <- "<p style='text-align:center; margin-top: 20px;'><i>(No individual results to display)</i></p>"
    }
    stats_html <- if (show_stats_viewer) render_stats_unified(stats_blocks_for_view) else ""
    html_body_content <- paste(results_grid_html, stats_html, sep = "\n")
  }

  html_content <- paste(
    "<!DOCTYPE html>", "<html>", "<head>", '<meta charset=\"UTF-8\">',
    "<title>Generated Results</title>", "<style>", css, "</style>", "</head>",
    "<body>",
    html_body_content,
    modal_html,
    modal_js,
    "</body>", "</html>"
  )

  temp_html_file <- tempfile(fileext = ".html")
  con <- file(temp_html_file, open = "wt", encoding = "UTF-8")
  writeLines(html_content, con)
  close(con)
  if (viewer_available) {
    rstudioapi::viewer(temp_html_file)
  } else {
    warning("RStudio Viewer is not accessible. The HTML file was saved at: ", temp_html_file)
  }
  invisible(NULL)
}
