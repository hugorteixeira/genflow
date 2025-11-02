#' Count voting tokens inside generated responses
#'
#' Scans a list of texts (typically the output lists returned by
#' [gen_batch()]) and counts the first occurrence of a vote marker that follows a
#' given trigger word or phrase. The vote itself can be extracted either as a
#' number (multi-digit) or as a single letter. When present, the provider model
#' associated with each response is surfaced in the ranking output.
#'
#' @param voting_list List of response objects (such as the elements produced by
#'   [gen_batch()]) or a character vector.
#' @param underlying_list Optional list containing the items being voted on.
#'   When supplied, `return_winner = "content"` returns the corresponding entry
#'   from this list (instead of the `voting_list`).
#' @param trigger Character string used to locate the vote inside each text (for
#'   example, `"option"`).
#' @param type Either `"number"` (default) to capture multi-digit options, or
#'   `"letter"` to capture a single alphabetical option.
#' @param label Character label included in the printed ranking (default
#'   `"Option"`).
#' @param title Optional character title printed before the ranking (default
#'   `"Vote Count"`). Use `NULL` or `""` to skip the title.
#' @param return_winner Logical. When `TRUE`, the function invisibly returns only
#'   the winning option instead of the full ranking.
#'
#' @return Invisibly returns the ranking (character vector) or, when
#'   `return_winner = TRUE`, the winning option. A message is printed when no
#'   votes are found.
#' @examples
#' votes <- list(
#'   list(response_value = "I choose option 2", model = "model-a"),
#'   list(response_value = "Option 2 is my pick", model = "model-b"),
#'   list(response_value = "My vote is option 3", model = "model-c")
#' )
#'
#' # Count numeric votes that follow the word "option"
#' gen_vote(votes, trigger = "option", type = "number")
#'
#' # Retrieve only the winning option
#' gen_vote(votes, trigger = "option", return_winner = TRUE)
#' @export
gen_vote <- function(voting_list,
                     underlying_list = NULL,
                     trigger,
                     type = c("number", "letter"),
                     label = "Option",
                     title = "Vote Count",
                     return_winner = c("ranking", "content", "scoreboard", "id")) {
  if (missing(voting_list)) {
    stop("`voting_list` must be provided.", call. = FALSE)
  }
  if (missing(trigger) || !nzchar(trigger)) {
    stop("`trigger` must be a non-empty string.", call. = FALSE)
  }

  type <- match.arg(type)

  normalize_text <- function(text) {
    tolower(iconv(text, to = "ASCII//TRANSLIT", sub = ""))
  }

  trigger_norm <- normalize_text(trigger)
  if (!nzchar(trigger_norm)) {
    stop("`trigger` could not be normalised to ASCII; please use a different value.", call. = FALSE)
  }

  pattern <- if (type == "number") {
    paste0("\\Q", trigger_norm, "\\E[^a-z0-9]*([0-9]+)")
  } else {
    paste0("\\Q", trigger_norm, "\\E[^a-z0-9]*([a-z])")
  }

  vote_values <- character(0)
  vote_models <- character(0)

  append_vote <- function(text, model_val = NA_character_) {
    if (!is.character(text) || length(text) == 0) {
      return()
    }
    for (single_text in text) {
      if (is.na(single_text)) next
      text_norm <- normalize_text(single_text)
      if (!nzchar(text_norm)) next
      match_info <- regexpr(pattern, text_norm, perl = TRUE, ignore.case = TRUE)
      if (match_info[1] == -1) {
        next
      }
      capture_start <- attr(match_info, "capture.start")[1]
      capture_length <- attr(match_info, "capture.length")[1]
      if (is.na(capture_start) || capture_start <= 0 || capture_length <= 0) {
        next
      }
      captured_value <- substr(text_norm, capture_start, capture_start + capture_length - 1)
      vote_values <<- c(vote_values, captured_value)
      vote_models <<- c(vote_models, model_val %||% NA_character_)
      break
    }
  }

  process_entry <- function(entry) {
    if (is.null(entry)) {
      return()
    }

    if (is.list(entry)) {
      # Skip combined statistics blocks returned by gen_batch()
      if (!is.null(entry$duration_real_secs) && !is.null(entry$parallel_mode)) {
        return()
      }

      model_val <- entry$model %||% attr(entry, "model") %||% NA_character_
      if (!is.null(entry$response_value)) {
        append_vote(entry$response_value, model_val)
      } else {
        # Fall back to any character elements inside the list
        char_elements <- Filter(is.character, entry)
        if (length(char_elements) > 0) {
          append_vote(unlist(char_elements, use.names = FALSE), model_val)
        }
      }
    } else {
      model_val <- attr(entry, "model") %||% NA_character_
      append_vote(as.character(entry), model_val)
    }
  }

  if (is.list(voting_list)) {
    list_names <- names(voting_list)
    for (idx in seq_along(voting_list)) {
      if (!is.null(list_names) && identical(list_names[idx], "combined_stats")) {
        next
      }
      process_entry(voting_list[[idx]])
    }
  } else {
    process_entry(voting_list)
  }

  valid_idx <- if (type == "number") {
    grep("^[0-9]+$", vote_values)
  } else {
    grep("^[a-z]$", vote_values)
  }

  valid_values <- vote_values[valid_idx]
  valid_models <- vote_models[valid_idx]

  if (length(valid_values) == 0) {
    message("No matches found for the requested trigger and type.")
    return(invisible(NULL))
  }

  counts <- sort(table(valid_values), decreasing = TRUE)
  ranking <- character(length(counts))

  for (i in seq_along(counts)) {
    option_value <- names(counts)[i]
    option_count <- counts[[i]]
    matching_idx <- which(valid_values == option_value)
    models_for_option <- valid_models[matching_idx]
    model_list <- paste(unique(models_for_option[!is.na(models_for_option) & nzchar(models_for_option)]), collapse = ", ")

    vote_label <- toupper(option_value)
    vote_word <- ifelse(option_count == 1, "vote", "votes")
    ranking_text <- sprintf("%d) %s %s (%d %s", i, label, vote_label, option_count, vote_word)
    if (nzchar(model_list)) {
      ranking_text <- paste0(ranking_text, " - Models: ", model_list)
    }
    ranking[i] <- paste0(ranking_text, ")")
  }

  if (!is.null(title) && nzchar(title)) {
    cat(title, "\n\n", sep = "")
  }

  cat(paste(ranking, collapse = "\n"), "\n", sep = "")

  if (length(return_winner) == 1L && is.logical(return_winner)) {
    return_winner <- if (isTRUE(return_winner)) "id" else "ranking"
  }
  return_mode <- match.arg(return_winner, c("ranking", "content", "scoreboard", "id"))
  if (identical(return_mode, "content") && is.null(underlying_list)) {
    stop("`underlying_list` must be provided when `return_winner = \"content\"`.", call. = FALSE)
  }

  winner_value <- names(counts)[1]

  select_entries <- function(lst) {
    if (is.null(lst)) {
      return(NULL)
    }
    if (!is.list(lst)) {
      return(as.list(lst))
    }
    entries <- list()
    names_lst <- names(lst)
    for (idx in seq_along(lst)) {
      if (!is.null(names_lst) && identical(names_lst[idx], "combined_stats")) {
        next
      }
      candidate <- lst[[idx]]
      if (is.list(candidate) &&
        !is.null(candidate$duration_real_secs) &&
        !is.null(candidate$parallel_mode)) {
        next
      }
      entries[[length(entries) + 1]] <- candidate
    }
    entries
  }

  candidates <- select_entries(if (!is.null(underlying_list)) underlying_list else voting_list)

  resolve_candidate_index <- function(option_value) {
    if (type == "number") {
      idx <- suppressWarnings(as.integer(option_value))
    } else {
      idx <- match(tolower(option_value), letters)
    }
    if (is.na(idx) || idx < 1) {
      return(NA_integer_)
    }
    idx
  }

  result <- switch(return_mode,
    ranking = ranking,
    id = winner_value,
    scoreboard = {
      data.frame(
        option = names(counts),
        votes = as.integer(counts),
        stringsAsFactors = FALSE
      )
    },
    content = {
      candidate_idx <- resolve_candidate_index(winner_value)
      if (!is.na(candidate_idx) &&
        !is.null(candidates) &&
        candidate_idx <= length(candidates)) {
        entry <- candidates[[candidate_idx]]
        if (is.list(entry)) {
          entry$response_value %||%
            entry$response %||%
            entry$text %||%
            NA_character_
        } else {
          as.character(entry)
        }
      } else {
        NA_character_
      }
    }
  )

  invisible(result)
}
