#' Update OpenRouter models list (internal)
#'
#' Connects to the OpenRouter API, retrieves the list of models and saves a
#' normalized CSV file named `openrouter.csv` in the provided directory.
#'
#' - Installs and loads required packages on-demand.
#' - Validates environment variable `OPENROUTER_API_KEY`.
#'
#' @param directory Character path where the CSV will be saved. If NULL, uses current working dir.
#' @param verbose Logical flag to print progress messages.
#' @return Invisibly returns a data frame with the processed models.
#' @keywords internal
#' @noRd
.update_models_openrouter <- function(directory = NULL, verbose = TRUE) {
   # --- 2. API Key ---
  api_key_openrouter <- Sys.getenv("OPENROUTER_API_KEY")
  if (api_key_openrouter == "") stop("Error: Environment variable 'OPENROUTER_API_KEY' not set.")

  # --- 3. API URL and Headers ---
  api_url <- "https://openrouter.ai/api/v1/models"
  headers <- httr::add_headers("Authorization" = paste("Bearer", api_key_openrouter))

  # --- 4. API Call ---
  if (verbose) message("Connecting to the OpenRouter API...")
  response <- tryCatch({ httr::GET(url = api_url, config = headers, httr::timeout(60)) },
                       error = function(e) stop("Error connecting to the OpenRouter API: ", e$message))

  # --- 5. Check Response Status ---
  if (httr::status_code(response) != 200) {
    error_content <- httr::content(response, "text", encoding = "UTF-8")
    stop("OpenRouter API Error (Status: ", httr::status_code(response), "): ", error_content)
  }

  # --- 6. Process JSON ---
  if (verbose) message("Processing JSON response...")
  raw_content <- httr::content(response, "raw")
  parsed_content <- tryCatch({ jsonlite::fromJSON(rawToChar(raw_content), simplifyVector = TRUE) },
                             error = function(e) stop("Error processing JSON from OpenRouter API: ", e$message))

  models_data <- NULL
  if (!is.null(parsed_content) && is.list(parsed_content) && "data" %in% names(parsed_content)) {
    if (is.data.frame(parsed_content$data)) {
      models_data <- parsed_content$data
    } else if (is.list(parsed_content$data)) {
      models_data <- tryCatch({
        list_data <- purrr::map(parsed_content$data, ~ as.list(.x))
        dplyr::bind_rows(!!!list_data)
      }, error = function(e) {
        warning("Field 'data' is not a data.frame and failed to convert: ", e$message)
        return(NULL)
      })
      if (is.null(models_data)) stop("Could not process the 'data' field from the OpenRouter API.")
    }
  }

  if (is.null(models_data) || nrow(models_data) == 0) {
    if (verbose) print(utils::str(parsed_content))
    stop("Field 'data' not found, empty, or not processable.")
  }

  # --- 7. Format Data ---
  if (verbose) message("Processing ", nrow(models_data), " OpenRouter models...")

  output_df <- purrr::map_df(seq_len(nrow(models_data)), function(i) {
    model_info <- models_data[i, ]

    get_nested <- function(data, ...) purrr::pluck(data, ...)
    model_id_safe <- get_nested(model_info, "id") %||% paste0("UNKNOWN_ID_", i)

    # Infer 'type' from architecture$input_modalities if present
    model_type <- "Chat"
    arch_data <- get_nested(model_info, "architecture")
    raw_modalities_data <- NULL
    if (!is.null(arch_data) && is.list(arch_data) && "input_modalities" %in% names(arch_data)) {
      raw_modalities_data <- arch_data[["input_modalities"]]
    }
    extracted_modalities <- NULL
    if (!is.null(raw_modalities_data)) {
      if (is.character(raw_modalities_data) && !is.list(raw_modalities_data)) {
        extracted_modalities <- raw_modalities_data
      } else if (is.list(raw_modalities_data)) {
        if (length(raw_modalities_data) > 0 && is.character(raw_modalities_data[[1]])) {
          extracted_modalities <- unlist(raw_modalities_data)
        } else {
          unlisted <- tryCatch(unlist(raw_modalities_data), error = function(e) NULL)
          if (!is.null(unlisted) && is.character(unlisted)) extracted_modalities <- unlisted
        }
      }
      if (!is.null(extracted_modalities)) {
        extracted_modalities <- extracted_modalities[!is.na(extracted_modalities) & extracted_modalities != ""]
        if (length(extracted_modalities) == 0) extracted_modalities <- NULL
      }
    }
    if (!is.null(extracted_modalities)) {
      modalities_lower <- tolower(extracted_modalities)
      has_text  <- "text"  %in% modalities_lower
      has_image <- "image" %in% modalities_lower
      if (has_text && has_image) model_type <- "Vision"
      else if (has_text)        model_type <- "Chat"
      else if (has_image)       model_type <- "Image"
      else if (length(modalities_lower) > 0) {
        first_modality <- extracted_modalities[[1]]
        model_type <- paste0(toupper(substr(first_modality, 1, 1)), substr(first_modality, 2, nchar(first_modality)))
      }
    }

    # Pricing: multiply by 1M and format as "prompt/completion", or "Free" if both 0
    prc_value_unquoted <- ""
    pricing_data <- get_nested(model_info, "pricing")
    if (!is.null(pricing_data) && is.list(pricing_data)) {
      price_prompt_num    <- suppressWarnings(as.numeric(pricing_data$prompt))
      price_completion_num <- suppressWarnings(as.numeric(pricing_data$completion))
      if (!is.na(price_prompt_num) && !is.na(price_completion_num)) {
        if (price_prompt_num == 0 && price_completion_num == 0) {
          prc_value_unquoted <- "Free"
        } else {
          price_prompt_num    <- price_prompt_num * 1000000
          price_completion_num <- price_completion_num * 1000000
          prompt_formatted    <- gsub("\\.?0+$", "", sprintf("%.10f", price_prompt_num))
          completion_formatted <- gsub("\\.?0+$", "", sprintf("%.10f", price_completion_num))
          prompt_formatted    <- gsub("\\.$", "", prompt_formatted)
          completion_formatted <- gsub("\\.$", "", completion_formatted)
          prc_value_unquoted  <- sprintf("%s/%s", prompt_formatted, completion_formatted)
        }
      } else {
        if (verbose) warning("Model ", model_id_safe, ": Failed to convert pricing to numeric. Pricing will be empty.")
      }
    } else {
      if (verbose) message("Model ", model_id_safe, ": Pricing information missing or invalid. Pricing will be empty.")
    }

    # Description: replace double quotes with single quotes
    raw_description <- get_nested(model_info, "description") %||% ""
    description_unquoted <- gsub('"', "'", raw_description, fixed = TRUE)

    tibble::tibble(
      service = "openrouter",
      model = model_id_safe,
      type = model_type,
      pricing = prc_value_unquoted,
      description = description_unquoted
    )
  })

  # --- 8. Post-processing: Add external quotes for CSV ---
  if (nrow(output_df) > 0) {
    output_df <- output_df %>%
      dplyr::mutate(
        pricing = ifelse(pricing == "", '""', sprintf('"%s"', pricing)),
        description = sprintf('"%s"', description)
      )
  }

  # --- 9. Create Directory (if necessary) ---
  if (!dir.exists(directory)) {
    if (verbose) message("Creating directory: ", directory)
    dir.create(directory, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(directory)) stop("Failed to create directory: ", directory)
  }

  # --- 10. Save CSV ---
  file_path <- file.path(directory, "openrouter.csv")
  if (verbose) message("\nSaving ", nrow(output_df), " OpenRouter models to: ", file_path)
  tryCatch({
    write.table(output_df, file = file_path, sep = ",", quote = FALSE,
                row.names = FALSE, col.names = TRUE, na = "", fileEncoding = "UTF-8")
  }, error = function(e) stop("Error saving CSV '", file_path, "' with write.table: ", conditionMessage(e)))

  if (verbose) message("File 'openrouter.csv' updated successfully.")
  invisible(output_df)
}
#' Update OpenAI models list (internal)
#'
#' Connects to the OpenAI API, retrieves the list of models and saves a
#' normalized CSV file named `openai.csv` in the provided directory.
#'
#' - Installs and loads required packages on-demand.
#' - Validates environment variable `OPENAI_API_KEY`.
#'
#' @param directory Character path where the CSV will be saved. If NULL, uses current working dir.
#' @param verbose Logical flag to print progress messages.
#' @return Invisibly returns a data frame with the processed models.
#' @keywords internal
#' @noRd
.update_models_openai <- function(directory = NULL, verbose = TRUE) {

  # --- 2. API Key ---
  api_key_openai <- Sys.getenv("OPENAI_API_KEY")
  if (api_key_openai == "") stop("Error: Environment variable 'OPENAI_API_KEY' not set.")

  # --- 3. API URL and Headers ---
  api_url <- "https://api.openai.com/v1/models"
  headers <- httr::add_headers("Authorization" = paste("Bearer", api_key_openai))

  # --- 4. API Call ---
  if (verbose) message("Connecting to the OpenAI API...")
  response <- tryCatch({ httr::GET(url = api_url, config = headers, httr::timeout(60)) },
                       error = function(e) stop("Error connecting to the OpenAI API: ", e$message))

  # --- 5. Check Response Status ---
  if (httr::status_code(response) != 200) {
    error_content <- httr::content(response, "text", encoding = "UTF-8")
    stop("OpenAI API Error (Status: ", httr::status_code(response), "): ", error_content)
  }

  # --- 6. Process JSON ---
  if (verbose) message("Processing JSON response...")
  raw_content <- httr::content(response, "raw")
  parsed_content <- tryCatch({ jsonlite::fromJSON(rawToChar(raw_content), simplifyVector = TRUE) },
                             error = function(e) stop("Error processing JSON from OpenAI API: ", e$message))

  models_list <- NULL
  if (!is.null(parsed_content) && is.list(parsed_content) && "data" %in% names(parsed_content)) {
    if (is.data.frame(parsed_content$data)) {
      models_list <- parsed_content$data
    } else if (is.list(parsed_content$data)) {
      models_list <- tryCatch({
        list_data <- purrr::map(parsed_content$data, ~ as.list(.x))
        dplyr::bind_rows(!!!list_data)
      }, error = function(e) {
        warning("Field 'data' is a list but failed conversion to data frame: ", e$message)
        return(NULL)
      })
      if (is.null(models_list)) stop("Could not process the 'data' list from the OpenAI API response.")
    }
  }

  if (is.null(models_list) || nrow(models_list) == 0) {
    if (verbose) print(utils::str(parsed_content))
    stop("Field 'data' not found, empty, or not processable in OpenAI API response.")
  }

  # --- 7. Format Data ---
  if (verbose) message("Processing ", nrow(models_list), " OpenAI models...")

  output_df <- purrr::map_df(seq_len(nrow(models_list)), function(i) {
    model_info <- models_list[i, ]
    model_id_safe <- model_info$id %||% paste0("UNKNOWN_ID_", i)

    model_type <- dplyr::case_when(
      grepl("^gpt-4.*vision", model_id_safe, ignore.case = TRUE) ~ "Vision",
      grepl("^gpt-4o", model_id_safe, ignore.case = TRUE) ~ "Vision",
      grepl("image", model_id_safe, ignore.case = TRUE) ~ "Image",
      grepl("^gpt-", model_id_safe, ignore.case = TRUE) ~ "Chat",
      grepl("^o[1-10]", model_id_safe, ignore.case = TRUE) ~ "Chat",
      grepl("^text-davinci-", model_id_safe, ignore.case = TRUE) ~ "Completion",
      grepl("^text-curie-", model_id_safe, ignore.case = TRUE) ~ "Completion",
      grepl("^text-babbage-", model_id_safe, ignore.case = TRUE) ~ "Completion",
      grepl("^text-ada-", model_id_safe, ignore.case = TRUE) ~ "Completion",
      grepl("^dall-e-", model_id_safe, ignore.case = TRUE) ~ "Image",
      grepl("^tts-", model_id_safe, ignore.case = TRUE) ~ "Audio",
      grepl("^whisper-", model_id_safe, ignore.case = TRUE) ~ "Audio",
      grepl("^text-embedding-", model_id_safe, ignore.case = TRUE) ~ "Embedding",
      grepl("^text-similarity-", model_id_safe, ignore.case = TRUE) ~ "Embedding",
      grepl("^text-search-", model_id_safe, ignore.case = TRUE) ~ "Embedding",
      grepl("^code-", model_id_safe, ignore.case = TRUE) ~ "Code",
      TRUE ~ "Unknown"
    )

    prc_value_unquoted <- ""
    description_unquoted <- ""

    tibble::tibble(
      service = "openai",
      model = model_id_safe,
      type = model_type,
      pricing = prc_value_unquoted,
      description = description_unquoted
    )
  })

  # --- 8. Post-processing: Add quotes for CSV ---
  if (nrow(output_df) > 0) {
    output_df <- output_df %>%
      dplyr::mutate(
        pricing = sprintf('"%s"', pricing),
        description = sprintf('"%s"', description)
      )
  }

  # --- 9. Create Directory ---
  if (!dir.exists(directory)) {
    if (verbose) message("Creating directory: ", directory)
    dir.create(directory, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(directory)) stop("Failed to create directory: ", directory)
  }

  # --- 10. Save CSV ---
  file_path <- file.path(directory, "openai.csv")
  if (verbose) message("\nSaving ", nrow(output_df), " OpenAI models to: ", file_path)
  tryCatch({
    write.table(output_df, file = file_path, sep = ",", quote = FALSE,
                row.names = FALSE, col.names = TRUE, na = "", fileEncoding = "UTF-8")
  }, error = function(e) stop("Error saving CSV '", file_path, "' with write.table: ", conditionMessage(e)))

  if (verbose) message("File 'openai.csv' updated successfully.")
  invisible(output_df)
}
#' Update Anthropic models list (internal)
#'
#' Connects to the Anthropic API, retrieves the list of models and saves a
#' normalized CSV file named `anthropic.csv` in the provided directory.
#'
#' - Validates environment variable `ANTHROPIC_API_KEY` (or `CLAUDE_API_KEY`).
#'
#' @param directory Character path where the CSV will be saved. If NULL, uses current working dir.
#' @param verbose Logical flag to print progress messages.
#' @return Invisibly returns a data frame with the processed models.
#' @keywords internal
#' @noRd
.update_models_anthropic <- function(directory = NULL, verbose = TRUE) {
  api_key <- trimws(Sys.getenv("ANTHROPIC_API_KEY"))
  if (!nzchar(api_key)) {
    api_key <- trimws(Sys.getenv("CLAUDE_API_KEY"))
  }
  if (!nzchar(api_key)) stop("Error: Environment variable 'ANTHROPIC_API_KEY' not set.")

  base_url <- trimws(Sys.getenv("ANTHROPIC_BASE_URL", "https://api.anthropic.com"))
  if (!nzchar(base_url)) {
    base_url <- "https://api.anthropic.com"
  }
  base_url <- sub("/+$", "", base_url)
  api_version <- trimws(Sys.getenv("ANTHROPIC_API_VERSION", "2023-06-01"))
  if (!nzchar(api_version)) {
    api_version <- "2023-06-01"
  }

  api_url <- paste0(base_url, "/v1/models")
  headers <- httr::add_headers(
    "x-api-key" = api_key,
    "anthropic-version" = api_version
  )

  if (verbose) message("Connecting to the Anthropic API...")
  response <- tryCatch({ httr::GET(url = api_url, config = headers, httr::timeout(60)) },
    error = function(e) stop("Error connecting to the Anthropic API: ", e$message)
  )

  if (httr::status_code(response) != 200) {
    error_content <- httr::content(response, "text", encoding = "UTF-8")
    stop("Anthropic API Error (Status: ", httr::status_code(response), "): ", error_content)
  }

  if (verbose) message("Processing JSON response...")
  raw_content <- httr::content(response, "raw")
  parsed_content <- tryCatch({ jsonlite::fromJSON(rawToChar(raw_content), simplifyVector = TRUE) },
    error = function(e) stop("Error processing JSON from Anthropic API: ", e$message)
  )

  models_list <- NULL
  if (!is.null(parsed_content) && is.list(parsed_content) && "data" %in% names(parsed_content)) {
    if (is.data.frame(parsed_content$data)) {
      models_list <- parsed_content$data
    } else if (is.list(parsed_content$data)) {
      models_list <- tryCatch({
        list_data <- purrr::map(parsed_content$data, ~ as.list(.x))
        dplyr::bind_rows(!!!list_data)
      }, error = function(e) {
        warning("Field 'data' is a list but failed conversion to data frame: ", e$message)
        return(NULL)
      })
      if (is.null(models_list)) stop("Could not process the 'data' list from the Anthropic API response.")
    }
  }

  if (is.null(models_list) || nrow(models_list) == 0) {
    if (verbose) print(utils::str(parsed_content))
    stop("Field 'data' not found, empty, or not processable in Anthropic API response.")
  }

  if (verbose) message("Processing ", nrow(models_list), " Anthropic models...")

  output_df <- purrr::map_df(seq_len(nrow(models_list)), function(i) {
    model_info <- models_list[i, ]
    model_id_safe <- model_info$id %||% model_info$model %||% model_info$name %||% paste0("UNKNOWN_ID_", i)
    display_name <- as.character(model_info$display_name %||% model_info$name %||% "")
    model_text <- tolower(paste(model_id_safe, display_name))
    model_type <- if (grepl("vision|image|multimodal", model_text, perl = TRUE)) "Vision" else "Chat"

    tibble::tibble(
      service = "anthropic",
      model = model_id_safe,
      type = model_type,
      pricing = "",
      description = gsub('"', "'", display_name, fixed = TRUE)
    )
  })

  if (nrow(output_df) > 0) {
    output_df <- output_df %>%
      dplyr::mutate(
        pricing = sprintf('"%s"', pricing),
        description = sprintf('"%s"', description)
      )
  }

  if (!dir.exists(directory)) {
    if (verbose) message("Creating directory: ", directory)
    dir.create(directory, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(directory)) stop("Failed to create directory: ", directory)
  }

  file_path <- file.path(directory, "anthropic.csv")
  if (verbose) message("\nSaving ", nrow(output_df), " Anthropic models to: ", file_path)
  tryCatch({
    write.table(output_df, file = file_path, sep = ",", quote = FALSE,
      row.names = FALSE, col.names = TRUE, na = "", fileEncoding = "UTF-8"
    )
  }, error = function(e) stop("Error saving CSV '", file_path, "' with write.table: ", conditionMessage(e)))

  if (verbose) message("File 'anthropic.csv' updated successfully.")
  invisible(output_df)
}
#' Update Groq models list (internal)
#'
#' Connects to the Groq API, retrieves the list of models and saves a
#' normalized CSV file named `groq.csv` in the provided directory.
#'
#' - Validates environment variable `GROQ_API_KEY`.
#'
#' @param directory Character path where the CSV will be saved. If NULL, uses current working dir.
#' @param verbose Logical flag to print progress messages.
#' @return Invisibly returns a data frame with the processed models.
#' @keywords internal
#' @noRd
.update_models_groq <- function(directory = NULL, verbose = TRUE) {
  api_key_groq <- trimws(Sys.getenv("GROQ_API_KEY"))
  if (api_key_groq == "") stop("Error: Environment variable 'GROQ_API_KEY' not set.")

  base_url <- trimws(Sys.getenv("GROQ_BASE_URL", "https://api.groq.com"))
  if (!nzchar(base_url)) {
    base_url <- "https://api.groq.com"
  }
  base_url <- sub("/+$", "", base_url)

  api_url <- paste0(base_url, "/openai/v1/models")
  headers <- httr::add_headers("Authorization" = paste("Bearer", api_key_groq))

  if (verbose) message("Connecting to the Groq API...")
  response <- tryCatch({ httr::GET(url = api_url, config = headers, httr::timeout(60)) },
    error = function(e) stop("Error connecting to the Groq API: ", e$message)
  )

  if (httr::status_code(response) != 200) {
    error_content <- httr::content(response, "text", encoding = "UTF-8")
    stop("Groq API Error (Status: ", httr::status_code(response), "): ", error_content)
  }

  if (verbose) message("Processing JSON response...")
  raw_content <- httr::content(response, "raw")
  parsed_content <- tryCatch({ jsonlite::fromJSON(rawToChar(raw_content), simplifyVector = TRUE) },
    error = function(e) stop("Error processing JSON from Groq API: ", e$message)
  )

  models_list <- NULL
  if (!is.null(parsed_content) && is.list(parsed_content) && "data" %in% names(parsed_content)) {
    if (is.data.frame(parsed_content$data)) {
      models_list <- parsed_content$data
    } else if (is.list(parsed_content$data)) {
      models_list <- tryCatch({
        list_data <- purrr::map(parsed_content$data, ~ as.list(.x))
        dplyr::bind_rows(!!!list_data)
      }, error = function(e) {
        warning("Field 'data' is a list but failed conversion to data frame: ", e$message)
        return(NULL)
      })
      if (is.null(models_list)) stop("Could not process the 'data' list from the Groq API response.")
    }
  }

  if (is.null(models_list) || nrow(models_list) == 0) {
    if (verbose) print(utils::str(parsed_content))
    stop("Field 'data' not found, empty, or not processable in Groq API response.")
  }

  if (verbose) message("Processing ", nrow(models_list), " Groq models...")

  output_df <- purrr::map_df(seq_len(nrow(models_list)), function(i) {
    model_info <- models_list[i, ]
    model_id_safe <- model_info$id %||% model_info$model %||% model_info$name %||% paste0("UNKNOWN_ID_", i)

    model_type <- dplyr::case_when(
      grepl("vision|vl|llava|minicpm|moondream|qwen2\\.5-vl|gemma3", model_id_safe, ignore.case = TRUE, perl = TRUE) ~ "Vision",
      grepl("whisper|audio|tts|speech", model_id_safe, ignore.case = TRUE) ~ "Audio",
      grepl("embed|embedding", model_id_safe, ignore.case = TRUE) ~ "Embedding",
      TRUE ~ "Chat"
    )

    tibble::tibble(
      service = "groq",
      model = model_id_safe,
      type = model_type,
      pricing = "",
      description = ""
    )
  })

  if (nrow(output_df) > 0) {
    output_df <- output_df %>%
      dplyr::mutate(
        pricing = sprintf('"%s"', pricing),
        description = sprintf('"%s"', description)
      )
  }

  if (!dir.exists(directory)) {
    if (verbose) message("Creating directory: ", directory)
    dir.create(directory, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(directory)) stop("Failed to create directory: ", directory)
  }

  file_path <- file.path(directory, "groq.csv")
  if (verbose) message("\nSaving ", nrow(output_df), " Groq models to: ", file_path)
  tryCatch({
    write.table(output_df, file = file_path, sep = ",", quote = FALSE,
      row.names = FALSE, col.names = TRUE, na = "", fileEncoding = "UTF-8"
    )
  }, error = function(e) stop("Error saving CSV '", file_path, "' with write.table: ", conditionMessage(e)))

  if (verbose) message("File 'groq.csv' updated successfully.")
  invisible(output_df)
}
#' Update Cerebras models list (internal)
#'
#' Connects to the Cerebras API, retrieves the list of models and saves a
#' normalized CSV file named `cerebras.csv` in the provided directory.
#'
#' - Validates environment variable `CEREBRAS_API_KEY`.
#'
#' @param directory Character path where the CSV will be saved. If NULL, uses current working dir.
#' @param verbose Logical flag to print progress messages.
#' @return Invisibly returns a data frame with the processed models.
#' @keywords internal
#' @noRd
.update_models_cerebras <- function(directory = NULL, verbose = TRUE) {
  api_key <- trimws(Sys.getenv("CEREBRAS_API_KEY"))
  if (!nzchar(api_key)) stop("Error: Environment variable 'CEREBRAS_API_KEY' not set.")

  base_url <- trimws(Sys.getenv("CEREBRAS_BASE_URL", "https://api.cerebras.ai"))
  if (!nzchar(base_url)) {
    base_url <- "https://api.cerebras.ai"
  }
  base_url <- sub("/+$", "", base_url)

  api_url <- paste0(base_url, "/v1/models")
  headers <- httr::add_headers("Authorization" = paste("Bearer", api_key))

  if (verbose) message("Connecting to the Cerebras API...")
  response <- tryCatch({ httr::GET(url = api_url, config = headers, httr::timeout(60)) },
    error = function(e) stop("Error connecting to the Cerebras API: ", e$message)
  )

  if (httr::status_code(response) != 200) {
    error_content <- httr::content(response, "text", encoding = "UTF-8")
    stop("Cerebras API Error (Status: ", httr::status_code(response), "): ", error_content)
  }

  if (verbose) message("Processing JSON response...")
  raw_content <- httr::content(response, "raw")
  parsed_content <- tryCatch({ jsonlite::fromJSON(rawToChar(raw_content), simplifyVector = TRUE) },
    error = function(e) stop("Error processing JSON from Cerebras API: ", e$message)
  )

  models_list <- NULL
  if (!is.null(parsed_content) && is.list(parsed_content) && "data" %in% names(parsed_content)) {
    if (is.data.frame(parsed_content$data)) {
      models_list <- parsed_content$data
    } else if (is.list(parsed_content$data)) {
      models_list <- tryCatch({
        list_data <- purrr::map(parsed_content$data, ~ as.list(.x))
        dplyr::bind_rows(!!!list_data)
      }, error = function(e) {
        warning("Field 'data' is a list but failed conversion to data frame: ", e$message)
        return(NULL)
      })
      if (is.null(models_list)) stop("Could not process the 'data' list from the Cerebras API response.")
    }
  }

  if (is.null(models_list) || nrow(models_list) == 0) {
    if (verbose) print(utils::str(parsed_content))
    stop("Field 'data' not found, empty, or not processable in Cerebras API response.")
  }

  if (verbose) message("Processing ", nrow(models_list), " Cerebras models...")

  output_df <- purrr::map_df(seq_len(nrow(models_list)), function(i) {
    model_info <- models_list[i, ]
    model_id_safe <- model_info$id %||% model_info$model %||% model_info$name %||% paste0("UNKNOWN_ID_", i)
    model_text <- tolower(model_id_safe)
    model_type <- dplyr::case_when(
      grepl("vision|vl|llava|minicpm|moondream|qwen2\\.5-vl|gemma3", model_text, perl = TRUE) ~ "Vision",
      grepl("whisper|audio|tts|speech", model_text) ~ "Audio",
      grepl("embed|embedding", model_text) ~ "Embedding",
      TRUE ~ "Chat"
    )

    tibble::tibble(
      service = "cerebras",
      model = model_id_safe,
      type = model_type,
      pricing = "",
      description = ""
    )
  })

  if (nrow(output_df) > 0) {
    output_df <- output_df %>%
      dplyr::mutate(
        pricing = sprintf('"%s"', pricing),
        description = sprintf('"%s"', description)
      )
  }

  if (!dir.exists(directory)) {
    if (verbose) message("Creating directory: ", directory)
    dir.create(directory, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(directory)) stop("Failed to create directory: ", directory)
  }

  file_path <- file.path(directory, "cerebras.csv")
  if (verbose) message("\nSaving ", nrow(output_df), " Cerebras models to: ", file_path)
  tryCatch({
    write.table(output_df, file = file_path, sep = ",", quote = FALSE,
      row.names = FALSE, col.names = TRUE, na = "", fileEncoding = "UTF-8"
    )
  }, error = function(e) stop("Error saving CSV '", file_path, "' with write.table: ", conditionMessage(e)))

  if (verbose) message("File 'cerebras.csv' updated successfully.")
  invisible(output_df)
}
#' Update Together models list (internal)
#'
#' Connects to the Together API, retrieves the list of models and saves a
#' normalized CSV file named `together.csv` in the provided directory.
#'
#' - Validates environment variable `TOGETHER_API_KEY`.
#'
#' @param directory Character path where the CSV will be saved. If NULL, uses current working dir.
#' @param verbose Logical flag to print progress messages.
#' @return Invisibly returns a data frame with the processed models.
#' @keywords internal
#' @noRd
.update_models_together <- function(directory = NULL, verbose = TRUE) {
  api_key <- trimws(Sys.getenv("TOGETHER_API_KEY"))
  if (!nzchar(api_key)) stop("Error: Environment variable 'TOGETHER_API_KEY' not set.")

  base_url <- trimws(Sys.getenv("TOGETHER_BASE_URL", "https://api.together.xyz"))
  if (!nzchar(base_url)) {
    base_url <- "https://api.together.xyz"
  }
  base_url <- sub("/+$", "", base_url)

  api_url <- paste0(base_url, "/v1/models")
  headers <- httr::add_headers("Authorization" = paste("Bearer", api_key))

  if (verbose) message("Connecting to the Together API...")
  response <- tryCatch({ httr::GET(url = api_url, config = headers, httr::timeout(60)) },
    error = function(e) stop("Error connecting to the Together API: ", e$message)
  )

  if (httr::status_code(response) != 200) {
    error_content <- httr::content(response, "text", encoding = "UTF-8")
    stop("Together API Error (Status: ", httr::status_code(response), "): ", error_content)
  }

  if (verbose) message("Processing JSON response...")
  raw_content <- httr::content(response, "raw")
  parsed_content <- tryCatch({ jsonlite::fromJSON(rawToChar(raw_content), simplifyVector = TRUE) },
    error = function(e) stop("Error processing JSON from Together API: ", e$message)
  )

  models_list <- NULL
  if (!is.null(parsed_content) && is.list(parsed_content) && "data" %in% names(parsed_content)) {
    if (is.data.frame(parsed_content$data)) {
      models_list <- parsed_content$data
    } else if (is.list(parsed_content$data)) {
      models_list <- tryCatch({
        list_data <- purrr::map(parsed_content$data, ~ as.list(.x))
        dplyr::bind_rows(!!!list_data)
      }, error = function(e) {
        warning("Field 'data' is a list but failed conversion to data frame: ", e$message)
        return(NULL)
      })
      if (is.null(models_list)) stop("Could not process the 'data' list from the Together API response.")
    }
  }

  if (is.null(models_list) || nrow(models_list) == 0) {
    if (verbose) print(utils::str(parsed_content))
    stop("Field 'data' not found, empty, or not processable in Together API response.")
  }

  if (verbose) message("Processing ", nrow(models_list), " Together models...")

  output_df <- purrr::map_df(seq_len(nrow(models_list)), function(i) {
    model_info <- models_list[i, ]
    model_id_safe <- model_info$id %||% model_info$model %||% model_info$name %||% paste0("UNKNOWN_ID_", i)
    model_text <- tolower(model_id_safe)
    model_type <- dplyr::case_when(
      grepl("vision|vl|llava|minicpm|moondream|qwen2\\.5-vl|gemma3", model_text, perl = TRUE) ~ "Vision",
      grepl("whisper|audio|tts|speech", model_text) ~ "Audio",
      grepl("embed|embedding", model_text) ~ "Embedding",
      TRUE ~ "Chat"
    )

    tibble::tibble(
      service = "together",
      model = model_id_safe,
      type = model_type,
      pricing = "",
      description = ""
    )
  })

  if (nrow(output_df) > 0) {
    output_df <- output_df %>%
      dplyr::mutate(
        pricing = sprintf('"%s"', pricing),
        description = sprintf('"%s"', description)
      )
  }

  if (!dir.exists(directory)) {
    if (verbose) message("Creating directory: ", directory)
    dir.create(directory, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(directory)) stop("Failed to create directory: ", directory)
  }

  file_path <- file.path(directory, "together.csv")
  if (verbose) message("\nSaving ", nrow(output_df), " Together models to: ", file_path)
  tryCatch({
    write.table(output_df, file = file_path, sep = ",", quote = FALSE,
      row.names = FALSE, col.names = TRUE, na = "", fileEncoding = "UTF-8"
    )
  }, error = function(e) stop("Error saving CSV '", file_path, "' with write.table: ", conditionMessage(e)))

  if (verbose) message("File 'together.csv' updated successfully.")
  invisible(output_df)
}
#' Update SambaNova models list (internal)
#'
#' Connects to the SambaNova API, retrieves the list of models and saves a
#' normalized CSV file named `sambanova.csv` in the provided directory.
#'
#' - Validates environment variable `SAMBANOVA_API_KEY`.
#'
#' @param directory Character path where the CSV will be saved. If NULL, uses current working dir.
#' @param verbose Logical flag to print progress messages.
#' @return Invisibly returns a data frame with the processed models.
#' @keywords internal
#' @noRd
.update_models_sambanova <- function(directory = NULL, verbose = TRUE) {
  api_key <- trimws(Sys.getenv("SAMBANOVA_API_KEY"))
  if (!nzchar(api_key)) {
    api_key <- trimws(Sys.getenv("SAMBA_API_KEY"))
  }
  if (!nzchar(api_key)) stop("Error: Environment variable 'SAMBANOVA_API_KEY' not set.")

  base_url <- trimws(Sys.getenv("SAMBANOVA_BASE_URL", "https://api.sambanova.ai"))
  if (!nzchar(base_url)) {
    base_url <- "https://api.sambanova.ai"
  }
  base_url <- sub("/+$", "", base_url)

  api_url <- paste0(base_url, "/v1/models")
  headers <- httr::add_headers("Authorization" = paste("Bearer", api_key))

  if (verbose) message("Connecting to the SambaNova API...")
  response <- tryCatch({ httr::GET(url = api_url, config = headers, httr::timeout(60)) },
    error = function(e) stop("Error connecting to the SambaNova API: ", e$message)
  )

  if (httr::status_code(response) != 200) {
    error_content <- httr::content(response, "text", encoding = "UTF-8")
    stop("SambaNova API Error (Status: ", httr::status_code(response), "): ", error_content)
  }

  if (verbose) message("Processing JSON response...")
  raw_content <- httr::content(response, "raw")
  parsed_content <- tryCatch({ jsonlite::fromJSON(rawToChar(raw_content), simplifyVector = TRUE) },
    error = function(e) stop("Error processing JSON from SambaNova API: ", e$message)
  )

  models_list <- NULL
  if (!is.null(parsed_content) && is.list(parsed_content) && "data" %in% names(parsed_content)) {
    if (is.data.frame(parsed_content$data)) {
      models_list <- parsed_content$data
    } else if (is.list(parsed_content$data)) {
      models_list <- tryCatch({
        list_data <- purrr::map(parsed_content$data, ~ as.list(.x))
        dplyr::bind_rows(!!!list_data)
      }, error = function(e) {
        warning("Field 'data' is a list but failed conversion to data frame: ", e$message)
        return(NULL)
      })
      if (is.null(models_list)) stop("Could not process the 'data' list from the SambaNova API response.")
    }
  }

  if (is.null(models_list) || nrow(models_list) == 0) {
    if (verbose) print(utils::str(parsed_content))
    stop("Field 'data' not found, empty, or not processable in SambaNova API response.")
  }

  if (verbose) message("Processing ", nrow(models_list), " SambaNova models...")

  output_df <- purrr::map_df(seq_len(nrow(models_list)), function(i) {
    model_info <- models_list[i, ]
    model_id_safe <- model_info$id %||% model_info$model %||% model_info$name %||% paste0("UNKNOWN_ID_", i)
    model_text <- tolower(model_id_safe)
    model_type <- dplyr::case_when(
      grepl("vision|vl|llava|minicpm|moondream|qwen2\\.5-vl|gemma3", model_text, perl = TRUE) ~ "Vision",
      grepl("whisper|audio|tts|speech", model_text) ~ "Audio",
      grepl("embed|embedding", model_text) ~ "Embedding",
      TRUE ~ "Chat"
    )

    tibble::tibble(
      service = "sambanova",
      model = model_id_safe,
      type = model_type,
      pricing = "",
      description = ""
    )
  })

  if (nrow(output_df) > 0) {
    output_df <- output_df %>%
      dplyr::mutate(
        pricing = sprintf('"%s"', pricing),
        description = sprintf('"%s"', description)
      )
  }

  if (!dir.exists(directory)) {
    if (verbose) message("Creating directory: ", directory)
    dir.create(directory, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(directory)) stop("Failed to create directory: ", directory)
  }

  file_path <- file.path(directory, "sambanova.csv")
  if (verbose) message("\nSaving ", nrow(output_df), " SambaNova models to: ", file_path)
  tryCatch({
    write.table(output_df, file = file_path, sep = ",", quote = FALSE,
      row.names = FALSE, col.names = TRUE, na = "", fileEncoding = "UTF-8"
    )
  }, error = function(e) stop("Error saving CSV '", file_path, "' with write.table: ", conditionMessage(e)))

  if (verbose) message("File 'sambanova.csv' updated successfully.")
  invisible(output_df)
}

#' @keywords internal
#' @noRd
.update_models_openai_compat <- function(directory = NULL,
                                         verbose = TRUE,
                                         provider_id,
                                         provider_name,
                                         api_key,
                                         base_url,
                                         base_urls = NULL,
                                         model_paths = c("/v1/models", "/models"),
                                         auth_header = "Authorization",
                                         auth_prefix = "Bearer",
                                         extra_headers = NULL,
                                         api_key_required = TRUE) {
  if (isTRUE(api_key_required) && !nzchar(api_key)) {
    stop("Error: API key not set for provider '", provider_id, "'.")
  }

  base_url_candidates <- c(base_url, base_urls %||% character())
  base_url_candidates <- as.character(base_url_candidates)
  base_url_candidates <- trimws(base_url_candidates)
  base_url_candidates <- sub("/+$", "", base_url_candidates)
  base_url_candidates <- base_url_candidates[nzchar(base_url_candidates)]
  base_url_candidates <- unique(base_url_candidates)
  if (!length(base_url_candidates)) {
    stop("Error: Base URL is empty for provider '", provider_id, "'.")
  }

  header_args <- list()
  if (!is.null(extra_headers) && length(extra_headers) > 0) {
    header_args <- c(header_args, extra_headers)
  }
  if (nzchar(auth_header) && nzchar(api_key)) {
    auth_value <- if (nzchar(auth_prefix)) paste(auth_prefix, api_key) else api_key
    header_args[[auth_header]] <- auth_value
  }
  headers <- do.call(httr::add_headers, header_args)

  model_paths <- unique(as.character(model_paths))
  parsed_content <- NULL
  api_url_used <- ""
  for (base_candidate in base_url_candidates) {
    for (model_path in model_paths) {
      if (!nzchar(model_path)) {
        next
      }
      api_url <- if (grepl("^https?://", model_path, ignore.case = TRUE)) {
        model_path
      } else {
        paste0(base_candidate, model_path)
      }

      if (verbose) message("Connecting to the ", provider_name, " API at ", api_url, " ...")
      response_try <- tryCatch({ httr::GET(url = api_url, config = headers, httr::timeout(60)) },
        error = function(e) NULL
      )
      if (is.null(response_try) || httr::status_code(response_try) != 200) {
        next
      }

      parsed_try <- tryCatch({
        raw_content <- httr::content(response_try, "raw")
        jsonlite::fromJSON(rawToChar(raw_content), simplifyVector = TRUE)
      }, error = function(e) NULL)
      if (is.null(parsed_try) || !is.list(parsed_try)) {
        next
      }

      parsed_content <- parsed_try
      api_url_used <- api_url
      break
    }
    if (!is.null(parsed_content)) break
  }

  if (is.null(parsed_content)) {
    stop(provider_name, " API Error: unable to fetch models from candidate endpoints.")
  }

  if (verbose) message("Processing JSON response from ", api_url_used, " ...")

  models_list <- NULL
  if (!is.null(parsed_content) && is.list(parsed_content) && "data" %in% names(parsed_content)) {
    if (is.data.frame(parsed_content$data)) {
      models_list <- parsed_content$data
    } else if (is.list(parsed_content$data)) {
      models_list <- tryCatch({
        list_data <- purrr::map(parsed_content$data, ~ as.list(.x))
        dplyr::bind_rows(!!!list_data)
      }, error = function(e) {
        warning("Field 'data' is a list but failed conversion to data frame: ", e$message)
        return(NULL)
      })
      if (is.null(models_list)) stop("Could not process the 'data' list from the ", provider_name, " API response.")
    }
  }

  if (is.null(models_list) || nrow(models_list) == 0) {
    if (verbose) print(utils::str(parsed_content))
    stop("Field 'data' not found, empty, or not processable in ", provider_name, " API response.")
  }

  if (verbose) message("Processing ", nrow(models_list), " ", provider_name, " models...")

  output_df <- purrr::map_df(seq_len(nrow(models_list)), function(i) {
    model_info <- models_list[i, ]
    model_id_safe <- model_info$id %||% model_info$model %||% model_info$name %||% paste0("UNKNOWN_ID_", i)
    model_text <- tolower(model_id_safe)
    model_type <- dplyr::case_when(
      grepl("vision|vl|llava|minicpm|moondream|qwen2\\.5-vl|gemma3", model_text, perl = TRUE) ~ "Vision",
      grepl("whisper|audio|tts|speech", model_text) ~ "Audio",
      grepl("embed|embedding", model_text) ~ "Embedding",
      TRUE ~ "Chat"
    )

    tibble::tibble(
      service = provider_id,
      model = model_id_safe,
      type = model_type,
      pricing = "",
      description = ""
    )
  })

  if (nrow(output_df) > 0) {
    output_df <- output_df %>%
      dplyr::mutate(
        pricing = sprintf('"%s"', pricing),
        description = sprintf('"%s"', description)
      )
  }

  if (!dir.exists(directory)) {
    if (verbose) message("Creating directory: ", directory)
    dir.create(directory, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(directory)) stop("Failed to create directory: ", directory)
  }

  file_path <- file.path(directory, paste0(provider_id, ".csv"))
  if (verbose) message("\nSaving ", nrow(output_df), " ", provider_name, " models to: ", file_path)
  tryCatch({
    write.table(output_df, file = file_path, sep = ",", quote = FALSE,
      row.names = FALSE, col.names = TRUE, na = "", fileEncoding = "UTF-8"
    )
  }, error = function(e) stop("Error saving CSV '", file_path, "' with write.table: ", conditionMessage(e)))

  if (verbose) message("File '", provider_id, ".csv' updated successfully.")
  invisible(output_df)
}

#' Update Nebius models list (internal)
#'
#' @param directory Character path where the CSV will be saved. If NULL, uses current working dir.
#' @param verbose Logical flag to print progress messages.
#' @return Invisibly returns a data frame with the processed models.
#' @keywords internal
#' @noRd
.update_models_nebius <- function(directory = NULL, verbose = TRUE) {
  api_key <- trimws(Sys.getenv("NEBIUS_API_KEY"))
  if (!nzchar(api_key)) stop("Error: Environment variable 'NEBIUS_API_KEY' not set.")
  base_url <- trimws(Sys.getenv("NEBIUS_BASE_URL", "https://api.studio.nebius.ai"))
  .update_models_openai_compat(
    directory = directory,
    verbose = verbose,
    provider_id = "nebius",
    provider_name = "Nebius",
    api_key = api_key,
    base_url = base_url,
    model_paths = c("/v1/models", "/models")
  )
}

#' Update DeepSeek models list (internal)
#'
#' @param directory Character path where the CSV will be saved. If NULL, uses current working dir.
#' @param verbose Logical flag to print progress messages.
#' @return Invisibly returns a data frame with the processed models.
#' @keywords internal
#' @noRd
.update_models_deepseek <- function(directory = NULL, verbose = TRUE) {
  api_key <- trimws(Sys.getenv("DEEPSEEK_API_KEY"))
  if (!nzchar(api_key)) stop("Error: Environment variable 'DEEPSEEK_API_KEY' not set.")
  base_url <- trimws(Sys.getenv("DEEPSEEK_BASE_URL", "https://api.deepseek.com"))
  .update_models_openai_compat(
    directory = directory,
    verbose = verbose,
    provider_id = "deepseek",
    provider_name = "DeepSeek",
    api_key = api_key,
    base_url = base_url,
    model_paths = c("/models", "/v1/models")
  )
}

#' Update Perplexity models list (internal)
#'
#' @param directory Character path where the CSV will be saved. If NULL, uses current working dir.
#' @param verbose Logical flag to print progress messages.
#' @return Invisibly returns a data frame with the processed models.
#' @keywords internal
#' @noRd
.update_models_perplexity <- function(directory = NULL, verbose = TRUE) {
  api_key <- trimws(Sys.getenv("PERPLEXITY_API_KEY"))
  if (!nzchar(api_key)) stop("Error: Environment variable 'PERPLEXITY_API_KEY' not set.")
  base_url <- trimws(Sys.getenv("PERPLEXITY_BASE_URL", "https://api.perplexity.ai"))
  .update_models_openai_compat(
    directory = directory,
    verbose = verbose,
    provider_id = "perplexity",
    provider_name = "Perplexity",
    api_key = api_key,
    base_url = base_url,
    model_paths = c("/models", "/v1/models")
  )
}

#' Update Fireworks models list (internal)
#'
#' @param directory Character path where the CSV will be saved. If NULL, uses current working dir.
#' @param verbose Logical flag to print progress messages.
#' @return Invisibly returns a data frame with the processed models.
#' @keywords internal
#' @noRd
.update_models_fireworks <- function(directory = NULL, verbose = TRUE) {
  api_key <- trimws(Sys.getenv("FIREWORKS_API_KEY"))
  if (!nzchar(api_key)) stop("Error: Environment variable 'FIREWORKS_API_KEY' not set.")
  base_url <- trimws(Sys.getenv("FIREWORKS_BASE_URL", "https://api.fireworks.ai/inference"))
  .update_models_openai_compat(
    directory = directory,
    verbose = verbose,
    provider_id = "fireworks",
    provider_name = "Fireworks",
    api_key = api_key,
    base_url = base_url,
    model_paths = c("/v1/models", "/models")
  )
}

#' Update DeepInfra models list (internal)
#'
#' @param directory Character path where the CSV will be saved. If NULL, uses current working dir.
#' @param verbose Logical flag to print progress messages.
#' @return Invisibly returns a data frame with the processed models.
#' @keywords internal
#' @noRd
.update_models_deepinfra <- function(directory = NULL, verbose = TRUE) {
  api_key <- trimws(Sys.getenv("DEEPINFRA_API_KEY"))
  if (!nzchar(api_key)) stop("Error: Environment variable 'DEEPINFRA_API_KEY' not set.")
  base_url <- trimws(Sys.getenv("DEEPINFRA_BASE_URL", "https://api.deepinfra.com/v1/openai"))
  .update_models_openai_compat(
    directory = directory,
    verbose = verbose,
    provider_id = "deepinfra",
    provider_name = "DeepInfra",
    api_key = api_key,
    base_url = base_url,
    model_paths = c("/models", "/v1/models")
  )
}

#' Update Hyperbolic models list (internal)
#'
#' @param directory Character path where the CSV will be saved. If NULL, uses current working dir.
#' @param verbose Logical flag to print progress messages.
#' @return Invisibly returns a data frame with the processed models.
#' @keywords internal
#' @noRd
.update_models_hyperbolic <- function(directory = NULL, verbose = TRUE) {
  api_key <- trimws(Sys.getenv("HYPERBOLIC_API_KEY"))
  if (!nzchar(api_key)) stop("Error: Environment variable 'HYPERBOLIC_API_KEY' not set.")
  base_url <- trimws(Sys.getenv("HYPERBOLIC_BASE_URL", "https://api.hyperbolic.xyz"))
  .update_models_openai_compat(
    directory = directory,
    verbose = verbose,
    provider_id = "hyperbolic",
    provider_name = "Hyperbolic",
    api_key = api_key,
    base_url = base_url,
    model_paths = c("/v1/models", "/models")
  )
}

#' @keywords internal
#' @noRd
.update_models_custom_openai_compat <- function(provider_id, directory = NULL, verbose = TRUE) {
  provider_cfg <- .genflow_get_custom_provider(provider_id)
  if (is.null(provider_cfg)) {
    stop("Custom provider '", provider_id, "' was not found.")
  }

  api_key <- ""
  api_key_env <- as.character(provider_cfg$api_key_env %||% "")[1]
  if (nzchar(api_key_env)) {
    api_key <- trimws(Sys.getenv(api_key_env, ""))
  }

  base_urls <- as.character(provider_cfg$base_urls %||% character())
  base_url <- if (length(base_urls)) base_urls[[1]] else ""

  .update_models_openai_compat(
    directory = directory,
    verbose = verbose,
    provider_id = provider_cfg$id %||% provider_id,
    provider_name = provider_cfg$label %||% provider_id,
    api_key = api_key,
    base_url = base_url,
    base_urls = base_urls,
    model_paths = provider_cfg$model_paths %||% c("/v1/models", "/models"),
    auth_header = as.character(provider_cfg$auth_header %||% "Authorization")[1],
    auth_prefix = as.character(provider_cfg$auth_prefix %||% "Bearer")[1],
    extra_headers = provider_cfg$extra_headers %||% list(),
    api_key_required = isTRUE(provider_cfg$api_key_required)
  )
}

#' Update Gemini models list (internal)
#'
#' Connects to the Gemini API, follows model-list pagination, and saves a
#' normalized CSV file in the provided directory.
#'
#' @param directory Character path where the CSV will be saved. If NULL, uses current working dir.
#' @param verbose Logical flag to print progress messages.
#' @return Invisibly returns a data frame with the processed models.
#' @keywords internal
#' @noRd
.update_models_gemini <- function(directory = NULL, verbose = TRUE) {
  api_key <- trimws(Sys.getenv("GOOGLE_API_KEY", ""))
  if (!nzchar(api_key)) {
    api_key <- trimws(Sys.getenv("GEMINI_API_KEY", ""))
  }
  if (!nzchar(api_key)) {
    stop("Environment variable GOOGLE_API_KEY or GEMINI_API_KEY not set.")
  }

  if (is.null(directory) || !length(directory) || is.na(directory[[1]]) ||
      !nzchar(trimws(as.character(directory[[1]])))) {
    stop("`directory` must be one non-empty path.", call. = FALSE)
  }
  directory <- path.expand(as.character(directory[[1]]))
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(directory)) {
    stop("Failed to create directory: ", directory, call. = FALSE)
  }

  headers <- httr::add_headers("x-goog-api-key" = api_key)
  endpoint <- "https://generativelanguage.googleapis.com/v1beta/models"
  page_token <- ""
  seen_tokens <- character()
  models <- list()

  if (verbose) message("Connecting to the Gemini models API...")
  repeat {
    query <- list(pageSize = 1000L)
    if (nzchar(page_token)) {
      query$pageToken <- page_token
    }
    response <- tryCatch(
      httr::GET(
        url = endpoint,
        headers,
        query = query,
        httr::timeout(60)
      ),
      error = function(e) {
        stop(
          "Error connecting to the Gemini models API: ",
          conditionMessage(e),
          call. = FALSE
        )
      }
    )
    if (httr::status_code(response) != 200L) {
      error_content <- httr::content(response, "text", encoding = "UTF-8")
      error_details <- tryCatch({
        parsed <- jsonlite::fromJSON(error_content, simplifyVector = FALSE)
        parsed$error$message %||% parsed$message %||% error_content
      }, error = function(e) error_content)
      stop(
        "Gemini models API returned HTTP ",
        httr::status_code(response),
        ": ",
        substr(as.character(error_details)[1], 1L, 1000L),
        call. = FALSE
      )
    }

    parsed <- tryCatch(
      httr::content(
        response,
        as = "parsed",
        type = "application/json",
        encoding = "UTF-8"
      ),
      error = function(e) {
        stop(
          "Could not parse the Gemini models response: ",
          conditionMessage(e),
          call. = FALSE
        )
      }
    )
    page_models <- parsed$models %||% list()
    if (is.data.frame(page_models) && nrow(page_models)) {
      page_models <- lapply(seq_len(nrow(page_models)), function(i) {
        as.list(page_models[i, , drop = FALSE])
      })
    }
    if (is.list(page_models)) {
      models <- c(models, page_models)
    }

    next_token <- trimws(as.character(parsed$nextPageToken %||% "")[1])
    if (!nzchar(next_token)) {
      break
    }
    if (next_token %in% seen_tokens) {
      stop("Gemini model pagination repeated a page token.", call. = FALSE)
    }
    seen_tokens <- c(seen_tokens, next_token)
    page_token <- next_token
  }

  rows <- lapply(models, function(model_info) {
    if (!is.list(model_info)) {
      return(NULL)
    }
    model_id <- sub(
      "^models/",
      "",
      trimws(as.character(model_info$name %||% "")[1])
    )
    if (!nzchar(model_id)) {
      return(NULL)
    }
    methods <- unique(as.character(unlist(
      model_info$supportedGenerationMethods %||% character(),
      use.names = FALSE
    )))
    model_type <- if ("generateContent" %in% methods) {
      "Chat"
    } else if ("embedContent" %in% methods ||
               grepl("(^|-)embedding", model_id, ignore.case = TRUE)) {
      "Embedding"
    } else {
      "Unknown"
    }
    data.frame(
      service = "gemini",
      model = model_id,
      type = model_type,
      pricing = "",
      description = as.character(model_info$description %||%
        model_info$displayName %||% "")[1],
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) {
    stop("The Gemini models API returned no usable models.", call. = FALSE)
  }
  output_df <- unique(do.call(rbind, rows))
  output_df <- output_df[order(output_df$type, output_df$model), , drop = FALSE]
  rownames(output_df) <- NULL

  file_path <- file.path(directory, "gemini.csv")
  utils::write.csv(
    output_df,
    file = file_path,
    row.names = FALSE,
    na = "",
    fileEncoding = "UTF-8"
  )
  if (verbose) {
    message(
      "File 'gemini.csv' updated successfully with ",
      nrow(output_df),
      " models."
    )
  }
  invisible(output_df)
}
#' Update FAL models list (internal)
#'
#' Connects to the FAL API, retrieves models from multiple pages and saves a
#' normalized CSV file in the provided directory.
#'
#' @param directory Character path where the CSV will be saved.
#' @param verbose Logical flag to print progress messages.
#' @return Invisibly returns a data frame with the processed models.
#' @keywords internal
#' @noRd
.update_models_fal <- function(directory = NULL, verbose = TRUE) {

  # --- 2. API Key ---
  api_key <- Sys.getenv("FAL_API_KEY")
  if (api_key == "") stop("Error: Environment variable 'FAL_API_KEY' not set.")

  # --- 3. API URL & Headers ---
  api_url <- "https://fal.ai/api/models"
  headers <- httr::add_headers(Authorization = paste("Key", api_key))

  # --- 4. Helper to fetch one page ---
  # Internal helper: fetch a page from FAL models endpoint
  fetch_page <- function(page, size) {
    if (verbose) message("  Fetching page ", page)
    res <- httr::GET(api_url, headers, query = list(page = page, size = size), httr::timeout(60))
    if (httr::status_code(res) != 200)
      stop("Fal API returned status ", httr::status_code(res), " on page ", page)
    jsonlite::fromJSON(rawToChar(httr::content(res, "raw")), simplifyVector = TRUE)
  }

  # --- 5. Fetch first page ---
  if (verbose) message("Connecting to the Fal API...")
  first_json <- fetch_page(1, 40)
  if (!"items" %in% names(first_json)) stop("Field 'items' not found.")

  items_all   <- first_json$items
  total_pages <- first_json$pages %||% 1
  page_size   <- first_json$size  %||% nrow(items_all)

  if (total_pages > 1) {
    for (p in 2:total_pages) {
      items_all <- dplyr::bind_rows(items_all, fetch_page(p, page_size)$items)
    }
  }

  if (verbose) message("Processing ", nrow(items_all), " Fal models...")

  output_df <- purrr::map_df(seq_len(nrow(items_all)), function(i) {
    row <- items_all[i, , drop = FALSE]
    id <- row$id %||% row$modelId %||% paste0("UNKNOWN_", i)

    combined <- paste(tolower(row$id %||% ""), tolower(row$category %||% ""), tolower(row$shortDescription %||% ""), sep = " ")

    model_type <- dplyr::case_when(
      grepl("image-to-video|text-to-video", combined) ~ "Video",
      grepl("image-to-image|text-to-image", combined) ~ "Image",
      grepl("stable-diffusion|sdxl|ideogram|kandinsky|hidream|recraft|photomaker|image|video", combined, perl = TRUE) ~ "Image",
      grepl("llava", combined)    ~ "Vision",
      grepl("whisper", combined)  ~ "Audio",
      grepl("llama|mistral|falcon", combined) ~ "Chat",
      TRUE ~ "Unknown"
    )

    tibble::tibble(
      service     = "fal",
      model       = id,
      type        = model_type,
      pricing     = sprintf('"%s"', row$pricingInfoOverride[[1]] %||% ""),
      description = sprintf('"%s"', row$shortDescription[[1]]    %||% "")
    )
  })

  # --- 6. Ensure directory and save ---
  if (!dir.exists(directory)) {
    if (verbose) message("Creating directory: ", directory)
    dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  }
  file_path <- file.path(directory, "fal.csv")
  if (verbose) message("\nSaving ", nrow(output_df), " Fal models to: ", file_path)
  write.table(output_df, file = file_path, sep = ",", quote = FALSE,
              row.names = FALSE, col.names = TRUE, na = "", fileEncoding = "UTF-8")
  if (verbose) message("File 'fal.csv' updated successfully.")
  invisible(output_df)
}
#' Update Replicate models list (internal)
#'
#' Fetches models from Replicate collections, merges and normalizes results and
#' saves a CSV file in the provided directory.
#'
#' @param directory Character path where the CSV will be saved.
#' @param verbose Logical flag for progress messages.
#' @param collection_page_limit Optional integer to limit the number of collection pages.
#' @param delay Numeric delay (seconds) between paginated API calls.
#' @return Invisibly returns a data frame with the processed models.
#' @keywords internal
#' @noRd
.update_models_replicate <- function(directory = NULL, verbose = TRUE, collection_page_limit = NULL, delay = 0.25) {
  # --- Utility ---
  `%||na||%` <- function(a, b) if (!is.null(a) && !is.na(a)) a else b

  # --- 2. API Key ---
  api_key_replicate <- Sys.getenv("REPLICATE_API_TOKEN")
  if (api_key_replicate == "") stop("Error: Environment variable 'REPLICATE_API_TOKEN' not set.")

  # --- 3. Auth Header ---
  headers <- httr::add_headers("Authorization" = paste("Bearer", api_key_replicate))

  # --- 4. Step 1: Fetch collection slugs ---
  if (verbose) message("Connecting to the Replicate API...")
  all_collections_list <- list()
  page_count_collections <- 0
  current_collections_url <- "https://api.replicate.com/v1/collections"

  if (verbose) message("Step 1: Fetching collection list with pagination...")
  while (!is.null(current_collections_url) && nzchar(current_collections_url)) {
    page_count_collections <- page_count_collections + 1
    if (!is.null(collection_page_limit) && page_count_collections > collection_page_limit) {
      if (verbose) message("Reached collection page limit (", collection_page_limit, "). Stopping fetch.")
      break
    }
    if (verbose) message("  Fetching collections page ", page_count_collections, ": ", current_collections_url)
    response_collections <- tryCatch({ httr::GET(url = current_collections_url, config = headers, httr::timeout(60)) },
                                     error = function(e) NULL)
    if (is.null(response_collections) || httr::status_code(response_collections) != 200) {
      error_content <- if (!is.null(response_collections)) httr::content(response_collections, "text", encoding = "UTF-8") else "Conn error"
      warning("API Error (Status: ", if (!is.null(response_collections)) httr::status_code(response_collections) else "N/A",
              ") fetching collections page ", page_count_collections, ". Stopping collection fetch. Error: ", substr(error_content, 1, 200))
      current_collections_url <- NULL
      next
    }
    next_collections_page_url <- NULL
    tryCatch({
      collections_text <- httr::content(response_collections, as = "text", encoding = "UTF-8")
      if (is.null(collections_text) || !jsonlite::validate(collections_text)[1]) stop("Invalid JSON for collections")
      parsed_collections <- jsonlite::fromJSON(collections_text, simplifyDataFrame = FALSE)
      if (!is.null(parsed_collections$results) && length(parsed_collections$results) > 0) {
        current_page_slugs <- purrr::map_chr(parsed_collections$results, ~ .x$slug %||% NA_character_)
        current_page_slugs <- current_page_slugs[!is.na(current_page_slugs)]
        all_collections_list[[length(all_collections_list) + 1]] <- current_page_slugs
        if (verbose) message("    -> Found ", length(current_page_slugs), " collections on page ", page_count_collections)
      } else {
        if (verbose) message("    -> No collection results on page ", page_count_collections)
      }
      next_collections_page_url <- parsed_collections$`next` %||% NULL
    }, error = function(e) {
      warning("Error processing JSON for collections page ", page_count_collections, ": ", e$message)
      next_collections_page_url <- NULL
    })
    current_collections_url <- next_collections_page_url
    if (!is.null(current_collections_url) && nzchar(current_collections_url) && delay > 0) Sys.sleep(delay)
  }

  collection_slugs <- unique(unlist(all_collections_list))
  if (length(collection_slugs) == 0) stop("No valid collection slugs retrieved from Replicate API.")
  if (verbose) message("Found ", length(collection_slugs), " unique collection slugs from ", page_count_collections, " page(s).")

  # --- 5. Step 2 & 3: Fetch models per collection ---
  if (verbose) message("\nStep 2 & 3: Fetching models for each collection slug...")
  all_simplified_models_list <- list()
  collection_fetch_count <- 0
  for (slug in collection_slugs) {
    collection_fetch_count <- collection_fetch_count + 1
    if (is.null(slug) || !nzchar(slug)) { if (verbose) warning("Skipping empty or NULL slug."); next }
    collection_detail_url <- paste0("https://api.replicate.com/v1/collections/", slug)
    if (verbose) message("  Fetching models for collection ", collection_fetch_count, "/", length(collection_slugs), ": ", slug)

    response_detail <- tryCatch({ httr::GET(url = collection_detail_url, config = headers, httr::timeout(60)) },
                                error = function(e) NULL)
    if (is.null(response_detail) || httr::status_code(response_detail) != 200) {
      error_content <- if (!is.null(response_detail)) httr::content(response_detail, "text", encoding = "UTF-8") else "Conn error"
      warning("API Error (Status: ", if (!is.null(response_detail)) httr::status_code(response_detail) else "N/A",
              ") fetching detail for collection '", slug, "'. Skipping. Error: ", substr(error_content, 1, 200))
      if (delay > 0) Sys.sleep(delay)
      next
    }

    tryCatch({
      detail_text <- httr::content(response_detail, as = "text", encoding = "UTF-8")
      if (is.null(detail_text) || !jsonlite::validate(detail_text)[1]) stop("Invalid JSON for collection detail")
      parsed_detail <- jsonlite::fromJSON(detail_text, simplifyDataFrame = FALSE)
      if (!is.null(parsed_detail$models) && length(parsed_detail$models) > 0 && is.list(parsed_detail$models)) {
        models_in_collection_simplified <- purrr::map_dfr(parsed_detail$models, function(model_item) {
          if (!is.list(model_item)) return(NULL)
          tibble::tibble(
            owner = as.character(model_item$owner %||% NA_character_),
            name  = as.character(model_item$name  %||% NA_character_),
            description = as.character(model_item$description %||% "")
          )
        })
        models_in_collection_simplified <- models_in_collection_simplified %>%
          dplyr::filter(!is.na(owner) & nzchar(owner), !is.na(name) & nzchar(name))
        if (nrow(models_in_collection_simplified) > 0) {
          all_simplified_models_list[[length(all_simplified_models_list) + 1]] <- models_in_collection_simplified
          if (verbose) message("    -> Processed ", nrow(models_in_collection_simplified), " models in collection '", slug, "'")
        } else {
          if (verbose) message("    -> No valid models with owner/name found after extraction in collection '", slug, "'")
        }
      } else {
        if (verbose) message("    -> No models list found or it was empty/not a list in collection '", slug, "'")
      }
    }, error = function(e) {
      warning("Error processing JSON or extracting data for collection '", slug, "': ", e$message)
    })

    if (delay > 0) Sys.sleep(delay)
  }

  if (length(all_simplified_models_list) == 0) stop("No models found with owner/name details within any of the retrieved collections.")

  # --- 6. Step 4: Combine and deduplicate models ---
  if (verbose) message("\nStep 4: Combining and deduplicating model list...")
  combined_models_df <- tryCatch({ dplyr::bind_rows(!!!all_simplified_models_list) },
                                 error = function(e) stop("Error combining simplified model lists from collections: ", e$message))
  if (is.null(combined_models_df) || nrow(combined_models_df) == 0) stop("Model list is empty after combining simplified results from collections.")

  deduplicated_models_df <- combined_models_df %>%
    dplyr::mutate(
      owner_safe = ifelse(is.na(owner) | is.null(owner) | !nzchar(owner), "unknown_owner", owner),
      name_safe  = ifelse(is.na(name)  | is.null(name)  | !nzchar(name),  "unknown_name",  name)
    ) %>%
    dplyr::mutate(model_id_full = paste(owner_safe, name_safe, sep = "/")) %>%
    dplyr::distinct(model_id_full, .keep_all = TRUE)

  # --- 7. Build final output ---
  if (verbose) message("Formatting model entries for CSV...")
  output_df <- tibble::tibble()
  if (nrow(deduplicated_models_df) > 0) {
    output_df <- deduplicated_models_df %>%
      dplyr::mutate(
        model = model_id_full,
        service = "replicate",
        # Simple heuristic for type based on owner/name/description
        model_text = tolower(paste(owner, name, description))
      ) %>%
      dplyr::mutate(
        type = dplyr::case_when(
          grepl("image|sdxl|stable-diffusion|kandinsky|ideogram|recraft|photomaker", model_text) ~ "Image",
          grepl("whisper|audio|tts", model_text) ~ "Audio",
          grepl("llava|vision", model_text) ~ "Vision",
          TRUE ~ "Chat"
        ),
        raw_description = description %||% "",
        prc_value_unquoted = ""
      ) %>%
      dplyr::mutate(
        description_clean = gsub('"', "'", raw_description, fixed = TRUE),
        description_clean = gsub("[\r\n]+", " ", description_clean),
        description_clean = gsub("\\s+", " ", description_clean),
        description_clean = trimws(description_clean),
        pricing = sprintf('"%s"', prc_value_unquoted),
        description = sprintf('"%s"', description_clean)
      ) %>%
      dplyr::select(service, model, type, pricing, description)
  }

  # --- 8. Create Dir ---
  if (!dir.exists(directory)) {
    if (verbose) message("Creating directory: ", directory)
    dir.create(directory, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(directory)) stop("Failed to create directory: ", directory)
  }

  # --- 9. Save CSV ---
  file_path <- file.path(directory, "replicate.csv")
  if (verbose) message("\nSaving ", nrow(output_df), " Replicate models to: ", file_path)
  tryCatch({
    write.table(output_df, file = file_path, sep = ",", quote = FALSE, row.names = FALSE, col.names = TRUE, na = "", fileEncoding = "UTF-8")
  }, error = function(e) stop("Error saving replicate.csv: ", conditionMessage(e)))
  if (verbose) message("File 'replicate.csv' updated successfully.")
  invisible(output_df)
}

#' Hugging Face Hub catalog task groups (internal)
#'
#' Rows are restricted to models with a live Inference Provider mapping.
#'
#' @keywords internal
#' @noRd
.hf_catalog_tasks <- function() {
  c(
    "text-generation",
    "text2text-generation",
    "image-text-to-text",
    "visual-question-answering",
    "text-to-image",
    "image-to-image",
    "automatic-speech-recognition",
    "audio-text-to-text",
    "text-to-speech",
    "text-to-audio"
  )
}

#' Extract the next Hugging Face pagination URL (internal)
#'
#' @keywords internal
#' @noRd
.hf_next_page_url <- function(link_header) {
  if (is.null(link_header) || !length(link_header)) {
    return(NULL)
  }
  link_header <- as.character(link_header[[1]])
  if (is.na(link_header) || !nzchar(trimws(link_header))) {
    return(NULL)
  }

  parts <- strsplit(link_header, ",(?=\\s*<)", perl = TRUE)[[1]]
  next_part <- parts[grepl("rel\\s*=\\s*['\"]?next['\"]?", parts, ignore.case = TRUE, perl = TRUE)]
  if (!length(next_part)) {
    return(NULL)
  }

  next_url <- sub("^\\s*<([^>]+)>.*$", "\\1", next_part[[1]], perl = TRUE)
  if (!nzchar(next_url) || identical(next_url, next_part[[1]])) {
    return(NULL)
  }
  next_url
}

#' Fetch one Hugging Face Hub model page (internal)
#'
#' @keywords internal
#' @noRd
.hf_fetch_models_page <- function(url,
                                  query = NULL,
                                  headers = httr::add_headers(Accept = "application/json"),
                                  timeout = 60,
                                  verbose = FALSE) {
  response <- tryCatch(
    httr::RETRY(
      "GET",
      url,
      config = headers,
      query = query,
      httr::timeout(timeout),
      times = 3L,
      pause_base = 1,
      pause_cap = 15,
      terminate_on = c(400L, 401L, 403L, 404L),
      quiet = !isTRUE(verbose)
    ),
    error = function(e) {
      stop("Error connecting to the Hugging Face Hub API: ", conditionMessage(e), call. = FALSE)
    }
  )

  status <- httr::status_code(response)
  if (!identical(status, 200L)) {
    error_content <- tryCatch(
      httr::content(response, "text", encoding = "UTF-8"),
      error = function(e) ""
    )
    error_content <- gsub("[\r\n]+", " ", as.character(error_content %||% ""))
    stop(
      "Hugging Face Hub API error (status ",
      status,
      "): ",
      substr(error_content, 1, 500),
      call. = FALSE
    )
  }

  response_text <- httr::content(response, "text", encoding = "UTF-8")
  parsed <- tryCatch(
    jsonlite::fromJSON(response_text, simplifyVector = FALSE),
    error = function(e) {
      stop("Error processing JSON from the Hugging Face Hub API: ", conditionMessage(e), call. = FALSE)
    }
  )
  if (is.null(parsed)) {
    parsed <- list()
  }
  if (!is.list(parsed) || (length(parsed) && !all(vapply(parsed, is.list, logical(1))))) {
    stop("Hugging Face Hub API returned an unexpected model-list payload.", call. = FALSE)
  }

  list(
    items = parsed,
    next_url = .hf_next_page_url(httr::headers(response)[["link"]])
  )
}

#' Convert Hugging Face provider mappings to records (internal)
#'
#' @keywords internal
#' @noRd
.hf_provider_mapping_records <- function(mapping) {
  if (is.null(mapping) || !length(mapping)) {
    return(list())
  }
  if (is.data.frame(mapping)) {
    return(lapply(seq_len(nrow(mapping)), function(i) as.list(mapping[i, , drop = FALSE])))
  }
  if (!is.list(mapping)) {
    return(list())
  }
  if (any(c("provider", "status", "providerId") %in% names(mapping))) {
    return(list(mapping))
  }
  Filter(is.list, unname(mapping))
}

#' Return live Hugging Face Inference Provider ids (internal)
#'
#' @keywords internal
#' @noRd
.hf_live_provider_names <- function(model_info) {
  mappings <- .hf_provider_mapping_records(
    model_info$inferenceProviderMapping %||% model_info$inference_provider_mapping
  )
  if (!length(mappings)) {
    return(character())
  }

  providers <- vapply(mappings, function(mapping) {
    status <- tolower(trimws(as.character(mapping$status %||% "")[1]))
    provider <- trimws(as.character(mapping$provider %||% "")[1])
    if (!identical(status, "live") || is.na(provider) || !nzchar(provider)) "" else provider
  }, character(1))
  unique(providers[nzchar(providers)])
}

#' Extract a model id from a Hugging Face Hub record (internal)
#'
#' @keywords internal
#' @noRd
.hf_model_id <- function(model_info) {
  if (!is.list(model_info)) {
    return("")
  }
  for (field in c("id", "modelId", "model")) {
    value <- model_info[[field]]
    if (is.null(value) || !length(value)) {
      next
    }
    value <- trimws(as.character(value[[1]]))
    if (!is.na(value) && nzchar(value)) {
      return(value)
    }
  }
  ""
}

#' Map a Hugging Face pipeline task to the genflow model type (internal)
#'
#' @keywords internal
#' @noRd
.hf_pipeline_type <- function(pipeline_tag) {
  task <- tolower(trimws(as.character(pipeline_tag %||% "")[1]))
  if (task %in% c(
    "image-text-to-text", "visual-question-answering", "image-to-text",
    "document-question-answering"
  )) {
    return("Vision")
  }
  if (task %in% c("text-to-image", "image-to-image")) {
    return("Image")
  }
  if (task %in% c(
    "automatic-speech-recognition", "audio-text-to-text",
    "text-to-speech", "text-to-audio", "audio-to-audio"
  )) {
    return("Audio")
  }
  if (task %in% c("feature-extraction", "sentence-similarity")) {
    return("Embedding")
  }
  if (task %in% c(
    "text-generation", "text2text-generation", "conversational",
    "question-answering", "summarization", "translation"
  )) {
    return("Chat")
  }
  "Unknown"
}

#' Format one Hugging Face model description (internal)
#'
#' @keywords internal
#' @noRd
.hf_model_description <- function(model_info, live_providers = character()) {
  scalar <- function(value, default = "") {
    if (is.null(value) || !length(value)) {
      return(default)
    }
    value <- as.character(value[[1]])
    if (is.na(value) || !nzchar(value)) default else value
  }

  task <- scalar(model_info$pipeline_tag %||% model_info$pipelineTag)
  library_name <- scalar(model_info$library_name %||% model_info$libraryName)
  downloads <- scalar(model_info$downloads)
  likes <- scalar(model_info$likes)
  updated <- scalar(model_info$lastModified %||% model_info$last_modified)
  if (nzchar(updated)) {
    updated <- substr(updated, 1, 10)
  }
  gated <- model_info$gated %||% FALSE
  gated <- isTRUE(gated) || identical(tolower(scalar(gated)), "true")

  parts <- c(
    if (nzchar(task)) paste0("task=", task) else NULL,
    if (nzchar(library_name)) paste0("library=", library_name) else NULL,
    if (length(live_providers)) {
      paste0("providers=", paste(sort(live_providers), collapse = "|"))
    } else NULL,
    if (nzchar(downloads)) paste0("downloads=", downloads) else NULL,
    if (nzchar(likes)) paste0("likes=", likes) else NULL,
    if (nzchar(updated)) paste0("updated=", updated) else NULL,
    if (gated) "gated=yes" else NULL
  )
  description <- paste(parts, collapse = "; ")
  description <- gsub("[\r\n]+", " ", description)
  trimws(gsub("\\s+", " ", description))
}

#' Normalize Hugging Face Hub model records for a genflow catalog (internal)
#'
#' @keywords internal
#' @noRd
.hf_models_to_catalog <- function(models, service = "hf", require_live_provider = TRUE) {
  empty <- tibble::tibble(
    service = character(),
    model = character(),
    type = character(),
    pricing = character(),
    description = character()
  )
  if (is.null(models) || !length(models)) {
    return(empty)
  }

  rows <- lapply(models, function(model_info) {
    if (!is.list(model_info)) {
      return(NULL)
    }
    model_id <- .hf_model_id(model_info)
    if (!nzchar(model_id)) {
      return(NULL)
    }

    live_providers <- .hf_live_provider_names(model_info)
    if (isTRUE(require_live_provider) && !length(live_providers)) {
      return(NULL)
    }
    pipeline_tag <- model_info$pipeline_tag %||% model_info$pipelineTag %||% ""

    tibble::tibble(
      service = service,
      model = model_id,
      type = .hf_pipeline_type(pipeline_tag),
      pricing = "",
      description = .hf_model_description(
        model_info,
        live_providers = live_providers
      )
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) {
    return(empty)
  }

  output <- dplyr::bind_rows(rows)
  output <- output[!duplicated(tolower(output$model)), , drop = FALSE]
  output <- output[order(tolower(output$type), tolower(output$model)), , drop = FALSE]
  rownames(output) <- NULL
  output
}

#' Write a Hugging Face model catalog without exposing a partial file (internal)
#'
#' @keywords internal
#' @noRd
.hf_write_catalog <- function(output_df, directory, service, verbose = TRUE) {
  if (!dir.exists(directory)) {
    if (verbose) message("Creating directory: ", directory)
    dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(directory)) {
    stop("Failed to create directory: ", directory, call. = FALSE)
  }

  file_path <- file.path(directory, paste0(service, ".csv"))
  temp_path <- tempfile(paste0(".", service, "-"), tmpdir = directory, fileext = ".csv")
  on.exit(unlink(temp_path, force = TRUE), add = TRUE)
  tryCatch(
    utils::write.table(
      output_df,
      file = temp_path,
      sep = ",",
      quote = TRUE,
      qmethod = "double",
      row.names = FALSE,
      col.names = TRUE,
      na = "",
      fileEncoding = "UTF-8"
    ),
    error = function(e) {
      stop("Error writing Hugging Face catalog: ", conditionMessage(e), call. = FALSE)
    }
  )

  replaced <- file.rename(temp_path, file_path)
  if (!isTRUE(replaced)) {
    replaced <- file.copy(temp_path, file_path, overwrite = TRUE)
  }
  if (!isTRUE(replaced)) {
    stop("Failed to replace Hugging Face catalog: ", file_path, call. = FALSE)
  }
  if (verbose) {
    message("File '", basename(file_path), "' updated successfully with ", nrow(output_df), " models.")
  }
  invisible(file_path)
}

#' Update one Hugging Face Hub-backed catalog (internal)
#'
#' @keywords internal
#' @noRd
.update_models_hf_catalog <- function(directory,
                                      service,
                                      tasks,
                                      sorts = "downloads",
                                      featured_models = character(),
                                      require_live_provider = TRUE,
                                      limit_per_query = 50L,
                                      page_size = 50L,
                                      timeout = 60,
                                      verbose = TRUE,
                                      fetch_page = .hf_fetch_models_page) {
  tasks <- unique(trimws(as.character(tasks)))
  tasks <- tasks[!is.na(tasks) & nzchar(tasks)]
  sorts <- unique(trimws(as.character(sorts)))
  sorts <- sorts[!is.na(sorts) & nzchar(sorts)]
  featured_models <- unique(trimws(as.character(featured_models)))
  featured_models <- featured_models[!is.na(featured_models) & nzchar(featured_models)]
  limit_per_query <- suppressWarnings(as.integer(limit_per_query[[1]]))
  page_size <- suppressWarnings(as.integer(page_size[[1]]))
  if (!length(tasks) || !length(sorts)) {
    stop("Hugging Face catalog tasks and sorts cannot be empty.", call. = FALSE)
  }
  if (is.na(limit_per_query) || limit_per_query < 1L) {
    stop("`limit_per_query` must be a positive integer.", call. = FALSE)
  }
  if (is.na(page_size) || page_size < 1L) {
    stop("`page_size` must be a positive integer.", call. = FALSE)
  }
  page_size <- min(page_size, limit_per_query, 100L)

  endpoint <- trimws(Sys.getenv("HF_ENDPOINT", "https://huggingface.co"))
  if (!nzchar(endpoint)) {
    endpoint <- "https://huggingface.co"
  }
  api_url <- paste0(sub("/+$", "", endpoint), "/api/models")

  token_candidates <- c(
    Sys.getenv("HUGGINGFACE_API_TOKEN"),
    Sys.getenv("HF_TOKEN")
  )
  token_candidates <- trimws(token_candidates)
  token <- token_candidates[nzchar(token_candidates)][1]
  header_values <- list(
    Accept = "application/json",
    `User-Agent` = "genflow-model-catalog"
  )
  if (length(token) && !is.na(token) && nzchar(token)) {
    header_values$Authorization <- paste("Bearer", token)
  }
  headers <- do.call(httr::add_headers, header_values)

  expand_fields <- c(
    "pipeline_tag", "downloads", "likes", "library_name", "lastModified",
    "gated", "inferenceProviderMapping"
  )
  all_models <- list()
  failures <- character()

  for (task in tasks) {
    for (sort_key in sorts) {
      query_label <- paste(task, sort_key, sep = "/")
      if (verbose) {
        message("Fetching Hugging Face models for ", task, " (", sort_key, ")...")
      }
      query <- c(list(
        pipeline_tag = task,
        sort = sort_key,
        direction = -1L,
        limit = page_size
      ), stats::setNames(
        as.list(expand_fields),
        rep("expand[]", length(expand_fields))
      ))
      if (isTRUE(require_live_provider)) {
        query$inference_provider <- "all"
      }

      current_url <- api_url
      current_query <- query
      seen_urls <- character()
      query_models <- list()

      repeat {
        if (current_url %in% seen_urls) {
          failures[[query_label]] <- "pagination returned the same URL twice"
          break
        }
        seen_urls <- c(seen_urls, current_url)

        page <- tryCatch(
          fetch_page(
            url = current_url,
            query = current_query,
            headers = headers,
            timeout = timeout,
            verbose = verbose
          ),
          error = function(e) {
            failures[[query_label]] <<- conditionMessage(e)
            NULL
          }
        )
        if (is.null(page)) {
          break
        }
        items <- page$items %||% list()
        if (!is.list(items)) {
          failures[[query_label]] <- "page items were not a list"
          break
        }

        remaining <- limit_per_query - length(query_models)
        if (remaining <= 0L) {
          break
        }
        if (length(items)) {
          query_models <- c(query_models, utils::head(items, remaining))
        }
        if (length(query_models) >= limit_per_query) {
          break
        }

        next_url <- page$next_url %||% ""
        if (!length(next_url) || is.na(next_url[[1]]) || !nzchar(next_url[[1]])) {
          break
        }
        current_url <- as.character(next_url[[1]])
        current_query <- NULL
      }
      all_models <- c(all_models, query_models)
    }
  }

  fetched_ids <- vapply(all_models, function(model_info) {
    .hf_model_id(model_info)
  }, character(1))
  missing_featured <- featured_models[
    !tolower(featured_models) %in% tolower(fetched_ids)
  ]
  for (model_id in missing_featured) {
    query_label <- paste0("featured/", model_id)
    if (verbose) {
      message("Fetching featured local Hugging Face model ", model_id, "...")
    }
    query <- c(list(
      search = model_id,
      limit = 20L
    ), stats::setNames(
      as.list(expand_fields),
      rep("expand[]", length(expand_fields))
    ))
    page <- tryCatch(
      fetch_page(
        url = api_url,
        query = query,
        headers = headers,
        timeout = timeout,
        verbose = verbose
      ),
      error = function(e) {
        failures[[query_label]] <<- conditionMessage(e)
        NULL
      }
    )
    if (is.null(page)) {
      next
    }
    candidates <- page$items %||% list()
    exact <- Filter(function(model_info) {
      identical(tolower(.hf_model_id(model_info)), tolower(model_id))
    }, candidates)
    if (!length(exact)) {
      failures[[query_label]] <- "exact model id was not returned by Hub search"
      next
    }
    all_models <- c(all_models, list(exact[[1]]))
  }

  output_df <- .hf_models_to_catalog(
    all_models,
    service = service,
    require_live_provider = require_live_provider
  )
  if (!nrow(output_df)) {
    detail <- if (length(failures)) {
      paste0(" Failures: ", paste(names(failures), failures, sep = ": ", collapse = "; "))
    } else {
      ""
    }
    stop("No usable Hugging Face models were returned; the existing catalog was not changed.", detail, call. = FALSE)
  }
  if (length(failures)) {
    warning(
      "Hugging Face catalog updated partially: ",
      paste(names(failures), failures, sep = ": ", collapse = "; "),
      call. = FALSE
    )
  }

  .hf_write_catalog(output_df, directory = directory, service = service, verbose = verbose)
  invisible(output_df)
}

#' Update remotely routable Hugging Face models (internal)
#'
#' Only models with at least one live Hugging Face Inference Provider mapping
#' are written to `hf.csv`.
#'
#' @keywords internal
#' @noRd
.update_models_hf <- function(directory = NULL,
                              verbose = TRUE,
                              tasks = .hf_catalog_tasks(),
                              limit_per_query = 50L,
                              page_size = 50L,
                              timeout = 60,
                              fetch_page = .hf_fetch_models_page) {
  if (is.null(directory) || !length(directory) || is.na(directory[[1]]) || !nzchar(directory[[1]])) {
    directory <- tools::R_user_dir("agent_models", which = "data")
  }
  .update_models_hf_catalog(
    directory = directory,
    service = "hf",
    tasks = tasks,
    sorts = "downloads",
    featured_models = character(),
    require_live_provider = TRUE,
    limit_per_query = limit_per_query,
    page_size = page_size,
    timeout = timeout,
    verbose = verbose,
    fetch_page = fetch_page
  )
}

#' Update Ollama models list (internal)
#'
#' Connects to a local Ollama server, retrieves installed models and saves a
#' normalized CSV file named `ollama.csv` in the provided directory.
#'
#' @param directory Character path where the CSV will be saved.
#' @param verbose Logical flag to print progress messages.
#' @param base_url Optional Ollama server URL. Defaults to the environment or
#'   saved local inference configuration.
#' @param fetch_tags Internal request function used for tests.
#' @return Invisibly returns a data frame with the processed models.
#' @keywords internal
#' @noRd
.fetch_ollama_tags <- function(api_url, timeout = 30) {
  response <- tryCatch(
    httr::GET(url = api_url, httr::timeout(timeout)),
    error = function(e) {
      stop("Error connecting to the Ollama API: ", e$message)
    }
  )

  if (httr::status_code(response) != 200) {
    error_content <- httr::content(response, "text", encoding = "UTF-8")
    stop(
      "Ollama API Error (Status: ",
      httr::status_code(response),
      "): ",
      error_content
    )
  }

  tryCatch(
    jsonlite::fromJSON(
      rawToChar(httr::content(response, "raw")),
      simplifyVector = TRUE
    ),
    error = function(e) {
      stop("Error processing JSON from Ollama API: ", e$message)
    }
  )
}

#' @keywords internal
#' @noRd
.update_models_ollama <- function(directory = NULL,
                                  verbose = TRUE,
                                  base_url = NULL,
                                  fetch_tags = .fetch_ollama_tags) {
  base_url <- .ollama_base_url(base_url = base_url)
  api_url <- paste0(base_url, "/api/tags")

  if (verbose) message("Connecting to the Ollama API...")
  parsed_content <- fetch_tags(api_url)

  models <- parsed_content$models
  if (is.null(models) || !is.data.frame(models) || nrow(models) == 0) {
    if (verbose) message("No Ollama models were returned by /api/tags.")
    models <- data.frame(name = character(), stringsAsFactors = FALSE)
  }

  infer_type <- function(model_name, details_family = NULL) {
    text <- tolower(paste(model_name %||% "", details_family %||% "", collapse = " "))
    if (grepl("vision|llava|bakllava|minicpm|moondream|vl|qwen2\\.5-vl|gemma3", text, perl = TRUE)) {
      return("Vision")
    }
    "Chat"
  }

  if (nrow(models) == 0) {
    output_df <- tibble::tibble(
      service = character(),
      model = character(),
      type = character(),
      pricing = character(),
      description = character()
    )
  } else {
    output_df <- purrr::map_df(seq_len(nrow(models)), function(i) {
      model_info <- models[i, , drop = FALSE]
      model_name <- as.character(model_info$name %||% model_info$model %||% paste0("unknown_", i))
      details <- if ("details" %in% names(model_info)) model_info$details[[1]] else NULL

      family <- ""
      parameter_size <- ""
      quant_level <- ""
      if (is.list(details)) {
        family <- as.character(details$family %||% "")
        parameter_size <- as.character(details$parameter_size %||% "")
        quant_level <- as.character(details$quantization_level %||% "")
      }

      desc_parts <- c(
        if (nzchar(family)) paste0("family=", family) else NULL,
        if (nzchar(parameter_size)) paste0("params=", parameter_size) else NULL,
        if (nzchar(quant_level)) paste0("quant=", quant_level) else NULL
      )
      desc <- paste(desc_parts, collapse = "; ")

      tibble::tibble(
        service = "ollama",
        model = model_name,
        type = infer_type(model_name, family),
        pricing = sprintf('"%s"', ""),
        description = sprintf('"%s"', gsub('"', "'", desc, fixed = TRUE))
      )
    })
  }

  if (!dir.exists(directory)) {
    if (verbose) message("Creating directory: ", directory)
    dir.create(directory, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(directory)) stop("Failed to create directory: ", directory)
  }

  file_path <- file.path(directory, "ollama.csv")
  if (verbose) message("\nSaving ", nrow(output_df), " Ollama models to: ", file_path)
  tryCatch({
    write.table(output_df, file = file_path, sep = ",", quote = FALSE,
                row.names = FALSE, col.names = TRUE, na = "", fileEncoding = "UTF-8")
  }, error = function(e) stop("Error saving CSV '", file_path, "' with write.table: ", conditionMessage(e)))

  if (verbose) message("File 'ollama.csv' updated successfully.")
  invisible(output_df)
}

#' Update llama.cpp models list (internal)
#'
#' Connects to a local llama.cpp-compatible server, retrieves models from
#' candidate model endpoints (`/v1/models` and `/models`) and saves a
#' normalized CSV file named `llamacpp.csv`.
#'
#' @param directory Character path where the CSV will be saved.
#' @param verbose Logical flag to print progress messages.
#' @return Invisibly returns a data frame with the processed models.
#' @keywords internal
#' @noRd
.update_models_llamacpp <- function(directory = NULL, verbose = TRUE) {
  base_urls <- .llamacpp_base_url_candidates()
  api_key <- .llamacpp_api_key()
  model_paths <- c("/v1/models", "/models")

  header_args <- list("Content-Type" = "application/json")
  if (nzchar(api_key)) {
    header_args[["Authorization"]] <- paste("Bearer", api_key)
  }
  headers <- do.call(httr::add_headers, header_args)

  parsed_content <- NULL
  api_url_used <- ""
  attempted_urls <- character()
  for (base_url in base_urls) {
    for (model_path in model_paths) {
      api_url <- paste0(base_url, model_path)
      attempted_urls <- c(attempted_urls, api_url)
      if (verbose) message("Connecting to the llama-cpp API at ", api_url, " ...")
      response_try <- tryCatch(
        {
          httr::GET(url = api_url, headers, httr::timeout(30))
        },
        error = function(e) NULL
      )
      if (is.null(response_try) || httr::status_code(response_try) != 200) {
        next
      }

      parsed_try <- tryCatch(
        {
          raw_content <- httr::content(response_try, "raw")
          jsonlite::fromJSON(rawToChar(raw_content), simplifyVector = TRUE)
        },
        error = function(e) NULL
      )
      if (is.null(parsed_try) || !is.list(parsed_try)) {
        next
      }

      parsed_content <- parsed_try
      api_url_used <- api_url
      break
    }
    if (!is.null(parsed_content)) {
      break
    }
  }

  if (is.null(parsed_content)) {
    attempted_txt <- paste(unique(attempted_urls), collapse = ", ")
    stop("llama-cpp API Error: unable to fetch valid JSON from candidate endpoints. Tried: ", attempted_txt)
  }

  if (verbose) message("Processing JSON response from ", api_url_used, " ...")

  models <- parsed_content$data %||% parsed_content$models
  if (is.null(models)) {
    if (verbose) message("No llama-cpp models were returned by candidate model endpoints.")
    models <- data.frame(id = character(), stringsAsFactors = FALSE)
  }

  infer_type <- function(model_name) {
    text <- tolower(model_name %||% "")
    if (grepl("vision|vl|llava|minicpm|moondream|qwen2\\.5-vl|gemma3", text, perl = TRUE)) {
      return("Vision")
    }
    "Chat"
  }

  extract_rows <- function(models_obj) {
    if (is.data.frame(models_obj)) {
      if (nrow(models_obj) == 0) return(list())
      return(split(models_obj, seq_len(nrow(models_obj))))
    }
    if (is.list(models_obj) && length(models_obj) > 0) {
      return(models_obj)
    }
    list()
  }
  model_rows <- extract_rows(models)

  if (length(model_rows) == 0) {
    output_df <- tibble::tibble(
      service = character(),
      model = character(),
      type = character(),
      pricing = character(),
      description = character()
    )
  } else {
    output_df <- purrr::map_df(seq_along(model_rows), function(i) {
      model_info <- model_rows[[i]]
      if (is.data.frame(model_info)) {
        model_info <- as.list(model_info[1, , drop = FALSE])
      }
      if (!is.list(model_info)) {
        model_info <- list()
      }

      model_name <- as.character(model_info$id %||% model_info$model %||% model_info$name %||% paste0("unknown_", i))
      model_name <- trimws(model_name[1] %||% "")
      if (!nzchar(model_name)) {
        model_name <- paste0("unknown_", i)
      }

      owner <- as.character(model_info$owned_by %||% model_info$owner %||% "")
      created <- model_info$created %||% NA
      created_txt <- ""
      if (!is.null(created) && length(created) == 1 && is.finite(suppressWarnings(as.numeric(created)))) {
        created_txt <- format(as.POSIXct(as.numeric(created), origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d")
      }

      desc_parts <- c(
        if (nzchar(owner[1] %||% "")) paste0("owner=", owner[1]) else NULL,
        if (nzchar(created_txt)) paste0("created=", created_txt) else NULL
      )
      desc <- paste(desc_parts, collapse = "; ")

      tibble::tibble(
        service = "llamacpp",
        model = model_name,
        type = infer_type(model_name),
        pricing = sprintf('"%s"', ""),
        description = sprintf('"%s"', gsub('"', "'", desc, fixed = TRUE))
      )
    })
  }

  if (!dir.exists(directory)) {
    if (verbose) message("Creating directory: ", directory)
    dir.create(directory, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(directory)) stop("Failed to create directory: ", directory)
  }

  file_path <- file.path(directory, "llamacpp.csv")
  if (verbose) message("\nSaving ", nrow(output_df), " llama-cpp models to: ", file_path)
  tryCatch({
    write.table(output_df, file = file_path, sep = ",", quote = FALSE,
                row.names = FALSE, col.names = TRUE, na = "", fileEncoding = "UTF-8")
  }, error = function(e) stop("Error saving CSV '", file_path, "' with write.table: ", conditionMessage(e)))

  if (verbose) message("File 'llamacpp.csv' updated successfully.")
  invisible(output_df)
}

#' Update the downloaded native STT model catalog (internal)
#'
#' Materializes regular models managed by the canonical CrispASR cache as the
#' `local-native` provider. Catalog model ids are flat cache filenames rather
#' than machine-specific absolute paths.
#'
#' @param directory Character path where `local-native.csv` will be saved.
#' @param verbose Logical flag to print progress.
#' @param inventory_fn Internal inventory function used by tests.
#' @return Invisibly returns the normalized catalog data frame.
#' @keywords internal
#' @noRd
.update_models_local_native <- function(
  directory = NULL,
  verbose = TRUE,
  inventory_fn = .genflow_crispasr_inventory
) {
  if (is.null(directory) || !length(directory) ||
      is.na(directory[[1]]) || !nzchar(trimws(directory[[1]]))) {
    directory <- tools::R_user_dir("agent_models", which = "data")
  }
  directory <- path.expand(as.character(directory[[1]]))
  if (!dir.exists(directory)) {
    if (verbose) message("Creating directory: ", directory)
    dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(directory)) {
    stop("Failed to create model directory: ", directory, call. = FALSE)
  }
  if (!is.function(inventory_fn)) {
    stop("`inventory_fn` must be a function.", call. = FALSE)
  }

  inventory <- inventory_fn()
  required <- c(
    "path", "filename", "quant", "size", "source_url", "managed"
  )
  if (!is.data.frame(inventory) ||
      length(setdiff(required, names(inventory)))) {
    stop(
      "CrispASR returned an invalid native model inventory.",
      call. = FALSE
    )
  }

  managed <- !is.na(inventory$managed) & inventory$managed
  inventory <- inventory[managed, , drop = FALSE]
  filenames <- if (nrow(inventory)) {
    vapply(
      inventory$filename,
      function(filename) {
        tryCatch(
          .genflow_crispasr_validate_filename(filename),
          error = function(e) ""
        )
      },
      character(1)
    )
  } else {
    character()
  }
  valid_filename <- nzchar(filenames)
  inventory <- inventory[valid_filename, , drop = FALSE]
  filenames <- filenames[valid_filename]
  if (!nrow(inventory)) {
    output_df <- data.frame(
      service = character(),
      model = character(),
      type = character(),
      pricing = character(),
      description = character(),
      stringsAsFactors = FALSE
    )
  } else {
    path_filenames <- basename(as.character(inventory$path))
    if (any(filenames != path_filenames)) {
      stop(
        "CrispASR returned an inconsistent managed model inventory.",
        call. = FALSE
      )
    }

    scalar_text <- function(value) {
      value <- trimws(as.character(value %||% "")[1])
      if (is.na(value)) "" else value
    }
    description <- vapply(seq_len(nrow(inventory)), function(index) {
      quant <- scalar_text(inventory$quant[[index]])
      size <- scalar_text(inventory$size[[index]])
      source <- scalar_text(inventory$source_url[[index]])
      paste(
        c(
          "engine=crispasr",
          if (nzchar(quant)) paste0("quant=", quant),
          if (nzchar(size)) paste0("size=", size),
          if (nzchar(source)) paste0("source=", source)
        ),
        collapse = "; "
      )
    }, character(1))

    output_df <- data.frame(
      service = rep("local-native", length(filenames)),
      model = filenames,
      type = rep("Audio", length(filenames)),
      pricing = rep("", length(filenames)),
      description = description,
      stringsAsFactors = FALSE
    )
    output_df <- output_df[
      !duplicated(output_df$model),
      ,
      drop = FALSE
    ]
    output_df <- output_df[
      order(tolower(output_df$model)),
      ,
      drop = FALSE
    ]
    rownames(output_df) <- NULL
  }

  file_path <- file.path(directory, "local-native.csv")
  tryCatch(
    utils::write.table(
      output_df,
      file = file_path,
      sep = ",",
      quote = TRUE,
      qmethod = "double",
      row.names = FALSE,
      col.names = TRUE,
      na = "",
      fileEncoding = "UTF-8"
    ),
    error = function(e) {
      stop(
        "Error writing native STT model catalog: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
  if (verbose) {
    message(
      "File 'local-native.csv' updated successfully with ",
      nrow(output_df),
      " model(s)."
    )
  }
  invisible(output_df)
}

.genflow_catalog_columns <- function() {
  c("model", "type", "pricing", "description")
}

.genflow_validate_catalog_file <- function(
  path,
  provider,
  allow_empty = FALSE
) {
  if (!is.logical(allow_empty) || length(allow_empty) != 1L ||
      is.na(allow_empty)) {
    stop("`allow_empty` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!file.exists(path) || isTRUE(file.info(path)$isdir[[1]])) {
    stop("Updater did not produce the expected catalog file.", call. = FALSE)
  }
  if (!isTRUE(file.info(path)$size[[1]] > 0)) {
    stop("Updater produced an empty catalog file.", call. = FALSE)
  }

  catalog <- tryCatch(
    utils::read.csv(
      path,
      stringsAsFactors = FALSE,
      check.names = FALSE,
      fileEncoding = "UTF-8"
    ),
    error = function(e) {
      stop(
        "Updater produced an unreadable catalog CSV: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
  if (!is.data.frame(catalog)) {
    stop("Updater produced an invalid catalog.", call. = FALSE)
  }
  if (anyDuplicated(names(catalog))) {
    stop("Updater produced a catalog with duplicate column names.", call. = FALSE)
  }

  provider_columns <- intersect(c("service", "provider"), names(catalog))
  missing_columns <- setdiff(.genflow_catalog_columns(), names(catalog))
  if (!length(provider_columns) || length(missing_columns)) {
    required <- c("service/provider", .genflow_catalog_columns())
    stop(
      "Updater produced an invalid catalog schema; required columns are: ",
      paste(required, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  if (!nrow(catalog)) {
    if (isTRUE(allow_empty)) {
      return(invisible(catalog))
    }
    stop("Updater produced a catalog without model rows.", call. = FALSE)
  }

  model_values <- trimws(as.character(catalog$model))
  type_values <- trimws(as.character(catalog$type))
  if (any(is.na(model_values) | !nzchar(model_values))) {
    stop("Updater produced a catalog with an empty model id.", call. = FALSE)
  }
  if (any(is.na(type_values) | !nzchar(type_values))) {
    stop("Updater produced a catalog with an empty model type.", call. = FALSE)
  }

  provider_values <- trimws(as.character(catalog[[provider_columns[[1]]]]))
  if (any(is.na(provider_values) | !nzchar(provider_values))) {
    stop("Updater produced a catalog with an empty provider id.", call. = FALSE)
  }
  normalized_values <- vapply(
    provider_values,
    .genflow_normalize_service_alias,
    character(1),
    USE.NAMES = FALSE
  )
  expected_provider <- .genflow_normalize_service_alias(provider)
  if (any(normalized_values != expected_provider)) {
    stop(
      "Updater produced a catalog for a different provider.",
      call. = FALSE
    )
  }

  invisible(catalog)
}

.genflow_promote_catalog_file <- function(staged_path,
                                          final_path,
                                          rename_fn = file.rename,
                                          portable_replace = identical(.Platform$OS.type, "windows")) {
  if (!file.exists(staged_path)) {
    stop("Validated staged catalog disappeared before publication.", call. = FALSE)
  }
  dir.create(dirname(final_path), recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(dirname(final_path))) {
    stop("Could not create the model catalog directory.", call. = FALSE)
  }

  lock <- .genflow_acquire_file_lock(
    final_path,
    timeout = getOption("genflow.catalog_lock_timeout", 30),
    poll = getOption("genflow.catalog_lock_poll", 0.05),
    stale_after = getOption("genflow.catalog_lock_stale_after", 300),
    lock_label = "model catalog"
  )
  on.exit(.genflow_release_file_lock(lock), add = TRUE)

  target_exists <- file.exists(final_path)
  if (!target_exists || !isTRUE(portable_replace)) {
    replaced <- tryCatch(
      rename_fn(staged_path, final_path),
      error = function(e) FALSE
    )
    if (!isTRUE(replaced)) {
      stop(
        "Could not atomically publish the validated model catalog; ",
        "the previous catalog was left unchanged.",
        call. = FALSE
      )
    }
    return(invisible(TRUE))
  }

  rollback <- .genflow_unique_sidecar_path(final_path, "rollback", ".tmp")
  moved_original <- tryCatch(
    rename_fn(final_path, rollback),
    error = function(e) FALSE
  )
  if (!isTRUE(moved_original)) {
    stop(
      "Could not prepare a recoverable model catalog replacement; ",
      "the previous catalog was left unchanged.",
      call. = FALSE
    )
  }

  replaced <- tryCatch(
    rename_fn(staged_path, final_path),
    error = function(e) FALSE
  )
  if (!isTRUE(replaced)) {
    restored <- tryCatch(
      rename_fn(rollback, final_path),
      error = function(e) FALSE
    )
    if (isTRUE(restored)) {
      stop(
        "Could not publish the validated model catalog; ",
        "the previous catalog was restored.",
        call. = FALSE
      )
    }
    stop(
      "Could not publish the validated model catalog; the previous catalog ",
      "remains in recovery file ",
      rollback,
      ".",
      call. = FALSE
    )
  }

  if (file.exists(rollback)) {
    unlink(rollback, force = TRUE)
  }
  invisible(TRUE)
}

.genflow_run_catalog_update <- function(provider,
                                        update_function,
                                        directory,
                                        verbose = TRUE,
                                        update_args = list(),
                                        allow_empty = FALSE,
                                        promote_fn = .genflow_promote_catalog_file) {
  provider <- as.character(provider)[[1]]
  safe_provider <- gsub("[^a-z0-9._-]+", "-", tolower(provider), perl = TRUE)
  staging_dir <- tempfile(
    pattern = paste0(".", safe_provider, "-staging-"),
    tmpdir = directory
  )
  created <- dir.create(
    staging_dir,
    recursive = FALSE,
    showWarnings = FALSE,
    mode = "0700"
  )
  if (!isTRUE(created) || !dir.exists(staging_dir)) {
    stop("Could not create an isolated model catalog staging directory.", call. = FALSE)
  }
  on.exit(unlink(staging_dir, recursive = TRUE, force = TRUE), add = TRUE)

  call_args <- c(
    update_args,
    list(directory = staging_dir, verbose = verbose)
  )
  update_result <- do.call(update_function, call_args)
  staged_path <- file.path(staging_dir, paste0(provider, ".csv"))
  .genflow_validate_catalog_file(
    staged_path,
    provider = provider,
    allow_empty = allow_empty
  )

  final_path <- file.path(directory, paste0(provider, ".csv"))
  promoted <- promote_fn(staged_path, final_path)
  if (!isTRUE(promoted) || !file.exists(final_path)) {
    stop("Validated model catalog was not published.", call. = FALSE)
  }
  if (verbose) {
    message("Published validated catalog '", basename(final_path), "'.")
  }
  invisible(update_result)
}

#' Update provider model lists and write CSVs
#'
#' High-level convenience function to update model lists from one or several
#' providers and write normalized CSV files to a directory.
#'
#' Each provider runs in an isolated staging directory. Its CSV must contain
#' `service` or `provider`, plus `model`, `type`, `pricing`, and `description`,
#' and is atomically published only after validation. A failed update never
#' truncates the previous provider catalog.
#'
#' The interactive interface preflights provider credentials before calling this
#' helper. When calling it directly, use `fail_on_error = TRUE` if provider
#' failures should stop the call instead of being reported only as warnings.
#'
#' @param provider Optional character scalar. If NULL, updates all supported providers.
#'        Otherwise one of the built-ins ("openrouter", "openai", "anthropic",
#'        "groq", "cerebras", "together", "sambanova", "nebius", "deepseek",
#'        "perplexity", "fireworks", "deepinfra", "hyperbolic", "gemini",
#'        "fal", "replicate", "hf", "ollama", "llamacpp", "local-native") or
#'        a custom provider id
#'        configured with [set_provider_openai_compat()].
#' @param directory Character path where CSVs will be saved. Defaults to working dir.
#' @param verbose Logical flag to print progress messages.
#' @param fail_on_error Logical. If TRUE, provider failures are collected and raised as an error after attempted updates.
#'
#' @return Invisibly returns the model directory path with attributes describing updated and failed providers.
#' @examples
#' # Update all providers to the default directory
#' # gen_update_models()
#'
#' # Update a single provider
#' # gen_update_models(provider = "openrouter", directory = tempdir())
#'
#' @export
gen_update_models <- function(provider = NULL, directory = NULL, verbose = TRUE, fail_on_error = FALSE) {
  # Set default directory if not provided
  if (is.null(directory) || !length(directory) ||
      (length(directory) == 1L && is.na(directory))) {
    directory <- tools::R_user_dir("agent_models", which = "data")
  } else if (!is.character(directory) || length(directory) != 1L ||
             is.na(directory) || !nzchar(trimws(directory))) {
    stop("`directory` must be one non-empty path.", call. = FALSE)
  }
  directory <- path.expand(directory)

  # Create directory if it doesn't exist
  if (!dir.exists(directory)) {
    if (verbose) message("Creating directory: ", directory)
    dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(directory)) {
    stop("Failed to create model directory: ", directory, call. = FALSE)
  }

  # Define all available built-in providers
  builtin_providers <- c(
    "openrouter", "openai", "anthropic", "groq", "cerebras", "together", "sambanova",
    "nebius", "deepseek", "perplexity", "fireworks", "deepinfra", "hyperbolic",
    "gemini", "fal", "replicate", "hf", "ollama", "llamacpp",
    "local-native"
  )
  custom_provider_cfgs <- .genflow_list_custom_provider_configs()
  custom_providers <- names(custom_provider_cfgs)
  all_providers <- unique(c(builtin_providers, custom_providers))

  # Determine which providers to update
  if (is.null(provider)) {
    providers_to_update <- all_providers
    if (verbose) message("Updating all models...")
  } else {
    provider <- vapply(
      as.character(provider),
      .genflow_normalize_service_alias,
      character(1),
      USE.NAMES = FALSE
    )
    provider <- unique(provider)
    invalid_providers <- setdiff(provider, all_providers)
    if (length(invalid_providers) > 0) {
      stop("Invalid provider(s): ", paste(invalid_providers, collapse = ", "),
           ". Valid providers are: ", paste(all_providers, collapse = ", "))
    }
    providers_to_update <- provider
    if (verbose) message("Updating selected providers: ", paste(providers_to_update, collapse = ", "))
  }

  # Update function mapping
  update_functions <- list(
    "openrouter" = list(func = ".update_models_openrouter", name = "OpenRouter"),
    "openai"     = list(func = ".update_models_openai",     name = "OpenAI"),
    "anthropic"  = list(func = ".update_models_anthropic",  name = "Anthropic"),
    "groq"       = list(func = ".update_models_groq",       name = "Groq"),
    "cerebras"   = list(func = ".update_models_cerebras",   name = "Cerebras"),
    "together"   = list(func = ".update_models_together",   name = "Together"),
    "sambanova"  = list(func = ".update_models_sambanova",  name = "SambaNova"),
    "nebius"     = list(func = ".update_models_nebius",     name = "Nebius"),
    "deepseek"   = list(func = ".update_models_deepseek",   name = "DeepSeek"),
    "perplexity" = list(func = ".update_models_perplexity", name = "Perplexity"),
    "fireworks"  = list(func = ".update_models_fireworks",  name = "Fireworks"),
    "deepinfra"  = list(func = ".update_models_deepinfra",  name = "DeepInfra"),
    "hyperbolic" = list(func = ".update_models_hyperbolic", name = "Hyperbolic"),
    "gemini"     = list(func = ".update_models_gemini",     name = "Gemini"),
    "fal"        = list(func = ".update_models_fal",        name = "Fal"),
    "replicate"  = list(func = ".update_models_replicate",  name = "Replicate"),
    "hf"         = list(func = ".update_models_hf",         name = "Hugging Face"),
    "ollama"     = list(func = ".update_models_ollama",     name = "Ollama"),
    "llamacpp"   = list(func = ".update_models_llamacpp",   name = "llama-cpp"),
    "local-native" = list(
      func = ".update_models_local_native",
      name = "Native STT",
      allow_empty = TRUE
    )
  )

  # Track progress
  total_providers <- length(providers_to_update)
  current_provider <- 0
  updated_providers <- character()
  failures <- list()

  # Update each selected provider
  for (prov in providers_to_update) {
    current_provider <- current_provider + 1
    if (prov %in% names(update_functions)) {
      update_info <- update_functions[[prov]]
      if (verbose) message(sprintf("%d/%d - Updating %s...", current_provider, total_providers, update_info$name))
      tryCatch({
        .genflow_run_catalog_update(
          provider = prov,
          update_function = update_info$func,
          directory = directory,
          verbose = verbose,
          allow_empty = isTRUE(update_info$allow_empty)
        )
        updated_providers <- c(updated_providers, prov)
      }, error = function(e) {
        failures[[prov]] <<- e$message
        if (verbose) warning("Failed to update ", update_info$name, ": ", e$message)
      })
      next
    }

    custom_cfg <- custom_provider_cfgs[[prov]]
    custom_name <- custom_cfg$label %||% prov
    if (verbose) message(sprintf("%d/%d - Updating %s...", current_provider, total_providers, custom_name))
    tryCatch({
      .genflow_run_catalog_update(
        provider = prov,
        update_function = ".update_models_custom_openai_compat",
        directory = directory,
        verbose = verbose,
        update_args = list(provider_id = prov)
      )
      updated_providers <- c(updated_providers, prov)
    }, error = function(e) {
      failures[[prov]] <<- e$message
      if (verbose) warning("Failed to update ", custom_name, ": ", e$message)
    })
  }

  # Display update statistics
  if (verbose) {
    message("\n--- Update stats ---")
    csv_files <- list.files(directory, pattern = "\\.csv$", full.names = FALSE)
    if (length(csv_files) > 0) {
      for (file in csv_files) {
        file_path <- file.path(directory, file)
        if (file.exists(file_path)) {
          df <- tryCatch(read.csv(file_path, stringsAsFactors = FALSE), error = function(e) NULL)
          if (!is.null(df)) message(sprintf("- %s: %d models", file, nrow(df)))
        }
      }
    }
    message("Directory: ", directory)
  }

  result <- directory
  attr(result, "updated_providers") <- updated_providers
  attr(result, "failed_providers") <- names(failures)
  attr(result, "failures") <- failures
  if (length(failures) && isTRUE(fail_on_error)) {
    failure_msg <- paste0(names(failures), ": ", unlist(failures, use.names = FALSE), collapse = "; ")
    stop("Model update failed for ", failure_msg, call. = FALSE)
  }

  invisible(result)
}

#' Normalize `provider` column values (internal)
#'
#' Ensures a consistent lowercase provider column.
#'
#' @param df A data frame with a `service` or `provider` column.
#' @return The same data frame with a normalized `provider` column.
#' @keywords internal
#' @noRd
.normalize_provider_col <- function(df) {
  if (!"provider" %in% names(df) && "service" %in% names(df)) {
    names(df)[names(df) == "service"] <- "provider"
  }
  expected <- c("provider", "model", "type", "pricing", "description")
  for (col in expected) if (!col %in% names(df)) df[[col]] <- NA_character_
  df[, expected, drop = FALSE]
}
#' Normalize type input vector (internal)
#'
#' Accepts NULL or character and returns a trimmed, lowercase vector.
#'
#' @param types_in Character vector or NULL.
#' @return Character vector (possibly empty) of normalized types.
#' @keywords internal
#' @noRd
.normalize_type_inputs <- function(types_in) {
  if (is.null(types_in)) return(NULL)
  tvec <- tolower(trimws(as.character(unlist(types_in))))
  tvec <- tvec[nzchar(tvec)]
  if (length(tvec) == 0) return(NULL)
  mapping <- list(
    text = c("chat", "completion"),
    chat = c("chat"),
    completion = c("completion"),
    image = c("image"),
    vision = c("vision"),
    audio = c("audio"),
    video = c("video"),
    embedding = c("embedding"),
    embeddings = c("embedding"),
    embed = c("embedding"),
    code = c("code"),
    aqa = c("aqa"),
    unknown = c("unknown")
  )
  out <- unlist(lapply(tvec, function(x) mapping[[x]] %||% x))
  unique(out)
}
#' Read provider CSV files (internal)
#'
#' Reads CSV files generated by update functions for the given providers from a
#' directory and returns a combined data frame.
#'
#' @param directory Character path where CSVs are stored.
#' @param providers Optional character vector of provider names to read.
#' @param verbose Logical flag to print progress messages.
#' @return A data frame combining all available provider rows.
#' @keywords internal
#' @noRd
.read_provider_csvs <- function(directory, providers = NULL, verbose = TRUE) {
  if (is.null(directory) || is.na(directory)) {
    directory <- tools::R_user_dir("agent_models", which = "data")
  }
  if (!dir.exists(directory)) {
    if (verbose) message("Directory does not exist: ", directory)
    return(tibble::tibble(provider = character(), model = character(), type = character(), pricing = character(), description = character()))
  }
  csv_paths <- list.files(directory, pattern = "\\.csv$", full.names = TRUE)
  if (length(csv_paths) == 0) {
    if (verbose) message("No CSV files found in ", directory)
    return(tibble::tibble(provider = character(), model = character(), type = character(), pricing = character(), description = character()))
  }
  base_names <- tolower(tools::file_path_sans_ext(basename(csv_paths)))
  active_files <- !.genflow_is_retired_service(base_names)
  csv_paths <- csv_paths[active_files]
  base_names <- base_names[active_files]
  if (!length(csv_paths)) {
    if (verbose) message("No active provider CSV files found in ", directory)
    return(tibble::tibble(provider = character(), model = character(), type = character(), pricing = character(), description = character()))
  }
  if (!is.null(providers)) {
    p_sel <- tolower(trimws(as.character(unlist(providers))))
    sel <- base_names %in% p_sel
    if (!any(sel)) {
      if (verbose) message("No files match requested provider(s): ", paste(unique(p_sel), collapse = ", "))
      return(tibble::tibble(provider = character(), model = character(), type = character(), pricing = character(), description = character()))
    }
    csv_paths <- csv_paths[sel]
    base_names <- base_names[sel]
  }
  dfs <- lapply(csv_paths, function(p) {
    df <- tryCatch(read.csv(p, stringsAsFactors = FALSE), error = function(e) NULL)
    if (is.null(df)) return(NULL)
    df <- .normalize_provider_col(df)
    if (all(is.na(df$provider)) || !nzchar(df$provider[1])) {
      df$provider <- tolower(tools::file_path_sans_ext(basename(p)))
    }
    df <- df[!.genflow_is_retired_service(df$provider), , drop = FALSE]
    if (!nrow(df)) return(NULL)
    df
  })
  dfs <- Filter(Negate(is.null), dfs)
  if (length(dfs) == 0) return(tibble::tibble(provider = character(), model = character(), type = character(), pricing = character(), description = character()))
  dplyr::bind_rows(dfs)
}
#' Strip leading/trailing double quotes (internal)
#'
#' @param x Character vector.
#' @return Character vector with surrounding quotes removed.
#' @keywords internal
#' @noRd
.strip_outer_quotes <- function(x) {
   x <- as.character(x)
   x <- sub('^"', '', x)
   x <- sub('"$', '', x)
   x
 }
#' Shorten character strings (internal)
#'
#' @param x Character vector to shorten.
#' @param n Maximum number of characters to keep.
#' @return Shortened character vector with an ellipsis if needed.
#' @keywords internal
#' @noRd
.shorten_str <- function(x, n = 20) {
   x <- as.character(x)
   x[is.na(x)] <- ""
   too_long <- nchar(x) > n
   x[too_long] <- paste0(substr(x[too_long], 1, n), "...")
   x
 }

#' Show available models from CSVs
#'
#' Reads the provider CSV files and prints a filtered, readable table of models.
#'
#' @param term Optional search term to match in model id or description.
#' @param provider Optional provider filter (e.g., "openrouter", "openai").
#' @param type Optional type filter (e.g., "chat", "vision", "image").
#' @param directory Directory where CSVs are stored. Defaults to working dir.
#' @param verbose Logical flag for progress messages.
#' @param max_desc Integer max description length for display.
#'
#' @return Invisibly returns the filtered data frame printed to the console.
#' @examples
#' # gen_show_models(term = "gpt", provider = "openai", directory = tempdir())
#'
#' @export
gen_show_models <- function(term = NULL, provider = NULL, type = NULL, directory = NULL, verbose = FALSE, max_desc = 20) {
   df <- .read_provider_csvs(directory, providers = provider, verbose = verbose)
   if (nrow(df) == 0) {
     if (verbose) message("No models found.")
     return(df)
   }

   # Clean fields for display
   df$pricing <- .strip_outer_quotes(df$pricing)
   df$description <- .strip_outer_quotes(df$description)
   # Normalize unicode ellipsis to ASCII for display
   df$description <- gsub("\u2026", "...", df$description, perl = TRUE)

   # Filter by type
   type_norm <- .normalize_type_inputs(type)
   if (!is.null(type_norm)) {
     df <- dplyr::filter(df, tolower(type) %in% type_norm)
   }

   # Filter by term across columns (case-insensitive, literal; avoid grepl warning)
   if (!is.null(term) && nzchar(trimws(term))) {
     cols <- c("provider", "model", "type", "pricing", "description")
     term_l <- tolower(term)
     mask <- Reduce(`|`, lapply(cols, function(cname) {
       vals <- as.character(df[[cname]])
       vals[is.na(vals)] <- ""
       grepl(term_l, tolower(vals), fixed = TRUE)
     }))
     df <- df[mask, , drop = FALSE]
   }

   # Prepare display dataframe based on verbosity
   df_disp <- df
   # Always shorten pricing for display
   df_disp$pricing <- .shorten_str(df_disp$pricing, n = max_desc)
   # Shorten description only when verbose is FALSE
   if (!isTRUE(verbose)) df_disp$description <- .shorten_str(df_disp$description, n = max_desc)

   # Sort
   df_disp <- dplyr::arrange(df_disp, provider, type, model)

   # Printing behavior
   providers_present <- unique(df_disp$provider)
   if (!is.null(provider)) {
     # Preserve the order the user requested in 'provider' argument
     prov_order <- unique(tolower(trimws(as.character(unlist(provider)))))
     prov_loop <- unique(c(prov_order[prov_order %in% providers_present], providers_present))
     for (prov in prov_loop) {
       if (!prov %in% providers_present) next
       message(sprintf("Provider: %s", prov))
       subdf <- df_disp[df_disp$provider == prov, c("model", "type", "pricing", "description"), drop = FALSE]
       print(subdf, row.names = FALSE)
       cat("\n")
     }
     # When provider is specified, show only grouped blocks and avoid printing the combined data frame again
     return(invisible(df_disp))
   } else {
     if (verbose) message("Found ", nrow(df_disp), " models across ", length(providers_present), " providers.")
   }

   df_disp
 }
