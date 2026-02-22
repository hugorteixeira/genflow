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
#' Connects to the Gemini API, retrieves the list of models and saves a
#' normalized CSV file in the provided directory.
#'
#' - Installs and loads required packages on-demand.
#' - Validates environment variable `GEMINI_API_KEY`.
#'
#' @param directory Character path where the CSV will be saved. If NULL, uses current working dir.
#' @param verbose Logical flag to print progress messages.
#' @return Invisibly returns a data frame with the processed models.
#' @keywords internal
#' @noRd
.update_models_gemini <- function(directory = NULL, verbose = TRUE) {
  # --- 2. API Key ---
  api_key_gemini <- Sys.getenv("GEMINI_API_KEY")
  if (api_key_gemini == "") stop("Error: Environment variable 'GEMINI_API_KEY' not set.")

  # --- 3. API URL ---
  api_base_url <- "https://generativelanguage.googleapis.com"
  api_version  <- "v1beta"
  api_path     <- paste0("/", api_version, "/models")
  api_url      <- paste0(api_base_url, api_path, "?key=", api_key_gemini)

  # --- 4. API Call ---
  if (verbose) message("Connecting to the Gemini API...")
  response <- tryCatch({ httr::GET(url = api_url, httr::timeout(60)) },
                       error = function(e) stop("Error connecting to the Google AI API: ", e$message))

  # --- 5. Check Response Status ---
  if (httr::status_code(response) != 200) {
    error_content <- httr::content(response, "text", encoding = "UTF-8")
    error_details <- ""
    try({
      parsed_error <- jsonlite::fromJSON(error_content)
      if (is.list(parsed_error) && "error" %in% names(parsed_error)) {
        error_details <- paste(parsed_error$error$code, parsed_error$error$status, parsed_error$error$message, sep = ": ")
      }
    }, silent = TRUE)
    if (nzchar(error_details)) stop("Google AI API Error (Status: ", httr::status_code(response), "): ", error_details)
    stop("Google AI API Error (Status: ", httr::status_code(response), "): ", substr(error_content, 1, 500))
  }

  # --- 6. Process JSON ---
  if (verbose) message("Processing JSON response...")
  models_list <- NULL
  parsed_content <- tryCatch({ jsonlite::fromJSON(rawToChar(httr::content(response, "raw")), simplifyVector = TRUE) },
                             error = function(e) stop("Error processing JSON or extracting 'models' field from Google AI API: ", e$message))
  if (!is.null(parsed_content) && is.list(parsed_content) && "models" %in% names(parsed_content)) {
    if (is.data.frame(parsed_content$models)) {
      models_list <- parsed_content$models
    } else if (is.list(parsed_content$models)) {
      models_list <- dplyr::bind_rows(!!!lapply(parsed_content$models, function(x) {
        x <- rapply(x, as.character, classes = "list", how = "replace")
        as.data.frame(t(unlist(x)), stringsAsFactors = FALSE)
      }))
    }
  }

  if (is.null(models_list) || nrow(models_list) == 0) {
    if (verbose) print(utils::str(parsed_content))
    stop("Field 'models' containing model list not found, empty, or not processable in Google AI API response.")
  }

  # --- 7. Format Data ---
  if (verbose) message("Processing ", nrow(models_list), " Gemini models...")

  output_df <- purrr::map_df(seq_len(nrow(models_list)), function(i) {
    model_info <- models_list[i, ]

    model_id_raw  <- model_info$name %||% paste0("UNKNOWN_GEMINI_NAME_", i)
    model_id_safe <- sub("^models/", "", model_id_raw)

    supported_methods <- model_info$supportedGenerationMethods %||% character(0)
    if (is.list(supported_methods)) supported_methods <- unlist(supported_methods)

    model_type <- dplyr::case_when(
      grepl("^gemini-.*pro",   model_id_safe, ignore.case = TRUE) ~ "Chat",
      grepl("^gemini-.*flash", model_id_safe, ignore.case = TRUE) ~ "Chat",
      grepl("^gemini-ultra",   model_id_safe, ignore.case = TRUE) ~ "Chat",
      grepl("^embedding-",     model_id_safe, ignore.case = TRUE) ~ "Embedding",
      grepl("^text-embedding-",model_id_safe, ignore.case = TRUE) ~ "Embedding",
      ("embedContent" %in% supported_methods) && !("generateContent" %in% supported_methods) ~ "Embedding",
      ("generateContent" %in% supported_methods) ~ "Chat",
      TRUE ~ "Unknown"
    )

    tibble::tibble(
      service = "gemini",
      model = model_id_safe,
      type = model_type,
      pricing = "",
      description = ""
    )
  })

  # --- 8. Post-processing: Add quotes ---
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
  file_path <- file.path(directory, "gemini.csv")
  if (verbose) message("\nSaving ", nrow(output_df), " Gemini models to: ", file_path)
  tryCatch({
    write.table(output_df, file = file_path, sep = ",", quote = FALSE,
                row.names = FALSE, col.names = TRUE, na = "", fileEncoding = "UTF-8")
  }, error = function(e) stop("Error saving CSV '", file_path, "' with write.table: ", conditionMessage(e)))

  if (verbose) message("File 'gemini.csv' updated successfully.")
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

#' Update Ollama models list (internal)
#'
#' Connects to a local Ollama server, retrieves installed models and saves a
#' normalized CSV file named `ollama.csv` in the provided directory.
#'
#' @param directory Character path where the CSV will be saved.
#' @param verbose Logical flag to print progress messages.
#' @return Invisibly returns a data frame with the processed models.
#' @keywords internal
#' @noRd
.update_models_ollama <- function(directory = NULL, verbose = TRUE) {
  base_url <- Sys.getenv("OLLAMA_BASE_URL", "http://127.0.0.1:11434")
  base_url <- trimws(base_url)
  if (!nzchar(base_url)) {
    base_url <- "http://127.0.0.1:11434"
  }
  base_url <- sub("/+$", "", base_url)
  api_url <- paste0(base_url, "/api/tags")

  if (verbose) message("Connecting to the Ollama API...")
  response <- tryCatch(
    {
      httr::GET(url = api_url, httr::timeout(30))
    },
    error = function(e) {
      stop("Error connecting to the Ollama API: ", e$message)
    }
  )

  if (httr::status_code(response) != 200) {
    error_content <- httr::content(response, "text", encoding = "UTF-8")
    stop("Ollama API Error (Status: ", httr::status_code(response), "): ", error_content)
  }

  parsed_content <- tryCatch(
    {
      jsonlite::fromJSON(rawToChar(httr::content(response, "raw")), simplifyVector = TRUE)
    },
    error = function(e) {
      stop("Error processing JSON from Ollama API: ", e$message)
    }
  )

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

#' Update provider model lists and write CSVs
#'
#' High-level convenience function to update model lists from one or several
#' providers and write normalized CSV files to a directory.
#'
#' @param provider Optional character scalar. If NULL, updates all supported providers.
#'        Otherwise one of the built-ins ("openrouter", "openai", "anthropic",
#'        "groq", "cerebras", "together", "sambanova", "nebius", "deepseek",
#'        "perplexity", "fireworks", "deepinfra", "hyperbolic", "gemini",
#'        "fal", "replicate", "ollama", "llamacpp") or a custom provider id
#'        configured with [set_provider_openai_compat()].
#' @param directory Character path where CSVs will be saved. Defaults to working dir.
#' @param verbose Logical flag to print progress messages.
#'
#' @return Invisibly returns a named list of data frames, one per provider updated.
#' @examples
#' # Update all providers to the default directory
#' # gen_update_models()
#'
#' # Update a single provider
#' # gen_update_models(provider = "openrouter", directory = tempdir())
#'
#' @export
gen_update_models <- function(provider = NULL, directory = NULL, verbose = TRUE) {
  # Set default directory if not provided
  if (is.null(directory) || is.na(directory)) {
    directory <- tools::R_user_dir("agent_models", which = "data")
  }

  # Create directory if it doesn't exist
  if (!dir.exists(directory)) {
    if (verbose) message("Creating directory: ", directory)
    dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  }

  # Define all available built-in providers
  builtin_providers <- c(
    "openrouter", "openai", "anthropic", "groq", "cerebras", "together", "sambanova",
    "nebius", "deepseek", "perplexity", "fireworks", "deepinfra", "hyperbolic",
    "gemini", "fal", "replicate", "ollama", "llamacpp"
  )
  custom_provider_cfgs <- .genflow_list_custom_provider_configs()
  custom_providers <- names(custom_provider_cfgs)
  all_providers <- unique(c(builtin_providers, custom_providers))

  # Determine which providers to update
  if (is.null(provider)) {
    providers_to_update <- all_providers
    if (verbose) message("Updating all models...")
  } else {
    provider <- tolower(provider)
    provider[provider %in% c("claude")] <- "anthropic"
    provider[provider %in% c("togetherai", "together-ai", "together_ai")] <- "together"
    provider[provider %in% c("samba-nova", "samba_nova")] <- "sambanova"
    provider[provider %in% c("deep-seek", "deep_seek")] <- "deepseek"
    provider[provider %in% c("deep-infra", "deep_infra")] <- "deepinfra"
    provider[provider %in% c("fireworks-ai", "fireworks_ai", "firework")] <- "fireworks"
    provider[provider %in% c("pplx")] <- "perplexity"
    provider[provider %in% c("llama-cpp", "llama_cpp")] <- "llamacpp"
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
    "ollama"     = list(func = ".update_models_ollama",     name = "Ollama"),
    "llamacpp"   = list(func = ".update_models_llamacpp",   name = "llama-cpp")
  )

  # Track progress
  total_providers <- length(providers_to_update)
  current_provider <- 0

  # Update each selected provider
  for (prov in providers_to_update) {
    current_provider <- current_provider + 1
    if (prov %in% names(update_functions)) {
      update_info <- update_functions[[prov]]
      if (verbose) message(sprintf("%d/%d - Updating %s...", current_provider, total_providers, update_info$name))
      tryCatch({
        do.call(update_info$func, list(directory = directory, verbose = verbose))
      }, error = function(e) {
        if (verbose) warning("Failed to update ", update_info$name, ": ", e$message)
      })
      next
    }

    custom_cfg <- custom_provider_cfgs[[prov]]
    custom_name <- custom_cfg$label %||% prov
    if (verbose) message(sprintf("%d/%d - Updating %s...", current_provider, total_providers, custom_name))
    tryCatch({
      .update_models_custom_openai_compat(provider_id = prov, directory = directory, verbose = verbose)
    }, error = function(e) {
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

  invisible(directory)
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
