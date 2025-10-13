#' #' Replicate video generation (internal)
#' #'
#' #' @keywords internal
#' #' @noRd
#' .gen_vid_replicate <- function(prompt, model, temp, steps, h, w, label_sanitized, add_img) {
#'
#'   service_name <- "replicate"
#'   replicate_token <- Sys.getenv("REPLICATE_API_TOKEN")
#'   if (replicate_token == "") stop("REPLICATE_API_TOKEN must be set.")
#'
#'   # --- Model Mapping & Configuration ---
#'   model_id <- model # User provides the identifier
#'   model_path <- model_id
#'   #version <- NULL
#'   api_url <-  paste0("https://api.replicate.com/v1/models/", model_path, "/predictions")
#'
#'   # Check if version is included in the model string
#'   if (grepl(":", model_id)) {
#'     parts <- strsplit(model_id, ":")[[1]]
#'     model_path <- parts[1]
#'     version <- parts[2]
#'   }
#'
#'   # Simplified model name for internal logic (extract last part)
#'   model_short_name <- basename(model_path)
#'
#'   # Specific endpoint overrides
#'   if (model_path %in% c("minimax/video-01", "haiper-ai/haiper-video-2", "luma/ray")) {
#'     api_url <- paste0("https://api.replicate.com/v1/models/", model_path, "/predictions")
#'   }
#'
#'   # --- Parameter Adjustments & Body Construction ---
#'   input_params <- list(prompt = prompt)
#'   body_list <- list(input = input_params)
#'
#'
#'   # Model-specific parameter adjustments
#'   if (model_short_name == "ltx-video") {
#'     valid_aspect_ratios <- list(
#'       "1:1" = c(1, 1), "1:2" = c(1, 2), "2:1" = c(2, 1),
#'       "2:3" = c(2, 3), "3:2" = c(3, 2), "3:4" = c(3, 4),
#'       "4:3" = c(4, 3), "4:5" = c(4, 5), "5:4" = c(5, 4),
#'       "9:16" = c(9, 16), "16:9" = c(16, 9),
#'       "9:21" = c(9, 21), "21:9" = c(21, 9)
#'     )
#'     current_aspect_ratio <- w / h
#'     closest_aspect_ratio <- names(valid_aspect_ratios)[which.min(abs(sapply(valid_aspect_ratios, function(x) x[1] / x[2]) - current_aspect_ratio))]
#'     # Note: LTX seems to primarily use aspect_ratio string, not w/h directly? API docs needed.
#'     # Assuming we still pass w/h based on original code's intent:
#'     target_ratio <- valid_aspect_ratios[[closest_aspect_ratio]]
#'     if (w / h > target_ratio[1] / target_ratio[2]) { w <- round(h * target_ratio[1] / target_ratio[2]) }
#'     else { h <- round(w * target_ratio[2] / target_ratio[1]) }
#'     w <- round(w / 8) * 8
#'     h <- round(h / 8) * 8
#'     body_list$input$width <- w
#'     body_list$input$height <- h
#'     # body_list$input$aspect_ratio <- closest_aspect_ratio # Uncomment if API prefers this
#'
#'     if (!is.null(add_img)) {
#'       img_b64 <- .encode_image(add_img)
#'       if (!is.null(img_b64)) {
#'         body_list$input$image <- paste0("data:image/png;base64,", img_b64) # Check mime type if not always png
#'       }
#'     }
#'     # Map 'temp' - Check LTX docs for correct param name (e.g., guidance_scale?)
#'     # body_list$input$guidance_scale <- temp
#'
#'   } else if (model_short_name == "video-01") { # minimax
#'     body_list$input$prompt_optimizer <- TRUE
#'     if (!is.null(add_img)) {
#'       img_b64 <- .encode_image(add_img)
#'       if (!is.null(img_b64)) {
#'         body_list$input$first_frame_image <- paste0("data:image/png;base64,", img_b64)
#'       }
#'     }
#'     # Map 'temp' - Check Minimax docs
#'
#'   } else if (model_short_name == "hunyuan-video") {
#'     body_list$input$width <- 854 # Fixed values from original code
#'     body_list$input$height <- 480
#'     body_list$input$flow_shift <- 7
#'     body_list$input$infer_steps <- 50
#'     body_list$input$video_length <- 129
#'     body_list$input$embedded_guidance_scale <- temp # Assuming temp maps here
#'
#'   } else if (model_short_name == "haiper-video-2") {
#'     body_list$input$duration <- 6 # Example default, might need parameter
#'     body_list$input$aspect_ratio <- "16:9" # Example default
#'     body_list$input$use_prompt_enhancer <- TRUE
#'     # Map 'temp'?
#'
#'   } else if (model_short_name == "ray") { # Luma
#'     body_list$input$loop <- FALSE
#'     body_list$input$aspect_ratio <- "16:9"
#'     # Map 'temp'? Image input?
#'     if (!is.null(add_img)) {
#'       # Check Luma Ray documentation for image input format (URL? Base64?)
#'       # Example assuming URL upload needed first (more complex) or base64:
#'       img_b64 <- .encode_image(add_img)
#'       if (!is.null(img_b64)) {
#'         # body_list$input$image_url <- upload_image_and_get_url(img_b64) # Fictional helper
#'         # OR
#'         # body_list$input$image_base64 <- img_b64 # If supported
#'         warning("Image input for Luma Ray needs specific implementation based on API docs.")
#'       }
#'     }
#'
#'   } else if (model_short_name %in% c("coherent-video-modeling", "animatediff")) {
#'     # Generic parameters, check docs for specifics like temp mapping, w/h
#'     body_list$input$width <- w
#'     body_list$input$height <- h
#'     # body_list$input$guidance_scale <- temp
#'   } else {
#'     print("Parameter configuration not defined for Replicate model: ", model_id, ". Using prompt only.")
#'   }
#'
#'
#'   # --- API Call & Polling ---
#'   headers <- httr::add_headers(
#'     'Content-Type' = 'application/json',
#'     'Authorization' = paste("Token", replicate_token)
#'     # 'Prefer' = 'wait=60' # Optional: request longer wait time
#'   )
#'
#'   cat("Sending initial request to Replicate (model:", model_path, ")...\n")
#'   # Ensure nulls are handled correctly, especially if some inputs become NULL
#'   body_json <- jsonlite::toJSON(body_list, auto_unbox = TRUE, null = "null", force=TRUE)
#'   # print(body_json) # Debug output
#'
#'   response <- httr::POST(
#'     api_url,
#'     headers,
#'     body = body_json,
#'     encode = "raw" # Use raw encoding since body is already JSON
#'   )
#'
#'   if (!(httr::status_code(response) %in% c(200, 201))) {
#'     erro <- httr::content(response, "text", encoding = "UTF-8")
#'     detail_error <- tryCatch(jsonlite::fromJSON(erro)$detail, error = function(e) erro)
#'     stop(sprintf("Initial Replicate API error (%s): %s\nURL: %s\nBody: %s",
#'                  httr::status_code(response), detail_error, api_url, body_json))
#'   }
#'
#'   content <- httr::content(response, as = "parsed", simplifyVector = TRUE)
#'   get_url <- content$urls$get
#'   prediction_id <- content$id # Useful for debugging
#'
#'   if(is.null(get_url)) {
#'     print(content)
#'     stop("Polling URL (urls$get) not found in Replicate response. Prediction ID: ", prediction_id)
#'   }
#'
#'   status <- content$status
#'   cat("Initial Replicate status:", status, "(ID:", prediction_id, "). Waiting...\n")
#'
#'   poll_start_time <- Sys.time()
#'   max_poll_time_secs <- 600 # 10 minutes timeout for polling
#'
#'   while (status %in% c("starting", "processing")) {
#'     elapsed_time <- as.numeric(difftime(Sys.time(), poll_start_time, units = "secs"))
#'     if (elapsed_time > max_poll_time_secs) {
#'       stop("Timeout (", max_poll_time_secs, "s) reached while waiting for Replicate result. ID: ", prediction_id)
#'     }
#'
#'     Sys.sleep(5) # Increase sleep time slightly
#'     poll_response <- httr::GET(get_url, httr::add_headers('Authorization' = paste("Token", replicate_token)))
#'
#'     if (httr::status_code(poll_response) != 200) {
#'       erro <- httr::content(poll_response, "text", encoding = "UTF-8")
#'       detail_error <- tryCatch(jsonlite::fromJSON(erro)$detail, error = function(e) erro)
#'       warning(sprintf("Replicate API status warning (%s): %s. Retrying...",
#'                       httr::status_code(poll_response), detail_error))
#'       Sys.sleep(5) # Extra sleep on error before retrying poll
#'       next # Continue loop
#'     }
#'
#'     poll_content <- httr::content(poll_response, as = "parsed", simplifyVector = TRUE)
#'     status <- poll_content$status
#'     cat("Current Replicate status:", status, sprintf("(%.0fs)\n", elapsed_time))
#'
#'     if (!is.null(poll_content$error)) {
#'       stop("Replicate prediction failed (ID: ", prediction_id, "): ", poll_content$error)
#'     }
#'     if (status == "failed") {
#'       error_details <- paste(names(poll_content$logs), poll_content$logs, collapse="\n")
#'       stop("Replicate prediction failed (ID: ", prediction_id, "). Logs:\n", error_details)
#'     }
#'     if (status == "canceled") stop("Replicate prediction canceled (ID: ", prediction_id, ").")
#'   }
#'
#'   if (status != "succeeded") {
#'     print(poll_content)
#'     stop("Replicate prediction did not succeed (ID: ", prediction_id, "). Final status: ", status)
#'   }
#'
#'   # --- Download Result ---
#'   result <- poll_content$output
#'   if (is.null(result) || length(result) == 0) {
#'     print(poll_content)
#'     stop("Output not found in Replicate response (ID: ", prediction_id, ").")
#'   }
#'
#'   # Output might be a list or a single URL
#'   video_url <- result[[1]]
#'   if (!is.character(video_url) || nchar(video_url) == 0) {
#'     print(poll_content)
#'     stop("Invalid video URL in Replicate result (ID: ", prediction_id, ").")
#'   }
#'
#'   cat("Downloading Replicate video from:", video_url, "\n")
#'   temp_video_path <- tempfile(fileext = ".mp4")
#'
#'   tryCatch({
#'     download_response <- httr::GET(video_url, httr::write_disk(temp_video_path, overwrite = TRUE), httr::timeout(120)) # 2 min timeout for download
#'     if (download_response$status_code != 200) {
#'       stop(sprintf("Error %s downloading video from %s", download_response$status_code, video_url))
#'     }
#'     if (!file.exists(temp_video_path) || file.info(temp_video_path)$size == 0) {
#'       stop("Failed to verify downloaded video file or file is empty.")
#'     }
#'   }, error = function(e) {
#'     if (file.exists(temp_video_path)) file.remove(temp_video_path) # Clean up failed download
#'     stop("Failed to download or save Replicate video: ", e$message)
#'   })
#'
#'   cat("Temporary Replicate video saved at:", temp_video_path, "\n")
#'   return(temp_video_path)
#' }
#'
#' #' Generate video via selected provider
#' #'
#' #' High-level wrapper that dispatches to provider-specific implementations
#' #' (e.g., FAL, Replicate, DeepInfra) to generate a video file. Saves the
#' #' generated video and optionally concatenates multiple segments.
#' #'
#' #' @param prompt Character text prompt.
#' #' @param add Optional character to append to the prompt.
#' #' @param add_img Optional image path to condition the first frame (provider-specific).
#' #' @param directory Output directory for the saved video.
#' #' @param label Optional label used in the filename.
#' #' @param service Provider service name (e.g., "fal", "replicate", "deepinfra").
#' #' @param model Provider model identifier.
#' #' @param temp Numeric temperature/guidance parameter.
#' #' @param steps Integer inference steps.
#' #' @param h Height in pixels.
#' #' @param w Width in pixels.
#' #' @param preview Logical; open the video after generation if interactive.
#' #' @param extend Logical; if TRUE, generate extra segments chained from the last frame.
#' #' @param extend_times Optional number of extra segments to generate.
#' #'
#' #' @return Invisibly returns a list with fields including `response_value` (saved file),
#' #'   `status_api`, `status_msg`, `service`, `model`, `temp`, `duration` and others.
#' #'
#' #' @examples
#' #' # .gen_vid("A cat running in a field", service = "fal",
#' #' #         model = "fal-ai/ltx-video-v095", h = 576, w = 1024)
#' #' @noRd
#' .gen_vid <- function(
#'     prompt,
#'     add           = NULL,
#'     add_img       = NULL,
#'     directory     = "content",
#'     label         = NULL,
#'     service       = "fal",
#'     model        = "fal-ai/ltx-video-v095",
#'     temp          = 0.8,
#'     steps         = 18,
#'     h             = 576,
#'     w             = 1024,
#'     preview       = FALSE,
#'     extend        = FALSE,
#'     extend_times  = NULL
#' ) {
#'   start_time <- Sys.time()
#'
#'   # 1) Build prompt and sanitize
#'   final_prompt <- if (!is.null(add)) paste(prompt, add) else prompt
#'   service      <- tolower(as.character(service)[1])
#'   model       <- as.character(model)[1]
#'
#'   # 2) Prepare directory
#'   if (is.null(directory)) directory <- "~/videos"
#'   directory <- path.expand(directory)
#'   if (!dir.exists(directory)) {
#'     dir.create(directory, recursive = TRUE, showWarnings = FALSE)
#'   }
#'
#'   # 3) Label for filename
#'   if (is.null(label) || !nzchar(label)) {
#'     palavras <- strsplit(final_prompt, "\\s+")[[1]]
#'     label <- paste(head(palavras, 5), collapse = "_")
#'   }
#'   label <- substr(label, 1, 36)
#'   .sanitize_filename <- function(x) gsub("[^0-9A-Za-z_.-]", "_", x)
#'   label_sanitized <- .sanitize_filename(label)
#'   model_sanitized <- .sanitize_filename(model)
#'
#'   # 4) Generate initial video (temp file)
#'   error_message <- NULL
#'   temp_video <- tryCatch({
#'     switch(service,
#'      #      "fal"       = .gen_vid_fal(final_prompt, model, temp, steps, h, w, label_sanitized, add_img),
#'            "replicate" = .gen_vid_replicate(final_prompt, model, temp, steps, h, w, label_sanitized, add_img),
#'       #     "deepinfra" = .gen_vid_deepinfra(final_prompt, model, temp, steps, h, w, label_sanitized, add_img),
#'            stop("Unsupported video service: ", service)
#'     )
#'   }, error = function(e) {
#'     error_message <- conditionMessage(e)
#'     NULL
#'   })
#'
#'   # 5) Validate return
#'   final_status <- if (is.null(temp_video)) "ERROR" else "SUCCESS"
#'   final_msg <- if (final_status == "ERROR") {
#'     if (!is.null(error_message)) error_message else "Subfunction did not return video path"
#'   } else "OK"
#'   final_path <- NA_character_
#'
#'   # 6) Copy temp to destination path
#'   if (final_status == "SUCCESS") {
#'     if (!is.character(temp_video) || length(temp_video) != 1) {
#'       final_status <- "ERROR"
#'       final_msg    <- paste0(
#'         "Unexpected subfunction return (expected character(1)): ",
#'         paste0(capture.output(str(temp_video)), collapse = " ")
#'       )
#'     } else if (!file.exists(temp_video)) {
#'       final_status <- "ERROR"
#'       final_msg    <- paste0("Temporary file does not exist: ", temp_video)
#'     } else {
#'       dt0       <- format(Sys.time(), "%Y%m%d_%H%M%S")
#'       filename0 <- sprintf("%s_%s_%s_%s.mp4",
#'                            label_sanitized, service, model_sanitized, dt0)
#'       path0     <- file.path(directory, filename0)
#'       ok        <- file.copy(temp_video, path0, overwrite = TRUE)
#'       if (!ok || !file.exists(path0)) {
#'         final_status <- "ERROR"
#'         final_msg    <- sprintf("Failed to copy video to '%s'", path0)
#'       } else {
#'         final_path <- path0
#'       }
#'     }
#'
#'     # 6.1) if extend = TRUE, repeat
#'     if (final_status == "SUCCESS" && isTRUE(extend)) {
#'       times <- if (is.null(extend_times)) 1 else as.integer(extend_times)
#'       if (is.na(times) || times < 1) times <- 1
#'
#'       segmentos <- final_path
#'
#'       for (i in seq_len(times)) {
#'         # last frame
#'         lf <- .find_last_frame(segmentos[length(segmentos)])
#'         # extract still
#'         st <- .save_video_still(segmentos[length(segmentos)],
#'                                directory = tempdir(),
#'                                frame     = lf)
#'         img_still <- st$saved_file
#'
#'         # generate next segment
#'         tmp_ext <- switch(service,
#'                         #  "fal"       = .gen_vid_fal(final_prompt, model, temp, steps, h, w, label_sanitized, img_still),
#'                           "replicate" = .gen_vid_replicate(final_prompt, model, temp, steps, h, w, label_sanitized, img_still),
#'                          # "deepinfra" = .gen_vid_deepinfra(final_prompt, model, temp, steps, h, w, label_sanitized, img_still),
#'                           stop("Unsupported video service: ", service)
#'         )
#'
#'         # copy segment
#'         dti    <- format(Sys.time(), "%Y%m%d_%H%M%S")
#'         fn_ext <- sprintf("%s_ext%02d_%s_%s_%s.mp4",
#'                           label_sanitized, i,
#'                           service, model_sanitized, dti)
#'         path_ext <- file.path(directory, fn_ext)
#'         if (!file.copy(tmp_ext, path_ext, overwrite = TRUE)) {
#'           stop("Failed to copy extended segment to ", path_ext)
#'         }
#'         segmentos <- c(segmentos, path_ext)
#'       }
#'
#'       # 6.1.5) concatenate with ffmpeg
#'       ffmpeg_bin <- Sys.which("ffmpeg")
#'       if (ffmpeg_bin == "") stop("Could not find ffmpeg in PATH.")
#'       seg_abspath <- normalizePath(segmentos, winslash = "/", mustWork = TRUE)
#'       lista_txt   <- tempfile(tmpdir = tempdir(), fileext = ".txt")
#'       lines       <- paste("file", shQuote(seg_abspath))
#'       writeLines(lines, lista_txt)
#'
#'       dt_ext    <- format(Sys.time(), "%Y%m%d_%H%M%S")
#'       fn_concat <- sprintf("%s_%s_%s_%s_extended.mp4",
#'                            label_sanitized, service, model_sanitized, dt_ext)
#'       path_concat <- file.path(directory, fn_concat)
#'
#'       concat_args <- c(
#'         "-f", "concat", "-safe", "0",
#'         "-i", lista_txt,
#'         "-c", "copy",
#'         "-y", path_concat
#'       )
#'       ffout <- system2(ffmpeg_bin, concat_args, stdout = TRUE, stderr = TRUE)
#'       if (!file.exists(path_concat)) {
#'         stop(
#'           "Failed to concatenate segments. ffmpeg returned:\n",
#'           paste(ffout, collapse = "\n")
#'         )
#'       }
#'       final_path <- path_concat
#'       file.remove(lista_txt)
#'     }
#'   }
#'
#'   # 7) Preview
#'   if (preview && final_status == "SUCCESS") {
#'     if (interactive()) browseURL(final_path)
#'     else message("Preview not available in non-interactive mode.")
#'   }
#'
#'   # 8) Build return
#'   total_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
#'   resultado <- list(
#'     response_value  = final_path,
#'     label           = label,
#'     label_cat       = label_sanitized,
#'     service         = service,
#'     model          = model,
#'     temp            = temp,
#'     duration           = total_time,
#'     status_api      = final_status,
#'     status_msg = final_msg,
#'     saved_file   = if (final_status == "SUCCESS") final_path else NA_character_,
#'     dimensoes       = paste0(w, "x", h)
#'   )
#'
#'   return(invisible(resultado))
#' }
#'
#' #' Transcribe an MP3 file using different providers
#' #'
#' #' Sends an MP3 file for transcription using one of several supported backends
#' #' (OpenAI, Groq, AssemblyAI, Cloudflare, Voicegain, or Hugging Face). Writes a
#' #' `.txt` file with the transcript alongside the audio file.
#' #'
#' #' @param mp3_filepath Character path to the MP3 file.
#' #' @param model Character provider key: 'openai', 'groq', 'assemblyai', 'cloudflare', 'voicegain', or 'hf'.
#' #' @return Character path to the saved `.txt` transcript.
#' #'
#' #' @examples
#' #' # .transcribe_mp3("/path/audio.mp3", model = "hf")
#' #' @noRd
#' .transcribe_mp3 <- function(mp3_filepath, model = 'hf') {
#'   txt_filepath <- sub("\\.mp3$", ".txt", mp3_filepath)
#'
#'   # If the transcription file already exists, return the path
#'   if (file.exists(txt_filepath)) {
#'     return(txt_filepath)
#'   }
#'
#'   if (model == 'openai') {
#'     # Use the Whisper API to transcribe the MP3 file
#'     audio_file <- upload_file(mp3_filepath)
#'     api_key <- Sys.getenv("OPENAI_API_KEY")
#'
#'     response <- POST(
#'       url = "https://api.openai.com/v1/audio/transcriptions",
#'       add_headers(Authorization = paste("Bearer", api_key)),
#'       body = list(
#'         file = audio_file,
#'         model = "whisper-1",
#'         response_format = "json",
#'         language = "pt"
#'       ),
#'       encode = "multipart"
#'     )
#'
#'     # Check for errors
#'     if (response$status_code != 200) {
#'       stop("API request error: ", content(response, "text", encoding = "UTF-8"))
#'     }
#'
#'     # Get the transcription
#'     result <- content(response, as = "parsed", type = "application/json", encoding = "UTF-8")
#'     transcricao <- result$text
#'
#'   } else if (model == 'groq') {
#'     # Use Groq API to transcribe the MP3 file
#'
#'     # Get Groq API key from environment variable
#'     GROQ_API_KEY <- Sys.getenv("GROQ_API_KEY")
#'
#'     # Build the API URL
#'     url <- "https://api.groq.com/openai/v1/audio/transcriptions"
#'
#'     # Prepare the audio file for upload
#'     audio_file <- httr::upload_file(mp3_filepath)
#'
#'     # Make the POST request
#'     response <- httr::POST(
#'       url = url,
#'       httr::add_headers(
#'         Authorization = paste("Bearer", GROQ_API_KEY)
#'       ),
#'       body = list(
#'         file = audio_file,
#'         model = "whisper-large-v3-turbo",
#'         temperature = 0,
#'         response_format = "json",
#'         language = "pt"  # Change to "en" for English
#'       ),
#'       encode = "multipart"
#'     )
#'
#'     # Check for errors
#'     if (response$status_code != 200) {
#'       stop("Groq API request error: ", httr::content(response, "text", encoding = "UTF-8"))
#'     }
#'
#'     # Get the transcription
#'     result <- httr::content(response, as = "parsed", type = "application/json", encoding = "UTF-8")
#'     transcricao <- result$text  # Adjust this path if needed
#'     Sys.sleep(3)
#'
#'   } else if (model == 'assemblyai') {
#'     # Use the AssemblyAI API to transcribe the MP3 file
#'
#'     # Your AssemblyAI API key
#'     ASSEMBLYAI_API_KEY <- Sys.getenv("ASSEMBLYAI_API_KEY")
#'
#'     # Step 1: Upload the audio file to AssemblyAI
#'     upload_url <- "https://api.assemblyai.com/v2/upload"
#'     audio_file <- mp3_filepath  # Path to your MP3 file
#'
#'     # Upload the audio file
#'     response <- httr::POST(
#'       url = upload_url,
#'       httr::add_headers(
#'         Authorization = ASSEMBLYAI_API_KEY
#'       ),
#'       body = httr::upload_file(audio_file)
#'     )
#'
#'     # Check for errors in the upload response
#'     if (response$status_code != 200) {
#'       stop("Error uploading file to AssemblyAI: ", httr::content(response, "text", encoding = "UTF-8"))
#'     }
#'
#'     # Retrieve the uploaded audio URL
#'     upload_result <- httr::content(response, as = "parsed", type = "application/json", encoding = "UTF-8")
#'     audio_url <- upload_result$upload_url
#'
#'     # Step 2: Submit the transcription request
#'     transcript_endpoint <- "https://api.assemblyai.com/v2/transcript"
#'
#'     transcript_response <- httr::POST(
#'       url = transcript_endpoint,
#'       httr::add_headers(
#'         Authorization = ASSEMBLYAI_API_KEY,
#'         `Content-Type` = "application/json"
#'       ),
#'       body = list(
#'         audio_url = audio_url,
#'         language_code = "pt"  # Change to "en" for English
#'       ),
#'       encode = "json"
#'     )
#'
#'     # Check for errors in the transcription request
#'     if (transcript_response$status_code != 200) {
#'       stop("Error submitting transcription request to AssemblyAI: ", httr::content(transcript_response, "text", encoding = "UTF-8"))
#'     }
#'
#'     # Retrieve the transcript ID
#'     transcript_result <- httr::content(transcript_response, as = "parsed", type = "application/json", encoding = "UTF-8")
#'     transcript_id <- transcript_result$id
#'
#'     # Step 3: Poll for transcription completion
#'     polling_endpoint <- paste0("https://api.assemblyai.com/v2/transcript/", transcript_id)
#'
#'     repeat {
#'       polling_response <- httr::GET(
#'         url = polling_endpoint,
#'         httr::add_headers(
#'           Authorization = ASSEMBLYAI_API_KEY
#'         )
#'       )
#'
#'       transcription_result <- httr::content(polling_response, as = "parsed", type = "application/json", encoding = "UTF-8")
#'
#'       if (transcription_result$status == "completed") {
#'         transcricao <- transcription_result$text
#'         break
#'       } else if (transcription_result$status == "error") {
#'         stop("Transcription failed: ", transcription_result$error)
#'       } else {
#'         Sys.sleep(5)  # Wait before checking again
#'       }
#'     }
#'
#'   } else if (model == 'cloudflare') {
#'     # Use Cloudflare API to transcribe the MP3 file
#'
#'     # Get Cloudflare credentials from environment variables
#'     CLOUDFLARE_ACCOUNT_ID <- Sys.getenv("CLOUDFLARE_ACCOUNT_ID")
#'     CLOUDFLARE_API_TOKEN <- Sys.getenv("CLOUDFLARE_API_TOKEN")
#'
#'     if (CLOUDFLARE_ACCOUNT_ID == "" || CLOUDFLARE_API_TOKEN == "") {
#'       stop("Please set environment variables 'CLOUDFLARE_ACCOUNT_ID' and 'CLOUDFLARE_API_TOKEN'.")
#'     }
#'
#'     # Build the API URL
#'     url <- paste0(
#'       "https://api.cloudflare.com/client/v4/accounts/",
#'       CLOUDFLARE_ACCOUNT_ID,
#'       "/ai/run/@cf/openai/whisper"
#'     )
#'
#'     # Read the MP3 file as binary data
#'     binary_data <- readBin(mp3_filepath, what = "raw", n = file.info(mp3_filepath)$size)
#'
#'     # Make the POST request
#'     response <- httr::POST(
#'       url = url,
#'       httr::add_headers(
#'         Authorization = paste("Bearer", CLOUDFLARE_API_TOKEN),
#'         'Content-Type' = 'application/octet-stream'
#'       ),
#'       body = binary_data,
#'       encode = "raw"
#'     )
#'
#'     # Check for errors
#'     if (response$status_code != 200) {
#'       stop("Cloudflare API request error: ", httr::content(response, "text", encoding = "UTF-8"))
#'     }
#'
#'     # Get the transcription (adjust according to the API's actual response format)
#'     result <- httr::content(response, as = "parsed", type = "application/json", encoding = "UTF-8")
#'     transcricao <- result$result$text  # Adjust this path as needed
#'
#'   } else if (model == 'voicegain') {
#'     # Use Voicegain API to transcribe the MP3 file in OFF-LINE mode
#'
#'     # 1. Get Voicegain API key from environment variable
#'     VOICEGAIN_API_KEY <- Sys.getenv("VOICEGAIN_API_KEY")
#'
#'     if (VOICEGAIN_API_KEY == "") {
#'       stop("Please set environment variable 'VOICEGAIN_API_KEY'.")
#'     }
#'
#'     # 2. Prepare the JSON payload
#'     payload <- list(
#'       sessions = list(
#'         list(
#'           asyncMode = "OFF-LINE",
#'           poll = list(
#'             persist = 600000  # 10 minutes in milliseconds
#'           ),
#'           content = list(
#'             incremental = list("progress"),
#'             full = list("transcript", "words")
#'           )
#'         )
#'       ),
#'       audio = list(
#'         source = list(
#'           fromUrl = list(
#'             url = mp3_filepath  # Assuming mp3_filepath is an accessible URL
#'           )
#'         )
#'       ),
#'       settings = list(
#'         asr = list(
#'           languages = list("pt")  # Brazilian Portuguese
#'         )
#'       )
#'     )
#'
#'     # 3. Convert the payload to JSON
#'     payload_json <- jsonlite::toJSON(payload, auto_unbox = TRUE)
#'
#'     # 4. Make the initial POST request
#'     response <- httr::POST(
#'       url = "https://api.voicegain.ai/v1/asr/transcribe/async",
#'       httr::add_headers(
#'         "Authorization" = paste("Bearer", VOICEGAIN_API_KEY),
#'         "Content-Type" = "application/json",
#'         "Accept" = "application/json"
#'       ),
#'       body = payload_json
#'     )
#'
#'     # 5. Verify the request succeeded
#'     if (response$status_code != 202) {
#'       stop("Voicegain API request error: ", httr::content(response, "text", encoding = "UTF-8"))
#'     }
#'
#'     # 6. Extract the polling URL from the response
#'     init_response <- httr::content(response, as = "parsed", type = "application/json", encoding = "UTF-8")
#'     polling_url <- init_response$sessions[[1]]$poll$url
#'
#'     # 7. Implement the polling loop
#'     max_retries <- 60  # 5 minutes with 5-second interval
#'     for (i in 1:max_retries) {
#'       Sys.sleep(5)  # Wait 5 seconds between checks
#'
#'       poll_response <- httr::GET(
#'         url = paste0(polling_url, "?full=false"),
#'         httr::add_headers(
#'           "Authorization" = paste("Bearer", VOICEGAIN_API_KEY),
#'           "Accept" = "application/json"
#'         )
#'       )
#'
#'       poll_content <- httr::content(poll_response, as = "parsed", type = "application/json", encoding = "UTF-8")
#'
#'       if (poll_content$result$final) {
#'         break
#'       }
#'
#'       cat("Progress: ", poll_content$progress$phase, "\n")
#'     }
#'
#'     # 8. Get the final result
#'     final_response <- httr::GET(
#'       url = paste0(polling_url, "?full=true"),
#'       httr::add_headers(
#'         "Authorization" = paste("Bearer", VOICEGAIN_API_KEY),
#'         "Accept" = "application/json"
#'       )
#'     )
#'
#'     final_content <- httr::content(final_response, as = "parsed", type = "application/json", encoding = "UTF-8")
#'
#'     # 9. Check the status and extract the transcription
#'     if (final_content$result$status == "MATCH") {
#'       transcricao <- final_content$result$transcript
#'     } else {
#'       stop("Transcription failed with status: ", final_content$result$status)
#'     }
#'
#'     if (is.null(transcricao) || transcricao == "") {
#'       stop("The transcription is empty.")
#'     }
#'
#'   } else if (model == 'hf') {
#'     # Use Hugging Face API to transcribe the MP3 file
#'
#'     # 1. Get Hugging Face API key from environment variable
#'     HUGGINGFACE_API_TOKEN <- Sys.getenv("HUGGINGFACE_API_TOKEN")
#'
#'     if (HUGGINGFACE_API_TOKEN == "") {
#'       stop("Please set environment variable 'HUGGINGFACE_API_TOKEN'.")
#'     }
#'
#'     # 2. Define the model API URL
#'     API_URL <- "https://api-inference.hf.co/models/openai/whisper-large-v3-turbo"
#'
#'     # 3. Read the MP3 file as binary data
#'     binary_data <- readBin(mp3_filepath, what = "raw", n = file.info(mp3_filepath)$size)
#'
#'     # 4. Make the POST request
#'     response <- httr::POST(
#'       url = API_URL,
#'       httr::add_headers(
#'         Authorization = paste("Bearer", HUGGINGFACE_API_TOKEN),
#'         `Content-Type` = "application/octet-stream"
#'       ),
#'       body = binary_data,
#'       encode = "raw"
#'     )
#'
#'     # 5. Check for errors
#'     if (response$status_code != 200) {
#'       stop("Hugging Face API request error: ", httr::content(response, "text", encoding = "UTF-8"))
#'     }
#'
#'     # 6. Get the transcription
#'     result <- httr::content(response, as = "parsed", type = "application/json", encoding = "UTF-8")
#'
#'     # The response structure may vary. Typically, the transcription is in result$text
#'     if (!is.null(result$text)) {
#'       transcricao <- result$text
#'     } else if (!is.null(result$transcription)) {  # Caso a response tenha outro campo
#'       transcricao <- result$transcription
#'     } else {
#'       stop("Unexpected response format from Hugging Face API.")
#'     }
#'
#'   } else {
#'     stop("Invalid model. Please choose 'openai', 'groq', 'assemblyai', 'cloudflare', 'voicegain' or 'hf'.")
#'   }
#'
#'   # Save the transcription to a txt file
#'   writeLines(transcricao, txt_filepath, useBytes = TRUE)
#'   cat("Transcription saved at:", txt_filepath, "\n")
#'   return(txt_filepath)
#' }
#'
#' #' Generate audio from text via provider
#' #'
#' #' Calls a provider (e.g., FAL) to synthesize audio from text. Saves the audio
#' #' on disk and returns metadata about the saved file.
#' #'
#' #' @param prompt Character prompt.
#' #' @param add Optional character text appended to prompt.
#' #' @param add_audio Optional warm-start audio file path (provider-specific).
#' #' @param directory Output directory for the saved audio.
#' #' @param label Optional short label for filenames.
#' #' @param service Provider service name (e.g., "fal").
#' #' @param model Provider model identifier.
#' #' @param duration Numeric desired duration (seconds) if supported.
#' #' @param influence Numeric warm-start influence (provider-specific).
#' #' @param preview Logical; attempt to open audio after save if interactive.
#' #'
#' #' @return Invisibly returns a list containing `response_value` (saved file),
#' #'   `status_api`, `status_msg`, `duration` and other metadata.
#' #'
#' #' @examples
#' #' # .text_to_audio("Ocean waves", service = "fal",
#' #' #              model = "fal-ai/elevenlabs/sound-effects", duration = 5)
#' #'
#' #' @noRd
#' .text_to_audio <- function(
#'     prompt,
#'     add         = NULL,
#'     add_audio   = NULL,
#'     directory   = "content",
#'     label       = NULL,
#'     service     = "fal",
#'     model      = "fal-ai/elevenlabs/sound-effects",
#'     duration    = 5,
#'     influence   = 1,
#'     preview     = FALSE
#' ) {
#'   start_time <- Sys.time()
#'   # 1) build prompt
#'   final_prompt <- if (!is.null(add)) paste(prompt, add) else prompt
#'   service      <- tolower(as.character(service)[1])
#'   model       <- as.character(model)[1]
#'
#'   # 2) directory
#'   if (is.null(directory)) directory <- "~/audios"
#'   directory <- path.expand(directory)
#'   if (!dir.exists(directory)) {
#'     dir.create(directory, recursive = TRUE, showWarnings = FALSE)
#'   }
#'
#'   # 3) label
#'   if (is.null(label) || !nzchar(label)) {
#'     palavras <- strsplit(final_prompt, "\\s+")[[1]]
#'     label <- paste(head(palavras, 5), collapse = "_")
#'   }
#'   label <- substr(label, 1, 36)
#'   .sanitize_filename <- function(x) gsub("[^0-9A-Za-z_.-]", "_", x)
#'   label_sanitized <- .sanitize_filename(label)
#'   model_sanitized <- .sanitize_filename(model)
#'
#'   # 4) call audio subfunction
#'   error_message <- NULL
#'   temp_audio <- tryCatch({
#'     switch(service,
#'            "fal" = .text_to_audio_fal(final_prompt, model, duration, influence, label_sanitized, add_audio),
#'            stop("Unsupported audio service: ", service)
#'     )
#'   }, error = function(e) {
#'     error_message <- conditionMessage(e)
#'     NULL
#'   })
#'
#'   # 5) prepare return
#'   final_status <- if (is.null(temp_audio)) "ERROR" else "SUCCESS"
#'   final_msg <- if (final_status == "ERROR") {
#'     if (!is.null(error_message)) error_message else "Subfunction did not return audio path"
#'   } else "OK"
#'   final_path <- NA_character_
#'
#'   # 6) if OK, copy from tmp to destination path
#'   if (final_status == "SUCCESS") {
#'     if (!is.character(temp_audio) || length(temp_audio) != 1) {
#'       final_status <- "ERROR"
#'       final_msg <- paste0(
#'         "Unexpected subfunction return (expected character(1), received): ",
#'         paste0(capture.output(str(temp_audio)), collapse = " ")
#'       )
#'     } else if (!file.exists(temp_audio)) {
#'       final_status <- "ERROR"
#'       final_msg <- paste0("Temporary file does not exist: ", temp_audio)
#'     } else {
#'       dt <- format(Sys.time(), "%Y%m%d_%H%M%S")
#'       filename <- sprintf("%s_%s_%s_%s.wav",
#'                           label_sanitized, service, model_sanitized, dt)
#'       final_path <- file.path(directory, filename)
#'       ok <- file.copy(temp_audio, final_path, overwrite = TRUE)
#'       if (!ok || !file.exists(final_path)) {
#'         final_status <- "ERROR"
#'         final_msg    <- sprintf("Failed to copy audio to '%s'", final_path)
#'         final_path   <- NA_character_
#'       }
#'     }
#'   }
#'
#'   # 7) optional preview
#'   if (preview && final_status == "SUCCESS") {
#'     if (interactive()) browseURL(final_path)
#'     else message("Preview not available in non-interactive mode.")
#'   }
#'
#'   # 8) build output list
#'   duration <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
#'   resultado <- list(
#'     response_value  = final_path,
#'     label           = label,
#'     label_cat       = label_sanitized,
#'     service         = service,
#'     model          = model,
#'     duration        = duration,
#'     influence       = influence,
#'     duration           = duration,
#'     status_api      = final_status,
#'     status_msg = final_msg,
#'     saved_file   = if (final_status == "SUCCESS") final_path else NA_character_,
#'     formato         = "wav"
#'   )
#'   return(invisible(resultado))
#' }
#' .text_to_audio_fal <- function(
#'     prompt,
#'     model,
#'     duration,
#'     influence,
#'     label_sanitized,
#'     add_audio   = NULL
#' ) {
#'   fal_token <- Sys.getenv("FAL_API_KEY")
#'   if (fal_token == "") stop("FAL_API_KEY must be set.")
#'
#'   # 1) build URL and body
#'   url <- sprintf("https://queue.fal.run/%s", model)
#'   body <- list(
#'     text              = prompt,
#'     input = prompt,
#'     voice = "Jennifer (English (US)/American)",
#'     duration_seconds = duration,
#'     prompt_influence      = influence
#'   )
#'   # if you want to pass initial audio (warm-start)
#'   if (!is.null(add_audio)) {
#'     # assuming you have a function encode_audio() that returns pure base64
#'     audio_b64 <- encode_audio(add_audio)
#'     if (!is.null(audio_b64)) {
#'       body$audio_url <- paste0("data:audio/wav;base64,", audio_b64)
#'     }
#'   }
#'
#'   headers <- httr::add_headers(
#'     `Content-Type`  = "application/json",
#'     Authorization   = paste("Key", fal_token)
#'   )
#'
#'   # 2) initial POST
#'   response <- httr::POST(
#'     url,
#'     headers,
#'     body   = jsonlite::toJSON(body, auto_unbox = TRUE, null = "null"),
#'     encode = "raw"
#'   )
#'   if (httr::status_code(response) >= 400) {
#'     txt <- httr::content(response, "text", encoding = "UTF-8")
#'     stop(sprintf("Initial FAL API error (%s): %s",
#'                  httr::status_code(response), txt))
#'   }
#'   content1 <- httr::content(response, as = "parsed", simplifyVector = TRUE)
#'
#'   # 3) extract status and response URLs
#'   links       <- content1[["_links"]]
#'   status_url   <- if (!is.null(links$status$href))   links$status$href   else content1$status_url
#'   response_url <- if (!is.null(links$response$href)) links$response$href else content1$response_url
#'   if (is.null(status_url) || is.null(response_url)) {
#'     print(content1)
#'     stop("Could not find status_url or response_url in FAL response.")
#'   }
#'
#'   # 4) polling
#'   status <- content1$status
#'   while (status %in% c("IN_QUEUE", "IN_PROGRESS")) {
#'     Sys.sleep(3)
#'     st <- httr::GET(status_url, headers)
#'     if (httr::status_code(st) >= 400) {
#'       txt <- httr::content(st, "text", encoding = "UTF-8")
#'       stop(sprintf("FAL status error (%s): %s",
#'                    httr::status_code(st), txt))
#'     }
#'     stc    <- httr::content(st, as = "parsed", simplifyVector = TRUE)
#'     status <- stc$status
#'     if (status %in% c("FAILED", "CANCELLED")) {
#'       msg <- if (!is.null(stc$error)) stc$error else status
#'       stop("FAL generation failed: ", msg)
#'     }
#'   }
#'   if (status != "COMPLETED") {
#'     stop("FAL generation did not complete. Final status: ", status)
#'   }
#'
#'   # 5) collect the result
#'   rr <- httr::GET(response_url, headers)
#'   if (httr::status_code(rr) >= 400) {
#'     txt <- httr::content(rr, "text", encoding = "UTF-8")
#'     stop(sprintf("FAL result error (%s): %s",
#'                  httr::status_code(rr), txt))
#'   }
#'   content2 <- httr::content(rr, as = "parsed", simplifyVector = TRUE)
#'
#'   # 6) extract audio URL
#'   audio_url <- NULL
#'   if (!is.null(content2$audio$url)) {
#'     audio_url <- content2$audio$url
#'   } else if (!is.null(content2$audios) &&
#'              length(content2$audios) > 0 &&
#'              !is.null(content2$audios[[1]]$url)) {
#'     audio_url <- content2$audios[[1]]$url
#'   } else if (!is.null(content2$file$url)) {
#'     audio_url <- content2$file$url
#'   } else {
#'     cand <- unlist(content2, use.names = FALSE)
#'     wavs <- grep("\\.(wav|mp3)", cand, value = TRUE)
#'     if (length(wavs) > 0) audio_url <- wavs[1]
#'   }
#'   if (is.null(audio_url)) {
#'     stop("Audio URL not found in FAL response.")
#'   }
#'
#'   # 7) download to temporary file
#'   tmp_aud <- tempfile(fileext = ".wav")
#'   dl <- httr::GET(
#'     audio_url,
#'     httr::write_disk(tmp_aud, overwrite = TRUE),
#'     httr::timeout(120)
#'   )
#'   if (httr::status_code(dl) != 200 ||
#'       !file.exists(tmp_aud) ||
#'       file.info(tmp_aud)$size == 0) {
#'     stop("Failed to download FAL audio: status ", httr::status_code(dl))
#'   }
#'
#'   return(tmp_aud)
#' }
#'
#' #' Find the last frame index of a video (internal)
#' #'
#' #' Uses ffprobe to count frames and return the last index.
#' #'
#' #' @keywords internal
#' #' @noRd
#' .find_last_frame <- function(video) {
#'   # 1) check file existence
#'   if (!file.exists(video)) {
#'     stop("Video file not found: ", video)
#'   }
#'   # 2) locate ffprobe
#'   ffprobe <- Sys.which("ffprobe")
#'   if (ffprobe == "") {
#'     stop("Could not find 'ffprobe' in your PATH. Install FFmpeg and ensure 'ffprobe' is available.")
#'   }
#'   # 3) build and run the command
#'   args <- c(
#'     "-v", "error",
#'     "-count_frames",
#'     "-select_streams", "v:0",
#'     "-show_entries", "stream=nb_read_frames",
#'     "-of", "default=nokey=1:noprint_wrappers=1",
#'     video
#'   )
#'   saida <- tryCatch({
#'     system2(ffprobe, args, stdout = TRUE, stderr = TRUE)
#'   }, warning = function(w) w, error = function(e) e)
#'
#'   # 4) validate output
#'   if (!is.character(saida) || length(saida) < 1) {
#'     stop("Could not get the number of frames via ffprobe.")
#'   }
#'   n <- as.integer(saida[1])
#'   if (is.na(n)) {
#'     stop("Failed to convert number of frames: '", saida[1], "'.")
#'   }
#'   # 5) print and return
#'   message("Last frame (total frames): ", n)
#'   return(n)
#' }
#'
#' #' Save a still frame from a video
#' #'
#' #' Extracts a specific frame from a video file (using ffmpeg/ffprobe) and saves
#' #' it as an image. When `frame` is NULL, uses the last frame.
#' #'
#' #' @param video Path to the video file.
#' #' @param directory Output directory to save the still image.
#' #' @param frame Optional frame index (1-based in common tools). If NULL, extract the last frame.
#' #' @return Invisibly returns a list with `saved_file`, `frame_extraido`, `status_api`, and `duration`.
#' #'
#' #' @examples
#' #' # .save_video_still("/path/video.mp4", directory = tempdir())
#' #'
#' #' @noRd
#' .save_video_still <- function(
#'     video,
#'     directory = "content",
#'     frame     = NULL
#' ) {
#'   t_start <- Sys.time()
#'
#'   # 1) initial validations
#'   if (!is.character(video) || length(video) != 1 || !nzchar(video)) {
#'     stop("???video??? must be a non-empty path.")
#'   }
#'   if (!file.exists(video)) {
#'     stop("Video not found: ", video)
#'   }
#'   video <- normalizePath(video, winslash = "/", mustWork = TRUE)
#'
#'   # prepare output
#'   if (is.null(directory)) directory <- "./"
#'   dir.create(directory, showWarnings = FALSE, recursive = TRUE)
#'   directory <- path.expand(directory)
#'
#'   # locate ffprobe and ffmpeg
#'   ffprobe <- Sys.which("ffprobe")
#'   ffmpeg  <- Sys.which("ffmpeg")
#'   if (ffprobe == "") stop("Could not find 'ffprobe' in PATH.")
#'   if (ffmpeg  == "") stop("Could not find 'ffmpeg' in PATH.")
#'
#'   # 2) helper functions
#'   get_duration <- function(f) {
#'     args <- c("-v","error",
#'               "-show_entries","format=duration",
#'               "-of","default=noprint_wrappers=1:nokey=1",
#'               f)
#'     out <- system2(ffprobe, args, stdout = TRUE, stderr = NULL)
#'     as.numeric(out[1])
#'   }
#'   count_frames <- function(f) {
#'     args <- c("-v","error",
#'               "-count_frames",
#'               "-select_streams","v:0",
#'               "-show_entries","stream=nb_read_frames",
#'               "-of","default=nokey=1:noprint_wrappers=1",
#'               f)
#'     out <- system2(ffprobe, args, stdout = TRUE, stderr = NULL)
#'     as.integer(out[1])
#'   }
#'   get_fps <- function(f) {
#'     args <- c("-v","error",
#'               "-select_streams","v:0",
#'               "-show_entries","stream=avg_frame_rate",
#'               "-of","csv=p=0",
#'               f)
#'     out <- system2(ffprobe, args, stdout = TRUE, stderr = NULL)
#'     # typically returns "30/1" or "24000/1001"
#'     fr <- strsplit(out[1], "/")[[1]]
#'     as.numeric(fr[1]) / as.numeric(fr[2])
#'   }
#'
#'   # 3) get values
#'   dur     <- get_duration(video)
#'   nframes <- count_frames(video)
#'   if (is.na(dur) || dur <= 0)  stop("Could not obtain video duration.")
#'   if (is.na(nframes) || nframes < 1) stop("Could not count video frames.")
#'
#'   last_index <- nframes - 1L
#'   fps_ff     <- tryCatch(get_fps(video), error = function(e) NA_real_)
#'   fps        <- if (!is.na(fps_ff) && fps_ff > 0) fps_ff else (nframes / dur)
#'
#'   # 4) choose the frame
#'   if (is.null(frame)) {
#'     frame_use <- last_index
#'   } else {
#'     frame_is <- suppressWarnings(as.integer(frame))
#'     frame_use <- frame_is - 2
#'     if (is.na(frame_use) || frame_use < 0L || frame_use > last_index) {
#'       stop("???frame??? invalid: must be between 0 and ", last_index)
#'     }
#'   }
#'
#'   # 5) convert frame -> timestamp "HH:MM:SS.xxx"
#'   time_sec <- frame_use / fps
#'   hh <- floor(time_sec / 3600)
#'   mm <- floor((time_sec - hh*3600) / 60)
#'   ss <- time_sec - hh*3600 - mm*60
#'   timestamp <- sprintf("%02d:%02d:%06.3f", hh, mm, ss)
#'
#'   # 6) build output and extract the frame
#'   base     <- tools::file_path_sans_ext(basename(video))
#'   nome_img <- sprintf("%s_frame_%04d.jpg", base, frame_use)
#'   dpath  <- file.path(directory, nome_img)
#'
#'   # -ss after -i ensures frame-accurate extraction
#'   args2 <- c(
#'     "-i", video,
#'     "-ss", timestamp,
#'     "-frames:v", "1",
#'     "-q:v", "2",
#'     "-y", dpath
#'   )
#'   res <- system2(ffmpeg, args2, stdout = TRUE, stderr = TRUE)
#'   if (!file.exists(dpath) || file.info(dpath)$size == 0) {
#'     stop("Failed to extract frame:\n", paste(res, collapse = "\n"))
#'   }
#'
#'   # 7) return list in the format you want
#'   t_elapsed <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))
#'   resultado <- list(
#'     response_value  = dpath,
#'     frame_extraido  = frame_use,
#'     duration           = t_elapsed,
#'     status_api      = "SUCCESS",
#'     status_msg = "OK",
#'     saved_file   = dpath
#'   )
#'   invisible(resultado)
#' }
#'
#' #' Merge scene elements (audio/video)
#' #'
#' #' Combines one or more videos and/or audio files into a single output. If
#' #' multiple videos are provided, concatenates them. If multiple audio files are
#' #' provided, mixes them to the longest duration.
#' #'
#' #' @param elementos List of result objects or lists containing `$saved_file`.
#' #' @param directory Output directory for the combined media.
#' #' @return Invisibly returns a list with `saved_file`, `status_api`, and `duration`.
#' #'
#' #' @examples
#' #' # .unir_cena(list(list(saved_file = "/path/a.mp4"), list(saved_file = "/path/b.mp3")))
#' #'
#' #' @noRd
#' .unir_cena <- function(elementos, directory = "content") {
#'   # helper to get duration (in seconds) of any media via ffprobe
#'   get_duration <- function(f) {
#'     out <- tryCatch(
#'       system2("ffprobe",
#'               c("-v","error",
#'                 "-show_entries","format=duration",
#'                 "-of","default=noprint_wrappers=1:nokey=1", f),
#'               stdout = TRUE, stderr = NULL),
#'       error = function(e) NA_character_
#'     )
#'     as.numeric(out[1])
#'   }
#'
#'   start_all <- Sys.time()
#'
#'   # 1) prepare directory
#'   if (is.null(directory)) directory <- "./"
#'   directory <- path.expand(directory)
#'   if (!dir.exists(directory)) dir.create(directory, recursive = TRUE)
#'
#'   # 2) normalize input: list of objects becomes a list of 1 group
#'   is_elem_com_arquivo <- function(x) is.list(x) && !is.null(x$saved_file)
#'   if (is.list(elementos) &&
#'       length(elementos)>0 &&
#'       all(vapply(elementos, is_elem_com_arquivo, logical(1)))) {
#'     elementos <- list(elementos)
#'   }
#'   if (!is.list(elementos) || length(elementos)==0) {
#'     stop("`elementos` must be a list of lists or a list of objects with $saved_file.")
#'   }
#'
#'   # 3) process each group separately
#'   results <- lapply(elementos, function(group) {
#'     t0 <- Sys.time()
#'
#'     # 3.1) extract paths
#'     paths <- vapply(group, function(el) {
#'       if (is.list(el) && !is.null(el$saved_file)) {
#'         el$saved_file
#'       } else {
#'         stop("Each item must be a list with field 'saved_file'.")
#'       }
#'     }, FUN.VALUE = "")
#'
#'     miss <- paths[!file.exists(paths)]
#'     if (length(miss)) stop("File(s) not found: ",
#'                            paste(miss, collapse = ", "))
#'
#'     # 3.2) measure duration of each input
#'     durs    <- vapply(paths, get_duration, numeric(1))
#'     max_dur <- max(durs, na.rm = TRUE)
#'
#'     # 3.3) classify video vs audio
#'     mtypes      <- vapply(paths, .get_mime_type, FUN.VALUE = "")
#'     is_video    <- grepl("^video/", mtypes)
#'     # extension-based fallback
#'     if (!any(is_video)) {
#'       ext0     <- tolower(tools::file_ext(paths))
#'       is_video <- ext0 %in% c("mp4","mov","avi","mkv","webm")
#'     }
#'
#'     video_files <- paths[is_video]
#'     audio_files <- paths[!is_video]
#'
#'     # 3.4) if there are 2+ videos, concatenate them in a single order
#'     video_agg   <- NULL
#'     tmp_concat  <- NULL
#'     listfile    <- NULL
#'     if (length(video_files) > 1) {
#'       # build list file
#'       listfile <- tempfile(fileext = ".txt")
#'       linhas   <- sprintf("file '%s'",
#'                           normalizePath(video_files, winslash = "/", mustWork = TRUE))
#'       writeLines(linhas, listfile)
#'
#'       tmp_concat <- tempfile(fileext = ".mp4")
#'       args_concat <- c("-f","concat","-safe","0",
#'                        "-i", listfile,
#'                        "-c","copy",
#'                        "-y", tmp_concat)
#'       rc <- system2("ffmpeg", args_concat, stdout = TRUE, stderr = TRUE)
#'       if (!file.exists(tmp_concat)) {
#'         stop("Failed to concatenate videos:\n", paste(rc, collapse = "\n"))
#'       }
#'       video_agg <- tmp_concat
#'
#'     } else if (length(video_files)==1) {
#'       video_agg <- video_files
#'     }
#'
#'     # 3.5) mix audios (duration = longest)
#'     tmp_mix <- NULL
#'     if (length(audio_files)==1) {
#'       tmp_mix <- audio_files
#'     } else if (length(audio_files) > 1) {
#'       tmp_mix <- tempfile(fileext = ".wav")
#'       ins   <- unlist(lapply(audio_files, function(f) c("-i", f)))
#'       filt  <- sprintf("amix=inputs=%d:duration=longest:dropout_transition=0",
#'                        length(audio_files))
#'       args <- c(ins,
#'                 "-filter_complex", filt,
#'                 "-c:a","pcm_s16le",
#'                 "-y", tmp_mix)
#'       rc2 <- system2("ffmpeg", args, stdout = TRUE, stderr = TRUE)
#'       if (!file.exists(tmp_mix)) {
#'         stop("Failed to mix audios:\n", paste(rc2, collapse = "\n"))
#'       }
#'     }
#'
#'     # 3.6) if aggregated video is shorter than max_dur, extend with tpad
#'     video_for_merge <- video_agg
#'     tmp_ext         <- NULL
#'     if (!is.null(video_agg)) {
#'       dur_vid <- durs[paths %in% video_files][1]
#'       diff    <- max_dur - dur_vid
#'       if (!is.na(diff) && diff > 0.05) {
#'         tmp_ext <- tempfile(fileext = ".mp4")
#'         args_e <- c(
#'           "-i", video_agg,
#'           "-vf", sprintf("tpad=stop_mode=clone:stop_duration=%f", diff),
#'           "-c:v","libx264","-preset","veryfast","-crf","23",
#'           "-an","-y", tmp_ext
#'         )
#'         rc_e <- system2("ffmpeg", args_e, stdout = TRUE, stderr = TRUE)
#'         if (!file.exists(tmp_ext)) {
#'           stop("Failed to extend the video:\n", paste(rc_e, collapse = "\n"))
#'         }
#'         video_for_merge <- tmp_ext
#'       }
#'     }
#'
#'     # 3.7) build final output in temp
#'     final_tmp <- NA_character_
#'     if (!is.null(video_for_merge)) {
#'       # video + audio
#'       if (length(audio_files)==0) {
#'         # video only
#'         extv     <- tools::file_ext(video_for_merge)
#'         final_tmp <- tempfile(fileext = paste0(".", extv))
#'         file.copy(video_for_merge, final_tmp, overwrite = TRUE) ||
#'           stop("Failed to copy video to temp.")
#'       } else {
#'         final_tmp <- tempfile(fileext = ".mp4")
#'         args_m <- c(
#'           "-i", video_for_merge,
#'           "-i", tmp_mix,
#'           "-c:v","copy",
#'           "-c:a","aac",
#'           "-map","0:v:0",
#'           "-map","1:a:0",
#'           "-y", final_tmp
#'         )
#'         rc3 <- system2("ffmpeg", args_m, stdout = TRUE, stderr = TRUE)
#'         if (!file.exists(final_tmp)) {
#'           stop("Failed to generate final video:\n", paste(rc3, collapse = "\n"))
#'         }
#'       }
#'     } else {
#'       # audio only
#'       if (length(audio_files)==0) {
#'         stop("There is neither video nor audio in this group.")
#'       }
#'       final_tmp <- tempfile(fileext = ".wav")
#'       file.copy(tmp_mix, final_tmp, overwrite = TRUE) ||
#'         stop("Failed to copy audio to temp.")
#'     }
#'
#'     # 3.8) cleanup of temporary files
#'     if (!is.null(tmp_mix) && tmp_mix != final_tmp && file.exists(tmp_mix)) {
#'       file.remove(tmp_mix)
#'     }
#'     if (!is.null(tmp_ext) && tmp_ext != final_tmp && file.exists(tmp_ext)) {
#'       file.remove(tmp_ext)
#'     }
#'     if (!is.null(tmp_concat) && tmp_concat != final_tmp && file.exists(tmp_concat)) {
#'       file.remove(tmp_concat)
#'     }
#'     if (!is.null(listfile) && file.exists(listfile)) {
#'       file.remove(listfile)
#'     }
#'
#'     # 3.9) build label and copy to directory
#'     labels <- vapply(group, function(el) {
#'       if (!is.null(el$label_cat)) el$label_cat else {
#'         if (!is.null(el$label)) gsub("[^0-9A-Za-z_.-]", "_", el$label)
#'         else tools::file_path_sans_ext(basename(el$saved_file))
#'       }
#'     }, FUN.VALUE = "")
#'
#'     if (!is.null(video_agg)) {
#'       is_vid_agg <- paths %in% video_files
#'       label_use  <- labels[which(is_vid_agg)[1]]
#'     } else {
#'       label_use  <- labels[1]
#'     }
#'     label_use <- substr(label_use, 1, 36)
#'
#'     ext_final <- tolower(tools::file_ext(final_tmp))
#'     nome_final <- paste0(label_use, ".", ext_final)
#'     dpath    <- file.path(directory, nome_final)
#'
#'     file.copy(final_tmp, dpath, overwrite = TRUE) ||
#'       stop("Failed to copy final to ", dpath)
#'     if (dpath != final_tmp && file.exists(final_tmp)) {
#'       file.remove(final_tmp)
#'     }
#'
#'     # 3.10) return
#'     tgroup <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
#'     list(
#'       response_value  = dpath,
#'       duration           = tgroup,
#'       status_api      = "SUCCESS",
#'       status_msg = "OK",
#'       saved_file   = dpath
#'     )
#'   })
#'
#'   # 4) unwrap if only one group
#'   if (length(results)==1) {
#'     invisible(results[[1]])
#'   } else {
#'     invisible(results)
#'   }
#' }
