.gen_text_attachments <- function(paths,service,timeout) {
  if(length(paths) && !service %in% c("openrouter","gemini")) stop("Unsupported text attachment transport.")
  lapply(names(paths),function(key) {
    path <- paths[[key]]; info <- file.info(path)
    if (is.na(info$size) || info$isdir || info$size<=0 || info$size>15*1024^2) stop("Inline attachment must be a file of at most 15 MiB.")
    if(key=="pdf") {
      if(!identical(readBin(path,"raw",5L),charToRaw("%PDF-"))) stop("Invalid PDF attachment.")
      mime <- "application/pdf"
    } else {
      meta <- .gen_video_probe(path,if(key=="context_video") "video" else "audio",timeout)
      mime <- meta$mime
      if(key=="context_video" && !grepl("mp4",meta$container,fixed=TRUE)) stop("Use an MP4 video attachment.")
    }
    data <- openssl::base64_encode(readBin(path,"raw",info$size))
    if(service=="gemini") return(list(inline_data=list(mime_type=mime,data=data)))
    uri <- paste0("data:",mime,";base64,",data)
    switch(key,pdf=list(type="file",file=list(filename=basename(path),file_data=uri)),
      context_video=list(type="video_url",video_url=list(url=uri)),
      context_audio={
        fmt <- switch(mime,"audio/mpeg"="mp3","audio/wav"="wav",stop("Use WAV or MP3 for OpenRouter audio input."))
        list(type="input_audio",input_audio=list(data=data,format=fmt))
      })
  })
}
