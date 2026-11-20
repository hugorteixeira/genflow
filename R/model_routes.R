# A route describes one operation. Input names are Genflow arguments; wire
# field names stay inside the corresponding adapter.
.gen_route <- function(operation, inputs, output, required=character(), transport=NULL,
                       source=NULL, revision=NULL, limits=list()) {
  list(operation=operation, inputs=as.list(inputs), required=as.list(required),
       output=output, transport=transport, source=source, revision=revision, limits=limits)
}

.gen_video_routes <- function(service, model) {
  make <- function(inputs=character(), required=character(), limits=list())
    .gen_route("video.generate", inputs, "video", required, service, model, limits=limits)
  if (service=="fal" && model %in% c("fal-ai/kling-video/ai-avatar/v2/standard","fal-ai/kling-video/ai-avatar/v2/pro"))
    return(list(make(c("image","audio"),c("image","audio"),list(max_seconds=300))))
  if (service=="replicate" && model=="wan-video/wan-2.2-s2v")
    return(list(make(c("image","audio"),c("image","audio"),list(max_seconds=60))))
  if (service=="replicate" && model %in% c("google/veo-3.1","google/veo-3.1-fast"))
    return(list(make("image",limits=list(wire=list(image="image")))))
  if (service=="openrouter") {
    if (model %in% c("bytedance/seedance-2.0","bytedance/seedance-2.5-flash"))
      return(list(make("image"),make(c("reference_clip","reference_sound"),"reference_clip")))
    if (model %in% c("google/veo-3.1","google/veo-3.1-fast"))
      return(list(make("image")))
    if (model=="black-forest-labs/flux-video-edit") return(list(make("source_video","source_video",list(max_seconds=15,max_bytes=50*1024^2))))
  }
  if (service=="gemini" && model=="gemini-omni-1.2-flash")
    return(list(make("image"), make("source_video","source_video")))
  list()
}

.gen_reviewed_routes <- function(service,model) {
  if (!.gen_scalar(model)) return(list())
  if(service=="replicate" && model=="openai/whisper")
    return(list(.gen_route("audio.transcribe","audio","text","audio",service,"https://replicate.com/openai/whisper/api/schema")))
  if(service=="replicate" && model=="qwen/qwen3-tts")
    return(list(.gen_route("audio.speak",character(),"audio",transport=service,source="https://replicate.com/qwen/qwen3-tts/api/schema")))
  if((service=="replicate" && model=="black-forest-labs/flux-schnell") ||
     (service=="fal" && model %in% c("fal-ai/flux/schnell","fal-ai/flux-2/edit"))) {
    edit <- model=="fal-ai/flux-2/edit"
    return(list(.gen_route("image.generate",if(edit) "add_img","image",if(edit) "add_img",service,model,limits=list(max_images=if(edit) 4 else 0))))
  }
  video <- .gen_video_routes(service,model)
  if (length(video)) return(video)
  if (service=="gemini") {
    source <- paste0("https://aistudio.google.com/docs/models/", model)
    if (model %in% c("gemini-3.1-pro-preview","gemini-3-flash-preview","gemini-3.5-flash","gemini-3.8-flash","gemini-3.6-flash","gemini-2.5-pro","gemini-2.5-flash","gemini-2.5-flash-lite"))
      return(list(.gen_route("text.generate",c("add_img","context_video","context_audio","pdf"),"text",transport="gemini",source=source)))
    if (model %in% c("gemini-3.1-flash-image","gemini-nano-banana-2.1","gemini-2.5-flash-image"))
      return(list(.gen_route("image.generate","add_img","image",transport=service,source=source,limits=list(max_images=1L))))
    if (model %in% c("gemini-3.8-flash-tts","gemini-3.8-flash-lite-tts"))
      return(list(.gen_route("audio.speak","instructions","audio",transport=service,source=source,limits=list(voices=list("Kore","Puck","Aoede","Charon","Fenrir","Leda","Orus","Zephyr")))))
    if (model %in% c("lyria-3-pro-preview","lyria-3.5","lyria-3-clip-preview"))
      return(list(.gen_route("audio.music","lyrics","audio",transport=service,source=source)))
  }
  if (service=="openrouter" && model=="google/gemini-3.1-flash-image-preview")
    return(list(.gen_route("image.generate","add_img","image",transport=service,source="https://openrouter.ai/api/v1/images/models",limits=list(max_images=1L))))
  if (service=="openrouter" && model=="mistralai/voxtral-mini-tts-2603")
    return(list(.gen_route("audio.speak",character(),"audio",transport=service,source="https://openrouter.ai/docs/guides/overview/multimodal/tts",limits=list(voices=list("en_paul_neutral"),wire_format="mp3"))))
  if (service=="openrouter" && model %in% c("google/lyria-3-clip-preview","google/lyria-3-pro-preview"))
    return(list(.gen_route("audio.music","lyrics","audio",transport=service,source="https://openrouter.ai/collections/audio-models")))
  list()
}

.gen_catalog_get <- function(url,service,query=NULL) {
  header <- switch(service,replicate=httr::add_headers(Authorization=paste("Bearer",Sys.getenv("REPLICATE_API_TOKEN"))),
    fal=httr::add_headers(Authorization=paste("Key",.genflow_first_env(c("FAL_API_KEY","FAL_KEY")))), httr::accept_json())
  # httr removes an existing pagination/filter query when passed query=NULL.
  args <- list("GET",url,header,httr::timeout(60),httr::config(followlocation=FALSE),
               times=3,pause_cap=10,terminate_on=c(400L,401L,403L,404L),quiet=TRUE)
  if(length(query)) args$query <- query
  r <- do.call(httr::RETRY,args)
  .gen_provider_check(r,expected=200L,stage="management")
  jsonlite::fromJSON(httr::content(r,as="text",encoding="UTF-8"),simplifyVector=FALSE)
}

.gen_catalog_row <- function(service,id,type,description,routes,pricing="") {
  if (!.gen_scalar(id) || !nzchar(id)) stop("Missing model identifier.")
  data.frame(service=service,model=id,type=type,pricing=pricing,description=description %||% "",
    routes=as.character(jsonlite::toJSON(unname(routes),auto_unbox=TRUE,null="null")),stringsAsFactors=FALSE)
}

.gen_openrouter_row <- function(x,kind) {
  id <- x$id; reviewed <- .gen_reviewed_routes("openrouter",id)
  source <- paste0("https://openrouter.ai/api/v1/",if(kind=="speech") "models?output_modalities=speech" else kind)
  input <- unlist(x$architecture$input_modalities)
  output <- unlist(x$architecture$output_modalities)
  routes <- list()
  if (kind=="speech") {
    routes <- list(.gen_route("audio.speak",if(startsWith(id,"google/")) "instructions","audio",transport=if(length(x$supported_voices) && "/api/v1/audio/speech" %in% unlist(x$endpoints) && !grepl("dialogue",id,fixed=TRUE)) "openrouter",source=source,revision=x$canonical_slug,
      limits=list(voices=x$supported_voices,wire_format=if(startsWith(id,"google/") || startsWith(id,"heygen/")) "pcm" else "mp3",sample_rate=if(startsWith(id,"heygen/")) 44100 else 24000)))
    type <- "TTS"
  } else if (kind=="models") {
    roles <- c(image="add_img",audio="context_audio",video="context_video",file="pdf")
    if ("text" %in% input && "text" %in% output && !any(vapply(reviewed,function(r) identical(r$operation,"audio.music"),logical(1)))) routes <- list(.gen_route("text.generate",unname(roles[intersect(names(roles),input)]),
      "text",transport="openrouter",source=source,revision=x$canonical_slug))
    if ("audio" %in% output) routes <- c(routes,Filter(function(r) identical(r$operation,"audio.music"),reviewed))
    type <- if ("audio.music" %in% vapply(reviewed,`[[`,"","operation")) "Music" else if ("text" %in% output) "Chat" else "Unknown"
  } else if (kind=="images/models") {
    parameters <- x$supported_parameters
    min_images <- parameters$input_references$min %||% 0L
    max_images <- parameters$input_references$max %||% 0L
    formats <- unlist(parameters$output_format$values)
    raster <- intersect(c("png","jpeg","webp","jpg"),formats)
    routes <- list(.gen_route("image.generate",if(max_images>0) "add_img","image",if(min_images>0) "add_img",
      transport=if(!length(formats) || length(raster)) "openrouter",source=source,
      limits=list(min_images=min_images,max_images=max_images,output_format=if(length(raster)) raster[[1]],parameters=parameters)))
    type <- "Image"
  } else {
    routes <- Filter(function(r) identical(r$operation,"video.generate"),reviewed)
    if(!length(routes) && is.null(x$upscale_factor) && "first_frame" %in% unlist(x$supported_frame_images))
      routes <- list(.gen_route("video.generate","image","video","image","openrouter",source))
    if (!length(routes)) routes <- list(.gen_route(if(!is.null(x$upscale_factor)) "video.upscale" else "video.unknown",
      character(),"video",source=source))
    for(i in seq_along(routes)) {
      routes[[i]]$revision <- x$canonical_slug
      routes[[i]]$limits <- c(routes[[i]]$limits,x[intersect(names(x),c("supported_frame_images","supported_durations","generate_audio","upscale_factor"))])
    }
    type <- "Video"
  }
  price <- x$pricing
  pricing <- if (!is.null(price$prompt) && !is.null(price$completion)) paste(1e6*as.numeric(price$prompt),1e6*as.numeric(price$completion),sep="/") else ""
  .gen_catalog_row("openrouter",id,type,x$description,routes,pricing)
}

.gen_catalog_write <- function(rows,service,directory) {
  if (!length(rows)) stop("Provider returned an empty catalog.")
  out <- do.call(rbind,rows)
  dir.create(directory,recursive=TRUE,showWarnings=FALSE)
  utils::write.csv(out,file.path(directory,paste0(service,".csv")),row.names=FALSE,na="",fileEncoding="UTF-8")
  invisible(out)
}

.gen_model_index_cache <- new.env(parent=emptyenv())
.gen_model_index <- function(service,directory=NULL) {
  if (!service %in% c("openrouter","replicate","fal","gemini")) return(list())
  path <- file.path(.genflow_resolve_models_dir(directory),paste0(service,".csv"))
  stat <- file.info(path)
  if (is.na(stat$size)) return(list())
  stamp <- c(as.numeric(stat$mtime),as.numeric(stat$ctime),stat$size)
  cached <- .gen_model_index_cache[[service]]
  if (identical(cached$stamp,stamp) && identical(cached$path,path)) return(cached$index)
  df <- utils::read.csv(path,stringsAsFactors=FALSE,check.names=FALSE)
  index <- new.env(hash=TRUE,parent=emptyenv())
  if ("routes" %in% names(df)) for(i in seq_len(nrow(df))) {
    id <- df$model[i]
    if (nzchar(id) && !is.na(df$routes[i]) && nzchar(df$routes[i]))
      index[[id]] <- c(index[[id]],jsonlite::fromJSON(df$routes[i],simplifyVector=FALSE))
  }
  .gen_model_index_cache[[service]] <- list(path=path,stamp=stamp,index=index)
  index
}

#' Read documented operations and input roles for a model
#' @param service Provider identifier.
#' @param model Provider model identifier.
#' @param directory Optional model catalog directory.
#' @return List of routes with their input roles, restrictions and source.
#' @export
gen_model_routes <- function(service,model,directory=NULL) {
  if (!.gen_scalar(service) || !.gen_scalar(model) || !nzchar(model)) return(list())
  frozen <- getOption("genflow.model_routes")
  if(identical(frozen$service,service) && identical(frozen$model,model)) return(frozen$routes)
  routes <- .gen_model_index(service,directory)[[model]]
  .gen_effective_routes(service,model,routes,directory,service %in% c("fal","replicate"))
}

.gen_effective_routes <- function(service,model,routes,directory,has_details) {
  if(has_details) {
    path <- .gen_route_file(service,model,directory)
    if(file.exists(path)) {
      catalog <- file.path(.genflow_resolve_models_dir(directory),paste0(service,".csv"))
      if(!file.exists(catalog) || file.mtime(path)>=file.mtime(catalog))
        return(jsonlite::read_json(path,simplifyVector=FALSE))
    }
  }
  if(length(routes)) routes else .gen_reviewed_routes(service,model)
}

#' Match the actual piece inputs against documented model routes
#' @inheritParams gen_model_routes
#' @param operation Studio operation ID.
#' @param inputs Connected media arguments.
#' @param complete Require every mandatory input (execution rather than editing).
#' @return A list with compatible and reason fields.
#' @export
gen_model_match <- function(service,model,operation,inputs=character(),complete=TRUE,directory=NULL) {
  routes <- gen_model_routes(service,model,directory)
  .gen_match_routes(routes,operation,inputs,complete)
}

.gen_match_routes <- function(routes,operation,inputs,complete) {
  matching <- Filter(function(x) identical(x$operation,operation) && !is.null(x$transport),routes)
  labels <- c(add_img="imagem",image="imagem da cena",audio="fala",source_video="vídeo para editar",
    reference_clip="vídeo de referência",reference_sound="áudio de referência",reference_audio="referência de voz",
    context_video="vídeo para análise",context_audio="áudio para análise",pdf="PDF",lyrics="letra",instructions="instruções")
  describe <- function(keys) paste(ifelse(keys %in% names(labels),unname(labels[keys]),keys),collapse=", ")
  reason <- "Sem integração confirmada para esta operação"
  pending <- NULL
  for (route in matching) {
    extra <- setdiff(inputs,unlist(route$inputs)); missing <- setdiff(unlist(route$required),inputs)
    if (!length(extra) && !length(missing)) return(list(compatible=TRUE,reason=NULL,missing=list()))
    if(!length(extra)) {
      reason <- paste("Requer",describe(missing))
      if(is.null(pending) || length(missing)<length(pending$missing))
        pending <- list(compatible=!complete,reason=reason,missing=as.list(missing))
    } else reason <- paste("Entrada sem suporte:",describe(extra))
  }
  pending %||% list(compatible=FALSE,reason=reason,missing=list())
}

#' List models with an authoring decision for the current piece
#' @inheritParams gen_model_match
#' @return Catalog data frame with model compatibility and reason columns.
#' @export
gen_model_choices <- function(service,operation,inputs=character(),directory=NULL) {
  if(!.gen_scalar(service)) stop("Select a provider.")
  df <- gen_models(service,directory)
  if (!service %in% c("openrouter","gemini","replicate","fal")) return(df)
  defaults <- gen_block_contracts()
  blocks <- Filter(function(b) identical(b$studio$id,operation),defaults)
  known <- if(length(blocks)) blocks[[1]]$arguments$properties$model[["x-default-by"]]$values[[service]]
  if (operation=="video.generate") {
    video <- gen_block_contracts("gen_video")$studio$models
    known <- unique(c(known,vapply(Filter(function(m) m$service==service,video),`[[`,"","model")))
  }
  if(operation=="text.generate" && service=="gemini") known <- c(known,.GEMINI_TEXT_DEFAULT_MODEL)
  if(operation=="audio.transcribe" && service=="replicate") known <- c(known,"openai/whisper")
  if(operation=="video.generate" && service=="replicate") known <- c(known,"google/veo-3.1","google/veo-3.1-fast")
  for(id in setdiff(unlist(known),df$model)) df <- rbind(df,data.frame(service=service, model=id,
    type="Unknown",pricing="",description="",source_file=""))
  df <- df[!duplicated(df$model),,drop=FALSE]
  index <- .gen_model_index(service,directory)
  schema_dir <- file.path(.genflow_resolve_models_dir(directory),"schemas")
  has_details <- service %in% c("fal","replicate") && dir.exists(schema_dir) && any(startsWith(list.files(schema_dir),paste0(service,"-")))
  df$compatible <- rep(NA,nrow(df)); df$reason <- rep("",nrow(df)); df$needs_inputs <- rep(FALSE,nrow(df))
  for(i in seq_len(nrow(df))) {
    id <- df$model[i]
    routes <- .gen_effective_routes(service,id,index[[id]],directory,has_details)
    decision <- .gen_match_routes(routes,operation,inputs,FALSE)
    df$compatible[i] <- decision$compatible; df$reason[i] <- decision$reason %||% ""
    df$needs_inputs[i] <- length(decision$missing)>0L
  }
  df
}
