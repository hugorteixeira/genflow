# Bounded GGUF metadata inspection. Never reads tensors or loads a model.
# Unknown/truncated headers stay unknown; filenames are catalog hints only.
.gen_native_architecture <- function(path) {
  if (!file.exists(path) || dir.exists(path)) return("")
  tryCatch({
    con <- file(path,"rb"); on.exit(close(con),add=TRUE)
    limit <- min(file.info(path)$size, 8 * 1024^2)
    take <- function(n) {
      if (!is.finite(n) || n<0 || seek(con)+n>limit) stop("metadata limit")
      x <- readBin(con,"raw",n=n); if (length(x)!=n) stop("truncated GGUF")
      x
    }
    uint <- function(n) sum(as.integer(take(n))*256^(seq_len(n)-1L))
    string <- function() rawToChar(take(uint(8)))
    skip <- function(type,depth=0L) {
      widths <- c(`0`=1,`1`=1,`2`=2,`3`=2,`4`=4,`5`=4,`6`=4,`7`=1,`10`=8,`11`=8,`12`=8)
      if (type==8) { take(uint(8)); return(invisible(NULL)) }
      if (type==9 && depth==0L) {
        subtype <- uint(4); n <- uint(8)
        if (n>100000) stop("metadata limit")
        for (i in seq_len(n)) skip(subtype,1L)
        return(invisible(NULL))
      }
      n <- widths[as.character(type)]
      if (is.na(n)) stop("unknown metadata type")
      take(n); invisible(NULL)
    }
    if (!identical(rawToChar(take(4)),"GGUF") || !uint(4) %in% c(2,3)) return("")
    uint(8); count <- uint(8)
    if (count>100000) return("")
    for (i in seq_len(count)) {
      key <- string(); type <- uint(4)
      if (key=="general.architecture") return(if (type==8) tolower(string()) else "")
      skip(type)
    }
    ""
  },error=function(e) "")
}

.gen_native_model_type <- function(model) {
  architecture <- .gen_native_architecture(path.expand(model))
  if (nzchar(architecture)) {
    if (architecture=="voxcpm2") return("TTS")
    hint <- architecture
  } else hint <- tolower(basename(model))
  if (grepl("voxcpm2",hint,fixed=TRUE)) return("TTS")
  if (grepl("ark[-_]?asr|funasr|whisper|ggml-(tiny|base|small|medium|large)|granite[-_]?speech|granite[-_]?nle|moss[-_](transcribe|diarize)|cohere|parakeet|canary|voxtral|qwen3[-_]?asr|sensevoice|moonshine|gigaam",hint,perl=TRUE)) return("STT")
  "Unknown"
}
