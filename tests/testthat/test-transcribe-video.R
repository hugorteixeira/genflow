transcribe_fixture <- function(extension = "mp4", sound = TRUE) {
  skip_if(!nzchar(Sys.which("ffmpeg")), "ffmpeg required")
  dir <- tempfile("transcribe media "); dir.create(dir)
  path <- file.path(dir, paste0("video with spaces.", extension))
  args <- c("-nostdin", "-y", "-loglevel", "error", "-f", "lavfi", "-i", "color=size=32x32:rate=10")
  if (sound) args <- c(args, "-f", "lavfi", "-i", "sine=frequency=440:sample_rate=44100")
  args <- c(args, "-t", "0.3", "-c:v", if(extension == "webm") "libvpx-vp9" else "mpeg4")
  if (sound) args <- c(args,"-c:a", if(extension == "webm") "libopus" else "aac")
  genflow:::.stt_chunk_run_ffmpeg(c(args,path))
  path
}

test_that("the transcribe contract accepts audio/video and rejects other contents", {
  expect_identical(gen_block_contracts("gen_stt")$inputs$audio,"asset:audio|asset:video")
  args <- list(audio=list(input="media"))
  for (kind in c("audio","video","text","image","json")) {
    result <- gen_block_ports("transcribe",args,list(media=paste0("asset:",kind)))
    expect_identical(result$valid,kind %in% c("audio","video"),info=kind)
  }
})

test_that("video soundtracks are extracted without altering the original", {
  for (extension in c("mp4","webm","mkv","mov")) {
    path <- transcribe_fixture(extension); on.exit(unlink(dirname(path),recursive=TRUE),add=TRUE)
    before <- tools::md5sum(path)
    prepared <- genflow:::.stt_prepare_audio(path)
    on.exit(unlink(prepared$tmp),add=TRUE)
    expect_identical(prepared$ext,"wav")
    expect_true(file.exists(prepared$path))
    probe <- system2(Sys.which("ffprobe"), c("-v","error","-show_streams","-of","json",shQuote(prepared$path)),stdout=TRUE)
    streams <- jsonlite::fromJSON(paste(probe,collapse="\n"))$streams
    expect_equal(nrow(streams),1L)
    expect_identical(streams$codec_type,"audio")
    expect_identical(streams$sample_rate,"16000")
    expect_equal(streams$channels,1L)
    expect_equal(unname(tools::md5sum(path)),unname(before))
    expect_error(genflow:::.stt_prepare_audio(path,convert=FALSE),"convert = TRUE")
    audio <- genflow:::.stt_prepare_audio(prepared$path)
    expect_identical(audio$path,prepared$path)
    expect_null(audio$tmp)
    unlink(c(path,prepared$tmp))
  }
})

test_that("gen_stt sends only extracted audio and cleans it after the call", {
  path <- transcribe_fixture(); on.exit(unlink(dirname(path),recursive=TRUE),add=TRUE)
  seen <- NULL
  local_mocked_bindings(.stt_local_native=function(audio_path,...) {
    seen <<- audio_path
    expect_true(file.exists(audio_path))
    expect_identical(tools::file_ext(audio_path),"wav")
    list(text="Transcrição do vídeo",metadata=list())
  },.package="genflow")
  result <- gen_stt(path,service="local-native",model="fixture.gguf",save_txt=FALSE)
  expect_identical(result$status_api,"SUCCESS")
  expect_identical(result$response_value,"Transcrição do vídeo")
  expect_false(file.exists(seen))
  expect_true(file.exists(path))
})

test_that("silent or corrupt videos fail before invoking any provider", {
  path <- transcribe_fixture(sound=FALSE); on.exit(unlink(dirname(path),recursive=TRUE),add=TRUE)
  called <- FALSE
  local_mocked_bindings(.stt_local_native=function(...) { called <<- TRUE; stop("PROVIDER CALLED") },.package="genflow")
  expect_error(gen_stt(path,service="local-native",model="fixture.gguf",save_txt=FALSE),"Não foi possível extrair", class="genflow_error")
  writeLines("not a video",path)
  expect_error(gen_stt(path,service="local-native",model="fixture.gguf",save_txt=FALSE),"Não foi possível extrair", class="genflow_error")
  expect_false(called)
})

test_that("downloaded video is prepared before the provider reads it", {
  path <- transcribe_fixture(); on.exit(unlink(dirname(path),recursive=TRUE),add=TRUE)
  local_mocked_bindings(.stt_download_audio=function(...) list(path=path,tmp=path),
    .stt_local_native=function(audio_path,...) {
      expect_identical(tools::file_ext(audio_path),"wav")
      expect_true(file.exists(audio_path))
      list(text="Downloaded video",metadata=list())
    },.package="genflow")
  result <- gen_stt("https://example.invalid/video.mp4",service="local-native",model="fixture.gguf",save_txt=FALSE)
  expect_identical(result$status_api,"SUCCESS")
  expect_false(file.exists(path))
})
