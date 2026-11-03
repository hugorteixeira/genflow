native_tts_model <- function(architecture="voxcpm2",path=tempfile(fileext=".gguf")) {
  con <- file(path,"wb"); on.exit(close(con))
  uint <- function(n,size) writeBin(as.raw(floor(n/256^(seq_len(size)-1L)) %% 256),con)
  string <- function(x) { bytes <- charToRaw(x); uint(length(bytes),8); writeBin(bytes,con) }
  writeBin(charToRaw("GGUF"),con); uint(3,4); uint(0,8); uint(1,8)
  string("general.architecture"); uint(8,4); string(architecture)
  path
}

native_tts_fixture <- function(env=parent.frame()) {
  root <- tempfile("tts-native-test-"); dir.create(root)
  withr::defer(unlink(root,recursive=TRUE),envir=env)
  model <- native_tts_model(path=file.path(root,"renamed model.gguf"))
  executable <- normalizePath(test_path("fixtures","tts-native.py"))
  config <- file.path(root,"config.json")
  withr::local_options(list(genflow.local_config_path=config),.local_envir=env)
  withr::local_envvar(c(GENFLOW_TEST_TTS_MODE="ok",GENFLOW_TEST_TTS_RECORD=file.path(root,"record.json"),
    GENFLOW_TTS_NATIVE_MODEL=NA,GENFLOW_TTS_NATIVE_EXECUTABLE=NA,GENFLOW_TTS_NATIVE_DEVICE=NA),.local_envir=env)
  gen_local_config(tts_native_model=model,tts_native_executable=executable,tts_native_device="cpu")
  list(root=root,model=model,executable=executable,record=file.path(root,"record.json"))
}
