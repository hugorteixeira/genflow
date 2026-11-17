# Local composition is deterministic; it does not synthesize character motion.
.gen_video_ffmpeg <- function(args,budget) {
  x <- processx::run('ffmpeg',c('-v','error','-nostdin','-y',args),timeout=budget(),error_on_status=FALSE)
  if(x$status!=0L) stop('Video composition failed: ',substr(x$stderr,1L,1200L),call.=FALSE)
}
.gen_video_layout <- function(width,fps) {
  if(length(width)!=1L || !width %in% c(640L,1280L,1920L) || length(fps)!=1L || !fps %in% c(24L,25L,30L)) stop('Unsupported video layout.')
  h <- width*9/16
  paste0('scale=',width,':',h,':force_original_aspect_ratio=decrease,pad=',width,':',h,':(ow-iw)/2:(oh-ih)/2,setsar=1,fps=',fps,',format=yuv420p')
}
.gen_video_local_result <- function(path,directory,label,persist,budget) {
  meta <- .gen_video_probe(path,'video',budget())
  if(!meta$has_audio) stop('Composed video has no audio.')
  target <- .genflow_copy_artifact(path,.genflow_media_directory(directory,'videos',persist),label,'ffmpeg','composition','mp4')
  .genflow_complete_result(list(status_api='SUCCESS',status_msg='OK',response_value=target,service='ffmpeg',model='composition',video=meta),inference_status='SUCCESS',persistence=.genflow_persistence(target,requested=persist))
}
#' Compose a narrated still frame without changing the supplied voice
#' @param image Local image path.
#' @param audio Local audio path. Its duration determines the clip duration.
#' @param width Output width; 640, 1280 or 1920, at 16:9.
#' @param fps Output frame rate; 24, 25 or 30.
#' @param directory Output directory.
#' @param label Filename label.
#' @param persist Whether to persist the final file.
#' @param timeout Total processing budget in seconds.
#' @return A common Genflow video result. This is a still-frame video, not AI animation.
#' @export
gen_video_compose <- function(image,audio,width=1280L,fps=24L,directory=NULL,label=NULL,persist=TRUE,timeout=240) {
  .gen_sound_tools(); budget <- .gen_image_budget(timeout); filter <- .gen_video_layout(width,fps)
  .gen_video_probe(image,'image',budget()); au <- .gen_video_probe(audio,'audio',budget())
  if(au$duration_seconds>600) stop('A narrated clip is limited to ten minutes.')
  path<-tempfile(fileext='.mp4');on.exit(unlink(path),add=TRUE)
  .gen_video_ffmpeg(c('-loop','1','-i',normalizePath(image),'-i',normalizePath(audio),'-map','0:v:0','-map','1:a:0','-vf',filter,'-t',as.character(au$duration_seconds),'-c:v','libx264','-preset','veryfast','-crf','20','-threads','2','-c:a','aac','-b:a','192k','-ar','48000','-ac','2','-movflags','+faststart',path),budget)
  .gen_video_local_result(path,directory,label,persist,budget)
}
#' Join an ordered collection of videos into one MP4
#' @param videos Ordered local video paths, at most 100, total at most one hour.
#' @inheritParams gen_video_compose
#' @return A common Genflow result with one playable MP4 in the supplied order.
#' @export
gen_video_join <- function(videos,width=1280L,fps=24L,directory=NULL,label=NULL,persist=TRUE,timeout=240) {
  if(!is.character(videos)||!length(videos)||length(videos)>100L||anyNA(videos)) stop('Supply one to 100 ordered video paths.')
  .gen_sound_tools();budget<-.gen_image_budget(timeout);filter<-.gen_video_layout(width,fps)
  meta<-lapply(videos,.gen_video_probe,kind='video',timeout=budget())
  if(any(!vapply(meta,`[[`,logical(1),'has_audio')) || sum(vapply(meta,`[[`,numeric(1),'duration_seconds'))>3600) stop('Each clip needs audio; maximum total duration is one hour.')
  scratch<-tempfile('video-join-');dir.create(scratch);on.exit(unlink(scratch,recursive=TRUE),add=TRUE)
  files<-file.path(scratch,sprintf('clip-%03d.mp4',seq_along(videos)))
  for(i in seq_along(videos)) .gen_video_ffmpeg(c('-i',normalizePath(videos[[i]]),'-map','0:v:0','-map','0:a:0','-vf',filter,'-af','aresample=48000:async=1:first_pts=0,apad','-t',as.character(meta[[i]]$duration_seconds),'-c:v','libx264','-preset','veryfast','-crf','20','-threads','2','-c:a','aac','-b:a','192k','-ar','48000','-ac','2',files[[i]]),budget)
  manifest<-file.path(scratch,'clips.txt');writeLines(sprintf("file 'clip-%03d.mp4'",seq_along(files)),manifest)
  path<-file.path(scratch,'joined.mp4')
  .gen_video_ffmpeg(c('-f','concat','-safe','1','-i',manifest,'-c','copy','-movflags','+faststart',path),budget)
  .gen_video_local_result(path,directory,label,persist,budget)
}
