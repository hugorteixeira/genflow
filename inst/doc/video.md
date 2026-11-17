# Video with supplied audio

`gen_video()` supports these qualified image + audio endpoints:

- Fal: `fal-ai/kling-video/ai-avatar/v2/standard` and `/pro`.
- Replicate: `wan-video/wan-2.2-s2v`.

The input image must be one composed scene, not a character sheet. Use image
reference generation first when the source material is a character sheet.
Supply a local image and a local speech audio file, including output of
`gen_tts()`. The selected model must be qualified; arbitrary catalog video rows
are not assumed to support these inputs. Credentials are `FAL_API_KEY` (or
`FAL_KEY`) and `REPLICATE_API_TOKEN`.

```r
result <- gen_video("Gentle expressive movement", image="scene.png",
  audio="speech.wav", service="fal",
  model="fal-ai/kling-video/ai-avatar/v2/standard")
```

For durable orchestration, call `gen_video_submit()` once, save its receipt,
poll with `gen_video_poll()`, then use `gen_video_collect(receipt, audio=...)`.
The receipt contains the task ID and original audio hash, not credentials or
media. Collection verifies the original audio hash and replaces the provider's
track with that audio (AAC in MP4). Polling and collection never submit another
prediction. An ambiguous submission must not be automatically repeated.
Provider outputs have limited retention: recover a completed result promptly.

Current input limits: image 10 MiB, audio 100 MiB; qualified clip duration at
most 300 seconds for Fal or 60 seconds for Replicate. These conservative local
limits do not guarantee a provider will accept every file. Returned videos must
have a duration consistent with the supplied audio and decode successfully.

`gen_video_compose(image,audio)` creates a narrated **still-frame** video
locally. It does not synthesize animation or lip motion. `gen_video_join(videos)`
normalizes an ordered collection to 16:9 H.264/AAC and writes one MP4. Both use
FFmpeg, accept 640/1280/1920 width and 24/25/30 fps, and run without API charges.
Studio exposes these as **Imagem com narração** and **Unir vídeos**.

Protocol tests use simulated provider responses and real local media. They do
not establish visual quality or constitute a live paid-provider acceptance test.

Provider references:
- https://fal.ai/models/fal-ai/kling-video/ai-avatar/v2/standard/api
- https://replicate.com/wan-video/wan-2.2-s2v/api
- https://replicate.com/docs/reference/http
