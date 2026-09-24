# Upload Media

Uploads an image, GIF or video to X so it can be attached to a post, via
the [chunked upload
endpoints](https://docs.x.com/x-api/media/quickstart/media-upload-chunked).
Needs a user token, so the first call opens a browser window to sign in.

Pass the returned id to
[`create_post()`](https://Ivey-Business-School.github.io/xapir/reference/create_post.md)
as `media_ids`. A post can carry up to 4 photos, 1 GIF or 1 video.

## Usage

``` r
upload_media(
  path,
  media_category = c("tweet_image", "tweet_video", "tweet_gif"),
  alt_text = NULL,
  chunk_size = 4 * 1024^2
)
```

## Arguments

- path:

  Path to the file. The type is read from the extension: png, jpg, jpeg,
  gif, webp, mp4 or mov.

- media_category:

  What the file is for: `"tweet_image"` (the default) for a photo,
  `"tweet_video"` for a video, `"tweet_gif"` for an animated GIF. X
  limits size and length by category and by the account's tier.

- alt_text:

  Optional description of the image for screen readers, up to 1,000
  characters. Sent as media metadata once the upload succeeds.

- chunk_size:

  Bytes per chunk. X accepts at most 5 MB a chunk; the default is 4 MB.

## Value

Invisibly, the media id as a string, with the `media_key` as an
attribute. Stops with the API's message when a step is refused or
processing fails.

## Details

The upload runs in four steps, each printed as it happens: initialize
(tell X the type and size), append (send the file in chunks of at most
`chunk_size` bytes, so a large video is never read into memory in one
go), finalize, and, for a video or GIF, poll the status endpoint until X
has finished processing it. Processing usually takes a few seconds; the
function gives up after ten minutes with the media id so you can try
again later.

The pricing page does not list media uploads, so no cost line is printed
for the upload itself. Alt text goes through the media metadata
endpoint, which is billed per request, and that request announces its
cost.

## Examples

``` r
if (FALSE) { # \dontrun{
media_id <- upload_media("chart.png", alt_text = "Sales by month, 2026")
create_post("Our year so far", media_ids = media_id)

video_id <- upload_media("launch.mp4", media_category = "tweet_video")
create_post("Watch the launch", media_ids = video_id)
} # }
```
