library(uuid)
library(functional)

voice.token =
make.web.call(
.method = "post",
.url = "https://oxford-speech.cloudapp.net/token/issueToken",
.headers =
  list(
  `Content-Type` = a(default = list("application/x-www-form-urlencoded"), export = NULL)),
.parameters = NULL,
.body=
  list(
    grant_type = a(default = "client_credentials", export = NULL),
    client_id = a(mandatory = TRUE),
    client_secret = a(mandatory = TRUE),
    scope = a(default = "https://speech.platform.bing.com", export = NULL)),
.body.encoding = "form")

voice.recognition =
make.web.call(
.method = "post",
.url = "https://speech.platform.bing.com/recognize",
.headers =
  list(
    `Content-Type` =
      a(
        default =
          list(
            "audio/wav",
            samplerate = 8000),
        conversion = format.content.type),
    Authorization = a(mandatory = TRUE, conversion = function(x) paste0("Bearer ", x))),
.parameters =
  list(
    version = a(default = "3.0", export = NULL),
    requestid	= a(export = NULL),
    appid = a(default = "D4D52672-91D7-4C74-8AD8-42B1D98141A5", export = NULL),
    format	= a(default = "xml", export = NULL),
    locale = a(default = "en-US"),
    device.os = a(default = "wp7", export = NULL),
    scenarios	=
      a(
        default = c("ulm", "websearch"),
        conversion = Curry(match.arg, choices = default)),
    instanceid	= a(export = NULL)),
#         maxnbest = a(default = 1),
#         result.profanitymarkup = a(default = TRUE, conversion = as.integer)),
.body = list(audio.data = a(mandatory = TRUE, conversion = identity)),
.body.encoding = function(x) x$audio.data,
.init =
  function(args) {
    args$requestid = UUIDgenerate()
    args$instanceid = UUIDgenerate()
    args
  })voice.synthesis =
  make.web.call(
    .method = "post",
    .url = "https://speech.platform.bing.com/synthesize",
    .parameters =
  )