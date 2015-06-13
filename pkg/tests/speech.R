library(uuid)
library(functional)

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
                codec = "audio/wav",
                samplerate = 16000,
                sourcerate = 8000,
                trustsourcerate = FALSE),
            conversion = format.content.type)),
    .parameters =
      list(
        version = a(default = "3.0", export = NULL),
        requestid	= a(export = NULL),
        appID = a(default = "D4D52672-91D7-4C74-8AD8-42B1D98141A5", export = NULL),
        format	= a(default = "JSON", export = NULL),
        locale = a(default = "en-US"),
        device.os = a(default = "SomeOS", export = NULL),
        scenarios	=
          a(
            default = c("ulm", "websearch"),
            conversion = Curry(match.arg, choices = default)),
        instanceid	= a(export = NULL),
        maxnbest = a(default = 1),
        result.profanitymarkup = a(default = TRUE, conversion = as.integer),
        client_secret = a(mandatory = TRUE),
        client_id = a(default = 111111)),
    .body = list(audio.data = a(mandatory = TRUE, conversion = identity)),
    .body.encoding = "multipart",
    .body.conversion = function(x) x$audio.data,
    .init =
      function(args) {
        args$requestid = UUIDgenerate()
        args$instanceid = UUIDgenerate()
        args
      })