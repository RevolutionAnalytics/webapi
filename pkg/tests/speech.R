library(uuid)

voice.recognition =
  make.web.call(
    .method = "post",
    .url = "https://speech.platform.bing.com/recognize",
    .headers =
      list(
        `Content-type` =
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
        Version = a(default = "3.0", export = NULL),
        requestid	= a(export = NULL),
        appID = a(default = "D4D52672-91D7-4C74-8AD8-42B1D98141A5", export = NULL),
        format	= a(default = "JSON", export = NULL),
        locale = a(default = "en-US"),
        device.os = a(default = "SomeOS", export = NULL),
        scenarios	= a(c("ulm", "websearch"), process = match.arg),
        instanceid	= a(export = NULL),
        maxnbest = a(default = 1),
        result.profanitymarkup = a(default = TRUE, process = as.integer)),
    .body = list(audio.data = a(mandatory = TRUE)),
    .body.encoding = "multipart",
    .body.conversion = function(x) x$audio.data,
    .init =
      function(args) {
        args$requestid = UUIDgenerate()
        args$instanceid = UUIDgenerate()
        args
      })