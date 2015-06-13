
#problems:
#needs to specifify for argument: type, if mandatory, default value if optional
#for body: needs to survace
#argument names: may or may be not valid in R or user friendly. Need mapping
#indirect arguments may speficy binary argument with file-name

library(functional)
base.url = "http://api.projectoxford.ai/face/v0"

face.headers =
  list(
    `Content-Type` =  a(default = "application/json", export = NULL),
    `Ocp-Apim-Subscription-Key` = a(mandatory = TRUE))

detect.faces =
  make.web.call(
    .method = "post",
    .url = paste(sep = "/", base.url, "detections"),
    .parameters =
      list(
        analyzesFaceLandmarks = a(default = FALSE, conversion = tolower),
        analyzesAge = a(default = FALSE, conversion = tolower),
        analyzesGender = a(default = FALSE, conversion = tolower),
        analyzesHeadPose = a(default = FALSE, conversion = tolower)),
    .headers =
      list(
        `Content-Type` = a(export = NULL),
        `Ocp-Apim-Subscription-Key` = a(mandatory = TRUE)),
    .body =
      list(
        url = a(),
        image.data = a()),
    .init =
      function(args) {
         if(is.null(args$url) == is.null(args$image.data))
           stop("Specify exactly one of url and image.data")
        c(
          args,
          list(
            `Content-type` =
              if(is.null(url))
                "application/octet-stream"
            else
              "application/json"))},
    .body.conversion = Curry(toJSON, auto_unbox = TRUE))

find.similar.faces =
  make.web.call(
    .method = "post",
    .url = paste(sep = "/", base.url, "findsimilars"),
    .parameters = list(),
    .headers = face.headers,
    .body =
      list(
        faceId = a(mandatory = TRUE),
        faceIds = a(mandatory = TRUE))))

group.faces =
  make.web.call(
    .method = "post",
    .url = paste(sep = "/", base.url, "groupings"),
    .parameters = list(),
    .headers = face.headers,
    .body = list(faceIds = a(mandatory = TRUE)))

identify.faces =
  make.web.call(
    .method = "post",
    .url = paste(sep = "/", base.url, "identifications"),
    .parameters = list(),
    .headers = face.headers,
    .body =
      list(
        faceIds = a(mandatory = TRUE),
        personGroupId = a(mandatory = TRUE),
        maxNumOfCandidatesReturned = a(default = 1)))

verify.faces =
  make.web.call(
    .method = "post",
    .url = paste(sep = "/", base.url, "verifications"),
    .parameters = list(),
    .headers = face.headers,
    .body =
      list(
        faceId1 = a(mandatory = TRUE),
        faceId2 = a(mandatory = TRUE)))
