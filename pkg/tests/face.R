
#problems:
#needs to specifify for argument: type, if mandatory, default value if optional
#for body: needs to survace
#argument names: may or may be not valid in R or user friendly. Need mapping
#indirect arguments may speficy binary argument with file-name

library(functional)
library(jsonlite)
library(purrr)

base.url = "https://api.projectoxford.ai/face/v0"

subkey = list(`Ocp-Apim-Subscription-Key` = a(mandatory = TRUE))
ctype =   list(`Content-Type` =  a(default = "application/json", export = NULL))
face.headers = c(ctype, subkey)
faceIds = list(faceIds = a(mandatory = TRUE))

#face
#

make.web.call.face =
  Curry(
    make.web.call,
    .headers = face.headers,
    .body.encoding = Curry(toJSON, auto_unbox = TRUE))

detect =
  make.web.call(
    .method = "post",
    .url = paste(sep = "/", base.url, "detections"),
    .parameters =
      list(
        analyzesFaceLandmarks = a(default = FALSE, conversion = tolower),
        analyzesAge = a(default = FALSE, conversion = tolower),
        analyzesGender = a(default = FALSE, conversion = tolower),
        analyzesHeadPose = a(default = FALSE, conversion = tolower)),
    .headers = face.headers,
    .body = list(url = a(), image.data = a()),
    .init =
      function(args) {
        if(is.null(args$url) == is.null(args$image.data))
          stop("Specify exactly one of url and image.data")
        c(
          args,
          list(
            `Content-Type` =
              if(is.null(url))
                "application/octet-stream"
            else
              "application/json"))},
    .body.encoding =
      function(x)
        toJSON(discard(x, ~length(.) == 0), auto_unbox = TRUE))

find.similar =
  make.web.call.face(
    .method = "post",
    .url = paste(sep = "/", base.url, "findsimilars"),
    .body = c(list(faceId = a(mandatory = TRUE)), faceIds))

group =
  make.web.call.face(
    .method = "post",
    .url = paste(sep = "/", base.url, "groupings"),
    .body = faceIds)

identify =
  make.web.call.face(
    .method = "post",
    .url = paste(sep = "/", base.url, "identifications"),
    .body =
      c(
        faceIds,
        list(
          personGroupId = a(mandatory = TRUE),
          maxNumOfCandidatesReturned = a(default = 1))))

verify =
  make.web.call.face(
    .method = "post",
    .url = paste(sep = "/", base.url, "verifications"),
    .body =
      list(
        faceId1 = a(mandatory = TRUE),
        faceId2 = a(mandatory = TRUE)))

## person

persongroups = list(persongroups = a(mandatory = TRUE))
persons = list(persons = a(mandatory = TRUE))
faces = list(faces = a(mandatory = TRUE))
fake.arg = a(default = "", export = NULL)
persons.empty = list(persons = fake.arg)
name = list(name = a(mandatory = TRUE))
userData = list(userData = a(default = "", conversion = toJSON))


make.web.call.person.body =
  Curry(
    make.web.call,
    .url = base.url,
    .headers = face.headers,
    .param.encoding = "path",
    .body.encoding =  Curry(toJSON, auto_unbox = TRUE))

make.web.call.person.no.body =
  Curry(
    make.web.call,
    .url = base.url,
    .headers = subkey,
    .param.encoding = "path")

add.person.face =
  make.web.call.person.body(
    .method = "put",
    .parameters = c(persongroups, persons, faces),
    .body = userData)

create.person =
  make.web.call.person.body(
    .method = "post",
    .parameters = c(persongroups, persons.empty),
    .body = c(faceIds, name, userData))

delete.person =
  make.web.call.person.no.body(
    .method = "delete",
    .parameters = c(persongroups, persons))

delete.person.face =
  make.web.call.person.no.body(
    .method = "delete",
    .parameters = c(persongroups, persons, faces))

get.person =
  make.web.call.person.no.body(
    .method = "get",
    .parameters = c(persongroups, persons))

get.person.face =
  make.web.call.person.no.body(
    .method = "get",
    .parameters = c(persongroups, persons, faces))

list.persons =
  make.web.call.person.no.body(
    .method = "get",
    .parameters = c(persongroups, persons.empty))

update.person =
  make.web.call.person.body(
    .method = "patch",
    .parameters = c(persongroups, persons),
    .body = c(faceIds, name, userData))

update.person.face =
  make.web.call.person.body(
    .method = "patch",
    .parameters = c(persongroups, persons, faces),
    .body = userData)

## persongroup
##

create.person.group =
  make.web.call.person.body(
    .method = "put",
    .parameters = persongroups,
    .body = c(name, userData))

delete.person.group =
  make.web.call.person.no.body(
    .method = "delete",
    .parameters = persongroups)

get.person.group =
  make.web.call.person.no.body(
    .method = "get",
    .parameters = persongroups)

get.person.group.training.status =
  make.web.call.person.no.body(
    .method = "get",
    .parameters = c(persongroups, list(training = fake.arg)))

list.person.groups =
  make.web.call.person.no.body(
    .method = "get",
    .parameters = list(persongroups = fake.arg))

train.person.group =
  make.web.call.person.no.body(
    .method = "post",
    .parameters = c(persongroups, list(training = fake.arg)))

update.person.group =
  make.web.call.person.body(
    .method = "patch",
    .parameters = persongroups,
    .body = c(name, userData))
