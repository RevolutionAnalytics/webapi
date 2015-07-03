
detect(
  analyzesFaceLandmarks = TRUE,
  `Ocp-Apim-Subscription-Key` = face.key,
  url = "http://www.projectoxford.ai/images/bright/face/Face%20detection.jpg")

group_2011 =
  detect(
    analyzesFaceLandmarks = TRUE,
    `Ocp-Apim-Subscription-Key` = face.key,
    url = "http://sims.ess.ucla.edu/people-images/2011_Feb_group.jpg")

group_2008 =
  detect(
    analyzesFaceLandmarks = TRUE,
    `Ocp-Apim-Subscription-Key` = face.key,
    url = "http://sims.ess.ucla.edu/people-images/2008_Dec_group.JPG")

find.similar(
  `Ocp-Apim-Subscription-Key` = face.key,
  faceId =  map(group_2011, "faceId")[[1]],
  faceIds =  unlist(map(group_2008, "faceId")))

grouped.faces =
  group(
    `Ocp-Apim-Subscription-Key` = face.key,
    faceIds =  unlist(c(map(group_2008, "faceId"), map(group_2011, "faceId"))))

# with_verbose(
#   identify(
#     Ocp-Apim-Subscription-Key = face.key,
#     )
# )

verify(
  `Ocp-Apim-Subscription-Key` = face.key,
  faceId1 = grouped.faces[[1]][[1]][[1]],
  faceId2 = grouped.faces[[1]][[2]][[2]])

create.person.group(
  persongroups = UUIDgenerate(),
  `Ocp-Apim-Subscription-Key` = face.key,
  name = "pippo",
  userData = list(relation = "friend of mine"))

groups = list.person.groups(`Ocp-Apim-Subscription-Key` = face.key)

delete.person.group(persongroups = groups[[1]]$personGroupId, `Ocp-Apim-Subscription-Key` = face.key)

list.person.groups(`Ocp-Apim-Subscription-Key` = face.key)

create.person.group(
  persongroups = UUIDgenerate(),
  `Ocp-Apim-Subscription-Key` = face.key,
  name = "pippo",
  userData = list(relation = "friend of mine"))

groups = list.person.groups(`Ocp-Apim-Subscription-Ke qy` = face.key)

get.person.group(persongroups = groups[[1]]$personGroupId, `Ocp-Apim-Subscription-Key` = face.key)

update.person.group(
  persongroups = groups[[1]]$personGroupId,
  name = "relatives",
  userData = list(relation = "a bunch of cousins"),
  `Ocp-Apim-Subscription-Key` = face.key)

groups = list.person.groups(`Ocp-Apim-Subscription-Key` = face.key)

joe =
  create.person(
    persongroups = groups[[1]]$personGroupId,
    `Ocp-Apim-Subscription-Key` = face.key,
    faceIds = unlist(grouped.faces$groups[[1]]),
    name = "Joe",
    userData = list(relation = "an old friend"))

add.person.face(persongroups = groups[[1]]$personGroupId, persons = joe$personId, faces = grouped.faces$groups[[2]][[1]], `Ocp-Apim-Subscription-Key` = face.key, userData = list(comment = "additional face"))

joe = get.person(persongroups = groups[[1]]$personGroupId, persons = joe$personId, `Ocp-Apim-Subscription-Key` = face.key)

delete.person.face(persongroups = groups[[1]]$personGroupId, persons = joe$personId, faces = joe$faceIds[[3]], `Ocp-Apim-Subscription-Key` = face.key )

joe = get.person(persongroups = groups[[1]]$personGroupId, persons = joe$personId, `Ocp-Apim-Subscription-Key` = face.key)

delete.person(persongroups = groups[[1]]$personGroupId, persons = joe$personId, `Ocp-Apim-Subscription-Key` = face.key)

get.person(persongroups = groups[[1]]$personGroupId, persons = joe$personId, `Ocp-Apim-Subscription-Key` = face.key)

joe =
  create.person(
    persongroups = groups[[1]]$personGroupId,
    `Ocp-Apim-Subscription-Key` = face.key,
    faceIds = unlist(grouped.faces$groups[[1]]),
    name = "Joe",
    userData = list(relation = "an old friend"))

joe = get.person(persongroups = groups[[1]]$personGroupId, persons = joe$personId, `Ocp-Apim-Subscription-Key` = face.key)

get.person.face(persongroups = groups[[1]]$personGroupId, persons = joe$personId, faces = joe$faceIds[[1]], `Ocp-Apim-Subscription-Key` = face.key)

list.person.groups(`Ocp-Apim-Subscription-Key` = face.key)

update.person(persongroups = groups[[1]]$personGroupId, persons = joe$personId, `Ocp-Apim-Subscription-Key` = face.key, faceIds = unlist(grouped.faces$groups[[2]]), name = "Joe Shmo", userData = list(comment = "very cool guy"))

joe = get.person(persongroups = groups[[1]]$personGroupId, persons = joe$personId, `Ocp-Apim-Subscription-Key` = face.key)

update.person.face(persongroups = groups[[1]]$personGroupId, persons = joe$personId, `Ocp-Apim-Subscription-Key` = face.key, faces = joe$faceIds[[1]], userData = list(data ="more data"))

get.person.face(persongroups = groups[[1]]$personGroupId, persons = joe$personId, faces = joe$faceIds[[1]], `Ocp-Apim-Subscription-Key` = face.key)


train.person.group(persongroups = groups[[1]]$personGroupId, `Ocp-Apim-Subscription-Key` = face.key)

get.person.group.training.status(persongroups = groups[[1]]$personGroupId, `Ocp-Apim-Subscription-Key` = face.key)

identify(`Ocp-Apim-Subscription-Key` = face.key, personGroupId = groups[[1]]$personGroupId, faceIds = joe$faceIds)
