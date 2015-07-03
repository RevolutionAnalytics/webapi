
detect(
  analyzesFaceLandmarks = TRUE,
  subscriptionKey = face.key,
  url = "http://www.projectoxford.ai/images/bright/face/Face%20detection.jpg")

group_2011 =
  detect(
    analyzesFaceLandmarks = TRUE,
    subscriptionKey = face.key,
    url = "http://sims.ess.ucla.edu/people-images/2011_Feb_group.jpg")

group_2008 =
  detect(
    analyzesFaceLandmarks = TRUE,
    subscriptionKey = face.key,
    url = "http://sims.ess.ucla.edu/people-images/2008_Dec_group.JPG")

find.similar(
  subscriptionKey = face.key,
  faceId =  map(group_2011, "faceId")[[1]],
  faceIds =  unlist(map(group_2008, "faceId")))

grouped.faces =
  group(
    subscriptionKey = face.key,
    faceIds =  unlist(c(map(group_2008, "faceId"), map(group_2011, "faceId"))))

verify(
  subscriptionKey = face.key,
  faceId1 = grouped.faces[[1]][[1]][[1]],
  faceId2 = grouped.faces[[1]][[2]][[2]])

create.person.group(
  personGroupId = UUIDgenerate(),
  subscriptionKey = face.key,
  name = "pippo",
  userData = list(relation = "friend of mine"))

groups = list.person.groups(subscriptionKey = face.key)

delete.person.group(personGroupId = groups[[1]]$personGroupId, subscriptionKey = face.key)

list.person.groups(subscriptionKey = face.key)

create.person.group(
  personGroupId = UUIDgenerate(),
  subscriptionKey = face.key,
  name = "pippo",
  userData = list(relation = "friend of mine"))

groups = list.person.groups(subscriptionKey = face.key)

get.person.group(personGroupId = groups[[1]]$personGroupId, subscriptionKey = face.key)

update.person.group(
  personGroupId = groups[[1]]$personGroupId,
  name = "relatives",
  userData = list(relation = "a bunch of cousins"),
  subscriptionKey = face.key)

groups = list.person.groups(subscriptionKey = face.key)

joe =
  create.person(
    personGroupId = groups[[1]]$personGroupId,
    subscriptionKey = face.key,
    faceIds = unlist(grouped.faces$groups[[1]]),
    name = "Joe",
    userData = list(relation = "an old friend"))

add.person.face(personGroupId = groups[[1]]$personGroupId, personId = joe$personId, faceId = grouped.faces$groups[[2]][[1]], subscriptionKey = face.key, userData = list(comment = "additional face"))

joe = get.person(personGroupId = groups[[1]]$personGroupId, personId = joe$personId, subscriptionKey = face.key)

delete.person.face(personGroupId = groups[[1]]$personGroupId, personId = joe$personId, faceId = joe$faceIds[[3]], subscriptionKey = face.key )

joe = get.person(personGroupId = groups[[1]]$personGroupId, personId = joe$personId, subscriptionKey = face.key)

delete.person(personGroupId = groups[[1]]$personGroupId, personId = joe$personId, subscriptionKey = face.key)

get.person(personGroupId = groups[[1]]$personGroupId, personId = joe$personId, subscriptionKey = face.key)

joe =
  create.person(
    personGroupId = groups[[1]]$personGroupId,
    subscriptionKey = face.key,
    faceIds = unlist(grouped.faces$groups[[1]]),
    name = "Joe",
    userData = list(relation = "an old friend"))

joe = get.person(personGroupId = groups[[1]]$personGroupId, personId = joe$personId, subscriptionKey = face.key)

get.person.face(personGroupId = groups[[1]]$personGroupId, personId = joe$personId, faceId = joe$faceIds[[1]], subscriptionKey = face.key)

list.person.groups(subscriptionKey = face.key)

update.person(personGroupId = groups[[1]]$personGroupId, personId = joe$personId, subscriptionKey = face.key, faceIds = unlist(grouped.faces$groups[[2]]), name = "Joe Shmo", userData = list(comment = "very cool guy"))

joe = get.person(personGroupId = groups[[1]]$personGroupId, personId = joe$personId, subscriptionKey = face.key)

update.person.face(personGroupId = groups[[1]]$personGroupId, personId = joe$personId, subscriptionKey = face.key, faceId = joe$faceIds[[1]], userData = list(data ="more data"))

get.person.face(personGroupId = groups[[1]]$personGroupId, personId = joe$personId, faceId = joe$faceIds[[1]], subscriptionKey = face.key)


train.person.group(personGroupId = groups[[1]]$personGroupId, subscriptionKey = face.key)

get.person.group.training.status(personGroupId = groups[[1]]$personGroupId, subscriptionKey = face.key)

identify(subscriptionKey = face.key, personGroupId = groups[[1]]$personGroupId, faceIds = joe$faceIds)
