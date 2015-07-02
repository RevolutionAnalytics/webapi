
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


