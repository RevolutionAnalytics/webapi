#webapi

Helper package to create web APIs.


## Example


```r
library(webapi)

my.web.call =
  make.web.call(
    .method = "post",
    .url = "http://httpbin.org/post",
    .parameters =
      list(
        analyzesFaceLandmarks = NULL,
        analyzesAge = NULL,
        analyzesGender = NULL,
        analyzesHeadPose = NULL),
    .headers =
      list(
        `Content-type` = NULL,
        `Ocp-Apim-Subscription-Key` = quote(stop("missing key"))),
    .body =
      list(url = ""))
```
