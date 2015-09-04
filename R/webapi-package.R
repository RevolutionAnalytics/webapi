#' Support for Creating an R API From a Web API.
#' 
#' Create R functions from a description of each web API entry point.
#' 
#' Important functions:
#' 
#' \code{\link{make.web.call}}: Generate a R function call from a detailed description of a web API entry point
#' 
#' \code{\link{check}}: Check that a policy allows access to a resource
#' 
#' \code{\link{enforce}}: Enforce an access policy
#' 
#' \code{\link{update}}: Update a policy
#'
#' Policy.rate.limit
#' 
#' arg.spec
#' 
#' format.content.type

#' @name webapi-package
#' @aliases webapi
#' @docType package
#' @keywords package
#' 
#' @import httr
#' @import jsonlite 
#' @import purrr
#' @importFrom RCurl curlEscape
NULL





