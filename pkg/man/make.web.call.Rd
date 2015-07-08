\name{make.web.call}
\alias{make.web.call}
\title{
Generate a R function call from a detailed description of a web API entry point.
}
\description{
Mapping R calls to web API is a repetitive and error prone task. By using this function instead you take advantage an automatic, uniform and tested mapping between a web entry point and an R function call.
}
\usage{
make.web.call(
  .method = c("get", "patch", "post", "put", "delete", "head"),
  .url,
  .parameters,
  .param.encoding = c("query", "path"),
  .headers,
  .body,
  .body.encoding = c("json", "form", "multipart"),
  .response.encoding = c("parsed", "text", "raw"),
  .init = identity,
  .policy = Policy())
}
\arguments{
  \item{.method}{
The http method to use.
}
  \item{.url}{
The url for the API endpoint (can be extended in some instances by using other options)
}
  \item{.parameters}{
  List of API parameter specs, created with the function \code{arg.spec} or \code{a} for short.
}
  \item{.param.encoding}{
  Whether these parameters should be encoded as a stadard query string or a Django-like path suffix \code{/name/value/name/value}
}
  \item{.headers}{
  List of API header specs, created  with the function \code{arg.spec} or \code{a} for short.
}
  \item{.body}{
  List of API body specs, created  with the function \code{arg.spec} or \code{a} for short. Use \code{httr::upload_file} as a value to upload a file.
}
  \item{.body.encoding}{How the body of the request is encoded. If one of "json", "multipart" or "form" and if \code{.body} is a named list, the corresponding encoding is used. If a function, that function will be used to encode \code{.body}. Use "multipart" to upload a file.
}
  \item{.response.encoding}{Hot the response to the http request is encoded. If one of "parsed", "text", "raw", pass the encoding to \code{httr::content}. In particular, the default, "parsed", chooses the encoding based on the response headers and covers a large variety of formats. If a function, the response contents are extracted as text and passed to this function. }
  \item{.init}{
Transformation to apply to list of all arguments of the generated call, for instance late initialization or verifying contstraints. Returns the list of arguments.
}
\item{.policy}{Policy object to contol access to web resource.}
}
\details{
The idea is to take a detailed description of web api and turn into one or more R calls using this function. A web api is characterized by what goes in the URL, a fixed part and a variable part, then headers that open the http transfer of data and finally what goes in the body of the request. Since we'd like R users to specify values in whatever type is most appropriate, but how to represent that in an http request is dependent on the specific API, conversions and representations are an important aspect of a Web API, not only in R. At the argument by argument level, this is taken care of the by the \code{arg.spec} function. This call instead specifies all the other aspects of conversion or encoding, that is how to form the URL for the call (\code{.param.encoding}) and how to encode the body, which is specified with two arguments. With \code{.body} the user specifies a list of elements that should contribute to the body of a request, each of which can have its own conversion function. Then \code{body.conversion} is applied to this list. Finally, if the result of \code{.body.conversion} is character, it becomes directly the body of the request. If it is a list, it will be encoded using the method specified by \code{.body.encoding}. It's a multi step process that tries to take cover very disparate situations whereby the body can contain an additional series of options represented with json or binary sound or image data, and we'll be looking to streamline this part even further. Defaults have been selected to cover the most common cases.
}
\value{
A function with an argument for each exported element (see \code{arg.spec}) contained in each of \code{.parameters}, \code{.headers} and \code{.body}. This function will form an http request as described above and perform it, and return the body of the response (decoding features are being worked on).
}
\examples{
# available in tests subdirectory
}
