# The MIT License (MIT)
# 
# Copyright (c) 2015 Microsoft Corporation
#   
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#   
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.




#' Function to define arguments for a web API call.
#' 
#' Allows to define each component of a web API call and to map it to an
#' argument of an R call
#' 
#' Used exclusively to define arguments to the function \code{make.web.call}
#' 
#' @aliases arg.spec a
#' @param default The default value for this argument
#' @param type The desired type (currently unenforced)
#' @param mandatory Whether this argument is mandatory.
#' @param validate Validation function for the value of an argument (currently
#' unused)
#' @param conversion The conversion function to be applied to an R value to
#' obtain and appropriate encoding for the web call.
#' @param export Whether this argument should aslo be an argument to the
#' corresponding R call (some web API call take a conventional value, in other
#' cases it can be computed from the other arguments, or it is a random id)
#' @return A list containing the information needed by \code{make.web.call}
#' @export
arg.spec =
  function(
    default = NULL,
    type = class(default),
    mandatory = is.null(default),
    validate = function(x) is.null(type) || class(x) %in% type,
    conversion = as.character,
    export = identity) {
    args = arglist()
    structure(
      args,
      class = "argspec")}

#' @rdname arg.spec
#' @export
a = arg.spec

arglist =
  function() {
    fun = sys.function(-1)
    fa = formals(fun)
    ma = as.list(match.call(fun, sys.call(-1)))[-1]
    sapply(names(ma), function(n) fa[n] <<- ma[n])
    fa}

arg.filler =
  function(spec, vals, encode){
    filled =
#      discard(
        setNames(
          lapply(
            names(spec),
            function(n) {
              sn = spec[[n]]
              n1 = {
                if(is.null(sn$export))
                  n
                else {
                  if (is.character(sn$export))
                    sn$export
                  else
                    sn$export(n)}}
              vn = {
                if(is.null(vals[[n1]]))
                  sn$conversion(sn$default)
                else
                  sn$conversion(vals[[n1]])}}),
          names(spec))  #### ,
#        is.null)
    filled = filled[!is.null(filled)]
    if(length(filled) == 0) NULL
    else {
      if(encode) sapply(filled, curlEscape)
      else filled }}



#' Performs the specific encoding required by the \code{Content-type} http
#' header.
#' 
#' Implements the typical encoding for \code{Content-type}, which is semicolon
#' separated values or key-value pairs, where the pairs are slash-separated.
#' 
#' This function should only be used as a conversion argument when specifyng
#' the \code{Content-type} header.
#' 
#' @param x Description of content type as a list
#' @param ... ignored
#' 
#' @return A formatted \code{Content-type} string.
#' @export
format.content.type =
  function(x, ...) {
    gsub(
      pattern = "^=",
      replacement = "",
      x = paste(names(content.type), tolower(x), sep = "=", collapse = ";"))}
## XXXX               ^^^^^^^^^^^^  content.type is not defined?

applyval =
  function(ll, frame)
    lapply(ll, function(x) lapply(x, eval, envir = x, enclos = frame))

path.encoding =
  function(arglist)
    paste(names(arglist), arglist, sep = "/", collapse = "/")

#' @export
interpylate =
  function(template)
    function(args) {
      template = strsplit(x = template, split = "[{}]")[[1]]
      paste(
        ifelse(
          1:length(template)%%2 == 1,
          template,
          args[template]),
        collapse = "")}



#' Generate a R function call from a detailed description of a web API entry point.
#' 
#' @description 
#' Mapping R calls to web API is a repetitive and error prone task. By using this function instead you take advantage an automatic, uniform and tested mapping between a web entry point and an R function call.
#' 
#' The idea is to take a detailed description of web api and turn into one or more R calls using this function. A web api is characterized by what goes in the URL, a fixed part and a variable part, then headers that open the http transfer of data and finally what goes in the body of the request.
#' 
#' Since we'd like R users to specify values in whatever type is most appropriate, but how to represent that in an http request is dependent on the specific API, conversions and representations are an important aspect of a Web API, not only in R.
#' 
#' At the argument by argument level, this is taken care of the by the \code{arg.spec} function. This call instead specifies all the other aspects of conversion or encoding, that is how to form the URL for the call (\code{.param.encoding}) and how to encode the body, which is specified with two arguments.
#' 
#' With \code{.body} the user specifies a list of elements that should contribute to the body of a request, each of which can have its own conversion function. Then \code{body.conversion} is applied to this list.
#' 
#' Finally, if the result of \code{.body.conversion} is character, it becomes directly the body of the request. If it is a list, it will be encoded using the method specified by \code{.body.encoding}.
#' 
#' It's a multi step process that tries to take cover very disparate situations whereby the body can contain an additional series of options represented with json or binary sound or image data, and we'll be looking to streamline this part even further.
#' 
#' Defaults have been selected to cover the most common cases.
#' 
#' @param .method The http method to use.
#' @param .url The url for the API endpoint (can be extended in some instances by using other options)
#' @param .parameters List of API parameter specs, created with the function \code{arg.spec} or \code{a} for short.
#' @param .param.encoding Whether these parameters should be encoded as a stadard query string or a Django-like path suffix \code{/name/value/name/value}
#' @param .headers List of API header specs, created with the function \code{arg.spec} or \code{a} for short.
#' @param .body List of API body specs, created with the function \code{arg.spec} or \code{a} for short. Use \code{httr::upload_file} as a value to upload a file.
#' @param .body.encoding How the body of the request is encoded. If one of "json", "multipart" or "form" and if \code{.body} is a named list, the corresponding encoding is used. If a function, that function will be used to
#' encode \code{.body}. Use "multipart" to upload a file.
#' @param .response.encoding Hot the response to the http request is encoded. If one of "parsed", "text", "raw", pass the encoding to
#' \code{httr::content}. In particular, the default, "parsed", chooses the encoding based on the response headers and covers a large variety of formats. If a function, the response contents are extracted as text and passed to this function.
#' @param .init Transformation to apply to list of all arguments of the generated call, for instance late initialization or verifying contstraints. Returns the list of arguments.
#' @param .skip.on.error If TRUE, gives a warning if HTTP response code is not equal to 200
#' @param .policy Policy object to contol access to web resource.
#' 
#' @return A function with an argument for each exported element (see \code{arg.spec}) contained in each of \code{.parameters}, \code{.headers} and \code{.body}. This function will form an http request as described above
#' and perform it, and return the body of the response (decoding features are being worked on).

#' @examples
#' 
#' # available in tests subdirectory
#' 
#' @export make.web.call
make.web.call =
  function(
    .method =
      c("get", "patch", "post", "put", "delete", "head"),
    .url,
    .parameters  = NULL,
    .param.encoding = c("query", "path", "none"),
    .headers = NULL,
    .body = NULL,
    .body.encoding = c("json", "form", "multipart"),
    .response.encoding = c("parsed", "text", "raw"),
    .init = identity,
    .skip.on.error = FALSE,
    .policy = Policy()) {
    .method = get(toupper(match.arg(.method)), envir = environment(httr::POST))
    if(is.character(.param.encoding)) .param.encoding = match.arg(.param.encoding)
    if(is.character(.response.encoding)){
      .response.conversion = identity
      .response.encoding = match.arg(.response.encoding)}
    else {
      .response.conversion = .response.encoding
      .response.encoding = "text"}
    if(is.function(.body.encoding)){
      .body.conversion = .body.encoding
      .body.encoding = "multipart"}
    else {
      .body.encoding = match.arg(.body.encoding)
      .body.conversion = identity}
    .parameters = applyval(.parameters, parent.frame())
    .headers = applyval(.headers, parent.frame())
    .body = applyval(.body, parent.frame())
#    formal.args = discard(c(.parameters, .headers, .body), ~is.null(.$export))
    formal.args = c(.parameters, .headers, .body)
    formal.args = formal.args[vapply(formal.args, function(z) !is.null(z$export),TRUE)]
# XXX shouldn't there be a check here for NULL formal.args after this?
    names(formal.args) =
      sapply(
        names(formal.args),
        function(n) {
          export = formal.args[[n]]$export
          if(is.character(export))
            export
          else
            export(n)})
    .web.call = (
      function() {
        last.call = Sys.time()
        function() {
          args = arglist()
          args = lapply(args, eval, envir = parent.frame())
          args = .init(args)
          if(is.character(.param.encoding) && .param.encoding == "path")
            .param.encoding = path.encoding
          if(is.function(.param.encoding)){
            .url =
              paste(
                .url,
                .param.encoding(arg.filler(.parameters, args, TRUE)), sep = "/")}
          enforce(.policy)
          arglist =
            c(
              list(
                url = .url,
                add_headers(unlist(arg.filler(.headers, args, FALSE))),
                body = .body.conversion(arg.filler(.body, args, FALSE)),
                encode = .body.encoding),
              if(identical(.param.encoding, "query"))
                list(
                  query =
                    arg.filler(.parameters, args, TRUE)))
          req = do.call(.method, arglist)
          update(.policy)
          if(req$status_code != 200) {
            if(.skip.on.error) {
              warn_for_status(req)
              return(NULL)}
            else
              stop(http_condition(req, "error", call = sys.call()), content(req, as = "text"))}
          warn_for_status(req)
          .response.conversion(content(req, .response.encoding))}})()
    formals(.web.call) =
      lapply(
        formal.args,
        function(x){
          if(x$mandatory)
            quote(stop("mandatory argument"))
          else
            x$default})
    .web.call}

