# Copyright 2015 Revolution Analytics
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

arg.spec =
  a =
  function(
    default = NULL,
    type = class(default),
    mandatory = FALSE,
    validate = function(x) is.null(type) || class(x) %in% type,
    conversion = as.character,
    export = identity) {
    args = arglist()
    structure(
      args,
      class = "argspec")}

arglist =
  function() {
    fun = sys.function(-1)
    fa = formals(fun)
    ma = as.list(match.call(fun, sys.call(-1)))[-1]
    sapply(names(ma), function(n) fa[n] <<- ma[n])
    fa}

arg.filler =
  function(spec, vals){
    filled =
      discard(
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
          names(spec)),
        is.null)
    if(length(filled) == 0) NULL else filled}

format.content.type =
  function(content.type) {
    gsub(
      pattern = "^=",
      replacement = "",
      x = paste(names(content.type), tolower(content.type), sep = "=", collapse = ";"))}

applyval =
  function(ll, frame)
    lapply(ll, function(x) lapply(x, eval, envir = x, enclos = frame))

path.encoding =
  function(arglist)
    paste(names(arglist), arglist, sep = "/", collapse = "/")

make.web.call =
  function(
    .method =
      c("get", "patch", "post", "put", "delete", "head"),
    .url,
    .parameters  = NULL,
    .param.encoding = c("query", "path"),
    .headers = NULL,
    .body = NULL,
    .body.encoding = c("json", "form", "multipart"),
    .response.encoding = c("parsed", "text", "raw"),
    .init = identity) {
    .method = get(toupper(match.arg(.method)), envir = environment(httr::POST))
    .param.encoding = match.arg(.param.encoding)
    .response.encoding = match.arg(.response.encoding)
    if(is.function(.body.encoding)){
      .body.conversion = .body.encoding
      .body.encoding = "multipart"}
    else {
      .body.encoding = match.arg(.body.encoding)
      .body.conversion = identity}
    .parameters = applyval(.parameters, parent.frame())
    .headers = applyval(.headers, parent.frame())
    .body = applyval(.body, parent.frame())
    formal.args = discard(c(.parameters, .headers, .body), ~is.null(.$export))
    names(formal.args) =
      sapply(
        names(formal.args),
        function(n) {
          export = formal.args[[n]]$export
          if(is.character(export))
            export
          else
            export(n)})
    .web.call = function() {
      args = arglist()
      args = lapply(args, eval, envir = parent.frame())
      args = .init(args)
      if(.param.encoding == "path") {
        .url =
          paste(
            .url,
            path.encoding(arg.filler(.parameters, args)), sep = "/")}
      req =
        .method(
          url = .url,
          query =
            if(.param.encoding == "query")
              arg.filler(.parameters, args),
          add_headers(unlist(arg.filler(.headers, args))),
          body = .body.conversion(arg.filler(.body, args)),
          encode = .body.encoding)
      stop_for_status(req)
      warn_for_status(req)
      content(req, .response.encoding)}
    formals(.web.call) =
      lapply(
        formal.args,
        function(x){
          if(x$mandatory)
            quote(stop("mandatory argument"))
          else
            x$default})
    .web.call}

