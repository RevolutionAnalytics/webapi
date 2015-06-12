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
    discard(
      setNames(
        lapply(
          names(spec),
          function(n) {
            sn = spec[[n]]
            vn = vals[[n]]
            if(is.null(vn))
              sn$conversion(sn$default)
            else
              sn$conversion(vn)}),
        names(spec)),
      is.null)}

format.content.type =
  function(content.type) {
    gsub(
      pattern = "^=",
      replacement = "",
      x = paste(names(content.type), content.type, sep = "=", collapse = ";"))}

applyval =
  function(ll, frame)
    lapply(ll, function(x) lapply(x, eval, envir = x, enclos = frame))

path.encoding =
  function(arglist)
    paste(names(arglist), arglist, sep = "/", collapse = "/")

make.web.call =
  function(
    .method =
      c("get", "post", "put", "browse", "delete", "head"),
    .url,
    .parameters,
    .param.encoding = c("query", "path"),
    .headers,
    .body,
    .body.encoding = c("json", "form", "multipart"),
    .body.conversion = identity,
    .init = identity) {
    .method = match.fun(toupper(match.arg(.method)))
    .param.encoding = match.arg(.param.encoding)
    .parameters = applyval(.parameters, parent.frame())
    .headers = applyval(.headers, parent.frame())
    .body = applyval(.body, parent.frame())
    formal.args = discard(c(.parameters, .headers, .body), ~is.null(.$export))
    names(formal.args) = sapply(names(formal.args), function(n) formal.args[[n]]$export(n))
    .web.call = function() {
      args = arglist()
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
              as.list(arg.filler(.parameters, args)),
          add_headers(arg.filler(.headers, args)),
          body = .body.conversion(as.list(arg.filler(.body, args))),
          encode = .body.encoding)
      content(req)}
    formals(.web.call) =
      lapply(
        formal.args,
        function(x){
          if(x$mandatory)
            quote(stop("mandatory argument"))
          else
            x$default})
    .web.call}

