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
              vn = vals[[n]]
              if(is.null(vn))
                sn$conversion(sn$default)
              else
                sn$conversion(vn)}),
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
    .parameters,
    .param.encoding = c("query", "path"),
    .headers,
    .body,
    .body.encoding = c("json", "form", "multipart"),
    .init = identity) {
    .method = get(toupper(match.arg(.method)), envir = environment(httr::POST))
    .param.encoding = match.arg(.param.encoding)
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
    names(formal.args) = sapply(names(formal.args), function(n) formal.args[[n]]$export(n))
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
      content(req, "text")}
    formals(.web.call) =
      lapply(
        formal.args,
        function(x){
          if(x$mandatory)
            quote(stop("mandatory argument"))
          else
            x$default})
    .web.call}

