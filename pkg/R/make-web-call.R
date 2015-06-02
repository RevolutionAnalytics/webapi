
arglist =
function() {
  fun = sys.function(-1)
  fa = formals(fun)
  ma = as.list(match.call(fun, sys.call(-1)))[-1]
  sapply(names(ma), function(n) fa[n] <<- ma[n])
  fa}

arg.filler =
function(vec, vals){
  filled = lapply(vals[names(vec)], function(x) eval.parent(x, n = 2))
  filled = filled[!sapply(filled, is.null)]
  setNames(as.character(filled), names(filled))}

qw = function(x) strsplit(x = x, split = " +")[[1]]

make.web.call =
function(
  .method =
    qw("get post put browse delete head"),
  .url,
  .parameters,
  .headers,
  .body,
  .body.encoding = "json") {
  .method = match.fun(toupper(match.arg(.method)))
  web.call = function() {
    args = arglist()
    req =
      .method(
        url = .url,
        query = as.list(arg.filler(.parameters, args)),
        add_headers(arg.filler(.headers, args)),
        body = arg.filler(.body, args),
        encode = .body.encoding)
    content(req)}
  formals(web.call) = c(.parameters, .headers, .body)
  web.call}

