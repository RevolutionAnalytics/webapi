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


Policy =
  function(...) {
    ne = list2env(list(...))
    structure(ne, class = "Policy")}

check = function(policy, ...) UseMethod("evaluate")
enforce = function(policy, ...) UseMethod("enforce")

check.Policy = function(policy, ...) TRUE
enforce.Policy = function(policy, ...) NULL
update.Policy =
  function(policy, ...)
    lapply(names(list(...)), function(x) policy[[x]] = list(...)[[x]])

Policy.rate.limit =
  function(rate.limit, last.access = NULL) {
    pol =
      Policy(
        rate.limit = rate.limit,
        last.access = last.access)
    structure(pol, class = c("Policy.rate.limit", "Policy"))}

check.Policy.rate.limit =
  function(policy) {
    elapsed = difftime(Sys.time(), policy$last.access, units = "secs")
    1/policy$rate.limit <= as.numeric(elapsed)}

enforce.Policy.rate.limit =
  function(policy) {
    if(!is.null(policy$last.access)) {
      elapsed = difftime(Sys.time(), policy$last.access, units = "secs")
      Sys.sleep(max(1/policy$rate.limit - as.numeric(elapsed), 0))}
    policy}

update.Policy.rate.limit  =
  function(policy)
    NextMethod(last.access = Sys.time())