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



#' Check that a policy allows access to a resource.
#' 
#' @param object A policy to check
#' @param \dots method specific arguments
#' 
#' @return Returns TRUE if policy allows access, FALSE otherwise
#' @export check
check = function(object, ...) UseMethod("evaluate")


#' Enforce an access policy.
#' 
#' The rate limit policy enforces a delay necessary to comply with the rate limit
#' 
#' @param policy A policy to check
#' @inheritParams check
#' @return Undefined
#' @export
enforce = function(policy, ...) UseMethod("enforce")

#' @export
check.Policy = function(policy, ...) TRUE

enforce.Policy = function(policy, ...) NULL


#' Update a policy.
#' 
#' A policy may contain internal state that needs to be updated (reference semantics). Use this method to do so
#' 
#' In the case of \code{\link{Policy.rate.limit}}, it is used internally by \code{\link{make.web.call}} to update the time of last access to a resource
#' 
#' @aliases update.Policy update.Policy.rate.limit
#' 
#' @param object A policy to update
#' @param \dots Slots to update in the policy
#' 
#' @name update
#' @return Undefined
update.Policy =
  function(object, ...)
    lapply(names(list(...)), function(x) object[[x]] = list(...)[[x]])



#' Create a Policy.rate.limit object enforcing the namesake access policy
#' 
#' A rate limit policy prescribes a minimum interval between accesses to a resource
#' 
#' This is used to provide the \code{.policy} argument to \code{\link{make.web.call}}.
#' 
#' @param rate.limit The rate limit, in accesses per second.
#' @param last.access The date and time of the last access. This should always be left at default
#' 
#' @return The newly minted Policy.rate.limit object
#' @export
Policy.rate.limit =
  function(rate.limit, last.access = NULL) {
    pol =
      Policy(
        rate.limit = rate.limit,
        last.access = last.access)
    structure(pol, class = c("Policy.rate.limit", "Policy"))}


#' @export
check.Policy.rate.limit =
  function(policy) {
    elapsed = difftime(Sys.time(), policy$last.access, units = "secs")
    1/policy$rate.limit <= as.numeric(elapsed)}


#' @export
enforce.Policy.rate.limit =
  function(policy, ...) {
    if(!is.null(policy$last.access)) {
      elapsed = difftime(Sys.time(), policy$last.access, units = "secs")
      Sys.sleep(max(1/policy$rate.limit - as.numeric(elapsed), 0))}
    policy}


#' @rdname update
#' @export update.Policy.rate.limit
update.Policy.rate.limit  =
  function(object, ...)
    NextMethod(last.access = Sys.time())

