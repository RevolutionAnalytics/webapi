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