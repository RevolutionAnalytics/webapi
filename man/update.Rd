\name{update}
\alias{update}

\title{
update a policy.
}
\description{A policy may contain internal state that needs to be updated (reference semantics). Use this method to do so}
\usage{
update(policy, ...)
\method{update}{Policy}
\method{update}{Policy.rate.limit}
}
\arguments{
  \item{policy}{A policy to update}
  \item{\dots}{Slots to update in the policy }
}
\details{In the case of Policy.rate.limit, it is used internally by make.web.call to update the time of last access to a resource}
\value{Undefined}
