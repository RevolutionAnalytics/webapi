\name{update}
\alias{update.Policy}
\alias{update.Policy.rate.limit}

\title{
update a policy.
}
\description{A policy may contain internal state that needs to be updated (reference semantics). Use this method to do so}
\usage{
\method{update}{Policy}(policy, \dots)
\method{update}{Policy.rate.limit}(\dots)
}
\arguments{
  \item{policy}{A policy to update}
  \item{\dots}{Slots to update in the policy }
}

\details{
In the case of \code{\link{Policy.rate.limit}}, it is used internally by \code{\link{make.web.call}} to update the time of last access to a resource
}
\value{Undefined}
