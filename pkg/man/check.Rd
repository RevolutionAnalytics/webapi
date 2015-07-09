\name{check}
\alias{check}

\title{
Check that a policy allows access to a resource.
}
\usage{
check(policy, ...)
\method{check}{Policy}
\method{check}{Policy.rate.limit}
}
\arguments{
  \item{policy}{A policy to check}
  \item{\dots}{
method specific arguments }
}
\value{
Returns TRUE if policy allows access, FALSE otherwise}
