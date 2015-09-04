\name{enforce}
\alias{enforce}

\title{
Enforce an access policy.
}
\description{
Enforce an access policy.
}

\usage{
enforce(policy, ...)
\method{enforce}{Policy}(policy, \dots)
\method{enforce}{Policy.rate.limit}(policy, \dots)
}
\arguments{
  \item{policy}{A policy to enforce}
  \item{\dots}{
method specific arguments }
}
\details{
The rate limit policy enforces a delay necessary to comply with the rate limit
}
\value{Undefined}
