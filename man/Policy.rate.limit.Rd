\name{Policy.rate.limit}
\alias{Policy.rate.limit}

\title{
Create a Policy.rate.limit object enforcing the namesake access policy}
\description{
A rate limit policy prescribes a minimum interval between accesses to a resource
}
\usage{
Policy.rate.limit(rate.limit, last.access = NULL)
}
\arguments{
  \item{rate.limit}{
The rate limit, in accesses per second.
}
  \item{last.access}{
The date and time of the last access. This should always be left at default}
}
\details{
This is used to provide the .policy argument to make.web.call.
}
\value{
The newly minted Policy.rate.limit object}
