\name{arg.spec}
\alias{arg.spec}
\alias{a}
\title{
Function to define arguments for a web API call
}
\description{
Allows to define each component of a web API call and to map it to an argument of an R call
}
\usage{
arg.spec(
  default = NULL,
  type = class(default),
  mandatory = FALSE,
  validate = function(x) is.null(type) || class(x) \%in\% type,
  conversion = as.character,
  export = identity)
}
\arguments{
  \item{default}{
  The default value for this argument
}
  \item{type}{
  The desired type (currently unenforced)
}
  \item{mandatory}{
Whether this argument is mandatory.
}
  \item{validate}{
Validation function for the value of an argument (currently unused)
}
  \item{conversion}{
The conversion function to be applied to an R value to obtain and appropriate encoding for the web call.
}
  \item{export}{
Whether this argument should aslo be an argument to the corresponding R call (some web API call take a conventional value, in other cases it can be computed from the other arguments, or it is a random id)
}
}
\details{
Used exclusively to define arguments to the function \code{make.web.call}
}
\value{
A list containing the information needed by \code{make.web.call}
}
