\name{format.content.type}
\alias{format.content.type}
\title{
Performs the specific encoding required by the \code{Content-type} http header.
}
\description{
Implements the typical encoding for \code{Content-type}, which is semicolon separated values or key-value pairs, where the pairs are slash-separated.
}
\usage{
format.content.type(content.type)
}
\arguments{
  \item{content.type}{Description of content type as a list}
}
\details{
This function should only be used as a conversion argument when specifyng the \code{Content-type} header.
}
\value{
A formatted \code{Content-type} string.
}

