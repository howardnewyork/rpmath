% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{raggedExtract}
\alias{raggedExtract}
\title{Extract values from a matrix with uneven column offsets}
\usage{
raggedExtract(mat, position, width, extensionMethod = "none",
  extensionValue = 0)
}
\arguments{
\item{mat}{matrix from which to extract values}

\item{position}{starting position of extraction}

\item{width}{A scalar showing the number of values to extract}

\item{extensionMethod}{Method of extending the mat.  Options are \code{none (default), lastValue, extensionValue}:  the \code{width} must fit within mat; \code{lastValue} mat is extended with the last value of each row; \code{extensionValue} mat is extended with extensionValue}

\item{extensionValue}{used if \code{extensionMethod} is set to \code{extensionValue}}
}
\value{
matrix of extracted values
}
\description{
Extract values from a matrix with uneven column offsets
}
\examples{
a <- matrix(1:81, 3)
raggedExtract(mat=a, position = c(1,2,3), width = 3)
}

