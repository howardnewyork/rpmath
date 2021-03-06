% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/matrix.R
\name{repMatrix}
\alias{repMatrix}
\title{Duplicates each number in a vector to create a matrix of values}
\usage{
repMatrix(vec, times, ncol = NULL)
}
\arguments{
\item{vec}{A vector of values}

\item{times}{a vector of the number of times to repeat each value.}

\item{ncol}{Default is \code{NULL}, in which case the output matrix will have a number of columns equal to the maximum value in the vector times.  If ncol contains a value, this will determine the number of columns in the output matrix.}
}
\value{
A matrix where each row contains the respective value from vec repeated the respective value in the times vector.  Missing values in the output matrix are set to zero.
The output matrix has nrow=length(vec).  Each item is duplicated up to the lesser of the respectice value in \code{times} and \code{ncol}
}
\description{
Duplicates each number in a vector to create a matrix of values
}
\examples{
vec <- c(1,2,3)
repMatrix(vec = vec, times=c(1,2,3), ncol = 5)

yearsLived <- c(4,2,19, 25)
exposureMatrix <- repMatrix(c(1,1,1,1), times=yearsLived, ncol=20)
}

