#### R + Math
#### Version 0.1.0



#' @import dplyr
NULL


#' @useDynLib rpmath
#' @importFrom Rcpp sourceCpp evalCpp
NULL

#' Remove rows from database where any row contains an NA
#'
#' @param data Data frame
#' @export
removeRowsNA <- function(data){
  naRows <- !apply(data, MARGIN = 2, is.na)
  naRows <- apply(naRows, MARGIN = 1, all)
  data[naRows,]
}


#' Remove set a of named columns from a data frame or matrix
#'
#' @param data data frame or matrix
#' @param cols Vector containing the names of columns
#' @return A data frame or matrix with the designatd columsn removed
#' @export
removeCols <- function(data, cols){
  data[,cols] <- NULL
  data
}


#' repeats each column of a matrix a specified number of times
#'
#' @param x Matrix
#' @param each Scalar or a vector.  This represents the number of times each column is repeated.  If \code{each} is a vector, then it must has the same length as the number of columns in x
#' @return A matrix with each column repeated \code{each} times
#' a<- matrix(1:12, 3)
#' repCols(a, c(2,3,4, 5))
#' @export
repCols <- function(x, each){

  if (length(each) == 1) each <- rep(each, ncol(x))
  totalCols <- sum(each)

  xNew <- matrix(NA, nrow(x), totalCols)

  eachCum <- c(0,cumsum(each))
  for (i in 1:ncol(x)){
    xNew[,(eachCum[i]+1):eachCum[i+1]] <- x[,rep(i, times = each[i])]
  }
  xNew
}


#' repeats each row of a matrix a specified number of times
#'
#' @param x Matrix
#' @param each Scalar or a vector.  This represents the number of times each row is repeated.  If \code{each} is a vector, then it must has the same length as the number of rows in x
#' @return A matrix with each row repeated \code{each} times
#' @examples
#' a<- matrix(1:12, 3)
#' repRows(a, c(2,3,4))
#' @export
repRows <- function(x, each){

  if (length(each) == 1) each <- rep(each, nrow(x))
  totalRows <- sum(each)

  xNew <- matrix(NA,  totalRows, ncol(x))

  eachCum <- c(0,cumsum(each))
  for (i in 1:nrow(x)){
    xNew[(eachCum[i]+1):eachCum[i+1],] <- x[rep(i, times = each[i]),]
  }
  xNew
}


#' Duplicates each number in a vector to create a matrix of values
#'
#'
#' @param vec A vector of values
#' @param times a vector of the number of times to repeat each value.
#' @param ncol Default is \code{NULL}, in which case the output matrix will have a number of columns equal to the maximum value in the vector times.  If ncol contains a value, this will determine the number of columns in the output matrix.
#' @return A matrix where each row contains the respective value from vec repeated the respective value in the times vector.  Missing values in the output matrix are set to zero.
#' The output matrix has nrow=length(vec).  Each item is duplicated up to the lesser of the respectice value in \code{times} and \code{ncol}
#' @examples
#' vec <- c(1,2,3)
#' repMatrix(vec = vec, times=c(1,2,3), ncol = 5)
#'
#' yearsLived <- c(4,2,19, 25)
#' exposureMatrix <- repMatrix(c(1,1,1,1), times=yearsLived, ncol=20)
#' @export
repMatrix <- function(vec, times, ncol = NULL) {
  if (is.null(ncol)) ncol <- max(times)
  ans <- repMatrixCpp(vec, times, ncol)
  ans
}



