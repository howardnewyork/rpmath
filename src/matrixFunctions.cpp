
#include <Rcpp.h>
using namespace Rcpp;


//' Calculate the cumulative sum of each row in a matrix
//'
//' @param x A numeric matrix
//' @return A matrix containing the cumulative sum of each row of the matrix \code{x}
//' @examples
//' a <- matrix(1:9, 3)
//' cumSumRow(a)
//' @export
// [[Rcpp::export]]
NumericMatrix cumSumRow (NumericMatrix x) {
  int m = x.nrow(), n= x.ncol();

  NumericMatrix ans(m,n);
  for (int i = 0; i < m; i++) {
    for (int j = 0; j < n; j++) {
      ans(i,j) = x(i,j);
      if (j>0) {
        ans(i,j) += ans(i,j-1);
      }
    }
  }
  return ans;
}




//' Calculate the cumulative sum of each column in a matrix
//' @param x A numeric matrix
//' @return A matrix containing the cumulative product of each column of the matrix \code{x}
//' @examples
//' a <- matrix(1:9, 3)
//' cumSumCol(a)
//' @export
// [[Rcpp::export]]
NumericMatrix cumSumCol (NumericMatrix x) {
  int m = x.nrow(), n= x.ncol();

  NumericMatrix ans(n,m);
  for (int i = 0; i < n; i++) {
    ans(0,i) = 0;
    for (int j = 0; j < m; j++) {
      ans(j,i) = x(j,i);
      if (j>0) ans(j,i) += ans(j-1,i);
    }
  }
  return ans;
}



//' Calculate the limited cumulative sum of each column in a matrix
//' @param x A numeric matrix
//' @param limits A vector showing the number of values to be added in each column, starting at the left side
//' @return A matrix containing the cumulative product of each column of the matrix \code{x}, with limited values in each row used
//' @examples
//' a <- matrix(1:9, 3)
//' sumColLimited(a, limits = c(1,2,3))
//' @export
// [[Rcpp::export]]
NumericVector sumColLimited (NumericMatrix x, NumericVector limits) {
  int n = x.ncol();

  NumericVector ans(n);
  for (int i = 0; i < n; i++) {
    ans[i] = 0;
    for (int j = 0; j < limits(i); j++) {
      ans[i] += x(j,i);
    }
  }
  return ans;
}

//' Calculate the limited cumulative sum of each row in a matrix
//' @param x A numeric matrix
//' @param limits A vector showing the number of values to be added in each column, starting at the left side
//' @return A matrix containing the cumulative product of each column of the matrix \code{x}, with limited values in each row used
//' @examples
//' a <- matrix(1:9, 3)
//' sumRowLimited(a, limits = c(1,2,3))
//' @export
// [[Rcpp::export]]
NumericVector sumRowLimited (NumericMatrix x, NumericVector limits) {
  int n = x.nrow();

  NumericVector ans(n);
  for (int i = 0; i < n; i++) {
    ans[i] = 0;
    for (int j = 0; j < limits(i); j++) {
      ans[i] += x(i,j);
    }
  }
  return ans;
}



//' Calculate the product of each row in a matrix
//'
//' @param x A numeric matrix
//' @return A vector containing the product of each row of the matrix \code{x}
//' @examples
//' a <- matrix(1:9, 3)
//' prodRow(a)
//' @export
// [[Rcpp::export]]
NumericVector prodRow (NumericMatrix x) {
  int m = x.nrow(), n= x.ncol();

  NumericVector ans(m);
  for (int i = 0; i < m; i++) {
    ans[i] = 1;
    for (int j = 0; j < n; j++) {
      ans[i] *= x(i,j);
    }
  }
return ans;
}

//' Calculate the product of each column in a matrix
//'
//' @param x A numeric matrix
//' @return A vector containing the product of each column of the matrix \code{x}
//' @examples
//' a <- matrix(1:9, 3)
//' prodCol(a)
//' @export
// [[Rcpp::export]]
NumericVector prodCol (NumericMatrix x) {
  int m = x.nrow(), n= x.ncol();

  NumericVector ans(n);
  for (int i = 0; i < n; i++) {
    ans[i] = 1;
    for (int j = 0; j < m; j++) {
      ans[i] *= x(j,i);
    }
  }
  return ans;
}


//' Calculate the cumulative product of each row in a matrix
//'
//' @param x A numeric matrix
//' @return A matrix containing the cumulative product of each row of the matrix \code{x}
//' @examples
//' a <- matrix(1:9, 3)
//' cumProdRow(a)
//' @export
// [[Rcpp::export]]
NumericMatrix cumProdRow (NumericMatrix x) {
  int m = x.nrow(), n= x.ncol();

  NumericMatrix ans(m,n);
  for (int i = 0; i < m; i++) {
    for (int j = 0; j < n; j++) {
      ans(i,j) = x(i,j);
      if (j>0) {
        ans(i,j) *= ans(i,j-1);
      }
    }
  }
  return ans;
}


//' Calculate the cumulative product of each column in a matrix
//' @param x A numeric matrix
//' @return A matrix containing the cumulative product of each column of the matrix \code{x}
//' @examples
//' a <- matrix(1:9, 3)
//' cumProdCol(a)
//' @export
// [[Rcpp::export]]
NumericMatrix cumProdCol (NumericMatrix x) {
  int m = x.nrow(), n= x.ncol();

  NumericMatrix ans(n,m);
  for (int i = 0; i < n; i++) {
    ans(0,i) = 1;
    for (int j = 0; j < m; j++) {
      ans(j,i) = x(j,i);
      if (j>0) ans(j,i) *= ans(j-1,i);
    }
  }
  return ans;
}



//' Calculate the limited cumulative product of each row in a matrix.
//' @param x A numeric matrix
//' @param limits A vector showing the number of values to be multiplied in each row, starting at the left side
//' @return A matrix containing the cumulative product of each row of the matrix \code{x}, with limited values in each row used
//' @examples
//' a <- matrix(1:9, 3)
//' prodRowLimited(a, c(1,2,3))
//' @export
// [[Rcpp::export]]
NumericVector prodRowLimited (NumericMatrix x, NumericVector limits) {
  int m = x.nrow();

  NumericVector ans(m);
  for (int i = 0; i < m; i++) {
    ans[i] = 1;
    for (int j = 0; j < limits(i); j++) {
      ans[i] *= x(i,j);
    }
  }
  return ans;

}

//' Calculate the limited cumulative product of each column in a matrix
//' @param x A numeric matrix
//' @param limits A vector showing the number of values to be multiplied in each column, starting at the left side
//' @return A matrix containing the cumulative product of each column of the matrix \code{x}, with limited values in each row used
//' @examples
//' a <- matrix(1:9, 3)
//' prodColLimited(a, limits = c(1,2,3))
//' @export
// [[Rcpp::export]]
NumericVector prodColLimited (NumericMatrix x, NumericVector limits) {
  int n = x.ncol();

  NumericVector ans(n);
  for (int i = 0; i < n; i++) {
    ans[i] = 1;
    for (int j = 0; j < limits(i); j++) {
      ans[i] *= x(j,i);
    }
  }
  return ans;
}



//' Extract values from a matrix with uneven column offsets
//'
//' @param mat matrix from which to extract values
//' @param position starting position of extraction
//' @param width A scalar showing the number of values to extract
//' @param extensionMethod Method of extending the mat.  Options are \code{none (default), lastValue, extensionValue}:  the \code{width} must fit within mat; \code{lastValue} mat is extended with the last value of each row; \code{extensionValue} mat is extended with extensionValue
//' @param extensionValue used if \code{extensionMethod} is set to \code{extensionValue}
//' @return matrix of extracted values
//' @examples
//' a <- matrix(1:81, 3)
//' raggedExtract(mat=a, position = c(1,2,3), width = 3)
//' @export
// [[Rcpp::export]]
NumericMatrix raggedExtract(NumericMatrix mat, IntegerVector position, int width, String extensionMethod = "none", double extensionValue = 0) {
  int n = mat.nrow(), np = position.size(), maxCols = mat.ncol();
  NumericMatrix ragged(n, width);
  if ((extensionMethod != "none") & (extensionMethod != "lastValue") & (extensionMethod != "extensionValue")){
    throw std::range_error("extension method must be one of: none, lastValue or extensionValue");
  }
  if (n != np) {
    throw std::range_error("Number of rows of mat and length of position do not match");
  }

  if ((maxCols < width + max(position)-1) & (extensionMethod == "none")) {
    throw std::range_error("width size is too large for mat and position when using extensionMethod = 'none' ");
  }

  for (int i = 0; i < n; i++) {
    for (int j = 0; j < width; j++){
      if (maxCols > (j + position(i)-1)) {
        ragged(i,j) = mat(i, position(i)+ j -1);
      } else if (extensionMethod == "lastValue"){
        ragged(i,j) = mat(i, maxCols-1);
      } else {
        ragged(i,j) = extensionValue;
      }
    }
  }
  return ragged;
}



// Duplicates each number in a vector to create a matrix of values
//
// see \code{repMatrix} for details
// [[Rcpp::export]]
NumericMatrix repMatrixCpp(NumericVector vec, IntegerVector times, int ncol) {
  int n = vec.size();
  NumericMatrix out(n, ncol);

  for (int i = 0; i < n; i++) {
    for (int j = 0; j < std::min(ncol, times(i)); j++){
      out(i,j) = vec(i);
    }
  }
  return out;
}



