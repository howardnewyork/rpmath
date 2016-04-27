// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// cumSumRow
NumericMatrix cumSumRow(NumericMatrix x);
RcppExport SEXP luxmatrix_cumSumRow(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    __result = Rcpp::wrap(cumSumRow(x));
    return __result;
END_RCPP
}
// cumSumCol
NumericMatrix cumSumCol(NumericMatrix x);
RcppExport SEXP luxmatrix_cumSumCol(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    __result = Rcpp::wrap(cumSumCol(x));
    return __result;
END_RCPP
}
// sumColLimited
NumericVector sumColLimited(NumericMatrix x, NumericVector limits);
RcppExport SEXP luxmatrix_sumColLimited(SEXP xSEXP, SEXP limitsSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type limits(limitsSEXP);
    __result = Rcpp::wrap(sumColLimited(x, limits));
    return __result;
END_RCPP
}
// sumRowLimited
NumericVector sumRowLimited(NumericMatrix x, NumericVector limits);
RcppExport SEXP luxmatrix_sumRowLimited(SEXP xSEXP, SEXP limitsSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type limits(limitsSEXP);
    __result = Rcpp::wrap(sumRowLimited(x, limits));
    return __result;
END_RCPP
}
// prodRow
NumericVector prodRow(NumericMatrix x);
RcppExport SEXP luxmatrix_prodRow(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    __result = Rcpp::wrap(prodRow(x));
    return __result;
END_RCPP
}
// prodCol
NumericVector prodCol(NumericMatrix x);
RcppExport SEXP luxmatrix_prodCol(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    __result = Rcpp::wrap(prodCol(x));
    return __result;
END_RCPP
}
// cumProdRow
NumericMatrix cumProdRow(NumericMatrix x);
RcppExport SEXP luxmatrix_cumProdRow(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    __result = Rcpp::wrap(cumProdRow(x));
    return __result;
END_RCPP
}
// cumProdCol
NumericMatrix cumProdCol(NumericMatrix x);
RcppExport SEXP luxmatrix_cumProdCol(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    __result = Rcpp::wrap(cumProdCol(x));
    return __result;
END_RCPP
}
// prodRowLimited
NumericVector prodRowLimited(NumericMatrix x, NumericVector limits);
RcppExport SEXP luxmatrix_prodRowLimited(SEXP xSEXP, SEXP limitsSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type limits(limitsSEXP);
    __result = Rcpp::wrap(prodRowLimited(x, limits));
    return __result;
END_RCPP
}
// prodColLimited
NumericVector prodColLimited(NumericMatrix x, NumericVector limits);
RcppExport SEXP luxmatrix_prodColLimited(SEXP xSEXP, SEXP limitsSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type limits(limitsSEXP);
    __result = Rcpp::wrap(prodColLimited(x, limits));
    return __result;
END_RCPP
}
// raggedExtract
NumericMatrix raggedExtract(NumericMatrix mat, IntegerVector position, int width, String extensionMethod, double extensionValue);
RcppExport SEXP luxmatrix_raggedExtract(SEXP matSEXP, SEXP positionSEXP, SEXP widthSEXP, SEXP extensionMethodSEXP, SEXP extensionValueSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericMatrix >::type mat(matSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type position(positionSEXP);
    Rcpp::traits::input_parameter< int >::type width(widthSEXP);
    Rcpp::traits::input_parameter< String >::type extensionMethod(extensionMethodSEXP);
    Rcpp::traits::input_parameter< double >::type extensionValue(extensionValueSEXP);
    __result = Rcpp::wrap(raggedExtract(mat, position, width, extensionMethod, extensionValue));
    return __result;
END_RCPP
}
// repMatrixCpp
NumericMatrix repMatrixCpp(NumericVector vec, IntegerVector times, int ncol);
RcppExport SEXP luxmatrix_repMatrixCpp(SEXP vecSEXP, SEXP timesSEXP, SEXP ncolSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type vec(vecSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type times(timesSEXP);
    Rcpp::traits::input_parameter< int >::type ncol(ncolSEXP);
    __result = Rcpp::wrap(repMatrixCpp(vec, times, ncol));
    return __result;
END_RCPP
}
