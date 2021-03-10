// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// lmW
List lmW(const arma::mat& X, const arma::vec& y, const arma::vec& w);
RcppExport SEXP _blblm_lmW(SEXP XSEXP, SEXP ySEXP, SEXP wSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type X(XSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type y(ySEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type w(wSEXP);
    rcpp_result_gen = Rcpp::wrap(lmW(X, y, w));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_blblm_lmW", (DL_FUNC) &_blblm_lmW, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_blblm(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
