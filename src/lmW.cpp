#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

//' Weighted Least Squares in RcppArmadillo.
//'
//' @param X numeric matrix
//' @param y numeric vector
//' @param w numeric vector
//' @return list
// [[Rcpp::export]]
List lmW(const arma::mat & X, const arma::vec & y, const arma::vec & w) {
  arma::vec coef = arma::solve((X.each_col() % w).t() * X, X.t() * (w % y)); // solve normal equations

  arma::colvec resid = y - X*coef; // find residuals

  return List::create(Named("coefficients") = coef,
                      Named("residuals")    = resid,
                      Named("weights")      = w,
                      Named("rank")         = X.n_cols);
}
