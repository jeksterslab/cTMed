// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-med.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.Med)]]
Rcpp::NumericVector Med(const arma::mat& phi, const double& delta_t,
                        const int& from, const int& to, const arma::vec& med) {
  int ms = med.n_elem;
  int p = phi.n_rows;
  arma::mat total = arma::expmat(delta_t * phi);
  double total_dbl = total(to - 1, from - 1);
  arma::mat d = arma::eye(p, p);
  for (int m = 0; m < ms; m++) {
    d(med[m] - 1, med[m] - 1) = 0;
  }
  arma::mat direct = arma::expmat(delta_t * d * phi * d);
  double direct_dbl = direct(to - 1, from - 1);
  double indirect_dbl = total_dbl - direct_dbl;
  Rcpp::NumericVector output(4);
  output(0) = total_dbl;
  output(1) = direct_dbl;
  output(2) = indirect_dbl;
  output(3) = delta_t;
  return output;
}
