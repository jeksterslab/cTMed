// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-ratio-vec.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.RatioVec)]]
Rcpp::NumericVector RatioVec(const arma::vec& phi_vec, const double& delta_t,
                             const int& from, const int& to,
                             const arma::vec& med) {
  int ms = med.n_elem;
  int p = std::sqrt(phi_vec.n_elem);
  arma::mat phi = arma::reshape(phi_vec, p, p);
  arma::mat total = arma::expmat(delta_t * phi);
  double total_dbl = total(to - 1, from - 1);
  arma::mat d = arma::eye(p, p);
  for (int m = 0; m < ms; m++) {
    d(med[m] - 1, med[m] - 1) = 0;
  }
  arma::mat direct = arma::expmat(delta_t * d * phi * d);
  double direct_dbl = direct(to - 1, from - 1);
  double indirect_dbl = total_dbl - direct_dbl;
  Rcpp::NumericVector output(2);
  output(0) = direct_dbl / total_dbl;
  output(1) = indirect_dbl / total_dbl;
  return output;
}
