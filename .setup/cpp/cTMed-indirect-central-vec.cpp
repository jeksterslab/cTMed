// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-indirect-central-vec.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.IndirectCentralVec)]]
Rcpp::NumericVector IndirectCentralVec(const arma::vec& phi_vec, const double& delta_t) {
  int p = std::sqrt(phi_vec.n_elem);
  arma::mat phi = arma::reshape(phi_vec, p, p);
  arma::mat total = arma::expmat(delta_t * phi);
  Rcpp::NumericVector output(p);
  for (int m = 0; m < p; m++) {
    arma::mat d = arma::eye(p, p);
    d(m, m) = 0;
    arma::mat direct = arma::expmat(delta_t * d * phi * d);
    for (int i = 0; i < p; i++) {
      for (int j = 0; j < p; j++) {
        if (!(m == i || m == j || i == j)) {
          output(m) += total(i, j) - direct(i, j);
        }
      }
    }
  } 
  return output;
}
