// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-indirect-d-t.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.Indirect)]]
double Indirect(const arma::mat& phi, const double& delta_t, const int& from, const int& to, const arma::vec& med) {
  int p = phi.n_rows;
  // total effect
  arma::mat total = arma::expmat(delta_t * phi);
  double total_dbl = total(to - 1, from - 1);
  // direct effect
  arma::mat d = arma::eye(p, p);
  int m = med.n_elem;
  for (int i = 0; i < m; i++) {
    d(med[i] - 1, med[i] - 1) = 0;
  }
  arma::mat direct = arma::expmat(delta_t * d * phi * d);
  double direct_dbl = direct(to - 1, from - 1);
  // indirect effect
  double indirect_dbl = total_dbl - direct_dbl;
  return indirect_dbl;
}
