// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-indirect-central-s.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.IndirectCentrals)]]
arma::mat IndirectCentrals(const arma::mat& phi, const arma::vec& delta_t) {
  arma::mat output(phi.n_rows, delta_t.n_elem, arma::fill::zeros);
  arma::mat total(phi.n_rows, phi.n_cols, arma::fill::none);
  arma::mat direct(phi.n_rows, phi.n_cols, arma::fill::none);
  arma::mat d = arma::eye(phi.n_rows, phi.n_cols);
  for (arma::uword t = 0; t < delta_t.n_elem; t++) {
    total = arma::expmat(delta_t[t] * phi);
    for (arma::uword m = 0; m < phi.n_rows; m++) {
      d = arma::eye(phi.n_rows, phi.n_cols);
      d(m, m) = 0;
      direct = arma::expmat(delta_t[t] * d * phi * d);
      for (arma::uword j = 0; j < phi.n_rows; j++) {
        for (arma::uword i = 0; i < phi.n_rows; i++) {
          if (!(m == i || m == j || i == j)) {
            output(m, t) += total(i, j) - direct(i, j);
          }
        }
      }
    }
  }
  return output.t();
}
