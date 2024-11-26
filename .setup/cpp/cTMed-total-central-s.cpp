// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-total-central-s.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.TotalCentrals)]]
arma::mat TotalCentrals(const arma::mat& phi, const arma::vec& delta_t) {
  arma::mat output = arma::mat(phi.n_rows, delta_t.n_elem);
  arma::mat total(phi.n_rows, phi.n_cols, arma::fill::none);
  for (arma::uword t = 0; t < delta_t.n_elem; t++) {
    total = arma::expmat(delta_t[t] * phi);
    output.col(t) = arma::vectorise(arma::sum(total, 0) - total.diag().t());
  }
  return output.t();
}
