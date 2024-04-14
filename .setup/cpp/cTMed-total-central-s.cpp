// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-total-central-s.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.TotalCentrals)]]
arma::mat TotalCentrals(const arma::mat& phi, const arma::vec& delta_t) {
  int ts = delta_t.n_rows;
  int p = phi.n_rows;
  arma::mat output = arma::mat(p, ts);
  for (int t = 0; t < ts; t++) {
    arma::mat total = arma::expmat(delta_t[t] * phi);
    output.col(t) = arma::vectorise(arma::sum(total, 0) - total.diag().t());
  }
  return output.t();
}
