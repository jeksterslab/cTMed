// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-med-s.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.Meds)]]
arma::mat Meds(const arma::mat& phi, const arma::vec& delta_t,
               const arma::uword& from, const arma::uword& to,
               const arma::vec& med) {
  arma::mat output(delta_t.n_elem, 4, arma::fill::none);
  arma::mat total(phi.n_rows, phi.n_rows, arma::fill::none);
  arma::mat direct(phi.n_rows, phi.n_rows, arma::fill::none);
  arma::mat d = arma::eye(phi.n_rows, phi.n_rows);
  double total_dbl;
  double direct_dbl;
  double indirect_dbl;
  for (arma::uword i = 0; i < med.n_elem; ++i) {
    d(med[i] - 1, med[i] - 1) = 0;
  }
  for (arma::uword t = 0; t < delta_t.n_elem; t++) {
    total = arma::expmat(delta_t[t] * phi);
    total_dbl = total(to - 1, from - 1);
    direct = arma::expmat(delta_t[t] * d * phi * d);
    direct_dbl = direct(to - 1, from - 1);
    indirect_dbl = total_dbl - direct_dbl;
    output(t, 0) = total_dbl;
    output(t, 1) = direct_dbl;
    output(t, 2) = indirect_dbl;
    output(t, 3) = delta_t[t];
  }
  return output;
}
