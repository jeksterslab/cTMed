// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-indirect-central-s.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.IndirectCentrals)]]
arma::mat IndirectCentrals(const arma::mat& phi, const arma::vec& delta_t) {
  int ts = delta_t.n_rows;
  int p = phi.n_rows;
  arma::mat output = arma::mat(p, ts);
  for (int t = 0; t < ts; t++) {
    arma::mat total = arma::expmat(delta_t[t] * phi);
    for (int m = 0; m < p; m++) {
      arma::mat d = arma::eye(p, p);
      d(m, m) = 0;
      arma::mat direct = arma::expmat(delta_t[t] * d * phi * d);
      for (int i = 0; i < p; i++) {
        for (int j = 0; j < p; j++) {
          if (!(m == i || m == j || i == j)) {
            output(m, t) += total(i, j) - direct(i, j);
          }
        }
      }
    }
  }
  return output.t();
}
