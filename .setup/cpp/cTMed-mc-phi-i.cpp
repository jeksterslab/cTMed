// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-mc-phi-i.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.MCPhiI)]]
arma::mat MCPhiI(const arma::mat& phi, const arma::mat& vcov_phi_vec_l, bool test_phi = true) {
  arma::mat phi_i(phi.n_rows, phi.n_cols, arma::fill::none);
  arma::vec phi_vec = arma::vectorise(phi);
  arma::vec phi_vec_i(phi.n_rows * phi.n_cols, arma::fill::none);
  bool run = true;
  while (run) {
    // generate data
    phi_vec_i = phi_vec + (vcov_phi_vec_l * arma::randn(phi.n_rows * phi.n_cols));
    phi_i = arma::reshape(phi_vec_i, phi.n_rows, phi.n_cols);
    // test phi
    if (test_phi) {
      if (TestPhi(phi_i)) {
        run = false;
      }
    } else {
      run = false;
    }
  }
  return phi_i;
}
