// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-mc-phi-i.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.MCPhiI)]]
arma::mat MCPhiI(const arma::mat& phi, const arma::mat& vcov_phi_vec_l, bool test_phi = true) {
  int p = phi.n_rows;
  int q = p * p;
  arma::mat phi_i = arma::mat(p, p);
  arma::vec phi_vec = arma::vectorise(phi);
  bool run = true;
  while (run) {
    // generate data
    arma::vec phi_vec_i = phi_vec + (vcov_phi_vec_l * arma::randn(q));
    phi_i = arma::reshape(phi_vec_i, p, p);
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
