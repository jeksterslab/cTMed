// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-mc-phi.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.MCPhi)]]
Rcpp::List MCPhi(const arma::mat& phi, const arma::mat& vcov_phi_vec_l,
                 const int& R, bool test_phi = true) {
  Rcpp::List output(R);
  int p = phi.n_rows;
  int q = p * p;
  arma::vec phi_vec = arma::vectorise(phi);
  for (int i = 0; i < R; i++) {
    bool run = true;
    while (run) {
      // generate data
      arma::vec phi_vec_i = phi_vec + (vcov_phi_vec_l * arma::randn(q));
      arma::mat phi_i = arma::reshape(phi_vec_i, p, p);
      // test phi
      if (test_phi) {
        if (TestPhi(phi_i)) {
          run = false;
        }
      } else {
        run = false;
      }
      // output
      if (!run) {
        output[i] = phi_i;
      }
    }
  }
  return output;
}
