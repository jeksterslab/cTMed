// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-test-phi.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.TestPhi)]]
bool TestPhi(const arma::mat& phi) {
  arma::vec phi_diag = phi.diag(0);
  arma::cx_vec eigenvalues_phi = arma::eig_gen(phi);
  return arma::all(arma::real(eigenvalues_phi) < 0) && arma::all(phi_diag <= 0);
}
