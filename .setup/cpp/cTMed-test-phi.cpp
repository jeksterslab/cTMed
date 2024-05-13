// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-test-phi.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

//' Test the Drift Matrix
//'
//' Both have to be true for the function to return `TRUE`.
//'   - Test that the real part of all eigenvalues of \eqn{\boldsymbol{\Phi}}
//'     are less than zero.
//'   - Test that the diagonal values of \eqn{\boldsymbol{\Phi}}
//'     are between 0 to negative inifinity.
//'
//' @author Ivan Jacob Agaloos Pesigan
//'
//' @param phi Numeric matrix.
//'   The drift matrix (\eqn{\boldsymbol{\Phi}}).
//'
//' @examples
//' phi <- matrix(
//'   data = c(
//'     -0.357, 0.771, -0.450,
//'     0.0, -0.511, 0.729,
//'     0, 0, -0.693
//'   ),
//'   nrow = 3
//' )
//' colnames(phi) <- rownames(phi) <- c("x", "m", "y")
//' TestPhi(phi = phi)
//' phi <- matrix(
//'   data = c(
//'     -6, 5.5, 0, 0,
//'     1.25, -2.5, 5.9, -7.3,
//'     0, 0, -6, 2.5,
//'     5, 0, 0, -6
//'   ),
//'   nrow = 4
//' )
//' colnames(phi) <- rownames(phi) <- paste0("y", 1:4)
//' TestPhi(phi = phi)
//'
//' @family Continuous Time Mediation Functions
//' @keywords cTMed test
//' @export
// [[Rcpp::export]]
bool TestPhi(const arma::mat& phi) {
  arma::vec phi_diag = phi.diag(0);
  arma::cx_vec eigenvalues_phi = arma::eig_gen(phi);
  return arma::all(arma::real(eigenvalues_phi) < 0) && arma::all(phi_diag <= 0);
}
