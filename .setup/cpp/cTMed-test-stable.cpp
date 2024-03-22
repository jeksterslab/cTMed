// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-test-stable.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

//' Test Stability
//'
//' The function computes the eigenvalues of the input matrix `x`.
//' It checks if the real part of all eigenvalues is negative.
//' If all eigenvalues have negative real parts,
//' the system is considered stable.
//'
//' @author Ivan Jacob Agaloos Pesigan
//'
//' @param x Numeric matrix.
//'
//' @examples
//' x <- matrix(
//'   data = c(
//'     -0.357, 0.771, -0.450,
//'     0.0, -0.511, 0.729,
//'     0, 0, -0.693
//'   ),
//'   nrow = 3
//' )
//' TestStable(x)
//' x <- matrix(
//'   data = c(
//'     -6, 5.5, 0, 0,
//'     1.25, -2.5, 5.9, -7.3,
//'     0, 0, -6, 2.5,
//'     5, 0, 0, -6
//'   ),
//'   nrow = 4
//' )
//' TestStable(x)
//'
//' @family Continuous Time Mediation Functions
//' @keywords cTMed test
//' @export
// [[Rcpp::export]]
bool TestStable(const arma::mat& x) {
  arma::cx_vec eigenvalues = arma::eig_gen(x);
  return arma::all(arma::real(eigenvalues) < 0);
}
