// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-mc-med-d-t.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.MCMed)]]
arma::mat MCMed(const arma::mat& phi, const arma::mat& vcov_phi_vec_l,
                const double& delta_t, const int& from, const int& to,
                const arma::vec& med, const int& R, const double& tol,
                bool test_phi = true) {
  arma::mat output(R, 4);
  int p = phi.n_rows;
  int q = p * p;
  arma::vec phi_vec = arma::vectorise(phi);
  // needed for direct effect
  arma::mat d = arma::eye(p, p);
  int m = med.n_elem;
  for (int j = 0; j < m; j++) {
    d(med[j] - 1, med[j] - 1) = 0;
  }
  for (int i = 0; i < R; i++) {
    bool run = true;
    int iter = 0;
    while (run) {
      // generate data
      arma::vec phi_vec_i = phi_vec + (vcov_phi_vec_l * arma::randn(q));
      arma::mat phi_i = arma::reshape(phi_vec_i, p, p);
      // Iterate over the diagonal elements
      for (int i = 0; i < p; ++i) {
        double diag_value = phi_i(i, i);
        if (std::abs(diag_value) < tol) {
          // Replace diagonal element with zero
          phi_i(i, i) = 0.0;
        }
      }
      // test phi
      if (test_phi) {
        iter += 1;
        if (iter > 1000000) {
          Rcpp::stop("Max iterations reached.");
        }
        if (TestPhi(phi_i)) {
          run = false;
        }
      } else {
        run = false;
      }
      // output
      if (!run) {
        // total effect
        arma::mat total = arma::mat(p, p);
        total = arma::expmat(delta_t * phi_i);
        double total_dbl = total(to - 1, from - 1);
        // direct effect
        arma::mat direct = arma::mat(p, p);
        direct = arma::expmat(delta_t * d * phi_i * d);
        double direct_dbl = direct(to - 1, from - 1);
        // indirect effect
        double indirect_dbl = total_dbl - direct_dbl;
        // output
        output(i, 0) = total_dbl;
        output(i, 1) = direct_dbl;
        output(i, 2) = indirect_dbl;
        output(i, 3) = delta_t;
      }
    }
  }
  return output;
}
