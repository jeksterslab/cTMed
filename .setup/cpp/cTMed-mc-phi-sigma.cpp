// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-mc-phi-sigma.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export(.MCPhiSigma)]]
Rcpp::List MCPhiSigma(const arma::vec& theta, const arma::mat& vcov_theta,
                      const arma::uword& R, bool test_phi = true,
                      bool diag_sigma = false) {
  Rcpp::List output(R);
  arma::uword n = theta.n_elem;
  arma::uword p;
  arma::uword q;
  if (diag_sigma) {
    p = (-1 + std::sqrt(1 + 4 * n)) / 2;
    q = p;
  } else {
    p = (-1 + std::sqrt(1 + 24 * n)) / 6;
    q = (p * (p + 1)) / 2;
  }
  arma::uword index = 0;
  arma::vec v_i(n, arma::fill::none);
  arma::mat phi_i(p, p, arma::fill::none);
  arma::vec phi_vec_i(p * p, arma::fill::none);
  arma::mat sigma_i(p, p, arma::fill::zeros);
  arma::vec sigma_vec_i(q, arma::fill::none);
  arma::vec eigval;
  arma::mat eigvec;
  for (arma::uword r = 0; r < R; ++r) {
    bool run = true;
    Rcpp::List output_i(2);
    while (run) {
      // generate data
      v_i = arma::mvnrnd(theta, vcov_theta);
      phi_vec_i = v_i(arma::span(0, (p * p) - 1));
      sigma_vec_i = v_i(arma::span(p * p, n - 1));
      // test phi
      phi_i = arma::reshape(phi_vec_i, p, p);
      if (test_phi) {
        if (TestPhi(phi_i)) {
          run = false;
        }
      } else {
        run = false;
      }
      if (run == false) {
        if (diag_sigma) {
          sigma_i.zeros();
          for (arma::uword j = 0; j < p; ++j) {
            sigma_i(j, j) = std::max(sigma_vec_i(j), 1e-8);
          }
        } else {
          // test sigma
          index = 0;
          for (arma::uword j = 0; j < p; ++j) {
            for (arma::uword k = j; k < p; ++k) {
              sigma_i(j, k) = sigma_vec_i(index);
              sigma_i(k, j) = sigma_vec_i(index);
              index++;
            }
          }
          arma::eig_sym(eigval, eigvec, sigma_i);
          eigval.transform([](double val) { return std::max(val, 1e-8); });
          sigma_i = eigvec * arma::diagmat(eigval) * eigvec.t();
        }
      }
    }
    output_i[0] = phi_i;
    output_i[1] = sigma_i;
    output[r] = output_i;
  }
  return output;
}
