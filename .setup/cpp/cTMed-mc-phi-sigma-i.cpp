// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-mc-phi-sigma-i.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.MCPhiSigmaI)]]
Rcpp::List MCPhiSigmaI(const arma::vec& theta, const arma::mat& vcov_theta,
                       bool test_phi = true) {
  Rcpp::List output(2);
  arma::uword n = theta.n_elem;
  arma::uword p = (-1 + std::sqrt(1 + 24 * n)) / 6;
  arma::uword q = (p * (p + 1)) / 2;
  arma::uword index = 0;
  arma::vec v_i(n, arma::fill::none);
  arma::mat phi_i(p, p, arma::fill::none);
  arma::vec phi_vec_i(p * p, arma::fill::none);
  arma::mat sigma_i(p, p, arma::fill::none);
  arma::vec sigma_vech_i(q, arma::fill::none);
  arma::vec eigval;
  arma::mat eigvec;
  bool run = true;
  while (run) {
    // generate data
    v_i = arma::mvnrnd(theta, vcov_theta);
    phi_vec_i = v_i(arma::span(0, (p * p) - 1));
    sigma_vech_i = v_i(arma::span(p * p, n - 1));
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
      // test sigma
      index = 0;
      for (arma::uword i = 0; i < p; ++i) {
        for (arma::uword j = i; j < p; ++j) {
          sigma_i(i, j) = sigma_vech_i(index);
          sigma_i(j, i) = sigma_vech_i(index);
          index++;
        }
      }
      arma::eig_sym(eigval, eigvec, sigma_i);
      eigval.transform([](double val) { return std::max(val, 1e-8); });
      sigma_i = eigvec * arma::diagmat(eigval) * eigvec.t();
    }
  }
  output[0] = phi_i;
  output[1] = sigma_i;
  return output;
}
