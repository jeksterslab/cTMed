// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-mc-phi-sigma-i.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.MCPhiSigmaI)]]
Rcpp::List MCPhiSigmaI(const arma::mat& phi, const arma::mat& vcov_phi_vec,
                       const arma::mat& sigma, const arma::mat& vcov_sigma_vech,
                       bool test_phi = true) {
  Rcpp::List output(2);
  // phi
  arma::mat phi_i(phi.n_rows, phi.n_cols, arma::fill::none);
  arma::vec phi_vec = arma::vectorise(phi);
  arma::vec phi_vec_i(phi.n_rows * phi.n_cols, arma::fill::none);
  bool run = true;
  while (run) {
    // generate data
    phi_vec_i = arma::mvnrnd(phi_vec, vcov_phi_vec);
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
  // sigma
  arma::uword q = (sigma.n_rows * (sigma.n_rows + 1)) / 2;
  arma::vec sigma_vech(q, arma::fill::none);
  arma::uword index = 0;
  for (arma::uword j = 0; j < sigma.n_cols; ++j) {
    for (arma::uword i = j; i < sigma.n_rows; ++i) {
      sigma_vech(index++) = sigma(i, j);
    }
  }
  arma::vec sigma_vech_i = arma::mvnrnd(sigma_vech, vcov_sigma_vech);
  arma::mat sigma_i(sigma.n_rows, sigma.n_rows, arma::fill::zeros);
  index = 0;
  for (arma::uword i = 0; i < sigma.n_rows; ++i) {
    for (arma::uword j = i; j < sigma.n_cols; ++j) {
      sigma_i(i, j) = sigma_vech_i(index);
      sigma_i(j, i) = sigma_vech_i(index);
      index++;
    }
  }
  arma::vec eigval;
  arma::mat eigvec;
  arma::eig_sym(eigval, eigvec, sigma_i);
  eigval.transform([](double val) { return std::max(val, 1e-8); });
  sigma_i = eigvec * arma::diagmat(eigval) * eigvec.t();
  output[0] = phi_i;
  output[1] = sigma_i;
  return output;
}
