// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-e-s-vec.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.MedVec)]]
Rcpp::NumericVector ESVec(const arma::vec& phi_vec, const double& delta_t,
                          const int& from, const int& to,
                          const arma::vec& med) {
  // combine phi_vec and sigma_vech into a single vector

  int ms = med.n_elem;
  int p = std::sqrt(phi_vec.n_elem);
  arma::mat phi = arma::reshape(phi_vec, p, p);
  arma::mat total = arma::expmat(delta_t * phi);
  double total_dbl = total(to - 1, from - 1);
  arma::mat d = arma::eye(p, p);
  for (int m = 0; m < ms; m++) {
    d(med[m] - 1, med[m] - 1) = 0;
  }
  arma::mat direct = arma::expmat(delta_t * d * phi * d);
  double direct_dbl = direct(to - 1, from - 1);
  double indirect_dbl = total_dbl - direct_dbl;

  arma::mat cov_total =
      arma::inv(arma::kron(I - total, total))* sigma_vec arma::mat cov_direct =
          arma::inv(arma::kron(I - direct,
                               direct))* sigma_vec arma::mat cov_indirect =
              arma::inv(arma::kron(I - indirect, indirect))* sigma_vec

      double var_total_dbl = cov_total(to - 1, from - 1);
  double var_direct_dbl = cov_direct(to - 1, from - 1);
  double var_indirect_dbl = cov_indirect(to - 1, from - 1);

  Rcpp::NumericVector output(2);
  output(0) = var_direct_dbl / (var_direct_dbl + var_total_dbl);
  output(1) = var_indirect_dbl / (var_indirect_dbl + var_total_dbl);

  return output;
}
