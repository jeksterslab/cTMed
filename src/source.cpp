// -----------------------------------------------------------------------------
// edit .setup/cpp/000-forward-declarations.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

bool TestPhi(const arma::mat& phi);

bool TestStable(const arma::mat& x);
// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-direct-vec.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.DirectVec)]]
double DirectVec(const arma::vec& phi_vec, const double& delta_t,
                 const int& from, const int& to, const arma::vec& med) {
  int ms = med.n_elem;
  int p = std::sqrt(phi_vec.n_elem);
  arma::mat phi = arma::reshape(phi_vec, p, p);
  arma::mat d = arma::eye(p, p);
  for (int m = 0; m < ms; m++) {
    d(med[m] - 1, med[m] - 1) = 0;
  }
  arma::mat direct = arma::expmat(delta_t * d * phi * d);
  return direct(to - 1, from - 1);
}
// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-direct.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.Direct)]]
double Direct(const arma::mat& phi, const double& delta_t, const int& from,
              const int& to, const arma::vec& med) {
  int ms = med.n_elem;
  int p = phi.n_rows;
  arma::mat d = arma::eye(p, p);
  for (int m = 0; m < ms; m++) {
    d(med[m] - 1, med[m] - 1) = 0;
  }
  arma::mat direct = arma::expmat(delta_t * d * phi * d);
  return direct(to - 1, from - 1);
}
// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-indirect-central-s.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.IndirectCentrals)]]
arma::mat IndirectCentrals(const arma::mat& phi, const arma::vec& delta_t) {
  int ts = delta_t.n_rows;
  int p = phi.n_rows;
  arma::mat output = arma::mat(p, ts);
  for (int t = 0; t < ts; t++) {
    arma::mat total = arma::expmat(delta_t[t] * phi);
    for (int m = 0; m < p; m++) {
      arma::mat d = arma::eye(p, p);
      d(m, m) = 0;
      arma::mat direct = arma::expmat(delta_t[t] * d * phi * d);
      for (int i = 0; i < p; i++) {
        for (int j = 0; j < p; j++) {
          if (!(m == i || m == j || i == j)) {
            output(m, t) += total(i, j) - direct(i, j);
          }
        }
      }
    }
  }
  return output.t();
}
// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-indirect-central-vec.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.IndirectCentralVec)]]
Rcpp::NumericVector IndirectCentralVec(const arma::vec& phi_vec,
                                       const double& delta_t) {
  int p = std::sqrt(phi_vec.n_elem);
  arma::mat phi = arma::reshape(phi_vec, p, p);
  arma::mat total = arma::expmat(delta_t * phi);
  Rcpp::NumericVector output(p);
  for (int m = 0; m < p; m++) {
    arma::mat d = arma::eye(p, p);
    d(m, m) = 0;
    arma::mat direct = arma::expmat(delta_t * d * phi * d);
    for (int i = 0; i < p; i++) {
      for (int j = 0; j < p; j++) {
        if (!(m == i || m == j || i == j)) {
          output(m) += total(i, j) - direct(i, j);
        }
      }
    }
  }
  return output;
}
// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-indirect-central.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.IndirectCentral)]]
Rcpp::NumericVector IndirectCentral(const arma::mat& phi,
                                    const double& delta_t) {
  int p = phi.n_rows;
  arma::mat total = arma::expmat(delta_t * phi);
  Rcpp::NumericVector output(p);
  for (int m = 0; m < p; m++) {
    arma::mat d = arma::eye(p, p);
    d(m, m) = 0;
    arma::mat direct = arma::expmat(delta_t * d * phi * d);
    for (int i = 0; i < p; i++) {
      for (int j = 0; j < p; j++) {
        if (!(m == i || m == j || i == j)) {
          output(m) += total(i, j) - direct(i, j);
        }
      }
    }
  }
  return output;
}
// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-indirect.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.Indirect)]]
double Indirect(const arma::mat& phi, const double& delta_t, const int& from,
                const int& to, const arma::vec& med) {
  int ms = med.n_elem;
  int p = phi.n_rows;
  arma::mat total = arma::expmat(delta_t * phi);
  double total_dbl = total(to - 1, from - 1);
  arma::mat d = arma::eye(p, p);
  for (int m = 0; m < ms; m++) {
    d(med[m] - 1, med[m] - 1) = 0;
  }
  arma::mat direct = arma::expmat(delta_t * d * phi * d);
  double direct_dbl = direct(to - 1, from - 1);
  return total_dbl - direct_dbl;
}
// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-r-sq.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

arma::mat.LinSDE2Cov(const double delta_t, const arma::vec& iota,
                     const arma::mat& phi, const arma::mat& sigma_l) {
  int p = iota.n_elem;
  arma::mat I = arma::eye(p, p);
  arma::mat beta = arma::expmat(phi * delta_t);
  arma::vec alpha = arma::vec(p);
  if (iota.is_zero()) {
    alpha = iota;
  } else {
    alpha = arma::inv(phi) * (beta - I) * iota;
  }
  arma::mat psi_l = arma::mat(p, p);
  if (sigma_l.is_zero()) {
    arma::mat cov_eta =
        arma::reshape(arma::inv(arma::kron(I - beta, beta)), p, p);
    psi_l = sigma_l;
  } else {
    arma::mat J = arma::eye(p * p, p * p);
    arma::mat phi_hashtag = arma::kron(phi, I) + arma::kron(I, phi);
    arma::vec sigma_vec = arma::vectorise(sigma_l * sigma_l.t());
    arma::vec psi_vec = arma::inv(phi_hashtag) *
                        (arma::expmat(phi_hashtag * delta_t) - J) * sigma_vec;
    psi_l = arma::chol(arma::reshape(psi_vec, p, p), "lower");
    arma::mat cov_eta =
        arma::reshape(arma::inv(arma::kron(I - beta, beta)) * psi_vec, p, p);
  }
  // output
  return Rcpp::List::create(
      Rcpp::Named("alpha") = alpha, Rcpp::Named("beta") = beta,
      Rcpp::Named("psi_l") = psi_l, Rcpp::Named("cov_eta") = cov_eta);
}
// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-mc-phi-i.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.MCPhiI)]]
arma::mat MCPhiI(const arma::mat& phi, const arma::mat& vcov_phi_vec_l,
                 bool test_phi = true) {
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
// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-med-s.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.Meds)]]
arma::mat Meds(const arma::mat& phi, const arma::vec& delta_t, const int& from,
               const int& to, const arma::vec& med) {
  int ts = delta_t.n_rows;
  int ms = med.n_elem;
  int p = phi.n_rows;
  arma::mat output = arma::mat(ts, 4);
  arma::mat total = arma::mat(p, p);
  arma::mat direct = arma::mat(p, p);
  arma::mat d = arma::eye(p, p);
  double total_dbl;
  double direct_dbl;
  double indirect_dbl;
  for (int m = 0; m < ms; m++) {
    d(med[m] - 1, med[m] - 1) = 0;
  }
  for (int t = 0; t < ts; t++) {
    total = arma::expmat(delta_t[t] * phi);
    total_dbl = total(to - 1, from - 1);
    direct = arma::expmat(delta_t[t] * d * phi * d);
    direct_dbl = direct(to - 1, from - 1);
    indirect_dbl = total_dbl - direct_dbl;
    output(t, 0) = total_dbl;
    output(t, 1) = direct_dbl;
    output(t, 2) = indirect_dbl;
    output(t, 3) = delta_t[t];
  }
  return output;
}
// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-med-vec.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.MedVec)]]
Rcpp::NumericVector MedVec(const arma::vec& phi_vec, const double& delta_t,
                           const int& from, const int& to,
                           const arma::vec& med) {
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
  Rcpp::NumericVector output(3);
  output(0) = total_dbl;
  output(1) = direct_dbl;
  output(2) = indirect_dbl;
  return output;
}
// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-med.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.Med)]]
Rcpp::NumericVector Med(const arma::mat& phi, const double& delta_t,
                        const int& from, const int& to, const arma::vec& med) {
  int ms = med.n_elem;
  int p = phi.n_rows;
  arma::mat total = arma::expmat(delta_t * phi);
  double total_dbl = total(to - 1, from - 1);
  arma::mat d = arma::eye(p, p);
  for (int m = 0; m < ms; m++) {
    d(med[m] - 1, med[m] - 1) = 0;
  }
  arma::mat direct = arma::expmat(delta_t * d * phi * d);
  double direct_dbl = direct(to - 1, from - 1);
  double indirect_dbl = total_dbl - direct_dbl;
  Rcpp::NumericVector output(4);
  output(0) = total_dbl;
  output(1) = direct_dbl;
  output(2) = indirect_dbl;
  output(3) = delta_t;
  return output;
}
// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-prop-vec.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.PropVec)]]
Rcpp::NumericVector PropVec(const arma::vec& phi_vec, const double& delta_t,
                            const int& from, const int& to,
                            const arma::vec& med) {
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
  Rcpp::NumericVector output(2);
  output(0) = direct_dbl / (direct_dbl + total_dbl);
  output(1) = indirect_dbl / (indirect_dbl + total_dbl);
  return output;
}
// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-med-s.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.Meds)]]
arma::mat Meds(const arma::mat& phi, const arma::mat& sigma_l,
               const arma::vec& delta_t, const int& from, const int& to,
               const arma::vec& med) {
  int ts = delta_t.n_rows;
  int ms = med.n_elem;
  int p = phi.n_rows;
  arma::mat output = arma::mat(ts, 4);
  arma::mat total = arma::mat(p, p);
  arma::mat direct = arma::mat(p, p);
  arma::mat d = arma::eye(p, p);
  double total_dbl;
  double direct_dbl;
  double indirect_dbl;
  arma::mat phi_hashtag = arma::kron(phi, I) + arma::kron(I, phi);
  arma::vec sigma_vec = arma::vectorise(sigma_l * sigma_l.t());
  for (int m = 0; m < ms; m++) {
    d(med[m] - 1, med[m] - 1) = 0;
  }
  for (int t = 0; t < ts; t++) {
    total = arma::expmat(delta_t[t] * phi);
    total_dbl = total(to - 1, from - 1);
    direct = arma::expmat(delta_t[t] * d * phi * d);
    direct_dbl = direct(to - 1, from - 1);
    indirect_dbl = total_dbl - direct_dbl;
    arma::vec psi_vec = arma::inv(phi_hashtag) *
                        (arma::expmat(phi_hashtag * delta_t[t]) - J) *
                        sigma_vec;
    output(t, 0) = total_dbl;
    output(t, 1) = direct_dbl;
    output(t, 2) = indirect_dbl;
    output(t, 3) = delta_t[t];
  }
  return output;
}
// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-ratio-vec.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.RatioVec)]]
Rcpp::NumericVector RatioVec(const arma::vec& phi_vec, const double& delta_t,
                             const int& from, const int& to,
                             const arma::vec& med) {
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
  Rcpp::NumericVector output(2);
  output(0) = direct_dbl / total_dbl;
  output(1) = indirect_dbl / total_dbl;
  return output;
}
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
// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-test-stable.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.TestStable)]]
bool TestStable(const arma::mat& x) {
  arma::cx_vec eigenvalues = arma::eig_gen(x);
  return arma::all(arma::real(eigenvalues) < 0);
}
// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-total-central-s.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.TotalCentrals)]]
arma::mat TotalCentrals(const arma::mat& phi, const arma::vec& delta_t) {
  int ts = delta_t.n_rows;
  int p = phi.n_rows;
  arma::mat output = arma::mat(p, ts);
  for (int t = 0; t < ts; t++) {
    arma::mat total = arma::expmat(delta_t[t] * phi);
    output.col(t) = arma::vectorise(arma::sum(total, 0) - total.diag().t());
  }
  return output.t();
}
// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-total-central-vec.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.TotalCentralVec)]]
Rcpp::NumericVector TotalCentralVec(const arma::vec& phi_vec,
                                    const double& delta_t) {
  int p = std::sqrt(phi_vec.n_elem);
  arma::mat phi = arma::reshape(phi_vec, p, p);
  arma::mat total = arma::expmat(delta_t * phi);
  arma::vec total_central =
      arma::vectorise(arma::sum(total, 0) - total.diag().t());
  return Rcpp::NumericVector(total_central.begin(), total_central.end());
}
// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-total-central.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.TotalCentral)]]
Rcpp::NumericVector TotalCentral(const arma::mat& phi, const double& delta_t) {
  arma::mat total = arma::expmat(delta_t * phi);
  arma::vec total_central =
      arma::vectorise(arma::sum(total, 0) - total.diag().t());
  return Rcpp::NumericVector(total_central.begin(), total_central.end());
}
// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-total-delta-t.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.TotalDeltaT)]]
Rcpp::NumericVector TotalDeltaT(const arma::mat& phi, const double& delta_t) {
  arma::mat total = arma::expmat(delta_t * phi);
  int q = total.n_elem;
  Rcpp::NumericVector total_vec(q);
  for (int i = 0; i < q; ++i) {
    total_vec[i] = total(i);
  }
  total_vec.push_back(delta_t);
  return total_vec;
}
// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-total-vec.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.TotalVec)]]
arma::vec TotalVec(const arma::vec& phi_vec, const double& delta_t) {
  int p = std::sqrt(phi_vec.n_elem);
  arma::mat phi = arma::reshape(phi_vec, p, p);
  arma::mat total = arma::expmat(delta_t * phi);
  return arma::vectorise(total);
}
// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-total.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.Total)]]
arma::mat Total(const arma::mat& phi, const double& delta_t) {
  return arma::expmat(delta_t * phi);
}
