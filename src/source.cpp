// -----------------------------------------------------------------------------
// edit .setup/cpp/000-forward-declarations.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

bool TestPhi(const arma::mat& phi);

bool TestStable(const arma::mat& x);
// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-direct-std.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.DirectStd)]]
double DirectStd(const arma::mat& phi, const arma::mat& sigma,
                 const double& delta_t, const arma::uword& from,
                 const arma::uword& to, const arma::vec& med) {
  arma::mat I = arma::eye(phi.n_rows, phi.n_cols);
  arma::mat J = arma::eye(phi.n_rows * phi.n_cols, phi.n_rows * phi.n_cols);
  arma::mat d = arma::eye(phi.n_rows, phi.n_rows);
  for (arma::uword i = 0; i < med.n_elem; ++i) {
    d(med[i] - 1, med[i] - 1) = 0;
  }
  arma::mat total = arma::expmat(delta_t * phi);
  arma::mat phi_hashtag = arma::kron(phi, I) + arma::kron(I, phi);
  arma::vec sigma_vec = arma::vectorise(sigma);
  // arma::vec psi_vec = arma::inv(phi_hashtag) * (arma::expmat(phi_hashtag *
  // delta_t) - J) * sigma_vec;
  arma::vec psi_vec = arma::solve(
      phi_hashtag, (arma::expmat(phi_hashtag * delta_t) - J) * sigma_vec);
  // arma::mat total_cov = arma::reshape(arma::inv(J - arma::kron(total, total))
  // * psi_vec, phi.n_rows, phi.n_cols);
  arma::mat total_cov =
      arma::reshape(arma::solve(J - arma::kron(total, total), psi_vec),
                    phi.n_rows, phi.n_cols);
  arma::mat sd_row = arma::diagmat(arma::sqrt(total_cov.diag()));
  arma::mat sd_col_inv = arma::diagmat(1.0 / arma::sqrt(total_cov.diag()));
  arma::mat direct = arma::expmat(delta_t * d * phi * d);
  // arma::mat direct_std = d * (sd_row * direct * sd_col_inv) * d;
  arma::mat direct_std = sd_row * direct * sd_col_inv;
  return direct_std(to - 1, from - 1);
}
// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-direct.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.Direct)]]
double Direct(const arma::mat& phi, const double& delta_t,
              const arma::uword& from, const arma::uword& to,
              const arma::vec& med) {
  arma::mat d = arma::eye(phi.n_rows, phi.n_rows);
  for (arma::uword i = 0; i < med.n_elem; ++i) {
    d(med[i] - 1, med[i] - 1) = 0;
  }
  arma::mat direct = arma::expmat(delta_t * d * phi * d);
  return direct(to - 1, from - 1);
}
// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-exp-cov.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.ExpCov)]]
arma::mat ExpCov(const arma::mat& phi, const arma::mat& sigma,
                 const double& delta_t) {
  arma::mat I = arma::eye(phi.n_rows, phi.n_cols);
  arma::mat J = arma::eye(phi.n_rows * phi.n_cols, phi.n_rows * phi.n_cols);
  arma::mat beta = arma::expmat(delta_t * phi);
  arma::mat phi_hashtag = arma::kron(phi, I) + arma::kron(I, phi);
  arma::vec sigma_vec = arma::vectorise(sigma);
  arma::vec psi_vec = arma::inv(phi_hashtag) *
                      (arma::expmat(phi_hashtag * delta_t) - J) * sigma_vec;
  return arma::reshape(arma::inv(J - arma::kron(beta, beta)) * psi_vec,
                       phi.n_rows, phi.n_cols);
}
// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-exp-mean.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.ExpMean)]]
Rcpp::NumericVector ExpMean(const arma::mat& phi, const arma::vec& iota,
                            const double& delta_t) {
  if (arma::all(iota == 0)) {
    return Rcpp::NumericVector(phi.n_rows, 0.0);
  }
  arma::mat I = arma::eye(phi.n_rows, phi.n_cols);
  arma::mat beta = arma::expmat(delta_t * phi);
  arma::vec alpha = arma::solve(phi, (beta - I) * iota);
  arma::vec mu = arma::solve((I - beta), alpha);
  Rcpp::NumericVector output(mu.begin(), mu.end());
  return output;
}
// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-indirect-central-s.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.IndirectCentrals)]]
arma::mat IndirectCentrals(const arma::mat& phi, const arma::vec& delta_t) {
  arma::mat output(phi.n_rows, delta_t.n_elem, arma::fill::zeros);
  arma::mat total(phi.n_rows, phi.n_cols, arma::fill::none);
  arma::mat direct(phi.n_rows, phi.n_cols, arma::fill::none);
  arma::mat d = arma::eye(phi.n_rows, phi.n_cols);
  for (arma::uword t = 0; t < delta_t.n_elem; t++) {
    total = arma::expmat(delta_t[t] * phi);
    for (arma::uword m = 0; m < phi.n_rows; m++) {
      d = arma::eye(phi.n_rows, phi.n_cols);
      d(m, m) = 0;
      direct = arma::expmat(delta_t[t] * d * phi * d);
      for (arma::uword j = 0; j < phi.n_rows; j++) {
        for (arma::uword i = 0; i < phi.n_rows; i++) {
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
  arma::uword p = std::sqrt(phi_vec.n_elem);
  arma::mat phi = arma::reshape(phi_vec, p, p);
  arma::mat total = arma::expmat(delta_t * phi);
  arma::mat direct(p, p, arma::fill::none);
  arma::mat d = arma::eye(p, p);
  Rcpp::NumericVector output(p);
  for (arma::uword m = 0; m < p; m++) {
    d = arma::eye(p, p);
    d(m, m) = 0;
    direct = arma::expmat(delta_t * d * phi * d);
    for (arma::uword j = 0; j < p; j++) {
      for (arma::uword i = 0; i < p; i++) {
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
  arma::uword p = phi.n_rows;
  arma::mat total = arma::expmat(delta_t * phi);
  arma::mat direct(p, p, arma::fill::none);
  arma::mat d = arma::eye(p, p);
  Rcpp::NumericVector output(p);
  for (arma::uword m = 0; m < p; m++) {
    d = arma::eye(p, p);
    d(m, m) = 0;
    direct = arma::expmat(delta_t * d * phi * d);
    for (arma::uword j = 0; j < p; j++) {
      for (arma::uword i = 0; i < p; i++) {
        if (!(m == i || m == j || i == j)) {
          output(m) += total(i, j) - direct(i, j);
        }
      }
    }
  }
  return output;
}
// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-indirect-std.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.IndirectStd)]]
double IndirectStd(const arma::mat& phi, const arma::mat& sigma,
                   const double& delta_t, const arma::uword& from,
                   const arma::uword& to, const arma::vec& med) {
  arma::mat I = arma::eye(phi.n_rows, phi.n_cols);
  arma::mat J = arma::eye(phi.n_rows * phi.n_cols, phi.n_rows * phi.n_cols);
  arma::mat d = arma::eye(phi.n_rows, phi.n_rows);
  for (arma::uword i = 0; i < med.n_elem; ++i) {
    d(med[i] - 1, med[i] - 1) = 0;
  }
  arma::mat total = arma::expmat(delta_t * phi);
  arma::mat phi_hashtag = arma::kron(phi, I) + arma::kron(I, phi);
  arma::vec sigma_vec = arma::vectorise(sigma);
  // arma::vec psi_vec = arma::inv(phi_hashtag) * (arma::expmat(phi_hashtag *
  // delta_t) - J) * sigma_vec;
  arma::vec psi_vec = arma::solve(
      phi_hashtag, (arma::expmat(phi_hashtag * delta_t) - J) * sigma_vec);
  // arma::mat total_cov = arma::reshape(arma::inv(J - arma::kron(total, total))
  // * psi_vec, phi.n_rows, phi.n_cols);
  arma::mat total_cov =
      arma::reshape(arma::solve(J - arma::kron(total, total), psi_vec),
                    phi.n_rows, phi.n_cols);
  arma::mat sd_row = arma::diagmat(arma::sqrt(total_cov.diag()));
  arma::mat sd_col_inv = arma::diagmat(1.0 / arma::sqrt(total_cov.diag()));
  arma::mat total_std = sd_row * total * sd_col_inv;
  double total_dbl = total_std(to - 1, from - 1);
  arma::mat direct = arma::expmat(delta_t * d * phi * d);
  // arma::mat direct_std = d * (sd_row * direct * sd_col_inv) * d;
  arma::mat direct_std = sd_row * direct * sd_col_inv;
  double direct_dbl = direct_std(to - 1, from - 1);
  return total_dbl - direct_dbl;
}
// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-indirect.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.Indirect)]]
double Indirect(const arma::mat& phi, const double& delta_t,
                const arma::uword& from, const arma::uword& to,
                const arma::vec& med) {
  arma::mat total = arma::expmat(delta_t * phi);
  double total_dbl = total(to - 1, from - 1);
  arma::mat d = arma::eye(phi.n_rows, phi.n_rows);
  for (arma::uword i = 0; i < med.n_elem; ++i) {
    d(med[i] - 1, med[i] - 1) = 0;
  }
  arma::mat direct = arma::expmat(delta_t * d * phi * d);
  double direct_dbl = direct(to - 1, from - 1);
  return total_dbl - direct_dbl;
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
  arma::mat phi_i(phi.n_rows, phi.n_cols, arma::fill::none);
  arma::vec phi_vec = arma::vectorise(phi);
  arma::vec phi_vec_i(phi.n_rows * phi.n_cols, arma::fill::none);
  bool run = true;
  while (run) {
    // generate data
    phi_vec_i =
        phi_vec + (vcov_phi_vec_l * arma::randn(phi.n_rows * phi.n_cols));
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
  return phi_i;
}
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
// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-mc-phi-sigma.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.MCPhiSigma)]]
Rcpp::List MCPhiSigma(const arma::vec& theta, const arma::mat& vcov_theta,
                      const arma::uword& R, bool test_phi = true) {
  Rcpp::List output(R);
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
  for (arma::uword i = 0; i < R; i++) {
    bool run = true;
    Rcpp::List output_i(2);
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
    output_i[0] = phi_i;
    output_i[1] = sigma_i;
    output[i] = output_i;
  }
  return output;
}
// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-mc-phi.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.MCPhi)]]
Rcpp::List MCPhi(const arma::mat& phi, const arma::mat& vcov_phi_vec_l,
                 const arma::uword& R, bool test_phi = true) {
  Rcpp::List output(R);
  arma::mat phi_i(phi.n_rows, phi.n_cols, arma::fill::none);
  arma::vec phi_vec = arma::vectorise(phi);
  arma::vec phi_vec_i(phi.n_rows * phi.n_cols, arma::fill::none);
  for (arma::uword i = 0; i < R; i++) {
    bool run = true;
    while (run) {
      // generate data
      phi_vec_i =
          phi_vec + (vcov_phi_vec_l * arma::randn(phi.n_rows * phi.n_cols));
      phi_i = arma::reshape(phi_vec_i, phi.n_rows, phi.n_cols);
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
arma::mat Meds(const arma::mat& phi, const arma::vec& delta_t,
               const arma::uword& from, const arma::uword& to,
               const arma::vec& med) {
  arma::mat output(delta_t.n_elem, 4, arma::fill::none);
  arma::mat total(phi.n_rows, phi.n_rows, arma::fill::none);
  arma::mat direct(phi.n_rows, phi.n_rows, arma::fill::none);
  arma::mat d = arma::eye(phi.n_rows, phi.n_rows);
  double total_dbl;
  double direct_dbl;
  double indirect_dbl;
  for (arma::uword i = 0; i < med.n_elem; ++i) {
    d(med[i] - 1, med[i] - 1) = 0;
  }
  for (arma::uword t = 0; t < delta_t.n_elem; t++) {
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
// edit .setup/cpp/cTMed-med-std-s.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.MedStds)]]
arma::mat MedStds(const arma::mat& phi, const arma::mat& sigma,
                  const arma::vec& delta_t, const arma::uword& from,
                  const arma::uword& to, const arma::vec& med) {
  arma::mat output(delta_t.n_elem, 4, arma::fill::none);
  arma::mat total(phi.n_rows, phi.n_cols, arma::fill::none);
  arma::mat direct(phi.n_rows, phi.n_cols, arma::fill::none);
  arma::mat d = arma::eye(phi.n_rows, phi.n_cols);
  arma::mat I = arma::eye(phi.n_rows, phi.n_cols);
  arma::mat J = arma::eye(phi.n_rows * phi.n_cols, phi.n_rows * phi.n_cols);
  double total_dbl;
  double direct_dbl;
  double indirect_dbl;
  for (arma::uword i = 0; i < med.n_elem; ++i) {
    d(med[i] - 1, med[i] - 1) = 0;
  }
  arma::mat phi_hashtag = arma::kron(phi, I) + arma::kron(I, phi);
  arma::vec sigma_vec = arma::vectorise(sigma);
  arma::vec psi_vec(phi.n_rows * phi.n_cols, arma::fill::none);
  arma::mat total_cov(phi.n_rows, phi.n_cols, arma::fill::none);
  arma::mat sd_row(phi.n_rows, phi.n_cols, arma::fill::none);
  arma::mat sd_col_inv(phi.n_rows, phi.n_cols, arma::fill::none);
  arma::mat total_std(phi.n_rows, phi.n_cols, arma::fill::none);
  arma::mat direct_std(phi.n_rows, phi.n_cols, arma::fill::none);
  for (arma::uword t = 0; t < delta_t.n_elem; t++) {
    total = arma::expmat(delta_t[t] * phi);
    // psi_vec = arma::inv(phi_hashtag) * (arma::expmat(phi_hashtag *
    // delta_t[t]) - J) * sigma_vec;
    psi_vec = arma::solve(
        phi_hashtag, (arma::expmat(phi_hashtag * delta_t[t]) - J) * sigma_vec);
    // total_cov = arma::reshape(arma::inv(J - arma::kron(total, total)) *
    // psi_vec, phi.n_rows, phi.n_cols);
    total_cov =
        arma::reshape(arma::solve(J - arma::kron(total, total), psi_vec),
                      phi.n_rows, phi.n_cols);
    sd_row = arma::diagmat(arma::sqrt(total_cov.diag()));
    sd_col_inv = arma::diagmat(1.0 / arma::sqrt(total_cov.diag()));
    total_std = sd_row * total * sd_col_inv;
    total_dbl = total_std(to - 1, from - 1);
    direct = arma::expmat(delta_t[t] * d * phi * d);
    // direct_std = d * (sd_row * direct * sd_col_inv) * d;
    direct_std = sd_row * direct * sd_col_inv;
    direct_dbl = direct_std(to - 1, from - 1);
    indirect_dbl = total_dbl - direct_dbl;
    output(t, 0) = total_dbl;
    output(t, 1) = direct_dbl;
    output(t, 2) = indirect_dbl;
    output(t, 3) = delta_t[t];
  }
  return output;
}
// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-med-std-vec.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.MedStdVec)]]
Rcpp::NumericVector MedStdVec(const arma::vec& v, const double& delta_t,
                              const arma::uword& from, const arma::uword& to,
                              const arma::vec& med) {
  arma::uword q = v.n_elem;
  arma::uword p = (-1 + std::sqrt(1 + 24 * q)) / 6;
  arma::mat phi = arma::mat(v.subvec(0, p * p - 1)).reshape(p, p);
  arma::vec sigma_vech = v.subvec(p * p, q - 1);
  arma::mat sigma(p, p, arma::fill::zeros);
  arma::uword index = 0;
  for (arma::uword j = 0; j < p; ++j) {
    for (arma::uword i = j; i < p; ++i) {
      sigma(i, j) = sigma_vech[index];
      if (i != j) {
        sigma(j, i) = sigma_vech[index];
      }
      index++;
    }
  }
  arma::mat d = arma::eye(p, p);
  for (arma::uword i = 0; i < med.n_elem; ++i) {
    d(med[i] - 1, med[i] - 1) = 0;
  }
  arma::mat I = arma::eye(p, p);
  arma::mat J = arma::eye(p * p, p * p);
  arma::mat total = arma::expmat(delta_t * phi);
  arma::mat phi_hashtag = arma::kron(phi, I) + arma::kron(I, phi);
  arma::vec sigma_vec = arma::vectorise(sigma);
  // arma::vec psi_vec = arma::inv(phi_hashtag) * (arma::expmat(phi_hashtag *
  // delta_t) - J) * sigma_vec;
  arma::vec psi_vec = arma::solve(
      phi_hashtag, (arma::expmat(phi_hashtag * delta_t) - J) * sigma_vec);
  // arma::mat total_cov = arma::reshape(arma::inv(J - arma::kron(total, total))
  // * psi_vec, phi.n_rows, phi.n_cols);
  arma::mat total_cov =
      arma::reshape(arma::solve(J - arma::kron(total, total), psi_vec),
                    phi.n_rows, phi.n_cols);
  arma::mat sd_row = arma::diagmat(arma::sqrt(total_cov.diag()));
  arma::mat sd_col_inv = arma::diagmat(1.0 / arma::sqrt(total_cov.diag()));
  arma::mat total_std = sd_row * total * sd_col_inv;
  double total_dbl = total_std(to - 1, from - 1);
  arma::mat direct = arma::expmat(delta_t * d * phi * d);
  // arma::mat direct_std = d * (sd_row * direct * sd_col_inv) * d;
  arma::mat direct_std = sd_row * direct * sd_col_inv;
  double direct_dbl = direct_std(to - 1, from - 1);
  double indirect_dbl = total_dbl - direct_dbl;
  Rcpp::NumericVector output(3);
  output(0) = total_dbl;
  output(1) = direct_dbl;
  output(2) = indirect_dbl;
  return output;
}
// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-med-std.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.MedStd)]]
Rcpp::NumericVector MedStd(const arma::mat& phi, const arma::mat& sigma,
                           const double& delta_t, const arma::uword& from,
                           const arma::uword& to, const arma::vec& med) {
  arma::mat I = arma::eye(phi.n_rows, phi.n_cols);
  arma::mat J = arma::eye(phi.n_rows * phi.n_cols, phi.n_rows * phi.n_cols);
  arma::mat d = arma::eye(phi.n_rows, phi.n_rows);
  for (arma::uword i = 0; i < med.n_elem; ++i) {
    d(med[i] - 1, med[i] - 1) = 0;
  }
  arma::mat total = arma::expmat(delta_t * phi);
  arma::mat phi_hashtag = arma::kron(phi, I) + arma::kron(I, phi);
  arma::vec sigma_vec = arma::vectorise(sigma);
  // arma::vec psi_vec = arma::inv(phi_hashtag) * (arma::expmat(phi_hashtag *
  // delta_t) - J) * sigma_vec;
  arma::vec psi_vec = arma::solve(
      phi_hashtag, (arma::expmat(phi_hashtag * delta_t) - J) * sigma_vec);
  // arma::mat total_cov = arma::reshape(arma::inv(J - arma::kron(total, total))
  // * psi_vec, phi.n_rows, phi.n_cols);
  arma::mat total_cov =
      arma::reshape(arma::solve(J - arma::kron(total, total), psi_vec),
                    phi.n_rows, phi.n_cols);
  arma::mat sd_row = arma::diagmat(arma::sqrt(total_cov.diag()));
  arma::mat sd_col_inv = arma::diagmat(1.0 / arma::sqrt(total_cov.diag()));
  arma::mat total_std = sd_row * total * sd_col_inv;
  double total_dbl = total_std(to - 1, from - 1);
  arma::mat direct = arma::expmat(delta_t * d * phi * d);
  // arma::mat direct_std = d * (sd_row * direct * sd_col_inv) * d;
  arma::mat direct_std = sd_row * direct * sd_col_inv;
  double direct_dbl = direct_std(to - 1, from - 1);
  double indirect_dbl = total_dbl - direct_dbl;
  Rcpp::NumericVector output(4);
  output(0) = total_dbl;
  output(1) = direct_dbl;
  output(2) = indirect_dbl;
  output(3) = delta_t;
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
                           const arma::uword& from, const arma::uword& to,
                           const arma::vec& med) {
  arma::uword p = std::sqrt(phi_vec.n_elem);
  arma::mat phi = arma::reshape(phi_vec, p, p);
  arma::mat total = arma::expmat(delta_t * phi);
  double total_dbl = total(to - 1, from - 1);
  arma::mat d = arma::eye(p, p);
  for (arma::uword i = 0; i < med.n_elem; ++i) {
    d(med[i] - 1, med[i] - 1) = 0;
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
                        const arma::uword& from, const arma::uword& to,
                        const arma::vec& med) {
  arma::mat total = arma::expmat(delta_t * phi);
  double total_dbl = total(to - 1, from - 1);
  arma::mat d = arma::eye(phi.n_rows, phi.n_rows);
  for (arma::uword i = 0; i < med.n_elem; ++i) {
    d(med[i] - 1, med[i] - 1) = 0;
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
  arma::mat output = arma::mat(phi.n_rows, delta_t.n_elem);
  arma::mat total(phi.n_rows, phi.n_cols, arma::fill::none);
  for (arma::uword t = 0; t < delta_t.n_elem; t++) {
    total = arma::expmat(delta_t[t] * phi);
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
  arma::uword p = std::sqrt(phi_vec.n_elem);
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
  Rcpp::NumericVector total_vec(total.memptr(), total.memptr() + total.n_elem);
  total_vec.push_back(delta_t);
  return total_vec;
}
// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-total-std-delta-t.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.TotalStdDeltaT)]]
Rcpp::NumericVector TotalStdDeltaT(const arma::mat& phi, const arma::mat& sigma,
                                   const double& delta_t) {
  arma::mat I = arma::eye(phi.n_rows, phi.n_cols);
  arma::mat J = arma::eye(phi.n_rows * phi.n_cols, phi.n_rows * phi.n_cols);
  arma::mat total = arma::expmat(delta_t * phi);
  arma::mat phi_hashtag = arma::kron(phi, I) + arma::kron(I, phi);
  arma::vec sigma_vec = arma::vectorise(sigma);
  // arma::vec psi_vec = arma::inv(phi_hashtag) * (arma::expmat(phi_hashtag *
  // delta_t) - J) * sigma_vec;
  arma::vec psi_vec = arma::solve(
      phi_hashtag, (arma::expmat(phi_hashtag * delta_t) - J) * sigma_vec);
  // arma::mat total_cov = arma::reshape(arma::inv(J - arma::kron(total, total))
  // * psi_vec, phi.n_rows, phi.n_cols);
  arma::mat total_cov =
      arma::reshape(arma::solve(J - arma::kron(total, total), psi_vec),
                    phi.n_rows, phi.n_cols);
  arma::mat sd_row = arma::diagmat(arma::sqrt(total_cov.diag()));
  arma::mat sd_col_inv = arma::diagmat(1.0 / arma::sqrt(total_cov.diag()));
  arma::mat total_std = sd_row * total * sd_col_inv;
  Rcpp::NumericVector total_std_vec(total_std.memptr(),
                                    total_std.memptr() + total_std.n_elem);
  total_std_vec.push_back(delta_t);
  return total_std_vec;
}
// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-total-std-vec.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.TotalStdVec)]]
arma::vec TotalStdVec(const arma::vec& v, const double& delta_t) {
  arma::uword q = v.n_elem;
  arma::uword p = (-1 + std::sqrt(1 + 24 * q)) / 6;
  arma::mat phi = arma::mat(v.subvec(0, p * p - 1)).reshape(p, p);
  arma::vec sigma_vech = v.subvec(p * p, q - 1);
  arma::mat sigma(p, p, arma::fill::zeros);
  arma::uword index = 0;
  for (arma::uword j = 0; j < p; ++j) {
    for (arma::uword i = j; i < p; ++i) {
      sigma(i, j) = sigma_vech[index];
      if (i != j) {
        sigma(j, i) = sigma_vech[index];
      }
      index++;
    }
  }
  arma::mat I = arma::eye(p, p);
  arma::mat J = arma::eye(p * p, p * p);
  arma::mat total = arma::expmat(delta_t * phi);
  arma::mat phi_hashtag = arma::kron(phi, I) + arma::kron(I, phi);
  arma::vec sigma_vec = arma::vectorise(sigma);
  // arma::vec psi_vec = arma::inv(phi_hashtag) * (arma::expmat(phi_hashtag *
  // delta_t) - J) * sigma_vec;
  arma::vec psi_vec = arma::solve(
      phi_hashtag, (arma::expmat(phi_hashtag * delta_t) - J) * sigma_vec);
  // arma::mat total_cov = arma::reshape(arma::inv(J - arma::kron(total, total))
  // * psi_vec, phi.n_rows, phi.n_cols);
  arma::mat total_cov =
      arma::reshape(arma::solve(J - arma::kron(total, total), psi_vec),
                    phi.n_rows, phi.n_cols);
  arma::mat sd_row = arma::diagmat(arma::sqrt(total_cov.diag()));
  arma::mat sd_col_inv = arma::diagmat(1.0 / arma::sqrt(total_cov.diag()));
  arma::mat total_std = sd_row * total * sd_col_inv;
  return arma::vectorise(total_std);
}
// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-total-std.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.TotalStd)]]
arma::mat TotalStd(const arma::mat& phi, const arma::mat& sigma,
                   const double& delta_t) {
  arma::mat I = arma::eye(phi.n_rows, phi.n_cols);
  arma::mat J = arma::eye(phi.n_rows * phi.n_cols, phi.n_rows * phi.n_cols);
  arma::mat total = arma::expmat(delta_t * phi);
  arma::mat phi_hashtag = arma::kron(phi, I) + arma::kron(I, phi);
  arma::vec sigma_vec = arma::vectorise(sigma);
  // arma::vec psi_vec = arma::inv(phi_hashtag) * (arma::expmat(phi_hashtag *
  // delta_t) - J) * sigma_vec;
  arma::vec psi_vec = arma::solve(
      phi_hashtag, (arma::expmat(phi_hashtag * delta_t) - J) * sigma_vec);
  // arma::mat total_cov = arma::reshape(arma::inv(J - arma::kron(total, total))
  // * psi_vec, phi.n_rows, phi.n_cols);
  arma::mat total_cov =
      arma::reshape(arma::solve(J - arma::kron(total, total), psi_vec),
                    phi.n_rows, phi.n_cols);
  arma::mat sd_row = arma::diagmat(arma::sqrt(total_cov.diag()));
  arma::mat sd_col_inv = arma::diagmat(1.0 / arma::sqrt(total_cov.diag()));
  return sd_row * total * sd_col_inv;
}
// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-total-vec.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.TotalVec)]]
arma::vec TotalVec(const arma::vec& phi_vec, const double& delta_t) {
  arma::uword p = std::sqrt(phi_vec.n_elem);
  arma::mat total = arma::expmat(delta_t * arma::reshape(phi_vec, p, p));
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
