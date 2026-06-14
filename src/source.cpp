// -----------------------------------------------------------------------------
// edit .setup/cpp/000-forward-declarations.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

bool TestPhi(const arma::mat& phi);

bool TestStable(const arma::mat& x);
// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-direct-central-s.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.DirectCentrals)]]
arma::mat DirectCentrals(const arma::mat& phi, const arma::vec& delta_t) {
  arma::mat output(phi.n_rows, delta_t.n_elem, arma::fill::zeros);
  arma::mat direct(phi.n_rows, phi.n_cols, arma::fill::none);
  arma::mat d = arma::eye(phi.n_rows, phi.n_cols);
  for (arma::uword t = 0; t < delta_t.n_elem; t++) {
    for (arma::uword m = 0; m < phi.n_rows; m++) {
      d = arma::eye(phi.n_rows, phi.n_cols);
      d(m, m) = 0;
      direct = arma::expmat(delta_t[t] * d * phi * d);
      for (arma::uword j = 0; j < phi.n_rows; j++) {
        for (arma::uword i = 0; i < phi.n_rows; i++) {
          if (!(m == i || m == j || i == j)) {
            output(m, t) += direct(i, j);
          }
        }
      }
    }
  }
  return output.t();
}
// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-direct-central-std-s.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.DirectCentralStds)]]
arma::mat DirectCentralStds(const arma::mat& phi, const arma::mat& sigma,
                            const arma::vec& delta_t) {
  arma::uword p = phi.n_rows;
  arma::mat output(p, delta_t.n_elem, arma::fill::zeros);
  arma::mat cov_eta;
  arma::sylvester(cov_eta, phi, phi.t(), sigma);
  arma::vec sqrt_diag = arma::sqrt(cov_eta.diag());
  arma::mat direct(p, p, arma::fill::none);
  arma::mat d = arma::eye(p, p);
  for (arma::uword t = 0; t < delta_t.n_elem; ++t) {
    for (arma::uword m = 0; m < p; ++m) {
      d = arma::eye(p, p);
      d(m, m) = 0;
      direct = arma::expmat(delta_t[t] * d * phi * d);
      for (arma::uword j = 0; j < p; ++j) {
        for (arma::uword i = 0; i < p; ++i) {
          if (!(m == i || m == j || i == j)) {
            output(m, t) += direct(i, j) * sqrt_diag(j) / sqrt_diag(i);
          }
        }
      }
    }
  }
  return output.t();
}
// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-direct-central-std-vec.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.DirectCentralStdVec)]]
arma::vec DirectCentralStdVec(const arma::vec& v, const double& delta_t,
                              const bool& sigma_diag = false) {
  arma::uword q = v.n_elem;
  arma::uword p = 0;
  if (sigma_diag) {
    double p_dbl = (-1.0 + std::sqrt(1.0 + 4.0 * q)) / 2.0;
    p = static_cast<arma::uword>(std::round(p_dbl));
    if (p * p + p != q) {
      Rcpp::stop(
          "When sigma_diag = TRUE, v must contain vec(phi) and diag(sigma).");
    }
  } else {
    double p_dbl = (-1.0 + std::sqrt(1.0 + 24.0 * q)) / 6.0;
    p = static_cast<arma::uword>(std::round(p_dbl));
    if (p * p + p * (p + 1) / 2 != q) {
      Rcpp::stop(
          "When sigma_diag = FALSE, v must contain vec(phi) and vech(sigma).");
    }
  }
  arma::mat phi = arma::reshape(v.subvec(0, p * p - 1), p, p);
  arma::mat sigma(p, p, arma::fill::zeros);
  if (sigma_diag) {
    sigma.diag() = v.subvec(p * p, q - 1);
  } else {
    arma::vec sigma_vech = v.subvec(p * p, q - 1);
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
  }
  arma::mat cov_eta;
  arma::sylvester(cov_eta, phi, phi.t(), sigma);
  arma::vec sqrt_diag = arma::sqrt(cov_eta.diag());
  arma::mat direct(p, p, arma::fill::none);
  arma::mat d = arma::eye(p, p);
  arma::vec output(p, arma::fill::zeros);
  for (arma::uword m = 0; m < p; ++m) {
    d = arma::eye(p, p);
    d(m, m) = 0;
    direct = arma::expmat(delta_t * d * phi * d);
    for (arma::uword j = 0; j < p; ++j) {
      for (arma::uword i = 0; i < p; ++i) {
        if (!(m == i || m == j || i == j)) {
          output(m) += direct(i, j) * sqrt_diag(j) / sqrt_diag(i);
        }
      }
    }
  }
  return output;
}
// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-direct-central-std.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.DirectCentralStd)]]
Rcpp::NumericVector DirectCentralStd(const arma::mat& phi,
                                     const arma::mat& sigma,
                                     const double& delta_t) {
  arma::uword p = phi.n_rows;
  arma::mat cov_eta;
  arma::sylvester(cov_eta, phi, phi.t(), sigma);
  arma::vec sqrt_diag = arma::sqrt(cov_eta.diag());
  arma::mat direct(p, p, arma::fill::none);
  arma::mat d = arma::eye(p, p);
  Rcpp::NumericVector output(p);
  for (arma::uword m = 0; m < p; ++m) {
    d = arma::eye(p, p);
    d(m, m) = 0;
    direct = arma::expmat(delta_t * d * phi * d);
    for (arma::uword j = 0; j < p; ++j) {
      for (arma::uword i = 0; i < p; ++i) {
        if (!(m == i || m == j || i == j)) {
          output(m) += direct(i, j) * sqrt_diag(j) / sqrt_diag(i);
        }
      }
    }
  }
  return output;
}
// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-direct-central-vec.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.DirectCentralVec)]]
Rcpp::NumericVector DirectCentralVec(const arma::vec& phi_vec,
                                     const double& delta_t) {
  arma::uword p = std::sqrt(phi_vec.n_elem);
  arma::mat phi = arma::reshape(phi_vec, p, p);
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
          output(m) += direct(i, j);
        }
      }
    }
  }
  return output;
}
// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-direct-central.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.DirectCentral)]]
Rcpp::NumericVector DirectCentral(const arma::mat& phi, const double& delta_t) {
  arma::uword p = phi.n_rows;
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
          output(m) += direct(i, j);
        }
      }
    }
  }
  return output;
}
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
  arma::mat d = arma::eye(phi.n_rows, phi.n_rows);
  for (arma::uword i = 0; i < med.n_elem; ++i) {
    d(med[i] - 1, med[i] - 1) = 0;
  }
  arma::mat direct = arma::expmat(delta_t * d * phi * d);
  arma::mat cov_eta;
  arma::sylvester(cov_eta, phi, phi.t(), sigma);
  arma::vec sqrt_diag = arma::sqrt(cov_eta.diag());
  arma::mat direct_std = direct;
  for (size_t i = 0; i < direct.n_rows; i++) {
    for (size_t j = 0; j < direct.n_cols; j++) {
      direct_std(i, j) *= sqrt_diag(j) / sqrt_diag(i);
    }
  }
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
// edit .setup/cpp/cTMed-indirect-central-std-s.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.IndirectCentralStds)]]
arma::mat IndirectCentralStds(const arma::mat& phi, const arma::mat& sigma,
                              const arma::vec& delta_t) {
  arma::uword p = phi.n_rows;
  arma::mat output(p, delta_t.n_elem, arma::fill::zeros);
  arma::mat cov_eta;
  arma::sylvester(cov_eta, phi, phi.t(), sigma);
  arma::vec sqrt_diag = arma::sqrt(cov_eta.diag());
  arma::mat total(p, p, arma::fill::none);
  arma::mat direct(p, p, arma::fill::none);
  arma::mat d = arma::eye(p, p);
  for (arma::uword t = 0; t < delta_t.n_elem; ++t) {
    total = arma::expmat(delta_t[t] * phi);
    for (arma::uword m = 0; m < p; ++m) {
      d = arma::eye(p, p);
      d(m, m) = 0;
      direct = arma::expmat(delta_t[t] * d * phi * d);
      for (arma::uword j = 0; j < p; ++j) {
        for (arma::uword i = 0; i < p; ++i) {
          if (!(m == i || m == j || i == j)) {
            output(m, t) +=
                (total(i, j) - direct(i, j)) * sqrt_diag(j) / sqrt_diag(i);
          }
        }
      }
    }
  }
  return output.t();
}
// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-indirect-central-std-vec.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.IndirectCentralStdVec)]]
arma::vec IndirectCentralStdVec(const arma::vec& v, const double& delta_t,
                                const bool& sigma_diag = false) {
  arma::uword q = v.n_elem;
  arma::uword p = 0;
  if (sigma_diag) {
    double p_dbl = (-1.0 + std::sqrt(1.0 + 4.0 * q)) / 2.0;
    p = static_cast<arma::uword>(std::round(p_dbl));
    if (p * p + p != q) {
      Rcpp::stop(
          "When sigma_diag = TRUE, v must contain vec(phi) and diag(sigma).");
    }
  } else {
    double p_dbl = (-1.0 + std::sqrt(1.0 + 24.0 * q)) / 6.0;
    p = static_cast<arma::uword>(std::round(p_dbl));

    if (p * p + p * (p + 1) / 2 != q) {
      Rcpp::stop(
          "When sigma_diag = FALSE, v must contain vec(phi) and vech(sigma).");
    }
  }
  arma::mat phi = arma::reshape(v.subvec(0, p * p - 1), p, p);
  arma::mat sigma(p, p, arma::fill::zeros);
  if (sigma_diag) {
    sigma.diag() = v.subvec(p * p, q - 1);
  } else {
    arma::vec sigma_vech = v.subvec(p * p, q - 1);
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
  }
  arma::mat total = arma::expmat(delta_t * phi);
  arma::mat cov_eta;
  arma::sylvester(cov_eta, phi, phi.t(), sigma);
  arma::vec sqrt_diag = arma::sqrt(cov_eta.diag());
  arma::mat direct(p, p, arma::fill::none);
  arma::mat d = arma::eye(p, p);
  arma::vec output(p, arma::fill::zeros);
  for (arma::uword m = 0; m < p; ++m) {
    d = arma::eye(p, p);
    d(m, m) = 0;
    direct = arma::expmat(delta_t * d * phi * d);
    for (arma::uword j = 0; j < p; ++j) {
      for (arma::uword i = 0; i < p; ++i) {
        if (!(m == i || m == j || i == j)) {
          output(m) +=
              (total(i, j) - direct(i, j)) * sqrt_diag(j) / sqrt_diag(i);
        }
      }
    }
  }
  return output;
}
// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-indirect-central-std.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.IndirectCentralStd)]]
Rcpp::NumericVector IndirectCentralStd(const arma::mat& phi,
                                       const arma::mat& sigma,
                                       const double& delta_t) {
  arma::uword p = phi.n_rows;
  arma::mat total = arma::expmat(delta_t * phi);
  arma::mat cov_eta;
  arma::sylvester(cov_eta, phi, phi.t(), sigma);
  arma::vec sqrt_diag = arma::sqrt(cov_eta.diag());
  arma::mat direct(p, p, arma::fill::none);
  arma::mat d = arma::eye(p, p);
  Rcpp::NumericVector output(p);
  for (arma::uword m = 0; m < p; ++m) {
    d = arma::eye(p, p);
    d(m, m) = 0;
    direct = arma::expmat(delta_t * d * phi * d);
    for (arma::uword j = 0; j < p; ++j) {
      for (arma::uword i = 0; i < p; ++i) {
        if (!(m == i || m == j || i == j)) {
          output(m) +=
              (total(i, j) - direct(i, j)) * sqrt_diag(j) / sqrt_diag(i);
        }
      }
    }
  }
  return output;
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
  arma::mat d = arma::eye(phi.n_rows, phi.n_rows);
  for (arma::uword i = 0; i < med.n_elem; ++i) {
    d(med[i] - 1, med[i] - 1) = 0;
  }
  arma::mat total = arma::expmat(delta_t * phi);
  arma::mat cov_eta;
  arma::sylvester(cov_eta, phi, phi.t(), sigma);
  arma::vec sqrt_diag = arma::sqrt(cov_eta.diag());
  arma::mat total_std = total;
  for (size_t i = 0; i < total.n_rows; i++) {
    for (size_t j = 0; j < total.n_cols; j++) {
      total_std(i, j) *= sqrt_diag(j) / sqrt_diag(i);
    }
  }
  double total_dbl = total_std(to - 1, from - 1);
  arma::mat direct = arma::expmat(delta_t * d * phi * d);
  arma::mat direct_std = direct;
  for (size_t i = 0; i < direct.n_rows; i++) {
    for (size_t j = 0; j < direct.n_cols; j++) {
      direct_std(i, j) *= sqrt_diag(j) / sqrt_diag(i);
    }
  }
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
                       bool test_phi = true, bool sigma_diag = false) {
  Rcpp::List output(2);
  arma::uword n = theta.n_elem;
  arma::uword p;
  arma::uword sigma_n;
  if (sigma_diag) {
    p = (-1 + std::sqrt(1 + 4 * n)) / 2;
    sigma_n = p;
  } else {
    p = (-1 + std::sqrt(1 + 24 * n)) / 6;
    sigma_n = (p * (p + 1)) / 2;
  }
  arma::uword index = 0;
  arma::vec v_i(n, arma::fill::none);
  arma::mat phi_i(p, p, arma::fill::none);
  arma::vec phi_vec_i(p * p, arma::fill::none);
  arma::mat sigma_i(p, p, arma::fill::zeros);
  arma::vec sigma_vec_i(sigma_n, arma::fill::none);
  arma::vec eigval;
  arma::mat eigvec;
  bool run = true;
  while (run) {
    v_i = arma::mvnrnd(theta, vcov_theta);
    phi_vec_i = v_i(arma::span(0, (p * p) - 1));
    sigma_vec_i = v_i(arma::span(p * p, n - 1));
    phi_i = arma::reshape(phi_vec_i, p, p);
    if (test_phi) {
      if (TestPhi(phi_i)) {
        run = false;
      }
    } else {
      run = false;
    }
    if (run == false) {
      if (sigma_diag) {
        sigma_i.zeros();
        for (arma::uword i = 0; i < p; ++i) {
          sigma_i(i, i) = std::max(sigma_vec_i(i), 1e-8);
        }
      } else {
        index = 0;
        for (arma::uword i = 0; i < p; ++i) {
          for (arma::uword j = i; j < p; ++j) {
            sigma_i(i, j) = sigma_vec_i(index);
            sigma_i(j, i) = sigma_vec_i(index);
            index++;
          }
        }
        arma::eig_sym(eigval, eigvec, sigma_i);
        eigval.transform([](double val) { return std::max(val, 1e-8); });
        sigma_i = eigvec * arma::diagmat(eigval) * eigvec.t();
      }
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
                      const arma::uword& R, bool test_phi = true,
                      bool sigma_diag = false) {
  Rcpp::List output(R);
  arma::uword n = theta.n_elem;
  arma::uword p;
  arma::uword q;
  if (sigma_diag) {
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
        if (sigma_diag) {
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
  arma::mat sd_row(phi.n_rows, phi.n_cols, arma::fill::none);
  arma::mat sd_col_inv(phi.n_rows, phi.n_cols, arma::fill::none);
  arma::mat total_std(phi.n_rows, phi.n_cols, arma::fill::none);
  arma::mat direct_std(phi.n_rows, phi.n_cols, arma::fill::none);
  arma::mat cov_eta;
  arma::sylvester(cov_eta, phi, phi.t(), sigma);
  arma::vec sqrt_diag = arma::sqrt(cov_eta.diag());
  for (arma::uword t = 0; t < delta_t.n_elem; t++) {
    total = arma::expmat(delta_t[t] * phi);
    arma::mat total_std = total;
    for (size_t i = 0; i < total.n_rows; i++) {
      for (size_t j = 0; j < total.n_cols; j++) {
        total_std(i, j) *= sqrt_diag(j) / sqrt_diag(i);
      }
    }
    total_dbl = total_std(to - 1, from - 1);
    direct = arma::expmat(delta_t[t] * d * phi * d);
    arma::mat direct_std = direct;
    for (size_t i = 0; i < direct.n_rows; i++) {
      for (size_t j = 0; j < direct.n_cols; j++) {
        direct_std(i, j) *= sqrt_diag(j) / sqrt_diag(i);
      }
    }
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
                              const arma::vec& med,
                              const bool& sigma_diag = false) {
  arma::uword q = v.n_elem;
  arma::uword p = 0;
  if (sigma_diag) {
    double p_dbl = (-1.0 + std::sqrt(1.0 + 4.0 * q)) / 2.0;
    p = static_cast<arma::uword>(std::round(p_dbl));
    if (p * p + p != q) {
      Rcpp::stop(
          "When sigma_diag = TRUE, v must contain vec(phi) and diag(sigma).");
    }
  } else {
    double p_dbl = (-1.0 + std::sqrt(1.0 + 24.0 * q)) / 6.0;
    p = static_cast<arma::uword>(std::round(p_dbl));
    if (p * p + p * (p + 1) / 2 != q) {
      Rcpp::stop(
          "When sigma_diag = FALSE, v must contain vec(phi) and vech(sigma).");
    }
  }
  if (from < 1 || from > p || to < 1 || to > p) {
    Rcpp::stop("from and to must be between 1 and p.");
  }
  arma::mat phi = arma::mat(v.subvec(0, p * p - 1)).reshape(p, p);
  arma::mat sigma(p, p, arma::fill::zeros);
  if (sigma_diag) {
    sigma.diag() = v.subvec(p * p, q - 1);
  } else {
    arma::vec sigma_vech = v.subvec(p * p, q - 1);
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
  }
  arma::mat d = arma::eye(p, p);
  for (arma::uword i = 0; i < med.n_elem; ++i) {
    arma::uword med_i = static_cast<arma::uword>(med[i]);
    if (med_i < 1 || med_i > p) {
      Rcpp::stop("All elements of med must be between 1 and p.");
    }
    d(med_i - 1, med_i - 1) = 0;
  }
  arma::mat total = arma::expmat(delta_t * phi);
  arma::mat cov_eta;
  arma::sylvester(cov_eta, phi, phi.t(), sigma);
  arma::vec sqrt_diag = arma::sqrt(cov_eta.diag());
  arma::mat total_std = total;
  for (arma::uword i = 0; i < total.n_rows; ++i) {
    for (arma::uword j = 0; j < total.n_cols; ++j) {
      total_std(i, j) *= sqrt_diag(j) / sqrt_diag(i);
    }
  }
  double total_dbl = total_std(to - 1, from - 1);
  arma::mat direct = arma::expmat(delta_t * d * phi * d);
  arma::mat direct_std = direct;
  for (arma::uword i = 0; i < direct.n_rows; ++i) {
    for (arma::uword j = 0; j < direct.n_cols; ++j) {
      direct_std(i, j) *= sqrt_diag(j) / sqrt_diag(i);
    }
  }
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
  arma::mat d = arma::eye(phi.n_rows, phi.n_rows);
  for (arma::uword i = 0; i < med.n_elem; ++i) {
    d(med[i] - 1, med[i] - 1) = 0;
  }
  arma::mat total = arma::expmat(delta_t * phi);
  arma::mat cov_eta;
  arma::sylvester(cov_eta, phi, phi.t(), sigma);
  arma::vec sqrt_diag = arma::sqrt(cov_eta.diag());
  arma::mat total_std = total;
  for (size_t i = 0; i < total.n_rows; i++) {
    for (size_t j = 0; j < total.n_cols; j++) {
      total_std(i, j) *= sqrt_diag(j) / sqrt_diag(i);
    }
  }
  double total_dbl = total_std(to - 1, from - 1);
  arma::mat direct = arma::expmat(delta_t * d * phi * d);
  arma::mat direct_std = direct;
  for (size_t i = 0; i < direct.n_rows; i++) {
    for (size_t j = 0; j < direct.n_cols; j++) {
      direct_std(i, j) *= sqrt_diag(j) / sqrt_diag(i);
    }
  }
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
// edit .setup/cpp/cTMed-total-central-std-s.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.TotalCentralStds)]]
arma::mat TotalCentralStds(const arma::mat& phi, const arma::mat& sigma,
                           const arma::vec& delta_t) {
  arma::uword p = phi.n_rows;
  arma::mat output(p, delta_t.n_elem, arma::fill::zeros);
  arma::mat cov_eta;
  arma::sylvester(cov_eta, phi, phi.t(), sigma);
  arma::vec sqrt_diag = arma::sqrt(cov_eta.diag());
  arma::mat total(p, p, arma::fill::none);
  for (arma::uword t = 0; t < delta_t.n_elem; ++t) {
    total = arma::expmat(delta_t[t] * phi);
    for (arma::uword j = 0; j < p; ++j) {
      for (arma::uword i = 0; i < p; ++i) {
        if (i != j) {
          output(j, t) += total(i, j) * sqrt_diag(j) / sqrt_diag(i);
        }
      }
    }
  }
  return output.t();
}
// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-total-central-std-vec.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.TotalCentralStdVec)]]
arma::vec TotalCentralStdVec(const arma::vec& v, const double& delta_t,
                             const bool& sigma_diag = false) {
  arma::uword q = v.n_elem;
  arma::uword p = 0;
  if (sigma_diag) {
    double p_dbl = (-1.0 + std::sqrt(1.0 + 4.0 * q)) / 2.0;
    p = static_cast<arma::uword>(std::round(p_dbl));
    if (p * p + p != q) {
      Rcpp::stop(
          "When sigma_diag = TRUE, v must contain vec(phi) and diag(sigma).");
    }
  } else {
    double p_dbl = (-1.0 + std::sqrt(1.0 + 24.0 * q)) / 6.0;
    p = static_cast<arma::uword>(std::round(p_dbl));
    if (p * p + p * (p + 1) / 2 != q) {
      Rcpp::stop(
          "When sigma_diag = FALSE, v must contain vec(phi) and vech(sigma).");
    }
  }
  arma::mat phi = arma::reshape(v.subvec(0, p * p - 1), p, p);
  arma::mat sigma(p, p, arma::fill::zeros);
  if (sigma_diag) {
    sigma.diag() = v.subvec(p * p, q - 1);
  } else {
    arma::vec sigma_vech = v.subvec(p * p, q - 1);
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
  }
  arma::mat total = arma::expmat(delta_t * phi);
  arma::mat cov_eta;
  arma::sylvester(cov_eta, phi, phi.t(), sigma);
  arma::vec sqrt_diag = arma::sqrt(cov_eta.diag());
  arma::vec output(p, arma::fill::zeros);
  for (arma::uword j = 0; j < p; ++j) {
    for (arma::uword i = 0; i < p; ++i) {
      if (i != j) {
        output(j) += total(i, j) * sqrt_diag(j) / sqrt_diag(i);
      }
    }
  }
  return output;
}
// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-total-central-std.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.TotalCentralStd)]]
Rcpp::NumericVector TotalCentralStd(const arma::mat& phi,
                                    const arma::mat& sigma,
                                    const double& delta_t) {
  arma::uword p = phi.n_rows;
  arma::mat total = arma::expmat(delta_t * phi);
  arma::mat cov_eta;
  arma::sylvester(cov_eta, phi, phi.t(), sigma);
  arma::vec sqrt_diag = arma::sqrt(cov_eta.diag());
  arma::vec output(p, arma::fill::zeros);
  for (arma::uword j = 0; j < p; ++j) {
    for (arma::uword i = 0; i < p; ++i) {
      if (i != j) {
        output(j) += total(i, j) * sqrt_diag(j) / sqrt_diag(i);
      }
    }
  }
  return Rcpp::NumericVector(output.begin(), output.end());
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
  arma::mat total = arma::expmat(delta_t * phi);
  arma::mat cov_eta;
  arma::sylvester(cov_eta, phi, phi.t(), sigma);
  arma::vec sqrt_diag = arma::sqrt(cov_eta.diag());
  arma::mat total_std = total;
  for (size_t i = 0; i < total.n_rows; i++) {
    for (size_t j = 0; j < total.n_cols; j++) {
      total_std(i, j) *= sqrt_diag(j) / sqrt_diag(i);
    }
  }
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
arma::vec TotalStdVec(const arma::vec& v, const double& delta_t,
                      const bool& sigma_diag = false) {
  arma::uword q = v.n_elem;
  arma::uword p = 0;
  if (sigma_diag) {
    double p_dbl = (-1.0 + std::sqrt(1.0 + 4.0 * q)) / 2.0;
    p = static_cast<arma::uword>(std::round(p_dbl));
    if (p * p + p != q) {
      Rcpp::stop(
          "When sigma_diag = TRUE, v must contain vec(phi) and diag(sigma).");
    }
  } else {
    double p_dbl = (-1.0 + std::sqrt(1.0 + 24.0 * q)) / 6.0;
    p = static_cast<arma::uword>(std::round(p_dbl));
    if (p * p + p * (p + 1) / 2 != q) {
      Rcpp::stop(
          "When sigma_diag = FALSE, v must contain vec(phi) and vech(sigma).");
    }
  }
  arma::mat phi = arma::mat(v.subvec(0, p * p - 1)).reshape(p, p);
  arma::mat sigma(p, p, arma::fill::zeros);
  if (sigma_diag) {
    sigma.diag() = v.subvec(p * p, q - 1);
  } else {
    arma::vec sigma_vech = v.subvec(p * p, q - 1);
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
  }
  arma::mat total = arma::expmat(delta_t * phi);
  arma::mat cov_eta;
  arma::sylvester(cov_eta, phi, phi.t(), sigma);
  arma::vec sqrt_diag = arma::sqrt(cov_eta.diag());
  arma::mat total_std = total;
  for (arma::uword i = 0; i < total.n_rows; ++i) {
    for (arma::uword j = 0; j < total.n_cols; ++j) {
      total_std(i, j) *= sqrt_diag(j) / sqrt_diag(i);
    }
  }
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
  arma::mat total = arma::expmat(delta_t * phi);
  arma::mat cov_eta;
  arma::sylvester(cov_eta, phi, phi.t(), sigma);
  arma::vec sqrt_diag = arma::sqrt(cov_eta.diag());
  arma::mat total_std = total;
  for (size_t i = 0; i < total.n_rows; i++) {
    for (size_t j = 0; j < total.n_cols; j++) {
      total_std(i, j) *= sqrt_diag(j) / sqrt_diag(i);
    }
  }
  return total_std;
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
