// -----------------------------------------------------------------------------
// edit .setup/cpp/000-forward-declarations.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

bool TestPhi(const arma::mat& phi);
// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-direct-d-t-vec.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.DirectVec)]]
double DirectVec(const arma::vec& phi_vec, const double& delta_t,
                 const int& from, const int& to, const arma::vec& med) {
  int q = phi_vec.n_elem;
  int p = std::sqrt(q);
  arma::mat phi = arma::reshape(phi_vec, p, p);
  arma::mat d = arma::eye(p, p);
  int m = med.n_elem;
  for (int i = 0; i < m; i++) {
    d(med[i] - 1, med[i] - 1) = 0;
  }
  arma::mat direct = arma::mat(p, p);
  direct = arma::expmat(delta_t * d * phi * d);
  double direct_dbl = direct(to - 1, from - 1);
  return direct_dbl;
}
// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-direct-d-t.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.Direct)]]
double Direct(const arma::mat& phi, const double& delta_t, const int& from,
              const int& to, const arma::vec& med) {
  int p = phi.n_rows;
  arma::mat d = arma::eye(p, p);
  int m = med.n_elem;
  for (int i = 0; i < m; i++) {
    d(med[i] - 1, med[i] - 1) = 0;
  }
  arma::mat direct = arma::mat(p, p);
  direct = arma::expmat(delta_t * d * phi * d);
  double direct_dbl = direct(to - 1, from - 1);
  return direct_dbl;
}
// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-indirect-d-t-vec.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.IndirectVec)]]
double IndirectVec(const arma::vec& phi_vec, const double& delta_t,
                   const int& from, const int& to, const arma::vec& med) {
  int q = phi_vec.n_elem;
  int p = std::sqrt(q);
  arma::mat phi = arma::reshape(phi_vec, p, p);
  // total effect
  arma::mat total = arma::mat(p, p);
  total = arma::expmat(delta_t * phi);
  double total_dbl = total(to - 1, from - 1);
  // direct effect
  arma::mat d = arma::eye(p, p);
  int m = med.n_elem;
  for (int i = 0; i < m; i++) {
    d(med[i] - 1, med[i] - 1) = 0;
  }
  arma::mat direct = arma::mat(p, p);
  direct = arma::expmat(delta_t * d * phi * d);
  double direct_dbl = direct(to - 1, from - 1);
  // indirect effect
  double indirect_dbl = total_dbl - direct_dbl;
  return indirect_dbl;
}
// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-indirect-d-t.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.Indirect)]]
double Indirect(const arma::mat& phi, const double& delta_t, const int& from,
                const int& to, const arma::vec& med) {
  int p = phi.n_rows;
  // total effect
  arma::mat total = arma::mat(p, p);
  total = arma::expmat(delta_t * phi);
  double total_dbl = total(to - 1, from - 1);
  // direct effect
  arma::mat d = arma::eye(p, p);
  int m = med.n_elem;
  for (int i = 0; i < m; i++) {
    d(med[i] - 1, med[i] - 1) = 0;
  }
  arma::mat direct = arma::mat(p, p);
  direct = arma::expmat(delta_t * d * phi * d);
  double direct_dbl = direct(to - 1, from - 1);
  // indirect effect
  double indirect_dbl = total_dbl - direct_dbl;
  return indirect_dbl;
}
// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-mc-med-d-t.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.MCMed)]]
arma::mat MCMed(const arma::mat& phi, const arma::mat& vcov_phi_vec_l,
                const double& delta_t, const int& from, const int& to,
                const arma::vec& med, const int& R, bool test_phi = true) {
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
  int iter = 0;
  while (run) {
    // generate data
    arma::vec phi_vec_i = phi_vec + (vcov_phi_vec_l * arma::randn(q));
    phi_i = arma::reshape(phi_vec_i, p, p);
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
    int iter = 0;
    while (run) {
      // generate data
      arma::vec phi_vec_i = phi_vec + (vcov_phi_vec_l * arma::randn(q));
      arma::mat phi_i = arma::reshape(phi_vec_i, p, p);
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
        output[i] = phi_i;
      }
    }
  }
  return output;
}
// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-med-d-t-s.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.Meds)]]
const arma::mat Meds(const arma::mat& phi, const arma::vec& delta_t,
                     const int& from, const int& to, const arma::vec& med) {
  int t = delta_t.n_rows;
  arma::mat output = arma::mat(t, 4);
  int p = phi.n_rows;
  arma::mat total = arma::mat(p, p);
  double total_dbl;
  arma::mat d = arma::eye(p, p);
  int m = med.n_elem;
  for (int i = 0; i < m; i++) {
    d(med[i] - 1, med[i] - 1) = 0;
  }
  arma::mat direct = arma::mat(p, p);
  double direct_dbl;
  double indirect_dbl;
  for (int i = 0; i < t; i++) {
    // total effect
    total = arma::expmat(delta_t[i] * phi);
    total_dbl = total(to - 1, from - 1);
    // direct effect
    direct = arma::expmat(delta_t[i] * d * phi * d);
    direct_dbl = direct(to - 1, from - 1);
    // indirect effect
    indirect_dbl = total_dbl - direct_dbl;
    // output
    output(i, 0) = total_dbl;
    output(i, 1) = direct_dbl;
    output(i, 2) = indirect_dbl;
    output(i, 3) = delta_t[i];
  }
  return output;
}
// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-med-d-t.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.MedVec)]]
Rcpp::NumericVector MedVec(const arma::vec& phi_vec, const double& delta_t,
                           const int& from, const int& to,
                           const arma::vec& med) {
  Rcpp::NumericVector output(3);
  int q = phi_vec.n_elem;
  int p = std::sqrt(q);
  arma::mat phi = arma::reshape(phi_vec, p, p);
  // total effect
  arma::mat total = arma::mat(p, p);
  total = arma::expmat(delta_t * phi);
  double total_dbl = total(to - 1, from - 1);
  // direct effect
  arma::mat d = arma::eye(p, p);
  int m = med.n_elem;
  for (int i = 0; i < m; i++) {
    d(med[i] - 1, med[i] - 1) = 0;
  }
  arma::mat direct = arma::mat(p, p);
  direct = arma::expmat(delta_t * d * phi * d);
  double direct_dbl = direct(to - 1, from - 1);
  // indirect effect
  double indirect_dbl = total_dbl - direct_dbl;
  // output
  output(0) = total_dbl;
  output(1) = direct_dbl;
  output(2) = indirect_dbl;
  return output;
}
// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-med-d-t.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.Med)]]
Rcpp::NumericVector Med(const arma::mat& phi, const double& delta_t,
                        const int& from, const int& to, const arma::vec& med) {
  Rcpp::NumericVector output(4);
  int p = phi.n_rows;
  // total effect
  arma::mat total = arma::mat(p, p);
  total = arma::expmat(delta_t * phi);
  double total_dbl = total(to - 1, from - 1);
  // direct effect
  arma::mat d = arma::eye(p, p);
  int m = med.n_elem;
  for (int i = 0; i < m; i++) {
    d(med[i] - 1, med[i] - 1) = 0;
  }
  arma::mat direct = arma::mat(p, p);
  direct = arma::expmat(delta_t * d * phi * d);
  double direct_dbl = direct(to - 1, from - 1);
  // indirect effect
  double indirect_dbl = total_dbl - direct_dbl;
  // output
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

//' Test the Drift Matrix
//'
//' Both have to be true for the function to return `TRUE`.
//'   - Test that the largest eigen value of \eqn{\boldsymbol{\Phi}}
//'     is less than one.
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
  int p = phi.n_rows;
  arma::vec phi_diag(p);
  phi_diag = phi.diag(0);
  arma::cx_vec eigenvalues_phi = arma::eig_gen(phi);
  bool test;
  if (arma::all(arma::abs(eigenvalues_phi) < 1) && arma::all(phi_diag <= 0)) {
    test = true;
  } else {
    test = false;
  }
  return test;
}
// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-total-d-t-vec.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.TotalVec)]]
arma::vec TotalVec(const arma::vec& phi_vec, const double& delta_t) {
  int q = phi_vec.n_elem;
  int p = std::sqrt(q);
  arma::mat phi = arma::reshape(phi_vec, p, p);
  arma::mat total = arma::mat(p, p);
  total = arma::expmat(delta_t * phi);
  arma::vec total_vec = arma::vectorise(total);
  return total_vec;
}
// -----------------------------------------------------------------------------
// edit .setup/cpp/cTMed-total-d-t.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.Total)]]
arma::mat Total(const arma::mat& phi, const double& delta_t) {
  int p = phi.n_rows;
  arma::mat total = arma::mat(p, p);
  total = arma::expmat(delta_t * phi);
  return total;
}
