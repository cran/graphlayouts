#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double constrained_stress(NumericMatrix x, NumericMatrix W, NumericMatrix D) {
  double fct = 0;
  int n = x.nrow();
  for (int i = 0; i < (n - 1); ++i) {
    for (int j = (i + 1); j < n; ++j) {
      double denom = sqrt((x(i, 0) - x(j, 0)) * (x(i, 0) - x(j, 0)) +
                          (x(i, 1) - x(j, 1)) * (x(i, 1) - x(j, 1)));
      fct += W(i, j) * (denom - D(i, j)) * (denom - D(i, j));
    }
  }
  return fct;
}

// [[Rcpp::export]]
NumericMatrix constrained_stress_major(NumericMatrix y, int dim,
                                       NumericMatrix W, NumericMatrix D,
                                       int iter, double tol) {
  int n = y.nrow();
  NumericMatrix x(clone(y));

  NumericVector wsum = rowSums(W);

  double stress_old = constrained_stress(x, W, D);

  for (int k = 0; k < iter; ++k) {
    NumericMatrix xnew(n, 2);
    xnew(_, dim - 1) = y(_, dim - 1);

    for (int i = 0; i < n; ++i) {
      for (int j = 0; j < n; ++j) {
        if (i != j) {
          double denom = sqrt(sum(pow(x(i, _) - x(j, _), 2)));
          if (denom > 0.00001) {
            int updateDim = 1 - (dim - 1);
            xnew(i, updateDim) +=
                W(i, j) *
                (x(j, updateDim) +
                 D(i, j) * (x(i, updateDim) - x(j, updateDim)) / denom);
          }
        }
      }
      int updateDim = 1 - (dim - 1);
      xnew(i, updateDim) /= wsum[i];
    }

    double stress_new = constrained_stress(xnew, W, D);
    double eps = (stress_old - stress_new) / stress_old;

    if (eps <= tol) {
      break;
    }

    stress_old = stress_new;
    x = clone(xnew);
  }
  return x;
}

NumericMatrix replaceNA(NumericMatrix x, NumericMatrix y) {
  int nrow = x.nrow(), ncol = x.ncol();

  for (int i = 0; i < nrow; ++i) {
    for (int j = 0; j < ncol; ++j) {
      if (!NumericMatrix::is_na(y(i, j))) {
        x(i, j) = y(i, j);
      }
    }
  }

  return x;
}

// [[Rcpp::export]]
NumericMatrix fixed_stress_major(NumericMatrix y, NumericMatrix fixedCoords,
                                 NumericMatrix W, NumericMatrix D, int iter,
                                 double tol) {
  int n = y.nrow();
  NumericMatrix x(clone(y));

  NumericVector wsum = rowSums(W);

  double stress_old = constrained_stress(x, W, D);

  for (int k = 0; k < iter; ++k) {
    NumericMatrix xnew(n, 2);
    std::fill(xnew.begin(), xnew.end(), 0);
    xnew = replaceNA(xnew, fixedCoords);

    for (int i = 0; i < n; ++i) {
      for (int j = 0; j < n; ++j) {
        if (i != j) {
          double denom = sqrt(sum(pow(x(i, _) - x(j, _), 2)));
          if ((denom > 0.00001)) {
            if (!NumericMatrix::is_na(fixedCoords(i, 0))) {
              xnew(i, 0) +=
                  W(i, j) * (x(j, 0) + D(i, j) * (x(i, 0) - x(j, 0)) / denom);
            }
            if (!NumericMatrix::is_na(fixedCoords(i, 1))) {
              xnew(i, 1) +=
                  W(i, j) * (x(j, 1) + D(i, j) * (x(i, 1) - x(j, 1)) / denom);
            }
          }
        }
      }
      if (!NumericMatrix::is_na(fixedCoords(i, 0))) {
        xnew(i, 0) = xnew(i, 0) / wsum[i];
      }
      if (!NumericMatrix::is_na(fixedCoords(i, 1))) {
        xnew(i, 1) = xnew(i, 1) / wsum[i];
      }
    }

    double stress_new = constrained_stress(xnew, W, D);
    double eps = (stress_old - stress_new) / stress_old;

    if (eps <= tol) {
      break;
    }

    stress_old = stress_new;
    x = clone(xnew);
  }
  return x;
}
