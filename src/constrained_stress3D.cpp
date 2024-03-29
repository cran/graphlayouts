#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double constrained_stress3D(NumericMatrix x, NumericMatrix W, NumericMatrix D) {
  double fct = 0;
  int n = x.nrow();
  for (int i = 0; i < (n - 1); ++i) {
    for (int j = (i + 1); j < n; ++j) {
      double denom = sqrt((x(i, 0) - x(j, 0)) * (x(i, 0) - x(j, 0)) +
                          (x(i, 1) - x(j, 1)) * (x(i, 1) - x(j, 1)) +
                          (x(i, 2) - x(j, 2)) * (x(i, 2) - x(j, 2)));
      fct += W(i, j) * (denom - D(i, j)) * (denom - D(i, j));
    }
  }
  return fct;
}

// [[Rcpp::export]]
NumericMatrix constrained_stress_major3D(NumericMatrix y, int dim,
                                         NumericMatrix W, NumericMatrix D,
                                         int iter, double tol) {
  int n = y.nrow();
  NumericMatrix x(clone(y));

  NumericVector wsum = rowSums(W);

  double stress_old = constrained_stress3D(x, W, D);

  for (int k = 0; k < iter; ++k) {
    NumericMatrix xnew(n, 3);
    xnew(_, dim - 1) = y(_, dim - 1);

    for (int i = 0; i < n; ++i) {
      for (int j = 0; j < n; ++j) {
        if (i != j) {
          double denom = sqrt(sum(pow(x(i, _) - x(j, _), 2)));
          if (denom > 0.00001) {
            for (int d = 0; d < 3; ++d) {
              if (d != dim - 1) {
                xnew(i, d) +=
                    W(i, j) * (x(j, d) + D(i, j) * (x(i, d) - x(j, d)) / denom);
              }
            }
          }
        }
      }
      for (int d = 0; d < 3; ++d) {
        if (d != dim - 1) {
          xnew(i, d) /= wsum[i];
        }
      }
    }

    double stress_new = constrained_stress3D(xnew, W, D);
    double eps = (stress_old - stress_new) / stress_old;

    if (eps <= tol) {
      break;
    }

    stress_old = stress_new;
    x = clone(xnew);
  }
  return x;
}
