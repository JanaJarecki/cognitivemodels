// shortfall.cpp
#include <Rcpp.h>
using namespace Rcpp;
// This fixed compiler bugs on mac os
#ifdef _OPENMP
  #include <omp.h>
#endif
#ifdef _OPENMP
  // multithreaded OpenMP version of code
#else
  // single-threaded version of code
#endif

//' Computes Predictions in the Shortfall Model
//' 
//' @param x Numeric matrix, the outcomes
//' @param p Numeric matrix, the probabilities
//' @param a Numeric vector, the aspiration levels
//' @param beta Model parameter
//' @param delta Model parameter
//'
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector shortfall_cpp(
  Rcpp::NumericMatrix x,
  Rcpp::NumericMatrix p,
  Rcpp::NumericVector a,
  double beta,
  double delta) {
 
  int nt = x.nrow(); //num trials
  int nj = x.ncol(); //num features
  int t, j; //indexes trial, feature
  Rcpp::NumericVector res(nt); // result init

  for (t = 0; t < nt; t++) {
    double EV = 0.0;
    double R = 0.0;
    for (j = 0; j < nj; j++) {
      EV += p(t, j) * x(t, j);
      R  += p(t, j) * std::max((delta * a[t] - x(t, j)), 0.0);
    }
    res[t] = EV - beta * R;
  }
  return res;
}