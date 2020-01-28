// ebm.cpp
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

//' Exemplar-based prediction computation
//' 
//' @param x numeric matrix with outcomes
//' @param p numeric matrix with probabilities
//' @param w numeric vector with aspiration leel
//' @param beta parameter
//' @param delta parameter
//' '
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