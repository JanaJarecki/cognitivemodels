#ifndef __COV__
#define __COV__

float mean(Rcpp::NumericVector arr, int n);
float covariance(Rcpp::NumericVector arr1, Rcpp::NumericVector arr2, int n);
void getCofactor(Rcpp::NumericMatrix A, Rcpp::NumericMatrix temp, int p, int q, int n);
float determinant(Rcpp::NumericMatrix A, int n);
void adjoint(Rcpp::NumericMatrix A, Rcpp::NumericMatrix adj);
Rcpp::NumericMatrix inverse(Rcpp::NumericMatrix A);
Rcpp::List invert_cov(Rcpp::NumericMatrix features, Rcpp::NumericVector criterion, int nfeatures);
  
#endif // __COV__