// cov.cpp
// adapted from https://www.geeksforgeeks.org/program-find-covariance/
#include <Rcpp.h>
using namespace Rcpp;
// This supposedly fixes compiler bugs on mac os
#ifdef _OPENMP
#include <omp.h>
#endif
/*#ifdef _OPENMP
 // multithreaded OpenMP version of code
#else
 // single-threaded version of code
#endif*/
 
// Function to find mean
float mean(Rcpp::NumericVector arr, int n) { 
  float sum = 0; 
  for(int i = 0; i < n; i++) 
    sum = sum + arr[i]; 
  return sum / n; 
} 


// Function to find covariance
float covariance(Rcpp::NumericVector arr1, Rcpp::NumericVector arr2, int n) { 
  float sum = 0; 
  for(int i = 0; i < n; i++) 
    sum = sum + (arr1[i] - mean(arr1, n)) * (arr2[i] - mean(arr2, n)); 
  return sum / (n - 1); 
} 
