// inv.cpp
// taken from https://www.geeksforgeeks.org/adjoint-inverse-matrix/
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
 
// Function to get cofactor of A in temp 
void getCofactor(Rcpp::NumericMatrix A, Rcpp::NumericMatrix temp, int p, int q, int n) { 
  int i = 0, j = 0; 
  
  // Looping for each element of the matrix 
  for (int row = 0; row < n; row++) { 
    for (int col = 0; col < n; col++) { 
      //  Copying into temporary matrix only those element 
      //  which are not in given row and column 
      if (row != p && col != q) { 
        temp(i, j++) = A(row, col); 
        
        // Row is filled, so increase row index and 
        // reset col index 
        if (j == n - 1) { 
          j = 0; 
          i++; 
        } 
      } 
    } 
  } 
} 

// Recursive function for finding determinant of matrix 
float determinant(Rcpp::NumericMatrix A, int n) { 
  float D = 0; // Initialize result 
  
  //  Base case : if matrix contains single element 
  if (n == 1) 
    return A(0, 0); 
  
  Rcpp::NumericMatrix temp(n - 1, n - 1); // To store cofactors 
  
  int sign = 1;  // To store sign multiplier 
  
  // Iterate for each element of first row 
  for (int f = 0; f < n; f++) { 
    // Getting Cofactor of A[0][f] 
    getCofactor(A, temp, 0, f, n); 
    D += sign * A(0, f) * determinant(temp, n - 1); 
    
    // terms are to be added with alternate sign 
    sign = -sign; 
  } 
  
  return D; 
} 

// Function to get adjoint of A in adj
void adjoint(Rcpp::NumericMatrix A, Rcpp::NumericMatrix adj) { 
  int N = A.ncol();
  if (N == 1) { 
    adj(0, 0) = 1; 
    return; 
  } 
  
  // temp is used to store cofactors of A[][] 
  int sign = 1;
  Rcpp::NumericMatrix temp(N - 1, N - 1); 
  
  for (int i = 0; i < N; i++) { 
    for (int j = 0; j < N; j++) { 
      // Get cofactor of A[i][j] 
      getCofactor(A, temp, i, j, N); 
      
      // sign of adj[j][i] positive if sum of row 
      // and column indexes is even. 
      sign = ((i + j) % 2 == 0) ? 1 : -1; 
      
      // Interchanging rows and columns to get the 
      // transpose of the cofactor matrix 
      adj(j, i) = (sign)*(determinant(temp, N-1)); 
    } 
  } 
} 

// Function to calculate inverse
Rcpp::NumericMatrix inverse(Rcpp::NumericMatrix A) { 
  int N = A.ncol();
  Rcpp::NumericMatrix inv(N, N);
  
  // Find determinant of A 
  double det = determinant(A, N); 
  if (det == 0) 
  { 
    return inv; 
  } 
  
  // Find adjoint 
  Rcpp::NumericMatrix adj(N, N); 
  adjoint(A, adj); 
  
  // Find Inverse using formula "inverse(A) = adj(A)/det(A)" 
  for (int i = 0; i < N; i++) 
    for (int j = 0; j < N; j++) 
      inv(i, j) = adj(i, j)/float(det); 
  
  return inv; 
}