#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;

// This fixed compiler bugs on mac os
#ifdef _OPENMP
  #include <omp.h>
#endif
#ifdef _OPENMP
  // multithreaded OpenMP version of code
#else
  // single-threaded version of code
#endif


// Indicator 1 "reached": Am I at the requirement (budget)?
// Indicator 2 "one": Can only one reach the requirement (budget)?
// Indicator 3 "sureY": Can one for sure reach the requirement (budget)?
// Indicator 4 "sureN": 1 - sureY

// Value 1: 
// Yes: default function
// Value 2:
// Yes: terminal reward of minimum outcome x timehorizon per option
// Value 3:
// Yes: terminal reward of maximum outcome x timehorizon per option
// No: reward of the expexted value x timehorizon

// Makes the indicator matrix lexicographic 
// sets all columns right to a row containing a 1 to 0
/*NumericMatrix lexicographic(NumericMatrix I) {
  int i = 1;
  for(unsigned int r = 0; r < I.nrow(); ++r) {
    i = 1;
    for (unsigned int j = 1; j < I.ncol(); ++j) {
      i *= (1 - I(r, (j-1)));
      I(r, j) = I(r, j) * i;
    }
  }
  return I;
}*/

// Indicator function for the nodes in the tree
// returns a matrix ntrials x depth of tree + 1
/*NumericMatrix indicators(arma::mat features,
      NumericVector splitcriteria) {

  unsigned int tdim = features.n_rows, // trials
               ndim = features.n_cols; // number of nodes

  // allocate the return
  NumericMatrix I = NumericMatrix(tdim, ndim+1);
  colnames(I) = CharacterVector({ "maxLV >= c1", "minHV >= c2", "p(max HV) >= c3", "p(max HV) <= c3"});
  
  // compute final value per option based on time x outcome 
 for (unsigned int t = 0; t < tdim; ++t) {
  for (unsigned int f = 0; f < ndim; ++f) {
    I(t, f) = features(t, f) >= splitcriteria(f);
  }
  I(t, ndim) = 1 - I(t, ndim-1);
  };
  
  I = lexicographic(I);
  return I;
}*/




// // Values to return at each node
// // Returns a matrix ntrials x depth of tree + 1+
// // [[Rcpp::export]]
// NumericMatrix nodevalues(NumericVector node_order,
//        NumericMatrix probs,
//        NumericMatrix outcomes,
//        NumericVector timehorizon,
//        NumericVector budget,
//        NumericVector state,
//        Function terminalreward) {

//   // get sizes
//   unsigned int ntrial = probs.nrow(),
//                noutc = probs.ncol();

//   // initialize budget per trial
//   NumericVector need = (budget - state) / timehorizon;
//   NumericMatrix reward = NumericMatrix(ntrial, noutc);

//   for (int j = 0; j < noutc; ++j) {
//       reward( _, j) = as<NumericVector>(terminalreward(_["budget"] = (budget), _["state"] = (outcomes( _, j) * timehorizon)));
//     reward( _, j) = as<NumericVector>(terminalreward(_["budget"] = need, _["state"] = outcomes( _, j)));
//   };
  
//   // allocate the return
//   NumericMatrix v = NumericMatrix(ntrial, 3);
  
//   // compute final value per option based on time x outcome 
//   for (int t = 0; t < ntrial; ++t) {
//     double max_reward = max(reward(t, _));
//     double min_reward = min(reward(t, _));
//     double prob_max_outcome =probs(t, which_max(outcomes(t,_))) * 10;
//     double prob_min_outcome =probs(t, which_min(outcomes(t,_))) * 10;
//     double max_outcome = max(outcomes(t,_));
//     double min_outcome = min(outcomes(t,_));
//     // sX-
//     v(t, node_order(0)) = max_outcome; // ~ select risky
//     // sX+rP-
//     v(t, node_order(1)) = max_outcome; // ~ select risky
//     // sX+rP+
//     v(t, 2) = min_outcome; // ~ select safe
//   
//     // R-O-S+
//     v(t, 2) = alpha * max_outcome + (1-alpha) * prob_max_outcome;
//     // R-O-S+
//     //v(t, 2) = ( min_reward == 0 ) * ( min_reward )+ ( min_reward > 0 ) * max_reward;
//     // R-O-S-I+
//     v(t, 3) = alpha * min_outcome + (1-alpha) * prob_min_outcome;
//     // R-O-S+
//     //v(t, 3) = outcomes(t, which_min(outcomes(t,_)));
//     // R-O-S-I-
//     v(t, 4) = 50 - 10 * max(probs(t,_));
//     // R-O-S+
//     //v(t, 4) = probs(t, which_min(outcomes(t,_))) * outcomes(t, which_min(outcomes(t,_)));
//     v(t, 1) = ( max_reward > 0 ) * ( max_reward );
//     v(t, 3) = - (max(outcomes(t,_) - min(outcomes(t,_))));
//      for (int j = 0; j < noutc; ++j) {
//       v(t, 3) += 
//       v(t, 3) += max( outcomes(t, _) );
//       v(t, 3) += reward(t,j) * probs(t,j);
//     };
//   };

//   // return the new matrix
//   return v;
// }