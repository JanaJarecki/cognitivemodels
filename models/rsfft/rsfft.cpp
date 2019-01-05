#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;

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
// [[Rcpp::export]]
NumericMatrix lexicographic(NumericMatrix I) {
  int i = 1;

  for(unsigned int r = 0; r < I.nrow(); ++r) {
    i = 1;
    for (unsigned int j = 1; j < I.ncol(); ++j) {
      i *= (1 - I(r, (i-1)));
      I(r, j) = I(r, j) * i;
    }
  }

  return I;
}

// Indicator function for the nodes in the tree
// returns a matrix ntrials x depth of tree + 1
// [[Rcpp::export]]
NumericMatrix indicators(NumericVector col_order,
      arma::cube outcomes,
      arma::cube probabilities,
      NumericVector timehorizon,
      NumericVector budget,
      NumericVector state) {

  // get sizes
  unsigned int tdim = outcomes.n_rows, // trials
               xdim = outcomes.n_cols, // outcomes
               adim = outcomes.n_slices; // actions

  // initialize budget per trial
  NumericVector need = (budget - state) / timehorizon;

  // allocate the return
  NumericMatrix I = NumericMatrix(tdim, 3);
/*  colnames(I) = CharacterVector({ "R+", "R-O+", "R-O-S+", "R-O-S-I+", "R-O-S-I-" });*/
 colnames(I) = CharacterVector({ "max_reaches+", "sX+rP-", "sX+rP+"});

  // Initialize tmp
  NumericMatrix tmpVarP = NumericMatrix(tdim, adim);
  
  // compute final value per option based on time x outcome 
 for (unsigned int t = 0; t < tdim; ++t) {
  for (unsigned int a = 0; a < adim; ++a) {
    // max reaches b
    
    tmpVarP(t, a) = max(probabilities.slice(a).row(t)) * min(probabilities.slice(a).row(t));
    // higher probability of min outcome?
     /* I(t, 2) += ((min(outcome(t, _)) - need(t)) >= 0);
      I(t, 3) += (max(outcome(t,_)) != max(as<NumericMatrix>(outcomes[0])(t,_))); // elimination by aspects: do the max outcomes differ?*/
    };
    // safe max outcome reaches budget
    unsigned int s_index = which_max(tmpVarP(t,_));
    unsigned int r_index = which_min(tmpVarP(t,_));
    unsigned int min_s_index = outcomes.slice(s_index).row(t).index_min();
    unsigned int min_r_index = outcomes.slice(r_index).row(t).index_min();
    I(t, col_order(0)) = max(outcomes.slice(s_index).row(t)) < need(t);
    I(t, col_order(1)) = as_scalar(probabilities.slice(r_index).row(t).col(min_r_index)) > as_scalar(probabilities.slice(s_index).row(t).col(min_s_index));
    I(t, 2) = 1 - I(t, 1);
    
    // last term implements the lexiography by setting the current term 0
    /*
    I(t, 0) = (state(t) - budget(t)) >= 0;
    I(t, 1) = (I(t, 1) == 1) * (1 - I(t, 0)); 
    I(t, 2) = (I(t, 2) == 1) * (1 - I(t, 1)) * (1 - I(t, 0));
    I(t, 3) = (I(t, 3) == 0) * (1 - I(t, 2)) * (1 - I(t, 1)) * (1 - I(t, 0));
    I(t, 4) = (1 - I(t, 3))  * (1 - I(t, 2)) * (1 - I(t, 1)) * (1 - I(t, 0));
    */
  };
  
  // return the new matrix
  I = lexicographic(I);
  return I;
}




// Values to return at each node
// Returns a matrix ntrials x depth of tree + 1+
// [[Rcpp::export]]
NumericMatrix nodevalues(NumericVector col_order,
       NumericMatrix probs,
       NumericMatrix outcomes,
       NumericVector timehorizon,
       NumericVector budget,
       NumericVector state,
       Function terminalreward) {

  // get sizes
  unsigned int ntrial = probs.nrow(),
               noutc = probs.ncol();

  // initialize budget per trial
  NumericVector need = (budget - state) / timehorizon;
  NumericMatrix reward = NumericMatrix(ntrial, noutc);

  for (int j = 0; j < noutc; ++j) {
   /* reward( _, j) = as<NumericVector>(terminalreward(_["budget"] = (budget), _["state"] = (outcomes( _, j) * timehorizon)));*/
    reward( _, j) = as<NumericVector>(terminalreward(_["budget"] = need, _["state"] = outcomes( _, j)));
  };
  
  // allocate the return
  NumericMatrix v = NumericMatrix(ntrial, 3);
  
  // compute final value per option based on time x outcome 
  for (int t = 0; t < ntrial; ++t) {
    double max_reward = max(reward(t, _));
    double min_reward = min(reward(t, _));
    double prob_max_outcome =probs(t, which_max(outcomes(t,_))) * 10;
    double prob_min_outcome =probs(t, which_min(outcomes(t,_))) * 10;
    double max_outcome = max(outcomes(t,_));
    double min_outcome = min(outcomes(t,_));
    // sX-
    v(t, col_order(0)) = max_outcome; // ~ select risky
    // sX+rP-
    v(t, col_order(1)) = max_outcome; // ~ select risky
    // sX+rP+
    v(t, 2) = min_outcome; // ~ select safe
  
  /*  // R-O-S+
    v(t, 2) = alpha * max_outcome + (1-alpha) * prob_max_outcome;
    // R-O-S+
    //v(t, 2) = ( min_reward == 0 ) * ( min_reward )+ ( min_reward > 0 ) * max_reward;
    // R-O-S-I+
    v(t, 3) = alpha * min_outcome + (1-alpha) * prob_min_outcome;
    // R-O-S+
    //v(t, 3) = outcomes(t, which_min(outcomes(t,_)));
    // R-O-S-I-
    v(t, 4) = 50 - 10 * max(probs(t,_));
    // R-O-S+
    //v(t, 4) = probs(t, which_min(outcomes(t,_))) * outcomes(t, which_min(outcomes(t,_)));
    /*v(t, 1) = ( max_reward > 0 ) * ( max_reward );*/
    /*v(t, 3) = - (max(outcomes(t,_) - min(outcomes(t,_))));
     for (int j = 0; j < noutc; ++j) {
      v(t, 3) += 
      v(t, 3) += max( outcomes(t, _) );
      v(t, 3) += reward(t,j) * probs(t,j);
    };*/
  };

  // return the new matrix
  return v;
}