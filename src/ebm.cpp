// ebm.cpp
#include <Rcpp.h>
using namespace Rcpp;

//' Exemplar-based prediction computation
//' 
//' @param values numeric vector with experienced values
//' @param features numeric matrix with feature values
//' @param w numeric vector of weights (model parameter)
//' @param r square root in distance metic (model parameter)
//' @param q exponent in distance metric (model parameter)
//' @param lambda sensitivity (model parameter)
//' @param b bias parameter vector for classification (model parameter), must be NA for judgments
//' @param lastLearnTrial integer last trial of learning phase
//' @param firstOutTrial integer first trial of output, starting the predictions later
//' @examples
//' # none
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector ebm_cpp(
  Rcpp::NumericVector values,
  Rcpp::NumericMatrix features,
  Rcpp::NumericVector w,
  double r,
  double q,
  double lambda,
  Rcpp::NumericVector b,
  int lastLearnTrial,
  int firstOutTrial) {
  
  int ntrials = values.length() - firstOutTrial + 1;
  int nfeatures = features.ncol();
  
  Rcpp::NumericVector dist(lastLearnTrial);
  Rcpp::NumericVector val(values.length());
  Rcpp::NumericVector sim(values.length());
  Rcpp::NumericVector res(ntrials);

  int i = 0; //a counter that starts at 0
  res[i] = NA_REAL; //initialize first result to NA

  firstOutTrial = (firstOutTrial == 1) ? firstOutTrial : firstOutTrial - 1;
  i = (firstOutTrial == 1) ? 1 : 0; //start at 1 if we predict all trials, otherweise at 0

  for (int t = firstOutTrial; t < values.length(); t++) {
    // compute the value starting from trial t (e.g. 2)
    
    // compute the similarity between trial t and the previous_trials t - 1
    // up to maximally t or the lastLearnTrial
    int previous_t_max = std::min(lastLearnTrial, t);

    for (int previous_t = 0; previous_t < previous_t_max; previous_t++) {
      
      // initialize the distance between t and previous_t to 0
      dist[previous_t] = 0.0;

      // compute the distance between t and previous_t
      for (int f = 0; f < nfeatures; f++) {
        dist[previous_t] += w[f] * pow( fabs(features(t, f) - features(previous_t, f)), r);
      }
      // transform the distance into a similarity
      dist[previous_t] = pow(dist[previous_t], q / r);

      /*// multiply the similarity between t and previous_t with the value of previous_t
      // and add the result up for all previous_ts
      val[t] += exp(-1 * lambda * dist[previous_t]) * values[previous_t];

      // add all the similarities between t and all previous_ts up
      sim[t] += exp(-1 * lambda * dist[previous_t]);*/
      // multiply the similarity between t and previous_t with the value of previous_t
      // and add the result up for all previous_ts
      val[t] += exp(-1 * lambda * dist[previous_t]) * values[previous_t] * (NumericVector::is_na(b[0]) ? 1 : b[1]);

      // add all the similarities between t and all previous_ts up
      sim[t] += exp(-1 * lambda * dist[previous_t]) * (NumericVector::is_na(b[0]) ? 1 : b[values[previous_t]]);
    }

    // standardize the value sum by the similarity sum and store in the ith row of the result
    if (sim[t] == 0) {
      sim[t] = DBL_EPSILON; // ensure sim[t] > 0
    }
    
    res[i] = val[t] / sim[t];
    i += 1;
  }

  return res;
}