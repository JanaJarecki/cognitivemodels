// ebm.cpp
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


// TODO outsosurce the distance function in the ebm.cpp script
// @body using the below lines of code in the ebm_cpp function
/*double dist_euclidean(Rcpp::NumericVector x; Rcpp::NumericVector y; Rcpp::NumericVector w, double r) {
  for (int i = 0; i < x.length(); i++) {
    res += w[i] * pow( fabs(x[i] - y[i]), r);
  } 
}*/

//' Computes Predictions for the Exemplar-based Models (GCM, EBM)
//' 
//' @param criterion numeric vector with experienced criterion
//' @param features numeric matrix with feature criterion
//' @param w numeric vector of weights (model parameter)
//' @param r square root in distance metic (model parameter)
//' @param q exponent in distance metric (model parameter)
//' @param lambda sensitivity (model parameter)
//' @param b bias parameter vector for classification (model parameter), must be NA for judgments
//' @param fw weight vector with a weight for each feature combination
//' @param lastLearnTrial integer last trial of learning phase
//' @param firstOutTrial integer first trial of output, starting the predictions later
//' @examples
//' # none
//'
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector ebm_cpp(
  Rcpp::NumericVector criterion,
  Rcpp::NumericMatrix features,
  Rcpp::NumericVector w,
  double r,
  double q,
  double lambda,
  Rcpp::NumericVector b,
  Rcpp::NumericVector wf,
  int lastLearnTrial,
  int firstOutTrial,
  double init,
  Rcpp::NumericVector has_criterion) {
  
  int ntrials = criterion.length() - firstOutTrial + 1;
  int nfeatures = features.ncol();
  int T = criterion.length();
  
  Rcpp::NumericVector dist(lastLearnTrial);
  Rcpp::NumericVector val(T);
  Rcpp::NumericVector sim(T);
  Rcpp::NumericVector res(ntrials);

  int i = 0; // a counter

  // initialize prediction in trial 1 in which no exemplar has been seen
  res[i] = init;

  firstOutTrial = (firstOutTrial == 1) ? firstOutTrial : firstOutTrial - 1;
  i = (firstOutTrial == 1) ? 1 : 0; // start at 1 if we predict all trials

  // compute the value starting from trial t (e.g. 2)
  for (int t = firstOutTrial; t < T; t++) {
    // compute the similarity between trial t and the thrials t - 1
    // up to maximally t or the lastLearnTrial
    int th_max = std::min(lastLearnTrial, t);

    // loop through history trials th 
    for (int th = 0; th < th_max; th++) {

      // substitute initial NAs (= no feedback shown yet)
      if (NumericVector::is_na(criterion[th])) {
        sim[t] = 1.0;
        val[t] = init;
        criterion[th] = 0;
        continue;
      }

      // Distance distance btwn. stimulus(t) and stimulus(th)
      dist[th] = 0.0; // initialize 
      for (int f = 0; f < nfeatures; f++) {
        dist[th] += w[f] * pow( fabs(features(t, f) - features(th, f)), r);
      }
      dist[th] = pow(dist[th], q / r); // distance -> similarity

      // Gaussian decay multiplied with criterion
      val[t] += exp(-1 * lambda * dist[th]) * criterion[th] * (NumericVector::is_na(b[0]) ? 1 : b[criterion[th]]);

      // add up all the similarities between t and all ths
      sim[t] += exp(-1 * lambda * dist[th]) * (NumericVector::is_na(b[0]) ? 1 : b[criterion[th]]) * wf[th] * has_criterion[th];
    }

    sim[t] = std::max(sim[t], DBL_EPSILON); // ensure sim[t] > 0    
    res[i] = val[t] / sim[t];
    i += 1;
  }

  return res;
}