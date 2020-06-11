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


//' Weighted Minkowski Distance
//' 
//' @param x A numeric vector, feature values of first object
//' @param y  Like x, feature values of second object
//' @param w numeric vector of weights (model parameter)
//' @param r square root in distance metic (model parameter)
//' @param q exponent in distance metric (model parameter)
//' @examples
//' # none
// [[Rcpp::export]]
double minkowski(Rcpp::NumericVector x, Rcpp::NumericVector y, Rcpp::NumericVector w, double r, double q) {
  double dist = 0.0;
  for (int i = 0; i < x.length(); i++) {
    dist += w[i] * pow( fabs(x[i] - y[i]), r);
  }
  dist = pow(dist, q / r);
  return dist;
}


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
//' @param similarity A string, the similarity function
//' @param is_additive A number (0 or 1), 0 means multiplicative exemplar model
//' @examples
//' # none
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
  Rcpp::NumericVector has_criterion,
  std::string similarity,
  int ismultiplicative) {
  
  int ntrials = criterion.length() - firstOutTrial + 1;
  int nfeatures = features.ncol();
  int T = criterion.length();
  
  Rcpp::NumericVector sim(lastLearnTrial);
  Rcpp::NumericVector val(T);
  Rcpp::NumericVector sim_all(T);
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

      // Similarity to stimulus at th
      if (similarity == "minkowski") {
        sim[th] = -1 * lambda * minkowski(features(t, _), features(th, _), w, r, q);
      }

      // Similarity x criterion value
      if (ismultiplicative == 1) {
        val[t] += exp(sim[th]) * criterion[th] * (NumericVector::is_na(b[0]) ? 1 : b[criterion[th]]);
        sim_all[t] += exp(sim[th]) * (NumericVector::is_na(b[0]) ? 1 : b[criterion[th]]) * wf[th] * has_criterion[th];
      } else {
        val[t] *= sim[th] * criterion[th] * (NumericVector::is_na(b[0]) ? 1 : b[criterion[th]]);
        sim_all[t] *= sim[th] * (NumericVector::is_na(b[0]) ? 1 : b[criterion[th]]) * wf[th] * has_criterion[th];
      }
    }

    sim_all[t] = std::max(sim_all[t], DBL_EPSILON); // ensure sim[t] > 0    
    res[i] = val[t] / sim_all[t];
    i += 1;
  }

  return res;
}