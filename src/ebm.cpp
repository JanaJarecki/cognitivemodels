// ebm.cpp
#include <Rcpp.h>
using namespace Rcpp;
#include "cov.h"
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
//' @param r exponent in distance metic (model parameter)
//' @param q exponent in similarity function (model parameter)
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

//' Weighted Mahalanobis Distance
//' 
//' @param x A numeric vector, feature values of first object
//' @param y Like x, feature values of second object
//' @param s Inverted variance-covariance matrix
//' @param w numeric vector of weights (model parameter)
//' @param q exponent in similarity function (model parameter)
//' @examples
//' # none
// [[Rcpp::export]]
double mahalanobis(Rcpp::NumericVector x, Rcpp::NumericVector y, Rcpp::NumericMatrix s, Rcpp::NumericVector w, double q) {
  double dist = 0.0;
  Rcpp::NumericVector z(x.length());
  for(int i = 0; i < x.length(); i++) {
    for(int j = 0; j < x.length(); j++) {
      z[i] += w[j] * (x[j] - y[j]) * s(j, i);    
    }
    dist += z[i] * w[i] * (x[i] - y[i]);
  }
  dist = pow(dist, q / 2);
  return dist;
}

//' Computes Predictions for the Exemplar-based Models (GCM, EBM)
//' 
//' @param criterion numeric vector with experienced criterion
//' @param features numeric matrix with feature criterion
//' @param w numeric vector of weights (model parameter)
//' @param r order of Minkowski distance metic (model parameter)
//' @param q relation between similarity and distance (model parameter)
//' @param lambda sensitivity (model parameter)
//' @param b bias parameter vector for classification (model parameter), must be NA for judgments
//' @param wf weight vector with a weight for each feature combination
//' @param init value for the initial trials
//' @param has_criterion vector where a criterion is present
//' @param ismultiplicative A number (0 or 1), 1 means the combination of exemplars is multiplicative, i.e. multiplicative exemplar model
//' @param lastLearnTrial integer last trial of learning phase
//' @param firstOutTrial integer first trial of output, starting the predictions later
//' @param similarity A string, the similarity function
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
  int T = criterion.length();
  
  Rcpp::NumericVector sim(lastLearnTrial);
  Rcpp::NumericVector val(T);
  Rcpp::NumericVector sim_all(T);
  Rcpp::NumericVector res(ntrials);
  Rcpp::NumericVector criterion_unique = Rcpp::unique(criterion);
  Rcpp::List cov_list; 
  
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
    
    // creates list with each category's inverted variance covariance matrix
    if (similarity == "mahalanobis") {
      cov_list = invert_cov(features, criterion, features.ncol(), th_max);
    }

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
      
      if (similarity == "mahalanobis") {
        int n = std::count(criterion.begin(), criterion.begin() + th_max, criterion[th]); // count number of exemplars
        if (n > 2) { // calculates Mahalanobis distance if covariance matrix is estimable (i.e., n > 2)
          int curr_c;
          for (int c = 0; c < criterion_unique.length(); c++) {
            if (criterion_unique[c] == criterion[th]) {
              curr_c = c;
            }
          }
          Rcpp::NumericMatrix s = cov_list[curr_c];
          sim[th] = -1 * lambda * mahalanobis(features(t, _), features(th, _), s, w, q);
        } else { // if covariance matrix cannot be estimated, calculate Euclidean distance
          sim[th] = -1 * lambda * minkowski(features(t, _), features(th, _), w, r, q);
        }
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

    // sim_all[t] = std::max(sim_all[t], DBL_EPSILON); // ensure sim[t] > 0    
    if(sim_all[t] < DBL_EPSILON) {
      res[i] = R_NaN;
    } else{
      res[i] = val[t] / sim_all[t];
    }
    i += 1;
  }

  return res;
}