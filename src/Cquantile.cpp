#include <Rcpp.h>
using namespace Rcpp;

// Basic function to do fast quantiles
// Better with larger vectors
// Need to implement Type 7 (R) version...

// [[Rcpp::export]]
NumericVector Cquant(NumericVector A, NumericVector probs) {
  
  NumericVector q(probs) ;
  NumericVector y = wrap(na_omit(A)) ;
  
  std::sort(y.begin(), y.end());
  return y[y.size()*(q - 0.000000001)];
}

