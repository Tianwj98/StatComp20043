#include <Rcpp.h>
using namespace Rcpp;

double lap_f(double x){
  return 0.5 * exp(-abs(x));
}

//' @title Metropolis sampler for Laplace distribution using Rcpp
//' @description Implement a random walk Metropolis sampler for generating the standard Laplace distribution.
//' @param sigma The Variance of normal distribution.
//' @param x0 Initial value.
//' @param N The length of the chain.
//' @return A chain.
//' @examples
//' \dontrun{
//' N <- 2000
//' sigma <- 0.5
//' x0 <- 25
//' rw1 <- rw.Metropolis(sigma, x0, N)
//' ｝
//' @export
// [[Rcpp::export]]
NumericVector MetropolisC(double sigma, double x0, int N){
  NumericVector x(N);
  x[0] = x0;
  NumericVector u(N);  
  u = runif(N);
  int k = 0;
  for (int i = 1; i < N; ++i){
    NumericVector y(1);
    y = rnorm(1, x[i-1],sigma);
    if ( u[i] <= lap_f(y[0])/lap_f(x[i-1])) 
      x[i] = y[0] ;
    else{
      x[i] = x[i-1];
      k = k+1;
    }       
  }
  return x;
} 