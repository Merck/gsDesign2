#include <Rcpp.h>
#include <algorithm>
using namespace Rcpp;

// [[Rcpp::export(".gridptsRcpp")]]
List gridptsRcpp(int r, double mu, double a, double b)
{
  
  // Define odd numbered grid points for real line
  NumericVector x(6*r-1);
  for (int i = 0; i < r-1; i++) {
    double tmp = 3 + 4 * log(r/(double)(i+1));
    x[i] = mu - tmp;
    x[6*r-2-i] = mu + tmp;
  }
  for (int i = r-1; i <= 5*r-1; i++) {
    x[i] =  mu - 3 + 3 * (i - (r-1)) / (double)(2*r);
  }
  
  // Trim points outside of [a, b] and include those points
  if (min(x) < a) {
    x = x[x > a];
    x.insert(x.begin(), a);
  }
  if (max(x) > b) {
    x = x[x < b];
    x.push_back(b);
  }
  
  // If extreme, include only 1 point where density will be essentially 0
  int m = x.size();
  if (m == 1) return List::create(Named("z") = x, Named("w") = 1);
  
  // Initialize output vectors
  NumericVector z(2*m-1);
  NumericVector w(2*m-1);
  
  // First two points with corresponding weights
  z[0] = x[0];
  z[1] = (x[0] + x[1]) / (double)2;
  w[0] = x[1] - x[0];
  w[1] = 4 * (x[1] - x[0]);
  
  for (int i = 2; i <= 2*m-4; i+=2) {
    z[i] = x[i/2];                            // odd grid points
    z[i+1] = (x[i/2] + x[i/2+1]) / (double)2; // even grid points
    w[i] = x[i/2+1] - x[i/2-1];               // odd weights
    w[i+1] = 4 * (x[i/2+1] - x[i/2]);         // even weights
  }
  
  // Last odd point with corresponding weight
  z[2*m-2] = x[m-1];
  w[2*m-2] = x[m-1] - x[m-2];
  
  // Divide weights by 6
  w = w / (double)6;
  
  return List::create(Named("z") = z,
                      Named("w") = w);
}

// [[Rcpp::export(".h1Rcpp")]]
List h1Rcpp(int r, double theta, double I, double a, double b)
{
  // compute drift at analysis 1
  double mu = theta * sqrt(I);
  List g = gridptsRcpp(r, mu, a, b);
  SEXP zz = g[0]; NumericVector z(zz);
  SEXP ww = g[1]; NumericVector w(ww);
  // compute deviation from drift
  NumericVector h = w * dnorm(z - mu);
  // compute standard normal density, multiply by grid weight and return
  // values needed for numerical integration
  
  return List::create(Named("z") = z,
                      Named("w") = w,
                      Named("h") = h);
}

// [[Rcpp::export(".hupdateRcpp")]]
List hupdateRcpp(int r, double theta, double I, double a, double b,
                 double thetam1, double Im1, List gm1){
  // sqrt of change in information
  double rtdelta = sqrt(I - Im1);
  double rtI = sqrt(I);
  double rtIm1 = sqrt(Im1);
  List g = gridptsRcpp(r, theta * rtI, a, b);
  SEXP zz = g[0]; NumericVector z(zz);
  SEXP ww = g[1]; NumericVector w(ww);
  SEXP zzm1 = gm1[0]; NumericVector zm1(zzm1);
  SEXP hhm1 = gm1[2]; NumericVector hm1(hhm1);
  // update integration
  double mu = theta * I - thetam1 * Im1;
  double d = rtI / rtdelta;
  NumericVector t = (zm1 * rtIm1 + mu) / rtdelta;
  NumericVector h(z.size());
  NumericVector x(zm1.size());
  for(int i = 0; i < z.size(); i++){
    x = dnorm(z[i] * d - t);
    h[i] = std::inner_product(hm1.begin(), hm1.end(), x.begin(), 0.0);
  }
  h = h * w * d;
  
  return List::create(Named("z") = z,
                      Named("w") = w,
                      Named("h") = h);
}
