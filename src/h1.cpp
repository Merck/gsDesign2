#include <Rcpp.h>
#include "gridpts.h"
using namespace Rcpp;

// [[Rcpp::export]]
List h1_rcpp(int r,
             double theta,
             double I,
             double a,
             double b)
{
    // Compute drift at analysis 1
    double mu = theta * sqrt(I);
    List g = gridpts_rcpp(r, mu, a, b);
    SEXP zz = g[0];
    NumericVector z(zz);
    SEXP ww = g[1];
    NumericVector w(ww);
    // Compute deviation from drift
    NumericVector h = w * dnorm(z - mu);
    // Compute standard normal density, multiply by grid weight and return
    // values needed for numerical integration

    return List::create(
        Named("z") = z,
        Named("w") = w,
        Named("h") = h);
}
