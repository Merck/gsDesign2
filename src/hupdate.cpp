#include <Rcpp.h>
#include <algorithm>
#include "gridpts.h"
using namespace Rcpp;

// [[Rcpp::export]]
List hupdate_rcpp(int r,
                  double theta,
                  double I,
                  double a,
                  double b,
                  double thetam1,
                  double Im1,
                  List gm1)
{
    // Square root of change in information
    double rtdelta = sqrt(I - Im1);
    double rtI = sqrt(I);
    double rtIm1 = sqrt(Im1);
    List g = gridpts_rcpp(r, theta * rtI, a, b);
    SEXP zz = g[0];
    NumericVector z(zz);
    SEXP ww = g[1];
    NumericVector w(ww);
    SEXP zzm1 = gm1[0];
    NumericVector zm1(zzm1);
    SEXP hhm1 = gm1[2];
    NumericVector hm1(hhm1);
    // Update integration
    double mu = theta * I - thetam1 * Im1;
    double d = rtI / rtdelta;
    NumericVector t = (zm1 * rtIm1 + mu) / rtdelta;
    NumericVector h(z.size());
    NumericVector x(zm1.size());
    for (int i = 0; i < z.size(); i++)
    {
        x = dnorm(z[i] * d - t);
        h[i] = std::inner_product(hm1.begin(), hm1.end(), x.begin(), 0.0);
    }
    h = h * w * d;

    return List::create(
        Named("z") = z,
        Named("w") = w,
        Named("h") = h);
}
