#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List gridpts_rcpp(int r,
                  double mu,
                  double a,
                  double b)
{
    // Define odd numbered grid points for real line
    NumericVector x(6 * r - 1);
    for (int i = 0; i < r - 1; i++)
    {
        double tmp = 3 + 4 * log(r / (double)(i + 1));
        x[i] = mu - tmp;
        x[6 * r - 2 - i] = mu + tmp;
    }
    for (int i = r - 1; i <= 5 * r - 1; i++)
    {
        x[i] = mu - 3 + 3 * (i - (r - 1)) / (double)(2 * r);
    }

    // Trim points outside of [a, b] and include those points
    if (min(x) < a)
    {
        x = x[x > a];
        x.insert(x.begin(), a);
    }
    if (max(x) > b)
    {
        x = x[x < b];
        x.push_back(b);
    }

    // If extreme, include only 1 point where density will be essentially 0
    int m = x.size();
    if (m == 1)
        return List::create(Named("z") = x, Named("w") = 1);

    // Initialize output vectors
    NumericVector z(2 * m - 1);
    NumericVector w(2 * m - 1);

    // The first two points with corresponding weights
    z[0] = x[0];
    z[1] = (x[0] + x[1]) / (double)2;
    w[0] = x[1] - x[0];
    w[1] = 4 * (x[1] - x[0]);

    for (int i = 2; i <= 2 * m - 4; i += 2)
    {
        z[i] = x[i / 2];                                  // Odd grid points
        z[i + 1] = (x[i / 2] + x[i / 2 + 1]) / (double)2; // Even grid points
        w[i] = x[i / 2 + 1] - x[i / 2 - 1];               // Odd weights
        w[i + 1] = 4 * (x[i / 2 + 1] - x[i / 2]);         // Even weights
    }

    // Last odd point with corresponding weight
    z[2 * m - 2] = x[m - 1];
    w[2 * m - 2] = x[m - 1] - x[m - 2];

    // Divide weights by 6
    w = w / (double)6;

    return List::create(
        Named("z") = z,
        Named("w") = w);
}
