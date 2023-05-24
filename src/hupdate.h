#ifndef GSDESIGN2_SRC_HUPDATE_H
#define GSDESIGN2_SRC_HUPDATE_H

#include <Rcpp.h>

Rcpp::List hupdate_rcpp(int r,
                        double theta,
                        double I,
                        double a,
                        double b,
                        double thetam1,
                        double Im1,
                        Rcpp::List gm1);

#endif // GSDESIGN2_SRC_HUPDATE_H
