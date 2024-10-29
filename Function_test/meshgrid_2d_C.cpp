#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List meshgrid_2d_C(NumericVector x, NumericVector y) {
  int nx = x.size();
  int ny = y.size();
  
  // Create arrays for the meshgrid
  NumericMatrix xx(ny, nx);
  NumericMatrix yy(ny, nx);
  
  // Populate the meshgrid
  for (int i = 0; i < ny; i++) {
    for (int j = 0; j < nx; j++) {
      xx(i, j) = x[j];
      yy(i, j) = y[i];
    }
  }
  
  // Return the meshgrid as a list of matrices
  return List::create(Named("X") = xx, Named("Y") = yy);
}
