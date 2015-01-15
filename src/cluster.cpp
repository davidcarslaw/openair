#include <Rcpp.h>
using namespace Rcpp;

// function to calculate Eucledian distance for trajectories
RcppExport SEXP distEuclid(SEXP x, SEXP y, SEXP z){
  NumericMatrix Am(x); // trajectory matrix of lats
  NumericMatrix Bm(y); // trajectory matrix of lons
  NumericMatrix Zm(z); // results matrix
  double sum = 0.0;
  int ncolumns = Am.ncol();
  int nrows = Am.nrow();
  for (int i = 0; i < ncolumns; i++) {
    for (int j = 0; j < ncolumns; j++) {
      sum = 0;
      if (j >= i) { // only need lower triangle
        for (int k = 0; k < nrows; k++) {
          sum += pow(Am(k , i) - Am(k , j), 2)   + pow(Bm(k , i) - Bm(k , j), 2);
        }
        Zm(j, i) = sqrt(sum);
      }
    }
  }
  return Zm ;
}


// distance matrix based on similarity of trajectory angles
RcppExport SEXP distAngle(SEXP x, SEXP y, SEXP z){
  NumericMatrix Lonm(x); // matrix of longitudes
  NumericMatrix Latm(y); // matrix of latitudes
  NumericMatrix Zm(z);
  double A, B, C; // intermediate trajectory variables
  double X0, Y0; // origin of lon, lat
  double sum = 0.0;
  int ncolumns = Lonm.ncol();
  int nrows = Lonm.nrow();
  X0 = (double) Lonm(nrows - 1, 0);
  Y0 = (double) Latm(nrows - 1, 0);
  for (int i = 0; i < ncolumns; i++) {
    for (int j = 0; j < ncolumns; j++) {
      sum = 0;
      if (j > i) { // only need lower triangle
        for (int k = 0; k < nrows - 1; k++) { // do not include origin
          A = pow((Lonm(k, i) - X0), 2) + pow((Latm(k, i) - Y0), 2);
          B = pow((Lonm(k, j) - X0), 2) + pow((Latm(k, j) - Y0), 2);
          C = pow((Lonm(k, j) - Lonm(k, i)), 2) + pow((Latm(k, j) - Latm(k, i)), 2);
          sum += acos((0.5 * (A + B - C) / pow(A * B, 0.5)));
        }
        Zm(j, i) = sum / (nrows - 1) ;
      }
    }
  }
  return Zm;
}
