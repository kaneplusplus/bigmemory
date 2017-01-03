// [[Rcpp::depends(bigmemory, BH)]]
#include <Rcpp.h>
#include <bigmemory/MatrixAccessor.hpp>

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector produ1(XPtr<BigMatrix> xpMat, 
                             const NumericVector& x) {
  
  MatrixAccessor<double> macc(*xpMat);
  
  int n = xpMat->nrow();
  int m = xpMat->ncol();
  
  NumericVector res(n);
  
  int i, j;
  double xj;
  
  for (j = 0; j < m; j++) {
    xj = x[j];
    for (i = 0; i < n; i++) {
      res[i] += xj * macc[j][i];
    }
  }
  
  return res;
}

// [[Rcpp::export]]
NumericVector produ2(XPtr<BigMatrix> xpMat, 
                     const NumericVector& x) {
  
  MatrixAccessor<double> macc(*xpMat);
  
  int n = xpMat->nrow();
  int m = xpMat->ncol();
  
  NumericVector res(n);
  
  int i, j;
  double xj;
  
  for (j = 0; j < m; j++) {
    xj = x[j];
    for (i = 0; i < n; i++) {
      res[i] += xj * macc(i, j);
    }
  }
  
  return res;
}