#include <Rcpp.h>
#include "bigmemory/BigMatrix.h"


// [[Rcpp::export]]
bool IsShared(XPtr<BigMatrix> pMat) {
  return pMat->shared();
}

// [[Rcpp::export]]
void SetRowOffsetInfo(XPtr<BigMatrix> pMat, double rowOffset, double numRows) {
  pMat->row_offset(rowOffset);
  pMat->nrow(numRows);
}

// [[Rcpp::export]]
void SetColumnOffsetInfo(XPtr<BigMatrix> pMat, double colOffset, double numCols) {
  pMat->col_offset(colOffset);
  pMat->ncol(numCols);
}

// [[Rcpp::export]]
double GetRowOffset(XPtr<BigMatrix> pMat) {
  return pMat->row_offset();
}

// [[Rcpp::export]]
double GetColOffset(XPtr<BigMatrix> pMat) {
  return pMat->col_offset();
}