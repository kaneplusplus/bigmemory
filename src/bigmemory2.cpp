#include <Rcpp.h>
#include "bigmemory/BigMatrix.h"
#include "bigmemory/util.h"

using namespace Rcpp;


/******************************************************************************/

// [[Rcpp::export]]
bool IsShared(XPtr<BigMatrix> pMat) {
  return pMat->shared();
}

/******************************************************************************/

// [[Rcpp::export]]
String GetTypeString(XPtr<BigMatrix> pMat) {
  switch(pMat->matrix_type()) {
  case 1:
    return "char";
  case 2:
    return "short";
  case 3:
    return "raw";
  case 4:
    return "integer";
  case 6:
    return "float";
  case 8:
    return "double";
  default:
    throw Rcpp::exception("unknown type detected for big.matrix object!");
  }
}

/******************************************************************************/

// [[Rcpp::export]]
SEXP GetColumnNamesBM(XPtr<BigMatrix> pMat) {
  Names cn = pMat->column_names();
  return cn.size() > 0 ? wrap(cn) : R_NilValue;
}

// [[Rcpp::export]]
SEXP GetRowNamesBM(XPtr<BigMatrix> pMat) {
  Names rn = pMat->row_names();
  return rn.size() > 0 ? wrap(rn) : R_NilValue;
}

/******************************************************************************/

// [[Rcpp::export]]
ListOf<SEXP> GetInfos(XPtr<BigMatrix> pMat) {
  return List::create(_["totalRows"] = pMat->total_rows(),
                      _["totalCols"] = pMat->total_columns(),
                      _["rowOffset"] = pMat->row_offset(),
                      _["colOffset"] = pMat->col_offset(),
                      _["nrow"] = pMat->nrow(),
                      _["ncol"] = pMat->ncol(),
                      _["rowNames"] = GetRowNamesBM(pMat),
                      _["colNames"] = GetColumnNamesBM(pMat),
                      _["type"] = GetTypeString(pMat),
                      _["separated"] = pMat->separated_columns());
}

/******************************************************************************/

// [[Rcpp::export]]
void SetRowOffsetInfo(XPtr<BigMatrix> pMat, 
                      index_type rowOffset, 
                      index_type numRows) {
  pMat->row_offset(rowOffset);
  pMat->nrow(numRows);
}

// [[Rcpp::export]]
void SetColumnOffsetInfo(XPtr<BigMatrix> pMat, 
                         index_type colOffset, 
                         index_type numCols) {
  pMat->col_offset(colOffset);
  pMat->ncol(numCols);
}

// [[Rcpp::export]]
index_type GetRowOffset(XPtr<BigMatrix> pMat) {
  return pMat->row_offset();
}

// [[Rcpp::export]]
index_type GetColOffset(XPtr<BigMatrix> pMat) {
  return pMat->col_offset();
}

/******************************************************************************/

// TO DELETE?

