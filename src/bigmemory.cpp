
#include <fstream>
//#include <typeinfo>

#define STRICT_R_HEADERS
#include <Rcpp.h>
#include "bigmemory/BigMatrix.h"
#include "bigmemory/MatrixAccessor.hpp"
#include "bigmemory/isna.hpp"

#include "bigmemory/util.h"

#define R_BYTE_MIN (0)
#define R_BYTE_MAX (255)
#define NA_BYTE (0)


using namespace Rcpp;

/* Notes
 * R does not natively contain float type objects
 * Therefore, every time you pass object to see they will initially be
 * double unless they are already within a C/C++ object.
 *
 * For example, the SetMatrixElements function
 * Normally the function looks like this:
 *  SetMatrixElements<double, double, MatrixAccessor<double> >(...

 * Where both the CType and RType are double but with float
 * types R is still passing only double.  Trying to pass RType
 * as float will result in all NA values.  So the function ultimately
 * must still pass double like so:
 *  SetMatrixElements<float, double, MatrixAccessor<float> >(...
 */


template<typename T>
string ttos(T i)
{
  stringstream s;
  s.precision(16);
  s << i;
  return s.str();
}

template<>
string ttos<unsigned char>(unsigned char i)
{
  stringstream s;
  s << static_cast<short>(i);
  return s.str();
}

template<>
string ttos<char>(char i)
{
	stringstream s;
	s << static_cast<short>(i);
	return s.str();
}

bool TooManyRIndices( index_type val )
{
  return double(val) > pow(2.0, 31.0)-1.0;
}

template<typename CType, typename RType, typename BMAccessorType>
void SetMatrixElements( BigMatrix *pMat, SEXP col, SEXP row, SEXP values,
  double NA_C, double C_MIN, double C_MAX, double NA_R)
{
  BMAccessorType mat( *pMat );
  double *pCols = REAL(col);
  index_type numCols = Rf_length(col);
  double *pRows = REAL(row);
  index_type numRows = Rf_length(row);
  VecPtr<RType> vec_ptr;
  RType *pVals = vec_ptr(values);
  index_type valLength = Rf_length(values);
  index_type i=0;
  index_type j=0;
  index_type k=0;
  CType *pColumn;
  index_type kIndex;
  for (i=0; i < numCols; ++i)
  {
    pColumn = mat[static_cast<index_type>(pCols[i])-1];
    for (j=0; j < numRows; ++j)
    {
      kIndex = k++%valLength;
      pColumn[static_cast<index_type>(pRows[j])-1] =
        ((pVals[kIndex] < C_MIN || pVals[kIndex] > C_MAX) ?
         static_cast<CType>(NA_C) : static_cast<CType>(pVals[kIndex]));
    }
  }
}

// Function contributed by Peter Haverty at Genentech.
template<typename CType, typename RType, typename BMAccessorType, typename RcppType>
SEXP
GetIndivMatrixElements(
  BigMatrix *pMat, double NA_C, double NA_R,
  NumericVector col, NumericVector row)
{
  BMAccessorType mat(*pMat);
  index_type numCols = col.size();
  RcppType retVec(numCols);
  index_type i;
  for (i=0; i < numCols; ++i)
  {
    CType element = mat[static_cast<index_type>(col[i])-1][static_cast<index_type>(row[i])-1];

    retVec[i] = element == static_cast<CType>(NA_C) ? static_cast<RType>(NA_R) : element;
  }
  return(retVec);
}

// Function contributed by Charles Detemran Jr.
template<typename CType, typename RType, typename BMAccessorType, typename RcppType>
SEXP
  GetIndivVectorMatrixElements(
    BigMatrix *pMat, double NA_C, double NA_R,
    NumericVector elems)
  {
    BMAccessorType mat(*pMat);
    index_type numElems = elems.size();
    RcppType retVec(numElems);
    index_type i = 0;
    int idx = 0;

    for (index_type j = 0; j < elems.size(); j++){
      CType element = mat[i][static_cast<index_type>(elems[j])-1];
      retVec[idx] = element == static_cast<CType>(NA_C) ? static_cast<RType>(NA_R) : element;
      idx += 1;
    }

    return(retVec);
  }

// Function contributed by Charles Detemran Jr.
template<typename CType, typename RType, typename BMAccessorType, typename RcppType>
void
  SetIndivVectorMatrixElements(
    BigMatrix *pMat, double NA_C, double NA_R,
    NumericVector elems, NumericVector inVec)
  {
    BMAccessorType mat(*pMat);
    index_type i = 0;

    for (index_type j = 0; j < elems.size(); j++){
      mat[i][static_cast<index_type>(elems[j])-1] = inVec[j];
    }
  }



// Function contributed by Peter Haverty at Genentech.
template<typename CType, typename RType, typename BMAccessorType>
void SetIndivMatrixElements( BigMatrix *pMat, SEXP col, SEXP row, SEXP values,
      double NA_C, double C_MIN, double C_MAX, double NA_R)
{
  BMAccessorType mat( *pMat );
  double *pCols = REAL(col);
  index_type numCols = Rf_length(col);
  double *pRows = REAL(row);
  VecPtr<RType> vec_ptr;
  RType *pVals = vec_ptr(values);
  index_type i=0;
  CType *pColumn;
  for (i=0; i < numCols; ++i)
  {
    pColumn = mat[static_cast<index_type>(pCols[i])-1];
    pColumn[static_cast<index_type>(pRows[i])-1] =
      ((pVals[i] < C_MIN || pVals[i] > C_MAX) ?
        static_cast<CType>(NA_C) :
        static_cast<CType>(pVals[i]));
  }
}

template<typename CType, typename RType, typename BMAccessorType>
void SetMatrixAll( BigMatrix *pMat, SEXP values,
  double NA_C, double C_MIN, double C_MAX, double NA_R)
{
  BMAccessorType mat( *pMat );
  index_type numCols = pMat->ncol();
  index_type numRows = pMat->nrow();
  VecPtr<RType> vec_ptr;
  RType *pVals = vec_ptr(values);
  index_type valLength = Rf_length(values);
  index_type i=0;
  index_type j=0;
  index_type k=0;
  CType *pColumn;
  index_type kIndex;
  for (i=0; i < numCols; ++i)
  {
    pColumn = mat[i];
    for (j=0; j < numRows; ++j)
    {
      kIndex = k++%valLength;
      pColumn[j] = ((pVals[kIndex] < C_MIN || pVals[kIndex] > C_MAX) ?
                    static_cast<CType>(NA_C) :
                    static_cast<CType>(pVals[kIndex]));
    }
  }
}

template<typename CType, typename RType, typename BMAccessorType>
void SetMatrixCols( BigMatrix *pMat, SEXP col, SEXP values,
  double NA_C, double C_MIN, double C_MAX, double NA_R)
{
  BMAccessorType mat( *pMat );
  double *pCols = REAL(col);
  index_type numCols = Rf_length(col);
  index_type numRows = pMat->nrow();
  VecPtr<RType> vec_ptr;
  RType *pVals = vec_ptr(values);
  index_type valLength = Rf_length(values);
  index_type i=0;
  index_type j=0;
  index_type k=0;
  CType *pColumn;
  index_type kIndex;
  for (i=0; i < numCols; ++i)
  {
    pColumn = mat[static_cast<index_type>(pCols[i])-1];
    for (j=0; j < numRows; ++j)
    {
      kIndex = k++%valLength;
      pColumn[j] = ((pVals[kIndex] < C_MIN || pVals[kIndex] > C_MAX) ?
                    static_cast<CType>(NA_C) :
                    static_cast<CType>(pVals[kIndex]));
    }
  }
}

template<typename CType, typename RType, typename BMAccessorType>
void SetMatrixRows( BigMatrix *pMat, SEXP row, SEXP values,
  double NA_C, double C_MIN, double C_MAX, double NA_R)
{
  BMAccessorType mat( *pMat );
  index_type numCols = pMat->ncol();
  double *pRows = REAL(row);
  index_type numRows = Rf_length(row);
  VecPtr<RType> vec_ptr;
  RType *pVals = vec_ptr(values);
  index_type valLength = Rf_length(values);
  index_type i=0;
  index_type j=0;
  index_type k=0;
  CType *pColumn;
  index_type kIndex;
  for (i=0; i < numCols; ++i)
  {
    pColumn = mat[i];
    for (j=0; j < numRows; ++j)
    {
      kIndex = k++%valLength;
      pColumn[static_cast<index_type>(pRows[j])-1] =
        ((pVals[kIndex] < C_MIN || pVals[kIndex] > C_MAX) ?
         static_cast<CType>(NA_C) : static_cast<CType>(pVals[kIndex]));
    }
  }
}

template<typename CType, typename BMAccessorType>
void SetAllMatrixElements( BigMatrix *pMat, SEXP value,
  double NA_C, double C_MIN, double C_MAX, double NA_R)
{
  BMAccessorType mat( *pMat );
  double val = REAL(value)[0];
  index_type i=0;
  index_type j=0;
  index_type ncol = pMat->ncol();
  index_type nrow = pMat->nrow();

  //bool outOfRange=false;
  if (val < C_MIN || val > C_MAX || isna(val))
  {
    if (!isna(val))
    {
      //outOfRange=true;
      Rf_warning("The value given is out of range, elements will be set to NA.");
    }
    val = NA_C;
  }
  for (i=0; i < ncol; ++i)
  {
    CType *pColumn = mat[i];
    for (j=0; j < nrow; ++j)
    {
      pColumn[j] = static_cast<CType>(val);
    }
  }
}

template<typename CType, typename RType, typename BMAccessorType>
SEXP GetMatrixElements( BigMatrix *pMat, double NA_C, double NA_R,
  SEXP col, SEXP row, SEXPTYPE sxpType)
{
  VecPtr<RType> vec_ptr;
  BMAccessorType mat(*pMat);
  double *pCols = REAL(col);
  double *pRows = REAL(row);
  index_type numCols = Rf_length(col);
  index_type numRows = Rf_length(row);
/*
  if (TooManyRIndices(numCols*numRows))
  {
    Rf_error("Too many indices (>2^31-1) for extraction.");
    return R_NilValue;
  }
*/
  SEXP ret = Rf_protect(Rf_allocVector(VECSXP, 3));
  int protectCount = 1;
  SET_VECTOR_ELT( ret, 1, R_NilValue );
  SET_VECTOR_ELT( ret, 2, R_NilValue );
  SEXP retMat;
  if (numCols == 1 || numRows == 1) {
    retMat = Rf_protect( Rf_allocVector(sxpType, numRows * numCols) );
  } else {
    retMat = Rf_protect( Rf_allocMatrix(sxpType, numRows, numCols) );
  }
  ++protectCount;
  SET_VECTOR_ELT(ret, 0, retMat);
  //SEXP ret = Rf_protect( new_vec(numCols*numRows) );
  RType *pRet = vec_ptr(retMat);
  CType *pColumn;
  index_type k=0;
  index_type i,j;
  for (i=0; i < numCols; ++i)
  {
    if (isna(pCols[i]))
    {
      for (j=0; j < numRows; ++j)
      {
        pRet[k] = static_cast<RType>(NA_R);
      }
    }
    else
    {
      pColumn = mat[static_cast<index_type>(pCols[i])-1];
      for (j=0; j < numRows; ++j)
      {
        if (isna(pRows[j]))
        {
          pRet[k] = static_cast<RType>(NA_R);
        }
        else
        {
          pRet[k] = (pColumn[static_cast<index_type>(pRows[j])-1] ==
            static_cast<CType>(NA_C)) ?  static_cast<RType>(NA_R) :
            (static_cast<RType>(pColumn[static_cast<index_type>(pRows[j])-1]));
        }
        ++k;
      }
    }
  }
  Names colNames = pMat->column_names();
  if (!colNames.empty())
  {
    ++protectCount;
    SEXP rCNames = Rf_protect(Rf_allocVector(STRSXP, numCols));
    for (i=0; i < numCols; ++i)
    {
      if (!isna(pCols[i]))
        SET_STRING_ELT( rCNames, i,
          Rf_mkChar(colNames[static_cast<index_type>(pCols[i])-1].c_str()) );
    }
    SET_VECTOR_ELT(ret, 2, rCNames);
  }
  Names rowNames = pMat->row_names();
  if (!rowNames.empty())
  {
    ++protectCount;
    SEXP rRNames = Rf_protect(Rf_allocVector(STRSXP, numRows));
    for (i=0; i < numRows; ++i)
    {
      if (!isna(pRows[i]))
      {
        SET_STRING_ELT( rRNames, i,
          Rf_mkChar(rowNames[static_cast<index_type>(pRows[i])-1].c_str()) );
      }
    }
    SET_VECTOR_ELT(ret, 1, rRNames);
  }
  Rf_unprotect(protectCount);
  return ret;
}

int convert_real_to_int(double val) {
  if (NumericVector::is_na(val) || val >= (double) INT_MAX + 1 || val <= INT_MIN) {
    return NA_INTEGER;  
  }
  return (int) val;
}

int convert_real_to_int(double val, bool &warn) {
  if (NumericVector::is_na(val)) {
    return NA_INTEGER;  
  }
  if (val >= (double) INT_MAX + 1 || val <= INT_MIN) {
    warn = true;
    return NA_INTEGER;
  }
  int val_int = (int) val;
  if (val_int != val) {
    warn = true;
  }
  return val_int;
}

// Function by Florian Prive
// [[Rcpp::export]]
SEXP to_int_checked(SEXP x) {
  if (TYPEOF(x) == INTSXP) return x;
  NumericVector nv(x);
  int i, n = nv.size();
  IntegerVector res(n);
  bool warn = false;
  for (i = 0; i < n; i++) {
    res[i] = convert_real_to_int(nv[i], warn);
    if (warn) {
      warning("Value changed when converting to integer type.");
      break;    
    }
  }
  for (; i < n; i++) res[i] = convert_real_to_int(nv[i]);
  return res;
}

// Function contributed by Peter Haverty at Genentech.
// [[Rcpp::export]]
SEXP GetIndivMatrixElements(SEXP bigMatAddr, SEXP col, SEXP row)
{
  BigMatrix *pMat =
    reinterpret_cast<BigMatrix*>(R_ExternalPtrAddr(bigMatAddr));
  if (pMat->separated_columns())
  {
    switch(pMat->matrix_type())
    {
      case 1:
        return GetIndivMatrixElements<char, int, SepMatrixAccessor<char>, IntegerVector >(
          pMat, NA_CHAR, NA_INTEGER, col, row);
      case 2:
        return GetIndivMatrixElements<short,int, SepMatrixAccessor<short>, IntegerVector >(
          pMat, NA_SHORT, NA_INTEGER, col, row);
	    case 3:
		return GetIndivMatrixElements<unsigned char, unsigned char, SepMatrixAccessor<unsigned char>, IntegerVector >(
		  pMat, NA_BYTE, NA_INTEGER, col, row);
	    case 4:
        return GetIndivMatrixElements<int, int, SepMatrixAccessor<int>, IntegerVector >(
          pMat, NA_INTEGER, NA_INTEGER, col, row);
      case 6:
        return GetIndivMatrixElements<float, double, SepMatrixAccessor<float>, NumericVector >(
          pMat, NA_FLOAT, NA_FLOAT, col, row);
      case 8:
        return GetIndivMatrixElements<double,double,SepMatrixAccessor<double>, NumericVector >(
          pMat, NA_REAL, NA_REAL, col, row);
    }
  }
  else
  {
    switch(pMat->matrix_type())
    {
      case 1:
        return GetIndivMatrixElements<char, int, MatrixAccessor<char>, IntegerVector >(
          pMat, NA_CHAR, NA_INTEGER, col, row);
      case 2:
        return GetIndivMatrixElements<short, int, MatrixAccessor<short>, IntegerVector >(
          pMat, NA_SHORT, NA_INTEGER, col, row);
	    case 3:
		return GetIndivMatrixElements<unsigned char, unsigned char, MatrixAccessor<unsigned char>, IntegerVector >(
		  pMat, NA_BYTE, NA_INTEGER, col, row);
	    case 4:
        return GetIndivMatrixElements<int, int, MatrixAccessor<int>, IntegerVector >(
          pMat, NA_INTEGER, NA_INTEGER, col, row);
      case 6:
        return GetIndivMatrixElements<float, double, MatrixAccessor<float>, NumericVector >(
          pMat, NA_FLOAT, NA_FLOAT, col, row);
      case 8:
        return GetIndivMatrixElements<double, double, MatrixAccessor<double>, NumericVector >(
          pMat, NA_REAL, NA_REAL, col, row);
    }
  }
  return R_NilValue;
}

// Function contributed by Charles Determan Jr.
// [[Rcpp::export]]
SEXP GetIndivVectorMatrixElements(SEXP bigMatAddr, NumericVector elems)
{
  BigMatrix *pMat =
    reinterpret_cast<BigMatrix*>(R_ExternalPtrAddr(bigMatAddr));
  if (pMat->separated_columns())
  {
    switch(pMat->matrix_type())
    {
    case 1:
      return GetIndivVectorMatrixElements<char, int, SepMatrixAccessor<char>, IntegerVector >(
          pMat, NA_CHAR, NA_INTEGER, elems);
    case 2:
      return GetIndivVectorMatrixElements<short,int, SepMatrixAccessor<short>, IntegerVector >(
          pMat, NA_SHORT, NA_INTEGER, elems);
    case 3:
    	return GetIndivVectorMatrixElements<unsigned char, unsigned char, SepMatrixAccessor<unsigned char>, IntegerVector >(
    	  pMat, NA_BYTE, NA_INTEGER, elems);
    case 4:
      return GetIndivVectorMatrixElements<int, int, SepMatrixAccessor<int>, IntegerVector >(
          pMat, NA_INTEGER, NA_INTEGER, elems);
    case 6:
      return GetIndivVectorMatrixElements<float, double, SepMatrixAccessor<float>, NumericVector >(
          pMat, NA_FLOAT, NA_FLOAT, elems);
    case 8:
      return GetIndivVectorMatrixElements<double,double,SepMatrixAccessor<double>, NumericVector >(
          pMat, NA_REAL, NA_REAL, elems);
    }
  }
  else
  {
    switch(pMat->matrix_type())
    {
    case 1:
      return GetIndivVectorMatrixElements<char, int, MatrixAccessor<char>, IntegerVector >(
          pMat, NA_CHAR, NA_INTEGER, elems);
    case 2:
      return GetIndivVectorMatrixElements<short, int, MatrixAccessor<short>, IntegerVector >(
          pMat, NA_SHORT, NA_INTEGER, elems);
    case 3:
    	return GetIndivVectorMatrixElements<unsigned char, unsigned char, MatrixAccessor<unsigned char>, IntegerVector >(
    			pMat, NA_BYTE, NA_INTEGER, elems);
    case 4:
      return GetIndivVectorMatrixElements<int, int, MatrixAccessor<int>, IntegerVector >(
          pMat, NA_INTEGER, NA_INTEGER, elems);
    case 6:
      return GetIndivVectorMatrixElements<float, double, MatrixAccessor<float>, NumericVector >(
          pMat, NA_FLOAT, NA_FLOAT, elems);
    case 8:
      return GetIndivVectorMatrixElements<double, double, MatrixAccessor<double>, NumericVector >(
          pMat, NA_REAL, NA_REAL, elems);
    }
  }
  return R_NilValue;
}

template<typename CType, typename RType, typename BMAccessorType>
SEXP GetMatrixRows( BigMatrix *pMat, double NA_C, double NA_R,
  SEXP row, SEXPTYPE sxpType)
{
  VecPtr<RType> vec_ptr;
  BMAccessorType mat(*pMat);
  double *pRows=REAL(row);
  index_type numRows = Rf_length(row);
  index_type numCols = pMat->ncol();
/*
  if (TooManyRIndices(numCols*numRows))
  {
    Rf_error("Too many indices (>2^31-1) for extraction.");
    return R_NilValue;
  }
*/
  SEXP ret = Rf_protect(Rf_allocVector(VECSXP, 3));
  int protectCount = 1;
  SET_VECTOR_ELT( ret, 1, R_NilValue );
  SET_VECTOR_ELT( ret, 2, R_NilValue );
  SEXP retMat;
  if (numCols == 1 || numRows == 1) {
    retMat = Rf_protect( Rf_allocVector(sxpType, numCols*numRows) );
  } else {
    retMat = Rf_protect( Rf_allocMatrix(sxpType, numRows, numCols) );
  }
  ++protectCount;
  SET_VECTOR_ELT(ret, 0, retMat);
  RType *pRet = vec_ptr(retMat);
  CType *pColumn = NULL;
  index_type k=0;
  index_type i,j;
  for (i=0; i < numCols; ++i)
  {
    pColumn = mat[i];
    for (j=0; j < numRows; ++j)
    {
      if (isna(pRows[j]))
      {
        pRet[k] = static_cast<RType>(NA_R);
      }
      else
      {
        pRet[k] = (pColumn[static_cast<index_type>(pRows[j])-1] ==
          static_cast<CType>(NA_C)) ?  static_cast<RType>(NA_R) :
          (static_cast<RType>(pColumn[static_cast<index_type>(pRows[j])-1]));
      }
      ++k;
    }
  }
  Names colNames = pMat->column_names();
  if (!colNames.empty())
  {
    ++protectCount;
    SEXP rCNames = Rf_protect(Rf_allocVector(STRSXP, numCols));
    for (i=0; i < numCols; ++i)
    {
      SET_STRING_ELT( rCNames, i, Rf_mkChar(colNames[i].c_str()) );
    }
    SET_VECTOR_ELT(ret, 2, rCNames);
  }
  Names rowNames = pMat->row_names();
  if (!rowNames.empty())
  {
    ++protectCount;
    SEXP rRNames = Rf_protect(Rf_allocVector(STRSXP, numRows));
    for (i=0; i < numRows; ++i)
    {
      if (!isna(pRows[i]))
      {
        SET_STRING_ELT( rRNames, i,
          Rf_mkChar(rowNames[static_cast<index_type>(pRows[i])-1].c_str()) );
      }
    }
    SET_VECTOR_ELT(ret, 1, rRNames);
  }
  Rf_unprotect(protectCount);
  return ret;
}

template<typename CType, typename RType, typename BMAccessorType>
SEXP GetMatrixCols( BigMatrix *pMat, double NA_C, double NA_R,
  SEXP col, SEXPTYPE sxpType)
{
  VecPtr<RType> vec_ptr;
  BMAccessorType mat(*pMat);
  double *pCols=REAL(col);
  index_type numCols = Rf_length(col);
  index_type numRows = pMat->nrow();
/*
  if (TooManyRIndices(numCols*numRows))
  {
    Rf_error("Too many indices (>2^31-1) for extraction.");
    return R_NilValue;
  }
*/
  SEXP ret = Rf_protect(Rf_allocVector(VECSXP, 3));
  int protectCount = 1;
  SET_VECTOR_ELT( ret, 1, R_NilValue );
  SET_VECTOR_ELT( ret, 2, R_NilValue );
  SEXP retMat;
  if (numCols == 1 || numRows == 1) {
    retMat = Rf_protect( Rf_allocVector(sxpType, numRows*numCols) );
  } else {
    retMat = Rf_protect( Rf_allocMatrix(sxpType, numRows, numCols) );
  }
  ++protectCount;
  SET_VECTOR_ELT(ret, 0, retMat);
  //SEXP ret = Rf_protect( new_vec(numCols*numRows) );
  RType *pRet = vec_ptr(retMat);
  CType *pColumn = NULL;
  index_type k=0;
  index_type i,j;
  for (i=0; i < numCols; ++i)
  {
    if (isna(pCols[i]))
    {
      for (j=0; j < numRows; ++j)
      {
        pRet[k] = static_cast<RType>(NA_R);
      }
    }
    else
    {
      pColumn = mat[static_cast<index_type>(pCols[i])-1];
      for (j=0; j < numRows; ++j)
      {
        pRet[k] = (pColumn[j] == static_cast<CType>(NA_C)) ?  static_cast<RType>(NA_R) :
                   (static_cast<RType>(pColumn[j]));
        ++k;
      }
    }
  }
  Names colNames = pMat->column_names();
  if (!colNames.empty())
  {
    ++protectCount;
    SEXP rCNames = Rf_protect(Rf_allocVector(STRSXP, numCols));
    for (i=0; i < numCols; ++i)
    {
      if (!isna(pCols[i]))
        SET_STRING_ELT( rCNames, i,
          Rf_mkChar(colNames[static_cast<index_type>(pCols[i])-1].c_str()) );
    }
    SET_VECTOR_ELT(ret, 2, rCNames);
  }
  Names rowNames = pMat->row_names();
  if (!rowNames.empty())
  {
    ++protectCount;
    SEXP rRNames = Rf_protect(Rf_allocVector(STRSXP, numRows));
    for (i=0; i < numRows; ++i)
    {
      SET_STRING_ELT( rRNames, i, Rf_mkChar(rowNames[i].c_str()) );
    }
    SET_VECTOR_ELT(ret, 1, rRNames);
  }
  Rf_unprotect(protectCount);
  return ret;
}

template<typename CType, typename RType, typename BMAccessorType>
SEXP GetMatrixAll( BigMatrix *pMat, double NA_C, double NA_R,
  SEXPTYPE sxpType)
{
  VecPtr<RType> vec_ptr;
  BMAccessorType mat(*pMat);
  index_type numCols = pMat->ncol();
  index_type numRows = pMat->nrow();
/*
  if (TooManyRIndices(numCols*numRows))
  {
    Rf_error("Too many indices (>2^31-1) for extraction.");
    return R_NilValue;
  }
*/
  SEXP ret = Rf_protect(Rf_allocVector(VECSXP, 3));
  int protectCount = 1;
  SET_VECTOR_ELT( ret, 1, R_NilValue );
  SET_VECTOR_ELT( ret, 2, R_NilValue );
  SEXP retMat;
  if (numCols == 1 || numRows == 1) {
    retMat = Rf_protect( Rf_allocVector(sxpType, numRows * numCols) );
  } else {
    retMat = Rf_protect( Rf_allocMatrix(sxpType, numRows, numCols) );
  }
  ++protectCount;
  SET_VECTOR_ELT(ret, 0, retMat);
  //SEXP ret = Rf_protect( new_vec(numCols*numRows) );
  RType *pRet = vec_ptr(retMat);
  CType *pColumn = NULL;
  index_type k=0;
  index_type i,j;
  for (i=0; i < numCols; ++i)
  {
    pColumn = mat[i];
    for (j=0; j < numRows; ++j)
    {
      pRet[k] = (pColumn[j] == static_cast<CType>(NA_C)) ?  static_cast<RType>(NA_R) :
                 (static_cast<RType>(pColumn[j]));
      ++k;
    }
  }
  Names colNames = pMat->column_names();
  if (!colNames.empty())
  {
    ++protectCount;
    SEXP rCNames = Rf_protect(Rf_allocVector(STRSXP, numCols));
    for (i=0; i < numCols; ++i)
    {
      SET_STRING_ELT( rCNames, i, Rf_mkChar(colNames[i].c_str()) );
    }
    SET_VECTOR_ELT(ret, 2, rCNames);
  }
  Names rowNames = pMat->row_names();
  if (!rowNames.empty())
  {
    ++protectCount;
    SEXP rRNames = Rf_protect(Rf_allocVector(STRSXP, numRows));
    for (i=0; i < numRows; ++i)
    {
      SET_STRING_ELT( rRNames, i, Rf_mkChar(rowNames[i].c_str()) );
    }
    SET_VECTOR_ELT(ret, 1, rRNames);
  }
  Rf_unprotect(protectCount);
  return ret;
}

template<typename T, typename BMAccessorType>
SEXP ReadMatrix(SEXP fileName, BigMatrix *pMat,
                SEXP firstLine, SEXP numLines, SEXP numCols, SEXP separator,
                SEXP hasRowNames, SEXP useRowNames, double C_NA, double posInf,
                double negInf, double notANumber)
{
  BMAccessorType mat(*pMat);
  SEXP ret = Rf_protect(Rf_allocVector(LGLSXP, 1));
  LOGICAL(ret)[0] = (Rboolean)0;
  index_type fl = static_cast<index_type>(REAL(firstLine)[0]);
  index_type nl = static_cast<index_type>(REAL(numLines)[0]);
  string sep(CHAR(STRING_ELT(separator,0)));
  index_type i=0,j;
  bool rowSizeReserved = false;
  //double val;

  ifstream file;
  string lc, element;
  file.open(CHAR(Rf_asChar(fileName)));
  if (!file.is_open())
  {
    Rf_unprotect(1);
    return ret;
  }
  for (i=0; i < fl; ++i)
  {
    std::getline(file, lc);
  }
  Names rn;
  index_type offset = static_cast<index_type>(LOGICAL(hasRowNames)[0]);
  double d;
  int charRead;
  char *pEnd;
  for (i=0; i < nl; ++i)
  {
    // getline may be slow
    std::getline(file, lc);
    string::size_type first=0, last=0;
    j=0;
    while (first < lc.size() && last < lc.size())
    {
      last = lc.find_first_of(sep, first);
      element = lc.substr(first, last-first);
      if (LOGICAL(hasRowNames)[0] && 0==j)
      {
        if (LOGICAL(useRowNames)[0])
        {
          if (!rowSizeReserved)
          {
            rn.reserve(nl);
            rowSizeReserved = true;
          }
          std::size_t pos;
          while ( (pos = element.find("\"", 0)) != string::npos )
          {
            element = element.replace(pos, 1, "");
          }
          while ( (pos = element.find("'", 0)) != string::npos )
          {
            element = element.replace(pos, 1, "");
          }
          rn.push_back(element);
        }
      }
      else
      {
        if (j-offset < pMat->ncol()+1)
        {
          d = strtod(element.c_str(), &pEnd);
          if (pEnd != element.c_str())
          {
            if (isna(d))
            {
              mat[j-offset][i] = static_cast<T>(C_NA);
            }
            else if (std::isinf(d) && d > 0)
            {
              mat[j-offset][i] = static_cast<T>(posInf);
            }
            else if (std::isinf(d) && d < 0)
            {
              mat[j-offset][i] = static_cast<T>(negInf);
            }
            else
            {
              mat[j-offset][i] = static_cast<T>(d);
            }
          }
          else
          {
            charRead = sscanf(element.c_str(), "%lf", &d);
            if (charRead == static_cast<int>(element.size()))
            {
              mat[j-offset][i] = static_cast<T>(d);
            }
            else if (element == "NA")
            {
              mat[j-offset][i] = static_cast<T>(C_NA);
            }
            else if (element == "inf" || element == "Inf")
            {
              mat[j-offset][i] = static_cast<T>(posInf);
            }
            else if (element == "-inf" || element == "-Inf")
            {
              mat[j-offset][i] = static_cast<T>(negInf);
            }
            else if (element == "NaN")
            {
              mat[j-offset][i] = static_cast<T>(notANumber);
            }
            else if (element =="")
            {
              mat[j-offset][i] = static_cast<T>(C_NA);
            }
            else
            {
              mat[j-offset][i] = static_cast<T>(C_NA);
            }
          }
        }
        else
        {
          string ws = string("Incorrect number of entries in row ") + ttos(j);
          Rf_warning("%s", ws.c_str());
        }
      }
      first = last+1;
      ++j;
    }
    if (j-offset < pMat->ncol())
    {
//      warning( (string("Incorrect number of entries in row ")+ttos(j)).c_str());
      while (j-offset < pMat->ncol())
      {
        mat[j++ - offset][i] = static_cast<T>(C_NA);
      }
    }
  }
  pMat->row_names( rn );
  file.close();
  LOGICAL(ret)[0] = (Rboolean)1;
  Rf_unprotect(1);
  return ret;
}

template<typename T, typename BMAccessorType>
void WriteMatrix( BigMatrix *pMat, SEXP fileName, SEXP rowNames,
                  SEXP colNames, SEXP sep, double C_NA )
{
  BMAccessorType mat(*pMat);
  FILE *FP = fopen(CHAR(Rf_asChar(fileName)), "w");
  index_type i,j;
  string  s;
  string sepString = string(CHAR(STRING_ELT(sep, 0)));

  Names cn = pMat->column_names();
  Names rn = pMat->row_names();
  if (LOGICAL(colNames)[0] == Rboolean(TRUE) && !cn.empty())
  {
    for (i=0; i < (int) cn.size(); ++i)
      s += "\"" + cn[i] + "\"" + (((int)cn.size()-1 == i) ? "\n" : sepString);
  }
  fprintf(FP, "%s", s.c_str());
  s.clear();
  for (i=0; i < pMat->nrow(); ++i)
  {
    if ( LOGICAL(rowNames)[0] == Rboolean(TRUE) && !rn.empty())
    {
      s += "\"" + rn[i] + "\"" + sepString;
    }
    for (j=0; j < pMat->ncol(); ++j)
    {
      if ( isna(mat[j][i]) )
      {
        s += "NA";
      }
      else
      {
        s += ttos(mat[j][i]);
      }
      if (j < pMat->ncol()-1)
      {
        s += sepString;
      }
      else
      {
        s += "\n";
      }
    }
    fprintf(FP, "%s", s.c_str());
    s.clear();
  }
  fclose(FP);
}

template<typename T>
struct NAMaker;

template<>
struct NAMaker<unsigned char>
{unsigned char operator()() const {return NA_BYTE;}};

template<>
struct NAMaker<char>
{char operator()() const {return NA_CHAR;}};

template<>
struct NAMaker<short>
{short operator()() const {return NA_SHORT;}};

template<>
struct NAMaker<int>
{int operator()() const {return NA_INTEGER;}};

template<>
struct NAMaker<double>
{double operator()() const {return NA_REAL;}};
// Note: naLast should be passed as an integer.

template<typename PairType>
struct SecondLess 
{
  SecondLess( const bool naLast ) : _naLast(naLast) {}

  bool operator()(const PairType &lhs, const PairType &rhs) const
  {
    if (_naLast)
    {
      if (isna(lhs.second) || isna(rhs.second)) return false;
      return lhs.second < rhs.second;
    }
    else
    {
      if (isna(lhs.second)) return true;
      if (isna(rhs.second)) return false;
      return lhs.second < rhs.second;
    }
  }

  bool _naLast;

};

template<typename PairType>
struct SecondGreater 
{
  SecondGreater(const bool naLast ) : _naLast(naLast) {}

  bool operator()(const PairType &lhs, const PairType &rhs) const
  {
    if (_naLast)
    {
      if (isna(lhs.second) || isna(rhs.second)) return false;
      return lhs.second > rhs.second;
    }
    else
    {
      if (isna(lhs.second)) return true;
      if (isna(rhs.second)) return false;
      return lhs.second > rhs.second;
    }
  }

  bool _naLast;

};

template<typename PairType>
struct SecondIsNA 
{
  bool operator()( const PairType &val ) const
  {
    return isna(val.second);
  }
};

template<typename MatrixAccessorType>
void reorder_matrix( MatrixAccessorType m, SEXP orderVec,
  index_type numColumns, FileBackedBigMatrix *pfbm )
{
  double *pov = REAL(orderVec);
  typedef typename MatrixAccessorType::value_type ValueType;
  typedef std::vector<ValueType> Values;
  Values vs(m.nrow());
  index_type i,j;
  for (i=0; i < numColumns; ++i)
  {
    for (j=0; j < m.nrow(); ++j)
    {
      vs[j] = m[i][static_cast<index_type>(pov[j])-1];
    }
    std::copy( vs.begin(), vs.end(), m[i] );
    if (pfbm) pfbm->flush();
  }
}

// Function to reorder columns
// It likely could use improvement as it just goes element by element
// Added 9-17-2015 by Charles Determan
template<typename MatrixAccessorType>
void reorder_matrix2( MatrixAccessorType m, Rcpp::IntegerVector pov,
  index_type numRows, FileBackedBigMatrix *pfbm )
{
  // double *pov = REAL(orderVec);
  typedef typename MatrixAccessorType::value_type ValueType;
  typedef std::vector<ValueType> Values;
  Values vs(m.ncol());
  index_type i,j;

  for (j=0; j < numRows; ++j)
  {
    for (i=0; i < m.ncol(); ++i)
    {
      vs[i] = m[static_cast<index_type>(pov[i])-1][j];
    }
    for(i = 0; i < m.ncol(); ++i)
    {
      m[i][j] = vs[i];
    }
    if (pfbm) pfbm->flush();
  }
}

template<typename RType, typename MatrixAccessorType>
SEXP get_order( MatrixAccessorType m, SEXP columns, SEXP naLast,
  SEXP decreasing )
{
  typedef typename MatrixAccessorType::value_type ValueType;
  typedef typename std::pair<double, ValueType> PairType;
  typedef std::vector<PairType> OrderVecs;
  std::size_t i;
  index_type k;
  index_type col;
  OrderVecs ov;
  ov.reserve(m.nrow());
  typename OrderVecs::iterator it;
  ValueType val;
  for (k=Rf_length(columns)-1; k >= 0; --k)
  {
    col = static_cast<index_type>(REAL(columns)[k]-1);
    if (k==Rf_length(columns)-1)
    {
      if (isna(Rf_asInteger(naLast)))
      {
        for (i=0; i < static_cast<size_t>(m.nrow()); ++i)
        {
          val = m[col][i];
          if (!isna(val))
          {
            ov.push_back( std::make_pair( static_cast<double>(i), val) );
          }
        }
      }
      else
      {
        ov.resize(m.nrow());
        for (i=0; i < static_cast<size_t>(m.nrow()); ++i)
        {
          val = m[col][i];
          ov[i].first = i;
          ov[i].second = val;
        }
      }
    }
    else // not the first column we've looked at
    {
      if (isna(Rf_asInteger(naLast)))
      {
        i=0;
        while (i < ov.size())
        {
          val = m[col][static_cast<index_type>(ov[i].first)];
          if (!isna(val))
          {
            ov[i++].second = val;
          }
          else
          {
            ov.erase(ov.begin()+i);
          }
        }
      }
      else
      {
        for (i=0; i < static_cast<size_t>(m.nrow()); ++i)
        {
          ov[i].second = m[col][static_cast<index_type>(ov[i].first)];
        }
      }
    }
    if (LOGICAL(decreasing)[0] == 0)
    {
      std::stable_sort(ov.begin(), ov.end(),
        SecondLess<PairType>(Rf_asInteger(naLast)) );
    }
    else
    {
      std::stable_sort(ov.begin(), ov.end(),
        SecondGreater<PairType>(Rf_asInteger(naLast)));
    }
  }

  SEXP ret = Rf_protect(Rf_allocVector(REALSXP,ov.size()));
  double *pret = REAL(ret);
  for (i=0, it=ov.begin(); it < ov.end(); ++it, ++i)
  {
    pret[i] = it->first+1;
  }
  Rf_unprotect(1);
  return ret;
}

template<typename RType, typename MatrixAccessorType>
SEXP get_order2( MatrixAccessorType m, SEXP rows, SEXP naLast,
  SEXP decreasing )
{
  typedef typename MatrixAccessorType::value_type ValueType;
  typedef typename std::pair<double, ValueType> PairType;
  typedef std::vector<PairType> OrderVecs;
  std::size_t i;
  index_type k;
  index_type row;
  OrderVecs ov;
  ov.reserve(m.ncol());
  typename OrderVecs::iterator it;
  ValueType val;
  for (k=Rf_length(rows)-1; k >= 0; --k)
  {
    row = static_cast<index_type>(REAL(rows)[k]-1);
    if (k==Rf_length(rows)-1)
    {
      if (isna(Rf_asInteger(naLast)))
      {
        for (i=0; i < static_cast<size_t>(m.ncol()); ++i)
        {
          val = m[row][i];
          if (!isna(val))
          {
            ov.push_back( std::make_pair( static_cast<double>(i), val) );
          }
        }
      }
      else
      {
        ov.resize(m.ncol());
        for (i=0; i < static_cast<size_t>(m.ncol()); ++i)
        {
          val = m[i][row];
          ov[i].first = i;
          ov[i].second = val;
        }
      }
    }
    else // not the first column we've looked at
    {
      if (isna(Rf_asInteger(naLast)))
      {
        i=0;
        while (i < ov.size())
        {
          val = m[static_cast<index_type>(ov[i].first)][row];
          if (!isna(val))
          {
            ov[i++].second = val;
          }
          else
          {
            ov.erase(ov.begin()+i);
          }
        }
      }
      else
      {
        for (i=0; i < static_cast<size_t>(m.ncol()); ++i)
        {
          ov[i].second = m[static_cast<index_type>(ov[i].first)][row];
        }
      }
    }
    if (LOGICAL(decreasing)[0] == 0)
    {
      std::stable_sort(ov.begin(), ov.end(),
        SecondLess<PairType>(Rf_asInteger(naLast)) );
    }
    else
    {
      std::stable_sort(ov.begin(), ov.end(),
        SecondGreater<PairType>(Rf_asInteger(naLast)));
    }
  }

  SEXP ret = Rf_protect(Rf_allocVector(REALSXP,ov.size()));
  double *pret = REAL(ret);
  for (i=0, it=ov.begin(); it < ov.end(); ++it, ++i)
  {
    pret[i] = it->first+1;
  }
  Rf_unprotect(1);
  return ret;
}


// Rcpp Functions

// [[Rcpp::export]]
void ReorderRIntMatrix( SEXP matrixVector, SEXP nrow, SEXP ncol, SEXP orderVec )
{
  return reorder_matrix(
    MatrixAccessor<int>(INTEGER(matrixVector),
      static_cast<index_type>(Rf_asInteger(nrow))), orderVec,
      static_cast<index_type>(Rf_asInteger(ncol)), NULL );
}

// [[Rcpp::export]]
void ReorderRNumericMatrix( SEXP matrixVector, SEXP nrow, SEXP ncol,
  SEXP orderVec )
{
  return reorder_matrix(
    MatrixAccessor<double>(REAL(matrixVector),
      static_cast<index_type>(Rf_asInteger(nrow))), orderVec,
      static_cast<index_type>(Rf_asInteger(ncol)), NULL );
}

// [[Rcpp::export]]
void ReorderBigMatrix( SEXP address, SEXP orderVec )
{
  BigMatrix *pMat = reinterpret_cast<BigMatrix*>(R_ExternalPtrAddr(address));
  if (pMat->separated_columns())
  {
    switch (pMat->matrix_type())
    {
      case 1:
        return reorder_matrix( SepMatrixAccessor<char>(*pMat), orderVec,
          pMat->ncol(), dynamic_cast<FileBackedBigMatrix*>(pMat) );
      case 2:
        return reorder_matrix( SepMatrixAccessor<short>(*pMat), orderVec,
          pMat->ncol(), dynamic_cast<FileBackedBigMatrix*>(pMat) );
	    case 3:
		return reorder_matrix( SepMatrixAccessor<unsigned char>(*pMat), orderVec,
		  pMat->ncol(), dynamic_cast<FileBackedBigMatrix*>(pMat) );
	    case 4:
        return reorder_matrix( SepMatrixAccessor<int>(*pMat),orderVec,
          pMat->ncol(), dynamic_cast<FileBackedBigMatrix*>(pMat) );
      case 6:
        return reorder_matrix( SepMatrixAccessor<float>(*pMat),orderVec,
          pMat->ncol(), dynamic_cast<FileBackedBigMatrix*>(pMat) );
      case 8:
        return reorder_matrix( SepMatrixAccessor<double>(*pMat),orderVec,
          pMat->ncol(), dynamic_cast<FileBackedBigMatrix*>(pMat) );
    }
  }
  else
  {
    switch (pMat->matrix_type())
    {
      case 1:
        return reorder_matrix( MatrixAccessor<char>(*pMat),orderVec,
          pMat->ncol(), dynamic_cast<FileBackedBigMatrix*>(pMat) );
      case 2:
        return reorder_matrix( MatrixAccessor<short>(*pMat),orderVec,
          pMat->ncol(), dynamic_cast<FileBackedBigMatrix*>(pMat) );
	    case 3:
		return reorder_matrix( MatrixAccessor<unsigned char>(*pMat),orderVec,
		  pMat->ncol(), dynamic_cast<FileBackedBigMatrix*>(pMat) );
	    case 4:
        return reorder_matrix( MatrixAccessor<int>(*pMat),orderVec,
          pMat->ncol(), dynamic_cast<FileBackedBigMatrix*>(pMat) );
      case 6:
        return reorder_matrix( MatrixAccessor<float>(*pMat),orderVec,
          pMat->ncol(), dynamic_cast<FileBackedBigMatrix*>(pMat) );
      case 8:
        return reorder_matrix( MatrixAccessor<double>(*pMat),orderVec,
          pMat->ncol(), dynamic_cast<FileBackedBigMatrix*>(pMat) );
    }
  }
}

// [[Rcpp::export]]
void ReorderRIntMatrixCols(
    Rcpp::IntegerMatrix matrixVector,
    SEXP nrow,
    SEXP ncol,
    Rcpp::IntegerVector orderVec )
{
  reorder_matrix2(
    MatrixAccessor<int>(INTEGER(matrixVector),
      static_cast<index_type>(Rf_asInteger(nrow)),
      static_cast<index_type>(Rf_asInteger(ncol))), orderVec,
      static_cast<index_type>(Rf_asInteger(nrow)), NULL );

  Rcpp::CharacterVector cols = colnames(matrixVector);
  colnames(matrixVector) = cols[orderVec - 1];

  return;
}

// [[Rcpp::export]]
void ReorderRNumericMatrixCols( Rcpp::NumericMatrix matrixVector, SEXP nrow, SEXP ncol,
  Rcpp::IntegerVector orderVec )
{
  reorder_matrix2(
    MatrixAccessor<double>(REAL(matrixVector),
      static_cast<index_type>(Rf_asInteger(nrow)),
      static_cast<index_type>(Rf_asInteger(ncol))), orderVec,
      static_cast<index_type>(Rf_asInteger(nrow)), NULL );


  Rcpp::CharacterVector cols = colnames(matrixVector);
  colnames(matrixVector) = cols[orderVec - 1];

  return;
}

// [[Rcpp::export]]
void ReorderRRawMatrixCols( Rcpp::RawMatrix matrixVector, SEXP nrow, SEXP ncol,
  Rcpp::IntegerVector orderVec )
{
  reorder_matrix2(
    MatrixAccessor<double>(REAL(matrixVector),
      static_cast<index_type>(Rf_asInteger(nrow)),
      static_cast<index_type>(Rf_asInteger(ncol))), orderVec,
      static_cast<index_type>(Rf_asInteger(nrow)), NULL );


  Rcpp::CharacterVector cols = colnames(matrixVector);
  colnames(matrixVector) = cols[orderVec - 1];

  return;
}

// [[Rcpp::export]]
void ReorderBigMatrixCols( SEXP address, SEXP orderVec )
{
  BigMatrix *pMat = reinterpret_cast<BigMatrix*>(R_ExternalPtrAddr(address));
  if (pMat->separated_columns())
  {
    switch (pMat->matrix_type())
    {
      case 1:
        return reorder_matrix2( SepMatrixAccessor<char>(*pMat), orderVec,
          pMat->nrow(), dynamic_cast<FileBackedBigMatrix*>(pMat) );
      case 2:
        return reorder_matrix2( SepMatrixAccessor<short>(*pMat), orderVec,
          pMat->nrow(), dynamic_cast<FileBackedBigMatrix*>(pMat) );
	  case 3:
		return reorder_matrix2( SepMatrixAccessor<unsigned char>(*pMat), orderVec,
		  pMat->nrow(), dynamic_cast<FileBackedBigMatrix*>(pMat) );
	  case 4:
        return reorder_matrix2( SepMatrixAccessor<int>(*pMat),orderVec,
          pMat->nrow(), dynamic_cast<FileBackedBigMatrix*>(pMat) );
      case 6:
        return reorder_matrix2( SepMatrixAccessor<float>(*pMat),orderVec,
          pMat->nrow(), dynamic_cast<FileBackedBigMatrix*>(pMat) );
      case 8:
        return reorder_matrix2( SepMatrixAccessor<double>(*pMat),orderVec,
          pMat->nrow(), dynamic_cast<FileBackedBigMatrix*>(pMat) );
    }
  }
  else
  {
    switch (pMat->matrix_type())
    {
      case 1:
        return reorder_matrix2( MatrixAccessor<char>(*pMat),orderVec,
          pMat->nrow(), dynamic_cast<FileBackedBigMatrix*>(pMat) );
      case 2:
        return reorder_matrix2( MatrixAccessor<short>(*pMat),orderVec,
          pMat->nrow(), dynamic_cast<FileBackedBigMatrix*>(pMat) );
	  case 3:
		return reorder_matrix2( MatrixAccessor<unsigned char>(*pMat),orderVec,
		  pMat->nrow(), dynamic_cast<FileBackedBigMatrix*>(pMat) );
	    case 4:
        return reorder_matrix2( MatrixAccessor<int>(*pMat),orderVec,
          pMat->nrow(), dynamic_cast<FileBackedBigMatrix*>(pMat) );
      case 6:
        return reorder_matrix2( MatrixAccessor<float>(*pMat),orderVec,
          pMat->nrow(), dynamic_cast<FileBackedBigMatrix*>(pMat) );
      case 8:
        return reorder_matrix2( MatrixAccessor<double>(*pMat),orderVec,
          pMat->nrow(), dynamic_cast<FileBackedBigMatrix*>(pMat) );
    }
  }
}

// [[Rcpp::export]]
SEXP OrderRIntMatrix( SEXP matrixVector, SEXP nrow, SEXP columns,
  SEXP naLast, SEXP decreasing )
{
  return get_order<int>(
    MatrixAccessor<int>(INTEGER(matrixVector),
      static_cast<index_type>(Rf_asInteger(nrow))),
    columns, naLast, decreasing );
}

// [[Rcpp::export]]
SEXP OrderRNumericMatrix( SEXP matrixVector, SEXP nrow, SEXP columns,
  SEXP naLast, SEXP decreasing )
{
  return get_order<double>(
    MatrixAccessor<double>(REAL(matrixVector),
      static_cast<index_type>(Rf_asInteger(nrow))),
    columns, naLast, decreasing );
}

// [[Rcpp::export]]
SEXP OrderBigMatrix(SEXP address, SEXP columns, SEXP naLast, SEXP decreasing)
{
  BigMatrix *pMat = reinterpret_cast<BigMatrix*>(R_ExternalPtrAddr(address));
  if (pMat->separated_columns())
  {
    switch (pMat->matrix_type())
    {
      case 1:
        return get_order<char>( SepMatrixAccessor<char>(*pMat),
          columns, naLast, decreasing );
      case 2:
        return get_order<short>( SepMatrixAccessor<short>(*pMat),
          columns, naLast, decreasing );
	  case 3:
	    return get_order<unsigned char>( SepMatrixAccessor<unsigned char>(*pMat),
	      columns, naLast, decreasing );
	  case 4:
        return get_order<int>( SepMatrixAccessor<int>(*pMat),
          columns, naLast, decreasing );
      case 6:
        return get_order<float>( SepMatrixAccessor<float>(*pMat),
          columns, naLast, decreasing );
      case 8:
        return get_order<double>( SepMatrixAccessor<double>(*pMat),
          columns, naLast, decreasing );
    }
  }
  else
  {
    switch (pMat->matrix_type())
    {
      case 1:
        return get_order<char>( MatrixAccessor<char>(*pMat),
          columns, naLast, decreasing );
      case 2:
        return get_order<short>( MatrixAccessor<short>(*pMat),
          columns, naLast, decreasing );
	  case 3:
	    return get_order<unsigned char>( MatrixAccessor<unsigned char>(*pMat),
	      columns, naLast, decreasing );
	  case 4:
        return get_order<int>( MatrixAccessor<int>(*pMat),
          columns, naLast, decreasing );
      case 6:
        return get_order<float>( MatrixAccessor<float>(*pMat),
          columns, naLast, decreasing );
      case 8:
        return get_order<double>( MatrixAccessor<double>(*pMat),
          columns, naLast, decreasing );
    }
  }
  return R_NilValue;
}

// [[Rcpp::export]]
SEXP OrderRIntMatrixCols( SEXP matrixVector, SEXP nrow, SEXP ncol,
  SEXP rows, SEXP naLast, SEXP decreasing )
{
  return get_order2<int>(
    MatrixAccessor<int>(INTEGER(matrixVector),
      static_cast<index_type>(Rf_asInteger(nrow)),
      static_cast<index_type>(Rf_asInteger(ncol))),
    rows, naLast, decreasing );
}

// [[Rcpp::export]]
SEXP OrderRNumericMatrixCols( SEXP matrixVector, SEXP nrow, SEXP ncol,
  SEXP rows, SEXP naLast, SEXP decreasing )
{
  return get_order2<double>(
    MatrixAccessor<double>(REAL(matrixVector),
      static_cast<index_type>(Rf_asInteger(nrow)),
      static_cast<index_type>(Rf_asInteger(ncol))),
    rows, naLast, decreasing );
}

// [[Rcpp::export]]
SEXP OrderBigMatrixCols(SEXP address, SEXP rows,
SEXP naLast, SEXP decreasing)
{
  BigMatrix *pMat = reinterpret_cast<BigMatrix*>(R_ExternalPtrAddr(address));
  if (pMat->separated_columns())
  {
    switch (pMat->matrix_type())
    {
      case 1:
        return get_order2<char>( SepMatrixAccessor<char>(*pMat),
          rows, naLast, decreasing );
      case 2:
        return get_order2<short>( SepMatrixAccessor<short>(*pMat),
          rows, naLast, decreasing );
	  case 3:
	    return get_order2<unsigned char>( SepMatrixAccessor<unsigned char>(*pMat),
	      rows, naLast, decreasing );
	  case 4:
        return get_order2<int>( SepMatrixAccessor<int>(*pMat),
          rows, naLast, decreasing );
      case 6:
        return get_order2<float>( SepMatrixAccessor<float>(*pMat),
          rows, naLast, decreasing );
      case 8:
        return get_order2<double>( SepMatrixAccessor<double>(*pMat),
          rows, naLast, decreasing );
    }
  }
  else
  {
    switch (pMat->matrix_type())
    {
      case 1:
        return get_order2<char>( MatrixAccessor<char>(*pMat),
          rows, naLast, decreasing );
      case 2:
        return get_order2<short>( MatrixAccessor<short>(*pMat),
          rows, naLast, decreasing );
	  case 3:
	    return get_order2<unsigned char>( MatrixAccessor<unsigned char>(*pMat),
	      rows, naLast, decreasing );
	  case 4:
        return get_order2<int>( MatrixAccessor<int>(*pMat),
          rows, naLast, decreasing );
      case 6:
        return get_order2<float>( MatrixAccessor<float>(*pMat),
          rows, naLast, decreasing );
      case 8:
        return get_order2<double>( MatrixAccessor<double>(*pMat),
          rows, naLast, decreasing );
    }
  }
  return R_NilValue;
}

// [[Rcpp::export]]
SEXP CCleanIndices(SEXP indices, SEXP rc)
{
  typedef std::vector<index_type> Indices;

  double *pIndices = REAL(indices);
  index_type numIndices = Rf_length(indices);
  double maxrc = REAL(rc)[0];
  int protectCount=1;
  SEXP ret = Rf_protect(Rf_allocVector(VECSXP, 2));
  index_type negIndexCount=0;
  index_type posIndexCount=0;
  index_type zeroIndexCount=0;
  Indices::size_type i,j;
  // See if the indices are within range, negative, positive, zero, or mixed.
  for (i=0; i < static_cast<Indices::size_type>(numIndices); ++i)
  {
    if (static_cast<index_type>(pIndices[i]) == 0)
    {
      ++zeroIndexCount;
    }
    if (static_cast<index_type>(pIndices[i]) < 0)
    {
      ++negIndexCount;
    }
    if (static_cast<index_type>(pIndices[i]) > 0)
    {
      ++posIndexCount;
    }
    if ( labs(static_cast<index_type>(pIndices[i])) > maxrc )
    {
      SET_VECTOR_ELT(ret, 0, R_NilValue);
      SET_VECTOR_ELT(ret, 1, R_NilValue);
      Rf_unprotect(protectCount);
      return ret;
    }
  }

  if ( (zeroIndexCount == numIndices) && (numIndices > 0) )
  {
    protectCount += 2;
    SEXP returnCond = Rf_protect(Rf_allocVector(LGLSXP,1));
    LOGICAL(returnCond)[0] = (Rboolean)1;
    SEXP newIndices = Rf_protect(Rf_allocVector(REALSXP,0));
    SET_VECTOR_ELT(ret, 0, returnCond);
    SET_VECTOR_ELT(ret, 1, newIndices);
    Rf_unprotect(protectCount);
    return ret;
  }

  if (posIndexCount > 0 && negIndexCount > 0)
  {
    SET_VECTOR_ELT(ret, 0, R_NilValue);
    SET_VECTOR_ELT(ret, 1, R_NilValue);
    Rf_unprotect(protectCount);
    return ret;
  }
  if (zeroIndexCount > 0)
  {
    protectCount += 2;
    SEXP returnCond = Rf_protect(Rf_allocVector(LGLSXP,1));
    LOGICAL(returnCond)[0] = (Rboolean)1;
    SEXP newIndices = Rf_protect(Rf_allocVector(REALSXP,posIndexCount));
    double *newPIndices = REAL(newIndices);
    j=0;
    for (i=0; i < static_cast<Indices::size_type>(numIndices); ++i)
    {
      if (static_cast<index_type>(pIndices[i]) != 0)
      {
        newPIndices[j++] = pIndices[i];
      }
    }
    SET_VECTOR_ELT(ret, 0, returnCond);
    SET_VECTOR_ELT(ret, 1, newIndices);
    Rf_unprotect(protectCount);
    return ret;
  }
  else if (negIndexCount > 0)
  {
    // It might be better to use a data-structure other than a vector
    // (sequential ordering).
    Indices ind;
    try
    {
      ind.reserve(static_cast<index_type>(maxrc));
    }
    catch(...)
    {
      SET_VECTOR_ELT(ret, 0, R_NilValue);
      SET_VECTOR_ELT(ret, 1, R_NilValue);
      Rf_unprotect(protectCount);
      return ret;
    }
    for (i=1; i <= static_cast<Indices::size_type>(maxrc); ++i)
    {
      ind.push_back(i);
    }
    Indices::iterator it;
    for (i=0; i < static_cast<Indices::size_type>(numIndices); ++i)
    {
      it = std::lower_bound(ind.begin(), ind.end(),
        static_cast<index_type>(-1*pIndices[i]));
      if ( it != ind.end() && *it == -1*static_cast<index_type>(pIndices[i]) )
      {
        ind.erase(it);
      }
    }
/*
    if (TooManyRIndices(ind.size()))
    {
      SET_VECTOR_ELT(ret, 0, R_NilValue);
      SET_VECTOR_ELT(ret, 1, R_NilValue);
      Rf_unprotect(protectCount);
      return ret;
    }
*/
    protectCount +=2;
    SEXP returnCond = Rf_protect(Rf_allocVector(LGLSXP,1));
    LOGICAL(returnCond)[0] = (Rboolean)1;
    SEXP newIndices = Rf_protect(Rf_allocVector(REALSXP,ind.size()));
    double *newPIndices = REAL(newIndices);
    for (i=0; i < ind.size(); ++i)
    {
      newPIndices[i] = static_cast<double>(ind[i]);
    }
    SET_VECTOR_ELT(ret, 0, returnCond);
    SET_VECTOR_ELT(ret, 1, newIndices);
    Rf_unprotect(protectCount);
    return ret;
  }
  protectCount += 1;
  SEXP returnCond = Rf_protect(Rf_allocVector(LGLSXP,1));
  LOGICAL(returnCond)[0] = (Rboolean)0;
  SET_VECTOR_ELT(ret, 0, returnCond);
  SET_VECTOR_ELT(ret, 1, R_NilValue);
  Rf_unprotect(protectCount);
  return ret;
}

// [[Rcpp::export]]
SEXP HasRowColNames(SEXP address)
{
  BigMatrix *pMat = (BigMatrix*)R_ExternalPtrAddr(address);
  SEXP ret = Rf_protect(Rf_allocVector(LGLSXP,2));
  LOGICAL(ret)[0] =
    pMat->row_names().empty() ? Rboolean(0) : Rboolean(1);
  LOGICAL(ret)[1] =
    pMat->column_names().empty() ? Rboolean(0) : Rboolean(1);
  Rf_unprotect(1);
  return ret;
}


// Not currently used?!?!
// [[Rcpp::export]]
SEXP GetIndexRowNames(SEXP address, SEXP indices_)
{
  BigMatrix *pMat = (BigMatrix*)R_ExternalPtrAddr(address);
  Names rn = pMat->row_names();
  Rcpp::IntegerVector indices = Rcpp::as<Rcpp::IntegerVector>(indices_);
  Rcpp::CharacterVector rcpp_rn = Rcpp::wrap(rn);
  return rcpp_rn[indices-1];
//  vector<int> c_idx = Rcpp::as<vector<int> >(indices);
//  return StringVec2RChar(rn, c_idx, indices.size());
}

// Not currently used?!?!
// [[Rcpp::export]]
SEXP GetIndexColNames(SEXP address, SEXP indices_)
{
  BigMatrix *pMat = (BigMatrix*)R_ExternalPtrAddr(address);
  Names cn = pMat->column_names();
  Rcpp::IntegerVector indices = Rcpp::as<Rcpp::IntegerVector>(indices_);
  Rcpp::CharacterVector rcpp_cn = Rcpp::wrap(cn);
  return rcpp_cn[indices-1];
//  vector<int> c_idx = Rcpp::as<vector<int> >(indices);
//  return StringVec2RChar(cn, c_idx, indices.size());
}

// [[Rcpp::export]]
SEXP GetColumnNamesBM(SEXP address)
{
  BigMatrix *pMat = (BigMatrix*)R_ExternalPtrAddr(address);
  Names cn = pMat->column_names();
  return Rcpp::wrap(cn);
}

// [[Rcpp::export]]
SEXP GetRowNamesBM(SEXP address)
{
  BigMatrix *pMat = (BigMatrix*)R_ExternalPtrAddr(address);
  Names rn = pMat->row_names();
  return Rcpp::wrap(rn);
}

// [[Rcpp::export]]
void SetColumnNames(SEXP address, SEXP columnNames)
{
  BigMatrix *pMat = (BigMatrix*) R_ExternalPtrAddr(address);
  Names cn;
  index_type i;
  for (i=0; i < Rf_length(columnNames); ++i)
    cn.push_back(string(CHAR(STRING_ELT(columnNames, i))));
  pMat->column_names(cn);
}

// [[Rcpp::export]]
void SetRowNames(SEXP address, SEXP rowNames)
{
  BigMatrix *pMat = (BigMatrix*) R_ExternalPtrAddr(address);
  Names rn;
  index_type i;
  for (i=0; i < Rf_length(rowNames); ++i)
    rn.push_back(string(CHAR(STRING_ELT(rowNames, i))));
  pMat->row_names(rn);
}

// [[Rcpp::export]]
SEXP IsReadOnly(SEXP bigMatAddr)
{
  BigMatrix *pMat = reinterpret_cast<BigMatrix*>(R_ExternalPtrAddr(bigMatAddr));
  SEXP ret = Rf_protect(Rf_allocVector(LGLSXP,1));
  LOGICAL(ret)[0] = (pMat->read_only() ? (Rboolean) 1 : (Rboolean) 0);
  Rf_unprotect(1);
  return ret;
}

// [[Rcpp::export]]
SEXP CIsSubMatrix(SEXP bigMatAddr)
{
  BigMatrix *pMat = reinterpret_cast<BigMatrix*>(R_ExternalPtrAddr(bigMatAddr));
  SEXP ret = Rf_protect(Rf_allocVector(LGLSXP,1));
  if ( pMat->col_offset() > 0 ||
       pMat->row_offset() > 0 ||
       pMat->nrow() < pMat->total_rows() ||
       pMat->ncol() < pMat->total_columns() )
  {
    LOGICAL(ret)[0] = (Rboolean) 1;
  }
  else
  {
    LOGICAL(ret)[0] = (Rboolean) 0;
  }
  Rf_unprotect(1);
  return ret;
}

// [[Rcpp::export]]
SEXP CGetNrow(SEXP bigMatAddr)
{
  BigMatrix *pMat = (BigMatrix*)R_ExternalPtrAddr(bigMatAddr);
  SEXP ret = Rf_protect(Rf_allocVector(REALSXP,1));
  REAL(ret)[0] = (double)pMat->nrow();
  Rf_unprotect(1);
  return(ret);
}

// [[Rcpp::export]]
SEXP CGetNcol(SEXP bigMatAddr)
{
  BigMatrix *pMat = (BigMatrix*)R_ExternalPtrAddr(bigMatAddr);
  SEXP ret = Rf_protect(Rf_allocVector(REALSXP,1));
  REAL(ret)[0] = (double)pMat->ncol();
  Rf_unprotect(1);
  return(ret);
}

// [[Rcpp::export]]
SEXP CGetType(SEXP bigMatAddr)
{
  Rcpp::XPtr<BigMatrix> pMat(bigMatAddr);
  int ret = pMat->matrix_type();
  return Rcpp::wrap(ret);
}

// not currently used?!?!?!
// [[Rcpp::export]]
SEXP IsSharedMemoryBigMatrix(SEXP bigMatAddr)
{
  BigMatrix *pMat = (BigMatrix*)R_ExternalPtrAddr(bigMatAddr);
  SEXP ret = Rf_protect(Rf_allocVector(LGLSXP,1));
  LOGICAL(ret)[0] =
    dynamic_cast<SharedMemoryBigMatrix*>(pMat) == NULL ?
      static_cast<Rboolean>(0) :
      static_cast<Rboolean>(1);
  Rf_unprotect(1);
  return ret;
}

// [[Rcpp::export]]
SEXP IsFileBackedBigMatrix(SEXP bigMatAddr)
{
  BigMatrix *pMat = (BigMatrix*)R_ExternalPtrAddr(bigMatAddr);
  SEXP ret = Rf_protect(Rf_allocVector(LGLSXP,1));
  LOGICAL(ret)[0] =
    dynamic_cast<FileBackedBigMatrix*>(pMat) == NULL ?
      static_cast<Rboolean>(0) :
      static_cast<Rboolean>(1);
  Rf_unprotect(1);
  return ret;
}

// [[Rcpp::export]]
SEXP IsSeparated(SEXP bigMatAddr)
{
  BigMatrix *pMat = (BigMatrix*)R_ExternalPtrAddr(bigMatAddr);
  SEXP ret = Rf_protect(Rf_allocVector(LGLSXP,1));
  LOGICAL(ret)[0] = pMat->separated_columns() ? (Rboolean)1 : (Rboolean)0;
  Rf_unprotect(1);
  return(ret);
}

// removed extern C because doesn't appear necessary
// Rcpp attributes can be used for R calls and the others
// are only used in the C code

// only called in C code
void CDestroyBigMatrix(SEXP bigMatrixAddr)
{
  BigMatrix *pm=(BigMatrix*)(R_ExternalPtrAddr(bigMatrixAddr));
  delete pm;
  R_ClearExternalPtr(bigMatrixAddr);
}

// only called in C code
inline bool Lcomp(double a, double b, int op) {
  return(op==0 ? a<=b : a<b);
}
inline bool Gcomp(double a, double b, int op) {
  return(op==0 ? a>=b : a>b);
}


template<typename T, typename MatrixType>
SEXP MWhichMatrix( MatrixType mat, index_type nrow, SEXP selectColumn,
  SEXP minVal, SEXP maxVal, SEXP chkMin, SEXP chkMax, SEXP opVal, double C_NA )
{
  index_type numSc = Rf_length(selectColumn);
  double *sc = REAL(selectColumn);
  double *min = REAL(minVal);
  double *max = REAL(maxVal);
  int *chkmin = INTEGER(chkMin);
  int *chkmax = INTEGER(chkMax);

  double minV, maxV;
  int ov = Rf_asInteger(opVal);
  index_type count = 0;
  index_type i,j;
  double val;
  for (i=0; i < nrow; ++i) {
    for (j=0; j < numSc; ++j)  {
      minV = min[j];
      maxV = max[j];
      if (isna(minV)) {
        minV = static_cast<T>(C_NA);
        maxV = static_cast<T>(C_NA);
      }
      val = (double) mat[(index_type)sc[j]-1][i];
      if (chkmin[j]==-1) { // this is an 'neq'
        if (ov==1) {
          // OR with 'neq'
          if  ( (minV!=val) ||
                ( (isna(val) && !isna(minV)) ||
                  (!isna(val) && isna(minV)) ) ) {
            ++count;
            break;
          }
        } else {
          // AND with 'neq'   // if they are equal, then break out.
          if ( (minV==val) || (isna(val) && isna(minV)) ) break;
        }
      } else { // not a 'neq'

        // If it's an OR operation and it's true for one, it's true for the
        // whole row.
        if ( ( (Gcomp(val, minV, chkmin[j]) && Lcomp(val, maxV, chkmax[j])) ||
               (isna(val) && isna(minV))) && ov==1 ) {
          ++count;
          break;
        }
        // If it's an AND operation and it's false for one, it's false for
        // the whole row.
        if ( ( (Lcomp(val, minV, 1-chkmin[j]) || Gcomp(val, maxV, 1-chkmax[j]))
             ||
               (isna(val) && !isna(minV)) || (!isna(val) && isna(minV)) ) &&
             ov == 0 ) break;
      }
    }
    // If it's an AND operation and it's true for each column, it's true
    // for the entire row.
    if (j==numSc && ov == 0) ++count;
  }

  if (count==0) return Rf_allocVector(INTSXP,0);

  SEXP ret = Rf_protect(Rf_allocVector(REALSXP,count));
  double *retVals = REAL(ret);
  index_type k = 0;
  for (i=0; i < nrow; ++i) {
    for (j=0; j < numSc; ++j) {
      minV = min[j];
      maxV = max[j];
      if (isna(minV)) {
        minV = static_cast<T>(C_NA);
        maxV = static_cast<T>(C_NA);
      }
      val = (double) mat[(index_type)sc[j]-1][i];

      if (chkmin[j]==-1) { // this is an 'neq'
        if (ov==1) {
          // OR with 'neq'
          if  ( (minV!=val) ||
                ( (isna(val) && !isna(minV)) ||
                  (!isna(val) && isna(minV)) ) ) {
            retVals[k++] = i+1;
            break;
          }
        } else {
          // AND with 'neq'   // if they are equal, then break out.
          if ( (minV==val) || (isna(val) && isna(minV)) ) break;
        }
      } else { // not a 'neq'

        if ( ( (Gcomp(val, minV, chkmin[j]) && Lcomp(val, maxV, chkmax[j])) ||
               (isna(val) && isna(minV))) && ov==1 ) {
          retVals[k++] = i+1;
          break;
        }
        if (((Lcomp(val, minV, 1-chkmin[j]) || Gcomp(val, maxV, 1-chkmax[j])) ||
               (isna(val) && !isna(minV)) || (!isna(val) && isna(minV)) ) &&
             ov == 0 ) break;

      }
    } // end j loop
    if (j==numSc && ov == 0) retVals[k++] = i+1;
  } // end i loop
  Rf_unprotect(1);
  return(ret);
}

template<typename T>
SEXP CreateRAMMatrix(SEXP row, SEXP col, SEXP colnames, SEXP rownames,
  SEXP typeLength, SEXP ini, SEXP separated)
{
  T *pMat=NULL;
  try
  {
    pMat = new T();

    if (!pMat->create( static_cast<index_type>(REAL(row)[0]),
      static_cast<index_type>(REAL(col)[0]),
      Rf_asInteger(typeLength),
      static_cast<bool>(LOGICAL(separated)[0])))
    {
      delete pMat;
      return R_NilValue;
    }

    if (colnames != R_NilValue)
    {
      pMat->column_names(RChar2StringVec(colnames));
    }
    if (rownames != R_NilValue)
    {
      pMat->row_names(RChar2StringVec(rownames));
    }
    if (Rf_length(ini) != 0)
    {
      if (pMat->separated_columns())
      {
        switch (pMat->matrix_type())
        {
          case 1:
            SetAllMatrixElements<char, SepMatrixAccessor<char> >(
              pMat, ini, NA_CHAR, R_CHAR_MIN, R_CHAR_MAX, NA_REAL);
            break;
          case 2:
            SetAllMatrixElements<short, SepMatrixAccessor<short> >(
              pMat, ini, NA_SHORT, R_SHORT_MIN, R_SHORT_MAX, NA_REAL);
            break;
	      case 3:
	        SetAllMatrixElements<unsigned char, SepMatrixAccessor<unsigned char> >(
	          pMat, ini, NA_BYTE, R_BYTE_MIN, R_BYTE_MAX, NA_REAL);
	        break;
	      case 4:
            SetAllMatrixElements<int, SepMatrixAccessor<int> >(
              pMat, ini, NA_INTEGER, R_INT_MIN, R_INT_MAX, NA_REAL);
            break;
          case 6:
            SetAllMatrixElements<float, SepMatrixAccessor<float> >(
              pMat, ini, NA_FLOAT, R_FLT_MIN, R_FLT_MAX, NA_REAL);
            break;
          case 8:
            SetAllMatrixElements<double, SepMatrixAccessor<double> >(
              pMat, ini, NA_REAL, R_DOUBLE_MIN, R_DOUBLE_MAX, NA_REAL);
        }
      }
      else
      {
        switch (pMat->matrix_type())
        {
          case 1:
            SetAllMatrixElements<char, MatrixAccessor<char> >(
              pMat, ini, NA_CHAR, R_CHAR_MIN, R_CHAR_MAX, NA_REAL );
            break;
          case 2:
            SetAllMatrixElements<short, MatrixAccessor<short> >(
              pMat, ini, NA_SHORT, R_SHORT_MIN, R_SHORT_MAX, NA_REAL );
            break;
	      case 3:
	        SetAllMatrixElements<unsigned char, MatrixAccessor<unsigned char> >(
	          pMat, ini, NA_BYTE, R_BYTE_MIN, R_BYTE_MAX, NA_REAL );
	        break;
	      case 4:
            SetAllMatrixElements<int, MatrixAccessor<int> >(
              pMat, ini, NA_INTEGER, R_INT_MIN, R_INT_MAX, NA_REAL );
            break;
          case 6:
            SetAllMatrixElements<float, MatrixAccessor<float> >(
              pMat, ini, NA_FLOAT, R_FLT_MIN, R_FLT_MAX, NA_REAL );
            break;
          case 8:
            SetAllMatrixElements<double, MatrixAccessor<double> >(
              pMat, ini, NA_REAL, R_DOUBLE_MIN, R_DOUBLE_MAX, NA_REAL);
        }
      }
    }
    SEXP address = R_MakeExternalPtr( dynamic_cast<BigMatrix*>(pMat),
      R_NilValue, R_NilValue);
    R_RegisterCFinalizerEx(address, (R_CFinalizer_t) CDestroyBigMatrix,
      (Rboolean) TRUE);
    return address;
  }
  catch(std::exception &e)
  {
    Rprintf("%s\n", e.what());
  }
  catch(...)
  {
    Rprintf("Exception caught while trying to create shared matrix.");
  }
  delete(pMat);
  Rf_error("The shared matrix could not be created\n");
  return(R_NilValue);
}


// Rcpp functions

// [[Rcpp::export]]
void SetRowOffsetInfo( SEXP bigMatAddr, SEXP rowOffset, SEXP numRows )
{
  BigMatrix *pMat =
    reinterpret_cast<BigMatrix*>(R_ExternalPtrAddr(bigMatAddr));
  pMat->row_offset(static_cast<index_type>(REAL(rowOffset)[0]));
  pMat->nrow(static_cast<index_type>(REAL(numRows)[0]));

}

// [[Rcpp::export]]
void SetColumnOffsetInfo( SEXP bigMatAddr, SEXP colOffset, SEXP numCols )
{
  BigMatrix *pMat =
    reinterpret_cast<BigMatrix*>(R_ExternalPtrAddr(bigMatAddr));
  pMat->col_offset(static_cast<index_type>(REAL(colOffset)[0]));
  pMat->ncol(static_cast<index_type>(REAL(numCols)[0]));
}

// [[Rcpp::export]]
SEXP GetRowOffset( SEXP bigMatAddr )
{
    Rcpp::XPtr<BigMatrix> pMat(bigMatAddr);
    Rcpp::NumericVector ret(2);
    ret[0] = pMat->row_offset();
    ret[1] = pMat->nrow();
    return ret;
}

// [[Rcpp::export]]
Rcpp::NumericVector GetColOffset( SEXP bigMatAddr )
{
    Rcpp::XPtr<BigMatrix> pMat(bigMatAddr);
    Rcpp::NumericVector ret(2);
    ret[0] = pMat->col_offset();
    ret[1] = pMat->ncol();
    return ret;
}

// [[Rcpp::export]]
SEXP GetTotalColumns( SEXP bigMatAddr )
{
    Rcpp::XPtr<BigMatrix> pMat(bigMatAddr);
    int ret = pMat->total_columns();
    return Rcpp::wrap(ret);
}

// [[Rcpp::export]]
SEXP GetTotalRows( SEXP bigMatAddr )
{
    Rcpp::XPtr<BigMatrix> pMat(bigMatAddr);
    int ret = pMat->total_rows();
    return Rcpp::wrap(ret);
}

template <typename T> std::string type_name();

// simplifed to use Rcpp tools 04/13/2015 -- Charles Determan Jr.
// [[Rcpp::export]]
Rcpp::String GetTypeString( SEXP bigMatAddr )
{
    Rcpp::XPtr<BigMatrix> pMat(bigMatAddr);


    switch(pMat->matrix_type())
    {
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


/* Added by Charles Determan Jr. 04/20/2015
 * quick function to access big.matrix sizes
 * possibly convert in to a method for object.size???
 */
//' @title big.matrix size
//' @description Returns the size of the created matrix in bytes
//' @param bigMat a \code{big.matrix} object
//' @export
// [[Rcpp::export]]
SEXP GetMatrixSize( SEXP bigMat )
{
    // declare as S4 object
    Rcpp::S4 As4(bigMat);
    // pull address slot
    SEXP BM_address = As4.slot("address");
    // declare as external pointer
    Rcpp::XPtr<BigMatrix> pMat(BM_address);
    // return the matrix size (in bytes)
    return Rcpp::wrap(pMat->allocation_size());
}


// [[Rcpp::export]]
SEXP MWhichBigMatrix( SEXP bigMatAddr, SEXP selectColumn, SEXP minVal,
                     SEXP maxVal, SEXP chkMin, SEXP chkMax, SEXP opVal )
{
    Rcpp::XPtr<BigMatrix> pMat(bigMatAddr);

    if (pMat->separated_columns())
    {
        switch (pMat->matrix_type())
        {
          case 1:
            return MWhichMatrix<char>( SepMatrixAccessor<char>(*pMat),
              pMat->nrow(), selectColumn, minVal, maxVal, chkMin, chkMax,
              opVal, NA_CHAR);
          case 2:
            return MWhichMatrix<short>( SepMatrixAccessor<short>(*pMat),
              pMat->nrow(), selectColumn, minVal, maxVal, chkMin, chkMax,
              opVal, NA_SHORT);
	       case 3:
	         return MWhichMatrix<unsigned char>( SepMatrixAccessor<unsigned char>(*pMat),
	           pMat->nrow(), selectColumn, minVal, maxVal, chkMin, chkMax,
	           opVal, NA_BYTE);
	       case 4:
            return MWhichMatrix<int>( SepMatrixAccessor<int>(*pMat),
              pMat->nrow(), selectColumn, minVal, maxVal, chkMin, chkMax,
              opVal, NA_INTEGER);
          case 6:
            return MWhichMatrix<float>( SepMatrixAccessor<float>(*pMat),
              pMat->nrow(), selectColumn, minVal, maxVal, chkMin, chkMax,
              opVal, NA_FLOAT);
          case 8:
            return MWhichMatrix<double>( SepMatrixAccessor<double>(*pMat),
              pMat->nrow(), selectColumn, minVal, maxVal, chkMin, chkMax,
              opVal, NA_REAL);
        }
    }
    else
    {
        switch (pMat->matrix_type())
        {
          case 1:
            return MWhichMatrix<char>( MatrixAccessor<char>(*pMat),
              pMat->nrow(), selectColumn, minVal, maxVal, chkMin, chkMax,
              opVal, NA_CHAR);
          case 2:
            return MWhichMatrix<short>( MatrixAccessor<short>(*pMat),
              pMat->nrow(), selectColumn, minVal, maxVal, chkMin, chkMax,
              opVal, NA_SHORT);
	      case 3:
	        return MWhichMatrix<unsigned char>( MatrixAccessor<unsigned char>(*pMat),
              pMat->nrow(), selectColumn, minVal, maxVal, chkMin, chkMax,
              opVal, NA_BYTE);
	      case 4:
            return MWhichMatrix<int>( MatrixAccessor<int>(*pMat),
              pMat->nrow(), selectColumn, minVal, maxVal, chkMin, chkMax,
              opVal, NA_INTEGER);
          case 6:
            return MWhichMatrix<float>( MatrixAccessor<float>(*pMat),
              pMat->nrow(), selectColumn, minVal, maxVal, chkMin, chkMax,
              opVal, NA_FLOAT);
          case 8:
            return MWhichMatrix<double>( MatrixAccessor<double>(*pMat),
              pMat->nrow(), selectColumn, minVal, maxVal, chkMin, chkMax,
              opVal, NA_REAL);
        }
    }
    return R_NilValue;
}

// [[Rcpp::export]]
SEXP MWhichRIntMatrix( SEXP matrixVector, SEXP nrow, SEXP selectColumn,
  SEXP minVal, SEXP maxVal, SEXP chkMin, SEXP chkMax, SEXP opVal )
{
  index_type numRows = static_cast<index_type>(Rf_asInteger(nrow));
  MatrixAccessor<int> mat(INTEGER(matrixVector), numRows);
  return MWhichMatrix<int, MatrixAccessor<int> >(mat, numRows,
    selectColumn, minVal, maxVal, chkMin, chkMax, opVal, NA_INTEGER);
}

// [[Rcpp::export]]
SEXP MWhichRNumericMatrix( SEXP matrixVector, SEXP nrow, SEXP selectColumn,
  SEXP minVal, SEXP maxVal, SEXP chkMin, SEXP chkMax, SEXP opVal )
{
  index_type numRows = static_cast<index_type>(Rf_asInteger(nrow));
  MatrixAccessor<double> mat(REAL(matrixVector), numRows);
  return MWhichMatrix<double, MatrixAccessor<double> >(mat, numRows,
    selectColumn, minVal, maxVal, chkMin, chkMax, opVal, NA_REAL);
}

// [[Rcpp::export]]
SEXP CCountLines(SEXP fileName)
{
  FILE *FP;
  double lineCount = 0;
  char readChar;
  FP = fopen(CHAR(Rf_asChar(fileName)), "r");
  SEXP ret = Rf_protect(Rf_allocVector(REALSXP,1));
  REAL(ret)[0] = -1;
  if (FP == NULL) {
    Rf_unprotect(1);
    return(ret);
  }
  do {
    readChar = fgetc(FP);
    if ('\n' == readChar) ++lineCount;
  } while( readChar != EOF );
  fclose(FP);
  REAL(ret)[0] = lineCount;
  Rf_unprotect(1);
  return(ret);
}

// [[Rcpp::export]]
SEXP ReadMatrix(SEXP fileName, SEXP bigMatAddr,
                SEXP firstLine, SEXP numLines, SEXP numCols, SEXP separator,
                SEXP hasRowNames, SEXP useRowNames)
{
    Rcpp::XPtr<BigMatrix> pMat(bigMatAddr);
    if (pMat->separated_columns())
    {
        switch (pMat->matrix_type())
        {
          case 1:
            return ReadMatrix<char, SepMatrixAccessor<char> >(
              fileName, pMat, firstLine, numLines, numCols,
              separator, hasRowNames, useRowNames, NA_CHAR, NA_CHAR, NA_CHAR,
              NA_CHAR);
          case 2:
            return ReadMatrix<short, SepMatrixAccessor<short> >(
              fileName, pMat, firstLine, numLines, numCols,
              separator, hasRowNames, useRowNames, NA_SHORT, NA_SHORT, NA_SHORT,
              NA_SHORT);
	        case 3:
			return ReadMatrix<unsigned char, SepMatrixAccessor<unsigned char> >(
				fileName, pMat, firstLine, numLines, numCols,
				separator, hasRowNames, useRowNames, NA_BYTE, NA_BYTE, NA_BYTE,
				NA_BYTE);
	        case 4:
            return ReadMatrix<int, SepMatrixAccessor<int> >(
              fileName, pMat, firstLine, numLines, numCols,
              separator, hasRowNames, useRowNames, NA_INTEGER, NA_INTEGER,
              NA_INTEGER, NA_INTEGER);
          case 6:
            return ReadMatrix<float, SepMatrixAccessor<float> >(
              fileName, pMat, firstLine, numLines, numCols,
              separator, hasRowNames, useRowNames, NA_FLOAT, NA_FLOAT,
              NA_FLOAT, NA_FLOAT);
          case 8:
            return ReadMatrix<double, SepMatrixAccessor<double> >(
              fileName, pMat, firstLine, numLines, numCols,
              separator, hasRowNames, useRowNames, NA_REAL, R_PosInf, R_NegInf,
              R_NaN);
        }
    }
    else
    {
        switch (pMat->matrix_type())
        {
          case 1:
            return ReadMatrix<char, MatrixAccessor<char> >(
              fileName, pMat, firstLine, numLines, numCols,
              separator, hasRowNames, useRowNames, NA_CHAR, NA_CHAR, NA_CHAR,
			  NA_CHAR);
          case 2:
            return ReadMatrix<short, MatrixAccessor<short> >(
              fileName, pMat, firstLine, numLines, numCols,
              separator, hasRowNames, useRowNames, NA_SHORT, NA_SHORT, NA_SHORT,
              NA_SHORT);
          case 3:
            return ReadMatrix<unsigned char, MatrixAccessor<unsigned char> >(
              fileName, pMat, firstLine, numLines, numCols,
              separator, hasRowNames, useRowNames, NA_BYTE, NA_BYTE, NA_BYTE,
              NA_BYTE);
          case 4:
            return ReadMatrix<int, MatrixAccessor<int> >(
              fileName, pMat, firstLine, numLines, numCols,
              separator, hasRowNames, useRowNames, NA_INTEGER, NA_INTEGER,
              NA_INTEGER, NA_INTEGER);
          case 6:
            return ReadMatrix<float, MatrixAccessor<float> >(
              fileName, pMat, firstLine, numLines, numCols,
              separator, hasRowNames, useRowNames, NA_FLOAT, NA_FLOAT,
              NA_FLOAT, NA_FLOAT);
          case 8:
            return ReadMatrix<double, MatrixAccessor<double> >(
              fileName, pMat, firstLine, numLines, numCols,
              separator, hasRowNames, useRowNames, NA_REAL, R_PosInf, R_NegInf,
              R_NaN);
        }
    }
    return R_NilValue;
}

// [[Rcpp::export]]
void WriteMatrix( SEXP bigMatAddr, SEXP fileName, SEXP rowNames,
  SEXP colNames, SEXP sep )
{
    Rcpp::XPtr<BigMatrix> pMat(bigMatAddr);
    if (pMat->separated_columns())
    {
        switch (pMat->matrix_type())
        {
          case 1:
            WriteMatrix<char, SepMatrixAccessor<char> >(
              pMat, fileName, rowNames, colNames, sep, NA_CHAR);
            break;
          case 2:
            WriteMatrix<short, SepMatrixAccessor<short> >(
              pMat, fileName, rowNames, colNames, sep, NA_SHORT);
            break;
          case 3:
            WriteMatrix<unsigned char, SepMatrixAccessor<unsigned char> >(
              pMat, fileName, rowNames, colNames, sep, NA_BYTE);
            break;
          case 4:
            WriteMatrix<int, SepMatrixAccessor<int> >(
              pMat, fileName, rowNames, colNames, sep, NA_INTEGER);
            break;
          case 6:
            WriteMatrix<float, SepMatrixAccessor<float> >(
              pMat, fileName, rowNames, colNames, sep, NA_FLOAT);
            break;
          case 8:
            WriteMatrix<double, SepMatrixAccessor<double> >(
              pMat, fileName, rowNames, colNames, sep, NA_REAL);
        }
    }
    else
    {
        switch (pMat->matrix_type())
        {
          case 1:
            WriteMatrix<char, MatrixAccessor<char> >(
              pMat, fileName, rowNames, colNames, sep, NA_CHAR);
            break;
          case 2:
            WriteMatrix<short, MatrixAccessor<short> >(
              pMat, fileName, rowNames, colNames, sep, NA_SHORT);
            break;
          case 3:
            WriteMatrix<unsigned char, MatrixAccessor<unsigned char> >(
              pMat, fileName, rowNames, colNames, sep, NA_BYTE);
            break;
          case 4:
            WriteMatrix<int, MatrixAccessor<int> >(
              pMat, fileName, rowNames, colNames, sep, NA_INTEGER);
            break;
          case 6:
            WriteMatrix<float, MatrixAccessor<float> >(
              pMat, fileName, rowNames, colNames, sep, NA_FLOAT);
            break;
          case 8:
            WriteMatrix<double, MatrixAccessor<double> >(
              pMat, fileName, rowNames, colNames, sep, NA_REAL);
        }
    }
}

// [[Rcpp::export]]
SEXP GetMatrixElements(SEXP bigMatAddr, SEXP col, SEXP row)
{
  Rcpp::XPtr<BigMatrix> pMat(bigMatAddr);
  if (pMat->separated_columns())
  {
    switch(pMat->matrix_type())
    {
      case 1:
        return GetMatrixElements<char, int, SepMatrixAccessor<char> >
          (pMat, NA_CHAR, NA_INTEGER, col, row, INTSXP);
      case 2:
        return GetMatrixElements<short,int, SepMatrixAccessor<short> >
          (pMat, NA_SHORT, NA_INTEGER, col, row, INTSXP);
      case 3:
        return GetMatrixElements<unsigned char, unsigned char, SepMatrixAccessor<unsigned char> >
          (pMat, NA_BYTE, NA_INTEGER, col, row, RAWSXP);
      case 4:
        return GetMatrixElements<int, int, SepMatrixAccessor<int> >
          (pMat, NA_INTEGER, NA_INTEGER, col, row, INTSXP);
      case 6:
        // possibly area of problem (REALSXP -> FLOATSXP???) but I think okay
        return GetMatrixElements<float, double, SepMatrixAccessor<float> >
          (pMat, NA_FLOAT, NA_FLOAT, col, row, REALSXP);
      case 8:
        return GetMatrixElements<double, double, SepMatrixAccessor<double> >(
          pMat, NA_REAL, NA_REAL, col, row, REALSXP);
    }
  }
  else
  {
    switch(pMat->matrix_type())
    {
      case 1:
        return GetMatrixElements<char, int, MatrixAccessor<char> >(
          pMat, NA_CHAR, NA_INTEGER, col, row, INTSXP);
      case 2:
        return GetMatrixElements<short, int, MatrixAccessor<short> >(
          pMat, NA_SHORT, NA_INTEGER, col, row, INTSXP);
      case 3:
        return GetMatrixElements<unsigned char, unsigned char, MatrixAccessor<unsigned char> >(
          pMat, NA_BYTE, NA_INTEGER, col, row, RAWSXP);
      case 4:
        return GetMatrixElements<int, int, MatrixAccessor<int> >(
          pMat, NA_INTEGER, NA_INTEGER, col, row, INTSXP);
      case 6:
        return GetMatrixElements<float, double, MatrixAccessor<float> >(
          pMat, NA_FLOAT, NA_FLOAT, col, row, REALSXP);
      case 8:
        return GetMatrixElements<double, double, MatrixAccessor<double> >
          (pMat, NA_REAL, NA_REAL, col, row, REALSXP);
    }
  }
  return R_NilValue;
}

// [[Rcpp::export]]
SEXP GetMatrixRows(SEXP bigMatAddr, SEXP row)
{
  Rcpp::XPtr<BigMatrix> pMat(bigMatAddr);
  if (pMat->separated_columns())
  {
    switch(pMat->matrix_type())
    {
      case 1:
        return GetMatrixRows<char, int, SepMatrixAccessor<char> >
          (pMat, NA_CHAR, NA_INTEGER, row, INTSXP);
      case 2:
        return GetMatrixRows<short,int, SepMatrixAccessor<short> >
          (pMat, NA_SHORT, NA_INTEGER, row, INTSXP);
      case 3:
        return GetMatrixRows<unsigned char, unsigned char, SepMatrixAccessor<unsigned char> >
          (pMat, NA_BYTE, NA_INTEGER, row, RAWSXP);
      case 4:
        return GetMatrixRows<int, int, SepMatrixAccessor<int> >
          (pMat, NA_INTEGER, NA_INTEGER, row, INTSXP);
      case 6:
        return GetMatrixRows<float, double, SepMatrixAccessor<float> >
          (pMat, NA_FLOAT, NA_FLOAT, row, REALSXP);
      case 8:
        return GetMatrixRows<double, double, SepMatrixAccessor<double> >(
          pMat, NA_REAL, NA_REAL, row, REALSXP);
    }
  }
  else
  {
    switch(pMat->matrix_type())
    {
      case 1:
        return GetMatrixRows<char, int, MatrixAccessor<char> >(
          pMat, NA_CHAR, NA_INTEGER, row, INTSXP);
      case 2:
        return GetMatrixRows<short, int, MatrixAccessor<short> >(
          pMat, NA_SHORT, NA_INTEGER, row, INTSXP);
      case 3:
        return GetMatrixRows<unsigned char, unsigned char, MatrixAccessor<unsigned char> >(
          pMat, NA_BYTE, NA_INTEGER, row, RAWSXP);
      case 4:
        return GetMatrixRows<int, int, MatrixAccessor<int> >(
          pMat, NA_INTEGER, NA_INTEGER, row, INTSXP);
      case 6:
        return GetMatrixRows<float, double, MatrixAccessor<float> >(
          pMat, NA_FLOAT, NA_FLOAT, row, REALSXP);
      case 8:
        return GetMatrixRows<double, double, MatrixAccessor<double> >
          (pMat, NA_REAL, NA_REAL, row, REALSXP);
    }
  }
  return R_NilValue;
}

// [[Rcpp::export]]
SEXP GetMatrixCols(SEXP bigMatAddr, SEXP col)
{
  Rcpp::XPtr<BigMatrix> pMat(bigMatAddr);
  if (pMat->separated_columns())
  {
    switch(pMat->matrix_type())
    {
      case 1:
        return GetMatrixCols<char, int, SepMatrixAccessor<char> >
          (pMat, NA_CHAR, NA_INTEGER, col, INTSXP);
      case 2:
        return GetMatrixCols<short,int, SepMatrixAccessor<short> >
          (pMat, NA_SHORT, NA_INTEGER, col, INTSXP);
      case 3:
        return GetMatrixCols<unsigned char, unsigned char, SepMatrixAccessor<unsigned char> >
          (pMat, NA_BYTE, NA_INTEGER, col, RAWSXP);
      case 4:
        return GetMatrixCols<int, int, SepMatrixAccessor<int> >
          (pMat, NA_INTEGER, NA_INTEGER, col, INTSXP);
      case 6:
        return GetMatrixCols<float, double, SepMatrixAccessor<float> >
          (pMat, NA_FLOAT, NA_FLOAT, col, REALSXP);
      case 8:
        return GetMatrixCols<double, double, SepMatrixAccessor<double> >(
          pMat, NA_REAL, NA_REAL, col, REALSXP);
    }
  }
  else
  {
    switch(pMat->matrix_type())
    {
      case 1:
        return GetMatrixCols<char, int, MatrixAccessor<char> >(
          pMat, NA_CHAR, NA_INTEGER, col, INTSXP);
      case 2:
        return GetMatrixCols<short, int, MatrixAccessor<short> >(
          pMat, NA_SHORT, NA_INTEGER, col, INTSXP);
      case 3:
        return GetMatrixCols<unsigned char, unsigned char, MatrixAccessor<unsigned char> >(
          pMat, NA_BYTE, NA_INTEGER, col, RAWSXP);
      case 4:
        return GetMatrixCols<int, int, MatrixAccessor<int> >(
          pMat, NA_INTEGER, NA_INTEGER, col, INTSXP);
      case 6:
        return GetMatrixCols<float, double, MatrixAccessor<float> >(
          pMat, NA_FLOAT, NA_FLOAT, col, REALSXP);
      case 8:
        return GetMatrixCols<double, double, MatrixAccessor<double> >
          (pMat, NA_REAL, NA_REAL, col, REALSXP);
    }
  }
  return R_NilValue;
}

// remove col because not used
// [[Rcpp::export]]
SEXP GetMatrixAll(SEXP bigMatAddr)
{
  Rcpp::XPtr<BigMatrix> pMat(bigMatAddr);
  if (pMat->separated_columns())
  {
    switch(pMat->matrix_type())
    {
      case 1:
        return GetMatrixAll<char, int, SepMatrixAccessor<char> >
          (pMat, NA_CHAR, NA_INTEGER, INTSXP);
      case 2:
        return GetMatrixAll<short,int, SepMatrixAccessor<short> >
          (pMat, NA_SHORT, NA_INTEGER, INTSXP);
      case 3:
        return GetMatrixAll<unsigned char, unsigned char, SepMatrixAccessor<unsigned char> >
          (pMat, NA_BYTE, NA_INTEGER, RAWSXP);
      case 4:
        return GetMatrixAll<int, int, SepMatrixAccessor<int> >
          (pMat, NA_INTEGER, NA_INTEGER, INTSXP);
      case 6:
        return GetMatrixAll<float, double, SepMatrixAccessor<float> >
          (pMat, NA_FLOAT, NA_FLOAT, REALSXP);
      case 8:
        return GetMatrixAll<double, double, SepMatrixAccessor<double> >(
          pMat, NA_REAL, NA_REAL, REALSXP);
    }
  }
  else
  {
    switch(pMat->matrix_type())
    {
    case 1:
      return GetMatrixAll<char, int, MatrixAccessor<char> >(
        pMat, NA_CHAR, NA_INTEGER, INTSXP);
      case 2:
        return GetMatrixAll<short, int, MatrixAccessor<short> >(
          pMat, NA_SHORT, NA_INTEGER, INTSXP);
      case 3:
        return GetMatrixAll<unsigned char, unsigned char, MatrixAccessor<unsigned char> >(
          pMat, NA_BYTE, NA_INTEGER, RAWSXP);
      case 4:
        return GetMatrixAll<int, int, MatrixAccessor<int> >(
          pMat, NA_INTEGER, NA_INTEGER, INTSXP);
      case 6:
        return GetMatrixAll<float, double, MatrixAccessor<float> >(
          pMat, NA_FLOAT, NA_REAL, REALSXP);
      case 8:
        return GetMatrixAll<double, double, MatrixAccessor<double> >
          (pMat, NA_REAL, NA_REAL, REALSXP);
    }
  }
  return R_NilValue;
}

// [[Rcpp::export]]
void SetMatrixElements(SEXP bigMatAddr, SEXP col, SEXP row, SEXP values)
{
  Rcpp::XPtr<BigMatrix> pMat(bigMatAddr);

  if (pMat->separated_columns())
  {
    switch (pMat->matrix_type())
    {
      case 1:
        SetMatrixElements<char, int, SepMatrixAccessor<char> >(
          pMat, col, row, values, NA_CHAR, R_CHAR_MIN, R_CHAR_MAX, NA_INTEGER);
        break;
      case 2:
        SetMatrixElements<short, int, SepMatrixAccessor<short> >(
          pMat, col, row, values, NA_SHORT, R_SHORT_MIN, R_SHORT_MAX,
          NA_INTEGER);
        break;
      case 3:
        SetMatrixElements<unsigned char, unsigned char, SepMatrixAccessor<unsigned char> >(
          pMat, col, row, values, NA_BYTE, R_BYTE_MIN, R_BYTE_MAX, NA_INTEGER);
        break;
      case 4:
        SetMatrixElements<int, int, SepMatrixAccessor<int> >(
          pMat, col, row, values, NA_INTEGER, R_INT_MIN, R_INT_MAX, NA_INTEGER);
        break;
      case 6:
        SetMatrixElements<float, double, SepMatrixAccessor<float> >(
          pMat, col, row, values, NA_FLOAT, R_FLT_MIN, R_FLT_MAX, NA_FLOAT);
        break;
      case 8:
        SetMatrixElements<double, double, SepMatrixAccessor<double> >(
          pMat, col, row, values, NA_REAL, R_DOUBLE_MIN, R_DOUBLE_MAX, NA_REAL);
    }
  }
  else
  {
    switch (pMat->matrix_type())
    {
      case 1:
        SetMatrixElements<char, int, MatrixAccessor<char> >(
          pMat, col, row, values, NA_CHAR, R_CHAR_MIN, R_CHAR_MAX, NA_INTEGER);
        break;
      case 2:
        SetMatrixElements<short, int, MatrixAccessor<short> >(
          pMat, col, row, values, NA_SHORT, R_SHORT_MIN, R_SHORT_MAX,
          NA_INTEGER);
        break;
      case 3:
        SetMatrixElements<unsigned char, unsigned char, MatrixAccessor<unsigned char> >(
          pMat, col, row, values, NA_BYTE, R_BYTE_MIN, R_BYTE_MAX, NA_INTEGER);
        break;
      case 4:
        SetMatrixElements<int, int, MatrixAccessor<int> >(
          pMat, col, row, values, NA_INTEGER, R_INT_MIN, R_INT_MAX, NA_INTEGER);
        break;
      case 6:
        SetMatrixElements<float, double, MatrixAccessor<float> >(
          pMat, col, row, values, NA_FLOAT, R_FLT_MIN, R_FLT_MAX, NA_FLOAT);
        break;
      case 8:
        SetMatrixElements<double, double, MatrixAccessor<double> >(
          pMat, col, row, values, NA_REAL, R_DOUBLE_MIN, R_DOUBLE_MAX, NA_REAL);
    }
  }
}

// Function contributed by Charles Determan Jr.
// [[Rcpp::export]]
void
SetIndivVectorMatrixElements(
    SEXP bigMatAddr,
    NumericVector elems,
    NumericVector inVec)
{
  BigMatrix *pMat =
    reinterpret_cast<BigMatrix*>(R_ExternalPtrAddr(bigMatAddr));
  if (pMat->separated_columns())
  {
    switch(pMat->matrix_type())
    {
    case 1:
      SetIndivVectorMatrixElements<char, int, SepMatrixAccessor<char>, IntegerVector >(
          pMat, NA_CHAR, NA_INTEGER, elems, inVec);
      break;
    case 2:
      SetIndivVectorMatrixElements<short,int, SepMatrixAccessor<short>, IntegerVector >(
          pMat, NA_SHORT, NA_INTEGER, elems, inVec);
      break;
    case 3:
      SetIndivVectorMatrixElements<unsigned char, unsigned char, SepMatrixAccessor<unsigned char>, IntegerVector >(
          pMat, NA_BYTE, NA_INTEGER, elems, inVec);
      break;
    case 4:
      SetIndivVectorMatrixElements<int, int, SepMatrixAccessor<int>, IntegerVector >(
          pMat, NA_INTEGER, NA_INTEGER, elems, inVec);
      break;
    case 6:
      SetIndivVectorMatrixElements<float, double, SepMatrixAccessor<float>, NumericVector >(
          pMat, NA_FLOAT, NA_FLOAT, elems, inVec);
      break;
    case 8:
      SetIndivVectorMatrixElements<double,double,SepMatrixAccessor<double>, NumericVector >(
          pMat, NA_REAL, NA_REAL, elems, inVec);
      break;
    }
  }
  else
  {
    switch(pMat->matrix_type())
    {
    case 1:
      SetIndivVectorMatrixElements<char, int, MatrixAccessor<char>, IntegerVector >(
          pMat, NA_CHAR, NA_INTEGER, elems, inVec);
      break;
    case 2:
      SetIndivVectorMatrixElements<short, int, MatrixAccessor<short>, IntegerVector >(
          pMat, NA_SHORT, NA_INTEGER, elems, inVec);
      break;
    case 3:
      SetIndivVectorMatrixElements<unsigned char, unsigned char, MatrixAccessor<unsigned char>, IntegerVector >(
          pMat, NA_BYTE, NA_INTEGER, elems, inVec);
      break;
    case 4:
      SetIndivVectorMatrixElements<int, int, MatrixAccessor<int>, IntegerVector >(
          pMat, NA_INTEGER, NA_INTEGER, elems, inVec);
      break;
    case 6:
      SetIndivVectorMatrixElements<float, double, MatrixAccessor<float>, NumericVector >(
          pMat, NA_FLOAT, NA_FLOAT, elems, inVec);
      break;
    case 8:
      SetIndivVectorMatrixElements<double, double, MatrixAccessor<double>, NumericVector >(
          pMat, NA_REAL, NA_REAL, elems, inVec);
      break;
    }
  }
}

// Function contributed by Peter Haverty at Genentech.
// [[Rcpp::export]]
void SetIndivMatrixElements(SEXP bigMatAddr, SEXP col, SEXP row, SEXP values)
{
  Rcpp::XPtr<BigMatrix> pMat(bigMatAddr);
  if (pMat->separated_columns())
  {
    switch (pMat->matrix_type())
    {
    case 1:
      SetIndivMatrixElements<char, int, SepMatrixAccessor<char> >(
        pMat, col, row, values, NA_CHAR, R_CHAR_MIN, R_CHAR_MAX, NA_INTEGER);
      break;
    case 2:
      SetIndivMatrixElements<short, int, SepMatrixAccessor<short> >(
        pMat, col, row, values, NA_SHORT, R_SHORT_MIN, R_SHORT_MAX, NA_INTEGER);
      break;
    case 3:
      SetIndivMatrixElements<unsigned char, unsigned char, SepMatrixAccessor<unsigned char> >(
        pMat, col, row, values, NA_BYTE, R_BYTE_MIN, R_BYTE_MAX, NA_INTEGER);
      break;
    case 4:
      SetIndivMatrixElements<int, int, SepMatrixAccessor<int> >(
        pMat, col, row, values, NA_INTEGER, R_INT_MIN, R_INT_MAX, NA_INTEGER);
      break;
    case 6:
      SetIndivMatrixElements<float, double, SepMatrixAccessor<float> >(
        pMat, col, row, values, NA_FLOAT, R_FLT_MIN, R_FLT_MAX, NA_FLOAT);
      break;
    case 8:
      SetIndivMatrixElements<double, double, SepMatrixAccessor<double> >(
        pMat, col, row, values, NA_REAL, R_DOUBLE_MIN, R_DOUBLE_MAX, NA_REAL);
    }
  }
  else
  {
    switch (pMat->matrix_type())
    {
    case 1:
      SetIndivMatrixElements<char, int, MatrixAccessor<char> >(
        pMat, col, row, values, NA_CHAR, R_CHAR_MIN, R_CHAR_MAX, NA_INTEGER);
      break;
    case 2:
      SetIndivMatrixElements<short, int, MatrixAccessor<short> >(
        pMat, col, row, values, NA_SHORT, R_SHORT_MIN, R_SHORT_MAX, NA_INTEGER);
      break;
    case 3:
      SetIndivMatrixElements<unsigned char, unsigned char, MatrixAccessor<unsigned char> >(
        pMat, col, row, values, NA_BYTE, R_BYTE_MIN, R_BYTE_MAX, NA_INTEGER);
      break;
    case 4:
      SetIndivMatrixElements<int, int, MatrixAccessor<int> >(
        pMat, col, row, values, NA_INTEGER, R_INT_MIN, R_INT_MAX, NA_INTEGER);
      break;
    case 6:
      SetIndivMatrixElements<float, double, MatrixAccessor<float> >(
        pMat, col, row, values, NA_FLOAT, R_FLT_MIN, R_FLT_MAX, NA_FLOAT);
      break;
    case 8:
      SetIndivMatrixElements<double, double, MatrixAccessor<double> >(
        pMat, col, row, values, NA_REAL, R_DOUBLE_MIN, R_DOUBLE_MAX, NA_REAL);
    }
  }
}

// [[Rcpp::export]]
void SetMatrixAll(SEXP bigMatAddr, SEXP values)
{
  Rcpp::XPtr<BigMatrix> pMat(bigMatAddr);
  if (pMat->separated_columns())
  {
    switch (pMat->matrix_type())
    {
      case 1:
        SetMatrixAll<char, int, SepMatrixAccessor<char> >(
          pMat, values, NA_CHAR, R_CHAR_MIN, R_CHAR_MAX, NA_INTEGER);
        break;
      case 2:
        SetMatrixAll<short, int, SepMatrixAccessor<short> >(
          pMat, values, NA_SHORT, R_SHORT_MIN, R_SHORT_MAX,
          NA_INTEGER);
        break;
      case 3:
        SetMatrixAll<unsigned char, unsigned char, SepMatrixAccessor<unsigned char> >(
          pMat, values, NA_BYTE, R_BYTE_MIN, R_BYTE_MAX, NA_INTEGER);
        break;
      case 4:
        SetMatrixAll<int, int, SepMatrixAccessor<int> >(
          pMat, values, NA_INTEGER, R_INT_MIN, R_INT_MAX, NA_INTEGER);
        break;
      case 6:
        SetMatrixAll<float, double, SepMatrixAccessor<float> >(
          pMat, values, NA_FLOAT, R_FLT_MIN, R_FLT_MAX, NA_FLOAT);
        break;
      case 8:
        SetMatrixAll<double, double, SepMatrixAccessor<double> >(
          pMat, values, NA_REAL, R_DOUBLE_MIN, R_DOUBLE_MAX, NA_REAL);
    }
  }
  else
  {
    switch (pMat->matrix_type())
    {
      case 1:
        SetMatrixAll<char, int, MatrixAccessor<char> >(
          pMat, values, NA_CHAR, R_CHAR_MIN, R_CHAR_MAX, NA_INTEGER);
        break;
      case 2:
        SetMatrixAll<short, int, MatrixAccessor<short> >(
          pMat, values, NA_SHORT, R_SHORT_MIN, R_SHORT_MAX,
          NA_INTEGER);
        break;
      case 3:
        SetMatrixAll<unsigned char, unsigned char, MatrixAccessor<unsigned char> >(
          pMat, values, NA_BYTE, R_BYTE_MIN, R_BYTE_MAX, NA_INTEGER);
        break;
      case 4:
        SetMatrixAll<int, int, MatrixAccessor<int> >(
          pMat, values, NA_INTEGER, R_INT_MIN, R_INT_MAX, NA_INTEGER);
        break;
      case 6:
        SetMatrixAll<float, double, MatrixAccessor<float> >(
          pMat, values, NA_FLOAT, R_FLT_MIN, R_FLT_MAX, NA_FLOAT);
        break;
      case 8:
        SetMatrixAll<double, double, MatrixAccessor<double> >(
          pMat, values, NA_REAL, R_DOUBLE_MIN, R_DOUBLE_MAX, NA_REAL);
    }
  }
}

// [[Rcpp::export]]
void SetMatrixCols(SEXP bigMatAddr, SEXP col, SEXP values)
{
  Rcpp::XPtr<BigMatrix> pMat(bigMatAddr);
  if (pMat->separated_columns())
  {
    switch (pMat->matrix_type())
    {
      case 1:
        SetMatrixCols<char, int, SepMatrixAccessor<char> >(
          pMat, col, values, NA_CHAR, R_CHAR_MIN, R_CHAR_MAX, NA_INTEGER);
        break;
      case 2:
        SetMatrixCols<short, int, SepMatrixAccessor<short> >(
          pMat, col, values, NA_SHORT, R_SHORT_MIN, R_SHORT_MAX,
          NA_INTEGER);
        break;
      case 3:
        SetMatrixCols<unsigned char, unsigned char, SepMatrixAccessor<unsigned char> >(
          pMat, col, values, NA_BYTE, R_BYTE_MIN, R_BYTE_MAX, NA_INTEGER);
        break;
      case 4:
        SetMatrixCols<int, int, SepMatrixAccessor<int> >(
          pMat, col, values, NA_INTEGER, R_INT_MIN, R_INT_MAX, NA_INTEGER);
        break;
      case 6:
        SetMatrixCols<float, double, SepMatrixAccessor<float> >(
          pMat, col, values, NA_FLOAT, R_FLT_MIN, R_FLT_MAX, NA_FLOAT);
        break;
      case 8:
        SetMatrixCols<double, double, SepMatrixAccessor<double> >(
          pMat, col, values, NA_REAL, R_DOUBLE_MIN, R_DOUBLE_MAX, NA_REAL);
    }
  }
  else
  {
    switch (pMat->matrix_type())
    {
      case 1:
        SetMatrixCols<char, int, MatrixAccessor<char> >(
          pMat, col, values, NA_CHAR, R_CHAR_MIN, R_CHAR_MAX, NA_INTEGER);
        break;
      case 2:
        SetMatrixCols<short, int, MatrixAccessor<short> >(
          pMat, col, values, NA_SHORT, R_SHORT_MIN, R_SHORT_MAX,
          NA_INTEGER);
        break;
      case 3:
        SetMatrixCols<unsigned char, unsigned char, MatrixAccessor<unsigned char> >(
          pMat, col, values, NA_BYTE, R_BYTE_MIN, R_BYTE_MAX, NA_INTEGER);
        break;
      case 4:
        SetMatrixCols<int, int, MatrixAccessor<int> >(
          pMat, col, values, NA_INTEGER, R_INT_MIN, R_INT_MAX, NA_INTEGER);
        break;
      case 6:
        SetMatrixCols<float, double, MatrixAccessor<float> >(
          pMat, col, values, NA_FLOAT, R_FLT_MIN, R_FLT_MAX, NA_FLOAT);
        break;
      case 8:
        SetMatrixCols<double, double, MatrixAccessor<double> >(
          pMat, col, values, NA_REAL, R_DOUBLE_MIN, R_DOUBLE_MAX, NA_REAL);
    }
  }
}

// [[Rcpp::export]]
void SetMatrixRows(SEXP bigMatAddr, SEXP row, SEXP values)
{
  Rcpp::XPtr<BigMatrix> pMat(bigMatAddr);
  if (pMat->separated_columns())
  {
    switch (pMat->matrix_type())
    {
      case 1:
        SetMatrixRows<char, int, SepMatrixAccessor<char> >(
          pMat, row, values, NA_CHAR, R_CHAR_MIN, R_CHAR_MAX, NA_INTEGER);
        break;
      case 2:
        SetMatrixRows<short, int, SepMatrixAccessor<short> >(
          pMat, row, values, NA_SHORT, R_SHORT_MIN, R_SHORT_MAX,
          NA_INTEGER);
        break;
      case 3:
        SetMatrixRows<unsigned char, unsigned char, SepMatrixAccessor<unsigned char> >(
          pMat, row, values, NA_BYTE, R_BYTE_MIN, R_BYTE_MAX, NA_INTEGER);
        break;
      case 4:
        SetMatrixRows<int, int, SepMatrixAccessor<int> >(
          pMat, row, values, NA_INTEGER, R_INT_MIN, R_INT_MAX, NA_INTEGER);
        break;
      case 6:
        SetMatrixRows<float, double, SepMatrixAccessor<float> >(
          pMat, row, values, NA_FLOAT, R_FLT_MIN, R_FLT_MAX, NA_FLOAT);
        break;
      case 8:
        SetMatrixRows<double, double, SepMatrixAccessor<double> >(
          pMat, row, values, NA_REAL, R_DOUBLE_MIN, R_DOUBLE_MAX, NA_REAL);
    }
  }
  else
  {
    switch (pMat->matrix_type())
    {
      case 1:
        SetMatrixRows<char, int, MatrixAccessor<char> >(
          pMat, row, values, NA_CHAR, R_CHAR_MIN, R_CHAR_MAX, NA_INTEGER);
        break;
      case 2:
        SetMatrixRows<short, int, MatrixAccessor<short> >(
          pMat, row, values, NA_SHORT, R_SHORT_MIN, R_SHORT_MAX,
          NA_INTEGER);
        break;
      case 3:
        SetMatrixRows<unsigned char, unsigned char, MatrixAccessor<unsigned char> >(
          pMat, row, values, NA_BYTE, R_BYTE_MIN, R_BYTE_MAX, NA_INTEGER);
        break;
      case 4:
        SetMatrixRows<int, int, MatrixAccessor<int> >(
          pMat, row, values, NA_INTEGER, R_INT_MIN, R_INT_MAX, NA_INTEGER);
        break;
      case 6:
        SetMatrixRows<float, double, MatrixAccessor<float> >(
          pMat, row, values, NA_FLOAT, R_FLT_MIN, R_FLT_MAX, NA_FLOAT);
        break;
      case 8:
        SetMatrixRows<double, double, MatrixAccessor<double> >(
          pMat, row, values, NA_REAL, R_DOUBLE_MIN, R_DOUBLE_MAX, NA_REAL);
    }
  }
}

// [[Rcpp::export]]
SEXP CreateSharedMatrix(SEXP row, SEXP col, SEXP colnames, SEXP rownames,
  SEXP typeLength, SEXP ini, SEXP separated)
{
  return CreateRAMMatrix<SharedMemoryBigMatrix>(row, col, colnames,
    rownames, typeLength, ini, separated);
}

// [[Rcpp::export]]
SEXP CreateLocalMatrix(SEXP row, SEXP col, SEXP colnames, SEXP rownames,
  SEXP typeLength, SEXP ini, SEXP separated)
{
  return CreateRAMMatrix<LocalBigMatrix>(row, col, colnames,
    rownames, typeLength, ini, separated);
}

// [[Rcpp::export]]
SEXP CreateFileBackedBigMatrix(SEXP fileName, SEXP filePath, SEXP row,
  SEXP col, SEXP colnames, SEXP rownames, SEXP typeLength, SEXP ini,
  SEXP separated)
{
  try
  {
    FileBackedBigMatrix *pMat = new FileBackedBigMatrix();
    string fn;
    string path = ((filePath == R_NilValue) ?
      "" :
      RChar2String(filePath));
    if (Rf_isNull(fileName))
    {
      fn=pMat->uuid()+".bin";
    }
    else
    {
      fn = RChar2String(fileName);
    }
    if (!pMat->create( fn, RChar2String(filePath),
      static_cast<index_type>(REAL(row)[0]),
      static_cast<index_type>(REAL(col)[0]),
      Rf_asInteger(typeLength),
      static_cast<bool>(LOGICAL(separated)[0])))
    {
      delete pMat;
      Rf_error("Problem creating filebacked matrix.");
      return R_NilValue;
    }
    if (colnames != R_NilValue)
    {
      pMat->column_names(RChar2StringVec(colnames));
    }
    if (rownames != R_NilValue)
    {
      pMat->row_names(RChar2StringVec(rownames));
    }
    if (Rf_length(ini) != 0)
    {
      if (pMat->separated_columns())
      {
        switch (pMat->matrix_type())
        {
          case 1:
            SetAllMatrixElements<char, SepMatrixAccessor<char> >(
              pMat, ini, NA_CHAR, R_CHAR_MIN, R_CHAR_MAX, NA_REAL);
            break;
          case 2:
            SetAllMatrixElements<short, SepMatrixAccessor<short> >(
              pMat, ini, NA_SHORT, R_SHORT_MIN, R_SHORT_MAX, NA_REAL);
            break;
          case 3:
            SetAllMatrixElements<unsigned char, SepMatrixAccessor<unsigned char> >(
              pMat, ini, NA_BYTE, R_BYTE_MIN, R_BYTE_MAX, NA_REAL);
            break;
          case 4:
            SetAllMatrixElements<int, SepMatrixAccessor<int> >(
              pMat, ini, NA_INTEGER, R_INT_MIN, R_INT_MAX, NA_REAL);
            break;
          case 6:
            SetAllMatrixElements<float, SepMatrixAccessor<float> >(
              pMat, ini, NA_FLOAT, R_FLT_MIN, R_FLT_MAX, NA_REAL);
            break;
          case 8:
            SetAllMatrixElements<double, SepMatrixAccessor<double> >(
              pMat, ini, NA_REAL, R_DOUBLE_MIN, R_DOUBLE_MAX, NA_REAL);
        }
      }
      else
      {
        switch (pMat->matrix_type())
        {
          case 1:
            SetAllMatrixElements<char, MatrixAccessor<char> >(
              pMat, ini, NA_CHAR, R_CHAR_MIN, R_CHAR_MAX, NA_REAL);
            break;
          case 2:
            SetAllMatrixElements<short, MatrixAccessor<short> >(
              pMat, ini, NA_SHORT, R_SHORT_MIN, R_SHORT_MAX, NA_REAL);
            break;
          case 3:
            SetAllMatrixElements<unsigned char, MatrixAccessor<unsigned char> >(
              pMat, ini, NA_BYTE, R_BYTE_MIN, R_BYTE_MAX, NA_REAL);
            break;
          case 4:
            SetAllMatrixElements<int, MatrixAccessor<int> >(
              pMat, ini, NA_INTEGER, R_INT_MIN, R_INT_MAX, NA_REAL);
            break;
          case 6:
            SetAllMatrixElements<float, MatrixAccessor<float> >(
              pMat, ini, NA_FLOAT, R_FLT_MIN, R_FLT_MAX, NA_REAL);
            break;
          case 8:
            SetAllMatrixElements<double, MatrixAccessor<double> >(
              pMat, ini, NA_REAL, R_DOUBLE_MIN, R_DOUBLE_MAX, NA_REAL);
        }
      }
    }
    SEXP address = R_MakeExternalPtr( dynamic_cast<BigMatrix*>(pMat),
      R_NilValue, R_NilValue);
    R_RegisterCFinalizerEx(address, (R_CFinalizer_t) CDestroyBigMatrix,
        (Rboolean) TRUE);
    return address;
  }
  catch(std::exception &e)
  {
    Rprintf("%s\n", e.what());
  }
  catch(...)
  {
    Rprintf("Unspecified problem trying to create big.matrix\n");
  }
  return R_NilValue;
}

// [[Rcpp::export]]
SEXP CAttachSharedBigMatrix(SEXP sharedName, SEXP rows, SEXP cols,
  SEXP rowNames, SEXP colNames, SEXP typeLength, SEXP separated,
  SEXP readOnly)
{
  SharedMemoryBigMatrix *pMat = new SharedMemoryBigMatrix();
  bool connected = pMat->connect(
    string(CHAR(STRING_ELT(sharedName,0))),
    static_cast<index_type>(REAL(rows)[0]),
    static_cast<index_type>(REAL(cols)[0]),
    Rf_asInteger(typeLength),
    static_cast<bool>(LOGICAL(separated)[0]),
    static_cast<bool>(LOGICAL(readOnly)[0]));
  if (!connected)
  {
    delete pMat;
    return R_NilValue;
  }
  if (Rf_length(colNames) > 0)
  {
    pMat->column_names(RChar2StringVec(colNames));
  }
  if (Rf_length(rowNames) > 0)
  {
    pMat->row_names(RChar2StringVec(rowNames));
  }
  SEXP address = R_MakeExternalPtr( dynamic_cast<BigMatrix*>(pMat),
    R_NilValue, R_NilValue);
  R_RegisterCFinalizerEx(address, (R_CFinalizer_t) CDestroyBigMatrix,
      (Rboolean) TRUE);
  return address;
}

// [[Rcpp::export]]
SEXP CAttachFileBackedBigMatrix(SEXP fileName,
  SEXP filePath, SEXP rows, SEXP cols, SEXP rowNames, SEXP colNames,
  SEXP typeLength, SEXP separated, SEXP readOnly)
{
  FileBackedBigMatrix *pMat = new FileBackedBigMatrix();
  bool connected = pMat->connect(
    string(CHAR(STRING_ELT(fileName,0))),
    string(CHAR(STRING_ELT(filePath,0))),
    static_cast<index_type>(REAL(rows)[0]),
    static_cast<index_type>(REAL(cols)[0]),
    Rf_asInteger(typeLength),
    static_cast<bool>(LOGICAL(separated)[0]),
    static_cast<bool>(LOGICAL(readOnly)[0]));
  if (!connected)
  {
    delete pMat;
    return R_NilValue;
  }
  if (Rf_length(colNames) > 0)
  {
    pMat->column_names(RChar2StringVec(colNames));
  }
  if (Rf_length(rowNames) > 0)
  {
    pMat->row_names(RChar2StringVec(rowNames));
  }
  SEXP address = R_MakeExternalPtr( dynamic_cast<BigMatrix*>(pMat),
    R_NilValue, R_NilValue);
  R_RegisterCFinalizerEx(address, (R_CFinalizer_t) CDestroyBigMatrix,
      (Rboolean) TRUE);
  return address;
}

// [[Rcpp::export]]
SEXP SharedName( SEXP address )
{
  BigMatrix *pMat = (BigMatrix*)R_ExternalPtrAddr(address);
  SharedMemoryBigMatrix *psmbm = dynamic_cast<SharedMemoryBigMatrix*>(pMat);
  if (psmbm) return String2RChar(psmbm->shared_name());
  Rf_error("Object is not a shared memory big.matrix.");
  return R_NilValue;

}

// [[Rcpp::export]]
SEXP FileName( SEXP address )
{
  BigMatrix *pMat = (BigMatrix*)R_ExternalPtrAddr(address);
  FileBackedBigMatrix *pfbbm = dynamic_cast<FileBackedBigMatrix*>(pMat);
  if (pfbbm) return String2RChar(pfbbm->file_name());
  Rf_error("Object is not a filebacked big.matrix.");
  return R_NilValue;
}

// [[Rcpp::export]]
SEXP DirName( SEXP address )
{
  BigMatrix *pMat = (BigMatrix*)R_ExternalPtrAddr(address);
  FileBackedBigMatrix *pfbbm = dynamic_cast<FileBackedBigMatrix*>(pMat);
  if (pfbbm) return String2RChar(pfbbm->file_path());
  Rf_error("Object is not a filebacked big.matrix.");
  return R_NilValue;
}

// [[Rcpp::export]]
SEXP Flush( SEXP address )
{
  FileBackedBigMatrix *pMat =
    reinterpret_cast<FileBackedBigMatrix*>(R_ExternalPtrAddr(address));
  FileBackedBigMatrix *pfbbm = dynamic_cast<FileBackedBigMatrix*>(pMat);
  SEXP ret = Rf_protect(Rf_allocVector(LGLSXP,1));
  if (pfbbm)
  {
    LOGICAL(ret)[0] = pfbbm->flush() ? (Rboolean)TRUE : Rboolean(FALSE);
  }
  else
  {
    LOGICAL(ret)[0] = (Rboolean)FALSE;
    Rf_error("Object is not a filebacked big.matrix");
  }
  Rf_unprotect(1);
  return ret;
}

// [[Rcpp::export]]
SEXP IsShared( SEXP address )
{
  FileBackedBigMatrix *pMat =
    reinterpret_cast<FileBackedBigMatrix*>(R_ExternalPtrAddr(address));
  SEXP ret = Rf_protect(Rf_allocVector(LGLSXP,1));
  LOGICAL(ret)[0] = pMat->shared() ? (Rboolean)TRUE : Rboolean(FALSE);
  Rf_unprotect(1);
  return ret;
}

// [[Rcpp::export]]
SEXP isnil(SEXP address)
{
  void *ptr = R_ExternalPtrAddr(address);
  SEXP ret = Rf_protect(Rf_allocVector(LGLSXP,1));
  LOGICAL(ret)[0] = (ptr==NULL) ? (Rboolean)TRUE : Rboolean(FALSE);
  Rf_unprotect(1);
  return(ret);
}

// removed extern C because doesn't appear necessary
// Rcpp attributes can be used for R calls and the others
// are only used in the C code

// WHERE IS THIS CALLED FROM?  Maybe only from C, not from R?
// We might like to be able to do this recycling efficiently in other
// cases?  I thought we did.
void SetAllMatrixElements(SEXP bigMatAddr, SEXP value)
{
    Rcpp::XPtr<BigMatrix> pMat(bigMatAddr);

    if (pMat->separated_columns())
    {
        switch (pMat->matrix_type())
        {
          case 1:
            SetAllMatrixElements<char, SepMatrixAccessor<char> >(
              pMat, value, NA_CHAR, R_CHAR_MIN, R_CHAR_MAX, NA_REAL);
            break;
          case 2:
            SetAllMatrixElements<short, SepMatrixAccessor<short> >(
              pMat, value, NA_SHORT, R_SHORT_MIN, R_SHORT_MAX, NA_REAL);
            break;
          case 3:
            SetAllMatrixElements<unsigned char, SepMatrixAccessor<unsigned char> >(
              pMat, value, NA_BYTE, R_BYTE_MIN, R_BYTE_MAX, NA_REAL);
            break;
          case 4:
            SetAllMatrixElements<int, SepMatrixAccessor<int> >(
              pMat, value, NA_INTEGER, R_INT_MIN, R_INT_MAX, NA_REAL);
            break;
          case 6:
            SetAllMatrixElements<float, SepMatrixAccessor<float> >(
              pMat, value, NA_FLOAT, R_FLT_MIN, R_FLT_MAX, NA_REAL);
            break;
          case 8:
            SetAllMatrixElements<double, SepMatrixAccessor<double> >(
              pMat, value, NA_REAL, R_DOUBLE_MIN, R_DOUBLE_MAX, NA_REAL);
        }
    }
    else
    {
        switch (pMat->matrix_type())
        {
          case 1:
            SetAllMatrixElements<char, MatrixAccessor<char> >(
              pMat, value, NA_CHAR, R_CHAR_MIN, R_CHAR_MAX, NA_REAL);
            break;
          case 2:
            SetAllMatrixElements<short, MatrixAccessor<short> >(
              pMat, value, NA_SHORT, R_SHORT_MIN, R_SHORT_MAX, NA_REAL);
            break;
          case 3:
            SetAllMatrixElements<unsigned char, MatrixAccessor<unsigned char> >(
              pMat, value, NA_BYTE, R_BYTE_MIN, R_BYTE_MAX, NA_REAL);
            break;
          case 4:
            SetAllMatrixElements<int, MatrixAccessor<int> >(
              pMat, value, NA_INTEGER, R_INT_MIN, R_INT_MAX, NA_REAL);
            break;
          case 6:
            SetAllMatrixElements<float, MatrixAccessor<float> >(
              pMat, value, NA_FLOAT, R_FLT_MIN, R_FLT_MAX, NA_REAL);
            break;
          case 8:
            SetAllMatrixElements<double, MatrixAccessor<double> >(
              pMat, value, NA_REAL, R_DOUBLE_MIN, R_DOUBLE_MAX, NA_REAL);
        }
    }
}

// This doesn't appear to be used anywhere?!?!
void* GetDataPtr(SEXP address)
{
  SharedBigMatrix *pMat =
    reinterpret_cast<SharedBigMatrix*>(R_ExternalPtrAddr(address));
  return pMat->data_ptr();
}
