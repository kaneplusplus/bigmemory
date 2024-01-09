#include <string>
#include <fstream>
#include <sstream>
#include <algorithm>

#include <boost/lexical_cast.hpp>
#define STRICT_R_HEADERS
#include <Rcpp.h>

#include "bigmemory/BigMatrix.h"
#include "bigmemory/MatrixAccessor.hpp"
#include "bigmemory/isna.hpp"

template<typename in_CType, typename in_BMAccessorType, 
  typename out_CType, typename out_BMAccessorType>
void DeepCopy(BigMatrix *pInMat, BigMatrix *pOutMat, SEXP rowInds, SEXP colInds)
{
  in_BMAccessorType inMat( *pInMat );
  out_BMAccessorType outMat( *pOutMat );
  
  double *pRows = REAL(rowInds);
  double *pCols = REAL(colInds);
  index_type nRows = Rf_length(rowInds);
  index_type nCols = Rf_length(colInds);
  
  if (nRows != pOutMat->nrow())
    Rf_error("length of row indices does not equal # of rows in new matrix");
  if (nCols != pOutMat->ncol())
    Rf_error("length of col indices does not equal # of cols in new matrix");
  
  index_type i = 0;
  index_type j = 0;
  in_CType *pInColumn;
  out_CType *pOutColumn;
  
  for (i = 0; i < nCols; ++i) {
    pInColumn = inMat[static_cast<index_type>(pCols[i])-1];
    pOutColumn = outMat[i];
    for (j = 0; j < nRows; ++j) {
      pOutColumn[j] = static_cast<out_CType>(
        pInColumn[static_cast<index_type>(pRows[j])-1]);
    }
  }
  
  return;
}


// [[Rcpp::export]]
SEXP CDeepCopy(SEXP inAddr, SEXP outAddr, SEXP rowInds, SEXP colInds, 
    SEXP typecast_warning)
  {
      
    #define CALL_DEEP_COPY_2(IN_CTYPE, IN_ACCESSOR, OUT_ACCESSOR) \
    switch(pOutMat->matrix_type()) \
    { \
      case 1: \
        DeepCopy<IN_CTYPE, IN_ACCESSOR<IN_CTYPE>, char, OUT_ACCESSOR<char> >( \
          pInMat, pOutMat, rowInds, colInds); \
        break; \
      case 2: \
        DeepCopy<IN_CTYPE, IN_ACCESSOR<IN_CTYPE>, short, OUT_ACCESSOR<short> >( \
          pInMat, pOutMat, rowInds, colInds); \
        break; \
	  case 3:                                                                \
	    DeepCopy<IN_CTYPE, IN_ACCESSOR<IN_CTYPE>, unsigned char, OUT_ACCESSOR<unsigned char> >( \
	      pInMat, pOutMat, rowInds, colInds);                                 \
	    break;                                                                \
	  case 4:                                                                \
        DeepCopy<IN_CTYPE, IN_ACCESSOR<IN_CTYPE>, int, OUT_ACCESSOR<int> >( \
          pInMat, pOutMat, rowInds, colInds); \
        break; \
      case 8: \
        DeepCopy<IN_CTYPE, IN_ACCESSOR<IN_CTYPE>, double, OUT_ACCESSOR<double> >( \
          pInMat, pOutMat, rowInds, colInds); \
        break; \
    }

    #define CALL_DEEP_COPY_1(IN_ACCESSOR, OUT_ACCESSOR) \
    switch(pInMat->matrix_type()) \
    { \
      case 1: \
        CALL_DEEP_COPY_2(char, IN_ACCESSOR, OUT_ACCESSOR) \
        break; \
      case 2: \
        CALL_DEEP_COPY_2(short, IN_ACCESSOR, OUT_ACCESSOR) \
        break; \
	  case 3:                                            \
	    CALL_DEEP_COPY_2(unsigned char, IN_ACCESSOR, OUT_ACCESSOR) \
	    break;                                            \
	  case 4:                                            \
        CALL_DEEP_COPY_2(int, IN_ACCESSOR, OUT_ACCESSOR) \
        break; \
      case 8: \
        CALL_DEEP_COPY_2(double, IN_ACCESSOR, OUT_ACCESSOR) \
        break; \
    }
      
    BigMatrix *pInMat = reinterpret_cast<BigMatrix*>(
      R_ExternalPtrAddr(inAddr));
    BigMatrix *pOutMat = reinterpret_cast<BigMatrix*>(
      R_ExternalPtrAddr(outAddr));
    
    if ((pOutMat->matrix_type() < pInMat->matrix_type()) &&
      (Rf_asLogical(typecast_warning) == (Rboolean)TRUE))
    {
      string type_names[9] = {
        "", "char", "short", "raw", "integer", "", "", "", "double"};
      
      std::string warnMsg = string("Assignment will down cast from ") + 
        type_names[pInMat->matrix_type()] + string(" to ") + 
        type_names[pOutMat->matrix_type()] + string("\n") + 
        string("Hint: To remove this warning type: ") + 
        string("options(bigmemory.typecast.warning=FALSE)");
      Rf_warning("%s", warnMsg.c_str());
    }
    
    // Not sure if there is a better way to do these function calls
    if (pInMat->separated_columns() && pOutMat->separated_columns()) {
      CALL_DEEP_COPY_1(SepMatrixAccessor, SepMatrixAccessor)
    }
    else if(pInMat->separated_columns() && !(pOutMat->separated_columns()))
    {
      CALL_DEEP_COPY_1(SepMatrixAccessor, MatrixAccessor)
    }
    else if(!(pInMat->separated_columns()) && pOutMat->separated_columns())
    {
      CALL_DEEP_COPY_1(MatrixAccessor, SepMatrixAccessor)
    }
    else
    {
      CALL_DEEP_COPY_1(MatrixAccessor, MatrixAccessor)
    }

    return R_NilValue;
  }
