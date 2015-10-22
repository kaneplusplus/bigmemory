#ifndef BIGMEMORY_UTIL_HPP
#define BIGMEMORY_UTIL_HPP

#include "bigmemoryDefines.h"

using namespace std;

vector<string> RChar2StringVec( SEXP charVec );

vector<string> RChar2StringVec( SEXP charVec, 
  const vector<index_type> &indices );

SEXP String2RChar(const std::string &str);

std::string RChar2String(SEXP str);

SEXP StringVec2RChar( const vector<string> &strVec );

// Removed because no longer required with Rcpp
/*
template<typename T>
SEXP StringVec2RChar( const vector<string> &strVec,
  T indices, const index_type indicesLength )
{
  if (strVec.empty())
    return NULL_USER_OBJECT;
  SEXP ret = PROTECT(allocVector(STRSXP, indicesLength));
  index_type i;
  for (i=0; i < indicesLength; ++i)
  {
    SET_STRING_ELT(ret, i, 
      mkChar(strVec[static_cast<index_type>(indices[i])-1].c_str()));
  }
  UNPROTECT(1);
  return ret;
}
*/

template<typename T>
struct NewVec;

template<>
struct NewVec<int>
{SEXP operator()(index_type n) const {return Rcpp::IntegerVector(n);};};


inline
SEXP NEW_FLOAT(index_type n)
{
    std::vector<float> out (n, 0);
    return Rcpp::wrap(out);
}

template<>
struct NewVec<float>
{SEXP operator()(index_type n) const {return NEW_FLOAT(n);};};

template<>
struct NewVec<double>
{SEXP operator()(index_type n) const {return Rcpp::NumericVector(n);};};

template<typename T>
struct VecPtr;

template<>
struct VecPtr<int>
{int* operator()(SEXP vec) const {return INTEGER(vec);};};

template<>
struct VecPtr<float>
{float* operator()(SEXP vec) const {return (float *)(vec);};};

template<>
struct VecPtr<double>
{double* operator()(SEXP vec) const {return REAL(vec);};};

#endif // BIGMEMORY_UTIL_HPP
