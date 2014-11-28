#ifndef BIGMEMORY_UTIL_HPP
#define BIGMEMORY_UTIL_HPP

#include <vector>
#include <string>
#include <Rdefines.h>

#include "bigmemoryDefines.h"

using namespace std;

vector<string> RChar2StringVec( SEXP charVec );

vector<string> RChar2StringVec( SEXP charVec, 
  const vector<index_type> &indices );

SEXP String2RChar(const std::string &str);

std::string RChar2String(SEXP str);

SEXP StringVec2RChar( const vector<string> &strVec );

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


template<typename T>
struct NewVec;

template<>
struct NewVec<int>
{SEXP operator()(index_type n) const {return NEW_INTEGER(n);};};

template<>
struct NewVec<double>
{SEXP operator()(index_type n) const {return NEW_NUMERIC(n);};};

template<typename T>
struct VecPtr;

template<>
struct VecPtr<int>
{int* operator()(SEXP vec) const {return INTEGER_DATA(vec);};};

template<>
struct VecPtr<double>
{double* operator()(SEXP vec) const {return NUMERIC_DATA(vec);};};

#endif // BIGMEMORY_UTIL_HPP
