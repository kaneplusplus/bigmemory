#include <limits.h>
#include <math.h>
#include "bigmemory/util.h"

vector<string> RChar2StringVec( SEXP charVec )
{
  vector<string> ret( GET_LENGTH(charVec) );
  vector<string>::size_type i;
  for (i=0; i < ret.size(); ++i)
  {
    ret[i] = string(CHAR(STRING_ELT(charVec, i)));
  }
  return ret;
}

vector<string> RChar2StringVec( SEXP charVec, 
  const vector<index_type> &indices )
{
  vector<string> ret( indices.size() );
  vector<string>::size_type i;
  for (i=0; i < indices.size(); ++i)
  {
    ret[i] = string(CHAR(STRING_ELT(charVec, indices[i]-1)));
  }
  return ret;
}

std::string RChar2String(SEXP str)
{
  return string(CHAR(STRING_ELT(str, 0)));
}

SEXP StringVec2RChar( const vector<string> &strVec )
{
  if (strVec.empty())
    return NULL_USER_OBJECT;
  SEXP ret = PROTECT(allocVector(STRSXP, strVec.size()));
  vector<string>::size_type i;
  for (i=0; i < strVec.size(); ++i)
  {
    SET_STRING_ELT(ret, i, mkChar(strVec[i].c_str()));
  }
  UNPROTECT(1);
  return ret;
}

SEXP String2RChar(const std::string &str)
{
  SEXP ret = PROTECT(allocVector(STRSXP, 1));
  SET_STRING_ELT(ret, 0, mkChar(str.c_str()));
  UNPROTECT(1);
  return ret;
}
