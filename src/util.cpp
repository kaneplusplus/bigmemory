//#include <limits.h>
//#include <math.h>
#define STRICT_R_HEADERS
#include <Rcpp.h>
#include "bigmemory/util.h"

vector<string> RChar2StringVec( SEXP charVec )
{
    vector<string> ret = Rcpp::as<vector<string> >(charVec);
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

//SEXP StringVec2RChar( const vector<string> &strVec )
//{
//    if (strVec.empty())
//        return NULL_USER_OBJECT;
////    SEXP ret = PROTECT(allocVector(STRSXP, strVec.size()));
////    vector<string>::size_type i;
////    for (i=0; i < strVec.size(); ++i)
////    {
////        SET_STRING_ELT(ret, i, mkChar(strVec[i].c_str()));
////    }
//    
//    vector<string> ret(strVec.size());
//    vector<string>::size_type i;
//    for (i=0; i < strVec.size(); ++i)
//    {
//        ret[i] = strVec[i];
//    }
//    return Rcpp::wrap(ret);
//}

SEXP String2RChar(const std::string &str)
{
  return Rcpp::wrap(str);
}
