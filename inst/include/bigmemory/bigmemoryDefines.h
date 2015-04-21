
#ifndef BIGMEMORY_DEFINES_H
#define BIGMEMORY_DEFINES_H

#include <limits.h>
#include <stdint.h>

#if __STDC_VERSION__ >= 199901L
  #define HAVE_COMPLEX
  #include <complex.h>
#else
  #undef HAVE_COMPLEX
#endif 

#ifdef __intptr_t_defined
  typedef intptr_t index_type;
#else
  // We don't have 64-bit indexing.  Build with 32.
  typedef long index_type;
#endif

extern "C"
{
  #include <Rdefines.h>
}

#define NA_CHAR CHAR_MIN
#define NA_SHORT SHRT_MIN
#define R_INT_MIN (1+INT_MIN)
#define R_INT_MAX INT_MAX
#define R_SHORT_MIN (1+SHRT_MIN)
#define R_SHORT_MAX SHRT_MAX
#define R_CHAR_MIN (1+CHAR_MIN)
#define R_CHAR_MAX CHAR_MAX
#define R_DOUBLE_MIN R_NegInf
#define R_DOUBLE_MAX R_PosInf

#define NA_FLOAT FLT_MIN
#define R_FLT_MIN -FLT_MAX
#define R_FLT_MAX FLT_MAX

#ifdef HAVE_LONG_DOUBLE
# define LDOUBLE long double
#else
# define LDOUBLE double
#endif

#endif //BIGMEMORY_DEFINES_H
