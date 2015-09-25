#ifndef BIG_MATRIX_ACCESSOR
#define BIG_MATRIX_ACCESSOR

#include "BigMatrix.h"

// The MatrixAccessor class allows the user to access non-separated
// big matrix data as matrix[i][j].
template<typename T>
class MatrixAccessor
{
  public:
    typedef T value_type;

  public:
    MatrixAccessor(T* pData, const index_type &nrow)
    {
      _pMat = pData;
      _totalRows = nrow;
      _rowOffset = 0;
      _colOffset = 0;
      _nrow = nrow;
    }
    
    MatrixAccessor(T* pData, const index_type &nrow, const index_type &ncol)
    {
      _pMat = pData;
      _totalRows = nrow;
      _totalCols = ncol;
      _rowOffset = 0;
      _colOffset = 0;
      _nrow = nrow;
      _ncol = ncol;
    }

    MatrixAccessor( BigMatrix &bm )
    {
      _pMat = reinterpret_cast<T*>(bm.matrix());
      _totalRows = bm.total_rows();
      _totalCols = bm.total_columns();
      _rowOffset = bm.row_offset();
      _colOffset = bm.col_offset();
      _nrow = bm.nrow();
      _ncol = bm.ncol();
    }

    inline T* operator[](const index_type &col) 
    {
      return _pMat + _totalRows * (col + _colOffset) + _rowOffset;
    }

    index_type nrow() const
    {
      return _nrow;
    }
    
    index_type ncol() const
    {
      return _ncol;
    }

  protected:
    T *_pMat;
    index_type _totalRows;
    index_type _totalCols;
    index_type _rowOffset;
    index_type _colOffset;
    index_type _nrow;
    index_type _ncol;
};

template<typename T>
class SepMatrixAccessor
{
  public:
    typedef T value_type;

  public:
    SepMatrixAccessor( BigMatrix &bm)
    {
      _ppMat = reinterpret_cast<T**>(bm.matrix());
      _rowOffset = bm.row_offset();
      _colOffset = bm.col_offset();
      _totalRows = bm.nrow();
      _totalCols = bm.ncol();
    }

    inline T* operator[](const index_type col) 
    {
      return _ppMat[col + _colOffset] + _rowOffset;
    }

    index_type nrow() const
    {
      return _totalRows;
    }
    
    index_type ncol() const
    {
      return _totalCols;
    }

  protected:
    T **_ppMat;
    index_type _rowOffset;
    index_type _colOffset;
    index_type _totalRows;
    index_type _totalCols;
};

#endif //BIG_MATRIX_ACCESSOR
