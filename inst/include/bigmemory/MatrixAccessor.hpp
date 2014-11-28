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

    MatrixAccessor( BigMatrix &bm )
    {
      _pMat = reinterpret_cast<T*>(bm.matrix());
      _totalRows = bm.total_rows();
      _rowOffset = bm.row_offset();
      _colOffset = bm.col_offset();
      _nrow = bm.nrow();
    }

    inline T* operator[](const index_type &col) 
    {
      return _pMat + _totalRows * (col + _colOffset) + _rowOffset;
    }

    index_type nrow() const
    {
      return _nrow;
    }

  protected:
    T *_pMat;
    index_type _totalRows;
    index_type _rowOffset;
    index_type _colOffset;
    index_type _nrow;
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
    }

    inline T* operator[](const index_type col) 
    {
      return _ppMat[col + _colOffset] + _rowOffset;
    }

    index_type nrow() const
    {
      return _nrow;
    }

  protected:
    T **_ppMat;
    index_type _rowOffset;
    index_type _colOffset;
    index_type _totalRows;
    index_type _nrow;
};

#endif //BIG_MATRIX_ACCESSOR
