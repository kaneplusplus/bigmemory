// proposed new names, InMemorySharedBigMatrix, FileBackedSharedBigMatrix
#ifndef BIGMATRIX_H
#define BIGMATRIX_H

#ifdef INT
#undef INT
#endif

#include <boost/interprocess/mapped_region.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/noncopyable.hpp>
#include <string>
#include <vector>

#include "bigmemoryDefines.h"
#include "SharedCounter.h"

using namespace std;

typedef vector<std::string> Names;
typedef boost::interprocess::mapped_region MappedRegion;
typedef boost::shared_ptr<MappedRegion> MappedRegionPtr;
typedef vector<MappedRegionPtr> MappedRegionPtrs;
typedef vector<index_type> Columns;

class BigMatrix : public boost::noncopyable
{
  // Public types
  public:
    enum MatrixType {CHAR=1, SHORT=2, INT=3, DOUBLE=4, COMPLEX=5, FLOAT=6};

  // Constructor and Destructor
  public:
    BigMatrix():_ncol(0),_nrow(0), _totalRows(0), _totalCols(0),
                _colOffset(0), _rowOffset(0),_matType(0), _pdata(NULL),
                _sepCols(false), _readOnly(false), _allocationSize(0){}
    virtual ~BigMatrix(){}

    // The next function returns the matrix data.  It will generally be passed
    // to an appropriate templated function. 
    void* matrix() {return _pdata;}
    
    // Accessors
    index_type ncol() const {return _ncol;}
    index_type nrow() const {return _nrow;}
   
    // For a submatrix, total_* includes the rows in the supermatrix.
    index_type total_rows() const {return _totalRows;}
    index_type total_columns() const {return _totalCols;}
    index_type col_offset() const {return _colOffset;}
    index_type row_offset() const {return _rowOffset;}
    int matrix_type() const {return _matType;}
    bool shared() const {return _shared;}
    bool separated_columns() const {return _sepCols;}
    Names column_names() 
    {
      Names ret;
      if (!_colNames.empty())
      {
        std::copy( _colNames.begin()+col_offset(), 
                  _colNames.begin()+col_offset()+ncol(),
                  std::back_inserter(ret) );
      }
      return ret;
    }

    Names row_names() 
    {
      Names ret;
      if (!_rowNames.empty())
      {
        ret.reserve(nrow());
        std::copy( _rowNames.begin() + row_offset(), 
                  _rowNames.begin() + row_offset() + nrow(),
                  std::back_inserter(ret) );
      }
      return ret;
    }

    bool is_submatrix()
    {
      return total_rows() != nrow() || total_columns() != ncol();
    }

    // Mutators
    bool column_names( const Names &newColNames )
    {
      if ( !is_submatrix() && 
        ((newColNames.size() == static_cast<Names::size_type>(ncol())) ||
        newColNames.size() == 0) )
      {
        _colNames = newColNames; 
        return true;
      }
      if ( is_submatrix() && 
        (newColNames.size() == static_cast<Names::size_type>(ncol())) )
      {
        std::copy( newColNames.begin(), newColNames.end(),
          _colNames.begin() + col_offset());
        return true;
      }
      return false;
    }

    bool row_names( const Names &newRowNames )
    {
      if ( !is_submatrix() && 
        ((newRowNames.size() == static_cast<Names::size_type>(nrow())) ||
        newRowNames.size() == 0) )
      {
        _rowNames = newRowNames; 
        return true;
      }
      if ( is_submatrix() && 
        (newRowNames.size() == static_cast<Names::size_type>(nrow())) )
      {
        std::copy( newRowNames.begin(), newRowNames.end(),
          _rowNames.begin() + row_offset());
        return true;
      }
      return false;
    }
  
    bool col_offset( const index_type &newOffset ) 
    {
      _colOffset=newOffset;
      return true;
    }
    
    bool row_offset( const index_type &newOffset )
    {
      _rowOffset=newOffset;
      return true;
    }
    
    bool ncol( const index_type &newNumCols )
    {
      _ncol=newNumCols;
      return true;
    }
    
    bool nrow( const index_type &newNumCols )
    {
      _nrow=newNumCols;
      return true;
    }
    
    const bool read_only() const
    {
      return _readOnly;
    }

    void read_only(bool newReadOnly)
    {
      _readOnly = newReadOnly;
    }
  
    void* data_ptr() {return _pdata;}
  
    const index_type allocation_size() const {return _allocationSize;}

  // Data Members

  protected:
    index_type _ncol;
    index_type _nrow;
    index_type _totalRows;
    index_type _totalCols;
    index_type _colOffset;
    index_type _rowOffset;
    index_type _nebytes;
    int _matType;
    void* _pdata;
    bool _shared;
    bool _sepCols;
    Names _colNames;
    Names _rowNames;
    bool _readOnly;
    index_type _allocationSize;
};

class LocalBigMatrix : public BigMatrix
{
  public:
    LocalBigMatrix() : BigMatrix() {_shared=false;}
    virtual ~LocalBigMatrix() {destroy();};
    virtual bool create( const index_type numRow, const index_type numCol,
      const int matrixType, const bool sepCols);

  protected:
    virtual bool destroy();
};
 
class SharedBigMatrix : public BigMatrix
{
  public:
    SharedBigMatrix() : BigMatrix() {_shared=true;}
    virtual ~SharedBigMatrix() {}
    std::string uuid() const {return _uuid;}
    std::string shared_name() const {return _sharedName;}

  protected:
    virtual bool destroy()=0;

  protected:
    // According to the documentation, shared memory has kernel or 
    // filesystem presistence (mechanism exists until the system reboots
    // or is deleted (kernel) or until the mechanism is explicitly deleted
    // (filesystem)).  As a result, we are going to need a usage counter
    // so that when the last object is done with the shared resource, it
    // can delete the resource.  The destructor will handle deletion
    // of the shared usage counter.
    bool create_uuid();
    bool uuid(const std::string &uuid) {_uuid=uuid; return true;}

  protected:
    std::string _uuid;
    std::string _sharedName;
    MappedRegionPtrs _dataRegionPtrs;
};

class SharedMemoryBigMatrix : public SharedBigMatrix
{
  public:
    SharedMemoryBigMatrix():SharedBigMatrix(){};
    virtual ~SharedMemoryBigMatrix(){destroy();};
    virtual bool create( const index_type numRow, const index_type numCol, 
      const int matrixType, const bool sepCols);
    virtual bool connect( const std::string &uuid, const index_type numRow, 
      const index_type numCol, const int matrixType,
      const bool sepCols, const bool readOnly=false);

  protected:
    virtual bool destroy();

  protected:
    SharedCounter _counter;
}; 

class FileBackedBigMatrix : public SharedBigMatrix
{
  // _sharedName is filename_uuid
  public:
    FileBackedBigMatrix():SharedBigMatrix(){}
    virtual ~FileBackedBigMatrix(){destroy();}
    virtual bool create( const std::string &fileName, 
      const std::string &filePath,const index_type numRow, 
      const index_type numCol, const int matrixType, const bool sepCols);
    virtual bool connect( const std::string &fileName, 
      const std::string &filePath, const index_type numRow, 
      const index_type numCol, const int matrixType, const bool sepCols,
      const bool readOnly=false);
    std::string file_name() const {return _fileName;}
    std::string file_path() const {return _filePath;}
    bool flush();
  protected:
    virtual bool destroy();

  protected:
    std::string _fileName, _filePath;
};

#endif // BIGMATRIX_H
