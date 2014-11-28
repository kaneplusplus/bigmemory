#ifndef _SHARED_COUNTER_H
#define _SHARED_COUNTER_H

#include <string>
#include <boost/interprocess/mapped_region.hpp>

#include "bigmemoryDefines.h"

// Note: Shared Counters are not mutex protected.
class SharedCounter
{
  public:
    SharedCounter(): _pVal(NULL),_pRegion(NULL){};
    ~SharedCounter(){reset();};

    bool init( const std::string &resourceName);
  public:
    index_type get() const;
    bool reset();
  private:
    index_type *_pVal;  
    boost::interprocess::mapped_region *_pRegion;
    std::string _resourceName;
};
#endif //_SHARED_COUNTER_H
