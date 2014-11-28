#include <boost/interprocess/shared_memory_object.hpp>
#include "bigmemory/SharedCounter.h"

bool SharedCounter::reset()
{
  if (_pVal)
  {
    --(*_pVal);
    if (get() == 0)
    {
      boost::interprocess::shared_memory_object::remove(_resourceName.c_str());
      _resourceName="";
    }
    delete _pRegion;
  }
  _pVal = NULL;
  _resourceName = "";
  return true;
}

bool SharedCounter::init( const std::string &resourceName )
{
  _resourceName = resourceName;
  // See if we are connecting for the first time.
  try
  {  
    boost::interprocess::shared_memory_object shm(
      boost::interprocess::create_only,
      _resourceName.c_str(), 
      boost::interprocess::read_write);
    // It's a new counter.
    shm.truncate( sizeof(index_type) );
    _pRegion = new boost::interprocess::mapped_region(shm, 
      boost::interprocess::read_write);
    _pVal = reinterpret_cast<index_type*>(_pRegion->get_address());
    *_pVal = 1;
  }
  catch(std::exception &ex)
  {
    // We are connecting to an existing counter.
    boost::interprocess::shared_memory_object shm(
      boost::interprocess::open_only,
      _resourceName.c_str(), 
      boost::interprocess::read_write);
    _pRegion = new boost::interprocess::mapped_region(shm, 
      boost::interprocess::read_write);
    _pVal = reinterpret_cast<index_type*>(_pRegion->get_address());
    ++(*_pVal);
  }
  return true;
}

index_type SharedCounter::get() const
{
  return _pVal == NULL ? 0 : *_pVal;
}

