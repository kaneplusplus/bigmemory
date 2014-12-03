.onLoad <- function(libname, pkgname) {
  library.dynam("bigmemory", pkgname, libname)
  options(bigmemory.print.warning=TRUE)
  options(bigmemory.typecast.warning=TRUE)
  options(bigmemory.allow.dimnames=FALSE)
  options(bigmemory.default.type="double")
}

.onUnload <- function(libpath) {
    library.dynam.unload("bigmemory", libpath);
    options(bigmemory.print.warning=NULL)
    options(bigmemory.typecast.warning=NULL)
    options(bigmemory.allow.dimnames=NULL)
    options(bigmemory.default.type=NULL)
}
