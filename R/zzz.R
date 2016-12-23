.onLoad <- function(libname, pkgname) {
  options(bigmemory.print.warning=TRUE)
  options(bigmemory.typecast.warning=TRUE)
  options(bigmemory.allow.dimnames=FALSE)
  options(bigmemory.default.type="double")
  options(bigmemory.default.shared=TRUE)
}

.onUnload <- function(libpath) {
    options(bigmemory.print.warning=NULL)
    options(bigmemory.typecast.warning=NULL)
    options(bigmemory.allow.dimnames=NULL)
    options(bigmemory.default.type=NULL)
    options(bigmemory.default.shared=NULL)
}
