bigmemory
=========

Create, store, access, and manipulate massive matrices.  Matrices are, by default, allocated to shared memory and may use memory-mapped files. Packages ‘biganalytics’, ‘synchronicity’, ‘bigalgebra’, and ‘bigtabulate’ provide advanced functionality. Access to and manipulation of a ‘big.matrix’ object is exposed in by an S4 class whose interface is simlar to that of an ‘matrix’. Use of these packages in parallel environments can provide substantial speed and memory efficiencies.  ‘bigmemory’ also provides a C++ framework for the development of new tools that can work both with ‘big.matrix’ and native ‘matrix’ objects.

```{R}
x <- big.matrix(5, 2, type="integer", init=0,
                dimnames=list(NULL, c("alpha", "beta")))
x
x[1:2,]
x[,1] <- 1:5
x[,"alpha"]
colnames(x)
options(bigmemory.allow.dimnames=TRUE)
colnames(x) <- NULL
x[,]
```

### Wish List
1. big.data.frame functionality
2. direct access to a RDBMS (Oracle, Postgresql, etc.)
3. Additional big.matrix subclasses (e.g. sparse, positive-definite, etc.) 
    -- see bigalgrebra package
