# Without specifying the type, this big.matrix x will hold integers.

x <- as.big.matrix(matrix(1:10, 5, 2))
x[2,2] <- NA
x[,]
write.big.matrix(x, "foo.txt")

# Just for fun, I'll read it back in as character (1-byte integers):
y <- read.big.matrix("foo.txt", type="char")
y[,]

# Other examples:
w <- as.big.matrix(matrix(1:10, 5, 2), type='double')
w[1,2] <- NA
w[2,2] <- -Inf
w[3,2] <- Inf
w[4,2] <- NaN
w[,]
write.big.matrix(w, "bar.txt")
w <- read.big.matrix("bar.txt", type="double")
w[,]
w <- read.big.matrix("bar.txt", type="short")
w[,]

# Another example using row names (which we don't like).
x <- as.big.matrix(as.matrix(iris), type='double')
rownames(x) <- as.character(1:nrow(x))
head(x)
write.big.matrix(x, 'IrisData.txt', col.names=TRUE, row.names=TRUE)
y <- read.big.matrix("IrisData.txt", header=TRUE, has.row.names=TRUE)
head(y)

# The following would fail with a dimension mismatch:
if (FALSE) y <- read.big.matrix("IrisData.txt", header=TRUE)
