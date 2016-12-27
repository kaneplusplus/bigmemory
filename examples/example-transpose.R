N <- 7
M <- 11
X <- big.matrix(N, M, dimnames = list(letters[1:N], LETTERS[1:M]))
X[] <- rnorm(length(X))

Xt <- transpose(X)
print(Xt[,])

print(identical(
  t(X[,]),
  Xt[,]
))
