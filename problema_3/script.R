
X <- matrix(c(c(1,1,1,1,1),cos(c(48,67,83,108,126))),ncol=2)

(X)

t <- 1/(c(2.7,2.00,1.61,1.2,1.02))

(t)

X2 <- solve(t(X) %*% X)  #(X^T * X)^-1

(X2)

X3 <- X2 %*% t(X) # ((X^T * X)^-1) * X^T  (pseudo)

(X3 %*% X) # (IDENTITAT)

w <- X3 %*% t

p <- 1/w[1,1]

e <- p  * w[2,1]

(p)

(e)

plot(X[,2],t)

lines (X[,2], w[2,1]*X[,2]+w[1,1], type="l")