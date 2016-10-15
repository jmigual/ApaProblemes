# Netegem totes les variàbles anteriors
rm(list=ls())

# Útil per imprimir paràmetres per pantalla
printf <- function(...) { cat(sprintf(...), "\n") }

within_scatter <- function(mat, m) {
  # S_k = SUM(n)[(x_n - m_m)(x_n - m_-)T
  # Inicialitzem la matriu S_k
  Sk <- matrix(0, nrow(mat), nrow(mat))
  for (i in 1:ncol(mat)) {
    vaux = mat[,i] - m
    Sk <- Sk + vaux %*% t(vaux)
  }
  return(Sk)
}

# Afegim les dades del problema per columnes
v1 <- matrix(c(c(4, 1), c(2, 4), c(2, 3), c(3, 6), c(4, 4)), nrow=2)
v2 <- matrix(c(c(9, 10), c(6, 8), c(9, 5), c(8, 7), c(10, 8)), nrow=2)

# Apartat 1 Calcular m1 i m2
m1 <- matrix(c(mean(v1[1,]), mean(v1[2,])))
m2 <- matrix(c(mean(v2[1,]), mean(v2[2,])))

printf("m1")
print(m1)

printf("m2")
print(m2)

# Apartat 2 Calcular S1, S2 i SW
S1 <- within_scatter(v1, m1)
S2 <- within_scatter(v2, m2)
SW <- S1 + S2
printf("S1:")
print(S1)
printf("S2:")
print(S2)
printf("SW:")
print(SW)

# Apartat 3 Calcular SB
m21 <- m2 - m1
SB <- m21 %*% t(m21)
printf("SB:")
print(SB) 

# Apartat 4a Calcular w*
wstar <- solve(SW) %*% (m1 - m2)
printf("W*:")
print(wstar)

# Apartat 4b Calcular w*
result <- eigen(solve(SW) %*% SB)$vectors
print(result)

#Apartat 5, representar les dades i w*
N <- ncol(v1)
par(mfrow=c(1,1))
plot(c(v1[1,], v2[1,]), c(v1[2,],v2[2,]),col=c(rep('red',N),rep('green',N)),xlab='',ylab='')
abline(-3,wstar[1]/wstar[2],col='black',lwd=2)