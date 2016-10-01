# Afegim seed perquè els resultats siguin sempre els mateixos
set.seed(7)

# Definim les variables necessàries per inicialitzar el problema
N <- 50
total <- 1000
mu <- 10
sigma <- 20

# Inicialitzem el bias de mu i de sigma
mu.bias = 0
sigma.bias = 0

# Inicialitzem la variància de mu i de sigma
varmu = 0
varsigma = 0

# Inicalitzem els vectors que contindran la mu i la sigma de cadascun dels
# conjunts
mulist = vector(mode="numeric", length = total)
sigmalist = vector(mode="numeric", length = total)

for (i in 1:total)
{
  # Obtenim el conjunt i en calculem la mu i la sigma
  mostra <- rnorm(N, mean = mu, sd = sigma)
  mulist[i] = mean(mostra)
  sigmalist[i] = var(mostra)
  
  # Calculem incrementalment el bias de mu i sigma
  sigma.bias <- sigma.bias + sigma - sigmalist[i]
  mu.bias <- mu.bias + mu - mulist[i]
}

# Finalitzem el càlcul de bias de mu i sigma
mu.bias <- mu.bias/total
sigma.bias <- sigma.bias /total

# Per evitar calcular aquests valors tota l'estona
mulist.mean <-  mean(mulist) 
sigmalist.mean <- mean(sigmalist)


for (i in 1:total)
{
  # Calculem incrementalment la variància de mu
  temp = mulist.mean - mulist[i]
  temp <- temp*temp
  varmu <- varmu + temp
  
  # Calculem incrementalment la variància de sigma
  temp = sigmalist.mean - sigmalist[i]
  temp <- temp*temp
  varsigma <- varsigma + temp
}

# Finalitzem el càlcul de la variància de mu i sigma
varmu <- varmu / total
varsigma <- varsigma / total

# Imprimim els resultats per pantalla
cat(sprintf("Mean mu: %f\n", mean(mulist)))
cat(sprintf("Mean sigma^2: %f\n", mean(sigmalist)))

cat(sprintf("Bias mu: %f\n", mu.bias))
cat(sprintf("Bias sigma^2: %f\n", sigma.bias))

cat(sprintf("Variance mu: %f\n", varmu))
cat(sprintf("Variance sigma^2: %f\n", varsigma))