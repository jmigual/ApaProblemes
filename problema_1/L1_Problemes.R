set.seed(7)

N <- 50
total <- 1000
mu <- 10
sigma <- 20

mu.bias = 0
sigma.bias = 0

mulist = vector(mode="numeric", length = total)
sigmalist = vector(mode="numeric", length = total)
varmu = 0
varsigma = 0

for (i in 1:total)
{
  sample <- rnorm(N, mean = mu, sd = sigma)
  mulist[i] = mean(sample)
  sigmalist[i] = var(sample)
  
  sigma.bias <- sigma.bias + sigma - sigmalist[i]
  mu.bias <- mu.bias + mu - mulist[i]
}

mu.bias <- mu.bias/total
sigma.bias <- sigma.bias /total

mulist.mean <-  mean(mulist) 
sigmalist.mean <- mean(sigmalist)

for (i in 1:total)
{
  temp = mulist.mean - mulist[i]
  temp <- temp*temp
  varmu <- varmu + temp
  
  
  temp = sigmalist.mean - sigmalist[i]
  temp <- temp*temp
  varsigma <- varsigma + temp
}

varmu <- varmu / total
varsigma <- varsigma / total

cat(sprintf("Mean mu: %f\n", mean(mulist)))
cat(sprintf("Mean sigma^2: %f\n", mean(sigmalist)))

cat(sprintf("Bias mu: %f\n", mu.bias))
cat(sprintf("Bias sigma^2: %f\n", sigma.bias))

cat(sprintf("Variance mu: %f\n", varmu))
cat(sprintf("Variance sigma^2: %f\n", varsigma))