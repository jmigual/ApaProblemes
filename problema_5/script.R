rm(list=ls())

tot <- read.table(file.choose())
lletres = tot[1:35]

canviaLletra <- function(lletra) {
  index = c(floor(runif(rpois(1, 1.01), 1, 35)))
  if (length(index) == 0) return(lletra)
  
  lletra[index] = 1 - lletra[index]
  lletra
}

sobrePP <- function(N, tot) {
  data = tot[c(floor(runif(N, 1, 26))),]
  lapply(data, canviaLletra)
}