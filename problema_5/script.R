rm(list=ls())

tot <- read.table(file.choose())
lletres = tot[1:35]

canviaLletra <- function(lletra) {
  lletra <- as.integer(lletra[1:35])
  index = c(sample.int(35,rpois(1, 1.01)))
  if (length(index) == 0) return(lletra)
  lletra[index] = 1 - lletra[index]
  lletra
}

sobrePP <- function(N, tot) {
  data <- tot[sample.int(26,N,replace=T),]
  data[,1:35] <- t(apply(data, 1,canviaLletra))
  rownames(data) <- NULL
  data
}