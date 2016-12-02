rm(list=ls())

library(nnet)

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

dades <- sobrePP(10000,tot)
test <- sobrePP(5000,tot)
model.nnet <- nnet(V36 ~., data = dades, size=10, maxit=200, decay=0.01)

p1 <- as.factor(predict (model.nnet, type="class"))
levels(p1) <- LETTERS
t1 <- table(p1,dades$V36)
error_rate.learn <- 100*(1-sum(diag(t1))/nrow(dades))
error_rate.learn

p2 <- as.factor(predict (model.nnet,newdata = test,type="class"))
levels(p2) <- LETTERS
t2 <- table(p2,test$V36)
error_rate.test <- 100*(1-sum(diag(t2))/nrow(test))
error_rate.test