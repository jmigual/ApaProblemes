rm(list=ls())

library(nnet)

# Llegim l'arxiu letters.txt

tot <- read.table(file.choose())
lletres = tot[1:35]

# La funció 'canviaLletra' agafa 'una 'lletra' i li canvia alguns bits a l'atzar

canviaLletra <- function(lletra) {
  lletra <- as.integer(lletra[1:35])
  index = c(sample.int(35,rpois(1, 1.01)))
  if (length(index) == 0) return(lletra)
  lletra[index] = 1 - lletra[index]
  lletra
}

# La funció 'sobrePP' genera 'N' dades corruptes de les dades 'tot'

sobrePP <- function(N, tot) {
  data <- tot[sample.int(26,N,replace=T),]
  data[,1:35] <- t(apply(data, 1,canviaLletra))
  rownames(data) <- NULL
  data
}

dades <- sobrePP(1000,tot)
test <- sobrePP(500,tot)
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

# Mirem quins són els millors paràmetres per la nostra xarxa (per cross-validation i amb regularització)

library(lattice)
library(ggplot2)
library(caret)
library(e1071)

# Fem una llista amb posibles 'decays'
(decays <- 10^seq(-3,0,by=0.1))
trc <- trainControl (method="repeatedcv", number=10, repeats=10)
model.10x10CV <- train (V36 ~., data = dades, method='nnet', maxit = 200, trace = FALSE,
                        tuneGrid = expand.grid(.size=10,.decay=decays), trControl=trc)

## We can inspect the full results
model.10x10CV$results

## and the best model found
model.10x10CV$bestTune

save(model.10x10CV, file = "model.10x10CV.regul")

load ("model.10x10CV.regul")