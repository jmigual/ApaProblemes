library(mlbench)
N.train <- 1000
N.test <- 10000

p <- mlbench.smiley(n = N.test,sd1=0.5, sd2 = 0.5)
plot(p)

test <- as.data.frame(p)

load(file.choose())

library(kernlab)

linear <- ksvm(classes ~., data = smiley, kernel = "vanilladot", C = 6, cross = 10)

cList <- c(0.01,0.1,1,10,100,1000)

polinomKsvm = function(C, kernel, degree) {
  auxCV = 0
  for (i in 1:10){
    linear <- ksvm(classes ~., data = smiley, kernel = kernel, degree = degree, C = C, cross = 10)
    auxCV <- auxCV + linear@cross
  }
  auxCV <- auxCV/10
  return(auxCV)
}

minCV <- Inf
minC <- Inf
for (C in cList) {
  auxCV = polinomKsvm(C, "vanilladot", 1)
  if(auxCV < minCV) { 
    minCV = auxCV  
    minC = C
  }
}
minCV
minC

###################### QUADRATIC SVM #########################################
minCV2 <- Inf
minC2 <- Inf
for (C in cList) {
  polinomKsvm(C, "polydot", 2)
  if(auxCV < minCV2) { 
    minCV2 = auxCV  
    minC2 = C
  }
}
minCV2
minC2



###################### CUBIC SVM #########################################
minCV3 <- Inf
minC3 <- Inf
for (C in cList) {
  polinomKsvm(C, "polydot", 3)
  if(auxCV < minCV3) { 
    minCV3 = auxCV  
    minC3 = C
  }
}
minCV3
minC3


###################### RBF SVM #########################################
minCV4 <- Inf
minC4 <- Inf
minS4 <- Inf

sigmalist <- c(1.3, 1.4, 1.5, 1.6, 1.7, 1.8)
for (s in sigmalist) {
  print("Sigma")
  print(s)
  for (C in cList) {
    print(C)
    auxCV = 0
    for (i in 1:10){
      linear <- ksvm(classes ~., data = smiley, kernel = "rbfdot", C = C, cross = 10, sigma = s)
      auxCV <- auxCV + linear@cross
    }
    auxCV <- auxCV/10
    print(auxCV)
    if(auxCV < minCV4) { 
      minCV4 = auxCV  
      minC4 = C
      minS4 = s
    }
  }
}
minCV4
minC4
minS4

model = ksvm(classes ~., data = smiley, kernel = "rbfdot", C = minC4, sigma = minS4)

result = predict(model)

100*(1 - sum(diag(table(result, smiley$classes)))/length(result))

result.test = predict(model, test)
table(result.test, test$classes)
100*(1 - sum(diag(table(result.test, test$classes)))/length(result.test))

par(mfrow=c(1,2))
plot(x = test$x.x4, y = test$x.V2, col = result.test)
plot(x = test$x.x4, y = test$x.V2, col = test$classes)
par(mfrow=c(1,1))