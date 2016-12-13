library(mlbench)
N.train <- 1000
N.test <- 10000

p <- mlbench.smiley(n = N.test,sd1=0.5, sd2 = 0.5)
plot(p)

test <- as.data.frame(p)
save(test,file = "/home2/users/alumnes/1181492/dades/APA/ApaProblemes/problema_6/testData.Rdata")

load("/home2/users/alumnes/1181492/dades/APA/ApaProblemes/problema_6/trainData.Rdata")

library(kernlab)

linear <- ksvm(classes ~., data = smiley, kernel = "vanilladot", C = 6, cross = 10)

cList <- c(0.01,0.1,1,10,100,1000)

minCV <- Inf
minC <- Inf
for (C in cList) {
  auxCV = 0
  for (i in 1:10){
    linear <- ksvm(classes ~., data = smiley, kernel = "vanilladot", C = C, cross = 10)
    auxCV <- auxCV + linear@cross
  }
  auxCV <- auxCV/10
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
  auxCV = 0
  for (i in 1:10){
    linear <- ksvm(classes ~., data = smiley, kernel = "polydot", C = C, cross = 10,degree= 2)
    auxCV <- auxCV + linear@cross
  }
  auxCV <- auxCV/10
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
  auxCV = 0
  for (i in 1:10){
    linear <- ksvm(classes ~., data = smiley, kernel = "polydot", C = C, cross = 10,degree = 3)
    auxCV <- auxCV + linear@cross
  }
  auxCV <- auxCV/10
  if(auxCV < minCV3) { 
    minCV3 = auxCV  
    minC3 = C
  }
}
minCV3
minC3


###################### RBF SVM #########################################
minCV3 <- Inf
minC3 <- Inf
for (C in cList) {
  auxCV = 0
  for (i in 1:10){
    linear <- ksvm(classes ~., data = smiley, kernel = "polydot", C = C, cross = 10,degree = 3)
    auxCV <- auxCV + linear@cross
  }
  auxCV <- auxCV/10
  if(auxCV < minCV3) { 
    minCV3 = auxCV  
    minC3 = C
  }
}
minCV3
minC3
