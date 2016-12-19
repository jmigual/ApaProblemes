
##########################
### PROBLEMA DE GRUP 6 ###
##########################
### Raúl Ibáñez        ###
### Lluc Bové          ###
### Aleix Trasserra    ###
### Joan  Marcè        ###
##########################

## La funció smiley() genera problemes de classificacio de 4 classes en
## dos dimensions, seguint una certa estructura, que es composa de dos
## ulls gaussians, un nas trapezoidal i una boca parabolica (amb soroll
## gaussia vertical).

## Es demana:
### 1) Carregueu les dades APA-TEMA8-P2.Rdata. Heu de crear tambe un
###    conjunt de test de mida N.test amb les mateixes caracteristiques
###    de soroll.

# Creem les dades de test amb les mateixes característiques de soroll.
library(mlbench)
N.train <- 1000
N.test <- 10000

p <- mlbench.smiley(n = N.test,sd1=0.5, sd2 = 0.5)
plot(p)

test <- as.data.frame(p)

# (per poder tractar tots amb les mateixes dades tenim unes guardades)
load(file.choose()) # Carregar dades de train
load(file.choose()) # Carregar dades de test

### 2) Dissenyeu un model de prediccio basat en la maquina de vectors
###    suport -rutina ksvm() del paquet kernlab- per predir la classe.

library(kernlab)


linear <- ksvm(classes ~., data = smiley, kernel = "vanilladot", C = 6, cross = 10)

cList <- c(0.01,0.1,1,10,100,1000)

# Aquesta funcio fa crides a ksvm i busca el millor model.
polinomKsvm = function(C, kernel, degree) {
  auxCV = 0
  for (i in 1:10){
    linear <- ksvm(classes ~., data = smiley, kernel = kernel, degree = degree, C = C, cross = 10)
    auxCV <- auxCV + linear@cross
  }
  auxCV <- auxCV/10
  return(auxCV)
}

## Busquem el millor model lineal ##

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

## Busquem el millor model quadratic ##

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

## Busquem el millor model cubic ##

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

## Busquem el millor model per RBF ##
# (Triga bastant)
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

# El model amb l'error minim es el model amb RBF

model = ksvm(classes ~., data = smiley, kernel = "rbfdot", C = minC4, sigma = minS4)

result = predict(model)

# Calculem l'error de training

100*(1 - sum(diag(table(result, smiley$classes)))/length(result))

# Error de training = 4.9%

result.test = predict(model, test)
table(result.test, test$classes)

# Calculem l'error de test

100*(1 - sum(diag(table(result.test, test$classes)))/length(result.test))

# Error de test = 6.72%

plot(test$x.x4,y = test$x.V2)
plot(smiley$x.x4,y=smiley$x.V2)

# Plots per veure les diferencies

par(mfrow=c(1,2))
plot(x = test$x.x4, y = test$x.V2, col = result.test, main = 'Dades de prediccio',xlab = '',ylab = '',xaxt='n',yaxt = 'n')
plot(x = test$x.x4, y = test$x.V2, col = test$classes, main = 'Dades originals',xlab = '', ylab = '',xaxt='n',yaxt = 'n')

### Heu d'acabar donant un sol model final i una estimacio honesta de
### com prediu.

# El nostre model final es el model per rbf amb els valors optims calculats, es a dir:
model = ksvm(classes ~., data = smiley, kernel = "rbfdot", C = minC4, sigma = minS4)
# Honestament, aquest model prediu força bé ja que s'equivoca en punts que es superposen, cosa que es lògic
# donat que si no s'equivoqués seria dolent, ja que no estaria donant la probabilitat més alta.

