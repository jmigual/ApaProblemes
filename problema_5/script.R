#####################
##   PROBLEMA 10   ##
#####################
## Reconeixement   ##
## de lletres amb  ##
## la xarxa MLP    ##
#####################
## Lluc Bové       ##
## Raúl Ibáñez     ##
## Joan Marcè      ##
## Aleix Trasserra ##
#####################
# Prenem les lletres A,B,C... codificades digitalment en una quadrícula de 7x5.
# Per exemple, A es codifica com:
#
#    0 1 1 1 0
#    1 0 0 0 1
#    1 0 0 0 1
#    1 1 1 1 1
#    1 0 0 0 1
#    1 0 0 0 1
#    1 0 0 0 1
#
# L'arxiu letters.txt conté codificacions de 21 lletres, cadascuna representada com un vector de 
# longitud 35. La tasca és dissenyar una xarxa neuronal que classifiqui imatges (possiblement 
# corruptes) com a lletres de l'alfabet.
#####################

# Llegim l'arxiu letters.txt

rm(list=ls())
tot <- read.table(file.choose())
lletres = tot[1:35]

#####################
# 1. Dissenyeu una funció que generi versions corruptes d'una lletra, a còpia de canviar un cert
#    número de bits de manera aleatòria. Una manera senzilla és generar primer el número de bits
#    corruptes -p.e. amb una Poisson(lambda = 1.01)- seleccionar els bits concrets (uniformement)
#    i després invertir-los.
#####################

# La funció 'canviaLletra' agafa 'una 'lletra' i li canvia alguns bits a l'atzar

canviaLletra <- function(lletra) {
  lletra <- as.integer(lletra[1:35])
  index = c(sample.int(35,rpois(1, 1.01)))
  if (length(index) == 0) return(lletra)
  lletra[index] = 1 - lletra[index]
  lletra
}

#####################
# 2. Dissenyeu una funcio que, partint de les lletres netes (arxiu letters.txt) generi unes dades
#    corruptes que usarem com a mostra de training, de mida N.
#####################

# La funció 'sobrePP' genera 'N' dades corruptes de les dades 'tot'

sobrePP <- function(N, tot) {
  data <- tot[sample.int(26,N,replace=T),]
  data[,1:35] <- t(apply(data, 1,canviaLletra))
  rownames(data) <- NULL
  data
}

#####################
# 3. Entreneu una xarxa MLP per aprendre la tasca. Caldrà que estimeu la millor arquitectura, cosa
#    que podeu fer per cross-validation, usant regularització.
#####################

# Generem 1000 dades de training i 500 de test.

dades <- sobrePP(1000,tot)
test <- sobrePP(500,tot)

# Estimem la millor arquitectura fent us de la funcio train().

library(lattice)
library(ggplot2)
library(caret)
library(e1071)

(decays <- 10^seq(-3,0,by=0.1))
trc <- trainControl (method="repeatedcv", number=10, repeats=10)
model.10x10CV <- train (V36 ~., data = dades, method='nnet', maxit = 200, trace = FALSE,
                        tuneGrid = expand.grid(.size=10,.decay=decays), trControl=trc)

# Com que triga molt, hem guardat una execució anterior.
# Carreguem model.10x10CV i les dades de training i test
load ("model.10x10CV.regul")
load ("model.dades")

# I mirem quin és el millor model per aquestes dades.
(model.10x10CV$bestTune)

# Veiem que per una xarxa amb 10 neurones, el millor decay és de 0.5011872.

#####################
# 4. Reporteu els resultats de predicció en una mostra de test gran -també generada per vosaltres,
#    i de manera anàloga a la de training.
#####################

# La nostra mostra de training eren 1000 dades i la de test és de 500 dades.
# Per tal d'obtenir els mateixos resultats hem guardat les dades de test.

load ("model.test")

# Ara calculem l'error de training i de test.

model.nnet <- nnet(V36 ~., data = dades, size=10, maxit=200, decay=0.5011872)

p1 <- as.factor(predict (model.nnet, type="class"))
levels(p1) <- LETTERS
t1 <- table(p1,dades$V36)
error_rate.learn <- 100*(1-sum(diag(t1))/nrow(dades))
(error_rate.learn)

# Error de training: 0.1%

p2 <- as.factor(predict (model.nnet,newdata = test,type="class"))
levels(p2) <- LETTERS
t2 <- table(p2,test$V36)
error_rate.test <- 100*(1-sum(diag(t2))/nrow(test))
error_rate.test

# Error de test: 0.4%