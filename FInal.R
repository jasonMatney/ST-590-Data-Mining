rm(list=ls())
set.seed(1)
library(psych)
library(MASS)
library(xtable)
setwd("/Users/jason/Desktop/ST\ 590\ Data\ Mining/Data\ Mining\ Final")
list.files()
wine <- read.csv("winequality-red.csv", sep=";",header=TRUE)
head(wine)
str(wine)

attach(wine)
par(mfrow=c(2,2), oma = c(1,1,0,0) + 0.1, mar = c(3,3,1,1) + 0.1)
barplot((table(quality)), col=c("slateblue4", "slategray", "slategray1", "slategray2", "slategray3", "skyblue4"))
mtext("Quality", side=1, outer=F, line=2, cex=0.8)
truehist(fixed.acidity, h = 0.5, col="slategray3")
mtext("Fixed Acidity", side=1, outer=F, line=2, cex=0.8)
truehist(volatile.acidity, h = 0.05, col="slategray3")
mtext("Volatile Acidity", side=1, outer=F, line=2, cex=0.8)
truehist(citric.acid, h = 0.1, col="slategray3")
mtext("Citric Acid", side=1, outer=F, line=2, cex=0.8)

par(mfrow=c(2,2), oma = c(1,1,0,0) + 0.1, mar = c(3,3,1,1) + 0.1)
truehist(residual.sugar, h = 0.5, col="slategray3")
mtext("Residual Sugar", side=1, outer=F, line=2, cex=0.8)
truehist(chlorides, col="slategray3")
mtext("Chlorides", side=1, outer=F, line=2, cex=0.8)
truehist(free.sulfur.dioxide, col="slategray3")
mtext("Free Sulfur", side=1, outer=F, line=2, cex=0.8)
truehist(total.sulfur.dioxide, col="slategray3")
mtext("Total Sulfur Dioxide", side=1, outer=F, line=2, cex=0.8)

par(mfrow=c(2,2), oma = c(1,1,0,0) + 0.1, mar = c(3,3,1,1) + 0.1)
truehist(density, col="slategray3")
mtext("Density", side=1, outer=F, line=2, cex=0.8)
truehist(pH, col="slategray3")
mtext("pH", side=1, outer=F, line=2, cex=0.8)
truehist(sulphates, col="slategray3")
mtext("Sulphates", side=1, outer=F, line=2, cex=0.8)
truehist(alcohol, col="slategray3")
mtext("Alcohol", side=1, outer=F, line=2, cex=0.8)




par(mfrow=c(1,5), oma = c(1,1,0,0) + 0.1,  mar = c(3,3,1,1) + 0.1)
boxplot(fixed.acidity, col="slategray2", pch=19)
mtext("Fixed Acidity", cex=0.8, side=1, line=2)
boxplot(volatile.acidity, col="slategray2", pch=19)
mtext("Volatile Acidity", cex=0.8, side=1, line=2)
boxplot(citric.acid, col="slategray2", pch=19)
mtext("Citric Acid", cex=0.8, side=1, line=2)
boxplot(residual.sugar, col="slategray2", pch=19)
mtext("Residual Sugar", cex=0.8, side=1, line=2)
boxplot(chlorides, col="slategray2", pch=19)
mtext("Chlorides", cex=0.8, side=1, line=2)

par(mfrow=c(1,6), oma = c(1,1,0,0) + 0.1,  mar = c(3,3,1,1) + 0.1)
boxplot(free.sulfur.dioxide, col="slategray2", pch=19)
mtext("Free Sulfur Dioxide", cex=0.8, side=1, line=2)
boxplot(total.sulfur.dioxide, col="slategray2", pch=19)
mtext("Total Sulfur Dioxide", cex=0.8, side=1, line=2)
boxplot(density, col="slategray2", pch=19)
mtext("Density", cex=0.8, side=1, line=2)
boxplot(pH, col="slategray2", pch=19)
mtext("pH", cex=0.8, side=1, line=2)
boxplot(sulphates, col="slategray2", pch=19)
mtext("Sulphates", cex=0.8, side=1, line=2)
boxplot(alcohol, col="slategray2", pch=19)
mtext("Alcohol", cex=0.8, side=1, line=2)

summary(wine)
names(wine) <- c("fixed acidity", "volatile acidity","citric acid","residual sugar","chlorides","free sulfur dioxide","total sulfur dioxide","density","pH","sulphates","alcohol","quality")

describe(wine)
cor(wine[,-12])
cor(wine[,-12], method="spearman")


pairs(wine[,-12], gap=0, pch=19, cex=0.4, col="darkblue")
title(sub="Scatterplot of Chemical Attributes", cex=0.8)

newobject<-xtable(object)
print.xtable(newobject, type="html", file="pearson.html")
?cor

limout <- rep(0,11)
for (i in 1:11){
  t1 <- quantile(wine[,i], 0.75)
  t2 <- IQR(wine[,i], 0.75)
  limout[i] <- t1 + 1.5*t2
}
wineIndex <- matrix(0, 1599, 11)
for (i in 1:1599)
  for (j in 1:11){
    if (wine[i,j] > limout[j]) wineIndex[i,j] <- 1
  }
WInd <- apply(wineIndex, 1, sum)
wineTemp <- cbind(WInd, wine)
Indexes <- rep(0, 208)
j <- 1
for (i in 1:1599){
  if (WInd[i] > 0) {Indexes[j]<- i
  j <- j + 1}
  else j <- j
}
wineLib <-wine[-Indexes,]   # Inside of Q3+1.5IQR
indexes = sample(1:nrow(wineLib), size=0.5*nrow(wineLib))
WTrain50 <- wine[indexes,]
WTest50 <- wineLib[-indexes,]
