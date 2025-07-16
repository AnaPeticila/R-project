
# Analiza companiilor Apple si Samsung ------------------------------------

#Import date in R
#import txt
setwd("C:\\Users\\Andrei Peticila\\Desktop\\proiectmm")
actiuni <-read.table("proiectMM.txt",header=TRUE,sep="\t",dec=".")

#import csv
actiuni2 <-read.csv("proiectMM.csv",header=TRUE,sep=",",dec=".")

#generare tabel cu date
fix(actiuni)

#capul de tabel
head(actiuni)

#vector cu denumirile variabilelor
names(actiuni)

#furnizarea statisticilor descriptive
summary(actiuni)

#coeficientul de asimetrie (Skeweness) pentru prețuri
library(moments)
skewness(actiuni$Pret.AAPL)
skewness(actiuni$Pret.SMSM_dolar)
skewness(actiuni$Pret.Indice)

#coeficientul de aplatizare (Kurtosis) pentru prețuri
kurtosis(actiuni$Pret.AAPL)
kurtosis(actiuni$Pret.SMSM_dolar)
kurtosis(actiuni$Pret.Indice)

#abaterea standard pentru prețuri
sd(actiuni$Pret.AAPL)
sd(actiuni$Pret.SMSM_dolar)
sd(actiuni$Pret.Indice)

#media prețurilor de la fiecare companie
mean(actiuni$Pret.AAPL)
mean(actiuni$Pret.SMSM_dolar)
mean(actiuni$Pret.Indice)

#covarianța pentru prețuri
sd(actiuni$Pret.AAPL)/mean(actiuni$Pret.AAPL)
sd(actiuni$Pret.SMSM_dolar)/mean(actiuni$Pret.SMSM_dolar)
sd(actiuni$Pret.Indice)/mean(actiuni$Pret.Indice)

#histograme
par(mfrow=c(1,3))
hist(actiuni$Pret.AAPL, col="lightblue", main="Histograma prețului de închidere pentru Apple", ylab="Frecvența", xlab="Valoarea")
hist(actiuni$Pret.SMSM_dolar, col="lightpink",main="Histograma prețului de închidere pentru Samsung", ylab="Frecvența", xlab="Valoarea")
hist(actiuni$Pret.Indice, col="lightgreen", main="Histograma prețului de închidere pentru S&P",ylab="Frecvența", xlab="Valoarea")

#boxploturi
par(mfrow=c(1,3))
boxplot(actiuni$Pret.AAPL, col="lightblue", main="Boxplot pret Apple", horizontal = TRUE)
boxplot(actiuni$Pret.SMSM_dolar, col="lightpink", main="Boxplot pret Samsung", horizontal = TRUE)
boxplot(actiuni$Pret.Indice, col="lightgreen", main="Boxplot pret Indice S&P", horizontal = TRUE)

#matricea de corelatie pentru preturi
co <- data.frame(actiuni$Pret.AAPL, actiuni$Pret.SMSM_dolar,actiuni$Pret.Indice )
matr_corelatie <-cor(co)
library(corrplot)
windows()
corrplot(matr_corelatie)

#rentabilitate Apple
rentabilitate_Apple <-numeric()
for(i in 1:length(actiuni$Pret.AAPL))
{
  rentabilitate_Apple[i] <-
    (actiuni$Pret.AAPL[i]/actiuni$Pret.AAPL[i-1])-1
}
rentabilitate_Apple<-na.omit(rentabilitate_Apple)
rentabilitate_Apple
View(as.matrix(rentabilitate_Apple))

#rentabilitatea pentru Samsung
rentabilitate_Samsung <-numeric()
for(i in 1:length(actiuni$Pret.SMSM_dolar))
{
  rentabilitate_Samsung [i] <-
    (actiuni$Pret.SMSM_dolar[i]/actiuni$Pret.SMSM_dolar[i-1])-1
}
rentabilitate_Samsung<-na.omit(rentabilitate_Samsung)
rentabilitate_Samsung
View(as.matrix(rentabilitate_Samsung ))

#rentabilitatea pentru indicele S&P
rentabilitate_Indice <-numeric()
for(i in 1:length(actiuni$Pret.Indice))
{
  rentabilitate_Indice [i] <-
    (actiuni$Pret.Indice[i]/actiuni$Pret.Indice[i-1])-1
}
rentabilitate_Indice<-na.omit(rentabilitate_Indice)
rentabilitate_Indice
View(as.matrix(rentabilitate_Indice ))

#coeficientul de asimetrie (Skeweness) pentru rentabilitate
library(moments)
skewness(rentabilitate_Apple)
skewness(rentabilitate_Samsung)
skewness(rentabilitate_Indice)

#coeficientul de aplatizare (Kurtosis) pentru rentabilitate
kurtosis(rentabilitate_Apple)
kurtosis(rentabilitate_Samsung)
kurtosis(rentabilitate_Indice)

#abaterea standard pentru rentabilitate
sd(rentabilitate_Apple)
sd(rentabilitate_Samsung)
sd(rentabilitate_Indice)

#media prețurilor de la fiecare companie
mean(rentabilitate_Apple)
mean(rentabilitate_Samsung)
mean(rentabilitate_Indice)

#covarianța pentru rentabilitate
sd(rentabilitate_Apple)/mean(rentabilitate_Apple)
sd(rentabilitate_Samsung)/mean(rentabilitate_Samsung)
sd(rentabilitate_Indice)/mean(rentabilitate_Indice)

#histograme
par(mfrow=c(1,3))
hist(rentabilitate_Apple, col="blue", main="Histograma rentabilității companiei Apple", ylab="Frecventa", xlab="Valoarea")
hist(rentabilitate_Samsung, col="red", main="Histograma rentabilității companieiSamsung", ylab="Frecventa", xlab="Valoarea")
hist(rentabilitate_Indice, col="green", main="Histograma rentabilității indicelui S&P", ylab="Frecventa", xlab="Valoarea")

#boxploturi
par(mfrow=c(1,3))
boxplot(rentabilitate_Apple, col="lightblue", main="Boxplot rentabilitate Apple", horizontal = TRUE)
boxplot(rentabilitate_Samsung, col="lightpink", main="Boxplot rentabilitate Samsung", horizontal = TRUE)
boxplot(rentabilitate_Indice, col="lightgreen", main="Boxplot rentabilitate Indice S&P", horizontal = TRUE)

#matricea de corelatie pentru rentabilitate
ma <- data.frame(rentabilitate_Apple, rentabilitate_Samsung,rentabilitate_Indice )
matr_corelatie <-cor(ma)
library(corrplot)
windows()
corrplot(matr_corelatie)

#outlierii pentru boxplot-ul cu prețuri
par(mfrow=c(1,3))
boxplot(actiuni$Pret.AAPL, col="lightblue", main="Boxplot pret Apple", horizontal = TRUE)$out
boxplot(actiuni$Pret.SMSM_dolar, col="lightpink", main="Boxplot pret Samsung", horizontal = TRUE)$out
boxplot(actiuni$Pret.Indice, col="lightgreen", main="Boxplot pret Indice S&P", horizontal = TRUE)$out

#outlierii pentru boxplot-ul cu rentabilități
par(mfrow=c(1,3))
boxplot(rentabilitate_Apple, col="lightblue", main="Boxplot rentabilitate Apple", horizontal = TRUE)$out
boxplot(rentabilitate_Samsung, col="lightpink", main="Boxplot rentabilitate Samsung", horizontal = TRUE)$out
boxplot(rentabilitate_Indice, col="lightgreen", main="Boxplot rentabilitate Indice S&P", horizontal = TRUE)$out

#Evoluția prețurilor
par(mfrow=c(1,3))
library(corrplot)
plot(actiuni$Pret.AAPL,type="l",main="Evoluția prețului acțiunii Apple")
p<-data.frame(actiuni$Pret.AAPL)

library(corrplot)
plot(actiuni$Pret.SMSM_dolar,type="l",main="Evoluția prețului acțiunii Samsung")
p<-data.frame(actiuni$Pret.SMSM_dolar)

library(corrplot)
plot(actiuni$Pret.Indice,type="l",main="Evoluția prețului indicelui S&P")
p<-data.frame(actiuni$Pret.Indice)

#Evoluția rentabilității
par(mfrow=c(1,3))
library(corrplot)
plot(rentabilitate_Apple,type="l",main="Evoluția rentabilității acțiunii Apple")

library(corrplot)
plot(rentabilitate_Samsung,type="l",main="Evoluția rentabilității acțiunii Samsung")
p<-data.frame(rentabilitate_Samsung)

library(corrplot)
plot(rentabilitate_Indice,type="l",main="Evoluția  rentabilității indicelui S&P")
p<-data.frame(rentabilitate_Indice)

