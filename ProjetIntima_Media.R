#X$paqan[is.na(X$paqan)] <- mean(X, na.rm = True)
#library(tidyr)
#for(i in 1:length(X$paqan)){
 # X$paqan[i]=replace_na(X$paqan[i], median(X$paqan,na.rm=TRUE))
#}

#X = na.omit(X)
#dim(X) #38 9
# alcool tabac sport sexe = binaire


setwd("C:/Users/msii/Desktop/AD projet")
library(readxl)
X<-read_excel("Intima_Media.xlsx",row.names=1)
View(X)
X<-X[,-6]
View(X)
str(X) #toutes les var sont quantitatives
summary(X)
#3rd qu : 75% des individus ont un age <48 ans, taille< 176, poid<75.75, paqan<13.95, mesure<0,57

par(mfrow=c(1,4))
boxplot(X$AGE,main='Age')
boxplot(X$taille,main='Taille')
boxplot(X$poids,main='Poids')
boxplot(X$mesure,main='Mesure')
#Interpretation:
#1 donnee aberrante du poids qui depasse 100 kg
#4 donnee aberrante du mesure depassant 0.7

library(ggplot2)
ggplot()+
  geom_point(data = X, aes(x=AGE, y=mesure))+
  xlab("Age")+
  ylab("Mesure")
  #scale_colour_manual(values="blue",label="",name=" ")
# l age a un impact sur l epaisseur de l intima media

reglin <- lm(X$mesure~X$AGE, data=X)
reglin
#Pvalue = 0.0042< 0.05 => l age a un impact sur l epaisseur de lintima media


#ACP
library("FactoMineR")
library("psych")
library("factoextra")
#2 tests: si on peut reduir la dim (acp)
C=cor(X)
dim(X)
C
KMO(C)
cortest.bartlett(C,n=110)

pca = PCA(X, scale.unit = TRUE,ncp=8, graph = TRUE)
summary(pca)

pca$eig
#1 2 3 a conserver
fviz_eig(pca)
#1 2 a retenir
summary(pca)

pca = PCA(X, scale.unit = TRUE,ncp = 3, graph = TRUE)
summary(pca)
#taille sex age poids mesure
pca = PCA(X, axe=c(1,3))
summary(pca)
indiv = pca$ind
indiv$contrib

fviz_pca_ind(pca,select.ind=list(indiv$contrib>1/110))

fviz_pca_ind(pca,select.ind=list(cos2=0.7)) #qualite >0.7
fviz_pca_biplot(pca,select.ind=list(cos2=0.7),select.var=list(cos2=0.5))

# Regression
resreg=lm(mesure~ SEXE +	AGE +	taille +	poids +	tabac +SPORT + alcool,data=X)
resreg
summary(resreg)


m0=lm(mesure~1,data=X)#model constant
AIC(m0)
BIC(m0)
summary(m0)
#demarches but; les var les plus pertinants
#demarche descendante
B=step(resreg,direction="backward")#complet
#AGE poids
summary(B)
AIC(B)
BIC(B)
#demarche ascendante
F=step(m0,scope=list(lower=m0, upper=resreg),direction="forward")
summary(F)
AIC(F)
BIC(F)
#demarch mixte
M=step(m0,scope=list(upper=resreg),direction="both")
summary(M)
AIC(M)
BIC(M)
