setwd("C:/Users/msii/Desktop/AD projet")
library(readxl)
X<-read_excel("Classeur2.xlsx")
View(X)
str(X)
X=X[-length(X$Y),-1]
View(X)
Y=as.integer(X$Y) 
View(Y)

X=X[,-9]
View(X)
summary(X)
#75% des ind ont x1<451, x2<138, x3<74.75, x4<24.75, X5<116, X6<81.5, X7<263.8, X8<313

par(mfrow=c(2,4))
boxplot(X$X1,main='X1')
boxplot(X$X2,main='X2')
boxplot(X$X3,main='X3')
boxplot(X$X4,main='X4')
boxplot(X$X5,main='X5')
boxplot(X$X6,main='X6')
boxplot(X$X7,main='X7')
boxplot(X$X8,main='X8')
#pas de donnees aberrantes

#ACP
library("FactoMineR")
library("psych")
library("factoextra")

#2 tests: si on peut reduir la dim (acp)
C=cor(X)
dim(X)
C
KMO(C)
cortest.bartlett(C,n=38)
#pvalue <alpha => On peut appliquer lacp

pca = PCA(X, scale.unit = TRUE, graph = TRUE)
summary(pca)

#var
pca$eig
# critere de kaiser 1 2 3 a preserver
fviz_eig(pca)
#critere de coude axe 1 a preserver

#relancer l acp avec 3 axes seulement
pca = PCA(X, scale.unit = TRUE,ncp = 3, graph = TRUE)
summary(pca)
#X7 X1 X3

pca = PCA(X, axe=c(1,3))
#X3<>X2, X5<>X8
screen.plot=fviz_contrib(pca,choice="var",axes=1)
screen.plot

#Individu
indiv = pca$ind
indiv$contrib

fviz_pca_ind(pca,select.ind=list(indiv$contrib>1/38))

fviz_pca_ind(pca,select.ind=list(cos2=0.6)) #qualite >0.6
fviz_pca_biplot(pca,select.ind=list(cos2=0.6),select.var=list(cos2=0.3))

#individu
indiv=pca$ind
indiv
fviz_pca_ind(pca,select.ind=list(cos2=0.7))
fviz_pca_ind(pca,select.ind=list(cos2=0.5))
fviz_pca_biplot(pca,repel=TRUE)
fviz_pca_biplot(pca,select.ind=list(cos2=0.5),select.var=list(cos2=0.7))

pca.pca <- PCA(X, axe=c(1,3))
resum=get_pca_var(pca)
resum$coord#coordonnees des var

dimdesc(pca,axe=c(1,3),proba=0.05) #correlation
summary(pca.pca)
#X2 bien represente sur laxe 3
#alpha(X)


#Regression


resreg=lm(Y~ X1 +	X2 +	X3 +	X4 +	X5 + X6 + X7 + X8,data=X)
resreg
summary(resreg)
AIC(resreg)

m0=lm(Y~1,data=X)#model constant
AIC(m0)
BIC(m0)
summary(m0)
#demarche descendante
B=step(resreg,direction="backward")#complet
#X1 X3 X5 X6
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

X<-read_excel("Classeur2.xlsx")
str(X)
X=X[,-1]
View(X)
X$Y[39] = 0
X$Y = as.integer(X$Y)
View(X)
resreg=lm(Y~ X1+X3 + X5 + X6 ,data=X)
summary(resreg)
pre= predict(resreg)#y^
pre
