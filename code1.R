#Statistiques ines

# Ouverture espace de travail et librabry
rm(list=ls())
setwd("C:/Users/Colombine/Desktop/R")
h<-read.table("RHOSPIT.txt",sep="",dec=",",header=T)
library(prettyR)
#Verification que h est bien un tableaa de donnees et verification de la nature des donees
str(h)

################################
#Description de chaque donnee
################################
par(mfrow=c(1,1))

##SIAMU
describe.numeric(h$SIAMU)
boxplot(h$SIAMU)
hist(h$SIAMU)
qqnorm(h$SIAMU)
qqline(h$SIAMU)
shapiro.test(h$SIAMU)
#Pas de distribution normale

##Medecine A
describe.numeric(h$MedA)
boxplot(h$MedA)
hist(h$MedA)
qqnorm(h$MedA)
qqline(h$MedA)
shapiro.test(h$MedA)
#Pas de distribution normale

##Medecine B
describe.numeric(h$MedB)
boxplot(h$MedB)
hist(h$MedB)
qqnorm(h$MedB)
qqline(h$MedB)
shapiro.test(h$MedB)
#distribution normale selon shapiro test mais non visuelle

##Chirurgie A
par(mar = rep(2, 4))
describe.numeric(h$ChirA)
summary(h$ChirA)
boxplot(h$ChirA)
hist(h$ChirA)
qqnorm(h$ChirA)
qqline(h$ChirA)
shapiro.test(h$ChirA)
# Non normale

##Chirurgie B
par(mar = rep(2, 4))
describe.numeric(h$ChirB)
boxplot(h$ChirB)
hist(h$ChirB)
qqnorm(h$ChirB)
qqline(h$ChirB)
shapiro.test(h$ChirB)
#Non normale

## Regroupement des donnes Medecine et Chirurgie et Siamu dans un nouveau tableau de 50 donnees



medA<-unlist(h$MedA)
medB<-unlist(h$MedB)
med<-c(1:50)
med[1:25]<-medA
med[26:50]<-medB
print(med)

chiA<-unlist(h$ChirA)
chiB<-unlist(h$ChirB)
chir<-c(1:50)
chir[1:25]<-chiA
chir[26:50]<-chiB
print(chir)

h2<-data.frame(med,chir)
h2$SIAM<-h$SIAM
h2$SIAM[26:50]<-c(NA)


#SIAM
describe.numeric(h2$SIAM)
boxplot(h2$SIAM)
hist(h2$SIAM)
qqnorm(h2$SIAM)
qqline(h2$SIAM)
shapiro.test(h2$SIAM)
#Non normal


#Medecine
describe.numeric(h2$med)
boxplot(h2$med)
hist(h2$med)
qqnorm(h2$med)
qqline(h2$med)
shapiro.test(h2$med)
#Non normal

#Chirurgie

describe.numeric(h2$chir)
boxplot(h2$chir)
hist(h2$chir)
qqnorm(h2$chir)
qqline(h2$chir)
shapiro.test(h2$chir)
#non normal

### Boxplot 
boxplot(h2$SIAM,h2$med,h2$chir,col=c("red","blue","yellow"),xlab=c("SIAMU","Medecine","Chirurgie"))
legend(x="topright",c("Siamu","Medecine","Chirurgie"),col=c("red","blue","yellow"),lwd=c(2,2))




# 27/09/2021
# Travaux tutor?s - Estimation
##########################################




##########################################
# d?finition du r?pertoire de travail, chargement
# des librairies
##########################################

rm(list=ls())
setwd("D:/docs_mpaul/Master_GIMAT/Module_StatEpi/Data")

library(prettyR)
library(Rmisc)

##########################################
# Lecture des donn?es
##########################################

d <- read.csv2("ECWP_Repro_GIMAT.txt")


##########################################
# Visualisation et v?rification des donn?es
##########################################

dim(d)              # dimensions (nb lignes ; nb colonnes)
head(d)             # affiche les premi?res lignes

####################################################################################
# G?n?rer des ?chantillons de 100 observations ? partir de la base de donn?es initiale
####################################################################################

d1 <- d[sample(nrow(d), 50), ]       # on travaillera ensuite avec d1


################################################
# Estimation de la moyenne de la population ? 
# partir d'un ?chantillon de 50 individus (d1)
################################################    

# Q-Q plot
qqnorm(d1$p1)
qqline(d1$p1)

# Test de normalit?
shapiro.test(d1$p1)

# calcul ? la main
describe(d1$p1)
# 39.26-1.96*5.11/sqrt(50)
# 39.26+1.96*5.11/sqrt(50)

# Calcul des intervalles de confiance de la moyenne
t.test(d1$p1)
CI(d1$p1, ci=0.95)



################################################
# Estimation d'un pourcentage de la population ? 
# partir d'un ?chantillon de 50 individus (d1)
################################################ 

table(d1$sexe)
binom.test(29, 50)     # proportion de femelles dans l'?chantillon , et IC


#jklj
##jklmjmj
###klzùerkjmlazejr

