##########################################
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

