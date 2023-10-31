library(lme4)
library(lmerTest)
library(dplyr)
library(ggplot2)

setwd("~/Desktop/Hippocampus_Files/CerebralCortex_resubmission")
left_AP<-read.csv("Hipp_left_AP.csv", header = TRUE)
right_AP<-read.csv("Hipp_right_AP.csv", header = TRUE)

coeffright <- lmer( scenevface ~sigma +(1|par), data=right_AP)
coeffrights<-summary(coeffright)
coeffrights

coeffleft <- lmer( scenevface ~sigma +(1|par), data=left_AP)
coefflefts<-summary(coeffleft)
coefflefts


#____________ This is the end of the coefficient graphs
#b<-aggregate(df_x, by=list(df_x$V1), FUN= mean) #this lists the mean for each variable but it does not have the anterior/posterior divisions
#coefficient by AP axis right hemisphere #this will be faces vs. scenes
coeff1 <- lmer( scenevface ~AP +(1|par), data=right_AP)
coeff1s<-summary(coeff1)
coeff1s

 #coefficient by AP axis left hemisphere #this will be faces vs. scenes
coeff1l <- lmer( scenevface ~AP +(1|par), data=left_AP)
coeff1sl<-summary(coeff1l)
coeff1sl
## NOW for SIZE
#Size by AP axis right hemisphere
size1r<- lmer( sigma ~AP +(1|par), data=right_AP)
sizer<-summary(size1r)
sizer

 #size by AP axis left hemisphere
size1l <- lmer( sigma ~AP +(1|V1), data=left_AP)
sizesl<-summary(size1l)
sizesl

 #eccentricity by AP axis left hemisphere
size1le <- lmer( eccen ~AP +(1|par), data=left_AP)
sizesle<-summary(size1le)
sizesle

 #eccentricity by AP axis right hemisphere
size1re <- lmer( eccen ~AP +(1|par), data=right_AP)
sizesre<-summary(size1re)
sizesre
###
## NOW for SIZE by coeff
#Size by coeff axis right hemisphere #next try with +V6
szco1r<- lmer( scenevface ~sigma +(1|par), data=right_AP)
szcor<-summary(szco1r)
szcor

#size by coeff axis left hemisphere
szco1l <- lmer( scenevface ~sigma +(1|par), data=left_AP)
szcol<-summary(szco1l)
szcol
###

## Again for SIZE by coeff
 #Size by coeff axis right hemisphere #now try with +V6 var expla
szco1r<- lmer( scenevface ~sigma +varx +(1|par), data=right_AP)
szcor<-summary(szco1r)
szcor

#size by coeff axis left hemisphere with var expl
szco1l <- lmer( scenevface ~sigma +varx +(1|par), data=left_AP)
szcol<-summary(szco1l)
szcol
###

## NOW for eccentricity by coeff
 #eccentricity by coeff axis right hemisphere
ecco1r<- lmer( scenevface ~eccen +(1|par), data=right_AP)
eccor<-summary(ecco1r)
eccor

 #eccentricity by coeff axis left hemisphere
ecco1l <- lmer( scenevface ~eccen +(1|par), data=left_AP)
eccol<-summary(ecco1l)
eccol
###

## NOW for eccentricity by size
 #eccentricity by size axis right hemisphere
eccsz1r<- lmer( sigma ~eccen +(1|par), data=right_AP)
eccszr<-summary(eccsz1r)
eccszr

 #eccentricity by size axis left hemisphere
eccsz1l <- lmer( sigma ~eccen +(1|par), data=left_AP)
eccszl<-summary(eccsz1l)
format(eccszl, scientific = FALSE)
eccszl
###
#var by AP axis left hemisphere
size1lv <- lmer( varx ~AP +(1|par), data=left_AP)
sizeslv<-summary(size1lv)
sizeslv

 #Voxel by AP axis right hemisphere
size1rv <- lmer( varx ~AP +(1|par), data=right_AP)
sizesrv<-summary(size1rv)
sizesrv

## Now for size ~ by ap axis and R2
 #Size by AP axis right hemisphere
size1r<- lmer( sigma ~AP+varx +(1|par), data=right_AP)
sizer<-summary(size1r)
sizer

#size by APaxis + R2  left hemisphere
size1l <- lmer( sigma ~AP +varx+(1|par), data=left_AP)
sizesl<-summary(size1l)
sizesl
##
##