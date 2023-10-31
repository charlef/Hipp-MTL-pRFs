library(dplyr)
library(lme4)
library(lmerTest)
library(dplyr)
library(ggplot2)
library(afex)
library(lsmeans)
library(emmeans)
df_xseg17<- read.csv("right_PHC.csv")
df_xseg7<- read.csv("left_PHC.csv")
b17<- read.csv("ave_right_PHC.csv")
b7<- read.csv("ave_left_PHC.csv")

df_xseg16<- read.csv("right_PRC.csv")
df_xseg6<-read.csv("left_PRC.csv")
b16<- read.csv("ave_right_PRC.csv")
b6<- read.csv("ave_left_PRC.csv")

df_xseg15<- read.csv("right_pmEC.csv")
df_xseg5<- read.csv("left_pmEC.csv")
b15<- read.csv("ave_right_pmEC.csv")
b5<- read.csv("ave_left_pmEC.csv")

df_xseg14<- read.csv("right_alEC.csv")
df_xseg4<- read.csv("left_alEC.csv")
b14<- read.csv("ave_right_alEC.csv")
b4<- read.csv("ave_left_alEC.csv")

df_xseg13<- read.csv("right_aHPC.csv")
df_xseg3<- read.csv("left_aHPC.csv")
b13<- read.csv("ave_right_aHPC.csv")
b3<- read.csv("ave_left_aHPC.csv")

df_xseg12<- read.csv("right_mHPC.csv")
df_xseg2<- read.csv("left_mHPC.csv")
b12<- read.csv("ave_right_mHPC.csv")
b2<- read.csv("ave_left_mHPC.csv")

df_xseg11<- read.csv("right_pHPC.csv")
df_xseg1<- read.csv("left_pHPC.csv")
b11<- read.csv("ave_right_pHPC.csv")
b1<- read.csv("ave_left_pHPC.csv")


coeff1a <- lmer( sigma ~contrast +(1|participant), data=df_xseg14)
coeff1as<-summary(coeff1a)
coeff1as
coeff1a <- lmer( sigma ~contrast +(1|participant), data=df_xseg17)
coeff1as<-summary(coeff1a)
coeff1as
coeff1a <- lmer( sigma ~contrast +(1|participant), data=df_xseg7)
coeff1as<-summary(coeff1a)
coeff1as
coeff1 <- lmer( sigma ~ contrast+ (1|participant), data=df_xseg16)
coeff1r<-summary(coeff1)
coeff1l <- lmer( sigma ~ contrast+ (1|participant), data=df_xseg6)
coeff1la<-summary(coeff1l)
#4 = leftECal 5 = leftECpm
coeff1 <- lmer( sigma ~ contrast+ (1|participant), data=df_xseg14)
coeff1r<-summary(coeff1)
coeff1 <- lmer( sigma ~ contrast+ (1|participant), data=df_xseg4)
coeff1l<-summary(coeff1)

coeff1 <- lmer( sigma ~ contrast+ (1|participant), data=df_xseg5)
coeff1l<-summary(coeff1)
coeff1 <- lmer( sigma ~ contrast+ (1|participant), data=df_xseg15)
coeff1r<-summary(coeff1)

  ###Generating Data Frames for ANOVA test
  bSigma<-data.frame(b17$sigma,b16$sigma,b15$sigma,b14$sigma,b13$sigma,b12$sigma,b11$sigma,b7$sigma,b6$sigma,b5$sigma,b4$sigma,b3$sigma,b2$sigma,b1$sigma,lenc=c(1:157))
  bEcc<-data.frame(b17$eccentricity,b16$eccentricity,b15$eccentricity,b14$eccentricity,b13$eccentricity,b12$eccentricity,b11$eccentricity,b7$eccentricity,b6$eccentricity,b5$eccentricity,b4$eccentricity,b3$eccentricity,b2$eccentricity,b1$eccentricity,lenc=c(1:157))
  bVoxel<-data.frame(b17$vexel,b16$vexel,b15$vexel,b14$vexel,b13$vexel,b12$vexel,b11$vexel,b7$vexel,b6$vexel,b5$vexel,b4$vexel,b3$vexel,b2$vexel,b1$vexel,lenc=c(1:157))
  bCoeff<-data.frame(b17$contrast,b16$contrast,b15$contrast,b14$contrast,b13$contrast,b12$contrast,b11$contrast,b7$contrast,b6$contrast,b5$contrast,b4$contrast,b3$contrast,b2$contrast,b1$contrast,lenc=c(1:157))
  
  ###
segmentx=c(rep(1,157),rep(2,157),rep(3,157),rep(4,157),rep(5,157),rep(6,157),rep(7,157))
segmentx=c(segmentx,segmentx)
hemix=c(rep(1,1099),rep(2,1099))
#Test ANOVA Variance Explained

data_long <- gather( bVoxel, condition, measurement, b17.vexel:b1.vexel, factor_key=TRUE)
data_long<-data.frame(data_long,hemix,segmentx)
data_long$segmentx<-as.factor(data_long$segmentx)
data_long$hemix<-as.factor(data_long$hemix)
ANOVAVOXtest.aov <-   aov(measurement ~ hemix *segmentx +
                      Error(factor(lenc) / (hemix*segmentx)),data=data_long)
summary(ANOVAVOXtest.aov)
avox<-lm(measurement ~ hemix*segmentx + Error(factor(lenc)/(hemix*segmentx)), data=data_long)
summary(avox)


a1 <- aov_car(measurement ~ hemix*segmentx + Error(factor(lenc)/(hemix*segmentx)), data=data_long)
a1
segmentx=c(rep(1,157),rep(2,157),rep(3,157),rep(4,157),rep(5,157),rep(6,157),rep(7,157))
segmentx=c(segmentx,segmentx)
hemix=c(rep(1,1099),rep(2,1099))

## Test Eccentricity ANOVA
data_long <- gather( bEcc, condition, measurement, b17.eccentricity:b1.eccentricity, factor_key=TRUE)

data_long<-data.frame(data_long,hemix,segmentx)
data_long$segmentx<-as.factor(data_long$segmentx)

ANOVAECCtest.aov <-   aov(measurement ~ hemix*segmentx + Error(factor(lenc) / (hemix*segmentx)),data=data_long)
summary(ANOVAECCtest.aov)
a1 <- aov_car(measurement ~ hemix*segmentx + Error(factor(lenc)/(hemix*segmentx)), data=data_long)
a1
aecc<-lm(measurement ~ hemix*segmentx + Error(factor(lenc)/(hemix*segmentx)), data=data_long)
summary(aecc)
etaecc<-eta_squared(aecc)
etaecc

## 
## Now test Sigma ANOVA


data_long <- gather(bSigma, condition, measurement, b17.sigma:b1.sigma, factor_key=TRUE)
data_long<-data.frame(data_long,hemix,segmentx)
data_long$segmentx<-as.factor(data_long$segmentx)
ANOVASIGtestsigma.aov <-   aov(measurement ~ hemix *segmentx + Error((factor(lenc)) / (hemix+segmentx)),data=data_long)
summary(ANOVASIGtestsigma.aov)
a1 <- aov_car(measurement ~ hemix*segmentx + Error(factor(lenc)/(hemix*segmentx)), data=data_long)
a1
asig<-lm(measurement ~ hemix*segmentx + Error(factor(lenc)/(hemix*segmentx)), data=data_long)
summary(asig)

## coeff Scenes versus Face contrast ANOVA
data_long <- gather(bCoeff, condition, measurement, b17.contrast:b1.contrast, factor_key=TRUE)
data_long<-data.frame(data_long,hemix,segmentx)
data_long$segmentx<-as.factor(data_long$segmentx)
ANOVASIGtest.aov <-   aov(measurement ~ hemix *segmentx + Error((factor(lenc)) / (hemix+segmentx)),data=data_long)
summary(ANOVASIGtest.aov)
a1 <- aov_car(measurement ~ hemix*segmentx + Error(factor(lenc)/(hemix*segmentx)), data=data_long)
a1
