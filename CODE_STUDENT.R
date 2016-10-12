setwd('//file.phhp.ufl.edu/home/huihu/files/MixedEffectsModel') #change the path

#########################################
#install the lme4 and arm packages first#
#########################################

#install.packages("lme4")
#install.packages("arm")
#load the packages
library(lme4)
library(arm)
#read csv file
dfsbp<-read.csv("sbp.csv",header=T)


#5.2.1 empty model analysis
m1<-lmer(sbp~(1|id),data=dfsbp)
display(m1)

#5.2.2 varying intercept with individual-level predictor
m2<-lmer(sbp~bmi+(1|id),data=dfsbp)
display(m2)
#coefficients for first five groups
coef_m2<-coef(m2)
head(coef_m2[[1]])
#fixed-effects
fixef(m2)
#random-effects
head(ranef(m2)[[1]])
#standard errors
se.fixef(m2)
head(se.ranef(m2)[[1]])
#95% CI of the intercept for id=51
coef(m2)$id[51,1]+c(-1.96,1.96)*se.ranef(m2)$id[51]

#5.2.3 varying intercept with individual- and group-level predictors
m3<-lmer(sbp~bmi+age+(1|id),data=dfsbp)
display(m3)

#5.2.4 varying slopes model
m4<-lmer(sbp~bmi+(1+bmi|id),data=dfsbp)
display(m4)
head(coef(m4)[[1]])
fixef(m4)
head(ranef(m4)[[1]])
#add the age
m5<-lmer(sbp~bmi+age+bmi:age+(1+bmi|id),data=dfsbp)
display(m5)

#5.2.5 non-nested models
m6<-lmer(sbp~(1|id)+(1|am),data=dfsbp)
display(m6)

##############################################################
#Add a dichotomous variable ht to indicte hypertension status#
##############################################################
dfsbp$ht<-(dfsbp$sbp>140)*1

#5.3.1 empty model
m7<-glmer(ht~(1|id),family=binomial(link="logit"),data=dfsbp)
display(m7)
head(coef(m7)[[1]])
fixef(m7)
head(ranef(m7)[[1]])

#add bmi and race
m8<-glmer(ht~bmi+factor(race)+(1|id),family=binomial(link="logit"),data=dfsbp)
display(m8)
head(coef(m8)[[1]])
fixef(m8)
head(ranef(m8)[[1]])

#OR and 95% CI for BMI
exp(fixef(m8)[2])
exp(fixef(m8)[2]+c(-1.96,1.96)*se.fixef(m8)[2])

#5.3.2 Poisson model and GLMM
m9<-glmer(ex~bmi+(1|id),family=poisson(link="log"),data=dfsbp)
display(m9)
head(coef(m9)[[1]])