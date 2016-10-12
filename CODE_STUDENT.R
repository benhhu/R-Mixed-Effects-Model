setwd('//file.phhp.ufl.edu/home/huihu/files/MixedEffectModel/') #change the path

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
m6<-lmer(sbp~(1|id)+(1|pm),data=dfsbp)
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

#5.4.6 JAGS
#install.packages('rjags')  
#Note: Make sure you also install the JAGS
library('rjags')
dfsbp<-read.csv("sbp.csv",header=T)
#prepare the data to import into JAGS
y<-dfsbp$sbp
x<-dfsbp$bmi
id<-dfsbp$id
N<-nrow(dfsbp) #the total number of measurements
K<-length(unique(id)) #the total number of groups
#count the number of measurements for each group
dfsbp$count=1
nk<-aggregate(count~id,data=dfsbp,sum)$count

#sets up model object
jags<-jags.model('JAGS.bug',data=list('y'=y,'x'=x,'N'=N,'K'=K,'id'=id),n.chains=4,n.adapt=100)
#burn-in period
update(jags,10000)
#draw 1,0000 samples from the sampler
k<-jags.samples(jags,c('gamma','b','precy','preca','a'),10000)

#A function to plot the posterior distribution of the parameter of interest
inference=function(num,nome){
  k1=k[[num]][]
  plot(NA,NA,xlim=c(0,10000),ylim=range(k1))
  for (i in 1:4) lines(1:10000,k[[num]][1,,i],col=i)
  k2=density(k1)
  plot(k2,type='l',xlab='',main=nome)
  z=quantile(k1,c(0.025,0.975))
  abline(v=z,col='red',lty=3)
  print(c(mean(k1),z))
}
#inference on the parameters of interest
par(mfrow=c(2,2))
inference(3,'gamma')
inference(2,'b')
par(mfrow=c(1,1))

par(mfrow=c(2,2))
inference(5,'precy')
inference(4,'preca')
par(mfrow=c(1,1))

#intercept for the 51st group
mean(k$a[51,,])
quantile(k$a[51,,],c(0.025,0.975))
