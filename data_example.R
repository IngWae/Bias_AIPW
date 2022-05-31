##################################
# Analyses for the NHANES dataset#
##################################

library(PSW)
library(iWeigReg)
library(cobalt)
library(WeightIt)

rm(list = ls())
#setwd("C:")

#read data from dfmale.csv
datat<-read.csv("dfmale.csv",header=TRUE) 
summary(datat)
form.ps<-"smoking~married+birth.country+edu+race+income+income.mis+cage+cage2+army+cfamily.size"
form.or<-"lead~married+birth.country+edu+race+income+income.mis+cage+cage2+army+cfamily.size"

#to explore models

mod.ps<-glm(form.ps,family=binomial,data=datat)
mod.or<-glm(form.or,family=gaussian,data=datat)

datat$ps<-fitted.values(mod.ps)
datat$or<-fitted.values(mod.or)

#Overlap and balance plots

overlap.plot <- psw(datat, form.ps = form.ps, weight = "ATE",
                    mirror.hist = TRUE, add.weight = TRUE )

legend(0.5,60,c("Top-Controls","Bottom-Treated"),bty="n")

w.out1 <- weightit(smoking~married+birth.country+edu+race+income+income.mis+cage+cage2+army+cfamily.size,estimand = "ATE", method = "ps",data=datat)

v <- data.frame(old = c("prop.score","marriedYes", "birth.country_Born in 50 US States or Washington, DC", "birth.country_Born in Mexico", "birth.country_Born in Other Non-Spanish Speaking Country",
                        "birth.country_Born in Other Spanish Speaking Country", "edu_edu.9to11",  "edu_edu.college", "edu_edu.hischl",  
                        "edu_edu.lt9","edu_edu.somecol","race_black","race_mexicanam","race_otherhispan","race_otherrace","race_white","income","income.mis","cage","cage2",
                        "army_Yes","cfamily.size"),
                new = c("Propensity Score","Married", "Born in US", "Born in Mexico",
                        "Born in other non spanish-speaking country", "Born in other spanish-speaking country", "9-11 years of schooling", "College education",
                        "High-School education", "Less than 9  years education","Some college education", "Black", "Mexican-american", "Other hispanic", "Other","White","Income",
                        "Income missing", "Age","Age^2", "Served in army","Family size")          )

love.plot(w.out1, stats = "m", threshold = .1,
          var.order = "alphabetical", var.names = v,colors = c("deepskyblue4", "azure4"))


#ATE

X.matrix<-model.matrix(smoking~married+birth.country+edu+race+income+income.mis+cage+cage2+army+cfamily.size,data=datat)

dat.X<-as.data.frame(X.matrix)
names(dat.X)<-c(LETTERS[1:19])

dat<-cbind(datat$smoking,datat$lead,dat.X)
names(dat)<-c("smoking","lead",LETTERS[1:19])

names(dat)
#ipw 1 with iweighreg

ipw1 <- ate.HT(datat$lead, datat$smoking, datat$ps, X=X.matrix)
ipw1$diff
sqrt(ipw1$v.diff)


ipw1$diff- 1.96*sqrt(ipw1$v.diff)
ipw1$diff+ 1.96*sqrt(ipw1$v.diff)

#ipw2 

form.ps<-paste(names(dat.X)[-1], collapse='+')

treatment<-"smoking"
ps.form<-paste(treatment,"~",form.ps)
outcome<-"lead"
or.form<-paste(outcome,"~",form.ps)

ps.form<-as.formula(ps.form)
or.form<-as.formula(or.form)


ipw2.psw <- psw(data=dat,form.ps=ps.form, weight ="ATE",wt=T,out.var = "lead")
ipw2.psw$est.wt
ipw2.psw$std.wt

ipw2.psw$est.wt- 1.96*ipw2.psw$std.wt
ipw2.psw$est.wt+ 1.96*ipw2.psw$std.wt

#aipw

aipw.psw<- psw( data = dat, form.ps = ps.form, weight = "ATE", aug = TRUE,
                form.outcome = or.form, family = "gaussian")

aipw.psw$est.aug
aipw.psw$std.aug


aipw.psw$est.aug- 1.96*aipw.psw$std.aug
aipw.psw$est.aug  + 1.96*aipw.psw$std.aug
  