#################
# NESARC wave 2 analysis
# Citation: Goldstein ND, Welles SL, Burstyn I. To be or not to be: Bayesian correction for misclassification of self-reported sexual behaviors among men who have sex with men.
# 7/7/14 -- Neal Goldstein
#################


### FUNCTIONS ###

library(psych) #describe, describeBy
library(gmodels) #CrossTable
library(car) #vif
library(survey) #complex survey sampling


### READ DATA ###

load("NESARCw2.RData")


### ANALYSIS: DESCRIPTIVES, black ###

describe(NESARCw2$age)
CrossTable(NESARCw2$nationality)
CrossTable(NESARCw2$education)
CrossTable(NESARCw2$income)
CrossTable(NESARCw2$homeless_hx)
CrossTable(NESARCw2$kids)
CrossTable(NESARCw2$sex_identity)
CrossTable(NESARCw2$relationship)
CrossTable(NESARCw2$sex_yr)
CrossTable(NESARCw2$sex_men)
CrossTable(NESARCw2$hiv_aids)
CrossTable(NESARCw2$std)
CrossTable(NESARCw2$abuse_sexual)
CrossTable(NESARCw2$abuse_physical)
CrossTable(NESARCw2$alcohol)
CrossTable(NESARCw2$alcohol_sex)
CrossTable(NESARCw2$drug_narcotic)
CrossTable(NESARCw2$drug_stimulant)
CrossTable(NESARCw2$drug_depressant)
CrossTable(NESARCw2$drug_hallucinogen)
CrossTable(NESARCw2$recent_drug)


### ANALYSIS: DESCRIPTIVES, non black ###

describe(NESARCw2_nonblack$age)
CrossTable(NESARCw2_nonblack$nationality)
CrossTable(NESARCw2_nonblack$education)
CrossTable(NESARCw2_nonblack$income)
CrossTable(NESARCw2_nonblack$homeless_hx)
CrossTable(NESARCw2_nonblack$kids)
CrossTable(NESARCw2_nonblack$sex_identity)
CrossTable(NESARCw2_nonblack$relationship)
CrossTable(NESARCw2_nonblack$sex_yr)
CrossTable(NESARCw2_nonblack$sex_men)
CrossTable(NESARCw2_nonblack$hiv_aids)
CrossTable(NESARCw2_nonblack$std)
CrossTable(NESARCw2_nonblack$abuse_sexual)
CrossTable(NESARCw2_nonblack$abuse_physical)
CrossTable(NESARCw2_nonblack$alcohol)
CrossTable(NESARCw2_nonblack$alcohol_sex)
CrossTable(NESARCw2_nonblack$drug_narcotic)
CrossTable(NESARCw2_nonblack$drug_stimulant)
CrossTable(NESARCw2_nonblack$drug_depressant)
CrossTable(NESARCw2_nonblack$drug_hallucinogen)


### ANALYSIS: DESCRIPTIVES by MSM ###

describeBy(NESARCw2$age, NESARCw2$sex_men); t.test(NESARCw2$age ~ NESARCw2$sex_men)
CrossTable(NESARCw2$nationality, NESARCw2$sex_men, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(NESARCw2$education, NESARCw2$sex_men, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(NESARCw2$income, NESARCw2$sex_men, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(NESARCw2$homeless_hx, NESARCw2$sex_men, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(NESARCw2$kids, NESARCw2$sex_men, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(NESARCw2$sex_identity, NESARCw2$sex_men, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(NESARCw2$relationship, NESARCw2$sex_men, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(NESARCw2$sex_yr, NESARCw2$sex_men, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(NESARCw2$hiv_aids, NESARCw2$sex_men, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(NESARCw2$std, NESARCw2$sex_men, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(NESARCw2$abuse_sexual, NESARCw2$sex_men, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(NESARCw2$abuse_physical, NESARCw2$sex_men, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(NESARCw2$alcohol, NESARCw2$sex_men, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(NESARCw2$alcohol_sex, NESARCw2$sex_men, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(NESARCw2$drug_narcotic, NESARCw2$sex_men, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(NESARCw2$drug_stimulant, NESARCw2$sex_men, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(NESARCw2$drug_depressant, NESARCw2$sex_men, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(NESARCw2$drug_hallucinogen, NESARCw2$sex_men, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)


### ANALYSIS: DESCRIPTIVES by sex identity ###

describeBy(NESARCw2$age, NESARCw2$sex_identity_gay); t.test(NESARCw2$age ~ NESARCw2$sex_identity_gay)
CrossTable(NESARCw2$nationality, NESARCw2$sex_identity_gay, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(NESARCw2$education, NESARCw2$sex_identity_gay, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(NESARCw2$income, NESARCw2$sex_identity_gay, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(NESARCw2$homeless_hx, NESARCw2$sex_identity_gay, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(NESARCw2$kids, NESARCw2$sex_identity_gay, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(NESARCw2$sex_identity, NESARCw2$sex_identity_gay, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(NESARCw2$relationship, NESARCw2$sex_identity_gay, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(NESARCw2$sex_yr, NESARCw2$sex_identity_gay, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(NESARCw2$hiv_aids, NESARCw2$sex_identity_gay, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(NESARCw2$std, NESARCw2$sex_identity_gay, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(NESARCw2$abuse_sexual, NESARCw2$sex_identity_gay, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(NESARCw2$abuse_physical, NESARCw2$sex_identity_gay, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(NESARCw2$alcohol, NESARCw2$sex_identity_gay, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(NESARCw2$alcohol_sex, NESARCw2$sex_identity_gay, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(NESARCw2$drug_narcotic, NESARCw2$sex_identity_gay, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(NESARCw2$drug_stimulant, NESARCw2$sex_identity_gay, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(NESARCw2$drug_depressant, NESARCw2$sex_identity_gay, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(NESARCw2$drug_hallucinogen, NESARCw2$sex_identity_gay, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)


### ANALYSIS: POSTERIOR CONFOUNDER SELECTION ###

#crude
summary(glm(hiv_aids~as.factor(sex_men),data=NESARCw2,family=binomial(link="logit")))

#check for associations with hiv_aids, p<0.10
#summary(glm(hiv_aids~age,data=NESARCw2,family=binomial(link="logit")))
#summary(glm(hiv_aids~as.factor(nationality),data=NESARCw2,family=binomial(link="logit")))
#summary(glm(hiv_aids~as.factor(education),data=NESARCw2,family=binomial(link="logit")))
summary(glm(hiv_aids~as.factor(income),data=NESARCw2,family=binomial(link="logit")))
summary(glm(hiv_aids~as.factor(homeless_hx),data=NESARCw2,family=binomial(link="logit")))
summary(glm(hiv_aids~as.factor(kids),data=NESARCw2,family=binomial(link="logit")))
summary(glm(hiv_aids~as.factor(relationship),data=NESARCw2,family=binomial(link="logit")))
#summary(glm(hiv_aids~as.factor(sex_yr),data=NESARCw2,family=binomial(link="logit")))
summary(glm(hiv_aids~as.factor(std),data=NESARCw2,family=binomial(link="logit")))
summary(glm(hiv_aids~as.factor(abuse_sexual),data=NESARCw2,family=binomial(link="logit")))
#summary(glm(hiv_aids~as.factor(abuse_physical),data=NESARCw2,family=binomial(link="logit")))
#summary(glm(hiv_aids~as.factor(alcohol),data=NESARCw2,family=binomial(link="logit")))
#summary(glm(hiv_aids~as.factor(alcohol_sex),data=NESARCw2,family=binomial(link="logit")))
summary(glm(hiv_aids~as.factor(drug_narcotic),data=NESARCw2,family=binomial(link="logit")))
summary(glm(hiv_aids~as.factor(drug_stimulant),data=NESARCw2,family=binomial(link="logit")))
summary(glm(hiv_aids~as.factor(drug_depressant),data=NESARCw2,family=binomial(link="logit")))
summary(glm(hiv_aids~as.factor(drug_hallucinogen),data=NESARCw2,family=binomial(link="logit")))

#check for associations with sex_men, p<0.10 and p<0.10 with hiv_aids
#summary(glm(sex_men~age,data=NESARCw2,family=binomial(link="logit")))
#summary(glm(sex_men~as.factor(nationality),data=NESARCw2,family=binomial(link="logit")))
#summary(glm(sex_men~as.factor(education),data=NESARCw2,family=binomial(link="logit")))
#summary(glm(sex_men~as.factor(income),data=NESARCw2,family=binomial(link="logit")))
summary(glm(sex_men~as.factor(homeless_hx),data=NESARCw2,family=binomial(link="logit")))
summary(glm(sex_men~as.factor(kids),data=NESARCw2,family=binomial(link="logit")))
summary(glm(sex_men~as.factor(relationship),data=NESARCw2,family=binomial(link="logit")))
#summary(glm(sex_men~as.factor(sex_yr),data=NESARCw2,family=binomial(link="logit")))
summary(glm(sex_men~as.factor(std),data=NESARCw2,family=binomial(link="logit")))
summary(glm(sex_men~as.factor(abuse_sexual),data=NESARCw2,family=binomial(link="logit")))
#summary(glm(sex_men~as.factor(abuse_physical),data=NESARCw2,family=binomial(link="logit")))
#summary(glm(sex_men~as.factor(alcohol),data=NESARCw2,family=binomial(link="logit")))
#summary(glm(sex_men~as.factor(alcohol_sex),data=NESARCw2,family=binomial(link="logit")))
#summary(glm(sex_men~as.factor(drug_narcotic),data=NESARCw2,family=binomial(link="logit")))
summary(glm(sex_men~as.factor(drug_stimulant),data=NESARCw2,family=binomial(link="logit")))
summary(glm(sex_men~as.factor(drug_depressant),data=NESARCw2,family=binomial(link="logit")))
summary(glm(sex_men~as.factor(drug_hallucinogen),data=NESARCw2,family=binomial(link="logit")))

#full model with potential confounders
summary(glm(hiv_aids~as.factor(sex_men)+as.factor(homeless_hx)+as.factor(kids)+as.factor(relationship)+as.factor(std)+as.factor(abuse_sexual)+as.factor(drug_stimulant)+as.factor(drug_depressant)+as.factor(drug_hallucinogen),data=NESARCw2,family=binomial(link="logit")))

#check for multicollinearity, VIF>=10
vif(glm(hiv_aids~as.factor(sex_men)+as.factor(homeless_hx)+as.factor(kids)+as.factor(relationship)+as.factor(std)+as.factor(abuse_sexual)+as.factor(drug_stimulant)+as.factor(drug_depressant)+as.factor(drug_hallucinogen),data=NESARCw2,family=binomial(link="logit")))

#backward remove nonsignificant vars and check for change in estimate >10%
summary(glm(hiv_aids~as.factor(sex_men)+as.factor(kids)+as.factor(relationship)+as.factor(std)+as.factor(abuse_sexual)+as.factor(drug_stimulant)+as.factor(drug_depressant)+as.factor(drug_hallucinogen),data=NESARCw2,family=binomial(link="logit")))
summary(glm(hiv_aids~as.factor(sex_men)+as.factor(relationship)+as.factor(std)+as.factor(abuse_sexual)+as.factor(drug_stimulant)+as.factor(drug_depressant)+as.factor(drug_hallucinogen),data=NESARCw2,family=binomial(link="logit")))
summary(glm(hiv_aids~as.factor(sex_men)+as.factor(relationship)+as.factor(std)+as.factor(abuse_sexual)+as.factor(drug_depressant)+as.factor(drug_hallucinogen),data=NESARCw2,family=binomial(link="logit")))
summary(glm(hiv_aids~as.factor(sex_men)+as.factor(relationship)+as.factor(std)+as.factor(abuse_sexual)+as.factor(drug_hallucinogen),data=NESARCw2,family=binomial(link="logit")))
summary(glm(hiv_aids~as.factor(sex_men)+as.factor(relationship)+as.factor(std)+as.factor(abuse_sexual)+as.factor(drug_hallucinogen),data=NESARCw2,family=binomial(link="logit")))

#final model, partner, black
model = glm(hiv_aids~as.factor(sex_men)+as.factor(relationship)+as.factor(std)+as.factor(abuse_sexual)+as.factor(recent_drug),data=NESARCw2,family=binomial(link="logit"))
summary(model)
round(exp(coef(model)),2)
round(exp(confint(model)),2)

#final model, identity, black
model = glm(hiv_aids~as.factor(sex_identity_gay)+as.factor(relationship)+as.factor(std)+as.factor(abuse_sexual)+as.factor(recent_drug),data=NESARCw2,family=binomial(link="logit"))
summary(model)
round(exp(coef(model)),2)
round(exp(confint(model)),2)

# #final model, partner, nonblack
# model = glm(hiv_aids~as.factor(sex_men)+as.factor(relationship)+as.factor(std)+as.factor(abuse_sexual)+as.factor(drug_hallucinogen),data=NESARCw2_nonblack,family=binomial(link="logit"))
# summary(model)
# round(exp(coef(model)),2)
# round(exp(confint(model)),2)
# 
# #final model, identity, nonblack
# model = glm(hiv_aids~as.factor(sex_identity_gay)+as.factor(relationship)+as.factor(std)+as.factor(abuse_sexual)+as.factor(drug_hallucinogen),data=NESARCw2_nonblack,family=binomial(link="logit"))
# summary(model)
# round(exp(coef(model)),2)
# round(exp(confint(model)),2)


### COMPLEX SURVEY ESTIMATES ###

#black

#some strata may have one sampled unit: http://r-survey.r-forge.r-project.org/survey/example-twostage.html
options(survey.lonely.psu="remove")

#specify survey design
NESARCw2_complex = svydesign(id=~psu, strata=~stratum, weights=~weight, data=NESARCw2)

#logistic regression using quasibinomial per: http://stackoverflow.com/questions/12953045/warning-non-integer-successes-in-a-binomial-glm-survey-packages
#partner
model = svyglm(hiv_aids~as.factor(sex_men)+as.factor(relationship)+as.factor(std)+as.factor(abuse_sexual)+as.factor(recent_drug), family=quasibinomial, design=NESARCw2_complex)
summary(model)
round(exp(coef(model)),2)
round(exp(confint(model)),2)
AIC(model)

#identity
model = svyglm(hiv_aids~as.factor(sex_identity_gay)+as.factor(relationship)+as.factor(std)+as.factor(abuse_sexual)+as.factor(recent_drug), family=quasibinomial, design=NESARCw2_complex)
summary(model)
round(exp(coef(model)),2)
round(exp(confint(model)),2)
AIC(model)
# 
# #nonblack
# 
# #some strata may have one sampled unit: http://r-survey.r-forge.r-project.org/survey/example-twostage.html
# options(survey.lonely.psu="remove")
# 
# #specify survey design
# NESARCw2_complex = svydesign(id=~psu, strata=~stratum, weights=~weight, data=NESARCw2_nonblack)
# 
# #logistic regression using quasibinomial per: http://stackoverflow.com/questions/12953045/warning-non-integer-successes-in-a-binomial-glm-survey-packages
# #partner
# model = svyglm(hiv_aids~as.factor(sex_men)+as.factor(relationship)+as.factor(std)+as.factor(abuse_sexual)+as.factor(drug_hallucinogen), family=quasibinomial, design=NESARCw2_complex)
# summary(model)
# round(exp(coef(model)),2)
# round(exp(confint(model)),2)
# AIC(model)
# 
# #identity, black
# model = svyglm(hiv_aids~as.factor(sex_identity_gay)+as.factor(relationship)+as.factor(std)+as.factor(abuse_sexual)+as.factor(drug_hallucinogen), family=quasibinomial, design=NESARCw2_complex)
# summary(model)
# round(exp(coef(model)),2)
# round(exp(confint(model)),2)
# AIC(model)


### PREVALENCE ###

#identity
prev = sum(NESARCw2$sex_identity_gay, na.rm=T)/length(na.omit(NESARCw2$sex_identity_gay))
prev_ci = 1.96 * sqrt((prev*(1-prev))/length(na.omit(NESARCw2$sex_identity_gay)))

#partner
prev = sum(NESARCw2$sex_men, na.rm=T)/length(na.omit(NESARCw2$sex_men))
prev_ci = 1.96 * sqrt((prev*(1-prev))/length(na.omit(NESARCw2$sex_men)))
