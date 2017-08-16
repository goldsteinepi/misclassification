#################
# BAAMHS analysis
# Citation: Goldstein ND, Welles SL, Burstyn I. To be or not to be: Bayesian correction for misclassification of self-reported sexual behaviors among men who have sex with men.
# 7/7/14 -- Neal Goldstein
#################


### FUNCTIONS ###

library(psych) #describe, describeBy
library(gmodels) #CrossTable
library(car) #vif


### READ DATA ###

load("BAAMHS.RData")


### ANALYSIS: DESCRIPTIVES ###

describe(BAAMHS$age)
CrossTable(BAAMHS$nationality)
CrossTable(BAAMHS$education)
CrossTable(BAAMHS$income)
CrossTable(BAAMHS$homeless_hx)
CrossTable(BAAMHS$kids)
CrossTable(BAAMHS$sex_identity)
CrossTable(BAAMHS$relationship)
CrossTable(BAAMHS$sex_yr)
CrossTable(BAAMHS$sex_men)
CrossTable(BAAMHS$hiv_aids)
CrossTable(BAAMHS$std)
CrossTable(BAAMHS$abuse_sexual)
CrossTable(BAAMHS$abuse_physical)
CrossTable(BAAMHS$alcohol)
CrossTable(BAAMHS$alcohol_sex)
CrossTable(BAAMHS$drug_narcotic)
CrossTable(BAAMHS$drug_stimulant)
CrossTable(BAAMHS$drug_depressant)
CrossTable(BAAMHS$drug_hallucinogen)
CrossTable(BAAMHS$recent_drug)

CrossTable(BAAMHS$any_anal_receptive)
CrossTable(BAAMHS$any_oral_receptive)
CrossTable(BAAMHS$any_anal_insertive)
CrossTable(BAAMHS$any_oral_insertive)
CrossTable(BAAMHS$any_receptive)
CrossTable(BAAMHS$any_insertive)
CrossTable(BAAMHS$any_anal)
CrossTable(BAAMHS$any_oral)
CrossTable(BAAMHS$any_msm_behavior)


### ANALYSIS: DESCRIPTIVES by MSM behavior ###

CrossTable(BAAMHS$sex_identity, BAAMHS$any_msm_behavior, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(BAAMHS$sex_men, BAAMHS$any_msm_behavior, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
phi(table(BAAMHS$sex_men, BAAMHS$any_msm_behavior))
CrossTable(BAAMHS$any_anal_receptive, BAAMHS$any_msm_behavior, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
phi(table(BAAMHS$any_anal_receptive, BAAMHS$any_msm_behavior))
CrossTable(BAAMHS$any_oral_receptive, BAAMHS$any_msm_behavior, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
phi(table(BAAMHS$any_oral_receptive, BAAMHS$any_msm_behavior))
CrossTable(BAAMHS$any_anal_insertive, BAAMHS$any_msm_behavior, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
phi(table(BAAMHS$any_anal_insertive, BAAMHS$any_msm_behavior))
CrossTable(BAAMHS$any_oral_insertive, BAAMHS$any_msm_behavior, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
phi(table(BAAMHS$any_oral_insertive, BAAMHS$any_msm_behavior))
CrossTable(BAAMHS$any_receptive, BAAMHS$any_msm_behavior, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
phi(table(BAAMHS$any_receptive, BAAMHS$any_msm_behavior))
CrossTable(BAAMHS$any_insertive, BAAMHS$any_msm_behavior, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
phi(table(BAAMHS$any_insertive, BAAMHS$any_msm_behavior))
CrossTable(BAAMHS$any_anal, BAAMHS$any_msm_behavior, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
phi(table(BAAMHS$any_anal, BAAMHS$any_msm_behavior))
CrossTable(BAAMHS$any_oral, BAAMHS$any_msm_behavior, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
phi(table(BAAMHS$any_oral, BAAMHS$any_msm_behavior))

CrossTable(BAAMHS$hiv_aids, BAAMHS$any_msm_behavior, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(BAAMHS$sex_men[BAAMHS$hiv_aids==0], BAAMHS$any_msm_behavior[BAAMHS$hiv_aids==0], prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(BAAMHS$sex_identity_gay[BAAMHS$hiv_aids==0], BAAMHS$any_msm_behavior[BAAMHS$hiv_aids==0], prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(BAAMHS$sex_men[BAAMHS$hiv_aids==1], BAAMHS$any_msm_behavior[BAAMHS$hiv_aids==1], prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(BAAMHS$sex_identity_gay[BAAMHS$hiv_aids==1], BAAMHS$any_msm_behavior[BAAMHS$hiv_aids==1], prop.r=F, prop.t=F, prop.chisq=F, chisq=T)


### ANALYSIS: DESCRIPTIVES by MSM behavior ###

CrossTable(BAAMHS$sex_identity, BAAMHS$sex_men, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(BAAMHS$any_anal_receptive, BAAMHS$sex_men, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
phi(table(BAAMHS$any_anal_receptive, BAAMHS$sex_men))
CrossTable(BAAMHS$any_oral_receptive, BAAMHS$sex_men, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
phi(table(BAAMHS$any_oral_receptive, BAAMHS$sex_men))
CrossTable(BAAMHS$any_anal_insertive, BAAMHS$sex_men, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
phi(table(BAAMHS$any_anal_insertive, BAAMHS$sex_men))
CrossTable(BAAMHS$any_oral_insertive, BAAMHS$sex_men, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
phi(table(BAAMHS$any_oral_insertive, BAAMHS$sex_men))
CrossTable(BAAMHS$any_receptive, BAAMHS$sex_men, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
phi(table(BAAMHS$any_receptive, BAAMHS$sex_men))
CrossTable(BAAMHS$any_insertive, BAAMHS$sex_men, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
phi(table(BAAMHS$any_insertive, BAAMHS$sex_men))
CrossTable(BAAMHS$any_anal, BAAMHS$sex_men, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
phi(table(BAAMHS$any_anal, BAAMHS$sex_men))
CrossTable(BAAMHS$any_oral, BAAMHS$sex_men, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
phi(table(BAAMHS$any_oral, BAAMHS$sex_men))
CrossTable(BAAMHS$any_msm_behavior, BAAMHS$sex_men, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
phi(table(BAAMHS$any_msm_behavior, BAAMHS$sex_men))


### ANALYSIS: DESCRIPTIVES by MSM ###

describeBy(BAAMHS$age, BAAMHS$sex_men); t.test(BAAMHS$age ~ BAAMHS$sex_men)
CrossTable(BAAMHS$nationality, BAAMHS$sex_men, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(BAAMHS$education, BAAMHS$sex_men, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(BAAMHS$income, BAAMHS$sex_men, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(BAAMHS$homeless_hx, BAAMHS$sex_men, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(BAAMHS$kids, BAAMHS$sex_men, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(BAAMHS$sex_identity, BAAMHS$sex_men, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(BAAMHS$relationship, BAAMHS$sex_men, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(BAAMHS$sex_yr, BAAMHS$sex_men, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(BAAMHS$hiv_aids, BAAMHS$sex_men, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(BAAMHS$std, BAAMHS$sex_men, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(BAAMHS$abuse_sexual, BAAMHS$sex_men, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(BAAMHS$abuse_physical, BAAMHS$sex_men, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(BAAMHS$alcohol, BAAMHS$sex_men, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(BAAMHS$alcohol_sex, BAAMHS$sex_men, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(BAAMHS$drug_narcotic, BAAMHS$sex_men, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(BAAMHS$drug_stimulant, BAAMHS$sex_men, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(BAAMHS$drug_depressant, BAAMHS$sex_men, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(BAAMHS$drug_hallucinogen, BAAMHS$sex_men, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)


### ANALYSIS: DESCRIPTIVES by sex identity ###

describeBy(BAAMHS$age, BAAMHS$sex_identity_gay); t.test(BAAMHS$age ~ BAAMHS$sex_identity_gay)
CrossTable(BAAMHS$nationality, BAAMHS$sex_identity_gay, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(BAAMHS$education, BAAMHS$sex_identity_gay, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(BAAMHS$income, BAAMHS$sex_identity_gay, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(BAAMHS$homeless_hx, BAAMHS$sex_identity_gay, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(BAAMHS$kids, BAAMHS$sex_identity_gay, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(BAAMHS$sex_identity, BAAMHS$sex_identity_gay, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(BAAMHS$relationship, BAAMHS$sex_identity_gay, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(BAAMHS$sex_yr, BAAMHS$sex_identity_gay, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(BAAMHS$hiv_aids, BAAMHS$sex_identity_gay, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(BAAMHS$std, BAAMHS$sex_identity_gay, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(BAAMHS$abuse_sexual, BAAMHS$sex_identity_gay, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(BAAMHS$abuse_physical, BAAMHS$sex_identity_gay, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(BAAMHS$alcohol, BAAMHS$sex_identity_gay, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(BAAMHS$alcohol_sex, BAAMHS$sex_identity_gay, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(BAAMHS$drug_narcotic, BAAMHS$sex_identity_gay, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(BAAMHS$drug_stimulant, BAAMHS$sex_identity_gay, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(BAAMHS$drug_depressant, BAAMHS$sex_identity_gay, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(BAAMHS$drug_hallucinogen, BAAMHS$sex_identity_gay, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)


### ANALYSIS: ADJUSTED INFOMATIVE PRIOR ###

#crude
model = glm(hiv_aids~as.factor(any_msm_behavior),data=BAAMHS,family=binomial(link="logit"))
summary(model)
round(coef(model),2)
#variance is standard error (apprxs the standard deviation) squared
round(0.3851*0.3851,2)

#check for associations with hiv_aids, p<0.10
summary(glm(hiv_aids~age,data=BAAMHS,family=binomial(link="logit")))
#summary(glm(hiv_aids~as.factor(nationality),data=BAAMHS,family=binomial(link="logit")))
#summary(glm(hiv_aids~as.factor(education),data=BAAMHS,family=binomial(link="logit")))
summary(glm(hiv_aids~as.factor(income),data=BAAMHS,family=binomial(link="logit")))
summary(glm(hiv_aids~as.factor(homeless_hx),data=BAAMHS,family=binomial(link="logit")))
summary(glm(hiv_aids~as.factor(kids),data=BAAMHS,family=binomial(link="logit")))
#summary(glm(hiv_aids~as.factor(religion),data=BAAMHS,family=binomial(link="logit")))
#summary(glm(hiv_aids~as.factor(relationship),data=BAAMHS,family=binomial(link="logit")))
summary(glm(hiv_aids~as.factor(std),data=BAAMHS,family=binomial(link="logit")))
summary(glm(hiv_aids~as.factor(abuse_sexual),data=BAAMHS,family=binomial(link="logit")))
summary(glm(hiv_aids~as.factor(abuse_physical),data=BAAMHS,family=binomial(link="logit")))
#summary(glm(hiv_aids~as.factor(alcohol),data=BAAMHS,family=binomial(link="logit")))
summary(glm(hiv_aids~as.factor(alcohol_sex),data=BAAMHS,family=binomial(link="logit")))
summary(glm(hiv_aids~as.factor(drug_narcotic),data=BAAMHS,family=binomial(link="logit")))
summary(glm(hiv_aids~as.factor(drug_stimulant),data=BAAMHS,family=binomial(link="logit")))
#summary(glm(hiv_aids~as.factor(drug_depressant),data=BAAMHS,family=binomial(link="logit")))
summary(glm(hiv_aids~as.factor(drug_hallucinogen),data=BAAMHS,family=binomial(link="logit")))

#check for associations with any_msm_behavior, p<0.10 and p<0.10 with hiv_aids
summary(glm(any_msm_behavior~age,data=BAAMHS,family=binomial(link="logit")))
#summary(glm(any_msm_behavior~as.factor(nationality),data=BAAMHS,family=binomial(link="logit")))
#summary(glm(any_msm_behavior~as.factor(education),data=BAAMHS,family=binomial(link="logit")))
summary(glm(any_msm_behavior~as.factor(income),data=BAAMHS,family=binomial(link="logit")))
summary(glm(any_msm_behavior~as.factor(homeless_hx),data=BAAMHS,family=binomial(link="logit")))
#summary(glm(any_msm_behavior~as.factor(kids),data=BAAMHS,family=binomial(link="logit")))
#summary(glm(any_msm_behavior~as.factor(religion),data=BAAMHS,family=binomial(link="logit")))
#summary(glm(any_msm_behavior~as.factor(relationship),data=BAAMHS,family=binomial(link="logit")))
summary(glm(any_msm_behavior~as.factor(std),data=BAAMHS,family=binomial(link="logit")))
summary(glm(any_msm_behavior~as.factor(abuse_sexual),data=BAAMHS,family=binomial(link="logit")))
summary(glm(any_msm_behavior~as.factor(abuse_physical),data=BAAMHS,family=binomial(link="logit")))
#summary(glm(any_msm_behavior~as.factor(alcohol),data=BAAMHS,family=binomial(link="logit")))
summary(glm(any_msm_behavior~as.factor(alcohol_sex),data=BAAMHS,family=binomial(link="logit")))
summary(glm(any_msm_behavior~as.factor(drug_narcotic),data=BAAMHS,family=binomial(link="logit")))
summary(glm(any_msm_behavior~as.factor(drug_stimulant),data=BAAMHS,family=binomial(link="logit")))
#summary(glm(any_msm_behavior~as.factor(drug_depressant),data=BAAMHS,family=binomial(link="logit")))
summary(glm(any_msm_behavior~as.factor(drug_hallucinogen),data=BAAMHS,family=binomial(link="logit")))

#full model with potential confounders
summary(glm(hiv_aids~as.factor(any_msm_behavior)+age+as.factor(income)+as.factor(homeless_hx)+as.factor(std)+as.factor(abuse_sexual)+as.factor(abuse_physical)+as.factor(alcohol_sex)+as.factor(drug_narcotic)+as.factor(drug_stimulant)+as.factor(drug_hallucinogen),data=BAAMHS,family=binomial(link="logit")))

#check for multicollinearity, VIF>=10
vif(glm(hiv_aids~as.factor(any_msm_behavior)+age+as.factor(income)+as.factor(homeless_hx)+as.factor(std)+as.factor(abuse_sexual)+as.factor(abuse_physical)+as.factor(alcohol_sex)+as.factor(drug_narcotic)+as.factor(drug_stimulant)+as.factor(drug_hallucinogen),data=BAAMHS,family=binomial(link="logit")))

#backward remove nonsignificant vars and check for change in estimate >10%
summary(glm(hiv_aids~as.factor(any_msm_behavior)+as.factor(income)+as.factor(homeless_hx)+as.factor(std)+as.factor(abuse_sexual)+as.factor(abuse_physical)+as.factor(alcohol_sex)+as.factor(drug_narcotic)+as.factor(drug_stimulant)+as.factor(drug_hallucinogen),data=BAAMHS,family=binomial(link="logit")))
summary(glm(hiv_aids~as.factor(any_msm_behavior)+as.factor(homeless_hx)+as.factor(std)+as.factor(abuse_sexual)+as.factor(abuse_physical)+as.factor(alcohol_sex)+as.factor(drug_narcotic)+as.factor(drug_stimulant)+as.factor(drug_hallucinogen),data=BAAMHS,family=binomial(link="logit")))
summary(glm(hiv_aids~as.factor(any_msm_behavior)+as.factor(homeless_hx)+as.factor(std)+as.factor(abuse_sexual)+as.factor(alcohol_sex)+as.factor(drug_narcotic)+as.factor(drug_stimulant)+as.factor(drug_hallucinogen),data=BAAMHS,family=binomial(link="logit")))
summary(glm(hiv_aids~as.factor(any_msm_behavior)+as.factor(homeless_hx)+as.factor(std)+as.factor(abuse_sexual)+as.factor(drug_narcotic)+as.factor(drug_stimulant)+as.factor(drug_hallucinogen),data=BAAMHS,family=binomial(link="logit")))
summary(glm(hiv_aids~as.factor(any_msm_behavior)+as.factor(homeless_hx)+as.factor(std)+as.factor(abuse_sexual)+as.factor(drug_stimulant)+as.factor(drug_hallucinogen),data=BAAMHS,family=binomial(link="logit")))
summary(glm(hiv_aids~as.factor(any_msm_behavior)+as.factor(homeless_hx)+as.factor(std)+as.factor(abuse_sexual)+as.factor(drug_hallucinogen),data=BAAMHS,family=binomial(link="logit")))
summary(glm(hiv_aids~as.factor(any_msm_behavior)+as.factor(homeless_hx)+as.factor(std)+as.factor(abuse_sexual),data=BAAMHS,family=binomial(link="logit")))

#final model
model = glm(hiv_aids~as.factor(any_msm_behavior)+as.factor(homeless_hx)+as.factor(std)+as.factor(abuse_sexual),data=BAAMHS,family=binomial(link="logit"))
summary(model)
round(coef(model),2)
#variance is standard error (apprxs the standard deviation) squared
round(0.4379*0.4379,2)

round(exp(coef(model)),2)
round(exp(confint(model)),2)

#final model w/ same covariates from NESARC
model = glm(hiv_aids~as.factor(any_msm_behavior)+as.factor(relationship)+as.factor(std)+as.factor(abuse_sexual)+as.factor(recent_drug),data=BAAMHS,family=binomial(link="logit"))
summary(model)
round(coef(model),2)
#variance is standard error (apprxs the standard deviation) squared
round(0.44108*0.44108,2)

round(exp(coef(model)),2)
round(exp(confint(model)),2)


### PREVALENCE ###

#gold standard
prev = sum(BAAMHS$any_msm_behavior, na.rm=T)/length(na.omit(BAAMHS$any_msm_behavior))
prev_ci = 1.96 * sqrt((prev*(1-prev))/length(na.omit(BAAMHS$any_msm_behavior)))

#identity
prev = sum(BAAMHS$sex_identity_gay, na.rm=T)/length(na.omit(BAAMHS$sex_identity_gay))
prev_ci = 1.96 * sqrt((prev*(1-prev))/length(na.omit(BAAMHS$sex_identity_gay)))

#partner
prev = sum(BAAMHS$sex_men, na.rm=T)/length(na.omit(BAAMHS$sex_men))
prev_ci = 1.96 * sqrt((prev*(1-prev))/length(na.omit(BAAMHS$sex_men)))

