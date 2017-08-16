#################
# Bayesian inference on NESARC data using BAAHMS priors
# Citation: Goldstein ND, Welles SL, Burstyn I. To be or not to be: Bayesian correction for misclassification of self-reported sexual behaviors among men who have sex with men.
# 8/1/14 -- Neal Goldstein
#################


### FUNCTIONS ###

library("rjags")
library("dclone") #parallel mcmc
library("boot") #bootstrapping

#boot strap prevalence of MSM from naive data
#called from boot with arguments dataframe, index of samples to pull, and model to run
bootPrev = function(data, index, indicator)
{
  newdata = data[index,]
  if (indicator=="identity")
  {
    return(sum(newdata$sex_identity_gay, na.rm=T)/length(na.omit(newdata$sex_identity_gay)))
  }
  else
  {
    return(sum(newdata$sex_men, na.rm=T)/length(na.omit(newdata$sex_men)))
  }
}
  

### READ DATA ###

load("NESARCw2.RData")


### BUGS MODEL ###

#non-differential
bugs_model =
"model {
  for (i in 1:n) {
    
    #outcome model, log odds of hiv_aids given these predictors
    hiv_aids[i] ~ dbern(p_hiv_aids[i])
    logit(p_hiv_aids[i]) <- b0+b1*msm[i]+b2*relationship[i]+b3*std[i]+b4*abuse_sexual[i]+b5*recent_drug[i]

    #exposure models, log odds of true msm status given these predictors
    msm[i] ~ dbern(p_msm[i])
    logit(p_msm[i]) <- a0+a1*relationship[i]+a2*std[i]+a3*abuse_sexual[i]+a4*recent_drug[i]

    #measurement model, imputing the true msm status given the measurement error
    msm.star[i] ~ dbern(p_msm.star[i])
    p_msm.star[i] <- sn.msm*msm[i]+(1-msm[i])*(1-sp.msm)

    #prevalence models of potential confounders
    relationship[i] ~ dbern(p_relationship[i])
    logit(p_relationship[i]) <- prev.relationship

    std[i] ~ dbern(p_std[i])
    logit(p_std[i]) <- prev.std

    abuse_sexual[i] ~ dbern(p_abuse_sexual[i])
    logit(p_abuse_sexual[i]) <- prev.abuse_sexual

    recent_drug[i] ~ dbern(p_recent_drug[i])
    logit(p_recent_drug[i]) <- prev.recent_drug

  }

  #priors
  #for normal distribution, provide (mean, precision=(1/variance))
  #for beta distribution, provide (alpha, beta)

  b0 ~ dnorm(0,1/10)
  b1 ~ dnorm(1.65,1/0.19)
  b2 ~ dnorm(0,1/10)
  b3 ~ dnorm(0,1/10)
  b4 ~ dnorm(0,1/10)
  b5 ~ dnorm(0,1/10)
  a0 ~ dnorm(0,1/10)
  a1 ~ dnorm(0,1/10)
  a2 ~ dnorm(0,1/10)
  a3 ~ dnorm(0,1/10)
  a4 ~ dnorm(0,1/10)
  prev.relationship ~ dnorm(0,1/10)
  prev.std ~ dnorm(0,1/10)
  prev.abuse_sexual ~ dnorm(0,1/10)
  prev.recent_drug ~ dnorm(0,1/10)
  sn.msm ~ dbeta(68,7) #add beta(1,1)
  sp.msm ~ dbeta(534,17) #add beta(1,1)
}"

#differential
bugs_model =
"model {
  for (i in 1:n) {
    
    #outcome model, log odds of hiv_aids given these predictors
    hiv_aids[i] ~ dbern(p_hiv_aids[i])
    logit(p_hiv_aids[i]) <- b0+b1*msm[i]+b2*relationship[i]+b3*std[i]+b4*abuse_sexual[i]+b5*recent_drug[i]

    #exposure models, log odds of true msm status given these predictors
    msm[i] ~ dbern(p_msm[i])
    logit(p_msm[i]) <- a0+a1*relationship[i]+a2*std[i]+a3*abuse_sexual[i]+a4*recent_drug[i]

    #measurement model, imputing the true msm status given the measurement error
    msm.star[i] ~ dbern(p_msm.star[i])
    p_msm.star[i] <- sn.msm.hivneg*msm[i]*(1-hiv_aids[i])+(1-msm[i])*(1-sp.msm.hivneg)*(1-hiv_aids[i]) + sn.msm.hivpos*msm[i]*(hiv_aids[i])+(1-msm[i])*(1-sp.msm.hivpos)*(hiv_aids[i])

    #prevalence models of potential confounders
    relationship[i] ~ dbern(p_relationship[i])
    logit(p_relationship[i]) <- prev.relationship

    std[i] ~ dbern(p_std[i])
    logit(p_std[i]) <- prev.std

    abuse_sexual[i] ~ dbern(p_abuse_sexual[i])
    logit(p_abuse_sexual[i]) <- prev.abuse_sexual

    recent_drug[i] ~ dbern(p_recent_drug[i])
    logit(p_recent_drug[i]) <- prev.recent_drug

  }

  #priors
  #for normal distribution, provide (mean, precision=(1/variance))
  #for beta distribution, provide (alpha, beta)

  b0 ~ dnorm(0,1/10)
  b1 ~ dnorm(0,1/10) #uninformative prior
  #b1 ~ dnorm(1.65,1/0.19)
  b2 ~ dnorm(0,1/10)
  b3 ~ dnorm(0,1/10)
  b4 ~ dnorm(0,1/10)
  b5 ~ dnorm(0,1/10)
  a0 ~ dnorm(0,1/10)
  a1 ~ dnorm(0,1/10)
  a2 ~ dnorm(0,1/10)
  a3 ~ dnorm(0,1/10)
  a4 ~ dnorm(0,1/10)
  prev.relationship ~ dnorm(0,1/10)
  prev.std ~ dnorm(0,1/10)
  prev.abuse_sexual ~ dnorm(0,1/10)
  prev.recent_drug ~ dnorm(0,1/10)
  #sn.msm.hivneg ~ dbeta(41,18) #identity, add beta(1,1)
  #sp.msm.hivneg ~ dbeta(488,12) #identity, add beta(1,1)
  #sn.msm.hivpos ~ dbeta(12,5) #identity, add beta(1,1)
  #sp.msm.hivpos ~ dbeta(14,1) #identity, add beta(1,1)
  sn.msm.hivneg ~ dbeta(54,6) #partner, add beta(1,1)
  sp.msm.hivneg ~ dbeta(518,17) #partner, add beta(1,1)
  sn.msm.hivpos ~ dbeta(15,2) #partner, add beta(1,1)
  sp.msm.hivpos ~ dbeta(17,1) #partner, add beta(1,1)
}"


### COMPLETE CASE ###

NESARCw2_identity = na.omit(NESARCw2[,c("sex_identity_gay","hiv_aids","relationship","std","abuse_sexual","recent_drug")])
NESARCw2_partner = na.omit(NESARCw2[,c("sex_men","hiv_aids","relationship","std","abuse_sexual","recent_drug")])


### BAYESIAN SAMPLING ###

#write bugs model to temp file for JAGS
writeLines(bugs_model, file("bugs_model.txt"))

#initialize cluster for two parallel chains
jags_cluster = makeCluster(2)

#initialize model, identity, black/AA
parJagsModel(jags_cluster, name="res", file="bugs_model.txt",
                   data = list('msm.star' = NESARCw2_identity$sex_identity_gay,
                               'hiv_aids' = NESARCw2_identity$hiv_aids,
                               'relationship' = NESARCw2_identity$relationship,
                               'std' = NESARCw2_identity$std,
                               'abuse_sexual' = NESARCw2_identity$abuse_sexual,
                               'recent_drug' = NESARCw2_identity$recent_drug,
                               'n' = nrow(NESARCw2_identity)),
                   n.chains = 2,
                   n.adapt = 100)

#initialize model, partner, black/AA
parJagsModel(jags_cluster, name="res", file="bugs_model.txt",
             data = list('msm.star' = NESARCw2_partner$sex_men,
                               'hiv_aids' = NESARCw2_partner$hiv_aids,
                               'relationship' = NESARCw2_partner$relationship,
                               'std' = NESARCw2_partner$std,
                               'abuse_sexual' = NESARCw2_partner$abuse_sexual,
                               'recent_drug' = NESARCw2_partner$recent_drug,
                               'n' = nrow(NESARCw2_partner)),
                   n.chains = 2,
                   n.adapt = 100)

#sample from the posterior distribution
#jags_samples = parCodaSamples(jags_cluster, "res", variable.names=c("b1","sn.msm","sp.msm"), n.iter=10000)
jags_samples = parCodaSamples(jags_cluster, "res", variable.names=c("b1","sn.msm.hivneg","sp.msm.hivneg","sn.msm.hivpos","sp.msm.hivpos","msm"), n.iter=10000)

#clean up
stopCluster(jags_cluster)
rm(jags_cluster)
file.remove("bugs_model.txt")


### SAVE POSTERIORS ###

#identity
save.image("Black identity.RData")
load("Black identity.RData")

#partner
save.image("Black partner.RData")
load("Black partner.RData")


### BAYESIAN INFERENCE ###

#check for convergence
plot(jags_samples[,"b1"]) #good mixing and shape of distribution
gelman.plot(jags_samples[,"b1"]) #no discernable difference
gelman.diag(jags_samples) #<1.1

#statistics for model fit, discard first 1000 observations for burn in
summary(window(jags_samples[,c("b1","sn.msm.hivneg","sp.msm.hivneg","sn.msm.hivpos","sp.msm.hivpos")], start=1000))


### PREVALENCE of MSM ###

#black/AA, identity

#obtain MSM status for each individual, for each simulation, in each chain
prevMSM_chain1 = as.data.frame(window(jags_samples[[1]][,2:(nrow(NESARCw2_identity)+1)], start=1000))
prevMSM_chain2 = as.data.frame(window(jags_samples[[2]][,2:(nrow(NESARCw2_identity)+1)], start=1000))

# #apply sampling probabilities
# samp_prob = na.omit(NESARCw2[,c("sex_identity_gay","hiv_aids","relationship","std","abuse_sexual","recent_drug","samp_prob")])
# samp_prob = samp_prob$samp_prob
# for (i in 1:nrow(NESARCw2_identity))
# {
#   prevMSM_chain1[,i] = prevMSM_chain1[,i]*samp_prob[i]
#   prevMSM_chain2[,i] = prevMSM_chain2[,i]*samp_prob[i]
# }
# rm(i,samp_prob)

#tally the total number for prevalance
#prevMSM = c(rowSums(prevMSM_chain1), rowSums(prevMSM_chain2))
prevMSM = c(rowSums(prevMSM_chain1)/nrow(NESARCw2_identity), rowSums(prevMSM_chain2)/nrow(NESARCw2_identity))

#summary of MSM prevalence
mean(prevMSM)
quantile(prevMSM,probs=c(0.025,0.975))

#naive prevalence for plotting
prev_boot = boot(NESARCw2_identity, bootPrev, R=1000, indicator="identity")

#output to a file (then open with gimp, scale to 1200, export as pdf)
tiff("Figure1a.tif",height=4,width=6,units='in',res=1200)

#density plot of MSM prevalence, naive and posterior
plot(density(prev_boot$t), xlim=c(0,0.055), ylim=c(0,155), main="a. MSM behavior indicated by sexual identity", xlab="Prevalence")
polygon(density(prev_boot$t), col="#CCCCCC", border="#CCCCCC")
lines(density(prevMSM))
legend("topright",fill=c("#FFFFFF","#CCCCCC"),c("Adjusted","Naive"), horiz=F, xpd=T)

#close file
dev.off()

#black/AA, partner

#obtain MSM status for each individual, for each simulation, in each chain
prevMSM_chain1 = as.data.frame(window(jags_samples[[1]][,2:(nrow(NESARCw2_partner)+1)], start=1000))
prevMSM_chain2 = as.data.frame(window(jags_samples[[2]][,2:(nrow(NESARCw2_partner)+1)], start=1000))

# #apply sampling probabilities
# samp_prob = na.omit(NESARCw2[,c("sex_men","hiv_aids","relationship","std","abuse_sexual","recent_drug","samp_prob")])
# samp_prob = samp_prob$samp_prob
# for (i in 1:nrow(NESARCw2_partner))
# {
#   prevMSM_chain1[,i] = prevMSM_chain1[,i]*samp_prob[i]
#   prevMSM_chain2[,i] = prevMSM_chain2[,i]*samp_prob[i]
# }
# rm(i,samp_prob)

#tally the total number for prevalance
#prevMSM = c(rowSums(prevMSM_chain1), rowSums(prevMSM_chain2))
prevMSM = c(rowSums(prevMSM_chain1)/nrow(NESARCw2_partner), rowSums(prevMSM_chain2)/nrow(NESARCw2_partner))

#summary of MSM prevalence
mean(prevMSM)
quantile(prevMSM,probs=c(0.025,0.975))

#naive prevalence for plotting
prev_boot = boot(NESARCw2_partner, bootPrev, R=1000, indicator="partner")

#output to a file (then open with gimp, scale to 1200, export as pdf)
tiff("Figure1b.tif",height=4,width=6,units='in',res=1200)

#density plot of MSM prevalence, naive and posterior
plot(density(prev_boot$t), xlim=c(0,0.055), ylim=c(0,155), main="b. MSM behavior indicated by same-sex partners", xlab="Prevalence")
polygon(density(prev_boot$t), col="#CCCCCC", border="#CCCCCC")
lines(density(prevMSM))
legend("topright",fill=c("#FFFFFF","#CCCCCC"),c("Adjusted","Naive"), horiz=F, xpd=T)

#close file
dev.off()

# #non-black/AA
# 
# #obtain MSM status for each individual, for each simulation, in each chain
# prevMSM_chain1 = as.data.frame(window(jags_samples[[1]][,2:(nrow(NESARCw2_nonblack)+1)], start=1000))
# prevMSM_chain2 = as.data.frame(window(jags_samples[[2]][,2:(nrow(NESARCw2_nonblack)+1)], start=1000))
# 
# # #apply sampling probabilities
# # for (i in 1:nrow(NESARCw2_nonblack))
# # {
# #   prevMSM_chain1[,i] = prevMSM_chain1[,i]*NESARCw2_nonblack$samp_prob[i]
# #   prevMSM_chain2[,i] = prevMSM_chain2[,i]*NESARCw2_nonblack$samp_prob[i]
# # }
# # rm(i)
# 
# #tally the total number for prevalance
# prevMSM = c(rowSums(prevMSM_chain1)/nrow(NESARCw2), rowSums(prevMSM_chain2)/nrow(NESARCw2))
# 
# #summary of MSM prevalence
# mean(prevMSM)
# quantile(prevMSM,probs=c(0.025,0.975))
# 
# #density plot of MSM prevalence
# plot(density(prevMSM), main="c. Non-black or African American MSM by sexual identity", xlab="Prevalence")
# plot(density(prevMSM), main="d. Non-black or African American MSM by partner gender", xlab="Prevalence")
