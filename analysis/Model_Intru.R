
# ---- libraries ----
library(plyr)
library(dplyr)
library(farrellLab)
library(farrellMem)
library(ggplot2)
library(tidyr)
library(lme4)

#---- read-data
dat <- read.csv("finalData.csv")

ll <- 7
nReps<-1000

dat$serposf <- dat$serpos # keep continuous version of serpos around
dat$serposf <- factor(dat$serposf)
dat$task_order <- ifelse(dat$task_order < 2, 0, 1)
dat$task_order <- factor(dat$task_order)
dat$task <- factor(dat$task)
dat$correct <- dat$recalled * 0
dat$correct[dat$recalled == 1 & dat$serposc < ll] <- 1

#---- data-filtering ----
cdat <- ddply(dat, .(ID), function(x)
  length(unique(x$trial_id)))
smallNSs <- cdat$ID[cdat$V1 <= (.8 * 32)]

# mean correct per participant
recdat <- dat[dat$task == "recall", ]
sdat <-
  ddply(recdat[recdat$serpos <= ll, ],
        .(ID, serposf),
        summarise,
        pcor = sum(recalled == 1) / length(unique(trial_id)))
ppdat <- ddply(sdat, .(ID), summarise, pcor = mean(pcor))
badSs1 <- ppdat$ID[ppdat$pcor < (1 / ll)]

# do the filtering
badFilter <- sapply(dat$ID, function(x) any(x==badSs1) | any(x==smallNSs))
dat <- dat[!badFilter,]

### QUESTIONS
# model one - above 10 and below 100 for the intrusions to include??
# how to get an avergae of the 1000 without always converging on central distribution

stim_intru<-recdat$stim[(recdat$recalled==-1)&(recdat$stim<100)& (recdat$stim>0)]     

#MODEL 1
# randomly sample from 10-99 

all_samp1<-{}
for (i in 1:nReps){
  sample_intru1<-sample(10:99,length(stim_intru),replace=TRUE)
  all_samp1<-cbind(all_samp1,sample_intru1)
}

# MODEL 2
# sample from overall distrubition of items ppts saw across the experiment 
kk <- hist(recdat$stim[recdat$recalled>0], breaks=seq(-0.5,99.5,by = 1));
all_samp2<-{}
for (i in 1:nReps){
  sample_intru2<-sample(1:100, length(stim_intru),replace=TRUE, prob=kk$density)
  all_samp2<-cbind(all_samp2,sample_intru2)
}

# MODEL 3

# use recdat to find the min and max value for each trial and sample from this for each intusion 

minmax <- ddply (recdat[(recdat$stim<100) & (recdat$stim>0),], .(ID, trial_id), function (x) {
    if (any(x$recalled==-1)){
      intru <- x$stim[x$recalled == -1]
      intdists <- rep(NA,length(intru))
      V1<-rep(NA,length(intru))
      V2<-rep(NA,length(intru))
      
      for (f in 1:length(intru)) {
        dist <- abs(x$stim[(x$serpos>0) & (x$serpos<=ll) & (x$recalled!=1) & (x$recalled!=-2)] - intru[f])
        intdists[f] <- min(dist)
        V1[f]<-min(x$stim[(x$serpos>0) & (x$serpos<=ll)])
        V2[f]<-max(x$stim[(x$serpos>0) & (x$serpos<=ll)])
       # S[f]<-sample(V1:V2,1)
        }
    return(data.frame(min=V1,max=V2))
  } else {
  return({})}
    
})
all_samp3<-{}
sample_intru3<-rep(NA,length(stim_intru))

for (i in 1:nReps) {
  for (f in 1:length(stim_intru)) {
  sample_intru3[f]<-sample(minmax$min[f]:minmax$max[f],1,replace=TRUE)}
  all_samp3<-cbind(all_samp3,sample_intru3)
}


#plot     
hist(stim_intru,breaks=100,freq=F)
lines(density(all_samp1),col="orange") # sample from 10-99
lines(density(all_samp2),col="blue") # sample from overall distrubtion
lines(density(all_samp3),col="red") # sample range of items on each trial 
