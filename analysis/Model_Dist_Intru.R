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
nReps<-100

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


stim_intru<-recdat$stim[(recdat$recalled==-1)&(recdat$stim<100)& (recdat$stim>0)]     

### DATA Intrusion Distances
intrude_df <- ddply (recdat[(recdat$stim<100) & (recdat$stim>0),], .(ID, trial_id), function (x) {
  
  if (any(x$recalled==-1)){
    
    # get the intrusions
    intru <- x$stim[x$recalled == -1]
    
    intdists <- rep(NA,length(intru))
    
    for (f in 1:length(intru)) {
      
      dist <- abs(x$stim[(x$serpos>0) & (x$serpos<=ll) & (x$recalled!=1) & (x$recalled!=-2)] - intru[f])
      intdists[f] <- min(dist)
      if ( min(dist)==0){
        print(x)
      }
    }
    
    return(data.frame(inddists=intdists))
  } else {
    return({})
  }
})


# MODEL 1
# for each intrusion find the min and maxand then take a sample 
all_samp1<-{}

for (i in 1:nReps) {
  print(i)
  intru_1 <- ddply (recdat[(recdat$stim<100) & (recdat$stim>0),], .(ID, trial_id), function (x) {
    if (any(x$recalled==-1)){
      intru <- x$stim[x$recalled == -1]
      intdists <- rep(NA,length(intru))
      sample_intru1<-rep(NA,length(intru))
      for (f in 1:length(intru)) {
        sample_intru1[f]<-sample(10:99, 1,replace=TRUE)
        dist <- abs(x$stim[(x$serpos>0) & (x$serpos<=ll) & (x$recalled!=1) & (x$recalled!=-2)] - sample_intru1[f])
        intdists[f] <- min(dist)
      }
      return(data_frame(samp=sample_intru1,dist=intdists))
    }
    else {
      return({})
    }
  })
  all_samp1<-cbind(all_samp1,intru_1$dist)
}


# MODEL 2
# for each intrusion find the min and maxand then take a sample 
all_samp2<-{}
kk <- hist(recdat$stim[recdat$recalled>0], breaks=seq(-0.5,99.5,by = 1));

for (i in 1:nReps) {
  intru_2 <- ddply (recdat[(recdat$stim<100) & (recdat$stim>0),], .(ID, trial_id), function (x) {
    if (any(x$recalled==-1)){
      intru <- x$stim[x$recalled == -1]
      intdists <- rep(NA,length(intru))
      sample_intru2<-rep(NA,length(intru))
      for (f in 1:length(intru)) {
        sample_intru2[f]<-sample(1:100, 1,replace=TRUE, prob=kk$density)
        dist <- abs(x$stim[(x$serpos>0) & (x$serpos<=ll) & (x$recalled!=1) & (x$recalled!=-2)] - sample_intru2[f])
        intdists[f] <- min(dist)
      }
      return(data_frame(samp=sample_intru2,dist=intdists))
    }
    else {
      return({})
    }
  })
  all_samp2<-cbind(all_samp2,intru_2$dist)
}


# MODEL 3
# for each intrusion find the min and maxand then take a sample 
all_samp3<-{}
for (i in 1:nReps) {
  intru_3 <- ddply (recdat[(recdat$stim<100) & (recdat$stim>0),], .(ID, trial_id), function (x) {
    
    if (any(x$recalled==-1)){
      intru <- x$stim[x$recalled == -1]
      intdists <- rep(NA,length(intru))
      sample_intru3<-rep(NA,length(intru))
      
      V1<-min(x$stim[(x$serpos>0) & (x$serpos<=ll)])
      V2<-max(x$stim[(x$serpos>0) & (x$serpos<=ll)])
      
      for (f in 1:length(intru)) {
        sample_intru3[f]<-sample(V1:V2,1,replace=TRUE)
        dist <- abs(x$stim[(x$serpos>0) & (x$serpos<=ll) & (x$recalled!=1) & (x$recalled!=-2)] - sample_intru3[f])
        intdists[f] <- min(dist)
      }
      return(data_frame(min=V1,max=V2,samp=sample_intru3,dist=intdists))
    }
    else {
      return({})
    }
  })
  all_samp3<-cbind(all_samp3,intru_3$dist)
}

# MODEL 2
# sample from overall distrubition of items ppts saw across the experiment 
# kk <- hist(recdat$stim[recdat$recalled>0], breaks=seq(-0.5,99.5,by = 1));
# all_samp2<-{}
# for (i in 1:nReps){
#   sample_intru2<-sample(1:100, length(stim_intru),replace=TRUE, prob=kk$density)
#   all_samp2<-cbind(all_samp2,sample_intru2)
# }


hiFilter <- 31
allBr <- seq(0.5,30.5)
allX <- 1:30
#png("graph_intru.png")
hist(intrude_df$inddists[intrude_df$inddists<hiFilter],breaks=allBr,freq=F)

kk1 <- hist(all_samp1[all_samp1>0 & all_samp1<hiFilter],breaks=allBr,freq=F,plot=F)
lines(kk1$mids, kk1$density, col="orange") # sample from 10-99

kk2 <- hist(all_samp2[all_samp2>0 & all_samp2<hiFilter],breaks=allBr,freq=F,plot=F)
lines(kk2$mids, kk2$density,col="blue") # sample from overall distrubtion

kk3 <- hist(all_samp3[all_samp3>0 & all_samp3<hiFilter],breaks=allBr,freq=F,plot=F)
lines(kk3$mids, kk3$density,col="red") # sample range of items on each trial 
dev.off()
