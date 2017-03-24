

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

###### for each person for each trial
recdat <- dat[dat$task=="recall",]

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

#########