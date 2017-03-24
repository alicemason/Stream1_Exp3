

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
recdat <- dat[dat$task=="recall",]

intrude_df <- ddply (recdat, .(ID, trial_id), function (x) {
  
  if (any(x$recalled==-1)){
    
    # get the intrusions
    intru <- x$stim[x$recalled == -1]
    intru<-cbind(x$stim[x$recalled == -1], x$serpos[x$recalled==-1],x$recalled[x$recalled==-1])
    
    intdists <- rep(NA,length(intru[,1]))
  
    for (f in 1:length(intru[,1])) {
      # distance between each intrusion and each item on the list that wasn't correctly recalled
      # why is there a 0 in intrude_df- these should be NA ?????
      dist <- abs(x$stim[(x$serpos>0) & (x$serpos<=ll) & (x$recalled!=1)] - intru[f,1])
      intdists[f] <- min(dist)
    }
    
    return(data.frame(intdists=intdists,stim=intru[,1],serpos=intru[,2],recalled=intru[,3]))
  } else {
    return(data.frame(intdists=NA,stim=x$stim,serpos=x$serpos,recalled=x$recalled))
  }
})

merge(recdat,intrude_df,by=c("ID","trial_id","serpos","stim","recalled"))

# don't inlcude repitions
# include only intrusions that are 1 item away from target item 

intru_cri<-intrude_df[(intrude_df$intdists>0) & (intrude_df$intdists<2),]    

test<- ddply(recdat, .(ID, trial_id), function (x) 
  {
  include <- rep(0,length(x$stim))
  if (any(x$recalled==-1)){
    for (f in 1:length(intru_cri)) {
      tag<-match(intru_cri$stim[f],x$stim)
      include[tag]<-1
    }
  }
  return(data.frame(included=include))
})

# ---- scores-per-trial
evdat <- ddply(dat, .(ID,trial_id), function(x){
  recev <- mean(x$stim[(x$recalled!=0) &
                         (x$stim>=0) &
                         & (x$include==1) &
                         (x$stim<=100)])
  
  #recev <- mean(x$stim[(x$recalled==1)])
  presev <- mean(x$stim[x$task=="offer"])
  WTP <- x$WTP[x$task=="offer"][1]
  return(data.frame(task_order=x$task_order[1],recev=recev,presev=presev,WTP=WTP))
})




#########