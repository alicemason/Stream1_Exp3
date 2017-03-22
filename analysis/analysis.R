# ---- libraries ----
library(plyr)
library(dplyr)
library(farrellLab)
library(farrellMem)
library(ggplot2)
library(tidyr)
library(lme4)

# ---- functions ----


#---- read-data
dat <- read.csv("finalData.csv")

ll<-7

dat$serposf <- dat$serpos # keep continuous version of serpos around
dat$serposf <- factor(dat$serposf)
dat$task_order <- ifelse(dat$task_order<2,0,1)
dat$task_order <- factor(dat$task_order)
dat$task <- factor(dat$task)
dat$correct <- dat$recalled*0
dat$correct[dat$recalled==1 & dat$serposc<ll] <- 1

#---- data-filtering ----
cdat <- ddply(dat, .(ID), function(x) length(unique(x$trial_id)))
smallNSs <- cdat$ID[cdat$V1<= (.8*32)]

# mean correct per participant
recdat <- dat[dat$task=="recall",]
sdat <- ddply(recdat[recdat$serpos<=ll,], .(ID,serposf), summarise, pcor=sum(recalled==1)/length(unique(trial_id)))
ppdat <- ddply(sdat, .(ID), summarise, pcor=mean(pcor))
badSs1 <- ppdat$ID[ppdat$pcor<(1/ll)]

# do the filtering
badFilter <- sapply(dat$ID, function(x) any(x==badSs1) | any(x==smallNSs))
dat <- dat[!badFilter,]

# ---- cond-spc ----
recdat <- dat[dat$task=="recall",]

sdat <- ddply(recdat[recdat$serpos>0 & recdat$serpos<=ll,], .(ID,task_order), function(x) getAccFree(x, ll=7))
plotdat <- ddply(sdat, .(serpos,task_order), summarise, pcor=mean(pcor))

ggplot(plotdat, aes(x=as.numeric(serpos), y=pcor, colour=task_order)) + 
  geom_line() + geom_point(size=4) +
  scale_x_continuous(breaks=1:7)+
  ylim(c(0,1)) + xlab("Serial Position") + ylab("Proportion Correct") + 
  theme_APA() + scale_shape_APA1() + scale_colour_CB() +
  theme(legend.position=c(0.8,0.2))

# plot individuals
ggplot(sdat, aes(x=as.numeric(serpos), y=pcor)) + 
  geom_line() + geom_point(size=4) +
  scale_x_continuous(breaks=1:10)+
  ylim(c(0,0.5)) + xlab("Serial Position") + ylab("Proportion Correct") +  facet_wrap( ~ ID) +
  theme_APA() + scale_shape_APA1() + scale_colour_CB() +
  theme(legend.position=c(0.8,0.2))


# ---- cond-frp ----
sdat <- ddply(recdat[recdat$serpos>0 & recdat$serpos<=ll,], .(ID,task_order), function(x) getFRP(x, ll=6))
plotdat <- ddply(sdat, .(serpos,task_order), summarise, prob=mean(prob))

ggplot(plotdat, aes(x=as.numeric(serpos), y=prob, colour=task_order)) + 
  geom_line() + geom_point(size=4) +
  scale_x_continuous(breaks=1:10)+
  ylim(c(0,0.5)) + xlab("Serial Position") + ylab("FRP") + 
  theme_APA() + scale_shape_APA1() + scale_colour_CB() +
  theme(legend.position=c(0.8,0.2))

# ---- scores-per-trial
evdat <- ddply(dat, .(ID,trial_id), function(x){
  recev <- mean(x$stim[(x$recalled!=0) &
                        (x$stim>=0) &
                      (x$stim<=100)])
  #recev <- mean(x$stim[(x$recalled==1)])
  presev <- mean(x$stim[x$task=="offer"])
  WTP <- x$WTP[x$task=="offer"][1]
  return(data.frame(task_order=x$task_order[1],recev=recev,presev=presev,WTP=WTP))
})

evdat$WTP[evdat$WTP<1] <- NA
evdat$WTP[evdat$WTP>99] <- NA

library(nlme)
fm <- lme(WTP ~ presev + recev, data = evdat, random = ~ 1 | ID, 
           na.action=na.omit, method="ML", keep.data=T)
summary(fm)

fm0 <- lme(WTP ~ recev + presev, data = evdat[evdat$task_order==0,], random = ~ 1 | ID, 
           na.action=na.omit, method="ML")
fm1 <- lme(WTP ~ recev + presev, data = evdat[evdat$task_order==1,], random = ~ 1 | ID, 
           na.action=na.omit, method="ML")

fmr <- lme(WTP ~ recev, data = evdat, random = ~ 1 | ID, 
           na.action=na.omit, method="ML")

fmp <- lme(WTP ~ presev, data = evdat, random = ~ 1 | ID, 
           na.action=na.omit, method="ML")

anova(fmr, fmp)
anova(fmr,fm)
anova(fmp,fm)


# fm1 <- lme(WTP ~ recev, data = evdat, random = ~ 1 | ID, na.action=na.omit, method="ML")
# fm2 <- lme(WTP ~ presev, data = evdat, random = ~ 1 | ID, na.action=na.omit, method="ML")
# anova(fm1,fm2)

# Utility for using presev vs recev?

