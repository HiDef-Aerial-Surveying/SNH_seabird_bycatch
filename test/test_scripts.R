
library(truncnorm)
library(gmodels)


Dive.profile <- function(avg,mx,mn=0,stdev){
  set.seed(99)
  X <- data.frame(x=rtruncnorm(10000,a=mn,b=mx,mean=avg,sd=stdev))
}


Proportion.available <- function(Density.prof,gear.top,gear.bottom){
  prop.in.range <- (length(which(Density.prof$x > gear.top & Density.prof$x < gear.bottom)))/10000
}


Bird.availability <- function(dive.duration,dives.per.day,perc.depth){
  ## In here, there will be factors that affect the likilhood that a bird will get caught up in the nets / hooks
  dives.per.sec <- dives.per.day / 86400
  Ba <- dive.duration * dives.per.sec * perc.depth
  return(Ba)
}



bootstrap.proportions <- function(mn,mx,avg,stdev,boot.size=1000,gear.top=10,gear.bottom=50){
  
  output <- foreach(i=1:boot.size,.combine='c') %do% {
    X <- data.frame(x=rtruncnorm(10000,a=mn,b=mx,mean=avg,sd=stdev))
    #X <- data.frame(x=rtruncnorm(10000,a=0,b=100,mean=60,sd=20))
    return(Proportion.available(X,gear.top,gear.bottom))
  }
  return(output)
  
}



Do.bootstrap <- function(boot.size=1000,prop.avail,dive.duration,dives.per.day,Density.profile,F.effort){
  
  Density <- sample(Density.profile$x,boot.size,replace=TRUE)
  FishEffort <- rep(F.effort,times=boot.size)
  BirdAvail <- Bird.availability(dive.duration,dives.per.day,prop.avail)
  
  ER <- Density * FishEffort * BirdAvail * 86400
  
  return(ER)
}



Density.profile <- Dive.profile(15,25,0,5)

bb <- bootstrap.proportions(0,100,60,20)

F.effort <- 0.027

dive.duration <- 20
dives.per.day <- 40

TT <- Do.bootstrap(boot.size = 1000,prop.avail = bb,dive.duration,dives.per.day,Density.profile,F.effort)



mx <- 138
avg <- 42
stdev <- 50.1
gear.top <- 10
gear.bottom <- 20

densavg <- 0.00000001594
densstd <- 0.0000000142

divdur <- 76
divPdy <- 38

F.eff <- 100


bP <- bootstrap.proportions(mn=0,mx=mx,avg=avg,
                            stdev=stdev,boot.size=100,gear.top=gear.top,
                            gear.bottom=gear.bottom)           

Density.profile <- Dive.profile(avg=densavg,mx=1,mn=0,stdev=densstd)


BootOut <- Do.bootstrap(boot.size=100,prop.avail=bP,dive.duration=divdur,
                        dives.per.day=divPdy,F.effort=F.eff,
                        Density.profile=Density.profile)

CIs <- ci(BootOut)
CIestimate <- signif(CIs[1],3)
CIlower <- signif(CIs[2],3)
CIupper <- signif(CIs[3],3)
CIstderr <- signif(CIs[4],3)


Density <- sample(Density.profile$x,100,replace=TRUE)
FishEffort <- rep(F.eff,times=100)
BirdAvail <- Bird.availability(divdur,divPdy,perc.depth = bP)




##################################################################
#### Code for overlaying density shapefiles with the marine units
##################################################################


library(sp)
library(raster)
library(maptools)
library(rgeos)
library(rgdal)


SMAUs <- readRDS('data/SMAU.rds')

#summer <- readOGR(dsn="../../03 - Data/Seabird_Sensitivity/MB0126_MRSea_Summer_Density.shp",
#                  layer='MB0126_MRSea_Summer_Density')
#winter <- readOGR(dsn="../../03 - Data/Seabird_Sensitivity/MB0126_MRSea_Winter_Density.shp",
#                  layer='MB0126_MRSea_Winter_Density')


UTM30 <- CRS(projection(summer))
WGS84 <- CRS(projection(SMAUs))

summer84 <- spTransform(summer,WGS84)
winter84 <- spTransform(winter,WGS84)


maxsummer <- over(SMAUs,summer84,fn=max)
meansummer <- over(SMAUs,summer84,fn=mean)
sdsummer <- over(SMAUs,summer84,fn=sd)

maxwinter <- over(SMAUs,winter84,fn=max)
meanwinter <- over(SMAUs,winter84,fn=mean)
sdwinter <- over(SMAUs,winter84,fn=sd)


density.table <- function(SMAUs,dens.map){
  
  test <- round(data.frame(t(dens.map[,c('GX','PU','KI','GU','RA','RH')])),2)
  names(test) <- SMAUs$objnam
  specs <- c('Northern Gannet','Atlantic Puffin','Kittiwake','Common Guillemot','Razorbill','Red-throated Diver')
  out <- data.frame(Species=specs,test)
  return(out)
}

SUM.max <- density.table(SMAUs,maxsummer)
SUM.mean <- density.table(SMAUs,meansummer)
SUM.sd <- density.table(SMAUs,sdsummer)
WIN.max <- density.table(SMAUs,maxwinter)
WIN.mean <- density.table(SMAUs,meanwinter)
WIN.sd <- density.table(SMAUs,sdwinter)


write.csv(SUM.max,'data/breeding_table_max.csv',row.names=F)
write.csv(SUM.mean,'data/breeding_table.csv',row.names=F)
write.csv(SUM.sd,'data/breeding_table_sd.csv',row.names=F)
write.csv(WIN.max,'data/nonbreeding_table_max.csv',row.names=F)
write.csv(WIN.mean,'data/nonbreeding_table.csv',row.names=F)
write.csv(WIN.sd,'data/nonbreeding_table_sd.csv',row.names=F)


####################################################################################################################
## testing the model with raw code

F.effort <- 2700  ## A gill net that is 90m long by 30m deep
geartop <- 20
gearbottm <- 50
max.dive <- 138
max.dive.std <- 60
mean.dive <- 52
dives.p.day <- 50
dive.duration <- 200
dive.duration.std <- 50
dive.duration.max <- 300
surf.density <- 5000
surf.density.sd <- 2000

underwater.density <- Depth.density(surf.density,max.dive)
underwater.density.sd <- Depth.density(surf.density.sd,max.dive)

Density.profile <- data.frame(x=rtruncnorm(10000,a=0,b=1,mean=underwater.density,sd=underwater.density.sd))

BPs <- bootstrap.proportions(mn = 0, mx=max.dive,avg=mean.dive,stdev=max.dive.std,gear.top=geartop,gear.bottom=gearbottm)

BOOTS <- Do.bootstrap(1000,BPs,dive.duration,dive.duration.std,dive.duration.max,dives.p.day,Density.profile,F.effort)



Proportion.available <- function(Density.prof,gear.top,gear.bottom){
  
  prop.in.range <- (length(which(Density.prof$x > gear.top & Density.prof$x < gear.bottom)))/10000
  
}



Depth.density <- function(surf.dens,max.dive.depth){
  Da <- surf.dens/1000000
  d <-  max.dive.depth
  
  D <- Da / d  
  
  return(D)
}



###################################################################################################


Bird.availability <- function(dive.duration,dives.per.day,perc.depth){
  dives.per.sec <- dives.per.day / 86400
  Ba <- dive.duration * dives.per.sec * perc.depth
  return(Ba)
}


###################################################################################################
bootstrap.proportions <- function(mn,mx,avg,stdev,boot.size=1000,gear.top=10,gear.bottom=50){
  
  output <- foreach(i=1:boot.size,.combine='c') %do% {
    X <- data.frame(x=rtruncnorm(10000,a=mn,b=mx,mean=avg,sd=stdev))
    #X <- data.frame(x=rtruncnorm(10000,a=0,b=100,mean=60,sd=20))
    return(Proportion.available(X,gear.top,gear.bottom))
  }
  return(output)
  
}



Do.bootstrap <- function(boot.size=1000,prop.avail,dive.duration,dive.duration.std,dive.duration.max,dives.per.day,Density.profile,F.effort){
  
  Density <- sample(Density.profile$x,boot.size,replace=FALSE)
  FishEffort <- rep(F.effort,times=boot.size)
  
  Dive.durations <- data.frame(x=rtruncnorm(10000,a=0,b=dive.duration.max,mean=dive.duration,sd=dive.duration.std))
  div.dur <- sample(Dive.durations$x,boot.size,replace=FALSE)
  
  DF <- data.frame(div.dur, rep(dives.per.day,times=boot.size),prop.avail)
  
  BirdAvail <- sapply(1:nrow(DF),function(x) Bird.availability(DF[x,1],DF[x,2],DF[x,3]))
  
  ER <- Density * FishEffort * BirdAvail * 86400
  
  return(ER)
}


get.cis <- function(vals){
  CIs <- ci(vals)
  CIestimate <- signif(CIs[1],3)
  
  CIlower <- signif(CIs[2],3)
  if(CIlower < 0){CIlower <- 0}
  
  CIupper <- signif(CIs[3],3)
  CIstderr <- signif(CIs[4],3)
  
  return(data.frame(CIestimate,CIlower,CIupper,CIstderr))
  
}



bootstrapped.plot <- function(BOOTS) {
  BOOTS <- data.frame(BOOTS)
  CIS <- ci(BOOTS$BOOTS)
  QNT <- quantile(BOOTS$BOOTS,c(0.05,0.95))
  
  Ylim <- max(density(BOOTS$BOOTS)$y) + max(density(BOOTS$BOOTS)$y)*.25
  
  ggplot(data=BOOTS,aes(BOOTS)) + 
    geom_histogram(aes(y=..density..),
                   col='grey',
                   fill='dodgerblue',
                   alpha=0.4) + 
    scale_x_continuous(expand=c(0,0))+
    scale_y_continuous(expand=c(0,0))+
    coord_cartesian(xlim=c(0,max(BOOTS)),ylim=c(Ylim,0))+
    xlab('Bird encounters per day')+
    geom_vline(xintercept=CIS[3],color='red',linetype='solid',size=0.25)+
    geom_vline(xintercept=CIS[2],color='red',linetype='solid',size=0.25)+
    geom_vline(xintercept=QNT[1],color='black',linetype='dashed',size=0.25)+
    geom_vline(xintercept=QNT[2],color='black',linetype='dashed',size=0.25)+
    
    geom_hline(aes(yintercept=10000,color='CI',linetype='CI'),size=0.2)+
    geom_hline(aes(yintercept=10000,color='QT',linetype='QT'),size=0.2)+
    
    scale_color_manual(name='',values=c(CI='red',QT='black'),labels=c('95% CI', '5% and 95% quantiles'))+
    scale_linetype_manual(name='',values=c(CI='solid',QT='dashed'),labels=c('95% CI', '5% and 95% quantiles'))+
    theme(legend.position='bottom')
  
}



  



ggplot(X,aes(x)) +
  geom_density(bw='nrd0',adjust=2,fill='lightgoldenrod',alpha=0.4,color='black',size=0.5)+
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  coord_cartesian(xlim=c(0,Xmax),ylim=c(Ylim,0))+
  geom_vline(xintercept=avg,color='red',linetype='dashed',size=0.25)+
  xlab('Depth (m)')+ylab('')+
  {if(plot.gear==TRUE)geom_vline(xintercept=gear.top,color='dodgerblue',linetype='solid',size=0.75)}+
  {if(plot.gear==TRUE)geom_vline(xintercept=gear.bottom,color='dodgerblue',linetype='solid',size=0.75)}+
  {if(plot.gear==TRUE)geom_hline(aes(yintercept=100,color='gear'),linetype='solid',size=0.75)}+
  {if(plot.gear==TRUE)scale_color_manual(name='',values=c(gear='dodgerblue'),labels=c('Gear depth'))}+
  {if(plot.gear==TRUE)coord_flip(ylim=c(Ylim,0))}+
  {if(plot.gear==TRUE)scale_x_reverse()}+
  {if(plot.gear==TRUE)theme(legend.position=c(0.5,0.1))}




