
# This program includes calculations and plotting of TCAL, and decomposition of TCAL difference by causes of deaths.

library(reshape2)
library(tidyverse)
library(tiff)
library(RColorBrewer)

## Cumulative all-cause TCAL plots
TCALplusACplot<-function(Mx1,Mx2,Y,Yb,Name1,Name2,gender) {
  CALlx<-c()
  CALlx1<-c()
  CALlx2<-c()
  PxCh<-c()
  PxCHlx<-c()
  YM<-Y-Yb+1
  
  
  for (x in 1:111){
    if (x <(YM+1)){
      px1<-c()
      px2<-c()
      for (z in 1:x){
        px1<-c(px1,Mx1[z,YM-x+z])
        px2<-c(px2,Mx2[z,YM-x+z])
      }
      pxCH<-c(log(px1/px2),rep(0,111-x))
      
      lx1<-prod(px1)
      lx2<-prod(px2)
      lx<-(lx1+lx2)/2
      pxCHlx<-lx*pxCH
    }
    if (x >(YM)){
      px1<-c()
      px2<-c()
      for (z in (x-YM+1):x){
        px1<-c(px1,Mx1[z,YM-x+z])
        px2<-c(px2,Mx2[z,YM-x+z])
      }
      px1<-c(rep(1,(x-YM)),px1)
      px2<-c(rep(1,(x-YM)),px2)
      pxCH<-c(log(px1/px2),rep(0,111-x))	 	
      
      lx1<-prod(px1)
      lx2<-prod(px2)
      lx<-(lx1+lx2)/2
      pxCHlx<-lx*pxCH
    }
    CALlx1<-c(CALlx1,lx1)
    CALlx2<-c(CALlx2,lx2)
    PxCh<-cbind(PxCh,pxCH)
    PxCHlx<-cbind(PxCHlx,pxCHlx)
  }
  CALlx<- t(matrix(rep((CALlx1+ CALlx2)/2,111),111))
  PxCh[is.na(PxCh)]<-0
  PxCHlx[is.na(PxCHlx)]<-0
  
  ## as Guillot calculates this plus a one for l(0)
  A1<-sum(c(1,CALlx1))+.5
  A2<-sum(c(1,CALlx2))+.5
  A3<-sum(CALlx1)-sum(CALlx2)
  A4<-sum(PxCh*CALlx)
  
  range(PxCh)
  ## create matrix for cumulative age-contribution
  
  CumAC<-matrix(0,111,111)
  for (x in 1:111) {
    for (z in 1:x) {
      CumAC[z,(111-x+z)]<-sum(PxCHlx[1:z,x])
    }  
  }
  CumAC1<-CumAC[,c((111-Y+Yb):111)]
  
  ## plot Lexis-surface for cumulative age-contribution
  bpy.colors <-
    function (n = 100, cutoff.tails = 0.1, alpha = 1)
    {
      n <- as.integer(n[1])
      if (n <= 0)
        return(character(0))
      
      if (cutoff.tails >= 1 || cutoff.tails < 0)
        stop("cutoff.tails should be in [0, 1]")
      i = seq(0.5 * cutoff.tails, 1 - 0.5 * cutoff.tails, length = n)
      r = ifelse(i < .25, 0, ifelse(i < .57, i / .32 - .78125, 1))
      g = ifelse(i < .42, 0, ifelse(i < .92, 2 * i - .84, 1))
      b = ifelse(i < .25, 4 * i, ifelse(i < .42, 1,
                                        ifelse(i < .92, -2 * i + 1.84, i / .08 - 11.5)))
      rgb(r, g, b, alpha)
    }
  mypalette<-rev(brewer.pal(8,"YlGnBu"))
  mypalette2<-rev(brewer.pal(8,"YlOrRd"))
  WildColors<-c(mypalette2[3:7],mypalette[5:1])
  customAxis <- function() { 
    n <- length(levels) 
    y <- seq(min(levels), max(levels), length.out=n) 
    rect(0, y[1:(n-1)], 1, y[2:n], col=WildColors) 
    axis(4, at=y, labels=levels) 
  } 
  levels<-c(-1,-0.1,-0.01,-0.001,-0.0001,0,0.0001,0.001,0.01,0.1,1)
  YEARS<-c(Yb:Y)
  Age<-c(0:110)
  
  tiff(paste("Cumulative All-cause", Name1, "vs", Name2, "(", gender, ")", ".tiff", sep=" "),res = 300,width = 2850,height =5514 ,units = "px")

  options(scipen = 5)
  par(cex.axis=1.4)
  par(mar=c(5,7,5,7))
  filled.contour(YEARS,Age,t(CumAC1),levels=levels,
                 col=WildColors,key.axes=customAxis(),
                 ylab="Age",xlab="Year",
                 main = paste("Total TCAL Difference=",format(round(A4,2),nsmall=2)),
                 plot.axes = { axis(2, seq(0, 110, by = 10))
                   axis(1, c(1979, 1990,2000,2010,Y)) }, 
                 cex.main=1.6, cex.lab=1.6,xlim = c(1979,Y))
  
  dev.off()
  print(rbind(c(paste("CAL-",Name1),paste("CAL-",Name2),"Diff","est-Diff"), format(round(c(A1,A2,A3,A4),2),2)))
}

## cumulative cause-specific TCAL plots

CALDecompFunctionCause<-function(Mx1,Mx2,Y,Y1,Name1,Name2,CAU1,CAU2,gender,file,Causespecific){
  
  CALlx<-c()
  CALlx1<-c()
  CALlx2<-c()
  PxCh<-c()
  PxChc<-c()
  PxCHlxc<-c()
  
  YM<-Y-Y1+1
  for (x in 1:111){
    if (x <(YM+1)){
      px1<-c()
      px2<-c()
      
      px1c<-c()
      px2c<-c()
      for (z in 1:x){
        px1<-c(px1,Mx1[z,YM-x+z])
        px2<-c(px2,Mx2[z,YM-x+z])
        px1c<-c(px1c,Mx1[z,YM-x+z]^CAU1[z,YM-x+z])
        px2c<-c(px2c,Mx2[z,YM-x+z]^CAU2[z,YM-x+z])
      }
      pxCH<-c(log(px1/px2),rep(0,111-x))
      pxCHc<-c(log(px1c/px2c),rep(0,111-x))
      lx1<-prod(px1)
      lx2<-prod(px2)
      lx<-(lx1+lx2)/2
      pxCHlxc<-lx*pxCHc
    }
    if (x >(YM)){
      px1<-c()
      px2<-c()
      px1c<-c()
      px2c<-c()
      for (z in (x-YM+1):x){
        px1<-c(px1,Mx1[z,YM-x+z])
        px2<-c(px2,Mx2[z,YM-x+z])
        px1c<-c(px1c,Mx1[z,YM-x+z]^CAU1[z,YM-x+z])
        px2c<-c(px2c,Mx2[z,YM-x+z]^CAU2[z,YM-x+z])
      }
      
      px1<-c(rep(1,(x-YM)),px1)
      px2<-c(rep(1,(x-YM)),px2)
      
      px1c<-c(rep(1,(x-YM)),px1c)
      px2c<-c(rep(1,(x-YM)),px2c)
      
      pxCH<-c(log(px1/px2),rep(0,111-x))
      pxCHc<-c(log(px1c/px2c),rep(0,111-x))
      
      lx1<-prod(px1)
      lx2<-prod(px2)
      lx<-(lx1+lx2)/2
      pxCHlxc<-lx*pxCHc
    }
    CALlx1<-c(CALlx1,lx1)
    CALlx2<-c(CALlx2,lx2)
    
    PxCh<-cbind(PxCh,pxCH)
    PxChc<-cbind(PxChc,pxCHc)
    PxCHlxc<-cbind(PxCHlxc,pxCHlxc)
  }
  
  CALlx<- t(matrix(rep((CALlx1+ CALlx2)/2,111),111))
  
  PxCh[is.na(PxCh)]<-0
  PxChc[is.na(PxChc)]<-0
  PxCHlxc[is.na(PxCHlxc)]<-0
  triangle <- PxChc* CALlx
  triangle[is.na(triangle)] <- 0
  
  
  ## as Guillot calculates this plus a one for l(0)
  A1<-sum(c(1,CALlx1))+.5
  A2<-sum(c(1,CALlx2))+.5
  A3<-sum(CALlx1)-sum(CALlx2)
  A4<-sum(PxCh* CALlx)
  A5<-sum(triangle)
 
  ## create matrix for cumulative age-contribution
  
  CumAC<-matrix(0,111,111)
  for (x in 1:111) {
    for (z in 1:x) {
      CumAC[z,(111-x+z)]<-sum(PxCHlxc[1:z,x])
    }  
  }
  CumAC1<-CumAC[,c((111-Y+Y1):111)]
  
  ## plot Lexis-surface for cumulative age-contribution
  bpy.colors <-
    function (n = 100, cutoff.tails = 0.1, alpha = 1)
    {
      n <- as.integer(n[1])
      if (n <= 0)
        return(character(0))
      
      if (cutoff.tails >= 1 || cutoff.tails < 0)
        stop("cutoff.tails should be in [0, 1]")
      i = seq(0.5 * cutoff.tails, 1 - 0.5 * cutoff.tails, length = n)
      r = ifelse(i < .25, 0, ifelse(i < .57, i / .32 - .78125, 1))
      g = ifelse(i < .42, 0, ifelse(i < .92, 2 * i - .84, 1))
      b = ifelse(i < .25, 4 * i, ifelse(i < .42, 1,
                                        ifelse(i < .92, -2 * i + 1.84, i / .08 - 11.5)))
      rgb(r, g, b, alpha)
    }
  
  
  mypalette<-rev(brewer.pal(8,"YlGnBu"))
  mypalette2<-rev(brewer.pal(8,"YlOrRd"))
  WildColors<-c(mypalette2[3:7],mypalette[5:1])
  
  customAxis <- function() { 
    n <- length(levels) 
    y <- seq(min(levels), max(levels), length.out=n) 
    rect(0, y[1:(n-1)], 1, y[2:n], col=WildColors) 
    axis(4, at=y, labels=levels) 
  } 
  levels<-c(-1,-0.1,-0.01,-0.001,-0.0001,0,0.0001,0.001,0.01,0.1,1)
  YEARS<-c(Y1:Y)
  Age<-c(0:110)
  
  tiff(paste("Cumulative", file, Name1, "vs", Name2, "(", gender, ")", ".tiff", sep=" "),res = 300,width = 2850,height =5514 ,units = "px")
  options(scipen = 5)
  par(cex.axis=1.4)
  par(mar=c(5,7,5,7))
  filled.contour(YEARS,Age,t(CumAC1),levels=levels,
                 col=WildColors,key.axes=customAxis(),
                 ylab="Age",xlab="Year",
                 main = paste(Causespecific,"=",format(round(A5,2),nsmall=2)),
                 plot.axes = { axis(2, seq(0, 110, by = 10))
                   axis(1, c(1979,Y1, 1990,2000,2010,Y)) }, 
                 cex.main=1.6, cex.lab=1.6,xlim = c(1979,Y))

  dev.off()
  print(rbind(c(paste("CAL-",Name1),paste("CAL-",Name2),"Diff","est-Diff","cause-specific est-Diff"),format(round(c(A1,A2,A3,A4,A5),2),2)))
  return(A5)
}  

## 95% Confidence interval of decomposed TCAL difference by causes of deaths
### function for resampling
count2rows2broad <- function(cou){
  n <- sum(cou)
  p <- cou/n
  if(n>0){
    newdat <- cbind(c(1:5), Freq = rmultinom(1, n, p))
  }
  if(n==0){
    newdat <- cbind(c(1:5), Freq = rep(0, 5))
  }
  return(newdat[,2])
}

### function for standard error
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  datac <- plyr::rename(datac, c("mean" = "value"))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

### function for 95% CI for cause-specific TCAL difference, taking HK vs HICs as an example.
CALDecompFunctionCausebootbroadhic<-function(cty1,cty2,sex,R){
  
  
  longcty1 <- read.table("Life Expectancy/hkdccod_merge_broad_new.csv",sep=",",header=TRUE)
  longcty1$icd <- as.character(longcty1$icd)
  longcty1$icd <- as.factor(longcty1$icd)
  longcty1 <- longcty1[longcty1$Age<=99,]
  longcty1 <- longcty1[longcty1$Sex==sex,]
  longcty1$icd <- as.factor(longcty1$icd)
  longcty1$Cause <- as.numeric(longcty1$icd)
  longcty1$Year <- as.numeric(longcty1$Year)
  longcty1 <- longcty1[order(longcty1$Year,longcty1$Cause,longcty1$Age),]
  longcty1 <- subset(longcty1, select=c(Year,Age,Cause,icd,n,nn))
  icd_data <- read.table(paste("Life Expectancy/out for broad bootstrap.csv",sep=""),sep=",", header=TRUE)
  icd_data$Sex<-as.factor(icd_data$Sex)
  levels(icd_data$Sex)[1]<-"Male"
  levels(icd_data$Sex)[2]<-"Female"
  icd_data <- icd_data[icd_data$Sex==sex,]
  
  
  if (sex=="Female") {
    qx1a<-read.table("Life Expectancy/HK_fltper_1x1.csv",sep=",",header=TRUE)
    qx1a<-as.data.frame(qx1a)
    qx1a$px<-1-qx1a$qx
    qx2a<-readxl::read_excel("Life Expectancy/hic_female_lt.xlsx")
    qx2a<-as.data.frame(apply(qx2a, 2, as.numeric))
    qx2a$px<-1-qx2a$qx
    Y1<-max(range(qx1a$Year)[1],range(qx2a$Year)[1],range(longcty1$Year)[1],range(icd_data$Year)[1],1979)
    Y2<-min(range(qx1a$Year)[2],range(qx2a$Year)[2],range(longcty1$Year)[2],range(icd_data$Year)[2],2016)
    qx1a$Year<-as.numeric(qx1a$Year)
    qx2a$Year<-as.numeric(qx2a$Year)
    qx1a<-qx1a[qx1a$Year %in% Y1:Y2,]
    qx2a<-qx2a[qx2a$Year %in% Y1:Y2,]
    qx1<-subset(qx1a, select = c(Age,Year,px))
    qx1<-reshape2::dcast(qx1,Age~Year,value.var = "px")
    qx1<-subset(qx1, select = -c(Age))
    qx2<-subset(qx2a, select = c(Age,Year,px))
    qx2<-reshape2::dcast(qx2,Age~Year,value.var = "px")
    qx2<-subset(qx2, select = -c(Age))
  }
  if (sex=="Male") {
    qx1a<-read.table("Life Expectancy/HK_mltper_1x1.csv",sep=",",header=TRUE)
    qx1a$px<-1-qx1a$qx
    qx2a<-readxl::read_excel("Life Expectancy/hic_male_lt.xlsx")
    qx2a<-as.data.frame(apply(qx2a, 2, as.numeric))
    qx2a$px<-1-qx2a$qx
    Y1<-max(range(qx1a$Year)[1],range(qx2a$Year)[1],range(longcty1$Year)[1],range(icd_data$Year)[1],1979)
    Y2<-min(range(qx1a$Year)[2],range(qx2a$Year)[2],range(longcty1$Year)[2],range(icd_data$Year)[2],2016)
    qx1a$Year<-as.numeric(qx1a$Year)
    qx2a$Year<-as.numeric(qx2a$Year)
    qx1a<-qx1a[qx1a$Year %in% Y1:Y2,]
    qx2a<-qx2a[qx2a$Year %in% Y1:Y2,]
    qx1<-subset(qx1a, select = c(Age,Year,px))
    qx1<-reshape2::dcast(qx1,Age~Year,value.var = "px")
    qx1<-subset(qx1, select = -c(Age))
    qx2<-subset(qx2a, select = c(Age,Year,px))
    qx2<-reshape2::dcast(qx2,Age~Year,value.var = "px")
    qx2<-subset(qx2, select = -c(Age))
  }
  
  longcty1 <- longcty1[longcty1$Year %in% Y1:Y2,]
  icd_data <- icd_data[icd_data$Year %in% Y1:Y2,]
  Repl <- matrix(NA,nrow=R,ncol=5)
  #Now the bootstrap Conf. Inf.
  for(i in 1:R){
    longcty1$Strat <- paste(longcty1$Age,longcty1$Year,sep=":")
    a1 <- tapply(longcty1$n,longcty1$Strat,count2rows2broad)
    b1 <- as.data.frame(do.call(rbind, a1))
    b1$id <- row.names(b1)
    c1 <- reshape2::melt(b1, id=c("id"))
    c1$Cause <- as.numeric(gsub("V","",(c1$variable)))
    
    newlongcty1 <- merge(longcty1,c1,by.x=c("Strat","Cause"),by.y=c("id","Cause"))
    newlongcty1 <- newlongcty1 %>%
      arrange(Year, Age, Cause) %>%
      dplyr::group_by(Year, Age) %>%
      dplyr::mutate(nn=sum(value))
    
    newlongcty1$cause<- (newlongcty1$value)/(newlongcty1$nn)
    newlongcty1$freq<-ifelse(newlongcty1$Age==99,12,1)
    newlongcty1<-newlongcty1 %>% arrange(icd,Year,Age)
    newlongcty1<-newlongcty1[rep(seq(nrow(newlongcty1)),newlongcty1$freq),]
    newlongcty1<-newlongcty1 %>%
      group_by(icd,Year) %>%
      dplyr:: mutate(age = sequence(n())) 
    newlongcty1$Age<-newlongcty1$age-1
    newlongcty1<-subset(newlongcty1,select=c(Year,Age,Cause,icd,cause))
    newlongcty1 <- newlongcty1[order(newlongcty1$Cause,newlongcty1$Year,newlongcty1$Age),]
    icd_data$Strat <- paste(icd_data$Country,icd_data$Year,icd_data$Agegroup,sep=":")
    a2 <- tapply(icd_data$Deaths,icd_data$Strat,count2rows2broad)
    b2 <- as.data.frame(do.call(rbind, a2))
    b2$id <- row.names(b2)
    c2 <- reshape2::melt(b2, id=c("id"))
    c2$Cause <- as.numeric(gsub("V","",(c2$variable)))
    icd_data$icd<-as.factor(icd_data$icd)
    icd_data$Cause <- as.numeric(icd_data$icd)
    
    icd_datab <- merge(icd_data,c2,by.x=c("Strat","Cause"),by.y=c("id","Cause"),all.x=T)
    icd_datab <- icd_datab %>%
      arrange(Country,Year, Agegroup, Cause) %>%
      group_by(Country,Year, Agegroup) %>%
      mutate(nn=sum(value))
    
    icd_datab <- icd_datab %>% mutate(overall_mx = nn/Exposure, icd_mx = value /Exposure) %>% data.frame()
    icd_datab <- icd_datab %>% group_by(Year,icd,Agegroup) %>% summarize(icd_mx = mean(icd_mx,na.rm = T),overall_mx = mean(overall_mx,na.rm = T)) %>% mutate(icd_prop = icd_mx/overall_mx,Country = "HICs") %>% data.frame()
    out1 <- data.frame(Year = rep(1979:2017,each = 111*5),
                       icd = rep(rep(c("neoplasm", "cardiovascular","external", "others","respiratory"),each = 111),39),
                       Age = rep(0:110,39*5)) %>% mutate(Agegroup = cut(Age, c(0,1,2,3,4,seq(5,95,5),111), right = FALSE)) %>%
      mutate(Agegroup = forcats::fct_recode(factor(Agegroup),
                                            "0"="[0,1)",
                                            "1"="[1,2)",
                                            "2"="[2,3)",
                                            "3"="[3,4)",
                                            "4"="[4,5)",
                                            "5-9"="[5,10)",
                                            "10-14"="[10,15)",
                                            "15-19"="[15,20)",
                                            "20-24"="[20,25)",
                                            "25-29"="[25,30)",
                                            "30-34"="[30,35)",
                                            "35-39"="[35,40)",
                                            "40-44"="[40,45)",
                                            "45-49"="[45,50)",
                                            "50-54"="[50,55)",
                                            "55-59"="[55,60)",
                                            "60-64"="[60,65)",
                                            "65-69"="[65,70)",
                                            "70-74"="[70,75)",
                                            "75-79"="[75,80)",
                                            "80-84"="[80,85)",
                                            "85-89"="[85,90)",
                                            "90-94"="[90,95)",
                                            "95+"="[95,111)"))
    out1$Agegroup<-as.character(out1$Agegroup)
    out1$icd<-as.character(out1$icd)
    icd_datab$icd<-as.character(icd_datab$icd)
    out1 <- out1 %>% left_join(.,icd_datab,by = c("Year","icd","Agegroup")) %>% filter(Year > 1978)
    out1$icd<-as.factor(out1$icd)
    out1$Cause<-as.numeric(out1$icd)
    out1<-subset(out1,select=c(Year,Age,Cause,icd,icd_prop))
    
    for(j in 1:5){
      CAU1 <- (matrix(newlongcty1$cause[newlongcty1$Cause==j],111))
      CAU2 <- (matrix(out1$icd_prop[out1$Cause==j],111))
      
      Repl[i,j] <- CALDecompFunctionCause(qx1,qx2,Y2,Y1,cty1,cty2,CAU1,CAU2,sex,j,j)
    }
    print(paste("Iteration n",i,sep=":"))
  }
  return(Repl)
}


###application of the function for 95% CI
rm(list=ls())
library(parallel)
library(foreach)
library(doParallel)
library(plyr)
library(reshape2)
library(dplyr)
library(tidyverse)

source("TCAL functions.R")
options(scipen=10)
no_cores <- 7
cl <- makeCluster(no_cores)
registerDoParallel(cl)

cty <- "HICs"
SEx <- c("Male","Female")
clusterExport(cl,list("melt","CALDecompFunctionCausebootbroadhic"))
repli <- 20
res = foreach(i = 2, 
              .combine = "rbind",.packages = "dplyr") %dopar% {
                j <- ifelse(i==1,1,2)
                x <- CALDecompFunctionCausebootbroadhic("hk","hic",sex=SEx[j],R=repli)
                x}

stopCluster(cl)
hichk <- res[1:repli,]

hicdat <- as.data.frame(hichk)


colnames(hicdat) <- c("Neoplasm", "Cardiovascular", "External", "other","Respiratory")


hicdat2 <- reshape2::melt(hicdat)


hic <- summarySE(hicdat2, measurevar="value", groupvars="variable")

hic$Country1 <- "HICs"

hic$CI <- 1.96*hic$sd

hic <- hic %>% dplyr::rename("Confidence interval" = "CI","Country compared" = "Country1")

