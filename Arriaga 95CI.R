#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
## function for constructing a classic (& rather general) lifetable
lifetable <- function(x, Nx, Dx, sex="M", ax=NULL){
  m <- length(x)
  mx  <- Dx/Nx
  n <- c(diff(x), NA)
  if(is.null(ax)){
    ax <- rep(0,m)
    if(x[1]!=0 | x[2]!=1){
      ax <- n/2
      ax[m] <- 1 / mx[m]
    }else{    
      if(sex=="F"){
        if(mx[1]>=0.107){
          ax[1] <- 0.350
        }else{
          ax[1] <- 0.053 + 2.800*mx[1]
        }
      }
      if(sex=="M"){
        if(mx[1]>=0.107){
          ax[1] <- 0.330
        }else{
          ax[1] <- 0.045 + 2.684*mx[1]
        }
      }
      ax[-1] <- n[-1]/2
      ax[m] <- 1 / mx[m]
    }
  }
  qx  <- n*mx / (1 + (n - ax) * mx)
  qx[m] <- 1
  px  <- 1-qx
  lx  <- cumprod(c(1,px))*100000
  dx  <- -diff(lx)
  Lx  <- n*lx[-1] + ax*dx
  lx <- lx[-(m+1)]
  Lx[m] <- lx[m]/mx[m]
  Lx[is.na(Lx)] <- 0 ## in case of NA values
  Lx[is.infinite(Lx)] <- 0 ## in case of Inf values
  Tx  <- rev(cumsum(rev(Lx)))
  ex  <- Tx/lx
  return.df <- data.frame(x, n, Nx, Dx, mx, ax, qx, px, lx, dx, Lx, Tx, ex)
  return(return.df)
}
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
## function for constructing a lifetable starting from probabilities
lifetable.qx <- function(x, qx, sex="M", ax=NULL, last.ax=5.5){
  m <- length(x)
  n <- c(diff(x), NA)
  qx[is.na(qx)] <- 0
  if(is.null(ax)){
    ax <- rep(0,m)
    if(x[1]!=0 | x[2]!=1){
      ax <- n/2
      ax[m] <- last.ax
    }else{    
      if(sex=="F"){
        if(qx[1]>=0.1){
          ax[1] <- 0.350
        }else{
          ax[1] <- 0.05 + 3*qx[1]
        }
      }
      if(sex=="M"){
        if(qx[1]>=0.1){
          ax[1] <- 0.33
        }else{
          ax[1] <- 0.0425 + 2.875*qx[1]
        }
      }
      ax[-1] <- n[-1]/2
      ax[m] <- last.ax
    }
  }
  px  <- 1-qx
  lx  <- cumprod(c(1,px))*100000
  dx  <- -diff(lx)
  Lx  <- n*lx[-1] + ax*dx
  lx <- lx[-(m+1)]
  Lx[m] <- lx[m]*last.ax
  Lx[is.na(Lx)] <- 0 ## in case of NA values
  Lx[is.infinite(Lx)] <- 0 ## in case of Inf values
  Tx  <- rev(cumsum(rev(Lx)))
  ex  <- Tx/lx
  return.df <- data.frame(x, n, ax, qx, px, lx, dx, Lx, Tx, ex)
  return(return.df)
}

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
ltrsmpl <- function(x, Nx, Dx, sex, ax = NULL,
                 ns=1000, level=0.95){
  ## point-estimated lifetable
  LT <- MortalityLaws::LifeTable(x, Ex = Nx, Dx = Dx, sex = sex, ax = ax)$lt
  ## number of ages
  m <- nrow(LT)
  ## estimated probs
  qx <- LT$qx
  ## trials for binomial
  Ntil <- round(Dx/qx)
  Ntil[is.na(Ntil)] <- 100000
  ## ax for last age
  last.ax <- LT$ax[m]
  ## simulated death counts
  ## from Binomial distribution
  Y <- suppressWarnings(matrix(rbinom(m*ns,Ntil,qx),m,ns))
  ## simulated probabilities
  QX <- Y/Ntil
  ## for all replicates, compute life expectancy
  ## ## by a for-loop
  ## exsimA <- rep(0,ns)
  ## for(s in 1:ns){
  ##   exsimA[s] <-lifetable.qx(x, qx=QX[,s], sex,
  ##                        last.ax=last.ax)$ex[wh]
  ## }
  ## by apply command
  ## (slighly faster, less intuitive)
  fun.ex <- function(qx){
    return(MortalityLaws::LifeTable(x=x, qx = qx, sex=sex)$lt)
  }
    exsim <- apply(QX, 2, fun.ex)
    for (i in 1 :1000){
      exsim[[i]][111,]$qx = 1
    }
    return(exsim)
}
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Load HMD data repeat process for all countries
CountryA<-c("../Life Expectancy Task/")
Names <- c("USA")

Country<-paste(CountryA,"/",Names[1],sep="")
setwd(Country)
usa_exposures<-read.table("Exposures_1x1.txt",header=TRUE,fill=TRUE,skip=1)
usa_deaths<-read.table("Deaths_1x1.txt",header=TRUE,fill=TRUE,skip=1)

# Data cleaning for Belgium and Germany data
bel_exposures <- bel_exposures %>% filter(Year>1949)
bel_exposures$Female <- as.numeric(as.character(bel_exposures$Female))
bel_exposures$Male <- as.numeric(as.character(bel_exposures$Male))
bel_exposures$Total <- as.numeric(as.character(bel_exposures$Total))

bel_deaths <- bel_deaths %>% filter(Year>1949)
bel_deaths$Female <- as.numeric(as.character(bel_deaths$Female))
bel_deaths$Male <- as.numeric(as.character(bel_deaths$Male))
bel_deaths$Total <- as.numeric(as.character(bel_deaths$Total))

deutnp_deaths <- data.frame(Year = deute_deaths$Year,Age = deute_deaths$Age,Female = deute_deaths$Female + deutw_deaths$Female,Male = deute_deaths$Male + deutw_deaths$Male,Total = deute_deaths$Total + deutw_deaths$Total)
deutnp_exposures <- data.frame(Year = deute_exposures$Year,Age = deute_exposures$Age,Female = deute_exposures$Female + deutw_exposures$Female,Male = deute_exposures$Male + deutw_exposures$Male,Total = deute_exposures$Total + deutw_exposures$Total)
#-------------------------------------------------------------------------------------------------------------------------------------------------------
# Try
hic_ltbs <- function(Nxdata,Dxdata,sex,year){
  out <- ltrsmpl(0:110,Nx=Nxdata[Nxdata$Year==year,][sex][,1],Dx=Dxdata[Dxdata$Year==year,][sex][,1],sex = tolower(sex))
  out <- lapply(out, function(x) x%>% select(qx))
  out
}

hk_m16 <- hic_ltbs(hk_exposures,hk_deaths,'Male',2016)
hk_f16 <- hic_ltbs(hk_exposures,hk_deaths,'Female',2016)


hk_m16_lt <- list()
for (i in 1:1000) {
  hk_m16_lt[[i]] <- hk_m16[[i]] %>% MortalityLaws::LifeTable(0:110,qx=.)
  hk_m16_lt[[i]] <- hk_m16_lt[[i]]$lt
}

hk_f16_lt <- list()
for (i in 1:1000) {
  hk_f16_lt[[i]] <- hk_f16[[i]] %>% MortalityLaws::LifeTable(0:110,qx=.)
  hk_f16_lt[[i]] <- hk_f16_lt[[i]]$lt
}

hk_m16_ex <- list()
for (i in 1:1000) {
  hk_m16_ex[[i]] <-  hk_m16_lt[[i]][1,]$ex
}
do.call(rbind,hk_m16_ex) %>% summary()

hk_f16_ex <- list()
for (i in 1:1000) {
  hk_f16_ex[[i]] <-  hk_f16_lt[[i]][1,]$ex
}
do.call(rbind,hk_f16_ex) %>% summary()
#-------------------------------------------------------------------------------------------------------------------------------

hic_m79 <- Map(cbind,hic_ltbs(aus_exposures,aus_deaths,'Male',1979),hic_ltbs(aut_exposures,aut_deaths,'Male',1979),
             hic_ltbs(bel_exposures,bel_deaths,'Male',1979),hic_ltbs(can_exposures,can_deaths,'Male',1979),
             hic_ltbs(che_exposures,che_deaths,'Male',1979),hic_ltbs(dnk_exposures,dnk_deaths,'Male',1979),
             hic_ltbs(deutnp_exposures,deutnp_deaths,'Male',1979),hic_ltbs(esp_exposures,esp_deaths,'Male',1979),
             hic_ltbs(fin_exposures,fin_deaths,'Male',1979),hic_ltbs(fratnp_exposures,fratnp_deaths,'Male',1979),
             hic_ltbs(gbr_exposures,gbr_deaths,'Male',1979),hic_ltbs(ita_exposures,ita_deaths,'Male',1979),
             hic_ltbs(jpn_exposures,jpn_deaths,'Male',1979),hic_ltbs(nld_exposures,nld_deaths,'Male',1979),
             hic_ltbs(nor_exposures,nor_deaths,'Male',1979),hic_ltbs(prt_exposures,prt_deaths,'Male',1979),
             hic_ltbs(swe_exposures,swe_deaths,'Male',1979),hic_ltbs(usa_exposures,usa_deaths,'Male',1979))

hic_f79 <- Map(cbind,hic_ltbs(aus_exposures,aus_deaths,'Female',1979),hic_ltbs(aut_exposures,aut_deaths,'Female',1979),
             hic_ltbs(bel_exposures,bel_deaths,'Female',1979),hic_ltbs(can_exposures,can_deaths,'Female',1979),
             hic_ltbs(che_exposures,che_deaths,'Female',1979),hic_ltbs(dnk_exposures,dnk_deaths,'Female',1979),
             hic_ltbs(deutnp_exposures,deutnp_deaths,'Female',1979),hic_ltbs(esp_exposures,esp_deaths,'Female',1979),
             hic_ltbs(fin_exposures,fin_deaths,'Female',1979),hic_ltbs(fratnp_exposures,fratnp_deaths,'Female',1979),
             hic_ltbs(gbr_exposures,gbr_deaths,'Female',1979),hic_ltbs(ita_exposures,ita_deaths,'Female',1979),
             hic_ltbs(jpn_exposures,jpn_deaths,'Female',1979),hic_ltbs(nld_exposures,nld_deaths,'Female',1979),
             hic_ltbs(nor_exposures,nor_deaths,'Female',1979),hic_ltbs(prt_exposures,prt_deaths,'Female',1979),
             hic_ltbs(swe_exposures,swe_deaths,'Female',1979),hic_ltbs(usa_exposures,usa_deaths,'Female',1979))

hic_m16 <- Map(cbind,hic_ltbs(aus_exposures,aus_deaths,'Male',2016),hic_ltbs(aut_exposures,aut_deaths,'Male',2016),
               hic_ltbs(bel_exposures,bel_deaths,'Male',2016),hic_ltbs(can_exposures,can_deaths,'Male',2016),
               hic_ltbs(che_exposures,che_deaths,'Male',2016),hic_ltbs(dnk_exposures,dnk_deaths,'Male',2016),
               hic_ltbs(deutnp_exposures,deutnp_deaths,'Male',2016),hic_ltbs(esp_exposures,esp_deaths,'Male',2016),
               hic_ltbs(fin_exposures,fin_deaths,'Male',2016),hic_ltbs(fratnp_exposures,fratnp_deaths,'Male',2016),
               hic_ltbs(gbr_exposures,gbr_deaths,'Male',2016),hic_ltbs(ita_exposures,ita_deaths,'Male',2016),
               hic_ltbs(jpn_exposures,jpn_deaths,'Male',2016),hic_ltbs(nld_exposures,nld_deaths,'Male',2016),
               hic_ltbs(nor_exposures,nor_deaths,'Male',2016),hic_ltbs(prt_exposures,prt_deaths,'Male',2016),
               hic_ltbs(swe_exposures,swe_deaths,'Male',2016),hic_ltbs(usa_exposures,usa_deaths,'Male',2016))

hic_f16 <- Map(cbind,hic_ltbs(aus_exposures,aus_deaths,'Female',2016),hic_ltbs(aut_exposures,aut_deaths,'Female',2016),
               hic_ltbs(bel_exposures,bel_deaths,'Female',2016),hic_ltbs(can_exposures,can_deaths,'Female',2016),
               hic_ltbs(che_exposures,che_deaths,'Female',2016),hic_ltbs(dnk_exposures,dnk_deaths,'Female',2016),
               hic_ltbs(deutnp_exposures,deutnp_deaths,'Female',2016),hic_ltbs(esp_exposures,esp_deaths,'Female',2016),
               hic_ltbs(fin_exposures,fin_deaths,'Female',2016),hic_ltbs(fratnp_exposures,fratnp_deaths,'Female',2016),
               hic_ltbs(gbr_exposures,gbr_deaths,'Female',2016),hic_ltbs(ita_exposures,ita_deaths,'Female',2016),
               hic_ltbs(jpn_exposures,jpn_deaths,'Female',2016),hic_ltbs(nld_exposures,nld_deaths,'Female',2016),
               hic_ltbs(nor_exposures,nor_deaths,'Female',2016),hic_ltbs(prt_exposures,prt_deaths,'Female',2016),
               hic_ltbs(swe_exposures,swe_deaths,'Female',2016),hic_ltbs(usa_exposures,usa_deaths,'Female',2016))

hic_m79_lt <- list()
for (i in 1:1000) {
  hic_m79_lt[[i]] <- (hic_m79[[i]] %>% rowMeans(.,na.rm = T))[c(1:100,111)] %>% MortalityLaws::LifeTable(0:100,qx=.)
  hic_m79_lt[[i]] <- hic_m79_lt[[i]]$lt
}

hic_f79_lt <- list()
for (i in 1:1000) {
  hic_f79_lt[[i]] <- (hic_f79[[i]] %>% rowMeans(.,na.rm = T))[c(1:100,111)] %>% MortalityLaws::LifeTable(0:100,qx=.)
  hic_f79_lt[[i]] <- hic_f79_lt[[i]]$lt
}

hic_m16_lt <- list()
for (i in 1:1000) {
  hic_m16_lt[[i]] <- (hic_m16[[i]] %>% rowMeans(.,na.rm = T))[c(1:100,111)]%>% MortalityLaws::LifeTable(0:100,qx=.)
  hic_m16_lt[[i]] <- hic_m16_lt[[i]]$lt
}

hic_f16_lt <- list()
for (i in 1:1000) {
  hic_f16_lt[[i]] <- (hic_f16[[i]] %>% rowMeans(.,na.rm = T))[c(1:100,111)] %>% MortalityLaws::LifeTable(0:100,qx=.)
  hic_f16_lt[[i]] <- hic_f16_lt[[i]]$lt
}

hic_m16_ex <- list()
for (i in 1:1000) {
  hic_m16_ex[[i]] <-  hic_m16_lt[[i]][51,]$ex
}
do.call(rbind,hic_m16_ex) %>% summary()

hic_f16_ex <- list()
for (i in 1:1000) {
  hic_f16_ex[[i]] <-  hic_f16_lt[[i]][1,]$ex
}
do.call(rbind,hic_f16_ex) %>% summary()

# Age decomposition
age_decomp <- function(list1,list2)
{
  l0 <- 100000
  lx1 <- list1$lx
  lx2 <- list2$lx
  Lx1 <- list1$Lx
  Lx2 <- list2$Lx
  Tx1 <- list1$Tx
  Tx2 <- list2$Tx
  mx1 <- list1$mx
  mx2 <- list2$mx
  
  # decomposing by Age
  age_contri <- NULL
  for(i in 1:100){
    age_contri[i] <- lx1[i]/l0 * (Lx2[i]/lx2[i] - Lx1[i]/lx1[i]) + (Tx2[i+1]/l0) * (lx1[i]/lx2[i] - lx1[i+1]/lx2[i+1])
    age_contri[101] <- lx1[101]/l0 * (Tx2[101]/lx2[101]-Tx1[101]/lx1[101])
  }
  return(age_contri)
}

outm <- list()
outf <- list()
for (i in 1:1000) {
  outm[[i]] <- age_decomp(hic_m16_lt[[i]],hk_m16_lt[[i]]) %>% data.frame(age = 0:100,contri = .)
  outf[[i]] <- age_decomp(hic_f16_lt[[i]],hk_f16_lt[[i]]) %>% data.frame(age = 0:100,contri = .)
}

outm_total <- list()
outf_total <- list()
for (i in 1:1000) {
  outm_total[[i]] <- sum(outm[[i]]$contri)
  outf_total[[i]] <- sum(outf[[i]]$contri)
  }

listm80_perc <- list()
listm80_contri <- list()
listf80_perc <- list()
listf80_contri <- list()
for (i in 1:1000) {
  listm80_contri[[i]] <- (outm[[i]] %>% mutate(agegroup = cut(age,breaks = c(-1,79,150))) %>% group_by(agegroup) %>% summarize(contri = sum(contri)) %>%
                            mutate(perc = contri/sum(contri)) %>% select(contri))[2,] %>% as.numeric() 
  listm80_perc[[i]] <- (outm[[i]] %>% mutate(agegroup = cut(age,breaks = c(-1,79,150))) %>% group_by(agegroup) %>% summarize(contri = sum(contri)) %>%
                          mutate(perc = contri/sum(contri)) %>% select(perc))[2,] %>% as.numeric()
  listf80_contri[[i]] <- (outf[[i]] %>% mutate(agegroup = cut(age,breaks = c(-1,79,150))) %>% group_by(agegroup) %>% summarize(contri = sum(contri)) %>%
                          mutate(perc = contri/sum(contri)) %>% select(contri))[2,] %>% as.numeric()  
  listf80_perc[[i]] <- (outf[[i]] %>% mutate(agegroup = cut(age,breaks = c(-1,79,150))) %>% group_by(agegroup) %>% summarize(contri = sum(contri)) %>%
     mutate(perc = contri/sum(contri)) %>% select(perc))[2,] %>% as.numeric()  
}

m80_contri <- do.call(rbind, listm80_contri)
quantile(m80_contri,c(0.025,0.975))
m80_perc <- do.call(rbind, listm80_perc)
quantile(m80_perc,c(0.025,0.975))
f80_contri <- do.call(rbind, listf80_contri)
quantile(f80_contri,c(0.025,0.975))
f80_perc <- do.call(rbind, listf80_perc)
quantile(f80_perc,c(0.025,0.975))

count2rows2broad <- function(cou){
  n <- sum(cou)
  p <- cou/n
  if(n>0){
    newdat <- cbind(c(1:12), Freq = rmultinom(1, n, p))
  }
  if(n==0){
    newdat <- cbind(c(1:12), Freq = rep(0, 12))
  }
  return(newdat[,2])
}

# Resampling number of deaths
Rx_hic$Strat <- paste(Rx_hic$Agegroup,Rx_hic$Year,Rx_hic$Sex,Rx_hic$Country,sep=":")
Rx_hic16 <- Rx_hic %>% filter(Year == 2016)
Rx_hic16others <- Rx_hic16 %>% group_by(Country,Year,Sex,Agegroup) %>% summarize(Deaths = sum(Deaths)) %>% mutate(COD='Others') %>%
  left_join(.,Rx_hic16[Rx_hic16$COD=='Neoplasms',][,c(1:3,5,7,9:10)]) %>% mutate(Deaths=`Total deaths`-Deaths) %>% mutate(Rx = Deaths/`Total deaths`)
Rx_hic16 <- rbind(Rx_hic16,Rx_hic16others)

Rx_hk16 <- Rx %>% rename("Agegroup" = "Age") %>% mutate(Sex = ifelse(Sex == 1, "Male", "Female")) %>%
  left_join(.,total_exposures,by = c("Year","Country","Sex","Agegroup")) %>% filter(Country == 'Hong Kong' & Year == 2016 & Agegroup != 'Total')
Rx_hk16others <- Rx_hk16 %>% group_by(Country,Year,Sex,Agegroup) %>% summarize(Deaths = sum(Deaths)) %>% mutate(COD='Others') %>%
  left_join(.,Rx_hk16[Rx_hk16$COD=='Neoplasms',][,c(1:3,5,7,9)],by = c("Country", "Year", "Sex", "Agegroup")) %>% mutate(Deaths=`Total deaths`-Deaths) %>% mutate(Rx = Deaths/`Total deaths`)
Rx_hk16 <- rbind(Rx_hk16,Rx_hk16others)
Rx_hk16$Strat <- paste(Rx_hk16$Agegroup,Rx_hk16$Year,Rx_hk16$Sex,Rx_hk16$Country,sep=":")
cod_id <- data.frame(COD = Rx_hic16$COD %>% unique(), cod = paste0('V',1:13))

a1 <- list()
b1 <- list()
c1 <- list()
char <- list()
for (i in 1:1000) {
a1[[i]] <- tapply(Rx_hic16$Deaths,Rx_hic16$Strat,count2rows2broad)
b1[[i]] <- as.data.frame(do.call(rbind, a1[[i]]))
b1[[i]]$id <- row.names(b1[[i]])
c1[[i]] <- tidyr::gather(b1[[i]], cod,n,V1:V13,factor_key = T)
char[[i]] <- stringr::str_split(c1[[i]]$id,":")
char[[i]] <- do.call(rbind.data.frame,char[[i]])
colnames(char[[i]]) <- c('Agegroup','Year','Sex','Country')
c1[[i]] <- cbind(char[[i]],c1[[i]])
c1[[i]] <- c1[[i]] %>% left_join(.,cod_id,by = 'cod')
c1[[i]] <- suppressMessages(c1[[i]] %>% left_join(.,c1[[i]] %>% group_by(Year,Sex,Country,Agegroup) %>% summarize(N = sum(n)),by=c('Year','Sex','Country','Agegroup')))
c1[[i]]$Year <- as.numeric(c1[[i]]$Year)
c1[[i]] <- c1[[i]] %>% left_join(.,Rx_hic16[,c(1:5,9)],by=c('Country','Year','Sex','COD','Agegroup')) 
}

hic_rx <- list()
for (i in 1:1000) {
  hic_rx[[i]] <- suppressMessages(c1[[i]] %>% mutate(mx = N/Pop,rx = n/N) %>% group_by(Agegroup,Year,Sex,COD) %>% summarize(mx = mean(mx,na.rm=T), rx = mean(rx,na.rm=T)))
}

d1 <- list()
e1 <- list()
f1 <- list()
char2 <- list()
for (i in 1:1000) {
  d1[[i]] <- tapply(Rx_hk16$Deaths,Rx_hk16$Strat,count2rows2broad)
  e1[[i]] <- as.data.frame(do.call(rbind, d1[[i]]))
  e1[[i]]$id <- row.names(e1[[i]])
  f1[[i]] <- tidyr::gather(e1[[i]], cod,n,V1:V13,factor_key = T)
  char2[[i]] <- stringr::str_split(f1[[i]]$id,":")
  char2[[i]] <- do.call(rbind.data.frame,char2[[i]])
  colnames(char2[[i]]) <- c('Agegroup','Year','Sex','Country')
  f1[[i]] <- cbind(char2[[i]],f1[[i]])
  f1[[i]] <- f1[[i]] %>% left_join(.,cod_id,by = 'cod')
  f1[[i]] <- suppressMessages(f1[[i]] %>% left_join(.,f1[[i]] %>% group_by(Year,Sex,Country,Agegroup) %>% summarize(N = sum(n)),by=c('Year','Sex','Country','Agegroup')))
  f1[[i]]$Year <- as.numeric(f1[[i]]$Year)
  f1[[i]] <- f1[[i]] %>% left_join(.,Rx_hk16[,c(1:5,9)],by=c('Country','Year','Sex','COD','Agegroup')) 
}

hk_rx <- list()
for (i in 1:1000) {
  hk_rx[[i]] <- f1[[i]] %>% mutate(mx1 = N/Pop,rx1 = n/N) %>% group_by(Agegroup,Year,Sex,COD) %>% summarize(mx1 = mean(mx1,na.rm=T), rx1 = mean(rx1,na.rm=T))
}

outb <- list()
for (i in 1:1000) {
  outm[[i]] <- outm[[i]] %>% mutate(Agegroup = cut(age, c(0,1,seq(5,105,5)), right = FALSE)) %>% mutate(Agegroup = forcats::fct_recode(factor(Agegroup),
                                                                                                          "0"="[0,1)",
                                                                                                          "1-4"="[1,5)",
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
                                                                                                          "85+"="[85,90)",
                                                                                                          "85+"="[90,95)",
                                                                                                          "85+"="[95,100)",
                                                                                                          "85+"="[100,105)"
)) %>% group_by(Agegroup) %>% summarize(contri = sum(contri)) %>% mutate(Sex = 'Male')
  outf[[i]] <- outf[[i]] %>% mutate(Agegroup = cut(age, c(0,1,seq(5,105,5)), right = FALSE)) %>% mutate(Agegroup = forcats::fct_recode(factor(Agegroup),
                                                                                                                                       "0"="[0,1)",
                                                                                                                                       "1-4"="[1,5)",
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
                                                                                                                                       "85+"="[85,90)",
                                                                                                                                       "85+"="[90,95)",
                                                                                                                                       "85+"="[95,100)",
                                                                                                                                       "85+"="[100,105)"
  )) %>% group_by(Agegroup) %>% summarize(contri = sum(contri)) %>% mutate(Sex = 'Female')
  outb[[i]] <- rbind(outm[[i]],outf[[i]])
}

cvd60f <- list()
for (i in 1:1000){
  cvd60f[[i]] <- hic_rx[[i]] %>% left_join(.,hk_rx[[i]],by=c('Year','Sex','COD','Agegroup')) %>%
  left_join(.,outb[[i]],by=c('Agegroup','Sex')) %>%
  mutate(age_cod_contri = contri*((rx1*mx1-rx*mx)/(mx1-mx))) %>%
  filter(COD == 'Cardiovascular diseases' & Agegroup %in% c('60-64','65-69','70-74','75-79','80-84','85+') & Sex == 'Female') %>%
  data.frame() %>% select(age_cod_contri) %>% sum()
}

cvd60m <- list()
for (i in 1:1000){
  cvd60m[[i]] <- hic_rx[[i]] %>% left_join(.,hk_rx[[i]],by=c('Year','Sex','COD','Agegroup')) %>%
    left_join(.,outb[[i]],by=c('Agegroup','Sex')) %>%
    mutate(age_cod_contri = contri*((rx1*mx1-rx*mx)/(mx1-mx))) %>%
    filter(COD == 'Cardiovascular diseases' & Agegroup %in% c('60-64','65-69','70-74','75-79','80-84','85+') & Sex == 'Male') %>%
    data.frame() %>% select(age_cod_contri) %>% sum()
}

save.image('D:/C/Tasks/Life Expectancy Task/Recent files/Script/bootstrap data.RData')
