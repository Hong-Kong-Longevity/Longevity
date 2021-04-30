library(dplyr)
library(data.table)

source('C:/Users/FreyaYu/Documents/Tasks/Life Expectancy Task/R Scripts/Nature medicine/LifeTableFUN.R')

# Load datasets
## Repeat for all countries of interest
CountryA <- 'Hong Kong'
Country<-paste('C:/Users/FreyaYu/Documents/Tasks/Life Expectancy Task/Recent files/Data/Exposure/',CountryA,sep="")
setwd(Country)

hk_death <- fread("Deaths_1x1.txt") %>% data.frame()
hk_exposure <- fread("Exposures_1x1.txt") %>% data.frame()
#-----------------------------------------------------------------------------------------------------------------------------------------------------------
# Update by sex
hk_mx <- data.frame(Year = hk_exposure$Year,Age = hk_exposure$Age,exposure = hk_exposure$Female,deaths = hk_death$Female) %>% mutate(mx = deaths/exposure)
japan_mx <- data.frame(Year = japan_exposure$Year,Age = japan_exposure$Age,exposure = japan_exposure$Female,deaths = japan_death$Female) %>% mutate(mx = deaths/exposure)


e0_95ci_hk <- function(i){
  c((CIex(0:110,hk_mx[hk_mx$Year== i,]$exposure,hk_mx[hk_mx$Year== i,]$death))$meanex,
    (CIex(0:110,hk_mx[hk_mx$Year== i,]$exposure,hk_mx[hk_mx$Year== i,]$death))$CIex, Year = round(i,0))
}

hk_e0_95ci_total <- do.call(rbind,lapply(c(1986:2017),e0_95ci_hk))
hk_e0_95ci_female <- do.call(rbind,lapply(c(1986:2017),e0_95ci_hk))
hk_e0_95ci_male <- do.call(rbind,lapply(c(1986:2017),e0_95ci_hk))

e0_95ci_jpn <- function(i){
  c((CIex(0:110,japan_mx[japan_mx$Year== i,]$exposure,japan_mx[japan_mx$Year== i,]$death))$meanex,
    (CIex(0:110,japan_mx[japan_mx$Year== i,]$exposure,japan_mx[japan_mx$Year== i,]$death))$CIex, Year = round(i,0))
}

japan_e0_95ci_total <- do.call(rbind,lapply(c(1986:2017),e0_95ci_jpn)) 
japan_e0_95ci_female <- do.call(rbind,lapply(c(1986:2017),e0_95ci_jpn)) 
japan_e0_95ci_male <- do.call(rbind,lapply(c(1986:2017),e0_95ci_jpn)) 

data_hk <- rbind(hk_e0_95ci_total,hk_e0_95ci_female,hk_e0_95ci_male) %>% data.frame() %>% rename("ex"="V1", "ex_ll"="X2.5.", "ex_ul"="X97.5.") %>% mutate(Country = "Hong Kong", Sex = rep(c("Overall","Female","Male"),each = 32))
data_japan <- rbind(japan_e0_95ci_total,japan_e0_95ci_female,japan_e0_95ci_male) %>% data.frame() %>% rename("ex"="V1", "ex_ll"="X2.5.", "ex_ul"="X97.5.") %>% mutate(Country = "Japan", Sex = rep(c("Overall","Female","Male"),each = 32))
data <- rbind(data_hk,data_japan)

p_overall <- ggplot(data %>% filter(Sex == "Overall"),aes(Year,ex,color = Country)) + geom_line() + geom_ribbon(aes(ymin = ex_ll,ymax = ex_ul,fill = Country),size = .75,alpha = 0.2) + theme_hc() +
  scale_x_continuous(breaks = c(1986,1990,2000,2010,2017)) + scale_y_continuous(breaks = seq(75,85,2),limits = c(75,85)) + labs(title = "Overall", x = "\nYear") +
  theme(legend.position = c(0.2,0.85),legend.title = element_blank(),axis.title = element_text(size = 16),axis.title.y = element_blank(),axis.text = element_text(size = 14), plot.title = element_text(size = 18, face = "bold"))

ggpubr::annotate_figure(ggpubr::ggarrange(p_overall + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()),
                          p_male + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()),
                          p_female,nrow = 3,common.legend = T,legend = "top"),left = ggpubr::text_grob("Life expectancy at birth (Years)",rot = 90,face = "bold", size = 18))

ggsave("C:/Users/FreyaYu/Documents/Tasks/Life Expectancy Task/Recent files/Results/Plots/LE HK vs Japan.jpg",dpi = 300,height = 12,width = 12)
save.image("C:/Users/FreyaYu/Documents/Tasks/Life Expectancy Task/Recent files/Data/LE HK vs Japan.RData")


