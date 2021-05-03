library(data.table)
library(dplyr)
library(forcats)
library(ggplot2)
library(ggthemes)
library(ggpubr)

setwd("../Life expectancy/CS&D Known Deaths")

## 2001
format_DeathData0105 <- function(yy){
  death_HK <- fread(paste0("DEATH",yy,".DAT"),header=FALSE,sep="\t")
  death_HK <- data.table(substring(death_HK$V1,1,32)) %>%
    # Decode, see file "Kd-sale[1]"
    mutate(Year=substring(V1,12,15), # Year of Death
           Month=substring(V1,9,10), # Month of Death
           Sex=factor(substring(V1,5,5)), # Sex
           age_def=substring(V1,1,1), # Age definition
           Age=as.numeric(substring(V1,2,4)), # Age
           cod=factor(substring(V1,16,19)), # Cause of Death
           E_cod=factor(substring(V1,20,23)), # External cause of Death
           PrevResidence=substring(V1,31,32),
           HK_sinceBirth=substring(V1,29,30)=="90",
           LengthHK=substring(V1,29,30),
           TPU = NA,
           Area = NA) 
  
  levels(death_HK$Sex) <- c("Female","Male",NA)
  # Add '.' to cod
  label <- levels(death_HK$cod)
  label <- gsub('([A-Z][0-9]{2})([0-9])','\\1.\\2',label)
  label <- gsub(' ','',label)
  levels(death_HK$cod) <- label
  label <- levels(death_HK$E_cod)
  label <- gsub('([A-Z][0-9]{2})([0-9])','\\1.\\2',label)
  label <- gsub(' ','',label)
  levels(death_HK$E_cod) <- label
  death_HK <- data.table(death_HK)
  
  death_HK[!E_cod %in% c('EXXX','E00.0'), cod:=E_cod]
  
  death_HK[age_def=="X",Age:=NA]
  death_HK[age_def %in% c("M","D"),Age:=0]			
  death_HK[LengthHK!='90',Length_Not_HK := as.numeric(Age) - as.numeric(LengthHK)]
  death_HK[Age>99]
  
  levels(death_HK$cod)[levels(death_HK$cod)=="XXX"] <- NA # assign NA to XXX
  return(death_HK)
}
DeathData01 <- format_DeathData0105("01")

## 2006
format_DeathData06 <- function(yy){
  death_HK <- fread("death2006.csv")
  names(death_HK) <- gsub(" ","_",names(death_HK))
  death_HK <- death_HK  %>%
    mutate(V1=NA,
           Year=2006,
           Month=NA,
           Sex=factor(sex,labels=c("Female","Male",NA)),
           age_def=age_definition,
           Age=age,
           cod=factor(cause_of_death),
           E_cod=factor(external_cause_of_death),
           PrevResidence=country_of_previous_residence,
           HK_sinceBirth=length_of_stay_in_HK=="90",
           LengthHK=length_of_stay_in_HK,
           TPU = NA,
           Area = NA) %>%
    select(V1,Year,Month,Sex,age_def,Age,cod,E_cod,PrevResidence,HK_sinceBirth,LengthHK, TPU, Area)
  # Add '.' to cod
  label <- levels(death_HK$cod)
  three_digits <- setdiff((1:length(label)),grep(' ',label))
  label[three_digits] <- gsub('([A-Z][0-9]{2})([0-9])','\\1.\\2',label[three_digits])
  label <- gsub(' ','',label)
  levels(death_HK$cod) <- label	
  label <- levels(death_HK$E_cod)
  three_digits <- setdiff((1:length(label)),grep(' ',label))
  label[three_digits] <- gsub('([A-Z][0-9]{2})([0-9])','\\1.\\2',label[three_digits])
  label <- gsub(' ','',label)
  levels(death_HK$E_cod) <- label	
  
  death_HK <- data.table(death_HK)			
  death_HK[!E_cod %in% c('EXXX','E00.0'), cod:=E_cod]
  
  death_HK[age_def=="X",Age:=NA]
  death_HK[age_def %in% c("M","D"),Age:=0]			
  death_HK[LengthHK!='90',Length_Not_HK := as.numeric(Age) - as.numeric(LengthHK)]
  death_HK[Age>99]
  
  levels(death_HK$cod)[levels(death_HK$cod)=="XXX"] <- NA # assign NA to XXX
  return(death_HK)
}
DeathData06 <- format_DeathData06("06")

## 2011 & 2016
format_DeathData1017 <- function(yy){
  death_HK <- fread(paste0("death",yy,".DAT"),header=FALSE,sep="\t")
  death_HK <- data.table(substring(death_HK$V1,1,33)) %>%
    # Decode, see file "Kd-sale[1]"
    mutate(Year=substring(V1,12,15), # Year of Death
           Month=substring(V1,9,10), # Month of Death
           Sex=factor(substring(V1,5,5)), # Sex
           age_def=substring(V1,1,1), # Age definition
           Age=as.numeric(substring(V1,2,4)), # Age
           cod=factor(substring(V1,16,19)), # Cause of Death
           E_cod=factor(substring(V1,20,23)), # External cause of Death
           PrevResidence=substring(V1,32,33),
           HK_sinceBirth=substring(V1,30,31)=="90",
           LengthHK=substring(V1,30,31),
           TPU = NA,
           Area = NA) 
  
  levels(death_HK$Sex) <- c("Female","Male",NA)
  # Add '.' to cod
  label <- levels(death_HK$cod)
  three_digits <- setdiff((1:length(label)),grep(' ',label))
  label[three_digits] <- gsub('([A-Z][0-9]{2})([0-9])','\\1.\\2',label[three_digits])
  label <- gsub(' ','',label)
  levels(death_HK$cod) <- label	
  label <- levels(death_HK$E_cod)
  three_digits <- setdiff((1:length(label)),grep(' ',label))
  label[three_digits] <- gsub('([A-Z][0-9]{2})([0-9])','\\1.\\2',label[three_digits])
  label <- gsub(' ','',label)
  levels(death_HK$E_cod) <- label	
  
  death_HK <- data.table(death_HK)
  death_HK[!E_cod %in% c('EXXX','E00.0'), cod:=E_cod]	
  
  death_HK[age_def=="X",Age:=NA]
  death_HK[age_def %in% c("M","D"),Age:=0]			
  death_HK[LengthHK!='90',Length_Not_HK := Age - as.numeric(LengthHK)]
  
  return(death_HK)
}
DeathData1116 <- do.call(rbind,lapply(c("11","16"),format_DeathData1017))

DeathData01 <- DeathData01 %>% select(Sex,Age,PrevResidence) %>% mutate(PrevResidence = ifelse(PrevResidence == "XX","Unknown",ifelse(PrevResidence %in% c("1","01"),"Mainland",ifelse(PrevResidence == "11","HK","NonHK"))), Year = 2001)
DeathData06 <- DeathData06 %>% select(Sex,Age,PrevResidence) %>% mutate(PrevResidence = ifelse(PrevResidence == "XX","Unknown",ifelse(PrevResidence %in% c("1","01"),"Mainland",ifelse(PrevResidence == "11","HK","NonHK"))), Year = 2006)
DeathData1116 <- DeathData1116 %>% select(Sex,Age,PrevResidence,Year) %>% mutate(PrevResidence = ifelse(PrevResidence == "XX","Unknown",ifelse(PrevResidence %in% c("1","01"),"Mainland",ifelse(PrevResidence == "11","HK","NonHK"))))

data <- rbind(DeathData01,DeathData06,DeathData1116) %>% filter(Year %in% c(2001,2006,2011,2016)) %>%
  select(Age,Sex,Year,PrevResidence)

data_mainland <- data %>% filter(PrevResidence == "Mainland")
data_nonhk <- data %>% mutate(PrevResidence = ifelse(PrevResidence=="HK","HK",ifelse(PrevResidence=="Unknown","Unknown","NonHK")))
data_nonhk <- rbind(data_nonhk,data_mainland)
data_nonhk <- data_nonhk %>% mutate(Age = ifelse(is.na(Age),"Unknown",ifelse(Age>99,100,Age))) %>%
  mutate(Sex = ifelse(is.na(Sex),"Unknown",Sex))
data_nonhk$Sex <- ifelse(data_nonhk$Sex==3,"Unknown",ifelse(data_nonhk$Sex==1,"Female","Male"))
data_nonhk$Age <- factor(data_nonhk$Age,levels = c(0:100,"Unknown"))
data_nonhk$Year <- as.numeric(data_nonhk$Year)
data_nonhk$Sex <- as.factor(data_nonhk$Sex)
data_nonhk$PrevResidence <- as.factor(data_nonhk$PrevResidence)
data_nonhk <- data_nonhk %>% table() %>% data.frame()
data_nonhk_prop <- data_nonhk %>% filter(PrevResidence == "Unknown") %>% group_by(Year) %>% summarize(Freq = sum(Freq)) %>% data.frame() %>%
  left_join(.,data %>% select(Year) %>% table() %>% data.frame() %>% rename("Year" = ".", "Total" = "Freq"),by = "Year") %>%
  mutate(prop = Total/(Total-Freq))
data_nonhk <- data_nonhk %>% left_join(.,data_nonhk_prop %>% select(Year,prop),by = "Year") %>% filter(Age != "Unknown") %>% filter(Sex != "Unknown") %>% filter(PrevResidence != "Unknown")
data_nonhk <- data_nonhk %>% mutate(Freq_new = Freq*prop)
data_nonhk$Age <- as.numeric(as.character(data_nonhk$Age))
data_nonhk$Year <- as.numeric(as.character(data_nonhk$Year))

# C&SD
setwd("../Life expectancy/Migrant")
popdata <- read.csv("Population Data.csv",h=T)

# EDA
popdata %>% filter(Age != 85) %>% tidyr::gather(.,PrevResidence,Exposure,HK:NonHK) %>%
  ggplot(.,aes(Age,Exposure,color=PrevResidence)) + geom_line(lwd = 1.2) + theme(axis.text = element_text(size = 16), axis.title = element_text(size = 18)) +
  facet_wrap(.~Year+Sex) + theme_classic()

popdata_single_age <- popdata %>% filter(Year == 2016 & Age > 49)
popdata <- popdata %>% mutate(HK_prop = HK/(HK+NonHK),Mainland_prop = Mainland/(HK+NonHK),NonHK_prop = NonHK/(HK+NonHK))
dummy <- data.frame(Age = rep(86:100,8),Sex = rep(rep(c("Male","Female"),each = 15),4),Year = rep(c(2001,2006,2011,2016),each = 30))
dummy <- dummy %>% left_join(.,popdata %>% filter(Age == 85) %>% select(-Age),by = c("Sex","Year"))
popdata <- rbind(popdata,dummy)

# HMD
popdata_hmd <- fread("../Life expectancy/Exposures_1x1.txt") %>% filter(Year %in% c(2001,2006,2011,2016))
popdata_hmd100 <- popdata_hmd %>% filter(Age %in% c(100:109,"110+"))
popdata_hmd100 <- popdata_hmd100 %>% group_by(Year) %>% summarize(Female = sum(Female),Male = sum(Male),Total = sum(Total)) %>% mutate(Age = 100)
popdata_hmd <- rbind(popdata_hmd %>% filter(!Age %in% c(100:109,"110+")),popdata_hmd100)
popdata_hmd <- popdata_hmd %>% tidyr::gather(.,Sex,Pop,Female:Male) %>% select(-Total)
popdata_hmd$Age <- as.numeric(as.character(popdata_hmd$Age))

# HMD using CSD proportion
popdata <- popdata %>% left_join(.,popdata_hmd,by = c("Year","Age","Sex"))
popdata <- popdata %>% mutate(HK_hmd = Pop*HK_prop,Mainland_hmd = Pop*Mainland_prop,NonHK_hmd = Pop*NonHK_prop)
popdata <- popdata %>% select(Age,Sex,Year,HK_hmd,NonHK_hmd,Mainland_hmd)
popdata <- popdata %>% rename("HK" = "HK_hmd", "NonHK" = "NonHK_hmd", "Mainland" = "Mainland_hmd") %>% reshape2::melt(.,id.vars = c("Age","Sex","Year")) %>% rename("PrevResidence" = "variable")

# Period mortality
period_mx <- popdata %>% left_join(.,data_nonhk,by=c("Year","Sex","Age","PrevResidence"))
period_mx_single_age <- period_mx
period_mx <- period_mx %>% mutate(Agegroup = cut(Age,breaks = c(-1,0,4,9,14,19,24,29,34,39,44,49,54,59,64,69,74,79,84,89,94,99,100)),
                                                        Agegroup = fct_recode(factor(Agegroup),'below 1' = '(-1,0]',
                                                                                                    '1-4' = '(0,4]',
                                                                                                    '5-9' = '(4,9]',
                                                                                                    '10-14' = '(9,14]',
                                                                                                    '15-19' = '(14,19]',
                                                                                                    '20-24' = '(19,24]',
                                                                                                    '25-29' = '(24,29]',
                                                                                                    '30-34' = '(29,34]',
                                                                                                    '35-39' = '(34,39]',
                                                                                                    '40-44' = '(39,44]',
                                                                                                    '45-49' = '(44,49]',
                                                                                                    '50-54' = '(49,54]',
                                                                                                    '55-59' = '(54,59]',
                                                                                                    '60-64' = '(59,64]',
                                                                                                    '65-69' = '(64,69]',
                                                                                                    '70-74' = '(69,74]',
                                                                                                    '75-79' = '(74,79]',
                                                                                                    '80-84' = '(79,84]',
                                                                                                    '85-89' = '(84,89]',
                                                                                                    '90-94' = '(89,94]',
                                                                                                    '95-99' = '(94,99]',
                                                                                                    '100+' = '(99,100]'))
period_mx <- period_mx %>% group_by(Sex,Year,PrevResidence,Agegroup) %>% summarize(Exposure = sum(value), Deaths = sum(Freq_new)) %>% data.frame()
period_mx <- period_mx %>% mutate(mx = Deaths/Exposure, mx = ifelse(mx>1,1,mx), mx = log(mx), mx_ll = mx - qnorm(.975)*(1/sqrt(Deaths)), mx_ul = mx + qnorm(.975)*(1/sqrt(Deaths)))
period_mx <- period_mx %>% mutate(mx = exp(mx), mx_ll = exp(mx_ll), mx_ul = exp(mx_ul))
period_mx <- period_mx %>% mutate(PrevResidence = ifelse(PrevResidence == "HK", "Natives",ifelse(PrevResidence == "Mainland", "Mainland migrants","All migrants")))

p1 <- ggplot(period_mx %>% filter(Deaths != 0 & Year == 2001),aes(Agegroup,mx,group = PrevResidence,color = PrevResidence)) +
  geom_line(size = 1.2) + scale_y_continuous(trans = "log10",labels = function(x) format(x, scientific = F)) +
  geom_ribbon(aes(ymin = mx_ll, ymax = mx_ul, fill = PrevResidence, group = PrevResidence),alpha = 0.2) + theme_classic() + facet_wrap(.~Sex)+
  scale_color_manual(values = c("blue2","#F8766D","#7CAE00")) + scale_fill_manual(values = c("blue2","#F8766D","#7CAE00")) + ggtitle("2001") +
  theme(legend.position = "bottom",legend.title = element_blank(),legend.text = element_text(size = 16),strip.text = element_text(size = 18),plot.title = element_text(hjust = 0.5,face = "bold",size = 18),axis.text.x = element_text(angle = 90),axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text = element_text(size = 12))

p2 <- ggplot(period_mx %>% filter(Deaths != 0 & Year == 2006),aes(Agegroup,mx,group = PrevResidence,color = PrevResidence)) +
  geom_line(size = 1.2) + scale_y_continuous(trans = "log10",labels = function(x) format(x, scientific = F)) +
  geom_ribbon(aes(ymin = mx_ll, ymax = mx_ul, fill = PrevResidence, group = PrevResidence),alpha = 0.2) + theme_classic() + facet_wrap(.~Sex)+
  scale_color_manual(values = c("blue2","#F8766D","#7CAE00")) + scale_fill_manual(values = c("blue2","#F8766D","#7CAE00")) + ggtitle("2006") +
  theme(legend.position = "bottom",legend.title = element_blank(),legend.text = element_text(size = 16),strip.text = element_text(size = 18),plot.title = element_text(hjust = 0.5,face = "bold",size = 18),axis.text.x = element_text(angle = 90),axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text = element_text(size = 12))

p3 <- ggplot(period_mx %>% filter(Deaths != 0 & Year == 2011),aes(Agegroup,mx,group = PrevResidence,color = PrevResidence)) +
  geom_line(size = 1.2) + scale_y_continuous(trans = "log10",labels = function(x) format(x, scientific = F)) +
  geom_ribbon(aes(ymin = mx_ll, ymax = mx_ul, fill = PrevResidence, group = PrevResidence),alpha = 0.2) + theme_classic() + facet_wrap(.~Sex)+
  scale_color_manual(values = c("blue2","#F8766D","#7CAE00")) + scale_fill_manual(values = c("blue2","#F8766D","#7CAE00")) + ggtitle("2011") +
  theme(legend.position = "bottom",legend.title = element_blank(),legend.text = element_text(size = 16),strip.text = element_blank(),plot.title = element_text(hjust = 0.5,face = "bold",size = 18),axis.text.x = element_text(angle = 90),axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text = element_text(size = 12))

p4 <- ggplot(period_mx %>% filter(Deaths != 0 & Year == 2016),aes(Agegroup,mx,group = PrevResidence,color = PrevResidence)) +
  geom_line(size = 1.2) + scale_y_continuous(trans = "log10",labels = function(x) format(x, scientific = F)) +
  geom_ribbon(aes(ymin = mx_ll, ymax = mx_ul, fill = PrevResidence, group = PrevResidence),alpha = 0.2) + theme_classic() + facet_wrap(.~Sex)+
  scale_color_manual(values = c("blue2","#F8766D","#7CAE00")) + scale_fill_manual(values = c("blue2","#F8766D","#7CAE00")) + ggtitle("2016") +
  theme(legend.position = "bottom",legend.title = element_blank(),legend.text = element_text(size = 16),strip.text = element_blank(),plot.title = element_text(hjust = 0.5,face = "bold",size = 18),axis.text.x = element_text(angle = 90),axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text = element_text(size = 12))

annotate_figure(ggarrange(p1,p2,p3,p4,ncol = 2,nrow = 2,common.legend = T,legend = "top"),
                left = text_grob("Log of mortality rates",rot = 90,face = "bold",size = 18),
                bottom = text_grob("Age group",face = "bold",size = 18))
ggsave("C:/Users/FreyaYu/Documents/Tasks/Life Expectancy Task/Recent files/Results/Plots/Natives vs NonHK Migrants 2016.png",dpi = 300,height = 12,width = 16)

#-------------------------------------------------------------------------------------------------------------------------------------------------------------
#### here put the folder where you have your HMD data - repeat process for all countries
CountryA<-c("C:/Users/FreyaYu/Documents/Tasks/Life Expectancy Task/Data/mortality.orghmd/STATS/Life Tables")
Names <- c("USA")

Country<-paste(CountryA,"/",Names[1],sep="")
setwd(Country)
usa_exposures<-read.table("Exposures_1x1.txt",header=TRUE,fill=TRUE,skip=1)
usa_deaths<-read.table("Deaths_1x1.txt",header=TRUE,fill=TRUE,skip=1)

# Data cleaning for belgium data
bel_exposures <- bel_exposures %>% filter(Year>1949)
bel_exposures$Female <- as.numeric(as.character(bel_exposures$Female))
bel_exposures$Male <- as.numeric(as.character(bel_exposures$Male))
bel_exposures$Total <- as.numeric(as.character(bel_exposures$Total))

bel_deaths <- bel_deaths %>% filter(Year>1949)
bel_deaths$Female <- as.numeric(as.character(bel_deaths$Female))
bel_deaths$Male <- as.numeric(as.character(bel_deaths$Male))
bel_deaths$Total <- as.numeric(as.character(bel_deaths$Total))

# Lump East and West Germany data
deutnp_deaths <- data.frame(Year = deute_deaths$Year,Age = deute_deaths$Age,Female = deute_deaths$Female + deutw_deaths$Female,Male = deute_deaths$Male + deutw_deaths$Male,Total = deute_deaths$Total + deutw_deaths$Total)
deutnp_exposures <- data.frame(Year = deute_exposures$Year,Age = deute_exposures$Age,Female = deute_exposures$Female + deutw_exposures$Female,Male = deute_exposures$Male + deutw_exposures$Male,Total = deute_exposures$Total + deutw_exposures$Total)

# Merge data for 18 HICs
total_exposures <- as.data.frame(bind_rows(aus_exposures %>% mutate(Country = "Australia"),aut_exposures %>% mutate(Country = "Austria"),
                                           ita_exposures %>% mutate(Country = "Italy"),fratnp_exposures %>% mutate(Country = "France"),
                                           deutnp_exposures %>% mutate(Country = "Germany"),gbr_exposures %>% mutate(Country = "UK"),
                                           esp_exposures %>% mutate(Country = "Spain"),usa_exposures %>% mutate(Country = "USA"),
                                           nld_exposures %>% mutate(Country = "Netherlands"),che_exposures %>% mutate(Country = "Switzerland"),
                                           swe_exposures %>% mutate(Country = "Sweden"),bel_exposures %>% mutate(Country = "Belgium"),
                                           prt_exposures %>% mutate(Country = "Portugal"),can_exposures %>% mutate(Country = "Canada"),
                                           dnk_exposures %>% mutate(Country = "Denmark"),jpn_exposures %>% mutate(Country = "Japan"),
                                           nor_exposures %>% mutate(Country = "Norway"),fin_exposures %>% mutate(Country = "Finland"))) %>% filter(Year == 2016)

total_deaths <- as.data.frame(bind_rows(aus_deaths %>% mutate(Country = "Australia"),aut_deaths %>% mutate(Country = "Austria"),
                                        ita_deaths %>% mutate(Country = "Italy"),fratnp_deaths %>% mutate(Country = "France"),
                                        deutnp_deaths %>% mutate(Country = "Germany"),gbr_deaths %>% mutate(Country = "UK"),
                                        esp_deaths %>% mutate(Country = "Spain"),usa_deaths %>% mutate(Country = "USA"),
                                        nld_deaths %>% mutate(Country = "Netherlands"),che_deaths %>% mutate(Country = "Switzerland"),
                                        swe_deaths %>% mutate(Country = "Sweden"),bel_deaths %>% mutate(Country = "Belgium"),
                                        prt_deaths %>% mutate(Country = "Portugal"),can_deaths %>% mutate(Country = "Canada"),
                                        dnk_deaths %>% mutate(Country = "Denmark"),jpn_deaths %>% mutate(Country = "Japan"),
                                        nor_deaths %>% mutate(Country = "Norway"),fin_deaths %>% mutate(Country = "Finland"))) %>% filter(Year == 2016)

hic_data <- total_deaths %>% rename("Female_d" = "Female", "Male_d" = "Male", "Total_d" = "Total") %>%
  left_join(.,total_exposures %>% rename("Female_e" = "Female", "Male_e" = "Male", "Total_e" = "Total"),by = c("Year","Age","Country")) %>%
  mutate(Age = ifelse(Age == '110+',110,Age), Age = as.numeric(as.character(Age)))
hic_data_mx <- hic_data %>% mutate(female_mx = Female_d/Female_e, male_mx = Male_d/Male_e) %>% group_by(Year,Age) %>% summarize(female_mx = mean(female_mx,na.rm = T), male_mx = mean(male_mx,na.rm = T)) %>% data.frame()
hic_data85 <- hic_data %>% filter(Age>84) %>% group_by(Year,Country) %>% summarize(Female_d = sum(Female_d),Male_d = sum(Male_d),Total_d = sum(Total_d),Female_e = sum(Female_e),Male_e = sum(Male_e),Total_e = sum(Total_e)) %>% mutate(Age = 85)
hic_data <- hic_data %>% filter(Age<85) %>% rbind(.,hic_data85) %>% mutate(Agegroup = cut(Age,breaks = c(-1,0,4,9,14,19,24,29,34,39,44,49,54,59,64,69,74,79,84,85)))
hic_single_age <- hic_data
hic_data <- hic_data %>% mutate(Agegroup = fct_recode(factor(Agegroup),'below 1' = '(-1,0]',
                                                      '1-4' = '(0,4]',
                                                      '5-9' = '(4,9]',
                                                      '10-14' = '(9,14]',
                                                      '15-19' = '(14,19]',
                                                      '20-24' = '(19,24]',
                                                      '25-29' = '(24,29]',
                                                      '30-34' = '(29,34]',
                                                      '35-39' = '(34,39]',
                                                      '40-44' = '(39,44]',
                                                      '45-49' = '(44,49]',
                                                      '50-54' = '(49,54]',
                                                      '55-59' = '(54,59]',
                                                      '60-64' = '(59,64]',
                                                      '65-69' = '(64,69]',
                                                      '70-74' = '(69,74]',
                                                      '75-79' = '(74,79]',
                                                      '80-84' = '(79,84]',
                                                      '85+' = '(84,85]')) %>%
  group_by(Year,Agegroup,Country) %>% summarize(Female_d = sum(Female_d),Male_d = sum(Male_d),Total_d = sum(Total_d),
                                                Female_e = sum(Female_e),Male_e = sum(Male_e),Total_e = sum(Total_e)) %>%
  mutate(Female_mx = Female_d/Female_e, Male_mx = Male_d/Male_e, Total_mx = Total_d/Total_e) %>% data.frame

hic_ave <- hic_data %>% group_by(Year,Agegroup) %>% summarize(Female_mx = mean(Female_mx), Male_mx = mean(Male_mx), Total_mx = mean(Total_mx)) %>%
  mutate(Country = "HICs") %>% left_join(.,hic_data %>% group_by(Year,Agegroup) %>% summarize(Female_d = sum(Female_d), Male_d = sum(Male_d), Total_d = sum(Total_d)),by = c("Year","Agegroup"))
hic_ave <- hic_ave %>% mutate(Female_mx_ll = Female_mx - qnorm(.975)*(Female_mx/sqrt(Female_d)), Female_mx_ul = Female_mx + qnorm(.975)*(Female_mx/sqrt(Female_d)),
                              Male_mx_ll = Male_mx - qnorm(.975)*(Male_mx/sqrt(Male_d)),Male_mx_ul = Male_mx + qnorm(.975)*(Male_mx/sqrt(Male_d)),
                              Total_mx_ll = Total_mx - qnorm(.975)*(Total_mx/sqrt(Total_d)),Total_mx_ul = Total_mx + qnorm(.975)*(Total_mx/sqrt(Total_d))) #%>%
hic_mx <- hic_ave %>% select(Year,Agegroup,Female_mx,Female_mx_ll,Female_mx_ul,Female_d) %>% mutate(Sex = "Female",Country = "High Income Countries") %>%
  rename("Deaths" = "Female_d", "mx" = "Female_mx", 'mx_ll' = "Female_mx_ll", 'mx_ul' = 'Female_mx_ul') %>%
  rbind(.,hic_ave %>% select(Year,Agegroup,Male_mx,Male_mx_ll,Male_mx_ul,Male_d) %>% mutate(Sex = "Male",Country = "High Income Countries") %>%
          rename("Deaths" = "Male_d", "mx" = "Male_mx", 'mx_ll' = "Male_mx_ll", 'mx_ul' = 'Male_mx_ul')) %>% data.frame()


# Japan
japan_data <- jpn_deaths %>% rename("Female_d" = "Female", "Male_d" = "Male", "Total_d" = "Total") %>%
  left_join(.,jpn_exposures %>% rename("Female_e" = "Female", "Male_e" = "Male", "Total_e" = "Total"),by = c("Year","Age")) %>%
  mutate(Age = ifelse(Age == '110+',110,Age), Age = as.numeric(as.character(Age))) %>% filter(Year == 2016)
japan_data85 <- japan_data %>% filter(Age>84) %>% group_by(Year) %>% summarize(Female_d = sum(Female_d),Male_d = sum(Male_d),Total_d = sum(Total_d),Female_e = sum(Female_e),Male_e = sum(Male_e),Total_e = sum(Total_e)) %>% mutate(Age = 85)
japan_data <- japan_data %>% filter(Age<85) %>% rbind(.,japan_data85) %>% mutate(Agegroup = cut(Age,breaks = c(-1,0,4,9,14,19,24,29,34,39,44,49,54,59,64,69,74,79,84,85)))
japan_single_age <- japan_data
japan_data <- japan_data %>% mutate(Agegroup = fct_recode(factor(Agegroup),'below 1' = '(-1,0]',
                                                          '1-4' = '(0,4]',
                                                          '5-9' = '(4,9]',
                                                          '10-14' = '(9,14]',
                                                          '15-19' = '(14,19]',
                                                          '20-24' = '(19,24]',
                                                          '25-29' = '(24,29]',
                                                          '30-34' = '(29,34]',
                                                          '35-39' = '(34,39]',
                                                          '40-44' = '(39,44]',
                                                          '45-49' = '(44,49]',
                                                          '50-54' = '(49,54]',
                                                          '55-59' = '(54,59]',
                                                          '60-64' = '(59,64]',
                                                          '65-69' = '(64,69]',
                                                          '70-74' = '(69,74]',
                                                          '75-79' = '(74,79]',
                                                          '80-84' = '(79,84]',
                                                          '85+' = '(84,85]')) %>%
  group_by(Year,Agegroup) %>% summarize(Female_d = sum(Female_d),Male_d = sum(Male_d),Total_d = sum(Total_d),
                                        Female_e = sum(Female_e),Male_e = sum(Male_e),Total_e = sum(Total_e)) %>%
  mutate(Female_mx = Female_d/Female_e, Male_mx = Male_d/Male_e, Total_mx = Total_d/Total_e) %>% data.frame %>% mutate(Country = "Japan")

japan_data_female <- japan_data %>% select(Year,Agegroup,Female_d,Female_mx) %>% mutate(Sex = "Female") %>%
  mutate(Female_mx_ll = Female_mx - qnorm(.975)*(Female_mx/sqrt(Female_d)), Female_mx_ul = Female_mx + qnorm(.975)*(Female_mx/sqrt(Female_d)))
japan_data_male <- japan_data %>% select(Year,Agegroup,Male_d,Male_mx) %>% mutate(Sex = "Male") %>%
  mutate(Male_mx_ll = Male_mx - qnorm(.975)*(Male_mx/sqrt(Male_d)), Male_mx_ul = Male_mx + qnorm(.975)*(Male_mx/sqrt(Male_d)))

japan_mx <- japan_data_male %>% rename("Deaths" = "Male_d", "mx" = "Male_mx", 'mx_ll' = "Male_mx_ll", 'mx_ul' = 'Male_mx_ul') %>%
  rbind(.,japan_data_female %>% rename("Deaths" = "Female_d", "mx" = "Female_mx", 'mx_ll' = "Female_mx_ll", 'mx_ul' = 'Female_mx_ul')) %>%
  mutate(Country = "Japan")

mx_data <- rbind(hic_mx,japan_mx,period_mx %>% rename("Country" = "PrevResidence") %>% select(-Exposure) %>% filter(Year == 2016)) %>%
  mutate(mx_ll = log(mx) - qnorm(.975)*(1/sqrt(Deaths)), mx_ll = exp(mx_ll), mx_ul = log(mx) + qnorm(.975)*(1/sqrt(Deaths)), mx_ul = exp(mx_ul))

p1 <- ggplot(mx_data %>% filter(Deaths != 0),aes(Agegroup,mx,group = Country,color = Country)) +
  geom_line(size = 1.2) + scale_y_continuous(trans = "log10",labels = function(x) format(x, scientific = F)) +
  geom_ribbon(aes(ymin = mx_ll, ymax = mx_ul, fill = Country, group = Country),alpha = 0.2) + theme_classic() +
  scale_color_manual(values = c("blue2","black","grey69","#F8766D","#7CAE00")) + scale_fill_manual(values = c("blue2","black","grey69","#F8766D","#7CAE00")) + facet_wrap(.~Sex) +
  theme(legend.position = "bottom",legend.title = element_blank(),legend.text = element_text(size = 16),plot.title = element_text(hjust = 0.5,face = "bold",size = 18),axis.text.x = element_text(angle = 90),axis.title.x = element_blank(),axis.text = element_text(size = 12)) +
  guides(color = F, fill = F) + labs(y = "Log of mortality rate") + theme(axis.title.y = element_text(size = 18,face = "bold"))

ggsave("../Life expectancy/Migrants Period mx.png",dpi = 300,height = 6,width = 8)

# by single age
japan_single_age <- japan_single_age %>% group_by(Age) %>% summarize(Female_d = sum(Female_d), Male_d = sum(Male_d), Female_e = sum(Female_e), Male_e = sum(Male_e)) %>%
  mutate(Female_mx = Female_d/Female_e, Male_mx = Male_d/Male_e) %>% data.frame() %>% group_by(Age) %>%
  summarize(Female_d = sum(Female_d), Male_d = sum(Male_d), Female_mx = mean(Female_mx, na.rm = T), Male_mx = mean(Male_mx, na.rm = T)) %>%
  mutate(Country = "Japan") %>% filter(Age>49) %>% data.frame()
japan_single_age <- japan_single_age %>% select(Age,Country,Female_d,Male_d) %>% reshape2::melt(.,id.vars = c("Age","Country")) %>%
  mutate(variable = ifelse(variable == 'Female_d',"Female","Male")) %>% rename(deaths = value,Sex = variable) %>%
  left_join(.,japan_single_age %>% select(Age,Country,Female_mx,Male_mx) %>% reshape2::melt(.,id.vars = c("Age","Country")) %>%
              mutate(variable = ifelse(variable == 'Female_mx',"Female","Male")) %>% rename(mx = value,Sex = variable),by = c('Age','Country','Sex'))

hic_single_age <- hic_single_age %>% group_by(Age,Country) %>% summarize(Female_d = sum(Female_d), Male_d = sum(Male_d), Female_e = sum(Female_e), Male_e = sum(Male_e)) %>%
  mutate(Female_mx = Female_d/Female_e, Male_mx = Male_d/Male_e) %>% data.frame() %>% group_by(Age) %>%
  summarize(Female_d = sum(Female_d), Male_d = sum(Male_d), Female_mx = mean(Female_mx, na.rm = T), Male_mx = mean(Male_mx, na.rm = T)) %>%
  mutate(Country = "High income countries") %>% filter(Age>49) %>% data.frame()
hic_single_age <- hic_single_age %>% select(Age,Country,Female_d,Male_d) %>% reshape2::melt(.,id.vars = c("Age","Country")) %>%
  mutate(variable = ifelse(variable == 'Female_d',"Female","Male")) %>% rename(deaths = value,Sex = variable) %>%
  left_join(.,hic_single_age %>% select(Age,Country,Female_mx,Male_mx) %>% reshape2::melt(.,id.vars = c("Age","Country")) %>%
              mutate(variable = ifelse(variable == 'Female_mx',"Female","Male")) %>% rename(mx = value,Sex = variable),by = c('Age','Country','Sex'))

HK_single_age <- data_nonhk %>% left_join(.,popdata,by = c("Age","Sex","Year","PrevResidence")) %>% filter(Age %in% c(50:100) & Year == 2016)
HK_single_age <- HK_single_age %>% mutate(PrevResidence = ifelse(PrevResidence == "HK", "Natives",ifelse(PrevResidence == "Mainland", "Mainland migrants","All migrants")))
HK_single_age <- HK_single_age %>% mutate(mx = Freq_new/value) %>% select(Age,Sex,Year,PrevResidence,Freq_new,mx) %>% rename("deaths" = "Freq_new", "Country" = "PrevResidence")

data_single_age <- rbind(hic_single_age,japan_single_age,HK_single_age %>% select(-Year)) %>% mutate(mx_ll = log(mx) - qnorm(.975)*(1/sqrt(deaths)), mx_ll = exp(mx_ll), mx_ul = log(mx) + qnorm(.975)*(1/sqrt(deaths)), mx_ul = exp(mx_ul))
data_single_age$Age <- as.numeric(data_single_age$Age)

p2 <- ggplot(data_single_age %>% filter(Age>49),aes(Age,mx,group = Country,color = Country)) +
  geom_line(size = 1.2) + theme_classic() +#scale_y_continuous(trans = "log10",labels = function(x) format(x, scientific = F),breaks = c(0.001,0.01,0.1),limits = c(0.001,0.18)) +
  geom_ribbon(aes(ymin = mx_ll, ymax = mx_ul, group = Country, fill = Country),alpha = 0.2) +
  scale_color_manual(values = c("blue2","black","grey69","#F8766D","#7CAE00")) + scale_fill_manual(values = c("blue2","black","grey69","#F8766D","#7CAE00")) + facet_wrap(.~Sex) +
  scale_x_continuous(breaks = c(50,60,70,80,90,100)) +
  theme(legend.position = "bottom",legend.title = element_blank(),legend.text = element_text(size = 16),strip.text = element_blank(),plot.title = element_text(hjust = 0.5,face = "bold",size = 18),axis.title.x = element_blank(),axis.text = element_text(size = 12)) +
  labs(y = "Mortality rate") + theme(axis.title.y = element_text(size = 18,face = "bold"))
ggarrange(p1,p2,nrow = 2)

ggsave("../Life expectancy/Migrants Period mx with 50+ zoomed in.png",dpi = 300,height = 9,width = 12)
