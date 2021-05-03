# Load dataset
load("../Life expectancy/DeathData7619.rda")
setwd("../Life expectancy/Mapping tables")

# Load packages
library(ggplot2)
library(ggthemes)
library(dplyr)
library(forcats)
library(data.table)
library(readxl)
library(RColorBrewer)
library(randomcoloR)
library(scales)

# Import Life tables
lt_HK <- data.table(read.csv("../Life expectancy/HK_life_table.csv"))
names(lt_HK)[names(lt_HK)=="year"] <- "Year"
lt_HK[Age=="100+",Age:="100"]
lt_HK[,Age:=as.numeric(as.character(Age))]
'--------------------'
'Made adjustments'
# Create mx from life table
lt_HK[qx!=1,mx:=2*qx/(2-qx)]
lt_HK[qx==1,mx:=lx/Lx]
'--------------------'

# Age decomposition
age_decomp <- function(year1,year2)
{
  # Truncate at Age=99
  lt_HK[Age==99, ':='(dx=lx, qx=1, Lx=Tx)]
  lt_HK <- lt_HK[Age<=99]
  lt_HK[,Age:=as.numeric(as.character(Age))]
  
  '--------------------'
  'Made adjustments'
  # Create mx from life table
  lt_HK[qx!=1,mx:=2*qx/(2-qx)]
  lt_HK[qx==1,mx:=lx/Lx]
  '--------------------'
  
  lt_HK1 <- lt_HK[sex=="Male",]
  l0 <- 1000000
  lx1 <- lt_HK1[Year==year1,lx]
  lx2 <- lt_HK1[Year==year2,lx]
  Lx1 <- lt_HK1[Year==year1,Lx]
  Lx2 <- lt_HK1[Year==year2,Lx]
  Tx1 <- lt_HK1[Year==year1,Tx]
  Tx2 <- lt_HK1[Year==year2,Tx]
  mx1 <- lt_HK1[Year==year1,mx]
  mx2 <- lt_HK1[Year==year2,mx]
  
  # decomposing by Age
  age_contri <- NULL
  for(i in 1:99){
    age_contri[i] <- lx1[i]/l0 * (Lx2[i]/lx2[i] - Lx1[i]/lx1[i]) + Tx2[i+1]/l0 * (lx1[i]/lx2[i] - lx1[i+1]/lx2[i+1])
  }
  age_contri[100] <- lx1[100]/l0 * (Tx2[100]/lx2[100]-Tx1[100]/lx1[100])
  arriaga_data_male <- cbind(sex = "Male",Age=0:99,data.frame(age_contri))
  
  lt_HK2 <- lt_HK[sex=="Female",]
  l0 <- 1000000
  lx1 <- lt_HK2[Year==year1,lx]
  lx2 <- lt_HK2[Year==year2,lx]
  Lx1 <- lt_HK2[Year==year1,Lx]
  Lx2 <- lt_HK2[Year==year2,Lx]
  Tx1 <- lt_HK2[Year==year1,Tx]
  Tx2 <- lt_HK2[Year==year2,Tx]
  mx1 <- lt_HK2[Year==year1,mx]
  mx2 <- lt_HK2[Year==year2,mx]
  
  # decomposing by Age
  age_contri <- NULL
  for(i in 1:99){
    age_contri[i] <- lx1[i]/l0 * (Lx2[i]/lx2[i] - Lx1[i]/lx1[i]) + Tx2[i+1]/l0 * (lx1[i]/lx2[i] - lx1[i+1]/lx2[i+1])
  }
  age_contri[100] <- lx1[100]/l0 * (Tx2[100]/lx2[100]-Tx1[100]/lx1[100])
  arriaga_data_female <- cbind(sex = "Female",Age=0:99,data.frame(age_contri))
  arriaga_data<-rbind(arriaga_data_female,arriaga_data_male)
  colnames(arriaga_data) <- c("Sex","Age","age_contri")
  return(arriaga_data)
}

# Data Cleaning
## Mapping tables
ICD9_level2 <- read_excel("ICD9to10 level2.xlsx")
ICD10_level2 <- read.csv("ICD10 level2.csv",header = T)

## Death data
HKDeathData7900 <- DeathData7619 %>% filter(Year<2001) %>% select(Year,Sex,Age,cod,E_cod) %>% left_join(.,ICD9_level2,by="cod")
HKDeathData0119 <- DeathData7619 %>% filter(Year>2000) %>% select(Year,Sex,Age,cod,E_cod) %>% mutate(cod1=substr(cod,1,3)) %>% left_join(.,ICD10_level2,by=c("cod1"="cod")) %>% select(-cod1)
HKDeathData7919 <- rbind(HKDeathData7900,HKDeathData0119)
HKDeathData7919$Level2 <- ifelse(is.na(HKDeathData7919$cod),"Unknown external cause of death",HKDeathData7919$Level2)
HKDeathData7919$Level3 <- ifelse(is.na(HKDeathData7919$cod),"External causes of morbidity and mortality",HKDeathData7919$Level3)

# Arriaga's age-cause decomposition
age_cod_decomp <- function(year1,year2,sex1)
{
  arriaga_data <- age_decomp(year1,year2)
  testing <- HKDeathData7919 %>% filter(Year==year1|Year==year2) %>% select(Year,Sex,Age,Level3)
  testing$ICD10_level3 <- as.character(testing$Level3)
  testing$ICD10_level3 <- as.factor(testing$ICD10_level3)
  testing <- testing %>% filter(!is.na(ICD10_level3))
  
  Rx1_male <- testing %>% filter(Year == year1 & Sex == "Male") %>% select(Age,ICD10_level3) %>% table() %>% prop.table(.,1) %>% data.frame %>% mutate(Sex="Male")
  colnames(Rx1_male) <- c("Age","ICD10_level3","Rx1","Sex")
  Rx1_female <- testing %>% filter(Year == year1 & Sex == "Female") %>% select(Age,ICD10_level3) %>% table() %>% prop.table(.,1) %>% data.frame %>% mutate(Sex="Female")
  colnames(Rx1_female) <- c("Age","ICD10_level3","Rx1","Sex")
  Rx1 <- rbind(Rx1_male,Rx1_female)
  Rx2_male <- testing %>% filter(Year == year2 & Sex == "Male") %>% select(Age,ICD10_level3) %>% table() %>% prop.table(.,1) %>% data.frame %>% mutate(Sex="Male")
  colnames(Rx2_male) <- c("Age","ICD10_level3","Rx2","Sex")
  Rx2_female <- testing %>% filter(Year == year2 & Sex == "Female") %>% select(Age,ICD10_level3) %>% table() %>% prop.table(.,1) %>% data.frame %>% mutate(Sex="Female")
  colnames(Rx2_female) <- c("Age","ICD10_level3","Rx2","Sex")
  Rx2 <- rbind(Rx2_male,Rx2_female)
  data_arriaga <- left_join(Rx1,Rx2,by=c("Age","Sex","ICD10_level3"))
  data_arriaga$Rx1 <- ifelse(is.na(data_arriaga$Rx1),0,data_arriaga$Rx1)
  data_arriaga$Rx2 <- ifelse(is.na(data_arriaga$Rx2),0,data_arriaga$Rx2)
  data_arriaga$Age <- as.numeric(as.character(data_arriaga$Age))
  mx1 <- data.frame(lt_HK) %>% filter(Year == year1) %>% select(Age,sex,mx)
  colnames(mx1) <- c("Age","Sex","mx1")
  mx2 <- data.frame(lt_HK) %>% filter(Year == year2) %>% select(Age,sex,mx)
  colnames(mx2) <- c("Age","Sex","mx2")
  mx <- left_join(mx1,mx2,by=c("Age","Sex"))
  data_arriaga <- data_arriaga %>% left_join(.,mx,by=c("Age","Sex")) %>% left_join(.,arriaga_data,by=c("Age","Sex"))
  data_arriaga <- data_arriaga %>% mutate(age_cod_contri = age_contri*((Rx1*mx1-Rx2*mx2)/(mx1-mx2)))
  data_arriaga <- data_arriaga %>% mutate(Agegroup = cut(Age, c(0,1,seq(5,95,5),99,150), right = FALSE)) %>% mutate(Agegroup = forcats::fct_recode(factor(Agegroup),
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
                                                                                                                                                   "85-89"="[85,90)",
                                                                                                                                                   "90-95"="[90,95)",
                                                                                                                                                   "95-98"="[95,99)",
                                                                                                                                                   "99+"="[99,150)"
  ))
  data_arriaga <- data_arriaga %>% mutate(ICD10_level3 = recode(ICD10_level3, 
                                                                'Neoplasms' = 'Neoplasms',
                                                                'Diseases of the respiratory system' = 'Respiratory diseases',
                                                                'Diseases of the circulatory system' = 'Cardiovascular diseases',
                                                                'Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified' = 'Ill-defined causes',
                                                                "External causes of morbidity and mortality" = 'External causes',
                                                                "Injury, poisoning and certain other consequences of external causes" = 'External causes',
                                                                'Diseases of the digestive system' = 'Digestive diseases',
                                                                'Certain conditions originating in the perinatal period' = 'Perinatal deaths',
                                                                'Certain infectious and parasitic diseases' = 'Infectious diseases',
                                                                'Congenital malformations, deformations and chromosomal abnormalities' = 'Congenital anomalies',
                                                                'Diseases of the genitourinary system' = 'Others',
                                                                'Endocrine, nutritional and metabolic diseases' = 'Others',
                                                                'Diseases of the skin and subcutaneous tissue' = 'Others',
                                                                'Diseases of the nervous system' = 'Others',
                                                                'Diseases of the eye and adnexa' = 'Others',
                                                                'Diseases of the ear and mastoid process' = 'Others',
                                                                'Diseases of the blood and blood-forming organs and certain disorders involving the immune mechanism' = 'Others',
                                                                'Pregnancy, childbirth and the puerperium' = 'Others',
                                                                'Diseases of the musculoskeletal system and connective tissue' = 'Others',
                                                                'Mental and behavioural disorders' = 'Others'
  ))
  
  colornames <- c('Cardiovascular diseases','Respiratory diseases','Neoplasms','Ill-defined causes','External causes','Digestive diseases','Perinatal deaths','Infectious diseases','Congenital anomalies','Others','Mental disorders','Unknown')
  myColors <- distinctColorPalette(12)
  
  data_arriaga_ggplot <- data_arriaga %>% group_by(Agegroup,Sex,ICD10_level3) %>% summarize(age_cod_contri_lump = sum(age_cod_contri)) %>% data.frame()
  data_arriaga_ggplot$ICD10_level3 <- factor(data_arriaga_ggplot$ICD10_level3,levels = colornames[-c(11:12)])
  x <- data_arriaga_ggplot %>% group_by(Sex,ICD10_level3) %>% summarize(sum(age_cod_contri_lump)) %>% data.frame()
  colnames(x) <- c("Sex","ICD10","Contribution")
  colnames(data_arriaga_ggplot) <- c("Agegroup","Sex","ICD10","Contribution")
  p <- ggplot(data_arriaga_ggplot %>% filter(Sex==sex1),aes(Agegroup,Contribution,fill=ICD10)) + geom_bar(stat = "identity") +
    theme_classic() + labs(x="\nAge group",y="Contribution to life expectancy gains (years)\n") +
    theme(legend.title = element_blank(),legend.position = c(0.32,0.75),legend.text = element_text(margin = margin(l = 15), hjust = 0)) +
    scale_y_continuous(breaks = seq(0,1.5,.25),limits = c(-0.035,1.61),expand = c(0,0)) +
    scale_fill_brewer(palette = "Spectral")
  return(data_arriaga_ggplot) # use this for age cause decomposition
}

p4 <- ggplot(age_cod_decomp(2009,2019,"Male"),aes(Agegroup,Contribution,fill=ICD10)) + geom_col() + facet_wrap(~forcats::fct_rev(Sex)) +
  theme_classic()  + ggtitle("2009-2019") +#labs(x="\nAge group",y="Contribution to life expectancy gains (years)\n") +
  theme(legend.title = element_blank(),legend.position = c(0.1,0.85),legend.text = element_text(margin = margin(l = 15), hjust = 0,size = 18),
        axis.title = element_blank(),strip.background = element_blank(),strip.text = element_text(size = 18,face = "bold"),plot.title = element_text(hjust = 0.5,size = 20,face = "bold"),
        axis.text = element_text(size = 16),axis.text.x = element_text(angle = 90, hjust = 0)) + theme(strip.text = element_blank()) +
  scale_y_continuous(breaks = c(seq(-0.20,0.5,.1)),limits = c(-0.2,0.5),expand = c(0,0)) +
  scale_fill_brewer(palette = "Spectral")

library(ggpubr)
ggpubr::annotate_figure(ggpubr::ggarrange(p1,p2,p3,p4,ncol = 2,nrow = 2,common.legend = T,legend = "bottom"),
                        left = ggpubr::text_grob("Contributions to life expectancy gains (years)\n",rot = 90,face = "bold",size = 18),
                        bottom = ggpubr::text_grob("\nAge group",face = "bold",size = 18))
ggsave("../Life expectancy/Arriaga's by cod, age and sex (by decades).jpg",dpi = 300,height = 24,width = 32)
#--------------------------------------------------------------------------------------------------------------------------------------------------------------
# Function to calculate cntributions by cause of death (Chapter heading) to life expectancy gains by sex and age group
age_cod_decomp(1979,2019,sex1 = "Male") %>% mutate(Agegroup = fct_recode(Agegroup,
                                                                         '0-44'='0',
                                                                         '0-44'='1-4',
                                                                         '0-44'='5-9',
                                                                         '0-44'='10-14',
                                                                         '0-44'='15-19',
                                                                         '0-44'='20-24',
                                                                         '0-44'='25-29',
                                                                         '0-44'='30-34',
                                                                         '0-44'='35-39',
                                                                         '0-44'='40-44',
                                                                         '45-64'='45-49',
                                                                         '45-64'='50-54',
                                                                         '45-64'='55-59',
                                                                         '45-64'='60-64',
                                                                         '65+'='65-69',
                                                                         '65+'='70-74',
                                                                         '65+'='75-79',
                                                                         '65+'='80-84',
                                                                         '65+'='85-89',
                                                                         '65+'='90-95',
                                                                         '65+'='95-98',
                                                                         '65+'='99+')) %>%
  group_by(ICD10,Sex,Agegroup) %>% summarize(Contribution = sum(Contribution))

# Arriaga's specific disease of interest age-cause decomposition
age_level2_decomp <- function(year1,year2)
{
  arriaga_data <- age_decomp(year1,year2)
  testing <- HKDeathData7919 %>% filter(Year==year1|Year==year2)
  testing$Level2 <- as.character(testing$Level2)
  testing$Level2 <- as.factor(testing$Level2)
  testing <- testing %>% filter(!is.na(Level2))
  
  Rx1_male <- testing %>% filter(Year == year1 & Sex == "Male") %>% select(Age,Level2) %>% table() %>% prop.table(.,1) %>% data.frame %>% mutate(Sex="Male")
  colnames(Rx1_male) <- c("Age","Level2","Rx1","Sex")
  Rx1_female <- testing %>% filter(Year == year1 & Sex == "Female") %>% select(Age,Level2) %>% table() %>% prop.table(.,1) %>% data.frame %>% mutate(Sex="Female")
  colnames(Rx1_female) <- c("Age","Level2","Rx1","Sex")
  Rx1 <- rbind(Rx1_male,Rx1_female)
  Rx2_male <- testing %>% filter(Year == year2 & Sex == "Male") %>% select(Age,Level2) %>% table() %>% prop.table(.,1) %>% data.frame %>% mutate(Sex="Male")
  colnames(Rx2_male) <- c("Age","Level2","Rx2","Sex")
  Rx2_female <- testing %>% filter(Year == year2 & Sex == "Female") %>% select(Age,Level2) %>% table() %>% prop.table(.,1) %>% data.frame %>% mutate(Sex="Female")
  colnames(Rx2_female) <- c("Age","Level2","Rx2","Sex")
  Rx2 <- rbind(Rx2_male,Rx2_female)
  data_arriaga <- left_join(Rx1,Rx2,by=c("Age","Sex","Level2"))
  data_arriaga$Rx1 <- ifelse(is.na(data_arriaga$Rx1),0,data_arriaga$Rx1)
  data_arriaga$Rx2 <- ifelse(is.na(data_arriaga$Rx2),0,data_arriaga$Rx2)
  data_arriaga$Age <- as.numeric(as.character(data_arriaga$Age))
  mx1 <- data.frame(lt_HK) %>% filter(Year == year1) %>% select(Age,sex,mx)
  colnames(mx1) <- c("Age","Sex","mx1")
  mx2 <- data.frame(lt_HK) %>% filter(Year == year2) %>% select(Age,sex,mx)
  colnames(mx2) <- c("Age","Sex","mx2")
  mx <- left_join(mx1,mx2,by=c("Age","Sex"))
  data_arriaga <- data_arriaga %>% left_join(.,mx,by=c("Age","Sex")) %>% left_join(.,arriaga_data,by=c("Age","Sex"))
  data_arriaga <- data_arriaga %>% mutate(age_cod_contri = age_contri*((Rx1*mx1-Rx2*mx2)/(mx1-mx2)))
  data_arriaga <- data_arriaga %>% mutate(Agegroup = cut(Age, c(0,1,seq(5,100,5)), right = FALSE)) %>% mutate(Agegroup = forcats::fct_recode(factor(Agegroup),
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
                                                                                                                                             "85-89"="[85,90)",
                                                                                                                                             "90+"="[90,95)",
                                                                                                                                             "90+"="[95,100)"
  ))
  
  data_arriaga <- data_arriaga %>% group_by(Agegroup,Sex,Level2) %>% dplyr::summarize(age_cod_contri_lump = sum(age_cod_contri)) %>% data.frame()
  return(data_arriaga)
}

level2 <- age_level2_decomp(1979,2019)
out <- level2 %>% mutate(Agegroup = fct_recode(Agegroup,
                                               '0-44'='0',
                                               '0-44'='1-4',
                                               '0-44'='5-9',
                                               '0-44'='10-14',
                                               '0-44'='15-19',
                                               '0-44'='20-24',
                                               '0-44'='25-29',
                                               '0-44'='30-34',
                                               '0-44'='35-39',
                                               '0-44'='40-44',
                                               '45-64'='45-49',
                                               '45-64'='50-54',
                                               '45-64'='55-59',
                                               '45-64'='60-64',
                                               '65+'='65-69',
                                               '65+'='70-74',
                                               '65+'='75-79',
                                               '65+'='80-84',
                                               '65+'='85-89',
                                               '65+'='90+')) %>%
  group_by(Level2,Sex,Agegroup) %>% summarize(Contribution = sum(age_cod_contri_lump))

