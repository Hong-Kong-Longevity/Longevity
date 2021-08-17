
# This program includes calculation of smoking attributable fractions by sex and age groups using modified Peto-Lopez method. 

library(plyr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(plotly)
library(htmlwidgets)
library(data.table)
library(MortalityLaws)

## WHO Mortality Raw Data

icd9 <- fread("Life Expectancy/Morticd9")
icd10_part1 <- fread("Life Expectancy/Morticd10_part1")
icd10_part2 <- fread("Life Expectancy/Morticd10_part2")
icd10_part3 <- fread("Life Expectancy/Morticd10_part3")
icd10_part4 <- fread("Life Expectancy/Morticd10_part4")
icd10_part5 <- fread("Life Expectancy/Morticd10_part5")
icd_total <- rbind(icd9,icd10_part1,icd10_part2, icd10_part3, icd10_part4, icd10_part5) %>% filter(Year>1949) %>%
  select(-Admin1,-SubDiv,-IM_Frmat,-IM_Deaths1,-IM_Deaths2,-IM_Deaths3,-IM_Deaths4,-c(paste0("Deaths",1:12))) %>%  
  filter(Country %in% c("5020","4010","4020","2090","4050","4070","4080","4085","4150","4160","4170","4180","3160","4210","5150","4220","4240","3350","3325","4280","4290","4300","4308","2450","3090"))
total_data <- icd_total %>% mutate(Country = forcats::fct_recode(factor(Country),"Australia"="5020",
                                                                 "Austria"="4010",
                                                                 "Belgium"="4020",
                                                                 "Canada"="2090",
                                                                 "Denmark"="4050",
                                                                 "Finland"="4070",
                                                                 "France"="4080",
                                                                 "Germany"="4085",
                                                                 "Hungary"="4150",
                                                                 #"Iceland"="4160",
                                                                 #"Ireland"="4170",
                                                                 "Italy"='4180',
                                                                 "Japan"="3160",
                                                                 "Netherlands"="4210",
                                                                 #"New Zealand"="5150",
                                                                 "Norway"="4220",
                                                                 "Portugal"="4240",
                                                                 "Singapore"="3350",
                                                                 "South Korea"="3325",
                                                                 "Spain"="4280",
                                                                 "Sweden"="4290",
                                                                 "Switzerland"="4300",
                                                                 "UK"="4308",
                                                                 "USA"="2450",
)) %>% filter(Frmat<3)

colnames(total_data) <- c(colnames(total_data)[1:6],"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown")


belgium_exposure <- fread("Life Expectancy/Exposures_5x1_BEL.txt") %>% data.frame()
australia_exposure <- fread("Life Expectancy/Exposures_5x1_AUS.txt") %>% data.frame()
austria_exposure <- fread("Life Expectancy/Exposures_5x1_AUT.txt") %>% data.frame()
canada_exposure <- fread("Life Expectancy/Exposures_5x1_CAN.txt") %>% data.frame()
denmark_exposure <- fread("Life Expectancy/Exposures_5x1_DEN.txt") %>% data.frame()
finland_exposure <- fread("Life Expectancy/Exposures_5x1_FIN.txt") %>% data.frame()
france_exposure <- fread("Life Expectancy/Exposures_5x1_FRA.txt") %>% data.frame()
germany_exposure <- fread("Life Expectancy/Exposures_5x1_GER.txt") %>% data.frame()
italy_exposure <- fread("Life Expectancy/Exposures_5x1_ITA.txt") %>% data.frame()
japan_exposure <- fread("Life Expectancy/Exposures_5x1_JPN.txt") %>% data.frame()
korea_exposure <- fread("Life Expectancy/Exposures_5x1_KOR.txt") %>% data.frame()
netherlands_exposure <- fread("Life Expectancy/Exposures_5x1_NET.txt") %>% data.frame()
norway_exposure <- fread("Life Expectancy/Exposures_5x1_NOR.txt") %>% data.frame()
portugal_exposure <- fread("Life Expectancy/Exposures_5x1_POR.txt") %>% data.frame()
spain_exposure <- fread("Life Expectancy/Exposures_5x1_SPA.txt") %>% data.frame()
sweden_exposure <- fread("Life Expectancy/Exposures_5x1_SWE.txt") %>% data.frame()
switzerland_exposure <- fread("Life Expectancy/Exposures_5x1_SWI.txt") %>% data.frame()
uk_exposure <- fread("Life Expectancy/Exposures_5x1_UK.txt") %>% data.frame()
usa_exposure <- fread("Life Expectancy/Exposures_5x1_USA.txt") %>% data.frame()
hongkong_exposure <- fread("Life Expectancy/Exposures_5x1_HK.txt") %>% data.frame()
#write.csv(hk_exposure,"Life Expectancy/Exposure_HK.csv",row.names = F)
## Exposure numbers (mid-year population)
total_exposure <- rbind(cbind(belgium_exposure %>% filter(Year>1949) %>% mutate(Male=as.numeric(Male)+0,Female=as.numeric(Female)+0),Country="Belgium"),
                        cbind(australia_exposure %>% filter(Year>1949),Country="Australia"),
                        cbind(austria_exposure %>% filter(Year>1949),Country="Austria"),
                        cbind(canada_exposure %>% filter(Year>1949),Country="Canada"),
                        cbind(denmark_exposure %>% filter(Year>1949),Country="Denmark"),
                        cbind(finland_exposure %>% filter(Year>1949),Country="Finland"),
                        cbind(france_exposure %>% filter(Year>1949),Country="France"),
                        cbind(germany_exposure %>% filter(Year>1949),Country="Germany"),
                        cbind(hongkong_exposure %>% filter(Year>1949),Country="Hong Kong"),
                        #cbind(hungary_exposure %>% filter(Year>1949),Country="Hungary"),
                        # cbind(iceland_exposure %>% filter(Year>1949),Country="Iceland"),
                        #cbind(ireland_exposure %>% filter(Year>1949),Country="Ireland"),
                        cbind(italy_exposure %>% filter(Year>1949),Country="Italy"),
                        cbind(japan_exposure %>% filter(Year>1949),Country="Japan"),
                        cbind(netherlands_exposure %>% filter(Year>1949),Country="Netherlands"),
                        #  cbind(newzealand_exposure %>% filter(Year>1949),Country="New Zealand"),
                        cbind(norway_exposure %>% filter(Year>1949),Country="Norway"),
                        cbind(portugal_exposure %>% filter(Year>1949),Country="Portugal"),
                        cbind(korea_exposure %>% filter(Year>1949),Country="South Korea"),
                        cbind(spain_exposure %>% filter(Year>1949),Country="Spain"),
                        cbind(sweden_exposure %>% filter(Year>1949),Country="Sweden"),
                        cbind(switzerland_exposure %>% filter(Year>1949),Country="Switzerland"),
                        cbind(uk_exposure %>% filter(Year>1949),Country="UK"),
                        cbind(usa_exposure %>% filter(Year>1949),Country="USA")
)
total_exposure$Female <- as.numeric(total_exposure$Female)
total_exposure$Male <- as.numeric(total_exposure$Male)
total_exposure_male80 <- total_exposure %>% select(-Total,-Female) %>% group_by(Year,Country) %>% filter(Age %in% c("80-84","85-89","90-94","95-99","100-104","105-109","110+")) %>% summarize(sum(Male)) %>% data.frame()
total_exposure_female80 <- total_exposure %>% select(-Total,-Male) %>% group_by(Year,Country) %>% filter(Age %in% c("80-84","85-89","90-94","95-99","100-104","105-109","110+")) %>% summarize(sum(Female)) %>% data.frame()
total_exposure80 <- total_exposure_male80 %>% left_join(.,total_exposure_female80,by=c("Country","Year"))
total_exposure80$Age <- '80+'
colnames(total_exposure80) <- c("Year","Country","Male","Female","Age")
pop_total <- total_exposure %>% select(-Total) %>% filter(Age %in% c("35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79")) %>% rbind(.,total_exposure80)
pop_total <- pop_total %>% reshape2::melt(.,measure.vars=3:4)
colnames(pop_total) <- c("Year","Age","Country","Sex","Exposure")
hk_exposure <- read.csv("Life Expectancy/Exposure_HK.csv",header=T) %>% data.frame()
hk_exposure <- hk_exposure %>% arrange(Sex,Year, Age)
colnames(pop_total) <- c("Year","Age","Country","Sex","Population")
pop_total$Population <- pop_total$Population/1000

hk_deaths <- DeathData7618 %>% filter(!Age < 35) %>% select(Year,Sex,Age,ICD,cod) %>% filter(!(ICD == 8 | is.na(cod)))
hk_deaths <- hk_deaths %>% mutate(Agegroup = cut(Age, c(seq(35,80,5),99), right = FALSE)) %>% mutate(Agegroup = forcats::fct_recode(factor(Agegroup),
                                                                                                                                    "35-39"="[35,40)",
                                                                                                                                    "40-44"="[40,45)",
                                                                                                                                    "45-49"="[45,50)",
                                                                                                                                    "50-54"="[50,55)",
                                                                                                                                    "55-59"="[55,60)",
                                                                                                                                    "60-64"="[60,65)",
                                                                                                                                    "65-69"="[65,70)",
                                                                                                                                    "70-74"="[70,75)",
                                                                                                                                    "75-79"="[75,80)",
                                                                                                                                    "80+"="[80,99)"))

# Lung cancer
lc_male_hk <- hk_deaths %>% filter(cod == 162 | substr(cod,1,3) %in% c("C33","C34")) %>% filter(Sex == "Male") %>% select(Year,Agegroup) %>% table() %>% data.frame() %>% rename("Age" = "Agegroup", "LungCancer" = "Freq")
lc_female_hk <- hk_deaths %>% filter(cod == 162 | substr(cod,1,3) %in% c("C33","C34")) %>% filter(Sex == "Female") %>% select(Year,Agegroup) %>% table() %>% data.frame() %>% rename("Age" = "Agegroup", "LungCancer" = "Freq")

# Aerodigestive cancer
adc_male_hk <- hk_deaths %>% filter(substr(cod,1,3) %in% c(140:150,161,paste('C0',c(0:9),sep=''),paste('C',c(10:15,32),sep=''))) %>% filter(Sex == "Male") %>% select(Year,Agegroup) %>% table() %>% data.frame() %>% rename("Age" = "Agegroup", "Aerodigestive" = "Freq")
adc_female_hk <- hk_deaths %>% filter(substr(cod,1,3) %in% c(140:150,161,paste('C0',c(0:9),sep=''),paste('C',c(10:15,32),sep=''))) %>% filter(Sex == "Female") %>% select(Year,Agegroup) %>% table() %>% data.frame() %>% rename("Age" = "Agegroup", "Aerodigestive" = "Freq")

# All cancer
ac_male_hk <- hk_deaths %>% filter(cod %in% (140:209) | substr(cod,1,1) == "C") %>% filter(Sex == "Male") %>% select(Year,Agegroup) %>% table() %>% data.frame() %>% rename("Age" = "Agegroup", "AllCancer" = "Freq")
ac_female_hk <- hk_deaths %>% filter(cod %in% (140:209) | substr(cod,1,1) == "C") %>% filter(Sex == "Female") %>% select(Year,Agegroup) %>% table() %>% data.frame() %>% rename("Age" = "Agegroup", "AllCancer" = "Freq")

# COPD
copd_male_hk <- hk_deaths %>% filter(substr(cod,1,3) %in% c(492:496,paste('J4',c(0:7),sep=''),"J67")) %>% filter(Sex == "Male") %>% select(Year,Agegroup) %>% table() %>% data.frame() %>% rename("Age" = "Agegroup", "COPD" = "Freq")
copd_female_hk <- hk_deaths %>% filter(substr(cod,1,3) %in% c(492:496,paste('J4',c(0:7),sep=''),"J67")) %>% filter(Sex == "Female") %>% select(Year,Agegroup) %>% table() %>% data.frame() %>% rename("Age" = "Agegroup", "COPD" = "Freq")

# All medical
am_male_hk <- hk_deaths %>% filter((substr(cod,1,3) %in% c(100:799,paste0("00",0:9),paste0("0",10:99))) | !(substr(cod,1,1) %in% c("V","W","X","Y"))) %>% filter(Sex == "Male") %>% select(Year,Agegroup) %>% table() %>% data.frame() %>% rename("Age" = "Agegroup", "AllMedical" = "Freq")
am_female_hk <- hk_deaths %>% filter((substr(cod,1,3) %in% c(100:799,paste0("00",0:9),paste0("0",10:99))) | !(substr(cod,1,1) %in% c("V","W","X","Y"))) %>% filter(Sex == "Female") %>% select(Year,Agegroup) %>% table() %>% data.frame() %>% rename("Age" = "Agegroup", "AllMedical" = "Freq")

# Liver cirrhosis
lci_male_hk <- hk_deaths %>% filter(cod == 571 | substr(cod,1,3) %in% c("K70","K74")) %>% filter(Sex == "Male") %>% select(Year,Agegroup) %>% table() %>% data.frame() %>% rename("Age" = "Agegroup", "LiverCirrhosis" = "Freq")
lci_female_hk <- hk_deaths %>% filter(cod == 571 | substr(cod,1,3) %in% c("K70","K74")) %>% filter(Sex == "Female") %>% select(Year,Agegroup) %>% table() %>% data.frame() %>% rename("Age" = "Agegroup", "LiverCirrhosis" = "Freq")

# Total deaths
total_male_hk <- hk_deaths  %>% filter(Sex == "Male") %>% select(Year,Agegroup) %>% table() %>% data.frame() %>% rename("Age" = "Agegroup", "Total" = "Freq")
total_female_hk <- hk_deaths  %>% filter(Sex == "Female") %>% select(Year,Agegroup) %>% table() %>% data.frame() %>% rename("Age" = "Agegroup", "Total" = "Freq")

# Merge Diseases Raw Mortality Datasets (Male)
pop_hk_exposure<-pop_total %>% filter (Country=="Hong Kong")
male_total_hk <- lc_male_hk %>% left_join(.,adc_male_hk,by = c("Year","Age")) %>% left_join(.,ac_male_hk,by = c("Year","Age")) %>%
  left_join(.,copd_male_hk,by = c("Year","Age")) %>% left_join(.,am_male_hk,by = c("Year","Age")) %>%
  left_join(.,total_male_hk,by = c("Year","Age")) %>% left_join(.,lci_male_hk,by = c("Year","Age")) %>%
  mutate(OtherCancer = AllCancer - Aerodigestive - LungCancer, OtherMedical = AllMedical - AllCancer - COPD - LiverCirrhosis)
male_total_hk$Year <- as.numeric(as.character(male_total_hk$Year))
male_total_hk <- male_total_hk %>% left_join(.,pop_hk_exposure %>% filter(Sex == "Male") %>% select(Year,Age,Population), by = c("Age","Year"))

male_total_hk <- bind_rows(male_total_hk %>% filter(Age %in% c('35-39','40-44','45-49','50-54','55-59')) %>% group_by(Year) %>%
                             summarise(Population = sum(Population),Total = sum(Total),AllCancer = sum(AllCancer),LungCancer = sum(LungCancer),
                                       Aerodigestive = sum(Aerodigestive),OtherCancer = sum(OtherCancer),COPD = sum(COPD),
                                       LiverCirrhosis = sum(LiverCirrhosis), AllMedical=sum(AllMedical),
                                       OtherMedical = sum(OtherMedical)) %>% mutate(Age = '35-59') %>% as.data.frame(),
                           male_total_hk %>% filter(Age %in% c('60-64','65-69','70-74','75-79','80+')))


# Lung Cancer Rate (Male)
male_total_lc_rate_hk <- male_total_hk %>% select(Year,Age,Population,LungCancer) %>% mutate(LungCancerRate = 100*LungCancer/Population)
male_total_lc_rate_80_hk <- male_total_lc_rate_hk %>% filter(Age == "80+") %>% select(-LungCancerRate) %>%
  left_join(.,male_total_lc_rate_hk %>% filter(Age == "75-79") %>% select(-Population,-LungCancer,-Age),by=c("Year"))
male_total_lc_rate_hk <- male_total_lc_rate_hk %>% filter(Age != "80+") %>% rbind(male_total_lc_rate_80_hk)


male_HK <- male_total_hk  %>% left_join(.,male_total_lc_rate_hk %>% select(-Population,-LungCancer),by = c("Year","Age")) %>% left_join(.,male_total_hk %>% select(Age) %>% unique() %>%
                                                                                                                                          mutate(NSm_Appendix = c(7.3,14,20,27,35,35),
                                                                                                                                                 Sm_Appendix = c(135.8,375,599,899,1168,1168),
                                                                                                                                                 NSm_LIMOR = c(7.82,41.22,45.64,50.44,63.32,63.32),
                                                                                                                                                 
                                                                                                                                                 ER_OtherMedical=c(0.349,0.300,0.271,0.056,0.010,0.010)),by="Age")




# Smoking Attributable Deaths Calculation (Male)
HKmale_PetoLopez_AF <- male_HK %>% mutate(SIR = ((LungCancerRate-NSm_LIMOR)/(Sm_Appendix-NSm_Appendix))*(NSm_Appendix/NSm_LIMOR), 
                                          SIR = ifelse(SIR < 0,0,ifelse(SIR>1,1,SIR)),
                                          SAF_LungCancer = (LungCancerRate - NSm_LIMOR)*Population/100,
                                          SAF_COPD = (1.165*SIR/(1+1.165*SIR))*COPD,
                                          SAF_Aerodigestive = (0.747*SIR/(1+0.747*SIR))*Aerodigestive,
                                          SAF_OtherCancer = (0.145*SIR/(1+0.145*SIR))*OtherCancer,
                                          
                                          SAF_OtherMedical = (ER_OtherMedical*SIR/(1+ER_OtherMedical*SIR))*OtherMedical,
                                          SAF = ifelse(SAF_LungCancer < 0, 0, ifelse(SAF_LungCancer > LungCancer, LungCancer, SAF_LungCancer)) + SAF_COPD  +
                                            SAF_Aerodigestive + SAF_OtherCancer +
                                            SAF_OtherMedical)
HKmale_PetoLopez_AF <- HKmale_PetoLopez_AF %>% arrange(Year, Age)
HKmale_PetoLopez_AF_age <- as.data.frame(HKmale_PetoLopez_AF %>% select(Year,Age,Total,SAF,SIR) %>% mutate(AF = SAF/Total))
HKmale_PetoLopez_AF$Country <- "Hong Kong"
HKmale_PetoLopez_AF_age$Country<-"Hong Kong"

# Merge Diseases Raw Mortality Datasets (Female)
pop_hk_exposure<-pop_total %>% filter( Country=="Hong Kong")
female_total_hk <- lc_female_hk %>% left_join(.,adc_female_hk,by = c("Year","Age")) %>% left_join(.,ac_female_hk,by = c("Year","Age")) %>%
  left_join(.,copd_female_hk,by = c("Year","Age")) %>% left_join(.,am_female_hk,by = c("Year","Age")) %>%
  left_join(.,total_female_hk,by = c("Year","Age")) %>% left_join(.,lci_female_hk,by = c("Year","Age")) %>%
  mutate(OtherCancer = AllCancer - Aerodigestive - LungCancer, OtherMedical = AllMedical - AllCancer - COPD - LiverCirrhosis)
female_total_hk$Year <- as.numeric(as.character(female_total_hk$Year))
female_total_hk <- female_total_hk %>% left_join(.,pop_hk_exposure %>% filter(Sex == "Female") %>% select(Year,Age,Population), by = c("Age","Year"))

female_total_hk <- bind_rows(female_total_hk %>% filter(Age %in% c('35-39','40-44','45-49','50-54','55-59')) %>% group_by(Year) %>%
                               summarise(Population = sum(Population),Total = sum(Total),AllCancer = sum(AllCancer),LungCancer = sum(LungCancer),
                                         Aerodigestive = sum(Aerodigestive),OtherCancer = sum(OtherCancer),COPD = sum(COPD),
                                         LiverCirrhosis = sum(LiverCirrhosis),
                                         OtherMedical = sum(OtherMedical), AllMedical = sum (AllMedical)) %>% mutate(Age = '35-59') %>% as.data.frame(),
                             female_total_hk %>% filter(Age %in% c('60-64','65-69','70-74','75-79','80+')))

# Lung Cancer Rate (Female)
female_total_lc_rate_hk <- female_total_hk %>% select(Year,Age,Population,LungCancer) %>% mutate(LungCancerRate = 100*LungCancer/Population)
female_total_lc_rate_80_hk <- female_total_lc_rate_hk %>% filter(Age == "80+") %>% select(-LungCancerRate) %>%
  left_join(.,female_total_lc_rate_hk %>% filter(Age == "75-79") %>% select(-Population,-LungCancer,-Age),by=c("Year"))
female_total_lc_rate_hk <- female_total_lc_rate_hk %>% filter(Age != "80+") %>% rbind(female_total_lc_rate_80_hk)
female_nsm_forecast<-read.csv("Life Expectancy/smoking/time varying NSm_LIMOR.csv",header=T) %>% data.frame()
female_nsm_b1998<-data.frame(Year = rep(1961:1997, each = 6),
                             Age = rep(c("35-59","60-64", "65-69", "70-74", "75-79","80+"), 37), 
                             NSm_LIMOR = rep(c(11.68,55.38,66.23,118.56,152.70,152.70), 37))
female_nsm_hk<- female_nsm_b1998 %>% rbind(., female_nsm_forecast %>% select(Year,Age, NSm_LIMOR)) %>% arrange(Year, Age)
female_HK <- female_total_hk %>% left_join(.,female_total_lc_rate_hk %>% select(-Population,-LungCancer),by = c("Year","Age")) %>% left_join(.,female_total_hk %>% select(Age) %>% unique() %>%
                                                                                                                                               mutate(NSm_Appendix = c(7,14,19,26,34,34),
                                                                                                                                                      Sm_Appendix = c(80.4,195,310,339,429,429),
                                                                                                                                                      ER_OtherMedical=c(0.227,0.176,0.203,0.230,0.184,0.184)),by="Age")
female_HK <- female_HK %>% left_join(., female_nsm_hk,by = c("Year", "Age"))
# Smoking Attributable Deaths Calculation (Female)
HKfemale_PetoLopez_AF <- female_HK %>% mutate(SIR = (LungCancerRate-NSm_LIMOR)/(Sm_Appendix-NSm_Appendix)*(NSm_Appendix/NSm_LIMOR),
                                              SIR = ifelse(SIR < 0,0,ifelse(SIR>1,1,SIR)),
                                              SAF_LungCancer = (LungCancerRate - NSm_LIMOR)*Population/100,
                                              SAF_COPD = (2.797*SIR/(1+2.797*SIR))*COPD,
                                              SAF_Aerodigestive = (0.451*SIR/(1+0.451*SIR))*Aerodigestive,
                                              SAF_OtherCancer = (0.140*SIR/(1+0.140*SIR))*OtherCancer,
                                              
                                              SAF_OtherMedical = (ER_OtherMedical*SIR/(1+ER_OtherMedical*SIR))*OtherMedical,
                                              SAF = ifelse(SAF_LungCancer < 0, 0, ifelse(SAF_LungCancer > LungCancer, LungCancer, SAF_LungCancer)) + SAF_COPD + 
                                                SAF_Aerodigestive + SAF_OtherCancer +
                                                SAF_OtherMedical)
HKfemale_PetoLopez_AF <- HKfemale_PetoLopez_AF %>% arrange(Year,Age)
HKfemale_PetoLopez_AF_age <- as.data.frame(HKfemale_PetoLopez_AF %>% select(Year,Age,Total,SIR,SAF) %>%  mutate(AF = SAF/Total))
HKfemale_PetoLopez_AF$Country <- "Hong Kong"
HKfemale_PetoLopez_AF_age$Country<-"Hong Kong"

# Overall Mortality
total_mo_male <- bind_rows(total_data %>% filter(List == 104 & Sex == 1 & Cause == "AAA") %>% select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),
                           total_data %>% filter(List == 103 & Sex == 1 & Cause == "AAA") %>% select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),
                           total_data %>% filter(List == "10M" & Sex == 1 & Cause == "AAA") %>% select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),
                           total_data %>% filter(List == '09B' & Sex == 1 & Cause == "B00") %>% select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),
                           total_data %>% filter(List == '08A' & Sex == 1 & Cause == "A000") %>% select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),
                           total_data %>% filter(List == '07A' & Sex == 1 & Cause == "A000") %>% select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"))
total_mo_male$`80+` <- rowSums(total_mo_male[,12:15],na.rm = T)

total_mo_female <- bind_rows(total_data %>% filter(List == 104 & Sex == 2 & Cause == "AAA") %>% select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),
                             total_data %>% filter(List == 103 & Sex == 2 & Cause == "AAA") %>% select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),
                             total_data %>% filter(List == "10M" & Sex == 2 & Cause == "AAA") %>% select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),
                             total_data %>% filter(List == '09B' & Sex == 2 & Cause == "B00") %>% select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),
                             total_data %>% filter(List == '08A' & Sex == 2 & Cause == "A000") %>% select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),
                             total_data %>% filter(List == '07A' & Sex == 2 & Cause == "A000") %>% select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"))
total_mo_female$`80+` <- rowSums(total_mo_female[,12:15],na.rm = T)

# Lung Cancer Mortality
total_ml_male <- bind_rows(ddply(total_data %>% filter(List == 104 & Sex == 1 & Cause %in% c("C33","C340","C341","C342","C343","C344","C345","C346","C347","C348","C349")) %>%
                                   select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                           ddply(total_data %>% filter(List == "10M" & Sex == 1 & Cause %in% c("C33","C340","C341","C342","C343","C344","C345","C346","C347","C348","C349")) %>%
                                   select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                           ddply(total_data %>% filter(List == 103 & Sex == 1 & Cause %in% c("C33","C34")) %>%
                                   select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                           ddply(total_data %>% filter(List == '09B' & Sex == 1 & Cause == "B101") %>%
                                   select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                           ddply(total_data %>% filter(List == '08A' & Sex == 1 & Cause == "A051") %>%
                                   select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                           ddply(total_data %>% filter(List == '07A' & Sex == 1 & Cause == "A050") %>%
                                   select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)))
total_ml_male$`80+` <- rowSums(total_ml_male[,12:15],na.rm = T)

total_ml_female <- bind_rows(ddply(total_data %>% filter(List == 104 & Sex == 2 & Cause %in% c("C33","C340","C341","C342","C343","C344","C345","C346","C347","C348","C349")) %>%
                                     select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                             ddply(total_data %>% filter(List == "10M" & Sex == 2 & Cause %in% c("C33","C340","C341","C342","C343","C344","C345","C346","C347","C348","C349")) %>%
                                     select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                             ddply(total_data %>% filter(List == 103 & Sex == 2 & Cause %in% c("C33","C34")) %>%
                                     select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                             ddply(total_data %>% filter(List == '09B' & Sex == 2 & Cause == "B101") %>%
                                     select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                             ddply(total_data %>% filter(List == '08A' & Sex == 2 & Cause == "A051") %>%
                                     select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                             ddply(total_data %>% filter(List == '07A' & Sex == 2 & Cause == "A050") %>%
                                     select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)))
total_ml_female$`80+` <- rowSums(total_ml_female[,12:15],na.rm = T)

# Upper Aerodigestive Cancer Mortality
total_adc_male <- bind_rows(ddply(total_data %>% filter(List == 104 & Sex == 1) %>% filter(substr(Cause,1,3) %in% c(paste('C0',c(0:9),sep=''),paste('C',c(10:15,32),sep=''))) %>%
                                    select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                            ddply(total_data %>% filter(List == "10M" & Sex == 1) %>% filter(substr(Cause,1,3) %in% c(paste('C0',c(0:9),sep=''),paste('C',c(10:15,32),sep=''))) %>%
                                    select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                            ddply(total_data %>% filter(List == 103 & Sex == 1) %>% filter(substr(Cause,1,3) %in% c(paste('C0',c(0:9),sep=''),paste('C',c(10:15,32),sep=''))) %>%
                                    select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                            ddply(total_data %>% filter(List == '09B' & Sex == 1 & Cause  %in% c("B08","B090","B100")) %>%
                                    select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                            ddply(total_data %>% filter(List == '08A' & Sex == 1 & Cause  %in% c("A045","A046","A050")) %>%
                                    select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                            ddply(total_data %>% filter(List == '07A' & Sex == 1 & Cause %in% c("A044","A045","A049")) %>%
                                    select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)))
total_adc_male$`80+` <- rowSums(total_adc_male[,12:15],na.rm = T)

total_adc_female <- bind_rows(ddply(total_data %>% filter(List == 104 & Sex == 2) %>% filter(substr(Cause,1,3) %in% c(paste('C0',c(0:9),sep=''),paste('C',c(10:15,32),sep=''))) %>%
                                      select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                              ddply(total_data %>% filter(List == "10M" & Sex == 2) %>% filter(substr(Cause,1,3) %in% c(paste('C0',c(0:9),sep=''),paste('C',c(10:15,32),sep=''))) %>%
                                      select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                              ddply(total_data %>% filter(List == 103 & Sex == 2) %>% filter(substr(Cause,1,3) %in% c(paste('C0',c(0:9),sep=''),paste('C',c(10:15,32),sep=''))) %>%
                                      select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                              ddply(total_data %>% filter(List == '09B' & Sex == 2 & Cause  %in% c("B08","B090","B100")) %>%
                                      select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                              ddply(total_data %>% filter(List == '08A' & Sex == 2 & Cause  %in% c("A045","A046","A050")) %>%
                                      select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                              ddply(total_data %>% filter(List == '07A' & Sex == 2 & Cause %in% c("A044","A045","A049")) %>%
                                      select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)))
total_adc_female$`80+` <- rowSums(total_adc_female[,12:15],na.rm = T)

# COPD Mortality
total_copd_male <- bind_rows(ddply(total_data %>% filter(List == 104 & Sex == 1) %>% filter(substr(Cause,1,3) %in% c(paste('J4',c(0:7),sep=''),"J67")) %>%
                                     select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                             ddply(total_data %>% filter(List == "10M" & Sex == 1) %>% filter(substr(Cause,1,3) %in% c(paste('J4',c(0:7),sep=''),"J67")) %>%
                                     select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                             ddply(total_data %>% filter(List == 103 & Sex == 1) %>% filter(substr(Cause,1,3) %in% c(paste('J4',c(0:7),sep=''),"J67")) %>%
                                     select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                             ddply(total_data %>% filter(List == '09B' & Sex == 1 & Cause  %in% c("B323","B324","B325")) %>%
                                     select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                             ddply(total_data %>% filter(List == '08A' & Sex == 1 & Cause  %in% c("A093","A094","A095","A096")) %>%
                                     select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                             ddply(total_data %>% filter(List == '07A' & Sex == 1 & Cause %in% c(paste("A09",c(2:7),sep=""))) %>%
                                     select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)))
total_copd_male$`80+` <- rowSums(total_copd_male[,12:15],na.rm = T)

total_copd_female <- bind_rows(ddply(total_data %>% filter(List == 104 & Sex == 2) %>% filter(substr(Cause,1,3) %in% c(paste('J4',c(0:7),sep=''),"J67")) %>%
                                       select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                               ddply(total_data %>% filter(List == "10M" & Sex == 2) %>% filter(substr(Cause,1,3) %in% c(paste('J4',c(0:7),sep=''),"J67")) %>%
                                       select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                               ddply(total_data %>% filter(List == 103 & Sex == 2) %>% filter(substr(Cause,1,3) %in% c(paste('J4',c(0:7),sep=''),"J67")) %>%
                                       select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                               ddply(total_data %>% filter(List == '09B' & Sex == 2 & Cause  %in% c("B323","B324","B325")) %>%
                                       select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                               ddply(total_data %>% filter(List == '08A' & Sex == 2 & Cause  %in% c("A093","A094","A095","A096")) %>%
                                       select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                               ddply(total_data %>% filter(List == '07A' & Sex == 2 & Cause %in% c(paste("A09",c(2:7),sep=""))) %>%
                                       select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)))
total_copd_female$`80+` <- rowSums(total_copd_female[,12:15],na.rm = T)

# Vascular Diseases Mortality
total_vd_male <- bind_rows(ddply(total_data %>% filter(List == 104 & Sex == 1) %>% filter(substr(Cause,1,3) %in% c(paste('I0',c(0:9),sep=''),paste("I",c(10:99),sep=""))) %>%
                                   select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                           ddply(total_data %>% filter(List == "10M" & Sex == 1) %>% filter(substr(Cause,1,3) %in% c(paste('I0',c(0:9),sep=''),paste("I",c(10:99),sep=""))) %>%
                                   select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                           ddply(total_data %>% filter(List == 103 & Sex == 1) %>% filter(substr(Cause,1,3) %in% c(paste('I0',c(0:9),sep=''),paste("I",c(10:99),sep=""))) %>%
                                   select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                           ddply(total_data %>% filter(List == '09B' & Sex == 1 & Cause  %in% c(paste('B',c(25:30),sep=''))) %>%
                                   select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                           ddply(total_data %>% filter(List == '08A' & Sex == 1 & Cause  %in% c(paste('A0',c(80:88),sep=''))) %>%
                                   select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                           ddply(total_data %>% filter(List == '07A' & Sex == 1 & Cause %in% c(paste('A0',c(70,79:86),sep=''))) %>%
                                   select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)))
total_vd_male$`80+` <- rowSums(total_vd_male[,12:15],na.rm = T)

total_vd_female <- bind_rows(ddply(total_data %>% filter(List == 104 & Sex == 2) %>% filter(substr(Cause,1,3) %in% c(paste('I0',c(0:9),sep=''),paste("I",c(10:99),sep=""))) %>%
                                     select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                             ddply(total_data %>% filter(List == "10M" & Sex == 2) %>% filter(substr(Cause,1,3) %in% c(paste('I0',c(0:9),sep=''),paste("I",c(10:99),sep=""))) %>%
                                     select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                             ddply(total_data %>% filter(List == 103 & Sex == 2) %>% filter(substr(Cause,1,3) %in% c(paste('I0',c(0:9),sep=''),paste("I",c(10:99),sep=""))) %>%
                                     select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                             ddply(total_data %>% filter(List == '09B' & Sex == 2 & Cause  %in% c(paste('B',c(25:30),sep=''))) %>%
                                     select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                             ddply(total_data %>% filter(List == '08A' & Sex == 2 & Cause  %in% c(paste('A0',c(80:88),sep=''))) %>%
                                     select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                             ddply(total_data %>% filter(List == '07A' & Sex == 2 & Cause %in% c(paste('A0',c(70,79:86),sep=''))) %>%
                                     select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)))
total_vd_female$`80+` <- rowSums(total_vd_female[,12:15],na.rm = T)

# Liver Cirrhosis Mortality
total_lc_male <- bind_rows(ddply(total_data %>% filter(List == 104 & Sex == 1) %>% filter(substr(Cause,1,3) %in% c("K70","K74")) %>%
                                   select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                           ddply(total_data %>% filter(List == "10M" & Sex == 1) %>% filter(substr(Cause,1,3) %in% c("K70","K74")) %>%
                                   select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                           ddply(total_data %>% filter(List == 103 & Sex == 1) %>% filter(Cause %in% c("K70","K74")) %>%
                                   select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                           ddply(total_data %>% filter(List == '09B' & Sex == 1 & Cause == "B347") %>%
                                   select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                           ddply(total_data %>% filter(List == '08A' & Sex == 1 & Cause == "A102") %>%
                                   select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                           ddply(total_data %>% filter(List == '07A' & Sex == 1 & Cause == "A105") %>%
                                   select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)))
#total_lc_male <- rbind(total_lc_male,c(2001,'Iceland',rep(0,14)),c(2002,'Iceland',rep(0,14)))
total_lc_male <- total_lc_male %>% mutate_if(is.character,as.numeric)
total_lc_male$`80+` <- rowSums(total_lc_male[,12:15],na.rm = T)

total_lc_female <- bind_rows(ddply(total_data %>% filter(List == 104 & Sex == 2) %>% filter(substr(Cause,1,3) %in% c("K70","K74")) %>%
                                     select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                             ddply(total_data %>% filter(List == "10M" & Sex == 2) %>% filter(substr(Cause,1,3) %in% c("K70","K74")) %>%
                                     select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                             ddply(total_data %>% filter(List == 103 & Sex == 2) %>% filter(Cause %in% c("K70","K74")) %>%
                                     select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                             ddply(total_data %>% filter(List == '09B' & Sex == 2 & Cause == "B347") %>%
                                     select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                             ddply(total_data %>% filter(List == '08A' & Sex == 2 & Cause == "A102") %>%
                                     select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                             ddply(total_data %>% filter(List == '07A' & Sex == 2 & Cause == "A105") %>%
                                     select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)))
#total_lc_female <- rbind(total_lc_female,c(2010,'Iceland',rep(0,14)))
total_lc_female <- total_lc_female %>% mutate_if(is.character,as.numeric)
total_lc_female$`80+` <- rowSums(total_lc_female[,12:15],na.rm = T)

# Non-Medical Causes Mortality
total_nmc_male <- bind_rows(ddply(total_data %>% filter(List == 104 & Sex == 1) %>% filter(substr(Cause,1,1) %in% c("V","W","X","Y")) %>%
                                    select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                            ddply(total_data %>% filter(List == "10M" & Sex == 1) %>% filter(substr(Cause,1,1) %in% c("V","W","X","Y")) %>%
                                    select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                            ddply(total_data %>% filter(List == 103 & Sex == 1) %>% filter(substr(Cause,1,1) %in% c("V","W","X","Y")) %>%
                                    select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                            ddply(total_data %>% filter(List == '09B' & Sex == 1) %>% filter(Cause %in% c(paste('B',c(47:56),sep=''))) %>%
                                    select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                            ddply(total_data %>% filter(List == '08A' & Sex == 1) %>% filter(Cause  %in% c(paste('A',c(138:150),sep=''))) %>%
                                    select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                            ddply(total_data %>% filter(List == '07A' & Sex == 1) %>% filter(Cause %in% c(paste('A',c(138:150),sep=''))) %>%
                                    select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)))
total_nmc_male$`80+` <- rowSums(total_nmc_male[,12:15],na.rm = T)

total_nmc_female <- bind_rows(ddply(total_data %>% filter(List == 104 & Sex == 2) %>% filter(substr(Cause,1,1) %in% c("V","W","X","Y")) %>%
                                      select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                              ddply(total_data %>% filter(List == "10M" & Sex == 2) %>% filter(substr(Cause,1,1) %in% c("V","W","X","Y")) %>%
                                      select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                              ddply(total_data %>% filter(List == 103 & Sex == 2) %>% filter(substr(Cause,1,1) %in% c("V","W","X","Y")) %>%
                                      select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                              ddply(total_data %>% filter(List == '09B' & Sex == 2) %>% filter(Cause %in% c(paste('B',c(47:56),sep=''))) %>%
                                      select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                              ddply(total_data %>% filter(List == '08A' & Sex == 2) %>% filter(Cause  %in% c(paste('A',c(138:150),sep=''))) %>%
                                      select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                              ddply(total_data %>% filter(List == '07A' & Sex == 2) %>% filter(Cause %in% c(paste('A',c(138:150),sep=''))) %>%
                                      select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)))
total_nmc_female$`80+` <- rowSums(total_nmc_female[,12:15],na.rm = T)

# Respiratory Diseases (Total) Mortality
total_rd_male <- bind_rows(ddply(total_data %>% filter(List == 104 & Sex == 1) %>% filter(substr(Cause,1,1) == "J") %>%
                                   select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                           ddply(total_data %>% filter(List == "10M" & Sex == 1) %>% filter(substr(Cause,1,1) == "J") %>%
                                   select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                           ddply(total_data %>% filter(List == 103 & Sex == 1) %>% filter(substr(Cause,1,1) == "J") %>%
                                   select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                           ddply(total_data %>% filter(List == '09B' & Sex == 1) %>% filter(Cause  %in% c("B31","B32")) %>%
                                   select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                           ddply(total_data %>% filter(List == '08A' & Sex == 1) %>% filter(Cause  %in% c(paste('A0',c(89:96),sep=''))) %>%
                                   select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                           ddply(total_data %>% filter(List == '07A' & Sex == 1) %>% filter(Cause %in% c(paste('A0',c(87:97),sep=''))) %>%
                                   select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)))
total_rd_male$`80+` <- rowSums(total_rd_male[,12:15],na.rm = T)

total_rd_female <- bind_rows(ddply(total_data %>% filter(List == 104 & Sex == 2) %>% filter(substr(Cause,1,1) == "J") %>%
                                     select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                             ddply(total_data %>% filter(List == "10M" & Sex == 2) %>% filter(substr(Cause,1,1) == "J") %>%
                                     select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                             ddply(total_data %>% filter(List == 103 & Sex == 2) %>% filter(substr(Cause,1,1) == "J") %>%
                                     select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                             ddply(total_data %>% filter(List == '09B' & Sex == 2) %>% filter(Cause  %in% c("B31","B32")) %>%
                                     select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                             ddply(total_data %>% filter(List == '08A' & Sex == 2) %>% filter(Cause  %in% c(paste('A0',c(89:96),sep=''))) %>%
                                     select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                             ddply(total_data %>% filter(List == '07A' & Sex == 2) %>% filter(Cause %in% c(paste('A0',c(87:97),sep=''))) %>%
                                     select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)))
total_rd_female$`80+` <- rowSums(total_rd_female[,12:15],na.rm = T)

# All Cancer (Total) Mortality
total_ac_male <- bind_rows(ddply(total_data %>% filter(List == 104 & Sex == 1) %>% filter(substr(Cause,1,1) == "C") %>%
                                   select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                           ddply(total_data %>% filter(List == "10M" & Sex == 1) %>% filter(substr(Cause,1,1) == "C") %>%
                                   select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                           ddply(total_data %>% filter(List == 103 & Sex == 1) %>% filter(substr(Cause,1,1) == "C") %>%
                                   select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                           ddply(total_data %>% filter(List == '09B' & Sex == 1) %>% filter(Cause  %in% c("B08","B09",paste('B',c(10:14),sep=''))) %>%
                                   select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                           ddply(total_data %>% filter(List == '08A' & Sex == 1) %>% filter(Cause  %in% c(paste('A0',c(45:60),sep=''))) %>%
                                   select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                           ddply(total_data %>% filter(List == '07A' & Sex == 1) %>% filter(Cause %in% c(paste('A0',c(44:59),sep=''))) %>%
                                   select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)))
total_ac_male$`80+` <- rowSums(total_ac_male[,12:15],na.rm = T)

total_ac_female <- bind_rows(ddply(total_data %>% filter(List == 104 & Sex == 2) %>% filter(substr(Cause,1,1) == "C") %>%
                                     select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                             ddply(total_data %>% filter(List == "10M" & Sex == 2) %>% filter(substr(Cause,1,1) == "C") %>%
                                     select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                             ddply(total_data %>% filter(List == 103 & Sex == 2) %>% filter(substr(Cause,1,1) == "C") %>%
                                     select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                             ddply(total_data %>% filter(List == '09B' & Sex == 2) %>% filter(Cause  %in% c("B08","B09",paste('B',c(10:14),sep=''))) %>%
                                     select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                             ddply(total_data %>% filter(List == '08A' & Sex == 2) %>% filter(Cause  %in% c(paste('A0',c(45:60),sep=''))) %>%
                                     select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)),
                             ddply(total_data %>% filter(List == '07A' & Sex == 2) %>% filter(Cause %in% c(paste('A0',c(44:59),sep=''))) %>%
                                     select(Country,Year,"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown"),c("Year","Country"),numcolwise(sum)))
total_ac_female$`80+` <- rowSums(total_ac_female[,12:15],na.rm = T)

# All Causes
mo_male <- total_mo_male %>% mutate("mid" = `35-39` + `40-44` + `45-49` + `50-54` + `55-59` + `60-64` + `65-69`,"old" = `70-74` + `75-79` + `80+`)
mo_female <- total_mo_female %>% mutate("mid" = `35-39` + `40-44` + `45-49` + `50-54` + `55-59` + `60-64` + `65-69`,"old" = `70-74` + `75-79` + `80+`)

# Cancer
ac_male <- total_ac_male %>% mutate("mid" = `35-39` + `40-44` + `45-49` + `50-54` + `55-59` + `60-64` + `65-69`,"old" = `70-74` + `75-79` + `80+`)
ac_female <- total_ac_female %>% mutate("mid" = `35-39` + `40-44` + `45-49` + `50-54` + `55-59` + `60-64` + `65-69`,"old" = `70-74` + `75-79` + `80+`)

adc_male <- total_adc_male %>% mutate("mid" = `35-39` + `40-44` + `45-49` + `50-54` + `55-59` + `60-64` + `65-69`,"old" = `70-74` + `75-79` + `80+`)
adc_female <- total_adc_female %>% mutate("mid" = `35-39` + `40-44` + `45-49` + `50-54` + `55-59` + `60-64` + `65-69`,"old" = `70-74` + `75-79` + `80+`)

lca_male <- total_ml_male %>% mutate("mid" = `35-39` + `40-44` + `45-49` + `50-54` + `55-59` + `60-64` + `65-69`,"old" = `70-74` + `75-79` + `80+`)
lca_female <- total_ml_female %>% mutate("mid" = `35-39` + `40-44` + `45-49` + `50-54` + `55-59` + `60-64` + `65-69`,"old" = `70-74` + `75-79` + `80+`)

# Liver Cirrhosis
lci_male <- total_lc_male %>% mutate("mid" = `35-39` + `40-44` + `45-49` + `50-54` + `55-59` + `60-64` + `65-69`,"old" = `70-74` + `75-79` + `80+`)
lci_female <- total_lc_female %>% mutate("mid" = `35-39` + `40-44` + `45-49` + `50-54` + `55-59` + `60-64` + `65-69`,"old" = `70-74` + `75-79` + `80+`)

# Vascular Diseases
vd_male <- total_vd_male %>% mutate("mid" = `35-39` + `40-44` + `45-49` + `50-54` + `55-59` + `60-64` + `65-69`,"old" = `70-74` + `75-79` + `80+`)
vd_female <- total_vd_female %>% mutate("mid" = `35-39` + `40-44` + `45-49` + `50-54` + `55-59` + `60-64` + `65-69`,"old" = `70-74` + `75-79` + `80+`)

# Respiratory Diseases
rd_male <- total_rd_male %>% mutate("mid" = `35-39` + `40-44` + `45-49` + `50-54` + `55-59` + `60-64` + `65-69`,"old" = `70-74` + `75-79` + `80+`)
rd_female <- total_rd_female %>% mutate("mid" = `35-39` + `40-44` + `45-49` + `50-54` + `55-59` + `60-64` + `65-69`,"old" = `70-74` + `75-79` + `80+`)

copd_male <- total_copd_male %>% mutate("mid" = `35-39` + `40-44` + `45-49` + `50-54` + `55-59` + `60-64` + `65-69`,"old" = `70-74` + `75-79` + `80+`)
copd_female <- total_copd_female %>% mutate("mid" = `35-39` + `40-44` + `45-49` + `50-54` + `55-59` + `60-64` + `65-69`,"old" = `70-74` + `75-79` + `80+`)

# Non-Medical Causes (Male)
nmc_male <- total_nmc_male %>% mutate("mid" = `35-39` + `40-44` + `45-49` + `50-54` + `55-59` + `60-64` + `65-69`,"old" = `70-74` + `75-79` + `80+`)
nmc_female <- total_nmc_female %>% mutate("mid" = `35-39` + `40-44` + `45-49` + `50-54` + `55-59` + `60-64` + `65-69`,"old" = `70-74` + `75-79` + `80+`)

# Convert from Wide to Long Format (Male)
male_mo <- gather(mo_male,"Age","Total",c("mid","old",`35-39`,`40-44`,`45-49`,`50-54`,`55-59`,`60-64`,`65-69`,`70-74`,`75-79`,`80+`)) %>% select(-`80-84`,-`85-89`,-`90-94`,-`95+`,-"Unknown") %>%
  left_join(.,pop_total %>% filter(Sex == "Male") %>% select("Country","Year","Age","Population"), by = c("Year","Country","Age")) %>% na.omit()
male_ac <- gather(ac_male,"Age","AllCancer",c("mid","old",`35-39`,`40-44`,`45-49`,`50-54`,`55-59`,`60-64`,`65-69`,`70-74`,`75-79`,`80+`)) %>% select(-`80-84`,-`85-89`,-`90-94`,-`95+`,-"Unknown") %>%
  left_join(.,pop_total %>% filter(Sex == "Male") %>% select("Country","Year","Age","Population") ,by = c("Year","Country","Age")) %>% na.omit()
male_adc <- gather(adc_male,"Age","Aerodigestive",c("mid","old",`35-39`,`40-44`,`45-49`,`50-54`,`55-59`,`60-64`,`65-69`,`70-74`,`75-79`,`80+`)) %>% select(-`80-84`,-`85-89`,-`90-94`,-`95+`,-"Unknown") %>%
  left_join(.,pop_total %>% filter(Sex == "Male") %>% select("Country","Year","Age","Population") ,by = c("Year","Country","Age")) %>% na.omit()
male_lca <- gather(lca_male,"Age","LungCancer",c("mid","old",`35-39`,`40-44`,`45-49`,`50-54`,`55-59`,`60-64`,`65-69`,`70-74`,`75-79`,`80+`)) %>% select(-`80-84`,-`85-89`,-`90-94`,-`95+`,-"Unknown") %>%
  left_join(.,pop_total %>% filter(Sex == "Male") %>% select("Country","Year","Age","Population") ,by = c("Year","Country","Age")) %>% na.omit()
male_lci <- gather(lci_male,"Age","LiverCirrhosis",c("mid","old",`35-39`,`40-44`,`45-49`,`50-54`,`55-59`,`60-64`,`65-69`,`70-74`,`75-79`,`80+`)) %>% select(-`80-84`,-`85-89`,-`90-94`,-`95+`,-"Unknown") %>%
  left_join(.,pop_total %>% filter(Sex == "Male") %>% select("Country","Year","Age","Population") ,by = c("Year","Country","Age")) %>% na.omit()
male_vd <- gather(vd_male,"Age","vascular",c("mid","old",`35-39`,`40-44`,`45-49`,`50-54`,`55-59`,`60-64`,`65-69`,`70-74`,`75-79`,`80+`)) %>% select(-`80-84`,-`85-89`,-`90-94`,-`95+`,-"Unknown") %>%
  left_join(.,pop_total %>% filter(Sex == "Male") %>% select("Country","Year","Age","Population") ,by = c("Year","Country","Age")) %>% na.omit()
male_rd <- gather(rd_male,"Age","Respiratory",c("mid","old",`35-39`,`40-44`,`45-49`,`50-54`,`55-59`,`60-64`,`65-69`,`70-74`,`75-79`,`80+`)) %>% select(-`80-84`,-`85-89`,-`90-94`,-`95+`,-"Unknown") %>%
  left_join(.,pop_total %>% filter(Sex == "Male") %>% select("Country","Year","Age","Population") ,by = c("Year","Country","Age")) %>% na.omit()
male_copd <- gather(copd_male,"Age","COPD",c("mid","old",`35-39`,`40-44`,`45-49`,`50-54`,`55-59`,`60-64`,`65-69`,`70-74`,`75-79`,`80+`)) %>% select(-`80-84`,-`85-89`,-`90-94`,-`95+`,-"Unknown") %>%
  left_join(.,pop_total %>% filter(Sex == "Male") %>% select("Country","Year","Age","Population") ,by = c("Year","Country","Age")) %>% na.omit()
male_nmc <- gather(nmc_male,"Age","NonMedical",c("mid","old",`35-39`,`40-44`,`45-49`,`50-54`,`55-59`,`60-64`,`65-69`,`70-74`,`75-79`,`80+`)) %>% select(-`80-84`,-`85-89`,-`90-94`,-`95+`,-"Unknown") %>%
  left_join(.,pop_total %>% filter(Sex == "Male") %>% select("Country","Year","Age","Population") ,by = c("Year","Country","Age")) %>% na.omit()

# Convert from Wide to Long Format (Female)
female_mo <- gather(mo_female,"Age","Total",c("mid","old",`35-39`,`40-44`,`45-49`,`50-54`,`55-59`,`60-64`,`65-69`,`70-74`,`75-79`,`80+`)) %>% select(-`80-84`,-`85-89`,-`90-94`,-`95+`,-"Unknown") %>%
  left_join(.,pop_total %>% filter(Sex == "Female") %>% select("Country","Year","Age","Population"), by = c("Year","Country","Age")) %>% na.omit()
female_ac <- gather(ac_female,"Age","AllCancer",c("mid","old",`35-39`,`40-44`,`45-49`,`50-54`,`55-59`,`60-64`,`65-69`,`70-74`,`75-79`,`80+`)) %>% select(-`80-84`,-`85-89`,-`90-94`,-`95+`,-"Unknown") %>%
  left_join(.,pop_total %>% filter(Sex == "Female") %>% select("Country","Year","Age","Population") ,by = c("Year","Country","Age")) %>% na.omit()
female_adc <- gather(adc_female,"Age","Aerodigestive",c("mid","old",`35-39`,`40-44`,`45-49`,`50-54`,`55-59`,`60-64`,`65-69`,`70-74`,`75-79`,`80+`)) %>% select(-`80-84`,-`85-89`,-`90-94`,-`95+`,-"Unknown") %>%
  left_join(.,pop_total %>% filter(Sex == "Female") %>% select("Country","Year","Age","Population") ,by = c("Year","Country","Age")) %>% na.omit()
female_lca <- gather(lca_female,"Age","LungCancer",c("mid","old",`35-39`,`40-44`,`45-49`,`50-54`,`55-59`,`60-64`,`65-69`,`70-74`,`75-79`,`80+`)) %>% select(-`80-84`,-`85-89`,-`90-94`,-`95+`,-"Unknown") %>%
  left_join(.,pop_total %>% filter(Sex == "Female") %>% select("Country","Year","Age","Population") ,by = c("Year","Country","Age")) %>% na.omit()
female_lci <- gather(lci_female,"Age","LiverCirrhosis",c("mid","old",`35-39`,`40-44`,`45-49`,`50-54`,`55-59`,`60-64`,`65-69`,`70-74`,`75-79`,`80+`)) %>% select(-`80-84`,-`85-89`,-`90-94`,-`95+`,-"Unknown") %>%
  left_join(.,pop_total %>% filter(Sex == "Female") %>% select("Country","Year","Age","Population") ,by = c("Year","Country","Age")) %>% na.omit()
female_vd <- gather(vd_female,"Age","vascular",c("mid","old",`35-39`,`40-44`,`45-49`,`50-54`,`55-59`,`60-64`,`65-69`,`70-74`,`75-79`,`80+`)) %>% select(-`80-84`,-`85-89`,-`90-94`,-`95+`,-"Unknown") %>%
  left_join(.,pop_total %>% filter(Sex == "Female") %>% select("Country","Year","Age","Population") ,by = c("Year","Country","Age")) %>% na.omit()
female_rd <- gather(rd_female,"Age","Respiratory",c("mid","old",`35-39`,`40-44`,`45-49`,`50-54`,`55-59`,`60-64`,`65-69`,`70-74`,`75-79`,`80+`)) %>% select(-`80-84`,-`85-89`,-`90-94`,-`95+`,-"Unknown") %>%
  left_join(.,pop_total %>% filter(Sex == "Female") %>% select("Country","Year","Age","Population") ,by = c("Year","Country","Age")) %>% na.omit()
female_copd <- gather(copd_female,"Age","COPD",c("mid","old",`35-39`,`40-44`,`45-49`,`50-54`,`55-59`,`60-64`,`65-69`,`70-74`,`75-79`,`80+`)) %>% select(-`80-84`,-`85-89`,-`90-94`,-`95+`,-"Unknown") %>%
  left_join(.,pop_total %>% filter(Sex == "Female") %>% select("Country","Year","Age","Population") ,by = c("Year","Country","Age")) %>% na.omit()
female_nmc <- gather(nmc_female,"Age","NonMedical",c("mid","old",`35-39`,`40-44`,`45-49`,`50-54`,`55-59`,`60-64`,`65-69`,`70-74`,`75-79`,`80+`)) %>% select(-`80-84`,-`85-89`,-`90-94`,-`95+`,-"Unknown") %>%
  left_join(.,pop_total %>% filter(Sex == "Female") %>% select("Country","Year","Age","Population") ,by = c("Year","Country","Age")) %>% na.omit()


# Merge Diseases Raw Mortality Datasets (Male)
male_total <- male_ac %>% left_join(.,male_adc,by=c("Country","Year","Age","Population")) %>% left_join(.,male_lca,by=c("Country","Year","Age","Population")) %>%
  mutate(OtherCancer=AllCancer-Aerodigestive-LungCancer) %>% left_join(.,male_copd,by=c("Country","Year","Age","Population")) %>%
  left_join(.,male_rd,by=c("Country","Year","Age","Population")) %>% left_join(.,male_vd,by=c("Country","Year","Age","Population")) %>%
  left_join(.,male_lci,by=c("Country","Year","Age","Population")) %>% left_join(.,male_nmc,by=c("Country","Year","Age","Population")) %>%
  left_join(.,male_mo,by=c("Country","Year","Age","Population")) %>% mutate(OtherMedical=Total-AllCancer-Respiratory-vascular-LiverCirrhosis-NonMedical) %>%
  mutate(OtherRespiratory=Respiratory-COPD)

male_total <- bind_rows(male_total %>% filter(Age %in% c('35-39','40-44','45-49','50-54','55-59')) %>% group_by(Year,Country) %>%
                          summarise(Population = sum(Population),Total = sum(Total),AllCancer = sum(AllCancer),LungCancer = sum(LungCancer),
                                    Aerodigestive = sum(Aerodigestive),OtherCancer = sum(OtherCancer),Respiratory = sum(Respiratory),COPD = sum(COPD),
                                    OtherRespiratory = sum(OtherRespiratory),vascular = sum(vascular), LiverCirrhosis = sum(LiverCirrhosis),
                                    OtherMedical = sum(OtherMedical), NonMedical = sum(NonMedical)) %>% mutate(Age = '35-59') %>% as.data.frame(),
                        male_total %>% filter(Age %in% c('60-64','65-69','70-74','75-79','80+')))

male_total_lc_rate <- male_total %>% select(Year,Country,Age,Population,LungCancer) %>% mutate(LungCancerRate = 100*LungCancer/Population)
male_total_lc_rate_80 <- male_total_lc_rate %>% filter(Age == "80+") %>% select(-LungCancerRate) %>%
  left_join(.,male_total_lc_rate %>% filter(Age == "75-79") %>% select(-Population,-LungCancer,-Age),by=c("Year","Country"))
male_total_lc_rate <- male_total_lc_rate %>% filter(Age != "80+") %>% rbind(male_total_lc_rate_80)

male_total <- male_total %>% left_join(.,male_total_lc_rate %>% select(-Population,-LungCancer),by = c("Year","Country","Age")) %>% left_join(.,male_total %>% select(Age) %>% unique() %>%
                                                                                                                                                mutate(NSm_CPSII = c(1900000/371806,1400000/121174,2200000/102124,2500000/71536,2100000/40363,2300000/26015),
                                                                                                                                                       Sm_CPSII = c(42700000/345074,38100000/105523,40000000/68771,34300000/37732,17000000/15201,6000000/5133),
                                                                                                                                                       NSm_Asians = c(7.99,28.79,53.06,86.98,115.6,115.6),
                                                                                                                                                       NSm_Appendix = c(7.3,14,20,27,35,35),
                                                                                                                                                       Sm_Appendix = c(135.8,375,599,899,1168,1168),
                                                                                                                                                       ER_OtherMedical=c(1.025,0.655,0.545,0.5,0.27,0.27)),by="Age")


# Merge Diseases Raw Mortality Datasets (Female)
female_total <- female_ac %>% left_join(.,female_adc,by=c("Country","Year","Age","Population")) %>% left_join(.,female_lca,by=c("Country","Year","Age","Population")) %>%
  mutate(OtherCancer=AllCancer-Aerodigestive-LungCancer) %>% left_join(.,female_copd,by=c("Country","Year","Age","Population")) %>%
  left_join(.,female_rd,by=c("Country","Year","Age","Population")) %>% left_join(.,female_vd,by=c("Country","Year","Age","Population")) %>%
  left_join(.,female_lci,by=c("Country","Year","Age","Population")) %>% left_join(.,female_nmc,by=c("Country","Year","Age","Population")) %>%
  left_join(.,female_mo,by=c("Country","Year","Age","Population")) %>% mutate(OtherMedical=Total-AllCancer-Respiratory-vascular-LiverCirrhosis-NonMedical) %>%
  mutate(OtherRespiratory=Respiratory-COPD)


female_total <- bind_rows(female_total %>% filter(Age %in% c('35-39','40-44','45-49','50-54','55-59')) %>% group_by(Year,Country) %>%
                            summarise(Population = sum(Population),Total = sum(Total),AllCancer = sum(AllCancer),LungCancer = sum(LungCancer),
                                      Aerodigestive = sum(Aerodigestive),OtherCancer = sum(OtherCancer),Respiratory = sum(Respiratory),COPD = sum(COPD),
                                      OtherRespiratory = sum(OtherRespiratory),vascular = sum(vascular), LiverCirrhosis = sum(LiverCirrhosis),
                                      OtherMedical = sum(OtherMedical), NonMedical = sum(NonMedical)) %>% mutate(Age = '35-59') %>% as.data.frame(),
                          female_total %>% filter(Age %in% c('60-64','65-69','70-74','75-79','80+')))

female_total_lc_rate <- female_total %>% select(Year,Country,Age,Population,LungCancer) %>% mutate(LungCancerRate = 100*LungCancer/Population)
female_total_lc_rate_80 <- female_total_lc_rate %>% filter(Age == "80+") %>% select(-LungCancerRate) %>%
  left_join(.,female_total_lc_rate %>% filter(Age == "75-79") %>% select(-Population,-LungCancer,-Age),by=c("Year","Country"))
female_total_lc_rate <- female_total_lc_rate %>% filter(Age != "80+") %>% rbind(female_total_lc_rate_80)


female_total <- female_total %>% left_join(.,female_total_lc_rate %>% select(-Population,-LungCancer),by = c("Year","Country","Age")) %>% left_join(.,female_total %>% select(Age) %>% unique() %>%
                                                                                                                                                      mutate(NSm_CPSII = c(1900000/371806,1400000/121174,2200000/102124,2500000/71536,2100000/40363,2300000/26015),
                                                                                                                                                             Sm_CPSII = c(42700000/345074,38100000/105523,40000000/68771,34300000/37732,17000000/15201,6000000/5133),
                                                                                                                                                             NSm_Asians = c(7.11,21.26,31.43,38.69,59.72,59.72),
                                                                                                                                                             NSm_Appendix = c(7,14,19,26,34,34),
                                                                                                                                                             Sm_Appendix = c(80.4,195,310,339,429,429),
                                                                                                                                                             ER_OtherMedical=c(0.845,0.844,0.76,0.5,0.22,0.22)),by="Age")


# Smoking Attributable Deaths Calculation (Male)
male_PetoLopez_AF <- male_total %>% mutate(SIR = (LungCancerRate-NSm_Appendix)/(Sm_Appendix-NSm_Appendix),
                                           SIR = ifelse(SIR < 0,0,ifelse(SIR>1,1,SIR)),
                                           SAF_LungCancer = (LungCancerRate - NSm_Appendix)*Population/100,
                                           SAF_COPD = (6.41*SIR/(1+6.41*SIR))*COPD,
                                           SAF_Aerodigestive = (3.435*SIR/(1+3.435*SIR))*Aerodigestive,
                                           SAF_OtherCancer = (0.345*SIR/(1+0.345*SIR))*OtherCancer,
                                           SAF_Vascular = (ER_OtherMedical*SIR/(1+ER_OtherMedical*SIR))*vascular,
                                           SAF_OtherRespiratory = (ER_OtherMedical*SIR/(1+ER_OtherMedical*SIR))*OtherRespiratory,
                                           SAF_OtherMedical = (ER_OtherMedical*SIR/(1+ER_OtherMedical*SIR))*OtherMedical,
                                           SAF = ifelse(SAF_LungCancer < 0, 0, ifelse(SAF_LungCancer > LungCancer, LungCancer, SAF_LungCancer)) + SAF_COPD + SAF_OtherRespiratory +
                                             SAF_Aerodigestive + SAF_OtherCancer +
                                             SAF_Vascular + SAF_OtherMedical)


male_PetoLopez_AF_age <- as.data.frame(male_PetoLopez_AF %>% select(Year,Country,Age,Total,SAF) %>% group_by(Year,Age,Country) %>% summarise(Total = sum(Total),SAF = sum(SAF)) %>% mutate(AF = SAF/Total))
male_PetoLopez_AF$Sex <- 'Male'

# Smoking Attributable Deaths Calculation (Female)
female_PetoLopez_AF <- female_total  %>% mutate(SIR = (LungCancerRate-NSm_Appendix)/(Sm_Appendix-NSm_Appendix),
                                                SIR = ifelse(SIR < 0,0,ifelse(SIR>1,1,SIR)),
                                                SAF_LungCancer = (LungCancerRate - NSm_Appendix)*Population/100,
                                                SAF_COPD = (6.605*SIR/(1+6.605*SIR))*COPD,
                                                SAF_Aerodigestive = (2.975*SIR/(1+2.975*SIR))*Aerodigestive,
                                                SAF_OtherCancer = (0.1*SIR/(1+0.1*SIR))*OtherCancer,
                                                SAF_Vascular = (ER_OtherMedical*SIR/(1+ER_OtherMedical*SIR))*vascular,
                                                SAF_OtherRespiratory = (ER_OtherMedical*SIR/(1+ER_OtherMedical*SIR))*OtherRespiratory,
                                                SAF_OtherMedical = (ER_OtherMedical*SIR/(1+ER_OtherMedical*SIR))*OtherMedical,
                                                SAF = ifelse(SAF_LungCancer < 0, 0, ifelse(SAF_LungCancer > LungCancer, LungCancer, SAF_LungCancer)) + SAF_COPD + SAF_OtherRespiratory +
                                                  SAF_Aerodigestive + SAF_OtherCancer +
                                                  SAF_Vascular + SAF_OtherMedical)

female_PetoLopez_AF <- female_PetoLopez_AF %>% arrange(Country, Year, Age)
female_PetoLopez_AF_age <- as.data.frame(female_PetoLopez_AF %>% select(Year,Country,Age,Total,SAF) %>% group_by(Age,Year,Country) %>% summarise(Total = sum(Total),SAF = sum(SAF)) %>% mutate(AF = SAF/Total))
female_PetoLopez_AF$Sex <- 'Female'


# Smoking AF by age
AF_age <- rbind(male_PetoLopez_AF_age  %>% select(Age,Year,Country,Total,SAF,AF) %>% mutate(Sex = "Male"),
                female_PetoLopez_AF_age %>%  select(Age,Year,Country,Total,SAF,AF) %>% mutate(Sex = "Female"),
                HKmale_PetoLopez_AF_age  %>% select(Age,Year,Country,Total,SAF,AF) %>% mutate(Sex = "Male"),
                HKfemale_PetoLopez_AF_age  %>% select(Age,Year,Country,Total,SAF,AF) %>% mutate(Sex = "Female"))

country <- (AF_age$Country %>% unique())

year = 1961:2016
sex = c("Male","Female")
AF_age_dummy <- data.frame(Country = rep(country,each = 111*56*2), Age = rep(0:110,56*2*20), Sex = rep(rep(sex,each = 111*56),20), Year = rep(rep(rep(1961:2016,each = 111),2),20))
AF_age_dummy <- AF_age_dummy %>% mutate(Age1 = Age, Age = cut(Age1,breaks = c(-1,34,59,64,69,74,79,111)), Age =  forcats::fct_recode(factor(Age),
                                                                                                                                     "0-34" = "(-1,34]",
                                                                                                                                     "35-59" = "(34,59]",
                                                                                                                                     "60-64" = "(59,64]",
                                                                                                                                     "65-69" = "(64,69]",
                                                                                                                                     "70-74" = "(69,74]",
                                                                                                                                     "75-79" = "(74,79]",
                                                                                                                                     "80+" = "(79,111]"))
AF_age_dummy <- AF_age_dummy %>% left_join(.,AF_age,by = c("Country", "Age","Sex","Year"))
AF_age_dummy$AF <- ifelse(AF_age_dummy$Age1<35,0,AF_age_dummy$AF)
