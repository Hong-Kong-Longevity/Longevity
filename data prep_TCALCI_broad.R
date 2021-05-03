# Preparion for datasets used in the example of applying the function of confidence interval calculation for cause-specific TCAL difference in "TCAL functions.R".

library(dplyr)
library(data.table)

## WHO Mortality Raw Data
icd9 <- fread("Life Expectancy/Morticd9")
icd10_part1 <- fread("Life Expectancy/Morticd10_part1")
icd10_part2 <- fread("Life Expectancy/Morticd10_part2b")
icd_total <- rbind(icd9,icd10_part1,icd10_part2) %>% filter(Year>1970) %>%
  filter(Country %in% c("5020","4010","4020","2090","4050","4070","4080","4085","4180","3160","4210","4220","4240","4280","4290","4300","4308","2450"))
total_data <- icd_total %>% mutate(Country = forcats::fct_recode(factor(Country),"Australia"="5020",
                                                                 "Austria"="4010",
                                                                 "Belgium"="4020",
                                                                 "Canada"="2090",
                                                                 "Denmark"="4050",
                                                                 "Finland"="4070",
                                                                 "France"="4080",
                                                                 "Germany"="4085",
                                                                 "Italy"='4180',
                                                                 "Japan"="3160",
                                                                 "Netherlands"="4210",
                                                                 "Norway"="4220",
                                                                 "Portugal"="4240",
                                                                 "Spain"="4280",
                                                                 "Sweden"="4290",
                                                                 "Switzerland"="4300",
                                                                 "UK"="4308",
                                                                 "USA"="2450")) %>% filter(Frmat != 9) %>% filter(Sex != 9)

colnames(total_data) <- c(colnames(total_data)[1:9],"Total","0","1","2","3","4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown",colnames(total_data)[36:39])


total_data_icd9 <- total_data %>% mutate(nchar = nchar(Cause)) %>% mutate(Cause1=substring(Cause,1,1)) %>% filter(List == "09B" & nchar == 3 & Cause1 =="B") %>%
  mutate(icd =  # ifelse(Cause %in% c("B08","B09",paste0("B1",0:4)),"cancer",
                    ifelse(Cause %in% paste("B",c(paste("0",8:9,sep=""),10:17),sep=""),"neoplasm",  
                       ifelse(Cause %in% c(paste0("B",25:30)),"cardiovascular",
                              ifelse(Cause %in% c("B31","B32"),"respiratory",
                                     ifelse(Cause %in% c(paste0("B",47:56)),"external",
                                            ifelse(Cause == "B00","Total","others")))))) 

total_data_icd10 <- total_data %>% filter(List %in% c("103","104","10M")) %>% mutate(icd =  ifelse(Cause == "AAA","Total",
                                                                                                 #  ifelse(substr(Cause,1,1) %in% c("C"),"cancer",
                                                                                                      ifelse(substr(Cause,1,1) %in% c("C") | substr(Cause,1,3) %in% paste("D",c(paste("0",0:9,sep=""),10:48),sep=""), "neoplasm",
                                                                                                          ifelse(substr(Cause,1,1) %in% c("I"),"cardiovascular",
                                                                                                                 ifelse(substr(Cause,1,1) %in% c("J"),"respiratory",
                                                                                                                        ifelse(substr(Cause,1,1) %in% c("V","W","X","Y"),"external","others"))))))

total_data_icd <- rbind(total_data_icd9 %>% select(-c(nchar,Cause1)),total_data_icd10) %>% select(-Admin1,-SubDiv,-IM_Frmat,-Unknown,-IM_Deaths1,-IM_Deaths2,-IM_Deaths3,-IM_Deaths4)

total_data_icd[total_data_icd$Country=="France" & total_data_icd$Year==2016,]$Frmat<-2

# COD of interest
total_data_icd_long <- total_data_icd %>% tidyr::gather(.,Age,Deaths,`0`:`95+`,factor_key = T)
total_deaths_icd <- total_data_icd_long %>% filter(icd == "Total") %>% rename("Deaths_total" = "Deaths") %>% select(-Cause,-icd,-List,-Frmat)
total_data_icd_long <- total_data_icd_long %>% group_by(Country,Year,List,Sex,Frmat,icd,Age) %>% summarize(Deaths = sum(Deaths))
total_data_icd_long <- total_data_icd_long %>% left_join(.,total_deaths_icd,by = c("Country","Year","Sex","Age")) %>% data.frame()


# Read exposure data for each country | repeat process for all countries

belgium_exposure <- fread("Life Expectancy/Exposures_1x1_BEL.txt") %>% data.frame()
australia_exposure <- fread("Life Expectancy/Exposures_1x1_AUS.txt") %>% data.frame()
austria_exposure <- fread("Life Expectancy/Exposures_1x1_AUT.txt") %>% data.frame()
canada_exposure <- fread("Life Expectancy/Exposures_1x1_CAN.txt") %>% data.frame()
denmark_exposure <- fread("Life Expectancy/Exposures_1x1_DEN.txt") %>% data.frame()
finland_exposure <- fread("Life Expectancy/Exposures_1x1_FIN.txt") %>% data.frame()
france_exposure <- fread("Life Expectancy/Exposures_1x1_FRA.txt") %>% data.frame()
germany_exposure <- fread("Life Expectancy/Exposures_1x1_GER.txt") %>% data.frame()
italy_exposure <- fread("Life Expectancy/Exposures_1x1_ITA.txt") %>% data.frame()
japan_exposure <- fread("Life Expectancy/Exposures_1x1_JPN.txt") %>% data.frame()
netherlands_exposure <- fread("Life Expectancy/Exposures_1x1_NET.txt") %>% data.frame()
norway_exposure <- fread("Life Expectancy/Exposures_1x1_NOR.txt") %>% data.frame()
portugal_exposure <- fread("Life Expectancy/Exposures_1x1_POR.txt") %>% data.frame()
spain_exposure <- fread("Life Expectancy/Exposures_1x1_SPA.txt") %>% data.frame()
sweden_exposure <- fread("Life Expectancy/Exposures_1x1_SWE.txt") %>% data.frame()
switzerland_exposure <- fread("Life Expectancy/Exposures_1x1_SWI.txt") %>% data.frame()
uk_exposure <- fread("Life Expectancy/Exposures_1x1_UK.txt") %>% data.frame()
usa_exposure <- fread("Life Expectancy/Exposures_1x1_USA.txt") %>% data.frame()
## Exposure numbers 
total_exposure <- rbind(cbind(belgium_exposure %>% filter(Year>1970) %>% mutate(Male=as.numeric(Male)+0,Female=as.numeric(Female)+0),Country="Belgium"),
                        cbind(australia_exposure %>% filter(Year>1970),Country="Australia"),
                        cbind(austria_exposure %>% filter(Year>1970),Country="Austria"),
                        cbind(canada_exposure %>% filter(Year>1970),Country="Canada"),
                        cbind(denmark_exposure %>% filter(Year>1970),Country="Denmark"),
                        cbind(finland_exposure %>% filter(Year>1970),Country="Finland"),
                        cbind(france_exposure %>% filter(Year>1970),Country="France"),
                        cbind(germany_exposure %>% filter(Year>1970),Country="Germany"),
                        cbind(italy_exposure %>% filter(Year>1970),Country="Italy"),
                        cbind(japan_exposure %>% filter(Year>1970),Country="Japan"),
                        cbind(netherlands_exposure %>% filter(Year>1970),Country="Netherlands"),
                        cbind(norway_exposure %>% filter(Year>1970),Country="Norway"),
                        cbind(portugal_exposure %>% filter(Year>1970),Country="Portugal"),
                        cbind(spain_exposure %>% filter(Year>1970),Country="Spain"),
                        cbind(sweden_exposure %>% filter(Year>1970),Country="Sweden"),
                        cbind(switzerland_exposure %>% filter(Year>1970),Country="Switzerland"),
                        cbind(uk_exposure %>% filter(Year>1970),Country="UK"),
                        cbind(usa_exposure %>% filter(Year>1970),Country="USA")
)


total_exposure$Age <- ifelse(total_exposure$Age == "110+",110,total_exposure$Age)
total_exposure$Age <- as.numeric(as.character(total_exposure$Age))
total_exposure$Female <- as.numeric(total_exposure$Female)
total_exposure$Male <- as.numeric(total_exposure$Male)
# 95+
total_exposure_male95 <- total_exposure %>% select(-Total,-Female) %>% group_by(Year,Country) %>% filter(Age > 94) %>% summarize(sum(Male)) %>% data.frame()
total_exposure_female95 <- total_exposure %>% select(-Total,-Male) %>% group_by(Year,Country) %>% filter(Age > 94) %>% summarize(sum(Female)) %>% data.frame()
total_exposure95 <- total_exposure_male95 %>% left_join(.,total_exposure_female95,by=c("Country",'Year'))
total_exposure95$Agegroup <- '95+'
colnames(total_exposure95) <- c("Year","Country","Male","Female","Agegroup")
# 85+
total_exposure_male85 <- total_exposure %>% select(-Total,-Female) %>% group_by(Year,Country) %>% filter(Age > 84) %>% summarize(sum(Male)) %>% data.frame()
total_exposure_female85 <- total_exposure %>% select(-Total,-Male) %>% group_by(Year,Country) %>% filter(Age > 84) %>% summarize(sum(Female)) %>% data.frame()
total_exposure85 <- total_exposure_male85 %>% left_join(.,total_exposure_female85,by=c("Country",'Year'))
total_exposure85$Agegroup <- '85+'
colnames(total_exposure85) <- c("Year","Country","Male","Female","Agegroup")
# 1 to 4
total_exposure_male1to4 <- total_exposure %>% select(-Total,-Female) %>% group_by(Year,Country) %>% filter(Age %in% c(1:4)) %>% summarize(sum(Male)) %>% data.frame()
total_exposure_female1to4 <- total_exposure %>% select(-Total,-Male) %>% group_by(Year,Country) %>% filter(Age %in% c(1:4)) %>% summarize(sum(Female)) %>% data.frame()
total_exposure1to4 <- total_exposure_male1to4 %>% left_join(.,total_exposure_female1to4,by=c("Country",'Year'))
total_exposure1to4$Agegroup <- '1-4'
colnames(total_exposure1to4) <- c("Year","Country","Male","Female","Agegroup")

total_exposure <- total_exposure %>% mutate(Agegroup = cut(Age, c(0:4,seq(5,95,5),111), right = FALSE)) %>% mutate(Agegroup = forcats::fct_recode(factor(Agegroup),
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
total_exposure <- total_exposure %>% group_by(Year,Country,Agegroup) %>% summarize(Male = sum(Male),Female = sum(Female)) %>% data.frame()
total_exposure_data <- total_exposure %>% rbind(total_exposure1to4,total_exposure85)

# Filter by Frmat
total_data_icd_long0 <- total_data_icd_long %>% filter(Frmat == 0 & Sex != 9) %>% rename("Agegroup" = "Age")
total_data_icd_long1 <- total_data_icd_long %>% filter(Frmat == 1 & Sex != 9) %>% rename("Agegroup" = "Age")  
total_data_icd_long2 <- total_data_icd_long %>% filter(Frmat == 2 & Sex != 9) %>% rename("Agegroup" = "Age") %>% filter(!Agegroup %in% c("2","3","4","90-94","95+")) 



# Merge exposure data - Use all data a/v
total_data_icd_long0$Country<-as.character(total_data_icd_long0$Country)
total_data_icd_long0$Agegroup<-as.character(total_data_icd_long0$Agegroup)
total_exposure$Agegroup<-as.character(total_exposure$Agegroup)
total_exposure_data$Agegroup<-as.character(total_exposure_data$Agegroup)
total_data_icd_long0 <- total_data_icd_long0 %>% left_join(.,total_exposure %>% tidyr::gather(.,Sex,Exposure,Female:Male,factor_key = T) %>% mutate(Sex = ifelse(Sex == "Male",1,2)),by = c("Year","Country","Sex","Agegroup"),na_matches = "never")

total_data_icd_long1$Country<-as.character(total_data_icd_long1$Country)
total_data_icd_long1$Agegroup <- as.character(total_data_icd_long1$Agegroup)
total_data_icd_long1[total_data_icd_long1$Agegroup=="85-89",]$Agegroup <- "85+"
total_data_icd_long1 <- total_data_icd_long1 %>% filter(!Agegroup %in% c("90-94","95+")) %>% left_join(.,total_exposure_data %>% tidyr::gather(.,Sex,Exposure,Female:Male,factor_key = T) %>% mutate(Sex = ifelse(Sex == "Male",1,2)),by = c("Year","Country","Sex","Agegroup"),na_matches = "never")
total_data_icd_long1_add <- total_data_icd_long1 %>% filter(Agegroup %in% c(1,2)) %>% mutate(Agegroup = ifelse(Agegroup == 1,"90-94","95+")) %>% select(Country,Year,Sex,icd,Agegroup) %>% left_join(.,total_data_icd_long1 %>% filter(Agegroup == "85+") %>% select(-Agegroup),by = c('Country','Year','Sex','icd'))
total_data_icd_long1[total_data_icd_long1$Agegroup=="85+",]$Agegroup <- "85-89"
total_data_icd_long1 <- rbind(total_data_icd_long1,total_data_icd_long1_add)
total_data_icd_long1 <- total_data_icd_long1 %>% arrange(Country, Year, Sex, Agegroup,icd)

total_data_icd_long2$Country<-as.character(total_data_icd_long2$Country)
total_data_icd_long2$Agegroup <- as.character(total_data_icd_long2$Agegroup)
total_data_icd_long2[total_data_icd_long2$Agegroup=="1",]$Agegroup <- "1-4"
total_data_icd_long2[total_data_icd_long2$Agegroup=="85-89",]$Agegroup <- "85+"
total_data_icd_long2 <- total_data_icd_long2 %>% left_join(.,total_exposure_data %>% tidyr::gather(.,Sex,Exposure,Female:Male,factor_key = T) %>% mutate(Sex = ifelse(Sex == "Male",1,2)),by = c("Year","Country","Sex","Agegroup"),na_matches = "never")
total_data_icd_long2_add_2to4 <- total_data_icd_long2 %>% filter(Agegroup %in% c("1-4","5-9","10-14")) %>% mutate(Agegroup = ifelse(Agegroup == "1-4","2",ifelse(Agegroup == "5-9","3","4"))) %>% select(Country,Year,Sex,icd,Agegroup) %>% left_join(.,total_data_icd_long2 %>% filter(Agegroup == "1-4") %>% select(-Agegroup),by = c('Country','Year','Sex','icd'))
total_data_icd_long2_add_90 <- total_data_icd_long2 %>% filter(Agegroup %in% c("1-4","5-9")) %>% mutate(Agegroup = ifelse(Agegroup == "1-4","90-94","95+")) %>% select(Country,Year,Sex,icd,Agegroup) %>% left_join(.,total_data_icd_long2 %>% filter(Agegroup == "85+") %>% select(-Agegroup),by = c('Country','Year','Sex','icd'))
total_data_icd_long2[total_data_icd_long2$Agegroup=="1-4",]$Agegroup <- "1"
total_data_icd_long2[total_data_icd_long2$Agegroup=="85+",]$Agegroup <- "85-89"
total_data_icd_long2 <- rbind(total_data_icd_long2,total_data_icd_long2_add_2to4,total_data_icd_long2_add_90)
total_data_icd_long2 <- total_data_icd_long2 %>% arrange(Country, Year, Sex, Agegroup,icd)

icd_data <- rbind(total_data_icd_long0,total_data_icd_long1,total_data_icd_long2) %>% filter(Agegroup != "Total") %>% na.omit() %>% filter(icd != "Total")
write.table(icd_data,"Life Expectancy/out for broad bootstrap.csv",sep=",",row.names = F)

## Hong Kong mortality data
DeathData7618$n<-1 ## HK cause-of-death data generated by "hk death data.R" using data from HK Census and Statistics Department
hkdccod<-DeathData7618
sum(is.na(hkdccod$cod))
hkdccod$Year<-as.numeric(hkdccod$Year)
hkdccod$Sex<-as.numeric(hkdccod$Sex)
hkdccod<-hkdccod %>% filter((hkdccod$Sex %in% 1:2) & !is.na(hkdccod$Age))
hkdccod9<-hkdccod[hkdccod$ICD==9,]
hkdccod9<-hkdccod9[!is.na(hkdccod9$cod),]
hkdccod9$Cause1<-substr(hkdccod9$cod,1,1)
hkdccod9$CauseB<-substr(hkdccod9$cod,1,3)
hkdccod9$CauseB<-as.numeric(hkdccod9$CauseB)
hkdccod9<-hkdccod9[!(hkdccod9$CauseB>=800 & hkdccod9$Cause1!="E"),]
hkdccod9$icd<-ifelse(hkdccod9$Cause1=="E", "external",
                    # ifelse(hkdccod9$CauseB>=140 & hkdccod9$CauseB<210, "cancer",
                         ifelse(hkdccod9$CauseB>=140 & hkdccod9$CauseB<240, "neoplasm",
                            ifelse(hkdccod9$CauseB>=390 & hkdccod9$CauseB<460, "cardiovascular",
                                   ifelse( hkdccod9$CauseB>=460 & hkdccod9$CauseB<520, "respiratory",
                                           "others"))))



hkdccod9<-subset(hkdccod9,select=c("Year", "Age", "Sex", "icd", "n"))

hkdccod10<-hkdccod[hkdccod$ICD==10,]
hkdccod10$CauseA<-gsub("[[:digit:]]", "", hkdccod10$cod)
unique(hkdccod10$CauseA)
hkdccod10$CauseB<-gsub("[^[:digit:]]", "", hkdccod10$cod)
hkdccod10$CauseA<-gsub("[.]", "", hkdccod10$CauseA)
hkdccod10$CauseA<-as.factor(hkdccod10$CauseA)
levels(hkdccod10$CauseA)
hkdccod10$icd <-  # ifelse(hkdccod10$CauseA=="C", "cancer",
                       ifelse(hkdccod10$CauseA=="C" | substr(hkdccod10$cod,1,3) %in% paste("D",c(paste("0",0:9,sep=""),10:48),sep=""), "neoplasm",
                          ifelse(hkdccod10$CauseA=="I", "cardiovascular",
                                 ifelse(hkdccod10$CauseA=="J", "respiratory",
                                        ifelse(hkdccod10$CauseA=="V" | hkdccod10$CauseA=="W" | hkdccod10$CauseA=="X" | hkdccod10$CauseA=="Y", "external",
                                               "others"))))

hkdccod10<-subset(hkdccod10,select=c("Year", "Age", "Sex", "icd", "n"))
hkdccod910<-rbind(hkdccod9,hkdccod10) 
hkdccod910$Sex<-as.factor(hkdccod910$Sex)
levels(hkdccod910$Sex)[1]<-"Male"
levels(hkdccod910$Sex)[2]<-"Female"
hkdccod910<-aggregate(n ~Year+Age+Sex+icd, data=hkdccod910, FUN = sum) 
hkdccod_expand<-hkdccod910%>%expand(Year,Age,Sex,icd)
hkdccod_expand$icd<-as.character(hkdccod_expand$icd)
hkdccod_merge<-left_join(hkdccod_expand,hkdccod910,by=c("Year","Sex","Age","icd")) 
hkdccod_merge$n[is.na(hkdccod_merge$n)]<-0
hkdccod_merge<-hkdccod_merge %>% arrange(Year, Age, Sex, icd) %>% filter(Year>1978)
hkdccod_merge<- hkdccod_merge %>%
  arrange(icd,Sex,Year, Age) %>%
  group_by(Year,Age,Sex) %>%
  mutate(nn=sum(n))
hkdccod_merge$cause<- (hkdccod_merge$n)/(hkdccod_merge$nn)
hkdccod_merge$freq<-ifelse(hkdccod_merge$Age==99,12,1)
hkdccod_merge<-hkdccod_merge %>% arrange(icd,Sex,Year,Age)
hkdccod_merge<-hkdccod_merge[rep(seq(nrow(hkdccod_merge)),hkdccod_merge$freq),]
hkdccod_merge<-hkdccod_merge %>%
  group_by(icd,Year,Sex) %>%
  mutate(age = sequence( n())) 
hkdccod_merge$Age<-hkdccod_merge$age-1
hkdccod_merge<-hkdccod_merge[hkdccod_merge$Year>=1979 & hkdccod_merge$Year<=2016,]
write.csv(hkdccod_merge,"Life Expectancy/hkdccod_merge_broad_new.csv",row.names = F)

