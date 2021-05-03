library(dplyr)
library(MortalityLaws)
library(data.table)
library(cowplot)

# Import HK Life tables
lt_HK <- data.table(read.csv("../Life expectancy/HK_life_table.csv"))
names(lt_HK)[names(lt_HK)=="year"] <- "Year"
lt_HK[Age=="100+",Age:="100"]
lt_HK[,Age:=as.numeric(as.character(Age))]
'--------------------'
'Made adjustments'
# Create mx from life table
lt_HK[qx!=1,mx:=2*qx/(2-qx)]
lt_HK[qx==1,mx:=lx/Lx]
lt_HK <- lt_HK %>% mutate(lx = lx*.1, dx = dx*.1, Lx = Lx*.1, Tx = Tx*.1, Country = "Hong Kong")
'--------------------'

# Import life tables (HICs)
CountryA <- 'Japan'
Country<-paste('../Life expectancy/',CountryA,sep="")
setwd(Country)
lt_japan_male <- fread("mltper_1x1.txt") %>% data.frame() %>% mutate(Country = "Japan")
lt_japan_female <- fread("fltper_1x1.txt")  %>% data.frame() %>% mutate(Country = "Japan")

# HIC's aggregated qx
hic_female <- read.table("../Life expectancy/hic_female_lt.csv",h=T,sep = ",")
hic_male <- read.table("../Life expectancy/hic_male_lt.csv",h=T,sep = ",")

trunc_lt <- function(data){
  rbind(
  (LifeTable(0:100,qx = c(data[data$Year == 1979 & data$Age %in% c(0:99),]$qx,1)))$lt %>% mutate(Year = 1979),
  (LifeTable(0:100,qx = c(data[data$Year == 2016 & data$Age %in% c(0:99),]$qx,1)))$lt %>% mutate(Year = 2016)
  ) %>% rename("Age" = "x") %>% select(-"x.int",-"ax")
}

japan_female_trunc <- trunc_lt(lt_japan_female) %>% mutate(sex = "Female", Country = "Japan")
hic_female_trunc <- trunc_lt(hic_female) %>% mutate(sex = "Female", Country = "HICs")
hic_male_trunc <- trunc_lt(hic_male) %>% mutate(sex = "Male", Country = "HICs")

lt_merged <- rbind(lt_HK,japan_female_trunc,hic_female_trunc,hic_male_trunc)

# Age decomposition
age_decomp <- function(country1,country2,gender,year)
{
  l0 <- 100000
  lx1 <- lt_merged[lt_merged$Year==year & lt_merged$Country==country1 & lt_merged$sex==gender,]$lx
  lx2 <- lt_merged[lt_merged$Year==year & lt_merged$Country==country2 & lt_merged$sex==gender,]$lx
  Lx1 <- lt_merged[lt_merged$Year==year & lt_merged$Country==country1 & lt_merged$sex==gender,]$Lx
  Lx2 <- lt_merged[lt_merged$Year==year & lt_merged$Country==country2 & lt_merged$sex==gender,]$Lx
  Tx1 <- lt_merged[lt_merged$Year==year & lt_merged$Country==country1 & lt_merged$sex==gender,]$Tx
  Tx2 <- lt_merged[lt_merged$Year==year & lt_merged$Country==country2 & lt_merged$sex==gender,]$Tx
  mx1 <- lt_merged[lt_merged$Year==year & lt_merged$Country==country1 & lt_merged$sex==gender,]$mx
  mx2 <- lt_merged[lt_merged$Year==year & lt_merged$Country==country2 & lt_merged$sex==gender,]$mx
  
  # decomposing by Age
  age_contri <- NULL
  for(i in 1:100){
    age_contri[i] <- lx1[i]/l0 * (Lx2[i]/lx2[i] - Lx1[i]/lx1[i]) + (Tx2[i+1]/l0) * (lx1[i]/lx2[i] - lx1[i+1]/lx2[i+1])
    age_contri[101] <- lx1[101]/l0 * (Tx2[101]/lx2[101]-Tx1[101]/lx1[101])
  }
  arriaga_data <- cbind(Age=0:100,data.frame(age_contri))
  arriaga_data <- arriaga_data %>% mutate(Agegroup = cut(Age, c(0,1,seq(5,105,5)), right = FALSE)) %>% mutate(Agegroup = forcats::fct_recode(factor(Agegroup),
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
  ))
  arriaga_data
}

# By cause of death
## WHO Mortality Raw Data
icd7 <- fread("../Life expectancy/Morticd7")
icd8 <- fread("../Life expectancy/Morticd8")
icd9 <- fread("../Life expectancy/Morticd9")
icd10_part1 <- fread("../Life expectancy/Morticd10_part1")
icd10_part2 <- fread("../Life expectancy/Morticd10_part2")
icd_total <- rbind(icd7,icd8,icd9,icd10_part1,icd10_part2) %>% filter(Year>1949) %>%
  select(-Admin1,-SubDiv,-IM_Frmat,-IM_Deaths1,-IM_Deaths2,-IM_Deaths3,-IM_Deaths4) %>%  
  filter(Country %in% c("5020","4010","4020","2090","4050","4070","4080","4085","4180","3160","4210","4220","4240","4280","4290","4300","4308","2450","3090"))
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
                                                                 "USA"="2450",
                                                                 "Hong Kong"="3090")) %>% filter(Frmat<3)

colnames(total_data) <- c(colnames(total_data)[1:6],"Total",0:4,"5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+","Unknown")
total_data_0 <- total_data %>% filter(Frmat == 0)
bind1 <- total_data %>% filter(Frmat == 1) %>% select("85-89")
total_data_1 <- total_data %>% filter(Frmat == 1) %>% mutate("85+" = bind1$`85-89`)
bind2a <- total_data %>% filter(Frmat == 2) %>% select("1") 
bind2b <- total_data %>% filter(Frmat == 2) %>% select("85-89")
total_data_2 <- total_data %>% filter(Frmat == 2) %>% mutate("1-4" = bind2a$`1`, "85+" = bind2b$`85-89`)
total_data_0 <- total_data_0 %>% select("1","2","3","4") %>% mutate("1-4" = rowSums(.,na.rm = T)) %>% select("1-4") %>% cbind(total_data_0,.)
total_data_0 <- total_data_0 %>% select("85-89","90-94","95+") %>% mutate("85+" = rowSums(.,na.rm = T)) %>% select("85+") %>% cbind(total_data_0,.)
total_data_1 <- total_data_1 %>% select("1","2","3","4") %>% mutate("1-4" = rowSums(.,na.rm = T)) %>% select("1-4") %>% cbind(total_data_1,.)
total_data_merged <- rbind(total_data_0 %>% select(c(names(total_data)[c(1:8,13:28)],"1-4","85+")),
                           total_data_1 %>% select(c(names(total_data)[c(1:8,13:28)],"1-4","85+")),
                           total_data_2 %>% select(c(names(total_data)[c(1:8,13:28)],"1-4","85+"))) %>% filter(Year>1978 & List != '08A')

total_mo <- total_data_merged %>% filter((List == 104 & Cause == "AAA")|
                                          (List == 103 & Cause == "AAA")|
                                           (List == "10M" & Cause == "AAA")|
                                           (List == '09B' & Cause == "B00")
                                           ) %>% filter(Sex != 9) %>% select(-c("List","Cause","Frmat")) %>% group_by(Country,Year,Sex) %>% summarise_each(funs(sum)) %>% mutate(COD = "Total")

total_respiratory <- total_data_merged %>% filter((List == 104 & substr(Cause,1,1) == "J")|
                                             (List == 103 & substr(Cause,1,1) == "J")|
                                             (List == "10M" & substr(Cause,1,1) == "J")|
                                             (List == '09B' & Cause  %in% c("B31","B32"))
                                             ) %>% filter(Sex != 9) %>% select(-c("List","Cause","Frmat")) %>% group_by(Country,Year,Sex) %>% summarise_each(funs(sum)) %>% mutate(COD = "Respiratory diseases")

total_cvd <- total_data_merged %>% filter((List == 104 & substr(Cause,1,1) == "I")|
                                                    (List == 103 & substr(Cause,1,1) == "I")|
                                                    (List == "10M" & substr(Cause,1,1) == "I")|
                                                    (List == '09B' & Cause  %in% c(paste('B',c(25:30),sep='')))
                                                    ) %>% filter(Sex != 9) %>% select(-c("List","Cause","Frmat")) %>% group_by(Country,Year,Sex) %>% summarise_each(funs(sum)) %>% mutate(COD = "Cardiovascular diseases")

total_neoplasms <- total_data_merged %>% filter((List == 104 & substr(Cause,1,1) %in% c("C",paste0("D",c(paste("0",0:9,sep=""),10:48))))|
                                            (List == 103 & substr(Cause,1,1) %in% c("C",paste0("D",c(paste("0",0:9,sep=""),10:48))))|
                                            (List == "10M" & substr(Cause,1,1) %in% c("C",paste0("D",c(paste("0",0:9,sep=""),10:48))))|
                                            (List == '09B' & Cause  %in% c("B08","B09",paste('B',c(10:17),sep='')))
                                            ) %>% filter(Sex != 9) %>% select(-c("List","Cause","Frmat")) %>% group_by(Country,Year,Sex) %>% summarise_each(funs(sum)) %>% mutate(COD = "Neoplasms")

total_external <- total_data_merged %>% filter((List == 104 & substr(Cause,1,1) %in% c("V","W","X","Y"))|
                                                  (List == 103 & substr(Cause,1,1) %in% c("V","W","X","Y"))|
                                                  (List == "10M" & substr(Cause,1,1) %in% c("V","W","X","Y"))|
                                                  (List == '09B' & Cause  %in% c(paste('B',c(47:56),sep='')))
                                                  ) %>% filter(Sex != 9) %>% select(-c("List","Cause","Frmat")) %>% group_by(Country,Year,Sex) %>% summarise_each(funs(sum)) %>% mutate(COD = "External causes")

total_digestive <- total_data_merged %>% filter((List == 104 & substr(Cause,1,1) %in% c("K"))|
                                                 (List == 103 & substr(Cause,1,1) %in% c("K"))|
                                                 (List == "10M" & substr(Cause,1,1) %in% c("K"))|
                                                 (List == '09B' & Cause  %in% c("B33","B34"))
                                                ) %>% filter(Sex != 9) %>% select(-c("List","Cause","Frmat")) %>% group_by(Country,Year,Sex) %>% summarise_each(funs(sum)) %>% mutate(COD = "Digestive diseases")

total_infectious <- total_data_merged %>% filter((List == 104 & Cause != "AAA" & substr(Cause,1,1) %in% c("A","B"))|
                                                  (List == 103 & Cause != "AAA" & substr(Cause,1,1) %in% c("A","B"))|
                                                  (List == "10M" & Cause != "AAA" & substr(Cause,1,1) %in% c("A","B"))|
                                                  (List == '09B' & Cause  %in% c(paste('B0',c(1:7),sep='')))
                                                  ) %>% filter(Sex != 9) %>% select(-c("List","Cause","Frmat")) %>% group_by(Country,Year,Sex) %>% summarise_each(funs(sum)) %>% mutate(COD = "Infectious diseases")

total_perinatal <- total_data_merged %>% filter((List == 104 & substr(Cause,1,1) %in% c("P"))|
                                                   (List == 103 & substr(Cause,1,1) %in% c("P"))|
                                                   (List == "10M" & substr(Cause,1,1) %in% c("P"))|
                                                   (List == '09B' & Cause  %in% c("B45"))
                                                   ) %>% filter(Sex != 9) %>% select(-c("List","Cause","Frmat")) %>% group_by(Country,Year,Sex) %>% summarise_each(funs(sum)) %>% mutate(COD = "Perinatal deaths")

total_ill_defined <- total_data_merged %>% filter((List == 104 & substr(Cause,1,1) %in% c("R"))|
                                                  (List == 103 & substr(Cause,1,1) %in% c("R"))|
                                                  (List == "10M" & substr(Cause,1,1) %in% c("R"))|
                                                  (List == '09B' & Cause  %in% c("B46"))
                                                  ) %>% filter(Sex != 9) %>% select(-c("List","Cause","Frmat")) %>% group_by(Country,Year,Sex) %>% summarise_each(funs(sum)) %>% mutate(COD = "Ill-defined causes")

total_congenital <- total_data_merged %>% filter((List == 104 & substr(Cause,1,1) %in% c("Q"))|
                                                    (List == 103 & substr(Cause,1,1) %in% c("Q"))|
                                                    (List == "10M" & substr(Cause,1,1) %in% c("Q"))|
                                                    (List == '09B' & Cause  %in% c("B44"))
                                                    ) %>% filter(Sex != 9) %>% select(-c("List","Cause","Frmat")) %>% group_by(Country,Year,Sex) %>% summarise_each(funs(sum)) %>% mutate(COD = "Congenital anomalies")

total_metabolic <- total_data_merged %>% filter((List == 104 & substr(Cause,1,1) %in% c("E"))|
                                                   (List == 103 & substr(Cause,1,1) %in% c("E"))|
                                                   (List == "10M" & substr(Cause,1,1) %in% c("E"))|
                                                   (List == '09B' & Cause  %in% c("B18","B19"))
                                                 ) %>% filter(Sex != 9) %>% select(-c("List","Cause","Frmat")) %>% group_by(Country,Year,Sex) %>% summarise_each(funs(sum)) %>% mutate(COD = "Metabolic diseases")

total_blood <- total_data_merged %>% filter((List == 104 & substr(Cause,1,1) %in% c(paste0("D",50:89)))|
                                                   (List == 103 & substr(Cause,1,1) %in% c(paste0("D",50:89)))|
                                                   (List == "10M" & substr(Cause,1,1) %in% c(paste0("D",50:89)))|
                                                   (List == '09B' & Cause  %in% c("B20"))
                                                 ) %>% filter(Sex != 9) %>% select(-c("List","Cause","Frmat")) %>% group_by(Country,Year,Sex) %>% summarise_each(funs(sum)) %>% mutate(COD = "Blood diseases")

total_mental <- total_data_merged %>% filter((List == 104 & substr(Cause,1,1) %in% c("F"))|
                                                  (List == 103 & substr(Cause,1,1) %in% c("F"))|
                                                  (List == "10M" & substr(Cause,1,1) %in% c("F"))|
                                                  (List == '09B' & Cause  %in% c("B21"))
                                                ) %>% filter(Sex != 9) %>% select(-c("List","Cause","Frmat")) %>% group_by(Country,Year,Sex) %>% summarise_each(funs(sum)) %>% mutate(COD = "Mental disorders")

total_nervous <- total_data_merged %>% filter((List == 104 & substr(Cause,1,1) %in% c("G"))|
                                                  (List == 103 & substr(Cause,1,1) %in% c("G"))|
                                                  (List == "10M" & substr(Cause,1,1) %in% c("G"))|
                                                  (List == '09B' & Cause  %in% c("B22"))
                                                ) %>% filter(Sex != 9) %>% select(-c("List","Cause","Frmat")) %>% group_by(Country,Year,Sex) %>% summarise_each(funs(sum)) %>% mutate(COD = "Nervous system diseases")

total_eyeandear <- total_data_merged %>% filter((List == 104 & substr(Cause,1,1) %in% c("H"))|
                                                  (List == 103 & substr(Cause,1,1) %in% c("H"))|
                                                  (List == "10M" & substr(Cause,1,1) %in% c("H"))|
                                                  (List == '09B' & Cause  %in% c("B23","B24"))
                                                ) %>% filter(Sex != 9) %>% select(-c("List","Cause","Frmat")) %>% group_by(Country,Year,Sex) %>% summarise_each(funs(sum)) %>% mutate(COD = "Eye and ear diseases")

total_genitourinary <- total_data_merged %>% filter((List == 104 & substr(Cause,1,1) %in% c("N"))|
                                                  (List == 103 & substr(Cause,1,1) %in% c("N"))|
                                                  (List == "10M" & substr(Cause,1,1) %in% c("N"))|
                                                  (List == '09B' & Cause  %in% c("B35","B36","B37"))
                                                  ) %>% filter(Sex != 9) %>% select(-c("List","Cause","Frmat")) %>% group_by(Country,Year,Sex) %>% summarise_each(funs(sum)) %>% mutate(COD = "Genitourinary diseases")


# Calculate proportion of each disease with reference to overall mortality
Rx <- rbind(total_cvd,total_respiratory,total_neoplasms,total_ill_defined,
                      total_external,total_digestive,total_perinatal,total_infectious,total_congenital,
                      total_metabolic,total_mental,total_nervous) %>%
                tidyr::gather(.,"Age","Deaths",Total:`85+`) %>%
                left_join(.,tidyr::gather(total_mo,"Age","Deaths",Total:`85+`) %>% rename("Total deaths" = "Deaths") %>% select(-COD),by = c("Country","Year","Sex","Age"))

Rx <- Rx %>% mutate(Rx = Deaths/`Total deaths`)
Rx_hic2 <- Rx %>% filter(Country !="Hong Kong") %>% group_by(Year,Sex,COD,Age) %>% summarize(Rx = mean(Rx,na.rm = T)) %>% mutate(Country = "HICs")
Rx_total2 <- Rx %>% select(-c("Deaths",`Total deaths`)) %>% rbind(Rx_hic2,.) %>% mutate(Sex = ifelse(Sex == 1, "Male", "Female")) %>% rename("Agegroup" = "Age")
Rx_total2 <- rbind(Rx_total2, Rx_total2 %>% group_by(Year,Sex,Agegroup,Country) %>% summarize(Rx = 1-sum(Rx)) %>% mutate(COD = "Others"))

# Average proportion for high-income coutries
Rx_hic <- Rx %>% filter(Age != "Total" & Country != "Hong Kong") %>% rename("Agegroup" = "Age") %>% mutate(Sex = ifelse(Sex == 1, "Male", "Female")) %>%
  left_join(.,total_exposures,by = c("Year","Country","Sex","Agegroup"))
Rx_hic <- Rx_hic %>% mutate(overall_mx = `Total deaths`/Pop, icd_mx = Deaths/Pop, icd_prop = icd_mx/overall_mx) %>% data.frame()
Rx_hic <- Rx_hic %>% filter(Country !="Hong Kong") %>% group_by(Year,Sex,COD,Agegroup) %>% summarize(icd_mx = mean(icd_mx,na.rm = T),overall_mx = mean(overall_mx,na.rm = T)) %>% mutate(icd_prop = icd_mx/overall_mx,Country = "HICs") %>% data.frame()

Rx_total <- Rx %>% select(-c("Deaths",`Total deaths`)) %>% rename("Agegroup" = "Age") %>% rbind(Rx_hic[,-c(5:6)] %>% rename("Rx" = "icd_prop"),.) %>% mutate(Sex = ifelse(Sex == 1|Sex == "Male", "Male", "Female"))
Rx_total <- rbind(Rx_total, Rx_total %>% group_by(Year,Sex,Agegroup,Country) %>% summarize(Rx = 1-sum(Rx)) %>% mutate(COD = "Others"))
Rx_total <- Rx_total %>% mutate(Age = ifelse(Agegroup %in% c("0", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34"),"0-34",
                                             ifelse(Agegroup %in% c("35-39", "40-44", "45-49", "50-54", "55-59"),"35-59",
                                                    ifelse(Agegroup %in% c("80-84","85+"),"80+",Agegroup))))

# Smoking AF
AF_final <- read.csv("../Life expectancy/AF_age_dummy_timevarying.csv",h=T) %>% select(-Age1) %>% distinct()
AF_final <- Rx_total %>% left_join(.,AF_final %>% select(Year,Sex,Country,Age,AF),by = c('Year','Sex','Country','Age'))
Rx_total <- rbind(Rx_total, AF_final %>% filter(COD == "Cardiovascular diseases") %>% select(-Rx) %>% mutate(COD = "Smoking") %>% rename("Rx" = "AF")) %>% select(-Age)

# Average mortality rate among HIC
# Read exposure data for each country | repeat process for all countries
CountryA <- 'USA' 
Country<-paste('../Life expectancy/',CountryA,sep="")
setwd(Country)
# Data source HMD
usa_exposures <- fread("Exposures_5x1.txt") %>% data.frame()
usa_deaths <- fread("Deaths_5x1.txt") %>% data.frame()

total_exposures <- as.data.frame(bind_rows(aus_exposures %>% mutate(Country = "Australia"),aut_exposures %>% mutate(Country = "Austria"),
                                           ita_exposures %>% mutate(Country = "Italy"),fratnp_exposures %>% mutate(Country = "France"),
                                           deutnp_exposures %>% mutate(Country = "Germany"),gbr_exposures %>% mutate(Country = "UK"),
                                           esp_exposures %>% mutate(Country = "Spain"),usa_exposures %>% mutate(Country = "USA"),
                                           nld_exposures %>% mutate(Country = "Netherlands"),che_exposures %>% mutate(Country = "Switzerland"),
                                           swe_exposures %>% mutate(Country = "Sweden"),bel_exposures %>% mutate(Country = "Belgium"),
                                           prt_exposures %>% mutate(Country = "Portugal"),can_exposures %>% mutate(Country = "Canada"),
                                           dnk_exposures %>% mutate(Country = "Denmark"),jpn_exposures %>% mutate(Country = "Japan"),
                                           nor_exposures %>% mutate(Country = "Norway"),fin_exposures %>% mutate(Country = "Finland")))
total_exposures$Age <- ifelse(total_exposures$Age == "110+",110,total_exposures$Age)
total_exposures$Age <- as.numeric(as.character(total_exposures$Age))

total_deaths <- as.data.frame(bind_rows(aus_deaths %>% mutate(Country = "Australia"),aut_deaths %>% mutate(Country = "Austria"),
                                        ita_deaths %>% mutate(Country = "Italy"),fratnp_deaths %>% mutate(Country = "France"),
                                        deutnp_deaths %>% mutate(Country = "Germany"),gbr_deaths %>% mutate(Country = "UK"),
                                        esp_deaths %>% mutate(Country = "Spain"),usa_deaths %>% mutate(Country = "USA"),
                                        nld_deaths %>% mutate(Country = "Netherlands"),che_deaths %>% mutate(Country = "Switzerland"),
                                        swe_deaths %>% mutate(Country = "Sweden"),bel_deaths %>% mutate(Country = "Belgium"),
                                        prt_deaths %>% mutate(Country = "Portugal"),can_deaths %>% mutate(Country = "Canada"),
                                        dnk_deaths %>% mutate(Country = "Denmark"),jpn_deaths %>% mutate(Country = "Japan"),
                                        nor_deaths %>% mutate(Country = "Norway"),fin_deaths %>% mutate(Country = "Finland")))
total_deaths$Age <- ifelse(total_deaths$Age == "110+",110,total_deaths$Age)
total_deaths$Age <- as.numeric(as.character(total_deaths$Age))

total_exposures <- total_exposures %>% tidyr::gather(.,Sex,Pop,Female:Total,factor_key = T) %>%
  mutate(Agegroup = cut(Age, c(0,1,seq(5,115,5)), right = FALSE)) %>% mutate(Agegroup = forcats::fct_recode(factor(Agegroup),
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
                                                                                                              "85+"="[100,105)",
                                                                                                              "85+"="[105,110)",
                                                                                                              "85+"="[110,115)"
))

total_deaths <- total_deaths %>% tidyr::gather(.,Sex,Deaths,Female:Total,factor_key = T) %>%
  mutate(Agegroup = cut(Age, c(0,1,seq(5,115,5)), right = FALSE)) %>% mutate(Agegroup = forcats::fct_recode(factor(Agegroup),
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
                                                                                                            "85+"="[100,105)",
                                                                                                            "85+"="[105,110)",
                                                                                                            "85+"="[110,115)"
  ))

total_deaths <- total_deaths %>% group_by(Year,Country,Sex,Agegroup) %>% summarize(Deaths = sum(Deaths,na.rm = T)) %>% filter(Year > 1978)
total_exposures <- total_exposures %>% group_by(Year,Country,Sex,Agegroup) %>% summarize(Pop = sum(Pop,na.rm = T)) %>% filter(Year > 1978)

total_mx <- total_exposures %>% left_join(.,total_deaths,by = c("Year","Agegroup","Country","Sex")) %>% mutate(mx = Deaths/Pop)
total_mx_hic <- total_mx %>% group_by(Year,Sex,Agegroup) %>% summarize(mx = mean(mx,na.rm = T)) %>% mutate(Country = "HICs")

test <- data.frame(Age = rep(0:100,14*2), COD = rep(unique(Rx_total$COD),each = 101*2),Sex = rep(rep(c("Male","Female"),each = 101),14)) %>%
  mutate(Agegroup = cut(Age, c(0,1,seq(5,105,5)), right = FALSE)) %>% mutate(Agegroup = forcats::fct_recode(factor(Agegroup),
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
  )) %>% left_join(.,Rx_total %>% filter(Year == 1979 & Country == "HICs"),by = c("Agegroup","COD","Sex")) %>% rename("Rx1" = "Rx") %>%
         left_join(.,Rx_total %>% filter(Year == 1979 & Country == "Hong Kong") %>% select(-Country,-Year),by = c("Agegroup","COD","Sex")) %>% rename("Rx2" = "Rx")


test <- test %>%
         left_join(.,lt_merged %>% filter(Year == 1979 & Country == "HICs") %>% select(Age,sex,mx),by = c("Age","Sex" = "sex")) %>% rename('mx1' = 'mx') %>%
         left_join(.,lt_merged %>% filter(Year == 1979 & Country == "Hong Kong") %>% select(Age,sex,mx),by = c("Age","Sex" = "sex")) %>% rename('mx2' = 'mx') %>%
         left_join(.,rbind(age_decomp("HICs","Hong Kong","Female",1979) %>% mutate(Sex = "Female"),age_decomp("HICs","Hong Kong","Male",1979) %>% mutate(Sex = "Male")) %>% select(-Agegroup),by = c("Age","Sex")) %>%
         mutate(age_cod_contri = age_contri*((Rx1*mx1-Rx2*mx2)/(mx1-mx2)))
test <- test %>% group_by(COD,Sex,Agegroup) %>% summarize(age_cod_contri79 = sum(age_cod_contri))

#-------------------------------------------------------------------------------------------------------------------------------------------------------
test2 <- data.frame(Age = rep(0:100,14*2), COD = rep(unique(Rx_total$COD),each = 101*2),Sex = rep(rep(c("Male","Female"),each = 101),14)) %>%
  mutate(Agegroup = cut(Age, c(0,1,seq(5,105,5)), right = FALSE)) %>% mutate(Agegroup = forcats::fct_recode(factor(Agegroup),
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
  )) %>% left_join(.,Rx_total %>% filter(Year == 2016 & Country == "HICs"),by = c("Agegroup","COD","Sex")) %>% rename("Rx1" = "Rx") %>%
  left_join(.,Rx_total %>% filter(Year == 2016 & Country == "Hong Kong") %>% select(-Country,-Year),by = c("Agegroup","COD","Sex")) %>% rename("Rx2" = "Rx")

test2 <- test2 %>%
  left_join(.,lt_merged %>% filter(Year == 2016 & Country == "HICs") %>% select(Age,sex,mx),by = c("Age","Sex" = "sex")) %>% rename('mx1' = 'mx') %>%
  left_join(.,lt_merged %>% filter(Year == 2016 & Country == "Hong Kong") %>% select(Age,sex,mx),by = c("Age","Sex" = "sex")) %>% rename('mx2' = 'mx') %>%
  left_join(.,rbind(age_decomp("HICs","Hong Kong","Female",2016) %>% mutate(Sex = "Female"),age_decomp("HICs","Hong Kong","Male",2016) %>% mutate(Sex = "Male")) %>% select(-Agegroup),by = c("Age","Sex")) %>%
  mutate(age_cod_contri = age_contri*((Rx1*mx1-Rx2*mx2)/(mx1-mx2)))
test2 <- test2 %>% group_by(COD,Sex,Agegroup) %>% summarize(age_cod_contri16 = sum(age_cod_contri))
#---------------------------------------------------------------------------------------------------------------------------------------------------------
for_plotting <- test2 %>% left_join(.,test,by = c("COD", "Sex", "Agegroup")) %>% mutate(contri = age_cod_contri16-age_cod_contri79)
for_plotting$COD <- factor(for_plotting$COD, levels = c('Smoking','Cardiovascular diseases','Respiratory diseases','Neoplasms',"Nervous system diseases","Mental disorders","Metabolic diseases",'Ill-defined causes','External causes','Digestive diseases','Perinatal deaths','Infectious diseases','Congenital anomalies','Others'))
for_plotting$Agegroup <- factor(for_plotting$Agegroup, levels = c("0", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34","35-39", "40-44", "45-49",
                                                                  "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+"))
by_cod <- for_plotting %>% group_by(COD,Sex) %>% summarize(contri = sum(contri))
by_age <- for_plotting %>% group_by(Sex,Agegroup) %>% summarize(contri = sum(contri))
test2$COD <- factor(test2$COD, levels = c('Smoking','Cardiovascular diseases','Respiratory diseases','Neoplasms',"Nervous system diseases","Mental disorders","Metabolic diseases",'Ill-defined causes','External causes','Digestive diseases','Perinatal deaths','Infectious diseases','Congenital anomalies','Others'))
test2$Agegroup <- factor(test2$Agegroup, levels = c("0", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34","35-39", "40-44", "45-49",
                                                                  "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+"))

blank <- ggplot() + geom_blank() + theme_classic() + theme(axis.line = element_line(colour = "white"))

p7 <- ggplot(test2 %>% filter(Sex == "Female" & COD!= "Smoking"),aes(Agegroup,fct_rev(COD),fill = age_cod_contri16)) + geom_tile() + 
  scale_fill_gradientn(colours = c("red",rgb(239,67,67,maxColorValue = 255),WildColors[5],rgb(54,31,155,maxColorValue = 255),rgb(87,58,213,maxColorValue = 255),rgb(34,7,150,maxColorValue = 255),WildColors[10]),breaks = c(-.4,-.2,0,0.2,.4,.6,.8)) + # Women
  scale_x_discrete(labels = c("0", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34","35-39", "40-44", "45-49",
                              "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", expression("">=85))) +
  labs(y = "Cause of death\n", x = "\nAge group (years)", fill = "Contribution to life expectancy gap (years)") + theme_classic() + theme(text =  element_text(size = 18),axis.text.x = element_text(angle = 90, vjust = 0.5)) + guides(fill = F)
ggsave("../Life expectancy/Arriaga change in difference (Women 2016) heatmap.png", dpi = 300, height = 9, width = 12)

p8 <- ggplot(test2 %>% filter(COD != 'Smoking' & Sex == "Female") %>% group_by(Agegroup,Sex) %>% summarize(contri = sum(age_cod_contri16)) %>% mutate(color = ifelse(contri>=0,"A","B")),aes(Agegroup,contri,fill = color)) + geom_col() + guides(fill = FALSE) + labs(y = "Total Contribution (years)", x = "", title = "Females") +
  geom_text(aes(label = "All causes",x = 10, y = 1.25), size = 7.5) + scale_fill_manual(values = c(WildColors[10],WildColors[1])) + theme_classic() +
  theme(text =  element_text(size = 18),axis.text.x = element_blank())
ggsave("../Life expectancy/Arriaga change in difference by age (Women 1979-2016).png", dpi = 300, height = 9, width = 12)

p9 <- ggplot(test2 %>% filter(COD != 'Smoking' & Sex == "Female") %>% group_by(COD,Sex) %>% summarize(contri = sum(age_cod_contri16)) %>% mutate(color = ifelse(contri>=0,"A","B")),aes(fct_rev(COD),contri,fill = color)) + geom_col() + guides(fill = FALSE) + coord_flip() +
  scale_fill_manual(values = c(WildColors[10],WildColors[1])) + theme_classic() + labs(y = "Total contribution (years)", x = "", title = "All ages") +
  theme(text =  element_text(size = 18),axis.text.y = element_blank(),plot.title = element_text(hjust = 0.5)) 
ggsave("../Life expectancy/Arriaga change in difference by cod (Women 1979-2016).png", dpi = 300, height = 9, width = 12)

p10 <- ggplot(test2 %>% filter(Sex == "Male" & COD!= "Smoking"),aes(Agegroup,fct_rev(COD),fill = age_cod_contri16)) + geom_tile() + 
  scale_x_discrete(labels = c("0", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34","35-39", "40-44", "45-49",
                              "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", expression("">=85))) +
  scale_fill_gradientn(colours = c("red",rgb(239,67,67,maxColorValue = 255),WildColors[5],rgb(34,7,150,maxColorValue = 255),WildColors[10]),breaks = c(-.35,-.175,0,.175,0.35)) + # Women
  labs(y = "Cause of death\n", x = "\nAge group (years)", fill = "Contribution to life expectancy gap (years)") + theme_classic() + theme(text =  element_text(size = 18),axis.text.x = element_text(angle = 90, vjust = 0.5)) + guides(fill = F)
ggsave("../Life expectancy/Arriaga change in difference (Men 2016) heatmap.png", dpi = 300, height = 9, width = 12)

p11 <- ggplot(test2 %>% filter(COD != 'Smoking' & Sex == "Male") %>% group_by(Agegroup,Sex) %>% summarize(contri = sum(age_cod_contri16)) %>% mutate(color = ifelse(contri>=0,"A","B")),aes(Agegroup,contri,fill = color)) + geom_col() + guides(fill = FALSE) + labs(y = "Total Contribution (years)", x = "", title = "Males") +
  geom_text(aes(label = "All causes",x = 10, y = .4), size = 7.5) + scale_fill_manual(values = c(WildColors[10],WildColors[1])) + theme_classic() +
  theme(text =  element_text(size = 18),axis.text.x = element_blank())
ggsave("../Life expectancy/Arriaga change in difference by age (Women 1979-2016).png", dpi = 300, height = 9, width = 12)

p12 <- ggplot(test2 %>% filter(COD != 'Smoking' & Sex == "Male") %>% group_by(COD,Sex) %>% summarize(contri = sum(age_cod_contri16)) %>% mutate(color = ifelse(contri>=0,"A","B")),aes(fct_rev(COD),contri,fill = color)) + geom_col() + guides(fill = FALSE) + coord_flip() +
  scale_fill_manual(values = c(WildColors[10],WildColors[1])) + theme_classic() + labs(y = "Total contribution (years)", x = "", title = "All ages") +
  theme(text =  element_text(size = 18),axis.text.y = element_blank(),plot.title = element_text(hjust = 0.5)) 
ggsave("../Life expectancy/Arriaga change in difference by cod (Women 1979-2016).png", dpi = 300, height = 9, width = 12)

p_female16 <- plot_grid(p8,blank,p7,p9,align = "hv")
p_male16 <- plot_grid(p11,blank,p10,p12,align = "hv")

ggsave("../Life expectancy/Arriaga change in difference (Men 2016).png",p_male16, dpi = 300, height = 12, width = 16)

save.image("../Life expectancy/Arriaga change in difference.RData")

