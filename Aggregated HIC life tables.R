library(dplyr)
library(MortalityLaws)

# Read exposure and deaths data for each country | repeat process for all countries
CountryA <- 'USA' 
Country<-paste('../Life expectancy/',CountryA,sep="")
setwd(Country)
# Data source HMD
usa_exposures <- fread("Exposures_5x1.txt") %>% data.frame()
usa_deaths <- fread("Exposures_5x1.txt") %>% data.frame()

total_exposures <- as.data.frame(bind_rows(aus_exposures %>% mutate(Country = "Australia"),aut_exposures %>% mutate(Country = "Austria"),
                                           ita_exposures %>% mutate(Country = "Italy"),fratnp_exposures %>% mutate(Country = "France"),
                                           deutnp_exposures %>% mutate(Country = "Germany"),gbr_exposures %>% mutate(Country = "UK"),
                                           esp_exposures %>% mutate(Country = "Spain"),usa_exposures %>% mutate(Country = "USA"),
                                           nld_exposures %>% mutate(Country = "Netherlands"),che_exposures %>% mutate(Country = "Switzerland"),
                                           swe_exposures %>% mutate(Country = "Sweden"),bel_exposures %>% mutate(Country = "Belgium"),
                                           prt_exposures %>% mutate(Country = "Portugal"),can_exposures %>% mutate(Country = "Canada"),
                                           dnk_exposures %>% mutate(Country = "Denmark"),jpn_exposures %>% mutate(Country = "Japan"),
                                           nor_exposures %>% mutate(Country = "Norway"),fin_exposures %>% mutate(Country = "Finland")))

total_deaths <- as.data.frame(bind_rows(aus_deaths %>% mutate(Country = "Australia"),aut_deaths %>% mutate(Country = "Austria"),
                                        ita_deaths %>% mutate(Country = "Italy"),fratnp_deaths %>% mutate(Country = "France"),
                                        deutnp_deaths %>% mutate(Country = "Germany"),gbr_deaths %>% mutate(Country = "UK"),
                                        esp_deaths %>% mutate(Country = "Spain"),usa_deaths %>% mutate(Country = "USA"),
                                        nld_deaths %>% mutate(Country = "Netherlands"),che_deaths %>% mutate(Country = "Switzerland"),
                                        swe_deaths %>% mutate(Country = "Sweden"),bel_deaths %>% mutate(Country = "Belgium"),
                                        prt_deaths %>% mutate(Country = "Portugal"),can_deaths %>% mutate(Country = "Canada"),
                                        dnk_deaths %>% mutate(Country = "Denmark"),jpn_deaths %>% mutate(Country = "Japan"),
                                        nor_deaths %>% mutate(Country = "Norway"),fin_deaths %>% mutate(Country = "Finland")))

# Calculate weighted mortality rate
aus_mx <- aus_deaths %>% left_join(.,aus_exposures,by = c("Year","Age")) %>% mutate(aus_male_mx = Male.x/Male.y,aus_female_mx = Female.x/Female.y,aus_total_mx = Total.x/Total.y) %>% select(Year,Age,aus_male_mx,aus_female_mx,aus_total_mx)
aut_mx <- aut_deaths %>% left_join(.,aut_exposures,by = c("Year","Age")) %>% mutate(aut_male_mx = Male.x/Male.y,aut_female_mx = Female.x/Female.y,aut_total_mx = Total.x/Total.y) %>% select(Year,Age,aut_male_mx,aut_female_mx,aut_total_mx)
bel_mx <- bel_deaths %>% left_join(.,bel_exposures,by = c("Year","Age")) %>% mutate(bel_male_mx = Male.x/Male.y,bel_female_mx = Female.x/Female.y,bel_total_mx = Total.x/Total.y) %>% select(Year,Age,bel_male_mx,bel_female_mx,bel_total_mx)
can_mx <- can_deaths %>% left_join(.,can_exposures,by = c("Year","Age")) %>% mutate(can_male_mx = Male.x/Male.y,can_female_mx = Female.x/Female.y,can_total_mx = Total.x/Total.y) %>% select(Year,Age,can_male_mx,can_female_mx,can_total_mx)
dnk_mx <- dnk_deaths %>% left_join(.,dnk_exposures,by = c("Year","Age")) %>% mutate(dnk_male_mx = Male.x/Male.y,dnk_female_mx = Female.x/Female.y,dnk_total_mx = Total.x/Total.y) %>% select(Year,Age,dnk_male_mx,dnk_female_mx,dnk_total_mx)
fin_mx <- fin_deaths %>% left_join(.,fin_exposures,by = c("Year","Age")) %>% mutate(fin_male_mx = Male.x/Male.y,fin_female_mx = Female.x/Female.y,fin_total_mx = Total.x/Total.y) %>% select(Year,Age,fin_male_mx,fin_female_mx,fin_total_mx)
fratnp_mx <- fratnp_deaths %>% left_join(.,fratnp_exposures,by = c("Year","Age")) %>% mutate(fratnp_male_mx = Male.x/Male.y,fratnp_female_mx = Female.x/Female.y,fratnp_total_mx = Total.x/Total.y) %>% select(Year,Age,fratnp_male_mx,fratnp_female_mx,fratnp_total_mx)
deute_mx <- deute_deaths %>% left_join(.,deute_exposures,by = c("Year","Age")) %>% mutate(deute_male_mx = Male.x/Male.y,deute_female_mx = Female.x/Female.y,deute_total_mx = Total.x/Total.y) %>% select(Year,Age,deute_male_mx,deute_female_mx,deute_total_mx)
deutw_mx <- deutw_deaths %>% left_join(.,deutw_exposures,by = c("Year","Age")) %>% mutate(deutw_male_mx = Male.x/Male.y,deutw_female_mx = Female.x/Female.y,deutw_total_mx = Total.x/Total.y) %>% select(Year,Age,deutw_male_mx,deutw_female_mx,deutw_total_mx)
ita_mx <- ita_deaths %>% left_join(.,ita_exposures,by = c("Year","Age")) %>% mutate(ita_male_mx = Male.x/Male.y,ita_female_mx = Female.x/Female.y,ita_total_mx = Total.x/Total.y) %>% select(Year,Age,ita_male_mx,ita_female_mx,ita_total_mx)
jpn_mx <- jpn_deaths %>% left_join(.,jpn_exposures,by = c("Year","Age")) %>% mutate(jpn_male_mx = Male.x/Male.y,jpn_female_mx = Female.x/Female.y,jpn_total_mx = Total.x/Total.y) %>% select(Year,Age,jpn_male_mx,jpn_female_mx,jpn_total_mx)
nld_mx <- nld_deaths %>% left_join(.,nld_exposures,by = c("Year","Age")) %>% mutate(nld_male_mx = Male.x/Male.y,nld_female_mx = Female.x/Female.y,nld_total_mx = Total.x/Total.y) %>% select(Year,Age,nld_male_mx,nld_female_mx,nld_total_mx)
nor_mx <- nor_deaths %>% left_join(.,nor_exposures,by = c("Year","Age")) %>% mutate(nor_male_mx = Male.x/Male.y,nor_female_mx = Female.x/Female.y,nor_total_mx = Total.x/Total.y) %>% select(Year,Age,nor_male_mx,nor_female_mx,nor_total_mx)
prt_mx <- prt_deaths %>% left_join(.,prt_exposures,by = c("Year","Age")) %>% mutate(prt_male_mx = Male.x/Male.y,prt_female_mx = Female.x/Female.y,prt_total_mx = Total.x/Total.y) %>% select(Year,Age,prt_male_mx,prt_female_mx,prt_total_mx)
esp_mx <- esp_deaths %>% left_join(.,esp_exposures,by = c("Year","Age")) %>% mutate(esp_male_mx = Male.x/Male.y,esp_female_mx = Female.x/Female.y,esp_total_mx = Total.x/Total.y) %>% select(Year,Age,esp_male_mx,esp_female_mx,esp_total_mx)
che_mx <- che_deaths %>% left_join(.,che_exposures,by = c("Year","Age")) %>% mutate(che_male_mx = Male.x/Male.y,che_female_mx = Female.x/Female.y,che_total_mx = Total.x/Total.y) %>% select(Year,Age,che_male_mx,che_female_mx,che_total_mx)
swe_mx <- swe_deaths %>% left_join(.,swe_exposures,by = c("Year","Age")) %>% mutate(swe_male_mx = Male.x/Male.y,swe_female_mx = Female.x/Female.y,swe_total_mx = Total.x/Total.y) %>% select(Year,Age,swe_male_mx,swe_female_mx,swe_total_mx)
gbr_mx <- gbr_deaths %>% left_join(.,gbr_exposures,by = c("Year","Age")) %>% mutate(gbr_male_mx = Male.x/Male.y,gbr_female_mx = Female.x/Female.y,gbr_total_mx = Total.x/Total.y) %>% select(Year,Age,gbr_male_mx,gbr_female_mx,gbr_total_mx)
usa_mx <- usa_deaths %>% left_join(.,usa_exposures,by = c("Year","Age")) %>% mutate(usa_male_mx = Male.x/Male.y,usa_female_mx = Female.x/Female.y,usa_total_mx = Total.x/Total.y) %>% select(Year,Age,usa_male_mx,usa_female_mx,usa_total_mx)

total_mx <- usa_mx %>% left_join(.,aus_mx,by = c("Year","Age")) %>%
  left_join(.,bel_mx,by = c("Year","Age")) %>%
  left_join(.,can_mx,by = c("Year","Age")) %>%
  left_join(.,dnk_mx,by = c("Year","Age")) %>%
  left_join(.,fin_mx,by = c("Year","Age")) %>%
  left_join(.,fratnp_mx,by = c("Year","Age")) %>%
  left_join(.,deute_mx,by = c("Year","Age")) %>%
  left_join(.,deutw_mx,by = c("Year","Age")) %>%
  left_join(.,ita_mx,by = c("Year","Age")) %>%
  left_join(.,jpn_mx,by = c("Year","Age")) %>%
  left_join(.,nld_mx,by = c("Year","Age")) %>%
  left_join(.,nor_mx,by = c("Year","Age")) %>%
  left_join(.,prt_mx,by = c("Year","Age")) %>%
  left_join(.,esp_mx,by = c("Year","Age")) %>%
  left_join(.,che_mx,by = c("Year","Age")) %>%
  left_join(.,swe_mx,by = c("Year","Age")) %>%
  left_join(.,gbr_mx,by = c("Year","Age")) %>%
  left_join(.,aut_mx,by = c("Year","Age")) %>% filter(Year %in% (1970:2017))

total_male_mx <- total_mx %>% select(Year,Age,aus_male_mx,aut_male_mx,bel_male_mx,can_male_mx,dnk_male_mx,fin_male_mx,fratnp_male_mx,deutw_male_mx,deute_male_mx,ita_male_mx,jpn_male_mx,nld_male_mx,nor_male_mx,prt_male_mx,esp_male_mx,che_male_mx,swe_male_mx,gbr_male_mx,usa_male_mx)
total_male_mx[,-c(1:2)][total_male_mx[,-c(1:2)] > 1] <- 1
total_male_mx <- cbind(total_male_mx[,1:2],mx = rowMeans(total_male_mx[,-c(1:2)],na.rm = T))
total_male_mx$Age <- rep(rep(0:110),48)

header.true <- function(df) {
  names(df) <- as.character(unlist(df[1,]))
  df[-1,]
}

hic_male_lt <- LifeTable(x = c(0:110),mx = header.true(total_male_mx %>% select(Year,mx) %>% group_by(Year) %>% mutate(seq = 1:n()) %>% ungroup() %>% tidyr::spread(seq,mx) %>% t(.) %>% as.data.frame()))

total_female_mx <- total_mx %>% select(Year,Age,aus_female_mx,aut_female_mx,bel_female_mx,can_female_mx,dnk_female_mx,fin_female_mx,fratnp_female_mx,deutw_female_mx,deute_female_mx,ita_female_mx,jpn_female_mx,nld_female_mx,nor_female_mx,prt_female_mx,esp_female_mx,che_female_mx,swe_female_mx,gbr_female_mx,usa_female_mx)
total_female_mx[,-c(1:2)][total_female_mx[,-c(1:2)] > 1] <- 1
total_female_mx <- cbind(total_female_mx[,1:2],mx = rowMeans(total_female_mx[,-c(1:2)],na.rm = T))
total_female_mx$Age <- rep(rep(0:110),48)

hic_female_lt <- LifeTable(x = c(0:110),mx = header.true(total_female_mx %>% select(Year,mx) %>% group_by(Year) %>% mutate(seq = 1:n()) %>% ungroup() %>% tidyr::spread(seq,mx) %>% t(.) %>% as.data.frame()))

# Export as csv
hic_male <- hic_male_lt$lt %>% select(LT,x,qx) %>% rename('Year'='LT','Age'='x')
hic_female <- hic_female_lt$lt %>% select(LT,x,qx) %>% rename('Year'='LT','Age'='x')

write.csv(hic_male,"../Life expectancy/hic_male_lt.csv",row.names = F)
write.csv(hic_female,"../Life expectancy/hic_female_lt.csv",row.names = F)


