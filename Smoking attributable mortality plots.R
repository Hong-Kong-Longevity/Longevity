library(dplyr)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(randomcoloR)
library(data.table)
library(tidyr)

# Create custom palette
mypalette<-rev(brewer.pal(9,"YlGnBu"))
mypalette2<-rev(brewer.pal(9,"YlOrRd"))
WildColors<-c(mypalette,"lemonchiffon","lemonchiffon","lemonchiffon",mypalette2[9:1])

# Load smoking attributable mortality data
mort2016 <- read.csv("C:/Users/FreyaYu/Documents/Tasks/Life Expectancy Task/Recent files/Results/Ezzati/New folder/AF_mort2016.csv",h=T)
mort2016$Country <- factor(mort2016$Country,levels = (mort2016 %>% arrange(value,Country))$Country)
mort2016 <- data.frame(mort2016)

plot_labels <- mort2016$Country %>% levels()
plot_labels[21] <- 'Denmark*'

ggplot(mort2016,aes(Country,value,fill = Country)) + geom_col() + theme_classic() +
  labs(x = "", y = "Estimated number of deaths attributable\nto smoking per 100,000 population\n") +
  theme(text = element_text(size = 36),axis.text.x = element_text(angle = 90,vjust = 0.5),axis.title.y = element_text(size = 32)) +
  scale_fill_manual(values = WildColors) + guides(fill = F) + scale_x_discrete(labels = plot_labels) +
  geom_errorbar(data = mort2016,aes(ymin = lb,ymax = ub),color = "grey44",size = 1.2,width = 0.2,position = position_dodge(width = 0.75))
ggsave("C:/Users/FreyaYu/Documents/Tasks/Life Expectancy Task/Recent files/Results/Ezzati/New folder/Smoking mortality.png",dpi=300,height = 12,width = 16)
#--------------------------------------------------------------------------------------------------------------------------------------------------------
# Load smoking attributable fraction data
af2016 <- read.csv("C:/Users/FreyaYu/Documents/Tasks/Life Expectancy Task/Recent files/Results/Ezzati/New folder/AF2016.csv",h=T)
af2016$Country <- factor(af2016$Country,levels = (af2016 %>% arrange(value,Country))$Country)
af2016 <- data.frame(af2016)

plot_labels <- af2016$Country %>% levels()
plot_labels[21] <- 'Denmark*'

ggplot(af2016,aes(Country,value,fill = Country)) + geom_col() + theme_classic() +
  labs(x = "", y = "Proportion of deaths attributable to smoking\n") +
  theme(text = element_text(size = 36),axis.title.y = element_text(size = 32),axis.text.x = element_text(angle = 90,vjust = 0.5)) +
  scale_fill_manual(values = WildColors) + guides(fill = F) + scale_x_discrete(labels = plot_labels) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  geom_errorbar(data = af2016,aes(ymin = lb,ymax = ub),color = "grey44",size = 1.2,width = 0.2,position = position_dodge(width = 0.75))
ggsave("C:/Users/FreyaYu/Documents/Tasks/Life Expectancy Task/Recent files/Results/Ezzati/New folder/Smoking AF.png",dpi=300,height = 12,width = 16)

plot_grid(p1,p2,align = "v",nrow = 2)
#----------------------------------------------------------------------------------------------
# Read exposure and deaths data for each country | repeat process for all countries
CountryA <- 'USA' 
Country<-paste('C:/Users/FreyaYu/Documents/Tasks/Life Expectancy Task/Recent files/Data/Exposure/',CountryA,sep="")
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

total_deaths <- as.data.frame(bind_rows(aus_deaths %>% mutate(Country = "Australia"),aut_deaths %>% mutate(Country = "Austria"),
                                        ita_deaths %>% mutate(Country = "Italy"),fratnp_deaths %>% mutate(Country = "France"),
                                        deutnp_deaths %>% mutate(Country = "Germany"),gbr_deaths %>% mutate(Country = "UK"),
                                        esp_deaths %>% mutate(Country = "Spain"),usa_deaths %>% mutate(Country = "USA"),
                                        nld_deaths %>% mutate(Country = "Netherlands"),che_deaths %>% mutate(Country = "Switzerland"),
                                        swe_deaths %>% mutate(Country = "Sweden"),bel_deaths %>% mutate(Country = "Belgium"),
                                        prt_deaths %>% mutate(Country = "Portugal"),can_deaths %>% mutate(Country = "Canada"),
                                        dnk_deaths %>% mutate(Country = "Denmark"),jpn_deaths %>% mutate(Country = "Japan"),
                                        nor_deaths %>% mutate(Country = "Norway"),fin_deaths %>% mutate(Country = "Finland")))
load('C:/Users/FreyaYu/Documents/Tasks/Life Expectancy Task/Data/CS&D Known Deaths/DeathData7618.rda')
hk_deaths <- DeathData7618 %>% select(Year,Sex) %>% table() %>% data.frame()
hk_deaths$Year <- as.integer(as.character(hk_deaths$Year))
hk_mx <- hk_exposure %>% left_join(.,hk_deaths,by=c("Year","Sex"))
hk_mx <- hk_mx %>% mutate(mx = Freq/Pop) %>% mutate(Country = "Hong Kong")
kor_deaths <- data.table::fread("C:/Users/Francis/Desktop/Ezzati/Ezzati/Deaths_1x1 Korea.txt") %>% data.frame()
kor_deaths <- gather(kor_deaths,Sex,Deaths,Female:Total) %>% filter(Sex != 'Total') %>% group_by(Year,Sex) %>% summarize(Deaths = sum(Deaths)) %>% data.frame()
kor_exposure <- data.table::fread("C:/Users/Francis/Desktop/Ezzati/Ezzati/Exposures_1x1 Korea.txt") %>% data.frame()
kor_exposure <- gather(kor_exposure,Sex,Pop,Female:Total) %>% filter(Sex != 'Total') %>% group_by(Year,Sex) %>% summarize(Pop = sum(Pop)) %>% data.frame()
kor_mx <- kor_deaths %>% left_join(.,kor_exposure,by = c("Year","Sex")) %>% mutate(mx = Deaths/Pop, Country = "South Korea")
deaths_overall <- total_deaths %>% group_by(Year,Country,Sex) %>% summarize(Deaths = sum(Deaths)) %>% data.frame()
exposure_overall <- total_exposures %>% group_by(Year,Country,Sex) %>% summarize(Pop = sum(Pop)) %>% data.frame()
mx_overall <- deaths_overall %>% left_join(.,exposure_overall,by = c("Year","Country","Sex")) %>% data.frame()
mx_overall <- mx_overall %>% mutate(mx = Deaths/Pop) %>% data.frame()
mx_overall_hic <- mx_overall %>% group_by(Year,Sex) %>% summarize(mx = mean(mx,na.rm = T)) %>% mutate(Country = 'HIC') %>% data.frame()
mx_overall <- rbind(mx_overall %>% select(-Pop,-Deaths),mx_overall_hic,sg_mx %>% select(-Deaths1,-Pop),kor_mx %>% select(-Pop,-Deaths),hk_mx %>% select(-Freq,-Pop))
mx_overall <- mx_overall %>% left_join(.,smokingAF,by = c("Year","Country","Sex")) %>% filter(Sex != "Total")
mx_overall <- mx_overall %>% mutate(mx_smoking = mx*AF)
mx_overall_dnk <- mx_overall %>% filter(Country == "Denmark" & Year < 2016)
mx_overall <- rbind(mx_overall %>% filter(Country != 'Denmark'),mx_overall_dnk)

mx_overall$Sex <- factor(mx_overall$Sex,levels = c("Male","Female"))
mx_overall$Country <- as.character(mx_overall$Country)
mx_overall$Country <- ifelse(mx_overall$Country == "HIC","18 OECD high-income countries",mx_overall$Country)
mx_overall <- mx_overall %>% filter(Country %in% c("Hong Kong","18 OECD high-income countries","Australia","Japan","Singapore","South Korea","UK","USA"))
#mx_overall$Country <- factor(mx_overall$Country,levels = c("Hong Kong (LIMOR)","Hong Kong (CPS-II)","18 OECD high-income countries","Australia","Japan","Singapore","South Korea","UK","USA"))
mx_overall$Country <- factor(mx_overall$Country,levels = c("Hong Kong","18 OECD high-income countries","Australia","Belgium","Denmark","Japan","Singapore","South Korea","UK","USA"))

ggplot(mx_overall %>% filter(Year<2017) %>% na.omit(),aes(Year,mx_smoking*100000,color=Country)) + geom_line(lwd = 1.2) +
  #geom_line(data = mx_overall %>% filter(Year<2017 & !Country %in% c("Hong Kong","High-income countries")),aes(Year,AF,color = Country),lwd = 1.2,alpha = 0.2) +
  facet_wrap(~Sex) + theme_classic() + scale_x_continuous(breaks = c(seq(1980,2010,10))) + #scale_y_continuous(breaks = seq(0,0.4,0.1),limits = c(0,0.4),labels = scales::percent_format(accuracy = 1)) +
  theme(text = element_text(family = "sans"),legend.title = element_blank(),axis.text = element_text(size = 32),axis.title = element_text(size = 32),strip.placement = "inside",strip.text = element_text(size = 32),strip.background = element_blank(),legend.position = c(0.82,0.75),panel.spacing = unit(2, "lines"),legend.text = element_text(size = 32)) +
  labs(y = "Estimated number of deaths attributable to smoking per 100,000 population\n", x = "\nYear") + scale_color_manual(values = c("blue","black",brewer.pal(8,"YlOrBr")[3],brewer.pal(8,"Greens")[3],"khaki2","lightpink1","#EBDFED","lightskyblue1"))
ggsave("C:/Users/FreyaYu/Documents/Tasks/Life Expectancy Task/Recent files/Results/Ezzati/New folder/Smoking AF mx.png",dpi=300,height = 16.5,width = 22)


# 95% CI for death rate
exposure_overall <- total_exposures %>% group_by(Year,Country,Sex) %>% summarize(Pop = sum(Pop)) %>% data.frame()
exposure_overall <- rbind(exposure_overall,sg_mx %>% select(Year,Sex,Pop,Country),kor_mx %>% select(Year,Sex,Pop,Country),hk_mx %>% select(Year,Sex,Pop,Country))

saf_smk <- read.csv("C:/Users/FreyaYu/Downloads/AF_overall_individual_0303_for CIs.csv",h=T)
saf_smk <- saf_smk %>% left_join(.,exposure_overall,by = c("Year","Country","Sex"))
saf_smk <- saf_smk %>% filter(Year %in% c(1979:2016)) %>% mutate(mx_smk = SAF/Pop)
saf_smk_dnk <- saf_smk %>% filter(Country == "Denmark" & Year < 2016)
saf_smk <- rbind(saf_smk %>% filter(Country != 'Denmark'),saf_smk_dnk)
saf_hic <- saf_smk %>% filter(!Country %in% c("Hong Kong","Singapore","South Korea"))
saf_hic <- saf_hic %>% mutate(mx = Total/Pop)
saf_hic <- saf_hic %>% group_by(Year,Sex) %>% summarize(mx = mean(mx,na.rm = T)) %>% mutate(Country = 'HIC') %>% data.frame()
smokingAF <- read.csv("C:/Users/FreyaYu/Documents/Tasks/Life Expectancy Task/Recent files/Results/Ezzati/New folder/AF_overall_FINAL_LIMOR_timevarying_0128.csv",h=T)
saf_hic <- saf_hic %>% left_join(.,smokingAF,by = c("Year","Country","Sex"))
saf_hic <- saf_hic %>% mutate(mx_smk = mx*AF) %>% select(-mx)
saf_smk_plot <- saf_smk %>% select(- c("SAF","Total","Pop")) %>% rbind(saf_hic)


saf_smk_plot$Sex <- factor(saf_smk_plot$Sex,levels = c("Male","Female"))
saf_smk_plot$Country <- as.character(saf_smk_plot$Country)
saf_smk_plot$Country <- ifelse(saf_smk_plot$Country == "HIC","18 OECD high-income countries",saf_smk_plot$Country)
saf_smk_plot <- saf_smk_plot %>% filter(Country %in% c("Hong Kong","18 OECD high-income countries","Australia","Japan","Singapore","South Korea","UK","USA"))
#mx_overall$Country <- factor(mx_overall$Country,levels = c("Hong Kong (LIMOR)","Hong Kong (CPS-II)","18 OECD high-income countries","Australia","Japan","Singapore","South Korea","UK","USA"))
saf_smk_plot$Country <- factor(saf_smk_plot$Country,levels = c("Hong Kong","18 OECD high-income countries","Australia","Japan","Singapore","South Korea","UK","USA"))

ggplot(saf_smk_plot %>% filter(Year<2017) %>% na.omit(),aes(Year,mx_smk*100000,color=Country)) + geom_line(lwd = 1.2) +
  #geom_line(data = mx_overall %>% filter(Year<2017 & !Country %in% c("Hong Kong","High-income countries")),aes(Year,AF,color = Country),lwd = 1.2,alpha = 0.2) +
  facet_wrap(~Sex) + theme_classic() + scale_x_continuous(breaks = c(seq(1980,2010,10))) + #scale_y_continuous(breaks = seq(0,0.4,0.1),limits = c(0,0.4),labels = scales::percent_format(accuracy = 1)) +
  theme(text = element_text(family = "sans"),legend.title = element_blank(),axis.text = element_text(size = 32),axis.title = element_text(size = 32),strip.placement = "inside",strip.text = element_text(size = 32),strip.background = element_blank(),legend.position = c(0.82,0.75),panel.spacing = unit(2, "lines"),legend.text = element_text(size = 32)) +
  labs(y = "Estimated number of deaths attributable to smoking per 100,000 population\n", x = "\nYear") + scale_color_manual(values = c("blue","black",brewer.pal(8,"YlOrBr")[3],brewer.pal(8,"Greens")[3],"khaki2","lightpink1","#EBDFED","lightskyblue1"))
ggsave("C:/Users/FreyaYu/Documents/Tasks/Life Expectancy Task/Recent files/Results/Ezzati/New folder/Smoking AF mx.png",dpi=300,height = 16.5,width = 22)


# Smoking attributable mortality during 2016
saf_smk16 <- saf_smk %>% filter(Year == 2016) %>% mutate(mx_smk = SAF/Pop)
saf_smk16 <- saf_smk16 %>% mutate(mx_smk_ll = (100000/Pop)*(SAF - (qnorm(.975)*sqrt(SAF*(1 - SAF/Pop)))))
saf_smk16 <- saf_smk16 %>% mutate(mx_smk_ul = (100000/Pop)*(SAF + (qnorm(.975)*sqrt(SAF*(1 - SAF/Pop)))))

saf_smk16 %>% filter(Year == 2016 & Sex == "Male") %>% ggplot(.,aes(Country,mx_smk,fill = Country)) + geom_col()

saf_smk16$Country <- as.factor(saf_smk16$Country)
saf_smk16$Country <- relevel(saf_smk16$Country,ref = 'Hong Kong')
saf_smk16 %>% select(Country,Sex,mx_smk,mx_smk_ll,mx_smk_ul) %>% mutate(mx_smk = 100000*mx_smk) %>%
  mutate(mx = paste0(sprintf("%.1f",(round(mx_smk,1)))," (",sprintf("%.1f",round(mx_smk_ll,1)),", ",sprintf("%.1f",round(mx_smk_ul,1)),")")) %>%
  select(Country,Sex,mx) %>% rename('Smoking-attributable mortality' = 'mx') %>% 
write.csv(.,"C:/Users/FreyaYu/Documents/Tasks/Life Expectancy Task/Recent files/Results/Ezzati/New folder/Smoking AF mx 2016 .csv",row.names = F)


