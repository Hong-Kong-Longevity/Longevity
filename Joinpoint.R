rm(list=ls())

require(devtools) 
install_version("ggplot2", version = "3.2.1", repos = "http://cran.us.r-project.org")

library(ggplot2)
library(ggthemes)
library(dplyr)

# World bank data			
WorldBank_LE <- read.csv("../Life expectancy/LE Overall.csv",h = T)
WorldBank_LE_male <- read.csv("../Life expectancy/LE Male.csv",h = T)
WorldBank_LE_female <- read.csv("../Life expectancy/LE Female.csv",h = T)

HICs <- c("Australia","Austria","Belgium","Canada","Denmark","Finland","France","Germany","Hong Kong","Italy","Japan","Netherlands","Norway","Portugal","Spain","Sweden","Switzerland","United Kingdom","United States")

# LE Ranking
HK_LE_rank_total <- WorldBank_LE %>% filter(Indicator == "Life expectancy") %>% select(-Indicator) %>%
  melt(id = 'Country') %>%
  mutate(value = as.numeric(value),
         variable = as.numeric(substring(variable, 2, 5))) %>%
  data.table() %>%
  .[,ranking := sum(!is.na(value)) + 1 - rank(value, na.last = 'keep', ties.method = 'max'), by = variable] %>%
  filter(`Country` == 'Hong Kong') %>%
  rename(Year = variable,
         e0 = value)

HK_LE_rank_male <- WorldBank_LE_male %>%
  melt(id = 'Country.Name') %>%
  mutate(value = as.numeric(value),
         variable = as.numeric(substring(variable, 2, 5))) %>%
  data.table() %>%
  .[,ranking := sum(!is.na(value)) + 1 - rank(value, na.last = 'keep', ties.method = 'max'), by = variable] %>%
  filter(`Country.Name` == 'Hong Kong SAR, China') %>%
  rename(Year = variable,
         e0 = value)
HK_LE_rank_male[,3][60] <- 82.2
HK_LE_rank_male[,4][60] <- 1


HK_LE_rank_female <- WorldBank_LE_female %>%
  melt(id = 'Country.Name') %>%
  mutate(value = as.numeric(value),
         variable = as.numeric(substring(variable, 2, 5))) %>%
  data.table() %>%
  .[,ranking := sum(!is.na(value)) + 1 - rank(value, na.last = 'keep', ties.method = 'max'), by = variable] %>%
  filter(`Country.Name` == 'Hong Kong SAR, China') %>%
  rename(Year = variable,
         e0 = value)
HK_LE_rank_female[,3][60] <- 88.1
HK_LE_rank_female[,4][60] <- 1


# Customzied color pallette
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
colour1 <- c(gg_color_hue(2)[1],'blue')


#----------------------------------------------------------------------------
## Load output dataset from Joinpoint regression program
jp_seg <- read.csv('../Life expectancy/joinpoint segmented.csv',h = T)
seg <- read.csv('../Life expectancy/jp seg.csv',h = T)

# Plot-----------------------------------------------------------------------
g <- jp_seg %>% ggplot()+
  # ranking of male e0 as background
  geom_bar(data = HK_LE_rank_male, 
           aes(x = Year, y = (88.755 - ranking/1.6)), 
           stat = 'identity', alpha = 0.5, fill = "grey69")+  
  # ranking of female e0 as background
  geom_bar(data = HK_LE_rank_female, 
           aes(x = Year, y = (88.755 - ranking/1.6)), 
           stat = 'identity', alpha = 0.5, fill = "grey22") +
  scale_fill_manual(name="World ranking",labels=c('Men','Women'))+
  # add labels of the breakpoints
  geom_text(data = seg, 
            aes(x = seg[,'X1'], y = seg[,'X2'], label = round(seg[,'X1']), colour = NULL),
            hjust = -.1,
            vjust =1.1,
            show.legend = FALSE)+
  # points and lines for e0
  geom_point(aes(x=Year,y=e0,colour=factor(Sex)),size=1.5) +
  geom_line(aes(x=Year,y=e0,colour=factor(Sex)),size=0.7, alpha = 0.3) +
  geom_line(aes(x=Year,y=segmented,colour=factor(Sex)), size=0.9)+
  # theme
  geom_hline(yintercept = 88.13, alpha = 0.1, linetype = 2) +
  scale_x_continuous(expand=c(0.008,0), breaks = seq(1960, 2020, 10))+
  scale_y_continuous(expand=c(0,0), 
                     sec.axis = sec_axis(~(-.+88.755)*1.6, 
                                         name = "World ranking in life expectancy\n", 
                                         breaks = c(1, seq(10, 40, 10)),
                                         labels = c(bquote(1^st), sapply(seq(10, 40, 10), function(x) bquote(.(x)^th)))))+		
  coord_cartesian(ylim=c(60,90))+
  scale_colour_manual(values=c(colour1),name=NULL,labels=c('Women','Men'))+
  theme_classic() +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        legend.position=c(0.2,0.85),
        axis.text = element_text(size=14),
        axis.title = element_text(size=16),
        legend.text = element_text(size=12)) +
  labs(x='Year',y='Life expectancy at birth (years)\n') +
  guides(col = guide_legend(reverse = TRUE))

# Add breakpoint lines				
for(i in 1:nrow(seg)){
  x <- seg[i,'X1']
  y <- seg[i,'X2']
  colour <- colour1[3-seg[i,'L1']]
  g <- g + geom_segment(x=x,xend=x,y=60,yend=y, colour=colour, linetype=2, size=1)
  
}
g

ggsave("../Life expectancy/Joinpoint 2018 rankings.png", width=8,height=8, dpi = 300)


