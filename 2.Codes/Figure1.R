#=====================================================================
# 2020/08/20
# Explaining homogamy decline (Figure 1)
# Fumiya Uchikoshi, uchikoshi@princeton.edu
#=====================================================================

######################################################################
# Loading packages
######################################################################
library(gdata)
library(tidyverse) 
library(dplyr)
library(ggthemes)
library(ggrepel)
library(knitr)
library(readxl)

######################################################################
# Set directory
######################################################################
setwd("/Users/fumiyau/Dropbox (Princeton)/4.EAMwithCredentials/Explaining-Homogamy-Decline/")

######################################################################
# Assign colors
######################################################################
cbp1 <- c("#A6CEE3", "#E0E0E0", "#E6AB02", "#1F78B4",
          "#878787", "#A6761D")
cbp2 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

df <- read_xlsx("1.Data/Figure1/SchoolBasicSurvey.xlsx",skip=10) %>% 
  dplyr::select(male=20,female=21) %>% 
  mutate(year=1953+row_number()) %>% 
  na.omit() %>% 
  pivot_longer(-year, names_to = "Sex", values_to = "EntRate") 
  
######################################################################
# lapply for 2015-2019 (except for 1999)
######################################################################

All_2015 <- lapply(1:4,function(i){
  lapply(2015:2019,function(j){
    read_excel(paste("1.Data/Figure1/NameEdited/",j,".xlsx",sep=""), sheet = i,skip=4) %>%
      dplyr::select(2,5,6) %>% 
      mutate(year = j,
             inst = i)
  })
})

df_all1 <- do.call(bind_rows, c(All_2015)) %>% 
  mutate(year=year-1) %>% 
  filter(is.na(as.numeric(paste(`...1`)))==FALSE) %>%
  mutate(`...1`=as.numeric(paste(`...1`))+1988) %>% 
  filter(`...1`==year) %>% 
  select(-`...1`,male=`男`,female=`女`,year,inst) %>% 
  pivot_wider(names_from = c("inst"), 
              values_from = c(male,female)) %>% 
  mutate(male=male_4/male_1,
         female=female_4/female_1) %>% 
  select(year,male,female) %>% 
  pivot_longer(-year, names_to = "Sex", values_to = "PriProp")

######################################################################
# Hiroshima Univ Data
######################################################################

df_pri <- read_xlsx("1.Data/Figure1/W3.xlsx",skip=6) %>% 
  dplyr::select(year=`...2`,total_1=`計...4`,total_2=`私立...7`,male_1=`計...9`,male_2=`私立...12`) %>% 
  na.omit() %>% 
  mutate(female_1=total_1-male_1,
         female_2=total_2-male_2,
         male=male_2/male_1,
         female=female_2/female_1) %>% 
  select(year,male,female) %>% 
  pivot_longer(-year, names_to = "Sex", values_to = "PriProp") %>% 
  filter(year<2014) %>% 
  bind_rows(df_all1) %>% 
  mutate(PriProp=PriProp*100) %>% 
  left_join(df,by=c("year","Sex")) %>% 
  pivot_longer(-c(year,Sex), names_to = "Type", values_to = "Prop") %>% 
  mutate(Sex=if_else(Sex=="male","Male","Female"),
         Type=if_else(Type=="EntRate","Entrance rate","Students enrolled in private universities"))

######################################################################
# Dataviz
######################################################################
ggplot(df_pri,aes(x = year, y = Prop, color = Sex,group=Sex)) + 
  geom_line(aes(linetype=Sex)) + facet_wrap(~Type)+
  ylab("%") + xlab("Year") + labs(caption = "Source: School Basic Survey, Ministry of Education, Culture, Sports, Science and Technology
                                  \n Note: Entrance rate is calculated by total number of enrolled students out of high school graduates") +
  theme_few()+scale_color_manual(values=cbp2)+theme(legend.title=element_blank(), legend.position = "bottom")+
  scale_x_continuous(breaks=c(1960,1970,1980,1990,2000,2010,2020))
ggsave(height=6,width=9,dpi=200, filename="4.Fig/Figure1.pdf",  family = "Helvetica")
