#===============================================================================
# 2022/06/24
# Log-linear / log-multiplicative models
# Educational assortative mating in Japan
# Fumiya Uchikoshi, uchikoshi@princeton.edu
#===============================================================================

######################################################################
# Change Working Directory
######################################################################
setwd("Explaining-Homogamy-Decline") 

######################################################################
# Assign colors
######################################################################
cbp1 <- c("#A6CEE3", "#E0E0E0", "#E6AB02", "#1F78B4",
          "#878787", "#A6761D")
cbp2 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

######################################################################
# Loading packages
######################################################################
library(gnm)
library(vcdExtra)
library(dplyr)
library(purrr)
library(ggplot2)
library(ggthemes)
library(egg) #for ggarrange
library(stringr)
library(tidyverse)
library(fastDummies)
library(haven)

######################################################################
# 5cat3time
######################################################################
df <- read.table("1.Data/log.tmp5cat3timec.txt",header = TRUE,sep = ",") 

#Pooled sample
df <- df %>% 
  filter(d_jhps==2) %>% 
  mutate(HusEdu = ifelse(HusEdu == 4,5,HusEdu),
         WifEdu = ifelse(WifEdu == 4,5,WifEdu)) %>% 
  group_by(HusEdu,WifEdu,marco) %>% 
  summarise_all(list(sum)) %>% 
  dplyr::select(-d_jhps) %>% 
  ungroup()

df <- cbind(df,Crossings(df$HusEdu,df$WifEdu))
df <- df %>% 
  mutate(
    # Homogamy 1: no distinction among university graduates
    Hom1=case_when(
      HusEdu==2 & WifEdu ==2 ~ 2,
      HusEdu>3 & WifEdu >3 ~ 4),
    Hom1 = as.factor(ifelse(is.na(Hom1),0,Hom1)),
    # Homogamy 1: no distinction among university graduates + JC homogamy
    Hom1x=case_when(
      HusEdu==2 & WifEdu ==2 ~ 2,
      HusEdu==3 & WifEdu ==3 ~ 3,
      HusEdu>3 & WifEdu >3 ~ 4),
    Hom1x = as.factor(ifelse(is.na(Hom1x),0,Hom1x)),
    # Homogamy 2: matrix 2 homogamy by univ type
    Hom2=case_when(
      HusEdu==2 & WifEdu ==2 ~ 2,
      #HusEdu==3 & WifEdu ==3 ~ 3,
      HusEdu>3 & WifEdu >3 & (HusEdu != WifEdu) ~ 3,
      #HusEdu==4 & WifEdu ==4 ~ 4,
      HusEdu==5 & WifEdu ==5 ~ 4,
      HusEdu==6 & WifEdu ==6 ~ 5),
    Hom2 = as.factor(ifelse(is.na(Hom2),0,Hom2)),
    # Homogamy 3: matrix 3 homogamy univ type distinction between national and private
    Hom3=case_when(
      HusEdu==2 & WifEdu ==2 ~ 2,
      #HusEdu==3 & WifEdu ==3 ~ 3,
      HusEdu>3 & WifEdu >3 & (HusEdu != WifEdu) ~ 4,
      #HusEdu==4 & WifEdu ==4 ~ 4,
      HusEdu==5 & WifEdu ==5 ~ 4,
      HusEdu==6 & WifEdu ==6 ~ 5),
    Hom3 = as.factor(ifelse(is.na(Hom3),0,Hom3)),
    # Homogamy 4: matrix 4
    Hom4 = as.factor(ifelse(HusEdu == WifEdu,HusEdu,0)),
    Hom4 = as.factor(ifelse(HusEdu == 3 & WifEdu == 3 ,0,Hom4)),
    # Homogamy 5 : Hypergamy model
    Hom5=case_when(
      HusEdu==2 & WifEdu ==2 ~ 2,
      #HusEdu==3 & WifEdu ==3 ~ 3,
      HusEdu>3 & WifEdu >3 & (HusEdu != WifEdu) & HusEdu > WifEdu ~ 5, #Hypergamy
      HusEdu>3 & WifEdu >3 & (HusEdu != WifEdu) & HusEdu < WifEdu ~ 6, #Hypogamy
      HusEdu==5 & WifEdu ==5 ~ 4, #Private univ homogamy
      HusEdu==6 & WifEdu ==6 ~ 8), #National public univ homogamy
    Hom5 = as.factor(ifelse(is.na(Hom5),0,Hom5)),
    # Homogamy 6 : Hypogamy model
    Hom6=case_when(
      HusEdu==2 & WifEdu ==2 ~ 2,
      HusEdu> 3 & WifEdu > 3 & (HusEdu != WifEdu) & HusEdu > WifEdu & (HusEdu == 6 | WifEdu == 6)  ~ 8,
      HusEdu> 3 & WifEdu > 3 & (HusEdu != WifEdu) & HusEdu < WifEdu & (HusEdu == 6 | WifEdu == 6)  ~ 9,
      HusEdu==5 & WifEdu ==5 ~ 7,
      HusEdu==6 & WifEdu ==6 ~ 10),
    Hom6 = as.factor(ifelse(is.na(Hom6),0,Hom6)),
    HusEdu=case_when(
      HusEdu == 2 ~ "HS",
      HusEdu == 3 ~ "JC",
      HusEdu == 5 ~ "PRU",
      HusEdu == 6 ~ "NPU"),
    WifEdu=case_when(
      WifEdu == 2 ~ "HS",
      WifEdu == 3 ~ "JC",
      WifEdu == 5 ~ "PRU",
      WifEdu == 6 ~ "NPU")) %>% 
  mutate(marco = paste("Cohort",marco))

######################################################################
# Run analysis
######################################################################
df <- df %>% 
  mutate(weighted=weighted+(1/nrow(df)))
set.seed(1024)
gnm_list <- list()
gnm_list$M0 <- gnm(formula = weighted ~ marco:HusEdu + marco:WifEdu , data = df, family = poisson)
gnm_list$M1a <- update(gnm_list$M0, ~ . + Hom1) # Homogamy 1
gnm_list$M1b <- update(gnm_list$M0, ~ . + Hom2) # Homogamy 2
#gnm_list$M1c <- update(gnm_list$M0, ~ . + Hom3) # Homogamy 3
gnm_list$M1d <- update(gnm_list$M0, ~ . + Hom4) # Homogamy 4
gnm_list$M1e <- update(gnm_list$M0, ~ . + C1 + C2 + C3 ) # Crossing
gnm_list$M2a <- update(gnm_list$M0, ~ . + Hom5) # Hypergamy
#gnm_list$M2b <- update(gnm_list$M0, ~ . + Hom6) # Hypergamy
gnm_list$M3a <- update(gnm_list$M0, ~ . + HusEdu:WifEdu + marco:Hom1)  # Homogamy 1, changing
gnm_list$M3b <- update(gnm_list$M0, ~ . + Mult(Exp(marco), Hom1)) # Homogamy 1, LM
gnm_list$M4a <- update(gnm_list$M0, ~ . + HusEdu:WifEdu + Hom2:marco) # Homogamy 2, changing
gnm_list$M4b <- update(gnm_list$M0, ~ . + HusEdu:WifEdu + Hom3:marco) # Homogamy 3, changing
gnm_list$M4c <- update(gnm_list$M0, ~ . + HusEdu:WifEdu + Hom4:marco) # Homogamy 4, changing
gnm_list$M4d <- update(gnm_list$M0, ~ . + HusEdu:WifEdu + marco:C1 + marco:C2 + marco:C3) # Crossing, changing
gnm_list$M5a <- update(gnm_list$M0, ~ . + HusEdu:WifEdu + Hom5:marco) # Hypergamy 1, changing

#--------------------#
#Testing hypothesis 2
#--------------------#

#coef with 95% interval
homlabel <- c("High school or less","Univ off-diagonal",
              "Private","National/Public")
homlabelx <- c("Private",
               "Hypergamy within univ","Hypogamy within univ","National/Public")

######## Homogamy 2 ######## 
coefM1b <- as.data.frame(summary(gnm_list$M1b)$coefficients[23:26,]) %>% 
  mutate(homlabel=factor(homlabel,levels=c("High school or less","Private",
                                           "Univ off-diagonal","National/Public")))
coefM1bfig <- ggplot(coefM1b, aes(x = homlabel, y = Estimate)) +
  ylab("Coefficients") +xlab("Homogamy parameters")+theme_few() +geom_hline(yintercept = 0)+
  theme(axis.text.x=element_text(angle=20,vjust=0.5,hjust=0.5))+
  geom_point()+geom_errorbar(aes(ymin = Estimate - 1.96 * `Std. Error`, ymax = Estimate + 1.96 * `Std. Error`), 
                             width = 0.2, position = position_dodge(0.9))

coefM1b[4,1]-1.96*coefM1b[4,2]-(coefM1b[3,1]+1.96*coefM1b[3,2])

coefM1b %>% mutate(l95 = Estimate - 1.96 * `Std. Error`,
                   u95 = Estimate + 1.96 * `Std. Error`) 

######## Crossing model #########
coefM1e <- as.data.frame(summary(gnm_list$M1e)$coefficients[23:25,]) %>% 
  mutate(var = c("High school or less/\njunior college","Junior college/\nprivate",
                 "Private/\nnational or public"))
coefM1efig <- ggplot(coefM1e, aes(x = var, y = Estimate)) +
  ylab("Coefficients") +xlab("Crossing parameters")+theme_few() +geom_hline(yintercept = 0)+
  theme(axis.text.x=element_text(angle=20,vjust=0.5,hjust=0.5))+
  geom_point()+geom_errorbar(aes(ymin = Estimate - 1.96 * `Std. Error`, ymax = Estimate + 1.96 * `Std. Error`), 
                             width = 0.2, position = position_dodge(0.9))

#Hypergamy
coefM2a <- as.data.frame(summary(gnm_list$M2a)$coefficients[24:27,]) %>% 
  mutate(homlabelx=factor(homlabelx,levels=c("Private","Hypergamy within univ",
                                             "Hypogamy within univ",
                                             "National/Public")))

coefM2a %>% mutate(l95 = Estimate - 1.96 * `Std. Error`,
                   u95 = Estimate + 1.96 * `Std. Error`) 

coefM2afig <- ggplot(coefM2a, aes(x = homlabelx, y = Estimate)) +
  ylab("Coefficients") +xlab("Pairing parameters")+theme_few() +geom_hline(yintercept = 0)+
  theme(axis.text.x=element_text(angle=20,vjust=0.5,hjust=0.5))+
  geom_point()+geom_errorbar(aes(ymin = Estimate - 1.96 * `Std. Error`, ymax = Estimate + 1.96 * `Std. Error`), 
                             width = 0.2, position = position_dodge(0.9))

# Merge figures
coeffig_integ <- ggarrange(coefM1bfig, coefM1efig,coefM2afig,
                           labels = c("A","B","C"),
                           ncol = 2, nrow = 2)
ggsave(coeffig_integ,height=8,width=10,dpi=200, filename="3.Results/Figure4_JPSC.pdf",  family = "Helvetica")
