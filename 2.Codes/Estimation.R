#===============================================================================
# 2020/06/07
# Log-linear log-multiplicative models
# Educational assortative mating in Japan
# This is for replicating previous results
# Fumiya Uchikoshi, uchikoshi@princeton.edu
#===============================================================================

######################################################################
# Change Working Directory
######################################################################
setwd("/Users/fumiyau/Dropbox (Princeton)/4.EAMwithCredentials/Explaining-Homogamy-Decline") 

######################################################################
# Loading packages
######################################################################
#install.packages("ggthemes")
#library(logmult)
library(gnm)
library(vcdExtra)
library(dplyr)
library(purrr)
library(ggplot2)
library(ggthemes)
library(egg) #for ggarrange
library(stringr)
library(tidyverse)

######################################################################
# 5cat3time
######################################################################
df <- read.table("1.Data/log.tmp5cat3timec.txt",header = TRUE,sep = ",") 
#df <- read.table("1.Data/Lem/Marriage/log.tmp5cat4timecKHPS.txt",header = TRUE,sep = ",") 

df <- cbind(df,Crossings(df$HusEdu,df$WifEdu))
df <- df %>% 
  mutate(
    Hom1=case_when(
      HusEdu==2 & WifEdu ==2 ~ 2,
      HusEdu==3 & WifEdu ==3 ~ 3,
      HusEdu>3 & WifEdu >3 ~ 4),
    Hom1 = as.factor(ifelse(is.na(Hom1),0,Hom1)),
    Hom2=case_when(
      HusEdu==2 & WifEdu ==2 ~ 2,
      HusEdu==3 & WifEdu ==3 ~ 3,
      HusEdu>3 & WifEdu >3 & (HusEdu != WifEdu) ~ 5,
      HusEdu==4 & WifEdu ==4 ~ 4,
      HusEdu==5 & WifEdu ==5 ~ 6,
      HusEdu==6 & WifEdu ==6 ~ 7),
    Hom2 = as.factor(ifelse(is.na(Hom2),0,Hom2)),
    Hom3 = as.factor(ifelse(HusEdu == WifEdu,HusEdu,0)),
    HusEdu=case_when(
      HusEdu == 2 ~ "HS",
      HusEdu == 3 ~ "JC",
      HusEdu == 4 ~ "PNS",
      HusEdu == 5 ~ "PST",
      HusEdu == 6 ~ "NPU"),
    WifEdu=case_when(
      WifEdu == 2 ~ "HS",
      WifEdu == 3 ~ "JC",
      WifEdu == 4 ~ "PNS",
      WifEdu == 5 ~ "PST",
      WifEdu == 6 ~ "NPU"),
    marco3=case_when(
      marco3 == 1 ~ "cohort 1",
      marco3 == 2 ~ "cohort 2",
      marco3 == 3 ~ "cohort 3"
    )) 

######################################################################
# Run analysis
######################################################################
df <- df %>% 
  mutate(freq=freq+(1/nrow(df)))
set.seed(1024)
gnm_list <- list()
gnm_list$M1 <- gnm(formula = freq ~ marco3:HusEdu + marco3:WifEdu , data = df, family = poisson)
gnm_list$M2 <- update(gnm_list$M1, ~ . + Hom1) # Homogamy 1
gnm_list$M3 <- update(gnm_list$M1, ~ . + HusEdu:WifEdu + marco3:Hom1)  # Homogamy 1, changing
gnm_list$M4 <- update(gnm_list$M1, ~ . + Mult(Exp(marco3), Hom1)) # Homogamy 1, LM
gnm_list$M5 <- update(gnm_list$M1, ~ . + Hom2) # Homogamy 2
gnm_list$M6 <- update(gnm_list$M1, ~ . + Hom3) # Homogamy 3
gnm_list$M7 <- update(gnm_list$M1, ~ . + C1 + C2 + C3 + C4 ) # Crossing
gnm_list$M8 <- update(gnm_list$M1, ~ . + HusEdu:WifEdu) #Saturated
gnm_list$M9 <- update(gnm_list$M1, ~ . + HusEdu:WifEdu + marco3:Hom2) # Homogamy 2, changing
gnm_list$M10 <- update(gnm_list$M1, ~ . + HusEdu:WifEdu + Hom3:marco3) # Homogamy 3, changing
gnm_list$M11 <- update(gnm_list$M1, ~ . + HusEdu:WifEdu + marco3:C1 + marco3:C2 + marco3:C3 + marco3:C4 ) # Crossing, changing
gnm_list$M12 <- update(gnm_list$M1, ~ . + Mult(Exp(marco3), Hom2)) # Homogamy 2, LM
gnm_list$M13 <- update(gnm_list$M1, ~ . + Mult(Exp(marco3), Hom3)) # Homogamy 3, LM
gnm_list$M14 <- update(gnm_list$M1, ~ . + Mult(Exp(marco3), C1 + C2 + C3 + C4 )) # Crossing, LM
gnm_list$M15 <- update(gnm_list$M1, ~ . + Mult(Exp(marco3), HusEdu:WifEdu)) # Saturated, LM

######################################################################
# Goodness of fit
######################################################################

fit_df <- function(gnm_object){
  df <- gnm_object$df.residual
  g_sq <- gnm_object$deviance
  id <- sum(abs(gnm_object$data$freq-gnm_object$fitted.values))/(2*sum(gnm_object$data$freq))
  bic <- g_sq - gnm_object$df.residual * log(sum(gnm_object$data$freq))
  fit_df <- data.frame(g_sq,df,id,bic)
  return(fit_df)
}

gof1 <- bind_rows(map(gnm_list,fit_df),.id = "model")
write.csv(gof1,"3.Results/gof1.csv")

######################################################################
# Hypothesis testing
######################################################################
#--------------------#
#Testing hypothesis 1
#--------------------#
1-pchisq(gof1[2,2]-gof1[4,2], 2) # significance test
marco1 <- c("1970-1984","1985-1999","2000-2015")
myContrastsM4 <- getContrasts(gnm_list$M4, 
                              pickCoef(gnm_list$M4, ", Hom1"))

figM4 <- myContrastsM4$qvframe %>% 
  cbind(marco1) %>% 
  mutate(estimate=1+estimate,
         group="G")  %>% 
  dplyr::select(estimate, SE, marco=marco1,group) %>% 
  ggplot(aes(x = marco, y = estimate,group=group)) + geom_point() +geom_line() +
  ylab("Bc parameter") +xlab("Marriage cohrots")+theme_few()+ylim(0.5,1) 
#ggsave(figM4,height=6,width=8,dpi=200, filename="4.Fig/FigM4.pdf",  family = "Helvetica")

#--------------------#
#Testing hypothesis 2
#--------------------#

#coef with 95% interval
homlabel <- c("1.High School or less","2.Junior College","3.Private(Non STEM)","4.Univ off-diagonal","5.Private(STEM)","6.National/Public")
#choose the best fitting model
#homogamy 2
coefM5 <- as.data.frame(summary(gnm_list$M5)$coefficients[29:34,])
coefM5fig <- ggplot(coefM5, aes(x = homlabel, y = Estimate)) +
  ylab("Coefficients") +xlab("Homogamy parameters")+theme_few() +geom_hline(yintercept = 0)+theme(axis.text.x=element_text(angle=20,vjust=1,hjust=1))+
  geom_point()+geom_errorbar(aes(ymin = Estimate - 1.96 * `Std. Error`, ymax = Estimate + 1.96 * `Std. Error`), width = 0.2, position = position_dodge(0.9))
coefM5[6,1]-1.96*coefM5[6,2]-(coefM5[5,1]+1.96*coefM5[5,2])
#ggsave(coefM5fig,height=6,width=8,dpi=200, filename="4.Fig/coefM5fig.pdf",  family = "Helvetica")

coefM5 %>% mutate(l95 = Estimate - 1.96 * `Std. Error`,
                  u95 = Estimate + 1.96 * `Std. Error`) 

#crossing (for supp)
coefM7 <- as.data.frame(summary(gnm_list$M7)$coefficients[29:32,])
coefM7fig <- ggplot(coefM7, aes(x = rownames(coefM7), y = Estimate)) +
  ylab("Coefficients") +xlab("Crossing parameters")+theme_few() +geom_hline(yintercept = 0)+
  geom_point()+geom_errorbar(aes(ymin = Estimate - 1.96 * `Std. Error`, ymax = Estimate + 1.96 * `Std. Error`), width = 0.2, position = position_dodge(0.9))
#ggsave(coefM5fig,height=6,width=8,dpi=200, filename="4.Fig/coefM7fig.pdf",  family = "Helvetica")

coeffig_integ <- ggarrange(coefM5fig, coefM7fig,
                           labels = c("A", "B"),
                           ncol = 2, nrow = 1)
ggsave(coeffig_integ,height=6,width=12,dpi=200, filename="4.Fig/Figure5.pdf",  family = "Helvetica")

#--------------------#
#Testing hypothesis 3
#--------------------#
myContrastsM31 <- getContrasts(gnm_list$M3, 
                               pickCoef(gnm_list$M3, ":Hom12"))
myContrastsM32 <- getContrasts(gnm_list$M3, 
                               pickCoef(gnm_list$M3, ":Hom13"))
myContrastsM33 <- getContrasts(gnm_list$M3, 
                               pickCoef(gnm_list$M3, ":Hom14"))
myContrastsM10_hom4 <- getContrasts(gnm_list$M10, 
                                    pickCoef(gnm_list$M10, ":Hom34"))
myContrastsM10_hom5 <- getContrasts(gnm_list$M10, 
                                    pickCoef(gnm_list$M10, ":Hom35"))
myContrastsM10_hom6 <- getContrasts(gnm_list$M10, 
                                    pickCoef(gnm_list$M10, ":Hom36"))
myContrastsM11_c2 <- getContrasts(gnm_list$M11, 
                                  pickCoef(gnm_list$M11, ":C2"))
myContrastsM11_c3 <- getContrasts(gnm_list$M11, 
                                  pickCoef(gnm_list$M11, ":C3"))
myContrastsM11_c4 <- getContrasts(gnm_list$M11, 
                                  pickCoef(gnm_list$M11, ":C4"))

marco2 <- rep(c("1970-1984","1985-1999","2000-2015"),3)
edu1 <- c(rep("1.High school or less",3),rep("2.Junior college",3),rep("3.University",3))
edu2 <- c(rep("3.Private(Non-STEM)",3),rep("4.Private(STEM)",3),rep("5.National/Public",3))
edu3 <- c(rep("2.Junior college vs Private(Non-STEM)",3),rep("3.Private(Non-STEM) vs Private(STEM)",3),rep("4.Private(STEM) vs National/Public",3))

figM3 <- rbind(myContrastsM31$qvframe,myContrastsM32$qvframe,myContrastsM33$qvframe) %>% 
  cbind(marco2,edu1) %>% 
  ggplot(aes(x = marco2, y = estimate,group=edu1)) + geom_point() +geom_line(aes(linetype=edu1)) + ylim(-1,1.2)+
  ylab("Coefficients") +xlab("Marriage cohrots")+theme_few()+ theme(legend.title=element_blank(), legend.position = "bottom")

figM10 <- rbind(myContrastsM10_hom4$qvframe,myContrastsM10_hom5$qvframe,myContrastsM10_hom6$qvframe) %>% 
  cbind(marco2,edu2) %>% 
  ggplot(aes(x = marco2, y = estimate,group=edu2)) + geom_point() +geom_line(aes(linetype=edu2)) +ylim(-1,1.2)+
  ylab("Coefficients") +xlab("Marriage cohrots")+theme_few()+  theme(legend.title=element_blank(), legend.position = "bottom")

figM11 <- rbind(myContrastsM11_c2$qvframe,myContrastsM11_c3$qvframe,myContrastsM11_c4$qvframe) %>% 
  cbind(marco2,edu3) %>% 
  ggplot(aes(x = marco2, y = estimate,group=str_wrap(edu3,5))) + geom_point() +geom_line(aes(linetype=edu3)) +ylim(-1,1.2)+
  ylab("Coefficients") +xlab("Marriage cohrots")+theme_few()+  theme(legend.title=element_blank(), legend.position=c(.35, .82)) 

coeffig_integ <-   ggarrange(figM3, figM10,figM11,
                             labels = c("A", "B","C"),
                             ncol = 2, nrow = 2)
ggsave(coeffig_integ,height=10,width=12,dpi=200, filename="4.Fig/Figure4.pdf",  family = "Helvetica")

# 1-pchisq(gof1[2,2]-gof1[4,2], 2)
# 1-pchisq(gof1[5,2]-gof1[12,2], 2)
# 1-pchisq(gof1[5,2]-gof1[9,2], 22)
# 1-pchisq(gof1[6,2]-gof1[10,2], 21)
# 1-pchisq(gof1[7,2]-gof1[11,2], 20)
