# main file 
# analyze data

#section 1 packages
library(ggplot2)
library(dplyr)
#library(reshape2)
library(kableExtra)
library(reshape2)
library(vars)
#library(mFilter)
#library(tseries)
#library(TSstudio)
library(forecast)
library(tidyverse)
#library(forcats)
library(lubridate)
#library(BBmisc)
#library(glmnet)
library(sparsevar)
library(bigtime)
library(fastDummies)
library(cowplot)

# WIT = dataWrapper$WIT;WINT = dataWrapper$WINT; WIO = dataWrapper$WIO; WITPER = dataWrapper$WITPER;
# WINTPER = dataWrapper$WINTPER; TWIT = dataWrapper$TWIT; TWINT = dataWrapper$TWINT ;TWIO = dataWrapper$TWIO;
# TWITPER = dataWrapper$TWITPER; TWINTPER = dataWrapper$TWINTPER ;Inc = dataWrapper$Inc

#load data
#load("/Users/gilly/OneDrive - The University of Texas at Dallas/TAAG/7variable_weekly_data/dataWrapper.RData")


# loading data
load( "~/OneDrive - The University of Texas at Dallas/TAAG/Code/jie/dataAll.RData")
source("~/OneDrive - The University of Texas at Dallas/TAAG/Code/jie/utility.R")

#second permutation
ratio.of.taag = sum(Inc$TAAG!="")/ nrow(Inc)  
pin_permu = sample(c(1,0),nrow(Inc), replace = TRUE, prob = c(ratio.of.taag, 1-ratio.of.taag)) 
Inc$permu_taag = pin_permu

Weekly_Incidents_NON_TAAG_permu <- Inc %>% dplyr::filter(permu_taag==0) %>% 
  group_by(year = year(Date), Week) %>% 
  summarise(ALCH_NT = sum(ALCH),ARSN_NT = sum(ARSN),ASST_NT = sum(ASST),BURG_NT = sum(BURG),
            DRUG_NT = sum(DRUG),HOMD_NT = sum(HOMD),
            MOTR_NT = sum(MOTR),
            ROBB_NT = sum(ROBB),THEF_NT = sum(THEF), VAND_NT = sum(VAND),
            WCCR_NT = sum(WCCR), WEAP_NT = sum(WEAP))
Weekly_Incidents_TAAG_permu <- Inc%>% dplyr::filter(permu_taag==1) %>% 
  group_by(year = year(Date), Week) %>% 
  summarise(ALCH_T = sum(ALCH),ARSN_T = sum(ARSN),ASST_T = sum(ASST),BURG_T = sum(BURG),
            DRUG_T = sum(DRUG),HOMD_T = sum(HOMD),
            MOTR_T = sum(MOTR),
            ROBB_T = sum(ROBB), THEF_T = sum(THEF), VAND_T = sum(VAND),
            WCCR_T = sum(WCCR), WEAP_T = sum(WEAP))
Weekly_Incidents_Overall_permu <- Inc %>% 
  group_by(year = year(Date), Week) %>% 
  summarise(ALCH_O = sum(ALCH),ARSN_O = sum(ARSN),ASST_O = sum(ASST),BURG_O = sum(BURG),
            DRUG_O = sum(DRUG),HOMD_O = sum(HOMD),
            MOTR_O = sum(MOTR),
            ROBB_O = sum(ROBB), THEF_O = sum(THEF), VAND_O = sum(VAND),
            WCCR_O = sum(WCCR), WEAP_O = sum(WEAP))


# scale data
split = floor(nrow(WIO)*0.6)
scaled_TWIO <- scale(TWIO[,3:ncol(TWIO)],
                     center = attributes(scale(TWIO[,3:ncol(TWIO)]))[[3]],
                     scale = attributes(scale(TWIO[,3:ncol(TWIO)]))[[4]])
scaled_TWITPER <- scale(TWITPER[,3:ncol(TWITPER)],
                        center = attributes(scale(TWITPER[,3:ncol(TWITPER)]))[[3]],
                        scale = attributes(scale(TWITPER[,3:ncol(TWITPER)]))[[4]])
scaled_TWINTPER <- scale(TWINTPER[,3:ncol(TWINTPER)],
                         center = attributes(scale(TWINTPER[,3:ncol(TWINTPER)]))[[3]],
                         scale = attributes(scale(TWINTPER[,3:ncol(TWINTPER)]))[[4]])

scaled_TWIT <- scale(TWIT[,3:ncol(TWIT)],
                        center = attributes(scale(TWIT[,3:ncol(TWIT)]))[[3]],
                        scale = attributes(scale(TWIT[,3:ncol(TWIT)]))[[4]])
scaled_TWINT <- scale(TWINT[,3:ncol(TWINT)],
                         center = attributes(scale(TWINT[,3:ncol(TWINT)]))[[3]],
                         scale = attributes(scale(TWINT[,3:ncol(TWINT)]))[[4]])

scaled_WIO <- scale(WIO[,3:ncol(WIO)],
                     center = attributes(scale(WIO[,3:ncol(WIO)]))[[3]],
                     scale = attributes(scale(WIO[,3:ncol(WIO)]))[[4]])
scaled_WITPER <- scale(WITPER[,3:ncol(WITPER)],
                        center = attributes(scale(WITPER[,3:ncol(WITPER)]))[[3]],
                        scale = attributes(scale(WITPER[,3:ncol(WITPER)]))[[4]])
scaled_WINTPER <- scale(WINTPER[,3:ncol(WINTPER)],
                         center = attributes(scale(WINTPER[,3:ncol(WINTPER)]))[[3]],
                         scale = attributes(scale(WINTPER[,3:ncol(WINTPER)]))[[4]])

scaled_WIT <- scale(WIT[,3:ncol(WIT)],
                     center = attributes(scale(WIT[,3:ncol(WIT)]))[[3]],
                     scale = attributes(scale(WIT[,3:ncol(WIT)]))[[4]])
scaled_WINT <- scale(WINT[,3:ncol(WINT)],
                      center = attributes(scale(WINT[,3:ncol(WINT)]))[[3]],
                      scale = attributes(scale(WINT[,3:ncol(WINT)]))[[4]])

#sparsevarMSFE <- function(scaled,stationary,data,transformation,testsplit)
#sparsevarMSFE_overall <- function(scaled,stationary,raw_data,testsplit,raw.taag,raw.nontaag)
#VARMSFE <- function(scaled,stationary,testsplit)
#VARMSFE_overall <- function(scaled,stationary,raw_data,testsplit,raw.taag,raw.nontaag)
#AR1MSFE <- function(scaled,stationary,testsplit)
#AR1_overall <- function(scaled, stationary,raw_data,testsplit,raw.taag,raw.nontaag)

# spliting ratio 
re_T = data.frame(method = NA,ASST_T =NA, BURG_T=NA, DRUG_T=NA, HOMD_T=NA, ROBB_T=NA, THEF_T=NA, WCCR_T=NA, split_ratio = NA)
re_NT = data.frame(method = NA,ASST_NT =NA, BURG_NT=NA, DRUG_NT=NA, HOMD_NT=NA, ROBB_NT=NA, THEF_NT=NA, WCCR_NT=NA, split_ratio = NA)
re_O = data.frame(method = NA, ASST_O =NA, BURG_O=NA, DRUG_O=NA, HOMD_O=NA, ROBB_O=NA, THEF_O=NA, WCCR_O=NA, split_ratio = NA)

split_ratio = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
for (i in 4:9){
  split = floor(split_ratio[i]*nrow(WIT))
  sparsevarMSFE_WIT =sparsevarMSFE(scaled_TWIT,TWIT,split)
  sparsevarMSFE_WINT =sparsevarMSFE(scaled_TWINT,TWINT,split)
  sparsevarMSFE_WIO =sparsevarMSFE_overall(scaled_TWIO,TWIO,WIO,split, WIT, WINT)
  # sparsevarMSFE_WITPER =sparsevarMSFE(scaled_TWITPER,TWITPER,split)
  # sparsevarMSFE_WINTPER =sparsevarMSFE(scaled_TWINTPER,TWINTPER,split)
  
  # re_T %*% priortaag 
  # re_NT %*% 1-priortaag
  
  re_T[(nrow(re_T)+1), ] =  c("sparseVAR",sparsevarMSFE_WIT$RMSE,split)
  re_T[(nrow(re_T)+1), ] =  c("sparseVAR_Overall",sparsevarMSFE_WIO$taagrmse,split)
  
  re_NT[(nrow(re_NT)+1), ] =  c("sparseVAR",sparsevarMSFE_WINT$RMSE,split)
  re_NT[(nrow(re_NT)+1), ] =  c("sparseVAR_Overall",sparsevarMSFE_WIO$nontaagrmse,split)
  
  VARMSFE_WIT =VARMSFE(scaled_TWIT,TWIT,split)
  VARMSFE_WINT =VARMSFE(scaled_TWINT,TWINT,split)
  VARMSFE_WIO =VARMSFE_overall(scaled_TWIO,TWIO,WIO,split, WIT, WINT)
  # VARMSFE_WITPER =VARMSFE(scaled_TWITPER,TWITPER,split)
  # VARMSFE_WINTPER =VARMSFE(scaled_TWINTPER,TWINTPER,split)
  
  re_T[(nrow(re_T)+1), ] =  c("VAR",VARMSFE_WIT$rmse,split)
  re_T[(nrow(re_T)+1), ] =  c("VAR_Overall",VARMSFE_WIO$taagrmse,split)
  
  re_NT[(nrow(re_NT)+1), ] =  c("VAR",VARMSFE_WINT$rmse,split)
  re_NT[(nrow(re_NT)+1), ] =  c("VAR_Overall",VARMSFE_WIO$nontaagrmse,split)
  
  
  
  AR1MSFE_WIT =AR1MSFE(scaled_TWIT,TWIT,split)
  AR1MSFE_WINT =AR1MSFE(scaled_TWINT,TWINT,split)
  AR1MSFE_WIO =AR1_overall(scaled_TWIO,TWIO,WIO,split, WIT, WINT)
  # AR1MSFE_WITPER =AR1MSFE(scaled_TWITPER,TWITPER,split)
  # AR1MSFE_WINTPER =AR1MSFE(scaled_TWINTPER,TWINTPER,split)
  
  re_T[(nrow(re_T)+1), ] =  c("AR(1)",AR1MSFE_WIT$rmse,split)
  re_T[(nrow(re_T)+1), ] =  c("AR(1)_Overall",AR1MSFE_WIO$taagrmse,split)
  
  re_NT[(nrow(re_NT)+1), ] =  c("AR(1)",AR1MSFE_WINT$rmse,split)
  re_NT[(nrow(re_NT)+1), ] =  c("AR(1)_Overall",AR1MSFE_WIO$nontaagrmse,split)
}


priortaag <- matrix(colSums(WIT[1:(split),3:ncol(WIT)])/sum(WIT[1:(split),3:ncol(WIT)]), ncol = 1)
priortaag1 <- matrix(colSums(WIT[1:(split),3:ncol(WIT)])/colSums(WIO[1:(split),3:ncol(WIO)]),ncol = 1)
priortaag2 <- matrix(colSums(WIT[1:(split),3:ncol(WIT)])/sum(WIO[1:(split),3:ncol(WIO)]),ncol =1)

# add mse 
re_T = re_T[-1,]
re = cbind(re_T, data.matrix(re_T[,2:8]) %*% priortaag, data.matrix(re_T[,2:8]) %*% priortaag1, data.matrix(re_T[,2:8]) %*% priortaag2 )
colnames(re) = c("method",colnames(scaled_TWIT), "split_ratio","MSFE_prior1","MSFE_prior2","MSFE_prior3")


re_NT = re_NT[-1,]
reN = cbind(re_NT, data.matrix(re_NT[,2:8]) %*% (1-priortaag), data.matrix(re_NT[,2:8]) %*% (1-priortaag1), data.matrix(re_NT[,2:8]) %*% (1-priortaag2) )
colnames(reN) = c("method",colnames(scaled_TWINT), "split_ratio","MSFE_prior1","MSFE_prior2","MSFE_prior3")

knitr::kable(re) %>%
  kable_styling()%>%
  save_kable(file = "/Users/gilly/Library/CloudStorage/OneDrive-TheUniversityofTexasatDallas/TAAG/presentation/split_ratio_taag.png")


knitr::kable(reN) %>%
  kable_styling()%>%
  save_kable(file = "/Users/gilly/Library/CloudStorage/OneDrive-TheUniversityofTexasatDallas/TAAG/presentation/split_ratio_nontaag.png")

write.csv(re,"/Users/gilly/Library/CloudStorage/OneDrive-TheUniversityofTexasatDallas/TAAG/presentation/split_ratio_taag.csv", row.names=FALSE )
write.csv(reN,"/Users/gilly/Library/CloudStorage/OneDrive-TheUniversityofTexasatDallas/TAAG/presentation/split_ratio_nontaag.csv", row.names=FALSE )





re = rbind(sparsevarMSFE_WINT$RMSE,
           #sparsevarMSFE_WINTPER$RMSE,
           sparsevarMSFE_WIO$nontaagrmse,
           VARMSFE_WINT$rmse,
           #VARMSFE_WINTPER$rmse,
           VARMSFE_WIO$nontaagrmse,
           AR1MSFE_WINT$rmse,
           #AR1MSFE_WINTPER$rmse,
           AR1MSFE_WIO$nontaagrmse
           )
rownames(re) <- c("sparseVAR","sparseVAR_Permt","sparseVAR_Overall",
                  "VAR","VAR_permt","VAR_Overall",
                  "AR1", "AR1_permt","AR1_Overall"
                  )

rownames(re) <- c("sparseVAR","sparseVAR_Overall",
                  "VAR","VAR_Overall",
                  "AR1", "AR1_Overall"
)
re = cbind(re, re %*% priortaag2)
colnames(re) = c(colnames(scaled_TWINT), "MSFE")


re_Twin = data.frame(method = NA,ASST_T =NA, BURG_T=NA, DRUG_T=NA, HOMD_T=NA, ROBB_T=NA, THEF_T=NA, WCCR_T=NA, window = 200)
re_NTwin = data.frame(method = NA,ASST_NT =NA, BURG_NT=NA, DRUG_NT=NA, HOMD_NT=NA, ROBB_NT=NA, THEF_NT=NA, WCCR_NT=NA, window = 200)


window = 200
sparsevarMSFE_WIT2 =sparsevarMSFE2(scaled_TWIT,TWIT,split,window,"heter")
sparsevarMSFE_WINT2 =sparsevarMSFE2(scaled_TWINT,TWINT,split,window,"heter")
sparsevarMSFE_WIO2 =sparsevarMSFE_overall2(scaled_TWIO,TWIO,WIO,split, WIT, WINT,window,"heter")
# sparsevarMSFE_WITPER2 =sparsevarMSFE2(scaled_TWITPER,TWITPER,split,window,"heter")
# sparsevarMSFE_WINTPER2 =sparsevarMSFE2(scaled_TWINTPER,TWINTPER,split,window,"heter")


re_Twin[(nrow(re_Twin)+1), ] =  c("sparseVAR",sparsevarMSFE_WIT2$RMSE,window)
re_Twin[(nrow(re_Twin)+1), ] =  c("sparseVAR_Overall",sparsevarMSFE_WIO2$taagrmse,window)

re_NTwin[(nrow(re_NTwin)+1), ] =  c("sparseVAR",sparsevarMSFE_WINT2$RMSE,window)
re_NTwin[(nrow(re_NTwin)+1), ] =  c("sparseVAR_Overall",sparsevarMSFE_WIO2$nontaagrmse,window)



VARMSFE_WIT2 =VARMSFE2(scaled_TWIT,TWIT,split,window,"heter")
VARMSFE_WINT2 =VARMSFE2(scaled_TWINT,TWINT,split,window,"heter")
VARMSFE_WIO2 =VARMSFE_overall2(scaled_TWIO,TWIO,WIO,split, WIT, WINT,window,"heter")
# VARMSFE_WITPER2 =VARMSFE2(scaled_TWITPER,TWITPER,split,window,"heter")
# VARMSFE_WINTPER2 =VARMSFE2(scaled_TWINTPER,TWINTPER,split,window,"heter")


re_Twin[(nrow(re_Twin)+1), ] =  c("VAR",VARMSFE_WIT2$rmse,window)
re_Twin[(nrow(re_Twin)+1), ] =  c("VAR_Overall",VARMSFE_WIO2$taagrmse,window)

re_NTwin[(nrow(re_NTwin)+1), ] =  c("VAR",VARMSFE_WINT2$rmse,window)
re_NTwin[(nrow(re_NTwin)+1), ] =  c("VAR_Overall",VARMSFE_WIO$nontaagrmse,window)


AR1MSFE_WIT2 =AR1MSFE2(scaled_TWIT,TWIT,split,window,"heter")
AR1MSFE_WINT2 =AR1MSFE2(scaled_TWINT,TWINT,split,window,"heter")
AR1MSFE_WIO2 =AR1_overall2(scaled_TWIO,TWIO,WIO,split, WIT, WINT,window,"heter")
# AR1MSFE_WITPER2 =AR1MSFE2(scaled_TWITPER,TWITPER,split,window,"heter")
# AR1MSFE_WINTPER2 =AR1MSFE2(scaled_TWINTPER,TWINTPER,split,window,"heter")
re_Twin[(nrow(re_Twin)+1), ] =  c("sparseVAR",VARMSFE_WIT2$rmse,window)
re_Twin[(nrow(re_Twin)+1), ] =  c("sparseVAR_Overall",VARMSFE_WIO2$taagrmse,window)

re_NTwin[(nrow(re_NTwin)+1), ] =  c("sparseVAR",VARMSFE_WINT2$rmse,window)
re_NTwin[(nrow(re_NTwin)+1), ] =  c("sparseVAR_Overall",VARMSFE_WIO$nontaagrmse,window)




re = rbind(sparsevarMSFE_WINT2$RMSE,
           sparsevarMSFE_WINTPER2$RMSE,
           sparsevarMSFE_WIO2$taagrmse,
           VARMSFE_WIT2$rmse,
           VARMSFE_WITPER2$rmse,
           VARMSFE_WIO2$taagrmse,
           AR1MSFE_WIT2$rmse,
           AR1MSFE_WITPER2$rmse,
           AR1MSFE_WIO2$taagrmse
)
rownames(re) <- c("sparseVAR","sparseVAR_Permt","sparseVAR_Overall",
                  "VAR","VAR_permt","VAR_Overall",
                  "AR1", "AR1_permt","AR1_Overall"
)

priortaag <- round(colSums(WIT[1:(split),3:ncol(WIT)])/sum(WIT[1:(split),3:ncol(WIT)]),5)

reT = round(reT,3)

re = cbind(re, re %*% priortaag)
colnames(re) = c(colnames(scaled_TWIT), "MSFE")

# weighted average
re[c(3,6,9),8] <- rowSums(re[,1:7])
priormatrix = matrix(rep(priortaag,9), ncol = 7, byrow = TRUE)

knitr::kable(re, caption = "model training with 40 weeks back ")%>%
  kable_styling()

knitr::kable(re, caption = "train test 7:3 ")%>%
  kable_styling()


##### TAAG non TAAG result

split = floor(0.8*nrow(WIT))
sparsevarMSFE_WIT =sparsevarMSFE2(scaled_TWIT,TWIT,split,window = 200,"heter")
sparsevarMSFE_WINT =sparsevarMSFE2(scaled_TWINT,TWINT,split,200,"heter")
sparsevarMSFE_WIO =sparsevarMSFE_overall2(scaled_TWIO,TWIO,WIO,split, WIT, WINT,window = 200,"heter")
# sparsevarMSFE_WITPER =sparsevarMSFE(scaled_TWITPER,TWITPER,split)
# sparsevarMSFE_WINTPER =sparsevarMSFE(scaled_TWINTPER,TWINTPER,split)
fit1 = sparseVAR(data.matrix(scale(TWIT[,3:ncol(TWIT)]), rownames.force = NA),  selection = "cv", VARpen = "HLag",h=1)
lagmatrix(fit =fit1,TRUE)

VARMSFE_WIT =VARMSFE2(scaled_TWIT,TWIT,split,200,"heter")
VARMSFE_WINT =VARMSFE2(scaled_TWINT,TWINT,split,200,"heter")
VARMSFE_WIO =VARMSFE_overall2(scaled_TWIO,TWIO,WIO,split, WIT, WINT,200,"heter")
# VARMSFE_WITPER =VARMSFE(scaled_TWITPER,TWITPER,split)
# VARMSFE_WINTPER =VARMSFE(scaled_TWINTPER,TWINTPER,split)



# AR1MSFE_WIT2 =AR1MSFE2(scaled_TWIT,TWIT,split,windows[i],"heter")
# AR1MSFE_WINT2 =AR1MSFE2(scaled_TWINT,TWINT,split,windows[i],"heter")
# AR1MSFE_WIO2 =AR1_overall2(scaled_TWIO,TWIO,WIO,split, WIT, WINT,windows[i],"heter")
AR1MSFE_WIT =AR1MSFE3(scaled_TWIT,TWIT,split,200,"heter")
AR1MSFE_WINT =AR1MSFE3(scaled_TWINT,TWINT,split,200,"heter")
AR1MSFE_WIO =AR1_overall3(scaled_TWIO,TWIO,WIO,split, WIT, WINT,200,"heter")
# AR1MSFE_WITPER =AR1MSFE(scaled_TWITPER,TWITPER,split)
# AR1MSFE_WINTPER =AR1MSFE(scaled_TWINTPER,TWINTPER,split)

reT = rbind(
            AR1MSFE_WIT$rmse,
            sparsevarMSFE_WIT$RMSE, 
            VARMSFE_WIT$rmse,
            AR1MSFE_WIO$taagrmse,
            sparsevarMSFE_WIO$taagrmse,
            VARMSFE_WIO$taagrmse
            )
reN = rbind(
            AR1MSFE_WINT$rmse,
            sparsevarMSFE_WINT$RMSE, 
            VARMSFE_WINT$rmse,
            AR1MSFE_WIO$nontaagrmse,
            sparsevarMSFE_WIO$nontaagrmse,
            VARMSFE_WIO$nontaagrmse
)

rownames(reT) <- c("AR(1)","sparseVAR","VAR", "AR(1)_Overall","sparseVAR_Overall","VAR_Overall")
rownames(reN) <- c("AR(1)","sparseVAR","VAR","AR(1)_Overall", "sparseVAR_Overall","VAR_Overall")

priortaag <- matrix(colSums(WIT[,3:ncol(WIT)])/sum(WIT[,3:ncol(WIT)]), ncol = 1)
#priortaag1 <- matrix(colSums(WIT[1:(split),3:ncol(WIT)])/colSums(WIO[1:(split),3:ncol(WIO)]),ncol = 1)
priortaag2 <- matrix(c(colSums(WINT[,3:ncol(WINT)]))/sum(WINT[,3:ncol(WINT)]),ncol =1)

priortaag3 <- matrix(c(colSums(WINT[, 3:ncol(WINT)]))/sum(WIO[,3:ncol(WIO)]),ncol =1)

cc = as.matrix(reT[,2:8] )
mode(cc) <- "double"
reT = cbind(reT,reT %*% (priortaag))
colnames(reT) = c(colnames(reT)[1:7],"MSFE")
print(xtable(reT, type = "latex",digits = 3), file = "/Users/gilly/Library/CloudStorage/OneDrive-TheUniversityofTexasatDallas/TAAG/presentation/filename2.tex")


reN = cbind(reN,reN %*% priortaag2)
colnames(reN) = c(colnames(reN)[1:7],"MSFE")
print(xtable(reN, type = "latex",digits = 3), file = "/Users/gilly/Library/CloudStorage/OneDrive-TheUniversityofTexasatDallas/TAAG/presentation/filename2.tex")


reT2 = reT[,-8]
tt = cbind(reT2,reN)%*% priortaag2
knitr::kable(reT, caption = "TAAG") %>%
  kable_styling()%>%
  save_kable(file = "/Users/gilly/Library/CloudStorage/OneDrive-TheUniversityofTexasatDallas/TAAG/presentation/taagRSFE.png")


knitr::kable(reN, caption = "non TAAG") %>%
  kable_styling()%>%
  save_kable(file = "/Users/gilly/Library/CloudStorage/OneDrive-TheUniversityofTexasatDallas/TAAG/presentation/nontaagRSFE.png")

par(mfrow = c(2,5))
for(i in 1:10){
  i=1
  X= rep(colnames(scaled_TWINT),7)
  Y = rep(colnames(scaled_TWINT),each = 7)
  data = data.frame(X = X, Y= Y)
  data$Z <- as.vector(lagNT[[i]]$LPhi)
  
  # Heatmap 
 g1= ggplot(data, aes(X, Y, fill= Z)) + 
    geom_tile() +
    scale_fill_gradient(low = "lightgreen", high = "red", breaks = c(0, median(data$Z), max(data$Z))) +
    theme_minimal()+
    # theme(panel.grid = element_line(colour="black", size=0.5))+
    geom_text(aes(label = Z)) +
    xlab("")+
    ylab("") +
    labs(title = "Lag Matrix for non TAAG after permutation")

}
library(cowplot)
plot_grid(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10, ncol = 5, main = "non TAAG")

M1 = sparseVAR(scale(TWIT[,3:9]),VARpen = "HLag",selection = "aic",p=12 )
lagmatrix(M1)

M2 = sparseVAR(scale(TWINT[1:200,3:9]),VARpen = "HLag",selection = "cv", p =12 )
lagmatrix(M2)

M3 = VAR(scaled_TWIT, ic = "AIC", type = "const",lag.max = VARselect(scale(TWIT[c(1:(split + 2 -1)),3:ncol(TWIT)]),
                                                                     lag.max = 12, type = "const")$selection[[1]])
M4 = VAR(scaled_TWINT, ic = "AIC", type = "const",lag.max = VARselect(scale(TWINT[c(1:(split + 2 -1)),3:ncol(TWIT)]),
                                                                     lag.max = 12, type = "const")$selection[[1]])
M5 = VAR(scaled_TWIO, ic = "AIC", type = "const",lag.max = VARselect(scale(TWIO[c(1:(split + 2 -1)),3:ncol(TWIT)]),
                                                                           lag.max = 12, type = "const")$selection[[1]])
# interprete result
stepAIC(lm(ASST_O ~ ASST_O.l1+
             BURG_O.l1+ DRUG_O.l1+ HOMD_O.l1+ ROBB_O.l1+ THEF_O.l1+ WCCR_O.l1+ ASST_O.l2+ BURG_O.l2+
             DRUG_O.l2+ HOMD_O.l2+ ROBB_O.l2+ THEF_O.l2+ WCCR_O.l2+ ASST_O.l3+ BURG_O.l3+ DRUG_O.l3+
             HOMD_O.l3+ ROBB_O.l3+ THEF_O.l3+ WCCR_O.l3, data = M5$datamat))

stepAIC(lm(ASST_NT ~ ASST_NT.l1+
             BURG_NT.l1+ DRUG_NT.l1+ HOMD_NT.l1+ ROBB_NT.l1+ THEF_NT.l1+ WCCR_NT.l1+ ASST_NT.l2+ BURG_NT.l2+
             DRUG_NT.l2+ HOMD_NT.l2+ ROBB_NT.l2+ THEF_NT.l2+ WCCR_NT.l2+ ASST_NT.l3+ BURG_NT.l3+ DRUG_NT.l3+
             HOMD_NT.l3+ ROBB_NT.l3+ THEF_NT.l3+ WCCR_NT.l3+ASST_T.l4+ BURG_T.l4+ DRUG_T.l4+
             HOMD_T.l4+ ROBB_T.l4+ THEF_T.l4+ WCCR_T.l4 , data = M4$datamat))

stepAIC(lm(ASST_T ~ ASST_T.l1+
             BURG_T.l1+ DRUG_T.l1+ HOMD_T.l1+ ROBB_T.l1+ THEF_T.l1+ WCCR_T.l1+ ASST_T.l2+ BURG_T.l2+
             DRUG_T.l2+ HOMD_T.l2+ ROBB_T.l2+ THEF_T.l2+ WCCR_T.l2+ ASST_T.l3+ BURG_T.l3+ DRUG_T.l3+
             HOMD_T.l3+ ROBB_T.l3+ THEF_T.l3+ WCCR_T.l3  , data = M3$datamat))
M6 = VAR(scaled_TWIT, ic = "AIC", type = "const",lag.max = VARselect(scale(TWIT[c(1:(split + 2 -1)),3:ncol(TWIT)]),
                                                                     lag.max = 12, type = "const")$selection[[1]])
M7 = VAR(scaled_WINT, ic = "AIC", type = "const",lag.max = VARselect(scale(WINT[c(1:(split + 2 -1)),3:ncol(TWIT)]),
                                                                      lag.max = 12, type = "const")$selection[[1]])
train.ar <- ar(window(scale(TWIT[,5+2]),start = 1,end = 342),aic= TRUE, order.max = 5,mathod = "ols")
stepAIC(lm(ASST_T ~ ASST_T.l1+
             BURG_T.l1+ DRUG_T.l1+ HOMD_T.l1+ ROBB_T.l1+ THEF_T.l1+ WCCR_T.l1+ ASST_T.l2+ BURG_T.l2+
             DRUG_T.l2+ HOMD_T.l2+ ROBB_T.l2+ THEF_T.l2+ WCCR_T.l2  , data = M6$datamat))
aa = list(list())
for(i in 1:7){
  aa[[i]] = lagmatrix(sparseVAR(scale(TWIT[,i+2]),VARpen = "HLag",selection = "aic", h = 1,p=5 ))
}
bb = list(list())
for(i in 1:7){
  bb[[i]] = lagmatrix(sparseVAR(scale(TWINT[,i+2]),VARpen = "HLag",selection = "aic", h = 1,p=5 ))
}
#cc = sparseVAR(scale(TWIT[,4]),VARpen = "HLag",selection = "aic", h = 1 ,p =12)
# heatmap

