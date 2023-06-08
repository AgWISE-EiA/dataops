

#################################################################################################################
# Sourcing required packages 
#################################################################################################################
#################################################################################################################
packages_required <- c("tidyverse", "dplyr", "stringr", "ggplot2", "ggpmisc", "Metrics", "lme4",
                       "MuMIn")

# check and install packages that are not yet installed
installed_packages <- packages_required %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages_required[!installed_packages])}

# load required packages
suppressWarnings(suppressPackageStartupMessages(invisible(lapply(packages_required, library, character.only = TRUE))))




country <- "Rwanda"
useCaseName <- "RAB"
Crop <- "Potato"


###############################
# 1. define path for input and output
###############################

pathIn <- paste("~/agwise-datacuration/dataops/datacuration/Data/useCase_", country, "_", useCaseName, "/", Crop, "/transform/", sep="")

pathOut1 <- paste("~/agwise-datacuration/dataops/datacuration/Data/useCase_", country, "_", useCaseName, "/", Crop, "/result/", sep="")
pathOut2 <- paste("~/agwise-responsefunctions/dataops/responsefunctions/Data/useCase_", country, "_", useCaseName, "/", Crop, "/raw/", sep="")



if (!dir.exists(pathOut1)){
  dir.create(file.path(pathOut1), recursive = TRUE)
}

if (!dir.exists(pathOut2)){
  dir.create(file.path(pathOut2), recursive = TRUE)
}


###############################
# 2. read all data fieldData #
###############################

ds <- readRDS(paste(pathIn, "aggregated_fieldData.RDS", sep=""))



###############################
# 3. EDA
###############################
str(ds)
head(ds)
ds$treat <- as.factor(ds$treat)

ggplot(ds, aes(treat, TY))+
  geom_point()+
  facet_wrap(~expCode)+
  theme_bw()+
  theme(axis.title = element_text(size=14), axis.text = element_text(size=12, angle=90), 
        plot.title = element_text(hjust = 0.5, size=16), strip.text = element_text(size=14))


  


ggplot(ds, aes(treat, TY))+
  geom_point()+
  facet_wrap(~expCode)



###############################
# 4. fit linear mixed effects model
###############################

#create variables to deal with scale issues:
ds$N100 <- ds$N/100
ds$P100 <- ds$P/100
ds$K100 <- ds$K/100

#base model with independent parabolic response curves, fixed season effect, and random TL intercepts:
fit0 <- lmer(TY ~ N + P + K + I(N100**2) + I(P100**2) + I(K100**2) + season + (1|TLID), data=ds)
anova(fit0)
r.squaredGLMM(fit0)

#updated model allowing fixed two- and three-way interactions between N, P and K: 
fit1 <- update(fit0, . ~ . + N100:P100 + N100:K100 + P100:K100 + N100:P100:K100)
anova(fit1, fit0)
anova(fit1)
r.squaredGLMM(fit1)

#updated model adding random slopes:
fit2 <- update(fit1, . ~ . +(0 + N100|TLID) +(0 + P100|TLID) +(0 + K100|TLID))
anova(fit2, fit1)
anova(fit2)
r.squaredGLMM(fit2) 

ds$blupYield <- predict(fit2, ds)


###############################
# 4. evaluate the effect of using linear mixed effects model
###############################

gg1 <- ggplot( ds, aes(TY, blupYield)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)+
  facet_wrap(~ expCode)+
  geom_abline()+ 
  ggtitle("Performance of the linear mixed effects model")+ 
  xlab("Observed tuber yield (kg/ha)")+
  ylab("BLUP tuber yield (kg/ha)")+
  theme_bw()+
  theme(axis.title = element_text(size=14), axis.text = element_text(size=12), 
        plot.title = element_text(hjust = 0.5, size=16), strip.text = element_text(size=14))




## calculate the yield differences as TYref – TYtreat , for RAB potato per trial ref treat is the one with >75N, >30P and >50K)

ds2 <- NULL
for (tid in unique(ds$TLID)){
  print(tid)
  locdata <- droplevels(ds[ds$TLID == tid, ])
  locdata$refTreat <- ifelse(locdata$N >75 & locdata$P >30 & locdata$K > 50, TRUE, FALSE)
  ## for trials without the global reference treatment, use it own highest global NPK rate
  if(all(locdata$refTreat == FALSE)){
    Nmax <- max(locdata$N)
    Pmax <- max(locdata$P)
    Kmax <- max(locdata$K)
    
    locdata$refTreat <- ifelse(locdata$N == Nmax & locdata$P == Pmax & locdata$K == Kmax, TRUE, FALSE)
    ## if the trial does not have the highest NPK rate combination use higher N rate
    if(all(locdata$refTreat == FALSE)){
      locdata$refTreat <- ifelse(locdata$N == Nmax, TRUE, FALSE)
    }
  }
  wfd <- locdata[,c("treat", "N", "P", "K", "TY", "blupYield","refTreat")]
  refyield <- mean(wfd[wfd$refTreat == TRUE, "TY"])
  refyieldBlup <- mean(wfd[wfd$refTreat == TRUE, "blupYield"])
  wfd$yieldEffectraw <- ifelse(wfd$refTreat == TRUE, 0,  refyield - wfd$TY )
  wfd$yieldEffectBlup <- ifelse(wfd$refTreat == TRUE, 0,  refyieldBlup - wfd$blupYield )
  locdata <- merge(locdata, wfd, by=c("treat","N","P","K","TY", "blupYield","refTreat"))
  ds2 <- rbind(ds2, locdata)
  
}

ds <- ds2 %>%
  select(-c(N100, P100, K100))
ds <- unique(ds)


ggeffraw <- ggplot(ds, aes(TY, yieldEffectraw)) +
  geom_point() +
  geom_abline(slope = 0, intercept = 0)+
  xlab("Yield") + ylab("ref.Yield - treat.Yield")+
  ggtitle("Yield effect with raw data")+
  xlim(min(ds$TY), max(ds$TY))+
  ylim(min(ds$yieldEffectraw), max(ds$yieldEffectraw))+
  theme_bw()+
  theme(axis.title = element_text(size=14), axis.text = element_text(size=12), 
        plot.title = element_text(hjust = 0.5, size=16), strip.text = element_text(size=14))



ggEffblup <- ggplot(ds, aes(TY, yieldEffectBlup)) +
  geom_point() +
  geom_abline(slope = 0, intercept = 0)+
  xlab("Yield") + ylab("ref.Yield - treat.Yield")+
  ggtitle("Yield effect with BLUP data")+
  xlim(min(ds$TY), max(ds$TY))+
  ylim(min(ds$yieldEffectraw), max(ds$yieldEffectraw))+
  theme_bw()+
  theme(axis.title = element_text(size=14), axis.text = element_text(size=12), 
        plot.title = element_text(hjust = 0.5, size=16), strip.text = element_text(size=14))


###############################
# 2. write out results
###############################


ggsave(paste(pathOut1, "LME_Performance.pdf", sep=""), gg1, width=8, height = 8)
ggsave(paste(pathOut1, "yieldEffect_rawData.pdf", sep=""), ggeffraw, width=8, height = 8)
ggsave(paste(pathOut1, "yieldEffect_BLUP.pdf", sep=""), ggEffblup, width=8, height = 8)


saveRDS(ds, paste(pathOut1, "compiled_fieldData.RDS", sep=""))
saveRDS(ds, paste(pathOut2, "compiled_fieldData.RDS", sep=""))



