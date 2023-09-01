

#################################################################################################################
# Sourcing required packages 
#################################################################################################################
#################################################################################################################
packages_required <- c("tidyverse", "dplyr", "stringr", "ggplot2", "ggpmisc", "Metrics", "lme4",
                       "MuMIn", "rgdal", "gridExtra", "ggspatial")

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
pathIn2 <- paste("~/agwise-datasourcing/dataops/datasourcing/Data/useCase_", country, "_", useCaseName, "/", Crop, "/Landing/", sep="")


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

ds[ds$TLID == "SATLRW475382409484", ]$lon <- 29.35827
ds[ds$TLID == "IFDC_3", ]

###############################
# 3. EDA
###############################
str(ds)
head(ds)
ds$treat <- as.factor(ds$treat)
ds$expCode2 <- ifelse(ds$expCode == "IFDC", "Exp 1", ifelse(ds$expCode == "SA-VAP-1", "Exp 2", "Exp 3"))
#plot showing yield ranges by experiment and season:
bplotyield <- ds %>%
  ggplot(aes(x = season, y = TY)) +
  geom_boxplot() +
  facet_wrap(~expCode2, scales="free_y", ncol=1) +
  coord_flip()+
  theme_bw()+
  #theme_gray()+
  ylab("\nPotato tuber yield [t/ha]")+
  theme(axis.title.x = element_text(size = 15, face="bold"),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14, face="bold", hjust=0))
ggsave("yielddist_box.pdf", bplotyield, width = 6, height = 6)

#plot showing yield ranges by experiment and season:

densityplot <- ds %>%
  ggplot(aes(x = TY,
             colour=paste0(expCode2, " (", season, ")"),
             fill=paste0(expCode2, " (", season, ")")
  )) +
  geom_density(alpha=.2, linewidth=1) +
  facet_wrap(~expCode2, scales="free_y", ncol=1) +
  theme_gray()+
  xlab("\nPotato tuber yield [t/ha]")+
  ylab("Density")+
  theme_bw()+
  theme(axis.title = element_text(size = 15, face="bold"),
        axis.text = element_text(size = 14),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 14, face="bold", hjust=0))
ggsave("yielddist_density.pdf", densityplot, width = 6, height = 6)



#plot showing yield ranges by experiment and season:
gg1 <- ds %>%
          ggplot(aes(x = season, y = TY)) +
          geom_boxplot() +
          facet_wrap(~expCode, scales="free_y", ncol=1) +
          coord_flip()+
          theme_gray()+
          ylab("\nPotato tuber yield [t/ha]")+
          theme(axis.title.x = element_text(size = 15, face="bold"),
                axis.title.y = element_blank(),
                axis.text = element_text(size = 14),
                strip.text = element_text(size = 14, face="bold", hjust=0))
        
ggsave(paste(pathIn, "yieldRange.pdf", sep=""), gg1, width=8, height=8)


#plot showing variation in yield as affected by NPK rate by experiment and season:
gg2 <- ds %>%
        gather(nutrient, rate, N:K) %>%
        mutate(nutrient = factor(nutrient, levels=c("N", "P", "K"))) %>%
        ggplot(aes(rate, TY)) + 
        geom_point(alpha=.33, shape=16) +
        facet_grid(nutrient ~ expCode2+season) + 
        ggtitle("Yield distribution by expCode x season")+
        xlab("\nFertilizer nutrient application rate [kg/ha]") +
        ylab("Observed tuber yield [kg/ha]\n") +
        theme(axis.title = element_text(size = 14, face="bold"),
              axis.text = element_text(size = 14),
              strip.text = element_text(size = 14, face="bold"),
              plot.title = element_text(hjust = 0.5, size=16))

ggsave(paste(pathIn, "yieldDist_season.pdf", sep=""), gg2, width=8, height=8)




#map with trial locations:

rwshp0 <- st_as_sf(geodata::gadm(country, level = 0, path='.'))
rwshp1 <- st_as_sf(geodata::gadm(country, level = 1, path='.'))
rwshp2 <- st_as_sf(geodata::gadm(country, level = 2, path='.'))
rwshp3 <- st_as_sf(geodata::gadm(country, level = 3, path='.'))
rwshp4 <- st_as_sf(geodata::gadm(country, level = 4, path='.'))
rwlake <- st_read(paste(pathIn2, "Lakes/RWA_Lakes_NISR.shp", sep=""))
AEZ <- readOGR(dsn=paste(pathIn2, "/AEZ", sep=""),  layer="AEZ_DEM_Dissolve")
RW_aez <- spTransform(AEZ, CRS( "+proj=longlat +ellps=WGS84 +datum=WGS84"))
RW_aez <- RW_aez[RW_aez$Names_AEZs %in% c("Birunga", "Congo-Nile watershed divide", "Buberuka highlands"),]
rwAEZ <- st_as_sf(RW_aez)

ggaa <- ggplot()+
  geom_sf(data = rwshp0, linewidth = 1.5, color = "#CD661D", fill=NA)+
  ggspatial::annotation_north_arrow(location = "tl", height = unit(2, "cm"), width=unit(1.5, "cm"),)+
  theme_bw()+
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("Rwandashape.pdf", ggaa, height = 4, width = 4)



gg3 <- ggplot()+
          geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) + 
          geom_sf(data = rwAEZ, aes(fill = Names_AEZs)) +
          geom_sf(data = rwlake, size=NA, fill="lightblue")+
          geom_sf(data = rwshp3[rwshp3$NAME_1 %in% c("Northern Province", "Western Province", "Southern Province"),], linewidth = 0.2, color = "white", fill=NA) + 
          geom_sf(data = rwshp2[rwshp2$NAME_1 %in% c("Northern Province", "Western Province", "Southern Province"),], linewidth = 0.6, color = "grey", fill=NA) +
          geom_sf(data = rwshp1, linewidth = 0.8, color = "black", fill=NA) + 
          geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) + 
          geom_sf_text(data = rwshp2[rwshp2$NAME_1 %in% c("Northern Province", "Western Province", "Southern Province"),], aes(label = NAME_2))+
          geom_point(data = ds, aes(x=as.numeric(lon), y=as.numeric(lat), shape = expCode, colour = expCode, size = expCode))+
          scale_shape_manual(values = c(15, 16, 18))+
          scale_size_manual(values = c(3,3,4))+
          scale_colour_manual(values = c("cornflowerblue", "blue", "blue4"))+
          scale_fill_manual(values = c("darkgoldenrod1", "darkgoldenrod", "burlywood"))+
          theme_bw()+
          xlab("Longitude")+
          ylab("Latitude")+
          theme(axis.title = element_blank(),
                axis.text = element_text(size=14),
                legend.title = element_text(size=18, face="bold"),
                legend.text = element_text(size=18),
                strip.text = element_text(size=14, face="bold"))
        


ggsave(paste(pathIn, "trial_locations.pdf", sep=""), gg3, width=8, height=8)



###############################
# 4. fit linear mixed effects model
###############################

#create variables to deal with scale issues:
ds$N100 <- ds$N/100
ds$P100 <- ds$P/100
ds$K100 <- ds$K/100

#base model with independent parabolic response curves, fixed season effect, and random TL intercepts:
fit0 <- lmer(sqrt(TY) ~ N + P + K + I(N100**2) + I(P100**2) + I(K100**2) + season + (1|TLID), data=ds)
anova(fit0)
r.squaredGLMM(fit0)
plot(fit0)

#updated model allowing fixed two- and three-way interactions between N, P and K: 
fit1 <- update(fit0, . ~ . + N100:P100 + N100:K100 + P100:K100 + N100:P100:K100)
anova(fit1, fit0)
anova(fit1)
r.squaredGLMM(fit1)
plot(fit1)

#updated model adding random slopes:
fit2 <- update(fit1, . ~ . +(0 + N100|TLID) +(0 + P100|TLID) +(0 + K100|TLID))
anova(fit2, fit1)
anova(fit2)
r.squaredGLMM(fit2) 
plot(fit2)

ds$blup <- predict(fit2, ds)**2

str(ds)


  
  

###############################
# 4. evaluate the effect of using linear mixed effects model
###############################

#plot showing relationship between observations (with random error) and BLUPs (without random error)
gg4 <- ggplot(ds, aes(x = blup, y = TY)) + 
  geom_point(alpha=.33, shape=16) +
  geom_abline(intercept = 0, slope = 1) +
  stat_poly_line(formula = y ~ x, se = F) +
  stat_poly_eq(use_label(c("eq", "R2")),
               formula = y ~ x, size = 6)+
  xlab("\nBLUP tuber yield [t/ha]") +
  ylab("Observed tuber yield [t/ha]\n") +
  ggtitle("Performance of linear mixed effect model")+
  theme_bw()+
  theme(axis.title = element_blank(),
        axis.text = element_text(size=14),
        legend.title = element_text(size=18, face="bold"),
        legend.text = element_text(size=18),
        plot.title = element_text(hjust = 0.5, size=16),
        strip.text = element_text(size=14, face="bold"))

ggsave(paste(pathIn, "LME_performance.pdf", sep=""), gg4, width=8, height=8)



#plot illustrating that the elimination of random error results in more meaningful structure in yield response:
# ds %>%
#   gather(variable, value, c(TY, blup)) %>%
#   group_by(TLID, variable) %>%
#   mutate(refY = ifelse(N > 75 & P > 30 & K > 50, value, NA),
#          refY = mean(refY, na.rm=TRUE),
#          dY = refY - value,
#          variable = factor(variable, levels=c("TY", "blup")),
#          variable = mapvalues(variable,
#                               from = c("TY", "blup"),
#                               to = c("Raw observations", "BLUPs"))) %>%
  # filter(!(N > 75 & P > 30 & K > 50))

ds2 <- NULL
for (tid in unique(ds$TLID)){
  # print(tid)
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
  wfd <- locdata[,c("treat", "N", "P", "K", "TY", "blup","refTreat", "expCode", "FDID", "TLID","season")]
  if(nrow(wfd[wfd$refTreat == TRUE, ]) >1){
    print(tid)
  }
  
  refyield <- mean(wfd[wfd$refTreat == TRUE, "TY"])
  refyieldBlup <- mean(wfd[wfd$refTreat == TRUE, "blup"])
  wfd$refY <- refyield
  wfd$refYBLUP <- refyieldBlup
  wfd$yieldEffectraw <- ifelse(wfd$refTreat == TRUE, 0,  refyield - wfd$TY )
  wfd$yieldEffectBlup <- ifelse(wfd$refTreat == TRUE, 0,  refyieldBlup - wfd$blup )
  locdata <- merge(locdata, wfd, by=c("treat","N","P","K","TY", "blup","refTreat", "expCode", "FDID", "TLID","season"))
  ds2 <- rbind(ds2, locdata)
  
}
ds2 <- unique(ds2)

str(ds2)
unique(ds2$treat)



gg5 <- ggplot(ds2, aes(x = refY, y = yieldEffectraw)) + 
          geom_point(shape=3) +
          geom_hline(yintercept = 0) +
          # facet_grid(expCode ~ .) + 
          xlab("\nYield in reference treatment [t/ha]") +
          ylab("Yield difference relative to reference treatment [t/ha]\n") +
          theme_bw()+
          theme(axis.title = element_text(size = 14, face="bold"),
                axis.text = element_text(size = 14),
                strip.text = element_text(size = 14, face="bold"),
                legend.position = "none")


gg6 <- ggplot(ds2, aes(x = refY, y = yieldEffectBlup)) + 
          geom_point(shape=16) +
          geom_hline(yintercept = 0) +
          # facet_grid(expCode ~ .) +  
          ylim(min(ds2$yieldEffectraw), max(ds2$yieldEffectraw))+
          xlab("") +
          ylab("") +
          theme_bw()+
          theme(axis.title = element_text(size = 14, face="bold"),
                axis.text = element_text(size = 14),
                strip.text = element_text(size = 14, face="bold"),
                legend.position = "none")


ggsave(paste(pathIn, "BLUP_reducingRandomError.pdf", sep=""), grid.arrange(gg5, gg6, ncol=2), width=12, height=8)



ds %>%
  filter(TLID %in% sample(unique(ds$TLID), 12, replace = F)) %>%
  ggplot(aes(x = treat, y = blup)) +
  geom_point(size = 3) + 
  geom_point(aes(y = TY), shape = 3, size = 3) +
  facet_wrap(~TLID, scales = "free_x") + 
  ylab("Potato tuber yield [t/ha]\n") +
  theme_gray()+
  theme(axis.title.y = element_text(size = 14, face="bold"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 12, angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 14),
        strip.text = element_text(size = 14, face="bold"))



###############################
# 5. write out results
###############################

saveRDS(ds2, paste(pathOut1, "compiled_fieldData.RDS", sep=""))
saveRDS(ds2, paste(pathOut2, "compiled_fieldData.RDS", sep=""))



