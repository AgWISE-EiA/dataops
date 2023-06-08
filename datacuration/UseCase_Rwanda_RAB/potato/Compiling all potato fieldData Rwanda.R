
###############################
# 1. Curate SAnDMan fieldData #
###############################

#get the data
ds1 <- readRDS("~/agwise/AgWise_Data/fieldData_analytics/UseCase_Rwanda_RAB/Potato/raw/SAnDMan-potato-fieldData.RDS")

#correcting season entries
ds1 <- ds1 %>%
  dplyr::mutate(season = ifelse(season %in% c("2222B", "B2022", "2022b", "2020B", "2022"), "2022B", season),
                season = ifelse(season == "2020A", "2021A", season))

#correcting plotsize and calculating yield
ds1 <- ds1 %>% 
  dplyr::mutate(plotSize = abs(plotSize),
                plotSize = ifelse(plotSize>500, plotSize/100, plotSize),
                plotSize = ifelse(plotSize>50, plotSize/10, plotSize)) %>%
  dplyr::group_by(TLID2) %>%
  dplyr::mutate(plotSize = median(plotSize)) %>%
  dplyr::group_by(POID2) %>%
  dplyr::filter(start == max(start)) %>% #only taking the last observation per POID
  dplyr::mutate(n = n()) %>%
  dplyr::filter(n == 1) %>% #drop all plots that have more than one yield observation
  dplyr::ungroup() %>%
  dplyr::mutate(TY = ifelse(is.na(tubersFW), tubersMarketableFW, tubersFW)/plotSize*10,
                TY = ifelse(POID2 == "SAPORW756633027058", TY/10, TY)) %>% #correcting entry without decimal separator
  dplyr::left_join(read.csv("~/agwise/AgWise_Data/data_sourcing/UseCase_Rwanda_RAB/Potato/raw/fieldData/RwaSIS_potato_trials_nutrient_rates.csv")) %>%
  dplyr::rename(FDID = FDID2,
                TLID = TLID2) %>%
  dplyr::select(expCode, FDID, TLID, lat, lon, season, plantingDate, harvestDate, treat, N, P, K, TY) %>%
  as.data.frame()

#adding and replacing plant and harvest dates from records by RAB staff for RS-PFR-1:
phd <- read.csv("~/agwise/AgWise_Data/data_sourcing/UseCase_Rwanda_RAB/Potato/raw/fieldData/RwaSIS_potato_trials_with_yield_data_2023-04-14_RwaSIS_PFR.csv") %>%
  dplyr::mutate(plantingDate_FB = as.Date(Planting.date, format="%d/%m/%Y"),
                harvestDate_FB = as.Date(Harvest.date, format="%d/%m/%Y")) %>%
  dplyr::rename(FDID = FDID2,
                TLID = TLID2) %>%
  dplyr::select(FDID, TLID, plantingDate_FB, harvestDate_FB)
  
ds1 <- ds1 %>% left_join(phd) %>%
  dplyr::mutate(plantingDate = if_else(is.na(plantingDate_FB), plantingDate, plantingDate_FB),
                harvestDate = if_else(is.na(harvestDate_FB), harvestDate, harvestDate_FB)) %>%
  dplyr::select(-c(plantingDate_FB, harvestDate_FB)) %>%
  #replace impossible harvest dates by the planting date + median duration of trials
  dplyr::mutate(harvestDate = if_else(is.na(plantingDate) | (as.numeric(harvestDate - plantingDate) < 150 & as.numeric(harvestDate - plantingDate) > 90), harvestDate, plantingDate + median(harvestDate - plantingDate)))
  
#########################################
# 2. Preparing the RwaSIS season 1 data #
#########################################

ds2 <- readRDS("~/agwise/AgWise_Data/fieldData_analytics/UseCase_Rwanda_RAB/Potato/raw/RwaSIS_potato-2022A-fieldData.RDS")

ds2 <- ds2 %>%
  dplyr::rename(lon = gps_lon,
                lat = gps_lat,
                treat = treatment,
                N = nfert_kgha,
                P = pfert_kgha,
                K = kfert_kgha,
                TY = yield_tha,
                FDID = farm_id) %>%
  dplyr::mutate(FDID = paste0("RwaSIS_", FDID),
                TLID = FDID) %>%
  mutate(harvestDate = if_else(is.na(plantingDate) | (as.numeric(harvestDate - plantingDate) < 150 & as.numeric(harvestDate - plantingDate) > 90), harvestDate, plantingDate + median(harvestDate - plantingDate))) %>%
  dplyr::select(expCode, FDID, TLID, lat, lon, season, plantingDate, harvestDate, treat, N, P, K, TY)

#####################################
# 3. Preparing the IFDC potato data #
#####################################

ds3 <- readRDS("~/agwise/AgWise_Data/fieldData_analytics/UseCase_Rwanda_RAB/Potato/raw/IFDC_potato-2014B-fieldData.RDS")

ds3 <- ds3 %>%
  dplyr::mutate(FDID = paste0("IFDC_", siteNr),
                TLID = FDID) %>%
  dplyr::select(expCode, FDID, TLID, lat, lon, season, plantingDate, harvestDate, treat, N, P, K, TY)

##########################
# Combining all datasets #
##########################

ds <- rbind(ds1, ds2, ds3)

saveRDS(ds, "~/agwise/AgWise_Data/fieldData_analytics/UseCase_Rwanda_RAB/Potato/result/compiled_potato_fieldData.RDS")
saveRDS(ds, "~/agwise/AgWise_Data/response_functions/UseCase_Rwanda_RAB/Potato/raw/compiled_potato_fieldData.RDS")
saveRDS(ds, "~/agwise/AgWise_Data/potential_yield/UseCase_Rwanda_RAB/Potato/raw/compiled_potato_fieldData.RDS")

