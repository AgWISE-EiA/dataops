

#################################################################################################################
## source "get_rain_temp_summary.R" function and get rain summary data 
#################################################################################################################
source("~/agwise-datasourcing/dataops/datasourcing/Scripts/generic/get_geoSpatialRainfall.R")



Rain_summary_trialLoc_CHIRPS <- get_rf_pointSummarydata(country = "Nigeria",  useCaseName = "ACAI", Crop = "Cassava", AOI = FALSE, 
                                                 overwrite = TRUE, Planting_month_date = NULL,
                                                 Harvest_month_date = NULL, jobs=10, dataSource = "CHIRPS", ID = "ID")

Rain_summary_trialLoc__AgEra <- get_rf_pointSummarydata(country = "Nigeria",  useCaseName = "ACAI", Crop = "Cassava", AOI = FALSE,
                                         overwrite = TRUE, Planting_month_date = NULL, Harvest_month_date = NULL, jobs=10, 
                                         dataSource = "AgEra", ID = "ID")
