

#################################################################################################################
## source "extractTemperaturePointData.R" function and execute it for Rwanda RAB use case
#################################################################################################################

### extracting temperature geo-spatial data for GPS locations (this is essential to get for location we have field/survey data)
source("/home/jovyan/agwise/AgWise_Scripts/fieldData_analytics/extractTemperaturePointData.R")

GPSdata <- read.csv("~/agwise/AgWise_Data/data_sourcing/UseCase_Rwanda_RAB/Potato/raw/fieldData/aggregated_field_data.csv")
extract_pointdata(country = "Rwanda", useCaseName = "RAB", Crop = "Potato", GPSdata = GPSdata)
                 
