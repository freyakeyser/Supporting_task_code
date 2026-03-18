source("C:/Users/keyserf/Documents/Github/Assessment_fns/Maps/github_spatial_import.R")

survey_boundaries <- github_spatial_import(subfolder = "survey_boundaries", zipname = "survey_boundaries.zip")
strata <- github_spatial_import(subfolder = "offshore_survey_strata", zipname = "offshore_survey_strata.zip")

require(ggplot2)
ggplot() + geom_sf(data=survey_boundaries)
ggplot() + geom_sf(data=strata)

survey_boundaries <- survey_boundaries[!survey_boundaries$ID == "SPB.shp",]

survey_boundaries$ID[survey_boundaries$ID=="0"] <- "Ger.shp"

survey_boundaries[survey_boundaries$ID=="BBn.shp",] %>% st_transform(32619) %>% st_area()/1000000
survey_boundaries[survey_boundaries$ID=="BBs.shp",] %>% st_transform(32620) %>% st_area()/1000000
survey_boundaries[survey_boundaries$ID=="GBb.shp",] %>% st_transform(32619) %>% st_area()/1000000
survey_boundaries[survey_boundaries$ID=="GBa.shp",] %>% st_transform(32619) %>% st_area()/1000000
survey_boundaries[survey_boundaries$ID=="Sab.shp",] %>% st_transform(32620) %>% st_area()/1000000
# survey domain for these does not match the total area of the strata. 
# need to combine strata shapefiles to create boundary file. 

survey_boundaries$SFA <- NA
survey_boundaries$SFA[survey_boundaries$ID=="Ban.shp"] <- "25B"
survey_boundaries$SFA[survey_boundaries$ID=="BBn.shp"] <- "26A"
survey_boundaries$SFA[survey_boundaries$ID=="BBs.shp"] <- "26B"
survey_boundaries$SFA[survey_boundaries$ID=="GBa.shp"] <- "27A"
survey_boundaries$SFA[survey_boundaries$ID=="GBb.shp"] <- "27B"
survey_boundaries$SFA[survey_boundaries$ID=="Ger.shp"] <- "26C"
survey_boundaries$SFA[survey_boundaries$ID=="Mid.shp"] <- "25A-Mid"
survey_boundaries$SFA[survey_boundaries$ID=="Sab.shp"] <- "25A-Sab"



require(sf)
write_sf(survey_boundaries, "C:/Users/keyserf/Documents/temp_data/offshore_scallop_survey_boundaries", driver = "ESRI Shapefile")

read_sf("C:/Users/keyserf/Documents/temp_data/offshore_scallop_survey_boundaries.zip")
