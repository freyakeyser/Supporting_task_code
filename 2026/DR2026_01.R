
#load("Y:/Offshore/Assessment/Data/Fishery_data/Summary/2025/OSAC_tidy_logs.RData") # Change to 2024or subset to only include up to 2024
#logs_and_fish(loc="offshore", get.local=T, direct="Y:/Offshore/Assessment/", year = 1981:2022)
new.log.dat <- read.csv("Y:/Offshore/Assessment/Data/Fishery_data/Logs/Compiled/2009-2022log_revisedMar182026.csv")
old.log.dat <- read.csv("Y:/Offshore/Assessment/Data/Fishery_data/Logs/Compiled/1982-2008logcorrected.csv")
fish.dat<-merge(new.log.dat,old.log.dat,all=T)


require(sf)
require(ggplot2)
require(dplyr)
require(rosettafish)
github_folder <- "C:/Users/keyserf/Documents/GitHub/Assessment_fns" # Path to your Github folder
output_path <- "C:/Users/keyserf/Documents/temp_data/" # Change to a local drive
french <- F

source(paste0(github_folder, "/Maps/github_spatial_import.R"))

fish.dat$ID<-1:nrow(fish.dat)
fish.dat <- fish.dat[!is.na(fish.dat$lon),]
fish.dat <- fish.dat[!is.na(fish.dat$lat),]
fish.dat <- fish.dat[!fish.dat$lon==0,]
fish.dat <- fish.dat[!fish.dat$lat==0,]
fish.dat$vrnum[fish.dat$year<2009] <- fish.dat$vesid[fish.dat$year<2009]

fish_sf <- st_as_sf(fish.dat, coords=c(X="lon", Y="lat"), crs=4326)

offshore <- github_spatial_import("offshore", "offshore.zip", quiet=T)
#ggplot() + geom_sf(data=offshore) + facet_wrap(~ID)

sf_use_s2(FALSE)
fish_sf <- st_intersection(fish_sf, offshore)
sf_use_s2(TRUE)
unique(fish_sf$ID.1)
unique(fish_sf$bank)
unique(fish_sf$sfa)
fish_sf$ID.1 <- gsub(x=fish_sf$ID.1, pattern="SFA", replacement="")

# if shp ID doesn't match log sfa:
dim(fish_sf[!fish_sf$ID.1==fish_sf$sfa & !is.na(fish_sf$sfa),])
# if shp ID matches log sfa:
dim(fish_sf[fish_sf$ID.1==fish_sf$sfa,])
unique(fish_sf$sfa)
unique(fish_sf$ID.1)

# look at the "bad" matches
check <- fish_sf[!fish_sf$ID.1==fish_sf$sfa & !is.na(fish_sf$sfa),]
ggplot() + geom_point(data=check, aes(sfa, ID.1))
#unique(fish_sf[fish_sf$bank=="Ban",]$ID.1)
#unique(fish_sf[fish_sf$bank=="Sab",]$ID.1)
table(check$sfa, check$ID.1) # relatively few misattributed records. Use intersected results

fish_sf$sfa <- fish_sf$ID.1

fish_sf <- fish_sf[fish_sf$year>1981 & fish_sf$year<2023,]

##########################################################################
# Calculate footprint for SFA 26
##########################################################################
sfa <- "26"
if(sfa== "26") {
  crs <- 32619
  foot <- st_as_sf(fish_sf[grep(x=fish_sf$sfa, pattern= "26"),]) %>%
    st_transform(crs)
  base <- offshore[grep(x=offshore$ID, pattern= "26"),] %>% st_transform(crs)
  base$ID <- gsub(x=base$ID, pattern=".shp", replacement="")
}

sort(unique(foot$year))
source(paste0(github_folder, "/Maps/create_grid.R"))


r <- create_grid(gridsize = sqrt(10)*1000, polygon = base) #create_grid created by Freya (ask if this doesn't work/can't find )

#plot(r)

foot <- foot %>%
  st_intersection(r) %>%
  group_by(cell) %>%
  dplyr::summarize(kg = sum(pro.repwt, na.rm = T),
                   hm = sum(hm, na.rm=T),
                   nvessels = length(unique(vrnum)))

st_geometry(foot) <- NULL

foot <- dplyr::left_join(r, foot)

lims <- foot[!is.na(foot$kg),] %>%
  st_combine() %>%
  st_convex_hull() %>%
  st_buffer(dist=50000) %>%
  st_bbox()


# if(french==T) nonGB$final <- nonGB$fr
# if(french==F) nonGB$final <- nonGB$lab_short

source(paste0(github_folder, "/Maps/pectinid_projector_sf.R"))


bp <- pecjector(area=list(y = c(lims$ymin[[1]], lims$ymax[[1]]), # Find function "pecjector"
                          x =c(lims$xmin[[1]], lims$xmax[[1]]),
                          crs = crs),
                repo = 'github',c_sys = 32619, add_layer = list(bathy = c(200,'c'), land="grey", sfa = 'offshore', scale.bar="br"),plot=F, quiet=T)# +

#manual adjustment to scalebar text size
bp$layers[[5]]$geom_params$text_cex <- 1.5

#png(filename=paste0(plotsGo, "/Fishery/footprint_", sfa, fr, ".png"),width=6.5*2, height=4*2, units = "in", res=420)
print(bp +
        geom_sf(data=foot[!is.na(foot$kg),], aes(fill=kg/1000), colour=NA, show.legend=T) +
        theme(legend.position="right") +
        geom_sf(data=foot[!is.na(foot$kg) & foot$nvessels<5,], aes(),fill="red", colour=NA, show.legend=T) +
        theme(legend.position="right") +
        scale_fill_viridis_c(name=paste0(en2fr(x = "Catch", translate=french, custom_terms=rosetta_terms), " (t)"), option="G", begin=0.95, end=0)+
        coord_sf(expand=F)#+
        # geom_sf_text(data = nonGB[nonGB$SFA %in% base$ID,],
        #              aes(label = final),
        #              size=6,
        #              fun.geometry = sf::st_centroid,
        #              nudge_x = nonGB[nonGB$SFA %in% base$ID,]$nudgex,
        #              nudge_y = nonGB[nonGB$SFA %in% base$ID,]$nudgey)
)#+
#theme(legend.position="bottom",legend.key.width= unit(2, 'cm'))

foot$kg_clean <- foot$kg
foot$kg_clean[!is.na(foot$kg_clean) & foot$nvessels<5] <- "Y"
foot$kg[foot$kg_clean=="Y"]<- NA
foot$kg_clean[!is.na(foot$kg) & foot$nvessels>=5] <- "Y"
foot$kg_clean[is.na(foot$kg) & is.na(foot$kg_clean)] <- "N"

foot$hm_clean <- foot$hm
foot$hm_clean[!is.na(foot$hm_clean) & foot$nvessels<5] <- "Y"
foot$hm[foot$hm_clean=="Y"]<- NA
foot$hm_clean[!is.na(foot$hm) & foot$nvessels>=5] <- "Y"
foot$hm_clean[is.na(foot$hm) & is.na(foot$hm_clean)] <- "N"

coords <- as.data.frame(st_coordinates(st_centroid(foot)))
coords$X_utm <- coords$X
coords$Y_utm <- coords$Y
foot$lon_UTM <- coords$X_utm
foot$lat_UTM <- coords$Y_utm

coords_wgs <- as.data.frame(st_coordinates(st_transform(st_centroid(foot), 4326)))
coords_wgs$X_wgs <- coords_wgs$X
coords_wgs$Y_wgs <- coords_wgs$Y
foot$lon_WGS <- coords_wgs$X_wgs
foot$lat_WGS <- coords_wgs$Y_wgs

# ggplot() + geom_sf(data=foot[c(1:10, 1000:1010, 2000:2010),]) +
#   geom_text(data=foot[c(1:10, 1000:1010, 2000:2010),], aes(lon, lat, label=cell))

out <- dplyr::select(foot, cell, lon_UTM, lat_UTM, lon_WGS, lat_WGS, kg, kg_clean, hm, hm_clean)
st_geometry(out) <- NULL
out <- out[out$kg_clean=="Y" | out$hm_clean=="Y",]

summary(out)
out$privacy_screen <- NA
out$privacy_screen[is.na(out$kg)] <- "screened out"
out$privacy_screen[!is.na(out$kg)] <- "screened in"

out <- dplyr::select(out, lon_UTM, lat_UTM, lon_WGS, lat_WGS, kg, hm, privacy_screen)

ggplot() + geom_point(data=out,aes(lon_UTM,lat_UTM, colour=kg))

write.csv(x=out, file="C:/Users/keyserf/Documents/temp_data/DR2026_01_26.csv", row.names = F)

##########################################################################
# Calculate footprint for SFA 27
##########################################################################
sfa <- "27"
if(sfa== "27") {
  crs <- 32619
  foot <- st_as_sf(fish_sf[grep(x=fish_sf$sfa, pattern= "27"),]) %>%
    st_transform(crs)
  base <- offshore[grep(x=offshore$ID, pattern= "27"),] %>% st_transform(crs)
  base$ID <- gsub(x=base$ID, pattern=".shp", replacement="")
}

sort(unique(foot$year))
source(paste0(github_folder, "/Maps/create_grid.R"))


r <- create_grid(gridsize = sqrt(10)*1000, polygon = base) #create_grid created by Freya (ask if this doesn't work/can't find )

#plot(r)

foot <- foot %>%
  st_intersection(r) %>%
  group_by(cell) %>%
  dplyr::summarize(kg = sum(pro.repwt, na.rm = T),
                   hm = sum(hm, na.rm=T),
                   nvessels = length(unique(vrnum)))

st_geometry(foot) <- NULL

foot <- dplyr::left_join(r, foot)

lims <- foot[!is.na(foot$kg),] %>%
  st_combine() %>%
  st_convex_hull() %>%
  st_buffer(dist=50000) %>%
  st_bbox()


# if(french==T) nonGB$final <- nonGB$fr
# if(french==F) nonGB$final <- nonGB$lab_short

source(paste0(github_folder, "/Maps/pectinid_projector_sf.R"))


bp <- pecjector(area=list(y = c(lims$ymin[[1]], lims$ymax[[1]]), # Find function "pecjector"
                          x =c(lims$xmin[[1]], lims$xmax[[1]]),
                          crs = crs),
                repo = 'github',c_sys = 32619, add_layer = list(bathy = c(200,'c'), land="grey", sfa = 'offshore', scale.bar="br"),plot=F, quiet=T)# +

#manual adjustment to scalebar text size
bp$layers[[5]]$geom_params$text_cex <- 1.5

#png(filename=paste0(plotsGo, "/Fishery/footprint_", sfa, fr, ".png"),width=6.5*2, height=4*2, units = "in", res=420)
print(bp +
        geom_sf(data=foot[!is.na(foot$kg),], aes(fill=kg/1000), colour=NA, show.legend=T) +
        theme(legend.position="right") +
        geom_sf(data=foot[!is.na(foot$kg) & foot$nvessels<5,], aes(),fill="red", colour=NA, show.legend=T) +
        theme(legend.position="right") +
        scale_fill_viridis_c(name=paste0(en2fr(x = "Catch", translate=french, custom_terms=rosetta_terms), " (t)"), option="G", begin=0.95, end=0)+
        coord_sf(expand=F)#+
        # geom_sf_text(data = nonGB[nonGB$SFA %in% base$ID,],
        #              aes(label = final),
        #              size=6,
        #              fun.geometry = sf::st_centroid,
        #              nudge_x = nonGB[nonGB$SFA %in% base$ID,]$nudgex,
        #              nudge_y = nonGB[nonGB$SFA %in% base$ID,]$nudgey)
)#+
#theme(legend.position="bottom",legend.key.width= unit(2, 'cm'))

#foot[!is.na(foot$kg) & foot$nvessels<5,]

foot$kg_clean <- foot$kg
foot$kg_clean[!is.na(foot$kg_clean) & foot$nvessels<5] <- "Y"
foot$kg[foot$kg_clean=="Y"]<- NA
foot$kg_clean[!is.na(foot$kg) & foot$nvessels>=5] <- "Y"
foot$kg_clean[is.na(foot$kg) & is.na(foot$kg_clean)] <- "N"

foot$hm_clean <- foot$hm
foot$hm_clean[!is.na(foot$hm_clean) & foot$nvessels<5] <- "Y"
foot$hm[foot$hm_clean=="Y"]<- NA
foot$hm_clean[!is.na(foot$hm) & foot$nvessels>=5] <- "Y"
foot$hm_clean[is.na(foot$hm) & is.na(foot$hm_clean)] <- "N"

coords <- as.data.frame(st_coordinates(st_centroid(foot)))
coords$X_utm <- coords$X
coords$Y_utm <- coords$Y
foot$lon_UTM <- coords$X_utm
foot$lat_UTM <- coords$Y_utm

coords_wgs <- as.data.frame(st_coordinates(st_transform(st_centroid(foot), 4326)))
coords_wgs$X_wgs <- coords_wgs$X
coords_wgs$Y_wgs <- coords_wgs$Y
foot$lon_WGS <- coords_wgs$X_wgs
foot$lat_WGS <- coords_wgs$Y_wgs

# ggplot() + geom_sf(data=foot[c(1:10, 1000:1010, 2000:2010),]) +
#   geom_text(data=foot[c(1:10, 1000:1010, 2000:2010),], aes(lon, lat, label=cell))

out <- dplyr::select(foot, cell, lon_UTM, lat_UTM, lon_WGS, lat_WGS, kg, kg_clean, hm, hm_clean)
st_geometry(out) <- NULL
out <- out[out$kg_clean=="Y" | out$hm_clean=="Y",]

summary(out)
out$privacy_screen <- NA
out$privacy_screen[is.na(out$kg)] <- "screened out"
out$privacy_screen[!is.na(out$kg)] <- "screened in"

out <- dplyr::select(out, lon_UTM, lat_UTM, lon_WGS, lat_WGS, kg, hm, privacy_screen)

ggplot() + geom_point(data=out,aes(lon_UTM,lat_UTM, colour=kg)) +
  geom_point(data=out[out$lon_UTM == 685083.9 & out$lat_UTM == 4654324,], aes(lon_UTM, lat_UTM))

foot[foot$lon_WGS < -66.6 & foot$lat_WGS >42 & foot$lat_WGS<42.1,]


write.csv(x=out, file="C:/Users/keyserf/Documents/temp_data/DR2026_01_27.csv", row.names = F)

##########################################################################
# Calculate footprint for SFAs 10 & 11 (combined)
##########################################################################
sfa <- 10
if(sfa== 10) {
  crs <- 32621
  foot <- st_as_sf(fish_sf[fish_sf$sfa %in% c("10.shp","11.shp","12.shp"),]) %>%
    st_transform(crs)
  base <- offshore[offshore$ID %in% c("SFA10.shp","SFA11.shp","SFA12.shp"),] %>% st_transform(crs)
  base$ID <- gsub(x=base$ID, pattern=".shp", replacement="")
}

sort(unique(foot$year))
source(paste0(github_folder, "/Maps/create_grid.R"))


r <- create_grid(gridsize = sqrt(10)*1000, polygon = base) #create_grid created by Freya (ask if this doesn't work/can't find )

#plot(r)

foot <- foot %>%
  st_intersection(r) %>%
  group_by(cell) %>%
  dplyr::summarize(kg = sum(pro.repwt, na.rm = T),
                   hm = sum(hm, na.rm=T),
                   nvessels = length(unique(vrnum)))

st_geometry(foot) <- NULL

foot <- dplyr::left_join(r, foot)

lims <- foot[!is.na(foot$kg),] %>%
  st_combine() %>%
  st_convex_hull() %>%
  st_buffer(dist=50000) %>%
  st_bbox()


# if(french==T) nonGB$final <- nonGB$fr
# if(french==F) nonGB$final <- nonGB$lab_short

source(paste0(github_folder, "/Maps/pectinid_projector_sf.R"))

bp <- pecjector(area=list(y = c(lims$ymin[[1]], lims$ymax[[1]]), # Find function "pecjector"
                          x =c(lims$xmin[[1]], lims$xmax[[1]]),
                          crs = crs),
                repo = 'github',c_sys = 32621, add_layer = list(bathy = c(200,'c'), land="grey", sfa = 'offshore', scale.bar="br"),plot=F, quiet=T)# +

#manual adjustment to scalebar text size
bp$layers[[5]]$geom_params$text_cex <- 1.5

#png(filename=paste0(plotsGo, "/Fishery/footprint_", sfa, fr, ".png"),width=6.5*2, height=4*2, units = "in", res=420)
print(bp +
        geom_sf(data=foot[!is.na(foot$kg),], aes(fill=kg/1000), colour=NA, show.legend=T) +
        theme(legend.position="right") +
        geom_sf(data=foot[!is.na(foot$kg) & foot$nvessels<5,], aes(),fill="red", colour=NA, show.legend=T) +
        theme(legend.position="right") +
        scale_fill_viridis_c(name=paste0(en2fr(x = "Catch", translate=french, custom_terms=rosetta_terms), " (t)"), option="G", begin=0.95, end=0)+
        coord_sf(expand=F)#+
        # geom_sf_text(data = nonGB[nonGB$SFA %in% base$ID,],
        #              aes(label = final),
        #              size=6,
        #              fun.geometry = sf::st_centroid)#,
                     #nudge_x = nonGB[nonGB$SFA %in% base$ID,]$nudgex,
                     #nudge_y = nonGB[nonGB$SFA %in% base$ID,]$nudgey)
)#+
#theme(legend.position="bottom",legend.key.width= unit(2, 'cm'))

foot$kg_clean <- foot$kg
foot$kg_clean[!is.na(foot$kg_clean) & foot$nvessels<5] <- "Y"
foot$kg[foot$kg_clean=="Y"]<- NA
foot$kg_clean[!is.na(foot$kg) & foot$nvessels>=5] <- "Y"
foot$kg_clean[is.na(foot$kg) & is.na(foot$kg_clean)] <- "N"

foot$hm_clean <- foot$hm
foot$hm_clean[!is.na(foot$hm_clean) & foot$nvessels<5] <- "Y"
foot$hm[foot$hm_clean=="Y"]<- NA
foot$hm_clean[!is.na(foot$hm) & foot$nvessels>=5] <- "Y"
foot$hm_clean[is.na(foot$hm) & is.na(foot$hm_clean)] <- "N"

coords <- as.data.frame(st_coordinates(st_centroid(foot)))
coords$X_utm <- coords$X
coords$Y_utm <- coords$Y
foot$lon_UTM <- coords$X_utm
foot$lat_UTM <- coords$Y_utm

coords_wgs <- as.data.frame(st_coordinates(st_transform(st_centroid(foot), 4326)))
coords_wgs$X_wgs <- coords_wgs$X
coords_wgs$Y_wgs <- coords_wgs$Y
foot$lon_WGS <- coords_wgs$X_wgs
foot$lat_WGS <- coords_wgs$Y_wgs

# ggplot() + geom_sf(data=foot[c(1:10, 1000:1010, 2000:2010),]) +
#   geom_text(data=foot[c(1:10, 1000:1010, 2000:2010),], aes(lon, lat, label=cell))

out <- dplyr::select(foot, cell, lon_UTM, lat_UTM, lon_WGS, lat_WGS, kg, kg_clean, hm, hm_clean)
st_geometry(out) <- NULL
out <- out[out$kg_clean=="Y" | out$hm_clean=="Y",]

summary(out)
out$privacy_screen <- NA
out$privacy_screen[is.na(out$kg)] <- "screened out"
out$privacy_screen[!is.na(out$kg)] <- "screened in"

out <- dplyr::select(out, lon_UTM, lat_UTM, lon_WGS, lat_WGS, kg, hm, privacy_screen)

ggplot() + geom_point(data=out,aes(lon_UTM,lat_UTM, colour=kg))

write.csv(x=out, file="C:/Users/keyserf/Documents/temp_data/DR2026_01_10-12.csv", row.names = F)

