#DR2026_06
#For SFA 26A-C: survey biomass estimates, survey growth rates, clapper proportion and relative F (not q-corrected)

require(tidyverse)
require(sf)


load("C:/Users/keyserf/Documents/temp_data/Data/Survey_data/2025/Survey_summary_output/Survey_all_results.RData")

# survey biomass estimates and clapper estimates
bnk<-"BBn"
survey.obj[[bnk]]$model.dat[, c("year", "IPR", "IR", "I")]
clap.survey.obj[[bnk]]$model.dat[, c("year", "IPR", "IR", "I")]

bnk<-"Ger"
lined.survey.obj$model.dat[, c("year", "IPR", "IR", "I")]
clap.survey.obj[[bnk]]$model.dat[, c("year", "IPR", "IR", "I")]

# Growth from framework

# The other option is to calculate the growth using w.bar of everything over 100 mm, which will be default exclude the vast majority of the
# recruits as 90 mm scallop will grow by about 12 cm, so might have a few recruits in there, but tracking the changes in that size class tells
# us what the realized growth was for the FRs that excludes the recruits
# So what we do is take the ratio of the w.bar for everything bigger than 100 mm in year 2, to the w.bar for all FR scallop in year one
# Based on the von.B the vast majority of the scallop in that ratio be the same individuals.
# So to calculate the 100 mm thing I'll need to use the shf in surv.dat...
# I can do the same with recruit growth can't I, everything from 90 to 100 were probably recruits last year
# so look at 75-90 last year and compare with 90 to 100 this year...
g.proper.all <- NULL
for(bnk in c("BBn", "Ger")){
  sizes <- seq(0.025,2,by=0.05) # So I'd be using the 1.025 bin and everything bigger for the t+1 fully-recruited
  surv.years <- unique(surv.dat[[bnk]]$year)
  # But not 2020...
  surv.years <- surv.years[surv.years != 2020]
  # The w.yst object is exactly proportional to mod.dat$I, there is an offset, but given I need proportions I think this object is perfectly fine to use.
  # SO this mw.per.bin is taking the stratified abundance and dividing it by the stratified numbers in each bin, which gives us the MW in that bin.
  # There is probably a MW object out there somewhere with this in it, but it should just be the same thing as this.
  if(bnk=="BBn") {
    mw.per.bin <- data.frame(mw.per.bin = rbind(survey.obj[[bnk]]$shf.dat$w.yst/survey.obj[[bnk]]$shf.dat$n.yst,rep(NA,40)),year = c(surv.years,2020))
    N.per.bin <- data.frame(N.per.bin = rbind(survey.obj[[bnk]]$shf.dat$n.yst,rep(NA,40)),year = c(surv.years,2020))
  }
  if(bnk=="Ger") {
    mw.per.bin <- data.frame(mw.per.bin = rbind(lined.survey.obj$shf.dat$w.yst/lined.survey.obj$shf.dat$n.yst),year = lined.survey.obj$shf.dat$w.yst[,1])
    N.per.bin <- data.frame(N.per.bin = rbind(lined.survey.obj$shf.dat$n.yst), year = lined.survey.obj$shf.dat$n.yst[,1])
  }
  #reorder them
  mw.per.bin <- mw.per.bin[order(mw.per.bin$year),]
  N.per.bin <- N.per.bin[order(N.per.bin$year),]
  # Get the right bins for the FRs
  if(bnk=="BBn"){
    max.bin <- length(sizes)
    bin.frs.plus <- which(sizes == 1.025):max.bin
    bin.90.plus <- which(sizes == 0.925):max.bin
    bin.rec <- which(sizes == 0.775):(min(bin.90.plus)-1)
    bin.frs.minus <- min(bin.90.plus):(min(bin.90.plus)+1)
  }
  if(bnk=="Ger"){
    max.bin <- length(sizes)
    bin.frs.plus <- which(sizes == 1.125):max.bin 
    bin.90.plus <- which(sizes == 1.075):max.bin
    bin.rec <- which(sizes == "0.975"):(min(bin.90.plus)-1)
    bin.frs.minus <- min(bin.90.plus):(min(bin.90.plus)+1)
  }
  
  # and the right bins for the recruits
  
  # Now make a new object
  g.proper <- data.frame(year = mw.per.bin$year, bank=bnk)
  if(bnk=="Ger") g.proper$year[is.na(g.proper$year)] <- 2020
  g.proper$total.abun.90 <- rowSums(N.per.bin[,bin.90.plus])
  g.proper$total.abun.frs <- rowSums(N.per.bin[,bin.frs.plus])
  g.proper$total.rec.abun <- rowSums(N.per.bin[,bin.rec])
  g.proper$total.frs.minus <- rowSums(N.per.bin[,bin.frs.minus])
  # Propotions in each bin, FRs and
  N.prop.per.bin.90 <- N.per.bin[,bin.90.plus]/g.proper$total.abun.90
  N.prop.per.bin.frs <- N.per.bin[,bin.frs.plus]/g.proper$total.abun.frs
  # Recs
  N.prop.per.bin.rec       <- N.per.bin[,bin.rec]/g.proper$total.rec.abun
  N.prop.per.bin.frs.minus <- N.per.bin[,bin.frs.minus]/g.proper$total.frs.minus
  
  # And the average mw in each of the bins of interest, first for the FRs
  g.proper$mw.frs.plus <-  rowSums(mw.per.bin[,bin.frs.plus] * N.prop.per.bin.frs,na.rm=T)
  g.proper$mw.90.plus <-   rowSums(mw.per.bin[,bin.90.plus] * N.prop.per.bin.90,na.rm=T)
  # and for the rec
  g.proper$mw.recs <-      rowSums(mw.per.bin[,bin.rec] * N.prop.per.bin.rec,na.rm=T)
  g.proper$mw.frs.minus <- rowSums(mw.per.bin[,bin.frs.minus] * N.prop.per.bin.frs.minus,na.rm=T)
  
  g.proper <- dplyr::arrange(g.proper, year)
  
  g.proper$g.proper <- c(g.proper$mw.frs.plus[2:length(g.proper$mw.frs.plus)]/g.proper$mw.90.plus[1:(length(g.proper$mw.90.plus)-1)],NA)
  g.proper$gR.proper<- c(g.proper$mw.frs.minus[2:length(g.proper$mw.frs.minus)]/g.proper$mw.recs[1:(length(g.proper$mw.recs)-1)],NA)
  
  
  g.proper[g.proper$year %in% c(1991,2020),-1] <- NA
  g.proper[g.proper$year %in% c(2019),which(names(g.proper) %in% c("g.proper","gR.proper"))] <- NA
  
  # # Fill in the mean for the missing years
  # g.proper$g.proper[g.proper$year %in% c(1991,2019,2020,2022)] <- median(g.proper$g.proper,na.rm=T)
  # g.proper$gR.proper[g.proper$year %in% c(1991,2019,2020,2022)] <- median(g.proper$gR.proper,na.rm=T)
  # 
  g.proper$bank <- bnk
  if(is.null(g.proper.all)) g.proper.all <- g.proper 
  if(!is.null(g.proper.all)) g.proper.all <- dplyr::full_join(g.proper.all, g.proper)
}

# require(ggplot2)
# ggplot(g.proper.all) +
#   geom_point(aes(x=year,y=g.proper),color="#FFD500",shape =21,fill="#FFD500") + 
#   geom_line(aes(x=year,y=g.proper),color="#FFD500") + 
#   geom_point(aes(x=year,y=gR.proper),color="#005BBB",shape =22,fill= "#005BBB") + 
#   geom_line(aes(x=year,y=gR.proper),color="#005BBB") + 
#   facet_wrap(~bank,nrow=2,) + ylim(c(0,NA))

#final!  
g.proper.all[, c("bank", "year", "g.proper", "gR.proper")]



# relative F
# exploitation rates (relative, not modelled)
# using survey year, so June to May

funs <- c("https://raw.githubusercontent.com/freyakeyser/Assessment_fns/master/Maps/create_grid.R",
          "https://raw.githubusercontent.com/freyakeyser/Assessment_fns/master/Maps/pectinid_projector_sf.R",
          "https://raw.githubusercontent.com/freyakeyser/Assessment_fns/master/Maps/github_spatial_import.R",
          "https://raw.githubusercontent.com/freyakeyser/Assessment_fns/master/Fishery/logs_and_fishery_data.r")
# Now run through a quick loop to load each one, just be sure that your working directory is read/write!
for(fun in funs) 
{
  download.file(fun,destfile = basename(fun))
  source(paste0(getwd(),"/",basename(fun)))
  file.remove(paste0(getwd(),"/",basename(fun)))
}
logs_and_fish(loc="offshore", get.local=T, direct="Y:/Offshore/Assessment/", year = 1981:2025)
fish.dat<-merge(new.log.dat,old.log.dat,all=T)
fish.dat$ID<-1:nrow(fish.dat)
fish.dat$lon <- as.numeric(fish.dat$lon)
fish.dat$lat <- as.numeric(fish.dat$lat)
fish.dat <- fish.dat[!is.na(fish.dat$lon),]
fish.dat <- fish.dat[!is.na(fish.dat$lat),]
fish.dat <- fish.dat[!fish.dat$lon==0,]
fish.dat <- fish.dat[!fish.dat$lat==0,]
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
#ggplot() + geom_point(data=check, aes(sfa, ID.1))
#unique(fish_sf[fish_sf$bank=="Ban",]$ID.1)
#unique(fish_sf[fish_sf$bank=="Sab",]$ID.1)
table(check$sfa, check$ID.1) # relatively few misattributed records. Use intersected results

fish_sf$sfa <- fish_sf$ID.1

fish_sf2 <- fish_sf 

fish_sf2$sfa[grep(x=fish_sf2$sfa, "26")] <- 26

fish_sf2$sfa[fish_sf2$bank %in% c("Ban", "Sab","Mid")] <- 25
fish_sf2$sfa[fish_sf2$bank %in% c("BBn", "BBs", "Ger")] <- 26
fish_sf2$sfa[fish_sf2$bank %in% c("GBb")] <- 27

fish_sf2$surv.year <- fish_sf2$year 
fish_sf2$surv.year[fish_sf2$month %in% 1:5 & fish_sf2$sfa %in% 25:26 ] <- fish_sf2$year[fish_sf2$month %in% 1:5 & fish_sf2$sfa %in% 25:26]-1
fish_sf2$surv.year[fish_sf2$month %in% 1:8 & fish_sf2$sfa %in% 27] <- fish_sf2$year[fish_sf2$month %in% 1:8 & fish_sf2$sfa %in% 27]-1

strata <- github_spatial_import("survey_boundaries", "survey_boundaries.zip", quiet=T)
fish_sf2 <- fish_sf2 %>% st_transform(32619)
fish_sf2 <- st_intersection(fish_sf2, st_transform(strata, 32619))
fish.dat2 <- fish.dat #safekeeping
fish.dat <- fish_sf2
#st_geometry(fish.dat) <- NULL

by.sy <- fish.dat %>% data.frame() %>% dplyr::group_by(bank, surv.year)%>%  dplyr::summarize(sum = sum(pro.repwt/1000))

by.sy[by.sy$bank=="BBn" & by.sy$surv.year>1990,]

# fish.dat$surv.year[fish.dat$month %in% 6:12 & fish.dat$sfa %in% 25:26 ] <- fish.dat$year[fish.dat$month %in% 6:12 & fish.dat$sfa %in% 25:26]+1
# fish.dat$surv.year[fish.dat$month %in% 9:12 & fish.dat$sfa %in% 27] <- fish.dat$year[fish.dat$month %in% 9:12 & fish.dat$sfa %in% 27]+1

#tail(fish.dat[, c("fished", "month", "year", "surv.year")], 50)
survey.yr <- fish.dat %>%
  dplyr::group_by(surv.year, bank, sfa) %>%
  dplyr::summarize(mt = sum(pro.repwt)/1000) %>%
  dplyr::mutate(year=surv.year)

fishing.yr <- fish.dat %>%
  dplyr::group_by(year, bank, sfa) %>%
  dplyr::summarize(mt = sum(pro.repwt)/1000) 


banks <- c("BBn", "BBs", "Ger")
relative.F <- NULL
for(b in banks) {
  # if(b=="Sab/Mid"){
  #   sab <- cbind(survey.obj[["Sab"]]$model.dat[, c("year", "I")], bank="Sab")
  #   mid <- cbind(survey.obj[["Mid"]]$model.dat[, c("year", "I")], bank="Mid")
  #   sabmid <- full_join(sab, mid) %>%
  #     group_by(year) %>%
  #     summarize(I=sum(I, na.rm=T)) #/0.35) # survey catchability (between 0.2 and 0.5), using this would mean "q-corrected", don't do this
  #   fishing <- survey.yr %>%
  #     filter(bank %in% c("Sab", "Mid")) %>%
  #     group_by(year) %>%
  #     summarize(mt=sum(mt)) %>%
  #     mutate(bank="Sab/Mid")
  #   compare <- full_join(sabmid, fishing[,c("year", "bank", "mt")])
  # }
  
  #if(!b=="Sab/Mid"){
  if(b=="Ger") {
    atow<-800*2.4384/10^6 # area of standard tow in km2
    ger.shape <- st_read("C:/Users/keyserf/Documents/Github/GIS_layers/other_boundaries/WGS_84_German.shp") %>% st_make_valid() %>% st_transform(32619)
    #ger.shape <- ger.shape %>% st_transform(crs = 32619) # BBn is right on the 19/20 border so think they are basically equivalent options here
    # Bring in the survey data
    ger.area.km2 <- st_area(ger.shape)/1e6
    units(ger.area.km2) <- NULL
    # Bring in the survey data
    
    surv.26c <- merged.survey.obj # Need to add in 2020 missing...
    surv.26c[nrow(surv.26c)+1,] <- NA
    surv.26c$year[nrow(surv.26c)] <- 2020
    # and reorder
    surv.26c <- surv.26c[order(surv.26c$year),]
    # Now the units of german are....? If it is grams/tow... this puts us in total tonnes,  must confirm the units coming out of merged survey object.
    surv.26c$I <- surv.26c$I /atow*ger.area.km2/1e6
    
    compare <- full_join(surv.26c[, c("year", "I")], survey.yr[survey.yr$bank==b,c("year", "bank", "mt")])
  }
  if(!b=="Ger") compare <- full_join(survey.obj[[b]]$model.dat[, c("year", "I")], survey.yr[survey.yr$bank==b &!is.na(survey.yr$bank),c("year", "bank", "mt")])
  # }
  compare$relative.F <- compare$mt / (compare$I) #/0.35) # survey catchability? don't use this
  relative.F <- rbind(relative.F, compare)
}
relFyrs <- expand.grid(year=min(relative.F$year, na.rm=T):max(relative.F$year, na.rm=T), bank=banks)
relative.F <- left_join(relFyrs, relative.F)
#relative.F <- relative.F[!is.na(relative.F$mt),]
#relative.F[relative.F$relative.F==Inf & !is.na(relative.F$relative.F),]$relative.F <- NA
relative.F$sfa[relative.F$bank %in% c("BBn", "BBs", "Ger", "BBn/BBs")] <- as.numeric(26)

flab <- "Relative fishing mortality"

relative.F$subarea[relative.F$bank=="BBn"] <- "26A"
relative.F$subarea[relative.F$bank=="BBs"] <- "26B"
relative.F$subarea[relative.F$bank=="Ger"] <- "26C"

ggplot() + geom_line(data=relative.F[relative.F$sfa==26,], aes(year, relative.F)) + 
  geom_point(data=relative.F[relative.F$sfa==26,], aes(year, relative.F)) +
  ylim(0,2) +
  facet_wrap(~subarea, ncol=1) +
  xlab("Year") +
  ylab(flab) +
  theme_bw()

relative.F[, c("year", "bank", "sfa", "subarea", "relative.F")]


# put it all together
smgf_all <- NULL
for (bnk in c("BBn", "BBs", "Ger")){
  if(!bnk == "Ger") {
    surv <- survey.obj[[bnk]]$model.dat[, c("year", "IPR", "IR", "I")]
    surv$bank <- bnk
    clap <- clap.survey.obj[[bnk]]$model.dat[, c("year", "IPR", "IR", "I")]
    clap$bank <- bnk
  }
  if(bnk == "Ger") {
    surv <- lined.survey.obj$model.dat[, c("year", "IPR", "IR", "I")]
    surv$bank <- bnk
    clap <- clap.survey.obj[[bnk]]$model.dat[, c("year", "IPR", "IR", "I")]
    clap$bank <- bnk
  }
  
  names(clap) <- c("year", "dead_IPR", "dead_IR", "dead_I", "bank")
  growth <- g.proper.all[g.proper.all$bank == bnk, c("bank", "year", "g.proper", "gR.proper")]
  relF <- relative.F[relative.F$bank == bnk, c("year", "bank", "sfa", "subarea", "relative.F")]
  
  survclap <- full_join(surv,clap)
  survclapgr <- full_join(survclap, growth)
  smgf <- full_join(survclapgr, relF)
  
  smgf_all <- rbind(smgf_all, smgf)
}

smgf_all <- dplyr::select(smgf_all, sfa, subarea, bank, year, IPR, IR, I, dead_IPR, dead_IR, dead_I, gR.proper, g.proper, relative.F) %>%
  arrange(sfa, subarea, year)

smgf_all <- smgf_all[(smgf_all$bank=="BBn" & smgf_all$year>1990) | 
                     (smgf_all$bank=="BBs" & smgf_all$year>2002) |
                     (smgf_all$bank=="Ger" & smgf_all$year>2007),]
View(smgf_all)

write.csv(x=smgf_all, file="Y:/Offshore/Data requests/2026/DR2026_06_Dal/SFA26_survey_growth_relF.csv")
