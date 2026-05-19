#DR2026_03: Fishing data MPC

logs_and_fish(loc="offshore", year=2009:2025, get.marfis = F, export = F, direct = direct)
#browser on line 339

mpc_dat <- log1[, c("MON_DOC_ID", "VR_NUMBER", "TRIP_ID", "LICENCE_ID", "NAFO_UNIT_AREA", 
         "FISHING_AREA", "ENT_LATITUDE", "LATITUDE_DEG", "ENT_LONGITUDE", "LONGITUDE_DEG", 
         "VESSEL_NAME", "DATE_FISHED", "WATCH", "NO_RAKES_FISHED", "NO_TOWS_PER_WATCH", 
         "AVG_TOW_TIME", "DEPTH_FM", "BOTTOM_TYPE", "PRORATED_RPTD_WEIGHT_KGS")]
names(mpc_dat)

write.csv(x = mpc_dat, row.names = F, file = "Y:/Offshore/Assessment/2026/Supporting_tasks/DR2026_03_OffshoreScallopLogs_2009_2025.csv")
