# spb summary for Elizabeth

load("Y:/Offshore/Assessment/Data/Fishery_data/Summary/2025/OSAC_summary.RData")

#cpue.dat$SPB %>% View()

load("Y:/Offshore/Assessment/Data/Fishery_data/Summary/2025/OSAC_tidy_logs.RData")

table(fish.dat$year, is.na(fish.dat$vrnum))
table(fish.dat$year, is.na(fish.dat$vesid))
table(fish.dat$year, is.na(fish.dat$licence))

fish.dat$vrnum[fish.dat$year<2009] <- fish.dat$vesid[fish.dat$year<2009]

screen <- fish.dat %>%
  dplyr::filter(bank=="SPB") %>%
  dplyr::group_by(year) %>%
  dplyr::summarize(kg = sum(pro.repwt, na.rm=T)/1000,
                   n.vrnum = length(unique(vrnum)),
                   n.licence = length(unique(licence)))

screen$n.licence[screen$year<2009] <- NA

screen <- dplyr::left_join(cpue.dat$SPB, screen)

screen <- dplyr::select(screen, year, catch, effort, cpue, n.vrnum, n.licence)
names(screen) <- c("Year", "Catch (t)", "Effort (hm)", "CPUE (kg/hm)", "Number of vessels that fished", "Number of licences that fished")
write.csv(x=screen, "Y:/Offshore/Data requests/2026/SPB_fishery_data_summary.csv")
